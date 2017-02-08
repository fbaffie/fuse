MODULE FUSE_RMSE_MODULE  ! have as a module because of dynamic arrays
  IMPLICIT NONE
CONTAINS
  SUBROUTINE FUSE_RMSE(XPAR,DISTRIBUTED,NCID_FORC,RMSE,OUTPUT_FLAG,MPARAM_FLAG)
    ! ---------------------------------------------------------------------------------------
    ! Creator:
    ! --------
    ! Martyn Clark, 2009
    ! Modified by Brian Henn to include snow model, 6/2013
    ! Modified by Nans Addor to enable distributed modeling, 9/2016
    ! ---------------------------------------------------------------------------------------
    ! Purpose:
    ! --------
    ! Calculate the RMSE for single FUSE model and single parameter set
    !   input: model parameter set
    !  output: root mean squared error
    ! ---------------------------------------------------------------------------------------
    USE nrtype                                               ! variable types, etc.
    ! data modules
    USE model_defn, ONLY:NSTATE,SMODL                        ! number of state variables
    USE model_defnames                                       ! integer model definitions
    USE multiparam, ONLY:LPARAM,NUMPAR,MPARAM                ! list of model parameters
    USE multiforce, ONLY:MFORCE,AFORCE,DELTIM,ISTART,&       ! model forcing data
         NUMTIM,warmup_beg                   ! model forcing data (continued)
    USE multiforce, ONLY:nspat1,nspat2                       ! spatial dimensions
    USE multiforce, ONLY:ncid_var                            ! NetCDF ID for forcing variables
    USE multiforce, ONLY:gForce,gForce_3d                    ! gridded forcing data
    USE multistate, ONLY:fracstate0,TSTATE,MSTATE,FSTATE,&   ! model states
         HSTATE                              ! model states (continued)
    USE multiforce, only:NA_VALUE, NA_VALUE_SP              ! NA_VALUE for the forcing
    USE multistate, ONLY:gState,gState_3d                    ! gridded state variables
    USE multiroute, ONLY:MROUTE,AROUTE,AROUTE_3d             ! routed runoff
    USE multistats, ONLY:MSTATS,PCOUNT,MOD_IX                ! access model statistics; counter for param set
    USE multi_flux                                           ! model fluxes
    USE multibands                                           ! elevation bands for snow modeling

    ! code modules
    USE get_gforce_module,ONLY:get_modtim                    ! get model time for a given time step
    USE get_gforce_module,ONLY:get_gforce                    ! get gridded forcing data for a given time step
    USE getPETgrid_module,ONLY:getPETgrid                    ! get gridded PET
    USE par_insert_module                                    ! insert parameters into data structures
    USE str_2_xtry_module                                    ! provide access to the routine str_2_xtry
    USE xtry_2_str_module                                    ! provide access to the routine xtry_2_str
    ! interface blocks
    USE interfaceb, ONLY:ode_int,fuse_solve                  ! provide access to FUSE_SOLVE through ODE_INT
    ! model numerix structures
    USE model_numerix
    USE fuse_deriv_module
    USE fdjac_ode_module
    IMPLICIT NONE
    ! input
    REAL(SP),DIMENSION(:),INTENT(IN)       :: XPAR           ! model parameter set
    LOGICAL(LGT), INTENT(IN)               :: DISTRIBUTED    ! .TRUE. if doing distributed simulations
    INTEGER(I4B), INTENT(IN)               :: NCID_FORC      ! NetCDF ID for the forcing file
    LOGICAL(LGT), INTENT(IN)               :: OUTPUT_FLAG    ! .TRUE. if desire time series output
    LOGICAL(LGT), INTENT(IN), OPTIONAL     :: MPARAM_FLAG    ! .FALSE. (used to turn off writing statistics)
    ! output
    REAL(SP),INTENT(OUT)                   :: RMSE           ! root mean squared error
    ! internal
    LOGICAL(lgt),PARAMETER                 :: computePET=.FALSE. ! flag to compute PET
    REAL(SP)                               :: T1,T2          ! CPU time
    INTEGER(I4B)                           :: ITIM           ! loop through time series
    INTEGER(I4B)                           :: iSpat1,iSpat2  ! loop through spatial dimensions
    INTEGER(I4B)                           :: IPAR           ! loop through model parameters
    REAL(SP)                               :: DT_SUB         ! length of sub-step
    REAL(SP)                               :: DT_FULL        ! length of time step
    REAL(SP), DIMENSION(:), ALLOCATABLE    :: STATE0         ! vector of model states at the start of the time step
    REAL(SP), DIMENSION(:), ALLOCATABLE    :: STATE1         ! vector of model states at the end of the time step
    REAL(SP), DIMENSION(:,:), ALLOCATABLE  :: J              ! used to compute the Jacobian (just as a test)
    REAL(SP), DIMENSION(:), ALLOCATABLE    :: DSDT           ! used to compute the ODE (just as a test)
    INTEGER(I4B)                           :: ITEST,JTEST    ! used to compute a grid of residuals
    REAL(SP)                               :: TEST_A,TEST_B  ! used to compute a grid of residuals
    INTEGER(I4B)                           :: IERR           ! error code
    INTEGER(I4B), PARAMETER                :: CLEN=1024      ! length of character string
    INTEGER(I4B)                           :: ERR            ! error code
    CHARACTER(LEN=CLEN)                    :: MESSAGE        ! error message
    CHARACTER(LEN=CLEN)                    :: CMESSAGE       ! error message of downwind routine
    INTEGER(I4B),PARAMETER::UNT=6  !1701 ! 6
    ! ---------------------------------------------------------------------------------------
    ! allocate state vectors
    ALLOCATE(STATE0(NSTATE),STATE1(NSTATE),STAT=IERR)
    IF (IERR.NE.0) STOP ' problem allocating space for state vectors in fuse_rmse '
    ! increment parameter counter for model output (shared in module MULTISTATS)
    IF (.NOT.PRESENT(MPARAM_FLAG)) THEN
       PCOUNT = PCOUNT + 1
    ELSE
       IF (MPARAM_FLAG) PCOUNT = PCOUNT + 1
    ENDIF

    PRINT *, 'PCOUNT is', PCOUNT

    ! add parameter set to the data structure
    CALL PUT_PARSET(XPAR)
    PRINT *, 'Parameter set added to data structure'
    !DO IPAR=1,NUMPAR; WRITE(*,'(A11,1X,F9.3)') LPARAM(IPAR), XPAR(IPAR); END DO
    ! compute derived model parameters (bucket sizes, etc.)
    CALL PAR_DERIVE(ERR,MESSAGE)
    IF (ERR.NE.0) WRITE(*,*) TRIM(MESSAGE); IF (ERR.GT.0) STOP
    ! initialize model states over the 2D gridded domain'
    DO iSpat1=1,nSpat1
       DO iSpat2=1,nSpat2
          CALL INIT_STATE(fracState0)             ! define FSTATE - fracState0 is shared in MODULE multistate
          !gState(iSpat1,iSpat2) = FSTATE         ! put the state into the 2-d structure
          gState_3d(iSpat1,iSpat2,1) = FSTATE     ! put the state into the 2-3 structure
       END DO
    END DO
    PRINT *, 'Model states initialized over the 2D gridded domain'

    ! allocate 3d data structures for fluxes and snow
    allocate(W_FLUX_3d(nspat1,nspat2,numtim))
    allocate(MBANDS_3d(nspat1,nspat2,numtim))
    !W_FLUX_3d = NA_VALUE_SP

    ! initialize model time step
    DT_SUB  = DELTIM                       ! init stepsize to full step (DELTIM shared in module multiforce)
    DT_FULL = DELTIM                       ! init stepsize to full step (DELTIM shared in module multiforce)
    ! initialize summary statistics
    CALL INIT_STATS()
    CALL CPU_TIME(T1)

    ! loop through time
    PRINT *, 'Running the model'
    DO ITIM=1,NUMTIM            ! (NUMTIM is shared in MODULE multiforce)

     ! if not distributed (i.e., lumped)
     IF(.NOT.distributed)THEN
        ! retrieve data from memory
        gForce(1,1) = aForce(iTim)
     ELSE ! distributed

       ! get the model time - does it do anything else than displaying time?
       CALL get_modtim(warmup_beg+itim,ncid_forc,ierr,message)
       IF(ierr/=0)THEN; PRINT*, TRIM(cmessage); STOP; ENDIF

        ! compute potential ET
        IF(computePET) CALL getPETgrid(ierr,cmessage)
        IF(ierr/=0)THEN; PRINT*, TRIM(cmessage); STOP; ENDIF
        ENDIF

        ! loop through grid points, and run the model for one time step
        DO iSpat1=1,nSpat1
           DO iSpat2=1,nSpat2

              ! NOTE: MFORCE, MSTATE, MBANDS and MFLUX are all scalars, i.e. have zero dimension
              ! MFORCE and MSTATE are initialized using 3d data structures, then FUSE is run, then the
              ! the output is stored in 3d data structures

              ! extract forcing data
              MFORCE = gForce_3d(iSpat1,iSpat2,itim)      ! assign model forcing data

              ! only run FUSE if forcing available
              IF(MFORCE%temp/=NA_VALUE)THEN

                ! extract model states
                MSTATE = gState_3d(iSpat1,iSpat2,itim)      ! refresh model states
                FSTATE = gState_3d(iSpat1,iSpat2,itim)      ! refresh model states

                ! get the vector of model states from the structure
                CALL STR_2_XTRY(FSTATE,STATE0)      ! state at the start of the time step (STATE0) set using FSTATE

                ! initialize model fluxes
                CALL INITFLUXES()                   ! set weighted sum of fluxes to zero

                ! if snow model, call UPDATE_SWE first to calculate snow fluxes and update snow bands
                ! using explicit Euler approach; if not, call QRAINERROR
                SELECT CASE(SMODL%iSNOWM)
                CASE(iopt_temp_index)
                   CALL UPDATE_SWE(DELTIM)
                CASE(iopt_no_snowmod)
                   CALL QRAINERROR()
                CASE DEFAULT
                   message="f-fuse_rmse/SMODL%iSNOWM must be either iopt_temp_index or iopt_no_snowmod"
                   RETURN
                END SELECT

                ! temporally integrate the ordinary differential equations
                CALL ODE_INT(FUSE_SOLVE,STATE0,STATE1,DT_SUB,DT_FULL,IERR,MESSAGE)
                IF (IERR.NE.0) THEN; PRINT *, TRIM(MESSAGE); PAUSE; ENDIF

               ! perform overland flow routing
               CALL Q_OVERLAND()

               ! save the state
               CALL XTRY_2_STR(STATE1,FSTATE)            ! update FSTATE using states at the end of the time step (STATE0)
               gState_3d(iSpat1,iSpat2,itim+1) = FSTATE  ! put the state into the 3-d structure

               ! save forcing data
               IF(distributed)THEN
                  aForce(iTim)%ppt = SUM(gForce_3d(:,:,itim)%ppt)/REAL(SIZE(gForce_3d(:,:,itim)), KIND(sp))
                  aForce(iTim)%pet = SUM(gForce_3d(:,:,itim)%pet)/REAL(SIZE(gForce_3d(:,:,itim)), KIND(sp))
               ENDIF

               ! save instantaneous and routed runoff
               AROUTE(ITIM)%Q_INSTNT = MROUTE%Q_INSTNT  ! save instantaneous runoff
               AROUTE(ITIM)%Q_ROUTED = MROUTE%Q_ROUTED  ! save routed runoff

               ! sanity check
               IF (AROUTE(ITIM)%Q_ROUTED.LT.0._sp) STOP ' Q_ROUTED is less than zero '
               IF (AROUTE(ITIM)%Q_ROUTED.GT.1000._sp) STOP ' Q_ROUTED is enormous '

               ! compute summary statistics
               CALL COMP_STATS()

               ! save fluxes and snow
               W_FLUX_3d(iSpat1,iSpat2,itim) = W_FLUX
               !MBANDS_3d(iSpat1,iSpat2,itim) = MBANDS
               AROUTE_3d(iSpat1,iSpat2,itim) = MROUTE

             ELSE ! INSERT NA VALUES

               !gState_3d(iSpat1,iSpat2,itim) = NA_VALUE_SP
               !W_FLUX_3d(iSpat1,iSpat2,itim) = NA_VALUE_SP
               !AROUTE_3d(iSpat1,iSpat2,itim) = NA_VALUE_SP

             ENDIF ! (is forcing available for this grid cell?)
          END DO  ! (looping thru 2nd spatial dimension)
       END DO  ! (looping thru 1st spatial dimension)

    END DO  ! (itim)

    ! write model output
    PRINT *, 'Write output for all time steps'
    IF (OUTPUT_FLAG) THEN
      CALL PUT_GOUTPUT_3D(PCOUNT,MOD_IX,ITIM)
    ENDIF

    ! get timing information
    CALL CPU_TIME(T2)
    WRITE(*,*) "TIME ELAPSED = ", t2-t1
    ! calculate mean summary statistics

    CALL MEAN_STATS()
    RMSE = MSTATS%RAW_RMSE
    WRITE(unt,'(2(I6,1X),3(F20.15,1X))') MOD_IX, PCOUNT, MSTATS%RAW_RMSE, MSTATS%NASH_SUTT, MSTATS%NUM_FUNCS
    ! write model parameters and summary statistics

    IF (.NOT.PRESENT(MPARAM_FLAG)) THEN
       CALL PUT_PARAMS(PCOUNT,MOD_IX)  ! PCOUNT = index for parameter set; ONEMOD=1 (just one model structure)
       CALL PUT_SSTATS(PCOUNT,MOD_IX)
    ELSE
       IF (MPARAM_FLAG) THEN
          CALL PUT_PARAMS(PCOUNT,MOD_IX)  ! PCOUNT = index for parameter set; ONEMOD=1 (just one model structure)
          CALL PUT_SSTATS(PCOUNT,MOD_IX)
       ENDIF
    ENDIF
    ! deallocate state vectors
    DEALLOCATE(STATE0,STATE1,STAT=IERR); IF (IERR.NE.0) STOP ' problem deallocating state vectors in fuse_rmse '
    ! ---------------------------------------------------------------------------------------
  END SUBROUTINE FUSE_RMSE
END MODULE FUSE_RMSE_MODULE