MODULE FUSE_RMSE_MODULE
  IMPLICIT NONE
  CONTAINS

  SUBROUTINE RUN_FUSE(PAR_STR,GRID_FLAG,NCID_FORC,OUTPUT_FLAG,IPSET,MPARAM_FLAG)

    ! ---------------------------------------------------------------------------------------
    ! Creator:
    ! --------
    ! Martyn Clark, 2009
    ! Modified by Brian Henn to include snow model, 6/2013
    ! Modified by Nans Addor to enable grid-based modeling, 9/2016
    ! ---------------------------------------------------------------------------------------
    ! Purpose:
    ! --------
    ! Run FUSE using parameter values provided as input, which can be distributed on a grid
    ! ---------------------------------------------------------------------------------------

    USE nrtype                                               ! variable types, etc.

    ! data modules
    USE model_defn, ONLY:NSTATE,SMODL                        ! number of state variables
    USE model_defnames                                       ! integer model definitions
    USE multiparam                                           ! list of model parameters and parameter structures
    USE multiforce, ONLY: MFORCE,AFORCE,DELTIM,ISTART        ! model forcing data
    USE multiforce, ONLY: numtim_in, itim_in                 ! length of input time series and associated index
    USE multiforce, ONLY: numtim_sim, itim_sim               ! length of simulated time series and associated index
    USE multiforce, ONLY: numtim_sub, itim_sub               ! length of subperiod time series and associated index
    USE multiforce, ONLY: numtim_sub_cur                     ! length of current subperiod
    USE multiforce, ONLY: sim_beg,sim_end                    ! timestep indices
    USE multiforce, ONLY: eval_beg,eval_end                  ! timestep indices
    USE multiforce,only:latitude,longitude                   ! dimension arrays

    USE multiforce, ONLY:nspat1,nspat2                       ! spatial dimensions
    USE multiforce, ONLY:ncid_var                            ! NetCDF ID for forcing variables
    USE multiforce, ONLY:gForce,gForce_3d                    ! gridded forcing data
    USE multistate, ONLY:fracstate0,TSTATE,MSTATE,FSTATE,&   ! model states
         HSTATE                              ! model states (continued)
    USE multiforce, ONLY:NA_VALUE, NA_VALUE_SP               ! NA_VALUE for the forcing
    USE multistate, ONLY:gState,gState_3d                    ! gridded state variables
    USE multiroute, ONLY:MROUTE,AROUTE,AROUTE_3d             ! routed runoff
    USE multistats, ONLY:MSTATS,PCOUNT,MOD_IX                ! access model statistics; counter for param set
    USE multi_flux                                           ! model fluxes
    USE multibands                                           ! elevation bands for snow modeling
    USE set_all_module
    USE fuse_fileManager,only: fuse_mode                     ! fuse_mode

    ! code modules
    USE time_io, ONLY:get_modtim                             ! get model time for a given time step
    USE get_gforce_module, ONLY:get_gforce_3d                ! get gridded forcing data for a range of time steps
    USE getPETgrid_module, ONLY:getPETgrid                   ! get gridded PET
    USE par_insert_module                                    ! insert parameters into data structures
    USE parextract_module                                    ! extract model parameters
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
    TYPE(PARADJ), DIMENSION(:,:), INTENT(IN):: PAR_STR       ! parmeter structure
    LOGICAL(LGT), INTENT(IN)               :: GRID_FLAG      ! .TRUE. if running FUSE on a grid
    INTEGER(I4B), INTENT(IN)               :: NCID_FORC      ! NetCDF ID for the forcing file
    LOGICAL(LGT), INTENT(IN)               :: OUTPUT_FLAG    ! .TRUE. if desire time series output
    INTEGER(I4B), INTENT(IN)               :: IPSET          ! index parameter set
    LOGICAL(LGT), INTENT(IN), OPTIONAL     :: MPARAM_FLAG    ! .FALSE. (used to turn off writing statistics) - TODO: still needed?

    ! internal
    REAL(SP)                               :: PAR_VAL        ! value of one parameter
    REAL(SP)                               :: RMSE           ! root mean squared error
    LOGICAL(lgt),PARAMETER                 :: computePET=.FALSE. ! flag to compute PET
    REAL(SP)                               :: T1,T2          ! CPU time
    INTEGER(I4B)                           :: iSpat1,iSpat2  ! loop through spatial dimensions
    INTEGER(I4B)                           :: ibands         ! loop through elevation bands
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
    INTEGER(I4B),PARAMETER::UNT=6

    ! ---------------------------------------------------------------------------------------

    ! allocate state vectors
    ALLOCATE(STATE0(NSTATE),STATE1(NSTATE),STAT=IERR)
    IF (IERR.NE.0) STOP ' problem allocating space for state vectors in fuse_rmse '

    ! increment parameter counter for model output
    IF (.NOT.PRESENT(MPARAM_FLAG)) THEN
       PCOUNT = PCOUNT + 1
    ELSE
       IF (MPARAM_FLAG) PCOUNT = PCOUNT + 1
    ENDIF

    ! initialize model states and compture derived model parameters
    DO iSpat2=1,nSpat2
      DO iSpat1=1,nSpat1

        ! only set up FUSE within domain defined by elev_mask
        IF(.NOT.elev_mask(iSpat1,iSpat2))THEN

          ! add parameter set to the data structure, i.e., populate MPARAM
          MPARAM=PAR_STR(iSpat1,iSpat2)

          ! compute derived model parameters (bucket sizes, etc.), i.e. populate
          ! DPARAM based on MPARAM
          CALL PAR_DERIVE(ERR,MESSAGE)
          IF (ERR.NE.0) WRITE(*,*) TRIM(MESSAGE); IF (ERR.GT.0) STOP

          ! populate DPARAM_2D - TODO add to output file
          DPARAM_2D(iSpat1,iSpat2)=DPARAM

          ! add parameter set to the data structure - MPARAM and DPARAM now defined abovea
          ! CALL PUT_PARSET(XPAR)
          ! PRINT *, 'Parameter set added to data structure:'
          ! PRINT *, XPAR

          ! check parameter values are within interval bounds allowing for exceedence by 1e-6
          DO IPAR=1,NUMPAR       ! loop through parameters

            PAR_VAL=PAREXTRACT(LPARAM(IPAR)%PARNAME) ! retrieve parameter value

            IF((BL(IPAR)-PAR_VAL)/ABS(PAR_VAL).GT.1e-6) THEN
              PRINT *, 'Error: value for parameter ',TRIM(LPARAM(IPAR)%PARNAME),' (',PAR_VAL,') is smaller than lower bound(',BL(IPAR),')'
              STOP
            ENDIF

            IF((PAR_VAL-BU(IPAR))/ABS(PAR_VAL).GT.1e-6) THEN
              PRINT *, 'Error: value for parameter ',TRIM(LPARAM(IPAR)%PARNAME),' (',PAR_VAL,') is greater than upper bound(',BU(IPAR),')'
              STOP
            ENDIF
          END DO

          ! initialize model states
          CALL INIT_STATE(fracState0)             ! define FSTATE using fracState0
          gState_3d(iSpat1,iSpat2,1) = FSTATE     ! put the state into first time step of 3D structure

          ! initialize elevations bands if snow module is on
          IF (SMODL%iSNOWM.EQ.iopt_temp_index) THEN
            DO IBANDS=1,N_BANDS
              MBANDS_VAR_4d(iSpat1,iSpat2,IBANDS,1)%SWE=0.0_sp         ! band snowpack water equivalent (mm)
              MBANDS_VAR_4d(iSpat1,iSpat2,IBANDS,1)%SNOWACCMLTN=0.0_sp ! new snow accumulation in band (mm day-1)
              MBANDS_VAR_4d(iSpat1,iSpat2,IBANDS,1)%SNOWMELT=0.0_sp    ! snowmelt in band (mm day-1)
              MBANDS_VAR_4d(iSpat1,iSpat2,IBANDS,1)%DSWE_DT=0.0_sp     ! rate of change of band SWE (mm day-1)
            END DO
          ENDIF
        ENDIF
      END DO
    END DO

    IF (SMODL%iSNOWM.EQ.iopt_temp_index) THEN
      PRINT *, 'Model states initialised, including state of',N_BANDS,' elevation bands'
    ELSE
      PRINT *, 'Model states initialised, snow module is off'
    END IF

    ! allocate 3d data structure for fluxes
    ALLOCATE(W_FLUX_3d(nspat1,nspat2,numtim_sub))

    ! initialize model time step
    DT_SUB  = DELTIM                       ! init stepsize to full step
    DT_FULL = DELTIM                       ! init stepsize to full step

    ! initialize summary statistics
    CALL INIT_STATS()
    CALL CPU_TIME(T1)

    ! FUSE2 enables the user to load slices of the forcing
    ! - FUSE1 used to access the input file at each time step, slowing operations
    ! down over large domains on systems with slow I/O. The number of timesteps
    ! of the slices is defined by the user in the filemanager. The default is
    ! that the whole time period needed for the simulation is loaded, but
    ! this can exceed memory capacity when large domains are processed.
    ! To overcome this, a subperiod (slice) of the forcing can be loaded in
    ! memory and used to run FUSE. Then, the results are saved to the
    ! output file, and the next slice of forcing is loaded. This enables FUSE to
    ! run quicker than when forcing is loaded at each time step and grid point,
    ! while also controlling memory usage.

    ! initialise time indices for whole simulation and subperiod
    itim_sub = 1
    itim_sim = 1

    ! RUN FUSE
    DO ITIM_IN=sim_beg,sim_end

      ! if start of subperiod: load forcing
      IF(itim_sub.EQ.1)THEN

        ! determine length of current subperiod
        numtim_sub_cur=MIN(numtim_sub,numtim_sim-itim_sim+1)

        ! load forcing for desired period into gForce_3d
        PRINT *, 'New subperiod: loading forcing for ',numtim_sub_cur,' time steps'
        CALL get_gforce_3d(itim_in,numtim_sub_cur,ncid_forc,err,message)
        IF(err/=0)THEN; WRITE(*,*) 'Error while extracting 3d forcing'; STOP; ENDIF
        PRINT *, 'Forcing loaded. Running FUSE...'

      ENDIF

      ! get the model time
      CALL get_modtim(itim_in,ncid_forc,ierr,message)
      IF(ierr/=0)THEN; PRINT*, TRIM(cmessage); STOP; ENDIF

      ! compute potential ET
      ! IF(computePET) CALL getPETgrid(ierr,cmessage)
      ! IF(ierr/=0)THEN; PRINT*, TRIM(cmessage); STOP; ENDIF

      ! loop through grid points and run the model for one time step
      DO iSpat2=1,nSpat2
        DO iSpat1=1,nSpat1

          ! only run FUSE for grid points within domain defined by elev_mask
          IF(.NOT.elev_mask(iSpat1,iSpat2))THEN

            ! FUSE works with MFORCE, MSTATE, MBANDS, W_FLUX, MROUTE, which are all scalars.
            ! Here we transfer forcing, state, flux variables from the 3D structures to these
            ! variables, run FUSE and then transfer the new values back to the 3D structures.

            ! extract forcing for this grid cell and time step
            MFORCE = gForce_3d(iSpat1,iSpat2,itim_sub)

            ! forcing sanity checks
            if(MFORCE%PPT.lt.0.0) then
              PRINT *, 'Negative precipitation in input file:',MFORCE%PPT
              PRINT *, 'Cell indices:',iSpat1,iSpat2
              PRINT *, 'Cell lon lat:',longitude(iSpat1),latitude(iSpat2)
              PRINT *, 'Elevation bands:',MBANDS_INFO_3d(iSpat1,iSpat2,:)%Z_MID
            stop; endif

            if(MFORCE%PPT.gt.5000.0) then; PRINT *, 'Precipitation greater than 5000 in input file:',iSpat1,iSpat2,MFORCE%PPT; stop; endif
            if(MFORCE%PET.lt.0.0) then; PRINT *, 'Negative PET in input file'; stop; endif
            if(MFORCE%PET.gt.100.0) then; PRINT *, 'PET greater than 100 in input file'; stop; endif
            if(MFORCE%TEMP.lt.-100.0) then; PRINT *, 'Temperature lower than -100 in input file'; stop; endif
            if(MFORCE%TEMP.gt.100.0) then; PRINT *, 'Temperature greater than 100 in input file'; stop; endif

            ! get parameter values
            MPARAM=MPARAM_2D(iSpat1,iSpat2)
            DPARAM=DPARAM_2D(iSpat1,iSpat2)

            ! extract model states for this grid cell and time step
            FSTATE = gState_3d(iSpat1,iSpat2,itim_sub)
            MSTATE = FSTATE                     ! refresh model states
            CALL STR_2_XTRY(FSTATE,STATE0)      ! set state at the start of the time step (STATE0) using FSTATE

            ! initialize model fluxes
            CALL INITFLUXES()                   ! set weighted sum of fluxes to zero

            ! if snow model, call UPDATE_SWE first to calculate snow fluxes and update snow bands
            ! using explicit Euler approach; if not, call QRAINERROR
            SELECT CASE(SMODL%iSNOWM)
            CASE(iopt_temp_index)

              ! load data from multidimensional arrays
              Z_FORCING          = Z_FORCING_grid(iSpat1,iSpat2)                       ! elevation of forcing data (m)
              MBANDS%Z_MID       = MBANDS_INFO_3d(iSpat1,iSpat2,:)%Z_MID               ! band mid-point elevation (m)
              MBANDS%AF          = MBANDS_INFO_3d(iSpat1,iSpat2,:)%AF                  ! fraction of basin area in band (-)
              MBANDS%SWE         = MBANDS_VAR_4d(iSpat1,iSpat2,:,itim_sub)%SWE         ! band snowpack water equivalent (mm)
              MBANDS%SNOWACCMLTN = MBANDS_VAR_4d(iSpat1,iSpat2,:,itim_sub)%SNOWACCMLTN ! new snow accumulation in band (mm day-1)
              MBANDS%SNOWMELT    = MBANDS_VAR_4d(iSpat1,iSpat2,:,itim_sub)%SNOWMELT    ! snowmelt in band (mm day-1)
              MBANDS%DSWE_DT     = MBANDS_VAR_4d(iSpat1,iSpat2,:,itim_sub)%DSWE_DT     ! rate of change of band SWE (mm day-1)

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

            ! runoff sanity check
            IF (MROUTE%Q_ROUTED.LT.0._sp) STOP 'Q_ROUTED is less than zero'

            IF (MROUTE%Q_ROUTED.GT.5000._sp) then
              PRINT *, 'Q_ROUTED is greater than 5000:',MROUTE%Q_ROUTED
              PRINT *, 'Cell indices:',iSpat1,iSpat2
              PRINT *, 'Cell lon lat:',longitude(iSpat1),latitude(iSpat2)
            stop; endif

            ! transfer simulations to corresponding 3D structures
            ! note that the first time step of gState_3d and MBANDS_VAR_4d is defined by initialisation
            ! or simulation over previous subperiod, so saving in itim_sub+1 - and hence, the allocated
            ! length of the temporal dimension of gState_3d and MBANDS_VAR_4d is numtim_sub+1,
            ! but numtim_sub for W_FLUX_3d and AROUTE_3d

            CALL XTRY_2_STR(STATE1,FSTATE)                ! update FSTATE using states at the end of the time step (STATE1)
            gState_3d(iSpat1,iSpat2,itim_sub+1) = FSTATE  ! transfer FSTATE into the 3-d structure
            W_FLUX_3d(iSpat1,iSpat2,itim_sub) = W_FLUX    ! fluxes
            AROUTE_3d(iSpat1,iSpat2,itim_sub) = MROUTE    ! instantaneous and routed runoff

            IF (SMODL%iSNOWM.EQ.iopt_temp_index) THEN

             gState_3d(iSpat1,iSpat2,itim_sub+1)%SWE_TOT = SUM(MBANDS%SWE*MBANDS_INFO_3d(iSpat1,iSpat2,:)%AF) ! weighted average of SWE over all the elevation bands

             MBANDS_VAR_4d(iSpat1,iSpat2,:,itim_sub+1)%SWE         = MBANDS%SWE          ! update MBANDS_VAR_4D
             MBANDS_VAR_4d(iSpat1,iSpat2,:,itim_sub+1)%SNOWACCMLTN = MBANDS%SNOWACCMLTN  !
             MBANDS_VAR_4d(iSpat1,iSpat2,:,itim_sub+1)%SNOWMELT    = MBANDS%SNOWMELT     !
             MBANDS_VAR_4d(iSpat1,iSpat2,:,itim_sub+1)%DSWE_DT     = MBANDS%DSWE_DT      !

            END IF

            ! save forcing data to export to output file
            IF(GRID_FLAG)THEN
               aForce(itim_sub)%ppt = SUM(gForce_3d(:,:,itim_sub)%ppt)/REAL(SIZE(gForce_3d(:,:,itim_sub)), KIND(sp))
               aForce(itim_sub)%pet = SUM(gForce_3d(:,:,itim_sub)%pet)/REAL(SIZE(gForce_3d(:,:,itim_sub)), KIND(sp))
            ENDIF

            ! compute summary statistics
            CALL COMP_STATS()

          ELSE ! insert NA values if grid point outside of domain or forcing not available

            CALL SET_STATE(NA_VALUE_SP) ! includes FSTATE%SWE_TOT
            gState_3d(iSpat1,iSpat2,itim_sub) = FSTATE

            CALL SET_FLUXES(NA_VALUE_SP)
            W_FLUX_3d(iSpat1,iSpat2,itim_sub) = W_FLUX

            CALL SET_ROUTE(NA_VALUE_SP)
            AROUTE_3d(iSpat1,iSpat2,itim_sub) = MROUTE

         ENDIF ! (is forcing available for this grid cell?)
      END DO  ! (looping thru 2nd spatial dimension)
    END DO  ! (looping thru 1st spatial dimension)

    ! if end of subperiod: move state of last time step to first and flush memory
    IF(itim_sub.EQ.numtim_sub_cur)THEN

      PRINT *, 'End of subperiod reached:'

      ! write model output
      IF (OUTPUT_FLAG) THEN
        PRINT *, 'Write output for ',numtim_sub_cur,' time steps starting at indice', itim_sim-numtim_sub_cur+1
        CALL PUT_GOUTPUT_3D(itim_sim-numtim_sub_cur+1,itim_in-numtim_sub_cur+1,numtim_sub_cur,IPSET)
        PRINT *, 'Done writing output'
      ELSE
        PRINT *, 'OUTPUT_FLAG is set on FALSE, no output written'
      END IF

      ! TODO: reinitialize gState_3d and MBANDS_VAR_4d instead of overwritting them

      ! reinitialize states
      !CALL XTRY_2_STR(STATE1,FSTATE)               ! update FSTATE using states at the end of the time step (STATE0)
      gState_3d(:,:,1) = gState_3d(:,:,itim_sub+1)  ! put the state into the 3-d structure
      !W_FLUX_3d(iSpat1,iSpat2,1) = W_FLUX
      !AROUTE_3d(iSpat1,iSpat2,1) = MROUTE
       MBANDS_VAR_4d(:,:,:,1)%SWE         = MBANDS_VAR_4d(:,:,:,itim_sub+1)%SWE
       MBANDS_VAR_4d(:,:,:,1)%SNOWACCMLTN = MBANDS_VAR_4d(:,:,:,itim_sub+1)%SNOWACCMLTN
       MBANDS_VAR_4d(:,:,:,1)%SNOWMELT    = MBANDS_VAR_4d(:,:,:,itim_sub+1)%SNOWMELT
       MBANDS_VAR_4d(:,:,:,1)%DSWE_DT     = MBANDS_VAR_4d(:,:,:,itim_sub+1)%DSWE_DT

       ! save fluxes instantaneous and routed runoff
       W_FLUX_3d(:,:,1) =  W_FLUX_3d(:,:,itim_sub)
       AROUTE_3d(:,:,1) =  AROUTE_3d(:,:,itim_sub)

      ! reset itim_sub
      itim_sub=1

    ELSE ! not the end of subperiod

      ! increment itim_sub
      itim_sub=itim_sub+1

    END IF

    ! increment itim_sim
    itim_sim=itim_sim+1

    END DO  ! (itim)

    ! get timing information
    CALL CPU_TIME(T2)
    WRITE(*,*) "TIME ELAPSED = ", t2-t1

    ! calculate mean summary statistics
    IF(.NOT.GRID_FLAG)THEN

      PRINT *, 'Calculating performance metrics...'
      CALL MEAN_STATS()
      RMSE = MSTATS%RAW_RMSE

      PRINT *, 'Writing parameter values...'
      CALL PUT_PARAMS(PCOUNT)

      PRINT *, 'Writing model statistics...'
      CALL PUT_SSTATS(PCOUNT)

    ENDIF

    ! deallocate state vectors
    DEALLOCATE(W_FLUX_3d)

    DEALLOCATE(STATE0,STATE1,STAT=IERR); IF (IERR.NE.0) STOP ' problem deallocating state vectors in run_fuse'

  END SUBROUTINE RUN_FUSE

  SUBROUTINE FUSE_RMSE(XPAR,GRID_FLAG,NCID_FORC,RMSE,OUTPUT_FLAG,IPSET,MPARAM_FLAG)

    ! ---------------------------------------------------------------------------------------
    ! Creator:
    ! --------
    ! Nans Addor, 2019
    ! ---------------------------------------------------------------------------------------
    ! Purpose:
    ! --------
    ! Wrapper to compute the RMSE for single FUSE model and a single parameter set
    ! ---------------------------------------------------------------------------------------

    USE nrtype                                               ! variable types, etc.
    USE multistats, ONLY:MSTATS                              ! access model statistics
    USE multiparam, ONLY:MPARAM, MPARAM_2D                   ! parameter structure
    USE par_insert_module                                    ! inserts model parameters

    IMPLICIT NONE

    ! input
    REAL(SP),DIMENSION(:),INTENT(IN)       :: XPAR           ! model parameter set
    LOGICAL(LGT), INTENT(IN)               :: GRID_FLAG      ! .TRUE. if running FUSE on a grid
    INTEGER(I4B), INTENT(IN)               :: NCID_FORC      ! NetCDF ID for the forcing file
    LOGICAL(LGT), INTENT(IN)               :: OUTPUT_FLAG    ! .TRUE. if desire time series output
    INTEGER(I4B), INTENT(IN)               :: IPSET          ! index parameter set
    LOGICAL(LGT), INTENT(IN), OPTIONAL     :: MPARAM_FLAG    ! .FALSE. (used to turn off writing statistics)

    ! output
    REAL(SP),INTENT(OUT)                   :: RMSE           ! root mean squared error

    CALL PUT_PARSET(XPAR)                                    ! populate MPARAM
    MPARAM_2D(1,1)=MPARAM                                    ! copy parameter set to MPARAM_2D

    CALL RUN_FUSE(MPARAM_2D,GRID_FLAG,NCID_FORC,OUTPUT_FLAG,IPSET,MPARAM_FLAG)
    RMSE = MSTATS%RAW_RMSE

  END SUBROUTINE FUSE_RMSE

END MODULE FUSE_RMSE_MODULE
