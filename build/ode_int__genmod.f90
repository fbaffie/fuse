        !COMPILER-GENERATED INTERFACE MODULE: Tue Apr 14 11:54:04 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ODE_INT__genmod
          INTERFACE 
            SUBROUTINE ODE_INT(MODL_SOLVE,STATE_START,STATE_END,DT_SUB, &
     &DT_FULL,IERR,MESSAGE)
              INTERFACE 
                SUBROUTINE MODL_SOLVE(CALCDSDT,IE_SOLVE,SI_SOLVE,       &
     &B_IMPOSE,AVG_FLUX,ADD_FLUX,NEWSTATE,DT,S0,S1,DSDT,NEWSTEP,        &
     &CONVCHECK,NITER,SOLUTION,HBOUND,IERR,MESSAGE)
                  LOGICAL(KIND=4) ,OPTIONAL, INTENT(IN) :: CALCDSDT
                  LOGICAL(KIND=4) ,OPTIONAL, INTENT(IN) :: IE_SOLVE
                  LOGICAL(KIND=4) ,OPTIONAL, INTENT(IN) :: SI_SOLVE
                  LOGICAL(KIND=4) ,OPTIONAL, INTENT(IN) :: B_IMPOSE
                  LOGICAL(KIND=4) ,OPTIONAL, INTENT(IN) :: AVG_FLUX
                  LOGICAL(KIND=4) ,OPTIONAL, INTENT(IN) :: ADD_FLUX
                  LOGICAL(KIND=4) ,OPTIONAL, INTENT(IN) :: NEWSTATE
                  REAL(KIND=8) ,OPTIONAL, INTENT(IN) :: DT
                  REAL(KIND=8) ,OPTIONAL, INTENT(IN) :: S0(:)
                  REAL(KIND=8) ,OPTIONAL, INTENT(OUT) :: S1(:)
                  REAL(KIND=8) ,OPTIONAL, INTENT(INOUT) :: DSDT(:)
                  LOGICAL(KIND=4) ,OPTIONAL, INTENT(IN) :: NEWSTEP
                  LOGICAL(KIND=4) ,OPTIONAL, INTENT(IN) :: CONVCHECK
                  INTEGER(KIND=4) ,OPTIONAL, INTENT(OUT) :: NITER
                  INTEGER(KIND=4) ,OPTIONAL, INTENT(IN) :: SOLUTION
                  LOGICAL(KIND=4) ,OPTIONAL, INTENT(IN) :: HBOUND
                  INTEGER(KIND=4), INTENT(OUT) :: IERR
                  CHARACTER(*), INTENT(OUT) :: MESSAGE
                END SUBROUTINE MODL_SOLVE
              END INTERFACE 
              REAL(KIND=8), INTENT(IN) :: STATE_START(:)
              REAL(KIND=8), INTENT(OUT) :: STATE_END(:)
              REAL(KIND=8), INTENT(INOUT) :: DT_SUB
              REAL(KIND=8), INTENT(IN) :: DT_FULL
              INTEGER(KIND=4), INTENT(OUT) :: IERR
              CHARACTER(*), INTENT(OUT) :: MESSAGE
            END SUBROUTINE ODE_INT
          END INTERFACE 
        END MODULE ODE_INT__genmod
