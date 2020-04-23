        !COMPILER-GENERATED INTERFACE MODULE: Tue Apr 14 11:54:02 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LOGISMOOTH__genmod
          INTERFACE 
            PURE FUNCTION LOGISMOOTH(STATE,STATE_MAX,PSMOOTH)
              REAL(KIND=8), INTENT(IN) :: STATE
              REAL(KIND=8), INTENT(IN) :: STATE_MAX
              REAL(KIND=8), INTENT(IN) :: PSMOOTH
              REAL(KIND=8) :: LOGISMOOTH
            END FUNCTION LOGISMOOTH
          END INTERFACE 
        END MODULE LOGISMOOTH__genmod
