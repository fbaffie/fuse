        !COMPILER-GENERATED INTERFACE MODULE: Tue Apr 14 11:54:01 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GAMMP_V__genmod
          INTERFACE 
            FUNCTION GAMMP_V(A,X)
              REAL(KIND=8), INTENT(IN) :: A(:)
              REAL(KIND=8), INTENT(IN) :: X(:)
              REAL(KIND=8) :: GAMMP_V(SIZE(X))
            END FUNCTION GAMMP_V
          END INTERFACE 
        END MODULE GAMMP_V__genmod
