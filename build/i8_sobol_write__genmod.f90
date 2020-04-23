        !COMPILER-GENERATED INTERFACE MODULE: Tue Apr 14 11:54:06 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE I8_SOBOL_WRITE__genmod
          INTERFACE 
            SUBROUTINE I8_SOBOL_WRITE(M,N,SKIP,R,FILE_OUT_NAME)
              INTEGER(KIND=8) :: N
              INTEGER(KIND=8) :: M
              INTEGER(KIND=8) :: SKIP
              REAL(KIND=8) :: R(M,N)
              CHARACTER(*) :: FILE_OUT_NAME
            END SUBROUTINE I8_SOBOL_WRITE
          END INTERFACE 
        END MODULE I8_SOBOL_WRITE__genmod
