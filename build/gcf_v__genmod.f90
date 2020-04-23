        !COMPILER-GENERATED INTERFACE MODULE: Tue Apr 14 11:54:01 2020
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE GCF_V__genmod
          INTERFACE 
            FUNCTION GCF_V(A,X,GLN)
              REAL(KIND=8), INTENT(IN) :: A(:)
              REAL(KIND=8), INTENT(IN) :: X(:)
              REAL(KIND=8) ,OPTIONAL, INTENT(OUT) :: GLN(:)
              REAL(KIND=8) :: GCF_V(SIZE(A))
            END FUNCTION GCF_V
          END INTERFACE 
        END MODULE GCF_V__genmod
