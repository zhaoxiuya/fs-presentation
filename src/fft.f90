PROGRAM MAIN
    IMPLICIT NONE
    INTEGER, PARAMETER :: DP = KIND(1.0D0)
    INTEGER :: N, M, K, I, J, TMP
    INTEGER, ALLOCATABLE :: RET(:)
    COMPLEX(DP), ALLOCATABLE :: X(:), Y(:)
    CHARACTER(LEN=4000010) :: XC, YC, BUF
    READ (*, *) XC, YC
    N = LEN_TRIM(XC)
    M = LEN_TRIM(YC)
    K = 1
    DO WHILE (K <= N+M)
    K = K * 2
    END DO
    ALLOCATE(X(0:K-1))
    X = CMPLX(0.0_DP, 0.0_DP)
    DO I = 0, N - 1
        X(I) = ICHAR(XC(N-I:N-I)) - ICHAR('0')
    END DO
    ALLOCATE(Y(0:K-1))
    Y = CMPLX(0.0_DP, 0.0_DP)
    DO I = 0, M - 1
        Y(I) = ICHAR(YC(M-I:M-I)) - ICHAR('0')
    END DO
    CALL FFT(X, .FALSE.)
    CALL FFT(Y, .FALSE.)
    X = X * Y
    CALL FFT(X, .TRUE.)
    ALLOCATE(RET(0:K-1))
    RET = NINT(REAL(X))
    DO I = 0, K - 2
        RET(I+1) = RET(I+1) + RET(I) / 10
        RET(I) = MODULO(RET(I), 10)
    END DO
    J = K - 1
    DO WHILE (0<J .AND. RET(J)==0)
        J = J - 1
    END DO
    TMP = 1
    DO I = J, 0, -1
        BUF(TMP:TMP) = CHAR(RET(I)+ICHAR('0'))
        TMP = TMP + 1
    END DO
    WRITE(*, '(A)') BUF(1:TMP-1)

CONTAINS

    SUBROUTINE FFT(IV, INV)
        IMPLICIT NONE
        INTEGER, PARAMETER :: DP = KIND(1.0D0)
        COMPLEX(DP), INTENT(INOUT), TARGET :: IV(:)
        LOGICAL, INTENT(IN) :: INV
        COMPLEX(DP), ALLOCATABLE :: TW(:)
        COMPLEX(DP), POINTER :: V(:)
        COMPLEX(DP) :: X, Y
        INTEGER :: I, J, K, N, K2, STEP
        REAL(DP) :: ANG
        REAL(DP), PARAMETER :: PI = 3.14159265358979323846_DP
        N = SIZE(IV)
        V(0:N-1) => IV(:)
        ALLOCATE(TW(0:N/2-1))
        DO I = 0, N / 2 - 1
            ANG = 2.0_DP * ACOS(-1.0_DP) * I / N
            IF (INV) ANG = -ANG
            TW(I) = CMPLX(COS(ANG), SIN(ANG), DP)
        END DO
        J = 0
        DO I = 1, N-1
            K = SHIFTR(N, 1)
            DO WHILE (IAND(J, K) /= 0)
                J = IEOR(J, K)
                K = SHIFTR(K, 1)
            END DO
            J = IEOR(J, K)
            IF (I < J) THEN
                X = V(I)
                V(I) = V(J)
                V(J) = X
            END IF
        END DO
        K = 2
        DO WHILE (K <= N)
            K2 = SHIFTR(K, 1)
            STEP = N / K
            DO I = 0, N-1, K
                DO J = 0, K2-1
                    X = V(I + J)
                    Y = V(I + J + K2) * TW(J * STEP)
                    V(I + J)      = X + Y
                    V(I + J + K2) = X - Y
                END DO
            END DO
            K = SHIFTL(K, 1)
        END DO
        IF (INV) THEN
            DO I=0, N-1
                V(I) = V(I) / REAL(N, DP)
            END DO
        END IF
        DEALLOCATE(TW)
    END SUBROUTINE FFT
END PROGRAM MAIN
