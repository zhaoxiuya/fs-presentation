PROGRAM MAIN
    IMPLICIT NONE
    INTEGER, PARAMETER :: DP = KIND(1.0D0)

    INTEGER :: N, M, K, I, J
    COMPLEX(DP), ALLOCATABLE :: X, Y
    REAL(DP) :: TMP

    READ *, N

    M = N * 2
    K = 0
    DO WHILE 0 < M
        M = SHIFTR(M, 1)
        K = K + 1
    END DO
    K = SHIFTL(1, K)

    ALLOCATE(X(0:2*K-1))
    ALLOCATE(Y(0:2*K-1))

    DO I=0, N-1
        READ *, TMP
        X(I)    = (TMP, 0.0_DP)
        X(I+N)  = (TMP, 0.0_DP)
    END DO

    DO I=0, N-1
        READ *, TMP
        Y(I)    = (TMP, 0.0_DP)
        Y(I+N)  = (TMP, 0.0_DP)
    END DO

    FFT(X, .FALSE.)
    FFT(Y, .FALSE.)
    DO I=0, K-1
        X(I) = X(I) * Y(I)
    END DO
    FFT(X, .TRUE.)

    PRINT *, MAXVAL(NINT(REAL(X)))

END PROGRAM MAIN

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
    V(0:N-1) => IV

    ALLOCATE(TW(0:N/2-1))
    DO I = 0, N / 2 - 1
        ANG = 2.0_DP * ACOS(-1.0_DP) * I / N
        IF (INV) ANG = -ANG
        TW(I) = CMPLX(COS(ANG), SIN(ANG), DP)
    END DO

    J = 0
    DO I = 1, N-1
        K = SHIFTR(N, 1)
        DO WHILE IAND(J, K) /= 0
            J = IEOR(J, K)
            K = SHIFTR(K, 1)
        END DO
        J = IEOR(J, K)
        IF I < J THEN
            W = V(I)
            V(I) = V(J)
            V(J) = W
        END IF
    END DO

    K = 2
    DO WHILE K <= N
        K2 = SHIFTR(K, 1)
        STEP = N / K

        !$OMP PARALLEL DO PRIVATE(I,J,X,Y)
        DO I = 0, N-1, K
            !DIR$ SIMD
            DO J = 0, K2-1
                X = V(I + J)
                Y = V(I + J + K2) * TW(J * STEP)
                V(I + J)      = X + Y
                V(I + J + K2) = X - Y
            END DO
        END DO
        !$OMP END PARALLEL DO

        K = SHIFTL(K, 1)
    END DO

    IF INV THEN
        DO I=0, N-1
            V(I) = V(I) / REAL(N, DP)
        END DO
    END IF

    DEALLOCATE(TW)
END SUBROUTINE FFT
