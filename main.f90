PROGRAM MAIN
    IMPLICIT NONE
    INTEGER, PARAMETER :: DP = KIND(1.0D0)
    INTEGER :: N

END PROGRAM MAIN

SUBROUTINE FFT(V, INV)
    IMPLICIT NONE
    INTEGER, PARAMETER :: DP = KIND(1.0D0)
    COMPLEX(DP), INTENT(INOUT) :: V(:)
    LOGICAL, INTENT(IN) :: INV
    INTEGER :: I, J, K, N, K2
    COMPLEX(DP) :: W, U, X, Y
    REAL(DP) :: ANG
    N = SIZE(V)
    J = 0
    DO I = 1, N-1
        K = SHIFTR(N, 1)
        DO WHILE IAND(J, K) /= 0
            J = IEOR(J, K)
            K = SHIFTR(K, 1)
        END DO
        J = IEOR(J, K)
        IF I < J THEN
            W = V(I+1)
            V(I+1) = V(J+1)
            V(J+1) = W
        END IF
    END DO
    K = 2
    DO WHILE K <= N
        ANG = 2.0_DP * ACOS(-1.0_DP) / K
        IF (INV) ANG = -ANG
        U = CMPLX(COS(ANG), SIN(ANG), DP)
        DO I = 0, N-1, K
            W = (1.0_DP, 0.0_DP)
            DO J = 0, SHIFTR(K, 1)-1
                X = V(I + J + 1)
                Y = V(I + J + SHIFTR(K, 1) + 1) * W
                V(I + J + 1) = X + Y
                V(I + J + SHIFTR(K, 1) + 1) = X - Y
                W *= U
            END DO
        END DO
        K = SHIFTL(K, 1)
    END DO
    IF INV THEN
        DO I=1, N
            V(I) = V(I) / REAL(N, DP)
        END DO
    END IF
END SUBROUTINE FFT
