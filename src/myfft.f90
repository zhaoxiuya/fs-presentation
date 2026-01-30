MODULE MYFFT
    IMPLICIT NONE

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

    SUBROUTINE NTT(IV, INV)
        IMPLICIT NONE
        INTEGER, PARAMETER :: LL = SELECTED_INT_KIND(18)
        INTEGER(LL), INTENT(INOUT), TARGET :: IV(:)
        LOGICAL, INTENT(IN) :: INV
        INTEGER(LL), POINTER :: V(:)
        INTEGER(LL) :: X, Y, WL, W
        INTEGER(LL) :: MODV = 998244353, G = 3_LL
        INTEGER(LL) :: I, J, K, N, STEP
        N = SIZE(IV)
        V(0:N-1) => IV(:)
        N = SIZE(V)
        J = 0_LL
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
        K = 2_LL
        DO WHILE (K <= N)
            WL = POW(G, (MODV-1_LL)/K)
            IF (INV) WL = POW(WL, MODV-2_LL)
            STEP = N / K
            DO I = 0, N-1, K
                W = 1_LL
                DO J = 0, SHIFTR(K, 1)-1
                    X = MODULO(V(I + J), MODV)
                    Y = MODULO(V(I + J + SHIFTR(K, 1)) * W, MODV)
                    V(I + J)      = MODULO(X + Y, MODV)
                    V(I + J + SHIFTR(K, 1)) = MODULO(X - Y, MODV)
                    W = MODULO(W * WL, MODV)
                END DO
            END DO
            K = SHIFTL(K, 1_LL)
        END DO
        IF (INV) THEN
            W = POW(N, MODV-2_LL)
            DO I=0, N-1
                V(I) = MODULO(V(I)*W, MODV)
            END DO
        END IF
    END SUBROUTINE NTT

    FUNCTION POW(BASE, EXP) RESULT(RET)
        IMPLICIT NONE
        INTEGER, PARAMETER :: LL = SELECTED_INT_KIND(18)
        INTEGER(LL), INTENT(IN) :: BASE, EXP
        INTEGER(LL) :: RET, B, E
        INTEGER(LL) :: MODV = 998244353, G = 3_LL
        RET = 1_LL
        B = MODULO(BASE, MODV)
        E = EXP
        DO WHILE (0_LL < E)
            IF (MOD(E, 2_LL)==1_LL) RET = MODULO(RET * B, MODV)
            B = MODULO(B*B, MODV)
            E = E / 2_LL
        END DO
    END FUNCTION POW

END MODULE
