PROGRAM MAIN
    IMPLICIT NONE
    INTEGER, PARAMETER :: LL = SELECTED_INT_KIND(38)

    INTEGER(LL) :: N, M, K, I, J, TMP, CNT, RET
    INTEGER(LL), ALLOCATABLE :: X(:), Y(:)
    INTEGER(LL) :: MODV = 9223372036737335297_LL, G = 3_LL

    READ *, N, M

    K = 1
    DO WHILE (K <= N+M)
        K = K * 2
    END DO

    ALLOCATE(X(0:K-1), Y(0:K-1))

    X = 0_LL
    Y = 0_LL

    READ *, (X(I), I=0, N)
    READ *, (Y(I), I=0, M)

    CALL FFT(X, .FALSE.)
    CALL FFT(Y, .FALSE.)
    X = MODULO(X * Y, MODV)
    CALL FFT(X, .TRUE.)

    RET = 0_LL
    DO I=0, N+M
        RET = IEOR(RET, X(I))
    END DO

    PRINT "(I0)", RET

CONTAINS
    SUBROUTINE FFT(V, INV)
        IMPLICIT NONE
        INTEGER, PARAMETER :: LL = SELECTED_INT_KIND(38)

        INTEGER(LL), INTENT(INOUT) :: V(0:)
        LOGICAL, INTENT(IN) :: INV
        INTEGER(LL) :: X, Y, WL, W
        INTEGER(LL) :: MODV = 9223372036737335297_LL, G = 3_LL
        INTEGER(LL) :: I, J, K, N, K2, STEP

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
            K2 = SHIFTR(K, 1)
            STEP = N / K
            DO I = 0, N-1, K
                W = 1_LL
                DO J = 0, K2-1
                    X = MODULO(V(I + J), MODV)
                    Y = MODULO(V(I + J + K2) * W, MODV)
                    V(I + J)      = MODULO(X + Y, MODV)
                    V(I + J + K2) = MODULO(X - Y, MODV)
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

    END SUBROUTINE FFT

    FUNCTION POW(BASE, EXP) RESULT(RET)
        IMPLICIT NONE
        INTEGER, PARAMETER :: LL = SELECTED_INT_KIND(38)
        INTEGER(LL), INTENT(IN) :: BASE, EXP
        INTEGER(LL) :: RET, B, E
        INTEGER(LL) :: MODV = 9223372036737335297_LL, G = 3_LL
        RET = 1_LL
        B = MODULO(BASE, MODV)
        E = EXP
        DO WHILE (0_LL < E)
            IF (MOD(E, 2_LL)==1_LL) RET = MODULO(RET * B, MODV)
            B = MODULO(B*B, MODV)
            E = E / 2_LL
        END DO
    END FUNCTION POW
END PROGRAM MAIN
