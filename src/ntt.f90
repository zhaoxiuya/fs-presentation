PROGRAM MAIN
    IMPLICIT NONE
    INTEGER, PARAMETER :: LL = SELECTED_INT_KIND(38)
    INTEGER(LL) :: MODV = 998244353, G = 3_LL
    INTEGER(LL) :: N, M, L, K, I, J, TMP
    INTEGER(LL), ALLOCATABLE :: X(:), Y(:), RET(:)
    CHARACTER(LEN=4000010) :: XC, YC, BUF
    READ (*, *) XC, YC
    N = LEN_TRIM(XC)
    M = LEN_TRIM(YC)
    K = 1
    DO WHILE (K <= N+M)
        K = K * 2
    END DO
    ALLOCATE(X(0:K-1))
    X = 0
    DO I = 0, N - 1
        X(I) = ICHAR(XC(N-I:N-I)) - ICHAR('0')
    END DO
    ALLOCATE(Y(0:K-1))
    Y = 0
    DO I = 0, M - 1
        Y(I) = ICHAR(YC(M-I:M-I)) - ICHAR('0')
    END DO
    CALL FFT(X, .FALSE.)
    CALL FFT(Y, .FALSE.)
    X = MODULO(X * Y, MODV)
    CALL FFT(X, .TRUE.)
    DO I = 0, K - 2
        X(I+1) = X(I+1) + X(I) / 10
        X(I) = MODULO(X(I), 10)
    END DO
    J = K - 1
    DO WHILE (0<J .AND. X(J)==0)
        J = J - 1
    END DO
    TMP = 1
    DO I = J, 0, -1
        BUF(TMP:TMP) = CHAR(X(I)+ICHAR('0'))
        TMP = TMP + 1
    END DO
    WRITE(*, '(A)') BUF(1:TMP-1)

CONTAINS

    SUBROUTINE FFT(V, INV)
        IMPLICIT NONE
        INTEGER, PARAMETER :: LL = SELECTED_INT_KIND(38)
        INTEGER(LL), INTENT(INOUT) :: V(0:)
        LOGICAL, INTENT(IN) :: INV
        INTEGER(LL) :: X, Y, WL, W
        INTEGER(LL) :: MODV = 998244353, G = 3_LL
        INTEGER(LL) :: I, J, K, N, STEP
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
    END SUBROUTINE FFT

    FUNCTION POW(BASE, EXP) RESULT(RET)
        IMPLICIT NONE
        INTEGER, PARAMETER :: LL = SELECTED_INT_KIND(38)
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
END PROGRAM MAIN
