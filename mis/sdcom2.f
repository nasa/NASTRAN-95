      SUBROUTINE SDCOM2 (P,AC,WA,WB)
C
      INTEGER          AC(1),ROW,C,START
      DOUBLE PRECISION P(1),WA(1),WB(1),PI,EPSI
      COMMON /SDCOMX/  ROW,C,SPFLG,START,FRSTPC,LASTPL,LASTI
      DATA    EPSI  /  1.D-36/
C
      J  = 1
      L  = 1
      K1 = LASTPL + 1
      IEND   = MIN0(LASTPL,LASTI)
      ISTART = MAX0(K1,START)
      IF (C .EQ. LASTPL) GO TO 200
      IF (START .GT. LASTPL) GO TO 100
      DO 48 I = START,IEND
      PI = -P(I)/P(1)
      IF (DABS(PI) .LT. EPSI) PI = 0.D0
      IJMK = J - I
      ILMK = L - I
      DO 10 K = I,LASTPL
      WB(K+IJMK) = PI*P(K) + WA(K+ILMK)
   10 CONTINUE
      L = ILMK + K1
      DO 18 K = K1,C
      IF (AC(K) .GT. 0) GO TO 12
      WB(K+IJMK) = PI*P(K)
      GO TO 18
   12 WB(K+IJMK) = PI*P(K) + WA(L)
      L = L + 1
   18 CONTINUE
      J = IJMK + C + 1
      P(I) = PI
   48 CONTINUE
      IF (LASTPL .GE. LASTI) RETURN
  100 DO 148 I = ISTART,LASTI
      PI = -P(I)/P(1)
      IF (DABS(PI) .LT. EPSI) PI = 0.D0
      IJMK = J - I
      IF (AC(I) .LT. 0) GO TO 120
      DO 118 K = I,C
      IF (AC(K) .GT. 0) GO TO 112
      WB(K+IJMK) = PI*P(K)
      GO TO 118
  112 WB(K+IJMK) = PI*P(K) + WA(L)
      L = L + 1
  118 CONTINUE
      GO TO 140
  120 DO 128 K = I,C
      WB(K+IJMK) = PI*P(K)
  128 CONTINUE
  140 J = IJMK + C + 1
      P(I) = PI
  148 CONTINUE
      RETURN
C
  200 IF (START .GT. LASTPL) GO TO 300
      DO 248 I = START,IEND
      PI = -P(I)/P(1)
      IF (DABS(PI) .LT. EPSI) PI = 0.D0
      IJMK = J - I
      ILMK = L - I
      DO 238 K = I,LASTPL
      WB(K+IJMK) = PI*P(K) + WA(K+ILMK)
  238 CONTINUE
      J = IJMK + K1
      L = ILMK + K1
      P(I) = PI
  248 CONTINUE
      IF (LASTPL .GE. LASTI) RETURN
  300 DO 348 I = ISTART,LASTI
      PI = -P(I)/P(1)
      IF (DABS(PI) .LT. EPSI) PI = 0.D0
      IJMK = J - I
      IF (AC(I) .LT. 0) GO TO 320
      DO 318 K = I,C
      IF (AC(K) .GT. 0) GO TO 312
      WB(K+IJMK) = PI*P(K)
      GO TO 318
  312 WB(K+IJMK) = PI*P(K) + WA(L)
      L = L + 1
  318 CONTINUE
      GO TO 340
  320 DO 328 K = I,C
      WB(K+IJMK) = PI*P(K)
  328 CONTINUE
  340 J = IJMK + C + 1
      P(I) = PI
  348 CONTINUE
      RETURN
      END
