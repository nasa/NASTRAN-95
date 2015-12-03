      SUBROUTINE MPY3P (Z,IZ,DZ)
C*****
C    PERFORMS MULTIPLICATION AND SUMMATION.
C*****
      DOUBLE PRECISION DZ(1),DFACT
C
C
C
      INTEGER CODE,PREC
      INTEGER ZPNTRS
C
C
C
      DIMENSION Z(1),IZ(1)
C
C
C SUBROUTINE CALL PARAMETERS
      COMMON / MPY3CP / ITRL,ICORE,N,NCB,M,DUM1(3),ZPNTRS(22),LAEND,
     1                  DUM2(6),KA,KB
C FILES
      COMMON / MPY3TL / FILEA(7),FILEB(7),FILEE(7),FILEC(7),SCR1,SCR2,
     1                  SCR,LKORE,CODE,PREC,LCORE,SCR3(7),BUF1,BUF2,
     2                  BUF3,BUF4,E
C
C
C
      EQUIVALENCE     (FACT,DFACT)
C OPEN CORE POINTERS
      EQUIVALENCE     (IPOINT,ZPNTRS(3)),      (IACOLS,ZPNTRS(5)),
     *                (ITRANS,ZPNTRS(7)),      (IC,ZPNTRS(9)),
     *                (IBCOLS,ZPNTRS(11)),     (IAKJ,ZPNTRS(21))
C*****
C    LOOP FOR ACCUMULATING SUMS.
C*****
      KJ = IAKJ + KA - 1
      KJ2 = (IAKJ - 1)/2 + KA
      KB = IBCOLS + PREC*((KB - 1)*N - 1)
      IF (CODE .EQ. 2 .OR. ICORE .EQ. 1) GO TO 100
C*****
C    A(T)BA CASE.
C*****
      LP = IPOINT - 1
      DO 90 L=1,N
C CALCULATE FACTOR = B(LK)*A(KJ) TO BE MULTIPLIED TO NON-ZERO TERMS IN
C LTH COLUMN OF A(T)
      KB = KB + PREC
      LP = LP + 1
      IF (IZ(LP) .EQ. 0) GO TO 90
      IF (PREC .EQ. 2) GO TO 10
      IF (Z(KB) .EQ. 0.0) GO TO 90
      FACT = Z(KB)*Z(KJ)
      GO TO 20
   10 KB2 = (KB + 1)/2
      IF (DZ(KB2) .EQ. 0.0D0) GO TO 90
      DFACT = DZ(KB2)*DZ(KJ2)
   20 I1 = IZ(LP)
      IF (L .EQ. N) GO TO 40
C ACCUMULATE SUMS FOR NON-ZERO TERMS IN COLUMN L OF A(T)
      L1 = L + 1
      LLP = LP
      DO 30 LL=L1,N
      LLP = LLP + 1
      IF (IZ(LLP) .NE. 0) GO TO 50
   30 CONTINUE
   40 I2 = LAEND
      GO TO 60
   50 I2 = IZ(LLP) - 1
   60 IAC = IACOLS + I1 - 2
      IF (PREC .EQ. 2) GO TO 80
C SINGLE PRECISION CASE
      IAT = ITRANS + I1 - 2
      DO 70 I=I1,I2
      IAC = IAC + 1
      IAT = IAT + 1
      II = IC + IZ(IAC) - 1
   70 Z(II) = Z(II) + Z(IAT)*FACT
      GO TO 90
C DOUBLE PRECISION CASE
   80 IAT = (ITRANS - 3)/2 + I1
      DO 85 I=I1,I2
      IAC = IAC + 1
      IAT = IAT + 1
      II = (IC - 1)/2 + IZ(IAC)
   85 DZ(II) = DZ(II) + DZ(IAT)*DFACT
      III = (IC - 1)/2 + 1
   90 CONTINUE
      GO TO 999
C*****
C    BA CASE.
C*****
  100 IF (PREC .EQ. 2) GO TO 140
C SINGLE PRECISION CASE
      II = IC - 1
      DO 130 I=1,N
      II = II + 1
      KB = KB + 1
      IF (Z(KB) .EQ. 0.0) GO TO 130
      Z(II) = Z(II) + Z(KB)*Z(KJ)
  130 CONTINUE
      GO TO 999
C DOUBLE PRECISION CASE
  140 II = (IC - 1)/2
      KB = (KB + 1)/2
      DO 150 I=1,N
      II = II + 1
      KB = KB + 1
      IF (DZ(KB) .EQ. 0.0D0) GO TO 150
      DZ(II) = DZ(II) + DZ(KB)*DZ(KJ2)
  150 CONTINUE
C
  999 RETURN
      END
