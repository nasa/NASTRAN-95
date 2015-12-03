      SUBROUTINE SSLOT2 (IOPT,IPART,BRANCH,EIGEN)
C
C     THE OPTIONS ARE
C         IOPT   -  CSLOT3 = 0,  CSLOT4 = 1
C         IPART  -  FIRST  = 1,  SECOND = 2
C         BRANCH -  SDR2 PROCESS CODE WORD
C
      INTEGER         SIL      ,ELID     ,BRANCH
      DIMENSION       EIGEN(3)
      COMMON /SDR2X4/ DUMY(35) ,IVEC
      COMMON /SDR2X7/ ELID     ,SIL(4)   ,SV(95)   ,ID1     ,VELR(11),
     1                ID2      ,VELI(11)
      COMMON /ZZZZZZ/ ZZ(1)
      COMMON /CONDAS/ CONSTS(5)
      EQUIVALENCE     (CONSTS(2),TWOPI)
C
      KL  = IOPT + 3
      KL2 = KL + 2
      IF (IPART .EQ. 2) GO TO 20
      DO 10 I = 1,11
      VELR(I) = 0.0
   10 VELI(I) = 0.0
   20 X   = 1.0
      Y   = 0.0
      IF (BRANCH .EQ. 2) X = SQRT(ABS(EIGEN(2)))
      IF (BRANCH .EQ. 5) X = TWOPI*EIGEN(1)
      IF (X    .NE. 0.0) X = 1.0/X
      IF (BRANCH .NE. 9) GO TO 30
      EM  = EIGEN(2)**2 + EIGEN(3)**2
      IF (EM .EQ. 0.0) GO TO 30
      X   = EIGEN(2)/EM
      Y   =-EIGEN(3)/EM
   30 IF (IPART .NE. 2) GO TO 40
      EM  = X
      X   =-Y
      Y   = EM
   40 ID1 = ELID
      ID2 = ELID
C
      DO 80 I = 1,KL
      K   = IVEC + SIL(I) - 1
      IF (X .EQ. 0.0) GO TO 60
      DO 50 J = 1,KL2
      IJ  = KL*(J-1) + I
C
      VELR(J) = SV(IJ)*ZZ(K)*X + VELR(J)
   50 CONTINUE
   60 IF (Y .EQ. 0.0) GO TO 80
      DO 70 J = 1,KL2
      IJ  = KL*(J-1) + I
      VELI(J) = SV(IJ)*ZZ(K)*Y + VELI(J)
   70 CONTINUE
   80 CONTINUE
      RETURN
      END
