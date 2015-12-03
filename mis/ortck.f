      SUBROUTINE ORTCK (X,MASS,IBUF,NUM,NDIM,GM,ACCUM,EPS)
C
C     ORTCK WILL GENERATE THE GENERALIZED MASS MATRIX FOR CLOSE ROOTS
C     AND MAKE THE EPSILON TEST TO DETERMINE IF THE VECTORS SHOULD BE
C     ORTHOGONALIZED
C
      DOUBLE PRECISION   ACCUM(1)
      DIMENSION          IBUF(1)   ,X(NDIM,1),GM(NUM,1),IM(7)
C     COMMON   /DESCRP/  LENGTH    ,MAJOR(1)
      COMMON   /ZNTPKX/  Z(4)      ,II       ,IEOL
      COMMON   /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW       ,NOREW    ,EOFNRW   ,RSP
C
      IDEN  = 0
      IM(1) = MASS
      CALL RDTRL (IM)
      IF (IM(4) .EQ. 8) IDEN = 1
      IF (IDEN  .EQ. 1) GO TO 10
      CALL GOPEN (MASS,IBUF,0)
   10 K = 1
   20 DO 30 I = 1,NUM
      DO 30 J = 1,NUM
   30 GM(I,J) = 0.
      DO 110 I = 1,NDIM
      DO 40 J = 1,NUM
   40 ACCUM(J) = 0.D0
      IF (IDEN .EQ. 1) GO TO 80
      CALL INTPK (*110,MASS,0,RSP,0)
   50 IF (IEOL .EQ. 1)GO TO 90
      CALL ZNTPKI
   60 DO 70 J = 1,NUM
   70 ACCUM(J) = ACCUM(J) + Z(1)*X(II,J)
      GO TO 50
C
C     IDENTITY
C
   80 IEOL = 1
      II   = I
      Z(1) = 1.0
      GO TO 60
   90 DO 100 J = 1,NUM
      DO 100 M = 1,NUM
  100 GM(J,M) = GM(J,M) + ACCUM(J)*X(I,M)
  110 CONTINUE
      IF (IDEN .EQ. 1) GO TO 120
      CALL REWIND (MASS)
      CALL SKPREC (MASS,1)
  120 GM(1,1) = SQRT(GM(1,1))
      DO 130 I = 2,NUM
      GM(I,I) = SQRT(GM(I,I))
      II = I - 1
      DO 130 J = 1,II
  130 GM(I,J) = GM(I,J)/(GM(I,I)*GM(J,J))
      DO 140 I = 1,NUM
      DO 140 J = 1,NDIM
  140 X(J,I) = X(J,I)/GM(I,I)
      J = 0
  150 DO 170 KK = 1,K
      IF (ABS(GM(K+1,KK)) .LT. EPS) GO TO 170
      J = 1
      DO 160 I = 1,NDIM
  160 X(I,K+1) = X(I,K+1) - GM(K+1,KK)*X(I,KK)
  170 CONTINUE
      K = K + 1
      IF (K .GE. NUM) GO TO 180
      IF (J .EQ.   0) GO TO 150
      GO TO 20
  180 IF (IDEN .EQ. 1) GO TO 190
      CALL CLOSE (MASS,REW)
  190 RETURN
      END
