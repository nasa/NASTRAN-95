      SUBROUTINE PROCES (X)
C
      INTEGER          AXES,AXIS,GP,PRJECT
      REAL             X(3,1),XMIN(3),XMAX(3),MIN,MAX
      DOUBLE PRECISION SUM,V(3)
      COMMON /CONDAS/  CONSTS(5)
      COMMON /BLANK /  SKPCOM(5),NGPSET
      COMMON /XXPARM/  PBUFSZ,PLOTER(5),PENPAP(30),SCALE(5),AXES(6),
     1                 ALPHA,BETA,GAMMA,BETA13,BETA2,VIEW(4),
     2                 VANPUT(8),PRJECT
      COMMON /RSTXXX/  CSTM(3,3),MIN(3),MAX(3),D(3),AVER(3),
     1                 AXIS(3),SIGN(3)
      COMMON /DRWAXS/  G(3,3)
      EQUIVALENCE      (CONSTS(3),RAD)
C
C     INITIALIZATION.
C
      DO 10 I = 1,3
      AXIS(I) = IABS(AXES(I))
      SIGN(I) = 1.
      IF (AXES(I) .LT. 0) SIGN(I) = -1.
      MIN(I)  = +1.E+20
      MAX(I)  = -1.E+20
      IF (PRJECT  .NE. 3) GO TO 10
      XMIN(I) = +1.E+20
      XMAX(I) = -1.E+20
   10 CONTINUE
C
C     CALCULATE THE CO-ORDINATE SYSTEM ROTATION MATRIX.
C
      IF (BETA .GT. -1.E+10) GO TO 20
      IF (PRJECT .NE. 2) BETA = BETA13
      IF (PRJECT .EQ. 2) BETA = BETA2
   20 SINA = SIN(ALPHA/RAD)
      SINB = SIN(BETA /RAD)
      SING = SIN(GAMMA/RAD)
      COSA = COS(ALPHA/RAD)
      COSB = COS(BETA /RAD)
      COSG = COS(GAMMA/RAD)
C
      CSTM(1,1) = COSB*COSG
      CSTM(2,1) = COSA*SING + SINA*SINB*COSG
      CSTM(3,1) = SINA*SING - COSA*SINB*COSG
      CSTM(1,2) =-COSB*SING
      CSTM(2,2) = COSA*COSG - SINA*SINB*SING
      CSTM(3,2) = SINA*COSG + COSA*SINB*SING
      CSTM(1,3) = SINB
      CSTM(2,3) =-SINA*COSB
      CSTM(3,3) = COSA*COSB
C
C     SWITCH AXES + ROTATE THE GRID POINT CO-ORDINATES.
C
      DO 60 GP = 1,NGPSET
      DO 30  I = 1,3
      J    = AXIS(I)
      V(I) = SIGN(I)*X(J,GP)
      IF (PRJECT .NE. 3) GO TO 30
      VAL  = V(I)
      XMIN(I) = AMIN1(XMIN(I),VAL)
      XMAX(I) = AMAX1(XMAX(I),VAL)
   30 CONTINUE
      DO 50 J = 1,3
      SUM = 0.D0
      DO 40 I = 1,3
      SUM = SUM + CSTM(J,I)*V(I)
   40 CONTINUE
      VAL = SUM
      X(J,GP) = VAL
      MIN(J)  = AMIN1(MIN(J),VAL)
      MAX(J)  = AMAX1(MAX(J),VAL)
   50 CONTINUE
   60 CONTINUE
C
C     CALCULATE THE MINIMA-MAXIMA DIFFERENCES + AVERAGES.
C
      DO 70 I = 1,3
      IF (PRJECT .NE. 3) D(I) =  MAX(I) -  MIN(I)
      IF (PRJECT .EQ. 3) D(I) = XMAX(I) - XMIN(I)
      AVER(I) = (MAX(I)+MIN(I))/2.
   70 CONTINUE
C
C     CREATE A X-Y-Z UNIT COORDINATES IN /DRWAXS/ FOR VIEW PLOTTING
C
      DO 90 I = 1,9
   90 G(I,1) = 0.0
      G(1,1) = 1.0
      G(2,2) = 1.0
      G(3,3) = 1.0
C
      DO 130 GP = 1,3
      DO 100  I = 1,3
      J    = AXIS(I)
      V(I) = SIGN(I)*G(J,GP)
  100 CONTINUE
      DO 120 J = 1,3
      SUM = 0.D0
      DO 110 I = 1,3
      SUM = SUM + CSTM(J,I)*V(I)
  110 CONTINUE
  120 G(J,GP) = SUM
  130 CONTINUE
C
      RETURN
      END
