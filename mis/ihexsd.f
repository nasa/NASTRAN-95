      SUBROUTINE IHEXSD (TYPE,SHP,DSHP,JACOB,DETJ,EID,XI,ETA,ZETA,BXYZ)
C
C     DOUBLE PRECISION VERSION
C
C     ISOPARAMETRIC UTILITY ROUTINE.  THIS ROUTINE WILL COMPUTE
C     VALUES OF THE SHAPE FUNCTIONS, THEIR DERIVATIVES WITH RESPECT TO
C     XI,ETA, AND ZETA, THE JACOBIAN MATRIX INVERSE, AND ITS DETERMINANT
C
C                       TYPE = 1       IHEX1
C                       TYPE = 2       IHEX2
C                       TYPE = 3       IHEX3
C
C     SHP    = VALUES OF SHAPE FUNCTIONS
C     DSHP   = DERIVATIVES OF SHAPE FUNCTIONS W.R.T. XI, ETA, ZETA
C     JACOB  = JACOBIAN MATRIX INVERSE
C     DETJ   = DETERMINANT OF JACOBIAN MATRIX
C     XI, ETA, ZETA = ELEMENT COORDINATES AT WHICH THESE COMPUTATIONS
C                     TAKE PLACE
C     BXYZ   = BASIC SYSTEM COORDINATES FOR GRID POINTS
C
C     LOCAL VARIABLES
C     X,Y,Z  = CONSTANTS FOR EACH SHAPE FUNCTION
C     NGP    = NUMBER OF SHAPE FUNCTIONS, ALSO NUMBER OF GRID POINTS
C
      INTEGER         TYPE       ,EID       ,OP
      REAL            BXYZ(3,8)
      DOUBLE PRECISION            SHP(8)    ,DSHP(3,8)  ,JACOB(3,3) ,
     1                DETJ       ,XI        ,ETA        ,ZETA       ,
     2                X          ,Y         ,Z          ,QXI        ,
     3                QETA       ,QZETA     ,QXYZ       ,WORK(3,3)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ SYSBUF     ,OP
C
      NGP = 12*TYPE - 4
      Y   =-1.0
      Z   =-1.0
      GO TO (100,200,310), TYPE
C
C     LINEAR ELEMENT IHEX1
C
  100 DO 110 J = 1,2
      IF (J .EQ. 2) Z = 1.0
      X =-1.0
      Y =-1.0
      DO 110 I = 1,4
      IF (I .EQ. 3) Y = 1.0
      IF (I .EQ. 2) X = 1.0
      IF (I .EQ. 4) X =-1.0
      K   = I + (J-1)*4
      QXI = 1.0 + XI*X
      QETA  = 1.0 + ETA*Y
      QZETA = 1.0 + ZETA*Z
      SHP(K) = QXI*QETA*QZETA/8.0
      DSHP(1,K) = X*QETA*QZETA/8.0
      DSHP(2,K) = Y*QXI*QZETA/8.0
      DSHP(3,K) = Z*QXI*QETA/8.0
  110 CONTINUE
      GO TO 430
C
C     QUADRATIC ELEMENT IHEX2
C
  200 D = 1.0
      X = 0.0
      DO 300 I = 1,20
C            1   2   3   4   5   6   7   8   9   10
      GO TO (220,210,210,230,230,220,220,240,250,210,
     1       230,220,250,210,210,230,230,220,220,240), I
  210 X = X + D
      GO TO 260
  220 X = X - D
      GO TO 260
  230 Y = Y + D
      GO TO 260
  240 Y = Y - D
      GO TO 260
  250 Z = Z + 1.0
      Y =-1.0
      D = 3.0-D
  260 IF (X .EQ. 0.0) GO TO 270
      IF (Y .EQ. 0.0) GO TO 280
      IF (Z .EQ. 0.0) GO TO 290
C
C     CORNER POINT
C
      QXI   = 1.0 + X*XI
      QETA  = 1.0 + Y*ETA
      QZETA = 1.0 + Z*ZETA
      QXYZ  = X*XI + Y*ETA + Z*ZETA
      SHP(I)= QXI*QETA*QZETA*(QXYZ-2.0)/8.0
      DSHP(1,I) = X*QETA*QZETA*(X*XI+QXYZ-1.0)/8.0
      DSHP(2,I) = Y*QXI*QZETA*(Y*ETA+QXYZ-1.0)/8.0
      DSHP(3,I) = Z*QXI*QETA*(Z*ZETA+QXYZ-1.0)/8.0
      GO TO 300
C
C     MID-EDGE POINT, X=0.0
C
  270 QXI   = 1.0 - XI**2
      QETA  = 1.0 + Y*ETA
      QZETA = 1.0 + Z*ZETA
      SHP(I)= QXI*QETA*QZETA/4.0
      DSHP(1,I) =-XI*QETA*QZETA/2.0
      DSHP(2,I) = QXI*QZETA*Y/4.0
      DSHP(3,I) = QXI*QETA*Z/4.0
      GO TO 300
C
C     MID-EDGE POINT, Y=0.0
C
  280 QXI   = 1.0 + X*XI
      QETA  = 1.0 - ETA**2
      QZETA = 1.0 + Z*ZETA
      SHP(I)= QETA*QXI*QZETA/4.0
      DSHP(1,I) = QETA*QZETA*X/4.0
      DSHP(2,I) =-ETA*QZETA*QXI/2.0
      DSHP(3,I) = QETA*QXI*Z/4.0
      GO TO 300
C
C     MID-EDGE POINT, Z=0.0
C
  290 QXI   = 1.0 + X*XI
      QETA  = 1.0 + Y*ETA
      QZETA = 1.0 - ZETA**2
      SHP(I)= QZETA*QXI*QETA/4.0
      DSHP(1,I) = QZETA*QETA*X/4.0
      DSHP(2,I) = QZETA*QXI*Y/4.0
      DSHP(3,I) =-ZETA*QXI*QETA/2.0
  300 CONTINUE
      GO TO 430
C
C     CUBIC ELEMENT IHEX3
C
  310 D = 2.0/3.0
      X =-1.0/3.0
      DO 420 I = 1,32
C            1   2   3   4   5   6   7   8   9   10
      GO TO (320,330,330,330,340,340,340,320,320,320,
     2       350,350,360,330,340,320,360,330,340,320,
     3       360,330,330,330,340,340,340,320,320,320,
     4       350,350),I
  320 X = X - D
      GO TO 370
  330 X = X + D
      GO TO 370
  340 Y = Y + D
      GO TO 370
  350 Y = Y - D
      GO TO 370
  360 Y =-1.0
      Z = Z + 2.0/3.0
      IF (Z .GT. -1.0) D = 2.0
      IF (Z .GT.  0.4) D = 2.0/3.0
  370 IF (DABS(X) .LT. 0.4) GO TO 390
      IF (DABS(Y) .LT. 0.4) GO TO 400
      IF (DABS(Z) .LT. 0.4) GO TO 410
C
C     CORNER POINT
C
      QXI   = 1.0 + X*XI
      QETA  = 1.0 + Y*ETA
      QZETA = 1.0 + Z*ZETA
      QXYZ  = XI**2+ETA**2+ZETA**2 - 19.0/9.0
      SHP(I)= 9.0*QXI*QETA*QZETA*QXYZ/64.0
      DSHP(1,I) = 9.0*QETA*QZETA*(X*(2.0*XI**2+QXYZ)+2.0*XI)/64.0
      DSHP(2,I) = 9.0*QXI*QZETA*(Y*(2.0*ETA**2+QXYZ)+2.0*ETA)/64.0
      DSHP(3,I) = 9.0*QXI*QETA*(Z*(2.0*ZETA**2+QXYZ)+2.0*ZETA)/64.0
      GO TO 420
C
C     MID-EDGE POINT, X = + OR - 1/3
C
  390 QXI   = 9.0*(1.0-XI**2)*(1.0+9.0*X*XI)/64.0
      QETA  = 1.0 + Y*ETA
      QZETA = 1.0 + Z*ZETA
      QXYZ  = 9.0*(-2.0*XI+9.0*X-27.0*XI*X*XI)/64.0
      SHP(I)= QXI*QETA*QZETA
      DSHP(1,I) = QETA*QZETA*QXYZ
      DSHP(2,I) = QXI*QZETA*Y
      DSHP(3,I) = QXI*QETA*Z
      GO TO 420
C
C     MID-EDGE POINT Y = + OR - 1/3
C
  400 QXI   = 1.0 + X*XI
      QETA  = 9.0*(1.0-ETA**2)*(1.0+9.0*ETA*Y)/64.0
      QZETA = 1.0 + Z*ZETA
      QXYZ  = 9.0*(-2.0*ETA+9.0*Y-27.0*ETA*Y*ETA)/64.0
      SHP(I)= QETA*QXI*QZETA
      DSHP(1,I) = QETA*QZETA*X
      DSHP(2,I) = QXI*QZETA*QXYZ
      DSHP(3,I) = QETA*QXI*Z
      GO TO 420
C
C     MID-EDGE POINTS Z = + OR - 1/3
C
  410 QXI   = 1.0 + X*XI
      QETA  = 1.0 + Y*ETA
      QZETA = 9.0*(1.0-ZETA**2)*(1.0+9.0*Z*ZETA)/64.0
      QXYZ  = 9.0*(-2.0*ZETA+9.0*Z-27.0*Z*ZETA**2)/64.0
      SHP(I)= QZETA*QXI*QETA
      DSHP(1,I) = QZETA*QETA*X
      DSHP(2,I) = QZETA*QXI*Y
      DSHP(3,I) = QXI*QETA*QXYZ
  420 CONTINUE
C
C     COMPUTE JACOBIAN MATRIX
C
  430 DO 440 I = 1,3
      DO 440 J = 1,3
      JACOB(I,J) = 0.0
      DO 440 K = 1,NGP
      JACOB(I,J) = JACOB(I,J) + DSHP(I,K)*DBLE(BXYZ(J,K))
  440 CONTINUE
C
C     COMPUTE INVERSE AND DETERMINANT OF JACOBIAN MATRIX
C
      WORK(1,1) = JACOB(2,2)*JACOB(3,3) - JACOB(2,3)*JACOB(3,2)
      WORK(2,1) = JACOB(2,3)*JACOB(3,1) - JACOB(2,1)*JACOB(3,3)
      WORK(3,1) = JACOB(2,1)*JACOB(3,2) - JACOB(2,2)*JACOB(3,1)
      WORK(1,2) = JACOB(1,3)*JACOB(3,2) - JACOB(1,2)*JACOB(3,3)
      WORK(2,2) = JACOB(1,1)*JACOB(3,3) - JACOB(1,3)*JACOB(3,1)
      WORK(3,2) = JACOB(1,2)*JACOB(3,1) - JACOB(1,1)*JACOB(3,2)
      WORK(1,3) = JACOB(1,2)*JACOB(2,3) - JACOB(1,3)*JACOB(2,2)
      WORK(2,3) = JACOB(1,3)*JACOB(2,1) - JACOB(1,1)*JACOB(2,3)
      WORK(3,3) = JACOB(1,1)*JACOB(2,2) - JACOB(1,2)*JACOB(2,1)
      DETJ = 0.0
      DO 450 I = 1,3
      DETJ = DETJ + JACOB(I,2)*WORK(2,I)
  450 CONTINUE
      IF (DETJ .EQ. 0.0) GO TO 470
      DO 460 I = 1,3
      DO 460 J = 1,3
      JACOB(I,J) = WORK(I,J)/DETJ
  460 CONTINUE
      RETURN
C
C     JACOBIAN MATRIX WAS SINGULAR.
C
  470 WRITE  (OP,480) UFM,EID
  480 FORMAT (A23,' 3306, SINGULAR JACOBIAN MATRIX FOR ISOPARAMETRIC ',
     1       'ELEMENT NO.',I9)
      RETURN
      END
