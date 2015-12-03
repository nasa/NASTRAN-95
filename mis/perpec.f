      SUBROUTINE PERPEC (X,STEREO)
C
      INTEGER          FVP,PRJECT,GP,STEREO
      REAL             X(3,1),MIN,MAX
      DOUBLE PRECISION DIAM,R
      COMMON /BLANK /  SKP(5),NGPSET
      COMMON /XXPARM/  SKPPLT(6),PENPAP(30),SCALX1,OBJMOD,SCALX2(3),
     1                 VIEW(15),FVP,R0,S0L,S0R,T0,D0,D02,D03,PRJECT,S0S
      COMMON /RSTXXX/  CSTM(3,3),MIN(3),MAX(3),D(3),AVER(3)
      DATA    RDIST /  29. /
C
C                               I====================I
C                         T     I                    I
C                         1     I     PROJECTION     I
C                         1     I                    I
C                         1     I       PLANE        I
C                         1     I                    I
C                         1     I====================I
C                         1    /                    /
C                         1   /                    /
C                         1  /   * OBSERVER       /
C                         1 /    1               /D0
C                         1/     1              /
C                         +--------------------/-----S
C                        /       1  /         /
C                       /      T01 /R0
C                      /         1/         /
C                     /----------+- - - - -/
C                    /    S0
C                   R
C
      IF (PRJECT .EQ. 1) GO TO 140
      IF (FVP    .EQ. 0) GO TO 110
      IF (PRJECT .EQ. 3) GO TO 100
C
C     PERSPECTIVE PROJECTION...FIND VANTAGE POINT
C
      R    = D(1)**2 + D(2)**2 + D(3)**2
      DIAM = DSQRT(R)
      R0   = 2.*DIAM + AVER(1)
      S0L  = AVER(2)
      T0   = DIAM + AVER(3)
      D0   = 1.5*DIAM
      GO TO 110
C
C     STEREO PROJECTION...FIND VANTAGE POINT
C
  100 R0  = RDIST + AVER(1)*OBJMOD
      S0L = AVER(2)*OBJMOD - S0S/2.
      S0R = AVER(2)*OBJMOD + S0S/2.
      T0  = AVER(3)*OBJMOD
      D0  = D03
      GO TO 140
C
  110 SCAL = 1.
      IF (PRJECT .EQ. 3) SCAL = OBJMOD
      SLR = S0L
      IF (STEREO .NE. 0) SLR = S0R
      DO 120 GP = 1,NGPSET
      R = D0/(R0-SCAL*X(1,GP))
      S = SLR + R*(SCAL*X(2,GP)-SLR)
      T = T0  + R*(SCAL*X(3,GP)-T0 )
      X(2,GP) = S
      X(3,GP) = T
      IF (PRJECT .EQ. 3) GO TO 120
      MIN(2) = AMIN1(MIN(2),S)
      MIN(3) = AMIN1(MIN(3),T)
      MAX(2) = AMAX1(MAX(2),S)
      MAX(3) = AMAX1(MAX(3),T)
  120 CONTINUE
      IF (PRJECT .EQ. 3) GO TO 140
C
C     FIND MINIMA + MAXIMA DIFFERENCES + AVERAGES
C
      DO 130 I = 2,3
      D(I) = MAX(I) - MIN(I)
      AVER(I) = (MAX(I)+MIN(I))/2.
  130 CONTINUE
C
  140 RETURN
      END
