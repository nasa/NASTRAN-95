      SUBROUTINE AXIS10 (X1,Y1,X2,Y2,PENDEN,OPT)
C
C     (X1,Y1) = STARTING POINT OF THE AXIS.
C     (X2,Y2) = TERMINAL POINT OF THE AXIS.
C     PENDEN  = PEN NUMBER OR LINE DENSITY.
C     OPT     = -1 TO INITIATE  THE AXIS MODE.
C     ...     = +1 TO TERMINATE THE AXIS MODE.
C     ...     =  0 TO DRAW AN AXIS.
C
      INTEGER PENDEN,OPT,OPTX,A(6),AXIS
      REAL    XY(2,2)
      COMMON /PLTDAT/ SKPPLT(2),XYMIN(2),XYMAX(2)
      DATA    OPTX  / -1 /
      DATA    AXIS  /  6 /
C
      IF (OPTX .GE. 0) OPTX = OPT
      IF (OPT) 200,100,150
  100 XY(1,1) = X1
      XY(2,1) = Y1
      XY(1,2) = X2
      XY(2,2) = Y2
      DO 101 J = 1,2
      DO 101 I = 1,2
      IF (XY(I,J) .LT. XYMIN(I)) XY(I,J) = XYMIN(I)
      IF (XY(I,J) .GT. XYMAX(I)) XY(I,J) = XYMAX(I)
  101 CONTINUE
C
C     DRAW THE AXIS.
C
      A(1) = AXIS
      A(2) = PENDEN
      DO 110 J = 1,2
      A(2*J+1) = XY(1,J) + .1
      A(2*J+2) = XY(2,J) + .1
  110 CONTINUE
      IF (OPTX .EQ. 0) GO TO 120
C
C     INITIATE THE AXIS MODE.
C
      A(1) = A(1) + 10
      OPTX = 0
C
C     DRAW THE LINE.
C
  120 CALL WPLT10 (A,0)
      GO TO 200
C
C
C     TERMINATE THE LINE MODE.
C
  150 CALL WPLT10 (A,1)
      OPTX = -1
C
  200 RETURN
      END
