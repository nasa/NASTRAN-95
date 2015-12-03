      SUBROUTINE DVECTR (GPT,X,U,PEN)
C
      INTEGER GPT(1),PEN
      REAL    X(3,1),U(2,1)
      COMMON /BLANK/ NGP
C
      CALL LINE (0,0,0,0,0,-1)
C
C     DO NOT DRAW A VECTOR AT ANY GRID POINT WHOSE INDEX .LE. 0.
C
      DO 120 I = 1,NGP
      J  = GPT(I)
      IF (J .LE. 0) GO TO 120
      X1 = X(2,J)
      Y1 = X(3,J)
      X2 = U(1,J)
      Y2 = U(2,J)
      CALL LINE (X1,Y1,X2,Y2,PEN,0)
  120 CONTINUE
C
      CALL LINE (0,0,0,0,0,1)
      RETURN
      END
