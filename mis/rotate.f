      SUBROUTINE ROTATE (DA,ROW,ROW1,ROW2, O,SIN,COS)
C
C     THIS ROUTINE IS CALLED ONLY BY TRIDI SUBROUTINE, WHICH IS CALLED
C     ONLY BY VALVEC
C
      INTEGER          ROW,ROW1,ROW2
C    1,                CHECK
      DOUBLE PRECISION O(1),SIN(1),COS(1),SINE,COSINE,X,Y,Z,DA(1)
      COMMON /GIVN  /  TITLE(100),N
C
C     O     = 2ND ROW OF THE COMPLETE MATRIX.
C     SIN   = SINES.
C     COS   = COSINES.
C     DA = MATRIX PARTITION (TRIANGULAR) - DOUBLE PRECISION
C
      M    = 0
  200 DO 230 J = ROW1,ROW2
      SINE = SIN(J)
      COSINE = COS(J)
      M    = M + 1
      IF (SINE .EQ. 0.0D0) GO TO 210
      X    = O(ROW+1)*COSINE + O(J)*SINE
      Y    = DA(M)   *SINE   + O(J)*COSINE
      Z    = X       *COSINE + Y   *SINE
      O(J) = Y       *COSINE - X   *SINE
      DA(M)= O(ROW+1)+ DA(M) - Z
      O(ROW+1) = Z
  210 IF (J .EQ. N) GO TO 230
      JP1  = J + 1
      DO 220 I = JP1,N
      M    = M + 1
      X    = DA(M)*COSINE - O(I)*SINE
      O(I) = O(I)*COSINE  + DA(M)*SINE
      Y    = COS(I)*O(J)  + SIN(I)*X
      DA(M)= COS(I)*X     - SIN(I)*O(J)
      O(J) = Y
  220 CONTINUE
  230 CONTINUE
      RETURN
      END
