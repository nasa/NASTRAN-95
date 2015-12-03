      SUBROUTINE ROTATE1 (A,ROW,ROW1,ROW2, O,SIN,COS)
C
C     ROTATION OF A MATRIX PARTITION.
C     THIS ROUTINE IS CALLED ONLY BY TRIDI SUBROUTINE, WHICH IS CALLED
C     ONLY BY VALVEC
C
      INTEGER          ROW,ROW1,ROW2
      REAL             A(1)
      REAL             O(1),SIN(1),COS(1),SINE,COSINE,X,Y,Z
      COMMON /GIVN  /  TITLE(100),N
C
C     O     = 2ND ROW OF THE COMPLETE MATRIX.
C     SIN   = SINES.
C     COS   = COSINES.
C     A  = MATRIX PARTITION (TRIANGULAR) - SINGLE PRECISION
C
      M    = 0
      DO 105 J = ROW1,ROW2
      SINE = SIN(J)
      COSINE = COS(J)
      M    = M + 1
      IF (SINE .EQ. 0.) GO TO 101
      X    = O(ROW+1)*COSINE + O(J)*SINE
      Y    = A(M)    * SINE  + O(J)*COSINE
      Z    = X       *COSINE + Y   *SINE
      O(J) = Y       *COSINE - X   *SINE
      A(M) = O(ROW+1) + A(M) - Z
      O(ROW+1) = Z
  101 IF (J .EQ. N) GO TO 105
      JP1  = J + 1
      DO 103  I = JP1,N
      M    = M + 1
      X    = A(M)*COSINE - O(I)*SINE
      O(I) = O(I)*COSINE + A(M)*SINE
      Y    = COS(I)*O(J) + SIN(I)*X
      A(M) = COS(I)*X    - SIN(I)*O(J)
      O(J) = Y
  103 CONTINUE
  105 CONTINUE
      RETURN
      END
