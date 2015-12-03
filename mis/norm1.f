      SUBROUTINE NORM1 (X,DIV)
C*******
C     NORM WILL NORMALIZE X TO MAXIMUM ELEMENT EQUAL TO ONE AND STORE TH
C     DIVISOR IN MAX
C*******
      DOUBLE PRECISION   X(1)      ,MAX      ,DIV
      COMMON   /INVPWX/  FILEK(7)
      EQUIVALENCE        (NCOL,FILEK(2))
      DATA IND1 /1/
      MAX = 0.D0
      DO 10 I=1,NCOL
      DIV = DABS( X(I) )
      IF( DIV .LE. MAX ) GO TO 10
      MAX = DIV
      IND = I
   10 CONTINUE
      IF( X(IND) .LT. 0.D0 ) IND = -IND
      I = IABS(IND1)
      XX = X(I)
      DIV = SIGN(1.,XX)*FLOAT(ISIGN(1,IND1))*MAX
      XX = DIV
      IND1 = IND*IFIX(SIGN(1.,XX))
      MAX = 1.D0/DIV
      DO 20 I=1,NCOL
      XI = X(I)*MAX
      IF (ABS(XI) .LT. 1.E-36) XI=0.
   20 X(I)= XI
      RETURN
      END
