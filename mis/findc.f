      INTEGER FUNCTION FINDC (B,BBAR,N,IX,JX)
      INTEGER B,BBAR
      DIMENSION IX(1),JX(1)
C*******
C     PICK OUT PAIRS OF NUMBERS FOR ACTIVE ROWS
C*******
      ICC = 0
      J = 1
      DO 10 I=1,N
      IF (I-IX(I) .LE. BBAR) GO TO 10
      JX(J) = I+B-1
      JX(J+1) = IX(I)
      J = J+2
   10 CONTINUE
      J = J-1
      IF(J .EQ. 0) GO TO 31
      DO 30 K = 1,J,2
      IF((J-K-1)/2 .LT. ICC) GO TO 31
      IC = 0
      DO 20 L=K,J,2
      IF(JX(K) .LT. JX(L+1)) GO TO 20
      IC = IC+1
   20 CONTINUE
      ICC = MAX0(ICC,IC)
   30 CONTINUE
   31 FINDC = ICC
      RETURN
      END
