      DOUBLE PRECISION FUNCTION DK211(I,A,B,X)
      DOUBLE PRECISION F6211, A, B, X, XX, C1, C2, AAJ, C3
      DIMENSION  X(1)
      XX = X(I)
      IF ( (B * XX) ** 2 - A ** 2 ) 100,1,200
    1 CONTINUE
      IF (A .NE. B * XX) GO TO 50
      F6211 = 0.5D0 * (DLOG (DABS(2.0D0 * B * XX)) ) **2
      DK211 = F6211
      RETURN
   50 CONTINUE
      F6211 = 0.0D0
      DK211 = F6211
      RETURN
  100 CONTINUE
      F6211 = DLOG(DABS(A))* DLOG(DABS(XX))
      C1 =-B * XX / A
      C2 = 1.0D0
      J = 0
  110 J = J + 1
      AAJ = J
      C2 = C2 * C1
      C3 = C2 / (AAJ ** 2)
      F6211 = F6211 - C3
      IF(DABS(C3) .GT. 0.1D-5)   GO TO 110
      DK211 = F6211
      RETURN
  200 CONTINUE
      F6211 = (DLOG(DABS(B* XX)) ** 2) / 2.0D0
      C1 =-A / (B * XX)
      C2 = 1.0D0
      J = 0
  210 J = J + 1
      AAJ = J
      C2 = C2 * C1
      C3 = C2 / (AAJ ** 2)
      F6211 = F6211 + C3
      IF(DABS(C3) .GT. 0.1D-5)   GO TO 210
      DK211 = F6211
      RETURN
      END
