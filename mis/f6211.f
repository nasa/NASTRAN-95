      FUNCTION F6211(I,A,B,X)
      DIMENSION  X(1)
      XX = X(I)
      IF ( (B * XX) ** 2 - A ** 2 ) 100,1,200
    1 CONTINUE
      IF (A .NE. B * XX) GO TO 50
      F6211=0.5* (ALOG(ABS(2.0 * B * XX))) ** 2
      RETURN
   50 CONTINUE
      F6211 = 0.0
      RETURN
  100 CONTINUE
      F6211 = ALOG(ABS(A)) * ALOG(ABS(XX))
      C1 =-B * XX / A
      C2 = 1.0
      J = 0
  110 J = J + 1
      AAJ = J
      C2 = C2 * C1
      C3 = C2 / (AAJ ** 2)
      F6211 = F6211 - C3
      IF (ABS(C3) .GT. 0.000001) GO TO 110
      RETURN
  200 CONTINUE
      F6211 = (ALOG(ABS(B * XX)) ** 2) / 2.0
      C1 =-A / (B * XX)
      C2 = 1.0
      J = 0
  210 J = J + 1
      AAJ = J
      C2 = C2 * C1
      C3 = C2 / (AAJ ** 2)
      F6211 = F6211 + C3
      IF (ABS(C3) .GT. 0.000001) GO TO 210
      RETURN
      END
