      SUBROUTINE EGNVCT (C1,C2,EIGEN,C3,N1,N2,N)
C
C     SUBROUTINE TO OBTAIN EIGENVECTOR FROM REAL NON-SYMMETRIC
C     MATRICES FOR WHICH THE EIGENVALUE IS KNOWN.  THE METHOD
C     USED IS THE DIRECT METHOD OUTLINED IN ERR-FW-   BY DR.
C     A. M. CUNNINGHAM.
C
      INTEGER N1(N),N2(N)
      COMPLEX C1(N,N),C2(N),C3(N),EIGEN,D1,D2,D3,D4,D5,D6,D8
C
      II3 = N
      II2 = N - 1
      X1  = 0.0
      DO 20 J = 1,N
      N1(J) = J
      N2(J) = J
      C1(J,J) = C1(J,J) - EIGEN
      DO 10 I = 1,N
      X2 = CABS(C1(I,J))
      IF (X1-X2) 5,10,10
    5 X1 = X2
      I1 = I
      J1 = J
   10 CONTINUE
   20 CONTINUE
      DO 150 K6 = 2,N
      IF (CABS(C1(I1,J1))) 50,30,50
   30 K5 = K6 - 1
C
C     SINGULAR MATRIX RETURN ZERO
C
      DO 36 I = 1,N
   36 C3(I) = 0.0
      GO TO 250
C
   50 D1 = (1.0,0.0)/C1(I1,J1)
      D2 = C1(I1,II3)
      D3 = C1(II3,J1)
      D4 = C1(II3,II3)
      DO 60 I = 1,II2
      C3(I    ) = C1(I,J1)
      C1(I,J1 ) = C1(I,II3)
      C1(I,II3) =-C3(I)*D1
      D5 = -C1(I1,I)*D1
      C1(I1 ,I) = C1(II3,I)
      C1(II3,I) = D5
   60 CONTINUE
      C3(I1) = D3
      C1(I1  ,J1) = D4
      C1(II3 ,J1) =-D2*D1
      C1(I1 ,II3) =-D3*D1
      C1(II3,II3) = D1
      IF (II3 .EQ. N) GO TO 80
      II4 = II3 + 1
      DO 70 I = II4,N
      D6 = C1(I1,I)
      C1(I1 ,I) = C1(II3,I)
      C1(II3,I) = D6
      C3(I    ) = C1(I,J1)
      C1(I,J1 ) = C1(I,II3)
   70 C1(I,II3) = C3(I)
   80 I = N1(J1)
      N1(J1 ) = N1(II3)
      N1(II3) = I
      I = N2(I1)
      N2(I1 ) = N2(II3)
      N2(II3) = I
      X1 = 0.0
      DO 140 J = 1,II2
      D8 = C1(II3,J)
      DO 130 I = 1,II2
      C1(I,J) = C1(I,J) + C3(I)*D8
      X2 = CABS(C1(I,J))
      IF(X1-X2) 120,130,130
  120 X1 = X2
      I1 = I
      J1 = J
  130 CONTINUE
  140 CONTINUE
      II3 = II3 - 1
      II2 = II2 - 1
  150 CONTINUE
C
      C3(2) = C1(2,1)
      C3(1) = (1.0,0.0)
      DO 180 J = 3,N
      C3(J) = (0.0,0.0)
      J1 = J - 1
      DO 170 I = 1,J1
      C3(J) = C3(J) + C3(I)*C1(J,I)
  170 CONTINUE
  180 CONTINUE
      IF (CABS(C1(1,1)) .LT. 1.0E-20) GO TO 202
      DO 201 K6 = 1,2
C
      DO 184 J = 1,N
      I1 = N2(J)
      DO 182 I = 1,N
      IF (I1 .EQ. N1(I)) GO TO 184
  182 CONTINUE
  184 C2(J) = C3(I)
C
      DO 190 J = 2,N
      I1 = N - J + 1
      J1 = I1 + 1
      DO 185 I = 1,I1
      C2(I) = C2(I) + C1(I,J1)*C2(J1)
  185 CONTINUE
  190 CONTINUE
      D1 = C1(1,1)/C2(1)
      C3(1) = (1.0,0.0)
      DO 200 J = 2,N
      I1 = J - 1
      C3(J) = C2(J)*C1(J,J)*D1
      DO 195 I = 1,I1
      C3(J) = C3(J) + C1(J,I)*C3(I)
  195 CONTINUE
  200 CONTINUE
  201 CONTINUE
C
C     C3(I) NOW CONTAINS THE EIGENVECTOR WHICH MUST BE RE-ARRANGED
C     ACCORDING TO THE ORDER DICTATED BY N1(I) BACK TO THE ORIGINAL
C     ORDER.
C
  202 DO 230 I = 1,N
      I1 = N1(I)
      N1(I) = I
  205 IF (I1-I) 210,230,210
  210 D1 = C3(I1)
      C3(I1) = C3(I)
      C3(I ) = D1
      K = N1(I1)
      N1(I1) = I1
      I1 = K
      GO TO 205
  230 CONTINUE
      N1(1) = 2
C
  250 RETURN
      END
