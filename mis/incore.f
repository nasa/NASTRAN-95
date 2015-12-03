      SUBROUTINE INCORE(A,N,B,CX,IX)
C
C     IN-CORE DECOMPOSITION OF SQUARE, COMPLEX, NXN MATRIX,A.
C     AX = B.
C     CX = X
C     IX = NUMBER OF B VECTORS SPECIFIED.
C
      COMPLEX A(N,N), B(IX,N), CX(IX,N), CMAX, SCRCH
      COMPLEX T1,T2,T3
      COMPLEX CSUM
C
C
      IF(N.EQ.2) GO TO 500
      IF(N.EQ.1) GO TO 600
      NM1 = N-1
C
C     PIVOT MAYBE.
C
      DO 150 J=1,NM1
      CMAX = A(J,J)
      JP1 = J + 1
      JMAX = J
      DO 100 JJ= JP1,N
      IF(CABS(A(J,JJ)).LE.CABS(CMAX)) GO TO 100
      CMAX = A(J,JJ)
      IROW = JJ
      JMAX = JJ
  100 CONTINUE
C
C     IROW = ROW WITH LARGEST ELEMENT IN COLUMN J.
C     MOVE PIVOT ROW TO TOP OF ELIMINATION
C
      AMAX = CABS(CMAX)
      IF(AMAX.EQ.0.) GO TO 150
      IF(JMAX.EQ.J) GO TO 120
      DO 110 JJ= J,N
      SCRCH = A(JJ,J)
      A(JJ,J) = A(JJ,IROW)
      A(JJ,IROW) = SCRCH
  110 CONTINUE
C
C     INTERCHANGE B VECTOR
C
      DO 115 JJ = 1,IX
      SCRCH = B(JJ,J)
      B(JJ,J) = B(JJ,IROW)
      B(JJ,IROW) = SCRCH
  115 CONTINUE
C
C     ELIMINATE COLUMN
C
  120 CONTINUE
      A(J,J) = (1.0,0.0) / A(J,J)
      T1 = A(J,J)
      DO 140 I=JP1,N
      T2 = A(I,J)
      IF(CABS(T2).LT.(1.0E-19)) GO TO 140
      T2 = -T2*T1
      A(I,J) = T2
      DO 130 L = JP1,N
      T3 = A(J,L)
      IF(CABS(T3).LT.(1.0E-19)) GO TO 130
      A(I,L) = A(I,L) + T3*T2
  130 CONTINUE
  140 CONTINUE
C
C     HANDLE B ELIMINATION.
C
      DO 141 JJ = 1,IX
      B(JJ,J) = B(JJ,J) * T1
  141 CONTINUE
      DO 145 JJ = 1,IX
      DO 145 K=JP1,N
      B(JJ,K) = B(JJ,K) - B(JJ,J)*A(J,K)
  145 CONTINUE
  150 CONTINUE
C
C     BACKWARD PASS.
C
      DO 185 JJ = 1,IX
      CX(JJ,N) = B(JJ,N)/A(N,N)
  185 CONTINUE
      DO 210 JJ = 1,IX
      I = N
  190 CONTINUE
      CSUM = (0.,0.)
      K = I-1
      DO 200 J=I,N
      CSUM = CSUM + CX(JJ,J)*A(J,K)
  200 CONTINUE
      CX(JJ, K)    = B(JJ, K)    + CSUM
      IF(I.LE.2) GO TO 210
      I = I-1
      GO TO 190
  210 CONTINUE
      RETURN
  500 CONTINUE
      DO 510 I = 1,IX
      CX(I ,2) = (B(I ,2)-(B(I ,1)*A(1,2)/A(1,1)))/(A(2,2)-(A(2,1)
     +  *A(1,2)/A(1,1)))
      CX(I ,1) = B(I ,1)/A(1,1)-A(2,1)*CX(I ,2)/A(1,1)
  510 CONTINUE
      RETURN
  600 CONTINUE
      DO 610 I=1,IX
      CX(I ,1) = B(I ,1)/A(1,1)
  610 CONTINUE
      RETURN
      END
