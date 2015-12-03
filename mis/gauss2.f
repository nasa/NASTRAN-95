      SUBROUTINE GAUSS2 (A,N,N2)
C
      COMPLEX A(20,1)
      DOUBLE COMPLEX DA(20,30)
C
      DO 5 I = 1, N
      DO 5 J = 1, N2
      DA(I,J) = A(I,J)
5     CONTINUE
      DO 100 I=1,N
      K=I+1
      DO 10 J=K,N2
   10 DA(I,J)=DA(I,J)/DA(I,I)
      DO 30 M=1,N
      IF(M.EQ.I) GO TO 30
      DO 20 L=K,N2
   20 DA(M,L)=DA(M,L)-DA(M,I)*DA(I,L)
   30 CONTINUE
  100 CONTINUE
      DO 150 I = 1, N
      DO 150 J = 1, N2
      A(I,J) = DA(I,J)
  150 CONTINUE
      RETURN
      END
