      SUBROUTINE GAUSS (A,N,N2)
C
      COMPLEX A(20,1)
C
      DO 100 I=1,N
      K=I+1
      DO 10 J=K,N2
   10 A(I,J)=A(I,J)/A(I,I)
      DO 30 M=1,N
      IF(M.EQ.I) GO TO 30
      DO 20 L=K,N2
   20 A(M,L)=A(M,L)-A(M,I)*A(I,L)
   30 CONTINUE
  100 CONTINUE
      RETURN
      END
