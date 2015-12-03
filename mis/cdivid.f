      SUBROUTINE CDIVID(A,B,D,NCOL)
      DOUBLE PRECISION A(1),B(1),D(2),DTEMP,DENM
C
C     THIS ROUTINE DIVIDES THE VECTOR A BY D AND STORE RESULT IN B
C
      DENM = D(1)**2 + D(2)**2
      DO 10 I = 1,NCOL,2
      DTEMP = (A(I)*D(1) +A(I+1)*D(2))/DENM
      B(I+1) = (A(I+1)*D(1) -A(I) * D(2))/DENM
      B(I) = DTEMP
   10 CONTINUE
      RETURN
      END
