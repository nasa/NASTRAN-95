      DOUBLE PRECISION FUNCTION DAPOLY(N,P)
C
C     CALCULATES AREA OF A POLYGON DESCRIBED BY N POINTS (P)
C        ( N .LE. 10 )
C
C        AREA= -1* LINE INTEGRAL OF Y*DX
C
C     AREA CONTRIBUTION FROM SIDE WHOSE ENDS ARE P(I), P(J):
C        A(I,J)= 0.5 * (Y(I)+Y(J)) * (X(I)-X(J))
C
      DOUBLE PRECISION P(2,1)
      INTEGER KEDGE(2,10), K(2,10)
C
      DATA KEDGE/ 1,2,  2,3,  3,4,  4,5,  5,6,  6,7,  7,8,  8,9,  9,10,
     1  10,1/
C
      DO 10 I=1,2
      DO 10 J=1,N
   10 K(I,J)= KEDGE(I,J)
      K(2,N)= 1
      DAPOLY= 0.0
C
      DO 20  NN= 1,N
      K1= K(1,NN)
      K2= K(2,NN)
   20 DAPOLY= DAPOLY +5.D-1 * (P(2,K1)+P(2,K2)) * (P(1,K1)-P(1,K2))
      RETURN
      END
