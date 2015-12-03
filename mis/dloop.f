      SUBROUTINE DLOOP (X,Y,MPY,END)
C*******
C     DLOOP IMPROVES THE EFFICIENCY OF AN INNER DCOMP LOOP
C*******
      DOUBLE PRECISION   X(1)      ,Y(1)     ,MPY
C*******
C     DDLOOP IMPROVES THE EFFICIENCY OF THE ACTIVE ROW LOOP
C*******
      DOUBLE PRECISION   A         ,B(1)     ,C(1)
      DOUBLE PRECISION XX(1),YY(1)
      INTEGER            END
      DO 10 I = 1,END
   10 X(I) = X(I)+MPY*Y(I)
      RETURN
C*******************************
      ENTRY DDLOOP (A,B,C,ENDD)
      DO 20 I = 1,ENDD
   20 A = A-B(I)*C(I)
      RETURN
C************
C     ENTRY FOR ANOTHER LOOP
C***********
      ENTRY XLOOP(XX,YY,NN)
      DO 105 I = 1,NN
  105 XX(I) = YY(I)
      RETURN
      END
