      SUBROUTINE MPYL (A,B,NCOLA,NROWA,NCOLB,C)
C
C     SINCE CDC FORTRAN 5 IMPOSES NO LONGER EXACT NO. OF DUMMY ARGUMENT
C     LIST FOR SUBROUTINE AND ENTRY POINTS, THIS ROUTINE IS NOW MACHINE
C     INDEPENDENT.
C
      DIMENSION A(NCOLA,NROWA),B(NCOLB,NCOLA),C(NCOLB,NROWA)
      DIMENSION D(NROWA,NCOLA),X(3),Y(3),VECT(3)
C
C     SIMPLE MATRIX MULTIPLICATION
C
      DO 10 N= 1,NCOLB
      DO 10 L= 1,NROWA
      C(N,L) = 0.0
      DO 10 M= 1,NCOLA
   10 C(N,L) = C(N,L)+B(N,M)*A(M,L)
      RETURN
C
      ENTRY NORM (X,Y)
C     ================
C
C     NORMALIZE X VECTOR
C
      Y(1) = X(1)*X(1)+X(2)*X(2)+X(3)*X(3)
      IF (Y(1) .EQ. 0.0)  GO TO 15
      W    = 1./SQRT(Y(1))
      X(1) = X(1)*W
      X(2) = X(2)*W
      X(3) = X(3)*W
   15 RETURN
C
      ENTRY CROSS (X,Y,VECT)
C     ======================
C
C     CROSS PRODUCT
C
      VECT(1) = X(2)*Y(3)-X(3)*Y(2)
      VECT(2) = Y(1)*X(3)-X(1)*Y(3)
      VECT(3) = X(1)*Y(2)-Y(1)*X(2)
      RETURN
C
      ENTRY MPYLT (D,B,NCOLA,NROWA,NCOLB,C)
C     =====================================
C
C     TRANSPOSE MULTIPLY
C
      DO 20 N= 1,NCOLB
      DO 20 L= 1,NROWA
      C(N,L) = 0.0
      DO 20 M= 1,NCOLA
   20 C(N,L) = C(N,L)+D(L,M)*B(N,M)
      RETURN
      END
