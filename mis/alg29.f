      SUBROUTINE ALG29 (Y,X,FXY,N)
C
      DIMENSION Y(3),X(3),FXY(3)
C
      X1=(X(3)+X(2))*(Y(3)-Y(2))/(X(3)-X(2))
      FXY(2)=X1/(X(3)-X(1))
      N2=N-2
      DO 100 J=3,N2
      X2=(X(J+1)+X(J))*(Y(J+1)-Y(J))/(X(J+1)-X(J))
      FXY(J)=(X2-X1)/(X(J+1)-X(J-1))
100   X1=X2
      FXY(N-1)=-X1/(X(N)-X(N-2))
      FXY(1)=FXY(2)-(FXY(3)-FXY(2))/(X(3)-X(2))*(X(2)-X(1))
      FXY(N)=FXY(N-1)+(FXY(N-1)-FXY(N-2))/(X(N-1)-X(N-2))*(X(N)-X(N-1))
      RETURN
      END
