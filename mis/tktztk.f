      SUBROUTINE TKTZTK(TK,Z,NZ,L,M,N)
C
C     THIS ROUTINE  PERFORMS A COORDINATE TRANSFORMATION ON THE
C     SYMMETRIC HALF OF A 3 BY 3 MATRIX
C
      DOUBLE PRECISION TK(3,3),Z(1)
      TK(1,1)=Z(NZ  )*Z(L  )+Z(NZ+3)*Z(L+1)+Z(NZ+6)*Z(L+2)
      TK(2,1)=Z(NZ+1)*Z(L  )+Z(NZ+4)*Z(L+1)+Z(NZ+7)*Z(L+2)
      TK(3,1)=Z(NZ+2)*Z(L  )+Z(NZ+5)*Z(L+1)+Z(NZ+8)*Z(L+2)
      TK(1,2)=Z(NZ  )*Z(L+1)+Z(NZ+3)*Z(M  )+Z(NZ+6)*Z(M+1)
      TK(2,2)=Z(NZ+1)*Z(L+1)+Z(NZ+4)*Z(M  )+Z(NZ+7)*Z(M+1)
      TK(3,2)=Z(NZ+2)*Z(L+1)+Z(NZ+5)*Z(M  )+Z(NZ+8)*Z(M+1)
      TK(1,3)=Z(NZ  )*Z(L+2)+Z(NZ+3)*Z(M+1)+Z(NZ+6)*Z(N  )
      TK(2,3)=Z(NZ+1)*Z(L+2)+Z(NZ+4)*Z(M+1)+Z(NZ+7)*Z(N  )
      TK(3,3)=Z(NZ+2)*Z(L+2)+Z(NZ+5)*Z(M+1)+Z(NZ+8)*Z(N  )
      Z(L  )=Z(NZ  )*TK(1,1)+Z(NZ+3)*TK(1,2)+Z(NZ+6)*TK(1,3)
      Z(L+1)=Z(NZ  )*TK(2,1)+Z(NZ+3)*TK(2,2)+Z(NZ+6)*TK(2,3)
      Z(L+2)=Z(NZ  )*TK(3,1)+Z(NZ+3)*TK(3,2)+Z(NZ+6)*TK(3,3)
      Z(M  )=Z(NZ+1)*TK(2,1)+Z(NZ+4)*TK(2,2)+Z(NZ+7)*TK(2,3)
      Z(M+1)=Z(NZ+1)*TK(3,1)+Z(NZ+4)*TK(3,2)+Z(NZ+7)*TK(3,3)
      Z(N  )=Z(NZ+2)*TK(3,1)+Z(NZ+5)*TK(3,2)+Z(NZ+8)*TK(3,3)
      RETURN
      END
