      SUBROUTINE ZEROC(IZ,N)
C
C     SET AND ARRAY TO ZERO
C
      INTEGER IZ(N)
C
      DO 10 I=1,N
   10 IZ(I) = 0
      RETURN
      END
