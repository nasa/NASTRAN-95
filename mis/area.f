      FUNCTION AREA (G,I,J,K)
C
C     THIS ROUTINE IS CALLED BY SFAREA WHICH IS CALLED BY EMGFIN TO
C     COMPUTE THE SURFACE AREAS OF THE SOLID ELEMENTS
C
      DIMENSION  G(1)
      AREA = 0.5*SQRT(
     1 ((G(J+2)-G(I+2))*(G(K+3)-G(I+3))-(G(J+3)-G(I+3))*(G(K+2)-G(I+2)))
     2 **2
     3+((G(J+3)-G(I+3))*(G(K+1)-G(I+1))-(G(J+1)-G(I+1))*(G(K+3)-G(I+3)))
     4 **2
     5+((G(J+1)-G(I+1))*(G(K+2)-G(I+2))-(G(J+2)-G(I+2))*(G(K+1)-G(I+1)))
     6 **2)
      RETURN
      END
