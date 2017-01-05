      FUNCTION CORWDS (I,J)        
C        
      INTEGER CORWDS        
C        
      CORWDS = IABS(LOCFX(I) - LOCFX(J)) + 1        
      RETURN        
      END        
