      SUBROUTINE WALTIM (WALSEC)        
C        
C     THIS ROUTINE OBTAINS THE CURRENT WALL CLOCK TIME IN SECONDS,      
C     PASS MID-NIGHT        
C        
      INTEGER WALSEC, TIME(3)        
C        
      CALL ITIME ( TIME )
      WALSEC = TIME(1) * 3600 + TIME(2) * 60 + TIME(3)
      RETURN        
      END        
