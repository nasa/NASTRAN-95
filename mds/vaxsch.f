      SUBROUTINE VAXSCH (NIN,NOUT)        
C        
C     TO SEARCH UNIT NIN FOR END OF BULK DATA DECK        
C        
      CHARACTER*8 E1,E2,E3,CHR        
      DATA        E1,E2,E3 / 'ENDDATA ', 'END DATA', 'ENDATA  ' /       
C        
C        
   60 READ (NIN,70,END=80) CHR        
   70 FORMAT (A8)        
      IF (CHR.EQ.E1 .OR. CHR.EQ.E2 .OR. CHR.EQ.E3) GO TO 100        
      GO TO 60        
C        
C     ENDDATA CARD NOT FOUND        
C        
   80 WRITE  (NOUT,90)        
   90 FORMAT ('0*** USER FATAL MESSAGE: "ENDDATA" CARD NOT FOUND BY ',  
     1        'INPUT MODULE')        
      CALL VAXEND        
C        
  100 RETURN        
      END        
