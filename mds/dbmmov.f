      SUBROUTINE DBMMOV ( INDEX1, INDEX2, NO )                                  
      INCLUDE 'ZZZZZZ.COM'
      DO 10 I = 1, NO                                                           
      MEM( INDEX2+I-1 ) = MEM( INDEX1+I-1 )                                     
10    CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
