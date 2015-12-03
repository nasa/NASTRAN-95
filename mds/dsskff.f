        SUBROUTINE DSSKFF ( NN )                                                
      INCLUDE 'DSIOF.COM'                                                       
        N = NN                                                                  
10      IF ( N .EQ. 0 ) GO TO 7000                                              
20      CALL DSFWR1                                                             
        IF ( IRETRN .EQ. 0 ) GO TO 20                                           
        N = N - 1                                                               
        GO TO 10                                                                
7000    RETURN                                                                  
        END                                                                     
