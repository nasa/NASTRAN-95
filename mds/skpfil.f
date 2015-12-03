        SUBROUTINE SKPFIL ( FILE, N )                                           
      INCLUDE 'DSIOF.COM'                                                       
        INTEGER FILE                                                            
        IF ( N .EQ. 0 ) GO TO 7777                                              
        NAME = FILE                                                             
        CALL DSGEFL                                                             
        IRWORD = N                                                              
        IF ( N .GT. 0 ) GO TO 20                                                
        IF ( ( INDCLR-INDBAS ) .NE. 5 ) GO TO 10                                
        IF ( NBLOCK .EQ. 1 ) GO TO 7000                                         
10      CALL DSSKFB( N )                                                        
        GO TO 7000                                                              
20      IF ( IPRVOP .NE. 0 ) CALL DSMSG( 4 )                                    
        CALL DSSKFF( N )                                                        
7000    CALL DSSDCB                                                             
7777    RETURN                                                                  
        END                                                                     
