        SUBROUTINE DSIPK1 ( BLOCK, ITYPOT )                                     
      INCLUDE 'DSIOF.COM'                                                       
        INTEGER BLOCK( 15 )                                                     
        IRETRN = 0                                                              
        BLOCK( 1 ) = NAME                                                       
        BLOCK( 8 ) = -1                                                         
        IF ( ITYPOT .GT. 0 ) GO TO 10                                           
        IFLAG = IABS( ITYPOT ) + 64                                             
        GO TO 20                                                                
10      IFLAG = ITYPOT                                                          
20      BLOCK( 13) = IFLAG                                                      
        CALL GETSTR( *777, BLOCK )                                              
        BLOCK( 7 ) = 0                                                          
        IF ( IFLAG .GE. 1 .AND. IFLAG .LE. 4 ) GO TO 30                         
        IF ( IFLAG .GE. 65 .AND. IFLAG .LE. 68 ) GO TO 30                       
        CALL DSMSG1( BLOCK )                                                    
        CALL DSMSG( 118 )                                                       
30      CONTINUE                                                                
        GO TO 700                                                               
777     IRETRN = 1                                                              
700     RETURN                                                                  
        END                                                                     
