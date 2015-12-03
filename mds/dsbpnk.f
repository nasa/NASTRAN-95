        SUBROUTINE DSBPNK ( BLOCK, MCB )                                        
      INCLUDE 'DSIOF.COM'                                                       
      INTEGER BLOCK( 15 ), MCB( 7 )                                             
      IF ( BLOCK( 1 ) .EQ. NAME ) GO TO 10                                      
      CALL DSMSG1( BLOCK )                                                      
      CALL DSMSG( 120 )                                                         
10    CONTINUE                                                                  
      IF ( MCB( 2 ) .EQ. 0 ) MCB( 7 ) = MCBMAS                                  
      MCB( 2 ) = MCB( 2 ) + 1                                                   
      NUM      = BLOCK( 10 )                                                    
      IF ( MCB( 6 ) .GT. NUM ) GO TO 20                                         
      MCB( 6 ) = NUM                                                            
20    MCB( 7 ) = MCB( 7 ) + NUM                                                 
      BLOCK( 8 ) = 1                                                            
      CALL ENDPUT ( BLOCK )                                                     
      RETURN                                                                    
      END                                                                       
