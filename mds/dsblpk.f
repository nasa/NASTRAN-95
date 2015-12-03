        SUBROUTINE DSBLPK ( BLOCK )                                             
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'PAKBLK.COM'                                                      
      INCLUDE 'XNSTRN.COM'                                                      
      INTEGER           BLOCK(15)                                               
      BLOCK( 1 ) = NAME                                                         
      BLOCK( 2 ) = ITYPO                                                        
      IF ( ITRAIL .EQ. -1 ) GO TO 10                                            
      BLOCK( 3 ) = 0                                                            
      GO TO 20                                                                  
10    BLOCK( 3 ) = 1                                                            
20    CONTINUE                                                                  
      BLOCK( 4 ) = 0                                                            
      BLOCK( 7 ) = 0                                                            
      BLOCK( 8 ) = -1                                                           
      BLOCK(10 ) = 0                                                            
      BLOCK(12 ) = BLOCK( 12 )  + 1                                             
      BLOCK(13 ) = ITYPI                                                        
      CALL PUTSTR( BLOCK )                                                      
      IFLAG =  FCB( 8, IFILEX )                                                 
      IF ( IFLAG .NE. 0 ) GO TO 700                                             
      BLOCK( 12 ) = 1                                                           
      FCB( 8, IFILEX ) = 1                                                      
      IBASE( INDCLR + 2 ) = 1                                                   
      GO TO 700                                                                 
700   RETURN                                                                    
      END                                                                       
