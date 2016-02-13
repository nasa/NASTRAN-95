      SUBROUTINE REWIND ( FILE )                                              
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
      INTEGER           FILE                                                  
      NAME   = FILE                                                           
      CALL DSGEFL                                                             
C CALL DBMMGR FOR REWIND SO TO SET BUFFER ADDRESS CORRECTLY                     
      CALL DBMMGR ( 3 )                                                       
      NBLOCK = FCB( 4, IFILEX )                                               
      IF ( IPRVOP .EQ. 0 ) GO TO 30                                           
C IF FILE OPEN FOR WRITE, THEN INITIAL BUFFER AND BLOCK NUMBER                  
      IBASE( INDBAS+3 ) = 1                                                   
      IBASE( INDBAS+4 ) = 6                                                   
CWKBNB NCL93007 11/94
C SET THE COUNTER FOR NUMBER OF STRINGS AND TERMS TO ZERO
      FCB( 16, IFILEX ) = 0
      FCB( 17, IFILEX ) = 0
CWKBNE NCL93007 11/94
30    INDCLR = INDBAS+5                                                       
      INDCBP = INDCLR                                                         
      CALL DSSDCB                                                             
      RETURN                                                                  
      END                                                                     
