        SUBROUTINE CLOSE ( FILE, IOP )                                          
C***************************************************************                
C                          NOTICE                                               
C                                                                               
C     THIS PROGRAM BELONGS TO RPK CORPORATION.  IT IS CONSIDERED                
C A TRADE SECRET AND IS NOT TO BE DIVULGED OR USED BY PARTIES                   
C WHO HAVE NOT RECEIVED WRITTEN AUTHORIZATION FROM RPK.                         
C***************************************************************                
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
      INTEGER*2         IUNIT                                                   
      COMMON / DSUNIT / IUNIT( 220 )                                            
      COMMON / SYSTEM / ISYSBF, DUM1( 77 ), IDIAG, DUM2( 21 )                   
      INTEGER           FILE                                                    
      NAME   = FILE                                                             
      IOCODE = IOP                                                              
      IRETRN = 77                                                               
      CALL DSGEFL                                                               
      IF ( IFILEX .EQ. 0 ) GO TO 50                                             
      IRETRN = 0                                                                
      IF ( IAND( IDIAG,2**14 ).NE. 0 ) CALL DSMSG( 2 )                          
      IF ( IOCODE .NE. 1 ) GO TO 20                                             
      IF ( IPRVOP .EQ. 0 ) GO TO 10                                             
      CALL DSEFWR                                                               
      IF ( ( INDCLR-INDBAS ) .EQ. 5 ) GO TO 5                                   
      IBASE( INDBAS+4 ) = INDCLR - INDBAS + 1                                   
      CALL DBMMGR( 4 )                                                          
5     CALL DSXFSZ                                                               
10    CONTINUE                                                                  
      CALL DBMMGR( 2 )                                                          
      NBLOCK  = 1                                                               
      INDCLR  = INDBAS + 5                                                      
      INDCBP  = INDCLR                                                          
      GO TO 40                                                                  
20    IF ( IPRVOP .EQ. 0 ) GO TO 30                                             
      CALL DSEFWR                                                               
      IBASE( INDBAS+4 ) = INDCLR - INDBAS + 1                                   
C SAVE INDBAS TO ALLOW DSBRC1 TO CORRECTLY BACKSPACE FILE OPENNED FOR WRITE
      ISAVE = INDBAS
      CALL DBMMGR( 4 )                                                          
      CALL DSXFSZ                                                               
      INDBAS = ISAVE
      IF ( IOCODE .NE. -2 ) CALL DSBRC1                                         
C      CALL DSGNCL                                                              
      CALL DBMMGR( 2 ) 
      GO TO 40                                                                  
30    IF ( INDCBP .EQ. INDCLR ) GO TO 35                                        
      CALL DSSKRC                                                               
35    CONTINUE                                                                  
      CALL DBMMGR( 2 )                                                          
40    CALL DSSDCB                                                               
      FCB( 2,IFILEX ) = 0                                                       
      FCB(12,IFILEX ) = 0
      IF ( NAME .LT. 101 .OR. NAME .GT. 320 ) GO TO 50                          
      IUNIT( NAME-100 ) = 0                                                     
50    RETURN                                                                    
C***************************************************************                
C                          NOTICE                                               
C                                                                               
C     THIS PROGRAM BELONGS TO RPK CORPORATION.  IT IS CONSIDERED                
C A TRADE SECRET AND IS NOT TO BE DIVULGED OR USED BY PARTIES                   
C WHO HAVE NOT RECEIVED WRITTEN AUTHORIZATION FROM RPK.                         
C***************************************************************                
      END                                                                       
