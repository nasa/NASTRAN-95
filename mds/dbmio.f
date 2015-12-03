      SUBROUTINE DBMIO ( OPCODE )                                               
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'GINOX.COM'                                                       
      INCLUDE 'XNSTRN.COM'                                                      
C                                                                               
C  OPCODE                                                                       
C         = 1   OPEN, IOCODE = 0 OPEN FOR READ WITH REWIND                      
C                            = 1 OPEN FOR WRITE WITH REWIND                     
C                            = 2 OPEN FOR READ WITHOUT REWIND                   
C                            = 3 OPEN FOR WRITE WITHOUT REWIND                  
C         = 2   CLOSE, IOCODE = 1 CLOSE WITH REWIND                             
C                                OTHERWISE, NO REWIND                           
C         = 3   REWIND                                                          
C         = 4   WRITE ONE BLOCK                                                 
C         = 5   READ ONE BLOCK                                                  
C         = 6   POSITION                                                        
C         = 7   DELETE FILE                                                     
C         = 8   WRTBLK CODE                                                     
C         = 9   RDBLK CODE                                                      
C-----------------------------------------------------------------------        
      INTEGER   OPCODE                                                          
C      PRINT *,' DBMIO CALLED WITH OPCODE,IFILEX=',OPCODE,IFILEX                
C      PRINT *,' DBMIO,NBLOCK,IOCODE=',NBLOCK,IOCODE                            
C      WRITE(6,40646)(FCB(K,IFILEX),K=1,15)                                     
40646 FORMAT(' DBMIO-ENTRY,FCB=',/                                              
     & I3,I7,4I5,I7,I2,4I7,1X,2A4,I4)                                           
      GO TO (100,200,300,400,500,600,700,800,900),OPCODE                        
C-OPEN ------------------------------                                           
C     OPEN FILE ACCORDING TO IOCODE                                             
C       =0, OPEN AND READ FIRST BLOCK                                           
C       =1, OPEN AND RETURN ( OPEN FOR WRITE )                                  
C       =2, OPEN AND READ THE CURRENT BLOCK WHEN FILE WAS CLOSED                
C       =3, OPEN AND READ THE CURRENT BLOCK WHEN FILE WAS CLOSED                
100   CONTINUE                                                                  
      CALL DSGNOP                                                               
      FCB( 15, IFILEX ) = 700+IOCODE                                            
      FCB(  1, IFILEX ) = IOCODE                                                
      IF ( IOCODE .EQ. 0 ) GO TO 110                                            
      IF ( IOCODE .EQ. 1 ) GO TO 111                                            
      IF ( IOCODE .EQ. 2 ) GO TO 112                                            
      IF ( IOCODE .EQ. 3 ) GO TO 113                                            
110   CONTINUE                                                                  
      FCB( 4, IFILEX ) = NBLOCK                                                 
      GO TO 600                                                                 
111   CONTINUE                                                                  
      FCB( 4, IFILEX ) = NBLOCK                                                 
      FCB( 5, IFILEX ) = NBLOCK                                                 
      FCB( 6, IFILEX ) = NBLOCK                                                 
      GO TO 7000                                                                
112   CONTINUE                                                                  
113   CONTINUE                                                                  
      NBLOCK = FCB( 4, IFILEX )                                                 
      IF ( FCB( 5, IFILEX ) .NE. 0 ) GO TO 600                                  
      NBLOCK = 1                                                                
      GO TO 111                                                                 
C-CLOSE -----------------------------                                           
200   CONTINUE                                                                  
      CALL DSGNCL                                                               
      IF ( IOCODE .EQ. 0 ) FCB( 4, IFILEX ) = 1                                 
      IF ( FCB( 15, IFILEX ) .NE. 701 .AND. FCB( 15, IFILEX ) .NE. 703 )        
     &     GO TO 210                                                            
      FCB(  4, IFILEX ) = FCB(  4, IFILEX ) - 1                                 
      FCB(  6, IFILEX ) = FCB(  6, IFILEX ) - 1                                 
210   CONTINUE                                                                  
      FCB( 15, IFILEX ) = 0                                                     
      GO TO 7000                                                                
C-REWIND ----------------------------                                           
300   CONTINUE                                                                  
      FCB(  4, IFILEX ) = 1                                                     
      NBLOCK = 1                                                                
      IF ( FCB( 15, IFILEX ) .EQ. 701 .OR.                                      
     &     FCB( 15, IFILEX ) .EQ. 703 ) GO TO 7000                              
      GO TO 600                                                                 
C-WRITE -----------------------------                                           
400   CONTINUE                                                                  
      CALL DSGNWR                                                               
      FCB( 4, IFILEX ) = FCB( 4, IFILEX ) + 1                                   
      IF ( FCB( 4, IFILEX ) .GT. FCB( 6, IFILEX ) )                             
     &     FCB( 6, IFILEX ) = FCB( 4, IFILEX )                                  
      GO TO 7000                                                                
C-READ                                                                          
500   CONTINUE                                                                  
      FCB( 4, IFILEX ) = FCB( 4, IFILEX ) + 1                                   
      NBLOCK = FCB( 4, IFILEX )                                                 
      CALL DSGNRD                                                               
      GO TO 7000                                                                
C-POSITION AND READ BLOCK "NBLOCK"                                              
600   CONTINUE                                                                  
      CALL DSGNRD                                                               
      GO TO 7000                                                                
C-DELETE FILE                                                                   
700   CONTINUE                                                                  
      OPEN  (IFILEX, FILE=MDSNAM(IFILEX), STATUS='UNKNOWN')                     
      CLOSE (IFILEX, STATUS='DELETE')                                           
      FCB( 5, IFILEX ) = 0                                                      
      FCB( 6, IFILEX ) = 0                                                      
      GO TO 7000                                                                
C-SPECIAL RDBLK CALL                                                            
900   CONTINUE                                                                  
      PRINT *,' ERROR, DBMIO CALLED FOR RDBLK CALL'                             
      STOP                                                                      
C-SPECIAL WRTBLK CALL                                                           
800   CONTINUE                                                                  
      PRINT *,' ERROR, DBMIO CALLED FOR WRTBLK CALL'                            
      STOP                                                                      
7000  CONTINUE                                                                  
C      WRITE(6,40647)(FCB(K,IFILEX),K=1,15)                                     
40647 FORMAT(' DBMIO-EXIT,FCB=',/                                               
     & I3,I7,4I5,I7,I2,4I7,1X,2A4,I4)                                           
      RETURN                                                                    
      END                                                                       
