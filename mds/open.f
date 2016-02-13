      SUBROUTINE OPEN(*,NAMFIL,BUFF,OP)                                         
C******                                                                         
C                                                                               
C OPEN IS AN INTERMEDIARY TO ENTRY POINT QOPEN IN SUBROUTINE GINO.              
C THE MAIN TASK OF OPEN IS TO INSURE THAT DATA BLOCKS WHICH WERE                
C WRITTEN AND CLOSED OFF THE LOAD POINT HAVE AN END-OF-FILE BEFORE              
C THEY ARE READ.                                                                
C                                                                               
C******                                                                         
      INTEGER BUFF(1), OP, XOP, XNAME                                           
      COMMON /SYSTEM/ ISYSTM(157)                                               
      INCLUDE 'DSIOF.COM'                                                       
C                                                                               
C                                                                               
C TEST FOR CONDITION IN WHICH END-OF-FILE IS TO BE WRITTEN                      
C                                                                               
      DATA INIT / 0 /                                                           
      IF ( INIT .NE. 0 ) GO TO 5                                                
      CALL DSIODD                                                               
      INIT = 1                                                                  
   5  CONTINUE                                                                  
      XNAME = NAMFIL                                                            
      IFILEX = 0                                                                
      CALL GETURN( XNAME )                                                      
      IF(IFILEX.EQ.0)RETURN 1                                                   
   10 IF( OP.EQ.1 .OR. OP.EQ.3 ) GO TO 80                                       
      IF( NBLOCK+NLR .GT. 7 ) GO TO 12                                          
   11 IF( OP .EQ. -2 ) RETURN                                                   
      GO TO 80                                                                  
   12 IF( IPRVOP .EQ. 0 ) GO TO 11                                              
C                                                                              
C DATA BLOCK WAS PREVIOUSLY OPENED TO WRITE AND IS NOW OFF LOAD POINT.          
C WRITE AN END-OF-FILE. IF SPECIAL CALL, RETURN                                 
C                                                                               
      CALL QOPEN(*88,NAMFIL,BUFF,3)                                             
      CALL EOF( NAMFIL )                                                        
      XOP = 2                                                                   
      IF( OP .EQ. -2 ) XOP = 1                                                  
      CALL CLOSE( NAMFIL, XOP )                                                 
      IF( OP .EQ. -2 ) RETURN                                                   
C                                                                               
C NOW OPEN ACCORDING TO OP. IF NECESSARY, POSITION PRIOR TO EOF                 
C                                                                               
      LASNAM = 0                                                                
      CALL GETURN( NAMFIL )                                                     
      CALL QOPEN(*88,NAMFIL,BUFF,OP)                                            
      IF( OP .EQ. 2 ) CALL BCKREC( NAMFIL )                                     
      RETURN                                                                    
C                                                                               
C NORMAL OPEN CALL                                                              
C                                                                               
   80 CALL QOPEN(*88,NAMFIL,BUFF,OP)                                            
CWKBNB NCL93007 11/94
C SET THE COUNT FOR THE TOTAL NUMBER OF STRINGS AND TERMS 
C TO ZERO IF FILE IS BEING OPENED FOR WRITE
      IF ( OP .NE. 1 ) GO TO 70
      FCB( 16, IFILEX ) = 0
      FCB( 17, IFILEX ) = 0
   70 CONTINUE
CWKBNE NCL93007 11/94
      RETURN                                                                    
   88 RETURN 1                                                                  
      END                                                                       
