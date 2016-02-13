      SUBROUTINE DBMNAM ( IGNAME, NAME, IFILEX )                                
C********************************************************************           
C  DBMNAM RETURNS THE DMAP NAME AND TRAILER FOR A GIVEN GINO FILE               
C     ARGUMENTS                                                                 
C       IGNAME  (INPUT )   GINO FILE NAME (E.G., 101,201,301)                   
C       FIST    (INPUT )   COMMON BLOCK /XFIST/                                 
C       FIAT    (INPUT )   COMMON BLOCK /XFIAT/                                 
C       NAME    (OUTPUT)   (2A4) DMAP FILE NAME                                 
C********************************************************************           
      INTEGER FIST(100), FIAT(100), NAME(2)                          
      INTEGER BLANK, POOL
      COMMON / XFIST / FIST                                                     
      COMMON / XFIAT / FIAT                                                     
      DATA     POOL / 4HPOOL /, BLANK / 4H    /
      IF ( IGNAME .LE. 100 .OR. IGNAME .GE. 400 ) GO TO 100
      LIM  = FIST(2)*2 - 1                                                      
      DO 10 I = 1, LIM, 2                                                       
      IF ( IGNAME .NE. FIST(2+I) ) GO TO 10                                     
      INDEX     = FIST(3+I)                                                     
      NAME(1)   = FIAT( INDEX+2 )                                               
      NAME(2)   = FIAT( INDEX+3 )                                               
      GO TO 700                                                                 
10    CONTINUE                                                                  
      NAME(1)   = 0                                                             
      NAME(2)   = 0                                                             
      GO TO 700
100   NAME(1) = IGNAME
      NAME(2) = BLANK
      IF ( IFILEX .EQ. 22 ) NAME(1) = POOL
700   RETURN                                                                    
      END                                                                       
