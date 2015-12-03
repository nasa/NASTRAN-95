      SUBROUTINE DBMREL                                                         
C********************************************************************           
C  DBMREL  -   RELEASES IN-MEMORY BLOCKS THAT ARE CURRENTLY                     
C              ALLOCATED TO AN IN-MEMORY FILE                                   
C********************************************************************           
      INCLUDE   'DSIOF.COM'                                                     
      INCLUDE   'ZZZZZZ.COM'
      COMMON / SYSTEM / ISYSBF, IWR                                             
      IF ( FCB( 9, IFILEX ) .EQ. 0 .OR. FCB( 10, IFILEX ) .EQ. 0 )              
     &    GO TO 701                                                             
      IF ( IDBFRE .NE. 0 ) GO TO 10                                             
C FREE CHAIN IS EMPTY, THIS CHAIN BECOMES FREE CHAIN                            
      IDBFRE = FCB( 9, IFILEX )                                                 
      GO TO 777                                                                 
C SET FIRST OF BLOCKS TO BE FREED AT FIRST OF FREE CHAIN AND                    
C THEN CONNECT LAST OF BLOCKS TO BE FREED WITH FIRST OF EXISTING                
C FREE CHAIN                                                                    
10    CONTINUE                                                                  
      IF ( FCB( 9, IFILEX ) .EQ. FCB( 10, IFILEX ) ) GO TO 20                   
      ISAVE          = IDBFRE                                                   
      IDBFRE         = FCB(  9, IFILEX )                                        
      MEM( ISAVE )   = FCB( 10, IFILEX )                                        
      INDEX          = FCB( 10, IFILEX )                                        
      MEM( INDEX+1 ) = ISAVE                                                    
      GO TO 777                                                                 
C FILE HAD ONLY ONLY ONE BLOCK ALLOCATED TO IT                                  
20    CONTINUE                                                                  
      ISAVE          = IDBFRE                                                   
      IDBFRE         = FCB(  9, IFILEX )                                        
      MEM( ISAVE )   = IDBFRE                                                   
      MEM( IDBFRE+1) = ISAVE                                                    
      GO TO 777                                                                 
701   WRITE( IWR, 901 )                                                         
901   FORMAT(///,' ERROR IN ATTEMPT TO FREE BLOCKS TO FREE CHAIN',              
     &       /,' CONTENTS OF THE DIRECTORY ARE AS FOLLOWS')                     
      CALL DBMDMP                                                               
      CALL MESAGE ( -61, 0, 0 )                                                 
777   CONTINUE                                                                  
      FCB(  9, IFILEX ) = 0                                                     
      FCB( 10, IFILEX ) = 0                                                     
      FCB( 11, IFILEX ) = 0                                                     
      RETURN                                                                    
      END                                                                       
