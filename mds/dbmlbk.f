      SUBROUTINE DBMLBK ( LASBLK )                                              
C                                                                               
C THIS SUBROUTINE WILL RETURN THE LAST BLOCK NUMBER ALLOCATED TO THE            
C UNIT "IFILEX"                                                                 
C                                                                               
      INCLUDE   'DSIOF.COM'                                                     
      INCLUDE   'ZZZZZZ.COM'
      LASBLK = FCB( 6, IFILEX )                                                 
      IF ( LASBLK .NE. 0 ) GO TO 7000                                           
      INDEX  = FCB( 10, IFILEX )                                                
      IF ( INDEX .EQ. 0 ) GO TO 200                                             
      LASBLK = MEM( INDEX+3 )                                                   
      GO TO 7000                                                                
200   LASBLK = 0                                                                
7000  CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
                                                                                
                                                                                
