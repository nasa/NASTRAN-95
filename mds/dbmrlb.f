      SUBROUTINE DBMRLB ( INDEX )                                               
C********************************************************************           
C  DBMRLB  -   RELEASES AN IN-MEMORY BLOCK THAT IS CURRENTLY                    
C              ALLOCATED AS THE LAST BLOCK OF AN IN-MEMORY FILE.                
C              THIS IS USED TO RELEASE THE NEXT ALLOCATED BLOCK FOR A           
C              FILE OPENED FOR WRITE BUT WAS NEVER USED BECAUSE THE            
C              FILE WAS CLOSED--I.E., THE LAST BLOCK ALLOCATED FOR A            
C              FILE OPENED FOR WRITE IS NEVER USED BUT IT MUST HAVE            
C              BEEN ALLOCATED JUST IN CASE THE FILE IS NOT TO BE CLOSED.        
C********************************************************************           
      INCLUDE   'DSIOF.COM'                                                     
      COMMON / ZZZZZZ / MEM(4)                                                  
      COMMON / SYSTEM / ISYSBF, IWR                                             
      INDEXL = INDEX
C CHECK IF OTHER BLOCKS ARE CHAINED TO THE END OF THIS BLOCK
      IF ( MEM( INDEX+1 ) .NE. 0 ) GO TO 100                                    
C SET "NEXT" OF PREVIOUS BLOCK TO ZERO, IF IT EXISTS                            
5     LINDEX = MEM( INDEX )                                                     
      IF ( LINDEX .EQ. 0 ) GO TO 10                                             
      MEM( LINDEX+1 ) = 0                                                       
10    IF ( IDBFRE .NE. 0 ) GO TO 20                                             
C FREE CHAIN IS EMPTY, THIS BLOCK BECOMES FREE CHAIN                            
      IDBFRE = INDEX                                                            
C SET "NEXT" AND "PREVIOUS" OF THIS CHAIN TO ZERO                               
      MEM( INDEX    ) = 0                                                        
      MEM( INDEXL+1 ) = 0                                                        
      GO TO 700                                                                 
C SET BLOCKS TO BE FREED AT FIRST OF FREE CHAIN AND                              
C THEN CONNECT FREE CHAIN TO THIS BLOCK                                         
20    ISAVE  = IDBFRE                                                           
      IDBFRE = INDEX                                                            
      MEM( ISAVE    ) = INDEXL                                                    
      MEM( INDEX    ) = 0                                                        
      MEM( INDEXL+1 ) = ISAVE                                                    
      GO TO 700                                                                 
C MORE THAN ONE BLOCK IN THIS CHAIN TO RELEASE BACK TO FREE CHAIN
100   CONTINUE
110   IF ( MEM( INDEXL+1 ) .EQ. 0 ) GO TO 5
CWKBR SPR94012 10/94      INDEXL = MEM( INDEX+1 )          
      INDEXL = MEM( INDEXL+1 )          
      GO TO 110
700   CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
