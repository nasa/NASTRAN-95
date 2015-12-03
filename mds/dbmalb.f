      SUBROUTINE DBMALB ( LENREQ, INDEX )                                       
C********************************************************************           
C     DBMALB - ALLOCATES A MEMORY BLOCK OF LENGTH "LENREQ"                      
C              FROM THE FREE CHAIN AND RETURNS THE                              
C              POINTER IN MEMORY FOR THE BLOCK IN "INDEX" (RELATIVE TO.         
C              /DBM/.                                                           
C                                                                               
C     EACH FREE BLOCK IN MEMORY HAS THE FOLLOWING FORMAT:                       
C         WORD 1  POINTER TO PREVIOUS FREE BLOCK (=0, IF FIRST)                 
C         WORD 2  POINTER TO NEXT FREE BLOCK (=0, IF END OF CHAIN)              
C         WORD 3  NUMBER OF WORDS AVAILABLE IN THIS FREE BLOCK                  
C                                                                               
C     NOTE:  IDBFRE POINTS TO THE FIRST FREE BLOCK OF THE CHAIN                 
C********************************************************************           
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'ZZZZZZ.COM'
      NEXT  = IDBFRE                                                            
      IF ( IDBFRE .EQ. 0 ) GO TO 701                                            
C  OBTAIN THE LENGTH OF THE FREE BLOCK                                          
10    LENAVL = MEM( NEXT + 2 )                                                  
      IF ( LENAVL .GE. LENREQ ) GO TO 100                                       
C  MEMORY NOT AVAILABLE IN THIS BLOCK, CHECK FOR OTHER BLOCKS                   
      NEXT  = MEM( NEXT + 1 )                                                   
C  IF NO MORE FREE BLOCKS, RETURN WITH INDEX SET TO -1                          
      IF ( NEXT .EQ. 0 ) GO TO 701                                              
      GO TO 10                                                                  
C  RETURN POINTER FOR THIS BLOCK                                                
100   INDEX = NEXT                                                              
      IF ( LENREQ .NE. LENAVL ) GO TO 200                                       
C  COME HERE WHEN REQUESTED BLOCK SAME SIZE AS FREE BLOCK                       
      NEXT  = MEM( NEXT+1 )                                                     
      IPREV = MEM(INDEX   )                                                     
      IF ( IPREV .EQ. 0 ) GO TO 110                                             
      IF ( NEXT  .EQ. 0 ) GO TO 120                                             
C  CONNECT THE PREVIOUS FREE BLOCK WITH THE NEXT FREE BLOCK                     
      MEM( IPREV+1 ) = NEXT                                                     
      MEM( NEXT    ) = IPREV                                                    
      GO TO 700                                                                 
110   IF ( NEXT .EQ. 0 ) GO TO 130                                              
C  NO PREVIOUS BLOCK, SET IDBFRE TO POINT TO NEW FIRST FREE BLOCK               
      IDBFRE = NEXT                                                             
      MEM( NEXT ) = 0                                                           
      GO TO 700                                                                 
C  PREVIOUS BLOCK EXITS BUT BLOCK ALLOCATED WAS LAST IN CHAIN                   
120   MEM( IPREV+1 ) = 0                                                        
      GO TO 700                                                                 
C  NO MORE FREE BLOCKS EXIST, SET IDBFRE TO ZERO                                
130   IDBFRE = 0                                                                
      GO TO 700                                                                 
C  COME HERE WHEN FREE BLOCK HAS MORE SPACE THEN REQUESTED                      
200   NEWIND = INDEX + LENREQ + 4                                               
      IPREV  = MEM( INDEX  )                                                    
      NEXT   = MEM( INDEX+1)                                                    
C  CHECK TO DETERMINE IF ANY SPACE REMAINS                                      
      IF ( ( LENAVL-LENREQ-4 ) .LE. 0 ) GO TO 240                               
C  RECOMPUTE FREE SPACE AND SET UP CHAIN WORDS                                  
      MEM( NEWIND+2 ) = LENAVL - LENREQ - 4                                     
      IF ( IPREV .EQ. 0 ) GO TO 210                                             
      IF ( NEXT  .EQ. 0 ) GO TO 220                                             
C  CONNECT TO PREVIOUS AND NEXT FREE BLOCK                                      
      MEM( NEWIND  ) = IPREV                                                    
      MEM( NEWIND+1) = NEXT                                                     
      MEM( IPREV+1 ) = NEWIND                                                   
      MEM( NEXT    ) = NEWIND                                                   
      GO TO 700                                                                 
210   IF ( NEXT .EQ. 0 ) GO TO 230                                              
C  NO PREVIOUS BLOCK, NEWLY CREATED BLOCK BECOMES THE FIRST FREE BLOCK          
      IDBFRE         = NEWIND                                                   
      MEM( NEWIND  ) = 0                                                        
      MEM( NEWIND+1) = NEXT                                                     
      MEM( NEXT    ) = NEWIND                                                   
      GO TO 700                                                                 
C  PREVIOUS BLOCK EXISTS BUT THE NEWLY CREATED BLOCK IS LAST                    
220   MEM( IPREV+1 ) = NEWIND                                                   
      MEM( NEWIND  ) = IPREV                                                    
      MEM( NEWIND+1) = 0                                                        
      GO TO 700                                                                 
C  NEW BLOCK IS THE ONLY FREE BLOCK                                             
230   IDBFRE  = NEWIND                                                          
      MEM( NEWIND  ) = 0                                                        
      MEM( NEWIND+1) = 0                                                        
      GO TO 700                                                                 
C  FREE CHAIN IS EXHAUSTED                                                      
240   IDBFRE = 0                                                                
701   INDEX = -1                                                                
      RETURN                                                                    
700   CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
