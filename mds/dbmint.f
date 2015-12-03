      SUBROUTINE DBMINT                                                         
C********************************************************************           
C     DBMINT - INITIALIZES ALL PARAMETERS AND THE FREE BLOCK CHAIN              
C              FOR THE IN-MEMORY DATA BASE.                                     
C                                                                               
C        ARGUMENTS                                                              
C            IDBADR - (INPUT)-BEGINNING ADDRESS FOR IN-MEMORY DATA BASE         
C            IDBLEN - (INPUT)-NUMBER OF MEMORY WORDS FOR IN-MEMORY              
C                             DATA BASE                                         
C        / DBMPAR/                                                              
C            IDBBAS - (OUTPUT)-INDEX TO IN-MEMORY DATA BASE RELATIVE            
C                              TO /DBM/                                         
C            IDBFRE - (OUTPUT)-INDEX TO FREE CHAIN OF IN-MEMORY DATA            
C                              BASE RELATIVE TO /DBM/                           
C            IDBDIR - (OUTPUT)-INDEX TO FIRST DIRECTORY BLOCK                   
C        FREE CHAIN FORMAT                                                      
C               IDBFRE==> WORD 0 - 0   (POINTS TO PREVIOUS FREE BLOCK           
C                                      IN CHAIN, ALWAYS 0 FOR 1ST BLK)          
C                         WORD 1 - 0   (POINTS TO NEXT BLOCK IN CHAIN           
C                                      -INITIALLY SET TO ZERO)                  
C                         WORD 2 - L   (NUMBER OF FREE WORDS IN BLOCK)          
C        DIRECTORY FORMAT                                                       
C               THE FIRST TWO WORDS OF THE DIRECTORY BLOCK CONTAIN:             
C                   WORD  0 - MAXIMUM NUMBER OF ENTRIES IN DIRECTORY            
C                   WORD  1 - CURRENT ENTRIES IN THE DIRECTORY                  
C               EACH ENTRY IN THE DIRECTORY HAS THE FOLLOWING FORMAT            
C               (NOTE, FIRST ENTRY BEGINS AT WORD 3 OF BLOCK)                   
C                   WORD  0 - UNIT NUMBER OF DMAP FILE AS FOUND IN FIAT         
C                   WORD  1 - INDEX TO FIRST IN-MEMORY DATA BLOCK               
C                   WORD  2 - INDEX TO LAST IN-MEMORY DATA BLOCK                
C                   WORD  3 - INDEX TO CURRENT IN-MEMORY DATA BLOCK             
C                   WORD  4 - CURRENT BLOCK NUMBER BEING PROCESSED              
C                   WORD  5 - LAST BLOCK NUMBER                                 
C                   WORD  6 - ORIGINAL BUFFER ADDRESS                           
C                   WORD  7 - TOTAL BLOCKS (EXT. FILE + IN M. DB)               
C                   WORD  8 - OPEN FLAG FOR EXT. FILE (0,NO;1,YES)              
C               WORDS  9-10 - DMAP FILE NAME                                    
C               WORDS 11-16 - DMAP FILE TRAILER                                 
C********************************************************************           
      INCLUDE  'DSIOF.COM'
      COMMON / SYSTEM / ISYSBF, IWR                                             
      COMMON / ZZZZZZ / MEM( 4 )                                                
      IDBDIR = 0                                                                
      IF ( IDBLEN .EQ. 0 ) GO TO 700                                            
C  INITIALIZE THE CHAIN OF FREE BLOCKS AS ONE BIG FREE BLOCK                    
      IDBBAS = LOCFX( MEM )                                                     
      IDBFRE = IDBADR - IDBBAS + 1                                              
      MEM( IDBFRE )  = 0                                                        
      MEM( IDBFRE+1) = 0                                                        
      MEM( IDBFRE+2) = IDBLEN - 2                                               
      MAXALC         = IDBLEN / ( ISYSBF-3+4 )                                  
      IDBDIR = 1                                                                
700   CONTINUE                                                                  
      END                                                                       
