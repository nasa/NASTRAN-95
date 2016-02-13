      SUBROUTINE DBMDIA                                                         
C********************************************************************           
C     DBMDIA - DUMPS THE IN MEMORY DATA BASE DIRECTORY                          
C********************************************************************           
      INCLUDE  'DSIOF.COM'                                                      
      INCLUDE  'ZZZZZZ.COM'
      COMMON / SYSTEM / ISYSBF, IWR                                             
      INTEGER           SCRATCH(2)                                              
      DATA              SCRATCH / 'SCRA','TCHX' /                               
      IBLKSZ = ISYSBF - 4                                                       
      ITOTI  = 0                                                                
      ITOTX  = 0                                                                
      WRITE ( IWR, 903 )                                                        
      DO 20 I = 1, 80                                                           
      IF ( I .EQ. 7 ) GO TO 20
      IF ( FCB( 9,I ) .EQ. 0 .AND. FCB( 5,I ) .EQ. 0 ) GO TO 20                 
      INDEX = FCB( 10, I )                                                      
      IINTB = 0                                                                 
      IEXTB = 0                                                                 
      IF ( FCB( 9,I ) .NE. 0 ) IINTB = MEM( INDEX+3 )                           
      ITOTI = ITOTI + IINTB                                                     
      IF ( FCB( 5,I ) .NE. 0 ) IEXTB = FCB(6,I) - FCB( 5,I) + 1                 
      IF ( IEXTB .GE. FCB( 7, IFILEX ) ) GO TO 20
      ITOTX = ITOTX + IEXTB                                                     
      IF ( FCB( 13,I ) .NE. 0 ) GO TO 15                                        
      FCB( 13,I ) = SCRATCH(1)                                                  
      FCB( 14,I ) = SCRATCH(2)                                                  
15    CONTINUE                                                                  
      WRITE ( IWR, 904 ) I, FCB( 13,I ), FCB( 14,I ), FCB( 4,I )                
     &,                  IINTB, IEXTB                                           
20    CONTINUE                                                                  
      WRITE ( IWR, 905 ) ITOTI, ITOTX                                           
C      WRITE ( IWR, 906 ) MAXBLK, MAXDSK, MAXALC, IBLKSZ                        
700   RETURN                                                                    
903   FORMAT(///,27X,' MEMORY DATA BASE DIRECTORY',//,                          
     &'    UNIT    NAME   CURRENT  IN-MEM'                                      
     &,'   DISK ',/,                                                            
     &'                    BLOCK   BLOCKS'                                      
     &,'  BLOCKS ',/)                                                           
904   FORMAT(I7,3X,2A4,2X,I6,2X,I6,2X,I6 )                                      
905   FORMAT(/,' CURRENT IN-MEMORY BLOCKS =',I8                                 
     &      ,/,' CURRENT DISK BLOCKS      =',I8 )                               
906   FORMAT(/,' MAXIMUM IN-MEMORY BLOCKS USED                   =',I8          
     &      ,/,' MAXIMUM DISK BLOCKS WRITTEN                     =',I8          
     &      ,/,' BLOCKS INITIALLY ALLOCATED FOR THE IN-MEMORY DB =',I8          
     &      ,/,' BLOCK SIZE                                      =',I8 )        
      END                                                                       
