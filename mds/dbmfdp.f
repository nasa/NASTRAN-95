      SUBROUTINE DBMFDP                                                         
C********************************************************************           
C     DBMFDP- DUMPS THE DIRECTORY CHAIN OF A GIVEN FILE.                        
C             ARGUMENT IDIR IS THE IN-MEMORY DIRECTORY FOR THE FILE             
C********************************************************************           
      COMMON / SYSTEM / ISYSBF, IWR                                             
      INCLUDE  'ZZZZZZ.COM'
      INCLUDE  'DSIOF.COM'                                                      
      IBASE  = LOCFX( MEM )                                                     
      IVAL2  = IBASE + FCB(  9, IFILEX )                                        
      IVAL3  = IBASE + FCB( 10, IFILEX )                                        
      IVAL4  = IBASE + FCB( 11, IFILEX )                                        
      INDEX  = FCB( 10, IFILEX )                                                
      LBLOCK = MEM( INDEX+3 )                                                   
      WRITE ( IWR, 902 ) IFILEX, IVAL2, IVAL3, IVAL4, FCB(12,IFILEX)           
      WRITE ( IWR, 903 )                                                        
      NEXT = FCB(  9, IFILEX )                                                  
      ICNT = 0                                                                  
      IF ( NEXT .EQ. 0 ) GO TO 25                                               
20    ICNT = ICNT + 1                                                           
      IF ( NEXT .EQ. 0 ) GO TO 30                                               
      IVAL = IBASE + NEXT                                                       
      IVALP= IBASE + MEM(NEXT)                                                  
      IVALN= IBASE + MEM(NEXT+1)                                                
      IF ( MEM( NEXT   ) .EQ. 0 ) IVALP = 0                                     
      IF ( MEM( NEXT+1 ) .EQ. 0 ) IVALN = 0                                     
      WRITE ( IWR, 904 )                                                        
     &       MEM(NEXT+3),MEM(NEXT+7),IVAL,IVALP,IVALN,MEM(NEXT+2)               
990   FORMAT( 12(8(1X,I8),/))                                                   
      NEXT = MEM( NEXT+1 )                                                      
      GO TO 20                                                                  
25    WRITE( IWR, 907 )                                                         
30    CONTINUE                                                                  
      WRITE( IWR, 908 )                                                         
      RETURN                                                                    
902   FORMAT(///,25X,' DUMP OF FILE CHAIN FOR UNIT=',I6,/                       
     &,14X,'( BLOCK ADDRESSES ARE IN WORDS,  BLOCK LENGTHS IN WORDS)',/         
     &,/,7X,                                                                    
     &' FIRST BLOCK ADDRESS   ',I12,'   LAST BLOCK ADDRESS      ',I12          
     &,/,7X,                                                                    
     &' CURRENT BLOCK ADDRESS ',I12,'   ORIGINAL BUFFER ADDRESS ',I12)          
903   FORMAT(/,                                                                 
     & '  IN-MEM     BUFFER',/                                                  
     &,' BLOCK NO.  BLOCK NO  BLOCK ADDRESS  PREV. BLOCK   NEXT BLOCK '         
     &,' LENGTH')                                                               
904   FORMAT( I9,I11,5X,I12,7X,I12,5X,I12,I12)                                      
907   FORMAT(//' *************** NO BLOCK ALLOCATED TO FILE **********')        
908   FORMAT(///)                                                               
      END                                                                       
