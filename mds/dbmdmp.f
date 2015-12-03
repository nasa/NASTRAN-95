      SUBROUTINE DBMDMP                                                         
C********************************************************************           
C     DBMDMP - DUMPS THE IN MEMORY DATA BASE DIRECTORY                          
C********************************************************************           
      INCLUDE 'DSIOF.COM'                                                       
      COMMON / ZZZZZZ / MEM(4)                                                  
      COMMON / SYSTEM / ISYSBF, IWR                                             
      WRITE ( IWR, 900 ) IDBBAS, IDBFRE, IDBDIR, INDBAS, INDCLR, INDCBP         
     &,                  NBLOCK, LENALC, IOCODE, IFILEX, NAME,   MAXALC         
     &,                  MAXBLK, MAXDSK, IDBLEN, IDBADR, IBASBF, INDDIR        
     &,                  NUMOPN, NUMCLS, NUMWRI, NUMREA, LENOPC                 
900   FORMAT(/,' CONTENTS OF / DBM / FOLLOW:'                                   
     &,/,' IDBBAS =',I8,' IDBFRE =',I8,' IDBDIR =',I8,' INDBAS =',I8            
     &,/,' INDCLR =',I8,' INDCBP =',I8,' NBLOCK =',I8,' LENALC =',I8            
     &,/,' IOCODE =',I8,' IFILEX =',I8,' NAME   =',I8,' MAXALC =',I8            
     &,/,' MAXBLK =',I8,' MAXDSK =',I8,' IDBLEN =',I8,' IDBADR =',I8            
     &,/,' IBASBF =',I8,' INDDIR =',I8,' NUMOPN =',I8,' NUMCLS =',I8            
     &,/,' NUMWRI =',I8,' NUMREA -',I8,' LENOPC =',I8 )                         
      WRITE ( IWR, 901 )                                                        
901   FORMAT(/,' CONTENTS OF FCB FOLLOW:',/)                                    
      DO 10 I = 1, 80                                                           
      WRITE ( IWR, 902 ) I, ( FCB(K,I),K=1,15)                                  
902   FORMAT(I3,'-',I3,I7,4I5,I12,I2,4I7,2A4,I4)                                
10    CONTINUE                                                                  
      CALL DBMDIA                                                               
C      WRITE ( IWR, 906 )                                                       
C      WRITE ( IWR, 907 )                                                       
      NEXT = IDBFRE                                                             
      ITOTAL = 0                                                                
      ITOTBK = 0                                                                
      ICNT = 0                                                                  
      IF ( NEXT .EQ. 0 ) GO TO 40                                               
30    ICNT = ICNT + 1                                                           
      IF ( NEXT .EQ. 0 ) GO TO 50                                               
      IVAL = NEXT                                                               
      IVALP= MEM(NEXT)                                                          
      IVALN= MEM(NEXT+1)                                                        
      IF ( MEM(NEXT  ) .EQ. 0 ) IVALP = 0                                       
      IF ( MEM(NEXT+1) .EQ. 0 ) IVALN = 0                                       
      ITOTAL = ITOTAL + MEM(NEXT+2)                                             
      ITOTBK = ITOTBK + 1                                                       
C      WRITE ( IWR, 908 ) ICNT,IVAL,IVALP,IVALN,MEM(NEXT+2)                     
      NEXT = MEM( NEXT+1 )                                                      
      GO TO 30                                                                  
40    CONTINUE                                                                  
C      WRITE( IWR, 909 )                                                        
      GO TO 60                                                                  
50    CONTINUE                                                                  
C     WRITE( IWR, 910 ) ITOTAL, ITOTBK                                          
60    CONTINUE                                                                  
700   RETURN                                                                    
906   FORMAT(///,31X,' DUMP OF FREE CHAIN',/                                    
     &,13X,' ( BLOCK ADDRESSES IN BYTES,  BLOCK LENGTHS IN WORDS )',/)          
907   FORMAT(10X,                                                               
     &'  BLOCK NO    BLOCK ADDRESS  PREV. BLOCK   NEXT BLOCK    LENGTH')        
908   FORMAT( I17,I20,I13,I13,I10)                                              
909   FORMAT(//' *************** NO FREE SPACE REMAINS **************')         
910   FORMAT(///,' TOTAL FREE SPACE IN WORDS            =',I10                  
     &,/,        ' NUMBER OF BLOCKS IN FREE SPACE CHAIN =',I10)                 
      END                                                                       
