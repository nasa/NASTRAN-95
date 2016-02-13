      SUBROUTINE DBMDFC                                                         
C********************************************************************           
C     DBMDFC - DUMPS THE FREE CHAIN                                             
C********************************************************************           
      INCLUDE  'DSIOF.COM'                                                      
      INCLUDE  'ZZZZZZ.COM'
      COMMON / SYSTEM / ISYSBF, IWR                                             
      WRITE ( IWR, 906 )                                                        
      WRITE ( IWR, 907 )                                                        
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
      WRITE ( IWR, 908 ) ICNT,IVALP,IVAL,IVALN,MEM(NEXT+2)                      
      NEXT = MEM( NEXT+1 )                                                      
      GO TO 30                                                                  
40    CONTINUE                                                                  
      WRITE( IWR, 909 )                                                         
      GO TO 60                                                                  
50    CONTINUE                                                                  
      WRITE( IWR, 910 ) ITOTAL, ITOTBK                                          
60    CONTINUE                                                                  
700   RETURN                                                                    
906   FORMAT(///,31X,' DUMP OF FREE CHAIN',/                                    
     &,13X,' ( BLOCK ADDRESSES IN WORDS,  BLOCK LENGTHS IN WORDS )',/)          
907   FORMAT(10X,                                                               
     &'  BLOCK NO    PREV. BLOCK    BLOCK ADDRESS NEXT BLOCK    LENGTH')        
908   FORMAT( I17,I20,I13,I13,I10)                                              
909   FORMAT(//' *************** NO FREE SPACE REMAINS **************')         
910   FORMAT(///,' TOTAL FREE SPACE IN WORDS            =',I10                  
     &,/,        ' NUMBER OF BLOCKS IN FREE SPACE CHAIN =',I10)                 
      END                                                                       
