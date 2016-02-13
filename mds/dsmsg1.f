        SUBROUTINE DSMSG1 ( BLOCK )                                             
        COMMON / ZBLPKX / A1(4), IROW1                                          
        COMMON / ZNTPKX / A2(4), IROW2, IEOL2, IEOR2                            
        COMMON / PACKX  / ITIN3, ITOUT3, IROW3, NROW3, INCR3                    
        COMMON / UNPAKX / ITOUT4,IROW4, NROW4, INCR4                            
        COMMON / SYSTEM / NONE,  IWR                                            
        INTEGER BLOCK(15)                                                       
        WRITE( IWR, 9000 )                                                      
        WRITE( IWR, 9010 )                                                      
        WRITE( IWR, 9015 ) BLOCK                                                
        WRITE( IWR, 9020 )                                                      
        WRITE( IWR, 9015 ) A1, IROW1                                            
        WRITE( IWR, 9030 )                                                      
        WRITE( IWR, 9015 ) A2, IROW2, IEOL2, IEOR2                              
        WRITE( IWR, 9040 )                                                      
        WRITE( IWR, 9015 ) ITIN3, ITOUT3, IROW3, NROW3, INCR3                   
        WRITE( IWR, 9050 )                                                      
        WRITE( IWR, 9015 ) ITOUT4, IROW4, NROW4, INCR4                          
9000    FORMAT(' *** ERROR OCCURRED IN PAKUNPK I/O SUBSYSTEM ***')              
9010    FORMAT(' CONTENTS OF THE STRING CONTROL BLOCK')                         
9015    FORMAT(10(5(1X,Z8),/))                                                  
9020    FORMAT(' CONTENTS OF /ZBLPKX/')                                         
9030    FORMAT(' CONTENTS OF /ZNTPKX/')                                         
9040    FORMAT(' CONTENTS OF /PACKX/ ')                                         
9050    FORMAT(' CONTENTS OF /UNPAKX/')                                         
        RETURN                                                                  
        END                                                                     
