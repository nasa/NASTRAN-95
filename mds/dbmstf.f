      SUBROUTINE DBMSTF                                                         
      INCLUDE   'DSIOF.COM'                                                     
      COMMON / SYSTEM / ISYSBF, IWR                                             
      COMMON / LOGOUT / LOUT
      IBLKSZ = ISYSBF - 4                                                       
      IF ( MAXBLK .NE. 0 )  PERC1 = MAXBLK*1.0 / MAXALC                         
      IPERC1 = PERC1 * 100.                                                     
      IMEMNU = ( MAXALC - MAXBLK ) * LENALC                                     
      WRITE( LOUT, 901 ) LENOPC, IDBLEN, MAXBLK, MAXALC, IPERC1, MAXDSK          
     &, IBLKSZ, NUMOPN, NUMCLS, NUMWRI, NUMREA                                  
      IF ( IDBDIR .NE. 0 ) WRITE( LOUT, 902 ) IMEMNU                             
901   FORMAT(1H1                                                                
     &,5X,'STATISTICS ON IN-MEMORY DATA BASE AND DISK I/O USAGE',/              
     &,/,8X,' LENGTH (IN WORDS) OF OPEN CORE ALLOCATED          ',I8            
     &,/,8X,' LENGTH (IN WORDS) OF IN-MEMORY DATA BASE ALLOCATED',I8            
     &,/,8X,' NUMBER OF BLOCKS USED IN THE IN-MEMORY DATA BASE  ',I8            
     &,/,8X,' NUMBER OF BLOCKS ALLOCATED FOR THE IN-MEMORY DATA ',I8            
     &,/,8X,' PERCENTAGE OF IN-MEMORY DATA USED                 ',I8,'%'       
     &,/,8X,' TOTAL BLOCKS WRITTEN TO DISK                      ',I8            
     &,/,8X,' BLOCK SIZE (IN WORDS)                             ',I8            
     &,/,8X,' NUMBER OF OPENS TO DISK FILES                     ',I8            
     &,/,8X,' NUMBER OF CLOSES TO DISK FILES                    ',I8            
     &,/,8X,' NUMBER OF WRITES TO DISK FILES                    ',I8            
     &,/,8X,' NUMBER OF READS FROM DISK FILES                   ',I8)            
902   FORMAT(                                                                   
     &   8X,' MEMORY (IN WORDS) NOT USED BY IN-MEM. DATA BASE   ',I8            
     & )                                                                        
      RETURN                                                                    
      END                                                                       
