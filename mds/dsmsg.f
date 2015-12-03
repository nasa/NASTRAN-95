      SUBROUTINE DSMSG ( IFLAG )                                                
      INCLUDE 'DSIOF.COM'                                                       
      COMMON / DSIO   / MSSOFT                                                  
      INCLUDE 'GINOX.COM'                                                       
      COMMON / LOGOUT / LOUT                                                    
      INCLUDE 'XNSTRN.COM'                                                      
      COMMON / DDIOSV / IFLPOS(2,MAXPRI)                                        
      COMMON / SOFCOM / NFILES,FILNAM(10),FILSIZ(10)                            
      COMMON / SYS    / BLKSIZ,DIRSIZ,SUPSIZ,AVBLKS,HIBLK                       
      COMMON / SYSTEM / ISYSTM(175)                                             
C                                                                               
      INTEGER XNAME(2), BLANK                                                   
      INTEGER FILNAM,FILSIZ,BLKSIZ,DIRSIZ,SUPSIZ,AVBLKS,HIBLK                   
      INTEGER GINO(52)                                                          
C                                                                               
      EQUIVALENCE ( IEOR, GINO(1) )                                             
      EQUIVALENCE (ISYSTM(  2), IWR   ),                                        
     *            (ISYSTM(151), NLLOG ),                                        
     *            (ISYSTM(152), LOGLIN)                                         
C                                                                               
      DATA BLANK / 1H /                                                         
      DATA INAME /4HDSMS/                                                       
C                                                                               
      CALL FNAME ( NAME, XNAME )                                                
      IF ( XNAME( 1 ) .NE. 0 ) GO TO 4                                          
      XNAME( 1 ) = BLANK                                                        
      XNAME( 2 ) = BLANK                                                        
    4 CONTINUE                                                                  
      IF ( IABS(IFLAG) .EQ. 777 ) GO TO 7770                                    
      IF ( IFLAG .NE. 1 .AND. IFLAG .NE. 2 .AND. IFLAG .NE. 8 )                 
     C      WRITE( IWR, 5 ) IFLAG                                               
    5 FORMAT(' I/O SUBSYSTEM ERROR NUMBER',I10)                                 
      IF ( IFLAG .GT. 100 ) GO TO 1000                                          
      GO TO ( 10, 20, 30, 40, 50, 60, 70, 80, 90 ),IFLAG                        
   10 CONTINUE                            
      WRITE ( LOUT, 15 ) 'OPEN ',XNAME, IOCODE                                  
      LOGLIN = LOGLIN + 1                                                       
      GO TO 90000                                                               
   20 CONTINUE                            
      WRITE ( LOUT, 15 ) 'CLOSE ', XNAME, IOCODE                                
      LOGLIN = LOGLIN + 1                                                       
      GO TO 90000                                                               
   30 WRITE ( IWR, 35 ) XNAME, IFILEX                                           
      GO TO 99999                                                               
   40 WRITE ( IWR, 45 ) XNAME, IFILEX                                           
      GO TO 7000                                                                
   50 WRITE ( IWR, 55 ) XNAME, IFILEX                                           
      GO TO 99999                                                               
   60 WRITE ( IWR, 65 ) XNAME, IFILEX                                           
      GO TO 99999                                                               
   70 WRITE ( IWR, 75 ) XNAME, IFILEX                                           
      GO TO 7000                                                                
   80 CONTINUE
      WRITE ( LOUT, 85 ) XNAME, IFILEX, IDSN                                    
      GO TO 90000                                                               
   90 WRITE ( IWR, 95 ) NBLOCK                                                  
      GO TO 99999                                                               
  100 CONTINUE                                                                  
      GO TO 7000                                                                
 1000 ITEMP = IFLAG - 100                                                       
      GO TO ( 1010, 1020, 1030, 1040, 1050, 1060, 1070, 1080                    
     *       ,1090, 1100, 1110, 1120, 1130, 1140, 1150, 1160                    
     *       ,1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240                    
     *       ), ITEMP                                                           
C1010 WRITE ( IWR, 1015 ) IOERR                                                 
 1010 GO TO 7000                                                                
 1020 WRITE ( IWR, 1025 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1030 WRITE ( IWR, 1035 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1040 WRITE ( IWR, 1045 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1050 WRITE ( IWR, 1055 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1060 WRITE ( IWR, 1065 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1070 WRITE ( IWR, 1075 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1080 WRITE ( IWR, 1085 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1090 WRITE ( IWR, 1095 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1100 WRITE ( IWR, 1105 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1110 WRITE ( IWR, 1115 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1120 WRITE ( IWR, 1125 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1130 WRITE ( IWR, 1135 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1140 WRITE ( IWR, 1145 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1150 WRITE ( IWR, 1155 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1160 WRITE ( IWR, 1165 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1170 WRITE ( IWR, 1175 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1180 WRITE ( IWR, 1185 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1190 WRITE ( IWR, 1195 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1200 WRITE ( IWR, 1205 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1210 WRITE ( IWR, 1215 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1220 WRITE ( IWR, 1225 ) XNAME, IFILEX                                         
      GO TO 7000                                                                
 1230 CONTINUE                                                                  
 1240 CONTINUE                                                                  
 7000 CONTINUE                                                                  
7770  CONTINUE                                                                  
      WRITE ( IWR, 91000 ) IOERR, NAME, XNAME, IFILEX                           
      WRITE ( IWR, 92000 )                                                      
      DO 7772 I = 1, MAXFCB                                                     
      CALL DSHXDD ( I, MDSFCB( 1, I ), 3 )                                      
 7772 CONTINUE                                                                  
      WRITE( IWR, 92001 )                                                       
      DO 7773 I = 1, 80                                                         
CWKBR NCL93007 11/94
C      WRITE ( IWR, 92003 ) I, ( FCB(K,I),K=1,15)            
      WRITE ( IWR, 92003 ) I, ( FCB(K,I),K=1,17)                                
CWKBR NCL93007 11/04
C92003 FORMAT(I3,'-',I3,I7,4I5,I12,I2,4I7,1X,2A4,I4)  
92003 FORMAT(I3,'-',I3,I7,4I5,I12,I2,4I7,1X,2A4,I4,2I8)                             
 7773 CONTINUE                                                                  
      WRITE ( IWR, 92002)IDBBAS, IDBFRE, IDBDIR, INDBAS, INDCLR, INDCBP         
     &,                  NBLOCK, LENALC, IOCODE, IFILEX, NAME,   MAXALC         
     &,                  MAXBLK, MAXDSK, IDBLEN, IDBADR, IBASBF, INDDIR         
     &,                  NUMOPN, NUMCLS, NUMWRI, NUMREA, LENOPC                
92002 FORMAT(/,' CONTENTS OF / DBM / FOLLOW:'                                   
     &,/,' IDBBAS =',I8,' IDBFRE =',I8,' IDBDIR =',I8,' INDBAS =',I8            
     &,/,' INDCLR =',I8,' INDCBP =',I8,' NBLOCK =',I8,' LENALC =',I8            
     &,/,' IOCODE =',I8,' IFILEX =',I8,' NAME   =',I8,' MAXALC =',I8            
     &,/,' MAXBLK =',I8,' MAXDSK =',I8,' IDBLEN =',I8,' IDBADR =',I8            
     &,/,' IBASBF =',I8,' INDDIR =',I8,' NUMOPN =',I8,' NUMCLS =',I8            
     &,/,' NUMWRI =',I8,' NUMREA =',I8,' LENOPC =',I8)                         
      IF ( IFLAG .GE. 118 .AND. IFLAG .LE. 120 ) GO TO 946                      
      WRITE ( IWR, 95020 )                                                      
      CALL DSHXDP ( NFILES, 16 )                                                
      WRITE ( IWR, 95030 )                                                      
      CALL DSHXDP ( BLKSIZ, 1 )                                                 
      WRITE ( IWR, 96000 )                                                      
      CALL DSHXDP ( IEOR, 59 )                                                  
      WRITE ( IWR, 97000 )                                                      
      DO 944 I = 1, MAXPRI                                                      
      WRITE ( IWR, 97001 ) I, IFLPOS(1,I), IFLPOS(2,I)                          
97001 FORMAT(I5,2I10)                                                           
  944 CONTINUE                                                                  
      LOOP = (NBUFF+LENDSP) / 8 + 4                                             
      INDEX = INDBAS                                                            
      WRITE ( IWR, 94510 )                                                      
      DO 945 I = 1, LOOP                                                        
      III = (I-1) * 8 + 1                                                       
      CALL DSHXDD ( III, IBASE( INDEX ), 8 )                                    
      INDEX = INDEX + 8                                                         
  945 CONTINUE                                                                  
      CALL DBMDIA                                                               
  946 IF ( IFLAG .NE. 777 ) GO TO 99999                                         
C     CALL TRBK( IWR )                                                          
      RETURN                                                                    
91000 FORMAT(' I/O ERROR #',I6,' ON FILE ',Z8,' NAME=',2A4,' UNIT=',I4)         
92000 FORMAT(//' CONTENTS OF MDSFCB' )                                          
92001 FORMAT(//' CONTENTS OF FCB' )                                             
94510 FORMAT(//' CONTENTS OF I/O BUFFER' )                                      
95020 FORMAT(//' CONTENTS OF SOFCOM ')                                          
95030 FORMAT(//' CONTENTS OF SYS ')                                             
96000 FORMAT(//' CONTENTS OF /DSIO/')                                           
97000 FORMAT(//' CONTENTS OF /DDIOSV/')                                         
99999 CALL MESAGE (-61, 0, 0)                                                   
90000 CONTINUE                                                                  
      RETURN                                                                    
   15 FORMAT( 40X, A6, 2A4, 2X, I2 )                                            
   35 FORMAT( ' BUFFER CONFLICTS WITH EXISTING BUFFERS',                        
     * ' ON FILE ',2A4, ' LOGICAL UNIT', I4 )                                   
   45 FORMAT(' ATTEMPT TO READ FILE OPENED FOR WRITE',                          
     * ' FILE=',2A4,' UNIT=',I4 )                                               
   55 FORMAT(' FILE IS ALREADY OPENED-FILE ',2A4,                               
     * ' UNIT=', I4 )                                                           
   65 FORMAT(' ATTEMPT TO WRITE LESS THAN ONE WORD',                            
     *       ' ON FILE ',2A4,' UNIT= ',I4 )                                     
   75 FORMAT(' ATTEMPT TO WRITE ON FILE OPENED FOR READ ',                      
     *       '-FILE=',2A4,' UNIT =',I4)                                         
   85 FORMAT(//,' ****** GINO SUBSYSTEM WILL EXTEND FILE ',2A4,                 
     *       ' ON UNIT',I4,' TO UNIT',I4,' ******' )                            
   95 FORMAT(//,' INSUFFICIENT SPACE ALLOCATION ON FILE NPTP',                  
     *      ' -, NUMBER OF BLOCKS WRITTEN WERE ',I10)                           
 1015 FORMAT(' ERROR DURING I/O REQUEST - ERROR FLAG=',Z8)                      
 1025 FORMAT(' INCORRECT BLOCK NUMBER ENCOUNTERED',                             
     *       ' ON FILE ',2A4,' UNIT=',I4)                                       
 1035 FORMAT(' EXPECTED RH, SB, EF, OR EB CONTROL WORD',                        
     *       ' ON FILE ',2A4,' UNIT=',I4)                                       
 1045 FORMAT(' EXPECTED RT CONTROL WORD ON FILE ',2A4,                          
     *       ' UNIT=',I4)                                                       
 1055 FORMAT(' EXPECTED RH OR EF CONTROL WORD ON FILE ',2A4,                    
     *       ' UNIT=',I4)                                                       
 1065 FORMAT(' EXPECTED RH, EB OR SB CONTROL WORD ON FILE ',2A4,                
     *       ' UNIT=',I4)                                                       
 1075 FORMAT(' REFERENCE IS MADE TO FILE ',2A4,                                 
     *       ' THAT IS NOT OPENED-UNIT=',I4)                                    
 1085 FORMAT(' INSUFFICIENT SPACE FOR I/O CONTROL WORDS ON FILE '               
     *         ,2A4,' UNIT=',I4)                                                
 1095 FORMAT(' TOO MANY TERMS WRITTEN TO STRING ON FILE ',2A4,                  
     *        ' UNIT=',I4)                                                      
 1105 FORMAT(' EXPECTED A SB OR EB CONTROL WORD ON FILE ',2A4,                  
     *       ' UNIT=',I4)                                                       
 1115 FORMAT(' EXPECTED A CH CONTROL WORD ON FILE ',2A4,                        
     *        ' UNIT=',I4)                                                      
 1125 FORMAT(' EXPECTED A SE, SD, CT, OR SH CONTROL WORD ON FILE ',             
     *        2A4,' UNIT=',I4)                                                  
 1135 FORMAT(' ERROR  - CLR.GT. LCW  ON FILE ',2A4, ' UNIT=',I4)                
 1145 FORMAT(' EXPECTED A RT CONTROL WORD ON FILE ',2A4, ' UNIT=',I4)           
 1155 FORMAT(' EXPECTED A CH CONTROL WORD ON FILE ',2A4,' UNIT=',I4)            
 1165 FORMAT(' EXPECTED A CH,ST,SH,SD,RT, OR SE CONTROL WORD ON FILE ',         
     *        2A4,' UNIT=',I4)                                                  
 1175 FORMAT(' EXPECTED A ST CONTROL WORD ON FILE ',2A4, ' UNIT=',I4)           
 1185 FORMAT(' TYPIN OR TYPOUT FOR MATRIX PACK IS OUT OF RANGE ON',             
     *       ' FILE ',2A4,' UNIT=',I4)                                          
 1195 FORMAT(' NON-ASCENDING ROW NUMBER GIVEN',                                 
     *       ' ON FILE ',2A4, ' UNIT=',I10)                                     
 1205 FORMAT(' FILE NAME DOES NOT MATCH STRING CONTROL BLOCK FOR ',             
     *       'FILE ',2A4,' UNIT=',I4)                                           
 1215 FORMAT(' INVALID UNIT NUMBER IN MDSFCB FOR FILE ',2A4,' UNIT=',I4)        
 1225 FORMAT(' INSUFFICIENT NUMBER OF FILES AVAILABLE FOR FILE ',               
     *       2A4,' UNIT=',I4)                                                   
      END                                                                       
