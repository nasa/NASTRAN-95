      SUBROUTINE MMA3 ( ZI, ZR, ZD, ZC, ZDC )
C
C     MMA3 PERFORMS THE MATRIX OPERATION USING METHODS 30, 31 AND 32
C       (+/-)A(T & NT) * B (+/-)C = D
C     
C     MMA3 IS DESIGNED AS FOLLOWS:
C       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
C       2.  PACK (IN COMPACT FORM) AS MANY COLUMNS OF "A" INTO MEMORY 
C           AS POSSIBLE LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
C           SEE SUBROUTINES MMARM1,2,3,4 FOR FORMAT OF COMPACT FORM.
C       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
C       4.  FOR METHODS 30 AND 31, CALL UNPACK TO READ MATRIX "C".
C       5.  FOR METHOD 30, CALL UNPACK TO READ COLUMNS OF MATRIX "B".
C       6.  FOR METHOD 31, CALL MMARC1,2,3,4 TO READ COLUMNS OF "B" INTO
C           MEMORY IN COMPACT FORM.
C       7.  FOR METHOD 32, CALL MMARC1,2,3,4 TO READ COLUMNS OF "B" AND
C           "C" INTO MEMORY IN COMPACT FORM.
C       8.  FOR METHODS 30 AND 31, CALL PACK TO WRITE "D" MATRIX.
C       9.  FOR METHOD 32, CALL BLDPK TO WRITE "D" MATRIX.
C
      INTEGER           ZI(2)      ,MODULE(3),SYSBUF,SCRTCH
      INTEGER           TYPEI      ,TYPEP    ,TYPEU ,SIGNAB, SIGNC
      INTEGER           RD         ,RDREW    ,WRT   ,WRTREW, CLSREW,CLS
      INTEGER           OFILE      ,FILEA    ,FILEB ,FILEC , FILED
      REAL              ZR(2)
      DOUBLE PRECISION  ZD(2)
      COMPLEX           ZC(2)
      DOUBLE COMPLEX    ZDC(2)
      COMMON / NAMES  / RD         ,RDREW    ,WRT   ,WRTREW, CLSREW,CLS
      COMMON / TYPE   / IPRC(2)    ,NWORDS(4),IRC(4)
      COMMON / MPYADX / FILEA(7)   ,FILEB(7) ,FILEC(7)    
     1,                 FILED(7)   ,NZ       ,T     ,SIGNAB,SIGNC ,PREC1 
     2,                 SCRTCH     ,TIME
      INCLUDE           'MMACOM.COM'
      COMMON / SYSTEM / KSYSTM(152)
      COMMON / UNPAKX / TYPEU      ,IUROW1   ,IUROWN, INCRU
      COMMON / PACKX  / TYPEI      ,TYPEP    ,IPROW1, IPROWN , INCRP
      EQUIVALENCE       (KSYSTM( 1),SYSBUF)  , (KSYSTM( 2),NOUT  ) 
      EQUIVALENCE       (KSYSTM(58),KSYS58)
      EQUIVALENCE       (FILEA(2)  ,NAC   )  , (FILEA(3)  ,NAR   )
     1,                 (FILEA(4)  ,NAFORM)  , (FILEA(5)  ,NATYPE)
     2,                 (FILEA(6)  ,NANZWD)  , (FILEA(7)  ,NADENS)
      EQUIVALENCE       (FILEB(2)  ,NBC   )  , (FILEB(3)  ,NBR   )
     1,                 (FILEB(4)  ,NBFORM)  , (FILEB(5)  ,NBTYPE)
     2,                 (FILEB(6)  ,NBNZWD)  , (FILEB(7)  ,NBDENS)
      EQUIVALENCE       (FILEC(2)  ,NCC   )  , (FILEC(3)  ,NCR   )
     1,                 (FILEC(4)  ,NCFORM)  , (FILEC(5)  ,NCTYPE)
     2,                 (FILEC(6)  ,NCNZWD)  , (FILEC(7)  ,NCDENS)
      EQUIVALENCE       (FILED(2)  ,NDC   )  , (FILED(3)  ,NDR   )
     1,                 (FILED(4)  ,NDFORM)  , (FILED(5)  ,NDTYPE)
     2,                 (FILED(6)  ,NDNZWD)  , (FILED(7)  ,NDDENS)
C
      DATA    MODULE / 4HMMA3  , 4H     ,4H    /
      DATA    KZERO  / 1H0   /   
      DATA    KONE   / 1H1   /
      DATA    KTWO   / 1H2   /
      DATA    JBEGN  / 4HBEGN/ , JEND  / 3HEND /
      MODULE( 3 ) = JBEGN
      IF ( METHOD .EQ. 30 ) MODULE( 2 ) = KZERO
      IF ( METHOD .EQ. 31 ) MODULE( 2 ) = KONE     
      IF ( METHOD .EQ. 32 ) MODULE( 2 ) = KTWO   
      CALL CONMSG ( MODULE, 3, 0 )
      INCRU  = 1
      TYPEI  = NDTYPE
      TYPEP  = NDTYPE
      NWDD   = NWORDS( NDTYPE )
      NWDB   = NWORDS( NBTYPE )
C
C   OPEN CORE ALLOCATION AS FOLLOWS:
C     Z( 1        ) = ARRAY FOR ONE COLUMN OF "B" MATRIX
C     Z( IDX      ) = ARRAY FOR ONE COLUMN OF "D" MATRIX
C     Z( IAX      ) = ARRAY FOR MULTIPLE COLUMNS OF "A" MATRIX
C        THROUGH
C     Z( LASMEM   )
C     Z( IBUF4    ) = BUFFER FOR "D" FILE
C     Z( IBUF3    ) = BUFFER FOR "C" FILE
C     Z( IBUF2    ) = BUFFER FOR "B" FILE 
C     Z( IBUF1    ) = BUFFER FOR "A" FILE
C     Z( NZ       ) = END OF OPEN CORE THAT IS AVAILABLE
C
      IDX = 1 + NWDD*NBR  
      IF ( METHOD .NE. 31 .AND. METHOD .NE. 32) GO TO 90
C
C REDEFINE IDX AND INSURE A QUAD WORD BOUNDARY FOR COMPLEX DOUBLE
C
      IDX = 1 + NWDD*NBR + NBR  
      ITEST = MOD( IDX, 4 )
      IF ( ITEST .EQ. 1 ) GO TO 90
      IF ( ITEST .EQ. 0 ) IDX = IDX + 1
      IF ( ITEST .EQ. 2 ) IDX = IDX + 3
      IF ( ITEST .EQ. 3 ) IDX = IDX + 2
90    CONTINUE
      IDX2   = ( ( IDX+1 ) / 2 ) - 1  
      IDX4   = ( IDX+1 ) / 4    
      IAX    = IDX  + NWDD*NDR     
      IF ( METHOD .NE. 32 ) GO TO 96
C
C FOR METHOD 32, INSURE IAX IS ON QUAD WORD BOUNDARY FOR COMPLEX DOUBLE
C 
      IAX    = IDX  + NWDD*NDR + NDR
      ITEST = MOD( IAX, 4 )
      IF ( ITEST .EQ. 1 ) GO TO 96
      IF ( ITEST .EQ. 0 ) IAX = IAX + 1
      IF ( ITEST .EQ. 2 ) IAX = IAX + 3
      IF ( ITEST .EQ. 3 ) IAX = IAX + 2
96    CONTINUE
      IAX2   = ( ( IAX+1 ) / 2 ) 
      IBUF1  = NZ    - SYSBUF
      IBUF2  = IBUF1 - SYSBUF
      IBUF3  = IBUF2 - SYSBUF
      IBUF4  = IBUF3 - SYSBUF
      LASMEM = IBUF4 - 1
      LASMEM = LASMEM - IAX
      IPROW1 = 1
      IPROWN = NDR
      INCRP  = 1
      CALL GOPEN  ( FILEA, ZR( IBUF1 ), RDREW )
      CALL GOPEN  ( FILEB, ZR( IBUF2 ), RDREW )
      IPASS  = 0
      IRCOLN = 0
100   IPASS  = IPASS + 1
      IRCOL1 = IRCOLN + 1
      IRCOLN = NAC
      IRFILE = FILEA( 1 )       
      SIGN   = SIGNAB      
      IF ( IPASS  .NE. 1 ) 
     &  CALL DSSPOS ( IRFILE, IRPOS( 1 ), IRPOS( 2 ),IRPOS( 3 ) )
      IF ( NDTYPE .EQ. 1 ) CALL MMARM1 ( ZI( IAX ), ZR( IAX  ), 0 )
      IF ( NDTYPE .EQ. 2 ) CALL MMARM2 ( ZI( IAX ), ZD( IAX2 ), 0 )
      IF ( NDTYPE .EQ. 3 ) CALL MMARM3 ( ZI( IAX ), ZC( IAX2 ), 0 )
      IF ( NDTYPE .EQ. 4 ) CALL MMARM4 ( ZI( IAX ), ZD( IAX2 ), 0 )
      NCOLPP = IRCOLN - IRCOL1 + 1
      IBROW  = IRCOL1 - 1
      IF ( IRCOLN .EQ. NAC ) GO TO 400
      ITEST = MOD( IPASS, 2 )
      IF ( ITEST .EQ. 0 ) GO TO 350
      IFILE = SCRTCH
      OFILE = FILED( 1 )
      GO TO 380
350   IFILE = FILED( 1 )
      OFILE = SCRTCH
380   CONTINUE
      IF ( IPASS .EQ. 1 ) GO TO 300
      CALL REWIND( FILEB )
      CALL SKPREC( FILEB, 1 )
      CALL GOPEN ( IFILE, ZR( IBUF3 ), RDREW )
      CALL GOPEN ( OFILE, ZR( IBUF4 ), WRTREW)
      GO TO 490
C FIRST PASS, OPEN "C" FILE IF IT EXISTS
300   CONTINUE
      CALL GOPEN ( OFILE, ZR( IBUF4 ), WRTREW)      
310   IFILE = FILEC( 1 )
      IF ( SIGNC .EQ. 0 ) IFILE = 0
      IF ( IFILE .EQ. 0 ) GO TO 490 
      CALL GOPEN  ( IFILE, ZR( IBUF3 ), RDREW )     
      GO TO 490
C LAST PASS, CREATE OUTPUT FILE
400   CONTINUE
      IF ( IFILE .EQ. 0 ) IFILE = SCRTCH
      IF ( OFILE .EQ. FILED( 1 ) .AND. IPASS .NE. 1 ) 
     &     CALL FILSWI( IFILE, OFILE )
      OFILE = FILED( 1 )
      IFILE = SCRTCH   
      CALL REWIND( FILEB )
      CALL SKPREC( FILEB, 1 )
      CALL GOPEN ( FILED, ZR( IBUF4 ), WRTREW)
      FILED( 2 ) = 0
      FILED( 6 ) = 0
      FILED( 7 ) = 0
      IF ( IPASS .EQ. 1 ) GO TO 310
      CALL GOPEN  ( IFILE, ZR( IBUF3 ), RDREW )     
490   CONTINUE
      SIGN = 1
      IF ( METHOD .EQ. 30 ) GO TO 950
      IF ( METHOD .EQ. 31 ) GO TO 1000  
      IF ( METHOD .EQ. 32 ) GO TO 2000  
C PROCESS ALL OF THE COLUMNS OF "B", ADD "C" DATA ON FIRST PASS
950   CONTINUE
      IF ( NDTYPE .EQ. 1 ) CALL MMA301( ZI, ZR )
      IF ( NDTYPE .EQ. 2 ) CALL MMA302( ZI, ZD )
      IF ( NDTYPE .EQ. 3 ) CALL MMA303( ZI, ZC )
      IF ( NDTYPE .EQ. 4 ) CALL MMA304( ZI, ZD, ZDC )
      GO TO 60000
1000  CONTINUE
      IF ( NDTYPE .EQ. 1 ) CALL MMA311( ZI, ZR )
      IF ( NDTYPE .EQ. 2 ) CALL MMA312( ZI, ZD )
      IF ( NDTYPE .EQ. 3 ) CALL MMA313( ZI, ZC )
      IF ( NDTYPE .EQ. 4 ) CALL MMA314( ZI, ZD, ZDC )
      GO TO 60000
2000  CONTINUE
      IF ( NDTYPE .EQ. 1 ) CALL MMA321( ZI, ZR )
      IF ( NDTYPE .EQ. 2 ) CALL MMA322( ZI, ZD )
      IF ( NDTYPE .EQ. 3 ) CALL MMA323( ZI, ZC )
      IF ( NDTYPE .EQ. 4 ) CALL MMA324( ZI, ZD )
60000 CONTINUE
      CALL CLOSE ( IFILE, CLSREW )
      CALL CLOSE ( OFILE, CLSREW )
      IF ( IRCOLN .LT. NAC ) GO TO 100
C
C ALL COLUMNS OF A HAVE BEEN PROCESSED, MULTIPLICATION COMPLETE
C
      CALL CLOSE ( FILEA, CLSREW )
      CALL CLOSE ( FILEB, CLSREW )
      MODULE( 3 ) = JEND
      CALL CONMSG ( MODULE, 3, 0 )
      RETURN
      END
