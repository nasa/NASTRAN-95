      SUBROUTINE MMA1 ( ZI, ZR, ZD, ZC, ZDC )
C
C     MMA1 PERFORMS THE MATRIX OPERATION USING METHODS 10 AND 11
C       (+/-)A(T & NT) * B (+/-)C = D
C     
C     MMA1 IS DESIGNED AS FOLLOWS:
C       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
C       2.  UNPACK AS MANY COLUMNS OF "A" INTO MEMORY AS POSSIBLE
C           LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
C       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
C       4.  CALL UNPACK TO READ MATRICES "A" AND "C".
C       5.  FOR METHOD 10, CALL UNPACK TO READ COLUMNS OF MATRIX "B".
C       6.  FOR METHOD 11, CALL MMARC1,2,3,4 TO READ COLUMNS OF MATRIX "B"
C           INTO MEMORY IN COMPACT FORM.
C
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
      DATA    MODULE / 4HMMA1  , 4H     ,4H    /
      DATA    KZERO  / 1H0   /   
      DATA    KONE   / 1H1   /
      DATA    JBEGN  / 4HBEGN/ , JEND  / 3HEND /
      MODULE( 3 ) = JBEGN
      IF ( NBSTOR .EQ. 1 ) MODULE( 2 ) = KZERO
      IF ( NBSTOR .EQ. 2 ) MODULE( 2 ) = KONE
      CALL CONMSG ( MODULE, 3, 0 )
      INCRU  = 1
      TYPEI  = NDTYPE
      TYPEP  = NDTYPE
      SIGN   = SIGNAB
      NWDD   = NWORDS( NDTYPE )
      NWDB   = NWORDS( NBTYPE )
      IRFILE = FILEB( 1 )
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
      IF ( NBSTOR .EQ. 1 .OR. KSYS58 .EQ. 10 ) IDX = 1 + NWDD*NBR  
      IF ( NBSTOR .NE. 2 .AND. KSYS58 .NE. 11 ) GO TO 90
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
      IBUF1  = NZ    - SYSBUF
      IBUF2  = IBUF1 - SYSBUF
      IBUF3  = 0
      IF ( FILEC( 1 ) .EQ. 0 .OR. SIGNC .EQ. 0 ) GO TO 100
      IBUF3  = IBUF2 - SYSBUF
      IBUF4  = IBUF3 - SYSBUF
      GO TO 200
100   CONTINUE
      IBUF4  = IBUF2 - SYSBUF
200   CONTINUE
      LASMEM = IBUF4 - 1
      IPROW1 = 1
      IPROWN = NDR
      INCRP  = 1
      CALL GOPEN  ( FILEA, ZR( IBUF1 ), RDREW )
      CALL GOPEN  ( FILEB, ZR( IBUF2 ), RDREW )
C
C   DETERMINE HOW MANY COLUMNS OF A CAN BE READ INTO MEMORY IN ONE PASS
C
220   CONTINUE
      IAVAIL = LASMEM - IAX + 1 
C
C   NCOLPP  -  NUMBER OF COLUMNS OF "A" THAT CAN BE READ IN ONE PASS
C   NPASS   -  NUMBER OF PASSES NEEDED TO READ ENTIRE "A" MATRIX
C      
      NCOLPP = IAVAIL / ( 2+NWDD*NAR )
      IF ( NCOLPP .GT. NAC ) NCOLPP = NAC
      IF ( NCOLPP .LE. 0 )
     &   CALL MESAGE ( -8, IAVAIL+NWDD*NAR, MODULE )
      NPASS  = ( (NAC-1) / NCOLPP ) + 1
      IF ( NPASS .EQ. 1 .OR. IBUF3 .NE. 0 ) GO TO 250
C
C MUST ALLOCATE TWO BUFFERS FOR MULTIPLE PASSES
C
      IBUF3  = IBUF2 - SYSBUF
      IBUF4  = IBUF3 - SYSBUF
      LASMEM = IBUF4 - 1
      GO TO 220
250   CONTINUE
      DO 70000 M = 1, NPASS
      IPASS = M
      IBROW = ( M-1 ) * NCOLPP
      IF ( M .EQ. NPASS ) GO TO 400
C
C MULTIPLE PASSES REQUIRED, DETERMINE PROPER FILE FOR OUTPUT SO THAT 
C REQUESTED OUTPUT FILE IS USED ON THE LAST PASS
C      
      ITEST = NPASS - M
      ITEST = MOD( ITEST, 2 )
      IF ( ITEST .NE. 0 ) GO TO 350
      IFILE = SCRTCH
      OFILE = FILED( 1 )
      GO TO 380
350   IFILE = FILED( 1 )
      OFILE = SCRTCH
380   CONTINUE
      IF ( M .EQ. 1 ) GO TO 300
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
      NCOLPP = NAC - NCOLPP*(NPASS-1)
      OFILE = FILED( 1 )
      IFILE = SCRTCH   
      CALL REWIND( FILEB )
      CALL SKPREC( FILEB, 1 )
      CALL GOPEN ( FILED, ZR( IBUF4 ), WRTREW)
      FILED( 2 ) = 0
      FILED( 6 ) = 0
      FILED( 7 ) = 0
      IF ( M .EQ. 1 ) GO TO 310
      CALL GOPEN  ( IFILE, ZR( IBUF3 ), RDREW )     
490   CONTINUE
      INDX   = IAX
      TYPEU  = NDTYPE
      DO 900 I = 1, NCOLPP
      IUROW1 = -1
      CALL UNPACK ( *500, FILEA, ZR( INDX+2 ) )
      ZI( INDX   ) = IUROW1
      ZI( INDX+1 ) = IUROWN
      INDX    = INDX + 2 + NWDD*NAR   
      GO TO 900
500   CONTINUE
C NULL COLUMN READ
      ZI( INDX   ) = 0
      ZI( INDX+1 ) = 0
      INDX    = INDX + 2 + NWDD*NAR   
900   CONTINUE
      IF ( KSYS58 .EQ. 10 ) GO TO 950
      IF ( KSYS58 .EQ. 11 ) GO TO 1000
      IF ( NBSTOR .EQ. 2  ) GO TO 1000     
C PROCESS ALL OF THE COLUMNS OF "B", ADD "C" DATA ON FIRST PASS
950   CONTINUE
      IF ( NDTYPE .EQ. 1 ) CALL MMA101( ZI, ZR )
      IF ( NDTYPE .EQ. 2 ) CALL MMA102( ZI, ZD )
      IF ( NDTYPE .EQ. 3 ) CALL MMA103( ZI, ZC )
      IF ( NDTYPE .EQ. 4 ) CALL MMA104( ZI, ZD, ZDC )
      GO TO 60000
1000  CONTINUE
      IF ( NDTYPE .EQ. 1 ) CALL MMA111( ZI, ZR )
      IF ( NDTYPE .EQ. 2 ) CALL MMA112( ZI, ZD )
      IF ( NDTYPE .EQ. 3 ) CALL MMA113( ZI, ZC )
      IF ( NDTYPE .EQ. 4 ) CALL MMA114( ZI, ZD, ZDC )
60000 CONTINUE
      CALL CLOSE ( IFILE, CLSREW )
      CALL CLOSE ( OFILE, CLSREW )
70000 CONTINUE
      CALL CLOSE ( FILEA, CLSREW )
      CALL CLOSE ( FILEB, CLSREW )
      MODULE( 3 ) = JEND
      CALL CONMSG ( MODULE, 3, 0 )
      RETURN
      END

