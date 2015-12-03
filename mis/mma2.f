      SUBROUTINE MMA2 ( ZI, ZR, ZD, ZC, ZDC )
C
C     MMA2 PERFORMS THE MATRIX OPERATION USING METHODS 20 AND 21 
C       (+/-)A(T & NT) * B (+/-)C = D
C     
C     MMA2 IS DESIGNED AS FOLLOWS:
C       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
C       2.  UNPACK AS MANY COLUMNS OF "B" INTO MEMORY AS POSSIBLE
C           LEAVING SPACE FOR A COLUMN OF "D" FOR EVERY COLUMN "B" READ.
C       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
C       4.  CALL UNPACK TO READ MATRICES "B" AND "C".
C       5.  FOR METHOD 20, CALL UNPACK TO READ COLUMNS OF MATRIX "A".
C       6.  FOR METHOD 21, CALL MMARC1,2,3,4 TO READ COLUMNS OF MATRIX "A"
C           INTO MEMORY IN COMPACT FORM.
C
      INTEGER           ZI(2)      ,MODULE(3),SYSBUF,SCRTCH
      INTEGER           TYPEI      ,TYPEP    ,TYPEU ,SIGNAB, SIGNC
      INTEGER           RD         ,RDREW    ,WRT   ,WRTREW, CLSREW,CLS
      INTEGER           FILEA      ,FILEB ,FILEC , FILED
      REAL              ZR(2)
      DOUBLE PRECISION  ZD(2)
      COMPLEX           ZC(2)
      DOUBLE COMPLEX    ZDC(2)
      INCLUDE           'MMACOM.COM'
      COMMON / NAMES  / RD         ,RDREW    ,WRT   ,WRTREW, CLSREW,CLS
      COMMON / TYPE   / IPRC(2)    ,NWORDS(4),IRC(4)
      COMMON / MPYADX / FILEA(7)   ,FILEB(7) ,FILEC(7)    
     1,                 FILED(7)   ,NZ       ,T     ,SIGNAB,SIGNC ,PREC1 
     2,                 SCRTCH     ,TIME
      COMMON / SYSTEM / KSYSTM(152)
      COMMON / UNPAKX / TYPEU      ,IUROW1   ,IUROWN, INCRU
      COMMON / PACKX  / TYPEI      ,TYPEP    ,IPROW1, IPROWN , INCRP
      EQUIVALENCE       (KSYSTM( 1),SYSBUF)  , (KSYSTM( 2),NOUT  ) 
     1,                 (KSYSTM(58),KSYS58)
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
      DATA    MODULE / 4HMMA2  , 4H     ,4H    /
      DATA    KZERO  / 1H0    /
      DATA    KONE   / 1H1    /
      DATA    JBEGN  / 4HBEGN/ , JEND  / 3HEND /
      IF ( NASTOR .EQ. 1 .OR. KSYS58 .EQ. 20 ) MODULE( 2 ) = KZERO
      IF ( NASTOR .EQ. 2 .OR. KSYS58 .EQ. 21 ) MODULE( 2 ) = KONE
      MODULE( 3 ) = JBEGN
      CALL CONMSG ( MODULE, 3, 0 )
      INCRU  = 1
      TYPEI  = NDTYPE
      TYPEP  = NDTYPE
      NWDD   = NWORDS( NDTYPE )
      IRFILE = FILEA( 1 )
C
C   OPEN CORE ALLOCATION AS FOLLOWS:
C     Z( 1        ) = ARRAY FOR ONE COLUMN OF "A" MATRIX
C     Z( IDX      ) = ARRAY FOR MULTIPLE COLUMNS OF "D" MATRIX
C     Z( IBX      ) = ARRAY FOR MULTIPLE COLUMNS OF "B" MATRIX
C        THROUGH
C     Z( LASMEM   )
C     Z( IBUF4    ) = BUFFER FOR "D" FILE
C     Z( IBUF3    ) = BUFFER FOR "C" FILE
C     Z( IBUF2    ) = BUFFER FOR "B" FILE 
C     Z( IBUF1    ) = BUFFER FOR "A" FILE
C     Z( NZ       ) = END OF OPEN CORE THAT IS AVAILABLE
C
      IDX    = 1 + NWDD*NAR  
      IF ( NASTOR .NE. 2 .AND. KSYS58 .NE. 21 ) GO TO 90
C
C REDEFINE IDX AND INSURE A QUAD WORD BOUNDARY FOR COMPLEX DOUBLE
C
      IDX = 1 + NWDD*NAR + NAR
      ITEST  = MOD ( IDX, 4 )
      IF ( ITEST .EQ. 1 ) GO TO 90
      IF ( ITEST .EQ. 0 ) IDX = IDX + 1
      IF ( ITEST .EQ. 2 ) IDX = IDX + 3
      IF ( ITEST .EQ. 3 ) IDX = IDX + 2
90    CONTINUE
      IDX2   = ( ( IDX+1 ) / 2 ) - 1
      IDX4   = ( IDX+1 ) / 4
      IBUF1  = NZ    - SYSBUF
      IBUF2  = IBUF1 - SYSBUF
      IF ( FILEC( 1 ) .EQ. 0 ) GO TO 100
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
      SIGN   = 1.0
      CALL GOPEN  ( FILEA, ZR( IBUF1 ), RDREW )
      CALL GOPEN  ( FILEB, ZR( IBUF2 ), RDREW )   
      IF ( FILEC( 1 ) .NE. 0 ) CALL GOPEN  ( FILEC, ZR( IBUF3 ), RDREW )   
      CALL GOPEN  ( FILED, ZR( IBUF4 ), WRTREW )   
      FILED( 2 ) = 0
      FILED( 6 ) = 0
      FILED( 7 ) = 0
C
C   DETERMINE HOW MANY COLUMNS OF "B" CAN BE READ INTO MEMORY AND HOW
C   MANY COLUMNS OF "D" CAN BE HELD IN MEMORY FOR ONE PASS
C
      IAVAIL = LASMEM - IDX + 1 
C
C   NCOLPP  -  NUMBER OF COLUMNS OF "B" THAT CAN BE READ IN ONE PASS
C   NPASS   -  NUMBER OF PASSES NEEDED TO READ ENTIRE "B" MATRIX
C      
      NWDDNDR = NWDD * NDR
      NWDDNBR = NWDD * NBR
      NCOLPP  = IAVAIL / ( 2+ NWDDNBR + NWDDNDR )
      IF ( NCOLPP .LE. 0 ) 
     &     CALL MESAGE ( -8, IAVAIL+NWDDNBR+NWDDNDR, MODULE)
      IF ( NCOLPP .GT. NBC ) NCOLPP = NBC
      NPASS   = ( (NBC-1) / NCOLPP ) + 1
      IBX     = IDX + NCOLPP*NWDDNDR
      DO 70000 M = 1, NPASS
      IPASS   = M
      IF ( M .EQ. NPASS ) NCOLPP = NBC - ( NCOLPP*(NPASS-1) )
      CALL REWIND ( FILEA )
      CALL SKPREC ( FILEA, 1 )
      INDXB  = IBX
      INDXD  = IDX
      TYPEU  = NDTYPE * SIGNAB  
      DO 600 I = 1, NCOLPP
      IUROW1 = -1
      CALL UNPACK ( *500, FILEB, ZR( INDXB+2 ) )
      ZI( INDXB   ) = IUROW1
      ZI( INDXB+1 ) = IUROWN
      GO TO 550
500   CONTINUE
C NULL COLUMN READ ON "B"
      ZI( INDXB   ) = 0
      ZI( INDXB+1 ) = 0
550   CONTINUE
      INDXB   = INDXB + NWDDNBR + 2
600   CONTINUE
      IF ( FILEC( 1 ) .EQ. 0 .OR. SIGNC .EQ. 0 ) GO TO 800
      TYPEU   = NDTYPE * SIGNC  
      IUROW1 = 1
      IUROWN = NCR
      DO 700 I = 1, NCOLPP
      CALL UNPACK ( *650, FILEC, ZR( INDXD ) )
      GO TO 680
C
C NULL COLUMN READ ON "C" 
C
650   CONTINUE
      LEN     = INDXD + NWDDNDR - 1
      DO 620 K = INDXD, LEN
      ZR( K ) = 0.0
620   CONTINUE
680   CONTINUE
      INDXD   = INDXD + NWDDNDR  
700   CONTINUE
      GO TO 900
C
C "C" MATRIX IS NULL OR "SIGNC" IS ZERO
C
800   CONTINUE
      LEN = IDX + NCOLPP*NWDDNDR - 1
      DO 850 K = IDX, LEN
      ZR( K ) = 0.
850   CONTINUE
900   CONTINUE
C
C PROCESS ALL OF THE COLUMNS OF "A" 
C      
      IF ( KSYS58 .EQ. 21 ) GO TO 1000
      IF ( KSYS58 .EQ. 20 ) GO TO 950
      IF ( NASTOR .EQ. 2  ) GO TO 1000
950   CONTINUE
      IF ( NDTYPE .EQ. 1 ) CALL MMA201 ( ZI, ZR )        
      IF ( NDTYPE .EQ. 2 ) CALL MMA202 ( ZI, ZD )   
      IF ( NDTYPE .EQ. 3 ) CALL MMA203 ( ZI, ZC )
      IF ( NDTYPE .EQ. 4 ) CALL MMA204 ( ZI, ZD, ZDC )
      GO TO 70000
1000  CONTINUE
      IF ( NDTYPE .EQ. 1 ) CALL MMA211 ( ZI, ZR )        
      IF ( NDTYPE .EQ. 2 ) CALL MMA212 ( ZI, ZD )   
      IF ( NDTYPE .EQ. 3 ) CALL MMA213 ( ZI, ZC )
      IF ( NDTYPE .EQ. 4 ) CALL MMA214 ( ZI, ZD, ZDC )
70000 CONTINUE
      CALL CLOSE ( FILEA, CLSREW )
      CALL CLOSE ( FILEB, CLSREW )
      CALL CLOSE ( FILEC, CLSREW )
      CALL CLOSE ( FILED, CLSREW )
      MODULE( 3 ) = JEND
      CALL CONMSG ( MODULE, 3, 0 )
      RETURN
      END

