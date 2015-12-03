      SUBROUTINE MMA101 ( ZI, ZR )
C
C     MMA101 PERFORMS THE MATRIX OPERATION USING METHOD 10 AND 
C       REAL SINGLE PRECISION
C
C       (+/-)A(T & NT) * B (+/-)C = D
C     
C     MMA10 USES METHOD 10 WHICH IS AS FOLLOWS:
C       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
C       2.  UNPACK AS MANY COLUMNS OF "A" INTO MEMORY AS POSSIBLE
C           LEAVING SPACE FOR ONE COLUMN OF "B" AND "D".
C       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
C       4.  USE UNPACK TO READ MATRICES "B" AND "C".
C
C     MEMORY FOR EACH COLUMN OF "A" IS AS FOLLOWS:
C         Z(1)   = FIRST NON-ZERO ROW NUMBER FOR COLUMN
C         Z(2)   = LAST NON-ZERO ROW NUMBER FOR COLUMN
C         Z(3-N) = VALUES OF NON-ZERO ROWS
C
      INTEGER           ZI(2)      ,T
      INTEGER           TYPEI      ,TYPEP    ,TYPEU ,SIGNAB, SIGNC
      INTEGER           RD         ,RDREW    ,WRT   ,WRTREW, CLSREW,CLS
      INTEGER           OFILE      ,FILEA    ,FILEB ,FILEC , FILED
      REAL              ZR(2)
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
C PROCESS ALL OF THE COLUMNS OF "B", ADD "C" DATA ON FIRST PASS
      DO 60000 II = 1, NBC
      IUROW1 = -1
      TYPEU  = NDTYPE * SIGNAB 
      CALL UNPACK ( *930, FILEB, ZR( 1 ) )
      IROWB1 = IUROW1
      IROWBN = IUROWN
      GO TO 940
930   IROWB1 = 0
      IROWBN = 0
940   CONTINUE
      IF ( IFILE .EQ. 0 ) GO TO 950            
      IUROW1 = 1
      IUROWN = NDR
      TYPEU  = NDTYPE 
      IF ( IPASS .EQ. 1 ) TYPEU = NDTYPE * SIGNC
      CALL UNPACK (*950, IFILE, ZR( IDX ) )
      GO TO 980
950   CONTINUE
      DO 970 J = 1, NDR
      ZR( IDX+J-1 ) = 0
970   CONTINUE
980   CONTINUE
      NWDDNAR = NWDD*NAR
C
C CHECK IF COLUMN OF "B" IS NULL
C
      IF ( IROWB1 .EQ. 0 ) GO TO 50000
      IF ( T .NE. 0 ) GO TO 5000
C      
C "A" NON-TRANSPOSE CASE    ( A * B  +  C )      
C
C SINGLE PRECISION
1000  CONTINUE
      DO 1500 I = 1, NCOLPP
      IBROWI = IBROW+I
      IF ( IBROWI .LT. IROWB1 .OR. IBROWI .GT. IROWBN ) GO TO 1500
      IBROWI = IBROWI - IROWB1 + 1
      IF ( ZR( IBROWI ) .EQ. 0. ) GO TO 1500
      INDXA  = IAX + 2*I + ( I-1 )*NAR 
      IROWA1 = ZI( INDXA-2 )
      IF ( IROWA1 .EQ. 0 ) GO TO 1500
      IROWAN = ZI( INDXA-1 )
      INDXA  = INDXA - IROWA1
      DO 1400 K = IROWA1, IROWAN
      ZR( IDX+K-1 ) = ZR( IDX+K-1 ) +  ZR( INDXA+K ) * ZR( IBROWI )
1400  CONTINUE
1500  CONTINUE
      GO TO 50000
C
C  TRANSPOSE CASE ( A(T) * B + C )
C
5000  CONTINUE      
      IDROW = IBROW
C SINGLE PRECISION
10000 CONTINUE
      DO 15000 I = 1, NCOLPP
      INDXA  = IAX + 2*I + ( I-1 )*NAR 
      IROWA1 = ZI( INDXA-2 )
      IF ( IROWA1 .EQ. 0 ) GO TO 15000   
      IROWAN = ZI( INDXA-1 )
      IROW1  = MAX0( IROWA1, IROWB1 )
      IROWN  = MIN0( IROWAN, IROWBN )
      IF ( IROWN .LT. IROW1 ) GO TO 15000
      INDXA  = INDXA - IROWA1
      INDXB  = 1 - IROWB1
      IDXX   = IDX + IDROW - 1
      DO 14000 K = IROW1, IROWN
      ZR( IDXX+I ) = ZR( IDXX+I ) +  ZR( INDXA+K ) * ZR( INDXB+K )
14000 CONTINUE
15000 CONTINUE
      GO TO 50000
C END OF PROCESSING THIS COLUMN FOR THIS PASS
50000 CONTINUE
C  NOW SAVE COLUMN 
      CALL PACK ( ZR( IDX ), OFILE, FILED )
60000 CONTINUE
      RETURN
      END

