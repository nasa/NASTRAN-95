      SUBROUTINE MMA211 ( ZI, ZR )
C
C     MMA211 PERFORMS THE MATRIX OPERATION USING METHOD 21 
C       IN REAL SINGLE PRECISION
C       (+/-)A(T & NT) * B (+/-)C = D
C     
C     MMA211 USES METHOD 21 WHICH IS AS FOLLOWS:
C       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
C       2.  UNPACK AS MANY COLUMNS OF "B" INTO MEMORY AS POSSIBLE
C           LEAVING SPACE FOR A COLUMN OF "D" FOR EVERY COLUMN "B" READ.
C       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
C       4.  USE GETSTR TO READ MATRIX "A", (SEE SUBROUTINES MMARC1,2,3,4).
C       5.  USE UNPACK TO READ MATRIX "C".
C
C     MEMORY FOR EACH COLUMN OF "B" IS AS FOLLOWS:
C         Z(1)   = FIRST NON-ZERO ROW NUMBER FOR COLUMN
C         Z(2)   = LAST NON-ZERO ROW NUMBER FOR COLUMN
C         Z(3-N) = VALUES OF NON-ZERO ROWS
C
      INTEGER           ZI(2)      ,T
      INTEGER           TYPEI      ,TYPEP    ,TYPEU ,SIGNAB, SIGNC
      INTEGER           RD         ,RDREW    ,WRT   ,WRTREW, CLSREW,CLS
      INTEGER           FILEA      ,FILEB ,FILEC , FILED
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
C
C PROCESS ALL OF THE COLUMNS OF "A" 
C      
      DO 60000 II = 1, NAC
C
C READ A COLUMN OF "A" MATRIX
C
      CALL MMARC1 ( ZI, ZR )
C
C CHECK FOR NULL COLUMN ON "A"
C
      IF ( ZI( 1 ) .EQ. 0 ) GO TO 60000
      IF ( T .NE. 0 ) GO TO 5000
C      
C "A" NON-TRANSPOSE CASE    ( A*B + C )      
C
C SINGLE PRECISION
1000  CONTINUE
      DO 1500 I = 1, NCOLPP
      INDX   = 1
      IROWA1 = ZI( INDX )
      INDXB  = IBX + 2*I + ( I-1 )*NBR 
      IROWB1 = ZI( INDXB-2 )
      IROWBN = ZI( INDXB-1 )
      IF ( II .LT. IROWB1 .OR. II .GT. IROWBN ) GO TO 1500
      INDXB  = INDXB + II - IROWB1
      IF ( ZR( INDXB ) .EQ. 0.0 ) GO TO 1500
      INDXD  = ( IDX + ( I-1 )*NDR ) - 1    
1100  CONTINUE      
      IROWS  = ZI( INDX+1 )
      IROWAN = IROWA1 + IROWS - 1
      INDXA  = INDX + 2 - IROWA1
      DO 1400 K = IROWA1, IROWAN
      ZR( INDXD+K ) = ZR( INDXD+K ) + ZR( INDXA+K ) * ZR( INDXB )
1400  CONTINUE
      INDX = INDX + 2 + IROWS
      IF ( INDX .GE. LASIND ) GO TO 1500
      IROWA1 = ZI( INDX )
      GO TO 1100
1500  CONTINUE     
      GO TO 60000
C
C  TRANSPOSE CASE ( A(T) * B + C )
C
5000  CONTINUE      
C SINGLE PRECISION
10000 CONTINUE
      DO 15000 I = 1, NCOLPP
      INDXB  =   IBX + 2*I + ( I-1 )*NBR 
      IROWB1 = ZI( INDXB-2 )
      IF ( IROWB1 .EQ. 0 ) GO TO 15000
      IROWBN = ZI( INDXB-1 )
      INDX   = 1
      IROWA1 = ZI( INDX )
      INDXD  = ( IDX + ( I-1 )*NDR ) - 1 + II
      INDXB  = INDXB - IROWB1 
11000 CONTINUE
      IROWS  = ZI( INDX + 1 )
      IROWAN = IROWA1 + IROWS - 1
      IROW1  = MAX0( IROWA1, IROWB1 )
      IROWN  = MIN0( IROWAN, IROWBN )
      IF ( IROWN .LT. IROW1 ) GO TO 14100
      INDXA  = INDX + 2 - IROWA1
      DO 14000 K = IROW1, IROWN
      ZR( INDXD ) = ZR( INDXD ) + ZR( INDXA+K ) * ZR( INDXB+K )
14000 CONTINUE
14100 CONTINUE
      INDX = INDX + 2 + IROWS
      IF ( INDX .GE. LASIND ) GO TO 15000
      IROWA1 = ZI( INDX )
      GO TO 11000
15000 CONTINUE
C END OF PROCESSING THIS COLUMN OF "A" FOR THIS PASS
60000 CONTINUE
C  NOW SAVE COLUMNS COMPLETED 
      DO 65000 K = 1, NCOLPP
      INDX = IDX + ( K-1 ) * NWDDNDR
      CALL PACK ( ZR( INDX ), FILED, FILED )
65000 CONTINUE
      RETURN
      END

