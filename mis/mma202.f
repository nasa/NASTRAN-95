      SUBROUTINE MMA202 ( ZI, ZD )
C
C     MMA202 PERFORMS THE MATRIX OPERATION USING METHOD 20 IN
C       REAL DOUBLE PRECISION
C
C       (+/-)A(T & NT) * B (+/-)C = D
C     
C     MMA202 USES METHOD 20 WHICH IS AS FOLLOWS:
C       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
C       2.  UNPACK AS MANY COLUMNS OF "B" INTO MEMORY AS POSSIBLE
C           LEAVING SPACE FOR A COLUMN OF "D" FOR EVERY COLUMN "B" READ.
C       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
C       4.  USE UNPACK TO READ MATRICES "B" AND "C".
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
      DOUBLE PRECISION  ZD(2)
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
      IUROW1 = -1
      TYPEU  = NDTYPE  
      CALL UNPACK ( *50000, FILEA, ZD( 1 ) )
      IROWA1 = IUROW1
      IROWAN = IUROWN
      INDXA  = 1 - IROWA1      
      IF ( T .NE. 0 ) GO TO 5000
C      
C "A" NON-TRANSPOSE CASE    ( A*B + C )      
C
C DOUBLE PRECISION
2000  CONTINUE
      DO 2500 I = 1, NCOLPP
      INDXB  = IBX + 2*I + ( I-1 )*NWDDNBR
      IROWB1 = ZI( INDXB-2 )
      IROWBN = ZI( INDXB-1 )
      IF ( II .LT. IROWB1 .OR. II .GT. IROWBN ) GO TO 2500
      INDXB  = ( ( INDXB+1 ) / 2 ) + II - IROWB1 
      IF ( ZD( INDXB ) .EQ. 0.0D0 ) GO TO 2500
      INDXD  = IDX + ( I-1 )*NWDDNDR
      INDXD  = ( ( INDXD+1 ) / 2 ) - 1
      DO 2400 K = IROWA1, IROWAN
      ZD( INDXD+K ) = ZD( INDXD+K ) +  ZD( INDXA+K ) * ZD( INDXB )
2400  CONTINUE
2500  CONTINUE
      GO TO 50000
C
C  TRANSPOSE CASE ( A(T) * B + C )
C
5000  CONTINUE      
C DOUBLE PRECISION
20000 CONTINUE
      DO 25000 I = 1, NCOLPP
      INDXB  = IBX + 2*I + ( I-1 )*NWDDNBR
      IROWB1 = ZI( INDXB-2 )
      IF ( IROWB1 .EQ. 0 ) GO TO 25000
      IROWBN = ZI( INDXB-1 )
      IROW1  = MAX0( IROWA1, IROWB1 )
      IROWN  = MIN0( IROWAN, IROWBN )
      IF ( IROWN .LT. IROW1 ) GO TO 25000
      INDXB  = ( ( INDXB+1 ) / 2 ) - IROWB1
      INDXD  = IDX + ( I-1 )*NWDDNDR
      INDXD  = ( ( INDXD+1 ) / 2 ) - 1 + II
      DO 24000 K = IROW1, IROWN
      ZD( INDXD ) = ZD( INDXD ) +  ZD( INDXA+K ) * ZD( INDXB+K )
24000 CONTINUE
25000 CONTINUE
      GO TO 50000
C END OF PROCESSING THIS COLUMN OF "A" FOR THIS PASS
50000 CONTINUE
60000 CONTINUE
C  NOW SAVE COLUMNS COMPLETED 
      DO 65000 K = 1, NCOLPP
      INDX = IDX2 + ( K-1 ) * NDR
      CALL PACK ( ZD( INDX+1 ), FILED, FILED )
65000 CONTINUE
      RETURN
      END

