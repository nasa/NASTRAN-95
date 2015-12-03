      SUBROUTINE MMA213 ( ZI, ZC )
C
C     MMA211 PERFORMS THE MATRIX OPERATION USING METHOD 21 
C       IN COMPLEX SINGLE PRECISION.
C
C       (+/-)A(T & NT) * B (+/-)C = D
C     
C     MMA213 USES METHOD 21 WHICH IS AS FOLLOWS:
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
      COMPLEX           ZC(2)
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
C READ A COLUMN FROM THE "A" MATRIX
C
      CALL MMARC3 ( ZI, ZC )
C
C CHECK IF "A" COLUMN IS NULL
C
      IF ( ZI( 1 ) .EQ. 0 ) GO TO 60000
      IF ( T .NE. 0 ) GO TO 5000
C      
C "A" NON-TRANSPOSE CASE    ( A*B + C )      
C
C COMPLEX SINGLE PRECISION
3000  CONTINUE
      DO 3500 I = 1, NCOLPP
      INDX   = 1
      IROWA1 = ZI( INDX )
      INDXB  = IBX + 2*I + ( I-1 )*NWDDNBR 
      IROWB1 = ZI( INDXB-2 )
      IROWBN = ZI( INDXB-1 )
      IF ( II .LT. IROWB1 .OR. II .GT. IROWBN ) GO TO 3500
      INDXB  = ( ( INDXB+1 ) / 2 ) + II - IROWB1 
      IF ( ZC( INDXB ) .EQ. (0.0, 0.0) ) GO TO 3500
      INDXD  = IDX + ( I-1 )*NWDDNDR
      INDXD  = ( ( INDXD+1 ) / 2 ) - 1 
3100  CONTINUE
      IROWS  = ZI( INDX+1 )
      IROWAN = IROWA1 + IROWS - 1
      INDXA  = ( (INDX+1)/2 ) + 1 - IROWA1
      DO 3400 K = IROWA1, IROWAN
      ZC( INDXD+K ) = ZC( INDXD+K ) +  ZC( INDXA+K ) * ZC( INDXB )
3400  CONTINUE
      INDX   = INDX + 2 + IROWS*NWDD
      IF ( INDX .GE. LASIND ) GO TO 3500
      IROWA1 = ZI( INDX )
      GO TO 3100
3500  CONTINUE
      GO TO 60000
C
C  TRANSPOSE CASE ( A(T) * B + C )
C
5000  CONTINUE      
C COMPLEX SINGLE PRECISION
30000 CONTINUE
      DO 35000 I = 1, NCOLPP
      INDXB  = IBX + 2*I + ( I-1 )*NWDDNBR 
      IROWB1 = ZI( INDXB-2 )
      IF( IROWB1 .EQ. 0 ) GO TO 35000
      IROWBN = ZI( INDXB-1 )
      INDX   = 1
      IROWA1 = ZI( INDX )
      INDXB  = ( ( INDXB+1 ) / 2 ) - IROWB1
      INDXA  = 1 - IROWA1
      INDXD  = IDX + ( I-1 )*NWDDNDR
      INDXD  = ( ( INDXD+1 ) / 2 ) - 1 + II
31000 CONTINUE
      IROWS  = ZI( INDX+1 )
      IROWAN = IROWA1 + IROWS - 1
      IROW1  = MAX0( IROWA1, IROWB1 )
      IROWN  = MIN0( IROWAN, IROWBN )
      IF ( IROWN .LT. IROW1 ) GO TO 34100
      INDXA  = ( (INDX+1)/2 ) + 1 - IROWA1
      DO 34000 K = IROW1, IROWN
      ZC( INDXD ) = ZC( INDXD ) +  ZC( INDXA+K ) * ZC( INDXB+K )
34000 CONTINUE
34100 CONTINUE
      INDX   = INDX + 2 + IROWS*NWDD
      IF ( INDX .GE. LASIND ) GO TO 35000
      IROWA1 = ZI( INDX )
      GO TO 31000
35000 CONTINUE
C END OF PROCESSING THIS COLUMN OF "A" FOR THIS PASS
60000 CONTINUE
C  NOW SAVE COLUMNS COMPLETED 
      DO 65000 K = 1, NCOLPP
      INDX = IDX2 + ( K-1 ) * NDR
      CALL PACK ( ZC( INDX+1 ), FILED, FILED )
65000 CONTINUE
      RETURN
      END

