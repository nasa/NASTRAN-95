      SUBROUTINE MMA411 ( ZI, ZR )
C
C     MMA411 PERFORMS THE MATRIX OPERATION USING METHOD 41 
C       IN REAL SINGLE PRECISION
C       (+/-)A(T & NT) * B (+/-)C = D
C     
C     MMA411 USES METHOD 41 WHICH IS AS FOLLOWS:
C       1.  THIS IS FOR "A" NON-TRANSPOSED AND TRANSPOSED
C       2.  READ AS MANY COLUMNS OF "B" INTO MEMORY AS POSSIBLE
C           INTO MEMORY IN COMPACT FORM LEAVING SPACE FOR A FULL
C           COLUMN OF "D" FOR EVERY COLUMN "B" READ.  SEE SUBROUTINES
C           MMARM1,2,3,4 FOR FORMAT OF COMPACT FORM.
C       3.  INITIALIZE EACH COLUMN OF "D" WITH THE DATA FROM "C".
C       4.  CALL UNPACK TO READ MATRICES "C".
C       5.  CALL MMARC1,2,3,4 TO READ COLUMNS OF MATRIX "A".
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
      EQUIVALENCE       (KSYSTM( 1),SYSBUF)  , (KSYSTM( 2),IWR   ) 
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
C     Z( IBX      ) = ARRAY FOR MULTIPLE COLUMNS OF "B" MATRIX
C                     (STORED IN COMPACT FORM)
C     Z( IDX      ) = ARRAY FOR MULTIPLE COLUMNS OF "D" MATRIX
C                     (FULL COLUMN SPACE ALLOCATION)
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
      IRFILE = FILEA( 1 )
      SIGN   = 1
      DO 60000 II = 1, NAC
C      print *,' processing column of a, ii=',ii
C      
C READ A COLUMN FROM THE "A" MATRIX
C
      CALL MMARC1 ( ZI, ZR )
C      
C CHECK FOR NULL COLUMN FROM THE "A" MATRIX
C
      IF ( ZI ( 1 ) .EQ. 0 ) GO TO 60000
      IF ( T .NE. 0 ) GO TO 5000
C      
C "A" NON-TRANSPOSE CASE    ( A*B + C )      
C
C SINGLE PRECISION
1000  CONTINUE
      INDXB  = IBX
      DO 1500 I = 1, NCOLPP
      ICOLB  = IBROW + I
      IF ( ICOLB .NE. IABS( ZI( INDXB ) ) ) GO TO 70001
      INDXBL = ZI( INDXB+1 ) + IBX - 1
      INDXB  = INDXB + 2
      INDXD  = ( IDX + ( I-1 )*NDR ) - 1    
1100  CONTINUE
      IF ( INDXB .GE. INDXBL ) GO TO 1450
      IROWB1 = ZI( INDXB   )
      IROWS  = ZI( INDXB+1 )
      IROWBN = IROWB1 + IROWS - 1
      IF ( II .GT. IROWBN ) GO TO 1410
      IF ( II .LT. IROWB1 ) GO TO 1450
      INDXBV = INDXB + 2 + II - IROWB1
      IF ( ZR( INDXBV ) .EQ. 0.0 ) GO TO 1450
      INDXA  = 1
1200  CONTINUE
      IROWA1 = ZI( INDXA   )
      NTMS   = ZI( INDXA+1 )
      IROWAN = IROWA1 + NTMS - 1
      INDXAV = INDXA + 2 - IROWA1
      DO 1400 K = IROWA1, IROWAN
      ZR( INDXD+K ) = ZR( INDXD+K ) + ZR( INDXAV+K ) * ZR( INDXBV )
1400  CONTINUE
      INDXA  = INDXA + 2 + NTMS
      IF ( INDXA .LT. LASIND ) GO TO 1200
      GO TO 1450
1410  CONTINUE
      INDXB  = INDXB + 2 + IROWS
      GO TO 1100
1450  INDXB  = INDXBL
1500  CONTINUE
      GO TO 60000
C
C  TRANSPOSE CASE ( A(T) * B + C )
C
5000  CONTINUE      
C SINGLE PRECISION
10000 CONTINUE
      INDXB  = IBX
      DO 15000 I = 1, NCOLPP
      INDXA  = 1
      ICOLB  = IBROW + I 
      IF ( ICOLB .NE. IABS( ZI( INDXB ) ) ) GO TO 70001
      INDXBL = ZI( INDXB+1 ) + IBX - 1
      INDXB  = INDXB + 2
      INDXD  = ( IDX + ( I-1 )*NDR ) + II - 1 
11000 CONTINUE
      IF ( INDXB .GE. INDXBL ) GO TO 14500
      IROWB1 = ZI( INDXB )
      IROWS  = ZI( INDXB+1 )
      IROWBN = IROWB1 + IROWS - 1
      INDXBV = INDXB + 2 - IROWB1    
      INDXB  = INDXB + 2 + IROWS
12000 CONTINUE
      IROWA1 = ZI( INDXA   )
      NTMS   = ZI( INDXA+1 )
      IROWAN = IROWA1 + NTMS - 1
      IF ( IROWBN .LT. IROWA1 ) GO TO 11000
      IF ( IROWAN .LT. IROWB1 ) GO TO 14200
      IROW1  = MAX0( IROWA1, IROWB1 )
      IROWN  = MIN0( IROWAN, IROWBN )
      IF ( IROWN .LT. IROW1 ) GO TO 11000
      INDXAV = INDXA + 2 - IROWA1
      DO 14000 K = IROW1, IROWN
      ZR( INDXD ) = ZR( INDXD ) + ZR( INDXAV+K ) * ZR( INDXBV+K )
14000 CONTINUE
      IF ( IROWAN .GT. IROWBN ) GO TO 11000
14200 CONTINUE
      INDXA  = INDXA + 2 + NTMS
      IF ( INDXA .GE. LASIND ) GO TO 14500
      GO TO 12000
14500 CONTINUE
      INDXB  = INDXBL
15000 CONTINUE
C END OF PROCESSING THIS COLUMN OF "A" FOR THIS PASS
60000 CONTINUE
C  NOW SAVE COLUMNS COMPLETED 
      DO 65000 K = 1, NCOLPP
      INDX = IDX + ( K-1 ) * NDR
      CALL PACK ( ZR( INDX ), FILED, FILED )
65000 CONTINUE
      GO TO 70000
70001 WRITE ( IWR, 90001 ) ICOLB, ZI( INDXB ), IBX, INDXB
90001 FORMAT(' UNEXPECTED COLUMN FOUND IN PROCESSING MATRIX B'
     &,/,' COLUMN EXPECTED:',I6
     &,/,' COLUMN FOUND   :',I6
     &,/,' IBX =',I7,'  INDXB =',I7 )
      CALL MESAGE ( -61, 0, 0 )
70000 RETURN
      END

