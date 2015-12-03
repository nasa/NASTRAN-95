      SUBROUTINE GMMERG(FILEA,FILE11,FILE21,FILE12,FILE22,RPART,CPART,
     1          NSUB,MRGTYP,CORE,LCORE)
C
C     GENERAL MATRIX MERGE ROUTINE
C
C
C                  --               --
C                  I        I        I
C                  I FILE11 I FILE12 I   --     --
C                  I        I        I   I       I
C                  I-----------------I = I FILEA I
C                  I        I        I   I       I
C                  I FILE21 I FILE22 I   --     --
C                  I        I        I
C                  --               --
C
C        WHERE
C
C             RPART - ROW PARTITIONING VECTOR
C             NSUB(1) - NUMBER OF COLUMNS IN RPART 0 SUBSET
C             NSUB(2) - NUMBER OF COLUMNS IN RPART 1 SUBSET
C             NSUB(3) - NUMBER OF ROWS IN CPART 0 SUBSET
C             NSUB(4) - NUMBER OF ROWS IN CPART 1 SUBSET
C             MRGTYP - MERGE TYPE (1 .EQ. SQUARE, 2 .EQ. RECTANGULAR)
C             CPART - COLUMN PARTITION VECTOR
C
C
      INTEGER       FILEA    ,FILE11   ,FILE12   ,FILE21   ,FILE22
     1             ,RPART    ,CPART    ,RULE     ,CORE(6)  ,NAME(2)
     2             ,RP(7)    ,CP(7)
C
      COMMON / PARMEG /       IA(7)    ,IA11(7)  ,IA21(7)  ,IA12(7)
     1                       ,IA22(7)  ,LCR      ,RULE
C
      DIMENSION NSUB(4)
C
      DATA NAME / 4HGMME , 4HRG   /
C
C***********************************************************************
C
C     GET TRAILERS FOR INPUTS
C
      RP(1) = RPART
      IF(RPART .NE. 0) CALL RDTRL(RP)
      CP(1) = CPART
      IF(CPART .NE. 0) CALL RDTRL(CP)
C
      DO 10 I=2,7
      IA(I) = 0
      IA11(I) = 0
      IA12(I) = 0
      IA21(I) = 0
   10 IA22(I) = 0
C
      IA11(1) = FILE11
      IF(FILE11 .NE. 0) CALL RDTRL(IA11)
      IF(IA11(1) .LT. 0) IA11(1) = 0
      IA12(1) = FILE12
      IF(FILE12 .NE. 0) CALL RDTRL(IA12)
      IF(IA12(1) .LT. 0) IA12(1) = 0
      IA21(1) = FILE21
      IF(FILE21 .NE. 0) CALL RDTRL(IA21)
      IF(IA21(1) .LT. 0) IA21(1) = 0
      IA22(1) = FILE22
      IF(FILE22 .NE. 0) CALL RDTRL(IA22)
      IF(IA22(1) .LT. 0) IA22(1) = 0
C
C     SET UP MATRIX CONTROL BLOCK FOR OUTPUT
C
      IA(1) = FILEA
      IA(4) = MRGTYP
      IA(5) = MAX0(IA11(5),IA12(5),IA21(5),IA22(5))
C
C     SET UP DUMMY PARTITION VECTOR
C
      CORE(1) = 0
      CORE(2) = 1
      CORE(3) = IA(2)
      CORE(4) = 2
      CORE(5) = 1
      CORE(6) = 0
      LCR = LCORE
      RULE = 0
C
      IF(RPART .EQ. 0) GO TO 30
      IF(CPART .EQ. 0) GO TO 20
C
C     FULL MERGE
C
      IA(2) = NSUB(1) + NSUB(2)
      IA(3) = NSUB(3) + NSUB(4)
      CALL MERGE(RP,CP,CORE)
      GO TO 40
C
C  *  *  MERGE COLUMNS ONLY
C
   20 IA(2) = NSUB(1) + NSUB(2)
      IA(3) = MAX0(IA11(3),IA12(3))
      CALL MERGE(RP,CORE,CORE)
      GO TO 40
C
C  *  *  MERGE ROWS ONLY
C
   30 IF(CPART .EQ. 0) GO TO 1007
      IA(2) = MAX0(IA11(2),IA21(2))
      IA(3) = NSUB(3) + NSUB(4)
      CALL MERGE(CORE,CP,CORE)
C
C     WRITE TRIALER FOR OUTPUT
C
   40 CALL WRTTRL(IA)
C
      RETURN
C
C     ILLEGAL INPUT - NO PARTITION VECTOR
C
 1007 CALL MESAGE(-7,0,NAME)
      RETURN
      END
