      SUBROUTINE GMPRTN(FILEA,FILE11,FILE21,FILE12,FILE22,RPART,CPART,
     1           NSUB0,NSUB1,CORE,LCORE)
C
C     GENERAL MATRIX PARTION ROUTINE
C
C
C                              --               --
C                              I        I        I
C                  --     --   I FILE11 I FILE12 I
C                  I       I   I        I        I
C                  I FILEA I = I-----------------I
C                  I       I   I        I        I
C                  --     --   I FILE21 I FILE22 I
C                              I        I        I
C                              --               --
C
C        WHERE
C
C             RPART - ROW PARTITIONING VECTOR
C             CPART - COLUMN PARTITION VECTOR
C
      INTEGER       FILEA    ,FILE11   ,FILE12   ,FILE21   ,FILE22
     1             ,RPART    ,CPART    ,RULE     ,CORE(6)  ,NAME(2)
     2             ,RP(7)    ,CP(7)
C
      COMMON / PARMEG /       IA(7)    ,IA11(7)  ,IA21(7)  ,IA12(7)
     1                       ,IA22(7)  ,LCR      ,RULE
C
      DATA NAME / 4HGMPR , 4HTN   /
C
C***********************************************************************
C
C     GET TRAILERS FOR INPUTS
C
      RP(1) = RPART
      IF(RPART .NE. 0) CALL RDTRL(RP)
      CP(1) = CPART
      IF(CPART .NE. 0) CALL RDTRL(CP)
      IA(1) = FILEA
      CALL RDTRL(IA)
C
C     SET UP MATRIX CONTROL BLOCKS FOR OUTPUTS
C
      IA11(1) = FILE11
      IA12(1) = FILE12
      IA21(1) = FILE21
      IA22(1) = FILE22
C
      DO 10 I=2,5
      IA11(I) = IA(I)
      IA12(I) = IA(I)
      IA21(I) = IA(I)
   10 IA22(I) = IA(I)
C
C     SET UP DUMMY PARTITION VECTOR
C
      CORE(1) = 0
      CORE(2) = 1
      CORE(3) = IA(2)
      CORE(4) = 2
      CORE(5) = 1
      CORE(6) = 0
C
      RULE = 0
      LCR = LCORE
C
      IF(RPART .EQ. 0) GO TO 30
      IF(CPART .EQ. 0) GO TO 20
C
C     FULL PARTITION
C
      IA11(3) = NSUB0
      IA12(3) = NSUB0
      IA21(3) = NSUB1
      IA22(3) = NSUB1
      CALL PARTN(RP,CP,CORE)
      GO TO 40
C
C  *  *  PARTITION COLUMNS ONLY
C
   20 CALL PARTN(RP,CORE,CORE)
      GO TO 40
C
C  *  *  PARTITION ROWS ONLY
C
   30 IF(CPART .EQ. 0) GO TO 1007
      IA11(3) = NSUB0
      IA12(3) = NSUB0
      IA21(3) = NSUB1
      IA22(3) = NSUB1
      CALL PARTN(CORE,CP,CORE)
C
C     WRITE TRAILERS FOR OUTPUTS
C
   40 IF(IA11(1) .NE. 0) CALL WRTTRL(IA11)
      IF(IA12(1) .NE. 0) CALL WRTTRL(IA12)
      IF(IA21(1) .NE. 0) CALL WRTTRL(IA21)
      IF(IA22(1) .NE. 0) CALL WRTTRL(IA22)
C
      RETURN
C
C     ILLEGAL INPUT - NO PARTITION VECTOR
C
 1007 CALL MESAGE(-7,0,NAME)
      RETURN
      END
