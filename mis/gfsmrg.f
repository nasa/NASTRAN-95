      SUBROUTINE GFSMRG (FILEA,FILE11,FILE21,FILE12,FILE22,RPART,CPART)
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
C             CPART - COLUMN PARTITION VECTOR
C
C
      INTEGER       FILEA    ,FILE11   ,FILE12   ,FILE21   ,FILE22  ,
     1              RPART    ,CPART    ,RULE     ,CORE     ,NAME(2) ,
     2              RP(7)    ,CP(7)
C
C     OPEN CORE
C
      COMMON / ZZZZZZ /       CORE(1)
C
C     CALCV COMMON BLOCK
C
      COMMON / PATX   /       LCORE    ,NSUB0    ,NSUB1
C
C     PARTITION - MERGE COMMON BLOCK
C
      COMMON / PARMEG /       IA(7)    ,IA11(7)  ,IA21(7)  ,IA12(7) ,
     1                        IA22(7)  ,LCR      ,RULE
C
      DATA     NAME   /       4HGFSM   , 4HRG    /
C
C
C     GET TRAILERS FOR INPUTS
C
      RP(1) = RPART
      IF (RPART .NE. 0) CALL RDTRL (RP)
      CP(1) = CPART
      IF (CPART .NE. 0) CALL RDTRL (CP)
C
      DO 10 I = 2,7
      IA(I)   = 0
      IA11(I) = 0
      IA12(I) = 0
      IA21(I) = 0
   10 IA22(I) = 0
C
      IA11(1) = FILE11
      IF (FILE11  .NE. 0) CALL RDTRL (IA11)
      IF (IA11(1) .LT. 0) IA11(1) = 0
      IA12(1) = FILE12
      IF (FILE12  .NE. 0) CALL RDTRL (IA12)
      IF (IA12(1) .LT. 0) IA12(1) = 0
      IA21(1) = FILE21
      IF (FILE21  .NE. 0) CALL RDTRL (IA21)
      IF (IA21(1) .LT. 0) IA21(1) = 0
      IA22(1) = FILE22
      IF (FILE22  .NE. 0) CALL RDTRL (IA22)
      IF (IA22(1) .LT. 0) IA22(1) = 0
C
C     SET UP MATRIX CONTROL BLOCK FOR OUTPUT
C
      IA(1) = FILEA
      IA(4) = 2
      IF (RPART.NE.0 .AND. CPART.NE.0) IA(4) = 1
      IA(5) = MAX0(IA11(5),IA12(5),IA21(5),IA22(5))
C
C     SET UP DUMMY PARTITION VECTOR
C
      I = 0
      CORE(  1) = 0
      CORE(I+2) = 1
      CORE(I+3) = IA(2)
      CORE(I+4) = 2
      CORE(I+5) = 1
      CORE(I+6) = 0
C
      RULE = 0
      LCR = KORSZ(CORE)
C
      IF (RPART .EQ. 0) GO TO 30
      IF (CPART .EQ. 0) GO TO 20
C
C     FULL MERGE
C
      IA(2) = NSUB0 + NSUB1
      IA(3) = IA(2)
      CALL MERGE (RP,CP,CORE)
      GO TO 40
C
C     ROW MERGE
C
   20 IA(2) = NSUB0 + NSUB1
      IA(3) = MAX0(IA11(3),IA12(3))
      CALL MERGE (RP,CORE,CORE)
      GO TO 40
C
C     COLUMN MERGE
C
   30 IF (CPART .EQ. 0) GO TO 50
      IA(2) = MAX0(IA11(2),IA21(2))
      IA(3) = NSUB0 + NSUB1
      CALL MERGE (CORE,CP,CORE)
C
C     WRITE TRIALER FOR OUTPUT
C
   40 CALL WRTTRL (IA)
C
      RETURN
C
C     ILLEGAL INPUT - NO PARTITION VECTOR
C
   50 CALL MESAGE (-7,0,NAME)
      RETURN
      END
