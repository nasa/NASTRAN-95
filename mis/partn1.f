      SUBROUTINE PARTN1
C
C     THIS IS THE DMAP MODULE PARTN WHICH PARTITIONS A MATRIX -A- INTO
C     FOUR PARTITIONS, SOME OR ALL OF WHICH MAY BE PURGED.
C
C
C                             **                  **
C                             *       I            *
C                             *  A11  I    A12     *
C          **   **            *       I            *
C          *     *            * ------+----------- *
C          *  A  *  BECOMES   *       I            *
C          *     *            *       I            *
C          **   **            *  A21  I    A22     *
C                             *       I            *
C                             **                  **
C
C
C     BASED ON ROW PARTITION MATRIX -RP- AND COLUMN PARTITION MATRIX
C     -CP-
C
C     DMAP SEQUENCE.
C
C     PARTN A,CP,RP/A11,A21,A12,A22/V,Y,SYM/V,Y,TYPE/V,Y,F11/V,Y,F21/
C                                   V,Y,F12/V,Y,F22 $
C
      IMPLICIT INTEGER (A-Z)
      EXTERNAL        RSHIFT,ANDF
      LOGICAL         CPHERE,RPHERE,CPNULL,RPNULL
      DIMENSION       MCB(7,4),BLOCK(80),SUBR(2),AIJ(4),BUFFS(5),
     1                HEAD(2),MCBA(7),REFUS(3)
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25,SWM*27
      COMMON /XMSSG / UFM,UWM,UIM,SFM,SWM
      COMMON /MAHCIN/ MACHX
      COMMON /ZNTPKX/ ELEM(4),ROW,EOL
      COMMON /NAMES / RD,RDREW,WRT,WRTREW,CLSREW,CLS
      COMMON /SYSTEM/ SYSBUF,OUTPT,XXX(37),NBPW
      COMMON /PRTMRG/ CPSIZE,RPSIZE,CPONES,RPONES,CPNULL,RPNULL,CPHERE,
     1                RPHERE,ICP,NCP,IRP,NRP
      COMMON /ZZZZZZ/ Z(1)
      COMMON /BLANK / SYM,TYPE,FORM(4),CPCOL,RPCOL,IREQCL
      DATA    SUBR  / 4HPART,4HN1    /, A,CP,RP /101,102,103/,
     1        AIJ   / 201,202,203,204/
      DATA    NAFORM/ 4HFORM /, NATYPE/ 4HTYPE /, REFUS/2*3H   ,3HREF /
      DATA    EOR   / 1      /
C
      CORE = KORSZ(Z)
      BUFFS(1) = CORE - SYSBUF - 2
      DO 10 I = 2,5
      BUFFS(I) = BUFFS(I-1) - SYSBUF - 2
   10 CONTINUE
      CORE = BUFFS(5) - 1
      IF (CORE .LT. 10) CALL MESAGE (-8,0,SUBR)
C
C     OPEN MATRIX TO BE PARTITIONED.  IF PURGED RETURN IS MADE
C
      BUFF = BUFFS(5)
      CALL OPEN (*425,A,Z(BUFF),RDREW)
      CALL SKPREC (A,1)
      MCBA(1) = A
      CALL RDTRL (MCBA)
      INFORM = MCBA(4)
C
C     CALL TO PARTN2 WILL PROCESS -CP- AND -RP- INTO BIT STRINGS AND
C     DETERMINE SIZES OF THE PARTITIONS.
C
      BUFF = BUFFS(4)
      CALL PARTN2 (CP,RP,CORE,Z(BUFF))
C
C     IF RPSIZE OR CPSIZE ARE 0 THEY ARE SET EQUAL TO THE RESPECTIVE
C     SIZE OF A
C
      IF (CPSIZE .EQ. 0) CPSIZE = MCBA(2)
      IF (RPSIZE .EQ. 0) RPSIZE = MCBA(3)
C
C     MATRIX COMPATIBILITY CHECKS
C
      IF (RPSIZE.EQ.MCBA(3) .AND. CPSIZE.EQ.MCBA(2)) GO TO 40
      WRITE  (OUTPT,30) SWM,MCBA(3),MCBA(2),RPSIZE,CPSIZE
   30 FORMAT (A27,' 2166, MATRIX TO BE PARTITIONED IS OF SIZE',I10,
     1       ' ROWS BY',I10,' COLUMNS.', /5X,'ROW PARTITION SIZE IS',
     2       I10,' COLUMN PARTITION SIZE IS',I10,' (INCOMPATIBLE).')
C
C     PREPARE OUTPUT DATA BLOCKS AS REQUIRED.
C
   40 CPZERO = MCBA(2) - CPONES
      RPZERO = MCBA(3) - RPONES
C
C     CHECK OF TYPE PARAMETER
C
      NTYPE = MCBA(5)
      IF (NTYPE .EQ. TYPE) GO TO 60
      IF (TYPE  .EQ.    0) GO TO 54
      IF (TYPE.LT.0 .OR. TYPE.GT.4) GO TO 52
      WRITE  (OUTPT,50) SWM,NATYPE,TYPE,REFUS(1),SUBR,NTYPE
   50 FORMAT (A27,' 2163, REQUESTED VALUE OF ',A4,I10,2X,A3,
     1        'USED BY ',2A4,'. LOGICAL CHOICE IS',I10)
      NTYPE = TYPE
      GO TO 60
   52 WRITE (OUTPT,50) SWM,NATYPE,TYPE,REFUS(3),SUBR,NTYPE
   54 TYPE = NTYPE
C
   60 DO 140 I = 1,4
      FILE = AIJ(I)
      MCB(1,I) = 0
      COLS = CPZERO
      ROWS = RPZERO
      IF (I.EQ.3 .OR. I.EQ.4) COLS = CPONES
      IF (I.EQ.2 .OR. I.EQ.4) ROWS = RPONES
C
C     IF ROWS OR COLS EQUAL ZERO NOTHING IS WRITTEN ON THIS PARTITION
C
      IF (ROWS.EQ.0 .OR. COLS.EQ.0) GO TO 140
      BUFF = BUFFS(I)
      CALL OPEN (*140,FILE,Z(BUFF),WRTREW)
      CALL FNAME (FILE,HEAD)
      CALL WRITE (FILE,HEAD,2,EOR)
C
C     CHECK OF THE FORM PARAMETER
C
      NFORM = FORM(I)
      IF (NFORM.LT.1 .OR. NFORM.GT.8) GO TO 110
      GO TO (70,130,100,70,70,70,100,70), NFORM
C
C     FORM IMPLIES SQUARE
C
   70 IF (ROWS .EQ. COLS) GO TO 130
   80 WRITE  (OUTPT,90) SWM,HEAD,NFORM,ROWS,COLS
   90 FORMAT (A27,' 2168, THE FORM PARAMETER AS GIVEN TO THE PARTITION',
     1        'ING MODULE FOR SUB-PARTITION ',2A4, /5X,'IS INCONSISTANT'
     2,       ' WITH ITS SIZE.  FORM =',I9,' SIZE =',I9,' ROWS BY',I9,
     3        ' COLUMNS.')
      GO TO 130
C
C     DIAGONAL OR ROW MATRIX
C
  100 IF (COLS .EQ. 1) GO TO 130
      GO TO 80
C
C     NO FORM SPECIFIED THUS IT IS SQUARE IF ROWS = COLS OR RECTANGULAR
C     OTHERWISE.
C
  110 NFORM = 2
      IF (ROWS .EQ. COLS) NFORM = 1
      IF (SYM.LT.0 .AND. INFORM.EQ.6 .AND. NFORM.EQ.1 .AND.
     1   (I.EQ.1 .OR. I.EQ.4)) NFORM = 6
      IF (FORM(I) .EQ. 0) GO TO 128
      JJ = 1
      IF (FORM(I).LT.1 .OR. FORM(I).GT.8) JJ = 3
      WRITE (OUTPT,50) SWM,NAFORM,FORM(I),REFUS(JJ),SUBR,NFORM
      IF (JJ .NE. 3) NFORM = FORM(I)
  128 FORM(I) = NFORM
C
C     TRAILER INITIALIZATION.  BLDPKN WILL SET MCB(2) AND MCB(6) LATER.
C
  130 CALL MAKMCB (MCB(1,I),FILE,ROWS,NFORM,NTYPE)
  140 CONTINUE
C
C     ROW PARTITIONING BIT STRING IS AT THIS POINT CONVERTED TO A CORE
C     VECTOR ONE WORD PER BIT.  EACH WORD CONTAINS THE ROW NUMBER OF THE
C     PARTITION TO WHICH THE ELEMENT OF -A- IS TO BE MOVED TO.  IF THE
C     NUMBER IS NEGATIVE THE ELEMENT IS MOVED TO THE LOWER PARTITIONS
C     AND IF THE NUMBER IS POSITIVE THE ELEMENT IS MOVED TO THE UPPER
C     PARTITION
C
      IZ = NRP + 1
      NZ = IZ  + RPSIZE - 1
      IF (NZ+NBPW .GT. CORE) CALL MESAGE (-8,0,SUBR)
      IF (.NOT.RPNULL .AND. RPONES.NE.0) GO TO 160
      K  = 0
      DO 150 I = IZ,NZ
      K  = K + 1
      Z(I) = K
  150 CONTINUE
      GO TO 210
  160 JZ   = IZ - 1
      ZERO = 0
      ONES = 0
C
C     NOTE THIS LOGIC WORKS ON CRAY WITH 48 OF 64 BIT INTEGER WORD
C
      DO 200 I = IRP,NRP
      DO 190 J = 1,NBPW
      SHIFT = NBPW - J
      BIT   = RSHIFT(Z(I),SHIFT)
      JZ    = JZ + 1
      IF (ANDF(BIT,1)) 180,170,180
  170 ZERO  = ZERO + 1
      Z(JZ) = ZERO
      GO TO 190
  180 ONES  = ONES - 1
      Z(JZ) = ONES
  190 CONTINUE
  200 CONTINUE
C
C     LOOP ON ALL THE COLUMNS OF -A-.
C
  210 IZM1  = IZ - 1
      DO 400 I = 1,CPSIZE
      IF (CPNULL) GO TO 220
      IL1   = I - 1
      BITWD = IL1/NBPW + ICP
      SHIFT = NBPW - MOD(IL1,NBPW) - 1
      BIT   = RSHIFT(Z(BITWD),SHIFT)
      IF (ANDF(BIT,1)) 230,220,230
C
C     ZERO-S COLUMN (LEFT PARTITIONS A11 AND A21)
C
  220 IFILE  = 1
      IBLOCK = 1
      GO TO 240
C
C     ONE-S COLUMN (RIGHT PARTITIONS A12 AND A22)
C
  230 IFILE  = 3
      IBLOCK = 41
      GO TO 240
C
C     START COLUMNS OF THE 2 AIJ PARTITIONS.
C
  240 KFILE  = IFILE
      KBLOCK = IBLOCK
      M = 0
      DO 270 J = 1,2
      IF (MCB(1,KFILE)) 260,260,250
  250 CALL BLDPK (NTYPE,MCB(5,KFILE),MCB(1,KFILE),BLOCK(KBLOCK),1)
      M = 1
  260 KFILE  = KFILE + 1
      KBLOCK = KBLOCK + 20
  270 CONTINUE
      IF (M) 280,390,280
C
C     START THE I-TH COLUMN OF THE MATRIX BEING PARTITIONED -A-.
C
  280 CALL INTPK (*350,A,0,NTYPE,0)
C
C     LOOP ON NON-ZEROS OF THE COLUMN
C
  290 IF (EOL) 300,300,350
C
C     PICK UP A NON-ZERO ELEMENT
C
  300 CALL ZNTPKI
C
C     DETERMINE ROW POSITION AND FILE DESTINATION.
C
      L = IZM1 + ROW
      IF (Z(L)) 320,310,310
C
C     ZERO-S ROW PARTITION.
C
  310 JROW   = Z(L)
      KFILE  = IFILE
      KBLOCK = IBLOCK
      GO TO 330
C
C     ONE-S ROW PARTITION.
C
  320 JROW   = -Z(L)
      KFILE  = IFILE  + 1
      KBLOCK = IBLOCK + 20
C
C     OUTPUT THE ELEMENT.
C
  330 IF (MCB(1,KFILE)) 290,290,340
  340 CALL BLDPKI (ELEM,JROW,MCB(1,KFILE),BLOCK(KBLOCK))
      GO TO 290
C
C     COMPLETE COLUMNS OF THE 2 AIJ PARTITIONS BEING WORKED ON.
C
  350 KFILE  = IFILE
      KBLOCK = IBLOCK
      DO 380 J = 1,2
      IF (MCB(1,KFILE)) 370,370,360
  360 CALL BLDPKN (MCB(1,KFILE),BLOCK(KBLOCK),MCB(1,KFILE))
  370 KFILE  = KFILE  + 1
      KBLOCK = KBLOCK + 20
  380 CONTINUE
      GO TO 400
C
C     COLUMN NOT BEING OUTPUT TO ANY PARTITIONS AT ALL THUS SKIP IT.
C
  390 CALL SKPREC (A,1)
C
  400 CONTINUE
C
C     WRAP UP.
C
      CALL CLOSE (A,CLSREW)
      DO 420 I = 1,4
      IF (MCB(1,I)) 420,420,410
  410 CALL WRTTRL (MCB(1,I))
      CALL CLOSE (MCB(1,I),CLSREW)
  420 CONTINUE
  425 RETURN
      END
