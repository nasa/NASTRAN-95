      SUBROUTINE SSGHT
C
C     THIS IS THE STATIC-SOLUTION-GENERATOR FOR HEAT TRANSFER.
C
C     DMAP CALLING SEQUENCE.
C
C     SSGHT  USET,SIL,GPTT,GM,EST,MPT,DIT,PF,PS,KFF,KFS,KSF,KSS,RFN,RSN,
C            LFILE,UFILE/UGV,QG,RULV/V,N,NLK/V,N,NLR/C,Y,EPS0/C,Y,TABS/
C            C,Y,MAXITR/C,Y,IRES/V,N,MPCF1/V,N,SINGLE $
C
      LOGICAL         NOGO,NOQG,RULVEC,DIAGON,LINEAR,LOOP1,NLRAD
      INTEGER         BUF(10),SYSBUF,OUTPT,TSET,RD,RDREW,WRT,WRTREW,
     1                CLSREW,CLS,PRECIS,CORE,SINGLE,EOR,UMCB,BMCB,XMCB,
     2                PKIN,PKOUT,PKIROW,PKNROW,PKINCR,EOL,BUF1,BUF2,
     3                MCB(7),RULMCB(7),GSIZE,FILE,FSIZE,SSIZE,FLAG,WORD,
     4                MCB2(7),NAME(2),USET,GPTT,GM,EST,DIT,UFILE,PF,PS,
     5                RFN,RSN,UGV,QG,RULV,SUBR(2),DITX,Z,SCRT1,SCRT2,
     6                SCRT3,TREQST,TSTART,TEND,TLOOP,SCRT4,TELAPS,
     7                ALIBI(5,5)
      REAL            RBUF(10),RZ(1)
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /XMSSG / UFM,UWM,UIM,SFM
      COMMON /FBSX  / JLMCB(7),JUMCB(7),JBMCB(7),JXMCB(7),JZZZ,JPREC,
     1                JSIGN
      COMMON /GFBSX / LMCB(7),UMCB(7),BMCB(7),XMCB(7),LZ,IPREC,ISIGN
      COMMON /PACKX / PKIN,PKOUT,PKIROW,PKNROW,PKINCR
      COMMON /ZNTPKX/ AI(4),IROW,EOL
      COMMON /ZBLPKX/ AO(4),JROW
      COMMON /SYSTEM/ KSYSTM(65)
      COMMON /STIME / TREQST
      COMMON /HMATDD/ IHMAT,NHMAT,MPTX,DITX
      COMMON /NAMES / RD,RDREW,WRT,WRTREW,CLSREW,CLS
      COMMON /ZZZZZZ/ Z(1)
      COMMON /BLANK / NLK,NLR,EPS0,TABS,MAXITR,IRES,MPCF1,SINGLE
      EQUIVALENCE     (KSYSTM(1),SYSBUF),(KSYSTM(2),OUTPT),
     1                (KSYSTM(10),TSET),(KSYSTM(55),IPREC1),
     2                (BUF(1),RBUF(1)),(Z(1),RZ(1)),(IDFALT,DEFALT)
      DATA    DIAGON/ .FALSE. /
      DATA    SUBR  / 4HSSGH,4HT     /, EOR,NOEOR /1,0/
      DATA    USET  , GPTT,GM,EST,MPTFIL / 101,103,104,105,106/
C     DATA    SIL   / 102     /
      DATA    DIT   , PF,PS,KFF,KFS,KSF,KSS/107,108,109,110,111,112,113/
      DATA    RFN   , RSN,LFILE,UFILE  / 114,115,116,117/
      DATA    UGV   , QG,RULV / 201,202,203 /
      DATA    SCRT1 , SCRT2,SCRT3,SCRT4/ 301,302,303,304/
      DATA    ALIBI / 4H NOR,4HMAL ,4HCONV,4HERGE,4HNCE ,
     1                4H MAX,4HIMUM,4H ITE,4HRATI,4HONS ,
     2                4H DIV,4HERGI,4HNG S,4HOLUT,4HION ,
     3                4H INS,4HUFFI,4HCIEN,4HT TI,4HME  ,
     4                4H MAX,4HIMUM,4H CON,4HVERG,4HENCE/
C
C     OPEN CORE
C
C          PRE-ITERATIONS              DURING-ITERATIONS
C         +--------------+
C         I              I  Z(IUNI)
C         I    I         I
C         I  (U ) VECTOR I
C         I    N         I
C         I              I  Z(NUNI)
C         +--------------+
C         I              I  Z(IHMAT)
C         I  HMAT CORE   I
C         I  BLOCK IF    I
C         I  REQUIRED    I
C         I              I  Z(NHMAT)
C         +--------------+
C         I              I  Z(IUN)
C         I (U ) PARTIT. I
C         I   N  VECTOR  I
C         I      FOR F+S I
C         I              I  Z(NUN)
C         +--------------+  -  -  -  -  -  -  -  -  -  -  -  -  -
C         I              I  Z(IEQIV)        MSIZE
C         I              I           -  -  -  *  -  -  EQUIV TABLE
C         I   E          I                   *I*       WILL BE PLACED
C         I(UN )EQIV.TBL I                  * I *      ON SCRATCH FILE
C         I   G          I                    I        DURING ITERATIONS
C         I              I  Z(NEQIV)          I
C         +--------------+  -  -  -   +----------------+
C         I              I  Z(IUM)    I                I  Z(ISN)
C         I (U )PARTIT.  I            I (S ) DIAGONAL  I
C         I   M VECTOR   I            I   N            I
C         I              I  Z(NUM)    I                I
C         +--------------+            I                I  Z(NSN)
C         I              I  Z(IUME)   +----------------+
C         I   E          I            I                I  Z(IDELU)
C         I (U ) TABLE   I            I (DELTA U ) VEC.I
C         I   M          I  Z(NUME)   I         N      I
C         +--------------+            I                I  Z(NDELU)
C                                     +----------------+
C                                     I                I  Z(IDELP)
C                                     I (DELTA P ) VEC.I
C                                     I         N      I
C                                     I                I  Z(NDELP)
C                                     +----------------+
C                                            .
C                                      CORE  . UNUSED
C                           Z(CORE)          .            Z(CORE)
C         - - - - - - - - - - - - - - +----------------+
C                                     I  BUFFER 2      I  Z(BUF2)
C                                     I                I
C                                     +----------------+
C                                     I  BUFFER 1      I  Z(BUF1)
C                                     I                I
C                                     +----------------+
C
C     CORE SIZE AND BUFFERS
C
      LCORE = KORSZ(Z)
      BUF1  = LCORE - SYSBUF - 2
      BUF2  = BUF1  - SYSBUF - 2
      CORE  = BUF2  - 1
      IF (CORE .LT. 100) CALL MESAGE (-8,0,SUBR)
      PRECIS = 1
C
C     SET MISC. FLAGS.
C
      EPS010 = 10.0*EPS0
      EPSOLD = EPS0 + 1.0
      NLRAD  = .TRUE.
      IF (NLR .EQ. -1) NLRAD = .FALSE.
      CALL SSWTCH (18,K)
      IF (K .EQ. 1) DIAGON = .TRUE.
      LINEAR = .TRUE.
      IF (NLK .EQ. +1) LINEAR = .FALSE.
C
C     READ TRAILER OF USET TO GET GSIZE.
C
      MCB(1) = USET
      CALL RDTRL (MCB)
      FILE = USET
      IF (MCB(1) .LE. 0) GO TO 1290
      GSIZE = MCB(3)
C
C     READ GM TRAILER TO DETERMINE COUNT OF UM POINTS
C
      MCB(1) = GM
      CALL RDTRL (MCB)
      IF (MCB(1) .LE. 0) GO TO 30
      MSIZE = MCB(3)
      GO TO 40
   30 MSIZE = 0
   40 NSIZE = GSIZE - MSIZE
C
C     CORE ALLOCATION.
C
      IUNI  = 1
      NUNI  = NSIZE
      IUNIZ = IUNI - 1
      IHMAT = NUNI + 1
      NHMAT = NUNI
      IF (.NOT.LINEAR) NHMAT = CORE
      MPTX  = MPTFIL
      DITX  = DIT
      IF (.NOT.LINEAR) CALL PREHMA (Z)
      IUN   = NHMAT + 1
      NUN   = NHMAT + NSIZE
      IEQIV = NUN + 1
      NEQIV = NUN + GSIZE
C
C     EQUIVALENCE TABLE WILL BE PUT ON SCRATCH DURING ITERATIONS.
C
      ISN   = NUN + MSIZE + 1
      NSN   = ISN + NSIZE - 1
      IF (.NOT.NLRAD) NSN = ISN - 1
      ISNZ  = ISN - 1
      IDELU = NSN + 1
      NDELU = NSN + NSIZE
      IDELUZ= IDELU - 1
      IDELP = NDELU + 1
      NDELP = NDELU + NSIZE
      IF (NDELP .GT. CORE) CALL MESAGE (-8,0,SUBR)
      IDELPZ= IDELP - 1
      IUM   = NEQIV + 1
      NUM   = NEQIV + MSIZE
      IUMZ  = IUM - 1
      IUME  = NUM + 1
      NUME  = NUM + MSIZE
      IUMEZ = IUME - 1
      IF (NUME .GT. CORE) CALL MESAGE (-8,0,SUBR)
C
C     CONSTRUCTION OF (U ) AND (U ) TABLES.
C                       M        N
C
      FILE   = USET
      CALL GOPEN (USET,Z(BUF1),RDREW)
      MPOINT = IUMZ
      NPOINT = IUN - 1
      ISIL   = 0
      FSIZE  = 0
      CALL FREAD (USET,Z(IEQIV),GSIZE,0)
      CALL CLOSE (USET,CLSREW)
      DO 120 I = IEQIV,NEQIV
      WORD = Z(I)
      ISIL = ISIL + 1
C
C     CHECK FOR M-POINT
C
      IF (MOD(WORD,2) .LE. 0) GO TO 90
      MPOINT = MPOINT + 1
      Z(MPOINT) = ISIL
      GO TO 120
C
C     ASSUME N-POINT
C
   90 NPOINT = NPOINT + 1
C
C     CHECK FOR F OR S POINT
C
      IF (MOD(WORD/2,2)) 100,100,110
C
C     OK N-POINT IS AN F-POINT.
C
  100 Z(NPOINT) = -ISIL
      FSIZE = FSIZE + 1
      GO TO 120
C
C     OK N-POINT IS ASSUMED AN S-POINT
C
  110 Z(NPOINT) = +ISIL
  120 CONTINUE
      SSIZE = NSIZE - FSIZE
C
C     U  AND U  ARE COMPLETE.
C      M      N
C
      IF (ISIL.EQ.GSIZE .AND. NPOINT.EQ.NUN .AND. MPOINT.EQ.NUM)
     1    GO TO 140
      WRITE  (OUTPT,130) SFM
  130 FORMAT (A25,' 3081, INCONSISTENT USET DATA DETECTED.')
      CALL MESAGE (-61,0,SUBR)
C
C             E
C     BUILD (U ) EQUIVALENCE (U ) POINTS FOR (U ).
C             M                N               M
C
  140 IF (NUME .LT. IUME) GO TO 250
      DO 150 I = IUME,NUME
      Z(I) = 0
  150 CONTINUE
      CALL GOPEN (GM,Z(BUF1),RDREW)
      DO 210 I = 1,NSIZE
C
C     OPERATE ON A COLUMN OF GM.
C
      CALL INTPK (*210,GM,0,PRECIS,0)
  160 CALL ZNTPKI
C
C                             E
C     ROW POSITION -IROW- IN U  GETS COLUMN NUMBER.
C                             M
C
      IPOS = IUMEZ + IROW
      IF (Z(IPOS)) 170,170,190
  170 Z(IPOS) = I
  180 IF (EOL) 160,160,210
C
C     ERROR
C
  190 WRITE  (OUTPT,200) UWM,IROW,I
  200 FORMAT (A25,' 3082, M =',I10,'  N =',I10)
      GO TO 180
  210 CONTINUE
      CALL CLOSE (GM,CLSREW)
C
C     INSURE ALL UME SLOTS FILLED
C
      NOGO = .FALSE.
      DO 240 I = IUME,NUME
      IF (Z(I)) 220,220,240
  220 M = I - IUMEZ
      ISIL = IUMZ + M
      WRITE  (OUTPT,230) UFM,M,Z(ISIL)
  230 FORMAT (A23,' 3083, UM POSITION =',I10,', SIL =',I10)
      NOGO = .TRUE.
  240 CONTINUE
      IF (NOGO) CALL MESAGE (-61,0,SUBR)
C
C                        E
C     CONSTRUCTION OF (UN ) EQUIVALENCE TABLE.
C                        G
C
  250 MPOINT = IUM
      MPT  = Z(MPOINT)
      IF (MPOINT .GT. NUM ) MPT = 1000000
      MPTE = IUME
      MVAL = Z(MPTE)
      NVAL = 1
      K = IEQIV - 1
      DO 270 I = 1,GSIZE
      K = K + 1
      IF (I .NE. MPT) GO TO 260
C
C     M-POINT NEXT
C
      Z(K) = MVAL
      MPTE = MPTE + 1
      MVAL = Z(MPTE)
      MPOINT = MPOINT + 1
      MPT  = Z(MPOINT)
      IF (MPOINT .GT. NUM ) MPT = 1000000
      GO TO 270
C
C     N-POINT NEXT
C
  260 Z(K) = NVAL
      NVAL = NVAL + 1
  270 CONTINUE
C
C     SET UP RULV IF RESIDUAL LOAD MATRIX IS TO BE FORMED.
C
      RULVEC = .FALSE.
      IF (IRES .LE. 0) GO TO 290
      CALL MAKMCB (RULMCB,RULV,FSIZE,2,PRECIS)
      CALL GOPEN (RULV,Z(BUF1),WRTREW)
      CALL CLOSE (RULV,CLS)
      RULVEC = .TRUE.
C
C     GRID POINT TEMPERATURE DATA IS EXPANDED INTO CORE NOW.  ONLY
C
C      1
C     U  IS FORMED.
C      N
C
C
  290 IF (TSET) 300,300,310
  300 K = 0
      GO TO 320
  310 K = 1
  320 DO 330 I = IUNI,NUNI
      Z(I) = K
  330 CONTINUE
      IF (TSET) 510,510,340
C
C     POSITION GPTT TO GRID TEMPERATURE DATA SECTION.
C
  340 FILE = GPTT
      CALL OPEN (*1290,GPTT,Z(BUF1),RDREW)
      CALL FREAD (GPTT,BUF,-2,0)
      NUMBER = 0
  350 CALL READ (*1300,*360,GPTT,BUF,3,NOEOR,FLAG)
      NUMBER = MAX0(NUMBER,BUF(3))
      GO TO 350
  360 CALL SKPREC (GPTT,NUMBER)
C
C     NOW AT GRID TEMP SECTION HEADER.
C
      CALL FREAD (GPTT,BUF,-2,0)
  400 CALL READ (*1300,*1330,GPTT,BUF,3,NOEOR,FLAG)
      IF (TSET .NE. BUF(1)) GO TO 400
C
C     BUF(1)=SET-ID,  BUF(2)=-1 OR DEFAULT TEMP,  BUF(3)=GPTT RECORD.
C
      DEFALT = RBUF(2)
      IF (BUF(3) .LE. 0) GO TO 470
      CALL SKPREC (GPTT,BUF(3))
C
C     TEMP PAIRS IN INTERNAL-ID AND TEMPERATURE.
C
      IUNAT = IUN
      ISIL  = IABS(Z(IUNAT))
      IUNIAT= IUNI
C
C     READ A TEMPERATURE PAIR.
C
  430 CALL READ (*1300,*470,GPTT,BUF,2,NOEOR,FLAG)
  440 IF (BUF(1)-ISIL) 430,450,460
  450 Z(IUNIAT) = BUF(2)
  460 IUNAT = IUNAT + 1
      ISIL  = IABS(Z(IUNAT))
      IUNIAT= IUNIAT + 1
      IF (IUNIAT .LE. NUNI) GO TO 440
  470 CALL CLOSE (GPTT,CLSREW)
C
C     CHECK FOR INTEGER 1-S WHICH GET THE DEFAULT TEMP.
C
      NOGO = .FALSE.
      DO 500 I = IUNI,NUNI
      IF (Z(I) .NE. 1) GO TO 500
      IF (IDFALT .NE. -1) GO TO 490
      NOGO = .TRUE.
      K = IUN + I - IUNI
      ISIL = IABS(Z(K))
      WRITE  (OUTPT,480) UFM,ISIL
  480 FORMAT (A23,' 3084, THERE IS NO TEMPERATURE DATA FOR SIL NUMBER',
     1        I10)
      GO TO 500
  490 RZ(I) = DEFALT
  500 CONTINUE
      IF (NOGO) CALL MESAGE (-61,0,SUBR)
  510 CONTINUE
C
C               1                  1
C     COMPUTE (P ) = (P ) - (K  )(U ) AND SAVE ON SCRATCH-4.
C               F      F      FS   S
C
      K = IDELPZ + FSIZE
      DO 520 I = IDELP,K
      Z(I) = 0
  520 CONTINUE
      CALL OPEN (*540,PF,Z(BUF1),RDREW)
      CALL FWDREC (*540,PF)
      CALL INTPK (*540,PF,0,PRECIS,0)
  530 CALL ZNTPKI
      K = IDELPZ + IROW
      RZ(K) = AI(1)
      IF (EOL) 530,530,540
  540 CALL CLOSE (PF,CLSREW)
C
C                         1
C     SUBTRACT OFF (K  )(U )
C                    FS   S
C
      IAT = IUN - 1
      CALL OPEN (*590,KFS,Z(BUF1),RDREW)
      CALL FWDREC (*590,KFS)
      DO 580 I = 1,SSIZE
C
C     FIND NEXT US POINT TEMPERATURE DATA.
C
  550 IAT = IAT + 1
      IF (Z(IAT)) 550,550,560
  560 K = IUNIZ + IAT - IUN + 1
      CALL INTPK (*580,KFS,0,PRECIS,0)
      VALUE = RZ(K)
  570 CALL ZNTPKI
      K = IDELPZ + IROW
      RZ(K) = RZ(K) - AI(1)*VALUE
      IF (EOL) 570,570,580
  580 CONTINUE
  590 CALL CLOSE (KFS,CLSREW)
C
C                1
C     PACK OUT (P ) ON SCRATCH-4
C                F
C
      CALL GOPEN (SCRT4,Z(BUF1),WRTREW)
      CALL MAKMCB (MCB,SCRT4,FSIZE,2,PRECIS)
      PKIN   = PRECIS
      PKOUT  = PRECIS
      PKIROW = 1
      PKNROW = FSIZE
      PKINCR = 1
      CALL PACK (Z(IDELP),SCRT4,MCB)
      CALL CLOSE (SCRT4,CLSREW)
      CALL WRTTRL (MCB)
C
C     ELEMENT INITIAL PROCESSING PHASE.
C
      CALL GOPEN  (SCRT1,Z(BUF2),WRTREW)
      IF (LINEAR) GO TO 600
      CALL GOPEN  (EST,Z(BUF1),RDREW)
      CALL SSGHT1 (EST,SCRT1,Z(IEQIV))
      CALL CLOSE  (EST,CLSREW)
C
C        E
C     (UN ) EQUIVALENCE TABLE IS NOW APPENDED TO -SCRT1-.
C        G
C
  600 CALL WRITE (SCRT1,0,0,1)
      CALL WRITE (SCRT1,Z(IEQIV),GSIZE,1)
      CALL CLOSE (SCRT1,CLSREW)
C
C                      1            3
C     FORM (S ) = 4( (U ) + (TABS) )  DIAGONAL MATRIX.
C            N         N
C
      IF (.NOT.NLRAD) GO TO 630
      J = IUNIZ
      DO 620 I = ISN,NSN
      J = J + 1
      RZ(I) = 4.0*(RZ(J) + TABS)**3
  620 CONTINUE
C
C     SET PARTITIONING TABLE IN TERMS OF WHERE ELEMENTS ARE TO MOVE TO
C     WHEN GOING FROM N-SET TO F+S SETS.
C
  630 IS = FSIZE
      IF = 0
      DO 660 I = IUN,NUN
      IF (Z(I)) 640,640,650
C
C     F-POINTER
C
  640 IF   = IF + 1
      Z(I) = IF
      GO TO 660
C
C     S-POINTER
C
  650 IS = IS + 1
      Z(I)  = IS
  660 CONTINUE
      LOOP  = 0
      LOOP1 = .TRUE.
      PFMAG = 0.0
C
C     == ITERATION SECTION ==
C
C     ITERATIVE LOOPING
C
  670 LOOP = LOOP + 1
C
C     TIME LEFT AT START OF LOOP
C
      CALL TMTOGO (TSTART)
      DO 680 I = IDELP,NDELP
      Z(I) = 0
  680 CONTINUE
      IF (LOOP1 .OR. LINEAR) GO TO 690
      CALL GOPEN (SCRT1,Z(BUF1),RDREW)
      CALL SSGHT2 (SCRT1,Z(IDELP),Z(IUNI))
      CALL CLOSE (SCRT1,CLSREW)
C
C     PARTITION DELTA-P VECTOR INTO DELTA-F AND DELTA-S VECTORS.
C
      CALL SSGHTP (Z(IUN),Z(IDELP),NSIZE)
C
C                     I
C     GENERATION OF (N ) WILL BE PERFORMED IN CORE SPACE OF (DELTA-P)
C                     F
C       I                          I        4         I
C     (N ) = (DELTA-P ) + (R  )( (U  + TABS)  - (S )(U ) )
C       F            F      FN     N               N  N
C
  690 IF (.NOT.NLRAD) GO TO 730
      CALL OPEN (*720,RFN,Z(BUF2),RDREW)
      CALL FWDREC (*720,RFN)
      DO 710 I = 1,NSIZE
C
C     OPERATE ON A COLUMN OF RFN
C
      CALL INTPK (*710,RFN,0,PRECIS,0)
C
C     COMPUTE CONSTANT FOR COLUMN
C
      K  = IUNIZ + I
      UN = RZ(K)
      K  = ISNZ + I
      SN = RZ(K)
      VALUE = (UN + TABS)**4 - SN*UN
C
C     UNPACK NON-ZERO TERMS OF COLUMN.
C
  700 CALL ZNTPKI
      K = IDELPZ + IROW
      RZ(K) = RZ(K) + AI(1)*VALUE
      IF (EOL) 700,700,710
  710 CONTINUE
  720 CALL CLOSE (RFN,CLSREW)
C
C          I      1      I
C     (PBAR ) = (P ) - (N )
C          F      F      F
C                    I
C     FIRST NEGATE (N ) SITTING IN DELTA-P CORE SPACE,
C                    F
C                                     1
C     THEN ADD IN NON-ZERO TERMS OF (P )
C                                     F
C
  730 K = IDELPZ + FSIZE
      DO 740 I = IDELP,K
      RZ(I) = -RZ(I)
  740 CONTINUE
C
C            1
C     OPEN (P ) FOR UNPACKING OF ONE COLUMN.
C            F
C
      CALL OPEN (*760,SCRT4,Z(BUF2),RDREW)
      CALL FWDREC (*760,SCRT4)
      CALL INTPK (*760,SCRT4,0,PRECIS,0)
  750 CALL ZNTPKI
      K = IDELPZ + IROW
      RZ(K) = RZ(K) + AI(1)
      IF (LOOP1) PFMAG = PFMAG + AI(1)*AI(1)
      IF (EOL) 750,750,760
  760 CALL CLOSE (SCRT4,CLSREW)
C
C          I
C     (PBAR ) IS NOW PACKED OUT TO SCRATCH-2.
C          F
C
      IF (.NOT.LOOP1) GO TO 790
      PFMAG = SQRT(PFMAG)
      IF (PFMAG) 770,770,790
  770 WRITE  (OUTPT,780) UFM
  780 FORMAT (A23,' 3085, THE PF LOAD VECTOR IS EITHER PURGED OR NULL.')
      CALL MESAGE (-61,0,SUBR)
  790 CALL MAKMCB (MCB2,SCRT2,FSIZE,2,2)
      CALL GOPEN (SCRT2,Z(BUF2),WRTREW)
      PKIN   = PRECIS
      PKOUT  = IPREC1
      PKIROW = 1
      PKNROW = FSIZE
      PKINCR = 1
      CALL PACK (Z(IDELP),SCRT2,MCB2)
      CALL CLOSE (SCRT2,CLSREW)
      CALL WRTTRL (MCB2)
C
C                       I           I
C     (DELTA-P ) = (PBAR ) - (K  )(U )
C             F         F      FF   F
C          I
C     (PBAR ) IS SITING IN CORE CURRENTLY.  (IT WILL BE GONE TOMORROW.)
C          F
C                       I       I        I
C     FIRST PARTITION (U ) TO (U ) AND (U )
C                       N       F        S
C
      CALL SSGHTP (Z(IUN),Z(IUNI),NSIZE)
      CALL OPEN (*820,KFF,Z(BUF1),RDREW)
      CALL FWDREC (*820,KFF)
      DO 810 I = 1,FSIZE
C
C     OPERATE ON ONE COLUMN OF KFF
C
      CALL INTPK (*810,KFF,0,PRECIS,0)
C
C                                 I
C     LOCATE COLUMN MULTIPLIER = U
C                                 FI
C
      K = IUNIZ + I
      VALUE = RZ(K)
  800 CALL ZNTPKI
C                                                            I
C     SUBTRACT THIS ELEMENT*VALUE FROM IROW POSITION OF (PBAR )
C                                                            F
      K = IDELPZ + IROW
      RZ(K) = RZ(K) - AI(1)*VALUE
      IF (EOL) 800,800,810
  810 CONTINUE
  820 CALL CLOSE (KFF,CLSREW)
C
C     COMPUTE EPSILON
C                    P
C
      K   = IDELPZ + FSIZE
      SUM = 0.0
      DO 830 I = IDELP,K
      SUM = SUM + RZ(I)**2
  830 CONTINUE
      SUM = SQRT(SUM)
      EPSUBP = SUM/PFMAG
      IF (LOOP1 .AND. DIAGON) WRITE (OUTPT,840) EPSUBP
  840 FORMAT ('1D I A G   1 8   O U T P U T   F R O M   S S G H T', //,
     1       ' ITERATION    EPSILON-P',9X,'LAMBDA-1',10X,'EPSILON-T',
     2       /1X,60(1H=), /,6H     1,1P,E19.6)
C
C                                                   I
C     IF -RULV- IS BEING FORMED, THEN WRITE (DELTA-P ) OUT ON -RULV-.
C                                                   F
C
      IF (.NOT. RULVEC) GO TO 850
      CALL OPEN (*850,RULV,Z(BUF1),WRT)
      PKIN   = PRECIS
      PKOUT  = PRECIS
      PKIROW = 1
      PKNROW = FSIZE
      PKINCR = 1
      CALL PACK (Z(IDELP),RULV,RULMCB)
      CALL CLOSE (RULV,CLS)
C
C                     I+1
C     NOW SOLVE FOR (U   ) IN,
C                     F
C                           I+1         I
C                   (L)(U)(U   ) = (PBAR )
C                           F           F
C
C
  850 ISIGN   =+1
      IPREC   = 2
      LMCB(1) = LFILE
      CALL RDTRL (LMCB)
      UMCB(1) = UFILE
      CALL RDTRL (UMCB)
      BMCB(1) = SCRT2
      CALL RDTRL (BMCB)
      CALL MAKMCB (XMCB,SCRT3,FSIZE,2,2)
C
C     INSURE EVEN BOUNDARY (ARRAY WILL BE USED AS DOUBLE PRECISION)
C
      JDELP = NDELP + 1 + MOD(NDELP+1,2) + 1
      LZ = LCORE - JDELP
CWKBI 3/94
      JZZZ = LZ
      DO 855 IJK = 1,31
      JLMCB(IJK) = LMCB(IJK)
  855 CONTINUE
      IF (UMCB(1) .GT. 0) CALL GFBS (Z(JDELP),Z(JDELP))
      IF (UMCB(1) .LE. 0) CALL  FBS (Z(JDELP),Z(JDELP))
      IF (UMCB(1) .GT. 0) CALL WRTTRL( XMCB)
      IF (UMCB(1) .LE. 0) CALL WRTTRL(JXMCB)
C
C       I+1
C     (U   ) IS NOW MOVED FROM SCRATCH-3 INTO CORE IN (DELTA-P ) SPACE.
C       F                                                     N
C
      CALL GOPEN (SCRT3,Z(BUF1),RDREW)
      K = IDELPZ + FSIZE
      DO 860 I = IDELP,K
      Z(I) = 0
  860 CONTINUE
      CALL INTPK (*880,SCRT3,0,PRECIS,0)
  870 CALL ZNTPKI
      K = IDELPZ + IROW
      RZ(K) = AI(1)
      IF (EOL) 870,870,880
  880 CALL CLOSE (SCRT3,CLSREW)
      IF (LOOP1) GO TO 985
C
C                      I+1       I
C     ALPHA = SUM OF (U   ) (PBAR )          IROW = 1,FSIZE
C                      F         F
C                       IROW      IROW
C
C                           I+1      I
C     BETA = SUM OF (DELTA-U   )(PBAR )      IROW = 1,FSIZE
C                           F        F
C                            IROW     IROW
C
C                      I+1    I       I
C     GAMMA = SUM OF (U    - U  )(PBAR )     IROW = 1,FSIZE
C                      F      F       F
C                       IROW   IROW    IROW
C
C     WHERE I = ITERATION GREATER THAN 1.
C
      CALL GOPEN (SCRT2,Z(BUF1),RDREW)
      ALPHA = 0.0
      BETA  = 0.0
      GAMMA = 0.0
      CALL INTPK (*900,SCRT2,0,PRECIS,0)
C
C     ONLY NON-ZERO TERMS OF (PBAR ) NEED BE CONSIDERED.
C                                 F
  890 CALL ZNTPKI
      KUFIP1 = IDELPZ + IROW
      KDELU  = IDELUZ + IROW
      KUFI   = IUNIZ  + IROW
      ALPHA  = ALPHA  + RZ(KUFIP1)*AI(1)
      BETA   = BETA   + RZ(KDELU) *AI(1)
      GAMMA  = GAMMA  + (RZ(KUFIP1) - RZ(KUFI))*AI(1)
      IF (EOL) 890,890,900
  900 CALL CLOSE (SCRT2,CLSREW)
C
C     CONVERGENCE TESTS ARE MADE HERE.
C
C     WHEN ENTERING EXIT MODE,
C         -IEXIT-        -REASON-
C            1           NORMAL CONVERGENCE
C            2           NO CONVERGENCE AT MAXIMUM ITERATIONS
C            3           NO CONVERGENCE UNSTABLE ITERATION
C            4           NO CONVERGENCE INSUFFICIENT TIME
C            5           MAXIMUM CONVERGENCE, BUT EPSHT NOT SATISFIED
C
      IF (GAMMA) 910,920,910
  910 FLAMDA = ABS(BETA/GAMMA)
      GO TO 930
  920 FLAMDA = 100.0
      EPST   = 0.0
      GO TO 970
  930 IF (ALPHA) 940,960,940
  940 IF (FLAMDA-1.0) 950,960,950
  950 EPST = ABS(GAMMA/((FLAMDA - 1.0)*ALPHA))
      GO TO 970
  960 EPST = 100.0
  970 CALL TMTOGO (KLEFT)
      TELAPS = TREQST - KLEFT
      TAU    = 1.0 - FLOAT(TLOOP+TELAPS)/(.8*FLOAT(TREQST))
      IF (DIAGON) WRITE (OUTPT,980) LOOP,EPSUBP,FLAMDA,EPST
  980 FORMAT (I6,1P,E19.6,1P,E18.6,1P,E18.6)
      IEXIT  = 1
      IF (EPST.LT.EPS0 .AND. FLAMDA.GT.1.0 .AND. EPSUBP.LT.EPS010)
     1    GO TO 1060
C
C     TEST FOR TWO SUCCESSIVE CASES PASSING TEST
C
      IF (EPST.LT.EPS0 .AND. EPSOLD.LT.EPS0) GO TO 1060
      EPSOLD = EPST
      IEXIT  = 2
      IF (LOOP .GE. MAXITR) GO TO 1060
      IEXIT  = 3
      IF (FLAMDA.LE.1.0 .AND. LOOP.GE.4) GO TO 1060
      IEXIT  = 5
      IF (GAMMA .EQ. 0.) GO TO 1060
      IEXIT  = 4
      IF (TAU) 1060,990,990
C
C                   I
C     COMPUTE (DELTA ) TO BE USED ON NEXT LOOP
C                   U
C
  985 IEXIT = 2
      IF (LOOP .GE. MAXITR) GO TO 1051
  990 K  = IDELPZ + FSIZE
      KDELU = IDELUZ
      KI = IUNIZ
      KIP1 = IDELPZ
      DO 1000 I = IDELP,K
      KDELU = KDELU + 1
      KI = KI + 1
      KIP1 = KIP1 + 1
      RZ(KDELU) = RZ(KIP1) - RZ(KI)
 1000 CONTINUE
C
C                       I+1
C     MOVE (U ) UNDER (U   ) BOTH TO BE IN (DELTA-P ) CORE.
C            S          F                          N
C
      ASSIGN 1050 TO IRETRN
 1010 K1 = IUNI + FSIZE
      K2 = IDELPZ + FSIZE
      IF (SSIZE .LE. 0) GO TO 1030
      DO 1020 I = K1,NUNI
      K2 = K2 + 1
      RZ(K2) = RZ(I)
 1020 CONTINUE
C
C             I+1                       I+1
C     MERGE (U   ) AND (U ) BACK INTO (U   ) FORM.
C             F          S              N
C
 1030 KUNI = IUNIZ
      DO 1040 I = IUN,NUN
      KUNI = KUNI + 1
      JPOS = IDELPZ + Z(I)
      Z(KUNI) = Z(JPOS)
 1040 CONTINUE
      GO TO IRETRN, (1050,1180)
C
C     READY NOW FOR ANOTHER LOOP.
C
 1050 CALL TMTOGO (TEND)
      TLOOP = TSTART - TEND
      LOOP1 = .FALSE.
      GO TO 670
C
C     == END ITERATION SECTION ==
C
C     ITERATION HALTED, NOW IN EXIT MODE.
C     IF QG FILE IS PRESENT, FORCES OF CONSTRAINT ARE PARTIALLY COMPUTED
C     QS WILL BE FORMED IN THE CORE SPACE USED UP TO NOW FOR (DELTA-U).
C
C                           I
C     (Q ) = -(P ) + (K  )(U ) + (K  )(U ) + (DELTA-P ) + (PRODUCT )
C       S       S      SF   F      SS   S            S            S
C
C                                 I         4        I
C     WHERE (PRODUCT ) = (R  )( (U   + TABS)  - (S  U ) )
C                   S      SN     NJ              NJ N
C
C                                      J = 1,NSIZE
C
C     LOAD (DELTA-P ) INTO QS FORMATION CORE SPACE.
C                  S
C
 1051 WRITE  (OUTPT,1052) UWM
 1052 FORMAT (A25,' 3132, SSGHT RECOVERING FROM SEVERE USER CONVERGENCE'
     1,       ' CRITERIA.')
 1060 WRITE  (OUTPT,1070) UIM,IEXIT,(ALIBI(J,IEXIT),J=1,5)
 1070 FORMAT (A29,' 3086, ENTERING SSGHT EXIT MODE BY REASON NUMBER ',
     1        I2,2H (,5A4,1H) )
      NOQG = .TRUE.
      CALL OPEN (*1170,QG,Z(BUF2),WRTREW)
      NOQG = .FALSE.
      CALL FNAME (QG,NAME)
      CALL WRITE (QG,NAME,2,EOR)
      IQS  = IDELU
      NQS  = IDELUZ + SSIZE
      IQSZ = IDELUZ
      K    = IDELPZ + FSIZE
      DO 1080 I = IQS,NQS
      K    = K + 1
      Z(I) = Z(K)
 1080 CONTINUE
C
C     SUBTRACT OFF NON-ZERO TERMS OF PS VECTOR.
C
      CALL OPEN (*1100,PS,Z(BUF1),RDREW)
      CALL FWDREC (*1100,PS)
      CALL INTPK (*1100,PS,0,PRECIS,0)
 1090 CALL ZNTPKI
      K = IQSZ + IROW
      RZ(K) = RZ(K) - AI(1)
      IF (EOL) 1090,1090,1100
 1100 CALL CLOSE (PS,CLSREW)
C
C                   I
C     ADD IN (K  )(U )
C              SF   F
C
      CALL OPEN (*1130,KSF,Z(BUF1),RDREW)
      CALL FWDREC (*1130,KSF)
      DO 1120 I = 1,FSIZE
      CALL INTPK (*1120,KSF,0,PRECIS,0)
      K = IDELPZ + I
      VALUE = RZ(K)
 1110 CALL ZNTPKI
      K = IQSZ + IROW
      RZ(K) = RZ(K) + AI(1)*VALUE
      IF (EOL) 1110,1110,1120
 1120 CONTINUE
 1130 CALL CLOSE (KSF,CLSREW)
C
C     ADD IN (K  )(U )
C              SS   S
C
      IF (SSIZE .EQ. 0) GO TO 1160
      CALL OPEN (*1160,KSS,Z(BUF1),RDREW)
      CALL FWDREC (*1160,KSS)
      IUSZ = IUNIZ + FSIZE
      DO 1150 I = 1,SSIZE
      CALL INTPK (*1150,KSS,0,PRECIS,0)
      K = IUSZ + I
      VALUE = RZ(K)
 1140 CALL ZNTPKI
      K = IQSZ + IROW
      RZ(K) = RZ(K) + AI(1)*VALUE
      IF (EOL) 1140,1140,1150
 1150 CONTINUE
 1160 CALL CLOSE (KSS,CLSREW)
C
C                                     I
C     TO COMPUTE ADDITIONAL PRODUCT (U ) IS NOW FORMED.
C                                     N
C                           I
C     THUS MERGE (U ) AND (U )
C                  S        F
C                                  I
C     FIRST MOVE (U ) DOWN UNDER (U ), THEN DO MERGE.
C                  S               F
C
 1170 ASSIGN 1180 TO IRETRN
      GO TO 1010
C
C     OK FORM AND ADD (PRODUCT) IN.
C
 1180 IF (.NOT.NLRAD) GO TO 1220
      IF (NOQG) GO TO 1250
      CALL OPEN (*1210,RSN,Z(BUF1),RDREW)
      CALL FWDREC (*1210,RSN)
      DO 1200 I = 1,NSIZE
      CALL INTPK (*1200,RSN,0,PRECIS,0)
      KU = IUNIZ + I
      KS = ISNZ + I
      VALUE = (RZ(KU) + TABS)**4 - RZ(KU)*RZ(KS)
 1190 CALL ZNTPKI
      K = IQSZ + IROW
      RZ(K) = RZ(K) + AI(1)*VALUE
      IF (EOL) 1190,1190,1200
 1200 CONTINUE
 1210 CALL CLOSE (RSN,CLSREW)
C
C     (QS) IS COMPLETE AND READY FOR EXPANSION TO GSIZE AND OUTPUT.
C
 1220 CALL MAKMCB (MCB,QG,GSIZE,2,PRECIS)
      JROW = 0
      FILE = USET
      IF (SSIZE .EQ. 0) GO TO 1250
      CALL GOPEN (USET,Z(BUF1),RDREW)
      IQ = IQS
      CALL BLDPK (PRECIS,PRECIS,QG,0,0)
 1230 CALL FREAD (USET,WORD,1,0)
      JROW = JROW + 1
      IF (MOD(WORD/2,2)) 1230,1230,1240
 1240 AO(1) = RZ(IQ)
      CALL ZBLPKI
      IQ = IQ + 1
      IF (IQ .LE. NQS) GO TO 1230
C
C     QS HAS NOW BEEN EXPANDED TO GSIZE AND OUTPUT ON QG DATA BLOCK.
C
      CALL BLDPKN (QG,0,MCB)
      CALL CLOSE  (QG,CLSREW)
      CALL WRTTRL (MCB)
      CALL CLOSE  (USET,CLSREW)
C
C     PACK OUT (U ) USING THE EQUIVALENCE TABLE TO ORDER
C                G
C
C     THE U  POINTS.
C          N
C
C
C     READ EQUIVALENCE TABLE BACK INTO CORE AT THIS TIME.
C
 1250 FILE = SCRT1
      CALL GOPEN  (SCRT1,Z(BUF1),RDREW)
      CALL SKPREC (SCRT1,1)
      CALL FREAD  (SCRT1,Z(IEQIV),GSIZE,0)
C
      CALL CLOSE  (SCRT1,CLSREW)
C
C     REPLACE POINTERS WITH THE VALUES.
C
      DO 1270 I = IEQIV,NEQIV
      K = IUNIZ + Z(I)
      RZ(I) = RZ(K)
 1270 CONTINUE
C
C     PACK OUT (U )
C                G
C
      CALL MAKMCB (MCB,UGV,GSIZE,2,PRECIS)
      CALL GOPEN  (UGV,Z(BUF1),1)
      PKIN   = PRECIS
      PKOUT  = PRECIS
      PKIROW = 1
      PKNROW = GSIZE
      PKINCR = 1
      CALL PACK (Z(IEQIV),UGV,MCB)
      CALL CLOSE (UGV,CLSREW)
      CALL WRTTRL (MCB)
C
C     COMPLETE RULV IF NECESSARY.
C
      IF (.NOT.RULVEC) GO TO 1280
      CALL GOPEN (RULV,Z(BUF1),3)
      CALL CLOSE (RULV,CLSREW)
      CALL WRTTRL (RULMCB)
 1280 RETURN
C
C     ERROR CONDITIONS
C
 1290 N = -1
      GO TO 1320
 1300 N = -2
      GO TO 1320
 1320 CALL MESAGE (N,FILE,SUBR)
 1330 WRITE  (OUTPT,1340) UFM,TSET
 1340 FORMAT (A23,' 3087, TEMPERATURE SET',I10,' IS NOT PRESENT IN ',
     1        'GPTT DATA BLOCK.')
      CALL MESAGE (-61,0,SUBR)
      RETURN
      END
