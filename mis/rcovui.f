      SUBROUTINE RCOVUI (UB,LASTSS,MODAL)
C
C     THIS ROUTINE CALCULATES THE IMPROVED LOWER LEVEL DISPLACEMENTS
C     ON A REDUCED SUBSTRUCTURE WHICH INCLUDE INERTIA AND DAMPING
C     EFFECTS
C
      LOGICAL          REQF       ,MODAL
      INTEGER          UB         ,SOF1       ,SOF2       ,SOF3      ,
     1                 BUF1       ,BUF2       ,SCR2       ,SCR3      ,
     2                 SCR4       ,SCR5       ,SCR6       ,SCR7      ,
     3                 SCR8       ,SCR9       ,MPYZ       ,TFLAG     ,
     4                 SIGNAB     ,HORG       ,BMTX       ,UPRT      ,
     5                 SIGNC      ,SCRM       ,Z          ,RC        ,
     6                 DRY        ,FSS        ,RFNO       ,RSS       ,
     7                 UA         ,BUF4       ,BGG        ,PID       ,
     8                 UAO        ,RULE       ,TYPA       ,TYPB      ,
     9                 BUF3       ,UPART      ,LASTSS(2)  ,GIMS      ,
     O                 DUA        ,UAD        ,TYPIN      ,TYPOT     ,
     1                 TYPC       ,NAME(2)
      REAL             RZ(1)
      DOUBLE PRECISION DZ(1)
      COMMON /BLANK /  DRY        ,LOOP       ,STEP       ,FSS(2)    ,
     1                 RFNO       ,NEIGV      ,LUI        ,UINMS(2,5),
     2                 NOSORT     ,UTHRES     ,PTHRES     ,QTHRES
      COMMON /RCOVCR/  ICORE      ,LCORE      ,BUF1       ,BUF2      ,
     1                 BUF3       ,BUF4       ,SOF1       ,SOF2      ,
     2                 SOF3
      COMMON /RCOVCM/  MRECVR     ,UA         ,PA         ,QA        ,
     1                 IOPT       ,RSS(2)     ,ENERGY     ,UIMPRO    ,
     2                 RANGE(2)   ,IREQ       ,LREQ       ,LBASIC
      COMMON /ZZZZZZ/  Z(1)
      COMMON /MPYADX/  MCBA(7)    ,MCBB(7)    ,MCBC(7)    ,MCBD(7)   ,
     1                 MPYZ       ,TFLAG      ,SIGNAB     ,SIGNC     ,
     2                 MPREC      ,SCRM
      COMMON /NAMES /  RD         ,RDREW      ,WRT        ,WRTREW    ,
     1                 REW        ,NOREW      ,EOFNRW     ,RSP       ,
     2                 RDP        ,CSP        ,CDP        ,SQUARE    ,
     3                 RECT       ,DIAG       ,UPPER      ,LOWER     ,
     4                 SYM
      COMMON /PARMEG/  MCB(7)     ,MCB11(7)   ,MCB21(7)   ,MCB12(7)  ,
     1                 MCB22(7)   ,MRGZ       ,RULE
      COMMON /SADDX /  NOMAT      ,LCOR       ,MCBAA(7)   ,TYPA      ,
     1                 ALPHA      ,ALP(3)     ,MCBBB(7)   ,TYPB      ,
     2                 BETA       ,BET(3)     ,MCBCC(7)   ,TYPC      ,
     3                 GAMA       ,GAM(3)     ,DUM(24)    ,MCBXX(7)
      COMMON /PACKX /  TYPIN      ,TYPOT      ,IRO        ,NRO       ,
     1                 INCRP
      EQUIVALENCE      (DZ(1),RZ(1),Z(1))
      DATA    SCR2  ,  SCR3,SCR4,SCR5,SCR6,SCR7,SCR8,SCR9 /
     1        302   ,  303 ,304 ,305 ,306 ,307 ,308 ,309  /
      DATA    HORG  ,  MMTX,BMTX,UPRT /  4HHORG,4HMMTX,4HBMTX,4HUPRT /
      DATA    K4MX  /  4HK4MX/,  K4GG / 110  /
      DATA    GIMS  ,  NHPDAT/ 4HGIMS,4HPDAT /
      DATA    MGG   ,  BGG   / 104,109/
      DATA    NAME  / 4 HRCOV, 4HUI   /
C
C     INITILIZE
C
      LCOREZ = KORSZ(Z) - LREQ - ICORE - 1
      IDPCOR = ICORE/2 + 1
      TFLAG  = 0
      SIGNAB = 1
      SIGNC  = 1
      MPREC  = 0
      SCRM   = 309
      REQF   = .FALSE.
      IF (LASTSS(1).EQ.FSS(1) .AND. LASTSS(2).EQ.FSS(2)) REQF = .TRUE.
C
C     GENERATE THE PARTIAL LOAD VECTOR USING THE NORMAL TRANSFORMATION
C
C     UPARTIAL = HORG*UB
C
      ITEM = HORG
      CALL MTRXI (SCR2,LASTSS,HORG,0,RC)
      IF (RC .NE. 1) GO TO 6317
C
      MCBA(1) = SCR2
      CALL RDTRL (MCBA)
      MCBB(1) = UB
      CALL RDTRL (MCBB)
      MCBC(1) = 0
      UPART   = SCR5
      CALL MAKMCB (MCBD,UPART,MCBA(3),RECT,MCBB(5))
      MPYZ    = LCOREZ
      CALL SOFCLS
      CALL MPYAD (DZ(IDPCOR),DZ(IDPCOR),DZ(IDPCOR))
      CALL WRTTRL (MCBD)
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
C
C     DETERMINE THE NUMBER OF OMITTED POINTS
C
      NROWO = MCBA(3) - MCBA(2)
      CALL SOFTRL (LASTSS,GIMS,MCBA)
      IF (MCBA(1) .EQ. 1) NROWO = MCBA(3)
C
C     GENERATE THE VELOCITIES AND ACCELERATIONS
C
      LCORE = BUF4 - ICORE - 1
      CALL RCOVVA (UPART,0,0,0,SCR7,SCR8,LASTSS,DZ(IDPCOR),DZ(IDPCOR),
     1             DZ(IDPCOR))
      IF (UPART .LE. 0) GO TO 9000
C
C     CALCULATE THE INERTIAL AND DAMPING LOADS
C
C     PID = -M*A - B*V
C
C     CALCULATE THE INERTAIL LOADS
C
      PID = 0
      IF (.NOT.REQF) GO TO 100
      MCBA(1) = MGG
      IF (MCBA(1) .GT. 0) GO TO 110
  100 CALL MTRXI (SCR2,LASTSS,MMTX,0,RC)
      IF (RC .NE. 1) GO TO 200
      MCBA(1) = SCR2
      CALL RDTRL (MCBA)
  110 MCBB(1) = SCR8
      CALL RDTRL (MCBB)
      MCBC(1) = 0
      CALL MAKMCB (MCBD,SCR6,MCBB(3),RECT,MCBB(5))
      SIGNAB = -1
      CALL SOFCLS
C
      CALL MPYAD (DZ(IDPCOR),DZ(IDPCOR),DZ(IDPCOR))
C
      DO 120 I = 1,7
  120 MCBC(I) = MCBD(I)
      PID = SCR6
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
C
C     CALCULATE THE DAMPING LOADS
C
  200 IF (RFNO .EQ. 3) GO TO 300
      IF (.NOT.REQF  ) GO TO 201
      MCBA(1) = K4GG
      CALL RDTRL (MCBA)
      IF (MCBA(1) .GT. 0) GO TO 202
  201 CALL MTRXI (SCR2,LASTSS,K4MX,0,RC)
      IF (RC .NE. 1) GO TO 209
      MCBA(1) = SCR2
      CALL RDTRL (MCBA)
  202 MCBB(1) = SCR7
      CALL RDTRL (MCBB)
      CALL MAKMCB (MCBD,SCR8,MCBB(3),RECT,MCBB(5))
      SIGNAB = -1
      CALL SOFCLS
      CALL MPYAD (DZ(IDPCOR),DZ(IDPCOR),DZ(IDPCOR))
      PID = SCR8
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
      DO 203 I = 1,7
  203 MCBC(I) = MCBD(I)
C
  209 IF (.NOT.REQF) GO TO 210
      MCBA(1) = BGG
      CALL RDTRL (MCBA)
      IF (MCBA(1) .GT. 0) GO TO 220
  210 CALL MTRXI (SCR2,LASTSS,BMTX,0,RC)
      IF (RC .NE. 1) GO TO 300
      MCBA(1) = SCR2
      CALL RDTRL (MCBA)
  220 MCBB(1) = SCR7
      CALL RDTRL (MCBB)
      CALL MAKMCB (MCBD,SCR6,MCBB(3),RECT,MCBB(5))
      SIGNAB = -1
      CALL SOFCLS
      CALL MPYAD(DZ(IDPCOR),DZ(IDPCOR),DZ(IDPCOR))
      PID = SCR6
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
C
C     PARTITION THE INERTIA AND DAMPING LOADS TO THE OMIT SET
C
C     GET THE PARTITIONING VECTOR FROM THE SOF
C
  300 IF (PID .EQ. 0) GO TO 400
      ITEM = UPRT
      CALL MTRXI (SCR2,LASTSS,UPRT,0,RC)
      IF (RC .NE. 1) GO TO 6317
      RULE = 0
      MRGZ = LCOREZ - 14
      IDP  = (ICORE+14)/2 + 1
      DO 310 I = 1,7
  310 MCB(I) = MCBD(I)
      PID  = SCR4
      CALL MAKMCB (MCB11,PID,NROWO,RECT,MCBD(5))
      MCB11(2) = MCBD(2)
      MCB12(1) = 0
      MCB21(1) = 0
      MCB22(1) = 0
C
C     SET UP A NULL ROW PARTITION VECTOR
C
      Z(ICORE) = SCR2
      CALL RDTRL (Z(ICORE))
      CALL MAKMCB (Z(ICORE+7),0,MCB(2),RECT,RSP)
      Z(ICORE+8) = 1
      CALL SOFCLS
      CALL PARTN (Z(ICORE+7),Z(ICORE),DZ(IDP))
      CALL WRTTRL (MCB11)
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
C
C     PERFORM THE FBS TO GET THE LOADS ON THE OMMITTED POINTS.  WE
C     WILL ALSO ADD IN THE EFFECTS OF THE DAMPING AND INERTIAL LOADS
C
  400 CALL RCOVUO (PID,UAO,LASTSS)
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
      IF (IOPT .LT. 0) GO TO 9000
C
C     IF RECOVERING A MODAL REDUCED SUBSTRUCTURE, CALCULATE
C     THE MODAL CORRECTION TO THE U PARTIAL
C
      DUA = 0
      IF (.NOT.MODAL) GO TO 900
C
C     IF RF-9, SPLIT THE DISPLACEMENTS FROM THE TOTAL VECTOR
C
      UAD = UPART
      IF (RFNO .NE. 9) GO TO 500
      UAD = SCR9
      CALL RCOVVA (UPART,1,0,UAD,0,0,LASTSS,DZ(IDPCOR),DZ(IDPCOR),
     1             DZ(IDPCOR))
C
C     PARTITION THE PARTIAL DISPLACEMENTS TO THE OMITTED AND
C     BOUNDARY SIZES
C
  500 ITEM = UPRT
      CALL MTRXI (SCR2,LASTSS,UPRT,0,RC)
      IF (RC .NE. 1) GO TO 6317
      RULE = 0
      MRGZ = LCOREZ - 14
      IDP  = (ICORE + 14)/2 + 1
      MCB(1) = UAD
      CALL RDTRL (MCB)
      CALL MAKMCB (MCB11,SCR3,NROWO,RECT,MCB(5))
      CALL MAKMCB (MCB21,SCR4,MCB(3)-NROWO,RECT,MCB(5))
      MCB11(2) = MCB(2)
      MCB21(2) = MCB(2)
      MCB12(1) = 0
      MCB22(1) = 0
C
      Z(ICORE) = SCR2
      CALL RDTRL (Z(ICORE))
      CALL MAKMCB (Z(ICORE+7),0,MCB(2),RECT,RSP)
      Z(ICORE+8) = 1
      CALL SOFCLS
C
      CALL BUG (NHPDAT,500,MCB(1),37)
      CALL PARTN (Z(ICORE+7),Z(ICORE),DZ(IDP))
      CALL WRTTRL (MCB11)
      CALL WRTTRL (MCB21)
C
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
C
C     CALCULATE THE CORRECTION TERMS
C
C     DUO = GI*UB - UO
C
      ITEM = GIMS
      CALL MTRXI (SCR6,LASTSS,GIMS,0,RC)
      IF (RC .NE. 1) GO TO 6317
      MCBA(1) = SCR6
      CALL RDTRL (MCBA)
      DO 520 I = 1,7
      MCBB(I) = MCB21(I)
      MCBC(I) = MCB11(I)
  520 CONTINUE
      CALL MAKMCB (MCBD,SCR9,MCBA(3),RECT,MCBB(5))
      SIGNAB = 1
      SIGNC  =-1
      TFLAG  = 0
      SCRM   = 308
      MPREC  = 0
      CALL SOFCLS
      MPYZ   = MRGZ
      CALL MPYAD (DZ(IDP),DZ(IDP),DZ(IDP))
      CALL WRTTRL (MCBD)
C
C     MERGE DUO TO -A- SIZE
C
      DO 540 I = 1,7
  540 MCB11(I) = MCBD(I)
      MCB21(1) = 0
      DUA = SCR4
      CALL MAKMCB (MCB,DUA,Z(ICORE+2),RECT,MCB11(5))
      MCB(2) = MCBD(2)
      IF (RFNO .EQ. 9) MCB(2) = 3*MCBD(2)
C
C     SET UP A NULL ROW PARTITIONING VECTOR (OR FOR RF-9)
C     SET UP A VECTOR THAT WILL MERGE IN A NULL VELOCITY AND
C     ACCELERATION VECTOR FOR EACH DISPLACEMENT VECTOR
C
      NRO = MCB(2)
      CALL MAKMCB (Z(ICORE+7),SCR3,NRO,RECT,RSP)
      IF (NRO+15 .GT. LCOREZ) GO TO 9008
      DO 550 I = 1,NRO
  550 RZ(ICORE+14+I) = 0.0
      IF (RFNO .NE. 9) GO TO 570
      DO 560 I = 1,NRO,3
      RZ(ICORE+15+I) = 1.0
  560 RZ(ICORE+16+I) = 1.0
  570 CONTINUE
      CALL GOPEN (SCR3,Z(BUF1),WRTREW)
      TYPIN = 1
      TYPOT = 1
      IRO   = 1
      INCRP = 1
      CALL PACK (Z(ICORE+15),SCR3,Z(ICORE+7))
      CALL CLOSE (SCR3,REW)
      CALL WRTTRL (Z(ICORE+7))
      CALL MERGE (Z(ICORE+7),Z(ICORE),DZ(IDP))
      CALL WRTTRL (MCB)
C
C     ADD THE PARTIAL DISPLACEMENT VECTOR TO THE DISPLACEMENTS FROM
C     THE OMITS, INERTIAL, DAMPING, AND MODAL CORRECTION EFFECTS
C     TO GET THE FINAL DISPLACEMENT VECTOR FOR THIS SUBSTRUCTURE
C
  900 NOMAT = 2
      IF (DUA .NE. 0) NOMAT = 3
      TYPA  = 1
      ALPHA = 1.0
      MCBAA(1) = UPART
      CALL RDTRL (MCBAA)
      TYPB = 1
      BETA = 1.0
      MCBBB(1) = UAO
      CALL RDTRL (MCBBB)
      IF (DUA .EQ. 0) GO TO 910
      TYPC = 1
      GAMA = 1.0
      MCBCC(1) = DUA
      CALL RDTRL (MCBCC)
  910 CALL MAKMCB (MCBXX,UA,MCBAA(3),RECT,MCBAA(5))
      MCBXX(2) = MCBAA(2)
      LCOR = LCOREZ
      CALL SOFCLS
      CALL SADD (DZ(IDPCOR),DZ(IDPCOR))
      CALL WRTTRL (MCBXX)
C
C     NORMAL RETURN
C
      SIGNAB = 1
      RETURN
C
C     ERROR MESSAGES
C
 6317 IF (RC .EQ. 2) RC = 3
      CALL SMSG (RC-2,ITEM,LASTSS)
 9000 IOPT = -1
      RETURN
C
 9008 CALL MESAGE (8,0,NAME)
      GO TO 9000
      END
