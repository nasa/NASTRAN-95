      SUBROUTINE RCOVUO (PID,UAO,LASTSS)
C
C     THIS SUBROUTINE CALCULATES THE FULL SIZE DISPLACEMENT VECTOR ON
C     ANY OMITTED POINTS.  THE OPTIONAL INERTIA AND DAMPING EFFECTS
C     WILL BE INCLUDED IF REQUESTED.
C
C     FILE USAGE IS AS FOLLOWS
C
C     SCR1 AND SCR5 ARE NOT USED
C     SCR4 CONTAINS PID ON INPUT AND IS DESTROYED
C     SCR7 CONTAINS UAO OUTPUT
C     ALL OTHER SCRATCH FILES ARE USED
C
      INTEGER          RULE       ,PAO        ,BUF1       ,PID        ,
     1                 RC         ,UAO        ,TYPIN      ,TYPOT      ,
     2                 POVE       ,DRY        ,STEP       ,FSS        ,
     3                 RFNO       ,UINMS      ,UA         ,LASTSS(2)  ,
C    4                 SOLN       ,SRD        ,SWRT       ,SCHK       ,
     5                 IZ(1)      ,RD         ,RDREW      ,WRT        ,
     6                 WRTREW     ,REW        ,EOFNRW     ,RSP        ,
     7                 RDP        ,CSP        ,CDP        ,SQUARE     ,
     8                 RECT       ,DIAG       ,UPPER      ,LOWER      ,
     9                 SYM        ,SCRA       ,SCRB       ,SDCMPZ     ,
     O                 SCR4       ,NAME(2)    ,POWER      ,FILE       ,
     1                 MCBPAO(7)  ,SCR3       ,UMCB       ,BMCB       ,
     2                 SCRC       ,CHLSKY     ,XMCB       ,FBSZ       ,
     3                 PREC       ,SIGN       ,SCR2       ,
     4                 UPRT       ,SCR7       ,SCR6       ,SCR8       ,
     5                 SOF1       ,SOF2       ,SOF3       ,SCR9       ,
     6                 TYPA       ,TYPB
      DOUBLE PRECISION DZ(1)      ,DET        ,DETI       ,MINDIA
      CHARACTER        UFM*23     ,UWM*25     ,UIM*29     ,SFM*25     ,
     1                 SWM*27
      COMMON /XMSSG /  UFM        ,UWM        ,UIM        ,SFM        ,
     1                 SWM
      COMMON /BLANK /  DRY        ,LOOP       ,STEP       ,FSS(2)     ,
     1                 RFNO       ,NEIGV      ,LUI        ,UINMS(2,5) ,
     2                 NOSORT     ,UTHRES     ,PTHRES     ,QTHRES
      COMMON /RCOVCR/  ICORE      ,LCORE      ,BUF1       ,BUF2       ,
     1                 BUF3       ,BUF4       ,SOF1       ,SOF2       ,
     2                 SOF3
      COMMON /RCOVCM/  MRECVR     ,UA         ,PA         ,QA         ,
     1                 IOPT       ,RSS(2)     ,ENERGY     ,UIMPRO     ,
     2                 RANGE(2)   ,IREQ       ,LREQ       ,LBASIC
      COMMON /SYSTEM/  SYSBUF     ,NOUT
      COMMON /SADDX /  NOMAT      ,LCOR       ,MCBAA(7)   ,TYPA       ,
     1                 ALPHA      ,ALP(3)     ,MCBBB(7)   ,TYPB       ,
     2                 BETA       ,BET(3)     ,DUM(36)    ,MCBXX(7)
      COMMON /NAMES /  RD         ,RDREW      ,WRT        ,WRTREW     ,
     1                 REW        ,NOREW      ,EOFNRW     ,RSP        ,
     2                 RDP        ,CSP        ,CDP        ,SQUARE     ,
     3                 RECT       ,DIAG       ,UPPER      ,LOWER      ,
     4                 SYM
      COMMON /PACKX /  TYPIN      ,TYPOT      ,IRO        ,NRO        ,
     1                 INCRP
      COMMON /PARMEG/  MCBK(7)    ,MCBK11(7)  ,MCBK21(7)  ,MCBK12(7)  ,
     1                 MCBK22(7)  ,MRGZ       ,RULE
      COMMON /SFACT /  MCBA(7)    ,MCBL(7)    ,MCBLT(7)   ,SCRA       ,
     1                 SCRB       ,SDCMPZ     ,DET        ,DETI       ,
     2                 POWER      ,SCRC       ,MINDIA     ,CHLSKY
      COMMON /FBSX  /  LMCB(7)    ,UMCB(7)    ,BMCB(7)    ,XMCB(7)    ,
     1                 FBSZ       ,PREC       ,SIGN
      COMMON /ZZZZZZ/  Z(1)
      EQUIVALENCE      (Z(1),IZ(1),DZ(1))
      DATA    NAME  /  4HRCOV,4HUO   /
      DATA    POVE  ,  LMTX / 4HPOVE,4HLMTX /
      DATA    UPRT  ,  KMTX / 4HUPRT,4HKMTX /
      DATA    SCR2  ,  SCR3,SCR4,SCR6,SCR7,SCR8,SCR9 /
     1        302   ,  303 ,304 ,306 ,307 ,308 ,309  /
C
C     SET UP COMMON BLOCKS
C
      LCOREZ = KORSZ(Z) - LREQ - ICORE - 1
      IDPCOR = ICORE/2 + 1
      RULE   = 0
      MCBK21(1) = 0
      MCBK12(1) = 0
      MCBK22(1) = 0
      SIGN   = 1
C
C     CALCUATE THE LOADS ON THE OMMITED POINTS
C
      PAO = 0
      IF (RFNO .EQ. 3) GO TO 10
      PAO = SCR3
      CALL RCOVSL (LASTSS,POVE,0,SCR6,SCR7,SCR8,PAO,Z(ICORE),Z(ICORE),
     1             SOF3-ICORE-1,.FALSE.,RFNO)
      MCBPAO(1) = PAO
      CALL RDTRL (MCBPAO)
C
C     ADD IN OPTIONAL INERTIA AND DAMPING FORCES TO THE LOADS ON THE
C     OMMITED POINTS
C
  10  IF (PID .EQ. 0) GO TO 200
      IF (PAO .EQ. 0) GO TO 120
      NOMAT = 2
      TYPA  = 1
      ALPHA = 1.0
      MCBAA(1) = PID
      CALL RDTRL (MCBAA)
      TYPB  = 1
      BETA  = 1.0
      MCBBB(1) = PAO
      CALL RDTRL (MCBBB)
      CALL MAKMCB (MCBXX,SCR6,MCBAA(3),RECT,MCBAA(5))
      MCBXX(2) = MCBAA(2)
      LCOR  = LCOREZ
      CALL SOFCLS
      CALL SADD (DZ(IDPCOR),DZ(IDPCOR))
      CALL WRTTRL (MCBXX)
      DO 110 I = 1,7
  110 MCBPAO(I) = MCBXX(I)
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
      GO TO 200
C
C     NO STATIC LOADS SO THE ADD IS UNECESSARY
C
  120 MCBPAO(1) = PID
      CALL RDTRL (MCBPAO)
C
  200 IF (MCBPAO(1) .LE. 0) GO TO 500
C
C     CHECK FOR EXISTENCE OF LMTX ON THE SOF.  IF IT EXISTS
C     SKIP THE PARTN AND DECOMP
C
      CALL SOFTRL (LASTSS,LMTX,LMCB(1))
      IF (LMCB(1) .NE. 1) GO TO 395
C
C     BRING IN LMTX FROM SOF AND SET UP FOR FBS DIRECTLY
C
      CALL MTRXI (SCR2,LASTSS,LMTX,0,RC)
      DO 390 I = 1,7
  390 BMCB(I) = MCBPAO(I)
      LMCB(1) = SCR2
      CALL SOFCLS
      GO TO 411
C
C     COMPUTE THE KOO PARTITION OF KMTX FOR LASTSS
C
C     COPY THE PARTITIONING VECTOR TO SCR2
C
  395 CALL MTRXI (SCR2,LASTSS,UPRT,0,RC)
      ITEM = UPRT
      IF (RC .NE. 1) GO TO 6317
C
C     COPY KMTX TO SCR5
C
      ITEM = KMTX
      CALL MTRXI (SCR8,LASTSS,KMTX,0,RC)
      IF (RC .NE. 1) GO TO 6317
      MCBK(1) = SCR8
      CALL RDTRL (MCBK)
C
C     PARTITION KMTX INTO KOO.  STORE KOO ON SCR4.
C
      CALL SOFCLS
      IZ(ICORE) = SCR2
      CALL RDTRL (IZ(ICORE))
      CALL MAKMCB (MCBK11,SCR9,MCBPAO(3),SYM,MCBK(5))
      MCBK11(2) = MCBPAO(3)
      MRGZ = LCOREZ - 7
      I    = (ICORE+7)/2 + 1
      CALL PARTN (Z(ICORE),Z(ICORE),DZ(I))
      CALL WRTTRL (MCBK11)
C
C     DECOMPOSE KOO
C
      DO 400 I = 1,7
  400 MCBA(I) = MCBK11(I)
      CALL MAKMCB (MCBL,SCR2,MCBA(3),LOWER,MCBA(5))
      MCBLT(1) = SCR8
      SCRA = SCR3
      IF (SCRA .EQ. MCBPAO(1)) SCRA = SCR6
      SCRB = SCR4
      IF (SCRB .EQ. MCBPAO(1)) SCRB = SCR6
      SCRC = SCR7
      SDCMPZ = MRGZ
      POWER  = 1
      CHLSKY = 0
      CALL SDCOMP (*6311,DZ(IDPCOR),DZ(IDPCOR),DZ(IDPCOR))
      CALL WRTTRL (MCBL)
C
C     FORWARD AND BACKWARD SUBSTITUTION TO SOLVE FOR UAO
C
      DO 410 I = 1,7
      LMCB(I) = MCBL(I)
  410 BMCB(I) = MCBPAO(I)
  411 FBSZ   = LCOREZ
      MATTYP = BMCB(5)
      CALL MAKMCB (XMCB,SCR8,BMCB(3),RECT,MATTYP)
      PREC = 2 - (MATTYP-2*(MATTYP/2))
      CALL FBS (DZ(IDPCOR),DZ(IDPCOR))
      CALL WRTTRL (XMCB)
C
C     MERGE UAO INTO THE UA SET
C
C     COPY UPRT BACK TO SCR2
C
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
      ITEM = UPRT
      CALL MTRXI (SCR2,LASTSS,UPRT,0,RC)
      IF (RC .NE. 1) GO TO 6317
      CALL SOFCLS
      IZ(ICORE) = SCR2
      CALL RDTRL (IZ(ICORE))
C
C     SETUP MCB-S IN /PARMEG/
C
      DO 412 I = 1,7
  412 MCBK11(I) = XMCB(I)
      UAO = SCR7
      CALL MAKMCB (MCBK,UAO,IZ(ICORE+2),RECT,MCBK11(5))
      MCBK(2) = XMCB(2)
      IF (RFNO .EQ. 9) MCBK(2) = 3*XMCB(2)
C
C     SETUP A NULL ROW PARTITIONING VECTOR OR FOR RIGID FORMAT 9 A
C     VECTOR THAT WILL MERGE IN A NULL VELOCITY AND ACCELERATION
C     VECTOR FOR EACH DISPLACEMENT VECTOR
C
      NRO = MCBK(2)
      CALL MAKMCB (Z(ICORE+7),SCR6,NRO,RECT,RSP)
      IF (NRO+15 .GT. LCOREZ) GO TO 9008
      DO 420 I = 1,NRO
  420 Z(ICORE+14+I) = 0.0
      IF (RFNO .NE. 9) GO TO 440
      DO 430 I = 1,NRO,3
      Z(ICORE+15+I) = 1.0
  430 Z(ICORE+16+I) = 1.0
  440 CONTINUE
      CALL GOPEN (SCR6,Z(BUF1),WRTREW)
      TYPIN = 1
      TYPOT = 1
      IRO   = 1
      INCRP = 1
      CALL PACK (Z(ICORE+15),SCR6,IZ(ICORE+7))
      CALL CLOSE (SCR6,REW)
      CALL WRTTRL (IZ(ICORE+7))
C
      MRGZ = LCOREZ - 14
      I    = (ICORE+14)/2 + 1
      CALL MERGE (Z(ICORE+7),Z(ICORE),DZ(I))
      CALL WRTTRL (MCBK)
C
C     NORMAL RETURN
C
      RETURN
C
C     NO LOADS SO THE DISPLACEMENTS ARE ZERO
C
  500 UAO = 0
      CALL SOFCLS
      RETURN
C
C     ERROR PROCESSING
C
 6311 WRITE  (NOUT,6312) SWM,LASTSS
 6312 FORMAT (A27,' 6311, SDCOMP DECOMPOSITION FAILED ON KOO MATRIX ',
     1       'FOR SUBSTRUCTURE ',2A4)
      GO TO 9000
 6317 IF (RC .EQ. 2) RC = 3
      CALL SMSG (RC-2,ITEM,LASTSS)
 9000 IOPT = -1
      RETURN
C
 9008 N    = 8
      IOPT = -1
      CALL SOFCLS
      CALL MESAGE (N,FILE,NAME)
      CALL CLOSE (PAO,REW)
      CALL CLOSE (SCR3,REW)
      RETURN
      END
