      SUBROUTINE EQMCKM
C
C     THIS SUBROUTINE CALCULATES THE MPC CONSTRAINT FORCES AND CREATES
C     THE OUTPUT FILE FOR OFP.
C     TASKS INCLUDE CREATING THE SCRATCH FILES FOR THE CURRENT SUBCASES
C     (PGG, QG - ALSO USED IN EQUILIBRIUM CHECKS).
C     NOT CODED TO HANDLE CONICAL ELEMENTS OR SORT2.
C
      LOGICAL         FIRSTC,FIRSTO,LASCAS,ANYOUT
      INTEGER         NAME(2),KON(10),IDAT(3),PARM,TRL,UG,UM,UN,MCB(7),
     1                RDNRW,RDRW,WRTNRW,WRTRW,ZZ,OCB(8)
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /NAMES / RDNRW,RDRW,WRTNRW,WRTRW,KRW,KNRW,KNERW
      COMMON /SYSTEM/ KSYSTM(80)
      COMMON /BITPOS/ UM,SKPS(8),UN,UG
      COMMON /BLANK / SKPB(2),NSKIP
      COMMON /EQMK1 / KSCC,KEXIN,KGPL,KBGDT,KSIL,KUSET,KGG,KGM,KUGV,
     1                KPGG,KQG,KCSTM,KLAM,KOQM,KSCR(7),KMPC,KLOAD,KSPC,
     2                PARM(4),TRL(7)
CZZ   COMMON /ZZSSA2/ ZZ(1)
      COMMON /ZZZZZZ/ ZZ(20000)
      COMMON /UNPAKX/ ITYPU,INRU,ILRU,INCU
      COMMON /MPYADX/ MA(7),MB(7),MC(7),MD(7),MZ,MT,MSAB,MSC,MPR,MSCR
      COMMON /PATX  / LCOR,INS(3),LUSET
      EQUIVALENCE     (MCB(1),OCB(1)),(KSYSTM(1),ISBZ),(KSYSTM(2),NOUT),
     1                (KSYSTM(15),ITIM),(KSYSTM(16),IDAT(1))
CWKBI 3/94 SPR93007      
      EQUIVALENCE     (KSYSTM(55),IPREC)
      DATA    NAME  / 4HEQMC,2HKM /
      DATA    KON   / 1,20,0,-1,0,0,0,0,1,8 /
      DATA    KG    / 1HG   /
C
C                      KNG
C     PARTITION  KGG = ---- , ONLY KMG SAVED
C                      KMG
      ANYOUT =.FALSE.
      NZZ    = KORSZ (ZZ(1))
      LCOR   = NZZ
      LUSET  = KUSET
      IF (KMPC .EQ. 0) GO TO 10
      KMPC   = -1
      CALL CALCV (KSCR(1),UG,UN,UM,ZZ)
      CALL SSG2A (KGG,0,KSCR(2),KSCR(1))
C
C     UNAPPEND FILES
C
   10 CONTINUE
      NZZ3 = NZZ - 3*ISBZ + 1
      NZZ2 = NZZ3 + ISBZ
      NZZ1 = NZZ2 + ISBZ
      NZZ4 = NZZ3
      IF (NSKIP.LE.0) NZZ4 = NZZ3 - ISBZ
C
      IF (NSKIP .LE. 1) GO TO 30
C
      IF (KLOAD .LE. 0) GO TO 20
      TRL(1) = KPGG
      MCB(1) = KSCR(4)
      CALL CURCAS (*15,NSKIP,TRL,MCB,ZZ,NZZ2)
      KPGG   = MCB(1)
       GO TO 20
   15 KLOAD  = 0
C
   20 IF (KUGV .LT. 0) GO TO 30
      TRL(1) = KUGV
      MCB(1) = KSCR(3)
      CALL CURCAS (*345,NSKIP,TRL,MCB,ZZ,NZZ2)
      KUGV   = MCB(1)
   30 CONTINUE
      IF (KMPC .EQ. 0) GO TO 360
C
C                      PN
C     PARTITION  PGG = --- , ONLY PM SAVED
C                      PM
      IF (KLOAD.GT.0) CALL SSG2A (KPGG,0,KSCR(7),KSCR(1))
C
C                 M
C     MULTIPLY  QM = -PM + KMG*UGV
C
      MD(1) = KSCR(5)
      MC(1) = KSCR(7)
      CALL RDTRL (MC)
      IF (KLOAD .LE. 0) MC(1) = 0
      MA(1) = KSCR(2)
      MB(1) = KUGV
      CALL RDTRL (MA)
      CALL RDTRL (MB)
      MD(3) = MA(3)
      MD(4) = MB(4)
CWKBR 11/93 SPR93007      MD(5) = 1
      MD(5) = IPREC
      MZ    = NZZ
      MT    = 0
      MSAB  = 1
      MSC   =-1
CWKBR 11/93 SPR93007      MPR   = 1
      MPR   = IPREC
      MSCR  = KSCR(1)
      CALL MPYAD (ZZ,ZZ,ZZ)
      IF (MD(3) .EQ. MD(2)) MD(4) = 1
      CALL WRTTRL (MD)
C
C                 N       T   M
C     MULTIPLY  QM = - GM * QM
C
      MD(1) = KSCR(6)
      MC(1) = 0
      MA(1) = KGM
      MB(1) = KSCR(5)
      CALL RDTRL (MA)
      CALL RDTRL (MB)
      MD(3) = MA(2)
      MD(4) = MB(4)
CWKBR SPR93007      MD(5) = 1
      MD(5) = IPREC
      MT    = 1
      MSAB  =-1
      CALL MPYAD (ZZ,ZZ,ZZ)
      IF (MD(3) .EQ. MD(2)) MD(4) = 1
      CALL WRTTRL (MD)
C
C             N      M
C     MERGE QM AND QM ON SCRATCH 3
C
      TRL(1) = KSCR(5)
      CALL RDTRL (TRL)
      IF (TRL(1) .LT. 0) GO TO 345
      KMPC   = 1
C
      CALL SDR1B (KSCR(1),KSCR(6),KSCR(5),KSCR(3),UG,UN,UM,KUSET,0,0)
      TRL(1) = KSCR(3)
      CALL RDTRL (TRL)
      MSZE   = 2*TRL(3)
C
C     CREATE MPC-CONSTRAINT OUTPUT FILE
C
      IF (KOQM .LE. 0) GO TO 360
      IAPP = 10
      IF (NSKIP .LT. 0) IAPP = 20
      CALL MXCID (*345,ZZ,KG,MSZE/2,2,KUSET,KGPL,KSIL,NZZ2)
      DO 40 I = 2,MSZE,2
   40 ZZ(I) = I/2
C
C     SORT ON EXTERNAL ID
C
      NENT = 2
      IF (MSZE .EQ. 2) GO TO 70
C
      IFIL = KSIL
      DO 60 I = 3,MSZE,2
      IF (ZZ(I) .GT. ZZ(I-2)) GO TO 50
      TRL(1) = ZZ(I)
      TRL(2) = ZZ(I+1)
      CALL BISHEL (*410,TRL,NENT,2,ZZ(1))
       GO TO 60
   50 NENT   = NENT + 2
   60 CONTINUE
C
   70 CONTINUE
      LKSCC  = MSZE + 1
      IF (LKSCC+146 .GE. NZZ4) GO TO 420
      IVEC   = 0
      TRL(1) = KSCR(3)
      CALL RDTRL (TRL)
      NVEC   = TRL(2)
      ITYPU  = 1
      INCU   = 1
      ISDONE = 0
      LASCAS = .FALSE.
      CALL GOPEN (KSCC,ZZ(NZZ1),RDRW)
      CALL GOPEN (KSCR(3),ZZ(NZZ3),RDRW)
C
      IF (NSKIP .GT. 0) GO TO 90
C
C     POSITION LAMA
C
      IFIL   = KLAM
      CALL OPEN (*350,KLAM,ZZ(NZZ4),RDRW)
      CALL READ (*440,*450,KLAM,0,0,1,I)
      CALL READ (*440,*450,KLAM,0,0,1,I)
   90 IFIL   = KOQM
      CALL OPEN (*430,KOQM,ZZ(NZZ2),WRTRW)
      CALL FNAME (KOQM,TRL(1))
      TRL(3) = ITIM
      TRL(4) = IDAT(1)
      TRL(5) = IDAT(2)
      TRL(6) = IDAT(3)
      TRL(7) = 1
      CALL WRITE (KOQM,TRL(1),7,1)
C
C     POSITION CASECC.  ASSUME USER WILL MISSET NSKIP
C
      IF (NSKIP .LE. 1) GO TO 100
      J    = NSKIP - 1
      IFIL = KSCC
      DO 95 I = 1,J
   95 CALL FWDREC (*440,KSCC)
C
C     LOOP ON EACH VECTOR
C
  100 IVEC = IVEC + 1
C
C     SUBCASE ID
C
      CALL READ (*160,*160,KSCC,ZZ(LKSCC),38,0,I)
      ISB = ZZ(LKSCC  )
      ILD = ZZ(LKSCC+3)
      IEG = 0
C
C     CLEAN UP UNUSED WORDS
C
      I = LKSCC + 10
      J = LKSCC + 49
      DO 105 K = I,J
  105  ZZ(K) = 0
C
C     TITLES
C
      CALL FREAD (KSCC,ZZ(LKSCC+50),96,0)
      CALL FREAD (KSCC,0,-31,0)
      CALL FREAD (KSCC,LCC,1,0)
      CALL FREAD (KSCC,0,-6,0)
C
C     MPCFORCE REQUEST
C
      NGSET = 0
      LSETD = LKSCC + 146
      CALL FREAD (KSCC,INS(1),3,0)
      IF (INS(1)) 110,120,130
C
C     ALL REQUESTED
C
  110 CALL FREAD (KSCC,0,0,1)
       GO TO 180
C
C     NONE REQUESTED
C
  120 IFIL = KLAM
      CALL FREAD (KSCC,0,0,1)
      IF (NSKIP .GT. 0) GO TO 240
      CALL READ (*350,*350,KLAM,TRL(1),7,0,I)
       GO TO 240
C
C     SET REQUESTED
C
  130 CONTINUE
      CALL FREAD (KSCC,0,-LCC+176,0)
C
C     SKIP SYMMETRY SEQUENCE
C
      CALL FREAD (KSCC,I,1,0)
      IF (I .LE. 0) GO TO 140
      CALL FREAD (KSCC,0,-I,0)
  140 IFIL = KSCC
      CALL READ (*440,*450,KSCC,TRL(1),2,0,I)
      IF (TRL(1) .EQ. INS(1)) GO TO 150
      CALL FREAD (KSCC,0,-TRL(2),0)
      GO TO 140
  150 IF (LSETD+TRL(2).GT.NZZ4) GO TO 420
      NGSET = TRL(2)
      CALL FREAD (KSCC,ZZ(LSETD),NGSET,1)
      GO TO 180
C
C     EOF ON CASE CONTROL.  CHECK IF REALLY DONE
C
  160 CONTINUE
      IF (NSKIP .LT.   0) GO TO 170
      IF (IVEC .GT. NVEC) GO TO 350
      IFIL = KSCC
      GO TO 440
C
  170 IF (IVEC .GT. NVEC) GO TO 350
      LASCAS = .TRUE.
      IVEC   = IVEC - 1
C
C     INITIALIZE
C
  180 CONTINUE
      IF (LASCAS) IVEC = IVEC + 1
      FIRSTC = .TRUE.
      FIRSTO = .TRUE.
      IF (NSKIP .GT. 0) GO TO 190
      CALL READ (*235,*235,KLAM,TRL(1),7,0,I)
      ILD = TRL(1)
      IEG = TRL(3)
  190 MT  = LKSCC - 1
      DO 200 J = 1,10
      I   = J + MT
  200 ZZ(I) = KON(J)
C
      IF (INS(3) .EQ. 1) GO TO 210
      CALL PAGE2 (2)
      WRITE  (6,205) UWM,NAME
  205 FORMAT (A25,' 2373, ONLY SORT1-REAL SUPPORTED IN ',2A4)
  210 ZZ(MT+1) = INS(2) + IAPP
      ZZ(MT+4) = ISB
      ZZ(MT+5) = ILD
      ZZ(MT+6) = IEG
      LVEC = LSETD + NGSET - 1
C
C     LOOP ON POINT DATA
C   MT = POINTER TO MATCID GRID ID, MS = POINTER TO CASECC GRID REQUEST.
C
      IDG = -1
      MT  = -1
      MS  = LSETD
  220 MT  = MT + 2
      IF (MT .GT. MSZE) GO TO 240
      IF (ZZ(MT)/10 .EQ. IDG) GO TO 220
      IDG = ZZ(MT)/10
      IF (INS(1) .LT. 0) GO TO 300
C
C     LOCATE POINT IN SET
C
  221 I = ZZ(MS)
  222 IF (MS-LVEC) 223,228,240
  223 IF (IDG-I) 230,300,224
  224 I = ZZ(MS+1)
      IF (I) 225,227,227
  225 IF (IDG+I) 300,300,226
  226 MS = MS+2
      GO TO 221
  227 MS = MS+1
      GO TO 222
C
C     LAST POINT IN SET
C
  228 IF (I.LT.0 .AND. IDG+I.LE.0) GO TO 300
      IF (IDG-I) 230,300,230
C
C     NOT IN SET
C
  230 IF (MT+2 .LT. LKSCC) GO TO 220
      GO TO 240
C
C     END-OF-FILE
C
  235 ISDONE = 1
C
C     NO MORE GRIDS IN THIS SET
C
  240 CONTINUE
      IF (.NOT.FIRSTO) CALL WRITE (KOQM,0,0,1)
      IF (IVEC+1 .GT. NVEC) GO TO 350
      IF (ISDONE .NE.    0) GO TO 350
C
C     CHECK IF COLUMN NEEDS TO BE SKIPPED
C
      IFIL = KSCR(3)
      IF (FIRSTC) CALL FWDREC (*440,KSCR(3))
C
      IF (LASCAS) GO TO 180
      GO TO 100
C
C     PROCESS THE GRID FOR OUTPUT
C
  300 MCB(1) = 10*IDG + INS(2)
      IF (.NOT.FIRSTC) GO TO 310
      IF (LVEC+MSZE/2 .GT. NZZ4) GO TO 420
      INRU = 1
      ILRU = MSZE/2
      FIRSTC = .FALSE.
      CALL UNPACK (*240,KSCR(3),ZZ(LVEC+1))
C
  310 CONTINUE
      L      = 1
      NENT   = 0
      OCB(3) = 0
      OCB(4) = 0
      OCB(5) = 0
      OCB(6) = 0
      OCB(7) = 0
      OCB(8) = 0
      M = MIN0(MT+10,MSZE)
      DO 330 I = MT,M,2
      J = ZZ(I)/10
      IF (J .NE. IDG) GO TO 335
      K = ZZ(I+1) + LVEC
      J = ZZ(I) - J *10 + 2
      IF (J .GT. 2) GO TO 320
C
C     SCALAR
C
      L = 2
      J = 3
C
  320 OCB(J) = ZZ(K)
      IF (ZZ(K) .NE. 0) NENT = NENT + 1
  330 CONTINUE
C
  335 OCB(2) = L
      IF (NENT .EQ. 0) GO TO 220
      IF (.NOT.FIRSTO) GO TO 340
C
C     WRITE OUT CONTROL RECORD (ODD NUMBER)
C
      ANYOUT = .TRUE.
      CALL WRITE (KOQM,ZZ(LKSCC),146,1)
      FIRSTO = .FALSE.
C
C     WRITE AN ENTRY OUT
C
  340 CALL WRITE (KOQM,OCB(1),8,0)
      GO TO 220
C
C     CLOSE FILES
C
  345 CALL CLOSE (KOQM,KRW)
      KOQM = -1
      ANYOUT = .FALSE.
  350 CALL CLOSE (KSCR(3),KRW)
      CALL CLOSE (KSCC,KRW)
      CALL CLOSE (KLAM,KRW)
      IF (ANYOUT) CALL EOF (KOQM)
      CALL CLOSE (KOQM,KRW)
      IF (.NOT.ANYOUT) GO TO 360
      TRL(1) = KOQM
      TRL(2) = NVEC
      TRL(3) = MSZE/2
      TRL(4) = 0
      TRL(5) = 0
      TRL(6) = 1
      TRL(7) = 0
      CALL WRTTRL (TRL)
  360 CONTINUE
C
C     CALCULATE UPDATED QG FILE - SCRATCH 5
C
      IF (KSPC  .EQ. 0) GO TO 405
      IF (NSKIP .LE. 1) GO TO 405
      TRL(1) = KQG
      MCB(1) = KSCR(5)
      CALL CURCAS (*407,NSKIP,TRL,MCB,ZZ,NZZ2)
      KQG = MCB(1)
C
  405 RETURN
C
C     ERROR CONDITIONS
C
C     KQG BAD
C
  407 KSPC = -1
      GO TO 405
C
  410 I = 7
      GO TO 490
  420 I = 8
      IFIL = NZZ4
      GO TO 490
  430 I = 1
      GO TO 490
  440 I = 2
      GO TO 490
  450 I = 3
  490 CALL MESAGE (I,IFIL,NAME)
C
C     MPC OUTPUT FILE NOT CREATED, BUT DATA IS ON SCR3 FOR EQMCKS
C
      CALL PAGE2 (2)
      WRITE  (NOUT,510) UWM,NAME
  510 FORMAT (A25,' 2380, MULTI-POINT CONSTRAINT FORCES NOT OUTPUT IN ',
     1        A4,A2,', SEE QUEUED MESSAGES.')
      GO TO 345
      END
