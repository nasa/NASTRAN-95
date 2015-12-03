      SUBROUTINE DPD4
C
C     DPD4 ASSEMBLES THE NON-LINEAR FORCING TABLE (NLFT)
C     AND THE TRANSIENT RESPONSE LIST (TRL).
C
      EXTERNAL        ANDF
      INTEGER         GPL   ,SIL   ,USET  ,USETD ,GPLD  ,SILD  ,DPOOL ,
     1                DLT   ,FRL   ,TFL   ,TRL   ,PSDL  ,EED   ,SCR1  ,
     2                SCR2  ,SCR3  ,SCR4  ,BUF   ,BUF1  ,BUF2  ,BUF3  ,
     3                BUF4  ,FLAG  ,FILE  ,EPOINT,SEQEP ,Z     ,LOADS ,
     5                ANDF  ,DLOAD ,FREQ1 ,FREQ  ,TIC   ,TSTEP ,TF    ,
     6                PSD   ,EIGR  ,EIGB  ,EIGC  ,NGRID ,EQDYN ,SDT   ,
     7                UD    ,UE    ,TWO
      DIMENSION       BUF(24)   ,EPOINT(2)    ,SEQEP(2)     ,MCB(7)   ,
     1                NAM(2)    ,LOADS(32)    ,DLOAD(2)     ,FREQ1(2) ,
     2                FREQ(2)   ,ZZ(1)        ,BUFR(20)     ,NOLIN(21),
     3                TIC(2)    ,TSTEP(2)     ,TF(2)        ,PSD(2)   ,
     4                MSG(3)    ,EIGR(2)      ,EIGB(2)      ,EIGC(2)
      COMMON /BLANK / LUSET ,LUSETD,NOTFL ,NODLT ,NOPSDL,NOFRL ,NONLFT,
     1                NOTRL ,NOEED
      COMMON /NAMES / RD    ,RDREW ,WRT   ,WRTREW,CLSREW
      COMMON /DPDCOM/ DPOOL ,GPL   ,SIL   ,USET  ,GPLD  ,SILD  ,USETD ,
     1                DLT   ,FRL   ,NLFT  ,TFL   ,TRL   ,PSDL  ,EED   ,
     2                SCR1  ,SCR2  ,SCR3  ,SCR4  ,BUF   ,BUF1  ,BUF2  ,
     3                BUF3  ,BUF4  ,EPOINT,SEQEP ,L     ,KN    ,NEQDYN,
     4                LOADS ,DLOAD ,FREQ1 ,FREQ  ,NOLIN ,NOGO  ,
     5                MSG   ,TIC   ,TSTEP ,TF    ,PSD   ,EIGR  ,EIGB  ,
     6                EIGC  ,MCB   ,NAM   ,EQDYN ,SDT   ,INEQ
      COMMON /TWO   / TWO(32)
      COMMON /BITPOS/ UM    ,UO    ,UR    ,USG   ,USB   ,UL    ,UA    ,
     1                UF    ,US    ,UN    ,UG    ,UE    ,UP    ,UNE   ,
     2                UFE   ,UD
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (Z(1) ,ZZ(1)),(BUF(1),BUFR(1)),(MSG(2),NGRID)
      DATA            NOLINR/ 7 /
C
C     INITIALIZE POINTERS. OPEN SCR1. OPEN DYNAMICS POOL.
C
      INOLIN = NEQDYN + 2
      J = INOLIN
      MSKUD = TWO(UD)
      MSKUE = TWO(UE)
      INCORE = 0
      II = 1
      I  = 1
      MSG(1) = 67
      CALL PRELOC (*2001,Z(BUF1),DPOOL)
      CALL OPEN (*2001,SCR1,Z(BUF2),WRTREW)
      INEQ = 0
C
C     LOCATE NOLINI CARD. IF PRESENT, TURN NONLFT FLAG OFF,
C
 1320 CALL LOCATE (*1358,Z(BUF1),NOLIN(I),FLAG)
      NONLFT = 1
      NWDIN  = NOLIN(I+2)
C
C     READ A NOLINI CARD. CONVERT POINTS ON CARD TO SIL NOS.
C     STORE DATA IN CORE. IF SPILL, WRITE ON SCRATCH FILE.
C
 1340 CALL READ (*2002,*1358,DPOOL,BUF,NWDIN,0,FLAG)
      MSG(3) = 100000000*II + BUF(1)
      IF (II .GE. 5) IF (II-6)  1350,  1354,  1342
C                             NOLIN5,NFTUBE,NOLIN6
      III = II
      IF (BUF(6) .LT. 10) GO TO 1341
      III = II + 4
      BUF(6) = BUF(6) - 10
 1341 IF (II .NE. 2) GO TO 1343
      IF (BUF(8) .LT. 10) GO TO 1343
      BUF(8) = BUF(8) - 10
      IF(III.EQ.2) III = 10
      IF(III.EQ.6) III = 9
      GO TO 1343
 1342 III = 13
      IF (BUF(6) .LT. 10) GO TO 1343
      III = 14
      BUF(6) = BUF(6) - 10
 1343 L = 2
      CALL DPDAA
      BUF(3) = BUF(2)
      L = 5
      CALL DPDAA
      L = 7
      IF (II .EQ. 2) CALL DPDAA
      BUF(6) = BUF(7)
      BUF(2) = III
 1344 NN = 6
 1345 IF (INCORE  .NE. 0) GO TO 1348
      IF (J+NN .GE. BUF2) GO TO 1347
      DO 1346 K = 1,NN
      Z(J) = BUF(K)
 1346 J = J + 1
      GO TO 1340
 1347 CALL WRITE (SCR1,Z(INOLIN),J-INOLIN,0)
      INCORE = 1
 1348 CALL WRITE (SCR1,BUF,NN,0)
      GO TO 1340
C
C     SPECIAL HANDLING OF NOLIN5 CARD
C     CARD FORMAT AS RECEIVED FROM IFP
C        SID  AA   AB   FAB  EA/TEA  EB/TEB  ALPA/TALPA  ALPB/TALPB
C        GA1  GA2  GA3  GA4  GB1     GB2     GB3         GB4
C
C     WE CONVERT THIS CARD INTO THE FOLLOWING 6-WORD ENTRY FORMAT
C
C        SID  12  SILA1  AA          SILA2  AB
C        SID  12  SILA3  FAB         SIL4   0
C        SID  12  SILB1  EA/TEA      SILB2  EB/TEB
C        SID  12  SILB3  ALPA/TALPA  SILB4  ALPB/TALPB
C
 1350 L = 23
      KK= 16
      DO 1351 K = 1,8
      BUF(L+1) = 0
      BUF(L  ) = BUF(KK)
      IF (BUF(L) .NE. 0) CALL DPDAA
      KK = KK - 1
 1351 L  = L -2
      BUF(24) = BUF( 8)
      BUF(22) = BUF( 7)
      BUF(18) = BUF( 6)
      BUF(16) = BUF( 5)
      BUF(12) = 0
      BUF(10) = BUF( 4)
      BUF( 6) = BUF( 3)
      BUF( 4) = BUF( 2)
      BUF( 3) = BUF( 9)
      BUF( 5) = BUF(11)
      BUF( 9) = BUF(13)
      BUF(11) = BUF(15)
      BUF(17) = BUF(19)
      DO 1352 K = 1,24,6
      BUF(K  ) = BUF(1)
 1352 BUF(K+1) = 12
      NN = 24
      GO TO 1345
C
 1354 L = 7
      BUF(7) = BUF(2)
      BUF(8) = 1
      CALL DPDAA
      BUF(3) = BUF(7)
      BUF(7) = BUF(3)
      BUF(8) = 1
      CALL DPDAA
      BUF(5) = BUF(7)
      BUF(6) = BUF(5)
      BUF(2) = 11
      MSG(3) = BUF(1)
      GO TO 1344
C
C     HERE WHEN ALL CARDS OF CURRENT TYPE HAVE BEEN READ.
C     TEST FOR ALL CARDS READ.
C
 1358 I  = I + 3
      II = II+ 1
      IF (II .LE. NOLINR) GO TO 1320
      CALL WRITE (SCR1,0,0,1)
      CALL CLOSE (SCR1,CLSREW)
      IF (NONLFT .EQ. -1) GO TO 1400
C
C     SORT THE DATA ON SET ID.
C
      IF (INCORE .NE. 0) GO TO 1362
      NNOLIN = J - 6
      N = J - INOLIN
      GO TO 1364
 1362 CALL OPEN (*2001,SCR1,Z(BUF2),RDREW)
      CALL READ (*2002,*1363,SCR1,Z,BUF1,1,N)
      CALL MESAGE (-8,0,NAM)
 1363 CALL CLOSE (SCR1,CLSREW)
      INOLIN = 1
      NNOLIN = N - 5
 1364 CALL SORT (0,0,6,1,Z(INOLIN),N)
C
C     READ USETD INTO CORE.
C
      FILE = USETD
      CALL OPEN (*2001,USETD,Z(BUF2),RDREW)
      CALL FWDREC (*2002,USETD)
      IUSETD = NNOLIN + 7
      CALL READ (*2002,*1365,USETD,Z(IUSETD),BUF2-IUSETD,1,N)
      CALL MESAGE (-8,0,NAM)
 1365 CALL CLOSE (USETD,CLSREW)
C
C     OPEN THE NLFT. WRITE SET IDS IN HEADER RECORD.
C
      FILE = NLFT
      CALL OPEN (*1392,NLFT,Z(BUF2),WRTREW)
      CALL FNAME (NLFT,BUF)
      CALL WRITE (NLFT,BUF,2,0)
      Z(NNOLIN+6) = 0
      DO 1371 I = INOLIN,NNOLIN,6
      IF (Z(I+6) .NE. Z(I)) CALL WRITE (NLFT,Z(I),1,0)
 1371 CONTINUE
      CALL WRITE (NLFT,0,0,1)
C
C     WRITE ONE RECORD PER SET. WITHIN EACH SET, SORT DATA ON SIL NO.
C     CONVERT SIL NOS. TO SIL NOS. IN UD AND UE SETS
C
      I = INOLIN
 1381 J = I
 1382 IF (Z(I+6) .NE. Z(I)) GO TO 1383
      I = I + 6
      GO TO 1382
 1383 N = I + 6 - J
C
C ... THE FOLLOWING SORT WAS REMOVED DUE TO THE INSTALLATION OF NOLIN5
C     CALL SORT (0,0,6,3,Z(J),N)
C
CWKBR SPR94005 6/94   DO 1387 KC = J,I,6                      
      DO 1387 K = J,I,6                
      BUF(1) = Z(K+1)
      BUF(2) = Z(K+2)
      BUF(4) = Z(K+3)
      BUF(5) = Z(K+4)
      BUF(8) = Z(K+5)
      BUF(9) = 0
      DO 1386 KK = 2,8,3
      IF (KK.GE.8 .AND. BUF(1).NE.2 .AND. BUF(1).NE.6 .AND. BUF(1).NE.9
     1   .AND.  BUF(1).NE.10.AND.KK.EQ.8) GO TO 1386
      K1 = 0
      K2 = 0
      NUSETD = IUSETD + BUF(KK) - 1
      IF (NUSETD .LT. IUSETD) GO TO 1385
      DO 1384 KKK = IUSETD,NUSETD
      BUF(10) = Z(KKK)
      IF (ANDF(BUF(10),MSKUD) .NE. 0) K1 = K1 + 1
      IF (ANDF(BUF(10),MSKUE) .NE. 0) K2 = K2 + 1
 1384 CONTINUE
 1385 BUF(KK  ) = K1
      BUF(KK+1) = K2
      IF (NUSETD .LT. IUSETD) GO TO 1386
      IF (ANDF(BUF(10),MSKUE) .EQ. 0) BUF(KK+1) = 0
      IF (ANDF(BUF(10),MSKUD) .NE. 0) GO TO 1386
      NOGO = 1
      BUF(1) = Z(K)
      BUF(2) = K1
      CALL MESAGE (30,93,BUF)
 1386 CONTINUE
      BUF(7) = BUF(8)
      BUF(8) = BUF(9)
      CALL WRITE (NLFT,BUF,8,0)
 1387 CONTINUE
      CALL WRITE (NLFT,0,0,1)
      I = I + 6
      IF (Z(I) .NE. 0) GO TO 1381
C
C     CLOSE FILE AND WRITE TRAILER.
C
      CALL CLOSE (NLFT,CLSREW)
      MCB(1) = NLFT
      MCB(2) = (NNOLIN-INOLIN)/6 + 1
      CALL WRTTRL (MCB)
      IF (INCORE .NE. 0) INEQ = 0
      GO TO 1400
 1392 NONLFT =-1
C
C     LOCATE TIC CARDS IN DYNAMICS POOL.
C
 1400 NOTRL =-1
      NOTIC = 0
      NOTSTP= 0
      CALL LOCATE (*1500,Z(BUF1),TIC,FLAG)
      NOTRL = 1
C
C     OPEN SCR1. INITIALIZE TO READ TIC CARDS.
C
      FILE = SCR1
      CALL OPEN (*2001,SCR1,Z(BUF2),WRTREW)
      ITIC = NEQDYN + 2
      NSET = BUF3 - 1
      J    = NSET
      L    = 2
      MSG(1) = 69
      ID   = 0
C
C     READ A TIC CARD. IF SET ID IS DIFFERENT, STORE IT IN LIST.
C     IF NOT FIRST CARD, SORT DATA ON SIL NO. AND WRITE IT IN SCR1.
C
 1420 CALL READ (*2002,*1440,DPOOL,BUF,5,0,FLAG)
      IF (BUF(1) .EQ. ID) GO TO 1430
      IF (ID .EQ. 0) GO TO 1421
      N = I - ITIC
      CALL SORT (0,0,3,1,Z(ITIC),N)
      CALL WRITE (SCR1,Z(ITIC),N,1)
 1421 ID = BUF(1)
      Z(J) = ID
      J  = J - 1
      I  = ITIC
      MSG(3) = ID
C
C     CONVERT POINT AND COMPONENT TO SIL NO.
C     STORE SIL NO., UO, VO IN CORE.
C
 1430 CALL DPDAA
      Z(I  ) = BUF(2)
      Z(I+1) = BUF(4)
      Z(I+2) = BUF(5)
      I = I + 3
      IF (I .LT. J) GO TO 1420
      CALL MESAGE (-8,0,NAM)
C
C     HERE WHEN LAST CARD READ - SORT AND WRITE LAST RECORD.
C
 1440 N = I - ITIC
      CALL SORT (0,0,3,1,Z(ITIC),N)
      CALL WRITE (SCR1,Z(ITIC),N,1)
      CALL CLOSE (SCR1,CLSREW)
      ISET = J + 1
C
C     OPEN TRL. WRITE SET IDS IN HEADER.
C
      FILE = TRL
      CALL OPEN (*1493,TRL,Z(BUF2),WRTREW)
      CALL FNAME (TRL,BUF)
      N = NSET - ISET + 1
      BUF(3) = N
      NOTIC  = N
      CALL WRITE (TRL,BUF,3,0)
      I  = ISET
      J  = NSET
 1451 ID = Z(J)
      Z(J) = Z(I)
      Z(I) = ID
      I  = I + 1
      J  = J - 1
      IF (I .LT. J) GO TO 1451
      CALL WRITE (TRL,Z(ISET),N,0)
C
C     READ USETD INTO CORE.
C     COMPUTE NO. OF POINTS UN UD SET. WRITE NO. AS LAST WORD OF HEADER.
C
 1460 FILE = USETD
      CALL OPEN (*2001,USETD,Z(BUF3),RDREW)
      CALL FWDREC (*2002,USETD)
      IUSETD = 1
      INEQ   = 0
      CALL READ (*2002,*1462,USETD,Z(IUSETD),BUF3-IUSETD,1,N)
      CALL MESAGE (-8,0,NAM)
 1462 CALL CLOSE (USETD,CLSREW)
      NUSETD = IUSETD + N - 1
      K = 0
      DO 1463 I = IUSETD,NUSETD
      IF (ANDF(Z(I),MSKUD) .NE. 0) K = K + 1
 1463 CONTINUE
      CALL WRITE (TRL,K,1,1)
      IF (NOTIC .EQ. 0) GO TO 1481
C
C     READ SCR1. CONVERT SIL NO. TO AN SIL NO. IN THE D-SET.
C     WRITE TRL ONE RECORD PER SET.
C
      FILE = SCR1
      KSET = ISET
      CALL OPEN (*2001,SCR1,Z(BUF3),RDREW)
 1475 K = 0
      IPOINT = IUSETD
 1471 CALL READ (*1474,*1473,SCR1,BUF,3,0,FLAG)
      NUSETD = IUSETD + BUF(1) - 1
      DO 1472 I = IPOINT,NUSETD
      IF (ANDF(Z(I),MSKUD) .NE. 0) K = K + 1
 1472 CONTINUE
      BUF(1) = K
      IF (ANDF(Z(NUSETD),MSKUD) .NE. 0) GO TO 1476
      NOGO = 1
      CALL MESAGE (30,133,Z(KSET))
 1476 CALL WRITE (TRL,BUF,3,0)
      IPOINT = NUSETD + 1
      GO TO 1471
 1473 CALL WRITE (TRL,0,0,1)
      KSET = KSET + 1
      GO TO 1475
 1474 CALL CLOSE (SCR1,CLSREW)
C
C     IF TSTEP CARDS PRESENT, COPY THEM ONTO TRL.
C
      CALL LOCATE (*1490,Z(BUF1),TSTEP,FLAG)
 1481 CALL READ (*2002,*1483,DPOOL,BUF,1,0,FLAG)
      NOTSTP = NOTSTP + 1
      CALL WRITE (TRL,BUF,1,0)
 1482 CALL READ (*2002,*2003,DPOOL,BUF,3,0,FLAG)
      IF (BUF(1) .EQ. -1) GO TO 1485
      CALL WRITE (TRL,BUF,3,0)
      GO TO 1482
 1485 CALL WRITE (TRL,0,0,1)
      GO TO 1481
 1483 CONTINUE
C
C     CLOSE FILES AND WRITE TRAILER.
C
 1490 CALL CLOSE (TRL,CLSREW)
      MCB(1) = TRL
      MCB(2) = NOTIC
      MCB(3) = NOTSTP
      CALL WRTTRL (MCB)
 1492 CALL CLOSE (DPOOL,CLSREW)
      RETURN
C
 1493 NOTRL = -1
      GO TO 1492
C
C     HERE IF NO TIC CARDS - LOCATE TSTEP CARDS IN DYNAMICS POOL.
C     IF ABSENT, RETURN. OTHERWISE OPEN TRL AND WRTIE HEADER.
C
 1500 CALL LOCATE (*1492,Z(BUF1),TSTEP,FLAG)
      NOTRL = 1
      FILE  = TRL
      CALL OPEN (*1493,TRL,Z(BUF2),WRTREW)
      CALL FNAME (TRL,BUF)
      BUF(3) = 0
      CALL WRITE (TRL,BUF,3,0)
      GO TO 1460
C
C     FATAL FILE ERRORS
C
 2001 N = -1
      GO TO 2005
 2002 N = -2
      GO TO 2005
 2003 N = -3
 2005 CALL MESAGE (N,FILE,NAM)
      RETURN
      END
