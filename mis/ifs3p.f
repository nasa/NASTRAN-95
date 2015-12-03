      SUBROUTINE IFS3P (*,*,*)
C
      LOGICAL         NOUD,NOS,BADDAT,BADFOR,ABORT,LH,IAX,IDFREQ,LHARM,
     1                GRDMSG,IFPDCO,PERM,PROL,RBE,FIRST,PRT
CHURNB 11/93
      LOGICAL ONEH,BLANKH
CHURNE
      INTEGER         R,R1,G1,T3,T4,THRU,ARIGID,BRIGID,CRIGID,DRIGID,
     1                ERIGID,FRIGID,BLNK,ENDT,IA(6),IB(6),IC(6),JA(6),
     2                JB(6),JC(6),CRTR,CRBA,CRBE,Q(92)
      DIMENSION       RM(50)
CHURNB 11/93
      DIMENSION NAM(2),IONES(4)
CHURNE
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25,SCC*19,GCC*19
      COMMON /XMSSG / UFM,UWM,UIM,SFM
      COMMON /SYSTEM/ NBUF,NOUT,ABORT,IDUMMY(52),ITHRML,DUM21(21),IPIEZ
      COMMON /IFPDTA/ ID,N,K,KX,KY,I(100),M(100),MF(100),M1(100),
     1                M1F(100),KN,BADDAT,BADFOR,NOPEN,NPARAM,IAX,NN,
     2                IAXF,NAXF,LHARM,KNT,SLOTDF(5),GC(7),LL(6)
CHURNB 11/93
     3,               NNS,ONEH,BLANKH,IAXG
CHURNE
      COMMON /ZZZZZZ/ IBUFF(1)
      COMMON /IFPX2 / T3(2,270)
      COMMON /IFPX3 / T4(2,270)
      COMMON /CIFS3P/ GRDMSG,LA1,L7,KM,L0,G1,LH,
     1                IGDST2,IGDST6,IGDST7,IGDST8,IDDSF,
     2                IDFREQ,IDRAD,NVAR,IDS,JMS,KMS,LPLF
      EQUIVALENCE     (M(1),RM(1)),(LINE,IDUMMY(9)),(NBPW,IDUMMY(37))
CHURNB 11/93
      EQUIVALENCE (XIN,IXIN)
CHURNE
      DATA PROL,ENDT    / .FALSE.,4HENDT/, PERM  /.FALSE.             /
      DATA FIRST,PRT    /  2*.TRUE.     /
      DATA LUD,LZ,KK,LS /  4HUD  ,4HZ    , 4HK   ,4HS   /, NT1 /250   /
      DATA ARIGID/4HCRIG/, BRIGID/4HD1  /, CRIGID/4HD2  /, IRIGID /1  /
      DATA DRIGID/4HD3  /, MSET  /4HMSET/, BLNK  /4H    /, THRU/4HTHRU/
      DATA ERIGID/4H1   /, FRIGID/4H2   /, IND /4HIN  /
      DATA CRTR  /4HCRTR/, CRBA  /4HCRBA/, CRBE  /4HCRBE/, IUM /4HUM  /
      DATA SCC   /'SORTED CARD COUNT =' /, GCC  /'GENERATED CARD    -'/
CHURNB 11/93
      DATA ISCR1 /301/, IONES/4*-1/, NAM/4HIFS3,4HP   /
CHURNE
C
      IF (K .GT. 100) GO TO 81
      GO TO (   100, 200,   5,   5,   5,   5,   5,   5,   5,   5,
     1            5,3980,4020,   5,   5,   5,1700,   5,   5,   5,
     2            5,   5,   5,   5,   5,   5,   5,2800,   5,   5,
     3            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     4            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     5            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     6            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     7            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     8            5,3960,4020,4060,   5,   5,   5,   5,   5,   5,
     9            5,3981,   5,   5,   5,   5,   5,   5,   5,   5  ),K
   81 IF (KX .GT. 100) GO TO 82
      GO TO (     5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     2            5,   5,4060,   5,   5,1260,   5,   5,   5,   5,
     3         1310,1310,   5,   5,   5,   5,   5,1380,1390,   5,
     4            5,   5,1430,1440,1450,1460,1470,1480,1490,1500,
     5         1500,1520,1530,1540,1550,1560,1560,   5,   5,   5,
     6            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     7            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     8            5,1820,1820,1820,1420,   5,   5,   5,   5,   5,
     9            5,   5,   5,   5,   5,   5,   5,   5,   5,   5  ),KX
   82 IF (KY .GT. 100) GO TO 83
      GO TO (     5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,   5,3981,   5,   5,   5,   5,
     2            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     3            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     4            5,   5,   5,1400,   5,   5,   5,   5,   5,   5,
     5            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     6            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     7            5,   5,1415,1415,   5,   5,   5,   5,2010,   5,
     8            5,   5,   5,2060,2111,2030,2040,2030,   5,1410,
     9            5,   5,   5,   5,   5,   5,7300,7000,   5,   5  ),KY
   83 KZ = KY - 100
      IF (KZ .GT. 53) GO TO 5
      IF (KZ.LT.47 .OR. KZ.GT.51 .OR. .NOT.FIRST) GO TO 90
      FIRST = .FALSE.
      IF (.NOT.PRT) GO TO 90
      CALL PAGE1
      WRITE  (NOUT,85) UIM
   85 FORMAT (A29,', CONVERSIONS OF RIGID ELEMENTS, CRROD, CRBAR, ',
     1       'CRTRPLT, CRBE1, AND CRBE2, TO CRIGDR, CRIGD2, OR CRIGD3',
     2       /5X,'ARE AS FOLLOWS (BLANK FIELDS MAY BE PRINTED AS ZEROS',
     3       '. CONTINUATION FIELDS ARE NOT PRINTED) -',/)
      LINE = 8
   90 GO TO (     5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     2            5,   5,   5,   5,   5,   5,   5,   5,5100,5200,
     3            5,3980,   5,   5,   5,   5,   5,   5,   5,   5,
     4            5,   5,   5,   5,2920,3010,6000,6100,6300,7000,
     5         6400,6500,6600                                     ),KZ
    5 CALL PAGE2 (2)
      WRITE  (NOUT,6) SFM
    6 FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS3P.')
      ABORT  = .TRUE.
      RETURN 1
    7 BADFOR = .TRUE.
      RETURN 1
    8 BADDAT = .TRUE.
      RETURN 1
    3 DO 4 L = 1,N
    4 I(L) = M(L)
    9 RETURN 3
C
C*******              1-GRID            ********************************
C
  100 IF (MF(2) .EQ. 0) M(2) = IGDST2
      IF (MF(6) .EQ. 0) M(6) = IGDST6
      IF (MF(7) .EQ. 0) M(7) = IGDST7
      IF (MF(8) .EQ. 0) M(8) = IGDST8
      IF (M(1).LE.0 .OR. M(2).LT.0 .OR. M(6).LT.-1) GO TO 8
      IF (M(6).GE.0 .OR. GRDMSG) GO TO 105
      CALL PAGE2 (2)
      WRITE  (NOUT,103) UWM
  103 FORMAT (A23,' 302, ONE OR MORE GRID CARDS HAVE DISPLACEMENT ',
     1       'COORDINATE SYSTEM ID OF -1')
      GRDMSG = .TRUE.
  105 IF (IFPDCO(M(7))) GO TO 8
      IF (IFPDCO(M(8))) GO TO 8
      IF (MF(8) .NE. 0) GO TO 7
      N = 8
      GO TO 3
C
C*******        2-GRDSET       ****************************************
C
  200 IF (G1 .EQ. 0) GO TO 8
      G1 = 0
      IF (M(2).EQ.0 .AND. M(6).EQ.0 .AND. M(7).EQ.0 .AND. M(8).EQ.0)
     1    GO TO 8
      IF (M(2).LT.0 .OR. M(6).LT.-1 .OR. M(7).LT.0 .OR. M(8).LT.0)
     1    GO TO 8
      IF (IFPDCO(M(7)) .OR. IFPDCO(M(8))) GO TO 8
      IF (MF(8) .NE. 0) GO TO 7
      IGDST2 = M(2)
      IGDST6 = M(6)
      IGDST7 = M(7)
      IGDST8 = M(8)
      RETURN 2
C
C*****         126-FREQ       ******************************************
C
 1260 IF (IDFREQ) IDDSF = 0
      IDFREQ = .FALSE.
      GO TO 1430
C
C******     131-RLOAD1, 132-RLOAD2    **********************************
C
 1310 IF (M(5).EQ.0 .AND. M(6).EQ.0) GO TO 8
      IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(3).LT.0 .OR. M(4).LT.0)
     1    GO TO 8
      IF (M(5).LT.0 .OR. M(6).LT.0) GO TO 8
      N = 6
      GO TO 3
C
C*******       138-TLOAD1      *****************************************
C
 1380 IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(3).LT.0 .OR. M(5).LE.0)
     1    GO TO 8
      IF (M(4).LT.0 .OR. M(4).GT.4) GO TO 8
      N = 5
      GO TO 3
C
C*******       139-TLOAD2      *****************************************
C
 1390 IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(3).LT.0) GO TO 8
      IF (RM(5).LT.0. .OR. RM(6).LE.RM(5) .OR. RM(7).LT.0.) GO TO 8
      IF (M(4).LT.0 .OR. M(4).GT.4) GO TO 8
      N = 10
      GO TO 3
C
C******        244-RADMTX     *****************************************
C
 1400 IF (KM .EQ. 1) GO TO 1431
      KM = 1
      IF (MF(1) .NE. 1) BADFOR = .TRUE.
      ID = M(1)
      IF (ID .LE. IDRAD) BADDAT = .TRUE.
      IDRAD = ID
      I(1)  = ID
      N  = 1
      L1 = 2
      GO TO 1432
C
C******        290-VARIAN        **************************************
C
 1410 IF (KM .EQ. 1) GO TO 1431
      KM = 1
      IF (NVAR .NE. 0) GO TO 8
      NVAR = 1
      GO TO 1431
C
C*****      273-AEFACT , 274-FLFACT    ********************************
C
 1415 IF (KM .EQ. 1) GO TO 1431
      KM = 1
      IF (MF(1) .NE. 1) BADFOR = .TRUE.
      IF (M(1)  .LE. 0) BADDAT = .TRUE.
      I(1) = M(1)
      N  = 1
      L1 = 2
      IF (MF(3) .NE. 3) GO TO 1432
      IF (M(3).NE.THRU .OR. M(4).NE.BLNK) BADDAT = .TRUE.
      IF (MF(2).NE.2 .OR. MF(4).NE.2 .OR. MF(5).NE.1 .OR. MF(6).NE.2)
     1    BADFOR = .TRUE.
      IF (M(6) .LE.    1) BADDAT = .TRUE.
      IF (M(5) .EQ. M(2)) BADDAT = .TRUE.
      IMID = 0
      IF (RM(5)-RM(7).GE.0. .AND. RM(7)-RM(2).LT.0.) IMID = 1
      IF (RM(5)-RM(7).LE.0. .AND. RM(7)-RM(2).GT.0.) IMID = 1
      IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 1416
      BADFOR = .TRUE.
      GO TO 1438
 1416 IF (BADFOR .OR. BADDAT) GO TO 1435
      IF (IMID .EQ. 0) GO TO 1418
      RM(7) = 0.5*(RM(2) + RM(5))
      CALL PAGE2 (3)
      WRITE  (NOUT,1417) UWM,I(1)
 1417 FORMAT (A25,' 528, FACTOR FMID IN FLFACT SET',I9,' DOES NOT LIE ',
     1       'BETWEEN F1 AND FNF.', /5X,'IT IS BEING RESET TO (F1 + ',
     2       'FNF)/2.0')
 1418 T4(2,K) = T4(2,K) + 1
      CALL WRITE (204,I,1,0)
      L = 1
 1419 TERM1 = (M(6)-L)*(RM(5)-RM(7))
      TERM2 = (L-1)*(RM(7)-RM(2))
      ANUM  = RM(2)*TERM1 + RM(5)*TERM2
      DEN   = TERM1 + TERM2
      FACTOR= ANUM/ DEN
      T4(2,K) = T4(2,K) + 1
      CALL WRITE (204,FACTOR,1,0)
      L = L + 1
      IF (L .LE. M(6)) GO TO 1419
      I(1) = -1
      T4(2,K) = T4(2,K) + 1
      CALL WRITE (204,I,1,0)
      N  = 0
      KM = 0
      KN = 0
      GO TO 9
C
C*****      143-DSFACT(1430), 185-PLFACT(1420)     ********************
C
 1420 IF (LPLF) 8,1425,1430
 1425 LPLF  = 1
      IDDSF = 0
 1430 IF (KM .EQ. 1) GO TO 1431
      KM = 1
      IF (MF(1) .NE. 1) BADFOR = .TRUE.
      ID = M(1)
      IF (ID .LE. IDDSF) BADDAT = .TRUE.
      IDDSF = ID
      I(1)  = ID
      IF (MF(2) .NE. 2) BADFOR = .TRUE.
      N  = 2
      L1 = 3
      I(N) = M(2)
      GO TO 1432
 1431 L1 = 1
 1432 DO 1433 L = L1,8
      IF (MF(L) .EQ. 0) GO TO 1436
      IF (MF(L) .NE. 2) BADFOR = .TRUE.
      N = N + 1
 1433 I(N) = M(L)
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 1438
 1435 KM = 0
      N  = N + 1
      I(N) =-1
      KN = 0
      GO TO 9
 1436 IF (L .EQ. 1) BADFOR = .TRUE.
      DO 1437 L2 = L,8
      IF (MF(L2).NE.0) BADFOR = .TRUE.
 1437 CONTINUE
      IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 1435
      BADFOR = .TRUE.
 1438 KN = 1
      GO TO 9
C
C******        144-AXIC           **************************************
C
 1440 IF (IAX) GO TO 1445
      IAX = .TRUE.
      NN  = 998
      DO 1442 L = 1,NT1
      IF (T4(1,L) .GT. 0) T3(1,L) = T3(1,K)
 1442 CONTINUE
CHURD2 11/93
C     IF (M(1).LT.0 .OR. M(1).GT.998 .OR. M(2).NE.0) GO TO 8
C     NN = M(1)
CHURNB 11/93
C
C M.LT.0 CHECK IS REMOVED TO ALLOW FOR SINGLE HARMONIC
C
C     IF(M(1).LT.0.OR.M(1).GT.998.OR.M(2).NE.0)GO TO 8
      IF(             M(1).GT.998.OR.M(2).NE.0)GO TO 8
      NNS = M(1)
      NN = IABS(M(1))
      ONEH = .FALSE.
      IF(NNS .LT. 0)ONEH = .TRUE.
CHURNE
      N  = 2
      IF (NN.GT.15 .AND. NBPW.LE.32) GO TO 1448
      GO TO 3
 1445 CALL PAGE2 (2)
      WRITE  (NOUT,1446) UFM
 1446 FORMAT (A23,' 329, ONLY ONE(1) AXIC CARD ALLOWED.')
      ABORT = .TRUE.
      GO TO 2
 1448 WRITE  (NOUT,1449) UWM
 1449 FORMAT (A25,', POTENTIAL SYSTEM FATAL ERROR DUE TO LARGE HARMONIC'
     1,      ' (LARGER THAN 15) ON 32-BIT WORD MACHINE')
      GO TO 3
C  OR GO TO 1447
C
C******        145-RINGAX         **************************************
C
 1450 IF (M(1).LE.0 .OR. RM(3).LE.0.) GO TO 8
      IH = NN
      ASSIGN 1451 TO R
      ASSIGN    8 TO R1
      GO TO 21
 1451 IF (IFPDCO(M(7))) GO TO 8
      N = 4
      I(1) = M(1)
      I(2) = M(3)
      I(3) = M(4)
      I(4) = M(7)
      GO TO 2
C
C******        146-CCONEAX        **************************************
C
 1460 IF (M(1).LE.0 .OR. M(3).LE.0 .OR. M(4).LE.0) GO TO 8
      IF (MF(2) .EQ. 0) M(2) = M(1)
      IF (M(2).LE.0 .OR. M(4).EQ.M(3)) GO TO 8
      IH = NN
      ASSIGN 1461 TO R
      ASSIGN    8 TO R1
      GO TO 21
 1461 N = 4
      GO TO 3
C
C******        147-PCONEAX        **************************************
C
 1470 IF (M(1) .LE. 0) GO TO 8
      IF (M(2).EQ.0 .AND. M(3).NE.0 .OR. M(2).LT.0) GO TO 8
      IF (M(4).EQ.0 .AND. M(5).NE.0 .OR. M(4).LT.0) GO TO 8
      IF (M(6).EQ.0 .AND. M(7).NE.0 .OR. M(6).LT.0) GO TO 8
      IF (M(2).NE.0 .AND. M(3).EQ.0) GO TO 8
      IF (M(6).NE.0 .AND. M(7).EQ.0) GO TO 8
      IH = NN
      ASSIGN 1471 TO R
      ASSIGN    8 TO R1
      GO TO 21
 1471 N = 24
      GO TO 3
C
C******        148-SPCAX       *****************************************
C
 1480 IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(3).LT.0) GO TO 8
      IF (IFPDCO(M(4))) GO TO 8
CHURNB 11/93
      IF(MF(3).EQ.0)GO TO 1481
CHURNE
      ASSIGN    8 TO R1
      ASSIGN 1489 TO R
      IH = M(3)
      GO TO 21
 1489 N = 5
      GO TO 3
CHURNB 11/93
C
C HID IS BLANK - GENERATE HID FOR THIS SPCAX FOR ALL HARMONICS
C
 1481 NHARMS=NNS+1
      IF(ONEH)NHARMS=1
      DO 1482 IL=1,NHARMS
      N=N+5
      I(N-4)=M(1)
      I(N-3)=M(2)
      I(N-1)=M(4)
      I( N )=M(5)
      I(N-2)=IL-1
      IF(ONEH)I(N-2)=NN
 1482 CONTINUE
      GO TO 2
CHURNE
C
C******        149-MPCAX       *****************************************
C
 1490 IF (M(7) .GT. 6) BADDAT = .TRUE.
      IF (ITHRML.EQ.1 .AND. M(7).GT.1) BADDAT = .TRUE.
      IF (KM .NE. 0) GO TO 1492
      KM = 1
      NT = 0
CHURNB 11/93
      BLANKH=.FALSE.
CHURNE
      IF (MF(1).NE.1 .OR. MF(2).NE.0 .OR. MF(3).NE.0 .OR. MF(4).NE.0)
     1    BADFOR = .TRUE.
      L1 = 5
CHURNB 11/93
      IF(MF(6).EQ.0)BLANKH=.TRUE.
      IF(BLANKH)CALL GOPEN(ISCR1,IBUFF(2*NBUF+1),1)
CHURNE
      ASSIGN 1491 TO R
      GO TO 1493
 1491 IF (M(1) .LE. 0) BADDAT = .TRUE.
      ID = M(1)
      N  = 1
      I(N) = ID
      IH = NN
      ASSIGN 1497 TO R
      ASSIGN    8 TO R1
      GO TO 21
 1492 L1 = 1
      IF (M(3) .GT. 6) BADDAT = .TRUE.
      IF (ITHRML.EQ.1 .AND. M(3).GT.1) BADDAT = .TRUE.
      ASSIGN 1496 TO R
 1493 DO 1495 L = L1,8
      IF (MF(L) .EQ. 0) GO TO 1495
      IF (L.EQ.4 .OR. L.EQ.8) GO TO 1494
      IF (MF(L) .NE. 1) BADFOR = .TRUE.
      GO TO 1495
 1494 IF (MF(L) .NE. 2) BADFOR = .TRUE.
 1495 CONTINUE
      GO TO R, (1491,1496)
 1496 N = 0
 1497 DO 1498 L = L1,5,4
      IF (M(L  ).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0 .AND.
     1    M(L+3).EQ.0) GO TO 1498
      IF (M(L).LE.0 .OR. M(L+1).LT.0 .OR. M(L+2).LT.0 .OR. M(L+3).EQ.0
     1   .AND. L1.EQ.5) BADDAT = .TRUE.
CHURNB 11/93
      IF(BLANKH.AND.L1.EQ.1.AND.MF(L+1).NE.0)BADFOR=.TRUE.
      IF(.NOT.BLANKH.AND.L1.EQ.1.AND.MF(L+1).EQ.0)BADFOR=.TRUE.
CHURNE
      N = N + 4
      I(N-3) = M(L  )
      I(N-2) = M(L+1)
      I(N-1) = M(L+2)
      I(N  ) = M(L+3)
 1498 CONTINUE
      NT = NT + N
      IF (N .LT. 4) BADDAT = .TRUE.
      KN = 1
CHURNB 11/93
      IF(M1(1).NE.0.OR.M1(2).NE.0)GO TO 1499
      IF(.NOT.BLANKH)GO TO 9
      CALL WRITE(ISCR1,I,N,0)
C      WRITE(6,10005)N,(I(IL),IL=1,N)
C10005 FORMAT(6H MPCAX,6I5)
      N=0
      GO TO 9
CHURNE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
CHURNB 11/93
 1499 CONTINUE
CHURNE
      N  = N + 4
      I(N-3) = -1
      I(N-2) = -1
      I(N-1) = -1
      I(N  ) = -1
      KN = 0
      KM = 0
      IF (NT .LT. 9) BADDAT = .TRUE.
CHURNB 11/93
      IF(.NOT.BLANKH)GO TO 9
C
C MPCAX CARD DONE - GENERATE CARDS FOR ALL HARMONICS ASSUMING THE ONE JU
C STORED (WITH BLANK HARMONIC) IS FOR THE ZERO HARMONIC
C
      IF(NT.GT.NOPEN)CALL MESAGE(-8,0,NAM)
      CALL WRITE(ISCR1,I,N-4,1)
C     WRITE(6,10006)N,(I(IL),IL=1,N)
C0006 FORMAT(7H MPCAX1,10I5)
      CALL CLOSE(ISCR1,1)
      CALL GOPEN(ISCR1,IBUFF(2*NBUF+1),0)
      CALL READ(*14990,*14991,ISCR1,IBUFF(3*NBUF+1),NOPEN,0,NNT)
14990 CALL MESAGE(-8,0,NAM)
14991 CALL CLOSE(ISCR1,1)
C     WRITE(6,10007)NT,NNT,(IBUFF(3*NBUF+IL),IL=1,NNT)
C0007 FORMAT(7H MPCAX2,10I5)
      IF(NT.NE.NNT)CALL MESAGE(-61,0,0)
C
C ALL MPCAX CARD INFO FOR THIS CARD IS READ IN. GENERATE FOR ALL HARMONI
C
      NHARMS=NNS+1
      IF(ONEH)NHARMS=1
      DO 14992 L=1,NHARMS
      ILL=L-1
      IF(ONEH)ILL=IABS(NNS)
      DO 14993 IL=3,NT,4
14993 IBUFF(3*NBUF+IL)=ILL
      T4(2,K)=T4(2,K)+NT
      CALL WRITE(215,IBUFF(3*NBUF+1),NT,0)
      T4(2,K)=T4(2,K)+4
      CALL WRITE(215,IONES,4,0)
14992 CONTINUE
      N=0
CHURNE
      GO TO 9
C
C******        151-SUPAX, 150-OMITAX     *******************************
C
 1500 L = 1
 1501 IF (M(L).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0) GO TO 1510
      IF (M(L).LE.0 .OR.  M(L+1).LT.0) GO TO 8
      IF (IFPDCO(M(L+2))) GO TO 8
      ASSIGN 1507 TO R
      ASSIGN    8 TO R1
      IH = M(L+1)
      GO TO 21
 1507 N  = N + 3
      IF (N.GT.3 .AND. M(L).EQ.M(L-3) .AND. M(L-1).EQ.M(L-4) .AND.
     1    M(L-2).EQ.M(L-5)) GO TO 8
      I(N-2) = M(L  )
      I(N-1) = M(L+1)
      I(N  ) = M(L+2)
 1510 L = L + 3
      IF (L .EQ. 4) GO TO 1501
      IF (N) 8,8,2
C
C******        152-POINTAX        **************************************
C
 1520 N = 3
 1521 IF (M(1).LE.0 .OR. M(2).LE.0) GO TO 8
 1522 ASSIGN 3 TO R
 1523 IH = NN
      ASSIGN 8 TO R1
      GO TO 21
 1524 ASSIGN 2 TO R
      GO TO 1523
C
C******        153-SECTAX         **************************************
C
 1530 N = 5
      IF (RM(3)) 8,8,1521
C
C******        154-PRESAX         **************************************
C
 1540 N = 6
      IF (M(1).LE.0 .OR. M(4).LE.0 .OR. M(4).EQ.M(3)) GO TO 8
      IF (IPIEZ .EQ. 1) GO TO 1522
      IF (M(3)  .LE. 0) GO TO 8
      IF (ABS(RM(5)).GE.ABS(RM(6)) .AND. SIGN(1.,RM(5)).EQ.SIGN(1.,RM(6
     1   ))) GO TO 8
      GO TO 1522
C
C******        155-TEMPAX         **************************************
C
 1550 DO 1555 L = 1,5,4
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0) GO TO 1555
      IF (M(L).LE.0 .OR.  M(L+1).LE.0) GO TO 8
      N = N + 4
      I(N-3) = M(L  )
      I(N-2) = M(L+1)
      I(N-1) = M(L+2)
      I(N  ) = M(L+3)
 1555 CONTINUE
      IF (N) 8,8,1524
C
C******     156-FORCEAX, 157-MOMAX    *******************************
C
 1560 IF (M(1).LE.0 .OR. M(2).LE.0) GO TO 8
      IF (MF(3).EQ.2.OR.MF(3).EQ.4) GO TO 8
      IF (MF(3).NE.3 .AND. M(3) .LT. 0) GO TO 8
      N = 8
      L = 4
      I(1) = M(1)
      I(2) = M(2)
      I(3) = M(3)
      I(4) = 0
      IF (MF(3) .EQ. 3) I(4) = M(4)
      IF (MF(3) .EQ. 3) L = 5
      I(5) = M(L)
      I(6) = M(L+1)
      I(7) = M(L+2)
      I(8) = M(L+3)
      GO TO 2
C
C******        17-MPC       ******************************************
C
 1700 IF (M(3).GT.6 .OR. M(6).GT.6) BADDAT = .TRUE.
      IF (ITHRML .NE. 1) GO TO 1710
      IF (M(3).GT.1 .OR. M(6).GT.1) BADDAT = .TRUE.
 1710 IF (KM .NE. 0) GO TO 1724
      KM = 1
      NT = 0
      IF (MF(1).NE.1 .OR. MF(8).NE.0) BADFOR = .TRUE.
      ASSIGN 1712 TO R
      GO TO 1725
 1712 IF (M(1) .LE. 0) BADDAT = .TRUE.
      ID = M(1)
      IF (M(2).LE.0 .OR. M(3).LT.0 .OR. M(4).EQ.0) BADDAT = .TRUE.
      IF (IDS.EQ.ID .AND. JMS.EQ.M(2) .AND. KMS.EQ.M(3)) BADDAT = .TRUE.
      IDS = ID
      JMS = M(2)
      KMS = M(3)
      N = 4
      DO 1721 L = 1,4
 1721 I(L) = M(L)
      GO TO 1745
 1724 IF (MF(1).NE.0 .OR. MF(8).NE.0) BADFOR = .TRUE.
      ASSIGN 1737 TO R
 1725 DO 1736 L = 2,7
      IF (MF(L) .EQ. 0) GO TO 1736
      IF (L.EQ.4 .OR. L.EQ.7) GO TO 1733
      IF (MF(L) .NE. 1) BADFOR = .TRUE.
      GO TO 1736
 1733 IF (MF(L) .NE. 2) BADFOR = .TRUE.
 1736 CONTINUE
      GO TO R, (1712,1737)
 1737 N = 0
      IF (M(2).EQ.0 .AND. M(3).EQ.0 .AND. M(4).EQ.0) GO TO 1745
      IF (M(2).LE.0 .OR.  M(3).LT.0) BADDAT = .TRUE.
      N = 3
      DO 1742 L = 2,4
 1742 I(L-1) = M(L)
 1745 IF (M(5).EQ.0 .AND. M(6).EQ.0 .AND. M(7).EQ.0) GO TO 1751
      IF (M(5).LE.0 .OR.  M(6).LT.0) BADDAT = .TRUE.
      N = N + 3
      I(N-2) = M(5)
      I(N-1) = M(6)
      I(N  ) = M(7)
 1751 IF (N .LE. 0) BADDAT = .TRUE.
      NT = NT + N
      DO 1754 L = 1,8
 1754 M(L) = 0
      KN = 1
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
      N = N + 3
      I(N-2) = -1
      I(N-1) = -1
      I(N  ) = -1
      KN = 0
      KM = 0
      IF (NT .LT. 7) BADDAT = .TRUE.
      GO TO 9
C
C******     182-DAREA, 183-DELAY, 184-DPHASE      *******************
C
 1820 IF (M(1) .LE. 0) GO TO 8
      DO 1825 L = 2,5,3
CHURNB 11/93
C     WRITE(6,10003)L,M(L),M(L+1),M(L+2),N,NNS,(I(IL),IL=1,N)
C0003 FORMAT(7H DAREA0,6I10/(1X,24I5))
CHURNE
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0) GO TO 1825
      IF (M(L).LE.0 .OR.  M(L+1).LT.0 .OR.  M(L+1).GT.6) GO TO 8
      N = N + 4
      I(N-3) = M(1)
      I(N-2) = M(L)
      I(N-1) = M(L+1)
      I(N  ) = M(L+2)
CHURNB 11/93
      IF(.NOT.IAX)GO TO 1825
      IF(M(L).GE.1000000)GO TO 1825
C
C FOR AXIC PROBLEMS AND GRID ID ON DAREA .LT. 10**6, GENERATE DAREAS FOR
C HARMONICS, COMPUTING THE GRID ID.  ASSUME  PRESSURE VALUE IS GIVEN FOR
C ZERO HARMONIC; FOR HIGHER HARMONICS, HALVE IT.
C
      NHARMS=NNS+1
      IF(ONEH)NHARMS=1
      DO 1824 IL=1,NHARMS
      ILL=IL
      IF(NNS.GE.0 .AND. IL.EQ.1)GO TO 1823
      IF(IL.GT.1)GO TO 1821
C
C NNS.LT.0 .AND. IL.EQ.1
C
      ILL=NN+1
      GO TO 1822
 1821 N=N+4
      I(N-3)=M(1)
      I(N-1)=M(L+1)
 1822 XIN=0.5*RM(L+2)
      I(N)=IXIN
 1823 I(N-2)=M(L)+1000000*ILL
 1824 CONTINUE
CHURNE
 1825 CONTINUE
CHURNB 11/93
C      WRITE(6,10001)NHARMS,NNS,N,(I(IL),IL=1,N)
C10001 FORMAT(6H DAREA,3I10/(1X,24I5))
CHURNE
      IF (N) 8,8,2
C
C******        279-CRIGD1         **********************************
C
 2010 CONTINUE
      KN = 1
      GO TO (2011,2012), IRIGID
 2011 CONTINUE
      IRIGID = IRIGID + 1
      IF (MF(1).NE.1) BADFOR = .TRUE.
      IF (M(1) .LE.0) BADDAT = .TRUE.
      I(1) = M(1)
      N = 2
      IF (MF(2).NE.1) BADFOR = .TRUE.
      IF (M(2) .LT.1) BADDAT = .TRUE.
      I(2) = M(2)
      IF (MF(4).EQ.3) GO TO 2020
      IRG = 3
      GO TO 2013
 2012 CONTINUE
      N = 0
      IRG = 1
 2013 CONTINUE
      DO 2015 L = IRG,8
      L1 = L
      IF (M(L)  .LE. 0) GO TO 2018
      IF (MF(L) .NE. 1) BADFOR = .TRUE.
      I(N+1) = M(L)
      DO 2014 J = 1,6
 2014 I(N+1+J) = J
      N = N + 7
 2015 CONTINUE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
 2016 IRIGID = 1
      DO 2017 J = 1,7
 2017 I(N+J) = -1
      IF (M1(1).EQ.ARIGID .AND. M1(2).EQ.BRIGID) I(N+2) = 0
      N  = N + 7
      KN = 0
      GO TO 9
 2018 CONTINUE
      DO 2019 LK = L1,8
      IF (M(LK) .NE.0) BADDAT = .TRUE.
      IF (MF(LK).NE.0) BADFOR = .TRUE.
 2019 CONTINUE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 2022
      GO TO 2016
 2020 IF (M(4).EQ.THRU .AND. M(5).EQ.BLNK) GO TO 2024
      BADDAT = .TRUE.
      IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 2016
 2022 BADFOR = .TRUE.
      GO TO 9
 2024 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 2022
      IF (MF(3).NE.1 .OR.MF(5).NE.1) BADFOR = .TRUE.
      IF (M(3).LE.0  .OR. M(6).LE.0) BADDAT = .TRUE.
      IF (M(6) .LE. M(3)) BADDAT = .TRUE.
      DO 2025 L = 6,8
      IF (MF(L) .NE. 0) BADFOR = .TRUE.
 2025 CONTINUE
      IF (BADFOR .OR. BADDAT) GO TO 2016
      T4(2,K) = T4(2,K) + 2
      CALL WRITE (210,M,2,0)
      L = M(3)
 2026 I(1) = L
      DO 2027 J = 1,6
 2027 I(J+1) = J
      T4(2,K) = T4(2,K) + 7
      CALL WRITE (210,I,7,0)
      L = L + 1
      IF (L .LE. M(6)) GO TO 2026
      IRIGID = 1
      DO 2028 J = 1,7
 2028 I(J) = -1
      IF (M1(1).EQ.ARIGID .AND. M1(2).EQ.BRIGID) I(2) = 0
      N  = 0
      KN = 0
      T4(2,K) = T4(2,K) + 7
      CALL WRITE (210,I,7,0)
      GO TO 9
C
C******        284-CRIGD2        **********************************
C
 2060 CONTINUE
      KN = 1
      GO TO (2061,2062), IRIGID
 2061 IRIGID = IRIGID + 1
      IF (MF(1) .NE. 1) BADFOR = .TRUE.
      IF (M(1)  .LE. 0) BADDAT = .TRUE.
      I(1) = M(1)
      N = 2
      IF (MF(2) .NE. 1) BADFOR = .TRUE.
      IF (M(2)  .LT. 1) BADDAT = .TRUE.
      I(2) = M(2)
      IRG  = 3
      GO TO 2063
 2062 CONTINUE
      N   = 0
      IRG = 1
 2063 CONTINUE
      DO  2065 L = IRG,8,2
      L1 = L
      IF (M(L  ) .LE. 0) GO TO 2068
      IF (M(L+1) .LE. 0) BADDAT = .TRUE.
      IF (MF(L).NE.1 .OR. MF(L+1).NE.1) BADFOR = .TRUE.
      I(N+1) = M(L)
      IF (IFPDCO(M(L+1))) BADDAT = .TRUE.
      DO 2064 J = 1,6
 2064 I(N+1+J) = LL(J)
      N = N + 7
 2065 CONTINUE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
 2066 IRIGID = 1
      DO 2067 J = 1,7
 2067 I(N+J) = -1
      IF (M1(1).EQ.ARIGID .AND. M1(2).EQ.CRIGID) I(N+2) = 0
      N  = N + 7
      KN = 0
      GO TO 9
 2068 CONTINUE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) BADDAT = .TRUE.
      DO 2069 LK = L1,8
      IF (M(LK)  .NE. 0) BADDAT = .TRUE.
      IF (MF(LK) .NE. 0) BADFOR = .TRUE.
 2069 CONTINUE
      GO TO 2066
C
C******      298-CRIGD3, 350-CRBE1       ******************************
C
 7000 KN = 1
      GO TO (7020,7160,7200,7240), IRIGID
 7020 IRIGID = 2
      JRIGID = 1
      KNT1 = KNT
      L1 = 2
      L2 = 6
      L6 = 0
      IF (MF(1).NE.1 .OR. MF(2).NE.1 .OR. MF(3).NE.1) BADFOR = .TRUE.
      IF (M(1) .LT.1 .OR. M(2) .LT.1 .OR. M(3) .LT.1) BADDAT = .TRUE.
      N  = 1
      I(1) = M(1)
      Q(1) = M(1)
      L8   = 1
      NCOMP= 0
 7040 L5 = L2 + 2
      DO 7080 L = L1,L2,2
      L3 = L + 1
      IF (MF(L-L6) .EQ. 0) GO TO 7120
      IF (MF(L-L6).NE.1 .OR. MF(L-L6+1).NE.1) BADFOR = .TRUE.
      IF (M(L).LT.1 .OR. M(L+1).LT.1) BADDAT = .TRUE.
      IF (.NOT.PRT) GO TO 7050
      Q(L8+1) = M(L )
      Q(L8+2) = M(L3)
      L8 = L8 + 2
 7050 I(N+1) = M(L)
      IF (IFPDCO(M(L+1))) BADDAT = .TRUE.
      DO 7060 J = 1,6
      I(N+J+1) = LL(J)
      IF (IRIGID.EQ. 4) GO TO 7060
      IF (LL(J) .NE. 0) NCOMP = NCOMP + 1
 7060 CONTINUE
      N = N + 7
      IF (IRIGID .EQ. 4) GO TO 7080
      IF (NCOMP  .GT. 6) BADDAT = .TRUE.
 7080 CONTINUE
      IF (MF(L5-L6) .NE. 0) BADFOR = .TRUE.
      IF (M(L5) .NE. 0) BADDAT = .TRUE.
      GO TO (7100,7100,7220), JRIGID
 7100 IF (M1(1).NE.0  .OR.  M1(2).NE.0) GO TO 7110
      IF (M1F(2).NE.0 .AND. NCOMP.LT.6) BADDAT = .TRUE.
      GO TO 9
 7110 BADFOR = .TRUE.
      IRIGID = 1
      GO TO 9
 7120 DO 7140 LK = L3,L5
      IF (MF(LK-L6) .NE. 0) BADFOR = .TRUE.
      IF (M(LK) .NE. 0) BADDAT = .TRUE.
 7140 CONTINUE
      GO TO (7100,7100,7220), JRIGID
C
 7160 IF (MF(1) .NE. 0) GO TO 7200
      IRIGID = 3
      JRIGID = 2
      L1 = 2
      L2 = 6
      L6 = 0
      IF (MF(2).NE.1 .OR. MF(3).NE.1) BADFOR = .TRUE.
      IF (M(1).NE.0 .OR. M(2).LT.1 .OR. M(3).LT.1) BADDAT = .TRUE.
      N = 0
      GO TO 7040
C
 7200 IRIGID = 4
      JRIGID = 3
      L1 = 3
      L2 = 7
      L6 = 1
      L7 = L8
      IF (MF(1).NE.3 .OR. MF(2).NE.1 .OR. MF(3).NE.1) BADFOR = .TRUE.
      IF ((M(1).NE.MSET .AND. M(1).NE.IUM) .OR. M(2).NE.BLNK .OR.
     1     M(3).LT.1 .OR. M(4).LT.1) GO TO 7250
      N = 1
      I(1) = MSET
      GO TO 7040
 7220 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
      IRIGID = 1
      DO 7230 J = 1,7
 7230 I(N+J) = -1
      IF (M1(1).EQ.ARIGID .AND. M1(2).EQ.DRIGID) I(N+2) = 0
      IF (M1(1).EQ.CRBE   .AND. M1(2).EQ.ERIGID) I(N+2) = 0
      N  = N + 7
      KN = 0
      IF (KZ.NE.50 .OR. .NOT.PRT) GO TO 9
      LK = (L8+4)/3 + 2
      CALL PAGE2 (LK)
      WRITE  (NOUT,7232) SCC,KNT1,(Q(J),J=1,L7)
 7232 FORMAT (/25X,A19,I7,1H-,5X,'CRBE1 ',7I8, /,(71X,6I8))
      LK = L7 + 1
      WRITE  (NOUT,7234) (Q(J),J=LK,L8)
 7234 FORMAT (69X,'UM',6I8, /,(71X,6I8))
      WRITE  (NOUT,7236) GCC,(Q(J),J=1,L7)
 7236 FORMAT (25X,A19,13X,'CRIGD3',7I8, /,(71X,6I8))
      WRITE  (NOUT,7238) (Q(J),J=LK,L8)
 7238 FORMAT (67X,'MSET',6I8, /,(71X,6I8))
      GO TO 9
C
 7240 L1 = 2
      L2 = 6
      L6 = 0
      IF (MF(1).NE.0 .OR. MF(2).NE.1 .OR. MF(3).NE.1) BADFOR = .TRUE.
      IF (M(1) .NE.0 .OR. M(2) .LT.1 .OR. M(3) .LT.1) BADDAT = .TRUE.
      N = 0
      GO TO 7040
 7250 WRITE (NOUT,6475) UFM,BLNK,Q(1),KNT1
      GO TO 8
C
C******         297-CRIGDR      *************************************
C
 7300 DO 7320 L = 1,5,4
      IF (M(L  ).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0 .AND.
     1    M(L+3).EQ.0) GO TO 7320
      IF (M(L).LE.0 .OR. M(L+1).LE.0 .OR. M(L+2).LE.0 .OR. M(L+3).LE.0)
     1   GO TO 8
      IF (M(L+1) .EQ. M(L+2)) GO TO 8
      IF (M(L+3) .GT. 3) GO TO 7310
      N = N + 4
      IF (N.GT.4 .AND. M(L).EQ.M(L-4)) GO TO 8
      I(N-3) = M(L  )
      I(N-2) = M(L+1)
      I(N-1) = M(L+2)
      I(N  ) = M(L+3)
      GO TO 7320
 7310 WRITE (NOUT,6475) UFM,BLNK,M(L),KNT
      BADDAT = .TRUE.
 7320 CONTINUE
      IF (N) 8,8,2
C
C******       347-CRROD        *****************************************
C
C     MAP THIS RIGID ELEMENT INTO CRIGID3 FORM
C
 6000 IF (MF(1)+MF(2)+MF(3) .NE. 3) GO TO 7
      IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(3).LE.0) GO TO 8
      IF (M(2) .EQ. M(3)) GO TO 8
      IF (M(4).LT.0 .OR. M(5).LT.0) GO TO 6480
      L = M(4) + M(5)
      IF (L.LT.1 .OR. L.GT.3) GO TO 6480
      IF (M(4).NE.0 .AND. M(5).NE.0) GO TO 6480
      IF (.NOT.PRT) GO TO 6004
      CALL PAGE2 (3)
      IF (M(4).NE.0) WRITE (NOUT,6002) SCC,KNT,(M(J),J=1,4),GCC,M(1),
     1                                 M(3),M(2),M(4)
      IF (M(4).EQ.0) WRITE (NOUT,6003) SCC,KNT,(M(J),J=1,3),M(5),GCC,
     1                                 (M(J),J=1,3),M(5)
 6002 FORMAT (/25X,A19,I7,1H-,5X,'CRROD ',4I8,
     1        /25X,A19,13X,'CRIGDR',4I8)
 6003 FORMAT (/25X,A19,I7,1H-,5X,'CRROD ',3I8,8X,I8,
     1        /25X,A19,13X,'CRIGDR',4I8)
 6004 L = M(3)
      IF (M(4) .EQ. 0) GO TO 6005
      L = M(2)
      M(2) = M(3)
      M(3) = L
      M(5) = M(4)
 6005 M(4) = M(5)
      N = 4
      GO TO 3
C
C******       348-CRBAR        *****************************************
C
C     MAP THIS RIGID ELEMENT INTO CRIGD3 FORM
C
 6100 IF (MF(1)+MF(2)+MF(3) .NE. 3) GO TO 7
      IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(3).LE.0) GO TO 8
      IF (M(2) .EQ. M(3)) GO TO 8
      RBE = .FALSE.
      IF (M(6).EQ.0 .AND. M(7).EQ.0) RBE = .TRUE.
      IF (M(4).EQ.0 .AND. M(5).EQ.0) GO TO 6470
      IF (IFPDCO(M(4))) GO TO 6470
      LK = 1
      DO 6110 L = 1,6
      LLL = LL(L)
      IF (RBE .AND. LLL.EQ.0) M(6) = M(6) + L*LK
      IF (LLL .EQ. 0) LK = LK*10
 6110 IA(L) = LLL
      IF (IFPDCO(M(5))) GO TO 6470
      LK = 1
      DO 6115 L = 1,6
      LLL = LL(L)
      IF (RBE .AND. LLL.EQ.0) M(7) = M(7) + L*LK
      IF (LLL .EQ. 0) LK = LK*10
 6115 IB(L) = LLL
      IF (RBE) GO TO 6130
      IF (IFPDCO(M(6))) GO TO 6480
      DO 6120 L = 1,6
      IF (IA(L) .EQ.     0) GO TO 6120
      IF (IA(L) .EQ. LL(L)) GO TO 6480
 6120 JA(L) = LL(L)
      IF (IFPDCO(M(7))) GO TO 6480
      DO 6125 L = 1,6
      IF (IB(L) .EQ.     0) GO TO 6125
      IF (IB(L) .EQ. LL(L)) GO TO 6480
 6125 JB(L) = LL(L)
C
 6130 IF (.NOT.PRT) GO TO 6133
      CALL PAGE2 (4)
      WRITE  (NOUT,6131) SCC,KNT,(M(L),L=1,7),GCC,M(1),M(2),M(4),M(3),
     1                   M(5),M(2),M(6),M(3),M(7)
 6131 FORMAT (/25X,A19,I7,1H-,5X,'CRBAR ',7I8,
     1        /25X,A19,13X,'CRIGD3',5I8, /67X,'MSET',4I8)
C
C     KZ=48 (CRBAR),   KZ=49 (CRTRPLT)
C
 6133 NCOMP = 0
      DO 6135 L = 1,6
      IF (IA(L) .NE. 0) NCOMP = NCOMP + 1
      IF (IB(L) .NE. 0) NCOMP = NCOMP + 1
      IF (KZ   .NE. 49) GO TO 6135
      IF (IC(L) .NE. 0) NCOMP = NCOMP + 1
 6135 CONTINUE
      IF (NCOMP .NE. 6) GO TO 6470
      LK = 0
      IF (KZ .EQ. 49) LK = 1
      I(1) = M(1)
      N  = 2
      IF (M(4+LK) .EQ. 0) GO TO 6143
      I(N) = M(2)
      DO 6140 J = 1,6
 6140 I(N+J) = IA(J)
      N = N + 7
 6143 IF (M(5+LK) .EQ. 0) GO TO 6147
      I(N) = M(3)
      DO 6145 J = 1,6
 6145 I(N+J) = IB(J)
      N = N + 7
 6147 IF (KZ.NE.49 .OR. M(6+LK).EQ.0) GO TO 6160
      I(N) = M(4)
      DO 6150 J = 1,6
 6150 I(J+N) = IC(J)
      N = N + 7
C
 6160 I(N) = MSET
      N = N + 1
      IF (.NOT.RBE) GO TO 6170
      DO 6165 J = 1,6
      IF (IA(J) .EQ. 0) IA(J) =-J
      IF (IA(J) .GT. 0) IA(J) = 0
      IF (IB(J) .EQ. 0) IB(J) =-J
      IF (IB(J) .GT. 0) IB(J) = 0
      IF (KZ   .NE. 49) GO TO 6165
      IF (IC(J) .EQ. 0) IC(J) =-J
      IF (IC(J) .GT. 0) IC(J) = 0
 6165 CONTINUE
 6170 IF (KZ .EQ. 49) LK = 3
      IF (M(6+LK) .EQ. 0) GO TO 6177
      I(N) = M(2)
      DO 6175 J = 1,6
      IF (     RBE) I(N+J) =-IA(J)
      IF (.NOT.RBE) I(N+J) = JA(J)
 6175 CONTINUE
      N = N + 7
 6177 IF (M(7+LK) .EQ. 0) GO TO 6182
      I(N) = M(3)
      DO 6180 J = 1,6
      IF (     RBE) I(N+J) =-IB(J)
      IF (.NOT.RBE) I(N+J) = JB(J)
 6180 CONTINUE
      N = N + 7
 6182 IF (KZ.NE.49 .OR. M(8+LK).EQ.0) GO TO 6190
      I(N) = M(4)
      DO 6185 J = 1,6
      IF (     RBE) I(N+J) =-IC(J)
      IF (.NOT.RBE) I(N+J) = JC(J)
 6185 CONTINUE
      N = N + 7
 6190 N = N - 1
      DO 6195 J = 1,7
 6195 I(N+J) = -1
      IF (M1(1).EQ.CRTR .OR. M1(1).EQ.CRBA) I(N+2) = 0
      N = N + 7
      GO TO 9
C
C******      349-CRTRPLT      ******************************************
C
C     MAP THIS RIGID ELEMENT INTO CRIGD3 FORM
C
 6300 IF (MF(1)+MF(2)+MF(3)+MF(4) .NE. 4) GO TO 7
      IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(3).LE.0 .OR. M(4).LE.0)
     1    GO TO 8
      IF (M(2).EQ.M(3) .OR. M(2).EQ.M(4) .OR. M(3).EQ.M(4)) GO TO 8
      IF (M(5).EQ.0 .AND. M(6).EQ.0 .AND. M(7).EQ.0) GO TO 6470
      RBE  = .FALSE.
      IF (M(9).EQ.0 .AND. M(10).EQ.0 .AND. M(11).EQ.0) RBE = .TRUE.
      IF (IFPDCO(M(5))) GO TO 6470
      LK = 1
      DO 6310 L = 1,6
      LLL = LL(L)
      IF (RBE .AND. LLL.EQ.0) M(9) = M(9) + L*LK
      IF (LLL .EQ. 0) LK = LK*10
 6310 IA(L) = LLL
      IF (IFPDCO(M(6))) GO TO 6470
      LK = 1
      DO 6320 L = 1,6
      LLL = LL(L)
      IF (RBE .AND. LLL.EQ.0) M(10) = M(10) + L*LK
      IF (LLL .EQ. 0) LK = LK*10
 6320 IB(L) = LLL
      IF (IFPDCO(M(7))) GO TO 6470
      LK = 1
      DO 6330 L = 1,6
      LLL = LL(L)
      IF (RBE .AND. LLL.EQ.0) M(11) = M(11) + L*LK
      IF (LLL .EQ. 0) LK = LK*10
 6330 IC(L) = LLL
      IF (RBE) GO TO 6365
      IF (IFPDCO(M(9))) GO TO 6480
      DO 6340 L = 1,6
      IF (IA(L) .EQ.     0) GO TO 6340
      IF (IA(L) .EQ. LL(L)) GO TO 6480
 6340 JA(L) = LL(L)
      IF (IFPDCO(M(10))) GO TO 6480
      DO 6350 L = 1,6
      IF (IB(L) .EQ.     0) GO TO 6350
      IF (IB(L) .EQ. LL(L)) GO TO 6480
 6350 JB(L) = LL(L)
      IF (IFPDCO(M(11))) GO TO 6480
      DO 6360 L = 1,6
      IF (IC(L) .EQ.     0) GO TO 6360
      IF (IC(L) .EQ. LL(L)) GO TO 6480
 6360 JC(L) = LL(L)
 6365 IF (.NOT.PRT) GO TO 6133
      KNT1 = KNT
      IF (.NOT.RBE) KNT1 = KNT - 1
      CALL PAGE2 (5)
      WRITE (NOUT,6370) SCC,KNT1,(M(L),L=1,7),(M(L),L=9,11), GCC,M(1),
     1   M(2),M(5),M(3),M(6),M(4),M(7),M(2),M(9),M(3),M(10),M(4),M(11)
 6370 FORMAT (/25X,A19,I7,1H-,5X,'CRTRPLT',I7,6I8, /63X,3I8,
     1        /25X,A19,13X,'CRIGD3',7I8, /67X,'MSET',6I8)
      GO TO 6133
C
C******    351-CRBE2         *******************************************
C
C     MAP THIS RIGID ELEMENT INTO CRIGD2 FORM
C
 6400 KN = 1
      GO TO (6405,6410), IRIGID
 6405 IRIGID = IRIGID + 1
      KNT1 = KNT
      L6 = 60
      L7 = L6
      L8 = 0
      IF (MF(1)+MF(2)+MF(3) .NE.   3) GO TO 7
      IF (M(1) .LE.0 .OR. M(2) .LE.0) GO TO 8
      I(1) = M(1)
      I(2) = M(2)
      Q(1) = M(1)
      Q(2) = M(2)
      M3   = M(3)
      L8   = L8+2
      Q(L7+1) = M(1)
      Q(L7+2) = M(2)
      Q(L7+3) = M3
      L7  = L7 + 3
      N   = 2
      IRG = 4
      IF (IFPDCO(M3)) BADDAT = .TRUE.
      IF (M3 .EQ.  0) BADDAT = .TRUE.
      GO TO 6420
 6410 N   = 0
      IRG = 1
 6420 DO 6430 L = IRG,8
      IF (MF(L) .EQ. 0) GO TO 6450
      IF (MF(L) .NE. 1) BADFOR = .TRUE.
      IF (M(L)  .LE. 0) BADDAT = .TRUE.
      IF (L8 .GE. L6) GO TO 6422
      Q(L8+1) = M(L)
      Q(L8+2) = M3
 6422 L8 = L8 + 2
      IF (L7 .LT. 92) Q(L7+1) = M(L)
      L7 = L7 + 1
      I(N+1) = M(L)
      DO 6425 J = 1,6
 6425 I(N+1+J) = LL(J)
      N = N + 7
 6430 CONTINUE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
 6440 IRIGID = 1
      DO 6445 J = 1,7
 6445 I(N+J) = -1
      IF (M1(1).EQ.CRBE .AND. M1(2).EQ.FRIGID) I(N+2) = 0
      N  = N + 7
      KN = 0
      IF (.NOT.PRT) GO TO 9
      L3 = L7
      L5 = L8
      IF (L3 .GT. 92) L3 = 92
      IF (L5 .GT. L6) L5 = L6
      J = (L5+2)/8 + (L3-L6+2)/8 + 2
      CALL PAGE2 (J)
      L6 = L6 + 1
      WRITE (NOUT,6447) SCC,KNT1,(Q(J),J=L6,L3)
      WRITE (NOUT,6448) GCC,(Q(J),J=1,L5)
 6447 FORMAT (/25X,A19,I7,1H-,5X,'CRBE2 ',8I8, /,(63X,8I8))
 6448 FORMAT ( 25X,A19,13X,'CRIGD2',8I8, /,(63X,8I8))
      IF (L8.GT.L6 .OR. L7.GT.102) WRITE (NOUT,6449)
 6449 FORMAT (57X,'*** ABOVE PRINTOUT MAY BE IMCOMPLETE.  DATA IS OK')
      GO TO 9
 6450 L1 = L
      IF (L1 .GT. 8) GO TO 6460
      DO 6455 L = L1,8
      IF (M(L)  .NE. 0) BADDAT = .TRUE.
      IF (MF(L) .NE. 0) BADFOR = .TRUE.
 6455 CONTINUE
 6460 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) BADDAT = .TRUE.
      GO TO 6440
C
 6470 WRITE  (NOUT,6475) UFM,IND,M(1),KNT1
 6475 FORMAT (A23,', ILLEGAL ',A2,'DEPENDENT D.O.F.',
     1       ' FOR RIGID ELEMENT',I9,' SORTED COUNT',I8)
      GO TO 8
 6480 WRITE (NOUT,6475) UFM,BLNK,M(1),KNT1
      GO TO 8
C
C******    352-CRBE3         *******************************************
C
C     CARD 3, OR CARDS 2 AND 3, CAN BE OMITTED IF THE CARD(S) CONTAINS
C     ALL BLANKS.
C     CARD 5, OR CARDS 4 AND 5, CAN BE OMITTED IF THE CARD(S) CONTIANS
C     ALL BLANKS, OR DEFAULT FOR THE 'UM' OPTION IS USED
C
C     ACTUALLY THIS CRBE3 INPUT CARD IS NOT WHAT SHOWN IN THE USER'S
C     MANUAL. THE LIST OF G(I,J) CAN BE AS LONG AS NEEDED. THEREFORE
C     CARDS 2 AND 3 CAN BE EXPANDED BEYOND THE 3 GRID POINTS AS SHOWN.
C     THE 4TH AND 5TH CARDS CAN BE EXPANDED TOO. THE WI AND CI FIELDS
C     NEED NOT BE IN THE FIELDS AS SHOWN IN THE EXAMPLE OF THE MANUAL
C
C     CHANGES DONE IN 92 VERSION WERE REMOVED AND REPLACED BY 91 CODE
C     SEE 93 CODE FOR THESE CHANGES
C
C     IM HERE IS CARD NUMBER COUNT
C
 6500 CONTINUE
      IF (KM .NE. 0) GO TO 6510
      KM = 1
      IM = 1
      IF (MF(1)+MF(3)+MF(4) .NE. 3) BADFOR = .TRUE.
      IF (M(1).LE.0 .OR. M(3).LE.0 .OR. M(4).LE.0) BADDAT = .TRUE.
      IF (IFPDCO(M(4))) BADDAT = .TRUE.
      IF (MF(5) .NE. 2) BADDAT = .TRUE.
      I(1) = M(1)
      I(2) = M(3)
      I(3) = M(4)
C
C ... NOTE - COMPONENTS IN LL NOT SENT OUT IN CRBE3
C
      N  = 3
      L1 = 5
      GO TO 6520
C
C
 6510 IF (MF(1) .EQ. 3) GO TO 6560
      IF (IM    .EQ. 0) GO TO 6565
      L1 = 1
 6520 DO 6540 L = L1,8
      IF (MF(L) .NE. 2) GO TO 6530
      IF (L1 .EQ. 5) GO TO 6525
      N   = N + 1
      I(N)=-1
 6525 IM  = 1
CWKBI 11/93 SPR93018
      L1  = 1
      N   = N + 1
      I(N)= M(L)
      GO TO 6540
 6530 IF (MF(L) .EQ. 0) GO TO 6540
      IF (MF(L).NE.1 .OR. M(L).LE.0) BADDAT =.TRUE.
      IF (IM  .EQ.  -1) GO TO 6535
      IF (IFPDCO(M(L))) BADDAT =.TRUE.
 6535 IM =-1
      N  = N + 1
      I(N) = M(L)
 6540 CONTINUE
      IF (M1(1) .NE. 0) GO TO 6550
      KN = 1
      GO TO 9
 6550 N  = N + 1
      I(N) = -1
 6555 KN = 0
      KM = 0
      N  = N + 1
      I(N) = -3
      GO TO 9
 6560 IF (M(1) .NE. IUM) BADDAT =.TRUE.
      I(N+1) = -1
      I(N+2) = -2
      N  = N + 2
      IM = 0
      L1 = 3
      GO TO 6570
 6565 L1 = 2
 6570 DO 6580 L = 2,6,2
      IF (MF(L) .EQ. 0) GO TO 6575
      IF (MF(L  ).NE.1 .OR. M(L1  ).LE.0) BADDAT =.TRUE.
      IF (MF(L+1).NE.1 .OR. M(L1+1).LE.0) BADDAT =.TRUE.
      IF (IFPDCO(M(L1+1))) BADDAT =.TRUE.
      I(N+1) = M(L1  )
      I(N+2) = M(L1+1)
      N  = N  + 2
 6575 L1 = L1 + 2
 6580 CONTINUE
      IF (M1(1) .NE. 0) GO TO 6555
      GO TO 9
C
C******    353-CRSPLINE      *******************************************
C
 6600 CONTINUE
      IF (KM .NE. 0) GO TO 6610
      KM = 1
      IM = -1
      IF (MF(1).NE.1 .OR. M(1).LE.0) GO TO 6680
      IF (MF(2) .EQ. 0 ) RM(2) = .1
      IF (RM(2) .LE. 0.) GO TO 6680
      IF (MF(3).NE.1 .OR. M(3).LE.0) GO TO 6680
      I(1) = M(1)
      I(2) = M(2)
      I(3) = M(3)
      N  = 3
      L1 = 4
      GO TO 6620
 6610 L1 = 1
      IF (IM .EQ. -9) GO TO 6680
 6620 DO 6640 L = L1,8
      IF (MF(L).NE.0 .AND. MF(L).NE.1) GO TO 6680
      IF (IM.EQ.-1 .AND. M(L).LT.0) GO TO 6680
      IF (IM.EQ.-1 .AND. M(L).EQ.0) GO TO 6650
      IF (IM  .EQ.  -1) GO TO 6630
      IF (IFPDCO(M(L))) GO TO 6680
C
C ... NOTE - COMPONENTS IN LL NOT SENT OUT IN CRSPLINE
C
 6630 IM = -IM
      N  = N + 1
      I(N) = M(L)
 6640 CONTINUE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 6670
      N   = N + 1
      I(N)= 0
 6650 IM  = -9
      N   = N + 1
      I(N)= -1
      IF (L .EQ. 8) GO TO 6670
      L1 = L
      DO 6660 L = L1,8
      IF (MF(L) .NE. 0) GO TO 6680
 6660 CONTINUE
C
 6670 KN = 1
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
      KN = 0
      KM = 0
      N  = N + 1
      I(N) = -1
      GO TO 9
 6680 BADDAT = .TRUE.
      GO TO 6670
C
C******        285-CTRIAAX       ***************************************
C
 2111 IF (M(1) .LE. 0 .OR. M(2) .LE. 0) GO TO 8
      IF (M(3) .LE. 0 .OR. M(4) .LE. 0) GO TO 8
      IF (M(3) .EQ. M(4)) GO TO 8
      IF (M(3) .EQ. M(5)) GO TO 8
      IH = NN
      ASSIGN    8 TO R1
      ASSIGN 2172 TO R
      GO TO 21
 2172 N = 6
      GO TO 3
C
C******       286-PTRIAX, 288-PTRAPAX   *******************************
C
 2030 IF (M(1) .LE. 0) GO TO 8
      IH = NN
      ASSIGN 8 TO R1
      ASSIGN 2031 TO R
      GO TO 21
 2031 N = 17
      GO TO 3
C
C*******       287-CTRAPAX             ********************************
C
 2040 IF (M(1) .LE. 0 .OR. M(2) .LE. 0) GO TO 8
      IF (M(3) .EQ. M(4)) GO TO 8
      IF (M(3) .EQ. M(5)) GO TO 8
      IH = NN
      ASSIGN 8 TO R1
      ASSIGN 2041 TO R
      GO TO 21
 2041 N = 7
      GO TO 3
C
C******         28-GENEL         **************************************
C
 2800 GO TO (2802,2810,2830,2810,2836,2844,2858,2844), L0
 2802 L0 = L0 + 1
      KZFLAG = 0
      L8 = 0
      NOUD = .TRUE.
      NOS  = .TRUE.
      IF (MF(1).NE.1 .OR. MF(2).NE.0) BADFOR = .TRUE.
      IF (M(1) .LE. 0) BADDAT = .TRUE.
      ID = M(1)
      I(1) = ID
      N  = 1
      L3 = 3
      GO TO 2812
 2810 L3 = 1
 2811 N = 0
 2812 DO 2814 L = L3,8
      IF (MF(L).NE.0 .AND. MF(L).NE.1) BADFOR = .TRUE.
 2814 CONTINUE
      L5 = 1
      DO 2818 L = L3,7,2
      IF (M(L) .EQ. 0) GO TO 2824
      L5 = L  + 2
      L8 = L8 + 1
      N  = N  + 2
      I(N-1) = M(L  )
      I(N  ) = M(L+1)
      IF (M(L) .LE. 0) GO TO 2816
      IF (M(L+1).GE.0 .AND. M(L+1).LE.6) GO TO 2818
 2816 BADDAT = .TRUE.
 2818 CONTINUE
      IF (M1F(2) .NE. 3) GO TO 2864
 2820 N = N + 2
      I(N-1) = -1
      I(N) = L8
      L0 = L0 + 1
      IF (L0 .EQ. 5) GO TO 2822
      L6 = L8
      GO TO 2864
 2822 L7 = L8
      GO TO 2864
 2824 DO 2826 L = L5,7,2
      IF (M(L).NE.0 .OR. M(L+1).NE.0) BADDAT = .TRUE.
 2826 CONTINUE
      IF (L5 .LE. 1) BADDAT = .TRUE.
      IF (M1F(2) .EQ. 3) GO TO 2820
      BADDAT = .TRUE.
      GO TO 2864
 2830 L0 = L0 + 1
      LB = 0
      IF (MF(1).NE.3 .OR. (M(1).NE.LZ .AND. M(1).NE.KK)) GO TO 2831
      L0 = L0 + 1
      LB = 2
      I(1) =-1
      I(2) = 0
      GO TO 2838
 2831 L8 = 0
      IF (MF(1).NE.3 .OR. MF(2).NE.0) BADFOR = .TRUE.
      IF (M(1) .NE. LUD) BADDAT = .TRUE.
      L3 = 3
      NOUD = .FALSE.
      DO 2835 L = 2,8
 2835 M(L) = M(L+1)
      GO TO 2811
 2836 IF (M(1).NE.LZ .AND. M(1).NE.KK) BADDAT = .TRUE.
 2838 L9 = (L6*(L6+1))/2
      LB = LB + 1
      IF (M(1) .EQ.LZ) KZFLAG = 1
      IF (M(1) .EQ.KK) KZFLAG = 2
      I(LB) = KZFLAG
 2840 L0 = L0 + 1
      L8 = 0
      IF (MF(1) .NE. 3) BADFOR = .TRUE.
      L3 = 2
      DO 2843 L = 2,8
 2843 M(L) = M(L+1)
      GO TO 2846
 2844 L3 = 1
      LB = 0
 2846 DO 2848 L = L3,8
      IF (MF(L).NE.2 .AND. MF(L).NE.0) BADFOR = .TRUE.
 2848 CONTINUE
      N  = LB
      L5 = L9 - L8 + L3 - 1
      IF (L5 .LE. 8) GO TO 2850
      L5 = 8
 2850 DO 2852 L = L3,L5
      N = N + 1
 2852 I(N) = M(L)
      L5 = L9 - L8 + L3
      L8 = L8 + N - LB
      IF (L9 .GT. L8) GO TO 2864
      IF (L9 .EQ. L8) GO TO 2855
      DO 2854 L = L5,8
      IF (M(L) .NE. 0) BADDAT = .TRUE.
 2854 CONTINUE
 2855 IF (L0 .EQ. 8) GO TO 2856
      L0 = L0 + 1
      GO TO 2864
 2856 L0 = 1
      GO TO 2864
 2858 IF (M(1) .NE. LS) BADDAT = .TRUE.
      L9 = L6*L7
      LB = 1
      I(1) = L7
      NOS  = .FALSE.
      GO TO 2840
 2864 DO 2866 L = 1,8
 2866 M(L) = 0
      KN = 1
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
      KN = 0
      IF (ID .LE. LA1) GO TO 2868
      LA1 = ID
      IF (.NOT.NOUD .AND. L7.NE.6 .AND. NOS .AND. KZFLAG.EQ.1)
     1   GO TO 2868
      IF (L7.EQ.0 .AND. .NOT. NOS) GO TO 2868
      L7 = 0
      IF (L0.EQ.1 .AND. .NOT.NOUD) GO TO 9
      N = N + 1
      I(N) = 0
      L0 = 1
      GO TO 9
 2868 BADDAT = .TRUE.
      L0 = 1
      L7 = 0
      LA1= ID
      GO TO 9
C
C******        345-STREAML1      **************************************
C
 2920 IF (KM .EQ. 1) GO TO 2921
      KM = 1
      IF (MF(1).NE.1) BADFOR = .TRUE.
      IF (M(1) .LE.0) BADDAT = .TRUE.
      IF (M(1) .LE.0) BADDAT = .TRUE.
      I(1) = M(1)
      N = 1
      IF (MF(3).EQ.3 .AND. M(3).EQ.THRU) GO TO 2928
      L1 = 2
      GO TO 2922
 2921 L1 = 1
 2922 DO 2923 L = L1,8
      IF (MF(L).NE.0 .AND. MF(L).NE.1) BADFOR = .TRUE.
 2923 CONTINUE
      DO 2926 L = L1,8
      IF (M(L)) 2925,2926,2924
 2924 N = N + 1
      I(N) = M(L)
      GO TO 2926
 2925 BADDAT = .TRUE.
 2926 CONTINUE
      IF (N.LT.L1) BADDAT = .TRUE.
      KN = 1
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
 2927 KM = 0
      N = N + 1
      I(N) = -1
      KN = 0
      GO TO 9
 2928 IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 2929
      KN = 1
      BADFOR = .TRUE.
      GO TO 9
 2929 IF (MF(2).NE.1 .OR. MF(4).NE.1) BADFOR = .TRUE.
      IF (M(2).LE.0 .OR. M(5).LE.0 .OR. (M(2).GT.M(5))) BADDAT = .TRUE.
      IF (BADFOR .OR. BADDAT) GO TO 2927
      CALL WRITE (204,I,N,0)
      L1 = M(2)
      L2 = M(5)
      DO 2930 L = L1,L2
 2930 CALL WRITE (204,L,1,0)
      N = 0
      GO TO 2927
C
C******        346-STREAML2      **************************************
C
C     THEORY DEPENDENT RESTRICTION -  (3.GE. NSTNS .LE.10)
C
 3010 IF (M(1) .LE. 0) GO TO 8
      IF (M(2).LT.3 .OR. M(2).GT.10) GO TO 8
      IF (RM(4) .LE. 0.0) GO TO 8
      DO 3012 L = 6,9
      IF (RM(L) .LE. 0.0) GO TO 8
 3012 CONTINUE
      IF (RM( 3).LE.-90.0 .OR. RM( 3).GE.90.0) GO TO 8
      IF (RM(10).LE.-90.0 .OR. RM(10).GE.90.0) GO TO 8
      N = 10
      GO TO 3
C
C******         82-PARAM         ***********************************
C
 3960 IF (MF(1).NE.3 .OR. MF(2).LE.0 .OR. MF(3).NE.0 .AND.
     1    MF(3).NE.MF(2)) GO TO 3968
      IF (MF(3).NE.0 .AND. MF(3).NE.2 .AND. MF(3).NE.4) GO TO 3968
      DO 3961 L = 4,8
      IF (MF(L) .NE. 0) GO TO 3968
 3961 CONTINUE
      IF (NPARAM+7 .LE. NOPEN) GO TO 3964
      CALL PAGE2 (2)
      WRITE  (NOUT,3962) SFM
 3962 FORMAT (A25,' 330, NO ROOM IN CORE FOR PARAM CARDS.')
 3963 ABORT = .TRUE.
      GO TO 2
 3964 IP = 2*NBUF + NPARAM
      IBUFF(IP+1) = M(1)
      IBUFF(IP+2) = M(2)
      IBUFF(IP+3) = MF(2)
      IBUFF(IP+4) = M(3)
      NPARAM = NPARAM + 4
      IF (MF(2).LE.2 .AND. MF(3).EQ.0) GO TO 2
      IBUFF(IP+5) = M(4)
      NPARAM = NPARAM + 1
      IF (MF(2).LE.4 .AND. MF(3).EQ.0) GO TO 2
      IF (MF(3) .EQ. 4) GO TO 3965
      IBUFF(IP+3) = 5
      GO TO 2
 3965 IBUFF(IP+3) = 6
      IBUFF(IP+6) = M(5)
      IBUFF(IP+7) = M(6)
      NPARAM = NPARAM + 2
      GO TO 2
 3968 WRITE  (NOUT,3969) UFM,M(1),M(2),KNT
 3969 FORMAT (A23,' 331, IMPROPER PARAM CARD ',2A4,10X,
     1       'SORTED CARD COUNT =',I7)
      CALL PAGE2 (2)
      GO TO 3963
C
C*******    12-SPC1(3980), 92-OMIT1(3981), 216-ASET1(3981)   ***********
C          332-CFLSTR(3980)
C
 3980 IZ = 2
      IFILE = 210
      IF (K .EQ. 332) IFILE = 208
      GO TO 3983
 3981 IZ = 1
      IFILE = 210
 3983 IF (KM .NE. 0) GO TO 3990
      KM = 1
      IF (MF(IZ).NE.0 .AND. MF(IZ).NE.1) BADFOR = .TRUE.
      IF (K .EQ. 332) GO TO 3984
      IF (IFPDCO(M(IZ))) BADDAT = .TRUE.
      IF (IZ .NE. 2) GO TO 3986
 3984 IF (MF(1) .NE. 1) BADFOR = .TRUE.
      IF (M(1)  .LE. 0) BADDAT = .TRUE.
 3986 ID = M(1)
      I(1) = M(1)
      IF (IZ .EQ. 2) I(2) = M(2)
      N  = IZ
      L1 = IZ + 1
      IF (MF(IZ+2).EQ.3 .AND. M(IZ+2).EQ.THRU) GO TO 4000
      GO TO 3991
 3990 L1 = 1
 3991 DO 3992 L = L1,8
      IF (MF(L).NE.0 .AND. MF(L).NE.1) BADFOR = .TRUE.
 3992 CONTINUE
      DO 3993 L = L1,8
      IF (MF(L) .EQ. 1) GO TO 3994
 3993 CONTINUE
      BADDAT = .TRUE.
 3994 DO 3998 L = L1,8
      IF (M(L)) 3996,3998,3995
 3995 N = N + 1
      I(N) = M(L)
      GO TO 3998
 3996 BADDAT = .TRUE.
 3998 CONTINUE
      KN = 1
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
 3999 KM = 0
      N  = N + 1
      I(N) =-1
      KN = 0
      GO TO 9
 4000 IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 4001
      KN = 1
      BADFOR = .TRUE.
      GO TO 9
 4001 IF (MF(IZ+1).NE.1 .OR. MF(IZ+3).NE.1) BADFOR = .TRUE.
      IF (M(IZ+1).LE.0 .OR. M(IZ+4).LE.M(IZ+1)) BADDAT = .TRUE.
      DO 4002 L = IZ,4
      IF (MF(L+4) .NE. 0) BADFOR = .TRUE.
 4002 CONTINUE
      IF (BADFOR .OR. BADDAT) GO TO 3999
      CALL WRITE (IFILE,M,IZ,0)
      L1 = M(IZ+1)
      L2 = M(IZ+4)
      L = L1
 4010 CALL WRITE (IFILE,L,1,0)
      L = L + 1
      IF (L .LE. L2) GO TO 4010
      N = 0
      GO TO 3999
C
C******      13-SPCADD, 83-MPCADD    **********************************
C
 4020 IF (KM .EQ. 1) GO TO 4990
      KM = 1
      IF (M(1) .LE. 0) BADDAT = .TRUE.
      ID = M(1)
      I(1) = ID
      IF (M(2).LE.0 .OR. M(3).LT.0) BADDAT = .TRUE.
      IF (M(3) .EQ. 0) CALL PAGE2 (2)
      IF (M(3) .EQ. 0) WRITE (NOUT,4024) UWM
 4024 FORMAT (A25,' 4124, THE SPCADD OR MPCADD UNION CONSISTS OF A ',
     1       'SINGLE SET.')
      N = 1
      GO TO 4992
C
C******        84-LOAD, 123-DLOAD      *******************************
C
 4060 IF (KM .EQ. 1) GO TO 4068
      KM = 1
      IF (MF(1).NE.0 .AND. MF(1).NE.1 .OR. MF(2).NE.0 .AND. MF(2).NE.2)
     1    BADFOR = .TRUE.
      IF (M(1) .LE. 0) BADDAT = .TRUE.
      ID = M(1)
      I(1) = ID
      I(2) = M(2)
      IF (M(4) .LE. 0) BADDAT = .TRUE.
      N = 2
      GO TO 4070
 4068 N = 0
 4070 L8 = N + 1
      DO 4074 L = L8,7,2
      IF (MF(L  ).NE.0 .AND. MF(L).NE.2 .OR. MF(L+1).NE.0 .AND.
     1    MF(L+1).NE.1) BADFOR = .TRUE.
 4074 CONTINUE
 4076 N = N + 2
      IF (M(N)) 4078,4080,4084
 4078 BADDAT = .TRUE.
 4080 N  = N - 2
      L7 = 1
      L8 = N + 1
      DO 4082 L = L8,8
      IF (MF(L) .NE. 0) BADDAT = .TRUE.
 4082 CONTINUE
      IF (N .LE. 0) BADDAT = .TRUE.
      GO TO 4086
 4084 I(N-1) = M(N-1)
      I(N  ) = M(N  )
      IF (N .LT. 8) GO TO 4076
 4086 KN = 1
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 4088
      KM = 0
      N  = N + 2
      I(N-1) =-1
      I(N  ) =-1
      KN = 0
      GO TO 4090
 4088 IF (L7 .NE. 1) GO TO 9
      BADDAT = .TRUE.
 4090 L7 = 0
      GO TO 9
C
C     ******************************************************************
C
   21 IF (.NOT.IAX) GO TO 22
      IF (IH.GT.NN .OR. IH.LT.0) GO TO 25
      GO TO 24
   22 IF (LH) WRITE (NOUT,23) UFM
   23 FORMAT (A23,' 332, AXIC CARD REQUIRED.')
      IF (LH) CALL PAGE2 (2)
      LH = .FALSE.
      ABORT = .TRUE.
   24 GO TO  R, (1489,1507,1521,1451,1461,1471,1497,2031,2041,2172,3,2)
   25 GO TO R1, (8)
C
C***** TEMPORARY UNFIX FOR SPCADD AND MPCADD ***************************
C
 4990 N = 0
 4992 DO 4994 L = 1,8
      IF (MF(L).NE.0 .AND. MF(L).NE.1) BADFOR = .TRUE.
 4994 CONTINUE
 4995 N = N + 1
      IF (M(N)) 4996,4998,5002
 4996 BADDAT = .TRUE.
 4998 N  = N - 1
      L7 = 1
      L8 = N + 1
      DO 5000 L = L8,8
      IF (MF(L) .NE. 0) BADDAT = .TRUE.
 5000 CONTINUE
      IF (N .LE. 0) BADDAT = .TRUE.
      GO TO 5004
 5002 I(N) = M(N)
      IF (N .LT. 8) GO TO 4995
 5004 KN = 1
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 5006
      KM = 0
      N  = N + 1
      I(N) =-1
      KN = 0
      GO TO 5008
 5006 IF (L7 .NE. 1) GO TO 9
      BADDAT = .TRUE.
 5008 L7 = 0
      GO TO 9
C
C******     329-PROLATE     ********************************************
C
 5100 IF (KM .NE. 0) GO TO 5115
      IF (PROL) BADDAT = .TRUE.
      PROL = .TRUE.
      KM = 1
      IF (MF(1).NE.2 .OR. MF(2).NE.2) BADFOR = .TRUE.
      IF (RM(1) .LE. RM(2)) BADDAT = .TRUE.
      DO 5105 L = 3,6
      IF (MF(L) .NE. 1) BADFOR = .TRUE.
      IF (M(L)  .LT. 0) BADDAT = .TRUE.
 5105 CONTINUE
      IF (M(3) .LT.  2) BADDAT = .TRUE.
      IF (M(4) .LT.  2) BADDAT = .TRUE.
      IF (M(5) .GT. 30) BADDAT = .TRUE.
      IF (M(6) .GT. M(5)) M(6) = M(5)
      ID = M(1)
      NSEGS = M(3)
      MSEGS = M(4)
      ITOT1 = (NSEGS-1)*(MSEGS+1) + 2
      ITOT2 = (NSEGS-1)*MSEGS + 2
      DO 5110 L = 1,6
 5110 I(L) = M(L)
      N  = 6
      L1 = 7
      ITEMS = 0
      GO TO 5120
 5115 L1 = 1
 5120 DO 5130 L = L1,8
      IF (MF(L).NE.1 .AND. MF(L).NE.3) BADFOR = .TRUE.
      IF (MF(L) .EQ. 3) GO TO 5140
      ITEMS = ITEMS + 1
      IF (M(L) .LE. 0) BADDAT = .TRUE.
      N = N + 1
      I(N) = M(L)
 5130 CONTINUE
      KN = 1
 5135 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
      BADDAT = .TRUE.
      GO TO 5150
 5140 IF (M(L) .NE. ENDT) GO TO 5145
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) BADDAT = .TRUE.
      GO TO 5150
 5145 BADDAT = .TRUE.
      GO TO 5135
 5150 KM = 0
      KN = 0
      IF (K .EQ. 330) GO TO 9
      IF (ITEMS.NE.ITOT1 .AND. ITEMS.NE.ITOT2) BADDAT = .TRUE.
      GO TO 9
C
C******      330-PERMBDY       *****************************************
C
 5200 IF (KM .NE. 0) GO TO 5210
      IF (PERM) BADDAT = .TRUE.
      PERM = .TRUE.
      KM = 1
 5210 DO 5220 L = 1,8
      IF (MF(L).NE.1 .AND. MF(L).NE.3) BADFOR = .TRUE.
      IF (MF(L) .EQ. 3) GO TO 5140
      IF (M(L)  .LE. 0) BADDAT = .TRUE.
      N = N + 1
      I(N) = M(L)
 5220 CONTINUE
      KN = 1
      GO TO 5135
C
    2 RETURN
      END
