      SUBROUTINE GPCYC
C
C     GPCYC IS THE GEOMETRY PROCESSOR FOR CYCLIC PROBLEM
C
C     INPUT DATA BLOCKS - GEOM4,EQEXIN,USET
C
C     OUTPUT DATA BLOCKS - CYCD
C
C     PARAMETERS  CTYPE - INPUT,BCD -
C                 NOGO  - OUTPUT--+1 UNLESS ERROR--THEN-1
C
C     SCRATCH FILES (2)
C     DEFINITION OF VARIABLES
C     NZ       OPEN CORE LENGTH
C     NX       ORIGINAL OPEN CORE
C     NENT     NUMBER OF ENTRIES IN EQEXIN
C     ITYP     PROBLEM TYPE (ROT=0 ,OTHERWISE=1)
C     LCYJ     LENGTH OF CJOIN CARDS
C     ISID1    POINTER TO START OF SIDE 1 CZRDS
C     ISID2    POINTER TO START OF SIDE 2 CZRDS
C
      EXTERNAL        ANDF
      INTEGER         GEOM4,EQEXIN,USET,CYCD,CTYPE,SYSBUF,FILE,NAME(2),
     1                SCR1,SCR2,REC,CYL,SPH,ROT,CYJOIN(2),IB(5),ANDF,
     2                IBB(4),MCB(7),BLK
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /TWO   / ITWO(32)
      COMMON /BITPOS/ ISK(6),IUA
      COMMON /SYSTEM/ KSYSTM(65)
      COMMON /ZZZZZZ/ IZ(1)
      COMMON /BLANK / CTYPE(2),NOGO
      EQUIVALENCE     (KSYSTM(1),SYSBUF),(KSYSTM(2),NOUT)
      DATA    GEOM4 , EQEXIN,USET,CYCD,SCR1,SCR2,NAME         /
     1        101   , 102   ,103 ,201 ,301 ,302 ,4HGPCY,4HC   /
      DATA    ROT   / 4HROT /,REC,CYL,SPH  / 1HR,1HC,1HS      /
      DATA    CYJOIN/ 5210,52  /
      DATA    NOCY  , NOSID1, ISID1, IBLEN, ICM,  ISAM, NOCNT, NOPAR /
     1        4024  , 4025,   4026,  4027,  4028, 4029, 4030,  4032  /
      DATA    NOEQ  , NCORD    /
     1        4037  , 4039     /
      DATA    MCB   / 7*0/, BLK/ 1H  /
C
C
      NZ   = KORSZ(IZ)
      NOGO = 1
C
C     IBUF1 IS PRELOC BUFFER
C
      IBUF1 = NZ - SYSBUF
      IBUF2 = IBUF1 - SYSBUF
      NZ = IBUF2 - 1
      NX = NZ
      IF (NZ .LE. 0) CALL MESAGE (-8,0,NAME)
C
C     PUT  SECOND RECORD OF EQEXIN ITO CORE
C
      FILE = EQEXIN
      CALL GOPEN (EQEXIN,IZ(IBUF1),0)
      CALL FWDREC (*560,EQEXIN)
      CALL READ (*560,*10,EQEXIN,IZ,NZ,0,IFLAG)
      CALL MESAGE (-8,0,NAME)
   10 CALL CLOSE (EQEXIN,1)
      NENT = IFLAG/2
C
C     DECIDE ON TYPE
C
      ITYP = 1
      IF (CTYPE(1) .EQ. ROT) ITYP = 0
C
C     FIND  CYJOIN CARDS ON GEOM4
C
      FILE = GEOM4
      CALL PRELOC (*540,IZ(IBUF1),GEOM4)
      CALL LOCATE (*580,IZ(IBUF1),CYJOIN,IDUM)
      NZ = NZ - IFLAG
      K  = IFLAG + 1
      CALL READ (*560,*20,GEOM4,IZ(K),NZ,0,LCYJ)
      CALL MESAGE (-8,0,NAME)
   20 CALL CLOSE (GEOM4,1)
      LCYJ = LCYJ + K - 1
      IF (IZ(K) .EQ. 1) GO TO 40
      WRITE  (NOUT,590) UFM,NOSID1
      WRITE  (NOUT,30)
   30 FORMAT ('0NO SIDE 1 DATA FOUND.')
      GO TO 620
   31 WRITE  (NOUT,590) UFM,NOSID1
      WRITE  (NOUT,32)
   32 FORMAT ('0NO SIDE 2 DATA FOUND.')
      GO TO 620
C
C     FIND SIDE TWO DATA
C
   40 L = K
   50 IF (L .GT. LCYJ) GO TO  31
      IF (IZ(L) .EQ. -1) GO TO 70
   60 L = L + 1
      GO TO 50
C
C     END OF CARD FOUND
C
   70 IF (L+1  .GT. LCYJ) GO TO 31
      IF (IZ(L+1) .EQ. 2) GO TO 90
      IF (ITYP    .EQ. 1) GO TO 60
      WRITE  (NOUT,590) UFM,ISID1
      WRITE  (NOUT,80)
   80 FORMAT ('0TOO MANY SIDE 1 CARDS.')
      GO TO 620
C
C     FOUND SIDE TWO LIST
C
   90 ISID2 = L + 1
      IF (ITYP .NE. 0) GO TO 370
C
C     CHECK LENGTH OF SIDE TWO LIST
C
      NS1 = ISID2 - K - 4
      NS2 = LCYJ - ISID2 - 3
      IF (NS1 .EQ. NS2) GO TO 110
      WRITE  (NOUT,590) UFM,IBLEN
      WRITE  (NOUT,100)
  100 FORMAT ('0NUMBER OF ENTRIES IN SIDE 1 NOT EQUAL TO NUMBER IN ',
     1        'SIDE 2')
      NOGO = -1
      GO TO 620
C
C     BUILD 5 WORDS FOR EACH PAIR
C
C
C     FIVE WORD ENTRY FOR EACH PAIR APPEARS AS FOLLOWS
C
C     1        CODE(1 = GRID   2 = SCALAR)
C     2        INTERNAL INDEX (SIL)      SIDE 1
C     3        GRID ID (EXTERNAL)       SIDE 1
C     4        INTERNAL INDEX (SIL)      SIDE 2
C     5        GRID ID (EXTERNAL)       SIDE 2
C
  110 CALL GOPEN (SCR1,IZ(IBUF1),1)
      L = ISID2 + 3
      K = K + 3
      DO 160 I = 1,NS1
      IF (IZ(K) .NE. IZ(L)) GO TO  130
      WRITE  (NOUT,590) UFM,ISAM
      WRITE  (NOUT,120) IZ(K)
  120 FORMAT ('0GRID POINT',I10,' APPEARS IN BOTH SIDE LISTS.')
      GO TO 620
  130 CONTINUE
      IP = IZ(K)
      CALL BISLOC (*610,IP,IZ(1),2,NENT,M)
      IX1 = IZ(M+1)/10
      IC1 = IZ(M+1) - IX1*10
      IP  = IZ(L)
      CALL BISLOC (*610,IP,IZ(1),2,NENT,M)
      IX2 = IZ(M+1)/10
      IC2 = IZ(M+1) - IX2*10
      IF (IC1 .EQ. IC2) GO TO 150
      WRITE  (NOUT,590) UFM,ICM
      WRITE  (NOUT,140) IZ(K),IZ(L)
  140 FORMAT ('0THE CODE FOR GRID POINT',I10,' DOES NOT MATCH THE CODE',
     1        ' FOR GRID POINT',I10)
      GO TO 620
  150 IB(1) = IC1
      IB(2) = IX1
      IB(3) = IZ(K)
      IB(4) = IX2
      IB(5) = IZ(L)
      CALL WRITE (SCR1,IB,5,0)
      K = K + 1
      L = L + 1
  160 CONTINUE
  170 CALL WRITE (SCR1,0,0,1)
      CALL CLOSE (SCR1,1)
C
C     SET UP USET
C
      CALL GOPEN (USET,IZ(IBUF1),0)
      FILE = USET
      NZ   = NX
      CALL READ (*560,*190,USET,IZ,NZ,0,LUSET)
      CALL MESAGE (-8,0,NAME)
  190 CALL CLOSE (USET,1)
C
C     SET UP REDUCED USET TABLE
C
      K = 0
      M = ITWO(IUA)
      DO 220 I = 1,LUSET
      IF (ANDF(IZ(I),M)) 210,200,210
  200 IZ(I) = 0
      GO TO 220
  210 K = K + 1
      IZ(I) = -K
  220 CONTINUE
      LUA = K
C
C     FORM SILA VALUES
C
      FILE = SCR1
      CALL GOPEN (SCR1,IZ(IBUF1),0)
      CALL GOPEN (SCR2,IZ(IBUF2),1)
      IF (ITYP .NE. 0) GO TO  410
  230 CALL READ (*560,*300,SCR1,IB(1),5,0,IFLAG)
      NP = 1
      IF = 0
      IF (IB(1) .EQ. 1) NP = 6
      K = 0
  240 L = IB(2) + K
      M = IB(4) + K
C
C     IF NEITHER IGNORE
C
      IF (IZ(L).EQ.0 .AND. IZ(M).EQ.0) GO TO 280
      IF (IZ(L).LT.0 .AND. IZ(M).LT.0) GO TO 270
      WRITE  (NOUT,250) UWM,NOCNT
  250 FORMAT (A25,I5)
      M = K + 1
      WRITE  (NOUT,260) M,IB(3),IB(5)
  260 FORMAT ('0COMPONENT',I4,' OF GRID POINTS',I10,5H AND ,I10,
     1        ' CANNOT BE CONNECTED.')
      GO TO 280
  270 IF = IF + 1
      IBB(1) = IABS(IZ(L))
      IBB(2) = IB(3)
      IBB(3) = IABS(IZ(M))
      IBB(4) = IB(5)
      CALL WRITE (SCR2,IBB,4,0)
  280 K = K + 1
      IF (K .NE. NP) GO TO 240
      IF (IF .NE. 0) GO TO 230
      WRITE  (NOUT,250) UWM,NOPAR
      WRITE  (NOUT,290) IB(3),IB(5)
  290 FORMAT ('0NO COMPONENTS OF GRID POINTS',I10,5H AND ,I10,
     1        ' WERE CONNECTED.')
      GO TO 230
C
C     CLOSE UP
C
  300 CALL WRITE (SCR2,0,0,1)
      CALL CLOSE (SCR1,1)
      CALL CLOSE (SCR2,1)
C
C     BUILD CYCD
C
      DO 310 I = 1,LUA
      IZ(I) = 0
  310 CONTINUE
      FILE = SCR2
      CALL GOPEN (SCR2,IZ(IBUF1),0)
      IF (ITYP .NE. 0) GO TO 520
  320 CALL READ (*560,*360,SCR2,IBB,4,0,IFLAG)
      K = IBB(1)
      M = IBB(3)
      IF (IZ(K) .EQ. 0) GO TO 340
      WRITE  (NOUT,590) UFM,NOEQ
      WRITE  (NOUT,330) IBB(2)
  330 FORMAT ('0GRID POINT',I10,' IS LISTED MORE THAN ONCE.')
      NO GO = -1
  340 IF (IZ(M) .EQ. 0) GO TO 350
      WRITE (NOUT,590) UFM,NOEQ
      WRITE (NOUT,330) IBB(4)
      NOGO  = -1
  350 IZ(K) =  M
      IZ(M) = -K
      GO TO  320
C
C     END OF PAIRS
C
  360 CALL CLOSE (SCR2,1)
      CALL GOPEN (CYCD,IZ(IBUF1),1)
      CALL WRITE (CYCD,IZ(1),LUA,1)
      CALL CLOSE (CYCD,1)
      MCB(1) = CYCD
      MCB(2) = ITYP + 1
      MCB(3) = LUA
      CALL WRTTRL (MCB)
      IF (NOGO .NE. -1) RETURN
      GO TO 620
C
C     1. DIHEDRAL TYPE
C
C     BUILD FIVE WORD LIST
C
C
C     FIVE WORD ENTRY FOR EACH POINT IN SIDE 1 OR SIDE TWO LOOKS AS
C         FOLLOWS
C     1        SIDE (1,2)
C     2        COORD SYS (R = 1,C = 1,S = 2,BLANK = 0)
C     3        CODE ( 1 = GRID   2 = SCALAR)
C     4        INTERNAL INDEX (SIL)
C     5        GRID ID (EXTERNAL)
C
  370 L = K
      CALL GOPEN (SCR1,IZ(IBUF1),1)
  380 ICID = IZ(L+1)
      ISID = IZ(L  )
      IF (ICID .EQ. REC) ICID = 1
      IF (ICID .EQ. CYL) ICID = 1
      IF (ICID .EQ. SPH) ICID = 2
      IF (ICID .EQ. BLK) ICID = 0
      L = L + 3
  390 IF (IZ(L) .EQ. -1) GO TO 400
      IP = IZ (L)
      CALL BISLOC (*610,IP,IZ(1),2,NENT,M)
      IB(1) = ISID
      IB(2) = ICID
      IB(4) = IZ(M+1)/10
      IB(3) = IZ(M+1) - IB(4)*10
      IB(5) = IP
      CALL WRITE (SCR1,IB,5,0)
      L = L + 1
      GO TO 390
C
C     END OF LIST
C
  400 IF (L .GE. LCYJ) GO TO 170
      L = L + 1
      GO TO 380
C
C     END OF CYJOIN LISTS
C
C
C     PRODUCE CYCD CODES
C
  410 CALL READ (*560,*300,SCR1,IB(1),5,0,IFLAG)
      NP = 1
      IF (IB(3) .EQ. 1) NP = 6
      IF = 0
      K  = 0
      IF (IB(1) .EQ. 2) IB(1) = IB(1) + 1
  420 L = IB(4) + K
      IF (IZ(L) .EQ. 0) GO TO 500
C
C     POINT IS IN  A SET
C
      IBB(2) = IABS(IZ(L))
      IBB(3) = IB(5)
      IF (IB(3) .EQ. 2) GO TO 480
      IF (IB(2) .EQ. 1) GO TO 440
      IF (IB(2) .EQ. 2) GO TO 460
C
C     COORD SYS = 0
C
      WRITE  (NOUT,590) UFM,NCORD
      WRITE  (NOUT,430) IBB(3)
  430 FORMAT ('0NO COORDINATE SYSTEM DEFINED FOR GRID POINT',I10)
      NOGO = -1
      GO TO 480
C
C     RECTANGULAR OR CYL
C
  440 IF (MOD(K+1,2) .EQ. 1) GO TO 480
  450 M = 1
      GO TO 490
C
C     SPH
C
  460 IF (K.LT.2 .OR. K.EQ.5 .OR. NP.LT.3 .OR. NP.EQ.6) GO TO 480
      GO TO 450
C
C     EVEN
C
  480 M = 0
  490 IBB(1) = IB(1) + M
      IF = IF + 1
      CALL WRITE (SCR2,IBB,3,0)
  500 K = K + 1
      IF (K .NE. NP) GO TO 420
      IF (IF .NE. 0) GO TO 410
      WRITE  (NOUT,250) UWM,NOPAR
      WRITE  (NOUT,510) IB(5)
  510 FORMAT ('0NO COMPONENTS OF GRID POINT',I10,' WERE IN THE A SET')
      GO TO 410
C
C     BUILD CYCD FOR DIH
C
  520 CALL READ (*540,*360,SCR2,IBB,3,0,IFLAG)
      K = IBB(2)
      IF (IZ(K) .EQ. 0) GO TO 530
      WRITE (NOUT,590) UFM,NOEQ
      WRITE (NOUT,330) IBB(3)
      NOGO  = -1
  530 IZ(K) = IBB(1)
      GO TO 520
C
C     ERROR MESSAGES
C
  540 IP1 = -1
  550 CALL MESAGE (IP1,FILE,NAME)
      RETURN
  560 IP1 = -2
      GO TO 550
  580 WRITE  (NOUT,590) UFM,NOCY
  590 FORMAT (A23,I5)
      WRITE  (NOUT,600)
  600 FORMAT ('0NO CYJOIN CARDS WERE SUPPLIED.')
      GO TO 620
  610 CALL MESAGE (-30,2,IP)
  620 CALL MESAGE (-61,0,NAME)
      RETURN
      END
