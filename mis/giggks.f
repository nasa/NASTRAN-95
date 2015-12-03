      SUBROUTINE GIGGKS
C
C     THIS SUBROUTINE READS THE SPLINE CARDS AND DETERMINES THE
C     POINTS IN THE G AND K S
C
      INTEGER         BUFF,BUFF1,SYSBUF,BUFF2,EQT(7),
     1                SCARD(5),CCARD(5),SS1(3),LS2(3),CAERO(3),SET1(3),
     2                ST2(3),NS(2),GKSET,TRL(7),TYPE,OUT,SPL3(3),
     3                ATAB(2),PCSTM,PBGPT,PRCP,PTCP,PTE,PRE,CTYP,
     4                SPLINE,USETA,CSTM,BAGPDT,SILA,ECTA,GM,GO,SCR1,
     5                SCR2,SCR3,SCR4,SCR5,NS1,NS2,KSIZE,GSIZE,GTKA
      DIMENSION       C(18),X1B(3),X4B(3),TEMP(3),TEMP1(6),X1E(3),
     1                X4E(3),CB(18),B(6),Z(28)
      DIMENSION       SET2(8),CRARD(16)
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /XMSSG / UFM,UWM,UIM,SFM
      COMMON /SYSTEM/ SYSBUF,OUT
      COMMON /GICOM / SPLINE,USETA,CSTM,BAGPDT,SILA,ECTA ,GM,GO,GTKA,
     1                KSIZE,GSIZE,SCR1,SCR2,SCR3,SCR4,SCR5
      COMMON /CONDAS/ DUM(3),DEGRA
      COMMON /ZZZZZZ/ IZ(1)
C
C     CHANGE IN EQUIV FOR SIZE OF SCARD OR CCARD
C
      EQUIVALENCE    (IZ(1),Z(1),SCARD(1),SET2(1)),(Z(28),NKSET)
      EQUIVALENCE    (Z(11),CCARD(1),CRARD(1))    ,(Z(27),NGSET),
     1               (SET2(3),SP1), (SET2(4),SP2) ,(SET2(5),CH1),
     2               (SET2(6),CH2), (SET2(7),Z1 ) ,(SET2(8),Z2 )
      DATA       C / 18*0.0      /,  SET1   / 3502,35,999/
      DATA     SS1 / 3302,33, 6  /,  LS2    / 3402,34,10 /,
     1         ST2 / 3602,36, 8  /,  CAERO  / 3002,30,16 /
      DATA    SPL3 / 4901,49, 1  /,  ATAB   / 200 ,2     /
      DATA      NS / 4HGIGG,  4HKS          /,IZ2 /2     /
C     DATA    IECT / 3002,46     /
C
C     INITILIZE
C
      CALL SSWTCH (18,I18)
      NWDS = KORSZ(IZ)
      NOGO = 0
      NS1  = 0
      NS2  = 0
      NS3  = 0
C
C     BUFF  HAS SPLINE
C     BUFF1 HAS CSTM,BGPT,EQAERO,SILA,SCR1
C     BUFF2 HAS SCR2
C
      BUFF  = NWDS - SYSBUF - 1
      BUFF1 = BUFF - SYSBUF - 1
      BUFF2 = BUFF1- SYSBUF
C
C     PROCESS SET CARDS AND WRITE G LISTS ON SCR2
C
      IFIL = SCR2
      CALL OPEN (*999,SCR2,IZ(BUFF2+1),1)
      IFIL = SPLINE
      CALL PRELOC (*999,IZ(BUFF+1),SPLINE)
C
C     SET1 CARDS
C
      CALL LOCATE (*340,IZ(BUFF+1),SET1,IDUM)
      N   = 1
      NCO = BUFF2 - N
      CALL READ (*998,*310,SPLINE,IZ(N),NCO,1,NWR)
      GO TO 993
  310 I  = N - 1
      N1 = 0
      ASSIGN 335 TO TYPE
  320 I = I + 1
      IF (IZ(I) .EQ. -1) GO TO 330
      IF (I    .EQ. NWR) GO TO 990
      N1 = N1 + 1
      GO TO 320
  330 IF (N1 .LT. 2) GO TO 9971
      CALL WRITE (SCR2,IZ(N),N1,1)
  335 IF (I .EQ. NWR) GO TO 340
      N  = I + 1
      N1 = 0
      GO TO 320
C
C     SET 2 CARDS
C
  340 CALL LOCATE (*490,IZ(BUFF+1),ST2,IDUM)
C
C     READ IN BAGPDT AND CSTM
C
      N = LS2(3) + CAERO(3) + 1
      TRL(1) = CSTM
      CALL RDTRL (TRL)
      IF (TRL(1) .LT. 0) TRL(3) = 0
      NCSTM = (TRL(3)+1)*14
      PCSTM = BUFF2 - NCSTM
      TRL(1)= BAGPDT
      CALL RDTRL (TRL)
      NBG   = (TRL(2)-TRL(3))*4
      PBGPT = PCSTM - NBG
      IF (PBGPT .LT. N+150) GO TO 993
C
C     READ IN CSTM AT PCSTM + 14 ADD BASIC COORD SYSTEM
C
      IZ(PCSTM  ) = 0
      IZ(PCSTM+1) = 1
      DO 5 I = 2,13
      Z(PCSTM+I) = 0.0
    5 CONTINUE
      Z(PCSTM+5 ) = 1.0
      Z(PCSTM+9 ) = 1.0
      Z(PCSTM+13) = 1.0
      IF (NCSTM .EQ. 14) GO TO 7
      IFIL = CSTM
      CALL GOPEN (CSTM,IZ(BUFF1+1),0)
      CALL READ  (*998,*998,CSTM,IZ(PCSTM+14),NCSTM-14,1,NWR)
      CALL CLOSE (CSTM,1)
    7 CONTINUE
C
C     READ IN BAGPDT AT PBGPT
C
      IFIL = BAGPDT
      CALL GOPEN (BAGPDT,IZ(BUFF1+1),0)
      CALL READ  (*998,*998,BAGPDT,IZ(PBGPT),NBG,1,NWR)
      CALL CLOSE (BAGPDT,1)
C
C     READ IN SET2 CARDS WITH CAERO1 APPENDED
C
      IFIL = SPLINE
      LCA  = 0
      ASSIGN 350 TO TYPE
  350 CALL READ (*998,*490,SPLINE,IZ(1),N-1,0,NWR)
      N1 = 1
      IF (CCARD(1) .EQ. LCA) GO TO 4001
      LCA= CCARD(1)
      K  = PCSTM
      J  = PCSTM + NCSTM - 1
      IF (CCARD(3) .EQ. 0) GO TO 371
      DO 360 I = K,J,14
      IF (CCARD(3) .EQ. IZ(I)) GO TO 370
  360 CONTINUE
      GO TO 990
  370 PRCP = I + 2
      PTCP = I + 5
      CTYP = IZ(I+1)
C
C     LOCATE POINTS 1 AND 4 AS INPUT
C
      GO TO (371,372,373), CTYP
  371 X1B(1) = CRARD(9)
      X1B(2) = CRARD(10)
      X1B(3) = CRARD(11)
      X4B(1) = CRARD(13)
      X4B(2) = CRARD(14)
      X4B(3) = CRARD(15)
      IF (CCARD(3) .EQ. 0) GO TO 390
      GO TO 374
  372 X1B(1) = CRARD( 9)*COS(CRARD(10)*DEGRA)
      X1B(2) = CRARD( 9)*SIN(CRARD(10)*DEGRA)
      X1B(3) = CRARD(11)
      X4B(1) = CRARD(13)*COS(CRARD(14)*DEGRA)
      X4B(2) = CRARD(13)*SIN(CRARD(14)*DEGRA)
      X4B(3) = CRARD(15)
      GO TO 374
  373 X1B(1) = CRARD( 9)*SIN(CRARD(10)*DEGRA)*COS(CRARD(11)*DEGRA)
      X1B(2) = CRARD( 9)*SIN(CRARD(10)*DEGRA)*SIN(CRARD(11)*DEGRA)
      X1B(3) = CRARD( 9)*COS(CRARD(10)*DEGRA)
      X4B(1) = CRARD(13)*SIN(CRARD(14)*DEGRA)*COS(CRARD(15)*DEGRA)
      X4B(2) = CRARD(13)*SIN(CRARD(14)*DEGRA)*SIN(CRARD(15)*DEGRA)
      X4B(3) = CRARD(13)*COS(CRARD(14)*DEGRA)
  374 CALL GMMATS (Z(PTCP),3,3,0, X1B,3,1,0, TEMP)
      X1B(1) = TEMP(1) + Z(PRCP  )
      X1B(2) = TEMP(2) + Z(PRCP+1)
      X1B(3) = TEMP(3) + Z(PRCP+2)
      CALL GMMATS (Z(PTCP),3,3,0, X4B,3,1,0, TEMP)
      X4B(1) = TEMP(1) + Z(PRCP  )
      X4B(2) = TEMP(2) + Z(PRCP+1)
      X4B(3) = TEMP(3) + Z(PRCP+2)
  390 IF (CCARD(2) .EQ. 0) GO TO 399
C
C     FIND ELEMENT COORDINATE SYSTEM
C
      DO 391 I = K,J,14
      IF (CCARD(2) .EQ. IZ(I)) GO TO 392
  391 CONTINUE
      GO TO 990
  392 PRE = I + 2
      PTE = I + 5
      X1B(1) = X1B(1) - Z(PRE  )
      X1B(2) = X1B(2) - Z(PRE+1)
      X1B(3) = X1B(3) - Z(PRE+2)
      X4B(1) = X4B(1) - Z(PRE  )
      X4B(2) = X4B(2) - Z(PRE+1)
      X4B(3) = X4B(3) - Z(PRE+2)
      CALL GMMATS (Z(PTE),3,3,1, X1B(1),3,1,0, X1E)
      CALL GMMATS (Z(PTE),3,3,1, X4B(1),3,1,0, X4E)
      GO TO 400
  399 X1E(1) = X1B(1)
      X1E(2) = X1B(2)
      X4E(1) = X4B(1)
      X4E(2) = X4B(2)
  400 X2E = X1E(1) + CRARD(12)
      Y2E = X1E(2)
      X3E = X4E(1) + CRARD(16)
      Y3E = X4E(2)
C
C     FIND PRISM POINTS
C
 4001 CONTINUE
      PX1 = (1.0-SP1)*(1.0-CH1)*X1E(1) + (1.0-SP1)*CH1*X2E +
     1      SP1*CH1*X3E + SP1*(1.0-CH1)*X4E(1)
      PX2 = (1.0-SP1)*(1.0-CH2)*X1E(1) + (1.0-SP1)*CH2*X2E +
     1      SP1*CH2*X3E + SP1*(1.0-CH2)*X4E(1)
      PX3 = (1.0-SP2)*(1.0-CH2)*X1E(1) + (1.0-SP2)*CH2*X2E +
     1      SP2*CH2*X3E + SP2*(1.0-CH2)*X4E(1)
      PX4 = (1.0-SP2)*(1.0-CH1)*X1E(1) + (1.0-SP2)*CH1*X2E +
     1      SP2*CH1*X3E + SP2*(1.0-CH1)*X4E(1)
C
C     CHECK FOR BAD GEOMETRY
C
      IF (PX1.GT.PX2 .OR. PX4.GT.PX3) GO TO 997
      PY1 = (1.0-SP1)*(1.0-CH1)*X1E(2) + (1.0-SP1)*CH1*Y2E +
     1      SP1*CH1*Y3E + SP1*(1.0-CH1)*X4E(2)
      PY2 = (1.0-SP1)*(1.0-CH2)*X1E(2) + (1.0-SP1)*CH2*Y2E +
     1      SP1*CH2*Y3E + SP1*(1.0-CH2)*X4E(2)
      PY3 = (1.0-SP2)*(1.0-CH2)*X1E(2) + (1.0-SP2)*CH2*Y2E +
     1      SP2*CH2*Y3E + SP2*(1.0-CH2)*X4E(2)
      PY4 = (1.0-SP2)*(1.0-CH1)*X1E(2) + (1.0-SP2)*CH1*Y2E +
     1      SP2*CH1*Y3E + SP2*(1.0-CH1)*X4E(2)
C
C     BUILD PRISM INEQUALITY MATRICES
C
      C(1) = PY1 - PY2
      C(2) = PX2 - PX1
      C(4) = PY2 - PY3
      C(5) = PX3 - PX2
      C(7) = PY3 - PY4
      C(8) = PX4 - PX3
      C(10)= PY4 - PY1
      C(11)= PX1 - PX4
      C(15)= 0.0
      C(18)= 0.0
      B(1) = PX2*PY1 - PX1*PY2
      B(2) = PX3*PY2 - PX2*PY3
      B(3) = PX4*PY3 - PX3*PY4
      B(4) = PX1*PY4 - PX4*PY1
      NR   = 4
      IF (Z1 .EQ. 0.0) GO TO 401
      C(15)=-1.0
      B(5) =-Z1
      NR   = 5
  401 IF (Z2 .EQ. 0.0) GO TO 404
      IF (Z1 .EQ. 0.0) GO TO 403
      C(18)= 1.0
      B(6) = Z2
      NR   = 6
      GO TO 404
  403 C(15)= 1.0
      B(5) = Z2
      NR   = 5
C
C     CONVERT TO BASIC
C
  404 IF (CCARD(2) .EQ. 0) GO TO 406
      CALL GMMATS (C,NR,3,0, Z(PTE),3,3,1, CB)
      CALL GMMATS (Z(PTE),3,3,1, Z(PRE),3,1,0, TEMP)
      CALL GMMATS (C,NR,3,0, TEMP,3,1,0, TEMP1)
      B(1) = B(1) + TEMP1(1)
      B(2) = B(2) + TEMP1(2)
      B(3) = B(3) + TEMP1(3)
      B(4) = B(4) + TEMP1(4)
      IF (NR .EQ. 4) GO TO 405
      B(5) = B(5) + TEMP1(5)
      IF (NR .EQ. 5) GO TO 405
      B(6) = B(6) + TEMP1(6)
      GO TO 405
  406 DO 407 I = 1,18
  407 CB(I) = C(I)
  405 CONTINUE
C
C     FINALLY TEST ALL GRID POINTS TO SEE IF THEY ARE IN PRISM
C
      KK = PBGPT
      KKK= KK + NBG - 1
      DO 440 K = KK,KKK,4
      IF (IZ(K) .EQ. -1) GO TO 440
      JJ = 0
      DO 430 I = 1,NR
      SUM = 0.0
      DO 420 J = 1,3
      JJ  = JJ + 1
      SUM = SUM + CB(JJ)*Z(K+J)
  420 CONTINUE
      IF (SUM .LT. B(I)) GO TO 440
  430 CONTINUE
C
C     FOUND ONE
C
      N1 = N1 + 1
      IZ(N1) = (K-PBGPT)/4 + 1
  440 CONTINUE
      IF (N1  .LT. 2) GO TO 997
      IF (I18 .EQ. 0) GO TO 446
      WRITE  (OUT,445) (IZ(II),II=1,N1)
  445 FORMAT (5H0SET2 ,I8,2X,(/,10I9))
  446 CONTINUE
      CALL WRITE (SCR2,IZ(1),N1,1)
      GO TO 350
  490 CALL CLOSE (SCR2,1)
      CALL OPEN  (*999,SCR2,IZ(BUFF2+1),0)
      NEQ = KSIZE*3
      EQT(1) = SILA
      CALL RDTRL (EQT)
      NSIL = EQT(2)
      IEQ  = BUFF2 - NEQ - NSIL
C
C     INITIAL CORE CHECK  PLUS FUDGE FACTOR
C
      IF (IEQ-150 .LT. 0) GO TO 993
C
C     READ SPLINE FOR K POINT POINTERS
C
C     READ SILA
C
      CALL LOCATE (*990,IZ(BUFF+1),ATAB,IDUM)
      CALL READ (*998,*11,SPLINE,IZ(IEQ),NEQ+1,0,NWR)
      GO TO 990
   11 NEQ  = NWR
      IFIL = SILA
      CALL GOPEN (SILA,IZ(BUFF1+1),0)
      CALL READ  (*998,*998,SILA,IZ(IEQ+NEQ),NSIL,1,NWR)
      CALL CLOSE (SILA,1)
      IFIL  = SPLINE
      TRL(1)= SCR1
      MAX   = 0
      CALL GOPEN (SCR1,IZ(BUFF1+1),1)
C
C     N = LENGTH OF LONGEST SPLINE CARD + CAERO1 CARD + 3
C     N  POINTS TO 1 ST LOCATION OF CORE AVAILABLE SEE EQIV
C
      N   = LS2(3) + CAERO(3) + 3
      NCO = IEQ - N
C
C     READ SPLINE1 CARDS
C
      CALL LOCATE (*100,IZ(BUFF+1),SS1,IDUM)
      ASSIGN 10 TO TYPE
      NR = LS2(3) + CAERO(3)
   10 CALL READ (*998,*100,SPLINE,IZ(1),NR,0,NWR)
      NS1 = NS1 + 1
      ASSIGN 30 TO GKSET
      GO TO 300
C
C     G AND K SET ARE IN CORE SORTED  BY INTERNAL NUMBERS
C     A SECOND SET OF G   ARE SORTED  BY SIL NUMBERS
C     A SECOND SET OF K   ARE IN CORE BY K NUMBER
C     NK POINTS TO K SET
C     N1 IS FIRST LOCATION OF OPEN CORE
C     NGSET IS THE NUMBER OF G  NKSET FOR K
C
   30 IF (NOGO .EQ. 1) GO TO 10
C
C     WRITE ALL SPLINE1 DATA ON SCR1 AS PROCESSED
C     ID OF SPLINE1 = 1
C
      IZ(IZ2) = 1
      NW  = N1 - 1
      MAX = MAX0(MAX,NW)
      CALL WRITE (SCR1,IZ(1),NW,1)
      GO TO 10
C
C     END OF SPLINE1 CARDS
C
C     READ SPLINE2 CARDS
C
  100 CALL LOCATE (*190,IZ(BUFF+1),LS2,IDUM)
      ASSIGN 110 TO TYPE
      NR = LS2(3) + CAERO(3)
  110 CALL READ (*998,*190,SPLINE,IZ(1),NR,0,NWR)
      NS2 = NS2 + 1
      ASSIGN 120 TO GKSET
      GO TO 300
C
C     ID OF SPLINE2 = 2
C
  120 IF (NOGO .EQ. 1) GO TO 110
      IZ(IZ2) = 2
      NW  = N1 - 1
      MAX = MAX0(MAX,NW)
      CALL WRITE (SCR1,IZ(1),NW,1)
      GO TO 110
C
C     END OF SPLINE2 CARDS
C
  190 CALL CLOSE (SCR1,1)
      CALL CLOSE (SCR2,1)
      CALL GOPEN (SCR3,IZ(BUFF1+1),1)
C
C     SPLINE 3 CARDS TO SCR3
C
      CALL LOCATE (*290,IZ(BUFF+1),SPL3,IDUM)
      CALL READ (*998,*200,SPLINE,IZ,IEQ,0,NS3)
      GO TO 993
  200 N = NS3 + 1
C
C     CONVERT AERO IDS TO K COLUMN NUMBERS, BUILD A LIST OF SPLINE CARD
C     POINTERS, SORT ON K COLUMNS, PROCESS CARDS IN SORTED ORDER GET
C     G POINTS TO SILS
C
      N1 = 1
      NW = IEQ - 1
      ASSIGN 240 TO TYPE
      I = N
  210 K = IZ(N1+3)
      DO 220 J = 1,NEQ,3
      IF (K .EQ. IZ(NW+J)) GO TO 230
  220 CONTINUE
      GO TO 992
  230 IZ(N1+3) = IZ(NW+J+2)
      IZ(I   ) = N1
      IZ(I +1) = IZ(N1+3)
      I  = I+2
  240 N1 = N1 + IZ(N1) + 1
      IF (N1 .GE. NS3) GO TO 250
      GO TO 210
  250 NW  = I - N
      NS3 = NW/2
      IF (NS3 .EQ. 0) GO TO 1001
      IF (NS3 .EQ. 1) GO TO 255
      CALL SORT (0,0,2,2,IZ(N),NW)
C
C     PROCESS BY SORTED ORDER
C
  255 N  = N - 1
      J  = IEQ + NEQ - 1
      JJ = 5
      DO 280 I = 1,NW,2
      N1 = IZ(N+I)
      JJJ= IZ(N1) - CAERO(3)
      DO 260 K = JJ,JJJ,3
      L  = IZ(N1+K)
      IZ(N1+K) = IZ(J+L)
  260 CONTINUE
      CALL WRITE (SCR3,IZ(N1+1),IZ(N1),1)
  280 CONTINUE
  290 CALL CLOSE (SPLINE,1)
      CALL CLOSE (SCR3,1)
      CALL DMPFIL (SCR1,Z,NWDS)
      CALL DMPFIL (SCR3,Z,NWDS)
      TRL(2) = MAX
      TRL(3) = NS1 + NS2
      CALL WRTTRL (TRL)
      IF (NOGO .EQ. 1) GO TO 1001
      IF (NS1.EQ.0 .AND. NS2.EQ.0 .AND. NS3.EQ.0) GO TO 990
      GO TO 1000
C
C     SET 1 CARDS
C     SET 2 CARDS
C
  300 NGSET = 0
      IFIL  = SCR2
  301 CALL READ (*996,*996,SCR2,IZ(N),1,0,NWR)
      IF (SCARD(5) .EQ. IZ(N)) GO TO 305
      CALL FWDREC (*998,SCR2)
      GO TO 301
  305 CALL READ (*998,*306,SCR2,IZ(N),NCO,1,NWR)
      GO TO 993
  306 CALL REWIND (SCR2)
      IFIL  = SPLINE
      NGSET = NWR
      N1 = N+NGSET
      CALL SORT (0,0,1,1,IZ(N),NGSET)
C
C     GET K SET
C
      NK   = N1 -1
      NKSET= 0
      NMIN = SCARD(3)
      NMAX = SCARD(4)
      NCORD= CCARD(5)
      IFRST= CCARD(1)
      IF (NMIN .GT. NMAX) GO TO 990
      J1 = NCORD*CCARD(4) + IFRST - 1
      IF (NMIN.LT.IFRST .OR. NMAX.GT.J1) GO TO 990
      J1 = (NMIN-IFRST)/NCORD + 1
      I1 = (NMIN-IFRST) - NCORD*(J1-1) + 1
      JL = (NMAX-IFRST)/NCORD + 1
      IL = (NMAX-IFRST) - NCORD*(JL-1) + 1
      DO 530 J = J1,JL
      DO 520 I = I1,IL
      IZ(N1) = IFRST + (I-1) + NCORD*(J-1)
      N1 = N1 + 1
      NKSET = NKSET + 1
  520 CONTINUE
  530 CONTINUE
C
C     MAKE A LIST OF SIL NUMBERS   FOR G SET
C
      NW = NGSET
      J = IEQ + NEQ - 1
      DO 610 I = 1,NW
      K = IZ(N+I-1)
      IZ(N1) = IZ(K+J)
      N1 = N1 + 1
  610 CONTINUE
C
C     FIND INTERNAL K POINT NUMBER  FOR BGPT PLUS K NUMBER
C
      JJ = 1
      NW = IEQ - 1
      DO 560 I = 1,NKSET
      DO 540 J = JJ,NEQ,3
      IF (IZ(NK+I) .EQ. IZ(NW+J)) GO TO 550
  540 CONTINUE
      GO TO 991
  550 JJ = J
      IZ(NK+I) = IZ(NW+J+1)
      IZ(N1  ) = IZ(NW+J+2)
      N1 = N1 + 1
  560 CONTINUE
      GO TO GKSET, (30,120)
C
C     ERROR MESSAGES
C
  999 CALL MESAGE (-1,IFIL,NS)
  998 CALL MESAGE (-3,IFIL,NS)
  993 CALL MESAGE (-8,0,NS)
  990 CALL MESAGE (-7,0,NS)
 9971 SCARD(1) = IZ(N)
  997 WRITE  (OUT,9970) UWM,SCARD(5),SCARD(1)
 9970 FORMAT (A25,' 2257, SET',I9,' REFERENCED ON SPLINE CARD',I9,
     1       ' IS EMPTY.')
      GO TO 901
  996 WRITE  (OUT,9960) UFM,SCARD(5),SCARD(1)
 9960 FORMAT (A23,' 2258, SET',I9,' REFERENCED ON SPLINE CARD',I9,
     2       ' NOT FOUND OR IT IS EMPTY.')
      CALL REWIND (SCR2)
      GO TO 900
  991 WRITE  (OUT,9910) SFM,IZ(NK+I-1),CCARD(1)
 9910 FORMAT (A25,' 2259, POINT ASSIGNED TO BOX',I9,' FOR CAER01',I9,
     1       ' NOT IN EQAERO.')
      GO TO 900
  992 WRITE (OUT,9910) K,IZ(N1+2)
      GO TO 900
 1001 CALL MESAGE (-61,0,NS)
  900 NOGO = 1
  901 GO TO TYPE, (10,100,240,335,350)
 1000 RETURN
      END
