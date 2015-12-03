      SUBROUTINE TABPCH
C
C     THE TABPCH MODULE WILL PUNCH UP TO 5 TABLES INTO DTI CARDS
C
C     DMAP CALL IS
C
C     TABPCH  IN1,IN2,IN3,IN4,IN5//P1,P2,P3,P4,P5
C
C     SINGLE FIELD CARDS WILL BE MADE UNLESS REAL NUMBERS ARE TO BE MADE
C     ALL REAL NUMBERS ARE ASSUMED TO BE SINGLE PRECISION.
C
C     LAST REVISED, 3/93, BY G.CHAN/UNISYS
C     PUNCH KELM, MELM AND BELM IN D.P. IF THESE DATA BLOCKS ARE IN D.P.
C
C  $MIXED_FORMATS
C
      INTEGER          SYSBUF    ,IZ(10)    ,IFNM(5)   ,NAME(2)   ,
     1                 MCB(7)    ,FILE      ,TABNM(2)  ,DTI(2)    ,
     2                 DTIS(2)   ,IDATA(20) ,ENDREC(2) ,OUT       ,
     3                 IFORM(20) ,BLANK     ,INT(2)    ,IREAL(2)  ,
     4                 LL(4)     ,INTD(2)   ,PFORM(30) ,IBCD(2)   ,
     5                 SP(3)     ,IBCDD(2)  ,FORM(30,2),FORMS(30,2)
      REAL             RDATA(20)
      DOUBLE PRECISION DZ(1)
      CHARACTER        UFM*23    ,UWM*25    ,UIM*29
      COMMON /XMSSG /  UFM       ,UWM       ,UIM
      COMMON /MACHIN/  MACH
      COMMON /SYSTEM/  SYSBUF    ,OUT       ,KSYSTM(88),LPCH
      COMMON /ZZZZZZ/  Z(1)
      COMMON /BLANK /  N1(2,5)
      EQUIVALENCE      (Z(1),IZ(1),DZ(1)),  (IDATA(1),RDATA(1))
      DATA    BLANK /  1H             /
      DATA    DTI   /  4HDTI , 1H     /
      DATA    DTIS  /  4HDTI*, 1H     /
      DATA    ENDREC/  4HENDR, 4HEC   /
      DATA    FORMS /  4H(2A4, 26*2H  ,4H,1H+ ,4HA2,I, 4H5)  , 4H(A1,,
     1                 4HA2,I ,4H5    ,24*2H  ,4H,1H+, 4HA2,I, 4H5)  /
      DATA    IBCD  /  4H,2A4, 1H     /
      DATA    IBCDD /  4H,2A4, 4H,8X  /
      DATA    IFNM  /  101, 102, 103, 104, 105/
      DATA    INT   /  4H,I8 , 1H     /
      DATA    INTD  /  4H,I16, 1H     /
      DATA    IPLUS /  1H+            /
      DATA    IREAL /  4H,E16, 4H.9   /
      DATA    ISTAR /  1H*            /
      DATA    NAME  /  4HTABP, 4HCH   /
      DATA    LL    /  1, 1, 3, 2     /
      DATA    NSP   ,  SP  / 3, 4HKELM, 4HMELM, 4HBELM /
C
      NZ    = KORSZ(Z)
      IBUF  = NZ - SYSBUF + 1
      NZ    = IBUF - 1
      ICRQ  = 10 - NZ
      IF (NZ .LE. 10) GO TO 830
      NREAD = NZ/2  - 2
      NLIST = NREAD + 3
      DO 10 J = 1,2
      DO 10 I = 1,30
      FORM(I,J) = FORMS(I,J)
   10 CONTINUE
C
C     FOR EACH  TABLE DEFINED
C
      NS = -1
      DO 720 I = 1,5
      MCB(1) = IFNM(I)
      CALL RDTRL (MCB)
      IF (MCB(1) .LE. 0) GO TO 720
C
C     TABLE EXISTS SET IT UP
C
      FILE = IFNM(I)
      CALL OPEN  (*800,FILE,IZ(IBUF),0)
      CALL FNAME (FILE,TABNM)
      IO  = 0
      KMB = 4
      IF (MCB(5).EQ.1 .OR. MCB(5).EQ.3) GO TO 40
      DO 20 J = 1,NSP
      IF (KMB.EQ.1 .OR. TABNM(1).NE.SP(J)) GO TO 20
      KMB = 1
      IO  = 1
      NREAD = NZ -1
   20 CONTINUE
      IF (NS .NE. -1) GO TO 40
      NS = 1
      CALL PAGE1
      WRITE  (OUT,30) UWM
   30 FORMAT (A25,', MODULE TABPCH ASSUMES ALL REAL DATA ARE IN S.P..',
     1       '  D.P. DATA THEREFORE MAY BE PUNCHED ERRONEOUSLY')
      IF (MACH.EQ.5 .OR. MACH.EQ.6 .OR. MACH.EQ.10 .OR. MACH.EQ.21)
     1   WRITE (OUT,35)
   35 FORMAT (4X,'(ALL INTEGERS EXCEEDING 16000 ARE PUNCHED AS REAL ',
     1        'NUMBERS. ALL REAL NUMBERS OUTSIDE E-27 OR E+27 RANGE ',
     2        'ARE PUNCHED AS INTEGERS)')
C
   40 CALL READ (*810,*820,FILE,IZ(1),-2,0,ILEN)
      IRECNO = 0
      ICHR   = N1(1,I)
      IZ(3)  = 0
C
C     SET UP FIRST RECORD
C
      IZ(1) = TABNM(1)
      IZ(2) = TABNM(2)
      IZ(4) = MCB(2)
      IZ(5) = MCB(3)
      IZ(6) = MCB(4)
      IZ(7) = MCB(5)
      IZ(8) = MCB(6)
      IZ(9) = MCB(7)
      CALL READ (*700,*50,FILE,IZ(10),NREAD,0,ILEN)
      ICRQ  = NREAD
      GO TO 830
   50 ILEN  = ILEN + 11
   60 IZ(ILEN-1) = ENDREC(1)
      IZ(ILEN  ) = ENDREC(2)
      GO TO 90
C
C     BRING IN NEXT RECORD
C
   70 CALL READ (*700,*80,FILE,IZ(KMB),NREAD,IO,ILEN)
      ICRQ  = NREAD
      GO TO 830
   80 IF (KMB .EQ. 1) GO TO 600
      IZ(3) = IZ(3) + 1
      IF (ILEN .EQ. 0) GO TO 70
      ILEN  = ILEN + 5
      GO TO 60
C
C     BUILD FORMAT VECTOR  1= INTEGER, 2 =BCD, 3=REAL
C
   90 JV = 3
      DO 100 K = 1,ILEN
      M  = NLIST + K - 1
      J  = NUMTYP(IZ(K))
      IF (J.EQ.0 .AND. JV.NE.3) J = JV
      IZ(M) = LL(J+1)
  100 JV = J
C
C     MOVE DATA/FORMAT TO DATA AREA 8 FIELDS AT A TIME--SET D.F. FLAG
C
      ID   = 1
      IF   = NLIST
      IFRS = 1
C
C     HERE FOR EIGHT MORE WORDS
C
  110 IDF = 0
      IDT = 1
      IFT = 1
      NF  = 1
C
C     HERE  FOR EACH FIELD
C
  120 IDATA(IDT) = IZ(ID)
      IFORM(IFT) = IZ(IF)
      IF (IFORM(IFT) .EQ. 3) IDF = 1
      IF (IFORM(IFT) .NE. 2) GO TO 140
C
C     BCD IS TWO WORDS
C
      IDATA(IDT+1) = IZ(ID+1)
C
C     MAY BE FALSE BCD, CHECK FORMAT OF SECOND WORD ALSO
C     (SOME REAL NUMBER BIT PATTERNS LOOK LIKE BCD).
C
      IF (IZ(IF+1) .EQ. 2) GO TO 130
C
C     SECOND WORD IS NOT BCD, ASSUME FIRST WORD IS REAL.
C
      IDF = 1
      IFORM(IFT) = 3
      GO TO 140
  130 IDT = IDT + 2
      IFT = IFT + 1
      ID  = ID  + 2
      IF  = IF  + 2
      GO TO 150
C
C     REAL OR INTEGER
C
  140 IDT = IDT + 1
      IFT = IFT + 1
      ID  = ID  + 1
      IF  = IF  + 1
C
C     BUMP FIELD COUNTER
C
  150 NF = NF + 1
      IF (NF .GT.    8) GO TO 160
      IF (ID .LT. ILEN) GO TO 120
C
C     FILL  WITH BLANKS
C
      IDATA(IDT  ) = BLANK
      IDATA(IDT+1) = BLANK
      IFORM(IFT  ) = 2
      GO TO 130
C
C     PUNCH OUT 8 FIELDS OF DATA
C
  160 IDT = 0
      IF (IDF .NE. 0) GO TO 400
C
C     SINGLE FIELD CARD
C
      NF = 1
  170 M  = 2*NF + 2
      IF (IFORM(NF)-2) 180,200,210
C
C     INTEGER
C
  180 FORM(M  ,IFRS) = INT(1)
      FORM(M+1,IFRS) = INT(2)
C
C     GET NEXT ITEM
C
      IDT = IDT + 1
  190 NF  = NF  + 1
      IF (NF .LE. 8) GO TO 170
      GO TO 220
C
C     BCD
C
  200 FORM(M  ,IFRS) = IBCD(1)
      FORM(M+1,IFRS) = IBCD(2)
      IDT = IDT + 2
      GO TO 190
C
C     REAL NOT LEGAL
C
  210 IP1 = -37
      GO TO 850
C
C     PUNCH OUT SINGLE CARD
C
  220 IF (IFRS .NE. 1) GO TO 270
      DO 230 J = 1,30
      PFORM(J) = FORM(J,1)
  230 CONTINUE
      WRITE (LPCH,PFORM,ERR=240) DTI,(RDATA(M),M=1,IDT),ICHR,IRECNO
  240 IRECNO = IRECNO + 1
      IFRS = 2
      DO 250 J = 1,30
  250 FORM(J,1) = FORMS(J,1)
  260 IF (ID .GE. ILEN) GO TO 70
      GO TO 110
C
C     CONTINUATION CARD
C
  270 IRCNM1 = IRECNO - 1
      DO 280 J = 1,30
      PFORM(J) = FORM(J,2)
  280 CONTINUE
      WRITE (LPCH,PFORM,ERR=290) IPLUS,ICHR,IRCNM1,(RDATA(M),M=1,IDT),
     1                           ICHR,IRECNO
  290 IRECNO = IRECNO + 1
      DO 300 J = 1,30
  300 FORM(J,2) = FORMS(J,2)
      GO TO 260
C
C     DOUBLE FIELD CARDS
C
  400 NF = 1
      IS = 1
      IT = 4
      IDT= 0
      M  = 2
  410 M  = M + 2
      IF (IFORM(NF)-2) 420,450,460
C
C     INTEGER
C
  420 FORM(M  ,IFRS) = INTD(1)
      FORM(M+1,IFRS) = INTD(2)
  430 IDT = IDT + 1
  440 NF  = NF  + 1
      IF (M .LE. 8) GO TO 410
      GO TO 470
C
C     BCD
C
  450 FORM(M  ,IFRS) = IBCDD(1)
      FORM(M+1,IFRS) = IBCDD(2)
      IDT = IDT + 2
      GO TO 440
C
C     REAL
C
  460 FORM(M  ,IFRS) = IREAL(1)
      FORM(M+1,IFRS) = IREAL(2)
      GO TO 430
C
C     PUNCH OUT DOUBLE FIELD CARD
C
  470 IF (IFRS .NE. 1) GO TO 520
      DO 480 J = 1,30
      PFORM(J) = FORM(J,1)
  480 CONTINUE
      WRITE (LPCH,PFORM,ERR=490) DTIS,(RDATA(M),M=IS,IDT),ICHR,IRECNO
  490 IRECNO = IRECNO + 1
      DO 500 J = 1,30
  500 FORM(J,1) = FORMS(J,1)
      IFRS = 2
  510 IT = 8
      M  = 2
      IS = IDT + 1
      GO TO 410
C
C     CONTINUATION CARD
C
  520 IRCNM1 = IRECNO - 1
      DO 530 J = 1,30
      PFORM(J) = FORM(J,2)
  530 CONTINUE
      WRITE (LPCH,PFORM,ERR=540) ISTAR,ICHR,IRCNM1,(RDATA(M),M=IS,IDT),
     1                           ICHR,IRECNO
  540 IRECNO = IRECNO + 1
      DO 550 J = 1,30
  550 FORM(J,2) = FORMS(J,2)
      IF (IT .EQ. 4) GO TO 510
      GO TO 260
C
C     PUNCH KELM, MELM AND BELM IN D.P.
C
  600 IF (ILEN .EQ. 0) GO TO 70
      ILEN = ILEN/2
      JE = 0
  610 JB = JE + 1
      JE = JE + 4
      IRCNM1 = IRECNO
      IRECNO = IRECNO + 1
      IF (JE .GE. ILEN) GO TO 630
      WRITE  (LPCH,620,ERR=840) ICHR,IRCNM1,(DZ(J),J=JB,JE),ICHR,IRECNO
  620 FORMAT (1H*,A2,I5,1P,4D16.9,1X,A2,I5)
      GO TO 610
  630 JE = ILEN
      WRITE  (LPCH,640,ERR=840) ICHR,IRCNM1,(DZ(J),J=JB,JE)
  640 FORMAT (1H*,A2,I5,1P,4D16.9)
      GO TO 70
C
C     CLOSE OFF FILES
C
  700 CALL CLOSE (FILE,1)
      CALL PAGE2 (2)
      WRITE  (OUT,710) UIM,TABNM,IRECNO
  710 FORMAT (A29,' 4015, TABLE ',2A4,' WAS PUNCHED OUT,',I8,' CARDS.')
  720 CONTINUE
      WRITE  (LPCH,730)
  730 FORMAT (1H , /,1H , /,1H )
      RETURN
C
C     ERROR MESAGES
C
  800 IP1 = -1
      GO TO 850
  810 IP1 =-2
      GO TO 850
  820 IP1 =-3
      GO TO 850
  830 IP1 = -8
      FILE = ICRQ
      GO TO 850
  840 IP1 = -37
C
  850 CALL MESAGE (IP1,FILE,NAME)
      RETURN
      END
