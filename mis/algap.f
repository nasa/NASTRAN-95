      SUBROUTINE ALGAP (IFNAME,IFNM)
C
C     THIS ROUTINE IS A MODIFIED VERSION OF SUBROUTINE TABPCH. IT WILL
C     ONLY PUNCH ONE TABLE INTO DTI CHARDS.
C
C     CONTINUATION CARD CHARACTERS ARE - AL.
C
C     SINGLE FIELD CARDS WILL BE MADE UNLESS REAL NUMBERS ARE TO BE MADE
C     ALL REAL NUMBERS ARE ASSUMED TO BE SINGLE PRECISION.
C
C  $MIXED_FORMATS
C
      INTEGER         SYSBUF   ,IZ(10)   ,NAME(2)  ,INT(2)   ,IREAL(2) ,
     1                MCB(7)   ,FILE     ,TABNM(2) ,DTI(2)   ,DTIS(2)  ,
     2                IDATA(20),ENDREC(2),OUT      ,IFORM(20),BLANK    ,
     3                IBCD(2)  ,INTD(2)  ,IBCDD(2) ,PFORM(30),LL(4)    ,
     4                FORM(30,2)         ,FORMS(30,2)
      REAL            RDATA(20)
      CHARACTER       UFM*23   ,UWM*25   ,UIM*29
      COMMON /XMSSG / UFM      ,UWM      ,UIM
      COMMON /SYSTEM/ KSYSTM(100)
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (KSYSTM( 1),SYSBUF),(KSYSTM(2),OUT ),
     1                (KSYSTM(91),LPUNCH),(IZ(1)    ,Z(1)),
     2                (IDATA(1),RDATA(1))
      DATA    BLANK / 1H             /
      DATA    DTI   / 4HDTI , 1H     /
      DATA    DTIS  / 4HDTI*, 1H     /
      DATA    ENDREC/ 4HENDR, 4HEC   /
      DATA    FORMS / 4H(2A4, 26*4H    ,4H,1H+ ,4HA2,I,4H5)  ,
     1                4H(A1,, 4HA2,I   ,4H5    ,24*4H        ,4H,1H+  ,
     2                4HA2,I, 4H5)   /
      DATA    IBCD  / 4H,2A4, 1H     /
      DATA    IBCDD / 4H,2A4, 4H,8X  /
      DATA    INT   / 4H,I8 , 1H     /
      DATA    INTD  / 4H,I16, 1H     /
      DATA    IPLUS / 1H+            /
      DATA    IREAL / 4H,E16, 4H.9   /
      DATA    ISTAR / 1H*            /
      DATA    NAME  / 4HALGA, 4HP    /
      DATA    N1    / 2HAL           /
      DATA    LL    / 3, 1, 3, 2     /
C
      NZ   = KORSZ(Z)
      IBUF = NZ - SYSBUF + 1
      NZ   = IBUF - 1
      IF (NZ .LE. 10) CALL MESAGE (-8,0,NAME)
      NREAD = NZ/2  - 2
      NLIST = NREAD + 3
      DO 5 J = 1,2
      DO 5 I = 1,30
      FORM(I,J) = FORMS(I,J)
    5 CONTINUE
C
C     FOR EACH  TABLE DEFINED
C
      MCB(1) = IFNM
      CALL RDTRL(MCB)
      IF (MCB(1) .LE. 0) GO TO 310
C
C     TABLE EXISTS SET IT UP
C
      FILE = IFNM
      CALL OPEN (*320,FILE,IZ(IBUF),0)
      CALL READ (*340,*350,FILE,IZ(1),-2,0,ILEN)
      CALL FNAME (IFNAME,TABNM)
      IRECNO = 0
      ICHR   = N1
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
      CALL READ (*290,*10,FILE,IZ(10),NREAD,0,ILEN)
      CALL MESAGE (-8,0,NAME)
   10 ILEN = ILEN + 11
   11 IZ(ILEN-1) = ENDREC(1)
      IZ(ILEN  ) = ENDREC(2)
      GO TO 40
C
C     BRING IN NEXT RECORD
C
   20 CALL READ (*290,*30,FILE,IZ(4),NREAD,0,ILEN)
      CALL MESAGE (-8,0,NAME)
   30 IZ(3) = IZ(3) + 1
      IF (ILEN .EQ. 0) GO TO 20
      ILEN = ILEN + 5
      GO TO 11
C
C     BUILD FORMAT VECTOR  1= INTEGER, 2 =BCD, 3=REAL
C
   40 DO 50 K = 1,ILEN
      M = NLIST + K - 1
      J = NUMTYP(IZ(K))
      IZ(M) = LL(J+1)
   50 CONTINUE
C
C     MOVE DATA/FORMAT TO DATA AREA 8 FIELDS AT A TIME--SET D.F. FLAG
C
      ID = 1
      IF = NLIST
      IFRS = 1
C
C     HERE FOR EIGHT MORE WORDS
C
   60 IDF = 0
      IDT = 1
      IFT = 1
      NF  = 1
C
C     HERE  FOR EACH FIELD
C
   70 IDATA(IDT) = IZ(ID)
      IFORM(IFT) = IZ(IF)
      IF (IFORM(IFT) .EQ. 3) IDF = 1
      IF (IFORM(IFT) .NE. 2) GO TO 80
C
C     BCD IS TWO WORDS
C
      IDATA(IDT+1) = IZ(ID+1)
C
C     MAY BE FALSE BCD, CHECK FORMAT OF SECOND WORD ALSO
C     ( SOME REAL NUMBER BIT PATTERNS LOOK LIKE BCD ).
C
      IF (IZ(IF+1) .EQ. 2) GO TO 100
C
C     SECOND WORD IS NOT BCD, ASSUME FIRST WORD IS REAL.
C
      IDF = 1
      IFORM(IFT) = 3
      GO TO 80
  100 IDT = IDT + 2
      IFT = IFT + 1
      ID  = ID  + 2
      IF  = IF  + 2
      GO TO 90
C
C     REAL OR INTEGER
C
   80 IDT = IDT + 1
      IFT = IFT + 1
      ID  = ID  + 1
      IF  = IF  + 1
C
C     BUMP FIELD COUNTER
C
   90 NF = NF + 1
      IF (NF .GT. 8) GO TO 110
      IF (ID .LT. ILEN) GO TO 70
C
C     FILL  WITH BLANKS
C
      IDATA(IDT  ) = BLANK
      IDATA(IDT+1) = BLANK
      IFORM(IFT  ) = 2
      GO TO 100
C
C     PUNCH OUT 8 FIELDS OF DATA
C
  110 IDT = 0
      IF (IDF .NE. 0) GO TO 200
C
C     SINGLE FIELD CARD
C
      NF = 1
  120 M = 2*NF + 2
      IF (IFORM(NF)-2) 130,150,160
C
C     INTEGER
C
  130 FORM(M  ,IFRS) = INT(1)
      FORM(M+1,IFRS) = INT(2)
C
C     GET NEXT ITEM
C
      IDT = IDT + 1
  140 NF  = NF  + 1
      IF (NF .LE. 8) GO TO 120
      GO TO 170
C
C     BCD
C
  150 FORM(M  ,IFRS) = IBCD(1)
      FORM(M+1,IFRS) = IBCD(2)
      IDT = IDT + 2
      GO TO 140
C
C     REAL NOT LEGAL
C
  160 CALL MESAGE (-61,0,NAME)
      RETURN
C
C     PUNCH OUT SINGLE CARD
C
  170 IF (IFRS .NE. 1) GO TO 190
      DO 171 J = 1,30
      PFORM(J) = FORM(J,1)
  171 CONTINUE
      WRITE (LPUNCH,PFORM,ERR=173) DTI,(RDATA(M),M=1,IDT),ICHR,IRECNO
  173 IRECNO = IRECNO + 1
      IFRS = 2
      DO 175 J = 1,30
  175 FORM(J,1) = FORMS(J,1)
  180 IF (ID .GE. ILEN) GO TO 20
      GO TO 60
C
C     CONTINUATION CARD
C
  190 IRCNM1 = IRECNO - 1
      DO 191 J = 1,30
      PFORM(J) = FORM(J,2)
  191 CONTINUE
      WRITE (LPUNCH,PFORM,ERR=193)
     1      IPLUS,ICHR,IRCNM1,(RDATA(M),M=1,IDT),ICHR,IRECNO
  193 IRECNO = IRECNO + 1
      DO 195 J = 1,30
  195 FORM(J,2) = FORMS(J,2)
      GO TO 180
C
C     DOUBLE FIELD CARDS
C
  200 NF = 1
      IS = 1
      IT = 4
      IDT= 0
      M  = 2
  210 M  = M + 2
      IF (IFORM(NF)-2) 211,240,250
C
C     INTEGER
C
  211 FORM(M  ,IFRS) = INTD(1)
      FORM(M+1,IFRS) = INTD(2)
  220 IDT = IDT + 1
  230 NF  = NF  + 1
      IF (M .LE. 8) GO TO 210
      GO TO 260
C
C     BCD
C
  240 FORM(M  ,IFRS) = IBCDD(1)
      FORM(M+1,IFRS) = IBCDD(2)
      IDT = IDT + 2
      GO TO 230
C
C     REAL
C
  250 FORM(M  ,IFRS) = IREAL(1)
      FORM(M+1,IFRS) = IREAL(2)
      GO TO 220
C
C     PUNCH OUT DOUBLE FIELD CARD
C
  260 IF (IFRS .NE. 1) GO TO 280
      DO 261 J = 1,30
      PFORM(J) = FORM(J,1)
  261 CONTINUE
      WRITE (LPUNCH,PFORM,ERR=263) DTIS,(RDATA(M),M=IS,IDT),ICHR,IRECNO
  263 IRECNO = IRECNO + 1
      DO 265 J = 1,30
  265 FORM(J,1) = FORMS(J,1)
      IFRS = 2
  270 IT = 8
      M  = 2
      IS = IDT + 1
      GO TO 210
C
C     CONTINUATION CARD
C
  280 IRCNM1 = IRECNO - 1
      DO 281 J = 1,30
      PFORM(J) = FORM(J,2)
  281 CONTINUE
      WRITE (LPUNCH,PFORM,ERR=283)
     1      ISTAR,ICHR,IRCNM1,(RDATA(M),M=IS,IDT),ICHR,IRECNO
  283 IRECNO = IRECNO + 1
      DO 285 J = 1,30
  285 FORM(J,2) = FORMS(J,2)
      IF (IT .EQ. 4) GO TO 270
      GO TO 180
C
C     CLOSE OFF FILES
C
  290 CALL CLOSE (FILE,1)
      WRITE  (OUT,300) UIM,TABNM,IRECNO
  300 FORMAT (A29,' 4015.', /5X,'TABLE NAMED ',2A4,' PUNCHED ONTO',I9,
     1        ' CARDS.')
  310 CONTINUE
      WRITE  (LPUNCH,311)
  311 FORMAT (1H , /,1H , /,1H )
      RETURN
C
C     ERROR MESAGES
C
  320 IP1 = -1
  330 CALL MESAGE (IP1,FILE,NAME)
      CALL MESAGE (-61,0,NAME)
  340 IP1 =-2
      GO TO 330
  350 IP1 =-3
      GO TO 330
      END
