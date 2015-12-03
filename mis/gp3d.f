      SUBROUTINE GP3D
C
C     GP3D CREATES THE ETT (ELEMENT TEMPERATURE TABLE)
C
C     THE GPTT AS PREPARED BY GP3B COMES TO THIS ROUTINE VIA SCRATCH
C     DATA SET 1.
C
C     DATA IN THE GPTT IS USED TOGETHER WITH DATA OBTAINED FROM TEMPP1,
C     TEMPP2, TEMPP3, AND TEMPRB CARDS WHICH RESIDE ON GEOM3.
C
      LOGICAL         ANYGPT   ,ANYET    ,LFLAG    ,ANY      ,HEAT
      INTEGER         GEOM3    ,EQEXIN   ,GEOM2    ,SLT      ,ETT      ,
     1                SCR1     ,SCR2     ,BUF1     ,BUF2     ,BUF      ,
     2                FILE     ,CARDID   ,CARDDT   ,STATUS   ,PLOAD2   ,
     3                TEMPD    ,TEMPP1   ,TEMPP2   ,TEMPP3   ,TEMPRB   ,
     4                RD       ,RDREW    ,WRT      ,WRTREW   ,REW      ,
     5                NOREW    ,Z        ,FLAG     ,TWOI     ,DEFALT   ,
     6                NAM(2)   ,RECORD   ,GPTREC   ,SETID    ,OUTPT    ,
     7                SYSBUF   ,OUTWDS   ,ECTWDS   ,ELEM     ,BUF3
      REAL            RZ(1)    ,RBUF(50) ,TGRID(32)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /BLANK / NOGRAV   ,NOLOAD   ,NOTEMP
      COMMON /SYSTEM/ KSYSTM(63)
      COMMON /NAMES / RD       ,RDREW    ,WRT      ,WRTREW   ,REW      ,
     1                NOREW
      COMMON /GP3COM/ GEOM3    ,EQEXIN   ,GEOM2    ,SLT      ,ETT      ,
     1                SCR1     ,SCR2     ,BUF1     ,BUF2     ,BUF(50)  ,
     2                CARDID(60),IDNO(30),CARDDT(60),MASK(60),STATUS(60)
     3,               NTYPES   ,IPLOAD   ,IGRAV    ,PLOAD2(2),LOAD(2)  ,
     4                NOPLD2   ,TEMP(2)  ,TEMPD(2) ,TEMPP1(2),TEMPP2(2),
     5                TEMPP3(2),TEMPRB(2),BUF3
      COMMON /GPTA1 / NELEM    ,LAST     ,INCR     ,ELEM(1)
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (RZ(1),Z(1)), (RBUF(1),BUF(1)), (DEFALT,DEFTMP),
     1                (KSYSTM(1),SYSBUF), (KSYSTM(2),OUTPT),
     2                (KSYSTM(56),IHEAT)
      DATA   NAM    / 4HGP3D,4H     /
C
C                 +---------------------+
C     OPEN CORE   I                     I  Z(ILIST) = Z(1)
C                 I  ET SET-LIST        I
C     DESIGN FOR  I  2 WDS/ENTRY        I
C                 I                     I  Z(NLIST)
C     GP3D        +---------------------+
C                 I                     I  Z(IGPTT)
C                 I  GPT SET-LIST       I
C                 I  3 WDS/ENTRY        I
C                 I                     I  Z(NGPTT)
C                 +---------------------+
C                 I                     I  Z(IGPT) *
C                 I  GPTT DATA          I           *
C                 I  FOR CURRENT SETID  I            *
C                 I  2 WDS/ENTRY        I             *
C                 I                     I  Z(NGPT)     *
C                 +---------------------+               *
C                 I                     I  Z(IET1)       *
C                 I  2-DIMEN EL-TEMP    I                * THIS SPACE IS
C                 I  FOR CURRENT SETID  I                * DYNAMIC FOR
C                 I  7 WDS/ENTRY        I                * EACH SET OF
C                 I                     I  Z(NET1)       * TEMPERATURE
C                 +---------------------+                * DATA.
C                 I                     I  Z(IET2)       *
C                 I  1-DIMEN EL-TEMP    I                *
C                 I  FOR CURRENT SETID  I               *
C                 I  15 WDS/ENTRY       I              *
C                 I                     I  Z(NET2)    *
C                 +---------------------+            *
C                 I/////////////////////I           *
C                 I/////////////////////I          *
C                 +---------------------+
C                 I                     I  Z(BUF1)
C                 I  BUFFER 2           I
C                 I                     I
C                 +---------------------+
C                 I                     I  Z(BUF2)
C                 I  BUFFER 1           I
C                 I                     I  Z(KORSZ)
C                 +---------------------+
C
C
C
C     OPEN GEOM3, AND SCR1. READ IN TEMPP1, TEMPP2, TEMPP3, TEMPRB CARDS
C     CONVERT AND WRITE THEM OUT ON SCR2.
C
      HEAT  = .FALSE.
      IF (IHEAT .EQ. 1) HEAT = .TRUE.
      LFLAG = .FALSE.
      J     = -1
      NWORDS= 8
      ILIST = 1
      NLIST = 0
      FILE  = GEOM3
      ANY   = .FALSE.
      CALL PRELOC (*820,Z(BUF1),GEOM3)
      FILE  = SCR2
      CALL OPEN (*820,SCR2,Z(BUF2),WRTREW)
C
C     PICK UP TEMPP1 CARDS
C
      FILE = GEOM3
      CALL LOCATE (*20,Z(BUF1),TEMPP1,FLAG)
      ANY  = .TRUE.
      ASSIGN 10 TO IRETRN
      BUF(7) = 0
      BUF(8) = 1
   10 CALL READ (*840,*20,GEOM3,BUF,6,0,FLAG)
      GO TO 170
C
C     PICK UP TEMPP2 CARDS
C
   20 CALL LOCATE (*40,Z(BUF1),TEMPP2,FLAG)
      ANY = .TRUE.
      ASSIGN 30 TO IRETRN
   30 CALL READ (*840,*40,GEOM3,BUF,8,0,FLAG)
      GO TO 170
C
C     PICK UP TEMPP3 CARDS (CONVERT THESE TO LOOK LIKE TEMPP1 CARDS)
C
   40 CALL LOCATE (*140,Z(BUF1),TEMPP3,FLAG)
      ANY = .TRUE.
      ASSIGN 50 TO IRETRN
   50 CALL READ (*840,*140,GEOM3,BUF,24,0,FLAG)
      N  = 25
      DO 60 I = 1,11
      N  = N - 2
      IF (RBUF(N).NE.0.0 .OR. RBUF(N+1).NE.0.0) GO TO 70
   60 CONTINUE
   70 N  = N/2
      T1 = RBUF(4)
      T2 = RBUF(2*N+2)
      IF (N .EQ. 1) GO TO 100
      H  = RBUF(2*N+1) - RBUF(3)
      SUM= 0.0
      N  = N-1
      DO 80 I = 1,N
      TWOI = 2*I
      FACTOR = RBUF(TWOI+3) - RBUF(TWOI+1)
      IF (FACTOR .LE. 0.0) GO TO 120
      SUM  = SUM + (RBUF(TWOI+2) + RBUF(TWOI+4))*FACTOR
   80 CONTINUE
      TBAR = SUM/(2.0*H)
      HOVER2 = H/2.0
      SUM  = 0.0
      DO 90 I = 1,N
      TWOI = 2*I
      SUM  = SUM + (RBUF(TWOI+3) - RBUF(TWOI+1)    )*(3.0*
     1             (RBUF(TWOI+1) - RBUF(3) - HOVER2)*
     2             (RBUF(TWOI+4) + RBUF(TWOI+2)    ) +
     3             (RBUF(TWOI+2) + 2.0*RBUF(TWOI+4))*
     4             (RBUF(TWOI+3) - RBUF(TWOI+1)   ))
   90 CONTINUE
      TPRIME = 2.0*SUM/H**3
      GO TO 110
C
  100 TBAR = RBUF(4)
      TPRIME = 0.0
C
  110 RBUF(3) = TBAR
      RBUF(4) = TPRIME
      RBUF(5) = T1
      RBUF(6) = T2
      BUF(7)  = 0
      BUF(8)  = 1
      GO TO 170
C
C     BAD DATA ON A TEMPP3 CARD
C
  120 WRITE  (OUTPT,130) UFM,BUF(1),BUF(2)
  130 FORMAT (A23,' 4010, TEMPP3 BULK DATA CARD WITH SET ID =',I8,
     1       ' AND ELEMENT ID =',I8, /27X,
     2       'DOES NOT HAVE ASCENDING VALUES SPECIFIED FOR Z.')
      LFLAG = .TRUE.
      GO TO 50
C
C     END OF 8 WORD CARDS.  WRITE EOR ON SCR2 AND DO TEMPRB CARDS NOW.
C
  140 CALL WRITE (SCR2,0,0,1)
      NWORDS = 16
      CALL LOCATE (*160,Z(BUF1),TEMPRB,FLAG)
      ANY = .TRUE.
      ASSIGN 150 TO IRETRN
  150 CALL READ (*840,*160,GEOM3,BUF,16,0,FLAG)
      GO TO 170
C
C     WRITE EOR ON SCR2. SCR2 THEN WILL HAVE 2 RECORDS (1 OR BOTH EMPTY)
C
  160 CALL WRITE (SCR2,0,0,1)
      CALL CLOSE (GEOM3,REW )
      CALL CLOSE (SCR2 ,REW )
      GO TO 230
C
C     INTERNAL SUBROUTINE TO BUILD SET LIST FROM TEMPERATURE CARD DATA
C     FIND SET-ID OR ADD IT TO LIST IN SORT, BUMP COUNT AND WRITE CARD.
C
  170 IF (J .EQ. -1) GO TO 210
      IF (BUF(1) .EQ. Z(J)) GO TO 180
      IF (BUF(1).GT.Z(J) .AND. J.EQ.NLIST-1) GO TO 210
C
C     LOOK FOR MATCHING SETID OR FIND WHERE NEW SETID BELONGS
C
      CALL BISLOC (*190,BUF(1),Z(ILIST),2,NLIST/2,J)
C
C     MATCH WAS FOUND  (ILIST ASSUMED TO BE EQUAL TO 1)
C
  180 Z(J+1) = Z(J+1) + NWORDS
      GO TO 220
C
C     ADD THIS NEW SETID INTO LIST
C
  190 IF (BUF(1) .GT. Z(J)) J = J + 2
C
C     PUSH Z(J) THRU Z(NLIST) DOWN TWO WORDS TO MAKE ROOM FOR NEW SETID
C
      I = NLIST + 2
      DO 200 K = J,NLIST
      Z(I) = Z(I-2)
      I = I - 1
  200 CONTINUE
      GO TO 211
C
C     ADD NEW SETID TO LIST
C
  210 J = J + 2
  211 Z(J)  = BUF(1)
      NLIST = NLIST + 2
      Z(J+1)= NWORDS
C
C     WRITE OUT THE DATA CARD ON THE SCRATCH FILE FOR LATER USE
C
  220 CALL WRITE (SCR2,BUF,NWORDS,0)
      GO TO IRETRN, (10,30,50,150)
C
C     READ IN GPTT HEADER RECORD FROM SCR1
C
  230 IGPTT = NLIST + 1
      NGPTT = IGPTT
      FILE  = SCR1
      IF (NOTEMP .NE. 1) GO TO 250
      CALL OPEN (*820,SCR1,Z(BUF1),RDREW)
      CALL READ (*840,*240,SCR1,Z(IGPTT),BUF2-IGPTT,1,FLAG)
      CALL MESAGE (-8,0,NAM)
  240 NGPTT = NLIST + FLAG
      IGPTT = IGPTT + 2
      NSETS = (NGPTT - IGPTT + 1)/3
C
C     DETERMINE NUMBER OF RECORDS OF EXTERNAL INDEX TEMP DATA
C     FOLLOWING HEADER RECORD.
C
      IRECS = 0
      IF (NSETS) 247,247,241
  241 DO 244 I = IGPTT,NGPTT,3
      IRECS = MAX0(Z(I+2),IRECS)
  244 CONTINUE
  247 CONTINUE
      CALL CLOSE (SCR1,NOREW)
C
C     OPEN ETT, PUT OUT HEADER RECORD WITH THE 3 WORD SET ENTRIES.
C
  250 IF (NOTEMP.NE.1 .AND. .NOT.ANY) GO TO 810
      NOTEMP = 1
      FILE   = ETT
      CALL OPEN (*820,ETT,Z(BUF2),WRTREW)
      CALL FNAME (ETT,BUF)
      CALL WRITE (ETT,BUF,2,0)
      LIST1 = ILIST
      LIST2 = IGPTT
      RECORD= 0
  260 IF (LIST1.GT.NLIST-1 .AND. LIST2.LE.NGPTT-2) GO TO 290
      IF (LIST1.LE.NLIST-1 .AND. LIST2.GT.NGPTT-2) GO TO 270
      IF (LIST1.GT.NLIST-1 .AND. LIST2.GT.NGPTT-2) GO TO 330
C
      IF (Z(LIST1) - Z(LIST2)) 270,280,290
C
C     SET-ID OF LIST1 IS .LT. SET-ID OF LIST2 OR LIST2 IS ALL USED.
C
  270 BUF(1) = Z(LIST1)
      BUF(2) = -1
      LIST1  = LIST1 + 2
      GO TO 300
C
C     SET-ID OF LIST1 IS .EQ. SET-ID OF LIST2.
C
  280 BUF(1) = Z(LIST2  )
      BUF(2) = Z(LIST2+1)
      LIST1  = LIST1 + 2
      LIST2  = LIST2 + 3
      GO TO 300
C
C     SET-ID OF LIST2 IS .LT. SET-ID OF LIST1 OR LIST1 IS ALL USED.
C
  290 BUF(1) = Z(LIST2  )
      BUF(2) = Z(LIST2+1)
      LIST2  = LIST2 + 3
      IF (Z(LIST2-1) .EQ. 0) GO TO 310
C
C     WRITE 3-WORD SET-ID ENTRY IN HEADER
C
  300 RECORD = RECORD + 1
      BUF(3) = RECORD
      GO TO 320
  310 BUF(3) = 0
  320 CALL WRITE (ETT,BUF,3,0)
      GO TO 260
C
C     HEADER RECORD IS COMPLETE.  WRITE EOR AND CLOSE WITH NOREWIND.
C
  330 CALL WRITE (ETT,0,0,1)
      CALL CLOSE (ETT,NOREW)
C
C     FOR EACH SET DEFINED IN THE EL-TEMP SET LIST AND OR THE GRID-TEMP
C     SET LIST PASS GEOM2 USING LOCATE FOR ALL THE ELEMENTS FOR
C     WHICH ETT TEMP DATA OUTPUT IS POSSIBLE.
C     IF ANY ELEMENTS CONCERNED ARE PRESENT THEN SELECT FROM THE TEMP
C     DATA AVAILABLE THAT WHICH IS APPLICABLE AND OUTPUT THE DATA ON THE
C     ETT IN THE FOLLOWING FORMAT.
C
C     CONTENTS OF 1 RECORD OF THE OUTPUT FILE ETT. EACH RECORD CONTAINS
C     DATA FOR 1 SET.
C
C         SET-ID
C         ELEMENT TYPE          * * * * * * * * * *
C         NUMBER OF TEMPERATURE DATA VALUES/EL-ID  *
C         EL-ID          *                          *
C         TEMP-VALUE      *                          *
C             .           * EL-ID                    *
C             .           * ENTRY                    *
C             .           *                          *  ELEMENT-TYPE
C         LAST-TEMP-VALUE*                           *     ENTRY
C               *             (1 OR MORE EL-ID       *
C               *              ENTRIES PER EL-TYPE   *   (1 OR MORE
C               *              ENTRY)                *    PER RECORD)
C         EL-ID          *                           *
C         TEMP-VALUE      *                          *
C             .           * EL-ID                    *
C             .           * ENTRY                    *
C             .           *                         *
C         LAST-TEMP-VALUE*                         *
C         0                     * * * * * * * * * *
C
C     IN THE ABOVE IF THE ELEMENT HAS NO SPECIAL DATA, A NEGATIVE
C     ELEMENT ID IS INSERTED FOLLOWED BY NO TEMPERATURE DATA.
C
C     NOW GATHER THE DATA AVAILABLE FOR A SET FROM SCR1 AND OR SCR2.
C
      GPTREC = 1
      LIST1  = ILIST
      LIST2  = IGPTT
  340 ANYGPT = .FALSE.
      ANYET  = .FALSE.
      IGPT   = 0
      NGPT   = 0
      IET1   = 0
      NET1   = 0
      IET2   = 0
      NET2   = 0
      IF (LIST1 .GT. NLIST-1) GO TO 350
      IF (LIST2 .LE. NGPTT-2) GO TO 360
      GO TO 370
  350 IF (LIST2 .LE. NGPTT-2) GO TO 390
      GO TO 770
C
  360 IF (Z(LIST1) - Z(LIST2)) 370,380,390
C
C     NEXT SET-ID HAS ONLY EL-TEMP DATA
C
  370 SETID  = Z(LIST1)
      DEFALT = -1
      ANYET  = .TRUE.
      NWORDS = Z(LIST1+1)
      LIST1  = LIST1 + 2
      GO TO 400
C
C     NEXT SET-ID HAS BOTH GRID-TEMP AND EL-TEMP DATA
C
  380 SETID  = Z(LIST2  )
      DEFALT = Z(LIST2+1)
      ANYET  = .TRUE.
      INREC  = Z(LIST2+2)
      IF (INREC .GT. 0) ANYGPT = .TRUE.
      NWORDS = Z(LIST1+1)
      LIST1  = LIST1 + 2
      LIST2  = LIST2 + 3
      GO TO 400
C
C     NEXT SET-ID HAS ONLY GRID-TEMP DATA
C
  390 SETID  = Z(LIST2  )
      DEFALT = Z(LIST2+1)
      INREC  = Z(LIST2+2)
      IF (INREC .GT. 0) ANYGPT = .TRUE.
      LIST2  = LIST2 + 3
      GO TO 400
C
C     AT THIS POINT READ IN ANY GRID-TEMP DATA AND/OR ANY EL-TEMP DATA.
C     SORT THE EL-TEMP DATA ON EL-ID. THE GRID-TEMP DATA IS SORTED ON
C     GRIDS
C
  400 IGPT = NGPTT + 1
      NGPT = IGPT
      IF (.NOT.ANYGPT) GO TO 460
      FILE = SCR1
      CALL OPEN (*820,SCR1,Z(BUF1),RD)
C
C     POSITION GPTT TO DESIRED GRID-POINT-TEMP SET AND READ IT IN.
C
      MOVE = INREC - GPTREC
      IF (MOVE) 410,440,420
  410 CALL REWIND (SCR1)
      MOVE = INREC
  420 DO 430 I = 1,MOVE
      CALL FWDREC (*840,SCR1)
  430 CONTINUE
  440 GPTREC = INREC + 1
      CALL READ (*840,*450,SCR1,Z(IGPT),BUF2-IGPT,1,FLAG)
      CALL MESAGE (-8,0,NAM)
  450 NGPT = IGPT + FLAG - 1
      CALL CLOSE (SCR1,NOREW)
C
C     READ IN EL-TEMP DATA PERTAINING TO THIS SET-ID
C
  460 IF (.NOT.ANYET) GO TO 520
      IF (NGPT+NWORDS .GE. BUF2) CALL MESAGE (-8,0,NAM)
      FILE = SCR2
      CALL OPEN (*820,SCR2,Z(BUF1),RDREW)
      IET1 = NGPT + 1
      NET1 = NGPT
  470 CALL READ (*840,*490,SCR2,BUF,8,0,FLAG)
      IF (BUF(1) .NE. SETID) GO TO 470
      DO 480 I = 2,8
      NET1 = NET1 + 1
  480 Z(NET1) = BUF(I)
      NWORDS  = NWORDS - 8
      IF (NWORDS .NE. 0) GO TO 470
      CALL FWDREC (*820,SCR2)
  490 IET2 = NET1 + 1
      NET2 = NET1
  500 CALL READ (*840,*520,SCR2,BUF,16,0,FLAG)
      IF (BUF(1) .NE. SETID) GO TO 500
      DO 510 I = 2,16
      NET2 = NET2 + 1
  510 Z(NET2) = BUF(I)
      NWORDS  = NWORDS - 16
      IF (NWORDS .NE. 0) GO TO 500
C
C     ALL DATA IS NOW IN CORE FOR THIS SET-ID
C
  520 CALL CLOSE (SCR2,REW)
      IF (.NOT.ANYET .AND. .NOT.ANYGPT) GO TO 340
C
C     SORT THE 7-WORD TEMP CARDS ON ID AND CHECK FOR DUPLICATE ID S
C     AMONG ALL THE ELEMENT TEMPERATURE DATA
C
      IF (IET1 .LT. NET1) CALL SORT (0,0, 7,1,Z(IET1),NET1-IET1+1)
      IF (IET2 .LT. NET2) CALL SORT (0,0,15,1,Z(IET2),NET2-IET2+1)
C
      LET1 = (NET1 - IET1 + 1)/7
      LET2 = (NET2 - IET2 + 1)/15
      LGPT = (NGPT - IGPT + 1)/2
      LFLAG = .FALSE.
      IF (LET1 .LE. 1) GO TO 560
      ID = Z(IET1)
      J  = IET1 + 7
      DO 550 I = J,NET1,7
      IF (ID .NE. Z(I)) GO TO 540
C
C     ERROR - TWO OR MORE ID-S EQUAL IN TEMPERATURE DATA WITHIN A SET.
C
      WRITE  (OUTPT,530) UFM,SETID,ID
  530 FORMAT (A23,' 4011, ELEMENT TEMPERATURE SET',I9,' CONTAINS ',
     1       'MULTIPLE TEMPERATURE DATA SPECIFIED FOR ELEMENT ID',I9)
      LFLAG = .TRUE.
  540 ID = Z(I)
  550 CONTINUE
  560 IF (LET2 .LE. 1) GO TO 590
      ID = Z(IET2)
      J  = IET2 + 15
      DO 580 I = J,NET2,15
      IF (ID .NE. Z(I)) GO TO 570
      WRITE (OUTPT,530) UFM,SETID,ID
      LFLAG = .TRUE.
  570 ID = Z(I)
  580 CONTINUE
C
C     OPEN GEOM2, PREPARE TO PASS GEOM2, AND OUTPUT A RECORD OF THE ETT.
C
  590 FILE = GEOM2
      CALL PRELOC (*820,Z(BUF1),GEOM2)
C
C     OPEN ETT TO PUT OUT DATA-RECORD FOR THIS SET AND WRITE SETID,
C
      FILE = ETT
      CALL OPEN (*820,ETT,Z(BUF2),WRT)
      CALL WRITE (ETT,SETID,1,0)
C
C     RUN THROUGH POSSIBLE TEMPERATURE DEPENDENT ELEMENTS ON GEOM2.
C
      FILE = GEOM2
  595 CALL ECTLOC (*760,FILE,BUF,I)
C
C     OK DATA FOR A CARD TYPE HAS BEEN FOUND.  WRITE EL-TYPE AND
C     DATA FOR A CARD TYPE FOUND.
C
      BUF(1) = ELEM(I+2)
      BUF(2) = ELEM(I+14) - 1
      IELTYP = BUF(1)
C
C     WRITE ELEMENT TYPE HEADER
C
      CALL WRITE (ETT,BUF,2,0)
      IF (ELEM(I+13) .EQ. 0) GO TO 740
      JTEMP  = ELEM(I+13)
      OUTWDS = ELEM(I+14)
      ECTWDS = ELEM(I+ 5)
      IGRID  = ELEM(I+12)
      NGRID  = IGRID + ELEM(I+9) - 1
      FGRIDS = 0.0
  600 CALL READ (*840,*740,GEOM2,BUF,ECTWDS,0,FLAG)
C
C     ON FIRST PASS COUNT NUMBER OF NON-ZERO GRIDS
C
      IF (FGRIDS) 605,601,605
  601 DO 603 J = IGRID,NGRID
      IF (BUF(J) .NE. 0) FGRIDS = FGRIDS + 1.0
  603 CONTINUE
  605 CONTINUE
C
C     SELECT DATA TO BE OUTPUT
C
      IF (.NOT.ANYET) GO TO 650
      GO TO (610,620,650,650), JTEMP
C
C     1 - DIMENSIONAL ELEMENT-TEMP DATA MAY BE AVAIL.
C
  610 IF (LET2 .LT. 1) GO TO 650
      CALL BISLOC (*650,BUF(1),Z(IET2),15,LET2,J)
      J = IET2 + J
C
C     AVERAGE T-BAR-A AND T-BAR-B IF THIS IS A ROD, CONROD, OR TUBE
C
      IF (IELTYP.NE.1 .AND. IELTYP.NE.3 .AND. IELTYP.NE.10) GO TO 630
      RBUF(2) = (RZ(J) + RZ(J+1))/2.0
      GO TO 730
C
C     2 - DIMENSIONAL ELEMENT-TEMP DATA MAY BE AVAIL.
C
  620 IF (LET1 .LT. 1) GO TO 650
      CALL BISLOC (*650,BUF(1),Z(IET1),7,LET1,J)
      J = IET1 + J
  630 DO 640 K = 2,OUTWDS
      BUF(K) = Z(J)
      J = J + 1
  640 CONTINUE
      GO TO 730
C
C     CHECK FOR GRID-POINT-TEMP-DATA
C
  650 IF (.NOT.ANYGPT) GO TO 700
C
C     GRID-POINT-TEMP-DATA IS AVAILABLE FOR SOME OR ALL GRID POINTS.
C
      ANY   = .FALSE.
      RTEMP = 0.0
      II    = 0
      DO 670 K = IGRID,NGRID
      II = II + 1
      IF (BUF(K)) 655,665,655
  655 CALL BISLOC (*660,BUF(K),Z(IGPT),2,LGPT,J)
      J  = IGPT + J
      RTEMP = RTEMP + RZ(J)
      IF (II .GT. 32) CALL MESAGE (-61,0,0)
      TGRID(II) = RZ(J)
      ANY = .TRUE.
      GO TO 670
  660 IF (DEFALT .EQ. -1) GO TO 710
      RTEMP = RTEMP + DEFTMP
      TGRID(II) = DEFTMP
      GO TO 670
C
C     UNDEFINED GRID-POINT
C
  665 TGRID(II) = 0
  670 CONTINUE
C
C     IF NOTHING BUT DEFAULT DATA THEN WRITE NOTHING SINCE THE
C     DEFAULT IS IN THE HEADER RECORD.
C
      IF (.NOT.ANY) GO TO 735
C
C     IF BAR ELEMENT PUT GRID TEMPS INTO BUFFER FOR T-BAR-A AND T-BAR-B
C
      IF (IELTYP .NE. 34) GO TO 675
      RBUF(2) = TGRID(1)
      RBUF(3) = TGRID(2)
      J = 4
      GO TO 676
C
  675 RBUF(2) = RTEMP/FGRIDS
      J = 3
      IF (JTEMP .EQ. 4) J = 2
C
  676 IF (JTEMP .LT. 3) GO TO 690
      DO 680 K = 1,II
      RBUF(J) = TGRID(K)
  680 J = J + 1
  690 IF (J .GT. OUTWDS) GO TO 730
      BUF(J) = 0
      J = J + 1
      GO TO 690
C
C     NO GRID-POINT-TEMP-DATA.  VERIFY THAT THERE IS A DEFAULT TEMP.
C
  700 IF (DEFALT .NE. -1) GO TO 735
C
C     ERROR NO TEMP DATA OR DEFALT OF ANY KIND FOR THIS ID.
C
  710 LFLAG = .TRUE.
      WRITE  (OUTPT,720) UFM,SETID,BUF(1)
  720 FORMAT (A23,' 4012, THERE IS NO ELEMENT, GRID POINT, OR DEFAULT',
     1       ' TEMPERATURE DATA FOR', /30X,'TEMPERATURE SET',I12,
     2       ', WITH RESPECT TO ELEMENT ID =',I8)
      GO TO 735
C
C     OUTPUT ELEMENT-TEMPERATURE DATA FOR 1 ELEMENT OF THIS TYPE IN SET
C
  730 CALL WRITE (ETT,BUF,OUTWDS,0)
      GO TO 600
C
C     OUTPUT A NEGATIVE ELEMENT ID SINCE THERE IS NO DATA AVAILABLE.
C
  735 ID = -BUF(1)
      CALL WRITE (ETT,ID,1,0)
      GO TO 600
C
C     END OF ELEMENTS FOR THIS EL-TYPE.  WRITE ZERO ON ETT
C
  740 CALL WRITE (ETT,0,1,0)
      GO TO 595
  760 CONTINUE
C
C     ETT-RECORD IS COMPLETE FOR THIS SET. WRITE EOR AND PROCESS NEXT
C     SET.
C
      CALL WRITE (ETT,0,0,1)
      CALL CLOSE (ETT,NOREW)
      GO TO 340
C
C     ETT IS COMPLETE
C
  770 IF (LFLAG) CALL MESAGE (-61,0,0)
C
C     WRITE TRAILER FOR ETT
C
      BUF(1) = ETT
      BUF(7) = 7
      DO 775 I = 2,6
  775 BUF(I) = 0
C
C     OPEN ETT AND APPEND GPTT SECTION OF TEMP DATA IN INTERNAL NOTATION
C
      FILE = ETT
      CALL OPEN (*820,ETT,Z(BUF2),WRT)
      IF (.NOT.ANYGPT .AND. .NOT.HEAT) GO TO 800
C
C     OPEN SCR1 AND SKIP THE TEMPERATURE DATA HAVING EXTERNAL INDICES
C
      FILE = SCR1
      CALL GOPEN (SCR1,Z(BUF1),RDREW)
      IF (IRECS) 790,790,780
  780 DO 785 I = 1,IRECS
      CALL FWDREC (*840,SCR1)
  785 CONTINUE
C
C     COPY BALANCE OF SCR1 TO ETT
C
  790 CALL READ  (*800,*795,SCR1,Z,BUF2-1,0,FLAG)
      CALL WRITE (ETT,Z,BUF2-1,0)
      GO TO 790
  795 CALL WRITE (ETT,Z,FLAG,1)
      GO TO 790
  800 CALL CLOSE (SCR1,REW)
      CALL CLOSE (ETT, REW)
      CALL WRTTRL (BUF)
C
C     THERE WAS NO GPTT DATA AND ALSO NO ETT DATA. THUS RETURN HAVING
C     CREATED NO ETT DATA SET.
C
  810 RETURN
C
C     ERROR CONDITIONS ON FILES
C
C
C     FILE NOT IN FIST OR PURGED
C
  820 J = -1
      GO TO 850
C
C     EOF HIT WHILE READING FILE
C
  840 J = -2
  850 CALL MESAGE (J,FILE,NAM)
      RETURN
      END
