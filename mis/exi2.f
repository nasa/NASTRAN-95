      SUBROUTINE EXI2
C
C     EXI2 PERFORMS EXTERNAL FORMAT SOFIN OPERATIONS
C
      EXTERNAL LSHIFT
      LOGICAL  USRMSG
      INTEGER  DRY      ,UNAME    ,UNIT     ,SYSBUF   ,A        ,
     1         Z        ,SOF      ,PRC      ,Q4       ,T3       ,
     2         SRD      ,SWRT     ,EOI      ,SP       ,BAR      ,
     3         SCR1     ,SUBR(2)  ,BUF1     ,BUF2     ,BUF3     ,
     4         BUF4     ,HDR(7)   ,RC       ,MCB(7)   ,PREC     ,
     5         DIT      ,NAME(2)  ,EOG      ,PLTS     ,OFFSET
      REAL     ZERO(6)
      DOUBLE PRECISION   DZ(1)    ,DA
      CHARACTER          UFM*23   ,UWM*25   ,UIM*29
      COMMON  /XMSSG /   UFM      ,UWM      ,UIM
      COMMON  /MACHIN/   MACH
      COMMON  /BLANK /   DRY      ,X1(3)    ,UNAME(2) ,X2(18)   ,
     1                   UNIT     ,UNIVAC   ,LBUF     ,IADD
      COMMON  /SYSTEM/   SYSBUF   ,NOUT     ,X3(6)    ,NLPP     ,
     1                   X4(2)    ,LINE
      COMMON  /ZBLPKX/   A(4)     ,IROW
      COMMON  /NAMES /   RD       ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW
      COMMON  /TYPE  /   PRC(2)   ,NWORD(4)
      COMMON  /ZZZZZZ/   Z(1)
      EQUIVALENCE       (Z(1),DZ(1))        ,(A(1),DA)
      DATA     SOF      ,SRD      ,SWRT     ,EOI      ,SP       /
     1         4HSOF    ,1        ,2        ,3        ,1        /
      DATA     LEOF     ,JH       ,SCR1     ,SUBR               /
     3         4H$EOF   ,1        ,301      ,4HEXI2   ,4H       /
      DATA     DIT      ,MDI      ,EOG      ,ZERO               /
     5         4HDIT    ,4HMDI    ,2        ,6*0.0              /
      DATA     Q4       ,T3       ,BAR      ,PLTS               /
     7         2HQ4     ,2HT3     ,2HBR     ,4HPLTS             /
C
C     INITIALIZE
C
      NCORE = KORSZ(Z)
      I     = NCORE - LBUF
      IF (MACH .EQ. 12) I = I - LBUF
      NCORE = I - 1
      IRW   = IADD
      IADD  = I
      CALL EXFORT (3,UNIT,0,0,IRW,0,0)
      BUF1  = NCORE - SYSBUF + 1
      BUF2  = BUF1  - SYSBUF - 1
      BUF3  = BUF2  - SYSBUF
      BUF4  = BUF3  - SYSBUF
      NCORE = BUF4  - 1
      NOS   = 0
      IDM   = 1
      USRMSG=.TRUE.
      LCORE = NCORE
      IF (NCORE .LE. 0) GO TO 9008
      CALL SOFOPN (Z(BUF1),Z(BUF2),Z(BUF3))
      CALL PAGE
C
C     READ THE HEADER OF THE NEXT ITEM AND FETCH THE ITEM ON THE SOF
C
   10 CALL EXFORT (SRD,UNIT,JH,HDR,7,SP,0)
   20 NAME(1) = HDR(1)
      NAME(2) = HDR(2)
      ITEM    = HDR(3)
      ITEST   = HDR(7)
      IF (ITEST .EQ. EOI) ITEST = EOG
      IF (HDR(1).EQ.DIT .OR. HDR(1).EQ. MDI) GO TO 200
      IF (HDR(3).EQ. -1 .OR. HDR(1).EQ.LEOF) GO TO 300
      ITM = ITTYPE(HDR(3))
      IF (ITM .EQ. 1) GO TO 100
      RC = 3
      CALL SFETCH (HDR(1),HDR(3),SWRT,RC)
      IF (RC .EQ. 3) GO TO 60
      LINE = LINE + 2
      IF (LINE .GT. NLPP) CALL PAGE
      GO TO (30,30,60,40,50), RC
   30 WRITE (NOUT,6346) UWM,HDR(1),HDR(2),HDR(3)
      USRMSG = .FALSE.
      GO TO 60
   40 CALL EXLVL (NOS,Z(IDM),HDR,Z,LCORE)
      RC = 3
      CALL SFETCH (HDR(1),HDR(3),SWRT,RC)
      IF (RC .EQ. 3) GO TO 60
   50 CALL SMSG (RC-2,HDR(3),HDR)
      USRMSG = .FALSE.
   60 CONTINUE
C
C     TABLES
C
C
C     ELSETS TABLE CORRECTION BY G.CHAN/UNISYS   4/91
C
C     IN 91 VERSION, ELEMENT PLOT SYMBOL LINE HAS 2 WORDS, SYMBOL AND
C     NO. OF GRID POINT PER ELEMENT, NGPEL, WRITTEN OUT BY EXO2 USING
C     FORMAT 25. THE ELSETS DATA LINE COMING UP NEXT USE FORMAT 10 FOR
C     ELEMENTS WITH NO OFFSETS, FORMAT 26 FOR BAR WHICH HAS 6 OFFSET
C     VALUES, AND FORMATS 27 AND 28 FOR TRIA3 AND QUAD4 WHICH HAS 1
C     OFFSET VALUE EACH.
C     IN 90 AND EARLIER VERSIONS, ONLY ONE ELEMENT PLOT SYMBOL WORD WAS
C     WRITTEN OUT, AND ON ELSETS DATA LINE COMING UP NEXT, FORMAT 10
C     WAS USED FOR ALL ELEMENTS. NO OFFSET DATA FOR THE BAR, QUAD4 AND
C     TRIA3 ELEMENTS. NGPEL WAS THE FIRST WORD ON THE ELSETS DATA LINE.
C     ALSO, THE 90 AND EARLIER VERSIONS DID NOT COUNT PROPERTY ID, PID,
C     ON THE ELSETS DATA LINE. THUS THE TOTAL NO. OF WORDS MAY BE IN
C     ERROR AND MAY CAUSE EXTRA ZEROS AT THE END OF THE DATA LINE.
C
C     THEREFORE, IF THE 90 OR EARLIER EXTERNAL SOF FILE WAS USED, WE
C     NEED TO ADD THE OFFSETS (1 OR 6 FLOATING POINTS ZEROS) TO THE BAR,
C     QUAD4 AND TRIA3 ELEMENTS FOR THE ELSETS TABLE.
C     (AS OF 4/91, THESE CHANGES HAVE NOT BEEN TESTED)
C
      OFFSET = 0
   70 NWDS = HDR(5)
      IF (NWDS .GT. LCORE) GO TO 9008
      CALL EXFORT (SRD,UNIT,HDR(4),Z,NWDS,SP,0)
      IF (OFFSET .EQ. 0) GO TO 80
      J = 1
      CALL SUWRT (Z(1),1,J)
      NP2 = Z(1) + 2
      DO 73 K = 2,NWDS,NP2
      IF (Z(K) .EQ. 0) GO TO 75
      CALL SUWRT (Z(K),NP2,J)
      CALL SUWRT (ZERO,OFFSET,J)
   73 CONTINUE
   75 Z(1) = 0
      NWDS = 1
   80 CALL SUWRT (Z,NWDS,ITEST)
      IF (HDR(7) .EQ. EOI) GO TO 90
      CALL EXFORT (SRD,UNIT,JH,HDR,7,SP,0)
      IF (HDR(1).NE.NAME(1) .OR. HDR(2).NE.NAME(2)) GO TO 160
      IF (HDR(3) .NE. ITEM) GO TO 160
      ITEST = HDR(7)
      IF (ITEST .EQ. EOI) ITEST = EOG
      IF (ITEM.NE.PLTS .OR. HDR(5).NE.1 .OR. HDR(4).NE.10) GO TO 85
      OFFSET = 0
      IF (Z(1) .EQ. BAR) OFFSET = 6
      IF (Z(1).EQ.Q4 .OR. Z(1).EQ.T3) OFFSET = 1
   85 IF (HDR(4) .GT. 0) GO TO 70
   90 ITEST = EOI
      CALL SUWRT (0,0,ITEST)
      GO TO 140
C
C     MATRICES
C
C
C     READ TRAILER
C
  100 CALL SOFTRL (HDR(1),HDR(3),MCB(1))
      RC = MCB(1)
      IF (RC .EQ. 3) GO TO 108
      LINE = LINE + 2
      IF (LINE .GT. NLPP) CALL PAGE
      GO TO (102,102,108,104,106), RC
  102 WRITE (NOUT,6346) UWM,HDR(1),HDR(2),HDR(3)
      USRMSG = .FALSE.
      GO TO 108
  104 CALL EXLVL (NOS,Z(IDM),HDR,Z,LCORE)
      GO TO 108
  106 CALL SMSG (3,HDR(3),HDR)
      USRMSG = .FALSE.
  108 CALL EXFORT (SRD,UNIT,HDR(4),MCB(2),6,SP,0)
      NCOL   = MCB(2)
      PREC   = MCB(5)
      MCB(1) = SCR1
      MCB(2) = 0
      MCB(6) = 0
      MCB(7) = 0
      IF (USRMSG) CALL GOPEN (SCR1,Z(BUF4),WRTREW)
C
C     READ MATRIX ONE COLUMN AT A TIME AND PACK ON SCR2
C
      DO 130 J = 1,NCOL
      CALL EXFORT (SRD,UNIT,JH,HDR,7,SP,0)
      IF (HDR(1).NE.NAME(1) .OR. HDR(2).NE.NAME(2)) GO TO 160
      IF (HDR(3) .NE. ITEM) GO TO 160
      NWDS = HDR(5)
      IF (NWDS*1.4 .GT. NCORE) GO TO 9008
      CALL EXFORT (SRD,UNIT,HDR(4),Z,NWDS,PREC,DZ)
      IF (.NOT. USRMSG) GO TO 130
      CALL BLDPK (PREC,PREC,SCR1,0,0)
      IPRC = PRC(PREC)
      N    = NWORD(PREC) + IPRC
      K    = 1
  110 IF (Z(K) .LT. 0) GO TO 120
      IROW = Z(K)
      A(1) = Z(K+IPRC)
      IF (PREC .EQ. 1) GO TO 115
      A(2) = Z(K+IPRC+1)
      IF (PREC .LE. 3) GO TO 115
      A(3) = Z(K+4)
      A(4) = Z(K+5)
  115 CALL ZBLPKI
      K = K + N
      GO TO 110
  120 CALL BLDPKN (SCR1,0,MCB)
  130 CONTINUE
      IF (.NOT.USRMSG) GO TO 150
      CALL WRTTRL (MCB)
      CALL CLOSE  (SCR1,REW)
      CALL MTRXO  (SCR1,HDR,HDR(3),0,RC)
C
C     WRITE USER MESSAGE
C
  140 IF (.NOT.USRMSG) GO TO 150
      LINE = LINE + 1
      IF (LINE .GT. NLPP) CALL PAGE
      WRITE (NOUT,6357) UIM,HDR(1),HDR(2),HDR(3),UNAME,SOF
  150 USRMSG = .TRUE.
      GO TO 10
C
C     NO EOI FOR ITEM AND A NEW ITEM WAS READ
C
  160 LINE = LINE + 2
      IF (LINE .GT. NLPP) CALL PAGE
      WRITE (NOUT,6363) UWM,NAME(1),NAME(2),ITEM,UNAME
      IF (ITM .EQ. 0) CALL DELETE (NAME,ITEM,RC)
      IF (ITM .EQ. 1) CALL CLOSE (SCR1,REW)
      USRMSG = .TRUE.
      GO TO 20
C
C     READ DIT AND MDI
C
  200 NOS   = HDR(5)/2
      LCORE = NCORE - HDR(5)*4
      IDM   = LCORE + 1
      IF (6*NOS .GT. LCORE) GO TO 9008
      CALL EXFORT (SRD,UNIT,HDR(4),Z,HDR(5),SP,0)
      DO 210 I = 1,NOS
      Z(IDM+4*I-4) = Z(2*I-1)
      Z(IDM+4*I-3) = Z(2*I  )
  210 CONTINUE
      CALL EXFORT (SRD,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SRD,UNIT,HDR(4),Z,HDR(5),SP,0)
      DO 220 I = 1,NOS
      J = IDM + 4*I - 2
      K = 6*I - 6
      Z(J  ) = LSHIFT(Z(K+1),20) + LSHIFT(Z(K+2),10) + Z(K+3)
      Z(J+1) = LSHIFT(Z(K+4),20) + LSHIFT(Z(K+5),10) + Z(K+6)
  220 CONTINUE
      GO TO 10
C
C     NORMAL MODULE COMPLETION
C
  300 CALL SOFCLS
      RETURN
C
C     ABNORMAL MODULE COMPLETION
C
 9008 CALL MESAGE (8,0,SUBR)
      DRY = -2
      CALL SOFCLS
      RETURN
C
C     MESSAGE TEXTS
C
 6346 FORMAT (A25,' 6346, SUBSTRUCTURE ',2A4,' ITEM ',A4,
     1       ' NOT COPIED.  IT ALREADY EXISTS ON THE SOF.')
 6357 FORMAT (A29,' 6357, SUBSTRUCTURE ',2A4,' ITEM ',A4,
     1       ' SUCCESSFULLY COPIED FROM ',2A4,' TO ',A4)
 6363 FORMAT (A25,' 6363, INCOMPLETE DATA FOR SUBSTRUCTURE ',2A4,
     1       ' ITEM ',A4,' ON ',2A4,'. THE ITEM WILL NOT BE COPIED.')
      END
