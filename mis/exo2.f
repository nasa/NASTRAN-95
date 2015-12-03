      SUBROUTINE EXO2
C
C     EXO2 PERFORMS EXTERNAL FORMAT SOFOUT OPERATIONS
C
      EXTERNAL RSHIFT   ,ANDF
      LOGICAL  UNIVAC
      INTEGER  DRY      ,COR(1)   ,UNAME    ,TYPE     ,UNIT     ,
     1         ITMS(50) ,SYSBUF   ,A        ,EOL      ,EOR      ,
     2         DITSIZ   ,Z        ,ALL      ,Q4       ,T3       ,
     3         MATRIC   ,TABLES   ,PHASE3   ,WHOLE(2) ,SUBR(2)  ,
     4         BLANK    ,SOF      ,XXXX     ,SRD      ,PRC      ,
     5         SWRT     ,EOG      ,EOI      ,SP       ,BAR      ,
     6         SCR1     ,BUF1     ,BUF2     ,BUF3     ,ELTYPE   ,
     7         BUF4     ,RC       ,HDR(7)   ,TYPOUT   ,BDIT     ,
     8         BMDI     ,RSHIFT   ,ANDF     ,OFFSET
      INTEGER  EQSS     ,BGSS     ,CSTM     ,LODS     ,LOAP     ,
     1         PLTS     ,SOLN     ,LAMS
      DOUBLE PRECISION   DZ(1)    ,DA
      CHARACTER          UFM*23   ,UWM*25   ,UIM*29   ,SFM*25   ,
     1                   SWM*27
      COMMON  /XMSSG /   UFM      ,UWM      ,UIM      ,SFM      ,
     1                   SWM
      COMMON  /BLANK /   DRY      ,X1(3)    ,UNAME(2) ,X2(6)    ,
     1                   TYPE(2)  ,NAMES(10),UNIT     ,UNIVAC   ,
     2                   LBUF     ,IADD
      COMMON  /SYSTEM/   SYSBUF   ,NOUT     ,X3(6)    ,NLPP     ,
     1                   X4(2)    ,LINE     ,X6(9)    ,MACH
      COMMON  /NAMES /   RD       ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW
      COMMON  /TYPE  /   PRC(2)   ,NWORD(4)
      COMMON  /ZNTPKX/   A(4)     ,IROW     ,EOL      ,EOR
      COMMON  /SOF   /   X5(3)    ,DITSIZ
      COMMON  /ITEMDT/   NITEM    ,ITEMS(7,1)
      COMMON  /ZZZZZZ/   Z(1)
      EQUIVALENCE        (COR(1)  ,Z(1))
      EQUIVALENCE        (Z(1)    ,DZ(1))   ,(A(1)    ,DA)
      DATA     ALL      ,MATRIC   ,TABLES   ,PHASE3   ,WHOLE          /
     1         4HALL    ,4HMATR   ,4HTABL   ,4HPHAS   ,4HWHOL  ,4HESOF/,
     2         SUBR               ,BLANK    ,SOF      ,XXXX           /
     3         4HEXO2   ,4H       ,4H       ,4HSOF    ,4HXXXX         /,
     4         EQSS     ,BGSS     ,CSTM     ,LODS     ,LOAP           /
     5         4HEQSS   ,4HBGSS   ,4HCSTM   ,4HLODS   ,4HLOAP         /,
     6         PLTS     ,SOLN     ,LAMS     ,Q4       ,T3      ,BAR   /
     7         4HPLTS   ,4HSOLN   ,4HLAMS   ,2HQ4     ,2HT3    ,2HBR  /,
     8         SRD      ,SWRT     ,MORE     ,EOG      ,EOI     ,SP    /
     9         1        ,2        ,1        ,2        ,3       ,1     /,
     O         JH       ,SCR1     ,BDIT     ,BMDI                     /
     A         1        ,301      ,4HDIT    ,4HMDI                    /
C
C     INITIALIZE
C
      IF (NITEM .GT. 50) CALL ERRMKN (23,10)
      NCORE = KORSZ(Z)
      I     = NCORE - LBUF
      IF (MACH .EQ. 4) I = I - LBUF
      NCORE = I - 1
      IRW   = IADD
      IADD  = I
      CALL EXFORT (3,UNIT,0,0,IRW,0,0)
      BUF1  = NCORE - SYSBUF + 1
      BUF2  = BUF1  - SYSBUF - 1
      BUF3  = BUF2  - SYSBUF
      BUF4  = BUF3  - SYSBUF
      NCORE = BUF4  - 1
      IF (BUF4 .LE. 0) GO TO 9008
      CALL SOFOPN (Z(BUF1),Z(BUF2),Z(BUF3))
C
C     CONSTRUCT ARRAY OF NAMES OF ITEMS TO BE COPIED
C
      IF (TYPE(1) .NE. ALL) GO TO 10
      NITEMS = NITEM
      DO 5 I = 1,NITEM
    5 ITMS(I) = ITEMS(1,I)
      GO TO 70
   10 IF (TYPE(1) .NE. TABLES) GO TO 20
      NITEMS = 0
      DO 15 I = 1,NITEM
      IF (ITEMS(2,I) .GT. 0) GO TO 15
      NITEMS = NITEMS + 1
      ITMS(NITEMS) = ITEMS(1,I)
   15 CONTINUE
      GO TO 70
   20 IF (TYPE(1) .NE. MATRIC) GO TO 50
      NITEMS = 0
      DO 30 I = 1,NITEM
      IF (ITEMS(2,I) .LE. 0) GO TO 30
      NITEMS = NITEMS + 1
      ITMS(NITEMS) = ITEMS(1,I)
   30 CONTINUE
      GO TO 70
   50 IF (TYPE(1) .NE. PHASE3) GO TO 60
      NITEMS = 0
      DO 55 I = 1,NITEM
      IF (ANDF(ITEMS(7,I),8) .EQ. 0) GO TO 55
      NITEMS = NITEMS + 1
      ITMS(NITEMS) = ITEMS(1,I)
   55 CONTINUE
      GO TO 70
   60 NITEMS  = 2
      ITMS(1) = TYPE(1)
      ITMS(2) = TYPE(2)
      IF (ITMS(2) .EQ. BLANK) NITEMS = 1
C
C     PUT NAMES OF ALL SUBSTRUCTURES TO BE COPIED AT TOP OF OPEN CORE
C
   70 NSS = 0
      IF (NAMES(1).EQ.WHOLE(1) .AND. NAMES(2).EQ.WHOLE(2)) GO TO 90
      DO 80 I = 1,9,2
      IF (NAMES(I) .EQ. XXXX) GO TO 80
      NSS = NSS + 1
      IF (2*NSS .GT. NCORE) GO TO 9008
      Z(2*NSS-1) = NAMES(I  )
      Z(2*NSS  ) = NAMES(I+1)
   80 CONTINUE
      GO TO 110
   90 N = DITSIZ/2
      DO 100 I = 1,N
      CALL FDIT (I,J)
      IF (COR(J) .EQ. BLANK) GO TO 100
      NSS = NSS + 1
      IF (2*NSS .GT. NCORE) GO TO 9008
      Z(2*NSS-1) = COR(J)
      Z(2*NSS  ) = COR(J+1)
  100 CONTINUE
  110 ICORE  = 2*NSS + 3
      LCORE  = NCORE - ICORE + 1
      IDPCOR = ICORE/2 + 1
      CALL PAGE
C
C     WRITE OUT DIT AND MDI CONTROL WORDS
C
      N = DITSIZ/2
      IF (6*N .GT. LCORE) GO TO 9008
      HDR(1) = BDIT
      HDR(2) = BLANK
      HDR(3) = BLANK
      HDR(4) = 2
      HDR(5) = DITSIZ
      HDR(6) = SP
      HDR(7) = EOG
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      DO 112 I = 1,N
      CALL FDIT (I,J)
      Z(ICORE+2*I-2) = COR(J)
      Z(ICORE+2*I-1) = COR(J+1)
  112 CONTINUE
      CALL EXFORT (SWRT,UNIT,2,Z(ICORE),DITSIZ,SP,0)
      HDR(1) = BMDI
      HDR(4) = 10
      HDR(5) = 6*N
      HDR(7) = EOI
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      K = ICORE
      DO 114 I = 1,N
      CALL FMDI (I,J)
      Z(K  ) = RSHIFT(COR(J+1),20)
      Z(K+1) = ANDF(RSHIFT(COR(J+1),10),1023)
      Z(K+2) = ANDF(COR(J+1),1023)
      Z(K+3) = ANDF(RSHIFT(COR(J+2),20),1023)
      Z(K+4) = ANDF(RSHIFT(COR(J+2),10),1023)
      Z(K+5) = ANDF(COR(J+2),1023)
      K = K + 6
  114 CONTINUE
      CALL EXFORT (SWRT,UNIT,10,Z(ICORE),6*N,SP,0)
C
C     LOOP OVER ALL SUBSTRUCTURES AND ITEMS, COPYING EACH ONE TO THE
C     EXTERNAL FILE
C
      DO 1000 ISS = 1,NSS
      HDR(1) = Z(2*ISS-1)
      HDR(2) = Z(2*ISS)
      DO 990 ITEM = 1,NITEMS
      HDR(3) = ITMS(ITEM)
      ITM = ITTYPE (ITMS(ITEM))
      IF (ITM .EQ. 1) GO TO 800
      CALL SFETCH (HDR,HDR(3),SRD,RC)
      GO TO (140,120,990,120,120), RC
  120 LINE = LINE + 2
      IF (LINE .GT. NLPP) CALL PAGE
      IF (RC .GT. 3) GO TO 130
      WRITE (NOUT,6340) UWM,(HDR(I),I=1,3)
      GO TO 990
  130 CALL SMSG (RC-2,HDR(3),HDR)
      GO TO 990
  140 CALL SUREAD (Z(ICORE),LCORE,NWDS,RC)
      IF (RC .NE. 2) GO TO 9008
C
      IF (ITMS(ITEM) .EQ. EQSS) GO TO 200
      IF (ITMS(ITEM) .EQ. BGSS) GO TO 300
      IF (ITMS(ITEM) .EQ. CSTM) GO TO 400
      IF (ITMS(ITEM) .EQ. LODS) GO TO 500
      IF (ITMS(ITEM) .EQ. LOAP) GO TO 500
      IF (ITMS(ITEM) .EQ. PLTS) GO TO 600
      IF (ITMS(ITEM) .EQ. SOLN) GO TO 700
      IF (ITMS(ITEM) .EQ. LAMS) GO TO 700
      GO TO 1100
C
C     EQSS
C
C     GROUP 0
C
  200 N  = NWDS
      NS = Z(ICORE+2)
      IF (NS .GT. 13) N = 30
      HDR(4) = 3
      HDR(5) = N
      HDR(6) = SP
      HDR(7) = EOG
      IF (N .LT. NWDS) HDR(7) = MORE
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,3,Z(ICORE),N,SP,0)
      IF (N .EQ. NWDS) GO TO 210
      HDR(4) = 2
      HDR(5) = NWDS - N
      HDR(7) = EOG
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,2,Z(ICORE+N),NWDS-N,SP,0)
C
C     GROUPS 1 TO NS + 1
C
  210 HDR(4) = 10
      NS = NS + 1
      DO 220 J = 1,NS
      CALL SUREAD (Z(ICORE),LCORE,NWDS,RC)
      IF (RC .NE. 2) GO TO 9008
      HDR(5) = NWDS
      IF (J .EQ. NS) HDR(7) = EOI
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,10,Z(ICORE),NWDS,SP,0)
  220 CONTINUE
      GO TO 900
C
C     BGSS
C
C     GROUP 0
C
  300 HDR(4) = 3
      HDR(5) = 3
      HDR(6) = SP
      HDR(7) = EOG
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,3,Z(ICORE),3,SP,0)
C
C     GROUP 1
C
      CALL SUREAD (Z(ICORE),LCORE,NWDS,RC)
      IF (RC .NE. 2) GO TO 9008
      HDR(4) = 6
      HDR(5) = NWDS
      HDR(7) = EOI
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,6,Z(ICORE),NWDS,SP,0)
      GO TO 900
C
C     CSTM
C
C     GROUP 0
C
  400 HDR(4) = 3
      HDR(5) = 2
      HDR(6) = SP
      HDR(7) = EOG
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,3,Z(ICORE),2,SP,0)
C
C     GROUP 1
C
      IF (ICORE+13 .GT. NCORE) GO TO 9008
  420 CALL SUREAD (Z(ICORE),14,NWDS,RC)
      IF (RC .EQ. 2) GO TO 430
      HDR(4) = 8
      HDR(5) = 4
      HDR(7) = MORE
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,8,Z(ICORE),4,SP,0)
      HDR(4) = 9
      HDR(5) = 10
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,9,Z(ICORE+4),10,SP,0)
      GO TO 420
  430 HDR(5) = 0
      HDR(7) = EOG
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      HDR(4) = 0
      HDR(7) = EOI
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      GO TO 900
C
C     LODS AND LOAP
C
C     GROUP 0
C
  500 N  = NWDS
      NS = Z(ICORE+3)
      IF (NS .GT. 13) N = 30
      HDR(4) = 3
      HDR(5) = N
      HDR(6) = SP
      HDR(7) = EOG
      IF (N .LT. NWDS) HDR(7) = MORE
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,3,Z(ICORE),N,SP,0)
      IF (N .EQ. NWDS) GO TO 510
      HDR(4) = 2
      HDR(5) = NWDS -  N
      HDR(7) = EOG
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,2,Z(ICORE+N),NWDS-N,SP,0)
C
C     GROUP 1 TO NS
C
  510 HDR(4) = 10
      DO 520 J = 1,NS
      CALL SUREAD (Z(ICORE),LCORE,NWDS,RC)
      IF (RC .NE. 2) GO TO 9008
      HDR(5) = NWDS
      IF (J .EQ. NS) HDR(7) = EOI
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,10,Z(ICORE),NWDS,SP,0)
  520 CONTINUE
      GO TO 900
C
C     PLTS
C
C     GROUP 0
C
  600 N  = NWDS
      NS = Z(ICORE+2)
      HDR(6) = SP
      HDR(4) = 3
      HDR(5) = 3
      HDR(7) = MORE
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,3,Z(ICORE),3,SP,0)
      DO 620 J = 1,NS
      HDR(4) = 13
      HDR(5) = 4
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,13,Z(ICORE+14*J-11),4,SP,0)
      HDR(4) = 9
      HDR(5) = 10
      IF (J .EQ. NS) HDR(7) = EOG
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,9,Z(ICORE+14*J-7),10,SP,0)
  620 CONTINUE
C
C     GROUP 1 -- BGPDT
C
      CALL SUREAD (Z(ICORE),LCORE,NWDS,RC)
      IF (RC .EQ. 3) GO TO 680
      IF (RC .NE. 2) GO TO 9008
      HDR(4) = 6
      HDR(5) = NWDS
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,6,Z(ICORE),NWDS,SP,0)
C
C     GROUP 2 -- EQEXIN
C
      CALL SUREAD (Z(ICORE),LCORE,NWDS,RC)
      IF (RC .NE. 2) GO TO 9008
      HDR(4) = 10
      HDR(5) = NWDS
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,10,Z(ICORE),NWDS,SP,0)
C
C     GROUP 3 -- GPSETS
C
      CALL SUREAD (Z(ICORE),LCORE,NWDS,RC)
      IF (RC .NE. 2) GO TO 9008
      HDR(4) = 10
      HDR(5) = NWDS
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,10,Z(ICORE),NWDS,SP,0)
C
C     GROUP 4 -- ELSETS
C
C     OUTPUT CHANGES MADE BY G.CHAN/UNISYS   4/91
C
C     IN 90 AND EARLIER VERSIONS, ONLY ONE ELEMENT PLOT SYMBOL WORD WAS
C     WRITTEN OUT USING FORMAT 2, AND ON NEXT ELSETS DATA LINE, FORMAT
C     10 WAS USED FOR ALL ELEMENTS. NO OFFSET DATA WAS PROVIDED FOR THE
C     BAR, QUAD4 AND TRIA3 ELEMENTS. THE NO. OF GRID POINT PER ELEMENT,
C     NGPEL, WAS THE FIRST WORD ON THE ELSETS DATA LINE.  (LINE=RECORD)
C     ALSO, THE 90 AND EARLIER VERSIONS DID NOT COUNT PROPERTY ID, PID,
C     ON THE ELSETS DATA LINE. THUS THE TOTAL NO. OF WORDS MAY BE IN
C     ERROR AND MAY CAUSE EXTRA ZEROS TO APPEAR AT THE END OF THE LINE.
C
C     IN 91 VERSION, ELEMENT PLOT SYMBOL LINE HAS 2 WORDS, SYMBOL AND
C     NGPEL, AND FORMAT 25 IS USED. ON NEXT ELSETS DATA LINE, FORMAT 10
C     IS USED FOR ALL ELEMENTS WITH NO OFFSETS. FORMAT 26 IS USED FOR
C     THE BAR WHICH HAS 6 OFFSET VALUES, AND FORMATS 27 AND 28 ARE USED
C     FOR TRIA3 AND QUAD4 WHICH HAVE 1 OFFSET VALUE EACH. NOTE THAT
C     NGPEL HAS BEEN MOVED, AND IS NO LONGER THE FIRST WORD ON THE
C     ELSETS DATA LINE.
C
      HDR(7) = MORE
C
C     READ PLOT SYMBOL, AND NO. OF GRID POINTS PER ELEMENT
C     SET UP NO. OF OFFSET DATA FOR BAR, QUAD4 AND TRIA3
C
  640 CALL SUREAD (Z(ICORE),2,NWDS,RC)
      IF (RC .GE. 2) GO TO 670
      HDR(4) = 25
      HDR(5) = 2
      NGPEL  = Z(ICORE+1)
      ELTYPE = Z(ICORE  )
      OFFSET = 0
      IF (ELTYPE .EQ. BAR) OFFSET = 6
      IF (ELTYPE.EQ.Q4 .OR. ELTYPE.EQ.T3) OFFSET = 1
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,25,Z(ICORE),2,SP,0)
C
C     READ ELEMENT ID NUMBER, PROPERTY ID, GRID POINT CONNECTION INDICES
C     AND OFFSETS IF THEY EXIST
C     (ERROR IN 90 AND EARLIER VERSIONS, PROPERTY ID WAS LEFT OUT, AND
C     THEREFORE DATA COUNT PER ELEMENT WAS INCORRECT)
C
      N = ICORE - NGPEL - 2 - OFFSET
  650 N = N     + NGPEL + 2 + OFFSET
      IF (N .GT. NCORE) GO TO 9008
      CALL SUREAD (Z(N),1,NWDS,RC)
      IF (Z(N) .EQ. 0) GO TO 655
      IF (N+NGPEL+2+OFFSET .GT. NCORE) GO TO 9008
      CALL SUREAD (Z(N+1),NGPEL+1,NWDS,RC)
      IF (OFFSET .NE. 0) CALL SUREAD (Z(N+NGPEL+2),OFFSET,NWDS,RC)
      GO TO (650,6100,6100), RC
C
C     ALL ELEMENTS OF ONE TYPE READ INTO CORE, NOW COPY OUT
C
  655 HDR(5) = N - ICORE + 1
      IF (OFFSET-1) 660,  661,  663
C               REGULAR  QUAD4  BAR
C               ELEMENT  TRIA3
C
  660 HDR(4) = 10
      GO TO 665
  661 HDR(4) = 27
      IF (ELTYPE .EQ. Q4) HDR(4) = 28
      GO TO 665
  663 HDR(4) = 26
  665 CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,HDR(4),Z(ICORE),HDR(5),SP,0)
      GO TO 640
C
C     WRITE END-OF-ITEM FOR PLTS
C
  670 HDR(5) = 0
      HDR(7) = EOG
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
  680 HDR(4) = 0
      HDR(5) = 0
      HDR(7) = EOI
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      GO TO 900
C
C     SOLN AND LAMS
C
  700 IRFNO = Z(ICORE+2)
      IF (IRFNO .EQ. 1) GO TO 715
      IF (IRFNO .EQ. 2) GO TO 715
      IF (IRFNO .EQ. 3) GO TO 730
      IF (IRFNO .EQ. 8) GO TO 750
      IF (IRFNO .EQ. 9) GO TO 750
      LINE = LINE + 2
      IF (LINE .GT. NLPP) CALL PAGE
      WRITE (NOUT,6358) SWM,IRFNO,HDR(1),HDR(2)
      GO TO 900
C
C     GROUP 0 -- STATICS
C
  715 N  = NWDS
      NS = Z(ICORE+3)
      IF (NS .GT. 6) N = 23
      NS = Z(ICORE+4)
      HDR(4) = 16
      HDR(5) = N
      HDR(6) = SP
      HDR(7) = EOG
      IF (N .LT. NWDS) HDR(7) = MORE
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,16,Z(ICORE),N,SP,0)
      IF (N .EQ. NWDS) GO TO 710
      HDR(4) = 17
      HDR(5) = NWDS - N
      HDR(7) = EOG
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,17,Z(ICORE+N),NWDS-N,SP,0)
C
C     GROUPS 1 TO NS (ONE PER SUBCASE) -- STATICS
C
  710 DO 720 J = 1,NS
      CALL SUREAD (Z(ICORE),LCORE,NWDS,RC)
      IF (RC .NE. 2) GO TO 9008
      N = NWDS
      IF (Z(ICORE) .GT. 5) N = 11
      HDR(4) = 18
      HDR(5) = N
      HDR(7) = EOG
      IF (J .EQ. NS) HDR(7) = EOI
      IF (N .LT. NWDS) HDR(7) = MORE
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,18,Z(ICORE),N,SP,0)
      IF (N .EQ. NWDS) GO TO 720
      HDR(4) = 19
      HDR(5) = NWDS - N
      HDR(7) = EOG
      IF (J .EQ. NS) HDR(7) = EOI
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,19,Z(ICORE+N),NWDS-N,SP,0)
  720 CONTINUE
      GO TO 900
C
C     GROUP 0 -- NORMAL MODES (REAL OR COMPLEX)
C
  730 NS = Z(ICORE+3)
      HDR(4) = 3
      HDR(5) = 4
      HDR(6) = SP
      HDR(7) = EOG
      IF (NS .LE. 0) HDR(7) = EOI
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,3,Z(ICORE),4,SP,0)
      IF (NS .LE. 0) GO TO 900
C
C     GROUP 1 -- NORMAL MODES
C
      CALL SUREAD (Z(ICORE),LCORE,NWDS,RC)
      IF (RC .NE. 2) GO TO 9008
      HDR(4) = 20
      HDR(5) = NWDS
      HDR(7) = EOI
      IF (ITMS(ITEM) .EQ. LAMS) HDR(7) = EOG
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,20,Z(ICORE),NWDS,SP,0)
      IF (ITMS(ITEM) .NE. LAMS) GO TO 900
C
C     GROUP 2 -- NORMAL MODES (LAMS ITEM ONLY)
C
      CALL SUREAD (Z(ICORE),LCORE,NWDS,RC)
      IF (RC .NE. 2) GO TO 9008
      HDR(4) = 10
      HDR(5) = NWDS
      HDR(7) = EOI
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,10,Z(ICORE),NWDS,SP,0)
      GO TO 900
C
C     GROUP 0 -- DYNAMICS
C
  750 NS    = Z(ICORE+3)
      NWDS0 = 3*NS + 5
      N     = NWDS0
      IF (NS .GT. 6) N = 23
      NS    = Z(ICORE+4) + 1
      IF (Z(ICORE+NWDS0) .EQ. 0) NS = 1
      HDR(4) = 16
      HDR(5) = N
      HDR(6) = SP
      HDR(7) = MORE
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,16,Z(ICORE),N,SP,0)
      IF (N .EQ. NWDS0) GO TO 760
      HDR(4) = 17
      HDR(5) = NWDS0 - N
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,17,Z(ICORE+N),NWDS0-N,SP,0)
  760 HDR(4) = 10
      HDR(5) = NWDS - NWDS0
      HDR(7) = EOG
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,10,Z(ICORE+NWDS0),NWDS-NWDS0,SP,0)
C
C     GROUP 1 TO NS+1 -- DYNAMICS
C
      DO 770 J = 1,NS
      CALL SUREAD (Z(ICORE),LCORE,NWDS,RC)
      IF (RC .NE. 2) GO TO 9008
      HDR(4) = 9
      HDR(5) = NWDS
      HDR(7) = EOG
      IF (J .EQ. NS) HDR(7) = EOI
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,9,Z(ICORE),NWDS,SP,0)
  770 CONTINUE
      GO TO 900
C
C     UNKNOWN TABLE ITME
C
 1100 LINE = LINE + 2
      IF (LINE .GT. NLPP) CALL PAGE
      WRITE (NOUT,6360) SWM,ITMS(ITEM)
      GO TO 990
C
C     MATRICES
C
C     ON CDC MACHINE (NOT ANY 64-BIT MACHINE), FORCE ALL MATRIX DATA TO
C     BE DOUBLE PRECISION SO THE EXTRA DIGITS WONT BE LOST GOING TO
C     OTHER MACHINES
C
C     GROUP 0 -- MATRIX TRAILER
C
  800 CALL SOFTRL (HDR,HDR(3),Z(ICORE-1))
      RC = Z(ICORE-1)
      GO TO (805,120,990,120,120), RC
  805 TYPOUT = Z(ICORE+3)
      IF (MACH.EQ.4 .AND. PRC(TYPOUT).EQ.1) TYPOUT = TYPOUT + 1
      Z(ICORE+3) = TYPOUT
      NCOL   = Z(ICORE)
      HDR(4) = 10
      HDR(5) = 6
      HDR(6) = SP
      HDR(7) = EOG
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,10,Z(ICORE),6,SP,0)
C
C     MOVE MATRIX TO SCR2
C
      CALL MTRXI (SCR1,HDR,HDR(3),Z(BUF4),RC)
      CALL GOPEN (SCR1,Z(BUF4),RDREW)
C
C     COPY MATRIX OUT ONE COLUMN AT A TIME, NON-ZEROES ONLY.
C
C                        ROW NO.  +
C                        VALUE     +
C                        ROW NO.    +
C                        VALUE       I  FORMAT OF ONE MATRIX
C                          .         I  COLUMN ON THE EXTERNAL
C                          .         I  FILE.
C                          .        +
C                        -1        +
C                        0.0      +
C
      HDR(4) = 20 + TYPOUT
      HDR(6) = TYPOUT
      IPRC   = PRC(TYPOUT)
      N  = NWORD(TYPOUT) + IPRC
      N2 = NWORD(TYPOUT) + 1
      DO 830 J = 1,NCOL
      NWDS = 0
      K  = ICORE
      CALL INTPK (*820,SCR1,0,TYPOUT,0)
  810 CALL ZNTPKI
      Z(K     ) = IROW
      Z(K+IPRC) = A(1)
      IF (TYPOUT .EQ. 1) GO TO 815
      Z(K+IPRC+1) = A(2)
      IF (TYPOUT .LE. 3) GO TO 815
      Z(K+4) = A(3)
      Z(K+5) = A(4)
  815 NWDS = NWDS + N2
      K = K + N
      IF (K+N .GT. NCORE) GO TO 9008
      IF (EOL .EQ.     0) GO TO 810
  820 Z(K) = -1
      Z(K+IPRC  ) = 0
      Z(K+IPRC+1) = 0
      Z(K+4) = 0
      Z(K+5) = 0
      NWDS   = NWDS + N2
      HDR(5) = NWDS
      IF (J .EQ. NCOL) HDR(7) = EOI
      CALL EXFORT (SWRT,UNIT,JH,HDR,7,SP,0)
      CALL EXFORT (SWRT,UNIT,20+TYPOUT,Z(ICORE),NWDS,TYPOUT,DZ(IDPCOR))
  830 CONTINUE
      CALL CLOSE (SCR1,REW)
C
C     WRITE USER MESSAGE FOR SUCCESSFUL COPY
C
  900 LINE = LINE + 1
      IF (LINE .GT. NLPP) CALL PAGE
      WRITE (NOUT,6357) UIM,HDR(1),HDR(2),HDR(3),SOF,UNAME
  990 CONTINUE
 1000 CONTINUE
C
C     NORMAL MODULE COMPLETION.  WRITE LOGICAL EOF
C
      CALL EXFORT (4,UNIT,0,0,1,0,0)
      CALL SOFCLS
      RETURN
C
C     ABNORMAL MODULE COMPLETION
C
 6100 CALL SMSG (RC+4,ITMS(ITEM),HDR)
      GO TO 9100
 9008 CALL MESAGE (8,0,SUBR)
 9100 DRY = -2
      CALL SOFCLS
      RETURN
C
C     MESSAGE TEXT
C
 6340 FORMAT (A25,' 6340, SUBSTRUCTURE ',2A4,' ITEM ',A4, /5X,
     1        ' PSEUDO-EXISTS ONLY AND CANNOT BE COPIED OUT BY EXIO.')
 6357 FORMAT (A29,' 6357, SUBSTRUCTURE ',2A4,' ITEM ',A4,
     1        ' SUCCESSFULLY COPIED FROM ',A4,' TO ',2A4)
 6358 FORMAT (A27,' 6358, ILLEGAL RIGID FORMAT NUMBER ',I5,
     1        ' IN SOLN ITEM FOR SUBSTRUCTURE ',2A4,1H.,
     2        /34X,'THE ITEM WILL NOT BE COPIED.')
 6360 FORMAT (A27,' 6360, SOFOUT (EXTERNAL) ENCOUNTERS A UNSUPPORTED ',
     1        'TABLE ITEM ',A4, /35X,'THE ITEM WILL NOT BE COPIED.')
      END
