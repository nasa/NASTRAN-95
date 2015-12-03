      SUBROUTINE ASDMAP
C
C     THIS ROUTINE PROCESSES THE SUBSTRUCTURE COMMAND DATA DECK
C
C     IT CREATES A SET OF SUBSTRUCTURE DATA ON THE FRONT OF THE CASE
C     FILE AND GENERATES DMAP ALTERS FOR THE XALTER FILE. THE ALTERS ARE
C     PLACED FIRST ON THE SCRATCH FILE AND THEN COPIED TO THE PROBLEM
C     TAPE
C
      IMPLICIT INTEGER (A-Z)
      EXTERNAL        ANDF        ,ORF        ,RSHIFT      ,LSHIFT    ,
     1                COMPLF
      LOGICAL         ALTER       ,ALTFL      ,FIRST       ,IFIN      ,
     1                SOLVE       ,OPSOF      ,PASS2       ,RECOV     ,
     2                REJECT      ,SKIP
      REAL            FACT        ,XX
      DIMENSION       ALTS(18)    ,CARD(20)   ,CDATA(30)   ,FNAME(7)  ,
     1                COMND(2,25) ,DMAP(18,60),EXTRA(3,200),II(9)     ,
     2                NASUB(2,100),OCARD(200) ,ITEMP(200)  ,ASD1(2)   ,
     3                ASD2(2)     ,PHS(3)     ,EXDEF(2,14) ,ITMN(5)   ,
     4                NCASEC(2)   ,NCASES(2)  ,NHEAD(16)   ,SUBNAM(2) ,
     5                DVEC(3)     ,DBVAR(6)   ,DBVAL(2,6,5),R3VAR(5)  ,
     6                R3VAL(5,5)  ,VAR(3,200) ,IVAR(3,200) ,Z(1)      ,
     7                COREY(2)
      CHARACTER       UFM*23      ,UWM*25     ,UIM*29      ,SFM*25
      COMMON /XMSSG / UFM         ,UWM        ,UIM         ,SFM
      COMMON /MACHIN/ MCHN
      COMMON /GINOX / IDUM(161)   ,IGINOB
      COMMON /BLANK / XX
      COMMON /ASDBD / IRDM        ,NRDM       ,IXTRA       ,NXTRA     ,
     1                IOCT        ,NOCT       ,IPTBS       ,NPTBS     ,
     1                IPH         ,NPH        ,IDAT(1248)
      COMMON /ZZZZZZ/ COREX(1)
      COMMON /OUTPUT/ IOTIT(68)   ,IHEAD(20)
      COMMON /SOFCOM/ NSOF        ,NNAME(10)  ,LENGTH(10)  ,STAT      ,
     1                PASWD(2)    ,FIRST      ,OPSOF
      COMMON /SYSTEM/ SYS(90)     ,LPCH
      EQUIVALENCE     (IBUF,SYS(1))           ,(OUTT,SYS(2))          ,
     1                (NOGO,SYS(3))           ,(INTP,SYS(4))          ,
     2                (NLPP,SYS(9))           ,(NLINES,SYS(12))       ,
     3                (IPREC,SYS(55))         ,(BANDIT,SYS(77))
      EQUIVALENCE     (VAR(1,1),IVAR(1,1))    ,(ITEMP(1),OCARD(1))    ,
     1                (COREX(1),COREY(1),NDBS),(COREY(2),Z(1))
      DATA   ALT1   / 4HALTE /,            ALT2   / 4HR    /,
     1       ASD1   / 4HASDM,4HBEGN /,     ASD2   / 4HASDM,4HEND /,
     2       BLANK  / 4H     /,            CASE   / 4HCASE /,
     3       DISK   / 4HDISK /,            DOLSN  / 4H$    /,
     4       DRY    / 4HDRY  /,            DRYGO  / 4HDRYG /,
     5       ENDS   / 4HENDS /,            EQSN   / 4H=    /
      DATA   EXDEF  / -1,0,  4HTAPE,4H    ,-1,0   , 4HINTE,4H    , 0,0,
     1                4HNORE,4H    ,4HALL ,4H     , 4HWHOL,4HESOF,
     2                8*4HXXXX,-1,0,   -1 ,0               /
      DATA   IDG    / 2H0    /,            IDRY   / 2H-1   /,
     1       INPT   / 4HINPT /,            IOPEN  / 1      /,
     2       ISTP   / 2H1    /,            ITEM   / 4HITEM /
      DATA   ITMN   / 4HITM1,4HITM2,4HITM3,4HITM4,  4HITM5 /
      DATA   JNEW   / 4HNEW  /,            KBEG   / 4HBEGI /,
     1       KWD    / 4HK    /,            GORUN  / 4HGO   /,
     2       LPAR   / 4H(    /,            MACH   / 4HMACH /,
     3       MWD    / 4HM    /,            NAME   / 4HNAME /,
     4       NANO   / 4HNANO /,            NBREC  / 4HBREC /,
     5       NCASEC / 4HCASE,4HCC   /,     NCASES / 4HCASE,4HSS   /,
     6       NEW    / 4HNEW  /
      DATA   NHEAD  / 2*4H    ,4HN A ,4HS T ,4HR A ,4HN  S,4H U B,4H S T
     1              , 4H R U,4H C T,4H U R,4H E  ,4HD E ,4HC K ,4H E C,
     2                4H H O /
      DATA   NH1    / 4H1    /,            NH1A   / 4H1A   /,
     1       NPHASE / 3      /,            NPREC  / 4HPREC /,
     2       NREC   / 4HRECO /,            NSAVE  / 4HSAVE /,
     3       NSOL   / 4HSOL  /,            NSTP   / 4HNSTP /,
     4       POIT   / 4HPOIT /,            NXALT  / 4HXALT /,
     5       NXCSA  / 4HXCSA /,            NXL2   / 4HER   /,
     6       OPER   / 4HOPER /,            OPTI   / 4HOPTI /,
     7       PASS   / 4HPASS /,            PASS2  / .FALSE./
      DATA   PHS    / 4HE1  ,4HE2  , 4HE3         /
      DATA   POSI   / 4HPOSI /,            PTAPE  / 4HNPTP /,
     1       PWD    / 4HP    /,            RUN    / 4HRUN  /,
     2       SCRT   / 301    /,            SOF    / 4HSOF  /,
     3       STEP   / 4HSTEP /,            TITL   / 4HTITL /
      DATA   MSKP   / 4HMSKP /
      DATA   PAPP   / 4HPAPP /,            PAWD   / 4HPA   /,
     1       PITM   / 4HPITM /,            POAP   / 4HPOAP /,
     2       POVE   / 4HPOVE /,            PVEC   / 4HPVEC /
      DATA   BWD    / 4HB    /,            K4WD   / 4HK4   /
      DATA   OUTP   / 4HOUTP /,            RANG   / 4HRANG /
      DATA   DVEC   / 4HDVEC,4HUDVF, 4HUDVT       /
      DATA   NDBVAR / 6      /
      DATA   DBVAR  / 4HGORL,4HPVEC, 4HUVEC,4HPFTL, 4HOVEC,4HOVC2 /
      DATA   DBVAL  / 4HGEOM,4H4   , 4HPGG ,4H    , 4HUGV ,4H
     1              , 4H    ,4H    , 4HOUGV,4H1   , 4HOUGV,4H
     2              , 4HGEOM,4H4   , 4HPGG ,4H    , 4HUGV ,4H
     3              , 4H    ,4H    , 4HOUGV,4H1   , 4HOUGV,4H
     4              , 4HLAMA,4H    , 4H    ,4H    , 4HPHIG,4H
     5              , 4H    ,4H    , 4HOPHI,4HG1  , 4HOPHI,4HG
     6              , 4HGEOM,4H4   , 4HPPF ,4H    , 4HUGV ,4H
     7              , 4HPPF ,4H    , 4HOUGV,4H1   , 4HOUGV,4H
     8              , 4HGEOM,4H4   , 4HPPT ,4H    , 4HUGV ,4H
     9              , 4HTOL ,4H    , 4HOUGV,4H1   , 4HOUGV,4H     /
      DATA   NR3VAR / 5      /
      DATA   R3VAR  / 4HUAPH,4HPGVC, 4HPSVC,4HDYNT, 4HQVEC /
      DATA   R3VAL  / 4HULV ,4HPGS , 4HPSS ,4H    , 4HQG
     1              , 4HULV ,4HPGS , 4HPSS ,4H    , 4HQG
     2              , 4HPHIA,4H    , 4H    ,4HLAMA, 4HQG
     3              , 4HUDVF,4H    , 4H    ,4HPPF , 4HQPC
     4              , 4HUDVT,4HPPT , 4HPST ,4HTOL , 4HQP          /
      DATA   NCOM   / 25 /
      DATA   COMND  /
     1                4HSUBS ,1    , 4HRUN  ,2
     3              , 4HENDD ,2    , 4HCOMB ,3
     5              , 4HREDU ,4    , 4HSOLV ,5
     7              , 4HRECO ,6    , 4HMREC ,6
     9              , 4HBREC ,7    , 4HMRED ,9
     1              , 4HCRED ,13   , 4HDEST ,10
     3              , 4HEDIT ,10   , 4HEQUI ,10
     5              , 4HSOFP ,10   , 4HDELE ,10
     7              , 4HRENA ,10   , 4HSOFI ,11
     9              , 4HSOFO ,11   , 4HREST ,11
     1              , 4HDUMP ,11   , 4HCHEC ,11
     3              , 4HCOMP ,11   , 4HAPPE ,11
     5              , 4HPLOT ,12   /
      DATA   SUBNAM / 4HASDM,4HAP  /
C
C
      CALL CONMSG (ASD1,2,0)
      DO 10 I  = 64,68
   10 IOTIT(I) = BLANK
      DO 20 I  = 1,16
   20 IHEAD(I) = NHEAD(I)
      CALL PAGE
      NZ   = KORSZ(Z(1))
      BUF1 = NZ - IBUF + 1
      BUF2 = BUF1 - IBUF
      BUF3 = BUF2 - IBUF
C
C     INITIALIZE THE CASE CONTROL FILE
C
      CALL OPEN (*2620,CASE,Z(BUF2),1)
      CALL CLOSE (CASE,1)
      IOPEN = 1
      NOPEN = BUF3 - 1
      IF (NOPEN .LE. 100) CALL MESAGE (-8,100-NOPEN,SUBNAM)
      FIRST = .TRUE.
      SKIP  = .FALSE.
      ISOPT = 0
C
C     SET NUMBER OF POSSIBLE COMMANDS HERE
C
C     SET LAST WORD INDICATER
C
      I6777 = RSHIFT(COMPLF(0),1)
C
C     READ FIRST CARD AFTER CEND
C
      ASSIGN 70 TO IREAD
      GO TO 50
   30 IF (SKIP) GO TO 60
      IF (NLINES .GE. NLPP) CALL PAGE
      NLINES = NLINES + 1
      WRITE (OUTT,40) CARD
   40 FORMAT (1H ,4X,20A4)
   50 CALL XREAD (*2600,CARD)
      CALL XRCARD (OCARD,200,CARD)
      IF (OCARD(1).GT.0 .AND. OCARD(2).EQ.BLANK) GO TO 30
      IF (OCARD(1) .EQ. 0) GO TO 30
      IF (OCARD(2).EQ.TITL .OR. OCARD(2).EQ.KBEG) GO TO 2600
   60 SKIP = .FALSE.
      GO TO IREAD, (70,90,330,630)
C                      90?  NOT ASSIGNED BY ANYBODY    G.CHAN  4/93
C
   70 IF (OCARD(1).GT.0 .AND. OCARD(2).EQ.COMND(1,1)) GO TO 100
C
C     NO SUBSTRUCTURE CARD
C
      WRITE (OUTT,80) UFM
      NLINES = NLINES + 2
C
   80 FORMAT (A23,' 6001. SUBSTRUCTURE DATA IS REQUIRED WITH THIS ',
     1       'APPROACH')
      NOGO  = 1
   90 PHASE = 2
      ALTER = .FALSE.
      SKIP  = .TRUE.
      ICOM  = 1
      IF (OCARD(2) .EQ. ENDS) GO TO 2200
      GO TO 130
C
C     PROCESS SUBSTRUCTURE CARD
C
  100 CNAME = COMND(1,1)
      J = OCARD(1)*2
      DO 110 I = 1,NPHASE
      IF (OCARD(J+1) .NE. PHS(I)) GO TO 110
      PHASE = I
      ALTER = .TRUE.
      ICOM  = 1
      GO TO 130
  110 CONTINUE
C
C     NO PHASE IS DEFINED
C
      WRITE (OUTT,120) UWM
      NLINES = NLINES +2
  120 FORMAT (A25,' 6002, INCORRECT PHASE DATA')
      ALTER = .FALSE.
      NOGO  = 1
      ICOM  = 1
      PHASE = 2
C
C     FOUND PHASE. TURN BANDIT OFF IF PHASE IS 2
C
  130 IF (PHASE .EQ. 2) BANDIT = -1
      J    = 2
      IAPP = IABS(SYS(21))
      IF (IAPP .NE. 2) ALTER = .FALSE.
      IAP2 = SYS(69)/10
      IF (IAP2 .EQ. 1) ALTER = .FALSE.
      SOL  = 1
      IF (.NOT. ALTER) GO TO 200
      FILE = PTAPE
      KALT = 0
      KFILE= 0
      CALL OPEN (*2620,PTAPE,Z(BUF1),0)
  140 CALL SKPFIL (PTAPE,1)
      KFILE = KFILE + 1
      CALL READ (*2620,*150,PTAPE,FNAME,7,1,NWORDS)
  150 CONTINUE
      IF (FNAME(1) .NE.NXALT) GO TO 160
      KALT = KFILE
C
      GO TO 140
  160 IF (FNAME(1) .NE. NXCSA) GO TO 140
      ALTFL = .FALSE.
      SOL = 1
      IF (IAPP .EQ. 3) GO TO 180
      CALL READ (*2620,*180,PTAPE,II,6,0,NWDS)
      SOL = II(5)
  180 CALL REWIND (PTAPE)
      IF (KALT .NE. 0) ALTFL = .TRUE.
      IF (ALTFL) GO TO 190
      CALL SKPFIL (PTAPE,KFILE)
      GO TO 200
  190 CALL SKPFIL (PTAPE,KALT)
      CALL FWDREC (*2620,PTAPE)
      CALL READ (*2620,*200,PTAPE,ALTS,2,1,NWDS)
C
C     NO XALTER FILE
C
C     OPEN CASE FILE FOR SUBSTRUCTURE DATA OR TITLE
C
  200 IF (PHASE .EQ. 3) GO TO 300
      FILE = CASE
      CALL OPEN (*2620,CASE,Z(BUF2),1)
      CALL WRITE (CASE,NCASES,2,1)
      FILE = SCRT
C
C     SET UP INITAL VALUES
C
  300 IAC   = 0
      ISTEP = 0
      NDBS  = 0
      DRYFLG= 1
      OBITS = 55
      IF (SOL .EQ. 1) OBITS = 5
      IF (SOL .EQ. 2) OBITS = 7
      IF (SOL .EQ. 3) OBITS = 3
      IF (SOL .EQ. 8) OBITS = 55
      IF (SOL .EQ. 9) OBITS = 55
      NEWBT = OBITS
      RECOV = .FALSE.
      SOLVE = .FALSE.
      IAPP  =  SYS(21)
      IF (IAPP .EQ. 3) ALTER = .FALSE.
      IF (.NOT.ALTER ) GO TO 310
      CALL OPEN (*2620,SCRT,Z(BUF3),1)
      II(1) = NXALT
      II(2) = NXL2
      CALL WRITE (SCRT,II,2,1)
  310 CONTINUE
      NSOF = 0
      ISOF = 1
      NNAME(1) = INPT
      STAT = 1
      LENGTH(1)= 100
      PASWD(1) = BLANK
      PASWD(2) = BLANK
C
C     READ PASSWORD AND SOF DECLARATIONS
C
      INEX = 0
  320 ASSIGN 330 TO IREAD
      GO TO 30
  330 IF (OCARD(2) .NE. PASS) GO TO 340
      K = 4
      IF (OCARD(5) .EQ. EQSN) K = 6
      PASWD(1) = OCARD(K)
      PASWD(2) = OCARD(K+1)
      GO TO 320
  340 IF (OCARD(2) .NE. SOF) GO TO 380
      K = 4
      IF (OCARD(5) .NE. LPAR) GO TO 350
      K = 9
      ISOF = OCARD(7)
  350 IF (ISOF.LT.0 .OR. ISOF.GT.10) GO TO 370
      NSOF = NSOF + 1
      IF (OCARD(K+1) .EQ. EQSN) K = K + 2
      IF (OCARD(K+4).EQ.JNEW .OR. OCARD(K+5).EQ.JNEW) STAT = 0
      NNAME (ISOF) = OCARD(K  )
      LENGTH(ISOF) = OCARD(K+3)
      IF (OCARD(K+2) .EQ. -1) GO TO 320
      LENGTH(ISOF) = 100
      IF (NLINES+3 .GT. NLPP) CALL PAGE
      NLINES = NLINES + 3
      IF (.NOT.SKIP) WRITE (OUTT,40) CARD
      WRITE  (OUTT,360) UWM,ISOF
  360 FORMAT (A25,', SOF(',I2,') FILESIZE NOT SPECIFIED. DEFAULT OF ',
     1       '100K WORDS WILL BE ALLOCATED',/)
      ASSIGN 330 TO IREAD
      IF (SKIP) GO TO 60
      GO TO 50
  370 WRITE (OUTT,790) UFM
      NLINES = NLINES + 1
      NOGO = 1
      GO TO 320
  380 IF (INEX .EQ. 1) GO TO 640
      INEX = 1
      SKIP = .TRUE.
      ICNEXT = 1
C
C     START PROCESSING SUBSTRUCTURE COMMAND CARDS HERE
C     TOP OF COMMAND LOOP
C
  400 ICOM = ICNEXT
      IF ( OCARD(2) .EQ. ENDS) GO TO 2100
      DO 410 L = 1,30
  410 CDATA(L) = OCARD(L)
  420 CNAME = COMND(1,ICOM)
      JCOM  = COMND(2,ICOM)
      IF (ICOM.EQ.6 .AND. SOL.GT.3) JCOM = 8
      REJECT = .FALSE.
      GO TO (430,440,450,460,470,480,490,500,510,520,530,540,550), JCOM
  430 CALL ASCM01 (CNAME,PHASE,SOL,NOGO)
      GO TO 600
  440 CALL ASCM02 (CNAME,PHASE,SOL,NOGO)
      GO TO 600
  450 CALL ASCM03 (CNAME,PHASE,SOL,NOGO)
      GO TO 600
  460 CALL ASCM04 (CNAME,PHASE,SOL,NOGO)
      GO TO 600
  470 CALL ASCM05 (CNAME,PHASE,SOL,NOGO)
      GO TO 600
  480 CALL ASCM06 (CNAME,PHASE,SOL,NOGO)
      GO TO 600
  490 CALL ASCM07 (CNAME,PHASE,SOL,NOGO)
      GO TO 600
  500 CALL ASCM08 (CNAME,PHASE,SOL,NOGO)
      GO TO 600
  510 CALL ASCM09 (CNAME,PHASE,SOL,NOGO)
      GO TO 600
  520 CALL ASCM10 (CNAME,PHASE,SOL,NOGO)
      GO TO 600
  530 CALL ASCM11 (CNAME,PHASE,SOL,NOGO)
      GO TO 600
  540 CALL ASCM12 (CNAME,PHASE,SOL,NOGO)
      GO TO 600
  550 CALL ASCM13 (CNAME,PHASE,SOL,NOGO)
  600 JX = 0
      ISTEP = ISTEP + 1
C
C     TRANSFER RAW DMAP TO WORKING AREA
C
      M = IRDM - 1
      DO 610 J = 1,NRDM
      DO 610 I = 1,18
      M = M + 1
      DMAP(I,J) = IDAT(M)
  610 CONTINUE
C
C     READ IN EXTRAS, FIND IN OPTION LIST, STOP AT NEXT COMMAND
C
  620 ASSIGN 630 TO IREAD
      GO TO 30
  630 IF (OCARD(2).EQ.PASS .OR. OCARD(2).EQ.SOF) GO TO 330
  640 IF (REJECT) GO TO 2090
      IF (ITEMP(2) .EQ. DOLSN) GO TO 620
      IF (ITEMP(2) .EQ.  ENDS) GO TO 810
      IF (ITEMP(2) .NE.  OPTI) GO TO 660
      NEWBT = 0
      I2 = 4
      IF (ITEMP(5) .EQ. EQSN) I2 = 6
      DO 650 I = 1,6
      J = 2*I + I2 - 2
      IF (ITEMP(J) .EQ. KWD) NEWBT = ORF(NEWBT,1)
      IF (ITEMP(J) .EQ. MWD) NEWBT = ORF(NEWBT,2)
      IF (ITEMP(J) .EQ. PWD) NEWBT = ORF(NEWBT,4)
      IF (ITEMP(J) .EQ.PAWD) NEWBT = ORF(NEWBT,8)
      IF (ITEMP(J) .EQ. BWD) NEWBT = ORF(NEWBT,16)
      IF (ITEMP(J) .EQ.K4WD) NEWBT = ORF(NEWBT,32)
  650 CONTINUE
      IF (ANDF(NEWBT,12) .EQ. 12) GO TO 780
      IF (ISTEP .LE. 1) OBITS = NEWBT
      GO TO 620
  660 CONTINUE
      IF (NXTRA .EQ. 0) GO TO 760
      M = IXTRA - 1
      DO 670 I = 1,NXTRA
      M = M + 1
      IF (ITEMP(2) .EQ. IDAT(M)) GO TO 680
  670 CONTINUE
C
C     CARD IS NOT AN EXTRA
C
      GO TO 760
C
C     FOUND AN EXTRA, STORE SEQUENTIALLY AS PAIRS OF TWO WORD ITEMS
C
  680 JX = JX + 1
      EXTRA(1,JX) = ITEMP(2)
      I2 = 4
      IF (ITEMP(5) .EQ. EQSN) I2 = 6
      EXTRA(2,JX) = ITEMP(I2  )
      EXTRA(3,JX) = ITEMP(I2+1)
C
C     SPECIAL OUTPUT EXTRA
C
      IF (ITEMP(2) .NE. OUTP) GO TO 700
      EXTRA(2,JX) = -1
      EXTRA(3,JX) = 0
  690 IF (ITEMP(I2).NE.-1 .OR. ITEMP(I2+1).LE.0 .OR. ITEMP(I2+1).GT.31)
     1    GO TO 620
      J = LSHIFT(1,ITEMP(I2+1)-1)
      EXTRA(3,JX) = ORF(EXTRA(3,JX),J)
      I2 = I2 + 2
      GO TO 690
  700 CONTINUE
      IF (ITEMP(2) .NE. RANG) GO TO 720
      JX = JX + 1
      EXTRA(1,JX) = RANG
      I2 = I2 + 2
      IF (ITEMP(I2).EQ.-1 .OR. ITEMP(I2).EQ.-2) GO TO 710
      EXTRA(2,JX) = EXTRA(2,JX-1)
      EXTRA(3,JX) = EXTRA(3,JX-1)
      EXTRA(3,JX-1) = 0
      GO TO 620
  710 EXTRA(2,JX) = ITEMP(I2)
      EXTRA(3,JX) = ITEMP(I2+1)
      GO TO 620
  720 CONTINUE
      IF (EXTRA(1,JX) .NE. RUN) GO TO 620
      EXTRA(3,JX) = BLANK
      EXTRA(2,JX) = IDRY
      IF (ITEMP(I2) .EQ.   DRY) GO TO 620
      IF (ITEMP(I2) .NE. GORUN) GO TO 730
      EXTRA(2,JX) = IDG
      GO TO 620
  730 IF (ITEMP(I2) .NE. STEP) GO TO 740
      EXTRA(2,JX) = ISTP
      GO TO 620
  740 IF (ITEMP(I2) .NE. DRYGO) GO TO 750
      DRYFLG = 0
      GO TO 620
  750 JX = JX - 1
      GO TO 620
C
C      CHECK AND SET IF COMMAND CARD
C
  760 DO 770 I = 2,NCOM
      ICNEXT = I
      IF (OCARD(2) .EQ. COMND(1,I)) GO TO 800
  770 CONTINUE
  780 WRITE (OUTT,790) UFM
      NLINES = NLINES +2
  790 FORMAT (A23,' 6003. ILLEGAL COMMANDS OR OPTIONS DEFINED ON NEXT ',
     1       'CARD')
      NOGO = 1
      GO TO 620
C
C     FOR PHASE 3 RECOVERY, CHANGE RECO TO BREC
C
  800 IF (PHASE.NE.3 .OR. COMND(1,ICNEXT).NE.NREC) GO TO 810
      OCARD(2) = NBREC
      ICNEXT = 9
  810 CONTINUE
C
      GO TO ( 820,1030,2200,1100,1200,1300,1400,1400,1700,1200,
     1       1200,1500,1500,1500,1500,1500,1500,1730,1730,1730,
     2       1730,1730,1730,1730,1900), ICOM
C
C     SUBSTRUCTURE   PHASES
C         PHASE 1
C      VARIABLES,     NO.     TYPE      POSITION      DEFINITION
C                     1,2,3    I           1    ALTE,R.F. REMOVE NUMBERS
C                     4,5,6    I           4    ALTE,R.F. REMOVE NUMBERS
C                     7,8,9                     ALTE,R.F. REMOVE NUMBERS
C                     10,11,12                  SAVE,-1, PLOT SET ID
C                 1   13,14,15                  RUN ,-1, RUN  FLAG
C                 1   16,17,18                  NAME, SUBS NAME
C
  820 GO TO (830,900,1000), PHASE
  830 NVAR = 24
      NOUT = 0
      DO 840 I = 1,NVAR
  840 VAR(I,1) = 0
      VAR(1,8) = PITM
      VAR(2,8) = PVEC
      VAR(3,8) = BLANK
      IF (ANDF(OBITS,8) .NE. 0) VAR(2,8) = PAPP
      DO 850 I = 1,JX
      DO 850 J = 1,3
      VAR(J,I+4) = EXTRA(J,I)
  850 CONTINUE
      NX   = JX + 4
      INAM = 0
      IRUN = 0
      ISAV = 0
C
C     CHECK FOR REQUIRED NAME
C
      DO 860 I = 5,NX
      IF (VAR(1,I) .EQ.  NAME) INAM = I
      IF (VAR(1,I) .EQ.   RUN) IRUN = I
      IF (VAR(1,I) .EQ. NSAVE) ISAV = I
  860 CONTINUE
C
C     NO NAME DEFINED IS A LEVEL 3 ERROR
C
      IF (INAM .LE. 0) GO TO 2640
      IF (IRUN .NE. 0) GO TO 870
      IRUN = NX + 1
      VAR(1,IRUN) =  RUN
      VAR(2,IRUN) = ISTP
      VAR(3,IRUN) = BLANK
      NX = NX + 1
  870 CONTINUE
      IF (ISAV .NE. 0) GO TO 880
      VAR(1,NX+1) = NSAVE
      VAR(2,NX+1) = -1
      VAR(3,NX+1) = 0
  880 CONTINUE
      M = IPH - 1
      DO 890 I = 1,4
      M = M + 2
      VAR(1,I) = ALT1
      VAR(2,I) = IDAT(M-1)
      VAR(3,I) = IDAT(M  )
  890 CONTINUE
      GO TO 2000
C
C     PHASE 2 PROCESS
C
  900 IF (JX .GT. 0) GO TO 910
      JX = 1
      EXTRA(1,1) = RUN
      EXTRA(2,1) = ISTP
      EXTRA(3,1) = BLANK
  910 VAR(1,1) = ALT1
      VAR(2,1) = 4
      IF (SOL .EQ. 1) VAR(2,1) = 5
      VAR(3,1) = 0
C
      DO 920 J = 1,JX
      DO 920 I = 1,3
      VAR(I,J+1) = EXTRA(I,J)
  920 CONTINUE
      NVAR = 3*(1+JX)
      NOUT = 0
      DO 930 I = 1,5
      DMAP(1,I) = -1
  930 CONTINUE
      GO TO 2000
C
C     PHASE 3 PROCESSING
C     NORMALLY THIS IS A RESTART, IF NOT THE DATA WILL BE REGENERATED
C
 1000 NVAR = 6
      VAR(1,1) = ALT1
      VAR(2,1) = IDAT(IPH)
      VAR(3,1) = IDAT(IPH+1)
      VAR(1,2) = RUN
      VAR(2,2) = ISTP
      VAR(3,2) = BLANK
      IF (JX .LT. 1) GO TO 1010
      IF (EXTRA(1,1) .EQ. RUN) VAR(2,2) = EXTRA(2,1)
 1010 NOUT = 0
      DO 1020 I = 1,5
      DMAP(1,I) = -1
 1020 CONTINUE
      GO TO 2000
C
C     RUN COMMAND (SOMETIMES AN EXTRA)
C
 1030 I2 = 4
      IF (CDATA(5) .EQ. EQSN) I2 = 6
      VAR(1,1) = CDATA(2)
      VAR(2,1) = ISTP
      VAR(3,1) = BLANK
      IF (CDATA(I2) .EQ. STEP) GO TO 1040
      VAR(2,1) = IDRY
      IF (CDATA(I2) .EQ. DRYGO) DRYFLG = 0
 1040 IF (DRYFLG .EQ. 0) GO TO 2080
      NVAR = 3
      NOUT = 0
      GO TO 2000
C
C     COMBINE OPERATION, USES SUBROUTINE COMBO
C
 1100 CALL COMBO (CDATA,JX,EXTRA,IAC,NASUB,NS,VAR(1,3),IER)
      NVAR = 3*(5+JX+3*NS)
      VAR(1,1) = NS
      VAR(2,1) = 0
      VAR(3,1) = 0
      VAR(1,2) = NSTP
      VAR(2,2) =-1
      VAR(3,2) = ISTEP
      NVAR = NVAR + 3
      VAR(NVAR+1,1) = PITM
      VAR(NVAR+2,1) = PVEC
      VAR(NVAR+3,1) = BLANK
      IF (ANDF(OBITS,8) .NE. 0) VAR(NVAR+2,1) = PAPP
      NVAR = NVAR + 3
      NOUT = NVAR
      IF (IER) 2640,2000,2640
C
C     REDUCE, MREDUCE, CREDUCE OPERATIONS - VARIABLES TO BE SET ARE
C
C                STEP - STEP NO.
C                NONA - NO. OF SUBSTRUCTURE A
C                NONB - NO. OF SUBSTRUCTURE B
C                NAMA - NAME OF SUBSTRUCTURE A
C                NAMB - NAME OF SUBSTRUCTURE B
C                PREC - PRECISION FLAG
C                PITM - LOAD ITEM
C                POIT - LOAD TRANSFORMATION ITEM
C
 1200 CALL REDU (CDATA,JX,EXTRA,IAC,NASUB,NVAR,VAR(1,2),IPREC,IER)
      VAR(1,1) = STEP
      VAR(2,1) = -1
      VAR(3,1) = ISTEP
      NVAR     = NVAR+3
      VAR(NVAR+1,1) = PITM
      VAR(NVAR+2,1) = PVEC
      VAR(NVAR+3,1) = BLANK
      VAR(NVAR+4,1) = POIT
      VAR(NVAR+5,1) = POVE
      VAR(NVAR+6,1) = BLANK
      IF (ANDF(OBITS,8) .EQ. 0) GO TO 1210
      VAR(NVAR+2,1) = PAPP
      VAR(NVAR+5,1) = POAP
 1210 NVAR = NVAR + 6
      NOUT = NVAR
      IF (IER) 2090,2000,2090
C
C     SOLVE OPERATION - VARIABLES ARE SUBSTRUCTURE NAME AND ALTER NO S
C
 1300 NVAR = 33
      NOUT = NVAR
      I2   = 4
      IF (CDATA(5) .EQ. EQSN) I2 = 6
      IF (CDATA(1)*2 .LT. I2) GO TO 2660
      VAR(1,8) = NAME
      VAR(2,8) = CDATA(I2  )
      VAR(3,8) = CDATA(I2+1)
      NSOLV1   = CDATA(I2  )
      NSOLV2   = CDATA(I2+1)
C
C     FIND STRUCTURE NUMBER
C
      NS = IAC
      IF (NS .EQ. 0) GO TO 1320
      DO 1310 I = 1,NS
      IF (CDATA(I2).EQ.NASUB(1,I) .AND. CDATA(I2+1).EQ.NASUB(2,I))
     1    GO TO 1330
 1310 CONTINUE
 1320 CONTINUE
      NS = NS+1
      NASUB(1,NS) = CDATA(I2  )
      NASUB(2,NS) = CDATA(I2+1)
      I = NS
 1330 VAR(1, 9) = NANO
      VAR(2, 9) = -1
      VAR(3, 9) = I
      VAR(1,10) = STEP
      VAR(2,10) = -1
      VAR(3,10) = ISTEP
      IF (JCOM .EQ. 8) GO TO 1340
      VAR(1,11) = NSOL
      VAR(2,11) = BLANK
      VAR(3,11) = BLANK
      GO TO 1350
 1340 VAR(1,11) = DVEC(1)
      VAR(2,11) = DVEC(2)
      VAR(3,11) = BLANK
      IF (SOL .EQ. 9) VAR(2,11) = DVEC(3)
 1350 CONTINUE
      IF (SOL .EQ. 1) GO TO 1360
      IF (SOL .EQ. 2) VAR(2,11) = NH1A
      IF (SOL .EQ. 3) VAR(2,11) = NH1
      NVAR = 36
      NOUT = 36
      VAR(1,12) = MSKP
      VAR(2,12) = BLANK
      VAR(3,12) = BLANK
 1360 CONTINUE
      IAC = NS
      M   = IPH - 1
      DO 1370 I = 1,7
      M = M + 2
      VAR(1,I) = ALT1
      VAR(2,I) = IDAT(M-1)
      VAR(3,I) = IDAT(M  )
 1370 CONTINUE
      SOLVE = .TRUE.
      GO TO 2000
C
C     RECOVERY PHASE2 - VARIABLES ARE SOLUTION STRUCTURE NAME,
C                       PRINT, NAME AND/OR SAVE, NAME+ALTER
C
 1400 I2 = 4
      IF (CDATA(5) .EQ. EQSN) I2 = 6
      IF (CDATA(1)*2 .LT. I2) GO TO 2660
      VAR(1,1) = NCASES(1)
      VAR(2,1) = NCASES(2)
      VAR(3,1) = BLANK
      VAR(1,2) = NAME
      VAR(2,2) = CDATA(I2  )
      VAR(3,2) = CDATA(I2+1)
      ISOL = SOL
      IF (SOL  .GT. 3) ISOL = ISOL - 4
      IF (ICOM .EQ. 8) ISOL = 3
      DO 1410 I = 1,NDBVAR
      VAR(1,I+2) = DBVAR(I)
      VAR(2,I+2) = DBVAL(1,I,ISOL)
      IF (ICOM.EQ.8 .AND. I.LT.4) VAR(2,I+2) = BLANK
      VAR(3,I+2) = DBVAL(2,I,ISOL)
      IF (ICOM.EQ.8 .AND. I.LT.4) VAR(3,I+2) = BLANK
 1410 CONTINUE
      VAR(1,9) = NSOL
      VAR(2,9) = -1
      VAR(3,9) = SOL
      IF (ICOM .EQ. 8) VAR(3,9) = 3
      VAR(1,10) = STEP
      VAR(2,10) = -1
      VAR(3,10) = ISTEP
      IF (JX .LE. 0) GO TO 1430
      DO 1420 I = 1,JX
      DO 1420 K = 1,3
      VAR(K,I+10) = EXTRA(K,I)
 1420 CONTINUE
 1430 IF (SOLVE) GO TO 1440
C
C     SAVE OPTION BITS AND SET TO ZERO
C
      OBITS   = 0
      VAR(1,4)= 0
      VAR(2,1)= NCASEC(2)
 1440 RECOV   = .TRUE.
      NVAR    = 3*JX + 30
      NOUT    = NVAR
      GO TO 2000
C
C     UTILITY COMMANDS - USE SOFOUT MODULE TO MANIPULATE SOF FILE(S).
C     DESTROY, EDITOUT, EQUIV, PRINT, DELETE, AND RENAME
C
 1500 NVAR = 0
      I2   = 4
      KWDS = 1
C
C     DECODE AND STORE COMMAND DATA FROM HEADER CARD
C
 1510 KWDS = KWDS + 1
      IF (CDATA(I2+1).EQ.LPAR .OR. CDATA(I2+1).EQ.EQSN) I2 = I2 + 2
      VAR(2,KWDS) = CDATA(I2  )
      VAR(3,KWDS) = CDATA(I2+1)
      I2 = I2 + 2
      IF (CDATA(I2).EQ.I6777 .OR. CDATA(I2+1).EQ.I6777) GO TO 1520
      IF (VAR(2,KWDS) .EQ. -1) I2 = I2 + 1
      IF (KWDS .LT. 8) GO TO 1510
C
C     INSERT VARIABLE NAMES
C
 1520 J = ICOM - 11
      VAR(1,1) = OPER
      VAR(2,1) = CNAME
      VAR(3,1) = BLANK
      JOPT = 0
      GO TO (1530,1540,1550,1580,1590,1550), J
C
C      DESTROY NAME
C
 1530 VAR(1,2) = NAME
      NVAR = 2
      GO TO 1620
C
C     EDITOUT(CODE) = NAME
C
 1540 GO TO 1580
C
C     EQUIV A,B   +PREFIX = B CARD
C
 1550 VAR(1,2) = NAME
      VAR(1,3) = NEW
      NVAR = 3
      IF (J    .EQ. 6) GO TO 1620
      IF (KWDS .LT. 2) GO TO 2640
      IF (JX   .LT. 1) GO TO 1560
      VAR(1,4) = EXTRA(1,1)
      VAR(2,4) = EXTRA(2,1)
      VAR(3,4) = EXTRA(3,1)
      NVAR = 4
      GO TO 1620
 1560 WRITE  (OUTT,1570) UWM
 1570 FORMAT (A25,' 6004, NO PREFIX DEFINED AFTER EQUIVALENCE.')
      NLINES = NLINES + 2
      GO TO 1620
C
C     PRINT(CODE) = NAME,ITM1,ITM2,ITM3,ITM4,ITM5
C
 1580 IF (VAR(2,2).NE. -1) GO TO 1590
      VAR(1,2) = OPTI
      JOPT = 1
      I2   = 3
      GO TO 1600
 1590 I2   = 2
 1600 VAR(1,I2) = NAME
      NS = KWDS - I2
      DO 1610 I = 1,NS
      J  = I2 + I
 1610 VAR(1,J) = ITMN(I)
      NVAR = KWDS
 1620 NOUT = 0
      IF (JOPT .EQ. 1) GO TO 1630
      NVAR = NVAR + 1
      VAR(1,NVAR) = OPTI
      VAR(2,NVAR) = -1
      VAR(3,NVAR) = 32
      IF (ICOM .EQ. 15) VAR(3,NVAR) = 0
 1630 NVAR = 3*NVAR
      GO TO 2000
C
C     RECOVERY, PHASE 3.  VARIABLES ARE NAME, SOL, STEP, PREC, UAPH,
C     PGVC, PSVC, DYNT, QVEC
C
 1700 I2 = 4
      IF (CDATA(I2) .EQ. EQSN) I2 = I2 + 2
      M = IPH - 1
      DO 1710 I = 1,3
      M = M + 2
      VAR(1,I) = ALT1
      VAR(2,I) = IDAT(M-1)
      VAR(3,I) = IDAT(M  )
 1710 CONTINUE
      ISOL = SOL
      IF (SOL .GT. 3) ISOL = ISOL - 4
      DO 1720 I = 1,NR3VAR
      VAR(1,I+3) = R3VAR(I)
      VAR(2,I+3) = R3VAL(I,ISOL)
      VAR(3,I+3) = BLANK
 1720 CONTINUE
      VAR(1, 9) = NAME
      VAR(2, 9) = CDATA(I2  )
      VAR(3, 9) = CDATA(I2+1)
      VAR(1,10) = NSOL
      VAR(2,10) = -1
      VAR(3,10) = SOL
      VAR(1,11) = STEP
      VAR(2,11) = -1
      VAR(3,11) = ISTEP
      VAR(1,12) = NPREC
      VAR(2,12) = -1
      VAR(3,12) = IPREC
      NVAR      = 36
      NOUT      = 0
      GO TO 2000
C
C     EXIO OPERATIONS -
C     SOFIN, SOFOUT, RESTORE, DUMP, CHECK, COMPRESS AND APPEND
C
 1730 NVAR = 42
      NOUT = 0
C
      DO 1740 I = 1,14
      VAR(1,I) = 100 + I
      VAR(2,I) = EXDEF(1,I)
 1740 VAR(3,I) = EXDEF(2,I)
C
C     DECODE COMMAND CARD
C
      VAR(2,5) = CDATA(2)
      VAR(3,5) = CDATA(3)
      I2 = 4
      IF (CDATA(5) .NE. LPAR) GO TO 1750
      VAR(2,4) = CDATA(6)
      VAR(3,4) = CDATA(7)
      I2 = 8
 1750 IF (CDATA(I2+1) .EQ. EQSN) I2 = I2 + 2
      IF (CDATA(I2).EQ.I6777 .OR. CDATA(I2+1).EQ.LPAR) GO TO 1830
      VAR(2,3) = CDATA(I2  )
      VAR(3,3) = CDATA(I2+1)
      IF (CDATA(I2+2) .EQ. I6777) GO TO 1760
      VAR(2,2) = CDATA(I2+2)
      VAR(3,2) = CDATA(I2+3)
C
C     SET EXTRAS
C
 1760 NN = 0
      IF (JX .EQ.0) GO TO 1820
      DO 1810 I = 1,JX
      IF (EXTRA(1,I) .NE. MACH) GO TO 1770
      K = 1
      GO TO 1800
 1770 IF (EXTRA(1,I) .NE. POSI) GO TO 1780
      K = 6
      GO TO 1800
 1780 IF (EXTRA(1,I) .NE. ITEM) GO TO 1790
      K = 7
      GO TO 1800
 1790 IF (EXTRA(1,I) .NE. NAME) GO TO 1810
      NN = NN + 1
      K  = NN + 7
 1800 VAR(2,K) = EXTRA(2,I)
      VAR(3,K) = EXTRA(3,I)
 1810 CONTINUE
C
C     SET DISK FIELD FOR COMPRESS ETC
C
 1820 IF (ICOM .GE. 24) VAR(2,2) = DISK
      GO TO 2000
 1830 WRITE  (OUTT,1840) UFM
 1840 FORMAT (A23,' 6008, ILLEGAL INPUT ON THE PREVIOUS COMMAND.', /5X,
     1       'MISSING FILE NAME FOR IO OPERATION')
      NLINES = NLINES + 3
      GO TO 2090
C
C     PLOT COMMAND
C          FORMAT
C     PLOT NAME
C
 1900 NVAR = 6
      NOUT = 0
      I2   = 4
      IF (CDATA(I2) .EQ. EQSN) I2 = 6
      IF (CDATA(1)*2 .LT.  I2) GO TO 2640
      VAR(1,1) = NAME
      VAR(2,1) = CDATA(I2  )
      VAR(3,1) = CDATA(I2+1)
      VAR(1,2) = STEP
      VAR(2,2) = -1
      VAR(3,2) = ISTEP
C
C     PROCESS VARIABLE CHARACTERS IF DMAP IS TO BE GENERATED
C
 2000 IF (.NOT.ALTER) GO TO 2080
      CALL ASPRO (DMAP,IVAR,NVAR,OBITS,SOL)
C
C     RESET OPTION BITS IF DUMMY VALUE WAS USED
C
      OBITS = NEWBT
      IF (NOGO .GE. 1) GO TO 2080
C
C     WRITE  DMAP ON  SCRATCH FILE
C
      DO 2070 I = 1,NRDM
C
C     GO TO SPECIAL CODE IF AN ALTER CARD
C
      IF (DMAP(1,I) .NE. ALT1) GO TO 2060
C
      II(1) = DMAP(2,I)
      II(2) = DMAP(3,I)
      IF (.NOT.ALTFL) GO TO 2050
      IF (II(2)  .EQ. 0) II(2)  = -II(1)
 2010 IF (ALTS(2).EQ. 0) ALTS(2)= -ALTS(1)
C
      IF (ALTS(1) .GT. IABS(II(2))) GO TO 2050
C
C     OVERLAPPING DMAP
C
      IF (IABS(ALTS(2)) .GE. II(1)) GO TO 2660
C
C     ALTERS ENCOUNTERED BEFORE NEW ALTERS
C
      IF (ALTS(2) .LT. 0) ALTS(2) = 0
      CALL WRITE (SCRT,ALTS,2,1)
      FILE = PTAPE
 2020 CALL READ (*2040,*2030,PTAPE,ALTS,18,1,NWDS)
C
C     DMAP DATA ENCOUNTERED
C
      CALL WRITE (SCRT,ALTS,18,1)
      GO TO 2020
C
C     MORE ALTERS ENCOUNTERED
C
 2030 IF (NWDS .EQ. 2) GO TO 2010
C
C     END OF USER ALTERS
C
 2040 ALTFL = .FALSE.
C
C     INSERT NEW DMAP ALTERS
C
 2050 IF (II(2) .LT. 0) II(2) = 0
C
      CALL WRITE (SCRT,II,2,1)
      GO TO 2070
C
C     WRITE ORDINARY DMAP DATA HERE
C
 2060 CALL WRITE (SCRT,DMAP(1,I),18,1)
 2070 CONTINUE
C
C     WRITE COMMAND AND VARIABLE DATA ON CASE CONTROL FILE
C
 2080 IF (PHASE .EQ. 3) GO TO 2090
      II(1) = CNAME
      II(2) = NOUT
      CALL WRITE (CASE,II,2,0)
      CALL WRITE (CASE,IVAR,NOUT,1)
C
C
 2090 IF (ITEMP(2) .EQ. ENDS) GO TO 2100
C
      REJECT = .FALSE.
C
      GO TO 400
C
C     ENDSUBS ENCOUNTERED,  STOP PROCESS
C     ENSURE THAT A RECOVER ALWAYS EXISTS FOLLOWING A SOLVE
C
 2100 IF (PHASE.NE.2 .OR. .NOT.SOLVE .OR. RECOV) GO TO 2110
C
C     CONSTRUCT A DUMMY INPUT CARD
C
      CDATA(1) = 4
      CDATA(2) = NREC
      CDATA(3) = BLANK
      CDATA(4) = NSOLV1
      CDATA(5) = NSOLV2
      SKIP = .TRUE.
      ICOM = 7
      GO TO 420
C
C     CHECK SOF AND PASSWORD DECLARATIONS
C
 2110 IF (PASWD(1) .NE. BLANK) IF (NSOF) 2120,2120,2140
 2120 CALL PAGE2 (2)
      WRITE  (OUTT,2130) UFM
 2130 FORMAT (A23,' 6011, SOF DATA PASSWORD MISSING')
      NOGO = 1
 2140 CONTINUE
      IBLKSZ = IBUF - 4
      IF (MCHN.EQ.3 .OR. MCHN.EQ.4) IBLKSZ = IGINOB
      FACT = 1000.0/IBLKSZ
      DO 2150 I = 1,10
      LENGTH(I) = LENGTH(I)*FACT
      JX = LENGTH(I)/2
 2150 LENGTH(I) = 2*JX
C
C     INITIALIZE DIRECT ACCESS FILES FOR IBM 360/370 MACHINES
C
      IF (MCHN .EQ. 2) CALL SOFIOI
 2200 CALL PAGE2 (1)
      WRITE  (OUTT,2210)
 2210 FORMAT (7X,7HENDSUBS)
      IF (.NOT. ALTER) GO TO 2540
C
C     WRAP UP DMAP
C     PUT LABEL ON END OF ALTER DECK
C
      CNAME = COMND(1,3)
      CALL ASCM02 (CNAME,PHASE,SOL,NOGO)
      M = IRDM + 18
      CALL WRITE (SCRT,IDAT(M),18,1)
C
C     REPEAT ALTER IF DRYGO IS ON
C
      IF (DRYFLG .NE. 0) GO TO 2230
      DO 2220 I = 1,3
      M = M + 18
      CALL WRITE (SCRT,IDAT(M),18,1)
 2220 CONTINUE
C
C     JUMP TO FINISH OF RIGID FORMAT
C
 2230 IF (PHASE .NE. 3) CALL WRITE (SCRT,IDAT(91),18,1)
      IFILE = PTAPE
      OFILE = SCRT
      IFIN  = .FALSE.
      PASS2 = .FALSE.
      IF (.NOT.ALTFL) GO TO 2240
      IF (ALTS(2) .LT. 0) ALTS(2)=0
      CALL WRITE (SCRT,ALTS,2,1)
      GO TO 2270
 2240 CALL EOF (SCRT)
      CALL READ (*2620,*2250,PTAPE,II,9,1,NW)
C
C     COPY REMAINDER OF PROBLEM TAPE TO SCRATCH FILE
C
 2250 IFIN = .FALSE.
 2260 IF (II(1) .EQ. NXCSA) IFIN = .TRUE.
      CALL WRITE (OFILE,II,NW,1)
C
C
      IREC = 1
 2270 CALL READ (*2290,*2280,IFILE,Z(IOPEN),NOPEN,0,NWDS)
C
      CALL WRITE (OFILE,Z(IOPEN),NOPEN,0)
      GO TO 2270
C
C     SET ALTER FLAG ON SOL RECORD OF XCSA FILE
C
 2280 IF (IFIN .AND. PASS2 .AND. IREC.EQ.1) Z(IOPEN+2) = 1
      CALL WRITE (OFILE,Z(IOPEN),NWDS,1)
      IREC = IREC + 1
      GO TO 2270
 2290 CONTINUE
      CALL EOF (OFILE)
      IF (IFIN) GO TO 2300
      CALL READ (*2620,*2260,IFILE,II,9,1,NW)
      GO TO 2260
 2300 CALL CLOSE (IFILE,1)
      CALL CLOSE (OFILE,3)
      IF (PASS2) GO TO 2530
C
C     PRINT OR PUNCH ALTER DECK HERE
C
C     DIAG 23 REQUESTS PRINT
C     DIAG 24 REQUESTS PUNCH
C
      CALL SSWTCH (23,KPRT)
      CALL SSWTCH (24,KPCH)
      IF (KPRT.EQ.0 .AND. KPCH.EQ.0) GO TO 2510
      ICARD = 0
      CALL OPEN (*2620,SCRT,Z(BUF3),0)
C
 2310 CONTINUE
      CALL PAGE
      WRITE  (OUTT,2320)
 2320 FORMAT (5X,'ALTER DECK ECHO')
      NLINES = NLINES + 1
 2330 IF (NLINES.GE.NLPP .AND. KPRT.NE.0) GO TO 2310
      CALL READ (*2500,*2360,SCRT,CARD,18,1,NW)
C
C     DMAP CARD
C
      NC = 18
      IF (KPRT .NE. 0) WRITE (OUTT,2340) ICARD,(CARD(I),I=1,NC)
      IF (KPRT .NE. 0) NLINES = NLINES + 1
      IF (KPCH .NE. 0) WRITE (LPCH,2350) (CARD(I),I=1,NC)
      ICARD = ICARD + 1
 2340 FORMAT (4X,I5,4X,18A4)
 2350 FORMAT (18A4)
      GO TO 2330
 2360 IF (ICARD .GT. 0) GO TO 2370
      ICARD = 1
      GO TO 2330
C
C      ALTER CARD
C
 2370 IF (CARD(2) .LE. 0) GO TO 2400
      IF (KPRT .NE. 0) WRITE (OUTT,2380) ICARD,ALT1,ALT2,(CARD(I),I=1,2)
      IF (KPRT .NE. 0) NLINES = NLINES + 1
      IF (KPCH .NE. 0) WRITE (LPCH,2390) ALT1,ALT2,CARD(1),CARD(2)
      ICARD = ICARD + 1
 2380 FORMAT (5X,I4,4X,2A4,I8,1H, ,I3)
 2390 FORMAT (2A4,I8,1H,,I3)
      GO TO 2330
 2400 IF (KPRT .NE. 0) WRITE (OUTT,2410)ICARD,ALT1,ALT2,CARD(1)
      IF (KPRT .NE. 0) NLINES = NLINES + 1
      IF (KPCH .NE. 0) WRITE (LPCH,2420) ALT1,ALT2,CARD(1)
      ICARD = ICARD + 1
 2410 FORMAT (5X,I4,4X,2A4,I8)
 2420 FORMAT (2A4,I8)
      GO TO 2330
C
C     END OF FILE
C
 2500 CALL CLOSE (SCRT,0)
 2510 CONTINUE
      CALL OPEN (*2620,SCRT,Z(BUF3),0)
      CALL OPEN (*2620,PTAPE,Z(BUF1),0)
C
C     COPY SCRATCH TO PROB.TAPE, FIRST POSITION PTAPE TO XALTER OR
C     XCSA FILE
C
      ISKP = KFILE
      IF (KALT .NE. 0) ISKP = KALT
      CALL SKPFIL (PTAPE,ISKP)
      CALL CLOSE (PTAPE,2)
      CALL OPEN (*2620,PTAPE,Z(BUF1),3)
      CALL READ (*2620,*2520,SCRT,II,9,1,NW)
 2520 PASS2 = .TRUE.
      IFIN  = .FALSE.
      IFILE = SCRT
      OFILE = PTAPE
      GO TO 2260
 2530 CONTINUE
C
      CALL CLOSE (SCRT,1)
C
C     CLOSE CASE CONTROL
C
 2540 IF (PHASE .NE. 3) CALL CLOSE (CASE,2)
      CALL CONMSG (ASD2,2,0)
      RETURN
C
C     USER FATAL MESSAGES
C
 2600 WRITE  (OUTT,2610) UFM
 2610 FORMAT (A23,' 6017, MISSING ENDSUBS CARD.')
      CALL MESAGE (-37,0,SUBNAM)
C
C     SYSTEM ERROR MESSAGES
C
 2620 WRITE  (OUTT,2630) SFM,FILE
 2630 FORMAT (A25,' 6007, IMPROPER FILE SETUP FOR ',A4)
      NLINES = NLINES + 2
      GO TO 2680
 2640 WRITE  (OUTT,2650) UFM,CNAME
 2650 FORMAT (A23,' 6005, ILLEGAL OR MISSING DATA FOR THE PREVIOUS ',
     1       'COMMAND - ',A4)
      NLINES = NLINES + 2
      NOGO   = 1
      GO TO 2090
 2660 WRITE  (OUTT,2670) UFM,ALTS(1),ALTS(2),II
 2670 FORMAT (A23,' 6006, DMAP ALTERS  ',2I8, /5X,
     1        'INTERFERE WITH SUBSTRUCTURE ALTERS  ',2I4)
      NLINES = NLINES + 3
      NOGO   = 1
      GO TO 2090
 2680 WRITE  (OUTT,2690) UFM
 2690 FORMAT (A23,' 6009, UNRECOVERABLE ERROR CONDITIONS IN SUBROUTINE',
     1       ' ASDMAP')
      NLINES = NLINES + 2
      NOGO   = 3
      CALL CLOSE (SCRT ,1)
      CALL CLOSE (CASE ,1)
      CALL CLOSE (PTAPE,1)
      RETURN
      END
