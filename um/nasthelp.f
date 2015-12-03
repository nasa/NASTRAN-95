      PROGRAM NASTHELP
C
CDC   PROGRAM NASTHELP (INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT,
CDC  1                  TAPE2,TAPE3,TAPE4)
C
C     THIS PROGRAM PROVIDES ON-LINE SCREEN HELP FOR NASTRAN USER'S
C     MANUAL INFORMATION. THE COMPLETE MANUAL IS STORED IN THE
C     FOLLOWING ASCII TEXT FILES, WHICH ARE ALSO ACCESSIBLE TO ANY
C     SYSTEM EDITOR:
C
C          MANUAL SECTION                                FILE NAME
C          -------------------                           ---------
C       1. EXECUTIVE CONTROL                             EXEC.TXT
C       2. CASE CONTROL                                  CASE.TXT
C       3. INPUT BULK DATA                               BULK.TXT
C       4. PLOTTING                                      PLOT.TXT
C       5. DMAP                                          DMAP.TXT
C       6. SUBSTRUCTURE                                  SUBS.TXT
C       7. ERROR MESSAGES                                MSSG.TXT
C       8. NASTRAN DICTIONARY                            DICT.TXT
C       9. INTRODUCTION & GENERAL INFORMATION            INTR.TXT
C      10. USER'S MASTER FILE AND USER GENERATED INPUT   UMFL.TXT
C      11. RIGID FORMATS                                 RFMT.TXT
C
C
C     IN ADDITION,
C     IF FILE SELECTION IS FOLLOWED BY ',C', THIS PROGRAM WILL ONLY
C     CHECK THE INPUT FILE FOR OUT-OF-ORDER ITEMS, DUPLICATE ITEMS,
C     AND HEADER 12 CHARACTERS (EXEC, CASE, BULK, PLOT, DMAP AND
C     SUBS.TXT FILES ONLY)
C
C     IF FILE SELECTION IS FOLLOWED BY ',P', THIS PROGRAM WILL PRINT
C     THE ENTIRE CONTENTS OF THE FILE WITH PROPER CARRIAGE CONTROL AND
C     PAGING
C
C     DESIGN REQUIREMENTS FOR MANUAL TEXT FILES
C     (1) A POUND SIGN (#) ON COLUMN 1, MUST PRECEED EACH ITEM
C     (2) '=PAGE=' IN FIRST 6 COLUMNS OF A LINE IS A PAGE MARK
C     (3) EACH ITEM MUST BEGIN WITH ONE OF THE FOLLOWING WORDS, 12 CHAR.
C         EACH               111111
C                 ..123456789012345..(COLUMN)
C                   Executive Co
C                   Case Control
C                   Input Data C
C                   Structure Pl
C                   X-Y Output D
C                   Name:
C                   Substructure Co (special
C                   Substructure Mo  15
C                   Substructure Op  chars.)
C         (REVISED 4/93, DUE TO CHANGES IN THE .TXT FILES, HEADER WORDS
C         IN (3) ARE NO LONGER USED)
C     (4) SEARCH BY KEY NASTRAN WORD AFTER EACH HEADER WORDS IN (3)
C     (5) KEY NASTRAN WORDS MUST BE IN ALPHA-NUMERIC SORT
C     (6) USE ',C' OPTION IN FILE SELECTION FOR DATA CARDS CHECK
C
C     SOME OF THE ABOVE COMMENTS MAY NO LONGER BE TRUE (1993)
C
C     FORTRAN FILE ASSIGNMENTS -
C
C                  FORTRAN
C     FILE NAME    UNIT NO.   STATUS            FILE CONTENTS
C     -----------  -------   ---------  --------------------------------
C     SYS$INPUT        5      INPUT     KEYBOARD INPUT
C     SYS$OUTPUT       6      OUTPUT    TERMINAL OUTPUT
C     COV.TAB          4      INPUT     CONTAINS MACHINE DEPENDENT ASCII
C                           (OPTIONAL)  SPECIAL SYMBOL CONVERSION TABLE
C     NASTRAN MANUAL   3      INPUT     EXEC,CASE,BULK,PLOT,SUBS,DMAP,
C        .TXT FILES                     MSSG,DICT,INTR UMFL and RFMT.TXT
C     USER GIVEN       2      OUTPUT    OUTPUT PRINT FILE
C      FILE NAME            (OPTIONAL)
C
C     PROGRAM FLAGS USED:
C     FL    = 1  THRU 11, FOR 11 DIFFERENT MANUAL.TXT FILES
C     SEC   = 1, MEANS SEARCH BY SECTION ALLOWED, ZERO OTHERWISE
C     HD12  = 0, NO HEADER LINE ON TEXT. OTHEREWISE,
C                HD12(FL) IS THE APPROPIATE HEADER LINE FOR FILE FL
C     BASE  = N, SKIP N WORDS ON HEADER LINE WHEN SEARCHING KEY WORD
C           = 0, SET TO ZERO DUE TO CHANGES IN 1993 USER'S MANUAL
C     MIDPT = AN INTEGER OF A CHARACTER SYMBOL INDICATING THE MID POINT
C                ON TEXT FILE
C     MDPT  = M, NO. OF LINES TO SKIP TO MID POINT OF TEXT FILE
C           = 0, MEANS NO SKIPPING
C     J4    =    2ND ALTERNATE BASE FOR PLOT.TXT
C     J5,J6 =    2ND AND 3RD ALTERNATE BASE FOR SUBS.TXT
C
C     WRITTEN BY GORDON CHAN/UNISYS       3/1992
C     REVISED FOR NEW .TXT FILES FORMAT   4/1993
C     General cleanup and comments 
C     added by Reg Mitchell, GSFC         8/1994
C     Last modification                   9/9/94
C
      IMPLICIT INTEGER (A-Z)
      LOGICAL      CHECK,PRINT,FIRST,ETY,POPEN,DEBUG
      INTEGER      IVAL(256),NUMSUB(256),NVAL(256,10),BASE(11)
      CHARACTER*1  KA,KB,KC,KD,KE,KF,KG,KH,KI,KJ,KK,KL,KM,KN,KO,KP,KQ,
     1             KR,KS,KT,KU,KV,KW,KX,KY,KZ,BNK1,LB1,UP1,LT1,MNS,PLUS,
     2             CMA,DOT,NUM,QM, LLa,LLz,
     3             LC,IC,JC,JX1,YES,NO, A1(80),A11,K1(8)
      CHARACTER*4  A4,KEY4,KEY42,BNK4,NEW4,WAS4,APPR,APP,SOLU,SOL,
     1             STP4,STP4L,EXIT4,EXIT4L,QUIT4,QUIT4L,NXTP,NXTS,NXTB,
     2             FILE,FILEX(11)
      CHARACTER    PAG3*3,PAG6*6,KEY6*6,KEY8*8,FNAME*8,MACH*16,DATE9*9,
     1             A3*3,A6*6,A12*12,OU12*42,A48*48,B48*48,A79*79,A80*80,
     2             K44*44,KA44*44,HD6*6,HD12(6)*12,DBGO*8,DBGF*8,TCTF*35
      CHARACTER    DEV_DIR*7
      COMMON /KHR/ KA,KB,KC,KD,KE,KF,KG,KH,KI,KJ,KK,KL,KM,KN,KO,KP,KQ,
     1             KR,KS,KT,KU,KV,KW,KX,KY,KZ,BNK1,LB1,UP1,LT1,MNS,PLUS,
     2             CMA,DOT,NUM(10)
      EQUIVALENCE  (KA44,KA),(YES,KY),(NO,KN),(PAG3,PAG6),(FILE,FNAME),
     1             (A1(1),A11,A4,A3,A6,A12,A48,A79,A80,JX1),(A1(3),JC),
     2             (K1(1),KEY4,KEY6,KEY8),(KEY42,K1(5)),(A1(2),IC)
      DATA         IN,OUT,TB,OU / 5, 6, 4, 2  /, NLP,ETY / 21, .TRUE. /,
C    1             BASE  / 23, 24, 17, 25, 6, 21, 0, 0, 0, 0, 0       /,
     1             BASE  /  0,  0,  0,  0, 0,  0, 0, 0, 0, 0, 0       /,
     2             J4,J5,J6, T1,       T2,    T3 , LLa, LLz, NEW4     /
     3             21,26,41, 16777216, 65536, 256, 'a', 'z', '|   '   /,
     4             K44 /'ABCDEFGHIJKLMNOPQRSTUVWXYZ #^<-+,.1234567890'/,
     5             BNK4,STP4,PAG6,B48  / '   ', 'STOP','=PAGE=', ' '  /,
     6             NXTP,NXTS,NXTB,QM   / 'P  ', 'S   ','B   ', '?'    /,
     7             APPR,APP, SOLU,SOL  / 'APPR','APP ','SOLU','SOL '  /,
     8             FILEX / 'EXEC','CASE','BULK','PLOT','DMAP','SUBS'  ,
     9             'MSSG','DICT','INTR','UMFL','RFMT'/,FNAME/'XXXX.TXT'
     O/,           MACH / '  UNIX VERSION  '  /,  DATE9 / 'AUG. 1994' /,
     1             FIRST,CHECK,PRINT,DEBUG    / .TRUE.,  3*.FALSE.    /,
     2             LU,IVFF / 3, 12 /,     HD6 / 'Name: '/,   HD12     /
     3             'Executive Co', 'Case Control', 'Input Data C'     ,
     4             'Structure Pl',  'X-Y Output D', 'Substructure'    /,
     5             TCTF / ' or terminate current text file(^):'       /,
     6             DBGO,DBGF / 'DEBUG ON', 'DEBUG OF'/,STP4L /'stop'/,
     7             EXIT4,EXIT4L / 'EXIT','exit' /,
     8             QUIT4,QUIT4L / 'QUIT','quit' /
      DATA DEV_DIR/'DEV_DIR'/        
C
      COMPLF(I) = NOT(I)
C
      KA44   = K44
      NUM1   = ICHAR(NUM(1))
      NUM9   = ICHAR(NUM(9))
      La     = ICHAR(LLa)
      Lz     = ICHAR(LLz)
      BA     = ICHAR(KA )
      Aa     = BA - La
      NUMCOV = 0
      POPEN  = .FALSE.
C
C     OPEN THE SPECIAL CHARACTER CONVERSION FILE (UNIT 2).
C
      OPEN (UNIT=TB,FILE='COV.TAB',ACCESS='SEQUENTIAL',FORM='FORMATTED',
     1      STATUS='OLD',ERR=130)
C
C     COV.TAB FILE BEGINS WITH A HEADER RECORD, THEN FOLLOWED BY RECORDS
C     OF 3 INTEGER WORDS, IN 3I4 FORMAT, WHICH ARE:
C     INCOMING SYMBOL, NO. OF BYTE, AND OUTGOING CORRESPONDING SYMBOL
C
C        1   1 124
C        2   1 124
C        :   :  :
C      256   1 124
C
      READ (TB,100,END=130)
      DO 120 I = 1,256
      READ (TB,100,END=130) IVAL(I),N,(NVAL(I,J),J=1,N)
      IF (N .GT. 10) WRITE (OUT,110) I,N
  100 FORMAT (12I4)
  110 FORMAT (' *** Error in COV.TAB    I,N =',2I5)
      NUMSUB(I) = N
  120 CONTINUE
      I = 257
      CLOSE (UNIT=TB)
C     SPECIAL CHARACTER TABLE HAS BEEN READ
  130 NUMCOV = I - 1
C
C     PRINT PROGRAM HEADER
C
      WRITE  (OUT,140)
  140 FORMAT (/////////)
      WRITE  (OUT,150) MACH,DATE9
  150 FORMAT (34X,4H****, /32X,1H*,6X,1H*, /31X,1H*,8X,1H*, /31X,
     1        18H*  N A S T H E L P, /31X,1H*,8X,1H*, /32X,1H*,6X,1H*,
     2        /34X,4H****, ///15X,A16,10X,17HSYSTEM RELEASE - ,A9)
      WRITE  (OUT,160)
  160 FORMAT (//,' Is your screen capable of MORE THAN 80 columns? ',
     1       '(Y or N (default))')
      READ   (IN,170) LC
  170 FORMAT (3A1)
      ETY = .FALSE.
      IF (LC .EQ. BNK1) GO TO 180
      IF (ICHAR(LC).GE.La .AND. ICHAR(LC).LE.Lz) LC = CHAR(ICHAR(LC)+Aa)
      IF (LC .EQ. YES) ETY = .TRUE.
  180 WRITE  (OUT,190) NLP
  190 FORMAT (//,' Enter LINES PER PAGE (default is',I3,') ')
      READ   (IN,200,ERR=180) J
  200 FORMAT (I2)
      IF (J .GT. 1) NLP = J
      NLP4 = NLP - 4
      WRITE  (OUT,210)
  210 FORMAT (/,' A text line marked by | in column 1 indicates that ',
     1        'this line contains updated', /,' material since the ',
     2        'June 1986 NASTRAN Users'' Manual')
      IF (ETY) WRITE (OUT,215)
  215 FORMAT (/,' If your screen loses character in column 80, it is ',
     1        'because your terminal lacks',/,' 80-column capability.')
      WRITE  (OUT,220)
  220 FORMAT (//,' NASTHELP accepts both UPPER and lower case input')
C    1         /,' The terms STOP and QUIT are interchangable')
C     READY TO BEGIN READING A MANUAL
      GO TO 240
C     CLOSE CURRENT MANUAL IF STILL OPEN
  230 CLOSE (UNIT=LU)
C
C     PROCESS REQUEST FOR USER MANUAL SELECTION
C
  240 WRITE  (OUT,250)
  250 FORMAT (/,' Enter letter for desired part of NASTRAN',
     O        ' User''s Manual',//,
     1 '  Introduction(I)     Don''t know(?)',/,
     2 '  Executive(E)        Case control(C)     Bulkdata(B)',/,
     3 '  DMAP(D)             Rigid Formats(R)    Plotting(P)',/,
     4 '  Messages(M)         Substructures(S)    UMF/UGI(U)',/,
     5 '  Dictionary(T)       Stop/quit(STOP)')
      READ (IN,170) LC,IC,JC
C     CONVERT TO UPPER CASE IF NECESSARY
      IF (ICHAR(LC).GE.La .AND. ICHAR(LC).LE.Lz) LC = CHAR(ICHAR(LC)+Aa)
      IF (ICHAR(IC).GE.La .AND. ICHAR(IC).LE.Lz) IC = CHAR(ICHAR(IC)+Aa)
      IF (ICHAR(JC).GE.La .AND. ICHAR(JC).LE.Lz) JC = CHAR(ICHAR(JC)+Aa)
C     CHECK FOR A COMMA IN SECOND POSITION
      IF (IC .NE. CMA) GO TO 260
      IF (JC .EQ.  KC) CHECK = .TRUE.
      IF (JC .EQ.  KP) PRINT = .TRUE.
  260 IF (LC.EQ.KS .AND. JC.EQ.KO) GO TO 2210
      FIRST = .TRUE.
      LAST  = -1
      SEC   =  1
      MDPT  =  0
      MQ    =  0
      IF (LC .EQ. KE) GO TO 310
      IF (LC .EQ. KC) GO TO 320
      IF (LC .EQ. KB) GO TO 330
      IF (LC .EQ. KP) GO TO 340
      IF (LC .EQ. KD) GO TO 350
      IF (LC .EQ. KS) GO TO 360
      IF (LC .EQ. KM) GO TO 370
      IF (LC .EQ. KT) GO TO 380
      IF (LC .EQ. KI) GO TO 390
      IF (LC .EQ. KU) GO TO 400
      IF (LC .EQ. KR) GO TO 405
      IF (LC .EQ. QM) GO TO 280
      WRITE  (OUT,270)
  270 FORMAT (/,' *** SELECTION error')
      GO TO 240
C
  280 MQ = 1
      GO TO 310
C
C     GET NEW MANUAL REQUEST
  300 CLOSE (UNIT=LU)
      GO TO 240
C
C     SET FL TO THE REQUESTED NASTRAN MANUAL (EXEC=1, CASE=2, ETC.)
C     DEFINE MID-POINT IN FILE FOR SKIPPING, AND SET HEADER SEARCH
C     MIDPT IS THE FIRST LETTER OF KEY WORDS,
C     MDPT  IS NO. OF RECORDS TO BE SKIPPED.
C
C     USE ANY SYSTEM EDITOR TO LOCATE THE MID-POINT OF FILE
C
  310 FL   = 1
      GO TO  410
  320 FL   = 2
      GO TO  410
  330 FL   = 3
      MIDPT= ICHAR(KN)
      MDPT = 10741
      GO TO  410
  340 FL   = 4
      GO TO  410
  350 FL   = 5
      GO TO  410
  360 FL   = 6
      GO TO  410
  370 FL   = 7
      SEC  = 0
      MIDPT= ICHAR(NUM(3))
      MDPT = 3550
      GO TO  410
  380 FL   = 8
      SEC  = 0
      GO TO  410
  390 FL   = 9
      IF (NLP-4 .EQ. NLP4) NLP = NLP - 1
      GO TO  410
  400 FL   = 10
      GO TO  410
  405 FL   = 11
      GO TO  410
C
C     OPEN THE REQUESTED NASTRAN MANUAL FILE
C
  410 FILE = FILEX(FL)
      WAS1 = COMPLF(0)
      WAS2 = WAS1
      OPEN (UNIT=LU,FILE=FNAME,ACCESS='SEQUENTIAL',FORM='FORMATTED',
     1      STATUS='OLD',ERR=412)
      GO TO 415
  412 WRITE  (OUT,413) FNAME
  413 FORMAT (//1X,A8,' file DOES NOT EXIST')
      IF (JC .NE. KU) GO TO 240
      GO TO 2210
C
  415 CONTINUE
      IF (CHECK) GO TO 1700
      IF (PRINT) GO TO 2000
      SOUNT = 0
C     SEARCH KEY WORD OR NUMBER AS APPROPRIATE, BASED ON MANUAL FLAG.
C     1=EXEC,2=CASE,3=BULK,4=PLOT,5=DMAP,6=SUBS,7=MSSG,8=DICT,
C     9=INTR,10=UMFL,11=RFMT
  420 GO TO (650,650,650,650,650,650,430,540,610,610,650), FL
C
C     MESSAGE SEARCH IN FILE MSSG.TXT
C
  430 WRITE  (OUT,440) TCTF
  440 FORMAT (/,' Enter MESSAGE NUMBER (up to 4 digits) or STOP,',A35)
      READ   (IN,450) A4
  450 FORMAT (A4)
      IF (A11 .EQ. UP1) GO TO 300
      IF (A4 .EQ. STP4) GO TO 2200
      IF (A4 .EQ. STP4L) GO TO 2200
      IF (A4 .EQ. EXIT4) GO TO 2200
      IF (A4 .EQ. EXIT4L) GO TO 2200
      IF (A4 .EQ. QUIT4) GO TO 2200
      IF (A4 .EQ. QUIT4L) GO TO 2200
      REWIND LU
      COUNT = 0
      J = 0
  460 IF (A1(3) .NE. BNK1) GO TO 470
      J = J + 1
      IF (J .GT. 2) GO TO 430
      A1(3) = A1(2)
      A1(2) = A1(1)
      A11   = BNK1
      GO TO 460
  470 IF (A1(4) .EQ. BNK1) GO TO 490
      IF (MDPT.EQ.0 .OR. ICHAR(A11).LT.MIDPT) GO TO 490
      DO 480 I = 1,MDPT
      READ (LU,450,END=500)
  480 CONTINUE
      COUNT = MDPT
  490 READ (LU,450,END=500) KEY4
      COUNT = COUNT + 1
      IF (KEY4.EQ.BNK4 .OR. KEY4.EQ.NEW4 .OR. KEY4.NE.A4) GO TO 490
      BACKSPACE LU
      KOUNT = 2
      GO TO 1200
  500 WRITE  (OUT,510)
  510 FORMAT (' *** No Such MESSAGE NO. ***',/)
      GO TO 430
C
C     DICTIONARY SEARCH IN FILE DICT.TXT
C
  540 WRITE  (OUT,550) TCTF
  550 FORMAT (/,' Enter DICTIONARY word or STOP,',A35)
      READ   (IN,560) A6
  560 FORMAT (A6)
      IF (A11 .EQ. UP1) GO TO 300
      DO 570 J = 1,6
      IF (ICHAR(A1(J)).GE.La .AND. ICHAR(A1(J)).LE.Lz)
     1    A1(J) = CHAR(ICHAR(A1(J))+Aa)
  570 CONTINUE
      IF (A4 .EQ. STP4) GO TO 300
      IF (A4 .EQ. QUIT4) GO TO 300
      IF (A4 .EQ. STP4L) GO TO 300
      IF (A4 .EQ. QUIT4L) GO TO 300
      REWIND LU
  580 READ (LU,560,END=590) KEY6
      IF (KEY4.EQ.BNK4 .OR. KEY4.EQ.NEW4 .OR. KEY6.NE.A6) GO TO 580
      BACKSPACE LU
      KOUNT = 1
      GO TO 1200
  590 WRITE  (OUT,600)
  600 FORMAT (' *** No such term in NASTRAN Dictionary')
      GO TO 540
C
C     INTRODUCTION OR USER-MASTER-FILE SEARCH OF FILES INTR OR UMFL.TXT
C
C     SEARCH BY SECTION, SUBSECTION, AND PAGE ONLY
C     SECTION AND SUBSECTION MUST BE PRECEEDED BY A BLANK LINE
C
  610 WRITE  (OUT,620) TCTF
  620 FORMAT (/,' Enter section(S), sub-section(B), page(P), ',
     1        'or stop(STOP),', /,A35)
      GO TO 670
C
  630 PASS = PASS + 1
      IF (PASS .GE. 2) WRITE (OUT,640) FNAME
  640 FORMAT (17X,'No such WORD in ',A8,' file')
      REWIND LU
      COUNT = 0
      IF (PASS .EQ. 1) GO TO 700
C
C     GENERAL SEARCH FOR FILE TYPES = EXEC, CASE, BULK, PLOT, DMAP,
C     SUBS OR RFMT.TXT
C     KEY WORD or SECTION SEARCH
C
  650 WRITE  (OUT,660) TCTF
  660 FORMAT (/,' Enter NASTRAN KEY WORD, STOP, next page(P),',
     1 ' next section(S),',/,' next sub-section(B), ',A35)
  670 READ   (IN,680) KEY8
  680 FORMAT (A8)
      PASS = 0
      LAST = COUNT
      IF (K1(1) .EQ. UP1) GO TO 230
C     NOT A ^ CHARACTER, CONVERT TO UPPER CASE AND PROCESS
      DO 690 ILOOP = 1,8
      IF (ICHAR(K1(ILOOP)).GE.La .AND. ICHAR(K1(ILOOP)).LE.Lz)
     1    K1(ILOOP) = CHAR(ICHAR(K1(ILOOP))+Aa)
  690 CONTINUE
      IF (KEY4 .EQ. STP4) GO TO 1600
      IF (KEY4 .EQ. QUIT4) GO TO 1600
      IF (KEY4 .EQ. BNK4) GO TO 650
      IF (KEY4 .EQ. NXTP) GO TO 980
      IF (KEY4 .EQ. NXTS) GO TO 750
      IF (KEY4 .EQ. NXTB) GO TO 730
      IF (KEY8 .EQ. DBGO) DEBUG = .TRUE.
      IF (KEY8 .EQ. DBGF) DEBUG = .FALSE.
      IF (KEY8.EQ.DBGO .OR. KEY8.EQ.DBGF) GO TO 650
      IF (FL.EQ.9 .OR. FL.EQ.10) GO TO 610
  700 JDX = 9
      IF (SEC .EQ. 1) GO TO 900
      IF (K1(1).NE.KS .AND. K1(1).NE.KB) GO TO 900
C
  710 WRITE  (OUT,720) FNAME
  720 FORMAT (/,' *** Search by SECTION is not practical on this ',A8,
     1        ' file')
      GO TO 650
C
  730 KEY4 = NXTB
      IF (SOUNT .GT. 0) GO TO 755
      WRITE  (OUT,740)
  740 FORMAT (/,' *** SUBSECTION is requested without first request of',
     1       ' SECTION ***')
      GO TO 650
  750 KEY4  = NXTS
      SOUNT = 0
      IF (COUNT .LE. 1) GO TO 1200
  755 A4   = STP4
  760 WAS4 = A4
      READ (LU,770,END=880) A12
  770 FORMAT (A12)
      COUNT = COUNT + 1
      IF (PASS.EQ.2 .AND. COUNT.EQ.LAST) GO TO 1620
      IF (A4.EQ.BNK4 .OR. A4.EQ.NEW4) GO TO 760
      IF (WAS4 .NE. BNK4) GO TO 760
      I = ICHAR(A11)
      IF (A11 .EQ. BNK1) I = ICHAR(A1(2))
      IF (I.LT.NUM1 .OR. I.GT.NUM9) GO TO 760
      NDOT = 0
      DO 780 I = 2,11
      IF (A1(I) .NE. DOT) GO TO 780
      IF (A1(I+1).NE.BNK1 .AND. A1(I+1).NE.DOT) NDOT = NDOT + 1
  780 CONTINUE
      IF (NDOT-1) 760,790,810
  790 IF (KEY4 .EQ. NXTB) GO TO 820
      SOUNT = COUNT - 1
  800 COUNT = COUNT - 1
      BACKSPACE LU
      GO TO 1200
  810 IF (KEY4 .EQ. NXTS) GO TO 760
      GO TO 800
  820 WRITE  (OUT,830) TCTF
  830 FORMAT (/,' *** End of SECTION ***', /,' return to Key(K), ',
     1       'return to beginning of section(R), next section(N)',
     2        /,' stop(STOP),',A35)
      READ (IN,170) IC
      IF (ICHAR(IC).GE.La .AND. ICHAR(IC).LE.Lz) IC = CHAR(ICHAR(IC)+Aa)
      IF (IC .EQ. UP1) GO TO 230
      IF (IC .EQ. KS ) GO TO 2200
      IF (IC .EQ. KK ) GO TO 650
      IF (IC .EQ. KN ) GO TO 860
      IF (IC .NE. KR ) GO TO 820
      IF (SOUNT .LE. 1) GO TO 870
      J = COUNT - SOUNT + 2
  840 DO 850 I = 1,J
      BACKSPACE LU
  850 COUNT = COUNT - 1
      IF (COUNT .LT. 0) COUNT = 0
      KEY4 = NXTS
      GO TO 1200
  860 J = 1
      GO TO 840
  870 REWIND LU
      KEY4 = NXTS
      GO TO 1200
  880 WRITE  (OUT,890)
  890 FORMAT (' *** End of File ***')
      REWIND LU
      SOUNT = 0
      GO TO 650
C
C     KEY WORD SEARCH - FIRST SEARCH HEADING THEN KEY WORD
C
  900 JDX = JDX - 1
      IF (K1(JDX) .EQ. BNK1) GO TO 900
      IF (JDX .LE. 0) GO TO 630
      IF (FL  .NE. 1) GO TO 910
C
C     SOME KEY WORDS IN EXECUTIVE CONTROL SECTION MAY BE ABBREVIATED.
C     4 BYTES ARE USED FOR ALL EXECUTIVE CONTROL KEY WORDS
C
      IF (JDX .GT. 4) JDX = 4
      IF (KEY4 .EQ. APPR) KEY4 = APP
      IF (KEY4 .EQ. SOLU) KEY4 = SOL
C
C     IF KEY IS LESS THAN 4 LETTERS, ADD A BLANK AT THE END SO THAT
C     'SOF' IS NOT 'SOFIN', 'SOFOUT', etc.
C
  910 IF (JDX .GE. 4) GO TO 920
      JDX = JDX + 1
      K1(JDX) = BNK1
C
C     T1  = 2**8,  T2 = 2**16,  T3 = 2**24
C     IS0 = FIRST CHARACTER OF THE 8-BYTE KEY WORD IN NUMERIC VALUE
C     IS1 = FIRST  HALF OF THE 8-BYTE KEY WORD IN NUMERIC VALUE
C     IS2 = SECOND HALF OF THE 8-BYTE KEY WORD IN NUMERIC VALUE
C
C     THAT IS, WE WILL USE NUMERIC VALUE FOR KEY WORD SEARCH
C
  920 IS0 = ICHAR(K1(1))
      IS1 = IS0*T1 + ICHAR(K1(2))*T2 + ICHAR(K1(3))*T3 + ICHAR(K1(4))
      IS2 = ICHAR(K1(5))*T1 + ICHAR(K1(6))*T2 + ICHAR(K1(7))*T3 +
     1      ICHAR(K1(8))
C
C     COMPARE PRESENT KEY WORD AND PREVIOUS KEY AND DETERMINE WE NEED
C     TO REWIND FILE OR NOT
C
C     IF TEXT FILE IS NOT PRESORTED, WE NEED TO REWIND FILE ON EACH NEW
C     KEY WORD.  (USER'S MANUAL IS SORTED)
C
      IF (IS1-WAS1) 940,930,980
  930 IF (IS2-WAS2) 940,950,980
  940 REWIND LU
      COUNT = 0
      GO TO 980
  950 WRITE (OUT,960)
  960 FORMAT (' Same KEY WORD as before. Continue? (Y,N) ')
      READ (IN,170) IC
      IF (ICHAR(IC).GE.La .AND. ICHAR(IC).LE.Lz) IC = CHAR(ICHAR(IC)+Aa)
      IF (IC .EQ. NO) GO TO 650
      REWIND LU
      COUNT = 0
C
C     IF KEY WORD IS BEYOND MID-POINT, SKIP HALF OF THE RECORDS IN FILE
C
      IF (MDPT.EQ.0 .OR. ICHAR(K1(1)).LT.MIDPT) GO TO 980
      L = MDPT - COUNT + 1
      DO 970 J = 1,L
      READ (LU,170)
  970 CONTINUE
      COUNT = COUNT + L
C
C     IVFF IS PAGE MARK.  PAG6 IS '=PAGE='
C     LOOK FOR PAGE MARK OR '=PA' FIRST
C
  980 READ (LU,170,END=630) JX1,IC,JC
      COUNT = COUNT + 1
      IF (PASS.EQ.2 .AND. COUNT.EQ.LAST) GO TO 1620
      IVJX1 = ICHAR(JX1)
      IF (JX1.NE.LB1 .AND. IVJX1.NE.IVFF .AND. A3.NE.PAG3) GO TO 980
      IF (DEBUG) WRITE (OUT,990) JX1,IC,JC
  990 FORMAT (40X,'@980 Just read- ',8A1)
      IF (KEY4.EQ.NXTP .AND. A3.EQ.PAG3) GO TO 1240
      J = 0
 1000 READ (LU,1250,END=1450) A80
      IF (DEBUG) WRITE (OUT,1005) (A1(I),I=1,8)
 1005 FORMAT (36X,'@1005 Just read- ',8A1)
      COUNT = COUNT + 1
      IF (PASS.EQ.2 .AND. COUNT.EQ.LAST) GO TO 1620
      J = J + 1
      IF (J .GE. 7) GO TO 980
      IF (A4.EQ.BNK4 .OR. A4.EQ.NEW4) GO TO 1000
      IF (ICHAR(A11) .EQ. IVFF) GO TO 1000
C
C     KEY WORD HEADING SEARCH
C
C     ******************************************************
C     *   HEADER WORDS WERE REMOVED IN 1993 USER'S MANUAL  *
          J = 0
          IF (J .EQ. 0) GO TO 1120
C     ******************************************************
C
      GO TO (1010,1010,1010,1010,1030,1010,1100,1100,1100,1100,1050), FL
 1010 IF (DEBUG) WRITE (OUT,1020) A12,HD12(FL)
 1020 FORMAT (50X,A12,'==> ',A12)
      IF (A12 .EQ. HD12(FL)) GO TO 1050
      IF (FL.EQ.4 .AND. A12.EQ.HD12(5)) GO TO 1040
      GO TO 1000
 1030 IF (A6 .NE. HD6) GO TO 1000
      GO TO 1050
C
 1040 J = J4
      GO TO 1060
 1050 J = BASE(FL)
 1060 IF (FL .NE. 6) GO TO 1070
      IF (A1(14) .EQ.  KM) J = J5
      IF (A1(14) .EQ.  KO) J = J6
 1070 IF (A1(J) .NE. BNK1) J = J - 1
      IF (DEBUG) WRITE (OUT,1080) J
 1080 FORMAT (45X,'@1080  BASE J =',I3)
 1090 IF (A1(J+1) .NE. BNK1) GO TO 1120
      J = J + 1
      GO TO 1090
C
 1100 WRITE  (OUT,1110) FL
 1110 FORMAT (/,' *** SHOULD NOT BE HERE.  FL =',I3)
      GO TO 240
C
C     KEY WORD SEARCH
C
 1120 IF (DEBUG) WRITE (OUT,1130) (A1(J+I),I=1,JDX),LT1,LT1,
     1                            (K1(I),I=1,JDX)
 1130 FORMAT (30X,'@1130 - ',18A1)
      DO 1140 I = 1,JDX
      IF (A1(J+I) .NE. K1(I)) GO TO 980
 1140 CONTINUE
C
C     KEY WORD FOUND ON FILE
C
      WAS1  = IS1
      WAS2  = IS2
      KOUNT = 6
      IF (FIRST) KOUNT = 8
      WRITE  (OUT,1150)
 1150 FORMAT (//)
      IF (     ETY) WRITE (OUT,1290) A80
      IF (.NOT.ETY) WRITE (OUT,1300) A79
C
C     RECORD FOUND.  READ AND PRINT ON SCREEN
C     ALLOW UP TO 4 BLANK LINES PRINTED ON SCREEN
C
 1200 JOUNT = COUNT - 4
      BLINE = 0
      IF (NUMCOV .EQ. 0) GO TO 1240
      DO 1210 J = 1,80
      DO 1210 I = 1,NUMCOV
      IF (ICHAR(A1(J)) .EQ. IVAL(I)) A1(J) = CHAR(NVAL(I,1))
 1210 CONTINUE
      GO TO 1240
C
 1220 WRITE  (OUT,1230)
 1230 FORMAT (///)
      KOUNT = KOUNT + 3
C
 1240 READ (LU,1250,END=1450) A80
 1250 FORMAT (A80)
      COUNT = COUNT + 1
      IF (ICHAR(A11).EQ.IVFF .OR. A6.EQ.PAG6) GO TO 1220
      IF (A48 .NE. B48) GO TO 1260
      IF (BLINE .GT. 4) GO TO 1240
      BLINE = BLINE + 1
      WRITE (OUT,170) A11
      GO TO 1310
 1260 BLINE = 0
      IF (NUMCOV .EQ. 0) GO TO 1280
      DO 1270 J = 1,80
      DO 1270 I = 1,NUMCOV
      IF (ICHAR(A1(J)) .EQ. IVAL(I)) A1(J) = CHAR(NVAL(I,1))
 1270 CONTINUE
 1280 IF (A11  .EQ.  LB1) GO TO 1470
      IF (     ETY) WRITE (OUT,1290) A80
      IF (.NOT.ETY) WRITE (OUT,1300) A79
 1290 FORMAT (1X,A80)
 1300 FORMAT (1X,A79)
 1310 KOUNT = KOUNT + 1
      IF (MOD(KOUNT,NLP) .NE. 0) GO TO 1240
      GO TO (1320,1320,1320,1320,1320,1320,420,420,1320,1320,1320), FL
 1320 IF (.NOT.FIRST) GO TO 1350
      FIRST = .FALSE.
      WRITE (OUT,1332) NLP4
 1332 FORMAT(' (Y,N,STOP,1,2,...,',I2,',-n,P,S,B,^,PRINT,HELP or <CR>)')
      GO TO 1350
 1330 WRITE (OUT,1340) NLP4,NLP,NLP4
 1340 FORMAT (' (Y,N,STOP,1,2,...,',I2,',-n,P,S,B,^,PRINT,HELP or <CR>)'
     C        /11X,'Y or <CR> = yes more',
     O        /11X,'N         = no more on this item',
     1        /11X,'STOP      = terminate NASTHELP',
     2        /11X,'1,2,...,n = keep bottom n lines on next page. (',I2,
     3                          ' max)',
     4        /11X,'-n        = back up n+',I2,' lines',
     5        /11X,'P,S,B     = go to next page, next section, or next',
     6                          ' sub-section',
     7        /11X,'^         = terminate current text file',
     8        /11X,'PRINT     = print text, up to last line on screen',
     9        /11X,'HELP      = echo options of MORE', /,' ...more? ')
      GO TO 1370
 1350 WRITE  (OUT,1360)
 1360 FORMAT (' ...more? ')
 1370 READ (IN,170) IC,JC,LC
      IF (IC .EQ. UP1) GO TO 230
      IF (ICHAR(LC).GE.La .AND. ICHAR(LC).LE.Lz) LC = CHAR(ICHAR(LC)+Aa)
      IF (ICHAR(IC).GE.La .AND. ICHAR(IC).LE.Lz) IC = CHAR(ICHAR(IC)+Aa)
      IF (ICHAR(JC).GE.La .AND. ICHAR(JC).LE.Lz) JC = CHAR(ICHAR(JC)+Aa)
      IF (IC .EQ. KH) GO TO 1330
      IF (IC .EQ. NO) GO TO 420
C     Check for STop, EXit ot QUit
      IF (IC.EQ.KS .AND. JC.EQ.KT) GO TO 1600
      IF (IC.EQ.KE .AND. JC.EQ.KX) GO TO 1600
      IF (IC.EQ.KQ .AND. JC.EQ.KU) GO TO 1600
C     Check for PRint request.
      IF (IC.EQ.KP .AND. JC.EQ.KR) GO TO 2000
      IF (SEC.EQ.0 .AND. (IC.EQ.KS .OR. IC.EQ.KB)) GO TO 710
      IF (IC .EQ. KS) GO TO 750
      IF (IC .EQ. KB) GO TO 1380
      IF (IC .NE. KP) GO TO 1390
      KEY4 = NXTP
      GO TO 980
 1380 IF (SOUNT .GT. 0) GO TO 730
      WRITE (OUT,740)
      GO TO 1350
 1390 KOUNT = 0
      IF (IC.EQ.BNK1 .OR. IC.EQ.YES) GO TO 1240
      I =  0
      J = -1
      L = -1
      DO 1400 K = 1,10
      IF (IC .EQ. NUM(K)) I = MOD(K,10)
      IF (JC .EQ. NUM(K)) J = MOD(K,10)
      IF (LC .EQ. NUM(K)) L = MOD(K,10)
 1400 CONTINUE
      IF (J+L .EQ. -2) IJL = I
      IF (L.EQ.-1 .AND. J.NE.-1) IJL = I*10  + J
      IF (L.NE.-1 .AND. J.NE.-1) IJL = I*100 + J*10 + L
      IF (IC .EQ. MNS) GO TO 1410
      IJL   = MIN0(NLP4,IJL)
      KOUNT = IJL + 1
      GO TO 1240
 1410 IJL   = IJL + NLP
      DO 1420 L = 1,IJL
      BACKSPACE LU
 1420 CONTINUE
      COUNT = COUNT - IJL
      KOUNT = 0
      IF (COUNT .LT. 0) COUNT = 0
      GO TO 1240
C
 1450 IF (LC .EQ. KU) GO TO 1610
      WRITE  (OUT,1460)
 1460 FORMAT (/,' ...EOF. <CR> to continue')
      GO TO 1500
C
 1470 WRITE  (OUT,1480)
 1480 FORMAT (/,' ...End of Description')
 1500 IF (KOUNT .LT. NLP4) GO TO 650
      READ (IN,170) IC
      IF (ICHAR(IC).GE.La .AND. ICHAR(IC).LE.Lz) IC = CHAR(ICHAR(IC)+Aa)
      IF (IC .NE. KS) GO TO 650
C
 1600 IF (MQ .EQ. 0) GO TO 2200
 1610 CLOSE (LU)
      KOUNT = 0
      GO TO (320,330,1620), FL
 1620 WRITE  (OUT,1630)
 1630 FORMAT (/,'*** No such KEY WORD in EXEC, CASE and BULK.TXT files')
      GO TO 240
C
C     CHECK INPUT TEXT FORMATS FOR
C     EXEC, CASE, BULK, PLOT, DMAP SUBS and RFMT.TXT FILES
C
 1700 IF (FL.LE.6 .OR. FL.EQ.11) GO TO 1720
      WRITE  (OUT,1710) FILEX(FL)
 1710 FORMAT (//,' *** Data CHECK OPTION not valid for ',A4,'.TXT file')
      GO TO 240
 1720 READ (LU,1730,END=2200) A48
 1730 FORMAT (A48)
      IF (A12 .EQ. HD12(FL)) GO TO 1760
      IF (FL.EQ.4 .AND. A12.EQ.HD12(5)) GO TO 1760
      IF (A11 .NE. LB1) GO TO 1720
 1740 READ (LU,1730,END=2200) A48
      IF (A4.EQ.BNK4 .OR. A4.EQ.NEW4) GO TO 1740
      IF (A12 .EQ. HD12(FL)) GO TO 1790
      IF (FL.EQ.4 .AND. A12.EQ.HD12(5)) GO TO 1780
      WRITE  (OUT,1750) A48
 1750 FORMAT (1X,A48,' <== HEADER 12 CHAR. ERROR')
      GO TO 1720
 1760 WRITE  (OUT,1770) A48
 1770 FORMAT (1X,A48,' <== NO PRECEEDING # SYMBOL')
      GO TO 1720
C
 1780 J = J4
      GO TO 1800
 1790 J = BASE(FL)
 1800 IF (FL .NE. 6) GO TO 1810
      IF (A1(14) .EQ. KM) J = J5
      IF (A1(14) .EQ. KO) J = J6
 1810 IF (A1(J  ) .NE. BNK1) J = J - 1
 1820 IF (A1(J+1) .NE. BNK1) GO TO 1830
      J = J + 1
      GO TO 1820
 1830 IS1 = ICHAR(A1(J+1))*T1 + ICHAR(A1(J+2))*T2 + ICHAR(A1(J+3))*T3 +
     1      ICHAR(A1(J+4))
      IS2 = ICHAR(A1(J+5))*T1 + ICHAR(A1(J+6))*T2 + ICHAR(A1(J+7))*T3 +
     1      ICHAR(A1(J+8))
      IF (IS1-WAS1) 1850,1840,1890
 1840 IF (IS2-WAS2) 1850,1870,1890
 1850 WRITE  (OUT,1860) A48
 1860 FORMAT (1X,A48,' <== OUT OF ORDER')
      GO TO  1910
 1870 WRITE  (OUT,1880) A48
 1880 FORMAT (1X,A48,' <== DUPLICATE')
      GO TO  1720
 1890 WRITE  (OUT,1900) A48
 1900 FORMAT (1X,A48)
 1910 WAS1 = IS1
      WAS2 = IS2
      GO TO 1720
C
C     PROCESS REQUEST TO PRINT MANUAL DATA FOUND
C
 2000 IF (POPEN) GO TO 2040
      WRITE  (OUT,2010)
 2010 FORMAT (/,1H-,' Enter OUTPUT FILE Name (assume default dir): ')
      READ   (IN,770) A12
      IF (A4.NE.BNK4 .AND. A4.NE.STP4 .AND. A4.NE.STP4L) GO TO 2030
      IF (A4.NE.EXIT4 .AND. A4.NE.EXIT4L) GO TO 2030
      IF (A4.NE.QUIT4 .AND. A4.NE.QUIT4L) GO TO 2030  
      WRITE  (OUT,2020)
 2020 FORMAT (/,' Output PRINT Aborted')
      GO TO 2200
 2030 OU12 = A12
      IF (POPEN) GO TO 2040
      OPEN (UNIT=OU,FILE=OU12,STATUS='NEW',ACCESS='SEQUENTIAL',ERR=2140,
     1      FORM='FORMATTED')
      POPEN = .TRUE.
      GO TO 2060
 2040 WRITE  (OU,2050)
 2050 FORMAT (1H1)
 2060 J = COUNT - JOUNT
      IF (J .LE. 0) GO TO 2120
      DO 2070 I = 1,J
      BACKSPACE LU
 2070 CONTINUE
      DO 2100 I = 1,J
      READ (LU,1250,END=2120) A80
      IF (A11 .EQ.  LB1) GO TO 2100
      IF (A6  .EQ. PAG6) GO TO 2090
      WRITE  (OU,2080) A80
 2080 FORMAT (1X,A80)
      GO TO 2100
 2090 WRITE  (OU,2050)
 2100 CONTINUE
C
 2120 WRITE  (OUT,2130) J
 2130 FORMAT (/,I9,' lines printed')
      GO TO 420
C
 2140 WRITE  (OUT,413) OU12
      GO TO 2000
C
C     END OF JOB PROCESSING
C
 2200 CLOSE  (LU)
      KOUNT = 0
 2210 WRITE  (OUT,2220)
 2220 FORMAT (//,'  *** NASTHELP is done.  Have a good run! ***',// )
      IF (.NOT.POPEN) GO TO 2240
      CLOSE  (OU)
      WRITE  (OUT,2230) OU12
 2230 FORMAT (/,'  *** Don''t forget the print file in ',A42)
      POPEN = .FALSE.
 2240 CONTINUE
      END
