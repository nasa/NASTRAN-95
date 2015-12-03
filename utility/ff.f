      PROGRAM FF
C
CDC   PROGRAM FF (TAPE3)
C
C     LAST MAJOR REVISIONS -  OCT 27, 1989
C                             JUN 25, 1991, FOR UNIX
C
C     A NASTRAN STAND-ALONE PROGRAM TO GENERATE NASTRAN FIX-FIELD INPUT
C     DECK FROM THIS FREE-FIELD INPUT PROGRAM.
C
C     *** THIS PROGRAM MUST BE COMPILED USING THE FOLLOWING COMPILERS
C         IBM VS FORTRAN, UNIVAC ASCII FORTRAN, CDC FORTRAN 5 (ANSI 77),
C         VAX FORTRAN IV-PLUS, AND IBM PC FORTRAN 3.0 OR RMC PROFESSION-
C         AL FORTRAN
C
C     *** VAX ONLY - THE OPEN AND CLOSE FUNCTIONS IN FF AND FFREAD MAY
C                    REQUIRE THE USE OF THE VAX ACCEPTABLE KEYWORDS.
C     (PRESENTLY, THE NON-VAX KEYWORDS ARE ACCEPTABLE BY ALL MACHINES)
C
C
C     WRITTEN BY G.CHAN/UNISYS,  APRIL 1985
C
C     SEE FFREAD SUBROUTINE FOR MORE DETAIL OF FREE-FIELD INPUT.
C
C     FILES ASSIGMENT
C           5 - CARD READER, OR CRT TERMINAL
C           6 - PRINTER, OR CRT TERMINAL
C           7 - PUNCH (IBM,CDC,VAX)
C           1 - PUNCH (UNIVAC ONLY)
C           2 - EQUATES TO USER FILE (ASSIGNED AUTOMATICALLY BY PROGRAM)
C           3 - A SCRATCH FILE TO HOLD THE USER ACTUAL INPUT CARDS
C           4 - USED ONLY IF READFILE IS USED TO READ AN OLD FILE
C
C     LINK THIS MAIN PROGRAM WITH THE FOLLOWING ROUTINES -
C     (THE EXECUTABLE IS CALLED LINKFF)
C
C           FF,FFHELP,FFREAD,UPCASE
C           INT2A8,A82INT,NA12IF,NA12A8,DUMMY
C
C     *** CDC ONLY - PROCEDURE TO LINK FF INTERACTIVELY
C           UPDATE,Q,P=EJPL,C=FF,L=0,N=0.
C           ? *C FF
C           ? <C/R>
C           REWIND,FF.
C           FTN5,STATIC,ARG=0,I=FF,L=0,B=FFLGO,LO=R/A/M/S.
C           ATTACH,NASTLIB/PN=DISC#.
C           LIBRARY,NASTLIB,FTN5LIB,BAMLIB.
C           REWIND,*.
C           LDSET,LIB=NASTLIB,PRESET=ZERO.
C           LDR>? LOAD,FFLGO.
C           LDR>? NOGO,LINKFF.
C
C     *** VAX ONLY - TO LINK FF USING NASTRAN LIBRARY
C           LINK/EXE=LINKFF.EXE NASTRAN.LIB/LIBRARY/INCLUDE=FF
C
C     *** UNIVAC ONLY - TO LINK FF WITH RELOCATABLES IN OBJ FILE
C           @PREP    OBJ.
C           @MAP,IS  TPF$.MAP,NASTRAN.LINKFF
C           LIB      OBJ.
C           IN       FF
C           END
C
C     *** IBM ONLY - LINKAGE EDITOR INPUT CARDS
C           INCLUDE LIB(FF,FFHELP,FFREAD,UPCASE)
C           INCLUDE LIB(INT2A8,A82INT,NA12IF,NA12A8)
C           INCLUDE PRIVLIB(IQZDDN,QQDCBF,QQGETF)
C           ENTRY   FF
C           NAME    LINKFF(R)
C
C           (NOTE - PRIVLIB IS LOCALLY SUPPLIED
C                   SEE THE DESCRIPTIONS OF IZZDDN,QQDCBF,QQGETF BELOW)
C
C     *** UNIX ONLY - TO LINK @LINKFF
C           cd
C           f77 ./mis/ff.f ./lib/nastlib.a -o @LINKFF
C
      LOGICAL         STAR,     PCT,      NOTYET,   PUNCH,    UPFLAG
      INTEGER         SCREEN,   PROM,     FFFLAG,   FACSF,    NC(7),
     1                IBM,      UNIVAC,   CDC,      VAX,      PC,
     2                UNIX
      CHARACTER*1     FN(1),    BK1,      N1,       Y1,       X1,
     1                S1,       W1,       A1,       H1,       CARD1(5),
     2                FX,       MARKQ,    QMARK,    TMP,      RPRN,
     3                SPL(8),   DOT,      D1,       T1
      CHARACTER*2     CARD2,    REPL
      CHARACTER*3     NEW,      OLD,      ODNW
      CHARACTER*4     CARD(20), SAVE(20), BEGN,     HELP,     STOP,
     1                LIST,     BLANK,    END1,     END2,     END3,
     2                CANC1,    CANC2,    ALTER,    CARD4,    FN4,
     3                BBK(2),   KSMB(9)
      CHARACTER*6     FN6,      BEGN6,    HELP6,    STOP6,    CARD6,
     1                MTYPE(7), CNTLWD(3)
      CHARACTER*8     FNAME(4), MYFILE,   BLNK8,    SITE,     CNTRL,
     1                USE(2),   SYM(4),   TAPEO3,   TPF,      FOROO3,
     2                DORK,     KEEP,     SPILL
      CHARACTER*11    F
      CHARACTER*32    FN32
      COMMON /SYSTEM/ IBUF,     NOUT,     NOGO,     IN,       ISYS(15)
      COMMON /MACHIN/ MACH
      COMMON /XXREAD/ INFLAG,   INSAVE,   LOOP4,    IBMCDC
      COMMON /XECHOX/ FFFLAG,   IECHO(3), ISORT(5)
      COMMON /XREADX/ SCREEN,   LOOP,     KOUNT,    PROM,     NOTYET,
     1                STAR,     PCT,      ICONT(36)
      COMMON /QMARKQ/ MARKQ,    TMP(16),  SPILL,    SAVE
      COMMON /UPCASX/ UPFLAG,   UPID(3)
      EQUIVALENCE     (FN(1),FN4,FN6,FN32,FNAME(1)),(XXI,LLI),
     1                (CARD(1),CARD6,CARD1(1),CARD2,CARD4),
     2                (SPL(1),SPILL)
      DATA            BEGN,     HELP,     STOP,     BLANK,    BK1    /
     1                'BEGI',   'HELP',   'STOP',   '    ',   ' '    /
      DATA            BEGN6,    HELP6,    STOP6,    BLNK8,    KEEP   /
     1                'BEGIN ', 'HELP  ', 'STOP  ', '    ',   'KEEP' /
      DATA            END1,     END2,     END3,     N1,       ALTER  /
     1                'ENDD',   'END ',   'ENDA',   'N',      'LTER' /
      DATA            CANC1,    CANC2,    DORK,     LLI,      LLJ    /
     1                'CANC',   'EL  ',   'DELETE', 4H  I ,   4H  J  /
      DATA            CNTRL,    MYFILE,   NEW,      OLD,      Y1     /
     1                'CENTRAL','MIFYLE', 'NEW',    'OLD',    'Y'    /
      DATA            NC/ 12,   7,   28,  7,   28,  0,  28           /
      DATA            UNIVAC,   IBM, CDC, VAX, PC,      UNIX         /
     1                3,        2,   4,   5,   1,       7            /
      DATA            MTYPE                                          /
     1                'IBM PC', '  IBM',  'UNIVAC', '  CDC',  '  VAX',
     2                ' ***  ', '  UNIX'                             /
      DATA            TAPEO3,   TPF,      FOROO3,   F                /
     1                'TAPE3.', 'FF$$.',  'FT03.',  'FF       FF'    /
      DATA            S1,       A1,       BBK,                LIST   /
     1                'S',      'A',      '(BLA', 'NK) ',     'LIST' /
      DATA            W1,       H1,       FX,       RPRN,     ICNTL  /
     1                'O',      'H',      'X',      ')',      0      /
      DATA            DOT,      D1,       T1                         /
     1                '.',      'D',      'T'                        /
      DATA            CNTLWD,                       NCNTL,    REPL   /
     1                'CANCEL', 'PROMPT', 'LIST  ', 3,        'R:'   /
      DATA            KSMB  /   '+C0N',   '+C1N',   '+C2N',   '+C3N' ,
     1                '+C4N',   '+C5N',   '+C6N',   '+C7N',   '+C8N' /
      DATA      USE / '@USE 3.,', 'FF$$ .  '/       PUNCH /   .FALSE./
      DATA      SYM / '@SYM    ', 'PUNCH$,,', '****', ' . '          /
      DATA            QMARK / '?' /
C     QMARK  IS QUESTION MARK -  0/12 PUNCH FOR 26 CODE
C                               0/7/8 PUNCH FOR 29 CODE
C
      J = LLJ - LLI
      MACH = IBM
      IF (J .EQ.   256) MACH = PC
      IF (J .EQ.   512) MACH = UNIVAC
      IF (J .GT. 65535) MACH = VAX
      IF (J .GT. 2**30) MACH = CDC
      IF (MACH.EQ.VAX .AND. (XXI.GT.1.60E-19 .OR. XXI.LT.1.8E-19))
     1    MACH = UNIX
      IF (MACH.EQ.VAX .AND. (XXI.GT.3.40E-20 .OR. XXI.LT.3.39E-20))
     1    MACH = PC
C
      LU      = 2
      LOUT    = 3
      IN      = 5
      NOUT    = 6
      IPUN    = 7
      SCREEN  = 6
      IF (MACH .EQ. VAX) SCREEN = 5
      LOOP    =-1
      KOUNT   = 0
      KONTN   = 10000000
      IKI     = 1
      PROM    =+1
      STAR    =.FALSE.
      PCT     =.FALSE.
      NOTYET  =.FALSE.
      MARKQ   = QMARK
      IECHO(2)=-2
      INSAVE  = IN
      INFLAG  = 0
      FFFLAG  = 0
      UPFLAG  =.FALSE.
      ISYS(15)= 0
      J       = NC(MACH)
      CARD4   = BLANK
      CARD6   = STOP6
      FN4     = STOP
      FN6     = STOP6
      LOOP4   = LOOP - 4
      DO 5 I = 1,20
 5    SAVE(I) = BLANK
      IBMCDC = UNIVAC + VAX + PC
      IF (MACH.EQ.IBM .OR. MACH.EQ.CDC) IBMCDC = 0
C
C     IF MACHINE IS CDC, OPEN INPUT, OUTPUT, AND PUNCH FILES
C
      IF (MACH .NE. CDC) GO TO 20
      OPEN (UNIT=IN  ,FILE='INPUT' ,STATUS='UNKNOWN')
      OPEN (UNIT=NOUT,FILE='OUTPUT',STATUS='UNKNOWN')
      OPEN (UNIT=IPUN,FILE='PUNCH' ,STATUS='UNKNOWN')
      OPEN (UNIT=LOUT,FILE='TAPE3' ,STATUS='UNKNOWN',ERR=15)
      GO TO 20
C
 15   STOP 'SCRATCH FILE ERROR, UNIT 3'
C
 20   WRITE  (NOUT,25) (F,I=1,7),MTYPE(MACH),(F,I=1,4)
 25   FORMAT (/////15X,A11, /14X,A11, 3(/13X,A11), /8X,2(3X,'FFFFFF'),
     1       2(/13X,A11),7X,A6,' VERSION / APRIL 93', /13X,A11,
     2       /12X,A11,10X,'COSMIC, (706) 542-3265', /11X,A11,11X,
     3       'UNIVERSITY OF  GEORGIA', /10X,A11,12X,'ATHENS, GEORGIA',
     4       '  30602')
 30   WRITE  (NOUT,35) J
 35   FORMAT (//,' *** ENTER A BLANK, ''HELP'', OR A FILE NAME (UP TO'
     1,       I3,' CHARACTERS)', /5X,'IF OUTPUT IS TO BE SAVED')
      READ (IN,40,ERR=330,END=330) FNAME
 40   FORMAT (4A8)
      IF (FNAME(1) .EQ. MYFILE) CALL FFHELP (*30,*600,4)
      IF (FNAME(1) .EQ. BLNK8) FNAME(1) = MYFILE
      IF (FN(J+1).NE.BK1 .OR. FN(J+2).NE.BK1 .OR. FN(J+3).NE.BK1)
     1   GO TO 330
      IF (FNAME(2) .NE. BLNK8) GO TO 50
      CALL UPCASE (FNAME,J)
      IF (FN6 .EQ. BEGN6) GO TO 330
      IF (FN6 .EQ. STOP6) GO TO 600
      IF (FN6 .EQ. HELP6) CALL FFHELP (*30,*600,1)
 50   ODNW = OLD
      IF (MACH .NE. VAX) GO TO 60
      DO 52 I = 2,J
      IF (FN(I) .EQ. DOT) GO TO 60
      IF (FN(I) .EQ. BK1) GO TO 55
 52   CONTINUE
      I = J + 1
 55   FN(I  ) = DOT
      FN(I+1) = D1
      FN(I+2) = A1
      FN(I+3) = T1
 60   IF (MACH .NE. IBM) GO TO 65
C
C     IBM ONLY, WE CALL SYSTEM ROUTINES
C         IQZDDN TO DETERMINE WHETHER FILES EXIST OR NOT
C         QQDCBF TO DYNAMICALLY BUILD AN ATTRIBUTE LIST BY DDNAME
C         QQGETF TO DYNAMICALLY ALLOCATE FILES IN TSO OR BATCH
C
      I = IQZDDN(FNAME(1))
      ODNW = OLD
      IF (I .EQ. 0) ODNW = NEW
      IF (ODNW .EQ. NEW) CALL QQDCBF (FNAME(1),0,'F  ',80,80,DA)
      CALL QQGETF (LU,FNAME(1),IERR)
      IF (IERR .NE. 0) GO TO 130
C
 65   IF (IBMCDC.EQ.0) OPEN (UNIT=LU,FILE=FNAME(1),STATUS=ODNW,ERR=130)
      IF (IBMCDC.NE.0) OPEN (UNIT=LU,FILE=FN32    ,STATUS=ODNW,ERR=130)
      IF (ODNW .EQ. NEW) GO TO 140
 70   WRITE  (NOUT,80)
 80   FORMAT (/,' FILE ALREADY EXISTS, ENTER ''STOP'', ''OVERWRITE'',',
     1        ' OR ''APPEND'' -')
      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
      READ (IN,90,END=70) X1
 90   FORMAT (A1)
      CALL UPCASE (X1,1)
      IF (X1 .EQ. S1) GO TO 480
      IF (X1 .EQ. W1) GO TO 140
      IF (X1 .NE. A1) GO TO 70
      SAVE(2) = BBK(1)
      SAVE(3) = BBK(2)
 110  READ (LU,180,END=115) SAVE
      IF (SAVE(1).EQ.BEGN .AND. SAVE(4).EQ.BLANK) FFFLAG = 1234
      IF (SAVE(19) .EQ. KSMB(IKI)) IKI = IKI + 1
      GO TO 110
 115  BACKSPACE LU
      IF (FFFLAG .EQ. 1234) WRITE (NOUT,120)
 120  FORMAT (/,' IF EXISTING FILE CONTAINS FREE-FIELD INPUT CARDS, ',
     1        ' THIS PROGRAM WILL NOT', /5X,'EXPAND THEM TO FIXED-',
     2        ' FIELD FORMATS',/)
      WRITE (NOUT,255) SAVE
      IF (FFFLAG.EQ.1234 .AND. INFLAG.EQ.0) CALL FFHELP (*125,*125,5)
 125  CARD(1) = SAVE(1)
      CARD(2) = SAVE(2)
      GO TO 140
 130  IF (ODNW .EQ. NEW) GO TO 310
      ODNW = NEW
      GO TO 60
 140  IF (MACH .EQ. UNIVAC) J = FACSF(USE)
      IF (FNAME(1) .EQ. MYFILE) FNAME(1) = BLNK8
      IF (FNAME(1) .EQ.  BLNK8) WRITE (NOUT,150)
 150  FORMAT (/5X,'*** OUTPUT NOT SAVED ***',//)
      WRITE  (NOUT,160)
 160  FORMAT (//,' *** NASTRAN FREE-FIELD INPUT PROGRAM ***',
     1 /5X,'(THERE WILL BE NO INPUT ECHO UNTIL ''BEGIN BULK'' IS TYPED',
     2 /5X,' TO TERMINATE JOB:  ENTER ''ENDDATA'' OR ''STOP'')',
     3 //5X,'PLEASE BEGIN -',/)
 170  CALL FFREAD (*320,CARD)
      IF (CARD4.EQ.CANC1 .AND. CARD(2).EQ.CANC2) GO TO 230
      IF (CARD4.EQ. LIST .AND. CARD(2).EQ.BLANK) GO TO 230
      IF (CARD2.EQ. REPL .AND. CARD(4).EQ.BLANK) GO TO 350
      IF (CARD4.EQ. STOP .AND. CARD(2).EQ.BLANK) GO TO 400
      IF (CARD4.EQ.BLANK .AND. CARD(2).EQ.BLANK) GO TO 370
      IF (CARD4.EQ. HELP .AND. CARD(2).EQ.BLANK)
     1   CALL FFHELP (*280,*400,2)
      IF (LU .EQ. 2) WRITE (LU,180) CARD
 180  FORMAT (20A4)
      IF (FFFLAG.NE.1234 .AND. INFLAG.EQ.4) WRITE (NOUT,190) CARD
 190  FORMAT (1X,20A4)
      IF (CARD4.EQ.BEGN .AND. CARD(5).EQ.BLANK) GO TO 340
      IF (CARD4.NE.END1 .AND. CARD4.NE.END2 .AND. CARD4.NE.END3)
     1   GO TO 170
      IF (CARD(2).EQ.BLANK .OR. CARD(2).EQ.ALTER) GO TO 170
      GO TO 410
 230  IF (LU .NE. 2) GO TO 290
      J = 1
      IF (CARD(5).EQ.CANC1 .AND. ICONT(1).GT.0) J = ICONT(1) + 1
      IF (CARD(5).EQ. LIST .AND. ICONT(1).GT.0) J = ICONT(1)
      DO 240 I = 1,J
 240  BACKSPACE LU
      ICONT(1) = 0
      IF (CARD(5) .EQ. LIST) GO TO 260
      READ (LU,180) SAVE
      J = J - 1
      WRITE  (NOUT,250) J
 250  FORMAT (1X,I4,' PREVIOUSLY GENERATED CARDS CANCELLED ***')
      IF (J .GT. 0) WRITE (NOUT,255) SAVE
 255  FORMAT (/,' *** LAST CARD WAS:', /1X,20A4)
      GO TO 280
 260  WRITE  (NOUT,265) J
 265  FORMAT (//,' *** PREVIOUS',I4,' CARDS WERE (COLS. 1-79) -',/)
      DO 270 I = 1,J
      READ (LU,180,END=285) SAVE
 270  WRITE  (NOUT,275) SAVE
 275  FORMAT (1X,20A4)
 280  CARD(1) = SAVE(1)
      CARD(2) = SAVE(2)
      GO TO 170
 285  BACKSPACE LU
      SAVE(1) = CARD(1)
      SAVE(2) = CARD(2)
      GO TO 170
 290  WRITE  (NOUT,300) CARD4,CARD(2)
 300  FORMAT (' *** ',A4,A3,'OPTION NOT ACTIVE.  NO SAVE FILE ',
     1        'REQUESTED')
      GO TO 170
 310  WRITE  (NOUT,315) FNAME
 315  FORMAT (' *** CAN NOT ASSIGN FILE - ',4A8)
      GO TO 20
 320  WRITE  (NOUT,325)
 325  FORMAT (' *INPUT ERROR/FF*')
      GO TO 170
 330  WRITE  (NOUT,335)
 335  FORMAT (' *NOT A VALID FILE NAME*')
      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
      GO TO 20
 340  FFFLAG = 1234
      IF (INFLAG .EQ. 0) CALL FFHELP (*170,*170,5)
      GO TO 170
C
C ... EDIT PREVIOUS LINE BY R:n)xxx
C
 350  I = 3
      IF (CARD1(5) .EQ. RPRN) I = 4
      READ (CARD1(I),355,ERR=170) II
 355  FORMAT (I1)
      IF (II .EQ. 0) II = 10
      I = I + 2
      DO 360 J = 1,8
      SPL(J) = CARD1(I)
 360  I = I + 1
      SAVE(II) = SPILL
      DO 365 I = 1,10
 365  CARD(I) = SAVE(I)
      BACKSPACE LU
      WRITE (LU,  180) SAVE
      WRITE (NOUT,190) SAVE
      GO TO 170
C
C ... FIRST 2 FIELDS ARE BLANK, OTHER FIELDS NOT, TREAT IT AS A
C     CONTINUATION CARD IF PREVIOUS CARD HAS A BLANK CONTINUATION FIELD
C
 370  IF (LU .NE. 2) GO TO 385
      DO 375 J = 3,18
      IF (CARD(J) .NE. BLANK) GO TO 380
 375  CONTINUE
      GO TO 170
 380  BACKSPACE LU
      READ (LU,180) SAVE
      IF (SAVE(19) .EQ. BLANK) GO TO 390
      WRITE  (NOUT,382)
 382  FORMAT (/,' BAD INPUT - FIRST FIELD BLANK. TRY AGAIN')
      WRITE  (NOUT,383)
 383  FORMAT (13X,'NOT ALLOW. PREVIOUS CARD HAS CONTINUATION FIELD ',
     1        'DEFINED')
      GO TO 170
 385  WRITE  (NOUT,382)
      WRITE  (NOUT,387)
 387  FORMAT (13X,'NOT ALLOW WITHOUT SAVE FILE')
      GO TO 170
 390  KONTN = KONTN + 1
      IF (MOD(KONTN,10000) .EQ. 0) IKI = IKI + 1
      CALL INT2K8 (*385,KONTN,SAVE(19))
      SAVE(19) = KSMB(IKI)
      CARD(1 ) = KSMB(IKI)
      CARD(2 ) = SAVE(20)
      WRITE  (NOUT,395)
 395  FORMAT (' ...PREVIOUS CARDS REPLACED BY:')
      WRITE (NOUT,190) SAVE
      WRITE (NOUT,190) CARD
      BACKSPACE LU
      WRITE (LU,180) SAVE
      WRITE (LU,180) CARD
      GO TO 170
C
 400  BACKSPACE LOUT
 410  ENDFILE LOUT
      IF (LU .NE. 6) ENDFILE LU
      PUNCH =.FALSE.
      SITE  = BLNK8
 420  WRITE  (NOUT,430) QMARK
 430  FORMAT (/,' *** DO YOU WANT TO PUNCH OUT THE NASTRAN DECK',A1,
     1       ' (Y,N,X,HELP) ')
      IF (MACH.EQ.CDC .AND. IN.EQ.5) REWIND IN
      READ (IN,90,END=420) X1
      CALL UPCASE (X1,1)
      IF (X1 .EQ. H1) CALL FFHELP (*420,*480,3)
      IF (X1 .EQ. N1) GO TO 500
      IF (X1.NE.Y1 .AND. X1.NE.FX) GO TO 420
      PUNCH =.TRUE.
      LX = LOUT
      IF (X1 .EQ. FX) LX = LU
      IF (MACH .NE. UNIVAC) GO TO 460
      IPUN = 1
      WRITE  (NOUT,440)
 440  FORMAT (/,' *** ENTER SITE-ID, OR ''CENTRAL'', WHERE CARDS ARE',
     1       ' TO BE PUNCHED ')
 450  READ (IN,40,ERR=450,END=450) SITE
      IF (SITE .EQ. BLNK8) GO TO 490
      CALL UPCASE (SITE,8)
      IF (SITE .EQ. CNTRL) GO TO 460
C
C     SEND PUNCH DECK TO SITE AS REQUESTED BY THE USER - UNIVAC ONLY
C-UNV FACSF IS UNIVAC SYSTEM FUNCTION
C
      SYM(3) = SITE
      J = FACSF(SYM)
 460  REWIND LX
 470  READ (LX,180,END=500) CARD
      DO 475 J = 2,NCNTL
      IF (CARD6 .NE. CNTLWD(J)) GO TO 475
      IF (CARD(4) .EQ. BLANK) GO TO 470
 475  CONTINUE
      IF ((CARD6.EQ.HELP6 .OR. CARD6.EQ.STOP6) .AND. CARD(3).EQ.BLANK)
     1   GO TO 470
      IF (CARD6.EQ.CNTLWD(1) .AND. CARD(4).EQ.BLANK) ICNTL = ICNTL + 1
      WRITE (IPUN,180) CARD
      GO TO 470
C
 480  FNAME(1) = BLNK8
 490  PUNCH    =.FALSE.
 500  WRITE  (NOUT,510)
 510  FORMAT (//10X,'ADIEU MY FRIEND.  IT IS A PLEASURE TO SERVE YOU')
      IF (FNAME(1) .NE. BLNK8) WRITE (NOUT,520) FNAME
 520  FORMAT (10X,'DON''T FORGET - YOUR NASTRAN DECK IS IN FILE -',
     1    /25X,4A8, /10X,'WHICH IS ACCESSIBLE BY THE SYSTEM EDITOR')
      IF (.NOT.PUNCH) GO TO 550
      WRITE  (NOUT,530)
 530  FORMAT (/10X,'AND DON''T FORGET TO PICK UP YOUR PUNCHED CARDS')
      IF (SITE .NE. BLNK8) WRITE (NOUT,535) SITE
      IF (SITE.EQ.BLNK8 .AND. MACH.NE.VAX) WRITE (NOUT,540)
      IF (SITE.EQ.BLNK8 .AND. MACH.EQ.VAX) WRITE (NOUT,545)
 535  FORMAT (10X,'WHEN YOU SIGN OFF',22X,'SITE-ID: ',A8)
 540  FORMAT (10X,'AT THE CENTRAL-SITE')
 545  FORMAT (10X,'IN FORTRAN FILE FOR007.DAT')
 550  IF (FNAME(1) .EQ. BLNK8) GO TO 570
      IF (MACH .EQ. UNIVAC) FOROO3 = TPF
      IF (MACH .EQ.    CDC) FOROO3 = TAPEO3
      WRITE  (NOUT,555)
 555  FORMAT (//10X,'A COPY OF YOUR ACTUAL INPUT CARDS WAS SAVED IN')
      IF (MACH .NE. VAX) WRITE (NOUT,560) FOROO3
      IF (MACH .EQ. VAX) WRITE (NOUT,565)
 560  FORMAT (1H+,56X,'FORTRAN FILE - ',A8)
 565  FORMAT (10X,'FORTRAN FILE FOR003.DAT')
 570  IF (ICNTL .NE. 0) WRITE (NOUT,575)
 575  FORMAT (/4X,'*** WARNING - CANCELLED CARDS IN PUNCHED DECK NEED ',
     1       'TO BE REMOVED', /19X,'OR MODIFIED BEFORE USE')
      IF (FNAME(1) .NE. BLNK8) DORK = KEEP
      CLOSE (UNIT=LU  ,STATUS=DORK)
      CLOSE (UNIT=LOUT,STATUS=DORK)
      IF (DORK.EQ.KEEP .OR. (PUNCH .AND. MACH.EQ.VAX)) WRITE (NOUT,585)
 585  FORMAT (/4X,'*** DON''T FORGET TO DELETE YOUR FILES GENERATED BY',
     1       ' THIS RUN ***')
      WRITE  (NOUT,590)
 590  FORMAT (/26X,'*** JOB DONE ***',/)
 600  CONTINUE
      END
