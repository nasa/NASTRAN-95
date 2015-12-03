      SUBROUTINE SCAN        
C        
C     THIS IS THE MAIN DRIVER FOR THE OUTPUT SCAN MODULE - SCAN        
C        
C     THIS SCAN MODULE CAN BE CALLED DIRECTLY FROM ALL RIGID FORMATS, OR
C     BY USER DMAP ALTER. THE CALLING INSTRUCTIONS ARE        
C        
C     (THREE INPUT FILES IF CALLED BY RIGID FORMAT VIA SCAN INPUT CARDS)
C     (1) FORCE AND STRESS SCAN -        
C     SCAN  CASECC,OESI,OEFI/OESFI/*RF*  $    (WHERE I=1, OR 2)        
C           OR        
C     SCAN  CASECC,OESI,OEFI/OESFI/*OLI* $    FOR ON-LINE SCAN        
C        
C         . IF INPUT FILES ARE OES1, OEF1, SORT1 TYPE DATA ARE SCANNED  
C         . IF INPUT FILES ARE OES2, OEF2, SORT2 TYPE DATA ARE SCANNED  
C        
C     (ONE INPUT FILE ONLY IF CALLED BY USER VIA DMAP ALTER)        
C     (2) STRESS SCAN -        
C     SCAN, ,OESI, /OESFI/C,N,ELEMENT/C,N,ICOMP/C,N,NTOP/C,N,AMAX/      
C           C,N,AMIN/C,N,IBEG/C,N,IEND/C,N,ICOMPX $        
C     OR (3) FORCE SCAN -        
C     SCAN, ,,OEFI /OESFI/C,N,ELEMENT/C,N,ICOMP/C,N,NTOP/C,N,AMAX/      
C           C,N,AMIN/C,N,IBEG/C,N,IEND/C,N,ICOMPX $        
C        
C         . FOR SORT1 TYPE DATA, OESI AND OEFI ARE OES1 AND OEF1, AND   
C           IBEG AND IEND ARE RANGE OF ELEMENT IDS TO BE SCANNED        
C         . FOR SORT2 TYPE DATA, OESI AND OEFI ARE OES2 AND OEF2, AND   
C           IBEG AND IEND ARE RANGE OF SUBCASE IDS TO BE SCANNED        
C         . IF IBEG AND IEND ARE NOT GIVEN, ALL IDS IMPLY        
C        
C         . OESB1, OESC1, OEFB1, AND OEFC1 CAN BE USED IN LIEU OF OES1  
C           AND OEF1. SIMILARLY, OESC2 AND OEFC2  FOR OES2 AND OEF2     
C        
C     INPUT  FILES  - CASECC, OES1, OEF1, (OR OES2, OEF2)        
C                     (OESB1, OESC1, OEFB1, OEFC1, OESB2, OEFB2 CAN BE  
C                     USED INSTEAD)        
C     OUTPUT FILE   - OESF1 (OR OESF2)        
C     SCRATCH FILE  - SCR1        
C        
C     THIS SCAN MODULE SHOULD BE FOLLOWED BY OFP TO PRINT SCAN RESULTS  
C     OFP  OESFI,,,,, //S,N,CARDNO $        
C        
C     PARAMETERS -        
C        
C           ELEMENT - ELEMENT NAME IN BCD.  E.G. BAR, CBAR, QUAD2, ETC. 
C           ICOMP   - THE OUTPUT FIELD NO. (BY COLUMN, 1 THRU 31) OF    
C                     OUTPUT LISTING.        
C           ICOMPX  - OUTPUT FIELD NO. CONTINUATION (FROM 32 THRU 62)   
C           NTOP    - TOP N VALUES TO BE OUTPUT.  DEFAULT=20        
C      AMAX-AMIN    - SCAN VALUES OUTSIDE THIS MAX-MIN RANGE, DEFAULT=0.
C      IBEG,IEND    - SEE EXPLANATION ABOVE        
C        
C     DEFINITION OF SOME LOCAL VARIABLES        
C        
C           DEBUG   - USED FOR LOCAL DEBUG        
C           S OR F  - STRESS OR FORCE SCAN FLAG        
C           NSCAN   - NO. OF SCAN INPUT CARDS IN CASECC        
C           SUBC    - CURRENT SUBCASE ID        
C           NZ      - TOP OF OPEN CORE, JUST BELOW GINO BUFFERS        
C           LCORE   - AVAILABLE CORE FOR STRSCN ROUTINE        
C           IOPEN   - INPUT  FILE STATUS FLAG, .T. FOR OPEN, .F. NOT    
C           JOPEN   - OUTPUT FILE STATUS FLAG, .T. FOR OPEN, .F. NOT    
C           KOPEN   - SCR1   FILE STATUS FLAG, .T. FOR OPEN, .F. NOT    
C           LOPEN   - CASECC FILE STATUS FLAG, .T. FOR OPEN, .F. NOT    
C           ISET    - SCAN ONLY BY THE SPECIFIED SET OF ELEM. IDS       
C                   - ALL IS IMPLIED IF ISET IS NOT GIVEN        
C                   - USED ONLY IF SCAN IS CALLED FROM RIGID FORMAT     
C      IDUPL,INC    - SET UP COMPONENT FIELDS TO BE REPEATEDLY SCANNED  
C                     IDUPL TIMES, WITH FIELD INCREMENT BY INC (RF ONLY)
C      LBEG,LEND    - A LIST OF TO-BE-SCANNED ELEMENT IDS, STORED IN    
C                     Z(LBEG) THRU Z(LEND).        
C                   - NO SUCH LIST EXISTS IF LBEG.GT.LEND OR LBEG=LEND=0
C           IOPT    - DATA SCAN BY AMAX AND AMIN IF IOPT=1, BY NTOP IF 2
C           ISORT   - SET TO 1 (BY STRSCN) IF DATA TYPE IS IN SORT1     
C                     FORMAT, AND SET TO 2 IF SORT2        
C        
C     WRITTEN BY G.CHAN/SPERRY      OCTOBER 1984        
C        
C     THIS ROUTINE OPENS AND CLOSES ALL INPUT AND OUTPUT FILES.        
C     IT SETS UP THE SCANNING PARAMETERS AND CALL STRSCN TO SCAN THE    
C     OUTPUT STRESS OR FORCE DATA        
C        
C     THE SCAN INPUT CARDS OPERATE VERY SIMILARY TO THE ELEMENT STRESS  
C     OR FORCE CARDS. THEY CAN BE PLACED ABOVE ALL SUBCASES, OR INSIDE  
C     ANY SUBCASE LEVEL, OR BOTH        
C     HOWEVER, UNLIKE THE STRESS OR FORCE CARDS, MULTI-SCAN CARDS ARE   
C     ALLOWED, AND THEY DO NOT EXCLUDE ONE ANOTHER.        
C        
C     MODIFIED IN 10/1989, TO ALLOW SETS TO BE DEFINED BEFORE OR AFTER  
C     SCAN CARDS IN CASE CONTROL SECTION        
C     (CURRENTLY, THIS MODIFICATION IS OK, BUT IFP1/IFP1H DO NOT ALLOW  
C     SET TO BE DEFINED AFTER SCAN. IN FACT, IFP1 DOES NOT ALLOW SET TO 
C     BE DEFINED AFTER ANY GUY WHO USES THE SET)        
C        
      LOGICAL         DEBUG,    IOPEN,    JOPEN,    KOPEN,    LOPEN     
CWKBI  1/4/94 SPR93010 & 93011
      LOGICAL         STRESS,   FORCE,    LAYERD
CWKBI  1/4/94 SPR93010 & 93011
      INTEGER         QUAD4,    TRIA3
CRLBR 12/29/93 SPR 93010 & 93011
C     INTEGER         CASECC,   OESI,     OEFI,     OESFI,    SCR1,     
      INTEGER         CASECC,   OESI(2),  OEFI(2),  OESFI(2), SCR1,     
     1                OUFILE,   FILE,     SORF,     Z(166),   NAM(2),   
     2                E,        EOR,      SUBC,     OSUBC,    OEL       
CRLBNB 12/29/93 SPR 93010 & 93011
      INTEGER         JELT(2)
CRLBNE 12/29/93 SPR 93010 & 93011
     
      CHARACTER       UFM*23,   UWM*25,   UIM*29,   SFM*25,   SWM*27    
      COMMON /XMSSG / UFM,      UWM,      UIM,      SFM,      SWM       
      COMMON /BLANK / IELT(2),  ICOMP,    NTOP,     AMAX,     AMIN,     
     1                IBEG,     IEND,     ICOMPX        
      COMMON /SYSTEM/ IBUF,     NOUT,     SKP(83),  INTRA        
      COMMON /NAMES / RD,       RDREW,    WRT,      WRTREW,   REW,      
     1                NOREW,    EOFNRW        
      COMMON /GPTA1 / NELEM,    LAST,     INCR,     E(1)        
      COMMON /XSCANX/ INFILE,   OUFILE,   LCORE,    LBEG,     LEND,     
     1                IOPEN,    JOPEN,    IEL,      IOPT,     ISET,     
     2                ISORT,    ITRL3,    SUBC,     OSUBC,    OEL,      
CWKBR 1/4/94 SPR93010 & 93011     3                DEBUG          
     3                DEBUG,    LLOOP,    QUAD4,    TRIA3,    STRESS,
     4                FORCE,    LAYERD
      COMMON /ZZZZZZ/ CORE(1)        
      EQUIVALENCE     (IMAX,AMAX),        (IMIN,AMIN),        
     1                (IDUPL,IBEG),       (INC,IEND),        
     2                (CORE(1),Z(1))        
CRLBDB 12/29/93 SPR 93010 & 93011
C     DATA            CASECC,   OESI,     OEFI,     OESFI,    SCR1    / 
C    1                101,      102,      103,      201,      301     / 
CRLBDE 12/29/93 SPR 93010 & 93011
CRLBNB 12/29/93 SPR 93010 & 93011
      DATA            CASECC,   OESI(1),  OEFI(1),  OESI(2),  OEFI(2),   
     1                OESFI(1), OESFI(2), SCR1    / 
     2                101,      102,      103,      104,      105,
     3                201,      202,      301     / 
CRLBNE 12/29/93 SPR 93010 & 93011
      DATA            NAM,                LLC,      EOR,      IRF     / 
     1                4HSCAN,   4H    ,   4HC   ,   1,        4HRF    / 
      DATA            IOL1,     IOL2      /        
     1                4HOL1 ,   4HOL2     /        
C        
      DEBUG = .FALSE.        
CWKBNB 1/4/94 SPR93011 & 93010
      QUAD4 = 0
      TRIA3 = 0
C        
C     ALLOCATE OPEN CORE        
C        
CRLBNB 12/29/93 SPR 93010 & 93011
      LLOOP = 1
      JELT(1) = IELT(1)
      JELT(2) = IELT(2)
  10  CONTINUE
CRLBNB 12/29/93 SPR 93010 & 93011
      NZ    = KORSZ(Z)        
      IBUF1 = NZ - IBUF + 1        
      IBUF2 = IBUF1 - IBUF        
      IBUF3 = IBUF2 - IBUF        
      NZ    = IBUF3 - 1        
      LCORE = IBUF2 - 1        
      IOPEN =.FALSE.        
      JOPEN =.FALSE.        
      KOPEN =.FALSE.        
      LOPEN =.FALSE.        
C        
C     OPEN CASECC AND CHECK SCAN DATA        
C        
      ISET = 0        
      IF (IELT(1) .NE. IRF) ISET = -2        
      IF (IELT(1).EQ.IOL1 .OR. IELT(1).EQ.IOL2) ISET = -3        
      IF (ISET .EQ. -2) GO TO 40        
      FILE = CASECC        
      CALL OPEN (*310,CASECC,Z(IBUF1),RDREW)        
      LOPEN = .TRUE.        
      CALL FWDREC (*320,CASECC)        
      IF (ISET .EQ. -3) GO TO 40        
 30   CALL READ (*80,*80,CASECC,Z(1),200,1,L)        
      LENCC = Z(166)        
      NSCAN = Z(LENCC-1)        
      IF (NSCAN .EQ. 0) GO TO 30        
C        
C     CHECK THE PRESENCE OF STRESS AND/OR FORCE FILE.        
C     QUIT IF BOTH ARE PURGED        
C        
 40   IOES  = 1        
      IOEF  = 1        
CRLBDB 12/29/93 SPR 93010 & 93011
C     Z( 1) = OESI        
C     Z(11) = OEFI        
CRLBDE 12/29/93 SPR 93010 & 93011
CRLBNB 12/29/93 SPR 93010 & 93011
      Z( 1) = OESI(LLOOP)        
      Z(11) = OEFI(LLOOP)        
CRLBNE 12/29/93 SPR 93010 & 93011
      CALL RDTRL (Z( 1))        
      CALL RDTRL (Z(11))        
      IF (Z( 1) .LT. 0) IOES = 0        
      IF (Z(11) .LT. 0) IOEF = 0        
      IF (IOES+IOEF.EQ.0 .AND. ISET.NE.-3) GO TO 300        
C        
C     OPEN OUTPUT FILE OESFI        
C        
CRLBDB 12/29/93 SPR 93010 & 93011
C     FILE = OESFI        
C     OUFILE = OESFI        
C     CALL FNAME (OESFI,Z)        
C     CALL OPEN  (*310,OESFI,Z(IBUF2),WRTREW)        
C     CALL WRITE (OESFI,Z,2,EOR)        
CRLBDE 12/29/93 SPR 93010 & 93011
CRLBNB 12/29/93 SPR 93010 & 93011
      FILE = OESFI(LLOOP) 
      OUFILE = OESFI(LLOOP) 
      CALL FNAME (OUFILE,Z)        
      CALL OPEN  (*310,OUFILE,Z(IBUF2),WRTREW)        
      CALL WRITE (OUFILE,Z,2,EOR)        
CRLBNE 12/29/93 SPR 93010 & 93011
      JOPEN  =.TRUE.        
      ITRL3  = 0        
      LX     =-1        
      IF (IELT(1) .EQ. IOL2) LX = -2        
      IF (ISET .EQ. -3) CALL ONLINS (*280,LX)        
      IF (ISET .NE. -2) GO TO 90        
C        
C     SCAN CALLED BY USER VIA DMAP ALTER (ISET=-2)        
C     ============================================        
C        
      LS   = LCORE        
      LBEG = 0        
      LEND = 0        
C        
C     CHECK USER DMAP ERROR, SET IOPT FLAG, AND INITIALIZE ISCAN ARRAY  
C     FOR COMPONENT SPECIFIED.        
C        
      IF (IOES+IOEF .GT. 1) GO TO 400        
      IF (AMIN  .GT.  AMAX) GO TO 410        
      IF (ICOMP .LE.     1) GO TO 420        
      IF ((AMAX.EQ.0. .AND. AMIN.EQ.0.) .AND. NTOP.EQ.0) GO TO 430      
      IF ((AMAX.NE.0. .OR.  AMIN.NE.0.) .AND. NTOP.NE.0) GO TO 440      
      IF ((IBEG.EQ.0 .AND. IEND.NE.0) .OR. IBEG.GT.IEND .OR.        
     1    (IBEG.NE.0 .AND. IEND.EQ.0)) GO TO 460        
      IF ( IBEG.EQ.0 .AND. IEND.EQ.0 ) IBEG = -1        
      IOPT = 1        
      IF (NTOP .GT. 0) IOPT = 2        
C        
C     DETERMINE ELEMENT TYPE, DROP THE FIRST LETTER C IF NECESSARY      
C        
      Z(1) = IRF        
      Z(2) = IRF        
      IF (KHRFN2(IELT(1),1,1) .NE. LLC) GO TO 50        
      Z(1) = KHRFN3(NAM(2),IELT(1),1,1)        
      Z(1) = KHRFN1(Z(1),4,IELT(2),1  )        
      Z(2) = KHRFN3(NAM(2),IELT(2),1,1)        
 50   DO 60 I = 1,LAST,INCR        
      IF (IELT(1).EQ.E(I) .AND. IELT(2).EQ.E(I+1)) GO TO 70        
      IF (   Z(1).EQ.E(I) .AND.    Z(2).EQ.E(I+1)) GO TO 70        
 60   CONTINUE        
      GO TO 450        
 70   IEL = E(I+2)        
C        
C     SPECIAL HANDLING OF THE QUAD4 AND TRIA3 ELEMENT, STRESS ONLY      
C     (THE 2ND, 3RD, 9TH, AND 13TH WORDS IN OES1/OES1L FILES ARE        
C     NOT PRINTED. THE 9TH AND 13TH WORDS MAY BE BLANKS OR ASTERISKS)   
C        
      IF ((IEL.NE.64 .AND. IEL.NE.83) .OR. IOES.EQ.0) GO TO 75        
CWKBD 1/3/94 SPR93011 & 93011      ICOMP = ICOMP + 2       
CWKBD 1/3/94 SPR93010 & 93011      IF (ICOMP .GT. 8) ICOMP = ICOMP + 1        
C        
C     OPEN INPUT FILE        
C        
CRLBDB 12/29/93 SPR 93010 & 93011
C75   INFILE = OESI        
C     IF (IOES .EQ. 0) INFILE = OEFI        
CRLBDE 12/29/93 SPR 93010 & 93011
CRLBNB 12/29/93 SPR 93010 & 93011
 75   INFILE = OESI(LLOOP)        
      STRESS = .TRUE.
      FORCE  = .FALSE.
      IF (IOES .NE. 0) GO TO 76
      STRESS = .FALSE.
      FORCE  = .TRUE.
      INFILE = OEFI(LLOOP)        
CRLBNE 12/29/93 SPR 93010 & 93011
 76   FILE = INFILE        
      CALL OPEN (*340,INFILE,Z(IBUF1),RDREW)        
      IOPEN = .TRUE.        
C        
C ... NEXT I/O OPERATION ON INFILE WILL BE IN SUBROUTINE STRSCN        
C        
C     ALL SET TO GO        
C        
      J = 1        
      IF (IOES .EQ. 0) J = 2        
      CALL STRSCN (J)        
      GO TO 280        
C        
 80   CALL CLOSE (CASECC,REW)        
      LOPEN = .FALSE.        
      RETURN        
C        
C        
C     SCAN IS CALLED BY RIGID FORMAT (ISET .GE. -1)        
C     OR CALLED BY INTERACTIVE MODE  (ISET .EQ. -3)        
C     =============================================        
C        
 90   LS = NZ        
C        
C     OPEN SCR1 FILE, SEPERATE SCAN DATA FROM SET DATA IN CASECC, AND   
C     SAVE THE COMPLETE SCAN DATA IN SCR1 FILE.        
C        
      FILE = SCR1        
      CALL OPEN (*310,SCR1,Z(IBUF3),WRTREW)        
      KOPEN =.TRUE.        
      NSCAN = 0        
      NCASE = 0        
      NXX   = NZ        
      IF (INTRA .LE. 0) GO TO 95        
      NXX = 198        
      L   = LX        
      IF (LX .GT. 0) GO TO 110        
 95   FILE = CASECC        
      CALL REWIND (CASECC)        
      CALL FWDREC (*320,CASECC)        
C        
C     READ CASECC AND PROCESS ALL SUBCASES        
C        
 100  CALL READ (*210,*110,CASECC,Z(1),NXX,1,L)        
      IF (NXX .GE. 200) GO TO 380        
 110  NCASE = NCASE + 1        
      LENCC = Z(166)        
      NSCAN = Z(LENCC-1)        
      LSEM  = Z(LENCC)        
      SUBC  = Z(1)        
C        
C     PICK UP ALL THE SET ID'S AND THEIR LOCATIONS IN Z ARRAY, Z(L1)    
C     THRU Z(LL). SORT, AND CHECK DUPLICATE        
C        
      JMP = 0        
      II  = LENCC + LSEM        
      L1  = L + 1        
      LL  = L        
 115  II  = II + JMP        
      IF (II .GE. L) GO TO 120        
      JMP = Z(II+2) + 2        
      IF (Z(II+1).GE.10000000 .AND. JMP.EQ.8) GO TO 115        
      Z(LL+1) = Z(II+1)        
      Z(LL+2) = II        
      LL = LL + 2        
      GO TO 115        
 120  LLL1 = LL - L1 + 1        
      LL2  = LLL1/2        
      IF (DEBUG) WRITE (NOUT,125) (Z(I),I=L1,LL)        
 125  FORMAT (' ...SET/@125',/,(10X,I8,' @',I6))        
C        
      JMP = 0        
      II  = LENCC + LSEM        
      KK  = NZ        
      IF (LL2 .LE. 1) GO TO 140        
      CALL SORT (0,0,2,1,Z(L1),LLL1)        
      J = L1 + 2        
      DO 130 I = J,LL,2        
      IF (Z(I) .EQ. Z(I-2)) WRITE (NOUT,600) UWM,Z(I)        
 130  CONTINUE        
C        
C     PROCESS THE SCAN CARDS        
C        
C     PICK UP SCAN 8 WORD ARRAY, AND PICK UP SET DATA        
C     WRITE TO SCR1 A RECORD (OF EACH SUBCASE) OF THE SCAN INPUT DATA   
C     IN REVERSE ORDER (FIRST SCAN CARD LAST, AS SET UP BY CASECC)      
C        
 140  II = II + JMP        
      IF (II .GE. L) GO TO 190        
      JMP = Z(II+2) + 2        
      IF (Z(II+1).LT.10000000 .OR. JMP.NE.8) GO TO 140        
      IE  = 0        
      ISET= Z(II+4)        
      IF (ISET .EQ. -1) GO TO 160        
      IF (LLL1 .LE.  0) GO TO 470        
      CALL BISLOC (*470,ISET,Z(L1),2,LL2,I)        
      IB = Z(I+L1) + 2        
      IE = Z(IB)        
      IF (DEBUG) WRITE (NOUT,145) ISET,I,IB,IE        
 145  FORMAT (' @145, SET',I8,' FOUND.  I,IB,IE =',3I6)        
      KK = KK - IE        
      DO 150 I = 1,IE        
 150  Z(KK+I) = Z(IB+I)        
 160  KK = KK - 9        
      DO 170 I = 1,8        
 170  Z(KK+I) = Z(II+I)        
      Z(KK+9) = 0        
      IDUPL   = Z(KK+8)        
      IF (IDUPL .EQ. 0) GO TO 180        
CWKBD 1/3/94 SPR93010 & 93011      INC = IDUPL/100        
CWKBD 1/3/94 SPR93010 & 93011      Z(KK+8) = MOD(IDUPL,100)        
CWKBNB 1/3/94 SPR93010 & 93011
      INC = MOD ( IDUPL, 100 )
      Z(KK+8) = IDUPL / 100
CWKBNE 1/3/94 SPR93010 & 93011
      Z(KK+9) = INC        
 180  Z(KK+2) = Z(KK+2) + 1 + IE        
C        
C     HERE AT THE TAIL END OF OPEN CORE, WE ACCUMULATE ANOTHER RECORD   
C     OF A SCAN DATA SET        
C        WORD 1,  10000000 FOR STRESS, OR 20000000 FOR FORCE        
C             2,  NO. OF WORDS OF THIS DATA SET (SCAN + SET)        
C                 (FIRST 2 WORDS NOT INCLUDED)        
C             3,  ELEMENT TYPE NUMERIC CODE        
C             4,  SET-ID, OR -1        
C             5,  COMPONENT CODE, ICOMP        
C             6,  NTOP, OR AMAX        
C             7,  -1,   OR AMIN        
C             8,  COMPONENT - DUPLICATION, OR ZERO        
C             9,  COMPONENT - INCREMENT,   OR ZERO        
C        10-END,  SET DATA        
C     REPEAT FOR ANOTHER SCAN CARD        
C        
C        
C     SPECIAL HANDLING OF THE QUAD4 AND TRIA3 ELEMENT, STRESS ONLY      
C     (THE 2ND, 3RD, 9TH,  AND 13TH WORDS IN OES1/OES1L FILES ARE       
C     NOT PRINTED. THE 9TH AND 13TH WORDS MAY BE BLANKS OR ASTERISKS)
CWKBI 12/93 SPR93010 & 93011
C     ABOVE IS TRUE ONLY FOR LAMINATED QUAD4 AND TRIA3)   
C        
CWKBD 12/31/93 SPR93010 & 93011     
C     IF ((Z(KK+3).NE.64 .AND. Z(KK+3).NE.83) .OR. Z(KK+1).NE.10000000) 
      IF ((Z(KK+3).NE.64 .AND. Z(KK+3).NE.83) .OR. Z(KK+8).EQ.0) 
     1    GO TO 140        
CWKBDB 1/3/94 SPR93010 & 93011
C      Z(KK+5) = Z(KK+5) + 2        
C      IF (Z(KK+5) .GT. 8) Z(KK+5) = Z(KK+5) + 1        
C      IF (Z(KK+9) .NE. 0) Z(KK+9) = Z(KK+9) + 2        
CWKBDE 1/3/94 SPR93010 & 93011
      GO TO 140        
C        
C     AT THE END OF EACH SUBCASE, WE COMPUTE THE TOTAL LENGTH OF THIS   
C     SCAN DATA ARRAY, AND WRITE THE ARRAY OUT TO SCR1.  ONE RECORD PER 
C     SUBCASE        
C        
 190  KK = KK - 2        
      IF (KK .LT. LL) GO TO 610        
      IE = NZ - KK        
      Z(KK+1) = SUBC        
      Z(KK+2) = IE - 2        
      CALL WRITE (SCR1,Z(KK+1),IE,1)        
      L  = KK + 1        
      NN = 200        
      IF (DEBUG) WRITE (NOUT,200) NN,(Z(J),J=L,NZ)        
 200  FORMAT (/,11H SCAN/DEBUG,I3,  (/2X,13I9))        
      IF (INTRA.LE.0 .OR. LX.LT.200) GO TO 100        
C        
C     THUS, END OF THE PREPARATION PHASE.  CLOSE CASECC AND SCR1        
C        
 210  CALL CLOSE (CASECC,REW)        
      CALL CLOSE (SCR1  ,REW)        
      KOPEN =.FALSE.        
      LOPEN =.FALSE.        
C        
C     NOW, SET UP 2 LOOPS FOR STRESS (10000000) AND FORCE (20000000)    
C     OUTPUT SCAN        
C        
      SORF = 30000000        
 220  SORF = SORF - 10000000        
      IF (DEBUG) WRITE (NOUT,225) SORF        
 225  FORMAT (///,18H PROCESSING SERIES,I15 /1X,8(4H====),/)        
      IF (IOPEN) CALL CLOSE (INFILE,REW)        
      IOPEN = .FALSE.        
      IF (SORF.EQ.10000000 .AND. IOES.EQ.0) GO TO 220        
      IF (SORF.EQ.20000000 .AND. IOEF.EQ.0) GO TO 220        
      IF (SORF .LE. 0) GO TO 280        
C        
C     OPEN INPUT FILES        
C        
CRLBDB 12/29/93 SPR 93010 & 93011
C     INFILE = OESI        
C     IF (SORF .GE. 20000000) INFILE=OEFI        
CRLBDE 12/29/93 SPR 93010 & 93011
CRLBNB 12/29/93 SPR 93010 & 93011
      INFILE = OESI(LLOOP)        
      STRESS = .TRUE.
      FORCE  = .FALSE.
      IF (SORF .LT. 20000000) GO TO 226
      STRESS = .FALSE.
      FORCE  = .TRUE.
      INFILE=OEFI(LLOOP)        
CRLBNE 12/29/93 SPR 93010 & 93011
226   FILE = INFILE        
      CALL OPEN (*310,INFILE,Z(IBUF1),RDREW)        
      IOPEN = .TRUE.        
C ... NEXT I/O OPERATION ON INFILE WILL BE IN SUBROUTINE STRSCN        
C        
C     NOW, LOAD THE SCAN DATA PREVIOUSLY SAVED IN SCR1, TO THE TAIL END 
C     OF THE OPEN CORE.        
C     ONE OR MORE SCAN CARDS MAY BE PRESENT IN  ONE SUBCASE        
C     SET UP POINTERS IN FRONT OF THE SCAN DATA, SO THAT FIRST SCAN     
C     INPUT CARD WILL BE PROCESS FIRST, SECOND CARD SECOND, ETC.        
C     NOTE - USE SUBCASE 1 SCAN DATA IF OUTPUT IS SORT 2 TYPE        
C            (IF SUBCASE 1 DOES NOT HAVE SCAN DATA, USE NEXT SUBCASE)   
C        
      FILE = SCR1        
      IF (.NOT.KOPEN) CALL OPEN (*310,SCR1,Z(IBUF3),RDREW)        
      IF (     KOPEN) CALL REWIND (SCR1)        
      KOPEN =.TRUE.        
      ISORT = 0        
      OSUBC = 0        
      OEL   = 0        
C        
      DO 270 II = 1,NCASE        
      IF (ISORT .EQ. 2) GO TO 220        
      CALL READ (*320,*330,SCR1,Z(1),2,0,L)        
      J = Z(2)        
      IF (J .EQ. 0) GO TO 260        
      SUBC = Z(1)        
      LS   = NZ - J        
      CALL READ (*320,*330,SCR1,Z(LS+1),J,1,L)        
      LE = LS        
      I  = LS        
 230  Z(LS) = I        
      LS = LS - 1        
      I  = I + Z(I+2) + 2        
      IF (I .LT. NZ) GO TO 230        
      LCORE = LS        
      J  = LS + 1        
      KK = 230        
      IF (DEBUG) WRITE (NOUT,200) KK,SUBC,(Z(I),I=J,NZ)        
C        
C     NOW IS THE TIME TO SET THE SCAN PARAMETERS FOR EACH SCAN CARD     
C     WITHIN A SUBCASE, AND CALL STRSCN TO SCAN THE OUTPUT DATA        
C        
      I = LS        
 240  I = I + 1        
      IF (I .GT. LE) GO TO 270        
      IB = Z(I)        
      IF (Z(IB+1) .NE. SORF) GO TO 240        
      JMP   = Z(IB+2)        
      IEL   = Z(IB+3)        
C ONLY QUAD4 (=64) AND TRIA3 (=83) ARE VALID FOR LLOOP=2
      IF ( LLOOP .EQ. 2 .AND. IEL .NE. 64 .AND. IEL .NE. 83 )
     &     GO TO 240
      ISET  = Z(IB+4)        
      ICOMP = Z(IB+5)        
      NTOP  = Z(IB+6)        
      IMAX  = Z(IB+6)        
      IMIN  = Z(IB+7)        
      IDUPL = Z(IB+8)        
      INC   = Z(IB+9)        
      IOPT  = 1        
      IF (IMIN .EQ. -1) IOPT = 2        
      IF (IOPT .NE.  2) NTOP = 0        
      LBEG = LCORE        
      LEND = LCORE - 1        
      IF (ISET .EQ. -1) GO TO 250        
      LBEG = IB + 10        
      LEND = IB + JMP + 2        
 250  J    = (IEL-1)*INCR        
      IELT(1) = E(J+1)        
      IELT(2) = E(J+2)        
      IF (DEBUG) WRITE (NOUT,255) IELT,(Z(IB+J),J=3,9),IOPT,LBEG,LEND,  
     1           II,SUBC        
 255  FORMAT (/5X,16HDEBUG/SCAN255 - ,2A4,/5X,12I9)        
      CALL STRSCN (SORF/10000000)        
      IF (IOPT .LT. 0) GO TO 480        
      GO TO 240        
 260  CALL FWDREC (*320,SCR1)        
 270  CONTINUE        
C        
C     GO BACK TO PROCESS NEXT INPUT FILE        
C        
      GO TO 220        
C        
C     ALL SCAN DONE.  WRITE OUTPUT FILE TRAILERS AND CLOSE ALL FILES    
C        
 280  IF (ITRL3 .LE. 0) GO TO 300        
CRLBR 12/29/93 SPR 93010 & 93011
C     Z(1) = OESFI        
      Z(1) = OESFI(LLOOP)        
      Z(2) = 1        
      Z(3) = ITRL3        
      DO 290 I = 4,7        
 290  Z(I) = 0        
      CALL WRTTRL (Z(1))        
C        
 300  IF (IOPEN) CALL CLOSE (INFILE,REW)        
      IF (JOPEN) CALL CLOSE (OUFILE,REW)        
      IF (KOPEN) CALL CLOSE (SCR1  ,REW)        
      IF (LOPEN) CALL CLOSE (CASECC,REW)        
CRLBNE 12/29/93 SPR 93010 & 93011
      IF (LLOOP .EQ. 2) GO TO 305
      LLOOP = 2
      IELT(1) = JELT(1)
      IELT(2) = JELT(2)
      GO TO 10
 305  CONTINUE
      IF ( QUAD4 .EQ. -1 ) WRITE ( NOUT, 605 ) 'QUAD4'
      IF ( TRIA3 .EQ. -1 ) WRITE ( NOUT, 605 ) 'TRIA3'
 605  FORMAT(//' SCAN MODULE DID NOT FIND ELEMENT ',A5,
     &       ' IN USER OUTPUT REQUESTS.',/
     &      ,' POSSIBLY WRONG COMPONENT SPECIFIED FOR LAYERED OR '
     &      ,'NON-LAYERED CASE',//)
CRLBNE 12/29/93 SPR 93010 & 93011
      RETURN        
C        
C     FILE ERRORS        
C        
 310  J = -1        
      GO TO 350        
 320  J = -2        
      GO TO 350        
 330  J = -3        
      GO TO 350        
 340  CONTINUE        
      GO TO 70        
 350  CALL MESAGE (J,FILE,NAM)        
 380  J = -8        
      GO TO 350        
C        
C     ERROR MESSAGES        
C        
 400  WRITE (NOUT,500)        
      GO TO 490        
 410  WRITE (NOUT,510)        
      GO TO 490        
 420  WRITE (NOUT,520)        
      GO TO 490        
 430  WRITE (NOUT,530)        
      GO TO 490        
 440  WRITE (NOUT,540)        
      GO TO 490        
 450  WRITE (NOUT,550) IELT        
      GO TO 490        
 460  WRITE (NOUT,560) SFM,IELT,IBEG,IEND        
      GO TO 490        
 470  WRITE (NOUT,570) UWM,ISET        
      GO TO 140        
 480  WRITE (NOUT,580) IOPT        
 490  WRITE (NOUT,590) SWM        
      GO TO 280        
C        
 500  FORMAT (//5X,48HONLY ONE INPUT FILE ALLOWED FROM SCAN DMAP ALTER) 
 510  FORMAT (//5X,21HAMAX-AMIN RANGE ERROR)        
 520  FORMAT (//5X,35HFIELD COMPONENT SPECIFICATION ERROR)        
 530  FORMAT (//5X,30HNO AMAX-AMIN OR NTOP SPECIFIED)        
 540  FORMAT (//5X,46HSPECIFY EITHER AMAX-AMIN OR NTOP, BUT NOT BOTH,   
     1         /5X,21H(NTOP=20  BY DEFAULT))        
 550  FORMAT (//5X,22HELEMENT MIS-SPELLED - ,2A4)        
 560  FORMAT (A25,' - SCANNING ',2A4,' ELEMENT. IBEG-IEND OUT OF RANGE',
     1       '.  SCAN ABORTED')        
 570  FORMAT (A25,' FROM SCAN, SET',I9,' NOT FOUND')        
 580  FORMAT (//5X,44HUSER ERROR.  ILLEGAL INPUT FILE SENT TO SCAN,I6)  
 590  FORMAT (A27,' FROM SCAN.  CASE ABORTED ***')        
 600  FORMAT (A25,' FROM SCAN, DUPLICATE SET',I9)        
C        
 610  CALL MESAGE (8,0,NAM)        
      RETURN        
      END        
