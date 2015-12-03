      SUBROUTINE IFP1H (I81,NZ,J400)
C
C     THIS ROUTINE PROCESSES THE SCAN CARD IN CASE CONTROL SECTION
C
C     WRITTEN BY G.CHAN/SPERRY,  OCTOBER 1984
C
C     PROGRAM METHOD
C
C     A 'SCAN(HELP)' INPUT CARD WILL SET J400 TO 2, AND ANY ERROR IN
C     A SCAN INPUT CARD WILL SET J400 TO 1. NON-ZERO J400 WILL CAUSE
C     SCAN COMPONENT KEY-WORDS TO BE PRINTED.
C
C     THE SCAN INPUT CARDS, AND THEIR DATA, ARE DECODED AND SAVED IN
C     CASECC FILE AS SETS OF PSEUDO SET COMMANDS (SET ID OF 10000000 FOR
C     STRESS, AND 20000000 FOR FORCE). IN THIS WAY, THE SCAN CARDS CAN
C     BE USED IN ALL SUBCASE LEVELS, OR ABOVE-SUBCASE LEVEL, SIMILAR TO
C     THE ELEM. STRESS AND ELEM. FORCE CARDS IN THE CASE CONTROL SECTION
C     HOWEVER, MULTIPLE SCAN CARDS CAN BE USED IN ALL SUBCASE LEVELS,
C     AND WITHIN EACH SUBCASE
C
C     ELEM. NAME CAN BE SPECIFIED WITH OR WITHOUT THE LEADING LETTER C
C     E.G.  BAR, CBAR, QUAD2, CQUAD2
C
C     SCAN COMPONENTS CAN BE REQUESTED BY ACTUAL OUTPUT COLUMN NUMBER(S)
C     OR BY COMPONENT KEYWORD(S)
C     IF THE ACTUAL OUTPUT COLUMN IS NOT IN THE SAME WORD ORDER AS IN
C     THE OUTPUT PRINT FILE (E.G. OES1L FOR THE QUAD4 LAYER), THE ACTUAL
C     COLUMN COUNT AS IT APPEARS IN THE PRINTOUT, IS USED HERE. ANY
C     DISCREPANCY SHOULD BE HANDLED BY SCAN OR STRSCN ROUTINES.
C
C     A LIST OF KEYWORDS WILL BE PRINTED AUTOMATICALLY IF ELEM. NAME OR
C     COMPONENT KEYWORD ARE MISSPELLED OR MISSSING
C
C     THIS LIST IS ALSO PRINTED IF A  SCAN (HELP) CARD IS IN INPUT DECK
C
C     THIS ROUTINE MAY ISSUE THE FOLLOWING ERROR MESSAGES -
C
C        604 - NON-INTEGER IN INTEGER FIELD
C        608 - SET NOT DEFINED
C        617 - IMPROPER FORMAT
C        634 - KEYWORD INSIDE BRACKET IS ILLEGAL OR MISSPELLED
C        635 - ONLY ONE SET-ID ALLOWED IN A SCAN CARD
C        636 - EXTRA VALUE ENCOUNTERED
C        637 - ILLEGAL COMPONENT SPECIFIED
C        638 - COMPONENT LIMIT OF 31 IS EXCEEDED
C        639 - SET ID ERROR (REQUESTED BEFORE EQUAL SIGN OR SPLITTED ID)
C        640 - TOO MANY COMPONENTS BY NAME
C        641 - -MAX EXCEEDS +MAX
C        642 - COMPONENT NAME NOT AVAILABLE FOR ELEMENT SELECTED
C        643 - SCAN BY STRESS OR FORCE ONLY
C        644 - WARNING MESSAGE FOR POSSIBLE INSUFFICIENT CORE
C        909 - CORE ARRAY NOT INITIALIZED CORRECTLY, OR MZERO IS NOT SET
C              IN AGREEMENT WITH XRCARD
C
C     EXAMPLE - TO ADD A NEW ELEMENT TO THE SCAN MODULE   BY G.C. 7/89
C           1.  INCREASE COMP DIMENSION TO ALLOW NEW COMPONENT WORDS
C               IF THEY DO NOT ALREADY EXIST.
C           2.  EXPAND THE SP-ARRAY IF NECESSARY. INCREASE NCOMP BY
C               THE NUMBER OF NEW WORDS ADDED
C           3.  REACTIVATE THE CORRESPONDING WORD IN ETAB THAT POINTS
C               TO THE NEW ARRAY IN TAB
C           4.  IF SP-ARRAY IS USED, MAKE SURE THAT THE COMPONENT WORDS
C               ARE PROPERLY PROCESSED, IN STATEMENT NOS. 110-120
C           5.  SET THE CODED WORDS IN TAB. SEE COMMENTS FUTHER DOWN
C           6.  PREPARE FORMAT FOR COMPONENT WORDS PRINT OUT (FMT 690)
C               UPDATE ISP-ARRAY IN CASE SP-ARRAY WAS USED PREVIOUSLY
C
      LOGICAL         DEBUG,   BIT64
      INTEGER         CORE(1), SCR1,    NAM(2),  STRESS,  FORCE,
     1                SETI,    BLANK,   E,       ERR,     EQUAL
      INTEGER         SAVE(5), ETAB(90),TAB(10,17),       COMP(2,60),
     1                COMP1(2,19),      COMP2(2,19),      ISP(10),
     2                TAB1(10,9),       TAB2(10,8),       SP(30),
     3                COMP3(2,19),      COMP4(2,3),       ICSE(400),
     4                COREY(401)
      DIMENSION       RCORE(1),LL(4),   CC(4),   KEYWDS(3)
      REAL            BCD(2,3)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ IBUF,    NOUT,    NOGO,    SKIP(5), NLPP,
     1                MORE(2), LINE
      COMMON /MACHIN/ MACH
      COMMON /GPTA1 / NELEM,   LAST,    INCR,    E(1)
      COMMON /XIFP1 / BLANK,   BIT64
      COMMON /IFP1A / SCR1,    CASECC,  IS,      NWPC,    NCPW,
     1                NMODES,  ICC,     NSET,    DUMMY(3),ISUB,
     2                LENCC,   IBLNK,   IEQUAL,  IEOR
      COMMON /IFP1HX/ MSST,    MISSET(1)
      COMMON /ZZZZZZ/ COREX(1)
      EQUIVALENCE     (CORE(1)  ,RCORE(1)   ,  COREY(401)           ),
     1                (COREX(1) ,COREY(1)   ,  ICSE(1)              ),
     2                (COMP(1,1),COMP1(1,1)), (COMP4(1,1),COMP(1,58)),
     3                (TAB1(1,1) ,TAB(1, 1)), (COMP2(1,1),COMP(1,20)),
     4                (TAB2(1,1),TAB(1,10)) , (COMP3(1,1),COMP(1,39)),
     5                (BLANK    ,XBLANK   ) , (STRESS    ,BCD(1,1)  ),
     6                (FORCE    ,BCD(1,2) ) , (SHEA      ,COMP(1, 9)),
     7                (NORM     ,COMP(1,9)) , (MOME      ,COMP(1,22))
      DATA    NCOMP,  EQUAL,    LLL,    SETI,   DEBUG    /
     1        58,     4H=   ,   4HL   , 4HSET , .FALSE.  /
      DATA    LLC,    COMMA,    MZERO,  BCD                          /
     1        4HC   , 4H,   ,   -0    , 4HSTRE,2HSS,4HFORC,1HE,2*1H  /
      DATA    NAM   / 4HIFP1,   4HH   /
      DATA    COMP1 / 4HAXIA,   4HL   ,
     2                4HTORS,   4HIONA,
     3                4HRADI,   4HAL  ,
     4                4HNORM,   4HAL  ,
     5                4HPRIN,   4HCIPA,
     6                4HMAJO,   4HR   ,
     7                4HMINO,   4HR   ,
     8                4HBEND,   4HING ,
     9                4HNORM,   4H-X  ,
C    9                or          -U  , AL-1, AL-X
     O                4HNORM,   4H-Y  ,
C    O                or          -V  , AL-2, AL-Y
     1                4HNORM,   4H-Z  ,
     2                4HSHEA,   4HR   ,
C    2                or          R-1Z, R-41
     3                4HSHEA,   4HR-XY,
C    3                or          R-ZR, R-X , R-U , R-12
     4                4HSHEA,   4HR-YZ,
C    4                or          R-RT, R-Y , R-V , R-23
     5                4HSHEA,   4HR-ZX,
C    5                or          R-ZT, R-UV, R-2Z, R-34
     6                4HMAX-,   4HSHR ,
     7                4HSHR-,   4HFORC,
     8                4HOCT-,   4HSHR ,
     9                4HSA-M,   4HAX  /
      DATA    COMP2 / 4HSB-M,   4HAX  ,
     1                4HMOME,   4HNT  ,
     2                4HMOME,   4HNT-A,
C    2                or          NT-X, NT-U , NT-1
     3                4HMOME,   4HNT-B,
C    3                or          NT-Y, NT-V , NT-2
     4                4HCURV,   4H    ,
     5                4HTORQ,   4HUE  ,
     6                4HCIRC,   4HUM  ,
     7                4HTWIS,   4HT   ,
     8                4HMARG,   4HIN  ,
     9                4HMAX ,   4H    ,
     O                4HMEAN,   4H    ,
     1                4HAVG ,   4H    ,
     2                4HMEM-,   4HT   ,
     3                4HMEM-,   4HC   ,
     4                4HFLEX,   4H-T  ,
     5                4HFLEX,   4H-C  ,
     6                4HPRIN,   4HC-A ,
     7                4HPRIN,   4HC-B ,
     8                4HPRIN,   4HC-C /
      DATA    COMP3 / 4HEFOR,   4HCE  ,
     O                4HFORC,   4HE-1 ,
C                     or          E-12,
     1                4HFORC,   4HE-2 ,
C                     or          E-23,
     2                4HFORC,   4HE-3 ,
C                     or          E-34,
     3                4HFORC,   4HE-4 ,
C                     or          E-41,
     4                4HKICK,   4H-FOR,
     5                4HSIG-,   4HX   ,
     6                4HSIG-,   4HY   ,
     7                4HTAU-,   4HXY  ,
     8                4HHELP,   4H    ,
     9                4HON-L,   4HINE ,
     O                4HFX+F,   4HY   ,
     1                4HFXY ,   4H    ,
     2                4HMX+M,   4HY   ,
     3                4HMXY ,   4H    ,
     4                4HVX+V,   4HY   ,
     5                4HKICK,   4H ON1,
     6                4HKICK,   4H ON2,
     7                4HKICK,   4H ON3/
      DATA    COMP4 / 4HKICK,   4H ON4,
     9                4H .. ,   4H .. ,
     O                4H .. ,   4H .. /
      DATA    SP    / 4HR-ZR,   4HR-U ,   4HR-RT,   4HR-V ,   4HR-ZT,
     1                4HR-UV,   4HNT-X,   4HNT-U,   4HNT-Y,   4HNT-V,
     2                4H-U  ,   4H-V  ,   4HR-X ,   4HR-Y ,   4HR-41,
     3                4HR-12,   4HR-23,   4HR-34,   4HNT-1,   4HNT-2,
     4                4HR-1Z,   4HR-2Z,   4HAL-1,   4HAL-2,   4HAL-X,
     5                4HAL-Y,   4HE-12,   4HE-23,   4HE-34,   4HE-41/
      DATA    ETAB  /
     1                  1, -02,   1,   2,   2,   3,   3,   3,   4,   1,
     2                  6,   6,   6, -14,   3,   4,   3,   3,   3, -20,
     3                -21, -22, -23, -24, -25, -26, -27, -28, -29, -30,
     4                -31, -32, -33,   7,   8,   9,  10,  11, -39, -40,
     5                -41, -42, -43, -44, -45, -46, -47, -48, -49, -50,
     6                -51, -52, -53, -54, -55, -56, -57, -58, -59, -60,
     7                -61,   4,   4,  15,  12,  12,  13,  14,  14, -70,
     8                -71, -72, -73, -74, -75, -76, -77, -78, -79,   5,
     9                  7, -82,  15, -84, -85, -86, -87, -88, -89, -90/
      DATA    TAB1  /
C    1. ROD, TUB, CONROD
     1                01000002, 02000004, 28000503,        0,        0,
     1               -01000002,-25000003,        0,        0,        0,
C    2. SHEAR, TWIST
     2                16000002, 28000004, 31000003, 29000002,        0,
     2               -40000002,-41000003,-22000002,-23000003,        0,
C    3. TRIA1, TRIA2, QUAD1, QUAD2, TRBSC, TRPLT, QDPLT
     3                09001103, 10001204, 13001305, 06001507, 07001608,
     3                16001709,-22000002,-23000003,-13000005,-14000006,
C    4. TRMEM, QDMEM, QDMEM1, DQMEM2
     4                09000002, 10000003, 13000004, 06000006, 07000007,
     4                16000008,-40000403,-41000605,-42000807,-43000902,
C   **  CONTINUE...
     5               -55000010,-56000012,-57000014,-58000016,-13000011,
     5               -14000013,-15000015,-12000017,        0,        0,
C    6. ELAS1, ELAS2, ELAS3, IS2D8
     6                18000002,-26000002,        0,        0,        0,
     6               -40000904,-41000603,-42000805,-43000702,        0,
C    7. BAR, ELBOW
     7                19000807, 20001514, 28001609, 01000006,        0,
     7               -01000008,-25000009,-12000605,-22000302,-23000504,
C    8. CONEAX
     8                09041852, 10051852, 15061852, 06081852, 07091852,
     8                16101852,-22000003,-23000004,-13000006,-14000007,
C    9. TRIARG
     9                03000002, 26000003, 01000004, 12000005,        0,
     9               -03020353,-26030353,-01040353,        0,        0/
      DATA    TAB2  /
C    O. TRAPRG
     O                03020455, 26030455, 01040455, 12050455, 17060455,
     O               -03020354,-26030354,-01040354,        0,        0,
C    1. TORDRG
     1                32020553, 33030553, 34040553, 35050553, 17060553,
     1               -03000802,-26000903,-01001004,-21001105,-24001307,
C   12. IHEX1, IHEX2
     2                09032258, 13042258, 36052258, 30092258, 10112258,
     2                14122258, 37132258, 11172258, 15182258, 38192258,
C   13. IHEX3
     3                09032382, 13042382, 36052382, 30092382, 10122382,
     3                14132382, 37142382, 11182382, 15192382, 38202382,
C   14. TRIAAX, TRAPAX
     4                03030853, 01040853, 26050853, 33060853, 34070853,
     4                35080853,-03030453,-26040453,-01050453,        0,
C   15. QUAD4, TRIA3 (GENERAL)
     5                09000003, 10000004, 13000005, 06000007, 07000008,
     5                16000009,-50000302,-51000004,-52000605,-53000007,
     6               -54000908,
C   **. QUAD4, TRIA3 (LAYER), 9 DIGIT CODE
     6                          81030899, 82040899, 84050899, 83070899,
     6                85080899,        0,        0,        0,        0,
C   17.
     7                10*0/
C
C     FIRST 2 DIGITS IN A TAB ITEM ARE COMPONENT POINTER, POINTING TO
C     THE BCD WORDS IN COMP ARRAY. POSITIVE FOR STRESS, AND NEGATIVE FOR
C     FORCE DATA (WITH SOME EXCEPTIONS). THIS POINTER IS USED ONLY
C     LOCALLY WITHIN THIS SUBROUTINE.
C     NEXT 3 NUMBERS (2 DIGITS EACH) ARE POINTERS TO THE FIELD NOS.
C
C     SPECIAL CASE -
C     IF LAST FIELD IS GREATER THAN 50 THEN, THIS LAST FIELD MINUS 50 IS
C     THE REPEAT FLAG. IF LAST FIELD IS 99, WE HAVE AN OPEN-END REPEAT.
C     IF LAST FIELD IS GREATER THAN 50, NEXT TO LAST FIELD IS FIELD
C     INCREMENT, AND THE FIELD IN FRONT IS THE FIRST STARTING COLUMN TO
C     BE SCANNED.
C     THE QUAD4/TRIA3 LAYER HAS COMPONENT INDICES 81 THRU 85
C     (THUS - IN FUTURE EXPANSION, ARRAY COMP SHOULD NOT EXCEED 80)
C
C     E.G.  TAB(3,1) =  09 00 11 03
C                       09          = NORMAL-X (STRESS)
C                          00       = SKIP
C                             11 03 = SCAN BY 3RD AND 11TH FIELDS
C
C     E.G.  TAB(9,8) = -01 04 03 54
C                      -01          = AXIAL (FORCE)
C                                54 = REPEAT 4 TIMES
C                             03    = INCREASE BY 3 ON EACH REPEAT
C                          04       = SCAN BY 4, 7, 10, AND 13TH FIELDS
C
      IF (J400 .EQ. 2) GO TO 400
      IF (MACH.EQ.2 .OR. MACH.GE.5) MZERO = -1
      CALL SSWTCH (20,J)
      IF (J .EQ. 1) DEBUG = .TRUE.
      ERR   = -909
      NSCAN = LENCC - 1
      IF (CORE(I81+3) .NE. MZERO) GO TO 300
      ISCAN = I81
      IWDS  = I81 + 1
      IISUB = I81 + 2
      IELEM = I81 + 3
      ISET  = I81 + 4
      ICOMP = I81 + 5
C     +MAX  = I81 + 6 = TOP N
C     -MAX  = I81 + 7
      IREPT = I81 + 8
C
C     NOTE - THE IISUB WORD WILL BE DROPPED WHEN THESE WORDS ARE
C            TRANSFERRED TO CASECC
C
      JCOMP = IWDS
      IEND  = I81 + CORE(I81)*2 - 1
      CORE(ISCAN) = 0
      CORE(IISUB) = ISUB
      CORE(IELEM) = 0
      CORE(ISET ) = 0
      CORE(JCOMP) = 0
      NWDSS = 0
      NWDSF = 0
      IEQ   = 0
      MAX   = 0
      MIN   = 0
      NSV   = 0
      NRP   = 0
      II    = I81 + 3
C
 10   II = II + 2
      JJ = CORE(II  )
      KK = CORE(II+1)
      JX = JJ
      IF (.NOT.BIT64) GO TO 15
CWKBD 3/94      CALL MVBITS (BLANK,0,32,JX,0)
CWKBD 3/94      CALL MVBITS (BLANK,0,32,KK,0)
 15   IF (JJ .EQ. IEOR) GO TO 200
      IF (II .GT. IEND) IF (JJ) 150,190,20
      GO TO 30
C
C     DECODE BCD WORD
C
 20   IEND = II + JJ*2 - 1
      II   = II - 1
      GO TO 10
C
C     LOOK FOR EQUAL SIGN OR SET
C
 30   ERR = -617
      IF (JJ .NE. MZERO) GO TO 40
      IF (KK .NE. EQUAL) GO TO 300
      IEQ = IEQ + 1
      IF (IEQ .GT. 1) GO TO 300
      CORE(ICOMP+1) = 0
      CORE(ICOMP+2) = 0
      GO TO 10
 40   IF (JX .EQ. SETI) GO TO 130
C
C     LOOK FOR STRESS OR FORCE
C
      IF (IEQ .EQ.1) GO TO 300
      IF (JX .NE. STRESS) GO TO 50
      CORE(ISCAN) = CORE(ISCAN) + 10000000
      GO TO 10
 50   IF (JX .NE. FORCE) GO TO 60
      CORE(ISCAN) = CORE(ISCAN) + 20000000
      GO TO 10
C
C     LOOK FOR ELEMENT, DROP THE FIRST LETTER C IF NECESSARY
C
 60   IF (CORE(IELEM) .NE. 0) GO TO 100
      JC = NAM(1)
      KC = JC
      IF (KHRFN2(JX,1,1) .NE. LLC) GO TO 70
      JC = KHRFN3(BLANK,JX,1,1)
      KC = KHRFN3(BLANK,KK,1,1)
      JC = KHRFN1(JC,4,KK,1)
 70   J  = 1
      DO 80 I = 1,NELEM
      IF (JX.EQ.E(J) .AND. KK.EQ.E(J+1)) GO TO 90
      IF (JC.EQ.E(J) .AND. KC.EQ.E(J+1)) GO TO 90
 80   J = J + INCR
      GO TO 100
 90   CORE(IELEM) = I
      NWDSS = E(J+17)
      NWDSF = E(J+18)
      GO TO 10
C
C     LOOK FOR COMPONENT
C
 100  DO 110 I = 1,NCOMP
      IF (JX.EQ.COMP(1,I) .AND. KK.EQ.COMP(2,I)) GO TO 120
 110  CONTINUE
      ERR = -634
      I = 0
C
C     SP ARRAYS
C        1     2     3     4     5     6     7     8     9     10
C        R-ZR  R-U   R-RT  R-V   R-ZT  R-UV  NT-X  NT-U  NT-Y  NT-V
C        11    12    13    14    15    16    17    18    19    20
C        -U    -V    R-X   R-Y   R-41  R-12  R-23  R-34  NT-1  NT-2
C        21    22    23    24    25    26    27    28    29    30
C        R-1Z  R-2Z  AL-1  AL-2  AL-X  AL-Y  E-12  E-23  E-34  E-41
C
      IF (JX .NE.  FORCE) GO TO 115
      IF (KK .EQ. SP(27)) I = 40
C                 FORCE-12 (USED IN QDMEM2)
      IF (KK .EQ. SP(28)) I = 41
C                 FORCE-23
      IF (KK .EQ. SP(29)) I = 42
C                 FORCE-34
      IF (KK .EQ. SP(30)) I = 43
C                 FORCE-41
      IF (I .EQ. 0) GO TO 300
 115  IF (JX.NE.NORM .AND. JX.NE.SHEA .AND. JX.NE.MOME) GO TO 300
      IF (KK.EQ.SP( 1) .OR. KK.EQ.SP( 2) .OR. KK.EQ.SP(13)) I = 13
C         SHEAR-ZR          SHEAR-U           SHEAR-X
      IF (KK.EQ.SP( 3) .OR. KK.EQ.SP( 4) .OR. KK.EQ.SP(14)  .OR.
     1    KK.EQ.SP(17)) I = 14
C         SHEAR-RT          SHEAR-V           SHEAR-6
C         SHEAR-23
      IF (KK.EQ.SP( 5) .OR. KK.EQ.SP( 6) .OR. KK.EQ.SP(18)) I = 15
C         SHEAR-ZT          SHEAR-UV          SHEAR-34
      IF (I .NE. 0) GO TO 120
      IF (KK.EQ.SP( 7) .OR. KK.EQ.SP( 8)) I = 22
C         MOMENT-X          MOMENT-U
      IF (KK.EQ.SP( 9) .OR. KK.EQ.SP(10)) I = 23
C         MOMENT-Y          MOMENT-V
      IF (KK.EQ.SP(15) .OR. KK.EQ.SP(19)) I = 12
C         SHEAR-41          MOMENT-1
      IF (KK.EQ.SP(11) .OR. KK.EQ.SP(25)) I = 9
C         NORM-U            NORNAL-X
      IF (KK.EQ.SP(12) .OR. KK.EQ.SP(26)) I = 10
C         NORM-V            NORNAL-Y
      IF (I .NE. 0) GO TO 120
      IF (KK.EQ.SP(16) .OR. KK.EQ.SP(20)) I = 13
C         SHEAR-12          MOMENT-2
C
C     SECOND SET KEYWORDS FOR QUAD4/TRIA3 LAYER, 81 AND HIGHER
C     (THE GENERAL QUAD4/TRIA3 KEYWORDS ARE BELOW 80)
C
      IF (I .NE. 0) GO TO 120
      IF (KK .EQ. SP(23)) I = 81
C         NORAML-1
      IF (KK .EQ. SP(24)) I = 82
C         NORMAL-2
      IF (KK .EQ. SP(16)) I = 84
C         SHEAR-12
      IF (KK .EQ. SP(21)) I = 83
C         SHEAR-1Z
      IF (KK .EQ. SP(22)) I = 85
C         SHEAR-2Z
      IF (I .EQ. 0) GO TO 300
C
 120  IF (I .EQ. 48) GO TO 320
      IF (I .EQ. 49) GO TO 900
      ERR = -640
      IF (NSV .GT. 5) GO TO 300
      IF (NSV .LE. 0) GO TO 125
      DO 123 J = 1,NSV
      IF (SAVE(J) .EQ. I) GO TO 127
 123  CONTINUE
 125  NSV = NSV + 1
      SAVE(NSV) = I
C
C     TWO WORDS, PRINCIPAL AND TORSIONAL, HAVE A LETTER L TOO LONG
C
C     LLL IS 4HL   , BLANK FILL
C     IBLNK IS 1H  , ZERO  FILL
C
 127  IWORD = CORE(II+2)
CWKBD 3/94      IF (BIT64) CALL MVBITS (BLANK,0,32,IWORD,0)
      IF (II.LT.IEND .AND. IWORD.EQ.LLL .AND. CORE(II+3).EQ.IBLNK)
     1    II = II + 2
      GO TO 10
C
C     PROCESS SET
C
 130  ERR = -635
      IF (CORE(ISET) .NE. 0) GO TO 300
      ERR = -639
      IF (IEQ.NE.1  .OR. CORE(II+2).NE.-1) GO TO 300
      CORE(ISET) = CORE(II+3)
      II = II + 2
      J  = NWPC + 1 + ICSE(LENCC)
      DO 140 I = 1,NSET
      IF (CORE(ISET) .EQ. CORE(J)) GO TO 10
 140  J = J + CORE(J+1) + 3
      ERR  = -608
      MSST = MSST + 1
      IF (MSST .GT.  0) GO TO 300
      MISSET(MSST) = CORE(ISET)
      ERR = 0
      GO TO 10
C
C     NUMERIC DATA
C
 150  IF (JJ .EQ. -2) GO TO 170
      IF (IEQ .EQ. 1) GO TO 160
C
C     INTEGER BEFORE EQUAL SIGN = COMPONENT(S)
C
      ERR = -637
      IF (JJ.NE.-1 .OR. KK.LE.1) GO TO 300
      ERR = -638
      IF (KK .GT. 31) GO TO 300
      CORE(JCOMP) = CORE(JCOMP) + 2**(KK-1)
      GO TO 10
C
C     INTEGER AFTER EQUAL SIGN = TOP N
C
 160  ERR = -608
      IF (IEQ .NE. 1) GO TO 300
      ERR = -636
      IF (MAX-1) 180,300,300
C
C     F.P. DATA = +MAX OR -MAX
C
 170  ERR = -608
      IF (IEQ .NE. 1) GO TO 300
      MIN = 1
      ERR = -636
      IF (MAX .GE. 2) GO TO 300
 180  MAX = MAX + 1
      CORE(ICOMP+MAX) = KK
      ERR = -641
      IF (MAX.EQ.2 .AND. RCORE(ICOMP+2).GT.RCORE(ICOMP+1)) GO TO 300
      GO TO 10
C
C     READ CONTINUATION CARD
C
 190  CALL READ (*290,*290,SCR1,CORE(1),NWPC,0,FLAG)
      WRITE (NOUT,310) ICC,(CORE(I),I=1,NWPC)
      ICC  = ICC  + 1
      LINE = LINE + 1
      IF (LINE .GT. NLPP) CALL PAGE
      II = I81 + 8
      NZ = NZ - II
      CALL XRCARD (CORE(II),NZ,CORE(1))
      II = II - 2
      IEND = II
      GO TO 10
C
C     SCAN CARD COMPLETED
C
 200  IF (NOGO .NE. 0) GO TO 330
      ERR = -643
      IF (CORE(ISCAN).NE.10000000 .AND. CORE(ISCAN).NE.20000000)
     1   GO TO 300
      CORE(ICOMP) = CORE(JCOMP)
      CORE(IWDS ) = 6
      IF (CORE(ISET ) .EQ. 0) CORE(ISET) = -1
      IF (CORE(IELEM).EQ.0 .AND. CORE(ICOMP+2).EQ.0) CORE(IELEM) = -1
      IF (MAX .EQ. 0) CORE(ICOMP+1) = 20
      IF (MAX.LE.1 .AND. MIN.EQ.0) CORE(ICOMP+2) = -1
      IF (CORE(ICOMP+2) .NE. -1) GO TO 205
C
C     COMPUTE HOW HIGH TOPN CAN GO ASSUMING LINK14 HAS AN OPEN CORE SIZE
C     SAME AS THAT OF LINK1
C
      IF (CORE(ISCAN) .GE. 20000000) NWDSS = NWDSF
      IF (2*NWDSS*CORE(ICOMP+1) .GT. KORSZ(ICSE(1))) CALL IFP1D (644)
C
C     CONVERT NAMED COMPONENT TO FIELD NO.
C
 205  IF (NSV.EQ.0 .OR. CORE(IELEM).EQ.-1) GO TO 250
      I = CORE(IELEM)
      I = ETAB(I)
      ERR = -642
      IF (I .LT. 0) GO TO 300
      IE = 10
      IF (I .EQ. 14) IE = 16
      DO 240 K = 1,NSV
      IF (CORE(ISCAN) .EQ. 20000000) SAVE(K) = -SAVE(K)
      DO 210 J = 1,IE
      II = TAB(J,I)/1000000
      IF (II .EQ. 0) GO TO 210
      IF (SAVE(K) .EQ. II) GO TO 220
 210  CONTINUE
C
C     5 SPECIAL CASES WHERE TAB ARRAY OF 10 IS NOT LONG ENOUGH
C     SET THE 11TH ITEM OF ECAH OF THESE 3 CASES
C
      II = 0
      IF (I.EQ. 3 .AND. SAVE(K).EQ.27) II = -27000004
C         TRIA1         TWIST (MOMENT)
      IF (I.EQ.12 .AND. SAVE(K).EQ.18) II = +18102258
C         IHEX1         OCT-SHR (STRESS)    +18102270 (IHEX2)
      IF (I.EQ.13 .AND. SAVE(K).EQ.18) II = +18102382
C         IHEX3         OCT-SHR (STRESS)
      IF (I.EQ.12 .AND. SAVE(K).EQ.16) II = +16102270
C         IHEX2         MAX-SHR (STRESS)
      IF (I.EQ.13 .AND. SAVE(K).EQ.16) II = +16102382
C         IHEX2         MAX-SHR (STRESS)
      ERR = -637
      IF (II) 300,300,225
 220  II = IABS(TAB(J,I))
 225  II = MOD(II,1000000)
      DO 230 JJ = 1,3
      KK = MOD(II,100)
      IF (CORE(ICOMP) .NE. 1) GO TO 223
      CORE(ICOMP) = 0
      IF (IELEM .EQ. 66) KK = 70
C                    IHEX2
      IF (IELEM .EQ. 69) KK = 54
C                    TRIATS
      NRP = NRP + KK
      KK  = 0
 223  IF (KK .LE. 50) GO TO 227
      NRP = (KK-50)*100
C     NRP = 4900 FOR OPEN-END REPEAT FLAG
      KK  = 1
 227  IF (KK .GT. 0) CORE(ICOMP) = CORE(ICOMP) + 2**(KK-1)
      II  = II/100
      IF (II .EQ. 0) GO TO 240
 230  CONTINUE
 240  CONTINUE
C
C     NRP/100 IS REPEAT FLAG, AND MOD(NRP,100) IS INCREMENT
C
 250  IF (NOGO .NE. 0) GO TO 330
      CORE(IREPT) = NRP
C
C     FINAL ERROR CHECK
C
      IF (CORE(IELEM).EQ.0 .OR. CORE(ICOMP).EQ.0) J400 = 1
      ERR = -617
      J = 0
      DO 260 I = 1,8
      IF (CORE(I81+J) .EQ. 0) GO TO 300
 260  J = J + 1
C
C     ALL GO WELL, RE-SET PARAMETERS
C     NOTE - THE (LENCC-1) WORD OF CASECC RECORDS THE NO. OF SCAN CARDS
C
      NSET = NSET + 1
      II = (ISUB-1)*LENCC
      ICSE(NSCAN+II) = ICSE(NSCAN+II) + 1
CWKBD IF (DEBUG) CALL BUG1 ('IFP1H',270,CORE(I81),9)
      I81 = I81 + 9
C
C     TURN ON STRESS OR FORCE OUTPUT FLAGS IF THEY ARE NOT ALREADY DONE
C     BY THE USER.   SET OUTPUT OPTIONS TO - ALL, NOPRINT, AND REAL
C     (WORD 23 ON CASECC IS STRESS OUTPUT FLAG, AND
C      WORD 26 ON CASECC IS FORCE  OUTPUT FLAG)
C
      IF (CORE(ISCAN).EQ.20000000 .OR. ICSE(23+II+1).NE.0) GO TO 280
      ICSE(23+II  ) =-1
      ICSE(23+II+1) = 2
      ICSE(23+II+2) = 1
 280  IF (CORE(ISCAN).NE.20000000 .OR. ICSE(26+II+1).NE.0) GO TO 330
      ICSE(26+II  ) =-1
      ICSE(26+II+1) = 2
      ICSE(26+II+2) = 1
      GO TO 330
C
 290  CALL MESAGE (-1,SCR1,NAM)
 300  CALL IFP1D (ERR)
 310  FORMAT (11X,I8,6X,20A4)
      IF (ICSE(NSCAN) .LT. 0) GO TO 330
      IF (ERR.NE.-634 .AND. ERR.NE.-637 .AND. ERR.NE.-642) GO TO 330
      ICSE(NSCAN) = -10000
 320  J400 = 1
 330  RETURN
C
C     PRINT OUT SCAN COMPONENT KEYWORDS
C
 400  CALL PAGE1
      II = 20
      GO TO 810
 410  II = 0
      WRITE  (NOUT,420)
 420  FORMAT (46H0*** COMPONENT KEYWORDS FOR THE SCAN OPERATION, //5X,
     1  59HFORCE/STRESS    KEYWORD        COMPONENT (OUTPUT FIELD NO.),
     2  /5X,15(4H----),/)
      LLINE = 15
 425  FORMAT (/5X,17HROD, TUBE, CONROD,/)
      GO TO 700
 430  ISP(8) = 19
      ISP(9) = 20
      LLINE  = 11
 435  FORMAT (/5X,12HSHEAR, TWIST,/)
      GO TO 700
 440  ISP( 7) = 7
      ISP( 8) = 9
      ISP( 9) = 13
      ISP(10) = 14
      LLINE   = 14
 445  FORMAT (/5X,47HTRIA1, TRIA2, QUAD1, QUAD2, TRBSC, TRPLT, QDPLT,/)
      GO TO 700
 450  WRITE  (NOUT,455)
 455  FORMAT (10X,'FORCE      TWIST',15X,'4')
      LINE = LINE + 1
      ISP( 7) = 27
      ISP( 8) = 28
      ISP( 9) = 29
      ISP(10) = 30
      LLINE = 13
 460  FORMAT (/5X,28HTRMEM, QDMEM, QDMEM1, QDMEM2,/)
      GO TO 700
 470  LLINE = 8
      GO TO 700
 480  LLINE = 9
 490  FORMAT (/5X,26HELAS1, ELAS2, ELAS3, IS2D8,/)
      GO TO 700
 500  LLINE = 12
 510  FORMAT (/5X,10HBAR, ELBOW,/)
      GO TO 700
 520  ISP(1) = 11
      ISP(2) = 12
      ISP(3) = 6
      ISP(7) = 8
      ISP(8) = 10
      LLINE  = 13
 530  FORMAT (/5X, 6HCONEAX,/)
      GO TO 700
 540  LLINE = 10
 550  FORMAT (/5X, 6HTRIARG,/)
      GO TO 700
 560  LLINE = 11
 570  FORMAT (/5X, 6HTRAPRG,/)
      GO TO 700
 580  LLINE = 13
 590  FORMAT (/5X, 6HTORDRG,/)
      GO TO 700
 600  LLINE = 14
 610  FORMAT (/5X,12HIHEX1, IHEX2,/)
      GO TO 700
 620  WRITE  (NOUT,625)
 625  FORMAT (10X,'STRESS     MAX-SHR',12X,'10, 32, 54, 76 ... ETC',
     1       /10X,'STRESS     OCT-SHR',12X,'10, 32, 54, 76 ... ETC')
      LINE  = LINE + 1
      LLINE = 14
 630  FORMAT (/5X, 6HIHEX3 ,/)
      GO TO 700
 640  WRITE  (NOUT,645)
 645  FORMAT (10X,'STRESS     MAX-SHR',12X,'10, 33, 56, 79 ... 746',
     1       /10X,'STRESS     OCT-SHR',12X,'10, 33, 56, 79 ... 746')
      LINE  = LINE + 1
      LLINE = 12
 650  FORMAT (/5X,14HTRIAAX, TRAPAX,/)
      GO TO 700
 660  ISP(1) = 25
      ISP(2) = 26
      LLINE  = 19
 665  FORMAT (/5X,12HQUAD4, TRIA3,/)
      GO TO 700
C
C   . QUAD4/TRIA3 LAYER
C
 670  ISP(2) = 23
      ISP(3) = 24
      ISP(4) = 16
      ISP(5) = 21
      ISP(6) = 22
      LLINE  = 5
      GO TO 700
C
 680  WRITE  (NOUT,685)
 685  FORMAT (1X)
      GO TO 840
C
 700  II = II + 1
      CALL PAGE2 (LLINE)
      GO TO (701,702,703,704,720,706,707,708,709,710,
     1       711,712,713,714,715,720,717,700,700,720), II
 701  WRITE (NOUT,425)
      GO TO 720
 702  WRITE (NOUT,435)
      GO TO 720
 703  WRITE (NOUT,445)
      GO TO 720
 704  WRITE (NOUT,460)
      GO TO 720
 706  WRITE (NOUT,490)
      GO TO 720
 707  WRITE (NOUT,510)
      GO TO 720
 708  WRITE (NOUT,530)
      GO TO 720
 709  WRITE (NOUT,550)
      GO TO 720
 710  WRITE (NOUT,570)
      GO TO 720
 711  WRITE (NOUT,590)
      GO TO 720
 712  WRITE (NOUT,610)
      GO TO 720
 713  WRITE (NOUT,630)
      GO TO 720
 714  WRITE (NOUT,650)
      GO TO 720
 715  WRITE (NOUT,665)
      GO TO 720
 717  LLINE = 0
      GO TO 700
C
 720  DO 800 I = 1,10
      JJ = TAB(I,II)
      IF (JJ .EQ. 0) GO TO 800
      BCD(1,3) = BCD(1,1)
      BCD(2,3) = BCD(2,1)
      IF (JJ .GE. 0) GO TO 725
      BCD(1,3) = BCD(1,2)
      BCD(2,3) = BCD(2,2)
 725  JJ = IABS(JJ)
C
C                     +-------------------+    LL(1) = XX
C     JJ = TAB(I,J)=  ! CC ! ZZ ! YY ! XX !    LL(2) = YY
C                     +-------------------+    LL(3) = ZZ
C                                              LL(4) = CC
      DO 730 J = 1,4
      LL(J) = MOD(JJ,100)
 730  JJ = JJ/100
      JJ = LL(4)
C
C     QUAD4/TRIA3 LAYER IF JJ IS 81 THRU 85
C
      IF (JJ.EQ.81 .OR.  JJ.EQ.82) JJ = 9
      IF (JJ.GE.83 .AND. JJ.LE.85) JJ = 12
C
      KEYWDS(1) = COMP(1,JJ)
      KEYWDS(2) = COMP(2,JJ)
      KEYWDS(3) = BLANK
      IF (II.EQ.4 .OR. II.EQ.16) GO TO  735
      IF (JJ.EQ.2 .OR. JJ.EQ. 5) KEYWDS(3) = LLL
      IF (JJ.LT.9 .OR. JJ.GT.30) GO TO 740
      IF (JJ.EQ.11 .OR. (JJ.GE.16 .AND. JJ.LE.21)) GO TO 740
 735  J = ISP(I)
      IF (J .GT. 0) KEYWDS(2) = SP(J)
 740  IF (LL(1) .GT. 50) GO TO 745
      LL(4) = 0
      IDUPL = 0
      JJ    = 3
      GO TO 760
 745  IDUPL = LL(1) - 50
      INC   = LL(2)
      JJ = MIN0(IDUPL,4)
      KK = LL(3)
      DO 750 J = 1,JJ
      LL(J) = KK
 750  KK = KK+INC
      KK = INC*IDUPL + LL(1)
 760  DO 765 J = 1,JJ
      IF (LL(J) .EQ. 0) GO TO 770
 765  CC(J) = COMMA
      J  = JJ + 1
 770  JJ = J  - 1
      CC(JJ) = XBLANK
      WRITE  (NOUT,775) BCD(1,3),BCD(2,3),KEYWDS,(LL(J),CC(J),J=1,JJ)
 775  FORMAT (10X,A4,A2,5X,2A4,A1,9X,4(I3,A1))
      IF (IDUPL .LE. 4) GO TO 800
      IF (II.NE.12 .AND. II.NE.14 .AND. II.NE.16) WRITE (NOUT,780) KK
      IF (II.EQ.12 .OR.  II.EQ.14 .OR.  II.EQ.16) WRITE (NOUT,785)
 780  FORMAT (1H+,54X,3H...,I4)
 785  FORMAT (1H+,54X,3H...,5H ETC.)
 800  CONTINUE
 810  DO 820 J = 1,10
 820  ISP(J) = 0
      GO TO (430,440,450,470,480,500,520,540,560,580,
     1       600,620,640,660,670,680,840,840,840,410), II
 840  WRITE  (NOUT,850)
 850  FORMAT (//5X,'USE OUTPUT FIELD NUMBER(S) TO SPECIFY COMPONENT(S)',
     1       'FOR ELEMENTS OR KEYWORDS', /5X,'NOT LISTED ABOVE',/)
      RETURN
C
C     ON-LINE
C
 900  WRITE  (NOUT,910) UFM
 910  FORMAT (A23,', SCAN ON-LINE OPTION IS NOT AVAILABLE IN THIS ',
     1        'NASTRAN RELEASE')
      NOGO = 1
      RETURN
      END
