      BLOCK DATA SEMDBD
CSEMDBD
C
C     *****  PRINCIPAL BLOCK DATA PROGRAM FOR NASTRAN  *****
C     (NOTE - MACHINE DEPENDENT CONSTANTS ARE INITIALIZED IN BTSTRP)
C
C     REVISED 7/91 BY G.CHAN/UNISYS
C     MAKE SURE THERE IS NO VARIABLES OR ARRAYS NOT INITIALIZED. GAPS
C     OR MISSING INITIALIZED DATA MAY CAUSE PROBLEMS IN SOME MACHINES.
C
      IMPLICIT INTEGER (A-Z)
      LOGICAL     BITPAS, FIRST,  NOTYET, OPNSOF, PASS,  PCT,  STAR
      INTEGER     KSYSTM(100)
      REAL        OSCAR,  OTAPID, TAPID , TIMDTA, TIME,  TOLEL,X
      CHARACTER   UFM*23, UWM*25, UIM*29, SFM*25, SWM*27, SIM*31
      EQUIVALENCE (KSYSTM(1),SYSBUF)
C
C
C     -------------------     /GINOX  /     ----------------------------
C
C     GINOX WORDS USED IN GINO
C     VAX AND UNIX USE 636 WORDS (SEE GINO.MDS)
CDC   CDC USES ONLY 244 WORDS (SEE CDC IO6600). CDC IS CORE THIRSTY AND
CDC   THE 392 WORDS IN OTHERS HERE, AND ON THE DATA LINE BELOW, CAN BE
CDC   COMMENTED OUT TO SAVE CORE SPACE FOR THE CDC MACHINE.
C
      COMMON /GINOX /    CDC(244)
     1,               OTHERS(392)
C            --------------------
C                     TOTAL= 636
C
C     -------------------     /XMSSG  /     ----------------------------
C
C     USER FATAL/WARNING/INFO AND SYSTEM FATAL/WARNING/INFO MESSAGES
C
      COMMON /XMSSG / UFM, UWM, UIM, SFM, SWM, SIM
C
C     -------------------     /NUMTPX /     ----------------------------
C
C     BCD-LOOK ALIKE FLOATING NUMBERS, USED ONLY BY NUMTYP SUBROUTINE.
C     DATA WILL BE LOADED FROM NASINFO.DOC FILE BY NSINFO
C
      COMMON /NUMTPX/ NBCD, BCD(19)
C
C     -------------------     / BLANK  /     ---------------------------
C
CWKBR COMMON /BLANK / IBLNK(60)
      COMMON /BLANK / IBLNK(100)
C
C     -------------------     / NTIME  /     ---------------------------
C
C     THE NTIME COMMON BLOCK CONTAINS TIMING CONSTANT DATA FOR THE
C     CURRENTLY RUNNING MACHINE CONFIGURATION AS DETERMINED BY THE
C     TMTSIO AND TMTSLP SUBROUTINES
C
      COMMON /NTIME / LNTIME, TIMDTA(23)
C
C     -------------------     / XLINK  /     ---------------------------
C
C     SPECIFIES MODULE LINK RESIDENCE, AND PROVIDES LINK SWITCHING
C     INFORMATION FOR LINK DRIVER SUBROUTINES, XSEMi.
C     LXLINK = NUMBER OF WORDS IN MXLINK.
C     MAXLNK = MAX NUMBER OF LINKS - SEE XGPIBS IF THIS NUMBER IS
C              INCREASED.
C     MXLINK = MODULE LINK SPECIFICATION TABLE - THIS TABLE IS
C              INITIALIZED BY SUBROUTINE XGPIBS.
C
      COMMON /XLINK / LXLINK, MAXLNK, MXLINK(220)
C
C     -------------------     / SEM    /     ---------------------------
C
C                      Moved to module MODSEMLNK.
C
C     -------------------     / SYSTEM /     ---------------------------
C
C                      Moved to module MODSYSTEM.
C
C     -------------------     / XFIST  /     ---------------------------
C
C     XFIST IS THE FILE STATUS TABLE (FIST).
C     NFIST  = TOTAL NO. OF ENTRIES IN FIST.
C     LFIST  = NO. OF ENTRIES IN THE CURRENT FIST.
C     FIST   = TABLE OF TWO-WORD ENTRIES.
C              FIRST WORD IS GINO FILE NAME.
C              SECOND WORD POINTS TO XFIAT IF .GT. 0 (I.E. NON-PERMANENT
C              ENTRY), OR POINTS TO XXFIAT IF .LE. 0 (I.E. PERMANENT
C              ENTRY). SIGN BIT MUST BE SET FOR ZERO POINTER ON 7094.
C
      COMMON /XFIST / NFIST, LFIST, FIST(112)
C
C     -------------------     / XPFIST /     ---------------------------
C
C     XPFIST DEFINES THE NO. OF PERMANENT ENTRIES IN THE FIST.
C
      COMMON /XPFIST/ NPFIST
C
C     -------------------     / XXFIAT /     ---------------------------
C
C     XXFIAT IS EXECUTIVE FILE ALLOCATION TABLE.
C
      COMMON /XXFIAT/ XXFIAT(24)
C
C     -------------------     / XFIAT  /     ---------------------------
C
C     XFIAT IS THE MODULE FILE ALLOCATION TABLE (FIAT).
C     MFIAT  = NO. OF UNIQUE FILES IN FIAT.
C     NFIAT  = TOTAL NO. OF ENTRIES IN FIAT.
C     LFIAT  = NO. OF ENTRIES IN CURRENT FIAT.
C     FIAT   = TABLE OF 8 OR 11 WORDS PER ENTRY OF GINO FILES
C              (DEFAULT IS SET BY ICFIAT, THE 24TH WORD OF /SYSTEM/)
C            . 1ST WORD DEFINES THE FILE + PURGE,EQVIV,SETUP,ETC INFO.
C            . 2ND AND 3RD WORDS DEFINE THE DATA BLOCK NAME (IN BCD)
C              WHICH IS ATTACHED TO THE FILE.
C            . SEE ICFIAT (24TH WORD OF /SYSTEM/) FOR THE DESCRIPTION
C              OF 4TH THRU 8TH (OR 11TH) WORDS.
C            . SET FIAT(880) IF 11-WORD/ENTRY TABLE IS USED, AND
C              SET FIAT(640) IF  8-WORD/ENTRY TABLE IS USED
C
CWKBR COMMON /XFIAT / MFIAT, NFIAT, LFIAT, FIAT(880)
      COMMON /XFIAT / MFIAT, NFIAT, LFIAT, FIAT(1100)
C
C     -------------------     / OSCENT /     ---------------------------
C
C     OSCENT DEFINES A STORAGE BLOCK FOR THE CURRENT OSCAR ENTRY.
C
      COMMON /OSCENT/ OSCAR(200)
C
C     -------------------     / OUTPUT /     ---------------------------
C
C     OUTPUT DEFINES A STORAGE BLOCK WHERE PROBLEM TITLE, SUBTITLE
C     AND LABEL ARE STORED.
C
      COMMON /OUTPUT/ OUTPUT(224)
C
C     -------------------     / XDPL   /     ---------------------------
C
C     XDPL DEFINES THE DATA POOL DICTIONARY.
C     MDPL   = POINTER TO NEXT AVAILABLE FILE.
C     NDPL   = TOTAL NO. OF ENTRIES IN DPL.
C     LDPL   = CURRENT NO. OF ENTRIES IN DPL.
C     DPL    = TABLE OF THREE-WORD ENTRIES
C              1ST + 2ND WORDS ARE DATA BLOCK NAME
C              3RD WORD DEFINES EQUIV FLAG, APPROX SIZE OF DATA BLOCK
C              AND FILE NO. IN THE POOL.
C
      COMMON /XDPL  / MDPL, NDPL, LDPL, DPL(240)
C
C     -------------------     / XVPS   /     ---------------------------
C
C     XVPS IS THE VARIABLE PARAMETER STORAGE TABLE.
C     VPS(1) = TOTAL NO. OF WORDS IN VPS.
C     VPS(2) = POINTER TO LAST WORD USED IN VPS.
C     VPS(3) = TABLE FOR STORAGE OF PARAMETERS
C              (VARIABLE NO OF WORDS/ENTRY).
C
      COMMON /XVPS  / VPS(600)
C
C     -------------------     / STAPID /     ---------------------------
C
C     STAPID CONTAINS THE I.D. FOR THE NEW AND OLD PROBLEM TAPES.
C     TAPID  = SIX-WORD I.D. FOR NEW PROBLEM TAPE.
C     OTAPID = SIX-WORD I.D. FOR OLD PROBLEM TAPE.
C     IDUMF  = (OBSOLETE) ID FOR USER-S MASTER FILE.
C
      COMMON /STAPID/ TAPID(6), OTAPID(6), IDUMF
C
C     -------------------     / STIME  /     ---------------------------
C
C     STIME DEFINES USER-S ESTIMATED PROBLEM SOLUTION TIME.
C
      COMMON /STIME / TIME(2)
C
C     -------------------     / XCEITB /     ---------------------------
C
C     XCEITB DEFINES LOOP CONTROL PARAMETERS FOR THE CONTROL ENTRY
C     INTERP.
C     CEI(1) = TOTAL NO. OF WORDS IN TABLE.
C     CEI(2) = POINTER TO LAST WORD USED.
C     CEI(3) = TABLE OF FOUR-WORD ENTRIES.
C
      COMMON /XCEITB/ CEI(42)
C
C     -------------------     / XMDMSK /     ---------------------------
C
C     XMDMSK DEFINES MASK FOR MODIFIED RESTART.
C     NMSKCD = NUMBER OF MASK WORDS FOR CARDS (CURRENTLY SET TO 3)
C     NMSKFL = NUMBER OF MASK WORDS FOR FILES (CURRENTLY SET TO 3)
C     NMSKRF = NUMBER OF MASK WORDS FOR RIGID FORMATS (CURRENTLY 1)
C     MSK    = MASK OF (NMSKCD+NMSKFL+NMSKRF) WORDS (31 BITS/WORD)
C
      COMMON /XMDMSK/ NMSKCD, NMSKFL, NMSKRF, MSK(7)
C
C     -------------------     / MSGX   /     ---------------------------
C
C     MSGX DEFINES A TABLE WHERE MESSAGES ARE QUEUED.
C     NMSG   = NUMBER OF MESSAGES CURRENTLY IN QUEUE.
C     MSGLG  = TOTAL NO. OF ENTRIES IN THE MESSAGE QUEUE.
C     MSG    = TABLE OF FOUR-WORD ENTRIES WHERE MESSAGES ARE STORED.
C
      COMMON /MSGX  / NMSG, MSGLG, MSG(4,40)
C
C     -------------------     / DESCRP /     ---------------------------
C
C     COMMENTS FROM G.CHAN/UNISYS, 7/1991
C     LABEL COMMON /DESCRP/ APPEARS IN THE FOLLOWING SUBROUTINES. BUT
C     IT IS ACTUALLY NEVER USED.
C        SEMDBD, DECOMP, GENVEC, GFBS,   TRANSP, CDCOMP, CTRNSP, INVTR,
C        MTIMSU, MTMSU1, CDIFBS, INTFBS, MATVC2, MATVEC, ORTCK,  CINFBS,
C        CMTIMU, AND INVFBS
C     IN ADDITION, INTPK IN VAX, IBM, CDC AND UNIVAC, DOES NOT USE THIS
C     /DESCRP/ LABEL COMMON.
C     STARTING IN 1992 VERSION, LABEL COMMON /DESCRP/ IS COMPLETELY
C     REMOVED FROM ALL NASTRAN SUBROUTINES.
C
C     DESCRP IS A STORAGE BLOCK USED BY SUBROUTINE INTPK.
C     LENGTH = TOTAL NO. OF WORDS IN BLOCK.
C
C     COMMON /DESCRP/ LENGTH, BLOCK(50)
C
C     -------------------     / TWO    /     ---------------------------
C
C                      Moved to module MODSYSTEM.
C
C     -------------------     / NAMES  /     ---------------------------
C
C     NAMES DEFINES VALUES FOR GINO FILE OPTIONS,ARITHMETIC TYPES
C     AND MATRIX FORMS.
C
      COMMON /NAMES /RD    ,RDREW ,WRT   ,WRTREW,REW   ,NOREW ,EOFNRW,
     1               RSP   ,RDP   ,CSP   ,CDP   ,SQUARE,RECT  ,DIAG  ,
     2               LOWER ,UPPER ,SYM   ,ROW   ,IDENT
C
C     -------------------     / TYPE   /     ---------------------------
C
C     TYPE DEFINES PROPERTIES AS A FUNCTION OF ARITHMETIC TYPE.
C     PRC    = PRECISION (1=SP, 2=DP).
C     NWDS   = NO. OF WORDS PER ELEMENT.
C     RC     = ARITHMETIC (1=REAL, 2=COMPLEX).
C     X      = PAD TO DEFINE WORK AREA.
C
      COMMON /TYPE  / PRC(2), NWDS(4), RC(4), X(6)
C
C     -------------------     / BITPOS /     ---------------------------
C
C     BITPOS DEFINES THE BIT POSITIONS FOR THE DEGREES-OF-FREEDOM IN
C     USET, AND HOLLERITH CHARACTERS DESCRIBING DEGREES-OF-FREEDOM.
C
      COMMON /BITPOS/ UM  ,UO  ,UR  ,USG ,USB ,UL  ,UA  ,UF  ,US  ,UN  ,
     1                UG  ,UE  ,UP  ,UNE ,UFE ,UD  ,UPS ,USA ,UK  ,UPA ,
     2                U21 ,U22 ,U23 ,UX  ,UY  ,UFR ,UZ  ,UAB ,UI  ,U30 ,
     3                U31 ,U32 ,
     O                HM  ,HO  ,HR  ,HSG ,HSB ,HL  ,HA  ,HF  ,HS  ,HN  ,
     1                HG  ,HE  ,HP  ,HNE ,HFE ,HD  ,HPS ,HSA ,HK  ,HPA ,
     2                H21 ,H22 ,H23 ,HX  ,HY  ,HFR ,HZ  ,HAB ,HI  ,H30 ,
     3                H31 ,H32
C
C     -------------------     / SOFCOM /     ---------------------------
C
C     SOFCOM DEFINES THE NAMES AND SIZES OF THE SOF FILES AND THE STATE
C     OF THE SOF
C     NFILES = NUMBER OF FILES ALLOCATED TO THE SOF (MAX 10)
C     FILNAM = 4 CHAR. BCD NAMES OF THE SOF FILES
C     FILSIZ = SIZES OF THE SOF FILES EXPRESSED IN AN EVEN NUMBER OF
C              BLOCKS
C     STATUS = SOF STATUS.  0 - SOF IS EMPTY.  1 - SOF IS NOT EMPTY.
C     PSSWRD = BCD PASSWORD FOR THE SOF.  EACH RUN USING THE SAME SOF
C              MUST USE THE SAME PASSWORD.
C     FIRST  = .TRUE. IF SOFINT HAS NOT YET BEEN CALLED TO INITIALIZE
C              THE SOF FOR THIS RUN.  OTHERWISE .FALSE.
C     OPNSOF = .TRUE. IF THE SOF IS OPEN.  .FALSE. IF IT IS CLOSED.
C     ASOFCB = ADDRESS OF SOF CONTROL BLOCKS ON IBM 360/370 COMPUTERS
C
      COMMON /SOFCOM/ NFILES, FILNAM(10), FILSIZ(10), STATUS, PSSWRD(2),
     1                FIRST , OPNSOF    , ASOFCB
C
C     --------------------    / XXREAD /     ---------------------------
C
C                      Moved to module MODSYSTEM.
C
C     --------------     /XECHOX/ AND /XREADX/     ---------------------
C
C     IECHO      = USED IN FREE-FIELD INPUT FOR INPUT CARD ECHO CONTROL
C     IXSORT,IWASFF,NCARD = USED LOCALLY AMONG XSORT, XREAD, AND FFREAD
C     NOECHO     = USED IN FFREAD AND XCSA ROUTINES
C
C     SCREEN,PROM= LOGICAL UNIT FOR TERMINAL SCREEN AND PROMPT SYMBOL
C     CONTRL NOTYET,STAR,PCT = FREE-FIELD INPUT FLAGS USED IN XREAD
C     LOOP,KOUNT = LOOP COUNT USED IN XREAD
C     ICONT      = 36 CONTROL WORDS USED IN FREE-FILED INPUT NOT TO BE
C                  DESTROYED
C
      COMMON /XECHOX/ IECHO(4), IXSORT, IWASFF, NCARD(3), NOECHO
      COMMON /XREADX/ SCREEN  , LOOP  , KOUNT , PROM    , NOTYET, STAR,
     1                PCT     , ICONT(36)
C
C     --------------     /MACHIN/ AND /LHPWX/     ---------------------
C
C                      Moved to module MODSYSTEM.
C
C
C     ==================================================================
C
C     -------------------     / GINOX  /     ---------------------------
C
      DATA    CDC   / 244*0   /
      DATA    OTHERS/ 392*0   /
C
C     -------------------     / XMSSG  /     ---------------------------
C                               1         2         3
C                      1234567890123456789012345678901
      DATA    UFM   / '0*** USER FATAL MESSAGE'        /
      DATA    UWM   / '0*** USER WARNING MESSAGE'      /
      DATA    UIM   / '0*** USER INFORMATION MESSAGE'  /
      DATA    SFM   / '0*** SYSTEM FATAL MESSAGE'      /
      DATA    SWM   / '0*** SYSTEM WARNING MESSAGE'    /
      DATA    SIM   / '0*** SYSTEM INFORMATION MESSAGE'/
C
C     -------------------     /NUMTPX  /     --------------------------
C
      DATA    NBCD  /  0      /
      DATA    BCD   /  19*0   /
C
C     -------------------     /BLANK  /     --------------------------
C
CWKBR DATA    IBLNK /  56*0, 4H CEA, 4HSE E, 4HMPIR, 4HE >  /
      DATA    IBLNK /  96*0, 4H CEA, 4HSE E, 4HMPIR, 4HE >  /
C
C     -------------------     / NTIME  /     ---------------------------
C
CWKBR DATA    LNTIME/ 16      /
CWKBR 9/94 SPR94009      DATA    LNTIME/ 23      /      
      DATA    LNTIME/ 16      /      
CWKBR DATA    TIMDTA/ 16*0.   /
      DATA    TIMDTA/ 23*0.   /
C
C     USE A NASTRAN BULKDATA=-3 CARD TO ACTIVATE TIME CONSTANTS COMPUTA-
C     TION AND PRINT OUT FROM SUBROUTINES TMTSIO AND TMTSLP.
C
C     EXAMPLE - THE FOLLOWING CARDS CAN BE USED FOR UNIVAC 1100/82 IN
C     MSFC TO ELIMINATE HARDWARE TIME COMPUTATION IN EVERY NASTRAN RUN.
C
C     DATA    TIMDTA/  0.51, 15.73, 15.00, 11.10, 10.00,  2.20,  2.23,
C    1                 4.00,  5.28, 15.90, 19.40,  4.00,  5.80, 16.45,
C    2                20.16,  0.00/
C
C     SIMILARLY, THE NEXT TABLE FOR VAX 11/780 MACHINE AT COSMIC SITE
C
C     DATA    TIMDTA/ 12.30,  88.0,  76.0, 78.0 ,  76.0, 16.0 , 30.0 ,
C    1                 7.00,  12.0,  20.0, 35.0 ,   8.0, 12.0 , 24.0 ,
C    2                36.00,  14.2/
C
C     SIMILARLY, THE NEXT TABLE FOR MICRO VAX 3600 MACHINE AT COSMIC
C     SITE
C
C     DATA    TIMDTA/ 12.30,  88.0,  76.0, 78.0 ,  76.0, 16.0 , 30.0 ,
C    1                 7.00,  12.0,  20.0, 35.0 ,   8.0, 12.0 , 24.0 ,
C    2                36.00,   0.0/
C
C
C     AND THE NEXT TABLE FOR IBM 3084 MACHINE AT MSFC SITE
C
C     DATA    TIMDTA/  1.12,  5.28,  4.59,  2.04,  1.86,  1.06,  1.10,
C    1                 0.78,  0.82,  2.69,  2.80,  0.78,  0.87,  2.70,
C    2                 2.82,  0.00/
C
C
C     NOTE - STARTING 1991 VERSION, THESE TIMINGS CONSTANTS CAN BE FED
C     ****   DIRECTLY INTO NASTRAN EXECUTABLE VIA THE NASINFO FILE.
C            THUS, EACH COMPUTER CENTER CAN EASILY SUPPLY THE CORRECT
C            TIMING CONSTANTS FOR ITS MACHINE. (SEE THE WRITE-UP IN THE
C            NASINFO FILE)
C            THE 16TH TIMING IS FOR READING STRING BACKWARD. CURRENTLY
C            NOT USED
C
C     -------------------     / XLINK  /     ---------------------------
C
      DATA    LXLINK/ 220 /,  MAXLNK / 15/,  MXLINK / 220*0 /
C
C     -------------------     / SEM    /     ---------------------------
C
C                      Moved to module MODSEMLNK.
C
C     -------------------     / SYSTEM /     ---------------------------
C
C                      Moved to module MODSYSTEM.
C
C     -------------------     / XFIST  /     ---------------------------
C
      DATA    NFIST / 56 /, LFIST /  56 /,  FIST /
     1        4HPOOL,  0,4HOPTP, -1,4HNPTP, -2,4HUMF , -3,4HNUMF, -4,
     2        4HPLT1, -5,4HPLT2, -6,4HINPT, -7,4HINP1, -8,4HINP2, -9,
     3        4HINP3,-10,4HINP4,-11,4HINP5,-12,4HINP6,-13,4HINP7,-14,
     4        4HINP8,-15,4HINP9,-16,4HXPTD,-17,4HSOF ,-18,4HUT1 ,-19,
     5        4HUT2 ,-20,4HUT3 ,-21,4HUT4 ,-22,4HUT5 ,-23,
C
C     USE VALUES BELOW WHEN ICFIAT (24TH WORD OF /SYSTEM/) IS 8
C    6           201,  3,   202, 11,   203, 19,   204, 27,   205, 35,
C    7        4HCASE, 43,   207, 51,4HPCDB, 59,   208, 67,   209, 75,
C    8           210, 83,   211, 91,   213, 99,   214,107,   215,115,
C    9           216,123,   301,131,   302,  3,   303, 11,   304, 19,
C    O           305, 27,   306, 35,4HXYCD,139,   307, 67,   308, 75,
C    1           309, 83,   310, 91,   311, 99,   312,107,   313,115,
C    2           314,123,   315,147/
C
C     USE VALUES BELOW WHEN ICFIAT (24TH WORD OF /SYSTEM/) IS 11
     6           201,  3,   202, 14,   203, 25,   204, 36,   205, 47,
     7        4HCASE, 58,   207, 69,4HPCDB, 80,   208, 91,   209,102,
     8           210,113,   211,124,   213,135,   214,146,   215,157,
     9           216,168,   301,179,   302,  3,   303, 14,   304, 25,
     O           305, 36,   306, 47,4HXYCD,190,   307, 91,   308,102,
     1           309,113,   310,124,   311,135,   312,146,   313,157,
     2           314,168,   315,201/
C
C     -------------------     / XPFIST /     ---------------------------
C
      DATA    NPFIST/ 24   /
C
C     -------------------     / XXFIAT /     ---------------------------
C
      DATA    XXFIAT/ 24*0 /
C
C     -------------------     / XFIAT  /     ---------------------------
C
C     USE 8*0 EACH INSTEAD OF 5*0 WHEN ICFIAT = 11
C
CWKBR DATA    MFIAT / 0 /, NFIAT / 80 /, LFIAT / 0 /, FIAT /
      DATA    MFIAT / 0 /, NFIAT / 100/, LFIAT / 0 /, FIAT /
     1                0, 4HGEOM, 4H1   , 8*0 ,
     2                0, 4HEPT , 4H    , 8*0 ,
     3                0, 4HMPT , 4H    , 8*0 ,
     4                0, 4HEDT , 4H    , 8*0 ,
     5                0, 4HDIT , 4H    , 8*0 ,
     6                0, 4HCASE, 4HCC  , 3*7 , 2*0 , 3*7 ,
     7                0, 4HDYNA, 4HMICS, 8*0 ,
     8                0, 4HPCDB, 4H    , 8*0 ,
     9                0, 4HGEOM, 4H2   , 8*0 ,
     O                0, 4HGEOM, 4H3   , 8*0 ,
     1                0, 4HGEOM, 4H4   , 8*0 ,
     2                0, 4HGEOM, 4H5   , 8*0 ,
     3                0, 4HFORC, 4HE   , 8*0 ,
     4                0, 4HMATP, 4HOOL , 8*0 ,
     5                0, 4HAXIC, 4H    , 8*0 ,
     6                0, 4HIFPF, 4HILE , 8*0 ,
     7                0, 4HSCRA, 4HTCH1, 8*0 ,
     8                0, 4HXYCD, 4HB   , 8*0 ,
CWKBR9                0, 4HSCRA, 4HTC15, 8*0 , 671*0 /
     9                0, 4HSCRA, 4HTC15, 8*0 , 671*0, 220*0 /
C
C     -------------------     / OSCENT /     ---------------------------
C
      DATA    OSCAR / 200*4H    /
C
C     -------------------     / OUTPUT /     ---------------------------
C
      DATA    OUTPUT/ 224*4H    /
C
C     -------------------     / XDPL   /     ---------------------------
C
      DATA    MDPL  / 1 /,  NDPL / 80 /,  LDPL  / 0 /,  DPL / 240*0 /
C
C     -------------------     / XVPS   /     ---------------------------
C
      DATA    VPS   / 600,  2,   598*0    /
C
C     -------------------     / STAPID /     ---------------------------
C
      DATA    TAPID / 6*0.0 /,   OTAPID  / 6*0.0 /
      DATA    IDUMF /   0   /
C
C     -------------------     / STIME  /     ---------------------------
C
      DATA    TIME  / 2*0.0 /
C
C     -------------------     / XCEITB /     ---------------------------
C
      DATA    CEI   / 42,   2,   40*0  /
C
C     -------------------     / XMDMSK /     ---------------------------
C
      DATA    NMSKCD, NMSKFL, NMSKRF, MSK / 3, 3, 1, 7*0  /
C
C     -------------------     / MSGX   /     ---------------------------
C
      DATA    NMSG  / 0 /,  MSGLG /  40 /,  MSG / 160*0   /
C
C     -------------------     / DESCRP /     ---------------------------
C
C     DATA    LENGTH / 50 /,   BLOCK / 50*0 /
C
C     -------------------     / TWO    /     ---------------------------
C
C                      Moved to module MODSYSTEM.
C
C     -------------------     / NAMES  /     ---------------------------
C
      DATA    RD    / 2 /,  RDREW / 0 /,  WRT   / 3 /,  WRTREW/ 1 /,
     1        REW   / 1 /,  NOREW / 2 /,  EOFNRW/ 3 /,  RSP   / 1 /,
     2        RDP   / 2 /,  CSP   / 3 /,  CDP   / 4 /,  SQUARE/ 1 /,
     3        RECT  / 2 /,  DIAG  / 3 /,  LOWER / 4 /,  UPPER / 5 /,
     4        SYM   / 6 /,  ROW   / 7 /,  IDENT / 8 /
C
C     -------------------     / TYPE   /     ---------------------------
C
      DATA    PRC   /     1,     2/      ,
     1        NWDS  /     1,     2,     2,     4/,
     2        RC    /     1,     1,     2,     2/,
     3        X     /     6*0.0   /
C
C     -------------------     / BITPOS /     ---------------------------
C
      DATA    UM  /32/  ,  HM  /2HM /  ,     UPS /16/  ,  HPS /2HPS/  ,
     1        UO  /30/  ,  HO  /2HO /  ,     USA /15/  ,  HSA /2HSA/  ,
     2        UR  /29/  ,  HR  /2HR /  ,     UK  /14/  ,  HK  /2HK /  ,
     3        USG /23/  ,  HSG /2HSG/  ,     UPA /13/  ,  HPA /2HPA/  ,
     4        USB /22/  ,  HSB /2HSB/  ,     U21 /10/  ,  H21 /4HXXXX/,
     5        UL  /24/  ,  HL  /2HL /  ,     U22 /11/  ,  H22 /4HYYYY/,
     6        UA  /25/  ,  HA  /2HA /  ,     U23 /12/  ,  H23 /4HZZZZ/,
     7        UF  /26/  ,  HF  /2HF /  ,     UX  / 9/  ,  HX  /2HX /  ,
     8        US  /31/  ,  HS  /2HS /  ,     UY  / 8/  ,  HY  /2HY /  ,
     9        UN  /27/  ,  HN  /2HN /  ,     UFR / 7/  ,  HFR /2HFR/  ,
     O        UG  /28/  ,  HG  /2HG /  ,     UZ  / 6/  ,  HZ  /2HZ /  ,
     1        UE  /21/  ,  HE  /2HE /  ,     UAB / 5/  ,  HAB /2HAB/  ,
     2        UP  /20/  ,  HP  /2HP /  ,     UI  / 4/  ,  HI  /2HI /  ,
     3        UNE /19/  ,  HNE /2HNE/  ,     U30 / 3/  ,  H30 /2HU3/  ,
     4        UFE /18/  ,  HFE /2HFE/  ,     U31 / 2/  ,  H31 /2HU2/  ,
     5        UD  /17/  ,  HD  /2HD /  ,     U32 / 1/  ,  H32 /2HU1/
C
C     -------------------     / SOFCOM /     ---------------------------
C
      DATA    NFILES/ 1          /
      DATA    FILNAM/ 4HINPT,9*0 /
      DATA    FILSIZ/ 100   ,9*0 /
      DATA    STATUS/ 1          /
      DATA    PSSWRD/ 2*4H       /
      DATA    FIRST / .TRUE.     /
      DATA    OPNSOF/ .FALSE.    /
      DATA    ASOFCB/ 0          /
C
C     --------------------    / XXREAD /     ---------------------------
C
C                      Moved to module MODSYSTEM.
C
C     --------------     /XECHOX/ AND /XREADX/     ---------------------
C
      DATA    IECHO , IXSORT, IWASFF, NCARD, NOECHO                /
     1        4*0,    0,      0,      3*0,   0                     /
      DATA    SCREEN, LOOP, KOUNT, PROM, NOTYET, STAR, PCT, ICONT  /
     1        6,      -1,   0,     0,    3*.FALSE.,         36*0   /
C
C     --------------     /MACHIN/ AND /LHPWX/     ---------------------
C
C                      Moved to module MODSYSTEM.
C
C
      END
