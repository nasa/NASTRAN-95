      SUBROUTINE DBASE
C
C     DRIVER FOR DATABASE MODULE
C
C     THIS UTILITY MODULE TRANSFERS GRID POINT DATA, CONNECTING ELEMENT
C     DATA, AND MOST OF THE OFP DATA BLOCKS (DISPLACEMENT, VELOCITY,
C     ACCELERTION, LOAD, GRID POINT FORCE, EIGENVECTOR, ELEMENT STRESS
C     AND ELEMENT FORCE) TO A FORTRAN FILE, FORMATTED OR UNFORMATTED.
C     THE GRID POINT DATA ARE IN BASIC COORDINATE SYSTEM, AND THE
C     DISPLACEMENT DATA IF REQUESTED, CAN BE IN BASIC SYSTEM (DEFAULT)
C     OR IN GLOBAL COORDINATE SYSTEM. GRID POINTS ARE IN EXTERNAL GRID
C     NUMBERING SYSTEM.
C     THE FORMATTED OUTTP FILE CAN BE PRINTED, OR EDITTED BY SYSTEM
C     EDITOR. ALL OUTPUT LINES ARE 132 COLUMNS OR LESS.
C
C
C     WRITTEN ON THE LAST DAY OF 1988 BY G.CHAN/UNISYS.
C     REVISED 10/89, EXPANDED TO INCLUDE THREE OFP FILES
C
C     DATABASE  EQEXIN,BGPDT,GEOM2,CSTM,O1,O2,O3//C,N,OUTTP/C,N,FORMAT
C                                                /C,N,BASIC   $
C
C               EQEXIN - MUST BE PRESENT
C               BGPDT  - IF PURGE, NO GRID POINT DATA SENT TO OUTTP
C               GEOM2  - IF PURGE, NO ELEMENT CONNECTIVITY DATA SENT TO
C                        OUTTP
C               CSTM   - IF PURGE, DISPLACEMENT VECTOR IN GLOBAL COORD.
C               Oi     - ANY ONE OF NASTRAN STANDARD OFP FILES LISTED
C                        BELOW. IF PURGE, NO DATA SENT TO OUTTP.
C                        IF THE DATA IN THIS OFP FILE IS COORDINATE
C                        SENSITIVE, SUCH AS DISPLACEMENT, THE DATA CAN
C                        BE SENT OUT TO OUTTP IN BASIC OR GLOBAL
C                        COORDINATES AS SPECIFIED THE PARAMETER BASIC.
C               OUTTP  - MUST BE ONE OF THE UT1,UT2,INPT,INP1,...,9 FILE
C               FORMAT = 0, UNFORMATTED OUTPUT TO OUTTP FILE (DEFAULT)
C                      = 1, FORMATTED
C               BASIC  = 0, DISPLACEMENT VECTORS REMAIN IN GLOBAL COORD.
C                           SYSTEM (DEFAULT)
C                      = 1, DISPLACEMENT VECTORS IN BASIC COORD. SYSTEM
C                           (NOT USED IN ELEMENT FORCES AND STRESSES)
C
C     LIST OF AVAILABLE OFP FILES (Oi)
C          OUDV1,  OUDVC1, OUGV1,  OUHV1,  OUHVC1, OUPV1,  OUPVC1,
C          OUDV2,  OUDVC2, OUGV2,  OUHV2,  OUHVC2, OUPV2,  OUPVC2,
C          OUBGV1, OPHID,  OPHIG,  OPHIH,  OCPHIP,
C          OPG1,   OPP1,   OPPC1,  OQG1,   OQP1,   OQPC1,  OQBG1,
C          OPG2,   OPP2,   OPPC2,  OQG2,   OQP2,   OQPC2,  OQBG2,
C          OEF1,   OEFC1,  OES1,   OESC1,  OEFB1,  OBEF1,
C          OEF2,   OEFC2,  OES2,   OESC2,  OESB1,  OBES1
C          OES1A,
C          HOUDV1, HOUGV1, HOPG1,  HOQG1,  HOEF1,  HOES1,  HOPNL1,
C          HOUDV2, HOUGV2, HOPP2,  HOQP2,  HOEF2,  HOEFIX, HOPNL2
C
C
C     MAP THIS ROUTINE IN LINK2, LINK4 AND LINK14
C
      IMPLICIT INTEGER (A-Z)
      LOGICAL          FMTTD,BASC,NOCSTM,NOBGPT,NOGEOM,DEBUG,EFS,ECXYZ
      INTEGER          SUB(2),B(5),NAM(8),A(10),FMT(4),ONAME(6),SUBN(3),
     1                 A1(80),F(79),F8(6),INPX(3),IX(1),FSTF(4),INP(8)
      REAL             RX(200),RZ(1),RA(1),T(9),FREQ
      CHARACTER*8      CA,MO,CAMO,BA,GL,BAGL,GPT,ELM,DIS,LOD,FORC,VELO,
     1                 ACC,EIGN,STR,ELF,DXX,DYY(3),DASH,BLK8
      CHARACTER        UFM*23,UWM*25,UIM*29
      COMMON /XMSSG /  UFM,UWM,UIM
      COMMON /SYSTEM/  SYSBUF,NOUT,NOGO
      COMMON /ZZZZZZ/  Z(1)
      COMMON /BLANK /  OUTTP,FORMTD,BASIC
      COMMON /GPTA1 /  NEL,LAST,INCR,E(1)
      COMMON /MACHIN/  MACH
      COMMON /NAMES /  RD,RDREW,WRT,WRTREW,REW,NOREW,EOFNRW
      EQUIVALENCE      (Z(1),RZ(1)),       (B(1),NAM(1)),
     1                 (A(1),RA(1),A1(3)), (RX(1),IX(1))
      DATA    EQEXIN,  BGPDT, GEOM2,  CSTM,  SCR1,  SUB              /
     1        101,     102,   103,    104,   301,   4HDBAS,4HE       /
      DATA    END1,    END2,  END3,   FMT                            /
     1        4H -EN,  2HD-,  2H--,   4H, UN,4HFORM,4HATTE,1HD       /
      DATA    FMT1,    MONE,  BLANK,  BZERO, IZERO,        DEBUG     /
     1        1H,,     -1,    4H    , 4H 0.0,4H-0  ,       .FALSE.   /
      DATA    LS,      INPX,                 LIMAF,        LIMRX     /
     1        1HS,     4H INP,4HINPT, 4H  UT,    78,       200       /
      DATA    GPT,            ELM,           DIS,          DASH      /
     1        'GRID PTS',     'ELEMENTS',    'DISPLCNT',  '--------' /
      DATA    LOD,            FORC,          VELO,         BLK8      /
     1        'LOADINGS',     'GD FORCE',    'VELOCITY',  '        ' /
      DATA    ACC,            EIGN,          STR,          ELF       /
     1        'ACCELERN',     'EIGENVCR',    'E.STRESS',  'E.FORCES' /
      DATA    CA,             MO,            BA,           GL        /
     1        ' CASE = ',     ' MODE = ',    '  BASIC ',  ' GLOBAL ' /
      DATA    FSTF / 4H1ST , 4H2ND ,4H3RD ,  4H4TH /   ,   INP       /
     1        4HEQEX,2HIN  , 4HBGPD,2HT  ,   4HGEOM,4H2   ,4HCSTM,1H /
C
C
      IF (DEBUG) WRITE (NOUT,10)
   10 FORMAT (/5X,'-- DBASE LOCAL DEBUG --')
      NAM(1) = 106
      CALL RDTRL (NAM(1))
      IF (NAM(1) .GT. 0) GO TO 20
      CALL PAGE
      WRITE  (NOUT,15) UIM
   15 FORMAT (A29,', DATABASE NEW DMAP FORMAT', //5X,
     1       'DATABASE   EQEXIN,BGPDT,GEOM2,CSTM,O1,O2,O3//C,N,OUTTP/',
     2       'C,N,FORMAT/C,N,BASIC  $', /5X,'FIRST 4 FILES ARE FIXED ',
     3       'IN NAMES AND ORDER, NEXT 3 FILES CAN BE SELECTED BY USER',
     4       /5X,'FIRST EQEXIN FILE MUST BE PRESENT, OTHERS CAN BE ',
     5       'SELECTIVELY OMITTED')
   20 IF (OUTTP.GE.11 .AND. OUTTP.LE.24) GO TO 30
      WRITE  (NOUT,25) UFM,OUTTP
   25 FORMAT (A23,', OUTPUT FILE SPEC. ERROR')
      CALL MESAGE (-37,0,SUB)
   30 EFS   = .FALSE.
      FMTTD = .FALSE.
      BASC  = .FALSE.
      ECXYZ = .FALSE.
      IF (FORMTD .EQ. 1) FMTTD = .TRUE.
      IF (BASIC  .EQ. 1) BASC  = .TRUE.
      IF (FMTTD) FMT(1) = FMT1
      CALL FNAME (101,NAM(1))
      IF (NAM(1).EQ.INP(1) .AND. NAM(2).EQ.INP(2)) GO TO 34
      CALL PAGE2 (3)
      WRITE  (NOUT,32) FSTF(1),NAM(1),NAM(2)
   32 FORMAT (//,' *** USER FATAL ERROR IN DATABASE MODULE, THE ',A4,
     1  'INPUT DATA BLOCK ',2A4,' IS ILLEGAL.', /5X,'THE FIRST 4 INPUT',
     2  ' DATA BLOCKS MUST BE ''EQEXIN,BGPDT,GEOM2,CSTM'', AND IN ',
     3  'EXACT ORDER SHOWN')
      NOGO   = 1
   34 NOBGPT = .FALSE.
      NOGEOM = .FALSE.
      NOCSTM = .FALSE.
      NAM(1) = BGPDT
      CALL RDTRL (NAM)
      IF (NAM(1) .LE. 0) NOBGPT = .TRUE.
      IF (NOBGPT) GO TO 35
      CALL FNAME (102,NAM(1))
      IF (NAM(1).EQ.INP(3) .AND. NAM(2).EQ.INP(4)) GO TO 35
      CALL PAGE2 (3)
      WRITE (NOUT,32) FSTF(2),NAM(1),NAM(2)
      NOGO = 1
   35 NAM(1) = GEOM2
      CALL RDTRL (NAM)
      IF (NAM(1) .LE. 0) NOGEOM = .TRUE.
      IF (NOGEOM) GO TO 36
      CALL FNAME (103,NAM(1))
      IF (NAM(1).EQ.INP(5) .AND. NAM(2).EQ.INP(6)) GO TO 36
      CALL PAGE2 (3)
      WRITE (NOUT,32) FSTF(3),NAM(1),NAM(2)
      NOGO = 1
   36 NAM(1) = CSTM
      CALL RDTRL (NAM)
      IF (NAM(1) .LE. 0) NOCSTM = .TRUE.
      IF (NOCSTM) GO TO 37
      CALL FNAME (104,NAM(1))
      IF (NAM(1).EQ.INP(7) .AND. NAM(2).EQ.INP(8)) GO TO 37
      CALL PAGE2 (3)
      WRITE (NOUT,32) FSTF(4),NAM(1),NAM(2)
      NOGO = 1
   37 IF (NOGO .EQ. 1) RETURN
C
      NZ    = KORSZ(Z(1))
      BUF1  = NZ   - SYSBUF
      BUF2  = BUF1 - SYSBUF
      NZ    = BUF2 - 1
      COOR  = 0
C
C     OPEN EQEXIN, READ FIRST RECORD, AND SORT EX-INT TABLE BY INTERNAL
C     NUMBERS, Z(1) THRU Z(NEQ)
C
      FILE = EQEXIN
      CALL OPEN   (*1300,EQEXIN,Z(BUF1),RDREW)
      CALL FWDREC (*1300,EQEXIN)
      CALL READ   (*1300,*60,EQEXIN,Z(1),NZ,1,NEQ)
      J = 0
   40 CALL READ (*1300,*50,EQEXIN,Z(1),NZ,1,NEQ)
      J = J + NZ
      GO TO 40
   50 J = J + NEQ
      J = J*2
      CALL MESAGE (-8,J,SUB)
C
   60 CALL CLOSE (EQEXIN,REW)
      LEFT = NZ - NEQ - 1
      NEQ2 = NEQ/2
      J = NEQ2*5 - LEFT
      IF (J .GT. 0) CALL MESAGE (-8,J,SUB)
      CALL SORT (0,0,2,2,Z(1),NEQ)
C
C     IF BGPDT FILE NOT REQUESTED, SKIP PROCESSING GRID POINT DATA
C
      IF (NOBGPT) GO TO 170
C
C
C     GRID POINTS PROCESSING
C     ======================
C
C     OPEN BGPDT, READ THE ENTIRE RECORD, AND REPLACE THE COORD.SYSTEM
C     WORD BY THE EXTERNAL GRID POINT NUMBER.
C     NOTE - EXT.GRID IDS ARE NO LONGER SORTED.
C     WRITE THE NEW DATA TO SCR1 FILE - EXT.GIRD ID, X,Y,Z BASIC COORD.
C
      FILE = SCR1
      CALL OPEN (*1300,SCR1,Z(BUF1),WRTREW)
      FILE  = BGPDT
      CALL OPEN (*170,BGPDT,Z(BUF2),RDREW)
      CALL FWDREC (*1300,BGPDT)
      NGD   = 0
  100 CALL READ (*110,*110,BGPDT,B(2),4,0,FLAG)
      NGD  = NGD + 1
      K    = NGD*2 - 1
      B(1) = Z(K)
      B(2) = 0
      CALL WRITE (SCR1,B,5,0)
      GO TO 100
  110 CALL WRITE (SCR1,0,0,1)
      CALL CLOSE (SCR1 ,REW)
      CALL CLOSE (BGPDT,REW)
C
C     OPEN SCR1 AND OUTTP
C     SORT THE GRID POINT DATA BY THEIR EXTERNAL NUMBERS
C
C     FOR UNFORMATTED TPAE, TRANSFER GRID DATA FROM SCR1 TO OUTTP IN ONE
C     LONG RECORD
C
C          WORD         CONTENT (UNFORMATTED, 2ND RECORD)
C        ------    ----------------------------------------------------
C             1     NO. OF WORDS (THIS FIRST WORD NOT INCLUDED) IN THIS
C                   RECORD (INTEGER)
C             2     EXTERNAL GRID ID (SORTED)
C             3     0 (NOT USED, RESERVED FOR FUTURE USE. INTEGER)
C         4,5,6     X,Y,Z COORDINATES IN BASIC COORD SYSTEM (REAL)
C             :     REPEAT 2 THRU 6 AS MANY TIMES AS THERE ARE GRIDS.
C
      FILE = SCR1
      JB   = NEQ+ 1
      JBP1 = JB + 1
      JBM1 = JB - 1
      K    = NGD*5
      CALL OPEN (*1300,SCR1,Z(BUF1),RDREW)
      CALL READ (*1300,*1310,SCR1,Z(JBP1),K,1,FLAG)
      CALL CLOSE (SCR1,REW)
      CALL SORT (0,0,5,1,Z(JBP1),K)
C
C     FIRST GRID POINT IDENTIFICATION RECORD TO OUTTP
C
      IF (.NOT.FMTTD) WRITE (OUTTP    ) GPT,DASH
      IF (     FMTTD) WRITE (OUTTP,120) GPT,DASH
  120 FORMAT (1X,2A8)
C
      IF (FMTTD) GO TO 130
      Z(JB) = K
      JE    = K + JB
      WRITE (OUTTP) (Z(J),J=JB,JE)
      GO TO 170
C
C     FOR FORMATTED TAPE
C
C       RECORD   WORD     CONTENT                               FORMAT
C       ------   ----    ----------------------------------------------
C           2      1      TOTAL NUMBER OF GRID POINTS             I8
C           3      1      EXTERNAL GRID ID (NOT SORTED)           I8
C                  2      0 (NOT USED, RESERVED FOR FUTURE USE)   I8
C                3,4,5    X,Y,Z COORDINATES IN BASIC SYSTEM  3(1P,E12.5)
C           :     1-5     REPEAT RECORD 3 AS MANY TIMES AS THERE
C                         ARE GRIDS
C
  130 WRITE  (OUTTP,140) NGD
  140 FORMAT (1X,I8,'= TOTAL NUMBER OF GRID POINTS')
      K = JB
      DO 160 I = 1,NGD
      WRITE  (OUTTP,150) Z(K+1),Z(K+2),RZ(K+3),RZ(K+4),RZ(K+5)
  150 FORMAT (1X,2I8,3(1P,E12.5))
  160 K = K + 5
C
C     IF GEOM2 IS NOT REQUESTED, SKIP PROCESSING ELEMENT DATA
C
  170 IF (NOGEOM) GO TO 490
C
C
C     ELEMENT CONNECTIVITY PROCESSING
C     ===============================
C
C     OPEN GEOM2 AND SCR1. TRANSFER ELEMENT DATA TO SCR1 FILE
C
      FILE = GEOM2
      CALL OPEN (*490,GEOM2,Z(BUF2),RDREW)
      CALL FWDREC (*1300,GEOM2)
C
C     FIRST ELEMENT IDENTIFICATION RECORD TO OUTTUP
C
      IF (.NOT.FMTTD) WRITE (OUTTP    ) ELM,DASH
      IF (     FMTTD) WRITE (OUTTP,120) ELM,DASH
C
  200 CALL READ (*420,*420,GEOM2,B,3,0,FLAG)
      IF (B(1).EQ.B(2) .AND. B(2).EQ.B(3)) GO TO 420
      DO 210 I = 4,LAST,INCR
      IF (B(1) .EQ. E(I)) GO TO 220
  210 CONTINUE
      CALL MESAGE (-61,0,0)
  220 NAM(1) = E(I-3)
      NAM(2) = E(I-2)
      ELTYP  = E(I-1)
      NWDS   = E(I+2)
      PID    = E(I+3)
      SYMBOL = E(I+12)
      NG     = E(I+6)
      G1     = E(I+9) - 1
      NG3    = NG +3
      NE     = 0
      MID    = 0
C               TETRA,WEDGE,HEXA1,HEXA2            FHEX1          FHEX2
      IF (ELTYP.GE.39 .AND. ELTYP.LE.42 .OR. ELTYP.EQ.76.OR.ELTYP.EQ.77)
     1   MID = 2
      NAM(3) = ELTYP
      NAM(4) = SYMBOL
      NAM(5) = NG
      NAM(6) = NE
      NAM(7) = NG3
      NAM(8) = 1
      IF (NG .GT. 13) NAM(8) = 2
      IF (NG .GT. 28) NAM(8) = 3
C
C     FOR UNFORMATTED TAPE -
C
C     ELEMENT HEADER RECORD WRITTEN TO SCR1
C
C        WORD        CONTENT  (UNFORMATTED)
C        ----    ----------------------------------------------------
C         1-2     ELEMENT BCD NAME
C           3     ELEMENT TYPE NUMBER, ACCORDING TO GPTABD ORDER
C           4     ELEMENT SYMBOL (2 LETTERS)
C           5     NG= NUMBER OF GRID POINTS
C           6     NE= TOTAL NO. OF ELEMENTS OF THIS CURRENT ELEMENT TYPE
C           7     NO. OF WORDS IN NEXT RECORD PER ELEMENT = NG+2
C           8     NO. OF 132-COLUMN LINES NEEDED IN NEXT RECORD IF OUTTP
C                 IS WRITTED WITH A FORMAT
C
      FILE  = SCR1
      CALL OPEN (*1300,SCR1,Z(BUF1),WRTREW)
      CALL WRITE (SCR1,NAM,8,0)
      FILE  = GEOM2
  230 CALL READ (*490,*250,GEOM2,A,NWDS,0,FLAG)
      A1(1) = A(1)
      A1(2) = A(2)
      A1(3) = 0
      IF (PID .EQ. 0) A1(2) = 0
      IF (MID .EQ. 2) A1(2) =-A(2)
      DO 240 J = 1,NG
  240 A1(J+3) = A(G1+J)
      CALL WRITE (SCR1,A1,NG3,0)
      NE = NE + 1
      GO TO 230
  250 CALL WRITE (SCR1,0,0,1)
      CALL CLOSE (SCR1,REW)
      FILE = SCR1
      CALL OPEN (*1300,SCR1,Z(BUF1),RDREW)
      CALL READ (*1300,*290,SCR1,Z(JB),LEFT,1,NWDS)
      CALL BCKREC (SCR1)
      IF (.NOT.FMTTD) GO TO 370
      J = 0
      CALL READ (*1300,*270,SCR1,Z(JB),LEFT,0,FLAG)
  270 CALL READ (*1300,*280,SCR1,Z(JB),LEFT,0,FLAG)
      J = J + LEFT
      GO TO 270
  280 J = J + FLAG
      CALL MESAGE (-8,J,SUB)
  290 CALL CLOSE (SCR1,REW)
      Z(JB+5) = NE
      IF (FMTTD) GO TO 300
      K = JB + 7
      WRITE (OUTTP) (Z(J),J=JB,K)
      I = K + 1
      K = NWDS + JB - 1
      WRITE (OUTTP) (Z(J),J=I,K)
      GO TO 200
C
C     ELEMENT RECORD TO SCR1
C
C       WORD      CONTENT, ALL INTEGERS  (UNFORMATTED)
C       ----    ------------------------------------------------
C         1      ELEMENT ID
C         2      POSITIVE INTEGER  = PROPERTY ID
C                ZERO IF ELEM HAS NO PROPERTY ID
C                NEGATIVE INTEGER  = MATERIAL ID (ELEMENT HAS NO
C                  PROPERTY ID, BUT IT HAS A MATERIAL ID)
C         3      0 (NOT USED. RESERVED FOR FUTURE USE)
C       4,5,...  ELEMENT CONNECTING GRID POINTS
C         :      REPEAT 1,2,3,4,... AS MANY TIMES AS THERE ARE ELEMENTS
C                  OF THIS SAME TYPE
C
C
C
C     FOR FORMATTED TAPE -
C
C     ELEMENT HEADER RECORD, IN 8-COLUMN FORMAT
C     (LINE ---+++ IS FOR VIDEO AID, NOT PART OF A RECORD)
C
C     --------++++++++--------++++++++--------++++++++--------++++++++
C     ELEMENT CBAR      TYPE =  34  BR GRIDS =       2 TOTAL = ETC...
C
C       RECORD  COLUMNS    CONTENT                             FORMAT
C       ------  -------  -----------------------------------------------
C          2      1- 8   'ELEMENT '                          8 LETTERS
C                 9-16   ELEMENT NAME                             2A4
C                17-24   '  TYPE ='                          8 LETTERS
C                25-28   ELEM. TYPE NO. ACCORDING TO GPTABD        I4
C                29,30   BLANK                                     2X
C                31-32   ELEMENT SYMBOL                            A2
C                33-40   ' GRIDS ='                          8 LETTERS
C                41-48   NO. OF GRIDS PER ELEMENT                  I8
C                49-56   ' TOTAL ='                          8 LETTERS
C                57-64   TOTAL NO. OF ELEMENTS OF THIS ELEM. TYPE  I8
C                65-72   ' WDS/EL='                          8 LETTERS
C                73-80   NO. OF WORDS PER ELEMENT IN NEXT RECORDS  I8
C                81-88   ' LINES ='                          8 LETTERS
C                89-96   NO. OF LINES (RECORDS) NEEDED ON NEXT     I8
C                        RECORD FOR THIS ELEMENT TYPE
C
C     ELEMENT RECORD
C     THERE SHOULD BE (TOTAL X LINES) RECORDS IN THIS GROUP
C
C       RECORD  WORD      CONTENT                               FORMAT
C       ------  ----     -----------------------------------------------
C          3      1       ELEMENT ID                               I8
C                 2       POSITIVE INTEGER  = PROPERTY ID          I8
C                         ZERO IF ELEM HAS NO PROPERTY ID
C                         NEGATIVE INTEGER  = MATERIAL ID (ELEMENT HAS
C                            NO PROPERTY ID, BUT IT HAS A MATERIAL ID)
C                 3       0 (NOT USED. RESERVED FOR FUTURE USE)    I8
C              4,5,...16  FIRST 13 EXTERNAL CONNECTING GRID PTS.  13I8
C          4              (IF NEEDED)
C              1,2,...15  NEXT 15 GRID POINTS                  8X,15I8
C          5              (IF NEEDED)
C              1,2,...15  MORE GRID POINTS                     8X,15I8
C
C
C     REPEAT FORMATTED RECORD 3 (AND POSSIBLE 4 AND 5) AS MANY TIMES AS
C     THERE ARE ELEMENTS
C
  300 WRITE  (OUTTP,310) (Z(J+JBM1),J=1,8)
  310 FORMAT (1X,'ELEMENT ',2A4,'  TYPE =',I4,2X,A2,' GRIDS =',I8,
     1        ' TOTAL =',I8,' WDS/EL=',I8,' LINES =',I8)
      I  = JB + 8
      DO 360 J = 9,NWDS,NG3
      JE = I + NG3 - 1
      IF (NG3 .GT. 16) GO TO 330
      WRITE (OUTTP,320,ERR=1390) (Z(K),K=I,JE)
C
C     320  FORMAT (1X,16I8,/,(1X,8X,15I8))
C     THIS FORMAT MAY CAUSE AN EXTRA LINE IN SOME MACHINE IF NG3=16
C
  320 FORMAT (1X,16I8)
      GO TO 360
  330 J16 = I + 15
      J17 = I + 16
      WRITE  (OUTTP,320,ERR=1390) (Z(K),K= I,J16)
      IF (NG3 .GT. 31) GO TO 350
      WRITE  (OUTTP,340,ERR=1390) (Z(K),K=J17,JE)
  340 FORMAT (1X,8X,15I8)
      GO TO 200
  350 J31 = I + 30
      J32 = I + 31
      WRITE (OUTTP,340,ERR=1390) (Z(K),K=J17,J31)
      WRITE (OUTTP,340,ERR=1390) (Z(K),K=J32,JE )
  360 I = JE + 1
      GO TO 200
C
C     BYPASSING INSUFF. CORE SITUATION, FORMATTED TAPE ONLY
C
  370 CALL READ (*1300,*1300,SCR1,A,8,0,FLAG)
      A(6) = NE
      WRITE (OUTTP,310,ERR=1390) (A(J),J=1,8)
  380 CALL READ (*1300,*410,SCR1,A,NG3,0,FLAG)
      IF (NG3  .GT. 16) GO TO 390
      WRITE (OUTTP,320,ERR=1390) (A(J),J=1,NG3)
      GO TO 380
  390 WRITE (OUTTP,320,ERR=1390) (A(J),J=1,16)
      IF (NG3  .GT. 32) GO TO 400
      WRITE (OUTTP,340,ERR=1390) (A(J),J=17,NG3)
      GO TO 380
  400 WRITE (OUTTP,340,ERR=1390) (A(J),J=17,32)
      WRITE (OUTTP,340,ERR=1390) (A(J),J=33,NG3)
      GO TO 380
  410 CALL CLOSE (SCR1,REW)
      GO TO 200
C
C
C     LAST RECORD FOR ELEMENT DATA, UNFORMATTED AND FORMATTED
C
C     --------++++++++--------++++++++--------++++++++--------++++++++
C     ELEMENT -END-     TYPE =   0  -- GRIDS =       0 TOTAL =  ETC...
C
  420 CALL CLOSE (GEOM2,REW)
      DO 430 I = 3,8
  430 NAM(I) = 0
      NAM(1) = END1
      NAM(2) = END2
      NAM(4) = END3
      IF (.NOT.FMTTD) WRITE (OUTTP    ) NAM
      IF (     FMTTD) WRITE (OUTTP,310) NAM
C
C
C     PROCESS OFP DATA BLOCKS   SIGNITURE
C     =======================   =========
C       DISPLACEMENT                 1
C       VELOCITIES                  10
C       ACCELERATIONS               11
C       LOADS                        2
C       GRID POINT OR SPC FORCES     3
C       EIGENVECTORS                 7
C       ELEMENT STRESSES, AND        5
C       ELEMENT STRAIN              21
C       ELEMENT FORCES               4
C
C    (GINO INPUT FILE 105,106,107)
C
  490 OFPSET = 0
      OFP  = 0
C
C     SETUP 500-1000 BIG LOOP FOR 3 OFP DATA BLOCKS
C
  500 OFP  = OFP + 1
      OFPX = CSTM + OFP
      NAM(1) = OFPX
      CALL RDTRL (NAM)
C
C     SKIP CURRENT OFP DATA BLOCK IF IT IS PURGED
C
      IF (NAM(1) .LE. 0) GO TO 1000
C
      FILE = OFPX
      CALL OPEN (*1000,OFPX,Z(BUF1),RDREW)
      CALL FWDREC (*980,OFPX)
      JOS  = 2*OFPSET + 1
      OFPSET = OFPSET + 1
      CALL FNAME (OFPX,ONAME(JOS))
      IF (BASC  .AND. NOBGPT .AND. .NOT.NOCSTM) GO TO 660
      IF (NOBGPT .OR. NOCSTM) BASC = .FALSE.
      KOUNT = 0
  510 KOUNT = KOUNT + 1
      FILE  = OFPX
      DO 515 I = 1,6
  515 F8(I) = 0
C
C     IDENTIFY CURRENT OFP DATA BLOCK IS A DISPLACEMENT FILE OR A NON-
C     DISPLACEMENT FILE
C
      CALL READ (*980,*980,OFPX,A,10,0,FLAG)
      DSPL = MOD(A(2),100)
      NWDS = A(10)
      DXX  = BLK8
      IF (NWDS.NE.8 .AND. NWDS.NE.14) GO TO 530
C
C     CURRENT OFP DATA BLOCK IS A DISPLACEMENT FILE
C
      CALL BCKREC (OFPX)
      IF (DSPL .EQ. 1) DXX = DIS
      IF (DSPL .EQ. 2) DXX = LOD
      IF (DSPL .EQ. 3) DXX = FORC
      IF (DSPL.EQ. 7 .OR. DSPL.EQ.14) DXX = EIGN
      IF (DSPL.EQ.15 .OR. DSPL.EQ.10) DXX = VELO
      IF (DSPL.EQ.16 .OR. DSPL.EQ.11) DXX = ACC
      IF (DXX .EQ. BLK8) GO TO 530
      F(1) = 1
      F(2) = 1
      DO 520 I = 3,NWDS
  520 F(I) = 2
      F8(1) = 11222222
      KK  = 1
      NA4 = 22
      IF (NWDS .EQ. 8) GO TO 600
      F8(2) = 22222200
      KK  = 2
      NA4 = 40
      GO TO 600
C
C     CURRENT OFP DATA BLOCK IS STRESS OR EL FORCE FILE.
C     THE DATA RECORDS HAVE VARIABLE LENGTH (I.E NWDS IS NOT A CONSTANT
C     OF 8 OR 14)
C     CONSTRUCT THE FORMAT CODE IN F AND F8
C           1 = INTEGER
C           2 = REAL
C           3 = BCD
C     AND TURN OFF GLOBAL TO BASIC CONVERSION FLAG BASC
C
  530 IF (DSPL .EQ. 4) DXX = ELF
      IF (DSPL .EQ. 5) DXX = STR
      IF (DXX  .EQ.  BLK8) GO TO 1260
      IF (NWDS .GT. LIMAF) GO TO 1350
      IF (BASC) GO TO 1370
      EFS  = .TRUE.
      CALL FWDREC (*980,OFPX)
      CALL READ (*980,*980,OFPX,A,NWDS,0,FLAG)
      DO 540 I = 1,NWDS
      J = NUMTYP(A(I))
      IF (J.EQ.0 .AND. I.GT.1) J = F(I-1)
  540 F(I)  = J
      IF (DEBUG) WRITE (NOUT,545) NWDS,(F(I),I=1,NWDS)
  545 FORMAT (/,' NWDS/@540=',I3,' F=',50I2, /,(14X,50I2))
      AGAIN = 0
      CALL READ (*980,*570,OFPX,A,NWDS,0,FLAG)
      DO 550 I = 1,NWDS
      J = NUMTYP(A(I))
      IF (F(I) .EQ. J) GO TO 550
      IF (J    .NE. 0) F(I) = -J
      AGAIN = 1
  550 CONTINUE
      IF (AGAIN .EQ. 0) GO TO 570
      CALL READ (*980,*570,OFPX,A,NWDS,0,FLAG)
      DO 560 I = 1,NWDS
      IF (F(I) .GT. 0) GO TO 560
      J = NUMTYP(A(I))
      IF (J .NE. 0) F(I) = J
  560 CONTINUE
      IMHERE = 560
      IF (DEBUG) WRITE (NOUT,545) IMHERE,(F(I),I=1,NWDS)
  570 F(NWDS+1) = -9
      CALL BCKREC (OFPX)
      CALL BCKREC (OFPX)
      NA4= 0
      KK = 0
      DO 580 I = 1,NWDS,8
      KK = KK + 1
      K  = I  + 7
      IF (K .GT. NWDS) K = NWDS
      L  = 10000000
      DO 580 J = I,K
      F8(KK) = F8(KK) + F(J)*L
      NA4 = NA4 + F(J)+ 1
      IF (F(J) .EQ. 3) NA4 = NA4 - 3
  580 L  = L/10
      IF (DEBUG) WRITE (NOUT,590) NA4,(F8(I),I=1,KK)
  590 FORMAT (/,'  NA4 =',I4,'  FORMAT CODE/@590 =',6I10)
C
  600 IF (KOUNT .GT. 1) GO TO 605
      IF (.NOT.FMTTD) WRITE (OUTTP    ) DXX,DASH
      IF (     FMTTD) WRITE (OUTTP,120) DXX,DASH
C
  605 IF (ECXYZ) GO TO 680
      ECXYZ = .TRUE.
      NCSTM = 0
      NSUB  = 0
      IF (.NOT.BASC) GO TO 680
C
C     DISPLACEMENT OFP FILE IS PRESENT, USER IS REQUESTING DISPLACEMENT
C     OUTPUT.
C
C     REMEMBER, WE STILL HAVE THE EXT-INT GRID TABLE IN Z(1) THRU Z(NEQ)
C     IN INTERNAL GIRD NUMBER (2ND WORD OF THE EXT-INT PAIR) SORT.
C     NOW, OPEN BGPDT, READ IN THE BASIC GRID POINT DATA (4 WORDS EACH
C     GRID) AND ADD THE EXTERNAL GRID POINT ID IN FRONT OF THE DATA SET.
C     THUS WE CREATE A NEW TABLE AFTER THE EXT-INT TABLE.
C
C     THE FOLLOWING 5 DATA WORDS FOR EACH GRID POINT:
C            EXTERNAL GRID ID
C            COORDINATE SYSTEM ID
C            X,Y,Z COORDINATES, IN BASIC COORD. SYSTEM
C
C     MOVE THIS NEW TABLE TO THE BEGINNING OF OPEN CORE SPACE
C     OVERWRITING THE OLD EXT-INT TABLE WHICH HAS NO LONGER NEEDED,
C     FROM Z(1) THRU Z(NBGT)
C     SORT THIS NEW TABLE BY THE EXTERNAL GRID NUMBERS.
C
      FILE = BGPDT
      CALL OPEN (*1300,BGPDT,Z(BUF2),RDREW)
      CALL FWDREC (*1300,BGPDT)
      K = -1
      J = JB
  610 CALL READ (*620,*620,BGPDT,Z(J+1),4,0,FLAG)
      K = K + 2
      Z(J) = Z(K)
      J = J + 5
      GO TO 610
  620 CALL CLOSE (BGPDT,REW)
      IF (K+1 .NE. NEQ) CALL MESAGE (-61,0,0)
      NBGT = J - JB
      NBG5 = NBGT/5
      DO 630 J = 1,NBGT
  630 Z(J) = Z(J+JBM1)
      CALL SORT (0,0,5,1,Z(1),NBGT)
      IF (DEBUG) WRITE (NOUT,640)
     1   (Z(J),Z(J+1),RZ(J+2),RZ(J+3),RZ(J+4),J=1,NBGT,5)
  640 FORMAT (/11X,'EXT.GRID - COOR - X,Y,Z/@640',/,(10X,2I8,3E11.4))
C
C     OPEN CSTM FILE IF IT EXISTS.  SAVE ALL COORDINATE TRANSFORMATION
C     MATRICES IN THE OPEN CORE SPACE IN Z(ICSTM) THRU Z(NCSTM), EITHER
C     AFTER THE EXT-COORD-X,Y,X TABLE, OR IN FRONT OF THE TABLE
C
      ICSTM = NBGT + 1
      NCSTM = NBGT
      FILE  = CSTM
      CALL OPEN (*1300,CSTM,Z(BUF2),RDREW)
      CALL FWDREC (*1300,CSTM)
      CALL READ (*650,*650,CSTM,Z(ICSTM),LEFT,1,FLAG)
      CALL MESAGE (-8,0,SUB)
  650 CALL CLOSE (CSTM,REW)
      NCSTM = NCSTM+FLAG
      CALL PRETRS (Z(ICSTM),FLAG)
      GO TO 680
C
  660 WRITE  (NOUT,670) UIM
  670 FORMAT (A29,' FROM DATABASE MODULE - DISPLACEMENT VECTORS REMAIN',
     1       ' IN GLOBAL COOR. SYSTEM', /5X,
     2       'DUE TO BGPDT OR CSTM FILE BEING PURGED',/)
      BASC = .FALSE.
C
C     NOW READ THE DISPLACMENT VECTORS (SUBCASES) FROM CURRENT OFP DATA
C     BLOCK, COMPUTE THE DISPLACEMENT FROM THE DISPLACMENT COORDINATE
C     BACK TO SYSTEM BASIC COORDINATE. SAVE THE VECTOR IN SCR1 FOR RE-
C     PROCESSING LATER.
C
C     2 (3 IF COMPLEX DATA) RECORDS PER ELEMENT TYPE,
C     SAME FORMAT AS GINO OUGV1 FILE
C
C     UNFORMATTED TAPE -
C
C     HEADER RECORD (UNFORMATTED)
C
C        RECORD  WORD       CONTENT (UNFORMATTED)
C        ------  ----   -----------------------------------------------
C           1      1     SUBCASE OR MODE NUMBER, INTEGER
C                  2     ZERO OR FREQUENCY, REAL
C                  3     NWDS, NUMBER OF WORDS PER ENTRY IN NEXT RECORD,
C                        INTEGER. (=8 FOR REAL DATA, OR =14 FOR COMPLEX
C                        FOR ALL DISPLACEMENT RECORDS)
C                 4-5    ORIGINAL GINO FILE NAME, BCD
C                 6-7    ' BASIC  '  OR 'GLOBAL  ', BCD
C                8-13    FORMAT CODE FOR NEXT RECORD, INTEGER
C                        8 DIGITS PER WORD,  1 FOR INTEGER
C                                            2 FOR REAL
C                        EX.  13222222       3 FOR BCD
C                                            0 NOT APPLICABLE
C               14-45    TITLE,    BCD
C               46-77    SUBTITLE, BCD
C              78-109    LABEL,    BCD
C
C     DISPLACEMENT RECORDS (UNFORMATTED)
C
C        RECORD  WORD       CONTENT (UNFORMATTED)
C        ------  ----   -----------------------------------------------
C           2      1     LENGTH, THIS FIRST WORD EXCLUDED, OF THIS
C                        RECORD (INTEGER)
C                  2     EXTERNAL GRID POINT NUMBER (INTEGER)
C                  3     POINT TYPE (1=GRID  PT.  2=SCALAR PT.
C                                    3=EXTRA PT.  4=MODAL  PT., INTEGER)
C                4-9     DISPLACEMENTS (REAL PARTS, REAL
C                        T1,T2,T3,R1,R2,R3)
C               10-15    (COMPLEX DATA ONLY)
C                        DISPLACEMENTS (IMGAGINARY PARTS, REAL
C                        T1,T2,T3,R1,R2,R3)
C                  :     REPEAT WORDS 2 THRU 9 (OR 15) AS MANY TIMES AS
C                        THERE ARE GRID POINT DISPLACEMENT DATA
C           :      :     REPEAT RECORD 2 AS MANY TIMES AS THERE ARE
C                        SUBCASES (OR MODES)
C
C
C     FORMATTED TAPE -
C
C     HEADER RECORD (FORMATTED)
C
C        RECORD   WORD      CONTENT (FORMATTED)                   FORMAT
C        ------   ----   -----------------------------------------------
C           1      1-2    ' CASE = ' OR ' MODE = '             8-LETTERS
C                    3    SUBCASE NUMBER                             I8
C                    4    ZERO OR FREQUENCY                     1P,E12.5
C                  5-6    ' WORDS ='                           8-LETTERS
C                    7    NWDS, NUMBER OF WORDS PER ENTRY IN NEXT    I8
C                         RECORD (=8 FOR REAL DATA, OR =14 COMPLEX,
C                         FOR ALL DISPLACEMENT RECORDS)
C                  8-9    ' INPUT ='                           8-LETTERS
C                10-11    ORIGINAL GINO FILE NAME                   2A4
C                12-13    ' COORD ='                           8-LETTERS
C                14-15    ' BASIC  ' OR 'GLOBAL  '                  2A4
C                16-17    '  CODE ='                           8-LETTERS
C                18-23    FORMAT CODE                               6I8
C                         8 DIGITS PER WORD,  1 FOR INTEGER
C                                             2 FOR REAL
C                         EX.  13222200       3 FOR BCD
C                                             0 NOT APPLICABLE
C                   23    NA4, NUMBER OF WORDS PER ENTRY IN NEXT    (I8)
C                         RECORD, IN A4-WORD COUNT (ONLY IF THE
C                         LAST FORMAT CODE WORD IS NOT USED)
C           2     1-32    TITLE,    32 BCD WORDS                   32A4
C           3    33-64    SUBTITLE, 32 BCD WORDS                   32A4
C           4    65-96    LABEL,    32 BCD WORDS                   32A4
C               (95-96    ELEMENT ID, STRESS AND FORCE ONLY         2A4)
C
C
C     DISPLACEMENT RECORDS (FORMATTED)
C
C        RECORD   WORD      CONTENT (FORMATTED)                   FORMAT
C        ------   ------------------------------------------------------
C           5       1     EXTERNAL GRID POINT NUMBER                 I8
C                   2     POINT TYPE (1=GRID  PT.  2=SCALAR PT.      I8
C                                     3=EXTRA PT.  4=MODAL  PT.)
C                 3-8     DISPLACEMENTS (REAL PARTS,         6(1P,E12.5)
C                         T1,T2,T3,R1,R2,R3)
C           6             (COMPLEX DATA ONLY)
C                 1-6     DISPLACEMENTS (IMAGINARY PARTS,    6(1P,E12.5)
C                         T1,T2,T3,R1,R2,R3)
C           :       :     REPEAT RECORD 5 (OR RECORDS 5 AND 6) AS MANY
C                         TIMES AS THERE ARE GRID POINT DISPLACMENT DATA
C         LAST      1     MINUS 0                                    I8
C                   2     MINUS 0                                    I8
C                 3-8     ZEROS                              6(1P,E12.5)
C        LAST+1           (COMPLEX DATA ONLY)
C                 1-6     ZEROS                              6(1P,E12.5)
C
C     IF CURRENT OFP DATA BLOCK IS AN ELEMENT STRESS OR ELEMENT FORCE
C     FILE, THE STRESS OR FORCE DATA HAVE VARIABLE LENGTH. (NWDS IS NO
C     LONGER 8 OR 14.)
C
C     THE ELEMENT STRESS OR FORCE RECORDS -
C
C        RECORD   WORD      CONTENT (UNFORMATTED)
C        ------   ------------------------------------------------------
C           2       1      NO. OF WORDS, EXCLUDING THIS FIRST WORD,
C                          IN THIS RECORD. (INTEGER)
C               2-NWDS+1   ELEMENT ID, STRESS OR FORCE DATA
C                          (VARIABLE DATA TYPES ARE DESCRIBED IN 'CODE')
C                   :      REPEAT (2-NWDS+1) WORDS AS MANY TIMES AS
C                          THERE ARE ELEMENTS
C           :       :      REPEAT RECORD 2 AS MANY TIMES AS THERE ARE
C                          SUBCASES.
C
C         WHERE NWDS IS THE NUMBER OF COMPUTER WORDS PER ENTRY, AND
C               CODE IS THE 6-WORD FORMAT CODE, AS DESCRIBED IN THE
C               HEADER RECORD.
C
C
C        RECORD   WORD      CONTENT (FORMATTED)                   FORMAT
C        ------   ------------------------------------------------------
C           5     1-NA4     ELEMENT ID, STRESS OR FORCE DATA       33A4
C                           (THE DATA TYPES ARE DESCRIBED IN
C                           'CODE'; ALL INTEGERS IN 2A4, REAL
C                           NUMBERS IN 3A4, AND BCD IN A4)
C           :       :       (MAXIMUM RECORD LENGTH IS 132 COLUMNS (33A4)
C                           CONTINUATION AND FOLDED INTO NEXT
C                           RECORD(S) IF NECESSARY.
C           :       :       A CARRIAGE CONTROL WORD ALWAYS PRECEEDS
C                           AN OUTPUT RECORD. THUS 1+132=133 COLUMNS
C                           LAST DATA VALUE ON A RECORD MAY SPILL
C                           TO THE NEXT RECORD)
C           :       :       REPEAT ABOVE RECORD(S) AS MANY TIMES
C                           AS THERE ARE ELEMENTS.
C
C         WHERE NA4 IS THE NUMBER OF WORDS PER ENTRY IN A4-WORD COUNT,
C               AND CODE IS 5-WORD FORMAT CODE
C
  680 FILE  = OFPX
      IOUGV = NCSTM + 1
      CALL READ (*980,*700,OFPX,Z(IOUGV),NZ-IOUGV,1,FLAG)
      CALL MESAGE (-37,FILE,SUB)
  700 IF (FLAG .NE. 146) GO TO 1320
      DSPL = MOD(Z(IOUGV+1),100)
      NWDS = Z(IOUGV+9)
      IF (.NOT.EFS .AND. NWDS.NE.8 .AND. NWDS.NE.14) GO TO 1320
      NSUB = NSUB + 1
      CAMO = CA
      CASE = Z(IOUGV+3)
      FREQ = 0.0
      IF (DSPL.NE.7 .AND. DSPL.NE.14) GO TO 710
      CAMO = MO
      CASE = Z (IOUGV+4)
      FREQ = RZ(IOUGV+5)
  710 BAGL = BA
      IF (.NOT.BASC) BAGL  = GL
      IF (FMTTD .AND. F8(6).EQ.0) F8(6) = NA4
      IF (.NOT.EFS) GO TO 715
      J = (Z(IOUGV+2)-1)*INCR
      Z(IOUGV+144) = E(J+1)
      Z(IOUGV+145) = E(J+2)
  715 IF (.NOT.FMTTD) WRITE (OUTTP)
     1   CASE,FREQ,NWDS,ONAME(JOS),ONAME(JOS+1),F8,(Z(J+IOUGV),J=50,145)
      IF ( FMTTD) WRITE (OUTTP,720) CAMO,CASE,
     1   FREQ,NWDS,ONAME(JOS),ONAME(JOS+1),BAGL,F8,(Z(J+IOUGV),J=50,145)
  720 FORMAT (1X,A8,I8,1P,E12.5,' WORDS =',I8,' INPUT =',2A4,
     1   ' COORD =',A8,'  CODE =',6I8, /1X,32A4, /1X,32A4, /1X,32A4)
      IF (FMTTD) GO TO 730
      FILE = SCR1
      CALL OPEN (*1300,SCR1,Z(BUF2),WRTREW)
      FILE = OFPX
  730 CALL READ (*970,*870,OFPX,A,NWDS,0,FLAG)
      A(1) = A(1)/10
      IF (EFS) GO TO 790
      IF (DEBUG) WRITE (NOUT,740) A(1)
  740 FORMAT (10X,'EXT.GRID/@740 =',I8)
      IF (BASC) GO TO 1200
  750 IF (COOR .LE. 0) GO TO 790
C
C     TRANSFORM THE DISPLACEMENT VECTOR FROM GLOBAL TO BASIC
C     UPON RETURN FROM 800, TRANSFORMATION MATRIX IN T
C
      DO 760 I = 3,NWDS
  760 RX(I)  = RA(I)
      CMPLX  = 0
  770 CALL GMMATS (T,3,3,0, RX(3),3,1,0, RA(3+CMPLX))
      CALL GMMATS (T,3,3,0, RX(6),3,1,0, RA(6+CMPLX))
      IF (NWDS.NE.14 .OR. CMPLX.EQ.6) GO TO 790
      CMPLX = 6
      DO 780 I = 3,8
  780 RX(I) = RX(I+CMPLX)
      GO TO 770
C
C     WRITE THE 8 (OR 14) DATA WORDS OUT TO SCR1 FILE IF OUTTP IS
C     UNFORMATTED, OR WRITE TO OUTTP DIRECTLY IF OUTTP IS FORMATTED
C
  790 IF (FMTTD) GO TO 800
      CALL WRITE (SCR1,A,NWDS,0)
      GO TO 730
  800 IF (EFS) GO TO 830
      WRITE  (OUTTP,810,ERR=1390) A(1),A(2),(RA(K),K=3,8)
  810 FORMAT (1X,2I8,6(1P,E12.5))
      IF (NWDS .EQ. 14) WRITE (OUTTP,820,ERR=1390) (RA(K),K=9,14)
  820 FORMAT (17X,6(1P,E12.5))
      GO TO 730
C
C     ELEMENT STRESS AND ELEMENT FORCE HAVE MIXED DATA, CHANGE THEM ALL
C     TO BCD WORDS, AND WRITE THEM OUT TO OUTTP UNDER A4 FORMAT
C     MAXIMUM OF 132 COLUMNS PER LINE.
C     NOTE - LAST DATA VALUE ON OUTPUT LINE MAY SPILL INTO NEXT RECORD.
C
  830 L = 0
      K = 0
  840 K = K + 1
      IF (F(K) .EQ. -9) GO TO 850
      IF (L+3 .GT. LIMRX) GO TO 1340
      CALL IFB2AR (F(K),A(K),IX,L)
      GO TO 840
  850 WRITE  (OUTTP,860,ERR=1390) (IX(K),K=1,L)
  860 FORMAT (1X,33A4)
      GO TO 730
C
C
C     JUST FINISH ONE VECTOR
C
C     UNFORMATTED TAPE -
C     TRANSFER THIS VECTOR FROM SCR1 TO OUTTP IN ONE LONG RECORD
C     (NO ZERO RECORD)
C     LOOP BACK FOR NEXT VECTOR
C
  870 IF (FMTTD) GO TO 890
      CALL WRITE (SCR1,0,0,1)
      CALL CLOSE (SCR1,REW)
      FILE = SCR1
      CALL OPEN (*1300,SCR1,Z(BUF2),RDREW)
      CALL READ (*880,*880,SCR1,Z(IOUGV+1),NZ-IOUGV,1,K)
      CALL MESAGE (-8,FILE,SUB)
  880 CALL CLOSE (SCR1,REW)
      Z(IOUGV) = K
      KIOUGV   = K + IOUGV
      WRITE (OUTTP) (Z(J),J=IOUGV,KIOUGV)
      GO TO 510
C
C     FORMATTED TAPE -
C     (DISPLACEMENTS ALREDY WRITTEN OUT IN SHORT RECORDS)
C     WRITE A ZERO RECORD
C     AND LOOP BACK FOR NEXT VECTOR
C
  890 IF (EFS) GO TO 920
      DO 900 I = 1,6
  900 RX(I) = 0.0
      WRITE  (OUTTP,910,ERR=1390) (RX(I),I=1,6)
  910 FORMAT (1X,2(6X,2H-0),6(1P,E12.5))
      IF (NWDS .EQ. 14) WRITE (OUTTP,820,ERR=1390) (RX(I),I=1,6)
      GO TO 510
C
C     WRITE A ZERO RECORD FOR EL.STRESS OR EL.FORCE TYPE OF DATA
C
  920 L = 0
      DO 960 I = 1,NWDS
      IX(L+2) = BLANK
      FI = F(I)
      GO TO (930,940,950), FI
  930 IX(L+1) = IZERO
      L = L + 2
      GO TO 960
  940 IX(L+1) = BZERO
      IX(L+3) = BLANK
      L = L + 3
      GO TO 960
  950 L = L + 1
  960 CONTINUE
      WRITE (OUTTP,860,ERR=1390) (IX(I),I=1,L)
      GO TO 510
C
C     END OF CURRENT OFP FILE
C     ADD AN ENDING RECORD TO OUTTP FILE AND ENDFILE
C
  970 CALL CLOSE (SCR1,REW)
  980 CALL CLOSE (OFPX,REW)
C
      DYY(OFPSET)  = DXX
      SUBN(OFPSET) = NSUB
      CASE = 0
      FREQ = 0.0
      Z(1) = 0
      J    = 0
      Z(J+2) = END1
      Z(J+3) = END2
      Z(J+4) = BLANK
      DO 985 J = 5,10
  985 Z(J) = 0
      DO 990 J = 11,106
  990 Z(J) = BLANK
      IF (.NOT.FMTTD) WRITE (OUTTP) CASE,FREQ,(Z(J),J=1,106)
      IF (     FMTTD) WRITE (OUTTP,720,ERR=1390) CAMO,CASE,FREQ,
     1                      (Z(J),J=1,106)
 1000 IF (OFP .LT. 3) GO TO 500
C
C     JOB DONE. WRITE A USER FRIENDLY MESSAGE OUT
C
      ENDFILE OUTTP
      REWIND  OUTTP
      SET = OFPSET
      IF (.NOT.NOBGPT) SET = SET + 1
      IF (.NOT.NOGEOM) SET = SET + 1
      J = BLANK
      IF (SET .GT. 1) J = LS
      K = 3 + 2*SET
      CALL PAGE2 (K)
      IF (OUTTP .GT. 12) GO TO 1010
      NAM(1) = INPX(3)
      NAM(2) = OUTTP - 10
      GO TO 1020
 1010 NAM(1) = INPX(1)
      NAM(2) = OUTTP - 14
      IF (OUTTP.NE.14 .AND. OUTTP.NE.25) GO TO 1020
      WRITE (NOUT,1030) UIM,SET,J,INPX(2)
      GO TO 1040
 1020 WRITE  (NOUT,1030) UIM,SET,J,NAM(1),NAM(2)
 1030 FORMAT (A29,' -', /5X,'DATABASE MODULE TRANSFERRED THE FOLLOWING',
     1       I3,' SET',A1,' OF DATA TO OUTPUT FILE ',A4,I1)
 1040 WRITE  (NOUT,1050) OUTTP,FMT
 1050 FORMAT (1H+,85X,'(FORTRAN UNIT',I3,1H),4A4)
      SET = 0
      IF (NOBGPT) GO TO 1070
      SET = SET + 1
      WRITE  (NOUT,1060) SET
 1060 FORMAT (/4X,I2,'. GRID POINT DATA - EXTERNAL NUMBERS AND BASIC ',
     1        'RECTANGULAR COORDINATES')
 1070 IF (NOGEOM) GO TO 1090
      SET = SET + 1
      WRITE  (NOUT,1080) SET
 1080 FORMAT (/4X,I2,'. ELEMENT CONNECTIVITY DATA - ALL GRID POINTS ',
     1        'ARE EXTERNAL NUMBERS')
 1090 IF (OFPSET .EQ. 0) GO TO 1190
      JSO = 1
      DO 1180 J = 1,OFPSET
      SET  = SET + 1
      NSUB = SUBN(J)
      WRITE  (NOUT,1100) SET,DYY(J),ONAME(JSO),ONAME(JSO+1)
 1100 FORMAT (/4X,I2,2H. ,A8,' DATA FROM INPUT FILE ',2A4)
      IF (EFS) GO TO 1120
      IF (     BASC) WRITE (NOUT,1110)
      IF (.NOT.BASC) WRITE (NOUT,1115)
 1110 FORMAT (1H+,46X,', CONVERTED TO BASIC RECT. COORDINATES,')
 1115 FORMAT (1H+,46X,', IN NASTRAN GLOBAL COORDINATE SYSTEM,')
      IF (DSPL.EQ.7 .OR. DSPL.EQ.14) GO TO 1140
 1120 IF (.NOT.EFS) WRITE (NOUT,1125) NSUB
      IF (     EFS) WRITE (NOUT,1130) NSUB
 1125 FORMAT (1H+,87X,I4,' SUBCASES')
 1130 FORMAT (1H+,46X,I4,' SUBCASES')
      GO TO 1160
 1140 WRITE  (NOUT,1150) NSUB
 1150 FORMAT (1H+,87X,I4,' FRQUENCIES')
 1160 IF (NOBGPT .AND. NOGEOM) WRITE (NOUT,1170)
 1170 FORMAT (/6X,'1. NONE')
 1180 JSO = JSO + 2
      RETURN
C
 1190 WRITE (NOUT,1170)
      RETURN
C
C     INTERNAL ROUTINE TO SEARCH FOR THE EXTERNAL GRID POINT AND RETURN
C     THE DISPLACEMENT COORDINATE ID ASSOCIATE WITH THAT POINT, AND SET
C     THE POINTER TO WHERE THE COORDINATE TRANSFORMATION MATRIX DATA
C     BEGINS.
C     EXTERNAL GRID VS. COORD SYSTEM ID TABLE IN Z(1) THRU Z(NEQ), IN
C     EXTERNAL GRID SORT
C     THE COORDINATE TRANSFORMATION MATRICES IN Z(ICSTM) THRU Z(NCSTM),
C     (14 WORDS PER MATRIX, FROM GLOBAL TO BASIC)
C
 1200 GRID = A(1)
      KLO  = 0
      KHI  = NBG5
      LASTK= 0
 1210 K = (KLO+KHI+1)/2
      IF (LASTK .EQ. K) CALL MESAGE (-61,0,0)
      LASTK = K
      K5  = K*5
      IF (GRID-Z(K5-4)) 1220,1240,1230
 1220 KHI = K
      GO TO 1210
 1230 KLO = K
      GO TO 1210
 1240 COOR = Z(K5-3)
      IF (COOR .LE. 0) GO TO 750
      CALL TRANSS (Z(K5-3),T)
      IF (.NOT.DEBUG) GO TO 750
      WRITE  (NOUT,1250) GRID,COOR,T
 1250 FORMAT (20X,'EXT GRID, COORD.ID AND TRANSF.MATRIX/@1250 =',2I8,
     1        /,(25X,3E13.5))
      GO TO 750
C
C     ILLEGITIMATE DATA IN OUGV FILE, ADVANCE TO NEXT RECORD
C
 1260 CALL FWDREC (*980,OUGV)
      CALL FWDREC (*980,OUGV)
      GO TO 510
C
C     ERRORS
C
 1300 J = -1
      GO TO 1400
 1310 J = -2
      GO TO 1400
 1320 WRITE  (NOUT,1325) UIM,ONAME(JSO),ONAME(JSO+1)
 1325 FORMAT (A29,', DATABASE MODULE SKIPS OUTPUTING ',2A4,
     1       ' FILE (OR PART OF THE FILE), DUE TO')
      WRITE  (NOUT,1330) NWDS
 1330 FORMAT (5X,'THE REQUEST OF AN ILLEGITIMATE DATA BLOCK.', 7X,
     1       'NO. OF WORDS =',I6)
      GO TO 1380
 1340 WRITE  (NOUT,1325) UIM,ONAME(JSO),ONAME(JSO+1)
      WRITE  (NOUT,1345) LIMRX
 1345 FORMAT (5X,'THE RX WORKING ARRAY OF',I5,' WORDS IN DBASE ',
     1       'SUBROUTINE IS NOT BIG ENOUGH TO RECEIVE OFP DATA.')
      GO TO 1360
 1350 WRITE  (NOUT,1325) UIM,ONAME(JSO),ONAME(JSO+1)
      WRITE  (NOUT,1355) LIMAF
 1355 FORMAT (5X,'THE A AND F WORKING ARRAYS OF',I4,' WORDS IN DBASE ',
     1       'SUBROUTINE ARE NOT BIG ENOUGH TO RECEIVE OFP DATA.')
 1360 WRITE  (NOUT,1365)
 1365 FORMAT (5X,'SUGGESTION - USE OUTPUT5 OR OUTPUT2 TO CAPTURE THE ',
     1       'REQUESTED DATA BLOCK')
      GO TO 1260
 1370 WRITE  (NOUT,1325) UIM,ONAME(JSO),ONAME(JSO+1)
      WRITE  (NOUT,1375)
 1375 FORMAT (5X,'ELEMENT STRESSES OR FORCES CAN NOT BE OUTPUT IN ',
     1       'BASIC COORDINATES AS REQUESTED')
 1380 CALL CLOSE (OFPX,REW)
      GO TO 1000
 1390 WRITE  (NOUT,1395)
 1395 FORMAT ('0*** SYSTEM FATAL ERROR WRITING FORMATTED TAPE IN DATA',
     1        'BASE MODULE')
      IF (MACH .EQ. 3) WRITE (NOUT,1396)
 1396 FORMAT (5X,'IBM USER - CHECK FILE ASSIGNMENT FOR DCB PARAMETER ',
     1       'OF 133 BYTES')
      J = -37
 1400 CALL MESAGE (J,FILE,SUB)
      RETURN
C     END
C
C
C     THE FOLLOWING PROGRAM WAS USED TO CHECKOUT THE UNFORMATTED TAPE
C     GENERATED BY DBASE. IT CAN BE SERVED AS A GUIDE TO OTHER USER WHO
C     WANTS TO ABSTRACT DATA FROM THAT TAPE.
C
C
C+    PROGRAM RDBASE
C
C     THIS FORTRAN PROGRAM READS THE UNFORMATTED OUTPUT FILE INP1
C     (FORTRAN UNIT 15) GENERATED BY DATABASE MODULE
C
C     (1) GRID POINTS DATA ARE READ AND SAVED IN GRID-ARRAY
C     (2) ELEMENTS DATA ARE READ AND SAVED IN ELM-ARRAY,
C         WITH ELEMENT NAMES AND POINTERS IN SAVE-ARRAY
C     (3) DISPLACEMENTS (VELOCITIES, ACCELERATIONS, LOADS, GRID-POINT
C         FORCE, OR EIGENVECTORS) DATA ARE READ AND SAVED IN DIS-ARRAY,
C         WITH SUBASES AND POINTERS IN SAVD-ARRAY
C
C     TO READ ELEMENT FORCES OR ELEMENT STRESSES, (3) ABOVE NEEDS SOME
C     CHANGES. PARTICULARLY WE NEED THE INFORMATION IN CODE TO GIVE US
C     THE TYPE OF EACH DATA WORD IN THE DATA LINE.
C     ASSUME CODE(1) = 11222222
C            CODE(2) = 31222000
C     THIS MEANS
C            THE 1ST, 2ND, AND 10TH DATA WORDS ARE INTEGERS;
C            9TH DATA WORD IS BCD; AND
C            3RD THRU 8TH, 11TH, 12TH AND 13TH WORDS ARE REAL NUMBERS
C
C
C     ANY OF ABOVE 3 SETS OF DATA NEED NOT EXIST IN ORIGINAL INP1 FILE
C
C     WRITTEN BY G.CHAN/UNISYS, JAN. 1989
C
C+    IMPLICIT INTEGER (A-Z)
C+    INTEGER          GRID(5,500),ELM(35,300),DIS(11200),SAVE(4,10),
C+   1                 SAVD(3,20),NAME(2),TITLE(32),SUBTTL(32),
C+   2                 LABL(32),CODE(6)
C+    REAL             GRIR(5,1),RIS(1),FREQ
C+    DOUBLE PRECISION GED,GD,EL,DS,ENDD,COORD
C+    EQUIVALENCE      (GRID(1),GRIR(1)),(DIS(1),RIS(1))
C+    DATA             INTAP, NOUT, MAXGRD, MAXELM, MAXDIS, MAXWDS    /
C+   1                 15,    6,    500,    300,    11200,  35        /
C+    DATA             GD,         EL,         DS,          END1      /
C+   1                 8HGRID PTS, 8HELEMENTS, 8HDISPLCNT,  4H -EN    /
C
C+    REWIND INTAP
C
C     READ DATA IDENTICATION RECORD
C
C+ 10 READ (INTAP,END=500) GED
C+    IF (NOUT .EQ. 6) WRITE (NOUT,20) GED
C+ 20 FORMAT (1X,A8,'--------')
C+    IF (GED .EQ. GD) GO TO 100
C+    IF (GED .EQ. EL) GO TO 200
C+    IF (GED .EQ. DS) GO TO 310
C+    STOP 'DATA TYPE UNKNOWN'
C
C     PROCESS GRID DATA
C     =================
C
C     READ GRID POINT DATA, ONE LONG RECORD OF MIXED INTEGERS AND REALS
C
C+100 READ (INTAP,END=500) L,(GRID(J,1),J=1,L)
C+    IF (NOUT .NE. 6) GO TO 10
C+    NGRID = L/5
C+    IF (NGRID .GT. MAXGRD) STOP 'GRID DIMENSION TOO SMALL'
C+    WRITE  (NOUT,110) NGRID
C+110 FORMAT (1X,I8,'=TOTAL NO. OF GRID POINTS')
C+    DO 130 I = 1,NGRID
C+    WRITE (NOUT,120) GRID(1,I),GRID(2,I),GRIR(3,I),GRIR(4,I),GRIR(5,I)
C+120 FORMAT (1X,2I8,3(1P,E12.5))
C+130 CONTINUE
C+    GO TO 10
C
C     PROCESS ELEMENT DATA
C     ====================
C
C+200 JS = 0
C+    JE = 0
C+
C+    READ ELEMENT HEADER RECORD, 8 WORDS
C+
C+210 READ (INTAP,END=500) NAME,TYPE,SYMBOL,GRIDS,TOTAL,WDS,LINE
C+    IF (NAME(1).EQ.END1 .AND. TYPE.EQ.0) GO TO 250
C+    IF (WDS .GT. MAXWDS) STOP 'ELM ROW DIMENSION TOO SMALL'
C+    IF (JE  .GT. MAXELM) STOP 'ELM COL DIMENSION TOO SMALL'
C+    JB = JE + 1
C+    JE = JE + TOTAL
C
C     READ ELEMENT DATA, ONE LONG RECORD PER ELEMENT TYPE (ALL INTEGERS)
C
C+    READ (INTAP) ((ELM(I,J),I=1,WDS),J=JB,JE)
C+    JS = JS + 1
C+    IF (JS .GE. 10) STOP 'SAVE DIMENSION TOO SMALL'
C
C     SAVE ELEMENT NAMES AND BEGINNING POINTERS IN SAVE-ARRAY
C     FOR EASY IDENTIFICATION
C
C+    SAVE(1,JS) = NAME(1)
C+    SAVE(2,JS) = NAME(2)
C+    SAVE(3,JS) = JB
C+    SAVE(4,JS) = WDS
C+    IF (NOUT .NE. 6) GO TO 210
C+    WRITE  (NOUT,220) NAME,TYPE,SYMBOL,GRIDS,TOTAL,WDS,LINE
C+220 FORMAT (1X,'ELEMNT =',2A4,'  TYPE =',I4,2X,A2,' GRIDS =',I8,
C+   1           ' TOTAL =',I8,' WDS/EL=',I8,      ' LINE  =',I8)
C+    DO 240 J = JB,JE
C+    WRITE  (NOUT,230) (ELM(I,J),I=1,WDS)
C+230 FORMAT (1X,3I8,13I8, /,(1X,8X,15I8))
C+240 CONTINUE
C+    GO TO 210
C
C     WRAP UP SAVE-ARRAY
C
C+250 JS = JS + 1
C+    SAVE(1,JS) = END1
C+    SAVE(2,JS) = NAME(2)
C+    SAVE(3,JS) = JE + 1
C+    SAVE(4,JS) = 0
C+    IF (NOUT .NE. 6) GO TO 10
C+    WRITE (NOUT,260)
C+    WRITE (NOUT,270) ((SAVE(I,J),I=1,4),J=1,JS)
C+260 FORMAT (/30X,'THIS REFERENCE TABLE IS NOT PART OF INPUT FILE')
C+270 FORMAT (40X,2A4,3H @ ,I4,',  WORDS=',I3)
C+    GO TO 10
C
C     PROCESS DISPLACEMENT DATA
C     =========================
C
C
C+300 STOP 'ERROR IN READING DISPLACEMENT DATA'
C+
C+310 KB = 1
C+    KS = 0
C
C     READ DISPLACEMENT HEADER RECORD
C
C+320 KS = KS + 1
C+    IF (KS .GT. 20) STOP 'SAVD DEMINSION TOO SMALL'
C+    READ (INTAP,END=390) CASE,FREQ,NWDS,NAME,COORD,CODE,TITLE,SUBTTL,
C+                         LABEL
C+    IF (CASE+NWDS .EQ. 0) GO TO 390
C+    IF (NOUT      .NE. 6) GO TO 340
C+    WRITE  (NOUT,330) CASE,FREQ,NWDS,NAME,COORD,CODE(1),CODE(2),TITLE,
C+                      SUBTTL,LABEL
C+330 FORMAT ('  CASES =',I8,1P,E12.5,' WORDS =',I8,' INPUT =',2A4,
C+   1        '  COORD =',A8,'  CODE = ',2I8, /,(1X,32A4))
C
C     DISPLACEMENT RECORS HAVE EITHER 8 OR 14 WORDS EACH DATA POINT
C     WITH CODE(1)=11222222, CODE(2) THRU (6) ARE ZEROS.
C
C
C     ------------------------------------------------------------------
C     IF ELEMENT STRESS OR ELEMENT FORCE FILE IS READ HERE, NWDS IS A
C     VARIABLE, NOT NECESSARY 8 OR 14. ALL INTEGERS ARE IN 2A4 FORMAT
C     (8-DIGITS), ALL REAL NUMBERS IN 3A4 (12-DIGITS), AND BCD WORD IN
C     A4 (4-LETTERS). THERE ARE NA4 A4-WORDS FOR EACH ELEMENT THAT HOLD
C     NWDS DATA VALUES.  MAXIMUM RECORD LENGTH IS 132 COLUMNS. ONE OR
C     MORE RECORDS ARE NEEDED PER ELEMENT. LAST DATA VALUE OF A RECORD
C     MAY SPILL INTO NEXT RECORD. NA4 IS THE 6TH WORD OF CODE. THE DATA
C     TYPE OF THIS RECORD IS DESCRIBED IN CODE. 1 FOR INTEGER, 2 FOR
C     REAL NUMBER, AND 3 FOR A BCD WORD. THERE ARE 5 CODE WORDS, EACH
C     HOLDS 8 DIGITS, AND ARE ARRANGED FROM LEFT TO RIGHT.
C
C     FOR EXAMPLE -
C     CODE(1)=12212222, CODE(2)=22213200, CODE(3)=CODE(4)=CODE(5)=0
C     INDICATE
C     DATA VALUES 1, 4 AND 12 ARE INTEGERS, DATA VALUE 13 IS ABCD WORD,
C     THE REST ARE REAL NUMBERS.
C     IN THIS EXAMPLE, NWDS SHOULD BE 14,
C                      NA4  SHOULD = 3X2 + 10X3 + 1X1 = 37.
C     2 RECORDS ARE NEEDED, 1ST RECORD 132 CHARACTERS LONG, 2ND RECORD
C     16 CHARACTERS. THESE TWO RECORDS CAN BE READ BY ONE FORTRAN LINE
C
C         READ (INTAP,10) (SS(J),J=1,NA4)
C     10  FORMAT (33A4)                        OR BY
C
C         READ (INTAP,20) IS(1),RS(2),RS(3),IS(4),(RS(J),J=5,11),IS(12)
C         READ (INTAP,30) IS(13),RS(14)
C     20  FORMAT (I8,2F12.0,I8,7F12.0,I8)
C     30  FORMAT (A4,F12.0)
C     ------------------------------------------------------------------
C
C+340 IF (NWDS.NE.8 .AND. NWDS.NE.14) STOP 'WORD COUNT ERROR'
C+    IF (CODE(1) .NE. 11222222) STOP 'FORMAT CODE ERROR'
C
C     SAVE SUBCASE NUMBER AND BEGINNING POINTERS IN SAVD-ARRAY
C     FOR EASY IDENTIFICATION
C
C+    KBM1 = KB - 1
C+    SAVD(1,KS) = CASE
C+    SAVD(2,KS) = KB
C+    SAVD(3,KS) = NWDS
C
C     READ DISPLACEMENT RECORD, ONE LONG RECORD PER SUBCASE (OR FREQ.)
C     EACH GRID POINT DISPLACEMENT DATA IN EVERY 8 OR 14 WORDS,
C     2 INTEGERS + 6 (OR 12) REALS
C
C+350 READ (INTAP,ERR=300) L,(DIS(I+KBM1),I=1,L)
C+    KE = L + KBM1
C+    DO 380 K = KB,KE,NWDS
C+    WRITE (NOUT,360) DIS(K),DIS(K+1),(RIS(K+I),I=2, 7)
C+    IF (NWDS .EQ. 14) WRITE (NOUT,370) (RIS(K+I),I=8,13)
C+360 FORMAT (1X,2I8,6(1P,E12.5))
C+370 FORMAT (1X,16X,6(1P,E12.5))
C+380 CONTINUE
C+    KB = KE + 1
C+    GO TO 320
C
C     WRAP UP SAVD-ARRAY
C
C+390 SAVD(1,KS) = 0
C+    SAVD(2,KS) = KE + 1
C+    SAVD(3,KS) = 0
C+    IF (NOUT .NE. 6) GO TO 10
C+    WRITE (NOUT,260)
C+    WRITE (NOUT,400) (SAVD(1,K),SAVD(2,K),SAVD(3,K),K=1,KS)
C+400 FORMAT (40X,'CASE',I8,3H @ ,I4,',  WORDS=',I4)
C+    GO TO 10
C
C+500 REWIND INTAP
      END
