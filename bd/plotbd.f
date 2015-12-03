      BLOCK DATA PLOTBD
CPLOTBD
      IMPLICIT INTEGER (A-Z)
      INTEGER         CHAR1(60,3),CHAR2(60,1),CHR19(2,79),CHRAM(2,88),
     1                CHRNZ(2,84),CHLPQM(2,52),CHRSYM(2,19),
     2                NPENS(20,2),PLTYPE(20,2),PBFSIZ(20,2),EOF(20,2)
      REAL            CHRSCL,CNTR,DATA,D02,D03,EDGE,G,MAXDEF,PAPSIZ,
     1                SCALE,S0S,VANGLE
      COMMON /CHAR94/ CHAR(60,4)
      COMMON /CHRDRW/ LSTCHR,CHRIND(60),CHR(2,350)
      COMMON /XXPARM/ BUFSIZ,
C    1    ... PLOTTING DATA
     1        CAMERA,BFRAMS,PLTMDL(2),TAPDEN,
C    2    ... PEN + PAPER DATA
     2        NOPENS,PAPSIZ(2),PAPTYP(2),PENSIZ(8),PENCLR(8,2),PENPAP,
C    3    ... SCALING DATA
     3        SCALE(2),FSCALE,MAXDEF,DEFMAX,
C    4    ... VIEWING DATA
     4        AXIS(3),DAXIS(3),VANGLE(5),VIEW(4),
C    5    ... VANTAGE POINT,               PROJECTION,OCULAR SEPARATION
     5        FVP,R0,S0L,S0R,T0,D0,D02,D03,PRJECT,    S0S,
C    6    ... ORIGIN DATA
     6        FORG,ORG,NORG,ORIGIN(11),EDGE(11,4),XY(11,3),
C    7    ... CONTOUR PLOTTING DATA
     7        NCNTR,CNTR(50),ICNTVL,WHERE,DIRECT,SUBCAS,FLAG,VALUE,
     7        LASSET,
C    8    ... DATA FOR USER PLOT TITLE CARD
     8        FPLTIT,PLTITL(17),COLOR,LAYER,
C    9    ... OFFSET SCALE (WILL BE SET TO 1 BY PLTSET)
     9        OFFSCL
      COMMON /PLTDAT/ MODEL,PLOTER,XYMIN(2),XYMAX(2),AXYMAX(2),
     1                XYEDGE(11),CHRSCL,PLTDAT(20),DATA(20,2)
      COMMON /SYMBLS/ NSYM,SYMBOL(20,2)
      COMMON /PLTSCR/ NCOR,PLTSC(50)
      COMMON /DRWAXS/ G(12)
C
C ... EQUIV FOR   /CHAR94/...
      EQUIVALENCE (CHAR(1,1),CHAR1(1,1))  , (CHAR(1,4),CHAR2(1,1))
C
C ... EQUIV FOR   /CHRDRW/...
      EQUIVALENCE (CHR(1,  1),CHR19(1,1)) , (CHR(1, 80),CHRAM(1,1)) ,
     1            (CHR(1,168),CHRNZ(1,1)) , (CHR(1,252),CHLPQM(1,1)),
     2            (CHR(1,304),CHRSYM(1,1))
C
C ... EQUIV FOR   /PLTDAT/...
      EQUIVALENCE (DATA( 7,1),NPENS(1,1)) , (DATA(10,1),PLTYPE(1,1)),
     1            (DATA(12,1),PBFSIZ(1,1)), (DATA(13,1),EOF(1,1))
C
      DATA CHAR1 /
     1     1H0,1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,1HA,1HB,1HC,1HD,1HE,
     2     1HF,1HG,1HH,1HI,1HJ,1HK,1HL,1HM,1HN,1HO,1HP,1HQ,1HR,1HS,1HT,
     3     1HU,1HV,1HW,1HX,1HY,1HZ,1H(,1H),1H+,1H-,1H*,1H/,1H=,1H.,1H,,
     4     1H$,1H-,1H ,12*0,
C
C ... THE FOLLOWING ARE NUMERIC EQUIVALENTS OF 7094 BINARY CHARACTERS.
C
C    5  ... 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F
     5     00,01,02,03,04,05,06,07,08,09,17,18,19,20,21,22,
C    6  ... G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V
     6     23,24,25,33,34,35,36,37,38,39,40,41,50,51,52,53,
C    7  ... W, X, Y, Z, (, ),  , -, *, /, =, ., ,, $, -,BLANK
     7     54,55,56,57,60,28,16,32,44,49,11,27,59,43,12,48,
C    8  . EOR,EOF, SPECIAL, FILLER
     8     58, 15, 63,42,26,   7*0,
C
C ... THE FOLLOWING ARE NUMBERIC EQUIVALENTS OF 7094 BCD CHARACTERS.
C
C    9  ... 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F
     9     10,01,02,03,04,05,06,07,08,09,49,50,51,52,53,54,
C    O  ... G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V
     O     55,56,57,33,34,35,36,37,38,39,40,41,18,19,20,21,
C   11  ... W, X, Y, Z, (, ),  , -, *, /, =, ., ,, $, -,BLANK
     1     22,23,24,25,28,60,48,32,44,17,11,59,27,43,12,16,
C   12  . EOR,EOF, SPECIAL, FILLER
     2     26, 15, 31,42,58,   7*0/
C
C ... THE FOLLOWING ARE NUMERIC VALUES ON CDC 6600 TO PRODUCE 7094 BCD
C     CHARACTERS.
C
      DATA CHAR2 /
C    1  ... 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F
     1     27,28,29,30,31,32,33,34,35,36,01,02,03,04,05,06,
C    2  ... G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V
     2     07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,
C    3  ... W, X, Y, Z, (, ),  , -, *, /, =, ., ,, $, -,BLANK
     3     23,24,25,26,41,42,37,38,39,40,44,47,46,43,52,45,
C    4  . EOR,EOF, SPECIAL, FILLER
     4      50, 49, 55,54,58,   7*0/
C
C ... DATA FOR DRAWING 6X6 CHARACTERS (8 UNITS WIDE - 16 UNITS HIGH).
C
C
C     THE FOLLOWING ARE INDICES USED TO DRAW CHARACTERS.
C
      DATA LSTCHR,CHRIND /  52,
C    1   0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
     1  -25,001,006,014,027,031,041,052,055,071,080,086,098,106,113,120,
C    2   G   H   I   J   K   L   M   N   O   P   Q   R   S   T   U   V
     2  126,136,142,148,155,160,163,168,172,181,188,199,208,220,225,231,
C    3   W   X   Y   Z   (   )   +   -   *   /   =   .   ,   $   -  DOT
     3  234,239,243,248,252,256,260,264,266,274,276,280,285,287,302,304,
C    4  CIRCLE SQUARE DIAMOND TRIANGLE END FILLER
     4   -25,   309,    314,    319,   323, 7*0 /
C
C ... DATA FOR DRAWING CHARACTERS 1 TO 9.
C
      DATA CHR19/
     1     2,5, 3,6, 3,0, 2,0, 4,0,
     2     0,5, 1,6, 4,6, 5,5, 5,4, 0,1, 0,0, 5,0,
     3     0,5, 1,6, 4,6, 5,5, 5,4, 4,3, 2,3, 4,3, 5,2, 5,1, 4,0, 1,0,
     3          0,1,
     4     4,0, 4,6, 0,2, 5,2,
     5     5,6, 0,6, 0,3, 1,4, 4,4, 5,3, 5,1, 4,0, 1,0, 0,1,
     6     4,6, 1,6, 0,5, 0,1, 1,0, 4,0, 5,1, 5,2, 4,3, 1,3, 0,2,
     7     0,6, 5,6, 2,0,
     8     4,3, 5,4, 5,5, 4,6, 1,6, 0,5, 0,4, 1,3, 4,3, 5,2, 5,1, 4,0,
     8          1,0, 0,1, 0,2, 1,3,
     9     5,0, 5,5, 4,6, 2,6, 1,5, 1,4, 2,3, 4,3, 5,4/
C
C ... DATA FOR DRAWING CHARACTERS A TO M.
C
      DATA CHRAM /
     A     0,0, 3,6, 5,2, 1,2, 5,2, 6,0,
     B     0,0, 0,6, 4,6, 5,5, 5,4, 4,3, 0,3, 4,3, 5,2, 5,1, 4,0, 0,0,
     C     5,5, 4,6, 1,6, 0,5, 0,1, 1,0, 4,0, 5,1,
     D     5,4, 4,6, 0,6, 0,0, 4,0, 5,2, 5,4,
     E     5,6, 0,6, 0,3, 3,3, 0,3, 0,0, 5,0,
     F     5,6, 0,6, 0,3, 3,3, 0,3, 0,0,
     G     5,5, 4,6, 1,6, 0,5, 0,1, 1,0, 4,0, 5,1, 5,3, 3,3,
     H     0,6, 0,0, 0,3, 5,3, 5,0, 5,6,
     I     2,6, 4,6, 3,6, 3,0, 2,0, 4,0,
     J     3,6, 5,6, 4,6, 4,1, 3,0, 1,0, 0,1,
     K     0,6, 0,0,-5,0, 0,3, 5,6,
     L     0,6, 0,0, 5,0,
     M     0,0, 0,6, 3,0, 6,6, 6,0/
C
C ... DATA FOR DRAWING CHARACTERS N TO Z.
C
      DATA CHRNZ /
     N     0,0, 0,6, 5,0, 5,6,
     O     6,5, 5,6, 1,6, 0,5, 0,1, 1,0, 5,0, 6,1, 6,5,
     P     0,0, 0,6, 4,6, 5,5, 5,4, 4,3, 0,3,
     Q     6,5, 5,6, 1,6, 0,5, 0,1, 1,0, 5,0, 6,1, 6,5,-4,2, 6,0,
     R     0,0, 0,6, 4,6, 5,5, 5,4, 4,3, 0,3, 3,3, 5,0,
     S     5,5, 4,6, 1,6, 0,5, 0,4, 1,3, 4,3, 5,2, 5,1, 4,0, 1,0, 0,1,
     T     0,6, 3,6, 3,0, 3,6, 6,6,
     U     0,6, 0,1, 1,0, 4,0, 5,1, 5,6,
     V     0,6, 3,0, 6,6,
     W     0,6, 1,0, 3,4, 5,0, 6,6,
     X     0,6, 6,0,-6,6, 0,0,
     Y     0,6, 3,3, 3,0, 3,3, 6,6,
     Z     0,6, 6,6, 0,0, 6,0/
C
C ... DATA FOR DRAWING CHARACTERS ( TO -.
C
      DATA CHLPQM /
     (     5,6, 3,4, 3,2, 5,0,
     )     1,6, 3,4, 3,2, 1,0,
     +     3,5, 3,1,-1,3, 5,3,
     -     1,3, 5,3,
     *     1,5, 5,1,-3,5, 3,1,-5,5, 1,1,-5,3, 1,3,
     /     0,0, 6,6,
     =     1,4, 4,4,-1,2, 4,2,
     .     2,0, 2,1, 3,1, 3,0, 2,0,
     ,     1,0, 3,2,
     $     6,5, 5,6, 1,6, 0,5, 0,4, 1,3, 5,3, 6,2, 6,1, 5,0, 3,0, 3,6,
     $          3,0, 1,0, 0,1,
     -     3,6, 3,4/
C
C ... DATA FOR DRAWING DOT, SQUARE, DIAMOND, TRIANGLE.
C
      DATA CHRSYM /
     D     3,4, 2,3, 3,2, 4,3, 3,4,
     S     0,0, 0,6, 6,6, 6,0, 0,0,
     D     3,6, 0,3, 3,0, 6,3, 3,6,
     T     0,0, 3,6, 6,0, 0,0/
C
      DATA BUFSIZ / 0 /,
C    1 ... CAMERA 2,  1 BLANK FRAME,  PLOTTER MODEL --M,1--
     1     CAMERA,BFRAMS,PLTMDL,TAPDEN / 2, 1, 1HM, 1, 0 /,
C    2 ... PAPER = DEFAULT,VELLUM...PEN SIZE = 1, COLOR = BLACK
     2     NOPENS,PAPSIZ,PAPTYP,PENSIZ,PENCLR /
     2     8, 2*0., 4HVELL, 2HUM, 8*1, 8*4HBLAC, 8*1HK /,
C    3 ... FIND THE SCALES, MAX DEFORMATION = 0
     3     SCALE(2),FSCALE,MAXDEF / 1.,1,0. /,
C    4 ... AXES = +X,+Y,+Z, VIEW ANGLES
     4     AXIS,DAXIS,VANGLE / 1,2,3,1,2,3, 0.,-1.E10,34.27,23.17,0./,
C    5 ... FIND VANTAGE POINT, ORTHOGRAPIC PROJECTION, PLANE+OCULAR SEP.
     5     FVP,PRJECT,D02,D03,S0S / 1,1,1.,2.,2.756  /,
C    6 ... LEFT=BOTTOM=0, RIGHT=TOP=1.
     6     NORG,ORG,FORG,EDGE / 10,0,1,22*0.,22*1.   /,
C    7 ... NCNTR=10=NO. CONTOURS, CNTR=LIST CONTOUR VALUES, ICNTVL=
C          MAJOR PRIN. STRESS, WHERE = Z1, DIRECT = COMMON
     7     NCNTR,CNTR,ICNTVL,WHERE,DIRECT,FLAG,LASSET/
     7     10 ,50*0.0,1,     1,    2,     0,   0     /,
C    8 ... DATA FOR USER PLOT TITLE CARD
     8     FPLTIT,PLTITL / 0, 17*4H      /,
C    9 ... OFFSET SCALE (AND ALSO PLOT TAPE MESSAGE CONTROL)
     9     OFFSCL / 0    /
C
C ... PLOTTER DATA.
C
      DATA  MODEL,PLOTER,CHRSCL / -1, 1, 1.0 /
C
C ... 1  NASTRAN GENERAL PURPOSE MICROFILM PLOTTER.
C
      DATA  DATA( 1,1) /1023.0   /,
     2      DATA( 2,1) /1023.0   /,
     3      DATA( 3,1) / 146.1429/,
     4      DATA( 4,1) /   8.0   /,
     5      DATA( 5,1) /  16.0   /,
     6      DATA( 6,1) /1023.0   /,
     8      DATA( 8,1) /   0.0   /,
     9      DATA( 9,1) /   0.0   /,
     1      DATA(11,1) /4HPLT2   /,
     4      DATA(14,1) /1484.761 /,
     5      DATA(15,1) /   0.0   /,
     6      DATA(16,1) /   0.0   /,
     7      DATA(17,1) /   0.0   /,
     8      DATA(18,1) /   0.0   /,
     9      DATA(19,1) /   0.0   /,
     *      DATA(20,1) /   0.0   /
C
C ... 2  NASTRAN GENERAL PURPOSE TABLE OR DRUM PLOTTER
C
      DATA  DATA( 1,2) /3000.0   /,
     2      DATA( 2,2) /3000.0   /,
     3      DATA( 3,2) / 100.0   /,
     4      DATA( 4,2) /   8.0   /,
     5      DATA( 5,2) /  16.0   /,
     6      DATA( 6,2) /3000.0   /,
     8      DATA( 8,2) /   0.0   /,
     9      DATA( 9,2) /   0.0   /,
     1      DATA(11,2) /4HPLT2   /,
     4      DATA(14,2) / 100.0   /,
     5      DATA(15,2) /   0.0   /,
     6      DATA(16,2) /   0.0   /,
     7      DATA(17,2) /   0.0   /,
     8      DATA(18,2) /   0.0   /,
     9      DATA(19,2) /   0.0   /,
     *      DATA(20,2) /   0.0   /
C
      DATA NPENS(1,1),PLTYPE(1,1),PBFSIZ(1,1),EOF(1,1)/ 64,-1,3000,1 /,
     1     NPENS(1,2),PLTYPE(1,2),PBFSIZ(1,2),EOF(1,2)/ 64,-2,3000,1 /
C
C ... SYMBOL DATA.
C
      DATA NSYM,SYMBOL /  9,
C           X, *, +, -, DOT, CIRCLE, SQUARE, DIAMOND, TRIANGLE
     1     34,41,39,40,  48,     49,     50,      51,       52, 11*0,
     2     34,41,39,40,  48,     49,     50,      51,       52, 11*0/
C
C ... PLOTTER SCRATCH AREA
C
C          NCOR = ARRAY LENGTH
      DATA NCOR,PLTSC  / 50,50*0 /
C
C ... DATA FOR DRAWING A X-Y-Z COORDINATE TRIAD IN /DRWAXS/
C     G   - X,Y,Z COORD. POINT DATA AND SYMBOLS
C
      DATA    G     / 9*0.0, 1HX, 1HY, 1HZ /
C
      END
