      SUBROUTINE PLTOPR
C
      INTEGER         PRNT,PLOTER,PLTYPE,EOF,ITYPE(4),LIST(20),SKIP,
     1                PLTTYP(3,3),A1(9),A2(12),A3(25),A4(13),A5(17),
     2                A6(6),B1(16),B2(16),B5(10),C1(16),C2(17),C3(24),
     3                FILM(2),PAPER(2),ILAY(20),D1(11),D2(6),D3(29),
     4                PRJ(3,3),PLUS,MINUS,XYZ(3),SYMM(4),BLANK,E1(12),
     5                E2(20),E3(14),E4(18),F1(22),F2(25),F3(12),F4(23),
     6                STRESS(34),DIST(12),WAY(4),G1(13),G2(11),G3(16),
     7                G4(6),G5(11),G6(8),G7(4),DISPLA(10),SPACE,LAYER,
     8                CAMERA,BFRAMS,PLTMDL,TAPDEN,PAPTYP,PENSIZ,PENCLR,
     9                AXIS,DAXIS,PRJECT,ORIGIN,STRAIN(4),P6(23),ORG,
     O                H1(11)
      REAL            ALST(20),CSCALE
      COMMON /BLANK / SKPCOM(20),PRNT
      COMMON /XXPARM/ PBUFSZ,CAMERA,BFRAMS,PLTMDL(2),TAPDEN,NPENS,
     1                PAPSIZ(2),PAPTYP(2),PENSIZ(8),PENCLR(8,2),PENPAP,
     2                SCALE(2),SKPSCL(3),AXIS(3),DAXIS(3),VANGLE(9),
     3                VANTX1,R0,S0L,S0R,T0,D0,VANTX2(2),PRJECT,S0S,
     4                FOR,ORG,NORG,ORIGIN(11),ORIGX3(11,4),
     5                XY(11,3),NCNTR,CNTR(50),ICNTVL,IWHERE,IDIREC,
     6                SKIP23(23),LAYER
      COMMON /PLTDAT/ MODEL,PLOTER,SKPPLT(17),CSCALE,SKPA(2),CNTSIN,
     1                SKPB(3),NOPENS,SKPC(2),PLTYPE,SKPD(2),EOF,CNTIN3
      EQUIVALENCE     (LIST(1),ALST(1))
C
      DATA    NSKIP , SKIP    /1        ,4H(1X)                     /
      DATA    FILM  , PAPER   /4HFILM   ,1H       ,4HPAPE   ,1HR    /
C
C     PLOTTER TYPE FORMATS.
C
      DATA    NP6   / 23 /
      DATA    P6    / 4H(10X  ,4H,38H   ,4HTHE    ,4HFOLL   ,4HOWIN  ,
     1                4HG PL  ,4HOTS    ,4HARE    ,4HFOR    ,4HA NA  ,
     2                4HSTPL  ,4HT ,2   ,4HA4,A   ,4H2,8H   ,4HPLOT  ,
     3                4HTER   ,4H,2A4   ,4H,17H   ,4HTYPI   ,4HNG C  ,
     4                4HAPAB  ,4HILIT   ,4HY,/)   /
      DATA    PLTTYP/ 4HMICR  ,4HOFIL   ,1HM      ,
     1                4H  TA  ,4HBLE    ,1H       ,
     2                4H  DR  ,4HUM     ,1H       /
      DATA    ITYPE / 4HWITH  ,4H       ,
     1                4HWITH  ,4HOUT    /
C
C     GENERAL PLOTTER FORMATS.
C
      DATA    NA1   / 9 /
      DATA    A1    / 4H(//,  ,4H25H    ,4HP L    ,4HO T    ,4HT E   ,
     1                4HR     ,4H D A   ,4H T A   ,4H,/)    /
      DATA    NA2   / 12 /
      DATA    A2    / 4H(10X  ,4H,27H   ,4HTHE    ,4HPLOT   ,4H TAP  ,
     1                4HE IS  ,4H WRI   ,4HTTEN   ,4H AT,   ,4HI4,4  ,
     2                4HH BP  ,4HI,/)   /
      DATA    NA3   / 25 /
      DATA    A3    / 4H(10X  ,4H,89H   ,4HTHE    ,4HPLOT   ,4HS AR  ,
     1                4HE SE  ,4HPARA   ,4HTED    ,4HBY E   ,4HND-O  ,
     2                4HF-FI  ,4HLE M   ,4HARKS   ,4H...T   ,4HWO E  ,
     3                4HND-O  ,4HF-FI   ,4HLE M   ,4HARKS   ,4H FOL  ,
     4                4HLOW   ,4HTHE    ,4HLAST   ,4H PLO   ,4HT,/)  /
      DATA    NA4   / 13 /
      DATA    A4    / 4H(10X  ,4H,41H   ,4HAN E   ,4HND-O   ,4HF-FI  ,
     1                4HLE M  ,4HARK    ,4HFOLL   ,4HOWS    ,4HTHE   ,
     2                4HLAST  ,4H PLO   ,4HT,/)   /
      DATA    NA5   / 17 /
      DATA    A5    / 4H(10X  ,4H,56H   ,4HTHE    ,4HFIRS   ,4HT CO  ,
     1                4HMMAN  ,4HD FO   ,4HR EA   ,4HCH P   ,4HLOT   ,
     2                4HCONT  ,4HAINS   ,4H THE   ,4H PLO   ,4HT NU  ,
     3                4HMBER  ,4H,/)    /
      DATA    NA6   / 6  /
      DATA    A6    / 4H(10X  ,4H,9HC   ,4HSCAL   ,4HE =    ,4H,F5.  ,
     1                4H2,/)  /
C
C     TABLE PLOTTER FORMATS.
C
      DATA    NB1   / 16 /
      DATA    B1    / 4H(10X  ,4H,30H   ,4HSET    ,4HTHE    ,4HX +   ,
     1                4HY SC  ,4HALE    ,4HFACT   ,4HORS    ,4HAT,F  ,
     2                4H6.1,  ,4H12H    ,4HCOUN   ,4HTS/I   ,4HNCH,  ,
     3                4H/)    /
      DATA    NB2   / 16 /
      DATA    B2    / 4H(10X  ,4H,12H   ,4HPAPE   ,4HR SI   ,4HZE =  ,
     1                4H,F5.  ,4H1,2H   ,4H X,F   ,4H5.1,   ,4H16H,  ,
     2                4H  PA  ,4HPER    ,4HTYPE   ,4H = ,   ,4H2A4,  ,
     3                4H/)    /
      DATA    NB5   / 10 /
      DATA    B5    / 4H(10X  ,4H,3HP   ,4HEN,I   ,4H2,7H   ,4H - S  ,
     1                4HIZE,  ,4HI2,2   ,4HH, ,   ,4H2A4,   ,4H/)    /
C
C     ELECTRONIC PLOTTER FORMATS.
C
      DATA    NC1   / 16 /
      DATA    C1    / 4H(10X  ,4H,37H   ,4HTHE    ,4HFOLL   ,4HOWIN   ,
     1                4HG PL  ,4HOTS    ,4HARE    ,4HREQU   ,4HESTE   ,
     2                4HD ON  ,4H ,A4   ,4H,A1,   ,4H5H O   ,4HNLY,   ,
     3                4H/)    /
      DATA    NC2   / 17 /
      DATA    C2    / 4H(10X  ,4H,54H   ,4HTHE    ,4HFOLL   ,4HOWIN   ,
     1                4HG PL  ,4HOTS    ,4HARE    ,4HREQU   ,4HESTE   ,
     2                4HD ON  ,4H BOT   ,4HH FI   ,4HLM +   ,4H PAP   ,
     3                4HER,/  ,4H)      /
      DATA    NC3   / 24 /
      DATA    C3    / 4H(10X  ,4H,I1,   ,4H79H    ,4HBLAN   ,4HK FR   ,
     1                4HAMES  ,4H WIL   ,4HL BE   ,4H INS   ,4HERTE   ,
     2                4HD ON  ,4H FIL   ,4HM ON   ,4HLY B   ,4HETWE   ,
     3                4HEN E  ,4HACH    ,4HOF T   ,4HHE F   ,4HOLLO   ,
     4                4HWING  ,4H PLO   ,4HTS,/   ,4H)      /
C
C     ENGINEERING DATA FORMATS.
C
      DATA    ND1   / 11 /
      DATA    D1    / 4H(//3  ,4H3H E   ,4H N G   ,4H I N   ,4H E E   ,
     1                4H R I  ,4H N G   ,4H       ,4HD A    ,4HT A,   ,
     2                4H/)    /
      DATA    ND2   / 6  /
      DATA    D2    / 4H(10X  ,4H,3A4   ,4H,11H   ,4H PRO   ,4HJECT   ,
     1                4HION)  /
      DATA    ND3   / 29 /
      DATA    D3    / 4H(10X  ,4H,29H   ,4HROTA   ,4HTION   ,4HS (D   ,
     1                4HEGRE  ,4HES)    ,4H- GA   ,4HMMA    ,4H=,F7   ,
     2                4H.2,8  ,4HH, B   ,4HETA    ,4H=,F7   ,4H.2,9   ,
     3                4HH, A  ,4HLPHA   ,4H =,F   ,4H7.2,   ,4H10H,   ,
     4                4H  AX  ,4HES =   ,4H ,2A   ,4H1,2(   ,4H1H,,   ,
     5                4H2A1)  ,4H,2H,   ,4H ,4A   ,4H4)     /
     5
      DATA    PRJ   , PLUS    ,MINUS    ,XYZ      ,SYMM     ,BLANK    /
     1                4HORTH  ,4HOGRA   ,4HPHIC   ,4HPERS   ,4HPECT   ,
     2                4HIVE   ,4HSTER   ,4HEOSC   ,4HOPIC   ,1H+,1H-  ,
     3                1HX     ,1HY      ,1HZ      ,4HANTI   ,4HSYMM   ,
     4                4HETRI  ,1HC      ,1H       /
C
C     ORTHOGRAPHIC + PERSPECTIVE ENGINEERING DATA FORMATS.
C
      DATA    NE1   / 12 /
      DATA    E1    / 4H(10X  ,4H,29H   ,4HSCAL   ,4HE (O   ,4HBJEC   ,
     1                4HT-TO  ,4H-PLO   ,4HT SI   ,4HZE)    ,4H=,1P   ,
     2                4H,E13  ,4H.6)    /
      DATA    NE2   / 20 /
      DATA    E2    / 4H(10X  ,4H,29H   ,4HVANT   ,4HAGE    ,4HPOIN   ,
     1                4HT (I  ,4HNCHE   ,4HS) -   ,4H RO    ,4H=,1P   ,
     2                4H,E13  ,4H.6,6   ,4HH, S   ,4H0 =,   ,4HE13.   ,
     3                4H6,6H  ,4H, T0   ,4H =,E   ,4H13.6   ,4H)      /
      DATA    NE3   / 14 /
      DATA    E3    / 4H(10X  ,4H,38H   ,4HPROJ   ,4HECTI   ,4HON P   ,
     1                4HLANE  ,4H SEP   ,4HARAT   ,4HION    ,4H(INC   ,
     2                4HHES)  ,4H =,1   ,4HP,E1   ,4H3.6)   /
      DATA    NE4   / 18 /
      DATA    E4    / 4H(10X  ,4H,6HO   ,4HRIGI   ,4HN,I8   ,4H,11H   ,
     1                4H   -  ,4H   X   ,4H0 =,   ,4H1P,E   ,4H14.6   ,
     2                4H,6H,  ,4H Y0    ,4H=,E1   ,4H4.6,   ,4H5X,8   ,
     3                4HH(IN  ,4HCHES   ,4H))      /
C
C     STEREO ENGINEERING DATA FORMATS.
C
      DATA    NF1   / 22 /
      DATA    F1    / 4H(10X  ,4H,30H   ,4HSCAL   ,4HES -   ,4H (MO   ,
     1                4HDEL-  ,4HTO-P   ,4HLOT    ,4HSIZE   ,4H =,1   ,
     2                4HP,E1  ,4H3.6,   ,4H25H,   ,4H  OB   ,4HJECT   ,
     3                4H-TO-  ,4HMODE   ,4HL SI   ,4HZE =   ,4H,E13   ,
     4                4H.6,1  ,4HH))    /
      DATA    NF2   / 25 /
      DATA    F2    / 4H(10X  ,4H,29H   ,4HVANT   ,4HAGE    ,4HPOIN   ,
     1                4HT (I  ,4HNCHE   ,4HS) -   ,4H R0    ,4H=,1P   ,
     2                4H,E13  ,4H.6,9   ,4HH, S   ,4H0(L)   ,4H =,E   ,
     3                4H13.6  ,4H,9H,   ,4H S0(   ,4HR) =   ,4H,E13   ,
     4                4H.6,6  ,4HH, T   ,4H0 =,   ,4HE13.   ,4H6)     /
      DATA    NF3   / 12 /
      DATA    F3    / 4H(10X  ,4H,28H   ,4HOCUL   ,4HAR S   ,4HEPAR   ,
     1                4HATIO  ,4HN (I   ,4HNCHE   ,4HS) =   ,4H,1P,   ,
     2                4HE13.  ,4H6)     /
      DATA    NF4   / 23 /
      DATA    F4    / 4H(10X  ,4H,6HO   ,4HRIGI   ,4HN,I8   ,4H,14H   ,
     1                4H   -  ,4H   X   ,4H0(L)   ,4H =,1   ,4HP,E1   ,
     2                4H4.6,  ,4H9H,    ,4HX0(R   ,4H) =,   ,4HE14.   ,
     3                4H6,6H  ,4H, Y0   ,4H =,E   ,4H14.6   ,4H,5X,   ,
     4                4H8H(I  ,4HNCHE   ,4HS))    /
C
C     CONTOUR PLOTTING DATA FORMATS
C
      DATA    NG1   / 13 /
     1        G1    / 4H(//4  ,4H2H C   ,4H O N   ,4H T O   ,4H U R   ,
     2                4H   P  ,4H L O   ,4H T T   ,4H I N   ,4H G     ,
     3                4H D A  ,4H T A   ,4H,/)    /
      DATA    NG2   / 11 /
     1        G2    / 4H(9X,  ,4H32HA   ,4HBOVE   ,4H PLO   ,4HT IS   ,
     2                4H A C  ,4HONTO   ,4HUR P   ,4HLOT    ,4HOF ,   ,
     3                4H4A4)  /
      DATA    NG3   / 16 /
     1        G3    / 4H(9X,  ,4H52HT   ,4HHE C   ,4HONTO   ,4HUR V   ,
     2                4HALUE  ,4HS AR   ,4HE CA   ,4HLCUL   ,4HATED   ,
     3                4H AT   ,4HFIBR   ,4HE DI   ,4HSTAN   ,4HCE ,   ,
     4                4H3A4)  /
      DATA    NG4   / 6  /
     1        G4    / 4H(9X,  ,4H4HIN   ,4H A,2   ,4HA4,6   ,4HHSYS   ,
     2                4HTEM)  /
      DATA    NG5   / 11 /
     1        G5    / 4H(//,  ,4H51X,   ,4H28HT   ,4HABLE   ,4H  OF   ,
     2                4H  PL  ,4HOTTI   ,4HNG     ,4HSYMB   ,4HOLS,   ,
     3                4H/)    /
      DATA    NG6   / 8  /
     1        G6    / 4H(5(5  ,4HX,13   ,4HHSYM   ,4HBOL    ,4H VAL   ,
     2                4HUE,6  ,4HX),/   ,4H)      /
      DATA    NG7   / 4  /
     1        G7    / 4H(5(I  ,4H9,1P   ,4H,E15   ,4H.6))   /
C
      DATA    NH1   / 11 /
      DATA    H1    / 4H(//5  ,4H0X,2   ,4H9HPL   ,4HOT M   ,4HODUL   ,
     1                4HE ME  ,4HSSAG   ,4HES C   ,4HONTI   ,4HNUE    ,
     2                4H,/)   /
C
      DATA   STRAIN / 4HSTRA  ,4HIN E   ,4HNERG   ,4HIES    /,
     1       DIST   / 4H Z2   ,2*1H     ,4H Z1    ,2*1H     ,4HMAX    ,
     2                4H- Z1  ,4H,Z2    ,4HAVER   ,4H-Z1,   ,4HZ2     /,
     3       WAY    / 4H LOC  ,4HAL     ,4H COM   ,4HMON    /
C
      DATA   SPACE  / 4H      /
     1       DISPLA / 4HDEFO  ,4HRMAT   ,4HION    ,1HX,1HY,1HZ, 3HMAG ,
     2                3*0     /
C
C                              1              3
      DATA   STRESS /       4HSTRE,4HSS,  ,4HSHEA,4HR -  ,
C               5 (1)          7 (2)          9 (3)         11 (4)
     1       4HMAJO,4HR-PR ,4HMINO,4HR-PR ,4HMAXI,4HMUM  ,4HNORM,4HAL X,
C              13 (5)         15 (6)         17     18      19
     2       4HNORM,4HAL Y ,4HNORM,4HAL Z ,4HXY  ,4HXZ   ,4HYZ   ,
C              20 (14)        22 (15)        24(16)         26 (17)
     3       4HNORM,4HAL 1 ,4HNORM,4HAL 2 ,4HSHEA,4HR 12 ,4HSHEA,4HR 1Z,
C              28 (18)        30 (19)        32             34
     4       4HSHEA,4HR 1Z ,4HBOND,4HSH12 ,4HLAYE,4HR NU ,4HMBER /
C
      DATA   ILAY   / 4H  1 ,4H  2 ,4H  3 ,4H  4 ,4H  5 ,4H  6 ,
     1                4H  7 ,4H  8 ,4H  9 ,4H 10 ,4H 11 ,4H 12 ,
     2                4H 13 ,4H 14 ,4H 15 ,4H 16 ,4H 17 ,4H 18 ,
     3                4H 19 ,4H 20 /
C
      IF (NCNTR .GT. 0) GO TO 201
C
C     PRINT THE PLOTTER ID.
C
      LIST(1) = 0
      CALL WRITE  (PRNT,LIST,1,0)
      CALL WRTPRT (PRNT,LIST,A1,NA1)
C
C     NASTRAN GENERAL PURPOSE PLOTTER.
C
      LIST(1) = 5
      J = IABS(PLTYPE)
      DO 126 I = 1,3
      LIST(I+1) = PLTTYP(I,J)
  126 CONTINUE
      MM = 1
      IF (PLTYPE .LT. 0) MM = 3
      LIST(5) = ITYPE(MM  )
      LIST(6) = ITYPE(MM+1)
      CALL WRTPRT (PRNT,LIST,P6,NP6)
C
C     GENERAL PLOTTER INFORMATION.
C
      IF (TAPDEN .LE. 0) GO TO 151
      LIST(1) = 1
      LIST(2) = TAPDEN
      CALL WRTPRT (PRNT,LIST,A2,NA2)
  151 IF (EOF .NE. 0) GO TO 152
      CALL WRTPRT (PRNT,0,A3,NA3)
      GO TO 154
  152 CALL WRTPRT (PRNT,0,A4,NA4)
  154 CALL WRTPRT (PRNT,0,A5,NA5)
      LIST(1) = 1
      ALST(2) = CSCALE
      CALL WRTPRT (PRNT,LIST,A6,NA6)
      IF (IABS(PLTYPE)-2) 170,160,163
C
C     TABLE PLOTTER INFORMATION.
C
  160 LIST(1) = 1
      ALST(2) = CNTSIN
      CALL WRTPRT (PRNT,LIST,B1,NB1)
  163 LIST(1) = 4
      ALST(2) = PAPSIZ(1)
      ALST(3) = PAPSIZ(2)
      LIST(4) = PAPTYP(1)
      LIST(5) = PAPTYP(2)
      CALL WRTPRT (PRNT,LIST,B2,NB2)
C
      LIST(1) = 4
      N = MIN0(NPENS,NOPENS)
      DO 168 I = 1,N
      LIST(2) = I
      LIST(3) = PENSIZ(I)
      IF (LIST(3) .LT. 0) GO TO 168
      LIST(4) = PENCLR(I,1)
      LIST(5) = PENCLR(I,2)
      IF (LIST(4).EQ.BLANK .AND. LIST(5).EQ.BLANK) GO TO 168
      CALL WRTPRT (PRNT,LIST,B5,NB5)
  168 CONTINUE
      CALL WRTPRT (PRNT,0,SKIP,NSKIP)
      GO TO 180
C
C     ELECTRONIC PLOTTER INFORMATION.
C
  170 IF (CAMERA-2) 171,172,174
  171 LIST(2) = FILM(1)
      LIST(3) = FILM(2)
      GO TO 173
  172 LIST(2) = PAPER(1)
      LIST(3) = PAPER(2)
  173 LIST(1) = 2
      CALL WRTPRT (PRNT,LIST,C1,NC1)
      GO TO 175
  174 CALL WRTPRT (PRNT,0,C2,NC2)
  175 IF (CAMERA.EQ.2 .OR. BFRAMS.EQ.0) GO TO 180
      LIST(1) = 1
      LIST(2) = BFRAMS
      CALL WRTPRT (PRNT,LIST,C3,NC3)
C
C     ENGINEERING DATA.
C
  180 CALL WRTPRT (PRNT,0,D1,ND1)
      LIST(1) = 3
      DO 181 I = 1,3
      LIST(I+1) = PRJ(I,PRJECT)
  181 CONTINUE
      CALL WRTPRT (PRNT,LIST,D2,ND2)
C
      LIST(1) = 13
      ALST(2) = VANGLE(3)
      IF (VANGLE(2) .GT. -1.E10) GO TO 1815
      IF (PRJECT .NE. 2) VANGLE(2) = VANGLE(4)
      IF (PRJECT .EQ. 2) VANGLE(2) = VANGLE(5)
 1815 ALST(3) = VANGLE(2)
      ALST(4) = VANGLE(1)
      DO 182 I = 1,3
      J = 2*I + 3
      K = IABS(AXIS(I))
      LIST(J) = PLUS
      IF (AXIS(I) .LT. 0) LIST(J) = MINUS
      LIST(J+1) = XYZ(K)
  182 CONTINUE
      N = 1
      IF (AXIS(1) .EQ. DAXIS(1)) N = 2
      LIST(14) = BLANK
      J = 1
      DO 183 I = N,4
      LIST(J+10) = SYMM(I)
      J = J + 1
  183 CONTINUE
      CALL WRTPRT (PRNT,LIST,D3,ND3)
      IF (PRJECT .EQ. 3) GO TO 195
C
C     ORTHOGRAPHIC + PERSPECTIVE ENGINEERING DATA.
C
      LIST(1) = 1
      ALST(2) = SCALE(1)/CNTSIN
      CALL WRTPRT (PRNT,LIST,E1,NE1)
      IF (PRJECT .EQ. 1) GO TO 191
      LIST(1) = 3
      ALST(2) = R0
      ALST(3) = S0L
      ALST(4) = T0
      CALL WRTPRT (PRNT,LIST,E2,NE2)
      LIST(1) = 1
      ALST(2) = D0
      CALL WRTPRT (PRNT,LIST,E3,NE3)
C
  191 CALL WRTPRT (PRNT,0,SKIP,NSKIP)
      LIST(1) = 3
      DO 192 I = 1,ORG
      LIST(2) = ORIGIN(I)
      ALST(3) = XY(I,1)/CNTSIN
      ALST(4) = XY(I,3)/CNTSIN
      CALL WRTPRT (PRNT,LIST,E4,NE4)
  192 CONTINUE
      GO TO 260
C
C     STEREO ENGINEERING DATA.
C
  195 LIST(1) = 2
      ALST(2) = SCALE(1)/CNTIN3
      ALST(3) = SCALE(2)
      CALL WRTPRT (PRNT,LIST,F1,NF1)
      LIST(1) = 4
      ALST(2) = R0
      ALST(3) = S0L
      ALST(4) = S0R
      ALST(5) = T0
      CALL WRTPRT (PRNT,LIST,F2,NF2)
      LIST(1) = 1
      ALST(2) = D0
      CALL WRTPRT (PRNT,LIST,E3,NE3)
      ALST(2) = S0S
      CALL WRTPRT (PRNT,LIST,F3,NF3)
C
      CALL WRTPRT (PRNT,0,SKIP,NSKIP)
      LIST(1) = 4
      DO 196 I = 1,ORG
      LIST(2) = ORIGIN(I)
      ALST(3) = XY(I,1)/CNTSIN
      ALST(4) = XY(I,2)/CNTSIN
      ALST(5) = XY(I,3)/CNTSIN
      CALL WRTPRT (PRNT,LIST,F4,NF4)
  196 CONTINUE
      GO TO 260
C
C     CONTOUR PLOTTING DATA
C
  201 LIST(1) = 0
      CALL WRTPRT (PRNT,LIST,G1,NG1)
      LIST(1) = 4
      IF (ICNTVL.GT.9 .AND. ICNTVL.LT.14) GO TO 210
C
C     STRESS CONTOURS
C
      I = 1
      IF (ICNTVL.GT. 6 .OR. ICNTVL.EQ. 3) I = 3
      IF (ICNTVL.GE.14 .AND.ICNTVL.LE.19) I = 1
      IF (ICNTVL .NE. 20) GO TO 203
C
C     STRAIN CONTOURS
C
      LIST(1) = 4
      LIST(2) = STRAIN(1)
      LIST(3) = STRAIN(2)
      LIST(4) = STRAIN(3)
      LIST(5) = STRAIN(4)
      CALL WRTPRT (PRNT,LIST,G2,NG2)
      GO TO 205
C
  203 LIST(2) = STRESS(I)
      LIST(3) = STRESS(I+1)
      I = ICNTVL*2 + 3
      IF (ICNTVL.GT.13 .AND. ICNTVL.LT.20) I = (ICNTVL-14)*2 + 20
      IF (ICNTVL.GT. 6 .AND. ICNTVL.LE. 9) I = ICNTVL + 10
      LIST(4) = STRESS(I)
      LIST(5) = SPACE
      IF (ICNTVL.LT.7 .OR. ICNTVL .GE.14) LIST(5) = STRESS(I+1)
      CALL WRTPRT (PRNT,LIST,G2,NG2)
C
C     ADDING LAYER NUMBER TO OUTPUT WHEN REQUESTED
C
      IF (ICNTVL.LT.14 .OR. ICNTVL.EQ.20) GO TO 204
      LIST(1) = 4
      LIST(2) = STRESS(32)
      LIST(3) = STRESS(33)
      LIST(4) = STRESS(34)
      LIST(5) = ILAY(LAYER)
      CALL WRTPRT (PRNT,LIST,G2,NG2)
      GO TO 205
C
  204 LIST(1) = 3
      I = IWHERE
      IF (IWHERE .LE. 0) I = 0
      I = I*3 + 1
      LIST(2) = DIST(I)
      LIST(3) = DIST(I+1)
      LIST(4) = DIST(I+2)
      CALL WRTPRT (PRNT,LIST,G3,NG3)
C
  205 J = 2*(IDIREC-1) + 1
      GO TO 220
C
C     DISPLACEMENT CONTOURS
C
  210 I = 1
      LIST(2) = DISPLA(I  )
      LIST(3) = DISPLA(I+1)
      LIST(4) = DISPLA(I+2)
      LIST(5) = DISPLA(ICNTVL-6)
      CALL WRTPRT (PRNT,LIST,G2,NG2)
      J = 3
  220 IF (ICNTVL.LT.4 .OR. ICNTVL.EQ.13) GO TO 235
      LIST(1) = 2
      LIST(2) = WAY(J  )
      LIST(3) = WAY(J+1)
      CALL WRTPRT (PRNT,LIST,G4,NG4)
  235 LIST(1) = 0
      CALL WRTPRT (PRNT,LIST,G5,NG5)
      CALL WRTPRT (PRNT,LIST,G6,NG6)
      L = (NCNTR-1)/10 + 1
      LIST(1) = 2*L
      K = MIN0(NCNTR,10)
      DO 250 J = 1,K
      N = J + (L-1)*10
      M = 2
      DO 240 I = J,N,10
      IF (I .GT. NCNTR) GO TO 245
      LIST(M  ) = I
      ALST(M+1) = CNTR(I)
  240 M = M + 2
      GO TO 247
  245 LIST(1) = LIST(1) - 2
      L = L - 1
  247 CALL WRTPRT (PRNT,LIST,G7,NG7)
  250 CONTINUE
C
  260 LIST(1) = 0
      CALL WRTPRT (PRNT,LIST,H1,NH1)
      RETURN
      END
