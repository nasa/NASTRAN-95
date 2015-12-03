      SUBROUTINE HDSURF (GPLST,X,U,PEN,DEFORM,NMAX,MAXSF,IZ,IB,PEDGE,
     1                   IOPCOR)
C
C     THIS ROUTINE PREPARES THE ELEMENT SURFACES FOR HIDDEN LINE PLOT
C     IT ALSO GENERATES THE SHRINK PLOT IF SHRINK ALONE IS REQUESTED.
C     IF SHRINK AND HIDDEN ARE REQUESTED, THIS ROUTINE WILL PREPARE THE
C     SHRUNK SURFACES FOR HDPLOT.
C
C     REVISED  10/1990 BY G.CHAN/UNISYS
C     (1) HIDDEN PLOT WITH SOLID ELEMENTS BUGS
C     (2) HIDDEN AND SHRINK TOGETHER
C     (3) SKIP ANY OFFSET DATA IN ELSET FILE IF THEY ARE PRESENT
C
      LOGICAL         SHRINK,HIDDEN
      INTEGER         GPLST(1),PEN,DEFORM,PEDGE,ETYP,G,NAME(2),GP,ELID,
     1                ELSET,IZ(14,1),M1(16),LDX(9),FILE,SOLID,TEMP(27),
     2                OFFSET
      REAL            X(3,1),U(2,1)
      COMMON /BLANK / NGP,SKP11(11),ELSET,SKP22(7),MERR,IDUM(3),NSCR1,
     1                NSCR2,NSCR3
      COMMON /SYSTEM/ IBUF,IOUT
      COMMON /PLTSCR/ NNN,G(3)
      COMMON /HDREC / NOFSUR,NS,ELID,LID,NPERS,P(3,13)
C
C     DIMENSIONS      TEMP, IZ, AND P ARE TEMP(2*N+1), IZ(N+1,1), AND
C                     P(3,N) WHERE N=LETSZ2=MAX OF LETSZ(2,I), I=1,9
C
      DIMENSION       LET1(5),LET2(4,4),LET3(5,5),LET4(5,6),LET5(9,6),
     1                LET6(13,6),LET7(5),LET8(7),LET9(9),LET(229),
     2                LETSZ(3,9)
      EQUIVALENCE     (LET(  1),LET1(  1)), (LET(  6),LET2(1,1)),
     1                (LET( 22),LET3(1,1)), (LET( 47),LET4(1,1)),
     2                (LET( 77),LET5(1,1)), (LET(131),LET6(1,1)),
     3                (LET(209),LET7(  1)), (LET(214),LET8(  1)),
     4                (LET(221),LET9(  1))
C
      DATA    NAME  / 4HHDSU, 4HRF   / ,
     1        NM1,M1/ 16,4H(33X, 4H,13H, 4HELEM, 4HENT , 4HTYPE, 4H A5,,
     2        4H4HWI, 4HTHI8, 4H,24H, 4H GRI, 4HDS S, 4HKIPP, 4HED I,
     3        4HN LI, 4HNEL., 4H)    /
C
C     SPECIAL ELEMENT CONNECTION PATTERNS
C
      DATA    LDX   / 2HD1,2HD2,2HD3,2HD4,2HD5,2HD6,2HD7,2HD8,2HD9  /
      DATA    KTET  / 2HTE /,   KWEG  / 2HWG /,    KHX1  / 2HH1 /,
     1        KHX2  / 2HH2 /,   KIX1  / 2HXL /,    KIX2  / 2HXQ /,
     2        KIX3  / 2HXC /,   KAE   / 2HAE /,    KTRIM6/ 2HT6 /,
     3        KTRPLT/ 2HP6 /,   KTRSHL/ 2HSL /,    KIS2D8/ 2HD8 /,
     4        KFHEX1/ 2HFA /,   KFHEX2/ 2HFB /,    KFTETA/ 2HFT /,
     5        KFWEDG/ 2HFW /,   KBAR  / 2HBR /,    KT3   / 2HT3 /,
     6        KQ4   / 2HQ4 /
C    7        KELBOW/ 2HEB /
C
C     1   -   LINE,TRIANGLE,QUAD    5   -   IHEXA2
C     2   -   TETRA                 6   -   IHEXA3
C     3   -   WEDGE                 7   -   AERO
C     4   -   HEXA                  8   -   TRIM6 AND TRPLT1 AND TRSHL
C
      DATA    LETSZ2/ 13 /
      DATA    LETSZ /
     1         1,      5,     1,
     2         4,      4,     6,
     3         5,      5,    22,
     4         6,      5,    47,
     5         6,      9,    77,
     6         6,     13,   131,
     7         1,      5,   209,
     8         1,      7,   214,
     9         1,      9,   221/
C         NELSRF,   NPTS,    IS
      DATA    LET1  /
     1         1,  2,  3,  4,  5/
      DATA    LET2  /
     1         1,  2,  3,  1,
     2         1,  2,  4,  1,
     3         2,  3,  4,  2,
     4         1,  3,  4,  1/
      DATA    LET3  /
     1         1,  2,  3,  1,  0,
     2         4,  5,  6,  4,  0,
     3         1,  3,  6,  4,  1,
     4         1,  2,  5,  4,  1,
     5         2,  3,  6,  5,  2/
      DATA    LET4  /
     1         1,  2,  3,  4,  1,
     2         5,  6,  7,  8,  5,
     3         3,  4,  8,  7,  3,
     4         1,  2,  6,  5,  1,
     5         2,  3,  7,  6,  2,
     6         1,  4,  8,  5,  1/
      DATA    LET5  /
     1         1,  2,  3,  4,  5,  6,  7,  8,  1,
     2        13, 14, 15, 16, 17, 18, 19, 20, 13,
     3         3, 10, 15, 16, 17, 11,  5,  4,  3,
     4         5, 11, 17, 18, 19, 12,  7,  6,  5,
     5         7, 12, 19, 20, 13,  9,  1,  8,  7,
     6         1,  2,  3, 10, 15, 14, 13,  9,  1/
      DATA    LET6  /
     1         1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12,  1,
     2        21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 21,
     3         4,  5,  6,  7, 15, 19, 27, 26, 25, 24, 18, 14,  4,
     4         7,  8,  9, 10, 16, 20, 30, 29, 28, 27, 19, 15,  7,
     5        10, 11, 12,  1, 13, 17, 21, 32, 31, 30, 20, 16, 10,
     6         1,  2,  3,  9, 14, 18, 24, 23, 22, 21, 17, 13,  1/
      DATA    LET7  /
     1         1,  2,  3,  4,  1/
      DATA    LET8  /
     1         1,  2,  3,  4,  5,  6,  1/
      DATA    LET9  /
     1         1,  5,  2,  6,  3,  7,  4,  8,  1/
C
C     PEDGE FLAG = 2 OR 200    - HIDDEN LINE PLOT
C                = 10 THRU 100 - SHRINK PLOT.
C                = 100         - FILL, NOT USED HERE
C                = .GT.    200 - SHRINK AND HIDDEN LINE PLOT
C     E.G. PEDGE = 270 INDICATES HIDDEN LINE PLOT WITH EACH ELEMENT
C                  SHRUNK TO 70 PERCENT OF FULL SIZE.
C
      IPEDGE = MOD(PEDGE,200)
      NWDS   = 0
      NMAX   = 0
      LS     = 0
      LSMAX  = IOPCOR/14
      IF (PEDGE .GT. 200) LSMAX = 0
      NOFSUR = 0
      SHK    = 1.0
      SHRINK = .FALSE.
      IF (PEDGE .LT. 10) GO TO 10
      SHRINK = .TRUE.
      SHK = 1. - FLOAT(IPEDGE)/100.
      CALL LINE (0.,0.,0.,0.,1.,-1)
   10 HIDDEN = .FALSE.
      IF (PEDGE.NE.2 .AND. PEDGE.LT.200) GO TO 20
      HIDDEN = .TRUE.
      CALL GOPEN (NSCR2,GPLST(IB),1)
      NWDS = 3*LETSZ2 + 5
C
   20 CALL READ (*310,*190,ELSET,ETYP,1,0,I)
      CALL FREAD (ELSET,I,1,0)
      NGPEL  = IABS(I)
      NGPELX = NGPEL
      SOLID  = 0
C
      OFFSET = 0
      IF (ETYP .EQ. KBAR) OFFSET = 6
      IF (ETYP.EQ.KT3 .OR. ETYP.EQ.KQ4) OFFSET = 1
      ITYPE = 1
      IF (ETYP .EQ. KTET    ) ITYPE = 2
      IF (ETYP .EQ. KFTETA  ) ITYPE = 2
      IF (ETYP .EQ. KWEG    ) ITYPE = 3
      IF (ETYP .EQ. KFWEDG  ) ITYPE = 3
      IF (ETYP .EQ. KHX1   .OR. ETYP .EQ. KHX2 .OR. ETYP .EQ. KIX1 .OR.
     1    ETYP .EQ. KFHEX1 .OR. ETYP .EQ. KFHEX2) ITYPE = 4
      IF (ETYP .EQ. KIX2    ) ITYPE = 5
      IF (ETYP .EQ. KIS2D8  ) ITYPE = 9
      IF (ETYP .EQ. KIX3    ) ITYPE = 6
      IF (ETYP .EQ. KAE     ) ITYPE = 7
      IF (ETYP .EQ. KTRIM6 .OR. ETYP .EQ. KTRPLT .OR. ETYP .EQ. KTRSHL)
     1    ITYPE = 8
C
      IF (ITYPE .NE. 1) GO TO 40
C
C     SIMPLE ELEMENT
C
      IF (NGPEL.GT.2 .AND. I.GT.0) NGPELX = NGPEL + 1
      IF (NGPEL .GT. 4) GO TO 130
      NPTS = NGPELX
      GO TO 50
C
C     COMPLEX ELEMENT
C
   40 CONTINUE
      IF (ITYPE.GE.2 .AND. ITYPE.LE.6) SOLID = 1
      NPTS = LETSZ(2,ITYPE)
   50 IF (NPTS-1 .GT. NMAX) NMAX = NPTS - 1
C
C     READ THE ELEMENT DATA
C
   60 CALL FREAD (ELSET,ELID,1,0)
      IF (ELID .LE. 0) GO TO 20
      CALL FREAD (ELSET,LID,1,0)
      CALL FREAD (ELSET,G,NGPEL,0)
      IF (OFFSET .NE. 0) CALL FREAD (ELSET,0,-OFFSET,0)
      IF (NGPEL .NE. NGPELX) G(NGPELX) = G(1)
      IF (HIDDEN .AND. .NOT.SHRINK) GO TO 80
      XC = 0.
      YC = 0.
      ZC = 0.
      DO 70 I = 1,NGPEL
      GP = G(I)
      GP = IABS(GPLST(GP))
      XC = XC + X(2,GP)
      YC = YC + X(3,GP)
      ZC = ZC + X(1,GP)
   70 CONTINUE
      XC = XC/NGPEL
      YC = YC/NGPEL
      ZC = ZC/NGPEL
C
   80 NELSRF = LETSZ(1,ITYPE)
      IS = LETSZ(3,ITYPE)
C
      DO 120 NS = 1,NELSRF
      NN = 0
      MM = (NS-1)*NPTS + IS - 1
      NPERS = NPTS
      DO 110 I = 1,NPTS
      M = MM + I
      N = LET(M)
      IF (N .NE. 0) GO TO 85
   82 NPERS = NPERS - 1
      GO TO 110
   85 GP = G(N)
      IF (GP .EQ. 0) GO TO 82
      NN = NN + 1
      GP = IABS(GPLST(GP))
      P(3,NN) = X(1,GP)
      IF (DEFORM .NE. 0) GO TO 90
      P(1,NN) = X(2,GP)
      P(2,NN) = X(3,GP)
      GO TO 100
   90 P(1,NN) = U(1,GP)
      P(2,NN) = U(2,GP)
  100 CONTINUE
      IF (.NOT.SHRINK) GO TO 110
      IF (     HIDDEN) GO TO 105
      IF (NN .EQ. 1) GO TO 110
      X1 = P(1,NN-1) - (P(1,NN-1)-XC)*SHK
      Y1 = P(2,NN-1) - (P(2,NN-1)-YC)*SHK
      X2 = P(1,NN  ) - (P(1,NN  )-XC)*SHK
      Y2 = P(2,NN  ) - (P(2,NN  )-YC)*SHK
      IPEN = PEN
      IF (IPEDGE.EQ.100 .AND. PEN.GT.31 .AND. I.EQ.NPERS) PEN = 0
      IF (SHRINK) CALL LINE (X1,Y1,X2,Y2,PEN,0)
      IF (PEN .EQ. 0) PEN = IPEN
      GO TO 110
  105 P(3,NN) = X(1,GP) - (X(1,GP)-ZC)*SHK
      P(1,NN) = X(2,GP) - (X(2,GP)-XC)*SHK
      P(2,NN) = X(3,GP) - (X(3,GP)-YC)*SHK
      IF (DEFORM .EQ. 0) GO TO 110
      P(1,NN) = U(1,GP) - (X(2,GP)-XC)*SHK
      P(2,NN) = U(2,GP) - (X(3,GP)-YC)*SHK
  110 CONTINUE
      IF (SHRINK .AND. .NOT.HIDDEN) GO TO 120
      CALL WRITE (NSCR2,NOFSUR,NWDS,0)
      NOFSUR = NOFSUR + 1
      IF (SOLID.EQ.0 .OR. .NOT.HIDDEN) GO TO 120
C
C     SAVE SOLID SURFACE DATA IN IZ SPACE FOR SECOND PROCESSING, HIDDEN
C     PLOT ONLY. SAVE AS MANY AS OPEN CORE CAN HOLD
C
      IF (LS .GE. LSMAX) GO TO 120
      LS = LS + 1
      NPS1 = NPERS - 1
      DO 112 I = 1,NPS1
      M  = MM + I
      N  = LET(M)
      GP = G(N)
      TEMP(I     ) = GP
  112 TEMP(I+NPS1) = GP
      M  = 1
      MIN= TEMP(1)
      DO 114 I = 2,NPS1
      IF (TEMP(I) .GE. MIN) GO TO 114
      M  = I
      MIN= TEMP(I)
  114 CONTINUE
      IF (M .EQ. 1) M = M + NPS1
      N = + 1
      IF (TEMP(M-1) .LT. TEMP(M+1)) N = -1
      IF (N.EQ.-1  .AND. M.LT.NPS1) M = M + NPS1
      K = NPS1 + 2
      DO 116 I = 3,K
      IZ(I,LS) = TEMP(M)
  116 M = M + N
      IZ(1,LS) = NOFSUR
      IZ(2,LS) = NPS1
C
  120 CONTINUE
      GO TO 60
C
C     CHECK FOR PDUM ELEMENTS BEFORE  EJECTING
C
  130 DO 135 I = 1,9
      IF (ETYP .EQ. LDX(I)) GO TO 160
  135 CONTINUE
C
C     ILLEGAL ELEMENT, NO CORE FOR 1 ELEMENT
C
  140 G(1) = 2
      G(2) = ETYP
      G(3) = NGPEL
      CALL WRTPRT (MERR,G,M1,NM1)
C
C     READ TO THE END OF THIS ELEMENT
C
  150 CALL READ (*180,*20,ELSET,ELID,1,0,M)
      IF (ELID .LE. 0) GO TO 20
      J = 1 + NGPEL + OFFSET
      CALL FREAD (ELSET,0,-J,0)
      GO TO 150
  160 WRITE  (IOUT,170) I
  170 FORMAT ('0*** MISSING PDUM',I1,' SUBROUTINE/HDSURF')
      GO TO 140
  180 CALL MESAGE (-8,ELSET,NAME)
C
  190 CONTINUE
      MAXSF = NOFSUR
      CALL BCKREC (ELSET)
      IF (SHRINK) CALL LINE (0.,0.,0.,0.,1.,+1)
      IF (.NOT.HIDDEN) GO TO 300
      CALL WRITE (NSCR2,0,0,1)
      IF (LS .LT. 60) GO TO 280
C
C     REPROCESS NSCR2 TO REMOVE DUPLICATE SURFACES (INTERIOR-INTERFACES)
C     AND SAVE REDUCED DATA IN NSCR1.
C     INTERCHANGE NSCR1 AND NSCR2 INDICES
C
      J = (LETSZ2+1)*LS
      CALL SORT2K (0,0,LETSZ2+1,3,IZ,J)
      M = 0
      NPS1 = 0
      DO 240 I = 1,LS
      NPS2 = IZ(2,I) + 2
      IF (NPS2 .EQ. NPS1) GO TO 200
      NPS1 = NPS2
      GO TO 240
  200 IM1  = I - 1
      DO 210 J = 3,NPS1
      IF (IZ(J,I) .NE. IZ(J,IM1)) GO TO 240
  210 CONTINUE
      IF (M .EQ. 0) GO TO 220
      IF (IZ(M,1) .EQ. IZ(1,IM1)) GO TO 230
  220 M = M + 1
      IZ(M,1) = IZ(1,IM1)
  230 M = M + 1
      IZ(M,1) = IZ(1,I)
  240 CONTINUE
C
      IF (M .LT. 20) GO TO 280
      CALL SORT (0,0,1,1,IZ,M)
      IZ(M+1,1) = 999999999
      FILE = NSCR1
      CALL GOPEN (NSCR1,GPLST(IB+IBUF),1)
      FILE = NSCR2
      CALL CLOSE (NSCR2,1)
      CALL GOPEN (NSCR2,GPLST(IB),0)
      N = 1
      DO 270 I = 1,MAXSF
      CALL READ (*320,*330,NSCR2,NOFSUR,NWDS,0,J)
      IF (I-IZ(N,1)) 250,260,260
  250 CALL WRITE (NSCR1,NOFSUR,NWDS,0)
      GO TO 270
  260 N = N + 1
  270 CONTINUE
C
      CALL CLOSE (NSCR2,1)
      J = NSCR2
      NSCR2 = NSCR1
      NSCR1 = J
      MAXSF = MAXSF - M
      CALL WRITE (NSCR2,0,0,1)
  280 CALL CLOSE (NSCR2,1)
  300 RETURN
C
  310 J = -1
      FILE = ELSET
      GO TO 340
  320 J = -2
      GO TO 340
  330 J = -3
  340 CALL MESAGE (J,FILE,NAME)
      GO TO 190
      END
