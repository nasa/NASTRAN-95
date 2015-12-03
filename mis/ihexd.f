      SUBROUTINE IHEXD (TYPE)
C
C     DOUBLE PRECISION VERSION
C
C     THIS ROUTINE PROCESSES IHEX1, IHEX2, AND IHEX3 ELEMENT DATA TO
C     PRODUCE STIFFNESS AND MASS MATRICES.  IF THE HEAT TRANSFER OPTION
C     IS ON, CONDUCTIVITY AND CAPACITY MATRICES ARE PRODUCED.  IF THE
C     DISPLACEMENT VECTOR POINTER IS NON-ZERO, THE DIFFERENTIAL
C     STIFFNESS MATRIX ONLY IS PRODUCED.
C
C           TYPE = 1    IHEX1
C           TYPE = 2    IHEX2
C           TYPE = 3    IHEX3
C
C           THE EST ENTRIES ARE
C
C     NAME  ----------INDEX----------   DESCRIPTION
C            IHEX1    IHEX2    IHEX3
C
C     EID        1        1        1    ELEMENT ID NO.
C     SIL      2-9     2-21     2-33    SCALAR INDEX LIST
C     MID       10       22       34    MATERIAL ID NO.
C     CID       11       23       35    MATERIAL COORD. SYSTEM ID NO.
C     NIP       12       24       36    NO. INTEGRATION POINTS PER EDGE
C     MAXAR     13       25       37    MAX ASPECT RATIO
C     ALFA      14       26       38    MAX ANGLE FOR NORMALS
C     BETA      15       27       39    MAX ANGLE FOR MIDSIDE POINTS
C     BGPDT  16-47   28-107   40-167    BASIC GRID POINT DATA
C     GPT    48-55  108-127  168-199    GRID POINT TEMPERATURES
C
C     - INSTALLATION NOTE --
C     GPTLD IS SUPPOSED TO CONTAIN GRID POINT TEMPERATURE LOADS FOR
C     COMPUTING DIFFERENTIAL STIFFNESS.  FOR INSTALLATION, GPTLD MUST
C     BE LOADED WITH DATA BY EMG.  IF GPTLD(1)=-1, NO TEMP LOAD IS
C     ASSUMED.
C
      LOGICAL          ANIS       ,RECT       ,TDEP       ,DIAG       ,
     1                 MTDEP      ,HEAT1      ,NOGO       ,NOCSTM
      INTEGER          HEAT       ,EID        ,SIL(1)     ,SCR4       ,
     1                 TYPE       ,JZ(1)      ,CID        ,IEST(1)    ,
     2                 BCORD      ,BGPDT      ,GPT        ,NC(8)      ,
     3                 EDGE       ,FACE       ,IB(46)     ,ELNO(3)    ,
     4                 EXCD(3)    ,TWINS(9)   ,RVRS(5)    ,IWORK(1)   ,
     5                 BACK       ,OTPT       ,UGV        ,CDAMP      ,
     6                 DICT(40)
      REAL             NU         ,KHEAT      ,MAXAR      ,DMAXAR(3)  ,
     1                 DALFA(3)   ,DBETA(2)   ,EVEC(3,12) ,WORK(66)   ,
     2                 VN(3,2)    ,GPTLD(32)  ,BCD2(3)
      DOUBLE PRECISION Z(1)       ,JACOB(3,3) ,DETJ       ,S(4)       ,
     1                 H(4)       ,GAUSS(8)   ,SFACT      ,PART(3,3)  ,
     2                 E1         ,E2         ,E3         ,TF(3,3)    ,
     3                 TK(3,3)    ,PRT1       ,SIG(6)     ,SX         ,
     4                 SY         ,SZ         ,SXY        ,SYZ        ,
     5                 SZX        ,STR(18)    ,C(3,3)     ,TEMP
      DOUBLE PRECISION GMAT(36)   ,DALPHA(6)  ,STORE(45)  ,TVOL
      CHARACTER        UFM*23
      COMMON /XMSSG /  UFM
      COMMON /MATIN/   MID        ,INFLAG     ,TEMP
      COMMON /MATOUT/  E          ,G          ,NU         ,RHO        ,
     1                 TALPHA     ,TREF       ,CDAMP      ,SPACE(18)  ,
     3                 MTDEP
      COMMON /MATISO/  BUFM6(46)
C     OMMON  /MATISO/  G11,G12,G13,...,G46,G56,G66,RHO,AXX,AYY,AZZ,AXY,
C                      AYZ,AZX,TREF,GE,IER
      COMMON /BLANK /  SKIP16(16) ,VOLUME     ,SURFAC
      COMMON /HMTOUT/  KHEAT(6)   ,CP
C     COMMON /EMG***/  ...,UGV,...
C
C     - INSTALLATION NOTE --
C     UGV POINTS TO BEGINNING OF SINGLE PRECISION GLOBAL DISPLACEMENT
C     VECTOR IN OPEN CORE ARRAY RZ.
C
      COMMON /EMGPRM/  IEXT       ,IZS        ,NZS         ,DUM(12)   ,
     1                 KGG1       ,MGG1       ,BGG1        ,IPREC     ,
     2                 NOGO       ,HEAT1
C
C     SZ IS OPEN CORE.  USE ONLY RZ(IZS) TO RZ(NZS).
C
      COMMON /ZZZZZZ/  RZ(1)
      COMMON /EMGEST/  EST(200)
      COMMON /SYSTEM/  SYSBUF     ,OTPT       ,SYS1(7)    ,MTEMP
      COMMON /EMGDIC/  SPAC(2)    ,NGRIDS     ,SPAC1      ,IESTID
      EQUIVALENCE      (Z(1),JZ(1),RZ(1))     ,(EID,EST(1),IEST(1))   ,
     1                 (SIL(1),EST(2))        ,(WORK(1),IWORK(1))     ,
     2                 (SIG(1),SX)            ,(SIG(2),SY)            ,
     3                 (SIG(3),SZ)            ,(SIG(4),SXY)           ,
     4                 (SIG(5),SYZ)           ,(SIG(6),SZX)           ,
     5                 (DSTLD,IDSTLD)
      EQUIVALENCE      (WORK(1),EVEC(1,1))    ,(WORK(37),VN(1,1))     ,
     1                 (WORK(43),NC(1))
      EQUIVALENCE      (WORK(1),JACOB(1,1))   ,(WORK(19),H(1))        ,
     1                 (WORK(27),S(1))        ,(WORK(35),PART(1,1))   ,
     2                 (WORK(53),SIG(1))      ,(WORK(1),C(1,1))
      EQUIVALENCE      (WORK(1),TF(1,1))      ,(WORK(35),TK(1,1))
      EQUIVALENCE      (IB(1),BUFM6(1))
      DATA    SCR4  /  304 /
      DATA    BCD1  ,  BCD2/ 4HCIHE, 4HX1  , 4HX2  , 4HX3   /
      DATA    DMAXAR,  DALFA,DBETA / 5.0      ,10.0       ,15.0       ,
     1                               45.0      ,45.0       ,45.0       ,
     2                                          45.0       ,45.0       /
      DATA    DTOR  ,  GAUSS /0.017453292519943E0,
     1                        0.577350269189626D0,
     2                        0.555555555555556D0,
     3                        0.774596669241483D0,
     4                        0.888888888888889D0,
     5                        0.347854845137454D0,
     6                        0.861136311594053D0,
     7                        0.652145154862546D0,
     8                        0.339981043584856D0/
      DATA    IHEX,ELNO       /4HIHEX,4H ELE,4HMENT,4H NO./
      DATA    BAR,BALFA,BBETA /4H  AR,4HALFA,4HBETA/
      DATA    EXCD            /4H EXC,4HEEDE,4HD.  /
      DATA    RVRS            /4HREVE,4HRSED,4H NUM,4HBERI,4HNG. /
      DATA    TWINS           /4HCOOR,4HDINA,4HTES ,4HOF T,4HWO P,
     1                         4HOINT,4HS AR,4HE SA,4HME. /
      DATA    NERR1,NERR2     /3301, 3302 /
C
C     FOR DOUBLE PRECISION, OPEN CORE POINTERS MUST BE MODIFIED
C
      IZ = IZS/2 + 1
      NZ = NZS/2 + 1
C
C     THIS ROUTINE OPERATES IN DOUBLE PRECISION.
C     EMGOUT WILL PRODUCE THE REQUIRED MATRIX IN THE REQUESTED PRECISION
C
C     ALLOCATE LARGE ARRAYS IN OPEN CORE
C
      NGP  = 12*TYPE - 4
      HEAT = 0
      KGG  = 0
      MGG  = 0
      IF (HEAT1) HEAT = 1
      IF (KGG1 .NE. 0) KGG = 1
      IF (MGG1 .NE. 0) MGG = 1
      NGRIDS = NGP
      UGV  = 0
      NGG  = 3*NGP
      DICT(1) = IESTID
      DICT(2) = 1
      IF (.NOT.HEAT1) GO TO 5
      DICT(3) = NGP
      DICT(4) = 1
      GO TO 30
    5 DICT(3) = NGG
      DICT(4) = 7
      IF (KGG .LE. 0) GO TO 10
      IK = IZ + 3*NGG
      NK = IK - 1 + (NGG+1)*NGG/2
      GO TO 20
   10 IK = IZ
      NK = IK + 3*NGG - 1
      IM = NK + 1
      NM = (NGP+1)*NGP/2 + NK
      GO TO 40
   20 NM = NK
      IF (MGG .LE. 0) GO TO 40
      IM = NK + 1
      NM = NK + (NGP+1)*NGP/2
      GO TO 40
   30 IK = IZ + 17
      NK = IK - 1 + NGP**2
      IM = NK + 1
      NM = IM - 1 + NGP**2
      NGG= NGP
   40 IN = NM + 1
      IG = IN + NGP
      IX = IG + 3*NGP
      ND = NM + 9*NGP
      IF (UGV .EQ. 0) GO TO 50
      ID = ND + 1
      ND = ID + NGG - 1
   50 IF (ND .LE. NZ) GO TO 100
      WRITE (OTPT,7100) UFM,NERR1,IHEX,TYPE,ELNO,EID
      NOGO = .TRUE.
C
C     ***** OPEN CORE MAP *****
C
C     DOUBLE PRECISION Z(1)
C     COMMON /EMGZZZ/  Z
C
C     NGG = ORDER OF ELEMENT MATRIX
C
C     INDEX      STIFFNESS             MASS                HEAT
C                AND MASS              ONLY              TRANSFER
C
C     IZ    NGG BY 3 PARTITION  NGG BY 3 PARTITION  FOUR WORD COORDINATE
C           OF MATRIX           OF MATRIX           VECTOR.  INPUT TO
C                                                   TRANSD
C
C     IZ+2                                          TRANSFORMED THERMAL
C                                                   CONDUCTANCE MATRIX
C
C     IT                                            MATERIAL TRANSFOR-
C                                                   MATION MATRIX
C
C     IK    SYMMETRIC HALF OF   SAME AS IZ          FULL CONDUCTANCE
C           STIFFNESS
C
C     IM    SYMMETRIC HALF OF   SYMMETRIC HALF OF   FULL CAPACITANCE
C           MASS                MASS
C
C     IN    --------------------SHAPE FUNCTIONS-------------------------
C
C     IG    --------------------D(SHAPE)/D(GREEK)-----------------------
C
C     IX    --------------------D(SHAPE)/D(BASIC XYZ)-------------------
C
C     ID    DISPLACEMENT
C           VECTOR IN BASIC
C           COORDINATES
C
C     CHECK GEOMETRY.  THE FOLLOWING CHECKS ARE MADE
C           1.  ASPECT RATIO
C           2.  ANGLES BETWEEN NORMALS OF SUB-TRIANGLES ON EACH FACE
C           3.  ANGLES BETWEEN VECTORS BETWEEN POINTS ALONG EACH EDGE
C           4.  REVERSE SEQUENCING
C           5.  DUPLICATE COORDINATE VALUES
C
C     FETCH EPT DATA, COMPUTE EST POINTERS
C
  100 MID  = 10 + 12*(TYPE-1)
      CID  = IEST(MID+1)
      NIP  = IEST(MID+2)
      MAXAR= EST(MID+3)
      ALFA = EST(MID+4)
      BETA = EST(MID+5)
      BGPDT= MID + 6
      GPT  = BGPDT + NGP*4
      MID  = IEST(MID)
      IF (NIP.LT.2 .OR. NIP.GT.4) NIP = TYPE/2 + 2
      IF (MAXAR .LE. 0.0) MAXAR = DMAXAR(TYPE)
      IF (ALFA  .LT. 0.0) ALFA  = DALFA(TYPE)
      IF (BETA.LT.0.0 .AND. TYPE.NE.1) BETA = DBETA(TYPE-1)
      ALFA = COS(DTOR*ALFA)
      BETA = COS(DTOR*BETA)
      IF (UGV .EQ. 0) GO TO 105
C
C     TRANSFORM DISPLACEMENT VECTOR TO BASIC COORDINATES
C     MULTIPLY BY 1/4 TO AVOID MULTIPLYING STRAIN-DISPLACEMENT
C     RELATIONS BY 1/2 UNDER THE INTEGRAL.  DITTO FOR LOADING TEMP-S.
C
      DSTLD = GPTLD(1)
      DO 104 I = 1,NGP
      M = BGPDT + 4*I - 4
      J = UGV + SIL(I) - 1
      K = ID + 3*I - 3
      IF (IEST(M) .EQ. 0) GO TO 102
      CALL TRANSD (EST(M),TK)
      DO 101 L = 1,3
  101 Z(IZ+L-1) = DBLE(RZ(J+L-1)*0.25)
      CALL GMMATD (TK,3,3,0,Z(IZ),3,1,0,Z(N))
      GPTLD(I) = 0.25*GPTLD(I)
      GO TO 104
  102 DO 103 L = 1,3
  103 Z(N+L-1) = DBLE(RZ(J+L-1)*0.25)
      GPTLD(I) = 0.25*GPTLD(I)
  104 CONTINUE
C
C     REARRANGE BGPDT
C
  105 DO 110 I = 1,NGP
  110 JZ(IZS+I) = IEST(BGPDT+I*4-4)
      BCORD = GPT - 3
      DO 120 I = 2,NGP
      DO 120 J = 1,3
      K = BGPDT + 4*(NGP-I) + 4 - J
      BCORD = BCORD - 1
      EST(BCORD) = EST(K)
  120 CONTINUE
      DO 130 I = 2,NGP
  130 IEST(BGPDT+I-1) = JZ(IZS+I)
C
C     IF COMPUTING DIFFERENTIAL STIFFNESS, SKIP CHECKS
C
      IF (UGV .GT. 0) GO TO 500
C
C     FIND 8 POINTERS TO CORNER COORDINATES IN EST
C
C     EDGE        CORNERS
C       1         1     2
C       2         2     3
C       3         3     4
C       4         4     1
C       5         1     5
C       6         2     6
C       7         3     7
C       8         4     8
C       9         5     6
C      10         6     7
C      11         7     8
C      12         8     5
C
      NC(1) = BCORD
      J = 3*TYPE
      GO TO (140,150,160), TYPE
  140 NC(5) = BCORD + 12
      GO TO 170
  150 NC(5) = BCORD + 36
      GO TO 170
  160 NC(5) = BCORD + 60
  170 DO 180 I = 2,4
      NC(I  ) = NC(I-1) + J
  180 NC(I+4) = NC(I+3) + J
C
C     COMPUTE 12 EDGE VECTORS, FIND SMALLEST AND LARGEST MAGNITUDES
C
      I = 0
      J = 1
      SMAG = 1.0E20
      BMAG = 0.0
      DO 250 EDGE = 1,12
      GO TO (190,190,190,200,210,190,190,190,220,190,190,200), EDGE
  190 I = I + 1
      J = J + 1
      L = NC(I) - 1
      M = NC(J) - 1
      GO TO 230
  200 L = M
      M = NC(J-3) - 1
      GO TO 230
  210 I = 0
      J = 4
      GO TO 190
  220 I = 4
      J = 5
      GO TO 190
  230 TMAG = 0.0
      DO 240 K = 1,3
      EVEC(K,EDGE) = EST(M+K) - EST(L+K)
  240 TMAG = TMAG + EVEC(K,EDGE)**2
      IF (TMAG .LT. SMAG) SMAG = TMAG
      IF (TMAG .GT. BMAG) BMAG = TMAG
  250 CONTINUE
C
C     CHECK ASPECT RATIO
C
      IF (SMAG .GT. 0.0) GO TO 260
      SMAG = 1.0E-10
  260 IF (BMAG/SMAG .LE. MAXAR**2) GO TO 265
      WRITE (OTPT,7200) UFM,NERR2,IHEX,TYPE,ELNO,EID,BAR,EXCD
      NOGO = .TRUE.
C
C     CHECK ANGLES BETWEEN FACE NORMALS
C
C     FACE              CORNERS
C       1         1     4     3     2
C       2         1     2     6     5
C       3         2     3     7     6
C       4         3     4     8     7
C       5         4     1     5     8
C       6         5     6     7     8
C
  265 DO 350 FACE = 1,6
      GO TO (270,280,290,290,300,310), FACE
  270 I = 1
      J = 4
      K = 3
      L = 2
      GO TO 320
  280 I = 1
      J = 6
      K = 9
      L = 5
      GO TO 320
  290 I = I + 1
      J = J + 1
      K = K + 1
      L = L + 1
      GO TO 320
  300 I = 4
      J = 5
      K = 12
      L = 8
      GO TO 320
  310 I = 12
      J = 9
      K = 10
      L = 11
  320 DO 340 N = 1,2
      VN(1,1) = EVEC(2,I)*EVEC(3,J) - EVEC(3,I)*EVEC(2,J)
      VN(2,1) = EVEC(3,I)*EVEC(1,J) - EVEC(1,I)*EVEC(3,J)
      VN(3,1) = EVEC(1,I)*EVEC(2,J) - EVEC(2,I)*EVEC(1,J)
      VN(1,2) = EVEC(2,K)*EVEC(3,L) - EVEC(3,K)*EVEC(2,L)
      VN(2,2) = EVEC(3,K)*EVEC(1,L) - EVEC(1,K)*EVEC(3,L)
      VN(3,2) = EVEC(1,K)*EVEC(2,L) - EVEC(2,K)*EVEC(1,L)
      SMAG = 0.0
      BMAG = 0.0
      TMAG = 0.0
      DO 330 M = 1,3
      SMAG = SMAG + VN(M,1)**2
      BMAG = BMAG + VN(M,2)**2
  330 TMAG = VN(M,1)*VN(M,2) + TMAG
      SMAG = SQRT(SMAG*BMAG)
      IF (SMAG .EQ. 0.0) GO TO 335
C
C     EPSILON INTRODUCED TO OVERCOME ROUNDOUT ERROR
C
      IF (TMAG/SMAG .GE. 0.99*ALFA) GO TO 335
      WRITE (OTPT,7200) UFM,NERR2,IHEX,TYPE,ELNO,EID,BALFA,EXCD
      NOGO = .TRUE.
  335 M = I
      I = L
      L = K
      K = J
      J = M
  340 CONTINUE
  350 CONTINUE
C
C     CHECK MID-EDGE POINTS
C
      IF (TYPE .EQ. 1) GO TO 455
      M = 1
      DO 450 EDGE = 1,12
      GO TO (370,370,370,370,380,390,390,390,400,370,370,370), EDGE
  370 I = NC(M)
      J = I + 3
      K = J + 3
      L = K + 3
      M = M + 1
      IF (EDGE.NE.4 .AND. EDGE.NE.12) GO TO 410
      IF (TYPE .EQ. 2) K = NC(M-4)
      IF (TYPE .EQ. 3) L = NC(M-4)
      GO TO 410
  380 M = 0
  390 M = M + 1
      I = NC(M)
      J = I + 12*TYPE - 3*(M-1)*(TYPE-1)
      K = J + 12
      K = K + 3*(M-1)*(3-TYPE)
      L = NC(M+4)
      GO TO 410
  400 M = 5
      GO TO 370
  410 SMAG = 0.0
      BMAG = 0.0
      TMAG = 0.0
      DO 420 N = 1,3
      VN(N,1) = EST(J+N-1) - EST(I+N-1)
      VN(N,2) = EST(K+N-1) - EST(J+N-1)
      TMAG = TMAG + VN(N,1)*VN(N,2)
      SMAG = SMAG + VN(N,1)**2
  420 BMAG = BMAG + VN(N,2)**2
      SMAG = SQRT(SMAG*BMAG)
      IF (SMAG .EQ. 0.0) GO TO 430
      IF (TMAG/SMAG .GE. BETA) GO TO 430
      GO TO 445
  430 IF (TYPE .EQ. 2) GO TO 450
      TMAG = 0.0
      SMAG = 0.0
      DO 440 N = 1,3
      VN(N,1) = EST(L+N-1) - EST(K+N-1)
      TMAG = TMAG + VN(N,1)*VN(N,2)
  440 SMAG = SMAG + VN(N,1)**2
      SMAG = SQRT(SMAG*BMAG)
      IF (SMAG .EQ. 0.0) GO TO 450
      IF (TMAG/SMAG .GE. BETA) GO TO 450
  445 WRITE (OTPT,7200) UFM,NERR2,IHEX,TYPE,ELNO,EID,BBETA,EXCD
      NOGO = .TRUE.
  450 CONTINUE
C
C     CHECK FOR LEFT-HANDED ELEMENT COORDINATE SYSTEM
C
C     VOL = EVEC(5)*(EVEC(1) X -EVEC(4))
C
  455 VN(1,1) = EVEC(2,4)*EVEC(3,1) - EVEC(3,4)*EVEC(2,1)
      VN(2,1) = EVEC(3,4)*EVEC(1,1) - EVEC(1,4)*EVEC(3,1)
      VN(3,1) = EVEC(1,4)*EVEC(2,1) - EVEC(2,4)*EVEC(1,1)
      TMAG    = 0.0
      DO 460 I = 1,3
  460 TMAG = TMAG + EVEC(I,5)*VN(I,1)
      IF (TMAG .GT. 0.0) GO TO 470
      WRITE (OTPT,7200) UFM,NERR2,IHEX,TYPE,ELNO,EID,RVRS
      NOGO = .TRUE.
C
C     CHECK FOR DUPLICATE COORDINATE VALUES
C
  470 L = NGP - 1
      DO 490 I = 1,L
      M = BCORD + 3*(I-1)
      K = I + 1
      DO 480 J = K,NGP
      N = BCORD + 3*(J-1)
      IF (EST(M  ) .NE. EST(N  )) GO TO 480
      IF (EST(M+1) .NE. EST(N+1)) GO TO 480
      IF (EST(M+2) .NE. EST(N+2)) GO TO 480
      WRITE (OTPT,7200) UFM,NERR2,IHEX,TYPE,ELNO,EID,TWINS
      NOGO = .TRUE.
  480 CONTINUE
  490 CONTINUE
C
C     IF NOGO FLAG ON, DON T COMPUTE ELEMENT MATRICES
C
      IF (NOGO) RETURN
C
C     INITIALIZE FOR NUMERICAL INTEGRATION
C
C     ABSCISSAE AND WEIGHT COEFFICIENTS FOR GAUSSIAN QUADRATURE
C
  500 I = NIP - 1
      GO TO (510,520,530), I
  510 H(1) = 1.0
      S(1) = GAUSS(1)
      H(2) = 1.0
      S(2) =-GAUSS(1)
      GO TO 540
  520 H(1) = GAUSS(2)
      S(1) = GAUSS(3)
      H(2) = GAUSS(4)
      S(2) = 0.0
      H(3) = GAUSS(2)
      S(3) =-GAUSS(3)
      GO TO 540
  530 H(1) = GAUSS(5)
      S(1) = GAUSS(6)
      H(2) = GAUSS(7)
      S(2) = GAUSS(8)
      H(3) = GAUSS(7)
      S(3) =-GAUSS(8)
      H(4) = GAUSS(5)
      S(4) =-GAUSS(6)
C
C     GENERATE TABLE OF EQUIVALENTS IN SIL ARRAY SO MATRIX WILL BE
C     ORDERED ACCORDING TO INCREASING SIL NUMBERS
C
  540 I = -NGP
  545 J =  0
      DO 560 K = 1,NGP
      IF (SIL(K) .LT. J) GO TO 560
      J = SIL(K)
      L = K
  560 CONTINUE
      SIL(L) = I
      I = I + 1
      IF (I .LT. 0) GO TO 545
      DO 570 I = 1,NGP
  570 SIL(I) = -SIL(I)
C
C     NOW SIL(I) = PARTITION NUMBER OF ELEMENT GRID POINT I
C
C     ZERO OUT OPEN CORE FOR MATRIX SUMMATION
C
      DO 580 I = IK,NM
  580 Z(I) = 0.0
C
C     BRANCH ON HEAT TRANSFER FLAG
C
      IF (HEAT .EQ. 1) GO TO 3000
C
C     FETCH MATERIAL PROPERTIES
C
C     =============================================================
C     THIS SECTION OF CODE MUST BE UPDATED WHEN GENERAL ANISOTROPIC
C     MATERIAL IS ADDED.
C
C     TEST FOR ANISOTROPIC MATERIAL
C
      INFLAG = 10
      ANIS   =.FALSE.
C
C     TEST FOR RECTANGULAR COORDINATE SYSTEM IN WHICH THE ANISOTROPIC
C     MATERIAL IS DEFINED
C
      RECT = .TRUE.
C     ===============================================================
C
C     CHECK FOR TEMPERATURE DEPENDENCE
C
      TDEP = .TRUE.
      DO 610 I = 2,NGP
      IF (EST(GPT) .NE. EST(GPT+I-1)) GO TO 630
  610 CONTINUE
      TDEP = .FALSE.
  630 TEMP = EST(GPT)
      CALL MAT (EID)
      IF (.NOT.MTDEP) TDEP = .FALSE.
      IF (IB(46) .EQ. 6) ANIS = .TRUE.
      IF (KGG .LE. 0) GO TO 1000
C
C     IF ISOTROPIC, TEMPERATURE INDEPENDENT MATERIAL, COMPUTE CONSTANTS
C
      IF (ANIS .OR. TDEP) GO TO 1000
      IF (IB(46) .NE.  0) GO TO 640
      WRITE (OTPT,7300) UFM,MID,EID
      NOGO = .TRUE.
      RETURN
C
C     SET UP FOR EASY MULTIPLICATION IF MATERIALS ARE ON MAT1
C
  640 E1 = BUFM6(1)
      E2 = BUFM6(2)
      E3 = BUFM6(22)
C
C     ============================================================
C     CODE TO TRANSFORM GENERAL ANISOTROPIC MATERIAL PROPERTIES TO
C     BASIC COORDINATE SYSTEM MUST BE ADDED HERE.
C     ============================================================
C
C     ALL SET TO BEGIN INTEGRATION LOOPS.  DO IT.
C
 1000 TVOL = 0.0D+0
      DO 2000 I = 1,NIP
      DO 2000 J = 1,NIP
      DO 2000 K = 1,NIP
C
C     GENERATE SHAPE FUNCTIONS AND JACOBIAN MATRIX INVERSE
C
      CALL IHEXSD (TYPE,Z(IN),Z(IG),JACOB,DETJ,EID,S(I),S(J),S(K),
     1             EST(BCORD))
      IF (DETJ .NE. 0.0) GO TO 1010
C
C     BAD ELEMENT IF FALL HERE.  JACOBIAN MATRIX WAS SINGULAR.
C
      NOGO = .TRUE.
      RETURN
C
 1010 SFACT = H(I)*H(J)*H(K)*DETJ
      TVOL  = TVOL + SFACT
      IF (KGG .LE. 0) GO TO 1015
C
C     STIFFNESS
C
C     COMPUTE STRAIN-DISPLACEMENT RELATIONS
C
C     MUST REVERSE CALLING ORDER SINCE MATRICES ARE STORED BY COLUMNS
C
      CALL GMMATD (Z(IG),NGP,3,0,JACOB,3,3,0,Z(IX))
C
C     IF MATERIAL IS TEMPERATURE DEPENDENT, MUST COMPUTE TEMPERATURE
C     AT THIS INTEGRATION POINT AND FETCH MATERIAL PROPERTIES AGAIN
C
 1015 IF (.NOT. TDEP) GO TO 1030
      TEMP = 0.0
      DO 1020 L = 1,NGP
 1020 TEMP = TEMP + Z(IN+L-1)*EST(GPT+L-1)
      CALL MAT (EID)
      IF (KGG .LE. 0) GO TO 1100
      IF (ANIS) GO TO 1040
      IF (IB(46) .NE. 0) GO TO 1025
      WRITE (OTPT,7300) UFM,MID,EID
      NOGO = .TRUE.
      RETURN
C
 1025 E1 = BUFM6(1)
      E2 = BUFM6(2)
      E3 = BUFM6(22)
      GO TO 1100
 1030 IF (KGG .LE. 0) GO TO 1100
C
C     IF MATERIAL IS ANISOTROPIC AND NOT DEFINED IN RECTANGULAR COOR-
C     DINATE SYSTEM, MUST TRANSFORM TO BASIC COORDINATE SYSTEM AT THIS
C     INTEGRATION POINT
C     IN THIS VERSION, ANISOTROPIC MATERIAL SYSTEMS MUST BE RECTANGULAR.
C     THEREFORE, NO FURTHER TRANSFORMATIONS ARE NECESSARY
C
C
C     ================================================================
C     THIS CODE MUST BE COMPLETED WHEN GENERAL ANISOTROPIC MATERIAL IS
C     ADDED
C
      IF (.NOT.ANIS) GO TO 1100
 1040 CONTINUE
C
C     INSERT GLOBAL TO BASIC TRANSFORMATION OPERATIONS HERE FOR
C     ANISOTROPIC MATERIAL MATRIX
C     =============+==================================================
C
      DO 1041 IJK = 1,36
 1041 GMAT(IJK) = BUFM6(IJK)
      IF (RECT) GO TO 1100
C
C     MATERIAL HAS BEEN EVALUATED FOR THIS INTEGRATION POINT WHEN
C     FALL HERE.
C
 1100 IF (UGV .EQ. 0) GO TO 1170
C
C     COMPUTE STRESSES FOR DIFFERENTIAL STIFFNESS MATRIX
C
C     THERMAL EFFECTS
C
      IF (IDSTLD .EQ. -1) GO TO 1120
      TEMP = 0.0
      DO 1110 L = 1,NGP
 1110 TEMP = TEMP + Z(IN+L-1)*DBLE(GPTLD(L))
      TEMP = TEMP - DBLE(TREF)
      IF (ANIS) GO TO 1115
      SIG(1) =-DBLE(TALPHA)*(E1+2.0*E2)*TEMP
      SIG(2) = SIG(1)
      SIG(3) = SIG(1)
      SIG(4) = 0.0
      SIG(5) = 0.0
      SIG(6) = 0.0
      GO TO 1140
C     ===========================================================
 1115 CONTINUE
C
C     ADD THERMAL STRESS COMPUTATIONS FOR ANISOTROPIC MATERIAL
C
C     STORE ALPHA IN DOUBLE PRECISION
C
      DO 1116 IJK = 1,6
 1116 DALPHA(IJK) = BUFM6(IJK+37)
C
      CALL GMMATD (GMAT,6,6,0, DALPHA,6,1,0,SIG)
      DO 1117 IJK = 1,6
 1117 SIG(IJK) = -SIG(IJK)*TEMP
      GO TO 1140
C     ===========================================================
 1120 DO 1130 L = 1,6
 1130 SIG(L) = 0.0
C
C     DISPLACEMENT EFFECTS, COMPUTE STRESS MATRIX AND MULTIPLY BY DISPL.
C
 1140 STR(12) = 0.0
      STR(13) = 0.0
      STR(17) = 0.0
      DO 1160 L = 1,NGP
      II = IX + 3*L - 4
      IF (ANIS) GO TO 1145
      STR( 1) = E1*Z(II+1)
      STR( 2) = E2*Z(II+2)
      STR( 3) = E2*Z(II+3)
      STR( 4) = E2*Z(II+1)
      STR( 5) = E1*Z(II+2)
      STR( 6) = E2*Z(II+3)
      STR( 7) = E2*Z(II+1)
      STR( 8) = E2*Z(II+2)
      STR( 9) = E1*Z(II+3)
      STR(10) = E3*Z(II+2)
      STR(11) = E3*Z(II+1)
      STR(14) = E3*Z(II+3)
      STR(15) = E3*Z(II+2)
      STR(16) = E3*Z(II+3)
      STR(18) = E3*Z(II+1)
      GO TO 1150
C     =========================================================
C
 1145 CONTINUE
C
C     ADD STRESS MATRIX COMPUTATION FOR ANISOTROPIC MATERIAL
C
      DO 1146 IJK = 1,18
 1146 STORE(IJK) = 0.D0
      STORE( 1) = Z(II+1)
      STORE( 5) = Z(II+2)
      STORE( 9) = Z(II+3)
      STORE(10) = Z(II+2)
      STORE(11) = Z(II+1)
      STORE(14) = Z(II+3)
      STORE(15) = Z(II+2)
      STORE(16) = Z(II+3)
      STORE(18) = Z(II+1)
C
      CALL GMMATD (GMAT,6,6,0,STORE(1),6,3,0,STR)
C
C     ============================================================
C
 1150 CALL GMMATD (STR,6,3,-2,Z(ID+3*L-3),3,1,0,SIG)
 1160 CONTINUE
      STR(1) = SX
      SX = SX + SY
      SY = SY + SZ
      SZ = SZ + STR(1)
C
C     NOW BEGIN LOOPS OVER GRID POINTS ALONG ROWS AND COLUMNS
C
 1170 DO 1400 N = 1,NGP
      DO 1400 M = N,NGP
C
C     COMPUTE PARTITION FOR POINTWISE ROW M AND COLUMN N
C
      IF (KGG .LE. 0) GO TO 1300
      IF (.NOT.ANIS ) GO TO 1200
C
C     =================================================================
C     MUST ADD CODE TO COMPUTE THE CONTRIBUTION TO THE STIFFNESS MATRIX
C     FOR ANISOTROPIC MATERIAL HERE
C     =================================================================
C
 1200 IF (SIL(M) .GE. SIL(N)) GO TO 1210
C
C     MUST COMPUTE TRANSPOSE OF THIS PARTITION FOR SUMMATION IN ELEMENT
C     MATRIX
C
      MZ = IX + (N-1)*3
      NZ = IX + (M-1)*3
      GO TO 1220
 1210 MZ = IX + (M-1)*3
      NZ = IX + (N-1)*3
 1220 IF (UGV .EQ. 0) GO TO 1222
C
C     DIFFERENTIAL STIFFNESS
C
      DO 1221 L = 1,3
      DO 1221 INC = 1,3
 1221 C(L,INC) = Z(MZ+INC-1)*Z(NZ+L-1)
      PART(1,1) = SX*C(2,2) + SYZ*(C(2,3)+C(3,2)) + SZ*C(3,3)
      PART(2,2) = SY*C(3,3) + SZX*(C(3,1)+C(1,3)) + SX*C(1,1)
      PART(3,3) = SZ*C(1,1) + SXY*(C(1,2)+C(2,1)) + SY*C(2,2)
      PART(2,1) =-SX*C(2,1) + SXY*C(3,3) -SYZ*C(1,3) - SZX*C(2,3)
      PART(3,1) =-SZ*C(3,1) - SXY*C(3,2) -SYZ*C(2,1) + SZX*C(2,2)
      PART(1,2) =-SX*C(1,2) + SXY*C(3,3) -SYZ*C(3,1) - SZX*C(3,2)
      PART(3,2) =-SY*C(3,2) - SXY*C(3,1) +SYZ*C(1,1) - SZX*C(1,2)
      PART(1,3) =-SZ*C(1,3) - SXY*C(2,3) -SYZ*C(1,2) + SZX*C(2,2)
      PART(2,3) =-SY*C(2,3) - SXY*C(1,3) +SYZ*C(1,1) - SZX*C(2,1)
      GO TO 1228
C
C     ELASTIC STIFFNESS
C
 1222 IF (.NOT.ANIS) GO TO 1226
C
C     STORE CI MATRIX
C
      DO 1223 IJK = 1,18
 1223 STORE(IJK) = 0.D0
      STORE( 1) = Z(MZ  )
      STORE( 4) = Z(MZ+1)
      STORE( 6) = Z(MZ+2)
      STORE( 8) = Z(MZ+1)
      STORE(10) = Z(MZ  )
      STORE(11) = Z(MZ+2)
      STORE(15) = Z(MZ+2)
      STORE(17) = Z(MZ+1)
      STORE(18) = Z(MZ  )
C
      CALL GMMATD (STORE(1),3,6,0,GMAT(1),6,6,0,STORE(19))
C
C     STORE CJ
C
      DO 1224 IJK = 1,18
 1224 STORE(IJK) = 0.D0
      STORE( 1) = Z(NZ  )
      STORE( 5) = Z(NZ+1)
      STORE( 9) = Z(NZ+2)
      STORE(10) = Z(NZ+1)
      STORE(11) = Z(NZ  )
      STORE(14) = Z(NZ+2)
      STORE(15) = Z(NZ+1)
      STORE(16) = Z(NZ+2)
      STORE(18) = Z(NZ  )
C
      CALL GMMATD (STORE(19),3,6,0,STORE(1),6,3,0,STORE(37))
      IJKL = 0
      DO 1225 IJK = 1,3
      DO 1225 IJL = 1,3
      IJKL = IJKL + 1
      PART(IJK,IJL) = STORE(IJKL+36)
 1225 CONTINUE
      GO TO 1228
 1226 PART(1,1) = E1*Z(NZ)*Z(MZ) + E3*(Z(NZ+1)*Z(MZ+1) +Z(NZ+2)*Z(MZ+2))
      PART(2,2) = E1*Z(NZ+1)*Z(MZ+1) + E3*(Z(NZ)*Z(MZ) +Z(NZ+2)*Z(MZ+2))
      PART(3,3) = E1*Z(NZ+2)*Z(MZ+2) + E3*(Z(NZ)*Z(MZ) +Z(NZ+1)*Z(MZ+1))
      PART(2,1) = E2*Z(NZ  )*Z(MZ+1) + E3*Z(NZ+1)*Z(MZ  )
      PART(3,1) = E2*Z(NZ  )*Z(MZ+2) + E3*Z(NZ+2)*Z(MZ  )
      PART(1,2) = E2*Z(NZ+1)*Z(MZ  ) + E3*Z(NZ  )*Z(MZ+1)
      PART(3,2) = E2*Z(NZ+1)*Z(MZ+2) + E3*Z(NZ+2)*Z(MZ+1)
      PART(1,3) = E2*Z(NZ+2)*Z(MZ  ) + E3*Z(NZ  )*Z(MZ+2)
      PART(2,3) = E2*Z(NZ+2)*Z(MZ+1) + E3*Z(NZ+1)*Z(MZ+2)
C
C     ADD STIFFNESS PARTITION TO ELEMENT MATRIX
C
C     COMPUTE INDEX INTO OPEN CORE WHERE PART(1,1) IS TO BE ADDED.
C
 1228 IF (SIL(M)-SIL(N)) 1230,1240,1250
 1230 MZ = SIL(N)
      NZ = SIL(M)
      DIAG = .FALSE.
      GO TO 1260
 1240 MZ = SIL(M)
      NZ = SIL(N)
      DIAG = .TRUE.
      GO TO 1260
 1250 MZ = SIL(M)
      NZ = SIL(N)
      DIAG = .FALSE.
C
C     COLUMN NUMBER
C
 1260 L = (NZ-1)*3 + 1
C
C     INCREMENT BETWEEN COLUMNS
C
      INC = NGG - L
C
C     FIRST WORD OF COLUMN
C
      L = IK + ((L-1)*L)/2 + (INC+1)*(L-1)
C
C     WORD IN COLUMN FOR THIS ROW
C
      L = L + 3*(MZ-NZ)
C
C     ADD PARTITION
C
      DO 1280 NZ = 1,3
      DO 1270 MZ = 1,3
      IF (DIAG .AND. MZ.LT.NZ) GO TO 1270
      Z(L+MZ-1) = Z(L+MZ-1) + PART(MZ,NZ)*SFACT
 1270 CONTINUE
      L = L + INC
      INC = INC - 1
 1280 CONTINUE
 1300 IF (MGG .LE. 0) GO TO 1400
C
C     MASS
C
C     COMPUTE TERM FOR MASS MATRIX
C
      RHO = BUFM6(37)
      MZ  = SIL(M)
      NZ  = SIL(N)
      IF (MZ .GE. NZ) GO TO 1310
      MZ  = SIL(N)
      NZ  = SIL(M)
C
C     COMPUTE INDEX INTO OPEN CORE FOR THIS MASS TERM
C
 1310 L = (NZ*(NZ+1))/2 + (NZ-1)*(NGP-NZ) + MZ - NZ + IM - 1
C
C     COMPUTE AND ADD MASS TERM TO ELEMENT MATRIX
C
      Z(L) = Z(L) + DBLE(RHO)*SFACT*Z(IN+M-1)*Z(IN+N-1)
 1400 CONTINUE
 2000 CONTINUE
C
C     END OF INTEGRATION LOOPS
C
      ICODE = 7
C
C     LOOK FOR NON-BASIC COORDINATE SYSTEM
C
      NOCSTM = .FALSE.
      DO 2003 I = 1,NGP
      IF (IEST(BGPDT+I-1) .NE. 0) GO TO 2005
 2003 CONTINUE
      NOCSTM = .TRUE.
      GO TO 2065
C
C     RESTORE GRID POINT DATA TO ORIGINAL FORM FOR DOING TRANSFORM
C     TO GLOBAL COORDINATES
C
C     FIRST, TRANSFER IT TO OPEN CORE AT IN
C
 2005 K = (IN-1)*2 + 1
      J = NGP*4
      DO 2010 I = 1,J
 2010 RZ(K+I-1) = EST(BGPDT+I-1)
C
C     NOW MOVE IT BACK AND REARRANGE IT
C
      DO 2020 I = 1,NGP
      IEST(BGPDT+4*I-4) = JZ(K+I-1)
      DO 2020 J = 1,3
      EST(BGPDT+4*I-4+J) = RZ(K+NGP+3*I+J-4)
 2020 CONTINUE
C
C     FETCH GLOBAL TO BASIC TRANSFORMATION MATRICES
C
      DO 2025 I = 1,NGP
      J = IN + (I-1)*9
      CALL TRANSD (EST(BGPDT+4*I-4),Z(J))
 2025 CONTINUE
      IF (KGG .LE. 0) GO TO 2110
C
C     TRANSFORM STIFFNESS TO GLOBAL COORDINATES
C
      I = 0
 2026 I = I + 1
      ICP = SIL(I)
C
C     COLUMN INDICES
C
      K   = (ICP-1)*3 + 1
      INC = NGG - K + 1
      L   = IK + ((K-1)*K)/2 + INC*(K-1)
      M   = L + INC
      N   = M + INC - 1
C
C     TRANSFORMATION MATRIX INDEX
C
      IGCS = IEST(BGPDT+4*I-4)
      NZ = IN + (I-1)*9
      IF (IGCS .EQ. 0) GO TO 2028
C
C     TERMS ON DIAGONAL PARTITION
C
      ASSIGN 2028 TO BACK
      GO TO 6000
C
C     OFF-DIAGONAL PARTITIONS
C
 2028 L = L + 3
      M = M + 2
      N = N + 1
      IRP = ICP + 1
      IF (IRP .GT. NGP) GO TO 2060
      MZ = NZ
      DO 2050 J = IRP,NGP
      DO 2029 K = 1,NGP
      IF (J .EQ. SIL(K)) GO TO 2031
 2029 CONTINUE
 2031 IF (IGCS .NE. 0) GO TO 2032
      IF (IEST(BGPDT+4*K-4) .EQ. 0) GO TO 2045
 2032 NZ = IN + (K-1)*9
      DO 2030 K = 1,3
      TK(K,1) = 0.0
      TK(K,2) = 0.0
      TK(K,3) = 0.0
      DO 2030 II = 1,3
      TK(K,1) = TK(K,1) + Z(L+II-1)*Z(NZ+3*II+K-4)
      TK(K,2) = TK(K,2) + Z(M+II-1)*Z(NZ+3*II+K-4)
      TK(K,3) = TK(K,3) + Z(N+II-1)*Z(NZ+3*II+K-4)
 2030 CONTINUE
      DO 2040 K = 1,3
      Z(L+K-1) = 0.0
      Z(M+K-1) = 0.0
      Z(N+K-1) = 0.0
      DO 2040 II = 1,3
      Z(L+K-1) = Z(L+K-1) + TK(K,II)*Z(MZ+3*II-3)
      Z(M+K-1) = Z(M+K-1) + TK(K,II)*Z(MZ+3*II-2)
      Z(N+K-1) = Z(N+K-1) + TK(K,II)*Z(MZ+3*II-1)
 2040 CONTINUE
 2045 L = L + 3
      M = M + 3
      N = N + 3
 2050 CONTINUE
 2060 IF (I .LT. NGP) GO TO 2026
C
C     BUILD STIFFNESS PARTITIONS AND PASS TO EMGOUT
C
 2065 IDON = 0
      DO 2100 I = 1,NGP
      IF (I .EQ. NGP) IDON = 1
      DO 2090 J = 1,3
C
C     COLUMN NUMBER
C
      K = (I-1)*3 + J
C
C     NUMBER OF TERMS TO FETCH TO COMPLETE THIS COLUMN IN PARTITION
C
      L = K - 1
      IF (L .EQ. 0) GO TO 2075
C
C     FETCH TERMS AND LOAD INTO J-TH COLUMN OF PARTITION
C
      N   = IK + L
      INC = NGG - 1
      DO 2070 M = 1,L
      Z(IZ+NGG*J-NGG+M-1) = Z(N)
      N   = N + INC
      INC = INC - 1
 2070 CONTINUE
C
C     FILL OUT PARTITION WITH COLUMNS OF STIFFNESS MATRIX
C
C     COMPUTE INDEX IN OPEN CORE OF FIRST TERM OF COLUMN K
C
 2075 N = IK + ((K-1)*K)/2 + (NGG-K+1)*(K-1)
C
C     INSERT THIS COLUMN IN PARTITION
C
      DO 2080 M = K,NGG
      Z(IZ+NGG*J-NGG+M-1) = Z(N)
      N = N + 1
 2080 CONTINUE
 2090 CONTINUE
      DICT(5) = IB(45)
      CALL EMGOUT (Z(IZ),Z(IZ),3*NGG,IDON,DICT,1,2)
 2100 CONTINUE
C
C     EXPAND AND TRANSFORM MASS MATRIX AND PASS TO EMGOUT
C
      IF (MGG .LE. 0) GO TO 2400
 2110 IDON = 0
      DO 2140 I = 1,NGP
      IF (I .EQ. NGP) IDON = 1
      DO 2130 J = 1,NGP
C
C     COMPUTE INDEX INTO OPEN CORE FOR MASS TERM
C
      K = I
      L = J
      IF (I .LE. J) GO TO 2115
      K = J
      L = I
 2115 N = ((K-1)*K)/2 + (K-1)*(NGP-K+1) + L - K + IM
C
C     MULTIPLY GLOBAL TO BASIC TRANSFORMATIONS
C
      M = IZ - NGG+3*J - 4
      IF (I.EQ.J .OR. NOCSTM) GO TO 2116
      IF (IEST(BGPDT+4*I-4) .NE. 0) GO TO 2118
      IF (IEST(BGPDT+4*J-4) .NE. 0) GO TO 2118
 2116 Z(M+NGG  +1) = Z(N)
      Z(M+NGG  +2) = 0.0
      Z(M+NGG  +3) = 0.0
      Z(M+NGG*2+1) = 0.0
      Z(M+NGG*2+2) = Z(N)
      Z(M+NGG*2+3) = 0.0
      Z(M+NGG*3+1) = 0.0
      Z(M+NGG*3+2) = 0.0
      Z(M+NGG*3+3) = Z(N)
      GO TO 2130
 2118 DO 2119 K = 1,NGP
      IF (I .EQ. SIL(K)) MZ = IN + 9*(K-1)
      IF (J .EQ. SIL(K)) NZ = IN + 9*(K-1)
 2119 CONTINUE
      CALL GMMATD (Z(MZ),3,3,1,Z(NZ),3,3,0,TF)
C
C     MULTIPLY BY MASS SCALAR FOR THIS 3 BY 3 PARTITION AND STORE
C     IN NGG BY 3 PARTITION
C
      DO 2120 K = 1,3
      DO 2120 L = 1,3
      Z(M+NGG*L+K) = TF(K,L)*Z(N)
 2120 CONTINUE
 2130 CONTINUE
      DICT(5) = 0
      CALL EMGOUT (Z(IZ),Z(IZ),3*NGG,IDON,DICT,2,2)
 2140 CONTINUE
C
C     SAVE ELEMENT BCD NAME, ID, VOLUME, MASS, NO. OF GRID POINTS, AND
C     GRID POINT DATA IN SCR4 IF USER REQUESTED VOLUME/AREA PRINTOUT
C     (NOTE - MAKE SURE THE GRID POINT DATA, BGPDT, IS IN ISTS ORIGIANL
C      FORM)
C
 2400 IF (VOLUME.LE.0.0 .AND. SURFAC.LE.0.0) GO TO 5000
      IL = IZ*2
      RZ(IL+1) = BCD1
      RZ(IL+2) = BCD2(TYPE)
      JZ(IL+3) = EID
      RZ(IL+4) = TVOL*VOLUME
      RZ(IL+5) = TVOL
      IF (RHO .GT. 0.0) RZ(IL+5) = TVOL*RHO
      JZ(IL+6) = NGP
      K = IL + 6
      DO 2410 I = 1,NGP
      K = K + 1
 2410 RZ(K) = EST(1+I)
      IF (SURFAC .LE. 0.0) GO TO 2460
      IF (.NOT.NOCSTM) GO TO 2440
      L = BGPDT + NGP
      DO 2430 I = 1,NGP
      K = K + 1
      JZ(K) = IEST(BGPDT+I-1)
      DO 2420 J = 1,3
      K = K + 1
      RZ(K) = EST(L)
 2420 L = L + 1
 2430 CONTINUE
      GO TO 2460
 2440 J = NGP*4
      DO 2450 I = 1,J
      K = K + 1
 2450 RZ(K) = EST(BGPDT+I-1)
 2460 L = K - IL
      CALL WRITE (SCR4,RZ(IL+1),L,1)
      GO TO 5000
C
C     HEAT TRANSFER SECTION
C
 3000 INFLAG = 3
      CALL HMAT (EID)
      ANIS = .FALSE.
      IF (KGG .LE. 0) GO TO 3100
C
C     CHECK FOR ANISOTROPY
C
      IF (KHEAT(1).NE.KHEAT(4) .OR. KHEAT(1).NE.KHEAT(6)) GO TO 3010
      IF (KHEAT(2).NE.0.0 .OR. KHEAT(3).NE.0.0 .OR. KHEAT(5).NE.0.0)
     1    GO TO 3010
      GO TO 3100
 3010 ANIS = .TRUE.
      IT = IZ + 8
C
C     CHECK FOR RECTANGULAR COORDINATE SYSTEM FOR MATERIAL
C
      RECT = .TRUE.
      IF (CID .EQ. 0) GO TO 3100
      JZ(IZS) = CID
      DO 3030 I = 1,3
 3030 RZ(IZS+I) = EST(BCORD+I-1)
      CALL TRANSD (RZ(IZS),Z(IT))
      DO 3040 I = 1,3
 3040 RZ(IZS+I) = -RZ(IZS+I)
      CALL TRANSD (RZ(IZS),Z(IN))
      DO 3050 I = 1,9
      IF (Z(IT+I-1) .NE. Z(IN+I-1)) RECT = .FALSE.
 3050 CONTINUE
C
C     IF NOT DEFINED IN A RECTANGULAR SYSTEM, MUST TRANSFORM INSIDE
C     INTEGRATION LOOPS
C
      IF (.NOT.RECT) GO TO 3100
C
C     TRANSFORM MATERIAL MATRIX TO BASIC SYSTEM
C
      DO 3060 I = 1,6
 3060 Z(IZ+I+1) = DBLE(KHEAT(I))
      L  = IZ + 2
      M  = L + 3
      N  = M + 2
      NZ = IT
      ASSIGN 3100 TO BACK
      GO TO 6000
C
C     ANISOTROPIC CONDUCTIVITY MATERIAL MATRIX NOW STORED AT RZ(IZ+2)
C     TO RZ(IZ+7)
C
C     ALL SET FOR DOING INTEGRATION.  DO IT.
C
 3100 I = 0
 3101 I = I + 1
      J = 0
 3102 J = J + 1
      K = 0
 3103 K = K + 1
C
C     GENERATE SHAPE FUNCTIONS AND JACOBIAN MATRIX INVERSE
C
      CALL IHEXSD (TYPE,Z(IN),Z(IG),JACOB,DETJ,EID,S(I),S(J),S(K),
     1             EST(BCORD))
      IF (DETJ .NE. 0.0) GO TO 3110
C
C     FALL HERE IF JACOBIAN MATRIX WAS SINGULAR
C
      NOGO = .TRUE.
      RETURN
C
 3110 SFACT = H(I)*H(J)*H(K)*DETJ
      IF (KGG .LE. 0) GO TO 3120
C
C     COMPUTE DERIVATIVES OF SHAPE FUNCTION W.R.T. BASIC SYSTEM.
C
C     MUST REVERSE CALLING ORDER SINCE MATRICES ARE STORED BY COLUMNS
C
      CALL GMMATD (Z(IG),NGP,3,0,JACOB,3,3,0,Z(IX))
C
C     IF MATERIAL IS ANISOTROPIC AND NOT DEFINED IN A RECTANGULAR
C     CORDINATE SYSTEM, MUST TRANSFORM TO BASIC SYSTEM AT THIS
C     INTEGRATION POINT
C
 3120 IF (.NOT.ANIS) GO TO 3160
      IF (RECT) GO TO 3160
C
C     COMPUTE BASIC COORDINATES VECTOR AT THIS POINT
C
      DO 3130 L = 1,3
 3130 RZ(IZS+L) = 0.0
      DO 3140 L = 1,NGP
      DO 3140 M = 1,3
      RZ(IZS+M) = RZ(IZS+M) + Z(IN+L-1)*EST(BCORD + 3*L+M-4)
 3140 CONTINUE
C
C     FETCH TRANSFORMATION AND CONDUCTIVITY MATRICES AND PERFORM
C     TRANSFORMATION OPERATIONS
C
      CALL TRANSD (RZ(IZS),Z(IT))
      DO 3150 L = 1,6
 3150 Z(IZ+L+1) = DBLE(KHEAT(L))
      NZ = IT
      L  = IZ + 2
      M  = L + 3
      N  = M + 2
      ASSIGN 3160 TO BACK
      GO TO 6000
C
C     MATERIAL HAS BEEN EVALUATED FOR THIS INTEGRATION POINT WHEN
C     FALL HERE
C
C     NOW BEGIN LOOPS OVER GRID POINTS ALONG ROWS AND COLUMNS
C
 3160 DO 3220 N = 1,NGP
      DO 3220 M = N,NGP
C
C     COMPUTE 1 BY 1 PARTITION FOR ROW M AND COLUMN N
C
      IF (KGG .LE. 0) GO TO 3210
C
C     CONDUCTIVITY
C
      IF (ANIS) GO TO 3180
C
C     ISOTROPIC CASE
C
      PRT1 = 0.0
      DO 3170 L = 1,3
 3170 PRT1 = PRT1 + Z(IX+3*M+L-4)*Z(IX+3*N+L-4)
      PRT1 = SFACT*DBLE(KHEAT(1))*PRT1
      GO TO 3190
C
C     ANISOTROPIC CASE
C
 3180 L  = IX + 3*(M-1)
      E1 = Z(L)*Z(IZ+2) + Z(L+1)*Z(IZ+3) + Z(L+2)*Z(IZ+4)
      E2 = Z(L)*Z(IZ+3) + Z(L+1)*Z(IZ+5) + Z(L+2)*Z(IZ+6)
      E3 = Z(L)*Z(IZ+4) + Z(L+1)*Z(IZ+6) + Z(L+2)*Z(IZ+7)
      L  = IX + 3*(N-1)
      PRT1 = SFACT*(Z(L)*E1 + Z(L+1)*E2 + Z(L+2)*E3)
C
C     COMPUTE INDEX INTO OPEN CORE FOR THIS TERM
C
 3190 L  = SIL(M)
      MZ = SIL(N)
      IF (L .LE. MZ) GO TO 3200
      L  = MZ
      MZ = SIL(M)
 3200 L  = (L-1)*NGG + MZ + IK - 1
C
C     ADD TERM TO MATRIX
C
      Z(L) = Z(L) + PRT1
C
C     CAPACITANCE
C
 3210 IF (MGG .LE. 0) GO TO 3220
C
C     COMPUTE INDEX INTO OPEN CORE FOR THIS TERM
C
      L  = SIL(M)
      MZ = SIL(N)
      IF (L .LE. MZ) GO TO 3215
      L  = MZ
      MZ = SIL(M)
 3215 L  = (L-1)*NGG + MZ + IM - 1
C
C     COMPUTE AND ADD TERM
C
      Z(L) = Z(L) + SFACT*DBLE(CP)*Z(IN+M-1)*Z(IN+N-1)
 3220 CONTINUE
      IF (K .LT. NIP) GO TO 3103
      IF (J .LT. NIP) GO TO 3102
      IF (I .LT. NIP) GO TO 3101
C
C     END OF HEAT TRANSFER INTEGRATION LOOPS
C
      ICODE = 1
C
C     FILL IN THE UPPER TRIANGLES OF THE MATRICES
C
      IF (KGG .LE. 0) GO TO 4010
      MZ = IK
      GO TO 4020
 4010 IF (MGG .LE. 0) GO TO 4040
      MZ = IM
 4020 L  = NGG - 1
      DO 4030 I = 1,L
      J  = I + 1
      DO 4030 K = J,NGG
      M  = (I-1)*NGG + K + MZ - 1
      N  = (K-1)*NGG + I + MZ - 1
      Z(N) = Z(M)
 4030 CONTINUE
      IF (MZ .EQ. IK) GO TO 4010
C
C     PASS MATRICES TO EMGOUT
C
 4040 K = NGG**2
      DICT(5) = 0
      IF (KGG .GT. 0) CALL EMGOUT (Z(IK),Z(IK),K,1,DICT,1,2)
      IF (MGG .GT. 0) CALL EMGOUT (Z(IM),Z(IM),K,1,DICT,3,2)
C
C     ALL DONE, NO ERRORS
C
 5000 RETURN
C
C
C     INTERNAL SUBROUTINE
C
C     TRANSFORM COORDINATE SYSTEM OF SYMMETRIC HALF OF A 3 BY 3 MATRIX
C
 6000 TK(1,1) = Z(NZ  )*Z(L  )  + Z(NZ+3)*Z(L+1)  + Z(NZ+6)*Z(L+2)
      TK(2,1) = Z(NZ+1)*Z(L  )  + Z(NZ+4)*Z(L+1)  + Z(NZ+7)*Z(L+2)
      TK(3,1) = Z(NZ+2)*Z(L  )  + Z(NZ+5)*Z(L+1)  + Z(NZ+8)*Z(L+2)
      TK(1,2) = Z(NZ  )*Z(L+1)  + Z(NZ+3)*Z(M  )  + Z(NZ+6)*Z(M+1)
      TK(2,2) = Z(NZ+1)*Z(L+1)  + Z(NZ+4)*Z(M  )  + Z(NZ+7)*Z(M+1)
      TK(3,2) = Z(NZ+2)*Z(L+1)  + Z(NZ+5)*Z(M  )  + Z(NZ+8)*Z(M+1)
      TK(1,3) = Z(NZ  )*Z(L+2)  + Z(NZ+3)*Z(M+1)  + Z(NZ+6)*Z(N  )
      TK(2,3) = Z(NZ+1)*Z(L+2)  + Z(NZ+4)*Z(M+1)  + Z(NZ+7)*Z(N  )
      TK(3,3) = Z(NZ+2)*Z(L+2)  + Z(NZ+5)*Z(M+1)  + Z(NZ+8)*Z(N  )
      Z(L  )  = Z(NZ  )*TK(1,1) + Z(NZ+3)*TK(1,2) + Z(NZ+6)*TK(1,3)
      Z(L+1)  = Z(NZ  )*TK(2,1) + Z(NZ+3)*TK(2,2) + Z(NZ+6)*TK(2,3)
      Z(L+2)  = Z(NZ  )*TK(3,1) + Z(NZ+3)*TK(3,2) + Z(NZ+6)*TK(3,3)
      Z(M  )  = Z(NZ+1)*TK(2,1) + Z(NZ+4)*TK(2,2) + Z(NZ+7)*TK(2,3)
      Z(M+1)  = Z(NZ+1)*TK(3,1) + Z(NZ+4)*TK(3,2) + Z(NZ+7)*TK(3,3)
      Z(N  )  = Z(NZ+2)*TK(3,1) + Z(NZ+5)*TK(3,2) + Z(NZ+8)*TK(3,3)
      GO TO BACK, (2028,3100,3160)
C
 7100 FORMAT (A23,I5,2H, ,A4,I1,3A4,I9,' INSUFFICIENT CORE TO COMPUTE',
     1       ' ELEMENT MATRIX')
 7200 FORMAT (A23,I5,2H, ,A4,I1,3A4,I9,3X,18HILLEGAL GEOMETRY, ,9A4)
 7300 FORMAT (A23,' 4005. AN ILLEGAL VALUE OF -NU- HAS BEEN SPECIFIED ',
     1       'UNDER MATERIAL ID =',I10,17H FOR ELEMENT ID =,I10)
C
      END
