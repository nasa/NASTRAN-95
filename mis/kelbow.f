      SUBROUTINE KELBOW
C
C     THIS ROUTINE COMPUTES THE TWO 6 X 6 MATRICES K(NPVT,NPVT) AND
C     K(NPVT,J) FOR A CURVED BAR ELEMENT HAVING END POINTS NUMBERED
C     NPVT AND J
C
C                   ECPT FOR THE ELBOW
C
C     ECPT( 1)  -  IELID         ELEMENT ID. NUMBER
C     ECPT( 2)  -  ISILNO(2)     * SCALAR INDEX NOS. OF THE GRID POINTS
C     ECPT( 3)  -    ...         *
C     ECPT( 4)  -  SMALLV(3)     $ REFERENCE VECTOR
C     ECPT( 5)  -    ...         $
C     ECPT( 6)  -    ...         $
C     ECPT( 7)  -  ICSSV         COOR. SYS. ID FOR SMALLV VECTOR
C     ECPT( 8)  -  IMATID        MATERIAL ID.
C     ECPT( 9)  -  A             CROSS-SECTIONAL AREA
C     ECPT(10)  -  I1            $ AREA MOMENTS OF INERTIA
C     ECPT(11)  -  I2            $
C     ECPT(12)  -  FJ            TORSIONAL CONSTANT
C     ECPT(13)  -  NSM           NON-STRUCTURAL MASS
C     ECPT(14)  -  FE            FORCE ELEM. DESCRIPTIONS, FORCE METHOD
C     ECPT(15)  -  R1            *STRESS RECOVERY COEFFICIENTS
C     ECPT(16)  -  T1            *  RI = RADIAL  LOCATION
C     ECPT(17)  -  R2            *  TI = ANGULAR LOCATION
C     ECPT(18)  -  T2            *       OF STRESS RECOVERY POINTS
C     ECPT(19)  -  R3            *
C     ECPT(20)  -  T3            *
C     ECPT(21)  -  R4            *
C     ECPT(22)  -  T4            *
C     ECPT(23)  -  K1            $  AREA FACTOR FOR SHEAR
C     ECPT(24)  -  K2            $
C     ECPT(25)  -  C             STRESS INTENSIFICATION FACTOR
C     ECPT(26)  -  KX            *  FLEXIBILITY CORRECTION FACTORS
C     ECPT(27)  -  KY            *
C     ECPT(28)  -  KZ            *
C     ECPT(29)  -  R             RADIUS OF CURVATURE
C     ECPT(30)  -  BETAR         ANGLE FROM GA TO GB
C     ECPT(31)  -  MCSIDA        COORD. SYS. ID. FOR GRID POINT A
C     ECPT(32)  -  GPA(3)        *  BASIC COORD. FOR GRID POINT A
C     ECPT(33)  -   ...          *
C     ECPT(34)  -   ...          *
C     ECPT(35)  -  MCSIDB        COORD. SYS. ID. FOR GRID POINT B
C     ECPT(36)  -  GPB(3)        *  BASIC COORD. FOR GRID POINT B
C     ECPT(37)  -   ...          *
C     ECPT(38)  -   ...          *
C     ECPT(39)  -  ELTEMP        AVG. ELEMENT TEMPERATURE
C
C     COMMENTS FROM G.CHAN/UNISYS   7/91
C     ABOUT K1 AND K2, THE AREA FACTORS FOR SHEAR
C
C     THE K1,K2 FOR BAR ARE 0. TO 1.0, AND ARE USED IN K1*G*A AND K2*G*A
C         THE K1,K2 ARE THEREFORE CORRECTION FACTORS FOR STIFFNESS
C     THE K1,K2 ARE USED IN ELBOW IN K1/G*A AND K2/G*A. AND THEREFORE
C         THE K1,K2 ARE COORECTION FACTORS FOR FLEXIBILITY. THE K1,K2
C         IN ELBOW ARE EQUIVALENT TO 1./K1 AND 1./K2 IN BAR ELEMENT.
C         THE PROPER VALUE FOR K1 AND K2 SHOULD BE INFINITY TO 1.0
C
C     IN 1992 COSMIC/NASTRAN, THE USE OF K1 AND K2 IN ELBOW AND BAR
C     ELMENTS ARE SYMCHRONIZED, WITH PROPER VALUES FROM 0. TO 1.0
C     THE K1 AND K2 ARE CHANGED TO 1./K1 AND 1./K2 IN ELBOW ELEMENT
C     SHEAR COMPUTATION. THAT IS, CORRECTION FACTORS FOR STIFFNESS IS
C     USED.
C
C     REFERENCE -  R.J. ROARK: FORMULAS FOR STRESS AND STRAIN,
C     SECTION 35, 'BEAMS FOR RELATIVELY GREAT DEPTH',
C     FOR BEAMS OF SAMLL SPAN/DEPTH RATIO
C
C     K = 1/F = 5/6 FOR RECTANGULAR SECTION
C             = 0.9 FOR SOLID CIRCULAR
C             = 0.5 FOR THIN-WALLED HOOLOW CIRCULAR SECTION
C             = 1.0 CAN BE USED FOR I-BEAM
C
C
C
      LOGICAL          HEAT,ABASIC,BBASIC,BASIC
      REAL             K1,K2,I1,I2,NSM,KX,KY,KZ
      DOUBLE PRECISION TA(18),TB(9),SMALV0(6),DELA,DELB,KE,KEP,VECI,
     1                 VECJ,VECK,FL,FLL,DF(6,6),DETERM,H(6,6),DP(16),
     2                 S(12,12),DAMPC,KEE(12,12)
      DIMENSION        VECI(3),VECJ(3),VECK(3),ECPT(100),IECPT(100),
     1                 IZ(1),IWORK(6,3),F(6,6)
      COMMON /SMA1IO/  IFCSTM,IFMPT,IFDIT,IDUM1,IFECPT,IGECPT,IFGPCT,
     1                 IGGPCT,IFGEI,IGGEI,IFKGG,IGKGG,IF4GG,IG4GG,
     2                 IFGPST,IGGPST,INRW,OUTRW,CLSNRW,CLSRW,NEOR,
     3                 EOR,MCBKGG(7),MCB4GG(7)
      COMMON /ZZZZZZ/  Z(1)
      COMMON /SMA1BK/  ICSTM,NCSTM,IGPCT,NGPCT,IPOINT,NPOINT,I6X6K,
     1                 N6X6K,I6X64,N6X64
      COMMON /SMA1CL/  IOPT4,K4GGSW,NPVT,LEFT,FROWIC,LROWIC,NROWSC,
     1                 TNROWS,JMAX,NLINKS,LINK(10),IDETCK,DODET,NOGO
      COMMON /SMA1HT/  HEAT
      COMMON /SMA1ET/  IELID,ISILNO(2),SMALLV(3),ICSSV,IMATID,A,I1,I2,
     1                 FJ,NSM,FE,C1,C2,D1,D2,F1,F2,G1,G2,K1,K2,C,KX,KY,
     2                 KZ,R,BETAR,MCSIDA,GPA(3),MCSIDB,GPB(3),TEMPEL
      COMMON /SMA1DP/  KE(144),KEP(144),DELA(6),DELB(6)
      COMMON /MATIN /  MATIDC,MATFLG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/  E,G,NU,RHO,ALPHA,TSUBO,GSUBE,SIGT,SIGC,SIGS
      COMMON /HMTOUT/  FK
      COMMON /SYSTEM/  SYSBUF,NOUT
      EQUIVALENCE      (IELID,ECPT(1),IECPT(1)),(IZ(1),Z(1)),
     1                 (TA(10),TB(1)),(ECPT(71),DP(1)),
     2                 (KEE(1,1),KE(1),S(1,1))
      DATA    DCR   /  .017453292 /
C
      SID(X) = SIN(X*DCR)
      COD(X) = COS(X*DCR)
      DTR(X) = X*DCR
C
C     DETERMINE WHICH POINT IS THE PIVOT POINT.
C
      X    = 1.
      IPVT = 1
      IF (ISILNO(1) .EQ. NPVT) GO TO 20
      IPVT = 2
      IF (ISILNO(2) .NE. NPVT) CALL MESAGE (-30,34,IECPT(1))
C
C     SET UP POINTERS TO COORD. SYSTEM IDS
C
   20 JCSIDA = 31
      JCSIDB = 35
      ICSIDA = IECPT(31)
      ICSIDB = IECPT(35)
C
C     DEFINE LOCATION OF END A, END B IN TERMS OF DP(1) THRU DP(6)
C
      DP(1) = ECPT(JCSIDA+1)
      DP(2) = ECPT(JCSIDA+2)
      DP(3) = ECPT(JCSIDA+3)
      DP(4) = ECPT(JCSIDB+1)
      DP(5) = ECPT(JCSIDB+2)
      DP(6) = ECPT(JCSIDB+3)
C
C     DEFINE COMPONENTS OF VECTOR FROM END A TO CENTER OF CURVATURE,C
C
      DP(7) = ECPT(4)
      DP(8) = ECPT(5)
      DP(9) = ECPT(6)
      FLD   = DSQRT(DP(7)**2 + DP(8)**2 + DP(9)**2)
      IF (FLD .LE. 0.000) GO TO 1010
      DP(7) = DP(7)/FLD
      DP(8) = DP(8)/FLD
      DP(9) = DP(9)/FLD
C
C     DETERMINE IF POINT A AND B ARE IN BASIC COORDINATES
C
      ABASIC =.TRUE.
      BBASIC =.TRUE.
      IF (ICSIDA .NE. 0) ABASIC =.FALSE.
      IF (ICSIDB .NE. 0) BBASIC =.FALSE.
C
C     COMPUTE THE TRANSFORMATION MATRICES TA AND TB IF NECESSARY
C
      IF (ABASIC) GO TO 30
      CALL TRANSD (ECPT(JCSIDA),TA)
      CALL GMMATD (TA,3,3,0, DP(7),3,1,0, VECJ)
      CALL GMMATD (TA,3,3,0, DP(1),3,1,0, DP(14))
      DP(1) = DP(14)
      DP(2) = DP(15)
      DP(3) = DP(16)
      GO TO 35
   30 CONTINUE
      VECJ(1) = DP(7)
      VECJ(2) = DP(8)
      VECJ(3) = DP(9)
   35 IF (BBASIC) GO TO 40
      CALL TRANSD (ECPT(JCSIDB),TB)
      CALL GMMATD (TB,3,3,0, DP(4),3,1,0, DP(14))
      DP(4) = DP(14)
      DP(5) = DP(15)
      DP(6) = DP(16)
   40 CONTINUE
C
C     CALCULATE TRUE LENGTH OF ELBOW
C
      FL = DBLE(R*DTR(BETAR))
      IF (FL .EQ. 0.0D0) GO TO 1010
C
C     NOW THAT LENGTH HAS BEEN COMPUTED, BRANCH IF THIS IS A -HEAT-
C     FORMULATION.
C
      IF (HEAT) GO TO 2000
C
C     CONSTRUCT VECTOR FROM A TO B
C
      SMALV0(1) = DP(4) - DP(1)
      SMALV0(2) = DP(5) - DP(2)
      SMALV0(3) = DP(6) - DP(3)
      FLL = DSQRT(SMALV0(1)**2 + SMALV0(2)**2 + SMALV0(3)**2)
      IF (FLL .EQ. 0.0D0) GO TO 1010
      SMALV0(1) = SMALV0(1)/FLL
      SMALV0(2) = SMALV0(2)/FLL
      SMALV0(3) = SMALV0(3)/FLL
C
C     COMPUTE THE K VECTOR VECK = SMALV0 X VECJ
C
      VECK(1) = SMALV0(2)*VECJ(3) - SMALV0(3)*VECJ(2)
      VECK(2) = SMALV0(3)*VECJ(1) - SMALV0(1)*VECJ(3)
      VECK(3) = SMALV0(1)*VECJ(2) - SMALV0(2)*VECJ(1)
      FLL = DSQRT(VECK(1)**2 + VECK(2)**2 + VECK(3)**2)
      IF (FLL .EQ. 0.0D0) GOTO 1010
      VECK(1) = VECK(1)/FLL
      VECK(2) = VECK(2)/FLL
      VECK(3) = VECK(3)/FLL
C
C     COMPUTE THE I VECTOR  VECI = VECJ X VECK
C
      VECI(1) = VECJ(2)*VECK(3) - VECJ(3)*VECK(2)
      VECI(2) = VECJ(3)*VECK(1) - VECJ(1)*VECK(3)
      VECI(3) = VECJ(1)*VECK(2) - VECJ(2)*VECK(1)
      FLL = DSQRT(VECI(1)**2 + VECI(2)**2 + VECI(3)**2)
      IF (FLL .EQ. 0.0D0) GO TO 1010
      VECI(1) = VECI(1)/FLL
      VECI(2) = VECI(2)/FLL
      VECI(3) = VECI(3)/FLL
C
C     SEARCH THE MATERIAL PROPERTIES TABLE FOR E,G AND THE DAMPING
C     CONSTANT.
C
      MATIDC = IMATID
      MATFLG = 1
      ELTEMP = TEMPEL
      CALL MAT (IECPT(1))
      DAMPC = G SUB E
C
C     SET UP INTERMEDIATE VARIABLES FOR ELEMENT STIFFNESS MATRIX
C     CALCULATION
C
      IF (KX .LT. 1.0E-8) KX = 1.0
      IF (KY .LT. 1.0E-8) KY = 1.0
      IF (KZ .LT. 1.0E-8) KZ = 1.0
      FI1 = I1/KZ
      FI2 = I2/KY
      FJK = FJ/KX
C
C     AREA FACTORS FOR SHEAR ARE FROM NEAR ZERO TO ONE
C
      IF (K1 .LT. 1.0E-8) K1 = 1.0
      IF (K2 .LT. 1.0E-8) K2 = 1.0
      IF (K1 .GT. 1.0) K1 = 1.0/K1
      IF (K2 .GT. 1.0) K2 = 1.0/K2
C
C     THE FOLLOWING CODE WAS TAKEN FROM SAP4 BENDKS ROUTINE
C     FOR A CURVED PIPE ELEMENT
C
C     COMPUTE SECTION PROPERTY CONSTANTS
C
      T   = DTR(BETAR)
      RA  = R/(A*E)
      RV1 = R/(2.*K1*G*A)
      RV2 = K1/K2*RV1
      RT  = R/(G*FJK*2.)
      RB0 = R/(E*FI2*2.)
      RB1 = R/(E*FI1)
      R2  = R**2
C
C     COMPUTE COMMON TRIGONOMETRIC CONSTANTS
C
      ST  = SID(BETAR)
      CT  = COD(BETAR)
      S2T = SID(2.0*BETAR)
      C2T = COD(2.0*BETAR)
C
C     FORM THE NODE FLEXIBILITY MATRIX AT NODE J REFERENCED TO THE
C     LOCAL (X,Y,Z) COORDINATE SYSTEM AT NODE I.
C
C     X - DIRECTION...  IN-PLANE TANGENT TO THE BEND AT NODE I AND
C                       DIRECTED TOWARD NODE J
C     Y - DIRECTION...  IN-PLANE AND DIRECTED RADIALLY INWARD TO THE
C                       CENTER OF CURVATURE
C     Z - DIRECTION...  OUT OF PLANE AND ORTHOGONAL TO X AND Y
C
      DO 50 I = 1,6
      DO 50 K = I,6
      F(I,K)  = 0.0
   50 CONTINUE
C
C     A X I A L
C
      F(1,1) = F(1,1) + 0.25*RA*(2.0*T+S2T)
      F(2,2) = F(2,2) + 0.25*RA*(2.0*T-S2T)
C
C     N O T E   (COEFFICIENT CHANGE)
C
      F(1,2) = F(1,2) + 0.50*RA*ST**2
C
C     S H E A R
C
      F(1,1) = F(1,1) + 0.5*RV1*(2.0*T-S2T)
      F(2,2) = F(2,2) + 0.5*RV1*(2.0*T+S2T)
      F(3,3) = F(3,3) + 2.0*RV2* T
C
C     N O T E   (SIGN CHANGE)
C
      F(1,2) = F(1,2) - RV1*ST**2
C
C     T O R S I O N
C
      F(3,3) = F(3,3) + 0.5*RT*R2*(6.0*T+S2T-8.0*ST)
      F(4,4) = F(4,4) + 0.5*RT*   (2.0*T+S2T)
      F(5,5) = F(5,5) + 0.5*RT*   (2.0*T-S2T)
      F(3,4) = F(3,4) +     RT*R *(ST-T*CT)
      F(3,5) = F(3,5) +     RT*R *(2.0-2.0*CT-T*ST)
      F(4,5) = F(4,5) + 0.5*RT*   (1.0-C2T)
C
C     B E N D I N G
C
      F(1,1) = F(1,1) + 0.25*RB1*R2*(2.0*T*(2.0+C2T)-3.0*S2T)
      F(2,2) = F(2,2) + 0.25*RB1*R2*(2.0*T*(2.0-C2T)+3.0*S2T-8.0*ST)
      F(3,3) = F(3,3) + 0.50*RB0*R2*(2.0*T-S2T)
      F(4,4) = F(4,4) + 0.50*RB0*   (2.0*T-S2T)
      F(5,5) = F(5,5) + 0.50*RB0*   (2.0*T+S2T)
      F(6,6) = F(6,6) +      RB1*T
      F(1,2) = F(1,2) + 0.25*RB1*R2*(1.0+3.0*C2T+2.0*T*S2T-4.0*CT)
      F(1,6) = F(1,6) -      RB1*R *(ST-T*CT)
      F(2,6) = F(2,6) +      RB1*R *(T*ST+CT-1.0)
      F(3,4) = F(3,4) +      RB0*R *(ST-T*CT)
      F(3,5) = F(3,5) -      RB0*R *T*ST
      F(4,5) = F(4,5) - 0.50*RB0*   (1.0-C2T)
C
C
C     FORM SYMMETRICAL UPPER PART OF FLEX MATRIX
C
      DO 65 I = 1,6
      DO 65 K = I,6
      DF(K,I) = DBLE(F(I,K))
      DF(I,K) = DF(K,I)
   65 CONTINUE
C
C     WRITE (6,4005) DF
C
C     INVERT FLEX TO FORM STIFFNESS
C
      CALL INVERD (6,DF,6,DUM,0,DETERM,ISING,IWORK)
      IF (ISING .EQ. 2) WRITE (6,4002) F
      IF (ISING .EQ. 2) CALL MESAGE (-30,38,ECPT(1))
 4002 FORMAT (1X,34HELBOW STIFFNESS MATRIX IS SINGULAR, /,(5X,6E13.5))
C
C
C     SET UP THE FORCE TRANSFORMATION RELATING REACTIONS AT NODE I
C     ACTING ON THE MEMBER END DUE TO UNIT LOADS APPLIED TO THE MEMBER
C     END AT NODE J.
C
      DO 100 I = 1,6
      DO 100 K = 1,6
      H(I,K) = 0.0D0
  100 CONTINUE
C
      DO 105 K = 1,6
      H(K,K) =-1.0D0
  105 CONTINUE
C
      H(4,3) =-DBLE(R*(1.0-CT))
      H(5,3) = DBLE(R*ST)
      H(6,1) =-H(4,3)
      H(6,2) =-H(5,3)
C
C     FORM THE UPPER TRIANGULAR PORTION OF THE LOCAL ELEMENT STIFFNESS
C     MATRIX FOR THE BEND
C
      DO 110 K = 1,6
      DO 110 I = K,6
      S(K+6,I+6) = DF(K,I)
  110 CONTINUE
C
      DO 130 IR = 1,6
      DO 130 IC = 1,6
      S(IR,IC+6) = 0.0D0
      DO 120 IN = 1,6
      S(IR,IC+6) = S(IR,IC+6) + H(IR,IN)*DF(IN,IC)
  120 CONTINUE
  130 CONTINUE
C
      DO 150 IR = 1,6
      DO 150 IC = IR,6
      S(IR,IC)  = 0.0D0
      DO 140 IN = 1,6
      S(IR,IC) = S(IR,IC) + S(IR,IN+6)*H(IC,IN)
  140 CONTINUE
  150 CONTINUE
C
C     REFLECT FOR SYMMETRY
C
      DO 165 I = 1,12
      DO 165 K = I,12
      S(K,I)   = S(I,K)
  165 CONTINUE
C
      J = 0
      IF (IPVT .EQ. 2) GO TO 327
      ILOW = 1
      ILIM = 72
      GO TO 329
  327 ILOW = 73
      ILIM = 144
  329 DO 340 I = ILOW,ILIM,12
      LOW = I
      LIM = LOW + 5
      DO 330 K = LOW,LIM
      J = J + 1
      KEP(J) = KE(K)
  330 KEP(J+36) = KE(K+6)
  340 CONTINUE
C
C                                                            T
C     STORE VECI, VECJ, VECK IN KE(1),...,KE(9) FORMING THE A  MATRIX.
C
      KE(1) = VECI(1)
      KE(2) = VECI(2)
      KE(3) = VECI(3)
      KE(4) = VECJ(1)
      KE(5) = VECJ(2)
      KE(6) = VECJ(3)
      KE(7) = VECK(1)
      KE(8) = VECK(2)
      KE(9) = VECK(3)
C
C     ZERO OUT THE ARRAY WHERE THE 3 X 3 MATRIX H AND THE W  AND W
C     6 X 6  MATRICES WILL RESIDE.                         A      B
C
      DO 350 I = 28,108
  350 KE(I) = 0.0D0
      IPASS = 1
      IWBEG = 0
C
C     SET UP POINTERS
C
      IF (IPVT-1) 365,360,365
  360 BASIC  = ABASIC
      JCSID  = JCSIDA
      IKEL   = 1
      INDEX  = ISILNO(1)
      GO TO 368
  365 BASIC  = BBASIC
      JCSID  = JCSIDB
      IKEL   = 37
      INDEX  = ISILNO(2)
C
C     SET UP THE -G- MATRIX.  IG POINTS TO THE BEGINNING OF THE G
C     MATRIX. G = AT X TI
C
  368 IG = 1
      IF (BASIC) GO TO 380
      CALL TRANSD (ECPT(JCSID),KE(10))
      CALL GMMATD (KE(1),3,3,0, KE(10),3,3,0, KE(19))
      IG = 19
C
C     FORM THE W  MATRIX OR THE W  MATRIX IN KE(37) OR KE(73) DEPENDING
C               A                B
C     UPON WHICH POINT - A OR B - IS UNDER CONSIDERATION.  G WILL BE
C     STORED IN THE UPPER LEFT AND LOWER RIGHT CORNERS.  H, IF NON-ZERO,
C     WILL BE STORED IN THE UPPER RIGHT CORNER.
C
  380 KE(IWBEG+37) = KE(IG  )
      KE(IWBEG+38) = KE(IG+1)
      KE(IWBEG+39) = KE(IG+2)
      KE(IWBEG+43) = KE(IG+3)
      KE(IWBEG+44) = KE(IG+4)
      KE(IWBEG+45) = KE(IG+5)
      KE(IWBEG+49) = KE(IG+6)
      KE(IWBEG+50) = KE(IG+7)
      KE(IWBEG+51) = KE(IG+8)
      KE(IWBEG+58) = KE(IG  )
      KE(IWBEG+59) = KE(IG+1)
      KE(IWBEG+60) = KE(IG+2)
      KE(IWBEG+64) = KE(IG+3)
      KE(IWBEG+65) = KE(IG+4)
      KE(IWBEG+66) = KE(IG+5)
      KE(IWBEG+70) = KE(IG+6)
      KE(IWBEG+71) = KE(IG+7)
      KE(IWBEG+72) = KE(IG+8)
C
C                       T      E
C     FORM THE PRODUCT W   X  K   AND STORE IN KEP(73)
C                       NPVT
C
      CALL GMMATD (KE(37),6,6,1, KEP(IKEL),6,6,0, KEP(73))
C
C     COMPUTE THE FINAL ANSWER AND STORE IN KEP(109)
C
      CALL GMMATD (KEP(73),6,6,0, KE(IWBEG+37),6,6,0, KEP(109))
C
C     INSERT THIS 6 X 6
C
      CALL SMA1B (KEP(109),INDEX,-1,IFKGG,0.0D0)
      IF (IOPT4.EQ.0 .OR. GSUBE.EQ.0.0) GO TO 400
      K4GGSW = 1
      CALL SMA1B (KEP(109),INDEX,-1,IF4GG,DAMPC)
C
C     IF IPASS = 2, WE ARE DONE.  OTHERWISE COMPUTE THE OFF-DIAGONAL
C     6 X 6.
C
  400 IF (IPASS .EQ. 2) GO TO 500
      IWBEG = 36
      IPASS = 2
      DO 410 I = 28,36
  410 KE(I) = 0.0D0
      IF (IPVT-1) 360,365,360
  500 RETURN
C
 1010 CALL MESAGE (30,26,IECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
C
C
C     HEAT FORMULATION CONTINUES HERE.  GET MATERIAL PROPERTY -K- FROM
C     HMAT
C
 2000 MATFLG = 1
      MATIDC = IECPT( 8)
      ELTEMP = ECPT(39)
      CALL HMAT (IELID)
C
      FL = DBLE(FK)*DBLE(ECPT(9))/(DP(9)*DP(10)*DBLE(DCR))
      IF (NPVT .EQ. IECPT(3)) FL = -FL
      DO 2020 I = 1,2
      CALL SMA1B (FL,IECPT(I+1),NPVT,IFKGG,0.0D0)
      FL = -FL
 2020 CONTINUE
      RETURN
      END
