      SUBROUTINE SELBO1
C
C     THIS ROUTINE IS PHASE 1 OF STRESS DATA RECOVERY FOR THE ELBOW
C     ELEMENT MUCH OF THE CODE WAS LIFTED FROM THE KELBOW SUBROUTINE
C
C     ECPT FOR THE ELBOW
C
C     ECPT( 1)  -  IELID          ELEMENT ID. NUMBER
C     ECPT( 2)  -  ISILNO(2)      * SCALAR INDEX NOS. OF THE GRID POINTS
C     ECPT( 3)  -    ...          *
C     ECPT( 4)  -  SMALLV(3)      $ REFERENCE VECTOR
C     ECPT( 5)  -    ...          $
C     ECPT( 6)  -    ...          $
C     ECPT( 7)  -  ICSSV          COOR. SYS. ID FOR SMALLV VECTOR
C     ECPT( 8)  -  IMATID         MATERIAL ID.
C     ECPT( 9)  -  A              CROSS-SECTIONAL AREA
C     ECPT(10)  -  I1             $ AREA MOMENTS OF INERTIA
C     ECPT(11)  -  I2             $
C     ECPT(12)  -  FJ             TORSIONAL CONSTANT
C     ECPT(13)  -  NSM            NON-STRUCTURAL MASS
C     ECPT(14)  -  FE             FORCE ELEM. DESCRIPTIONS, FORCE METHOD
C     ECPT(15)  -  R1             *STRESS RECOVERY COEFFICIENTS
C     ECPT(16)  -  T1             *  RI=RADIAL LOCATION
C     ECPT(17)  -  R2             *  TI=ANGULAR LOCATION
C     ECPT(18)  -  T2             *     OF STRESS RECOVERY POINTS
C     ECPT(19)  -  R3             *
C     ECPT(20)  -  T3             *
C     ECPT(21)  -  R4             *
C     ECPT(22)  -  T4             *
C     ECPT(23)  -  K1             $ AREA FACTOR FOR SHEAR
C     ECPT(24)  -  K2             $
C     ECPT(25)  -  C              STRESS INTENSIFICATION FACTOR
C     ECPT(26)  -  KX             * FLEXIBILITY CORRECTION FACTORS
C     ECPT(27)  -  KY             *
C     ECPT(28)  -  KZ             *
C     ECPT(29)  -  R              RADIUS OF CURVATURE
C     ECPT(30)  -  BETAR          ANGLE FROM GA TO GB
C     ECPT(31)  -  MCSIDA         COORD. SYS. ID. FOR GRID POINT A
C     ECPT(32)  -  GPA(3)         *BASIC COORD. FOR GRID POINT A
C     ECPT(33)  -   ...           *
C     ECPT(34)  -   ...           *
C     ECPT(35)  -  MCSIDB         COORD. SYS. ID. FOR GRID POINT B
C     ECPT(36)  -  GPB(3)         *BASIC COORD. FOR GRID POINT B
C     ECPT(37)  -   ...           *
C     ECPT(38)  -   ...           *
C     ECPT(39)  -  ELTEMP         AVG. ELEMENT TEMPERATURE
C
C
      LOGICAL         ABASIC,BBASIC,BASIC
      REAL            L,I1,I2,K1,K2,KE,KEP,NSM,HUT( 6),KEE(12,12),
     1                KX,KY,KZ
      DIMENSION       VECI(3),VECJ(3),VECK(3),ECPT(100),IECPT(100),
     1                TA(18),TB(9),SMALV0(6),DP(20),F(6,6),S(12,12),
     2                H(6,6),DF(6,6)
      COMMON /SDR2X5/ IELID,ISILNO(2),SMALLV(3),ICSSV,IMATID,A,I1,I2,
     1                FJ,NSM,FE,C1,C2,D1,D2,F1,F2,G1,G2,K1,K2,C,
     2                KX,KY,KZ,R,BETAR,MCSIDA,GPA(3),MCSIDB,GPB(3),
     3                TEMPEL,DUM3(61)
      COMMON /SDR2X5/ JELID,JSILNO(2),SA(36),SB(36),OUT(21),THERM(30)
      COMMON /SDR2X6/ KE(144),KEP(144),DELA(6),DELB(6)
      COMMON /MATIN / MATIDC,MATFLG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ E,G,NU,RHO,ALPHA,T SUB 0,GSUBE,SIGT,SIGC,SIGS
      EQUIVALENCE     (IELID,ECPT(1),IECPT(1)), (TA(10),TB(1)),
     1                (KEE(1,1),KE(1),S(1,1))
      DATA    DCR   / .017453292 /
C
      SID(X) = SIN(X*DCR)
      COD(X) = COS(X*DCR)
      DTR(X) = X*DCR
C
      X    = 1.0
      ISOP = -1
C
C     SET UP POINTERS TO COORD. SYSTEM IDS
C
      JCSIDA = 31
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
      FLD   = SQRT(DP(7)**2 + DP(8)**2 + DP(9)**2)
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
      IF (ABASIC) GO TO 60
      CALL TRANSS (ECPT(JCSIDA),TA)
      CALL GMMATS (TA,3,3,0, DP(7),3,1,0, VECJ)
      CALL GMMATS (TA,3,3,0, DP(1),3,1,0, DP(14))
      DP(1) = DP(14)
      DP(2) = DP(15)
      DP(3) = DP(16)
      GO TO 61
   60 CONTINUE
      VECJ(1) = DP(7)
      VECJ(2) = DP(8)
      VECJ(3) = DP(9)
   61 IF (BBASIC) GO TO 62
      CALL TRANSS (ECPT(JCSIDB),TB)
      CALL GMMATS (TB,3,3,0, DP(4),3,1,0, DP(14))
      DP(4) = DP(14)
      DP(5) = DP(15)
      DP(6) = DP(16)
   62 CONTINUE
C
C     CONSTRUCT VECTOR FROM A TO B
C
      SMALV0(1) = DP(4) - DP(1)
      SMALV0(2) = DP(5) - DP(2)
      SMALV0(3) = DP(6) - DP(3)
      FLL       = SQRT(SMALV0(1)**2 + SMALV0(2)**2 + SMALV0(3)**2)
      SMALV0(1) = SMALV0(1)/FLL
      SMALV0(2) = SMALV0(2)/FLL
      SMALV0(3) = SMALV0(3)/FLL
C
C     COMPUTE THE K VECTOR VECK = SMALV0 X VECJ
C
      VECK(1) = SMALV0(2)*VECJ(3) - SMALV0(3)*VECJ(2)
      VECK(2) = SMALV0(3)*VECJ(1) - SMALV0(1)*VECJ(3)
      VECK(3) = SMALV0(1)*VECJ(2) - SMALV0(2)*VECJ(1)
      FLL     = SQRT(VECK(1)**2 + VECK(2)**2 + VECK(3)**2)
      VECK(1) = VECK(1)/FLL
      VECK(2) = VECK(2)/FLL
      VECK(3) = VECK(3)/FLL
C
C     COMPUTE THE I VECTOR  VECI = VECJ X VECK
C
      VECI(1) = VECJ(2)*VECK(3) - VECJ(3)*VECK(2)
      VECI(2) = VECJ(3)*VECK(1) - VECJ(1)*VECK(3)
      VECI(3) = VECJ(1)*VECK(2) - VECJ(2)*VECK(1)
      FLL     = SQRT(VECI(1)**2 + VECI(2)**2 + VECI(3)**2)
      VECI(1) = VECI(1)/FLL
      VECI(2) = VECI(2)/FLL
      VECI(3) = VECI(3)/FLL
C
C     SEARCH THE MATERIAL PROPERTIES TABLE FOR E,G AND THE DAMPING
C     CONSTANT.
C
      MATIDC = IMATID
      MATFLG = 1
      IF (ISOP .EQ. 3) MATFLG = 12
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
C
C     THE FOLLOWING CODE WAS TAKEN FROM SAP4 BENDKS ROUTINE FOR A CURVED
C     PIPE ELEMENT
C
C
C     COMPUTE SECTION PROPERTY CONSTANTS
C
      T  = DTR(BETAR)
      RA = R/(A*E)
      RV1= K1*R/(2.*G*A)
      RV2= K2/K1*RV1
      RT = R/(G*FJK*2.)
      RB0= R/(E*FI2*2.)
      RB1= R/(E*FI1)
      R2 = R**2
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
C     X - DIRECTION  IN-PLANE TANGENT TO THE BEND AT NODE I AND
C                    DIRECTED TOWARD NODE J
C     Y - DIRECTION  IN-PLANE AND DIRECTED RADIALLY INWARD TO THE
C                    CENTER OF CURVATURE
C     Z - DIRECTION  OUT OF PLANE AND ORTHOGONAL TO X AND Y
C
      DO 50 I = 1,6
      DO 50 K = I,6
      F(I,K) = 0.0
   50 CONTINUE
C
C     A X I A L
C
      F(1,1) = F(1,1) + 0.25*RA*(2.0*T + S2T)
      F(2,2) = F(2,2) + 0.25*RA*(2.0*T - S2T)
C
C     N O T E   (COEFFICIENT CHANGE)
C
      F(1,2) = F(1,2) + 0.50*RA*ST**2
C
C     S H E A R
C
      F(1,1) = F(1,1) + 0.5*RV1*(2.0*T - S2T)
      F(2,2) = F(2,2) + 0.5*RV1*(2.0*T + S2T)
      F(3,3) = F(3,3) + 2.0*RV2*T
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
      DF(K,I) = F(I,K)
      DF(I,K) = DF(K,I)
   65 CONTINUE
C
C
C     INVERT FLEX TO FORM STIFFNESS
C
      CALL INVERS (6,DF,6,DUM,0,DETERM,ISING,H)
      IF (ISING .EQ. 2) WRITE (6,4002) F
      IF (ISING .EQ. 2) CALL MESAGE (-30,38,ECPT(1))
 4002 FORMAT (35H ELBOW STIFFNESS MATRIX IS SINGULAR, /,(5X,6E13.5))
C
C
C     SET UP THE FORCE TRANSFORMATION RELATING REACTIONS AT NODE I
C     ACTING ON THE MEMBER END DUE TO UNIT LOADS APPLIED TO THE MEMBER
C     END AT NODE J.
C
      DO 100 I = 1,6
      DO 100 K = 1,6
      H(I,K) = 0.0
  100 CONTINUE
C
      DO 105 K = 1,6
      H(K,K) =-1.0
  105 CONTINUE
C
      H(4,3) =-(R*(1.0 - CT))
      H(5,3) = (R*ST)
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
      S(IR,IC+6) = 0.0
      DO 120 IN = 1,6
      S(IR,IC+6) = S(IR,IC+6) + H(IR,IN)*DF(IN,IC)
  120 CONTINUE
  130 CONTINUE
C
      DO 150 IR = 1,6
      DO 150 IC = IR,6
      S(IR,IC)  = 0.0
      DO 140 IN = 1,6
      S(IR,IC) = S(IR,IC) + S(IR,IN+6)*H(IC,IN)
  140 CONTINUE
  150 CONTINUE
C
C     REFLECT FOR SYMMETRY
C
      DO 165 I = 1,12
      DO 165 K = I,12
      S(K,I) = S(I,K)
  165 CONTINUE
C
C            E
C     STORE K   IN KEP(1) THRU KEP(36) AND
C            AA
C
C            E
C     STORE K   IN KEP(37) THRU KEP(72)
C            AB
C
      J = 0
      DO 340 I = 1,72,12
      LOW = I
      LIM = LOW + 5
      DO 330 K = LOW,LIM
      J = J + 1
      KEP(J) = KE(K)
  330 KEP(J+36) = KE(K+6)
  340 CONTINUE
C
C     COMPUTE THERMAL MATRIX
C
      L = DCR*ECPT(29)*ECPT(30)
      DO 341 I = 1,6
  341 HUT(I) = 0.0
      ALPHAR = ALPHA*R
      HUT(1) =-ALPHAR*SID(BETAR)
      HUT(2) =-ALPHAR*(1.-COD(BETAR))
      HUT(6) = 0.0
      CALL GMMATS (KEP(1),6,6,0, HUT,6,1,0, THERM(1))
C
C                                                             T
C     STORE VECI, VECJ, VECK IN KE(1) THRU KE(9) FORMING THE A  MATRIX.
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
C     SET POINTERS SO THAT WE WILL BE WORKING WITH POINT A.
C
      BASIC = ABASIC
      JCSID = JCSIDA
      IWBEG = 0
      IKEL  = 1
      IAB   = 1
      INDEX = ISILNO(1)
C
C     ZERO OUT THE ARRAY WHERE THE 3 X 3 MATRIX AND THE W  AND W  6 X 6
C     MATRICES WILL RESIDE.                              A      B
C
      DO 350 I = 28,108
  350 KE(I) = 0.0
C
C     SET UP THE -G- MATRIX. IG POINTS TO THE BEGINNING OF THE G MATRIX.
C     G = AT X TI
C
  360 IG = 1
      IF (BASIC) GO TO 380
      CALL TRANSS (ECPT(JCSID),KE(10))
      CALL GMMATS (KE(1),3,3,0, KE(10),3,3,0, KE(19))
      IG = 19
C
C     FORM THE W  MATRIX OR THE W  MATRIX IN KE(37) OR KE(73) DEPENDING
C               A                B
C     UPON WHICH POINT - A OR B - IS UNDER CONSIDERATION.  G WILL BE
C     STORED IN THE UPPER LEFT AND LOWER RIGHT CORNERS.  H, IF NON-ZERO,
C     WILL BE STORED IN THE UPPER RIGHT CORNER.
C
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
C                              E                    E
C     FORM THE PRODUCT  S  =  K   X  W   OR  S   = K    X  W, DEPENDING
C                        A     AA     A       B     AB      B
C     UPON WHICH POINT WE ARE WORKING WITH.
C
      CALL GMMATS (KEP(IKEL),6,6,0, KE(IWBEG+37),6,6,0, SA(IAB))
C
C     IF THE POINT UNDER CONSIDERATION IS POINT B WE ARE FINISHED. IF
C     NOT, SET UP POINTS AND INDICATORS FOR WORKING WITH POINT B.
C
      IF (IWBEG .EQ. 36) GO TO 500
      BASIC = BBASIC
      JCSID = JCSIDB
      IWBEG = 36
      IKEL  = 37
      IAB   = 37
      INDEX = ISILNO(2)
      DO 400 I = 28,36
  400 KE(I) = 0.0
      GO TO 360
C
C     FILL REMAINDER OF OUTPUT BLOCK.
C
  500 JELID   = IELID
      JSILNO(1) = ISILNO(1)
      JSILNO(2) = ISILNO(2)
      I12     = 0.
      OUT( 1) = A*E*ALPHA
      OUT( 2) = A*E/L
      OUT( 3) = A
      OUT( 4) = FJ
      OUT( 5) = I1
      OUT( 6) = I2
      OUT( 7) = C
      OUT( 8) = C1
      OUT( 9) = C2
      OUT(10) = D1
      OUT(11) = D2
      OUT(12) = F1
      OUT(13) = F2
      OUT(14) = G1
      OUT(15) = G2
      OUT(16) = T SUB 0
      OUT(17) = SIGT
      OUT(18) = SIGC
      OUT(19) = L
      OUT(20) = R
      OUT(21) = BETAR
      RETURN
      END
