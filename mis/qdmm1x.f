      SUBROUTINE QDMM1X
C
C     THIS ROUTINE IS SAME AS QDMM1D EXCEPT IT USES EMGOLD/SMA1B LOGIC.
C     (QDMM1D USE EMGOUT LOGIC). IT IS CALLED ONLY BY KTRIQD TO IMPROVE
C     QUAD2 MEMBRANE COMPUTATION. KTRIQD BELONGS TO THE EMGOLD FAMILY OF
C     ELEMENTS.
C
C     QDMM1D COMPUTE THE STIFFNESS MATRIX FOR THE FIRST QUADRILATERAL
C     MEMBRANE ELEMENT. MASS MATRIX IS NOT COMPUTE HERE.
C
C     THIS ROUTINE WAS RE-ASSEMBLED BY G.CHAN/UNISYS   5/91
C
C     ECPT LIST
C                                                   IN THIS
C        ECPT       DESCRIPTION                     ROUTINE    TYPE
C     ========   ================================   ========  =======
C     ECPT( 1) = ELEMENT ID                         NECPT(1)  INTEGER
C     ECPT( 2)   GRID POINT A                       NGRID(1)  INTEGER
C     ECPT( 3)   GRID POINT B                       NGRID(2)  INTEGER
C     ECPT( 4)   GRID POINT C                       NGRID(3)  INTEGER
C     ECPT( 5)   GRID POINT D                       NGRID(4)  INTEGER
C     ECPT( 6) = THETA = ANGLE OF MATERIAL          ANGLE     REAL
C     ECPT( 7)   MATERIAL ID                        MATID     INTEGER
C     ECPT( 8) = THICKNESS                          T         REAL
C     ECPT( 9) = NON-STRUCTURAL MASS                FMU       REAL
C     ECPT(10)   COORD. SYSTEM ID 1                 NECPT(10) INTEGER
C     ECPT(11) = X1                                 X1        REAL
C     ECPT(12) = Y1                                 Y1        REAL
C     ECPT(13) = Z1                                 Z1        REAL
C     ECPT(14)   COORD. SYSTEM ID 2                 NECPT(14) INTEGER
C     ECPT(15) = X2                                 X2        REAL
C     ECPT(16) = Y2                                 Y2        REAL
C     ECPT(17) = Z2                                 Z2        REAL
C     ECPT(18)   COORD. SYSTEM ID 3                 NECPT(18) INTEGER
C     ECPT(19) = X3                                 X3        REAL
C     ECPT(20) = Y3                                 Y3        REAL
C     ECPT(21) = Z3                                 Z3        REAL
C     ECPT(22)   COORD. SYSTEM ID 4                 NECPT(22) INTEGER
C     ECPT(23) = X4                                 X4        REAL
C     ECPT(24) = Y4                                 Y4        REAL
C     ECPT(25)   Z4                                 Z4        REAL
C     ECPT(26) = ELEMENT TEMPERATURE                ELTEMP    REAL
C
      LOGICAL         NOGO,     HEAT,     PLANAR
      INTEGER         OUTPT,    MAP(2,4), ELID
      REAL            ECPT(26)
      DOUBLE PRECISION          AQ,       BQ,       CQ,       B,
     1                C,        D,        E,        F,        H,
     2                O,        P,        Q,        U,        H1,
     3                HH,       LA,       LB,       LC,       LD,
     4                LBD1,     LCD1,     LCD2,     LDD2,     DLT1,
     5                DLT2,     PI1,      PI2,      PI3,      PJ1,
     6                PJ2,      PJ3,      PK1,      PK2,      PK3,
     7                CTH1,     CTH2,     CTH31,    CTH32,    CTH41,
     8                CTH42,    STH1,     STH2,     STH31,    STH32,
     9                STH41,    STH42,    BTXK,     TIE,      TI,
     O                FACT,     TEMP,     ETA01(2), YSUB4,    MAGI,
     1                MAGJ,     MAGK,     X12,      X13,      X14,
     2                X21,      X23,      X24,      X31,      X34,
     3                X41,      X42,      Y21,      Y31,      Y34,
     4                Y41,      Y42,      Y3A,      Y4A,      Z21,
     5                Z31,      Z41,      Z42,      KJJ(3,3), ETA,
     6                TEA,      V,        ETJ,      TEMPAR(144),
     7                KIJ,      ZERO
      CHARACTER       UFM*23,   UWM*25,   UIM*29
      COMMON /XMSSG / UFM,      UWM,      UIM
      COMMON /SYSTEM/ KSYSTM,   OUTPT
      COMMON /CONDAS/ CONSTS(4),DEGRA
      COMMON /SMA1ET/ NECPT(1), NGRID(4), ANGLE,    MATID1,   THICK,
     1                FMU,      DUMMY1,   X1,       Y1,       Z1,
     2                          DUMMY2,   X2,       Y2,       Z2,
     3                          DUMMY3,   X3,       Y3,       Z3,
     4                          DUMMY4,   X4,       Y4,       Z4
      COMMON /SMA1CL/ IOPT4,    K4GGSW,   NPVT,     DUM19(19),NOGO
      COMMON /SMA1IO/ DUM1(10), IFKGG,    DUM2,     IF4GG
      COMMON /SMA1HT/ HEAT
      COMMON /SMA1DP/ KIJ(36),  TIE(9,4), B(144),   E(9),     ETJ(9,4),
     1                U(64),    C(6),     AQ(24),   BQ(24),   CQ(30),
     2                TI(9),    BTXK(96)
      COMMON /MATIN / MATID,    INFLAG,   ELTEMP,   STRESS,   SINTH,
     1                COSTH
      COMMON /MATOUT/ G11,      G12,      G13,      G22,      G23,
     1                G33,      RHO,      ALPHA1,   ALPHA2,   ALP12,
     2                TSUB0,    GSUBE,    SIGTEN,   SIGCOM,   SIGSHE,
     3                G2X211,   G2X212,   G2X222
      EQUIVALENCE     (ECPT(1),NECPT(1),ELID),      (KJJ(1,1),KIJ(1)),
     1                (U(1),TEMPAR(1))
      DATA    ZERO  / 0.0D0 /
C     DATA    M     / 1, 2, 4,  2, 3, 1,  3, 4, 2,  4, 1, 3 /
C           TRIANGLES 1-2-4,    2-3-1,    3-4-2, AND  4-1-3
C
      O(D,V,F,H,P,Q,Y4A,X12,Y34,Y3A,X23,X14,ETA,TEA) =
     1 (D + (V*TEA) + (F*ETA) + (H*TEA*ETA) + (P*TEA*TEA) + (Q*ETA*ETA))
     2 /((-Y4A*X12) + (-Y34*X12*ETA) + ((-Y4A*X23) + (Y3A*X14))*TEA)
C
C     FIND WHICH POINT IS THE PIVOT POINT
C
      DO 10 I = 1,4
      IF (NPVT .NE. NGRID(I)) GO TO 10
      NPIVOT = I
      GO TO 15
   10 CONTINUE
      GO TO 450
C
   15 ETA = 1.D0
      TEA = 1.D0
      ETA01(1) = 0.211324865D0
      ETA01(2) = 0.788675135D0
C
C     COMPUTE DIFFERENCES OF COORDINATES OF ACTUAL GRID POINTS
C
      X21 = X2 - X1
      Y21 = Y2 - Y1
      Z21 = Z2 - Z1
      X31 = X3 - X1
      Y31 = Y3 - Y1
      Z31 = Z3 - Z1
      X41 = X4 - X1
      Y41 = Y4 - Y1
      Z41 = Z4 - Z1
      X42 = X4 - X2
      Y42 = Y4 - Y2
      Z42 = Z4 - Z2
C
C     COMPUTE ELEMENTS OF THE E MATRIX
C
      PK1 = Y31*Z42 - Z31*Y42
      PK2 = Z31*X42 - X31*Z42
      PK3 = X31*Y42 - Y31*X42
      MAGK= DSQRT(PK1**2 + PK2**2 + PK3**2)
      IF (MAGK .LE. 1.D-6) GO TO 410
      PK1 = PK1/MAGK
      PK2 = PK2/MAGK
      PK3 = PK3/MAGK
C
C     HH IS THE MEASURE OF NON-PLANARITY OF THE ELEMENT
C
      HH  = X21*PK1 + Y21*PK2 + Z21*PK3
      PI1 = X21 - HH*PK1
      PI2 = Y21 - HH*PK2
      PI3 = Z21 - HH*PK3
      MAGI= DSQRT(PI1**2 + PI2**2 + PI3**2)
      IF (MAGI .LE. 1.D-6) GO TO 420
      PI1 = PI1/MAGI
      PI2 = PI2/MAGI
      PI3 = PI3/MAGI
      HH  =-HH/2.D0
C
C     THIS SIGN CHANGE MADE BECAUSE SIGN OF H AS DEFINED ON PP 4.87-105
C     OF PROGRAMMERS MANUAL IS WRONG
C
      TEMP = DSQRT(X31**2 + Y31**2 + Z31**2)
      YSUB4= DSQRT(X42**2 + Y42**2 + Z42**2)
      H1   = (2.0*HH)/(TEMP+YSUB4)
      PLANAR = .TRUE.
      IF (H1 .GT. 1.0D-6) PLANAR = .FALSE.
      IF (H1 .GE. 1.0D-2) WRITE (OUTPT,35) UIM,H1,NECPT(1)
   35 FORMAT (A29,' 3061, THE MEASURE OF NON-PLANARITY IS ',D13.5,
     1       ' FOR ELEMENT NUMBER',I9)
      PJ1 = PK2*PI3 - PK3*PI2
      PJ2 = PK3*PI1 - PK1*PI3
      PJ3 = PK1*PI2 - PK2*PI1
      MAGJ= DSQRT(PJ1**2 + PJ2**2 + PJ3**2)
      IF (MAGJ .LE. 1.D-6) GO TO 430
      PJ1 = PJ1/MAGJ
      PJ2 = PJ2/MAGJ
      PJ3 = PJ3/MAGJ
C
C     SET UP E MATRIX (3X3) FOR QUAD-MEMBRANE PROJECTION ONTO MEAN PLANE
C     E IS TRANSPOSE OF E MATRIX IN THEORETICAL MANUAL
C
C     E(1),E(4),E(7) IS I-VECTOR
C     E(2),E(5),E(8) IS J-VECTOR
C     E(3),E(6),E(9) IS K-VECTOR
C
      E(1) = PI1
      E(2) = PJ1
      E(3) = PK1
      E(4) = PI2
      E(5) = PJ2
      E(6) = PK2
      E(7) = PI3
      E(8) = PJ3
      E(9) = PK3
C
C     COMPUTE DIFFERENCES OF COORDINATES OF GRID POINTS IN THE MEAN PLAN
C
      X12 =-(X21*E(1) + Y21*E(4) + Z21*E(7))
      X13 =-(X31*E(1) + Y31*E(4) + Z31*E(7))
      X24 =-(X42*E(1) + Y42*E(4) + Z42*E(7))
      X14 = X12 + X24
      Y3A = X31*E(2)  + Y31*E(5) + Z31*E(8)
      Y4A = X42*E(2)  + Y42*E(5) + Z42*E(8)
      X34 = X14 - X13
      Y34 = Y3A - Y4A
      X23 = X13 - X12
      IF (Y3A.LE.ZERO .OR. Y4A.LE.ZERO) GO TO 430
      TEMP = X12 + X23*(Y4A/Y3A)
      YSUB4= (Y3A/Y4A)*X14
C
C                                              0
C     CHECK FOR INTERNAL ANGLE GREATER THAN 180
C
      IF (X13.GE.YSUB4 .OR. X14.LE.TEMP) GO TO 430
C
C     COMPUTE LENGTHS OF SIDES OF ELEMENT IN THE MEAN PLANE
C
      LA = DABS(X12)
      LB = DSQRT(X23**2 + Y3A**2)
      LC = DSQRT(X34**2 + Y34**2)
      LD = DSQRT(X14**2 + Y4A**2)
      IF (LA.EQ.ZERO .OR. LB.EQ.ZERO .OR. LC.EQ.ZERO .OR. LD.EQ.ZERO)
     1   GO TO 430
C
C     COMPUTE THE CHARACTERISTIC ANGLES OF ELEMENT IN THE MEAN PLANE
C
      IF (PLANAR) GO TO 70
      CTH1  =-X14/LD
      STH1  = Y4A/LD
      CTH2  = X23/LB
      STH2  = Y3A/LB
      CTH31 = X34/LC
      STH31 =-Y34/LC
      CTH41 = CTH1
      STH41 = STH1
      CTH32 = STH2
      STH32 = CTH2
      CTH42 = STH31
      STH42 = CTH31
      DLT1  = CTH31*CTH32 - STH31*STH32
      DLT2  = CTH42*CTH41 + STH41*STH42
      LDD2  = LD*DLT2
      LBD1  = LB*DLT1
      LCD1  = LC*DLT1
      LCD2  = LC*DLT2
C
C     SET UP THE (12X8) TRANSFORMATION MATRIX B BETWEEN THE MEAN PLANE
C     AND ACTUAL GRID POINTS
C
      DO 60 I = 2,92
      B(I) = 0.0
   60 CONTINUE
C
      B( 1) = 1.0
      B(10) = 1.0
      B(17) =-HH/LA
      B(18) =-HH/(LD*STH1) + ((HH*CTH1)/(LA*STH1))
      B(19) = HH/LA
      B(20) = (HH*CTH2)/(LA*STH2)
      B(23) = (HH*CTH42)/LDD2
      B(24) = (HH*STH42)/LDD2
      B(27) = 1.0
      B(36) = 1.
      B(41) =-B(17)
      B(42) = (-HH*CTH1)/(LA*STH1)
      B(43) = B(17)
      B(44) = ((-HH*CTH2)/(LA*STH2)) + (HH/(LB*STH2))
      B(45) = (-HH*STH31)/LBD1
      B(46) = (-HH*CTH31)/LBD1
      B(53) = 1.
      B(62) = 1.
      B(68) =-HH/(LB*STH2)
      B(69) = HH*((STH31/LBD1) + (CTH32/LCD1))
      B(70) = HH*((CTH31/LBD1) + (STH32/LCD1))
      B(71) = (-HH*STH41)/LCD2
      B(72) = (HH*CTH41)/LCD2
      B(79) = 1.0
      B(88) = 1.0
      B(90) = HH/(LD*STH1)
      B(93) = (-HH*CTH32)/LCD1
      B(94) = (-HH*STH32)/LCD1
      B(95) = HH*((-CTH42/LDD2) + (STH41/LCD2))
      B(96) = HH*((-STH42/LDD2) - (CTH41/LCD2))
C
   70 THETA = ANGLE*DEGRA
      SINTH = SIN(THETA)
      COSTH = COS(THETA)
      IF (ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0
      ELTEMP = ECPT(26)
      INFLAG = 2
      MATID  = MATID1
C
C                                                     T
C     COMPUTE TRANSFORMED MATRIX OF STIFFNESSES  C = P  * G * P
C
      CALL MAT (ECPT(1))
C
C     STORE INTO G MATRIX
C
      C(1) = G11
      C(2) = G12
      C(3) = G22
      C(4) = G13
      C(5) = G23
      C(6) = ZERO
      FACT = G33*DBLE(THICK)/(X24*Y3A - X13*Y4A)*2.0D0
C
C     COMPUTE COEFFICIENTS OF THE GENERAL INTEGRAL
C
C                                            2         2
C     D + E*ETA + F*ZETA + H*ETA*ZETA + P*ETA  + Q*ZETA
C     --------------------------------------------------
C     Y *X   +Y  *X  *ZETA + (Y *X   - Y *X  ) * ETA
C      4  21   34  21          4  32    3  41
C
      AQ( 1) =-Y4A
      AQ( 3) =-X24
      AQ( 5) =-X24
      AQ( 6) =-Y4A
      AQ( 7) = Y4A
      AQ( 9) = X14
      AQ(11) = X14
      AQ(12) = Y4A
      AQ(13) = 0.0
      AQ(15) = 0.0
      AQ(17) = 0.0
      AQ(18) = 0.0
      AQ(19) = 0.0
      AQ(21) =-X12
      AQ(23) =-X12
      AQ(24) = 0.0
C
      BQ( 1) = Y3A
      BQ( 3) = X23
      BQ( 5) = X23
      BQ( 6) = Y3A
      BQ( 7) =-Y4A
      BQ( 9) =-X14
      BQ(11) =-X14
      BQ(12) =-Y4A
      BQ(13) = Y4A
      BQ(15) = X14
      BQ(17) = X14
      BQ(18) = Y4A
      BQ(19) =-Y3A
      BQ(21) =-X23
      BQ(23) =-X23
      BQ(24) =-Y3A
C
      CQ( 1) =-Y34
      CQ( 3) = X34
      CQ( 5) = X34
      CQ( 6) =-Y34
      CQ( 7) = Y34
      CQ( 9) =-X34
      CQ(11) =-X34
      CQ(12) = Y34
      CQ(13) = 0.0
      CQ(15) =-X12
      CQ(17) =-X12
      CQ(18) = 0.0
      CQ(19) = 0.0
      CQ(21) = X12
      CQ(23) = X12
      CQ(24) = 0.0
C
      NN = 0
      DO 120 I = 1,4
      DO 110 K = 1,2
      DO 100 J = 1,4
      DO  90 L = 1,2
      NN  = NN + 1
      IM1 = I  - 1
      JM1 = J  - 1
      KM1 = K  - 1
      LM1 = L  - 1
      K1  = 6*IM1 + 4*KM1 + 1
      K2  = 6*IM1 + 3*KM1 + 3
      L1  = 6*JM1 + 4*LM1 + 1
      L2  = 6*JM1 + 3*LM1 + 3
      KL  = K + L - 1
      K3  = K + 3
      L3  = L + 3
      D = C(KL)*AQ(K1)*AQ(L1)+C(K3)*AQ(K1)*AQ(L2)+C(L3)*AQ(K2)*AQ(L1)
C
      V = C(KL)*((AQ(K1)*BQ(L1))+(BQ(K1)*AQ(L1)))+C(K3)*((AQ(K1)*BQ(L2))
     1  + (BQ(K1)*AQ(L2)))+C(L3)*((AQ(K2)*BQ(L1))+(BQ(K2)*AQ(L1)))
C
      F = C(KL)*((AQ(K1)*CQ(L1))+(CQ(K1)*AQ(L1)))+C(K3)*((AQ(K1)*CQ(L2))
     1  + (CQ(K1)*AQ(L2)))+C(L3)*((AQ(K2)*CQ(L1))+(CQ(K2)*AQ(L1)))
C
      H = C(KL)*((BQ(K1)*CQ(L1))+(CQ(K1)*BQ(L1)))+C(K3)*((BQ(K1)*CQ(L2))
     1  + (CQ(K1)*BQ(L2)))+C(L3)*((BQ(K2)*CQ(L1))+(CQ(K2)*BQ(L1)))
C
      P = C(KL)*BQ(K1)*BQ(L1)+C(K3)*BQ(K1)*BQ(L2)+C(L3)*BQ(K2)*BQ(L1)
C
      Q = C(KL)*CQ(K1)*CQ(L1)+C(K3)*CQ(K1)*CQ(L2)+C(L3)*CQ(K2)*CQ(L1)
C
C     USE GAUSSIAN INTEGRATION TO FIND THE PARTITIONS OF THE STIFFNESS
C     MATRIX FOR THE MEAN PLANE ELEMENT
C
      U(NN) = ZERO
      DO 80 IA01 = 1,2
      DO 80 JA01 = 1,2
      U(NN) = U(NN) +
     1    O(D,V,F,H,P,Q,Y4A,X12,Y34,Y3A,X23,X14,ETA01(IA01),ETA01(JA01))
   80 CONTINUE
      U(NN) = U(NN)/4.0D0*DBLE(THICK)
C
C     ADD SHEAR TERMS HERE
C
      U(NN) = U(NN) + FACT*(AQ(K2)+0.5*(BQ(K2)+CQ(K2)))
     1                    *(AQ(L2)+0.5*(BQ(L2)+CQ(L2)))
   90 CONTINUE
  100 CONTINUE
  110 CONTINUE
  120 CONTINUE
C
C     TRANSFORM FROM MEAN PLANE TO ACTUAL GRID POINTS
C
C                   T
C      K = B * K * B
C
C     EXPAND MATRIX TO INCLUDE Z COORDINATES
C
C     IF NON-PLANAR,
C
      IF (PLANAR) GO TO 130
      CALL GMMATD (B(1),12,8,0, U(1),8,8,0, BTXK(1))
      CALL GMMATD (BTXK(1),12,8,0, B(1),12,8,1, TEMPAR(1))
      GO TO 200
C
C     IF PLANAR, TEMPAR(12X12) .EQ. U(8X8)
C
  130 IJ1 =-12
      I2  = 144
      DO 140 I = 1,64
  140 TEMPAR(I2+I) = U(I)
      DO 190 I = 1,12
      IJ1 = IJ1 + 12
      IF (MOD(I,3) .NE. 0) GO TO 160
      DO 150 J = 1,12
      IJ = IJ1 + J
  150 TEMPAR(IJ) = ZERO
      GO TO 190
  160 DO 180 J = 1,12
      IJ = IJ1 + J
      IF (MOD(J,3) .NE. 0) GO TO 170
      TEMPAR(IJ) = ZERO
      GO TO 180
  170 I2 = I2 + 1
      TEMPAR(IJ) = TEMPAR(I2)
  180 CONTINUE
  190 CONTINUE
C
C                T            T
C     GENERATE (T  * E) AND (E  * T )
C                I                 J
C
  200 DO 230 I = 1,4
      KA = 4*I + 6
      IF (NECPT(KA) .EQ. 0) GO TO 210
      CALL TRANSD (NECPT(KA),TI)
      CALL GMMATD (TI,3,3,1, E,3,3,0, TIE(1,I))
      CALL GMMATD (E,3,3,1, TI,3,3,0, ETJ(1,I))
      GO TO 230
  210 DO 220 II = 1,9
      TIE(II,I) = E(II)
  220 CONTINUE
      ETJ(1,I) = E(1)
      ETJ(2,I) = E(4)
      ETJ(3,I) = E(7)
      ETJ(4,I) = E(2)
      ETJ(5,I) = E(5)
      ETJ(6,I) = E(8)
      ETJ(7,I) = E(3)
      ETJ(8,I) = E(6)
      ETJ(9,I) = E(9)
  230 CONTINUE
C                                      T              T
C     COMPUTE STIFFNESS MATRIX  K   = T  * E * S   * E  * T
C                                IJ    I        IJ         J
C
C     EXTRACT 3 BY 3 PARTITIONS, TRANSFORM TO GLOBAL, AND INSERT BY
C     ORDER OF SILS INTO A 12X12 MATRIX
C
      DO 260 I = 1,4
      J = NGRID(I)
      DO 240 K = 2,5
      IF (NECPT(K) .EQ. J) GO TO 250
  240 CONTINUE
      GO TO 450
  250 MAP(1,I) = J
  260 MAP(2,I) = I
      CALL SORT (0,0,2,1,MAP(1,1),8)
C
C     REPLACE SILS WITH INDICES
C     RESORT FOR ORIGINAL ORDER - WORD 1 WILL CONTAIN NEW LOCATION
C
      DO 270 I = 1,4
  270 MAP(1,I) = I
      CALL SORT (0,0,2,2,MAP(1,1),8)
C
C     MOVE AND TRANSFORM HERE
C
C     ROW LOOP
C
      DO 300 I = 1,4
      IOR = 36*(I-1)
      INR = 36*(MAP(1,I)-1)
C
C     COLUMN LOOP
C
      DO 300 J = 1,4
      IOCL = IOR + 3*(J-1)
      INCL = INR + 3*(MAP(1,J)-1)
C
C     INNER LOOPS
C
      DO 280 K = 1,3
      KL = IOCL + 12*(K-1)
      DO 280 L = 1,3
      KJJ(L,K) = TEMPAR(KL+L)
  280 CONTINUE
C
C     TRANSFORM 3 BY 3
C
      CALL GMMATD (KJJ,3,3,0, ETJ(1,J),3,3,0, E)
      CALL GMMATD (TIE(1,I),3,3,0, E,3,3,0, KJJ)
C
C     INSERT
C
      DO 290 K = 1,3
      KL = INCL + 12*(K-1)
      DO 290 L = 1,3
      B(KL+L) = KJJ(L,K)
  290 CONTINUE
  300 CONTINUE
C
C     PREPARE OUTPUT TO SMA1B
C
      CALL SORT (0,0,4,1,NGRID(1),4)
      DO 350 J = 1,4
      IF (NPVT .NE. NGRID(J)) GO TO 350
      MPOINT = (J-1)*36
      IF (HEAT) GO TO 330
C
C     SEND ONLY THE 4 6X6 SUBMATRICES ASSOCIATED TO THE PIVOT POINT TO
C     SMA1B
C
      DO 320 K = 1,4
      DO 310 I = 1,36
  310 KIJ(I) = ZERO
C
      KIJ( 1) = B(MPOINT+ 1)
      KIJ( 2) = B(MPOINT+ 2)
      KIJ( 3) = B(MPOINT+ 3)
      KIJ( 7) = B(MPOINT+13)
      KIJ( 8) = B(MPOINT+14)
      KIJ( 9) = B(MPOINT+15)
      KIJ(13) = B(MPOINT+25)
      KIJ(14) = B(MPOINT+26)
      KIJ(15) = B(MPOINT+27)
      CALL SMA1B (KIJ(1),NGRID(K),-1,IFKGG,ZERO)
  320 MPOINT = MPOINT + 3
C
      IF (IOPT4.EQ.0 .OR. GSUBE.EQ. 0.0) GO TO 350
      TEMP = GSUBE
      CALL SMA1B (KIJ(1),NGRID(J),-1,IF4GG,TEMP)
      K4GGW = 1
      GO TO 350
C
C     HEAT FORMULATION
C
  330 CALL SMA1B (B(MPOINT+1),NGRID(I),NPVT,IFKGG,ZERO)
C
  350 CONTINUE
      GO TO 470
C
C     ERROR EXITS
C
  410 J = 32
      GO TO 440
  420 J = 31
      GO TO 440
  430 J = 26
  440 K = 30
      GO TO 460
  450 K =-30
      J = 34
  460 CALL MESAGE (K,J,ECPT(1))
      NOGO = .TRUE.
C
  470 RETURN
      END
