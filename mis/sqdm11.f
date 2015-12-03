      SUBROUTINE SQDM11
C
C     PHASE I OF STRESS DATA RECOVERY FOR THE  QUADRILATERAL MEMBRANE
C     ELEMENT
C
C     CALLS FROM THIS ROUTINE ARE MADE TO
C
C            MAT    - MATERIAL DATA ROUTINE
C            MESAGE - ERROR MESSAGE WRITER
C            GMMATS - SINGLE MATRIX MULTIPLY AND TRANSPOSE
C            TRANSS - SINGLE PRECISION TRANSFORMATION SUPPLIER
C
C
      REAL            LA,LB,LC,LD,LDD2,LBD1,LCD1,LCD2,MAGI,MAGJ,MAGK
      DIMENSION       ECPT(26),EE(144)
      COMMON /SYSTEM/ DUMMY(39),NBPW
      COMMON /CONDAS/ CONSTS(5)
      COMMON /SDR2X5/ NECPT(1),NGRID(4),ANGLE,MATID1,T,FMU,
     2                DUMMY1,X1,Y1,Z1,DUMMY2,X2,Y2,Z2,
     4                DUMMY3,X3,Y3,Z3,DUMMY4,X4,Y4,Z4,DUMB(75),
     6                PH1OUT(100),FORVEC(25)
      COMMON /SDR2X6/ E(9),TI(9),THETA,TEMPAR(150),A(24),G(9),B(96)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ G11,G12,G13,G22,G23,G33,RHO,ALPHAS(3),TSUB0,GSUBE,
     1                SIGTEN,SIGCOM,SIGSHE,G2X211,G2X212,G2X222
      EQUIVALENCE     (CONSTS(4),DEGRA),(ECPT(1),NECPT(1))
C
C
C      ECPT LIST
C                                                      IN
C                                                      THIS
C        ECPT       DESCRIPTION                        ROUTINE    TYPE
C     *******    *********************************     ********  ******
C     ECPT( 1) = ELEMENT ID                            NECPT(1)  INTEGER
C     ECPT( 2)   GRID POINT A                          NGRID(1)  INTEGER
C     ECPT( 3)   GRID POINT B                          NGRID(2)  INTEGER
C     ECPT( 4)   GRID POINT C                          NGRID(3)  INTEGER
C     ECPT( 5)   GRID POINT D                          NGRID(4)  INTEGER
C     ECPT( 6) = THETA = ANGLE OF MATERIAL             ANGLE      REAL
C     ECPT( 7)   MATERIAL ID                           MATID     INTEGER
C     ECPT( 8) = THICKNESS                             T          REAL
C     ECPT( 9) = NON-STRUCTURAL MASS                   FMU        REAL
C     ECPT(10)   COORD. SYSTEM ID 1                    NECPT(10) INTEGER
C     ECPT(11) = X1                                     X1        REAL
C     ECPT(12) = Y1                                     Y1        REAL
C     ECPT(13) = Z1                                     Z1        REAL
C     ECPT(14)   COORD. SYSTEM ID 2                    NECPT(14) INTEGER
C     ECPT(15) = X2                                     X2        REAL
C     ECPT(16) = Y2                                     Y2        REAL
C     ECPT(17) = Z2                                     Z2        REAL
C     ECPT(18)   COORD. SYSTEM ID 3                    NECPT(18) INTEGER
C     ECPT(19) = X3                                     X3        REAL
C     ECPT(20) = Y3                                     Y3        REAL
C     ECPT(21) = Z3                                     Z3        REAL
C     ECPT(22)   COORD. SYSTEM ID 4                    NECPT(22) INTEGER
C     ECPT(23) = X4                                     X4        REAL
C     ECPT(24) = Y4                                     Y4        REAL
C     ECPT(25)   Z4                                     Z4        REAL
C     ECPT(26) = ELEMENT TEMPERATURE                    ECPT(26)  REAL
C
C
C     SET UP THE E MATRIX WHICH IS (12X12) FOR THE QUAD-MEMBRANE PROJECT
C                         ONTO THE MEAN PLANE
C
      DO 2 I = 1,144
      EE(I) = 0.
    2 CONTINUE
C
C     E(1), E(4), E(7) WILL BE THE I-VECTOR
C     E(2), E(5), E(8) WILL BE THE J-VECTOR
C     E(3), E(6), E(9) WILL BE THE K-VECTOR
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
C     COMPUTE ELEMENTS OF THE E MATRIX (3X3)
C
      PK1 = Y31*Z42 - Z31*Y42
      PK2 = Z31*X42 - X31*Z42
      PK3 = X31*Y42 - Y31*X42
      MAGK= SQRT(PK1**2 + PK2**2 + PK3**2)
      IF (MAGK .GT. 1.0E-06) GO TO 40
      CALL MESAGE (-30,32,ECPT(1))
   40 PK1 = PK1/MAGK
      PK2 = PK2/MAGK
      PK3 = PK3/MAGK
C
C     HH IS THE MEASURE OF NON-PLANARITY OF THE ELEMENT
C
      HH  = X21*PK1 + Y21*PK2 + Z21*PK3
      PI1 = X21 - HH*PK1
      PI2 = Y21 - HH*PK2
      PI3 = Z21 - HH*PK3
      MAGI= SQRT(PI1**2 + PI2**2 + PI3**2)
      IF (MAGI .GT. 1.0E-06) GO TO 41
      CALL MESAGE (-30,31,ECPT(1))
   41 PI1 = PI1/MAGI
      PI2 = PI2/MAGI
      PI3 = PI3/MAGI
      HH  =-HH/2.
C
C     THIS SIGN CHANGE MADE BECAUSE SIGN OF H AS DEFINED ON
C     PAGE 4.87-105 OF PROGRAMMERS MANUAL IS WRONG
C
      PJ1 = PK2*PI3 - PK3*PI2
      PJ2 = PK3*PI1 - PK1*PI3
      PJ3 = PK1*PI2 - PK2*PI1
      MAGJ= SQRT(PJ1**2 + PJ2**2 + PJ3**2)
      PJ1 = PJ1/MAGJ
      PJ2 = PJ2/MAGJ
      PJ3 = PJ3/MAGJ
      E(1)= PI1
      E(2)= PJ1
      E(3)= PK1
      E(4)= PI2
      E(5)= PJ2
      E(6)= PK2
      E(7)= PI3
      E(8)= PJ3
      E(9)= PK3
C
C     STORE FOUR (3X3) E MATRICES INTO (12X12) E MATRIX
C
      LLCT = -39
      DO 5 IICT = 1,12,3
      LLCT = LLCT + 39
      NNCT = 0
      MMCT =-12
      DO 4 JJCT = 1,3
      MMCT = MMCT + 12
      DO 3 KKCT = 1,3
      NNCT = NNCT + 1
      KTOT = KKCT + LLCT + MMCT
      EE(KTOT) = E(NNCT)
    3 CONTINUE
    4 CONTINUE
    5 CONTINUE
C
C     COMPUTE DIFFERENCES OF COORDINATES OF GRID POINTS IN THE MEAN PLAN
C
      X12 =-(X21*E(1) + Y21*E(4) + Z21*E(7))
      X13 =-(X31*E(1) + Y31*E(4) + Z31*E(7))
      X14 =-(X41*E(1) + Y41*E(4) + Z41*E(7))
      Y3A = (X31*E(2) + Y31*E(5) + Z31*E(8))
      Y4A = (X42*E(2) + Y42*E(5) + Z42*E(8))
      X24 = X14 - X12
      X23 = X13 - X12
      X34 = X14 - X13
      Y34 = Y3A - Y4A
C
C
C     COMPUTE LENGTHS OF SIDES OF ELEMENT IN THE MEAN PLANE
C
      LA = ABS(X12)
      LB = SQRT(X23**2 + Y3A**2)
      LC = SQRT(X34**2 + Y34**2)
      LD = SQRT(X14**2 + Y4A**2)
C
C     COMPUTE THE CHARACTERISTIC ANGLES OF ELEMENT IN THE MEAN PLANE
C
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
C
      DLT1  = CTH31*CTH32 - STH31*STH32
      DLT2  = CTH42*CTH41 + STH41*STH42
      LDD2  = LD*DLT2
      LBD1  = LB*DLT1
      LCD1  = LC*DLT1
      LCD2  = LC*DLT2
C
C                                                  *       *
C     COMPUTE THE INTERSECTION OF THE DIAGONALS(ETA  AND XI ) OF
C     THE ELEMENTS IN THE MEAN PLANE
C
      TOL  = 1.0E-3*(-X12)
      IF (NBPW .GE. 60) TOL  = 1.0E-5*(-X12)
      TOL2 = 1.0E-3*X12*X12
      IF (NBPW .GE. 60) TOL2 = 1.0E-5*X12*X12
      IF (ABS(X34+X12).GT.TOL .OR. ABS(Y34).GT.TOL) GO TO 6
      ETAS =.5
      XIS  =.5
      GO TO 16
    6 IF (ABS(X24).LT.TOL .OR. ABS(X13).LT.TOL) GO TO 7
      XSTAR = (Y4A*X13*X12)/((Y3A*X24)-(Y4A*X13))
      YSTAR = (-Y4A/X24)*(XSTAR+X12)
      GO TO 9
    7 IF (ABS(X13) .GT. TOL) GO TO 8
      XSTAR = -X13
      YSTAR = (-Y4A/X24)*X12
      GO TO 9
    8 XSTAR = -X12
      YSTAR = (Y3A/X13)*X12
    9 IF (ABS(X34+X12) .LT. TOL) GO TO 13
      C1 = Y34*XSTAR - YSTAR*(X34+X12)
      A2 =-Y4A*X23 + Y3A*X14
      B2 =-Y4A*X12 + C1
      IF (ABS(A2) .LE. TOL2) GO TO 10
      TEMP2 = SQRT(B2**2-(4.*A2*X12*YSTAR))/(2.*A2)
      TEMP1 =-B2/(2.*A2)
      ETAS  = TEMP1 - TEMP2
      IF (ETAS.LE.0. .OR. ETAS.GE.1.) ETAS = TEMP1 + TEMP2
      GO TO 11
   10 ETAS = (-X12*YSTAR)/B2
   11 IF (ABS(Y34) .LT. TOL) GO TO 12
      XIS = (-C1 + ((Y4A*X23) - (Y3A*X14))*ETAS)/(Y34*X12)
      GO TO 16
   12 XIS = (XSTAR + (X14*ETAS))/((ETAS*(X34+X12)) - X12)
      GO TO 16
   13 A3 = -X14*Y34
      B3 =  X12*Y4A - Y34*XSTAR
      IF (ABS(A3) .LE. TOL2) GO TO 14
      TEMP2 = SQRT(B3**2 + (4.*A3*X12*YSTAR))/(2.*A3)
      TEMP1 = -B3/(2.*A3)
      ETAS  = TEMP1 - TEMP2
      IF (ETAS.LE.0. .OR. ETAS.GE.1.) ETAS = TEMP1 + TEMP2
      GO TO 15
   14 ETAS = (X12*YSTAR)/B3
   15 XIS  = (YSTAR - (Y4A*ETAS))/(Y34*ETAS)
C
C     SET UP THE (12X12) TRANSFORMATION MATRIX B BETWEEN THE MEAN PLANE
C                        AND ACTUAL GRID POINTS
C
   16 DO 17 I = 2,92
      B(I)  = 0.
   17 CONTINUE
      B(1)  = 1.
      B(10) = 1.
      B(17) =-HH/LA
      B(18) =-HH/(LD*STH1) + ((HH*CTH1)/(LA*STH1))
      B(19) = HH/LA
      B(20) = (HH*CTH2)/(LA*STH2)
      B(23) = (HH*CTH42)/LDD2
      B(24) = (HH*STH42)/LDD2
      B(27) = 1.
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
      B(79) = 1.
      B(88) = 1.
      B(90) = HH/(LD*STH1)
      B(93) = (-HH*CTH32)/LCD1
      B(94) = (-HH*STH32)/LCD1
      B(95) = HH*((-CTH42/LDD2) + (STH41/LCD2))
      B(96) = HH*((-STH42/LDD2) - (CTH41/LCD2))
      DO 18 I = 1,24
      A(I)  = 0.
   18 CONTINUE
C                                                     T
C     COMPUTE TRANSFORMED MATRIX OF STIFFNESSES  G = P  * G * P
C
      THETA  = ANGLE*DEGRA
      SINTH  = SIN(THETA)
      COSTH  = COS(THETA)
      IF (ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0E0
      MATID  = MATID1
      INFLAG = 2
      ELTEMP = ECPT(26)
      CALL MAT (ECPT(1))
C
C     STORE INTO G MATRIX
C
      G(1) = G11
      G(2) = G12
      G(3) = G13
      G(4) = G12
      G(5) = G22
      G(6) = G23
      G(7) = G13
      G(8) = G23
      G(9) = G33
C
C     COMPUTE MATRIX A TO RELATE DISPLACEMENTS TO STRAINS
C
      AJ    = (-Y4A*X12) + (-Y34*X12*XIS) + ETAS*((-Y4A*X23)+(Y3A*X14))
      A(1)  = (-Y4A + (Y3A*ETAS) - (Y34*XIS))/AJ
      A(3)  = ( Y4A - (Y4A*ETAS) + (Y34*XIS))/AJ
      A(5)  = ( Y4A*ETAS)/AJ
      A(7)  = (-Y3A*ETAS)/AJ
      A(10) = (-X24 + (X23*ETAS) + (X34*XIS))/AJ
      A(12) = ( X14 - (X14*ETAS) - (X34*XIS))/AJ
      A(14) = ((X14*ETAS) - (X12*XIS))/AJ
      A(16) = (-X12 - (X23*ETAS) + (X12*XIS))/AJ
      A(17) = (-X24 + (X23*ETAS) + (X34*XIS))/AJ
      A(18) = (-Y4A + (Y3A*ETAS) - (Y34*XIS))/AJ
      A(19) = ( X14 - (X14*ETAS) - (X34*XIS))/AJ
      A(20) = ( Y4A - (Y4A*ETAS) + (Y34*XIS))/AJ
      A(21) = ((X14*ETAS) - (X12*XIS))/AJ
      A(22) = (Y4A*ETAS)/AJ
      A(23) = (-X12 - (X23*ETAS) + (X12*XIS))/AJ
      A(24) = (-Y3A*ETAS)/AJ
C
C                          T    T
C     COMPUTE S = G * A * B  * E
C
      CALL GMMATS (B(1),12,8,1,EE(1),12,12,1,TEMPAR(1))
      CALL GMMATS (A(1),3,8,0,TEMPAR(1),8,12,0,TEMPAR(100))
      CALL GMMATS (G(1),3,3,0,TEMPAR(100),3,12,0,TEMPAR(1))
      DO 27 L = 1,4
      DO 19 N = 2,5
      IF (NECPT(N) .NE. NGRID(L)) GO TO 19
      KA = 4*N + 2
      GO TO 20
   19 CONTINUE
      CALL MESAGE (-30,34,ECPT(1))
   20 IF (NECPT(KA) .EQ. 0) GO TO 21
      CALL TRANSS (NECPT(KA),TI)
      GO TO 23
   21 DO 22 II = 1,9
      TI(II) = 0.
   22 CONTINUE
      TI(1) = 1.
      TI(5) = 1.
      TI(9) = 1.
   23 LCNT  = 3*(L-1)
      IROWCT= -12
      NN    = 0
      DO 25 JJ = 1,3
      IROWCT= IROWCT + 12
      DO 24 KK = 1,3
      NN   = NN + 1
      KTOT = KK + IROWCT + LCNT
      NN49 = NN + 49
      TEMPAR(NN49) = TEMPAR(KTOT)
   24 CONTINUE
   25 CONTINUE
      CALL GMMATS (TEMPAR(50),3,3,0,TI,3,3,0,TEMPAR(60))
C
C                                                          TH
C     MATRICES S  RELATE DISPLACEMENTS TO STRESSES AT THE I   GRIDPOINT
C               I
C
      DO 26 IL = 1,9
      KTOT = IL + 9*L
      IL59 = IL + 59
      PH1OUT(KTOT) = TEMPAR(IL59)
   26 CONTINUE
   27 CONTINUE
      CALL GMMATS (G(1),3,3,0,ALPHAS(1),3,1,0,PH1OUT(7))
      PH1OUT(1) = ECPT(1)
      PH1OUT(2) = ECPT(2)
      PH1OUT(3) = ECPT(3)
      PH1OUT(4) = ECPT(4)
      PH1OUT(5) = ECPT(5)
      PH1OUT(6) = TSUB0
      RETURN
      END
