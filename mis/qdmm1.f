      SUBROUTINE QDMM1 (TBAR,PG)
C
C     QUADRILATERAL MEMBRANE ELEMENT
C
C     CALLS FROM THIS ROUTINE ARE MADE TO
C           MAT    - MATERIAL DATA ROUTINE
C           MESAGE - ERROR MESSAGE WRITER
C           BASGLB - TRANSFER COORDINATES FROM BASIC TO GLOBAL
C           GMMATS - SINGLE PRECISION MATRIX MULTIPLY AND TRANSPOSE
C           TRANSS - SINGLE PRECISION TRANSFORMATION SUPPLIER
C
C     ECPT LIST
C                                                    IN THIS
C      ECPT       DESCRIPTION                        ROUTINE   TYPE
C     ========   =================================   ========  =======
C     ECPT( 1) = ELEMENT ID                          NECPT(1)  INTEGER
C     ECPT( 2)   GRID POINT A                        NGRID(1)  INTEGER
C     ECPT( 3)   GRID POINT B                        NGRID(2)  INTEGER
C     ECPT( 4)   GRID POINT C                        NGRID(3)  INTEGER
C     ECPT( 5)   GRID POINT D                        NGRID(4)  INTEGER
C     ECPT( 6) = THETA = ANGLE OF MATERIAL           ANGLE      REAL
C     ECPT( 7)   MATERIAL ID                         MATID1    INTEGER
C     ECPT( 8) = THICKNESS                           T          REAL
C     ECPT( 9) = NON-STRUCTURAL MASS                 FMU        REAL
C     ECPT(10)   COORD. SYSTEM ID 1                  NECPT(10) INTEGER
C     ECPT(11) = X1                                   X1        REAL
C     ECPT(12) = Y1                                   Y1        REAL
C     ECPT(13) = Z1                                   Z1        REAL
C     ECPT(14)   COORD. SYSTEM ID 2                  NECPT(14) INTEGER
C     ECPT(15) = X2                                   X2        REAL
C     ECPT(16) = Y2                                   Y2        REAL
C     ECPT(17) = Z2                                   Z2        REAL
C     ECPT(18)   COORD. SYSTEM ID 3                  NECPT(18) INTEGER
C     ECPT(19) = X3                                   X3        REAL
C     ECPT(20) = Y3                                   Y3        REAL
C     ECPT(21) = Z3                                   Z3        REAL
C     ECPT(22)   COORD. SYSTEM ID 4                  NECPT(22) INTEGER
C     ECPT(23) = X4                                   X4        REAL
C     ECPT(24) = Y4                                   Y4        REAL
C     ECPT(25)   Z4                                   Z4        REAL
C
      REAL            LA,LB,LC,LD,LDD2,LBD1,LCD1,LCD2,MAGI,MAGJ,MAGK
      DIMENSION       ECPT(26),PG(1),G(9),E(9)
      COMMON /CONDAS/ CONSTS(5)
      COMMON /TRIMEX/ NECPT(1),NGRID(4),ANGLE,MATID1,T,FMU,
     1                DUMMY1,X1,Y1,Z1,DUMMY2,X2,Y2,Z2,DUMMY3,X3,Y3,Z3,
     2                DUMMY4,X4,Y4,Z4
      COMMON /SSGWRK/ EE(144),B(96),TEMPAR(24),C(24),TI(9)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ G11,G12,G13,G22,G23,G33,RHO,ALPHAS(3),
     1                T SUB 0,G SUB E,SIGTEN,SIGCOM,SIGSHE,
     2                G2X211,G2X212,G2X222
      EQUIVALENCE     (CONSTS(4),DEGRA),(ECPT(1),NECPT(1))
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
C     COMPUTE THE ELEMENTS OF THE (3X3) E MATRIX
C
      PK1  = Y31*Z42 - Z31*Y42
      PK2  = Z31*X42 - X31*Z42
      PK3  = X31*Y42 - Y31*X42
      MAGK = SQRT(PK1**2 + PK2**2 + PK3**2)
      IF (MAGK .GT. 1.0E-06) GO TO 40
      CALL MESAGE (-30,32,ECPT(1))
   40 PK1  = PK1/MAGK
      PK2  = PK2/MAGK
      PK3  = PK3/MAGK
C
C     HH IS THE MEASURE OF NON-PLANARITY OF THE ELEMENT
C
      HH   = X21*PK1 + Y21*PK2 + Z21*PK3
      PI1  = X21 - HH*PK1
      PI2  = Y21 - HH*PK2
      PI3  = Z21 - HH*PK3
      MAGI = SQRT(PI1**2 + PI2**2 + PI3**2)
      IF (MAGI.GT.1.0E-06) GO TO 41
      CALL MESAGE (-30,31,ECPT(1))
   41 PI1  = PI1/MAGI
      PI2  = PI2/MAGI
      PI3  = PI3/MAGI
      HH   =-HH/2.
C
C     THIS SIGN CHANGE MADE BECAUSE SIGN OF H AS DEFINED ON
C     PAGE 4.87-105 OF PROGRAMMERS MANUAL IS WRONG
C
      PJ1  = PK2*PI3 - PK3*PI2
      PJ2  = PK3*PI1 - PK1*PI3
      PJ3  = PK1*PI2 - PK2*PI1
      MAGJ = SQRT(PJ1**2 + PJ2**2 + PJ3**2)
      PJ1  = PJ1/MAGJ
      PJ2  = PJ2/MAGJ
      PJ3  = PJ3/MAGJ
C
C     INSERT ELEMENTS INTO THE (3X3) E MATRIX
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
      X12 = -X21*E(1) - Y21*E(4) - Z21*E(7)
      X13 = -X31*E(1) - Y31*E(4) - Z31*E(7)
      X14 = -X41*E(1) - Y41*E(4) - Z41*E(7)
      Y3A =  X31*E(2) + Y31*E(5) + Z31*E(8)
      Y4A =  X42*E(2) + Y42*E(5) + Z42*E(8)
      X24 =  X14 - X12
      X23 =  X13 - X12
      X34 =  X14 - X13
      Y34 =  Y3A - Y4A
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
      DLT1  = CTH31*CTH32 - STH31*STH32
      DLT2  = CTH42*CTH41 + STH41*STH42
      LDD2  = LD*DLT2
      LBD1  = LB*DLT1
      LCD1  = LC*DLT1
      LCD2  = LC*DLT2
C
C     SET UP THE (12X12) TRANSFORMATION MATRIX B BETWEEN THE MEAN PLANE
C                        AND ACTUAL GRID POINTS
C
      DO 6 I = 1,96
    6 B( I) = 0.
      B( 1) = 1.
      B(10) = 1.
      B(17) =-HH/LA
      B(18) =-HH/(LD*STH1)+((HH*CTH1)/(LA*STH1))
      B(19) = HH/LA
      B(20) = (HH*CTH2)/(LA*STH2)
      B(23) = (HH*CTH42)/LDD2
      B(24) = (HH*STH42)/LDD2
      B(27) = 1.
      B(36) = 1.
      B(41) =-B(17)
      B(42) =-(HH*CTH1)/(LA*STH1)
      B(43) = B(17)
      B(44) = ((-HH*CTH2)/(LA*STH2))+(HH/(LB*STH2))
      B(45) =-(HH*STH31)/LBD1
      B(46) =-(HH*CTH31)/LBD1
      B(53) = 1.
      B(62) = 1.
      B(68) =-HH/(LB*STH2)
      B(69) = HH*((STH31/LBD1)+(CTH32/LCD1))
      B(70) = HH*((CTH31/LBD1)+(STH32/LCD1))
      B(71) =-(HH*STH41)/LCD2
      B(72) = (HH*CTH41)/LCD2
      B(79) = 1.
      B(88) = 1.
      B(90) = HH/(LD*STH1)
      B(93) =-(HH*CTH32)/LCD1
      B(94) =-(HH*STH32)/LCD1
      B(95) = HH*((-CTH42/LDD2)+(STH41/LCD2))
      B(96) = HH*((-STH42/LDD2)-(CTH41/LCD2))
      H     = ECPT( 8)
      ELTEMP= ECPT(26)
C
C     SET UP (3X8) C MATRIX (SEE FMMS)
C
      C( 1) =-(H*Y4A)/2.
      C( 2) = 0.
      C( 3) =-(H*X24)/2.
      C( 4) = 0.
      C( 5) =-(H*X24)/2.
      C( 6) =-(H*Y4A)/2.
      C( 7) = (H*Y3A)/2.
      C( 8) = 0.
      C( 9) = (H*X13)/2.
      C(10) = 0.
      C(11) = (H*X13)/2.
      C(12) = (H*Y3A)/2.
      C(13) = (H*Y4A)/2.
      C(14) = 0.
      C(15) = (H*X24)/2.
      C(16) = 0.
      C(17) = (H*X24)/2.
      C(18) = (H*Y4A)/2.
      C(19) =-(H*Y3A)/2.
      C(20) = 0.
      C(21) =-(H*X13)/2.
      C(22) = 0.
      C(23) =-(H*X13)/2.
      C(24) =-(H*Y3A)/2.
      THETA = ANGLE*DEGRA
      SINTH = SIN(THETA)
      COSTH = COS(THETA)
      IF (ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0
      MATID  = MATID1
      INFLAG = 2
C                                                     T
C     COMPUTE TRANSFORMED MATRIX OF STIFFNESSES  G = P  * G * P
C
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
C                   T                            -
C     COMPUTE PG = T  * E * B * C * G * ALPHA * (T - T )
C                                                     0
C
      TEMP = TBAR - TSUB0
      TEMPAR(1) = ALPHAS(1)*TEMP
      TEMPAR(2) = ALPHAS(2)*TEMP
      TEMPAR(3) = ALPHAS(3)*TEMP
      CALL GMMATS (G(1),3,3,0,TEMPAR(1),3,1,0,TEMPAR(13))
      CALL GMMATS (C(1),8,3,0,TEMPAR(13),3,1,0,TEMPAR(1))
      CALL GMMATS (B(1),12,8,0,TEMPAR(1),8,1,0,TEMPAR(13))
      CALL GMMATS (EE(1),12,12,0,TEMPAR(13),12,1,0,TEMPAR(1))
      DO 13 I = 1,4
C
C     T-SUB-I WILL BE USED BELOW ONLY IF THE PIVOT COORDINATE SYSTEM ID
C     IS NOT ZERO, OTHERWISE IT IS ASSUMED TO BE THE IDENTITY MATRIX.
C
      KA = 4*I + 6
C
C     DO WE NEED TRANSFORMATION TI
C
      ISW = 0
      JJ  = 3*I - 2
      IF (NECPT(KA) .EQ. 0) GO TO 9
      ISW = 1
      CALL BASGLB (TEMPAR(JJ),TEMPAR(20),NECPT(KA+1),NECPT(KA))
C
C     COMPUTE PG VECTOR
C
    9 DO 12 K = 1,3
      JJK = JJ + K - 1
      K19 = K + 19
      IF (ISW .EQ. 0) TEMPAR(K19) = TEMPAR(JJK)
      I1 = I + 1
      L  = NECPT(I1) + K - 1
      PG(L) = PG(L) + TEMPAR(K19)
   12 CONTINUE
   13 CONTINUE
      RETURN
      END
