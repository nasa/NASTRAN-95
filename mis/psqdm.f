       SUBROUTINE PSQDM
C  THIS SUBROUTINE IS THE DRIVER FOR THE QUAD-MEMBRANE CALCULATIONS IN
C  PLA3
C
C
C     ECPT LIST
C                                                      IN
C                                                      THIS
C       ECPT       DESCRIPTION                         ROUTINE   TYPE
C     ******************************************************************
C       ECPT( 1) = ELEMENT ID                          NECPT(1)  INTEGER
C       ECPT( 2) = GRID POINT A                        NGRID(1)  INTEGER
C       ECPT( 3) = GRID POINT B                        NGRID(2)  INTEGER
C       ECPT( 4) = GRID POINT C                        NGRID(3)  INTEGER
C       ECPT( 5) = GRID POINT D                        NGRID(4)  INTEGER
C       ECPT( 6) = THETA = ANGLE OF MATERIAL           ANGLE     REAL
C       ECPT( 7) = MATERIAL ID                         MATID     INTEGER
C       ECPT( 8) = T                                   T         REAL
C       ECPT( 9) = NON-STRUCTURAL MASS                 FMU       REAL
C       ECPT(10) = COORD. SYSTEM ID 1                  NECPT(10) INTEGER
C       ECPT(11) = X1                                  X1        REAL
C       ECPT(12) = Y1                                  Y1        REAL
C       ECPT(13) = Z1                                  Z1        REAL
C       ECPT(14) = COORD. SYSTEM ID 2                  NECPT(14) INTEGER
C       ECPT(15) = X2                                  X2        REAL
C       ECPT(16) = Y2                                  Y2        REAL
C       ECPT(17) = Z2                                  Z2        REAL
C       ECPT(18) = COORD. SYSTEM ID 3                  NECPT(18) INTEGER
C       ECPT(19) = X3                                  X3        REAL
C       ECPT(20) = Y3                                  Y3        REAL
C       ECPT(21) = Z3                                  Z3        REAL
C       ECPT(22) = COORD. SYSTEM ID 4                  NECPT(22) INTEGER
C       ECPT(23) = X4                                  X4        REAL
C       ECPT(24) = Y4                                  Y4        REAL
C       ECPT(25) = Z4                                  Z4        REAL
C       ECPT(26) = ELEMENT TEMPERATURE                 ELTEMP    REAL
C       ECPT(27) = STRAIN (MINUS ONE)                  EPS0      REAL
C       ECPT(28) = STRAIN (PRESENT)                    EPSS      REAL
C       ECPT(29) = MODULUS OF ELASTICITY               ESTAR     REAL
C       ECPT(30) = STRESS SUB X                        SIGXS     REAL
C       ECPT(31) = STRESS SUB Y                        SIGYS     REAL
C       ECPT(32) = STRESS SUB XY                       SIGXYS    REAL
C       ECPT(33) = DISPLACEMENT VECTOR   A1            UI(1)     REAL
C       ECPT(34) = DISPLACEMENT VECTOR   A2            UI(2)     REAL
C       ECPT(35) = DISPLACEMENT VECTOR   A3            UI(3)     REAL
C       ECPT(36) = DISPLACEMENT VECTOR   B1            UI(4)     REAL
C       ECPT(37) = DISPLACEMENT VECTOR   B2            UI(5)     REAL
C       ECPT(38) = DISPLACEMENT VECTOR   B3            UI(6)     REAL
C       ECPT(39) = DISPLACEMENT VECTOR   C1            UI(7)     REAL
C       ECPT(40) = DISPLACEMENT VECTOR   C2            UI(8)     REAL
C       ECPT(41) = DISPLACEMENT VECTOR   C3            UI(9)     REAL
C       ECPT(42) = DISPLACEMENT VECTOR   D1            UI(10)    REAL
C       ECPT(43) = DISPLACEMENT VECTOR   D2            UI(11)    REAL
C       ECPT(44) = DISPLACEMENT VECTOR   D3            UI(12)    REAL
C
C     ******************************************************************
      REAL NU
C
      DIMENSION NECPT(26), NECPTS(26)
C
      COMMON /PLA32E/ ECPT(26),EPS0,EPSS,ESTAR,SIGXS,SIGYS,SIGXYS,
     1              UI(12),DUMMY(56)
      COMMON /PLA3ES/ ECPTSA(100),PH1OUT(200)
      COMMON /PLA3UV/  IVEC, Z(24)
C
C SCRATCH BLOCK  325 CELLS
C
      COMMON /PLA32S/S(3),DUM(297),TAU0 ,TAU1 ,TAU2 ,F,SX,SY,DEPS,DEPSS,
     1                EPS1,EPS2, DUM1,IDUM2,IDUM3(3,3)
     2,              EXTRA(4)
      COMMON /MATIN/ MATID,INFLAG,ELTEMP,PLAARG,SINTH,COSTH
      COMMON /MATOUT/ G11,G12,G13,G22,G23,G33
      COMMON /PLA32C/ GAMMA, GAMMAS, IPASS
      COMMON /PLAGP/  GP(9), MIDGP   , ELID
C
      EQUIVALENCE (NECPT(7),MATID1) , (ECPT(1),NECPT(1)) , (G11,PLAANS),
     1            (G13,NU)   , (G11,ESUB0)
     2,  (NECPTS(1),ECPTSA(1))
     3,   (G12,NIROF)
C
C SETUP GP MATRIX FOR PLAMAT
C
      ELID = ECPT(1)
      MIDGP = MATID1
      DO 10 I=1,9
   10 GP(I)=0.0
      TAU0  = SQRT(SIGXS**2 - SIGXS*SIGYS + SIGYS**2 + 3.0*SIGXYS**2)
      IF(ESTAR .EQ. 0.0) GO TO 50
      IF(IPASS .NE. 1  ) GO TO 20
      MATID = MATID1
      COSTH = 1.0
      SINTH = 0.0E0
      INFLAG= 2
C
      CALL MAT(ECPT(1))
C
      GP(1) = G11
      GP(2) = G12
      GP(3) = G13
      GP(4) = G12
      GP(5) = G22
      GP(6) = G23
      GP(7) = G13
      GP(8) = G23
      GP(9) = G33
      GO TO 50
   20 IF(TAU0  .EQ. 0.0) GO TO 50
      MATID = MATID1
      INFLAG = 1
C
      CALL MAT(ECPT(1))
C
      F =   9.0*(ESUB0 - ESTAR) / (4.0 * TAU0**2 * ESTAR)
      SX = (2.0*SIGXS - SIGYS)/ 3.0
      SY = (2.0*SIGYS - SIGXS)/ 3.0
      GP(1) = (1.0+SX**2*F) / ESUB0
      GP(2) = (-NU+SX*SY*F) / ESUB0
      GP(3) = (2.0*SIGXYS*SX*F) / ESUB0
      GP(4) = GP(2)
      GP(5) = (1.0+SY**2*F) / ESUB0
      GP(6) = (2.0*SIGXYS*SY*F) / ESUB0
      GP(7) = GP(3)
      GP(8) = GP(6)
      GP(9) = (2.0*(1.0+NU) + 4.0*F*SIGXYS**2) / ESUB0
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      IDUM2 = -1
      CALL INVERS(3,GP,3,0,0,DUM1,IDUM2,IDUM3)
C
C CHECK SINGULARITY
C
      IF( IDUM2 .EQ. 2) CALL MESAGE(-30,38,ECPT(1))
C
C CALCULATE PHASE I STRESSES
C
   50 DO 30 I = 1,32
   30 ECPTSA(I) = ECPT(I)
      NECPTS(2) = 1
      NECPTS(3) = 4
      NECPTS(4) = 7
      NECPTS(5) = 10
C
      CALL PSQDM1
C
C
C CALCULATE PHASE II STRESSES
C
      IVEC = 1
      DO 60 I = 1,24
   60 Z(I) = UI(I)
      DO 70 I = 1,200
   70 ECPTSA(I) = PH1OUT(I)
      S(1) = SIGXS
      S(2) = SIGYS
      S(3) = SIGXYS
C
      CALL PSTRQ2(2)
C
C
C  UPDATE ECPT FOR STRESSES
C
      SIGXS = S(1)
      SIGYS = S(2)
      SIGXYS = S(3)
      TAU1  = SQRT(SIGXS**2 - SIGXS*SIGYS + SIGYS**2 + 3.0*SIGXYS**2)
      MATID= MATID1
      INFLAG = 8
      PLAARG = TAU1
C
      CALL MAT(ECPT(1))
C
C
C TEST FOR TAU 1 OUTSIDE THE RANGE OF FUNCTION
C
      IF ( NIROF . EQ. 1 ) GO TO 80
C
C RETURNS EPS SUB 1 GIVEN TAU1
C
      EPS1 = PLAANS
      DEPS = EPS1 - EPSS
      DEPSS= EPSS - EPS0
      EPS2 = EPS1 + GAMMA * DEPS
      INFLAG=6
      PLAARG = EPS2
C
      CALL MAT(ECPT(1))
C
C RETURNS  TAU2 GIVEN EPS2
C
      TAU2  = PLAANS
      ESTAR = 0.0
      IF( (EPS2 - EPS1) .NE. 0.0) ESTAR = (TAU2 - TAU1) / (EPS2-EPS1)
      EPS0  = EPSS
      EPSS  = EPS1
      RETURN
C
   80 ESTAR = 0.0
      RETURN
      END
