      SUBROUTINE PSROD
C*****
C THIS ROUTINE COMPUTES STRESSES AND FORCES FOR THE ROD ELEMENT FOR THE
C PLA3 FUNCTIONAL MODULE.
C*****
C
C                        E C P T  F O R  T H E  R O D
C
C                                                                CARD
C                                                 TYPE   TABLE   TYPE
C ECPT( 1)ELEMENT ID.                               I     ECT    CROD
C ECPT( 2)SCALAR INDEX NUMBER FOR GRID POINT A      I     ECT    CROD
C ECPT( 3)SCALAR INDEX NUMBER FOR GRID POINT B      I     ECT    CROD
C ECPT( 4)MATERIAL ID.                              I     EPT    PROD
C ECPT( 5)AREA  (A)                                 R     EPT    PROD
C ECPT( 6)POLAR MOMENT OF INERTIA (J)               R     EPT    PROD
C ECPT( 7) TORSIONAL STRESS COEFF (C)                R    EPT    PROD
C ECPT( 8) NON-STRUCTRAL MASS (MU)                   R    EPT    PROD
C ECPT( 9) COOR. SYS. ID. NO. FOR GRID POINT A       I   BGPDT   GRID
C ECPT(10) X-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
C ECPT(11) Y-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
C ECPT(12) Z-COORDINATE OF GRID PT. A (IN BASIC COOR)R   BGPDT
C ECPT(13) COOR. SYS. ID. NO. FOR GRID POINT B       I   BGPDT
C ECPT(14) X-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
C ECPT(15) Y-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
C ECPT(16) Z-COORDINATE OF GRID PT. B (IN BASIC COOR)R   BGPDT
C ECPT(17) ELEMENT TEMPERATURE
C ECPT(18) PREVIOUS STRAIN VALUE, ONCE REMOVED (EPS STAR SUB 0)
C ECPT(19) PREVIOUS STRAIN VALUE  (EPS STAR)
C ECPT(20) PREVIOUSLY COMPUTED VALUE OF MODULUS OF ELASTICITY (ESTAR)
C ECPT(21) PREVIOUSLY COMPUTED TORSIONAL MOMENT (TSTAR)
C ECPT(22) INCREMENTAL DISPLACEMENT VECTOR FOR GRID POINT A
C ECPT(23)                       ...
C ECPT(24)                       ...
C ECPT(25)                       ...
C ECPT(26)                       ...
C ECPT(27)                       ...
C ECPT(28) INCREMENTAL DISPLACEMENT VECTOR FOR GRID POINT B
C ECPT(29)                       ...
C ECPT(30)                       ...
C ECPT(31)                       ...
C ECPT(32)                       ...
C ECPT(33)                       ...
C
C
C
      DIMENSION
     1                   ECPT(100)          ,IECPT(100)
     2,                  XN(3)              ,UA(9)
     3,                  UB(9)              ,DIFF(3)
     4,                  TA(9)              ,TB(9)
C
C EST (ECPT) COMMON BLOCK
C
      COMMON   /PLA32E/
     1                   ECPT
C
C SCRATCH BLOCK FOR VARIABLES LOCAL TO PLA3 ELEMENT ROUTINES.
C
      COMMON   /PLA32S/
     1                   XL                 ,XN
     2,                  UA                 ,UB
     3,                  TA                 ,TB
     4,                  DIFF
C
C PLA32 COMMUNICATION BLOCK
C
      COMMON   /PLA32C/  GAMMA              ,GAMMAS
C
C OUTPUT BLOCK FOR ELEMENT STRESSES
C
      COMMON   /SOUT  /
     1                   JSELID             ,SIGMA
     2,                  SMSIG              ,TAU
     3,                  SMTAU
C
C INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
C
      COMMON   /MATIN/
     1                   MATIDC             ,MATFLG
     2,                  TEMDUM             ,PLAARG
     3,                  DUM2(2)
C
C
C
      COMMON   /MATOUT/
     1                   E SUB 0            ,G SUB 0
     2,                  MATDUM(5)          ,SIGMAT
     3,                  SIGMAC             ,SIGMAS
C
C
C
      EQUIVALENCE
     1                   (IECPT(1),ECPT(1)) ,(E SUB 0,PLAANS)
     2,                  (SMSIG,MSSIG)      ,(SMTAU,MSTAU)
C
C CALL MAT ROUTINE TO GET MATERIAL PROPERTIES AND STORE IN LOCAL NAMES.
C
      MATIDC = IECPT(4)
      MATFLG = 1
      CALL MAT (IECPT(1))
      E SUB 0 L = E SUB 0
      G SUB 0 L = G SUB 0
C
C SET UP VECTOR ALONG THE ROD, COMPUTE LENGTH AND NORMALIZE
C
      XN(1) = ECPT(10) - ECPT(14)
      XN(2) = ECPT(11) - ECPT(15)
      XN(3) = ECPT(12) - ECPT(16)
      XL    =  XN(1)**2 + XN(2)**2 + XN(3)**2
      XL    = SQRT (XL)
      XN(1) = XN(1) / XL
      XN(2) = XN(2) / XL
      XN(3) = XN(3) / XL
C
C STORE DISPLACEMENT VECTORS IN LOCAL VARIABLES
C
      DO 10 I = 1,6
      UA(I) = ECPT(I+21)
   10 UB(I) = ECPT(I+27)
C
C TRANSFORM DISPLACEMENT VECTOR TRANSLATIONAL COMPONENTS IF NECESSARY
C
      IBASEA = 0
      IF (IECPT(9) .EQ. 0) GO TO 20
      IBASEA = 6
      CALL TRANSS (IECPT(9),TA)
      CALL GMMATS (TA,3,3,0, UA(1),3,1,0, UA(7))
   20 IBASEB = 0
      IF (IECPT(13) .EQ. 0) GO TO 30
      IBASEB = 6
      CALL TRANSS (IECPT(13),TB)
      CALL GMMATS (TB,3,3,0, UB(1),3,1,0, UB(7))
C
C FORM DIFFERENCE VECTOR, DOT PRODUCT AND INCREMENT OF STRAIN
C
   30 DIFF(1) = UA(IBASEA+1) - UB(IBASEB+1)
      DIFF(2) = UA(IBASEA+2) - UB(IBASEB+2)
      DIFF(3) = UA(IBASEA+3) - UB(IBASEB+3)
      CALL GMMATS (XN,3,1,1, DIFF,3,1,0, TERM)
      DEPS1 = TERM / XL
      EPSIN2 = ECPT(19)
      EPSIN1 = ECPT(18)
      DEPS2 = EPSIN2 - EPSIN1
C
C COMPUTE EPS1 AND EPS2 AND FETCH VIA MAT STRESSES SIGMA1 AND SIGMA2
C
      EPS1 = EPSIN2 + DEPS1
      EPS2 = EPSIN2  +  (DEPS1+GAMMAS**2*DEPS2)*(GAMMA+1.0)/(GAMMAS+1.0)
     1     + GAMMAS*(DEPS1-GAMMAS*DEPS2)*(GAMMA+1.0)**2 / (GAMMAS+1.0)
      MATFLG = 6
      PLAARG = EPS1
      CALL MAT (IECPT(1))
      SIGMA1 = PLAANS
      PLAARG = EPS2
      CALL MAT (IECPT(1))
      SIGMA2 = PLAANS
      IF (EPS1 .EQ. EPS2) GO TO 42
      E = (SIGMA2 - SIGMA1) / (EPS2 - EPS1)
      GO TO 44
   42 E = ECPT(20)
   44 G = ECPT(20) * G SUB 0 L / E SUB 0 L
C
C COMPUTE STRESSES
C
      ISELID = IECPT(1)
      SIGMA  = SIGMA1
      P      =  ECPT(5) * SIGMA1
C
C TRANSFORM DISPLACEMENT VECTOR ROTATIONAL DISPLACEMENTS IF NECESSARY.
C
      IBASEA = 3
      IF (IECPT(9) .EQ. 0) GO TO 60
      CALL GMMATS (TA,3,3,0, UA(4),3,1,0, UA(7))
      IBASEA = 6
   60 IBASEB = 3
      IF (IECPT(13) .EQ. 0) GO TO 70
      IBASEB = 6
      CALL GMMATS (TB,3,3,0, UB(4),3,1,0, UB(7))
   70 DIFF(1) = UA(IBASEA+1) - UB(IBASEB+1)
      DIFF(2) = UA(IBASEA+2) - UB(IBASEB+2)
      DIFF(3) = UA(IBASEA+3) - UB(IBASEB+3)
      CALL GMMATS (XN,3,1,1, DIFF,3,1,0, TERM)
      T = ECPT(6) * G * TERM / XL  +  ECPT(21)
      IF (ECPT(6) .EQ. 0.0) GO TO 80
      TAU = ECPT(7) * T / ECPT(6)
      GO TO 90
   80 TAU = 0.0
C
C COMPUTE MARGIN OF SAFETY IN EXTENSION
C
   90 IF(SIGMA.LE.0.0)GO TO 101
      IF(SIGMAT.LE.0.0)GO TO 102
      SMSIG=SIGMAT/SIGMA-1.0
      GO TO 180
  101 IF(SIGMA.NE.0.0) GO TO 103
      GO TO 102
  103 SIGMAC=-ABS(SIGMAC)
      SMSIG=SIGMAC/SIGMA - 1.0
      GO TO 180
  102 MSSIG=1
C
C     COMPUTE MARGIN OF SAFETY IN TORSION
C
  180 IF(SIGMAS.LE.0.0) GO TO 190
      IF(TAU.EQ.0.0)GO TO 190
      SMTAU= SIGMAS/ABS(TAU) - 1.0
      GO TO 210
  190 MSTAU=1
  210 JSELID = IECPT(1)
C
C UPDATE EST (ECPT) ENTRY
C
      ECPT(18) = ECPT(19)
      ECPT(19) = EPS1
      ECPT(20) = E
      ECPT(21) = T
      RETURN
      END
