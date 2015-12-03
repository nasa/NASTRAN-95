      SUBROUTINE SROD1
C*****
C THIS ROUTINE IS PHASE I OF STRESS DATA RECOVERY FOR THE ROD.
C*****
C
C
C
      DIMENSION          IECPT(13)
C
C
C
C
C INPUT AND OUTPUT BLOCK
C
      COMMON   /SDR2X5/
     A                   ECPT(17)           ,DUMMY1(83),
     1                   IELID              ,ISILNO(2)
     2,                  SAT(3)             ,SBT(3)
     3,                  SAR(3)             ,SBR(3)
     4,                  ST                 ,SDELTA
     5,                  AREA               ,FJOVRC
     6,                  T SUBC 0           ,SIGMAT
     7,                  SIGMAC             ,SIGMAS
     8,                  SIGVEC(77)         ,FORVEC(25)
C
C SCRATCH BLOCK
C
      COMMON   /SDR2X6/
     1                   XN(6)              ,TI(9)
     2,                  XL                 ,EOVERL
     3,                  IBASE
C
C INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
C
      COMMON   /MATIN/
     1                   MATIDC             ,MATFLG
     2,                  ELTEMP             ,STRESS
     3,                  SINTH              ,COSTH
C
C
C
      COMMON   /MATOUT/
     1                   E                  ,G
     2,                  NU                 ,RHO
     3,                  ALPHA              ,T SUB 0
     4,                  GSUBE              ,SIGT
     5,                  SIGC               ,SIGS
C
C
C
      EQUIVALENCE        (IECPT(1),ECPT(1))
C
C CALL MAT TO GET MATERIAL PROPERTIES
C
      MATIDC = IECPT(4)
      MATFLG = 1
      ELTEMP = ECPT(17)
      CALL MAT (IECPT(1))
C
C SET UP VECTOR ALONG THE ROD, COMPUTE LENGTH AND NORMALIZE
C
      XN(1) = ECPT(10) - ECPT(14)
      XN(2) = ECPT(11) - ECPT(15)
      XN(3) = ECPT(12) - ECPT(16)
      XL =  XN(1)**2  +  XN(2)**2  +  XN(3)**2
      XL =  SQRT(XL)
      XN(1) = XN(1) / XL
      XN(2) = XN(2) / XL
      XN(3) = XN(3) / XL
      EOVERL = E / XL
      GCOVRL = G * ECPT(6) / XL
      IBASE = 0
C
C TRANSFORM XN VECTOR IF POINT A IS NOT IN BASIC COORDINATES.
C
      IF (IECPT(9) .EQ. 0) GO TO 10
      IBASE = 3
      CALL TRANSS (IECPT(9),TI)
      CALL GMMATS (XN(1),3,1,1, TI(1),3,3,0, XN(4) )
   10 SAT(1) = XN(IBASE +1) * EOVERL
      SAT(2) = XN(IBASE +2) * EOVERL
      SAT(3) = XN(IBASE +3) * EOVERL
      SAR(1) = XN(IBASE +1) * GCOVRL
      SAR(2) = XN(IBASE +2) * GCOVRL
      SAR(3) = XN(IBASE +3) * GCOVRL
C
C TRANSFORM XN VECTOR IF POINT B IS NOT IN BASIC COORDINATES.
C
      IBASE = 0
      IF (IECPT(13) .EQ. 0) GO TO 20
      IBASE = 3
      CALL TRANSS (IECPT(13),TI)
      CALL GMMATS (XN(1),3,1,1, TI(1),3,3,0, XN(4) )
   20 SBT(1) = - XN(IBASE+1) * EOVERL
      SBT(2) = - XN(IBASE+2) * EOVERL
      SBT(3) = - XN(IBASE+3) * EOVERL
      SBR(1) = - XN(IBASE+1) * GCOVRL
      SBR(2) = - XN(IBASE+2) * GCOVRL
      SBR(3) = - XN(IBASE+3) * GCOVRL
C
C FILL REMAINDER OF OUTPUT BLOCK
C
      ST     = - ALPHA * E
      SDELTA = - EOVERL
      AREA   =   ECPT(5)
      IF(ECPT(6)) 30,40,30
   30 FJOVRC = ECPT(7) / ECPT(6)
      GO TO 50
   40 FJOVRC = 0.0
   50 TSUBC0 = TSUB0
      SIGMAT = SIGT
      SIGMAC = SIGC
      SIGMAS = SIGS
      IELID = IECPT(1)
      ISILNO(1) = IECPT(2)
      ISILNO(2) = IECPT(3)
      RETURN
      END
