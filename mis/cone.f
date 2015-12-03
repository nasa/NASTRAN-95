      SUBROUTINE CONE (TI,Z)
C
C     THIS ROUTINE COMPUTES THE THERMAL LOADS ON A AXISYMMETRIC CONE
C
      REAL    I00      ,I10
      REAL    I01      ,I11      ,I21      ,I31      ,I41
      REAL    I02      ,I12      ,I22
      REAL    I03      ,I13      ,I23      ,I33
      REAL    TI(2)    ,Z(1)     ,PA(8)    ,XI(6)    ,ECPT(35)
      REAL    N2D33    ,NSP      ,NCP      ,NSPOPI
      REAL    EHT(96)  ,HUQ(100) ,HYQ(10)
      COMMON /CONDAS/   PI       ,TWOPI    ,RADEG    ,DEGRA    ,
     1                  S4PISQ
      COMMON /TRIMEX/   MECPT(35)
      COMMON /MATIN /   MATID    ,INFLAG   ,TEMP     ,STRESS   ,SINTH  ,
     1                  COSTH
      COMMON /MATOUT/   G11,G12  ,G13,G22  ,G23,G33  ,RHO,ALPH1,ALPH2  ,
     1                  ALPH3    ,TSUB0    ,GSUBE    ,SIGTEN   ,SIGCOM ,
     2                  SIGSHE   ,G2X211   ,G2X212   ,G2X222
      EQUIVALENCE      (ECPT( 1) ,MECPT(1)),(ECPT( 9), TS    )
      EQUIVALENCE      (ECPT(28) ,RA      ),(ECPT(32), RB    )
      EQUIVALENCE      (ECPT(29) ,ZA      ),(ECPT(33), ZB    )
      EQUIVALENCE      (ECPT( 6) ,MATID2  ),(ECPT( 8), MATID3)
      EQUIVALENCE      (GSHEAR   ,G12     )
      DATA    ONE   /  1.0   /
C
C     DEFINITION OF VARIABLES
C
C        ECPT  ENTRIES FOR CONE
C
C     ECPT(1)  INTEGER   ELEMENT ID = 1000*ELID + HARMONIC
C     ECPT(2)  INTEGER   SIL  A
C     ECPT(3)  INTEGER   SIL  B
C     ECPT(4)  INTEGER   MAT ID  1
C     ECPT(5)  REAL      T    MEMBRANE THICKNESS
C     ECPT(6)  INTEGER   MAT ID  2
C     ECPT(7)  REAL      MOMENT OF INERTIA
C     ECPT(8)  INTEGER   MAT ID 3
C     ECPT(9)  REAL      SHEAR THICKNESS
C     ECPT(10) REAL      NON -STRUCTRAL  MASS
C     ECPT(11) REAL      Z1
C     ECPT(12) REAL      Z2
C     ECPT(13) REAL      PHI 1
C     ECPT(14) REAL          2
C     ECPT(15) REAL          3
C     ECPT(16) REAL          4
C     ECPT(17) REAL          5
C     ECPT(18) REAL          6
C     ECPT(19) REAL          7
C     ECPT(20) REAL          8
C     ECPT(21) REAL          9
C     ECPT(22) REAL         10
C     ECPT(23) REAL         11
C     ECPT(24) REAL         12
C     ECPT(25) REAL         13
C     ECPT(26) REAL         14
C     ECPT(27) INTEGER   COORDINANT SYSTEM FOR POINT  A
C     ECPT(28) REAL      R   (A)
C     ECPT(29) REAL      Z   (A)
C     ECPT(30) REAL      NULL
C     ECPT(31) INTEGER   COORDINANT SYSTEM FOR POINT B
C     ECPT(32) REAL      R    (B)
C     ECPT(33) REAL      Z    (B)
C     ECPT(34) REAL      NULL
C     ECPT(35) REAL      TEMPERATURE OF MATERIAL
C
C     XL       LENGTH  BETWEEN  POINTS
C     SP       SINE  OF  PHI
C     CP       COSINE OF PHI
C     I-S      INTEGRAL  FROM  PAGE 46 MS,28
C     MATID    MATERIAL ID  (MAT 1 CARD)
C     INFLAG   OPTION  2  OF MAT ROUTINE
C     TEMP     MATERIAL TEMPERATURE
C     SINTH    0.0  DUMMY
C     COSTH    1.0  DUMMY
C     XN       HARMONIC NUMBER
C     PA(8)    TOTAL LOAD VECTOR
C     XI(6)    CYLINDRICAL LOAD
C
C
C     IF MEMBRANE THICKNESS = 0, THEN LOAD IS ZERO
C
      IF (ECPT(5) .EQ. 0.0) GO TO 160
C
C     COMPUTE  L, SINPHI, COSPHI
C
      RBMA = RB - RA
      ZBMA = ZB - ZA
      XL2  = RBMA**2 + ZBMA**2
      XL   = SQRT(XL2)
      IF (XL .EQ. 0.0) GO TO 160
      SP = RBMA/XL
      CP = ZBMA/XL
C
C     COMPUTE  I-S
C
      XL4 = XL2*XL2
      RAV = (RA + RB)*0.5
      I00 = XL *RAV
      I10 = XL2*(RA + 2.0*RB)/6.0
      I01 = XL
      I11 = XL2/2.0
      I21 = XL2*XL/3.0
      I31 = XL4/4.0
      I41 = XL4*XL/5.0
C
C     SET UP FOR MAT ROUTINE
C
      MATID = MECPT(4)
      INFLAG= 2
      TEMP  = ECPT(35)
      SINTH = 0.0
      COSTH = 1.0
      CALL MAT (MECPT(1))
C
C     COMPUTE COEFICCIENTS
C
      F  = (G12*ALPH2 + G22*ALPH1)*ECPT(5)*PI
      FF = (G11*ALPH2 + G12*ALPH1)*ECPT(5)*PI
C
C     COMPUTE  A
C
      A = (TI(1)-TSUB0)*F
C
C     COMPUTE  B
C
      B = (TI(2)-TI(1))/XL*F
C
C     COMPUTE  C
C
      C = (TI(1)-TSUB0)*FF
C
C     COMPUTE  D
C
      D = (TI(2)-TI(1))/XL*FF
C
C     DECODE  N
C
      IXN = MECPT(1)/1000
      XN  = MECPT(1) - IXN*1000 - 1
C
C     COMPUTE  PA
C
      F  = I01*A + I11*B
      FF = I11*A + I21*B
      PA(1) = XN*F
      PA(2) = XN*FF
      PA(3) = SP*F
      PA(4) = SP*FF + I00*C + I10*D
      PA(5) = CP*F
      PA(6) = CP*FF
      PA(7) = CP*(I21*A + I31*B)
      PA(8) = CP*(I31*A + I41*B)
C
C     CHECK HARMONIC NO.  IF(XN = 0.0) DOUBLE PA VECTOR
C
      IF (XN .NE. 0.0) GO TO 30
      DO 20 I = 1,8
   20 PA(I) = 2.0*PA(I)
C
C     OMPUTE TRANSFORMATION MATRIX HUQ. SEE MS-28, PP. 15, 16, 24, 25
C
   30 DO 40 I = 1,100
   40 HUQ(I) = 0.0
      HUQ(  1) = ONE
      HUQ( 13) = ONE
      HUQ( 25) = ONE
      HUQ( 36) = ONE
      HUQ( 41) = CP/RA
      HUQ( 45) = XN/RA
      HUQ( 49) = ONE
      HUQ( 51) = ONE
      HUQ( 52) = XL
      HUQ( 63) = ONE
      HUQ( 64) = XL
      HUQ( 75) = ONE
      HUQ( 76) = XL
      HUQ( 77) = XL2
      HUQ( 78) = HUQ(77)*XL
      HUQ( 86) = ONE
      HUQ( 87) = 2.0*XL
      HUQ( 88) = 3.0*HUQ(77)
      HUQ( 91) = CP/RB
      HUQ( 92) = HUQ(91)*XL
      HUQ( 95) = XN/RB
      HUQ( 96) = HUQ(95)*XL
      HUQ( 97) = HUQ(95)*XL2
      HUQ( 98) = HUQ(96)*XL2
      HUQ( 99) = ONE
      HUQ(100) = XL
C
C     CHCEK IF HYQ VECTOR NEEDED
C
      IF (MATID2 .EQ.0   .OR. MATID3 .EQ.0  ) GO TO 60
      IF (ECPT(7).EQ.0.0 .OR. ECPT(9).EQ.0.0) GO TO 60
C
C     FORM  (D) = I*(G)
C
      D11 = ECPT(7)*G11
      D12 = ECPT(7)*G12
      D22 = ECPT(7)*G22
      D33 = ECPT(7)*G33
C
C     PICK UP GSHEAR FROM MAT
C
      INFLAG = 1
      MATID  = MATID3
      TEMP   = ECPT(35)
      CALL MAT (MECPT(1))
      IF (GSHEAR .EQ. 0.0) GO TO 60
C
C     COMPUTE INTEGRALS
C
      B  = SP
      B2 = B*B
      B3 = B*B2
      B4 = B*B3
      RLOG = ALOG(RB/RA)
      RASQ = RA*RA
      RBMA2  = RBMA*RAV
      ORBORA = ONE/RB - ONE/RA
      TWORA  = RA + RA
C
C     IF SP = 0 EVALUATE INTEGRALS DIFFERENTLY
C
      IF (SP .NE. 0.0) GO TO 45
      TEMP1= RAV*RAV
      TEMP3= XL2*XL
      I02  = XL/RAV
      I12  = XL2/(2.0*RAV)
      I22  = TEMP3/(3.0*RAV)
      I03  = XL/TEMP1
      I13  = XL2/(2.0*TEMP1)
      I23  = TEMP3/(3.0*TEMP1)
      I33  = (XL2*XL2)/(4.0*TEMP1)
      GO TO 49
   45 CONTINUE
      I02 = RLOG/B
      I12 = (RBMA - RA*RLOG)/B2
      I22 = (RBMA2 - TWORA*RBMA + RASQ*RLOG)/B3
      I03 =-ORBORA/B
      I13 = (RLOG + RA*ORBORA)/B2
      I23 = (RBMA - TWORA*RLOG - RASQ*ORBORA)/B3
      I33 = (RBMA2 - 3.0*RA*RBMA + 3.0*RASQ*RLOG + RASQ*RA*ORBORA)/B4
C
C     COMPUTE HYQ
C
   49 CONTINUE
      CP2 = CP*CP
      SP2 = SP*SP
      XN2 = XN*XN
      OPI = ONE/PI
      N2D33  = XN2*D33
      SP2D22 = SP2*D22
      OQ  = XL*TS*GSHEAR*RAV + I02*(N2D33 + SP2D22)*OPI
      OQ  = ONE/OQ
      NSP = XN*SP
      NCP = XN*CP
      NSPOPI  = NSP*OPI
      TWOD33  = 2.0*D33
      TEMP1   = D12*ORBORA
      TEMP2   = NSPOPI*(D22 + D33)
      TEMP3   = XN*NSPOPI*(TWOD33 + D22)
      TEMP4   = OQ*0.5*N2D33*CP*OPI
      TEMP5   = OPI*(XN2*TWOD33 + SP2D22)
      TEMP6   = D12*XN2*XL2/RB
      TEMP7   = NSPOPI*CP*0.5
      HYQ( 1) = OQ*(TEMP1*NCP - TEMP7*I03*(D33 + 2.0*D22))
      HYQ( 2) = OQ*(NCP*XL/RB*D12 - TEMP7*I13*(3.0*D33 + D22)
     1        + 1.5*NCP*OPI*I02*D33)
      HYQ( 3) = TEMP4*I03
      HYQ( 4) = TEMP4*I13
      HYQ( 5) = OQ*(TEMP1*XN2  -  TEMP3*I03)
      HYQ( 6) = OQ*(D12*XN2*XL/RB - TEMP3*I13 + TEMP5*I02)
      HYQ( 7) = OQ*(2.0*D11*(RA-RB) + TEMP6 + 2.0*I12*TEMP5 - TEMP3*I23)
      HYQ( 8) = OQ*(-D11*6.*XL*RB + TEMP6*XL + 3.*I22*TEMP5 - TEMP3*I33)
      HYQ( 9) =-OQ*TEMP2*I02
      HYQ(10) = OQ*(XN*XL*(D12 + D33) - TEMP2*I12)
      DO 50 I = 1,10
      HUQ(I+30) = HUQ(I+30) - HYQ(I)
   50 HUQ(I+80) = HUQ(I+80) - HYQ(I)
C
      ITEST = 1
      GO TO 61
   60 ITEST = 0
      HUQ(41) = 0.0
      HUQ(45) = 0.0
      HUQ(91) = 0.0
      HUQ(92) = 0.0
      HUQ(95) = 0.0
      HUQ(96) = 0.0
      HUQ(97) = 0.0
      HUQ(98) = 0.0
      HUQ(99) = 0.0
   61 CONTINUE
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (10,HUQ(1),10,DUM,0,DETERM,ISING,EHT(1))
      IF (ISING .EQ. 2) CALL MESAGE (-30,40,MECPT(1))
      IF (ITEST .NE. 0) GO TO 62
      HUQ( 85) = 0.0
      HUQ(100) = 0.0
   62 CONTINUE
C
C     COMPLETE SOLUTION
C
C     FIRST OBTAIN PRODUCTS
C                       T
C        EHAT  =  (E)(H  )      AND STORE AT EHT(1) . . . EHT(48)
C                      A
C
C                       T
C        EHBT  =  (E)(H  )      AND STORE AT EHT(49). . . EHT(96)
C                      B
C                                /
C              WHERE  (HUQ) = (HA/HB)
C                                /
C              AND
C                             0    CP   SP   0    0
C
C                             1    0    0    0    0
C
C                             0    CP  -SP   0    0
C                  E MATRIX =
C                             0    0    0    0    SP
C
C                             0    0    0    1    0
C
C                             0    0    0    0    CP
C
      INC1 = 0
      INC2 = 0
  110 DO 120 I = 1,8
      KROW = I + INC1
      NCOL = (I-1)*10 + INC2
      EHT(KROW   ) = SP*HUQ(NCOL+2) + CP*HUQ(NCOL+3)
      EHT(KROW+ 8) =    HUQ(NCOL+1)
      EHT(KROW+16) = CP*HUQ(NCOL+2) - SP*HUQ(NCOL+3)
      EHT(KROW+24) = SP*HUQ(NCOL+5)
      EHT(KROW+32) =    HUQ(NCOL+4)
  120 EHT(KROW+40) = CP*HUQ(NCOL+5)
      IF (INC1 .GT. 0) GO TO 130
      INC1 = 48
      INC2 = 5
      GO TO 110
C
C     PERFORM TRANSFORMATION OF LOAD VECTOR
C
  130 DO 150 J = 1,2
      CALL GMMATS (EHT(48*J-47),6,8,0,PA(1),8,1,0,XI(1))
      K = MECPT(J+1) - 1
      DO 140 I = 1,6
      K = K + 1
  140 Z(K) = Z(K) + XI(I)
  150 CONTINUE
C
  160 RETURN
C
      END
