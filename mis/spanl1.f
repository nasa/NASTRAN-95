      SUBROUTINE SPANL1(IARG)
C*****
C THIS ROUTINE COMPUTES PHASE I PARAMETERS FOR STRESS DATA RECOVERY FOR
C THE SHEAR PANEL (IF IARG = 4) AND THE TWIST PANEL (IF IARG = 5).
C MUCH OF THE CODE WAS LIFTED FROM SUBROUTIVE KPANEL
C*****
C
C                 E C P T  F O R  B O T H  P A N E L S
C ECPT( 1)  -  IELID          ELEMENT ID. NO.
C ECPT( 2)  -  ISILNO(4)      SCALAR INDEX NUMBERS
C ECPT( 3)  -   ...                   ...
C ECPT( 4)  -   ...                   ...
C ECPT( 5)  -   ...                   ...
C ECPT( 6)  -  MATID          MATERIAL ID.
C ECPT( 7)  -  T              THICKNESS
C ECPT( 8)  -  FMU            NON-STRUCTURAL MASS
C ECPT( 9)  -  ICSID1         COOR. SYS. ID. FOR GRID POINT 1
C ECPT(10)  -  GP1(3)         BASIC COORDINATES FOR GRID POINT 1
C ECPT(11)  -   ...                      ...
C ECPT(12)  -   ...                      ...
C ECPT(13)  -  ICSID2         COOR. SYS. ID. FOR GRID POINT 2
C ECPT(14)  -  GP2(3)         BASIC COORDINATES FOR GRID POINT 2
C ECPT(15)  -   ...                      ...
C ECPT(16)  -   ...                      ...
C ECPT(17)  -  ICSID3         COOR. SYS. ID. FOR GRID POINT 3
C ECPT(18)  -  GP3(3)         BASIC COORDINATES FOR GRID POINT 3
C ECPT(19)  -   ...                      ...
C ECPT(20)  -   ...                      ...
C ECPT(21)  -  ICSID4         COOR. SYS. ID. FOR GRID POINT 4
C ECPT(22)  -  GP4(3)         BASIC COORDINATES FOR GRID POINT 4
C ECPT(23)  -   ...                      ...
C ECPT(24)  -   ...                      ...
C ECPT(25)  -  TEMPEL         ELEMENT TEMPERATURE
C
C
C
      REAL
     1                   NU
C
C
C
C
C
C
      DIMENSION
     1                   VD1(3)             ,VD2(3)
     2,                  VKN(3)             ,VK(3)
     3,                  V12(3)             ,V41(3)
     4,                  VP12(3)            ,VI(3)
     5,                  VJ(3)              ,AVEC(4)
     6,                  SMALLU(4)          ,SMALLV(4)
     7,                  P(4)               ,IECPT(100)
     8,                  ECPT(100)
     9,                  VLEFT(6)           ,TI(9)
C
C SDR2 PHASE I INPUT AND OUTPUT BLOCK
C
      COMMON   /SDR2X5/
     1                   IELID              ,ISILNO(4)
     2,                  MATID              ,T
     3,                  FMU                ,ICSID1
     4,                  GP1(3)             ,ICSID2
     5,                  GP2(3)             ,ICSID3
     6,                  GP3(3)             ,ICSID4
     7,                  GP4(3)             ,TEMPEL
     8,                  XXXXXX(75)
      COMMON   /SDR2X5/
     1                   JELID              ,JSILNO(4)
     2,                  S(3,4)             ,OUT(15)
     3,                  YYYYYY(93)
C
C SDR2 SCRATCH BLOCK
C
      COMMON   /SDR2X6/
     1                   VLEFT              ,TI
     2,                  SPCON
     4,                  VD1                ,VD2
     5,                  VKN                ,VK
     6,                  V12                ,V41
     7,                  VP12               ,VI
     8,                  VJ                 ,AVEC
     9,                  SMALLU             ,SMALLV
     T,                  P                  ,X1
     1,                  X2                 ,X3
     2,                  X4                 ,Y1
     3,                  Y2                 ,Y3
     4,                  Y4                 ,VKL
     5,                  PA                 ,V12DK
     6,                  CEP1               ,CEP2
     7,                  EP                 ,TEMP
      COMMON   /SDR2X6/
     1                   YP                 ,XP
     2,                  SA                 ,XQ
     4,                  B                  ,XL
     5,                  A                  ,A2
     6,                  A3                 ,A4
     7,                  A5                 ,B2
     8,                  B3                 ,B4
     9,                  B5                 ,C
     T,                  C2                 ,C3
     1,                  C4                 ,C5
     2,                  D                  ,D2
     3,                  D3                 ,D4
     4,                  D5                 ,TERM1
     5,                  TERM2              ,TERM3
     6,                  TERM4              ,TERM5
     7,                  XL13               ,XL24
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
     3,                  ALPHA              ,TSUBO
     4,                  GSUBE              ,SIGT
     5,                  SIGC               ,SIGS
C
C
C
      EQUIVALENCE
     1                   (IELID,IECPT(1),ECPT(1))
C
C CALL MAT TO GET MATERIAL PROPERTIES.
C
      MATIDC = MATID
      MATFLG = 1
      ELTEMP = TEMPEL
      CALL MAT (IECPT(1))
C
C COMPUTE DIAGONAL VECTORS.
C
      DO 10 I=1,3
      VD1(I) = GP3(I) - GP1(I)
   10 VD2(I) = GP4(I) - GP2(I)
C
C COMPUTE THE NORMAL VECTOR VKN, NORMALIZE, AND COMPUTE THE PROJECTED
C AREA, PA
C
      VKN(1) = VD1(2)*VD2(3)-VD1(3)*VD2(2)
      VKN(2) = VD1(3)*VD2(1)-VD1(1)*VD2(3)
      VKN(3) = VD1(1)*VD2(2)-VD1(2)*VD2(1)
      VKL = SQRT(VKN(1)**2+VKN(2)**2+VKN(3)**2)
      IF (VKL .EQ. 0.0) GO TO 160
      VK(1) = VKN(1)/VKL
      VK(2) = VKN(2)/VKL
      VK(3) = VKN(3)/VKL
      PA = .5 * VKL
C
C COMPUTE  SIDES -12- AND -41-
C
      DO 20 I=1,3
      V12(I) = GP2(I) - GP1(I)
   20 V41(I) = GP1(I) - GP4(I)
C
C COMPUTE DOT PRODUCT, V12DK, OF V12 AND VK, THE VECTORS VP12, VI, VJ
C
      V12DK = V12(1)*VK(1)+V12(2)*VK(2)+V12(3)*VK(3)
      VP12(1) = V12(1)-V12DK*VK(1)
      VP12(2) = V12(2)-V12DK*VK(2)
      VP12(3) = V12(3)-V12DK*VK(3)
      VP12L = SQRT(VP12(1)**2+VP12(2)**2+VP12(3)**2)
      IF (VP12L .EQ. 0.0) GO TO 170
      VI(1) = VP12(1)/VP12L
      VI(2) = VP12(2)/VP12L
      VI(3) = VP12(3)/VP12L
      VJ(1) = VK(2)*VI(3)-VK(3)*VI(2)
      VJ(2) = VK(3)*VI(1)-VK(1)*VI(3)
      VJ(3) = VK(1)*VI(2)-VK(2)*VI(1)
C
C NORMALIZE J FOR GOOD MEASURE
C
      VJL = SQRT (VJ(1)**2  +  VJ(2)**2  +  VJ(3)**2)
      IF (VJL .EQ. 0.0) GO TO 180
      VJ(1) = VJ(1) / VJL
      VJ(2) = VJ(2) / VJL
      VJ(3) = VJ(3) / VJL
      X1 = 0.0
      Y1 = 0.0
      X2 = VP12L
      Y2 = 0.0
      X3 = VI(1) * VD1(1)  +  VI(2) * VD1(2)  +  VI(3) * VD1(3)
      Y3 = VJ(1) * VD1(1)  +  VJ(2) * VD1(2)  +  VJ(3) * VD1(3)
      X4 =-VI(1) * V41(1)  -  VI(2) * V41(2)  -  VI(3) * V41(3)
      Y4 =-VJ(1) * V41(1)  -  VJ(2) * V41(2)  -  VJ(3) * V41(3)
C
C CHECK TO SEE IF INTERIOR ANGLES ARE LESS THAN 180 DEGREES.  IF NOT,
C CALL FATAL ERROR MESSAGE.
C
      IF (Y3 .LE. 0.0) GO TO 190
      IF (X3 .LE. Y3*X4/Y4) GO TO 200
      IF (Y4 .LE. 0.0) GO TO 210
      IF (X4 .GE. X2 - (X2-X3)*Y4/Y3) GO TO 220
C
C TEST FOR PARALLEL EFFECTS.
C
      TEMP = X3 - X2
      EP = 0.01
      IF (ABS(Y3-Y4).LT.ABS(X3-X4)*EP) GO TO 30
      IF (ABS(Y4*TEMP-Y3*X4).LT.ABS(X4*TEMP+Y4*Y3)*EP) GO TO 40
      GO TO 70
   30 IF (ABS(Y4*TEMP-Y3*X4).LT.ABS(X4*TEMP+Y4*Y3)*EP) GO TO 50
C
C AT THIS POINT THE LINE CONNECTING POINTS 3 AND 4 IS -PARALLEL- TO THE
C LINE CONNECTING POINTS 1 AND 2.
C
      TEMP = Y3*X4  -  Y4 * (X3-X2)
      YP   = X2*Y3*Y4 / TEMP
      P(1) = YP - Y1
      P(2) = YP - Y2
      P(3) = YP - Y3
      P(4) = YP - Y4
      XP   = X2*Y3*X4 / TEMP
      SA   =(X2 - XP) / YP
      C    =(X1 - XP) / YP
      Z    =(  (P(1)*P(2)*PA) / (P(3)*P(4)*2.0*G*T)  ) *
     1      (  1.0  +  2.0/(3.0 + 3.0*NU) * (SA**2 + SA*C + C**2)  )
      GO TO 80
C
C AT THIS POINT THE LINE CONNECTING POINTS 1 AND 4 IS -PARALLEL- TO THE
C LINE CONNECTING POINTS 2 AND 3.
C
   40 D    = -.5 * (  X4/Y4  +  (X3-X2)/Y3  )
      XQ   = X4  - Y4  *  (X3-X4)/(Y3-Y4)
      TEMP = 1.0 / SQRT (1.0 + D**2)
      P(1) = ( XQ - X1 - D*Y1) * TEMP
      P(2) = ( XQ - X2 - D*Y2) * TEMP
      P(3) = ( XQ - X3 - D*Y3) * TEMP
      P(4) = ( XQ - X4 - D*Y4) * TEMP
      TEMP =   XQ - X4
      B    =   (TEMP * D  +  Y4)  /  (TEMP  -  Y4*D)
      Z    =(  (P(1)*P(2)*PA) / (P(3)*P(4)*2.0*G*T)  ) *
     1      (  1.0  +  2.0/(3.0 + 3.0*NU) * (B**2 + B*D + D**2)  )
      GO TO 80
C
C IN THIS CASE THE PANEL APPROXIMATES A PARALLELOGRAM.
C
   50 DO 60 I=1,4
   60 P(I) = 1.0
      D    = -.5 * (  X4/Y4  +  (X3-X2)/Y3  +  (Y3-Y4)/(X3-X4)  )
      Z    = PA / (2.0*G*T) * (1.0 + 2.0*D**2/(1.0+NU))
      GO TO 80
C
C IN THIS CASE NO PARALLEL EFFECTS EXIST.
C
   70 XQ   = X4  -  (X3-X4)/(Y3-Y4) * Y4
      TEMP = Y3*X4  -  Y4*(X3-X2)
      XP   = X2*Y3*X4 / TEMP
      YP   = X2*Y3*Y4 / TEMP
      XL   = SQRT ( (XQ-XP)**2 + YP**2 )
      D    = (XQ-XP)/YP
      TEMP = YP/XL
      P(1) = TEMP * (XQ - X1 - D*Y1)
      P(2) = TEMP * (XQ - X2 - D*Y2)
      P(3) = TEMP * (XQ - X3 - D*Y3)
      P(4) = TEMP * (XQ - X4 - D*Y4)
      C    = XL/P(1) - D
      B    = XL/P(4) - C
      A    = XL/P(2) - D
      A2   = A**2
      B2   = B**2
      C2   = C**2
      D2   = D**2
      A3   = A2*A
      B3   = B2*B
      C3   = C2*C
      D3   = D2*D
      A4   = A3*A
      B4   = B3*B
      C4   = C3*C
      D4   = D3*D
      A5   = A4*A
      B5   = B4*B
      C5   = C4*C
      D5   = D4*D
      TEMP = .5 * P(1) * P(2) * P(3) * P(4) / XL**2
      TERM =    A  +  B  +  2.0*(A3+B3)/3.0  +  .2*(A5+B5)
      TERM1=    C  +  D  +  2.0*(C3+D3)/3.0  +  .2*(C5+D5)
      TERM2=    B  +  C  +  2.0*(B3+C3)/3.0  +  .2*(B5+C5)
      TERM3=    D  +  A  +  2.0*(D3+A3)/3.0  +  .2*(D5+A5)
      TERM =   TERM  * ALOG(ABS(A+B))
      TERM1=   TERM1 * ALOG(ABS(C+D))
      TERM2=   TERM2 * ALOG(ABS(B+C))
      TERM3=   TERM3 * ALOG(ABS(D+A))
      TERM4=  .1*( (A2-C2)*(B3-D3)  +  (B2-D2)*(A3-C3) )
      TERM5=  .2*( (A -C )*(B4-D4)  +  (B -D )*(A4-C4) )
      F    =  TEMP * (TERM + TERM1 - TERM2 - TERM3 + TERM4 - TERM5)
      Z    =  P(1)*P(2) / (P(3)*P(4)*2.0*G*T) * (PA + 4.0/(1.0+NU) *
     1                                               (F - 2.0*PA/3.0))
   80 XL13 =  SQRT (X3**2 + Y3**2)
      XL24 =  SQRT (  (X4-X2)**2  +  Y4**2  )
      SMALLU(1) = X3/XL13
      SMALLU(2) = (X4-X2)/XL24
      SMALLU(3) = SMALLU(1)
      SMALLU(4) = SMALLU(2)
      SMALLV(1) = Y3/XL13
      SMALLV(2) = Y4/XL24
      SMALLV(3) = SMALLV(1)
      SMALLV(4) = SMALLV(2)
      TEMP = X4 * Y3  -  X3 * Y4
      AVEC(1) = -.5 * X2 * Y4 * XL13 / TEMP
      AVEC(2) = .5 * X2 * Y3 * XL24 / (TEMP - X2 * (Y3-Y4) )
      AVEC(3) = - AVEC(1)
      AVEC(4) = - AVEC(2)
C
C IF IARG = 4, WE HAVE A SHEAR PANEL, AND IF IARG = 5, A TWIST PANEL.
C
      IF (IARG .EQ. 4) GO TO 100
C
C SINCE WE ARE DEALING WITH A TWIST PANEL STORE -SMALLV IN SMALLU AND
C SMALLU IN SMALLV.
C
      DO 90 I=1,4
      TEMP = SMALLU(I)
      SMALLU(I) = -SMALLV(I)
   90 SMALLV(I) = TEMP
C
C COMPUTE THE SINGLE PRECISION CONSTANT SPCON
C
  100 IF (IARG .EQ. 5) GO TO 110
      SPCON = -1.0/ (2.0 * Z * T)
      GO TO 120
  110 SPCON = -1.0/ (4.0 * Z)
C
C COMPUTE THE FOUR 1 X 3 MATRICES S
C
  120 DO 140 I=1,4
      IVLBEG = 1
      VLEFT(1) = SMALLU(I) * VI(1)  +  SMALLV(I) * VJ(1)
      VLEFT(2) = SMALLU(I) * VI(2)  +  SMALLV(I) * VJ(2)
      VLEFT(3) = SMALLU(I) * VI(3)  +  SMALLV(I) * VJ(3)
      IF (IECPT(4*I+5) .EQ. 0) GO TO 130
      IVLBEG = 4
      CALL TRANSS (IECPT(4*I+5),TI)
      CALL GMMATS (VLEFT(1),3,1,1, TI,3,3,0, VLEFT(4) )
  130 CONTINUE
      S(1,I) = SPCON * VLEFT(IVLBEG  ) * AVEC(I)
      S(2,I) = SPCON * VLEFT(IVLBEG+1) * AVEC(I)
      S(3,I) = SPCON * VLEFT(IVLBEG+2) * AVEC(I)
  140 CONTINUE
      OUT(1) = AVEC(1)
      OUT(2) = AVEC(2)
      OUT(3) = T
      OUT(4) = P(2) / P(1)
      OUT(5) = P(1) * P(2) / P(3)**2
      OUT(6) = P(1) * P(2) / P(4)**2
      OUT(7) = SIGS
      JELID = IELID
      DO 150 I=1,4
  150 JSILNO(I) = ISILNO(I)
      IF( IARG .NE. 4 ) RETURN
C*****
C  ADDITIONAL PHASE-1 OUTPUTS FOR SHEAR PANEL FORCES  IN PHASE 2
C*****
      OUT(8) = P(1) / P(3) *T
      OUT(9) = ( P(1)*P(2) ) / ( P(3)*P(4) ) * T
      OUT(10) = P(2) / P(4) * T
      OUT(11)= -V12DK / 2.0
      OUT(12)= X2 / 2.0
      OUT(13)= SQRT( (X3-X2)**2 + Y3**2 ) / 2.0
      OUT(14)= SQRT( (X4-X3)**2 + (Y4-Y3)**2 ) / 2.0
      OUT(15)= SQRT( X4**2 + Y4**2 ) / 2.0
      RETURN
  160 CONTINUE
  170 CONTINUE
  180 CALL MESAGE (-30,26,IECPT(1))
  190 IECPT(2) = 2
      GO TO 230
  200 IECPT(2) = 4
      GO TO 230
  210 IECPT(2) = 1
      GO TO 230
  220 IECPT(2) = 3
  230 CALL MESAGE (-30,27,IECPT(1))
      RETURN
      END
