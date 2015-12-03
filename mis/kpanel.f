      SUBROUTINE KPANEL (IARG)
C*****
C THIS ROUTINE COMPUTES THE  4  6 X 6 MATRICES K(NPVT,NPVT), K(NPVT,J1)
C K(NPVT,J2), K(NPVT,J3) FOR A SHEAR PANEL (IF IARG = 4) AND FOR A
C TWIST PANEL (IF IARG = 5)
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
C
C
C
      DOUBLE PRECISION
     1                   DPCON              ,VLEFT(6)
     2,                  VRIGHT(6)          ,TI(9)
     3,                  KE(36)             ,DAMPC
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
      DOUBLE PRECISION
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
      DOUBLE PRECISION
     1                   VP12L              ,VJL
     2,                  Z                  ,TERM
     3,                  F                  ,E
     4,                  G                  ,NU
     5,                  T                  ,C23
     6,                  NUC
      REAL
     1                   NUSP
C
C
C
      INTEGER
     1                   OUTRW              ,CLSNRW
     2,                  CLSRW              ,EOR
     3,                  FROWIC             ,TNROWS
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
C
C
C
      COMMON   /SYSTEM/
     1                   ISYS
C
C SMA1 I/O PARAMETERS
C
      COMMON   /SMA1IO/
     1                   IFCSTM             ,IFMPT
     2,                  IFDIT              ,IDUM1
     3,                  IFECPT             ,IGECPT
     4,                  IFGPCT             ,IGGPCT
     5,                  IFGEI              ,IGGEI
     6,                  IFKGG              ,IGKGG
     7,                  IF4GG              ,IG4GG
     8,                  IFGPST             ,IGGPST
     9,                  INRW               ,OUTRW
     T,                  CLSNRW             ,CLSRW
     1,                  NEOR               ,EOR
     2,                  MCBKGG(7)          ,MCB4GG(7)
C
C SMA1 VARIABLE CORE BOOKKEEPING PARAMETERS
C
      COMMON   /SMA1BK/
     1                   ICSTM              ,NCSTM
     2,                  IGPCT              ,NGPCT
     3,                  IPOINT             ,NPOINT
     4,                  I6X6K              ,N6X6K
     5,                  I6X64              ,N6X64
C
C SMA1 PROGRAM CONTROL PARAMETERS
C
      COMMON   /SMA1CL/
     1                   IOPT4              ,K4GGSW
     2,                  NPVT               ,LEFT
     3,                  FROWIC             ,LROWIC
     4,                  NROWSC             ,TNROWS
     5,                  JMAX               ,NLINKS
     6,                  LINK(10)           ,IDETCK
     7,                  DODET              ,NOGO
C
C ECPT COMMON BLOCK
C
      COMMON   /SMA1ET/
     1                   IELID              ,ISILNO(4)
     2,                  MATID              ,TSP
     3,                  FMU                ,ICSID1
     4,                  GP1(3)             ,ICSID2
     5,                  GP2(3)             ,ICSID3
     6,                  GP3(3)             ,ICSID4
     7,                  GP4(3)             ,TEMPEL
C
C SMA1 LOCAL VARIABLES
C
      COMMON   /SMA1DP/
     1                   KE                 ,TI
     2,                  VLEFT              ,VRIGHT
     3,                  DAMPC              ,DPCON
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
      COMMON   /SMA1DP/
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
      COMMON   /SMA1DP/
     1                   VP12L              ,VJL
     2,                  Z                  ,TERM
     3,                  F                  ,E
     4,                  G                  ,NU
     5,                  T                  ,C23
     6,                  NUC
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
     1                   ESP                ,GSP
     2,                  NUSP               ,RHO
     3,                  ALPHA              ,TSUBO
     4,                  GSUBE              ,SIGT
     5,                  SIGC               ,SIGS
C
C
C
      EQUIVALENCE    ( IECPT(1), ECPT(1), IELID )
C
C CALL MAT TO GET MATERIAL PROPERTIES.
C
      MATIDC = MATID
      MATFLG = 1
      ELTEMP = TEMPEL
      CALL MAT (IECPT(1))
      DAMPC = G SUB E
C
C STORE ECPT AND MPT VARIABLES IN DOUBLE PRECISION LOCATIONS
C
      E    = ESP
      G    = GSP
      NU   = NUSP
      T    = TSP
      IF(T*G .EQ. 0.0) GO TO 250
      C23  = 2.0D0 / 3.0D0
      NUC  = 1.0D0 / (1.0D0 + NU)
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
      VKL = DSQRT (VKN(1)**2  +  VKN(2)**2  +  VKN(3)**2  )
      IF (VKL .EQ. 0.0D0) GO TO 230
      VK(1) = VKN(1)/VKL
      VK(2) = VKN(2)/VKL
      VK(3) = VKN(3)/VKL
      PA = .5D0 * VKL
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
      VP12L = DSQRT (VP12(1)**2  +  VP12(2)**2  +  VP12(3)**2 )
      IF (VP12L .EQ. 0.0D0) GO TO 240
      VI(1) = VP12(1)/VP12L
      VI(2) = VP12(2)/VP12L
      VI(3) = VP12(3)/VP12L
      VJ(1) = VK(2)*VI(3)-VK(3)*VI(2)
      VJ(2) = VK(3)*VI(1)-VK(1)*VI(3)
      VJ(3) = VK(1)*VI(2)-VK(2)*VI(1)
C
C NORMALIZE J FOR GOOD MEASURE
C
      VJL = DSQRT (VJ(1)**2  + VJ(2)**2  +  VJ(3)**2 )
      IF (VJL .EQ. 0.0D0) GO TO 250
      VJ(1) = VJ(1) / VJL
      VJ(2) = VJ(2) / VJL
      VJ(3) = VJ(3) / VJL
      X1 = 0.0D0
      Y1 = 0.0D0
      X2 = VP12L
      Y2 = 0.0D0
      X3 = VI(1) * VD1(1)  +  VI(2) * VD1(2)  +  VI(3) * VD1(3)
      Y3 = VJ(1) * VD1(1)  +  VJ(2) * VD1(2)  +  VJ(3) * VD1(3)
      X4 =-VI(1) * V41(1)  -  VI(2) * V41(2)  -  VI(3) * V41(3)
      Y4 =-VJ(1) * V41(1)  -  VJ(2) * V41(2)  -  VJ(3) * V41(3)
C
C CHECK TO SEE IF INTERIOR ANGLES ARE LESS THAN 180 DEGREES.  IF NOT,
C CALL FATAL ERROR MESSAGE.
C
      IF (Y3 .LE. 0.0D0) GO TO 260
      IF(Y4 .LE. 0.0D0) GO TO 280
      IF(X3 .LE. Y3*X4/Y4) GO TO 270
      IF (X4 .GE. X2 - (X2-X3)*Y4/Y3) GO TO 290
C
C TEST FOR PARALLEL EFFECTS.
C
      TEMP = X3 - X2
      EP=1.0D-1
      IF (DABS(Y3-Y4).LT.DABS(X3-X4)*EP) GO TO 30
      IF (DABS(Y4*TEMP-Y3*X4).LT.DABS(X4*TEMP+Y4*Y3)*EP) GO TO 40
      GO TO 70
   30 IF (DABS(Y4*TEMP-Y3*X4).LT.DABS(X4*TEMP+Y4*Y3)*EP) GO TO 50
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
      Z   = ( (P(1)*P(2)*PA) / (P(3)*P(4)*2.0D0*G*T) ) *
     1        (1.0D0 + C23*NUC  * (SA**2 + SA*C + C**2) )
      GO TO 80
C
C AT THIS POINT THE LINE CONNECTING POINTS 1 AND 4 IS -PARALLEL- TO THE
C LINE CONNECTING POINTS 2 AND 3.
C
   40 D   = -.5D0 * (  X4/Y4  +  (X3-X2) / Y3  )
      XQ   = X4  - Y4  *  (X3-X4)/(Y3-Y4)
      TEMP = 1.0D0 / DSQRT (1.0D0 + D**2)
      P(1) = ( XQ - X1 - D*Y1) * TEMP
      P(2) = ( XQ - X2 - D*Y2) * TEMP
      P(3) = ( XQ - X3 - D*Y3) * TEMP
      P(4) = ( XQ - X4 - D*Y4) * TEMP
      TEMP =   XQ - X4
      B    =   (TEMP * D  +  Y4)  /  (TEMP  -  Y4*D)
      Z   = (  (P(1)*P(2)*PA) / (P(3)*P(4)*2.0D0*G*T)  ) *
     1      (1.0D0  +  C23*NUC* (B**2 + B*D + D**2)  )
      GO TO 80
C
C IN THIS CASE THE PANEL APPROXIMATES A PARALLELOGRAM.
C
   50 DO 60 I=1,4
   60 P(I) = 1.0D0
      D   = -.5D0 * (  X4/Y4  +  (X3-X2)/Y3  +  (Y3-Y4)/(X3-X4)  )
      Z   = PA / (2.0D0*G*T) * (1.0D0 + 2.0D0*D**2*NUC)
      GO TO 80
C
C IN THIS CASE NO PARALLEL EFFECTS EXIST.
C
   70 XQ   = X4  -  (X3-X4)/(Y3-Y4) * Y4
      TEMP = Y3*X4  -  Y4*(X3-X2)
      XP   = X2*Y3*X4 / TEMP
      YP   = X2*Y3*Y4 / TEMP
      XL   = DSQRT ( (XQ-XP)**2 + YP**2 )
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
      TEMP = .5D0 * P(1) * P(2) * P(3) * P(4) / XL**2
      TERM = A  +  B  +  C23*(A3+B3)  +  .2D0*(A5+B5)
      TERM1= C  +  D  +  C23*(C3+D3)  +  .2D0*(C5+D5)
      TERM2= B  +  C  +  C23*(B3+C3)  +  .2D0*(B5+C5)
      TERM3= D  +  A  +  C23*(D3+A3)  +  .2D0*(D5+A5)
      TERM =    TERM * DLOG( DABS (A+B) )
      TERM1=    TERM1* DLOG( DABS (C+D) )
      TERM2=    TERM2* DLOG( DABS (B+C) )
      TERM3=    TERM3* DLOG( DABS (D+A) )
      TERM4=   .1D0*( (A2-C2)*(B3-D3)  +  (B2-D2)*(A3-C3) )
      TERM5=   .2D0*( (A -C )*(B4-D4)  +  (B -D )*(A4-C4) )
      F    =  TEMP * (TERM + TERM1 - TERM2 - TERM3 + TERM4 - TERM5)
      Z  =  P(1)*P(2) / (P(3)*P(4)*2.0D0*G*T) * (PA + 4.0D0*NUC*
     1                                                  (F - C23*PA))
   80 XL13 = DSQRT (X3**2 + Y3**2)
      XL24 = DSQRT (  (X4-X2)**2  +  Y4**2  )
      SMALLU(1) = X3/XL13
      SMALLU(2) = (X4-X2)/XL24
      SMALLU(3) = SMALLU(1)
      SMALLU(4) = SMALLU(2)
      SMALLV(1) = Y3/XL13
      SMALLV(2) = Y4/XL24
      SMALLV(3) = SMALLV(1)
      SMALLV(4) = SMALLV(2)
      TEMP = X4 * Y3  -  X3 * Y4
      AVEC(1) = -.5D0 * X2 * Y4 * XL13 / TEMP
      AVEC(2) = .5D0 * X2 * Y3 * XL24 / (TEMP - X2 * (Y3-Y4) )
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
C SEARCH THE LIST OF THE 4 SIL NOS. TO DETERMINE WHICH IS THE PIVOT
C
  100 DO 110 I=1,4
      IF (ISILNO(I) .NE. NPVT) GO TO 110
      IPVT = I
      GO TO 120
  110 CONTINUE
      CALL MESAGE (-30,34,IECPT(1))
C
C COMPUTE THE DOUBLE PRECISION CONSTANT DPCON
C
  120 IF (IARG .EQ. 5) GO TO 130
      DPCON = AVEC(IPVT) / (2.0D0 * Z)
      GO TO 140
  130 DPCON = AVEC(IPVT) * T**2 / (24.0D0 * Z)
C
C COMPUTE THE -VLEFT- VECTOR
C
  140 IVLBEG = 1
      VLEFT(1) = VI(1) * SMALLU(IPVT)  +  VJ(1) * SMALLV(IPVT)
      VLEFT(2) = VI(2) * SMALLU(IPVT)  +  VJ(2) * SMALLV(IPVT)
      VLEFT(3) = VI(3) * SMALLU(IPVT)  +  VJ(3) * SMALLV(IPVT)
      IF (IECPT(4*IPVT+5) .EQ. 0) GO TO 150
      CALL TRANSD (IECPT(4*IPVT+5),TI)
      IVLBEG = 4
      CALL GMMATD (TI,3,3,1, VLEFT(1),3,1,0, VLEFT(4) )
C
C ZERO OUT THE 6 X 6 MATRIX KE
C
  150 DO 160 I=1,36
  160 KE(I) = 0.0D0
C
C COMPUTE THE 6 X 6 -S
C
      DO 220 J=1,4
      IVRBEG = 1
      VRIGHT(1) = SMALLU(J) * VI(1)  +  SMALLV(J) * VJ(1)
      VRIGHT(2) = SMALLU(J) * VI(2)  +  SMALLV(J) * VJ(2)
      VRIGHT(3) = SMALLU(J) * VI(3)  +  SMALLV(J) * VJ(3)
      IF (IECPT(4*J+5) .EQ. 0) GO TO 170
      CALL TRANSD (IECPT(4*J+5),TI)
      CALL GMMATD (VRIGHT(1),1,3,0, TI,3,3,0, VRIGHT(4) )
      IVRBEG = 4
  170 CALL GMMATD (VLEFT(IVLBEG),3,1,0, VRIGHT(IVRBEG),1,3,0, KE(1) )
      DO 180 K=1,9
  180 KE(K) = DPCON * KE(K) * AVEC(J)
      IF (IARG . EQ. 5) GO TO 190
      KE(13) = KE(7)
      KE(14) = KE(8)
      KE(15) = KE(9)
      KE( 7) = KE(4)
      KE( 8) = KE(5)
      KE( 9) = KE(6)
      KE( 4) = 0.0D0
      KE( 5) = 0.0D0
      KE( 6) = 0.0D0
      GO TO 210
  190 KE(22) = KE(1)
      KE(23) = KE(2)
      KE(24) = KE(3)
      KE(28) = KE(4)
      KE(29) = KE(5)
      KE(30) = KE(6)
      KE(34) = KE(7)
      KE(35) = KE(8)
      KE(36) = KE(9)
      DO 200 II=1,9
  200 KE(II) = 0.0D0
  210 CALL SMA1B (KE,IECPT(J+1),-1,IFKGG,0.0D0)
      IF (IOPT4 .EQ. 0  .OR.  G SUB E .EQ. 0.0) GO TO 220
      K4GGSW = 1
      CALL SMA1B (KE,IECPT(J+1),-1,IF4GG,DAMPC)
  220 CONTINUE
      RETURN
  230 CONTINUE
  240 CONTINUE
  250 CALL MESAGE(30,26,IECPT(1))
C
C  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
C
      NOGO=1
      RETURN
  260 IECPT(2) = 2
      GO TO 300
  270 IECPT(2) = 4
      GO TO 300
  280 IECPT(2) = 1
      GO TO 300
  290 IECPT(2) = 3
  300 CALL MESAGE(30,27,IECPT(1))
C
C  SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO ACCUMULATE
C
      NOGO=1
      RETURN
      END
