      SUBROUTINE DSHEAR
C
C     THIS COMPUTES THE THE TWO 6X6 DIFFERENTIAL STIFFNESS MATRICES
C     K(NPVT,NPVT) AND K(NPVT,J) WHERE J = 3,4,1,2 IF NPVT = 1,2,3,4
C     RESPECTIVELY.
C
C     ECPT FOR BOTH PANELS
C
C     ECPT( 1)  -  IELID          ELEMENT ID. NO.
C     ECPT( 2)  -  ISILNO(4)      SCALAR INDEX NUMBERS
C     ECPT( 3)  -   ...                   ...
C     ECPT( 4)  -   ...                   ...
C     ECPT( 5)  -   ...                   ...
C     ECPT( 6)  -  MATID          MATERIAL ID.
C     ECPT( 7)  -  T              THICKNESS
C     ECPT( 8)  -  FMU            NON-STRUCTURAL MASS
C     ECPT( 9)  -  ICSID1         COOR. SYS. ID. FOR GRID POINT 1
C     ECPT(10)  -  GP1(3)         BASIC COORDINATES FOR GRID POINT 1
C     ECPT(11)  -   ...                      ...
C     ECPT(12)  -   ...                      ...
C     ECPT(13)  -  ICSID2         COOR. SYS. ID. FOR GRID POINT 2
C     ECPT(14)  -  GP2(3)         BASIC COORDINATES FOR GRID POINT 2
C     ECPT(15)  -   ...                      ...
C     ECPT(16)  -   ...                      ...
C     ECPT(17)  -  ICSID3         COOR. SYS. ID. FOR GRID POINT 3
C     ECPT(18)  -  GP3(3)         BASIC COORDINATES FOR GRID POINT 3
C     ECPT(19)  -   ...                      ...
C     ECPT(20)  -   ...                      ...
C     ECPT(21)  -  ICSID4         COOR. SYS. ID. FOR GRID POINT 4
C     ECPT(22)  -  GP4(3)         BASIC COORDINATES FOR GRID POINT 4
C     ECPT(23)  -   ...                      ...
C     ECPT(24)  -   ...                      ...
C     ECPT(25)  -  TEMPEL         ELEMENT TEMPERATURE
C     ECPT(26)  -  DEFORM         ELEMENT DEFORMATION (NOT USED)
C     ECPT(27)  -  AVGLTP         AVG.ELEM LOADING TEMPERATURE, NOT USED
C     ECPT(28)  -  U1(3)          TRANSLATION DISPLACEMENTS AT PT. 1
C     ECPT(29)  -  ...                         ...
C     ECPT(30)  -  ...                         ...
C     ECPT(31)  -  U2(3)          TRANSLATION DISPLACEMENTS AT PT. 2
C     ECPT(32)  -  ...                         ...
C     ECPT(33)  -  ...                         ...
C     ECPT(34)  -  U3(3)          TRANSLATION DISPLACEMENTS AT PT. 3
C     ECPT(35)  -  ...                         ...
C     ECPT(36)  -  ...                         ...
C     ECPT(37)  -  U4(3)          TRANSLATION DISPLACEMENTS AT PT. 4
C     ECPT(38)  -  ...                         ...
C     ECPT(39)  -  ...                         ...
C
      REAL             NUSP
      DOUBLE PRECISION KE(36),TI(9),VLEFT(6),VD1,VD2,VKN,VK,V12,V41,
     1                 VP12,VI,VJ,AVEC,SMALLU,SMALLV,P,X1,X2,X3,X4,
     2                 Y1,Y2,Y3,Y4,VKL,PA,V12DK,CEP1,CEP2,EP,TEMP
      DOUBLE PRECISION YP,XP,SA,XQ,B,XL,A,A2,A3,A4,A5,B2,B3,B4,B5,C,C2,
     1                 C3,C4,C5,D,D2,D3,D4,D5,TERM1,TERM2,TERM3,TERM4,
     2                 TERM5,XL13,XL24
      DOUBLE PRECISION VP12L,VJL,Z,TERM,F,E,G,NU,T,C23,NUC
      DOUBLE PRECISION UI(3),DPTERM,SUM,F13,F24,FXX,JJ(3),J3X3(9),
     1                 K3X3(9)
      DIMENSION        VD1(3),VD2(3),VKN(3),VK(3),V12(3),V41(3),VP12(3),
     1                 VI(3),VJ(3),AVEC(4),SMALLU(4),SMALLV(4),P(4),
     2                 IECPT(100),ECPT(100),IZ(1)
      COMMON /ZZZZZZ/  ZZ(1)
      COMMON /DS1AAA/  NPVT,ICSTM,NCSTM,DUMCL(32),NOGO
      COMMON /DS1AET/  IELID,ISILNO(4),MATID,TSP,FMU,ICSID1,GP1(3),
     1                 ICSID2,GP2(3),ICSID3,GP3(3),ICSID4,GP4(3),TEMPEL,
     2                 DEFORM,AVGLTP,U1(3),U2(3),U3(3),U4(4)
      COMMON /DS1ADP/  KE,TI,VLEFT,VD1,VD2,VKN,VK,V12,V41,VP12,VI,VJ,
     1                 AVEC,SMALLU,SMALLV,P,X1,X2,X3,X4,Y1,Y2,Y3,Y4,
     2                 VKL,PA,V12DK,CEP1,CEP2,EP,TEMP
      COMMON /DS1ADP/  YP,XP,SA,XQ,B,XL,A,A2,A3,A4,A5,B2,B3,B4,B5,C,C2,
     1                 C3,C4,C5,D,D2,D3,D4,D5,TERM1,TERM2,TERM3,TERM4,
     2                 TERM5,XL13,XL24
      COMMON /DS1ADP/  VP12L,VJL,Z,TERM,F,E,G,NU,T,C23,NUC
      COMMON /DS1ADP/  UI,DPTERM,SUM,F13,F24,FXX,JJ,J3X3,K3X3
      COMMON /MATIN /  MATIDC,MATFLG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/  ESP,GSP,NUSP,RHO,ALPHA,TSUBO,GSUBE,SIGT,SIGC,SIGS
      EQUIVALENCE      (IZ(1),ZZ(1)),(IELID,IECPT(1),ECPT(1))
     1
C
C     CALL MAT TO GET MATERIAL PROPERTIES.
C
      MATIDC = MATID
      MATFLG = 1
      ELTEMP = TEMPEL
      CALL MAT (IECPT(1))
C
C     STORE ECPT AND MPT VARIABLES IN DOUBLE PRECISION LOCATIONS
C
      E    = ESP
      G    = GSP
      NU   = NUSP
      T    = TSP
      C23  = 2.0D0/3.0D0
      NUC  = 1.0D0/(1.0D0+NU)
C
C     COMPUTE DIAGONAL VECTORS.
C
      DO 10 I = 1,3
      VD1(I) = GP3(I) - GP1(I)
   10 VD2(I) = GP4(I) - GP2(I)
C
C     COMPUTE THE NORMAL VECTOR VKN, NORMALIZE, AND COMPUTE THE
C     PROJECTED AREA, PA
C
      VKN(1) = VD1(2)*VD2(3) - VD1(3)*VD2(2)
      VKN(2) = VD1(3)*VD2(1) - VD1(1)*VD2(3)
      VKN(3) = VD1(1)*VD2(2) - VD1(2)*VD2(1)
      VKL    = DSQRT(VKN(1)**2 + VKN(2)**2 + VKN(3)**2)
      IF (VKL .EQ. 0.0D0) GO TO 1010
      VK(1)  = VKN(1)/VKL
      VK(2)  = VKN(2)/VKL
      VK(3)  = VKN(3)/VKL
      PA     = .5D0*VKL
C
C     COMPUTE  SIDES -12- AND -41-
C
      DO 12 I = 1,3
      V12(I) = GP2(I) - GP1(I)
   12 V41(I) = GP1(I) - GP4(I)
C
C     COMPUTE DOT PRODUCT, V12DK, OF V12 AND VK, THE VECTORS VP12,VI,VJ
C
      V12DK   = V12(1)*VK(1) + V12(2)*VK(2) + V12(3)*VK(3)
      VP12(1) = V12(1) - V12DK*VK(1)
      VP12(2) = V12(2) - V12DK*VK(2)
      VP12(3) = V12(3) - V12DK*VK(3)
      VP12L   = DSQRT(VP12(1)**2 + VP12(2)**2 + VP12(3)**2)
      IF (VP12L .EQ. 0.0D0) GO TO 1020
      VI(1)   = VP12(1)/VP12L
      VI(2)   = VP12(2)/VP12L
      VI(3)   = VP12(3)/VP12L
      VJ(1)   = VK(2)*VI(3) - VK(3)*VI(2)
      VJ(2)   = VK(3)*VI(1) - VK(1)*VI(3)
      VJ(3)   = VK(1)*VI(2) - VK(2)*VI(1)
C
C     NORMALIZE J FOR GOOD MEASURE
C
      VJL   = DSQRT(VJ(1)**2 + VJ(2)**2 + VJ(3)**2)
      IF (VJL .EQ. 0.0D0) GO TO 1030
      VJ(1) = VJ(1)/VJL
      VJ(2) = VJ(2)/VJL
      VJ(3) = VJ(3)/VJL
      X1    = 0.0D0
      Y1    = 0.0D0
      X2    = VP12L
      Y2    = 0.0D0
      X3    = VI(1)*VD1(1) + VI(2)*VD1(2) + VI(3)*VD1(3)
      Y3    = VJ(1)*VD1(1) + VJ(2)*VD1(2) + VJ(3)*VD1(3)
      X4    =-VI(1)*V41(1) - VI(2)*V41(2) - VI(3)*V41(3)
      Y4    =-VJ(1)*V41(1) - VJ(2)*V41(2) - VJ(3)*V41(3)
C
C     CHECK TO SEE IF INTERIOR ANGLES ARE LESS THAN 180 DEGREES.
C     IF NOT, CALL FATAL ERROR MESSAGE.
C
      IF (Y3 .LE.    0.0D0) GO TO 1040
      IF (X3 .LE. Y3*X4/Y4) GO TO 1050
      IF (Y4 .LE.    0.0D0) GO TO 1060
      IF (X4 .GE. X2-(X2-X3)*Y4/Y3) GO TO 1070
C
C     TEST FOR PARALLEL EFFECTS.
C
      CEP1 = DABS((Y3-Y4)/(X3-X4))
      TEMP = X3 - X2
      CEP2 = DABS((Y4*TEMP-Y3*X4)/(X4*TEMP+Y4*Y3))
      EP   = 1.0D-1
      IF (CEP1 .LT. EP) GO TO 15
      IF (CEP2 .LT. EP) GO TO 30
      GO TO 50
   15 IF (CEP2 .LT. EP) GO TO 40
C
C     AT THIS POINT THE LINE CONNECTING POINTS 3 AND 4 IS -PARALLEL- TO
C     THE LINE CONNECTING POINTS 1 AND 2.
C
      TEMP = Y3*X4 - Y4*(X3-X2)
      YP   = X2*Y3*Y4/TEMP
      P(1) = YP - Y1
      P(2) = YP - Y2
      P(3) = YP - Y3
      P(4) = YP - Y4
      XP   = X2*Y3*X4/TEMP
      SA   = (X2 - XP)/YP
      C    = (X1 - XP)/YP
      Z    = ((P(1)*P(2)*PA)/(P(3)*P(4)*2.0D0*G*T))*
     1       (1.0D0 + C23*NUC*(SA**2 + SA*C + C**2))
      GO TO 60
C
C     AT THIS POINT THE LINE CONNECTING POINTS 1 AND 4 IS -PARALLEL- TO
C     THE LINE CONNECTING POINTS 2 AND 3.
C
   30 D    = -.5D0*(X4/Y4 + (X3-X2)/Y3)
      XQ   = X4 - Y4*(X3-X4)/(Y3-Y4)
      TEMP = 1.0D0/DSQRT(1.0D0 + D**2)
      P(1) = (XQ - X1 - D*Y1)*TEMP
      P(2) = (XQ - X2 - D*Y2)*TEMP
      P(3) = (XQ - X3 - D*Y3)*TEMP
      P(4) = (XQ - X4 - D*Y4)*TEMP
      TEMP =  XQ - X4
      B    = (TEMP*D + Y4)/(TEMP - Y4*D)
      Z    = ((P(1)*P(2)*PA)/(P(3)*P(4)*2.0D0*G*T))*
     1       (1.0D0 + C23*NUC*(B**2 + B*D + D**2))
      GO TO 60
C
C     IN THIS CASE THE PANEL APPROXIMATES A PARALLELOGRAM.
C
   40 DO 45 I = 1,4
   45 P(I) = 1.0D0
      D = -.5D0*(X4/Y4 + (X3-X2)/Y3 + (Y3-Y4)/(X3-X4))
      Z = PA/(2.0D0*G*T)*(1.0D0 + 2.0D0*D**2*NUC)
      GO TO 60
C
C     IN THIS CASE NO PARALLEL EFFECTS EXIST.
C
   50 XQ    = X4 - (X3-X4)/(Y3-Y4)*Y4
      TEMP  = Y3*X4 - Y4*(X3-X2)
      XP    = X2*Y3*X4/TEMP
      YP    = X2*Y3*Y4/TEMP
      XL    = DSQRT((XQ-XP)**2 + YP**2)
      D     = (XQ-XP)/YP
      TEMP  = YP/XL
      P(1)  = TEMP*(XQ - X1 - D*Y1)
      P(2)  = TEMP*(XQ - X2 - D*Y2)
      P(3)  = TEMP*(XQ - X3 - D*Y3)
      P(4)  = TEMP*(XQ - X4 - D*Y4)
      C     = XL/P(1) - D
      B     = XL/P(4) - C
      A     = XL/P(2) - D
      A2    = A**2
      B2    = B**2
      C2    = C**2
      D2    = D**2
      A3    = A2*A
      B3    = B2*B
      C3    = C2*C
      D3    = D2*D
      A4    = A3*A
      B4    = B3*B
      C4    = C3*C
      D4    = D3*D
      A5    = A4*A
      B5    = B4*B
      C5    = C4*C
      D5    = D4*D
      TEMP  = .5D0*P(1)*P(2)*P(3)*P(4)/XL**2
      TERM  = A + B + C23*(A3+B3) + .2D0*(A5+B5)
      TERM1 = C + D + C23*(C3+D3) + .2D0*(C5+D5)
      TERM2 = B + C + C23*(B3+C3) + .2D0*(B5+C5)
      TERM3 = D + A + C23*(D3+A3) + .2D0*(D5+A5)
      TERM  = TERM *DLOG(DABS(A+B))
      TERM1 = TERM1*DLOG(DABS(C+D))
      TERM2 = TERM2*DLOG(DABS(B+C))
      TERM3 = TERM3*DLOG(DABS(D+A))
      TERM4 = .1D0*((A2-C2)*(B3-D3) + (B2-D2)*(A3-C3))
      TERM5 = .2D0*((A -C )*(B4-D4) + (B -D )*(A4-C4))
      F     = TEMP*(TERM + TERM1 - TERM2 - TERM3 + TERM4 - TERM5)
      Z     = P(1)*P(2)/(P(3)*P(4)*2.0D0*G*T)*(PA+4.0D0*NUC*(F-C23*PA))
   60 XL13  = DSQRT(X3**2 + Y3**2)
      XL24  = DSQRT((X4-X2)**2 + Y4**2)
      SMALLU(1) = X3/XL13
      SMALLU(2) = (X4-X2)/XL24
      SMALLU(3) = SMALLU(1)
      SMALLU(4) = SMALLU(2)
      SMALLV(1) = Y3/XL13
      SMALLV(2) = Y4/XL24
      SMALLV(3) = SMALLV(1)
      SMALLV(4) = SMALLV(2)
      TEMP      = X4*Y3 - X3*Y4
      AVEC(1)   =-.5D0*X2*Y4*XL13/TEMP
      AVEC(2)   = .5D0*X2*Y3*XL24/(TEMP-X2*(Y3-Y4))
      AVEC(3)   =-AVEC(1)
      AVEC(4)   =-AVEC(2)
C
C     COMPUTE THE SUM GIVEN ON P. 16 OF FMMS-39
C
      SUM = 0.0D0
      DO 80 I = 1,4
      IVLBEG   = 1
      VLEFT(1) = SMALLU(I)*VI(1) + SMALLV(I)*VJ(1)
      VLEFT(2) = SMALLU(I)*VI(2) + SMALLV(I)*VJ(2)
      VLEFT(3) = SMALLU(I)*VI(3) + SMALLV(I)*VJ(3)
      IF (IECPT(4*I+5) .EQ. 0) GO TO 70
      CALL TRANSD (IECPT(4*I+5),TI)
      IVLBEG = 4
      CALL GMMATD (VLEFT(1),3,1,1, TI,3,3,0, VLEFT(4))
   70 K = 24 + 3*I
      UI(1) = ECPT(K+1)
      UI(2) = ECPT(K+2)
      UI(3) = ECPT(K+3)
      CALL GMMATD (VLEFT(IVLBEG),3,1,1, UI,3,1,0, DPTERM)
   80 SUM = SUM + AVEC(I)*DPTERM
      F13 =-AVEC(1)*SUM/(2.0D0*Z)
      F24 = AVEC(2)*F13/AVEC(1)
C
C     SEARCH LIST OF SIL NOS. IN THE ECPT FOR THE PIVOT POINT.
C
      DO 90 I = 1,4
      II = I
      IF (NPVT .EQ. IECPT(I+1)) GO TO 100
   90 CONTINUE
      CALL MESAGE (-30,34,IECPT(1))
  100 IF (II.EQ.2 .OR. II.EQ.4) GO TO 110
      FXX = F13/XL13
      I   = 1
      GO TO 120
  110 FXX = F24/XL24
      I   = 2
  120 JJ(1) = -VI(1)*SMALLV(I) + VJ(1)*SMALLU(I)
      JJ(2) = -VI(2)*SMALLV(I) + VJ(2)*SMALLU(I)
      JJ(3) = -VI(3)*SMALLV(I) + VJ(3)*SMALLU(I)
C
C                     T            T
C     COMPUTE  JJ X JJ  AND VK X VK
C
      CALL GMMATD (JJ,3,1,0, JJ,3,1,1, J3X3)
      CALL GMMATD (VK,3,1,0, VK,3,1,1, K3X3)
C
C     SUM THE TWO IN J3X3
C
      DO 130 J = 1,9
  130 J3X3(J) = J3X3(J) + K3X3(J)
      GO TO (140,150,160,170), II
  140 KK = 3
      GO TO 180
  150 KK = 4
      GO TO 180
  160 KK = 1
      GO TO 180
  170 KK = 2
C
C     ZERO OUT KE
C
  180 DO 190 I = 1,36
  190 KE(I) = 0.0D0
C
C                 D
C     SET UP THE K   MATRIX
C                 II
C
      MPOINT = 1
      IF (IECPT(4*II+5) .EQ. 0) GO TO 200
      CALL TRANSD (ECPT(4*II+5),TI)
      MPOINT = 10
      CALL GMMATD (TI,3,3,1, J3X3(1),3,3,0, K3X3(1))
      CALL GMMATD (K3X3(1),3,3,0, TI,3,3,0, J3X3(1))
  200 K = 1
      J = II
  210 KE( 1) = FXX*J3X3(K  )
      KE( 2) = FXX*J3X3(K+1)
      KE( 3) = FXX*J3X3(K+2)
      KE( 7) = FXX*J3X3(K+3)
      KE( 8) = FXX*J3X3(K+4)
      KE( 9) = FXX*J3X3(K+5)
      KE(13) = FXX*J3X3(K+6)
      KE(14) = FXX*J3X3(K+7)
      KE(15) = FXX*J3X3(K+8)
      CALL DS1B (KE,IECPT(J+1))
      IF (J .EQ. KK) RETURN
C
C                 D
C     SET UP THE K   MATRIX
C                 IJ
C
      J = KK
      IF (IECPT(4*J+5) .EQ. 0) GO TO 220
      CALL TRANSD (ECPT(4*J+5),TI)
      NPOINT = 10
      IF (MPOINT .EQ. 10) NPOINT = 1
      CALL GMMATD (J3X3(MPOINT),3,3,0, TI,3,3,0, J3X3(NPOINT))
      K = NPOINT
      GO TO 230
  220 K = MPOINT
  230 FXX = -FXX
      GO TO 210
C
C     ERROR RETURNS
C
 1010 CONTINUE
 1020 CONTINUE
 1030 CALL MESAGE (30,26,IECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
C
 1040 IECPT(2) = 2
      GO TO 2000
 1050 IECPT(2) = 4
      GO TO 2000
 1060 IECPT(2) = 1
      GO TO 2000
 1070 IECPT(2) = 3
 2000 CALL MESAGE (30,27,IECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
      END
