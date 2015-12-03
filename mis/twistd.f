      SUBROUTINE TWISTD
C
C     THIS SUBROUTINE COMPUTES THE 12 X 12 STIFFNESS MATRIX FOR THE
C     TWIST PANEL ELEMENT, AS WELL AS ITS DIAGONALIZED MASS MATRIX
C
C     DOUBLE PRECISION VERSION
C
C     ECPT FOR THE BOTH TWIST PANEL ELEMENTS
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
C
      LOGICAL          NOGO,IHEAT
      INTEGER          DICT(11),IECPT(2),ESTID,ELID,IPART(4)
      REAL             NU,ECPT(100)
      DOUBLE PRECISION CEPX,CEPY,KE(144),KOUT(144),ME(144),MOUT(144),
     1                 VLEFT(6),A,B,C,A2,B2,C2,VRIGHT(6),P(4),T,C23,
     2                 NUC,G,E,X1,Y1,X2,Y2,X3,D2,A3,B3,C3,D3,Y3,X4,Y4,
     3                 CEP1,TEMP,CEP2,EP,D,XP,YP,XL,XQ,A4,B4,C4,D4,
     4                 A5,B5,C5,D5,TERM,TERM1,TERM2,TERM3,TERM4,TERM5,
     5                 F,Z,VD1(3),VD2(3),VKN(3),VK(3),V12(3),V41(3),
     6                 VP12(3),VI(3),VJ(3),AVEC(4),SMALLU(4),SMALLV(4),
     7                 XL13,XL24,CON,TI(9)
      COMMON /SYSTEM/  KSYSTM(55),IHEAT
      COMMON /EMGPRM/  DUM(15), ISMB(3), IPREC,NOGO, HEAT
      COMMON /EMGDIC/  IDM,LDICT,NGRIDS,ELID,ESTID
C
C     ECPT COMMON BLOCK
C
      COMMON /EMGEST/  IELID,ISILNO(4),MATID,TSP,FMU,ICSID1,GP1(3),
     1                 ICSID2,GP2(3),ICSID3,GP3(3),ICSID4,GP4(3),TEMPEL
C
C     INPUT AND OUTPUT BLOCKS FOR SUBROUTINE MAT
C
      COMMON /MATIN /  MATIDC,MATFLG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/  ESP,GSP,NU,RHO,ALPHA,TSUB0,GSUBE,SIGT,SIGC,SIGS
      EQUIVALENCE      (IECPT(1),ECPT(1),IELID),(DICT(5),DICT5),
     1                 (ME(1),KE(1)),(KOUT(1),MOUT(1))
      DATA    IPART /  1,2,3,4/
C
C
      DICT(1) = ESTID
      DICT(2) = 1
      DICT(3) = 12
      DICT(4) = 56
      IP      = IPREC
      ISORT   = 0
C
C     IF STIFFNESS MATRIX NOT NEEDED GO TO PERFORM MASS CALCULATIONS
C
      IF (ISMB(1) .EQ. 0) GOTO 400
C
C
      MATIDC = MATID
      MATFLG = 1
      ELTEMP = TEMPEL
      CALL MAT (IECPT(1))
      DICT5  = GSUBE
      E   = ESP
      G   = GSP
      T   = TSP
      IF (T*G  .EQ. 0.D0) GO TO 7770
      C23 = 2.D0/3.D0
      NUC = 1./(1.+NU)
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
      IF (VKL .EQ. 0.) GO TO 7770
      VK(1)  = VKN(1)/VKL
      VK(2)  = VKN(2)/VKL
      VK(3)  = VKN(3)/VKL
      PA     = VKL/2.
C
C     COMPUTE  SIDES -12- AND -41-
C
      DO 20 I = 1,3
      V12(I) = GP2(I) - GP1(I)
   20 V41(I) = GP1(I) - GP4(I)
C
C     COMPUTE DOT PRODUCT, V12DK, OF V12 AND VK, THE VECTORS VP12, VI,
C     VJ
C
      V12DK   = V12(1)*VK(1) + V12(2)*VK(2) + V12(3)*VK(3)
      VP12(1) = V12(1) - V12DK*VK(1)
      VP12(2) = V12(2) - V12DK*VK(2)
      VP12(3) = V12(3) - V12DK*VK(3)
      VP12L   = DSQRT(VP12(1)**2 + VP12(2)**2 + VP12(3)**2)
      IF (VP12L .EQ. 0.) GO TO 7770
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
      IF (VJL .EQ. 0.) GO TO 7770
      VJ(1) = VJ(1)/VJL
      VJ(2) = VJ(2)/VJL
      VJ(3) = VJ(3)/VJL
      X1 = 0.
      Y1 = 0.
      X2 = VP12L
      Y2 = 0.
      X3 = VI(1)*VD1(1) + VI(2)*VD1(2) + VI(3)*VD1(3)
      Y3 = VJ(1)*VD1(1) + VJ(2)*VD1(2) + VJ(3)*VD1(3)
      X4 =-VI(1)*V41(1) - VI(2)*V41(2) - VI(3)*V41(3)
      Y4 =-VJ(1)*V41(1) - VJ(2)*V41(2) - VJ(3)*V41(3)
C
C     CHECK TO SEE IF INTERIOR ANGLES ARE LESS THAN 180 DEGREES. IF NOT,
C     CALL FATAL ERROR MESSAGE.
C
      IF (Y3 .LE. 0.) GO TO 7780
      IF (Y4 .LE. 0.) GO TO 7800
      IF (X3 .LE. Y3*X4/Y4) GO TO 7810
      IF (X4 .GE. X2-(X2-X3)*Y4/Y3) GO TO 7790
C
C     TEST FOR PARALLEL EFFECTS.
C
      CEP1 = DABS(Y3-Y4)
      CEPX = DABS(X3-X4)
      TEMP = X3 - X2
      CEP2 = DABS(Y4*TEMP - Y3*X4)
      CEPY = DABS(X4*TEMP + Y4*Y3)
      EP   = 0.01D0
      IF (CEP1 .LT. EP*CEPX) GO TO 30
      IF (CEP2 .LT. EP*CEPY) GO TO 40
      GO TO 70
   30 IF (CEP2 .LT. EP*CEPY) GO TO 50
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
      Z    = ((P(1)*P(2)*PA)/(P(3)*P(4)*2.*G*T))*
     1       (1.+C23*NUC*(SA**2+SA*C+C**2))
      GO TO 80
C
C     AT THIS POINT THE LINE CONNECTING POINTS 1 AND 4 IS -PARALLEL- TO
C     THE LINE CONNECTING POINTS 2 AND 3.
C
   40 D    = -.5*(X4/Y4 + (X3-X2)/Y3)
      XQ   = X4  - Y4*(X3-X4)/(Y3-Y4)
      TEMP = 1.D0/DSQRT(1.D0 + D**2)
      P(1) = (XQ - X1 - D*Y1)*TEMP
      P(2) = (XQ - X2 - D*Y2)*TEMP
      P(3) = (XQ - X3 - D*Y3)*TEMP
      P(4) = (XQ - X4 - D*Y4)*TEMP
      TEMP =  XQ - X4
      B    = (TEMP*D + Y4)/(TEMP - Y4*D)
      Z    = ((P(1)*P(2)*PA)/(P(3)*P(4)*2.*G*T))*
     1       (1.+C23* NUC*(B**2+B*D+D**2))
      GO TO 80
C
C     IN THIS CASE THE PANEL APPROXIMATES A PARALLELOGRAM.
C
   50 DO 60 I = 1,4
   60 P(I) = 1.
      D = -.5*(X4/Y4 + (X3-X2)/Y3 + (Y3-Y4)/(X3-X4))
      Z =  PA/(2.*G*T)*(1.+2.*D**2 * NUC)
      GO TO 80
C
C     IN THIS CASE NO PARALLEL EFFECTS EXIST.
C
   70 XQ   = X4 - (X3-X4)/(Y3-Y4)*Y4
      TEMP = Y3*X4 - Y4*(X3-X2)
      XP   = X2*Y3*X4/TEMP
      YP   = X2*Y3*Y4/TEMP
      XL   = DSQRT((XP-YP)**2 + YP**2)
      D    = (XQ-XP)/YP
      TEMP = YP/XL
      P(1) = TEMP*(XQ - X1 - D*Y1)
      P(2) = TEMP*(XQ - X2 - D*Y2)
      P(3) = TEMP*(XQ - X3 - D*Y3)
      P(4) = TEMP*(XQ - X4 - D*Y4)
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
      TEMP = .5*P(1)*P(2)*P(3)*P(4)/XL**2
      TERM =(A + B + C23*(A3+B3) + .2 *(A5+B5))*DLOG(DABS(A+B))
      TERM1=(C + D + C23*(C3+D3) + .2*(C5+D5))*DLOG(DABS(C+D))
      TERM2=(B + C + C23*(B3+C3) + .2*(B5+C5))*DLOG(DABS(B+C))
      TERM3=(D + A + C23*(D3+A3) + .2*(D5+A5))*DLOG(DABS(D+A))
      TERM4= .1*((A2-C2)*(B3-D3)+ (B2-D2)*(A3-C3))
      TERM5= .2*((A - C )*(B4-D4) + (B-D)*(A4-C4))
      F    = TEMP*(TERM + TERM1 - TERM2 - TERM3 + TERM4 - TERM5)
      Z    = P(1)*P(2)/(P(3)*P(4)*2.*G*T)*(PA+4.*NUC*(F-C23*PA))
   80 XL13 = DSQRT(X3**2 + Y3**2)
      XL24 = DSQRT((X4-X2)**2 + Y4**2)
      SMALLU(1) = X3/XL13
      SMALLU(2) = (X4-X2)/XL24
      SMALLU(3) = SMALLU(1)
      SMALLU(4) = SMALLU(2)
      SMALLV(1) = Y3/XL13
      SMALLV(2) = Y4/XL24
      SMALLV(3) = SMALLV(1)
      SMALLV(4) = SMALLV(2)
      TEMP      = X4*Y3 - X3*Y4
      AVEC(1)   =-.5*X2*Y4*XL13/TEMP
      AVEC(2)   = .5*X2*Y3 *XL24/(TEMP -X2*(Y3-Y4))
      AVEC(3)   =-AVEC(1)
      AVEC(4)   =-AVEC(2)
C
C     SINCE WE ARE DEALING WITH A TWIST PANEL STORE -SMALLV IN SMALLU
C     AND SMALLU IN SMALLV.
C
      DO 90 I = 1,4
      TEMP = SMALLU(I)
      SMALLU(I) =-SMALLV(I)
   90 SMALLV(I) = TEMP
C
      DO 95 I = 1,144
   95 KE(I) = 0.D0
      DO 230 IPVT = 1,4
      CON = AVEC(IPVT)*T**2/(24.*Z)
C
C     COMPUTE THE -VLEFT- VECTOR
C
      IVLBEG = 1
      VLEFT(1) = VI(1)*SMALLU(IPVT) + VJ(1)*SMALLV(IPVT)
      VLEFT(2) = VI(2)*SMALLU(IPVT) + VJ(2)*SMALLV(IPVT)
      VLEFT(3) = VI(3)*SMALLU(IPVT) + VJ(3)*SMALLV(IPVT)
      IF (IECPT(4*IPVT+5) .EQ. 0) GO TO 150
      CALL TRANSD (IECPT(4*IPVT+5),TI)
      IVLBEG = 4
      CALL GMMATD (TI,3,3,1, VLEFT(1),3,1,0, VLEFT(4))
C
C     COMPUTE THE 6 X 6 -S
C
  150 DO 220 J = 1,4
      JT = (IPVT-1)*36 + (J-1)*9 + 1
      IVRBEG = 1
      VRIGHT(1) = SMALLU(J)*VI(1) + SMALLV(J)*VJ(1)
      VRIGHT(2) = SMALLU(J)*VI(2) + SMALLV(J)*VJ(2)
      VRIGHT(3) = SMALLU(J)*VI(3) + SMALLV(J)*VJ(3)
      IF (IECPT(4*J+5) .EQ. 0) GO TO 170
      CALL TRANSD (IECPT(4*J+5),TI)
      CALL GMMATD (VRIGHT(1),1,3,0, TI,3,3,0, VRIGHT(4))
      IVRBEG = 4
  170 CALL GMMATD (VLEFT(IVLBEG),3,1,0, VRIGHT(IVRBEG),1,3,0, KE(JT))
      JT8 = JT + 8
      DO 180 K = JT,JT8
  180 KE(K)= CON*KE(K)*AVEC(J)
  220 CONTINUE
  230 CONTINUE
C
C     NOW REARRANGE KE BY INCREASING SIL THEN OUTPUT IT VIA EMGOUT
C     FIRST DETERMINE WHAT INCREASING SIL ORDER WILL BE
C
      ASSIGN 290TO  K OR M
  275 CONTINUE
      DO 280 I = 1,3
      IP1 = I + 1
      IT  = IPART (I)
      DO 270 J = IP1,4
      JT = IPART(J)
      IF (ISILNO(IT) .LE. ISILNO(JT)) GO TO 270
      IPART(I) = JT
      IPART(J) = IT
      IT = JT
      GO TO 275
 270  CONTINUE
 280  CONTINUE
      ISORT = 1
      GO TO  KORM, (290,420)
C
C     NOW REARRANGE TERMS IN THE STIFFNESS MATRIX KE AND STORE IN KOUT
C
C
C     KE = (K  ,K  ,K  ,K  ,K  ,...,K  ,K  ,...,K  )
C            11  12  13  14  21      24  31      44
C
C     WHERE  K  IS A 3X3 SUBMATRIX AND  SILS ARE IN GRID POINT ORDER
C             IJ
C
C     AND    *****                 ****
C            * K     K     K     K    *
C            *  L1L1  L1L2  L1L3  L1L4*
C            *                        *
C            * K     K     K     K    *
C     KOUT = *  L2L1  L2L2  L2L3  L2L4*
C            *                        *
C            * K     K     K     K    *
C            *  L3L1  L3L2  L3L3  L3L4*
C            *                        *
C            * K     K     K     K    *
C            *  L4L1  L4L2  L4L3  L4L4*
C            ****                  ****
C
C     WHERE  KOUT     IS A   3X3    MATRIX AND SILS ARE IN INCREASING
C                LILJ
C     ORDER
C
  290 CONTINUE
      DO 300 I = 1,4
      IS = IPART(I)
      DO 300 J = 1,4
      JS = IPART(J)
      DO 300 K = 1,3
      DO 300 L = 1,3
      IOUT = (I -1)*36 + (J -1)*3 + (K-1)*12 + L
      IKE  = (IS-1)*36 + (JS-1)*9 + (K-1)*3  + L
  300 KOUT(IOUT) = KE(IKE)
C
C     OUTPUT THE STIFFNESS MATRIX
C
      CALL EMGOUT (KOUT,KOUT,144,1,DICT,1,IP)
C
C     HERE WE CALCULATE THE MASS MATRIX VIA SUBROUTINE EMASTQ
C
C
  400 IF (ISMB(2) .EQ. 0) RETURN
C
      CALL EMADTQ (6,ME)
      IF (ISORT .EQ. 1) GO TO 420
      ASSIGN 420 TO KORM
      GO TO 275
C
C     RETURN WITH A GRID POINT SORT ARRAY IN IPART
C
C
  420 DO 440 I = 1,4
      IT = 1 + (IPART(I)-1)*3
      IJ = (I-1)*3 + 1
      MOUT(IJ  ) = ME(IT  )
      MOUT(IJ+1) = ME(IT+1)
  440 MOUT(IJ+2) = ME(IT+2)
C
      DICT(1) = ESTID
      DICT(2) = 2
      DICT(3) = 12
      DICT(4) = 7
      DICT5   = 0.
C
      CALL  EMGOUT (KOUT,KOUT,12,1,DICT,2,IP)
      RETURN
C
C     ERROR EXITS
C
 7770 CALL MESAGE (30,26,IECPT(1))
 7777 NOGO = .TRUE.
      RETURN
C
 7780 IECPT(2) = 2
      GO TO 7820
7790  IECPT(I) = 4
      GO TO 7820
 7800 IECPT(2) = 1
      GO TO 7820
 7810 IECPT(2) = 3
 7820 CALL MESAGE (30,27,IECPT(1))
      GO TO 7777
      END
