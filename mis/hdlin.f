      SUBROUTINE HDLIN (X,Y,Z,NP,NC,
     1           XCC,ICOUNT,IRCT,X21,Y21,Z21,IIA,XE,YE,XU,YU,XI,YI,ZI,
     2           DI,IBEG,IEND,ICT,ICCT,
     3           IND,NIND,XXX,CCC,IN,IN1,IN2,TGM,TGMT,TGI,ZM,ZMI,RV,
     4           RVI,NNO,NOCT,YMIN,ZMIN,COORD,SNDT,NEH,KEEP)
C
C
C     THIS SUBROUTINE IS THE EXECUTIVE.
C
C
      DIMENSION     X(1),Y(1),Z(1),I2(2),I3(2),RRX(20),NGX(15),H(15),
     1              U(6),V(6),W(6),X1(10),Y1(10)
      DIMENSION     XCC(1),ICOUNT(1),IRCT(1),X21(1),Y21(1),Z21(1),
     1              IIA(1),XE(1),YE(1),XU(1),YU(1),XI(1),YI(1),ZI(1),
     2              DI(1),IBEG(1),IEND(1),ICT(1),ICCT(1)
      DIMENSION     IND(1),NIND(1),XXX(1),CCC(1),IN(1),IN1(1),IN2(1),
     1              TGM(1),TGMT(1),TGI(1),ZM(1),ZMI(1),RV(1),RVI(1),
     2              NNO(1),NOCT(1),YMIN(1),ZMIN(1),COORD(1),SNDT(1),
     3              NEH(1),KEEP(1)
      COMMON /GO3 / L0,L1,L00,L01,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,
     1              L13
      COMMON /HDSC/ SCX,YAW,ROLL,PIT,LZ,VP,JJJ,ICORE
      COMMON /HEDG/ JAT,ME,JT,VX,VX1,VX2,VX3,NN
C
      IF (VP .LT. 0.) GO TO 20
      HXX = .015
      AVA = .0
      HX1 = .001
      LC  = 10**6
      IXXX= 0
      IF (SCX .LT. 0.) IXXX = 1
      SCX = ABS(SCX)
C
C     INITIALIZE VARIABLES.
C
      LZ  = LZ*5
      SW1 = 0
      SW  = 0
      IDAV= 0
C
C     CALCULATE MAXIMUM ALLOWABLE ELEMENTS.
C
      IABC = ICORE/(25+LZ+4*JJJ)
      SCT  = 1.
      VP   = VP/SCT
      VPX  = ABS(VP)
      ISAVE= NC
      NC   = IABC
      L5   = 0
      L6   = NC
      L7   = 2*NC
      L8   = 3*NC
      L2   = 4*NC
      L3   = 5*NC
      L4   = 6*NC
      L00  = 7*NC
      L01  = 8*NC
      L1   = 9*NC
      L0   = 10*NC
      L9   = 11*NC
      L10  = 12*NC
      L11  = 13*NC
      L15  = 14*NC
      L16  = 15*NC
      L17  = 16*NC
      L18  = 19*NC
      L12  = 20*NC
      L13  = 25*NC
      L14  = L13 + LZ*NC
      DO 10 J = 1,NC
      RVI(L8+J) = 10**6
      TGM(L5+J) = 10**6
      RV (L7+J) =-RVI(L8+J)
      TGI(L6+J) =-TGM(L5+J)
      NOCT(L9+J)= 0
      ZM  (L2+J)= RV(L7+J)
      ZMI(L3+J) = RVI(L8+J)
      NIND(L16+J) = 0
      IND (L15+J) = J
      KEEP(L18+J) = 0
   10 CONTINUE
      NC  = ISAVE
      IK  = 0
      IKT = 0
      KR  = JJJ
      PI  = 3.1416/180.
      U(6)= SCX
      V(6)= SCX
      VP  =-VP
C
C     STORE EULERIAN ANGLES.
C
      XX   = YAW*PI
      YY   = ROLL*PI
      ZZ   = PIT*PI
      COSY = COS(YY)
      SINY = SIN(YY)
      COSZ = COS(ZZ)
      SINZ = SIN(ZZ)
      COSX = COS(XX)
      SINX = SIN(XX)
   20 CONTINUE
      NT  = NP-1
      IKK = IK+1
      IK  = IK+1
C
C     SET ERROR CODES, IF NECESSARY.
C
      IF (IKK .LE. IABC) GO TO 30
      SW = 1
   30 CONTINUE
      IF (NC .EQ. 0) GO TO 40
      IDAV = 1
      NC   =-SW1
      IF (SW .EQ. 0.) GO TO 50
      ICORE = (25+LZ+4*JJJ)*IKK
      NC    =-(SW+SW1)
   40 CONTINUE
   50 CONTINUE
      DO 60 J = 1,NP
      X21(J) = X(J)
      Y21(J) = Y(J)
      Z21(J) = Z(J)
   60 CONTINUE
C
C     STORE COORDINATES AND SET PEN POSITION WHENEVER ABS(Z)=9999.
C
      DO 70 J = 1,NT
      IIA(J) = 0
      IF (Z21(J) .NE. 9999.) GO TO 70
      IIA(J) = 1
      IXU = J - 2
      IBB = J - ISIGN(1,IXU)
      X21(J) = X21(IBB)
      Y21(J) = Y21(IBB)
      Z21(J) = Z21(IBB)
   70 CONTINUE
      IIA(NP) = 1
      Z21(NP) = Z21(NT)
      Y21(NP) = Y21(NT)
      X21(NP) = X21(NT)
      JXX = IKK
      I   = 1
      VL  = ABS(VP)
C
C     LOOP THAT DOES THE THREE DIMENSIONAL TRANSFORMATION ON THE
C     COORDINATES.
C
      JV = L14 + (IKK-1)*4*JJJ
      JT = 1
      DO 90 J = 1,NP
      XJ = X21(J)/SCT
      YJ = Y21(J)/SCT
      ZJ = Z21(J)/SCT
      U(I) = ZJ*(COSY*SINX) + XJ*(COSY*COSX) - YJ*SINY
      TW = YJ*COSY*COSZ
      TZ = XJ*( SINZ*SINX+SINY*COSZ*COSX)
      TY = ZJ*(-SINZ*COSX+SINY*COSZ*SINX)
      V(I) = TZ + TW + TY
      PT = YJ*COSY*SINZ
      PK = ZJ*( COSZ*COSX+SINY*SINZ*SINX)
      PS = XJ*(-COSZ*SINX+SINY*SINZ*COSX)
      ZJ = PK + PS + PT
      IF (ZJ .LT. VL) GO TO 80
      SW1 = 2
      VPX = AMAX1(ZJ,VPX)
      VPX = VPX + (.5/SCT)
   80 CONTINUE
      T = SW + SW1
      IF (T .NE. 0.) GO TO 90
C
C     CALCULATES PERSPECTIVE BASED ON VALUE VP(DV) FROM CALLING PROGRAM.
C
      HH = VL/(VL-ZJ)
      X21(J) = U(I)*HH
      Y21(J) = V(I)*HH
      Z21(J) = ZJ*HH
C
C     CALCULATES MAX/MIN VALUES OF EACH ELEMENT ON THE X,Y,Z DIMENSION
C
      RV (L7+JXX) = AMAX1(RV (L7+JXX),Y21(J))
      RVI(L8+JXX) = AMIN1(RVI(L8+JXX),Y21(J))
      TGI(L6+JXX) = AMAX1(TGI(L6+JXX),X21(J))
      TGM(L5+JXX) = AMIN1(TGM(L5+JXX),X21(J))
      ZM (L2+JXX) = AMAX1(ZM (L2+JXX),Z21(J))
      ZMI(L3+JXX) = AMIN1(ZMI(L3+JXX),Z21(J))
      COORD(JT+JV  ) = X21(J)
      COORD(JT+JV+1) = Y21(J)
      COORD(JT+JV+2) = Z21(J)
      COORD(JT+3+JV) = IIA(J)
      JT = JT + 4
   90 CONTINUE
      IF (IDAV .EQ. 1) VP = VPX*SCT
      IF (T   .NE. 0.) GO TO 400
      NOCT(L9+IKK) = NOCT(L9+IKK) + NP
      NS  = NP
      AVA = AVA + (TGI(L6+JXX)-TGM(L5+JXX))*(RV(L7+JXX)-RVI(L8+JXX))
      IF (IXXX .EQ. 1) GO TO 95
C
C     CALL SUBROUTINE WHICH CALCULATES BOTH THE EQUATIONS OF THE LINE
C     SEGMENTS AND POLYGONS.
C
      CALL HDCOEF (X21,Y21,Z21,XXX,JXX,NS,CCC,LZ)
C
C     CHECKS TO SEE IF ALL ELEMENTS(SETS) HAVE BEEN PASSED.
C
   95 CONTINUE
      IF (IDAV .EQ. 1) GO TO 100
      GO TO 400
  100 CONTINUE
      AVA = AVA/IKK
      DO 1301 J = 1,100
      ICCT(J) = 0
      ICT (J) = 0
      IRCT(J) = J - 1
      IBEG(J) = 1
      IEND(J) = 0
 1301 CONTINUE
      IAUG  = 50 + (IKK/10000)*2
      AMAXX =-999999.
      AMAXY =-999999.
      AMINX = 999999.
      AMINY = 999999.
      DO 1400 J = 1,IKK
      AMAXX = AMAX1(AMAXX,TGI(L6+J))
      AMAXY = AMAX1(AMAXY,RV (L7+J))
      AMINX = AMIN1(AMINX,TGM(L5+J))
      AMINY = AMIN1(AMINY,RVI(L8+J))
 1400 CONTINUE
      TMAX = (AMAXX-AMINX)*(AMAXY-AMINY)
      IBL  = TMAX/AVA
      IBL  = IBL/4
C
C     DETERMINES THE NUMBER OF GRID POINTS IN THE GRID.
C
C
      EN = IKK
      K  = (ALOG(EN)/ALOG(2.)) + .01
      K  = K + IAUG
      K  = MIN0(K,IBL)
      IF (K .LE. 1) K = 1
      T  = K
      R  = T**.5
      KS = R + .5
      S  = T/KS
      MS = S + .5
      N  = KS*MS
      MND= N + 1
      XMD= MND
      T  = 3./(MND-1)
      IGY= T*IKK
      K  = KS
      K1 = MS
      CRX= (AMAXX-AMINX)/K
      CRY= (AMAXY-AMINY)/K1
C
C
C     DETERMINES THE RELEVANT ELEMENTS VIA THE GRID BLOCKS.
C
C
      DO 3 J = 1,IKK
      IA = 0
      XMAT = TGI(L6+J)
      XMIT = TGM(L5+J)
      YMAT =  RV(L7+J)
      YMIT = RVI(L8+J)
      M = 0
      DO 1 I = 1,K1
      DO 2 L = 1,K
      M  = M + 1
      S  = XMAT - ((L-1)*CRX+AMINX)
      S1 = XMAT - (L*CRX+AMINX)
      R  = XMIT - ((L-1)*CRX+AMINX)
      R1 = XMIT - (L*CRX+AMINX)
      A  = YMAT - ((I-1)*CRY+AMINY)
      A1 = YMAT - (I*CRY+AMINY)
      B  = YMIT - ((I-1)*CRY+AMINY)
      B1 = YMIT - (I*CRY+AMINY)
      IF (S.LE.0. .OR. R1.GE.0.) GO TO 2
      IF (A.LE.0. .OR. B1.GE.0.) GO TO 2
      IF (S*S1.GT.0. .OR. R*R1.GT.0.) GO TO 4
      IF (A*A1.GT.0. .OR. B*B1.GT.0.) GO TO 4
      NIND(L16+J) = M
      GO TO 3
    4 CONTINUE
      IA = IA + 1
      IF (IA .LE. 4) GO TO 8000
      NIND(J+L16) = 0
      GO TO 8001
 8000 CONTINUE
      NIND(L16+J) = NIND(L16+J) + M*(MND**(IA-1))
 8001 CONTINUE
      IF (ICCT(M) .LT. 0) GO TO 2
      ICCT(M) = ICCT(M) + 1
      JK      = (M-1)*IGY+ICCT(M) + L17
      NEH(JK) = J
      IF (ICCT(M) .GE. IGY) ICCT(M) = -1
    2 CONTINUE
    1 CONTINUE
    3 CONTINUE
      CALL HDVS1 (NIND(L16+1),IK,IND(L15+1))
      SW = 0
      L  = 1
      DO 5 I = 1,IKK
   11 CONTINUE
      IF (NIND(L16+I) .NE. IRCT(L)) GO TO 6
      SW = SW + 1
      IF (SW .EQ. 1.) LT = I
      ICT(L) = ICT(L) + 1
      GO TO 5
    6 CONTINUE
      IF (SW .NE. 0.) GO TO 8
      L = L + 1
      GO TO 11
    8 CONTINUE
      IBEG(L) = LT
      IEND(L) = LT + ICT(L) - 1
      SW = 0
      IF (NIND(L16+I) .GE. MND) GO TO 2110
      L = L + 1
      GO TO 11
    5 CONTINUE
      IBEG(L) = LT
      IEND(L) = LT + ICT(L) - 1
 2110 CONTINUE
      DO 2111 J = 1,IKK
      SNDT(L4+J) = IND(L15+J)
 2111 CONTINUE
      CALL HDVSR (SNDT(L4+1),IK,NIND(L16+1))
      EN  = IKK
      IGX = (ALOG(EN)/ALOG(2.)) + 1.
      DO 105 J = 1,IGX
      RRX(J) = 2**(IGX-J)
  105 CONTINUE
      U(6) = SCX
      V(6) = SCX
      W(6) = SCX
      IKT  = NC
      T    = AMINY
      T1   = AMINX
      V(5) = T
      U(5) = T1
      IJ   = 0
      X1(3)= U(5)
      Y1(3)= V(5)
      X1(4)= U(6)
      Y1(4)= V(6)
      X1(4)= X1(4)/SCT
      Y1(4)= Y1(4)/SCT
      DO 115 J = 1,IKK
      IN(L11 +J) = J
      IN1(L0 +J) = J
      IN2(L00+J) = J
      TGMT(L10+J) = TGM(L5+J)
      YMIN(L1 +J) = RVI(L8+J)
      ZMIN(L01+J) = ZM(L2+J)
  115 CONTINUE
C
C     CALL SUBROUTINE WHICH WILL SORT ON X,Y AND Z.
C
      CALL HDVSR (TGMT(L10+1),IK,IN(L11+1))
      CALL HDVSR (YMIN(L1+1),IK,IN1(L0+1))
      CALL HDVSR (ZMIN(L01+1),IK,IN2(L00+1))
      H(8) = 0
      DO 395 J = 1,IKK
      KS = IKK
      JJ = L14 + (J-1)*4*JJJ
      JH = 1
      II = 0
      IXR= NOCT(L9+J)
      NIT= 0
      JT = L12 + 5*(J-1)
      JO = L13 + LZ*(J-1)
      IF (IXXX .EQ. 1) GO TO 200
      NS = XXX(5+JT)
      NG = NS*5
      A3 = XXX(1+JT)
      B3 = XXX(2+JT)
      C3 = XXX(3+JT)
      D3 = XXX(4+JT)
      I  = 0
      DO 121 IX = 1,NG,5
      IF (IXR .LE. 3) GO TO 121
      I = I + 1
      XE(I) = CCC(IX+3+JO)
      IF (CCC(IX+JO) .NE. 0.) GO TO 120
      XE(I) =-CCC(IX+2+JO)
      YE(I) = CCC(IX+3+JO)
      GO TO 121
  120 CONTINUE
      YE(I) =-CCC(IX+2+JO) - CCC(IX+1+JO)*XE(I)
  121 CONTINUE
C
C     THIS LOOP DETERMINES THE RELEVANT ELEMENTS AS THEY RELATE TO A
C     PARTICULAR ELEMENT.  THAT IS, EACH ELEMENT HAS ASSOCIATED WITH IT
C     THOSE OTHER ELEMENTS WHICH COULD POSSIBLY HIDE SOME PORTION
C     OF THE GIVEN ELEMENT.
C
      K  = 2**IGX
      K1 = K
      K2 = K
C
C     DO LOGARITHMIC SEARCH TO DETERMINE RELEVANT ELEMENTS.
C
      S = -1
      DO 131 I = 1,IGX
      K = K + SIGN(RRX(I),S)
      IF (K .GT. IKK) K = IKK
      S  = TGI(L6+J) - TGMT(L10+K  )
      S1 = TGI(L6+J) - TGMT(L10+K-1)
      IF (S*S1 .LE. 0.) GO TO 132
  131 CONTINUE
      K = IKK
  132 CONTINUE
      S = -1
      DO 133 I = 1,IGX
      K1 = K1 + SIGN(RRX(I),S)
      IF (K1 .GT. IKK) K1 = IKK
      S  = RV(L7+J) - YMIN(L1+K1  )
      S1 = RV(L7+J) - YMIN(L1+K1-1)
      IF (S*S1 .LE. 0.) GO TO 134
  133 CONTINUE
      K1 = IKK
  134 CONTINUE
      S = -1
      DO 135 I = 1,IGX
      K2 = K2 + SIGN(RRX(I),S)
      IF (K2 .LE.   1) K2 = 2
      IF (K2 .GT. IKK) K2 = IKK
      S  = ZMI(L3+J) - ZMIN(L01+K2  )
      S1 = ZMI(L3+J) - ZMIN(L01+K2-1)
      IF (S*S1 .LE. 0.) GO TO 136
  135 CONTINUE
      K2 = 1
  136 CONTINUE
      I1 = IKK - K2 + 1
C
C     RETRIEVE THE RELEVANT ELEMENTS DETERMINED FROM SCHEME 1.
C
      IF  (NIND(L16+J) .EQ. 0) GO TO 1270
      IR = NIND(L16+J)
      VX = NIND(L16+J)
      T  = ALOG(VX)
      IF (NIND(L16+J) .LE. LC) GO TO 1800
      E  = LC
      LG = NIND(L16+J)/LC
      MU = MOD(IR,LC)
      UX = LG + (MU/E)
      T  = ALOG(UX) + ALOG(E)
 1800 CONTINUE
      IXT = 0
      IEXP= (T/ALOG(XMD)) + 1
      DO 8004 L = 1,IEXP
      IV = IR/(MND**(IEXP-L))
      IR = IR - IV*(MND**(IEXP-L))
      IV = IV + 1
      IF (ICCT(IV-1) .EQ. 0) GO TO 4000
      IF (ICCT(IV-1) .GT. 0) GO TO 4001
      GO TO 1270
 4001 CONTINUE
      KE = ICCT(IV-1)
      IL = 0
      JTT= (IV-2)*IGY + L17
      DO 4003 I = 1,KE
      KV = NEH(I+JTT)
      IF (KEEP(L18+KV) .EQ. J) GO TO 4003
      IL = IL + 1
      NNO(L4+IXT+IL) = KV
      KEEP(L18+KV) = J
 4003 CONTINUE
      IXT = IXT + IL
 4000 CONTINUE
      IX  = IBEG(IV)
      IX1 = IEND(IV)
      DO 1170 I = IX,IX1
 1170 NNO(L4+IXT+I-IX+1) = IND(L15+I)
      IXT = IXT + IX1 - IX + 1
 8004 CONTINUE
      KS = IXT
 1270 CONTINUE
      IM = MIN0(I1,K,K1)
C
C     PICK MINIMUM COUNT FROM BOTH SCHEMES.
C
      IF (KS .LT. IM) GO TO 129
      IF (IM .EQ. I1) GO TO 1000
      IF (IM .EQ.  K) GO TO 1001
      IF (IM .EQ. K1) GO TO 1002
 1000 CONTINUE
      KS = I1
      DO 1003 I = 1,KS
 1003 NNO(L4+I) = IN2(L00+IKK-I+1)
      GO TO 129
 1001 CONTINUE
      KS = K
      DO 1004 I = 1,KS
 1004 NNO(L4+I) = IN(L11+I)
      GO TO 129
 1002 CONTINUE
      KS = K1
      DO 1006 I = 1,KS
 1006 NNO(L4+I) = IN1(L0+I)
  129 CONTINUE
      DO 170 I = 1,KS
      IT = 0
      JB = NNO(L4+I)
      IF (J .EQ. JB) GO TO 170
      JK = L13 + LZ*(JB-1)
      JS = L12 +  5*(JB-1)
      IF (TGM(L5+J).GE.TGI(L6+JB) .OR. TGI(L6+J).LE.TGM(L5+JB))
     1    GO TO 170
      IF (RV(L7+J).LE.RVI(L8+JB) .OR. RVI(L8+J).GE.RV(L7+JB)) GO TO 170
      IF (ZMI(L3+J) .GE. ZM(L2+JB)) GO TO 170
      NV = XXX(5+JS)
      IF (XXX(JS+3) .EQ. 0.) GO TO 170
      IF (XXX(3+JT) .EQ. 0.) GO TO 165
      NB = 5*NV
C
C
C     TEST TO SEE IF ALL VERTICES LIE EITHER BEHIND OR IN FRONT OF
C     THE GIVEN POLYGON.
C
C
      M = 0
      DO 145 IX = 1,NB,5
      M = M + 1
      A = CCC(IX+3+JK)
      IF (CCC(IX+JK) .NE. 0.) GO TO 130
      A =-CCC(IX+2+JK)
      B = CCC(IX+3+JK)
      GO TO 140
  130 CONTINUE
      B =-CCC(IX+2+JK) - CCC(IX+1+JK)*A
  140 CONTINUE
      XU(M) = A
      YU(M) = B
      VX  = XXX(4+JS)
      VX1 = XXX(2+JS)*B
      VX2 = XXX(1+JS)*A
      ZS  =-(VX+VX1+VX2)/XXX(3+JS)
      VX  = XXX(4+JT)
      VX1 = XXX(2+JT)*B
      VX2 = XXX(1+JT)*A
      ZS1 =-(VX+VX1+VX2)/XXX(3+JT)
      IF (ABS(ZS-ZS1) .LT. HXX) GO TO 145
      IT = IT + 1
      ICOUNT(IT) = 0
      IF (ZS .GT. ZS1) ICOUNT(IT) = 1
  145 CONTINUE
C
C
C     TESTS FOR SEMI-RELEVANT PLANES.  THAT IS,NEGATIVE INDEXES
C     INDICATE ELEMENT IS TO BE USED FOR VISIBILITY TEST, BUT NOT FOR
C     INTERSECTION LINE DETERMINATION.
C
C
      IF (IT .EQ. 0) GO TO 170
      L = 0
      DO 150 M = 1,IT
  150 L = L + ICOUNT(M)
      IF (L  .EQ.  0) GO TO 170
      IF (L  .EQ. IT) JB = -JB
      IF (II .NE.  0) GO TO 165
C
C
C     INTERROGATE THE RELATIONSHIP OF THE CANDIDATE POLYGON TO THE
C     GIVEN POLYGON BY DETERMINING IF THE PROJECTION OF ONE POLYGON
C     CAN BE SEPARATED BY AN EDGE FROM THE OTHER'S PROJECTION
C
C
      C3 = XXX(3+JT)
      C4 = XXX(3+JS)
      SD = 0
      I3(1) = JK
      I3(2) = JO
      I2(1) = NV*5
      I2(2) = NS*5
      DO 164 KU = 1,2
      IS = I3(KU)
      IB = I2(KU)
      DO 163 L = 1,IB,5
  151 CONTINUE
      IF (SD .EQ. 1.) GO TO 152
      A = XXX(2+JT)*C4 - XXX(2+JS)*C3
      B = XXX(1+JT)*C4 - XXX(1+JS)*C3
      C = XXX(4+JT)*C4 - XXX(4+JS)*C3
      GO TO 153
  152 CONTINUE
      A = CCC(L+IS  )
      B = CCC(L+IS+1)
      C = CCC(L+IS+2)
  153 CONTINUE
      IF (A.EQ.0. .AND. B.EQ.0.) GO TO 162
      IF (A .NE. 0.) GO TO 154
      A = 0
      C = C/B
      B = 1
      GO TO 155
  154 CONTINUE
      B = B/A
      C = C/A
      A = 1
  155 CONTINUE
      M = 0
      R1= 0
      DO 158 IX = 1,NV
      M = M + 1
      YG= YU(M)
      IF (A .NE. 0.) GO TO 156
      DY = -C/B
      YG = XU(M)
      GO TO 157
  156 CONTINUE
      DY = -C - B*XU(M)
  157 IF (ABS(DY-YG) .LT. HXX) GO TO 158
      R = YG - DY
      IF (R*R1 .LT. 0.) GO TO 162
      R1 = R
  158 CONTINUE
      M  = 0
      R2 = 0
      DO 161 IX = 1,NS
      M  = M + 1
      YG = YE(M)
      IF (A .NE. 0.) GO TO 159
      DY = -C/B
      YG = XE(M)
      GO TO 160
  159 CONTINUE
      DY = -C - B*XE(M)
  160 IF (ABS(DY-YG) .LT. HXX) GO TO 161
      R  = YG - DY
      IF (R*R2 .LT. 0.) GO TO 162
      R2 = R
  161 CONTINUE
      IF (R1*R2 .LT. 0.) GO TO 170
  162 CONTINUE
      IF (SD .NE. 0.) GO TO 163
      SD = 1
      GO TO 151
  163 CONTINUE
  164 CONTINUE
  165 CONTINUE
      II = II + 1
      NNO(L4+II) = JB
  170 CONTINUE
      JS  = 1
      JAT =-4
      JT  = L12 + (J-1)*5
      NN  = XXX(JT+5)
      VX  = XXX(JT+4)
      VX1 = XXX(2+JT)
      VX2 = XXX(1+JT)
      VX3 = XXX(3+JT)
      IF (IXR .LE. 2) GO TO 200
      IF (II  .EQ. 0) GO TO 190
C
C     CALL SUBROUTINE WHICH SOLVES FOR THE LINES OF INTERSECTION,IF ANY,
C     OF THE JTH ELEMENT WITH OTHER ELEMENTS.
C
      CALL HDSOLV(IXR,J,XXX,CCC,II,NNO,NIT,X21,Y21,Z21,IIA,NC,ZM,ZMI,LZ)
  190 CONTINUE
  200 CONTINUE
      DO 210 JM = 1,IXR
      X21(JM) = COORD(JH  +JJ)
      Y21(JM) = COORD(JH+1+JJ)
      Z21(JM) = COORD(JH+2+JJ)
      IIA(JM) = COORD(JH+3+JJ)
      JH = JH + 4
  210 CONTINUE
      IXR = IXR + 3*NIT
      IF (II   .EQ. 0) GO TO 220
      IF (IXXX .NE. 1) GO TO 240
  220 CONTINUE
      DO 230 JM = 1,IXR
      X1(2) = X21(JM)
      Y1(2) = Y21(JM)
      IM = IIA(JM)
      CALL HDPLT (X1,Y1,IJ,IM)
  230 CONTINUE
      GO TO 390
  240 CONTINUE
      JX = 1
  250 CONTINUE
C
C     PLOTS IF IIA(JX+1) IS EQUAL TO 1.
C
      IF (IIA(JX).EQ.0 .AND. IIA(JX+1).EQ.0) GO TO 260
      IM    = IIA(JX+1)
      X1(2) = X21(JX+1)
      Y1(2) = Y21(JX+1)
      CALL HDPLT (X1,Y1,IJ,IM)
      JX = JX + 2
      IF (JX .GE. IXR) GO TO 390
      GO TO 250
  260 CONTINUE
      JAT = JAT + 5
      ME  = 0
C
C     CALL SUBROUTINE WHICH DETERMINES THE POINTS OF INTERSECTIONS
C     OF THE LINES OF THE JTH SET WITH THE RELEVANT LINES AND PLANES
C     OF OTHER ELEMENTS.
C
      CALL HDCHK (XXX,CCC,NNO,II,XI,YI,NGX,ZM,ZMI,RV,RVI,TGM,TGI,ZI,LZ,
     1            XCC)
      IF (JS .NE. 1) STOP 'MY GOSH. JS IS NOT 1 /HDLIN'
      NG = NGX(JS) + 2
      XI(1)  = X21(JX)
      YI(1)  = Y21(JX)
      ZI(1)  = Z21(JX)
      XI(NG) = X21(JX+1)
      YI(NG) = Y21(JX+1)
      ZI(NG) = Z21(JX+1)
      IF (NG .LE. 3) GO TO 340
C
C     THE FOLLOWING CODE SORTS THE INTERSECTION POINTS IN ASCENDING
C     ORDER OF OCCURENCE AND THEN SHRINKS THE LIST IF REDUNDANCY EXIST.
C
      NI  = NG - 2
      NII = NI
      DO 270 M = 1,NG
      DI(M) = (XI(M)-XI(1))**2
      PPPPP = (YI(M)-YI(1))**2
      DI(M) = DI(M) + PPPPP
  270 CONTINUE
      DO 290 M = 2,NI
      DO 280 MX= 2,NII
      IF (DI(MX) .LE. DI(MX+1)) GO TO 280
      HOLD   = DI(MX)
      HOLD1  = XI(MX)
      HOLD2  = YI(MX)
      HOLD3  = ZI(MX)
      XI(MX) = XI(MX+1)
      YI(MX) = YI(MX+1)
      ZI(MX) = ZI(MX+1)
      DI(MX) = DI(MX+1)
      DI(MX+1) = HOLD
      XI(MX+1) = HOLD1
      YI(MX+1) = HOLD2
      ZI(MX+1) = HOLD3
  280 CONTINUE
      NII = NII - 1
  290 CONTINUE
      LX  = 1
      NPX = NG
  300 NPX = NPX - 1
      I   = LX
      DO 320 M = I,NPX
      RX  = 0
      T   = XI(M) - XI(M+1)
      T1  = YI(M) - YI(M+1)
      T   = (T**2+T1**2)**.5
      IF (T .GT. HX1) GO TO 320
      IX  = M
      IX1 = NPX
      DO 310 MX = IX,IX1
      XI(MX) = XI(MX+1)
      YI(MX) = YI(MX+1)
      ZI(MX) = ZI(MX+1)
  310 CONTINUE
      RX = 1
      LX = M
      IF (LX .EQ. NPX) GO TO 330
      GO TO 300
  320 CONTINUE
  330 CONTINUE
      IF (RX .EQ. 1.) NPX = NPX - 1
      NG = NPX + 1
  340 CONTINUE
C
C     THIS CODE DETERMINES THE HDSTUS(VISIBILITY) OF EVERY OTHER POINT
C     AS SUGGESTED BY THE THEOREM IN THE TECHNICAL REPORT.
C
      DO 350 L = 1,NG,2
C
      OJ  = XI(L)
      TMJ = YI(L)
      ZJ  = ZI(L)
      CALL HDSTUS (OJ,TMJ,XXX,TGM,RV,RVI,TGI,ZM,NNO,II,H,IM,JXT,ZJ,NC,
     1             ZMI,CCC,LZ)
      DI(L) = IM
  350 CONTINUE
      DO 370 L = 1,NG,2
      IF (L .EQ. NG  ) GO TO 370
      IF (L .EQ. NG-1) GO TO 360
      C = DI(L) + DI(L+2)
      IF (C .NE. 2.) GO TO 360
      DI(L+1) = DI(L)
      GO TO 370
  360 OJ  = XI(L+1)
      TMJ = YI(L+1)
      ZJ  = ZI(L+1)
      CALL HDSTUS (OJ,TMJ,XXX,TGM,RV,RVI,TGI,ZM,NNO,II,H,IM,JXT,ZJ,NC,
     1             ZMI,CCC,LZ)
      DI(L+1) = IM
  370 CONTINUE
C
C     THE FOLLOWING CODE ACTUALLY PLOTS THE POINTS ON A GIVEN LINE
C     GOVERNED BY THE VALUE(IM) RETURNED BY HDSTUS SUBROUTINE.
C     1 MEANS INVISIBLE,...0 MEANS VISIBLE.
C
      DO 380 L = 1,NG
      X1(2) = XI(L)
      Y1(2) = YI(L)
      IM = DI(L)
      CALL HDPLT (X1,Y1,IJ,IM)
      IF (L .EQ. NG) GO TO 380
      C = DI(L) + DI(L+1)
      IF (C .GT. 0.) GO TO 380
      H(8) = 1
      OJ   = (XI(L)+XI(L+1))/2
      TMJ  = (YI(L)+YI(L+1))/2
      ZJ   = (ZI(L)+ZI(L+1))/2
      CALL HDSTUS (OJ,TMJ,XXX,TGM,RV,RVI,TGI,ZM,NNO,II,H,IM,JXT,ZJ,NC,
     1             ZMI,CCC,LZ)
      H(8) = 0
      X1(2)= OJ
      Y1(2)= TMJ
      CALL HDPLT (X1,Y1,IJ,IM)
  380 CONTINUE
      JX = JX + 1
      GO TO 250
  390 CONTINUE
C
C     DECREMENTS THE COUNT OF THE NUMBER OF LINES IN THE JTH SET
C     SINCE THE LINES OF INTERSECTIONS WERE ADDED TO THIS ELEMENT
C     BY THE SUBROUTINE SOLVE.
C
      XXX(5+JT) = XXX(5+JT) - NIT
  395 CONTINUE
  400 CONTINUE
      RETURN
      END
