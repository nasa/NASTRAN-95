      SUBROUTINE CRSPLS (*,JUMP,MU,BP,RS,AGAIN,N23)
C
C     THIS ROUTINE HANDLES CRBE3 AND CRSPLINE RIGID ELEMENTS
C     CALLED ONLY BY CRIGGP SUBROUTINE
C
C     SINGLE PRECISION VERSION
C
      IMPLICIT INTEGER (A-Z)
      LOGICAL          AGAIN,DEBUG
      INTEGER          MCODE(2),NAME(2),SILD(6),RS(3)
      REAL             Z(1),ZK,WT,DL,COEFF
      REAL             X1,X2,X3,Y1,Y2,Y3,Z1,Z2,Z3,A(3),B(3),C(3),D(9),
     1                 LEN,LENG,ONE,ZERO,HALF,EPS,ESPX,ANS,DI,FAC,LN3,
     2                 T(36),TX(36),KNN(36),GNN(36),UNN(36),ZNN(36),
     3                 SNN(36),X(36),Y(36),W(6)
      CHARACTER        UFM*23
      COMMON /XMSSG /  UFM
      COMMON /SYSTEM/  SYSBUF,NOUT
      COMMON /GP4FIL/  GEOMP,BGPDT,CSTM,RGT
      COMMON /GP4PRM/  BUF(20),BUF1,BUF2,BUF3,BUF4,KNKL1,TWO16,NOGO,
     1                 GPOINT,KN
      COMMON /CRSPLY/  X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3
      COMMON /ZZZZZZ/  IZ(1)
      EQUIVALENCE      (Z(1),IZ(1)), (WT,IWT ), (DL,IDL ),
     1                 (X1  ,A(1) ), (X2,B(1)), (X3,C(1))
      DATA    ONE,     ZERO,    HALF,    EPS,     TIMES, DEBUG  /
     1        1.0,     0.0,     0.5,     1.0E-10, 0,     .FALSE./
      DATA    CM,CN,   NOGOX,   MASK15,  NAME            /
     1        6 ,12,   0,       32767,   4HCRSP,4HLS     /
C
      IF (AGAIN) RETURN 1
      IF (DEBUG) WRITE (NOUT,10) KN,KNKL1,BP,GEOMP,BGPDT,CSTM,RGT,JUMP
   10 FORMAT ('0  CRSPLS DEBUG- KN,KNKL1,BP,GEOMP,BGPDT,CSTM,RGT,JUMP=',
     1        /3X,8I7)
      KN2 = KN/2
      CALL SSWTCH (38,L38)
C
C     UNIT MATRIX UNN
C
      DO 20 I = 2,35
   20 UNN( I) = ZERO
      UNN( 1) = ONE
      UNN( 8) = ONE
      UNN(15) = ONE
      UNN(22) = ONE
      UNN(29) = ONE
      UNN(36) = ONE
C
C     JUMP = 1 FOR CRBE3 DATA,  JUMP = 2 FOR CRSPLINE DATA
C
      IF (JUMP .EQ. 2) GO TO 400
C
C     READ CRBE3 DATA ON INPUT FILE
C     =============================
C
C     CLEAR WORKING SPACE
C     READ INPUT CARD, SAVE BEGINING POINTER, BEGN, AND
C     COUNT NUMBER OF WORDS READ, NWDS, IN FIRST PASS
C     (EACH INPUT CARD WILL BE READ TWICE)
C
      BEGN = 3
   30 PASS = 1
      DO 40 I = 1,36
      T(I)   = ZERO
   40 KNN(I) = ZERO
      CALL READ (*1300,*1300,GEOMP,BUF,3,0,FLAG)
      NWDS = 3
      IF (DEBUG .OR. L38.EQ.1) WRITE (NOUT,50) BUF(1)
   50 FORMAT (5X,'ELEMENT',I8,' IS BEING PROCESSED')
      EID    = BUF(1)
      GPOINT = BUF(2)
      ASSIGN 60   TO RETN
      ASSIGN 1000 TO RETN1
      KX = BUF(3)
      NM = CN
      GO TO 950
   60 REFG = K
      SIL  = GPOINT
      DO 70 I = 1,6
   70 SILD(I) = SIL + I - 1
      X2 = Z(K+1)
      Y2 = Z(K+2)
      Z2 = Z(K+3)
C
C     READ WEIGHT FACTORS AND COMPONENTS.
C     GENERATE WEIGHT VECTOR W
C
   80 CALL READ (*1110,*1110,GEOMP,IWT,1,0,FLAG)
      IF (PASS  .EQ.  1) NWDS = NWDS + 1
      IF (IWT   .EQ. -2) GO TO 170
      IF (IWT   .EQ. -3) GO TO 240
      CALL READ (*1110,*1110,GEOMP,COMP,1,0,FLAG)
      IF (PASS .EQ. 1) NWDS = NWDS + 1
      ASSIGN 90 TO RETN1
      KX = COMP
      NM = CM
      GO TO 950
   90 DO 100 I = 1,6
      W(I) = ZERO
      IF (BUF(CM+I) .NE. 0) W(I) = WT
  100 CONTINUE
C
C     READ GRID POINT, GET TRANSFORMATION MATRIX, AND SUMMING UP
C     WT MATRIX, AND FINALLY KNN MATRIX
C
  110 CALL READ (*1110,*1110,GEOMP,GRID,1,0,FLAG)
      IF (PASS .EQ.  1) NWDS = NWDS + 1
      IF (GRID .EQ. -1) GO TO 80
      ASSIGN 120 TO RETN
      GPOINT = GRID
      GO TO 1000
  120 X1 = Z(K+1)
      Y1 = Z(K+2)
      Z1 = Z(K+3)
      ASSIGN 850 TO RETN2
      ASSIGN 130 TO RETN3
      ZK = Z(K)
      GO TO 800
  130 CALL GMMATS (T,6,6,0, UNN,6,6,0, X)
      IF (PASS .EQ. 2) GO TO 270
      DO 140 I = 1,36
  140 TX(I) = X(I)
      L = 0
      DO 160 I = 1,6
      DO 150 J = 1,6
  150 X(L+J) = X(L+J)*W(I)
  160 L = L + 6
      CALL GMMATS (TX,6,6,-1, X,6,6,0, KNN)
C
C     REPEAT FOR MORE GRID POINT
C
      GO TO 110
C
C     UM SET WAS SPECIFIED BY USER. REBUILD SILD WITH THE UM SET, AND
C     CHECK TOTAL NUMBER OF COMPONENTS FOR POSSIBLE ERROR
C
  170 IF (PASS .EQ. 2) GO TO 310
      JJ = 1
  180 CALL READ (*1110,*1110,GEOMP,GRID,1,0,FLAG)
      NWDS = NWDS + 1
      IF (GRID .EQ. -3) GO TO 240
      CALL READ (*1110,*1110,GEOMP,COMP,1,0,FLAG)
      NWDS = NWDS + 1
      ASSIGN 190 TO RETN1
      KX = COMP
      NM = CM
      GO TO 950
  190 GPOINT = GRID
      ASSIGN 200 TO RETN
      GO TO 1000
  200 DO 230 I = 1,6
      IF (BUF(CM+ I) .EQ. 0) GO TO 230
      IF (JJ .GT. 6) GO TO 1160
  210 IF (BUF(CN+JJ) .NE. 0) GO TO 220
      JJ = JJ + 1
      IF (JJ .GT. 6) GO TO 1160
      GO TO 210
  220 SILD(JJ) = GPOINT + I - 1
      JJ = JJ + 1
  230 CONTINUE
      GO TO 180
  240 IF (PASS .EQ. 2) GO TO 320
C
C     STORE DIAG TERMS WITH -1.
C     ADD DEPENDENT SIL TO THE END OF OPEN CORE VIA MU POINTER
C
      DO 250 I = 1,6
      IF (BUF(CN+I) .EQ. 0) GO TO 250
      MCODE(1) = SIL + I - 1
      MCODE(2) = SILD(I)
      COEFF    = -1.
      CALL WRITE (RGT,MCODE,2,0)
      CALL WRITE (RGT,COEFF,1,0)
      IZ(MU) = MCODE(2)
      MU = MU - 1
  250 CONTINUE
C
C     GET MATRIX READY FOR SECOND PASS, IN TX
C
      SING = -1
      CALL INVERS (6,KNN,6,0,0,LEN,SING,X)
      IF (SING .EQ. 2) GO TO 1120
      ASSIGN 260 TO RETN2
      ZK = Z(REFG)
      GO TO 800
  260 CALL GMMATS (KNN,6,6,0, T,6,6,0, TX)
C
C     BACK RECORD FOR 2ND PASS
C     SKIP TO WHERE WEIGHT FACTORS BEGIN
C
      CALL BCKREC (GEOMP)
      PASS = 2
      I = BEGN + 3
      CALL READ (*1110,*1110,GEOMP,BUF,-I,0,FLAG)
      GO TO 80
C
C     INSERT THIS GRID MPC EQUATIONS
C
  270 CALL GMMATS (TX,6,6,0, X,6,6,1, KNN)
      DO 280 I = 1,6
      DO 280 J = 1,31,6
      L = I + J - 1
      KNN(L) = KNN(L)*W(I)
  280 CONTINUE
      DO 300 I = 1,6
      IF (BUF(CN+I) .EQ. 0) GO TO 300
      SIL = SILD(I)
      L = (I-1)*6
      DO 290 J = 1,6
      IF (BUF(CM+J) .EQ. 0) GO TO 290
      ANS = KNN(L+J)
      IF (ANS .EQ. ZERO) GO TO 290
      MCODE(1) = GPOINT + J - 1
      MCODE(2) = SIL
      COEFF    = ANS
      CALL WRITE (RGT,MCODE,2,0)
      CALL WRITE (RGT,COEFF,1,0)
  290 CONTINUE
  300 CONTINUE
      GO TO 110
C
C     SKIP TO END OF CARD
C
  310 CALL READ (*1110,*1110,GEOMP,J,1,0,FLAG)
      IF (J .NE. -3) GO TO 310
C
C     UPDATE BEGIN POINTER, AND RETURN FOR ANOTHER RBE3 CARD
C
  320 BEGN = BEGN + NWDS
      GO TO 30
C
C
C     READ CRSPLINE DATA ON INPUT FILE
C     ================================
C
C     INPUT DATA WILL BE SAVED IN RS ARRAY
C     3 WORDS SAVED FOR EACH GRID - BGPDT POINTER, COMPONENT, AND SIL
C
  400 CALL READ (*1300,*1300,GEOMP,BUF,3,0,FLAG)
      EID = BUF(1)
      IDL = BUF(2)
      RS(1) = BUF(3)
      RS(2) =-1
      RS(3) = 0
      IF (DEBUG .OR. L38.EQ.1) WRITE (NOUT,50) BUF(1)
      K = 4
  410 CALL READ (*1110,*1110,GEOMP,RS(K),2,0,FLAG)
      IF (RS(K) .EQ. -1) GO TO 420
      RS(K+2) = 0
      K = K + 3
      IF (K .GT. MU) CALL MESAGE (-8,0,NAME)
      GO TO 410
C
C     END OF INPUT FOR THIS RIGID ELEMENT, NOW COMPUTE LENGTH, INTERNAL
C     NUMBER (BGPDT POINTER), AND CHANGE GRID TO SIL
C
  420 IF (K .LT. 8) GO TO 1100
      IF (DEBUG) CALL BUG1 ('RS-     ',310,RS,K)
      IEND = K - 1
      LEN  = ZERO
      ASSIGN 430 TO RETN
C
C     DO 460 I = 1,IEND,3
      I = 1
  425 GPOINT = RS(I)
      GO TO 1000
C
C     UPON RETURN FROM 1000, K IS BGPDT AND GPOINT IS SIL
C
  430 RS(I  ) = K
      RS(I+2) = GPOINT
      IF (DEBUG) WRITE (NOUT,440) I,GPOINT,K,Z(K+1)
  440 FORMAT (/10X,'@430  I, NEW GPOINT & K=',I4,2I6,E11.3)
C
      IF (I .NE. 1) GO TO 450
      X1 = Z(K+1)
      Y1 = Z(K+2)
      Z1 = Z(K+3)
      GO TO 460
  450 X2 = Z(K+1)
      Y2 = Z(K+2)
      Z2 = Z(K+3)
      LEN= LEN + SQRT((X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2)
      X1 = X2
      Y1 = Y2
      Z1 = Z2
  460 I  = I + 3
      IF (I .LT. IEND) GO TO 425
C
      DI = LEN*DL
      IS = 1
      IF (.NOT.DEBUG) GO TO 480
      CALL BUG1 ('RS-     ',345,RS,IEND)
      WRITE  (NOUT,470) LEN,DI
  470 FORMAT ('0  LEN,DI/@470 =',2E14.5)
C
C     COMPUTATION FOR EACH SEPARATED SPLINE
C     SET NUMBER OF SEGMENTS, NS
C
  480 NS = 0
      DO 490 I = IS,IEND,3
      IF (RS(I+1) .EQ. 0) GO TO 500
  490 NS = NS + 1
C
C     IB = BEGIN,  IE = END,  IS = PRESENT SEGMENT
C     ND = NUMBER OF DEPENDENT POINTS
C
C     ZERO MTRAIX WORKING SPACE KNN ,GNN, AND T
C
  500 IE = I
      IB = IS
      ND = NS - 1
      DO 510 I = 1,36
      KNN(I) = ZERO
      GNN(I) = ZERO
      T(I)   = ZERO
  510 CONTINUE
C
C     COMPUTE FLEXIBILITY MATRIX ZNN AND ITS INVERSE KNN, FOR EACH
C     SPLINE SEGMENT.
C
C     DO 540 I = 1,NS
      I = 0
  515 I = I + 1
      I1 = RS(IS  )
      I2 = RS(IS+3)
      ASSIGN 520 TO RETN4
      GO TO 900
  520 IF (NOGOX .EQ. 1) GO TO 540
      X2 = Z(I1+1)
      Y2 = Z(I1+2)
      Z2 = Z(I1+3)
      X1 = Z(I2+1)
      Y1 = Z(I2+2)
      Z1 = Z(I2+3)
      X2 = (X2+X1)*HALF
      Y2 = (Y2+Y1)*HALF
      Z2 = (Z2+Z1)*HALF
      I1 = RS(IE)
      X1 = Z(I1+1)
      Y1 = Z(I1+2)
      Z1 = Z(I1+3)
C
C     FORM UNN USING BASIC UNN MATRIX
C     DO NOT DESTROY RIGID TRANSFER MATRIX
C
      ASSIGN 530 TO RETN3
      GO TO 850
  530 CALL GMMATS (UNN,6,6,0, ZNN,6,6,0, SNN)
C
C     SUM INTO KNN
C
      CALL GMMATS (SNN,6,6,-2, UNN,6,6,1, KNN)
  540 IS = IS + 3
      IF (I .LT. NS) GO TO 515
C
      IF (NOGOX .EQ. 1) GO TO 730
C
C     INVERT KNN
C
      SING = -1
      CALL INVERS (6,KNN,6,0,0,LEN,SING,SNN)
      IF (SING .EQ. 2) GO TO 1120
C
C     LOOP FOR FINAL CONSTRAINT EQUATIONS
C
      IS = IB
      JS = RS(IS)
      II = 0
  545 II = II + 1
      I1 = RS(IS)
      ID = IS + 3
      I2 = RS(ID)
      ASSIGN 550 TO RETN4
      GO TO 900
  550 X1 = Z(I2+1)
      Y1 = Z(I2+2)
      Z1 = Z(I2+3)
      X2 = Z(I1+1)
      Y2 = Z(I1+2)
      Z2 = Z(I1+3)
      X3 = Z(I2+1)
      Y3 = Z(I2+2)
      Z3 = Z(I2+3)
      X2 = (X2+X3)*HALF
      Y2 = (Y2+Y3)*HALF
      Z2 = (Z2+Z3)*HALF
C
C     Y I+1 I X   S I+1 S
C
      ASSIGN 560 TO RETN3
      GO TO 850
  560 CALL GMMATS (UNN,6,6,0, ZNN,6,6,0, SNN)
      CALL GMMATS (SNN,6,6,0, UNN,6,6,1,   Y)
      X2 = Z(I1+1)
      Y2 = Z(I1+2)
      Z2 = Z(I1+3)
C
C     S I+1 I X GIN
C
      ASSIGN 570 TO RETN3
      GO TO 850
  570 CALL GMMATS (UNN,6,6,0, GNN,6,6,0, SNN)
      I3 = RS(IE)
      X3 = Z(I3+1)
      Y3 = Z(I3+2)
      Z3 = Z(I3+3)
      X2 = Z(JS+1)
      Y2 = Z(JS+2)
      Z2 = Z(JS+3)
C
C     GNN = G I+1 N
C
      ASSIGN 580 TO RETN3
      GO TO 860
  580 CALL GMMATS (Y  ,6,6,0, UNN,6,6,1, ZNN)
      CALL GMMATS (ZNN,6,6,0, KNN,6,6,0, GNN)
      DO 590 J = 1,36
  590 GNN(J) = GNN(J) + SNN(J)
C
C     Y = G I+1 1
C
      ASSIGN 600 TO RETN3
      GO TO 870
  600 CALL GMMATS (GNN,6,6,0, UNN,6,6,0, SNN)
      ASSIGN 610 TO RETN3
      GO TO 850
  610 DO 620 J = 1,36
  620 Y(J) = UNN(J) - SNN(J)
C
C     TRANSFORM TO GLOBAL AND STORE ANSWERS IN Y AND SNN
C
      ASSIGN 630 TO RETN2
      ZK = Z(I2)
      GO TO 800
  630 CALL GMMATS (T,6,6,1, Y  ,6,6,0, SNN)
      CALL GMMATS (T,6,6,1, GNN,6,6,0, ZNN)
      ASSIGN 640 TO RETN2
      ZK = Z(JS)
      GO TO 800
  640 CALL GMMATS (SNN,6,6,0, T,6,6,0, Y)
      ASSIGN 650 TO RETN2
      ZK = Z(I3)
      GO TO 800
  650 CALL GMMATS (ZNN,6,6,0, T,6,6,0, SNN)
C
C     Y = G I 1  SNN = G I N
C
      ASSIGN 660 TO RETN1
      KX = RS(ID+1)
      NM = CM
      GO TO 950
C
C     ADD DEPENDENT TO LIST AND MPC EQUATIONS TO RGT
C
  660 IF (.NOT.DEBUG) GO TO 680
      WRITE (NOUT,670) Y
      WRITE (NOUT,670) SNN
  670 FORMAT ('0  CRSPLS/@670',/,(2X,10E12.4))
  680 DO 710 J = 1,6
      IF (BUF(CM+J) .EQ. 0) GO TO 710
C
C     SELF TERM FOR DEPENDENT SIL
C
      SIL = RS(ID+2) + J - 1
      MCODE(1) = SIL
      MCODE(2) = SIL
      COEFF    = -1.
      CALL WRITE (RGT,MCODE,2,0)
      CALL WRITE (RGT,COEFF,1,0)
      IZ(MU) = MCODE(2)
      MU = MU - 1
      IF (II .GE. MU) CALL MESAGE (-8,0,NAME)
      LL = (J-1)*6
C
C     END ONE DEPENDENT
C
      DO 690 L = 1,6
      ANS = Y(LL+L)
C
C     TEST FOR COMPUTED ZERO
C
      ESPX = EPS
      IF (J.GT.3 .AND. L.LT.4) ESPX = ESPX/LENG
      IF (J.LT.4 .AND. L.GT.3) ESPX = ESPX*LENG
      IF (ABS(ANS) .LT. ESPX) GO TO 690
      MCODE(1) = RS(IB+2) + L - 1
      MCODE(2) = SIL
      COEFF    = ANS
      CALL WRITE (RGT,MCODE,2,0)
      CALL WRITE (RGT,COEFF,1,0)
  690 CONTINUE
C
C     END N INDEPENDENT
C
      DO 700 L = 1,6
      ANS = SNN(LL+L)
C
C     TEST FOR COMPUTED ZERO
C
      ESPX = EPS
      IF (J.GT.3 .AND. L.LT.4) ESPX = ESPX/LENG
      IF (J.LT.4 .AND. L.GT.3) ESPX = ESPX*LENG
      IF (ABS(ANS) .LT. ESPX) GO TO 700
      MCODE(1) = RS(IE+2) + L - 1
      MCODE(2) = SIL
      COEFF    = ANS
      CALL WRITE (RGT,MCODE,2,0)
      CALL WRITE (RGT,COEFF,1,0)
  700 CONTINUE
  710 CONTINUE
C
      IS = IS + 3
      IF (II .LT. ND) GO TO 545
C
C     END BIG DO (720) LOOP
C
  730 IF (IE+2 .GE. IEND) GO TO 400
      IS = IE
      RS(IS+1) = -1
      GO TO 480
C
C     ----------------------------------------------------
C
C     INTERNAL ROUTINE TO BUILD 6X6 BASIC TO GLOBAL MATRIX
C     (T = 0 ON ENTRY)
C
  800 CALL TRANSS (ZK,D)
      J = 1
      DO 810 I = 1,15,6
      T(I   ) = D(J  )
      T(I+ 1) = D(J+1)
      T(I+ 2) = D(J+2)
      T(I+21) = D(J  )
      T(I+22) = D(J+1)
      T(I+23) = D(J+2)
  810 J = J + 3
      GO TO RETN2, (260,850,630,640,650)
C
C     INTERNAL ROUTINE TO MAKE RIGID BODY TRANSFER MATRIX FOR CRSPLINES
C     (UNN = IDENTITY MATRIX ON ENTRY)
C
  850 UNN( 5) = A(3) - B(3)
      UNN( 6) = B(2) - A(2)
      UNN(10) = B(3) - A(3)
      UNN(12) = A(1) - B(1)
      UNN(16) = A(2) - B(2)
      UNN(17) = B(1) - A(1)
      GO TO 880
  860 UNN( 5) = C(3) - A(3)
      UNN( 6) = A(2) - C(2)
      UNN(10) = A(3) - C(3)
      UNN(12) = C(1) - A(1)
      UNN(16) = C(2) - A(2)
      UNN(17) = A(1) - C(1)
      GO TO 880
  870 UNN( 5) = C(3) - B(3)
      UNN( 6) = B(2) - C(2)
      UNN(10) = B(3) - C(3)
      UNN(12) = C(1) - B(1)
      UNN(16) = C(2) - B(2)
      UNN(17) = B(1) - C(1)
  880 GO TO RETN3, (130,530,560,570,580,600,610)
C
C     INTERNAL ROUTINE TO FORM FLEXIBILITY MATRIX FOR CRSPLINE
C
  900 DO 910 I = 1,36
  910 ZNN(I) = ZERO
      X1 = Z(I1+1)
      Y1 = Z(I1+2)
      Z1 = Z(I1+3)
      X2 = Z(I2+1)
      Y2 = Z(I2+2)
      Z2 = Z(I2+3)
      LENG = SQRT((X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2)
      IF (LENG .EQ. ZERO) GO TO 930
      ZNN(22) = LENG
      ZNN(29) = LENG
      ZNN(36) = LENG
      FAC = LENG/12.0*((3.0*DI**2)/(2.0*LENG**2)-ONE)
      LN3 = LENG**3/12.0
      ZNN( 1) = LN3 + FAC*(X2-X1)**2
      ZNN( 2) = FAC *(X2-X1)*(Y2-Y1)
      ZNN( 3) = FAC *(X2-X1)*(Z2-Z1)
      ZNN( 7) = ZNN(2)
      ZNN( 8) = LN3 + FAC*(Y2-Y1)**2
      ZNN( 9) = FAC*(Y2-Y1)*(Z2-Z1)
      ZNN(13) = ZNN(3)
      ZNN(14) = ZNN(9)
      ZNN(15) = LN3 + FAC*(Z2-Z1)**2
  920 GO TO RETN4, (520,550)
  930 CALL MESAGE (30,31,EID)
      NOGOX = 1
      GO TO 920
C
C     INTERNAL ROUTINE TO ABSTRACT CODED DOF
C
  950 DO 960 I = 1,6
      BUF(NM+I) = 0
  960 CONTINUE
      IF (KX .LE. 0) GO TO 980
      DO 970 I = 1,6
      K1 = KX/10
      K2 = KX - K1*10
      IF (K2 .GT. 6) GO TO 980
      BUF(NM+K2) = K2
      IF (K1  .EQ. 0) GO TO 980
  970 KX = K1
  980 GO TO RETN1, (90,190,1000,660)
C
C     INTERNAL ROUTINE TO PERFORM BINARY SEARCH IN EQEXIN AND
C     CONVERT THE EXTERNAL NUMBER TO A SIL VALUE
C
 1000 KLO = 0
      KHI = KN2
      LASTK = 0
 1010 K = (KLO+KHI+1)/2
      IF (LASTK .EQ. K) GO TO 1140
      LASTK = K
      IF (GPOINT-IZ(2*K-1)) 1020,1040,1030
 1020 KHI = K
      GO TO 1010
 1030 KLO = K
      GO TO 1010
 1040 K = IZ(2*K)
      GPOINT = IZ(K+2*KN)
      K = (K-1)*4 + BP
      IF (GPOINT+5 .GT. MASK15) N23 = 3
      GO TO RETN, (60,120,200,430)
C
C     ERROR MESSAGES
C
 1100 MSG = 131
      GO TO 1130
 1110 CALL MESAGE (-3,GEOMP,NAME)
 1120 MSG = 38
 1130 CALL MESAGE (30,MSG,EID)
      GO TO 1180
 1140 WRITE  (NOUT,1150) UFM,GPOINT,EID
 1150 FORMAT (A23,', UNDEFINED GRID POINT',I9,' SPECIFIED BY RIGID ',
     1       'ELEMENT ID',I9)
      TIMES = TIMES + 1
      IF (TIMES .GT. 50) CALL MESAGE (-37,0,NAME)
      GO TO 1180
 1160 WRITE  (NOUT,1170) UFM,EID
 1170 FORMAT (A23,', RIGID ELEMENT CRBE3',I9,' HAS ILLEGAL UM SET ',
     1        'SPECIFICATION')
      GO TO 1190
C
 1180 NOGO  = 1
      NOGOX = 0
      GO TO (30,400), JUMP
C
C     REPOSITION GEOMP FILE FOR NEXT CRBE3 INPUT CARD
C
 1190 NOGO  = 1
      NOGOX = 0
      CALL BCKREC (GEOMP)
      I = BEGN + 1
      CALL READ (*1110,*1110,GEOMP,J,-I,0,FLAG)
 1200 CALL READ (*1110,*1110,GEOMP,J, 1,0,FLAG)
      I = I + 1
      IF (J .NE. -3) GO TO 1200
      BEGN = I
      GO TO 30
C
 1300 IF (NOGOX .EQ. 1) NOGO = 1
      RETURN
      END
