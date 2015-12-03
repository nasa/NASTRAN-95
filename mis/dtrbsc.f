      SUBROUTINE DTRBSC (IOPT,NPIVOT)
C
C     IOPT = 1  IMPLIES THAT A CLOUGH TRIANGLE IS CALLING
C     IOPT = 2  IMPLIES THAT A QUADRILATERAL IS CALLING
C
C     ECPT LISTS OF NECESSARY VARIABLES
C
C     POSITION     TRIA1      QUAD1
C     ========     =====      =====
C     ECPT(51)      EID        EID
C     ECPT(52)      SIL1       SIL1
C     ECPT(53)      SIL2       SIL2
C     ECPT(54)      SIL3       SIL3
C     ECPT(55)      THETA      SIL4
C     ECPT(56)      MATID1     THETA
C     ECPT(57)      T1         MATID1
C     ECPT(58)      MATID2     T1
C     ECPT(59)      EYE        MATID2
C     ECPT(60)      MATID3     EYE
C     ECPT(61)      T2         MATID3
C     ECPT(62)      NSMASS     T2
C       :
C     ECT.
C
      DOUBLE PRECISION G,G2X2,AR,EYE,XBAR,YBAR,XCSQ,YCSQ,XBSQ,XCYC,PX2,
     1                 PY2,PXY2,XBAR3,YBAR3,YBAR2,T2,R,SP,T,U,R2,S2,DI,
     2                 DUMDP,C,A,D,S,J2X2,DETERM
      DOUBLE PRECISION XB2,XC2,YC2,XBC,SX,SY,SXY,XSUBB,XSUBC,YSUBC
      DIMENSION        D(9),DI(5,5),J2X2(4),S(18),NECPT(51),A(144)
      COMMON /MATIN /  MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/  G11,G12,G13,G22,G23,G33,RHO,ALPHA1,ALPHA2,ALPH12,
     1                 TSUBD,GSUBE,SIGTEN,SIGCON,SIGSHE,G2X211,G2X212,
     2                 G2X222
      COMMON /DS1ADP/  G(9),G2X2(4),AR,EYE,XBAR,YBAR,XCSQ,YCSQ,XBSQ,XCYC
     1,                PX2,PY2,PXY2,XBAR3,YBAR3,YBAR2,T2,R,SP,T,U,R2,S2,
     2                 DUMDP(81),C(24,3),SX,SY,SXY,XSUBB,XSUBC,YSUBC
      COMMON /DS1AET/  ECPT(100)
      EQUIVALENCE      (A(1),D(1),G(1)),(NECPT(1),ECPT(1)),
     1                 (J2X2(1),DUMDP(1)),(DI(1,1),G(1))
C
C//////
C     CALL BUG (4HTBIG,30,SX,12)
C//////
C     IF NO TRANSVERSE SHEAR FLEXIBILITY EXISTS THE H-INVERSE IS
C     CALCULATED DIRECTLY.  TEST AS FOLLOWS
C
      IF (ECPT(IOPT+60).NE.0.0 .AND. NECPT(IOPT+59).NE.0) GO TO 30
C
C     THE H-INVERSE MATRIX IS GENERATED IN TWO PARTITIONS
C         HB IS IN POSITIONS C(7,2) TO C(24,2)
C         HC IS IN POSITIONS C(7,3) TO C(24,3)
C
   10 NOHYQ = 1
      R  = 1.0/XSUBB
      SP = 1.0/YSUBC
      T  = SP*XSUBC
      U  = R*R*SP*T
      R2 = R*R
      S2 = SP**2
C
      DO 20 I = 1,72
   20 C(I,1) = 0.0D0
C
      C(7 ,2) = 3.0D0*R2
      C(9 ,2) = R
      C(11,2) = R
      C(13,2) =-C(7,2)*T**2
      C(14,2) =-R*T
      C(15,2) = C(14,2)*T
      C(16,2) =-2.0D0*R2*R
      C(18,2) =-R2
      C(19,2) =-6.0D0*R*U*(XSUBB-XSUBC)
      C(20,2) =-R*SP
      C(21,2) = U*(3.0D0*XSUBC -2.0D0*XSUBB)
      C(22,2) = R*T*U*(6.0D0*XSUBB - 4.0D0*XSUBC)
      C(23,2) = R*SP*T
      C(24,2) = 2.0D0*T*U*(XSUBB - XSUBC)
C
      C(13,3) = 3.0D0*S2
      C(14,3) =-SP
      C(15,3) = SP*T
      C(21,3) =-S2
      C(22,3) =-2.0D0*S2*SP
      C(23,3) = S2
      GO TO 110
C
C     THE  MATERIAL COEFFICIENTS FOR TRANSVERSE SHEAR ARE CALCULATE HERE
C     AND THE H-INVERSE MATRIX IS GENERATED THE NORMAL WAY
C
C     GET THE G2X2 MATRIX
C
   30 MATID  = NECPT(IOPT+59)
      INFLAG = 3
      CALL MAT (ECPT(51))
      IF (G2X211.EQ.0. .AND. G2X212.EQ.0. .AND. G2X222.EQ.0.) GO TO 10
      T2      = ECPT(IOPT+60)
      G2X2(1) = G2X211*T2
      G2X2(2) = G2X212*T2
      G2X2(3) = G2X212*T2
      G2X2(4) = G2X222*T2
C
      DETERM  = G2X2(1)*G2X2(4) - G2X2(3)*G2X2(2)
      J2X2(1) = G2X2(4)/DETERM
      J2X2(2) =-G2X2(2)/DETERM
      J2X2(3) =-G2X2(3)/DETERM
      J2X2(4) = G2X2(1)/DETERM
C
C     SETTING UP G MATRIX
C
      INFLAG = 2
      MATID  = NECPT(IOPT+57)
      CALL MAT (NECPT(51))
C
C     FILL G-MATRIX WITH OUTPUT FROM MAT ROUTINE
C
      G(1) = G11
      G(2) = G12
      G(3) = G13
      G(4) = G12
      G(5) = G22
      G(6) = G23
      G(7) = G13
      G(8) = G23
      G(9) = G33
C
C     COMPUTATION OF D = I.G-MATRIX (EYE IS INPUT FROM THE ECPT)
C
      EYE = ECPT(IOPT+58)
      DO 50 I = 1,9
   50 D(I) = G(I)*EYE
C
C     (H  ) IS PARTITIONED INTO A LEFT AND RIGHT PORTION AND ONLY THE
C       YQ  RIGHT PORTION IS COMPUTED AND USED AS A (2X3). THE LEFT
C           2X3 PORTION IS NULL.  THE RIGHT PORTION WILL BE STORED AT
C           A(73) THRU A(78) UNTIL NOT NEEDED ANY FURTHER.
C
C
C
      TEMP  = 2.0D0*D(2) + 4.0D0*D(9)
      A(73) =-6.0D0*(J2X2(1)*D(1) + J2X2(2)*D(3))
      A(74) =-J2X2(1)*TEMP - 6.0D0*J2X2(2)*D(6)
      A(75) =-6.0D0*(J2X2(1)*D(6) + J2X2(2)*D(5))
      A(76) =-6.0D0*(J2X2(2)*D(1) + J2X2(4)*D(3))
      A(77) =-J2X2(2)*TEMP - 6.0D0*J2X2(4)*D(6)
      A(78) =-6.0D0*(J2X2(2)*D(6) + J2X2(4)*D(5))
C
C     THE ABOVE 6 ELEMENTS NOW REPRESENT THE (H  ) MATRIX (2X3)
C                                              YQ
C
      XBAR = (XSUBB + XSUBC)/3.0D0
      YBAR = YSUBC/3.0D0
C
      XCSQ  = XSUBC**2
      YCSQ  = YSUBC**2
      XBSQ  = XSUBB**2
      XCYC  = XSUBC*YSUBC
      PX2   = (XBSQ + XSUBB*XSUBC + XCSQ)/6.0D0
      PY2   = YCSQ/6.0D0
      PXY2  = YSUBC*(XSUBB + 2.0D0*XSUBC)/12.0D0
      XBAR3 = 3.0D0*XBAR
      YBAR3 = 3.0D0*YBAR
      YBAR2 = 2.0D0*YBAR
C
C     F1LL (HBAR) MATRIX STORING AT A(37) THRU A(72)
C
      DO 60 I = 37,72
   60 A(I)  = 0.0D0
C
      A(37) = XBSQ
      A(40) = XBSQ*XSUBB
      A(44) = XSUBB
      A(49) =-2.0D0*XSUBB
      A(52) =-3.0D0*XBSQ
      A(55) = XCSQ
      A(56) = XCYC
      A(57) = YCSQ
      A(58) = XCSQ*XSUBC
      A(59) = YCSQ*XSUBC
      A(60) = YCSQ*YSUBC
      A(62) = XSUBC
      A(63) = YSUBC*2.0D0
      A(65) = XCYC *2.0D0
      A(66) = YCSQ *3.0D0
      A(67) =-2.0D0*XSUBC
      A(68) =-YSUBC
      A(70) =-3.0D0*XCSQ
      A(71) =-YCSQ
C
C     ADD TO 6 OF THE (HBAR) ELEMENTS THE RESULT OF (H  )(H  )
C                                                     UY   YQ
C     THE PRODUCT IS FORMED DIRECTLY IN THE ADDITION PROCESS BELOW.
C     NO (H  ) MATRIX IS ACTUALLY COMPUTED DIRECTLY.
C          UY
C
C     THE FOLLOWING IS THEN PER STEPS 6 AND 7 PAGE -16- MS-17.
C
      DO 70 I = 1,3
      A(I+39) = A(I+39) + XSUBB*A(I+72)
   70 A(I+57) = A(I+57) + XSUBC*A(I+72) + YSUBC*A(I+75)
C
C     AT THIS POINT INVERT  (H) WHICH IS STORED AT A(37) THRU A(72)
C     STORE INVERSE BACK IN A(37) THRU A(72)
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERD (6,A(37),6,A(73),0,DETERM,ISING,A(79))
C
C     CHECK TO SEE IF H WAS SINGULAR
C
C     ISING = 2 IMPLIES SINGULAR MATRIX THUS ERROR CONDITION.
      IF(ISING .NE. 2) GO TO 90
      CALL MESAGE (-30,33,ECPT(1))
      RETURN
C
C     PARTITION H-INVERSE AND STORE IN C2 AND C3 LOCATIONS 7 THRU 24
C
   90 DO 100 I = 1,6
      IH = 6*I -6
      IC = 3*I -3
C
      DO 100 J = 1,3
      JH= IH + J + 36
      JC= IC + J + 6
      C(JC,2) = A(JH)
      C(JC,3) = A(JH+3)
  100 CONTINUE
      NOHYQ = 0
C
C     THIS ENDS ADDED COMPUTATION FOR CASE OF T2 NOT ZERO
C
C     THE C1, C2, AND C3 MATRICES ARE GENERATED WITH THE FOLLOWING CODE
C     FIRST GENERATE THE S MATRICES IN POSITIONS 1 THRU 9 AND 10 THRU 18
C
  110 DO 120 I = 1,18
  120 S(I) = 0.0
      DO 130 I = 1,9,4
      S(I  ) = 1.0
  130 S(I+9) = 1.0
      S(  3) =-XSUBB
      S( 11) = YSUBC
      S( 12) =-XSUBC
C
C     COMPUTE HA  AND STORE IN  CA, POSITIONS 7 THRU 24
C
C         HA =  -(HB TIMES SB + HC TIMES SC)
C
      CALL GMMATD (C(7,2),6,3,0, S(1),3,3,0, A(37))
      CALL GMMATD (C(7,3),6,3,0, S(10),3,3,0, A(55))
C
      DO 140 I = 1,18
C
  140 C(I+6,1) = -A(I+36) - A(I+54)
C
C     COMPUTE  HYQ TIMES HX  AND STORE IN CX POSITIONS 1 THRU 6
C     (THE FIRST THREE COLUMNS OF HYQ ARE NULL)
C
      IF (NOHYQ .EQ. 1) GO TO 160
C
      DO 150 I = 1,3
      CALL GMMATD (A(73),2,3,0, C(16,I),3,3,0, C(1,I))
  150 CONTINUE
C
  160 C(3,1) = C(3,1) - 1.0D0
      C(5,1) = C(5,1) + 1.0D0
C
C     THE INTEGRALS FOR THE  KDQQ MATRIX ARE GENERATED HERE
C
      YC2 = YSUBC**2
      XB2 = XSUBB**2
      XC2 = XSUBC**2
      XBC = XSUBB*XSUBC
C
      DI(1,1) = 1.0D0
      DI(1,2) = YSUBC/3.0D0
      DI(1,3) = YC2/6.0D0
      DI(1,4) = YC2*YSUBC/10.0D0
      DI(1,5) = YC2**2/15.0D0
      DI(2,1) = (XSUBB + XSUBC)/3.0D0
      DI(2,2) = YSUBC*(XSUBB + 2.0D0*XSUBC)/12.0D0
      DI(2,3) = DI(1,3)*(XSUBB + 3.0D0*XSUBC)/5.0D0
      DI(2,4) = DI(1,4)*(XSUBB + 4.0D0*XSUBC)/6.0D0
      DI(3,1) = (XB2 +XBC + XC2)/6.0D0
      DI(3,2) = DI(1,2)*(XB2 + 2.0D0*XBC + 3.0D0*XC2)/10.0D0
      DI(3,3) = DI(1,3)*(XB2 + 3.0D0*XBC + 6.0D0*XC2)/15.0D0
      DI(4,1) = (XSUBB + XSUBC)*(XB2 + XC2)/10.0D0
      DI(4,2) = DI(1,2)*((XSUBB + 2.0D0*XSUBC)*XB2 +
     1          (3.0D0*XSUBB + 4.0D0*XSUBC)*XC2)/20.0D0
      DI(5,1) = (XB2*XB2 + XB2*XBC + XBC*XBC + XBC*XC2 + XC2*XC2)/15.0
C
      AR = XSUBB*YSUBC*DBLE(ECPT(IOPT+56))/2.0D0
      DO 170 I = 1,5
      IC = 6 - I
      DO 170 J = 1,IC
      DI(I,J) = DI(I,J)*AR
  170 CONTINUE
C
C     THE ABOVE INTEGRALS  D(I,J) CORRESPOND TO THE DOCUMENTED
C     VALUES  I(I-1,J-1).  ZERO INDICES DONT ALWAYS COMPILE.
C
C     THE DIFFERENTIAL STIFFNESS MATRIX IN GENERALIZED COORDINATES IS
C     CREATED BELOW AT POSITIONS A(28) TO A(91)
C
      A(28) = SX*DI(1,1)
      A(29) = SXY*DI(1,1)
      A(30) = 2.0D0*SX*DI(2,1)
      A(31) = SX*DI(1,2) + SXY*DI(2,1)
      A(32) = 2.0D0*SXY*DI(1,2)
      A(33) = 3.0D0*SX *DI(3,1)
      A(34) = SX*DI(1,3) + 2.0*SXY*DI(2,2)
      A(35) = 3.0D0*SXY*DI(1,3)
C
      A(37) = SY*DI(1,1)
      A(38) = 2.0D0*SXY*DI(2,1)
      A(39) = SXY*DI(1,2) + SY*DI(2,1)
      A(40) = 2.0D0*SY*DI(1,2)
      A(41) = 3.0D0*SXY*DI(3,1)
      A(42) = SXY*DI(1,3) + 2.0D0*SY*DI(2,2)
      A(43) = 3.0D0*SY*DI(1,3)
C
      A(46) = 4.0D0*SX*DI(3,1)
      A(47) = 2.0D0*(SX*DI(2,2) + SXY*DI(3,1))
      A(48) = 4.0D0*SXY*DI(2,2)
      A(49) = 6.0D0*SX*DI(4,1)
      A(50) = 2.0D0*(SX*DI(2,3) + 2.0D0*SXY*DI(3,2))
      A(51) = 6.0D0*SXY*DI(2,3)
C
      A(55) = SX*DI(1,3) + 2.0D0*SXY*DI(2,2)+SY*DI(3,1)
      A(56) = 2.0D0*(SXY*DI(1,3) + SY*DI(2,2))
      A(57) = 3.0D0*(SX* DI(3,2) + SXY*DI(4,1))
      A(58) = SX*DI(1,4) + 3.0D0*SXY*DI(2,3) + 2.0D0*SY*DI(3,2)
      A(59) = 3.0D0*(SXY*DI(1,4) + SY*DI(2,3))
C
      A(64) = 4.0D0*SY*DI(1,3)
      A(65) = 6.0D0*SXY*DI(3,2)
      A(66) = 2.0D0*(SXY*DI(1,4) + 2.0D0*SY*DI(2,3))
      A(67) = 6.0D0*SY*DI(1,4)
C
      A(73) = 9.0D0*SX*DI(5,1)
      A(74) = 3.0D0*(SX*DI(3,3) + 2.0D0*SXY*DI(4,2))
      A(75) = 9.0D0*SXY*DI(3,3)
C
      A(82) = SX*DI(1,5) + 4.0D0*SXY*DI(2,4) + 4.0D0*SY*DI(3,3)
      A(83) = 3.0D0*SXY*DI(1,5) + 6.0D0*SY*DI(2,4)
C
      A(91) = 9.0D0*SY*DI(1,5)
C
C     FILL IN SYMMETRIC TERMS
C
      DO 180 I = 2,8
      IH = I - 1
      DO 180 J = 1,IH
      IC =  8*(I-1) + J
      JC =  8*(J-1) + I
      A(IC+27) = A(JC+27)
  180 CONTINUE
C
C     AT THIS STAGE THE 3X3 MATRIX PARTITIONS MAY BE GENERATED
C     THE ACTUAL MATRICES DEPEND ON IOPT
C
      IC = NPIVOT
      IF (IC .EQ. 0) GO TO 200
      CALL GMMATD (C(1,IC),8,3,1, A(28),8,8,0, A(92))
      DO 190 I = 1,3
      IH=  9*(I-1) + 1
      CALL GMMATD (A(92),3,8,0, C(1,I),8,3,0, A(IH))
  190 CONTINUE
C//////
C     CALL BUG (4HTBKD,300,A,54)
C//////
C
C     AT THIS STAGE THE QUADRILATERAL CALCULATIONS ARE COMPLETE
C
  200 IF (IOPT .EQ.2) RETURN
C
C     THE TRIANGLE SUBROUTINE  MUST RETURN THE FOLLOWING DATA
C         KAC,KBC,KCC  IN POSITIONS  A(28) THRU A(54) -I=NPIVOT
C             S        IN POSITIONS  A(55) THRU A(72)
C           H-INVERSE  IN POSITIONS  A(73) THRU A(108)
C
      CALL GMMATD (A(28),8,8,0, C(1,3),8,3,0, A(92))
      DO 210 I = 1,3
      IH = 28 + 9*(I-1)
      CALL GMMATD (C(1,I),8,3,1, A(92),8,3,0, A(IH))
  210 CONTINUE
C
C     RECALCULATE THE S MATRIX (IT WAS DESTROYED) -
C     PLACE IN A(55 THRU 72)
C
      DO 230 I = 1,18
  230 A(I+54) = 0.0
      DO 240 I = 1,9,4
      A(I+54) = 1.0
  240 A(I+63) = 1.0
      A(57) =-XSUBB
      A(65) = YSUBC
      A(66) =-XSUBC
C
C     EXTRACT THE H-INVERSE MATRIX FROM THE C MATRICES
C     STORE AT POSITIONS A(73) THRU A(108)
C
      DO 250 I = 1,6
      IH = 6*I - 6
      IC = 3*I - 3
C
      DO 250 J = 1,3
      JH = IH + J + 72
      JC = IC + J + 6
      A(JH  ) = C(JC,2)
      A(JH+3) = C(JC,3)
  250 CONTINUE
      RETURN
      END
