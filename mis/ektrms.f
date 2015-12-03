      SUBROUTINE EKTRMS (NTYPE)
C
C     TRIANGULAR MEMBRANE ELEMENT
C
C     ECPT LIST
C                                                      THIS
C       ECPT       DESCRIPTION                         ROUTINE   TYPE
C     ******************************************************************
C       ECPT( 1) = ELEMENT ID                          NECPT(1)  INTEGER
C       ECPT( 2) = GRID POINT A                        NGRID(1)  INTEGER
C       ECPT( 3) = GRID POINT B                        NGRID(2)  INTEGER
C       ECPT( 4) = GRID POINT C                        NGRID(3)  INTEGER
C       ECPT( 5) = THETA = ANGLE OF MATERIAL           ANGLE     REAL
C       ECPT( 6) = MATERIAL ID                         MATID     INTEGER
C       ECPT( 7) = T                                   T         REAL
C       ECPT( 8) = NON-STRUCTURAL MASS                 FMU       REAL
C       ECPT( 9) = COORD. SYSTEM ID 1                  NECPT(9)  INTEGER
C       ECPT(10) = X1                                  X1        REAL
C       ECPT(11) = Y1                                  Y1        REAL
C       ECPT(12) = Z1                                  Z1        REAL
C       ECPT(13) = COORD. SYSTEM ID 2                  NECPT(13) INTEGER
C       ECPT(14) = X2                                  X2        REAL
C       ECPT(15) = Y2                                  Y2        REAL
C       ECPT(16) = Z2                                  Z2        REAL
C       ECPT(17) = COORD. SYSTEM ID 3                  NECPT(17) INTEGER
C       ECPT(18) = X3                                  X3        REAL
C       ECPT(19) = Y3                                  Y3        REAL
C       ECPT(20) = Z3                                  Z3        REAL
C       ECPT(21) = ELEMENT TEMPERATURE                 ELTEMP    REAL
C
C     IF NTYPE = 0  COMPLETE MEMBRANE COMPUTATION IS PERFORMED
C     IF NTYPE = 1  9  3X3 MATRICES FOR THE GRID POINTS  IN ECPT
C
      LOGICAL         NOGO,HEAT
      INTEGER         NECPT(6)
      REAL            K,KOUT,ECPT(21),MATBUF,KIJ,G(9),C(18),TT(2),TI(9),
     1                TEMPAR(27)
      COMMON /EMGTRX/ A(225),PROD9(9),TEMP9(9),XSUBB,XSUBC,YSUBC,DICT5,
     1                E(18),K(324),KOUT(324),KIJ(81)
      COMMON /SYSTEM/ KSYSTM(60)
      COMMON /CONDAS/ CONSTS(5)
      COMMON /EMGPRM/ DUM(19),NOGO,HEAT
      COMMON /EMGEST/ MECPT(1),NGRID(3),ANGLE,MATID1,T,FMU,DUMMY1,X1,Y1,
     1                Z1,DUMMY2,X2,Y2,Z2,DUMMY3,X3,Y3,Z3,DUMB(80)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ G11,G12,G13,G22,G23,G33,RHO,ALPHA1,ALPHA2,ALP12,
     1                T SUB 0,G SUB E,SIGTEN,SIGCOM,SIGSHE,
     2                G2X211,G2X212,G2X222
      COMMON /HMTOUT/ MATBUF(4)
      EQUIVALENCE     (CONSTS(4),DEGRA),(ECPT(1),NECPT(1),MECPT(1)),
     1                (KSYSTM(2),IOUTPT),(KSYSTM(56),IHEAT)
C
C     SET UP THE E MATRIX WHICH IS (3X2) FOR THE TRI-MEMBRANE
C
C     E(1), E(3), E(5) WILL BE THE I-VECTOR
C     E(2), E(4), E(6) WILL BE THE J-VECTOR
C     E(7), E(8), E(9) WILL BE THE K-VECTOR NOT USED IN E FOR MEMBRANE
C
C     FIRST FIND I-VECTOR = RSUBB - RSUBA  (NON-NORMALIZED)
C
      E(1) = X2 - X1
      E(3) = Y2 - Y1
      E(5) = Z2 - Z1
C
C     NOW FIND LENGTH = X-SUB-B   COORD. IN ELEMENT SYSTEM
C
      XSUBB = SQRT(E(1)**2 + E(3)**2 + E(5)**2)
      IF (XSUBB .LE. 1.E-06)  GO TO 7770
C
C  20 NOW NORMALIZE I-VECTOR WITH X-SUB-B
C
      E(1) = E(1)/XSUBB
      E(3) = E(3)/XSUBB
      E(5) = E(5)/XSUBB
C
C     HERE WE NOW TAKE RSUBC - RSUBA AND STORE TEMPORARILY IN
C     E(2), E(4), E(6) WHICH IS WHERE THE J-VECTOR WILL FIT LATER
C
      E(2) = X3 - X1
      E(4) = Y3 - Y1
      E(6) = Z3 - Z1
C
C     X-SUB-C  =  I . (RSUBC - RSUBA), THUS
C
      XSUBC = E(1)*E(2) + E(3)*E(4) + E(5)*E(6)
C
C     AND CROSSING THE I-VECTOR TO (RSUBC-RSUBA) GIVES THE K-VECTOR
C     (NON-NORMALIZED)
C
      E(7) = E(3)*E(6) - E(5)*E(4)
      E(8) = E(5)*E(2) - E(1)*E(6)
      E(9) = E(1)*E(4) - E(3)*E(2)
C
C     THE LENGTH OF THE K-VECTOR IS NOW FOUND AND EQUALS Y-SUB-C
C     COORD. IN ELEMENT SYSTEM
C
      YSUBC = SQRT(E(7)**2 + E(8)**2 + E(9)**2)
      IF (YSUBC .LE. 1.E-06) GO TO 7780
C
C  25 NOW NORMALIZE K-VECTOR WITH YSUBC JUST FOUND
C
      E(7) = E(7)/YSUBC
      E(8) = E(8)/YSUBC
      E(9) = E(9)/YSUBC
C
C     J VECTOR = K CROSS I
C     STORE IN THE SPOT FOR J
C
      E(2) = E(5)*E(8) - E(3)*E(9)
      E(4) = E(1)*E(9) - E(5)*E(7)
      E(6) = E(3)*E(7) - E(1)*E(8)
C
C     AND JUST FOR COMPUTER EXACTNESS NORMALIZE J-VECTOR TO MAKE SURE.
C
      TEMP = SQRT(E(2)**2 + E(4)**2 + E(6)**2)
      IF (TEMP .EQ. 0.0) GO TO 7790
      E(2) = E(2)/TEMP
      E(4) = E(4)/TEMP
      E(6) = E(6)/TEMP
C
C     VOLUME OF ELEMENT, THETA, MU, LAMDA, AND DELTA
C
      VOL    = XSUBB*YSUBC*T/2.
      REELMU = 1./XSUBB
      FLAMDA = 1./YSUBC
      DELTA  = XSUBC/XSUBB - 1.
C
C     NOW FORM THE  C MATRIX   (3X6) PARTITIONED AS FOLLOWS HERE.
C         CSUBA = (3X2) STORED IN C( 1) THRU C( 6) BY ROWS
C         CSUBB = (3X2) STORED IN C( 7) THRU C(12) BY ROWS
C         CSUBC = (3X2) STORED IN C(13) THRU C(18) BY ROWS
C
      C(1)  =-REELMU
      C(2)  = 0.
      C(3)  = 0.
      C(4)  = FLAMDA*DELTA
      C(5)  = C(4)
      C(6)  =-REELMU
      C(7)  = REELMU
      C(8)  = 0.
      C(9)  = 0.
      C(10) =-FLAMDA*REELMU*XSUBC
      C(11) = C(10)
      C(12) = REELMU
      C(13) = 0.
      C(14) = 0.
      C(15) = 0.
      C(16) = FLAMDA
      C(17) = FLAMDA
      C(18) = 0.
C
      IF (NTYPE .EQ. 1) GO TO 30
C
      THETA = ANGLE*DEGRA
      SINTH = SIN(THETA)
      COSTH = COS(THETA)
   30 IF (ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0
C
C     BRANCH ON -HEAT- PROBLEM AT THIS POINT.
C
      IF (HEAT) GO TO 300
      ELTEMP = ECPT(21)
      MATID  = MATID1
      INFLAG = 2
      CALL MAT (ECPT(1))
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
C     AT 50 G, E, AND C MATRICES ARE COMPLETE
C
C     AT THIS POINT THE FOLLOWING EQUATION CAN BE SOLVED FOR K-SUB-IJ
C
C                     T        T             T
C       K   = VOL . T  * E * C  * G * C  * E  * T
C        IJ          I        I        J         J
C
C     T-SUB-I WILL BE USED IN THE ABOVE ONLY IF THE PIVOT COORDINATE
C     SYSTEM ID IS NOT ZERO, OTHERWISE IT IS ASSUMED TO BE THE
C     IDENTITY MATRIX.
C
C     THE I SUBSCRIPT IMPLIES THE PIVOT POINT  1,2, OR 3 (ELEMENT SYST)
C     THE J SUBSCRIPT IMPLIES  1 THRU 3  FOR EACH CALL TO THIS ROUTINE.
C
C     DO COMPUTATIONS FOR EACH POINT IN ECPT LIST
C
      DO 60 I = 1,81
   60 KIJ(I) = 0.
      DO 200 NPVT = 1,3
      KA = 4*NPVT + 5
      NPOINT = 6*NPVT - 5
C
C                     T
C     COMPUTE   E * C   * G       AND STORE IN TEMPAR(1 THRU 9)
C                    I
C
      CALL GMMATS (E,3,2,0, C(NPOINT),3,2,1, TEMPAR(10))
      CALL GMMATS (TEMPAR(10),3,3,0, G,3,3,0, TEMPAR(1))
C
C     NCOM WILL ALWAYS POINT TO THE COMMON 3 X 3 PRODUCT ABOVE
C     NPT1 WILL POINT TO FREE WORKING SPACE LENGTH 9
C
      NCOM = 1
      NPT1 = 10
C
C     MULTIPLY COMMON PRODUCT BY SCALER VOL
C
      DO 90 I = 1,9
   90 TEMPAR(I) = TEMPAR(I)*VOL
C
C     CHECK FOR PIVOT  CSID = 0,  IF ZERO SKIP TRANSFORMATION TSUBI.
C
      IF (NECPT(KA) .EQ. 0) GO TO 110
C
C     NOT-ZERO THUS GET TI
C
      CALL TRANSS (NECPT(KA),TI)
C
C     INTRODUCE TI INTO THE COMMON PRODUCT AND STORE AT
C     TEMPAR(10 THRU 18)
C
      CALL GMMATS (TI,3,3,1, TEMPAR(1),3,3,0, TEMPAR(10))
C
C     COMMON PRODUCT NOW STARTS AT TEMPAR(10) THUS CHANGE NCOM AND NPT1
C
      NCOM = 10
      NPT1 =  1
C
C  80 NOW HAVE COMMON PRODUCT STORED BEGINNING TEMPAR(NCOM),  (3X3).
C     NPT1 POINTS TO FREE WORKING SPACE LENGTH 9.
C
C     PROCEED NOW AND RUN OUT THE 3 6X6 MATRICES KIJ-SUB-1,2,3.
C
  110 NSAVE  = NPT1
      NPOINT = (NPVT-1)*27
C
C     INSERT G INTO TEMPAR
C
      DO 115 I = 1,9
115   TEMPAR(I+18) = G(I)
      DO 190 I = 1,3
      CALL GMMATS (C(6*I-5),3,2,0, E,3,2,1, TEMPAR(NSAVE))
C
C     NPT2 IS SET TO POINT TO THE BEGINNING OF THE PRODUCT  C * E * T
C                                                            J       J
      NPT2 = NSAVE
      NPT1 = 19
C
C     CHECK FOR ZERO CSID IN WHICH CASE TJ IS NOT NEEDED
C
      IF (NECPT(4*I +5) .EQ. 0) GO TO 120
C
C     COMMING HERE IMPLIES NEED FOR TJ
C     WILL STORE TJ IN TI
C
      CALL TRANSS (NECPT(4*I+5),TI)
      CALL GMMATS (TEMPAR(NPT2),3,3,0, TI,3,3,0, TEMPAR(19))
      NPT1 = NPT2
      NPT2 = 19
C
C  60 AT THIS POINT COMPLETE COMPUTATION FOR  K-SUB-I,J
C
  120 CALL GMMATS (TEMPAR(NCOM),3,3,0, TEMPAR(NPT2),3,3,0, TEMPAR(NPT1))
      NPT36 = NPT1 + 35
C
      DO 140 J = 1,9
      NPOINT = NPOINT + 1
      NPT2   = NPT1 + J - 1
  140 KIJ(NPOINT) = TEMPAR(NPT2)
  190 CONTINUE
  200 CONTINUE
C
      DICT5 = GSUBE
      RETURN
C
C     HEAT PROBLEM LOGIC PICKS UP HERE.  CALL HMAT FOR MATERIAL DATA.
C
  300 INFLAG = 2
      MATID  = NECPT(6)
      ELTEMP = ECPT(21)
      CALL HMAT (NECPT)
      G(1) = MATBUF(1)
      G(2) = MATBUF(2)
      G(3) = MATBUF(2)
      G(4) = MATBUF(3)
C
C     CONDENSE C MATRIX FOR HEAT PROBLEM (FORMED ABOVE)  C IS (2X3)
C
      C(2) = C(4)
      C(3) = C(7)
      C(4) = C(10)
      C(5) = C(13)
      C(6) = C(16)
C
C     DETERMINE THE PIVOT POINT.
C
      KQ   = 3
      KMAX = KQ*3
      DO 320 I = 1,KMAX
  320 KIJ(I) = 0.
      DO 400 NPVT = 1,3
C
C     PIVOT C MATRIX TIMES VOLUME (STORED INTO TT(1) AND TT(2).)
C
      TT(1) = VOL*C(2*NPVT-1)
      TT(2) = VOL*C(2*NPVT  )
C
C     OUTPUT THE CONDUCTIVITY MATRICES
C
      NPOINT = (NPVT-1)*KQ
C
      DO 380 I = 1,3
      N2 = 2*I
      N1 = N2 - 1
      TEMPAR(1) = (G(1)*C(N1) + G(2)*C(N2))*TT(1)  +
     1            (G(3)*C(N1) + G(4)*C(N2))*TT(2)
C
C     SUB-TRIANGLE (RETURN 3X3-S AS ABOVE IN STIFFNESS PORTION)
C
      KIJ(NPOINT+1) = TEMPAR(1)
      NPOINT = NPOINT + 1
  380 CONTINUE
  400 CONTINUE
      RETURN
C
C     ERROR  EXITS
C
 7770 CALL MESAGE (30,31,NECPT(1))
 7777 NOGO = .TRUE.
      RETURN
 7780 CALL MESAGE (30,32,NECPT(1))
      GO TO 7777
 7790 CALL MESAGE (30,26,NECPT(1))
      GO TO 7777
C
      END
