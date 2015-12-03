      SUBROUTINE KTRMEM (NTYPE)
C
C     TRIANGULAR MEMBRANE ELEMENT
C
C     IF NTYPE = 0 COMPLETE MEMBRANE COMPUTATION IS PERFORMED
C     IF NTYPE = 1 RETURN 3 TRANSFORMED  3X3 MATRICES ONLY FOR THE PIVOT
C
C     CALLS FROM THIS ROUTINE ARE MADE TO -
C
C     MAT    - MATERIAL DATA ROUTINE
C     SMA1B  - INSERTION ROUTINE
C     TRANSD - DOUBLE PRECISION TRANSFORMATION SUPPLIER
C     GMMATD - DOUBLE PRECISION MATRIX MULTIPLY AND TRANSPOSE
C     MESAGE - ERROR MESSAGE WRITER
C
      LOGICAL          HEAT
      INTEGER          NECPT(6)
      REAL             ECPT(21),MATBUF
      DOUBLE PRECISION TEMPAR,C,E,TI,TEMP,G(9),XSUBC,VOL,XSUBB,YSUBC,
     1                 REELMU,FLAMDA,DELTA,KIJ,TT(2)
      COMMON /CONDAS/  CONSTS(5)
      COMMON /SMA1IO/  DUM1(10),IFKGG,DUM2(1),IF4GG,DUM3(23)
      COMMON /SMA1CL/  IOPT4,K4GGSW,NPVT,DUMCL(7),LINK(10),IDETCK,
     1                 DODET,NOGO
      COMMON /SMA1HT/  HEAT
      COMMON /SMA1ET/  MECPT(1),NGRID(3),ANGLE,MATID1,T,FMU,DUMMY1,X1,
     1                 Y1,Z1,DUMMY2,X2,Y2,Z2,DUMMY3,X3,Y3,Z3,DUMB(80)
      COMMON /MATIN /  MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/  G11,G12,G13,G22,G23,G33,RHO,ALPHA1,ALPHA2,ALP12,
     1                 T SUB 0,G SUB E,SIGTEN,SIGCOM,SIGSHE,
     2                 G2X211,G2X212,G2X222
      COMMON /SMA1DP/  KIJ(36),C(18),E(9),TEMPAR(27),TI(9),TEMP,
     1                 XSUBB,XSUBC,YSUBC,VOL,REELMU,DELTA,FLAMDA,THETA,
     2                 KA,NPOINT,NSAVE,DUMMY(382)
      COMMON /HMTOUT/  MATBUF(4)
      EQUIVALENCE      (CONSTS(4),DEGRA),(G(1),TEMPAR(19)),
     1                 (ECPT(1),MECPT(1),NECPT(1))
C
C     ECPT LIST
C                                                      THIS
C     ECPT       DESCRIPTION                         ROUTINE    TYPE
C     ======================================         ========  =======
C     ECPT( 1) = ELEMENT ID                          NECPT(1)  INTEGER
C     ECPT( 2) = GRID POINT A                        NGRID(1)  INTEGER
C     ECPT( 3) = GRID POINT B                        NGRID(2)  INTEGER
C     ECPT( 4) = GRID POINT C                        NGRID(3)  INTEGER
C     ECPT( 5) = THETA = ANGLE OF MATERIAL           ANGLE     REAL
C     ECPT( 6) = MATERIAL ID                         MATID     INTEGER
C     ECPT( 7) = T                                   T         REAL
C     ECPT( 8) = NON-STRUCTURAL MASS                 FMU       REAL
C     ECPT( 9) = COORD. SYSTEM ID 1                  NECPT(9)  INTEGER
C     ECPT(10) = X1                                  X1        REAL
C     ECPT(11) = Y1                                  Y1        REAL
C     ECPT(12) = Z1                                  Z1        REAL
C     ECPT(13) = COORD. SYSTEM ID 2                  NECPT(13) INTEGER
C     ECPT(14) = X2                                  X2        REAL
C     ECPT(15) = Y2                                  Y2        REAL
C     ECPT(16) = Z2                                  Z2        REAL
C     ECPT(17) = COORD. SYSTEM ID 3                  NECPT(17) INTEGER
C     ECPT(18) = X3                                  X3        REAL
C     ECPT(19) = Y3                                  Y3        REAL
C     ECPT(20) = Z3                                  Z3        REAL
C     ECPT(21) = ELEMENT TEMPERATURE                 ELTEMP    REAL
C
C
C     SET UP THE E MATRIX WHICH IS (3X2) FOR THE TRI-MEMBRANE
C
C     E(1), E(3), E(5) WILL BE THE I-VECTOR
C     E(2), E(4), E(6) WILL BE THE J-VECTOR
C     E(7), E(8), E(9) WILL BE THE K-VECTOR NOT USED IN E FOR MEMBRANE
C
C     FIRST FIND I-VECTOR = RSUBB - RSUBA  (NON-NORMALIZED)
C
      E(1) = DBLE(X2) - DBLE(X1)
      E(3) = DBLE(Y2) - DBLE(Y1)
      E(5) = DBLE(Z2) - DBLE(Z1)
C
C     NOW FIND LENGTH = X-SUB-B   COORD. IN ELEMENT SYSTEM
C
      XSUBB = DSQRT(E(1)**2 + E(3)**2 + E(5)**2)
      IF (XSUBB .GT. 1.0D-06) GO TO 20
      CALL MESAGE (30,31,ECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
C
C     NOW NORMALIZE I-VECTOR WITH X-SUB-B
C
   20 E(1) = E(1)/XSUBB
      E(3) = E(3)/XSUBB
      E(5) = E(5)/XSUBB
C
C     HERE WE NOW TAKE RSUBC - RSUBA AND STORE TEMPORARILY IN
C     E(2), E(4), E(6) WHICH IS WHERE THE J-VECTOR WILL FIT LATER
C
      E(2) = DBLE(X3) - DBLE(X1)
      E(4) = DBLE(Y3) - DBLE(Y1)
      E(6) = DBLE(Z3) - DBLE(Z1)
C
C     X-SUB-C  =  I . (RSUBC - RSUBA),  THUS
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
      YSUBC = DSQRT(E(7)**2 + E(8)**2 + E(9)**2)
      IF (YSUBC .GT. 1.0D-06) GO TO 25
      CALL MESAGE (30,32,ECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
C
C     NOW NORMALIZE K-VECTOR WITH YSUBC JUST FOUND
C
   25 E(7) = E(7)/YSUBC
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
      TEMP = DSQRT(E(2)**2 + E(4)**2 + E(6)**2)
      IF (TEMP .NE. 0.0D0) GO TO 26
      CALL MESAGE (30,26,ECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
C
   26 CONTINUE
      E(2) = E(2)/TEMP
      E(4) = E(4)/TEMP
      E(6) = E(6)/TEMP
C
C     VOLUME OF ELEMENT, THETA, MU, LAMDA, AND DELTA
C
      VOL = XSUBB*YSUBC*DBLE(T)/2.0D0
      REELMU = 1.0D0/XSUBB
      FLAMDA = 1.0D0/YSUBC
      DELTA  = XSUBC/XSUBB - 1.0D0
C
C     NOW FORM THE  C MATRIX   (3X6) PARTITIONED AS FOLLOWS HERE.
C         CSUBA = (3X2) STORED IN C( 1) THRU C( 6) BY ROWS
C         CSUBB = (3X2) STORED IN C( 7) THRU C(12) BY ROWS
C         CSUBC = (3X2) STORED IN C(13) THRU C(18) BY ROWS
C
      C(1)  = -REELMU
      C(2)  =  0.0D0
      C(3)  =  0.0D0
      C(4)  =  FLAMDA*DELTA
      C(5)  =  C(4)
      C(6)  = -REELMU
      C(7)  =  REELMU
      C(8)  =  0.0D0
      C(9)  =  0.0D0
      C(10) = -FLAMDA*REELMU*XSUBC
      C(11) =  C(10)
      C(12) =  REELMU
      C(13) =  0.0D0
      C(14) =  0.0D0
      C(15) =  0.0D0
      C(16) =  FLAMDA
      C(17) =  FLAMDA
      C(18) =  0.0D0
C
      IF (NTYPE .EQ. 1) GO TO 30
C
      THETA = ANGLE*DEGRA
      SINTH = SIN(THETA)
      COSTH = COS(THETA)
   30 IF (ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0E0
C
C     BRANCH ON -HEAT- PROBLEM AT THIS POINT.
C
      IF (HEAT) GO TO 1010
      ELTEMP = ECPT(21)
      MATID  = MATID1
      INFLAG = 2
      CALL MAT (ECPT(1))
      IF (NOGO .EQ. 1) RETURN
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
C     G, E, AND C MATRICES ARE COMPLETE
C
C     AT THIS POINT THE FOLLOWING EQUATION CAN BE SOLVED FOR K-SUB-IJ
C
C                    T         T     T
C       K   = VOL . T *E*C *G*C *E *T
C        IJ          I         I  J  J
C
C     T-SUB-I WILL BE USED IN THE ABOVE ONLY IF THE PIVOT COORDINATE
C     SYSTEM ID IS NOT ZERO, OTHERWISE IT IS ASSUMED TO BE THE
C     IDENTITY MATRIX.
C
C     THE I SUBSCRIPT IMPLIES THE PIVOT POINT  1,2, OR 3 (ELEMENT SYST)
C     THE J SUBSCRIPT IMPLIES  1 THRU 3  FOR EACH CALL TO THIS ROUTINE.
C
C
C     FIRST LOCATE WHICH POINT IS THE PIVOT
C
C
      DO 100 I = 1,3
      IF (NGRID(I) .NE. NPVT) GO TO 100
      KA = 4*I + 5
      NPOINT = 6*I - 5
      GO TO 150
  100 CONTINUE
C
C     FALLING THRU ABOVE LOOP INDICATES THE PIVOT POINT SPECIFIED BY
C     NPVT WAS NOT FOUND EQUAL TO ANY OF THE 3 GRID POINTS IN THE ECPT
C     THUS ERROR CONDITION.
C
      CALL MESAGE (-30,34,ECPT(1))
C
C                     T
C     COMPUTE   E*C *G       AND STORE IN TEMPAR (1 THRU 9)
C                  I
C
  150 CALL GMMATD (E,3,2,0, C(NPOINT),3,2,1, TEMPAR(10))
      CALL GMMATD (TEMPAR(10),3,3,0, G,3,3,0, TEMPAR(1))
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
      IF (NECPT(KA) .EQ. 0) GO TO 80
C
C     NOT-ZERO THUS GET TI
C
      CALL TRANSD (NECPT(KA),TI)
C
C     INTRODUCE TI INTO THE COMMON PRODUCT AND STORE AT
C     TEMPAR (10 THRU 18)
C
      CALL GMMATD (TI,3,3,1, TEMPAR(1),3,3,0, TEMPAR(10))
C
C     COMMON PRODUCT NOW STARTS AT TEMPAR(10) THUS CHANGE NCOM AND NPT1
C
      NCOM = 10
      NPT1 =  1
C
C     NOW HAVE COMMON PRODUCT STORED BEGINNING TEMPAR(NCOM),  (3X3).
C     NPT1 POINTS TO FREE WORKING SPACE LENGTH 9.
C
C     PROCEED NOW AND RUN OUT THE 3 6X6 MATRICES KIJ-SUB-1,2,3.
C
C     FIRST ZERO OUT (6 X 6) K
C                             IJ
C
   80 NSAVE = NPT1
      DO 700 I = 1,36
  700 KIJ(I) = 0.0D0
      NPOINT = 0
C
      DO 500 I = 1,3
      CALL GMMATD (C(6*I-5),3,2,0, E,3,2,1, TEMPAR(NSAVE))
C
C                                                                  T
C     NPT2 IS SET TO POINT TO THE BEGINNING OF THE PRODUCT  C *E *T
C                                                            J     J
      NPT2 = NSAVE
      NPT1 = 19
C
C     CHECK FOR ZERO CSID IN WHICH CASE TJ IS NOT NEEDED
C
      IF (NECPT(4*I+5) .EQ. 0) GO TO 60
C
C     COMMING HERE IMPLIES NEED FOR TJ
C     WILL STORE TJ IN TI
C
      CALL TRANSD (NECPT(4*I+5),TI)
      CALL GMMATD (TEMPAR(NPT2),3,3,0, TI,3,3,0, TEMPAR(19))
      NPT1 = NPT2
      NPT2 = 19
C
C     AT THIS POINT COMPLETE COMPUTATION FOR  K-SUB-I,J
C
   60 CALL GMMATD (TEMPAR(NCOM),3,3,0, TEMPAR(NPT2),3,3,0, TEMPAR(NPT1))
C
      IF (NTYPE .EQ. 0) GO TO 95
      DO 96 J = 1,9
      NPOINT = NPOINT + 1
      NPT2 = NPT1 + J - 1
   96 KIJ(NPOINT) = TEMPAR(NPT2)
      GO TO 500
C
   95 KIJ( 1) = TEMPAR(NPT1    )
      KIJ( 2) = TEMPAR(NPT1 + 1)
      KIJ( 3) = TEMPAR(NPT1 + 2)
      KIJ( 7) = TEMPAR(NPT1 + 3)
      KIJ( 8) = TEMPAR(NPT1 + 4)
      KIJ( 9) = TEMPAR(NPT1 + 5)
      KIJ(13) = TEMPAR(NPT1 + 6)
      KIJ(14) = TEMPAR(NPT1 + 7)
      KIJ(15) = TEMPAR(NPT1 + 8)
C
      CALL SMA1B (KIJ(1),NECPT(I+1),-1,IFKGG,0.0D0)
      TEMP = G SUB E
      IF (IOPT4) 501,500,501
  501 IF (GSUBE) 502,500,502
  502 CALL SMA1B (KIJ(1),NECPT(I+1),-1,IF4GG,TEMP)
      K4GGSW = 1
C
  500 CONTINUE
      RETURN
C
C     HEAT PROBLEM LOGIC PICKS UP HERE.  CALL HMAT FOR MATERIAL DATA.
C
 1010 INFLAG = 2
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
      DO 1030 I = 1,3
      IF (NGRID(I) .EQ. NPVT) GO TO 1060
 1030 CONTINUE
      CALL MESAGE (-30,34,ECPT(1))
C
C     PIVOT C MATRIX TIMES VOLUME (STORED INTO TT(1) AND TT(2).)
C
 1060 TT(1) = VOL*C(2*I-1)
      TT(2) = VOL*C(2*I  )
C
C     OUTPUT THE CONDUCTIVITY MATRICES
C
      K = 36
      IF (NTYPE .NE. 0) K = 27
      DO 1070 I = 1,K
 1070 KIJ(I) = 0.0D0
      NPOINT = 0
C
      DO 1100 I = 1,3
      N2 = 2*I
      N1 = N2 - 1
      TEMPAR(1) = (G(1)*C(N1) + G(2)*C(N2))*TT(1)  +
     1            (G(3)*C(N1) + G(4)*C(N2))*TT(2)
      IF (NTYPE) 1080,1090,1080
C
C     SUB-TRIANGLE (RETURN 3X3-S AS ABOVE IN STIFFNESS PORTION)
C
 1080 KIJ(NPOINT+1) = TEMPAR(1)
      NPOINT = NPOINT + 9
      GO TO 1100
C
C     TRIANGLE BY ITSELF
C
 1090 CALL SMA1B (TEMPAR(1),NECPT(I+1),NPVT,IFKGG,0.0D0)
 1100 CONTINUE
      RETURN
      END
