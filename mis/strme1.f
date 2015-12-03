      SUBROUTINE STRME1 ( NTYPE )
C
C     ******** PHASE I OF STRESS DATA RECOVERY *************************
C     ******** TRIANGULAR MEMBRANE ELEMENT *****************************
C
C     CALLS FROM THIS ROUTINE ARE MADE TO. . .
C
C     MAT    - MATERIAL DATA ROUTINE
C     TRANSS - SINGLE PRECISION TRANSFORMATION SUPPLIER
C     GMMATS - SINGLE PRECISION MATRIX MULTIPLY AND TRANSPOSE
C     MESAGE - ERROR MESSAGE WRITER
C
C     IF NTYPE = 0  COMPLETE MEMBRANE COMPUTATION IS PERFORMED
C
C     IF NTYPE = 1 RETURN 3 TRANSFORMED 3X3 MATRICES ONLY
C
C
C
      DIMENSION G(9), ECPT(21)
C
      LOGICAL STRAIN
C
      COMMON /BLANK / IDUMMY(10), STRAIN
      COMMON /CONDAS/ CONSTS(5)
      COMMON /SDR2X5/
     1                   NECPT(1)           ,NGRID(3)
     2                  ,ANGLE              ,MATID1
     3                  ,T                  ,FMU
     4                  ,DUMMY1             ,X1
     5                  ,Y1                 ,Z1
     6                  ,DUMMY2             ,X2
     7                  ,Y2                 ,Z2
     8                  ,DUMMY3             ,X3
     9                  ,Y3                 ,Z3            ,DUMB(80)
     T                  ,PH1OUT(100)        ,FORVEC(25)
      COMMON /SDR2X6/ C(18),E(18),TI(9),TEMPAR(27),TEMP
     2      ,XSUBB,XSUBC,YSUBC,VOL,REELMU,DELTA,FLAMDA,THETA ,DUMMY(219)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ G11,G12,G13,G22,G23,G33,RHO,ALPHAS(3),
     1                T SUB 0, G SUB E, SIGTEN, SIGCOM, SIGSHE,
     2                G2X211, G2X212, G2X222
C
      EQUIVALENCE ( CONSTS(4) , DEGRA  )
      EQUIVALENCE (G(1),TEMPAR(19)) ,(ECPT(1),NECPT(1))
C
C     ECPT LIST
C                                                      IN
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
C     ******************************************************************
      ELTEMP = ECPT(21)
C
C     SET UP THE E MATRIX WHICH IS (3X2) FOR THE TRI-MEMBRANE
C
C     E(1), E(3), E(5) WILL BE THE I-VECTOR
C     E(2), E(4), E(6) WILL BE THE J-VECTOR
C     E(7), E(8), E(9) WILL BE THE K-VECTOR NOT USED IN E FOR MEMBRANE
C
C     FIRST FIND I-VECTOR = RSUBB - RSUBA  (NON-NORMALIZED)
      E(1) = X2 - X1
      E(3) = Y2 - Y1
      E(5) = Z2 - Z1
C
C     NOW FIND LENGTH = X-SUB-B   COORD. IN ELEMENT SYSTEM
      XSUBB =  SQRT( E(1)**2 + E(3)**2 + E(5)**2 )
      IF(XSUBB .GT. 1.0E-06) GO TO 20
      CALL MESAGE(-30,31,ECPT(1))
C
C  20 NOW NORMALIZE I-VECTOR WITH X-SUB-B
   20 E(1) = E(1) / XSUBB
      E(3) = E(3) / XSUBB
      E(5) = E(5) / XSUBB
C
C     HERE WE NOW TAKE RSUBC - RSUBA AND STORE TEMPORARILY IN
C     E(2), E(4), E(6) WHICH IS WHERE THE J-VECTOR WILL FIT LATER
C
      E(2) = X3 - X1
      E(4) = Y3 - Y1
      E(6) = Z3 - Z1
C
C     X-SUB-C  =  I . (RSUBC - RSUBA) ,  THUS
      XSUBC = E(1) * E(2) + E(3) * E(4) + E(5) * E(6)
C
C     AND CROSSING THE I-VECTOR TO (RSUBC-RSUBA) GIVES THE K-VECTOR
C     (NON-NORMALIZED)
C
      E(7) = E(3) * E(6)  -  E(5) * E(4)
      E(8) = E(5) * E(2)  -  E(1) * E(6)
      E(9) = E(1) * E(4)  -  E(3) * E(2)
C
C
C     THE LENGTH OF THE K-VECTOR IS NOW FOUND AND EQUALS Y-SUB-C
C     COORD. IN ELEMENT SYSTEM
      YSUBC =  SQRT( E(7)**2 + E(8)**2 + E(9)**2 )
      IF(YSUBC .GT. 1.0E-06) GO TO 25
      CALL MESAGE(-30,32,ECPT(1))
C
C  25 NOW NORMALIZE K-VECTOR WITH YSUBC JUST FOUND
C
   25 E(7) = E(7) / YSUBC
      E(8) = E(8) / YSUBC
      E(9) = E(9) / YSUBC
C
C     NOW HAVING I AND K VECTORS.GET J = I CROSS K AND
C     STORE IN THE SPOT FOR J
C
      E(2) = E(5) * E(8) - E(3) * E(9)
      E(4) = E(1) * E(9) - E(5) * E(7)
      E(6) = E(3) * E(7) - E(1) * E(8)
C
C     AND JUST FOR COMPUTER EXACTNESS NORMALIZE J-VECTOR TO MAKE SURE.
      TEMP =  SQRT( E(2)**2 + E(4)**2 + E(6)**2 )
      E(2) = E(2)/TEMP
      E(4) = E(4)/TEMP
      E(6) = E(6)/TEMP
C
C     VOLUME OF ELEMENT, THETA, MU, LAMDA, AND DELTA
C
      REELMU = 1.0D0 / XSUBB
      FLAMDA = 1.0D0 / YSUBC
      DELTA  = XSUBC / XSUBB - 1.0E0
C
C     ******************************************************************
C
C     NOW FORM THE  C MATRIX   (3X6) PARTITIONED AS FOLLOWS HERE.
C                 CSUBA = (3X2) STORED IN C(1) . . .C(6)  BY ROWS
C                 CSUBB = (3X2) STORED IN C(7) . . .C(12) BY ROWS
C                 CSUBC = (3X2) STORED IN C(13). . .C(18) BY ROWS
C
      C(1)  = -REELMU
      C(2)  =  0.0E0
      C(3)  =  0.0E0
      C(4)  =  FLAMDA * DELTA
      C(5)  =  C(4)
      C(6)  = -REELMU
      C(7)  =  REELMU
      C(8)  =  0.0E0
      C(9)  =  0.0E0
      C(10) = -FLAMDA * REELMU * XSUBC
      C(11) =  C(10)
      C(12) =  REELMU
      C(13) =  0.0E0
      C(14) =  0.0E0
      C(15) =  0.0E0
      C(16) =  FLAMDA
      C(17) =  FLAMDA
      C(18) =  0.0E0
C
      IF( NTYPE .EQ. 1 ) GO TO 30
      THETA = ANGLE * DEGRA
      SINTH = SIN( THETA )
      COSTH = COS( THETA )
   30 IF(ABS(SINTH) .LT. 1.0E-06) SINTH = 0.0E0
      ELTEMP = ECPT(21)
      MATID = MATID1
      INFLAG = 2
      CALL MAT( ECPT(1) )
C
C     FILL G-MATRIX WITH OUTPUT FROM MAT ROUTINE
C
      IF (STRAIN) GO TO 40
      G(1) = G11
      G(2) = G12
      G(3) = G13
      G(4) = G12
      G(5) = G22
      G(6) = G23
      G(7) = G13
      G(8) = G23
      G(9) = G33
      GO TO 50
   40 G(1) = 1.0
      G(2) = 0.0
      G(3) = 0.0
      G(4) = 0.0
      G(5) = 1.0
      G(6) = 0.0
      G(7) = 0.0
      G(8) = 0.0
      G(9) = 0.5
   50 CONTINUE
C
C     ******************************************************************
C
C     G, E, AND C MATRICES ARE COMPLETE
C
C
C
C                           T
C     COMPUTE  S  = G  C   E   T   , I = 1,2,3.
C               I       I       I
C
      DO 100 I = 1,3
C
C     POINTER TO C   = 6*I - 5
C                 I
C
      CALL GMMATS ( G,3,3,0,  C(6*I-5),3,2,0,  TEMPAR(1))
      CALL GMMATS ( TEMPAR(1),3,2,0,  E,3,2,1,  TEMPAR(10) )
C
C     DO WE NEED TRANSFORMATION TI
C
      IF( NECPT(4*I + 5) .EQ. 0 ) GO TO 60
      CALL TRANSS( NECPT(4*I + 5), TI )
      CALL GMMATS( TEMPAR(10),3,3,0,  TI,3,3,0,  PH1OUT(9*I+1) )
      GO TO 100
   60 NPT1 = 9 * I
      DO 80 J = 10,18
      NPT1 = NPT1 + 1
   80 PH1OUT(NPT1) = TEMPAR(J)
  100 CONTINUE
C
C     COMPUTE S    = G  ALPHAS
C               T
      CALL GMMATS( G,3,3,0,  ALPHAS,3,1,0,  PH1OUT(7) )
C
C     SAVE  T SUB 0  FOR PHASE II
C
      PH1OUT(6) = T SUB 0
      PH1OUT(1) = ECPT(1)
      PH1OUT(2) = ECPT(2)
      PH1OUT(3) = ECPT(3)
      PH1OUT(4) = ECPT(4)
C
C     THIS CONCLUDES PHASE 1 FOR TRIANGULAR MEMBRANE OR SUB CALCULATION
C     TO ANOTHER ROUTINE...
      RETURN
C
      END
