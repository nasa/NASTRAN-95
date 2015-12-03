      SUBROUTINE DCONE
C
C     DIFFERENTIAL STIFFNESS FOR THE CONICAL SHELL. FMMS-68
C
C     CALLS FROM DCONE ARE MADE TO
C           MESAGE
C           MAT
C           INVERD
C           GMMATD
C           DS1B
C
C     ECPT( 1) = ELEMENT ID                                INTEGER
C     ECPT( 2) = SIL PT A                                  INTEGER
C     ECPT( 3) = SIL PT B                                  INTEGER
C     ECPT( 4) = MATID 1                                   INTEGER
C     ECPT( 5) = TM  (MEMBRANE THICK)                      REAL
C     ECPT( 6) = MATID 2                                   INTEGER
C     ECPT( 7) = I   (MOM.OF INERTIA)                      REAL
C     ECPT( 8) = MATID 3                                   INTEGER
C     ECPT( 9) = TS  (SHEAR THICKNESS)                     REAL
C     ECPT(10) = NON-STRUCTURAL-MASS                       REAL
C     ECPT(11) = Z1                                        REAL
C     ECPT(12) = Z2                                        REAL
C     ECPT(13) = PHI  1                                    REAL
C     ECPT(14) = PHI  2                                    REAL
C     ECPT(15) = PHI  3                                    REAL
C     ECPT(16) = PHI  4                                    REAL
C     ECPT(17) = PHI  5                                    REAL
C     ECPT(18) = PHI  6                                    REAL
C     ECPT(19) = PHI  7                                    REAL
C     ECPT(20) = PHI  8                                    REAL
C     ECPT(21) = PHI  9                                    REAL
C     ECPT(22) = PHI 10                                    REAL
C     ECPT(23) = PHI 11                                    REAL
C     ECPT(24) = PHI 12                                    REAL
C     ECPT(25) = PHI 13                                    REAL
C     ECPT(26) = PHI 14                                    REAL
C     ECPT(27) = COORD. SYS. ID PT.1                       INTEGER
C     ECPT(28) = RADIUS PT. 1                              REAL
C     ECPT(29) = DISTANCE TO PT.1                          REAL
C     ECPT(30) = NULL                                      REAL
C     ECPT(31) = COORD. SYS. ID PT.2                       INTEGER
C     ECPT(32) = RADIUS PT 2                               REAL
C     ECPT(33) = DISTANCE TO PT. 2                         REAL
C     ECPT(34) = NULL                                      REAL
C     ECPT(35) = ELEMENT TEMPERATURE                       REAL
C     ECPT(36) = ELEMENT DEFORMATION                       REAL
C     ECPT(37) = ELEMENT LOADING TEMPERATURE - GRID PT A   REAL
C     ECPT(38) = ELEMENT LOADING TEMPERATURE - GRID PT B   REAL
C     ECPT(39) = DISPLACEMENT COMPONENTS AT GRID POINT A   REAL
C     ECPT(40) =                  ...                      REAL
C     ECPT(41) =                  ...                      REAL
C     ECPT(42) =                  ...                      REAL
C     ECPT(43) =                  ...                      REAL
C     ECPT(44) =                  ...                      REAL
C     ECPT(45) = DISPLACEMENT COMPONENTS AT GRID POINT B   REAL
C     ECPT(46) =                  ...                      REAL
C     ECPT(47) =                  ...                      REAL
C     ECPT(48) =                  ...                      REAL
C     ECPT(49) =                  ...                      REAL
C     ECPT(50) =                  ...                      REAL
C
      INTEGER          NECPT(100) ,NERROR(2) ,NA(10)
      DOUBLE PRECISION INT(10,4)  ,HUQ       ,U(10)     ,KQD       ,
     1                 A(5,3)     ,HYQ(10)   ,Q(8)      ,KIJ       ,
     2                 B(7,3)     ,EHT       ,FAC(10)   ,C(3,3)    ,
     3                 RA         ,E11       ,TEMP      ,ONE       ,
     4                 RB         ,E12       ,TEMP1     ,OPI       ,
     5                 ZA         ,E22       ,TEMP2     ,N2D33     ,
     6                 ZB         ,E33       ,TEMP3     ,SP2D22    ,
     7                 RASQ       ,D11       ,TEMP4     ,SP2D4     ,
     8                 RBSQ       ,D12       ,TEMP5     ,OQ        ,
     9                 PI         ,D22       ,TEMP6     ,TDIF      ,
     O                 PIOVB      ,D33       ,TEMP7     ,DEPS      ,
     1                 N          ,TS        ,NSPOPI    ,DEPP      ,
     2                 N2         ,TM        ,TWOD33    ,EPS       ,
     3                 SL         ,NSP       ,NOV4      ,EPP       ,
     4                 L2         ,NCP       ,NSPOV4    ,TE11      ,
     5                 SP         ,SP2       ,N2OV4     ,TE12      ,
     6                 CP         ,CP2       ,SD22PI    ,TE22      ,
     7                 SUM        ,SIGN      ,GSHEAR    ,TEMP48(48),
     8                 A0         ,A1        ,A2        ,A3
      DOUBLE PRECISION B0         ,B1        ,B2        ,B3        ,
     1                 C0         ,C1        ,D0        ,D1        ,
     2                 DETERM     ,CONSTD
      COMMON /DS1AAA/  NPVT       ,DUMCL(34) ,NOGO
      COMMON /DS1AET/  ECPT(100)
      COMMON /DS1ADP/  HUQ(100)   ,KQD(64)   ,KIJ(36)   ,EHT(96)   ,
     1                 E11        ,E12       ,E22       ,E33
      COMMON /MATIN /  MATID      ,INFLAG    ,ELTEMP    ,STRESS    ,
     1                 SINTH      ,COSTH
      COMMON /MATOUT/  G11        ,G12       ,G13       ,G22       ,
     1                 G23        ,G33       ,RHOY      ,ALPHA1    ,
     2                 ALPHA2     ,ALPH12    ,DUM(10)
      COMMON /CONDAD/  CONSTD(5)
      EQUIVALENCE      (G,G12)               ,(ECPT(1),NECPT(1))   ,
     1                 (ECPT(4),MATID1)      ,(ECPT(6), MATID2)    ,
     2                 (ECPT(8),MATID3)      ,(CONSTD(1),   PI)
      DATA    NA    /  6*1, 2*2, 2*4/
      DATA    FAC   /  1.0D0, 1.0D0, 2.0D0, 6.0D0, 24.0D0, 120.0D0,
     1                 720.0D0, 5040.0D0, 40320.0D0, 362880.0D0   /
      DATA    ONE   /  1.0D0  /
C
C
C     CALCULATE SHELL ORIENTATION CONSTANTS
C
      SINTH = 0.0
      COSTH = 1.0
      NINT  = NECPT(1)/1000
      N     = NECPT(1) - NINT*1000 - 1
      RA    = ECPT(28)
      ZA    = ECPT(29)
      RB    = ECPT(32)
      ZB    = ECPT(33)
      TEMP1 = RB - RA
      TEMP2 = ZB - ZA
      L2    = TEMP1**2 + TEMP2**2
      SL    = DSQRT(L2)
      IF (SL) 30,20,30
   20 NERROR(1) = NECPT(1)/1000
      NERROR(2) = N + .3D0
      CALL MESAGE (30,39,NERROR(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
C
   30 SP = TEMP1/SL
      CP = TEMP2/SL
C
C     COMPUTE INTEGRALS I     FOR M = 0,9
C                        MN       N = 0,3
C
C     FOR EVALUATION OF INTEGRALS  A = RA,  B = SP
C
      IF (SP) 60,40,60
C
C     COMPUTE INTEGRAL FOR B = 0
C
C                            1-N
C                      PI  RA     M+1
C               I   = --------- SL    (FOR ALL M,N .GE. 0)
C                M,N    M + 1
C
C
C     M = I - 1    WHERE I IS THE DO LOOP INDEX
C     N = J - 1    WHERE J IS THE DO LOOP INDEX
C     MPLUS1 THUS EQUALS I
C
   40 DO 50 I = 1,10
      NBEGIN  = NA(I)
      DO 50 J = NBEGIN,4
   50 INT(I,J) = (PI*SL**I)/(DBLE(FLOAT(I))*RA**(J-2))
C
      GO TO 100
C
C
C     COMPUTE INTEGRALS FOR (B .NE. 0)
C
C     FIRST M = 0 CASE
C
C                             2-N     2-N
C                      PI ( RB    - RA   )
C               I     =--------------------   (N NOT EQUAL TO 2)
C                0,N       (2-N)  B
C
C
C     FOR N=2   I     = PI * (LOG RB  -  LOG RA) / B
C                0,2             E          E
C
C
   60 RASQ  = RA*RA
      RBSQ  = RB*RB
      PIOVB = PI/SP
C
      INT(1,1) = 0.5D0*PIOVB*(RBSQ - RASQ)
      INT(1,2) = PIOVB*(RB - RA)
      INT(1,3) = PIOVB*DLOG(RB/RA)
      INT(1,4) =-PIOVB*(ONE/RB - ONE/RA)
C
C
C     M = I        WHERE I IS THE DO LOOP INDEX
C     N = J - 1    WHERE J IS THE DO LOOP INDEX
C
C     WE ARE GETTING INTEGRAL(M,N)
C     M = POWER OF S
C     N = POWER OF R
C
C
C     EVALUATING AT R = RB  THEN AT R = RA
C
C                                  K   NPOW
C                  M FAC.      M   (-A) (R)
C     I  = (PI)(-----------)( SUM ------------------------) + (TERM-X)
C      MN               (M+1)   K=0  (M-K)FAC.(K)FAC.(NPOW)
C                    B        (K.NE.M-N+2)                  (K.EQ.M-N+2)
C
C
C     WHERE NPOW = M - N - K + 2
C
C
C                    M-N+2
C                (-A)     LOG(R)
C       TERM-X = --------------------
C               (M-N+2)FAC.(N-2)FAC.
C
C
C     NOTE IN DATA STATEMENT THAT 0 FACTORIAL = FAC(1)
C                                 1 FACTORIAL = FAC(2)
C                                 2 FACTORIAL = FAC(3)    ETC.
C
      DO 90 I = 1,9
      MPLUS1  = I + 1
      NBEGIN  = NA(MPLUS1)
      DO 90 J = NBEGIN,4
      SUM  = 0.0D0
      SIGN =-1.0D0
      NPOW = I - J + 3
      DO 80 KK = 1,MPLUS1
      SIGN = -SIGN
      K    = KK - 1
      IF (K .EQ. NPOW) GO TO 70
      KPOW = NPOW   - K
      IFAC = MPLUS1 - K
      TEMP = KPOW
      SUM  = SUM + SIGN*RA**K*(RB**KPOW - RA**KPOW)/
     1       (FAC(IFAC)*FAC(KK)*TEMP)
      GO TO 80
   70 SUM = SUM + SIGN*RA**NPOW*DLOG(RB/RA)/(FAC(NPOW+1)*FAC(J-2))
   80 CONTINUE
C
      INT(MPLUS1,J) = SUM*PI*FAC(MPLUS1)/SP**MPLUS1
   90 CONTINUE
  100 CONTINUE
C
C     CRANK OUT HUQ MATRIX FOR ZERO HARMONIC
C     FOR EXPLICIT FORMULATION OF HUQ, SEE MS-28, PP.15,16 AND PP.24,25.
C
      DO 105 I = 1,100
  105 HUQ(  I) = 0.0D0
      HUQ(  1) = ONE
      HUQ( 13) = ONE
      HUQ( 25) = ONE
      HUQ( 36) = ONE
      HUQ( 41) = CP/RA
      HUQ( 49) = ONE
      HUQ( 51) = ONE
      HUQ( 52) = SL
      HUQ( 63) = ONE
      HUQ( 64) = SL
      HUQ( 75) = ONE
      HUQ( 76) = SL
      HUQ( 77) = L2
      HUQ( 78) = HUQ(77)*SL
      HUQ( 86) = ONE
      HUQ( 87) = 2.0D0*SL
      HUQ( 88) = 3.0D0*HUQ(77)
      HUQ( 91) = CP/RB
      HUQ( 92) = HUQ(91)*SL
      HUQ( 99) = ONE
      HUQ(100) = SL
C
C     IF TRANSVERSE SHEAR IS ZERO
C
C     OR INERTIA           = 0.0
C     OR SHEAR MODULUS(G)  = 0.0
C     OR MATID2            = 0
C     OR MATID3            = 0
C
C     THEN (HYQ)  = (0).  THEREFORE, USE HUQ MATRIX AS IS
C
      IF (MATID2.EQ.0 .OR. MATID3.EQ.0) GO TO 130
      IF (ECPT(9).EQ.0.0 .OR. ECPT(7).EQ.0.0) GO TO 130
      INFLAG = 1
      MATID  = MATID3
      ELTEMP = ECPT(35)
      CALL MAT (ECPT(1))
      GSHEAR = G
      IF (G .EQ. 0.0) GO TO 130
      INFLAG = 2
      MATID  = MATID2
      ELTEMP = ECPT(35)
      CALL MAT (ECPT(1))
C
C     FORM
C     (D) = I*(G)
C
      D11 = ECPT(7)*G11
      D12 = ECPT(7)*G12
      D22 = ECPT(7)*G22
      D33 = ECPT(7)*G33
C
      TS  = ECPT(9)
C
      DO 110 I = 1,10
  110 HYQ(I) = 0.0D0
      CP2 = CP*CP
      SP2 = SP*SP
      N2  = N*N
      OPI = ONE/PI
      SD22PI = SP2*D22*OPI
      OQ  = SL*TS*GSHEAR*(RA+RB)*0.5D0 + SD22PI*INT(1,3)
      OQ  = ONE/OQ
C
      HYQ(6) = OQ*INT(1,3)*SD22PI
      HYQ(7) = OQ*2.0D0*(D11 *(RA-RB) + INT(2,3)*SD22PI)
      HYQ(8) = OQ*(-D11*6.0D0*SL*RB   + 3.0D0*INT(3,3)*SD22PI)
C
      DO 120 I = 6,8
      HUQ(I+30) = HUQ(I+30) - HYQ(I)
  120 HUQ(I+80) = HUQ(I+80) - HYQ(I)
C
  130 CONTINUE
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERD (10,HUQ(1),10,DUM,0,DETERM,ISING,TEMP48(1))
C
C     CHECK SINGULARITY
C
      IF (ISING .NE. 2) GO TO 140
      CALL MESAGE (30,40,NECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
C
  140 CONTINUE
C
C     CALCULATE GENERALIZED DISPLACEMENT VECTOR(Q)
C
C                       ***     ***
C                       *  T      *
C                       *(E )(U ) *
C                       *      A  *
C        (Q)  =  (HUQ)  *---------*
C                       *  T      *
C                       *(E )(U ) *
C                       *      B  *
C                       ***     ***
C
C           WHERE
C                                0    1    0    0    0    0
C                          T     SP   0    CP   0    0    0
C                MATRIX  (E )  = CP   0   -SP   0    0    0
C                                0    0    0    0    1    0
C                                0    0    0    SP   0    CP
C
C
      K1 = 0
      K2 = 0
  320 U(K2+1) = DBLE(ECPT(K1+40))
      U(K2+2) = DBLE(ECPT(K1+39))*SP + DBLE(ECPT(K1+41))*CP
      U(K2+3) = DBLE(ECPT(K1+39))*CP - DBLE(ECPT(K1+41))*SP
      U(K2+4) = DBLE(ECPT(K1+43))
      U(K2+5) = DBLE(ECPT(K1+42))*SP + DBLE(ECPT(K1+44))*CP
C
      IF (K1 .NE. 0) GO TO 400
      K1 = 6
      K2 = 5
      GO TO 320
C
  400  CALL GMMATD (HUQ(1),8,10,0,U(1),10,1,0,Q(1))
C
C     CALCULATE STRAIN COEFFICIENTS AND OBTAIN MATERIAL PROPERTY MATRIX
C     (E)
C
      MATID  = MATID1
      INFLAG = 2
      ELTEMP = ECPT(35)
      CALL MAT (ECPT(1))
      E11   = G11
      E12   = G12
      E22   = G22
      E33   = G33
      TDIF  = (DBLE(ECPT(38)) - DBLE(ECPT(37)))/SL
      DEPS  = DBLE(ALPHA1)*TDIF
      DEPP  = DBLE(ALPHA2)*TDIF
      EPS   = DBLE(ALPHA1)*DBLE(ECPT(37))
      EPP   = DBLE(ALPHA2)*DBLE(ECPT(37))
C
C     COMPUTE COEFFICIENTS FOR POWER SERIES OF DIFFERENTIAL STIFF. COEFF
C
      TM    = ECPT(5)
      TEMP1 = SP*Q(3) + CP*Q(5)
      TEMP2 = SP*Q(4) + CP*Q(6)
      TEMP3 = Q(4) - EPS
      TE11  = TM*E11
      TE12  = TM*E12
      TE22  = TM*E22
C
      A0    = TE12*TEMP1
      A1    = TE12*TEMP2
      A2    = TE12*CP*Q(7)
      A3    = TE12*CP*Q(8)
      B0    = TE22*TEMP1
      B1    = TE22*TEMP2
      B2    = TE22*CP*Q(7)
      B3    = TE22*CP*Q(8)
      C0    = TE11*TEMP3 - TE12*EPP
      C1    =-TE11*DEPS  - TE12*DEPP
      D0    = TE12*TEMP3 - TE22*EPP
      D1    =-TE12*DEPS  - TE22*DEPP
C
C     COMPUTE DIFFERENTIAL STIFFNESS COEFFICIENTS
C
      DO 500 I = 1,3
      IP1 = I + 1
      IP2 = I + 2
      IP3 = I + 3
      DO 500 J = I,3
      JP1 = J + 1
      A(I,J) = A0*INT(I  ,JP1) + A1*INT(IP1,JP1) + A2*INT(IP2,JP1)
     1       + A3*INT(IP3,JP1) + C0*INT(I  ,J  ) + C1*INT(IP1,J  )
      B(I,J) = B0*INT(I  ,JP1) + B1*INT(IP1,JP1) + B2*INT(IP2,JP1)
     1       + B3*INT(IP3,JP1) + D0*INT(I  ,J  ) + D1*INT(IP1,J  )
  500 C(I,J) = A(I,J) + B(I,J)
C
      J   = 1
      JP1 = 2
      DO 510 I = 2,5
      IP1 = I + 1
      IP2 = I + 2
      IP3 = I + 3
  510 A(I,J) = A0*INT(I  ,JP1) + A1*INT(IP1,JP1) + A2*INT(IP2,JP1)
     1       + A3*INT(IP3,JP1) + C0*INT(I  ,J  ) + C1*INT(IP1,J  )
C
      J   = 3
      JP1 = 4
      DO 520 I = 4,7
      IP1 = I + 1
      IP2 = I + 2
      IP3 = I + 3
  520 B(I,J) = B0*INT(I  ,JP1) + B1*INT(IP1,JP1) + B2*INT(IP2,JP1)
     1       + B3*INT(IP3,JP1) + D0*INT(I  ,J  ) + D1*INT(IP1,J  )
C
C     COMPUTE KQD
C     FOR EXPLICIT FORMULATION OF KQD, SEE MS-31, PP. 8-11
C     CASE ONE.. HARMONIC NUMBER = ZERO
C
      DO 600 I = 1,64
  600 KQD(I)  = 0.0D0
      SP2D4   = SP2*0.25D0
      KQD( 1) = CP2*B(1,3) + SP2D4*C(1,3)
      KQD( 2) = CP2*B(2,3) + 0.25D0*SP*C(1,2) + SP2D4*C(2,3)
      KQD( 9) = KQD(2)
      KQD(10) = CP2*B(3,3) + (C(1,1) + 2.0D0*SP*C(2,2)
     $        + SP2*C(3,3))*0.25D0
      KQD(46) = A(1,1)
      KQD(47) = A(2,1)*2.0D0
      KQD(48) = A(3,1)*3.0D0
      KQD(54) = KQD(47)
      KQD(55) = A(3,1)*4.0D0
      KQD(56) = A(4,1)*6.0D0
      KQD(62) = KQD(48)
      KQD(63) = KQD(56)
      KQD(64) = A(5,1)*9.0D0
C
C     CHECK HARMONIC NUMBER
C
      IF (N .EQ. 0.0D0) GO TO 800
C
C     CASE TWO.. HARMONIC NUMBER .NE. ZERO
C
      NOV4    = N*0.25D0
      NSPOV4  = NOV4*SP
      NCP     = N*CP
      N2OV4   = NOV4*N
      KQD( 3) = NSPOV4*C(1,3)
      KQD( 4) = NSPOV4*C(2,3)
      KQD( 5) = NCP*B(1,3)
      KQD( 6) = NCP*B(2,3)
      KQD( 7) = NCP*B(3,3)
      KQD( 8) = NCP*B(4,3)
      KQD(11) = NOV4*(C(1,2) + SP*C(2,3))
      KQD(12) = NOV4*(C(2,2) + SP*C(3,3))
      KQD(13) = NCP*B(2,3)
      KQD(14) = NCP*B(3,3)
      KQD(15) = NCP*B(4,3)
      KQD(16) = NCP*B(5,3)
      KQD(17) = KQD( 3)
      KQD(18) = KQD(11)
      KQD(19) = N2OV4*C(1,3)
      KQD(20) = N2OV4*C(2,3)
      KQD(25) = KQD( 4)
      KQD(26) = KQD(12)
      KQD(27) = KQD(20)
      KQD(28) = N2OV4*C(3,3)
      KQD(33) = KQD( 5)
      KQD(34) = KQD(13)
      KQD(37) = N2*B(1,3)
      KQD(38) = N2*B(2,3)
      KQD(39) = N2*B(3,3)
      KQD(40) = N2*B(4,3)
      KQD(41) = KQD( 6)
      KQD(42) = KQD(14)
      KQD(45) = KQD(38)
      KQD(46) = KQD(46) + N2*B(3,3)
      KQD(47) = KQD(47) + N2*B(4,3)
      KQD(48) = KQD(48) + N2*B(5,3)
      KQD(49) = KQD( 7)
      KQD(50) = KQD(15)
      KQD(53) = KQD(39)
      KQD(54) = KQD(47)
      KQD(55) = KQD(55) + N2*B(5,3)
      KQD(56) = KQD(56) + N2*B(6,3)
      KQD(57) = KQD( 8)
      KQD(58) = KQD(16)
      KQD(61) = KQD(40)
      KQD(62) = KQD(48)
      KQD(63) = KQD(56)
      KQD(64) = KQD(64) + N2*B(7,3)
C
C     COMPUTE HUQ FOR NTH HARMONIC
C
      DO 690 I = 1,100
  690 HUQ(  I) = 0.0D0
      HUQ(  1) = ONE
      HUQ( 13) = ONE
      HUQ( 25) = ONE
      HUQ( 36) = ONE
      HUQ( 41) = CP/RA
      HUQ( 45) = N /RA
      HUQ( 49) = ONE
      HUQ( 51) = ONE
      HUQ( 52) = SL
      HUQ( 63) = ONE
      HUQ( 64) = SL
      HUQ( 75) = ONE
      HUQ( 76) = SL
      HUQ( 77) = L2
      HUQ( 78) = HUQ(77)*SL
      HUQ( 86) = ONE
      HUQ( 87) = 2.0D0*SL
      HUQ( 88) = 3.0D0*HUQ(77)
      HUQ( 91) = CP/RB
      HUQ( 92) = HUQ(91)*SL
      HUQ( 95) = N /RB
      HUQ( 96) = HUQ(95)*SL
      HUQ( 97) = HUQ(95)*L2
      HUQ( 98) = HUQ(96)*L2
      HUQ( 99) = ONE
      HUQ(100) = SL
C
C     COMPUTE HYQ
C
      IF (MATID2.EQ.0 .OR. MATID3.EQ.0) GO TO 710
      IF (ECPT(9).EQ.0.0 .OR. ECPT(7).EQ.0.0) GO TO 710
      IF (GSHEAR .EQ. 0.0D0) GO TO 710
C
      N2D33  = N2 *D33
      SP2D22 = SP2*D22
      OQ     = SL*TS*GSHEAR*(RA+RB)*0.5D0 + INT(1,3)*(N2D33+SP2D22)*OPI
      OQ     = ONE/OQ
      NSP    = N*SP
      NCP    = N*CP
      NSPOPI = NSP*OPI
      TWOD33 = 2.0D0*D33
      TEMP1  = D12*(ONE/RB - ONE/RA)
      TEMP2  = NSPOPI*(D22 + D33)
      TEMP3  = N*NSPOPI*(TWOD33 + D22)
      TEMP4  = OQ*0.5D0*N2D33*CP*OPI
      TEMP5  = OPI*(N2*TWOD33  + SP2D22)
      TEMP6  = D12*N2*L2/RB
      TEMP7  = NSPOPI*CP*0.50D0
C
      HYQ( 1) = OQ*(TEMP1*NCP - TEMP7*INT(1,4)*(D33 + 2.0D0*D22))
      HYQ( 2) = OQ*(NCP*SL/RB*D12 - TEMP7*INT(2,4)*(3.0D0*D33 + D22) +
     1          1.5D0*NCP*OPI*INT(1,3)*D33)
      HYQ( 3) = TEMP4*INT(1,4)
      HYQ( 4) = TEMP4*INT(2,4)
      HYQ( 5) = OQ*(TEMP1*N2 - TEMP3*INT(1,4))
      HYQ( 6) = OQ*(D12*N2*SL/RB - TEMP3*INT(2,4) + TEMP5*INT(1,3))
      HYQ( 7) = OQ*(2.0D0*D11*(RA-RB) + TEMP6 + 2.0D0*INT(2,3)*TEMP5
     $        - TEMP3*INT(3,4))
      HYQ( 8) = OQ*(-D11*6.0D0*SL*RB + TEMP6*SL + 3.0D0*INT(3,3)*TEMP5
     $        - TEMP3*INT(4,4))
      HYQ( 9) =-OQ*TEMP2*INT(1,3)
      HYQ(10) = OQ*(N*SL*(D12 + D33) - TEMP2*INT(2,3))
C
      DO 700 I = 1,10
      HUQ(I+30) = HUQ(I+30) - HYQ(I)
  700 HUQ(I+80) = HUQ(I+80) - HYQ(I)
C
  710 CONTINUE
C
C     AGAIN SET ISING TO -1
C
      ISING = -1
      CALL INVERD (10,HUQ(1),10,DUM,0,DETERM,ISING,TEMP48(1))
      IF (ISING .NE. 2) GO TO 720
      CALL MESAGE (30,40,NECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
C
  720 CONTINUE
C
C     COMPLETE SOLUTION BY TRANSFORMING KQD TO GLOBAL COORDINATES
C
C                     T            T
C       (K  ) = (E)(H  )(KQD)(H)(E )     FOR I = PIVOT POINT
C         IJ         I         J             J = A,B
C
C     FIRST OBTAIN PRODUCTS
C                       T
C        EHAT  =  (E)(H  )      AND STORE AT EHT(1) . . . EHT(48)
C                      A
C
C                       T
C        EHBT  =  (E)(H  )      AND STORE AT EHT(49). . . EHT(96)
C                      B
C
C              0    CP   SP   0    0
C              1    0    0    0    0
C
C              0    CP  -SP   0    0
C
C        MATRIX E =
C              0    0    0    0    SP
C
C              0    0    0    1    0
C
C              0    0    0    0    CP
C
  800 INC1 = 0
      INC2 = 0
  810 DO 820 I = 1,8
      KROW = I + INC1
      NCOL = (I-1)*10 + INC2
      EHT(KROW   ) = SP*HUQ(NCOL+2) + CP*HUQ(NCOL+3)
      EHT(KROW+ 8) =    HUQ(NCOL+1)
      EHT(KROW+16) = CP*HUQ(NCOL+2) - SP*HUQ(NCOL+3)
      EHT(KROW+24) = SP*HUQ(NCOL+5)
      EHT(KROW+32) =    HUQ(NCOL+4)
  820 EHT(KROW+40) = CP*HUQ(NCOL+5)
      IF(INC1 .GT. 0) GO TO 830
      INC1 = 48
      INC2 = 5
      GO TO 810
C
C     CHECK FOR PIVOT POINT NUMBER
C
  830 DO 840 I = 1,2
      IF (NPVT .EQ. NECPT(I+1)) GO TO 850
  840 CONTINUE
C
C     FALL THRU LOOP IMPLIES NO PIVOT POINT NUMBER
C
      CALL MESAGE (-30,34,ECPT(1))
C
  850 NPIVOT = I
      CALL GMMATD ( EHT(48*NPIVOT-47),6,8,0, KQD(1),8,8,0, TEMP48(1) )
C
C     IF N = 0 DOUBLE RESULT
C
      IF (N .NE. 0.0D0) GO TO 870
      DO 860 I = 1,48
  860 TEMP48(I) = 2.0D0*TEMP48(I)
C
  870 DO 880 J = 1,2
      CALL GMMATD (TEMP48(1),6,8,0,EHT(48*J-47),6,8,1,KIJ(1))
  880 CALL DS1B (KIJ(1),NECPT(J+1))
C
      RETURN
      END
