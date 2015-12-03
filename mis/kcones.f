      SUBROUTINE KCONES
C
C     SINGLE PRECISION CONEAX ROUTINE, MACHINE INDEPENDENT VERSION
C
C     FOUR KCONE VERSIONS
C     KCONES  FOR MACHINES WITH 60 OR 64 BIT WORD (e.g. CDC, CRAY).
C             S.P. COMPUTATION IS USED
C     KCONE2, SIMILAR TO KCONES, EXECPT CERTAIN CRITICAL AREAS ARE
C             COMPUTED IN D.P. FOR IMPROVED ACCURACY
C     KCONED  FOR MAHCINES WITH LESS THEN 60 BIT WORD, WITHOUT QUAD
C             PRECISION SOFTWARE SUPPORT (e.g. DEC3100)
C             C.P. COMPUTAION IS USED
C     KCONEQ, SIMILAR TO KCONED, EXECPT CERTAIN CRITICAL AREAS ARE
C             COMPUTED IN QUAD PREC. FOR IMPROVED ACCURACY
C
C     ORIGINALLY, THIS ROUTINE CALLS KCONEX AND KCONEY/Z. THESE THREE
C     SUPPORTING ROUTINES ARE NOW MOVED INTO KCONES (AND ALSO KCONED)
C
C     ECPT( 1) = ELEMENT ID             INTEGER        ECT
C     ECPT( 2) = SIL PT A               INTEGER        ECT
C     ECPT( 3) = SIL PT B               INTEGER        ECT
C     ECPT( 4) = MATID 1                INTEGER        EPT
C     ECPT( 5) = T   (MEMBRANE THICK)   REAL           EPT
C     ECPT( 6) = MATID 2                INTEGER        EPT
C     ECPT( 7) = I   (MOM.OF INERTIA)   REAL           EPT
C     ECPT( 8) = MATID 3                INTEGER        EPT
C     ECPT( 9) = TS  (SHEAR THICKNESS)  REAL           EPT
C     ECPT(10) = NON-STRUCTURAL-MASS    REAL           EPT
C     ECPT(11) = Z1                     REAL           EPT
C     ECPT(12) = Z2                     REAL           EPT
C     ECPT(13) = PHI  1                 REAL           EPT
C     ECPT(14) = PHI  2                 REAL           EPT
C     ECPT(15) = PHI  3                 REAL           EPT
C     ECPT(16) = PHI  4                 REAL           EPT
C     ECPT(17) = PHI  5                 REAL           EPT
C     ECPT(18) = PHI  6                 REAL           EPT
C     ECPT(19) = PHI  7                 REAL           EPT
C     ECPT(20) = PHI  8                 REAL           EPT
C     ECPT(21) = PHI  9                 REAL           EPT
C     ECPT(22) = PHI 10                 REAL           EPT
C     ECPT(23) = PHI 11                 REAL           EPT
C     ECPT(24) = PHI 12                 REAL           EPT
C     ECPT(25) = PHI 13                 REAL           EPT
C     ECPT(26) = PHI 14                 REAL           EPT
C     ECPT(27) = COORD. SYS. ID PT.1    INTEGER        BGPDT
C     ECPT(28) = RADIUS PT. 1           REAL           BGPDT
C     ECPT(29) = DISTANCE TO PT.1       REAL           BGPDT
C     ECPT(30) = NULL                   REAL           BGPDT
C     ECPT(31) = COORD. SYS. ID PT.2    INTEGER        BGPDT
C     ECPT(32) = RADIUS PT 2            REAL           BGPDT
C     ECPT(33) = DISTANCE TO PT. 2      REAL           BGPDT
C     ECPT(34) = NULL                   REAL           BGPDT
C     ECPT(35) = ELEMENT TEMPERATURE    REAL           GEOM3
C
C
      INTEGER          NERROR(2)     ,NECPT(100)     ,NA(7)  ,
     1                 OLDPT1 ,OLDPT2
      REAL             I00    ,I01   ,I02    ,I03    ,I04    ,
     1                 I10    ,I11   ,I12    ,I13    ,I14    ,
     2                 I20    ,I21   ,I22    ,I23    ,I24    ,
     3                         I31   ,I32    ,I33    ,I34    ,
     4                                I42    ,I43    ,I44    ,
     5                                I52    ,I53    ,I54    ,
     6                                I62    ,I63    ,I64
      REAL             KQN(10,10)    ,KQX(10,10)     ,KQE(10,10)     ,
     1                 KQY(10,10)    ,FAC(7) ,H(120) ,INTEG  ,KIJ    ,
     2                 NSPOPI ,N     ,N2     ,NSP    ,L2     ,NCP    ,
     3                 N2E22  ,N2E33 ,N2D33
      DOUBLE PRECISION SUM    ,QQ1   ,QQ2    ,QQ3    ,QQ4    ,KIJD
      COMMON /CONDAS/  CONSTS(5)
      COMMON /MATIN /  MATID  ,INFLAG,ELTEMP ,STRESS ,SINTH  ,COSTH
      COMMON /MATOUT/  G11    ,G12   ,G13    ,G22    ,G23    ,G33     ,
     1                 DUM(5) ,GSUBE
      COMMON /SMA1IO/  DUM1(10)      ,IFKGG  ,DUM2   ,IF4GG
      COMMON /SMA1CL/  IOPT4  ,K4GGSW,NPVT   ,DUMCL(7)       ,LINK(10),
     1                 IDETCK ,DODET ,NOGO
      COMMON /SMA1ET/  ECPT(100)
      COMMON /SMA1DP/  SUM    ,QQ1   ,QQ2    ,QQ3    ,QQ4    ,KIJD(36),
     1                 INTEG(28)     ,KIJ(36),HUQ(100)       ,HYQF(10),
     2                 HYQ(10),TEMP60(60)    ,OPI    ,ZA     ,E11     ,
     3                 CP     ,SPE22 ,ZB     ,E12    ,SP     ,CPE22   ,
     4                 A      ,E22   ,CP2    ,SP2E22 ,B      ,E33     ,
     5                 SP2    ,CP2E22,SIGN   ,T      ,D11    ,TEMP1   ,
     6                 RA     ,TS    ,D12    ,TEMP2  ,RB     ,N       ,
     7                 D22    ,TEMP3 ,RASQ   ,N2     ,D33    ,TEMP4   ,
     8                 RBSQ   ,SL    ,NSP    ,TEMP5  ,TN     ,L2      ,
     9                 NCP    ,TEMP6 ,PIOVB  ,DL     ,SPE12  ,TEMP7   ,
     O                 TD     ,TEMP  ,CPE12  ,OQ     ,N2E22  ,TWOD33  ,
     1                 TNSP   ,N2E33  ,SP2E33,SPE33
      EQUIVALENCE      (CONSTS(1),PI  ), (ECPT(4),MATID1),
     1                 (ECPT(6),MATID2), (ECPT(8),MATID3),
     2                 (ECPT(1),NECPT(1))
      EQUIVALENCE      (G,G12), (KQN(1,1),KQE(1,1),KQX(1,1),KQY(1,1))
      EQUIVALENCE      (HYQ(1),H11), (HYQ(2),H12), (HYQ(3),H13),
     1                 (HYQ(4),H14), (HYQ(5),H15), (HYQ(6),H16),
     2                 (HYQ(7),H17), (HYQ(8),H18), (HYQ(9),H19),
     3                 (HYQ(10),H1TEN)
      EQUIVALENCE      (I00,INTEG( 1)), (I20,INTEG(11)),
     1                 (I01,INTEG( 2)), (I21,INTEG(12)),
     2                 (I02,INTEG( 3)), (I22,INTEG(13)),
     3                 (I03,INTEG( 4)), (I23,INTEG(14)),
     4                 (I04,INTEG( 5)), (I24,INTEG(15)),
     5                 (I10,INTEG( 6)), (I31,INTEG(16)),
     6                 (I11,INTEG( 7)), (I32,INTEG(17)),
     7                 (I12,INTEG( 8)), (I33,INTEG(18)),
     8                 (I13,INTEG( 9)), (I34,INTEG(19)),
     9                 (I14,INTEG(10)), (I52,INTEG(23)),
     O                 (I42,INTEG(20)), (I53,INTEG(24)),
     1                 (I43,INTEG(21)), (I54,INTEG(25)),
     2                 (I44,INTEG(22)), (I62,INTEG(26)),
     3                 (I63,INTEG(27)), (I64,INTEG(28))
      DATA    OLDPT1,  OLDPT2 / 0, 0  /
      DATA    FAC   /  1.0, 1.0, 2.0, 6.0, 24.0, 120.0, 720.0 /
      DATA    NA    /  1,1,1,2,3,3,3  /
      DATA    ONE   /  1.0   /
C
C     DOES PIVOT POINT EQUAL EITHER OF THE LAST TWO SILS
C
      IF (OLDPT1 .EQ. NECPT(2)) IF (OLDPT2-NECPT(3)) 10,110,10
      IF (OLDPT2 .EQ. NECPT(2)) IF (OLDPT1-NECPT(3)) 10,110,10
   10 CONTINUE
C
C     NO MATCH THUS DO ENTIRE COMPUTATION
C
      SINTH = 0.0
      COSTH = 1.0
      NINT  = NECPT(1) - (NECPT(1)/1000)*1000 - 1
      N     = NINT
      RA    = ECPT(28)
      ZA    = ECPT(29)
      RB    = ECPT(32)
      ZB    = ECPT(33)
      TEMP1 = RB - RA
      TEMP2 = ZB - ZA
      SL    = SQRT(TEMP1**2 + TEMP2**2)
      L2    = SL*SL
      IF (SL) 30,20,30
   20 NERROR(1) = NECPT(1)/1000
      NERROR(2) = N + .3
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
      A  = RA
      B  = SP
      IF (ABS(B) .GT. 0.001) GO TO 60
C
C     GO TO 40 FOR B = 0.
C
C                               1-N
C                         PI  RA     M+1
C     FOR B = 0,   I   = --------- SL    (FOR ALL M,N .GE. 0)
C                   M,N    M + 1
C
C  40 CONTINUE
C
      IDX = 0
      DO 50 I = 1,7
      NBEGIN = NA(I)
C
      DO 50 J = NBEGIN,5
C
C     M = I - 1
C     N = J - 1
C     MPLUS1 THUS EQUALS I
C
      IDX = IDX + 1
      INTEG(IDX) = (PI*SL**I)/(FLOAT(I)*RA**(J-2))
   50 CONTINUE
      GO TO 100
C
C     ABOVE COMPLETES ALL INTEGRALS FOR B = 0.
C
   60 CONTINUE
C
C     FOR B .NE. ZERO
C
C     IF AN OVERFLOW RESULTS BELOW POSSIBLY B IS NOT ZERO, BUT SMALL
C
C     OK BELOW IS FOR B NOT EQUAL TO ZERO
C
C     FIRST M = 0 CASE
C
C                             2-N     2-N
C                       PI (RB    - RA   )
C               I    = --------------------   (N NOT EQUAL TO 2)
C                0,N        (2-N) B
C
C
C     FOR N=2   I    = PI * (LOG RB  -  LOG RA) / B
C                0,2            E          E
C
      RASQ  = RA*RA
      RBSQ  = RB*RB
      PIOVB = PI/B
C
      INTEG(1) = 0.5*PIOVB*(RBSQ - RASQ)
      INTEG(2) = PIOVB*(RB - RA)
      INTEG(3) = PIOVB*LOG(RB/RA)
      INTEG(4) =-PIOVB*(ONE/RB - ONE/RA)
      INTEG(5) =-0.5*PIOVB*(ONE/RBSQ - ONE/RASQ)
C
      IDX  = 5
      DO 90 I = 1,6
      MPLUS1 = I + 1
      NBEGIN = NA(MPLUS1)
      DO 90 J = NBEGIN,5
C
C     M = I
C     N = J - 1
C
C     WE ARE GETTING INTEGRAL(M,N)
C     M = POWER OF S
C     N = POWER OF R
C
C     EVALUATING AT R = RB,  THEN AT R = RA
C
C                                    K   MNK2
C                (M)FAC.     M   (-A) (R)
C     I  = (PI) (-------) ((SUM -------------------------) + (TERM-X))
C      MN          (M+1)    K=0  (M-K)FAC. (K)FAC. (MNK2)
C                 B        (FOR K.NE. MN2                   (FOR K=MN2)
C
C       WHERE    MNK2 = M-N-K+2
C                MN2  = M-N  +2
C             (X)FAC. = X!
C                             MN2
C                         (-A)    LOG(R)
C              TERM-X = --------------------
C                       (M-N+2)FAC. (N-2)FAC.
C
C     NOTE IN DATA STATEMENT THAT 0 FACTORIAL = FAC(1)
C                                 1 FACTORIAL = FAC(2)
C                                 2 FACTORIAL = FAC(3)    ETC.
C
      SUM  = 0.0
      SIGN =-1.0
      DO 80 KK = 1,MPLUS1
      SIGN =-SIGN
      K    = KK - 1
      MN2  = I - J + 3
      QQ1  = DBLE(A )
      QQ2  = DBLE(RB)
      QQ3  = DBLE(RA)
      IF (K .EQ. MN2) GO TO 70
      MNK2 = MN2 - K
      MK1  = MPLUS1 - K
      TEMP = MNK2
C
C     QQ4  = A**K*(RB**MNK2-RA**MNK2)/(FAC(MK1)*FAC(KK)*TEMP)
C
      QQ1  = QQ1**K
      QQ2  = QQ2**MNK2
      QQ3  = QQ3**MNK2
      QQ2  = QQ2 - QQ3
      QQ3  = DBLE(FAC(MK1)*FAC(KK)*TEMP)
      GO TO 75
C
C     QQ4 = A**MN2*DLOG(RB/RA)/(FAC(MN2+1)*FAC(J-2))
C
   70 QQ1 = QQ1**MN2
      QQ3 = QQ2/QQ3
      QQ2 = DLOG(QQ3)
      QQ3 = DBLE(FAC(MN2+1)*FAC(J-2))
   75 QQ4 = QQ1*QQ2/QQ3
   80 SUM = SUM + DBLE(SIGN)*QQ4
C
      QQ1 = DBLE(PI*FAC(MPLUS1))
      QQ2 = DBLE(B)
      QQ3 = QQ2**MPLUS1
      QQ4 = SUM*QQ1/QQ3
      IDX = IDX + 1
      INTEG(IDX) = SNGL(QQ4)
   90 CONTINUE
C
  100 OLDPT1 = NECPT(2)
      OLDPT2 = NECPT(3)
      GO TO 140
C
C     WE HAVE A MATCH ON OLD SIL NUMBER 1
C
  110 IF (NPVT-OLDPT1) 130,120,130
  120 NPIVOT = 1
      GO TO 410
C
C     WE HAVE A MATCH ON OLD SIL NUMBER 2
C
  130 NPIVOT = 2
      GO TO 410
C
C     ZERO OUT THE KQN MATRIX
C
  140 DO 150 I = 1,10
      DO 150 J = 1,10
  150 KQN(I,J) = 0.0
C
C     IF MEMBRANE THICKNESS IS NOT ZERO FORM THE KQE MATRIX
C
      T = ECPT(5)
      IF (T) 160,200,160
  160 ASSIGN 190 TO IRETRN
      MATID  = MATID1
  170 INFLAG = 2
  180 ELTEMP = ECPT(35)
      CALL MAT (ECPT(1))
      GO TO IRETRN, (190,230,242)
  190 E11 = G11
      E12 = G12
      E22 = G22
      E33 = G33
      TN  = T *N
      CP2 = CP*CP
      SP2 = SP*SP
      N2  = N *N
      CP2E22= CP2*E22
      SP2E22= SP2*E22
      CPE22 = CP *E22
      SPE22 = SP *E22
      CPE12 = CP *E12
      SPE12 = SP *E12
      N2E33 = N2 *E33
      N2E22 = N2 *E22
      SP2E33= SP2*E33
      SPE33 = SP *E33
C
C /// FURTHER REDUCTION IS NEEDED HERE ///
C
      KQE(1,1) = T*(N2E22 + SP2E33)*I02
      KQE(1,2) = T*(N2E22*I12 - SPE33*I01 + SP2E33*I12)
      TEMP     = E22 + E33
      TNSP     = TN*SP
      KQE(1,3) = TNSP*TEMP*I02
      KQE(1,4) = TN*(E12*I01 + SP*TEMP*I12)
      TEMP     = TN*CP*E22
      KQE(1,5) = TEMP*I02
      KQE(1,6) = TEMP*I12
      KQE(1,7) = TEMP*I22
      KQE(1,8) = TEMP*I32
      TEMP4    = 2.*SP*I11
      KQE(2,2) = T *(N2E22*I22 + E33*(I00-TEMP4 + SP2*I22))
      KQE(2,3) = TN*(SPE22*I12 - E33*I01 + SPE33*I12)
      KQE(2,4) = TN*(E12*I11 + SPE22*I22 - E33*I11 + SPE33*I22)
      KQE(2,5) = KQE(1,6)
      KQE(2,6) = KQE(1,7)
      KQE(2,7) = KQE(1,8)
      KQE(2,8) = TN*CPE22*I42
      KQE(3,3) = T*(SP2E22*I02 + N2E33 *I02)
      KQE(3,4) = T*(SPE12 *I01 + SP2E22*I12 + N2E33*I12)
      TEMP     = T*CP*SPE22
      KQE(3,5) = TEMP*I02
      KQE(3,6) = TEMP*I12
      KQE(3,7) = TEMP*I22
      KQE(3,8) = TEMP*I32
      KQE(4,4) = T*(E11*I00 + TEMP4*E12 + SP2E22*I22 + N2E33*I22)
      TEMP     = SP*CPE22
      KQE(4,5) = T*(CPE12*I01 + TEMP*I12)
      KQE(4,6) = T*(CPE12*I11 + TEMP*I22)
      KQE(4,7) = T*(CPE12*I21 + TEMP*I32)
      KQE(4,8) = T*(CPE12*I31 + TEMP*I42)
      TEMP     = T*CP2E22
      KQE(5,5) = TEMP*I02
      KQE(5,6) = TEMP*I12
      KQE(5,7) = TEMP*I22
      KQE(5,8) = TEMP*I32
      KQE(6,6) = KQE(5,7)
      KQE(6,7) = KQE(5,8)
      KQE(6,8) = TEMP*I42
      KQE(7,7) = KQE(6,8)
      KQE(7,8) = TEMP*I52
      KQE(8,8) = TEMP*I62
C
  200 IF (ECPT(7) .EQ. 0.0) GO TO 270
C
C     NOW GET G MATERIAL MATRIX ID = MATID2
C
      MATID = MATID2
      ASSIGN 230 TO IRETRN
      GO TO 170
C
C     NOW FORM D = I DOT G
C
  230 D11 = ECPT(7)*G11
      D12 = ECPT(7)*G12
      D22 = ECPT(7)*G22
      D33 = ECPT(7)*G33
C
C     IF SHEAR THICKNESS IS NOT ZERO FORM THE HYQ AND KQY MATRICES
C
      TS = ECPT(9)
      IF (TS) 240,265,240
  240 CONTINUE
C
C     GET G FOR MATID3
C
      MATID  = MATID3
      INFLAG = 1
      ASSIGN 242 TO IRETRN
      GO TO 180
C
  242 CONTINUE
      IF (G .EQ. 0.0) GO TO 261
C
C     FORMING 1.0/Q DIRECTLY
C
      OPI = ONE/PI
C
C /// MAKE SURE ALL BASIC PRODUCTS ARE AT TOP BEFORE ANY SKIPS
C
      N2D33  = N2 *D33
      SP2D22 = SP2*D22
      OQ     = SL*TS*G*(RA+RB)*0.5 + I02*(N2D33+SP2D22)*OPI
      OQ     = ONE/OQ
      NSP    = N*SP
      NCP    = N*CP
      NSPOPI = NSP*OPI
      TWOD33 = 2.0*D33
      TEMP1  = D12*(ONE/RB - ONE/RA)
      TEMP2  = NSPOPI*(D22 + D33)
      TEMP3  = N*NSPOPI*(TWOD33 + D22)
      TEMP4  = OQ*0.5*NCP*N*D33*OPI
      TEMP5  = OPI*(N2*TWOD33 + SP2*D22)
      TEMP6  = D12*N2*L2/RB
      TEMP7  = NSPOPI*CP*0.5
      HYQ(1) = OQ*(TEMP1*NCP - TEMP7*I03*(D33+2.0*D22))
      HYQ(2) = OQ*(NCP*SL/RB*D12 - TEMP7*I13*(3.0*D33+D22)
     1       + 1.5*NCP*OPI*I02*D33)
      HYQ(3) = TEMP4*I03
      HYQ(4) = TEMP4*I13
      HYQ(5) = OQ*(TEMP1*N2 - TEMP3*I03)
      HYQ(6) = OQ*(D12*N2*SL/RB - TEMP3*I13 + TEMP5*I02)
      HYQ(7) = OQ*(2.0*D11*(RA-RB) + TEMP6+2.0*I12*TEMP5 - TEMP3*I23)
      HYQ(8) = OQ*(-D11*6.*SL*RB + TEMP6*SL+3.*I22*TEMP5 - TEMP3*I33)
      HYQ(9) =-OQ*TEMP2*I02
      HYQ(10)= OQ*(N*SL*(D12 + D33) - TEMP2*I12)
C
      TEMP = TS*G*I00
      DO 250 I = 1,10
  250 HYQF(I) = HYQ(I)*TEMP
      DO 260 I = 1,10
      DO 260 J = I,10
  260 KQY(I,J) = KQY(I,J) + HYQ(I)*HYQF(J)
C
C     ADD IN TERMS PER EQUATION-90- PAGE -27- MS-28
C
      TEMP = TS*G
      KQY( 9,10) = KQY( 9,10) + TEMP*I10
      KQY(10,10) = KQY(10,10) + TEMP*I20
      KQY( 9, 9) = KQY( 9, 9) + TEMP*I00
C
C     END OF KQY COMPUTATION
C
      GO TO 265
  261 TS = 0.0
  265 CONTINUE
C
C     THE FOLLOWING CODES WERE MOVED HERE FROM KCONEX
C
C     KQX MATRIX FOR SHEAR THICKNESS CONSIDERATION
C
C     (THE FOLLOWING CODE WAS MACHINE GENERATED AND WILL NOT BE SIMPLI-
C     FIED FURTHER UNTIL FORMULATION VERIFICATION IS COMPLETED)
C
      KQX(1, 1) = KQX(1, 1) + CP*CP*I04*(+D22*N**2+2.25*D33*SP**2)
      KQX(1, 2) = KQX(1, 2) + CP*CP*(D33*SP*(+2.25*SP*I14-2.25*I03)
     1          + D22*N*N*I14)
      KQX(1, 3) = KQX(1, 3) + D33*CP*CP*SP*N*I04*(-0.75)
      KQX(1, 4) = KQX(1, 4) + D33*CP*CP*SP*N*I14*(-0.75)
      KQX(1, 5) = KQX(1, 5) + CP*N*I04*(+D22*N**2+3.0*D33*SP**2)
      KQX(1, 6) = KQX(1, 6) + CP*N*(SP*(D33*(+3.0*SP*I14-3.0*I03)
     1          - D22*I03)  + D22*N*N*I14)
      KQX(1, 7) = KQX(1, 7) + CP*N*(SP*(D33*(+3.0*SP*I24-6.0*I13)
     1          + D22*I13*(-2.0)) - 2.0*D12*I02 + D22*N**2*I24)
      KQX(1, 8) = KQX(1, 8) + CP*N*(SP*(D33*(+3.0*SP*I34-9.0*I23)
     1          + D22*I23*(-3.0)) - 6.0*D12*I12 + D22*N**2*I34)
      KQX(1, 9) = KQX(1, 9) + CP*I03*(+D22*N**2+1.5*D33*SP**2)
      KQX(1,10) = KQX(1,10) + CP*(D33*SP*(-1.5*I02+1.5*SP*I13)
     1          + D22*N*N*I13)
      KQX(2, 2) = KQX(2, 2) + CP*CP*(D33*(SP*(I13*(-4.5)
     1          + SP*I24*2.25) + I02*2.25) + D22*N*N*I24)
      KQX(2, 3) = KQX(2, 3) + D33*CP*CP*N*(-0.75*SP*I14+0.75*I03)
      KQX(2, 4) = KQX(2, 4) + D33*CP*CP*N*(-0.75*SP*I24+0.75*I13)
      KQX(2, 5) = KQX(2, 5) + CP*N*(D33*SP*(+3.0*SP*I14-3.0*I03)
     1          + D22*N*N*I14)
      KQX(2, 6) = KQX(2, 6) + CP*N*(D33*(SP*(I13*(-6.0)
     1          + SP*I24*3.0) + I02*3.0) + D22*(-SP*I13+N**2*I24))
      KQX(2, 7) = KQX(2, 7) + CP*N*(D33*(SP*(I23*(-9.0)
     1          + SP*I34*3.0) + I12*6.0)
     2          + D22*(-2.0*SP*I23 + N**2*I34) + D12*I12*(-2.0))
      KQX(2, 8) = KQX(2, 8) + CP*N*(D33*(SP*(I33*(-12.0)
     1          + SP*I44*3.0) + I22*9.0)
     2          + D22*(-3.0*SP*I33+N**2*I44) + D12*I22*(-6.0))
      KQX(2, 9) = KQX(2, 9) + CP*(D33*SP*(+1.5*SP*I13-1.5*I02)
     1          + D22*N*N*I13)
      KQX(2,10) = KQX(2,10) + CP*(D33*(SP*(I12*(-3.0)+SP*I23*1.5)
     1          + I01*1.5)+ D22*N*N*I23)
      KQX(3, 3) = KQX(3, 3) + D33*CP*CP*N*N*I04*0.25
      KQX(3, 4) = KQX(3, 4) + D33*CP*CP*N*N*I14*0.25
      KQX(3, 5) = KQX(3, 5) + D33*CP*SP*N*N*I04*(-1.0)
      KQX(3, 6) = KQX(3, 6) + D33*CP*N*N*(-SP*I14+I03)
      KQX(3, 7) = KQX(3, 7) + D33*CP*N*N*(-SP*I24+2.0*I13)
      KQX(3, 8) = KQX(3, 8) + D33*CP*N*N*(-SP*I34+3.0*I23)
      KQX(3, 9) = KQX(3, 9) + D33*CP*SP*N*I03*(-0.5)
      KQX(3,10) = KQX(3,10) + D33*CP*N*(+0.5*I02-0.5*SP*I13)
      KQX(4, 4) = KQX(4, 4) + D33*CP*CP*N*N*I24*0.25
      KQX(4, 5) = KQX(4, 5) + D33*CP*SP*N*N*I14*(-1.0)
      KQX(4, 6) = KQX(4, 6) + D33*CP*N*N*(-SP*I24+I13)
      KQX(4, 7) = KQX(4, 7) + D33*CP*N*N*(-SP*I34+2.0*I23)
      KQX(4, 8) = KQX(4, 8) + D33*CP*N*N*(-SP*I44+3.0*I33)
      KQX(4, 9) = KQX(4, 9) + D33*CP*SP*N*I13*(-0.5)
      KQX(4,10) = KQX(4,10) + D33*CP*N*(+0.5*I12-0.5*SP*I23)
      KQX(5, 5) = KQX(5, 5) + N*N*I04*(+D22*N**2+4.0*D33*SP**2)
      KQX(5, 6) = KQX(5, 6) + N*N*(SP*(D33*(+4.0*SP*I14-4.0*I03)
     1          + D22*I03*(-1.0)) + D22*N*N*I14)
      KQX(5, 7) = KQX(5, 7) + N*N*(SP*(D33*(+4.0*SP*I24-8.0*I13)
     1          + D22*I13*(-2.0)) - 2.0*D12*I02 + D22*N**2*I24)
      KQX(5, 8) = KQX(5, 8) + N*N*(SP*(D33*(+4.0*SP*I34-12.0*I23)
     1          + D22*I23*(-3.0)) - 6.0*D12*I12 + D22*N**2*I34)
      KQX(5, 9) = KQX(5, 9) + N*I03*(+D22*N**2+2.0*D33*SP**2)
      KQX(5,10) = KQX(5,10) + N*(D33*SP*(-2.0*I02+2.0*SP*I13)
     1          + D22*N*N*I13)
      KQX(6, 6) = KQX(6, 6) + N*N*(SP*(I13*(D22*(-2.0)+D33*(-8.0))
     1          + D33*SP*I24*4.0) + D22*N**2*I24 + 4.0*D33*I02)
     2          + D22*SP*SP*I02
      KQX(6, 7) = KQX(6, 7) + N*N*(SP*(I23*(D22*(-3.0)+D33*(-12.0))
     1          + D33*SP*I34*4.0) + I12*(-2.0*D12+8.0*D33)
     2          + D22*N*N*I34) + SP*(+2.0*D12*I01+2.0*D22*SP*I12)
      KQX(6, 8) = KQX(6, 8) + N*N*(SP*(I33*(D22*(-4.0)+D33*(-16.0))
     1          + D33*SP*I44*4.0) + I22*(-6.0*D12+12.0*D33)
     2          + D22*N*N*I44)+SP*(+6.0*D12*I11+3.0*D22*SP*I22)
      KQX(6, 9) = KQX(6, 9) + N*(SP*(D33*(+2.0*SP*I13-2.0*I02)
     1          + D22*I02*(-1.0)) + D22*N*N*I13)
      KQX(6,10) = KQX(6,10) + N*(D33*(SP*(I12*(-4.0) + SP*I23*2.0)
     1          + I01*2.0)+ D22*(+N**2*I23-SP*I12))
      KQX(7, 7) = KQX(7, 7) + N*N*(SP*(I33*(D22*(-4.0)+D33*(-16.0))
     1          + D33*SP*I44*4.0) + I22*(D12*(-4.0) +D33*16.0)
     2          + D22*N*N*I44) + SP*(D12*I11*8.0+D22*SP*I22*4.0)
     3          + D11*I00*4.0
      KQX(7, 8) = KQX(7, 8) + N*N*(SP*(I43*(D22*(-5.0)+D33*(-20.0))
     1          + D33*SP*I54*4.0) + I32*(D12*(-8.0)+D33*24.0)
     2          + D22*N*N*I54) + SP*(D12*I21*18.0+D22*SP*I32*6.0)
     3          + D11*I10*12.0
      KQX(7, 9) = KQX(7, 9) + N*(SP*(D33*(+2.0*SP*I23-4.0*I12)
     1          + D22*I12*(-2.0)) - 2.0*D12*I01 + D22*N**2*I23)
      KQX(7,10) = KQX(7,10) + N*(D33*(SP*(I22*(-6.0)+SP*I33*2.0)
     1          + I11*4.0)+ D22*(+N**2*I33-2.0*SP*I22)
     2          + D12*I11*(-2.0))
      KQX(8, 8) = KQX(8, 8) + N*N*(SP*(I53*(D22*(-6.0)+D33*(-24.0))
     1          + D33*SP*I64*4.0) + I42*(D12*(-12.0) + D33*36.0)
     2          + D22*N*N*I64) + SP*(D12*I31*36.0+D22*SP*I42*9.0)
     3          + D11*I20*36.0
      KQX(8, 9) = KQX(8, 9) + N*(SP*(D33*(+2.0*SP*I33-6.0*I22)
     1          + D22*I22*(-3.0)) - 6.0*D12*I11 + D22*N**2*I33)
      KQX(8,10) = KQX(8,10) + N*(D33*(SP*(I32*(-8.0)+SP*I43*2.0)
     1          + I21*6.0)+ D22*(+N**2*I43-3.0*SP*I32)
     2          + D12*I21*(-6.0))
      KQX(9, 9) = KQX(9, 9) + I02*(+D22*N**2+D33*SP**2)
      KQX(9,10) = KQX(9,10) + D33*SP*(-I01+SP*I12) + D22*N*N*I12
      KQX(10,10)= KQX(10,10)+ D33*(SP*(I11*(-2.0)+ SP*I22)+I00)
     1          + D22*N*N*I22
      IF (TS .EQ. 0.0) GO TO 270
C
C     THE FOLLOWING CODES WERE MOVED HERE FROM KCONEY
C
      KQX(1, 1) = KQX(1, 1) + H11*(SP*(CP*N*I03*(D22*2.0+D33*3.0)
     1          + D22*SP*H11*I02) + D33*N*N*H11*I02)
      KQX(1, 2) = KQX(1, 2) + N*(CP*(SP*(D22*(+H12*I03+H11*I13)
     1          + D33*(+1.5*H12*I03+1.5*H11*I13))
     2          + D33*H11*I02*(-1.5)) + D33*N*H11*H12*I02)
     3          + D22*SP*SP*H11*H12*I02
      KQX(1, 3) = KQX(1, 3) + N*(D33*(CP*I03*(+1.5*SP*H13
     1          - 0.5*N*H11) + N*H11*H13*I02) + D22*CP*SP*H13*I03)
     2          + D22*SP*SP*H11*H13*I02
      KQX(1, 4) = KQX(1, 4) + N*(D33*(CP*(+1.5*SP*H14*I03
     1          - 0.5*N*H11*I13)+N*H11*H14*I02) + D22*CP*SP*H14*I03)
     2          + D22*SP*SP*H11*H14*I02
      KQX(1, 5) = KQX(1, 5) + SP*(N*I03*(D22*(+CP*H15+N*H11)
     1          + D33*(+1.5*CP*H15+2.0*N*H11))
     2          + D22*SP*H11*H15*I02) + D33*N*N*H11*H15*I02
      KQX(1, 6) = KQX(1, 6) + SP*(D22*(H11*(SP*I02*(-1.0+H16)
     1          + N*N*I13)  + CP*N*H16*I03)+D33*N*(+1.5*CP*H16*I03
     2          + 2.0*N*H11*I13)) + D33*N*N*H11*I02*(-2.0+H16)
      KQX(1, 7) = KQX(1, 7) + SP*(H11*(D22*(SP*(-2.0*I12+H17*I02)
     1          + N*N*I23)  - 2.0*D12*I01 + 2.0*D33*N**2*I23)
     2          + CP*N*H17*I03*(+D22+1.5*D33))
     3          + D33*N*N*H11*(-4.0*I12+H17*I02)
      KQX(1, 8) = KQX(1, 8) + SP*(H11*(D22*(SP*(-3.0*I22+H18*I02)
     1          + N*N*I33)  - 6.0*D12*I11 + 2.0*D33*N**2*I33)
     2          + CP*N*H18*I03*(+D22+1.5*D33))
     3          + D33*N*N*H11*(-6.0*I22+H18*I02)
      KQX(1, 9) = KQX(1, 9) + SP*(N*(D22*(+CP*H19*I03+H11*I02)
     1          + D33*(+1.5*CP*H19*I03 + H11*I02))
     2          + D22*SP*H11*H19*I02) + D33*N*N*H11*H19*I02
      KQX(1,10) = KQX(1,10) + N*(D33*(H11*(-I01+SP*I12+N*H1TEN*I02)
     1          + CP*SP*H1TEN*I03*1.5) + D22*SP*(+CP*H1TEN*I03
     2          + H11*I12)) + D22*SP*SP*H11*H1TEN*I02
      KQX(2, 2) = KQX(2, 2) + H12*(N*(CP*(D33*(SP*I13*3.+I02*(-3.))
     1          + D22*SP*I13*2.) + D33*N*H12*I02) + D22*SP*SP*H12*I02)
      KQX(2, 3) = KQX(2, 3) + N*(D33*(CP*(H13*(+1.5*SP*I13-1.5*I02)
     1          + N*H12*I03*(-0.5)) + N*H12*H13*I02)
     2          + D22*CP*SP*H13*I13) + D22*SP*SP*H12*H13*I02
      KQX(2, 4) = KQX(2, 4) + N*(D33*(CP*(H14*(+1.5*SP*I13-1.5*I02)
     1          + N*H12*I13*(-0.5)) + N*H12*H14*I02)
     2          + D22*CP*SP*H14*I13) + D22*SP*SP*H12*H14*I02
      KQX(2, 5) = KQX(2, 5) + N*(D33*(H15*(CP*(+1.5*SP*I13-1.5*I02)
     1          + N*H12*I02)+ SP*N*H12*I03*2.0) + D22*SP*(+CP*H15*I13
     2          + N*H12*I03)) + D22*SP*SP*H12*H15*I02
      KQX(2, 6) = KQX(2, 6) + N*(D33*(N*H12*(I02*(-2.0+H16)
     1          + SP*I13*2.0) + CP*H16*(+1.5*SP*I13-1.5*I02))
     2          + D22*SP*I13*(+CP*H16+N*H12))
     2          + D22*SP*SP*H12*I02*(-1.0+H16)
      KQX(2, 7) = KQX(2, 7) + SP*(H12*(D22*(SP*(-2.0*I12+H17*I02)
     1          + N*N*I23)  - 2.0*D12*I01 + 2.0*D33*N**2*I23)
     2          + CP*N*H17*I13*(+D22+1.5*D33))
     3          + D33*N*(N*H12*(-4.0*I12 + H17*I02)
     4          + CP*H17*I02*(-1.5))
      KQX(2, 8) = KQX(2, 8) + SP*(H12*(D22*(SP*(-3.0*I22+H18*I02)
     1          + N*N*I33)  - 6.0*D12*I11 + 2.0*D33*N**2*I33)
     2          + CP*N*H18*I13*(+D22+1.5*D33))
     3          + D33*N*(N*H12*(-6.0*I22 + H18*I02)
     4          + CP*H18*I02*(-1.5))
      KQX(2, 9) = KQX(2, 9) + N*(D33*(H19*(CP*(+1.5*SP*I13-1.5*I02)
     1          + N*H12*I02)+ SP*H12*I02)+D22*SP*(+CP*H19*I13+H12*I02))
     2          + D22*SP*SP*H12*H19*I02
      KQX(2,10) = KQX(2,10) + N*(D33*(H12*(-I01+SP*I12+N*H1TEN*I02)
     1          + CP*H1TEN*(+1.5*SP*I13 - 1.5*I02))
     2          + D22*SP*(+CP*H1TEN*I13 + H12*I12))
     3          + D22*SP*SP*H12*H1TEN*I02
      KQX(3, 3) = KQX(3, 3) + H13*(D33*N*N*(CP*I03*(-1.0)+H13*I02)
     1          + D22*SP*SP*H13*I02)
      KQX(3, 4) = KQX(3, 4) + D33*N*N*(CP*(-0.5*H14*I03-0.5*H13
     1          * I13)+H13*H14*I02) + D22*SP*SP*H13*H14*I02
      KQX(3, 5) = KQX(3, 5) + N*N*(D33*(H13*(+2.0*SP*I03+H15*I02)
     1          + CP*H15*I03*(-0.5)) + D22*SP*H13*I03)
     2          + D22*SP*SP*H13*H15*I02
      KQX(3, 6) = KQX(3, 6) + H13*(SP*(D22*(SP*I02*(-1.+H16)+N*N*I13)
     1          + D33*N*N*I13*2.0) + D33*N*N*I02*(-2.0+H16))
     2          + D33*CP*N*N*H16*I03*(-0.5)
      KQX(3, 7) = KQX(3, 7) + H13*(SP*(D22*(SP*(-2.0*I12+H17*I02)
     1          + N*N*I23)  - 2.0*D12*I01 + 2.0*D33*N**2*I23)
     2          + D33*N*N*(-4.0*I12 + H17*I02))
     3          + D33*CP*N*N*H17*I03*(-0.5)
      KQX(3, 8) = KQX(3, 8) + H13*(SP*(D22*(SP*(-3.0*I22+H18*I02)
     1          + N*N*I33)  - 6.0*D12*I11+2.0*D33*N**2*I33)
     2          + D33*N*N*(-6.0*I22 + H18*I02))
     3          + D33*CP*N*N*H18*I03*(-0.5)
      KQX(3, 9) = KQX(3, 9) + N*(D33*(N*H19*(-0.5*CP*I03+H13*I02)
     1          + SP*H13*I02) + D22*SP*H13*I02) + D22*SP*SP*H13*H19*I02
      KQX(3,10) = KQX(3,10) + N*(D33*(H13*(-I01+SP*I12+N*H1TEN*I02)
     1          + CP*N*H1TEN*I03*(-0.5))+D22*SP*H13*I12)
     2          + D22*SP*SP*H13*H1TEN*I02
      KQX(4, 4) = KQX(4, 4) + H14*(D33*N*N*(CP*I13*(-1.0)+H14*I02)
     1          + D22*SP*SP*H14*I02)
      KQX(4, 5) = KQX(4, 5) + N*N*(D33*(H14*(+2.0*SP*I03+H15*I02)
     1          + CP*H15*I13*(-0.5)) + D22*SP*H14*I03)
     2          + D22*SP*SP*H14*H15*I02
C
C     THE FOLLOWING CODES, THRU 270, WERE MOVED HERE FROM KCONEZ
C
      KQX(4, 6) = KQX(4 ,6) + H14*(SP*(D22*(SP*I02*(-1.+H16)+N*N*I13)
     1          + D33*N*N*I13*2.0) + D33*N*N*I02*(-2.0+H16))
     2          + D33*CP*N*N*H16*I13*(-0.5)
      KQX(4, 7) = KQX(4, 7) + H14*(SP*(D22*(SP*(-2.0*I12+H17*I02)
     1          + N*N*I23)  - 2.0*D12*I01 + 2.0*D33*N**2*I23)
     2          + D33*N*N*(-4.0*I12+H17*I02))
     3          + D33*CP*N*N*H17*I13*(-0.5)
      KQX(4, 8) = KQX(4, 8) + H14*(SP*(D22*(SP*(-3.0*I22+H18*I02)
     1          + N*N*I33)  - 6.0*D12*I11 + 2.0*D33*N**2*I33)
     2          + D33*N*N*(-6.0*I22+H18*I02))
     3          + D33*CP*N*N*H18*I13*(-0.5)
      KQX(4, 9) = KQX(4, 9) + N*(D33*(N*H19*(-0.5*CP*I13+H14*I02)
     1          + SP*H14*I02)+D22*SP*H14*I02)+D22*SP*SP*H14*H19*I02
      KQX(4,10) = KQX(4,10) + N*(D33*(H14*(-I01+SP*I12+N*H1TEN*I02)
     1          + CP*N*H1TEN*I13*(-0.5)) + D22*SP*H14*I12)
     2          + D22*SP*SP*H14*H1TEN*I02
      KQX(5, 5) = KQX(5, 5) + H15*(SP*(N*N*I03*(D22*2.0+D33*4.0)
     1          + D22*SP*H15*I02) + D33*N*N*H15*I02)
      KQX(5, 6) = KQX(5, 6) + SP*(D22*(H15*(SP*I02*(-1.+H16)+N*N*I13)
     1          + N*N*H16*I03) + D33*N*N*(+2.0*H16*I03+2.0*H15*I13))
     2          + D33*N*N*H15*I02*(-2.0+H16)
      KQX(5, 7) = KQX(5, 7) + SP*(H15*(D22*(SP*(-2.0*I12+H17*I02)
     1          + N*N*I23)  - 2.0*D12*I01 + 2.0*D33*N**2*I23)
     2          + N*N*H17*I03*(+D22+2.0*D33))
     3          + D33*N*N*H15*(-4.0*I12+H17*I02)
      KQX(5, 8) = KQX(5, 8) + SP*(H15*(D22*(SP*(-3.0*I22+H18*I02)
     1          + N*N*I33) - 6.0*D12*I11 + 2.0*D33*N**2*I33)
     2          + N*N*H18*I03*(+D22+2.0*D33))
     3          + D33*N*N*H15*(-6.0*I22+H18*I02)
      KQX(5, 9) = KQX(5, 9) + SP*(N*(D22*(+N*H19*I03+H15*I02)
     1          + D33*(+2.0*N*H19*I03+H15*I02)) + D22*SP*H15*H19*I02)
     2          + D33*N*N*H15*H19*I02
      KQX(5,10) = KQX(5,10) + N*(D33*(H15*(-I01+SP*I12+N*H1TEN*I02)
     1          + SP*N*H1TEN*I03*2.) + D22*SP*(+N*H1TEN*I03+H15*I12))
     2          + D22*SP*SP*H15*H1TEN*I02
      KQX(6, 6) = KQX(6, 6) + H16*(SP*(D22*(SP*I02*(-2.0+H16)
     1          + N*N*I13*2.0) + D33*N*N*I13*4.0)
     2          + D33*N*N*I02*(-4.0+H16))
      KQX(6, 7) = KQX(6, 7) + SP*(D22*(SP*(H16*(-2.0*I12+H17*I02)
     1          + H17*I02*(-1.0)) + N*N*(+H17*I13+H16*I23))
     2          + D33*N*N*(+2.0*H17*I13 + 2.0*H16*I23)
     3          + D12*H16*I01*(-2.0))+D33*N*N*(H16*(-4.0*I12
     4          + H17*I02)  + H17*I02*(-2.0))
      KQX(6, 8) = KQX(6, 8) + SP*(D22*(SP*(H16*(-3.0*I22+H18*I02)
     1          + H18*I02*(-1.0)) + N*N*(+H18*I13+H16*I33))
     2          + D33*N*N*(+2.0*H18*I13 + 2.0*H16*I33)
     3          + D12*H16*I11*(-6.0)) + D33*N*N*(H16*(-6.0*I22
     4          + H18*I02)  + H18*I02*(-2.0))
      KQX(6, 9) = KQX(6, 9) + SP*(D22*(H19*(SP*I02*(-1.+H16)+N*N*I13)
     1          + N*H16*I02)+ D33*N*(+2.0*N*H19*I13+H16*I02))
     2          + D33*N*N*H19*I02*(-2.0+H16)
      KQX(6,10) = KQX(6,10) + N*(D33*(N*H1TEN*(I02*(-2.0+H16)
     1          + SP*I13*2.0) + H16*(-I01+SP*I12))
     2          + D22*SP*(+N*H1TEN*I13+H16*I12))
     3          + D22*SP*SP*H1TEN*I02*(-1.0+H16)
      KQX(7, 7) = KQX(7, 7) + H17*(SP*(D22*(SP*(I12*(-4.0)+H17*I02)
     1          + N*N*I23*2.0) + D12*I01*(-4.0)+D33*N*N*I23*4.0)
     2          + D33*N*N*(I12*(-8.0)+H17*I02))
      KQX(7, 8) = KQX(7, 8) + SP*(D22*(SP*(H17*(-3.0*I22+H18*I02)
     1          + H18*I12*(-2.0)) + N*N*(+H18*I23+H17*I33))
     2          + D12*(-6.0*H17*I11-2.0*H18*I01)
     3          + D33*N*N*(+2.0*H18*I23 + 2.0*H17*I33))
     4          + D33*N*N*(H17*(-6.0*I22+H18*I02) + H18*I12*(-4.0))
      KQX(7, 9) = KQX(7, 9) + SP*(H19*(D22*(SP*(+H17*I02-2.0*I12)
     1          + N*N*I23)  - 2.0*D12*I01 + 2.0*D33*N**2*I23)
     2          + N*H17*I02*(+D22+D33))+D33*N*N*H19*(-4.*I12+H17*I02)
      KQX(7,10) = KQX(7,10) + SP*(H1TEN*(D22*(SP*(+H17*I02-2.0*I12)
     1          + N*N*I23)  - 2.0*D12*I01 + 2.0*D33*N**2*I23)
     2          + N*H17*I12*(+D22+D33))+D33*N*(N*H1TEN*(-4.0*I12
     3          + H17*I02)  + H17*I01*(-1.0))
      KQX(8, 8) = KQX(8, 8) + H18*(SP*(D22*(SP*(I22*(-6.0)+H18*I02)
     1          + N*N*I33*2.0) + D12*I11*(-12.0)+D33*N*N*I33*4.0)
     2          + D33*N*N*(I22*(-12.0)+H18*I02))
      KQX(8, 9) = KQX(8, 9) + SP*(H19*(D22*(SP*(+H18*I02-3.0*I22)
     1          + N*N*I33)  - 6.0*D12*I11 + 2.0*D33*N**2*I33)
     2          + N*H18*I02*(+D22+D33))+D33*N*N*H19*(-6.*I22+H18*I02)
      KQX(8,10) = KQX(8,10) + SP*(H1TEN*(D22*(SP*(+H18*I02-3.0*I22)
     1          + N*N*I33)  - 6.0*D12*I11 + 2.0*D33*N**2*I33)
     2          + N*H18*I12*(+D22+D33)) + D33*N*(N*H1TEN*(-6.0*I22
     3          + H18*I02)  + H18*I01*(-1.0))
      KQX(9, 9) = KQX(9, 9) + H19*I02*(SP*(N*(D22*2.0+D33*2.0)
     1          + D22*SP*H19) + D33*N*N*H19)
      KQX(9,10) = KQX(9,10) + N*(D33*(H19*(-I01+SP*I12+N*H1TEN*I02)
     1          + SP*H1TEN*I02) + D22*SP*(+H1TEN*I02+H19*I12))
     2          + D22*SP*SP*H19*H1TEN*I02
      KQX(10,10)= KQX(10,10)+ H1TEN*(N*(D33*(SP*I12*2.0+I01*(-2.0)
     1          + N*H1TEN*I02) + D22*SP*I12*2.0)+D22*SP*SP*H1TEN*I02)
C
C     SET LOWER TRIANGLE EQUAL TO UPPER TRIANGLE OF KQN MATRIX
C
  270 DO 280 I = 1,10
      DO 280 J = I,10
  280 KQN(J,I) = KQN(I,J)
C
C     FILL HUQ PER PAGE 15 MS-28
C
      DO 290 I = 1,100
  290 HUQ(I) = 0.0
      HUQ(  1) = ONE
      HUQ( 13) = ONE
      HUQ( 25) = ONE
      HUQ( 36) = ONE
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
      HUQ( 87) = 2.0*SL
      HUQ( 88) = 3.0*HUQ(77)
      HUQ(100) = SL
C
      IF (TS) 300,320,300
  300 HUQ( 41) = CP/RA
      HUQ( 45) = N /RA
      HUQ( 91) = CP/RB
      HUQ( 92) = HUQ(91)*SL
      HUQ( 95) = N/RB
      HUQ( 96) = HUQ(95)*SL
      HUQ( 97) = HUQ(95)*L2
      HUQ( 98) = HUQ(96)*L2
      HUQ( 99) = ONE
C
C     SUBTRACT FROM ROWS 4 AND 9 OF THE ABOVE MATRIX, THE HYQ MATRIX
C
      DO 310 I = 1,10
      HUQ(I+30) = HUQ(I+30) - HYQ(I)
  310 HUQ(I+80) = HUQ(I+80) - HYQ(I)
  320 CONTINUE
C
C     NO NEED TO CALCULATE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY
C
      ISING =-1
      CALL INVERS (10,HUQ(1),10,DUM,0,DETERM,ISING,TEMP60(1))
C     CHECK SINGULARITY
C
      GO TO (340,330), ISING
  330 CALL MESAGE (30,40,NECPT(1))
C
C     SET FLAG FOR FATAL ERROR WHILE ALLOWING ERROR MESSAGES TO
C     ACCUMULATE
C
      NOGO = 1
      RETURN
C
C     NOT SINGULAR, CONTINUE ON..
C
  340 CONTINUE
      IF (TS .NE. 0.0) GO TO 345
      HUQ( 85) = 0.0
      HUQ(100) = 0.0
  345 CONTINUE
C
C                                 T    N       T
C     NOW SOLVE FOR (K  ) = (E)(H  )(K  )(H )(E )   I = PIVOT A OR B
C                     IJ         I    Q    J        J = A,B
C
C
C                             T    N        T  T
C     WE WILL SOLVE FOR (E)(H  )(K  )((E)(H  ))
C                            A    Q        B
C
C
C                            T                      T
C     FIRST GET EHAT = (E)(H  ),  AND  EHBT = (E)(H  )
C                           A                      B
C
C
C     EHAT WILL BE STORED AT H(1)...H(60) AND EHBT AT H(61)...H(120)
C
C                0    SP   CP   0    0
C                1    0    0    0    0
C                0    CP  -SP   0    0
C     MATRIX E = 0    0    0    0    SP
C                0    0    0    1    0
C                0    0    0    0    CP
C
      INC1 = 0
      INC2 = 0
  350 DO 360 I = 1,10
      IDX  = I + INC1
      ITEN = 10*I - 9 + INC2
      H(IDX   ) = HUQ(ITEN+1)*SP + HUQ(ITEN+2)*CP
      H(IDX+10) = HUQ(ITEN  )
      H(IDX+20) = HUQ(ITEN+1)*CP - HUQ(ITEN+2)*SP
      H(IDX+30) = HUQ(ITEN+4)*SP
      H(IDX+40) = HUQ(ITEN+3)
  360 H(IDX+50) = HUQ(ITEN+4)*CP
      IF (INC1) 380,370,380
  370 INC1 = 60
      INC2 = 5
      GO TO 350
  380 CONTINUE
C
C     DETERMINE PIVOT POINT NUMBER
C
      IF (NECPT(2) .EQ. NPVT) GO TO 390
      IF (NECPT(3) .EQ. NPVT) GO TO 400
      CALL MESAGE (-30,34,NECPT(1))
  390 NPIVOT = 1
      GO TO 410
  400 NPIVOT = 2
      GO TO 410
C
C     EHAT(1) IS AT H( 1)
C     EHBT(1) IS AT H(61)
C
  410 CALL GMMATS (H(60*NPIVOT-59),6,10,0, KQN(1,1),10,10,0, TEMP60(1))
C
C     IF N = 0 DOUBLE RESULT FOR KIJ
C
      IF (N) 440,420,440
  420 DO 430 I = 1,60
  430 TEMP60(I) = TEMP60(I)*2.0
C
  440 DO 470 J = 1,2
      CALL GMMATS (TEMP60(1),6,10,0, H(60*J-59),6,10,1, KIJ(1))
      DO 445 I = 1,36
  445 KIJD(I) = KIJ(I)
      CALL SMA1B (KIJD(1),NECPT(J+1),-1,IFKGG,0.0D0)
      IF (IOPT4) 450,470,450
  450 IF (GSUBE) 460,470,460
  460 SUM = GSUBE
      K4GGSW = 1
      CALL SMA1B (KIJD(1),NECPT(J+1),-1,IF4GG,SUM)
  470 CONTINUE
C
      RETURN
      END
