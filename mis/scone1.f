      SUBROUTINE SCONE1
C     ******* PHASE I OF STRESS DATA RECOVERY FOR CONICAL SHELL*********
C     OUTPUTS FROM THIS ROUTINE FOR USE IN PHASE II ARE...
C     1) ELEMENT ID
C     2 AND 3) SILS A AND B
C     4) S SUB T
C     5) N
C     6) I
C     7) Z1
C     8) Z2
C     9 THRU 22) PHI-S
C     23 THRU 118) TWO 8X6 S MATRICES
C     TOTAL OF 118 WORDS
C***********************************************************************
C     ECPT( 1) = ELEMENT ID             INTEGER        ECT
C     ECPT( 2) = SIL PT A               INTEGER        ECT
C     ECPT( 3) = SIL PT B B             INTEGER        ECT
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
C***********************************************************************
      REAL III
      REAL NSPRSQ
      REAL NCPRSQ
      REAL N2RSQ
      REAL T30(30)
      REAL G(9)
      REAL NSPOPI
      REAL INTEG(28)
      REAL FAC(7)
      REAL N
      REAL L2
      REAL NSP
      REAL NCP
      REAL N2
      REAL KS
      REAL NOVR
      REAL N2D33
      REAL HYQ(20)
C
      REAL               I00 ,I01 ,I02 ,I03 ,I04
      REAL               I10 ,I11 ,I12 ,I13 ,I14
      REAL               I20 ,I21 ,I22 ,I23 ,I24
      REAL                    I31 ,I32 ,I33 ,I34
      REAL                         I42 ,I43 ,I44
      REAL                         I52 ,I53 ,I54
      REAL                         I62 ,I63 ,I64
C
      INTEGER NECPT(100)
      INTEGER NERROR(2)
      INTEGER NA(7)
C
      COMMON /CONDAS/    PI       ,TWOPI    ,RADEG    ,DEGRA    ,
     1                   S4PISQ
C
      COMMON /MATIN / MATID, INFLAG, ELTEMP, STRESS, SINTH, COSTH
C
      COMMON /MATOUT/ G11, G12, G13, G22, G23, G33, ALPHA
C
      COMMON /SDR2X5/    ECPT(100)     ,PH1OUT(118)
C
      COMMON /SDR2X6/    HUQ(100)      ,H(120)        ,KS(80)
C
      EQUIVALENCE   ( ECPT(1), NECPT(1))
      EQUIVALENCE   ( ECPT(4), MATID1  )
      EQUIVALENCE   ( G(1)    ,HUQ(1)  )
      EQUIVALENCE   ( ECPT(5) ,T       )
      EQUIVALENCE   ( ECPT(6), MATID2  )
      EQUIVALENCE   ( ECPT(7) ,III     )
      EQUIVALENCE   ( ECPT(8), MATID3  )
      EQUIVALENCE   ( ECPT(9) ,TS      )
      EQUIVALENCE   ( ECPT(11),Z1      )
      EQUIVALENCE   ( ECPT(12),Z2      )
      EQUIVALENCE   ( ECPT(28),RA      )
      EQUIVALENCE   ( ECPT(29),ZA      )
      EQUIVALENCE   ( ECPT(32),RB      )
      EQUIVALENCE   ( ECPT(33),ZB      )
      EQUIVALENCE   ( D11     ,G(1)    )
      EQUIVALENCE   ( D12     ,G(2)    )
      EQUIVALENCE   ( D22     ,G(5)    )
      EQUIVALENCE   ( D33     ,G(9)    )
      EQUIVALENCE   ( INTEG(1),HUQ(1)  )
      EQUIVALENCE   ( T30(1)  ,H(1)    )
      EQUIVALENCE   ( HYQ(1)  ,H(31)   )
      EQUIVALENCE   ( HYQ( 1), H11     )
      EQUIVALENCE   ( HYQ( 2), H12     )
      EQUIVALENCE   ( HYQ( 3), H13     )
      EQUIVALENCE   ( HYQ( 4), H14     )
      EQUIVALENCE   ( HYQ( 5), H15     )
      EQUIVALENCE   ( HYQ( 6), H16     )
      EQUIVALENCE   ( HYQ( 7), H17     )
      EQUIVALENCE   ( HYQ( 8), H18     )
      EQUIVALENCE   ( HYQ( 9), H19     )
      EQUIVALENCE   ( HYQ(10), H1TEN   )
      EQUIVALENCE
     1                   (I00 , INTEG( 1))  ,(I20 , INTEG(11))
     2                  ,(I01 , INTEG( 2))  ,(I21 , INTEG(12))
     3                  ,(I02 , INTEG( 3))  ,(I22 , INTEG(13))
     4                  ,(I03 , INTEG( 4))  ,(I23 , INTEG(14))
     5                  ,(I04 , INTEG( 5))  ,(I24 , INTEG(15))
     6                  ,(I10 , INTEG( 6))  ,(I31 , INTEG(16))
     7                  ,(I11 , INTEG( 7))  ,(I32 , INTEG(17))
     8                  ,(I12 , INTEG( 8))  ,(I33 , INTEG(18))
     9                  ,(I13 , INTEG( 9))  ,(I34 , INTEG(19))
     T                  ,(I14 , INTEG(10))  ,(I52 , INTEG(23))
     1                  ,(I42 , INTEG(20))  ,(I53 , INTEG(24))
     2                  ,(I43 , INTEG(21))  ,(I54 , INTEG(25))
     3                  ,(I44 , INTEG(22))  ,(I62 , INTEG(26))
     4                                      ,(I63 , INTEG(27))
     5                                      ,(I64 , INTEG(28))
C
      DATA FAC/1.0E0,1.0E0,2.0E0,6.0E0,24.0E0,120.0E0,720.0E0/
      DATA NA /1,1,1,2,3,3,3/
      DATA ONE/1.0E0/
C
      COSTH=1.0
      SINTH=0.0
      N = NECPT(1) - ( (NECPT(1)/1000)*1000 ) - 1
      TEMP1 = RB-RA
      TEMP2 = ZB-ZA
      SL =  SQRT(TEMP1**2 + TEMP2**2)
      L2 = SL * SL
      IF(SL) 30,20,30
   20 NERROR(1) = NECPT(1) / 1000
      NERROR(2) = N + .3E0
      CALL MESAGE(-30, 39, NERROR(1) )
   30 SP = TEMP1 / SL
      CP = TEMP2 / SL
      NSP = N * SP
      NCP = N * CP
      N2 = N * N
      SP2 = SP * SP
      A=RA
      B=SP
      IF( B ) 60,40,60
C
C     GO TO 302 FOR B = 0
C
C                            1-N
C                      PI  RA     M+1
C     FOR B = 0 I   = --------- SL    (FOR ALL M,N .GE. 0)
C                M,N    M + 1
C
   40 ISUB = 0
      DO 50 I=1,7
      NBEGIN = NA(I)
C
C
      DO 50 J=NBEGIN,5
C
C     M = I - 1
C     N = J - 1
C     MPLUS1 THUS EQUALS I
      ISUB = ISUB + 1
   50 INTEG(ISUB) = (PI * SL**I) / ( FLOAT(I) * RA**(J-2))
C
C     ABOVE COMPLETES ALL INTEGRALS FOR B = 0...
C
C     IF AN OVERFLOW RESULTS BELOW POSSIBLY B IS NOT ZERO, BUT SMALL..
C
      GO TO 100
C
C     OK BELOW IS FOR B NOT EQUAL TO ZERO
C
C     FIRST M = 0 CASE...
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
   60 RASQ = RA * RA
      RBSQ = RB * RB
      PIOVB = PI / B
C
      INTEG(1) = 0.5E0 * PIOVB * (RBSQ - RASQ)
      INTEG(2) = PIOVB * (RB - RA)
      INTEG(3) = PIOVB * ALOG(RB/RA)
      INTEG(4) = -PIOVB * (ONE/RB - ONE/RA)
      INTEG(5) = -0.5E0 * PIOVB * (ONE/RBSQ - ONE/RASQ)
C
      ISUB = 5
      DO 90 I=1,6
      MPLUS1 = I + 1
      NBEGIN = NA(MPLUS1)
      DO 90 J=NBEGIN,5
      ISUB = ISUB + 1
C
C     M = I
C     N = J - 1
C
C     WE ARE GETTING INTEGRAL(M,N)
C     M = POWER OF S
C     N = POWER OF R
C
C
C     EVALUATING AT R = RB  THEN AT R = RA...
C
C                                    K   NPOW
C                 M FAC.     M   (-A) (R)
C I    = (PI)(-----------)( SUM ------------------------) + (TERM-X)
C  MN               (M+1)   K=0  (M-K)FAC.(K)FAC.(NPOW)
C                  B        (K.NE.M-N+2)                    (K.EQ.M-N+2)
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
C     NOTE IN DATA STATEMENT THAT 0 FACTORIAL = FAC(1)
C                                 1 FACTORIAL = FAC(2)
C                                 2 FACTORIAL = FAC(3)    ETC...
C
      SUM = 0.0E0
      SIGN = -1.0E0
      DO 80 KK=1,MPLUS1
      SIGN = -SIGN
      K = KK - 1
      NPOW = I - J + 3
      IF(K .EQ. NPOW ) GO TO 70
      NPOW = NPOW - K
      IFAC = MPLUS1 - K
      TEMP = NPOW
      SUM=SUM+SIGN*A**K*(RB**NPOW - RA**NPOW)/(FAC(IFAC)*FAC(K+1)*TEMP)
      GO TO 80
   70 SUM = SUM+SIGN*A**NPOW*ALOG(RB/RA) / (FAC(NPOW+1)*FAC(J-2))
   80 CONTINUE
C
      INTEG(ISUB) = SUM * PI * FAC(MPLUS1) / B**MPLUS1
   90 CONTINUE
  100 CONTINUE
C
C
      DO 120 I = 1,80
  120 KS(I) = 0.0E0
C
      R = 0.50E0 * ( RA + RB )
      S = 0.50E0 * SL
C
      IF( T ) 130,170,130
  130 VAR=1.0
      MATID = MATID1
      ASSIGN 150 TO ICONT
C
  140 ELTEMP = ECPT(35)
      INFLAG = 2
      CALL MAT( ECPT(1) )
      G(1) = G11 * VAR
      G(2) = G12 * VAR
      G(3) = G13 * VAR
      G(4) = G12 * VAR
      G(5) = G22 * VAR
      G(6) = G23 * VAR
      G(7) = G13 * VAR
      G(8) = G23 * VAR
      G(9) = G33 * VAR
C
      GO TO ICONT,(150,195)
C
  150 DO 160 I = 1,30
  160 T30(I) = 0.0E0
C
      T30( 4) = 1.0E0
      T30(11) = N / R
      T30(12) = T30(11) * S
      T30(13) =   SP / R
      T30(14) = S * T30(13)
      T30(15) = CP / R
      T30(16) = S * T30(15)
      T30(17) = S * T30(16)
      T30(18) = S * T30(17)
      T30(21) = - T30(13)
      T30(22) = 1.0E0 - T30(14)
      T30(23) = - T30(11)
      T30(24) = - T30(12)
C
      CALL GMMATS( G(1),3,3,0,  T30(1),3,10,0,  KS(1) )
C
  170 IF( III ) 190,180,190
  180 DO 181 I = 1,9
  181 G(I) = 0.0E0
      GO TO 195
C
C     GET G MATERIAL MATRIX FOR MATERIAL ID 2 AND MULTIPLY BY I...
C     THIS THEN IS THE D 3X3 MATRIX BY EQUIVALENCE...
C
  190 VAR = III
      MATID = MATID2
      ASSIGN 195 TO ICONT
      GO TO 140
C
C     FORMING 1.0/Q DIRECTLY
C
  195 OPI = ONE / PI
      DO 299 I = 1,20
  299 HYQ(I) = 0.0E0
      IF( TS ) 351,352,351
C
  351 ELTEMP = ECPT(35)
      INFLAG = 1
      MATID = MATID3
      CALL MAT( NECPT(1) )
C
      IF(G12.EQ.0.0) GO TO 354
      N2D33 = N2 * D33
      SP2D22 = SP2 * D22
      OQ = SL * TS *  G12    * (RA+RB)*0.5E0 + I02 * (N2D33+SP2D22)*OPI
      OQ = ONE / OQ
      NSPOPI = NSP * OPI
      TWOD33 = 2.0E0 * D33
      TEMP1 = D12 * (ONE/RB - ONE/RA)
      TEMP2 = NSPOPI * (D22 + D33)
      TEMP3 = N * NSPOPI * (TWOD33 + D22)
      TEMP4 = OQ * 0.5E0 * NCP * N * D33 * OPI
      TEMP5 = OPI * (N2 * TWOD33  + SP2 * D22)
      TEMP6 = D12 * N2 * L2 / RB
      TEMP7 = NSPOPI * CP * 0.50E0
C
      HYQ( 1) = OQ*(TEMP1*NCP - TEMP7*I03*(D33+2.0E0*D22))
      HYQ( 2) = OQ*(NCP*SL/RB*D12-TEMP7*I13*(3.0E0*D33+D22)+
     1   1.0E0*NCP*OPI*I02*D33)
      HYQ( 3) = TEMP4 * I03
      HYQ( 4) = TEMP4 * I13
      HYQ( 5) = OQ * (TEMP1*N2  -  TEMP3*I03)
      HYQ( 6) = OQ * (D12*N2*SL/RB - TEMP3*I13 + TEMP5*I02)
      HYQ( 7) = OQ*(2.0E0*D11*(RA-RB)+TEMP6+2.0E0*I12*TEMP5-TEMP3*I23)
      HYQ( 8) =OQ*(-D11*6.E0*SL*RB+TEMP6*SL+3.E0*I22*TEMP5-TEMP3*I33)
      HYQ( 9) = -OQ * TEMP2 * I02
      HYQ(10) = OQ * (N*SL*(D12 + D33) - TEMP2*I12)
      HYQ(19) = 1.0E0
      HYQ(20) = S
C
      TSG3 = TS * G12
      DO 359 I = 1,20
  359 KS(I+60) = HYQ(I) * TSG3
C     FILL HXQ MATIX
C
      GO TO 352
  354 TS=0.0
  352 IF( III ) 500,400,500
  500 S2 = S * S
      S3 = S * S2
      RSQ = R * R
      SPOVR = SP / R
      NCPRSQ = NCP/RSQ
      NSPRSQ = NSP/RSQ
      N2RSQ = N2 / RSQ
      SPCPR2 = SP * CP / RSQ
      NOVR = N / R
      T30( 7) = 2.0E0
      T30( 8) = 6.0E0 * S
      T30(11) = - NCPRSQ - SPOVR * H11
      T30(12) = - S * NCPRSQ - SPOVR * H12
      T30(13) = - SPOVR * H13
      T30(14) = - SPOVR * H14
      T30(15) = - N2RSQ - SPOVR * H15
      T30(16) = SPOVR - N2RSQ * S - SPOVR * H16
      T30(17) = 2.0E0 * S * SPOVR - N2RSQ * S2 - SPOVR * H17
      T30(18) = 3.0E0 * S2 * SPOVR - N2RSQ * S3 - SPOVR * H18
      T30(19) = - NOVR - SPOVR * H19
      T30(20) = - NOVR * S - SPOVR * H1TEN
      T30(21) = 0.5E0 * SPCPR2 + NOVR * H11
      T30(22) = 0.5E0 * ( S * SPCPR2 - 3.0E0 * CP / R ) + NOVR * H12
      T30(23) = -0.50E0 * NCPRSQ + NOVR * H13
      T30(24) = -NCPRSQ * S * 0.50E0 * NOVR * H14
      T30(25) = NSPRSQ + NOVR * H15
      T30(26) = NSPRSQ * S - NOVR * ( 2.0E0 - H16 )
      T30(27) = NSPRSQ * S2 - NOVR * ( 4.0E0 * S - H17 )
      T30(28) = NSPRSQ * S3 - NOVR * ( 6.0E0 * S2 - H18 )
      T30(29) = SPOVR + NOVR * H19
      T30(30) = -1.0E0 + SPOVR * S + NOVR * H1TEN
C
      CALL GMMATS( G(1),3,3,0,  T30(1),3,10,0,  KS(31) )
C
C
C
C     FILL HUQ PER PAGE 15 MS-28
C
  400 DO 290 I=1,100
  290 HUQ(I) = 0.0E0
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
      HUQ( 78) = HUQ(77) * SL
      HUQ( 86) = ONE
      HUQ( 87) = 2.0E0 * SL
      HUQ( 88) = 3.0E0 * HUQ(77)
      HUQ(100) = SL
C
      IF( TS ) 300,320,300
C
  300 HUQ( 41)=CP/RA
      HUQ( 45)=N/RA
      HUQ( 91) = CP / RB
      HUQ( 92) = HUQ(91) * SL
      HUQ( 95) = N / RB
      HUQ( 96) = HUQ(95) * SL
      HUQ( 97) = HUQ(95) * L2
      HUQ( 98) = HUQ(96)*L2
      HUQ( 99) = ONE
      HUQ(100) = SL
C
C     SUBTRACT FROM ROWS 4 AND 9 OF THE ABOVE MATRIX, THE HYQ MATRIX...
C
      DO 310 I=1,10
      HUQ(I+30) = HUQ(I+30) - HYQ(I)
  310 HUQ(I+80) = HUQ(I+80) - HYQ(I)
C
  320 CONTINUE
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      ISING = -1
      CALL INVERS( 10, HUQ(1), 10, DUM, 0, DETERM, ISING, T30(1) )
C
C     CHECK SINGULARITY
      GO TO(340,330),ISING
  330 CALL MESAGE( -30, 40, NECPT(1) )
C
C
C     NOT SINGULAR, CONTINUE ON..
  340 CONTINUE
      IF(TS.NE.0.0) GO TO 345
      HUQ(85)=0.0
      HUQ(100)=0.0
  345 CONTINUE
C                            T                      T
C           GET EHAT = (E)(H  ),  AND  EHBT = (E)(H  )
C                           A                      B
C     EHAT WILL BE STORED AT H(1)...H(60) AND EHBT AT H(61)...H(120)
C
C
C              0    SP   CP   0    0
C
C              1    0    0    0    0
C
C              0    CP  -SP   0    0
C   MATRIX E =
C              0    0    0    0    SP
C
C              0    0    0    1    0
C
C              0    0    0    0    CP
      INC1 = 0
      INC2 = 0
  350 DO 360 I=1,10
      ISUB = I + INC1
      ITEN = 10*I - 9 + INC2
      H(ISUB   ) = HUQ(ITEN+1) * SP  +  HUQ(ITEN+2) * CP
      H(ISUB+10) = HUQ(ITEN  )
      H(ISUB+20) = HUQ(ITEN+1) * CP  -  HUQ(ITEN+2) * SP
      H(ISUB+30) = HUQ(ITEN+4) * SP
      H(ISUB+40) = HUQ(ITEN+3)
  360 H(ISUB+50) = HUQ(ITEN+4) * CP
      IF( INC1 ) 380,370,380
  370 INC1 = 60
      INC2 = 5
      GO TO 350
  380 CONTINUE
C
      DO 700 I = 1,2
      CALL GMMATS( KS(1),8,10,0,  H(60*I-59),6,10,1,  PH1OUT(48*I-25) )
  700 CONTINUE
      SSUBT = 0.0E0
      IF( MATID1 ) 800,850,800
C     COMPUTE S SUB T
C
  800 INFLAG = 1
      MATID = MATID1
      ELTEMP = ECPT(35)
      CALL MAT( ECPT(1) )
      SSUBT = G11 * PI * ALPHA / (1.0E0 - G13)
      IF( N .EQ. 0.0E0 ) SSUBT = 2.0E0  * SSUBT
C
  850 PH1OUT(1) = ECPT(1)
      PH1OUT(2) = ECPT(2)
      PH1OUT(3) = ECPT(3)
      PH1OUT(4) = SSUBT
      PH1OUT(5) = N
      PH1OUT(6) = III
      PH1OUT(7) = Z1
      PH1OUT(8) = Z2
      DO 900 I = 9,22
  900 PH1OUT(I) = ECPT(I+4)
C
      RETURN
      END
