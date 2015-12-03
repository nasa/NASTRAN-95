      SUBROUTINE TKER (X0,Y0,Z0,KR,BR,SGR,CGR,SGS,CGS,T1,T2,M)
C
C     COMPUTES EITHER THE TOTAL KERNELS (IND=0) USED IN THE CALCULATION
C     OF A FINITE LENGTH DOUBLET LINE,  OR
C     THE INCREMENTAL OSCILLATORY KERNELS (IND=1) USED IN EVALUATING
C     THE INFLUENCE COEFFICIENT MATRIX ELEMENTS
C
      REAL         M,KR,I00R,I00I,J00R,J00I,I10R,I10I,I20R3,I20I3,I0UR,
     1             I0UI,J0UR,J0UI,I1UR,I1UI,I2UR3,I2UI3,K1,MU1,MU,K2,
     2             K10,K20,K1RT1,K1IT1,K2RT2P,K2IT2P,K10T1,K20T2P,KD1R,
     3             KD1I, KD2R,KD2I
      COMMON /DLM/ K10,K20,K1RT1,K1IT1,K2RT2P,K2IT2P,K10T1,K20T2P
      COMMON /KDS/ IND,KD1R,KD1I,KD2R,KD2I
C
      EPS    = 0.00001
      K10    = 0.0
      K20    = 0.0
      K1RT1  = 0.0
      K1IT1  = 0.0
      K2RT2P = 0.0
      K2IT2P = 0.0
      K10T1  = 0.0
      K20T2P = 0.0
      R1     = SQRT(Y0*Y0 + Z0*Z0)
      R1S    = R1
      IF (ABS(R1) .GT. EPS) GO TO 200
      IF (X0) 905,120,120
  120 C1     = KR*X0/BR
      T1     = CGR*CGS + SGR*SGS
      K10    = 2.0
      K1RT1  = 2.0*T1*COS(C1)
      K1IT1  =-2.0*T1*SIN(C1)
      K10T1  = 2.0*T1
      GO TO  905
  200 C1     = CGR
      C2     = SGR
      C3     = CGS
      C4     = SGS
      T2P    = (Z0*Z0*C1*C3 + Y0*Y0*C2*C4 - Z0*Y0*(C2*C3+C1*C4))
      T2     = (100.*T2P)/(BR*BR)
      IF (ABS(T2)-EPS) 210,220,220
  210 ICHUZ  = 1
      T1     = CGR*CGS + SGR*SGS
      T2     = 0.0
      GO TO 300
  220 T1     = CGR*CGS + SGR*SGS
      IF (ABS(T1)-EPS) 230,240,240
  230 ICHUZ  = 2
      T1     = 0.
      GO TO 300
  240 ICHUZ  = 3
  300 BETA2  = (1.-M*M)
      BIGR   = SQRT(X0*X0 + BETA2*R1*R1)
      K1     = KR*R1/BR
      MU1    = (M*BIGR-X0)/(BETA2*R1)
      MU     = ABS(MU1)
      K2     = K1*K1
      IF (MU1) 310,320,330
  310 ICHUZ  = ICHUZ + 3
      GO TO 330
  320 ICHUZ  = ICHUZ + 6
C
C     (N*C)**2  FOR  N = 1,11  AND C = .372 =
C
C       .138384      .553536     1.245456      2.214144
C      3.4596       4.981824     6.780816      8.856576
C     11.209104    13.8384      16.744464
C
C     (N*C)  FOR  N = 1,12  AND  14,16,18,20,22   =
C
C       .744        1.116        1.488         1.86      2.232
C      2.604        2.976        3.348         3.72      4.092
C      4.464        5.208        5.952         6.696     7.44
C      8.184
C
C     A(N)  FOR N = 1,11  =
C
C      .24186198   -2.7918027    24.991079    -111.59196
C      271.43549   -305.75288    -41.18363     545.98537
C     -644.78155    328.72755    -64.279511
C
  330 CONTINUE
      EXARG = -0.372*MU
C
C     THE FOLLOWING TEST ON THE SIZE OF THE ARGUMENT TO  EXP  IS
C     NEEDED TO AVOID UNDERFLOW IN  SUBPROGRAM  EXP
C
      IF (EXARG .GE. -180.0) GO TO 335
      E   = 0.0
      GO TO 337
  335 E   = EXP(EXARG)
  337 CONTINUE
      C1  =  0.138384 + K2
      C2  =  0.553536 + K2
      C3  =  1.245456 + K2
      C4  =  2.214144 + K2
      C5  =  3.4596   + K2
      C6  =  4.981824 + K2
      C7  =  6.780816 + K2
      C8  =  8.856576 + K2
      C9  = 11.209104 + K2
      C10 = 13.8384   + K2
      C11 = 16.744464 + K2
      R1  = .24186198 / C1
      R2  =-2.7918027 / C2
      R3  = 24.991079 / C3
      R4  =-111.59196 / C4
      R5  = 271.43549 / C5
      R6  =-305.75288 / C6
      R7  =-41.18363  / C7
      R8  = 545.98537 / C8
      R9  =-644.78155 / C9
      R10 = 328.72755 / C10
      R11 =-64.279511 / C11
      IF (ICHUZ .LT. 4) GO TO 340
      I00R = .372*(R1 + 2.*R2 + 3.*R3 + 4.*R4 + 5.*R5 + 6.*R6 + 7.*R7 +
     1        8.*R8 + 9.*R9 + 10.*R10 + 11.*R11)
      I00I =-K1*(R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8 + R9 + R10 + R11)
  340 GO TO (420,350,350,390,350,350,380,350,350), ICHUZ
  350 Q1  = R 1/ C1
      Q2  = R 2/ C2
      Q3  = R 3/ C3
      Q4  = R 4/ C4
      Q5  = R 5/ C5
      Q6  = R 6/ C6
      Q7  = R 7/ C7
      Q8  = R 8/ C8
      Q9  = R 9/ C9
      Q10 = R10/C10
      Q11 = R11/C11
      GO TO (420,410,410,390,360,360,380,360,360), ICHUZ
  360 J00R  = Q1*(.138384-K2)+Q2*(.553536-K2)+Q3*(1.245456-K2)+Q4*
     1        (2.214144-K2)+Q5*(3.4596-K2)+Q6*(4.981824-K2)+Q7*(6.780816
     2        -K2)+Q8*(8.856576-K2)+Q9*(11.209104-K2)+Q10*(13.8384-K2)+
     3        Q11*(16.744464-K2)
      I20R3 = 2.+K1*I00I+K2*J00R
      GO TO  (420,410,410,390,410,390,380,370,370),ICHUZ
  370 J00I  = -K1*(.744*Q1+1.488*Q2+2.232*Q3+2.976*Q4+3.72*Q5+4.464*Q6+
     1        5.208*Q7+5.952*Q8+6.696*Q9+7.44*Q10+8.184*Q11)
      I20I3 = -K1*I00R+K2*J00I
      IF (ICHUZ .EQ. 8) GO TO 500
  380 I10I = -K1*I00R
  390 I10R = 1.+ K1*I00I
      GO TO (420,410,410,420,410,410,500,500,500), ICHUZ
  410 J0UR = E*(Q1*(0.138384 - K2 + 0.372*MU*C1) +
     1       E*(Q2*(0.553536 - K2 + 0.744*MU*C2) +
     2       E*(Q3*(1.245456 - K2 + 1.116*MU*C3) +
     3       E*(Q4*(2.214144 - K2 + 1.488*MU*C4) +
     4       E*(Q5*(3.4596   - K2 + 1.860*MU*C5) +
     5       E*(Q6*(4.981824 - K2 + 2.232*MU*C6) +
     6       E*(Q7*(6.780816 - K2 + 2.604*MU*C7) +
     7       E*(Q8*(8.856576 - K2 + 2.976*MU*C8) +
     8       E*(Q9*(11.209104- K2 + 3.348*MU*C9) +
     9       E*(Q10*(13.8384 - K2 + 3.72*MU*C10) +
     O       E*(Q11*(16.744464-K2 + 4.092*MU*C11))))))))))))
      J0UI = -K1*(E*(Q1*(0.744 + MU*C1) + E*(Q2*(1.488 + MU*C2) +
     1            E*(Q3*(2.232 + MU*C3) + E*(Q4*(2.976 + MU*C4) +
     2            E*(Q5*(3.720 + MU*C5) + E*(Q6*(4.464 + MU*C6) +
     3            E*(Q7*(5.208 + MU*C7) + E*(Q8*(5.952 + MU*C8) +
     4            E*(Q9*(6.696 + MU*C9) + E*(Q10*(7.44 + MU*C10)+
     5            E*(Q11*(8.184+ MU*C11)))))))))))))
  420 I0UR = .372*E*(R1+E*(2.*R2+E*(3.*R3+E*(4.*R4+E*(5.*R5+E*(6.*R6+
     1            E*(7.*R7+E*(8.*R8+E*(9.*R9+E*(10.*R10+E*11.*R11))))))
     2            ))))
      I0UI = -K1*(E*(R1+E*(R2+E*(R3+E*(R4+E*(R5+E*(R6+E*(R7+E*(R8+E*(R9
     1           +E*(R10+E*R11)))))))))))
      R1   = R1S
      C6   = K1*MU
      C1   = SIN(C6)
      C2   = COS(C6)
      C3   = SQRT(1.+MU*MU)
      C4   = MU/C3
      C5   = C4/(1.+MU*MU)
      GO TO (430,440,430,430,440,430,500,500,500), ICHUZ
  430 I1UR = C2*(1.-C4+K1*I0UI) - C1*K1*I0UR
      I1UI =-C2*K1*I0UR - C1*(1.-C4+K1*I0UI)
      GO TO (500,440,440,460,440,440,500,500,500), ICHUZ
  440 I2UR3 = C2*(2.*(1.-C4)-C5+K1*I0UI+K2*J0UR)+C1*(C6*(1.-C4)-K1*I0UR
     1        + K2*J0UI)
      I2UI3 = C2*(C6*(1.-C4)-K1*I0UR+K2*J0UI)-C1*(2.*(1.-C4)-C5+K1*I0UI
     1        + K2*J0UR)
      GO TO (500,500,500,460,450,450,500,500,500), ICHUZ
  450 I2UR3 = 2.0*I20R3 - I2UR3
      IF (ICHUZ-6) 500,460,500
  460 CAR  = 2.*I10R - I1UR
      I1UR = CAR
  500 DK1R = 0.
      R1   = R1S
      DK1I = 0.
      DK2R = 0.
      DK2I = 0.
      C3   = K1*MU1
      C1   = COS(C3)
      C2   = SIN(C3)
      C3   = M*R1/BIGR
      C4   = SQRT(1.+MU1*MU1)
      C5   = KR*X0/BR
      C6   = COS(C5)
      C7   = SIN(C5)
      GO TO (530,540,530,530,540,530,510,520,510), ICHUZ
  510 I1UR = I10R
      I1UI = I10I
      IF (ICHUZ-7) 520,530,520
  520 I2UR3 = I20R3
      I2UI3 = I20I3
      IF (ICHUZ-8) 530,540,530
  530 CK1R = I1UR + C3*C1/C4
      CK1I = I1UI - C3*C2/C4
      K10  = 1.0 + X0/BIGR
      DK1R = CK1R*C6 + CK1I*C7
      DK1I = CK1I*C6 - CK1R*C7
      GO TO (900,540,540,900,540,540,900,540,540), ICHUZ
  540 C8   = (BETA2*(R1/BIGR)**2 + (2.+MU1*C3)/(C4*C4))*(-C3/C4)
      C9   = (K1*C3)*( C3/C4)
      CK2R = -I2UR3 + C8*C1 - C9*C2
      CK2I = -I2UI3 - C9*C1 - C8*C2
      K20  = -2.0 - X0*(2.0+BETA2*(R1/BIGR)**2)/BIGR
      DK2R = CK2R*C6 + CK2I*C7
      DK2I = CK2I*C6 - CK2R*C7
  900 CONTINUE
      K1RT1  = T1 *DK1R
      K1IT1  = T1 *DK1I
      K2RT2P = T2P*DK2R
      K2IT2P = T2P*DK2I
      K10T1  = K10*T1
      K20T2P = K20*T2P
  905 CONTINUE
      KD1R = K1RT1  - K10T1*FLOAT(IND)
      KD1I = K1IT1
      KD2R = K2RT2P - K20T2P*FLOAT(IND)
      KD2I = K2IT2P
      RETURN
      END
