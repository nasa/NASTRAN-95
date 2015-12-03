      SUBROUTINE SUBBB
C
      COMPLEX         SBKDE1,SBKDE2,F4,F4S,AM4,F5S,F6S,AM4TST,SUM3,SUM4,
     1                AM5TT,AM6,SUMSV1,SUMSV2,SVKL1,SVKL2,F5,F5T,AM5,
     2                AM5T,AI,A,B,BSYCON,ALP,F1,AM1,ALN,BLKAPM,BKDEL3,
     3                F1S,C1,C2P,C2N,C2,AMTEST,FT2,BLAM1,FT3,AM2,SUM1,
     4                SUM2,F2,BLAM2,FT2T,C1T,FT3T,F2P,AM2P,SUM1T,SUM2T,
     5                GUSAMP,C1P,C1N,BKDEL1,BKDEL2,BLKAP1,ARG,ARG2,
     6                FT3TST,BC,BC2,BC3,BC4,BC5,CA1,CA2,CA3,CA4,CLIFT,
     7                CMOMT,PRES1,PRES2,PRES3,PRES4,QRES4,CEXP4C,FQA,
     8                FQB,T1,T2,T3,T4,CEXP2A,CEXP2B,CEXP2C,CEXP4A,
     9                CEXP4B,FQ7,C1A,C3A,C4A,CONST,CEXP3,CEXP4,CEXP3A,
     O                CEXP3B,CEXP3C
      DIMENSION       PRES1(21),PRES2(21),PRES3(21),PRES4(21),QRES4(21),
     1                SBKDE1(201),SBKDE2(201),SUMSV1(201),SUMSV2(201),
     2                SVKL1(201),SVKL2(201),XLSV1(21),XLSV2(21),
     3                XLSV3(21),XLSV4(21)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ SYSBUF,IBBOUT
      COMMON /BLK1  / SCRK,SPS,SNS,DSTR,AI,PI,DEL,SIGMA,BETA,RES
      COMMON /BLK2  / BSYCON
      COMMON /BLK3  / SBKDE1,SBKDE2,F4,F4S,AM4,F5S,F6S,AM4TST,SUM3,SUM4,
     1                AM5TT,AM6,SUMSV1,SUMSV2,SVKL1,SVKL2,F5,F5T,AM5,
     2                AM5T,A,B,ALP,F1,AM1,ALN,BLKAPM,BKDEL3,F1S,C1,C2P,
     3                C2N,C2,AMTEST,FT2,BLAM1,FT3,AM2,SUM1,SUM2,F2,
     4                BLAM2,FT2T,C1T,FT3T,F2P,AM2P,SUM1T,SUM2T,C1P,C1N,
     5                BKDEL1,BKDEL2,BLKAP1,ARG,ARG2,FT3TST,BC,BC2,BC3,
     6                BC4,BC5,CA1,CA2,CA3,CA4,CLIFT,CMOMT,PRES1,PRES2,
     7                PRES3,PRES4,QRES4,FQA,FQB,FQ7
      COMMON /BLK4  / I,R,Y,A1,B1,C4,C5,GL,I6,I7,JL,NL,RI,RT,R5,SN,SP,
     1                XL,Y1,AMU,GAM,IDX,INX,NL2,RL1,RL2,RQ1,RQ2,XL1,
     2                ALP1,ALP2,GAMN,GAMP,INER,IOUT,REDF,STAG,STEP,
     3                AMACH,BETNN,BETNP,BKAP1,XLSV1,XLSV2,XLSV3,XLSV4,
     4                ALPAMP,AMOAXS,GUSAMP,DISAMP,PITAXS,PITCOR
C
      S1    = 2.0 + SNS - SPS
      T1    = CEXP(-AI*SIGMA)
      T2    = CEXP(+AI*SIGMA)
      TEMP  = S1/RL2
      C1A   = AI*GL
      CONST = B*AI*(DEL-AMU)*BLAM2/BKAP1
      CEXP3 = CEXP(C1A*SPS )
      CEXP4 = CEXP(C1A*TEMP)
      XL    = SPS
      DO 456 JL = 1,NL2
      PRES3(JL) = (FT2T*CEXP3+FT3T+CONST*XL)*T1
      CEXP3 = CEXP3*CEXP4
      XL    = XL + TEMP
  456 CONTINUE
      FT3TST = 0.0
      FT2  = 0.0
      FT3  = 0.0
      FT2T = 0.0
      FT3T = 0.0
      FQA  = BKDEL1/(BC*BETA)*(A*AI*BKDEL2/BKDEL1-B*BLKAP1)*
     1       CEXP(-AI*(DEL*SPS-SIGMA)/2.0)
      DO 60 I = 1,50
      RT  = 0.0
      R   = I - 1
      RI  = (-1.0)**(I-1)
CWKBR ALP = SQRT((R*PI/SNS)**2+SCRK**2)
      ALP = SQRT((R*PI/SNS)**2+SCRK**2)
      ALN = -ALP
      CALL AKAPM (ALP,BKDEL3)
      T3  = ALP - DEL
      SVKL1(I) = BKDEL3
      IF (I .EQ. 1) RT = 1.0
      SUM1  = (ALP-AMU)/(T3)*(RI-CEXP(AI*(T3)*SPS)*T2)/
     1        (BETA*(1.0+RT))*RI/(SNS*ALP)*BKDEL1/BKDEL3*(A*AI*BKDEL2/
     2        BKDEL1*(T3)/(T3+GL)-B*BLKAP1-B/(T3))
      SUM1T = (ALP-AMU)/(T3)*(1.0-CEXP(AI*(T3)*SPS)*T2*RI)/
     1        (BETA*(1.0+RT))*RI/(SNS*ALP)*BKDEL1/BKDEL3*(A*AI*BKDEL2/
     2        BKDEL1*(T3)/(T3+GL)-B*BLKAP1-B/(T3))
      SUMSV1(I) = (ALP-AMU)/(T3)*(1.0-CCOS((T3)*SPS+SIGMA+R*PI))/
     1        (BETA*(1.0+RT)*SNS*ALP)*BKDEL1/BKDEL3*CEXP(-2.0*AI*(ALP-
     2        DEL))*(A*BKDEL2/BKDEL1*(T3)/(T3+GL)+B*AI*BLKAP1+B*AI/(T3))
      FT2   = SUM1*AI/(T3)*(CEXP(-2.0*AI*(T3))-CEXP(-AI*(SPS-SNS)*(T3)))
     1        + FT2
      FT3   = SUM1*(2.0*AI*CEXP(-2.0*AI*(T3))/(T3)-AI*(SPS-SNS)/
     1        (T3)*CEXP(-AI*(T3)*(SPS-SNS))+CEXP(-2.0*AI*(T3))/
     2        ((T3)**2)-CEXP(-AI*(T3)*(SPS-SNS))/((T3)**2)) + FT3
      FT2T  = SUM1T*T1*CEXP(-AI*(T3)*SPS)*AI/(T3)*(CEXP(-AI*(T3)*(S1))-
     1        1.0) + FT2T
      FT3T  = SUM1T*T1*CEXP(-AI*(T3)*SPS)*((S1)*AI/(T3)*CEXP(-AI*(T3)*
     1        (S1)) + 1.0/((T3)**2)*(CEXP(-AI*(T3)*(S1))-1.0)) + FT3T
      CALL AKAPM (ALN,BKDEL3)
      T4    = ALN - DEL
      SVKL2(I) = BKDEL3
      SUM2  = (ALN-AMU)/(T4)*(RI-CEXP(AI*(T4)*SPS)*T2)/(BETA*(1.0+RT))*
     1        RI/(SNS*ALN)*BKDEL1/BKDEL3*(A*AI*BKDEL2/BKDEL1*(T4)/
     2        (T4+GL)-B*BLKAP1-B/(T4))
      SUM2T = (ALN-AMU)/(T4)*(1.0-CEXP(AI*(T4)*SPS)*T2*RI)/(BETA*(1.0+
     1        RT))*RI/(SNS*ALN)*BKDEL1/BKDEL3*(A*AI*BKDEL2/BKDEL1*(T4)/
     2        (T4+GL)-B*BLKAP1-B/(T4))
      SUMSV2(I) = (ALN-AMU)/(T4)*(1.0-CCOS((T4)*SPS+SIGMA+R*PI))/
     1        (BETA*(1.0+RT)*SNS*ALN)*BKDEL1/BKDEL3*CEXP(-2.0*AI*(T4))*
     2        (A*BKDEL2/BKDEL1*(T4)/(T4+GL)+B*AI*BLKAP1+B*AI/(T4))
      FT2   = FT2+SUM2*AI/(T4)*(CEXP(-2.0*AI*(T4))-CEXP(-AI*(SPS-SNS)*
     1        (T4)))
      FT2T  = SUM2T*T1*CEXP(-AI*(T4)*SPS)*AI/(T4)*(CEXP(-AI*(T4)*(S1))-
     1        1.0) + FT2T
      FT3   = FT3+SUM2*(2.0*AI*CEXP(-2.0*AI*(T4))/(T4)-AI*(SPS-SNS)/
     1        (T4)*CEXP(-AI*(T4)*(SPS-SNS))+CEXP(-2.0*AI*(T4))/
     2        ((T4)**2)-CEXP(-AI*(T4)*(SPS-SNS))/((T4)**2))
      FT3T  = FT3T+SUM2T*T1*CEXP(-AI*(T4)*SPS)*((S1)*AI/(T4)*
     1        CEXP(-AI*(T4)*(S1))+1./((T4)**2)*(CEXP(-AI*(T4)*(S1))-1.))
      I7    = I
      AA    = SPS - SNS
      TEMP  = S1/RL2
      TEMP2 = R*PI/SNS
      CONST = 4.0/PI*FQA
      TEMP3 = R + RT
      C3A   = -AI*T3
      C4A   = -AI*T4
      C1A   = AI*DEL
      CEXP3A = CEXP(C3A*AA)
      CEXP3B = CEXP(C3A*SPS)
      CEXP3C = CEXP(C3A*TEMP)
      CEXP4A = CEXP(C4A*AA)
      CEXP4B = CEXP(C4A*SPS)
      CEXP4C = CEXP(C4A*TEMP)
      CEXP2A = CEXP(C1A*AA)
      CEXP2B = CEXP(C1A*SPS)
      CEXP2C = CEXP(C1A*TEMP)
      XL1    = AA
      DO 457 JL = 1,NL2
      PRES2(JL) = SUM1*CEXP3A+SUM2*CEXP4A + PRES2(JL)
      PRES2(JL) = PRES2(JL) + CONST*CEXP2A*RI/TEMP3*SIN(TEMP2*(XL1-SPS))
      XL2 = XL1 + SNS
      PRES3(JL) = (SUM1T*CEXP3B+SUM2T*CEXP4B)*T1 + PRES3(JL)
      PRES3(JL) = PRES3(JL)+CONST*CEXP2B/TEMP3*SIN(TEMP2*(XL2-SPS))*T1
      XL1 = XL1 + TEMP
      CEXP3A = CEXP3A*CEXP3C
      CEXP4A = CEXP4A*CEXP4C
      CEXP2A = CEXP2A*CEXP2C
      CEXP3B = CEXP3B*CEXP3C
      CEXP4B = CEXP4B*CEXP4C
      CEXP2B = CEXP2B*CEXP2C
  457 CONTINUE
      IF (CABS((FT3-FT3TST)/FT3) .LT. 0.0006) GO TO 65
      FT3TST = FT3
   60 CONTINUE
      GO TO 9994
   65 CONTINUE
      FT3TST = FT3
      F2     = F2  + FT2
      AM2    = AM2 + FT3
      F2P    = F2P + FT2T
      AM2P   = AM2P+ FT3T
      AA     = SPS - SNS
      AA1    = SPS + SNS
      AA2    = SPS + 2.0*SNS
      TEMP   = S1/RL2
      XL     = AA
      C1A    = AI*DEL
      CEXP3  = CEXP(C1A*AA)
      CEXP3C = CEXP(C1A*TEMP)
      CEXP4  = CEXP(C1A*SPS)
      CONST  = 2.0*FQA
      CEXP2A = T1*CONST
      DO 4571 JL = 1,NL2
      STEP = 0.0
      IF (XL .GE. AA1) STEP = 1.0
      PRES2(JL) = PRES2(JL) + CONST*CEXP3*((XL-SPS)/SNS-2.0*STEP)
      XL2  = XL + SNS
      STEP = 0.0
      IF (XL2 .GE. AA2) STEP = 1.0
      PRES3(JL) = PRES3(JL) - CEXP2A*CEXP4*(1.0-(XL2-SPS)/SNS+2.0*STEP)
      CEXP3 = CEXP3*CEXP3C
      CEXP4 = CEXP4*CEXP3C
      XL  = XL + TEMP
 4571 CONTINUE
      GAM = SPS*DEL - SIGMA
      C1P = (GAM/DSTR) - SCRK
      C2P = (GAM/DSTR) + SCRK
      ALP = GAM*SPS/(DSTR**2) - SNS/DSTR*CSQRT(C1P)*CSQRT(C2P)
      T3  = ALP - DEL
      F4  = CEXP(AI*(ALP*SPS-GAM))*(ALP*SPS-GAM)/((ALP*DSTR**2-GAM*SPS)*
     1      (T3))
      CALL AKAPM (ALP,BKDEL3)
      SBKDE1(1) = BKDEL3
      SBKDE2(1) = 0.0
      CALL AKAPPA (DEL,BKAP1)
      CARG = DEL - GL
      CALL AKAPPA (CARG,CKAP1)
      F4  = F4*BKDEL3/(BKDEL1*BKAP1)*(A*(BKDEL1/BKDEL2*(T3)/(T3+GL)*
     1      (DEL-GL-AMU)*CEXP(2.0*AI*GL)*BKAP1/CKAP1)+B*AI*(1.0-2.0*AI*
     2      (DEL-AMU)-(DEL-AMU)*RES)-B*AI*(DEL-AMU)*(BLKAP1-1.0/(T3)))
      F5S = B*AI/(BKDEL1*BKAP1)*(1.0-2.0*AI*(DEL-AMU) - (DEL-AMU)*RES -
     1      (DEL-AMU)*BLKAP1)
      F6S = A/(BKDEL1*BKAP1)*(BKDEL1/BKDEL2*(DEL-GL-AMU)*CEXP(2.0*AI*GL)
     1      *BKAP1/CKAP1)
      F4S = F4
      FQ7 = BC*(F6S+F5S)
      TEMP  = (SPS-SNS)/RL1
      TEMP2 = 2.0 - SPS
      CONST = -T1*F4S
      C1A   = -AI*T3
      CEXP3A = CEXP(C1A*SNS)
      CEXP3B = CEXP(C1A*TEMP)
      DO 458 JL = 1,NL
      PRES4(JL) = CONST*CEXP3A
      CEXP3A = CEXP3A*CEXP3B
  458 CONTINUE
      C1  = CEXP(-AI*(T3)*SPS)
      C2  = CEXP(-AI*(T3)*SNS)
      F4  = F4*AI*T1/(T3)*(C1-C2)
      AM4 = F4S*T1*(AI*SPS*C1/(T3)-AI*SNS*C2/(T3)+(C1-C2)/
     1      ((T3)**2))+F4S*AI*(2.0-SPS)*T1/(T3)*(C1-C2)
      CALL SUBC
      RETURN
C
 9994 WRITE  (IBBOUT,3015) UFM
 3015 FORMAT (A23,' - AMG MODULE -SUBROUTINE SUBC.  AM4 LOOP DID NOT ',
     1       'CONVERGE.')
      CALL MESAGE (-61,0,0)
      RETURN
      END
