      SUBROUTINE KFLUD2
C
C     THIS ROUTINE GENERATES THE PSUEDO STIFFNESS MATRIX TERMS
C     FOR THE CENTER PLUG FLUID ELEMENT
C
C     THE ECPT DATA BLOCK CONTAINS THE FOLLOWING DATA
C
C         FIELD    SYMBOL
C           1        ID
C           2        SIL1
C           3        SIL2
C           4        RHO
C           5        BULK
C           6        N
C           7        CSF
C           8        R1
C           9        Z1
C           10       -
C           11       CSF
C           12       R2
C           13       Z2
C           14       -
C           15       -
C
      LOGICAL          NOGO
      INTEGER          OUT,ELTYPE,NECPT(100)
      DOUBLE PRECISION R1,Z1,R2,Z2,CONSTD,DPI,
     1                 Z1P,Z2P,Z1P1,Z2P1,RK,RI,KFACT,F0,A,B,I2N0,I2N1,
     2                 I2N2,I2NP2,DZ,HPQ,PIRHO,TWOPR,KH,K1,K2
      CHARACTER        UFM*23
      COMMON  /XMSSG / UFM
      COMMON  /CONDAD/ CONSTD(5)
      COMMON  /SYSTEM/ SYSBUF,OUT,NOGO
      COMMON  /EMGDIC/ ELTYPE
      COMMON  /SMA1IO/ DUM1(10),IFKGG
      COMMON  /SMA1CL/ IOPT4,K4GGSW,NPVT
      COMMON  /SMA1DP/ Z1P,Z2P,RK,RI,KFACT,F0,A,B,I2N0,I2N1,I2N2,I2NP2,
     1                 DZ,HPQ(4),PIRHO,TWOPR,KH(4),K1,K2
      COMMON  /SMA1ET/ ECPT(100)
      EQUIVALENCE      (CONSTD(1),DPI),(ECPT(1),NECPT(1))
C
C
      IF (ECPT(13)-ECPT(9)) 5,10,10
    5 R1 = ECPT(12)
      R2 = ECPT(8)
      Z1 = ECPT(13)
      Z2 = ECPT(9)
      I  = NECPT(3)
      NECPT(3) = NECPT(2)
      NECPT(2) = I
      GO TO 15
   10 R1 = ECPT(8)
      Z1 = ECPT(9)
      R2 = ECPT(12)
      Z2 = ECPT(13)
   15 IF (R1.EQ.0.0D0 .OR. R2.EQ.0.0D0) GO TO 5000
      IF (Z1 .EQ. Z2) RETURN
C
C     CALCULATE THE INTEGRAL PARAMETERS I2N0,I2N1,I2N2,AND I2NP2
C
      K  = 2*NECPT(6)
      RK = K
      IF (K .GT. 0) GO TO 20
C
      I2N0 = 0.0
      I2N1 = 0.0
      I2N2 = 0.0
      I2NP2= (Z2-Z1)*(R2**2 + R2*R1 + R1**2)/6.0D0
C
      GO TO 300
C
   20 B    = (R2-R1)/(Z2-Z1)
      DUM  = DABS(B)
      IF (DUM .GT. 1.0E-6) GO TO 30
C
      Z1P  = ((R1+R2)/2.0D0)**K
      I2N0 = (Z1P/RK)*(Z2-Z1)
      I2N1 = I2N0*(Z2+Z1)/2.0D0
      I2N2 = I2N0*(Z2**2+Z2*Z1+Z1**2)/3.0D0
      I2NP2= I2N0*RK/(RK+2.0D0)*R1**2
      GO TO 300
   30 Z1P  = R1**(K+1)
      Z2P  = R2**(K+1)
      Z1P1 = Z1P*R1
      Z2P1 = Z2P*R2
C
      A    = 1.0D0/B
      I2N0 = A/(RK*(RK+1.0D0))*(Z2P-Z1P)
      I2N1 = A/(RK*(RK+1.0D0))*(Z2P*Z2-Z1P*Z1-A/(RK+2.0D0)*(Z2P1-Z1P1))
      I2N2 = A/(RK*(RK+1.0D0))*(Z2P*Z2**2-Z1P*Z1**2 -A/(RK+2.0D0)*2.0D0
     1     * (Z2P1*Z2-Z1P1*Z1-A/(RK+3.0D0)*(Z2P1*R2-Z1P1*R1)))
      I2NP2= A/((RK+2.0D0)*(RK+3.0D0))*(Z2P1*R2-Z1P1*R1)
  300 DZ   = Z2 - Z1
      N    = NECPT(6)
      Z1P  = R1**N
      Z2P  = R2**N
      HPQ(1) = Z2/(DZ*Z1P)
      HPQ(2) =-Z1/(DZ*Z2P)
      HPQ(3) =-1.0D0/(DZ*Z1P)
      HPQ(4) = 1.0D0/(DZ*Z2P)
      LP   = 1
      IF (NPVT .EQ. NECPT(2)) GO TO 320
      IF (NPVT .EQ. NECPT(3)) GO TO 310
      RETURN
C
  310 LP = 2
  320 IF (ECPT(4) .EQ. 0.0) RETURN
      PIRHO  = DPI/DBLE(ECPT(4))
      IF (N .EQ. 0) PIRHO = PIRHO*2.0D0
      RK = N
      TWOPR = 2.0*PIRHO*RK**2
      KH(1) = TWOPR*(I2N0*HPQ(LP)+I2N1*HPQ(LP+2))
      KH(2) = TWOPR*(I2N1*HPQ(LP)+I2N2*HPQ(LP+2)) +PIRHO*I2NP2*HPQ(LP+2)
      K1    = KH(1)*HPQ(1) + KH(2)*HPQ(3)
      K2    = KH(1)*HPQ(2) + KH(2)*HPQ(4)
      IFILE = IFKGG
      I     = NPVT
      J     = NECPT(2)
      CALL SMA1B (K1,J,I,IFILE,0.0D0)
      J     = NECPT(3)
      CALL SMA1B (K2,J,I,IFILE,0.0D0)
      RETURN
C
 5000 N = NECPT(1)
      IF (ELTYPE .EQ. 43) N = N/1000
      WRITE  (OUT,6000) UFM,N
 6000 FORMAT (A23,' 5000, NEGATIVE OR ZERO RADIUS DETECTED FOR ',
     1       'CFLUID2/CAXIF2 ELEMENT ID',I9)
      NOGO = .TRUE.
      RETURN
      END
