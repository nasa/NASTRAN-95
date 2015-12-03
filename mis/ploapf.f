      SUBROUTINE PLOAPF (ECPT,IECPT,L,PA,PB)
C
C     THIS ROUTINE IS CALLED ONLY BY PLOAD1 FOR HANDLING PIN FLAGS OF
C     THE CBAR
C
      REAL            I1,I2,I12,K1,K2,L,L2,L3,KE,KEP,LB,LR1,LR2,
     1                L2B3,L2B6
      DIMENSION       ECPT(33),IECPT(9),PA(1),PB(1),PE(12),PEP(12),
     1                IPIN(10),KE(144),KEP(144)
      COMMON /MATOUT/ E,G
C
      KA = IECPT(8)
      KB = IECPT(9)
      IF (KA.EQ.0 .AND. KB.EQ.0) GO TO 200
      DO 10 I = 1,6
      PE(I  ) = PA(I)
   10 PE(I+6) = PB(I)
      L2  = L**2
      L3  = L2*L
      A   = ECPT(17)
      I1  = ECPT(18)
      I2  = ECPT(19)
      FJ  = ECPT(20)
      K1  = ECPT(31)
      K2  = ECPT(32)
      I12 = ECPT(33)
      EI1 = E*I1
      EI2 = E*I2
      R1  = 12.0*EI1/L3
      R2  = 12.0*EI2/L3
      IF (K1.EQ.0.0 .OR. I12.NE.0.0) GO TO 20
      GAK = G*A*K1
      R1  = (12.0*EI1*GAK)/(GAK*L3 + 12.0*L*EI1)
   20 IF (K2.EQ.0.0 .OR. I12.NE.0.0) GO TO 30
      GAK = G*A*K2
      R2  = (12.0*EI2*GAK)/(GAK*L3 + 12.0*L*EI2)
C
C     COMPUTE THE -SMALL-K-S. SK1, SK2, SK3 AND SK4
C
   30 SK1 = 0.25*R1*L2 + EI1/L
      SK2 = 0.25*R2*L2 + EI2/L
      SK3 = 0.25*R1*L2 - EI1/L
      SK4 = 0.25*R2*L2 - EI2/L
C
C     COMPUTE THE 12 X 12 MATRIX KE
C
      AEL = A*E /L
      LR1 = L*R1/2.0
      LR2 = L*R2/2.0
      GJL = G*FJ/L
C
      DO 40 I = 1,144
   40 KE(I) = 0.0
      KE(  1) = AEL
      KE(  7) =-AEL
      KE( 14) = R1
      KE( 18) = LR1
      KE( 20) =-R1
      KE( 24) = LR1
      KE( 27) = R2
      KE( 29) =-LR2
      KE( 33) =-R2
      KE( 35) =-LR2
      KE( 40) = GJL
      KE( 46) =-GJL
      KE( 51) =-LR2
      KE( 53) = SK2
      KE( 57) = LR2
      KE( 59) = SK4
      KE( 62) = LR1
      KE( 66) = SK1
      KE( 68) =-LR1
      KE( 72) = SK3
      KE( 73) =-AEL
      KE( 79) = AEL
      KE( 86) =-R1
      KE( 90) =-LR1
      KE( 92) = R1
      KE( 96) =-LR1
      KE( 99) =-R2
      KE(101) = LR2
      KE(105) = R2
      KE(107) = LR2
      KE(112) =-GJL
      KE(118) = GJL
      KE(123) =-LR2
      KE(125) = SK4
      KE(129) = LR2
      KE(131) = SK2
      KE(134) = LR1
      KE(138) = SK3
      KE(140) =-LR1
      KE(144) = SK1
      IF (I12 .EQ. 0.0) GO TO 50
      BETA    =-12.0*E*I12/L3
      LB      = L *BETA/2.0
      L2B3    = L2*BETA/3.0
      L2B6    = L2*BETA/6.0
      KE( 15) =-BETA
      KE( 17) = LB
      KE( 21) = BETA
      KE( 23) = LB
      KE( 26) =-BETA
      KE( 30) =-LB
      KE( 32) = BETA
      KE( 36) =-LB
      KE( 50) = LB
      KE( 54) = L2B3
      KE( 56) =-LB
      KE( 60) = L2B6
      KE( 63) =-LB
      KE( 65) = L2B3
      KE( 69) = LB
      KE( 71) = L2B6
      KE( 87) = BETA
      KE( 89) =-LB
      KE( 93) =-BETA
      KE( 95) =-LB
      KE( 98) = BETA
      KE(102) = LB
      KE(104) =-BETA
      KE(108) = LB
      KE(122) = LB
      KE(126) = L2B6
      KE(128) =-LB
      KE(132) = L2B3
      KE(135) =-LB
      KE(137) = L2B6
      KE(141) = LB
      KE(143) = L2B3
C
C     SET UP THE IPIN ARRAY
C
   50 DO 60 I = 1,5
      IPIN(I  ) = MOD(KA,10)
      IPIN(I+5) = MOD(KB,10) + 6
      IF (IPIN(I+5) .EQ. 6) IPIN(I+5) = 0
      KA = KA/10
   60 KB = KB/10
C
C     ALTER KE MATRIX DUE TO PIN FLAGS
C
      DO 130 I = 1,10
      IP = IPIN(I)
      IF (IP .EQ. 0) GO TO 130
      II = IP*13 - 12
      IF (KE(II) .NE. 0.0) GO TO 80
      IL = IP
      II = II - IL
      DO 70 J = 1,12
      II = II + 1
      KE(II) = 0.0
      KE(IL) = 0.0
   70 IL = IL + 12
      GO TO 130
   80 IP12 = (IP-1)*12
      DO 100 J = 1,12
      J12 = (J-1)*12
      JI  = J12  + IP
      IJ  = IP12 + J
      DO 90 LL = 1,12
      JLL = J12  + LL
      ILL = IP12 + LL
   90 KEP(JLL) = KE(JLL) - (KE(ILL)/KE(II))*KE(JI)
      PEP(J  ) = PE(J  ) - (KE(JI )/KE(II))*PE(IP)
      KEP(IJ ) = 0.0
      KEP(JI ) = 0.0
  100 CONTINUE
      PEP(IP ) = 0.0
      DO 110 K = 1,144
  110 KE(K) = KEP(K)
      DO 120 K = 1,12
  120 PE(K) = PEP(K)
  130 CONTINUE
C
      DO 140 I = 1,10
      IP = IPIN(I)
      IF (IP .EQ. 0) GO TO 140
      PE(IP) = 0.0
  140 CONTINUE
      DO 150 I = 1,6
      PA(I) = PE(I  )
  150 PB(I) = PE(I+6)
C
  200 RETURN
      END
