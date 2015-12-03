      SUBROUTINE TIMEEQ (B,BBAR,C,CBAR,R,IENTRY,NCOL,TIM)
C
C     TIMEEQ SOLVES THE TIME AND CORE FUNCTIONS FOR DECOMP AND CDCOMP
C
      INTEGER         SYSBUF
      REAL            MB(1),MC(1),K1,K2,K3,K4,K5
      COMMON /NTIME / LNTIME, TCONS(15)
     1       /SYSTEM/ KSYSTM(65)
C
      EQUIVALENCE     (KSYSTM( 1),SYSBUF),(KSYSTM(40),NBPW),
     1                (KSYSTM(55),IPREC ),(TCONS (1) ,AAIO),
     2                (TCONS ( 2),AAPAK ),(TCONS (8),MB(1)),
     3                (TCONS (12),MC(1) )
C
C
      IRET  = 0
      IENTR = IENTRY
    1 AMB   = MB(IPREC)
      AMC   = MC(IPREC)
      IF (NBPW .LT. 60) GO TO 2
      AMB   = 3.0*AMB
      AMC   = 3.0*AMC
    2 AIO   = AAIO
      APAK  = AAPAK
      IF (IENTR .EQ. 1) GO TO 10
      AMB   = 5.*AMB
      AMC   = 5.*AMC
      AIO   = AIO+AIO
      APAK  = 1.1*APAK
   10 IF (IRET .EQ. 1) GO TO 20
      TIM = FLOAT(NCOL)*(AMB*BBAR*R+AMC*(BBAR*C+BBAR*CBAR+B*CBAR+
     1      2.0*C*CBAR)+AIO*BBAR*(B+BBAR-R-1.0))*1.E-06
      RETURN
C
C
      ENTRY TFIN (AB,ABBAR,AC,ACBAR,AR,JENTRY,ANCOL,TIMEX)
C     ====================================================
C
      IRET  = 1
      IENTR = JENTRY
      GO TO 1
   20 TIMEX = 0.
      K1    = ANCOL - AB - ABBAR - ABBAR
      IF (K1 .LE. 0.) GO TO 30
      TIMEX = K1*(AMB*ABBAR*AR+AIO*ABBAR*(AB+ABBAR-AR)+APAK*(AB+ABBAR*
     1        2.))
   30 K2  = AB + ABBAR
      K3  = K2
      IF (ANCOL .GE. AB+ABBAR+ABBAR) GO TO 35
      K2  = ANCOL - ABBAR
      K3  = AB + ABBAR
      IF (ANCOL .LT. AB+ABBAR) K3 = ANCOL
   35 TIMEX = TIMEX+.5*K2*(ABBAR*K2*AMB+(K3-AR)*(AIO-AMB)*ABBAR+
     1        2.*APAK*ABBAR+APAK*K2)
      IF (ANCOL .LT. AB+ABBAR+ABBAR) GO TO 40
      K4 = AB + ABBAR - AR
      K5 = AB + 1.5*ABBAR
      IF (AB .GT. AR) K4 = ABBAR
      GO TO 50
   40 K4 = ANCOL - AR
      K5 = ANCOL
      IF (ANCOL-AR .GT. ABBAR) K4 = ABBAR
   50 TIMEX = TIMEX+ABBAR**3/3.*AMB+K4**3*.5*AIO+APAK*ABBAR*K5
      TIMEX = (TIMEX+(ANCOL-ABBAR)*(AMC*(ABBAR*AC+AB*ACBAR+ABBAR*ACBAR+
     1        AC*ACBAR)+APAK*(AC+ACBAR)))*1.E-06
      RETURN
C
C
      ENTRY RCORE (IB,IBBAR,IC,ICBAR,INCOL,KENTRY,NX,IR)
C     ==================================================
C     ENTRY FOR THE CORE FUNCTION
C
      IR = (NX-((IB+IBBAR+1) +2*KENTRY*MIN0(INCOL,IB+IBBAR+IBBAR)+
     1     2*KENTRY*IC*(IBBAR+2)+2*ICBAR*KENTRY*(MIN0(IB+IBBAR,INCOL)+1)
     2     +2*KENTRY*IC*ICBAR +IC+ICBAR*KENTRY+ICBAR)-6*SYSBUF)/
     3     (2*KENTRY*IBBAR)
      RETURN
      END
