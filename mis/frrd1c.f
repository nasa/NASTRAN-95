      SUBROUTINE FRRD1C (FRL,FRQSET,MDD,BDD,KDD,IFR,ULL,LLL,SCR1,SCR2,
     1                   SCR3,SCR4,IGOOD)
C                                         (A)      (B)        (C)
C     THIS ROUTINE FORMS AND DECOMPOSES   KDD + I*W*BDD - W**2*MDD
C     WHERE  W = OMEGA, CYCLIC FREQ. AND I = SQUARE ROOT MINUS ONE
C
C     THE DECOMPOSITION ROUTINES ARE CALLED ACCORDING TO THE FOLLOWING
C     TABLE AS DETERMINED BY THE MATRIX RESULTING FROM THE ADDITION
C
C     IF MATRIX IS     COMPLEX SYMMETRIC    CALL SDCOMP
C                              UNSYMMETRIC  CALL CDCOMP
C                      REAL    SYMMETRIC    CALL SDCOMP
C                              UNSYMMETRIC  CALL DECOMP
C
      INTEGER          FA,FL,SCR1,SCR2,FRL,FU,FRQ SET,BDD,ULL,LLL,SR1,
     1                 SR2,SCR3,SYSBUF,SCR4,SR3,CHLSKY,NAME(2),
     2                 MCORE(1),ICORE(1)
      DOUBLE PRECISION DET,MINDA,AMCB(2),BMCB(2),CMCB(2),DDR,DDC,MINDD,
     1                 DETT
      CHARACTER        UFM*23
      COMMON /XMSSG /  UFM
      COMMON /SYSTEM/  KSYSTM(63)
      COMMON /CDCMPX/  FA(7),FL(7),FU(7),SR1,SR2,SR3,DET(2),POWR,NX,
     1                 MINDA,IB,IBBAR
      COMMON /DCOMPX/  IA(7),IL(7),IU(7),ISCR1,ISCR2,ISCR3,DETT,IPOW,
     1                 NY,MINDIA,IIB,IIBB,ICBR(3)
      COMMON /SFACT /  MFA(7),MFL(7),MFC(7),M1FIL,M2FIL,MXX,DDR,DDC,
     1                 POWER,M3FIL,MINDD,CHLSKY
      COMMON /SADDX /  NOMAT,LCORE,MCBA(12),MCBB(12),MCBC(12),MCBD(12),
     1                 MCBE(12),MX(7)
      COMMON /ZZZZZZ/  CORE(1)
      EQUIVALENCE      (MCORE(1),CORE(1))
      EQUIVALENCE      (ICORE(1),CORE(1))
      EQUIVALENCE      (AMCB(1),MCBA(9)),(BMCB(1),MCBB(9)),
     1                 (CMCB(1),MCBC(9)),(KSYSTM(2),NOUT)
      EQUIVALENCE      (KSYSTM(1),SYSBUF),(KSYSTM(55),IPREC)
      DATA    NAME  /  4HFRRD,4H1C  /
C
C
      NX = KORSZ(CORE)
      NZ = NX - SYSBUF
C
C     PICK UP CURRENT FREQUENCY
C
      CALL GOPEN  (FRL,CORE(NZ+1),0)
      CALL SKPREC (FRL,FRQSET-1)
      CALL FREAD  (FRL,CORE,IFR,1)
      W  = CORE(IFR)
      CALL CLOSE  (FRL,1)
C
C     ADD MATRICES TOGETHER
C
      MCBA(1) = KDD
      MCBB(1) = BDD
      MCBC(1) = MDD
      CALL RDTRL (MCBA)
      CALL RDTRL (MCBB)
      CALL RDTRL (MCBC)
      IF (MCBA(1).GT.0 .AND. MCBC(1).GT.0) GO TO 20
      WRITE  (NOUT,10) UFM
   10 FORMAT (A23,', EITHER STIFFNESS MATRIX OR MASS MATRIX IS MISSING')
      CALL MESAGE (-37,0,NAME)
C
   20 MCBA(8) = 2
      MCBB(8) = 4
      MCBC(8) = 2
      AMCB(1) = 1.0D0
      AMCB(2) = 0.0D0
      BMCB(1) = 0.0D0
      BMCB(2) = W
      CMCB(1) =-W*W
      CMCB(2) = 0.0D0
      IF (MCBB(1) .GT. 0) GO TO 30
C
C     NO BDD TO BE ADDED
C
      MCBB(1) = 0
      MCBB(8) = 0
      BMCB(2) = 0.0D0
C
   30 MX(1)   = SCR3
      MX(2)   = MCBA(2)
      MX(3)   = MCBA(3)
      MX4A    = 6
      MX4B    = 6
      MX4C    = 6
      IF (MCBA(1) .GT. 0) MX4A = MCBA(4)
      IF (MCBB(1) .GT. 0) MX4B = MCBB(4)
      IF (MCBC(1) .GT. 0) MX4C = MCBC(4)
      MX(4) = MIN0(MX4A,MX4B,MX4C)
      MX(5) = 2 + IPREC
      IF (MCBA(1).GT.0 .AND. MCBA(5).GT.2) GO TO 40
      IF (MCBB(1) .GT. 0) GO TO 40
      IF (MCBC(1).GT.0 .AND. MCBC(5).GT.2) GO TO 40
      MX(5) = IPREC
   40 CONTINUE
      LCORE = NX
      NOMAT = 3
      CALL SADD  (CORE,CORE)
      CALL WRTTRL (MX)
C
C     SET UP TO DECOMPOSE MATRICES
C
      FA(1) = SCR3
      CALL RDTRL (FA)
      IGOOD = 1
      IF (FA(4) .EQ. 6) GO TO 120
      IF (FA(5) .LE. 2) GO TO 150
      FL(1) = LLL
      FU(1) = ULL
      DO 50 I = 2,7
      FL(I) = FA(I)
      FU(I) = FA(I)
   50 CONTINUE
      FL(4) = 4
      FU(4) = 5
      SR1   = SCR1
      SR2   = SCR2
      SR3   = SCR4
      CALL CDCOMP (*100,CORE(1),CORE(1),CORE(1))
      IGOOD = 0
      CALL WRTTRL (FL)
      CALL WRTTRL (FU)
C
C     FORCE RE-EVALUATION OF DECOMP PARAM IF W = 0.0
C
   60 IF (W .NE. 0.0) GO TO 70
      IB    = 0
      IBBAR = 0
   70 RETURN
C
C     MATRIX SINGULR
C
  100 I = 5
      IF (W .NE. 0.0) I = -5
      CALL MESAGE (I,SCR3,NAME)
      GO TO 60
C
C     USE SDCOMP TO PERFORM DECOMPOSITION
C
  120 MFA(1) = SCR3
      MFL(1) = LLL
      MFC(1) = ULL
      DO 130 I = 2,7
      MFA(I) = FA(I)
      MFL(I) = FA(I)
      MFC(I) = FA(I)
  130 CONTINUE
      MFL(4) = 4
      M1FIL  = SCR1
      M2FIL  = SCR2
      M3FIL  = SCR4
      MXX    = KORSZ(MCORE)
      CHLSKY = 0
      CALL SDCOMP (*100,MCORE,MCORE,MCORE)
      IGOOD  = 0
C
C     DIRECTION FOR FRRD1D TO USE  FBS RATHER THAN GFBS
C
      ULL = -IABS(ULL)
C
      CALL WRTTRL (MFL)
      GO TO 60
C
C     USE DECOMP TO PERFORM DECOMPOSITION
C
  150 IA(1) = SCR3
      IL(1) = LLL
      IU(1) = ULL
      DO 160 I = 2,7
      IA(I) = FA(I)
      IL(I) = FA(I)
      IU(I) = FA(I)
  160 CONTINUE
      IL(4) = 4
      IU(4) = 5
      ISCR1 = SCR1
      ISCR2 = SCR2
      ISCR3 = SCR4
      NY    = KORSZ(ICORE)
      CALL DECOMP (*100,ICORE,ICORE,ICORE)
      CALL WRTTRL (IL)
      CALL WRTTRL (IU)
      IGOOD = 0
      RETURN
      END
