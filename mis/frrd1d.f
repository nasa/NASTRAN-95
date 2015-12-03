      SUBROUTINE FRRD1D (PD,ULL,LLL,SCR1,SCR2,UDV,IFR,NLOAD,IGOOD,NFREQ)
C
C     ROUTINE SOLVES FOR UDV GIVEN ULL,LLL, AND PD
C
C     IF IGOOD = 1 DCOMP FAILED -- PUT ZERO SOLUTION VECTORS OUT
C
C     1. PULL LOADS FROM PD ONTO SCR1
C     2. SOLVE FOR UDV-S ON SCR2
C     3. STACK SOLVED LOADS ON UDV
C
      INTEGER         SYSBUF,PD,ULL,LLL,SCR1,SCR2,UDV,FL,FU,FB,FX,PREC,
     1                FILE,ICORE(1),UDV1,MCB(7),NAME(2),MCORE(1)
      COMMON /MACHIN/ MACH
      COMMON /UNPAKX/ IT1,II,JJ,INCR
      COMMON /PACKX / IT2,IT3,II1,JJ1,INCR1
      COMMON /SYSTEM/ KSYSTM(65)
      COMMON /GFBSX / FL(7),FU(7),FB(7),FX(7),NX,PREC,ISIGN
      COMMON /FBSX  / MFL(7),MFLT(7),MFB(7),MFX(7),MX,MPREC,MSIGN,ISCRX
      COMMON /ZZZZZZ/ CORE(1)
      EQUIVALENCE     (KSYSTM(1),SYSBUF),(KSYSTM(55),IPREC),
     1                (CORE(1),ICORE(1),MCORE(1))
      DATA    NAME  / 4HFRRD,4H1D  /, IC / 0 /
C
      NX    = KORSZ(CORE)
      FB(1) = PD
      CALL RDTRL (FB)
      FX(1) = SCR2
      IF (IFR .EQ. 1) FX(1) = UDV
      FX(2) = NLOAD
      FX(3) = FB(3)
      FX(4) = 2
      FX(5) = 2 + IPREC
      IT1   = FB(5)
      INCR  = 1
      INCR1 = 1
      IT2   = IT1
      IT3   = 2 + IPREC
      IF (IGOOD .EQ. 1) GO TO 98
C
C     PULL LOADS FROM PD ONTO SCR1
C
      FU(1) = ULL
      CALL RDTRL (FU)
      FL(1) = LLL
      CALL RDTRL (FL)
      IF (NFREQ .EQ. 1) GO TO 30
      NZ = NX - SYSBUF
      CALL GOPEN (PD,CORE(NZ+1),0)
      CALL SKPREC (PD,IFR-1)
      NZ = NZ - SYSBUF
      CALL GOPEN (SCR1,CORE(NZ+1),1)
      CALL MAKMCB (MCB,SCR1,FB(3),2,IT3)
      DO 10 I = 1,NLOAD
      IF (I .GT. 1) CALL SKPREC (PD,NFREQ-1)
      II = 0
      CALL UNPACK (*28,PD,CORE)
      II1 = II
      JJ1 = JJ
   22 CALL PACK (CORE,SCR1,MCB)
      GO TO 10
   28 CORE(   1) = 0
      CORE(IC+2) = 0
      CORE(IC+3) = 0
      CORE(IC+4) = 0
      II1 = 1
      JJ1 = 1
      GO TO 22
   10 CONTINUE
      CALL WRTTRL (MCB)
      CALL CLOSE (PD,1)
      CALL CLOSE (SCR1,1)
C
C     SET UP FOR GFBS
C
      FB(1) = SCR1
   30 FB(2) = NLOAD
      CALL WRTTRL (FB)
      PREC = 1
      IF (FB(5).EQ.2 .OR. FB(5).EQ.4) PREC = 2
      ISIGN = 1
      IF (FU(1) .LT. 0) GO TO 40
      CALL GFBS (CORE,CORE)
      CALL WRTTRL (FX)
      GO TO 98
C
C     SET UP FOR FBS
C
   40 DO 41 I = 1,7
      MFL(I) = FL(I)
C
C     FBS DOES NOT USE THE MATRIX CONTROL BLOCK MFLT.
C     IF MFLT(1) EXISTS, SET ISCRX = MFLT(1) FILE FOR NEW FBS METHOD.
C     OTHERWISE SET ISCRX = 0, AND WE DO NOT HAVE A SCRATCH FILE FOR
C     NEW FBS. OLD FBS WILL BE USED.
C
      MFB(I) = FB(I)
      MFX(I) = FX(I)
   41 CONTINUE
      MPREC = PREC
      MSIGN = ISIGN
      MX    = KORSZ(MCORE)
      ISCRX = MFLT(1)
      MCORE(1) = MFLT(1)
      CALL RDTRL (MCORE(1))
      IF (MCORE(1) .LE. 0) ISCRX = 0
      CALL FBS (MCORE,MCORE)
      CALL WRTTRL (MFX)
   98 ICORE(1) = 16777215
C                16777215 = '00FFFFFF'X
      IFLAG = 1
C
C     STACK LOADS ONTO UDV
C
      FILE = UDV
      NZ   = NX-SYSBUF
      IF (IFR .EQ. 1) GO TO 300
      CALL OPEN (*900,UDV,CORE(NZ+1),0)
      FX(1) = UDV
      CALL RDTRL (FX)
      IF (MACH .NE. 1) GO TO 60
   50 CALL FWDREC (*51,UDV)
      GO TO 50
   51 CALL BCKREC (UDV)
      CALL SKPREC (UDV,1)
      GO TO 61
   60 CALL SKPFIL (UDV,1)
      CALL SKPFIL (UDV,-1)
   61 CALL CLOSE (UDV,2)
      CALL OPEN (*900,UDV,CORE(NZ+1),3)
C
C     RESET TYPE FLAGS
C
      IT1 = FX(5)
      IT2 = IT1
      IT3 = IT1
      IF (IGOOD .EQ. 1) GO TO 101
      NZ  = NZ - SYSBUF
      CALL GOPEN (SCR2,CORE(NZ+1),0)
  101 DO 55 I = 1,NLOAD
      IF (IGOOD .EQ. 1) GO TO 54
      II  = 0
      CALL UNPACK (*54,SCR2,CORE)
      II1 = II
      JJ1 = JJ
   53 CALL PACK (CORE,UDV,FX)
      GO TO 55
   54 CORE(   1) = 0
      CORE(IC+2) = 0
      CORE(IC+3) = 0
      CORE(IC+4) = 0
      II1 = 1
      JJ1 = 1
      GO TO 53
   55 CONTINUE
      CALL CLOSE (UDV,1)
      IF (IGOOD .EQ. 1) GO TO 56
      CALL CLOSE (SCR2,1)
   56 CONTINUE
      CALL WRTTRL (FX)
  350 RETURN
C
  300 IF (IGOOD .NE. 1) GO TO 350
      CALL GOPEN (UDV,CORE(NZ+1),1)
      FX(2) = 0
      FX(6) = 0
      FX(7) = 0
      CALL WRTTRL (FX)
      GO TO 101
C
C     ERROR MESAGES
C
  900 CALL MESAGE (-1,FILE,NAME)
C
C
      ENTRY FRRD1E (UDV1,UDV,NLOAD,NFREQ)
C     ===================================
C
      NZ = KORSZ(CORE) - SYSBUF
C
C     ROUTINE REORDERS SOLUTIONS TO GET SORT BY LOADS
C
      FILE = UDV1
      CALL OPEN (*900,UDV1,CORE(NZ+1),0)
      NZ   = NZ - SYSBUF
      CALL GOPEN (UDV,CORE(NZ+1),1)
      FILE = UDV1
      DO 400 I = 1,NLOAD
      CALL SKPREC (UDV1,I)
      DO 500 M = 1,NFREQ
      II  = 0
      CALL UNPACK (*420,UDV1,CORE)
      II1 = II
      JJ1 = JJ
  421 CALL PACK (CORE,UDV,MCB)
      GO TO 422
  420 CORE(   1) = 0
      CORE(IC+2) = 0
      CORE(IC+3) = 0
      CORE(IC+4) = 0
      II1 = 1
      JJ1 = 1
      GO TO 421
  422 IF (M .LT. NFREQ) CALL SKPREC (UDV1,NLOAD-1)
  500 CONTINUE
      CALL REWIND (UDV1)
  400 CONTINUE
      CALL CLOSE (UDV1,1)
      CALL CLOSE (UDV,1)
      FX(1) = UDV1
      CALL RDTRL (FX)
      FX(1) = UDV
      CALL WRTTRL (FX)
      RETURN
      END
