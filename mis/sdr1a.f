      SUBROUTINE SDR1A (INPUT,IOUT)
C
C     THIS ROUTINE MAKES PS AND IUF COMPATABLE TO COMPUTE QS IN
C     CASE OF TRANSIENT ANALYSIS
C
      INTEGER        SYSBUF,BCD1(2),MCB(7),PS,IA(7),CORE(1100)
C
      COMMON /ZZZZZZ/ COREX(1)
      COMMON /SYSTEM/ SYSBUF,KSYSTM(65)
      COMMON /BLANK / LOADNN
      COMMON /UNPAKX/ IT1,II,JJ,INCR
      COMMON /PACKX / IT2,IT3,II1,JJ1,INCR1
      EQUIVALENCE     (CORE(1),COREX(1))
C
      DATA    BCD1  / 4HSDR1,4HA    /
C
      NZ = KORSZ(CORE) - SYSBUF
      CALL OPEN (*40,INPUT,CORE(NZ+1),0)
      CALL SKPREC (INPUT,1)
      NZ = NZ - SYSBUF
      LOADNN = MAX0(LOADNN,1)
      IF (LOADNN .EQ. 1) GO TO 50
      IA(1) = IOUT
      CALL RDTRL (IA)
      IF (IA(2) .EQ. 0) GO TO 50
      IA(1) = INPUT
      CALL RDTRL(IA)
      IF(IA(2) .EQ. 0) CALL MESAGE (-7,0,BCD1)
C
C     POSITION TO END
C
      CALL GOPEN  (IOUT,CORE(NZ+1),0)
      CALL SKPFIL (IOUT,+1)
      CALL SKPFIL (IOUT,-1)
      CALL CLOSE  (IOUT,+2)
      IA(1) = IOUT
      CALL RDTRL (IA)
      IA(7) = 0
      CALL GOPEN (IOUT,CORE(NZ+1),3)
   10 MCB(1) = INPUT
      CALL RDTRL (MCB)
      K   = MCB(2)
      IT1 = MCB(5)
      IT2 = IT1
      IT3 = IT2
      INCR  = 1
      INCR1 = 1
      DO 30 I = 1,K
      II = 0
      CALL UNPACK (*20,INPUT,CORE)
      II1 = II
      JJ1 = JJ
      CALL PACK (CORE,IOUT,IA)
      GO TO 30
   20 II1 = 1
      JJ1 = 1
      CORE(1) = 0
      CORE(2) = 0
      CORE(3) = 0
      CORE(4) = 0
      CALL PACK (CORE,IOUT,IA)
   30 CONTINUE
      CALL CLOSE (INPUT,1)
      CALL CLOSE (IOUT,1)
      CALL WRTTRL (IA)
   40 RETURN
C
C     FIRST TIME
C
   50 CALL GOPEN (IOUT,CORE(NZ+1),1)
      IA(1) = INPUT
      CALL RDTRL (IA)
      IA(2) = 0
      IA(6) = 0
      IA(7) = 0
      IA(1) = IOUT
      GO TO 10
C
C     SDR1D -
C
      ENTRY SDR1D (PS,IUF,IUF1,ITRAN)
C     ===============================
C
      IF (ITRAN .EQ. 0) GO TO 60
      ITRAN  = 1
      MCB(1) = PS
      CALL RDTRL (MCB)
      IF (MCB(1) .LE. 0) GO TO 100
      NCOLPS = MCB(2)
      MCB(1) = IUF
      CALL RDTRL (MCB)
      IF (NCOLPS .EQ. MCB(2)) RETURN
C
C     THIS IS A TRANSIENT PROBLEM
      ITRAN = 0
C
   60 MCB(1) = IUF
      CALL RDTRL (MCB)
      NCOLPS = MCB(2)/3
      IBF = KORSZ(CORE) - SYSBUF
      CALL GOPEN (IUF,CORE(IBF),0)
      IBF1 = IBF - SYSBUF
      CALL GOPEN (IUF1,CORE(IBF1),1)
      IT1  = MCB(5)
      IT2  = IT1
      IT3  = IT2
      INCR = 1
      INCR1  = 1
      MCB(1) = IUF1
      MCB(2) = 0
      MCB(6) = 0
      MCB(7) = 0
      DO 90 I = 1,NCOLPS
      II  = 0
      CALL UNPACK (*70,IUF,CORE)
      II1 = II
      JJ1 = JJ
      GO TO 80
   70 CORE(1) = 0
      CORE(2) = 0
      CORE(3) = 0
      CORE(4) = 0
      II1 = 1
      JJ1 = 1
   80 CALL SKPREC (IUF,2)
      CALL PACK (CORE,IUF1,MCB)
   90 CONTINUE
      CALL CLOSE (IUF1,1)
      CALL CLOSE (IUF,1)
      CALL WRTTRL (MCB)
  100 RETURN
      END
