      SUBROUTINE FRD2I (FL,NFREQ,NCORE,QHHL,SCR2,SCR1,SCR3,SCR4,NROW)
C
      INTEGER         QHHL,SCR1,SCR2,SCR3,SCR4,TRL(7),OUT
      DIMENSION       FL(1),MCB(7),NAME(2)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /BLANK / BOV,Q,RM
      COMMON /CONDAS/ PI,TWOPI
      COMMON /SYSTEM/ ISYS,OUT,DUM(52),IPREC
      COMMON /UNPAKX/ IOUT,INN,NNN,INCR1
      COMMON /PACKX / ITI,ITO,II,NN,INCR
      COMMON /TYPE  / P(2),IWC(4)
      DATA    NAME  / 4HFRD2,4HI   /
      DATA    NHFRDI/ 4HFRDI/
C
      IBUF1 = NCORE - ISYS
      IBUF2 = IBUF1 - ISYS
      NROW  = 0
      INCR  = 1
      INCR1 = 1
      II    = 1
      INN   = 1
      MCB(1)= QHHL
      CALL RDTRL (MCB)
      IF (MCB(1) .LT. 0) GO TO 1000
      NROW  = MCB(3)
      NI    =(MCB(2)/MCB(3))*2
      NNN   = NROW
      NN    = NROW*NROW
      ITI   = 3
      ITO   = ITI
      IOUT  = ITI
      NWC   = IWC(ITI)
      ISCR  = SCR1
      NLOOP = 1
      INDX  = 0
      XM    = RM
      IF (RM .GE. 0.0) GO TO 5
      ISCR  = SCR2
      NLOOP = NFREQ
      INDX  = 1
    5 CALL MAKMCB (TRL,ISCR,NN,MCB(4),ITO)
C
C     MAKE INDEPENDENT FREQ LIST
C
      IPD   = 1
      NL    = 2*NFREQ
      N     = NFREQ + 1
      ICORE = IBUF1
      IPI   = IPD + NL
      DO 10 I = 1,NFREQ
      FL(NL) = FL(N-I)*TWOPI*BOV
      FL(NL-1) = 0.0
      NL = NL -2
   10 CONTINUE
C
C     MAKE INDEPENDENT FREQ LIST
C
      CALL OPEN  (*1000,QHHL,FL(IBUF2),0)
      CALL GOPEN (ISCR,FL(IBUF1),1)
      CALL READ  (*999,*999,QHHL,FL(IPI),-3,0,FLAG)
      CALL READ  (*999,*999,QHHL,N,1,0,FLAG)
      N = N + N
      IF (RM.GE.0.0 .OR. N.EQ.NI) GO TO 15
      WRITE  (OUT, 2000) UFM,N,NI
 2000 FORMAT (A23,', THE NUMBER OF (M,K) PAIRS SPECIFIED ON MKAEROX ',
     1       'CARDS (', I5, ') IS NOT EQUAL ', /5X,
     2       'TO THE NUMBER OF FREQUENCIES SPECIFIED (', I5, '),')
      CALL MESAGE (-37,0,NAME)
  15  NI = MIN0(NI,N)
      CALL READ (*999,*999,QHHL,FL(IPI),NI,1,FLAG)
      IF (RM .LT. 0.0) CALL CLOSE (QHHL, 1)
C
      DO 200 KKK = 1, NLOOP
      IF (RM .GE. 0.0) GO TO 20
      XM = FL(2*KKK)
      CALL GOPEN (QHHL,FL(IBUF2),0)
   20 CONTINUE
C
C     FOR RM.GE.0.0, FIND M CLOSEST TO XM
C     FOR RM.LT.0.0, FIND K CLOSEST TO XM
C
      ICP = IPI + NI
      RMI = 1.E20
      RMS = 0.0
      DO 30 I = 1,NI,2
      RMX = ABS(FL(IPI+I+INDX-1)-XM)
      RMI = AMIN1(RMI,RMX)
      IF (RMX .GT. RMI) GO TO 30
      RMS = FL(IPI+I+INDX-1)
   30 CONTINUE
      RMI = RMS
C
C     FOR RM.GE.0.0, SELECT ALL K'S ASSOCIATED WITH RMI
C     FOR RM.LT.0.0, SELECT THE K EQUAL TO RMI
C
      K = 0
      DO 100 I = 1,NI,2
      IF (FL(IPI+I+INDX-1) .EQ. RMI) GO TO 120
C
C     SKIP MATRIX
C
      CALL SKPREC (QHHL,NROW)
      GO TO 100
C
C     MAKE MATRIX INTO COLUMN
C
  120 FL(IPI+K+1) = FL(IPI+I)
      K  = K + 2
      JI = ICP
      N  = NROW*NWC
      DO 130 J = 1,NROW
      CALL UNPACK (*131,QHHL,FL(JI))
      GO TO 135
  131 CALL ZEROC (FL(JI),N)
  135 JI = JI + N
  130 CONTINUE
C
C     DIVIDE IMAG PART OF QHHL BY FREQUENCY
C
      JJ = ICP + 1
      KK = JI  - 1
      DO 132 J = JJ,KK,2
      FL(J) = FL(J)/FL(IPI+I)
  132 CONTINUE
      IF (RM .LT. 0.0) FL(IPI+I) = -10000.0
      CALL PACK (FL(ICP),ISCR,TRL)
      IF (RM .LT. 0.0) GO TO 150
  100 CONTINUE
  150 CALL CLOSE (QHHL,1)
      CALL CLOSE (ISCR,1)
  200 CONTINUE
C
      CALL WRTTRL (TRL)
      CALL BUG (NHFRDI,200,K ,1)
      CALL BUG (NHFRDI,200,NFREQ,1)
      CALL BUG (NHFRDI,200,FL(1),ICP)
      IF (RM .LT. 0.0) RETURN
C
C     SETUP TO CALL MINTRP
C
      NI   = K/2
      NOGO = 0
      NC   = NCORE - ICP
      CALL DMPFIL (-SCR1,FL(ICP),NC)
      IM   = 0
      IK   = 1
      CALL MINTRP (NI,FL(IPI),NFREQ,FL(IPD),-1,IM,IK,0.0,SCR1,SCR2,SCR3,
     1             SCR4,FL(ICP),NC,NOGO,IPREC)
      IF (NOGO .EQ. 1) GO TO 998
      CALL DMPFIL (-SCR2,FL(ICP),NC)
      RETURN
C
  998 WRITE  (OUT,9980) UFM
 9980 FORMAT (A23,' 2271, INTERPOLATION MATRIX IS SINGULAR')
      GO TO 9999
  999 CALL MESAGE (-3,QHHL,NAME)
 9999 CALL MESAGE (-61,0,NAME)
 1000 CALL CLOSE (QHHL,1)
      RETURN
      END
