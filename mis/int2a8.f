      SUBROUTINE INT 2 A8 (*,X,A8)
C
      INTEGER         JX,       POWER,    A8(2)
      REAL            RX,       X(1)
      CHARACTER*1     A(10),    IP,       IM,       IB,       PT,
     1                ALP(10)
      CHARACTER*8     K8(1),    TEMP,     ZERO,     ZEROX
      CHARACTER*10    ALP10,    TEMP10
      COMMON /MACHIN/ MACH
      COMMON /SYSTEM/ DUMMY(38),NBPC,     NBPW,     NCPW
      EQUIVALENCE     (TEMP,TEMP10,A(1)), (JX,RX),  (ALP10,ALP(1))
      DATA IP,  IM,  IB,  PT,  TEMP, ZERO, ZEROX, NN, LL, ALP10        /
     1     '+', '-', ' ', '.', 'T',  '0',  '0.0', 0,  0,  '1234567890' /
C
C     THESE ROUTINES ENCODE AN INTEGER OR F.P. NUMBER IN X, TO AN 8-BYTE
C     BCD WORD IN A8, OR AN 8-CHARACTER WORD IN K8, LEFT ADJUSTED.
C     WITH MAXIMUM NUMBERS OF DIGITS SQUEEZED INTO THE 8-BYTE FORMAT.
C
C     ENTRY POINT    INT 2 A8  (INTEGER-BCD VERSION)
C                    INT 2 K8  (INTEGER-CHARACTER VERSION)
C                    FP  2 A8  (REAL-BCD VERSION)
C                    FP  2 K8  (REAL-CHARACTER VERSION)
C
C     WRITTEN BY G.CHAN/UNISYS IN AUG. 1985
C     PARTICULARLY FOR XREAD ROUTINE, IN SUPPORT OF ITS NEW FREE-FIELD
C     INPUT FORMAT.
C     THIS ROUTINE IS MACHINE INDEPENDENT
C
      NT = +1
      GO TO 100
C
      ENTRY INT2K8 (*,X,K8)
C     =====================
C
      NT = -1
      GO TO 100
C
      ENTRY FP2A8 (*,X,A8)
C     ====================
C
      NT = +2
      GO TO 100
C
      ENTRY FP 2 K8 (*,X,K8)
C     ======================
C
      NT = -2
C
 100  INT = IABS(NT)
      DO 110 J = 1,8
 110  A(J) = IP
      A( 9) = IB
      A(10) = IB
      IF (INT .NE. 1) GO TO 200
C
C     INTEGER
C
      LU = 8
      N  = 0
      RX = X(1)
      IX = IABS(JX)
      XLL = FLOAT(IX) + .01
      ABSX = ABS(XLL)
      NN = 0
      IF (JX.GE.0 .AND. IX.LT.10**8) GO TO 140
      IF (JX.LT.0 .AND. IX.LT.10**7) GO TO 140
      RETURN 1
 140  IF (JX) 210,150,220
C
 150  TEMP = ZERO
      GO TO 310
 160  TEMP = ZEROX
      GO TO 310
C
C     F.P. NUMBER
C
 200  ABSX = ABS(X(1))
      IF (ABSX .LT. 1.E-20) GO TO 160
      ABSX = ABSX*(1.0+1.E-20)
      LU = 7
      LL =-3
      N  = 0
      IF (X(1) .GT. 0.) GO TO 220
      LU = LU - 1
      LL = LL + 1
 210  N  = 1
      A(1) = IM
 220  N1 = N
      IF (INT .EQ. 1) GO TO 240
      XLL = ALOG10(ABSX)
      IF (XLL .LT. 0.) XLL = XLL - .99998
      IF (XLL .GT. 0.) XLL = XLL + .00002
      POWER = IFIX(XLL)
      NP1 = POWER + 1
      IP1 = IABS(NP1)
      XLU = 10.**LU
      XLL = 10.**LL
      IF (ABSX.LT.XLL .OR. ABSX.GT.XLU) GO TO 400
C
C     F.P. NUMBER IS SQUEEZED INTO AN EIGHT DIGIT F FORMAT, IF
C     X IS BETWWEN 10**-3 AND 10**7 AND X IS POSITUVE, OR
C          BETWWEN 10**-2 AND 10**6 AND X IS NEGATIVE,
C
 230  IF (IP1 .GE. 10) LU = LU - 1
      IF (NP1 .EQ. -1) LU = LU + 1
      NN = LU - NP1
      IF (INT.EQ.2 .AND. NN.GT.7) NN = 7
      IX = IFIX(ABSX*10.**NN)
 240  LU = LU - 1
      IF (LU.LT.0 .AND. INT.EQ.3) GO TO 420
      IF (LU.LT.0 .AND.   N.EQ.7) GO TO 260
      POWER = 10**LU
      IF (POWER .EQ. 0) POWER = 1
      J  = IX/POWER
      IF (J .GE. 10) GO TO 240
      IX = MOD(IX,POWER)
      IF (LU-NN+1) 280,250,270
 250  IF (INT .EQ. 3) GO TO 420
 260  N  = N + 1
      A(N) = PT
      IF (N .GE. 8) GO TO 290
 270  IF (J.EQ.0 .AND. N.LE.N1) GO TO (240,280,280), INT
 280  IF (J .EQ. 0) J = 10
      N  = N + 1
      A(N) = ALP(J)
      IF (LU.EQ.0 .AND. INT.EQ.1) GO TO 350
      IF (N .LT. 8) GO TO 240
 290  DO 300 J = 1,8
      IF (A(N) .EQ. PT) GO TO 310
      IF (A(N) .NE. ALP(10)) GO TO 310
      A(N) = IB
 300  N  = N - 1
C
 310  IF (NT) 320,440,330
 320  K8(1) = TEMP
      GO TO 440
 330  IF (MACH .NE. 4) CALL KHRBC2 (TEMP,A8(1))
CWKBD IF (MACH .EQ. 4) A8(1) = ISWAP(TEMP10)
C     IF (NCPW .GE. 8) A8(2) = LSHIFT(A8(1),4*NBPC)
      GO TO 440
C
 350  N = N + 1
      IF (N .GT. 8) GO TO 310
      DO 360 J = N,8
 360  A(J) = IB
      GO TO 310
C
C     F.P. NUMBER IN .XXXXX+X, .XXXX-XX, -.XXXX-X, OR -.XXX+XX FORMS
C     FOR MAXIMUM NOS. OF DIGITS POSSIBLE IN AN A8 WROD.
C
 400  INT = 3
      N   = N + 1
      A(N)= PT
      LU  = LU - 2
      GO TO 230
C
 420  N = N + 1
      IF (NP1 .GE. 0) A(N) = IP
      IF (NP1 .LT. 0) A(N) = IM
      IF (IP1 .GE. 10) GO TO 430
      A(N+1) = ALP(IP1)
      GO TO 310
 430  J = IP1/10
      A(N+1) = ALP(J)
      J = MOD(IP1,10)
      IF (J .EQ. 0) J = 10
      A(N+2) = ALP(J)
      GO TO 310
C
 440  RETURN
      END
