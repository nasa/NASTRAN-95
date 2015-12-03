      SUBROUTINE INCRO (AX,AY,AZ,AX1,AY1,AZ1,AX2,AY2,AZ2,SGR,CGR,SGS,
     1                  CGS,KR,FL,BETA,SDELX,DELY,DELR,DELI)
C
C     CALCULATES THE UNSTEADY PART OF THE INFLUENCE COEFFICIENT MATRIX
C     ELEMENTS USING  SUBROUTINES  KERNEL, IDF1  AND  IDF2
C
      REAL         K10,K20,K1RT1,K1IT1,K2RT2P,K2IT2P,K10T1,K20T2P,KR,M
      COMMON /DLM/ K10,K20,K1RT1,K1IT1,K2RT2P,K2IT2P,K10T1,K20T2P
      COMMON /KDS/ IND
C
C     DKRO = REAL PART OF THE PLANAR KERNEL  *  OUTBOARD POINT
C     DKIO = IMAGINARY PART OF THE PLANAR KERNEL  *  OUTBOARD POINT
C     XKRO = REAL PART OF THE NONPLANAR KERNEL  *  OUTBOARD POINT
C     XKIO = IMAGINARY PART OF THE NONPLANAR KERNEL  *  OUTBOARD POINT
C     DKRI = REAL PART OF THE PLANAR KERNEL  *   INBOARD POINT
C     DKII = IMAGINARY PART OF THE PLANAR KERNEL  *   INBOARD POINT
C     XKRI = REAL PART OF THE NONPLANAR KERNEL  *   INBOARD POINT
C     XKII = IMAGINARY PART OF THE NONPLANAR KERNEL  *   INBOARD POINT
C
      IND   = 1
      M     = SQRT(1.0 - BETA**2)
      BR    = FL/2.
      EPS   = 0.00001
      PI    = 3.14159265
      XDELX = SDELX
      XDELY = DELY
      EE    = 0.5*XDELY
      E2    = EE**2
      DELR  = 0.0
      DELI  = 0.0
      AT1S  = 0.0
      AT2S  = 0.0
      T1    = 0.0
      T2    = 0.0
      COUNT = 0.
      X0    = AX
      Y0    = AY
      Z0    = AZ
   80 CONTINUE
      CALL TKER (X0,Y0,Z0,KR,BR,SGR,CGR,SGS,CGS,T1,T2,M)
      AT1   = ABS(T1)
      AT2   = ABS(T2)
      IF (AT1 .GT. AT1S) AT1S = AT1
      IF (AT2 .GT. AT2S) AT2S = AT2
      IF (COUNT) 130,90,150
   90 DKRC  = K1RT1 - K10T1
      DKIC  = K1IT1
      XKRC  = K2RT2P - K20T2P
      XKIC  = K2IT2P
      AT2   = ABS(T2)
      COUNT = -1.
      X0    = AX1
      Y0    = AY1
      Z0    = AZ1
      GO TO 80
  130 DKRI  = K1RT1 - K10T1
      DKII  = K1IT1
      XKRI  = K2RT2P - K20T2P
      XKII  = K2IT2P
      COUNT = 1.
      X0    = AX2
      Y0    = AY2
      Z0    = AZ2
      GO TO 80
  150 DKRO  = K1RT1 - K10T1
      DKIO  = K1IT1
      XKRO  = K2RT2P - K20T2P
      XKIO  = K2IT2P
      X0    = AX
      Y0    = AY
      Z0    = AZ
      ZERO  = 0.0
      XIIJR = 0.
      XIIJI = 0.
      DIIJR = 0.0
      DIIJI = 0.0
      XMULT = XDELX/(8.0*PI)
      IF (Y0.EQ.ZERO .AND.  Z0.EQ.ZERO) GO TO 220
      IF (Z0.EQ.ZERO .AND. SGS.EQ.ZERO) GO TO 230
      ETA01 = Y0*CGS + Z0*SGS
      ZET01 =-Y0*SGS + Z0*CGS
      AZET0 = ABS(ZET01)
      IF (AZET0 .LE. 0.0001) ZET01 = 0.
      R1SQX = ETA01**2 + ZET01**2
  210 ARE   = (DKRI - 2.*DKRC + DKRO)/(2.0*E2)
      AIM   = (DKII - 2.*DKIC + DKIO)/(2.0*E2)
      BRE   = (DKRO - DKRI)/(2.0*EE)
      BIM   = (DKIO - DKII)/(2.0*EE)
      CRE   =  DKRC
      CIM   =  DKIC
      GO TO 250
  220 ETA01 = 0.0
      ZET01 = 0.0
      R1SQX = 0.0
      GO TO  210
  230 ETA01 = Y0*CGS
      ZET01 = 0.
      R1SQX = ETA01**2
      GO TO  210
  250 CONTINUE
      IF (AT1S .EQ. 0.0) GO TO 255
      CALL IDF1 (EE,E2,ETA01,ZET01,ARE,AIM,BRE,BIM,CRE,CIM,R1SQX,XIIJR,
     1           XIIJI)
      DELR  = XMULT*XIIJR
      DELI  = XMULT*XIIJI
  255 CONTINUE
      IF (AT2S .EQ. 0.0) GO TO 260
      A2R   = (XKRI - 2.0*XKRC + XKRO)/(2.0*E2)
      A2I   = (XKII - 2.0*XKIC + XKIO)/(2.0*E2)
      B2R   = (XKRO - XKRI)/(2.0*EE)
      B2I   = (XKIO - XKII)/(2.0*EE)
      C2R   =  XKRC
      C2I   =  XKIC
      CALL IDF2 (EE,E2,ETA01,ZET01,A2R,A2I,B2R,B2I,C2R,C2I,R1SQX,DIIJR,
     1           DIIJI)
      DELR  = DELR + XMULT*DIIJR
      DELI  = DELI + XMULT*DIIJI
  260 CONTINUE
C
      RETURN
      END
