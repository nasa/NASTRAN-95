      SUBROUTINE VECPRT (*,*,PX,NX,A,OX)
C
      INTEGER         P,PX,O,OX,COUNT,EJECT,PM,TRA,RSP,RDP,CSP,CDP
      DIMENSION       A(NX)
      COMMON /SYSTEM/ SKP1,MO,SKP2(6),MAXLIN,SKP3(2),COUNT
      DATA            RSP,RDP,CSP,CDP / 1,2,3,4 /
C
C     PX = VECTOR TYPE + PRECISION.
C     NX = VECTOR LENGTH.
C     A  = VECTOR LOCATION.
C
C     THE VECTOR COMPONENTS WILL BE PRINTED 6 PER LINE IF REAL OR
C                IMAGINARY, AND 3 PER LINE IF COMPLEX.
C          O = 0 IF ALL THE VECTOR COMPONENTS ARE TO BE PRINTED, AND IF
C                THEY ARE TO BE PRINTED STARTING ON A NEW PAGE IF THEY
C                WILL NOT FIT ON THE CURRENT PAGE.
C          O = 1 IF ONLY THOSE LINES WHICH HAVE AT LEAST ONE NON-ZERO
C                COMPONENT ARE TO BE PRINTED, AND IF THE VECTOR IS TO BE
C                PRINTED STARTING ON A NEW PAGE IF IT WILL NOT FIT ON
C                THE CURRENT PAGE.
C          O =-1 IF ONLY THOSE LINES WHICH HAVE AT LEAST ONE NON-ZERO
C                COMPONENT ARE TO BE PRINTED, AND IF THE VECTOR IS TO BE
C                PRINTED ON THE CURRENT PAGE UNLESS TWO LINES WILL NOT
C                FIT.
C
C     RETURN 1 - PRINT SUBTITLE + VECTOR IDENTIFICATION.
C     RETURN 2 - PRINT VECTOR IDENTIFICATION ONLY.
C                PRTVEC = RETURN ENTRY POINT.
C
      P = PX
      N = NX
      O = OX
C
      PM = P
      IF (P .EQ. RDP) PM = RSP
      IF (P .EQ. CDP) PM = CSP
      KK = 1
      IF (PM .EQ. CSP) KK = 2
      IF (P.EQ.RDP .OR. P.EQ.CDP) KK = 2*KK
      KN = KK*N
      IF (PM .EQ. CSP) KK = KK/2
      K6 = KK*6
      IF (O .EQ. 0) GO TO 40
C
      M = 1
      DO 30 K = 1,KN,K6
      L = K + K6 - KK
      IF (L .GT. KN)  L = KN
      DO 10 I = K,L,KK
      IF (A(I) .NE. 0.) GO TO 20
   10 CONTINUE
      GO TO 30
   20 M = M + 1
   30 CONTINUE
      IF (M .EQ. 1) GO TO 160
C
      IF (O .LT. 0) M = 2
      GO TO 50
   40 M = (N+5)/6 + 1
      IF (PM .EQ. CSP) M = (N+2)/3 + 2
   50 ASSIGN 60 TO TRA
      IF (EJECT(M)) 170,180,170
   60 COUNT = COUNT - M
      KNKK  = KN/KK
      IF (KNKK .GT. 6) GO TO 70
      CALL FORMAT (A,1,KN,KK,-1,N)
      COUNT = COUNT + 1
      GO TO 140
C
   70 ASSIGN 110 TO TRA
      K = 1
   80 L = K + K6 - KK
      IF (L .GT. KN) L = KN
      IF (O .EQ.  0) GO TO 100
      DO 90 I = K,L,KK
      IF (A(I) .NE. 0.) GO TO 100
   90 CONTINUE
      GO TO 130
  100 IF (EJECT(1) .NE. 0) GO TO 170
  110 K1 = (K + KK - 1)/KK
      K2 = (L + KK - 1)/KK
      IF (PM .NE. CSP) GO TO 120
      K1 = (K1+1)/2
      K2 = K2/2
  120 CALL FORMAT (A,K,L,KK,K1,K2)
  130 K = K + K6
      IF (K .LE. KN) GO TO 80
C
  140 WRITE  (MO,150)
  150 FORMAT (1X)
      COUNT = COUNT + 1
  160 RETURN
C
  170 RETURN 1
  180 RETURN 2
C
C
      ENTRY PRTVEC (*,*)
C     ==================
C
      COUNT = COUNT + 1
      IF (PM .NE. CSP) GO TO 260
      COUNT = COUNT + 1
      IF (KNKK-4) 200,220,240
  200 WRITE  (MO,210)
  210 FORMAT (51X,4HREAL,11X,9HIMAGINARY)
      GO TO 260
  220 WRITE  (MO,230)
  230 FORMAT (21X,2(12X,4HREAL,11X,9HIMAGINARY))
      GO TO 260
  240 WRITE  (MO,250)
  250 FORMAT (3X,3(12X,4HREAL,11X,9HIMAGINARY))
  260 GO TO TRA, (60,110)
      END
