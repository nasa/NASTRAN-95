      SUBROUTINE MBBSLJ (ARG,N,BSL)
C
C     SUBROUTINE TO COMPUTE EVEN ORDERED BESSEL FUNCTIONS OF FIRST KIND
C
C     UNDERFLOW MAY OCCUR IN THIS ROUTINE. THE RESULTS ARE NOT AFFECTED
C
      DIMENSION       BSL(4)
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /XMSSG / UFM,UWM,UIM,SFM
      COMMON /SYSTEM/ SYSBUF,N6
C
      DO 1 I = 1,20
    1 BSL(I) = 0.0
      ASQ = ARG**2
      IF (ASQ .LT. 0.01) GO TO 60
      N  = AMIN1(17.0,(ARG+10.0))
      F  = 2*N + 4
      BSL(N+3) = 0.0
      PF = (4.0*F*(F-1.0)/ASQ-(F-1.0)/F)*0.3
      IF (PF .LE. 1.E-08) GO TO 70
      BSL(N+2) = PF*1.E-30
      PF = 0.0
      J  = N + 1
      DO 10 I = 1,J
      M  = N - I + 2
      F  = 2*M + 1
      BSL(M) = ((4.*(F-1.)/ASQ-1./F-1./(F-2.))*BSL(M+1)-BSL(M+2)/F)*
     1         (F-2.0)
   10 PF = PF + 2.0*BSL(M+1)
      PF = PF + BSL(1)
      F  = 0.0
      IF (ABS(PF) .LE. 1.0) GO TO 20
      F  = ABS(PF)*1.E-10
   20 N  = N + 2
      DO 40 I = 1,N
      IF (F .GE. ABS(BSL(I))) BSL(I) = 0.0
      BSL(I) = BSL(I)/PF
   40 CONTINUE
      M  = N
      DO 50 I = 1,M
      IF (ABS(BSL(N)) .GT. 1.0E-07) RETURN
      N  = N - 1
   50 CONTINUE
      RETURN
C
   60 BSL(2) = 0.125*ASQ
      BSL(1) = 1.0 - 2.0*BSL(2)
      N  = 2
      GO TO 90
C
   70 CALL PAGE2 (3)
      WRITE  (N6,80) SFM,ARG
   80 FORMAT (A25,' 2435, MBBSLJ SUBROUTINE FAILED BECAUSE THE ARGUMEN',
     1       'T IS TOO LARGE FOR THE BSL ARRAY', /5X,'ARG =',1P,E13.5)
      CALL MESAGE (-61,0,0)
   90 RETURN
      END
