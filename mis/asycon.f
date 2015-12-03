      SUBROUTINE ASYCON
C
C     SUBROUTINE FOR COMPUTING CONSTANT TERM IN KAPPA MINUS
C
      COMPLEX         BSYCON,AI,C1,C1TEST,ALP,ALN,ARAT1,ARAT2
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /BLK2  / BSYCON
      COMMON /SYSTEM/ SYSBUF,IBBOUT
      COMMON /BLK1  / SCRK,SPS,SNS,DSTR,AI,PI,DEL,SIGMA,BETA,RES
C
      C1   = 1.0
      PI2  = 2.0*PI
      A1   = PI2/(SPS-SNS)
      GAM0 = SPS*DEL - SIGMA
      A2   =-A1
      B1   = GAM0/(SPS-SNS)
      S1   = SPS/(DSTR**2)
      S2   = SNS/DSTR
      C1TEST = 0.0
      DO 10 I = 1,200
      R    = I
      GAMP = PI2*R + GAM0
      GAMN =-PI2*R + GAM0
      C2P  = GAMP/DSTR - SCRK
      C2Q  = GAMP/DSTR + SCRK
      C2N  = GAMN/DSTR - SCRK
      C3Q  = GAMN/DSTR + SCRK
      NN   = 0
      CSEC = C2P*C2Q
      IF (CSEC .LT. 0.0) NN = 1
      T1   = GAMP*S1
      T2   = S2*SQRT(ABS(CSEC))
      IF (C2P.LT.0.0 .AND. C2Q.LT.0.0) T2 =-T2
      IF (NN .EQ. 0) ALP = T1 + T2
      IF (NN .EQ. 1) ALP = CMPLX(T1,T2)
      NN   = 0
      CSEC = C2N*C3Q
      IF (CSEC .LT. 0.0) NN = 1
      T1   = GAMN*S1
      T2   = S2*SQRT(ABS(CSEC))
      IF (C2N.LT.0.0 .AND. C3Q.LT.0.0) T2 =-T2
      IF (NN .EQ. 0) ALN = T1 + T2
      IF (NN .EQ. 1) ALN = CMPLX(T1,T2)
      ARAT1 = (A1*R+B1)/ALP
      ARAT2 = (A2*R+B1)/ALN
      C1    = C1*ARAT1*ARAT2
      IF (CABS((C1-C1TEST)/C1) .LT. 0.0001)  GO TO 60
      C1TEST = C1
   10 CONTINUE
      GO TO 9999
   60 CONTINUE
      BSYCON = C1
      RETURN
C
 9999 WRITE  (IBBOUT,1000) UFM
 1000 FORMAT (A23,' - AMG MODULE - SUBROUTINE ASYCON')
      CALL MESAGE (-61,0,0)
      RETURN
      END
