      SUBROUTINE AKAPM (ARG,BKPM)
C
C     SUBROUTINE FOR COMPUTING KAPPA MINUS
C
      COMPLEX         BKPM,C1,AI,C1TEST,BSYCON,ARG,
     1                AT2,AT3,ALP0,ALP,ALN
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ SYSBUF,IBBOUT
      COMMON /BLK1  / SCRK,SPS,SNS,DSTR,AI,PI,DEL,SIGMA,BETA,RES
      COMMON /BLK2  / BSYCON
C
      C1   = CEXP(-AI*ARG/2.0*(SPS-SNS))
      GAM0 = SPS*DEL - SIGMA
      PI2  = 2.0*PI
      S1   = SPS/(DSTR**2)
      S2   = SNS/DSTR
      C2Q  = GAM0/DSTR - SCRK
      C3Q  = GAM0/DSTR + SCRK
      NN   = 0
      CSEC = C2Q*C3Q
      IF (CSEC .LT. 0.0) NN = 1
      T1   = GAM0*S1
      T2   = S2*SQRT(ABS(CSEC))
      IF (C2Q.LT.0.0 .AND. C3Q.LT.0.0) T2 =-T2
      IF (NN .EQ. 0) ALP0 = T1 + T2
      IF (NN .EQ. 1) ALP0 = CMPLX(T1,T2)
      C1   = C1*(1.0-ARG/ALP0)
      A1   = PI2/(SPS-SNS)
      A2   =-A1
      B1   = GAM0/(SPS-SNS)
      C1TEST = 0.0
      DO 20 I = 1,200
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
      AT2  = (ALP-A1*R-B1)/(A1*R+B1-ARG)
      AT3  = (ALN-A2*R-B1)/(A2*R+B1-ARG)
      C1   = C1*(1.0+AT2)*(1.0+AT3)
      IF (CABS((C1-C1TEST)/C1) .LT. 0.0009) GO TO 50
      C1TEST = C1
   20 CONTINUE
      GO TO 70
   50 CONTINUE
      C1   = C1*B1/(ARG-B1)*CSIN(PI/A1*(ARG-B1))/(SIN(PI*B1/A1))
      C1   = C1*BSYCON
      BKPM = C1
      RETURN
C
   70 WRITE  (IBBOUT,80) UFM
   80 FORMAT (A23,' - AMG MODULE -SUBROUTINE AKAPM')
      CALL MESAGE (-61,0,0)
      RETURN
      END
