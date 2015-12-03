      SUBROUTINE DLKAPM (ARG,BLKAPM)
C
C     SUBROUTINE FOR COMPUTING LOGARITHMIC DERIVATIVE OF KAPPA MINUS
C
      COMPLEX         BLKAPM,AI,C1,D1,D2,C1TEST,ARG,E1,ALP0,ALP,ALN
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ SYSBUF,IBBOUT
      COMMON /BLK1  / SCRK,SPS,SNS,DSTR,AI,PI,DEL,SIGMA,BETA,RES
C
      C1   =-AI/2.0*(SPS-SNS)
      PI2  = 2.0*PI
      S1   = SPS/(DSTR**2)
      S2   = SNS/DSTR
      GAM0 = SPS*DEL - SIGMA
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
      C1   = C1 + 1.0/(ARG-ALP0)
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
      E1   = A1*R + B1 - ARG
      D1   = (ALP-A1*R-B1)/E1
      D2   = D1/E1
      C1   = C1 + 1.0/(1.0+D1)*D2
      E1   = A2*R + B1 - ARG
      D1   = (ALN-A2*R-B1)/E1
      D2   = D1/E1
      C1   = C1 + 1.0/(1.0+D1)*D2
      IF (CABS((C1-C1TEST)/C1) .LT. 0.0006) GO TO 50
      C1TEST = C1
   20 CONTINUE
      GO TO 70
   50 CONTINUE
      E1   = ARG - B1
      B    = PI/A1
      C1   = C1 - 1.0/E1 + B*CCOS(B*E1)/(CSIN(B*E1))
      BLKAPM = C1
      RETURN
C
   70 WRITE  (IBBOUT,80) UFM
   80 FORMAT (A23,' - AMG MODULE -SUBROUTINE DLKAPM')
      CALL MESAGE (-61,0,0)
      RETURN
      END
