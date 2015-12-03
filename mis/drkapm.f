      SUBROUTINE DRKAPM (ARG,INDX,RESLT)
C
C     THIS SUBROUTINE COMPUTES THE DERVIATIVE OF KAPPA MINUS
C
      COMPLEX         AI,ARG,RESLT,BSYCON,C1,C2,C2TEST,AT2,AT3,ALP0,
     1                ALP,ALN
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ SYSBUF,IBBOUT
      COMMON /BLK1  / SCRK,SPS,SNS,DSTR,AI,PI,DEL,SIGMA,BETA,RES
      COMMON /BLK2  / BSYCON
C
      PI2  = 2.0*PI
      A1   = PI2/(SPS-SNS)
      A2   =-A1
      GAM0 = SPS*DEL - SIGMA
      B1   = GAM0/(SPS-SNS)
      C1   = CEXP(-AI*ARG/2.0*(SPS-SNS))
      C2Q  = GAM0/DSTR - SCRK
      C3Q  = GAM0/DSTR + SCRK
      S1   = SPS/(DSTR**2)
      S2   = SNS/DSTR
      NN   = 0
      CSEC = C2Q*C3Q
      IF (CSEC .LT. 0.0) NN = 1
      T1   = GAM0*S1
      T2   = S2*SQRT(ABS(CSEC))
      IF (C2Q.LT.0.0 .AND. C3Q.LT.0.0) T2 =-T2
      IF (NN .EQ. 0) ALP0 = T1 + T2
      IF (NN .EQ. 1) ALP0 = CMPLX(T1,T2)
      RINDX = INDX
      IF (INDX .EQ. 0) GO TO 10
      C2   = C1*B1/ALP0*CSIN(PI/A1*(ARG-B1))/(A1*RINDX+B1-ARG)*
     1       (1.0+(ALP0-B1)/(B1-ARG))/(SIN(PI*B1/A1))*BSYCON
      GO TO 20
   10 CONTINUE
      C2   = C1*B1/ALP0*CSIN(PI/A1*(ARG-B1))/((B1-ALP0)*SIN(PI*B1/A1))*
     1       BSYCON
   20 CONTINUE
      C2TEST = 0.0
      DO 30 I = 1,200
      R    = I
      IF (INDX.LT.0 .AND. ABS(RINDX).EQ.R) GO TO 30
      IF (INDX.GT.0 .AND. RINDX.EQ.R) GO TO 30
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
      C2   = C2*(1.0+AT2)*(1.0+AT3)
      IF (CABS((C2-C2TEST)/C2) .LT. 0.0009) GO TO 40
      C2TEST = C2
   30 CONTINUE
      GO TO 50
   40 CONTINUE
      RESLT = C2
      RETURN
C
   50 CONTINUE
      WRITE  (IBBOUT,60) UFM
   60 FORMAT (A23,' - AMG MODULE -SUBROUTINE DRKAPM')
      CALL MESAGE (-61,0,0)
      RETURN
      END
