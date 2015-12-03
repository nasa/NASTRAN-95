      DOUBLE PRECISION FUNCTION DK89(I,A,B,M,N,X)
      DOUBLE PRECISION F89, A, B, X, CAPX, AMF, AN1, AN2, S, SF, AMMSF
      DOUBLE PRECISION AMN1F, ANM1F
      DIMENSION X(1)
      F89 = 0.0D0
      CAPX = A + B * X(I)
      NFAC = M
      ASSIGN 5 TO IRET
      GO TO 1000
    5 AMF = KFAC
      N1 = M + 1
      N2 = N1 - N
      AN1 = N1
      AN2 = N2
      IS = 0
      S = 0.0D0
      SF = 1.0D0
      AMMSF = AMF
      GO TO 50
   10 IS = IS + 1
      S = IS
      SF = SF * S
      AMMSF = AMMSF / (AN1 - S)
   50 CONTINUE
      N3 = N2 - IS
      IF (N3 .EQ. 0) GO TO 100
      F89 = F89 + AMF *((-A)** IS) * (CAPX ** N3) / (AMMSF * SF *
     1            (AN2 - S))
      GO TO 200
  100 CONTINUE
      NFAC = N2
      ASSIGN 110 TO IRET
      GO TO 1000
  110 AMN1F = KFAC
      NFAC = N-1
      ASSIGN 120 TO IRET
      GO TO 1000
  120 ANM1F = KFAC
      F89 = F89 + AMF *((-A)** N2) *DLOG(DABS(CAPX)) / (AMN1F * ANM1F)
  200 IF (IS .LT. M) GO TO 10
      IF( B .EQ. 0.0D0 ) GO TO 300
      F89 = F89 / (B ** N1)
      DK89 = F89
      RETURN
  300 DK89 = 0.0D0
      RETURN
 1000 KFAC = 1
      IF(NFAC.LT.2) GO TO 1020
      DO 1010 LFAC=2,NFAC
      KFAC=KFAC*LFAC
 1010 CONTINUE
 1020 GO TO IRET,(5,110,120)
      END
