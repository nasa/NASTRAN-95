      FUNCTION F89 (I,A,B,M,N,X)
C
      DIMENSION X(1)
C
      F89  = 0.0
      CAPX = A + B*X(I)
      NFAC = M
      ASSIGN 5 TO IRET
      GO TO 1000
    5 AMF = IFAC
      N1  = M + 1
      N2  = N1 - N
      AN1 = N1
      AN2 = N2
      IS  = 0
      S   = 0.0
      SF  = 1.0
      AMMSF = AMF
      GO TO 50
   10 IS = IS + 1
      S  = IS
      SF = SF*S
      AMMSF = AMMSF/(AN1-S)
   50 CONTINUE
      N3 = N2 - IS
      IF (N3 .EQ. 0) GO TO 100
      F89 = F89 + AMF*((-A)**IS)*(CAPX**N3)/(AMMSF*SF*(AN2-S))
      GO TO 200
  100 CONTINUE
      NFAC = N2
      ASSIGN 110 TO IRET
      GO TO 1000
  110 AMN1F = IFAC
      NFAC  = N - 1
      ASSIGN 120 TO IRET
      GO TO 1000
  120 ANM1F = IFAC
      F89 = F89 + AMF*((-A)**N2)*ALOG(ABS(CAPX))/(AMN1F*ANM1F)
  200 IF (IS .LT.  M) GO TO 10
      IF (B .EQ. 0.0) GO TO 300
      F89 = F89/(B**N1)
      RETURN
C
  300 F89 = 0.0
      RETURN
C
 1000 IFAC = 1
      IF (NFAC .LT. 2) GO TO 1020
      DO 1010 LFAC = 2,NFAC
      IFAC = IFAC*LFAC
 1010 CONTINUE
 1020 GO TO IRET, (5,110,120)
      END
