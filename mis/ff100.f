      FUNCTION FF100(I,A,B,M,N,X)
      DIMENSION  X(1)
      F100 = 0.0
      CAPX = A + B * X(I)
      XX = X(I)
      N1 = M + N - 2
      N2 = M - 1
      N3 = N1 + 1
      AN1 = N1
      AN2 = N2
      NFAC = N1
      ASSIGN 5 TO IRET
      GO TO 1000
    5 AMN2F = IFAC
      AN1P1 = AN1 + 1.0
      IS = 0
      S = 0.0
      SF = 1.0
      AMN2SF = AMN2F
      GO TO 50
   10 IS = IS + 1
      S = IS
      SF = SF * S
      AMN2SF = AMN2SF / (AN1P1 - S)
   50 CONTINUE
      N4 = N2 - IS
      IF (N4 .EQ. 0) GO TO 100
      F100 = F100 + AMN2F * (CAPX ** N4) *((-B)** IS) / (AMN2SF * SF
     1            * (AN2 - S) * (XX ** N4))
      GO TO 200
  100 CONTINUE
      NFAC = N2
      ASSIGN 110 TO IRET
      GO TO 1000
  110 AM1F = IFAC
      NFAC = N-1
      ASSIGN 120 TO IRET
      GO TO 1000
  120 AN1F = IFAC
      F100 = F100 + AMN2F *((-B)** N2) * ALOG(ABS(CAPX/XX))
     1            / (AM1F * AN1F)
  200 CONTINUE
      IF (IS .LT. N1) GO TO 10
      F100 =  -F100 / (A ** N3)
       FF100  = F100
      RETURN
 1000 IFAC = 1
      IF(NFAC.LT.2) GO TO 1020
      DO 1010 LFAC=2,NFAC
      IFAC=IFAC*LFAC
 1010 CONTINUE
 1020 GO TO IRET,(5,110,120)
      END
