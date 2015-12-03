      SUBROUTINE FZY2 (XIJ, X1, X2,ETA,ZETA, YB, ZB, A, BETA2,CBAR, K,
     1           FZZR, FZZI, FZYR, FZYI, FYZR, FYZI, FYYR, FYYI)
C   ***   THIS SUBROUTINE IS AN ALTERNATIVE TO SUBROUTINE  FMZY   ---
C         IT IS USED WHENEVER THE OPTION FLAG   IBFS  =  1
C   ***
      REAL M,K,KBAR,KBAR2,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,KBAR3
      DATA LASTBR /0/
      DATA    TEST1,TEST2,CTH,STH,RAIJ,RAIJ2 /0.142857, 0.5, 1.0,3*0.0/
      DATA    CAPDR,CAPDI,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11/ 13*0.0 /
      M      = SQRT(1.0 - BETA2)
      IF  (K. LE .0.0001 . AND . M. LE .0.0001)  GO TO  110
      KBAR   = 2.0 *K *M *A / CBAR
      KBAR2  = KBAR*KBAR
      GO TO  120
  110 CONTINUE
      KBAR   = 0.0
      KBAR2  = 0.0
  120 XA     = 0.5 * (X1 + X2)
      DX     = X2 - X1
      A2     = A * A
      EPS    = 0.001 * A2
      IF  (ETA. EQ .YB. AND . ZETA. EQ .ZB)  GO TO  130
      RAIJ2  = (ETA-YB)**2 + (ZETA-ZB)**2
      RAIJ   = SQRT(RAIJ2)
      CTH    = (ETA- YB) / RAIJ
      STH    = (ZETA-ZB) / RAIJ
      IF  (RAIJ2. GT .A2)  GO TO  150
      GO TO  140
  130 CONTINUE
      RAIJ   = 0.0
      RAIJ2  = 0.0
      CTH    = 1.0
      STH    = 0.0
  140 RWIG2  = A2
      GO TO 160
  150 RWIG2  = RAIJ2
  160 RAA    = SQRT((XA -XIJ)**2 + BETA2*RWIG2)
      CT2    = CTH*CTH
      ST2    = 0.0
      IF  (ABS(STH) . GT . 0.0001)  ST2=STH*STH
      RWIG   = SQRT(RWIG2)
      RAA2   = RAA * RAA
      RAA3   = RAA * RAA2
      RAA4   = RAA * RAA3
      CAPA   = M - (XA- XIJ) / RAA
      DELTA  = DX / RAA
      DELTA2 = DELTA * DELTA
      EARG   = 0.0
      IF  (KBAR . LE . 0.0001)  GO TO  180
      EARG   = KBAR * (M * (XA-XIJ) - RAA) / (BETA2 * A)
      QR     = COS(EARG) / (4.0 * DX)
      QI     = SIN(EARG) / (4.0 * DX)
      GO TO  190
  180 QR     = 1.0 / (4.0 * DX)
      QI     = 0.0
  190 CONTINUE
      IF  (DELTA. GT . TEST1)  GO TO  240
      I1     = DELTA / RAA2
      TRM1   = BETA2 * A * I1
      FTHR   = A * QR * TRM1
      FTHI   = A * QI * TRM1
      IF  (KBAR. LE .0.0001)  GO TO  210
      I4     = DELTA / RAA
      TRM2   = KBAR * I4
      FTHR   = FTHR - A * QI * TRM2
      FTHI   = FTHI + A * QR * TRM2
  210 CONTINUE
      IF  (RAIJ2. GT . (A2+EPS))  GO TO  220
      FRR    = FTHR
      FRI    = FTHI
      GO TO  370
  220 I6     = DELTA / RAA4
      TRM1   = -3.0 * A2 * BETA2*BETA2 * I6
      CAPDR  = RAIJ2 * QR * TRM1
      CAPDI  = RAIJ2 * QI * TRM1
      IF  (KBAR . LE . 0.0001)  GO TO  230
      I9     = DELTA / RAA3
      TRM1   = TRM1 + KBAR2 * I1
      TRM2   = -3.0 * A * BETA2 * KBAR * I9
      CAPDR  = RAIJ2 * (QR * TRM1 - QI * TRM2)
      CAPDI  = RAIJ2 * (QR * TRM2 + QI * TRM1)
  230 FRR    = FTHR + CAPDR
      FRI    = FTHI + CAPDI
      GO TO  370
  240 CONTINUE
      IF  (DELTA . GT . TEST2)  GO TO  320
      LASTBR = 0
      TAU    = (XA - XIJ) / RAA
      TAU2   = TAU * TAU
      I1     = DELTA * (1.0 - (-1.0+5.0*TAU2)*DELTA2/8.0) / RAA2
  250 TRM1   = A * BETA2 * I1
      FTHR   = A * QR * TRM1
      FTHI   = A * QI * TRM1
      IF  (KBAR . LE . 0.0001)  GO TO  270
      IF  (LASTBR . NE . 0)  GO TO  350
      DELTA3 = DELTA * DELTA2
      I2     = -(TAU * DELTA3) / (4.0 * RAA)
      I3     = DELTA3 / 12.0
      I4     = DELTA * (1.0 + (-1.0+3.0*TAU2)*DELTA2/12.0) / RAA
      I5     = -(TAU * DELTA3) / 6.0
  260 TRM1   = TRM1 - (KBAR2 * CAPA * I5) / (A * BETA2)
      TRM2   = KBAR * (CAPA * I2 + I4 - I3*BETA2*RWIG2/(2.0*RAA3) )
      FTHR   = A * (QR * TRM1 - QI * TRM2)
      FTHI   = A * (QR * TRM2 + QI * TRM1)
  270 IF  (RAIJ2. GT . (A2+EPS))  GO TO  280
      FRR    = FTHR
      FRI    = FTHI
      GO TO  370
  280 CONTINUE
      KBAR3  = KBAR*KBAR2
      IF  (LASTBR . NE . 0)  GO TO  340
      I6     = DELTA * (1.0 + 5.0*(-1.0+7.0*TAU2)*DELTA2/24.0) / RAA4
  290 TRM1   = -3.0 * A2 * BETA2*BETA2 * I6
      CAPDR  = RAIJ2 * QR * TRM1
      CAPDI  = RAIJ2 * QI * TRM1
      IF  (KBAR . LE . 0.0001)  GO TO  310
      IF  (LASTBR . NE . 0)  GO TO  360
      I7     = -5.0 * TAU * DELTA3 / (12.0 * RAA3)
      I8     = DELTA3 / (12.0 * RAA2)
      I9     = DELTA * (1.0 + (-1.0+6.0*TAU2)*DELTA2/6.0) / RAA3
      I10    = -DELTA3 * TAU / (3.0 * RAA2)
  300 TRM1   = TRM1 + KBAR2 * (I1 + 3.0 * CAPA * I10)
      TRM2   = 3.0*A*BETA2 * KBAR * (-CAPA*I7 +I8*BETA2*RWIG2/(2.0*RAA3)
     1    -I9) + KBAR3 * CAPA * I2 / (A * BETA2)
      CAPDR  = RAIJ2 * (QR * TRM1 - QI * TRM2)
      CAPDI  = RAIJ2 * (QR * TRM2 + QI * TRM1)
  310 FRR    = FTHR + CAPDR
      FRI    = FTHI + CAPDI
      GO TO  370
  320 CONTINUE
      LASTBR = 1
      RWIG   = SQRT(RWIG2)
      RA12   = (X1 - XIJ)**2 + BETA2 * RWIG2
      RA22   = (X2 - XIJ)**2 + BETA2 * RWIG2
      RA1    = SQRT(RA12)
      RA2    = SQRT(RA22)
      I1     = ((X2-XIJ)/RA2 - (X1-XIJ)/RA1) / (BETA2*RWIG2)
      GO TO  250
  340 CONTINUE
      RA13   = RA1 * RA12
      RA23   = RA2 * RA22
      I6     = ((X2-XIJ)/RA23-(X1-XIJ)/RA13 + 2.0*I1)/(3.0*BETA2*RWIG2)
      GO TO  290
  350 PART1  = 0.5 * DX * (XA - XIJ)
      I2     = -((PART1+RAA2)/RA2 + (PART1-RAA2)/RA1)/ (BETA2*RWIG2)
      DENOM  = X1 - XIJ + RA1
      I11    = ALOG(ABS((X2 - XIJ + RA2) / DENOM))
      I3     = I11 - 2.0*(XA - XIJ)*I2 - RAA2 * I1
      DENO4  = SQRT(BETA2) * RWIG
      ARG1   = (X2 - XIJ) / DENO4
      ARG2   = (X1 - XIJ) / DENO4
      I4     = (ATAN(ARG1) - ATAN(ARG2)) / DENO4
      I5     = 0.5 * ALOG(RA22 / RA12) - (XA - XIJ) * I4
      GO TO  260
  360 CONTINUE
      I7     = -(1.0/RA23 - 1.0/RA13) / 3.0 - (XA - XIJ) * I6
      I8     = I1 - 2.0 * (XA - XIJ) * I7 - RAA2 * I6
      I9     = ((X2-XIJ)/RA22-(X1-XIJ)/RA12 + I4) / (2.0*BETA2*RWIG2)
      I10    = -((PART1 + RAA2)/RA22 + (PART1 - RAA2)/RA12 +
     1           (XA - XIJ) * I4) / (2.0 * BETA2 * RWIG2)
      GO TO 300
  370 CONTINUE
      FZZR   = CT2 * FTHR + ST2 * FRR
      FZZI   = CT2 * FTHI + ST2 * FRI
      FYYR   = ST2 * FTHR + CT2 * FRR
      FYYI   = ST2 * FTHI + CT2 * FRI
      IF  (CTH. EQ .0.0 . OR . STH. EQ . 0.0)  GO TO  400
      FZYR   = CTH * STH * (FRR - FTHR)
      FZYI   = CTH * STH * (FRI - FTHI)
      GO TO  410
  400 FZYR   = 0.0
      FZYI   = 0.0
  410 CONTINUE
      FYZR = FZYR
      FYZI = FZYI
      RETURN
      END
