      SUBROUTINE FQRW (M,E,ER,A,B,W,P,Q,XM,INT,ZB,SRFLE,MCBC)
C
      LOGICAL            INT(1)
      INTEGER            SRFLE
      DOUBLE PRECISION   DLAMDA
      REAL               LAMBDA
      DIMENSION          A(1)     ,B(2)    ,W(1)   ,P(1)   ,E(1)    ,
     1                   Q(1)     ,ER(1)   ,XM(1)
      DIMENSION          ZB(1)    ,MCB(7)  ,MCBC(7)
      CHARACTER          UFM*23   ,UWM*25
      COMMON   /XMSSG /  UFM      ,UWM
      COMMON   /FEERXX/  DLAMDA   ,CNDFLG  ,ITER   ,TIMED  ,L16
      COMMON   /NAMES /  RD       ,RDREW   ,WRT    ,WRTREW ,REW     ,
     1                   NOREW    ,EOFNRW
      COMMON   /LHPWX /  LHPW(3)  ,IACC
      COMMON   /SYSTEM/  KSYSTM(65)
      COMMON   /PACKX /  ITP1     ,ITP2    ,IIP    ,NNP    ,INCRP
      COMMON   /UNPAKX/  IPRC     ,II      ,NN     ,INCR
      EQUIVALENCE        (KSYSTM(2),IO)    ,(KSYSTM(55),IPREC)
      DATA      ILIM  ,  IEXP   ,BASE /    120, 60,  2.    /
C
C     IACC  =  ACCURACY CONTROL (EPSILON) FOR UNDERFLOW
C
      IF (M .EQ. 1) RETURN
      LAMBDA = DLAMDA
      IPRC = 1
      CALL MAKMCB (MCB(1),SRFLE,M,2,IPRC)
      ICF  = MCBC(1)
      INCR = 1
      INCRP= 1
      ITP1 = IPRC
      ITP2 = IPRC
      IT   = IACC*IPREC
      PRC  = 10.**(-IT)
      PPRC = 10.E-4
      JERR = 0
      EPX  = 10.**(2-IT)
      EPX2 = EPX**2
      HOV  = BASE**IEXP
      M1   = M - 1
      DO 20 I = 1,M
   20 E(I) = A(I)
      TOL  = PRC/(10.*FLOAT(M))
      BMAX = 0.
      TMAX = 0.
      W(M+1) = 0.
      DO 30 I = 1,M
      IF (BMAX .LT. ABS(B(I))) BMAX = ABS(B(I))
      IF (TMAX .LT. ABS(A(I))) TMAX = ABS(A(I))
   30 CONTINUE
      IF (TMAX .LT. BMAX) TMAX = BMAX
      SCALE = 1.
      DO 40 I = 1,ILIM
      IF (SCALE*TMAX .GT. HOV) GO TO 50
   40 SCALE = SCALE*2.
   50 IF (BMAX .EQ. 0.) GO TO 170
      DO 60 I = 1,M
      E(I) =  A(I)*SCALE
   60 W(I) = (B(I)*SCALE)**2
      DELTA= TMAX*SCALE*TOL
      EPS  = DELTA*DELTA
      K  = M
   70 L  = K
      IF (L .LE. 0) GO TO 140
      L1 = L - 1
      DO 80 I = 1,L
      K1 = K
      K  = K - 1
      IF (W(K1) .LT. EPS) GO TO 90
   80 CONTINUE
   90 IF (K1 .NE. L) GO TO 100
      W(L) = 0.
      GO TO 70
  100 T = E(L) - E(L1)
      X = W(L)
      Y = .5*T
      S = SQRT(X)
      IF (ABS(T) .GT. DELTA) S = (X/Y)/(1.+SQRT(1.+X/Y**2))
      E1 = E(L ) + S
      E2 = E(L1) - S
      IF (K1 .NE. L1) GO TO 110
      E(L ) = E1
      E(L1) = E2
      W(L1) = 0.
      GO TO 70
  110 SHIFT = E1
      IF (ABS(T).LT.DELTA .AND. ABS(E2).LT.ABS(E1)) SHIFT = E2
      S  = 0.
      C  = 1.
      GG = E(K1) - SHIFT
      GO TO 130
  120 C  = F/T
      S  = X/T
      X  = GG
      GG = C*(E(K1) - SHIFT) - S*X
      E(K) = (X - GG) + E(K1)
  130 IF (ABS(GG) .LT. DELTA) GG = GG + C*DELTA*GG/ABS(GG)
      F  = GG**2/C
      K  = K1
      K1 = K + 1
      X  = W(K1)
      T  = X + F
      W(K) = S*T
      IF (K .LT. L) GO TO 120
      E(K) = GG + SHIFT
      GO TO 70
  140 DO 150 I = 1,M
  150 E(I) = E(I)/SCALE
      DO 155 L = 1,M1
      K = M - L
      DO 155 I = 1,K
      IF (E(I) .GT. E(I+1)) GO TO 155
      X = E(I)
      E(I  ) = E(I+1)
      E(I+1) = X
  155 CONTINUE
      DO 160 L = 1,M1
      K = M - L
      DO 160 I = 1,K
      IF (ABS(E(I)) .GT. ABS(E(I+1))) GO TO 160
      X = E(I)
      E(I  ) = E(I+1)
      E(I+1) = X
  160 CONTINUE
  170 IF (M .EQ. 0) RETURN
C
C     COMPUTE EIGENVECTORS BY INVERSE ITERATION
C
      ERF  = B(M+1)
      MVEC = M
      F = SCALE/HOV
      DO 190 I = 1,M
      A(I) = A(I)*F
  190 B(I) = B(I)*F
      DIMF = 10.**(-IT/3)
      DO 460 NV = 1,MVEC
      IJ   = NV
      SUMX = 0.
      IRP  = 0
      IF (NV .EQ. 1) GO TO 200
      RATIO = ABS(E(NV)/E(NV-1) - 1.)
      DIM   = .02*ABS(1.-LAMBDA*E(NV))
      IF (RATIO.LT.DIM .OR. RATIO.LT.DIMF) GO TO 220
      NRP = 0
      GO TO 225
  200 NRP = 0
      DO 210 I = 1,M
  210 W(I) = 1.
      IIP  = 1
      NNP  = M
      GO TO 330
C
C     MULTIPLE EIGENVALUES
C
  220 NRP = NRP + 1
  225 IF (NV .NE. 2) GO TO 230
      CALL GOPEN (SRFLE,ZB(1),WRTREW)
      MCB(2) = 0
      MCB(6) = 0
      GO TO 240
  230 CALL GOPEN (SRFLE,ZB(1),WRT)
  240 IIP = 1
      NNP = M
      CALL PACK (W(1),SRFLE,MCB(1))
      CALL CLOSE (SRFLE,NOREW)
      SS  = 1.0
      SUM = 0.
      DO 250 I = 1,M
      SS =-SS
      IJ = IJ + 1
      P(I) = FLOAT(MOD(IJ,3)+1)/(3.0*FLOAT((MOD(IJ,13)+1)*(1+5*I/M)))
      P(I) = P(I)*SS
  250 SUM  = SUM + P(I)**2
      SUM  = 1./SQRT(SUM)
      DO 255 I = 1,M
      P(I) = P(I)*SUM
  255 Q(I) = P(I)
      CALL GOPEN (SRFLE,ZB(1),RDREW)
      J = 0
  260 SUM = 0.
      J = J + 1
      DO 270 I = 1,M
  270 SUM = SUM + W(I)*P(I)
      DO 280 I = 1,M
  280 Q(I) = Q(I) - SUM*W(I)
      IF (J .EQ. NV-1) GO TO 290
      II = 1
      NN = M
      CALL UNPACK (*290,SRFLE,W(1))
      GO TO 260
  290 CALL CLOSE (SRFLE,NOREW)
      SUM = 0.
      DO 300 I = 1,M
  300 SUM = SUM + Q(I)**2
      SUM = 1./SQRT(SUM)
      DO 310 I = 1,M
      Q(I) = Q(I)*SUM
  310 W(I) = Q(I)
  330 EV = E(NV)*F
      X  = A(1) - EV
      Y  = B(2)
      DO 350 I = 1,M1
      C  = A(I+1) - EV
      S  = B(I+1)
      IF (ABS(X) .GE. ABS(S)) GO TO 340
      P(I) = S
      Q(I) = C
      INT(I) = .TRUE.
      Z = -X/S
      X = Y + Z*C
      IF (I .LT. M1) Y = Z*B(I+2)
      GO TO 350
  340 IF (ABS(X) .LT. TOL) X = TOL
      P(I) = X
      Q(I) = Y
      INT(I) = .FALSE.
      Z = -S/X
      X = C + Z*Y
      Y = B(I+2)
  350 XM(I) = Z
      IF (ABS(X) .LT. TOL) X = TOL
      NITER = 0
  360 NITER = NITER + 1
      W(M)  = W(M)/X
      EMAX  = ABS(W(M))
      DO 370 L = 1,M1
      I = M - L
      Y = W(I) - Q(I)*W(I+1)
      IF (INT(I)) Y = Y - B(I+2)*W(I+2)
      W(I) = Y/P(I)
      IF (ABS(W(I)) .GT. EMAX) EMAX = ABS(W(I))
  370 CONTINUE
      SUM = 0.
      DO 375 I = 1,M
      W(I) = (W(I)/EMAX)/EPX
      IF (ABS(W(I)) .LT. EPX2) W(I) = EPX2
  375 SUM  = SUM + W(I)**2
      S    = SQRT(SUM)
      DO 380 I = 1,M
      W(I) = W(I)/S
  380 CONTINUE
      IF (NITER .GE. 4) GO TO 402
      DO 400 I = 1,M1
      IF (INT(I)) GO TO 390
      W(I+1) = W(I+1) + XM(I)*W(I)
      GO TO 400
  390 Y = W(I)
      W(I  ) = W(I+1)
      W(I+1) = Y + XM(I)*W(I)
  400 CONTINUE
      GO TO 360
  402 IF (NV .EQ. 1) GO TO 410
C
C     MULTIPLE EIGENVALUES AND ORTHOGONALIZATION
C
      IRP = IRP + 1
      CALL GOPEN (SRFLE,ZB(1),RDREW)
      DO 404 I = 1,M
  404 Q(I) = W(I)
      SUMX = 0.
      JRP  = NV - 1
      DO 407 I = 1,JRP
      II = 1
      NN = M
      CALL UNPACK (*408,SRFLE,P(1))
      SUM = 0.
      DO 405 J = 1,M
  405 SUM = SUM + P(J)*Q(J)
      IF (ABS(SUM) .GT. SUMX) SUMX = ABS(SUM)
      DO 406 J = 1,M
  406 W(J) = W(J) - SUM*P(J)
  407 CONTINUE
  408 CALL CLOSE (SRFLE,NOREW)
  410 CONTINUE
C
C     LOGIC SETTING SUM (BY G.CHAN/UNISYS  7/92)
C
C     SUM = PRC*PREC COULD PRODUCE UNDERFLOW
C     SUM = ZERO, COULD CAUSE DIVIDED BY ZERO AFTER 420 FOR NULL VECTOR
C     SO, WE CHOOSE SUM A LITTLE SMALLER THAN PRC
C
C     SUM = PRC*PRC
C     SUM = 0.0
      SUM = PRC*1.0E-4
C
      DO 420 I = 1,M
  420 SUM = SUM + W(I)*W(I)
      SUM = 1./SQRT(SUM)
      DO 430 I = 1,M
  430 W(I) = W(I)*SUM
      IF (SUMX.GT.0.9 .AND. IRP.LT.3) GO TO 330
      IF (L16 .NE. 0) WRITE (IO,435) NV,NITER,IRP,SUMX
  435 FORMAT (10X,18H FEER QRW ELEMENT ,I5,6H ITER ,2I3,6H PROJ ,E16.8)
      IF (JERR.GT. 0) GO TO 450
      ZERR = ABS(W(1))
      DO 440 I = 2,M
      IF (ABS(W(I)) .GT. ZERR) ZERR = ABS(W(I))
  440 CONTINUE
      ZERR = (ABS(W(M)))/ZERR
      IF (ZERR .GT. PPRC) JERR = NV-1
      IF (JERR .NE.    0) WRITE (IO,445) UWM,JERR
  445 FORMAT (A25,' 2399', /5X,'ONLY THE FIRST',I6,' EIGENSOLUTIONS ',
     1       'CLOSEST TO THE SHIFT POINT (F1 OR ZERO) PASS THE FEER ',
     2       'ACCURACY TEST FOR EIGENVECTORS.')
  450 CONTINUE
      CALL PACK (W(1),ICF,MCBC(1))
      ER(NV) = ABS(W(M)*ERF/E(NV))
  460 CONTINUE
      RETURN
      END
