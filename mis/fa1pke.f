      SUBROUTINE FA1PKE (KHH,BHH,MHH,BXHH,FSAVE,NLOOP,BREF,RREF,NEIW,
     1                   EPS)
C
C     FA1PKE COMPUTES THE EIGENVALUES FOR THE PK METHOD
C
C     LAST REVISED  2/91, BY J.PETKAS/LOCKHEED
C     ELEMENTS OF INTERPOLATION MATRIX IN D.P. AND LEAST SQUARE FIT
C
      LOGICAL          EIGV
      INTEGER          BHH,BXHH,SYSBUF,NAME(2),TRL(7),BUF1,FLOOP,FSAVE
      REAL             KINT
      DOUBLE PRECISION DX1,DX2,DSUM,DZ(1)
      CHARACTER        UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /XMSSG /  UFM,UWM,UIM,SFM
      COMMON /SYSTEM/  SYSBUF,NOUT
      COMMON /UNPAKX/  IOUT,INN,NNN,INCR1
      COMMON /ZZZZZZ/  Z(1)
      COMMON /FA1PKC/  NCORE,NK,IMVR,IK,IA,IQ,ICP,IFLAG
      COMMON /BLANK /  FLOOP
      COMMON /CONDAS/  PI,TWOPI
      EQUIVALENCE      (Z(1),DZ(1))
      DATA    NAME  /  4HFA1P,4HKE  /
      DATA    ISTART/  0 /
C
C     REINITIALIZE EVERY TIME MACH CHANGES
C
      IF (IFLAG .EQ. 0) GO TO 100
      CALL SSWTCH (39,L39)
      TRL(1) = KHH
      CALL RDTRL (TRL)
      NROW  = TRL(2)
      NEIW  = MIN0(NEIW,NROW)
      NEIGN = NROW*2
      IOUT  = 1
      INN   = 1
      INCR1 = 1
      NNN   = NROW
      IEIGNS= NCORE  - NROW*5 - 1
      BUF1  = IEIGNS - SYSBUF
      NN    = NROW*NROW
      NN2   = NN*2
      IMH   = ICP
      IBH   = IMH + NN
      IKH   = IBH + NN
      IV    = IKH + NN
      IB    = IV  + NN
      IMA   = IB  + NN
      IF (MOD(IMA,2) .EQ. 0) IMA = IMA + 1
      IOP   = IMA + NN2*4
C
C     CORE CHECK
C
      IF (IOP+SYSBUF .GT. IEIGNS) CALL MESAGE (-8,0,NAME)
C
C     PUT K B M IN CORE
C
      IFL = KHH
      JI  = IKH
   10 CALL GOPEN (IFL,Z(BUF1),0)
      DO 20 I = 1,NROW
      CALL UNPACK (*15,IFL,Z(JI))
      GO TO 20
   15 CALL ZEROC (Z(JI),NROW)
   20 JI = JI + NROW
      CALL CLOSE (IFL,1)
      IF (IFL .EQ. MHH) GO TO 40
      IF (IFL .EQ. BHH) GO TO 30
      IFL = BHH
      JI  = IBH
      TRL(1) = BHH
      CALL RDTRL (TRL)
      IF (TRL(1) .GT. 0) GO TO 10
      CALL ZEROC (Z(JI),NN)
   30 IFL = MHH
      JI  = IMH
      GO TO 10
   40 CONTINUE
C
C     MODIFICATION FOR LEVEL 17.7 UPDATE
C     REPLACE CALLS TO INVAER WITH CALLS TO INVERS.
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (NROW,Z(IMH),NROW,0,0,DET,ISING,Z(IOP))
      IF (ISING .EQ. 2) CALL MESAGE (-7,0,NAME)
C
C     START OF LARGE LOOP WITH K = 0.0
C
  100 KINT = 0.0
      IF (EPS .LE. 0.0) EPS = .001
      KN1 = NK + 1
      IQ0 = IEIGNS - (NN2+1)
      IC0 = IQ0 - KN1*2 - 2
      IF (MOD(IC0,2) .EQ. 0) IC0 = IC0 - 1
      IP0 = IC0 - KN1*2 - 2
      IF (MOD(IP0,2) .EQ. 0) IP0 = IP0 - 1
C
      I   = (FLOOP-1)*3
      EIGV= .FALSE.
      IF (Z(IMVR+I+1) .LT. 0.0) EIGV = .TRUE.
      VEL = ABS(Z(IMVR+I+1))
      VELS= VEL*VEL
      RHO = (RREF*Z(IMVR+I+2))/2.0
      IF (L39 .NE. 0) WRITE (NOUT,105) FLOOP,Z(IMVR+I),Z(IMVR+I+1),
     1                                 Z(IMVR+I+2)
  105 FORMAT ('0 TRACE FOR PK METHOD LOOP',I5,6X,4HMACH,8X,
     1        8HVELOCITY,8X,7HDENSITY,/,30X,1P,E15.5,1P,E15.5,1P,E15.5)
      NIT   = 0
      NROOT = 0
C
C     INITIALIZE LEAST SQUARE COEFFCIENTS
C
      XAV   = 0.
      YAV1  = 0.
      X10   = 0.
      X11   = 0.
      X12   = 0.
      Y10   = 0.
      Y11   = 0.
C
C     BUILD P
C
  110 CONTINUE
      NIT = NIT + 1
C
C     SUM LEAST SQUARE COEFFICIENTS ASSOCIATED WITH INDEPENDENT
C     VARIABLE STARTING WITH SECOND TRIAL
C
      IF (NIT .EQ. 1) GO TO 115
      XAV = XAV + KINT
      X10 = X10 + 1.
      X11 = X11 + KINT
      X12 = X12 + KINT**2
C
  115 IP0D = IP0/2 + 1
      DX1  = KINT
      DO 120 I = 1,NK
      DX2 = Z(IK+I-1)
      DZ(IP0D+I) = DABS((DX1-DX2)**3) + (DX1+DX2)**3
  120 CONTINUE
      DZ(IP0D+KN1) = 1.D0
C
C     FIND C = A-1  P
C
      IAD  = IA/2  + 1
      IC0D = IC0/2 + 1
      L = IAD
      DO 135 I = 1,KN1
      DSUM = 0.D+0
      DO 130 J = 1,KN1
      DSUM = DSUM + DZ(L)*DZ(IP0D+J)
      L = L + 1
  130 CONTINUE
      DZ(IC0D+I) = DSUM
  135 CONTINUE
C
C     FIND QR AND QI = Q  C  Q IS COLUMN STORED
C
      L = IQ
      DO 145 I = 1,NN2
      DSUM = 0.D+0
      DO 140 J = 1,NK
      K = L + (J-1)*NN2
      DSUM = DSUM + Z(K)*DZ(IC0D+J)
  140 CONTINUE
      L = L + 1
      Z(IQ0+I) = DSUM
  145 CONTINUE
C
C     COLUMN STORED M-1  BHH  KNH  QR (Z(IQ0+1)   QI (Z(IQ3+NN+1)
C
C     B  =  -BHH  + RHO*BREF*VEL  QHHI
C
C     K  =  -KHH  + RHO*VELS      QHHR
C
C     BUILD  A
C                  0         I
C
C                   -1       -1
C                 -M K     -M B
C
      NREM = IQ0 - IOP
      IF (NREM-NN .LE. 0) CALL MESAGE (-8,0,NAME)
      IT = IOP
      IF (MOD(IT,2) .EQ. 0) IT = IT + 1
      IF (EIGV .AND. IT+NN.GT.BUF1) CALL MESAGE (-8,0,NAME)
      BOV = BREF/VEL
      RBV = RHO*BREF*VEL
      IQR = IQ0
      IQI = IQ0 + NN
      RVS = RHO*VELS
C
C     BUILD M-1K IN IB AND M-1B IN IT  THEN GMMATS INTO IV AND IB
C
      DO 150 I = 1,NN
      Z(IT+I-1) = -Z(IBH+I-1) + RBV*Z(IQI+I)
      Z(IB+I-1) = -Z(IKH+I-1) + RVS*Z(IQR+I)
  150 CONTINUE
      CALL GMMATS (Z(IB),NROW,NROW,0,Z(IMH),NROW,NROW,0,Z(IV))
      CALL GMMATS (Z(IT),NROW,NROW,0,Z(IMH),NROW,NROW,0,Z(IB))
C
C     CALL FA1PKA TO MAKE A MATRIX AND GET EIGENVALUES
C
      CALL FA1PKA (Z(IMA),Z(IV),Z(IB),Z(IT),IEIGNS-IT,NROW)
C
C     SORT EIGENVALUES
C
      J = NEIGN*2
      CALL RSORT (2,1,Z(IT),J)
      CALL RSORT (2,2,Z(IT),J)
      IF (KINT .NE. 0.0) GO TO 180
      NLFT = NEIGN
      DO 160 I = 1,J,2
      IF (Z(IT+I) .GE. 0.0) GO TO 170
      NLFT = NLFT - 1
  160 CONTINUE
  170 NL = IT + (NEIGN-NLFT)*2
      NR = 0
      DO 175 I = 1,J,2
      IF (Z(IT+I) .NE. 0.0) GO TO 175
      NR = NR + 1
      IF (EIGV) CALL FA1PKV (Z(IMA),Z(IV),Z(IB),NROW,Z(IT+I-1),Z(IMA),
     1                       BREF,PI,VEL,Z(BUF1))
  175 CONTINUE
      NRS = NR + 1
      NR  = NR/2
      NRA = 0
  180 CONTINUE
      IF (L39 .EQ. 0) GO TO 200
      WRITE  (NOUT,182) KINT
  182 FORMAT (1H0,29H ESTIMATED REDUCED FREQUENCY ,1P,E15.5, /10X,
     1        11HEIGENVALUES,10X,18H REDUCED FREQUENCY,4X,9HFREQUENCY,
     2        6X,8H DAMPING,/,7X,4HREAL,10X,4HIMAG)
      DO 190 I = 1,J,2
      ER = Z(IT+I-1)
      EI = Z(IT+I  )
      IF (EI .EQ. 0.0) GO TO 183
      RK = BOV*EI
      RF = (1.0/TWOPI)*EI
      RG = (2.0*ER)/EI
      GO TO 185
  183 RK = 0.0
      RF = 0.0
      RG = (BREF/(PI*VEL))*ER
  185 WRITE  (NOUT,186) ER,EI,RK,RF,RG
  186 FORMAT (1H ,1P,E15.5,1P,E15.5,3X,1P,E15.5,1P,E15.5,1P,E15.5)
  190 CONTINUE
C
C     ROOT ACCEPTANCE AND SAVING
C
  200 J = NLFT*2
      L = NROOT*2 + 1 + NRA*2
      IMHERE = 200
      IF (L .GT. J) GO TO 360
C
      DO 270 I = L,J,2
      K = (NROOT*5) + 1 + IEIGNS
      IF (Z(NL+I) .NE. 0.0) GO TO 220
      IF (KINT    .NE. 0.0) GO TO 220
      IF (NRS .NE. NR) NRS = NRS - 1
      IF (NRS .NE. NR) GO TO 270
      NRA = NRA + 1
      Z(K  ) = Z(NL+I-1)
      Z(K+1) = Z(NL+I  )
      Z(K+2) = 0.0
      Z(K+3) = 0.0
      Z(K+4) = (BREF/(.34657*VEL))*Z(NL+I-1)
  210 NROOT  = NROOT + 1
C
C     PRINT EIGENVECTORS IF ASKED FOR
C
      NIT = 0
C
C     NO. OF ITERATIONS RESET TO ZERO. RE-INITIALIZE LEASE SQUARE COEFF.
C
      XAV = 0.
      YAV1= 0.
      X10 = 0.
      X11 = 0.
      X12 = 0.
      Y10 = 0.
      Y11 = 0.
      IF (NROOT .GE. NEIW) GO TO 300
      GO TO 270
  220 RKTST = BOV*Z(NL+I)
      IF (ABS(RKTST-KINT) .LT. EPS) GO TO 230
      IF (RKTST .EQ. 0.0) GO TO 230
C
C     SUM LEAST SQUARE COEFFICIENTS ASSOCIATED WITH DEPENDENT VARIABLE
C     STARTING WITH RESULT OF SECOND TIRAL
C
      IF (NIT .EQ. 1) GO TO 225
      YAV1 = YAV1 + RKTST
      Y10  = Y10  + RKTST
      Y11  = Y11  + RKTST*KINT
  225 KINT = RKTST
      IF (NIT .EQ. 10) GO TO 240
      GO TO 110
C
C     START LOOP OVER
C
  230 Z(K  ) = Z(NL+I-1)
      Z(K+1) = Z(NL+I  )
      Z(K+2) = RKTST
      Z(K+3) = (1.0/TWOPI)*Z(NL+I)
      IF (Z(NL+I) .NE. 0.0) Z(K+4) = (2.0*Z(NL+I-1))/Z(NL+I)
      IF (Z(NL+I) .EQ. 0.0) Z(K+4) = (BREF/(.34657*VEL))*Z(NL+I-1)
      IF (EIGV) CALL FA1PKV (Z(IMA),Z(IV),Z(IB),NROW,Z(K),Z(IMA),
     1                       BREF,PI,VEL,Z(BUF1))
      GO TO 210
C
C     FAILURE TO CONVERGE. REPLACE LOOP END WITH LEAST SQUARES FIT
C
  240 NIT  = NIT + 1
      XAV1 = XAV/(NIT-2)
      XAV  = (XAV + RKTST)/(NIT-1)
      YAV1 = YAV1/(NIT-2)
      D1   = X12*X10  - X11*X11
      A11  = (X10*Y11 - X11*Y10)/D1
      A10  = (X12*Y10 - X11*Y11)/D1
      RKTST= -A10/(A11-1.)
      WRITE  (NOUT,250) UWM,NIT,FLOOP,NROOT,NEIW
  250 FORMAT (A25,', PK METHOD FIALED TO CONVERGE', /1X,I4,
     1       ' ITERATIONS ON LOOP',I5,',  FOUND',I5,',  ROOTS WANTED',
     2       I5, /5X,'LEAST SQUARES FIT APPROXIMATION IMPLEMENTED.')
      IF (L39 .EQ. 1) WRITE (NOUT,260) XAV1,YAV1,XAV, A11,A10,RKTST
  260 FORMAT (/5X,'AVG. TRIAL = ',1P,E12.5,',  AGV. RESLT. = ',1P,E12.5,
     4   ',  NET AVG. = ',1P,E12.5,  //9X,'SLOPE = ',1P,E12.5,
     5   ',    INTERCEPT = ',1P,E12.5,',  VALUE    = ',1P,E12.5)
      GO TO 230
C
  270 CONTINUE
C
C     LOGIC ERROR
C
      IMHERE = 270
      GO TO 360
C
C     SAVE EIGENVALUES ON BXHH
C
  300 IF (ISTART .NE. 0) GO TO 310
      ISTART = 1
      CALL GOPEN (BXHH,Z(BUF1),1)
      CALL CLOSE (BXHH,2)
  310 CALL GOPEN (BXHH,Z(BUF1),3)
      CALL WRITE (BXHH,Z(IEIGNS+1),NROOT*5,1)
      IF (FLOOP .GE. NLOOP) GO TO 320
      CALL CLOSE (BXHH,3)
      RETURN
C
C     LAST LOOP BUILD FSAVE
C
  320 CALL CLOSE (BXHH,1)
      IBUF2 = BUF1 - SYSBUF
      CALL GOPEN (BXHH,Z(BUF1),0)
      CALL GOPEN (FSAVE,Z(IBUF2),0)
      CALL SKPREC (FSAVE,3)
      CALL CLOSE (FSAVE,2)
      CALL GOPEN (FSAVE,Z(IBUF2),3)
  330 CALL READ  (*350,*340,BXHH,Z(1),IBUF2,1,NWR)
  340 CALL WRITE (FSAVE,Z(1),NWR,1)
      GO TO 330
  350 CALL CLOSE (BXHH,1)
      CALL CLOSE (FSAVE,1)
      TRL(1) = FSAVE
      TRL(2) = NLOOP
      TRL(7) = NEIW
      CALL WRTTRL (TRL)
      GO TO 400
C
  360 WRITE  (NOUT,370) SFM,IMHERE,L,J
  370 FORMAT (A25,'. ERROR IN FA1PKE/@',I3,'  L,J=',2I7)
      CALL MESAGE (-61,0,0)
C
  400 RETURN
      END
