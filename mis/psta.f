      SUBROUTINE PSTA(DELTAY,BI,CA,ALPH,THI,AJJL)
      DIMENSION A(3,3),AI(6),AJ(6),H(3,3),EK(6),G(3,3),GI(3,3),Q(3,3)
      DIMENSION DELTAY(1),BI(1),CA(1),ALPH(1),THI(13)
      DIMENSION P(3,6),QI(3,3)
      COMPLEX PC(3)
      COMMON /PACKX/ ITI,IT0,II,NN,INCR
      COMMON /AMGMN/ MCB(7),NROW,ND,NE,REFC,EMACH,RFK
      COMMON /PSTONC/ NJJ,NMACH,NTHRY,NTHICK,NALPHA,NXIS,NTAUS,NSTRIP,
     *                SECLAM
      COMMON / CONDAS / PI,TWOPI,RADG,DEGRA
      DATA A /9*0.0/ , H /9*0.0/
      BREF = REFC * .5
      RFC = RFK/BREF
      II = NROW +1
      NN = NROW
C
C     BUILD AJJL FOR EACH STRIP
C
      DO 200 I=1,NSTRIP
      B = BI(I)
      CONST = 8.0 * DELTAY(I) * (RFC * B)**2
      A(1,1) = -1.0
      A(2,1) = -.5 * B
      A(2,2) = B
      A(3,3) = B
      H(1,1) = -1.0
      H(1,2) = A(2,1)
      H(2,2) = B
      H(3,3) = B
      ALPHA = ALPH(1)
      IF(NALPHA.NE.1 ) ALPHA = ALPH(I)
      ALPHA = ALPHA * DEGRA
      ALPHA2 = ALPHA*ALPHA
      N = 2
      IF(CA(I) .NE. 0.0 ) N = 3
      IF( NTHICK .EQ. 0 ) GO TO 20
      DO 10 J=1,6
      AI(J) = THI(J)
      AJ(J) = 0.0
      IF(N .EQ. 3 ) AJ(J) = THI(J+6)
   10 CONTINUE
      ZETAH = 1.0
      IF( NXIS .EQ. 1 ) ZETAH = THI(13)
      IF( NXIS.GT.1) ZETAH = THI(I+12)
      GO TO 70
   20 IF( NTAUS .NE. 1 ) GO TO 30
      TAU = THI(1)
      TAUH = THI(2)
      TAUT = THI(3)
      IF( N .EQ. 2 ) TAUT = 0.
      T = TAUH-TAUT
      ZETAM = THI(4)
      ZETAH = THI(5)
      GO TO 50
   30 K = (I-1) * 3+1
      TAU = THI(K)
      TAUH = THI(K+1)
      TAUT = THI(K+2)
      IF( N .EQ. 2 ) TAUT = 0.
      T = TAUH - TAUT
      K = (I-1)*2+1 + 3*NSTRIP
      ZETAM = THI(K)
      ZETAH = THI(K+1)
      DO 40 J=1,6
   40 AJ(J) = 0.
   50 IF(N .EQ. 2 ) ZETAH = 1.0
      IF( N .EQ. 2 ) GO TO 60
      AJ(1) = -.5 * T
      AJ(2) = -.25*T*(1.0+ZETAH)
      AJ(3) = -(1./6.)*T*(1.+ZETAH+ZETAH*ZETAH)
      AJ(4) = .25*T*T/ (1.-ZETAH)
      AJ(5) = .125*T*T*(1.0+ZETAH) / (1.-ZETAH)
      AJ(6) = (1./12.)*T*T*(1.+ZETAH+ZETAH*ZETAH) / (1.-ZETAH)
   60 TS = TAU-TAUH*(TAU-TAUH)
      AI(1) = TAUH*.5 + AJ(1)
      AI(2) = -(TAU/3.)*ZETAH + (TAUH/6.) * (2.*ZETAH+ZETAM) + AJ(2)
      AI(3) = -(TAU/12.)*ZETAH *(3.*ZETAH+2.*ZETAM) + (TAUH/12.) *
     *          (3. *ZETAH*ZETAH + 2.*ZETAH*ZETAM+ZETAM*ZETAM) + AJ(3)
      AI(4) = (TAU*TAU/(3. *ZETAM)) + (1./3.)*TS*(ZETAH-ZETAM) + AJ(4)
      AI(5) = (TAU*TAU/12.) + (1./12.)*TS*(3.*ZETAH + ZETAM) /
     *         (ZETAH-ZETAM) + AJ(5)
      AI(6) = (TAU*TAU/30.) * ZETAM + (1./30.)*TS*(6.*ZETAH*ZETAH +
     *         3.*ZETAH*ZETAM + ZETAM*ZETAM) / (ZETAH-ZETAM) + AJ(6)
   70 EMS = EMACH*EMACH
      SECS = SECLAM*SECLAM
      IF( NTHRY .NE. 0 ) GO TO 80
      CBAR1 = 1.
      CBAR2 = (1.4+1.)/4.
      GO TO 90
   80 CBAR1 = EMACH / SQRT(EMS-SECS)
      CBAR2 = (EMS*EMS*(1.4+1.)- 4.*SECS*(EMS-SECS)) /(4.*(EMS-SECS)**2)
   90 CBAR3 = (1.4+1.) / 12.
      EK(1) = (1./EMACH ) *(CBAR1+2.*CBAR2*EMACH*AI(1)
     *       + 3.*CBAR3*EMS*(AI(4)+ALPHA2))
      EK(2) = (1./EMACH) * (CBAR1+4.*CBAR2*EMACH * AI(2)
     *       + 3.*CBAR3*EMS*(2.*AI(5)+ALPHA2))
      EK(3) = (4./(3.*EMACH)) * (CBAR1+6.*CBAR2*EMACH*AI(3)
     *       + 3.*CBAR3*EMS*(3.*AI(6)+ALPHA2))
      IF( N .EQ. 3 ) GO TO 100
      EK(4) = (1./EMACH) * (CBAR1*(1.-ZETAH) + 2.*CBAR2*EMACH*AJ(1)
     *       +3.*CBAR3*EMS*AJ(4) + ALPHA2*(1.-ZETAH))
      EK(5) = (1./EMACH) * (CBAR1*(1.-ZETAH*ZETAH) + 4.*CBAR2*EMACH*
     *       AJ(2) + 3.*CBAR3*EMS*(2.*AJ(5)+ALPHA2*(1.-ZETAH*ZETAH)))
      EK(6) = (4./(3.*EMACH))*(CBAR1*(1.-ZETAH**3) + 6.*CBAR2*EMACH*
     *       AJ(3) + 3.*CBAR3*EMS*(3.*AJ(6)+ ALPHA2*(1.-ZETAH**3)))
      E1K = 1.0/(RFC *B)
      E1KS = E1K*E1K
      G(1,1) = 0.
      G(1,2) = -EK(1) * E1KS
      G(2,1) = 0.
      G(2,2) = -EK(2) * E1KS
      GI(1,1) = -EK(1) * E1K
      GI(1,2) = -EK(2) * E1K
      GI(2,1) = GI(1,2)
      GI(2,2) = -EK(3) * E1K
      IF( N .EQ. 3 ) GO TO 100
      G(1,3) = -EK(4) * E1KS
      G(2,3) = -EK(5) * E1KS
      G(3,1) = 0.
      G(3,2) =-(EK(5)-2.*EK(4)*ZETAH) * E1KS
      G(3,3) = G(3,2)
      GI(1,3) = -(EK(5)-2.*EK(4)*ZETAH) * E1K
      GI(2,3) = -(EK(6)-2.*EK(5)*ZETAH) * E1K
      GI(3,1) = GI(1,3)
      GI(3,2) = -(EK(6) -2.*EK(5)*ZETAH) * E1K
      GI(3,3) = -(EK(6)-4.*EK(5)*ZETAH+4.*EK(4)*ZETAH*ZETAH) * E1K
C
C     MATRICES BUILT TIME TO MULTIPLY
C
  100 DO 110 K=1,N
      DO 110 L=1,N
      Q(K,L) = 0.
      QI(K,L) = 0.
      DO 110 M1=1,N
      Q(K,L) = Q(K,L) + A(K,M1) * G(M1,L)
      QI(K,L) = QI(K,L) + A(K,M1) * GI(M1,L)
  110 CONTINUE
      N2 = 2*N
      DO 130 K=1,N
      DO 130 L=1,N2,2
      IT = L/2+1
      P(K,L) = 0.
      P(K,L+1) = 0.
      DO 120 M1=1,N
      P(K,L) = P(K,L) + Q(K,M1) * H(M1,IT)
      P(K,L+1) = P(K,L+1) + QI(K,M1)*H(M1,IT)
  120 CONTINUE
      P(K,L) = P(K,L) * CONST
      P(K,L+1) = P(K,L+1) * CONST
  130 CONTINUE
C
C     PACK OUT
C
      NN = NN+N
      DO 150 J=1,N2,2
      DO 140 K=1,N
      PC(K) = CMPLX(P(K,J),P(K,J+1))
  140 CONTINUE
      CALL PACK(PC,AJJL,MCB)
  150 CONTINUE
      II = II+N
  200 CONTINUE
      RETURN
      END
