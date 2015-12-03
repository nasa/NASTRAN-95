      SUBROUTINE AMGT1B (Q,NSTNS2,C1SBAR,C2SBAR)        
C        
C      SUBSONIC RAO (CASCADES) CODE FOR SWEPT TURBOPROPS.        
C        
      INTEGER         SLN        
      REAL            M,KAPPA,MU,MUS,LAMDA,LAMDM,NU,X(20),DISP(20,10),  
     1                W(8),WW(8),M2SBAR        
      COMPLEX         Q(NSTNS2,NSTNS2),LOADS(21),STT(20),SUM1,SUM2,STTI,
     1                AN(401),AB(401),FK(401),CN(401),CB(401),PD(401),  
     2                SO(100),S1(100),P(50),A(20,40),FF,ST,STP,FG,FS,FO,
     3                SLOPE        
      CHARACTER       UFM*23        
      COMMON /XMSSG / UFM        
      COMMON /TAMG1L/ IREF,MINMAC,MAXMAC,NLINES,NSTNS,REFSTG,REFCRD,    
     1                REFMAC,REFDEN,REFVEL,REFSWP,SLN,NSTNSX,STAG,      
     2                CHORD,DCBDZB,BSPACE,MACH,DEN,VEL,SWEEP,AMACH,     
     3                REDF,BLSPC,AMACHR,TSONIC        
      COMMON /AMGMN / MCB(7),NROW,DUM(2),REFC,SIGMA,RFREQ        
      COMMON /SYSTEM/ IBUF,IOUT        
      DATA    W     / 1.48283,  .89414, .83521,  .66721,        
     1                 .64172,  .55519, .54026,  .48547 /        
      DATA    WW    / 1.13333,-0.00036,0.18796,-0.00027,        
     1                0.08469,-0.00022,0.05049,-0.00019 /        
C        
C     THEORY DEPENDENT RESTRICTION OF NO MORE THAN 10 COMPUTING        
C     STATIONS PER STREAMLINE IS REFLECTED IN CODING.        
C        
      IF (NSTNS .GT. 10) GO TO 1000        
C        
      M      = AMACH        
      OMEGA  = REDF        
      SS     = 2*BLSPC        
      DELTM  =-SIGMA        
      XLAM   = STAG        
      NM     = NSTNS        
      NNM    = NSTNS2        
      CSBAR  = .25*(DEN*VEL**2*CHORD**2)/(REFDEN*REFVEL**2)        
      CSBAR1 = 2.0/CHORD        
      M2SBAR =-DCBDZB/CHORD        
      C2SSCH = CSBAR1*C2SBAR        
      CSBLSB = CSBAR*CSBAR1        
      CSBM2S = CSBAR*M2SBAR        
      N      = 20        
      PI     = 3.141593        
      PI2    = PI*2        
      CON    = 1.0E-5        
      NNN    = 100        
      KKK    = 2*NNN + 1        
      DELTM  = DELTM/360        
      XL     = XLAM*PI/180        
      B      = 1.0/N        
      B2     = 2*B        
      D      = SS*SIN(XL)        
      HH     = SS*COS(XL)        
      BETA   = SQRT(1. - M**2)        
      H      = HH*BETA        
      ZER    = 0.0        
      S      = SQRT(H**2 + D**2)        
      LAMDM  = ATAN(D/H)        
      CL     = COS(LAMDM)        
      SL     = SIN(LAMDM)        
      NU     = OMEGA/BETA**2        
      KAPPA  = M*NU        
      LAMDA  = M*KAPPA        
      DELTA  = DELTM + LAMDA*D/PI2        
      MU     = KAPPA*S/PI2        
      MUS    = MU**2        
      FF     = (0.0,1.0)        
      FG     = CMPLX(ZER,NU*S)        
      L      = 1        
      CC     = DELTA**2 - MUS        
      IF (CC .EQ. 0.0) GO TO 200        
      IF (CC .LT. 0.0) FK(L) = SQRT(-CC)*FF        
      IF (CC .GT. 0.0) FK(L) = SQRT(CC)        
      AN(L)  = FK(L)*CL + FF*DELTA*SL        
      AB(L)  = FK(L)*CL - FF*DELTA*SL        
      PD(L)  = FK(L)*(PI2*AB(L) + FG)        
      CK     = PI2*B/S        
      CN(L)  = CEXP(-AN(L)*CK)        
      CB(L)  = CEXP(-AB(L)*CK)        
      DO 20 I = 1,NNN        
      L      = L + 1        
      CC     = (DELTA+I)**2 - MUS        
      IF (CC .EQ. 0.0) GO TO 200        
      IF (CC .LT. 0.0) FK(L) = SQRT(-CC)*FF        
      IF (CC .GT. 0.0) FK(L) = SQRT(CC)        
      AN(L)  = FK(L)*CL + (DELTA+I)*FF*SL        
      AB(L)  = FK(L)*CL - (DELTA+I)*FF*SL        
      PD(L)  = FK(L)*(PI2*AB(L) + FG)        
      CN(L)  = CEXP(-AN(L)*CK)        
      CB(L)  = CEXP(-AB(L)*CK)        
      L      = L + 1        
      CC     = (DELTA-I)**2 - MUS        
      IF (CC .EQ. 0.0) GO TO 200        
      IF (CC .GT. 0.0) FK(L) = SQRT(CC)        
      IF (CC .LT. 0.0) FK(L) = SQRT(-CC)*FF        
      AN(L)  = FK(L)*CL + (DELTA-I)*FF*SL        
      AB(L)  = FK(L)*CL - (DELTA-I)*FF*SL        
      PD(L)  = FK(L)*(PI2*AB(L) + FG)        
      CN(L)  = CEXP(-AN(L)*CK)        
      CB(L)  = CEXP(-AB(L)*CK)        
   20 CONTINUE        
      STP    = 0.0        
      L      = 1        
      ST     = ((1-CN(L))/AN(L) + (1-CB(L))/AB(L))/FK(L)        
      DO 25 I = 2,KKK,2        
      L      = I        
      ST     = ((1-CN(L))/AN(L) + (1-CB(L))/AB(L))/FK(L) + ST        
      L      = L + 1        
      ST     = ((1-CN(L))/AN(L) + (1-CB(L))/AB(L))/FK(L) + ST        
      IF (CABS(ST-STP) .LT. CON) GO TO 30        
      STP    = ST        
   25 CONTINUE        
   30 CONTINU E        
      SO(1)  =-ST*S/(2*PI2*B2)        
      DO 40 J = 2,N        
      JK     = 2*(J-1)        
      L      = 1        
      STP    = 0.0        
      ST     = CN(L)**JK/FK(L)        
      DO 32 I = 2,KKK,2        
      L      = L + 1        
      ST     = CN(L)**JK/FK(L) + ST        
      L      = L + 1        
      ST     = CN(L)**JK/FK(L) + ST        
      IF (CABS(ST-STP) .LT. CON) GO TO 35        
   32 STP    = ST        
   35 SO(J)  =-0.5*ST        
   40 CONTINUE        
      N1     = N + 1        
      N2     = 3*N - 1        
      DO 50 J = N1,N2        
      JK     = J - N        
      STP    = 0.0        
      L      = 1        
      ST     = CB(L)**JK/FK(L)        
      DO 42 I = 2,KKK,2        
      L      = L + 1        
      ST     = CB(L)**JK/FK(L) + ST        
      L      = L + 1        
      ST     = CB(L)**JK/FK(L) + ST        
      IF (CABS(ST-STP) .LT. CON) GO TO 45        
   42 STP    = ST        
   45 SO(J)  =-0.5*ST        
   50 CONTINUE        
      DO 55 J = 1,N        
      JK     = (J-1)*2 + 1        
      L      = 1        
      STP    = 0.0        
      ST     = AN(L)*CN(L)**JK/FK(L)        
      DO 52 I = 2,KKK,2        
      L      = L + 1        
      ST     = AN(L)*CN(L)**JK/FK(L) + ST        
      L      = L + 1        
      ST     = AN(L)*CN(L)**JK/FK(L) + ST        
      IF (CABS(ST-STP) .LT. CON) GO TO 54        
      STP    = ST        
   52 CONTINUE        
   54 S1(J)  =-PI/S*ST        
   55 CONTINUE        
      N1     = N + 1        
      N2     = 2*N        
      DO 60 J = N1,N2        
      JK     = (J-N1)*2 + 1        
      L      = 1        
      STP    = 0.0        
      ST     = AB(L)*CB(L)**JK/FK(L)        
      DO 57 I = 2,KKK,2        
      L      = L + 1        
      ST     = AB(L)*CB(L)**JK/FK(L) + ST        
      L      = L + 1        
      ST     = AB(L)*CB(L)**JK/FK(L) + ST        
      IF (CABS(ST-STP) .LT. CON) GO TO 59        
      STP    = ST        
   57 CONTINUE        
   59 S1(J)  = PI/S*ST        
   60 CONTINUE        
      DO 64 J = 1,N        
      JK     = (J-1)*2 + 1        
      L      = 1        
      STP    = 0.0        
      ST     = CB(L)**JK/PD(L)        
      DO 61 I = 2,KKK,2        
      L      = L + 1        
      ST     = CB(L)**JK/PD(L) + ST        
      L      = L + 1        
      ST     = CB(L)**JK/PD(L) + ST        
      IF (CABS(ST-STP) .LT. CON) GO TO 62        
      STP    = ST        
   61 CONTINUE        
   62 P(J)   =-S/2*ST        
   64 CONTINUE        
      FG     = CMPLX(ZER,-NU*B)        
      FG     = 1/(CEXP(FG) + CMPLX(ZER,NU*B2))        
      FS     = CMPLX(ZER,NU)        
      CJ     = (NU*BETA)**2        
      L      = 0        
      CT     = 2*KAPPA**2*B        
      DO 70 J = 1,N        
      DO 70 I = 1,N        
      L      = L + 1        
      NK     = I - J + 1        
      NK1    = I - J        
      NK2    = NK1 + 1        
      IF (I .EQ. J) NK1 = N + 1        
      IF (I .EQ. J) NK2 = 1        
      IF (J .LE. I) GO TO 65        
      NK1    = N + J - I + 1        
      NK2    = NK1 - 1        
      NK     = N + 2*(J-I)        
   65 A(I,J) = S1(NK1) - S1(NK2) + CT*SO(NK)        
      IF (J .NE. N) GO TO 70        
      NK     = N + 2*(J-I) + 1        
      NK2    = J - I + 1        
      A(I,J) = A(I,J) - FG*(S1(NK1) + SO(NK)*FS + CJ*P(NK2))        
   70 CONTINUE        
      X(1)   =-1.0 + B        
      DO 81 I = 2,N        
   81 X(I)   = X(I-1) + B2        
      N1     = N  + NM        
      NN     = N1        
      NN1    = NN + NM        
      N1N    = N  - 1        
      N1M    = NM - 1        
      N11    = N  + 1        
      NN11   = NN + 1        
      N22    = N  + 2        
      NN22   = NN + 2        
      FO     = FF*OMEGA        
      TANLAM = TAN(SWEEP*PI/180.0)        
      DLSDZB = DCBDZB/2.0        
      TD     = TANLAM*DLSDZB        
      DO 75 I = 1,N        
      DISP(I,1) =-1.0        
      DISP(I,2) =-1.0 - X(I)        
      STT(I) = CEXP(-FF*LAMDA*X(I))*PI2/BETA        
      STTI   = STT(I)        
      A(I,N11 ) = STTI*DISP(I,1)*(FO+TD)        
      A(I,NN11) = STTI*DISP(I,1)*TANLAM        
      A(I,N22 ) = STTI*(DISP(I,2)*(FO+TD)-1.)        
   75 A(I,NN22) = STTI*DISP(I,2)*TANLAM        
      DO 83 JJ = 3,NM        
      NF     = N  + JJ        
      NNF    = NN + JJ        
      CON2   = PI*(JJ-2)/2        
      DO 83 I = 1,N        
      CON    = CON2*DISP(I,2)        
      DISP(I,JJ) = SIN(CON)        
      A(I,NF ) = STT(I)*(DISP(I,JJ)*(FO+TD)-CON2*COS(CON))        
   83 A(I,NNF) = STT(I)*DISP(I,JJ)*TANLAM        
CWKBR SPR93019 10/93 CALL GAUSS (A,N,NN1)        
      CALL GAUSS2 (A,N,NN1) 
      DO 95 J = 1,NNM        
      NF     = N + J        
      DO 84 I = 1, N        
   84 LOADS(I) = A(I,NF)        
C        
      SLOPE   = LOADS(2)/3./B        
      A(1,NF) = 2.*CEXP(LAMDA*FF*X(1))*(FF*NU*LOADS(1) + SLOPE)        
C        
      SLOPE   = (LOADS(N) - LOADS(N1N))/B2        
      A(N,NF) = 2.*CEXP(LAMDA*FF*X(N))*(FF*NU*LOADS(N) + SLOPE)        
C        
      DO 85 I = 2,N1N        
      SLOPE   = (LOADS(I+1) - LOADS(I-1))/4./B        
   85 A(I,NF) = 2.*CEXP(LAMDA*FF*X(I))*(FF*NU*LOADS(I) + SLOPE)        
   95 CONTINUE        
      DO 86 I = 1,N        
      A(I,1) = SQRT((1-X(I))/(1+X(I)))        
      DO 87 J = 2,N1M        
   87 A(I,J) =-DISP(I,J+1)        
      DO 86 J = NM,N        
      CON2   =-PI*(J-1)*DISP(I,2)/2        
   86 A(I,J) = SIN(CON2)        
CWKBR SPR93019 10/93      CALL GAUSS (A,N,NN1)        
      CALL GAUSS2 (A,N,NN1) 
      A(1,1) = C2SSCH*PI+C1SBAR*PI/2.        
      A(2,1) = (C2SSCH+C1SBAR)*PI/2.        
      CON    = 1.        
      CONN   = 1.        
      DO 88 J = 1,N1N        
      A(1,J+1) = (C2SSCH*CON+C1SBAR*CONN)*4./J/PI        
      A(2,J+1) = (C2SSCH+2.*C1SBAR)*CONN*4./J/PI-CON*C1SBAR*32./        
     1           (J*PI)**3        
      CON    = 1. - CON        
   88 CONN   =-CONN        
      DO 90 I = 3,NM        
      IR     = I - 2        
      DO 90 J = 2,N        
      IS     = J - 1        
      IF (IR .EQ. IS) GO TO 901        
      IF ((IR+IS)/2*2.EQ.(IR+IS)) GO TO 902        
      A(I,J) =-C1SBAR*16.*IR*IS/(PI*(IR+IS)*(IR-IS))**2        
      GO TO 90        
  902 A(I,J) = (0.,0.)        
      GO TO 90        
  901 A(I,J) = C2SSCH + C1SBAR        
   90 CONTINUE        
      DO 91 J = 3,NM        
   91 A(J,1) = C2SSCH*W(J-2)+C1SBAR*WW(J-2)        
      DO 160 J = 1,NM        
      DO 160 K = 1,NM        
      NF     = N  + K        
      NNF    = NN + K        
      SUM1   = (0.,0.)        
      SUM2   = (0.,0.)        
      DO150 I = 1,N        
      SUM1 = SUM1 + A(J,I)*A(I,NF)        
  150 SUM2 = SUM2 + A(J,I)*A(I,NNF)        
      Q(J,K   ) = CSBLSB*SUM1 + CSBM2S*SUM2        
      Q(J,K+NM) = CSBAR*SUM2        
      Q(J+NM,K) = (0.,0.)        
  160 Q(J+NM,K+NM) = (0.,0.)        
  200 RETURN        
C        
 1000 WRITE  (IOUT,3001) UFM,SLN,NSTNS        
 3001 FORMAT (A23,' - AMG MODULE - NUMBER OF COMPUTING STATIONS ON ',   
     1       'STREAMLINE',I8,4H IS ,I3,1H., /39X,'SUBSONIC CASCADE ',   
     2       'ROUTINE AMGT1B ALLOWS ONLY A MAXIMUM OF 10.')        
      CALL MESAGE (-61,0,0)        
      RETURN        
      END        
