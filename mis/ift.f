      SUBROUTINE IFT
C
C     INVERSE FOURIER TRANSFORM MODULE (IFT)
C
C     DMAP CALLING SEQ.
C
C     IFT   UHVF,CASECC,TRL,FOL/UHVT,TOL/C,Y,IFTM
C
      INTEGER SYSBUF,      IZ(1),UHVF,UHVT,CASECC,TRL,TOL,FOL,NAME(2),
     1  MCB(7),FILE,MCB1(7)
      COMMON /SYSTEM/SYSBUF,NOUT
      COMMON /PACKX/IT1,IT2,II,JJ,INCR
      COMMON /UNPAKX/IT3,II1,JJ1,INCR1
      COMMON /CONDAS/ PHI,TWOPI
      COMMON  /ZZZZZZ/ Z(1)
      COMMON /BLANK/ IFTM
      EQUIVALENCE (Z(1),IZ(1))
      DATA UHVF,CASECC,TRL,FOL,UHVT,TOL/101,102,103,104,201,202/
      DATA NAME /4HIFT , 1H  /
C
C     VARIABLE CORE
C
C        CONTENT         LENGTH         POINTER
C        -------         ------         -------
C     FOL                NFREQ          IFREQ
C     TSTEP              NGROUP*3       ITSTP
C     UHVF               NMODES*NFREQ*2 IUHVF
C     CK                 NBIG           ICK
C     SK                 NBIG           ISK
C     UDOT               NMODES*NFREW*2 IUDOT
C     UHVT               NMODES         IUVT
C
C
C
C     PUT FOL INTO CORE
C
      NZ = KORSZ(IZ)
      IBUF1 = NZ-SYSBUF+1
      IBUF2 = IBUF1-SYSBUF
      NZ = NZ-2*SYSBUF
      FILE = FOL
      CALL OPEN(*900,FOL,IZ(IBUF1),0)
      CALL FREAD(FOL ,IZ,-2,0)
      CALL READ(*910,*10,FOL,IZ,NZ,0,NFREQ)
      CALL MESAGE(-8,0,NAME)
   10 CALL CLOSE(FOL ,1)
      IFREQ = 1
      NZ = NZ-NFREQ
      ITSTP=NFREQ+1
C
C     DEFINE BASIC SIZES
C
      MCB(1) = UHVF
      CALL RDTRL(MCB)
      NLOAD = MCB(2)/NFREQ
      NMODES = MCB(3)
      MCB(1) = UHVT
      MCB(2) = 0
      MCB(5) = 1
      MCB(6) = 0
      MCB(7) = 0
      K = NFREQ + 2*(NMODES*NFREQ*2) + NMODES
      IF(K .GT. NZ) CALL  MESAGE(-8,0,NAME)
C
C     DETERMINE IF EQUAL FREQ - CONVERT TO W'S
C
      DELW = Z(IFREQ+1)-Z(IFREQ)
      EPSI = DELW*1.E-6
      J = NFREQ-1
      IEQUAL = 1
      DO 20 I=1,J
      M = IFREQ+I-1
      IF( ABS(Z(M+1)-Z(M)-DELW) .GE. EPSI) IEQUAL = 0
      Z(M) = Z(M)*TWOPI
   20 CONTINUE
      Z(IFREQ+NFREQ-1) = Z(IFREQ+NFREQ-1)*TWOPI
      DELW = DELW*TWOPI
C
C     FIRST FREQUENCY MUST BE MULTIPLE OF DELW
C
      NBIG = ABS(Z(IFREQ)/DELW)+.1
      IF(ABS(FLOAT(NBIG)*DELW-ABS(Z(IFREQ))) .GT. EPSI) IEQUAL = 0
      LLL = NBIG -1
C
C     FIND TSTEP IN TRL
C
      CALL GOPEN(CASECC,IZ(IBUF1),0)
      CALL FREAD(CASECC,0,-37,0)
      CALL FREAD(CASECC,J,1,0)
      CALL CLOSE(CASECC,1)
      FILE = TRL
      CALL OPEN(*900,TRL,IZ(IBUF1),0)
      CALL FREAD(TRL,MCB1,3,1)
      M = MCB1(3)
      CALL SKPREC(TRL,M)
   25 CONTINUE
      CALL FREAD(TRL,M,1,0)
      IF(M .EQ. J) GO TO 30
      CALL FREAD(TRL,0,0,1)
      GO TO 25
C
C     FOUND TSTEP
C
   30 CALL READ(*910,*40,TRL,IZ(ITSTP),NZ,0,NGROUP)
      CALL MESAGE(-8,0,NAME)
   40 NZ = NZ-NGROUP
      IUHVF = ITSTP+NGROUP
      CALL CLOSE(TRL,1)
      NGROUP = NGROUP/3
      IF(NGROUP .NE. 1) IEQUAL = 0
      IF( IEQUAL .EQ. 0 ) GO TO 50
C
C     FORCE WAT TO BE INTEGER MULTIPLE OF TWOPI/N
C
      FBIG = TWOPI/(DELW*Z(ITSTP+1))
      NBIG = FBIG+.9
      Z(ITSTP+1) = TWOPI/(FLOAT(NBIG)*DELW)
   50 CONTINUE
C
C     BUILD / WRITE TOL
C
      FILE = TOL
      CALL OPEN(*900,TOL,IZ(IBUF1),1)
      CALL FNAME(TOL,MCB1)
      CALL WRITE(TOL,MCB1,2,0)
      DELT = Z(ITSTP+1)
      T = 0.0
      N = 0
      M = ITSTP
      DO 60 I=1,NGROUP
      NSTEP = IZ(M)
      IF(I .EQ. 1) NSTEP = NSTEP +1
      M = M+3
      DO 70 J=1,NSTEP
      CALL WRITE(TOL,T,1,0)
      N = N+1
      IF(J .EQ. NSTEP  .AND. I .NE. NGROUP) DELT = Z(M+1)
      T = T+DELT
   70 CONTINUE
   60 CONTINUE
      CALL WRITE(TOL,0,0,1)
      CALL CLOSE(TOL,1)
      MCB1(1) = TOL
      MCB1(2) = NGROUP
      MCB1(3) = N
      MCB1(4) = 0
      MCB1(5) = 0
      MCB1(6) = 0
      MCB1(7) = 0
      CALL WRTTRL(MCB1)
C
C     BUILD TABLE OF CK, SK
C
      ICK = IUHVF + 2*NMODES*NFREQ
      ISK = ICK
      IUDOT = ISK
      IF( IEQUAL .EQ. 0 ) GO TO 100
      ISK = ICK + NBIG
      IUDOT = ISK + NBIG
      M = ICK
      M1 = ISK
      M2 = ISK
      J = IUDOT
      RP = COS(TWOPI/FLOAT(NBIG))
      CP = SIN(TWOPI/FLOAT(NBIG))
      I = M
      N = M1+1
      L = M2
      KK = J
      Z(I) = 1.0
      Z(L) = 0.0
   65 IF(M1-I-2) 61,62,63
   62 CMNR = -1.
      CMNC = 0.
      GO TO 64
   63 CMNR = RP*Z(I) -CP*Z(L)
      CMNC = CP*Z(I) +RP*Z(L)
   64 I = I+1
      L = L+1
      M1 = M1-1
      KK = KK-1
      Z(I) = CMNR
      Z(L) = CMNC
      Z(M1) = CMNR
      Z(KK) = -CMNC
      GO TO 65
   61 CONTINUE
C     GET READY FOR OUTPUTS
C
  100 CALL GOPEN(UHVF,IZ(IBUF1),0)
      CALL GOPEN(UHVT,IZ(IBUF2),1)
      IT1 = 1
      IT2 = 1
      II = 1
      JJ=NMODES
      INCR = 1
      IT3 = 3
      II1 = 1
      JJ1 = NMODES
      INCR1 = 1
      IUVT = IUDOT
      IF(IFTM .EQ. 2) IUVT = IUVT+2*NFREQ*NMODES
      ASSIGN 235 TO IHOP
C
C     BEGIN LOOP ON LOADS
C
      DO 200 I=1,NLOAD
C
C     PUT UHVF INTO CORE
C
      DO 110 J=1,NFREQ
      M = IUHVF+(J-1)*NMODES*2
      CALL UNPACK(*120,UHVF,Z(M))
      GO TO 110
  120 CALL ZEROC(Z(M),2*NMODES)
  110 CONTINUE
      IF(IFTM .NE. 2) GO TO 150
      ASSIGN 236 TO IHOP
C
C     COMPUTE SPLINE FIT FOR U DOT
C
C
C     COMPUTE A'S
C
      IAP = IUVT + NMODES
      M =NFREQ + IAP - 1
      Z(M) = 0.0
      L = NFREQ-2
      IF(L .LE. 0) GO TO 126
      DO 125 J=1,L
      M = IAP + NFREQ -J-1
      N = IFREQ + NFREQ-J-1
      Z(M) = (Z(N) - Z(N-1))/(2.*(Z(N+1)-Z(N-1))-(Z(N+1)-Z(N))*Z(M+1))
  125 CONTINUE
  126 CONTINUE
C
C     COMPUTE U DOT DOT
C
      DO 122 M1=1,NMODES
      M = IUDOT +(NFREQ-1)*NMODES*2 +(M1-1)*2
      Z(M) = 0.0
      Z(M+1) = 0.0
C
C     BEGIN BACKWARD PASS
C
      M2= IUHVF +(NFREQ-1)*NMODES*2 +(M1-1)*2
      IF(L .LE. 0) GO TO 122
      DO 130 J=1,L
      N2 = M
      M = M-NMODES*2
      N = IFREQ + NFREQ -J-1
      M2 = M2-NMODES*2
      KK = IAP + NFREQ-J
      LL = M2+2*NMODES
      RP = Z(N+1) - Z(N)
      CP = Z(N) -Z(N-1)
      N1 = M2-2*NMODES
      Z(M) = (6. *((Z(LL)-Z(M2))/RP-(Z(M2)-Z(N1))/CP)-RP*Z(KK)*Z(N2))
     U   /CP
      Z(M+1) = (6.*((Z(LL+1)-Z(M2+1))/RP-(Z(M2+1)-Z(N1+1))/CP)-RP*Z(KK)*
     1  Z(N2+1))/CP
  130 CONTINUE
  122 CONTINUE
C
C     BEGIN FORWARD PASS
C
      DO 135 M1=1,NMODES
      M = IUDOT +(M1-1)*2
      M2 = IUHVF +(M1-1)*2
      N1 = M2+2*NMODES
      LL = M+2*NMODES
      RP = Z(IFREQ+1) -Z(IFREQ)
      Z(M) =(6.*(Z(N1)-Z(M2))/RP-RP*Z(IAP+1)*Z(LL))/(6.*Z(IFREQ)+(RP)*
     1  (2.-Z(IAP+1)) )
      Z(M+1) = 0.0
      DO 138 J=2,NFREQ
      KK = IAP+J-1
      M2 = M
      M = M+2*NMODES
      Z(M) = Z(KK)*(Z(LL) - Z(M2))
      Z(M+1) = Z(KK)*(Z(LL+1)-Z(M2+1))
      LL = LL + 2*NMODES
  138 CONTINUE
  135 CONTINUE
  150 CONTINUE
      T = 0.0
      N = 0
      M = ITSTP
      DELT = Z(ITSTP+1)
C
C     BEGIN LOOP ON TIMES
C
      DO 160 L=1,NGROUP
      NSTEP = IZ(M)
      IF(L .EQ. 1) NSTEP = NSTEP+1
      M = M+3
      DO 170 J=1,NSTEP
      TT = T
      CALL ZEROC(Z(IUVT),NMODES)
C
C     BEGIN LOOP ON FREQUENCIES
C
      LX = LLL
      DO 180 LL=1,NFREQ
      LX = LX+1
      WN = Z(IFREQ+LL-1)
      IF(LL .EQ. 1) GO TO 191
      WNM1 = Z(IFREQ+LL-2)
  191 IF(LL .EQ. NFREQ) GO TO 192
      WNP1 = Z(IFREQ+LL)
  192 CONTINUE
      IF(IEQUAL .EQ. 0) GO TO 190
      KK = MOD(LX*N,NBIG)
      CK = Z(ICK+KK)
      SK = Z(ISK+KK)
      GO TO 195
  190 CK = COS(WN*TT)
      SK = SIN(WN*TT)
  195 CONTINUE
C
C     COMPUTE CMN, DMN
C
      IF(IFTM .NE. 0) GO TO 220
C
C     IFTM  =0
C
      CMNC = 0.0
      IF(LL .EQ. 1) GO TO 196
      IF(LL .EQ. NFREQ) GO TO 197
      CMNR = (WNP1-WNM1)*.5
      GO TO 230
  196 CONTINUE
      CMNR = WNP1-WN
      IF(WN .EQ. 0.0) CMNR = CMNR*.5
      GO TO 230
  197 CMNR = WN -WNM1
      GO TO 230
C
C     IFTM = 1
C
  220 CONTINUE
      IF(LL .EQ. 1) GO TO 221
      IF(LL.GT. 2 .AND. IEQUAL .NE. 0 .AND. LL .NE. NFREQ) GO TO 223
      R1 = WN-WNM1
      CALL IFTE2(-TT*R1,RP,CP)
      CMNR = R1*.5*RP
      CMNC = R1*.5*CP
      GO TO 222
  221 CMNR = 0.
      CMNC = 0.
  222 CONTINUE
      IF(LL .EQ. NFREQ) GO TO 223
      R2 = WNP1-WN
      CALL IFTE2(TT*R2,RP,CP)
      CMNR = CMNR+R2*.5*RP
      CMNC = CMNC+R2*.5*CP
  223 IF(IFTM .EQ. 2) GO TO 229
      DMNR = 0.0
      DMNC = 0.0
      GO TO 230
  229 CONTINUE
C
C     IFTM = 2
C
      IM2 = IUDOT -2 +(LL-1)*NMODES*2
      IF(LL .EQ. 1) GO TO 224
      IF(LL .GT. 2 .AND.  IEQUAL .NE. 0 .AND. LL .NE. NFREQ) GO TO 230
      CALL IFTG(-TT*R1,RP,CP)
      R1 = -R1*R1*R1/24.
      DMNR = R1*RP
      DMNC = R1*CP
      GO TO 228
  224 CONTINUE
      DMNR = 0.0
      DMNC = 0.0
  228 CONTINUE
      IF(LL .EQ. NFREQ) GO TO 230
      CALL IFTG(TT*R2,RP,CP)
      R2 = -R2*R2*R2/24.
      DMNR = DMNR+R2*RP
      DMNC = DMNC+R2*CP
  230 CONTINUE
      IM1 = IUHVF-2 +(LL-1)*NMODES*2
C
C     BEGIN LOOP ON MODES
C
      DO 240 KK=1,NMODES
      IM = IM1+2*KK
      RP = CMNR*Z(IM)-CMNC*Z(IM+1)
      CP = CMNC*Z(IM)+CMNR* Z(IM+1)
      GO TO IHOP,(235,236)
  236 CONTINUE
      IM = IM2+2*KK
      RP = RP+DMNR*Z(IM)-DMNC*Z(IM+1)
      CP = CP+DMNC*Z(IM)+DMNR*Z(IM+1)
  235 CONTINUE
      Z(IUVT+KK-1) = Z(IUVT+KK-1) + RP*CK-CP*SK
C
C     END LOOP ON MODES
C
  240 CONTINUE
C
C     END LOOP ON FREQUENCIES
C
  180 CONTINUE
      DO 181 KK=1,NMODES
      Z(IUVT+KK-1) = Z(IUVT+KK-1)/PHI
  181 CONTINUE
      CALL PACK(Z(IUVT),UHVT,MCB)
      DO 182 KK =1,2
      CALL BLDPK(1,1,UHVT,0,0)
      CALL BLDPKN(UHVT,0,MCB)
  182 CONTINUE
      IF(J .EQ. NSTEP) DELT = Z(M+1)
      T = T + DELT
      N = N+1
  170 CONTINUE
C
C     END LOOP ON TIME
C
  160 CONTINUE
C
C     END LOOP ON LOADS
C
  200 CONTINUE
      CALL CLOSE(UHVF,1)
      CALL CLOSE(UHVT,1)
      CALL WRTTRL(MCB)
      RETURN
C
C     ERROR MESSAGES
C
  900 N1=-1
  901 CALL MESAGE(N1,FILE,NAME)
      CALL PEXIT
  910 N1=-2
      GO TO 901
      END
