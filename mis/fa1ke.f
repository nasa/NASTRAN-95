      SUBROUTINE FA1KE (SCR1,KFREQ,BREF,RHO,RREF,FLOOP,NLOOP)
C
      INTEGER         SCR1,MHH,KHH,FLOOP,SYSBUF,MOUT,NAME(2)
      INTEGER         BUF1,TRL(7),FSAVE
C
      REAL            KFREQ,K2B2
C
      COMPLEX         CZ(1)
C
      COMMON /SYSTEM/ SYSBUF
      COMMON /ZZZZZZ/ Z(1)
      COMMON /UNPAKX/ IOUT,INN,NNN,INCR1
C
      EQUIVALENCE     (Z(1),CZ(1))
C
      DATA NAME /4HFA1K,4HE   /
      DATA KHH  /101/, MHH /103/ , MOUT /203/ , FSAVE /201/
C
C     INITILIZE ON FIRST LOOP
C
      IF(FLOOP .GT. 1) GO TO 100
      NCORE = KORSZ(Z)
      TRL(1)= KHH
      CALL RDTRL(TRL)
      N  = TRL(3)
      NN = N*N
      N2 = N*2
      IM = NN * 2
C
C       LOC     SIZE     USE
C
C     IAM1K     N*N*2    A-1 K
C     IKC       N*N*2    K   SCRATCH FOR ALLMAT
C     IMS       N*N*2    M + Q   LAMBDA FOR ALLMAT
C     IPM       N*N*2    M   HELD IN CORE
C     IPK       N*N*2    K   BETWEEN LOOPS
C
      IAM1K = 1
      IKC = IAM1K + IM
      IMS = IKC + IM
      ICP = IMS + IM
      IF(IM*5+SYSBUF.GT.NCORE) CALL MESAGE(-8,0,NAME)
      IPM = NCORE - IM
      IPK = IPM - IM
      NCORE= IPK -1
      BUF1 = NCORE - SYSBUF
      IOUT = 3
      INN  = 1
      NNN  = N
      INCR1= 1
C
C     PUT MHH AND KHH IN CORE
C
      IFL = KHH
      JI  = IPK
   10 CALL GOPEN(IFL,Z(BUF1),0)
      DO 20 I=1,N
      CALL UNPACK(*15,IFL,Z(JI))
      GO TO 16
   15 CALL ZEROC(Z(JI),N2)
   16 JI = JI + N2
   20 CONTINUE
      CALL CLOSE(IFL,1)
      IF(IFL.EQ.MHH) GO TO 40
      IFL = MHH
      JI  = IPM
      GO TO 10
C
C     WRITE A HEADER ON MOUT
C
   40 CALL GOPEN(MOUT,Z(BUF1),1)
      CALL CLOSE(MOUT,2)
C
C              2  2
C     SOLVE   K /B  MHH + (RHO*RREF)/2.0 QHH     KHH
C
  100 K2B2 = (KFREQ*KFREQ) /(BREF*BREF)
      RR2  = (RHO*RREF) / 2.0
      IOUT = 3
      INN  = 1
      NNN  = N
      INCR1= 1
      DO 105 I=1,IKC
  105 Z(I) = 0.0
      JI = IMS
      CALL GOPEN(SCR1,Z(BUF1),0)
      DO 110 I=1,N
      CALL UNPACK(*115,SCR1,Z(JI))
  115 JI = JI+N2
  110 CONTINUE
      CALL CLOSE(SCR1,1)
      ICK = IKC -1
      IKP = IPK -1
      IMP = IPM -1
      ISM = IMS -1
      J   = NN*2
      DO 120 I=1,J
      Z(I+ISM) = Z(I+ISM) * RR2 + Z(I+IMP) * K2B2
      Z(I+ICK) = - Z(I+IKP)
  120 CONTINUE
      CALL INCORE(Z(IMS),N,Z(IKC),Z(IAM1K),N)
C
C     GET EIGENVALUES FROM ALLMAT
C
      IM = IMS + N2
      IN = IM + N2
      L  = 0
      CALL ALLMAT(Z(IAM1K),Z(IMS),Z(IKC),0,0,Z(IM),0,Z(IN),N,L,0)
C
C     WRITE OUT EIGENVALUES ON MOUT
C
      IM = IMS/2
      NL = 2*L
      DO 130 I = 1,L
      IF(CZ(I+IM).NE.(0.0,0.0))CZ(I+IM) = CSQRT(CZ(I+IM))
      IF(AIMAG(CZ(I+IM)) .LT. 0.0) CZ(I+IM) = - CZ(I+IM)
  130 CONTINUE
      CALL GOPEN(MOUT,Z(BUF1),3)
      CALL WRITE(MOUT,Z(IMS),NL,1)
      IF(FLOOP.GE.NLOOP) GO TO 200
      CALL CLOSE(MOUT,3)
      RETURN
C
C     LAST LOOP BUILD FSAVE
C
  200 CALL CLOSE(MOUT,1)
      IBUF2 = BUF1 - SYSBUF
      CALL GOPEN(MOUT,Z(BUF1),0)
      CALL GOPEN(FSAVE,Z(IBUF2),0)
      CALL SKPREC(FSAVE,3)
      CALL CLOSE(FSAVE,2)
      CALL GOPEN(FSAVE,Z(IBUF2),3)
  210 CALL READ(*230,*220,MOUT,Z(1),IBUF2,1,NWR)
  220 CALL WRITE(FSAVE,Z(1),NWR,1)
      GO TO 210
  230 CALL CLOSE(MOUT,1)
      CALL CLOSE(FSAVE,1)
      TRL(1) = FSAVE
      TRL(2) = NLOOP
      TRL(7) = L
      CALL WRTTRL(TRL)
      RETURN
      END
