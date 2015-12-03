      SUBROUTINE MINTRP(NI,XI,ND,XD,TYPE,SYMM1,SYMK1,DZ,INFILE,OUTFIL,
     *                  SCR,SCR1,G,NCORE,NOGO,IPRES)
C
      INTEGER SYSBUF,SYMK1,SYMM1,ISNG,SCR,SCR1,OUTFIL,SCRM
      INTEGER A,B,C,D,NAME(2),TYPE,BUFF,GPOINT
      LOGICAL NIMAG
      LOGICAL SPEC
      COMPLEX ALPHA
      DOUBLE PRECISION AR,AI
      DIMENSION XI(1),XD(2),G(1)
C
      COMMON /SYSTEM/ SYSBUF
      COMMON / PACKX/ ITI,ITO,II,NN,INCR
      COMMON /MPYADX/ A(7),B(7),C(7),D(7),NWORDS,NT,ISAB,ISC,IPRE,SCRM
      COMMON /SADDX / NMAT,LCORE,MA(7),ITA,ALPHA(2),DUM(48),MC(7)
      COMMON /UNPAKX/ IOUT,IN,NNN,INCRU
C
      EQUIVALENCE (ALPHA(1),AR),(ALPHA(2),AI)
C
      DATA NAME /4HMINT,4HRP  /
C
C-----------------------------------------------------------------------
C
      SPEC = .FALSE.
      NOGO = 0
C
C     DETERMINE TYPE OF CALL FOR G
C     NEGATIVE VALUE FOR KO CALL LSPLIN, POSITIVE CALL SSPLIN
C
C
C     CHECK CORE NEED AT LEAST 1 BUFFER + G
C
      ITY = IABS(TYPE)
      KD = 0
      IF(ITY.GT.3) KD=1
      NCOL = (1+KD)*ND
      IF(SYSBUF+NCOL*NI  .GT. NCORE) CALL MESAGE(-8,0,NAME)
C
C     PROTECT AGAINST BAD CALL
      IF(SYMK1.LT.0) SYMK1 = -1
      IF(SYMM1.LT.0) SYMM1 = -1
      IF(SYMK1.GT.0) SYMK1 = 1
      IF(SYMM1.GT.0) SYMM1 = 1
C     TRANSPOSE FLAG ON
      KT = 1
C     SPECIAL CASE
      IF(ND.EQ.1.AND.ITY.LT.4) GO TO 300
   10 IF(TYPE.LT.0) GO TO 100
      CALL SSPLIN(NI,XI,ND,XD,SYMM1,SYMK1,KD,KT,DZ,G,NCORE,ISNG)
      IF(ISNG.EQ.2) GO TO 999
      GO TO 200
  100 NII = 2*NI
      DO 110 I=1,NII,2
      XI(I) = 0.0
  110 CONTINUE
      NII = 2*ND
      DO 120 I = 1,NII,2
      XD(I) = 0.0
  120 CONTINUE
      CALL LSPLIN(NI,XI,ND,XD,SYMK1,KD,KT,DZ,-1.0,-1.0,1.0,G,NCORE,ISNG)
      IF(ISNG.EQ.2) GO TO 999
C     PUT OUT G
  200 BUFF = NCORE-SYSBUF+1
      NIMAG = .TRUE.
      IF(ITY.EQ.3.OR.ITY.EQ.6) NIMAG = .FALSE.
      IF(NIMAG) GO TO 210
      ITI = SCR
      SCR = OUTFIL
      OUTFIL = ITI
  210 ITO = 1
      JJ = NCOL
      ITI = 1
      NN = NI
      B(3) = NI
      B(5) = 1
      GPOINT = 1
  215 INCR = 1
      J = 1
      II = 1
      B(1) = SCR
      B(2) = 0
      B(4) = 2
      B(6) = 0
      B(7) = 0
      CALL GOPEN(SCR,G(BUFF),1)
      DO 220 I = J,JJ
      CALL PACK(G(GPOINT),SCR,B)
      GPOINT = GPOINT + NI
  220 CONTINUE
      CALL CLOSE(SCR,1)
      CALL WRTTRL(B)
      IF(SPEC) GO TO 1000
C
C     MULT INFILE BY G
C
      C(1) = 0
      A(1) = INFILE
      CALL RDTRL(A)
      D(1) = OUTFIL
      D(3) = A(3)
      D(4) = 2
      D(5) = A(5)
      IF(ITY.EQ.2.OR.ITY.EQ.5) D(5) = 1
      IF(D(5).EQ.1.AND.A(5).EQ.4) D(5) = 2
      NWORDS = NCORE
      NT = 0
      ISAB = 1
      IPRE = IPRES
      SCRM = SCR1
      CALL MPYAD(G,G,G)
      CALL WRTTRL(D)
      IF(NIMAG) GO TO 1000
C
C     IMAG PART ONLY WANTED
C
      NMAT = 1
      LCORE = NCORE
      MA(1) = OUTFIL
      CALL RDTRL(MA)
      ITA = 3
      ALPHA(1) = (0.0,-1.0)
      MC(1) = SCR
      MC(2) = MA(2)
      MC(3) = MA(3)
      MC(4) = 2
      MC(5) = MA(5)
      MC(6) = 0
      MC(7) = 0
      AI = -1.0D0
      IF(MA(5) .EQ.4) ITA = 4
      IF(ITA.EQ.4) AR = 0.0D0
      CALL SADD(G,G)
      CALL WRTTRL(MC)
      GO TO 1000
C
C     TEST FOR SPECIAL CASE
C
  300 NII = 2*NI
      K = 0
      DO 310 I = 1,NII,2
      K = K+1
      IF(XI(I).EQ.XD(1).AND.XI(I+1).EQ.XD(2)) GO TO 315
  310 CONTINUE
      GO TO 10
C
C     PACK OUT COLUMN OF INFILE
C
  315 A(1) = INFILE
      CALL RDTRL(A)
      BUFF = NCORE-SYSBUF +1
      CALL GOPEN(INFILE,G(BUFF),0)
      INCRU = 1
      IN = 1
      NNN = A(3)
      IOUT = A(5)
      IF(K.EQ.1) GO TO 330
      K = K-1
      CALL SKPREC(INFILE,K)
  330 CALL UNPACK(*998,INFILE,G)
      CALL CLOSE(INFILE,1)
      SPEC = .TRUE.
      SCR = OUTFIL
      ITI = A(5)
      NN = A(3)
      JJ = 1
      GPOINT = 1
      IF(ITY.EQ.3) GPOINT = 2
      ITO = 1
      IF(ITY .EQ.1) ITO = 3
      IF(A(5).EQ.4) ITO = ITO+1
      B(3) = A(3)
      B(5) = ITO
      GO TO 215
  998 CALL MESAGE(-7,0,NAME)
  999 NOGO = 1
 1000 RETURN
      END
