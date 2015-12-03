      SUBROUTINE MODAC1(CASECC,TOL,TOL1,CASEZZ,CASEYY)
C
C     MODAC1 REDUCES THE NUMBER OF ENTRIES ON TOL TO THE TIMES
C         SPECIFIED BY THE OFREQ SET IN CASECC
C
C     CORE IS        OUT AS FOLLOWS ON RETURN
C
C         CONTENTS            LENGTH  TYPE   POINTER
C         --------            ------  ----   -------
C         NEW TIMES           NFN      R     IFN
C         KEEP REMOVE         NFO      I     IKR
C
C
C
C
C
C
C
C
C
C
C
C
C
      INTEGER SYSBUF,CASECC,TOL,NAME(2),TOL1,FILE,IHD(2),MCB(7),IBUF(6)
      INTEGER CASEZZ,CASEYY,FLAG
      REAL  Z(1)
      COMMON /SYSTEM/ SYSBUF
      COMMON /MODAC3/ NFO,NFN,NZ, ID
      COMMON /ZZZZZZ/ IZ(1)
      EQUIVALENCE   ( Z(1), IZ(1) )
      DATA NAME /4HMODA,4HC1  /
C
C     BRING  IN  CASECC
C
      LW = 6
      IF(ID .EQ. 4) LW = 7
      IBUF1 = NZ -SYSBUF+1
      IBUF2 = IBUF1-SYSBUF
      IBUF3 = IBUF2 -SYSBUF
      CALL GOPEN(CASECC,IZ(IBUF1),0)
      FILE = CASECC
      CALL READ(*900,*10,CASECC,IZ,IBUF2-1,0,IVEC)
      CALL MESAGE(-8,0,NAME)
   10 CONTINUE
      ICC = 0
      CALL CLOSE(CASECC, 1)
      IFROUT =145
      ILSYM = 200
      IVEC  = IVEC+1
      ILIST = IVEC
      IF(ID .EQ. 5) GO TO 600
C
C     BRING IN OLD TIME/FREQ  LIST
C
      FILE = TOL
      CALL OPEN(*900,TOL,IZ(IBUF1),0)
      I = ILIST
      M = 3
      IX= 2
      NFO = NFO + I
      IF(ID .EQ. 2 .OR. ID .EQ. 4) GO TO 25
   20 CALL READ(*910,*30,TOL,IBUF,M,0,FLAG)
      IZ(I) =IBUF(M)
      IZ(I+1)= 0
      I =  I + IX
      M =1
      GO TO 20
   25 CALL FWDREC(*910,TOL)
      CALL FWDREC(*910,TOL)
   26 CALL READ(*910,*30,TOL,IBUF,LW,0,FLAG)
      IZ(I) = IBUF(4)
C     REIG SHOULD BE ON CYCLES
      IF(ID .EQ. 4) IZ(I) = IBUF(5)
      IZ(I+1) = 0
      I = I+2
      IF(I.EQ.NFO) GO TO 30
      GO TO 26
   30 CALL CLOSE(TOL,1)
      NLIST = I -IX
C
C     MATCH LIST OF  SELECTED VALUES WITH TIME LIST IN CORE
C
   35 CONTINUE
      IX = ICC + IFROUT
      IFSET = IZ(IX)
      IF ( IFSET  .LE. 0)  GO TO  70
      IX = ICC +ILSYM
      ISETNF = IX + IZ(IX)+1
   40 ISETF  = ISETNF +2
      NSETF  =IZ(ISETNF+1) + ISETF-1
      IF( IZ(ISETNF).EQ. IFSET) GO TO 80
      ISETNF = NSETF +1
      IF ( ISETNF .LT. IVEC) GO TO 40
      IFSET = -1
   70 DO  75 J = ILIST,NLIST,2
   75 IZ(J+1) = 1
      GO TO 200
   80 DO 100 I = ISETF,NSETF
      K = 0
      DIFF = 1.E25
      REAL = Z(I)
      DO 90  J = ILIST,NLIST,2
      IF (IZ(J+1) .NE. 0) GO TO 90
      DIFF1 =  ABS(Z(J) - REAL)
      IF( DIFF1 .GE. DIFF) GO TO 90
      DIFF = DIFF1
      K = J
   90 CONTINUE
      IF ( K .NE. 0)  IZ(K+1) = 1
  100 CONTINUE
C
C     SELECTED FREQUENCIES MARKED FOR OUTPUT
C
  200 NFO =(NLIST - ILIST +2)/2
C
C     MOVE NEW FREQ  TO UPPER
C
      K=1
      DO 300 I= ILIST,NLIST,2
      IF( IZ(I+1).EQ. 0) GO TO 300
      Z(K) = Z(I)
      K = K +1
  300 CONTINUE
      NFN = K-1
      DO 400  I = ILIST,NLIST,2
      IZ(K) = IZ(I+1)
      K = K+1
  400 CONTINUE
      IF(ID .EQ. 5) RETURN
      FILE =TOL1
      CALL OPEN(*800,TOL1,IZ(IBUF1),1)
      CALL FNAME(TOL1,IHD)
      CALL WRITE(TOL1,IHD,2,0)
      IF(ID .EQ. 2 .OR. ID .EQ. 4) GO TO 402
      CALL WRITE(TOL1,Z,NFN,1)
  401 CONTINUE
      CALL CLOSE(TOL1,1)
      MCB(1)= TOL1
      MCB(2)= NFN
      CALL WRTTRL(MCB )
      IF(ID .EQ. 2) GO TO 500
  800 RETURN
C
C     COPY OVER CLAMA STUFF
C
  402 CALL WRITE(TOL1,0,0,1)
      K = NFN + NFO + 1
      NZX  = IBUF3 -K
      FILE = TOL
      CALL GOPEN(TOL,IZ(IBUF2),0)
      CALL READ(*910,*920,TOL,IZ(K),146,1,FLAG)
      CALL WRITE(TOL1,IZ(K),146,1)
      M = NFN+1
      N = M+NFO -1
      DO 410 I = M,N
      CALL READ(*910,*920,TOL,IZ(K),LW,0,FLAG)
      IF(IZ(I) .EQ. 0) GO TO 410
      CALL WRITE(TOL1,IZ(K),LW,0)
  410 CONTINUE
      CALL CLOSE(TOL,1)
      CALL WRITE(TOL1,0,0,1)
      GO TO 401
C
C      COPY OVER CASECC
C
  500 CALL GOPEN(CASECC,IZ(IBUF1),0)
      CALL GOPEN(CASEZZ,IZ(IBUF2),1)
      M = NFN +1
      N = M+NFO-1
      DO 510 I = M,N
      CALL READ(*511,*520,CASECC,IZ(K),NZX,0,FLAG)
  520 IF(IZ(I) .EQ. 0) GO TO 510
      CALL WRITE(CASEZZ,IZ(K),FLAG,1)
  510 CONTINUE
  511 CALL CLOSE(CASECC,1)
      CALL CLOSE(CASEZZ,1)
      MCB(1) = CASECC
      CALL RDTRL(MCB)
      MCB(1) = CASEZZ
      CALL WRTTRL(MCB)
      RETURN
C
C     STATIC ANALYSIS
C
  600 CONTINUE
      R = 1.0
      NFO = NFO+ILIST
      NLIST = NFO-2
      DO 610 I = ILIST,NFO,2
      Z(I) = R
      IZ(I+1) = 0
      R = R+1.
  610 CONTINUE
C
C     COPY EDT
C
      CALL OPEN(*670,TOL,IZ(IBUF1),0)
      CALL OPEN(*670,TOL1,IZ(IBUF2),1)
      FILE = TOL
      CALL FNAME(TOL1,IHD)
      CALL WRITE(TOL1,IHD,2,0)
  620 CALL READ(*630,*920,TOL,IZ(NFO+2),NZ,0,FLAG)
      CALL WRITE(TOL1,IZ(NFO+2),FLAG,1)
      GO TO 620
  630 CALL CLOSE(TOL,1)
      CALL CLOSE(TOL1,1)
      MCB(1) = TOL
      CALL RDTRL(MCB)
      MCB(1) = TOL1
      CALL WRTTRL(MCB)
  670 GO TO 35
C
C     ERROR MESSAGES
C
  900 IP1=-1
  901 CALL MESAGE(IP1,FILE,NAME)
  910 IP1=-2
      GO TO 901
  920 IP1=-3
      GO TO 901
      END
