      SUBROUTINE SMPYAD
C
      INTEGER         SRESLT   ,SADD     ,TMAT     ,TRLRA    ,TRLRB  ,
     1                TRLRC    ,TRLRD    ,TRNSP    ,SIGNAB   ,SIGNC  ,
     1                SCRTCH   ,TRLR(7,5),MAT(5)   ,ADDMAT   ,RESMAT ,
     1                RECMAT   ,DOSI(3)  ,REFUS(3) ,OUTPT    ,NAME(2)
      CHARACTER       UFM*23   ,UWM*25   ,UIM*29   ,SFM*25   ,SWM*27
      COMMON /XMSSG / UFM      ,UWM      ,UIM      ,SFM      ,SWM
      COMMON /MPYADX/ TRLRA(7) ,TRLRB(7) ,TRLRC(7) ,TRLRD(7) ,NA     ,
     1                TRNSP    ,SIGNAB   ,SIGNC    ,IPREC1   ,SCRTCH
     2       /ZZZZZZ/ A(1)
     3       /BLANK / N        ,SRESLT   ,SADD     ,IPREC    ,TMAT(4)
     4       /SYSTEM/ KSYSTM(65)
      EQUIVALENCE     (KSYSTM(55),KPREC) ,(KSYSTM(2),OUTPT)
      DATA    MAT   / 101      ,102      ,103      ,104      ,105 /
      DATA    ADDMAT, RESMAT   ,INTRES   ,MPYADS   ,RECMAT        /
     1        106   , 201      ,301      ,302      ,2             /
      DATA    DOSI  / 4HSING   ,4HDOUB   ,4HMLTP   /
      DATA    REFUS / 2*3H     ,3HREF    /
      DATA    NAME  / 4HSMPY   ,4HAD     /
     1
C
      IF (N .LE. 1) GO TO 200
      IF (N .GT. 5) N = 5
      IPREC1 = 1
      ITYPE  = 0
C
C     IF ONE OF THE -N- MATRICES IN THE PRODUCT DOES NOT EXIST,
C     SKIP THE ENTIRE CALCULATION.
C
      DO 101 I = 1,N
      TRLR(1,I) = MAT(I)
      CALL RDTRL (TRLR(1,I))
      IF (TRLR(1,I).LE.0 .OR. TRLR(2,I).LE.0 .OR. TRLR(3,I).LE.0)
     1    GO TO 200
      IF (TRLR(5,I).EQ.2 .OR. TRLR(5,I).EQ.4) IPREC1 = 2
      IF (TRLR(5,I).EQ.3 .OR. TRLR(5,I).EQ.4) ITYPE  = 2
  101 CONTINUE
C
C     CHECK TO SEE IF THE INPUT MATRICES ARE CONFORMABLE
C
      NM1  = N - 1
      NOGO = 0
      DO 170 I = 1,NM1
      ICOL = TRLR(2,I)
      IF (TMAT(I) .NE. 0) ICOL = TRLR(3,I)
      IROW = TRLR(3,I+1)
      IF (I .EQ. NM1) GO TO 160
      IF (TMAT(I+1) .NE. 0) IROW = TRLR(2,I+1)
  160 IF (ICOL .NE. IROW) NOGO = 1
  170 CONTINUE
      TRLRC(1) = ADDMAT
      CALL RDTRL (TRLRC)
      IF (TRLRC(1) .LE. 0) GO TO 180
      IROW = TRLR(3,1)
      IF (TMAT(1) .NE. 0) IROW = TRLR(2,1)
      ICOL = TRLR(2,N)
      IF (IROW.NE.TRLRC(3) .OR. ICOL.NE.TRLRC(2)) NOGO = 1
  180 IF (NOGO .EQ. 1) CALL MESAGE (-55,0,NAME)
C
      IF (IPREC1.LT.1 .OR. IPREC1.GT.2) IPREC1 = KPREC
      IF (IPREC.EQ.IPREC1 .OR. IPREC.EQ.0) GO TO 222
      IF (IPREC.LT.1 .OR. IPREC.GT.2) IPREC = 3
      WRITE  (OUTPT,221) SWM,DOSI(IPREC),REFUS(IPREC),NAME,DOSI(IPREC1)
  221 FORMAT (A27,' 2163, REQUESTED ',A4,'LE PRECISION ',A3,'USED BY ',
     1        2A4,2H. ,A4,'LE PRECISION IS LOGICAL CHOICE')
      IF (IPREC .NE. 3) IPREC1 = IPREC
  222 IPREC = IPREC1
      ITYPE = ITYPE + IPREC1
C
C     SETUP THE MPYADX COMMON BLOCK.
C
      IF ((N+1)/2 .EQ. N/2) GO TO 105
      TRLRB(1) = INTRES
      M = RESMAT
      GO TO 106
  105 TRLRB(1) = RESMAT
      M = INTRES
  106 TRLRC(1) = 0
      DO 107 I = 1,7
      TRLRD(I) = TRLR(I,N)
  107 CONTINUE
      TRLRD(4) = RECMAT
      NA = KORSZ(A)
      SIGNAB = 1
      SIGNC  = SADD
      SCRTCH = MPYADS
C
C     DO THE N-1 MULTIPLICATIONS.
C
      DO 125 K = 2,N
      J = N - K + 1
      TRLRA(1) = TRLR(1,J)
      IF (K .NE. 3) L = TRLRB(1)
      IF (K .EQ. 3) L = M
      TRLRB(1) = TRLRD(1)
      TRLRD(1) = L
      DO 110 I = 2,7
      TRLRA(I) = TRLR(I,J)
      TRLRB(I) = TRLRD(I)
  110 CONTINUE
      IF (K .NE. N) GO TO 111
      TRLRC(1) = ADDMAT
      CALL RDTRL (TRLRC)
      IF (TRLRC(1). LT. 0) TRLRC(1) = 0
      TRLRD(5) = ITYPE
      SIGNAB   = SRESLT
      GO TO 115
  111 TRLRD(5) = IPREC1
      IF (TRLRA(5).GT.2 .OR. TRLRB(5).GT.2) TRLRD(5) = IPREC1 + 2
  115 TRNSP = TMAT(J)
      TRLRD(3) = TRLRA(3)
      IF (TRNSP .NE. 0) TRLRD(3) = TRLRA(2)
      TRLRD(2) = TRLRB(2)
      CALL MPYAD (A,A,A)
  125 CONTINUE
      IF (TRLRD(2) .EQ. TRLRD(3)) TRLRD(4) = 1
      CALL WRTTRL (TRLRD)
  200 RETURN
      END
