      SUBROUTINE TIMTS1
C
C     TIMTS1 TIME TESTS GINO AND THE PACK ROUTINES
C
      EXTERNAL        ANDF
      INTEGER         SYSBUF, OUTPUT, FILES(2), F1, F2, BUF1, BUF2,
     1                END, RD(4), WRT(4), BCK(4), MCB(7), EOL, EOR,
     2                BLD(16), INT(16), PAK(16), UNP(16), TYPE, P,
     3                TYPIN1, TYPOU1, TYPOU2, ISUBR(2), ANDF, OPT1,
     4                OPT2, NAME(4), MASK( 9), ABLK(15), BBLK(15),
     5                GET(16), PUT(16)
      REAL            X(1), Z(1)
      DOUBLE PRECISION ZD, XD
      CHARACTER       UFM*23, UWM*25, UIM*29, SFM*25
      COMMON /XMSSG / UFM, UWM, UIM, SFM
      COMMON /BLANK / N, M, TYPE, OPT1, OPT2
      COMMON /SYSTEM/ SYSBUF, OUTPUT
      COMMON /ZBLPKX/ ZD(2), IZ
      COMMON /ZNTPKX/ XD(2), IX, EOL, EOR
      COMMON /PACKX / TYPIN1, TYPOU1, I1, J1, INCR1
      COMMON /UNPAKX/ TYPOU2, I2, J2, INCR2
      COMMON /ZZZZZZ/ A(1)
      EQUIVALENCE     (ZD(1),Z(1)),(XD(1),X(1))
      DATA    FILES / 301, 302 / , RD   / 1H , 1H , 1H , 4HREAD /
      DATA    I1000 / 1000     / , I1001/ 1001 /  ,
     1        WRT   / 1H , 1H , 4H   W, 4HRITE /  ,
     2        BCK   / 4H   B, 4HACKW,   4HARD , 4HREAD /
      DATA    BLD   / 1H , 4HBLDP, 4HK( R, 4HSP ) ,
     1                1H , 4HBLDP, 4HK( R, 4HDP ) ,
     2                1H , 4HBLDP, 4HK( C, 4HSP ) ,
     3                1H , 4HBLDP, 4HK( C, 4HDP ) /
      DATA    INT   / 1H , 4HINTP, 4HK( R, 4HSP ) ,
     1                1H , 4HINTP, 4HK( R, 4HDP ) ,
     2                1H , 4HINTP, 4HK( C, 4HSP ) ,
     3                1H , 4HINTP, 4HK( C, 4HDP ) /
      DATA    PAK   / 1H , 4H PAC, 4HK( R, 4HSP ) ,
     1                1H , 4H PAC, 4HK( R, 4HDP ) ,
     2                1H , 4H PAC, 4HK( C, 4HSP ) ,
     3                1H , 4H PAC, 4HK( C, 4HDP ) /
      DATA    UNP   / 1H , 4HUNPA, 4HK( R, 4HSP ) ,
     1                1H , 4HUNPA, 4HK( R, 4HDP ) ,
     2                1H , 4HUNPA, 4HK( C, 4HSP ) ,
     3                1H , 4HUNPA, 4HK( C, 4HDP ) /
      DATA    PUT   / 4H   P, 4HUTST, 4HR( R, 4HSP ) ,
     1                4H   P, 4HUTST, 4HR( R, 4HDP ) ,
     2                4H   P, 4HUTST, 4HR( C, 4HSP ) ,
     3                4H   P, 4HUTST, 4HR( C, 4HDP ) /
      DATA    GET   / 4H   G, 4HETST, 4HR( R, 4HSP ) ,
     1                4H   G, 4HETST, 4HR( R, 4HDP ) ,
     2                4H   G, 4HETST, 4HR( C, 4HSP ) ,
     3                4H   G, 4HETST, 4HR( C, 4HDP ) /
      DATA    NMASK / 9 /
      DATA    ISUBR / 4HTIMT, 4HS1  /
C
C     INITIALIZE
C
      CALL PAGE1
      F1   = FILES(1)
      F2   = FILES(2)
      BUF1 = KORSZ(A) - SYSBUF
      BUF2 = BUF1 - SYSBUF
      END  = N*M
      IF (END .GE. BUF1-1) CALL MESAGE (-8,0,ISUBR)
      DO 12 I = 1,END
      A(I) = I
   12 CONTINUE
      N10  = N*10
      M10  = M/10
      IF (M10 .LE. 0) M10 = 1
      FN = N
      FM = M
      P  = 4*(TYPE-1) + 1
      MASK(1) = 1
      DO 14 I = 2,NMASK
   14 MASK(I) = 2*MASK(I-1)
      WRITE  (OUTPUT,11) N, M, TYPE, OPT1, OPT2
   11 FORMAT (1H  , 20X, 25HNASTRAN TIME TEST C   N =, I4, 5H, M =, I4 ,
     1         8H, TYPE =,I4, 8H, OPT1 =,I4, 8H, OPT2 =,I4)
C
C     WRITE TEST
C
      IF (ANDF(OPT2,MASK(1)) .EQ. 0) GO TO 50
      CALL OPEN (*901,F1,A(BUF1),1)
      CALL CPUTIM (T1,T1,1)
      DO 21 I = 1,N
      CALL WRITE (F1,A,M,1)
   21 CONTINUE
      CALL CPUTIM (T2,T1,1)
      CALL CLOSE  (F1,1)
      CALL OPEN (*901,F2,A(BUF2),1)
      CALL CPUTIM (T3,T3,1)
      DO 22 I = 1,N10
      CALL WRITE (F2,A,M10,1)
   22 CONTINUE
      CALL CPUTIM (T4,T4,1)
      CALL CLOSE  (F2,1)
      ASSIGN 30 TO IRET
      NAME(1) = WRT(1)
      NAME(2) = WRT(2)
      NAME(3) = WRT(3)
      NAME(4) = WRT(4)
      GO TO 100
C
C     READ TEST
C
   30 CONTINUE
      IF (ANDF(OPT2,MASK(2)) .EQ. 0) GO TO 40
      CALL OPEN (*901,F1,A(BUF1),0)
      CALL CPUTIM (T1,T1,1)
      DO 31 I = 1,N
      CALL READ (*902,*903,F1,A(I1000),M,1,FLAG)
   31 CONTINUE
      CALL CPUTIM (T2,T2,1)
      CALL CLOSE  (F1,2)
      CALL OPEN (*901,F2,A(BUF2),0)
      CALL CPUTIM (T3,T3,1)
      DO 32 I = 1,N10
      CALL READ (*902,*903,F2,A(I1000),M10,1,FLAG)
   32 CONTINUE
      CALL CPUTIM (T4,T4,1)
      CALL CLOSE  (F2,2)
      ASSIGN 40 TO IRET
      NAME(1) = RD(1)
      NAME(2) = RD(2)
      NAME(3) = RD(3)
      NAME(4) = RD(4)
      GO TO 100
C
C     BACKWARD READ TEST
C
   40 CONTINUE
      IF (ANDF(OPT2,MASK(3)) .EQ. 0) GO TO 50
      CALL OPEN (*901,F1,A(BUF1),2)
      CALL CPUTIM (T1,T1,1)
      DO 41 I = 1,N
      CALL BCKREC (F1)
      CALL READ (*902,*903,F1,A(I1000),M,1,FLAG)
      CALL BCKREC (F1)
   41 CONTINUE
      CALL CPUTIM (T2,T2,1)
      CALL CLOSE  (F1,1)
      CALL OPEN (*901,F2,A(BUF2),2)
      CALL CPUTIM (T3,T3,1)
      DO 42 I = 1,N10
      CALL BCKREC (F2)
      CALL READ (*902,*903,F2,A(I1000),M10,1,FLAG)
      CALL BCKREC (F2)
   42 CONTINUE
      CALL CPUTIM (T4,T4,1)
      CALL CLOSE  (F2,1)
      ASSIGN 50 TO IRET
      NAME(1) = BCK(1)
      NAME(2) = BCK(2)
      NAME(3) = BCK(3)
      NAME(4) = BCK(4)
      GO TO 100
C
C     BLDPK TEST
C
   50 CONTINUE
      IF (ANDF(OPT2,MASK(4)) .EQ. 0) GO TO 70
      CALL OPEN (*901,F1,A(BUF1),1)
      CALL MAKMCB (MCB,F1,M,2,TYPE)
      CALL CPUTIM (T1,T1,1)
      DO 51 I = 1,N
      CALL BLDPK (TYPE,TYPE,F1,0,0)
      DO 52 J = 1,M
      Z(1) = 1.0
      IZ   = J
      CALL ZBLPKI
   52 CONTINUE
      CALL BLDPKN (F1,0,MCB)
   51 CONTINUE
      CALL CPUTIM (T2,T2,1)
      CALL WRTTRL (MCB)
      CALL CLOSE  (F1,1)
      CALL MAKMCB (MCB,F2,M10,2,TYPE)
      CALL OPEN (*901,F2,A(BUF2),1)
      CALL CPUTIM (T3,T3,1)
      DO 54 I = 1,N10
      CALL BLDPK (TYPE,TYPE,F2,0,0)
      DO 55 J = 1,M10
      Z(1) = 2.0
      IZ   = J
      CALL ZBLPKI
   55 CONTINUE
      CALL BLDPKN (F2,0,MCB)
   54 CONTINUE
      CALL CPUTIM (T4,T4,1)
      CALL WRTTRL (MCB)
      CALL CLOSE  (F2,1)
      ASSIGN 60 TO IRET
      NAME(1) = BLD(P)
      NAME(2) = BLD(P+1)
      NAME(3) = BLD(P+2)
      NAME(4) = BLD(P+3)
      GO TO 100
C
C     INTPK TEST
C
   60 CONTINUE
      IF (ANDF(OPT2,MASK(5)) .EQ. 0) GO TO 70
      CALL OPEN (*901,F1,A(BUF1),0)
      CALL CPUTIM (T1,T1,1)
      DO 61 I = 1,N
      CALL INTPK (*902,F1,0,TYPE,0)
      DO 62 J = 1,M
      CALL ZNTPKI
      IF (IX  .NE. J) GO TO 110
      IF (EOL .EQ. 0) GO TO 62
      IF (IX  .NE. M) GO TO 110
   62 CONTINUE
      IF (EOL .EQ. 0) GO TO 110
   61 CONTINUE
      CALL CPUTIM (T2,T2,1)
      CALL CLOSE  (F1,1)
      CALL OPEN (*901,F2,A(BUF2),0)
      CALL CPUTIM (T3,T3,1)
      DO 63 I = 1,N10
      CALL INTPK (*902,F2,0,TYPE,0)
      DO 64 J = 1,M10
      CALL ZNTPKI
      IF (IX  .NE.   J) GO TO 110
      IF (EOL .EQ.   0) GO TO 64
      IF (IX  .NE. M10) GO TO 110
   64 CONTINUE
      IF (EOL .EQ. 0) GO TO 110
   63 CONTINUE
      CALL CPUTIM (T4,T4,1)
      CALL CLOSE  (F2,1)
      ASSIGN 70 TO IRET
      NAME(1) = INT(P)
      NAME(2) = INT(P+1)
      NAME(3) = INT(P+2)
      NAME(4) = INT(P+3)
      GO TO 100
C
C     PACK TEST
C
   70 CONTINUE
      IF (ANDF(OPT2,MASK(6)) .EQ. 0) GO TO 90
      CALL MAKMCB (MCB,F1,M,2,TYPE)
      TYPIN1 = TYPE
      TYPOU1 = TYPE
      I1 = 1
      J1 = M
      INCR1 = 1
      MX = M*TYPE
      DO 72 I = 1,MX
      A(I+1000) = I
   72 CONTINUE
      CALL OPEN (*901,F1,A(BUF1),1)
      CALL CPUTIM (T1,T1,1)
      DO 73 I = 1,N
      CALL PACK (A(I1001),F1,MCB)
   73 CONTINUE
      CALL CPUTIM (T2,T2,1)
      CALL WRTTRL (MCB)
      CALL CLOSE  (F1,1)
      CALL MAKMCB (MCB,F2,M10,2,TYPE)
      J1 = M10
      CALL OPEN (*901,F2,A(BUF2),1)
      CALL CPUTIM (T3,T3,1)
      DO 75 I = 1,N10
      CALL PACK (A(I1001),F2,MCB)
   75 CONTINUE
      CALL CPUTIM (T4,T4,1)
      CALL WRTTRL (MCB)
      CALL CLOSE  (F2,1)
      ASSIGN 80 TO IRET
      NAME(1) = PAK(P)
      NAME(2) = PAK(P+1)
      NAME(3) = PAK(P+2)
      NAME(4) = PAK(P+3)
      GO TO 100
C
C     UNPACK TEST
C
   80 CONTINUE
      IF (ANDF(OPT2,MASK(7)) .EQ. 0) GO TO 90
      TYPOU2 = TYPE
      I2 = 1
      J2 = M
      INCR2 = 1
      CALL OPEN (*901,F1,A(BUF1),0)
      CALL CPUTIM (T1,T1,1)
      DO 81 I = 1,N
      CALL UNPACK (*902,F1,A(I1001))
   81 CONTINUE
      CALL CPUTIM (T2,T2,1)
      CALL CLOSE  (F1,1)
      J2 = M10
      CALL OPEN (*901,F2,A(BUF2),0)
      CALL CPUTIM (T3,T3,1)
      DO 82 I = 1,N10
      CALL UNPACK (*902,F2,A(I1001))
   82 CONTINUE
      CALL CPUTIM (T4,T4,1)
      CALL CLOSE  (F2,2)
      ASSIGN 90 TO IRET
      NAME(1) = UNP(P)
      NAME(2) = UNP(P+1)
      NAME(3) = UNP(P+2)
      NAME(4) = UNP(P+3)
      GO TO 100
   90 CONTINUE
C
C     PUTSTR TEST
C
C
      IF (ANDF(OPT2,MASK(8)) .EQ. 0) GO TO 220
      KERR = 1
      ABLK(1) = F1
      ABLK(2) = TYPE
      ABLK(3) = 1
      CALL GOPEN (F1,A(BUF1),1)
      NWDS = TYPE
      IF (TYPE .EQ. 3) NWDS = 2
      CALL CPUTIM (T1,T1,1)
      DO 95 I = 1,N
      ABLK(4) = 0
      ABLK(8) = -1
      DO 94 J = 1,10
      NBRSTR  = M10
   91 CALL PUTSTR (ABLK)
      IF( NBRSTR .EQ. 0) GO TO 910
      ABLK(7) = MIN0(ABLK(6),NBRSTR)
      ABLK(4) = ABLK(4) + ABLK(7) + 4
      MM = ABLK(7)*NWDS
      DO 92 K = 1,MM
      X(1) = A(K)
   92 CONTINUE
      IF (ABLK(7) .EQ. NBRSTR) GO TO 93
      CALL ENDPUT (ABLK)
      NBRSTR = NBRSTR - ABLK(7)
      GO TO 91
   93 IF (J .EQ. 10) ABLK(8) = 1
      CALL ENDPUT (ABLK)
   94 CONTINUE
   95 CONTINUE
      CALL CPUTIM (T2,T2,1)
      CALL CLOSE  (F1,1)
      M100 = MAX0(M10/10,1)
      CALL GOPEN (F2,A(BUF2),1)
      KERR = 2
      BBLK(1) = F2
      BBLK(2) = TYPE
      BBLK(3) = 1
      CALL CPUTIM (T3,T3,1)
      DO 209 I = 1,N10
      BBLK(4) = 0
      BBLK(8) = -1
      DO 208 J = 1,10
      NBRSTR = M100
  202 CALL PUTSTR (BBLK)
      IF (NBRSTR .EQ. 0) GO TO 910
      BBLK(7) = MIN0(BBLK(6),NBRSTR)
      BBLK(4) = BBLK(4) + BBLK(7) + 4
      MM = BBLK(7)*NWDS
      DO 203 K = 1,MM
      X(1) = A(K)
  203 CONTINUE
      IF (BBLK(7) .EQ. NBRSTR) GO TO 206
      NBRSTR = NBRSTR - BBLK(7)
      GO TO 202
  206 IF (J .EQ. 10) BBLK(8) = 1
      CALL ENDPUT (BBLK)
  208 CONTINUE
  209 CONTINUE
      CALL CPUTIM (T4,T4,1)
      CALL CLOSE  (F2,1)
      ASSIGN 210 TO IRET
      NAME(1) = PUT(P)
      NAME(2) = PUT(P+1)
      NAME(3) = PUT(P+2)
      NAME(4) = PUT(P+3)
      GO TO 100
C
C     GETSTR TEST
C
  210 IF (ANDF(OPT2,MASK(9)) .EQ. 0) GO TO 220
      CALL GOPEN (F1,A(BUF1),0)
      CALL CPUTIM (T1,T1,1)
      DO 214 I = 1,N
      ABLK(8) = -1
  211 CALL GETSTR (*214,ABLK)
      MM = ABLK(6)*NWDS
      DO 212 K = 1,MM
      X(1) = A(K)
  212 CONTINUE
      CALL ENDGET (ABLK)
      GO TO 211
  214 CONTINUE
      CALL CPUTIM (T2,T2,1)
      CALL CLOSE  (F1,1)
      CALL GOPEN  (F2,A(BUF2),0)
      CALL CPUTIM (T3,T3,1)
      DO 219 I = 1,N10
      BBLK(8) = -1
  215 CALL GETSTR (*219,BBLK)
      MM = BBLK(6)*NWDS
      DO 216 K = 1,MM
      X(1) = A(K)
  216 CONTINUE
      CALL ENDGET (BBLK)
      GO TO 215
  219 CONTINUE
      CALL CPUTIM (T4,T4,1)
      CALL CLOSE  (F2,1)
      ASSIGN 220 TO IRET
      NAME(1) = GET(P)
      NAME(2) = GET(P+1)
      NAME(3) = GET(P+2)
      NAME(4) = GET(P+3)
      GO TO 100
C
  220 CONTINUE
      RETURN
C
C
C     INTERNAL ROUTINE TO WRITE OUTPUT ONTO THE OUTPUT FILE
C
  100 CONTINUE
      TIME1 = T2 - T1
      TIME2 = T4 - T3
      TPRREC = 1.0E6*(TIME2 - TIME1)/(9.0*FN)
      TPRWRD = (1.0E6*TIME1 - FN*TPRREC)/(FN*FM)
C
      WRITE  (OUTPUT,111) NAME, TIME1, NAME, TIME2, TPRWRD, TPRREC
  111 FORMAT (1H0, 4A4,
     1        '   N     M-WORD RECORDS -- TIME1 = ', E12.5, ' SECONDS'/
     2        1X , 4A4,
     3        ' 10*N M/10-WORD RECORDS -- TIME2 = ', E12.5, ' SECONDS'/
     4        1H0,'IF THE MODEL IS TIME = (N*M)TPRWRD + N*TPRREC, THEN'/
     5        1H0, 16X,
     6        '     -- TIME PER WORD   (TPRWRD) = ', E12.5, ' MICRO',
     7        'SECONDS  --  DATA FOR USE IN COMMON /NTIME/'/
     8        1X , 16X,
     9        '     -- TIME PER RECORD (TPRREC) = ', E12.5, ' MICRO',
     O        'SECONDS')
C
      GO TO IRET, (30,40,50,60,70,80,90,210,220)
C
C     INTERNAL ROUTINE CALLED FOR AN ABORT IN THE INTPK TEST
C
  110 CONTINUE
      WRITE  (OUTPUT,121) SFM,INT(P),INT(P+1),INT(P+2),INT(P+3)
  121 FORMAT (A25,' 2197, ABORT CALLED DURING TIME TEST OF ',4A4)
C
C     ABNORMAL RETURNS FROM GINO--ALL FATAL ERRORS
C
  901 CONTINUE
  902 CONTINUE
  903 CALL MESAGE (-61,0,0)
  910 WRITE  (OUTPUT,911) KERR
  911 FORMAT (23H0*** TIMTS1 FATAL ERROR,I4 )
      GO TO 903
      END
