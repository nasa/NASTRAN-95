      SUBROUTINE GIPSST
C
C     THIS SUBROUTINE LOCATES ALL THE G AND K SET POINTS IN THE SPLINE
C     COORDINATE SYSTEM AND FORMS G FOR EACH SET THEN
C     INSERTS THE G INTO THE FULL SIZED G MATRIX
C
      LOGICAL         OXR,OYR,ZAP,KCOL
      INTEGER         SYSBUF,OUT,BUFF,BUFF1,TRL(7),TGKG(7),OLDID,IZ(28),
     1                PCSTM,PBGPT,NWR,TYPE,NS(2),PROE,PTE,ISNG,SLOPE,
     2                PG,PK,PROL,PTL,BUFF2,CTYPE,PSIL
      INTEGER         SCARD(10),CCARD(16)
      INTEGER         SPLINE,USETA,CSTM,BAGPDT,SILA,EQAERO,GM,GO,SCR1,
     1                SCR2,SCR3,SCR4,SCR5,KSIZE,GSIZE,GTKA
      DIMENSION       TL(9),ROL(3),AN(6),BLOCK(20),TGS(18),T(3),TG(9)
      DIMENSION       TT(9),Z(1),SRARD(10)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /ZNTPKX/ A,DUM(3),NR,IEOL,IEOR
      COMMON /SYSTEM/ SYSBUF,OUT
      COMMON /GICOM / SPLINE,USETA,CSTM,BAGPDT,SILA,EQAERO,GM,GO,GTKA,
     1                KSIZE,GSIZE,SCR1,SCR2,SCR3,SCR4,SCR5
      COMMON /UNPAKX/ ITC,II,J1,INCR
      COMMON /ZZZZZZ/ IZX(1)
C
C     CHANGE IN EQUIV FOR SIZE OF SCARD OR CCARD
C     NEED TO CHANGE PENDC
C
      EQUIVALENCE     (IZX(1),IZ(1),Z(1),SCARD(1),SRARD(1)),
     1                (IZ(11),CCARD(1)) ,(IZ(27),NGSET) ,(IZ(28),NKSET)
      DATA     NS   / 4HGIPS,4HST  /
      DATA     TGS  / 18*0.0       /
C
      PENDC = 28
      NOGO  = 0
      OLDID =-1
      LC    =-1
      NWDS  = KORSZ(IZ)
      BUFF  = NWDS - SYSBUF
      BUFF1 = BUFF - SYSBUF
      BUFF2 = BUFF1- SYSBUF
      TRL(1)= CSTM
      CALL RDTRL (TRL)
      IF (TRL(1) .LT. 0) TRL(3) = 0
      NCSTM = (TRL(3)+1)*14
      PCSTM = BUFF2 - NCSTM
      TRL(1)= BAGPDT
      CALL RDTRL (TRL)
      NBG   = TRL(2)*4
      PBGPT = PCSTM - NBG
      TRL(1)= SCR1
      CALL RDTRL (TRL)
      MAX   = TRL(2)
      IF (TRL(3) .EQ. 0) GO TO 1000
      I     = SCR2
      SCR2  = SCR3
      SCR3  = I
      IPASS = 0
C
C     INITIAL CORE CHECK
C
      IF (PBGPT-2*MAX .LT. 0) GO TO 993
C
C     OPEN SCR1 TO LOOP ON G AND K SET RECORDS
C
      CALL GOPEN (SCR1,IZ(BUFF+1),0)
C
C     READ IN CSTM AT PCSTM + 14 ADD BASIC COORD SYSTEM
C
    1 IZ(PCSTM  ) = 0
      IZ(PCSTM+1) = 1
      DO 5 I = 2,13
      Z(PCSTM+I) = 0.0
    5 CONTINUE
      Z(PCSTM+5) = 1.0
      Z(PCSTM+9) = 1.0
      Z(PCSTM+13)= 1.0
      IF (NCSTM .EQ. 14) GO TO 7
      IFIL = CSTM
      CALL GOPEN (CSTM,IZ(BUFF1+1),0)
      CALL READ  (*999,*999,CSTM,IZ(PCSTM+14),NCSTM-14,1,NWR)
      CALL CLOSE (CSTM,1)
    7 CALL PRETRS (IZ(PCSTM),NCSTM)
C
C     READ IN BAGPDT AT PBGPT
C
      IFIL = BAGPDT
      CALL GOPEN (BAGPDT,IZ(BUFF1+1),0)
      CALL READ  (*999,*999,BAGPDT,IZ(PBGPT),NBG,1,NWR)
      CALL CLOSE (BAGPDT,1)
C
C     READ SCR1 AND PROCESS A SPLINE DEPENDING ON TYPE
C
   10 N1  = MAX + 1
      IFIL= SCR1
      CALL READ (*500,*20,SCR1,IZ(1),N1,1,NWR)
   20 J   = 2
      TYPE= IZ(J)
      PG  = PENDC
      PK  = PG + NGSET
      PSIL= PK + NKSET
      IPK = PSIL + NGSET
      NP  = NGSET + NKSET
C
C     USE A K POINT TO PICK UP POINTER TO BAGPDT FOR
C     COORDINATE SYSTEM ID OF SPLINE
C
      NEWID = CCARD(2)
      CTYPE = CCARD(8)
      K = PCSTM
      J = PCSTM + NCSTM - 1
      IF (NEWID .EQ. OLDID) GO TO 40
      DO 30 I = K,J,14
      IF (IZ(I) .NE. NEWID) GO TO 30
      PROE = I + 2
      PTE  = I + 5
      OLDID = NEWID
      GO TO 40
   30 CONTINUE
      IC = NEWID
      GO TO 997
   40 GO TO (50,100), TYPE
C
C     SURFACE SPLINE
C
   50 GO TO (51,998,51,51,51), CTYPE
   51 CONTINUE
      IS  = 1
      PTL = PTE
      DO 54 I = 1,9
   54 TL(I) = Z(PTE+I-1)
      DO 60 I = 1,NP
      K = (IZ(PG+I)-1)*4
C
C     BASIC COORDINATES
C
      BX =  Z(PBGPT+K+1)
      BY =  Z(PBGPT+K+2)
      BZ =  Z(PBGPT+K+3)
      IF (NEWID .EQ. 0) GO TO 55
C
C     X AND Y OF SPLINE
C
      T1 = BX -  Z(PROE  )
      T2 = BY -  Z(PROE+1)
      T3 = BZ -  Z(PROE+2)
      Z(N1  ) = Z(PTE)*T1   + Z(PTE+3)*T2 + Z(PTE+6)*T3
      Z(N1+1) = Z(PTE+1)*T1 + Z(PTE+4)*T2 + Z(PTE+7)*T3
      GO TO 59
   55 Z(N1  ) = BX
      Z(N1+1) = BY
   59 N1 = N1 + 2
   60 CONTINUE
      K = MAX + 1
      J = K + 2*NGSET
      NCORE = PBGPT - N1
C
C     CORE CHECK
C
      N  = NGSET + 3
      ND = NKSET*2
      NN = N*N + 3*N + N*ND + ND*NGSET
      IF (NN .LT. NCORE) GO TO 70
      NCORE = BUFF2 - N1
      IF (NN .GT. NCORE) GO TO 992
      ZAP =.TRUE.
C
C     GET G FOR A SURFACE SPLINE
C
   70 CALL SSPLIN (NGSET,IZ(K),NKSET,IZ(J),0,0,1,1,SCARD(6),IZ(N1),
     1             NCORE,ISNG)
      IF (ISNG .EQ. 2) GO TO 998
      IF (NOGO .EQ. 1) GO TO 10
C
C     REVERSE SIGN OF SLOPE COLUMN
C
      K = N1
      DO 80 I = 1,NKSET
      K = K + NGSET
      DO 90 J = 1,NGSET
      Z(K) = -Z(K)
      K = K + 1
   90 CONTINUE
   80 CONTINUE
      GO TO 300
C
C     LINEAR SPLINE
C
  100 GO TO (101,130,101,101,101), CTYPE
C
C     CAERO2 PROSESSING   BODIES
C
  130 SCARD( 8) = NEWID
      SCARD( 9) = SCARD(10)
      SCARD(10) = -1.0
      IBTYP = CCARD(16)
      KD    = 1
      DO 135 I = 2,8
  135 TL(I) = 0.0
      TL(1) = 1.0
      TL(5) = 1.0
      TL(9) = 1.0
      GO TO 102
  101 KD = 2
  102 CONTINUE
C
C     FIND CORD SYSTEM OF LINEAR SPLINE
C
      IF (SCARD(8) .EQ. LC) GO TO 120
      DO 110 I = K,J,14
      IF (SCARD(8) .NE. IZ(I)) GO TO 110
      LC   = SCARD(8)
      PROL = I + 2
      PTL  = I + 5
      GO TO 120
  110 CONTINUE
      IC = SCARD(8)
      GO TO 997
  120 IF (NEWID.EQ.0 .AND. SCARD(8).EQ.0) GO TO 145
      T1 = Z(PROL  ) - Z(PROE)
      T2 = Z(PROL+1) - Z(PROE+1)
      T3 = Z(PROL+2) - Z(PROE+2)
      T1 = Z(PTE+2)*T1 + Z(PTE+5)*T2 + Z(PTE+8)*T3
      T2 = Z(PTE+5)*T1
      T3 = Z(PTE+8)*T1
      T1 = Z(PTE+2)*T1
      ROL(1) = Z(PROL  ) - T1
      ROL(2) = Z(PROL+1) - T2
      ROL(3) = Z(PROL+2) - T3
      T1 = Z(PTL+4)*Z(PTE+8) - Z(PTL+7)*Z(PTE+5)
      T2 = Z(PTL+7)*Z(PTE+2) - Z(PTL+1)*Z(PTE+8)
      T3 = Z(PTL+1)*Z(PTE+5) - Z(PTL+4)*Z(PTE+2)
      T4 = SQRT(T1*T1 + T2*T2 + T3*T3)
      IF (T4 .EQ. 0.0) GO TO 996
      TL(1) = T1/T4
      TL(4) = T2/T4
      TL(7) = T3/T4
      TL(2) = Z(PTE+5)*TL(7) - Z(PTE+8)*TL(4)
      TL(5) = Z(PTE+8)*TL(1) - Z(PTE+2)*TL(7)
      TL(8) = Z(PTE+2)*TL(4) - Z(PTE+5)*TL(1)
      TL(3) = Z(PTE+2)
      TL(6) = Z(PTE+5)
      TL(9) = Z(PTE+8)
  145 DO 160 I = 1,NP
C
C     BASIC CORD
C
      K  = (IZ(PG+I)-1)*4
      BX =  Z(PBGPT+K+1)
      BY =  Z(PBGPT+K+2)
      BZ =  Z(PBGPT+K+3)
      IF (NEWID.EQ.0 .AND. SCARD(8).EQ.0) GO TO 150
      T1 = BX - ROL(1)
      T2 = BY - ROL(2)
      T3 = BZ - ROL(3)
      Z(N1  ) = TL(1)*T1 + TL(4)*T2 + TL(7)*T3
      Z(N1+1) = TL(2)*T1 + TL(5)*T2 + TL(8)*T3
      GO TO 155
  150 Z(N1  ) = BX
      Z(N1+1) = BY
  155 N1 = N1 + 2
  160 CONTINUE
      IF (CTYPE .NE. 2) GO TO 169
      N1 = MAX + 1
      DO 165 I = 1,NP
      Z(N1+1) = Z(N1)
      Z(N1  ) = 0.0
  165 N1 = N1 + 2
C
C     CHECK CORE
C
  169 K = MAX + 1
      J = K + 2*NGSET
      NCORE = PBGPT - N1
      OYR = .FALSE.
      OXR = .FALSE.
      IF (SRARD( 9) .LT. 0.0) OXR = .TRUE.
      IF (SRARD(10) .LT. 0.0) OYR = .TRUE.
      IS = 3
      IF (OXR) IS = IS - 1
      IF (OYR) IS = IS - 1
      N  = IS*NGSET + 3
      ND = NKSET*(1+KD)
      NN = N*N + 3*N + N*ND + ND*NGSET*IS
      IF (NN .LT. NCORE) GO TO 170
      NCORE = BUFF2 - N1
      IF (NN .GT. NCORE) GO TO 992
      ZAP =.TRUE.
C
C     GET G FOR A LINEAR SPLINE
C
  170 CALL LSPLIN (NGSET,IZ(K),NKSET,IZ(J),0,KD,1,SCARD(6),SCARD(9),
     1             SCARD(10),SCARD(7),IZ(N1),NCORE,ISNG)
      IF (ISNG .EQ. 2) GO TO 998
      IF (NOGO .EQ. 1) GO TO 10
      IF (CTYPE.EQ. 2) GO TO 300
C
C     TRANSFORM G TO SPLINE COORDINATES
C
      TYL = 1.0
      TXL = 0.0
      IF (NEWID.EQ.0 .AND. SCARD(8).EQ.0) GO TO 190
      TYL = Z(PTE+1)*TL(2) + Z(PTE+4)*TL(5) + Z(PTE+7)*TL(8)
      TXL = Z(PTE+1)*TL(1) + Z(PTE+4)*TL(4) + Z(PTE+7)*TL(7)
C
C     MOVE COLUMNS UP
C
  190 NRGS = NGSET*IS
      K2   = NRGS + NRGS
      K3   = K2 + NRGS
      NCORE= N1
      N1   = N1 + NRGS - 1
      N2   = N1
      DO 200 I = 1,NKSET
      DO 210 K = 1,NRGS
      Z(N2+K) = Z(N1+K)*TXL + Z(N1+NRGS+K)*TYL
      Z(N2+NRGS+K) = Z(N1+K2+K)
  210 CONTINUE
      N1 = N1 + K3
      N2 = N2 + K2
  200 CONTINUE
      N1 = NCORE
C
C     TRANSFORM G INTO GLOBAL
C
  300 CONTINUE
C
C                         T
C     OPEN SCR2 TO WRITE G   MATRIX
C                         KG
C
      CALL GOPEN (SCR2,IZ(BUFF1+1),1)
      CALL GOPEN (SCR3,IZ(BUFF2+1),0)
      TGKG(3) = GSIZE
      TGKG(4) = 2
      TGKG(5) = 1
      TGKG(1) = SCR2
      TGKG(2) = 0
      TGKG(6) = 0
      TGKG(7) = 0
      IBCC    = 1
      SIGN    = 1.0
      SLOPE   = 1
      KCOL    = .FALSE.
      KN      = 1
      KCOLN   = IZ(IPK+KN)
C
C     KCOLN PICKS UP COLUMN NUMBER TO INSERT
C     KN POINT TO COLUMN OF G MATRIX
C     SLOPE IS FLIP FLOP SWITCH FOR SLOPE COLUMN (KEEPS KCOL TRUE)
C
C
C     LOOP THROUGH COLUMNS OF GKT
C
      DO 400 I = 1,KSIZE
      CALL BLDPK (1,1,SCR2,BLOCK,1)
      IF (KCOLN .EQ. I) KCOL = .TRUE.
C
C     COPY A COLUMN OR OUTPUT A NULL COLUMN
C
      CALL INTPK (*340,SCR3,0,1,0)
      IF (KCOL) GO TO 995
  330 CALL ZNTPKI
      CALL BLDPKI (A,NR,SCR2,BLOCK)
      IF (IEOL .EQ. 0) GO TO 330
      GO TO 390
  340 IF (.NOT.KCOL) GO TO 390
C
C     LOOP THROUGH COLUMN OF G BUILDING COLUMN OF GKT
C
      DO 380 J = 1,NGSET
      NR = IZ(PSIL+J)
      K  = (IZ(PG+J)-1)*4
      CALL TRANSS (IZ(PBGPT+K),TT)
      CALL GMMATS (TT,3,3,1,TL,3,3,0,TG)
      GO TO (350,360), TYPE
C
C     TERMS OF SURFACE SPLINE
C
  350 CONTINUE
      DO 351 JJ = 3,9,3
      A = TG (JJ)*Z(N1)
      CALL BLDPKI (A,NR,SCR2,BLOCK)
      NR = NR + 1
  351 CONTINUE
      N1 = N1 + 1
      GO TO 380
C
C     TERMS OF LINEAR SPLINE
C
  360 IF (CTYPE .EQ. 2) GO TO 370
      IF (IS .EQ. 1) GO TO 350
      TGS( 1) = TG(3)
      TGS( 4) = TG(6)
      TGS( 7) = TG(9)
      TGS(11) = TG(1)
      TGS(12) = TG(2)
      TGS(14) = TG(4)
      TGS(15) = TG(5)
      TGS(17) = TG(7)
      TGS(18) = TG(8)
      GO TO 365
C
C     BODIES
C
  370 GO TO (372,371,373), IBTYP
  371 GO TO (373,372,372,373), IBCC
  372 TGS( 1) = TG(3)*SIGN
      TGS( 4) = TG(6)*SIGN
      TGS( 7) = TG(9)*SIGN
      TGS(11) =-TG(2)*SIGN
      TGS(12) = TG(1)*SIGN
      TGS(14) =-TG(5)*SIGN
      TGS(15) = TG(4)*SIGN
      TGS(17) =-TG(8)*SIGN
      TGS(18) = TG(7)*SIGN
      GO TO 365
  373 TGS( 1) = TG(2)
      TGS( 4) = TG(5)
      TGS( 7) = TG(8)
      TGS(11) = TG(3)
      TGS(12) = TG(1)
      TGS(14) = TG(6)
      TGS(15) = TG(4)
      TGS(17) = TG(9)
      TGS(18) = TG(7)
  365 T(1) = Z(N1)
      N1   = N1 + 1
      T(2) = 0.0
      T(3) = 0.0
      IF (OXR) GO TO 361
      T(2) = Z(N1)
      N1   = N1 + 1
  361 IF (OYR) GO TO 362
      T(3) = Z(N1)
      N1   = N1 + 1
  362 CALL GMMATS (TGS,6,3,0,T,3,1,0,AN)
      DO 363 JJ = 1,6
      CALL BLDPKI (AN(JJ),NR,SCR2,BLOCK)
      NR = NR + 1
  363 CONTINUE
  380 CONTINUE
C
C     COLUMN FINISHED CHECKSLOPE COLUMN NEXT OR END OF G
C
      IF (CTYPE .NE. 3) GO TO 382
      N1 = N1 + NGSET*IS
      GO TO 384
  382 IF (CTYPE .NE. 2) GO TO 383
      IF (IBTYP .EQ. 1) SIGN = -SIGN
      IF (IBTYP .NE. 2) GO TO 383
      IBCC = IBCC + 1
      IF (IBCC .EQ. 3) SIGN = -SIGN
      IF (IBCC .EQ. 5) SIGN = -SIGN
      IF (IBCC .EQ. 5) IBCC = 1
C
C     KEEP SLOPE NEG FOR ZY BODIES AND REPROCESS SAME COLUMN TWICE
C
      IF (IBCC.EQ.2 .OR. IBCC.EQ.4) N1 = N1 - NGSET*IS
      IF (IBCC .GT. 2) GO TO 390
  383 SLOPE = -SLOPE
      IF (SLOPE .NE. 1) GO TO 390
  384 KN = KN + 1
      IF (KN .GT. NKSET) GO TO 385
      KCOLN = IZ(IPK+KN)
  385 KCOL  = .FALSE.
  390 CALL BLDPKN (SCR2,BLOCK,TGKG)
  400 CONTINUE
C
C     SWITCH FILES FOR ANOTHER SPLINE
C
      CALL CLOSE (SCR2,1)
      CALL WRTTRL (TGKG)
      CALL CLOSE (SCR3,1)
      I    = SCR2
      SCR2 = SCR3
      SCR3 = I
      IPASS= IPASS + 1
      IF (ZAP) GO TO 1
      GO TO 10
C
C     FINISHED SWITCH FILES SO OUTPUT IS SCR2
C
C
C     IF ALL DONE BE SURE SCR2 IS GTKA
C
  500 I    = SCR2
      SCR2 = SCR3
      SCR3 = I
      IF (SCR3 .NE. 201) GO TO 520
      CALL GOPEN (SCR2,Z(BUFF1),0)
      CALL GOPEN (SCR3,Z(BUFF2),1)
      TGKG(1) = SCR2
      CALL RDTRL (TGKG)
      N    = TGKG(2)
      TGKG(1) = SCR3
      TGKG(2) = 0
      TGKG(6) = 0
      TGKG(7) = 0
      INCR = 1
      ITC  = 1
      CALL CYCT2B (SCR2,SCR3,N,Z,TGKG)
      CALL CLOSE (SCR2,1)
      CALL CLOSE (SCR3,1)
      CALL WRTTRL (TGKG)
  520 CONTINUE
      CALL CLOSE (SCR1,1)
      IF (NOGO .EQ. 0) GO TO 1000
C
C     ERROR MESSAGES
C
      CALL MESAGE (-61,0,NS)
  999 CALL MESAGE (-3,IFIL,NS)
  993 CALL MESAGE (-8,0,NS)
  998 WRITE  (OUT,9980) UFM,SCARD(1)
 9980 FORMAT (A23,' 2260, SINGULAR MATRIX DEVELOPED WHILE PROCESSING ',
     1       'SPLINE',I9)
      GO TO  1001
  997 CALL MESAGE (30,25,IC)
      GO TO 1001
  996 WRITE  (OUT,9960) UFM,SCARD(1),CCARD(1)
 9960 FORMAT (A23,' 2261, PLANE OF LINEAR SPLINE',I9,
     1        ' PERPENDICULAR TO PLANE OF AERO ELEMENT',I9)
      GO TO 1001
  995 WRITE  (OUT,9950) UFM,SCARD(1)
 9950 FORMAT (A23,' 2262, SPLINE',I9,' INCLUDES AERO BOX INCLUDED ON A',
     1       ' EARLIER SPLINE')
      GO TO 1001
  992 WRITE  (OUT,9920) UFM,SCARD(1)
 9920 FORMAT (A23,' 2263, INSUFFICIENT CORE TO PROCESS SPLINE',I9)
 1001 NOGO = 1
      GO TO 10
 1000 RETURN
      END
