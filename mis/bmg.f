      SUBROUTINE BMG
C
C     HYDROELASTIC BOUNDARY MATRIX GENERATOR
C
C     7/12/73 NO AXIAL SYMMETRY UPPER INTEGRATION LIMIT OF LAST
C             CIRCUMFERENTIAL GRID IS INCORRECT
C
      LOGICAL          LABFL    ,LKBFL    ,NSTAR    ,HEAD
      INTEGER          SYSBUF   ,NABFL(2) ,NKBFL(2) ,SUBR(2)  ,FORM   ,
     1                 BUF(10)  ,Z        ,SCRT1    ,BDPOOL   ,CSTM   ,
     2                 EQEXIN   ,BGPDT    ,RD       ,RDREW    ,ENTRYS ,
     3                 WRT      ,WRTREW   ,CLSREW   ,DMIG(3)  ,CLS    ,
     4                 FILE     ,BUF1     ,BUF2     ,BUF3     ,CORE   ,
     5                 POINT    ,BNDFL(2) ,FLAG     ,MONES(3) ,EOR
      REAL             RBUF(10) ,RZ(1)    ,KII
      DOUBLE PRECISION DZ(1)    ,DTEMP(3) ,TERM(3)  ,VI(3)    ,T0F(9) ,
     1                 TI(9)    ,AIN      ,DUB
      CHARACTER        UFM*23   ,UWM*25   ,UIM*29   ,SFM*25
      COMMON /XMSSG /  UFM      ,UWM      ,UIM      ,SFM
      COMMON /CONDAS/  CONSTS(5)
      COMMON /SYSTEM/  SYSBUF   ,IOUT     ,ISKP(52) ,IPREC
      COMMON /BLANK /  KFLAGS(2),VALUE(2)
      COMMON /ZZZZZZ/  Z(1)
      COMMON /NAMES /  RD,RDREW,WRT,WRTREW,CLSREW,CLS
      EQUIVALENCE      (CONSTS(2),TWOPI)  ,(CONSTS(4),DEGRAD)  ,
     1                 (Z(1),RZ(1),DZ(1)) ,(BUF(1),RBUF(1))
      DATA    SUBR  /  4HBMG ,4H    /,  NABFL / 4HABFL,4H    /
      DATA    BNDFL /  9614  ,96    /,  MONES / -1, -1, -1   /
      DATA    IS    /  1            /,  DMIG  / 114, 1, 120  /
      DATA    EOR   ,  NOEOR/ 1,0   /,  NKBFL / 4HKBFL,4H    /
      DATA    MATPOL,  BGPDT,EQEXIN,CSTM / 101,102,103,104   /
      DATA    BDPOOL/  201          /,  SCRT1 / 301          /
      DATA    IZ2   ,  IZ6,IZ7,IZ8,IZ9   /  2, 6, 7, 8, 9    /
C
C     DEFINE CORE AND BUFFER POINTERS
C
      CORE = KORSZ(Z)
      BUF1 = CORE - SYSBUF - 2
      BUF2 = BUF1 - SYSBUF - 2
      BUF3 = BUF2 - SYSBUF - 2
      CORE = BUF3 - 1
      IF (CORE .LT. 100) CALL MESAGE (-8,0,SUBR)
      KFLAGS(1) = -1
      KFLAGS(2) = -1
C
C     OPEN MATPOOL AND LOCATE THE BNDFL RECORD AS PREPARED BY IFP4.
C
      CALL PRELOC (*10000,Z(BUF1),MATPOL)
      CALL LOCATE (*10000,Z(BUF1),BNDFL,FLAG)
C
C     THIS MODULE DOES NOTHING IF THE MATPOOL IS PURGED OR THE BNDFL
C     RECORD IS ABSENT.  NOW READ THE HEADER DATA OF THIS RECORD.
C
      FILE = MATPOL
      CALL READ (*10002,*10003,MATPOL,Z(1),9,NOEOR,FLAG)
      IN = 10
      NN = Z(IZ9) + 9
      IF (NN+5 .GT. CORE) CALL MESAGE (-8,0,SUBR)
C
C     READ THE INDICES
C
      CALL READ (*10002,*10003,MATPOL,Z(IN),Z(IZ9),NOEOR,FLAG)
      VALUE(1) = Z(IZ6)
      VALUE(2) = 0.0
      IF (Z(IZ6) .EQ. 0) VALUE(1) = 1.0
C
C     MODIFY LIST OF INDICES TO FIT THE FOLLOWING TABLE
C
C        M      S1    S2         N              N*
C        -      --    --         -              --
C        0                      ALL             ALL
C
C                               K M
C     .GE.2      S     S        ---             NONE
C                                2
C
C                             (2K+1)M
C     .GE.2      S     A      -------           NONE
C                                4
C
C                                              (2K+1)M
C     .GE.2      A     S        NONE           -------
C                                                 4
C
C                                               K M
C     .GE.2      A     A        NONE            ---
C                                                2
C
C     K MAY BE 0,1,2,..... IN ORDER TO CHECK INDICE FOR MATCH.
C
      IF (Z(IZ6)) 90,90,10
C
C     M IS POSITIVE THUS CHECK FOR STAR OR NO-STAR INDICES PERMITTED.
C     DETERMINE THE FORM OF THE CHECK EQUATION.
C
C          Z(7) = S1
C          Z(8) = S2
C          Z(6) = M
C
   10 IF (Z(IZ7) .EQ. IS) GO TO 12
      NSTAR = .TRUE.
      GO TO 13
   12 NSTAR = .FALSE.
   13 IF (Z(IZ7) .EQ. Z(IZ8)) GO TO 14
      FORM = 1
      GO TO 15
   14 FORM = -1
C
C     NOW FORM NEW LIST OF INDICES
C
   15 INN = NN + 1
      NNN = NN
      DO 80 I = IN,NN
      N = (Z(I)-1)/2
      IF (MOD(Z(I),2)) 30,20,30
C
C     NON-STAR CASE
C
   20 IF (NSTAR) GO TO 80
      IF (FORM ) 40,70,70
C
C     STAR CASE
C
   30 IF (.NOT.NSTAR) GO TO 80
      IF (FORM) 40,70,70
C
C                           K M
C     CHECK USING EQUATION  ---
C                            2
C
   40 N2 = N*2
      K  = N2/Z(IZ6)
      IF (K*Z(IZ6) .NE. N2) GO TO 80
C
C     GOOD INDICE,  ADD IT TO THE LIST
C
   50 NNN = NNN + 1
      Z(NNN) = Z(I)
      GO TO 80
C
C                            (2K+1)M
C     CHECK USING EQUATION   -------
C                               4
C
   70 N4 = N*4
      IK = N4 / Z(IZ6)
      IK = IK - 1
      K  = IK / 2
      IF ((2*K+1)*Z(IZ6) .EQ. N4) GO TO 50
   80 CONTINUE
C
C     LIST IS COMPLETE
C
      IN = INN
      NN = NNN
   90 LABFL = .TRUE.
      IF (NN .LT. IN) LABFL = .FALSE.
C
C     SET LKBFL AS A FLAG INDICATING WHETHER KBFL WILL BE GENERATED
C     ALONG WITH ABFL.  IF G IS NON-ZERO THEN KBFL WILL BE GENERATED.
C
      LKBFL = .TRUE.
      IF (RZ(IZ2).EQ.0.0) LKBFL = .FALSE.
      IF (LKBFL) KFLAGS(1) = 0
      IF (LABFL) KFLAGS(2) = 0
      IF (.NOT.LABFL .AND. .NOT.LKBFL) GO TO 10000
C
C     BGPDT IS NOW READ INTO CORE AS 5 WORD ENTRIES, RESERVING FIRST
C     WORD FOR THE EXTERNAL ID.
C
      FILE   = BGPDT
      IBGPDT = NN + 1
      NBGPDT = NN
      CALL GOPEN (BGPDT,Z(BUF2),RDREW)
  100 CALL READ (*10002,*120,BGPDT,Z(NBGPDT+2),4,NOEOR,FLAG)
      NBGPDT = NBGPDT + 5
      IF (NBGPDT+5 .GT. CORE) CALL MESAGE (-8,0,SUBR)
      GO TO 100
  120 CALL CLOSE (BGPDT,CLSREW)
C
C     READ EQEXIN PLACING EXTERNAL ID ON RESPECTIVE BGPDT ENTRY.
C
      FILE = EQEXIN
      CALL GOPEN (EQEXIN,Z(BUF2),RDREW)
  130 CALL READ (*10002,*140,EQEXIN,BUF,2,NOEOR,FLAG)
      N = 5*BUF(2) - 5 + IBGPDT
      Z(N) = BUF(1)
      GO TO 130
  140 CALL CLOSE (EQEXIN,CLSREW)
      LBGPDT = NBGPDT - IBGPDT + 1
      ENTRYS = LBGPDT / 5
C
C     SORT THE BGPDT ON EXTERNAL ID
C
      CALL SORT (0,0,5,1,Z(IBGPDT),LBGPDT)
C
C  BLAST CSTM INTO CORE
C
      FILE = CSTM
      CALL GOPEN (CSTM,Z(BUF2),RDREW)
      ICSTM = NBGPDT + 1
      CALL READ (*10002,*160,CSTM,Z(ICSTM),CORE-ICSTM,NOEOR,FLAG)
      CALL MESAGE (-8,0,SUBR)
  160 NCSTM = ICSTM + FLAG - 1
      LCSTM = NCSTM - ICSTM + 1
      CALL CLOSE (CSTM,CLSREW)
C
C     LOCATE THE T   MATRIX IN THE CSTM DATA BY USING CSID = CDF IN
C                 0F
C
C     THE HEADER DATA.      ( Z(1) )
C
      DO 200 I = ICSTM,NCSTM,14
      IF (Z(1) .EQ. Z(I)) GO TO 240
  200 CONTINUE
      WRITE  (IOUT,210) SFM,Z(1)
  210 FORMAT (A25,' 4060, COORDINATE SYSTEM =',I9,
     1       ' CAN NOT BE FOUND IN CSTM DATA.')
      GO TO 9999
  240 N = I + 5
      DO 250 I = 1,9
      T0F(I) = DBLE(RZ(N))
      N = N + 1
  250 CONTINUE
C
C     OPEN BDPOOL FOR ABFL, AND SCRATCH1 FOR KBFL AND WRITE THE DMIG
C     HEADER INFORMATION.
C
      CALL GOPEN (BDPOOL,Z(BUF2),WRTREW)
C
C     WRITE DMIG RECORD ID
C
      CALL WRITE (BDPOOL,DMIG,3,NOEOR)
      BUF(1) = NABFL(1)
      BUF(2) = NABFL(2)
      BUF(3) = 0
      BUF(4) = 1
      BUF(5) = 1
      BUF(6) = IPREC
      BUF(7) = 0
      BUF(8) = 0
      BUF(9) = 0
      IF (.NOT. LABFL) GO TO 270
      CALL WRITE (BDPOOL,BUF,9,NOEOR)
  270 IF (.NOT.LKBFL) GO TO 280
      FILE = SCRT1
      CALL OPEN (*10001,SCRT1,Z(BUF3),WRTREW)
      BUF(1) = NKBFL(1)
      BUF(2) = NKBFL(2)
      CALL WRITE (SCRT1,BUF,9,NOEOR)
C
C     READ SOME FLUID-PT DATA (IDF,R,Z,L,C,S,RHO)
C
  280 FILE = MATPOL
      CALL READ (*10002,*10003,MATPOL,IDF,1,NOEOR,FLAG)
  285 IDATA = NCSTM + 1
      NDATA = NCSTM + 6
      CALL READ (*10002,*10003,MATPOL,Z(IDATA),6,NOEOR,FLAG)
C
C     START BUILDING TABLE OF CONNECTED GRID POINTS.
C     READ ID,PHI.  CREATE A 26 WORD ENTRY FOR EACH ID,PHI.
C
      ITABLE = NDATA + 1
C
C     INSURE THAT TABLE STARTS ON AN EVEN BOUNDARY FOR DOUBLE
C     PRECISION
C
      IF (MOD(ITABLE,2) .NE. 1) ITABLE = ITABLE + 1
      NTABLE = ITABLE - 1
  290 CALL READ (*10002,*10003,MATPOL,Z(NTABLE+1),2,NOEOR,FLAG)
      IF (Z(NTABLE+1) .EQ. -1) GO TO 300
C
C     CONVERT PHI TO RADIANS
C
      RZ(NTABLE+2) = RZ(NTABLE+2)*DEGRAD
      NTABLE = NTABLE + 26
      IF (NTABLE+26 .GT. CORE) CALL MESAGE (-8,0,SUBR)
      GO TO 290
C
C     COMPUTATION AND INSERTION OF PHI   AND PHI   FOR EACH ENTRY.
C                                     0         1
C
  300 DO 370 I = ITABLE,NTABLE,26
C
C     SET UP PHI  IN THIRD SLOT OF ENTRY = (PHI  + PHI   )/2.0
C               0                              I      I-1
C
      IF (I .NE. ITABLE) GO TO 310
C
C     SPECIAL CASE ON FIRST POINT, TEST M TO FIND PHI
C                                                    I-1
C
      IF (Z(IZ6) .GT. 1) GO TO 320
      PHIL1 = RZ(NTABLE-24) - TWOPI
      GO TO 350
  320 PHIL1 = RZ(ITABLE+1)
      GO TO 350
  310 PHIL1 = RZ(I-25)
  350 RZ(I+2) = (RZ(I+1) + PHIL1) / 2.0
C
C     SET UP PHI  IN FOURTH SLOT OF ENTRY = (PHI  + PHI   )/2.0
C               1                               I      I+1
C
      IF (I .NE. NTABLE-25) GO TO 345
C
C     SPECIAL CASE ON LAST POINT, TEST M TO FIND PHI
C                                                   I+1
C
      IF (Z(IZ6) .GT. 1) GO TO 340
      PHIP1 = RZ(ITABLE+1) + TWOPI
      GO TO 360
  340 PHIP1 = RZ(NTABLE-24)
      GO TO 360
  345 PHIP1 = RZ(I+27)
  360 RZ(I+3) = (RZ(I+1) + PHIP1) / 2.0
  370 CONTINUE
C
C     PICK UP NEXT FLUID POINT IDF
C
      NEXTID = 0
      CALL READ (*10002,*400,MATPOL,NEXTID,1,NOEOR,FLAG)
      IF (NEXTID .NE. IDF) GO TO 400
C
C     NEXTID IS SAME AS CURRENT IDF, THUS ADD ANOTHER ENTRY OF R,Z,L,C,
C     S,RH FIRST MOVE SINGLE ENTRY DOWN UNDER TABLE SO IT CAN GROW.
C
      Z(NTABLE+1) = Z(IDATA  )
      Z(NTABLE+2) = Z(IDATA+1)
      Z(NTABLE+3) = Z(IDATA+2)
      Z(NTABLE+4) = Z(IDATA+3)
      Z(NTABLE+5) = Z(IDATA+4)
      Z(NTABLE+6) = Z(IDATA+5)
      IDATA = NTABLE + 1
      NDATA = NTABLE + 6
  380 IF (NDATA+6 .GT. CORE) CALL MESAGE (-8,0,SUBR)
      CALL READ (*10002,*10003,MATPOL,Z(NDATA+1),6,NOEOR,FLAG)
      NDATA = NDATA + 6
C
C     SKIP THE ID-PHI PAIRS AS THEY SHOULD BE IDENTICAL TO ONES ALREADY
C     IN THE TABLE.
C
  390 CALL READ (*10002,*10003,MATPOL,BUF,2,NOEOR,FLAG)
      IF (BUF(1) .NE. -1) GO TO 390
C
C     READ THE NEXTID
C
      NEXTID = 0
      CALL READ (*10002,*400,MATPOL,NEXTID,1,NOEOR,FLAG)
      IF (NEXTID .EQ. IDF) GO TO 380
C
C     SORT THE TABLE ON FIELD ONE OF EACH ENTRY THE ID.
C
  400 CALL SORT (0,0,26,1,Z(ITABLE),NTABLE-ITABLE+1)
C
C                                  T
C     FOR EACH ENTRY GENERATE THE T T   MATRICE AND IF LKBFL = .TRUE.
C                                  I 0F
C
C     THE W  MATRICE.
C          I
C
      DO 500 I = ITABLE,NTABLE,26
C
C     LOCATE THE TRANSFORMATION MATRIX IN DOUBLE PRECISION.
C     FIRST LOCATE BGPDT ENTRY
C
      KID = Z(I)
      CALL BISLOC (*10004,KID,Z(IBGPDT),5,ENTRYS,POINT)
      POINT = POINT + IBGPDT
      CALL BMGTNS (Z(ICSTM),LCSTM,Z(POINT),TI(1))
C
C     COMPUTE VI MATRIX.  (3X3)
C
      CALL GMMATD (TI(1),3,3,1, T0F(1),3,3,0, Z(I+4))
      IF (.NOT. LKBFL) GO TO 500
      J = (I+4)/2
      RZ(I+22) = DZ(J+3)
      RZ(I+23) = DZ(J+6)
      RZ(I+24) = DZ(J+9)
  500 CONTINUE
C
C     GENERATION AND OUTPUT OF MATRIX COLUMNS TO THE ABFL MATRIX.
C
      IF (.NOT.LABFL) GO TO 690
      DO 680 I = IN,NN
C
C     COLUMN INDEX INFORMATION GJ,CJ FOR THIS HARMONIC COLUMN
C
      BUF(1) = IDF + Z(I)*500000
      BUF(2) = 0
      CALL WRITE (BDPOOL,BUF,2,NOEOR)
C
C     TERMS OF THE COLUMN
C
      DO 670 J = ITABLE,NTABLE,26
C
C     3 TERMS FOR THE J-TH ID ARE THE FOLLOWING SUMMATION
C
      TERM(1) = 0.0D0
      TERM(2) = 0.0D0
      TERM(3) = 0.0D0
      DO 650 K = IDATA,NDATA,6
C
C                     N
C     COMPUTATION OF A
C                     I
C
      AIN = RZ(K)*RZ(K+2)
      N   = (Z(I) - 1) / 2
      FN  = N
      IF (N) 520,510,520
C
C     N = 0
C
  510 AIN = AIN*DBLE(RZ(J+3) - RZ(J+2))
      GO TO 545
C
C     N IS POSITIVE, CHECK FOR STAR CASE = N*
C
  520 IF (MOD(Z(I),2)) 540,530,540
  530 DUB = (SIN(RZ(J+3)*FN) - SIN(RZ(J+2)*FN)) / FN
      AIN = AIN*DUB
      GO TO 545
  540 DUB = (COS(RZ(J+2)*FN) - COS(RZ(J+3)*FN)) / FN
      AIN = AIN*DUB
C
C     FORM VI MATRIX FOR THIS POINT
C
  545 DTEMP(1) = RZ(K+3)*COS(RZ(J+1))
      DTEMP(2) = RZ(K+3)*SIN(RZ(J+1))
      DTEMP(3) = RZ(K+4)
      CALL GMMATD (Z(J+4),3,3,0, DTEMP(1),3,1,0, VI(1))
      DO 550 L = 1,3
      TERM(L) = TERM(L) + AIN*VI(L)
  550 CONTINUE
  650 CONTINUE
C
C     OUTPUT THESE 3 TERMS
C
      BUF(1) = Z(J)
      DO 660 K = 1,3
      BUF(2) = K
      RBUF(3) = TERM(K)
      IF (RBUF(3)) 655,660,655
  655 CALL WRITE (BDPOOL,BUF,3,NOEOR)
  660 CONTINUE
  670 CONTINUE
      CALL WRITE (BDPOOL,MONES,2,NOEOR)
  680 CONTINUE
C
C     GENERATION AND OUTPUT OF COLUMNS TO THE KBFL MATRIX.
C
  690 IF (.NOT.LKBFL) GO TO 800
      DO 750 I = ITABLE,NTABLE,26
      COSPHI = COS(RZ(I+1))
      SINPHI = SIN(RZ(I+1))
      ANGLE   = RZ(I+3) - RZ(I+2)
C
C     PUT OUT 3 COLUMNS FOR EACH OF THESE CONNECTED GRIDPOINTS
C
C     SOLVE NOW FOR K   V  = 3X1  CONSTANT FOR THE 3 COLUMNS
C                    II  I
C
C     AND IS A SUMMATION
C
      TERM(1) = 0.0D0
      TERM(2) = 0.0D0
      TERM(3) = 0.0D0
      DO 715 J = IDATA,NDATA,6
      KII = RZ(J)*RZ(J+2)*RZ(J+5)*RZ(IZ2)*ANGLE
      DTEMP(1) = KII*RZ(J+3)*COSPHI
      DTEMP(2) = KII*RZ(J+3)*SINPHI
      DTEMP(3) = KII*RZ(J+4)
      CALL GMMATD (Z(I+4),3,3,0, DTEMP(1),3,1,0, VI(1))
      DO 710 K = 1,3
      TERM(K) = TERM(K) + VI(K)
  710 CONTINUE
  715 CONTINUE
C
C     PUT OUT THE 3 COLUMNS
C
      DO 740 J = 1,3
      HEAD = .FALSE.
      L = I + J + 21
      DTEMP(1) = DBLE(RZ(L))*TERM(1)
      DTEMP(2) = DBLE(RZ(L))*TERM(2)
      DTEMP(3) = DBLE(RZ(L))*TERM(3)
      BUF(1) = Z(I)
      DO 730 K = 1,3
      BUF(2) = K
      RBUF(3) = DTEMP(K)
C
C     TERM IS NOT WRITTEN IF HAS A ZERO VALUE
C
      IF (RBUF(3)) 720,730,720
  720 IF (HEAD) GO TO 721
      BUF(4) = Z(I)
      BUF(5) = J
      CALL WRITE (SCRT1,BUF(4),2,NOEOR)
      HEAD = .TRUE.
  721 CALL WRITE (SCRT1,BUF,3,NOEOR)
  730 CONTINUE
      IF (HEAD) CALL WRITE (SCRT1,MONES,2,NOEOR)
  740 CONTINUE
  750 CONTINUE
C
C     PROCESS THE NEXT FLUID POINT
C
  800 IF (NEXTID) 810,840,810
  810 IDF = NEXTID
      GO TO 285
C
C     ALL FLUID POINTS HAVE NOW BEEN PROCESSED.  APPEND THE KBFL, IF
C     ANY, DATA TO THE ABFL DATA AND WRAP UP.
C
  840 IF (LABFL) CALL WRITE (BDPOOL,MONES,2,NOEOR)
      IF (.NOT.LKBFL) GO TO 900
      CALL WRITE (SCRT1,0,0,EOR)
      CALL CLOSE (SCRT1,CLSREW)
      FILE = SCRT1
      CALL OPEN (*10001,SCRT1,Z(BUF3),RDREW)
  850 CALL READ (*10002,*860,SCRT1,Z(1),CORE,NOEOR,FLAG)
      CALL WRITE (BDPOOL,Z(1),CORE,NOEOR)
      GO TO 850
  860 CALL WRITE (BDPOOL,Z(1),FLAG,NOEOR)
      CALL WRITE (BDPOOL,MONES,2,EOR)
  900 CALL CLOSE (BDPOOL,CLSREW)
C
C     PREPARE AND WRITE TRAILER
C
      BUF(1) = BDPOOL
C
C     SET TRAILER BIT FOR DMIG CARDS
C
      BUF(2) = 32768
      BUF(3) = 0
      BUF(4) = 0
      BUF(5) = 0
      BUF(6) = 0
      BUF(7) = 0
      CALL WRTTRL (BUF)
      CALL CLOSE (SCRT1,CLSREW)
C
C     END OF PROCESSING
C
10000 CALL CLOSE (MATPOL,CLSREW)
      RETURN
C
C     ERROR CONDITIONS
C
10001 CALL MESAGE (-1,FILE,SUBR)
10002 CALL MESAGE (-2,FILE,SUBR)
10003 CALL MESAGE (-3,FILE,SUBR)
      GO TO 10000
10004 WRITE  (IOUT,10005) SFM,Z(I)
10005 FORMAT (A25,' 4061, CONNECTED FLUID POINT ID =',I10,
     1       ' IS MISSING BGPDT DATA.')
C
 9999 CALL MESAGE (-61,0,0)
      RETURN
C
      END
