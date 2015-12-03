      SUBROUTINE CINVP3
C
C     SUBROUTINE CINVP3, THE MAIN LINK OF CINVPR, SOLVES FOR THE
C     EIGENVALUES AND EIGENVECTORS OF (LAMBDA**2*M + LAMBDA*B*K)
C
C     TYPE DECLARATIONS
C
      INTEGER            COMFLG    ,REAL     ,END      ,FILEK
      INTEGER            FILEB     ,FILELM   ,SCRFIL
      INTEGER            SWITCH    ,CDP      ,FILEL    ,SR1FIL   ,
     1                   FILEU     ,SR3FIL   ,SR4FIL   ,SR8FIL   ,
     2                   SR9FIL    ,SYSBUF   ,NAME(2)  ,FILEVC   ,
     3                   TIMED     ,TIMEIT   ,SQR      ,FILE(7)  ,
     4                   SR2FIL    ,S11FIL   ,T1       ,T2
      REAL               MAXMOD
      DOUBLE PRECISION   DZ(1)     ,LMBDA    ,PLUS1(2)
      DOUBLE PRECISION   LAMBDA    ,ALN(2)   ,ALNM1(2) ,ETA(2)   ,
     1                   ETANM1(2) ,H2N(2)   ,H2NM1(2) ,LAM1     ,
     2                   LM1NM1(2) ,LAM2(2)  ,LM2NM1(2),CON1(2)  ,
     3                   CON2(2)   ,CN(2)    ,DELTA(2) ,XYZ(2)
      COMMON   /SYSTEM/  SYSBUF    ,NOUT
      COMMON   /CINVPX/  FILEK(7)  ,FILEM(7) ,FILEB(7) ,FILELM(7),
     1                   FILEVC(7) ,DUDXX    ,SCRFIL(11)         ,
     2                   NOREG     ,EPS
      COMMON   /CINFBX/  FILEL(7)  ,FILEU(7)
      COMMON   /CINVXX/  LAMBDA(2) ,SWITCH   ,COMFLG   ,LMBDA(2) ,
     1                   ITERTO    ,TIMED    ,NOCHNG   ,RZERO    ,
     2                   IND       ,IVECT    ,IREG     ,REAL     ,
     3                   LEFT      ,NORTHO   ,NOROOT   ,NZERO    ,
     4                   LAM1(2)   ,MAXMOD   ,NODES    ,NOEST    ,
     5                   ISTART    ,IND1     ,ITER     ,ISYM
      COMMON   /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                   RDP       ,CSP      ,CDP      ,SQR
      COMMON   /CDCMPX/  XXYY(20)  ,IOFFF
      COMMON   /ZZZZZZ/  Z(1)
      EQUIVALENCE        (FILEK(2),NCOL)     ,(SCRFIL(1),SR1FIL) ,
     1                   (SCRFIL(2),SR2FIL)  ,(SCRFIL(3),SR3FIL) ,
     2                   (SCRFIL(4),SR4FIL)  ,
     4                   (SCRFIL(8),SR8FIL)  ,(SCRFIL(9),SR9FIL) ,
     5                   (DZ(1),Z(1))        ,(SCRFIL(11),S11FIL)
      DATA      NAME  /  4HCINV,4HP3   /
      DATA      PLUS1 /  +1.D0 ,0.D0   /
C
C     DEFINITION OF LOCAL PARAMETERS
C
C     ITER     =
C     IRAPID   =
C     IEP2     =
C     NCOUNT   =
C     IEPCNT   =
C     SWITCH   =
C     A        =
C     EP1      =
C     EP2      =
C     EP3      =
C     GAMMA    =
C     II1      =    POINTER TO U(N)
C     II2      =    POINTER TO U(N-1) OR DELTA U(N)
C     JJ1      =    POINTER TO F(N)
C     JJ2      =    POINTER TO DELTA F(N-1)
C     JJ3      =    POINTER TO F(N-1) OR DELTA F(N)
C     JJ4      =
C     JJ5      =
C     KK1      =    POINTER TO V(N)
C     KK2      =    POINTER TO V(N-1)
C
   10 CONTINUE
      TIMEIT= 0
      NZ    = KORSZ(Z)
      NCOL2 = NCOL  + NCOL
      NCOL4 = NCOL2 + NCOL2
C
C     INITIALIZE
C
      CN(1)  = 0.0D0
      CN(2)  = 0.0D0
      XYZ(1) = 0.0D0
      XYZ(2) = 0.0D0
      H2N(1) = 0.0D0
      H2N(2) = 0.0D0
      LAM2(1)= 0.0D0
      LAM2(2)= 0.0D0
      LAM1(1)= 0.0D0
      LAM1(2)= 0.0D0
      ITER   = 0
      CALL KLOCK(T1)
      IRAPID = 0
      IEP2   = 0
   20 NCOUNT = 0
      IEPCNT = 0
      IF (SWITCH .EQ. 1) GO TO 30
      FILEL(1) = SR3FIL
      FILEU(1) = SR4FIL
      GO TO 40
   30 FILEL(1) = SR8FIL
      FILEU(1) = SR9FIL
   40 FILEL(5) = CDP
      FILEL(3) = FILEK(3)
      FILEU(7) = IOFFF
      FILE(4)  = SQR
      FILE(5)  = CDP
C
C     SET CONVERGENCE CRITERIA
C
      A   = .1
      CALL SSWTCH (12,IDIAG)
      EP1 = .001
      EP2 = .02
      EP3 = .05
      GAMMA = .01
C
C     INITILIZE POINTERS TO VECTORS
C
      II1 = 1
      II2 = II1 + NCOL2
      JJ1 = II2 + NCOL2
      JJ2 = JJ1 + NCOL2
      JJ3 = JJ2 + NCOL2
      JJ5 = JJ3 + NCOL2
      KK1 = JJ5 + NCOL2
      KK2 = KK1 + NCOL2
      KK3 = KK2 + NCOL2
      KK4 = KK3 + NCOL2
      KK5 = KK4 + NCOL2
      KK6 = KK5 + NCOL2
      LL1 = KK6 + NCOL2
      LL2 = LL1 + NCOL2
      END = (LL2 + NCOL2)*2
      IOBUF = NZ - SYSBUF + 1
      IBUF1 = IOBUF - SYSBUF
      IBUF2 = IBUF1 - SYSBUF
      IBUF3 = IBUF2 - SYSBUF
C     IBUF4 = IBUF3 - SYSBUF
C     IBUF5 = IBUF4 - SYSBUF
C     IBUF6 = IBUF5 - SYSBUF
C     IF (END .GE. IBUF6) GO TO 240
C     NZZ = IBUF6 - END
      IF (END .GE. IBUF3) GO TO 600
      NZZ = IBUF3 - END
C     IFILE = FILEL(1)
C     CALL OPEN (*610,FILEL,Z(IBUF4),0)
C     IFILE = FILEU(1)
C     CALL OPEN (*610,FILEU,Z(IBUF5),0)
C     IFILE = FILEM(1)
C     CALL OPEN (*610,FILEM,Z(IBUF6),0)
C
C     GENERATE A STARTING VECTOR
C
C     FORM U0
C
      IF (LEFT .EQ. 1) GO TO 500
      LAM1(1) = 0.0D0
      LAM1(2) = 0.0D0
      GO TO 70
   50 LAM1(1) = 0.0D0
      LAM1(2) = 0.0D0
      IF (NORTHO .EQ. 0) GO TO 70
C
C     TEST FOR INSUFFICIENT TIME
C
      CALL KLOCK  (ICURNT)
      CALL TMTOGO (IIJJKK)
      NAVG = (ICURNT-ISTART)/NORTHO
      IF (IIJJKK .GE. 2*NAVG) GO TO 70
   60 COMFLG = 8
      GO TO 490
   70 CONTINUE
      IF (IVECT .EQ. 1) GO TO 90
      K = IABS (IND)
      DO 80 I = 1,NCOL2,2
      DZ(I) = (MOD(K,13)+1)*(1+5*I/NCOL)
      K = K + 1
      DZ(I+1) = 0.D0
   80 DZ(I  ) = 1.D0/DZ(I)
      CALL CNORM1 (DZ(II1),NCOL)
C
C     FORM V0 = LAMBDA*U0
C
      GO TO 110
C
C     USE PREVIOUSLY STORED VECTOR FOR STARTING VECTOR
C
   90 IFILE = FILEVC(1)
      CALL GOPEN  (FILEVC,Z(IOBUF),RD)
      CALL BCKREC (FILEVC(1))
      IN1 = 1
      IF (COMFLG .NE. 1) GO TO 100
      IN1 = JJ5
      CALL BCKREC (FILEVC(1))
  100 CALL FREAD  (FILEVC,DZ(IN1),NCOL4,1)
      IF (COMFLG .EQ. 1) GO TO 140
      CALL BCKREC (FILEVC(1))
      CALL CLOSE  (FILEVC(1),NOREW)
      IVECT = 0
  110 CONTINUE
      DO 120 IU = 1,NCOL2,2
      J = KK1 + IU - 1
      DZ(J  ) = DZ(IU)*LAMBDA(1) - DZ(IU+1)*LAMBDA(2)
  120 DZ(J+1) = DZ(IU)*LAMBDA(2) + DZ(IU+1)*LAMBDA(1)
      IF (NORTHO .EQ. 0) GO TO 150
      CALL ORTHO (DZ(II1),DZ(KK1),DZ(KK2),DZ(KK3),DZ(KK4),DZ(KK5),
     1            DZ(KK6),NZZ,Z(IOBUF),Z(IBUF1),Z(IBUF2),Z(IBUF3))
      IF (FILEB(1) .NE. 0) GO TO 150
      DO 130 IU = 1,NCOL2,2
      J = KK1 + IU - 1
      DZ(J  ) = DZ(IU)*LAMBDA(1) - DZ(IU+1)*LAMBDA(2)
  130 DZ(J+1) = DZ(IU)*LAMBDA(2) + DZ(IU+1)*LAMBDA(1)
      GO TO 150
C
C     PICK UP LAST ITERATED VECTOR FOR A STARTING VECTOR
C
  140 CALL FREAD  (FILEVC,DZ,NCOL4,1)
      CALL SKPREC (FILEVC,-2)
      CALL CLOSE  (FILEVC(1),NOREW)
      GO TO 110
  150 CONTINUE
      CALL CM TIM U (DZ(II1),DZ(JJ1),0,Z(IOBUF))
      IF (FILEB(1) .EQ. 0) GO TO 160
      FILE(1) = FILEB(1)
      CALL CM TIM U (DZ(II1),DZ(KK2),FILE,Z(IBUF1))
      CON1(1) = 2.0D0*LAMBDA(1)
      CON1(2) = 2.0D0*LAMBDA(2)
      CALL CDIVID (PLUS1,CON2,CON1,2)
      CON2(1) = -CON2(1)
      CON2(2) = -CON2(2)
      CALL CSUB (DZ(JJ1),DZ(KK2),DZ(JJ1),PLUS1,CON2)
  160 CONTINUE
      CALL CX TRN Y (DZ(II1),DZ(JJ1),ALN(1))
      CALL CSQRTX (ALN(1),ALN(1))
C
C     COMPUTE THE R.H.S. OF THE SYSTEM OF EQUATIONS
C
  170 FILE(1) = SR2FIL
      IF (SWITCH .EQ. 1) FILE(1) = S11FIL
      CALL CM TIM U (DZ(II1),DZ(LL1),FILE(1),Z(IOBUF))
      CALL CM TIM U (DZ(KK1),DZ(LL2),0      ,Z(IOBUF))
      CALL CSUB (DZ(LL1),DZ(LL2),DZ(LL2),PLUS1(1),PLUS1(1))
C
C     SHIFT POINTERS
C
      II  = II1
      II1 = II2
      II2 = II
      II  = JJ1
      JJ1 = JJ2
      JJ2 = JJ3
      JJ3 = II
C
C     SAVE THE N-1 VECTOR
C
      IF (SWITCH .NE. 0) GO TO 190
      IXX = JJ5 + NCOL2 - 1
      IXZ = II2
      DO 180 I = JJ5,IXX
      DZ(I) = DZ(IXZ)
  180 IXZ = IXZ + 1
  190 CONTINUE
      CALL TMTOGO (IXX)
      IF (IXX .LE. 0) GO TO 60
C
C     SHIFT PARAMETERS
C
      ALNM1(1)  = ALN(1)
      ALNM1(2)  = ALN(2)
      ETANM1(1) = XYZ(1)
      ETANM1(2) = XYZ(2)
      H2NM1(1)  = H2N(1)
      H2NM1(2)  = H2N(2)
      LM1NM1(1) = LAM1(1)
      LM1NM1(2) = LAM1(2)
      LM2NM1(1) = LAM2(1)
      LM2NM1(2) = LAM2(2)
C
C     CALL CINFBS TO MAKE ONE ITERATION
C
      CALL CINFBS (DZ(LL2),DZ(II1),Z(IOBUF))
      ITERTO = ITERTO + 1
      ITER   = ITER + 1
      IEPCNT = IEPCNT + 1
      CALL CNORM (DZ(II1),CN(1),DZ(II2))
C
      IF (IDIAG .EQ. 0) GO TO 210
      KKKK = II1 + NCOL2 - 1
      WRITE  (NOUT,200) ITERTO,ITER,CN,TIMED,TIMEIT,(DZ(KX),KX=II1,KKKK)
  200 FORMAT (15H ITERTO =        ,I5,10H ITER =     ,I5,' CN =    ',
     1        2D15.5,10H TIMED =    ,I5,10H TIMEIT=       ,I5,
     2        //,20H ITERATER VECTOR      , //,(10D12.4))
  210 CONTINUE
C
C     COMPUTE V(N)BAR
C
      CON1(1) =-CN(1)/(CN(1)**2 + CN(2)**2)
      CON1(2) = CN(2)/(CN(1)**2 + CN(2)**2)
      CALL CSUB (DZ(II1),DZ(II2),DZ(KK1),LAMBDA,CON1)
      IF (NORTHO .EQ. 0) GO TO 220
C
C     ORTHOGONALIZE CURRENT ITERANT WITH RESPECT TO VECTORS FOUND IN
C     THE CURRENT AND PREVIOUS REGIONS
C
      CALL ORTHO (DZ(II1),DZ(KK1),DZ(KK2),DZ(KK3),DZ(KK4),DZ(KK5),
     1            DZ(KK6),NZZ,Z(IOBUF),Z(IBUF1),Z(IBUF2),Z(IBUF3))
  220 CONTINUE
      IF (FILEB(1) .NE. 0) GO TO 230
C
C     COMPUTE V(N)
C
      CALL CSUB (DZ(II1),DZ(II2),DZ(KK1),LAMBDA,CON1(1))
  230 CONTINUE
C
C     BEGIN TESTING CONVERGENCE CRITERIA
C
C     COMPUTE F(N)
C
      CALL CM TIM U (DZ(II1),DZ(JJ1),0,Z(IOBUF))
      IF (FILEB(1) .EQ. 0) GO TO 240
      FILE(1) = FILEB(1)
      CALL CM TIM U (DZ(II1),DZ(KK2),FILE,Z(IBUF1))
      CON1(1) = 2.0D0*LAMBDA(1)
      CON1(2) = 2.0D0*LAMBDA(2)
      CALL CDIVID (PLUS1,CON2,CON1,2)
      CON2(1) = -CON2(1)
      CON2(2) = -CON2(2)
      CALL CSUB (DZ(JJ1),DZ(KK2),DZ(JJ1),PLUS1,CON2)
  240 CONTINUE
C
C     COMPUTE ALPHA(N)
C
      CALL CX TRN Y (DZ(II1),DZ(JJ1),ALN(1))
      CALL CSQRTX (ALN(1),ALN(1))
C
C     COMPUTE DELTA U(N)
C
      CON1(1) = ALN(1)/(ALN(1)**2 + ALN(2)**2)
      CON1(2) =-ALN(2)/(ALN(1)**2 + ALN(2)**2)
      CON2(1) = ALNM1(1)/(ALNM1(1)**2 + ALNM1(2)**2)
      CON2(2) =-ALNM1(2)/(ALNM1(1)**2 + ALNM1(2)**2)
      CALL CSUB (DZ(II1),DZ(II2),DZ(II2),CON1(1),CON2(1))
C
C     COMPUTE DELTA F(N)
C
      CALL CSUB (DZ(JJ1),DZ(JJ3),DZ(JJ3),CON1(1),CON2(1))
      CON1(1) = CN(1)*ALN(1) - CN(2)*ALN(2)
      CON1(2) = CN(2)*ALN(1) + CN(1)*ALN(2)
      LAM1(1) = (ALNM1(1)*CON1(1) + ALNM1(2)*CON1(2))/(CON1(1)**2 +
     1          CON1(2)**2)
      LAM1(2) = (ALNM1(2)*CON1(1) - ALNM1(1)*CON1(2))/(CON1(1)**2 +
     1          CON1(2)**2)
      IF (IRAPID .EQ. 1) GO TO 410
      CALL CX TRN Y (DZ(II2),DZ(JJ3),ETA(1))
      CALL CSQRTX (ETA(1),XYZ(1))
C
      IF (IDIAG .EQ. 0) GO TO 260
      WRITE  (NOUT,250) LAM1,XYZ,ALN
  250 FORMAT (12H LAMBDA =    ,2D15.5,12H  ETA =        ,2D15.5,
     1        12H ALPHA =     ,2D15.5)
  260 CONTINUE
      IF (ITER .EQ. 1) GO TO 170
C
C     RAPID CONVERGENCE TEST
C
C     IF (ETA.GE.A*EPS*GAMMA*(1.+LAMBDA/LAM1)
C
      CON1(1) = (LAMBDA(1)*LAM1(1) + LAMBDA(2)*LAM1(2))/(LAM1(1)**2 +
     1          LAM1(2)**2)
      CON1(2) = (LAMBDA(2)*LAM1(1) - LAMBDA(1)*LAM1(2))/(LAM1(1)**2 +
     1          LAM1(2)**2)
      IF (DSQRT(XYZ(1)**2+XYZ(2)**2).GE. A*EPS*GAMMA*DSQRT(1.+CON1(1)**2
     1         + CON1(1)**2+CON1(2)**2)) GO TO 280
  270 IRAPID = 1
      GO TO 170
  280 CONTINUE
      IF (DSQRT(ETANM1(1)**2+ETANM1(2)**2) .GE. 1.E-06) GO TO 290
      IF (DSQRT(XYZ(1)**2+XYZ(2)**2)-1.01*DSQRT(ETANM1(1)**2+
     1    ETANM1(2)**2)) 290,290,270
C
C     EPSILON 2 TEST
C
  290 IF (IEP2 .EQ. 1) GO TO 310
      CALL CX TRN Y (DZ(II2),DZ(JJ2),CON1(1))
      CON2(1) = CON1(1)*LAM1(1) - CON1(2)*LAM1(2)
      CON1(2) = CON1(1)*LAM1(2) + CON1(2)*LAM1(1)
      CON1(1) = CON2(1)
      LAM2(1) = (CON1(1)*ETA(1) + CON1(2)*ETA(2))/(ETA(1)**2 +ETA(2)**2)
      LAM2(2) = (CON1(2)*ETA(1) - CON1(1)*ETA(2))/(ETA(1)**2 +ETA(2)**2)
      CON1(1) = LAM2(1) - LM2NM1(1)
      CON1(2) = LAM2(2) - LM2NM1(2)
      H2N(1)  = (CON1(1)*LAMBDA(1) + CON1(2)*LAMBDA(2))/(LAMBDA(1)**2 +
     1          LAMBDA(2)**2)
      H2N(2)  = (CON1(2)*LAMBDA(1) - CON1(1)*LAMBDA(2))/(LAMBDA(1)**2 +
     1          LAMBDA(2)**2)
      IF (ITER .LT. 4) GO TO 310
      IF (EP2 .GT. DSQRT(H2N(1)**2 + H2N(2)**2).AND.
     1   DSQRT(H2N(1)**2+H2N(2)**2) .GT. DSQRT(H2NM1(1)**2+H2NM1(2)**2))
     1   GO TO 300
      GO TO 310
  300 IEP2 = 1
      LAM2(1) = LM2NM1(1)
      LAM2(2) = LM2NM1(2)
  310 CON1(1) = 1. - (LAM2(1)*LAM1(1) + LAM2(2)*LAM1(2))/(LAM1(1)**2 +
     1          LAM1(2)**2)
      CON1(2) = (LAM2(2)*LAM1(1) - LAM2(1)*LAM1(2))/(LAM1(1)**2 +
     1          LAM1(2)**2)
      CON2(1) = CON1(1)*CON1(1) - CON1(2)*CON1(2)
      CON1(2) = 2.*CON1(2)*CON1(1)
      CON1(1) = CON2(1)
      CON1(1) = DMIN1(DSQRT(CON1(1)**2+CON1(2)**2),10.0D0)
      DELTA(1)= ETA(1)/CON1(1)
      DELTA(2)= ETA(2)/CON1(1)
C
      IF (IDIAG .EQ. 0) GO TO 330
      WRITE  (NOUT,320)LAM2,H2N,DELTA
  320 FORMAT (12H  LAMBDA =      ,2D15.5,     12H  H2N =     ,2D15.5,
     1        12H DELTA =      ,2D15.5)
  330 CONTINUE
C
C     VECTOR CONVERGENCE TEST
C
      IF (DSQRT(DELTA(1)**2+DELTA(2)**2).LE. (A*EPS)**2) GO TO 410
      IF (ITER .LE. 3) GO TO 170
C
C     EPSILON 1 TEST
C
      IF (IEPCNT .GE. 100) GO TO 520
      IF (IEPCNT .GE.  10) GO TO 340
      IF (DSQRT((LAM1(1)-LM1NM1(1))**2+(LAM1(2)-LM1NM1(2))**2)
     1   /DSQRT((LAMBDA(1) + DABS(LAM1(1)))**2+(LAMBDA(2) +DABS(LAM1(2))
     2    )**2) .GE. EP1) GO TO 170
      IEPCNT = 0
  340 CONTINUE
C
C     SHIFT DECISION
C
      CALL KLOCK (T2)
      TIMEIT = T2-T1
      IF (IDIAG .EQ. 0) GO TO 360
      WRITE  (NOUT,350) T2,T1,TIMEIT
  350 FORMAT (3I15)
  360 CONTINUE
      K = DLOG(DSQRT(DELTA(1)**2+DELTA(2)**2)/(A*EPS)**2)/DABS(DLOG(
     1    DSQRT(LAM1(1)**2+LAM1(2)**2)/DSQRT(LAM2(1)**2+LAM2(2)**2)))+1.
      K = K/2
      IF (IDIAG .EQ. 0) GO TO 380
      WRITE  (NOUT,370) K
  370 FORMAT (I5)
  380 CONTINUE
      IR1 = FLOAT(K-3)*FLOAT(TIMEIT)/FLOAT(ITER)
      IF (TIMED .GE. IR1) GO TO 170
      LAMBDA(1) = LAMBDA(1) + LAM1(1)
      LAMBDA(2) = LAMBDA(2) + LAM1(2)
C
C     STORE THE LAST VECTOR BEFORE A SHIFT FOR USE AS A STARTING VECTOR
C
      IF (SWITCH .EQ. 1) GO TO 390
      IN1 = II1
      GO TO 400
  390 IN1 = JJ5
  400 IFILE = FILEVC(1)
      CALL GOPEN (IFILE,Z(IOBUF),WRT)
      CALL WRITE (IFILE,DZ(IN1),NCOL4,1)
      IVECT  = 1
      COMFLG = 1
C
C     STORE  THE CURRENT VECTOR ON THE EIGENVECTOR FILE SO IT CAN BE
C     USED AS THE STARTING VECTOR
C
      CALL WRITE (IFILE,DZ(II1),NCOL4,1)
      CALL CLOSE (IFILE,EOFNRW)
      GO TO 490
C
C     M  RAPID CONVERGENCE MAKE SURE LAMD1 PASSES EP1 TEST
C
  410 CONTINUE
      IF (DSQRT((LAM1(1)-LM1NM1(1))**2+(LAM1(2)-LM1NM1(2))**2)
     1   /DSQRT((LAMBDA(1) + DABS(LAM1(1)))**2+(LAMBDA(2) +DABS(LAM1(2))
     2   )**2) .GE. EP1) GO TO 170
C
C     CONVERGENCE ACHIEVED, NORMALIZE THE VECTOR
C
C     STORE THE EIGENVECTOR AND EIGENVALUE ON THE OUTPUT FILES
C
      CALL CNORM1 (DZ(II1),NCOL)
      LAM1(1) = LAM1(1) + LAMBDA(1)
      LAM1(2) = LAM1(2) + LAMBDA(2)
      INU = II1 + NCOL2 - 1
      IF (IDIAG .EQ. 0) GO TO 430
      WRITE  (NOUT,420) LAM1,(DZ(I),I=II1,INU)
  420 FORMAT (1H1, 20H CONVERGENCE           ,//,' LAMBDA = ',2D15.5,
     1        //,(10D12.4))
  430 CONTINUE
      IFILE = FILEVC(1)
      CALL GOPEN (IFILE,Z(IOBUF),WRT)
      CALL WRITE (IFILE,DZ(II1),NCOL4,1)
      CALL CLOSE (IFILE,EOFNRW)
      IFILE = FILELM(1)
      CALL GOPEN (IFILE,Z(IOBUF),WRT)
      CALL WRITE (IFILE,LAM1(1),4,1)
      CALL CLOSE (IFILE,EOFNRW)
      NORTHO = NORTHO + 1
      NOROOT = NOROOT + 1
      IEP2   = 0
      IRAPID = 0
      NOCHNG = 0
      COMFLG = 0
      IF (SWITCH .EQ. 0) GO TO 440
      SWITCH = 0
      LAMBDA(1) = LMBDA(1)
      LAMBDA(2) = LMBDA(2)
      GO TO 450
  440 CONTINUE
      IVECT = 0
      IF (ITER .LE. 5) GO TO 460
  450 IN1   = JJ5
      IFILE = FILEVC(1)
      CALL GOPEN (IFILE,Z(IOBUF),WRT)
      CALL WRITE (IFILE,DZ(IN1),NCOL4,1)
      CALL CLOSE (IFILE,EOFNRW)
      IVECT = 1
  460 ITER  = 0
C
C     COMPUTE PSEUDO LEFT VECTOR
C
      CALL CM TIM U (DZ(II1),DZ(JJ3),0,Z(IOBUF))
      IF (FILEB(1) .EQ. 0) GO TO 470
      CALL CMTIMU (DZ(II1),DZ(JJ2),FILEB,Z(IBUF1))
      CON1(1) = 2.0D0*LAM1(1)
      CON1(2) = 2.0D0*LAM1(2)
      CON2(1) =-1.0D0
      CON2(2) = 0.0D0
      CALL CSUB (DZ(JJ3),DZ(JJ2),DZ(JJ3),CON1,CON2)
  470 IF (ISYM .EQ. 1) GO TO 480
C
C     LEFT = RIGHT FINISH JOB
C
      CALL CX TRN Y (DZ(II1),DZ(JJ3),CON1)
      CALL CDIVID (DZ(II1),DZ(JJ3),CON1,NCOL2)
C
C     PUT SCALED VECTOR ON LEFT VECTOR FILE
C
  480 IFILE = SCRFIL(10)
      CALL GOPEN (IFILE,Z(IBUF1),WRT)
      CALL WRITE (IFILE,DZ(JJ3),NCOL4,1)
      CALL CLOSE (IFILE,EOFNRW)
      LEFT = 1
      IF (ISYM .EQ. 0) GO TO 10
C
C 490 CALL CLOSE (FILEL,1)
C     CALL CLOSE (FILEU,1)
C     CALL CLOSE (FILEM,1)
  490 RETURN
C
C     RETURN TO MAIN DRIVER TO COMPUTE THE LEFT EIGENVECTOR
C
C
C     ENTRY POINT UPON RETURNING FROM OBTAINING THE LEFT VECTOR
C
  500 LEFT = 0
      IF (NODES  .LE. NOROOT) GO TO 550
      IF (NOROOT .GE.3*NOEST) GO TO 540
      AAA = DSQRT((LAMBDA(1) - LAM1(1))**2 + (LAMBDA(2)-LAM1(2))**2)
      IF (AAA .LE. RZERO) GO TO 570
      IF (IREG .EQ.    0) GO TO 510
      IF (IND) 510,510,530
  510 IF (NODES .LE. NOROOT) GO TO 550
      IF (LAM1(1)**2+LAM1(2)**2 .GE. MAXMOD) GO TO 560
C
C     GET NEW STARTING  POINT
C
  520 COMFLG = 0
      IND    =-IND
      GO TO 490
C
C     GENERATE NEW ARBITRARY STARTING VECTOR
C
  530 IND   =-(IND+1)
      IVECT = 0
      IF (IND .EQ. -13) IND = -1
      GO TO 50
C
C     3*NOEST FOUND
C
  540 COMFLG = 4
      GO TO 490
C
C     ALL ROOTS IN PROBLEM FOUND
C
C     COMFLG = 5
C     GO TO 176
C
C     NO. DES. ROOTS FOUND IN REGION OF CONVERGENCE OUTSIDE REGION
C
  550 COMFLG = 6
      GO TO 490
C
C     ONE OR MORE ROOTS OUTSIDE REGION
C
  560 COMFLG = 7
      GO TO 490
C
C     FOUND ROOT OUTSIDE REGION OF CURRENT START POINT
C
  570 IND  = IABS(IND)
      IREG = 1
      IF (EPS*RZERO/DSQRT((LAM1(1)-LAMBDA(1))**2+(LAM1(2)-LAMBDA(2))**2)
     1   .LT. EP3) GO TO 20
C
C     CURRENT SHIFT POINT IS TOO CLOSE TO AN EIGENVALUE
C
      IF (COMFLG .NE. 2) GO TO 580
      COMFLG = 9
      GOT O 490
  580 LAMBDA(1) = LAMBDA(1) + .02*RZERO
      LAMBDA(2) = LAMBDA(2) + .02*RZERO
      COMFLG = 2
      GO TO 490
C
C     ERROR EXITS
C
  600 J = -8
      GO TO 620
C 610 J = -1
  620 CALL MESAGE (J,IFILE,NAME)
      RETURN
      END
