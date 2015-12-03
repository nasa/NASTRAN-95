      SUBROUTINE TRHT1C (NGROUP,UDVT,PD,RDD,ILOOP)
C
C     THIS ROUTINE  STEPS INTEGRATION PROCEDURE
C
      INTEGER          SYSBUF,   UDVT,     PD,       RDD,      IZ(1),
     1                 A,        FILE,     MCB(7),   PNL1,     RADLIN,
     2                 NAME(2),  IFN(7),   ITAB(4),  LL1(7)
      DOUBLE PRECISION DZ(1)
      CHARACTER        UFM*23,   UWM*25
      COMMON /XMSSG /  UFM,      UWM
      COMMON /BLANK /  BETA,     TABS,     NORAD,    RADLIN,   SIGMA
      COMMON /SYSTEM/  KSYSTM(63)
      COMMON /TRDXX /  KTRDXX(28)
      COMMON /ZZZZZZ/  Z(1)
      COMMON /PACKX /  IT1,      IT2,      II,       JJ,       INCR
      COMMON /TRHTX /  IK(7),    IB(7),    ICR1,     ICR2,     ICR3,
     1                 ICR4,     ICR5,     ISYM,     A,        ICR7
      COMMON /TRDD1 /  NLFT1,    DIT1,     NLFTP1,   NOUT,     ICOUNT,
     1                 ILOOP1,   MODA1,    NZ,       ICORE,    IU1,
     2                 IN2,      IPNL(7),  NMODES,   NSTEP,    PNL1,
     3                 IST,      IU1DUM,   DELTAT,   IFRST,    TABS1,
     4                 SIGMA1,   TIM1
      COMMON /UNPAKX/  IT3,      III,      JJJ,      INCR1
      COMMON /INFBSX/  ILL1(7),  IUL1(7)
      COMMON /FBSX  /  LL1
      EQUIVALENCE      (KSYSTM(1),SYSBUF), (KSYSTM(55),IPREC),
     1                 (KTRDXX(28),IOPEN), (Z(1),IZ(1),DZ(1)),
     2                 (ILL1(3),MROW),     (KSYSTM(2),NPRT)
      DATA    NAME  /  4HTRHT,  4H1C     /
C
C     SYMBOL TABLE
C
C     ICR1 IS LLL
C     ICR2 IS ULL
C     ICR5 IS INITIAL CONDITIONS
C     ICR6 IS THE  A  MATRIX
C
C     NROW     PROBLEM ORDER
C     NGROUP   NUMBER OF TRIPLES OF TIME STEPS
C     UDVT     DISPLACEMENTS AND VELOCITIES
C     PD       LOADS
C     RDD      RADIATION MATRIX
C     ILOOP    CURRENT TIME STEP GROUP
C     IBUF1    UDVT BUFFER
C     IBUF2    A    BUFFER
C     IBUF3    LLL  BUFFER
C     IBUF4    ULL  BUFFER
C     IBUF5    PD   BUFFER
C     IBUF6    PNL1 BUFFER
C     IBUF7    RDD  BUFFER
C     IBUF8    SCRATCH BUFFER(DIT,NLLOADS,SAVE STUFF ETC)
C     NZ       OPEN CORE
C     IST      OUTPUT FLAG
C     IU1,IU2  DISPLACMENT VECTOR POINTERS
C     IP1,IP2  LOAD VECTOR POINTERS
C     IN1,IN2  NON-LINEAR LOAD POINTERS
C     NOLIN    =0  MEAN NO NON-LINEAR LOADS
C     IPNT     POINTER FOR INTERNAL ZERO ROUTINE
C     FILE     FILE    FOR INTERNAL ZERO ROUTINE
C     NSTEP    NUMBER OF TIME STEPS
C     DELTAT   DELTA  T
C     NOUT     OUTPUT INCREMENT
C     H        1/ 2*DELTAT
C     ICOUNT   STEP COUNTER
C     ITLEFT   TIME LEFT
C     NORAD    =-1  NO RADIATION
C     RADLIN   =-1  NON LINEAR RADIATION
C     NLFTP1   NONLINEAR SET SELECTED BY THE USER
C     BETA,OMBETA,OPBETA  --USER BETA 1-BETA, 1+BETA
C     ISYM     0    UNSYMETRIC   1  SYMMETRIC
C     DELTA1   OLD DELTA  T
C
      ISCR5  = ICR5
      NOLOAD = 0
      NBUST  = 0
      MCB(1) = PD
      CALL RDTRL (MCB)
      IF (MCB(1) .LE. 0) NOLOAD = -1
      NROW   = IK(2)
      IT1    = 1
      IT2    = 1
      II     = 1
      JJ     = NROW
      INCR   = 1
      IT3    = 1
      III    = 1
      JJJ    = NROW
      INCR1  = 1
      TABS1  = TABS
      SIGMA1 = SIGMA
      NZ     = KORSZ(Z)
      IGROUP = NZ - 3*NGROUP + 1
      IBUF1  = IGROUP- SYSBUF
      IBUF2  = IBUF1 - SYSBUF
      IBUF3  = IBUF2 - SYSBUF
      IBUF4  = IBUF3 - SYSBUF
      IBUF5  = IBUF4 - SYSBUF
      IBUF6  = IBUF5 - SYSBUF
      IBUF7  = IBUF6 - SYSBUF
      IBUF8  = IBUF7 - SYSBUF
      NZ     = IBUF8 - 1
      ILOOP1 = ILOOP
      IST    = 0
      ILL1(1)= ICR1
      CALL RDTRL (ILL1)
      IFN(1) = ICR1
      CALL RDTRL (IFN)
      IU1    = 0
      IU2    = IU1 + NROW
      IP1    = IU2 + NROW
      IP2    = IP1 + NROW
      IUK    = IP2 + NROW
      NOLIN  = 0
      IF (NLFTP1.NE.0 .OR. NORAD.NE.-1) NOLIN = 1
      IF (NOLIN .EQ. 0) GO TO 10
      IN1    = IUK + NROW
      IN2    = IN1 + NROW
      NZ     = NZ - 7*NROW
      GO TO 20
C
C     NO NON-LINEAR EFFECTS
C
   10 NZ     = NZ - 4*NROW
      IN2    = IP2
   20 IF (NZ .LT. 0) CALL MESAGE (-8,0,NAME)
      ICORE  = IN2 + NROW
      IUL1(1)= ICR2
      CALL RDTRL (IUL1)
      OMBETA = 1.0 - BETA
      OPBETA = 1.0 + BETA
C
C     SET UP FOR CORE I/O
C
      IF (NLFTP1 .EQ. 0) GO TO 21
      IFRST  = 0
      CALL TRD1D
      IFRST  = 1
   21 ITAB(1) = A
      ITAB(2) = ILL1(1)
      ITAB(3) = IUL1(1)
      ITAB(4) = RDD
      ICOR = IN2 + NROW + 1
      NF   = 4
      CALL GOPEN (A,IZ(IBUF2),0)
      CALL REWIND (A)
      IF (NOLIN.EQ.0 .OR. RADLIN.NE.-1 .OR. NORAD.EQ.-1) GO TO 30
      CALL GOPEN (RDD,IZ(IBUF7),0)
      CALL REWIND (RDD)
   30 CONTINUE
      CALL GOPEN (ILL1,IZ(IBUF3),0)
      CALL REWIND (ILL1)
      IF (ISYM .EQ. 1) GO TO 31
      CALL GOPEN (IUL1,IZ(IBUF4),0)
      CALL REWIND (IUL1)
   31 CONTINUE
C
C     IS  THIS  A TIME  STEP CHANGE
C
      IF (ILOOP  .NE. 1) GO TO  280
      IF (NOLOAD .NE. 0) GO TO 33
      CALL GOPEN (PD,IZ(IBUF5),0)
      CALL FWDREC (*440,PD)
   33 CONTINUE
      IST = -1
      CALL GOPEN (ICR5,IZ(IBUF1),0)
C
      CALL FREAD(ICR5,IZ(IGROUP),3*NGROUP,1)
C
C     BRING IN  U0 AND UK
C
      CALL READ (*450,*35,ICR5,Z(IU1+1),NROW,1,NWDS)
      GO TO 40
C
C     SHORT VECTOR ENCOUNTERED
C
   35 K = NWDS + 1
      DO 38 L = K,NROW
      M = IU1 + L
      Z(M) = 0.0
   38 CONTINUE
   40 CONTINUE
      IF (NORAD .EQ. -1) GO TO 50
      CALL READ (*450,*45,ICR5,Z(IUK+1),NROW,1,NWDS)
      GO TO 410
C
C     SHORT VECTOR ENCOUNTERED
C
   45 K = NWDS + 1
      DO 48 L = K,NROW
      M = IUK + L
      Z(M) = 0.0
   48 CONTINUE
      GO TO 410
   50 CONTINUE
      CALL CLOSE (ICR5,1)
      NSTEP  = IZ(IGROUP) + 1
      DELTAT =  Z(IGROUP+1)
      NOUT   = IZ(IGROUP+2)
      H = 1.0/DELTAT
      CALL GOPEN (UDVT,IZ(IBUF1),1)
      CALL MAKMCB (MCB,UDVT,NROW,2,1)
      IF (NOLIN .EQ. 0) GO TO 60
      CALL GOPEN (PNL1,IZ(IBUF6),1)
      CALL MAKMCB (IPNL,PNL1,NROW,2,1)
C
C     LETS  GO
C
   60 ICOUNT = 1
C
C     TOP OF LOOP
C
   70 CALL TMTOGO (ITLEFT)
      IF (ITLEFT .LE. 0) GO TO 230
C
C     COMPUTE  NR
C
      IF (NORAD  .EQ. -1) GO TO 110
      IF (RADLIN .EQ. -1) GO TO 90
      DO 80 I = 1,NROW
      L = IN2 + I
      K = IUK + I
      Z(L) = Z(K)
   80 CONTINUE
      GO TO 130
C
C     NON-CONSTANT RADIATION
C
   90 DO 100 I = 1,NROW
      L = IU1 + I
      K = IUK + I
      M = IN2 + I
      J = IU2 + I
C
C     CHECK FOR UNSTABLE SOLUTION ABOUT TO CAUSE ARITHMETIC OVERFLOWS.
C
      IF (Z(L) .LT. 1.0E8) GO TO 98
      NBUST = NBUST + 1
      IF (NBUST .GT. 10) GO TO 94
      WRITE  (NPRT,92) UWM,Z(L),ICOUNT,I
   92 FORMAT (A25,' 3102, SUBROUTINE TRHT1C, UNSTABLE TEMP. VALUE OF',
     1       E20.8,' COMPUTED FOR TIME STEP',I5, /5X,
     2       'AT POINT NUMBER',I6,' IN THE ANALYSIS SET.')
      Z(L) = 1.0E6
      GO TO 98
   94 WRITE  (NPRT,96) UFM
   96 FORMAT (A23,' 3103, SUBROUTINE TRHT1C TERMINATING DUE TO ERROR ',
     1       'COUNT FOR MESSAGE 3102.')
      CALL MESAGE (-61,0,NAME)
C
   98 Z(J) = -(Z(L)+TABS)**4 + 4.0*(Z(K)+TABS)**3*Z(L)
      Z(M) = 0.0
  100 CONTINUE
      IOPEN  = 1
      IFN(1) = RDD
      CALL MATVEC (Z(IU2+1),Z(IN2+1),IFN,IZ(IBUF7))
      GO TO 130
  110 IF (NLFTP1 .EQ. 0) GO TO 140
      DO 120 I = 1,NROW
      M = IN2 + I
      Z(M) = 0.0
  120 CONTINUE
  130 IF (NLFTP1 .EQ. 0) GO TO 140
      TIM1 = TIM
      CALL TRD1D
  140 IF (ICOUNT .NE. 1 .OR. ILOOP .NE. 1) GO TO 160
      DO 150 I = 1,NROW
      K = IP1 + I
      Z(K) = 0.0
      IF (NOLIN .EQ. 0) GO TO 150
      L = IN2 + I
      M = IN1 + I
      Z(M) =  Z(L)
      Z(K) = -Z(L)
  150 CONTINUE
      IOPEN = 0
      CALL MATVEC (Z(IU1+1),Z(IP1+1),IK,Z(IBUF8))
C
C     BRING IN  NEXT P
C
  160 IF (NOLOAD .NE. 0) GO TO 165
      CALL UNPACK (*165,PD,Z(IP2+1))
      GO TO 170
  165 DO 167 I = 1,NROW
      K = IP2 + I
      Z(K) = 0.0
  167 CONTINUE
C
C     ADD ALL LOAD CONTRIBUTIONS
C
  170 CONTINUE
      DO 180 I = 1,NROW
      L = IP1 + I
      M = IP2 + I
      Z(L) = OMBETA*Z(L) + BETA*Z(M)
      IF (NOLIN .EQ. 0) GO TO 180
      M = IN1 + I
      J = IN2 + I
      Z(L) = Z(L) + OPBETA*Z(J) - BETA*Z(M)
  180 CONTINUE
C
C     MULTIPLY  IN  A MATRIX
C
      IOPEN  = 1
      IFN(1) = A
      CALL MATVEC (Z(IU1+1),Z(IP1+1),IFN,IZ(IBUF2))
C
C     SOLVE  FOR NEXT DISPLACEMENT
C
      IOPEN = 1
      IF (ISYM .EQ. 0) CALL INTFBS (Z(IP1+1),Z(IU2+1),IZ(IBUF4))
      IF (ISYM .NE. 1) GO TO 188
C
C     ABSORBED SUBROUTINE FBSINT   SEE ALSO EQUIV.   DATA.
C
      DO 182 I = 1,MROW
      Z(I+IU2) = Z(I+IP1)
  182 CONTINUE
C
C     FORWARD PASS
C
      CALL REWIND (ILL1)
      CALL FWDREC (*186,ILL1)
      IZ(IBUF4) = ILL1(1)
      LL1(1) = ILL1(1)
      CALL RDTRL (LL1)
      IF (IPREC .NE. 1) GO TO 184
      CALL FBS1 (IZ(IBUF4),Z(IU2+1),Z(IU2+1),MROW)
      GO TO 188
  184 CALL FBS21(IZ(IBUF4),Z(IU2+1),Z(IU2+1),MROW)
      GO TO 188
  186 CALL MESAGE (-2,ILL1,NAME)
C
C     ABSORBED SUBROUTINE FBSINT    SEE ALSO EQUIV.   DATA.
C
  188 CONTINUE
      IF (ICOUNT.EQ.1 .OR. ICOUNT.EQ.NSTEP .OR.
     1    MOD(ICOUNT+IST,NOUT).EQ.0) GO TO 200
C
C     ROTATE POINTERS
C
  190 J   = IP1
      IP1 = IP2
      IP2 = J
      J   = IU1
      IU1 = IU2
      IU2 = J
      J   = IN1
      IN1 = IN2
      IN2 = J
      TIM = TIM + DELTAT
      ICOUNT = ICOUNT + 1
      IF (ICOUNT-NSTEP) 70,220,230
C
C     IT  IS OUTPUT TIME
C
  200 CALL PACK (Z(IU1+1),UDVT,MCB)
C
C     COMPUTE  U DOT
C
      DO 210 I = 1,NROW
      L = IP1 + I
      M = IU1 + I
      J = IU2 + I
      Z(L) = (Z(J)-Z(M))*H
  210 CONTINUE
      CALL PACK (Z(IP1+1),UDVT,MCB)
C
C     PUT OUT ZERO ACCERERATION VECTOR FOR LATER MODULES
C
      CALL BLDPK (1,1,UDVT,0,0)
      CALL BLDPKN (UDVT,0,MCB)
      IF (NOLIN .EQ. 0) GO TO 190
      CALL PACK (Z(IN2+1),PNL1,IPNL)
      GO TO 190
C
C     END OF 1 GROUP
C
  220 IF (ILOOP .NE. NGROUP) GO TO 260
      GO TO 70
  230 J = 1
  240 CALL CLOSE (UDVT,J)
      CALL CLOSE (PD,J)
      CALL CLOSE (ILL1,1)
      CALL CLOSE (IUL1,1)
      CALL CLOSE (A,1)
      CALL WRTTRL (MCB)
      IF (NORAD .EQ. -1) GO TO 245
      CALL CLOSE (RDD,1)
  245 IF (NOLIN .EQ. 0) GO TO 250
      CALL CLOSE (PNL1,J)
      CALL WRTTRL (IPNL)
  250 CONTINUE
      RETURN
C
C     MORE GROUPS TO COME  SAVE STUFF
C
  260 J = 2
      CALL GOPEN (ISCR5,IZ(IBUF8),1)
      CALL WRITE (ISCR5,IZ(IGROUP),3*NGROUP,1)
      IF (NOLIN .NE. 0) CALL WRITE (ISCR5,IZ(IUK+1),NROW,1)
C
C     SAVE   UI -1
C
      CALL WRITE (ISCR5,Z(IU2+1),NROW,1)
C
C     SAVE   UI
C
      CALL WRITE (ISCR5,Z(IU1+1),NROW,1)
      IF (NOLIN .EQ. 0) GO TO 270
C
C     SAVE    NI - 1
C
      CALL WRITE (ISCR5,Z(IN2+1),NROW,1)
C
C     SAVE    NI
C
      CALL WRITE (ISCR5,Z(IN1+1),NROW,1)
  270 CONTINUE
      CALL CLOSE (ISCR5,1)
      GO TO 240
C
C     REENTRY FROM CHANGE OF TIME STEP
C
  280 CONTINUE
      CALL GOPEN (ISCR5,IZ(IBUF8),0)
      CALL FREAD (ISCR5,IZ(IGROUP),3*NGROUP,1)
      NEWGRP = IGROUP + (ILOOP-1)*3
      DELTA1 =  Z(NEWGRP-2)
      NSTEP  = IZ(NEWGRP)
      DELTAT = Z(NEWGRP+1)
      NOUT   = IZ(NEWGRP+2)
      CALL GOPEN (PD,IZ(IBUF5),2)
      H = 1.0/DELTAT
      CALL GOPEN (UDVT,IZ(IBUF1),3)
      MCB(1) = UDVT
      CALL RDTRL (MCB(1))
      IF (NOLIN .EQ. 0) GO TO 290
      CALL GOPEN (PNL1,IZ(IBUF6),3)
      IPNL(1) = PNL1
      CALL RDTRL (IPNL)
  290 CONTINUE
C
C     RESTORE  STUFF  SAVED
C
      IF (NOLIN .NE. 0) CALL FREAD (ISCR5,Z(IUK+1),NROW,1)
      CALL FREAD (ISCR5,Z(IU2+1),NROW,1)
      CALL FREAD (ISCR5,Z(IU1+1),NROW,1)
      IF (NOLIN .EQ. 0) GO TO 300
      CALL FREAD (ISCR5,Z(IN1+1),NROW,1)
      CALL FREAD (ISCR5,Z(IN2+1),NROW,1)
  300 CONTINUE
      CALL CLOSE (ISCR5,1)
C
C     COMPUTE  PBAR
C
      DO 310 I = 1,NROW
      L = IP1 + I
      Z(L) = 0.0
      IF (NOLIN .EQ. 0) GO TO 310
      M    = IN2 + I
      Z(L) =-Z(M)
  310 CONTINUE
      IOPEN = 0
      CALL MATVEC (Z(IU1+1),Z(IP1+1),IK,IZ(IBUF8))
      IF (IB(1) .EQ. 0) GO TO 330
      DO 320  I = 1,NROW
      L = IU2 + I
      M = IU1 + I
      Z(L) = (Z(M)-Z(L))/DELTA1
  320 CONTINUE
      IOPEN = 0
      CALL MATVEC (Z(IU2+1),Z(IP1+1),IB,IZ(IBUF8))
  330 CONTINUE
      IF (NOLIN .EQ. 0) GO TO 350
      H1 = 1.0 - DELTAT/DELTA1
      H2 = DELTAT/DELTA1
      DO 340 I = 1,NROW
      L  = IN1 + I
      M  = IN2 + I
      Z(L) = H2*Z(L) + H1*Z(M)
  340 CONTINUE
  350 ICOUNT = 0
      GO TO 70
C
C     CONSTANT RADIATION
C
  410 IF(RADLIN .EQ. -1) GO TO 50
      DO 420 I = 1,NROW
      L = IUK + I
      K = IN2 +I
      Z(L) = -(Z(L)+TABS)**4 + 4.0*(Z(L)+TABS)**3*Z(L)
      Z(K) = 0.0
  420 CONTINUE
      IOPEN = 1
      IFN(1) = RDD
      CALL MATVEC (Z(IUK+1),Z(IN2+1),IFN, IZ(IBUF7))
      DO 430 I = 1,NROW
      L = IUK + I
      M = IN2 + I
      Z(L) = Z(M)
  430 CONTINUE
      GO TO 50
C
C     I/O ERROR
C
  440 FILE = PD
      GO TO 460
  450 FILE = ICR5
  460 CALL MESAGE (-2,FILE,NAME)
      RETURN
      END
