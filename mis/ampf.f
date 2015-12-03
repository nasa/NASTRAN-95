      SUBROUTINE AMPF (SKJ,GKH,AJJL,QHJL,PLAN,IMAX,SCR1,SCR2,SCR3,SCR4,
     1                 SCR5,SCR6,SCR7,SCR8,SCR9,SCR10)
C
C     THE PURPOSE OF THIS ROUTINE IS TO SOLVE FOR QHJL
C
C     THE STEPS ARE AS FOLLOWS
C
C       I.  FOR EACH M-K PAIR
C
C           A. FIND SKJ FROM SKJ LIST
C                                      T
C           B.  COMPUTE  S(K) =  SKJ(K) *GKH
C
C           C.  FOR EACH GROUP
C                                                  G
C               1. BREAK  S(K) INTO GROUPS  =  S(K)
C
C               2. SOLVE FOR  RJH
C                                                        -1     G
C                      D.L. AND D.L. WITH BODIES RGH= AJJ  *S(K)
C                                                        T      G
C                      OTHERS                    RGH= AJJ  *S(K)
C
C               3. MERGE RESULTS
C
C                  1    G11
C                  1 RJH  1
C                  1------1  =   RJH(K)
C                  1    G21
C                  1 RJH  1
C                  1      1
C
C
C           D.  APPEND  RJH ONTO GROWING  QHJL
C                1       1       1
C                1RJH(K1)1RJH(K2)1  =  QHJL
C                1       1       1
C                1       1       1
C
      INTEGER         SKJ,GKH,AJJL,QHJL,PLAN,MCB(7),SYSBUF,NAME(2),
     1                AJJCOL,SCR1,SCR2,SCR3,SCR4,SCR5,SCR6,SCR7,SCR8,
     2                SCR9,SCR10,RJH
      COMMON /AMPCOM/ NCOLJ,NSUB,XM,XK,AJJCOL,QHHCOL,NGP,NGPD(2,30),
     1                MCBQHH(7),MCBQJH(7),NOH,IDJH
      COMMON /SYSTEM/ SYSBUF,NOUT,SKP(52),IPREC
      COMMON /ZZZZZZ/ Z(1)
      COMMON /CDCMPX/ DUM32(32),IB
      COMMON /UNPAKX/ ITC,II,JJ,INCR
      COMMON /PACKX / ITC1,ITC2,II1,JJ1,INCR1
      DATA    NAME  / 4HAMPF,1H /
C
C     INITIALIZE
C
      IBUF1 = KORSZ(Z) - SYSBUF + 1
      IBUF2 = IBUF1 - SYSBUF
      IOP   = 0
      ITL   = 0
      DO 9000 ILOOP = 1,IMAX
      CALL KLOCK (ITS)
      CALL GOPEN (PLAN,Z(IBUF1),IOP)
      IOP = 2
      CALL FREAD (PLAN,XM,4,1)
      CALL CLOSE (PLAN,2)
C
C     FIND  SKJ(K) IN SKJL
C
      CALL GOPEN (SKJ,Z(IBUF1),0)
      CALL GOPEN (SCR1,Z(IBUF2),1)
      K = AJJCOL - 1
      CALL SKPREC (SKJ,K)
      MCB(1) = SKJ
      CALL RDTRL (MCB)
      CALL MAKMCB (MCB,SCR1,MCB(3),MCB(4),MCB(5))
      INCR = 1
      ITC  = MCB(5)
      CALL CYCT2B (SKJ,SCR1,NCOLJ,Z,MCB)
      CALL CLOSE  (SKJ,1)
      CALL CLOSE  (SCR1,1)
      CALL WRTTRL (MCB)
C                     T
C     MULTIPLY  SKJ(K) *GKH  ONTO SCR2
C
      CALL SSG2B (SCR1,GKH,0,SCR2,1,IPREC,1,SCR3)
C
C     POSITION AJJL
C
      CALL GOPEN (AJJL,Z(IBUF1),0)
      K = AJJCOL - 1
      CALL SKPREC (AJJL,K)
      CALL CLOSE  (AJJL,2)
C
C     SET UP TO LOOP ON CONSTANT THEORY
C
      NGPS = 1
      NTH  = NGPD(1,NGPS)
      NCOLTH = 0
  135 NCLOLD = NCOLTH + 1
  140 IF (NGPS .GT. NGP) GO TO 150
      IF (NGPD(1,NGPS) .NE. NTH) GO TO 150
      NCOLTH = NCOLTH + NGPD(2,NGPS)
      NGPS   = NGPS + 1
      GO TO 140
  150 CONTINUE
      IONCE = 0
      IF (NCLOLD.EQ.1 .AND. NGPS.GT.NGP) IONCE = 1
C                                 G
C     COPY AJJL(K) TO SCR1 (AJJ(K) )
C
      CALL GOPEN (AJJL,Z(IBUF1),2)
      CALL GOPEN (SCR1,Z(IBUF2),1)
      MCB(1) = AJJL
      CALL RDTRL (MCB)
      CALL MAKMCB (MCB,SCR1,NCOLTH,MCB(4),MCB(5))
      II   = NCLOLD
      JJ   = NCOLTH
      II1  = 1
      JJ1  = NCOLTH - NCLOLD + 1
      ITC  = MCB(5)
      ITC1 = ITC
      ITC2 = ITC
      INCR = 1
      INCR1= 1
      CALL AMPC1 (AJJL,SCR1,NCOLTH,Z,MCB)
      CALL CLOSE (AJJL,2)
      CALL CLOSE (SCR1,1)
      CALL WRTTRL (MCB)
C                                   G
C     COPY SKJ(K)  ONTO SCR3 (SKJ(K) )
C
      CALL GOPEN (SCR2,Z(IBUF1),0)
      CALL GOPEN (SCR3,Z(IBUF2),1)
      MCB(1) = SCR2
      CALL RDTRL (MCB)
      CALL MAKMCB (MCB,SCR3,NCOLTH,MCB(4),MCB(5))
      ITC  = MCB(5)
      ITC1 = ITC
      ITC2 = ITC
      CALL AMPC1 (SCR2,SCR3,NOH,Z,MCB)
      CALL CLOSE (SCR2,1)
      CALL CLOSE (SCR3,1)
      CALL WRTTRL (MCB)
      RJH = SCR10
      IF (IONCE .NE. 0) RJH = SCR9
C
C     BRANCH ON THEORY
C
      GO TO (1000,2000,3000,4000,5000), NTH
C
C     DOUBLET LATTICE--D.L. WITH SLENDER BODIES
C
 1000 CONTINUE
 2000 CONTINUE
C                     G
C     DECOMPOSE AJJ(K)
C
      IB = 0
      CALL CFACTR (SCR1,SCR4,SCR5,SCR6,SCR7,SCR8,IOPT)
      CALL CFBSOR (SCR4,SCR5,SCR3,RJH,IOPT)
      GO TO 1020
C
C     OTHER THEORIES
C
 3000 CONTINUE
 4000 CONTINUE
 5000 CONTINUE
      CALL SSG2B (SCR1,SCR3,0,RJH,1,IPREC,1,SCR4)
C
C     COPY ACCUMULATIVELY ONTO RJH(K)
C
 1020 IF (IONCE .NE. 0) GO TO 8000
      CALL AMPC2 (RJH,SCR9,SCR1)
      IF (NGPS .GT. NGP) GO TO 8000
      GO TO 135
C
C     ALL GROUPS /THEORIES COMPLETE
C
 8000 CONTINUE
C
C     COPY ONTO  QHJL
C
      CALL GOPEN (SCR9,Z(IBUF1),0)
      CALL GOPEN (QHJL,Z(IBUF2),3)
      MCB(1) = QHJL
      CALL RDTRL (MCB(1))
      ITC  = MCB(5)
      INCR = 1
      CALL CYCT2B (SCR9,QHJL,NOH,Z,MCB)
      CALL CLOSE  (QHJL,2)
      CALL CLOSE  (SCR9,1)
      CALL WRTTRL (MCB)
C
C     END LOOP ON M-K PAIRS
C
      IF (ILOOP .EQ. IMAX) GO TO 9000
C
C     CHECK TIME
C
      CALL KLOCK (ITF)
      CALL TMTOGO (ITMTO)
      ITL= MAX0(ITF-ITS,1,ITL)
      IF (1.1*ITL .GE. ITMTO) GO TO 9010
 9000 CONTINUE
      RETURN
C
C     INSUFFICIENT TIME TO COMPLETE
C
 9010 CALL MESAGE (45,IMAX-ILOOP,NAME)
      RETURN
      END
