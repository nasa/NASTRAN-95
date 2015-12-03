      SUBROUTINE FRSW2 (V1,V2,V3,VB)
C
C     LAST REVISED  11/91, BY G.CHAN/UNISYS
C     ADDITION OF A NEW FORWARD-BACKWARD SUBSTITUTION METHOD, WHICH IS
C     MORE EFFICIENT, AND IS ALREADY GOOD FOR VECTORIZATION.
C
CDB   LOGICAL          DEBUG
      INTEGER          NAM(6)  ,LJJ(2) ,IBLK(15),BASE
      DOUBLE PRECISION V1(1)   ,V2(1)  ,V3(1)   ,VB(1) ,XL(1) ,XLJJ  ,
     1                 V3J     ,ZERO   ,SUM
      COMMON  /OPINV / MCBLT(7),MCBSMA(7)
      COMMON  /SYSTEM/ KSYSTM  ,IO
      COMMON  /FEERXX/ DUMM(18),NZVB
      COMMON  /ZZZZZZ/ IZ(1)
      EQUIVALENCE      (XL(1),IZ(1))
      EQUIVALENCE      (LJJ(1) ,XLJJ)  ,(L16,DUMM(6))
      DATA     NAM   / 4HFRSW  ,4H2    ,2*4HBEGN,4HEND ,4HBGIN /
      DATA     ZERO  / 0.0D+0        /
CDB   DATA     DEBUG , ITER    ,MAX  / .FALSE.  ,0     ,3      /
C
CDB   IF (.NOT.DEBUG) GO TO 20
C     ITER = ITER + 1
C     IF (ITER .GT. MAX) DEBUG = .FALSE.
C     WRITE  (IO,10) NZVB,ITER
C  10 FORMAT ('  .... IN FRSW2.  NZVB =',I8,',   ITER =',I3)
C  20 CONTINUE
      NROW = MCBLT(2)
      CALL FRMLTD (MCBSMA(1),V1(1),V3(1),VB(1))
      IF (MCBLT(7) .LT. 0) GO TO 200
C
C     NASTRAN ORIGINAL METHOD
C
      IBLK( 1) = MCBLT(1)
      IBLK( 9) = 1
      IBLK(10) = 1
      CALL REWIND (MCBLT)
      CALL SKPREC (MCBLT,1)
C
C     FORWARD SWEEP DIRECTLY ON V3
C
      DO 80 J = 1,NROW
      IBLK(8) = -1
   30 CALL GETSTR (*70,IBLK(1))
      JI   = IBLK(5)
      NTMS = IBLK(6)
      IK   = IBLK(4)
      IF (IK .NE. J) GO TO 40
      NTMS = NTMS - 1
      XLJJ = XL(JI)
      JI   = JI + 1
      IK   = IK + 1
   40 IF (NTMS .EQ. 0) GO TO 60
      V3J  = V3(J)
      IF (V3J .EQ. ZERO) GO TO 60
      DO 50 II = 1,NTMS
      V3(IK) = V3(IK) + XL(JI)*V3J
      IK   = IK + 1
      JI   = JI + 1
   50 CONTINUE
   60 CALL ENDGET (IBLK(1))
      GO TO 30
   70 V3(J)= V3(J)/XLJJ
   80 CONTINUE
C
C     BACKWARD SUBSTITUTION OMIT DIAGONAL
C
      IF (NROW .EQ. 1) GO TO 500
      J    = NROW
   90 IBLK(8) = -1
  100 CALL GETSTB (*130,IBLK(1))
      NTMS = IBLK(6)
      JI   = IBLK(5)
      IK   = IBLK(4)
      IF (IK-NTMS+1 .EQ. J) NTMS = NTMS - 1
      IF (NTMS .EQ. 0) GO TO 120
      SUM  = ZERO
      DO 110 II = 1,NTMS
      SUM  = SUM + XL(JI)*V3(IK)
      JI   = JI - 1
      IK   = IK - 1
  110 CONTINUE
      V3(J)= V3(J) + SUM
  120 CALL ENDGTB (IBLK(1))
      GO TO 100
  130 IF (J .EQ. 1) GO TO 500
      J    = J - 1
      GO TO 90
C
C     NEW METHOD
C
C     THE MCBLT MATRIX HAS BEEN RE-WRITTEN FORWARD FIRST THAN BACKWARD
C     BY UNPSCR IN FEER3. NO STRING OPERATION HERE
C
  200 IF (NAM(3) .EQ. NAM(5)) NAM(3) = NAM(6)
      IF (L16 .NE. 0) CALL CONMSG (NAM,3,0)
      MCBLTX =-MCBLT(7)
      IF (MOD(MCBLT(4),10) .NE. 2) GO TO 440
      NREC = 0
      CALL REWIND (MCBLTX)
      CALL FWDREC (*400,MCBLTX)
      NWDS = MCBLT(5)
C
C     IZ(1)                                                     GINO
C      / V1   V2    V3          VB (OPEN CORE LENGTH = NZVB)   BUFFERS
C     +-----+-----+-----+-----+-------------------------------+--------
C                         OPEN  CORE
C
C     FORWARD SWEEP DIRECTLY ON V3
C
      LL2  = 0
      BASE = 1
      IFB  = +450
      DO 270 J = 1,NROW
      IF (BASE .LT. LL2) GO TO 240
      NREC = NREC + 1
CDB   IF (DEBUG) WRITE (IO,210) NREC,IFB
C 210 FORMAT ('  ...READING RECORD',I5,'.   IFB =',I5)
      CALL READ (*400,*220,MCBLTX,VB,NZVB,1,LL)
      CALL MESAGE (-8,0,NAM)
  220 LL2  = LL/NWDS
CDB   LL3  = LL2/30
C     LL4  = LL2 - LL3
C     IF (DEBUG) WRITE (IO,230) LL,NREC,LL2
C 230 FORMAT (5X,I10,' WORDS READ FROM RECORD',I5,'.   LL2 =',I8)
      BASE = 1
  240 XLJJ = VB(BASE)
      II   = LJJ(1)
      JJ   = LJJ(2)
CDB   IF (DEBUG .AND. (BASE.LT.LL3 .OR. BASE.GT.LL4))
C    1    WRITE (IO,250) J,BASE,II,JJ,IFB
C 250 FORMAT (11X,'J,BASE,II,JJ,IFB =',5I8)
      IF (II .NE. J) GO TO 420
      NTMS = JJ - II + 1
      IB   = BASE + 2
      IE   = BASE + NTMS
      BASE = IE + 1
      IF (NTMS .LE. 1) GO TO 270
      V3J  = V3(J)
      IF (V3J .EQ. ZERO) GO TO 270
      DO 260 I = IB,IE
      II   = II + 1
  260 V3(II) = V3(II) + VB(I)*V3J
  270 V3(J)= V3(J)/VB(IB-1)
C
C     BACKWARD SUBSTITUTION OMIT DIAGONAL
C
      IF (NROW .EQ. 1) GO TO 500
      NREC = 0
      LL2  = 0
      BASE = 1
      J    = NROW
      IFB  = -490
      DO 310 JX = 1,NROW
      IF (BASE .LT. LL2) GO TO 290
      NREC = NREC + 1
CDB   IF (DEBUG) WRITE (IO,210) NREC,IFB
      CALL READ (*400,*280,MCBLTX,VB,NZVB,1,LL)
      CALL MESAGE (-8,0,NAM)
  280 LL2  = LL/NWDS
CDB   LL3  = LL2/30
C     LL4  = LL2 - LL3
C     IF (DEBUG) WRITE (IO,230) LL,NREC,LL2
      BASE = 1
  290 XLJJ = VB(BASE)
      II   = LJJ(1)
      JJ   = LJJ(2)
CDB   IF (DEBUG .AND. (BASE.LT.LL3 .OR. BASE.GT.LL4))
C    1    WRITE (IO,250) J,BASE,II,JJ,IFB
      IF (II .NE. J) GO TO 420
      NTMS = JJ - II + 1
      IB   = BASE + 2
      IE   = BASE + NTMS
      BASE = IE + 1
      IF (NTMS .LE. 1) GO TO 310
      SUM  = ZERO
      DO 300 I = IB,IE
      II   = II + 1
  300 SUM  = SUM + VB(I)*V3(II)
      V3(J)= V3(J) + SUM
  310 J    = J - 1
      GO TO 500
C
C     ERROR
C
  400 I = MCBLT(4)/10
      WRITE  (IO,410) NREC,J,I,IFB
  410 FORMAT ('0*** TRY TO READ RECORD',I5,'.  J,MCBLT(4),IFB =',I7,2I5)
      CALL MESAGE (-2,MCBLTX,NAM)
  420 WRITE  (IO,430) IFB,II,J
  430 FORMAT ('0*** ERROR.   IFB),II,J =',I5,1H),2I8)
      GO TO 460
  440 J = MOD(MCBLT(4),10)
      WRITE  (IO,450) J
  450 FORMAT ('0*** MCBLT MATRIX IN WRONG FORM.  UNPSCR FLAG =',I3)
  460 CALL MESAGE (-37,0,NAM)
C
  500 NAM(3) = NAM(5)
      IF (L16 .NE. 0) CALL CONMSG (NAM,3,0)
      RETURN
      END
