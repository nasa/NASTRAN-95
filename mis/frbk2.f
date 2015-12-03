      SUBROUTINE FRBK2 (V1,V2,V3,VB)
C
C     LAST REVISED BY G.CHAN/UNISYS  11/1991
C     . ELIMINATE UN-NECCESSARY REWIND AND SKIP AFTER FIRST CALL TO THIS
C       ROUTINE (NASTRAN ORIGINAL METHOD)
C     . ADDITION OF A NEW BACKWARD-FORWARD SUBSTITUTION METHOD WHICH IS
C       MORE EFFICIENT, AND IS ALREADY GOOD FOR VECTORIZATION
C
CDB   LOGICAL          DEBUG
      INTEGER          BASE    ,BUF(6) ,LJJ(2)  ,IBLK(15)
      DOUBLE PRECISION V1(1)   ,V2(1)  ,V3(1)   ,VB(1)  ,XL(1)  ,XLJJ ,
     1                 ZERO    ,V3J    ,SUM
      COMMON  /OPINV / MCBLT(7),MCBSMA(7)
      COMMON  /SYSTEM/ KSYSTM  ,IO
      COMMON  /FEERXX/ DUMM(18),NZVB
      COMMON  /ZZZZZZ/ IZ(1)
      EQUIVALENCE      (XL(1),IZ(1))
      EQUIVALENCE      (LJJ(1) ,XLJJ)  ,(L16,DUMM(6))
      DATA     BUF   / 4HFRBK  ,4H2    ,2*4HBEGN ,4HEND  ,4HBGIN /
      DATA     ZERO  / 0.0D+0       /
CDB   DATA     DEBUG , ITER    ,MAX /  .FALSE.   ,0      ,3      /
C
CDB   IF (.NOT.DEBUG) GO TO 20
C     ITER = ITER + 1
C     IF (ITER .GT. MAX) DEBUG = .FALSE.
C     WRITE  (IO,10) NZVB,ITER
C  10 FORMAT ('  .... IN FRBK2.  NZVB =',I8,',  ITER =',I3)
C  20 CONTINUE
      NROW = MCBLT(2)
      DO 30 I = 1,NROW
   30 V2(I) = V1(I)
C
C     SELECTION OF ORIGINAL OR NEW FBS METHOD
C
      J = NROW
      IF (MCBLT(7) .LT. 0) GO TO 200
C
C     NASTRAN ORIGIANL METHOD
C
      IBLK( 1) = MCBLT(1)
      IBLK( 9) = 1
      IBLK(10) = 1
C
C     BACKWARD SUBSTITUTION
C
      IF (BUF(3) .EQ. BUF(5)) GO TO 40
C     BUF(3) = BUF(4)
      IF (L16 .NE. 0) CALL CONMSG (BUF,3,0)
C
C     REWIND AND SKIP TO COLUMN N
C
      CALL REWIND (MCBLT)
      CALL SKPREC (MCBLT,NROW+1)
      GO TO 50
C
C     ALREADY AT END, NO SKIP NEEDED
C
   40 BUF(3) = BUF(6)
      IF (L16 .NE. 0) CALL CONMSG (BUF,3,0)
C
   50 IBLK(8) = -1
   60 CALL GETSTB (*100,IBLK(1))
      NTMS = IBLK(6)
      JI   = IBLK(5)
      IK   = IBLK(4)
      IF (IK-NTMS+1 .NE. J) GO TO 70
      NTMS = NTMS - 1
      XLJJ = XL(JI-NTMS)
      IF (NTMS .EQ. 0) GO TO 90
   70 SUM  = ZERO
      DO 80 II = 1,NTMS
      SUM  = SUM + XL(JI)*V2(IK)
      JI   = JI - 1
      IK   = IK - 1
   80 CONTINUE
      V2(J)= V2(J) + SUM
   90 CALL ENDGTB (IBLK(1))
      GO TO 60
  100 V2(J)= V2(J)/XLJJ
      IF (J .EQ. 1) GO TO 110
      J    = J - 1
      GO TO 50
  110 CALL FRMLTD (MCBSMA(1),V2(1),V3(1),VB(1))
C
C     FORWARD SWEEP DIRECTLY ON V3
C
      DO 160 J = 1,NROW
      IBLK(8) = -1
  120 CALL GETSTR (*160,IBLK(1))
      JI   = IBLK(5)
      NTMS = IBLK(6)
      IK   = IBLK(4)
      IF (IK .NE. J) GO TO 130
      NTMS = NTMS - 1
      V3(J)= V3(J)/XL(JI)
      JI   = JI + 1
      IK   = IK + 1
  130 IF (NTMS .EQ. 0) GO TO 150
      V3J  = V3(J)
      IF (V3J .EQ. ZERO) GO TO 150
      DO 140 II = 1,NTMS
      V3(IK) = V3(IK) + XL(JI)*V3J
      IK   = IK + 1
      JI   = JI + 1
  140 CONTINUE
  150 CALL ENDGET (IBLK(1))
      GO TO 120
  160 CONTINUE
      GO TO 500
C
C     NEW METHOD
C
C     MATRIX MCBLT HAS BEEN RE-WRITTEN TO MCBLTX BY UNPSCR/FEER3. NO
C     STRING OPERATIONS HERE.
C
  200 IF (BUF(3) .EQ. BUF(5)) BUF(3) = BUF(6)
      IF (L16 .NE. 0) CALL CONMSG (BUF,3,0)
      MCBLTX = -MCBLT(7)
      IF (MOD(MCBLT(4),10) .NE. 3) GO TO 440
      NREC = 0
      CALL REWIND (MCBLTX)
      CALL FWDREC (*400,MCBLTX)
      NWDS = MCBLT(5)
C
C     IZ(1)                                                      GINO
C      / V1   V2    V3           VB (OPEN CORE LENGTH = NZVB)   BUFFERS
C     +-----+-----+-----+-----+-------------------------------+---------
C                          OPEN  CORE
C
C
C     BACKWARD SUBSTITUTION
C
C
      LL2  = 0
      BASE = 1
      IFB  = -350
      DO 280 IK = 1,NROW
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
C 230 FORMAT (1X,I10,' WORDS READ FROM RECORD NO.',I5,'.   LL2 =',I10)
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
      SUM  = ZERO
      DO 260 I = IB,IE
      II   = II + 1
  260 SUM  = SUM + VB(I)*V2(II)
      V2(J)= V2(J) + SUM
  270 V2(J)= V2(J)/VB(IB-1)
  280 J    = J - 1
      CALL FRMLTD (MCBSMA(1),V2(1),V3(1),VB(1))
C
C     FORWARD SWEEP DIRECTLY ON V3
C
      IF (NROW .EQ. 1) GO TO 500
      NREC = 0
      LL2  = 0
      BASE = 1
      IFB  = +390
      DO 320 J = 1,NROW
      IF (BASE .LT. LL2) GO TO 300
      NREC = NREC + 1
CDB   IF (DEBUG) WRITE (IO,210) NREC,IFB
      CALL READ (*400,*290,MCBLTX,VB,NZVB,1,LL)
      CALL MESAGE (-8,0,NAM)
  290 LL2  = LL/NWDS
CDB   LL3  = LL2/30
C     LL4  = LL2 - LL3
C     IF (DEBUG) WRITE (IO,230) LL,NREC,LL2
      BASE = 1
  300 XLJJ = VB(BASE)
      II   = LJJ(1)
      JJ   = LJJ(2)
CDB   IF (DEBUG .AND. (BASE.LT.LL3 .OR. BASE.GT.LL4))
C    1    WRITE (IO,250) J,BASE,II,JJ,IFB
      IF (II .NE. J) GO TO 420
      NTMS = JJ - II + 1
      V3(J)= V3(J)/VB(BASE+1)
      IF (NTMS .LE. 1) GO TO 320
      V3J  = V3(J)
      IF (V3J .EQ. ZERO) GO TO 320
      IB   = BASE + 2
      IE   = BASE + NTMS
      DO 310 I = IB,IE
      II   = II + 1
  310 V3(II) = V3(II) + VB(I)*V3J
  320 BASE = BASE + NTMS + 1
      GO TO 500
C
  400 I = MCBLT(4)/10
      WRITE  (IO,410) NREC,J,I,IFB
  410 FORMAT ('0*** TRY TO READ RECORD',I5,'.  J,MCBLT(4),IFB =',I7,2I5)
      CALL MESAGE (-3,MCBLTX,NAM)
  420 WRITE  (IO,430) J,II,IFB
  430 FORMAT ('0*** ROW MISMATCH.  J,II,(IFB =',I7,I12,3H  (,I4)
      GO TO 460
  440 J = MOD(MCBLT(4),10)
      WRITE  (IO,450) J
  450 FORMAT ('0*** MCBLT MATRIX IN WRONG FORM.   UNPSCR FLAG =',2I3)
  460 CALL MESAGE (-37,0,BUF(1))
C
  500 BUF(3) = BUF(5)
      IF (L16 .NE. 0) CALL CONMSG (BUF,3,0)
      RETURN
      END
