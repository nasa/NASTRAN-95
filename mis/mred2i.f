      SUBROUTINE MRED2I (KODE,NUF,N2)
C
C     THIS SUBROUTINE COMPUTES THE GS MATRIX FOR THE MRED2 MODULE.
C
      INTEGER          DRY,GBUF1,OTFILE,Z,TYPIN,TYPOUT,TYPINU,
     1                 QSMROW,QSMCOL,GS,QSM,DBLKOR,SGLKOR,QSMTYP
      DOUBLE PRECISION DZ
      DIMENSION        MODNAM(2),ITRLR1(7),RZ(1),DZ(1)
      CHARACTER        UFM*23
      COMMON /XMSSG /  UFM
      COMMON /BLANK /  IDUM1,DRY,IDUM2,GBUF1,IDUM3(5),INFILE(12),
     1                 OTFILE(6),ISCR(10),KORLEN,KORBGN,IDUM6(14),
     2                 NMODES,MODLEN
      COMMON /ZZZZZZ/  Z(1)
      COMMON /SYSTEM/  IDUM4,IPRNTR
      COMMON /CONDAS/  IDUM5(4),FORPI2
      COMMON /PACKX /  TYPIN,TYPOUT,IROW,NROW,INCR
      COMMON /UNPAKX/  TYPINU,IROWU,NROWU,INCRU
      EQUIVALENCE      (LAMAMR,INFILE(2)),(QSM,INFILE(12)),
     1                 (GS,ISCR(7)),(RZ(1),Z(1)),(DZ(1),Z(1))
      DATA    MODNAM/  4HMRED,4H2I  /, IDIAG / 3 /
C
C     TEST OPERATION MODE
C
      IF (DRY .EQ. -2) RETURN
C
C     FORM GS MATRIX
C
C                 **     **
C                 *       *        T
C        **  **   * .   0 * **   **
C        *    *   *  .    * *     *                 2
C        * GS * =-*  1/K  * * QSM *    WHERE K = M W
C        *    *   *    .  * *     *               I I
C        **  **   * 0   . * **   **
C                 *       *
C                 **     **
C
      ITRLR1(1) = QSM
      CALL RDTRL (ITRLR1)
      IF (ITRLR1(1) .LT. 0) GO TO 100
      QSMROW = ITRLR1(2)
      QSMCOL = ITRLR1(3)
C
C                        2
C     FORM K = 1.0 / (M W )
C                      I I
C
      IF (KORBGN+7+QSMROW .GE. KORLEN) GO TO 80
      IFILE = LAMAMR
      CALL GOPEN (LAMAMR,Z(GBUF1),0)
      CALL FWDREC (*70,LAMAMR)
      NMODES = 0
   10 CALL READ (*60,*15,LAMAMR,Z(KORBGN),7,0,NWDSRD)
      RZ(KORBGN+7+NMODES) = 1.0/(FORPI2*RZ(KORBGN+5)*(RZ(KORBGN+4)**2))
      NMODES = NMODES + 1
      IF (KORBGN+7+NMODES .GE. KORLEN) GO TO 80
      GO TO 10
   15 CALL CLOSE (LAMAMR,1)
      IF (NMODES .NE. QSMROW) GO TO 110
      MODLEN = NMODES
C
C     READ QSM INTO CORE
C
      KORE   = KORBGN
      KORBGN = KORBGN + 7 + ITRLR1(2)
      IF (KORBGN+QSMROW*(QSMCOL+1) .GE. KORLEN) GO TO 80
      TYPINU = ITRLR1(5)
      IROWU  = 1
      NROWU  = ITRLR1(3)
      INCRU  = 1
      QSMTYP = ITRLR1(5)
      DBLKOR = KORBGN/2 + 1
      SGLKOR = 2*DBLKOR - 1
      CALL GOPEN (QSM,Z(GBUF1),0)
      IF (QSMTYP .EQ. 2) GO TO 26
      LOCQSM = SGLKOR
      DO 25 I = 1,QSMROW
      CALL UNPACK (*20,QSM,RZ(SGLKOR))
      GO TO 25
   20 DO 22 J = 1,QSMCOL
   22 RZ(SGLKOR+J-1) = 0.0E0
   25 SGLKOR = SGLKOR + ITRLR1(3)
      KORBGN = SGLKOR
      GO TO 30
   26 LOCQSM = DBLKOR
      DO 29 I = 1,QSMROW
      CALL UNPACK (*27,QSM,DZ(DBLKOR))
      GO TO 29
   27 DO 28 J = 1,QSMCOL
   28 DZ(DBLKOR+J-1) = 0.0D0
   29 DBLKOR = DBLKOR + ITRLR1(3)
      KORBGN = DBLKOR
   30 CALL CLOSE (QSM,1)
C
C     FORM GS MATRIX
C
      TYPIN  = ITRLR1(5)
      TYPOUT = ITRLR1(5)
      IROW   = 1
      NROW   = QSMROW
      INCR   = 1
      CALL MAKMCB (ITRLR1,GS,QSMROW,IDIAG,TYPIN)
      DBLKOR = KORBGN/2 + 1
      SGLKOR = 2*DBLKOR - 1
      CALL GOPEN (GS,Z(GBUF1),1)
      DO 35 I = 1,QSMCOL
      DO 34 J = 1,QSMROW
      K = 3*(J-1)
      IF (QSMTYP .EQ. 2) GO TO 32
      RZ(SGLKOR+J-1) = RZ(KORE+7+J-1)*RZ(LOCQSM+K)
      GO TO 34
   32 CONTINUE
      DZ(DBLKOR+J-1) = RZ(KORE+7+J-1)*DZ(LOCQSM+K)
   34 CONTINUE
   35 CALL PACK (DZ(DBLKOR),GS,ITRLR1)
      KORBGN = KORE
      CALL CLOSE (GS,1)
      CALL WRTTRL (ITRLR1)
      RETURN
C
C     PROCESS SYSTEM FATAL ERRORS
C
   60 IMSG = -2
      GO TO 90
   70 IMSG = -3
      GO TO 90
   80 IMSG = -8
      IFILE = 0
   90 CALL SOFCLS
      CALL MESAGE (IMSG,IFILE,MODNAM)
      RETURN
C
C     PROCESS MODULE FATAL ERRORS
C
  100 WRITE (IPRNTR,900) UFM
      GO TO 120
  110 WRITE (IPRNTR,901) UFM,QSMROW,QSMCOL,NMODES
  120 DRY = -2
      RETURN
C
  900 FORMAT (A23,' 6638, IN MODULE MREDUCE WITH USERMODE=2, THE ',
     1       'CONSTRAINT FORCES MATRIX (QSM) CANNOT BE PURGED.')
  901 FORMAT (A23,' 6634, IN MODULE MREDUCE WITH USERMODE=2, THE ',
     1       'CONSTRAINT FORCES MATRIX (',I3,3H X ,I3,1H), /30X,
     2       'IS INCOMPATABLE WITH THE NUMBER OF MODES (',I3,2H).)
C
      END
