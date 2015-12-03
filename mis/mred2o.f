      SUBROUTINE MRED2O (NUS)
C
C     THIS SUBROUTINE FORMS THE M MATRIX FOR THE MRED2 MODULE.
C
      INTEGER         DRY,GBUF1,GBUF2,SBUF1,SBUF2,SBUF3,OTFILE,Z,
     1                TYPIN,TYPOUT,PREC,TYPEA,TYPEB,HK,GS,RPRTN,HM,
     2                GSZERO,DBLKOR
      DOUBLE PRECISION DZ
      DIMENSION       ITRLR1(7),ITRLR2(7),MODNAM(2),ISUB(4),
     1                RZ(1),BLOCK(11),DZ(1)
      COMMON /BLANK / IDUM1,DRY,IDUM2,GBUF1,GBUF2,IDUM3,SBUF1,SBUF2,
     1                SBUF3,INFILE(12),OTFILE(6),ISCR(10),KORLEN,KORBGN,
     2                IDUM5(14),NMODES,IDUM4(2),LSTZWD
      COMMON /ZZZZZZ/ Z(1)
      COMMON /MPY3TL/ ITRLRA(7),ITRLRB(7),ITRLRE(7),ITRLRC(7),JSCR(3),
     1                LKORE,ICODE,PREC,DUMMY(13)
      COMMON /PACKX / TYPIN,TYPOUT,IROW,NROW,INCR
      EQUIVALENCE     (LAMAMR,INFILE(2)),(GS,ISCR(7)),(DZ(1),Z(1)),
     1                (HK,ISCR(2)),(KMW2,ISCR(5)),(HM,ISCR(9)),
     2                (GSZERO,ISCR(10)),(M,ISCR(10)),(RPRTN,ISCR(8)),
     3                (RZ(1),Z(1)),(TYPEA,BLOCK(1)),(TYPEB,BLOCK(7))
      DATA    MODNAM/ 4HMRED,4H2O  /
C
C     FORM HM MATRIX
C
C        **  **   **  **   **          **
C        *    *   *    *   *    .   .   *
C        * HM * = * HK * + * GS . 0 . 0 *
C        *    *   *    *   *    .   .   *
C        **  **   **  **   **          **
C
      IF (DRY .EQ. -2) RETURN
      IF (NUS .EQ.  0) GO TO 60
C
C     GENERATE ROW PARTITION VECTOR
C
      ITRLR1(1) = HK
      CALL RDTRL (ITRLR1)
      ITRLR2(1) = GS
      CALL RDTRL (ITRLR2)
      TYPIN = 1
      TYPOUT= 1
      IROW  = 1
      NROW  = ITRLR1(2)
      INCR  = 1
      DO 40 I = 1,NROW
      RZ(KORBGN+I-1) = 0.0
      IF (I .GT. ITRLR2(2)) RZ(KORBGN+I-1) = 1.0
   40 CONTINUE
      IFORM = 7
      CALL MAKMCB (ITRLR2,RPRTN,NROW,IFORM,TYPIN)
      CALL GOPEN  (RPRTN,Z(GBUF1),1)
      CALL PACK   (Z(KORBGN),RPRTN,ITRLR2)
      CALL CLOSE  (RPRTN,1)
      CALL WRTTRL (ITRLR2)
C
C     MERGE GS, ZERO MATRICES
C
      ISUB(1) = ITRLR2(2)
      ISUB(2) = ITRLR1(2) - ITRLR2(2)
      CALL GMMERG (GSZERO,GS,0,0,0,RPRTN,0,ISUB,ITRLR2(5),Z(KORBGN),
     1             KORLEN)
C
C     FORM HM MATRIX
C
      ITRLR2(1) = GSZERO
      CALL RDTRL (ITRLR2)
      DO 50 I = 1,11
   50 BLOCK(I) = 0.0
      BLOCK(2) = 1.0
      BLOCK(8) = 1.0
      TYPEA = ITRLR1(5)
      TYPEB = ITRLR2(5)
      IOP   = 1
      CALL SOFCLS
      CALL SSG2C (HK,GSZERO,HM,IOP,BLOCK)
      GO TO 70
C
C     IF NO US POINTS
C
C        **  **   **  **
C        *    *   *    *
C        * HM * = * HK *
C        *    *   *    *
C        **  **   **  **
C
   60 HM = HK
      CALL SOFCLS
C
C     FORM KMW2 = M  MATRIX
C                  I
C
   70 IFILE = LAMAMR
      CALL GOPEN  (LAMAMR,Z(GBUF1),0)
      CALL FWDREC (*140,LAMAMR)
      IFORM = 3
      ITYPE = 1
      CALL MAKMCB (ITRLR1,KMW2,NMODES,IFORM,ITYPE)
      TYPIN = 1
      TYPOUT= 1
      IROW  = 1
      NROW  = NMODES
      INCR  = 1
      CALL GOPEN (KMW2,Z(GBUF2),1)
      DO 90 I = 1,NMODES
      CALL READ (*130,*140,LAMAMR,Z(KORBGN),7,0,NWDSRD)
      DO 80 J = 1,NMODES
      RZ(KORBGN+7+J-1) = 0.0
      IF (J .EQ. I) RZ(KORBGN+7+J-1) = RZ(KORBGN+5)
   80 CONTINUE
   90 CALL PACK  (Z(KORBGN+7),KMW2,ITRLR1)
      CALL CLOSE (LAMAMR,1)
      CALL CLOSE (KMW2,1)
      CALL WRTTRL (ITRLR1)
C
C     FORM M MATRIX
C
C                      T
C                **  ** **     ** **  **
C        ** **   *    * * .     * *    *
C        *   *   *    * *  .    * *    *
C        * M * = * HM * *   M   * * HM *     WHERE M = M
C        *   *   *    * *    .  * *    *                I
C        ** **   *    * *     . * *    *
C                **  ** **     ** **  **
C
      ITRLR1(1) = HM
      ITRLR2(1) = KMW2
      CALL RDTRL (ITRLR1)
      CALL RDTRL (ITRLR2)
      DO 100 I = 1,7
      ITRLRA(I) = ITRLR1(I)
      ITRLRB(I) = ITRLR2(I)
  100 ITRLRE(I) = 0
      IPRC = 1
      ITYP = 0
      IF ((ITRLRA(5) .EQ. 2) .OR. (ITRLRA(5) .EQ. 4)) IPRC = 2
      IF ((ITRLRB(5) .EQ. 2) .OR. (ITRLRB(5) .EQ. 4)) IPRC = 2
      IF (ITRLRA(5) .GE. 3) ITYP = 2
      IF (ITRLRB(5) .GE. 3) ITYP = 2
      ITYPE = IPRC + ITYP
      IFORM = 6
      CALL MAKMCB (ITRLRC,M,ITRLR1(3),IFORM,ITYPE)
      JSCR(1) = ISCR(7)
      JSCR(2) = ISCR(8)
      JSCR(3) = ISCR(6)
      ICODE   = 0
      PREC    = 0
      DBLKOR  = (KORBGN/2) + 1
      LKORE   = LSTZWD - (2*DBLKOR - 1)
      CALL MPY3DR (DZ(DBLKOR))
      CALL WRTTRL (ITRLRC)
      CALL SOFOPN (Z(SBUF1),Z(SBUF2),Z(SBUF3))
      RETURN
C
C     PROCESS SYSTEM FATAL ERRORS
C
  130 IMSG = -2
      GO TO 150
  140 IMSG = -3
  150 CALL SOFCLS
      CALL MESAGE (IMSG,IFILE,MODNAM)
      RETURN
      END
