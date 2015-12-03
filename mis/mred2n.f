      SUBROUTINE MRED2N
C
C     THIS SUBROUTINE CALCULATES THE K MATRIX FOR THE MRED2 MODULE.
C
      INTEGER         DRY,GBUF1,GBUF2,SBUF1,SBUF2,SBUF3,OTFILE,PREC,
     1                TYPIN,TYPOUT,HK,DBLKOR
      DOUBLE PRECISION DZ
      DIMENSION       ITRLR1(7),ITRLR2(7),MODNAM(2),RZ(1),DZ(1)
      COMMON /BLANK / IDUM1,DRY,IDUM2,GBUF1,GBUF2,IDUM3,SBUF1,SBUF2,
     1                SBUF3,INFILE(12),OTFILE(6),ISCR(10),KORLEN,KORBGN,
     2                IDUM4(14),NMODES,IDUM6(2),LSTZWD
      COMMON /ZZZZZZ/ Z(1)
      COMMON /CONDAS/ IDUM5(4),FORPI2
      COMMON /MPY3TL/ ITRLRA(7),ITRLRB(7),ITRLRE(7),ITRLRC(7),JSCR(3),
     1                LKORE,ICODE,PREC,DUMMY(13)
      COMMON /PACKX / TYPIN,TYPOUT,IROW,NROW,INCR
      EQUIVALENCE     (LAMAMR,INFILE(2)),(RZ(1),Z(1)),(DZ(1),Z(1)),
     1                (HK,ISCR(2)),(KMW2,ISCR(5)),(K,ISCR(3))
      DATA    MODNAM/ 4HMRED,4H2N  /
C
C                    2
C     FORM KMW2 = M W  MATRIX
C                  I I
C
      IF (DRY .EQ. -2) RETURN
      IF (KORBGN+7+NMODES .GE. KORLEN) GO TO 75
      IFILE = LAMAMR
      CALL GOPEN  (LAMAMR,Z(GBUF1),0)
      CALL FWDREC (*70,LAMAMR)
      IFORM = 3
      ITYPE = 1
      CALL MAKMCB (ITRLR1,KMW2,NMODES,IFORM,ITYPE)
      TYPIN = 1
      TYPOUT= 1
      IROW  = 1
      NROW  = NMODES
      INCR  = 1
      CALL GOPEN (KMW2,Z(GBUF2),1)
      DO 20 I = 1,NMODES
      CALL READ (*60,*70,LAMAMR,Z(KORBGN),7,0,NWDSRD)
      DO 10 J = 1,NMODES
      RZ(KORBGN+7+J-1) = 0.0
      IF (J .EQ. I)
     1    RZ(KORBGN+7+J-1) = FORPI2*RZ(KORBGN+5)*(RZ(KORBGN+4)**2)
   10 CONTINUE
   20 CALL PACK (Z(KORBGN+7),KMW2,ITRLR1)
      CALL CLOSE (LAMAMR,1)
      CALL CLOSE (KMW2,1)
      CALL WRTTRL (ITRLR1)
C
C     FORM K MATRIX
C
C                      T
C                       **      **
C        ** **   **  ** * .    0 * **  **
C        *   *   *    * *  .     * *    *                  2
C        * K * = * HK * *   K    * * HK *     WHERE K = M W
C        *   *   *    * *    .   * *    *                I I
C        ** **   **  ** * 0   .  * **  **
C                       **      **
C
      ITRLR2(1) = HK
      CALL RDTRL (ITRLR2)
      DO 30 I = 1,7
      ITRLRA(I) = ITRLR2(I)
      ITRLRB(I) = ITRLR1(I)
   30 ITRLRE(I) = 0
      IPRC = 1
      ITYP = 0
      IF ((ITRLRA(5) .EQ. 2) .OR. (ITRLRA(5) .EQ. 4)) IPRC = 2
      IF ((ITRLRB(5) .EQ. 2) .OR. (ITRLRB(5) .EQ. 4)) IPRC = 2
      IF (ITRLRA(5) .GE. 3) ITYP = 2
      IF (ITRLRB(5) .GE. 3) ITYP = 2
      ITYPE = IPRC + ITYP
      IFORM = 6
      CALL MAKMCB (ITRLRC,K,ITRLR2(3),IFORM,ITYPE)
      JSCR(1) = ISCR(8)
      JSCR(2) = ISCR(6)
      JSCR(3) = ISCR(2)
      ICODE   = 0
      PREC    = 0
      DBLKOR  = (KORBGN/2) + 1
      LKORE   = LSTZWD - (2*DBLKOR - 1)
      CALL SOFCLS
      CALL MPY3DR (DZ(DBLKOR))
      CALL WRTTRL (ITRLRC)
      CALL SOFOPN (Z(SBUF1),Z(SBUF2),Z(SBUF3))
      RETURN
C
C     PROCESS SYSTEM FATAL ERRORS
C
   60 IMSG = -2
      GO TO 80
   70 IMSG = -3
      GO TO 80
   75 IMSG = -8
      IFILE = 0
   80 CALL SOFCLS
      CALL MESAGE (IMSG,IFILE,MODNAM)
      RETURN
      END
