      SUBROUTINE MRED2L (NUF,N2,NUS,UFBITS)
C
C     THIS SUBROUTINE PREFORMS PRELIMINARY CALCULATIONS AND MERGES OF
C     THE HK MATRIX FOR THE MRED2 MODULE.
C
      EXTERNAL        ANDF
      INTEGER         DRY,GBUF1,SBUF1,SBUF2,SBUF3,OTFILE,T,SIGNAB,SIGNC,
     1                PREC,SCR,TYPIN,TYPOUT,ANDF,ROWS,UFBITS,PHISS1,
     2                PHISS,DBLKOR,SGLKOR
      DOUBLE PRECISION DZ
      DIMENSION       ITRLR1(7),ITRLR2(7),MODNAM(2),ISUB(4),RZ(1),DZ(1)
      COMMON /BLANK / IDUM1,DRY,IDUM2,GBUF1,IDUM3(2),SBUF1,SBUF2,SBUF3,
     1                INFILE(12),OTFILE(6),ISCR(10),KORLEN,KORBGN,
     2                IDUM4(14),MODPTS,IDUM5(2),LSTZWD
      COMMON /ZZZZZZ/ Z(1)
      COMMON /MPYADX/ ITRLRA(7),ITRLRB(7),ITRLRC(7),ITRLRD(7),NZ,T,
     1                SIGNAB,SIGNC,PREC,SCR
      COMMON /PACKX / TYPIN,TYPOUT,IROW,NROW,INCR
      EQUIVALENCE     (GS,ISCR(7)),(PHISS1,ISCR(8)),(PHISS2,ISCR(9)),
     1                (IDENT,ISCR(5)),(PHISS,ISCR(6)),(PHIGS,ISCR(2)),
     2                (PHIS12,ISCR(2)),(PHI12I,ISCR(8)),(RPRTN,ISCR(5)),
     3                (CPRTN,ISCR(10)),(RZ(1),Z(1)),(DZ(1),Z(1))
      DATA    MODNAM/ 4HMRED,4H2L  /
C
C                  -1
C     COMPUTE PHISS1
C
      IF (DRY .EQ. -2) RETURN
      CALL SOFCLS
      IFILE = PHISS1
      ITRLR1(1) = PHISS1
      CALL RDTRL (ITRLR1)
      CALL GOPEN (PHISS1,Z(GBUF1),0)
      KOLUMN = ITRLR1(2)
      ROWS  = ITRLR1(3)
      ITEST = KOLUMN * ROWS
      IF ((KORBGN+ITEST+(3*KOLUMN)) .GE. KORLEN) GO TO 190
      KORE = 0
      DBLKOR = (KORBGN/2) + 1
      SGLKOR = (2*DBLKOR) - 1
      IF (ITRLR1(5) .EQ. 2) GO TO 15
      DO 10 I = 1,KOLUMN
      CALL READ (*170,*180,PHISS1,Z(SGLKOR+KORE),ROWS,0,NWDSRD)
   10 KORE  = KORE + ROWS
      ICORE = ((SGLKOR+ITEST)/2) + 1
      GO TO 19
   15 DO 18 I = 1,KOLUMN
      CALL READ (*170,*180,PHISS1,DZ(DBLKOR+KORE),ROWS,0,NWDSRD)
   18 KORE  = KORE + ROWS
      ICORE = DBLKOR + ITEST
   19 CALL CLOSE (PHISS1,1)
      INVERT = 0
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (ROWS,DZ(DBLKOR),KOLUMN,B,INVERT,DETERM,ISING,
     1             DZ(ICORE))
      IF (ISING .EQ. 2) GO TO 192
      KORE  = 0
      INCR  = 1
      TYPIN = 1
      TYPOUT= 1
      IROW  = 1
      NROW  = KOLUMN
      CALL MAKMCB (ITRLR2,PHISS1,NROW,ITRLR1(4),ITRLR1(5))
      CALL GOPEN (PHISS1,Z(GBUF1),1)
      DO 29 I = 1,ROWS
      IF (ITRLR1(5) .EQ. 2) GO TO 25
      CALL PACK (RZ(SGLKOR+KORE),PHISS1,ITRLR2)
      GO TO 29
   25 CALL PACK (DZ(DBLKOR+KORE),PHISS1,ITRLR2)
   29 KORE = KORE + ROWS
      CALL CLOSE (PHISS1,1)
C
C     COMPUTE PHIGS
C
C                               -1
C        **     **    **      **  **     ** **  **
C        *       *    *        *  *       * *    *
C        * PHIGS * = -* PHISS1 *  * PHISS * * GS *
C        *       *    *        *  *       * *    *
C        **     **    **      **  **     ** **  **
C
      ITRLR1(1) = PHISS1
      ITRLR2(1) = PHISS
      CALL RDTRL (ITRLR1)
      CALL RDTRL (ITRLR2)
      ICOL = ITRLR2(3)
      DO 30 I = 1,7
      ITRLRA(I) = ITRLR1(I)
      ITRLRB(I) = ITRLR2(I)
   30 ITRLRC(I) = 0
      CALL MAKMCB (ITRLRD,PHISSI,ITRLR2(3),ITRLR2(4),ITRLR2(5))
      T = 0
      SIGNAB = -1
      SIGNC = 1
      PREC = 0
      SCR = ISCR(10)
      NZ = LSTZWD - ((2*DBLKOR)-1)
      CALL MPYAD (DZ(DBLKOR),DZ(DBLKOR),DZ(DBLKOR))
      CALL WRTTRL (ITRLRD)
      ITRLR1(1) = GS
      CALL RDTRL (ITRLR1)
      DO 40 I = 1,7
      ITRLRA(I) = ITRLRD(I)
   40 ITRLRB(I) = ITRLR1(I)
      CALL MAKMCB (ITRLRD,PHIGS,ITRLR1(3),ITRLR1(4),ITRLR1(5))
      SIGNAB = 1
      CALL MPYAD (DZ(DBLKOR),DZ(DBLKOR),DZ(DBLKOR))
      CALL WRTTRL (ITRLRD)
C
C     FORM HKPG MATRIX
C
C                   **               **
C                   *       .         *
C                   *       .      -1 *
C        **    **   * PHIGS . PHISS   *
C        *      *   *       .         *
C        * HKPG * = *.................*
C        *      *   *       .         *
C        **    **   *   0   .    0    *
C                   *       .         *
C                   **               **
C
      NROW = NUF + N2
      DO 90 I = 1,NROW
      RZ(KORBGN+I-1) = 0.0
      IF (I .GT. N2) RZ(KORBGN+I-1) = 1.0
   90 CONTINUE
      TYPIN  = 1
      TYPOUT = 1
      IROW   = 1
      INCR   = 1
      IFORM  = 7
      CALL MAKMCB (ITRLR1,CPRTN,NROW,IFORM,TYPIN)
      CALL GOPEN (CPRTN,Z(GBUF1),1)
      CALL PACK (Z(KORBGN),CPRTN,ITRLR1)
      CALL CLOSE (CPRTN,1)
      CALL WRTTRL (ITRLR1)
      NROW  = NUS + NUF
      IFILE = USETMR
      CALL GOPEN (USETMR,Z(GBUF1),0)
      DO 100 I = 1,NROW
      CALL READ (*170,*180,USETMR,Z(KORBGN),1,0,NWDSRD)
      RZ(KORBGN+I) = 0.0
      IF (ANDF(Z(KORBGN),UFBITS) .NE. 0) RZ(KORBGN+I) = 1.0
  100 CONTINUE
      CALL CLOSE (USETMR,1)
      NROW = ROWS
      ROWS = NUF + N2
      CALL MAKMCB (ITRLR2,RPRTN,NROW,IFORM,TYPIN)
      CALL GOPEN (RPRTN,Z(GBUF1),1)
      CALL PACK (Z(KORBGN+1),RPRTN,ITRLR2)
      CALL CLOSE (RPRTN,1)
      CALL WRTTRL (ITRLR2)
      ISUB(1) = NUF
      ISUB(2) = N2
      ISUB(3) = NUS
      ISUB(4) = N2
      ITYPE   = 2
      CALL GMMERG (HKPG,PHIGSH,0,PHISS1,0,RPRTN,CPRTN,ISUB,ITYPE,
     1             Z(KORBGN),KORLEN)
C
C     COMPUTE PHIS12
C
C                                -1
C        **      **    **      **  **      **
C        *        *    *        *  *        *
C        * PHIS12 * = -* PHISS1 *  * PHISS2 *
C        *        *    *        *  *        *
C        **      **    **      **  **      **
C
      ITRLR1(1) = PHISS1
      ITRLR2(1) = PHISS2
      CALL RDTRL (ITRLR1)
      CALL RDTRL (ITRLR2)
      MODPTS = ITRLR1(3) + ITRLR2(3)
      DO 110 I = 1,7
      ITRLRA(I) = ITRLR1(I)
  110 ITRLRB(I) = ITRLR2(I)
      CALL MAKMCB (ITRLRD,PHIS12,ITRLR2(3),ITRLR2(4),ITRLR2(5))
      SIGNAB = -1
      CALL MPYAD (DZ(DBLKOR),DZ(DBLKOR),DZ(DBLKOR))
      CALL WRTTRL (ITRLRD)
C
C     GENERATE IDENTITY MATRIX
C
      NROW = ITRLRD(3)
      CALL MAKMCB (ITRLR1,IDENT,NROW,ITRLRD(4),ITRLRD(5))
      CALL GOPEN (IDENT,Z(GBUF1),1)
      DO 130 I = 1,NROW
      DO 120 J = 1,NROW
      RZ(KORBGN+J-1) = 0.0
      IF (J .EQ. I) RZ(KORBGN+J-1) = 1.0
  120 CONTINUE
  130 CALL PACK (Z(KORBGN),IDENT,ITRLR1)
      CALL CLOSE (IDENT,1)
      CALL WRTTRL (ITRLR1)
C
C     GENERATE PHI12I MATRIX
C
C                     **      **
C                     *        *
C        **      **   * PHIS12 *
C        *        *   *        *
C        * PHI12I * = *........*
C        *        *   *        *
C        **      **   *   I    *
C                     *        *
C                     **      **
C
      ITRLR1(1) = PHIS12
      CALL RDTRL (ITRLR1)
      ISUB(3) = ITRLR1(3)
      ISUB(4) = NROW
      NROW = ITRLR1(2) + NROW
      DO 140 I = 1,NROW
      RZ(KORBGN+I-1) = 0.0
      IF (I .GT. ITRLR1(2)) RZ(KORBGN+I-1) = 1.0
  140 CONTINUE
      INCR = 1
      CALL MAKMCB (ITRLR2,CPRTN,NROW,IFORM,TYPIN)
      CALL GOPEN (CPRTN,Z(GBUF1),1)
      CALL PACK (Z(KORBGN),RPRTN,ITRLR2)
      CALL CLOSE (CPRTN,1)
      CALL WRTTRL (ITRLR2)
      CALL GMMERG (PHI12I,PHIS12,IDENT,0,0,0,CPRTN,ISUB,ITYPE,
     1             Z(KORBGN),KORLEN)
      CALL SOFOPN (Z(SBUF1),Z(SBUF2),Z(SBUF3))
      RETURN
C
C     PROCESS SYSTEM FATAL ERRORS
C
  170 IMSG = -2
      GO TO 200
  180 IMSG = -3
      GO TO 200
  190 IMSG = -8
      GO TO 194
  192 IMSG = -37
  194 IFILE = 0
  200 CALL SOFCLS
      CALL MESAGE (IMSG,IFILE,MODNAM)
      RETURN
      END
