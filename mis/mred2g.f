      SUBROUTINE MRED2G (KODE)
C
C     THIS SUBROUTINE CALCULATES THE FINAL STRUCTURAL MATRICES FOR THE
C     MRED2 MODULE.
C
C     INPUT DATA
C     GINO -   KBB    - STIFFNESS PARTITION MATRIX
C              KIB    - KIB  STIFFNESS PATTITION MATRIX
C              HIE    - HIE  PARTITION MATRIX
C              KII    - KII  PARTITION MATRIX
C              HGH    - HORG PARTITION MATRIX
C              MAA    - MASS INPUT MATRIX
C              BAA    - DAMPING INPUT MATRIX
C              K4AA   - STIFFNESS INPUT MATRIX
C              PAA    - LOADS INPUT MATRIX
C     SOF  -   GIMS   - G TRANSFORMATION MATRIX
C
C     OUTPUT DATA
C     GINO -   KHH    - STIFFNESS MATRIX
C              MHH    - MASS MATRIX
C              BHH    - DAMPING MATRIX
C              K4HH   - K4HH  MATRIX
C              PHH    - LOADS MATRIX
C     SOF  -   KMTX   - STIFFNESS MATRIX
C              MMTX   - MASS  MATRIX
C              PVEC   - LOADS MATRIX
C              PAPP   - APPENDED LOADS MATRIX
C              BMTX   - DAMPING MATRIX
C              K4MX   - K4MX STIFFNESS MATRIX
C
C     PARAMETERS
C     INPUT  - POPT   - LOADS OPTION FLAG
C              GBUF   - GINO BUFFERS
C              INFILE - INPUT   FILE NUMBERS
C              OTFILE - OUTPUT  FILE NUMBERS
C              ISCR   - SCRATCH FILE NUMBERS
C              KORLEN - LENGTH OF OPEN CORE
C              KORBGN - BEGINNING ADDRESS OF OPEN CORE
C              OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
C     OTHERS - PAA    - LOADS INPUT FILE NUMBER
C              KHH    - STIFFNESS I
C              KHH    - STIFFNESS OUTPUT FILE NUMBER
C              POVE   - LOADS OUTPUT FILE NUMBER
C              UPRT   - PARTITION VECTOR FILE NUMBER
C              ZEROEB - ZERO PARTITION FILE NUMBER
C              KBB    - KBB INPUT FILE NUMBER
C              ZEROBE - ZERO PARTITION MATRIX
C              KIB    - KIB INPUT FILE NUMBER
C              KII    - KII INPUT FILE NUMBER
C              KBARBB - KBARBB FILE NU BER
C              GIB    - GIB INPUT FILE NUMBER
C              KEE    - KEE FILE NUMBER
C              HGH    - HORG INPUT FILE NUMBER
C
      LOGICAL          FREBDY,BOUNDS,MODES,PONLY
      INTEGER          DRY,POPT,GBUF1,SBUF1,SBUF2,SBUF3,OTFILE,OLDNAM,Z,
     1                 T,SIGNAB,SIGNC,PREC,SCR,TYPIN,TYPOUT,UN,UB,UI,
     2                 FUSET,PREC3,ZEROBE,ZEROEB,BLANKS,PAPP,PAA,POVE,
     3                 UPRT,GIB,HGH,HIE,RPRTN,CPRTN,USETMR,DBLKOR,EQST
      DOUBLE PRECISION DZ
      DIMENSION        MODNAM(2),ITRLR1(7),ITRLR2(7),ITRLR3(7),ISUB(4),
     1                 ITMLST(11),ITMNAM(2),RZ(1),DZ(1)
      CHARACTER        UFM*23
      COMMON /XMSSG /  UFM
      COMMON /BLANK /  IDUM1,DRY,POPT,GBUF1,IDUM2(2),SBUF1,SBUF2,SBUF3,
     1                 INFILE(12),OTFILE(6),ISCR(10),KORLEN,KORBGN,
     2                 OLDNAM(2),NEWNAM(2),FREBDY,IDUM6(5),BOUNDS,MODES,
     3                 IDUM7(4),PONLY,LSTZWD
      COMMON /ZZZZZZ/  Z(1)
      COMMON /SYSTEM/  IDUM3,IPRNTR
      COMMON /MPYADX/  ITRLRA(7),ITRLRB(7),ITRLRC(7),ITRLRD(7),NZ,T,
     1                 SIGNAB,SIGNC,PREC,SCR,DUMM
      COMMON /PACKX /  TYPIN,TYPOUT,IROW,NROW,INCR
      COMMON /BITPOS/  IDUM4(9),UN,IDUM5(10),UB,UI
      COMMON /PATX  /  LCORE,NSUB(3),FUSET
      COMMON /MPY3TL/  JTRLRA(7),JTRLRB(7),JTRLRE(7),JTRLRC(7),JSCR(3),
     1                 LKORE,ICODE,PREC3,DUMMY(13)
      EQUIVALENCE      (EQST,INFILE(5)),(USETMR,INFILE(5)),
     1                 (PAA,INFILE(10)),(KHH,OTFILE(1)),(POVE,OTFILE(6))
      EQUIVALENCE      (ZEROBE,ISCR(1)),(UPRT,ISCR(1)),(KIB,ISCR(2)),
     1                 (ZEROEB,ISCR(3)),(KII,ISCR(3)),(KBB,ISCR(1)),
     2                 (GIB,ISCR(4)),(KBARBB,ISCR(5)),(KEE,ISCR(6)),
     3                 (HIE,ISCR(7)),(HGH,ISCR(8)),(RPRTN,ISCR(2)),
     4                 (CPRTN,ISCR(4)),(RZ(1),Z(1)),(DZ(1),Z(1))
      DATA    MODNAM/  4HMRED,4H2G  /,      PAPP  ,BLANKS/4HPAPP,4H    /
      DATA    ITMLST/  4HKMTX,4HMMTX,4HBMTX,4HK4MX,4HPVEC,4HPAPP,4HPOVE,
     1                 4HGIMS,4HHORG,4HPOAP,4HUPRT/
      DATA    MRED2 /  27 /
C
C     SELECT OPERATION
C     KODE = 1, NO SETLVL, NO STIFFNESS CALCULATIONS
C     KODE = 2, SETLVL, STIFFNESS CALCULATIONS
C     KODE = 3, NO SETLVL, NO STIFFNESS CALCULATIONS
C     KODE = 4, SETLVL, NO STIFFNESS CALCULATIONS
C
      IF (DRY .EQ. -2) GO TO 300
      GO TO (90,1,90,1), KODE
C
C     SET UP NEW SUBSTRUCTURE
C
    1 IF (BOUNDS .OR. MODES) GO TO 5
      NUMB = 1
      CALL SETLVL (NEWNAM,NUMB,OLDNAM,ITEST,MRED2)
      IF (ITEST .EQ. 8) GO TO 290
    5 IF (KODE  .EQ. 4) GO TO 90
C
C     FORM PRELIMINARY STIFFNESS CALCULATION
C
C                                      T
C        **      **   **   **   **   ** **   **
C        *        *   *     *   *     * *     *
C        * KBARBB * = * KBB * + * GIB * * KIB *
C        *        *   *     *   *     * *     *
C        **      **   **   **   **   ** **   **
C
      ITRLR1(1) = KBB
      CALL RDTRL (ITRLR1)
      ITEM = ITMLST(8)
      ITMNAM(1) = OLDNAM(1)
      ITMNAM(2) = OLDNAM(2)
      CALL SOFTRL (OLDNAM,ITEM,ITRLR2)
      ITEST = ITRLR2(1)
      IF (ITEST .NE. 1) GO TO 200
      CALL MTRXI (GIB,OLDNAM,ITEM,0,ITEST)
      IF (ITEST .NE. 1) GO TO 200
      CALL SOFCLS
      ITRLR2(1) = GIB
      CALL RDTRL (ITRLR2)
      ITRLR3(1) = KIB
      CALL RDTRL (ITRLR3)
      DO 10 I = 1,7
      ITRLRA(I) = ITRLR2(I)
      ITRLRB(I) = ITRLR3(I)
   10 ITRLRC(I) = ITRLR1(I)
      IFORM = 6
      IPRC  = 1
      ITYP  = 0
      IF ((ITRLRA(5) .EQ. 2) .OR. (ITRLRA(5) .EQ. 4)) IPRC = 2
      IF ((ITRLRB(5) .EQ. 2) .OR. (ITRLRB(5) .EQ. 4)) IPRC = 2
      IF ((ITRLRC(5) .EQ. 2) .OR. (ITRLRC(5) .EQ. 4)) IPRC = 2
      IF (ITRLRA(5) .GE. 3) ITYP = 2
      IF (ITRLRB(5) .GE. 3) ITYP = 2
      IF (ITRLRC(5) .GE. 3) ITYP = 2
      ITYPE = IPRC + ITYP
      CALL MAKMCB (ITRLRD,KBARBB,ITRLR1(3),IFORM,ITYPE)
      T      = 1
      SIGNAB = 1
      SIGNC  = 1
      PREC   = 0
      SCR    = ISCR(9)
      DBLKOR = KORBGN/2 + 1
      NZ = LSTZWD - (2*DBLKOR - 1)
      CALL MPYAD  (DZ(DBLKOR),DZ(DBLKOR),DZ(DBLKOR))
      CALL WRTTRL (ITRLRD)
      KBAROW = ITRLRD(3)
      KCOL   = ITRLRD(2)
C
C     FORM PRELIMINARY STIFFNESS CALCULATION
C
C                         T
C        **   **   **   ** **   ** **   **
C        *     *   *     * *     * *     *
C        * KEE * = * HIE * * KII * * HIE *
C        *     *   *     * *     * *     *
C        **   **   **   ** **   ** **   **
C
      ITRLR1(1) = HIE
      ITRLR2(1) = KII
      CALL RDTRL (ITRLR1)
      CALL RDTRL (ITRLR2)
      DO 20 I = 1,7
      JTRLRA(I) = ITRLR1(I)
      JTRLRB(I) = ITRLR2(I)
   20 JTRLRE(I) = 0
      IPRC = 1
      ITYP = 0
      IF (JTRLRA(5).EQ.2 .OR. JTRLRA(5).EQ.4) IPRC = 2
      IF (JTRLRB(5).EQ.2 .OR. JTRLRB(5).EQ.4) IPRC = 2
      IF (JTRLRA(5).GE.3 .OR. JTRLRB(5).GE.3) ITYP = 2
      ITYPE = IPRC + ITYP
      CALL MAKMCB (JTRLRC,KEE,ITRLR1(2),IFORM,ITYPE)
      JSCR(1) = ISCR(9)
      JSCR(2) = ISCR(2)
      JSCR(3) = ISCR(1)
      LKORE   = NZ
      ICODE   = 0
      PREC3   = 0
      CALL MPY3DR (DZ(DBLKOR))
      CALL WRTTRL (JTRLRC)
      KEEROW = JTRLRC(3)
      KEECOL = JTRLRC(2)
C
C     GENERATE MERGE PARTITION VECTOR
C
      NROW = KCOL + KEECOL
      DO 80 I = 1,NROW
      RZ(KORBGN+I-1) = 0.0
      IF (I .GT. KCOL) RZ(KORBGN+I-1) = 1.0
   80 CONTINUE
      TYPIN  = 1
      TYPOUT = 1
      IROW   = 1
      INCR   = 1
      IFORM  = 7
      CALL MAKMCB (ITRLR1,RPRTN,NROW,IFORM,TYPIN)
      CALL GOPEN  (RPRTN,Z(GBUF1),1)
      CALL PACK   (RZ(KORBGN),RPRTN,ITRLR1)
      CALL CLOSE  (RPRTN,1)
      CALL WRTTRL (ITRLR1)
C
C     FORM STIFFNESS MATRIX
C
C                  **            **
C                  *        .     *
C        **   **   * KBARBB .  0  *
C        *     *   *        .     *
C        * KHH * = *..............*
C        *     *   *        .     *
C        **   **   *   0    . KEE *
C                  *        .     *
C                  **            **
C
      ISUB(1) = KCOL
      ISUB(2) = KEECOL
      ISUB(3) = KBAROW
      ISUB(4) = KEEROW
      IFORM   = 6
      CALL GMMERG (KHH,KBARBB,0,0,KEE,RPRTN,RPRTN,ISUB,IFORM,Z(KORBGN),
     1             KORLEN)
C
C     STORE KHH AS KMTX ON SOF
C
      CALL SOFOPN (Z(SBUF1),Z(SBUF2),Z(SBUF3))
      ITMNAM(1) = NEWNAM(1)
      ITMNAM(2) = NEWNAM(2)
      CALL MTRXO (KHH,NEWNAM,ITMLST(1),0,ITEST)
      ITEM = ITMLST(1)
      IF (ITEST .NE. 3) GO TO 200
      GO TO 100
C
C     LOCATE HGH MATRIX
C
   90 CALL MTRXI (HGH,OLDNAM,ITMLST(9),0,ITEST)
      ITEM = ITMLST(9)
      ITMNAM(1) = OLDNAM(1)
      ITMNAM(2) = OLDNAM(2)
      IF (ITEST .NE. 1) GO TO 200
  100 SIGNAB = 1
      SIGNC  = 1
      SCR    = ISCR(1)
      DBLKOR = KORBGN/2 + 1
      LCORE  = LSTZWD - (2*DBLKOR - 1)
C
C     GENERATE MATRICES REQUESTED
C     I = 2, GENERATE MHH MATRIX
C     I = 3, GENERATE BHH MATRIX
C     I = 4, GENERATE K4HH MATRIX
C     I = 5, GENERATE PHH MATRIX
C
      DO 180 I = 2,5
      ITRLR1(1) = INFILE(I+5)
      CALL RDTRL (ITRLR1)
      IF (ITRLR1(1) .LT. 0) GO TO 180
      CALL SOFCLS
C
C     CALCULATE MATRIX REQUIRED
C
C                                T
C        **          **   **   ** **          ** **   **
C        *            *   *     * *            * *     *
C        * (M,B,K4)HH * = * HGH * * (M,B,K4)AA * * HGH *
C        *            *   *     * *            * *     *
C        **          **   **   ** **          ** **   **
C
C                         T
C        **   **   **   ** **   **
C        *     *   *     * *     *
C        * PHH * = * HGH * * PAA *
C        *     *   *     * *     *
C        **   **   **   ** **   **
C
      ITRLR2(1) = HGH
      CALL RDTRL (ITRLR2)
      ITEM = ITMLST(I)
      IF (I.EQ.5 .AND. POPT.EQ.PAPP) ITEM = ITMLST(6)
      DO 120 J = 1,7
      JTRLRA(J) = ITRLR2(J)
      JTRLRB(J) = ITRLR1(J)
  120 JTRLRE(J) = 0
      IFORM = 6
      IPRC  = 1
      ITYP  = 0
      IF (JTRLRA(5).EQ.2 .OR. JTRLRA(5).EQ.4) IPRC = 2
      IF (JTRLRB(5).EQ.2 .OR. JTRLRB(5).EQ.4) IPRC = 2
      IF (JTRLRA(5).GE.3 .OR. JTRLRB(5).GE.3) ITYP = 2
      ITYPE = IPRC + ITYP
      CALL MAKMCB (JTRLRC,OTFILE(I),ITRLR2(2),IFORM,ITYPE)
      JSCR(1) = ISCR(9)
      JSCR(2) = ISCR(2)
      JSCR(3) = ISCR(1)
      ICODE   = 0
      IF (I .EQ. 5) ICODE = 1
      PREC3 = 0
      CALL MPY3DR (DZ(DBLKOR))
      CALL WRTTRL (JTRLRC)
C
C     STORE MATRIX ON SOF
C     I = 2, STORE MHH AS MMTX
C     I = 3, STORE BHH AS BMTX
C     I = 4, STORE K4HH AS K4MX
C     I = 5, STORE PHH AS PVEC OR PAPP
C
      CALL SOFOPN (Z(SBUF1),Z(SBUF2),Z(SBUF3))
      ITMNAM(1) = NEWNAM(1)
      ITMNAM(2) = NEWNAM(2)
      CALL MTRXO (OTFILE(I),NEWNAM,ITEM,0,ITEST)
      IF (ITEST .NE. 3) GO TO 200
  180 CONTINUE
C
C     TEST FOR LOAD PROCESSING
C
      IF (POPT .EQ. BLANKS) GO TO 190
      IF (.NOT. PONLY) GO TO 184
      ITRLR1(1) = EQST
      CALL RDTRL (ITRLR1)
      NSUB(1 )  = ITRLR1(6)
      NSUB(2)   = ITRLR1(7)
      ITEM      = ITMLST(11)
      ITMNAM(1) = OLDNAM(1)
      ITMNAM(2) = OLDNAM(2)
      CALL MTRXI (UPRT,OLDNAM,ITEM,0,ITEST)
      IF (ITEST .NE. 1) GO TO 200
      GO TO 188
C
C     PARTITION PAA VECTOR
C
  184 LCORE = KORLEN
      FUSET = USETMR
      CALL CALCV (UPRT,UN,UI,UB,Z(KORBGN))
  188 CONTINUE
      CALL GMPRTN (PAA,POVE,0,0,0,0,UPRT,NSUB(1),NSUB(2),Z(KORBGN),
     1             KORLEN)
C
C     SAVE POVE AS POVE OR POAP ON SOF
C
      IF (MODES) GO TO 190
      ITEM = ITMLST(7)
      IF (POPT .EQ. PAPP) ITEM = ITMLST(10)
      CALL MTRXO (POVE,OLDNAM,ITEM,0,ITEST)
      IF (ITEST .NE. 3) GO TO 200
  190 CONTINUE
      GO TO 300
C
C     PROCESS MODULE ERRORS
C
  200 GO TO (210,220,230,240,250,270), ITEST
  210 IMSG = -9
      GO TO 280
  220 IMSG = -11
      GO TO 280
  230 IMSG = -1
      GO TO 260
  240 IMSG = -2
      GO TO 260
  250 IMSG = -3
  260 CALL SMSG (IMSG,ITEM,ITMNAM)
      GO TO 300
  270 IMSG = -10
  280 DRY  = -2
      CALL SMSG1 (IMSG,ITEM,ITMNAM,MODNAM)
      GO TO 300
  290 WRITE  (IPRNTR,295) UFM
  295 FORMAT (A23,' 6518, ONE OF THE COMPONENT SUBSTRUCTURES HAS BEEN ',
     1       'USED IN A PREVIOUS COMBINE OR REDUCE.')
      DRY = -2
  300 RETURN
      END
