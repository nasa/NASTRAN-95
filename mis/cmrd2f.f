      SUBROUTINE CMRD2F (KODE)
C
C     THIS SUBROUTINE CALCULATES THE FINAL STRUCTURAL MATRICES FOR THE
C     CMRED2 MODULE.
C
C     INPUT  DATA
C     GINO - KBB    - STIFFNESS PARTITION MATRIX
C            KIB    - KIB STIFFNESS PATTITION MATRIX
C            HIE    - HIE PARTITION MATRIX
C            KII    - KII PARTITION MATRIX
C            HGH    - HORG PARTITION MATRIX
C            MAA    - MASS INPUT MATRIX
C            BAA    - DAMPING INPUT MATRIX
C            K4AA   - STIFFNESS INPUT MATRIX
C            PAA    - LOADS INPUT MATRIX
C     SOF  - GIMS   - G TRANSFORMATION MATRIX
C
C     OUTPUT DATA
C     GINO - KHH    - STIFFNESS MATRIX
C            MHH    - MASS MATRIX
C            BHH    - DAMPING MATRIX
C            K4HH   - K4HH MATRIX
C            PHH    - LOADS MATRIX
C     SOF  - KMTX   - STIFFNESS MATRIX
C            MMTX   - MASS MATRIX
C            PVEC   - LOADS MATRIX
C            PAPP   - APPENDED LOADS MATRIX
C            BMTX   - DAMPING MATRIX
C            K4MX   - K4MX STIFFNESS MATRIX
C
C     PARAMETERS
C     INPUT- POPT   - LOADS OPTION FLAG
C            GBUF   - GINO BUFFERS
C            INFILE - INPUT FILE NUMBERS
C            OTFILE - OUTPUT FILE NUMBERS
C            ISCR   - SCRATCH FILE NUMBERS
C            KORLEN - LENGTH OF OPEN CORE
C            KORBGN - BEGINNING ADDRESS OF OPEN CORE
C            OLDNAM - NAME OF SUBSTRUCTURE BEING REDUCED
C     OTHERS-PAA    - LOADS INPUT FILE NUMBER
C            KHH    - STIFFNESS OUTPUT FILE NUMBER
C            POVE   - LOADS OUTPUT FILE NUMBER
C            UPRT   - PARTITION VECTOR FILE NUMBER
C            ZEROMB - ZERO PARTITION FILE NUMBER
C            KBB    - KBB INPUT FILE NUMBER
C            ZEROBM - ZERO PARTITION MATRIX
C            KIB    - KIB INPUT FILE NUMBER
C            KII    - KII INPUT FILE NUMBER
C            KBARBB - KBARBB FILE NU BER
C            GIB    - GIB INPUT FILE NUMBER
C            KMM    - KMM FILE NUMBER
C            HGH    - HORG INPUT FILE NUMBER
C
      LOGICAL         SYMTRY,MODES,PONLY
      INTEGER         DRY,POPT,GBUF1,SBUF1,SBUF2,SBUF3,OTFILE,OLDNAM,Z,
     1                T,SIGNAB,SIGNC,PREC,SCR,TYPIN,TYPOUT,UN,UB,UI,
     2                FUSET,PREC3,PAA,HIM,POVE,UPRT,GIB,GIBBAR,HGHBAR,
     3                HGH,USETMR,CMRED2,PAPP,BLANKS,DBLKOR,HIMBAR,EQST
      DOUBLE PRECISION DZ
      DIMENSION       MODNAM(2),ITRLR1(7),ITRLR2(7),ITRLR3(7),ISUB(4),
     1                ITMLST(12),ITMNAM(2),RZ(1),DZ(1)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /BLANK / IDUM1,DRY,POPT,GBUF1,IDUM2(2),SBUF1,SBUF2,SBUF3,
     1                INFILE(11),OTFILE(6),ISCR(11),KORLEN,KORBGN,
     2                OLDNAM(2),NEWNAM(2),SYMTRY,IDUM6(4),MODES,
     3                IDUM7(4),PONLY,LSTZWD
      COMMON /ZZZZZZ/ Z(1)
      COMMON /MPYADX/ ITRLRA(7),ITRLRB(7),ITRLRC(7),ITRLRD(7),NZ,T,
     1                SIGNAB,SIGNC,PREC,SCR
      COMMON /PACKX / TYPIN,TYPOUT,IROW,NROW,INCR
      COMMON /BITPOS/ IDUM4(9),UN,IDUM5(10),UB,UI
      COMMON /PATX  / LCORE,NSUB(3),FUSET
      COMMON /SYSTEM/ IDUM3,IPRNTR
      COMMON /MPY3TL/ JTRLRA(7),JTRLRB(7),JTRLRE(7),JTRLRC(7),JSCR(3),
     1                LKORE,ICODE,PREC3
      EQUIVALENCE     (EQST,INFILE(5)),(USETMR,INFILE(6)),
     1                (PAA,INFILE(11)),(KHH,OTFILE(1)),(POVE,OTFILE(6)),
     2                (KBB,ISCR(1)),(KIB,ISCR(2)),(KII,ISCR(4)),
     3                (HIM,ISCR(10)),(UPRT,ISCR(1)),(HIMBAR,ISCR(8)),
     4                (KBARBB,ISCR(5)),(KMM,ISCR(6)),(GIB,ISCR(3)),
     5                (GIBBAR,ISCR(11)),(HGHBAR,ISCR(9)),(HGH,ISCR(8)),
     6                (RPRTN,ISCR(1)),(RZ(1),Z(1)),(DZ(1),Z(1))
      DATA    MODNAM/ 4HCMRD,4H2F  /, PAPP  / 4HPAPP/, BLANKS/ 4H    /
      DATA    CMRED2/ 26    /
      DATA    ITMLST/ 4HKMTX,4HHORG,4HHLFT,4HMMTX,4HBMTX,4HK4MX,4HPVEC,
     1                4HPAPP,4HPOVE,4HGIMS,4HPOAP,4HUPRT/
C
C     SELECT OPERATION MODE
C
      IF (DRY .EQ. -2) RETURN
      IF (PONLY .OR. DRY.EQ.0) GO TO 90
C
C     SET UP NEW SUBSTRUCTURE
C
      IF (MODES) GO TO 1
      NUMB = 1
      CALL SETLVL (NEWNAM,NUMB,OLDNAM,ITEST,CMRED2)
      IF (ITEST .EQ. 8) GO TO 290
C
C     CHECK FOR STIFFNESS MATRIX GENERATION
C
    1 ITRLR1(1) = KHH
      CALL RDTRL (ITRLR1)
      IF (ITRLR1(1) .LT. 0) GO TO 90
C
C     FORM PRELIMINARY STIFFNESS CALCULATION
C
C                                           T
C        **      **   **   **   **        ** **   **
C        *        *   *     *   *          * *     *
C        * KBARBB * = * KBB * + * GIB(BAR) * * KIB *
C        *        *   *     *   *          * *     *
C        **      **   **   **   **        ** **   **
C
      ITRLR1(1) = KBB
      CALL RDTRL (ITRLR1)
      IF (SYMTRY) GO TO 2
      ITRLR2(1) = GIBBAR
      CALL RDTRL (ITRLR2)
      GO TO 4
    2 ITEM  = ITMLST(10)
      CALL SOFTRL (OLDNAM,ITEM,ITRLR2)
      ITEST = ITRLR2(1)
      ITMNAM(1) = OLDNAM(1)
      ITMNAM(2) = OLDNAM(2)
      IF (ITEST .NE. 1) GO TO 200
      CALL MTRXI (GIB,OLDNAM,ITEM,0,ITEST)
      IF (ITEST .NE. 1) GO TO 200
      ITRLR2(1) = GIB
    4 ITRLR3(1) = KIB
      CALL RDTRL (ITRLR3)
      DO 10 I = 1,7
      ITRLRA(I) = ITRLR2(I)
      ITRLRB(I) = ITRLR3(I)
   10 ITRLRC(I) = ITRLR1(I)
      IFORM = 1
      IPRC  = 1
      ITYP  = 0
      IF (ITRLRA(5).EQ.2 .OR. ITRLRA(5).EQ.4) IPRC = 2
      IF (ITRLRB(5).EQ.2 .OR. ITRLRB(5).EQ.4) IPRC = 2
      IF (ITRLRC(5).EQ.2 .OR. ITRLRC(5).EQ.4) IPRC = 2
      IF (ITRLRA(5) .GE. 3) ITYP = 2
      IF (ITRLRB(5) .GE. 3) ITYP = 2
      IF (ITRLRC(5) .GE. 3) ITYP = 2
      ITYPE  = IPRC + ITYP
      CALL MAKMCB (ITRLRD,KBARBB,ITRLR1(3),IFORM,ITYPE)
      T      = 1
      SIGNAB = 1
      SIGNC  = 1
      PREC   = 0
      SCR    = ISCR(7)
      SCR    = ISCR(1)
      CALL SOFCLS
      DBLKOR = KORBGN/2 + 1
      NZ     = LSTZWD - (2*DBLKOR - 1)
      CALL MPYAD (DZ(DBLKOR),DZ(DBLKOR),DZ(DBLKOR))
      CALL WRTTRL (ITRLRD)
      KBAROW = ITRLRD(3)
      KCOL   = ITRLRD(2)
C
C     FORM PRELIMINARY STIFFNESS CALCULATION
C
C                              T
C        **   **   **        ** **   ** **   **
C        *     *   *          * *     * *     *
C        * KMM * = * HIM(BAR) * * KII * * HIM *
C        *     *   *          * *     * *     *
C        **   **   **        ** **   ** **   **
C
      ITRLR1(1) = KII
      ITRLR2(1) = HIM
      CALL RDTRL (ITRLR1)
      CALL RDTRL (ITRLR2)
      DO 20 I = 1,7
      ITRLRA(I) = ITRLR1(I)
      ITRLRB(I) = ITRLR2(I)
   20 ITRLRC(I) = 0
      IFORM = 2
      IPRC  = 1
      ITYP  = 0
      IF (ITRLRA(5).EQ.2 .OR. ITRLRA(5).EQ.4) IPRC = 2
      IF (ITRLRB(5).EQ.2 .OR. ITRLRB(5).EQ.4) IPRC = 2
      IF (ITRLRA(5) .GE. 3) ITYP = 2
      IF (ITRLRB(5) .GE. 3) ITYP = 2
      ITYPE = IPRC + ITYP
      CALL MAKMCB (ITRLRD,ISCR(2),ITRLR2(3),IFORM,ITYPE)
      PREC  = 0
      T     = 0
      SIGNAB= 1
      SIGNC = 1
      SCR   = ISCR(1)
      CALL MPYAD (DZ(DBLKOR),DZ(DBLKOR),DZ(DBLKOR))
      CALL WRTTRL (ITRLRD)
      ITRLR1(1) = HIM
      IF (.NOT.SYMTRY) ITRLR1(1) = HIMBAR
      CALL RDTRL (ITRLR1)
      DO 30 I = 1,7
      ITRLRA(I) = ITRLR1(I)
   30 ITRLRB(I) = ITRLRD(I)
      IFORM = 1
      IPRC  = 1
      ITYP  = 0
      IF (ITRLRA(5).EQ.2 .OR. ITRLRA(5).EQ.4) IPRC = 2
      IF (ITRLRB(5).EQ.2 .OR. ITRLRB(5).EQ.4) IPRC = 2
      IF (ITRLRA(5) .GE. 3) ITYP = 2
      IF (ITRLRB(5) .GE. 3) ITYP = 2
      ITYPE = IPRC + ITYP
      CALL MAKMCB (ITRLRD,KMM,ITRLR1(2),IFORM,ITYPE)
      T    = 1
      PREC = 0
      CALL MPYAD (DZ(DBLKOR),DZ(DBLKOR),DZ(DBLKOR))
      CALL WRTTRL (ITRLRD)
      KMMROW = ITRLRD(3)
      KMMCOL = ITRLRD(2)
C
C     GENERATE MERGE PARTITION VECTOR
C
      NROW = KCOL + KMMCOL
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
      CALL GOPEN (RPRTN,Z(GBUF1),1)
      CALL PACK (RZ(KORBGN),RPRTN,ITRLR1)
      CALL CLOSE (RPRTN,1)
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
C        **   **   *   0    . KMM *
C                  *        .     *
C                  **            **
C
      ISUB(1) = KCOL
      ISUB(2) = KMMCOL
      ISUB(3) = KBAROW
      ISUB(4) = KMMROW
      ITYPE   = 1
      CALL GMMERG (KHH,KBARBB,0,0,KMM,RPRTN,RPRTN,ISUB,ITYPE,Z(KORBGN),
     1             KORLEN)
C
C     STORE KHH AS KMTX ON SOF
C
      CALL SOFOPN (Z(SBUF1),Z(SBUF2),Z(SBUF3))
      CALL MTRXO (KHH,NEWNAM,ITMLST(1),0,ITEST)
      ITEM      = ITMLST(1)
      ITMNAM(1) = NEWNAM(1)
      ITMNAM(2) = NEWNAM(2)
      IF (ITEST .NE. 3) GO TO 200
C
C     LOCATE HGH MATRIX
C       KODE .EQ. 0, BOTH HORG, HLFT ON SOF
C       KODE .EQ. 1, HORG CALCULATED, HLFT ON SOF
C       KODE .EQ. 2, HORG ON SOF, HLFT CALCULATED
C       KODE .EQ. 3, BOTH HORG, HLFT CALCULATED
C
   90 ITEM      = ITMLST(2)
      ITMNAM(1) = OLDNAM(1)
      ITMNAM(2) = OLDNAM(2)
      CALL MTRXI (HGH,OLDNAM,ITEM,0,ITEST)
      IF (ITEST .NE. 1) GO TO 200
      IF (KODE.GT.1 .OR. SYMTRY) GO TO 100
      ITEM = ITMLST(3)
      CALL MTRXI (HGHBAR,OLDNAM,ITEM,0,ITEST)
      IF (ITEST .NE. 1) GO TO 200
  100 SIGNAB = 1
      SIGNC  = 1
      SCR    = ISCR(1)
      DBLKOR = KORBGN/2 + 1
      NZ     = LSTZWD - (2*DBLKOR - 1)
      ITMNAM(1) = NEWNAM(1)
      ITMNAM(2) = NEWNAM(2)
C
C     GENERATE MATRICES REQUESTED
C        I .EQ. 2, GENERATE MHH MATRIX
C        I .EQ. 3, GENERATE BHH MATRIX
C        I .EQ. 4, GENERATE K4HH MATRIX
C        I .EQ. 5, GENERATE PHH MATRIX
C
      DO 180 I = 2,5
      ITRLR1(1) = INFILE(I+6)
      CALL RDTRL (ITRLR1)
      IF (ITRLR1(1) .LT. 0) GO TO 180
      CALL SOFCLS
C
C     CALCULATE MATRIX REQUIRED
C
C                                     T
C        **          **   **        ** **          ** **   **
C        *            *   *          * *            * *     *
C        * (M,B,K4)HH * = * HGH(BAR) * * (M,B,K4)AA * * HGH *
C        *            *   *          * *            * *     *
C        **          **   **        ** **          ** **   **
C
C                              T
C        **   **   **        ** **   **
C        *     *   *          * *     *
C        * PHH * = * HGH(BAR) * * PAA *
C        *     *   *          * *     *
C        **   **   **        ** **   **
C
      ITRLR2(1) = HGH
      CALL RDTRL (ITRLR2)
      IF (I .EQ. 5) GO TO 112
      DO 110 J = 1,7
      ITRLRA(J) = ITRLR1(J)
      ITRLRB(J) = ITRLR2(J)
  110 ITRLRC(J) = 0
      IFORM = 2
      IF (ITRLR1(3) .EQ. ITRLR2(2)) IFORM = 1
      IPRC  = 1
      ITYP  = 0
      IF (ITRLR1(5).EQ.2 .OR. ITRLR1(5).EQ.4) IPRC = 2
      IF (ITRLR2(5).EQ.2 .OR. ITRLR2(5).EQ.4) IPRC = 2
      IF (ITRLR1(5) .GE. 3) ITYP = 2
      IF (ITRLR2(5) .GE. 3) ITYP = 2
      ITYPE = IPRC + ITYP
      CALL MAKMCB (ITRLRD,ISCR(2),ITRLR1(3),IFORM,ITYPE)
      PREC  = 0
      T     = 0
      SIGNAB= 1
      SIGNC = 1
      SCR   = ISCR(1)
      CALL MPYAD (DZ(DBLKOR),DZ(DBLKOR),DZ(DBLKOR))
      CALL WRTTRL (ITRLRD)
      ITEM = ITMLST(I+2)
      GO TO 116
  112 DO 114 J = 1,7
  114 ITRLRD(J) = ITRLR1(J)
      ITEM = ITMLST(7)
      IF (POPT .EQ. PAPP) ITEM = ITMLST(8)
  116 ITRLR2(1) = HGH
      IF (.NOT. SYMTRY) ITRLR2(1) = HGHBAR
      CALL RDTRL (ITRLR2)
      DO 120 J = 1,7
      ITRLRA(J) = ITRLR2(J)
  120 ITRLRB(J) = ITRLRD(J)
      IFORM = 1
      IPRC  = 1
      ITYP  = 0
      IF (ITRLRD(5).EQ.2 .OR. ITRLRD(5).EQ.4) IPRC = 2
      IF (ITRLR2(5).EQ.2 .OR. ITRLR2(5).EQ.4) IPRC = 2
      IF (ITRLRD(5) .GE. 3) ITYP = 2
      IF (ITRLR2(5) .GE. 3) ITYP = 2
      ITYPE = IPRC + ITYP
      CALL MAKMCB (ITRLRD,OTFILE(I),ITRLR2(2),IFORM,ITYPE)
      T    = 1
      PREC = 0
      CALL MPYAD (DZ(DBLKOR),DZ(DBLKOR),DZ(DBLKOR))
      CALL WRTTRL (ITRLRD)
C
C     STORE MATRIX ON SOF
C        I .EQ. 2, STORE MHH AS MMTX
C        I .EQ. 3, STORE BHH AS BMTX
C        I .EQ. 4, STORE K4HH AS K4MX
C        I .EQ. 5, STORE PHH AS PVEC OR PAPP
C
      CALL SOFOPN (Z(SBUF1),Z(SBUF2),Z(SBUF3))
      CALL MTRXO (OTFILE(I),NEWNAM,ITEM,0,ITEST)
      IF (ITEST .NE. 3) GO TO 200
  180 CONTINUE
C
C     TEST FOR LOAD PROCESSING
C
      IF (POPT .EQ. BLANKS) GO TO 190
      ITMNAM(1) = OLDNAM(1)
      ITMNAM(2) = OLDNAM(2)
      IF (.NOT.PONLY) GO TO 184
      ITRLR1(1) = EQST
      CALL RDTRL (ITRLR1)
      NSUB(1) = ITRLR1(6)
      NSUB(2) = ITRLR1(7)
      ITEM = ITMLST(12)
      CALL MTRXI (UPRT,OLDNAM,ITEM,0,ITEST)
      IF (ITEST .NE. 1) GO TO 200
      GO TO 188
C
C     PARTITION PAA VECTOR
C
  184 LCORE = KORLEN
      FUSET = USETMR
      CALL CALCV (UPRT,UN,UI,UB,Z(KORBGN))
  188 CALL GMPRTN (PAA,POVE,0,0,0,0,UPRT,NSUB(1),NSUB(2),Z(KORBGN),
     1             KORLEN)
C
C     SAVE POVE AS POVE OR POAP ON SOF
C
      IF (MODES) GO TO 190
      ITEM = ITMLST(9)
      IF (POPT .EQ. PAPP) ITEM = ITMLST(11)
      CALL MTRXO (POVE,OLDNAM,ITEM,0,ITEST)
      IF (ITEST .NE. 3) GO TO 200
  190 CONTINUE
      RETURN
C
C     PROCESS MODULE ERRORS
C
  200 GO TO (210,210,210,220,230,250), ITEST
  210 WRITE (IPRNTR,900) UFM,MODNAM,ITEM,ITMNAM
      DRY = -2
      RETURN
C
  220 IMSG = -2
      GO TO 240
  230 IMSG = -3
  240 CALL SMSG (IMSG,ITEM,ITMNAM)
      RETURN
C
  250 WRITE (IPRNTR,901) UFM,MODNAM,ITEM,ITMNAM
      DRY = -2
      RETURN
C
  290 WRITE (IPRNTR,902) UFM
      DRY = -2
      RETURN
C
  900 FORMAT (A23,' 6211, MODULE ',2A4,' - ITEM ',A4,
     1       ' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
  901 FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',
     1       ' OF SOF ITEM ',A4,', SUBSTRUCTURE ',2A4,', IS PURGED.')
  902 FORMAT (A23,' 6518, ONE OF THE COMPONENT SUBSTRUCTURES HAS BEEN ',
     1       'USED IN A PREVIOUS COMBINE OR REDUCE.')
C
      END
