      SUBROUTINE MRED2F
C
C     THIS SUBROUTINE COMPUTES THE FREEBODY EFFECTS FOR THE MRED2
C     MODULE.
C
C     INPUT DATA
C     GINO   - MAA    - SUBSTRUCTURE MASS MATRIX
C              DMR    - FREEBODY MATRIX
C     SOF    - GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS OF
C                       ORIGINAL SUBSTRUCTURE
C
C     OUTPUT DATA
C     GINO   - HGH    - HORG PARTITION MATRIX
C     SOF    - HORG   - H TRANSFORMATION MATRIX FOR ORIG. SUBSTRUCTURE
C
C     PARAMETERS
C     INPUT  - GBUF   - GINO BUFFERS
C              INFILE - INPUT FILE NUMBERS
C              ISCR   - SCRATCH FILE NUMBERS
C              KORLEN - LENGTH OF OPEN CORE
C              KORBGN - BEGINNING ADDRESS OF OPEN CORE
C              FREBDY - FREEBODY MODES OPTION FLAG
C     OTHERS - RPRTN  - ROW PARTITIONING VECTOR FILE NUMBER
C              LII    - LII PARTITION MATRIX FILE NUMBER (ISCR11)
C              IDENT  - IDENTITY MATRIX FILE NUMBER
C              ZERO   - ZERO MATRIX FILE NUMBER
C              HIE    - HIE PARTITION MATRIX FILE NUMBER
C              HIR    - HIR PARTITION MATRIX FILE NUMBER
C              HIRSCR - HIR SCRATCH PARTITION MATRIX FILE NUMBER
C              FBR    - FBR PARTITION MATRIX FILE NUMBER
C              FIR    - FIR PARTITION MATRIX FILE NUMBER
C              GIB    - GIMS INPUT FILE NUMBER
C              CPRTN  - COLUMN PARTITIONING VECTOR FILE NUMBER
C              HIM    - HIM PARTITION MATRIX FILE NUMBER
C              HGH    - HORG MATRIX FILE NUMBER
C
      LOGICAL          FREBDY,BOUNDS
      INTEGER          DRY,GBUF1,GBUF2,SBUF1,SBUF2,SBUF3,OLDNAM,Z,T,
     1                 SIGNAB,SIGNC,PRECMP,SCR,FUSET,PRECFB,SIGN,TYPINP,
     2                 TYPEOP,TYPINU,UN,UB,UI,DMR,FAR,FIR,FARIND,ZERO,
     3                 RPRTN,HIE,HIR,CPRTN,HIM,HGH,GIB,HIRSCR,USETMR,
     4                 DBLKOR,SGLKOR
      DOUBLE           PRECISION DZ,DHIRMG
      DIMENSION MODNAM (2),ITRLR1(7),ITRLR2(7),RZ(1),ISUB(4),ITMLST(4),
     1                 DZ(1)
      COMMON /BLANK /  IDUM1,DRY,IDUM7,GBUF1,GBUF2,IDUM2,SBUF1,SBUF2,
     1                 SBUF3,INFILE(12),OTFILE(6),ISCR(10),KORLEN,
     2                 KORBGN,OLDNAM(2),IDUM4(2),FREBDY,IDUM8(5),BOUNDS,
     3                 IDUM9(6),LSTZWD,ISCR11
      COMMON /ZZZZZZ/  Z(1)
      COMMON /MPYADX/  ITRLRA(7),ITRLRB(7),ITRLRC(7),ITRLRD(7),NZMPY,T,
     1                 SIGNAB,SIGNC,PRECMP,SCR
      COMMON /BITPOS/  IDUM5(9),UN,IDUM6(10),UB,UI
      COMMON /PATX  /  LCORE,NSUB(3),FUSET
      COMMON /FBSX  /  JTRLRL(7),JTRLRU(7),JTRLRB(7),JTRLRX(7),NZFBS,
     1                 PRECFB,SIGN
      COMMON /PACKX /  TYPINP,TYPEOP,IROWP,NROWP,INCRP
      COMMON /UNPAKX/  TYPINU,IROWU,NROWU,INCRU
      COMMON /SYSTEM/  IDUM3,IPRNTR
      EQUIVALENCE      (USETMR,INFILE(5)),(MAA,INFILE(7)),
     1                 (DMR,INFILE(11)),(RPRTN,ISCR(9)),(IDENT,ISCR(5)),
     2                 (CPRTN,ISCR(10)),(PPRTN,ISCR(4)),(RZ(1),Z(1)),
     3                 (DZ(1),Z(1)),(GIB,ISCR(4)),(LII ,ISCR11),
     4                 (HIRSCR,ISCR(5)),(HGH,ISCR(8)),(ZERO,ISCR(6)),
     5                 (HIM,ISCR(8)),(HIE,ISCR(7)),(HIR ,ISCR(9)),
     6                 (FAR,ISCR(9)),(FIR,ISCR(10))
      DATA    MODNAM/  4HMRED,4H2F  /
      DATA    FARIND,  ISCR7 ,ISCR8 /6, 307, 308  /
      DATA    ITMLST/  4HGIMS,4HHORG,4HUPRT,4HLMTX/
C
C     TEST FREEBODY MODES CALCULATION FLAG
C
      IF (DRY .EQ. -2) GO TO 300
      ITRLR2(1) = DMR
      CALL RDTRL (ITRLR2)
      IF (ITRLR2(1) .LT. 0) GO TO 110
C
C     COMPUTE FREEBODY MATRIX
C
C        **   **   **   ** **   **
C        *     *   *     * *     *
C        * FAR * = * MAA * * DMR *
C        *     *   *     * *     *
C        **   **   **   ** **   **
C
      CALL SOFCLS
      FREBDY = .TRUE.
      ITRLR1(1) = MAA
      CALL RDTRL (ITRLR1)
      DO 10 I = 1,7
      ITRLRA(I) = ITRLR1(I)
      ITRLRB(I) = ITRLR2(I)
   10 ITRLRC(I) = 0
      IFORM = 2
      IPRC  = 1
      ITYP  = 0
      IF (ITRLRA(5).EQ.2 .OR. ITRLRA(5).EQ.4) IPRC = 2
      IF (ITRLRB(5).EQ.2 .OR. ITRLRB(5).EQ.4) IPRC = 2
      IF (ITRLRA(5) .GE. 3) ITYP = 2
      IF (ITRLRB(5) .GE. 3) ITYP = 2
      ITYPE = IPRC + ITYP
      CALL MAKMCB (ITRLRD,FAR,ITRLR1(3),IFORM,ITYPE)
      T     = 0
      SIGNAB= 1
      SIGNC = 1
      PREC  = 0
      SCR   = ISCR(4)
      DBLKOR= 1 + KORBGN/2
      NZMPY = LSTZWD - 2*DBLKOR - 1
      CALL MPYAD  (DZ(DBLKOR),DZ(DBLKOR),DZ(DBLKOR))
      CALL WRTTRL (ITRLRD)
C
C     PARTITION FAR INTO BOUNDARY, INTERIOR POINTS
C
C                  **   **
C                  *     *
C        **   **   * FBR *
C        *     *   *     *
C        * FAR * = *.....*
C        *     *   *     *
C        **   **   * FIR *
C                  *     *
C                  **   **
C
      LCORE = NZMPY
      FUSET = USETMR
      CALL CALCV (PPRTN,UN,UI,UB,Z(KORBGN))
      CALL GMPRTN (FAR,FIR,0,0,0,0,PPRTN,NSUB(1),NSUB(2),Z(KORBGN),
     1             KORLEN)
C
C     CALCULATE FREEBODY TRANSFORMATION MATRIX
C
C                       T
C        **   ** **   ** **   **    **   **
C        *     * *     * *     *    *     *
C        * LII * * LII * * HIR * = -* FIR *
C        *     * *     * *     *    *     *
C        **   ** **   ** **   **    **   **
C
      IF (.NOT.BOUNDS) GO TO 20
      ITEM = ITMLST(4)
      CALL SOFTRL (OLDNAM,ITEM,JTRLRL)
      ITEST = JTRLRL(1)
      IF (ITEST .NE. 1) GO TO 20
      JTRLRL(1) = LII
      CALL SOFOPN (Z(SBUF1),Z(SBUF2),Z(SBUF3))
      CALL MTRXI (LII,OLDNAM,ITEM,0,ITEST)
      IF (ITEST .NE. 1) GO TO 210
      CALL SOFCLS
      GO TO 30
   20 JTRLRL(1) = LII
      CALL RDTRL (JTRLRL)
   30 JTRLRB(1) = FIR
      CALL RDTRL (JTRLRB)
      IFORM = 2
      IPRC  = 1
      ITYP  = 0
      IF (JTRLRL(5).EQ.2 .OR. JTRLRL(5).EQ.4) IPRC = 2
      IF (JTRLRB(5).EQ.2 .OR. JTRLRB(5).EQ.4) IPRC = 2
      IF (JTRLRL(5) .GE. 3) ITYP = 2
      IF (JTRLRB(5) .GE. 3) ITYP = 2
      ITYPE = IPRC + ITYP
      CALL MAKMCB (JTRLRX,HIR,JTRLRB(3),IFORM,ITYPE)
      NZFBS  = NZMPY
      PRECFB = ITYPE
      SIGN   = -1
      CALL FBS (Z(KORBGN),Z(KORBGN))
      CALL WRTTRL (JTRLRX)
C
C     UNPACK HIR COLUMNS FOR SCALING
C
      TYPINU = JTRLRX(5)
      IROWU  = 1
      NROWU  = JTRLRX(3)
      INCRU  = JTRLRX(5)
      TYPINP = JTRLRX(5)
      TYPEOP = JTRLRX(5)
      IROWP  = 1
      NROWP  = JTRLRX(3)
      INCRP  = JTRLRX(5)
      CALL GOPEN (HIR,Z(GBUF1),0)
      IFORM  = JTRLRX(4)
      CALL MAKMCB (ITRLR1,HIRSCR,JTRLRX(3),IFORM,JTRLRX(5))
      CALL GOPEN (HIRSCR,Z(GBUF2),1)
      SGLKOR = 2*DBLKOR - 1
      DO 80 I = 1,FARIND
      CALL UNPACK (*60,HIR,DZ(DBLKOR))
C
C     CALCULATE MAGNITUDE OF HIR
C
      IF (JTRLRX(5) .EQ. 2) GO TO 42
      HIRMAG = RZ(SGLKOR)
      IF (NROWU .EQ. 1) GO TO 50
      DO 40 J = 2,NROWU
      IF (ABS(RZ(SGLKOR+J-1)) .GT. ABS(HIRMAG)) HIRMAG = RZ(SGLKOR+J-1)
   40 CONTINUE
      GO TO 50
   42 DHIRMG = DZ(DBLKOR)
      IF (NROWU .EQ. 1) GO TO 50
      DO 44 J = 2,NROWU
      IF (DABS(DZ(DBLKOR+J-1)) .GT. DABS(DHIRMG)) DHIRMG =DZ(DBLKOR+J-1)
   44 CONTINUE
C
C     SCALE HIR COLUMN
C
   50 IF (JTRLRX(5) .EQ. 2) GO TO 54
      DO 52 J = 1,NROWU
   52 RZ(SGLKOR+J-1) = RZ(SGLKOR+J-1)/HIRMAG
      GO TO 80
   54 DO 56 J = 1,NROWU
   56 DZ(DBLKOR+J-1) = DZ(DBLKOR+J-1)/DHIRMG
      GO TO 80
C
C     NULL COLUMN
C
   60 IF (JTRLRX(5) .EQ. 2) GO TO 74
      DO 72 J = 1,NROWU
   72 RZ(SGLKOR+J-1) = 0.0
      GO TO 80
   74 DO 76 J = 1,NROWU
   76 DZ(DBLKOR+J-1) = 0.0D0
C
C     PACK HIR COLUMN
C
   80 CALL PACK (DZ(DBLKOR),HIRSCR,ITRLR1)
      CALL CLOSE (HIRSCR,1)
      CALL CLOSE (HIR,1)
      CALL WRTTRL (ITRLR1)
      ISUB(1) = ITRLR1(2)
C
C     SET UP MERGE COLUMN PARTITION VECTOR
C
      ITRLR2(1) = HIM
      CALL RDTRL (ITRLR2)
      I = ITRLR1(2) + ITRLR2(2)
      ISUB(2) = ITRLR2(2)
      DO 100 J = 1,I
      RZ(KORBGN+J-1) = 0.0
      IF (J .GT. ISUB(1)) RZ(KORBGN+J-1) = 1.0
  100 CONTINUE
      TYPINP = 1
      TYPEOP = 1
      IROWP  = 1
      NROWP  = I
      INCRP  = 1
      IFORM  = 7
      CALL MAKMCB (ITRLR2,RPRTN,NROWP,IFORM,TYPINP)
      CALL GOPEN (RPRTN,Z(GBUF1),1)
      CALL PACK(RZ (KORBGN),RPRTN,ITRLR2)
      CALL CLOSE (RPRTN,1)
      CALL WRTTRL (ITRLR2)
C
C     MERGE FREEBODY, MODAL TRANSFORMATION MATRICES
C
C        **   **   **         **
C        *     *   *     .     *
C        * HIE * = * HIR . HIM *
C        *     *   *     .     *
C        **   **   **         **
C
      IF (HIE .NE. HIM) GO TO 105
      HIE = ISCR8
      HGH = ISCR7
  105 ITYPE = 1
      IF (I .NE. ITRLR2(3)) ITYPE = 2
      CALL GMMERG (HIE,HIRSCR,0,HIM,0,RPRTN,0,ISUB,ITYPE,Z(KORBGN),
     1             KORLEN)
      CALL SOFOPN (Z(SBUF1),Z(SBUF2),Z(SBUF3))
      GO TO 120
C
C     FREEBODY MODES NOT REQUESTED
C
  110 HIE = HIM
      IF (HIE .EQ. ISCR7) HGH = ISCR8
      IF (HIE .EQ. ISCR8) HGH = ISCR7
C
C     FORM HGH MATRIX
C
C                  **         **
C                  *     .     *
C        **   **   *  I  .  0  *
C        *     *   *     .     *
C        * HGH * = *...........*
C        *     *   *     .     *
C        **   **   * GIB . HIE *
C                  *     .     *
C                  **         **
C
  120 CALL SOFTRL (OLDNAM,ITMLST(2),ITRLR1)
      IF (ITRLR1(1) .EQ. 1) GO TO 190
C
C     GENERATE IDENTITY MATRIX
C
      CALL SOFTRL (OLDNAM,ITMLST(1),ITRLR1)
      ITEST = ITRLR1(1)
      ITEM  = ITMLST(1)
      IF (ITEST .NE. 1) GO TO 210
      TYPINP = 1
      TYPEOP = ITRLR1(5)
      IROWP  = 1
      NROWP  = ITRLR1(2)
      INCRP  = 1
      IFORM  = 8
      II = ITRLR1(2)
      CALL MAKMCB (ITRLR1,IDENT,NROWP,IFORM,TYPEOP)
      CALL GOPEN  (IDENT,Z(GBUF1),1)
      DO 140 I = 1,II
      DO 130 J = 1,II
      RZ(KORBGN+J-1) = 0.0
      IF (I .EQ. J) RZ(KORBGN+J-1) = 1.0
  130 CONTINUE
  140 CALL PACK (RZ(KORBGN),IDENT,ITRLR1)
      CALL CLOSE (IDENT,1)
      CALL WRTTRL (ITRLR1)
C
C     SET UP MERGE ROW PARTITION VECTOR
C
      ITRLR1(1) = HIE
      CALL RDTRL (ITRLR1)
      ITER  = ITRLR1(2)
      NROWP = II + ITER
      DO 170 I = 1,NROWP
      RZ(KORBGN+I-1) = 0.0
      IF (I .GT. II) RZ(KORBGN+I-1) = 1.0
  170 CONTINUE
      TYPINP = 1
      TYPEOP = 1
      INCRP  = 1
      IFORM  = 7
      CALL MAKMCB (ITRLR2,RPRTN,NROWP,IFORM,TYPINP)
      CALL GOPEN (RPRTN,Z(GBUF1),1)
      CALL PACK (RZ(KORBGN),RPRTN,ITRLR2)
      CALL CLOSE (RPRTN,1)
      CALL WRTTRL (ITRLR2)
      NROWS = NROWP
C
C     SET UP MERGE COLUMN PARTITION VECTOR
C
      ITEM = ITMLST(3)
      CALL MTRXI (CPRTN,OLDNAM,ITEM,0,ITEST)
      IF (ITEST .NE. 1) GO TO 210
C
C     SET UP GIB MATRIX
C
      CALL MTRXI (GIB,OLDNAM,ITMLST(1),0,ITEST)
      ITEM = ITMLST(1)
      IF (ITEST .NE. 1) GO TO 210
C
C     MERGE ALL STRUCTURAL REDUCTION TRANSFORMATION MATRICES
C
      ISUB(1) = II
      ISUB(2) = ITER
      ISUB(3) = ITRLR1(3)
      ISUB(4) = II
      ITYPE   = 1
      IF (NROWS .NE. NROWP) ITYPE = 2
      CALL GMMERG (HGH,GIB,IDENT,HIE,0,RPRTN,CPRTN,ISUB,ITYPE,Z(KORBGN),
     1             KORLEN)
C
C     SAVE HGH ON SOF AS HORG MATRIX
C
      CALL MTRXO (HGH,OLDNAM,ITMLST(2),0,ITEST)
      ITEM = ITMLST(2)
      IF (ITEST .NE. 3) GO TO 210
  190 CONTINUE
      GO TO 300
C
C     PROCESS MODULE FATAL ERRORS
C
  210 GO TO (220,230,240,250,260,280), ITEST
  220 IMSG = -9
      GO TO 290
  230 IMSG = -11
      GO TO 290
  240 IMSG = -1
      GO TO 270
  250 IMSG = -2
      GO TO 270
  260 IMSG = -3
  270 CALL SMSG (IMSG,ITEM,OLDNAM)
      GO TO 300
  280 IMSG = -10
  290 DRY  = -2
      CALL SMSG1 (IMSG,ITEM,OLDNAM,MODNAM)
  300 RETURN
      END
