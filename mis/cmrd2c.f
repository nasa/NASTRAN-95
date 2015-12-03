      SUBROUTINE CMRD2C (ITER)
C
C     THIS SUBROUTINE PERFORMS THE GUYAN REDUCTION ON THE STRUCTURE
C     POINTS FOR THE CMRED2 MODULE.
C
C     INPUT  DATA
C     GINO - KII    - KII PARTITION MATRIX
C            KIB    - KIB KIB PARTITION MATRIX
C     SOF  - GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS OF
C                      ORIGINAL SUBSTRUCTURE
C
C     OUTPUT DATA
C     SOF  - LMTX   - LII PARTITION MATRIX
C            GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS OF
C                    ORIGINAL SUBSTRUCTURE
C
C     PARAMETERS
C     INPUT- GBUF   - GINO BUFFER
C            ISCR   - SCRATCH FILE NUMBER ARRAY
C            KORLEN - LENGTH OF OPEN CORE
C            KORBGN - BEGINNING ADDRESS OF OPEN CORE
C            OLDNAM - NAME OF SUBSTRUCTURE BEGING REDUCED
C            RSAVE  - DECOMPOSITION SAVE FLAG
C     OTHERS-KII    - KII PARTITION MATRIX FILE NUMBER
C            LII    - LII PARTITION MATRIX FILE NUMBER
C            SYMTRY - KII SYMMETRY FLAG
C
      LOGICAL         RSAVE,RESTOR,SYMTRY
      INTEGER         DRY,SBUF1,SBUF2,SBUF3,OLDNAM,Z,POWER,CHLSKY,UIITC,
     1                SCR,POWERC,B,BBAR,U,GIBT,PREC,SIGN,UGFBS,GIBFBS,
     2                PREC1,ATRLR,ATTRLR,GIB,UII,DBLKOR,UPPER,HIM
      DOUBLE PRECISION DETR,DETI,MINDIA,DET,MINDC,DZ
      DIMENSION       ITRLR(7),MODNAM(2),ITMLST(3),DZ(1)
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /BLANK / IDUM1,DRY,IDUM4(4),SBUF1,SBUF2,SBUF3,IDUM3(11),
     1                OTFILE(6),ISCR(11),KORLEN,KORBGN,OLDNAM(2),
     2                IDUM2(8),RSAVE,IDUM6(4),LSTZWD
      COMMON /ZZZZZZ/ Z(1)
      COMMON /SFACT / KIIT(7),LIIT(7),ISCRQ(7),ISCRA,ISCRB,NZSF,
     1                DETR,DETI,POWER,ISCRC,MINDIA,CHLSKY
      COMMON /CDCMPX/ KIITC(7),LIITC(7),UIITC(7),SCR(3),DET(2),POWERC,
     1                NX,MINDC,B,BBAR
      COMMON /FBSX  / LIIFBS(7),U(7),KIBT(7),GIBT(7),NZFBS,PREC,SIGN
      COMMON /GFBSX / LIGFBS(7),UGFBS(7),KIGFBS(7),GIBFBS(7),NZGFBS,
     1                PREC1,ISIGN
      COMMON /TRNSPX/ ATRLR(7),ATTRLR(7),LCORE,NSCRTH,ISCRTH(8)
      COMMON /SYSTEM/ IDUM5,IPRNTR
      EQUIVALENCE     (KIB,ISCR(2)),(KBI,ISCR(3)),(KII,ISCR(4)),
     1                (LII,ISCR(8)),(UII,ISCR(9)),(HIM,ISCR(10)),
     2                (GIB,ISCR(11)),(DZ(1),Z(1))
      DATA    MODNAM/ 4HCMRD,4H2C  /
      DATA    LOWER , UPPER /4,5   /
      DATA    ITMLST/ 4HLMTX,4HGIMS,4HHORG/
C
C     PREFORM GUYAN REDUCTION
C
      IF (DRY .EQ. -2) GO TO 130
      RESTOR = .FALSE.
C
C     TRANSPOSE KII, KBI
C
      IF (ITER .EQ. 1) GO TO 8
      IF (SYMTRY) GO TO 37
    1 DBLKOR = (KORBGN/2) + 1
      LCORE  = LSTZWD - ((2*DBLKOR)-1)
      NSCRTH = 5
      DO 2 I = 1,NSCRTH
    2 ISCRTH(I) = ISCR(4+I)
      DO 6 I = 1,2
      ITRLR(1) = KII
      IF (I .EQ. 2) ITRLR(1) = KBI
      CALL RDTRL (ITRLR)
      DO 4 J = 1,7
      ATRLR(J) = ITRLR(J)
    4 ATTRLR(J) = ITRLR(J)
      ATTRLR(2) = ITRLR(3)
      ATTRLR(3) = ITRLR(2)
      CALL TRNSP (DZ(DBLKOR))
    6 CALL WRTTRL (ATTRLR)
      IF (RESTOR) CALL SOFOPN (Z(SBUF1),Z(SBUF2),Z(SBUF3))
      IF (RESTOR) GO TO 39
      RESTOR = .TRUE.
      CALL SOFCLS
      GO TO 16
C
C     DECOMPOSE INTERIOR STIFFNESS MATRIX
C        (SYMMETRIC)
C
C                                 T
C        **   **   **   ** **   **
C        *     *   *     * *     *
C        * KII * = * LII * * LII *
C        *     *   *     * *     *
C        **   **   **   ** **   **
C
    8 CALL SOFCLS
      KIIT(1) = KII
      CALL RDTRL (KIIT)
      IF (KIIT(4) .NE. 6) GO TO 12
      SYMTRY = .TRUE.
      IPRC = 1
      ITYP = 0
      IF (KIIT(5).EQ.2 .OR. KIIT(5).EQ.4) IPRC = 2
      IF (KIIT(5) .GE. 3) ITYP = 2
      ITYPE = IPRC + ITYP
      CALL MAKMCB (LIIT,LII,KIIT(3),LOWER,ITYPE)
      ISCRQ(1) = ISCR(5)
      ISCRA  = ISCR(6)
      ISCRB  = ISCR(7)
      ISCRC  = ISCR(9)
      CHLSKY = 0
      POWER  = 1
      DBLKOR = (KORBGN/2) + 1
      NZSF   = LSTZWD - ((2*DBLKOR)-1)
      CALL SDCOMP (*40,DZ(DBLKOR),DZ(DBLKOR),DZ(DBLKOR))
      CALL WRTTRL (LIIT)
      GO TO 18
C
C     DECOMPOSE INTERIOR STIFFNESS MATRIX
C        (UNSYMMETRIC)
C
C        **   **   **   ** **   **
C        *     *   *     * *     *
C        * KII * = * LII * * UII *
C        *     *   *     * *     *
C        **   **   **   ** **   **
C
   12 SYMTRY = .FALSE.
   16 KIITC(1) = KII
      CALL RDTRL (KIITC)
      ITYP = 0
      IPRC = 1
      IF (KIITC(5).EQ.2 .OR. KIITC(5).EQ.4) IPRC = 2
      IF (KIITC(5) .GE. 3) ITYP = 2
      ITYPE = IPRC + ITYP
      CALL MAKMCB (LIITC,LII,KIITC(3),LOWER,ITYPE)
      CALL MAKMCB (UIITC,UII,KIITC(3),UPPER,ITYPE)
      SCR(1) = ISCR(5)
      SCR(2) = ISCR(6)
      SCR(3) = ISCR(7)
      B      = 0
      BBAR   = 0
      DBLKOR = (KORBGN/2) + 1
      NX = LSTZWD - ((2*DBLKOR)-1)
      CALL CDCOMP (*42,DZ(DBLKOR),DZ(DBLKOR),DZ(DBLKOR))
      CALL WRTTRL (LIITC)
      CALL WRTTRL (UIITC)
C
C     SAVE LII AS LMTX ON SOF
C
   18 IF (ITER.EQ.2 .OR. .NOT.RSAVE) GO TO 20
      CALL SOFOPN (Z(SBUF1),Z(SBUF2),Z(SBUF3))
      IFILE = LII
      CALL MTRXO (LII,OLDNAM,ITMLST(1),0,ITEST)
      ITEM = ITMLST(1)
      IF (ITEST .NE. 3) GO TO 70
      CALL SOFCLS
C
C     SOLVE STRUCTURE REDUCTION TRANSFORMATION MATRIX
C        (SYMMETRIC)
C
C                       T
C        **   ** **   ** **   **    **   **
C        *     * *     * *     *    *     *
C        * LII * * LII * * GIB * = -* KIB *
C        *     * *     * *     *    *     *
C        **   ** **   ** **   **    **   **
C
   20 IF (.NOT.SYMTRY) GO TO 32
      KIBT(1) = KIB
      IF (ITER .EQ. 2) KIBT(1) = KBI
      CALL RDTRL (KIBT)
      DO 30 I = 1,7
   30 LIIFBS(I) = LIIT(I)
      IPRC = 1
      ITYP = 0
      IF (KIBT(5).EQ.2 .OR. KIBT(5).EQ.4) IPRC = 2
      IF (LIIT(5).EQ.2 .OR. LIIT(5).EQ.4) IPRC = 2
      IF (KIBT(5) .GE. 3) ITYP = 2
      IF (LIIT(5) .GE. 3) ITYP = 2
      ITYPE = IPRC + ITYP
      CALL MAKMCB (GIBT,GIB,KIBT(3),KIBT(4),ITYPE)
      NZFBS = LSTZWD - ((2*DBLKOR)-1)
      PREC  = KIBT(5) - 2
      SIGN  = -1
      CALL FBS (DZ(DBLKOR),DZ(DBLKOR))
      CALL WRTTRL (GIBT)
      GO TO 36
C
C     SOLVE STRUCTURE REDUCTION TRANSFORMATION MATRIX
C        (UNSYMMETRIC)
C
C        **   ** **   ** **   **    **   **
C        *     * *     * *     *    *     *
C        * LII * * UII * * GIB * = -* KIB *
C        *     * *     * *     *    *     *
C        **   ** **   ** **   **    **   **
C
   32 KIGFBS(1) = KIB
      IF (ITER .EQ. 2) KIGFBS(1) = KBI
      CALL RDTRL (KIGFBS)
      DO 34 I = 1,7
      LIGFBS(I) = LIITC(I)
   34 UGFBS(I)  = UIITC(I)
      IPRC = 1
      ITYP = 0
      IF (KIGFBS(5).EQ.2 .OR. KIGFBS(5).EQ.4) IPRC = 2
      IF (LIITC(5) .EQ.2 .OR. LIITC(5) .EQ.4) IPRC = 2
      IF (UIITC(5) .EQ.2 .OR. UIITC(5) .EQ.4) IPRC = 2
      IF (KIGFBS(5) .GE. 3) ITYP = 2
      IF (LIITC(5)  .GE. 3) ITYP = 2
      IF (UIITC(5)  .GE. 3) ITYP = 2
      ITYPE = IPRC + ITYP
      CALL MAKMCB (GIBFBS,GIB,KIGFBS(3),KIGFBS(4),ITYPE)
      NZGFBS = LSTZWD - ((2*DBLKOR)-1)
      PREC1  = IPRC
      ISIGN  = -1
      CALL GFBS (DZ(DBLKOR),DZ(DBLKOR))
      CALL WRTTRL (GIBFBS)
C
C     SAVE GIB AS GIMS ON SOF
C
   36 IF (RESTOR) GO TO 1
      IF (ITER .EQ. 2) GO TO 39
      CALL SOFOPN (Z(SBUF1),Z(SBUF2),Z(SBUF3))
      IFILE = GIB
      CALL MTRXO (GIB,OLDNAM,ITMLST(2),0,ITEST)
      ITEM = ITMLST(2)
      IF (ITEST .NE. 3) GO TO 70
      GO TO 39
C
C     KII SYMMETRIC, GIBBAR = GIB
C
   37 ITEM = ITMLST(2)
      CALL MTRXI (GIBBAR,OLDNAM,ITEM,0,ITEST)
      IF (ITEST .NE. 1) GO TO 70
   39 CONTINUE
      GO TO 130
C
C     PROCESS SYSTEM FATAL ERRORS
C
   40 WRITE (IPRNTR,903) UWM,OLDNAM
      GO TO 44
   42 WRITE (IPRNTR,904) UWM,OLDNAM
   44 IMSG  = -37
      IFILE = 0
      CALL SOFCLS
      CALL MESAGE (IMSG,IFILE,MODNAM)
      GO TO 130
C
C     PROCESS MODULE FATAL ERRORS
C
   70 GO TO (80,82,84,90,100,120), ITEST
   80 WRITE (IPRNTR,900) UFM,MODNAM,ITEM,OLDNAM
      DRY = -2
      GO TO 130
C
   82 WRITE (IPRNTR,902) UFM,MODNAM,ITEM,OLDNAM
      DRY = -2
      GO TO 130
C
   84 IMSG = -1
      GO TO 110
   90 IMSG = -2
      GO TO 110
  100 IMSG = -3
  110 CALL SMSG (IMSG,ITEM,OLDNAM)
      GO TO 130
C
  120 WRITE (IPRNTR,901) UFM,MODNAM,ITEM,OLDNAM
      DRY = -2
  130 RETURN
C
  900 FORMAT (A23,' 6211, MODULE ',2A4,' - ITEM ',A4,
     1       ' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
  901 FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',
     1       ' OF SOF ITEM ',A4,', SUBSTRUCTURE ',2A4,', IS PURGED.')
  902 FORMAT (A23,' 6215, MODULE ',2A4,' - ITEM ',A4,
     1       ' OF SUBSTRUCTURE ',2A4,' PSEUDO-EXISTS ONLY.')
  903 FORMAT (A25,' 6311, SDCOMP DECOMPOSITION FAILED ON KII MATRIX ',
     1       'FOR SUBSTRUCTURE ',2A4)
  904 FORMAT (A23,' 6635, CDCOMP DECOMPOSITION FAILED ON KII MATRIX ',
     1       'FOR SUBSTRUCTURE ',2A4)
C
      END
