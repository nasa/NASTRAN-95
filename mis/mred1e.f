      SUBROUTINE MRED1E
C
C     THIS SUBROUTINE GENERATES THE RIGID BODY MATRIX DMX IF FREEBODY
C     MODES ARE REQUESTED FOR THE MRED1 MODULE.
C
C     INPUT DATA
C     SOF -    BGSS   - BASIC GRID POINT IDENTIFICATION TABLE
C              EQSS   - SUBSTRUCTURE EQUIVALENCE TABLE
C              CSTM   - COORDINATE SYSTEM TRANSFORMATION MATRIX
C
C     OUTPUT DATA
C     GINO -   SCR1   - SCRATCH FILE HOLDING UNTRANSPOSED DMX MATRIX
C              DMR    - RIGID BODY MATRIX
C
C     PARAMETERS
C     INPUT  - DRY    - MODULE OPERATION FLAG
C              RGRID  - FREEBODY MODES FLAGS
C                       RGRID(1) .EQ. INTERNAL GRID POINT IDENTIFICATION
C                                     NUMBER (SET IN MRED1C)
C                       RGRID(2) .EQ. NUMBER OF THE CONTRIBUTING
C                                     SUBSTRUCTURE (SET IN MRED1)
C              KORBGN - BEGINNING ADDRESS OF OPEN CORE
C              KORLEN - LENGTH OF OPEN CORE
C              RGRID0 - FREE BODY MODE BASIC COORDINATES
C     OTHERS - NBGSS  - NUMBER OF INTERNAL GRID IDENTIFICATION POINTS
C              LOCBGS - BEGINNING ADDRESS OF BGSS DATA
C              LOCSTM - BEGINNING ADDRESS OF CSTM DATA
C              LOCSIL - BEGINNING ADDRESS OF SIL DATA
C              SMALD  - MATRIX OF COORDINATE LOCATION DIFFERENCES (3X3)
C              TI     - MATRIX OF COORDINATE TRANSFORMATIONS (3X3)
C              BIGD   - PARTITIONED MATRIX OF TRANSFORMATIONS (6X6)
C
C                                                  T
C              TTD    - TEMPORARY MATRIX HOLDING (T SMALD) (3X3)
C              KOMPNT - ARRAY HOLDING DECODED SIL COMPONENTS
C
      INTEGER         OLDNAM,DRY,GBUF1,GBUF2,RGRID,RNAME,Z,TYPIN,TYPPCK,
     1                TYPUNP,SCR1,DMR,TITTD,TIIJD1,TTDIJD,ZEROIJ,TIIJD2,
     2                DMRNAM
      DIMENSION       MODNAM(2),ITRLR(7),TI(9),SMALD(9),BIGD(36),TTD(9),
     1                KOMPNT(32),RZ(1),DMRNAM(2)
      COMMON /BLANK / OLDNAM(2),DRY,IDUM1(6),GBUF1,GBUF2,IDUM2(3),
     1                KORLEN,IDUM3(6),RGRID(2),RNAME(2),IDUM4,KORBGN,
     2                NCSUBS,IDUM5(3),NSIL,IDUM6(4),RGRID0(3)
      COMMON /ZZZZZZ/ Z(1)
      COMMON /PACKX / TYPIN,TYPPCK,IROWP,LROWP,INCRP
      COMMON /UNPAKX/ TYPUNP,IROWUP,LROWUP,INCRUP
      EQUIVALENCE     (RZ(1),Z(1))
      DATA    MODNAM/ 4HMRED,4H1E   /
      DATA    NHBGSS, NHCSTM,NHEQSS /4HBGSS,4HCSTM,4HEQSS/
      DATA    SCR1  , DMR /301,204  /
C
C     TEST FOR MODULE ERRORS
C
      IF (DRY .EQ. -2) GO TO 190
C
C     TEST FOR FREEBODY MODES REQUEST
C
      IF (RGRID(1) .EQ. -1) GO TO 190
C
C     READ BGSS DATA
C
      IT = 1
      CALL SFETCH (OLDNAM,NHBGSS,1,ITEST)
      IF (ITEST .EQ. 3) GO TO 240
      IF (ITEST .EQ. 4) GO TO 250
      IF (ITEST .EQ. 5) GO TO 260
      CALL SUREAD (Z(KORBGN),-1,NWDSRD,ITEST)
C
C     EXTRACT SUBSTRUCTURE IP DATA
C
      NBGSS  = Z(KORBGN+2)
      LOCBGS = KORBGN
      CALL SUREAD (Z(KORBGN),-2,NWDSRD,ITEST)
      KORBGN = KORBGN + NWDSRD
C
C     READ CSTM DATA
C
      LOCSTM = KORBGN
      IF (KORLEN .LE. LOCSTM) GO TO 200
      IT = 2
      CALL SFETCH (OLDNAM,NHCSTM,1,ITEST)
      IF (ITEST .EQ. 3) GO TO 30
      IF (ITEST .EQ. 4) GO TO 250
      IF (ITEST .EQ. 5) GO TO 260
      CALL SUREAD (Z(LOCSTM),-2,NWDSRD,ITEST)
      CALL PRETRS (Z(LOCSTM+3),NWDSRD-4)
C
C     CHECK FOR BASIC COORDINATES
C
   30 DO 40 I = 1, 3
   40 RGRID0(I) = 0.0
      IF (RGRID(1) .EQ. 0) GO TO 60
C
C     EXTRACT FREEBODY BASIC COORDINATES
C
      LOCRGR = LOCBGS + (4*(RGRID(1)-1))
      DO 50 I = 1,3
   50 RGRID0(I) = RZ(LOCRGR+I)
C
C     OPEN SCRATCH FILE
C
   60 IFILE = SCR1
      ITRLR(1) = IFILE
      CALL OPEN (*210,SCR1,Z(GBUF2),1)
      TYPIN = 1
      TYPPCK= 1
      IROWP = 1
      LROWP = 6
      INCRP = 1
C
C     OPEN EQSS FILE AND CHECK OPEN CORE LENGTH
C
      IT = 3
      CALL SFETCH (OLDNAM,NHEQSS,1,ITEST)
      IF (ITEST .EQ. 3) GO TO 240
      IF (ITEST .EQ. 4) GO TO 250
      IF (ITEST .EQ. 5) GO TO 260
      LOCSIL = LOCSTM + NWDSRD
      CALL SUREAD (Z(LOCSIL),-1,NWDSRD,ITEST)
      IF (KORLEN .LE. LOCSIL) GO TO 240
C
C     READ UP TO SIL DATA
C
      IF (KORLEN .LE. 2*NSIL) GO TO 240
      DO 70 I = 1,NCSUBS
      CALL SUREAD (Z(LOCSIL),-1,NWDSRD,ITEST)
      IF (KORLEN .LE. LOCSIL+NWDSRD) GO TO 240
   70 CONTINUE
C
C     GENERATE SMALD MATRIX (3X3)
C
C                **                               **
C                *                                 *
C                *    0.0      DELTA(Z)  -DELTA(Y) *
C                *                                 *
C        SMALD = * -DELTA(Z)     0.0      DELTA(X) *
C                *                                 *
C                *  DELTA(Y)  -DELTA(X)     0.0    *
C                *                                 *
C                **                               **
C
      DO 140 I = 1,NBGSS
      II       = 4*(I-1)
      SMALD(1) = 0.0
      SMALD(2) = RZ(LOCBGS+II+3) - RGRID0(3)
      SMALD(3) =-RZ(LOCBGS+II+2) + RGRID0(2)
      SMALD(4) =-SMALD(2)
      SMALD(5) = 0.0
      SMALD(6) = RZ(LOCBGS+II+1) - RGRID0(1)
      SMALD(7) =-SMALD(3)
      SMALD(8) =-SMALD(6)
      SMALD(9) = 0.0
C
C     SELECT TI, TTD MATRIX GENERATION
C
      IF (Z(LOCBGS+II)) 120,85,80
C
C     GENERATE TI, TTD MATRICES (3X3)
C     (CID .GT. 0)
C
 80   CALL TRANSS (Z(LOCBGS+II),TI)
      CALL GMMATS (TI,3,3,0,SMALD,3,3,1,TTD)
      GO TO 95
C
C     GENERATE TI, TTD MATRICES (3X3)
C     (CID .EQ. 0)
C
   85 DO 90 J = 1,3
      DO 90 K = 1,3
      L = K + 3*(J-1)
      TI(L) = 0.0
      IF (J .EQ. K) TI(L) = 1.0
   90 TTD(L) = SMALD(L)
C
C     GENERATE BIGD MATRIX (6X6)
C
C               **            **
C               *    .         *
C               *  T .  T      *
C               * T  . T SMALD *
C               *    .         *
C        BIGD = *..............*
C               *    .         *
C               *    .    T    *
C               * 0  .  T      *
C               *    .         *
C               **            **
C
   95 DO 100 J = 1,3
      DO 100 K = 1,3
      TITTD  = K + 3*(J-1)
      TIIJD1 = K + 6*(J-1)
      TTDIJD = TIIJD1 + 3
      ZEROIJ = TIIJD1 + 18
      TIIJD2 = TIIJD1 + 21
      BIGD(TIIJD1) = TI(TITTD)
      BIGD(TTDIJD) = TTD(TITTD)
      BIGD(ZEROIJ) = 0.0
  100 BIGD(TIIJD2) = TI(TITTD)
C
C     EXTRACT ROWS OF BIGD CORRESPONDING TO ACTIVE SIL COMPONENTS
C
      CALL SUREAD (Z(LOCSIL),2,NWDSRD,ITEST)
      ICODE = Z(LOCSIL+1)
      CALL DECODE (ICODE,KOMPNT,NWDSD)
      DO 110 J = 1,NWDSD
      IROWD = 1 + 6*KOMPNT(J)
  110 CALL PACK (BIGD(IROWD),SCR1,ITRLR)
      GO TO 140
C
C     SCALAR POINT ADDS NULL COLUMN TO BIGD
C     (CID .LT. 0)
C
  120 DO 130 J = 1,6
  130 BIGD(J) = 0.0
      IROWP = 1
      CALL PACK (BIGD(1),SCR1,ITRLR)
  140 CONTINUE
      CALL CLOSE (SCR1,1)
      ITRLR(3) = LROWP
C
C     READ SCR1 INTO TRANSPOSED FORM
C
      CALL OPEN (*210,SCR1,Z(GBUF1),0)
      TYPUNP = 1
      IROWUP = 1
      LROWUP = 6
      INCRUP = ITRLR(2)
      KOLMNS = ITRLR(2)
      KORBGN = LOCBGS
      IF (KORLEN .LE. KORBGN+LROWP*KOLMNS) GO TO 240
      DO 170 I = 1,KOLMNS
      CALL UNPACK (*150,SCR1,Z(KORBGN))
      GO TO 170
  150 J = KORBGN
      DO 160 K = 1,6
      RZ(J) = 0.0
  160 J = J + INCRUP
  170 KORBGN = KORBGN + 1
      CALL CLOSE (SCR1,1)
C
C     PLACE TRANSPOSED BIGD ONTO DMR OUTPUT FILE
C
      IFILE  = DMR
      CALL OPEN (*210,DMR,Z(GBUF2),1)
      CALL FNAME (DMR,DMRNAM)
      CALL WRITE (DMR,DMRNAM,2,1)
      LOCDMR = LOCBGS
      LROWP  = KOLMNS
      IFORM  = 2
      CALL MAKMCB (ITRLR,DMR,LROWP,IFORM,TYPIN)
      DO 180 I = 1,6
      CALL PACK (Z(LOCDMR),DMR,ITRLR)
  180 LOCDMR = LOCDMR + KOLMNS
      CALL CLOSE (DMR,1)
      CALL WRTTRL (ITRLR)
  190 RETURN
C
C     PROCESS SYSTEM FATAL ERRORS
C
  200 IMSG  =-8
      IFILE = 0
      GO TO 230
  210 IMSG = -1
  230 CALL SOFCLS
      CALL MESAGE (IMSG,IFILE,MODNAM)
      GO TO 190
C
C     PROCESS MODULE FATAL ERRORS
C
  240 IMSG = -1
      GO TO 270
  250 IMSG = -2
      GO TO 270
  260 IMSG = -3
  270 IF (IT-2) 280,290,300
  280 CALL SMSG (IMSG,NHBGSS,OLDNAM)
      GO TO 190
C
  290 CALL SMSG (IMSG,NHCSTM,OLDNAM)
      GO TO 190
C
  300 CALL SMSG (IMSG,NHEQSS,OLDNAM)
      GO TO 190
C
      END
