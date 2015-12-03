      SUBROUTINE MRED2M (NUF,N2,NUS)
C
C     THIS SUBROUTINE FORMS THE HK MATRIX FOR THE MRED2 MODULE.
C
      INTEGER         DRY,GBUF1,OTFILE,Z,TYPIN,TYPOUT,HKPG,PHI12I,HK,
     1                RPRTN,CPRTN
      DIMENSION       ITRLR1(7),MODNAM(2),ITRLR2(7),ISUB(4),RZ(1)
      COMMON /BLANK / IDUM1,DRY,IDUM4,GBUF1,IDUM2(17),OTFILE(6),
     1                ISCR(10),KORLEN,KORBGN,IDUM3(14),NMODES
      COMMON /ZZZZZZ/ Z(1)
      COMMON /PACKX / TYPIN,TYPOUT,IROW,NROW,INCR
      EQUIVALENCE     (HK,ISCR(2)),(IDENT,ISCR(8)),(HKPG,ISCR(3)),
     1                (PHI12I,ISCR(8)),(CPRTN,ISCR(9)),(RPRTN,ISCR(9))
      EQUIVALENCE     (RZ(1),Z(1))
      DATA    MODNAM/ 4HMRED,4H2M  /
C
C     FORM HK MATRIX
C
C        **  **   **             **
C        *    *   *      .        *
C        * HK * = * HKPG . PHI12I *
C        *    *   *      .        *
C        **  **   **             **
C
      IF (DRY .EQ. -2) RETURN
      IF (NUF .EQ.  0) GO TO 30
      ITRLR1(1) = HKPG
      CALL RDTRL (ITRLR1)
      ITRLR2(1) = PHI12I
      CALL RDTRL (ITRLR2)
      INCR    = 1
      TYPIN   = 1
      TYPOUT  = 1
      IROW    = 1
      NROW    = ITRLR1(3) + ITRLR2(3)
      ISUB(1) = ITRLR1(3)
      ISUB(2) = ITRLR2(3)
      DO 20 I = 1,NROW
      RZ(KORBGN+I-1) = 0.0
      IF (I .GT. ITRLR1(3)) RZ(KORBGN+I-1) = 1.0
   20 CONTINUE
      IFORM = 7
      CALL MAKMCB (ITRLR2,RPRTN,NROW,IFORM,TYPIN)
      CALL GOPEN (RPRTN,Z(GBUF1),1)
      CALL PACK (Z(KORBGN),RPRTN,ITRLR2)
      CALL CLOSE (RPRTN,1)
      CALL WRTTRL (ITRLR2)
      ITYPE = 2
      CALL GMMERG (HK,HKPG,0,PHI12I,0,RPRTN,0,ISUB,ITYPE,Z(KORBGN),
     1            KORLEN)
      RETURN
C
C     NO UF POINTS
C
C        **  **   **     **
C        *    *   *   .   *
C        * HK * = * 0 . I *
C        *    *   *   .   *
C        **  **   **     **
C
   30 TYPIN  = 1
      TYPOUT = 1
      IROW   = 1
      NROW   = NMODES
      INCR   = 1
      IFORM  = 8
      IF (KORBGN+NMODES .GE. KORLEN) GO TO 100
C
C     GENERATE IDENTITY MATRIX
C
      CALL MAKMCB (ITRLR2,IDENT,NMODES,IFORM,TYPIN)
      CALL GOPEN  (IDENT,Z(GBUF1),1)
      DO 70 I = 1,NMODES
      DO 60 J = 1,NMODES
      RZ(KORBGN+J-1) = 0.0
      IF (J .EQ. I) RZ(KORBGN+J-1) = 1.0
   60 CONTINUE
   70 CALL PACK (Z(KORBGN),IDENT,ITRLR2)
      CALL CLOSE (IDENT,1)
      CALL WRTTRL (ITRLR2)
C
C     GENERATE ROW PARTITIONING VECTOR
C
      NROW = NUS + NMODES
      IF (KORBGN+NROW .GE. KORLEN) GO TO 100
      J = NROW
      DO 90 I = 1,J
      RZ(KORBGN+I-1) = 0.0
      IF (I .GT. NUS) RZ(KORBGN+I-1) = 1.0
   90 CONTINUE
      IFORM = 7
      CALL MAKMCB (ITRLR2,RPRTN,NROW,IFORM,TYPIN)
      CALL GOPEN (RPRTN,Z(GBUF1),1)
      CALL PACK (Z(KORBGN),RPRTN,ITRLR2)
      CALL CLOSE (RPRTN,1)
      CALL WRTTRL (ITRLR2)
C
C     FORM HK MATRIX
C
      ISUB(1) = NUS
      ISUB(2) = NMODES
      ITYPE   = 2
      CALL GMMERG (HK,0,0,IDENT,0,RPRTN,0,ISUB,ITYPE,Z(KORBGN),KORLEN)
      RETURN
C
C     PROCESS SYSTEM ERRORS
C
  100 IMSG  =-8
      IFILE = 0
      CALL SOFCLS
      CALL MESAGE (IMSG,IFILE,MODNAM)
      RETURN
C
      END
