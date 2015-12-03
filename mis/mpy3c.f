      SUBROUTINE MPY3C (Z,IZ,DZ)
C*****
C    PERFORMS MULTIPLICATION AND SUMMATION FOR REMAINING TERMS OF COLUMN
C    OF A.
C*****
      INTEGER PREC
      INTEGER SCR1,SCR2,FILE
      INTEGER ZPNTRS
      INTEGER UTYP,UROW1,UROWN,UINCR
      INTEGER PRECN
C
C
C
      LOGICAL FIRST2
C
C
C
      DIMENSION Z(1),IZ(1)
C     DIMENSION NAME(2)
C
C
C FILES
      COMMON / MPY3TL / FILEA(7),FILEB(7),FILEE(7),FILEC(7),SCR1,SCR2,
     1                  SCR,LKORE,CODE,PREC,LCORE,SCR3(7),BUF1,BUF2,
     2                  BUF3,BUF4,E
C SUBROUTINE CALL PARAMETERS
      COMMON / MPY3CP / ITRL,ICORE,N,NCB,M,NK,DUM1(2),ZPNTRS(22),
     1                  DUM2(2),FIRST2,K,K2,KCOUNT,IFLAG,KA,LTBC,J,
     2                  LTAC,NTBU
C UNPACK
      COMMON / UNPAKX / UTYP,UROW1,UROWN,UINCR
C
C
C
      EQUIVALENCE     (IBCOLS,ZPNTRS(11)),     (IBCID,ZPNTRS(13)),
     *                (IBNTU,ZPNTRS(15)),      (IKTBP,ZPNTRS(17)),
     *                (IANTU,ZPNTRS(19))
C
C
C
C     DATA NAME / 4HMPY3,4HC    /
C*****
C    INITIALIZATION.
C*****
      UTYP  = PREC
      UROW1 = 1
      UROWN = N
      UINCR = 1
      PRECN = PREC*N
      FILE  = SCR1
C*****
C    TEST TO SEE IF LESS THAN NK COLUMNS OF B IN CORE.
C*****
      IF (FIRST2) GO TO 30
C*****
C    DETERMINE WHICH COLUMN OF B TO BE PUT INTO CORE.
C*****
      LTA = 0
      IA  = IANTU - 1
      DO 10 I=1,K
      IA = IA + 1
      IF (LTA .GE. IZ(IA)) GO TO 10
      LTA = IZ(IA)
      IK  = IKTBP + I - 1
      LTAC= IZ(IK)
      KA  = I
   10 CONTINUE
C*****
C    DETERMINE WHICH COLUMN OF B TO BE REPLACED.
C*****
      LTB = 0
      IB  = IBNTU - 1
      DO 20 I=1,NK
      IB = IB + 1
      IF (LTB .GE. IZ(IB)) GO TO 20
      LTB  = IZ(IB)
      LTBC = I
   20 CONTINUE
      GO TO 50
C*****
C    LESS THAN NK COLUMNS OF B IN CORE.
C*****
   30 K2  = K2 + 1
      LTBC= K2
      KK  = IKTBP - 1
      DO 40 KA=1,K
      KK  = KK + 1
      IF (IZ(KK) .EQ. 0) GO TO 40
      LTAC = IZ(KK)
      GO TO 50
   40 CONTINUE
C*****
C    ADD OR REPLACE COLUMN OF B INTO CORE.
C*****
   50 CALL FILPOS (SCR1,IZ(LTAC))
      KK = IBCOLS + PRECN*(LTBC - 1)
      CALL UNPACK(*55,SCR1,Z(KK))
      GO TO 59
   55 IK = KK - 1
      DO 57 L=1,PRECN
      IK = IK + 1
   57 Z(IK) = 0.
   59 CONTINUE
      IF (FIRST2) GO TO 70
      IF (ICORE .EQ. 1) GO TO 60
      CALL MPY3NU (Z)
      KK = IBNTU + LTBC - 1
      IZ(KK) = NTBU
   60 KK = IANTU + KA - 1
      IZ(KK) = 0
   70 KK = IBCID + LTBC - 1
      IZ(KK) = LTAC
      KB = LTBC
C*****
C    PERFORM COMPUTATION.
C*****
      CALL MPY3P (Z,Z,Z)
      LTBC = KB
      KK = IKTBP + KA - 1
      IZ(KK) = 0
      KCOUNT = KCOUNT + 1
      RETURN
      END
