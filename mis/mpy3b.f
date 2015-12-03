      SUBROUTINE MPY3B (Z,IZ,DZ)
C*****
C    PROCESSES A AND PERFORMS FIRST PART OF PRODUCT.
C*****
      DOUBLE PRECISION DZ(1),DA
C
C
C
      INTEGER FILEA,SCR1,SCR2,FILE
      INTEGER PREC,PRECN
      INTEGER ZPNTRS
      INTEGER UTYP,UROW1,UROWN,UINCR
      INTEGER EOL,EOR
      INTEGER PRECK
C
C
C
      LOGICAL FIRST1,FIRST2
C
C
C
      DIMENSION Z(1),IZ(1)
      DIMENSION NAME(2)
C
C
C FILES
      COMMON / MPY3TL / FILEA(7),FILEB(7),FILEE(7),FILEC(7),SCR1,SCR2,
     1                  SCR,LKORE,CODE,PREC,LCORE,SCR3(7),BUF1,BUF2,
     2                  BUF3,BUF4,E
C SUBROUTINE CALL PARAMETERS
      COMMON / MPY3CP / ITRL,ICORE,N,NCB,M,NK,DUMCP(2),ZPNTRS(22),LAEND,
     1                  FIRST1,FIRST2,K,K2,KCOUNT,IFLAG,KA,KB,J,I,NTBU
C UNPACK
      COMMON / UNPAKX / UTYP,UROW1,UROWN,UINCR
C TERMWISE MATRIX READ
      COMMON / ZNTPKX / A(2),DUM(2),IROW,EOL,EOR
C
C
C
      EQUIVALENCE     (A(1),DA)
C OPEN CORE POINTERS
      EQUIVALENCE     (IBCOLS,ZPNTRS(11)),     (IBCID,ZPNTRS(13)),
     *                (IBNTU,ZPNTRS(15)),      (IKTBP,ZPNTRS(17)),
     *                (IAKJ,ZPNTRS(21))
C
C
C
      DATA NAME / 4HMPY3,4HB    /
C*****
C    INITIALIZATION.
C*****
      FILE = SCR1
      UTYP = PREC
      UROW1 = 1
      UROWN = N
      UINCR = 1
      PRECN = PREC*N
C*****
C    READ AND STORE COLUMN OF A.
C*****
      K = 0
      KT = IKTBP - 1
      IF (PREC .EQ. 2) GO TO 20
C SINGLE PRECISION CASE
      KJ = IAKJ - 1
      CALL INTPK(*120,FILEA,0,1,0)
   10 CALL ZNTPKI
      K = K + 1
      KT = KT + 1
      IZ(KT) = IROW
      KJ = KJ + 1
      Z(KJ) = A(1)
      IF (EOL .EQ. 1) GO TO 30
      GO TO 10
C DOUBLE PRECISION CASE
   20 KJ = (IAKJ - 1)/2
      CALL INTPK(*120,FILEA,0,2,0)
   25 CALL ZNTPKI
      K = K + 1
      KT = KT + 1
      IZ(KT) = IROW
      KJ = KJ + 1
      DZ(KJ) = DA
      IF (EOL .EQ. 1) GO TO 30
      GO TO 25
   30 IF (.NOT. FIRST1) GO TO 80
C*****
C    READ COLUMNS OF B INTO CORE.
C*****
      FIRST1 = .FALSE.
      IF (K .GT. NK) GO TO 40
      K2 = K
      GO TO 50
   40 K2 = NK
   50 KT = IKTBP - 1
      KB = IBCOLS - PRECN
      KBC = IBCID - 1
      DO 60 KK=1,K2
      KT = KT + 1
      KKK = IZ(KT)
      CALL FILPOS (SCR1,IZ(KKK))
      KB = KB + PRECN
      CALL UNPACK(*55,SCR1,Z(KB))
      GO TO 59
   55 IB = KB - 1
      DO 57 L=1,PRECN
      IB = IB + 1
   57 Z(IB) = 0.
   59 CONTINUE
      KBC = KBC + 1
      IZ(KBC) = KKK
   60 CONTINUE
C*****
C    BEGIN CALCULATING MATRIX PRODUCT.
C*****
   80 KT = IKTBP - 1
      KCOUNT = 0
      PRECK = PREC*K
      DO 110 KA=1,K
      KT = KT + 1
      KBC = IBCID - 1
      DO 90 KB=1,K2
      KBC = KBC + 1
      IF (IZ(KT) .EQ. IZ(KBC)) GO TO 100
   90 CONTINUE
      GO TO 110
  100 KKB = KB
      CALL MPY3P (Z,Z,Z)
      IZ(KT) = 0
      KCOUNT = KCOUNT + 1
      IF (FIRST2 .OR. ICORE .EQ. 1) GO TO 110
      I = IZ(KBC)
      CALL MPY3NU (Z)
      KBN = IBNTU + KKB - 1
      IZ(KBN) = NTBU
  110 CONTINUE
C*****
C    SET RETURN FLAG.
C*****
      IF (KCOUNT .EQ. K) GO TO 120
      IFLAG = 1
      GO TO 9999
  120 IFLAG = 0
      IF (ICORE .NE. 1 .OR. FIRST2) GO TO 9999
      IF (J .EQ. M) GO TO 9999
      FILE = SCR2
      CALL FWDREC(*902,SCR2)
      GO TO 9999
C*****
C    ERROR MESSAGES.
C*****
  902 NERR = -2
      CALL MESAGE (NERR, FILE, NAME)
C
 9999 RETURN
      END
