      SUBROUTINE HESS1 (KDD,MDD,LAMD,PHID,OEIGS,NFOUND,NVECD,BDD,SCR1,
     1                  SCR2,SCR3,SCR4,SCR5,SCR6,SCR7,EED,METHOD)
C
C     SUBROUTINE HESS1 TRANSFORMS THE  PROBLEM
C         PSQ M  + P B  + K   INTO   PSQ I  + MINV K
C
C     THREE CASES ARE AVAILABLE
C         1    BDB = 0   MDD  NOT IDENTITY
C              AMAT=     MINVERSE K  (MINUS ADDED IN CORE)
C                        OUTPUT   P = CSQRT COMPUTED  PS
C                        OUTPUT VEC = COMPUTED VECTOR
C
C         2    BDD = 0   MDD  IDENTITY
C              AMAT=     KDD
C                        OUTPUT AS IN CASE 1
C
C         3    BDD NOT  ZERO   MDD  NOT  IDENTITY
C              AMAT=  1    1    1
C                     1  0 1-I  1
C                     1----------
C                     1 -1 1 -1 1
C                     1M  K1M  B1
C                     1    1    1
C                     OUTPUT  P   = COMPUTED  P
C                     OUTPUT  VEC = FIRST HALF OF COMPUTED VECTOR
C
C     CORE  LAYOUT (FOR ALLMAT) IS AS FOLLOWS)
C
C     CONTENTS                SIZE              POINTER   TYPE  NAME
C     --------                ----              -------   ----  ----
C     INPUT MATRIX--VECTORS   2*NROW*NROW        IA       COMP  A
C     EIGENVALUES             2*NROW             IL       COMP  LAMBDA
C     H MATRIX                2*NROW*NROW        IH       COMP  H
C     HL MATRIX               2*NROW*NROW        IHL      COMP  HL
C     VECTOR STORAGE          2*NROW             IV       COMP  VEC
C     MULTPLIERS              2*NROW             IM       COMP  MULT
C     INTH                    NROW               INTH     INT   INTH
C     INT                     NROW               INT      LOG   INT
C
C     BUFFER                  SYSBUF             IBUF1    INT   BUFFER
C
C
C     VARIABLE  DEFINITION
C
C     ID   0  MEANS  IDENTY MASS MATRIX
C     IBDD 0  MEANS  NULL B MATRIX
C     AMAT    FINAL  A MATRIX GINO NAME
C     NROW    ORDER  OF PROBLEM
C
C
C
      INTEGER         PHID,OEIGS,BDD,SCR1,SCR2,SCR3,SCR4,SCR5,SCR6,SCR7,
     1                IZ(8),MCB(7),SYSBUF,NAME(2),FILE,IHEAD(10),EED,
     2                AMAT,EIGC(2),POIN
      DOUBLE PRECISION D1,D2,D3,D4,D5,DZ(1),TEMP(2)
      COMPLEX         CZ(1),TZ
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /UNPAKX/ ITC,II,JJ,INCR
      COMMON /ZZZZZZ/ Z(1)
      COMMON /CDCMPX/ DUM32(32),IB
      COMMON /SYSTEM/ KSYSTM(65)
      COMMON /OUTPUT/ HEAD(1)
      EQUIVALENCE     (KSYSTM( 1),SYSBUF), (KSYSTM(2),MOUT ),
     1                (KSYSTM(55),IPREC ), (Z(1),DZ(1),IZ(1),CZ(1))
      DATA    NAME  / 4HHESS,4H1        /
      DATA    IHEAD / 0,1009,4,7*0      /
      DATA    EIGC  , POIN/ 207,2,4HPOIN/
      DATA    IZ0   / 0   /
C
C     DETERMINE  IF MASS MATRIX IS IDENTITY
C
      MCB(1) = MDD
      CALL RDTRL (MCB)
      ID = 0
      IF (MCB(4) .EQ. 8) ID = 1
      NROW  = MCB(2)
      AMAT  = KDD
      IF (ID .NE. 0) GO TO 10
C
C     DECOMPOSE  MASS MATRIX
C
      IB = 0
      CALL CFACTR (MDD,SCR1,SCR2,SCR3,SCR4,SCR5,IOPT)
C
C     SOLVE FOR AMATRIX
C
      CALL CFBSOR (SCR1,SCR2,KDD,SCR3,IOPT)
C
C     DETERMINE IF  B MATRIX IS NULL
C
      AMAT = SCR3
   10 IBDD = 0
      MCB(1) = BDD
      CALL RDTRL (MCB)
      IF (MCB(1).LE.0 .OR. MCB(6).EQ.0) GO TO 30
C
C     FORM  M-1  B
C
      IBDD  = 1
      IMAT1 = BDD
      IMAT2 = KDD
      IF (ID .NE. 0) GO TO 20
C
C     - AS OF APRIL 1985 -
C     THE UPPER AND LOWER TRIANGULAR MATRICES IN SCR1 AND SCR2 WERE
C     MYSTERIOUSLY DESTROYED HERE. MUST CALL CFACTR TO RE-GENERATE THEM
C
C     - AS OF JUNE 1991 -
C     TRY WITHOUT 2ND CALL TO CFACTR, AND MAKE SURE SCR1 AND SCR2 ARE
C     STILL GINO UNITS 301 AND 302
C
      IB = 0
      CALL CFACTR (MDD,SCR1,SCR2,SCR3,SCR4,SCR5,IOPT)
C
      CALL CFBSOR (SCR1,SCR2,BDD,SCR4,IOPT)
      IMAT1 = SCR4
      IMAT2 = SCR3
   20 CALL HESS2 (NROW,SCR5,SCR6)
C
C     IDENTITY ON SCR5  MERGE VECTOR ON SCR6
C
      CALL MERGED (0,SCR5,IMAT2,IMAT1,SCR7,SCR6,SCR6,0,0)
      AMAT = SCR7
      NROW = 2*NROW
C
C     ALLOCATE  CORE FOR  ALLMAT
C
   30 IA  = 1
      IL  = IA + 2*NROW*NROW
      IH  = IL + 2*NROW
      IHL = IH + 2*NROW*NROW
      IV  = IHL+ 2*NROW*NROW
      IM  = IV + 2*NROW
      INTH= IM + 2*NROW
      INT = INTH + NROW
      NZ  =  KORSZ(IZ)
      IBUF1 = NZ - SYSBUF + 1
      IF (IH+SYSBUF .GT. NZ) CALL MESAGE (-8,0,NAME)
C
C     PROCESS EIGC CARD
C
      FILE = EED
      CALL PRELOC (*900,IZ(IBUF1-1),EED)
      CALL LOCATE (*900,IZ(IBUF1-1),EIGC,IFLAG)
   50 CALL FREAD (EED,IZ,10,0)
      IF (METHOD.EQ.IZ(1) .OR. METHOD.EQ.-1) GO TO 70
C
C     SKIP REMAINDER OF EIGC CARD
C
   60 CALL FREAD (EED,IZ,7,0)
      IF (IZ(6) .NE. -1) GO TO 60
      GO TO 50
C
C     EIGC  CARD  FOUND
C
   70 INORM = 0
      IF (IZ(4)  .NE. POIN) INORM = 1
      ISIL  = IZ(6)
      EPSI  = 1.0E-6
      IF (Z(IZ0+8) .NE. 0.0) EPSI = Z(IZ0+8)
C
C     PROCESS  REGION  DEFINITION
C
      CALL FREAD (EED,IZ,7,0)
      ALPH1 = Z(1)
      ALPH2 = Z(IZ0+3)
      W1    = Z(IZ0+2)
      W2    = Z(IZ0+4)
      NVECD = IZ(7)
      IF (NVECD .GT. 0) GO TO 95
C
C     ---- SET DEFAULT TO ONE SOLUTION VECTOR ----
C
      NVECD = 1
      WRITE  (MOUT,90) UWM
   90 FORMAT (A25,' 2357, ONE VECTOR (DEFAULT) WILL BE COMPUTED IN THE',
     1       ' COMPLEX REGION.')
   95 CALL CLOSE (EED,1)
      NVECD = MAX0(NVECD,1)
C
C     BRING IN  TERMS OF MATRIX
C
      CALL GOPEN (AMAT,IZ(IBUF1),0)
      ITC  =-3
      II   = 1
      JJ   = NROW
      INCR = 1
      DO 100 I = IA,IL
      Z(I) = 0.0
  100 CONTINUE
      J = IA
      DO 120 I = 1,NROW
      CALL UNPACK (*110,AMAT,Z(J))
  110 J = J + 2*NROW
  120 CONTINUE
      CALL CLOSE (AMAT,1)
C
C     DO IT
C
      NCOUNT = NVECD
      CALL ALLMAT (Z(IA),Z(IL),Z(IH),Z(IHL),Z(IV),Z(IM),Z(INTH),Z(INT),
     1             NROW,NCOUNT,IOPT1)
      NFOUND = NCOUNT/IPREC
      FILE   = LAMD
      CALL OPEN (*900,LAMD,IZ(IBUF1),1)
      DO 230 I = 1,NROW
      J  = IA + NROW*NROW + I - 1
      IF (IBDD .NE. 0) GO TO 210
C
C     PUT OUT COMPLEX SQUARE ROOT
C
      TZ = CSQRT(CZ(J))
      IF (AIMAG(TZ) .LT. 0.0) TZ = -TZ
      TEMP(1) = REAL(TZ)
      TEMP(2) = AIMAG(TZ)
      GO TO 220
C
C     NON-ZERO  B
C
  210 CONTINUE
      TEMP(1) = REAL(CZ(J))
      TEMP(2) = AIMAG(CZ(J))
  220 CALL WRITE (LAMD,TEMP,4,1)
  230 CONTINUE
      CALL CLOSE (LAMD,1)
C
C     PUT OUT  EIGENVECTORS
C
      FILE = PHID
      CALL OPEN (*900,PHID,IZ(IBUF1),1)
      J    = NROW*NROW + NROW
      K    = IA - 1
      NOUT = NROW*2
      IF (IBDD .NE. 0) NOUT = NOUT/2
      DO 370 M = 1,NVECD
      D1 = 0.0
      DO 310 I = 1,NOUT,2
      II = J + I
      JJ = K + I
      DZ(II  ) = Z(JJ  )
      DZ(II+1) = Z(JJ+1)
      D2 = DZ(II)*DZ(II) + DZ(II+1)*DZ(II+1)
      IF (D2 .LT. D1) GO TO 310
      D3 = DZ(II  )
      D4 = DZ(II+1)
      D1 = D2
  310 CONTINUE
      IF (INORM .EQ. 0) GO TO 350
  320 DO 330 I = 1,NOUT,2
      JJ = J + I
      D5 = (DZ(JJ)*D3 + DZ(JJ+1)*D4)/D1
      DZ(JJ+1) = (D3*DZ(JJ+1) - D4*DZ(JJ))/D1
      DZ(JJ  ) = D5
  330 CONTINUE
      GO TO 360
  350 JJ = 2*ISIL + J
      D2 = DZ(JJ)*DZ(JJ) + DZ(JJ-1)*DZ(JJ-1)
      IF (D2.EQ.0.0D0 .OR. D1/D2.GT.1.0D6) GO TO 320
      D3 = DZ(JJ-1)
      D4 = DZ(JJ  )
      D1 = D2
      GO TO 320
  360 CONTINUE
      CALL WRITE (PHID,DZ(J+1),NOUT*2,1)
      K  = K + NROW*2
  370 CONTINUE
      CALL CLOSE (PHID,1)
C
C     PUT OUT OEIGS
C
      CALL GOPEN (OEIGS,IZ(IBUF1),1)
      CALL WRITE (OEIGS,IHEAD,10,0)
      IZ(1) = NFOUND
      IZ(2) = NVECD
      IZ(3) = 0
      IZ(4) = 0
      IZ(5) = 0
      IZ(6) = 0
      IZ(7) = 0
      IZ(8) = 1
      CALL WRITE (OEIGS,IZ,40,0)
      CALL WRITE (OEIGS,HEAD,96,1)
      CALL CLOSE (OEIGS,1)
      MCB(1) = OEIGS
      MCB(2) = NFOUND
      MCB(3) = NVECD
      CALL WRTTRL (MCB)
      RETURN
C
C     ERROR MESSAGES
C
  900 IP1 =-1
      CALL MESAGE (IP1,FILE,NAME)
      RETURN
C
      END
