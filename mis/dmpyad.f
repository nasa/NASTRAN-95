      SUBROUTINE DMPYAD
C
C     DMPYAD IS THE DMAP DRIVER FOR MATRIX MULTIPLICATION.
C
C     COMMENTS FROM G.CHAN/UNISYS ABOUT PREC1 IN /MPYADX/     1/91
C     ACCORDING TO THE USER'S MANUAL ON P. 3.5-18
C       PREC1 = 0, PERFORM ARITHMETIC IN D.P. IF A,B OR C IS IN D.P.
C             = 1, PERFORM ARITHMETIC IN S.P.
C             = 2, PERFORM ARITHMETIC IN D.P.
C     HOWEVER, THE CODE BELOW ALWAYS SETS
C       PREC1 TO 2, IF ANY OF THE A,B OR C IS IN D.P. AND 1 OTHERWISE
C       IN SUBROUTINE MPYAD, PREC1 IS ALWAYS SET TO 1 FOR CDC MACHINE
C
C     IF ITYPE IN /BLANK/ IS 1 OR 3, MPYAD PRODUCT WILL BE OUPUT IN S.P.
C     AND IN D.P. OF IT IS 2 OR 4
C     IF ITYPE IS 0, MPYAD PRODUCT WILL BE IN S.P. ONLY IF ALL A, B, AND
C     C MATRICES ARE IN S.P. OTHERWISE, THE PRODUCT WILL BE IN D.P.
C
      IMPLICIT INTEGER (A-Z)
      INTEGER         NAME(2)      ,DOSI(3)      ,REFUS(3)      ,
     1                P(4)  ,Q(4)  ,R(4)  ,ZZ(1) ,ZZZ(1)
      REAL            ALP(1),BET(1)
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25,SWM*27
      COMMON /XMSSG / UFM   ,UWM   ,UIM   ,SFM   ,SWM
      COMMON /SYSTEM/ KSYSTM(65)
      COMMON /BLANK / T     ,SIGNAB,SIGNC ,ITYPE
      COMMON /MPYADX/ A(7)  ,B(7)  ,C(7)  ,D(7)  ,NZ    ,TRNSP  ,
     1                SAB   ,SC    ,PREC1 ,SCR
      COMMON /DMPYX / E(7)  ,F(7)  ,G(7)  ,NZZ   ,FLAG  ,SGN
      COMMON /SADDX / NOMAT ,NZZZ  ,MCBS(67)
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (ZZ(1),Z(1))
      EQUIVALENCE     (ZZZ(1),Z(1))
      EQUIVALENCE     (KSYSTM(55),KPREC), (KSYSTM(2),OUTPT)
      EQUIVALENCE     (MCBS( 1),P(1)   ), (MCBS( 8),TYPA  ),
     1                (MCBS( 9),ALP(1) ), (MCBS(13),Q(1)  ),
     2                (MCBS(20),TYPB   ), (MCBS(21),BET(1)),
     3                (MCBS(61),R(1)   )
      DATA    FILEA , FILEB,FILEC,FILED,SCRTCH  /
     1        101   , 102  ,103  ,201  ,301     /
      DATA    NAME  / 4HMPYA,4HD   /
      DATA    DOSI  / 4HSING,4HDOUB,4HMLTP /, REFUS/ 2*3H   ,3HREF /
      DATA    SQUARE, RECT,DIAG,SYMM,IDENT /  1,2,3,6,8  /
C
C
C     READ TRAILERS FOR A, B AND C MATRICES.
C
      NZ   = KORSZ(Z)
      A(1) = FILEA
      CALL RDTRL (A)
      IF (A(1) .NE. FILEA) GO TO 230
      B(1) = FILEB
      CALL RDTRL (B)
      IF (B(1) .NE. FILEB) GO TO 230
      C(1) = FILEC
      C(5) = 0
      CALL RDTRL (C)
      IF (C(1) .LT. 0) C(1) = 0
      D(1) = FILED
      D(3) = A(3)
      IF (T .NE. 0) D(3) = A(2)
      D(4) = RECT
C
C     CHECK FOR CONFORMABLE MATRICIES
C
      IF (((C(2).NE.B(2) .OR.  C(3).NE.D(3)) .AND. C(1).NE.0) .OR.
     1     (B(3).NE.A(2) .AND. T.EQ.0) .OR. (B(3).NE.A(3) .AND. T.NE.0))
     2     CALL MESAGE (-55,0,NAME)
      TRNSP = T
      SAB   = SIGNAB
      SC    = SIGNC
      PREC  = 1
      IF (ITYPE .EQ. 0) PREC = 0
      IF (ITYPE.EQ.2 .OR. ITYPE.EQ.4) PREC = 2
      PREC1 = MAX0(A(5),B(5),C(5))
      IF (PREC1 .GT. 2) PREC1 = PREC1 - 2
      IF (PREC1.LT.1 .OR. PREC1.GT.2) PREC1 = KPREC
      IF (PREC.EQ.PREC1 .OR. PREC.EQ.0) GO TO 20
      IF (PREC.LT.1 .OR. PREC.GT.2) PREC = 3
      WRITE  (OUTPT,10) SWM,DOSI(PREC),REFUS(PREC),NAME,DOSI(PREC1)
   10 FORMAT (A27,' 2430, REQUESTED ',A4,'LE PRECISION ',A3,'USED BY ',
     1       2A4,2H. ,A4,'LE PRECISION IS LOGICAL CHOICE')
      IF (PREC .NE. 3) PREC1 = PREC
   20 LTYPE = PREC1
      IF (A(5).EQ.3 .OR. A(5).EQ.4 .OR. B(5).EQ.3 .OR. B(5).EQ.4 .OR.
     1    C(5).EQ.3 .OR. C(5).EQ.4) LTYPE = PREC1 + 2
      IF (ITYPE.EQ.0 .OR. ITYPE.EQ.LTYPE) GO TO 40
      JJ = 1
      IF (ITYPE.LT.1 .OR. ITYPE.GT.4) JJ = 3
      WRITE  (OUTPT,30) SWM,ITYPE,REFUS(JJ),NAME,LTYPE
   30 FORMAT (A27,' 2431, REQUESTED TYPE ',I4,2H, ,A3,'USED BY ',2A4,
     1        7H. TYPE ,I4,'  IS LOGICAL CHOICE.')
      IF (JJ .NE. 3) LTYPE = ITYPE
   40 ITYPE = LTYPE
      D(5)  = ITYPE
      SCR   = SCRTCH
C
C     IF NEITHER A NOR B IS DIAGONAL, CALL MPYAD AND RETURN.
C
      IF (A(4).EQ.DIAG .OR. B(4).EQ.DIAG) GO TO 100
      CALL MPYAD (Z,Z,Z)
      IF (D(2) .NE. D(3)) GO TO 60
      D(4) = SQUARE
      IF (C(4) .EQ. 0) C(4) = DIAG
      K = 0
      DO 50 I = 4,21,7
      J = A(I)
      IF (J.NE.SYMM .AND. J.NE.DIAG .AND. J.NE.IDENT) GO TO 60
      IF (J .EQ. SYMM) K = 1
   50 CONTINUE
      IF (K .EQ. 1) D(4) = SYMM
   60 CALL WRTTRL (D)
      RETURN
C
C     OTHERWISE, CALL DMPY FOR DIAGONAL MULTIPLICATION.
C
  100 DO 110 I = 1,7
      E(I) = A(I)
      F(I) = B(I)
      IF (A(4) .EQ. DIAG) GO TO 110
      E(I) = B(I)
      F(I) = A(I)
  110 G(I) = D(I)
      NZZ  = KORSZ(ZZ)
      SGN  = SIGNAB
      FLAG = 0
      IF (B(4) .EQ. DIAG) FLAG = 1
      IF (C(1) .NE.    0) G(1) = SCRTCH
      CALL DMPY (ZZ,ZZ)
      IF (G(2) .NE. G(3)) GO TO 130
      G(4) = SQUARE
      K = 0
      DO 120 I = 4,14,7
      J = E(I)
      IF (J.NE.SYMM .AND. J.NE.DIAG .AND. J.NE.IDENT) GO TO 130
      IF (J .EQ. SYMM) K = 1
  120 CONTINUE
      IF (K .EQ. 1) G(4) = SYMM
  130 CALL WRTTRL (G)
C
C     IF ADDITION REQUIRED, CALL ADD ROUTINE.
C
      IF (C(1) .EQ. 0) RETURN
      DO 200 I = 1,7
      P(I)  = G(I)
      Q(I)  = C(I)
  200 R(I)  = D(I)
      DO 210 I = 2,4
      ALP(I)= 0.0
  210 BET(I)= 0.0
      TYPA  = 1
      ALP(1)= 1.0
      TYPB  = 1
      BET(1)= 1.0
      IF (SIGNC .LT. 0) BET(1) =-1.0
      NZZZ  = KORSZ(ZZZ)
      NOMAT = 2
      CALL SADD (ZZZ,ZZZ)
      IF (R(2) .NE. R(3)) GO TO 230
      R(4) = SQUARE
      IF (P(4).EQ.SYMM .AND. (Q(4).EQ.SYMM .OR. Q(4).EQ.DIAG .OR.
     1    Q(4).EQ.IDENT)) R(4) = SYMM
      CALL WRTTRL (R)
  230 RETURN
      END
