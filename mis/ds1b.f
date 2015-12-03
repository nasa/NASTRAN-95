      SUBROUTINE DS1B (KE,J)
C
C     THIS ROUTINE ADDS THE 6 X 6 DOUBLE PRECISION MATRIX KE TO THE
C     SUBMATRIX OF ORDER NROWSC X JMAX.
C
      INTEGER          CSTM  ,MPT   ,DIT   ,ECPTDS,OUTRW ,EOR   ,
     1                 CLSRW ,FROWIC,IZ(1)
      DOUBLE PRECISION DZ(1) ,KE(36)
      COMMON /ZZZZZZ/  Z(1)
      COMMON /DS1AAA/  NPVT  ,ICSTM ,NCSTM ,IGPCT ,NGPCT ,IPOINT,
     1                 NPOINT,I6X6K ,N6X6K ,CSTM  ,MPT   ,DIT   ,
     2                 ECPTDS,GPCT  ,KGGD  ,INRW  ,OUTRW ,EOR   ,
     3                 NEOR  ,CLSRW ,JMAX  ,FROWIC,LROWIC,NROWSC,
     4                 NLINKS,LINK(10)     ,NOGO
      EQUIVALENCE      (DZ(1),Z(1),IZ(1))
C
C     SEARCH THE GPCT AND FIND AN INDEX M SUCH THAT
C     IABS(GPCT(M)) .LE. J .LT. IABS(GPCT(M+1))
C
      LOW = IGPCT + 1
      LIM = NGPCT + LOW - 2
      IF (LOW .GT. LIM) GO TO 15
      DO 10 I = LOW,LIM
      ISAVE = I
      IF (J .GE. IABS(IZ(I+1))) GO TO 10
      IF (J .GE. IABS(IZ(I  ))) GO TO 20
   10 CONTINUE
      IF (J .GE. IABS(IZ(ISAVE+1))) ISAVE = ISAVE + 1
      GO TO 20
   15 ISAVE = LOW
C
C     ADD KE TO THE SUBMATRIX
C
   20 L1  = FROWIC - 1
      JJ  = IPOINT + ISAVE - IGPCT
      J2  = IZ(JJ) - 1
      I1  = 0
      LIM = NROWSC - 1
   30 IF (I1 .GT. LIM) RETURN
      K1  = I6X6K + I1*JMAX + J2
      J1  = 0
      L   = 6*L1
      K   = K1
   40 J1  = J1 + 1
      IF (J1 .GT. 6) GO TO 50
      K   = K + 1
      L   = L + 1
      DZ(K) = DZ(K) + KE(L)
      GO TO 40
   50 I1  = I1 + 1
      L1  = L1 + 1
      GO TO 30
      END
