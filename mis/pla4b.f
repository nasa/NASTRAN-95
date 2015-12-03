      SUBROUTINE PLA4B (KE,J)
C*****
C THIS ROUTINE IS THE INSERTION ROUTINE FOR THE PLA4 MODULE.  IT ADDS
C THE 6 X 6 DOUBLE PRECISION MATRIX KE TO THE SUBMATRIX OF ORDER
C 6 X JMAX
C*****
      DOUBLE PRECISION
     1                   DZ(1)              ,KE(36)
C
C
C
      INTEGER
     1                   FROWIC             ,IZ(1)
C
C VARIABLE CORE
C
      COMMON   /ZZZZZZ/
     1                   Z(1)
C
C PLA42 COMMUNICATIONS BLOCK
C
      COMMON   /PLA42C/
     1                   IDUM5(6)
     2,                  IGPCT              ,NGPCT
     3,                  IPOINT             ,NPOINT
     4,                  I6X6K              ,N6X6K
     5,                  IDUM11(12)
     6,                  JMAX               ,FROWIC
     7,                  LROWIC             ,NROWSC
     8,                  IDUM(121)
C
C
C
      EQUIVALENCE
     1                   (DZ(1),Z(1),IZ(1))
C
C SEARCH THE GPCT AND FIND AN INDEX M SUCH THAT
C IABS(GPCT(M)) .LE. J .LT. IABS(GPCT(M+1))
C
      LOW = IGPCT + 1
      LIM = NGPCT + LOW - 2
      IF (LOW .GT. LIM) GO TO 15
      DO 10 I = LOW,LIM
      ISAVE = I
      IF (J .GE. IABS(IZ(I+1)) ) GO TO 10
      IF (J .GE. IABS(IZ(I)) ) GO TO 20
   10 CONTINUE
      IF (J .GE. IABS(IZ(ISAVE+1)) ) ISAVE = ISAVE + 1
      GO TO 20
   15 ISAVE = LOW
C
C ADD KE TO THE SUBMATRIX
C
   20 L1 = FROWIC - 1
      JJ = IPOINT + ISAVE - IGPCT
      J2 = IZ(JJ) - 1
      I1 = 0
      LIM = NROWSC - 1
   30 IF (I1 .GT. LIM) RETURN
      K1 = I6X6K + I1*JMAX + J2
      J1 = 0
      L  = 6*L1
      K  = K1
   40 J1 = J1 + 1
      IF (J1 .GT. 6) GO TO 50
      K  = K + 1
      L  = L + 1
      DZ(K) = DZ(K) + KE(L)
      GO TO 40
   50 I1 = I1 + 1
      L1 = L1 + 1
      GO TO 30
      END
