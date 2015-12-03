      SUBROUTINE OUTPAK (II,IOUT,ISN)
C
      EXTERNAL        ORF
      INTEGER         IOUT(1),NUMBER(10),IDIG(4),ORF,OP
      COMMON /SYSTEM/ KSYS(65)
      EQUIVALENCE     (KSYS(2),OP),(KSYS(9),NLPP),(KSYS(12),NLINE),
     1                (KSYS(41),NCPW)
      DATA    NUMBER/ 1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9,1H0/
      DATA    NBLANK/ 4H    /
C
      KCODE = 0
      IF (ISN .LT. 0) KCODE = 1
      ISN = IABS(ISN)
      ICODE = 0
      IF (II .GT. 32) ICODE = 1
      IF (ICODE .EQ. 1) GO TO 50
C
C     TRANSLATE ISN TO DIGITS
C
   10 IDIG(1) = ISN/1000
      IDIG(2) = (ISN-IDIG(1)*1000)/100
      IDIG(3) = (ISN-IDIG(1)*1000 - IDIG(2)*100)/10
      IDIG(4) = ISN - IDIG(1)*1000 - IDIG(2)*100 - IDIG(3)*10
      DO 20 I = 1,4
      IF (IDIG(I) .EQ. 0) IDIG(I) = 10
   20 CONTINUE
C
C     FORM WORD AND STORE IN IOUT ARRAY
C
      K = 0
      DO 30 I = 1,4
      J = IDIG(I)
      K = ORF(KLSHFT(KRSHFT(NUMBER(J),NCPW-1),NCPW-I),K)
   30 CONTINUE
      IOUT(II) = K
      GO TO 80
   50 NLINE = NLINE + 1
      IF (NLINE .LE. NLPP) GO TO 60
      CALL PAGE
      NLINE = NLINE + 1
      WRITE (OP,100)
      NLINE = NLINE + 1
   60 WRITE (OP,90) (IOUT(I),I=2,32)
      II = 5
      IF (KCODE .EQ. 1) II = 7
      DO 70 LL = 2,32
      IOUT(LL) = NBLANK
   70 CONTINUE
      GO TO 10
   80 RETURN
C
   90 FORMAT (5X,31A4)
  100 FORMAT (/,1H )
      END
