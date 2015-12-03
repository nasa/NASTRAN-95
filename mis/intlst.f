      SUBROUTINE INTLST (LIST,N,SIGN,N1,N2)
C
      INTEGER LIST(1),SIGN,TO,THRU
      DATA    TO,THRU/ 2HTO,4HTHRU /
C
      SIGN = ISIGN(1,LIST(N))
      N1 = IABS(LIST(N))
      IF (LIST(N+1).EQ.TO .OR. LIST(N+1).EQ.THRU) GO TO 110
      N2 = N1
      N  = N + 1
      GO TO 150
C
  110 N2 = IABS(LIST(N+2))
      N  = N + 3
      IF (N1 .LE. N2) GO TO 150
      I  = N1
      N1 = N2
      N2 = I
C
  150 RETURN
      END
