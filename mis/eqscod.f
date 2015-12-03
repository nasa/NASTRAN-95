      SUBROUTINE EQSCOD (LOC,N,Z)
C
      EXTERNAL  LSHIFT,ORF
      INTEGER   Z(1),ORF
C
      I   = LOC
      MEND= LOC+N-1
    1 IST = I
      NG  = 1
    2 CONTINUE
      IF (I .GE. MEND-2) GO TO 3
      IF (Z(I+3) .NE. Z(IST)) GO TO 3
      NG = NG+1
      I  = I+3
      GO TO 2
    3 CONTINUE
      IF (NG .NE. 1) GO TO 4
      I = I+3
      IF (I .GE. MEND-2) GO TO 6
      GO TO 1
    4 DO 5 J=1,NG
      ILOC  = IST+3*(J-1)
      ICODE = 8*J+NG
      INEW  = LSHIFT(ICODE,26)
      Z(ILOC+2) = ORF(Z(ILOC+2),INEW)
    5 CONTINUE
      I = I+3
      IF (I .GE. MEND-2) GO TO 6
      GO TO 1
    6 CONTINUE
      RETURN
      END
