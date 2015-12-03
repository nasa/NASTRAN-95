      SUBROUTINE BUG (NAME,LOC,BUF,NWDS)
C
C     THIS ROUTINE PRINTS NAME,LOC, AND CONTENT OF BUF ARRAY
C     E.G.   CALL BUG ('SUBR ABC',105,CORE(1),120)
C     LIMITED TO 5000 LINES EACH CALL,  14 VALUES PER LINE
C
C     (THIS ROUTINE REPLACES THE OLD ONE IN NASTRAN)
C     WRITTEN BY G.CHAN/SPERRY     MARCH 1986
C
      REAL            BUF(1),   NAME(3)
      CHARACTER*4     A(28),    XLOC,     BLANK
      CHARACTER*8     B(14),    ZERO,     ERR
      COMMON /SYSTEM/ IBUF,     NOUT
      EQUIVALENCE     (A(1),B(1))
      DATA    LINE,   NWPL,     LIMIT              /
     1        0,      14,       5000               /
      DATA    ZERO,   BLANK,    XLOC,     ERR      /
     1        ' 00 ', '    ',   'LOC',    '(ERR)'  /
C
      CALL SSWTCH (20,L)
      IF (L .EQ. 0) RETURN
      GO TO 5
C
      ENTRY BUG1 (NAME,LOC,BUF,NWDS)
C     ==============================
C
 5    IF (NWDS .LT. 0) RETURN
      L = 2
      I = 0
      CALL A42K8 (NAME(1),NAME(2),B(1))
      CALL INT2K8 (*20,LOC,A(3))
      A(4) = A(3)
      A(3) = XLOC
C
 10   IF (I .GE. NWDS) GO TO 60
 15   I = I + 1
      L = L + 1
      J = NUMTYP(BUF(I)) + 1
      GO TO (  25, 30,  35, 40), J
C            ZERO,INT,REAL,BCD
 20   B(L) = ERR
      GO TO 55
 25   B(L) = ZERO
      GO TO 55
 30   CALL INT2K8 (*20,BUF(I),B(L))
      GO TO 55
 35   CALL FP2K8  (*20,BUF(I),B(L))
      GO TO 55
 40   CALL A42K8 (BUF(I),BUF(I+1),B(L))
      IF (NUMTYP(BUF(I+1)) .NE. 3) GO TO 45
      I = I + 1
      GO TO 50
 45   A(L*2) = BLANK
 50   IF (I .GE. NWDS) GO TO 60
 55   IF (L .LT. NWPL) GO TO 10
 60   IF (L .GT. 0) WRITE (NOUT,65) (B(J),J=1,L)
 65   FORMAT (2X,14(A8,1X))
      LINE = LINE + 1
      IF (LINE .GT. LIMIT) GO TO 70
      L = 0
      IF (I .LT. NWDS) GO TO 15
      RETURN
C
 70   WRITE  (NOUT,75) LIMIT
 75   FORMAT (/2X,'PRINT LINES IN BUG EXCEEDS LIMIT OF',I6)
      RETURN
      END
