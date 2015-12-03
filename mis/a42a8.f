      SUBROUTINE A4 2 A8 (A,B,C)
C
C     MERGES TWO A4 BCD WORDS (A AND B) TO ONE A8 BCD WORD (C)
C
      CHARACTER*4     KA,    KB
      CHARACTER*8     KC,    D
      REAL            A,     B
      REAL            C(2)
      COMMON /SYSTEM/ DUMMY(40), NCPW
C
      WRITE (D,10) A,B
      IF (NCPW .LT. 8) READ (D,10) C(1),C(2)
      IF (NCPW .GE. 8) READ (D,20) C(1)
 10   FORMAT (2A4)
 20   FORMAT ( A8)
      RETURN
C
C
      ENTRY A4 2 K8 (A,B,KC)
C     ======================
C
C     MERGES TWO A4 BCD WORDS (A AND B) TO ONE A8 CHARACTER WORD (KC)
C
      WRITE (KC,10) A,B
      RETURN
C
C
      ENTRY A4 2 K4 (A,KA,NOTUSE)
C     ===========================
C
C     CONVERTS ONE A4 BCD WORD (A) TO ONE A4 CHARACTER WORD (KA)
C
      WRITE  (KA,30) A
 30   FORMAT (A4)
      RETURN
C
C
      ENTRY A8 2 K8 (C,KC,NOTUSE)
C     ===========================
C
C     CONVERTS ONE A8 BCD WORD (C) TO ONE A4 CHARACTER WORD (KC)
C
      IF (NCPW .LT. 8) WRITE (KC,10) C(1),C(2)
      IF (NCPW .GE. 8) WRITE (KC,20) C(1)
      RETURN
C
C
      ENTRY K4 2 K8 (KA,KB,KC)
C     ========================
C
C     MERGES TWO A4 CHARACTER WORDS (KA AND KB) TO ONE A8 CHARACTER
C     WORD (KC)
C
C     NOTE - SOME MACHINES, SUCH AS UNIVAC, HANDLE BCD WORD AND
C            CHARACTER WORD QUIT DIFFERENTLY
C
      WRITE (KC,10) KA,KB
      RETURN
C
C
      ENTRY K4 2 A8 (KA,KB,C)
C     =======================
C
C     MERGES TWO A4 CHARACTER WORDS (KA AND KB) TO ONE A8 BCD WORD (C)
C
      WRITE (D,10) KA,KB
      IF (NCPW .LT. 8) READ (D,10) C(1),C(2)
      IF (NCPW .GE. 8) READ (D,20) C(1)
      RETURN
C
C
      ENTRY K4 2 A4 (KA,A,NOTUSE)
C     ===========================
C
C     CONVERTS ONE A4 CHARACTER WORD (KA) TO ONE A4 BCD WORD (A)
C
      READ (KA,30) A
      RETURN
C
C
      ENTRY K8 2 A8 (KC,C,NOTUSE)
C     ===========================
C
C     CONVERTS ONE A8 CHARACTER WORD (KC) TO ONE A8 BCD WORD (C)
C
      IF (NCPW .LT. 8) READ (KC,10) C(1),C(2)
      IF (NCPW .GE. 8) READ (KC,20) C(1)
      RETURN
C
      END
