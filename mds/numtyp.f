      FUNCTION NUMTYP ( IVALUE )
C
      CHARACTER * 2  BYTE(4)
      CHARACTER * 8  WORD

C
      EQUIVALENCE    ( BYTE, WORD )
C
C      WRITE(6,40646) IVALUE
40646 FORMAT(' NUMTYP,IVALUE=',Z9)
      IF ( IVALUE .EQ. 0 ) GO TO 200
      WRITE ( WORD, 2000 ) IVALUE
      IF ( BYTE(1) .EQ. '  ' ) GO TO 210
      IF ( BYTE(1) .EQ. '00' ) GO TO 210
      IF ((BYTE(1) .EQ. '07'.OR. BYTE(1) .EQ. ' 7') .AND.
     &     BYTE(2) .EQ. 'FF' .AND.   
     &     BYTE(3) .EQ. 'FF' .AND.   
     &     BYTE(4) .EQ. 'FF' )  GO TO 210
      IF ( BYTE(1) .EQ. '7F' .AND.
     &     BYTE(2) .EQ. 'FF' .AND.   
     &     BYTE(3) .EQ. 'FF' .AND.   
     &     BYTE(4) .EQ. 'FF' )  GO TO 210

      IF ( BYTE(1) .EQ. 'FF' ) GO TO 210
      DO 100 I = 1, 4
      IF ( BYTE(I) .LT. '1F' .OR. BYTE(I) .GT. '5E' ) GO TO 220
100   CONTINUE
      GO TO 230
C
C     VALUE IS ZERO
C
200   NUMTYP = 0
      GO TO 700
C
C     VALUE IS INTEGER
C
210   NUMTYP = 1
      GO TO 700
C
C     VALUE IS REAL
C
220   NUMTYP = 2
      GO TO 700
C
C     VALUE IS ALPHA
C
230   NUMTYP = 3
C
700   CONTINUE
      RETURN
C*****
2000  FORMAT(Z8)
C*****
      END
