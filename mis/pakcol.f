      SUBROUTINE PAKCOL(TERMS,NTERMS)
C
C     PACKS OUT A COLUMN OF AF OR DKGG MATRIX - DATA IS IN THE
C     FOLLOWING SAMPLE FORMATS.
C
C                  ---------------------
C                  I  NEGATIVE ROWSIL  I
C                  I-------------------I
C                  I  1 MATRIX TERM    I
C                  I-------------------I
C                  I  POSITIVE ROWSIL  I
C                  I-------------------I
C                  I                   I
C                  I  3 MATRIX TERMS   I
C                  I                   I
C                  ---------------------
C
C     MATRIX TERMS ARE IN DOUBLE PRECISION
C
C
      DOUBLE PRECISION        VAL      ,TVAL
C
      INTEGER       TERMS(1) ,A        ,TEMP(7)
C
C     PACK COMMON BLOCK
C
      COMMON / ZBLPKX /       A(4)     ,IROW
C
      EQUIVALENCE  ( VAL , A(1) )
      EQUIVALENCE  ( TVAL , A(3) )
C
C***********************************************************************
C
C     SORT THE MATRIX ENTRIES BY ABSOULUTE SIL VALUES
C
      ILOC = 1
   10 ISIL = TERMS(ILOC)
      JLOC = ILOC
      JSIL = ISIL
   20 JLOC = JLOC + 3
      IF(JSIL .GT. 0) JLOC = JLOC + 4
      IF(JLOC .GE. NTERMS) GO TO 60
      JSIL = TERMS(JLOC)
      IF(IABS(JSIL) .GE. IABS(ISIL)) GO TO 20
C
      NT = 3
      IF(JSIL .GT. 0) NT = 7
      DO 30 I=1,NT
   30 TEMP(I) = TERMS(JLOC+I-1)
C
      KLOC = JLOC - 1
      DO 40 I=ILOC,KLOC
      J = KLOC - I + ILOC
   40 TERMS(J+NT) = TERMS(J)
C
      DO 50 I=1,NT
   50 TERMS(ILOC+I-1) = TEMP(I)
      ISIL = JSIL
      GO TO 20
C
   60 ILOC = ILOC + 3
      IF(ISIL .GT. 0) ILOC = ILOC + 4
      IF(ILOC .LT. NTERMS) GO TO 10
C
C     PACK OUT TERMS - ADDING ANY IDENTICAL SIL
C
      ILOC = 1
   70 IROW = IABS(TERMS(ILOC))
      NT = 2
      IF(TERMS(ILOC) .GT. 0) NT = 6
C
      DO 100 I=1,NT,2
      A(1) = TERMS(ILOC+I)
      A(2) = TERMS(ILOC+I+1)
      JLOC = ILOC
   80 J = JLOC
      JLOC = J + 3
      IF(TERMS(J) .GT. 0) JLOC = J + 7
      IF(JLOC .GE. NTERMS) GO TO 90
      IF(TERMS(JLOC) .NE. TERMS(ILOC)) GO TO 90
C
C     DUPLICATE SILS - ADD THEM
C
      A(3) = TERMS(JLOC+I)
      A(4) = TERMS(JLOC+I+1)
      VAL = VAL + TVAL
      J = JLOC
      GO TO 80
C
C     PACK OUT TERM
C
   90 CONTINUE
      CALL ZBLPKI
  100 IROW = IROW + 1
C
      ILOC = JLOC
      IF(ILOC .LT. NTERMS) GO TO 70
C
      RETURN
      END
