      SUBROUTINE BISLOC (*,ID,ARR,LEN,KN,JLOC)
C-----
C     BINARY SEARCH - LOCATE KEY WORD 'ID' IN ARRAY 'ARR', 1ST ENTRY
C     IF FOUND, 'JLOC' IS THE MATCHED POSITION IN 'ARR'
C     IF NOT FOUND, NON-STANDARD RETURN
C                                                     I.E.
C     ID  = KEY WORD TO MATCH IN ARR.      MATCH AGAINST 1ST COL OF ARR
C     ARR = ARRAY TO SEARCH.                          ARR(ROW,COL)
C     LEN = LENGTH OF EACH ENTRY IN ARRAY.            LEN=ROW
C     KN  = NUMBER OF ENTRIES IN THE ARR.             KN =COL
C     JLOC= POINTER RETURNED - FIRST WORD OF ENTRY.   MATCHED ROW
C-----
C
      INTEGER  ARR(1)
      DATA     ISWTCH / 16 /
C
      JJ = LEN - 1
      IF (KN .LT. ISWTCH) GO TO 120
      KLO = 1
      KHI = KN
   10 K   = (KLO+KHI+1)/2
   20 J   = K*LEN - JJ
      IF (ID-ARR(J)) 30,90,40
   30 KHI = K
      GO TO 50
   40 KLO = K
   50 IF (KHI-KLO -1) 100,60,10
   60 IF (K .EQ. KLO) GO TO 70
      K   = KLO
      GO TO 80
   70 K   = KHI
   80 KLO = KHI
      GO TO 20
   90 JLOC = J
      RETURN
  100 JLOC = KHI*LEN - JJ
      J    = KN *LEN - JJ
      IF (ID .GT.ARR(J)) JLOC = JLOC + LEN
  110 RETURN 1
C
C     SEQUENTIAL SEARCH MORE EFFICIENT
C
  120 KHI = KN*LEN - JJ
      DO 130 J = 1,KHI,LEN
      IF (ARR(J)-ID) 130,90,140
  130 CONTINUE
      JLOC = KHI + LEN
      GO TO 110
  140 JLOC = J
      GO TO 110
      END
