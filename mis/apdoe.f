      SUBROUTINE APDOE(ID,Z,START,END,FOUND,COUNT)
C
C     APDOE FINDS AND OPEN ENDED CARD FOR ID
C     GIVEN A LIST Z(START ) TO Z(END)
C     FOUND = 0 IF NOT FOUND
C     FOUND = POINTER TO START OF CARD Z(FOUND)
C     COUNT = NUMBER OF DATA ITEMS NOT COUNTING THE ID
C
      INTEGER START,END,FOUND,COUNT,Z(1)
      LOGICAL LOOK
      FOUND = 0
      LOOK = .TRUE.
      COUNT = 0
      IF(START.EQ.0) GO TO 50
      DO 10 I = START,END
      IF(LOOK) GO TO 20
      IF(Z(I).EQ.-1) LOOK = .TRUE.
      GO TO 10
   20 IF(Z(I).EQ.ID) GO TO 30
      LOOK = .FALSE.
   10 CONTINUE
      GO TO 50
   30 FOUND = I
      J = I + 2
      COUNT = COUNT + 1
C
C     START COUNT AT + 2 BECAUSE PAERO4 CARD CAN HAVE -1 IN FIELD 2
C
      DO 40 I=J,END
      IF(Z(I).EQ.-1) GO TO 50
      COUNT = COUNT + 1
   40 CONTINUE
   50 RETURN
      END
