      SUBROUTINE MCE1
C
C     MCE1 PARTITIONS RG INTO RM AND RN
C     THEN SOLVES THE MATRIX EQUATION RM * GM = -RN.
C
C
      INTEGER        USET   ,RG    ,GM    ,SCR1  ,SCR2  ,SCR3  ,RM   ,
     1               RN     ,L     ,U
      COMMON /BLANK/ USET   ,RG    ,GM    ,SCR1  ,SCR2  ,SCR3  ,RM   ,
     1               RN     ,L     ,U     ,MCB(7)
C
C     SET INPUT, OUTPUT AND SCRATCH FILES
C
      USET = 101
      RG   = 102
      GM   = 201
      SCR1 = 304
      SCR2 = 305
      SCR3 = 301
      RM   = 302
      RN   = 303
      L    = 306
      U    = 307
C
C     PARTITION RG INTO RM AND RN
C
      CALL MCE1A
C
C     TEST FOR RM DIAGONAL
C
      MCB(1) = RM
      CALL RDTRL (MCB)
      IF (MCB(5).EQ.1 .AND. MCB(6).EQ.1) GO TO 50
      IF (MCB(5).EQ.2 .AND. MCB(6).EQ.2) GO TO 50
C
C     RM IS NOT DIAGONAL, DECOMPOSE RM THEN SOLVE FOR GM
C     BY FORWARD-BACKWARD SUBSTITUTION.
C
      CALL MCE1B
      CALL MCE1C
      RETURN
C
C     RM IS DIAGONAL, COMPUTE GM = -RM(-1) * RN
C
   50 CALL MCE1D
      RETURN
      END
