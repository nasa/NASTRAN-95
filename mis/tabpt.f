      SUBROUTINE TABPT
C
C     MODULE DRIVER TO PRINT TABLES
C
      DIMENSION      IN(5),ITRL(7)
      COMMON /BLANK/ OP(2),IRC,IWD
      DATA    IN   / 101,102,103,104,105 /, BLANK / 4H     /
C
      DO 10 I = 1,5
      ITRL(1) = IN(I)
      CALL RDTRL (ITRL(1))
      IF (ITRL(1) .GT. 0) CALL TABPRT (IN(I))
   10 CONTINUE
      OP(1) = BLANK
      OP(2) = BLANK
      IRC = 0
      IWD = 0
      RETURN
      END
