      SUBROUTINE IFP4F (IBIT,FILE,BIT)
C
C     TEST BIT -IBIT- IN TRAILER OF DATA BLOCK -FILE-
C
      EXTERNAL     ANDF
      LOGICAL      BIT
      INTEGER      TWO, TRAIL(7), FILE, ANDF
      COMMON /TWO/ TWO(32)
C
      TRAIL(1) = FILE
      CALL RDTRL (TRAIL)
      I1 = (IBIT-1)/16 + 2
      I2 = IBIT - (I1-2)*16 + 16
      IF (ANDF(TRAIL(I1),TWO(I2))) 10,20,10
   10 BIT = .TRUE.
      RETURN
   20 BIT = .FALSE.
      RETURN
      END
