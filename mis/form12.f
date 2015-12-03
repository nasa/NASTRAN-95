      SUBROUTINE FORM12 (U0, UDOT0, U1, P0, P1, DELTT, IBUF)
C*******
C     FORM12 GENERATES THE STARTING VECTORS FOR THE INTEGRATION MODULE
C
C     THIS ROUTINE IS SUITABLE FOR DOUBLE PRECISION OPERATION
C*******
      DOUBLE PRECISION   U0(1)     ,UDOT0(1) ,U1(1)    ,P0(1)    ,
     1                   P1(1)
C
      DIMENSION          IBUF(1)
C
      COMMON /BLANK/  DUMMY(5)  ,ISTART
      COMMON   /TRDXX /  IFILK(7)  ,IFILM(7) ,IFILB(7)
C
      NROW = IFILK(2)
C*******
C     FORM U(-1)
C*******
      DO 10 I = 1,NROW
      P1(I) = 0.0D0
   10 U1(I) = U0(I)-DELTT*UDOT0(I)
      IF (ISTART.GE.0) GO TO 30
      DO 15 I = 1, NROW
      P0(I) = 0.0D0
   15 CONTINUE
C*******
C     FORM P0
C*******
      CALL MATVC2(U0(1),P0(1),IFILK(1),IBUF)
      CALL MATVC2(UDOT0(1),P0(1),IFILB(1),IBUF)
C*******
C     FORM P(-1)
C*******
      CALL MATVC2(UDOT0(1),P1(1),IFILK(1),IBUF)
      DO 20 I = 1,NROW
   20 P1(I) = P0(I)-DELTT*P1(I)
      RETURN
C
C     ALTERNATE STARTING METHOD
C
   30 CALL MATVC2 (U0(1), P1(1), IFILK(1), IBUF)
      CALL MATVC2 (UDOT0(1), P1(1), IFILB(1), IBUF)
      DO 40 I = 1, NROW
      P0(I) = 0.5D0*(P0(I) + P1(I))
      UDOT0(I) = - UDOT0(I)*DELTT
   40 CONTINUE
C
C     ADD UDOT CONTRIBUTION
C
      CALL MATVC2 (UDOT0(1), P1(1), IFILK(1), IBUF)
C
C     RESTORE UDOT
C
      DO 50 I = 1, NROW
      UDOT0(I) = - UDOT0(I)/DELTT
   50 CONTINUE
      RETURN
      END
