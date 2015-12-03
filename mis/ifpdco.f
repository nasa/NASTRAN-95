      LOGICAL FUNCTION IFPDCO (IC)
C
C     DECODE D.O.F. INTO LL SPACE.
C     RETURN WITH IFPDCO=.TRUE. IF ERROR ENCOUNTERED
C     FOR EXAMPLE - GIVEN IC=124, THEN
C                   LL(1)=1,   LL(2)=2,  LL(4)=4, LL(3)=LL(5)=LL(6)=0
C                   GC(1)=124, GC(2)=12, GC(3)=1, GC(4)=GC(5),GC(6)=0
C                   IFPDCO=.FALSE.
C
      INTEGER DG,GC
      COMMON /IFPDTA/ DUM(521),GC(7),LL(6)
      COMMON /SYSTEM/ IDUMMY(55),ITHRML
C
      GC(1) = IC
      DO 110 LC=1,6
  110 LL(LC) = 0
      IF (IC) 120,116,112
  112 DO 114 LC=1,6
      GC(LC+1) = GC(LC)/10
      DG = GC(LC)-10*GC(LC+1)
      IF (ITHRML.NE.1 .AND. DG.GT.6) GO TO 120
      IF (ITHRML.EQ.1 .AND. DG.GT.1) GO TO 120
      IF (DG .EQ. 0) GO TO 118
      IF (LL(DG) .NE. 0) GO TO 120
  114 LL(DG) = DG
      IF (GC(7) .NE. 0) GO TO 120
  116 IFPDCO = .FALSE.
      RETURN
  118 IF (GC(LC) .EQ. 0) GO TO 116
  120 IFPDCO = .TRUE.
      RETURN
      END
