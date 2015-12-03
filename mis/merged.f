      SUBROUTINE MERGED (A11,A12,A21,A22,A,RP,CP,N1,N2)
C
      INTEGER         A11,A12,A21,A22,A,RP,CP,RULE,MCB(20),MCB1(20)
      COMMON /PARMEG/ MCBA(7),MCBA11(7),MCBA21(7),MCBA12(7),MCBA22(7),
     1                NX,RULE
      COMMON /ZZZZZZ/ IZ(1)
C
      IF (RP .NE. 0) GO TO 10
      MCB(1) = 0
      MCB(2) = 1
      MCB(3) = N1
      MCB(4) = 2
      MCB(5) = 1
      GO TO 20
C
   10 MCB(1) = RP
      CALL RDTRL (MCB)
   20 IF (CP .NE. 0) GO TO 30
      MCB1(1) = 0
      MCB1(2) = 1
      MCB1(3) = N2
      MCB1(4) = 2
      MCB1(5) = 1
      GO TO 40
C
   30 MCB1(1) = CP
      CALL RDTRL (MCB1)
   40 NX    = KORSZ (IZ)
      RULE  = 0
      IOTYP = 0
      MCBA11(1) = A11
      IF (A11 .EQ. 0) GO TO 50
      CALL RDTRL (MCBA11)
      IF (MCBA11(1) .LE. 0) MCBA11(1) = 0
C
   50 MCBA21(1) = A21
      IF (A21 .EQ. 0) GO TO 60
      CALL RDTRL (MCBA21)
      IF (MCBA21(1) .LE. 0) MCBA21(1) = 0
C
   60 MCBA12(1) = A12
      IF (A12 .EQ. 0) GO TO 70
      CALL RDTRL (MCBA12)
      IF (MCBA12(1) .LE. 0) MCBA12(1) = 0
C
   70 MCBA22(1) = A22
      IF (A22 .EQ. 0) GO TO 80
      CALL RDTRL (MCBA22)
      IF (MCBA22(1) .LE. 0) MCBA22(1) = 0
C
   80 MCBA(1) = A
      MCBA(2) = MCB(3)
      MCBA(3) = MCB1(3)
      DO 90 I = 1,28,7
      IF (MCBA11(I) .EQ. 0) GO TO 90
      IOTYP   = MAX0(IOTYP,MCBA11(I+4))
   90 CONTINUE
      MCBA(4) = 2
      MCBA(5) = IOTYP
      IF (MCBA(2) .EQ. MCBA(3)) MCBA(4) = 1
      CALL MERGE (MCB,MCB1,IZ)
      CALL WRTTRL (MCBA)
      RETURN
      END
