      SUBROUTINE READ6 (IRIG,GPHIA,NR,PHIA)
C
C     ADDS GIVENS EIGENVECTORS TO RIGID BODY MODES ON PHIA
C
      INTEGER         GPHIA,SYSBUF,PHIA,MCB(7),FILE
      REAL            Z(3)
      COMMON /SYSTEM/ SYSBUF
      COMMON /ZZZZZZ/ IZ(1)
      COMMON /UNPAKX/ IT2U,IIU,JJU,INCR1U
      COMMON /PACKX / IT1,IT2,II,JJ,INCR1
      EQUIVALENCE     (IZ(1),Z(1))
C
C
      IBUF1 =  KORSZ(Z) - SYSBUF + 1
      IBUF2 =  IBUF1 - SYSBUF
      MCB(1)= GPHIA
      CALL RDTRL (MCB)
      NCOL = MCB(2) - NR
      II   = 1
      JJ   = MCB(3)
      IT1  = MCB(5)
      IT2  = MCB(5)
      IT2U = MCB(5)
      CALL MAKMCB (MCB,PHIA,JJ,MCB(4),IT1)
      INCR1 = 1
      CALL GOPEN (PHIA,Z(IBUF1),1)
      IF (NR .EQ. 0) GO TO 21
      FILE = IRIG
      CALL GOPEN (IRIG,Z(IBUF2),0)
      Z(1) = 0.0
      Z(2) = 0.0
      DO 20 I = 1,NR
      IIU = 0
      CALL UNPACK (*11,IRIG,Z(3))
      II = IIU
      JJ = JJU
      CALL PACK (Z(3),PHIA,MCB)
      GO TO 20
   11 II = 1
      JJ = 1
      CALL PACK (Z,PHIA,MCB)
   20 CONTINUE
      CALL CLOSE (IRIG,1)
   21 CONTINUE
      IF (NCOL .LE. 0) GO TO 31
      CALL GOPEN (GPHIA,Z(IBUF2),0)
      FILE = GPHIA
      INCR1U = 1
      Z(1) = 0.0
      Z(2) = 0.0
      CALL SKPREC (GPHIA,NR)
      DO 30 I = 1,NCOL
      IIU = 0
      CALL UNPACK (*35,GPHIA,Z(3))
      II = IIU
      JJ = JJU
      CALL PACK (Z(3),PHIA,MCB)
      GO TO 30
   35 II = 1
      JJ = 1
      CALL PACK (Z,PHIA,MCB)
   30 CONTINUE
      CALL CLOSE (GPHIA,1)
   31 CALL CLOSE (PHIA,1)
      CALL WRTTRL (MCB)
      RETURN
      END
