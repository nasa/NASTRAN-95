      SUBROUTINE SSG2A (PG,PNBAR,PM,PVACT)
C
      INTEGER         PG,PNBAR,PM,PVACT,RULE,PVECT(7),CORE(6)
      COMMON /PARMEG/ IA1(7),IA11(7),IA12(7),IA21(7),IA22(7),LCR,RULE
      COMMON /PATX  / LCORE,N,NO(4)
      COMMON /ZZZZZZ/ ICORE(1)
      EQUIVALENCE     (ICORE(1),CORE(1))
C
C
      PVECT(1)= PVACT
      CALL RDTRL (PVECT)
      IA1(1)  = PG
      CALL RDTRL (IA1)
      IA11(1) = PNBAR
      IA12(1) = PM
      DO 10 I = 2,5
      IA11(I) = IA1(I)
   10 IA12(I) = IA1(I)
      IA11(3) = N
      IA12(3) = NO(1)
      IA21(1) = 0
      IA22(1) = 0
      RULE    = 0
      LCR     = KORSZ(CORE)
      CORE(1) = 0
      CORE(2) = 1
      CORE(3) = IA1(2)
      CORE(4) = 2
      CORE(5) = 1
      CORE(6) = 0
      CALL PARTN (CORE,PVECT,CORE)
      IF (IA11(1) .NE. 0) CALL WRTTRL (IA11)
      IF (IA12(1) .NE. 0) CALL WRTTRL (IA12)
      RETURN
      END
