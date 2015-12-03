      SUBROUTINE UPART (USET,SCR1,MAJOR,SUB0,SUB1)
C
C     UPART ALONG WITH MPART WILL PERFORM A SYMMETRIC PARTITION OF A
C     MATRIX
C
      INTEGER         USET,SCR1,MAJOR,SUB0,SUB1,RULE,PVECT,USET1
C
      COMMON /PARMEG/ IA(7),IA11(7),IA12(7),IA21(7),IA22(7),LCORE,RULE
      COMMON /PATX  / LC,N1,N2,N3,USET1,PVECT(7)
      COMMON /ZZZZZZ/ CORE(1)
C
C
      USET1 = USET
C
C     TRANSFER OF PVECT TRAILER AS LOADED BY CALCV IS NOW BY /PATX/
C
      RULE  = 0
      LC    = KORSZ(CORE)
      LCORE = LC
      CALL CALCV (SCR1,MAJOR,SUB0,SUB1,CORE)
      N4 = N2 + N3
      IA11(2) = N1
      IA11(3) = N1
      IA21(2) = N4
      IA21(3) = N1
      IA21(4) = 2
      IA12(2) = N1
      IA12(3) = N4
      IA12(4) = 2
      IA22(2) = N4
      IA22(3) = N4
   10 RETURN
C
C
      ENTRY MPART (IA1,IA111,IA121,IA211,IA221)
C     =========================================
C
      IA(1) = IA1
      CALL RDTRL (IA)
      IF (IA(1)) 10,20,20
   20 IA11(1) = IA111
      IA12(1) = IA121
      IA21(1) = IA211
      IA22(1) = IA221
      IA11(4) = IA(4)
      IA11(5) = IA(5)
      IA21(5) = IA(5)
      IA12(5) = IA(5)
      IA22(4) = IA(4)
      IA22(5) = IA(5)
      CALL PARTN (PVECT,PVECT,CORE)
      DO 40 I = 1,4
      J = (I-1)*7 + 1
      IF (IA11(J)) 30,40,30
   30 CALL WRTTRL (IA11(J))
   40 CONTINUE
      GO TO 10
      END
