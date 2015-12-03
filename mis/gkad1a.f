      SUBROUTINE GKAD1A (USETD,GO,GOD,SCR1,UE,UA,UD)
C
C     GKAD1A WILL EXPAND GO BY NULL MATRIX TO MAKE GOD, AND
C     AA-S TO D-S ADDING ZEROS FOR E-S
C
      INTEGER USETD,USET1,GO,GOD,IPV1(7),SCR1,CORE,BAA,B1DD
      COMMON /PATX  / LC,N,NO,N4,USET1
      COMMON /ZZZZZZ/ CORE(1)
      COMMON /PARMEG/ IA(7),IA11(7),IA12(7),IB11(7),IB12(7),NZ,IRULE
      COMMON /SYSTEM/ IDUM(54),IPREC
C
C
      IENT = 0
C
C     COMPUTE CORE FOR CALCV AND MERGE
C
   20 LC = KORSZ(CORE)
C
C     BUILD PART VECTOR
C
      USET1 = USETD
      CALL CALCV (SCR1,UD,UA,UE,CORE(1))
C
C     SET UP FOR MERGE
C
      NZ = LC
      IRULE = 0
      DO 10 I = 1,7
      IA11 (I) = 0
      IA   (I) = 0
      IA12 (I) = 0
      IB11 (I) = 0
      IB12 (I) = 0
   10 CONTINUE
      IPV1(1) = SCR1
      CALL RDTRL (IPV1)
      IF (IENT .NE. 0) GO TO 30
C
C     SET UP FOR 2 WAY MERGE
C
      IA11(1) = GO
      CALL RDTRL (IA11)
      IA(1) = GOD
      IA(2) = N+NO+N4
      IA(3) = IA11(3)
      IA(4) = IA11(4)
      IA(5) = IA11(5)
C     BUILD NULL COLUMN IN CORE
      I = 0
      CORE(  1) = 0
      CORE(I+2) = 1
      CORE(I+3) = IA(3)
      CORE(I+4) = 2
      CORE(I+5) = 1
      CORE(I+6) = 0
      CORE(I+7) = 0
      CALL MERGE (IPV1(1),CORE(1),CORE(1))
      CALL WRTTRL (IA)
   40 RETURN
C
C
      ENTRY GKAD1B (USETD,KAA,MAA,BAA,K4AA,K1DD,M1DD,B1DD,K41DD,UA,UE,
     1              UD,SCR1)
C     ================================================================
C
      IENT = 1
      GO TO 20
C
C     VECTOR MADE, SET UP MCB-S
C
   30 IA(2) = N+NO+N4
      IA(3) = IA(2)
      IA(4) = 6
      IA(5) = IPREC
      IA11(1) = KAA
      IA(1) = K1DD
      IOUT  = 1
      CALL RDTRL (IA11)
      IF (IA11(1) .GT. 0) GO TO 35
      K1DD = 0
      GO TO 31
   35 CALL MERGE (IPV1(1),IPV1(1),CORE(1))
      CALL WRTTRL (IA)
   31 GO TO (32,33,34,40), IOUT
   32 IOUT  = 2
      IA(1) = B1DD
      IA11(1) = BAA
      CALL RDTRL (IA11)
      IF (IA11(1) .GT. 0) GO TO 35
      B1DD = 0
      GO TO 31
   33 IOUT  = 3
      IA(1) = M1DD
      IA11(1) = MAA
      CALL RDTRL (IA11)
      IF (IA11(1) .GT. 0) GO TO 35
      M1DD = 0
      GO TO 31
   34 IOUT  = 4
      IA(1) = K41DD
      IA11(1) = K4AA
      CALL RDTRL (IA11)
      IF (IA11(1) .GT. 0) GO TO 35
      K41DD = 0
      GO TO 31
      END
