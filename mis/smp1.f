      SUBROUTINE SMP1
C
C     SMP1 PARTITIONS KFF INTO KAAB,KOAB AND KOOB
C     GO IS SOLVED FROM THE EQUATION KOOB*GO = -KOAB
C     KAA IS THEN COMPUTED FROM THE EQUATION KAA = KAAB + KOAB(T)*GO
C     IF ANY OF THE MATRICES MFF, BFF OR K4FF IS PRESENT, THEN
C     PARTITIONS ARE MADE AND THE EQUATION
C     XAA = XAAB + GO(T)*XOAB + XOAB(T)*GO + GO(T)*XOOB*GO
C     IS EVALUATED WHERE X = M OR B OR K4
C
      INTEGER         MCB(7), USET,   SCR1,   SCR2,    SCR3,
     1                SCR4,   SCR5,   SCR6,   SCR7,    UF,
     2                UA,     GO,     FF(3),  AAB,     OAB,
     3                OOB,    AA(3),  BAA,    IOAB(3), IOOB(3),
     4                UO,     BFF
      COMMON /BITPOS/ UM,     UO,     UR,     USG,     USB,    UL,
     1                UA,     UF,     US,     UN,      UG,     UE,
     2                UP
      EQUIVALENCE     (FF(3),MFF), (FF(2),BFF), (FF(1),K4FF),
     1                (AA(3),MAA), (AA(2),BAA), (AA(1),K4AA),
     2                (IOOB(3),OOB), (IOAB(3),OAB), (AAB,SCR5),
     3                (IOOB(1),SCR6), (IOAB(1),SCR7)
      DATA            USET,   KFF,    MFF,    BFF,     K4FF  /
     1                101,    102,    103,    104,     105   /
      DATA            GO,     KAA,    KOOB,   LOO,     MAA   /
     1                201,    202,    203,    204,     205   /
      DATA            OOB,    OAB,    BAA,    K4AA           /
     1                206,    207,    208,    209            /
      DATA            SCR1,   SCR2,   SCR3,   SCR4,    SCR5  /
     1                301,    302,    303,    304,     305   /
      DATA            SCR6,   SCR7,   IOOB(2),IOAB(2)        /
     1                306,    307,    306,    307            /
C
C     MATRIX NAME EQUIVALENCES NOT REFERENCED
C
C     EQUIVALENCED    (AAB,   MAAB,   BAAB,   K4AAB,  SCR5)
C                     (OOB,   MOOB,   BOOB,   K4OOB,  SCR6)
C                     (OAB,   MOAB,   BOAB,   K4OAB,  SCR7)
C
C     PARTITION KFF INTO KAAB,KOAB, AND KOOB
C
      CALL UPART (USET,SCR1,UF,UA,UO)
      CALL MPART (KFF,AAB,IOAB,0,KOOB)
C
C     DECOMPOSE KOOB INTO LOO
C
      CALL FACTOR (KOOB,LOO,SCR2,SCR3,SCR4,SCR6)
C
C     SOLVE KOOB*GO = -KOAB FOR GO
C     THEN COMPUTE  KAA = KAAB + KOAB(T)*GO
C
      CALL SOLVER (LOO,GO,IOAB,AAB,KAA,0,0,SCR4)
C
      DO 40 I = 1,3
C
C                     K4FF
C     TEST TO SEE IF   BFF  IS PRESENT
C                      MFF
C
      MCB(1) = FF(I)
      CALL RDTRL (MCB)
      IF (MCB(1) .LT. 0) GO TO 40
C
C         K4FF                              K4AAB, K4OAB, K4OOB
C     IF   BFF IS PRESENT, PARTITION INTO    BAAB,  BOAB,  BOOB
C          MFF                               MAAB,  MOAB,  MOOB
C
C     THEN COMPUTE  K4AA, BAA, MAA  RESPECTIVELY
C
      CALL MPART (FF(I),AAB,IOAB(I),0,IOOB(I))
      CALL ELIM  (AAB,IOAB(I),IOOB(I),GO,AA(I),SCR2,SCR3,SCR4)
   40 CONTINUE
C
      RETURN
      END
