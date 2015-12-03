      SUBROUTINE CINVP2 (*)
C
C     CINVP2 INITIALIZES AND CALLS CDCOMP FOR CINVPR
C
      INTEGER            FILEA     ,FILEL    ,FILEU    ,SCR1     ,
     1                   SCR2      ,SCR3     ,SCR4     ,SCR5     ,
     2                   SCR6      ,SCR7     ,SCR8     ,SWITCH   ,
     3                   SR1FIL    ,SR2FIL   ,SR3FIL   ,SYSBUF   ,
     5                   DUMM      ,CDP      ,SCR9
      DOUBLE PRECISION   DET       ,MINDIA
      COMMON   /CDCMPX/  FILEA(7)  ,FILEL(7) ,FILEU(7), SR1FIL   ,
     1                   SR2FIL    ,SR3FIL   ,DET(2)   ,POWER    ,
     2                   NZ        ,MINDIA   ,IB
      COMMON   /CINVXX/  DUM(4)    ,SWITCH
      COMMON   /CINVPX/  DUMM(36)  ,SCR1     ,SCR2     ,SCR3     ,
     1                   SCR4      ,SCR5     ,SCR6     ,SCR7     ,
     2                   SCR8      ,SCR9
      COMMON   /ZZZZZZ/  Z(1)
      COMMON   /NAMES /  IJ(10)    ,CDP
      COMMON / SYSTEM /  SYSBUF
C
      IOFF     = FILEU(7)
      FILEA(1) = SCR1
      IF (SWITCH .EQ. 0) GO TO 10
      FILEL(1) = SCR8
      FILEU(1) = SCR9
      IF (SWITCH .LT. 0) FILEA(1) = -FILEA(1)
      IF (SWITCH .EQ. -204) GO TO 20
      SWITCH   = 1
      GO TO 20
   10 FILEL(1) = SCR3
      FILEU(1) = SCR4
   20 SR1FIL   = SCR5
      SR2FIL   = SCR6
      SR3FIL   = SCR7
      FILEA(2) = DUMM(3)
      FILEA(3) = DUMM(3)
      FILEA(4) = DUMM(4)
      FILEA(5) = CDP
      FILEA(6) = 0
      FILEA(7) = 0
      FILEL(5) = CDP
      NZ       = KORSZ(Z)
      IF (SWITCH .EQ. -204) NZ = NZ - 2*SYSBUF
      IB       = 0
      CALL CDCOMP (*30,Z,Z,Z)
      IF (SWITCH  .NE. 0) FILEU(7) = IOFF
      RETURN
C
   30 RETURN 1
      END
