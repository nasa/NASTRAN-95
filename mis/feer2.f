      SUBROUTINE FEER2 (IRET)
C
C     FEER2 INITIALIZES THEN CALLS  SDCOMP
C
      INTEGER            FILEA     ,FILEL    ,FILEU    ,SR1FLE   ,
     1                   SR2FLE    ,SR3FLE   ,SR4FLE   ,SR5FLE   ,
     2                   SR6FLE    ,SR7FLE   ,SR8FLE   ,RDP      ,
     3                   UPRTRI    ,PREC
      DOUBLE PRECISION   DET       ,DETC     ,MINDD
C
      COMMON   /OPINV /  MCBLT(7)  ,MCBSMA(7)
      COMMON   /SFACT /  FILEA(7)  ,FILEL(7) ,FILEU(7) ,ISR1FL   ,
     1                   ISR2FL    ,NZ       ,DET      ,DETC     ,
     2                   POWER     ,ISR3FL   ,MINDD    ,ICHL
      COMMON   /FEERXX/  DUMM(12)  ,IFSET
      COMMON   /FEERCX/  IFKAA(7)  ,IFMAA(7) ,IFLELM(7),IFLVEC(7),
     1                   SR1FLE    ,SR2FLE   ,SR3FLE   ,SR4FLE   ,
     2                   SR5FLE    ,SR6FLE   ,SR7FLE   ,SR8FLE   ,
     3                   DMPFLE    ,NORD     ,XLMBDA   ,NEIG     ,
     4                   MORD      ,IBK      ,CRITF    ,NORTHO   ,
     5                   IFLRVA    ,IFLRVC
      COMMON   /ZZZZZZ/  Z(1)
      COMMON   /NAMES /  IJ(8)     ,RDP      ,IK(5)    ,LOWTRI   ,
     1                   UPRTRI
      COMMON   /SYSTEM/  KSYSTM(54),PREC
C
      IRET     = 0
C
      FILEA(1) = IFLELM(1)
      FILEL(1) = IFLVEC(1)
      FILEU(1) = SR3FLE
      ISR1FL   = SR4FLE
      ISR2FL   = SR5FLE
      ISR3FL   = SR6FLE
      ICHL     = 0
      IF (IBK.EQ.1 .OR. IFSET.EQ.1) ICHL = 1
      FILEA(2) = IFKAA(2)
      FILEA(3) = IFKAA(3)
      FILEA(4) = IFKAA(4)
      FILEA(5) = PREC
      FILEA(6) = 0
      FILEA(7) = 0
      FILEL(5) = PREC
C
C     SYMMETRIC DECOMPOSITION
C
      NZ = KORSZ(Z)
      CALL SDCOMP (*30,Z,Z,Z)
   10 FILEL(3) = FILEL(2)
      FILEL(4) = LOWTRI
      CALL WRTTRL (FILEL)
      DO 20 I = 1,7
   20 MCBLT(I) = FILEL(I)
      RETURN
C
   30 IRET = 1
      GO TO 10
      END
