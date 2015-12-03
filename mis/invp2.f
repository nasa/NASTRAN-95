      SUBROUTINE INVP2 (*)
C
C     INVP2 INITIALIZES THEN CALLS EITHER SDCOMP OR DECOMP DEPENDING ON
C     THE OPTION SELECTED ON THE EIGR CARD
C
      INTEGER            FILEA     ,FILEL    ,FILEU    ,SCR1     ,
     1                   SCR2      ,SCR3     ,SCR4     ,SCR5     ,
     2                   SR1FIL    ,SR2FIL   ,DUM      ,SCR6     ,
     3                   RDP       ,UPRTRI   ,
     4                   SWITCH    ,SCR7     ,SCR8     ,OPTION   ,
     5                   OPT2      ,PREC     ,Q(1)
      DOUBLE PRECISION   DET       ,DETDET   ,DETC     ,MINDD
      COMMON   /SFACT /  FILEA(7)  ,FILEL(7) ,FILEU(7) ,SR1FIL   ,
     1                   SR2FIL    ,NZ       ,DET      ,DETC     ,
     2                   POWER     ,ISR3FL   ,MINDD    ,ICHL
      COMMON   /INVPXX/  DUMM(12)  ,SWITCH
      COMMON   /INVPWX/  DUM(14)   ,SCR1(7)  ,SCR2(7)  ,SCRX     ,
     1                   SCRXX     ,SCR3     ,SCR4     ,SCR5     ,
     2                   SCR6      ,SCR7     ,SCR8
      COMMON   /NAMES /  IJ(8)     ,RDP      ,IK(5)    ,LOWTRI   ,
     1                   UPRTRI
      COMMON   /DCOMPX/  IA(7)     ,IL(7)    ,IU(7)    ,ISCR1    ,
     1                   ISCR2     ,ISCR3    ,DETDET   ,IPOWR    ,
     2                   MZ        ,MIND
      COMMON   /SYSTEM/  KSYSTM(63)
      COMMON   /REIGKR/  OPTION
      COMMON   /ZZZZZZ/  Z(1)
      EQUIVALENCE        (Q(1),Z(1))
      EQUIVALENCE        (KSYSTM(55),PREC)
      DATA      OPT2  /  4HUINV/
C
      FILEA(1) = SCR1(1)
      IF (SWITCH .EQ. 1) GO TO 10
      FILEL(1) = SCR2(1)
      FILEU(1) = SCR3
      GO TO 20
   10 FILEL(1) = SCR7
      FILEU(1) = SCR8
   20 CONTINUE
      SR1FIL   = SCR4
      SR2FIL   = SCR5
      ISR3FL   = SCR6
      ICHL     = 0
      FILEA(2) = DUM(2)
      FILEA(3) = DUM(3)
      FILEA(4) = DUM(4)
      FILEA(5) = PREC
      FILEA(6) = 0
      FILEA(7) = 0
      FILEL(5) = PREC
      IF (OPTION .EQ. OPT2) GO TO 40
C
C     SYMMETRIC DECOMPOSITION SELECTED.
C
      NZ       = KORSZ(Z)
      CALL SDCOMP (*30,Z,Z,Z)
      FILEL(3) = FILEL(2)
      FILEL(4) = LOWTRI
      CALL WRTTRL (FILEL)
      RETURN
   30 RETURN 1
C
C     UNSYMMETRIC DECOMPOSITION SELECTED.
C
   40 DO 50 I = 1,21
      IA(I) = FILEA(I)
   50 CONTINUE
      ISCR1 = SCR4
      ISCR2 = SCR5
      ISCR3 = SCR6
      MZ    = KORSZ(Q)
      CALL DECOMP (*30,Q,Q,Q)
      IL(3) = IL(2)
      IL(4) = LOWTRI
      CALL WRTTRL (IL)
      IU(3) = IU(2)
      IU(4) = UPRTRI
      IU(5) = IL(5)
      CALL WRTTRL (IU)
      RETURN
      END
