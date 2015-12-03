      SUBROUTINE KFLUD3
C
C     THIS ROUTINE GENERATES THE PSUEDO STIFFNESS MATRIX TERMS
C     FOR THE TRIANGULAR FLUID ELEMENT
C
C     THE ECPT DATA IS THE FOLLOWING
C
C         FIELD         SYMBOL
C           1             ID
C           2             SIL1
C           3             SIL2
C           4             SIL3
C           5             RHO
C           6             BULK
C           7             N
C           8             CSF
C           9             R1
C           10            Z1
C           11            -
C           12            CSF
C           13            R2
C           14            Z2
C           15            -
C           16            CSF
C           17            R3
C           18            Z3
C           19            -
C           20            -
C
      LOGICAL          NOGO
      INTEGER          OUT      ,NP(3)    ,NECPT(100)
      DOUBLE PRECISION CONSTD   ,DPI      ,
     1                 R        ,R1       ,R2       ,R3       ,
     2                 Z1       ,Z2       ,Z3       ,DETH     ,
     3                 H        ,RA       ,RB       ,ZA       ,
     4                 ZB       ,DR       ,DZ       ,BETA     ,
     5                 BLOG     ,DZR      ,DZR2     ,BET2     ,
     6                 R12      ,R22      ,G00      ,G10      ,
     7                 G20      ,G01      ,G11      ,G02      ,
     8                 RN       ,PIRO     ,PRN2     ,KQ       ,
     9                 KVEC     ,KG
      CHARACTER        UFM*23
      COMMON /XMSSG /  UFM
      COMMON /CONDAD/  CONSTD(5)
      COMMON /SYSTEM/  SYSBUF   ,OUT      ,NOGO
      COMMON /SMA1IO/  DUM1(10) ,IFKGG
      COMMON /SMA1CL/  IOPT4    ,K4GGSW   ,NPVT
      COMMON /SMA1ET/  ECPT(100)
      COMMON /SMA1DP/  R        ,R1       ,R2       ,R3       ,
     1                 Z1       ,Z2       ,Z3       ,DETH     ,
     2                 H(9)     ,RA       ,RB       ,ZA       ,
     3                 ZB       ,DR       ,DZ       ,BETA     ,
     4                 BLOG     ,DZR      ,DZR2     ,BET2     ,
     5                 R12      ,R22      ,G00      ,G10      ,
     6                 G20      ,G01      ,G11      ,G02      ,
     7                 RN       ,PIRO     ,PRN2     ,KQ(9)    ,
     8                 KVEC(3)  ,KG(3)    ,IRET     ,IPT      ,
     9                 JC       ,IR
      EQUIVALENCE      (CONSTD(1),DPI)    ,(ECPT(1),NECPT(1))
C
C     SELECT POINTS FOR COUNTERCLOCKWISE ORDER
C
      NP(1) = NECPT(2)
      NP(2) = NECPT(3)
      NP(3) = NECPT(4)
      R1    = ECPT( 9)
      Z1    = ECPT(10)
      R2    = ECPT(13)
      Z2    = ECPT(14)
      R3    = ECPT(17)
      Z3    = ECPT(18)
      R     = (R2-R1)*(Z3-Z1) - (R3-R1)*(Z2-Z1)
      IF (R) 10,2000,20
   10 NP(2) = NP(3)
      NP(3) = NECPT(3)
      R2    = ECPT(17)
      R3    = ECPT(13)
      Z2    = ECPT(18)
      Z3    = ECPT(14)
   20 IF (R1.LE.0.0D0 .OR. R2.LE.0.0D0 .OR. R3.LE.0.0D0) GO TO 1000
      DETH  = DABS(R)
      H(1)  = (R2*Z3-R3*Z2)/DETH
      H(4)  = (R3*Z1-R1*Z3)/DETH
      H(7)  = (R1*Z2-R2*Z1)/DETH
      H(2)  = (Z2-Z3)/DETH
      H(5)  = (Z3-Z1)/DETH
      H(8)  = (Z1-Z2)/DETH
      H(3)  = (R3-R2)/DETH
      H(6)  = (R1-R3)/DETH
      H(9)  = (R2-R1)/DETH
C
C     THE INTEGRAL PARAMETERS ARE THE SUM DUE TO SIDES 1-2,2-3,3-1.
C
      G00   = 0.0
      G01   = 0.0
      G02   = 0.0
      G10   = 0.0
      G11   = 0.0
      G20   = 0.0
      IRET  = 1
      RA    = R1
      RB    = R2
      ZA    = Z1
      ZB    = Z2
      GO TO 500
  100 IRET  = 2
      RA    = R2
      RB    = R3
      ZA    = Z2
      ZB    = Z3
      GO TO 500
  110 IRET  = 3
      RA    = R3
      RB    = R1
      ZA    = Z3
      ZB    = Z1
C
C     THE INTEGRAL PARAMETERS ARE CALCULATED BELOW
C
  500 DR    = RB - RA
      DZ    = ZB - ZA
      IF (DR**2/DETH .LE. 1.0D-6) GO TO 140
      BETA  = ZA - RA*DZ/DR
      BET2  = BETA**2
      BLOG  = BETA*DLOG(RA/RB)
      DZR   = DZ/DR
      DZR2  = DZR**2
      R12   = (RA**2-RB**2)/2.0D0
      R22   = (RA**3-RB**3)/3.0D0
      G00   = G00 + BLOG - DZ
      G10   = G10 - BETA*DR  + R12*DZR
      G20   = G20 + BETA*R12 + DZR*R22
      G01   = G01 + BLOG*BETA/2.0D0 - BETA*DZ + DZR2*R12/2.0D0
      G11   = G11 - BET2*DR/2.0D0 + BETA*DZR*R12 + DZR2*R22/2.0D0
      G02   = G02 + BET2*BLOG/3.0D0 - BET2*DZ + BETA*DZR2*R12 +
     1        DZR*DZR2*R22/3.0D0
  140 CONTINUE
      GO TO (100,110,120), IRET
  120 CONTINUE
C
C     FORM THE PSUEDO STIFFNESS MATRIX USING THE PARAMETERS
C
      RN    = NECPT(7)
      IF (ECPT(5) .LE. 0.0) RETURN
      PIRO  = DPI/DBLE(ECPT(5))
      IF(NECPT(7) .EQ. 0) PIRO=PIRO*2.0D0
      PRN2  = PIRO*RN**2
      KQ(1) = PRN2*G00
      KQ(2) = PRN2*G10
      KQ(3) = PRN2*G01
      KQ(4) = KQ(2)
      KQ(5) = (PIRO + PRN2)*G20
      KQ(6) = PRN2*G11
      KQ(7) = KQ(3)
      KQ(8) = KQ(6)
      KQ(9) = PIRO*G20 + PRN2*G02
      DO 200 I = 1,3
      IPT   = I - 1
      IF (NPVT .EQ. NP(I)) GO TO 210
  200 CONTINUE
      RETURN
C
  210 IPT   = 3*IPT + 1
      CALL GMMATD (H(IPT),1,3,0,KQ,3,3,0,KVEC)
      CALL GMMATD (KVEC,1,3,0,H(1),3,3,1,KG)
      JC    = NPVT
      DO 220 I = 1,3
      IR    = NP(I)
      CALL SMA1B (KG(I),IR,JC,IFKGG,0.0D0)
  220 CONTINUE
 2000 RETURN
C
 1000 IR    = NECPT(1)/1000
      WRITE  (OUT,5001) UFM,IR
 5001 FORMAT (A23,' 5001, NEG. OR ZERO RADIUS DETECTED FOR CFLUID3 OR',
     1       ' CFLUID4 ELEMENT',I12)
      NOGO = .TRUE.
      RETURN
      END
