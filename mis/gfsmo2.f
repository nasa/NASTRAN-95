      SUBROUTINE GFSMO2
C
C     THIS ROUTINE IS THE CONTINUATION OF GFSMOD
C
      REAL         RZ(1)     ,EIGVAL(7)
C
      DOUBLE PRECISION       DBADD(5)
C
      INTEGER       SCR1     ,SCR2     ,SCR3     ,SCR4     ,SCR5
     1             ,SCR6     ,SCR7     ,SCR8     ,SCR9     ,SCR10
     2             ,AXY      ,AFRY     ,KYY      ,DKAA     ,DKFRFR
     3             ,USETF    ,PHIA     ,PHIX     ,AC       ,POUT
     4             ,LAMA     ,KMAT     ,MMAT     ,GIA      ,PVEC
     5             ,IDENT    ,USET     ,USETD    ,KC       ,H
     6             ,COMPTP   ,AZY      ,AHY      ,AHJ      ,AJH
     7             ,KJJ      ,AYH      ,KJJL     ,GJH      ,MZZ
     8             ,KZZ      ,MHHBAR   ,PHIAR    ,KZZBAR   ,KHHBAR
     9             ,GYH      ,MCB(7)   ,SFBIT    ,FILE     ,UM
     1             ,UZ       ,UNZ      ,UFR      ,UH       ,UY
     2             ,UF       ,US       ,UI       ,Z        ,SYSBUF
     3             ,TWO      ,TYPIN    ,TYPOUT   ,BADD(11) ,NAME(2)
C
C     MODULE PARAMETERS
C
      COMMON /BLANK/          NOGRAV   ,NOFREE   ,KCOMP   ,COMPTP
     1                       ,FORM     ,LLMODE
C
C     SYSTEM COMMON
C
      COMMON / SYSTEM /       SYSBUF   ,NOUT
C
C     CALCV COMMON BLOCK
C
      COMMON / PATX /         LCORE    ,NSUB0    ,NSUB1    ,NSUB2
     1                       ,USET
C
C     OPEN CORE
C
      COMMON / ZZZZZZ /        Z(1)
C
C     PACK COMMON BLOCK
C
      COMMON / PACKX /        TYPIN    ,TYPOUT   ,II       ,NN
     1                       ,INCR
C
C     POWERS OF TWO
C
      COMMON / TWO /          TWO(32)
C
C     USET BIT POSITIONS - SOME OF THESE ARE USED JUST HERE
C
      COMMON / BITPOS /       UNZ      ,UZ       ,UM       ,UH
     1                       ,BIT1(3)  ,UF       ,US       ,BIT2(15)
     2                       ,UY       ,UFR      ,BIT3(2)  ,UI
C
C     LOCAL VARIABLES FOR GFSMO1 AND GFSMO2
C
      COMMON /GFSMOX/AXY     ,AFRY     ,KYY      ,DKAA     ,DKFRFR
     1             ,USETF    ,PHIA     ,PHIX     ,LAMA
     2             ,KMAT     ,MMAT     ,GIA      ,POUT
     3             ,SCR1     ,SCR2     ,SCR3     ,SCR4     ,SCR5
     4             ,SCR6     ,SCR7     ,SCR8
     5             ,LMODES   ,NMODES   ,IBUF     ,SFBIT    ,BADD
     6             ,NAME
C
C     SCRATCH FILE ASSIGNMENTS
C
      EQUIVALENCE   ( BADD(2) , DBADD(1) )
     1             ,( RZ(1) , Z(1) )
     2             ,( SCR1 , USETD )
     3             ,( SCR2 , PVEC , IDENT , KJJL )
     4             ,( SCR3 , AZY , AHJ , KJJ , GJH )
     5             ,( SCR4 , AJH , KHHBAR , GYH )
     6             ,( SCR5 , AC , AYH , MZZ , KZZBAR )
     7             ,( SCR6 , KZZ )
     8             ,( SCR7 , KC , AHY )
     9             ,( SCR8 , H )
     1             ,( SCR9 , MMAT )
     2             ,( SCR10 , GIA , MHHBAR )
C
C
C***********************************************************************
C
C
C     GET THE GENERALIZED STIFFNESS AND MASS FOR THE DESIRED MODES
C     FROM THE LAMA DATA BLOCK
C
      IF(2*LMODES .GE. IBUF) GO TO 1008
      CALL GOPEN(LAMA,Z(IBUF),0)
      FILE = LAMA
      CALL FWDREC(*1002,LAMA)
      IGK = 1
      IGM = LMODES + 1
      DO 170 I=1,LMODES
  165 CALL READ(*1001,*1002,LAMA,EIGVAL,7,0,N)
      IF(EIGVAL(6) .EQ. 0.0) GO TO 165
      RZ(IGK) = EIGVAL(7)
      IGK = IGK + 1
      RZ(IGM) = EIGVAL(6)
      IGM = IGM + 1
  170 CONTINUE
      CALL CLOSE(LAMA,1)
C
C     GENERATE THE DIAGONAL MODAL STIFFNESS MATRIX
C
      I1 = 1
      I2 = LMODES
      CALL MAKMCB(MCB,KZZ,LMODES,6,2)
      CALL GOPEN(KZZ,Z(IBUF),1)
      TYPIN = 1
      TYPOUT = 2
      INCR = 1
      DO 180 I=I1,I2
      II = I
      NN = I
  180 CALL PACK(RZ(I),KZZ,MCB)
      CALL CLOSE(KZZ,1)
      CALL WRTTRL(MCB)
C
C     GENERATE THE DIAGANOL MODAL MASS MATRIX
C
      I1 = LMODES + 1
      I2 = 2 * LMODES
      CALL MAKMCB(MCB,MZZ,LMODES,6,2)
      CALL GOPEN(MZZ,Z(IBUF),1)
      DO 190 I=I1,I2
      II = I - LMODES
      NN = II
  190 CALL PACK(RZ(I),MZZ,MCB)
      CALL CLOSE(MZZ,1)
      CALL WRTTRL(MCB)
C
C     IF A FREE SURFACE EXISTS - EXPAND THE MASS MATRIX
C     THE PARTITIONING VECTOR WILL BE SAVED FOR DMAP USE
C
      IF(NOFREE) 210,200,200
  200 USET = USETD
      CALL CALCV(POUT,UH,UZ,UFR,Z(1))
      NSUB0S = NSUB0
      NSUB1S = NSUB1
      CALL GFSMRG(MHHBAR,MZZ,0,0,0,POUT,POUT)
      GO TO 220
C
  210 CALL GFSWCH(MHHBAR,MZZ)
C
C     COMPUTE THE FINAL MASS MATRIX
C
  220 CALL SSG2B(AJH,GJH,MHHBAR,MMAT,1,2,1,SCR2)
C
C     IF GRAVITY EXISTS - TRANSFORM THE ADDITIONAL STIFFNESS AND
C     ADD IT IN.  BE SURE TO USE ONLY THOSE MODES REQUESTED IN
C     THE TRANSFORMATION FROM PHIA
C
      IF(NOGRAV) 260,230,230
  230 USET = USETD
      IF(LMODES .GE. NMODES) GO TO 240
      CALL CALCV(PVEC,UM,UZ,UNZ,Z(1))
      CALL GFSPTN(PHIA,PHIAR,0,0,0,PVEC,0)
      GO TO 250
C
  240 PHIAR = PHIA
C
  250 CALL SSG2B(PHIAR,DKAA,0,SCR2,1,2,1,SCR5)
      CALL SSG2B(SCR2,PHIAR,KZZ,KZZBAR,0,2,1,SCR10)
      GO TO 270
C
  260 CALL GFSWCH(KZZ,KZZBAR)
C
C     IF A FREE SURFACE EXISTS - MERGE THE FREE SURFACE STIFFNESS IN
C
  270 IF(NOFREE) 290,280,280
  280 NSUB0 = NSUB0S
      NSUB1 = NSUB1S
      CALL GFSMRG(KHHBAR,KZZBAR,0,0,DKFRFR,POUT,POUT)
      GO TO 300
C
  290 CALL GFSWCH(KHHBAR,KZZBAR)
C
C     COMPUTE THE FINAL STIFFNESS MATRIX BY ADDING IN COMPRESSIBILITY
C     IF IT EXISTS
C
  300 IF(SFBIT) 320,310,320
  310 BADD(1) = 2
      DBADD(1) = 1.0D0
      BADD(7) = 2
      DBADD(4) = 1.0D0
      CALL SSG2C(KHHBAR,KC,KMAT,0,BADD)
C
      GO TO 330
  320 CALL GFSWCH(KHHBAR,KMAT)
C
C     TRANSFORM THE FINAL PRESSURE TRANSFORMATION MATRIX OR IF SPC
C     POINTS EXIST ON THE FLUID MERGE IN ZEROS
C
  330 USET = USETF
      IF(SFBIT) 350,340,350
  340 CALL SSG2B(H,GJH,0,GYH,1,2,1,SCR5)
      GO TO 360
C
  350 CALL CALCV(PVEC,UY,UF,US,Z(1))
      CALL GFSMRG(GYH,GJH,0,0,0,0,PVEC)
C
C     PARTITION OUT THE FREE SURFACE POINTS
C
  360 IF(NOFREE) 380,370,370
  370 CALL CALCV(PVEC,UY,UFR,UI,Z(1))
      CALL GFSPTN(GYH,0,GIA,0,0,0,PVEC)
      RETURN
C
  380 CALL GFSWCH(GYH,GIA)
      RETURN
C
C     ERROR EXITS
C
 1001 N = -1
      GO TO 9999
 1002 N = -2
      GO TO 9999
 1008 N = -8
C
 9999 CALL MESAGE(N,FILE,NAME)
      RETURN
      END
