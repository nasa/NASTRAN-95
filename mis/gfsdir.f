      SUBROUTINE GFSDIR
C
C     THIS ROUTINE PERFORMS THE DIRECT FORMULATION OF THE
C     FLUID/STRUCTURE MATRICES
C
      EXTERNAL      ANDF
      INTEGER       SCR1     ,SCR2     ,SCR3     ,SCR4     ,SCR5     ,
     1              SCR6     ,SCR7     ,SCR8     ,AXY      ,AFRY     ,
     2              KYY      ,DKAA     ,DKFRFR   ,KAA      ,MAA      ,
     3              GM       ,GO       ,USETS    ,USETF    ,KMAT     ,
     4              MMAT     ,GIA      ,PVEC     ,IDENT    ,KJJL     ,
     5              ANYBAR   ,AFY      ,AWY      ,SCR9     ,KAABAR   ,
     6              AMY      ,AAYBAR   ,AWJ      ,KJJ      ,GJW      ,
     7              ANY      ,AOY      ,AJW      ,AC       ,GYW      ,
     8              AAY      ,KWWBAR   ,MWWBAR   ,KC       ,H        ,
     9              USET     ,MCB(7)   ,MBIT     ,SBIT     ,SFBIT    ,
     O              OBIT     ,UM       ,US       ,UO       ,UG       ,
     1              UN       ,UA       ,UF       ,UY       ,UAB      ,
     2              UFR      ,UI       ,Z        ,SYSBUF   ,TWO      ,
     3              FILE     ,TYPIN    ,TYPOUT   ,BADD(11) ,NAME(2)  ,
     4              UR       ,USG      ,USB      ,UL       ,UX       ,
     5              UZ       ,BIT      ,AYW      ,MT       ,HC       ,
     6              COMPTP   ,ANDF
      REAL          RZ(1)    ,KCOMP    ,RBADD(12)
      DOUBLE PRECISION        DBADD(5)
C
C     MODULE PARAMETERS
C
      COMMON / BLANK  /       NOGRAV   ,NOFREE   ,KCOMP   ,COMPTP    ,
     1                        FORM     ,LMODES
C
C     SYSTEM COMMON
C
      COMMON / SYSTEM /       SYSBUF
C
C     CALCV COMMON BLOCK
C
      COMMON / PATX   /       LCORE    ,NSUB0    ,NSUB1    ,NSUB2    ,
     1                        USET
C
C     OPEN CORE
C
      COMMON / ZZZZZZ /       Z(1)
C
C     PACK COMMON BLOCKS
C
      COMMON / ZBLPKX /       A(4)     ,IROW
      COMMON / PACKX  /       TYPIN    ,TYPOUT   ,II       ,NN       ,
     1                        INCR
C
C     POWERS OF TWO
C
      COMMON / TWO    /       TWO(32)
C
C     USET BIT POSITIONS
C
      COMMON / BITPOS /       UM       ,UO       ,UR       ,USG      ,
     1                        USB      ,UL       ,UA       ,UF       ,
     2                        US       ,UN       ,UG       ,BIT(12)  ,
     3                        UX       ,UY       ,UFR      ,UZ       ,
     4                        UAB      ,UI
C
C     SCRATCH FILE ASSIGNMENTS
C
      EQUIVALENCE   (BADD(1),RBADD(2)) , (DBADD(1),RBADD(3)) ,
     1              (RZ(1), Z(1)),
     2              (SCR1 , PVEC , IDENT , KJJL) ,
     3              (SCR2 , ANYBAR , AFY , AWY ) ,
     4              (SCR3 , AMY , AAYBAR , AWJ , GJW , KJJ) ,
     5              (SCR4 , AOY , AJW , GYW)  ,
     6              (SCR5 , AAY , KWWBAR , AYW ) ,
     7              (SCR6 , AC) ,
     8              (SCR7 , KC  , MT) ,
     9              (SCR8 , H)  ,
     O              (SCR9 , GIA)
C
C     GINO FILE ASSIGNMENTS
C
      DATA          AXY      ,AFRY     ,KYY      ,DKAA     ,DKFRFR   ,
     1              KAA      ,MAA      ,GM       ,GO       ,USETS    ,
     2              USETF    ,KMAT     ,MMAT     ,HC       ,
     3              GIA      ,SCR1     ,SCR2     ,SCR3     ,SCR4     ,
     4              SCR5     ,SCR6     ,SCR7     ,SCR8               /
     5              101      ,102      ,103      ,104      ,105      ,
     6              106      ,107      ,108      ,109      ,110      ,
     7              111      ,201      ,202      ,205      ,
     8              203      ,301      ,302      ,303      ,304      ,
     9              305      ,306      ,307      ,308                /
C
      DATA   NAME / 4HGFSD   ,4HIR     /
      DATA   BADD / 11*0     /
C
C
      ANY    = SCR4
      KAABAR = SCR2
      MWWBAR = SCR6
C
      LCORE = KORSZ(Z(1))
      IBUF  = LCORE - SYSBUF - 1
      IF (IBUF .LT. 0) GO TO 1008
C
C     REDUCE FLUID / STRUCTURE AREA MATRIX.  MATRIX IS TREATED AS
C     A LOAD VECTOR
C
      MCB(1) = USETS
      CALL RDTRL (MCB)
      MBIT = ANDF(MCB(5),TWO(UM))
      SBIT = ANDF(MCB(5),TWO(US))
      OBIT = ANDF(MCB(5),TWO(UO))
C
      USET = USETS
C
C     PARTITION OUT MULTIPOINT CONSTRAINTS
C
      IF (MBIT) 10,20,10
   10 CALL CALCV  (PVEC,UG,UN,UM,Z(1))
      CALL GFSPTN (AXY,ANYBAR,AMY,0,0,0,PVEC)
      CALL SSG2B  (GM,AMY,ANYBAR,ANY,1,2,1,SCR1)
      GO TO 30
C
   20 ANY = AXY
C
C     PARTITION OUT SINGLE POINT CONSTRAINTS
C
   30 IF (SBIT) 40,50,40
   40 CALL CALCV  (PVEC,UN,UF,US,Z(1))
      CALL GFSPTN (ANY,AFY,0,0,0,0,PVEC)
      GO TO 60
C
   50 CALL GFSWCH (AFY,ANY)
C
C     PARTITION OUT OMITS
C
   60 IF (OBIT) 70,80,70
   70 CALL CALCV  (PVEC,UF,UA,UO,Z(1))
      CALL GFSPTN (AFY,AAYBAR,AOY,0,0,0,PVEC)
      CALL SSG2B  (GO,AOY,AAYBAR,AAY,1,2,1,SCR1)
      GO TO 85
C
   80 CALL GFSWCH (AAY,AFY)
C
C     IF FREE SURFACE POINTS EXIST - MERGE THEM WITH THE REDUCED
C     AREA MATRIX
C
   85 USET = USETF
      IF (NOFREE) 100,90,90
   90 CALL CALCV  (PVEC,UA,UAB,UFR,Z(1))
      CALL GFSMRG (AWY,AAY,AFRY,0,0,0,PVEC)
      GO TO 110
C
  100 CALL GFSWCH (AWY,AAY)
C
C     DETERMINE IF ANY SINGLE POINT CONSTRAINTS EXIST ON THE FLUID
C
  110 CALL CALCV (PVEC,UY,UF,US,Z(1))
      NUY   = NSUB0 + NSUB1
      SFBIT = 1
      IF (NSUB1 .EQ. 0) SFBIT = 0
C
C     IF SPC POINTS EXIST ON THE FLUID - PARTITION THEM OUT OF
C     THE FLUID AREA AND STIFFNESS MATRIX
C
      IF (SFBIT) 120,130,120
  120 CALL GFSPTN (AWY,AWJ,0,0,0,PVEC,0)
      CALL GFSTRN (AWJ,AJW,SCR2,SCR5)
      CALL GFSPTN (KYY,KJJ,0,0,0,PVEC,PVEC)
      GO TO 170
C
C     NO SPC POINTS EXIST ON THE FLUID
C
C     CONSTRAIN THE FIRST FLUID POINT TO REMOVE ANY POTENTIAL
C     SINGULARITIES
C
  130 CALL GFSSPC (NUY,PVEC)
      NSUB0 = NUY - 1
      NSUB1 = 1
      CALL GFSPTN (KYY,KJJ,0,0,0,PVEC,PVEC)
C
C     GENERATE THE H TRANSFORMATION MATRIX
C
      CALL GFSH   (NUY,H)
      CALL GFSTRN (AWY,AYW,SCR1,SCR6)
      CALL SSG2B  (H,AYW,0,AJW,0,2,1,SCR6)
C
C     CHECK COMPRESSIBLITY TYPE
C
      IF (COMPTP .GT. 0) GO TO 140
C
C     A SPRING WILL BE GENERATED TO COUPLE THE STRUCTURE AND THE
C     FREE SURFACE TO RESTRICT VOLUME CHANGES
C
C     COMPUTE THE COMPRESSIBLITY MATRIX WHICH CONTAINS THIS SPRING
C
      CALL GFSCOM (AWY,NUY,KC,IDENT,AC,SCR5)
      GO TO 170
C
C     PURELY INCOMPRESSIBLE APPROACH - A CONSTRAINT EQUATION IS
C     GENERATED TO RESTRICT VOLUME CHANGE
C
C     GENERATE HC MATRIX WHICH CONTAINS THE CONSTRAINT
C
  140 CALL GFSHC (AWY,NUY,HC,IDENT,AC,MROW)
C
C     SOLVE FOR THE INITIAL PRESSURE TRANSFORMATION MATRIX
C
  170 CALL FACTOR (KJJ,KJJL,SCR2,SCR5,SCR6,SCR9)
      CALL SSG3A  (0,KJJL,AJW,GJW,SCR5,SCR6,-1,0)
C
C     IF GRAVITY EXISTS - ADD THE ADDITIONAL STIFFNESS
C
      IF (NOGRAV) 190,180,180
  180 BADD (1) = 2
      DBADD(1) = 1.0D0
      BADD (7) = 2
      DBADD(4) = 1.0D0
      CALL SSG2C (KAA,DKAA,KAABAR,0,BADD)
      GO TO 200
C
  190 KAABAR = KAA
C
C     IF FREE SURFACE EXISTS - MERGE THE STIFFNESS TO SOLUTION SIZE
C     AND EXPAND THE MASS MATRIX
C
  200 IF (NOFREE) 220,210,210
  210 CALL CALCV  (PVEC,UA,UAB,UFR,Z(1))
      CALL GFSMRG (KWWBAR,KAABAR,0,0,DKFRFR,PVEC,PVEC)
      CALL GFSMRG (MWWBAR,MAA,0,0,0,PVEC,PVEC)
      GO TO 230
C
  220 CALL GFSWCH (KWWBAR,KAABAR)
      MWWBAR = MAA
C
C     COMPUTE THE FINAL MASS MATRIX
C     FOR COMPTP = 1 THIS MATRIX IS NOT THE FINAL ONE
C
  230 CALL SSG2B (AJW,GJW,MWWBAR,MMAT,1,2,1,SCR2)
C
C     COMPUTE THE FINAL STIFFNESS MATRIX
C
      IF (SFBIT) 260,240,260
  240 IF (COMPTP .GT. 0) GO TO 250
C
C     ADD IN THE SPRING FACTOR KC
C
      BADD (1) = 2
      DBADD(1) = 1.0D0
      BADD (7) = 2
      DBADD(4) = 1.0D0
      CALL SSG2C (KWWBAR,KC,KMAT,0,BADD)
      GO TO 270
C
C     APPLY THE CONSTRAINT EQUATION TO STIFFNESS AND MASS FOR
C     THE INCOMPRESSIBLE APPROACH
C
  250 CALL SSG2B (HC,KWWBAR,0,SCR2,1,2,1,SCR1)
      CALL SSG2B (SCR2,HC,0,KMAT,0,2,1,SCR1)
      CALL SSG2B (HC,MMAT,0,SCR2,1,2,1,SCR1)
      CALL SSG2B (SCR2,HC,0,MT,0,2,1,SCR1)
C
C     ADD 1.0 TO THE NULL COLUMN IN THE MASS MATRIX TO PREVENT
C     SINGULATITIES
C
      CALL GFSMT (MT,MMAT,MROW)
      GO TO 270
C
  260 CALL GFSWCH (KMAT,KWWBAR)
C
C     TRANSFORM THE FINAL PRESSURE TRANSFORMATION MATRIX OR IF
C     SPC POINTS EXIST ON THE FLUID MERGE IN ZEROS
C
  270 IF (SFBIT) 300,280,300
  280 CALL SSG2B (H,GJW,0,GYW,1,2,1,SCR5)
      GO TO 310
C
  300 CALL CALCV  (PVEC,UY,UF,US,Z(1))
      CALL GFSMRG (GYW,GJW,0,0,0,0,PVEC)
C
C     PARTITON OUT THE FREE SURFACE POINTS
C
  310 IF (NOFREE) 330,320,320
  320 CALL CALCV  (PVEC,UY,UFR,UI,Z(1))
      CALL GFSPTN (GYW,0,GIA,0,0,0,PVEC)
      RETURN
C
  330 CALL GFSWCH (GIA,GYW)
      RETURN
C
C     ERROR CONDITIONS
C
 1008 N = -8
      CALL MESAGE (N,FILE,NAME)
      RETURN
      END
