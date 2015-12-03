      SUBROUTINE GFSMOD
C
C     THIS ROUTINE PERFORMS THE MODAL FORMULATION OF THE
C     FLUID / STRUCTURE MATRICES
C
      EXTERNAL        ANDF
      INTEGER         SCR1     ,SCR2     ,SCR3     ,SCR4     ,SCR5     ,
     1                SCR6     ,SCR7     ,SCR8     ,SCR9     ,SCR10    ,
     2                AXY      ,AFRY     ,KYY      ,DKAA     ,DKFRFR   ,
     3                USETF    ,PHIA     ,PHIX     ,AC       ,POUT     ,
     4                LAMA     ,KMAT     ,MMAT     ,GIA      ,PVEC     ,
     5                IDENT    ,USET     ,USETD    ,KC       ,H        ,
     6                COMPTP   ,AZY      ,AHY      ,AHJ      ,AJH      ,
     7                KJJ      ,AYH      ,KJJL     ,GJH      ,MZZ      ,
     8                KZZ      ,MHHBAR   ,PHIAR    ,KZZBAR   ,KHHBAR   ,
     9                GYH      ,MCB(7)   ,SFBIT    ,FILE     ,UM       ,
     1                UZ       ,UNZ      ,UFR      ,UH       ,UY       ,
     2                UF       ,US       ,UI       ,Z        ,SYSBUF   ,
     3                TWO      ,TYPIN    ,TYPOUT   ,BADD(11) ,NAME(2)  ,
     4                PHIXR    ,ANDF     ,NAMEX(2)
      REAL            RZ(1)
      DOUBLE PRECISION          DBADD(5)
      CHARACTER       UFM*23   ,UWM*25
      COMMON /XMSSG / UFM      ,UWM
      COMMON /BLANK / NOGRAV   ,NOFREE   ,KCOMP    ,COMPTP   ,FORM     ,
     1                LLMODE
      COMMON /SYSTEM/ SYSBUF   ,NOUT
      COMMON /PATX  / LCORE    ,NSUB0    ,NSUB1    ,NSUB2    ,USET
      COMMON /ZZZZZZ/ Z(1)
      COMMON /PACKX / TYPIN    ,TYPOUT   ,II       ,NN       ,INCR
      COMMON /TWO   / TWO(32)
      COMMON /BITPOS/ UNZ      ,UZ       ,UM       ,UH       ,BIT1(3)  ,
     1                UF       ,US       ,BIT2(15) ,UY       ,UFR      ,
     2                BIT3(2)  ,UI
      COMMON /GFSMOX/ AXY      ,AFRY     ,KYY      ,DKAA     ,DKFRFR   ,
     1                USETF    ,PHIA     ,PHIX     ,LAMA     ,
     2                KMAT     ,MMAT     ,GIA      ,POUT     ,
     3                SCR1     ,SCR2     ,SCR3     ,SCR4     ,SCR5     ,
     4                SCR6     ,SCR7     ,SCR8     ,
     5                LMODES   ,NMODES   ,IBUF     ,SFBIT    ,BADD     ,
     6                NAME
      EQUIVALENCE     (BADD(2),DBADD(1)) ,(RZ(1),Z(1)) ,
     1                (SCR1,USETD) ,(SCR2,PVEC,IDENT,KJJL) ,
     2                (SCR3,AZY,AHJ,KJJ,GJH) ,(SCR4,AJH,KHHBAR,GYH) ,
     3                (SCR5,AC,AYH,MZZ,KZZBAR) ,(SCR6,KZZ) ,
     4                (SCR7,KC,AHY) ,(SCR8,H) ,(SCR9,MMAT) ,
     5                (SCR10,GIA,MHHBAR)
C     DATA            AXY      ,AFRY     ,KYY      ,DKAA     ,DKFRFR   ,
C    1                USETF    ,PHIA     ,PHIX     ,LAMA     ,
C    2                KMAT     ,MMAT     ,GIA      ,POUT     ,
C    3                SCR1     ,SCR2     ,SCR3     ,SCR4     ,SCR5     ,
C    4                SCR6     ,SCR7     ,SCR8     /
C    5                101      ,102      ,103      ,104      ,105      ,
C    6                111      ,112      ,113      ,114      ,
C    7                201      ,202      ,203      ,204      ,
C    8                301      ,302      ,303      ,304      ,305      ,
C    9                306      ,307      ,308      /
C
C     DATA    BADD  / 11*0   /
      DATA    NAMEX / 4HGFSM , 4HOD   /
C
      AXY    = 101
      AFRY   = 102
      KYY    = 103
      DKAA   = 104
      DKFRFR = 105
      USETF  = 111
      PHIA   = 112
      PHIX   = 113
      LAMA   = 114
      KMAT   = 201
      MMAT   = 202
      GIA    = 203
      POUT   = 204
      SCR1   = 301
      SCR2   = 302
      SCR3   = 303
      SCR4   = 304
      SCR5   = 305
      SCR6   = 306
      SCR7   = 307
      SCR8   = 308
      NAME(1)= NAMEX(1)
      NAME(2)= NAMEX(2)
      DO 5 I = 1,11
    5 BADD(I)= 0
C
C
      PHIAR = SCR4
      PHIXR = SCR2
C
      LCORE = KORSZ(Z(1))
      IBUF  = LCORE - SYSBUF - 1
      IF (IBUF .LT. 0) GO TO 1008
C
C     CREATE A DUMMY USET VECTOR TOR USE WITH THE MODAL DISPLACEMENTS
C
C     BIT POSITIONS WILL BE
C
C     UM  - MODAL POINT  UZ + UNZ
C     UZ  - DESIRED MODAL POINT
C     UNZ - MODAL POINT TO BE SKIPPED
C     UFR - FREE SURFACE POINT
C     UH  - UFR + UZ
C
C     SET MODAL DISPLACEMENTS
C
      FILE   = PHIX
      MCB(1) = PHIX
      CALL RDTRL (MCB)
      IF (MCB(1) .LT. 0) GO TO 1001
      NMODES = MCB(2)
      IF (LLMODE.GT.NMODES .OR. LLMODE.EQ.0) LLMODE = -1
      LMODES = LLMODE
      IF (LMODES .LE. 0) LMODES = NMODES
      IF (LMODES.LE.0 .OR. LMODES.GT.NMODES) LMODES = NMODES
      IZM  = TWO(UZ)  + TWO(UM) + TWO(UH)
      INZM = TWO(UNZ) + TWO(UM)
      IF (IBUF .LE. NMODES) GO TO 1008
      DO 10 I = 1,NMODES
      Z(I) = IZM
      IF (I .GT. LMODES) Z(I) = INZM
   10 CONTINUE
C
C     SET FREE SURFACE DISPLACEMENTS
C
      IFR  = TWO(UFR) + TWO(UH)
      LVEC = NMODES
      IF (NOFREE) 45,20,20
   20 CALL GOPEN (USETF,Z(IBUF),0)
   30 CALL READ (*40,*40,USETF,IBIT,1,0,N)
      IF (ANDF(IBIT,TWO(UFR)) .EQ. 0) GO TO 30
      LVEC = LVEC + 1
      IF (LVEC .GE. IBUF) GO TO 1008
      Z(LVEC) = IFR
      GO TO 30
C
   40 CALL CLOSE (USETF,1)
C
C     WRITE DUMMY USETD FILE
C
   45 CALL GOPEN (USETD,Z(IBUF),1)
      CALL WRITE (USETD,Z(1),LVEC,1)
      CALL CLOSE (USETD,1)
      MCB(1) = USETD
      MCB(2) = LVEC
      DO 50 I = 3,7
   50 MCB(I) = 0
      CALL WRTTRL (MCB)
C
C     EXTRACT THE DESIRED MODES FORM THE PHIX MATRIX
C
      USET = USETD
      IF (LMODES .GE. NMODES) GO TO 70
      CALL CALCV (PVEC,UM,UZ,UNZ,Z(1))
      CALL GFSPTN (PHIX,PHIXR,0,0,0,PVEC,0)
      GO TO 80
C
   70 PHIXR = PHIX
C
C     TRANSFORM THE FLUID STRUCTURE AREA MATRIX
C
   80 CALL SSG2B (PHIXR,AXY,0,AZY,1,2,1,SCR5)
C
C     IF FREE SURFACE POINTS EXIST - MERGE THEM WITH THE TRANSFORMED
C     AREA MATRIX
C
      IF (NOFREE) 100,90,90
   90 CALL CALCV (PVEC,UH,UZ,UFR,Z(1))
      CALL GFSMRG (AHY,AZY,AFRY,0,0,0,PVEC)
      GO TO 110
C
  100 CALL GFSWCH (AHY,AZY)
C
C     DETERMINE IF ANY SINGLE POINT CONSTRAINTS EXIST ON THE FLUID
C
  110 USET = USETF
      CALL CALCV (PVEC,UY,UF,US,Z(1))
      NUY = NSUB0 + NSUB1
      SFBIT = 1
      IF (NSUB1 .EQ. 0) SFBIT = 0
C
C     IF SPC POINTS EXIST ON THE FLUID - PARTITION THEM OUT OF THE
C     FLUID AREA AND STIFFNESS MATRICES
C
      IF (SFBIT) 120,130,120
  120 CALL GFSPTN (AHY,AHJ,0,0,0,PVEC,0)
      CALL GFSTRN (AHJ,AJH,SCR5,SCR6)
      CALL GFSPTN (KYY,KJJ,0,0,0,PVEC,PVEC)
      GO TO 160
C
C     IF NO SPC POINTS EXIST ON THE FLUID, CONSTRAIN THE FIRST FLUID
C     POINT TO REMOVE POTENTIAL SINGULARITIES
C
  130 IF (COMPTP .GT. 0) WRITE (NOUT,140) UWM
  140 FORMAT (A25,' 8015. THE PURELY INCOMPRESSIBLE METHOD IS AVAIL',
     1       'ABLE ONLY WITH THE DIRECT FORMULATION.')
      CALL GFSSPC (NUY,PVEC)
      NSUB0 = NUY - 1
      NSUB1 = 1
      CALL GFSPTN (KYY,KJJ,0,0,0,PVEC,PVEC)
C
C     GENERATE THE H TRANSFORMATION MATRIX
C
      CALL GFSH (NUY,H)
      CALL GFSTRN (AHY,AYH,SCR2,SCR6)
      CALL SSG2B (H,AYH,0,AJH,0,2,1,SCR6)
C
C     GENERATE THE COMPRESSIBLITY MATRIX
C
      CALL GFSCOM (AHY,NUY,KC,IDENT,AC,SCR6)
C
C     SOLVE FOR THE INITIAL PRESSURE TRANSFORMATION MATRIX
C
  160 CALL FACTOR (KJJ,KJJL,SCR5,SCR6,SCR9,SCR10)
      CALL SSG3A (0,KJJL,AJH,GJH,SCR5,SCR6,-1,0)
C
C     FOR COMPUTER CORE CONSERVATION REASON, THE REST OF GFSMOD IS
C     MOVED TO GFSMO2, WHICH CAN BE SEGMENTED IN PARALLEL WITH GFSMOD.
C
      RETURN
C
C     ERROR EXITS
C
 1001 N = -1
      GO TO 9999
 1008 N = -8
C
 9999 CALL MESAGE (N,FILE,NAME)
      RETURN
      END
