      SUBROUTINE DDCMPS
C
C     DDCMPS IS THE DMAP DRIVER FOR SDCMPS
C
C     SDCMPS   USET,GPL,SIL,KAA/LLL,ULL/SYM=0/DIAGCK=0/DIAGET=20/
C              PDEFCK=0/SING=0/SET=L/CHLSKY=0/DET=0.0D0/MINDIA=0.0D0/
C              POWER=0/SUBNAM=NONE
C
C     SYM      =  1 - USE SYMMETRIC DECOMPOSITION
C                 0 - CHOOSE WHICH DECOMPOSITION BASED ON INPUT MATRIX
C                -1 - USE UNSYMETRIC DECOMPOSITION
C     DIAGCK   =  DIAGONAL SINGULARITY CHECK              (SDCMPS)
C                 - = NO CHECK                            (SDCMPS)
C                 0 = NONFATAL                            (SDCMPS)
C                 + = MAX ALLOWED FATAL                   (SDCMPS)
C     DIAGET   =  DIAGONAL SINGULARITY ERROR TOLERANCE.   (SDCMPS)
C     PDEFCK   =  POSITIVE DEFINATE CHECK                 (SDCMPS)
C                 - = NO CHECK                            (SDCMPS)
C                 0 = NONFATAL                            (SDCMPS)
C                 + = MAX ALLOWED FATAL                   (SDCMPS)
C     SING     =  SINGULARITY OUTPUT FLAG
C                 1 = OK
C                 0 = NONCONSERVATIVE OR ES FAILURE
C                -1 = SINGULAR OR LIMITS EXCEEDED
C     SET      =  SET MATRIX BELONGS TO                   (SDCMPS)
C     CHLSKY   =  1 USE CHOLESKY DECOMPOSITION LLL = C
C     DET      =  DETERMINANT OF KAA
C     MINDIA   =  MINIMUM DIAGONAL OF ULL
C     POWER    =  SCALE FACTOR FOR DET
C     SUBNAM   =  SUBSTRUCTURE NAME                       (SDCMPS)
C
      LOGICAL          OPNSCR   ,FIRST
      INTEGER          BUF6     ,CHLSKY   ,DIAGCK   ,DIAGET   ,NAME(2) ,
     1                 NAM(2)   ,OUTPT    ,PARM     ,PDEFCK   ,POWER   ,
     2                 RECT     ,SET      ,SING     ,SQR      ,SYM     ,
     3                 ULL
      REAL             ZZ(1)    ,ZZZ(1)   ,ZZZZ(1)  ,ZM(1)
      DOUBLE PRECISION CDET     ,CMNDIA   ,MINDIA   ,SDETC    ,MINDS   ,
     1                 DDET     ,DMNDIA   ,SDET
      CHARACTER        UFM*23   ,UWM*25   ,UIM*29   ,SFM*25   ,SWM*27
      COMMON /XMSSG /  UFM      ,UWM      ,UIM      ,SFM      ,SWM
      COMMON /BLANK /  ISYM     ,DIAGCK   ,DIAGET   ,PDEFCK   ,SING    ,
     1                 SET(2)   ,CHLSKY   ,DET(2)   ,MINDIA   ,POWER   ,
     2                 SUBNAM(2)
      COMMON /SDCQ  /  NERR(2)   ,NOGLEV   ,BUF6     ,ISCMSG   ,ISCDIA ,
     1                 ISTSCR    ,KPDFCK   ,KDGCK    ,KDGET    ,KPREC  ,
     2                 PARM(4)   ,OPNSCR   ,FIRST
      COMMON /SFACT /  IFILA(7)  ,IFILL(7) ,IFILU(7) ,KSCR1    ,
     1                 KSCR2     ,NZ       ,SDET     ,SDETC    ,KPOW   ,
     2                 KSCR3     ,MINDS    ,ICHLK
      COMMON /DCOMPX/  IA(7)     ,IL(7)    ,IU(7)    ,ISCR1    ,
     1                 ISCR2     ,ISCR3    ,DDET     ,IPOW     ,
     3                 NZZ       ,DMNDIA   ,IB
      COMMON /CDCMPX/  JA(7)     ,JL(7)    ,JU(7)    ,JSCR1    ,
     1                 JSCR2     ,JSCR3    ,CDET(2)  ,JPOW     ,
     3                 NZZZ      ,CMNDIA   ,JB
      COMMON /NAMES /  KNAMES(19)
      COMMON /SYSTEM/  KSYSTM(69)
      COMMON /ZZZZZZ/  Z(1)
      EQUIVALENCE      (ZZ(1),Z(1))
      EQUIVALENCE      (ZZZ(1),Z(1))
      EQUIVALENCE      (ZZZZ(1),Z(1))
      EQUIVALENCE      (ZM(1),Z(1))
      EQUIVALENCE      (KSYSTM( 1),NBUFSZ) ,(KSYSTM( 2),OUTPT) ,
     1                 (KNAMES(12),SQR   ) ,(KNAMES(13),RECT ) ,
     2                 (KNAMES(17),SYM   )
      DATA    LUSET ,  LGPL ,LSIL ,KAA  ,LLL  ,ULL  ,LSCR1,LSCR2,LSCR3 /
     1        101   ,  102  ,103  ,104  ,201  ,202  ,301  ,302  ,303   /
      DATA    LSCR4 ,  LSCR5,LSCR6/
     1        304   ,  305  ,306  /
      DATA    NAME  /  4HDDCM, 4HPS   /
      DATA    NAM   /  4HSDCM, 4HPS   /
C
C     NOTE SYM DECOMP DOES NOT OUTPUT  ULL
C
C
      OPNSCR = .FALSE.
      FIRST  = .TRUE.
      SING   = 1
      JA(1)  = KAA
      CALL RDTRL (JA)
      IF (JA(1) .LT. 0) GO TO 490
      IFORM  = JA(4)
      IF (ISYM) 10,50,30
   10 IF (IFORM .NE. SYM) GO TO 20
      CALL PAGE2 (2)
      WRITE  (OUTPT,15) SWM,NAM
   15 FORMAT (A27,' 2340, MODULE ',2A4,' HAS BEEN REQUESTED TO DO ',
     1       'UNSYMETRIC DECOMPOSITION OF A SYMETRIC MATRIX')
   20 IFORM = RECT
      IF (JA(2) .EQ. JA(3)) IFORM = SQR
      GO TO 50
C
   30 IF (IFORM .EQ. SYM) GO TO 50
      CALL PAGE2 (2)
      WRITE  (OUTPT,40) SWM,NAM
   40 FORMAT (A27,' 2341, MODULE ',2A4,' HAS BEEN FURNISHED A SQUARE ',
     1       'MATRIX MARKED UNSYMETRIC FOR SYMETRIC DECOMPOSITION.')
      IFORM = SYM
   50 ISYM  = -1
      IF (IFORM .EQ. SYM) ISYM = 1
      JA(4) = IFORM
      I = 0
      IF (JA(2) .EQ. JA(3)) GO TO 60
      CALL PAGE2 (2)
      I = 1
      WRITE  (OUTPT,55) SWM,NAM
   55 FORMAT (A27,' 2375, MODULE ',2A4,' HAS BEEN REQUESTED TO ',
     1       'DECOMPOSE A RECTANGULAR MATRIX')
   60 CONTINUE
      IF (ISYM .LT. 0) GO TO 200
C
C     SET UP CALL TO SDCOMP
C
      IF (I .NE. 0) GO TO 500
      IFILA(1) = KAA
      CALL RDTRL (IFILA)
      IFILL(1) = LLL
      IFILU(1) = LSCR4
      KSCR1 = LSCR1
      KSCR2 = LSCR2
      KSCR3 = LSCR3
      IFILL(5) = IFILA(5)
      ICHLK = CHLSKY
      IF (IFILA(5) .LE. 2) GO TO 100
      NZ = KORSZ (Z)
      CALL SDCOMP (*400,Z,Z,Z)
      GO TO 130
  100 NZ = KORSZ(ZZZZ)
      ISCMSG = LSCR5
      ISCDIA = LSCR6
      KPDFCK = PDEFCK
      KDGCK  = DIAGCK
      KDGET  = DIAGET
      CALL SDCMPS (ZZZZ,ZZZZ,ZZZZ)
      IF (NERR(1)+NERR(2) .EQ. 0) GO TO 110
      BUF6 = KORSZ(ZM) - 2*NBUFSZ + 1
      IF (BUF6+NBUFSZ .LE. 0) GO TO 510
      CALL SDCMM (ZM,SET(1),IFILA(2),IFILA(1),LUSET,LGPL,LSIL,SUBNAM)
      SING = 0
C
C     ONLY ES CHECK AND NONCONSERVATIVE COLUMN CAN EXIT WITH SING = 1
C     OR IF USER DESIRES TO CONTINUE
C
      IF (NOGLEV .GT. 0) SING = -1
  110 CONTINUE
      IF (PARM(1) .NE. 0) CALL MESAGE (PARM(1),PARM(2),PARM(3))
  130 DET(1) = SDET
      DET(2) = SDETC
      MINDIA = MINDS
      POWER  = KPOW
      IFILL(2) = IFILA(2)
      IFILL(3) = IFILA(3)
      IFILL(4) = 4
      IF (SING .GE. 0) CALL WRTTRL (IFILL)
      GO TO 410
C
C     SET UP CALL TO DECOMP
C
  200 CONTINUE
      IF (JA(5) .GT. 2) GO TO 300
      IA(1) = KAA
      CALL RDTRL (IA)
      IL(1) = LLL
      IU(1) = ULL
      NZZ   = KORSZ(ZZ)
      ISCR1 = LSCR1
      ISCR2 = LSCR2
      ISCR3 = LSCR3
      IB    = 0
      IL(5) = 2
      CALL DECOMP (*400,ZZ,ZZ,ZZ)
      IU(5) = 2
      IL(4) = 4
      IU(4) = 5
      IL(3) = IL(2)
      IU(3) = IU(2)
      DET(1)= DDET
      DET(2)= 0.0
      POWER = IPOW
      MINDIA= DMNDIA
      CALL WRTTRL (IU)
      CALL WRTTRL (IL)
      GO TO 410
C
C     SET UP CALL TO CDCOMP
C
  300 CONTINUE
      JL(1) = LLL
      JU(1) = ULL
      JSCR1 = LSCR1
      JSCR2 = LSCR2
      JSCR3 = LSCR3
      NZZZ  = KORSZ(ZZZ)
      JL(5) = 4
      JB    = 0
      CALL CDCOMP (*400,ZZZ,ZZZ,ZZZ)
      JU(5) = 4
      JL(4) = 4
      JU(4) = 5
      JL(3) = JL(2)
      JU(3) = JU(2)
      DET(1)= CDET(1)
      DET(2)= CDET(2)
      MINDIA= CMNDIA
      POWER = JPOW
      CALL WRTTRL (JL)
      CALL WRTTRL (JU)
      GO TO 410
C
  400 SING   = -1
      DET(1) = 0.0
      DET(2) = 0.0
      POWER  = 0
      MINDIA = 0.0
  410 RETURN
C
C     ERROR  MESSAGES
C
C     PURGED INPUT
C
  490 PARM(1) = -1
      PARM(2) = KAA
      GO TO 520
C
C     NUMBER ROWS.NE.COLUMNS
C
  500 PARM(1) = -16
      PARM(2) = KAA
      GO TO 520
C
C     INSUFFICIENT CORE
C
  510 PARM(1) = -8
      PARM(2) = -BUF6 - NBUFSZ
  520 PARM(3) = NAME(1)
      PARM(4) = NAME(2)
      GO TO 110
      END
