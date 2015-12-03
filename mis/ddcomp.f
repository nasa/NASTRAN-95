      SUBROUTINE DDCOMP
C
C     DDCOMP IS THE DMAP DRIVER FOR DECOMP
C
C     DECOMP    KAA/LLL,ULL/SYM/CHLSKY/MINDIA/DET/POWER/SING $
C
C        SYM    =  1 - USE SYMMETRIC DECOMPOSITION
C                  0 - CHOOSE WHICH DECOMPOSITION BASED ON INPUT MATRIX
C                 -1 - USE UNSYMETRIC DECOMPOSITION
C        CHLSKY =  1 USE CHOLESKY DECOMPOSITION LLL = C
C        DET    =  DETERMINANT OF KAA
C        POWER  =  SCALE FACTOR FOR DET
C        MINDIA =  MINIMUM DIAGONAL OF ULL
C        SING   = -1 SINGULAR MATRIX
C
      INTEGER          ULL       ,SYM      ,POWER    ,SING     ,
     1                 CHLSKY    ,NAME(2)  ,SQR      ,RECT     ,
     2                 OUTPT     ,UPPER
      REAL             ZZ(1)     ,ZZZ(1)
      DOUBLE PRECISION CDET      ,CMNDIA   ,MINDIA   ,SDETC    ,
     1                 MINDS     ,DDET     ,DMNDIA   ,SDET
      CHARACTER        UFM*23    ,UWM*25   ,UIM*29   ,SFM*25   ,
     1                 SWM*27
      COMMON /XMSSG /  UFM       ,UWM      ,UIM      ,SFM      ,
     1                 SWM
      COMMON /BLANK /  ISYM      ,CHLSKY                       ,
     1                 MINDIA    ,DET(2)   ,POWER    ,SING
      COMMON /SFACT /  IFILA(7)  ,IFILL(7) ,IFILU(7) ,KSCR1    ,
     1                 KSCR2     ,NZ       ,SDET     ,SDETC    ,
     2                 KPOW      ,KSCR3    ,MINDS    ,ICHLK
      COMMON /DCOMPX/  IA(7)     ,IL(7)    ,IU(7)    ,ISCR1    ,
     1                 ISCR2     ,ISCR3    ,DDET     ,IPOW     ,
     2                 NZZ       ,DMNDIA   ,IB
      COMMON /CDCMPX/  JA(7)     ,JL(7)    ,JU(7)    ,JSCR1    ,
     1                 JSCR2     ,JSCR3    ,CDET(2)  ,JPOW     ,
     2                 NZZZ      ,CMNDIA   ,JB
      COMMON /NAMES /  KNAMES(19)
      COMMON /SYSTEM/  KSYSTM(65)
      COMMON /ZZZZZZ/  Z(1)
      EQUIVALENCE      (ZZ(1),Z(1))
      EQUIVALENCE      (ZZZ(1),Z(1))
      EQUIVALENCE      (KSYSTM( 2),OUTPT)  ,(KNAMES(12),SQR)   ,
     1                 (KNAMES(13),RECT )  ,(KNAMES(17),SYM)   ,
     2                 (KNAMES(16),UPPER)  ,(KNAMES(15),LOWER)
      DATA    KAA,     LLL,   ULL,    LSCR1,  LSCR2,  LSCR3,  LSCR4 /
     1        101,     201,   202,    301  ,  302  ,  303  ,  304   /
      DATA    NAME  /  4HDDCO,4HMP    /
C
      SING  = 0
      JA(1) = KAA
      CALL RDTRL (JA)
      IFORM = JA(4)
      IF (ISYM) 10,50,30
   10 IF (IFORM .EQ. SYM) WRITE (OUTPT,20) SWM,NAME
   20 FORMAT (A27,' 2340, MODULE ',2A4,' HAS BEEN REQUESTED TO DO ',
     1       'UNSYMMETRIC DECOMPOSITION OF A SYMMETRIC MATRIX')
      IFORM = RECT
      IF (JA(2) .EQ. JA(3)) IFORM = SQR
      GO TO 50
   30 IF (JA(2).EQ.JA(3) .AND. IFORM.NE.SYM) WRITE (OUTPT,40) SWM,NAME
   40 FORMAT (A27,' 2341, MODULE ',2A4,'HAS BEEN FURNISHED A SQUARE ',
     1       'MATRIX MARKED UNSYMMETRIC FOR SYMMETRIC DECOMPOSITION.')
      IFORM = SYM
   50 ISYM  = -1
      IF (IFORM .EQ. SYM) ISYM = 1
      JA(4) = IFORM
      IF (ISYM .LT. 0) GO TO 200
C
C     SET UP CALL TO SDCOMP
C
      IFILA(1) = KAA
      CALL RDTRL (IFILA)
      IFILL(1) = LLL
      IFILU(1) = LSCR4
      KSCR1    = LSCR1
      KSCR2    = LSCR2
      KSCR3    = LSCR3
      NZ       = KORSZ(Z)
      IFILL(5) = IFILA(5)
      ICHLK    = CHLSKY
      CALL SDCOMP (*400,Z,Z,Z)
      DET(1)   = SDET
      DET(2)   = SDETC
      MINDIA   = MINDS
      POWER    = KPOW
      IFILL(2) = IFILA(2)
      IFILL(3) = IFILA(3)
      IFILL(4) = LOWER
      CALL WRTTRL (IFILL)
      RETURN
C
C     SET UP CALL TO DECOMP
C
  200 CONTINUE
      IF (JA(5) .GT. 2) GO TO 300
      IA(1)  = KAA
      CALL RDTRL (IA)
      IL(1)  = LLL
      IU(1)  = ULL
      NZZ    = KORSZ(ZZ)
      ISCR1  = LSCR1
      ISCR2  = LSCR2
      ISCR3  = LSCR3
      IB     = 0
      IL(5)  = 2
      CALL DECOMP (*400,ZZ,ZZ,ZZ)
      IU(5)  = 2
      IL(4)  = LOWER
      IU(4)  = UPPER
      IL(3)  = IL(2)
      IU(3)  = IU(2)
      DET(1) = DDET
      DET(2) = 0.0
      POWER  = IPOW
      MINDIA = DMNDIA
      CALL WRTTRL (IU)
      CALL WRTTRL (IL)
      RETURN
C
C     SET UP CALL TO CDCOMP
C
  300 CONTINUE
      JL(1)  = LLL
      JU(1)  = ULL
      JSCR1  = LSCR1
      JSCR2  = LSCR2
      JSCR3  = LSCR3
      NZZZ   = KORSZ(ZZZ)
      JL(5)  = 4
      JB     = 0
      CALL CDCOMP (*400,ZZZ,ZZZ,ZZZ)
      JU(5)  = 4
      JL(4)  = LOWER
      JU(4)  = UPPER
      JL(3)  = JL(2)
      JU(3)  = JU(2)
      DET(1) = CDET(1)
      DET(2) = CDET(2)
      MINDIA = CMNDIA
      POWER  = JPOW
      CALL WRTTRL (JL)
      CALL WRTTRL (JU)
      RETURN
C
  400 SING   = -1
      DET(1) = 0.0
      DET(2) = 0.0
      POWER  = 0
      MINDIA = 0.0
      CALL FNAME (KAA,JA(1))
      WRITE  (OUTPT,410) UIM,JA(1),JA(2)
  410 FORMAT (A29,' FORM DECOMP MODULE. MATRIX ',2A4,' IS SINGULAR')
      RETURN
      END
