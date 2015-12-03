      SUBROUTINE SOLVE
C
C     SOLVE IS A DMAP DRIVER TO SOLVE THE MATRIX EQUATION AX=B
C
C     SOLVE   A,B/X/SYM/SIGN/PREC/TYPE $
C
C     SYM     =  1 - USE SYMETRIC DECOMPOSITION
C                0 - CHOOSE WHICH DECOMPOSITION BASED ON INPUT MATRIX
C               -1 - USE UNSYMETRIC DECOMPOSITION
C     ISIGN   =  1   SOLVE AX = B
C               -1   SOLVE AX =-B
C     IPREC   =  PRECISION USED IN THE FBS PASS
C     ITYPE   =  DESIRED TYPE OF THE OUTPUT MATRIX X
C
C
      INTEGER          NAME(2)   ,RECT     ,A        ,B        ,
     1                 CDP       ,RDP      ,SYM      ,SQR      ,
     2                 DOSI(3)   ,REFUS(3) ,OUTPT    ,X
      REAL             ZZ(1)     ,ZZZ(1)   ,ZZZZ(1)  ,ZZZZZ(1)
      DOUBLE PRECISION DET       ,DETT     ,MINDIA   ,CDET     ,
     1                 CMNDIA    ,DETC     ,MINDS
      CHARACTER        UFM*23    ,UWM*25   ,UIM*29   ,SFM*25   ,
     1                 SWM*27
      COMMON /XMSSG /  UFM       ,UWM      ,UIM      ,SFM      ,
     1                 SWM
      COMMON /BLANK /  ISYM      ,KSIGN    ,IPREC    ,ITYPE
      COMMON /SYSTEM/  KSYSTM(65)
      COMMON /SFACT /  IFILA(7)  ,IFILL1(7),IFILC(7) ,ISCR11   ,
     1                 ISCR22    ,NZ       ,DET      ,DETC     ,
     2                 IPOWER    ,ISCR33   ,MINDS    ,ICHOL
      COMMON /FBSX  /  IFILL(7)  ,IFILLT(7),IFILB(7) ,IFILX(7) ,
     1                 NX        ,IPREC1   ,ISIGN1   ,ISCR
      COMMON /DCOMPX/  IA(7)     ,IL(7)    ,IU(7)    ,ISR1     ,
     1                 ISR2      ,ISR3     ,DETT     ,IPOW     ,
     2                 NZZ       ,MINDIA   ,IB       ,IBBAR
      COMMON /CDCMPX/  JA(7)     ,KL(7)    ,KU(7)    ,JSCR1    ,
     1                 JSCR2     ,JSCR3    ,CDET(2)  ,JPOW     ,
     2                 NZZZZ     ,CMNDIA   ,JBB      ,JBBAR
      COMMON /GFBSX /  JL(7)     ,JU(7)    ,JB(7)    ,JX(7)    ,
     1                 NZZZ      ,IPR      ,ISGN
      COMMON /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                 REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                 RDP       ,CSP      ,CDP      ,SQR      ,
     3                 RECT      ,DIAG     ,LOWER    ,UPPER    ,
     4                 SYM       ,ROW      ,IDENTY
      COMMON /ZZZZZZ/  Z(1)
      EQUIVALENCE      (ZZ(1),Z(1))
      EQUIVALENCE      (ZZZ(1),Z(1))
      EQUIVALENCE      (ZZZZ(1),Z(1))
      EQUIVALENCE      (ZZZZZ(1),Z(1))
      EQUIVALENCE      (KSYSTM(55),KPREC)  ,(KSYSTM(2),OUTPT)
      DATA    A,B,X /  101,102,201/,     NAME  / 4HSOLV,4HE    /
      DATA    ISCR1 ,  ISCR2,ISCR3,ISCR4,ISCR5 /
     1        301   ,  302  ,303  ,304  ,305   /
      DATA    DOSI  /  4HSING , 4HDOUB , 4HMLTP/,
     1        REFUS /  2*3H   , 3HREF          /
C
C
      JA(1) = A
      CALL RDTRL (JA)
C
      IFORM = JA(4)
      IF (ISYM) 1,5,3
    1 IF (IFORM .EQ. SYM) WRITE (OUTPT,2) UWM,NAME
    2 FORMAT (A25,' 2340, MODULE ',2A4,' HAS BEEN REQUESTED TO DO ',
     1       'UNSYMETRIC DECOMPOSITION OF A SYMETRIC MATRIX.')
      IFORM = RECT
      IF (JA(2) .EQ. JA(3)) IFORM = SQR
      GO TO 5
    3 IF (JA(2).EQ.JA(3) .AND. IFORM.NE.SYM) WRITE (OUTPT,4) SWM,NAME
    4 FORMAT (A27,' 2341, MODULE ',2A4,' HAS BEEN FURNISHED A SQUARE ',
     1       'MATRIX MARKED UNSYMETRIC FOR SYMETRIC DECOMPOSITION.')
      IFORM = SYM
    5 ISYM  = -1
      IF (IFORM .EQ. SYM) ISYM = 1
      JA(4) = IFORM
      IF (ISYM .LT. 0) GO TO 30
C
C     SET UP CALL TO SDCOMP AND FBS
C
      INDEX = 1
      ICHOL = 0
      DO  9  I = 1,7
    9 IFILA(I) = JA(I)
      N = IFILA(2)
      IFILL1(1) = ISCR1
      IFILC(1)  = ISCR2
      ISCR11 = ISCR3
      ISCR22 = ISCR4
      ISCR33 = ISCR5
      NZ = KORSZ(Z)
      IFILL1(5) = IFILA(5)
      CALL SDCOMP (*20,Z,Z,Z)
      IFILL1(3) = IFILL1(2)
      IFILL1(4) = LOWER
      CALL WRTTRL (IFILL1)
      IFILL(1) = ISCR1
      CALL RDTRL (IFILL)
      IFILB(1) = B
      CALL RDTRL (IFILB)
C
C     IF THE B MATRIX IS PURGED, ASSUME AN IDENTITY MATRIX IN ITS PLACE
C
      IF (IFILB(1) .LE. 0) CALL MAKMCB (IFILB,B,N,IDENTY,JA(5))
      ISIGN1 = KSIGN
      IA5 = IFILA(5)
      IB5 = IFILB(5)
C
C     DETERMINE THE PRECISION FOR THE CALCULATIONS
C     AND THE TYPE OF THE OUTPUT MATRIX
C
  200 IPREC1 = KPREC
      IF ((IA5.GT.0 .AND. IA5.LE.4) .OR. (IB5.GT.0 .AND. IB5.LE.4))
     1     IPREC1 = 1
      IF (IA5.EQ.2 .OR. IA5.EQ.4 .OR. IB5.EQ.2 .OR. IB5.EQ.4) IPREC1 = 2
      IF (IPREC.EQ.IPREC1 .OR. IPREC.EQ.0) GO TO 222
      IF (IPREC.LT.1 .OR. IPREC.GT.2) IPREC = 3
      WRITE  (OUTPT,221) SWM,DOSI(IPREC),REFUS(IPREC),NAME,DOSI(IPREC1)
  221 FORMAT (A27,' 2163, REQUESTED ',A4,'LE PRECISION ',A3,'USED BY ',
     1       2A4,2H. ,A4,'LE PRECISION IS LOGICAL CHOICE')
      IF (IPREC .NE. 3 ) IPREC1 = IPREC
  222 IPREC = IPREC1
      LTYPE = IPREC1
      IF (IA5.EQ.3 .OR. IA5.EQ.4 .OR. IB5.EQ.3 .OR. IB5.EQ.4)
     1    LTYPE = IPREC1 + 2
      IF (ITYPE.EQ.0 .OR. ITYPE.EQ.LTYPE) GO TO 224
      JJ = 1
      IF (ITYPE.LT.1 .OR. ITYPE.GT.4 ) JJ = 3
      WRITE  (OUTPT,223) SFM,ITYPE,REFUS(JJ),NAME,LTYPE
  223 FORMAT (A27,' 2164, REQUESTED TYPE ',I4,2H, ,A3,'USED BY ',2A4,
     1       '. TYPE ',I4,' IS LOGICAL CHOICE.')
      IF (JJ .NE. 3 ) LTYPE = ITYPE
  224 ITYPE = LTYPE
      IF (INDEX .EQ. 2) GO TO 45
C
C     DEFINE THE MATRIX CONTROL BLOCK FOR THE OUTPUT MATRIX
C
      CALL MAKMCB (IFILX,X,N,RECT,ITYPE)
      NX = KORSZ(ZZ)
      IF (IFILB(4) .EQ. IDENTY) IFILB(5) = IPREC
      ISCR = ISCR1
      CALL FBS (ZZ,ZZ)
      IF (IFILX(2) .EQ. N) IFILX(4) = SQR
      CALL WRTTRL (IFILX)
      RETURN
C
   20 NO = ISIGN(5,ISYM)
      ISYM = -1
      CALL MESAGE (NO,A,NAME)
C
C     SET UP THE CALL TO DECOMP AND GFBS
C
   30 CONTINUE
      INDEX = 2
      IF (JA(5) .GT. 2) GO TO 80
      IA(1) = A
      IL(1) = ISCR1
      IU(1) = ISCR2
      ISR1  = ISCR3
      ISR3  = ISCR5
      ISR2  = ISCR4
      NZZ   = KORSZ(ZZZ)
      CALL RDTRL (IA)
      IA(4) = SQR
      N     = IA(2)
      IL(5) = JA(5)
      IB    = 0
      IBBAR = 0
      CALL DECOMP (*20,ZZZ,ZZZ,ZZZ)
      DO 35 I = 1,7
      JL(I) = IL(I)
      JU(I) = IU(I)
   35 CONTINUE
   40 JB(1) = B
      CALL RDTRL (JB)
C
C     IF THE B MATRIX IS PURGED, ASSUME AN IDENTITY MATRIX IN ITS PLACE
C
      IF (JB(1) .LE. 0) CALL MAKMCB (JB,B,N,IDENTY,JA(5))
      IA5  = JA(5)
      IB5  = JB(5)
      ISGN = KSIGN
C
C     DETERMINE THE PRECISION FOR THE CALCULATIONS
C     AND THE TYPE OF THE OUTPUT MATRIX
C
      GO TO 200
   45 IPR = IPREC
C
C     DEFINE THE MATRIX CONTROL BLOCK FOR THE OUTPUT MATRIX
C
      CALL MAKMCB (JX,X,N,RECT,ITYPE)
      NZZZ = KORSZ(ZZZZ)
      IF (JB(4) .EQ. IDENTY) JB(5) = IPREC
      CALL GFBS (ZZZZ,ZZZZ)
      IF (JX(2) .EQ. N) JX(4) =  SQR
      CALL WRTTRL (JX)
      RETURN
C
C     SET UP CALL TO CDCOMP AND GFBS
C
   80 CONTINUE
      KL(1) = ISCR1
      KU(1) = ISCR2
      JSCR1 = ISCR3
      JSCR2 = ISCR4
      JSCR3 = ISCR5
      NZZZZ = KORSZ(ZZZZZ)
      JA(4) = SQR
      N     = JA(2)
      KL(5) = JA(5)
      JBB   = 0
      JBBAR = 0
      CALL CDCOMP (*20,ZZZZZ,ZZZZZ,ZZZZZ)
      DO 90 I = 1, 7
      JL(I) = KL(I)
      JU(I) = KU(I)
   90 CONTINUE
      GO TO 40
      END
