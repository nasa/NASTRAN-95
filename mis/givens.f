      SUBROUTINE GIVENS
C
C     DRIVER FOR GIVENS-HOUSEHOLDER METHOD
C
      INTEGER          SYSBUF   ,EIGR(4)  ,ICORE(1) ,OPTION   ,FILE    ,
     1                 NAME(4)  ,END      ,PHIA     ,T        ,IX(7)   ,
     2                 SCR1     ,SCR2     ,SCR3     ,SCR4     ,SCR5    ,
     3                 SCR6     ,SCR7
      REAL             LFREQ    ,MB(1)
      DOUBLE PRECISION DCORE(1) ,DLMDAS   ,DALPHA(2),DBETA(2)
      CHARACTER        UFM*23   ,UWM*25   ,UIM*29   ,SFM*25
      COMMON /XMSSG /  UFM      ,UWM      ,UIM      ,SFM
      COMMON /SADDX /  NOMAT    ,LLCORE   ,MCBA(7)  ,ITYPA    ,ALPHA(4),
     1                 MCBB(7)  ,ITYPB    ,BETA(4)  ,MCBCDE(36),MCBX(7)
      COMMON /MGIVXX/  DLMDAS
      COMMON /BLANK /  IPROB(2) ,NUMMOD   ,ICASE    ,XLMDAS
     1       /GIVN  /  DUM(100) ,N        ,LFREQ    ,ORDER    ,D1      ,
     2                 HFREQ    ,D2       ,NV       ,D3       ,D4      ,
     3                 NFR
     4       /NTIME /  LNTIME   ,TCONS(15)
     5       /REGEAN/  IM(7)    ,IK(7)    ,IEV(7)   ,SCR1     ,SCR2    ,
     6                 SCR3     ,SCR4     ,SCR5     ,LCORE    ,RMAX    ,
     7                 RMIN     ,MZ       ,NEV      ,EPSI     ,RMINR   ,
     8                 NE       ,NIT      ,NEVM     ,SCR6     ,SCR7    ,
     9                 NFOUND   ,LAMDA    ,IBUCK    ,NSYM
     O       /REIGKR/  OPTION
     1       /SYSTEM/  SYSBUF   ,NOUT     ,NOGO     ,KSYS(51) ,JPREC
     2       /ZZZZZZ/  CORE(1)
      COMMON /CONDAS/  PI       ,TWOPI    ,RADEG    ,DEGRA    ,FPS
      COMMON /PACKX /  ITP1     ,ITP2     ,IIP      ,JJP      ,INCRP
      COMMON /UNPAKX/  ITU      ,IIU      ,JJU      ,INCRU
      EQUIVALENCE      (BETA(1),DBETA(1)) ,(ALPHA(1),DALPHA(1)),
     1                 (SLMDAS  ,DLMDAS ) ,(TCONS(8),MB(1)   ),
     2                 (CORE(1),DCORE(1)) ,(ICORE(1),CORE(1) ),
     3                 (TCONS(4),APC    ) ,(TCONS(5),APU     )
      DATA     NAME /  4HGIVE   ,4HNS     ,4HBEGI   ,4HNS    /,
     1         MGIV /  4HMGIV   /         ,END   /   4HENDS  /,
     2         ICR1 ,  ICR2     /301, 302 /
C
C
      CALL CONMSG (NAME,4,0)
      I   = 0
      KAA = ICORE(  1)
      MAA = ICORE(I+2)
      PHIA= ICORE(I+3)
      DO 50 I = 1,4
   50 EIGR(I) = ICORE(I+3)
      NNV = NV
      NZ  = KORSZ(CORE(1))
      IBUF1 = NZ - 3 - SYSBUF
      IBUF2 = IBUF1  - SYSBUF
      IX(1) = KAA
      CALL RDTRL (IX)
      IF (IX(1) .GT. 0) GO TO 70
      WRITE  (NOUT,60) SFM,IX,KAA,MAA,PHIA
   60 FORMAT (A25,' FROM GIVENS.  FILE ERROR,  TRAIL =',5I5,2I8, /5X,
     1       'KAA,MAA,PHIA = ',3I5)
      CALL ERRTRC ('GIVENS  ',60)
   70 AN = IX(2)
C
C     CHECK THE CORE SIZE REQUIREMENT FOR WILVEC/VALVEC BEFORE GOING
C     BLINDLY INTO EIGENVALUE COMPUTATION AND EVENTUALLY STOP DUE TO
C     INSUFFICIENT CORE IN THOSE ROUTINES.
C     PRESENTLY CDC IS USING D.P. IN GIVENS COMPUTATION.  IF CDC VERSION
C     IS MODIFIED TO USE S.P., 19 IN THE FOLLOWING FORMULA SHOULD CHANGE
C     TO 10. (COMMENT FROM G.CHAN/UNISYS)
C
      N   = (9*JPREC+1)*IX(2) + 2*SYSBUF - NZ
      IF (N .GT. 0) GO TO 120
      AZ  = NZ - (3*JPREC+1)*IX(2) - 2*SYSBUF
      AZ  = AZ/JPREC
      AM  = SQRT(2.0*AZ)
      AK  = AN - AM
      AN2 = AN**2
      AMB = MB(JPREC)
      AV  = NV
      ANV = AN*AV
      AV2 = AV**2
      T1  = AMB*AN*(3.0*(AN2+ANV) + AV2)
      T23 = APU*(10.0*AN2 + 5.0*ANV)
      T2  = APC*( 5.0*AN2 + 3.0*ANV + AV2) + T23
      T3  = 0
      IF (AM .LT. AN) T3 = T23+.5*(APC+APU)*AK*(AN2-AK*(AN+.5+AK/3.)+AN)
      T   = (T1+T2+T3)*1.0E-6
      N   = AN
      M   = AM
      WRITE  (NOUT,100) UIM,T,N,M
  100 FORMAT (A29,' 2016, GIVENS TIME ESTIMATE IS ',I8,' SECONDS.',
     1       /36X,'PROBLEM SIZE IS',I8,', SPILL WILL OCCUR FOR THIS ',
     2       'CORE AT A PROBLEM SIZE OF',I8,2H .)
      IF (T.GT.2000 .OR. N.GT.1000) WRITE (NOUT,110) UIM
  110 FORMAT (A29,', FEER METHOD WOULD BE MORE EFFICIENT FOR PROBLEM ',
     1       'OF THIS SIZE',/)
      CALL TMTOGO (I)
      IF (I .GE. T) GO TO 200
      IP1  =-50
      FILE = T
      GO TO 180
  120 WRITE  (NOUT,150) UIM,IX(2),IX(2),N
  150 FORMAT (A29,' 3008, INSUFFICIENT CORE FOR GIVENS METHOD.', /5X,
     1       'MATRIX SIZE IS',I5,3H BY,I5,'.  ADDITIONAL CORE OF',I7,
     2       ' WORDS IS NEEDED.', /5X,'OR SWITCH TO INVPWR OR FEER ',
     3       'METHOD.')
      CALL MESAGE (-37,0,NAME)
  180 CALL MESAGE (IP1,FILE,NAME)
C
C     CHOLESKI DECOMPOSE  MAA
C
  200 IF (OPTION .NE. MGIV) GO TO 250
      NOMAT   = 2
      MCBA(1) = KAA
      MCBB(1) = MAA
      CALL RDTRL (MCBA)
      CALL RDTRL (MCBB)
      MCBX(1) = ICR1
      MCBX(2) = MCBA(2)
      MCBX(3) = MCBA(3)
      MCBX(4) = MCBA(4)
      MCBX(5) = JPREC
      MCBX(6) = 0
      MCBX(7) = 0
      DALPHA(1) = 0.0D0
      DALPHA(2) = 0.0D0
      DBETA(1)  = 0.0D0
      DBETA(2)  = 0.0D0
      IF (JPREC .EQ. 2) GO TO 210
      SLMDAS = XLMDAS
      ALPHA(1) = 1.0
      BETA(1)  = SLMDAS
      ITYPA = 1
      ITYPB = 1
      GO TO 220
  210 DLMDAS = XLMDAS
      DALPHA(1) = 1.0D0
      DBETA(1)  = DLMDAS
      ITYPA = 2
      ITYPB = 2
  220 LLCORE = NZ
      CALL SADD (CORE,CORE)
      CALL WRTTRL (MCBX)
      IFILE1 = ICR1
      IFILE2 = MAA
      GO TO 260
  250 IFILE1 = MAA
      IFILE2 = KAA
  260 CALL FACTOR (IFILE1,SCR3,-SCR4,SCR5,SCR6,SCR7)
C
C     C  IS ON SCR3
C
C     CHANGE SIGNS OF THE OFF-DIAGONAL TERMS OF C AS SDCOMP HAS THEM
C     REVERSED.
C
      IP1   = -5
      FILE  = SCR3
      IX(1) = SCR3
      CALL RDTRL (IX)
      IX(5) = JPREC
      ITP1  = IX(5)
      ITP2  = ITP1
      ITU   = ITP1
      INCRP = 1
      INCRU = 1
      NCOL  = IX(2)
      IX(1) = SCR7
      IX(2) = 0
      IX(6) = 0
      IX(7) = 0
      CALL GOPEN (SCR3,CORE(IBUF1+1),0)
      CALL GOPEN (SCR7,CORE(IBUF2+1),1)
      DO 400 L = 1,NCOL
      IIU = 1
      JJU = NCOL
      CALL UNPACK (*180,SCR3,CORE)
      IF (ITU .EQ. 2) GO TO 320
      DO 300 K = 1,NCOL
      CORE(K) = -CORE(K)
  300 CONTINUE
      CORE(L) = -CORE(L)
      GO TO 350
  320 DO 340 K = 1,NCOL
      DCORE(K) = -DCORE(K)
  340 CONTINUE
      DCORE(L) = -DCORE(L)
  350 IIP = IIU
      JJP = JJU
      CALL PACK (CORE,SCR7,IX)
  400 CONTINUE
      CALL CLOSE (SCR3,1)
      CALL CLOSE (SCR7,1)
      CALL WRTTRL (IX)
C
C     C IS NOW ON SCR7
C
C     INVERT  C
C
      CALL INVERT (SCR7,SCR5,SCR6)
C
C     C INVERSE IS ON SCR5
C
C
C     GET C INVERSE TRANSPOSE ON SCR6
C
C     CALL TRANP1 (SCR5,SCR6,4,SCR4,SCR3,SCR7,ICR1,0,0,0,0)
C     GINO UNITS    308, 305,   304, 303, 204, 301
C                   ARE THESE UNITS AVAILABEL?    , 306, 307, 309
C                                                  SCR1,SCR2, EMPTY
C
C     TRANP1 SHOULD BE 60 PERCENT FASTER BY ADDING 3 MORE SCRATCH FILES
C
      CALL TRANP1 (SCR5,SCR6,7,SCR4,SCR3,SCR7,ICR1,SCR1,SCR2, 309,0)
C
C     COMPUTE  J
C
      CALL SSG2B (IFILE2,SCR6,0,SCR5,0,JPREC,1,SCR4)
      CALL SSG2B (SCR6  ,SCR5,0,SCR4,1,JPREC,1,SCR3)
C
C     J IS ON SCR4
C
C     EXTRACT EIGENVALUES
C
      CALL VALVEC
C
C     TRANSFORM
C
      CALL SSG2B (SCR6,SCR5,0,SCR4,0,JPREC,1,SCR7)
C
C     MERGE MODES AND FREE BODY MODES
C
      CALL READ6 (ICR2,SCR4,NFR,PHIA)
      ICORE(1)= NNV
      NAME(3) = END
      CALL CONMSG (NAME,3,0)
      RETURN
      END
