      SUBROUTINE TRANSP (IX,X,NX,FILEA,B,SR1FIL)
C
C     TRANSP WILL DO AN INCORE TRANSPOSE OF THE UPPER TRIANGLE OF
C     ACTIVE ELEMENTS
C     (OUT-OF-CORE TRANSPOSE IS DONE BY TRNSP)
C
      EXTERNAL           LSHIFT    ,RSHIFT   ,ORF      ,COMPLF
      INTEGER            B         ,FILEA    ,SR1FIL   ,TYPEA    ,
     1                   EOL       ,SYSBUF   ,ORF      ,LSHIFT   ,
     2                   NAME(2)   ,RSHIFT   ,RDP      ,EOR      ,
     3                   COMPLF
      DOUBLE PRECISION   DI
      DIMENSION          FILEA(7)  ,IX(1)    ,III(4)   ,X(1)
      COMMON   /MACHIN/  MACH      ,IHALF
C     COMMON   /DESCRP/  LENGTH    ,MAJOR
      COMMON   /ZNTPKX/  IA(4)     ,II       ,EOL      ,EOR
      COMMON   /SYSTEM/  SYSBUF
      COMMON   /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                   RDP
      EQUIVALENCE        (III(3),DI)
      DATA      NAME  /  4HTRAN, 4HSP   /
C
C
      NUM   = RSHIFT(COMPLF(0),1)
      IOBUF = NX - 4*SYSBUF
      IFILE = FILEA(1)
C
C     POSITION INPUT FILE AT START OF THE UPPER TRIANGLE
C
      N = B + 1
      CALL SKPREC (FILEA,N)
      TYPEA = FILEA(5)
      NCOL  = FILEA(2)
      NO    = 0
      ISTOR = 1
      K     = 1
    5 CALL INTPK (*50,FILEA(1),0,TYPEA,0)
   10 CALL ZNTPKI
      IF (II .GT. K) GO TO 40
C
C     PACK I AND J IN ONE WORD AND STORE IT AND THE NONZERO VALUE
C     IN CORE
C
      L  = ORF(LSHIFT(II,IHALF),K+B)
      NO = NO + 1
      IX(ISTOR   ) = L
      IX(ISTOR+ 1) = IA(1)
      ISTOR = ISTOR + 2
      IF (TYPEA .NE. RDP) GO TO 20
      IX(ISTOR) = IA(2)
      ISTOR = ISTOR + 1
   20 IF (ISTOR+3 .GT. IOBUF) GO TO 230
      IF (EOL) 50,10,50
   40 IF (EOR .EQ. 0) CALL SKPREC (FILEA,1)
   50 K = K + 1
      IF (K+B .LE. NCOL) GO TO 5
      CALL REWIND (FILEA(1))
C
C     ALL ELEMENTS ARE IN CORE.  WRITE THEM OUT IN THE TRANSPOSED ORDER
C
      IFILE = SR1FIL
      CALL OPEN (*200,SR1FIL,IX(IOBUF),WRTREW)
      INCR  = TYPEA + 1
      ISTOR = ISTOR - INCR
      DO 100 I = 1,NO
      K = NUM
      DO 90 J = 1,ISTOR,INCR
      IF (IX(J) .GT. K) GO TO 90
      KK = J
      K  = IX(J)
   90 CONTINUE
C
C     UNPACK I AND J, AND WRITE OUT I,J,AND A(I,J)
C
      III(1) = RSHIFT(K,IHALF)
      III(2) = K - LSHIFT(III(1),IHALF)
      IX(KK) = NUM
      IF (INCR .EQ. 3) GO TO 95
      DI = X(KK+1)
      GO TO 96
   95 III(3) = IX(KK+1)
      III(4) = IX(KK+2)
   96 CONTINUE
      CALL WRITE (SR1FIL,III(1),4,0)
      IF (KK .EQ. ISTOR) ISTOR = ISTOR - INCR
  100 CONTINUE
C
C     WRITE A TRAILER RECORD ON THE FILE
C     NOTE - FORMAL GINO FILE TRAILER IS NOT GENERATED HERE
C
      III(1) = -1
      CALL WRITE (SR1FIL,III(1),4,0)
      CALL CLOSE (SR1FIL,REW)
      RETURN
C
  200 NO = -1
      GO TO 250
  230 NO = -8
  250 CALL MESAGE (NO,IFILE,NAME)
      RETURN
      END
