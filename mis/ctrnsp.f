      SUBROUTINE CTRNSP (IX,X,NX,FILEA,B,SR1FIL)
C
C     TRANS WILL DO AN INCORE TRANSPOSE OF THE UPPER TRIANGLE OF ACTIVE
C     ELEMENTS
C
      EXTERNAL           LSHIFT    ,RSHIFT   ,ORF      ,COMPLF
      INTEGER            B         ,FILEA    ,SR1FIL   ,TYPEA    ,
     1                   EOL       ,SYSBUF   ,ORF      ,LSHIFT   ,
     2                   NAME(2)   ,RSHIFT   ,RDP      ,EOR      ,
     3                   CDP       ,COMPLF
      DOUBLE PRECISION   DI(2)
      DIMENSION          FILEA(7)  ,IX(1)    ,III(6)   ,X(1)
      COMMON   /MACHIN/  MACH      ,IHALF
      COMMON   /ZNTPKX/  IA(4)     ,II       ,EOL      ,EOR
      COMMON   /SYSTEM/  SYSBUF
      COMMON   /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                   RDP       ,CSP      ,CDP
      COMMON   /TYPE  /  JPREC(2)  ,NWDS(4)
      EQUIVALENCE        (III(3),DI(1))
      DATA      NAME /   4HCTRN ,4HSP  /
C
C
      NUM   = RSHIFT(COMPLF(0),1)
      IOBUF = NX - 4*SYSBUF
      IFILE = FILEA(1)
C
C     POSITION INPUT FILE AT START OF THE UPPER TRIANGLE
C
      CALL SKPREC (FILEA(1),B+1)
      TYPEA = FILEA(5)
      NCOL  = FILEA(2)
      NO    = 0
      ISTOR = 1
      IPREC = JPREC(TYPEA)
      INCR  = NWDS(TYPEA) + 1
      K     = 1
   20 CALL INTPK (*70,FILEA(1),0,TYPEA,0)
   30 CALL ZNTPKI
      IF (II .GT. K) GO TO 50
C
C     PACK I AND J IN ONE WORD AND STORE IT AND THE NONZERO VALUE
C     IN CORE
C
      L  = ORF(LSHIFT(II,IHALF),K+B)
      NO = NO + 1
      IX(ISTOR  ) = L
      IX(ISTOR+1) = IA(1)
      IX(ISTOR+2) = IA(2)
      IX(ISTOR+3) = IA(3)
      IX(ISTOR+4) = IA(4)
      ISTOR = ISTOR+INCR
      IF (ISTOR+INCR .GT. IOBUF) GO TO 140
      IF (EOL) 70,30,70
   50 IF (EOR .EQ. 0) CALL SKPREC (FILEA(1),1)
   70 K = K + 1
      IF (K+B .LE. NCOL) GO TO 20
      CALL REWIND (FILEA(1))
C
C     ALL ELEMENTS ARE IN CORE.  WRITE THEM OUT IN THE TRANSPOSED ORDER
C
      IFILE = SR1FIL
      CALL OPEN (*120,SR1FIL,IX(IOBUF),WRTREW)
      ISTOR = ISTOR - INCR
      DO 110 I = 1,NO
      K = NUM
      DO 80 J = 1,ISTOR,INCR
      IF (IX(J) .GT. K) GO TO 80
      KK = J
      K  = IX(J)
   80 CONTINUE
C
C     UNPACK I AND J, AND WRITE OUT I,J,AND A(I,J)
C
      III(1) = RSHIFT(K,IHALF)
      III(2) = K - LSHIFT(III(1),IHALF)
      IX(KK) = NUM
      IF (IPREC .EQ. 2) GO TO 90
      DI(1) = X(KK+1)
      DI(2) = 0.D0
      IF (TYPEA .GT. 2) DI(2) = X(KK+2)
      GO TO 100
   90 III(3) = IX(KK+1)
      III(4) = IX(KK+2)
      III(5) = 0
      III(6) = 0
      IF (TYPEA .LE. 2) GO TO 100
      III(5) = IX(KK+3)
      III(6) = IX(KK+4)
  100 CONTINUE
      CALL WRITE (SR1FIL,III(1),6,0)
      IF (KK .EQ. ISTOR) ISTOR = ISTOR - INCR
  110 CONTINUE
C
C     WRITE A TRAILER RECORD ON THE FILE
C
      III(1) = -1
      CALL WRITE (SR1FIL,III(1),6,0)
      CALL CLOSE (SR1FIL,REW)
      RETURN
C
  120 NO = -1
      GO TO 150
  140 NO = -8
  150 CALL MESAGE (NO,IFILE,NAME)
      RETURN
      END
