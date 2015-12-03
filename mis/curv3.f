      SUBROUTINE CURV3
C*****
C  THIS OVERLAY WILL FORM OES1G (IF REQUESTED BY DMAP PARAMETER = 0)
C
C  OES1G OUTPUTS FOR CURRENT SUBCASE WILL BE GROUPED ON THE BASIS OF
C  THE MCSID. THUS THERE WILL BE A PASS FOR EACH -MCSID- HAVING A NON-
C  ZERO COUNT IN TABLE(IMCSID-NMCSID).
C
C  TO CONSERVE CORE FOR SSPLIN UTILITY, THE SIGMAS FOR EACH -MCSID- PASS
C  WILL BE WRITTEN TO SCR4 AS ENTRIES ARE SELECTED FROM SCR2. DEPENDENT
C  POINTS, EXTERNAL-IDS, AND INDEPENDENT POINTS WILL BE PLACED IN CORE
C  AND THEN REDUCED DURING THE PROJECTION SURFACE DETERMINATION PHASE.
C
C     OPEN CORE MAP DURING -CURV3- EXECUTION.
C     =======================================
C
C     FROM-------+------------+
C     CURV1      I  Z(IELTYP) I  MASTER LIST OF ELEMENT TYPES  ON
C     EXECUTION  I    THRU    I  ESTX(SCR1)
C                I  Z(NELTYP) I
C                +------------+
C                I  Z(IMCSID) I  MASTER LIST OF MCSIDS ELEMENTS IN
C                I    THRU    I  PROBLEM REFERENCE, WITH COUNTS OF
C                I  Z(NMCSID) I  OES1M ELEMENTS FOR CURRENT SUBCASE.
C                +------------+
C                I  Z(ICSTM)  I  CSTM FOR EACH -MCSID- IN ABOVE LIST.
C                I    THRU    I  14 WORD ENTRIES. (USER MAY NOT HAVE
C                I  Z(NCSTM)  I  SUPPLIED ALL, BUT MAY BE OK.)
C     FROM-------+------------+
C     AND DURING I  Z(IINDEP) I  INDEPENDENT POINT COORDINATES FOR ONE
C     CURV3      I    THRU    I  -MCSID- OF CURRENT SUBCASE.
C     EXECUTION  I  Z(NINDEP) I  TWO OR THREE WORD ENTRIES POSSIBLE.
C                +------------+
C                I  Z(IDEP)   I  DEPENDENT POINT COORDINATES FOR ONE
C                I    THRU    I  -MCSID- OF CURRENT SUBCASE.
C                I  Z(NDEP)   I  TWO OR FOUR WORD ENTRIES POSSIBLE.
C                +------------+
C                I  Z(IGMAT)  I  G MATRIX FROM SSPLIN UTILITY
C                I    THRU    I  (N-DEPENDENT-PTS BY N-INDEPENDENT-PTS)
C                I  Z(NGMAT)  I
C                +------------+
C                I  Z(ISIGMA) I  OES1M SIGMAS FOR ONE -MCSID- OF CURRENT
C                I    THRU    I  SUBCASE.  6X1 ENTRIES.
C                I  Z(NSIGMA) I
C                +------------+
C                I     .      I  AVAILABLE CORE.
C                I     .      I  (SSPLIN UTILITY USES Z(ISIGMA) THRU
C                I     .      I  Z(LCORE) FOR WORKING SPACE.)
C                I     .      I
C                I  Z(JCORE)  I
C                +------------+
C                I  Z(IBUF4)  I  GINO-BUFFER
C                I            I
C                +------------+
C                I  Z(IBUF3)  I  GINO-BUFFER
C                I            I
C                +------------+
C                I  Z(IBUF2)  I  GINO-BUFFER
C                I            I
C                +------------+
C                I  Z(IBUF1)  I  GINO-BUFFER
C                I            I
C                I  Z(LCORE)  I
C                +------------+
C
C  INPUTS - SCR2 CONTAINING ACTUAL ELEMENT ENTRIES USED TO FORM
C                OES1M FOR CURRENT OES1 SUBCASE. MAY BE MORE THAN
C                ONE -MCSID-. HAS THE SIX SIGMAS OF EACH ELEMENT
C                APPENDED TO EACH ELEMENT.
C
C                       -ELEMENT-ENTRY-
C
C                        MCSID = MATERIAL COORDINATE SYSTEM ID
C                        SIGMA1-X
C                        SIGMA1-Y
C                        SIGMA1-XY
C                        SIGMA2-X
C                        SIGMA2-Y
C                        SIGMA2-XY
C                        XC  *
C                        YC   * MEAN CENTER OF INDEPENDENT POINT
C                        ZC  *
C                        NPTS = NUMBER OF CONNECTED DEPENDENT GRIDS
C                        EXTERNAL GRID IDS (1 FOR EACH POINT)
C                        X,Y,Z  COMPONENTS OF EACH DEPENDENT GRID
C
C           SCR3 CONTAINING OFP TYPE -ID- RECORD TO USE AS A MODEL
C                FOR OES1G -ID- RECORD.
C
C
C           TABLE(IMCSID) THRU Z(NMCSID) CONTAINS PAIRS OF MCSID-S AND
C                COUNTS. (ONE PAIR FOR EACH UNIQUE MCSID OF CURRENT
C                SUBCASE.)
C
C
C           TABLE  Z(ICSTM) TO Z(NCSTM) CONTAINING TRANSFORMATIONS
C
C*****
      REAL               Z(1)     ,RBUF(100)
C
      INTEGER            MCB(7)
C
      INTEGER            CSTMS    ,SCR1     ,SCR2     ,SCR3
      INTEGER            SCR4     ,OES1M    ,OES1G    ,OES1     ,SCR5
      INTEGER            CSTM     ,EST      ,SIL      ,GPL
      INTEGER            ELTYPE   ,SUBCAS   ,FILE     ,ESTWDS
      INTEGER            EWORDS   ,OWORDS   ,DEPTS    ,CSTYPE
      INTEGER            DEVICE   ,OLDID    ,BUF      ,SBUF
      INTEGER            RD       ,RDREW    ,WRT      ,WRTREW
      INTEGER            CLS      ,CLSREW   ,EOR      ,SYSBUF
C
      LOGICAL            ANY      ,EOFOS1   ,FIRST    ,ANYOUT
      LOGICAL            FOES1G   ,STRAIN   ,ANY1M    ,ANY1G
C
      COMMON/BLANK /     IP1      ,IP2
C
      COMMON/SYSTEM/     SYSBUF   ,IOUTPT
C
      COMMON/NAMES /     RD       ,RDREW    ,WRT      ,WRTREW
     1                  ,CLSREW   ,CLS
C
      COMMON/ZZZZZZ/     IZ(1)
C
      COMMON/CURVC1/     LSBUF    ,SBUF(10)
C
      COMMON/CURVC2/     LBUF     ,BUF(100)
C
      COMMON/CURVC3/     VEC(3)   ,VMAX(3)  ,VMIN(3)  ,IDREC(146)
C
      COMMON/CURVTB/     IMID     ,NMID     ,LMID     ,NMIDS
     A                  ,IELTYP   ,NELTYP   ,JELTYP   ,ICSTM
     B                  ,NCSTM    ,CSTMS    ,LCSTM    ,IESTX
     C                  ,NESTX    ,IMCSID   ,NMCSID   ,LMCSID
     D                  ,MCSIDS   ,JMCSID   ,KMCSID   ,ISIL
     E                  ,NSIL     ,LSIL     ,JSIL     ,IOES1M
     F                  ,NOES1M   ,LOES1M   ,IDEP     ,NDEP
     G                  ,IINDEP   ,NINDEP   ,JINDEP   ,ISIGMA
     H                  ,NSIGMA   ,IGMAT    ,NGMAT    ,IEXT
     I                  ,NEXT     ,LEXT     ,SCR1     ,SCR2
     J                  ,SCR3     ,SCR4     ,OES1M    ,OES1G
     K                  ,OES1     ,MPT      ,CSTM     ,EST
     L                  ,SIL      ,GPL      ,JCORE    ,LCORE
     M                  ,IBUF1    ,IBUF2    ,IBUF3    ,IBUF4
     N                  ,I        ,J        ,K        ,L
     O                  ,K1       ,K2       ,IXYZ1    ,IXYZ2
     P                  ,LX1      ,LX2      ,ELTYPE   ,MCSID
     Q                  ,IDSCR1   ,IDOES1   ,NPTS     ,NPTS4
     R                  ,IWORDS   ,NWORDS   ,SUBCAS   ,KOUNT
     S                  ,ISIG1    ,ISIG2    ,LOC      ,FILE
      COMMON/CURVTB/     IMSG     ,NELEMS   ,IMATID   ,ICOMP
     1                  ,ESTWDS   ,EWORDS   ,JP       ,OWORDS
     2                  ,MATID    ,DEPTS    ,INDPTS   ,ICTYPE
     3                  ,IVMAT    ,ITRAN    ,CSTYPE   ,ISING
     4                  ,DEVICE   ,OLDID    ,ANY      ,EOFOS1
     5                  ,FIRST    ,ANYOUT   ,FOES1G   ,STRAIN
     6                  ,LOGERR   ,ANY1M    ,ANY1G    ,SCR5
C
      EQUIVALENCE        (Z(1),IZ(1)), (BUF(1),RBUF(1))
      EQUIVALENCE        (NOEOR,RDREW), (EOR,CLS)
C
      DATA MCB/ 7*1 /
C
C  BRING OES1G -ID- RECORD INTO CORE AND MODIFY AS NECESSARY.
C
      FILE = SCR3
      LOC = 50
      CALL OPEN(*9001,SCR3,IZ(IBUF1),RDREW)
      CALL READ(*9002,*9003,SCR3,IDREC(1),146,NOEOR,NWORDS)
      CALL CLOSE( SCR3, CLSREW )
C
C
C
C
C  OVERALL LOOP IS ON ENTRIES OF TABLE(IMCSID-NMCSID)
C
      JMCSID = IMCSID
C
  100 MCSID = IZ(JMCSID)
      INDPTS = IZ(JMCSID+1)
      LOC = 100
      IF( INDPTS ) 9000,980,110
C
C  COLLECT DATA REQUIRED FROM SCR2.
C
  110 FILE = SCR2
C
C  CORE ALLOCATION FOR XC, YC, ZC OF EACH INDEPENDENT POINT.
C
      IINDEP = NCSTM + 1
      NINDEP = NCSTM + 3*INDPTS
C
C  CORE ALLOCATION FOR EXT-ID,X,Y,Z OF EACH UNIQUE DEPENDENT POINT.
C  (THE QUANTITY OF DEPENDENT POINTS IS NOT YET KNOWN.)
C
      IDEP = NINDEP + 1
      NDEP = NINDEP
      LOC = 110
      ICRQ = NDEP - JCORE
      IF( NDEP .GT. JCORE ) GO TO 9008
C
      CALL OPEN(*9001,SCR2,IZ(IBUF1),RDREW)
      FILE = SCR3
      CALL OPEN(*9001,SCR3,IZ(IBUF2),WRTREW)
C
      JINDEP = IINDEP
      FILE = SCR2
C
C  FIND -INDPTS- NUMBER OF INDEPENDENT ELEMENT POINTS ENTRIES
C  FOR CURRENT -MCSID- PASS. (LOGIC ERROR IF CAN NOT FIND THIS MANY)
C
      DO 400 I = 1,INDPTS
C
C  READ ELEMENT INDEPENDENT PORTION OF ENTRY
C
  150 LOC = 150
      CALL READ(*9002,*9003,SCR2,BUF(1),11,NOEOR,NWORDS)
      NPTS = BUF(11)
      NPTS4 = 4*NPTS
C
C  CHECK MCSID OF ENTRY TO BE SAME AS ONE OF THIS PASS.
C
      IF( BUF(1) .EQ. MCSID ) GO TO 170
C
C  NO IT IS NOT THUS SKIP BALANCE OF ENTRY.
C
      LOC = 170
      CALL READ(*9002,*9003,SCR2,0,-NPTS4,NOEOR,NWORDS)
      GO TO 150
C
C  YES, THIS ENTRY IS OF CURRENT PASS MCSID. ADD POINT DATA TO CORE.
C  FIRST OUTPUT SIGMAS TO SCR3
C
  170 CALL WRITE( SCR3, BUF(2), 6, NOEOR )
      Z(JINDEP  ) = RBUF(8)
      Z(JINDEP+1) = RBUF(9)
      Z(JINDEP+2) = RBUF(10)
      JINDEP = JINDEP + 3
C
C  INDEPENDENT POINTS NOT YET IN CORE ARE ADDED.
C
      CALL READ(*9002,*9003,SCR2,BUF(1),NPTS4,NOEOR,NWORDS)
      K = NPTS
      DO 300 J = 1,NPTS
C
C  CHECK IF EXTERNAL ID IS IN TABLE YET.
C
      IF( NDEP .LT. IDEP ) GO TO 220
      DO 200 L = IDEP,NDEP,4
      IF( BUF(J) .EQ. IZ(L) ) GO TO 290
  200 CONTINUE
C
C  NOT YET IN THUS ADD IT TO TABLE
C
  220 ICRQ = NDEP + 4 - JCORE
      IF( NDEP+4 .GT. JCORE ) GO TO 9008
      IZ(NDEP+1) = BUF(J)
      Z(NDEP+2) = RBUF(K+1)
      Z(NDEP+3) = RBUF(K+2)
      Z(NDEP+4) = RBUF(K+3)
      NDEP = NDEP + 4
C
  290 K = K + 3
C
  300 CONTINUE
C
  400 CONTINUE
C*****
C  ALL DATA FOR CURRENT MCSID HAS BEEN COLLECTED FROM SCR2.
C*****
      CALL CLOSE( SCR2, CLSREW )
      CALL CLOSE( SCR3, CLSREW )
C
C  DEPENDENT COORDINATES ARE SORTED ON EXTERNAL-ID.
C
      CALL SORT( 0, 0, 4, 1, Z(IDEP), NDEP-IDEP+1 )
C*****
C  CONVERSION OF INDEPENDENT AND DEPENDENT POINTS TO LOCAL
C  MATERIAL COORDINATE SYSTEM. FIRST GET CSTM DATA TO USE.
C*****
      LOC = 400
      CALL BISLOC(*9000,MCSID,IZ(ICSTM),14,CSTMS,JP)
      IVMAT = ICSTM + JP + 1
      ITRAN = IVMAT + 3
      ICTYPE = IZ(IVMAT-1)
C
C  FOR EACH POINT
C                               T
C                   (R     )=( T ) ( R     - V     )
C                     LOCAL           BASIC   MCSID
C
C                    (3X1)   (3X3)   (3X1)   (3X1)
C
      DO 480 I = IINDEP,NINDEP,3
      VEC(1) = Z(I  ) - Z(IVMAT  )
      VEC(2) = Z(I+1) - Z(IVMAT+1)
      VEC(3) = Z(I+2) - Z(IVMAT+2)
      CALL GMMATS( Z(ITRAN),3,3,1, VEC(1),3,1,0, Z(I) )
  480 CONTINUE
C
      DO 490 I = IDEP,NDEP,4
      VEC(1) = Z(I+1) - Z(IVMAT  )
      VEC(2) = Z(I+2) - Z(IVMAT+1)
      VEC(3) = Z(I+3) - Z(IVMAT+2)
      CALL GMMATS( Z(ITRAN),3,3,1, VEC(1),3,1,0, Z(I+1) )
  490 CONTINUE
C*****
C  CONVERSION OF INDEPENDENT POINT LOCAL COORDINATES TO MAPPING
C  COORDINATES. (IF MCSID IS A RECTANGULAR SYSTEM THEN NO CHANGE.)
C*****
      LOC = 490
      IF( ICTYPE.LT.1 .OR. ICTYPE.GT.3 ) GO TO 9000
      GO TO ( 589,510,530 ),ICTYPE
C
C  CYLINDRICAL COORDINATES
C
  510 AVGL = 0.0
      DO 520 I = IINDEP,NINDEP,3
      VEC(1) = SQRT( Z(I)**2 + Z(I+1)**2 )
      AVGL = AVGL + VEC(1)
      IF( VEC(1) .LE. 0.0 ) GO TO 515
      Z(I+1) = ATAN2( Z(I+1), Z(I) )
      GO TO 517
  515 Z(I+1) = 0.0
  517 Z(I) = VEC(1)
  520 CONTINUE
      AVGL = AVGL / FLOAT(INDPTS)
      GO TO 589
C
C  SPHERICAL COORDINATES
C
  530 AVGL = 0.0
      DO 580 I = IINDEP,NINDEP,3
      XSQYSQ = Z(I)**2 + Z(I+1)**2
      FL = SQRT( XSQYSQ )
      VEC(1) = SQRT( XSQYSQ + Z(I+2)**2 )
      AVGL = AVGL + VEC(1)
      IF( VEC(1) .GT. 0.0 ) GO TO 540
      VEC(2) = 0.0
      GO TO 550
  540 VEC(2) = ATAN2( FL, Z(I+2) )
  550 IF( FL .GT. 0.0 ) GO TO 560
      VEC(3) = 0.0
      GO TO 570
  560 VEC(3) = ATAN2( Z(I+1), Z(I) )
  570 Z(I  ) = VEC(1)
      Z(I+1) = VEC(2)
      Z(I+2) = VEC(3)
  580 CONTINUE
      AVGL = AVGL / FLOAT(INDPTS)
C*****
C  CONVERSION OF DEPENDENT POINT LOCAL COORDINATES TO MAPPING
C  COORDINATES.
C  (IF MCSID IS RECTANGULAR SYSTEM THEN NO CHANGE.)
C*****
  589 GO TO(609,590,600), ICTYPE
C
C  CYLINDRICAL COORDINATES
C
  590 DO 594 I = IDEP,NDEP,4
      VEC(1) = SQRT( Z(I+1)**2 + Z(I+2)**2 )
      IF( VEC(1) .LE. 0.0 ) GO TO 592
      Z(I+2) = ATAN2( Z(I+2), Z(I+1) )
      GO TO 593
  592 Z(I+2) = 0.0
  593 Z(I+1) = VEC(1)
  594 CONTINUE
      GO TO 609
C
C  SPHERICAL COORDINATES
C
  600 DO 607 I = IDEP,NDEP,4
      XSQYSQ = Z(I+1)**2 + Z(I+2)**2
      FL = SQRT( XSQYSQ )
      VEC(1) = SQRT( XSQYSQ + Z(I+3)**2 )
      IF( VEC(1) .GT. 0.0 ) GO TO 602
      VEC(2) = 0.0
      GO TO 604
  602 VEC(2) = ATAN2( FL, Z(I+3) )
  604 IF( FL .GT. 0.0 ) GO TO 605
      VEC(3) = 0.0
      GO TO 606
  605 VEC(3) = ATAN2( Z(I+2), Z(I+1) )
  606 Z(I+1) = VEC(1)
      Z(I+2) = VEC(2)
      Z(I+3) = VEC(3)
  607 CONTINUE
C
C  SET MAXIMUM AND MIMIMUM X,Y,Z VALUES.
C
  609 DO 610 I = 1,3
      VMAX(I) = Z(IINDEP+I-1)
      VMIN(I) = Z(IINDEP+I-1)
  610 CONTINUE
C
      DO 650 I = IINDEP,NINDEP,3
      DO 640 J = 1,3
      VMAX(J) = AMAX1( Z(I+J-1), VMAX(J) )
      VMIN(J) = AMIN1( Z(I+J-1), VMIN(J) )
  640 CONTINUE
  650 CONTINUE
C
C  SET THE X,Y,Z RANGES
C
      DO 670 I = 1,3
      VMAX(I) = VMAX(I) - VMIN(I)
      VEC(I) = VMAX(I)
  670 CONTINUE
C
      IF( ICTYPE .EQ. 1 ) GO TO 680
      VMAX(2) = AVGL * VMAX(2)
      IF( ICTYPE .EQ. 2 ) GO TO 680
      VMAX(3) = AVGL * VMAX(3)
C
C  DIRECTION YIELDING MINIMUM RANGE DETERMINES PROJECTION
C
  680 IF( VMAX(1) .LT. VMAX(2) ) GO TO 700
      IF( VMAX(2) .LT. VMAX(3) ) GO TO 690
  685 K1 = 1
      K2 = 2
      KCTYPE = 3
      GO TO 710
  690 K1 = 1
      K2 = 3
      KCTYPE = 2
      GO TO 710
  700 IF( VMAX(3) .LT. VMAX(1) ) GO TO 685
      K1 = 2
      K2 = 3
      KCTYPE = 1
C
  710 XRANGE = VEC(K1)
      YRANGE = VEC(K2)
      IF( XRANGE ) 712,711,712
  711 XRANGE = 1.0
  712 IF( YRANGE ) 714,713,714
  713 YRANGE = 1.0
C
C  COORDINATES -K1- AND -K2- WILL BE KEPT.
C
C  TABLE OF INDEPENDENT AND DEPENDENT POINTS ARE REDUCED TO
C  TABLES OF X,Y PAIRS. FIRST TO GAIN SOME CORE, EXTERNAL
C  IDS- ARE WRITTEN TO SCR4.
C
  714 FILE = SCR4
      LOC = 714
      CALL OPEN(*9001,SCR4,IZ(IBUF1),WRTREW)
      DO 720 I = IDEP,NDEP,4
      CALL WRITE( SCR4, IZ(I), 1, NOEOR )
  720 CONTINUE
      CALL CLOSE( SCR4, CLSREW )
C
C  REDUCE INDEPENDENT POINTS TO XY PAIRS, SCALE BY X AND Y RANGES
C  RESPECTIVELY, AND COMPRESS IN CORE.
C
      J = IINDEP
      DO 740 I = IINDEP,NINDEP,3
      Z(J  ) = Z(I+K1-1) / XRANGE
      Z(J+1) = Z(I+K2-1) / YRANGE
      J = J + 2
  740 CONTINUE
      NINDEP = J - 1
C
C  REDUCE DEPENDENT POINTS LIST. (J IS STILL GOOD)
C
      DO 770 I=IDEP,NDEP,4
      Z(J  ) = Z(I+K1) / XRANGE
      Z(J+1) = Z(I+K2) / YRANGE
      J = J + 2
  770 CONTINUE
      IDEP = NINDEP + 1
      NDEP = J - 1
      DEPTS = (NDEP - IDEP + 1) / 2
C*****
C  INDEPENDENT AND DEPENDENT POINT COORDINATE LISTS ARE NOW
C  COMPLETE.  CALL FOR INTERPOLATION.
C*****
      CALL CURVIT( Z(IINDEP), INDPTS, Z(IDEP), DEPTS, SCR5,
     1        Z(NDEP+1), IZ(NDEP+1), LCORE-NDEP-1, IP2, 15.0, MCSID,
     2        XRANGE, YRANGE )
C
C  BRING -OES1M- SIGMAS INTO CORE FOR CURRENT -MCSID- PASS.
C
      ISIGMA = IINDEP + 1
      NSIGMA = IINDEP + 6*INDPTS
      JSIGMA = ISIGMA - 7
      ICRQ = NSIGMA - IBUF3
      IF( NSIGMA .GE. IBUF3 ) GO TO 9008
      FILE = SCR3
      LOC = 800
      CALL OPEN(*9001,SCR3,IZ(IBUF1),RDREW)
      CALL READ(*9002,*810,SCR3,IZ(ISIGMA),IBUF3-ISIGMA,NOEOR,NWORDS)
      LOC = 810
      GO TO 9000
C
  810 IF( NWORDS .NE. 6*INDPTS ) GO TO 9000
      CALL CLOSE( SCR3, CLSREW )
C
C    (SIGMAS                ) = (G)(SIGMAS                        )
C           DEPENDENT POINTS              OES1M INDEPENDENT POINTS
C
C  SINCE THE ORDER OF THE ROWS IN THE G MATRIX ARE IN SORTED EXTERNAL
C  GRID ORDER EACH OUTPUT LINE OF OES1G WILL BE HANDLED ON ITS
C  OWN. THIS ELIMINATES NECESSITY OF HOLDING ANOTHER SIGMA ARRAY
C  IN CORE.
C
      FILE = OES1G
      LOC = 815
      CALL OPEN(*9001,OES1G,IZ(IBUF1),WRT)
C
C  OUTPUT ID RECORD. PREVIOUSLY PREPARED.
C
      IDREC(3) = IDREC(3) + 2000
      CALL WRITE( OES1G, IDREC(1), 146, EOR )
      MCB(1) = OES1G
      CALL WRTTRL( MCB(1) )
      ANY1G = .TRUE.
C
C  OPEN SCR5 CONTAINING ROWS OF THE G-MATRIX.
C
      FILE = SCR5
      CALL OPEN(*9001,SCR5,IZ(IBUF3),RDREW)
      CALL FWDREC(*9002,SCR5)
C
C  OPEN SCR4 CONTAINING LIST OF EXTERNAL IDS )
C
      FILE = SCR4
      CALL OPEN(*9001,SCR4,IZ(IBUF2),RDREW)
C
C  COMPUTE AND OUTPUT SIGMAS FOR THE DEPENDENT POINTS
C
      BUF(2) = MCSID
      DO 900 I=1,DEPTS
C
C  READ THE EXTERNAL ID
C
      FILE = SCR4
      CALL READ(*9002,*9003,SCR4,BUF(1),1,NOEOR,NWORDS)
      FILE = SCR5
C
C  INITIALIZE SIGMAS(DEPENDENT POINT) TO ZERO
C
      DO 820 J = 3,8
      RBUF(J) = 0.0
  820 CONTINUE
C
      K = 0
      LOC = 825
C
C  READ ACTIVE INDEX AND G-VALUE FROM SCRATCH 5
C
  825 CALL READ(*9002,*840,SCR5,RBUF(11),2,NOEOR,NWORDS)
      K = K + 10
      IDX = JSIGMA + 6*BUF(11)
      DO 830 J = 1,6
      RBUF(J+2) = RBUF(J+2) + RBUF(12)*Z(IDX+J)
  830 CONTINUE
      GO TO 825
C
C  IF THERE WERE ANY G-VALUES THEN NOW COMPLETE THE OUTPUT LINE.
C
  840 IF( K .LE. 0 ) GO TO 900
C
      BUF(10) = K + KCTYPE
C
      RBUF(11) = RBUF(6)
      RBUF(12) = RBUF(7)
      RBUF(13) = RBUF(8)
C
C  COMPUTE INVARIANTS FOR EACH LINE
C
      CALL CURVPS( RBUF( 3), RBUF( 6) )
      CALL CURVPS( RBUF(11), RBUF(14) )
      IF( .NOT. STRAIN ) GO TO 881
      RBUF(5) = 2.0 * RBUF(5)
      RBUF(9) = 2.0 * RBUF(9)
      RBUF(13) = 2.0 * RBUF(13)
      RBUF(17) = 2.0 * RBUF(17)
C
C  APPEND DEVICE CODE TO EXTERNAL ID AND OUTPUT LINE
C
  881 BUF(1) = 10*BUF(1) + DEVICE
      CALL WRITE( OES1G, BUF(1), 17, NOEOR )
  900 CONTINUE
C
      CALL WRITE( OES1G, 0, 0, EOR )
      IF(EOFOS1 .AND. JMCSID+2 .GT. NMCSID) CALL CLOSE(OES1G,CLSREW)
      CALL CLOSE( OES1G, CLS )
      CALL CLOSE( SCR4, CLSREW )
      CALL CLOSE( SCR5, CLSREW )
C*****
C  ALL INDEPENDENT POINTS OUTPUT TO OES1G FOR 1 ACTIVE MCSID OF
C  CURRENT SUBCASE. GO TO NEXT MCSID.
C*****
  980 JMCSID = JMCSID + 2
      IF( JMCSID .LE. NMCSID ) GO TO 100
C*****
C  ALL THROUGH FORMING OES1G FOR CURRENT SUBCASE.
C*****
 5000 RETURN
C*****
C  ERROR CONDITION ENCOUNTERED
C*****
 9000 IMSG = -LOGERR
      GO TO 5000
 9001 IMSG = -1
      GO TO 5000
 9002 IMSG = -2
      GO TO 5000
 9003 IMSG = -3
      GO TO 5000
 9008 IMSG = -8
      LCORE = ICRQ
      GO TO 5000
      END
