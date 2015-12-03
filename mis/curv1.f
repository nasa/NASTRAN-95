      SUBROUTINE CURV1
C*****
C  INITIALIZATION OVERLAY. ALL LOGIC INDEPENDENT OF PROCESSING
C  THE SUBCASE DATA ON OES1 IS HANDLED IN THIS INITIALIZATION
C  ROUTINE OF THE -CURV- MODULE
C
C     OPEN CORE MAP DURING -CURV1- EXECUTION.
C     =======================================
C       INITIAL               AFTER CURV1 RETURNS
C     +-----------+             +----------------+
C     I Z(IELTYP) I             I  Z(IELTYP)     I
C     I    .      I             I     .          I
C     I  ELEMENT  I             I  REDUCED       I
C     I  TYPES    I             I  ELEMENT-TYPES I
C     I  BEING    I             I  LIST          I
C     I  PLACED   I             I     .          I
C     I  IN SCR1  I             I  Z(NELTYP)     I
C     I    .      I             +----------------+
C     I Z(NELTYP) I             I  Z(IMCSID)     I
C     +-----------+             I     .          I
C     I Z(IMID)   I             I  MCSID LIST    I
C     I    .      I             I  OF MCSIDS     I
C     I MATID-    I             I  ACTUALLY      I
C     I MCSID-    I             I  REFERENCED    I
C     I FLAG-     I             I     .          I
C     I ENTRIES   I             I  Z(NMCSID)     I
C     I    .      I             +----------------+
C     I Z(NMID)   I             I  Z(ICSTM)      I
C     +-----------+             I     .          I
C     I Z(ISIL)   I             I  CSTMS IN      I
C     I    .      I             I  EXISTENCE     I
C     I SILS IN   I             I  FOR MCSIDS    I
C     I INTERNAL  I             I  IN ABOVE      I
C     I SORT      I             I  TABLE         I
C     I    .      I             I     .          I
C     I Z(NSIL)   I             I  Z(NCSTM)      I
C     +-----------+             +----------------+
C     I Z(IEXT)   I             I     .          I
C     I    .      I             I  AVAILABLE     I
C     I EXTERNAL  I             I  CORE          I
C     I IDS IN    I             I     .          I
C     I INTERNAL  I             I     .          I
C     I SORT      I             I     .          I
C     I    .      I             I     .          I
C     I Z(NEXT)   I             I     .          I
C     +-----------+             I     .          I
C     I   .       I             I     .          I
C     I AVAILABLE I             I     .          I
C     I CORE      I             I     .          I
C     I   .       I             I     .          I
C     I Z(JCORE)  I             I  Z(JCORE)      I
C     +-----------+             +----------------+
C     I Z(IBUF4)  I             I  Z(IBUF4)      I
C     I Z(IBUF3)  I             I  Z(IBUF3)      I
C     I Z(IBUF2)  I             I  Z(IBUF2)      I
C     I Z(IBUF1)  I             I  Z(IBUF1)      I
C     I GINO-BUFS I             I  GINO-BUFS     I
C     I Z(LCORE)  I             I  Z(LCORE)      I
C     +-----------+             +----------------+
C
C*****
      REAL               Z(1)     ,RBUF(100)
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
      INTEGER            MAT(6)   ,ELEM(5,4)
C
      LOGICAL            ANY      ,EOFOS1   ,FIRST    ,ANYOUT
      LOGICAL            FOES1G   ,STRAIN   ,ANY1M    ,ANY1G
C
      COMMON/BLANK /     IP1      ,IP2      ,ICMPLX   ,ZDUM(3)
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
      DATA MAT / 103,1,12,   203,2,17  /
C
C        - - - - - - - - CURV-MODULE ELEMENTS DATA - - - - - - - -
C
C                   ELEMENT   EST       CONNECT.  MATID     BGPDT
C                   TYPE      WORDS     POINTS    INDEX     INDEX
C                   =======   =======   =======   =======   =======
C  TRIA1
      DATA ELEM /   6        ,27       ,3        ,6        ,15
C  TRIA2
     *             ,17       ,21       ,3        ,6        ,9
C  QUAD1
     *             ,19       ,32       ,4        ,7        ,16
C  QUAD2
     *             ,18       ,26       ,4        ,7        ,10       /
C
C  IF EITHER OF THESE PARAMS IS EXCEEDED RESET AND RE-DIMENSION
C  SBUF OR BUF.
C
      LSBUF  = 10
      LBUF   = 100
      NELEMS = 4
      LOGERR = 37
C*****
C  INITIALIZATION OF CORE AND FLAGS
C*****
      FOES1G = .TRUE.
      IF (IP1.GT.0) FOES1G = .FALSE.
      ANY1M = .FALSE.
      ANY1G = .FALSE.
      LMCSID = 0
C
      LCORE = KORSZ( IZ(1) )
      DO 100 I = 1,LCORE
      IZ(I) = 0
  100 CONTINUE
      IBUF1 = LCORE - SYSBUF
      IBUF2 = IBUF1 - SYSBUF
      IBUF3 = IBUF2 - SYSBUF
      IBUF4 = IBUF3 - SYSBUF
C
C  SET FILE NUMBERS EXPLICITYLY.  ALL OVERLAYS REFERENCE /CURVTB/
C
      OES1  = 101
      MPT   = 102
      CSTM  = 103
      EST   = 104
      SIL   = 105
      GPL   = 106
      OES1M = 201
      OES1G = 202
      SCR1  = 301
      SCR2  = 302
      SCR3  = 303
      SCR4  = 304
      SCR5 = 305
      JCORE = IBUF4 - 1
      FILE = 0
      LOC = 300
      ICRQ =-IBUF4
      IF( IBUF4 ) 9008,9008,300
C*****
C  ALLOCATE TABLE OF ELEMENT TYPES PLACED ON ESTX(SCR1).  MAXIMUM
C  SIZE NOW AND REDUCED LATER TO ACTUAL SIZE.
C*****
  300 IELTYP = 1
      JELTYP = IELTYP
      NELTYP = NELEMS
C*****
C  CONSTRUCTION OF TABLE CONTAINING ENTRIES OF,
C
C     MID   = MATERIAL-ID
C     MCSID = MATERIAL-COORDINATE-SYSTEM-ID
C     FLAG  = REFERENCE-FLAG
C
C  ALL MAT1 AND MAT2 BULK DATA CARDS CONTAINING A NON-ZERO -MCSID-
C  RESULT IN AN ENTRY BEING ADDED TO THIS TABLE. TABLE IS THEN SORTED
C  ON -MID-.
C*****
      IMID = NELTYP + 1
      NMID = IMID - 1
C
C  OPEN MPT USING -PRELOC- FUNCTION.
C
      FILE = MPT
      LOC = 400
      CALL PRELOC(*9001,IZ(IBUF1),MPT)
C
C  PASS MAT1 AND MAT2 DATA IF ANY.
C
      DO 480 I = 1,6,3
      IWORDS = MAT(I+2)
      IF( IWORDS .GT. LBUF ) GO TO 9000
      CALL LOCATE(*480,IZ(IBUF1),MAT(I),IDUM)
  410 CALL READ(*9002,*480,MPT,BUF(1),IWORDS,NOEOR,NWORDS)
      IF( BUF(IWORDS) .LE. 0 ) GO TO 410
      ICRQ = NMID + 3 - JCORE
      IF( NMID+3 .GT. JCORE ) GO TO 9008
      IZ(NMID+1) = BUF(1)
      IZ(NMID+2) = BUF(IWORDS)
      IZ(NMID+3) = 0
      NMID = NMID + 3
      GO TO 410
C
C  EOR HIT READING MAT1 OR MAT2 CARDS
C
  480 CONTINUE
C
C  TABLE COMPLETE, THUS NOW SORT IT. IF TABLE IS EMPTY WE ARE THROUGH
C
      CALL CLOSE( MPT, CLSREW )
      LMID = NMID - IMID + 1
      NMIDS = LMID / 3
      LOC = 570
      IF (LMID) 9000, 950, 570
  570 CALL SORT( 0, 0, 3, 1, IZ(IMID), LMID )
C*****
C  LOAD LIST OF SILS INTO CORE, FOLLOWED BY LIST OF EXTERNAL IDS.
C  THIS IS REQUIRED ONLY IF OES1G IS TO BE FORMED.
C*****
      IF( .NOT. FOES1G ) GO TO 630
      FILE = SIL
      LOC = 580
      ISIL = NMID + 1
      CALL GOPEN( SIL, IZ(IBUF1), 0 )
      CALL READ(*9002,*580,SIL,IZ(ISIL),JCORE-ISIL,NOEOR,LSIL)
      ICRQ = JCORE - ISIL
      GO TO 9008
C
  580 NSIL = ISIL + LSIL - 1
      CALL CLOSE( SIL, CLSREW )
C
      FILE = GPL
      LOC = 590
      IEXT = NSIL + 1
      CALL GOPEN( GPL, IZ(IBUF1), 0 )
      CALL READ(*9002,*590,GPL,IZ(IEXT),JCORE-IEXT,NOEOR,LEXT)
      ICRQ = JCORE - IEXT
      GO TO 9008
C
  590 NEXT = IEXT + LEXT - 1
      CALL CLOSE( GPL, CLSREW )
      IF( LSIL .NE. LEXT ) GO TO 9000
C*****
C  EST IS NOW READ. ANY ELEMENTS IN THE EST WHOSE MATERIAL ID REFERENCES
C  A MAT1 OR MAT2 ENTRY WHICH CONTAINS A NON-ZERO MATERIAL-COORDINATE-
C  SYSTEM-ID, WILL BE PLACED IN AN ABBREVIATED EST ON SCRATCH1.
C
C  FORMAT OF EACH ELEMENT TYPE RECORD.
C
C         ELEMENT TYPE NUMBER
C         NUMBER OF WORDS PER EACH OF THE FOLLOWING ENTRIES.
C         NUMBER OF POINTS PER THIS ELEMENT TYPE.
C
C        * ELEMENT-ID
C       *  MCSID = MATERIAL-COORDINATE-SYSTEM-ID
C ENTRY*
C       *  EXTERNAL-GRID-IDS THIS ELEMENT CONNECTS (1 OR MORE)
C        * X,Y,Z BASIC COORDINATE SETS OF EACH CONNECTED POINT(1 OR MORE
C
C           ( ABOVE ELEMENT ENTRY REPEATS FOR EACH ELEMENT
C             REFERENCING A MAT1 OR MAT2 CARD HAVING A NON-ZERO MCSID.)
C
C*****
  630 LOC = 630
      FILE = SCR1
      CALL OPEN(*9001,SCR1,IZ(IBUF2),WRTREW)
      FILE = EST
      CALL GOPEN(  EST, IZ(IBUF1), 0 )
C
      OLDID = -99999998
C
C  READ ELEMENT TYPE OF NEXT EST RECORD AND DETERMINE IF IT IS
C  AMONG ELEMENT TYPES TO BE EVEN CONSIDERED.
C
  645 LOC = 645
      CALL READ(*800,*9003,EST,ELTYPE,1,NOEOR,NWORDS)
      DO 650 I = 1,NELEMS
      IF( ELTYPE .EQ. ELEM(1,I) ) GO TO 670
  650 CONTINUE
      CALL FWDREC(*9002,EST)
      GO TO 645
C
C  OK THIS  ELEMENT TYPE RECORD IS TO BE CONSIDERED.
C
  670 ESTWDS = ELEM(2,I)
      LOC = 670
      IF( ESTWDS .GT. LBUF ) GO TO 9000
      ANY    = .FALSE.
      NPTS   = ELEM(3,I)
      IMATID = ELEM(4,I)
      IXYZ1 = ELEM(5,I)
      IXYZ2 = IXYZ1 + 4*NPTS - 1
      K1 = 2 + NPTS
      LOC = 680
      IF( K1 .GT. LSBUF ) GO TO 9000
C
C  READ AN ELEMENT ENTRY AND CHECK TO DETERMINE IF IT IS TO BE USED.
C
  690 LOC = 690
      CALL READ(*9002,*780,EST,BUF(1),ESTWDS,NOEOR,NWORDS)
      MATID = BUF(IMATID)
      IF( MATID .EQ. OLDID ) GO TO 730
      CALL BISLOC(*690,MATID,IZ(IMID),3,NMIDS,JP)
      MCSID = IZ(IMID+JP)
      OLDID = MATID
      IZ(IMID+JP+1) = 7
C
C  DEVELOP AND OUTPUT ABBREVIATED ENTRY TO SCRATCH1.
C  (INITIALIZE RECORD WITH THREE-WORD-HEADER ENTRY.)
C
  730 IF( ANY ) GO TO 733
      SBUF(1) = ELTYPE
      SBUF(2) = 4*NPTS + 2
      SBUF(3) = NPTS
      CALL WRITE( SCR1, SBUF(1), 3, NOEOR )
      IZ(JELTYP) = ELTYPE
      JELTYP = JELTYP + 1
      ANY = .TRUE.
C
  733 SBUF(1) = BUF(1)
      SBUF(2) = MCSID
C
C  CONVERT SILS TO EXTERNAL-IDS IF OES1G IS TO BE BUILT
C
      IF( FOES1G ) GO TO 740
      DO 735 I=3,K1
      SBUF(I) = 0
  735 CONTINUE
      GO TO 760
C
  740 JSIL = 2
      LOC = 740
      DO 750 I = 3,K1
      CALL BISLOC(*9000,BUF(JSIL),IZ(ISIL),1,LSIL,JP)
      SBUF(I) = IZ(IEXT+JP-1)
      JSIL = JSIL + 1
  750 CONTINUE
C
C  OUTPUT THIS PORTION OF ENTRY AND THEN XYZ COMPONENTS OF CONNECTED
C  POINTS
C
  760 CALL WRITE( SCR1, SBUF(1), NPTS+2, NOEOR )
C
      DO 770 I = IXYZ1,IXYZ2,4
      CALL WRITE( SCR1, BUF(I+1), 3, NOEOR )
  770 CONTINUE
C
C  GO FOR NEXT ELEMENT OF THIS TYPE
C
      GO TO 690
C
C  END OF ENTRIES FUR CURRENT ELEMENT TYPE.
C
  780 LOC = 780
      IF( NWORDS .NE. 0 ) GO TO 9000
      IF( ANY ) CALL WRITE( SCR1, 0, 0, EOR )
      GO TO 645
C
C  END OF ALL ELEMENT TYPES IN EST
C
  800 CALL CLOSE(  EST, CLSREW )
      CALL CLOSE( SCR1, CLSREW )
C*****
C  REDUCTION OF MATERIAL-ID AND COORDINATE-SYSTEM-ID TO THOSE
C  ACTUALLY REFERENCED BY ELEMENTS BEING CONSIDERED.
C*****
      NELTYP = JELTYP - 1
C
C  RESORT MID-MCSID TABLE ON MCSID.
C
      CALL SORT( 0, 0, 3, 2, IZ(IMID), LMID )
      IMCSID = NELTYP + 1
      NMCSID = NELTYP
      LOC = 820
      OLDID = 0
      DO 840 I = IMID,NMID,3
      IF( IZ(I+2) ) 9000,840,820
C
C  ELIMINATE DUPLICATE MCSIDS.
C
  820 IF( IZ(I+1) .EQ. OLDID ) GO TO 840
      OLDID = IZ(I+1)
      NMCSID = NMCSID + 2
      IZ(NMCSID-1) = IZ(I+1)
      IZ(NMCSID  ) = 0
  840 CONTINUE
      LMCSID = NMCSID - IMCSID + 1
      MCSIDS = LMCSID / 2
C
C  IF TABLE IS NOW EMPTY THERE IS NOTHING MORE TO DO
C
      LOC = 860
      IF (LMCSID) 9000, 950, 860
C*****
C  COORDINATE SYSTEMS WHICH MAY BE REFERENCED ARE AT THIS TIME
C  PULLED INTO CORE FROM THE -CSTM- DATA BLOCK. (SILS AND EXTERNAL-IDS
C  TABLES IF IN CORE ARE NO LONGER REQUIRED.)
C*****
  860 ICSTM = NMCSID + 1
      NCSTM = NMCSID
      FILE = CSTM
      CALL GOPEN( CSTM, IZ(IBUF1), 0 )
  870 ICRQ = NCSTM + 14 - JCORE
      IF( NCSTM+14 .GT. JCORE ) GO TO 9008
  880 CALL READ(*9002,*900,CSTM,IZ(NCSTM+1),14,NOEOR,NWORDS)
      KID = IZ(NCSTM+1)
      CALL BISLOC (*880, KID, IZ(IMCSID), 2, MCSIDS, JP)
      NCSTM = NCSTM + 14
      GO TO 870
C
C  END OF COORDINATE SYSTEM DATA
C
  900 CALL CLOSE( CSTM, CLSREW )
      LCSTM = NCSTM - ICSTM + 1
      CSTMS = LCSTM / 14
      CALL SORT( 0, 0, 14, 1, Z(ICSTM), LCSTM )
      CALL PRETRS( IZ(ICSTM), LCSTM )
C*****
C  INITIALIZE INPUT AND OUTPUT FILE POSITIONS.
C*****
  950 CALL GOPEN (OES1, IZ(IBUF1), 0)
C
C  CHECK FOR STRAIN OPTION
C
      FILE = OES1
      LOC = 910
      CALL READ(*9002,*9003,OES1,IDREC(1),2,0,FLAG)
      I = IDREC(2)
      IF (I.NE.5.AND.I.NE.21.AND.I.NE.1005) GO TO 9000
      STRAIN = .FALSE.
      IF (I.EQ.21) STRAIN = .TRUE.
      ICMPLX = 0
      IF (I.EQ.1005) ICMPLX = 1
      CALL BCKREC (OES1)
C
      CALL CLOSE( OES1, CLS )
      EOFOS1 = .FALSE.
C
      CALL GOPEN( OES1M, IZ(IBUF1), 1 )
      CALL CLOSE( OES1M, CLS )
C
      IF( .NOT. FOES1G ) GO TO 5000
      CALL GOPEN( OES1G, IZ(IBUF1), 1 )
      CALL CLOSE( OES1G, CLS )
C*****
C  END OF INITIALIZATION
C*****
 5000 RETURN
C*****
C  ERROR CONDITION ENCOUNTERED.
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
