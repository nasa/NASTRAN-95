      SUBROUTINE RCOVC
C
C     RCOVC COMPUTES REACTION FORCES AND GENERATES OUTPUT DATA BLOCKS
C     FOR DISPLACEMENTS, APPLIED LOADS, AND REACTION FORCES.
C
      LOGICAL         INCORE     ,UFLAG      ,PFLAG      ,NON0       ,
     1                QFLAG      ,END        ,ONCE       ,COMPLX     ,
     2                SUPRES     ,KEEP
      INTEGER         DRY        ,STEP       ,FSS        ,RFNO       ,
     1                UINMS      ,UA         ,RSS        ,SYSBUF     ,
     2                UTYPO      ,SOF2       ,SOF3       ,BUF1       ,
     3                BUF2       ,CASESS     ,SOF1       ,OUGV1      ,
     4                OPG1       ,OQG1       ,SCR1       ,EQSS       ,
     5                SOLN       ,ENERGY     ,PVEC       ,UVEC       ,
     6                NAME(2)    ,CASECC(2)  ,SRD        ,PG         ,
     7                SUBSTR(4)  ,IZ(2)      ,RC         ,FILE       ,
     8                QA         ,PA         ,NAMEF(2)   ,IDBUF(146) ,
     9                DISP(3)    ,OLOAD(3)   ,SPCF(3)    ,DOFS(32)   ,
     O                BUF3       ,NFWD(3)    ,COMPS(3)   ,BUF(1)     ,
     1                ACCE(3)    ,VELO(3)    ,BUF4       ,SCR2       ,
     2                SCR6       ,SCR7       ,SCR8       ,SCR3
      DIMENSION       MCBA(7)    ,RBUF(1)    ,RDBUF(7)   ,DATA(12)
      CHARACTER       UFM*23     ,UWM*25     ,UIM*29     ,SFM*25     ,
     1                SWM*27
      COMMON /XMSSG / UFM        ,UWM        ,UIM        ,SFM        ,
     1                SWM
      COMMON /BLANK / DRY        ,LOOP       ,STEP       ,FSS(2)     ,
     1                RFNO       ,NEIGV      ,LUI        ,UINMS(2,5) ,
     2                NOSORT     ,UTHRES     ,PTHRES     ,QTHRES
      COMMON /RCOVCR/ ICORE      ,LCORE      ,BUF1       ,BUF2       ,
     1                BUF3       ,BUF4       ,SOF1       ,SOF2       ,
     2                SOF3
      COMMON /RCOVCM/ MRECVR     ,UA         ,PA         ,QA         ,
     1                IOPT       ,RSS(2)     ,ENERGY     ,UIMPRO     ,
     2                RANGE(2)   ,IREQ       ,LREQ       ,LBASIC
      COMMON /SYSTEM/ SYSBUF     ,NOUT
      COMMON /NAMES / RD         ,RDREW      ,WRT        ,WRTREW     ,
     1                REW        ,NOREW
      COMMON /UNPAKX/ UTYPO      ,IRU        ,NRU        ,INCU
      COMMON /CONDAS/ PHI        ,TWOPHI     ,RADDEG
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (BUF(1)    ,Z(1))
      EQUIVALENCE     (Z(1)      ,IZ(1)),    (BUF(1)     ,RBUF(1))   ,
     1                (IDBUF(1)  ,RDBUF(1))
      DATA    CASESS, OUGV1      ,OPG1       ,OQG1       ,SCR1       ,
     1        PG    , SCR3       ,SCR6       ,SCR7       ,SCR8       ,
     2        SCR2  /
     3        101   , 201        ,202        ,203        ,301        ,
     4        105   , 303        ,306        ,307        ,308        ,
     5        302   /
      DATA    SRD   / 1          /
      DATA    EQSS  , SOLN       ,UVEC       ,PVEC       /
     1        4HEQSS, 4HSOLN     ,4HUVEC     ,4HPVEC     /
      DATA    NAME               ,CASECC                 ,SUBSTR     /
     2        4HRCOV, 4HC        ,4HCASE     ,4HCC       ,4HSUBS     ,
     3        4HTRUC, 4HTURE     ,4H         /
      DATA    COMPS /
     1        4HCOMP, 4HONEN     ,4HT        /
C
C     INITIALIZE
C
      IF (DRY .LT. 0) RETURN
      SOF1 = KORSZ(Z) - LREQ - SYSBUF + 1
      SOF2 = SOF1 - SYSBUF - 1
      SOF3 = SOF2 - SYSBUF
      BUF1 = SOF3 - SYSBUF
      BUF2 = BUF1 - SYSBUF
      BUF3 = BUF2 - SYSBUF
      BUF4 = BUF3 - SYSBUF
      LCORE= BUF4 - 1
      IF (LCORE .LE. 0) GO TO 6313
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
C
C     ================================================
C     THIS CARD SHOULD BE ADDED WHEN SDR3 IS FIXED
C
C     IF (RFNO .EQ. 9) NOSORT = 1
C
C     ================================================
      PA = 0
      QA = 0
      UFLAG = .FALSE.
      PFLAG = .FALSE.
      QFLAG = .FALSE.
C
C     CHECK OUTPUT REQUESTS ON CASESS
C
      CALL GOPEN (CASESS,Z(BUF1),RDREW)
      NCCREC = 1
      FILE   = CASESS
  110 CALL FREAD (CASESS,Z,2,1)
      NCCREC = NCCREC + 1
      IF (IZ(1).NE.CASECC(1) .OR. IZ(2).NE.CASECC(2)) GO TO 110
  120 CALL READ (*130,*9003,CASESS,IDBUF,35,1,I)
      IF (IDBUF(17) .NE. 0) PFLAG = .TRUE.
      IF (IDBUF(20) .NE. 0) UFLAG = .TRUE.
      IF (IDBUF(29).NE.0 .AND. RFNO.GE.8) UFLAG = .TRUE.
      IF (IDBUF(32).NE.0 .AND. RFNO.GE.8) UFLAG = .TRUE.
      IF (IDBUF(35) .NE. 0) QFLAG = .TRUE.
      IF (PFLAG .AND. UFLAG .AND. QFLAG) GO TO 130
      GO TO 120
  130 CALL CLOSE (CASESS,REW)
C
      IF (BUF(IREQ  ) .EQ. 1) UFLAG = .TRUE.
      IF (BUF(IREQ+1) .EQ. 1) PFLAG = .TRUE.
      IF (BUF(IREQ+2) .EQ. 1) QFLAG = .TRUE.
      IF (ENERGY .EQ. 0) GO TO 135
      UFLAG = .TRUE.
      IF (RFNO.GE.3 .AND. RFNO.LE.8) PFLAG = .TRUE.
      IF (RFNO.GE.3 .AND. RFNO.LE.8) QFLAG = .TRUE.
  135 CONTINUE
C
      IF (.NOT.(UFLAG .OR. PFLAG .OR. QFLAG)) GO TO 900
C
C     COMPUTE THE APPLIED STATIC LOADS FOR THE REQUESTED SUBSTRUCTURE
C     IF WE ARE PRINTING THE SOLUTION SUBSTRUCTURE CHECK IF THE LOADS
C     ARE ON A GINO FILE.
C
      IF (RFNO .EQ. 3) PFLAG = .FALSE.
      IF (.NOT.PFLAG .AND. (.NOT.QFLAG .OR. RFNO .EQ. 3)) GO TO 150
      IF (RSS(1).NE.FSS(1) .OR. RSS(2).NE.FSS(2)) GO TO 140
      PA = PG
      MCBA(1) = PG
      CALL RDTRL (MCBA)
      IF (MCBA(1) .GT. 0) GO TO 150
  140 PA = SCR3
      CALL RCOVSL (RSS,PVEC,0,SCR6,SCR7,SCR8,PA,Z(1),Z(1),SOF3-1,
     1            .FALSE.,RFNO)
      IF (PA .LE. 0) PFLAG = .FALSE.
C
C     GET THE DISPLACEMENT VECTOR AND IF RIGID FORMAT 8 THEN
C     CALCULATE THE VELOCITIES AND ACCELERATIONS.
C
  150 IF (.NOT.UFLAG .AND. .NOT.QFLAG) GO TO 170
      MCBA(1) = UA
      CALL RDTRL (MCBA)
      IF (MCBA(1) .GT. 0) GO TO 160
      UA = SCR2
      CALL MTRXI (UA,RSS,UVEC,0,RC)
      IF (RC .EQ. 1) GO TO 160
  155 UA = 0
      WRITE (NOUT,63190) SWM,RSS
      UFLAG = .FALSE.
      QFLAG = .FALSE.
      ENERGY = 0
C
  160 IF (RFNO.NE.8 .OR. .NOT.(UFLAG.OR.QFLAG)) GO TO 170
      CALL RCOVVA (UA,0,SCR1,0,0,0,RSS,Z(1),Z(1),Z(1))
      IF (UA .LE. 0) GO TO 155
      UA = SCR1
C
C     COMPUTE THE SPCF REACTIONS IF OUTPUT REQUESTS WERE SPECIFIED
C
  170 IF (QFLAG) CALL RCOVQV
      IF (QA .LE. 0) QFLAG = .FALSE.
C
C     OUTPUT PROCESSING
C
C
C     IF IOPT IS EQUAL TO ONE THEN THE OUTPUT WILL BE SORTED BY SUBCASE
C     IF EQUAL TO TWO IT WILL BE SORTED BY SUBSTRUCTURE
C
      NP = BUF(IREQ+3)
      NS = BUF(IREQ+4)
C
C     FIND THE LENGTH AND TYPE OF THE VECTORS TO BE OUTPUT
C
      CALL SOFTRL (RSS,UVEC,MCBA)
      NMODES = MCBA(2)
      NSTEPS = MCBA(2)
      IF (RFNO .EQ. 9) NSTEPS = NSTEPS/3
      COMPLX = .FALSE.
      IF (MCBA(5) .GE. 3) COMPLX = .TRUE.
      NWORD = 1
      IF (COMPLX) NWORD = 2
C
C     PERFORM GENERAL INITIALIZATION OF OFP ID RECORD
C
      IDBUF( 3) = 0
      IDBUF( 6) = 0
      IDBUF( 7) = 0
      IDBUF( 8) = 0
      IDBUF(10) = 8
      IF (COMPLX) IDBUF(10) = 14
      DO 370 I = 11,50
  370 IDBUF(I) = 0
C
C     INITALIZE THE UNPACK COMMON BLOCK
C
      UTYPO = 1
      IF (COMPLX) UTYPO = 3
      IRU   = 1
      NRU   = MCBA(3)
      INCU  = 1
C
C     ALLOCATE OPEN CORE
C
      ISETS = 1
      LSETS = 100
      IVECT = ISETS + LSETS
      ISIL  = IVECT + (NRU*NWORD)
      IEQSS = ISIL  + NP
      IF (IEQSS+2 .GT. LCORE) GO TO 6313
C
C
C                          OPEN CORE DIAGRAM FOR /RCOVCX/
C
C                       +----------------------------------+
C          Z(ISETS)     I                                  I
C                       I     CASECC SET INFORMATION       I
C                       I                                  I
C                       +----------------------------------+
C          Z(IVECT)     I                                  I
C                       I     VECTOR TO BE PRINTED         I
C                       I                                  I
C                       +----------------------------------+
C          Z(ISIL )     I                                  I
C                       I     SCALAR INDEX LIST FROM EQSS  I
C                       I                                  I
C                       +----------------------------------+
C          Z(IEQSS)     I                                  I
C                       I     EQSS DATA IN TRIPLES OF      I
C                       I        (1) EXTERNAL GRID ID      I
C                       I        (2) INTERNAL POINT INDEX  I
C                       I        (3) COMPONENT CODE        I
C                       I     DATA FOR EACH BASIC SUB-     I
C                       I     STRUCTURE TERMINATED BY      I
C                       I     THREE (-1)S                  I
C                       I                                  I
C                       I     NOTE  EQSS DATA MAY NOT BE   I
C                       I     IN CORE IF SPILL LOGIC       I
C                       I     INVOKED.                     I
C                       I                                  I
C                       +----------------------------------+
C          Z(ISEQ)      I                                  I
C                       I     SYMMETRY SEQUENCE            I
C                       I                                  I
C                       +----------------------------------+
C          Z(ICOMB)     I                                  I
C                       I     VECTOR CONTRIBUTING TO THE   I
C                       I     LINEAR COMBINATION FOR THE   I
C                       I     SYMMETRY SEQUENCE            I
C                       I                                  I
C                       +----------------------------------+
C
C     READ SIL FROM EQSS INTO OPEN CORE AT ISIL
C
      CALL SFETCH (RSS,EQSS,SRD,RC)
      N = NS + 1
      CALL SJUMP (N)
      DO 470 I = 1,NP
      CALL SUREAD (Z(ISIL+I-1),1,NWDS,RC)
  470 CALL SUREAD (J,1,NWDS,RC)
C
C     READ EQSS DATA INTO OPEN CORE AT IEQSS IF IT WILL FIT.  IF IOPT
C     EQUALS 2, READ ONLY ONE GROUP AND PRCESS ONE BASIC SUBSTRUCTURE
C     A TIME.
C
      INCORE = .FALSE.
      NEQSS  = IEQSS + 2
      CALL SFETCH (RSS,EQSS,SRD,RC)
      N = 1
      CALL SJUMP (N)
      NSS = NS
      IF (IOPT .EQ. 2) NSS = 1
      ISS = 0
C
C     TOP OF LOOP OVER BASIC SUBSTRUCTURES WHEN PROCESSING ONE AT A TIME
C
  475 ISS = ISS + 1
      K   = LCORE - IEQSS + 1
      J   = IEQSS
      ITEM= EQSS
      DO 480 I = 1,NSS
      CALL SUREAD (Z(J),K,NWDS,RC)
      IF (RC .EQ. 3) GO TO 6107
      IF (RC .NE. 2) GO TO 490
      J = J + NWDS
      IF (J+3 .GT. LCORE) GO TO 490
      IZ(J  ) = -1
      IZ(J+1) = -1
      IZ(J+2) = -1
      J       = J + 3
      NEQSS   = J - 1
      K       = K - NWDS - 3
      IF (K .LE. 0) GO TO 490
  480 CONTINUE
      INCORE  = .TRUE.
      GO TO 491
C
C     EQSS WILL NOT FIT IN CORE
C
  490 NEQSS = IEQSS + 2
  491 ISEQ  = NEQSS + 1
C
C     WRITE HEADER RECORDS ON OUTPUT DATA BLOCKS AND POSITION BOTH
C     INPUT AND OUTPUT DATA BLOCKS AFTER THE HEADER RECORD
C
      DO 497 I = 1,3
      GO TO (495,492,493), I
C
C     CHECK DISPLACEMENT VECTOR
C
  495 IF (.NOT.UFLAG) GO TO 497
      IN   = UA
      IOUT = OUGV1
      GO TO 494
C
C     CHECK LOAD VECTOR
C
  492 IF (.NOT.PFLAG) GO TO 497
      IN   = PA
      IOUT = OPG1
      GO TO 494
C
C     CHECK READTIONS VECTOR
C
  493 IF (.NOT.QFLAG) GO TO 497
      IN   = QA
      IOUT = OQG1
C
C     POSITION FILES
C
  494 CALL GOPEN (IN,Z(BUF1),RDREW)
      CALL CLOSE (IN,NOREW)
      IF (ISS .GT. 1) GO TO 497
      CALL OPEN (*496,IOUT,Z(BUF2),WRTREW)
      CALL FNAME (IOUT,NAMEF)
      CALL WRITE (IOUT,NAMEF,2,1)
      CALL CLOSE (IOUT,NOREW)
      GO TO 497
C
C     OUTPUT FILE PURGED - TURN OFF REQUEST FLAG
C
  496 WRITE (NOUT,63140) SWM,IOUT
      IF (IOUT .EQ. OUGV1) UFLAG = .FALSE.
      IF (IOUT .EQ. OPG1 ) PFLAG = .FALSE.
      IF (IOUT .EQ. OQG1 ) QFLAG = .FALSE.
  497 CONTINUE
C
C     SETUP FOR LOOP OVER SUBCASES
C
      ISC = 0
      DO 510 I = 1,3
  510 NFWD(I) = 0
C
C     POSITION CASESS TO FIRST CASECC SUBCASE
C
      FILE = CASESS
      CALL OPEN (*9001,CASESS,Z(BUF3),RDREW)
      DO 530 I = 1,NCCREC
  530 CALL FWDREC (*9002,CASESS)
      END = .FALSE.
C
C     TOP OF LOOP OVER SUBCASES
C
  540 ISC   = ISC + 1
      ITYPE = 1
      IF (END) GO TO 596
C
C     READ OUTPUT REQUESTS FROM CASECC RECORD
C
      CALL READ  (*545,*9003,CASESS,0,-3,0,NWDS)
      CALL FREAD (CASESS,LID  ,1  ,0)
      CALL FREAD (CASESS,0    ,-12,0)
      CALL FREAD (CASESS,OLOAD,3  ,0)
      CALL FREAD (CASESS,DISP ,3  ,0)
      CALL FREAD (CASESS,0    ,-6 ,0)
      CALL FREAD (CASESS,ACCE ,3  ,0)
      CALL FREAD (CASESS,VELO ,3  ,0)
      CALL FREAD (CASESS,SPCF ,3  ,0)
      CALL FREAD (CASESS,0    ,-1 ,0)
C
C     SET OUTPUT TYPE AND MEDIA - IF NO REQUEST IN CASE CONTROL
C     THE DEFAULT VALUES ARE REAL AND PRINTER
C
      IFORM = 1
      IF (COMPLX)  IFORM = 2
      IF (DISP(2)  .EQ. 0) DISP(2) = 1
      IF (DISP(3)  .EQ. 0) DISP(3) = IFORM
      IF (DISP(3)  .LT. 0) NOSORT  = 1
      IF (OLOAD(2) .EQ. 0) OLOAD(2)= 1
      IF (OLOAD(3) .EQ. 0) OLOAD(3)= IFORM
      IF (OLOAD(3) .LT. 0) NOSORT  = 1
      IF (SPCF(2)  .EQ. 0) SPCF(2) = 1
      IF (SPCF(3)  .EQ. 0) SPCF(3) = IFORM
      IF (SPCF(3)  .LT. 0) NOSORT  = 1
      IF (VELO(2)  .EQ. 0) VELO(2) = 1
      IF (VELO(3)  .EQ. 0) VELO(3) = IFORM
      IF (VELO(3)  .LT. 0) NOSORT  = 1
      IF (ACCE(2)  .EQ. 0) ACCE(2) = 1
      IF (ACCE(3)  .EQ. 0) ACCE(3) = IFORM
      IF (ACCE(3)  .LT. 0) NOSORT  = 1
      GO TO 548
C
C     END OF CASE CONTROL RECORDS - CHECK IF THIS IS REALLY THE END
C
  545 END = .TRUE.
      IF (RFNO .LE. 2) GO TO 860
      IF (RFNO.EQ.3 .AND. ISC.GT.NMODES) GO TO 860
      IF (RFNO.GE.8 .AND. ISC.GT.NSTEPS) GO TO 860
      GO TO 596
C
C     READ TITLE, SUBTITLE, AND LABEL.  WILL REPLACE RIGHTMOST WORDS OF
C     SUBTITLE WITH BASIC SUBSTRUCTURE NAME
C
  548 CALL FREAD (CASESS,IDBUF(51),96,0)
      DO 550 I = 1,3
      IDBUF(I+101) = SUBSTR(I)
  550 IDBUF(I+133) = COMPS(I)
      IDBUF(  105) = SUBSTR(4)
      IDBUF(  106) = RSS(1)
      IDBUF(  107) = RSS(2)
C
C     READ SYMMETRY SEQUENCE AND SET INFORMATION
C
      NWDS =-1
      IZ(ISETS  ) = 0
      IZ(ISETS+1) = 0
      CALL FREAD (CASESS,0,-31,0)
      CALL FREAD (CASESS,LCC,1,0)
      LSKIP = 167 - LCC
      CALL FREAD (CASESS,0,LSKIP,0)
      CALL READ (*9002,*590,CASESS,LSEQ,1,0,N)
      IF (NEQSS+LSEQ .GT. LCORE) GO TO 6313
      IF (LSEQ .GT. 0)CALL READ (*9002,*590,CASESS,Z(ISEQ),LSEQ,0,N)
      ICOMB = ISEQ + LSEQ
      IF (ICOMB+NRU .GT. LCORE) GO TO 6313
      CALL READ (*9002,*590,CASESS,Z(ISETS),LSETS,0,NWDS)
      K = LSETS
C
C     MUST EXPAND SETS PORTION OF OPEN CORE
C
  560 N = LCORE - NEQSS
      IF (N .GT. 0) GO TO 570
      IF (.NOT. INCORE) GO TO 6313
      INCORE = .FALSE.
      NEQSS  = IEQSS + 2
      GO TO 560
  570 DO 575 I = ISIL,NEQSS
  575 IZ(LCORE-I+1) = IZ(NEQSS-I+1)
      IVECT = IVECT + N
      ISIL  = ISIL  + N
      IEQSS = IEQSS + N
      NEQSS = NEQSS + N
      CALL READ (*9002,*580,CASESS,Z(ISETS+LSETS),N,0,NWDS)
      K     = K + N
      GO TO 560
  580 NWDS  = K + NWDS
  590 NSETS = ISETS + NWDS
C
C     PROCESS OUTPUT ITYPE
C
  596 ONCE  = .FALSE.
      JEQSS = IEQSS - 3
      ISKIP = 0
      IF (ITYPE.EQ.1 .AND. .NOT.UFLAG) GO TO 855
      IF (ITYPE.EQ.2 .AND. .NOT.PFLAG) GO TO 855
      IF (ITYPE.EQ.3 .AND. .NOT.QFLAG) GO TO 855
      IF (ITYPE.EQ.4 .AND. .NOT.UFLAG) GO TO 855
      IF (ITYPE.EQ.5 .AND. .NOT.UFLAG) GO TO 855
C
C     FOR EACH BASIC SUBSTRUCTURE CURRENTLY BEING PROCESSED, CONSTRUCT
C     ONE OFP ID AND DATA RECORD PAIR.  THE BASIC LOOP IS ABOVE THE
C     VECTOR PROCESSING BECAUSE OUTPUT REQUESTS CAN CHANGE FOR EACH
C     BASIC
C
      DO 840 JS = 1,NSS
      JSS  = ISS  + JS - 1
      NREQ = IREQ + (JSS-1)*LBASIC + 5
      KPOINT = BUF(NREQ+12)
C
C     STATICS
C
      IF (RFNO .GT. 2) GO TO 603
      IF (JS   .GT. 1) GO TO 598
      IAPPRO   = 1
      IDBUF(4) = ISC
      IDBUF(5) = LID
      GO TO 598
C
C     FOR NORMAL MODES GET MODE NUMBER, EIGENVALUE AND FREQUENCY
C
  603 IF (RFNO .NE. 3) GO TO 612
      IF (JS   .GT. 1) GO TO 598
      CALL SFETCH (FSS,SOLN,SRD,RC)
      N = 1
      CALL SJUMP (N)
      J = ISC - 1
      IF (J .EQ. 0) GO TO 611
      DO 597 I = 1,J
  597 CALL SUREAD (MCBA(1),7,NWDS,RC)
  611 CONTINUE
      CALL SUREAD (MODE,1,NWDS,RC)
      CALL SUREAD (I,1,NWDS,RC)
      CALL SUREAD (EIGEN ,1,NWDS,RC)
      CALL SUREAD (EIGENI,1,NWDS,RC)
      CALL SUREAD (VALUE,1,NWDS,RC)
C
      IAPPRO = 2
      IF (COMPLX) IAPPRO = 9
      IDBUF(4) = ISC
      IDBUF(5) = MODE
      RDBUF(6) = EIGEN
      RDBUF(7) = 0.0
      IF (COMPLX) RDBUF(7) = EIGENI
      GO TO 598
C
C     FOR DYNAMICS GET THE TIME OR FREQUENCY
C
  612 IF (RFNO.NE.8 .AND. RFNO.NE.9) GO TO 598
      IF (JS .GT. 1) GO TO 598
      CALL SFETCH (FSS,SOLN,SRD,RC)
      N = 1
      CALL SJUMP (N)
      J = ISC - 1
      IF (J .EQ. 0) GO TO 614
      DO 613 I = 1,J
  613 CALL SUREAD (MCBA(1),1,NWDS,RC)
  614 CONTINUE
      CALL SUREAD (VALUE,1,NWDS,RC)
C
      IAPPRO = 5
      IF (RFNO .EQ. 9) IAPPRO = 6
      IDBUF(4) = ISC
      RDBUF(5) = VALUE
      IDBUF(8) = LID
C
C     GET SUBCASE OR MODE REQUEST
C
  598 IF (RFNO .GT. 2) GO TO 599
      ISUB = ISC
      ILOC = 5
      GO TO 600
  599 IF (RFNO .NE. 3) GO TO 607
      ISUB = MODE
      ILOC = 6
      GO TO 600
  607 ISUB = ISC
      ILOC = 11
  600 ISET = BUF(NREQ+ILOC)
      IF (ISET .LT. 0) GO TO 608
      IF (ISET .EQ. 0) GO TO 835
C
C     FIND THE REQUESTED SET
C
      JSET = ISETS
  601 CONTINUE
      IF (ISET .EQ. IZ(JSET)) GO TO 602
      JSET = JSET + IZ(JSET+1) + 2
      IF (JSET .LT. NSETS) GO TO 601
C
C     SET NOT FOUND, ISSUE WARNING AND PRINT ALL INSTEAD.
C
      WRITE (NOUT,63650) UWM,ISET
      BUF(NREQ+ILOC) = -1
      GO TO 608
C
C     FIND IF CURRENT SUBCASE OR MODE IS IN REQUESTED SET
C
  602 NEXT = 1
      KSET = IZ(JSET+1)
      CALL SETFND (*835,IZ(JSET+2),KSET,ISUB,NEXT)
C
C     SO FAR SO GOOD - IF NORMAL MODES OR DYNAMICS PROBLEM CHECK IF
C     EIGEN VALUE, TIME OR FREQUENCY IS IN REQUESTED RANGE
C
  608 CONTINUE
      IF (RFNO .LT. 3) GO TO 609
      IF (VALUE .LT. RBUF(NREQ+7)) GO TO 835
      IF (VALUE .GT. RBUF(NREQ+8)) GO TO 835
C
  609 GO TO (615,640,650,652,654), ITYPE
C
C     PROCESS DISPLACEMENT REQUESTS
C
  615 IOPST = DISP(1)
      IF (BUF(NREQ+2) .GT. -2) IOPST = BUF(NREQ+2)
      IF (IOPST.EQ.0 .AND. LSEQ.EQ.0) GO TO 835
      IF (ONCE) GO TO 705
      ONCE  = .TRUE.
C
      IDC   = DISP(2)
      IFORM = IABS(DISP(3))
      IDBUF(2) = 1
      IF (RFNO .EQ. 3) IDBUF(2) = 7
      THRESH = UTHRES
      SUPRES = .FALSE.
      IN   = UA
      IOUT = OUGV1
      GO TO 660
C
C     PROCESS OLOAD REQUESTS
C
  640 IOPST = OLOAD(1)
      IF (BUF(NREQ+3) .GT. -2) IOPST = BUF(NREQ+3)
      IF (IOPST.EQ.0 .AND. LSEQ.EQ.0) GO TO 835
      IF (ONCE) GO TO 705
      ONCE = .TRUE.
C
      IDC    = OLOAD(2)
      IFORM  = IABS(OLOAD(3))
      THRESH = PTHRES
      SUPRES = .TRUE.
      IDBUF(2) = 2
      IN   = PA
      IOUT = OPG1
      GO TO 660
C
C     PROCESS SPCFORCE (ACTUALLY, ALL REACTIONS) REQUESTS
C
  650 IOPST = SPCF(1)
      IF (BUF(NREQ+4) .GT. -2 ) IOPST = BUF(NREQ+4)
      IF (IOPST.EQ.0 .AND. LSEQ.EQ.0) GO TO 835
      IF (ONCE) GO TO 705
      ONCE = .TRUE.
C
      IDC    = SPCF(2)
      IFORM  = IABS(SPCF(3))
      THRESH = QTHRES
      SUPRES = .TRUE.
      IDBUF(2) = 3
      IN   = QA
      IOUT = OQG1
      GO TO 660
C
C     PROCESS VELOCITY REQUESTS
C
  652 IOPST = VELO(1)
      IF (BUF(NREQ+9) .GT. -2) IOPST = BUF(NREQ+9)
      IF (IOPST.EQ.0 .AND. LSEQ.EQ.0) GO TO 835
      IF (ONCE) GO TO 705
      ONCE = .TRUE.
C
      IDC   = VELO(2)
      IFORM = IABS(VELO(3))
      IDBUF(2) = 10
      THRESH = UTHRES
      SUPRES = .FALSE.
      IN   = UA
      IOUT = OUGV1
      GO TO 660
C
C     PROCESS ACCELERATION REQUESTS
C
  654 IOPST = ACCE(1)
      IF (BUF(NREQ+10) .GT. -2) IOPST = BUF(NREQ+10)
      IF (IOPST.EQ.0 .AND. LSEQ.EQ.0) GO TO 835
      IF (ONCE) GO TO 705
      ONCE = .TRUE.
C
      IDC   = ACCE(2)
      IFORM = IABS(ACCE(3))
      IDBUF(2) = 11
      THRESH = UTHRES
      SUPRES = .FALSE.
      IN   = UA
      IOUT = OUGV1
C
C     OPEN FILES AND UNPACK VECTOR TO BE PRINTED
C
  660 FILE = IN
      CALL GOPEN (IN,Z(BUF1),RD)
      CALL GOPEN (IOUT,Z(BUF2),WRT)
      IT  = ITYPE
      IF (ITYPE .GT. 3) IT = 1
      IF (LSEQ .GT. 0) GO TO 664
      N   = NFWD(IT)
      IF (N .LE. 0) GO TO 663
      DO 662 I = 1,N
  662 CALL FWDREC (*9002,IN)
      NFWD(IT) = 0
  663 CALL UNPACK (*673,IN,Z(IVECT))
      GO TO 675
C
C     FORM LINEAR COMBINATION FOR SYMMETRY SEQUENCE
C
  664 N = NFWD(IT) - LSEQ
      IF (N) 665,669,667
  665 N = -N
      DO 666 I = 1,N
  666 CALL BCKREC(IN)
      GO TO 669
  667 DO 668 I = 1,N
  668 CALL FWDREC (*9002,IN)
  669 DO 670 I = 1,NRU
  670 Z(IVECT+I-1) = 0.0E0
      DO 672 I = 1,LSEQ
      CALL UNPACK (*672,IN,Z(ICOMB))
      DO 671 J = 1,NRU
  671 Z(IVECT+J-1) = Z(IVECT+J-1) + Z(ISEQ+I-1)*Z(ICOMB+J-1)
  672 CONTINUE
      NFWD(IT) = 0
      GO TO 675
  673 N = NRU*NWORD
      DO 674 I = 1,N
  674 Z(IVECT+I-1) = 0.0
C
C     IF EQSS DATA NOT IN CORE, POSITION THE SOF
C
  675 IF (INCORE) GO TO 705
      CALL SFETCH (RSS,EQSS,SRD,RC)
      NSKIP = ISS + ISKIP
      CALL SJUMP (NSKIP)
      JEQSS = IEQSS
C
C     INSERT SUBSTRUCTURE NAME IN IDREC WRITE IT OUT
C
  705 IDBUF(1) = IDC + 10*IAPPRO
      IF (COMPLX .AND. JS.EQ.1) IDBUF(2) = IDBUF(2) + 1000
      IDBUF(  9) = IFORM
      IDBUF(138) = BUF(NREQ)
      IDBUF(139) = BUF(NREQ+1)
      KEEP = .FALSE.
C
C     FIND THE REQUESTED OUTPUT SET
C
      NEXT  = 1
      JSET  = ISETS
      NJSET = JSET + 1
      IF (IOPST .LT. 0) GO TO 730
  710 IF (IOPST .EQ. IZ(JSET)) GO TO 730
      JSET  = JSET + IZ(JSET+1) + 2
      IF (JSET .LT. NSETS) GO TO 710
C
C     SET NOT FOUND. ISSUE A WARNING AND PRINT ALL INSTEAD
C
      WRITE (NOUT,63650) UWM,IOPST
      I = ITYPE + 1
      IF (ITYPE .GT. 3) I = I + 4
      BUF(NREQ+I) = -1
      IOPST = -1
C
C     FOR EACH GRID POINT ID IN EQSS FOR THE CURRENT SUBSTRUCTURE WHICH
C     IS A MEMBER OF THE REQUESTED OUTPUT SET, WRITE A LINE OF OUTPUT
C
  730 IF (INCORE) GO TO 780
      CALL SUREAD (Z(JEQSS),3,NWDS,RC)
      IF (RC .NE. 1) GO TO 830
      GO TO 790
  780 JEQSS = JEQSS + 3
      IF (IZ(JEQSS) .GT. 0) GO TO 790
      GO TO 830
C
  790 IF (IOPST .LT. 0) GO TO 800
      IF (NEXT .GT. IZ(JSET+1)) GO TO 830
      KSET = IZ(JSET+1)
      KID  = IZ(JEQSS )
      CALL SETFND (*730,IZ(JSET+2),KSET,KID,NEXT)
C
C     WRITE A LINE OF OUTPUT
C
  800 ICODE = IZ(JEQSS+2)
      CALL DECODE (ICODE,DOFS(1),N)
      DOFS(N+1) = -1
      JSIL = IZ(JEQSS+1) + ISIL - 1
      K    = 0
      NON0 = .FALSE.
      DO 820 I = 1,6
      IF (DOFS(K+1)+1 .NE. I) GO TO 815
      J = IVECT + (IZ(JSIL)-1)*NWORD + K*NWORD
      K = K + 1
      DATA(I) = Z(J)
      IF (COMPLX) GO TO 805
      IF (SUPRES .AND. DATA(I).EQ.0.0) GO TO 815
      IF (ABS(DATA(I)) .LT. THRESH) GO TO 815
      NON0 = .TRUE.
      GO TO 820
  805 DATA(6+I) = Z(J+1)
      IF (IFORM.NE.3 .OR. DATA(I)+DATA(6+I).EQ.0.0) GO TO 810
      DATA(I)   = SQRT(Z(J)**2 + Z(J+1)**2)
      DATA(6+I) = ATAN2(Z(J+1),Z(J))*RADDEG
      IF (DATA(6+I) .LT. -.000005) DATA(6+I) = DATA(6+I) + 360.0
  810 IF (SUPRES .AND. DATA(I)+DATA(6+I).EQ.0.0) GO TO 815
      IF (ABS(DATA(I)).LT.THRESH .AND. ABS(DATA(6+I)).LT.THRESH)
     1    GO TO 815
      NON0 = .TRUE.
      GO TO 820
  815 DATA(  I) = 0.0
      DATA(6+I) = 0.0
  820 CONTINUE
      IF (.NOT.NON0) GO TO 825
      IF (.NOT. KEEP) CALL WRITE (IOUT,IDBUF,146,1)
      CALL WRITE (IOUT,10*IZ(JEQSS)+IDC,1,0)
      CALL WRITE (IOUT,KPOINT,1,0)
      CALL WRITE (IOUT,DATA,6*NWORD,0)
      KEEP = .TRUE.
  825 CONTINUE
      IF (NEXT.LE.IZ(JSET+1) .OR. IOPST.LT.0) GO TO 730
C
C     IF NO DATA WAS WRITTEN FOR THIS BASIC BACKREC THE OFP FILE
C     OVER THE PREVIOUSLY WRITTEN ID RECORD
C
830   IF (KEEP) CALL WRITE (IOUT,0,0,1)
      IF (IZ(JEQSS).LT.0 .OR. (.NOT.INCORE .AND. RC.NE.1)) GO TO 840
C
C     NO MORE OUTPUT FOR THIS BASIC - SKIP EQSS DATA
C
  835 CONTINUE
      IF (INCORE) GO TO 836
      IF (ONCE  ) GO TO 837
      ISKIP = ISKIP + 1
      GO TO 840
  837 N = 1
      CALL SJUMP (N)
      GO TO 840
  836 JEQSS = JEQSS + 3
      IF (IZ(JEQSS) .GT. 0) GO TO 836
  840 CONTINUE
C
C     GO BACK AND DO ANOTHER OUTPUT TYPE
C
      CALL CLOSE (IN,NOREW)
      CALL CLOSE (IOUT,NOREW)
  855 IF (ONCE) GO TO 856
      IT = ITYPE
      IF (ITYPE .GT. 3) IT = 1
      NFWD(IT) = NFWD(IT) + 1
  856 ITYPE = ITYPE + 1
      IF (ITYPE .LE. 3) GO TO 596
      IF (ITYPE.LE.5 .AND. RFNO.GE.8) GO TO 596
      IF (.NOT.END) GO TO 540
      IF (RFNO.EQ.3 .AND. ISC.LT.NMODES) GO TO 540
      IF (RFNO.GE.8 .AND. ISC.LT.NSTEPS) GO TO 540
C
C     ALL SUBCASES PROCESSED,  IF IOPT EQ 2, GO BACK AND PROCESS
C     NEXT BASIC SUBSTRUCTURE
C
  860 CALL CLOSE (CASESS,REW)
      IF (IOPT.EQ.1 .OR. ISS.EQ.NS) GO TO 870
      CALL SFETCH (RSS,EQSS,SRD,RC)
      N = ISS + 1
      CALL SJUMP (N)
      GO TO 475
C
C     WRITE TRAILERS AND EOF ON OUTPUT DATA BLOCKS
C
  870 DO 880 I = 2,7
  880 MCBA(I) = 1
      IF (.NOT.UFLAG) GO TO 885
      CALL GOPEN (OUGV1,Z(BUF1),WRT)
      CALL CLOSE (OUGV1,REW)
      MCBA(1) = OUGV1
      CALL WRTTRL (MCBA)
  885 IF (.NOT.PFLAG) GO TO 890
      CALL GOPEN (OPG1,Z(BUF1),WRT)
      CALL CLOSE (OPG1,REW)
      MCBA(1) = OPG1
      CALL WRTTRL (MCBA)
  890 IF (.NOT.QFLAG) GO TO 900
      CALL GOPEN (OQG1,Z(BUF1),WRT)
      CALL CLOSE (OQG1,REW)
      MCBA(1) = OQG1
      CALL WRTTRL (MCBA)
C
C     NORMAL MODULE TERMINATION
C
  900 CALL SOFCLS
      RETURN
C
C     ERROR PROCESSING
C
 6107 N = 7
      CALL SMSG (N,ITEM,RSS)
      GO TO 9200
 6313 WRITE (NOUT,63130) SWM,RSS
      GO TO 9200
 9001 N = 1
      GO TO 9100
 9002 N = 2
      GO TO 9100
 9003 N = 3
      GO TO 9100
 9100 CALL MESAGE (N,FILE,NAME)
 9200 CALL SOFCLS
      DO 9201 I = 101,111
 9201 CALL CLOSE (I,REW)
      DO 9202 I = 201,203
 9202 CALL CLOSE (I,REW)
      DO 9203 I = 301,308
 9203 CALL CLOSE(I,REW)
      RETURN
C
C     DIAGNOSTICS FORMAT STATEMENTS
C
63130 FORMAT (A27,' 6313, INSUFFICIENT CORE FOR RCOVR MODULE WHILE ',
     1       'TRYING TO PROCESS', /34X,'PRINTOUT DATA BLOCKS FOR ',
     2       'SUBSTRUCTURE',2A4)
63140 FORMAT (A27,' 6314, OUTPUT REQUEST CANNOT BE HONORED.', /34X,
     1       'RCOVR MODULE OUTPUT DATA BLOCK',I4,' IS PURGED.')
63190 FORMAT (A27,' 6319, DISPLACEMENT MATRIX FOR SUBSTRUCTURE ',2A4,
     1       ' MISSING.' /5X,'DISPLACEMENT OUTPUT REQUESTS CANNOT BE ',
     2       'HONORED.  SPCFORCE OUTPUT REQUESTS CANNOT BE HONORED UN',
     3       'LESS THE', /5X,'REACTIONS HAVE BEEN PREVIOUSLY COMPUTED.')
63650 FORMAT (A25,' 6365, REQUESTED OUTPUT SET ID',I6,' IS NOT DECLARED'
     1,      ' IN CASE CONTROL, ALL OUTPUT WILL BE PRODUCED.')
      END
