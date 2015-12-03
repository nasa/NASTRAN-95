      SUBROUTINE RCOVO
C
C     RCOVO READS THE CASESS RECOVER RECORD AND PROCESSES ANY
C     OUTPUT REQUESTS FOR THE CURRENT SAVE OR PRINT REQUEST
C
C     THE OUTPUT REQUESTS ARE STORED AT THE BOTTOM OF OPEN CORE IN
C     A TABLE WITH THE FOLLOWING FORM
C
C     BUF(IREQ) -  UVEC  -I
C                  PVEC   I- NONZERO IF ANY REQUEST PRESENT
C                  QVEC  -I
C                  NO. OF POINTS
C                  NO. OF BASICS
C                  BASIC NAME(2) -I
C                  DISP SET       I
C                  OLOAD SET      I
C                  SPCF SET       I - REPEATED FOR EACH BASIC
C                  SUBCASES SET   I   SUBSTRUCTURE
C                  MODES SET      I
C                  RANGE(2)       I
C                  VELO SET       I
C                  ACCE SET       I
C                  STEPS SET      I
C                  GRID OR MODAL -I
C
      EXTERNAL        LSHIFT     ,ANDF
      LOGICAL         BASIC      ,MRECVR
      INTEGER         RSS        ,BUF(1)     ,STEP       ,CASESS     ,
     1                EQSS       ,Z          ,BUF1       ,SYSBUF     ,
     2                SOF1       ,SOF2       ,SOF3       ,RECOVR     ,
     3                SAVE       ,PRINT      ,SRD        ,FSS(2)     ,
     4                RD         ,SUBNAM(2)  ,RDREW      ,WRT        ,
     5                WRTREW     ,REW        ,NOREW      ,RC         ,
     6                REC(3)     ,SUBC       ,SUBS       ,MODE       ,
     7                ALL        ,NONE       ,COMDS(13)  ,ENERGY     ,
     8                UIMPRO     ,RFNO       ,TIME       ,FREQ       ,
     9                ANDF       ,MRECOV
      REAL            RBUF(1)    ,RREC(3)
      CHARACTER       UFM*23     ,UWM*25     ,UIM*29     ,SFM*25     ,
     1                SWM*27
      COMMON /XMSSG / UFM        ,UWM        ,UIM        ,SFM        ,
     1                SWM
      COMMON /BLANK / DRY        ,LOOP       ,STEP       ,FSS        ,
     1                RFNO       ,NEIGV      ,LUI        ,UINMS(2,5) ,
     2                NOSORT     ,UTHRES     ,PTHRES     ,QTHRES
      COMMON /RCOVCR/ ICORE      ,LCORE      ,BUF1       ,BUF2       ,
     1                BUF3       ,BUF4       ,SOF1       ,SOF2       ,
     2                SOF3
      COMMON /RCOVCM/ MRECVR     ,UA         ,PA         ,QA         ,
     1                IOPT       ,RSS(2)     ,ENERGY     ,UIMPRO     ,
     2                RANG(2)    ,IREQ       ,LREQ       ,LBASIC
      COMMON /ZZZZZZ/ Z(1)
      COMMON /SYSTEM/ SYSBUF     ,NOUT
      COMMON /NAMES / RD         ,RDREW      ,WRT        ,WRTREW     ,
     1                REW        ,NOREW      ,EOFNRW
      EQUIVALENCE     (BUF(1)    ,Z(1))
      EQUIVALENCE     (BUF(1)    ,RBUF(1))   ,(REC(1)    ,RREC(1))   ,
     1                (ISET      ,RSET)
      DATA   CASESS / 101   /, SUBNAM / 4HRCOV,4HO   /
      DATA   EQSS   / 4HEQSS/
      DATA   RECOVR / 4HRECO/, MRECOV / 4HMREC       /
      DATA   PRINT  / 4HPRIN/, SAVE   / 4HSAVE       /
      DATA   SRD    / 1     /
      DATA   LL     / 2     /
      DATA   NCOMDS / 13    /
      DATA   COMDS  / 4HDISP,4HOLOA,4HSPCF,4HMODE,4HRANG,4HSUBC,4HSORT,
     1                4HBASI,4HVELO,4HACCE,4HENER,4HUIMP,4HSTEP       /
      DATA   SUBC   , SUBS,   MODE,   ALL,    NONE,   TIME,   FREQ    /
     1       4HSUBC , 4HSUBS, 4HMODE, 4HALL , 4HNONE, 4HTIME, 4HFREQ  /
C
C     SET UP BUFFERS
C
      SOF1  = 1
      SOF2  = SOF1 + SYSBUF
      SOF3  = SOF2 + SYSBUF + 1
      BUF1  = SOF3 + SYSBUF
      ICORE = BUF1 + SYSBUF
      LCORE = KORSZ(Z(1)) - ICORE + 1
      IF (LCORE .LE. 0) GO TO 9008
C
C     FIND RECOVER RECORD IN CASESS
C
      CALL GOPEN (CASESS,Z(BUF1),RDREW)
      IF (STEP .EQ. 1) GO TO 20
      DO 10 I = 2,STEP
   10 CALL FWDREC (*9002,CASESS)
   20 CALL FREAD (CASESS,REC,2,0)
      IF (REC(1).NE.RECOVR .AND. REC(1).NE.MRECOV) GO TO 6305
      MRECVR = .FALSE.
      IF (REC(1) .EQ. MRECOV) MRECVR = .TRUE.
C
C     GET PRINT OR SAVE OPTION FOR THIS PASS
C
      I = 0
   30 CALL READ (*9002,*600,CASESS,REC,3,0,NWDS)
      IF (REC(1).NE.PRINT .AND. REC(1).NE.SAVE) GO TO 30
      IF (LOOP .EQ. I) GO TO 40
      I = I + 1
      GO TO 30
C
C     GET NAME OF SUBSTRUCTURE TO BE OPERATED ON
C
   40 RSS(1) = REC(2)
      RSS(2) = REC(3)
      LOOP   = LOOP + 1
      IF (REC(1) .EQ. SAVE) GO TO 700
      IOPT   = 1
C
C     OPEN SOF AND FETCH EQSS FOR SUBSTRUCTURE TO BE PRINTED
C
C
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
      CALL SFETCH (RSS,EQSS,SRD,RC)
      GO TO (60,50,50,6306,50), RC
C
C     FETCH ON EQSS WAS UNSUCCESSFUL
C
   50 IF (RC .EQ. 2) RC = 3
      CALL SMSG (RC-2,EQSS,RSS)
      GO TO 800
C
C     READ GROUP 0 OF EQSS INTO CORE
C
   60 CALL SUREAD (Z(ICORE),LCORE,NWDS,RC)
      GO TO (9008,65,62,800), RC
   62 CALL SMSG (7,EQSS,RSS)
      GO TO 800
C
C     DETERMINE SIZE OF OUTPUT REQUEST BLOCK AND ALLOCATE SPACE
C     AT BOTTOM OF OPEN CORE
C
   65 NBS  = Z(ICORE+2)
      NP   = Z(ICORE+3)
      LBASIC = 13
      LREQ = 5 + LBASIC*NBS + 2
      IF (LREQ .GT. LCORE-NWDS) GO TO 9008
      NREQ = KORSZ (BUF(1))
      IREQ = NREQ - LREQ + 1
      DO 70 I = IREQ,NREQ
   70 BUF(I) = 0
C
C     MOVE NAMES OF BASICS INTO OUTPUT AREA
C
      BUF(IREQ+3) = NP
      BUF(IREQ+4) = NBS
      DO 80 I = 1,NBS
      I1 = IREQ  + (I-1)*LBASIC + 5
      I2 = ICORE + (I-1)*2+4
      BUF(I1  ) = Z(I2  )
   80 BUF(I1+1) = Z(I2+1)
C
C     INSERT DEFAULTS INTO OUTPUT BLOCK
C
C     MODES    = ALL
C     SUBCASES = ALL
C     RANGE    = -1.0E+35,1.0E+35
C     STEPS    = ALL
C
      ENERGY  = 0
      UIMPRO  = 0
      RANG(1) = -1.0E+35
      RANG(2) =  1.0E+35
      BUF(IREQ  ) = -2
      BUF(IREQ+1) = -2
      BUF(IREQ+2) = -2
      DO 85 I = 1,NBS
      I1 = IREQ + (I-1)*LBASIC + 5
      BUF(I1+ 2) = -2
      BUF(I1+ 3) = -2
      BUF(I1+ 4) = -2
      BUF(I1+ 5) = -1
      BUF(I1+ 6) = -1
      RBUF(I1+7) = -1.0E+35
      RBUF(I1+8) =  1.0E+35
      BUF(I1+ 9) = -2
      BUF(I1+10) = -2
   85 BUF(I1+11) = -1
C
C     READ NEXT COMMAND AND PROCESS OUTPUT REQUEST
C
      NSS1   = 1
      NSS2   = NBS
      IRANGE = 0
      BASIC  = .FALSE.
   90 CALL READ (*9002,*500,CASESS,REC,3,0,NWDS)
      IF (REC(1).EQ.PRINT .OR. REC(1).EQ.SAVE) GO TO 510
      DO 100 I = 1,NCOMDS
      IF (REC(1) .EQ. COMDS(I)) GO TO 110
  100 CONTINUE
      GO TO 90
  110 CONTINUE
      GO TO (120,130,140,150,160,165,170,190,230,240,250,260,270), I
C
C     DISP REQUEST
C
  120 IF (REC(2) .NE. NONE) BUF(IREQ) = 1
      ILOC = 2
      GO TO 400
C
C     OLOAD REQUEST
C
  130 IF (REC(2) .NE. NONE) BUF(IREQ+1) = 1
      IF (REC(2).EQ.NONE .AND. .NOT.BASIC) BUF(IREQ+1) = 0
      ILOC = 3
      GO TO 400
C
C     SPCF REQUEST
C
  140 IF (REC(2) .NE. NONE) BUF(IREQ+2) = 1
      IF (REC(2).EQ.NONE .AND. .NOT.BASIC) BUF(IREQ+2) = 0
      ILOC = 4
      GO TO 400
C
C     MODES REQUEST
C
  150 ILOC = 6
      GO TO 400
C
C     RANGE REQUEST (IF BEFORE A BASIC COMMAND SAVE IT FOR ENERGY
C                    PROCESSING ALSO)
C
  160 ILOC = 7
      IF (MOD(IRANGE,2) .EQ. 1) ILOC = 8
      IRANGE = IRANGE + 1
      IF (REC(2).NE.-2 .AND. REC(3).NE.0) GO TO 450
      IF (BASIC) GO TO 410
      RANG(ILOC-6) = RREC(3)
      GO TO 410
C
C     SUBCASES REQUEST
C
  165 ILOC = 5
      GO TO 400
C
C     SORT COMMAND - IGNORE COMMAND IF AFTER A BASIC DESIGNATOR
C
  170 IF (BASIC) GO TO 180
      I = 0
      IF (REC(2) .EQ. SUBC) I = 1
      IF (REC(2) .EQ. SUBS) I = 2
      IF (REC(2) .EQ. MODE) I = 1
      IF (REC(2) .EQ. TIME) I = 1
      IF (REC(2) .EQ. FREQ) I = 1
      IF (I .EQ. 0) GO TO 450
      IOPT = I
      GO TO 90
  180 WRITE (NOUT,63660) UWM
      GO TO 90
C
C     BASIC COMMAND - VERIFY SUBSTRUCTURE NAME
C
  190 DO 200 I = 1,NBS
      I1 = IREQ + (I-1)*LBASIC + 5
      IF (BUF(I1).EQ.REC(2) .AND. BUF(I1+1).EQ.REC(3)) GO TO 210
  200 CONTINUE
      GO TO 220
  210 NSS1  = I
      NSS2  = I
      BASIC = .TRUE.
      GO TO 90
C
C     NAME NOT A BASIC - SKIP TO NEXT BASIC, PRINT OR SAVE COMMAND
C
  220 WRITE (NOUT,63680) UWM,REC(2),REC(3),RSS
  225 CALL READ (*9002,*500,CASESS,REC,3,0,NWDS)
      IF (REC(1).EQ.PRINT .OR. REC(1).EQ.SAVE) GO TO 510
      IF (REC(1) .EQ. COMDS(8)) GO TO 190
      GO TO 225
C
C     VELOCITY REQUEST
C
  230 IF (RFNO.NE.8 .AND. RFNO.NE.9) GO TO 90
      IF (REC(2) .NE. NONE) BUF(IREQ) = 1
      ILOC = 9
      GO TO 400
C
C     ACCELERATION REQUEST
C
  240 IF (RFNO.NE.8 .AND. RFNO.NE.9) GO TO 90
      IF (REC(2) .NE. NONE) BUF(IREQ) = 1
      ILOC = 10
      GO TO 400
C
C     ENERGY REQUEST
C
  250 ILOC = -1
      GO TO 400
C
C     UIMPROVED REQUEST
C
  260 UIMPRO = 1
      GO TO 90
C
C     STEPS REQUEST
C
  270 ILOC = 11
      GO TO 400
C
C     CHECK VALIDITY OF SET REQUEST
C
  400 IF (REC(2) .EQ. -2) GO TO 450
  410 ISET = 1
      IF (REC(2) .EQ.  ALL) ISET = -1
      IF (REC(2) .EQ. NONE) ISET = 0
      IF (ISET   .LE.  0) GO TO 430
      IF (REC(2) .EQ. -2) GO TO 420
      IF (REC(2) .NE. -1) GO TO 450
C
C     INTEGER VALUE
C
      ISET = REC(3)
      GO TO 430
C
C     REAL VALUE
C
  420 RSET = RREC(3)
C
C     LOOP OVER APPROPRIATE BASIC AREA AND INSERT REQUEST
C
  430 IF (ILOC .LT. 0) GO TO 445
      DO 440 I = NSS1,NSS2
      I1 = IREQ + (I-1)*LBASIC + 5 + ILOC
      BUF(I1) = ISET
  440 CONTINUE
      GO TO 90
C
  445 ENERGY = ISET
      GO TO 90
C
C     ILLEGAL COMMAND FORMAT
C
  450 WRITE (NOUT,63670) UWM,REC(1)
      GO TO 90
C
C
C     END OF RECORD READING CASESS - THIS IS THEREFORE THE LAST
C     SAVE OR PRINT COMMAND
C
  500 LOOP = -1
C
C     END OF PROCESSING FO THIS PRINT COMMAND
C
  510 CALL CLOSE (CASESS,REW)
C
C     DETERMINE IF EACH BASIC IS REALLY A BASIC.  IF NOT THEN THESE
C     WILL BE MODAL POINTS
C
C     BASIC   POINT TYPE = 1
C     MODAL   POINT TYPE = 4
C
      MASKLL = LSHIFT(1023,20)
      DO 550 I = 1,NBS
      I1 = IREQ + (I-1)*LBASIC + 5
      BUF(I1+12) = 1
      CALL FDSUB (BUF(I1),IDIT)
      IF (IDIT .LT. 0) GO TO 550
      CALL FMDI (IDIT,IMDI)
      IF (ANDF(BUF(IMDI+LL),MASKLL) .NE. 0) BUF(I1+12) = 4
  550 CONTINUE
      CALL SOFCLS
      RETURN
C
C
C     NO PRINT OR SAVE COMMAND SPECIFIED - GENERATE A SAVE ON
C     THE SOLUTION SUBSTRUCTURE
C
  600 RSS(1) = FSS(1)
      RSS(2) = FSS(2)
      LOOP   = -1
      GO TO 720
C
C     THIS LOOP IS A SAVE COMMAND - SEE IF ANY OTHER COMMANDS FOLLOW
C
  700 CALL READ (*9002,*710,CASESS,REC,3,0,NWDS)
      IF (REC(1).EQ.PRINT .OR. REC(1).EQ.SAVE) GO TO 720
      GO TO 700
  710 LOOP = -1
C
C     NO OUTPUT BLOCK IS REQUIRED FOR A SAVE COMMAND
C
  720 CALL CLOSE (CASESS,REW)
      IREQ   = 0
      LREQ   = 0
      IOPT   = 0
      ENERGY = 0
      UIMPRO = 0
      RETURN
C
C     ERROR RETURNS
C
  800 CALL SOFCLS
      IOPT = -1
      LOOP = -1
      CALL CLOSE (CASESS,REW)
      RETURN
C
 6305 WRITE (NOUT,63050) SWM,STEP,REC(1)
      GO TO 800
 6306 WRITE (NOUT,63060) UWM,RSS
      GO TO 800
 9002 N = -2
      GO TO 9100
 9008 N = -8
      GO TO 9100
 9100 CALL SOFCLS
      CALL MESAGE (N,CASESS,SUBNAM)
      RETURN
C
C     FORMATS
C
63050 FORMAT (A27,' 6305, RECORD NUMBER',I5,' IS NOT A RECOVER RECORD.',
     1       '  IT IS A ', A4,' RECORD.')
63060 FORMAT (A25,' 6306, ATTEMPT TO RECOVER DISPLACEMENTS FOR NON-',
     1       'EXISTANT SUBSTRUCTURE ',2A4)
63660 FORMAT (A25,' 6366, THE RECOVER OUTPUT COMMAND SORT MUST APPEAR ',
     1       'BEFORE THE FIRST BASIC SUBCOMMAND.', /32X,
     *       'ANY OTHER SORT COMMANDS ARE IGNORED.')
63670 FORMAT (A25,' 6367, ILLEGAL FORMAT ON THE RECOVER OUTPUT COMMAND',
     1       1X,A4,', COMMAND IGNORED.')
63680 FORMAT (A25,' 6368, THE SUBSTRUCTURE ',2A4,' APPEARING ON A ',
     1       'BASIC COMMAND IS NOT A COMPONENT OF ',2A4, /32X,
     2       'ALL OUTPUT REQUESTS UNTIL THE NEXT BASIC, PRINT OR SAVE ',
     3       'COMMAND ARE IGNORED.')
      END
