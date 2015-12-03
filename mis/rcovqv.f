      SUBROUTINE RCOVQV
C
C     THIS SUBROUTINE CALCULATES THE REACTION FORCES FOR THE REQUESTED
C     SUBSTRUCTURE
C
      LOGICAL         REQF       ,REIGEN
      INTEGER         FSS        ,RFNO       ,UA         ,RSS       ,
     1                RC         ,TFLAG      ,SIGNAB     ,SIGNC     ,
     2                PREC       ,SCRM       ,NAME(2)    ,SCR1      ,
     3                SCR2       ,SCR4       ,SCR5       ,SCR6      ,
     4                SCR7       ,SCR8       ,MGG        ,KGG       ,
     5                BGG        ,BUF1       ,BUF2       ,BUF3      ,
     6                BUF4       ,SOF1       ,SOF2       ,SOF3      ,
     7                BMTX       ,QVEC       ,MMTX       ,KMTX      ,
     8                SYSBUF     ,PA         ,QA
C    9,               UVEC       ,SRD
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
      COMMON /MPYADX/ MCBA(7)    ,MCBB(7)    ,MCBC(7)    ,MCBD(7)    ,
     1                MPYZ       ,TFLAG      ,SIGNAB     ,SIGNC      ,
     2                PREC       ,SCRM
      COMMON /ZZZZZZ/ Z(1)
      DATA    QVEC  , KMTX,   MMTX,   BMTX,   K4MX                   /
     1        4HQVEC, 4HKMTX, 4HMMTX, 4HBMTX, 4HK4MX                 /
C     DATA    UVEC  , SRD   / 4HUVEC, 1                              /
      DATA    KGG   , MGG,    BGG,   K4GG,           NAME            /
     1        103   , 104,    109,    110,         4HRCOV,   4HQV    /
      DATA    SCR1  , SCR2,   SCR4,   SCR5,   SCR6,   SCR7,   SCR8   /
     1        301   , 302,    304,    305,    306,    307,    308    /
C
C
C     CHECK TO SEE IF QVEC HAS ALREADY BEEN CALCULATED
C
      CALL MTRXI (SCR4,RSS,QVEC,0,RC)
      IF (RC .NE. 1) GO TO 10
      QA = SCR4
      RETURN
C
C     INITILIZE FOR QVEC CALCULATIONS
C
   10 PREC  = 0
      TFLAG = 0
      SCRM  = SCR5
      SIGNAB= 1
      MPYZ  = KORSZ(Z(1)) - LREQ
      REQF  = .FALSE.
      IF (FSS(1).EQ.RSS(1) .AND. FSS(2).EQ.RSS(2)) REQF = .TRUE.
      REIGEN = .FALSE.
      MALCOM = 0
      IF (UA .NE. SCR1) GO TO 30
      SCR2 = 301
      SCR1 = 302
C
C     CHECK THE DISPLACEMENT MATRIX
C
   30 MCBB(1) = UA
      CALL RDTRL (MCBB)
      IF (MCBB(1) .LE. 0) GO TO 9200
C
C     BRANCH ON RIGID FORMAT
C
      IF (RFNO .GT. 9) GO TO 9007
      GO TO (100,100,200,9007,9007,9007,9007,400,400), RFNO
C
C     STATIC SOUTION
C
C     Q = KU - P
C
C     SET UP LOAD VECTOR FOR SUBSTRACTION
C
  100 SIGNC = -1
      MCBC(1) = PA
      IF (PA .GT. 0) CALL RDTRL (MCBC)
      GO TO 500
C
C     NORMAL MODES
C
C     CHECK IF THE EIGEN VECTORS ARE COMPLEX
C
  200 IF (MCBB(5) .GE. 3) GO TO 300
C
C     REAL NORMAL MODES
C
C      Q = KU + MA   WHERE A = -(2*PI*FREQ)**2 * U
C
      REIGEN = .TRUE.
C
C     MALCOM TAGG OF MDC, IN MSFC, RECOMMANDED THAT FOR RIGID FORMAT 3
C     THE SPC REACTION FORCE SHOULD NOT CONTAIN THE MASS TERM.  JULY/86
C     I.E.    Q = KU ONLY   (DROP THE MA TERM)
C     THUS,   GO TO 250 THEN TO 500
C
C     MARCH 1989 - MALCOM RECOMMENDATION REMOVED. IT CAUSES IMBALANCED
C     SPC FORCES
C
C     MALCOM = 1
      IF (MALCOM .EQ. 1) GO TO 250
C
C     CALCULATE THE ACCLERATION VECTOR FOR REAL NORMAL MODES
C
      IN = UA
      CALL RCOVVA (IN,0,0,0,0,SCR8,RSS,Z(1),Z(1),Z(1))
      IF (IN .LE. 0) GO TO 9200
C
C
C     INDICATE A POSITIVE SIGN ON THE M * A MULTIPLY
C
  250 SIGNAB  = 1
      MCBC(1) = 0
      IF (MALCOM .EQ. 1) GO TO 500
      GO TO 420
C
C     COMPLEX NORMAL MODES
C
C     Q = KU + BV + MA
C
C     CALCULATE THE COMPLEX VELOCITIES AND ACCLERATION VECTORS FOR
C     THE EIGENVECTORS
C
  300 IN = UA
C
C     SEE MALCOM TAGG RECOMMENDATION, 25 LINES ABOVE
C
      CALL SOFCLS
      IF (MALCOM .EQ. 1) GO TO 445
C
      CALL RCOVVA (IN,0,0,SCR6,SCR7,SCR8,RSS,Z(1),Z(1),Z(1))
      IF (IN .LE. 0) GO TO 9200
C
C     INDICATE ZERO LOAD VECTOR FOR NORMAL MODES
C
      MCBC(1) = 0
      GO TO 420
C
C     DYNAMIC ANALYSIS
C
C     Q = KU + BV + MA - P
C
C
C     SPLIT DISPLACEMENT, VELOCITIES AND ACCELERATIONS ONTO SEPERATE
C     FILES
C
  400 IN = UA
      CALL RCOVVA (IN,1,0,SCR6,SCR7,SCR8,RSS,Z(1),Z(1),Z(1))
      IF (IN .LE. 0) GO TO 9200
C
C     SETUP TO SUBTRACT LOAD VECTOR
C
      SIGNC = -1
      MCBC(1) = PA
      IF (PA .GT. 0) CALL RDTRL (MCBC)
C
C     COMMON PROCESSING FOR DYNAMICS AND NORMAL MODES
C
C
C     MULTIPLY AND ADD    SCR1 = MA - P
C
  420 CONTINUE
      IF (.NOT.REQF) GO TO 430
      MCBA(1) = MGG
      CALL RDTRL (MCBA)
      IF (MCBA(1) .GT. 0) GO TO 440
  430 CALL MTRXI (SCR4,RSS,MMTX,0,RC)
      IF (RC .NE. 1) GO TO 460
      MCBA(1) = SCR4
      CALL RDTRL (MCBA)
  440 MCBB(1) = SCR8
      CALL RDTRL (MCBB)
      CALL MAKMCB (MCBD,SCR1,MCBB(3),MCBB(4),MCBB(5))
      CALL SOFCLS
C
      CALL MPYAD (Z(1),Z(1),Z(1))
C
  445 DO 450 I = 1,7
  450 MCBC(I) = MCBD(I)
      SIGNC = 1
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
C
C     MULTIPLY AND ADD   SCR8 = K4V + MCBC
C
  460 IF (REIGEN .OR. RFNO.EQ.9) GO TO 464
      IF (.NOT.REQF) GO TO 461
      MCBA(1) = K4GG
      CALL RDTRL (MCBA)
      IF (MCBA(1) .GT. 0) GO TO 462
  461 CALL MTRXI (SCR4,RSS,K4MX,0,RC)
      IF (RC .NE. 1) GO TO 464
      MCBA(1) = SCR4
      CALL RDTRL (MCBA)
  462 MCBB(1) = SCR7
      CALL RDTRL (MCBB)
      CALL MAKMCB (MCBD,SCR8,MCBB(3),MCBB(4),MCBB(5))
      SIGNAB = 1
      CALL SOFCLS
C
      CALL MPYAD (Z(1),Z(1),Z(1))
C
      DO 463 I = 1,7
  463 MCBC(I) = MCBD(I)
      SIGNC = 1
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
  464 CONTINUE
      IF (REIGEN) GO TO 500
C
C     MULTIPLY AND ADD   SCR1 = BV + MCBC
C
      IF (.NOT.REQF) GO TO 470
      MCBA(1) = BGG
      CALL RDTRL (MCBA)
      IF (MCBA(1) .GT. 0) GO TO 480
  470 CALL MTRXI (SCR4,RSS,BMTX,0,RC)
      IF (RC .NE. 1) GO TO 500
      MCBA(1) = SCR4
      CALL RDTRL (MCBA)
  480 MCBB(1) = SCR7
      CALL RDTRL (MCBB)
      CALL MAKMCB (MCBD,SCR1,MCBB(3),MCBB(4),MCBB(5))
      SIGNAB = 1
      CALL SOFCLS
C
      CALL MPYAD (Z(1),Z(1),Z(1))
C
      DO 490 I = 1,7
  490 MCBC(I) = MCBD(I)
      SIGNC = 1
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
C
C     COMMON PROCESSING FOR ALL RIGID FORMATS
C
C
C     MULTIPLY AND ADD  Q = KU + MCBC
C
  500 IF (.NOT.REQF) GO TO 520
      MCBA(1) = KGG
      CALL RDTRL (MCBA)
      IF (MCBA(1) .GT. 0) GO TO 540
  520 ITEM = KMTX
      FILE = SCR7
      CALL MTRXI (SCR7,RSS,KMTX,0,RC)
      IF (RC .NE. 1) GO TO 6000
      MCBA(1) = SCR7
      CALL RDTRL (MCBA)
  540 MCBB(1) = SCR6
      IF (REIGEN .OR. RFNO.LE.2) MCBB(1) = UA
      CALL RDTRL (MCBB)
      CALL MAKMCB (MCBD,SCR4,MCBB(3),MCBB(4),MCBB(5))
      SIGNAB = 1
      CALL SOFCLS
C
      CALL MPYAD (Z(1),Z(1),Z(1))
C
      CALL WRTTRL (MCBD)
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
C
C     COPY REACTIONS TO SOF
C
      CALL MTRXO (SCR4,RSS,QVEC,0,RC)
      QA = SCR4
C
      RETURN
C
C     ERRORS
C
 6000 IF (RC .EQ. 6) GO TO 9100
      CALL SMSG (RC-2,ITEM,RSS)
      GO TO 9200
 9007 N = 7
 9100 CALL MESAGE (N,0,NAME)
 9200 QA = 0
      WRITE  (NOUT,6318) SWM
 6318 FORMAT (A27,' 6318, OUTPUT REQUEST FOR REACTIONS FORCES IGNORED.')
      RETURN
C
      END
