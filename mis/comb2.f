      SUBROUTINE COMB2
C
C     COMB2 PERFORMS THE TRANSFORMATION AND ADDITION OF STIFFNESS, MASS,
C     OR LOAD MATRICES FOR THE PHASE 2 SUBSTRUCTURE COMBINE OPERATION
C
C     NOVEMBER 1973
C
C
      LOGICAL           ADDFLG
      INTEGER           TFLAG      ,SIGNAB     ,SIGNC      ,PREC      ,
     1        SCR1     ,SCR2       ,SCR3       ,SCR4       ,SCR5      ,
     2        RULE     ,TYPIN      ,TYPOUT     ,ACOMB      ,AMCB(7,7) ,
     3        HMCB(6,7),SOF1       ,SOF2       ,SOF3       ,DRY       ,
     4        BUF1     ,RDSOF      ,RC         ,USE        ,HORG      ,
     5        PVEC     ,RFILES     ,IZ(1)      ,RECT       ,RSP       ,
     6        NAME(2)  ,RSOFAR     ,KMP(5)     ,TYPE       ,KMPITM(5) ,
     7        BLANK    ,SYSBUF     ,CPV(7)     ,RPV(7)     ,SCR6      ,
     8        XXXX     ,SCR7       ,PORA       ,PAPP
      DOUBLE PRECISION  DBZ(1)
      DIMENSION         MCBTRL(7)
      CHARACTER         UFM*23     ,UWM*25     ,UIM*29     ,SFM*25
      COMMON /XMSSG /   UFM        ,UWM        ,UIM        ,SFM
      COMMON /BLANK /   DRY        ,TYPE(2)    ,PORA(2)    ,NAMESS(2,7),
     1                  ACOMB      ,USE(14)    ,RFILES(3)  ,KK        ,
     2                  KN         ,JN
      COMMON /SYSTEM/   SYSBUF     ,NOUT
      COMMON /NAMES /   RD         ,RDREW      ,WRT        ,WRTREW    ,
     1                  REW        ,NOREW      ,EOFNRW     ,RSP       ,
     2                  RDP        ,CSP        ,CDP        ,SQUARE    ,
     3                  RECT
      COMMON /PACKX /   TYPIN      ,TYPOUT     ,IROW       ,NROW      ,
     1                  INCR
      COMMON /PARMEG/   MCBP(7)    ,MCBP11(7)  ,MCBP21(7)  ,MCBP12(7) ,
     1                  MCBP22(7)  ,MRGZ       ,RULE
      COMMON /MPY3TL/   MCBA2(7)   ,MCBB2(7)   ,MCBC2(7)   ,MCBD2(7)  ,
     1                  SCR5       ,SCR6       ,SCR7       ,LKORE     ,
     2                  ICODE      ,IPREC      ,DUMMY(13)
      COMMON /MPYADX/   MCBA(7)    ,MCBB(7)    ,MCBC(7)    ,MCBD(7)   ,
     1                  LCORE      ,TFLAG      ,SIGNAB     ,SIGNC     ,
     2                  PREC       ,MSCR       ,DUMM
      COMMON /ZZZZZZ/   Z(1)
      EQUIVALENCE      (DBZ(1),Z(1),IZ(1)),(PVEC,KMPITM(3))
      DATA   NAME   /  4HCOMB,4H2   /
      DATA   HORG   /  4HHORG       /
      DATA   BLANK  /  4H           /
      DATA   XXXX   /  4HXXXX       /
      DATA   PAPP   /  4HPAPP       /
      DATA   KMP    /  4HK   , 4HM   , 4HP   , 4HB   , 4HK4   /
      DATA   KMPITM /  4HKMTX, 4HMMTX, 4HPVEC, 4HBMTX, 4HK4MX /
C
C     INITIALIZE
C
      DO 5 I = 1,14
      IF (NAMESS(I,1).EQ.XXXX .OR. NAMESS(I,1).EQ.0) NAMESS(I,1) = BLANK
    5 CONTINUE
      ACOMB = 201
      SCR1  = 301
      SCR2  = 302
      SCR3  = 303
      SCR4  = 304
      SCR5  = 305
      SCR6  = 306
      SCR7  = 307
      SIGNAB= 1
      SIGNC = 1
      PREC  = 0
      MSCR  = SCR5
      ICODE = 0
      RULE  = 0
      RDSOF = 1
      NOGO  = 0
      RFILES(1) = ACOMB
      NSIZE = 0
      MCBP21(1) = 0
      MCBP22(1) = 0
      RSOFAR = 0
      KN = 1
      JN =-1
      DO 10 I = 1,5
      IF (TYPE(1) .EQ. KMP(I)) GO TO 20
   10 CONTINUE
      WRITE (NOUT,6302) SFM,TYPE
      IF (DRY .LT. 0) RETURN
C
      DRY  =-2
      ITEM = 0
      GO TO 30
   20 ITEM = KMPITM(I)
      IF (ITEM .EQ. PVEC) ITEM = PORA(1)
      IF (DRY .LT. 0) RETURN
C
   30 LCORE = KORSZ(Z) - 1
      LKORE = LCORE
      BUF1  = LCORE - SYSBUF + 1
      SOF1  = BUF1  - SYSBUF
      SOF2  = SOF1  - SYSBUF - 1
      SOF3  = SOF2  - SYSBUF
      IF (SOF3 .GT. 0) GO TO 40
      CALL MESAGE (8,0,NAME)
      DRY =-2
      RETURN
C
   40 CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
C
C     GRAB THE MATRIX CONTROL BLOCKS
C
      NMAT = 0
      DO 170 I = 1,7
      IF (NAMESS(1,I) .EQ. BLANK) GO TO 170
      AMCB(1,I) = 100 + I
      CALL RDTRL (AMCB(1,I))
      IF (AMCB(1,I) .GT. 0) GO TO 135
C
C     NO GINO FILE.  CHECK SOF
C
      CALL SOFTRL (NAMESS(1,I),ITEM,AMCB(1,I))
      RC = AMCB(1,I)
      GO TO (130,110,115,120,120), RC
  110 NOGO = 1
      WRITE (NOUT,6301) SFM,NAMESS(1,I),NAMESS(2,I),ITEM
      GO TO 170
  115 IF (TYPE(1) .EQ. KMP(3)) GO TO 170
  120 NOGO = 1
      CALL SMSG (RC-2,ITEM,NAMESS(1,I))
      GO TO 170
C
C     MATRIX FOUND ON SOF
C
  130 CONTINUE
      AMCB(1,I) = 0
C
C     GRAB THE MCB OF THE TRANSFORMATION MATRIX
C
  135 CALL SOFTRL (NAMESS(1,I),HORG,MCBTRL)
      RC = MCBTRL(1)
      GO TO (160,140,150,155,155), RC
  140 NOGO = 1
      CALL SMSG (1,HORG,NAMESS(1,I))
      GO TO 170
  150 NOGO = 1
      WRITE (NOUT,6303) SFM,NAMESS(1,I),NAMESS(2,I)
      GO TO 170
  155 NOGO = 1
      CALL SMSG (RC-2,HORG,NAMESS(1,I))
      GO TO 170
  160 DO 161 IT = 1,6
  161 HMCB(IT,I) = MCBTRL(IT+1)
      NMAT = NMAT + 1
      USE(2*NMAT-1) = I
      DEN = FLOAT(AMCB(7,I))/10000.
      USE(2*NMAT) = AMCB(2,I)*AMCB(3,I)*DEN
C
C     CHECK COMPATIBILITY OF DIMENSIONS
C
      IF (NSIZE .EQ. 0) NSIZE = HMCB(1,I)
      IF (HMCB(1,I).EQ.NSIZE .AND. HMCB(2,I).EQ.AMCB(2,I) .AND.
     1    HMCB(2,I).EQ.AMCB(3,I)) GO TO 170
      IF (ITEM.EQ.PVEC .OR. ITEM.EQ.PAPP .AND. HMCB(1,I).EQ.NSIZE .AND.
     1    HMCB(2,I).EQ.AMCB(3,I)) GO TO 170
      NOGO = 1
      WRITE (NOUT,6304) SFM,I,NAMESS(1,I),NAMESS(2,I)
  170 CONTINUE
      IF (NOGO .EQ. 0) GO TO 175
C
  174 DRY =-2
      WRITE  (NOUT,177) AMCB,HMCB
  177 FORMAT ('0*** COMB2 MATRIX TRAILER DUMP',
     1        //7(4X,7I10/), /7(11X,6I10/))
      GO TO 9999
C
  175 IF (NMAT .EQ. 0) GO TO 9999
C
C     DETERMINE PRECISION FOR FINAL MATRIX
C
      IPRC = 1
      ITYP = 0
      DO 176 I = 1,NMAT
      IF (AMCB(5,I).EQ.2 .OR. AMCB(5,I).EQ.4) IPRC = 2
      IF (AMCB(5,I) .GE. 3) ITYP = 2
  176 CONTINUE
      IPREC = ITYP + IPRC
C
      IF (ITEM.EQ.PVEC .OR. ITEM.EQ.PAPP) GO TO 300
C                                               ******
C                                                         *
C     PROCESS STIFFNESS, MASS OR DAMPING MATRICES         *
C                                                         *
C                                               ******
C
C     IF NMAT IS ODD, PUT FIRST RESULT ON ACOMB.  IF EVEN, PUT IT ON
C     SCR4.  FINAL RESULT WILL THEN BE ON ACOMB.
C
      CALL SORT (0,0,2,2,USE,2*NMAT)
      IRF = 1
      IF ((NMAT/2)*2 .EQ. NMAT) IRF = 2
      IFORM = 6
      RFILES(2) = SCR4
      ADDFLG =.FALSE.
C
      DO 230 KK = 1,NMAT
      J  = 2*KK - 1
      JN = JN + 2
      INUSE = USE(JN)
C
C     MOVE TRANSFORMATION MATRIX TO SCR2
C
      CALL MTRXI (SCR2,NAMESS(1,INUSE),HORG,Z(BUF1),RC)
C
C     IF INPUT MATRIX IS ON SOF, MOVE IT TO SCR1
C
      MCBB2(1) = 100 + INUSE
      IF (AMCB(1,INUSE) .GT. 0) GO TO 180
      MCBB2(1) = SCR1
      CALL MTRXI (SCR1,NAMESS(1,INUSE),ITEM,Z(BUF1),RC)
C
C     PERFORM TRIPLE MULTIPLY  H(T)*INPUT*H
C
  180 CALL SOFCLS
      MCBA2(1) = SCR2
      MCBC2(1) = 0
      IF (ADDFLG) MCBC2(1) = RFILES(3-IRF)
      ADDFLG = .TRUE.
      DO 190 J = 2,7
      MCBA2(J) = HMCB(J-1,INUSE)
      MCBB2(J) = AMCB(J,INUSE)
  190 CONTINUE
      IF (MCBB2(4) .LE. 2) IFORM = MCBB2(4)
      CALL MAKMCB (MCBD2,RFILES(IRF),HMCB(1,INUSE),IFORM,IPREC)
C
      CALL MPY3DR (Z)
C
      CALL WRTTRL (MCBD2)
      DO 220 J = 2,7
  220 MCBC2(J) = MCBD2(J)
      IRF = 3 - IRF
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
  230 CONTINUE
      GO TO 9999
C                                          ******
C                                                    *
C     PROCESS LOAD MATRICES                          *
C                                                    *
C****                                           ******
  300 MCBC(1) = 0
      MCBA(1) = SCR2
      TFLAG   = 1
      MRGZ    = LCORE
      PREC    = 0
C
C     SELECT FIRST RESULT FILE SO THAT FINAL RESULT WILL WIND UP ON
C     ACOMB
C
      RFILES(2) = SCR3
      RFILES(3) = SCR4
      IRF = 1
      IF (NMAT.EQ.2 .OR. NMAT.EQ.5) IRF = 2
      IF (NMAT.EQ.3 .OR. NMAT.EQ.6) IRF = 3
      IF (NMAT .EQ. 1) MCBP(1) = ACOMB
C
C     CREATE COLUMN PARTITIONING VECTOR FOR ALL MERGES
C     (VECTOR IS ALWAYS NULL)
C
      CALL MAKMCB (CPV,SCR6,NSIZE,RECT,RSP)
      TYPIN  = RSP
      TYPOUT = RSP
      IROW = 1
      NROW = 1
      INCR = 1
      CALL GOPEN (SCR6,Z(BUF1),WRTREW)
      CALL PACK  (0,SCR6,CPV)
      CALL CLOSE (SCR6,REW)
      ADDFLG =.TRUE.
C
      DO 400 KK = 1,NMAT
      INUSE = USE(2*KK-1)
C
C     COPY TRANSFORMATION MATRIX TO SCR2
C
      CALL MTRXI (SCR2,NAMESS(1,INUSE),HORG,Z(BUF1),RC)
C
C     IF LOAD MATRIX IS ON SOF, COPY IT TO SCR1
C
      MCBB(1) = 100 + INUSE
      IF (AMCB(1,INUSE) .GT. 0) GO TO 330
      MCBB(1) = SCR1
      CALL MTRXI (SCR1,NAMESS(1,INUSE),ITEM,Z(BUF1),RC)
C
C     MULTIPLY (HT * A) AND STORE RESULT ON RFILES(IRF)
C     (ACOMB,SCR3, OR SCR4)
C
  330 CALL SOFCLS
      DO 340 J = 2,7
      MCBA(J) = HMCB(J-1,INUSE)
      MCBB(J) = AMCB(J,INUSE)
  340 CONTINUE
      IF (MCBB(6).EQ.0 .OR. MCBA(3).EQ.MCBB(3)) GO TO 350
      I = KK
      NOGO = 1
      WRITE (NOUT,6304) SFM,I,NAMESS(1,I),NAMESS(2,I)
      GO TO 174
  350 CALL MAKMCB (MCBD,RFILES(IRF),HMCB(1,INUSE),RECT,IPREC)
C
      CALL MPYAD (Z,Z,Z)
C
      IF (ADDFLG) GO TO 390
C
C     COMPUTE ROW PARTITIONING VECTOR TO MERGE RESULT OF THIS MULTIPLY
C     WITH ALL PREVIOUS RESULTS
C
      K = AMCB(2,INUSE)
      CALL MAKMCB (RPV,SCR5,RSOFAR+K,RECT,RSP)
      IF (K .GT. LCORE) GO TO 9008
      DO 360 J = 1,K
  360 Z(J)  = 1.0E0
      TYPIN = RSP
      TYPOUT= RSP
      IROW  = RSOFAR + 1
      NROW  = RSOFAR + K
      INCR  = 1
      CALL GOPEN (SCR5,Z(BUF1),WRTREW)
      CALL PACK  (Z,SCR5,RPV)
      CALL CLOSE (SCR5,REW)
C
C     MERGE MATRICES   STORE RESULT ON NEXT AVAILABLE RFILE
C
      J = MOD(IRF,3) + 1
      CALL MAKMCB (MCBP,RFILES(J),NSIZE,RECT,IPREC)
      MCBP(2) = RPV(3)
      J = MOD(J,3) + 1
      MCBP11(1) = RFILES(J)
      MCBP12(1) = RFILES(IRF)
      DO 380 J = 2,7
      MCBP11(J) = MCBP(J)
      MCBP12(J) = MCBD(J)
  380 CONTINUE
C
      CALL MERGE (RPV,CPV,Z)
C
      IRF = MOD(IRF,3) + 1
      GO TO 395
  390 DO 391 J = 2,7
  391 MCBP(J) = MCBD(J)
  395 RSOFAR = RSOFAR + AMCB(2,INUSE)
      ADDFLG =.FALSE.
      IRF = MOD(IRF,3) + 1
      CALL SOFOPN (Z(SOF1),Z(SOF2),Z(SOF3))
  400 CONTINUE
      CALL WRTTRL (MCBP)
      GO TO 9999
C
C     DIAGNOSTICS
C
 6301 FORMAT (A25,' 6301, DATA MISSING IN GO MODE FOR SUBSTRUCTURE ',
     1        2A4,' ITEM ',A4)
 6302 FORMAT (A25,' 6302, ',2A4,' IS ILLEGAL MATRIX TYPE FOR MODULE ',
     1        'COMB2')
 6303 FORMAT (A25,' 6303, H OR G TRANSFORMATION MATRIX FOR SUBSTRUCTURE'
     1,       1X,2A4,' CANNOT BE FOUND ON SOF')
 6304 FORMAT (A25,' 6304, MODULE COMB2 INPUT MATRIX NUMBER ',I2,
     1        ' FOR SUBSTRUCTURE ,2A4,28H HAS INCOMPATIBLE DIMENSIONS')
 9008 CALL MESAGE (8,0,NAME)
C
C     NORMAL COMPLETION
C
 9999 CALL SOFCLS
      RETURN
      END
