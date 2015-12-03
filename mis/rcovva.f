      SUBROUTINE RCOVVA (IN,INTYP,OUTT,OUTU,OUTV,OUTA,SSNM,RZ,DZ,CZ)
C
C     THIS SUBROUTINE COMPUTES THE VELOCITIES AND ACCELERATIONS FOR
C     FOR A GIVEN DISPLACEMENT VECTOR
C
C     INTYP = 0   IN CONTAINS U ONLY AND V AND A ARE CALCULATED
C     INTYP = 1   U CONTAINS U, V AND A SO THEY ARE SPLIT ONTO OUTU,
C                 OUTV AND OUTA
C     INTYP =-1   OUTU, OUTV AND OUTA ARE MERGED ONTO OUTT
C
      INTEGER          DRY        ,RSS        ,UA         ,NAME(2)    ,
     1                 RC         ,RFNO       ,MCBU(7)    ,MCBV(7)    ,
     2                 SRD        ,SOLN       ,BUF1       ,BUF2       ,
     3                 BUF3       ,BUF4       ,INBLK3(15) ,OUTBLK(15) ,
     4                 SYSBUF     ,OUTT       ,OUTU       ,OUTV       ,
     5                 OUTA       ,MCB(7)     ,MCBA(7)    ,INBLK(15)  ,
     6                 OBLK1(15)  ,OBLK2(15)  ,OBLK3(15)  ,TEMP(4)    ,
     7                 FILE       ,SSNM(2)    ,INBLK1(15) ,INBLK2(15)
      REAL             RZ(4)      ,FREQ       ,RSCALE     ,ISCALE
      DOUBLE PRECISION DZ(1)      ,RVAL       ,IVAL
      COMPLEX          CZ(2)      ,SCALE
      CHARACTER        UFM*23     ,UWM*25     ,UIM*29     ,SFM*25     ,
     1                 SWM*27
      COMMON /XMSSG /  UFM        ,UWM        ,UIM        ,SFM        ,
     1                 SWM
      COMMON /BLANK /  DRY        ,LOOP       ,STEP       ,FSS(2)     ,
     1                 RFNO       ,NEIGV      ,LUI        ,UINMS(2,5) ,
     2                 NOSORT     ,UTHRES     ,PTHRES     ,QTHRES
      COMMON /RCOVCR/  ICORE      ,LCORE      ,BUF1       ,BUF2       ,
     1                 BUF3       ,BUF4       ,SOF1       ,SOF2       ,
     2                 SOF3
      COMMON /RCOVCM/  MRECVR     ,UA         ,PA         ,QA         ,
     1                 IOPT       ,RSS(2)     ,ENERGY     ,UIMPRO     ,
     2                 RANGE(2)   ,IREQ       ,LREQ       ,LBASIC
      COMMON /SYSTEM/  SYSBUF     ,NOUT
      COMMON /NAMES /  RD         ,RDREW      ,WRT        ,WRTREW     ,
     1                 REW        ,NOREW      ,EOFNRW     ,RSP        ,
     2                 RDP        ,CSP        ,CDP        ,SQUARE     ,
     3                 RECT       ,DIAG       ,UPPER      ,LOWER      ,
     4                 SYM
      COMMON /CONDAS/  PHI        ,TWOPHI
      COMMON /PACKX /  ITINP      ,ITOUTP     ,IRP        ,NRP        ,
     1                 INCRP
      COMMON /UNPAKX/  ITINU      ,IRU        ,NRU        ,INCRU
      COMMON /TYPE  /  PR(2)      ,NWORDS(4)
      EQUIVALENCE      (TEMP(1),SCALE,RSCALE) ,(TEMP(2),ISCALE)       ,
     1                 (INBLK1(1),OBLK1(1))   ,(INBLK2(1),OBLK2(1))   ,
     2                 (INBLK3(1),OBLK3(1))   ,(OUTBLK(1),INBLK(1))
      DATA    NAME  /  4HRCOV,4HVA   /
      DATA    SRD   /  1             /
      DATA    SOLN  /  4HSOLN        /
C
C     GET DISPLACEMENT TRAILER AND DETERMINE TYPE
C
      IF (OUTT.NE.0 .AND. OUTU+OUTV+OUTA.NE.0 .AND. INTYP.GE.0)
     1   GO TO 9007
C
      FILE = IN
      IF (INTYP .LT. 0) FILE = OUTU
      MCB(1) = FILE
      CALL RDTRL (MCB)
      IF (MCB(1) .LT. 0) GO TO 9001
      NCOL  = MCB(2)
      NROW  = MCB(3)
      IPREC = MCB(5)
      NWORD = NWORDS(IPREC)
      NWCOL = NROW*NWORD
C
C     SET UP PACK UNPACK COMMONS
C
      ITINU = IPREC
      IRU   = 1
      NRU   = NROW
      INCRU = 1
      ITINP = IPREC
      ITOUTP= IPREC
      IRP   = 1
      NRP   = NROW
      INCRP = 1
C
C     BRANCH ON TYPE OF DISPLACEMENTS OR RIGID FORMAT
C
      IF (INTYP .GT. 0) GO TO 400
      IF (INTYP .LT. 0) GO TO 500
C
      IF (RFNO .GT. 9) GO TO 9007
      GO TO (600,600,100,9007,9007,9007,9007,200,400), RFNO
C
C     NORMAL MODES
C
C     CHECK IF VECTORS ARE COMPLEX
C
  100 IF (IPREC .GE. 3) GO TO 200
C
C     REAL NORMAL MODES
C
C     V =  U*OMEGA
C     A = -V*OMEGA
C
      IF (LCORE .LT. NWCOL) GO TO 6313
      ITEM = SOLN
      CALL SFETCH (SSNM,SOLN,SRD,RC)
      IF (RC .NE. 1) GO TO 6000
      N = 1
      CALL SJUMP (N)
      IF (N .LT. 0) GO TO 6100
C
      CALL GOPEN (IN,RZ(BUF1),RDREW)
      IF (OUTT .NE. 0) CALL GOPEN (OUTT,RZ(BUF2),WRTREW)
      IF (OUTU .NE. 0) CALL GOPEN (OUTU,RZ(BUF2),WRTREW)
      IF (OUTV .NE. 0) CALL GOPEN (OUTV,RZ(BUF3),WRTREW)
      IF (OUTA .NE. 0) CALL GOPEN (OUTA,RZ(BUF4),WRTREW)
      CALL MAKMCB (MCB,OUTT,NROW,RECT,IPREC)
      CALL MAKMCB (MCBU,OUTU,NROW,RECT,IPREC)
      CALL MAKMCB (MCBV,OUTV,NROW,RECT,IPREC)
      CALL MAKMCB (MCBA,OUTA,NROW,RECT,IPREC)
C
C     LOOP THROUGH EACH COLUMN
C
      DO 180 I = 1,NCOL
C
C     GET SCALE FACTOR FOR THIS COLUMN
C
      CALL SUREAD (RZ,7,NWDS,RC)
      IF (RC .NE. 1) GO TO 6200
      RSCALE = RZ(4)
C
      CALL UNPACK (*110,IN,RZ)
      GO TO 120
  110 DO 105 J = 1,NWCOL
  105 RZ(J) = 0.0
  120 IF (OUTT .NE. 0) CALL PACK (RZ(1),OUTT,MCB)
      IF (OUTU .NE. 0) CALL PACK (RZ(1),OUTU,MCBU)
C
      DO 170 J = 1,2
      IF (IPREC .EQ. 2) GO TO 140
C
      DO 130 K = 1,NROW
  130 RZ(K) = RSCALE*RZ(K)
      GO TO 160
C
  140 DO 150 K = 1,NROW
  150 DZ(K) = RSCALE*DZ(K)
C
  160 IF (OUTT .NE. 0) CALL PACK (RZ(1),OUTT,MCB)
      IF (OUTV.NE.0 .AND. J.EQ.1) CALL PACK (RZ(1),OUTV,MCBV)
      IF (OUTA.NE.0 .AND. J.EQ.2) CALL PACK (RZ(1),OUTA,MCBA)
C
      RSCALE = -RSCALE
  170 CONTINUE
  180 CONTINUE
C
C
      CALL CLOSE (IN,REW)
      IF (OUTT .NE. 0) CALL CLOSE (OUTT,REW)
      IF (OUTU .NE. 0) CALL CLOSE (OUTU,REW)
      IF (OUTV .NE. 0) CALL CLOSE (OUTV,REW)
      IF (OUTA .NE. 0) CALL CLOSE (OUTA,REW)
      IF (OUTT .NE. 0) CALL WRTTRL (MCB)
      IF (OUTU .NE. 0) CALL WRTTRL (MCBU)
      IF (OUTV .NE. 0) CALL WRTTRL (MCBV)
      IF (OUTA .NE. 0) CALL WRTTRL (MCBA)
      GO TO 600
C
C     COMPLEX NORMAL MODES
C
C     V = U*POLE
C     A = V*POLE
C
C     FREQUENCY RESPONSE
C
C     V = U*TWOPHI*FREQ*I
C     A = V*TWOPHI*FREQ*I
C
  200 IF (LCORE .LT. NWCOL) GO TO 6313
      ITEM = SOLN
      CALL SFETCH (SSNM,SOLN,SRD,RC)
      IF (RC .NE. 1) GO TO 6000
      N = 1
      CALL SJUMP (N)
      IF (N .LT. 0) GO TO 6100
C
      CALL GOPEN (IN,RZ(BUF1),RDREW)
      IF (OUTT .NE. 0) CALL GOPEN (OUTT,RZ(BUF2),WRTREW)
      IF (OUTU .NE. 0) CALL GOPEN (OUTU,RZ(BUF2),WRTREW)
      IF (OUTV .NE. 0) CALL GOPEN (OUTV,RZ(BUF3),WRTREW)
      IF (OUTA .NE. 0) CALL GOPEN (OUTA,RZ(BUF4),WRTREW)
      CALL MAKMCB (MCB ,OUTT,NROW,RECT,IPREC)
      CALL MAKMCB (MCBU,OUTU,NROW,RECT,IPREC)
      CALL MAKMCB (MCBV,OUTV,NROW,RECT,IPREC)
      CALL MAKMCB (MCBA,OUTA,NROW,RECT,IPREC)
C
C     LOOP THROUGH EACH COLUMN
C
      DO 290 I = 1,NCOL
C
C     GET SCALE FACTOR FOR THIS COLUMN
C
      IF (RFNO .EQ. 8) GO TO 204
      CALL SUREAD (CZ(1),7,NWDS,RC)
      IF (RC .NE. 1) GO TO 6200
      SCALE = CZ(2)
      GO TO 206
  204 CALL SUREAD (FREQ,1,NWDS,RC)
      IF (RC .NE. 1) GO TO 6200
      SCALE = TWOPHI*FREQ*(0.0,1.0)
C
  206 CALL UNPACK (*210,IN,CZ(1))
      GO TO 230
  210 DO 220 J = 1,NWCOL
  220 RZ(J) = 0.0
  230 IF (OUTT .NE. 0) CALL PACK (CZ(1),OUTT,MCB)
      IF (OUTU .NE. 0) CALL PACK (CZ(1),OUTU,MCBU)
C
      DO 280 J = 1,2
      IF (IPREC .GT. 3) GO TO 250
C
      DO 240 K = 1,NROW
  240 CZ(K) = SCALE*CZ(K)
      GO TO 270
C
  250 NT = NROW*2
      DO 260 K = 1,NT,2
      RVAL = DZ(K  )
      IVAL = DZ(K+1)
      DZ(K  ) = RSCALE*RVAL - ISCALE*IVAL
  260 DZ(K+1) = RSCALE*IVAL + ISCALE*RVAL
C
  270 IF (OUTT .NE. 0) CALL PACK (CZ(1),OUTT,MCB)
      IF (OUTV.NE.0 .AND. J.EQ.1) CALL PACK (CZ(1),OUTV,MCBV)
      IF (OUTA.NE.0 .AND. J.EQ.2) CALL PACK (CZ(1),OUTA,MCBA)
C
  280 CONTINUE
C
  290 CONTINUE
C
      CALL CLOSE (IN,REW)
      IF (OUTT .NE. 0) CALL CLOSE (OUTT,REW)
      IF (OUTU .NE. 0) CALL CLOSE (OUTU,REW)
      IF (OUTV .NE. 0) CALL CLOSE (OUTV,REW)
      IF (OUTA .NE. 0) CALL CLOSE (OUTA,REW)
      IF (OUTT .NE. 0) CALL WRTTRL (MCB)
      IF (OUTU .NE. 0) CALL WRTTRL (MCBU)
      IF (OUTV .NE. 0) CALL WRTTRL (MCBV)
      IF (OUTA .NE. 0) CALL WRTTRL (MCBA)
      GO TO 600
C
C     THE DISPLACEMENT FILE ALREADY CONTAINS THE VELOCITIES AND
C     ACCELERATIONS SO WE JUST SANT TO SPLIT THEM UP
C
  400 IF (LCORE .LT. 0) GO TO 6313
      CALL GOPEN (IN,RZ(BUF1),RDREW)
      IF (OUTU .NE. 0) CALL GOPEN (OUTU,RZ(BUF2),WRTREW)
      IF (OUTV .NE. 0) CALL GOPEN (OUTV,RZ(BUF3),WRTREW)
      IF (OUTA .NE. 0) CALL GOPEN (OUTA,RZ(BUF4),WRTREW)
C
      INBLK(1) = IN
      OBLK1(1) = OUTU
      OBLK2(1) = OUTV
      OBLK3(1) = OUTA
      FILE = IN
      NCOL = NCOL/3
C
      DO 410 I = 1,NCOL
      IF (OUTU .NE. 0) CALL CPYSTR (INBLK,OBLK1,0,I)
      IF (OUTU .EQ. 0) CALL FWDREC (*9002,IN)
      IF (OUTV .NE. 0) CALL CPYSTR (INBLK,OBLK2,0,I)
      IF (OUTV .EQ. 0) CALL FWDREC (*9002,IN)
      IF (OUTA .NE. 0) CALL CPYSTR (INBLK,OBLK3,0,I)
      IF (OUTA .EQ. 0) CALL FWDREC (*9002,IN)
  410 CONTINUE
C
      CALL CLOSE (IN,REW)
      IF (OUTU .NE. 0) CALL CLOSE (OUTU,REW)
      IF (OUTV .NE. 0) CALL CLOSE (OUTV,REW)
      IF (OUTA .NE. 0) CALL CLOSE (OUTA,REW)
      MCB(2) = NCOL
      MCB(1) = OUTU
      IF (OUTU .NE. 0) CALL WRTTRL (MCB)
      MCB(1) = OUTV
      IF (OUTV .NE. 0) CALL WRTTRL (MCB)
      MCB(1) = OUTA
      IF (OUTA .NE. 0) CALL WRTTRL (MCB)
      GO TO 600
C
C     THE DISPLACEMENTS, VELOCITIES AND ACCLERATIONS ALREADY EXIST AND
C     ARE TO BE MERGED TOGETHER
C
  500 IF (LCORE .LT. 0) GO TO 6313
      CALL GOPEN (OUTU,RZ(BUF1),RDREW)
      CALL GOPEN (OUTV,RZ(BUF2),RDREW)
      CALL GOPEN (OUTA,RZ(BUF3),RDREW)
      CALL GOPEN (OUTT,RZ(BUF4),WRTREW)
C
      INBLK1(1) = OUTU
      INBLK2(1) = OUTV
      INBLK3(1) = OUTA
      OUTBLK(1) = OUTT
C
      J = 1
      DO 510 I = 1,NCOL
      CALL CPYSTR (INBLK1,OUTBLK,0,J)
      J = J + 1
      CALL CPYSTR (INBLK2,OUTBLK,0,J)
      J = J + 1
      CALL CPYSTR (INBLK3,OUTBLK,0,J)
      J = J + 1
  510 CONTINUE
C
      CALL CLOSE (OUTU,REW)
      CALL CLOSE (OUTV,REW)
      CALL CLOSE (OUTA,REW)
      CALL CLOSE (OUTT,REW)
      MCB(1) = OUTT
      MCB(2) = NCOL*3
      CALL WRTTRL (MCB)
C
C     NORMAL RETURN
C
  600 RETURN
C
C     ERRORS
C
 6000 IF (RC .EQ. 6) GO TO 9100
      CALL SMSG (RC-2,ITEM,SSNM)
      GO TO 9200
 6100 CALL SMSG (7,ITEM,SSNM)
      GO TO 9200
 6200 CALL SMSG (RC+4,ITEM,SSNM)
      GO TO 9200
 6313 WRITE  (NOUT,6314) SWM,RSS
 6314 FORMAT (A25,' 6313, INSUFFICIENT CORE FOR RCOVR MODULE WHILE ',
     1       'TRYING TO PROCESS', /34X,'PRINTOUT DATA BLOCKS FOR ',
     2       'SUBSTRUCTURE ',2A4)
      GO TO 9200
 9001 N = 1
      GO TO 9100
 9002 N = 2
      GO TO 9100
 9007 N = 7
 9100 CALL MESAGE (N,FILE,NAME)
 9200 IN = 0
      CALL CLOSE (IN,REW)
      IF (OUTT .NE. 0) CALL CLOSE (OUTT,REW)
      IF (OUTU .NE. 0) CALL CLOSE (OUTU,REW)
      IF (OUTV .NE. 0) CALL CLOSE (OUTV,REW)
      IF (OUTA .NE. 0) CALL CLOSE (OUTA,REW)
C
      RETURN
      END
