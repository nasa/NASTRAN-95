      SUBROUTINE DPD3
C
C     DPD3 ASSEMBLES THE FREQUENCY RESPONSE LIST (FRL)
C     AND THE POWER SPECTRAL DENSITY LIST (PSDL).
C
      INTEGER         GPL   ,SIL   ,USET  ,USETD ,GPLD  ,SILD  ,DPOOL ,
     1                DLT   ,FRL   ,TFL   ,TRL   ,PSDL  ,EED   ,SCR1  ,
     2                SCR2  ,SCR3  ,SCR4  ,BUF   ,BUF1  ,BUF2  ,BUF3  ,
     3                BUF4  ,FLAG  ,FILE  ,EPOINT,SEQEP ,Z     ,LOADS ,
     5                RANDT2,DLOAD ,FREQ1 ,FREQ  ,TIC   ,TSTEP ,TF    ,
     6                PSD   ,EIGR  ,EIGB  ,EIGC  ,NGRID ,EQDYN ,SDT   ,
     7                FREQ2 ,RANDPS,RANDT1
      DIMENSION       BUF(24)   ,EPOINT(2)    ,SEQEP(2)     ,MCB(7)   ,
     1                NAM(2)    ,LOADS(32)    ,DLOAD(2)     ,FREQ1(2) ,
     2                FREQ(2)   ,ZZ(1)        ,BUFR(20)     ,NOLIN(21),
     3                TIC(2)    ,TSTEP(2)     ,TF(2)        ,PSD(2)   ,
     4                MSG(3)    ,EIGR(2)      ,EIGB(2)      ,EIGC(2)  ,
     5                FREQ2(2)  ,RANDPS(2)    ,RANDT1(2)    ,RANDT2(2)
      COMMON /CONDAS/ CONSTS(5)
      COMMON /BLANK / LUSET ,LUSETD,NOTFL ,NODLT ,NOPSDL,NOFRL ,NONLFT,
     1                NOTRL ,NOEED
      COMMON /NAMES / RD    ,RDREW ,WRT   ,WRTREW,CLSREW
      COMMON /DPDCOM/ DPOOL ,GPL   ,SIL   ,USET  ,GPLD  ,SILD  ,USETD ,
     1                DLT   ,FRL   ,NLFT  ,TFL   ,TRL   ,PSDL  ,EED   ,
     2                SCR1  ,SCR2  ,SCR3  ,SCR4  ,BUF   ,BUF1  ,BUF2  ,
     3                BUF3  ,BUF4  ,EPOINT,SEQEP ,L     ,KN    ,NEQDYN,
     4                LOADS ,DLOAD ,FREQ1 ,FREQ  ,NOLIN ,NOGO  ,
     5                MSG   ,TIC   ,TSTEP ,TF    ,PSD   ,EIGR  ,EIGB  ,
     6                EIGC  ,MCB   ,NAM   ,EQDYN ,SDT   ,INEQ
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (CONSTS(2),TWOPI), (Z(1),ZZ(1)), (BUF(1),BUFR(1)),
     1                (MSG(2),NGRID)
      DATA    FREQ2 , RANDPS , RANDT1 , RANDT2  /
     1       1107,11, 2107,21, 2207,22, 2307,23 /
C
C     OPEN DYNAMICS POOL. SET POINTERS.
C
      FILE = DPOOL
      CALL PRELOC (*2001,Z(BUF1),DPOOL)
      NOFRQ1 = 0
      NOFRQ2 = 0
      NOFRQ  = 0
      IFRQ1  = 1
      IFRQ2  = IFRQ1
      IFRQ   = IFRQ1
      I = IFRQ1
      J = I
C
C     READ FREQ1 CARDS. CONVERT F1 AND DELTA F TO RADIANS.
C
      CALL LOCATE (*1265,Z(BUF1),FREQ1,FLAG)
      NOFRQ1 = 1
 1261 CALL READ (*2002,*1262,DPOOL,Z(I),4,0,FLAG)
      ZZ(I+1) = TWOPI*ZZ(I+1)
      ZZ(I+2) = TWOPI*ZZ(I+2)
      I = I + 4
      GO TO 1261
 1262 NFRQ1 = I - 4
      IFRQ2 = I
      IFRQ  = I
      J = I
C
C     READ FREQ2 CARDS. CONVERT FREQUENCIES TO RADIANS.
C
 1265 CALL LOCATE (*1270,Z(BUF1),FREQ2,FLAG)
      NOFRQ2 = 1
 1266 CALL READ (*2002,*1267,DPOOL,Z(I),4,0,FLAG)
      ZZ(I+1) = TWOPI*ZZ(I+1)
      ZZ(I+2) = TWOPI*ZZ(I+2)
      I = I + 4
      GO TO 1266
 1267 NFRQ2 = I - 4
      IFRQ  = I
      J = I
C
C     READ FREQ CARDS. CONVERT FREQUENCIES TO RADIANS.
C
 1270 CALL LOCATE (*1274,Z(BUF1),FREQ,FLAG)
      NOFRQ = 1
 1271 CALL READ (*2002,*1274,DPOOL,Z(J+1),1,0,FLAG)
      J = J + 2
 1272 CALL READ (*2002,*2003,DPOOL,Z(J),1,0,FLAG)
      IF (Z(J) .EQ. -1) GO TO 1273
      ZZ(J) = TWOPI*ZZ(J)
      J = J + 1
      GO TO 1272
 1273 Z(I) = J - (I+1)
      I = J
      GO TO 1271
C
C     TEST FOR ANY FREQ TYPE CARDS.
C
 1274 NOFRL = NOFRQ1 + NOFRQ2 + NOFRQ
      IF (NOFRL .NE. 0) GO TO 1280
      GO TO 1276
 1275 INEQ  = 0
 1276 NOFRL =-1
      GO TO 1310
C
C     COLLECT LIST OF FREQUENCY SET IDS AND POINTERS TO CARDS.
C     SORT THIS LIST ON SET ID.
C
 1280 ILIST = J + 1
      I = ILIST
      IF (NOFRQ1 .EQ. 0) GO TO 1282
C
C     FOR FREQ1 SET STORE SET ID, POINTER TO SET, 0.
C
      DO 1281 K = IFRQ1,NFRQ1,4
      Z(I  ) = Z(K)
      Z(I+1) = K
      Z(I+2) = 0
 1281 I = I + 3
      NLIST = I - 3
 1282 IF (NOFRQ2 .EQ. 0) GO TO 1287
C
C     FOR FREQ2 SET STORE SET ID, POINTER TO SET, -1.
C
      DO 1286 K = IFRQ2,NFRQ2,4
      Z(I  ) = Z(K)
      Z(I+1) = K
      Z(I+2) =-1
 1286 I = I + 3
      NLIST = I - 3
 1287 IF (NOFRQ .EQ. 0) GO TO 1285
C
C     FOR FREQ SET STORE SET ID, POINTER TO SET, NO. OF WORDS IN SET.
C
      J = IFRQ
 1283 N = Z(J)
      IF (N .EQ. -1) GO TO 1284
      J = J + 1
      Z(I  ) = Z(J)
      Z(I+1) = J
      Z(I+2) = N
      I = I + 3
      J = J + N
      GO TO 1283
 1284 NLIST = I - 3
 1285 N = I - ILIST
      CALL SORT (0,0,3,1,Z(ILIST),N)
C
C     OPEN THE FRL. WRITE NAME + SET IDS IN HEADER.
C
      FILE = FRL
      CALL OPEN (*1275,FRL,Z(BUF2),WRTREW)
      CALL FNAME (FRL,BUF)
      CALL WRITE (FRL,BUF,2,0)
      DO 1291 I = ILIST,NLIST,3
      BUF(1) = Z(I)
 1291 CALL WRITE (FRL,BUF,1,0)
      CALL WRITE (FRL,0,0,1)
C
C     WRITE THE FRL ONE RECORD PER FREQUENCY SET.
C     CONVERT FREQ1 SETS TO LOOK LIKE FREQ SETS.
C     CONVERT FREQ2 SETS TO LOOK LIKE FREQ SETS.
C
      DO 1308 I = ILIST,NLIST,3
      J = Z(I+1)
      N = Z(I+2)
      IF (N) 1304,1301,1303
C
C     FREQ SET ---  SORT FREQUENCY LIST AND DISCARD ANY DUPLICATES.
C     THEN WRITE FREQUENCIES ON THE FRL
C
 1303 N = N - 1
      IF (N .EQ. 1) GO TO 1307
      CALL SORTF (0,0,1,1,Z(J+1),N)
      J1 = J + 2
      JN = J + N
      IX = J + 1
      DO 1306 JX = J1,JN
      IF (Z(JX) .EQ. Z(IX)) GO TO 1306
      IX = IX + 1
      Z(IX) = Z(JX)
 1306 CONTINUE
      N = IX - J
 1307 CALL WRITE (FRL,Z(J+1),N,1)
      GO TO 1308
C
C     FREQ1 SET-- FORM F = F0 + (I-1)*DELTA F, WHERE I = 1 THRU N+1.
C
 1301 F0   = ZZ(J+1)
      DELF = ZZ(J+2)
      N  = Z(J+3) + 1
      FI = 0.
      DO 1302 K = 1,N
      F = F0 + FI*DELF
      CALL WRITE (FRL,F,1,0)
 1302 FI = FI + 1.0
      CALL WRITE (FRL,0,0,1)
      GO TO 1308
C
C     FREQ2 SET-- FORM F = F0*10.0**((I-1)*DELTA)
C     WHERE DELTA = (LOG10(FE/F0))/N AND I = 1 THRU N+1.
C
 1304 F0 = ZZ(J+1)
      FE = ZZ(J+2)
      N  =  Z(J+3)
      FN = N
      DELTA = (ALOG10(FE/F0))/FN
      FI = 0.
      N  = N + 1
      DO 1305 K = 1,N
      F  = F0*10.0**(FI*DELTA)
      CALL WRITE (FRL,F,1,0)
 1305 FI = FI + 1.0
      CALL WRITE (FRL,0,0,1)
 1308 CONTINUE
C
C     CLOSE FRL AND WRITE TRAILER.
C
      MCB(1) = FRL
      MCB(2) = (NLIST-ILIST)/3 + 1
      CALL WRTTRL (MCB)
      CALL CLOSE (FRL,CLSREW)
      INEQ = 0
C
C     OPEN PSDL. IF PURGED, BYPASS PSDL PROCESSING.
C     OTHERWISE, LOCATE RANDPS CARDS. IF ABSENT, BYPASS PSDL PROCESSING.
C
 1310 FILE = PSDL
      CALL OPEN (*1381,PSDL,Z(BUF2),WRTREW)
      CALL LOCATE (*1381,Z(BUF1),RANDPS,FLAG)
C
C     READ RANDPS CARDS INTO CORE.
C
      IRPS = 1
      FILE = DPOOL
      CALL READ (*2002,*1322,DPOOL,Z(IRPS),BUF2-IRPS,1,NRPS)
      GO TO 2004
 1322 IRT1 = IRPS + NRPS
      IRT2 = IRT1
      I    = IRT1
      J    = I
      NORT1= 0
      NORT2= 0
C
C     READ RANDT1 CARDS.
C
      CALL LOCATE (*1340,Z(BUF1),RANDT1,FLAG)
      CALL READ (*2002,*1332,DPOOL,Z(IRT1),BUF2-IRT1,1,NORT1)
      GO TO 2004
 1332 IRT2 = IRT1 + NORT1
      NRT1 = IRT2 - 4
      I = IRT2
      J = I
C
C     READ RANDT2 CARDS.
C
 1340 CALL LOCATE (*1350,Z(BUF1),RANDT2,FLAG)
      NORT2 = 1
 1341 CALL READ (*2002,*1350,DPOOL,Z(J+1),1,0,FLAG)
      J = J + 2
 1342 CALL READ (*2002,*2003,DPOOL,Z(J),1,0,FLAG)
      IF (Z(J) .EQ. -1) GO TO 1343
      J = J + 1
      IF (J .LT. BUF2) GO TO 1342
      GO TO 2004
 1343 Z(I) = J - (I+1)
      I = J
      GO TO 1341
C
C     COLLECT LIST OF RANDT1 AND RANDT2 SET IDS AND POINTERS TO DATA.
C
 1350 NORT = NORT1 + NORT2
      IF (NORT .EQ. 0) GO TO 1360
      ILIST = J + 1
      I = ILIST
      IF (NORT1 .EQ. 0) GO TO 1352
C
C     FOR RANDT1 SETS STORE SET ID, POINTER TO SET, 0.
C
      DO 1351 K = IRT1,NRT1,4
      Z(I  ) = Z(K)
      Z(I+1) = K
      Z(I+2) = 0
 1351 I = I + 3
      NLIST = I - 3
      IF (I .GT.  BUF2) GO TO 2004
 1352 IF (NORT2 .EQ. 0) GO TO 1355
C
C     FOR RANDT2 SETS STORE SET ID, POINTER TO SET, NO. OF WORDS IN SET.
C
      J = IRT2
 1353 N = Z(J)
      IF (N .EQ. -1) GO TO 1354
      Z(I  ) = Z(J)
      Z(I+1) = J
      Z(I+2) = N
      I = I + 3
      J = J + N
      IF (I .LT. BUF2) GO TO 1353
      GO TO 2004
 1354 NLIST = I - 3
C
C     SORT LIST ON SET ID.
C
 1355 N = I - ILIST
      CALL SORT (0,0,3,1,Z(ILIST),N)
C
C     WRITE SET IDS FOR RANDT1 AND RANDT2 CARDS IN HEADER RECORD OF
C     PSDL. THEN WRITE RANDPS DATA AS FIRST RECORD OF PSDL.
C
 1360 CALL FNAME (PSDL,BUF)
      CALL WRITE (PSDL,BUF,2,0)
      IF (NORT .EQ. 0) GO TO 1362
      DO 1361 I = ILIST,NLIST,3
 1361 CALL WRITE (PSDL,Z(I),1,0)
 1362 CALL WRITE (PSDL,0,0,1)
      CALL WRITE (PSDL,Z(IRPS),NRPS,1)
      IF (NORT .EQ. 0) GO TO 1380
C
C     WRITE ONE RECORD ON PSDL FOR EACH RANDT1 OR RANDT2 SET.
C
      DO 1378 I = ILIST,NLIST,3
      J = Z(I+1)
      N = Z(I+2)
      IF (N .EQ. 0) GO TO 1372
C
C     RANDT2 SET--  SORT DATA AND DISCARD ANY DUPLICATES. THEN WRITE SET
C
      N = N - 1
      IF (N .EQ. 1) GO TO 1376
      CALL SORTF (0,0,1,1,Z(J+1),N)
      J1 = J + 2
      JN = J + N
      IX = J + 1
      DO 1375 JX = J1,JN
      IF (Z(JX) .EQ. Z(IX)) GO TO 1375
      IX = IX + 1
      Z(IX) = Z(JX)
 1375 CONTINUE
      N = IX - J
 1376 CALL WRITE (PSDL,Z(J+1),N,1)
      GO TO 1378
C
C     RANDT1 SET-- WRITE TI = T0 + (I-1)*DELTA T, WHERE I = 1 THRU N+1.
C
 1372 N  = Z(J+1)
      FN = N
      DELT = (ZZ(J+3)-ZZ(J+2))/FN
      T0 = ZZ(J+2)
      FI = 0.
      N  = N + 1
      DO 1373 K = 1,N
      TI = T0 + FI*DELT
      CALL WRITE (PSDL,TI,1,0)
 1373 FI = FI + 1.0
      CALL WRITE (PSDL,0,0,1)
 1378 CONTINUE
C
C     CLOSE FILES, WRITE TRAILER AND EXIT.
C
 1380 MCB(1) = PSDL
      MCB(2) = (NLIST-ILIST)/3 + 1
C      2147483647  = 2**31 - 1
      IF (NORT .EQ. 0) MCB(2) = 2147483647
      CALL WRTTRL (MCB)
      INEQ  = 0
      NOPSDL= 1
 1381 CALL CLOSE (DPOOL,CLSREW)
      CALL CLOSE (PSDL ,CLSREW)
      RETURN
C
C     FATAL FILE ERRORS
C
 2001 N = -1
      GO TO 2005
 2002 N = -2
      GO TO 2005
 2003 N = -3
      GO TO 2005
 2004 N = -8
 2005 CALL MESAGE (N,FILE,NAM)
      RETURN
      END
