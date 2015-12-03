      SUBROUTINE DPD2
C
C     DPD2 ASSEMBLES THE DYNAMIC LOADS TABLE (DLT).
C
      INTEGER         GPL   ,SIL   ,USET  ,USETD ,GPLD  ,SILD  ,DPOOL ,
     1                DLT   ,FRL   ,TFL   ,TRL   ,PSDL  ,EED   ,SCR1  ,
     2                SCR2  ,SCR3  ,SCR4  ,BUF   ,BUF1  ,BUF2  ,BUF3  ,
     3                BUF4  ,FLAG  ,FILE  ,EPOINT,SEQEP ,Z     ,LOADS ,
     5                SDT   ,DLOAD ,FREQ1 ,FREQ  ,TIC   ,TSTEP ,TF    ,
     6                PSD   ,EIGR  ,EIGB  ,EIGC  ,NGRID ,EQDYN ,SCR   ,
     7                BUFX
      DIMENSION       BUF(24)      ,EPOINT(2)    ,SEQEP(2)     ,MCB(7),
     1                NAM(2),LOADS(32)    ,DLOAD(2)     ,FREQ1(2)     ,
     2                FREQ(2)      ,ZZ(1) ,BUFR(20)     ,NOLIN(21)    ,
     3                TIC(2),TSTEP(2)     ,TF(2) ,PSD(2),MSG(3),EIGR(2)
     4,               EIGB(2)      ,EIGC(2)      ,SCR(4),BUFX(3)
      COMMON /BLANK / LUSET ,LUSETD,NOTFL ,NODLT ,NOPSDL,NOFRL ,NONLFT,
     1                NOTRL ,NOEED ,NOSDT ,NOUE
      COMMON /SYSTEM/ IDUMMY(55)   ,ITHRML
      COMMON /NAMES / RD    ,RDREW ,WRT   ,WRTREW,CLSREW
      COMMON /DPDCOM/ DPOOL ,GPL   ,SIL   ,USET  ,GPLD  ,SILD  ,USETD ,
     1                DLT   ,FRL   ,NLFT  ,TFL   ,TRL   ,PSDL  ,EED   ,
     2                SCR1  ,SCR2  ,SCR3  ,SCR4  ,BUF   ,BUF1  ,BUF2  ,
     3                BUF3  ,BUF4  ,EPOINT,SEQEP ,L     ,KN    ,NEQDYN,
     4                LOADS ,DLOAD ,FREQ1 ,FREQ  ,NOLIN ,NOGO  ,MSG   ,
     5                TIC   ,TSTEP ,TF    ,PSD   ,EIGR  ,EIGB  ,EIGC  ,
     6                MCB   ,NAM   ,EQDYN ,SDT   ,INEQ
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (Z(1),ZZ(1)), (BUF(1),BUFR(1)), (MSG(2),NGRID),
     1                (SCR1,SCR(1)),(BUF2 ,BUFX(1))
C
C     OPEN DYNAMICS POOL. SET POINTERS TO LOOP THRU DAREA, DELAY
C     AND DPHASE TABLES.
C
      FILE = DPOOL
      CALL PRELOC (*2001,Z(BUF1),DPOOL)
      II  = 1
      III = 1
      ITABL = NEQDYN + 2
      L   = 2
      J   = BUF4 - 1
      MSG(1) = 66
C
C     LOCATE CARD TYPE. IF PRESENT--
C     STORE POINTER TO 1ST TABLE NO. IN LOADS TABLE, OPEN SCRATCH FILE
C     FOR TABLES, SET ID = 0.
C
 1110 CALL LOCATE (*1141,Z(BUF1),LOADS(II),FLAG)
      LOADS(II+2) = J
      FILE = SCR(III)
      CALL OPEN (*2001,FILE,Z(BUF2),WRTREW)
      ID   = 0
C
C     READ A CARD. IF TABLE NO. IS DIFFERENT, STORE TABLE NO. IN TABLE
C     LIST.  IF NOT FIRST CARD, SORT TABLE ON SIL NO. AND WRITE ON
C     SCRATCH FILE.
C
 1120 CALL READ (*2002,*1140,DPOOL,BUF,4,0,FLAG)
      IF (BUF(1) .EQ. ID) GO TO 1130
      IF (ID .EQ. 0) GO TO 1122
      N = I - ITABL
      CALL SORT (0,0,2,1,Z(ITABL),N)
      CALL WRITE (FILE,Z(ITABL),N,1)
 1122 ID = BUF(1)
      Z(J) = ID
      J = J - 1
      I = ITABL
      MSG(3) = ID
C
C     CONVERT POINT AND COMPONENT TO SIL NO.
C     STORE SIL NO. AND VALUE IN CORE.
C
 1130 CALL DPDAA
      Z(I  ) = BUF(2)
      Z(I+1) = BUF(4)
      I = I + 2
      IF (I .LT. J) GO TO 1120
      CALL MESAGE (-8,0,NAM)
C
C     HERE WHEN LAST CARD OF CURRENT TYPE HAS BEEN READ--
C     SORT AND WRITE LAST RECORD. CLOSE SCRATCH FILE.  STORE
C     NUMBER OF TABLES IN TABLE LIST. TEST FOR ALL CARD TYPES PROCESSED.
C
 1140 N = I - ITABL
      CALL SORT (0,0,2,1,Z(ITABL),N)
      CALL WRITE (FILE,Z(ITABL),N,1)
      CALL CLOSE (FILE,CLSREW)
      LOADS(II+3) = LOADS(II+2) - J
 1141 II  = II  + 4
      III = III + 1
      IF (III .LE. 3) GO TO 1110
C
C     SET POINTERS TO LOOP THRU RLOAD1,2 AND TLOAD1,2 CARDS
C
      NCORE = J
      J     = 1
      III   = 1
      INEQ  = 0
C
C     LOCATE A CARD TYPE. IF PRESENT--
C     READ ALL CARDS OF TYPE INTO CORE.
C
 1160 CALL LOCATE (*1165,Z(BUF1),LOADS(II),FLAG)
      M = LOADS(II+2)
 1161 Z(J) = III
      CALL READ (*2002,*1165,DPOOL,Z(J+1),M,0,FLAG)
      J = J + 11
      IF (J .LT. NCORE) GO TO 1161
      CALL MESAGE (-8,0,NAM)
C
C     TEST FOR ALL CARD TYPES PROCESSED.
C     IF SO, SORT CARDS ON LOAD SET ID.
C
 1165 II  = II  + 4
      III = III + 1
      IF (III .LE. 4) GO TO 1160
      N   = J - 1
      IF (N .NE. 0) GO TO 1166
      CALL CLOSE (DPOOL,CLSREW)
      RETURN
C
 1166 CALL SORT (0,0,11,2,Z,N)
      NLIST = J - 11
C
C     LOCATE DLOAD CARDS ON DYNAMICS POOL.
C     IF PRESENT READ INTO CORE. SORT EACH DLOAD CARD ON REFERENCED SET
C     ID.
C
      NODLD  = 0
      CALL LOCATE (*1174,Z(BUF1),DLOAD,FLAG)
      IDLOAD = J
      I = IDLOAD
      J = I
 1171 CALL READ (*2002,*1174,DPOOL,Z(J+1),2,0,FLAG)
      J = J + 3
      NODLD = NODLD + 1
 1172 CALL READ (*2002,*2003,DPOOL,Z(J),2,0,FLAG)
      IF (Z(J) .EQ. -1) GO TO 1173
      J = J + 2
      IF (J .GE. NCORE) CALL MESAGE (-8,0,NAM)
      GO TO 1172
 1173 N = J - (I+3)
      CALL SORT (0,0,2,2,Z(I+3),N)
C
C     CHECK FOR DLOAD SET ID UNIQUENESS
C
      DO 11731 KK = 2,N,2
      JJ = I + 2 + KK
      IF (KK .GE. N) GO TO 11731
      IF (Z(JJ) .NE. Z(JJ+2)) GO TO 11731
      NOGO = 1
      MSG(2) = Z(I+1)
      MSG(3) = Z(JJ)
      CALL MESAGE (30,135,MSG(2))
11731 CONTINUE
      Z(I) = N + 2
      I = J
      GO TO 1171
 1174 CALL CLOSE (DPOOL,CLSREW)
C
C     OPEN THE DLT. WRITE NAME IN HEADER RECORD.
C     THEN WRITE NO. OF DLOAD CARDS FOLLOWED BY DLOAD SET IDS.
C     THEN WRITE SET IDS FOR EACH RECORD OF THE DLT (FOLLOWING DLOAD
C     RECORD)
C
      FILE = DLT
      CALL OPEN (*1249,DLT,Z(BUF1),WRTREW)
      CALL FNAME (DLT,BUF)
      BUF(3) = NODLD
      CALL WRITE (DLT,BUF,3,0)
      IF (NODLD .EQ. 0) GO TO 1182
      I = IDLOAD
      J = 1
 1181 CALL WRITE (DLT,Z(I+1),1,0)
      I = I + Z(I) + 1
      J = J + 1
      IF (J .LE. NODLD) GO TO 1181
C
C     CHECK DLOAD SID  VS  RLOAD1,2 AND TLOAD1,2 FOR UNIQUENESS
C
      I = IDLOAD
      DO 11810 JJ = 1,NODLD
      ITEMP = Z(I+1)
      DO 11811 KK = 1,NLIST,11
      IF (ITEMP .NE. Z(KK+1)) GO TO 11811
      NOGO = 1
      MSG(2) = ITEMP
      CALL MESAGE (30,136,MSG(2))
11811 CONTINUE
      I = I + Z(I) + 1
11810 CONTINUE
 1182 DO 1183 I = 1,NLIST,11
      BUF(1) = Z(I+1)
C
C     CHECK FOR UNIQUE SET IDS ON TLOAD1,2 AND RLOAD1,2 CARDS  THEN WRIT
C
      IF (I .GE. NLIST) GO TO 1184
      IF (Z(I+1) .NE. Z(I+12)) GO TO 1184
      NOGO = 1
      MSG(2) = ITEMP
      CALL MESAGE (30,136,MSG(2))
 1184 CALL WRITE (DLT,BUF,1,0)
 1183 CONTINUE
      CALL WRITE (DLT,0,0,1)
C
C     IF DLOAD CARDS PRESENT, WRITE THE DLOAD RECORD.
C
      IF (NODLD .EQ. 0) GO TO 1200
      BUF(1) = -1
      BUF(2) = -1
      I = IDLOAD
      J = 1
 1191 N = Z(I)
      CALL WRITE (DLT,Z(I+1),N,0)
      CALL WRITE (DLT,BUF,2,0)
      I = I + N + 1
      J = J + 1
      IF (J .LE. NODLD) GO TO 1191
      CALL WRITE (DLT,0,0,1)
C
C     INITIALIZE TO LOOP THRU ALL LOAD SETS. THE REMAINDER OF THE DLT
C     WILL CONSIST OF ONE LOGICAL RECORD PER LOAD SET.
C
 1200 I = 1
C
C     WRITE FIXED SECTION OF DLT RECORD.
C
 1205 BUF(1) = Z(I  )
      BUF(2) = Z(I+2)
C
C     SAVE INFORCED MOTION FLAG ON TLOAD CARDS
C
      IF (Z(I).LT.3 .OR. Z(I).GT.4) GO TO 1206
      IEMF = Z(I+4)
      Z(I+4) = 0
 1206 CONTINUE
      CALL WRITE (DLT,BUF,2,0)
      CALL WRITE (DLT,Z(I+5),6,0)
C
C     POSITION SCRATCH FILES TO SELECTED TABLES.
C
      IDAREA = 0
      DO 1215 J = 1,3
      BUF(2*J-1) = 16777215
C                  16777215 =2**24 - 1
      K = I + J
      BUF(J+16) = Z(K+1)
      IF (BUF(J+16) .EQ. 0) GO TO 1215
      JJ = LOADS(4*J-1)
      NN = LOADS(4*J  )
      IF (NN .EQ. 0) GO TO 1212
      DO 1211 NX = 1,NN
      IF (Z(JJ) .EQ. BUF(J+16)) GO TO 1213
      JJ = JJ - 1
 1211 CONTINUE
 1212 IF (ITHRML.NE.1 .OR. J.NE.1) GO TO 1300
      IDAREA  = -1
      BUF(17) = 0
      GO TO 1215
 1300 BUF(10) = Z(I+1)
      BUF(11) = BUF(J+16)
      BUF(11) = BUF(11) + 100000000*J
      NOGO    = 1
      CALL MESAGE (30,71,BUF(10))
      BUF(J+16) = 0
      GO TO 1215
 1213 NN   = NX - 1
      FILE = SCR(J)
      IBUF = BUFX(J)
      CALL OPEN (*2001,FILE,Z(IBUF),RDREW)
      IF (NN .EQ. 0) GO TO 1215
      DO 1214 NX = 1,NN
      CALL FWDREC (*2002,FILE)
 1214 CONTINUE
 1215 CONTINUE
C
C     INITIALIZE TABLE READ.
C
      BUF(14) = BUF(17)
      BUF(15) = BUF(18)
      BUF(16) = BUF(19)
C
C     READ AN ENTRY FROM APPROPRIATE TABLE/S).
C     IF ALL ENTRIES HAVE BEEN READ, GO TO CLOSE DLT RECORD.
C
 1220 DO 1222 J = 1,3
      IF (ITHRML.NE.1 .OR. J.NE.1) GO TO 1320
      IF (IDAREA .EQ. 0) GO TO 1320
      IF (IDAREA .EQ.-2) GO TO 1221
      IDAREA  = -2
      BUF(1)  = 1
      BUF(2)  = 0
      BUF(14) = 0
 1320 IF (BUF(J+13) .EQ. 0) GO TO 1222
      FILE = SCR(J)
      J2   = 2*J
      CALL READ (*2002,*1221,FILE,BUF(J2-1),2,0,FLAG)
      GO TO 1222
 1221 BUF(2*J-1) = 16777215
      BUF(J+13)  = 0
 1222 CONTINUE
      IF (BUF(1)+BUF(3)+BUF(5) .EQ. 3*16777215) GO TO 1240
C
C     SELECT MINIMUM SIL NO(S) AND FORMAT OUTPUT.
C
      DO 1231 J = 1,6
 1231 BUF(J+10) = 0
      BUF(7) = 1
      BUF(8) = 2
      BUF(9) = 3
      IF (BUF(1) .GT. BUF(3)) GO TO 1232
C
C     1 .LE. 2--COMPARE 2 TO 3. IF 2 .GT. 3, SWITCH 2 AND 3.
C
      IF (BUF(3) .LE. BUF(5)) GO TO 1234
      K = BUF(8)
      BUF(8) = BUF(9)
      BUF(9) = K
      GO TO 1233
C
C     1 .GT. 2--SWITCH 1 AND 2 THEN COMPARE 2 AND 3. IF 2 .GT. 3, SWITCH
C
 1232 K = BUF(7)
      BUF(7) = BUF(8)
      BUF(8) = K
      IF (BUF(1) .LE. BUF(5)) GO TO 1234
      K = BUF(8)
      BUF(8) = BUF(9)
      BUF(9) = K
C
C     COMPARE 1 TO 2--IF 1 .GT. 2, SWITCH 1 AND 2.
C
 1233 K = BUF(7)
      L = BUF(8)
      IF (BUF(2*K-1) .LE. BUF(2*L-1)) GO TO 1234
      BUF(7) = L
      BUF(8) = K
C
C     PICK UP 1. SET TO READ 1.
C
 1234 K = BUF(7)
      BUF(  10) = BUF(2*K-1)
      BUF(K+10) = BUF(2*K  )
      BUF(K+13) = K
C
C     IF 1 .EQ. 2, PICK UP 2 AND SET TO READ 2.
C
      L = BUF(8)
      IF (BUF(2*K-1) .NE. BUF(2*L-1)) GO TO 1235
      BUF(L+10) = BUF(2*L)
      BUF(L+13) = L
C
C     IF 1 .EQ. 2 .EQ. 3, PICK UP 3 AND SET TO READ 3.
C
      M = BUF(9)
      IF (BUF(2*L-1) .NE. BUF(2*M-1)) GO TO 1235
      BUF(M+10) = BUF(2*M)
      BUF(M+13) = M
C
C     WRITE SIL NO., A, TAU, THETA. THEN GO TO READ ANOTHER TABLE
C     ENTRY(S).
C
 1235 IF (Z(I).LT.3 .OR. Z(I).GT.4) GO TO 1236
      BUF(13) = IEMF
 1236 CALL WRITE (DLT,BUF(10),4,0)
      GO TO 1220
C
C     CLOSE DLT RECORD,  CLOSE TABLES AND TEST FOR COMPLETION OF DLT.
C
 1240 CALL WRITE (DLT,0,0,1)
      DO 1241 J = 1,3
      IF (BUF(J+16) .NE. 0) CALL CLOSE (SCR(J),CLSREW)
 1241 CONTINUE
      I = I + 11
      IF (I .LE. NLIST) GO TO 1205
C
C     CLOSE DLT, WRITE TRAILER AND RETURN.
C
      CALL CLOSE (DLT,CLSREW)
      MCB(1) = DLT
      MCB(2) = DLT
      CALL WRTTRL (MCB)
      NODLT = 1
 1249 RETURN
C
C     FATAL FILE ERRORS
C
 2001 N= -1
      GO TO 2005
 2002 N= -2
      GO TO 2005
 2003 N= -3
 2005 CALL MESAGE (N,FILE,NAM)
      RETURN
      END
