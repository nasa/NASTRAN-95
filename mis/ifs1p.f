      SUBROUTINE IFS1P (*,*,*)
C
      LOGICAL         ABORT,BADDAT,BADFOR,IAX,LHARM,SLOT,IFPDCO
      INTEGER         M(100),KLOTDF(5),B1,BARDF2,BARDF5,BARDF6,BARDF7,
     1                BARDF8,HBDYNM(2,7),HBDYIX(7),THRU,BLK,BCDC,BCDR,
     2                BCDS,E(40)
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /XMSSG / UFM,UWM,UIM,SFM
      COMMON /SYSTEM/ KSYSTM(80)
      COMMON /BLANK / E
      COMMON /IFPDTA/ ID,N,K,KX,KY,I(100),RM(100),MF(100),M1(100),
     1                M1F(100),KN,BADDAT,BADFOR,NOPEN,NPARAM,IAX,NAX,
     2                IAXF,NAXF,LHARM,KNT,SLOTDF(5),GC(7),LL(6)
      COMMON /CIFS1P/ B1,BARDF2,BARDF5,BARDF6,BARDF7,BARDF8,KM,SLOT,
     1                IDRDL
      EQUIVALENCE     (KSYSTM(2),NOUT),(KSYSTM(3),ABORT),(M(1),RM(1)),
     1                (SLOTDF(1) ,KLOTDF(1))
      DATA   HBDYNM / 4HPOIN , 4HT
     1              , 4HLINE , 4H
     2              , 4HREV  , 4H
     3              , 4HAREA , 4H3
     4              , 4HAREA , 4H4
     5              , 4HELCY , 4HL
     6              , 4HFTUB , 4HE   /
      DATA   HBDYIX / 1,2,2,3,4,2,2  /
      DATA   THRU   / 4HTHRU         /
      DATA   BLK    , BCDC,BCDR,BCDS /  1H ,1HC,1HR,1HS/
      DATA   IT1,IT2, IT3 / 2HT1, 2HT2, 2HT3           /
C
      IF (K .GT. 100) GO TO 81
      GO TO (     5,   5,   5,  40, 500, 600, 700, 800, 900,1000,
     1         1111,   5,   5,1400,1400,1600,   5,1800,1800,2000,
     2         2000,2200,2200,2400,2500,2600,2500,   5,2900,2920,
     3          318,   5,2980,3011,3020,3020,3012,2980,3013,3020,
     4         3014,3015,3016,3210,3220,3255,3260,3281,3282,3283,
     5            5,3360,3360,3360,3360,3360,3460,3460,3460,3460,
     6         3540,3540,3580,3600,3620,3623,3674,3697,3620,3623,
     7         3675,3698,3620,3800,3676,3699,3860,3880,   5,   5,
     8         2500,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     9            5,   5,   5,   5,   5,   5,   5,   5,   5,   5), K
   81 IF (KX .GT. 100) GO TO 82
      GO TO (     5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     2         3017,   5,   5,   5,1250,   5,1270,1280,1290,1290,
     3            5,   5,   5,   5,  40,1360,1370,   5,   5,   5,
     4            5,1420,   5,   5,   5,   5,   5,   5,   5,   5,
     5            5,   5,   5,   5,   5,   5,   5,1580,   5,   5,
     6            5,   5,   5,   5,   5,1660,   5,   5,   5,   5,
     7            5,   5,   5,   5,   5,   5,   5,   5, 100, 200,
     8          300,   5,   5,   5,   5,   5,   5,   5,   5,1900,
     9            5,   5,   5,   5,   5,   5,   5,   5,   5,   5), KX
   82 IF (KY .GT. 100)  GO TO 83
      GO TO (     5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,1400,   5,   5,   5,   5,   5,
     2            5,   5,4100,4200,4300,4400,4500,4600,4700,4800,
     3         4900,5000,5050,3900,4000,5100,3950,4050,   5,5150,
     4         5200,   5,5250,   5,   5,   5,   5,   5,3460,3018,
     5            5,   5,   5,   5,   5,1600,5240,5245,3460,3019,
     6            5,   5,   5,   5,   5,   5,   5,5300,   5,   5,
     7            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     8            5,   5,   5,   5,   5,   5,   5,   5,5175,   5,
     9         6101,6201,6301,6401,   5,   5,   5,   5,7501,7601), KY
   83 KZ = KY - 100
      IF (KZ .GT. 59) GO TO 5
      GO TO (     5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,4060,4070,4080,4090,4060,4080,
     2            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     3         1111,   5,   5,   5,   5,   5,3900,   5,   5,1260,
     4         1270,1230,1235,1240,   5,   5,   5,   5,   5,   5,
     5            5,   5,   5,   5,   5,3280,3360,3460,7700     ), KZ
    5 CALL PAGE2 (2)
      WRITE  (NOUT,6) SFM
    6 FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS1P.')
      ABORT  = .TRUE.
      RETURN 1
    7 BADFOR = .TRUE.
      RETURN 1
    8 BADDAT = .TRUE.
      RETURN 1
    3 DO 4 L = 1,N
    4 I(L) = M(L)
    2 RETURN
    9 RETURN 3
C
C*****         4-SEQGP,135-SEQEP    ************************************
C
   40 DO 45 L = 1,7,2
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0) GO TO 45
      IF (M(L).LE.0 .OR.  M(L+1).LE.0) GO TO 8
      N = N + 2
      I(N-1) = M(L  )
      I(N  ) = M(L+1)
      IF (N .LE. 2) GO TO 45
      DO 43 L1 = 4,N,2
      IF (I(N-1).EQ.I(L1-3) .OR. I(N).EQ.I(L1-2)) GO TO 8
   43 CONTINUE
   45 CONTINUE
      IF (N) 8,8,2
C
C*****         179-BAROR       *****************************************
C
  100 IF (B1 .EQ. 0) GO TO 8
      B1 = 0
      IF (M(2).EQ.0 .AND. M(5).EQ.0 .AND. M(6).EQ.0 .AND. M(7).EQ.0
     1   .AND. M(8).EQ.0) GO TO 8
      IF (M(2).LT.0 .OR. M(8).LT.0 .OR. M(8).GT.2) GO TO 8
      IF (MF(8) .NE. 0) GO TO 110
      IF (MF(5).EQ.1 .AND. MF(6).NE.0 .AND. MF(7).NE.0) GO TO 8
      IF (MF(5).EQ.1 .AND. MF(6).EQ.0 .AND. MF(7).EQ.0) M(8) = 2
      IF (MF(5).EQ.2 .OR.  MF(6).EQ.2 .OR.  MF(7).EQ.2) M(8) = 1
  110 BARDF2 = M(2)
      BARDF5 = M(5)
      BARDF6 = M(6)
      BARDF7 = M(7)
      BARDF8 = M(8)
      RETURN 2
C
C*****         180-CBAR        *****************************************
C
  200 IF (MF(2)  .NE. 0) GO TO 201
      IF (BARDF2 .EQ. 0) GO TO 203
      M(2) = BARDF2
      GO TO 201
  203 M(2) = M(1)
  201 CONTINUE
      IF (MF(5) .EQ. 0) M(5) = BARDF5
      IF (MF(8) .EQ. 0) M(8) = BARDF8
      IF (MF(5).GE.3 .OR.  MF(6).GE.3 .OR. MF(7).GE.3) GO TO 8
      IF (M(8).EQ.0 .AND. (MF(5).EQ.2 .OR. MF(6).EQ.2 .OR. MF(7).EQ.2))
     1    M(8) = 1
      IF (M(8).EQ.0 .AND. MF(5).EQ.1 .AND. MF(6)+MF(7).EQ.0) M(8) = 2
      IF (M(8).LE.0 .OR.  M(8).GT.2) GO TO 8
      IF (M(8)  .EQ. 2) GO TO 205
      IF (MF(6) .EQ. 0) M(6) = BARDF6
      IF (MF(7) .EQ. 0) M(7) = BARDF7
  205 CONTINUE
      IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(3).LE.0 .OR. M(4).LE.0)
     1    GO TO 8
      IF (M(8).EQ.1 .AND. (MF(5).NE.2 .AND. MF(5).NE.0 .OR.
     1         M(5).EQ.0 .AND. M(6).EQ.0 .AND. M(7).EQ.0)) GO TO 8
      IF ((M(8).EQ.2 .OR. M(8).EQ.3) .AND. (MF(5).NE.1 .AND.MF(5).NE.0
     1    .OR. M(5).LE.0 .OR. M(6).NE.0 .OR. M(7).NE.0)) GO TO 8
      IF (IFPDCO(M(9))) GO TO 8
      IF (M(9) .GT. 65432) GO TO 8
      IF (IFPDCO(M(10))) GO TO 8
      IF (M(10) .GT. 65432) GO TO 8
      IF (M(3).EQ.M(4) .OR. M(3).EQ.M(5) .AND. M(8).EQ.2) GO TO 8
      IF (M(8).EQ.2 .AND. M(4).EQ.M(5)) GO TO 8
      N = 16
      GO TO 3
C
C*****         181-PBAR        *****************************************
C
  300 N = 19
      IF (RM(4).LT.0. .OR. RM(5).LT.0. .OR. RM(4)*RM(5).LT.RM(19)**2)
     1    GO TO 8
      GO TO 2903
C
C*****         31-PVISC        *****************************************
C
  310 DO 315 L = 1,5,4
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0) GO TO 315
      IF (M(L) .LE. 0) GO TO 8
      N = N + 3
      I(N-2) = M(L  )
      I(N-1) = M(L+1)
      I(N  ) = M(L+2)
      IF (E(KL) .LT. 0) GO TO 315
      IF (M(L) .GT. E(KL)) GO TO 314
      E(KL) = -M(L)
      GO TO 315
  314 E(KL) = M(L)
  315 CONTINUE
      IF (N) 8,8,2
  318 KL = 33
      GO TO 310
C
C*****         5-CORD1R        *****************************************
C
  500 L50 = 1
  510 DO 519 L = 1,5,4
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0 .AND.
     1    M(L+3).EQ.0) GO TO 519
      IF (M(L).LE.0 .OR. M(L+1).LE.0 .OR. M(L+2).LE.0 .OR. M(L+3).LE.0)
     1    GO TO 8
      IF (M(L+1).EQ.M(L+2) .OR. M(L+1).EQ.M(L+3) .OR. M(L+3).EQ.M(L+2))
     1    GO TO 8
      N = N + 6
      IF (N.GT.6 .AND. M(L).EQ.M(L-4)) GO TO 8
      I(N-5) = M(L  )
      I(N-4) = L50
      I(N-3) = 1
      I(N-2) = M(L+1)
      I(N-1) = M(L+2)
      I(N  ) = M(L+3)
  519 CONTINUE
      IF (N) 8,8,2
C
C*****         6-CORD1C        *****************************************
C
  600 L50 = 2
      GO TO 510
C
C*****         7-CORD1S        *****************************************
C
  700 L50 = 3
      GO TO 510
C
C*****         8-CORD2R        *****************************************
C
  800 I(2) = 1
  810 I(1) = M(1)
      IF (M(1).LE.0 .OR. M(2).LT.0) GO TO 8
      IF (M(3).EQ.M(6) .AND. M(4).EQ.M( 7) .AND. M(5).EQ.M( 8)) GO TO 8
      IF (M(3).EQ.M(9) .AND. M(4).EQ.M(10) .AND. M(5).EQ.M(11)) GO TO 8
      IF (M(6).EQ.M(9) .AND. M(7).EQ.M(10) .AND. M(8).EQ.M(11)) GO TO 8
      I(3) = 2
      DO 813 L = 2,11
  813 I(L+2) = M(L)
      N = 13
      GO TO 2
C
C*****         9-CORD2C        *****************************************
C
  900 I(2) = 2
      GO TO 810
C
C*****         10-CORD2S       *****************************************
C
 1000 I(2) = 3
      GO TO 810
C
C*****   11-PLOTEL,   331-CFFREE   *************************************
C
 1100 DO 1110 L = 1,5,4
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0) GO TO 1110
      IF (M(L).LE.0 .OR.  M(L+1).LE.0 .OR.  M(L+2).LE.0) GO TO 8
      IF (M(L+1) .EQ. M(L+2)) GO TO 8
      N = N + 3
      I(N-2) = M(L  )
      I(N-1) = M(L+1)
      I(N  ) = M(L+2)
      IF (E(KL) .LT. 0) GO TO 1110
      IF (M(L) .GT. E(KL)) GO TO 1107
      E(KL) = -M(L)
      GO TO 1110
 1107 E(KL) = M(L)
 1110 CONTINUE
      IF (N) 8,8,2
 1111 KL = 10
      GO TO 1100
C
C*********       342-CFTUBE    *****************************************
C
 1230 N = 4
      IF (M(1).LE.0  .OR. M(3).LE.0 .OR. M(4).LE.0) GO TO 8
      IF (MF(2) .EQ. 0) M(2) = M(1)
      IF (M(2).LE.3 .OR. M(3).EQ.M(4)) GO TO 8
      GO TO 3
C
C*********       343-PFTUBE    ****************************************
C
 1235 N = 5
      IF (M(1)  .LE. 0) GO TO 8
      IF (RM(2).LE.0. .OR. RM(3).LT.0. .OR. RM(4).LE.0.) GO TO 8
      IF (RM(5) .EQ. 0.) RM(5) = RM(4)
      IF (RM(5) .LT. 0.) GO TO 8
      GO TO 3
C
C*********       344-NFTUBE    *****************************************
C
 1240 N = 5
      IF (MF(2).NE.1 .OR. M(1).LE.0) GO TO 8
      IF (MF(2).NE.1 .OR. M(2).LE.0) GO TO 8
      IF (MF(3).NE.1 .OR. M(3).LE.0) GO TO 8
      IF (M(2) .EQ. M(3)) GO TO 8
      IF (MF(4).NE.0 .AND. MF(4).NE.2) GO TO 8
      IF (MF(5).EQ.1 .AND.  M(5).LT.0) GO TO 8
      IF (MF(5) .GT. 2) GO TO 7
      GO TO 3
C
C***********       125-FREQ1      **************************************
C
 1250 IF (M(1).LE.0 .OR. RM(2).LT.0. .OR. RM(3).LE.0. .OR. M(4).LE.0)
     1    GO TO 8
      N = 4
      GO TO 3
C
C*****             340-NOLIN5         **********************************
C
 1260 IF (KM .NE. 0) GO TO 1262
      KM  = 1
      KN  = 1
      NMO = 8
      IF (MF(1).NE.1 .OR.  M(1).LE.0 ) BADDAT =.TRUE.
      IF (MF(2).NE.2 .OR. RM(2).LE.0.) BADDAT =.TRUE.
      IF (MF(3).NE.2 .OR. RM(3).LE.0.) BADDAT =.TRUE.
      IF (MF(4).NE.2 .OR. RM(4).LE.0.) BADDAT =.TRUE.
      IF (MF(5).EQ.1 .AND. M(5).LT.0 ) BADDAT =.TRUE.
      IF (MF(6).EQ.1 .AND. M(6).LT.0 ) BADDAT =.TRUE.
      IF (MF(7).EQ.1 .AND. M(7).LT.0 ) BADDAT =.TRUE.
      IF (MF(8).EQ.1 .AND. M(8).LT.0 ) BADDAT =.TRUE.
      IF (MF(5).EQ.2 .AND.RM(5).LT.0.) BADDAT =.TRUE.
      IF (MF(6).EQ.2 .AND.RM(6).LT.0.) BADDAT =.TRUE.
      IF (MF(7).EQ.2 .AND.RM(7).LT.0.) BADDAT =.TRUE.
      IF (MF(8).EQ.2 .AND.RM(8).LT.0.) BADDAT =.TRUE.
      N = 8
      DO 1261 L = 1,8
 1261 I(L) = M(L)
      GO TO 1265
 1262 N = 8
      NMO = NMO + 8
      DO 1263 L = 1,8
      I(L) = M(L)
      IF (MF(L) .EQ. 0) GO TO 1263
      IF (MF(L).NE.1 .OR. M(L).LE.0) BADDAT =.TRUE.
 1263 CONTINUE
 1265 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
      KM = 0
      KN = 0
      IF (NMO .EQ. 16) GO TO 9
      IF (NMO .GT. 16) BADDAT =.TRUE.
      DO 1266 L = 1,8
      N = N + 1
      I(N) = 0
 1266 CONTINUE
      GO TO 9
C
C*****         127-NOLIN1,341-NOLIN6    ********************************
C
 1270 IF (MF(8)) 8,1282,8
 1272 IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(3).LT.0 .OR. M(5).LE.0 .OR.
     1    M(6).LT.0) GO TO 8
      IF (M(3) .GT. 6) GO TO 8
      IF ((M(6).GT.6 .AND. M(6).LT.10) .OR. M(6).GT.16) GO TO 8
      N = 8
      GO TO 3
C
C*****         128-NOLIN2        ***************************************
C
 1280 IF (M(8).LT.0 .OR. MF(8).NE.1 .AND. MF(8).NE.0) GO TO 8
      IF ((M(8).GT.6 .AND. M(8).LT.10) .OR. M(8).GT.16) GO TO 8
 1282 IF (MF(7).NE.1 .OR. M(7).LE.0) GO TO 8
      GO TO 1272
C
C*****         129-NOLIN3,130-NOLIN4        ****************************
C
 1290 IF (MF(8).NE.0 .OR. MF(7).NE.2 .AND. MF(7).NE.0) GO TO 8
      GO TO 1272
C
C*****         136-TF         ******************************************
C
 1360 IF (KM .NE. 0) GO TO 1363
      NMO = 5
      ID  = M(1)
      IF (ID.LE.0 .OR. M(2).LE.0 .OR. M(3).LT.0) GO TO 1427
      IF (MF(1).NE.1 .OR. MF(2).NE.1 .OR. MF(3).GT.1) BADFOR =.TRUE.
      IF ((MF(4).NE.2 .AND. MF(4).NE.0) .OR. (MF(5).NE.2 .AND.
     1    MF(5).NE.0) .OR. (MF(6).NE.2 .AND. MF(6).NE.0)) BADFOR =.TRUE.
      N = 6
 1361 DO 1362 L = 1,N
 1362 I(L) = M(L)
      GO TO 1428
 1363 IF (M(1).LE.0 .OR. M(2).LT.0) GO TO 1427
      IF (MF(1).NE.1 .OR. MF(2).GT.1) BADFOR =.TRUE.
      IF ((MF(3).NE.2 .AND. MF(3).NE.0) .OR. (MF(4).NE.2 .AND.
     1    MF(4).NE.0) .OR. (MF(5).NE.2 .AND. MF(5).NE.0)) BADFOR =.TRUE.
      N = 5
      GO TO 1361
C
C*****         137-TIC        ******************************************
C
 1370 IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(3).LT.0 .OR. M(3).GT.6)
     1    GO TO 8
      N = 5
      GO TO 3
C
C*****         14-SUPORT,15-OMIT,215-ASET         **********************
C
 1400 L = 1
 1402 IF (M(L).EQ.0 .AND. M(L+1).EQ.0) GO TO 1409
      IF (M(L) .LE. 0) GO TO 8
      IF (IFPDCO(M(L+1))) GO TO 8
      IZ = 6
      IF (M(L+1) .EQ. 0) IZ = 1
      DO 1407 L2 = 1,IZ
      IF (IZ.NE.1 .AND. LL(L2).EQ.0) GO TO 1407
      N = N + 2
      I(N-1) = M(L  )
      I(N  ) = LL(L2)
      IF (N .LE. 2) GO TO 1407
      DO 1408 L1 = 4,N,2
      IF (I(N-1).EQ.I(L1-3) .AND. I(N).EQ.I(L1-2)) GO TO 8
 1408 CONTINUE
 1407 CONTINUE
 1409 L = L + 2
      IF (L .LE. 7) GO TO 1402
      IF (N) 8,8,2
C
C*****         142-TSTEP        ****************************************
C
 1420 IF (MF(5).NE.0 .OR. MF(6).NE.0 .OR. MF(7).NE.0 .OR. MF(8).NE.0)
     1    GO TO 7
      IF (KM .NE. 0) GO TO 1422
      NMO = 3
      ID  = M(1)
      IF (ID.LE.0 .OR. MF(1).NE.1) GO TO 1427
      N   = 1
      I(N) = M(1)
      GO TO 1425
 1422 IF (MF(1) .NE. 0) GO TO 1427
 1425 IF (MF(2).NE.1 .OR. MF(4).NE.1 .OR. MF(3).NE.2) GO TO 1427
      IF (M(4).LE.0 .OR. RM(3).LE.0. .OR. M(2).LT.M(4)) GO TO 1427
      N = N + 3
      I(N-2) = M(2)
      I(N-1) = M(3)
      I(N  ) = M(4)
      GO TO 1428
 1427 BADDAT =.TRUE.
 1428 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 1429
      KM = 0
      KN = 0
      IF (NMO .LE. 0) GO TO 9
      DO 1426 L = 1,NMO
      N = N + 1
 1426 I(N) =-1
      GO TO 9
 1429 KM = 1
      KN = 1
      GO TO 9
C
C*****         158-EIGP        *****************************************
C
 1580 IF (M(1) .LE. 0) GO TO 8
      DO 1585 L = 2,5,3
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0) GO TO 1585
      IF (M(L+2) .LE. 0) GO TO 8
      N = N + 4
      I(N-3) = M(  1)
      I(N-2) = M(  L)
      I(N-1) = M(L+1)
      I(N  ) = M(L+2)
 1585 CONTINUE
      IF (N) 8,8,2
C
C*****         16-SPC , 256-SPCD ***********************************
C
 1600 IF (M(1) .LE. 0) GO TO 8
      L = 2
 1601 IF (M(L).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0) GO TO 1609
      IF (M(L).LE.0 .OR. M(L+1).LT.0) GO TO 8
      IF (IFPDCO(M(L+1))) GO TO 8
      N = N + 4
      IF (N.GT.4 .AND. M(L).EQ.M(L-3) .AND. M(L+1).EQ.M(L-2)) GO TO 8
      I(N-3) = M(1  )
      I(N-2) = M(L  )
      I(N-1) = M(L+1)
      I(N  ) = M(L+2)
 1609 L = L + 3
      IF (L .EQ. 5) GO TO 1601
      IF (N) 8,8,2
C
C***********       166-FREQ2      **************************************
C
 1660 IF (RM(2)) 8,8,1250
C
C*******       18-FORCE,19-MOMENT   **************************
C
 1800 IF (M(2)) 8,8,1900
C
C***************        190-RFORCE    *****************************
C
 1900 IF (MF(3).NE.0 .AND. MF(3).NE.1) GO TO 8
      IF (M(1).LE.0 .OR. M(2).LT.0 .OR. M(3).LT.0) GO TO 8
      IF (M(5).NE.0 .OR. M(6).NE.0 .OR. M(7).NE.0) GO TO 1905
      IF (M(4) .NE. 0) GO TO 8
      RM(5) = 1.0
 1905 N = 7
CWKBDB 2/95 SPR94015
C      IF (K .NE. 190) GO TO 3
C      IF (M(8) .EQ. 0) M(8) = 1
C      IF (M(8) .LT.0 .OR. M(8).GT.2) GO TO 8
C      N = 8
CWKBDE 2/95 SPR94015
      GO TO 3
C
C*****         20-FORCE1,21-MOMENT1   **********************************
C
 2000 IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(4).LE.0 .OR. M(5).LE.0)
     1    GO TO 8
      IF (M(4) .EQ. M(5)) GO TO 8
      N = 5
      GO TO 3
C
C*****         22-FORCE2,23-MOMENT2   **********************************
C
 2200 IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(4).LE.0) GO TO 8
      IF (M(5).LE.0 .OR. M(6).LE.0 .OR. M(7).LE.0) GO TO 8
      IF (M(4).EQ.M(5) .OR. M(6).EQ.M(7) .OR.  M(4).EQ.M(6) .AND.
     1    M(5).EQ.M(7) .OR. M(4).EQ.M(7) .AND. M(5).EQ.M(6)) GO TO 8
      N = 7
      GO TO 3
C
C*****         24-PLOAD        *****************************************
C
 2400 IF (M(1).LE.0 .OR. M(3).LE.0 .OR. M(4).LE.0 .OR. M(5).LE.0)
     1    GO TO 8
      IF (M(6).LT.0 .OR. M(6).EQ.0 .AND. MF(6).NE.0) GO TO 8
      DO 2404 L  = 4,6
      DO 2403 L1 = L,6
      IF (M(L-1) .EQ. M(L1)) GO TO 8
 2403 CONTINUE
 2404 CONTINUE
      N = 6
      GO TO 3
C
C*****         25-SLOAD,27-TEMP,81-DEFORM    ***************************
C
 2500 IF (M(1) .LE. 0) GO TO 8
      DO 2510 L = 2,6,2
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0) GO TO 2510
      IF (M(L) .LE. 0) GO TO 8
      N = N + 3
      I(N-2) = M(1  )
      I(N-1) = M(L  )
      I(N  ) = M(L+1)
      IF (N .LE. 3) GO TO 2510
      DO 2502 L1 = 6,N,3
      IF (I(N-1) .EQ. I(L1-4)) GO TO 8
 2502 CONTINUE
 2510 CONTINUE
      IF (N) 8,8,2
C
C*****         26-GRAV         *****************************************
C
 2600 IF (M(1).LE.0 .OR. M(2).LT.0) GO TO 8
      IF (M(4).NE.0 .OR. M(5).NE.0 .OR. M(6).NE.0) GO TO 2605
      IF (M(3) .NE. 0) GO TO 8
      RM(4) = 1.0
 2605 N = 6
      GO TO 3
C
C*****         29-PROD         *****************************************
C
 2900 N = 6
 2903 IF (M(2) .LE. 0) GO TO 8
 2906 IF (M(1) .LE. 0) GO TO 8
      GO TO 3
C
C*****         30-PTUBE        *****************************************
C
 2920 N = 5
      IF (RM(3).LE.0.0 .OR. RM(4).LT.0.0 .OR. RM(4).GT.0.5*RM(3))
     1    GO TO 8
      IF (RM(4) .EQ. 0.0) RM(4) = 0.5*RM(3)
      GO TO 2903
C
C*****         33-PTRIA1,38-PQUAD1    **********************************
C
 2980 IF (M(2).LT.0 .OR.  M(4).LT.0 .OR.  M(6).LT.0) GO TO 8
      IF (M(2).EQ.0 .AND. M(4).EQ.0 .AND. M(6).EQ.0) GO TO 8
      DO 2986 L = 2,6,2
      IF (M(L).EQ.0 .AND. M(L+1).NE.0) GO TO 8
 2986 CONTINUE
      N = 10
      GO TO 2906
C
C*****      34-PTRIA2,37-PTRMEM,39-PQUAD2,41-PQDMEM  *******************
C*****      42-PSHEAR,43-PTWIST,121-PTORDRG,250-PQDMEM1    *************
C*****      260-PQDMEM2
C
 3000 DO 3010 L = 1,5,4
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0) GO TO 3010
      IF (M(L).LE.0 .OR.  M(L+1).LE.0) GO TO 8
      IF (RM(L+2) .LE. 0.0) GO TO 8
      N = N + 4
      I(N-3) = M(L  )
      I(N-2) = M(L+1)
      I(N-1) = M(L+2)
      I(N  ) = M(L+3)
      IF (E(KL) .LT. 0) GO TO 3010
      IF (M(L) .GT. E(KL)) GO TO 3004
      E(KL) = -M(L)
      GO TO 3010
 3004 E(KL) = M(L)
 3010 CONTINUE
      IF (N) 8,8,2
 3011 KL = 30
      GO TO 3000
 3012 KL = 31
      GO TO 3000
 3013 KL = 27
      GO TO 3000
 3014 KL = 24
      GO TO 3000
 3015 KL = 28
      GO TO 3000
 3016 KL = 32
      GO TO 3000
 3017 KL = 29
      GO TO 3000
 3018 KL = 25
      GO TO 3000
 3019 KL = 26
      GO TO 3000
C
C*****         35-PTRBSC,36-PTRPLT,40-PQDPLT     ***********************
C
 3020 IF (M(2).LT.0 .OR. M(4).LT.0 .OR. M(2).EQ.0 .AND. M(4).EQ.0)
     1    GO TO 8
      DO 3026 L = 2,4,2
      IF (M(L).EQ.0 .AND. M(L+1).NE.0) GO TO 8
 3026 CONTINUE
      N = 8
      GO TO 2906
C
C*****         44-PMASS,45-PDAMP    ***********************************
C
 3200 DO 3206 L = 1,7,2
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0) GO TO 3206
      IF (M(L) .LE. 0) GO TO 8
      N = N + 2
      I(N-1) = M(L  )
      I(N  ) = M(L+1)
      IF (E(KL) .LT. 0) GO TO 3206
      IF (M(L) .GT. E(KL)) GO TO 3204
      E(KL) = -M(L)
      GO TO 3206
 3204 E(KL) = M(L)
 3206 CONTINUE
      IF (N) 8,8,2
 3210 KL = 23
      GO TO 3200
 3220 KL = 21
      GO TO 3200
C
C*****         46-PELAS             ************************************
C
 3240 DO 3250 L = 1,5,4
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0) GO TO 3250
      IF (M(L) .LE. 0) GO TO 8
      N = N + 4
      I(N-3) = M(L  )
      I(N-2) = M(L+1)
      I(N-1) = M(L+2)
      I(N  ) = M(L+3)
      IF (E(KL) .LT. 0) GO TO 3250
      IF (M(L) .GT. E(KL)) GO TO 3244
      E(KL) = -M(L)
      GO TO 3250
 3244 E(KL) = M(L)
 3250 CONTINUE
      IF (N) 8,8,2
 3255 KL = 22
      GO TO 3240
C
C*****         47-CONROD       *****************************************
C
 3260 IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(3).LE.0 .OR. M(4).LE.0)
     1    GO TO 8
      IF (M(2) .EQ. M(3)) GO TO 8
      N = 8
      GO TO 3
C
C*****         48-CROD,49-CTUBE,50-CVISC,356-CPSE2    *****************
C
 3280 DO 3289 L = 1,5,4
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0 .AND.
     1    M(L+3).EQ.0) GO TO 3289
      IF (M(L).LE.0 .OR. M(L+2).LE.0 .OR. M(L+3).LE.0) GO TO 8
      IF (MF(L+1) .EQ. 0) M(L+1) = M(L)
      IF (M(L+1).LE.0 .OR. M(L+2).EQ.M(L+3)) GO TO 8
      N = N + 4
      I(N-3) = M(L  )
      I(N-2) = M(L+1)
      I(N-1) = M(L+2)
      I(N  ) = M(L+3)
      IF (E(KL) .LT. 0) GO TO 3289
      IF (M(L) .GT. E(KL)) GO TO 3287
      E(KL) = -M(L)
      GO TO 3289
 3287 E(KL) = M(L)
 3289 CONTINUE
      IF (N) 8,8,2
 3281 KL = 1
      GO TO 3280
 3282 KL = 2
      GO TO 3280
 3283 KL = 3
      GO TO 3280
C
C*****       52-CTRIA1,53-CTRIA2,54-CTRBSC,55-CTRPLT,56-CTRMEM    ****
C            357-CPSE3
C
 3360 IF (M(3).LE.0 .OR. M(4).LE.0 .OR. M(5).LE.0) GO TO 8
      IF (M(3).EQ.M(4) .OR. M(4).EQ.M(5) .OR. M(3).EQ.M(5)) GO TO 8
      N = 6
      IF (K .EQ. 357) N = 5
 3370 IF (MF(2) .EQ. 0) M(2) = M(1)
      GO TO 2903
C
C*****       57-CQUAD1,58-CQUAD2,59-CQDPLT,60-CQDMEM,249-CQDMEM1    ****
C*****       259-CQDMEM2,358-CPSE4
C
 3460 IF (M(3).LE.0 .OR. M(4).LE.0 .OR. M(5).LE.0 .OR. M(6).LE.0)
     1    GO TO 8
      IF (M(3).EQ.M(4) .OR. M(4).EQ.M(5) .OR. M(5).EQ.M(6) .OR.
     1    M(3).EQ.M(5) .OR. M(4).EQ.M(6) .OR. M(3).EQ.M(6)) GO TO 8
      N = 7
      IF (K .EQ. 358) N = 6
      GO TO 3370
C
C*****         61-CSHEAR,62-CTWIST    **********************************
C
 3540 IF (M(3).LE.0 .OR. M(4).LE.0 .OR. M(5).LE.0 .OR. M(6).LE.0)
     1    GO TO 8
      IF (M(3).EQ.M(4) .OR. M(4).EQ.M(5) .OR. M(5).EQ.M(6) .OR.
     *    M(3).EQ.M(5) .OR. M(4).EQ.M(6) .OR. M(3).EQ.M(6)) GO TO 8
      N = 6
      GO TO 3370
C
C*****         63-CONM1        *****************************************
C
 3580 IF (M(1).LT.0 .OR. M(2).LE.0 .OR. M(3).LT.0) GO TO 8
      N = 24
      GO TO 3
C
C*****         64-CONM2        *****************************************
C
 3600 IF (M(1).LT. 0 .OR. M(2).LE.0) GO TO 8
      DO 3612 L = 1,7
 3612 I(L) = M(L)
      DO 3615 L = 8,13
 3615 I(L) = M(L+1)
      N = 13
      GO TO 2
C
C*****         65-CMASS1,69-CDAMP1,73-CELAS1,70-CDAMP2,66-CMASS2    ****
C
 3620 IF (MF(2) .EQ. 0) M(2) = M(1)
      IF (M(2)  .LE. 0) GO TO 8
 3623 N = 6
 3626 IF (M(1) .LE. 0) GO TO 8
      IF (M(3).LT.0 .OR.  M(4).LT.0 .OR. M(5).LT.0 .OR. M(6).LT.0)
     1    GO TO 8
      IF (M(4).GT.6 .OR.  M(6).GT.6 .OR. M(3).EQ.0 .AND. M(5).EQ.0)
     1    GO TO 8
      IF (M(3).EQ.0 .AND. M(4).NE.0 .OR. M(5).EQ.0 .AND. M(6).NE.0)
     1    GO TO 8
      IF (M(3).EQ.M(5) .AND. M(4).EQ.M(6)) GO TO 8
      ICELL = M(4)
      M(4)  = M(5)
      M(5)  = ICELL
      GO TO 3
C
C*****         67-CMASS3,75-CELAS3,71-CDAMP3    ************************
C
 3660 DO 3669 L = 1,5,4
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0 .AND.
     1    M(L+3).EQ.0) GO TO 3669
      IF (M(L).LE.0 .OR. M(L+2).LT.0 .OR. M(L+3).LT.0) GO TO 8
      IF (MF(L+1) .EQ. 0) M(L+1) = M(L)
      IF (M(L+1).LE.0 .OR. M(L+2).EQ.M(L+3)) GO TO 8
      N = N + 4
      I(N-3) = M(L  )
      I(N-2) = M(L+1)
      I(N-1) = M(L+2)
      I(N  ) = M(L+3)
      IF (E(KL) .LT. 0) GO TO 3669
      IF (M(L) .GT. E(KL)) GO TO 3667
      E(KL) = -M(L)
      GO TO 3669
 3667 E(KL) = M(L)
 3669 CONTINUE
      IF (N) 8,8,2
 3674 KL = 4
      GO TO 3660
 3675 KL = 5
      GO TO 3660
 3676 KL = 6
      GO TO 3660
C
C*****         68-CMASS4,76-CELAS4,72-CDAMP4    ************************
C
 3680 DO 3689 L = 1,5,4
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0 .AND.
     1    M(L+3).EQ.0) GO TO 3689
      IF (M(L).LE.0 .OR. M(L+2).LT.0 .OR. M(L+3).LT.0) GO TO 8
      IF (M(L+2) .EQ. M(L+3)) GO TO 8
      N = N + 4
      I(N-3) = M(L  )
      I(N-2) = M(L+1)
      I(N-1) = M(L+2)
      I(N  ) = M(L+3)
      IF (E(KL) .LT. 0) GO TO 3689
      IF (M(L) .GT. E(KL)) GO TO 3687
      E(KL) = -M(L)
      GO TO 3689
 3687 E(KL) = M(L)
 3689 CONTINUE
      IF (N) 8,8,2
 3697 KL = 7
      GO TO 3680
 3698 KL = 8
      GO TO 3680
 3699 KL = 9
      GO TO 3680
C
C*****         74-CELAS2       *****************************************
C
 3800 N = 8
      GO TO 3626
C
C*****         77-MAT1         *****************************************
C
 3860 IF (M(1).LE.0 .OR. (RM(2).EQ.0 .AND. RM(3).EQ.0)) GO TO 8
      IF ((RM(2).LT.0. .OR. RM(3).LT.0.) .AND. KSYSTM(78).GE.0) GO TO 8
      N = 12
      IF (M(12) .LT. 0) GO TO 8
      L = 3
      IF (MF(2).EQ.0 .OR. RM(2).EQ.0.) L = L - 1
      IF (MF(3).EQ.0 .OR. RM(3).EQ.0.) L = L - 1
      IF (MF(4).EQ.0 .OR. RM(4).EQ.0.) L = L - 1
      IF (L .GE. 2) GO TO 3865
      CALL PAGE2 (3)
      WRITE  (NOUT,3862) UWM,M(1)
 3862 FORMAT (A25,' 2251, TWO OF THE E, G AND NU ON MAT1 CARD ',I8,
     1       ' ARE ZEROS OR BLANKS.', /5X,
     2        'POTENTIAL ERROR MAY OCCUR LATER')
 3865 IF (MF(2).EQ.2 .AND. MF(3).EQ.2 .AND. MF(4).EQ.2) GO TO 3
      IF (MF(2) .EQ. 0) RM(2) = 2.0*RM(3)*(1.0+RM(4))
      IF (MF(3) .EQ. 0) RM(3) = RM(2)/(2.0*(1.0+RM(4)))
      IF (MF(4) .EQ. 0) RM(4) = RM(2)/(2.0*RM(3)) - 1.0
      IF (RM(4).GE.-1.0 .AND. RM(4).LE.0.5) GO TO 3
      CALL PAGE2 (2)
      WRITE  (NOUT,3870) UWM,M(1),RM(4)
 3870 FORMAT (A25,' 2251, PHYSICALLY UNREALISTIC VALUE FOR NU ON MAT1 ',
     1       'CARD ',I8,'.  VALUE = ',1P,E16.4)
      GO TO 3
C
C*****         78-MAT2         *****************************************
C
 3880 N = 17
      IF (M(17) .LT. 0) GO TO 8
      IF (M(1)) 8,8,3
C
C*****     234-MAT4,   337-MATF   **************************************
C
 3900 IF (M(1) .LE. 0) GO TO 8
      IF (RM(2) .LE. 0.0) GO TO 8
      IF (RM(3).LE.0.0 .AND. MF(3).EQ.2) GO TO 8
      N = 3
      GO TO 3
C
C*****         237-MATT4           *************************************
C
 3950 IF (M(1) .LE. 0) GO TO 8
      IF (M(2) .LT. 0) GO TO 8
      N = 2
      GO TO 3
C
C*****         235-MAT5            *************************************
C
 4000 IF (M(1) .LE. 0) GO TO 8
      IF (RM(8).LE.0.0 .AND. MF(8).EQ.2) GO TO 8
      N = 8
      GO TO 3
C
C*****         238-MATT5           *************************************
C
 4050 IF (M(1)  .LE. 0) GO TO 8
      IF (MF(8) .NE. 0) GO TO 7
      N = 7
      GO TO 3
C
C*****  315-MATPZ1,   319-MAT6  ****************************************
C
 4060 IF (M(1) .LE. 0) GO TO 8
      N = 15
      IF (K .EQ. 319) N = 31
      GO TO 3
C
C*****    316-MATPZ2        ********************************************
C
 4070 IF (M(1) .LE. 0) GO TO 8
      N = 52
      GO TO 3
C
C*****     317-MTTPZ1,   320-MATT6   ***********************************
C
 4080 N = 15
      IF (K .EQ. 320) N = 31
      DO 4081 L = 1,N
      IF (M(L) .LT. 0) GO TO 8
 4081 CONTINUE
      IF (M(1) .EQ. 0) GO TO 8
      GO TO 3
C
C*****   318-MTTPZ2    *************************************************
C
 4090 DO 4091 L = 1,52
      IF (M(L) .LT. 0) GO TO 8
 4091 CONTINUE
      IF (M(1) .EQ. 0) GO TO 8
      N = 52
      GO TO 3
C
C*****         223-AXSLOT         **************************************
C
 4100 IF (SLOT) GO TO 8
      SLOT = .TRUE.
      IAXF = IAXF + 2
      SLOTDF(1) = RM(1)
      SLOTDF(2) = RM(2)
      IF (M(3) .LT. 0) BADDAT =.TRUE.
      KLOTDF(3) = M(3)
      SLOTDF(4) = RM(4)
      IF (M(5) .LT. 0) BADDAT =.TRUE.
      KLOTDF(5) = M(5)
      N = 5
      GO TO 3
C
C*****         224-CAXIF2         **************************************
C
 4200 IF (MF(4).NE.0 .OR. MF(5).NE.0) GO TO 7
      N = 3
 4250 IF (M(1)  .LE. 0) GO TO 8
      IF (MF(6) .EQ. 0) RM(6) = SLOTDF(1)
      IF (MF(7) .EQ. 0) RM(7) = SLOTDF(2)
      IF (MF(8) .EQ. 0) M(8)  = KLOTDF(3)
      DO 4255 L = 2,N
      IF (M(L) .LE. 0) GO TO 8
      IF (L .EQ. 2) GO TO 4255
      DO 4253 L1 = 3,L
      IF (M(L1-1) .EQ. M(L)) GO TO 8
 4253 CONTINUE
 4255 CONTINUE
C     CHECK FOR RHO .GE. 0.0
C     CHECK FOR B .GE. 0.0
C     CHECK FOR N .GE. 0
      DO 4260 L = 6,8
      L1 = L + N - 5
 4260 I(L1) = M(L)
      DO 4270 L = 1,N
 4270 I(L) = M(L)
      N = N + 3
      GO TO 2
C
C*****         225-CAXIF3         **************************************
C
 4300 IF (MF(5) .NE. 0) GO TO 7
      N = 4
      GO TO 4250
C
C*****         226-CAXIF4         **************************************
C
 4400 N = 5
      GO TO 4250
C
C*****         227-CSLOT3         **************************************
C
 4500 IF (MF(5) .NE. 0) GO TO 7
      N = 4
 4550 IF (MF(6) .EQ. 0) RM(6) = SLOTDF(1)
      IF (MF(7) .EQ. 0) RM(7) = SLOTDF(2)
      IF (MF(8) .EQ. 0) M(8)  = KLOTDF(5)
C     CHECK FOR ALL KINDS OF THINGS
      DO 4560 L = 6,8
      L1 = L + N - 5
 4560 I(L1) = M(L)
      DO 4570 L = 1,N
 4570 I(L) = M(L)
      N = N + 4
      I(N) = KLOTDF(3)
      GO TO 2
C
C*****         228-CSLOT4         **************************************
C
 4600 N = 5
      GO TO 4550
C
C*****         229-GRIDF          **************************************
C
 4700 IF (M(1) .LE. 0) GO TO 8
      IF (RM(2) .LE. 0.0) GO TO 8
      N = 3
      GO TO 3
C
C*****         230-GRIDS          **************************************
C
 4800 IF (M(1) .LE. 0) GO TO 8
      IF (M(5) .LT. 0) GO TO 8
      IF (MF(4) .EQ. 0) RM(4) = SLOTDF(4)
      N = 5
      GO TO 3
C
C*****         231-SLBDY          **************************************
C
 4900 IF (KM .NE. 0) GO TO 4905
      KM = 1
      IF (MF(1).NE.2 .AND. MF(1).NE.0) BADFOR =.TRUE.
      IF (MF(1) .EQ. 0) M(1) = KLOTDF(1)
      IF (MF(2).NE.1 .AND. MF(2).NE.0) BADFOR =.TRUE.
      IF (MF(2) .EQ. 0) M(2) = KLOTDF(5)
      IF (M(2) .LT. 0) BADDAT =.TRUE.
      I(1) = M(1)
      I(2) = M(2)
      N  = 2
      IZ = 3
      GO TO 4906
 4905 IZ = 1
 4906 DO 4908 L = IZ,8
      IF (MF(L) .EQ. 0) GO TO 4940
      IF (M(L)  .LE. 0) BADDAT =.TRUE.
      N = N + 1
      I(N) = M(L)
 4908 CONTINUE
 4910 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 4920
      KM = 0
      N  = N + 1
      I(N) =-1
      KN = 0
 4920 GO TO 9
 4940 IZ = L + 1
      DO 4950  L = IZ,8
      IF (MF(L) .NE. 0) BADFOR =.TRUE.
 4950 CONTINUE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) BADFOR =.TRUE.
      GO TO 4910
C
C*****         232-CHBDY           *************************************
C
 5000 IF (M(1) .LE. 0) GO TO 8
      I(1) = M(1)
      IF (M(2) .LT. 0) GO TO 8
      I(2) = M(2)
      DO 5010 L = 1,7
      IF (M(3).EQ.HBDYNM(1,L) .AND. M(4).EQ.HBDYNM(2,L)) GO TO 5020
 5010 CONTINUE
      GO TO 8
 5020 I(3) = L
      L1 = HBDYIX(L)
      DO 5025 L2 = 1,L1
      IF (M(L2+4).LE.0 .OR. M(L2+9).LT.0) GO TO 8
      I(L2+3) = M(L2+4)
 5025 I(L2+7) = M(L2+9)
      IF (L1 .EQ. 4) GO TO 5035
      DO 5030 L2 = L1,3
      IF (M(L2+5).NE.0 .OR. M(L2+10).NE.0) GO TO 8
      I(L2+4) = 0
 5030 I(L2+8) = 0
 5035 DO 5040 L2 = 12,14
 5040 I(L2) = M(L2+2)
      N = 15
      I(15) = M(9)
      GO TO 2
C
C*****         233-QHBDY           *************************************
C
 5050 IF (M(1) .LE. 0) GO TO 8
      I(1) = M(1)
      DO 5055 L = 1,5
      IF (M(2).EQ.HBDYNM(1,L) .AND. M(3).EQ.HBDYNM(2,L)) GO TO 5060
 5055 CONTINUE
      GO TO 8
 5060 I(2) = L
      L1 = HBDYIX(L)
      DO 5065 L2 = 1,L1
      IF (M(L2+5) .LE. 0) GO TO 8
 5065 I(L2+4) = M(L2+5)
      IF (L1 .EQ. 4) GO TO 5075
      DO 5070 L2 = L1,3
      IF (M(L2+6) .NE. 0) GO TO 8
 5070 I(L2+5) = 0
 5075 I(3) = M(4)
      IF (L.GE.3 .AND. MF(4).NE.0) GO TO 7
      IF (L.LT.3 .AND. RM(5).LE.0.0) GO TO 8
      I(4) = M(5)
      N = 8
      GO TO 2
C
C*****         236-PHBDY           *************************************
C
 5100 IF (M(1) .LE. 0) GO TO 8
      IF (M(2) .LT. 0) GO TO 8
      IF (RM(3) .LT. 0.0) GO TO 8
      IF (RM(4).LT.0.0 .OR. RM(4).GT.1.0) GO TO 8
      IF (RM(5).LT.0.0 .OR. RM(5).GT.1.0) GO TO 8
      IF (MF(5) .EQ. 0) RM(5) = RM(4)
      N = 7
      GO TO 3
C
C*****         240-QBDY2           *************************************
C
 5150 IF (M(1) .LE. 0) GO TO 8
      IF (M(2) .LE. 0) GO TO 8
      N = 6
      GO TO 3
C
C*****                  289-VIEW                 ***************
C
 5175 N = 6
      IF (M(1) .GT. 0) GO TO 3
      GO TO 8
C
C*****         241-QVECT           *************************************
C
 5200 IF (KM .NE. 0) GO TO 5215
      IF (M(1) .LE. 0) BADDAT =.TRUE.
      IF (MF(2).NE.2 .AND. MF(2).NE.0) BADFOR =.TRUE.
      I(1) = M(1)
      I(2) = M(2)
      DO 5210 L = 3,6
      IF (MF(L) .EQ. 1) GO TO 5205
      IF (MF(L).NE.2 .AND. MF(L).NE.0) BADFOR =.TRUE.
      I(L) = M(L)
      GO TO 5210
 5205 IF (M(L) .LT. 0) BADDAT =.TRUE.
      I(L) = M(L)
 5210 CONTINUE
      L = 6
      K914 = 209
      GO TO 5216
 5215 L  = 1
 5216 KM = 1
      KN = 1
      N  = 6
      L4 = L
      IF (MF(L) .NE. 1) BADFOR =.TRUE.
      IF (M(L4) .LE. 0) BADDAT =.TRUE.
 5220 IF (L .EQ. 8) GO TO 5235
      IF (MF(L)   .EQ. 3) GO TO 5225
      IF (MF(L+1) .EQ. 0) GO TO 5234
      IF (M(L4) .LE. 0) BADDAT =.TRUE.
      I(N) = M(L4)
      L  = L  + 1
      L4 = L4 + 1
      CALL WRITE (K914,I,N,0)
      GO TO 5220
 5225 IF (MF(L+1).NE.1 .OR. M(L4).NE.THRU) GO TO 5232
      IF (M(L4-1) .GE. M(L4+2)) GO TO 5232
      L1 = M(L4-1) + 1
      L2 = M(L4+2) - 1
      IF (L2 .LE. L1) GO TO 5230
 5227 L3 = L1
      I(N) = L3
      CALL WRITE (K914,I,N,0)
      L1 = L1 + 1
      IF (L1 .LE. L2) GO TO 5227
 5230 L  = L  + 1
      L4 = L4 + 2
      GO TO 5220
 5232 BADDAT =.TRUE.
      L  = L  + 1
      L4 = L4 + 2
      GO TO 5220
 5234 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) BADFOR =.TRUE.
 5235 IF (MF(L) .NE. 1) BADFOR =.TRUE.
      I(N) = M(L4)
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
      KN = 0
      KM = 0
      GO TO 9
C
C*****         257-CYJOIN       ***************************************
C
 5240 IF (KM .NE. 0) GO TO 5253
      IF (M(1).NE.1 .AND. M(1).NE.2) BADDAT =.TRUE.
      I(1) = M(1)
      IF (MF(2) .EQ. 3) GO TO 5242
      IF (MF(2) .NE. 0) BADFOR =.TRUE.
      I(2) = BLK
      L4 = 3
      GO TO 5244
 5242 IF (M(2).NE.BCDC .AND. M(2).NE.BCDR .AND. M(2).NE.BCDS .AND.
     1    M(2).NE.IT1  .AND. M(2).NE.IT2  .AND. M(2).NE.IT3)
     2    BADDAT=.TRUE.
      I(2) = M(2)
      L4 = 4
 5244 KM = 1
      I(3) = BLK
      N = 3
      L = 3
      K914 = 210
      IF (MF(L) .NE. 1) BADFOR =.TRUE.
      IF (M(L4) .LE. 0) BADDAT =.TRUE.
      GO TO 5252
C
C*****         258-CNGRNT          *************************************
C
 5245 IF (KM .NE. 0) GO TO 5253
      K914 = 208
      GO TO 5253
C
C*****         243-RADLST          *************************************
C
 5250 IF (KM .NE. 0) GO TO 5253
      IF (IDRDL .EQ. 1) BADFOR =.TRUE.
      IDRDL = 1
      K914  = 214
 5253 L  = 1
      N  = 0
 5251 KM = 1
      L4 = L
      IF (MF(L) .NE. 1) BADFOR =.TRUE.
      IF (M(L4) .LE. 0) BADDAT =.TRUE.
 5252 IF (L .GT. 8) GO TO 5260
      IF (MF(L) .EQ. 0) GO TO 5262
      IF (MF(L) .EQ. 3) GO TO 5254
      IF (M(L4) .LE. 0) BADDAT =.TRUE.
      IF (N .LT. 49) GO TO 5255
      CALL WRITE (K914,I,N,0)
      N = 0
 5255 N = N + 1
      I(N) = M(L4)
      L  = L  + 1
      L4 = L4 + 1
      GO TO 5252
 5254 IF (L .EQ. 8) GO TO 5258
      IF (MF(L+1).NE.1 .OR. M(L4).NE.THRU) GO TO 5258
      IF (M(L4-1) .GE. M(L4+2)) GO TO 5258
      L1 = M(L4-1) + 1
      L2 = M(L4+2)
 5256 L3 = L1
      IF (N .LT. 49) GO TO 5257
      CALL WRITE (K914,I,N,0)
      N = 0
 5257 N = N + 1
      I(N) = L3
      L1 = L1 + 1
      IF (L1 .LE. L2) GO TO 5256
      L  = L  + 2
      L4 = L4 + 3
      GO TO 5252
 5258 BADDAT =.TRUE.
      L  = L  + 1
      L4 = L4 + 2
      GO TO 5252
 5260 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 5266
 5261 KM = 0
      N  = N + 1
      I(N) =-1
      KN = 0
      GO TO 9
 5262 DO 5264 L2 = L,8
      IF (MF(L2) .NE. 0) BADFOR =.TRUE.
 5264 CONTINUE
      IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 5261
      BADFOR =.TRUE.
 5266 KN = 1
      GO TO 9
C
C*****         268-SET1       ******************************************
C
 5300 IF (KM .NE. 0) GO TO 5253
      IF (MF(1) .NE. 1) BADFOR =.TRUE.
      I(1) = M(1)
      N = 1
      L = 2
      K914 = 204
      GO TO 5251
C
C*****       291-CTRIM6         ****************************************
C
 6101 IF (M(3).LE.0 .OR. M(4).LE.0 .OR. M(5).LE.0 .OR. M(6).LE.0 .OR.
     1    M(7).LE.0 .OR. M(8).LE.0) GO TO 8
      IF (M(3).EQ.M(4) .OR. M(3).EQ.M(5) .OR. M(3).EQ.M(6) .OR.
     1    M(3).EQ.M(7) .OR. M(3).EQ.M(8) .OR. M(4).EQ.M(5) .OR.
     2    M(4).EQ.M(6) .OR. M(4).EQ.M(7) .OR. M(4).EQ.M(8)) GO TO  8
      IF (M(5).EQ.M(6) .OR. M(5).EQ.M(7) .OR. M(5).EQ.M(8) .OR.
     1    M(6).EQ.M(7) .OR. M(6).EQ.M(8) .OR. M(7).EQ.M(8)) GO TO 8
      DO 6102 L = 1,8
      IF (MF(L) .NE. 1) GO TO 7
 6102 CONTINUE
      IF (MF(9).NE.0 .AND. MF(9).NE.2) GO TO 7
      IF (MF(2) .EQ. 0) M(2) = M(1)
      N = 9
      GO TO 2903
C
C*****       292-PTRIM6         ****************************************
C
 6201 IF (M(2).LT.0 .OR. RM(3).LT.0.0 .OR. RM(4).LT.0.0 .OR.
     1    RM(5).LT.0.0) GO TO 8
      IF (RM(3) .EQ. 0.0)  GO TO  8
      IF (MF(1).NE.1 .AND. MF(2).NE.1) GO TO 7
      IF (MF(3) .NE. 2) GO TO 7
      DO 6202 L = 4,6
      IF (MF(L).NE.0 .AND. MF(L).NE.2) GO TO 7
 6202 CONTINUE
      N = 6
      GO TO 2906
C
C*****       293-CTRPLT1        ****************************************
C
 6301 GO TO 6101
C
C*****       294-PTRPLT1        ****************************************
C
 6401 IF (M(2).LT.0 .OR. M(6).LT.0 .OR. M(2).EQ.0 .AND. M(6).EQ.0)
     1   GO TO 8
      IF (M(2).EQ.0  .AND. M(3).NE.0 ) GO TO 8
      IF (M(6).EQ.0  .AND. M(7).NE.0 ) GO TO 8
      IF (MF(1).NE.1 .AND. MF(2).NE.1) GO TO 7
      IF (MF(6).NE.0 .AND. MF(6).NE.1) GO TO 7
      IF (MF(3) .NE. 2) GO TO 7
      IF (MF(4).NE.0 .AND. MF(4).NE.2) GO TO 7
      IF (MF(5).NE.0 .AND. MF(5).NE.2) GO TO 7
      DO 6402 L = 7,16
      IF (MF(L).NE.0 .AND. MF(L).NE.2) GO TO 7
 6402 CONTINUE
      N = 16
      GO TO 2906
C
C*****       295-CTRSHL         ****************************************
C
 7501 GO TO 6101
C
C*****       296-PTRSHL         ****************************************
C
 7601 CONTINUE
      IF (M(2).LT.0 .OR. M(6).LT.0 .OR. M(10).LT.0 .OR. M(2).EQ.0 .AND.
     1    M(6).EQ.0 .AND. M(10).EQ.0) GO TO 8
      IF (M(2).EQ.0 .AND. RM(3).NE.0.0) GO TO 8
      IF (M(6).EQ.0 .AND. RM(7).NE.0.0) GO TO 8
      IF (M(10).EQ.0 .AND. RM(11).NE.0.0) GO TO 8
      IF (RM(3).LT.0.0 .OR. RM(4).LT.0.0 .OR. RM(5).LT.0.0) GO TO 8
      IF (RM(7).LT.0.0 .OR. RM(8).LT.0.0 .OR. RM(9).LT.0.0) GO TO 8
      IF (RM(11).LT.0.0 .OR. RM(12).LT.0.0 .OR. RM(13).LT.0.0) GO TO 8
      IF (MF(10).NE.0 .AND. MF(10).NE.1) GO TO 7
      IF (MF(1) .NE. 1) GO TO 7
      IF (MF(2).NE.0 .AND. MF(2).NE.1) GO TO 7
      IF (MF(6).NE.0 .AND. MF(6).NE.1) GO TO 7
      DO 7602 L = 3,11,4
      IF (MF(L).NE.0 .AND. MF(L).NE.2) GO TO 7
      IF (MF(L+1).NE.0 .AND. MF(L+1).NE.2) GO TO 7
      IF (MF(L+2).NE.0 .AND. MF(L+2).NE.2) GO TO 7
 7602 CONTINUE
      N = 20
      GO TO 2906
C
C*********     359-PPSE       ******************************************
C
 7700 N = 5
      IF (M(1)  .LE.  0) GO TO 8
      IF (RM(2) .EQ. 0.) GO TO 8
      RM(3) = 0.0
      RM(4) = 0.0
      RM(5) = 0.0
      GO TO 3
C
      END
