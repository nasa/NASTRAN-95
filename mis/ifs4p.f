      SUBROUTINE IFS4P (*,*,*)
C
      LOGICAL         ABORT,BADDAT,BADFOR,LHARM,LFLSYM,FPHYS1,IFPDCO
      INTEGER         T1,T4,THRU,SAVE(24),NM(2),TY1,TY2,
     1                RET,BCDYES,BCDNO,BCDS,BCDA,BCDNON,BCDAXI
      REAL            Z(100)
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /MACHIN/ MACH
      COMMON /XMSSG / UFM,UWM,UIM,SFM
      COMMON /SYSTEM/ NBUF,NOUT,ABORT
      COMMON /IFPX1 / NCDS,T1(2,310)
      COMMON /IFPX3 / T4(2,314)
      COMMON /IFPDTA/ ID,N,K,KX,KY,I(100),M(100),MF(100),M1(100),
     1                M1F(100),KN,BADDAT,BADFOR,NOPEN,NPARAM,IAX,NAX,
     2                IAXF,NAXF,LHARM,KNT,SLOTDF(5),GC(7),LL(6)
      COMMON /CIFS4P/ J(20),KM,LFLSYM,FPHYS1
      EQUIVALENCE     (Z(1),M(1)),(KOUT,J(2))
      DATA    THRU  , BCDYES,BCDNO /4HTHRU,4HYES ,4HNO  /
      DATA    BCDS  , BCDA,BCDNON  /4HS   ,4HA   ,4HNONE/
      DATA    BCDAXI/ 4HAXIS/
C
      IF (K .GT. 100) GO TO 81
      GO TO (     5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     2            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     3            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     4            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     5            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     6            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     7            5,   5,   5,   5,   5,   5,   5,   5, 790, 800,
     8            5,   5,   5,   5,   5,   5,   5,   5,   5, 900,
     9          900,   5,   5,   5,   5,   5,   5, 980,   5,   5 ), K
   81 IF (KX .GT. 100) GO TO 82
      GO TO (     5,1020,   5,1040,1050,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     2            5,1220,   5,1050,   5,   5,   5,   5,   5,   5,
     3            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     4            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     5            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     6            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     7            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     8            5,   5,   5,   5,   5,   5,   5,   5,1020,   5,
     9            5,   5,   5,   5,1950,1960,   5,   5,1990,   5 ), KX
   82 IF (KY .GT. 100) GO TO 83
      GO TO (  2100,2200,2300,2400,3100,3200,3300,3400,3500,3600,
     1         3700,3800,3900,4000,   5,   5,4100,4200,4300,4400,
     2         4500,4600,   5,   5,   5,   5,   5,   5,   5,   5,
     3            5,   5,   5,   5,   5,   5,   5,   5,1990,   5,
     4            5,1990,   5,   5,   5,   5,   5,   5,   5,   5,
     5            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     6            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     7            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     8            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     9            5,   5,   5,   5,6501,6601,   5,   5,   5,   5 ), KY
   83 KZ = K - 300
      IF (KZ .GT. 39) GO TO 5
      GO TO (     5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     2         8000,9000,9100,9200,9300,9000,9400,9500,   5,   5,
     3            5,   5,4300,4400,4100,4200,   5,   5,   5      ), KZ
    5 CALL PAGE2 (2)
      WRITE  (NOUT,6) SFM
    6 FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS4P.')
      ABORT =.TRUE.
      RETURN 1
    7 BADFOR =.TRUE.
      RETURN 1
    8 BADDAT =.TRUE.
      RETURN 1
    3 DO 4 L = 1,N
    4 I(L) = M(L)
    2 RETURN
    9 RETURN 3
C
C******              79-CTRIARG,80-CTRAPRG             ****************
C
  790 I1 = 4
      GO TO 791
  800 I1 = 5
  791 IF (M(1).LE.0 .OR. M(I1+2).LE.0) GO TO 8
      DO 793 L = 2,I1
      IF (M(L) .LE. 0) GO TO 8
      IF (L .EQ. 2) GO TO 793
      DO 792 L1 = L,I1
      IF (M(L-1) .EQ. M(L1)) GO TO 8
  792 CONTINUE
  793 CONTINUE
      N = I1 + 2
      GO TO 3
C
C*******       MATS1,MATT1        **************************************
C
  900 DO 902 L = 1,11
      IF (M(L) .LT. 0) GO TO 8
  902 I(L) = M(L)
      N = 11
      GO TO 2
C
C*******       TEMPD              **************************************
C
  980 DO 986 L = 1,7,2
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0) GO TO 986
      IF (M(L) .LE. 0) GO TO 8
      N = N + 2
      I(N-1) = M(L  )
      I(N  ) = M(L+1)
      IF (N .LE. 2) GO TO 986
      DO 987 L1 = 4,N,2
      IF (I(N-1) .EQ. I(L1-3)) GO TO 8
  987 CONTINUE
  986 CONTINUE
      IF (N) 8,8,2
C
C**************    MATT2,189-MATT3     *********************************
C
 1020 DO 1022 L = 1,16
      IF (M(L) .LT. 0) GO TO 8
 1022 I(L) = M(L)
      IF (M(1) .EQ. 0) GO TO 8
      N = 16
      GO TO 2
C
C******           104-CTORDRG           ************************
C
 1040 IF (M(1).LE.0 .OR. M(3).LE.0 .OR. M(4).LE.0 .OR. M(3).EQ.M(4) .OR.
     1    Z(5).LT.0.0 .OR. Z(5).GT.180.0 .OR. Z(6).LT.0.0 .OR.
     2    Z(6).GT.180.0) GO TO 8
      IF (MF(2) .EQ. 0) M(2) = M(1)
      IF (M(2)  .LE. 0) GO TO 8
      N = 7
      GO TO 3
C
C*******       SPOINT,124-EPOINT    ************************************
C
 1050 IF (MF(2) .EQ. 3) GO TO 1056
      DO 1055 L = 1,8
      IF (MF(L).NE.1 .AND. MF(L).NE.0) GO TO 7
      IF (M(L)) 8,1055,1052
 1052 IF (M(L) .GT. 999999) GO TO 8
      N = N + 1
      I(N) = M(L)
      IF (N .LE. 1) GO TO 1055
      DO 1054 L1 = 2,N
      IF (I(N) .EQ. I(L1-1)) GO TO 8
 1054 CONTINUE
 1055 CONTINUE
      IF (N) 8,8,2
 1056 IF (M(2) .NE. THRU) GO TO 8
      IF (MF(1).NE.1 .OR. MF(3).NE.1) GO TO 7
      K2078 = 208
      IF (K .EQ. 124) K2078 = 207
      L1 = 1
      L2 = 4
      DO 1058 L = L2,8
      IF (MF(L) .NE. 0) GO TO 7
 1058 CONTINUE
      IF (M(L2) .GT. 9999999) GO TO 8
      II = M(L1) - 1
      L2 = M(L2) - M(L1)
      IF (II.LT.0 .OR. L2.LE.0) GO TO 8
      L1 = 1
      DO 1059 L = 1,L2
      II = II + 1
      CALL WRITE (K2078,II,1,0)
 1059 CONTINUE
      I(1) = II + 1
      N = 1
      GO TO 2
C
C*******         122-MAT3        *****************************
C
 1220 IF (M(1).LE.0 .OR. Z(2).LT.0. .OR. Z(3).LT.0. .OR. Z(4).LT.0. .OR.
     1    Z(9).LT.0. .OR. Z(10).LT.0. .OR. Z(11).LT.0.) GO TO 8
      IF (ABS(Z(5)).LE.1. .AND. ABS(Z(6)).LE.1. .AND. ABS(Z(7)).LE.1.)
     1    GO TO 1222
      CALL PAGE2 (2)
      WRITE  (NOUT,1221) UWM,T1(1,K),T1(2,K),KNT
 1221 FORMAT (A25,' 301, BULK DATA CARD ',2A4,' CONTAINS INCONSISTENT',
     1       ' DATA.',10X,'SORTED CARD COUNT =',I7)
 1222 N = 16
      GO TO 3
C
C
C*******       195-RANDPS       ****************************************
C
 1950 IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(3).LT.M(2) .OR. M(6).LT.0)
     1    GO TO 8
      IF (M(2).EQ.M(3) .AND. Z(5).NE.0.0) GO TO 8
      N = 6
      IF (KOUT .LE. 2) GO TO 1955
      IF (M(1) .EQ. J(KOUT)) GO TO 3
      IF (KOUT .EQ. J(   1)) GO TO 8
 1955 KOUT = KOUT + 1
      J(KOUT) = M(1)
      GO TO 3
C
C*******       196-RANDT1       ****************************************
C
 1960 IF (KOUT .LE. 2) GO TO 8
      DO 1961 IN = 3,KOUT
      IF (M(1) .EQ. J(IN)) GO TO 1962
 1961 CONTINUE
      GO TO 8
 1962 IF (M(1).LE.0 .OR. M(2).LE.0 .OR. Z(3).LT.0.0 .OR. Z(4).LE.Z(3))
     1    GO TO 8
      N = 4
      GO TO 3
C
C*****         199-PLOAD2,239-QBDY1,242-QVOL   *************************
C
 1990 IF (KM   .NE. 0) GO TO 1991
      IF (MF(1).NE.1 .OR. MF(2).NE.2 .AND. MF(2).NE.0) GO TO 7
      IF (M(1) .LE. 0) GO TO 8
      L = 3
      ISID = M(1)
      IQVL = M(2)
      GO TO 1992
 1991 L = 1
 1992 IF (MF(8) .EQ. 3) GO TO 7
      NTOT  = 0
      K2078 = 209
 1993 IF (M(L)  .EQ. 0) GO TO 1998
      IF (M(L)  .LT. 0) GO TO 8
      IF (MF(L) .EQ. 3) GO TO 7
      IF (MF(L+1).EQ.3) GO TO 1995
      IF (MF(L).NE.1 .AND. MF(L).NE.0) GO TO 7
      N = N + 3
      I(N-2) = ISID
      I(N-1) = IQVL
      I(N) = M(L)
      L = L + 1
      IF (N .LT. 48) GO TO 1997
      CALL WRITE (K2078,I,N,0)
      NTOT = NTOT + N
      N = 0
      GO TO 1997
 1995 IF (M(L+1) .NE. THRU) GO TO 8
      IF (MF(L+3).NE.1 .AND. MF(L+3).NE.0) GO TO 7
      L1 = M(L  ) - 1
      L2 = M(L+3) - L1
      IF (L2.LE.1 .OR. L1.LT.0) GO TO 8
      DO 1996 II = 1,L2
      N = N + 3
      I(N-2) = ISID
      I(N-1) = IQVL
      I(N) = II + L1
      IF (N .LT. 48) GO TO 1996
      CALL WRITE (K2078,I,N,0)
      NTOT = NTOT + N
      N = 0
 1996 CONTINUE
      L = L + 4
 1997 IF (L .LE. 8) GO TO 1993
 1998 T4(2,K) = T4(2,K) + NTOT
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 1999
      KM = 0
      GO TO 2
 1999 KM = 1
      GO TO 2
C
C**********          201-TEMPP1          *******************************
C
 2100 IF (KM .NE. 0) GO TO 2120
      NN = 6
      N  = 6
      ID = M(1)
      IF (MF(5) .EQ. -32767) GO TO 2106
      IF (MF(7).NE.0 .OR. MF(8).NE.0) BADFOR =.TRUE.
 2101 DO 2102 L = 3,6
      IF (MF(L).EQ.0 .OR. MF(L).EQ.2) GO TO 2102
      BADFOR =.TRUE.
 2102 CONTINUE
 2103 CONTINUE
      IF (MF(1).NE.1 .OR. MF(2).NE.1) BADFOR =.TRUE.
      IF (M(1).LE.0 .OR. M(2).LE.0) BADDAT =.TRUE.
      DO 2105 L = 1,N
      I(L) = M(L)
 2105 SAVE(L) = M(L)
      GO TO 2110
 2106 DO 2107 L = 3,4
      IF (MF(L).EQ.0 .OR. MF(L).EQ.2) GO TO 2107
      BADFOR =.TRUE.
 2107 CONTINUE
      GO TO 2103
 2110 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 2115
      KM = 0
      KN = 0
      GO TO 9
 2115 KN = 1
      KM = KM + 1
      GO TO 9
 2120 IF (MF(2).EQ.3 .OR. MF(5).EQ.3) GO TO 2150
      N  = 0
      DO 2140 L = 1,8
      IF (MF(L) .EQ. 0) GO TO 2140
      IF (MF(L) .EQ. 1) GO TO 2125
      IF (MF(L) .EQ.-32767) GO TO 2145
      BADFOR =.TRUE.
      GO TO 2140
 2125 IF (M(L) .GT. 0) GO TO 2130
      BADDAT =.TRUE.
      GO TO 2140
 2130 SAVE(2) = M(L)
      CALL WRITE (209,SAVE,NN,0)
 2140 CONTINUE
 2145 CONTINUE
      GO TO 2110
 2150 N = 0
      IF (MF(7).EQ.0 .AND. MF(8).EQ.0) GO TO 2155
      IF (MF(4).EQ.0 .AND. MF(5).EQ.-32767) GO TO 2155
      BADFOR =.TRUE.
      GO TO 2110
 2155 L1 =-1
      DO 2180 L = 1,4,3
      IF (MF(L).EQ.0 .AND. MF(L+1).EQ.0 .AND. MF(L+2).EQ.0) GO TO 2180
      IF (MF(L).EQ.1 .AND. MF(L+1).EQ.3 .AND. MF(L+2).EQ.1) GO TO 2160
      IF (MF(L+1) .EQ. -32767) GO TO 2185
      BADFOR =.TRUE.
      GO TO 2180
 2160 L1 = L1 + 1
      L2 = L1 + L
      IF (M(L2).GT.0 .AND. M(L2+1).EQ.THRU .AND. M(L2+3).GT.M(L2))
     1    GO TO 2165
      BADDAT =.TRUE.
      GO TO 2180
 2165 L3 = M(L2  )
      L4 = M(L2+3)
      DO 2170 L5 = L3,L4
      SAVE(2) = L5
      CALL WRITE (209,SAVE,NN,0)
 2170 CONTINUE
 2180 CONTINUE
 2185 CONTINUE
      GO TO 2110
C
C*******       202-TEMPP2         **************************************
C
 2200 IF (KM .NE. 0) GO TO 2120
      NN = 8
      N  = 8
      ID = M(1)
      IF (MF(5) .EQ. -32767) GO TO 2106
      IF (MF(7).NE.0 .AND. MF(7).NE.2 .OR.
     1    MF(8).NE.0 .AND. MF(8).NE.2) BADFOR =.TRUE.
      GO TO 2101
C
C*******       203-TEMPP3         **************************************
C
 2300 IF (KM .NE. 0) GO TO 2330
      NN = 24
      N  = 0
      ID = M(1)
      L1 = 1
      IF (MF(1).NE.1 .OR. MF(2).NE.1) BADFOR =.TRUE.
      DO 2305 L = 3,8
      IF (MF(L).EQ.0 .OR. MF(L).EQ.2) GO TO 2305
      IF (MF(L) .EQ. -32767) GO TO 2302
      BADFOR =.TRUE.
      GO TO 2305
 2302 DO 2303 L5 = L,8
      M(L5) = 0
 2303 CONTINUE
      MF(7) = 0
      MF(8) = 0
 2305 CONTINUE
      IF (M(1).LE.0 .OR. M(2).LE.0) BADDAT =.TRUE.
      IF (Z(3) .GE. Z(5)) BADDAT =.TRUE.
      ZZ = Z(5)
      IF (MF(7).EQ.0 .AND. MF(8).EQ.0) GO TO 2310
      IF (ZZ .GE. Z(7)) BADDAT =.TRUE.
 2310 ZZ = Z(7)
      DO 2320 L = 1,8
      I(L) = M(L)
 2320 SAVE(L) = M(L)
 2321 L1 = L1 + 8
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 2328
      KM = 0
      KN = 0
 2322 IF (L1 .GT. NN) GO TO 2326
      DO 2325 L = L1,NN
      I(L) = 0
 2325 SAVE(L) = 0
 2326 N  = NN
      GO TO 9
 2328 KM = KM + 1
      KN = 1
      IF (KM-3) 9,2322,2322
 2330 IF (KM .GT. 2) GO TO 2120
      N  = 0
      L3 = 8*KM
      DO 2350 L = 1,7,2
      IF (MF(L).EQ.0 .AND. MF(L+1).EQ.0) GO TO 2340
      IF (MF(L) .NE. -32767) GO TO 2335
      MF(7) = 0
      MF(8) = 0
      GO TO 2340
 2335 CONTINUE
      IF (MF(L  ).NE.0 .AND. MF(L  ).NE.2  .OR.
     1    MF(L+1).NE.0 .AND. MF(L+1).NE.2) BADFOR =.TRUE.
      IF (ZZ .GE. Z(L)) BADDAT =.TRUE.
 2340 ZZ = Z(L)
      L5 = L3 + L
      I(L5) = M(L)
      SAVE(L5) = M(L)
      I(L5+1)  = M(L+1)
      SAVE(L5+1) = M(L+1)
 2350 CONTINUE
      GO TO 2321
C
C*******       204-TEMPRB         **************************************
C
 2400 IF (KM .NE. 0) GO TO 2430
      NN = 16
      N  = 0
      ID = M(1)
      L1 = 1
      IF (MF(1).NE.1 .OR. MF(2).NE.1) BADFOR =.TRUE.
      DO 2405 L = 3,8
      IF (MF(L).EQ.0 .OR. MF(L).EQ.2) GO TO 2405
      IF (MF(L) .EQ. -32767) GO TO 2402
      BADFOR =.TRUE.
      GO TO 2405
 2402 DO 2403 L5 = L,8
      M(L5) = 0
 2403 CONTINUE
 2405 CONTINUE
      IF (M(1).LE.0 .OR. M(2).LE.0) BADDAT =.TRUE.
      DO 2420 L = 1,8
      I(L) = M(L)
 2420 SAVE(L) = M(L)
 2421 L1 = L1 + 8
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 2428
      KM = 0
      KN = 0
 2422 IF (L1 .GT. NN) GO TO 2426
      DO 2425 L = L1,NN
      I(L) = 0
 2425 SAVE(L) = 0
 2426 N  = NN
      GO TO 9
 2428 KM = KM + 1
      KN = 1
      IF (KM-2) 9,2422,2422
 2430 IF (KM .GT. 1) GO TO 2120
      N  = 0
      DO 2450 L = 1,8
      IF (MF(L) .EQ. -32767) GO TO 2455
      IF (MF(L).NE.0 .AND. MF(L).NE.2) BADFOR =.TRUE.
      I(L+8) = M(L)
      SAVE(L+8) = M(L)
 2450 CONTINUE
      GO TO 2421
 2455 DO 2460 L = 5,8
      I(L+8) = 0
      SAVE(L+8) = 0
 2460 CONTINUE
      GO TO 2421
C
C    TEMPG IS MODELLED AFTER TEMPP3
C    TEMPP4 IS MODELLED AFTER TEMPP1,EXCEPT THAT TEMPP1 HAS ONE LESS C
C
C
C*******      295-TEMPG     ********************************************
C
 6501 CONTINUE
      GO TO 2300
C
C*******      296-TEMPP4    ********************************************
C
 6601 CONTINUE
      IF (KM .NE. 0) GO TO 6630
      NN = 14
      N  = 0
      ID = M(1)
      L1 = 1
      IF (MF(1).NE.1 .OR. MF(2).NE.1) BADFOR =.TRUE.
      DO 6605 L = 3,8
      IF (MF(L).EQ.0 .OR. MF(L).EQ.2) GO TO 6605
      IF (MF(L) .EQ. -32767) GO TO 6602
      BADFOR =.TRUE.
      GO TO 6605
 6602 DO 6603 L5 = L,8
      M(L5) = 0
 6603 CONTINUE
 6605 CONTINUE
      IF (M(1).LE.0 .OR. M(2).LE.0) BADDAT =.TRUE.
      DO 6620 L = 1,8
      I(L) = M(L)
 6620 SAVE(L) = M(L)
 6621 L1 = L1 + 8
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 6628
      KM = 0
      KN = 0
 6622 IF (L1 .GT. NN) GO TO 6626
      DO 6625 L = L1,NN
      I(L) = 0
 6625 SAVE(L) = 0
 6626 N  = NN
      GO TO 9
 6628 KM = KM + 1
      KN = 1
      IF (KM-2) 9,6622,6622
 6630 IF (KM .GT. 1) GO TO 2120
      N  = 0
      L3 = 8*KM
      IF (MF(7).NE.0 .AND. MF(8).NE.0) BADFOR =.TRUE.
      DO 6640 L = 1,6
      IF (MF(L).EQ.0 .OR. MF(L).EQ.2) GO TO 6640
      IF (MF(L) .EQ. -32767) GO TO 6632
      BADFOR =.TRUE.
      GO TO 6640
 6632 DO 6633 L6 = L,6
      M(L6) = 0
 6633 CONTINUE
 6640 CONTINUE
      DO 6650 L = 1,6
      L5 = L3 + L
      I(L5) = M(L)
      SAVE(L5) = M(L)
 6650 CONTINUE
      GO TO 6621
C
C*******       205-GRIDB          **************************************
C
 3100 ASSIGN 3105 TO RET
      GO TO 3890
 3105 IF (M(1).LE.0 .OR. M(6).LT.0 .OR. M(8).LE.0) GO TO 8
      IF (IFPDCO(M(7))) GO TO 8
      N    = 5
      I(1) = M(1)
      I(2) = M(4)
      I(3) = M(6)
      I(4) = M(7)
      I(5) = M(8)
      GO TO 2
C
C*******       206-FSLIST         **************************************
C
 3200 IF (KM .NE. 0) GO TO 3270
      ASSIGN 3205 TO RET
      GO TO 3890
 3205 IF (MF(1).EQ.0 .OR. MF(1).EQ.2) GO TO 3207
 3206 BADFOR =.TRUE.
      GO TO 3250
 3207 IF (MF(1).EQ.0 .OR. (MF(1).EQ.2 .AND. Z(1).GT.0.0)) GO TO 3209
 3208 BADDAT =.TRUE.
      GO TO 3250
 3209 IF (MF(1) .EQ. 0) M(1) = 1
      I(1) = M(1)
      N  = 1
      L1 = 2
      L2 = 0
      IF (MF(2) .NE. 3) GO TO 3220
      IF (M(2)  .NE. BCDAXI) GO TO 3208
      N  = N + 1
      I(N) = 0
      L1 = L1 + 1
      L2 = 1
 3220 DO 3225 L = L1,8
      L3 = L + L2
      IF (MF(L) .EQ. 3) GO TO 3230
      IF (MF(L) .EQ. 0) GO TO 3235
      IF (MF(L) .NE. 1) GO TO 3206
      IF (M(L3) .LE. 0) GO TO 3208
      N  = N + 1
      I(N) = M(L3)
 3225 CONTINUE
 3227 IF (N) 3208,3208,3250
 3230 IF (M(L3) .NE. BCDAXI) GO TO 3208
      N  = N + 1
      I(N) = 0
 3235 IF (L .EQ. 8) GO TO 3245
      L  = L + 1
      DO 3240 L2 = L,8
      IF (MF(L2) .NE. 0) GO TO 3206
 3240 CONTINUE
 3245 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 3208
      GO TO 3227
 3250 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 3255
      KM = 0
      KN = 0
      N  = N + 1
      I(N) =-1
      GO TO 9
 3255 KN = 1
      KM = KM + 1
      GO TO 9
 3270 L1 = 1
      L2 = 0
      GO TO 3220
C
C*******       207-RINGFL         **************************************
C
 3300 ASSIGN 3310 TO RET
      GO TO 3890
 3310 DO 3350 L = 1,5,4
      IF (M(L).EQ.0.AND.M(L+1).EQ.0.AND.M(L+2).EQ.0.AND.M(L+3).EQ.0)
     1    GO TO 3350
      IF (M(L).LE.0 .OR. Z(L+1).LE.0.0) GO TO 8
      N = N + 4
      IF (N.GT.4 .AND. M(L).EQ.M(L-4)) GO TO 8
      IF (M(L) .LE. 99999) GO TO 3320
      CALL PAGE2 (2)
      WRITE (NOUT,3512) UFM
      GO TO 8
 3320 I(N-3) = M(L  )
      I(N-2) = M(L+1)
      I(N-1) = M(L+2)
      I(N  ) = M(L+3)
 3350 CONTINUE
      IF (N) 8,8,2
C
C*******       208-PRESPT         **************************************
C
 3400 ASSIGN 3410 TO RET
      GO TO 3890
 3410 IF (M(1).LE.0) GO TO 8
      DO 3450 L = 3,7,2
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0) GO TO 3450
      IF (M(L) .LE. 0) GO TO 8
      N      = N + 3
      I(N-2) = M(1)
      I(N-1) = M(L)
      I(N  ) = M(L+1)
 3450 CONTINUE
      IF (N) 8,8,2
C
C*******       209-CFLUID2        **************************************
C
 3500 KFL = 2
 3505 ASSIGN 3510 TO RET
      GO TO 3890
 3510 IF (M(1) .LE. 0) GO TO 8
      IF (M(1) .LE. 99999) GO TO 3513
      CALL PAGE2 (2)
      WRITE  (NOUT,3512) UFM
 3512 FORMAT (A23,' 5004, FLUID POINT ID ON CFLUID OR RINGFL CARD ',
     1       'EXCEEDS 999999 LIMIT')
      GO TO 8
 3513 DO 3520 L = 2,KFL
      IF (M(L) .LE. 0) GO TO 8
      IF (L  .EQ. KFL) GO TO 3520
      L2 = L + 1
      DO 3515 L1 = L2,KFL
      IF (M(L) .EQ. M(L1)) GO TO 8
 3515 CONTINUE
 3520 CONTINUE
      I(1) = M(1)
      N    = KFL + 3
      IF (MF(6) .EQ. 0) M(6) = 1
      IF (MF(7) .EQ. 0) M(7) = 1
      I(KFL+2) = M(6)
      I(KFL+3) = M(7)
      DO 3530 L = 1,KFL
 3530 I(L+1) = M(L+1)
      GO TO 2
C
C*******       210-CFLUID3        **************************************
C
 3600 KFL = 3
      GO TO 3505
C
C*******       211-CFLUID4        **************************************
C
 3700 KFL = 4
      GO TO 3505
C
C*******       212-AXIF           **************************************
C
 3800 N = 0
      IF (KM   .NE. 0) GO TO 3850
      IF (IAXF .GT. 0) GO TO 3840
      IAXF = IAXF + 1
      IF (MF(1).NE.1 .OR. MF(2).NE.0 .AND. MF(2).NE.2 .OR. MF(3).NE.0
     1   .AND. MF(3).NE.2 .OR. MF(4).NE.0 .AND. MF(4).NE.2 .OR.
     2   MF(5).NE.3) BADFOR =.TRUE.
      IF (MF(7).NE.0 .OR. MF(8).NE.0 .OR. MF(6).NE.0 .AND. MF(6).NE.3)
     1    BADFOR =.TRUE.
      IF (MF(3) .EQ. 0) M(3) = 1
      IF (M(5).NE.BCDYES .AND. M(5).NE.BCDNO) BADDAT =.TRUE.
      IF (M(5) .EQ. BCDYES) M(5) = 1
      IF (M(5) .EQ. BCDNO ) M(5) = 0
      CALL WRITE (215,M,5,0)
      IF (MF(6) .EQ. 3) GO TO 3820
      IF (M1(1).NE.0 .OR. M1(2).NE.0) BADDAT =.TRUE.
      GO TO 3875
 3820 IF (M(7) .NE. BCDNON) BADDAT =.TRUE.
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) BADDAT =.TRUE.
      GO TO 3875
 3840 CALL PAGE2 (2)
      WRITE  (NOUT,3841) UFM
 3841 FORMAT (A23,' 4121, ONLY ONE (1) AXIF CARD ALLOWED IN BULK DATA.')
      ABORT =.TRUE.
      GO TO 3875
 3850 IF (MF(2) .EQ. 3) GO TO 3860
      DO 3855 L = 1,8
      IF (MF(L) .EQ. 0) GO TO 3855
      IF (MF(L) .EQ. 1) GO TO 3853
      BADFOR =.TRUE.
      N = 0
      GO TO 3856
 3853 IF (M(L) .LE. NAXF) BADDAT =.TRUE.
      N = N + 1
      NAXF = M(L)
      I(N) = M(L)
 3855 CONTINUE
      IF (N .LE. 0) BADDAT =.TRUE.
 3856 GO TO 3875
 3860 IF (MF(4) .EQ. 3) GO TO 3870
      L1 = 1
      L2 = 1
      IF (MF(1).EQ.1 .AND. MF(3).EQ.1) GO TO 3862
 3861 BADFOR =.TRUE.
      GO TO 3875
 3862 DO 3863 L = 4,8
      IF (MF(L) .NE. 0) GO TO 3861
 3863 CONTINUE
      IF (M(1).LT.M(4) .AND. M(1).GE.0) GO TO 3864
      BADDAT =.TRUE.
      GO TO 3875
 3864 IF (M(1) .LE. NAXF) BADDAT =.TRUE.
      IF (M(1) .GT. 0) GO TO 3866
      CALL WRITE (215,0,1,0)
      GO TO 3867
 3866 L2 = M(1)
 3867 L3 = M(4)
      DO 3868 L = L2,L3,L1
      CALL WRITE (215,L,1,0)
 3868 CONTINUE
      NAXF = L3
      GO TO 3875
 3870 L1 = M(7)
      L2 = L1
      IF (MF(1).EQ.1 .AND. MF(3).EQ.1 .AND. MF(5).EQ.1 .AND. MF(6).EQ.0
     1   .AND. MF(7).EQ.0 .AND. MF(8).EQ.0) GO TO 3872
      GO TO 3861
 3872 IF (M(1).LT.M(4) .AND. M(7).GT.0 .AND. M(7).LE.M(4) .AND.
     1    MOD(M(4)-M(1) , M(7)).EQ.0) GO TO 3874
      BADDAT =.TRUE.
      GO TO 3875
 3874 GO TO 3864
 3875 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 3878
      KM = 0
      KN = 0
      IF (NAXF .LT. 100) GO TO 3877
      CALL PAGE2 (2)
      WRITE  (NOUT,3876) UFM,NAXF
 3876 FORMAT (A23,' 4125, MAXIMUM ALLOWABLE HARMONIC ID IS 99.  DATA ',
     1       'CONTAINS MAXIMUM =',I20)
      ABORT =.TRUE.
 3877 CONTINUE
      N  = N + 1
      I(N) =-1
      GO TO 9
 3878 KN = 1
      KM = KM + 1
      GO TO 9
 3890 IF (IAXF .GT. 0) GO TO 3892
      IF (LHARM) CALL PAGE2 (2)
      IF (LHARM) WRITE (NOUT,3891) UFM
 3891 FORMAT (A23,' 4122, AXIF CARD REQUIRED.')
      LHARM =.FALSE.
      ABORT =.TRUE.
 3892 GO TO RET, (3105,3205,3310,3410,3510,4504,4610)
C
C*******       213-BDYLIST        **************************************
C
 3900 GO TO 3200
C
C*******       214-FREEPT         **************************************
C
 4000 GO TO 3400
C
C*******       217-CTETRA,  335-CFTETRA  *******************************
C
 4100 N = 6
 4105 DO 4110 L = 1,N
      IF (M(L) .LE. 0) GO TO 8
 4110 CONTINUE
      N1 = N - 1
      DO 4130 L = 3,N1
      L2 = L + 1
      DO 4120 L1 = L2,N
      IF (M(L) .EQ. M(L1)) GO TO 8
 4120 CONTINUE
 4130 CONTINUE
      GO TO 3
C
C*******       218-CWEDGE,  336-CFWEDGE  *******************************
C
 4200 N = 8
      GO TO 4105
C
C*******       219-CHEXA1,  333-CFHEX1   *******************************
C
 4300 IF (MF(15).NE.0 .OR. MF(16).NE.0) GO TO 7
      N = 10
      GO TO 4105
C
C*******       220-CHEXA2,  334-CFHEX2   *******************************
C
 4400 IF (MF(15).NE.0 .OR. MF(16).NE.0) GO TO 7
      N = 10
      GO TO 4105
C
C*******       221-DMIAX          **************************************
C
 4500 IF (.NOT.FPHYS1) GO TO 4501
      FPHYS1 =.FALSE.
      NM(1) = 0
      NM(2) = 0
 4501 IF (KM   .NE. 0) GO TO 4505
      IF (M(3) .EQ. 0) GO TO 4503
      IF (M(1).NE.NM(1) .OR. M(2).NE.NM(2)) GO TO 4510
      IF (MF(2).NE.1 .OR. MF(3).NE.1 .AND. MF(3).NE.0) GO TO 4511
      IF (MF(4).NE.1 .AND. MF(4).NE.0) GO TO 4511
      IF (M(3).LE.0 .OR. M(4).LT.0 .OR. M(4).GT.6) GO TO 4511
      IF (IABS(M(5)) .GT. NAXF) GO TO 4511
      IF (MF(5).NE.0 .OR. MF(6).NE.0 .OR. MF(7).NE.0 .OR. MF(8).NE.0)
     1    GO TO 4511
      IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 4511
      N  = 2
      I(2) = M(4)
      L1 = 4
      L2 = 5
      ASSIGN 4595 TO RET
      GO TO 4520
 4503 ASSIGN 4504 TO RET
      GO TO 3890
 4504 IF (MF(1).NE.3 .OR. M(1).EQ.NM(1) .AND. M(2).EQ.NM(2)) GO TO 4510
      IFO = M(4)
      TY1 = M(5)
      ITY1= 2*MOD(TY1,2)
      TY2 = M(6)
      IF (MACH .NE. 12) GO TO 45045
      IF (TY2.EQ.2.OR.TY2.EQ.4) TY2 = TY2 - 1
45045 CONTINUE
      IF (IFO.NE.1 .AND. IFO.NE.2 .AND. IFO.NE.6) GO TO 4510
      IF (TY1.LE.0 .OR. TY1.GT.4 .OR. TY2.LE.0 .OR. TY2.GT.4) GO TO 4510
      IF (TY2.EQ.1 .AND. TY1.EQ.3) GO TO 4510
      NM(1) = M(1)
      NM(2) = M(2)
      IF (MF(6).NE.0 .OR. MF(7).NE.0 .OR. MF(8).NE.0) GO TO 4511
      IF (M1F(2).NE.3 .OR. M1(3).NE.NM(1) .OR. M1(4).NE.NM(2))
     1    GO TO 4511
      N = 9
      GO TO 3
 4505 IF (M(1).LE.0 .OR. M(2).LT.0 .OR. M(2).GT.6) GO TO 4511
      IF (MF(1).NE.1 .OR. MF(2).NE.1 .AND. MF(2).NE.0) GO TO 4511
      IF (MF(4).NE.0 .AND. MF(4)+ITY1.NE.4) GO TO 4511
      IF (MF(5).NE.0 .AND. TY1.NE.3 .AND. TY1.NE.4) GO TO 4511
      IF (IABS(M(3)) .GT. NAXF) GO TO 4511
      IF (MF(3).NE.1 .AND. MF(3).NE.0) GO TO 4511
      N  = 3
      I(2) = M(2)
      L1 = 3
      L2 = 3
      ASSIGN 4506 TO RET
      GO TO 4520
 4506 I(3) = M(4)
      IF (TY1 .EQ. 1) GO TO 4508
      N = 4
      I(4) = M(5)
      IF (TY1.EQ.2 .OR. TY1.EQ.3) GO TO 4508
      N = 6
      I(5) = M(6)
      I(6) = M(7)
 4508 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 4595
      N = N + 2
      I(N-1) =-1
      I(N  ) =-1
      IF (M1(1).EQ.T1(1,K) .AND. M1(2).EQ.T1(2,K) .AND. M1(3).EQ.NM(1)
     1   .AND. M1(4).EQ.NM(2)) GO TO 4592
      N = N + 2
      I(N-1) =-1
      I(N  ) =-1
      GO TO 4592
 4510 NM(1) = M(1)
      NM(2) = M(2)
 4511 ABORT =.TRUE.
      CALL PAGE2 (2)
      WRITE  (NOUT,4512) UFM,NM(1),NM(2)
 4512 FORMAT (A23,' 4126, BAD DATA OR FORMAT OR NON-UNIQUE NAME, DMIAX',
     1       1X ,2A4)
      GO TO 4590
 4520 IF (MF(L1) .EQ. 1) GO TO 4521
      I(1) = M(L2-2)
      GO TO 4525
 4521 IF (M(L2) .LT. 0) GO TO 4522
      I(1) = 1000000*(1+M(L2)) + M(L2-2)
      GO TO 4525
 4522 I(1) = 500000*(1-M(L2)*2) + M(L2-2)
 4525 GO TO RET, (4506,4595)
 4590 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 4595
 4592 KM = 0
      KN = 0
      GO TO 2
 4595 KN = 1
      KM = KM + 1
      GO TO 2
C
C*******       222-FLSYM          **************************************
C
 4600 IF (LFLSYM) GO TO 4690
      LFLSYM =.TRUE.
      ASSIGN 4610 TO RET
      GO TO 3890
 4610 CONTINUE
      IF (MF(1).NE.1 .OR. MF(2).NE.3 .OR. MF(3).NE.3) BADFOR =.TRUE.
      DO 4615 L = 4,8
      IF (MF(L) .NE. 0) BADFOR =.TRUE.
 4615 CONTINUE
      IF (M(1).LT.2 .OR. M(2).NE.BCDS .AND. M(2).NE.BCDA .OR.
     1    M(4).NE.BCDS .AND. M(4).NE.BCDA) BADDAT =.TRUE.
      IF (MOD(M(1),2) .NE. 0) BADDAT =.TRUE.
      IF (M(2) .EQ. BCDS) M(2) = +1
      IF (M(2) .EQ. BCDA) M(2) = -1
      IF (M(4) .EQ. BCDS) M(3) = +1
      IF (M(4) .EQ. BCDA) M(3) = -1
      N = 3
      GO TO 3
 4690 CALL PAGE2 (2)
      WRITE  (NOUT,4691) UFM
 4691 FORMAT (A23,' 4123, ONLY ONE (1) FLSYM CARD ALLOWED IN BULK DATA')
      ABORT =.TRUE.
      GO TO 2
C
C*******     321-CEMLOOP     *******************************************
C
 8000 IF (M(1).LE.0 .OR. M(13).LT.0) GO TO 8
      IF (M(3) .EQ. 0) GO TO 8002
      IF (M(5) .NE. 0) GO TO 8
      DO 8001 IEM = 7,13
      IF (M(IEM) .NE. 0) GO TO 8
 8001 CONTINUE
      GO TO 8003
 8002 DX1 = Z(4) - Z(10)
      DY1 = Z(5) - Z(11)
      DZ1 = Z(6) - Z(12)
      DX2 = Z(7) - Z(10)
      DY2 = Z(8) - Z(11)
      DZ2 = Z(9) - Z(12)
      DL1 = DX1**2 + DY1**2 + DZ1**2
      DL2 = DX2**2 + DY2**2 + DZ2**2
      IF (ABS(DL1-DL2) .GT. 1.E-4) GO TO 8
      DC1 = DY1*DZ2 - DY2*DZ1
      DC2 = DX2*DZ1 - DX1*DZ2
      DC3 = DX1*DY2 - DY1*DX2
      DLC = SQRT(DC1**2 + DC2**2 + DC3**2)
      IF (DLC/SQRT(DL2) .LT. .0001) GO TO 8
 8003 N = 13
      GO TO 3
C
C*******    322-SPCFLD,   326-REMFLUX      *****************************
C
 9000 IF (M(1) .LE. 0) GO TO 8
      IF (M(2) .LT. 0) GO TO 8
      IF (M(6) .NE.-1) GO TO 9003
      DO 9002 L = 7,8
      IF (MF(L) .NE. 0) GO TO 7
 9002 CONTINUE
      N = 6
      GO TO 3
 9003 IF (MF(7) .EQ. 3) GO TO 9005
      DO 9004 L = 6,8
      IF (MF(L).NE.1 .AND. MF(L).NE.0) GO TO 7
      IF (M(L) .LT. 0) GO TO 8
      IF (M(L) .EQ. 0) GO TO 9004
      N = N + 6
      I(N-5) = M(1)
      I(N-4) = M(2)
      I(N-3) = M(3)
      I(N-2) = M(4)
      I(N-1) = M(5)
      I(N  ) = M(L)
 9004 CONTINUE
      IF (N) 8,8,2
 9005 IF (M(7) .NE. THRU) GO TO 8
      IF (MF(6).NE.1 .OR. MF(8).NE.1) GO TO 7
      L1 = 6
      L2 = 9
      II = M(L1) - 1
      L2 = M(L2) - M(L1)
      IF (II.LT.0 .OR. L2.LE.0) GO TO 8
      L1 = 1
      DO 9007 L = 1,5
 9007 I(L) = M(L)
      N = 6
      DO 9008 L = L1,L2
      I(6) = L + II
 9008 CALL WRITE (209,I,N,0)
      I(6) = II + L2 + 1
      GO TO 2
C
C*****   323-CIS2D8   **************************************************
C
 9100 IF (M( 1).LE.0 .OR. M( 2).LE.0 ) GO TO 8
      IF (M(11).LT.0 .OR. Z(12).LT.0.) GO TO 8
      IF (M(11) .EQ. 0) M(11) = 2
      IF (M(11).NE.2 .AND. M(11).NE.3) GO TO 8
      DO 9101 L = 3,10
      IF (M(L) .LE. 0) GO TO 8
 9101 CONTINUE
      DO 9102 L = 3,9
      LP1 = L + 1
      DO 9102 LLL = LP1,10
      IF (M(L) .EQ. M(LLL)) GO TO 8
 9102 CONTINUE
      N = 12
      GO TO 3
C
C*****   324-PIS2D8   **************************************************
C
 9200 IF (Z(3) .LE. 0.) GO TO 8
      IF (M(1).LE.0 .OR. M(2).LE.0) GO TO 8
      N = 3
      GO TO 3
C
C*****   325-GEMLOOP   *************************************************
C
 9300 IF (MF(1) .NE. 1) GO TO 7
      IF (MF(2).NE.2 .AND. MF(2).NE.0) GO TO 7
      IF (MF(3).NE.1 .AND. MF(3).NE.0) GO TO 7
      IF (M(1).LE.0 .OR. M(3).LT.0) GO TO 8
C
C     FOR NOW, CID MUST BE 0
C
      IF (M(3) .NE. 0) GO TO 7
      NPTS = 0
      DO 9310 L = 4,49,3
      IF (MF(L) .EQ. 3) GO TO 9320
      NPTS = NPTS + 1
      IF (MF(L  ).NE.2 .AND. MF(L  ).NE.0) GO TO 7
      IF (MF(L+1).NE.2 .AND. MF(L+1).NE.0) GO TO 7
      IF (MF(L+2).NE.2 .AND. MF(L+2).NE.0) GO TO 7
 9310 CONTINUE
      GO TO 8
 9320 IF (NPTS .LT. 2) GO TO 8
      DO 9325 LLL = L,49
 9325 M(LLL) = 0
      DO 9330 L = 1,3
 9330 I(L) = M(L)
      I(4) = NPTS
      DO 9340 L = 4,48
 9340 I(L+1) = M(L)
      N = 49
      GO TO 2
C
C*****   327-BFIELD   **************************************************
C
 9400 IF (M(1) .LT. 0) GO TO 8
      IF (M(2) .NE.-1) GO TO 9405
      DO 9402 L = 3,8
      IF (MF(L) .NE. 0) GO TO 7
 9402 CONTINUE
      N = 2
      GO TO 3
 9405 IF (MF(3) .EQ. 3) GO TO 9420
      DO 9410 L = 2,8
      IF (MF(L).NE.1 .AND. MF(L).NE.0) GO TO 7
      IF (M(L) .LT. 0) GO TO 8
      IF (M(L) .EQ. 0) GO TO 9410
      N = N + 2
      I(N-1) = M(1)
      I(N  ) = M(L)
 9410 CONTINUE
      IF (N) 8,8,2
 9420 IF (M(3) .NE. THRU) GO TO 7
      IF (MF(2).NE.1 .OR. MF(4).NE.1) GO TO 7
      L1 = 2
      L2 = 5
      II = M(L1) - 1
      L2 = M(L2) - M(L1)
      IF (II.LT.0 .OR. L2.LE.0) GO TO 8
      L1 = 1
      I(1) = M(1)
      N  = 2
      DO 9430 L = L1,L2
      I(2) = L + II
 9430 CALL WRITE (201,I,N,0)
      I(2) = II + L2 + 1
      GO TO 2
C
C*****   328-MDIPOLE     ***********************************************
C
 9500 IF (M(1).LE.0  .OR. M( 2).LT.0 ) GO TO 8
      IF (Z(9).LT.0. .OR. Z(10).LT.0.) GO TO 8
      N = 10
      GO TO 3
C
      END
