      SUBROUTINE IFS5P (*,*,*)
C
      EXTERNAL        LSHIFT,RSHIFT,ORF
      LOGICAL         ABORT,BADDAT,BADFOR,IFPDCO
      INTEGER         M(100),RET,THRU,NFDH(10),ITYPE(12),ISCAL(4),
     1                ORF,RSHIFT,LSHIFT,C,P,T1,BLANK,MET(4),MOT(3),GC
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /XMSSG / UFM,UWM,UIM,SFM
      COMMON /IFPX1 / NCDS,T1(2,310)
      COMMON /SYSTEM/ NBUF,NOUT,ABORT,JUNK(42),KDUMEL(9)
      COMMON /BITPOS/ KB(32,2)
      COMMON /IFPDTA/ ID,N,K,KX,KY,I(100),RM(100),MF(100),M1(100),
     1                M1F(100),
     2                KN,BADDAT,BADFOR,NOPEN,NPARAM,IAX,NAX,IAXF,NAXF,
     3                LHARM,KNT,SLOTDF(5),GC(7),LL(6)
      COMMON /CIFS5P/ KM,C,P,ICONT,IAERO,IPOPT
      EQUIVALENCE     (M(1),RM(1)), (BLANK,IBLANK)
      DATA    THRU  / 4HTHRU/
      DATA    BLANK / 1H    /
      DATA    IYES  , INO   /    4HYES , 4HNO   /
      DATA    MS,ML / 4HS   ,    4HL   /
      DATA    MOT   / 1HZ,  1HY, 2HZY  /
      DATA    MET   / 1HK,  2HPK,2HKE,   3HINV  /
      DATA    NMT   / 4     /
      DATA    ITYPE, ISCAL  /
     1        4HFX  ,4HFY  ,4HFZ  ,4HFXE ,4HFYE ,4HFZE ,4HMX  ,4HMY  ,
     2        4HMZ  ,4HMXE ,4HMYE ,4HMZE ,4HLE  ,4HFR  ,4HLEPR,4HFRPR/
C
      IF (K .GT. 100) GO TO 81
      GO TO (     5,   5, 100,   5,   5,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     2            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     3            5, 200,   5,   5,   5,   5,   5,   5,   5,   5,
     4            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     5          300,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     6            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     7            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     8            5,   5,   5,   5,   5,   5,   5, 400,   5,   5,
     9            5,   5,   5,   5,   5,   5,   5,   5, 500, 600 ), K
   81 IF (KX .GT. 100) GO TO 82
      GO TO (   700,   5, 800,   5,   5, 900,1000,1100,1200,1300,
     1         1400,1500,1600,1700,1800,1900,2000,2100,   5,   5,
     2            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     3            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     4            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     5            5,   5,   5,   5,   5,   5,   5,   5,2200,2300,
     6         2400,   5,2500,2600,2700,   5,2800,2900,3000,3100,
     7         3200,3300,3400,3500,3600,3700,3800,3900,   5,   5,
     8            5,   5,   5,   5,   5,4000,4100,   5,   5,   5,
     9            5,   5,4400,4500,   5,   5,   5,6000,   5,   5 ), KX
   82 IF (KY .GT. 100) GO TO 83
      GO TO (     5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     2            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     3            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     4            5,   5,   5,   5,4600,4600,   5,   5,   7,   8,
     5         5000,5100,5200,5300,5400,   5,   5,   5,   5,   5,
     6            5,   5,6400,6500,6600,6700,6800,   5,5600,5700,
     7         5800,5900,   5,   5,6100,6200,6300,6900,   5,   5,
     8            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     9            5,   5,   5,   5,   5,   5,   5,   5,   5,   5 ), KY
   83 KZ = KY-100
      IF (KZ .GT. 39) GO TO 5
      GO TO (  6400,6400,6400,6510,6520,6530,6850,7600,6400,7700,
     1         3300,3300,3300,3350,   5,   5,   5,   5,   5,   5,
     2            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     3            5,   5,   5,   5,   5,   5,   5,4700,4710      ), KZ
    5 CALL PAGE2 (2)
      WRITE  (NOUT,6) SFM
    6 FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS5P.')
      ABORT =.TRUE.
      IF (K .EQ. 0) GO TO 9999
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
C*****         3-ADUM1        ******************************************
C
  100 CONTINUE
      IDUMEL = 1
      GO TO 8100
C
C*****         32-ADUM2       ******************************************
C
  200 CONTINUE
      IDUMEL = 2
      GO TO 8100
C
C*****         51-ADUM3       ******************************************
C
  300 CONTINUE
      IDUMEL = 3
      GO TO 8100
C
C*****         88-ADUM4       ******************************************
C
  400 CONTINUE
      IDUMEL = 4
      GO TO 8100
C
C*****         99-ADUM5       ******************************************
C
  500 CONTINUE
      IDUMEL = 5
      GO TO 8100
C
C*****         100-ADUM6      ******************************************
C
  600 CONTINUE
      IDUMEL = 6
      GO TO 8100
C
C*****         101-ADUM7      ******************************************
C
  700 CONTINUE
      IDUMEL = 7
      GO TO 8100
C
C*****         103-ADUM8      ******************************************
C
  800 CONTINUE
      IDUMEL = 8
      GO TO 8100
C
C*****         106-ADUM9      ******************************************
C
  900 CONTINUE
      IDUMEL = 9
      GO TO 8100
C
C*****         107-CDUM1      ******************************************
C
 1000 CONTINUE
      IDUMEL = 1
      GO TO 8200
C
C*****         108-CDUM2      ******************************************
C
 1100 CONTINUE
      IDUMEL = 2
      GO TO 8200
C
C*****         109-CDUM3      ******************************************
C
 1200 CONTINUE
      IDUMEL = 3
      GO TO 8200
C
C*****         110-CDUM4      ******************************************
C
 1300 CONTINUE
      IDUMEL = 4
      GO TO 8200
C
C*****         111-CDUM5      ******************************************
C
 1400 CONTINUE
      IDUMEL = 5
      GO TO 8200
C
C*****         112-CDUM6      ******************************************
C
 1500 CONTINUE
      IDUMEL = 6
      GO TO 8200
C
C*****         113-CDUM7      ******************************************
C
 1600 CONTINUE
      IDUMEL = 7
      GO TO 8200
C
C*****         114-CDUM8      ******************************************
C
 1700 CONTINUE
      IDUMEL = 8
      GO TO 8200
C
C*****         115-CDUM9      ******************************************
C
 1800 CONTINUE
      IDUMEL = 9
      GO TO 8200
C
C*****         116-PDUM1      ******************************************
C
 1900 CONTINUE
      IDUMEL = 1
      GO TO 8300
C
C*****         117-PDUM2      ******************************************
C
 2000 CONTINUE
      IDUMEL = 2
      GO TO 8300
C
C*****         118-PDUM3      ******************************************
C
 2100 CONTINUE
      IDUMEL = 3
      GO TO 8300
C
C*****         159-PDUM4      ******************************************
C
 2200 CONTINUE
      IDUMEL = 4
      GO TO 8300
C
C*****         160-PDUM5      ******************************************
C
 2300 CONTINUE
      IDUMEL = 5
      GO TO 8300
C
C*****         161-PDUM6      ******************************************
C
 2400 CONTINUE
      IDUMEL = 6
      GO TO 8300
C
C*****         163-PDUM7      ******************************************
C
 2500 CONTINUE
      IDUMEL = 7
      GO TO 8300
C
C*****         164-PDUM8      ******************************************
C
 2600 CONTINUE
      IDUMEL = 8
      GO TO 8300
C
C*****         165-PDUM9      ******************************************
C
 2700 CONTINUE
      IDUMEL = 9
      GO TO 8300
C
C*****         167-CONCT1     ******************************************
C
 2800 IF (KM .EQ. 1) GO TO 2850
      NSS = 0
      IF (MF(1) .NE. 1) GO TO 7
      DO 2805 L = 2,8
      IF (MF(L).NE.3 .AND. MF(L).NE.0) GO TO 7
      IF (MF(L) .EQ. 3) NSS = NSS + 1
 2805 CONTINUE
      IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 7
      IF (M(1) .LE. 0) GO TO 8
      IF (NSS  .EQ. 1) GO TO 8
      I(1) = NSS
      I(2) = M(1)
      N  = 2
      NB = 0
      DO 2810 L = 2,8
      IF (MF(L) .EQ. 0) GO TO 2809
      N = N + 2
      NFDH(L-1) = 1
      I(N-1) = M(N-2+NB)
      I(N  ) = M(N-1+NB)
      GO TO 2810
 2809 NB = NB + 1
      NFDH(L-1) = 0
 2810 CONTINUE
      KM = 1
      GO TO 2
 2850 KM = 0
      DO 2855 L = 1,8
      IF (MF(L) .GT. 1) GO TO 7
      IF (M(L).LE.0 .AND. MF(L).EQ.1) GO TO 8
 2855 CONTINUE
      DO 2860 L = 2,8
      IF (MF(L).EQ.1 .AND. NFDH(L-1).EQ.0) GO TO 8
 2860 CONTINUE
      I(1) = M(1)
      N = 1
      DO 2870 L = 2,8
      IF (NFDH(L-1) .EQ. 0) GO TO 2870
      N = N + 1
      I(N) = M(L)
 2870 CONTINUE
      KN = 1
      KM = 1
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 2
      KN = 0
      KM = 0
      N  = N + 1
      I(N) = -1
      GO TO 2
C
C*****         168-CONCT      ******************************************
C
 2900 IF (KM .EQ. 1) GO TO 2950
      KM = 1
      DO 2905 L = 1,2
      IF (MF(L  ) .NE. 1) GO TO 7
      IF (MF(L+2) .NE. 3) GO TO 7
      IF (M(L) .LE. 0) GO TO 8
 2905 CONTINUE
      DO 2910 L = 1,6
 2910 I(L) = M(L)
      N = 6
      IF (M1(1).NE.0 .AND. M1(2).NE.0) GO TO 7
      GO TO 2
 2950 DO 2955 L = 1,8
      IF (MF(L).NE.0 .AND. MF(L).NE.1) GO TO 7
 2955 CONTINUE
      DO 2960 L = 1,8
      IF (MF(L).EQ.1 .AND. M(L).LE.0) GO TO 8
 2960 CONTINUE
      N = 0
      DO 2965 L = 1,8,2
      KDLH = MF(L) + MF(L+1)
      IF (KDLH.NE.0 .AND. KDLH.NE.2) GO TO 8
      IF (KDLH .EQ. 0) GO TO 2965
      N = N + 2
      I(N-1) = M(N-1)
      I(N  ) = M(N  )
 2965 CONTINUE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 2
      N = N + 2
      I(N-1) = -1
      I(N  ) = -1
      KM = 0
      GO TO 2
C
C*****         169-TRANS      ******************************************
C
 3000 IF (MF(1).NE.1 .OR. MF(2).NE.0) GO TO 7
      DO 3010 L = 3,11
      IF (MF(L).NE.2 .AND. MF(L).NE.0) GO TO 7
 3010 CONTINUE
      IF (M(1) .LE. 0) GO TO 8
      V11   = RM( 6) - RM(3)
      V12   = RM( 7) - RM(4)
      V13   = RM( 8) - RM(5)
      V21   = RM( 9) - RM(3)
      V22   = RM(10) - RM(4)
      V23   = RM(11) - RM(5)
      TR1   = V12*V23 - V13*V22
      TR2   = V11*V23 - V13*V21
      TR3   = V11*V22 - V12*V21
      TMAG  = SQRT(TR1**2 + TR2**2 + TR3**2)
      V1MAG = SQRT(V11**2 + V12**2 + V13**2)
      V2MAG = SQRT(V21**2 + V22**2 + V23**2)
      IF (V1MAG .EQ. 0.0) GO TO 8
      IF (V2MAG .EQ. 0.0) GO TO 8
      ANGSIN = TMAG/V1MAG/V2MAG
      IF (ANGSIN .LT. 0.087) GO TO 8
      I(1) = M(1)
      DO 3020 L = 3,11
      I(L-1) = M(L)
 3020 CONTINUE
      N = 10
      GO TO 2
C
C*****         170-RELES      ******************************************
C
 3100 IF (KM .EQ. 1) GO TO 3170
      KM = 1
      IF (MF(1) .NE. 1) GO TO 7
      IF (MF(2) .NE. 3) GO TO 7
      IF (M(1)  .LE. 0) GO TO 8
      I(1) = M(1)
      I(2) = M(2)
      I(3) = M(3)
      L1 = 3
      N  = 3
 3180 DO 3110 L = L1,8,2
      KDLH = MF(L) + MF(L+1)
      IF (KDLH.NE.0 .AND. KDLH.NE.2) GO TO 8
      IF (KDLH .EQ. 0) GO TO 3110
      N = N + 2
      I(N-1) = M(N-1)
      I(N  ) = M(N  )
 3110 CONTINUE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 2
      N = N + 2
      I(N-1) = -1
      I(N  ) = -1
      KM = 0
      GO TO 2
 3170 N  = 0
      L1 = 1
      GO TO 3180
C
C*****         171-LOADC      ******************************************
C
 3200 IF (KM .EQ. 1) GO TO 3250
      KM = 1
      IF ((MF(1).NE.0 .AND. MF(1).NE.1) .OR.
     1    (MF(2).NE.0 .AND. MF(2).NE.2)) GO TO 7
      IF (M(1).LE.0 .OR. M(2).EQ.0) GO TO 8
      IF (MF(3).NE.3 .OR. (MF(6).NE.3 .AND. MF(6).NE.0)) GO TO 7
      I(1) = M(1)
      I(2) = M(2)
      N   = 2
      LDH = 0
 3260 DO 3210 L = 3,8,3
      KDLH = MF(L) + MF(L+1) + MF(L+2)
      IF (KDLH.NE.0 .AND. KDLH.NE.6) GO TO 8
      IF (KDLH .EQ. 0) GO TO 3210
      N = N + 4
      I(N-3) = M(N-3+LDH)
      I(N-2) = M(N-2+LDH)
      I(N-1) = M(N-1+LDH)
      I(N)   = M(N  +LDH)
 3210 CONTINUE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 2
      N = N + 4
      I(N-3) = IBLANK
      I(N-2) = IBLANK
      I(N-1) = -1
      I(N  ) = -1
      KM = 0
      GO TO 2
 3250 N  = 0
      LDH = 2
      GO TO 3260
C
C*****         172-SPCSD ,  311-DAREAS          **********************
C              312-DELAYS,  313-DPHASES
C
 3300 IF (M(1) .LE. 0) GO TO 8
      IF (M(4) .LE. 0) GO TO 8
      IF (M(5) .LT. 0) GO TO 8
      IF (M(7) .LT. 0) GO TO 8
      IF (M(8) .LT. 0) GO TO 8
      N = 12
      IF (M(7) .EQ. 0 ) N = 9
      M(N-2) = -1
      M(N-1) = -1
      M(N  ) = -1
      GO TO 3
C
C*****         314-TICS         **************************************
C
 3350 IF (M(1) .LE. 0) GO TO 8
      IF (M(4) .LE. 0) GO TO 8
      IF (M(5) .LT. 0) GO TO 8
      DO 3351 L = 8,11
      M(L) = -1
 3351 CONTINUE
      N = 11
      GO TO 3
C
C*****         173-SPCS1      ******************************************
C
 3400 IF (KM .EQ. 1) GO TO 3410
      KM = 1
      IF (MF(1) .NE. 1) BADFOR =.TRUE.
      IF (MF(2) .NE. 3) BADFOR =.TRUE.
      IF (M(4)  .LT. 0) BADDAT =.TRUE.
      CALL WRITE (210,M,4,0)
      J1 = 4
      L1 = 5
      GO TO 3920
 3410 L1 = 1
      J1 = 1
      GO TO 3920
C
C*****         174-SPCS       ******************************************
C
C
C     SAME AS RELES DATA CARD
C
 3500 GO TO 3100
C
C*****         175-BDYC       ******************************************
 3600 IF (KM .EQ. 1) GO TO 3650
C
      IF (MF(8).NE.0 .OR. MF(1).NE.1) GO TO 7
      IF (M(1) .LE. 0) GO TO 8
 3660 DO 3605 L = 2,7,2
      IF (MF(  L).NE.0 .AND. MF(L  ).NE.3) GO TO 7
      IF (MF(L+1).NE.0 .AND. MF(L+1).NE.1) GO TO 7
 3605 CONTINUE
      I(1) = M(1)
      N  = 1
      J1 = 1
      IF (KM .EQ. 1) J1 = 0
      DO 3610 L = 2,7,2
      KDLH = MF(L) + MF(L+1)
      IF (KDLH.NE.0 .AND. KDLH.NE.4) GO TO 8
      IF (KDLH .EQ. 0) GO TO 3610
      N  = N  + 3
      J1 = J1 + 3
      I(J1-2) = M(N-2)
      I(J1-1) = M(N-1)
      I(J1  ) = M(N  )
 3610 CONTINUE
      N  = J1
      KM = 1
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 2
      KM = 0
      N  = N  + 3
      J1 = J1 + 3
      I(J1-2) = IBLANK
      I(J1-1) = IBLANK
      I(J1  ) = -1
      GO TO 2
 3650 IF (MF(1).NE.0 .OR. MF(8).NE.0) GO TO 7
      GO TO 3660
C
C*****         176-MPCS       ******************************************
C
 3700 IF (KM .EQ. 1) GO TO 3750
      KM = 1
      IF (MF(1) .NE. 1) GO TO 7
      IF (MF(2) .NE. 3) GO TO 7
      IF (MF(3) .NE. 1) GO TO 7
      IF (MF(4) .NE. 1) GO TO 7
      IF (MF(5) .NE. 2) GO TO 7
      IF (M(1)  .LE. 0) GO TO 8
      IF (M(4)  .LE. 0) GO TO 8
      IF (M(5)  .LT. 0) GO TO 8
      IF (M(6)  .EQ. 0) GO TO 8
      DO 3710 L = 1,6
      I(L) = M(L)
 3710 CONTINUE
      N = 6
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 2
      GO TO 7
 3750 IF (MF(1) .NE. 0) GO TO 7
      IF (MF(2) .NE. 3) GO TO 7
      DO 3755 L = 3,6,3
      IF (MF(L)+MF(L+2)+MF(L+1) .EQ. 0) GO TO 3755
      IF (MF(L).NE.1 .OR. MF(L+1).NE.1) GO TO 7
      IF (MF(L+2) .NE. 2) GO TO 7
      IF (M(L+1).LE.0 .AND. MF(L+2).LE.0) GO TO 8
 3755 CONTINUE
      N = 0
      DO 3765 L = 3,8,3
      KDLH = MF(L) + MF(L+1) + MF(L+2)
      IF (KDLH.NE.0 .AND. KDLH.NE.4) GO TO 8
      IF (KDLH .EQ. 0) GO TO 3765
      I(N+1) = M(2)
      I(N+2) = M(3)
      N = N + 5
      I(N-2) = M(L+1)
      I(N-1) = M(L+2)
      I(N  ) = M(L+3)
 3765 CONTINUE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 2
      I(N+1) = IBLANK
      I(N+2) = IBLANK
      N = N + 5
      I(N-2) = -1
      I(N-1) = -1
      I(N  ) = -1
      KM = 0
      GO TO 2
C
C*****         177-BDYS       ******************************************
C
 3800 DO 3810 L = 1,7
      IF (MF(L).NE.1 .AND. MF(L).NE.0) GO TO 7
      IF (MF(L).EQ.1 .AND.  M(L).LE.0) GO TO 8
 3810 CONTINUE
      IF (MF(1) .EQ. 0) GO TO 7
      N = 1
      I(N) = M(1)
      DO 3820 L = 2,7,2
      KDLH = MF(L) + MF(L+1)
      IF (KDLH.NE.2 .AND. KDLH.NE.0) GO TO 8
      IF (KDLH .EQ. 0) GO TO 3820
      N = N + 2
      I(N-1) = M(N-1)
      I(N  ) = M(N  )
 3820 CONTINUE
       N = N + 2
       I(N-1) = -1
       I(N  ) = -1
      GO TO 2
C
C*****         178-BDYS1      ******************************************
C
 3900 IF (KM .EQ. 1) GO TO 3910
      KM = 1
      IF (MF(1).NE.1 .OR. MF(2).GT.1) BADFOR =.TRUE.
      IF (M(1) .LT.1 .OR. M(2) .LT.0) BADDAT =.TRUE.
      CALL WRITE (210,M,2,0)
      J1 = 3
      L1 = 3
      GO TO 3920
 3910 J1 = 1
      L1 = 1
C
C     COMMON PROCESSING FOR SPCS1 AND BDYS1 CARDS
C
 3920 IF (MF(J1) .NE. 0) GO TO 3925
      J1 = J1 + 1
      L1 = L1 + 1
      GO TO 3960
 3925 IF (MF(J1) .EQ. 1) GO TO 3930
      BADFOR =.TRUE.
      GO TO 3965
 3930 IF (J1 .GT. 6) GO TO 3955
      IF (MF(J1+1) .NE. 3) GO TO 3955
      IF (M(L1+1) .EQ. THRU) GO TO 3935
      BADDAT =.TRUE.
      GO TO 3965
 3935 IF (MF(J1+2) .EQ. 1) GO TO 3940
      BADFOR =.TRUE.
      GO TO 3965
 3940 IF (M(L1+3) .GT. M(L1)) GO TO 3945
      BADDAT =.TRUE.
      GO TO 3965
 3945 IG1 = M(L1  )
      IG2 = M(L1+3)
      DO 3950 J = IG1,IG2
      CALL WRITE (210,J,1,0)
 3950 CONTINUE
      J1 = J1 + 3
      L1 = L1 + 4
      GO TO 3960
 3955 CALL WRITE (210,M(L1),1,0)
      J1 = J1 + 1
      L1 = L1 + 1
 3960 IF (J1 .LE. 8) GO TO 3920
 3965 IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 3970
      KN = 1
      N  = 0
      GO TO 9
 3970 KM = 0
      KN = 0
      N  = 1
      I(1) = -1
      GO TO 9
C
C*****         186-GNEW       ******************************************
C
 4000 IF (MF(1) .NE. 1) GO TO 7
      IF (MF(2) .NE. 3) GO TO 7
      IF (MF(3).NE.1 .AND. MF(3).NE.0) GO TO 7
      IF (MF(4) .NE. 1) GO TO 7
      IF (MF(5) .NE. 1) GO TO 7
      IF (M(1)  .LE. 0) GO TO 8
      IF (M(4)  .LT. 0) GO TO 8
      IF (M(5)  .LE. 0) GO TO 8
      IF (M(6)  .LE. 0) GO TO 8
      N = 6
      GO TO 3
C
C*****         187-GTRAN      ******************************************
C
 4100 IF (MF(1) .NE. 1) GO TO 7
      IF (MF(2) .NE. 3) GO TO 7
      IF (MF(3) .NE. 1) GO TO 7
      IF (MF(4).NE.1 .AND. MF(4).NE.0) GO TO 7
      IF (M(1)  .LE. 0) GO TO 8
      IF (M(4)  .LE. 0) GO TO 8
      IF (M(5)  .LT. 0) GO TO 8
      N = 5
      GO TO 3
C
C*****         193-USET       ******************************************
C
 4400 ASSIGN 4405 TO RET
 4401 N = 0
      IF (M(2) .NE. BLANK) GO TO 8
      DO 4402 L = 1,32
      IF (M(1) .EQ. KB(L,2)) GO TO 4404
 4402 CONTINUE
      GO TO 8
 4404 ID = KB(L,1)
      GO TO RET, (4405,4505)
 4405 DO 4440 L = 3,7,2
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0) GO TO 4440
      IF (M(L) .LE. 0) GO TO 8
      IF (IFPDCO(M(L+1))) GO TO 8
      LZ = 6
      IF (M(L+1) .EQ. 0) LZ = 1
      DO 4430 L2 = 1,LZ
      IF (LZ.NE.1 .AND. LL(L2).EQ.0) GO TO 4430
      N = N + 3
      I(N-2) = ID
      I(N-1) = M(L  )
      I(N  ) = LL(L2)
      IF (N .LE. 3) GO TO 4430
      DO 4420 L1 = 6,N,3
      IF (I(N-1).EQ.I(L1-4) .AND. I(N).EQ.I(L1-3)) GO TO 8
 4420 CONTINUE
 4430 CONTINUE
 4440 CONTINUE
      IF (N) 8,8,2
C
C*****         194-USET1      ******************************************
C
 4500 IF (KM .NE. 0) GO TO 4510
      KM = 1
      ASSIGN 4505 TO RET
      GO TO 4401
 4505 N = 2
      I(1) = ID
      IF (MF(2).NE.0 .AND. MF(2).NE.1) BADFOR =.TRUE.
      IF (IFPDCO(M(3))) BADDAT =.TRUE.
      I(2) = M(3)
      IF (MF(4).EQ.3 .AND. M(5).EQ.THRU) GO TO 4550
      L1 = 4
      L3 =-1
      L2 = 9
      GO TO 4511
 4510 L1 = 1
      L3 = 0
      L2 = 8
 4511 DO 4515 L = L1,L2
      IF (MF(L+L3).NE.0 .AND. MF(L+L3).NE.1) BADFOR =.TRUE.
 4515 CONTINUE
      DO 4520 L = L1,L2
      IF (MF(L+L3) .EQ. 1) GO TO 4525
 4520 CONTINUE
      BADDAT =.TRUE.
 4525 DO 4540 L = L1,L2
      IF (M(L)) 4535,4540,4530
 4530 N = N + 1
      I(N) = M(L)
      GO TO 4540
 4535 BADDAT =.TRUE.
 4540 CONTINUE
      KN = 1
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
 4545 KM = 0
      N  = N + 1
      I(N) = -1
      KN = 0
      GO TO 9
 4550 IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 4555
      KN = 1
      BADFOR =.TRUE.
      GO TO 9
 4555 IF (MF(3).NE.1 .OR. MF(5).NE.1 ) BADFOR =.TRUE.
      IF (M(4).LE.0 .OR. M(7).LE.M(4)) BADDAT =.TRUE.
      DO 4560 L = 1,3
      IF (MF(L+5) .NE. 0) BADFOR =.TRUE.
 4560 CONTINUE
      IF (BADFOR .OR. BADDAT) GO TO 4545
      CALL WRITE (210,I,2,0)
      L1 = M(4)
      L2 = M(7)
      DO 4570 L = L1,L2
 4570 CALL WRITE (210,L,1,0)
      N = 0
      GO TO 4545
C
C*****         245-SAME 246-NOSAME          ****************************
C
 4600 CONTINUE
      IALT = 1
      IF (M(3) .EQ. THRU) IALT = 3
      KDX  = IALT + ICONT
      GO TO (4620,4620,4630,4640), KDX
 4620 DO 4621 IN1 = 1,8,2
      IN2 = IN1 + 1
      IF (MF(IN1).EQ.0 .AND. MF(IN2).EQ.0) GO TO 4621
      IF (MF(IN1).NE.1 .OR.  MF(IN2).NE.1) BADFOR =.TRUE.
      IF (M(IN1) .LE.0 .OR.  M(IN2) .LE.0) BADDAT =.TRUE.
C
      N = N + 2
      I(N-1) = M(IN1)
      I(N  ) = M(IN2)
 4621 CONTINUE
      GO TO 4680
C
 4630 IF (MF(1).NE.1 .OR. MF(2).NE.1 .OR. MF(3).NE.3 .OR. MF(4).NE.1 .OR
     1.   MF(5).NE.1 .OR. MF(6).NE.1 .OR. MF(7).NE.3 .OR. MF(8).NE.1)
     2    BADFOR =.TRUE.
      IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(5).LE.0 .OR. M(6).LE.0 .OR.
     1    M(7).LE.0 .OR. M(8).NE.THRU .OR. M(10).LE.0) BADDAT =.TRUE.
     2
      IF (M(5).LE.M(2) .OR. M(10).LE.M(7)) BADDAT =.TRUE.
      IRANGE = M(5) - M(2)
      IF ((M(10)-M(7)) .NE. IRANGE)  BADDAT =.TRUE.
      I(1) = -1
      I(2) = IRANGE + 1
      I(3) = M(1)
      I(4) = M(2)
      I(5) = M(6)
      I(6) = M(7)
      N = 6
      GO TO 4680
 4640 DO 4650 IN1 = 1,6,5
      IN2 = IN1 + 1
      IN3 = IN2 + 1
      IN4 = IN3 + 1
      IN5 = IN4 + 1
      IF (MF(IN1).EQ.0 .AND. MF(IN2).EQ.0 .AND. MF(IN3).EQ.0 .AND.
     1    MF(IN4).EQ.0) GO TO 4650
      IF (MF(IN1).NE.1 .OR. MF(IN2).NE.1 .OR. MF(IN3).NE.3 .OR.
     1    MF(IN4).NE.1) BADFOR = .TRUE.
      IF (M(IN1).LE.0 .OR. M(IN2).LE.0 .OR. M(IN3).NE.THRU .OR.
     1    M(IN5).LE.0) BADDAT =.TRUE.
      IF (M(IN5).LE.M(IN2) .OR. M(IN5)-M(IN2).NE.IRANGE) BADDAT =.TRUE.
      I(N+1) = M(IN1)
      I(N+2) = M(IN2)
      N = N + 2
 4650 CONTINUE
C
 4680 IF (M1F(1).EQ.0 .AND. M1F(2).EQ.0) GO TO 4685
      ICONT  = 0
      I(N+1) =-1
      I(N+2) =-1
      N = N + 2
      GO TO 9
 4685 ICONT = 1
      GO TO 9
C
C*****         338-CELBOW     ******************************************
C
 4700 IF (M(2) .EQ. 0) M(2) = M(1)
      N = 8
      GO TO 3
C
C*****         339-PELBOW     ******************************************
C
 4710 N = 24
      GO TO 3
C
C*****         251-CIHEX1     ******************************************
C
 5000 N = 10
 5010 DO 5020 L = 1,N
      IF (M(L) .LE. 0) GO TO 8
 5020 CONTINUE
      N1 = N - 1
      DO 5040 L = 3,N1
      L2 = L + 1
      DO 5030 L1 = L2,N
      IF (M(L) .EQ. M(L1)) GO TO 8
 5030 CONTINUE
 5040 CONTINUE
      GO TO 3
C
C*****         252-CIHEX2     ******************************************
C
 5100 N = 22
      GO TO 5010
C
C*****         253-CIHEX3     ******************************************
C
 5200 N = 34
      GO TO 5010
C
C*****         254-PIHEX      ******************************************
C
 5300 N = 7
      IF (M(1).LE.0 .OR. M(2).LE.0) GO TO 8
      IF (M(3) .LT. 0) GO TO 8
      IF ((M(4).LT.2 .OR. M(4).GT.4) .AND. M(4).NE.0) GO TO 8
      DO 5320 L = 5,7
      IF (MF(L) .EQ.   0) GO TO 5310
      IF (MF(L) .NE.   2) GO TO 7
      IF (RM(L) .LT. 0.0) GO TO 8
      GO TO 5320
 5310 RM(L) = -1.0
 5320 CONTINUE
      IF (RM(5).GE.0.0  .AND. RM(5).LT.1.0  ) GO TO 8
      IF (RM(6).GT.180.0 .OR. RM(7).GT.180.0) GO TO 8
      GO TO 3
C
C*****         255-PLOAD3     ******************************************
C
 5400 IF (M(1) .LE. 0) GO TO 8
      DO 5410 L = 3,6,3
      IF (M(L).EQ.0 .AND. M(L+1).EQ.0 .AND. M(L+2).EQ.0) GO TO 5410
      IF (M(L).LT.0 .OR.  M(L+1).LT.0 .OR.  M(L+2).LT.0) GO TO 8
      N = N + 5
      I(N-4) = M(1)
      I(N-3) = M(2)
      I(N-2) = M(L)
      I(N-1) = M(L+1)
      I(N  ) = M(L+2)
 5410 CONTINUE
      IF (N) 8,8,2
C
C*****    263-CAERO1, 301-CAERO2, 302-CAERO3, 303-CAERO4  *******
C         309-CAERO5
C
 6400 IF (M(1) .LE. 0) GO TO 8
      IF (M(2) .LE. 0) GO TO 8
      DO 6404 L = 3,8
      IF (M(L) .LT. 0) GO TO 8
 6404 CONTINUE
      IF (K .EQ. 302) GO TO 6410
      IF (K .EQ. 303) GO TO 6420
      IF (K .EQ. 309) GO TO 6420
      IF (M(4).EQ.0 .AND. M(6).EQ.0) GO TO 8
      IF (M(5).EQ.0 .AND. M(7).EQ.0) GO TO 8
      IF (M(8) .LE. 0) GO TO 8
 6405 IF (RM(12) .LT. 0.0) GO TO 8
      IF (RM(16) .LT. 0.0) GO TO 8
      IF (RM(12).EQ.0.0 .AND. RM(16).EQ.0.0) GO TO 8
      N = 16
      GO TO 3
C
C*****     CAERO3      ************************************************
C
 6410 IF (M(4)   .EQ. 0 ) GO TO 8
      IF (RM(12) .EQ. 0.) GO TO 8
      GO TO 6405
C
C*****     CAERO4   CAERO5    ******************************************
C
 6420 IF (M(4).EQ.0 .AND. M(5).EQ.0) GO TO 8
      IF (M(6) .GT. 2) GO TO 8
      GO TO 6405
C
C*****         264-PAERO1     ******************************************
C
 6500 IF (M(1) .LE. 0) GO TO 8
      DO 6501 L = 2,8
      IF (M(L) .LT. 0) GO TO 8
 6501 CONTINUE
      N = 8
      GO TO 3
C
C*****      304 - PAERO2    ***************
C
 6510 IF (M(1) .LE. 0) GO TO 8
      DO 6511 L = 1,3
      IF (M(2) .EQ. MOT(L)) GO TO 6512
 6511 CONTINUE
      GO TO 8
 6512 IF (RM(4) .LE. 0.0) GO TO 8
      IF (RM(5) .LE. 0.0) GO TO 8
      DO 6513 L = 6,15
      IF (M(L) .LT. 0) GO TO 8
 6513 CONTINUE
      N = 15
      GO TO 3
C
C*****      305 - PAERO3    ****************
C
 6520 IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(3).LT.0) GO TO 8
      IF (M(2) .GT. 50) GO TO 8
      N = 0
      IF (M(3) .EQ. 0) N = 4
      IF (M(3) .EQ. 1) N = 12
      IF (M(3) .EQ. 2) N = 16
      IF (N .EQ. 0) GO TO 8
      M(4) = N
      N = N + 4
      IF (N .EQ. 8) GO TO 6522
      DO 6521 L = 9,N
      IF (MF(L) .EQ. -32767) GO TO 8
 6521 CONTINUE
      IF (RM(12) .LT. RM(10)) GO TO 8
      IF (RM(16) .LT. RM(14)) GO TO 8
      IF (N .EQ.  16) GO TO 6522
      IF (RM(20) .LT. RM(18)) GO TO 8
 6522 GO TO 3
C
C*****      306 - PAERO4   **********************
C
 6530 IF (KM .NE. 0) GO TO 6535
      KM = 1
      IF (MF(1).NE.1 .OR. M(1).LE.0) GO TO 6540
      DO 6531 L = 2,5
      IF (MF(1).LT.0 .OR. MF(L).GT.1) GO TO 6540
 6531 CONTINUE
      IF (M(3) .LT. 0) GO TO 6540
      IF (M(2).EQ.0 .AND. M(3).NE.0) GO TO 6540
      IF (M(2).GT.0 .AND. M(3).EQ.0) GO TO 6540
      IF (M(2).NE.0 .AND. M(4).NE.0) GO TO 6540
      IF (M(4).LT.0 .OR.  M(4).GT.3) GO TO 6540
      IF (M(4).EQ.0 .AND. M(5).NE.0) GO TO 6540
      IF (M(4).GT.0 .AND. M(5).EQ.0) GO TO 6540
      DO 6532 L = 1,5
 6532 I(L) = M(L)
      N  = 5
      L1 = 6
      GO TO 6533
 6535 L1 = 1
 6533 DO 6534 L = L1,8
      IF (MF(L) .EQ.  0) GO TO 6550
      IF (MF(L) .NE.  2) GO TO 6540
      IF (RM(L) .LT. 0.) GO TO 6540
      N = N + 1
      I(N) = M(L)
 6534 CONTINUE
 6539 KN = 1
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
      KN = 0
      KM = 0
      N  = N + 1
      I(N) = -1
      GO TO 9
 6540 BADDAT = .TRUE.
      GO TO 6539
 6550 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) BADDAT = .TRUE.
      GO TO 6539
C
C*****   310 - PAERO5  ************
C
 7700 IF (KM .NE. 0) GO TO 6535
      KM = 1
      DO 7701 L = 1,3
      IF (MF(L).NE.1 .OR. M(L).LE.0) GO TO 6540
 7701 CONTINUE
      DO 7702 L = 4,7
      IF (MF(L).LT.0 .OR. MF(L).GT.1) GO TO 6540
 7702 CONTINUE
      IF (M(4).NE.0 .AND. M(5).EQ.0) GO TO 6540
      IF (M(6).NE.0 .AND. M(7).EQ.0) GO TO 6540
      DO 7703 L = 1,7
 7703 I(L) = M(L)
      N = 7
      GO TO 6539
C
C*****         265-AERO       ******************************************
C
 6600 IF (IAERO .NE. 0) GO TO 8
      IAERO = 1
      IF (M(1) .LT. 0) GO TO 8
      N = 6
      GO TO 3
C
C*****         266-SPLINE1    ******************************************
C
 6700 IF (M(2).LE.0 .OR. M(3).LE.0 .OR. M(4).LE.0 .OR. M(5).LE.0 .OR.
     1    M(1).LE.0 .OR. RM(6).LT.0.0) GO TO 8
      N = 6
      GO TO 3
C
C*****         267-SPLINE2    ******************************************
C
 6800 IF (M(1).LE.0 .OR. M(2).LE.0 .OR. M(3).LE.0 .OR. M(4).LE.0 .OR.
     1    M(5).LE.0 .OR.M(8).LT.0) GO TO 8
      N = 10
      GO TO 3
C
C*****    307 - SPLINE3       *********************
C
 6850 IF (KM .NE. 0) GO TO 6852
      KM = 1
      IF (MF(1).NE.1 .OR. MF(2).NE.1 .OR. MF(3).NE.1 .OR. MF(4).NE.1)
     1    GO TO 6540
      IF (M(2).LE.0 .OR. M(3).LT.0) GO TO 6540
      IF (IFPDCO(M(4))) GO TO 6540
      IF (GC(2) .NE. 0) GO TO 6540
      DO 6851 L = 1,4
 6851 I(L) = M(L)
      N  = 4
      L1 = 5
      GO TO 6853
 6852 L1 = 1
 6853 DO 6854 L = L1,8,4
      IF (MF(L  ) .EQ. 0) GO TO 6854
      IF (MF(L  ) .NE. 1) GO TO 6540
      IF (MF(L+1) .NE. 1) GO TO 6540
      IF (IFPDCO(M(L+1))) GO TO 6540
      IF (GC(2  ) .NE. 0) GO TO 6540
      IF (MF(L+2) .NE. 2) GO TO 6540
      IF (M(L)    .LE. 0) GO TO 6540
      N = N + 3
      I(N  ) = M(L+2)
      I(N-1) = M(L+1)
      I(N-2) = M(L  )
 6854 CONTINUE
      GO TO 6539
C
C*****         269-SET2       ******************************************
C
 5600 IF (M(1).LE.0 .OR. M(2).LE.0) GO TO 8
      N = 8
      GO TO 3
C
C*****         270-MKAERO2    ******************************************
C
 5700 N = 0
      DO 5702  L = 2,8,2
      IF (MF(L).EQ.0 .AND. MF(L-1).EQ.0) GO TO 5702
      IF (MF(L).EQ.0 .OR.  MF(L-1).EQ.0) GO TO 7
      N = N + 2
      I(N-1) = M(L-1)
      IF (RM(L) .LE. 0.0) GO TO 8
      I(N) = M(L)
 5702 CONTINUE
      IF (N .EQ. 0) GO TO 8
      GO TO 2
C
C*****         271-MKAERO1    ******************************************
C
 5800 IF (MF(1).NE.2 .OR. MF(9).NE.2) GO TO 7
      IF (RM(9) .LE. 0.0) GO TO 8
      DO 5810 L = 2,8
      IF (MF(L) .EQ. 0) M(L) = -1
      IF (MF(L+8).NE.0 .AND. RM(L+8).LE.0.0) GO TO 8
      IF (MF(L+8) .EQ. 0) M(L+8) = -1
 5810 CONTINUE
      N = 16
      GO TO 3
C
C*****         257-FLUTTER    ******************************************
C
 5900 IF (M(1).LE.0 .OR. M(4).LT.0 .OR. M(5).LT.0 .OR. M(6).LT.0) GOTO 8
      DO 5910 L = 1,NMT
      IF (M(2) .EQ. MET(L)) GO TO 5920
 5910 CONTINUE
      GO TO 8
 5920 CONTINUE
      IF (M(7).NE.MS .AND. M(7).NE.ML) GO TO 8
      N = 10
      GO TO 3
C
C******    308 - GUST
C
 7600 IF (M(1).LE.0 .OR. M(2).LE.0) GO TO 8
      IF (RM(3).EQ.0.0 .OR. RM(5).EQ.0.0) GO TO 8
      N = 5
      GO TO 3
C
C*****         198-PLOAD1       ****************************************
C
 6000 IF (M(1).LE.0 .OR. M(2).LE.0) GO TO 8
      I(1) = M(1)
      I(2) = M(2)
      DO 6010 L = 1,12
      IF (M(3) .EQ. ITYPE(L)) GO TO 6020
 6010 CONTINUE
      GO TO 8
 6020 I(3) = L
      DO 6030 L = 1,4
      IF (M(5) .EQ. ISCAL(L)) GO TO 6040
 6030 CONTINUE
      GO TO 8
 6040 I(4) = L
      IF (RM(9) .EQ. 0.0) RM(9) = RM(7)
      IF (RM(9) .LT. RM(7)) GO TO 8
      DO 6050 L = 7,10
 6050 I(L-2) = M(L)
      N = 8
      GO TO 2
C
C*****         275-CBARAO       ****************************************
C
 6100 IF (M(1) .LE. 0) GO TO 8
      I(1) = M(1)
      DO 6110 L = 1,2
      IF (M(2) .EQ. ISCAL(L)) GO TO 6120
 6110 CONTINUE
      GO TO 8
 6120 I(2) = L
      DO 6130 L = 4,9
 6130 I(L-1) = M(L)
      N = 9
      IF (MF(3) .EQ. 2) GO TO 6140
      IF (MF(3) .NE. 1) GO TO 7
      IF (I(3)  .LE. 0) GO TO 8
      IF (I(3) .GT. 20) I(3) = 20
      IF (RM(5).LE.0.0 .OR. RM(6).LE.0.0) GO TO 8
      I(9) = -1
      GO TO 2
 6140 I(9) = 1
      DO 6150 L = 4,9
      IF (RM(L) .LT. 0.0) GO TO 8
 6150 CONTINUE
      GO TO 2
C
C*****         276-PLIMIT       ****************************************
C
 6200 IF (MF(1) .NE. 3) GO TO 7
      IF (MF(2).NE.2 .AND. MF(2).NE.0) GO TO 7
      IF (RM(3) .LT. 0.0) GO TO 8
      IF (RM(3).EQ.0.0 .AND. RM(4).EQ.0.0) GO TO 8
      IF (RM(4) .EQ. 0.0) GO TO 6210
      IF (MF(3).NE.2 .OR. RM(4).LE.RM(3)) GO TO 8
 6210 IF (MF(5) .EQ.3 ) GO TO 6230
      DO 6220 L = 4,8
      IF (MF(L).NE.0 .AND. MF(L).NE.1) GO TO 7
      IF (M(L+1) .LT. 0) GO TO 8
 6220 CONTINUE
      GO TO 6240
 6230 IF (M(6) .NE. THRU) GO TO 8
      IF (MF(4).NE.1 .OR. MF(6).NE.1) GO TO 7
      IF (M(8) .LE. M(5)) GO TO 8
 6240 N = 9
      GO TO 3
C
C*****         277-POPT         ****************************************
C
 6300 IF (M(1).LE.0 .OR. M(4).EQ.0) GO TO 8
      IF (IPOPT .NE. 0) GO TO 8
      IPOPT = 1
      IF (RM(2) .LT. 0.0) GO TO 8
      IF (RM(3) .LE. 0.0) GO TO 8
      IF (M(5).NE.IYES .AND. M(5).NE.INO) GO TO 8
      N = 6
      GO TO 3
C
C******       278  PLOADX   ******************************************
C
 6900 IF (M(1) .LE. 0) GO TO 8
      IF (M(4).LE.0 .OR. M(5).LE.0 .OR. M(6).LE.0) GO TO 8
      N = 6
      GO TO 3
C
C
C
C     ******************************************************************
C
C     PROCESS ADUM-I CARDS.
C
 8100 CONTINUE
      IF (M(1) .LE. 0) GO TO 8
      IF (M(2) .LT. 0) GO TO 8
      IF (M(3) .LT. 0) GO TO 8
      IF (M(4).NE.3 .AND. M(4).NE.6) GO TO 8
      IF (MF(5).NE.0 .OR. MF(6).NE.0 .OR. MF(7).NE.0 .OR. MF(8).NE.0)
     1    GO TO 7
      KDUMEL(IDUMEL) = M(4) + 10*(M(3) + 1000*(M(2) + 1000*M(1)))
C
C     PUT IN CONNECTION AND PROPERTY CARD NAME IF SUPPLIED BY USER
C
      IF (MF(5).NE. 3) GO TO 8150
      NBPC = JUNK(36)
      NCPW = JUNK(38)
      NSHT = NBPC*(NCPW-1)
      NM1  = T1(1,K)
      NM2  = T1(2,K)
      NM1  = RSHIFT(LSHIFT(NM1,NBPC),NBPC)
      C    = LSHIFT(RSHIFT(C,NSHT),NSHT)
      NM1  = ORF(NM1,C)
      P    = LSHIFT(RSHIFT(P,NSHT),NSHT)
      DO 8110 L = 1,NCDS
      IF (NM1.EQ.T1(1,L) .AND. NM2.EQ.T1(2,L)) GO TO 8120
 8110 CONTINUE
      GO TO 8150
 8120 T1(1,L) = M(5)
      T1(2,L) = M(6)
      NM1 =  ORF(P,RSHIFT(LSHIFT(NM1,NBPC),NBPC))
      DO 8130 L = 1,NCDS
      IF (NM1.EQ.T1(1,L) .AND. NM2.EQ.T1(2,L)) GO TO 8140
 8130 CONTINUE
      GO TO 8150
 8140 M(5) = ORF(P,RSHIFT(LSHIFT(M(5),NBPC),NBPC))
      T1(1,L) = M(5)
      T1(2,L) = M(6)
 8150 CONTINUE
      RETURN 3
C
C     ******************************************************************
C
C     PROCESS CDUM-I CARDS.
C
 8200 CONTINUE
C
C     ==============
C     ONLY DO THIS FOR FIRST ONE IF I CAN FIGURE OUT HOW
C
      ASSIGN 8210 TO RET
      GO TO 9010
 8210 CONTINUE
C     ==============
C
      IF (MF(1).NE.1 .OR. MF(2).NE.1) GO TO 7
      IF (M(1).LE.0  .OR. M(2) .LE.0) GO TO 8
      L1 = NDUMG + 2
      DO 8220 L = 3,L1
      IF (MF(L) .NE. 1) GO TO 7
      IF (M(L)  .LE. 0) GO TO 8
      IF (L .EQ. 3) GO TO 8220
      L3 = L - 1
      DO 8215 L2 = 3,L3
      IF (M(L2)-M(L)) 8215,8,8215
 8215 CONTINUE
 8220 CONTINUE
      N = NDUMC
      GO TO 3
C
C     ******************************************************************
C
C     PROCESS PDUM-I CARDS.
C
 8300 CONTINUE
C
C     ==============
C     ONLY DO THIS FOR FIRST ONE IF I CAN FIGURE OUT HOW
C
      ASSIGN 8310 TO RET
      GO TO 9010
 8310 CONTINUE
C     ==============
C
      IF (MF(1).NE.1 .OR. MF(2).NE.1) GO TO 7
      IF (M(1).LE.0  .OR. M(2) .LE.0) GO TO 8
      N = NDUMP
      GO TO 3
C
C     ******************************************************************
C
C     DECODE ADUM-I CARD CONTENTS AS PACKED INTO /SYSTEM/
C
 9010 CONTINUE
      NDUMG = KDUMEL(IDUMEL)/10000000
      NDUMD = KDUMEL(IDUMEL) - 10000000*NDUMG
      NDUMC = NDUMD/10000
      NDUMP = (NDUMD - NDUMC*10000)/10
      NDUMD = KDUMEL(IDUMEL) - (KDUMEL(IDUMEL)/10)*10
      NDUMC = NDUMG + NDUMC + 2
      NDUMP = NDUMP + 2
      IF (NDUMC .GT. 24) GO TO 8
      IF (NDUMP .GT. 24) GO TO 8
      GO TO RET, (8210,8310)
 9999 RETURN
C
      END
