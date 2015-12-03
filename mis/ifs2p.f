      SUBROUTINE IFS2P (*,*,*)
C
      IMPLICIT INTEGER (A-Z)
      EXTERNAL        LSHIFT,ORF
      LOGICAL         ABORT,FLUSH,FLSHAL,EC,INT,DMIFLG,FPHYS,FPHYS1,
     1                BADDAT,BADFOR,IAX,IAXF,FPHYS2,LHARM,SECD,FTHRU
      INTEGER         NAM(2),ONM(2),NM(2),T(7),IHILL(2),IHOFF(2),
     1                ITSAI(2),ISTRS(2),ISTRN(2),IALL(2),ISYM(2),
     2                IMEM(2),ISYMM(2)
      REAL            XM(100),Z(100),XL,XL1,X1,X2,ZSEQ,ZSEQ1,OLDXM3
      DOUBLE PRECISION DA(2)
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /MACHIN/ MACH
      COMMON /XMSSG / UFM,UWM,UIM,SFM
      COMMON /SYSTEM/ KSYSTM(77)
      COMMON /XPFIST/ IPFIST
      COMMON /XFIST / IFIST(1)
      COMMON /XFIAT / IFIAT(2)
      COMMON /IFPX1 / NT1,T1(2,1)
      COMMON /IFPDTA/ ID,N,K,KX,KY,I(100),M(100),MF(100),M1(100),
     1                M1F(100),KN,BADDAT,BADFOR,NOPEN,NPARAM,IAX,NAX,
     2                IAXF,NAXF,LHARM,KNT,SLOTDF(5),GC(7),LL(6)
      COMMON /ZBLPKX/ A(4),I0
      COMMON /ZZZZZZ/ IBUF(1)
      COMMON /CIFS2P/ FPHYS,FPHYS1,KM,DMIFLG,IBCDS,FTHRU,FPHYS2
      COMMON /XDPL  / P(3)
      COMMON /L15 L8/ L15,L8
C
C     P(1) = NEXT AVAILABLE FILE ON POOL
C     P(2) = TOTAL NUMBER OF POSSIBLE ENTRYS
C     P(3) = CURRENT NUMBER OF ENTRYS PRESENT
C     P(4) - P(3*P(2)+3) = THREE WORDS FOR EACH ENTRY AS FOLLOWS...
C            1.  NAME(1)
C            2.  NAME(2)
C            3.  EQUIV FLAG, SIZE/1000, FILE NO. ON POOL
C
      EQUIVALENCE   (KSYSTM( 1) , NBUF  )  , (KSYSTM(24) , ICFIAT) ,
     1              (KSYSTM( 2) , NOUT  )  , (KSYSTM(55) , KPREC ) ,
     2              (KSYSTM( 3) , ABORT )  , (KSYSTM(77) , BANDIT) ,
     3              (NROWS,T(3)),(IFO,T(4)),(TY2,T(5)),(Z(1),I(1)) ,
     4              (XM(1),M(1)),(DA(1),A(1))
C
      DATA   NAM    / 4HISF2,   4HP    /
      DATA   ENDT   / 4HENDT /, SKIP   / 4HSKIP /, POOL   / 4HPOOL /
      DATA   BCDBLK / 4H     /, BCDDET / 4HDET  /, BCDSDT / 4HSDET /,
     1       BCDUDT / 4HUDET /, BCDINV / 4HINV  /, BCDSIN / 4HSINV /,
     2       BCDUIN / 4HUINV /, BCDGIV / 4HGIV  /, BCDMGV / 4HMGIV /,
     3       BCDHES / 4HHESS /, BCDFER / 4HFEER /, BCDMAS / 4HMASS /,
     4       BCDMAX / 4HMAX  /, BCDPOI / 4HPOIN /,
     5       BCDQ   / 4H-Q   /, BCDT   / 4HT    /, BCDZ   / 4H-X   /,
     6       BCDLL  / 4HLL   /, BCDSL  / 4HSL   /, BCDLS  / 4HLS   /
      DATA   THRU   / 4HTHRU /, EIGR   / 4HEIGR /, EIGB   / 4HEIGB /
      DATA   DMI    / 4H DMI /, DTI    / 4H DTI /, DMIG   / 4HDMIG /
      DATA   ENDRC1,  ENDRC2 /  4HENDR , 4HEC   /
      DATA   ISCR1  / 301    /, ICOMP  / 1      /
      DATA   IHILL  , IHOFF   , ITSAI  , ISTRS   , ISTRN  /
     1       4HHILL , 4H      , 4HHOFF , 4H      , 4HTSAI , 4H      ,
     2       4HSTRE , 4HSS    , 4HSTRA , 4HIN   /
      DATA   IALL   , ISYM    , IMEM   , ISYMM  /
     1       4HALL  , 4H      , 4HSYM  , 4H      , 4HMEM  , 4H      ,
     2       4HSYMM , 4HEM   /
      DATA   IYES,    INO    /  4HYES  , 4HNO   /
C
C     =======================================================
C     DMI AND DMIG MUST ACCOMODATE ALL KINDS OF SPECIAL FORMS
C     E.G., IDENTITY MATRIX
C     =======================================================
C
      IF (K .GT. 100) GO TO 81
      GO TO (     5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     2            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     3            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     4            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     5            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     6            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     7            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     8            5,   5,   5,   5, 850, 850, 870,   5, 890,   5,
     9            5,   5, 920, 920, 920, 960, 920,   5,   5,   5 ), K
   81 IF (KX .GT. 100) GO TO 82
      GO TO (     5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,   5,   5,   5,   5,1190,1200,
     2            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     3            5,   5, 920, 920,   5,   5,   5,   5,   5, 920,
     4          960,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     5            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     6            5, 920,   5,   5,   5,   5,   5,   5,   5,   5,
     7            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     8            5,   5,   5,   5,   5,   5,   5,1000,   5,   5,
     9          920,2900,   5,   5,   5,   5,   5,   5,   5,2000 ), KX
   82 IF (KY .GT. 100) GO TO 83
      GO TO (     5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     2            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     3            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     4            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     5            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     6         3100,3300,   5,   5,   5,   5,   5,   5,   5,   5,
     7            5,   5,   5,   5,   5,   5,   5,   5,   5,4100,
     8         4300,4500,4700,   5,   5,   5,   5,   5,   5,   5,
     9            5,   5,   5,   5,   5,   5,   5,   5,   5,   5 ), KY
   83 KZ = K - 300
      IF (KZ .GT. 60) GO TO 5
      GO TO (     5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     1            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     2            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     3            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     4            5,   5,   5,   5,   5,   5,   5,   5,   5,   5,
     5            5,   5,   5,3200,   5,   5, 920,   5,   5,   5 ), KZ
    5 CALL PAGE2 (2)
      WRITE  (NOUT,6) SFM
    6 FORMAT (A25,' 322, ILLEGAL ENTRY TO IFS2P.')
      ABORT  =.TRUE.
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
C*******       85-EIGR, 86-EIGB      ***********************************
C
  850 IF (M(1) .LE. 0) GO TO 8
      IF (M(3).NE.BCDBLK .AND. M(2).NE.BCDFER) GO TO 8
      IF (M(2).NE.BCDDET .AND. M(2).NE.BCDSDT .AND. M(2).NE.BCDUDT .AND.
     1    M(2).NE.BCDINV .AND. M(2).NE.BCDSIN .AND. M(2).NE.BCDUIN .AND.
     2    M(2).NE.BCDGIV .AND. M(2).NE.BCDMGV .AND. M(2).NE.BCDFER)
     3    GO TO 8
      IF (M(2).EQ.BCDFER .AND. (M(3).NE.BCDBLK .AND. M(3).NE.BCDQ .AND.
     1    M(3).NE.BCDZ)) GO TO 8
      IF (M(10)+M(11) .EQ. 0) GO TO 852
      IF (M(10).NE.BCDBLK .OR. M(11).NE.BCDBLK) GO TO 860
  852 NM(1) = EIGR
      NM(2) = BCDMAS
      IF (K .EQ. 85) GO TO 855
      NM(1) = EIGB
      NM(2) = BCDMAX
  855 M(10) = NM(2)
      M(12) = 0
      M(13) = 0
      CALL MESAGE (30,222,NM)
      GO TO 865
  860 IF ((M(10).NE.BCDMAS .OR. M(11).NE.BCDBLK) .AND.
     1    (M(10).NE.BCDMAX .OR. M(11).NE.BCDBLK) .AND.
     2    (M(10).NE.BCDPOI .OR. M(11).NE.BCDT )) GO TO 8
      IF (M(10).NE.BCDPOI .AND. (M(12).NE.0 .OR. M(13).NE.0)) GO TO 8
      IF (M(10).EQ.BCDPOI .AND. (M(12).LE.0 .OR. M(13).LT.0)) GO TO 8
  865 IF (M(6).EQ.0 .AND. M(2).NE.BCDGIV .AND. M(2).NE.BCDMGV .AND.
     1    M(2).NE.BCDFER) GO TO 8
      IF (K.EQ.86 .AND. (M(2).EQ.BCDGIV .OR. M(2).EQ.BCDMGV)) GO TO 8
      IF ((M(2).EQ.BCDDET .OR. M(2).EQ.BCDSDT) .AND. XM(4).LT.0.0)
     1    GO TO 8
      IF (M(2).EQ.BCDUDT .AND. XM(4).LT.0.0) GO TO 8
      IF (K.EQ.85 .AND. M(2).NE.BCDGIV .AND. M(2).NE.BCDMGV .AND.
     1    XM(4).LT.0.0) GO TO 8
      IF (M(2).NE.BCDGIV .AND. M(2).NE.BCDMGV .AND. M(2).NE.BCDFER .AND.
     1    XM(5).LE.0.0) GO TO 8
      IF (M(2).NE.BCDGIV .AND. M(2).NE.BCDMGV .AND. M(2).NE.BCDFER .AND.
     1    XM(4).GE.XM(5)) GO TO 8
      N = 18
      GO TO 3
C
C*****         87-EIGC          **************************************
C
  870 IF (KM .NE. 0) GO TO 872
      IF (MF(1).NE.1 .OR. MF(2).NE.3 .OR. MF(3).NE.3 .OR. MF(4).NE.1
     1   .AND. MF(4).NE.0 .OR. MF(5).NE.1 .AND. MF(5).NE.0 .OR.
     2   MF(6).NE.2 .AND. MF(6).NE.0 .OR. MF(7).NE.0 .OR. MF(8).NE.0)
     3   GO TO 875
      IF (M(1).LE.0 .OR. M(2).NE.BCDDET .AND. M(2).NE.BCDINV .AND.
     1    M(2).NE.BCDHES .AND. M(2).NE.BCDFER .OR. M(4).NE.BCDMAX .AND.
     2   (M(4).NE.BCDPOI .OR. M(5).NE.BCDT) .OR. XM(8).LT.0.) GO TO 875
      IF (M(4).EQ.BCDMAX .AND. (M(6).NE.0 .OR. M(7).NE.0)) GO TO 875
      IF (M(4).EQ.BCDPOI .AND. (M(6).LE.0 .OR. M(7).LT.0)) GO TO 875
      IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 875
      N = 10
  874 DO 871 L = 1,N
  871 I(L) = M(L)
      GO TO 876
  872 DO 873 L = 1,5
      IF (MF(L).NE.2 .AND. MF(L).NE.0) GO TO 875
  873 CONTINUE
      IF (MF(6).NE.1 .AND. MF(6).NE.0 .OR. MF(7).NE.1 .AND. MF(7).NE.0
     1   .OR. MF(8).NE.0) GO TO 875
      IF (XM(5) .LE. 0.) XM(5) = 1.0
      IF (M(6).LT.0 .OR. M(7).LT.0) GO TO 875
      N = 7
      GO TO 874
  875 BADDAT = .TRUE.
  876 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 877
      DO 878 L = 1,7
      N = N + 1
  878 I(N) =-1
      KM = 0
      KN = 0
      GO TO 9
  877 KN = 1
      KM = 1
      GO TO 9
C
C*******       -BLANK CARD-        *************************************
C
  890 IF (IBCDS .NE. 0)RETURN 2
      IBCDS = 1
      CALL PAGE2 (2)
      WRITE  (NOUT,891) UWM
  891 FORMAT (A25,' 324, BLANK CARD(S) IGNORED.')
      RETURN 2
C
C*******       93- TABLEM1, 94-TABLEM2, 95-TABLEM3  ********************
C              133-TABLED1,134-TABLED2,140-TABLED3
C              162-TABDMP1, 97-TABLES1,191-TABRND1
C              357-TABLEM5
C                 (TABLEM5 IS DESIGNED FOR THERMAL COEFFICIENT WHICH IS
C                          FUNCTION OF TIME
C                          THIS PROJECT TEMPORARY HALTS HERE  6/90)
C
  920 IF (KM .NE. 0) GO TO 933
      I2 = M(1)
      ITEMS = 0
      N = 8
      IF (M(1) .LE. 0) BADDAT = .TRUE.
      IF (MF(1).NE. 1) BADFOR = .TRUE.
      I(1) = I2
      DO 925 L = 2,7
      IF (MF(L).NE.0 .AND. MF(L).NE.2) BADFOR = .TRUE.
  925 I(L) = M(L)
C
C     LOGARITHMIC SCALE
C     I(8) = 0, LINEAR-LINEAR SCALE (ALL TABLES)
C          = 1, LOG-LOG SCALE (TABLE-1 ONLY)
C          = 2, LINEAR-LOG SCALE (TABLE-1, TABLE-2 AND TABLE-3)
C          = 3, LOG-LINEAR SCALE (TABLE-1 ONLY)
C     TABLE-1 INCLUDES TABLED1, TABLEM1, TABLES1, TABDMP1 AND TABRND1
C     TABLE-2 INCLUDES TABLED2 AND TABLEM2
C
      I(8) = 0
      IF (MF(8).NE. 3) GO TO 930
      IF (M(8) .EQ. BCDLL) I(8) = 1
      IF (M(8) .EQ. BCDSL) I(8) = 2
      IF (M(8) .EQ. BCDLS) I(8) = 3
      IF (M(8).NE.BCDSL .AND. (K.EQ.94 .OR. K.EQ.95 .OR. K.EQ.134 .OR.
     1    K.EQ.140)) BADDAT = .TRUE.
C
  930 IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 932
      KN = 1
      KM = 1
      GO TO 966
  932 BADDAT = .TRUE.
      KN = 0
      KM = 0
      GO TO 966
  933 L1 = 0
      DO 955 L = 1,7,2
      IF (MF(L).EQ.3 .OR. MF(L+1).EQ.3) GO TO 937
      IF (MF(L).NE.0 .AND. MF(L).NE.2 .OR. MF(L+1).NE.0 .AND.
     1   MF(L+1).NE.2) GO TO 943
      ITEMS = ITEMS + 1
      N  = N + 2
      L1 = L1 + 2
      I(N-1) = M(L1-1)
      I(N  ) = M(L1  )
      IF (ITEMS .GT. 2) GO TO 935
      IF (ITEMS .GT. 1) GO TO 934
      X1 = Z(N-1)
      XL = X1
      GO TO 936
  934 X2  = Z(N-1)
      XL1 = XL
      XL  = X2
      ZSEQ= SIGN(1.0,X2-X1)
      IF (X2 .EQ. X1) BADDAT = .TRUE.
      GO TO 936
  935 XL1 = XL
      XL  = Z(N-1)
      ZSEQ1 = SIGN(1.0,XL-XL1)
      IF (ZSEQ1.NE.ZSEQ .AND. XL.NE.XL1) BADDAT = .TRUE.
  936 GO TO 955
  937 IF (MF(L) .EQ. 3) GO TO 938
      L1  = L1 + 1
      LP1 = L1
      KWORD1 = 0
      GO TO 939
  938 L1  = L1 + 2
      LP1 = L1 - 1
      KWORD1 = M(LP1)
  939 IF (MF(L+1) .EQ. 3) GO TO 941
      L1  = L1 + 1
      LP2 = L1
      KWORD2 = 0
      GO TO 942
  941 L1  = L1 + 2
      LP2 = L1 - 1
      KWORD2 = M(LP2)
  942 IF (KWORD1.EQ.ENDT .OR. KWORD2.EQ.ENDT) GO TO 961
      IF (KWORD1.EQ.SKIP .OR. KWORD2.EQ.SKIP) GO TO 955
      BADDAT = .TRUE.
      GO TO 956
  955 CONTINUE
      GO TO 956
  943 BADFOR = .TRUE.
      GO TO 956
  956 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 966
      KN = 0
      KM = 0
      BADDAT = .TRUE.
      GO TO 966
  961 N = N + 2
      I(N-1) = -1
      I(N  ) = -1
      IF (XL  .EQ. XL1) BADDAT = .TRUE.
      IF (ITEMS .LT. 2) BADDAT = .TRUE.
  958 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 965
      KN = 0
      KM = 0
      GO TO 966
  965 KN = 1
      KM = 1
      BADDAT = .TRUE.
  966 IF (BADDAT .OR. BADFOR) GO TO 968
      GO TO 2
  968 M(1) = I2
      GO TO 8
C
C*******      96-TABLEM4, 141-TABLED4     ******************************
C
  960 IF (KM .NE. 0) GO TO 964
      ITEMS = 0
      I2 = M(1)
      N  = 8
      IF (M(1)  .LE. 0) BADDAT = .TRUE.
      IF (MF(1) .NE. 1) BADFOR = .TRUE.
      I(1) = I2
      IF (M(3) .EQ. 0) BADDAT = .TRUE.
      DO 962 L = 2,8
      IF (MF(L).NE.0 .AND. MF(L).NE.2 .OR. L.GE.6 .AND. MF(L).NE.0)
     1   BADFOR = .TRUE.
  962 I(L) = M(L)
      I(8) = 0
      IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 963
      KN = 1
      KM = 1
      GO TO 966
  963 BADDAT = .TRUE.
      KN = 0
      KM = 0
      GO TO 966
  964 L1 = 0
      DO 967 L = 1,8
      KWORD1 = 0
      IF (MF(L) .EQ. 3) GO TO 969
      IF (MF(L).NE.0 .AND. MF(L).NE.2) GO TO 943
      N = N + 1
      ITEMS = ITEMS + 1
      L1   = L1 + 1
      I(N) = M(L1)
      GO TO 967
  969 L1 = L1 + 2
      KWORD1 = M(L1-1)
      IF (KWORD1 .EQ. ENDT) GO TO 959
      BADDAT = .TRUE.
      GO TO 956
  967 CONTINUE
      GO TO 956
  959 N = N + 1
      I(N) = -1
      IF (ITEMS .LT. 1) BADDAT = .TRUE.
      GO TO 958
C
C*****     188-TABRNDG       **************************************
C
 1000 IF (M(1) .LT. 0) GO TO 8
      IF (M(2).LT.1 .OR. M(2).GT.2) GO TO 8
      I(1) = M(1)
      I(2) = M(2)
      I(3) = M(3)
      I(4) = M(4)
      I(5) = 0
      I(6) = 0
      I(7) = 0
      I(8) = 0
      I(9) =-1
      I(10)=-1
      N    = 10
      GO TO 2
C
C******         119-DMI          ************************************
C
 1190 IF (KM .NE. 0) GO TO 8150
      IF (FPHYS) GO TO 8005
      IF (M(1).EQ.NM(1) .AND. M(2).EQ.NM(2)) GO TO 8100
      ASSIGN 8010 TO R
      GO TO 8973
 8005 IF (P(1) .GT. 1) DMIFLG = .TRUE.
      FPHYS = .FALSE.
      NM(1) = 0
      NM(2) = 0
      IF (BANDIT.NE.-1 .AND. BANDIT.NE.-2) BANDIT = +9
 8010 FLUSH = .FALSE.
      FLSHAL= .FALSE.
      EC    = .TRUE.
      SECD  = .FALSE.
      T(1)  = ISCR1
      DO 8012 L = 2,7
 8012 T(L) = 0
      IF (M(3) .NE. 0) FLUSH = .TRUE.
      ONM(1) = NM(1)
      ONM(2) = NM(2)
      IF (MF(1).NE.3 .OR. M(1).EQ.ONM(1) .AND. M(2).EQ.ONM(2))
     1    FLUSH = .TRUE.
      NM(1)  = M(1)
      NM(2)  = M(2)
      IPRINT = 0
      J0     = 0
      IF (P(1) .LE. P(2)) GO TO 8020
      FLUSH  = .TRUE.
      FLSHAL = .TRUE.
 8020 ASSIGN 8025 TO R1
      ASSIGN 8030 TO R
      GO TO 200
 8025 FLUSH = .TRUE.
 8030 IF (FLUSH) GO TO 8960
      IFO = M(4)
      TY1 = M(5)
      TY2 = M(6)
      IF (TY2.EQ.0 .AND. MOD(TY1,2).EQ.1) TY2 = TY1 + KPREC - 1
      IF (TY2.EQ.0 .AND. MOD(TY1,2).EQ.0) TY2 = TY1
      IF (MACH .NE. 12) GO TO 8033
      IF (TY2.EQ.2 .OR. TY2.EQ.4) TY2 = TY2 - 1
 8033 CONTINUE
      IF (TY1.LT.1 .OR. TY1.GT.4 .OR. TY2.LT.1 .OR. TY2.GT.4) GO TO 8950
      IF (TY1.GE.3 .AND. TY2.LE.2) WRITE (NOUT,8035) UWM,DMI,NAM(1),
     1                                               NAM(2),KNT
 8035 FORMAT (A25,' 327A, ',A4,' CARD ',2A4,', SORTED CARD COUNT =',I7,
     1       ' SPECIFYING COMPLEX DATA INPUT', /5X,
     2       'AND REAL MATRIX OUTPUT MAY NOT MAKE SENSE',/)
      NROWS = M(8)
      NCOLS = M(9)
      IF (IFO   .GT. 8) GO TO 8950
      IF (MF(6) .NE. 0) GO TO 8950
      IF (NROWS.LE.0 .OR. NCOLS.LE.0) GO TO 8950
      IF ((IFO.EQ.1 .OR. IFO.EQ.6 .OR. IFO.EQ.8) .AND. (NROWS.NE.NCOLS))
     1   GO TO 8950
      NBUF2 = 2*NBUF
      CALL OPEN (*9997,ISCR1,IBUF(NBUF2+1),1)
      CALL WRITE (ISCR1,NM,2,1)
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 8950
      IF (IFO .EQ. 8) GO TO 8040
      IF (M1(1).NE.T1(1,K) .OR. M1(2).NE.T1(2,K)) GO TO 8950
      GO TO 8960
 8040 IF (M1(1).EQ.T1(1,K) .AND. M1(2).EQ.T1(2,K) .AND.
     1    M1(3).EQ.NM(1)   .AND. M1(4).EQ.NM(2) ) GO TO 8950
      GO TO 8960
 8100 IF (.NOT.EC) GO TO 8950
      IF (FLUSH) GO TO 8960
      EC = .FALSE.
      IF (M(3) .LE. J0) GO TO 8950
 8130 J0 = J0 + 1
      IF (M(3) .EQ. J0) GO TO 8140
      CALL BLDPK (TY1,TY2,ISCR1,0,0)
      CALL BLDPKN (ISCR1,0,T)
      GO TO 8130
 8140 I0 = 1
      L1 = 4
      L1F=-1
      L2 = 9
      IF (TY1.EQ.2 .OR. TY1.EQ.4) L2 = 14
      IF (MF(3).NE.1 .OR. M(4).LT.I0) GO TO 8950
      I0  = M(4) - 1
      INT = .FALSE.
      CALL BLDPK (TY1,TY2,ISCR1,0,0)
      GO TO 8155
 8150 IF (J0.LE.0 .OR. J0.GT.NCOLS) GO TO 8950
      L1  = 1
      L1F = 0
      L2  = 8
      IF (TY1.EQ.2 .OR. TY1.EQ.4) L2 = 16
 8155 L  = L1
 8156 LF = L + L1F
      IF (FTHRU) GO TO 8192
      IF (MF(LF) .EQ. 0) GO TO 8300
      IF (MF(LF).EQ.2 .OR. MF(LF).EQ.4) GO TO 8180
      IF (MF(LF) .EQ. -32767) GO TO 8291
      IF (INT) GO TO 8950
      IF (MF(LF) .EQ. 3) GO TO 8191
      IF (MF(LF).NE.1 .OR. M(L).LT.I0 .OR. M(L).GT.NROWS) GO TO 8950
      I0  = M(L)
      INT = .TRUE.
      GO TO 8290
 8180 GO TO (8181,8182,8183,8184), TY1
C   . REAL SINGLE PRECISION
 8181 IF (MF(LF) .EQ. 4) GO TO 8950
      IF (FLUSH .OR. M(L) .EQ. 0) GO TO 8190
      A(1) = M(L)
      GO TO 8185
C   . REAL DOUBLE PRECISION
 8182 IF (MF(LF) .EQ. 2) GO TO 8950
      A(1) = M(L  )
      A(2) = M(L+1)
      L    = L + 1
      L1F  = L1F - 1
      IF (FLUSH .OR. DA(1).EQ.0.0D0) GO TO 8190
      GO TO 8185
C   . COMPLEX SINGLE PRECISION
 8183 IF (MF(LF) .EQ. 4) GO TO 8950
      IF (SECD) GO TO 8186
      A(1) = M(L)
      SECD = .TRUE.
      GO TO 8290
 8186 A(2) = M(L)
      SECD = .FALSE.
      IF (A(1).EQ.0 .AND. A(2).EQ.0 .OR. FLUSH) GO TO 8190
      GO TO 8185
C   . COMPLEX DOUBLE PRECISION
 8184 IF (MF(LF) .EQ. 2) GO TO 8950
      IF (SECD) GO TO 8187
      A(1) = M(L  )
      A(2) = M(L+1)
      L    = L + 1
      L1F  = L1F - 1
      SECD = .TRUE.
      GO TO 8290
 8187 A(3) = M(L  )
      A(4) = M(L+1)
      L    = L + 1
      L1F  = L1F - 1
      SECD = .FALSE.
      IF (FLUSH .OR. DA(1).EQ.0.0D0 .AND. DA(2).EQ.0.0D0) GO TO 8190
C
C     PACK AN ELEMENT
C
 8185 CALL ZBLPKI
 8190 INT = .FALSE.
      I0  = I0 + 1
      IF (I0 .GT. NROWS) GO TO 8300
      IF (L+1   .GT. L2) GO TO 8290
      IF (MF(LF+1) .NE. 3) GO TO 8290
      L = L + 1
 8191 IF (M(L) .NE. THRU) GO TO 8950
      FTHRU = .TRUE.
      L1F = L1F - 1
      L2  = L2  + 1
      L   = L   + 1
      IF (L .GE. L2) GO TO 8291
      L  = L + 1
      LF = L + L1F
 8192 IF (MF(LF).NE.1 .OR. M(L).LT.I0 .OR. M(L).GT.NROWS) GO TO 8950
 8193 CALL ZBLPKI
      I0 = I0 + 1
      IF (I0 .LE. M(L)) GO TO 8193
      FTHRU = .FALSE.
      IF (I0 .GT. NROWS) GO TO 8300
 8290 L = L + 1
      IF (L .LE. L2) GO TO 8156
 8291 IF (M1(1).EQ.0 .AND. M1(2).EQ.0 .OR. INT) GO TO 8960
      GO TO 8315
 8300 IF (L .EQ. L2) GO TO 8310
      LF = LF + 1
      DO 8305 LX = LF,8
      IF (MF(LX) .EQ. -32767) GO TO 8310
      IF (MF(LX) .NE. 0) GO TO 8950
 8305 CONTINUE
 8310 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 8950
 8315 IF (FLUSH) GO TO 8320
      IF (SECD ) GO TO 8950
      IF (FTHRU) GO TO 8950
      CALL BLDPKN (ISCR1,0,T)
 8320 EC = .TRUE.
      GO TO 8960
 8950 FLUSH = .TRUE.
 8960 IF (M1(1).EQ.0 .AND. M1(2).EQ.0 .OR. M1(1).EQ.T1(1,K) .AND.
     1    M1(2).EQ.T1(2,K)) GO TO 8970
      ASSIGN 8970 TO R
      GO TO 8973
 8970 N = 0
      IF (.NOT.FLUSH .OR. IPRINT.NE.0) GO TO 1226
      CALL PAGE2 (2)
      WRITE  (NOUT,8971) UFM,NM(1),NM(2),KNT
 8971 FORMAT (A23,' 325, BAD DATA OR FORMAT OR NON-UNIQUE NAME. DMI ',
     1        2A4,10X,' SORTED CARD COUNT =',I7)
      IPRINT = 1
      GO TO 1226
 8973 IF (FLSHAL) GO TO 8993
      IF (FLUSH ) GO TO 8987
      IF (IFO .EQ. 8) GO TO 8995
 8975 J0 = J0 + 1
      IF (J0 .GT. NCOLS) GO TO 8977
      CALL BLDPK  (TY1,TY2,ISCR1,0,0)
      CALL BLDPKN (ISCR1,0,T)
      GO TO 8975
 8977 IF (NCOLS .EQ. T(2)) GO TO 8978
      FLUSH = .TRUE.
      GO TO 8987
 8978 CONTINUE
      CALL CLOSE (ISCR1,1)
      CALL WRTTRL (T)
      CALL RDTRL  (T)
      IF (ICFIAT .EQ. 11) GO TO 8982
      DO 8980 LX = 1,3
 8980 T(LX+1) = ORF(LSHIFT(T(2*LX),16),T(2*LX+1))
      J = 3
      GO TO 8985
 8982 J = 6
 8985 CALL WRITE (POOL,NM,2,0)
      CALL WRITE (POOL,T(2),J,1)
      IF (L8 .NE. 0) WRITE (NOUT,8986) NM,DMI,(T(IP+1),IP=1,J)
 8986 FORMAT ('0*** DIAG  8 MESSAGE -- TRAILER FOR DATA BLOCK ',2A4,
     1        ' (VIA ',A4,' CARDS) = ',5I7,I9)
      CALL GOPEN (ISCR1,IBUF(2*NBUF+1),2)
      CALL CPYFIL (ISCR1,POOL,IBUF(3*NBUF+1),NOPEN,NWORDS)
      CALL CLOSE (ISCR1,1)
      CALL EOF (POOL)
      DMIFLG = .TRUE.
      P(1) = P(1) + 1
 8987 IP   = 3*P(3) + 4
      P(IP  ) = NM(1)
      P(IP+1) = NM(2)
      IF (FLUSH) NWORDS = 0
      P(IP+2) = ORF(LSHIFT(NWORDS/1000,16),P(1)-1)
      P(3   ) = P(3) + 1
      IF (.NOT.FLUSH) GO TO 8992
      CALL CLOSE (ISCR1,1)
      CALL EOF (POOL)
      P(1) = P(1) + 1
      CALL SKPFIL (POOL,-1)
      IF (DMIFLG) CALL EOF (POOL)
 8990 ABORT = .TRUE.
 8992 GO TO R, (8010,8970)
 8993 WRITE  (NOUT,8994) SFM,NM(1),NM(2)
 8994 FORMAT (A25,' 326, NO ROOM IN /XDPL/ FOR DMI ',2A4)
      CALL PAGE2 (2)
      GO TO 8990
 8995 T(2) = NCOLS
      GO TO 8978
 9997 CALL MESAGE (-1,ISCR1,NM)
C
C******          120-DMIG          ********************************
C
 1200 IF (.NOT.FPHYS1) GO TO 1202
      FPHYS1 = .FALSE.
      NM(1)  = 0
      NM(2)  = 0
 1202 IERR   = 0
      IF (KM   .NE. 0) GO TO 1208
      IF (M(3) .EQ. 0) GO TO 1206
      IF (M(1).NE.NM(1) .OR. M(2).NE.NM(2)) GO TO 1218
      IF (MF(2).NE.1 .OR. MF(3).NE.1 .AND. MF(3).NE.0) GO TO 1218
      IF (M(3).LE.0 .OR. M(4).LT.0 .OR. M(4).GT.6) GO TO 1218
      IF (MF(4) .NE. 0) GO TO 1218
      IF (MF(5).NE.1 .OR. MF(6).NE.1 .AND. MF(6).NE.0) GO TO 1218
      IF (M(6).LE.0 .OR. M(7).LT.0 .OR. M(7).GT.6) GO TO 1218
      IF (MF(7)+ITY1.NE.4 .AND. MF(7).NE.0) GO TO 1218
      IF ((TY1.EQ.1 .OR.  TY1.EQ.2)  .AND. MF(8).NE.0 .OR.
     1     TY1.EQ.3 .AND. MF(8).NE.2 .AND. MF(8).NE.0 .OR.
     2     TY1.EQ.4 .AND. MF(8).NE.4 .AND. MF(8).NE.0) GO TO 1218
      N = 5
      I(N-4) = M(3)
      I(N-3) = M(4)
      I(N-2) = M(6)
      I(N-1) = M(7)
      I(N  ) = M(8)
      IF (TY1 .EQ. 1) GO TO 1204
      N = 6
      I(N) = M(9)
      IF (TY1 .NE. 4) GO TO 1204
      N = 8
      I(N-1) = M(10)
      I(N  ) = M(11)
 1204 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 1230
      N = N + 2
      I(N-1) = -1
      I(N  ) = -1
      GO TO 1216
 1206 IF (MF(1).NE.3 .OR. M(1).EQ.NM(1) .AND. M(2).EQ.NM(2)) GO TO 1218
      IFO = M(4)
      TY1 = M(5)
      ITY1= 2*MOD(TY1,2)
      TY2 = M(6)
      IF (TY2.EQ.0 .AND. MOD(TY1,2).EQ.1) TY2 = TY1 + KPREC - 1
      IF (TY2.EQ.0 .AND. MOD(TY1,2).EQ.0) TY2 = TY1
      IF (MACH .NE. 12) GO TO 1207
      IF (TY2.EQ.2.OR.TY2.EQ.4) TY2 = TY2 - 1
 1207 CONTINUE
      IF (TY1.LE.0 .OR. TY1.GT.4 .OR. TY2.LE.0 .OR. TY2.GT.4) GO TO 1218
      IF (TY1.GE.3 .AND. TY2.LE.2) WRITE (NOUT,8035) UWM,DMIG,NM(1),
     1                                               NM(2),KNT
      IF (IFO.NE.1 .AND. IFO.NE.2 .AND. IFO.NE.6) GO TO 1218
      IF (TY2.EQ.1 .AND. TY1.EQ.3) GO TO 1218
      NM(1) = M(1)
      NM(2) = M(2)
      IF (MF(6).NE.0 .OR. MF(7).NE.0 .OR. MF(8).NE.0) GO TO 1220
      IF (M1F(2).NE.3 .OR. M1(3).NE.NM(1) .OR. M1(4).NE.NM(2))
     1    GO TO 1220
      M(6) = TY2
      N = 9
      GO TO 3
 1208 LF = 1
      L  = 1
 1210 IF (M(L).NE.0 .OR. M(L+1).NE.0 .OR. M(L+2).NE.0 .OR. M(L+3).NE.0)
     1    GO TO 1212
      LF = LF + 4
      L  = L  + 4
      GO TO 1214
 1212 IF (M(L).LE.0 .OR. M(L+1).LT.0 .OR. M(L+1).GT.6) GO TO 1220
      IF (MF(LF).NE.1 .OR. MF(LF+1).NE.1 .AND. MF(LF+1).NE.0) GO TO 1220
      IERR = 1
      IF (MF(LF+2)+ITY1.NE.4 .AND. MF(LF+2).NE.0) GO TO 1220
      IF (MF(LF+3).NE.0 .AND. TY1.NE.3 .AND. TY1.NE.4) GO TO 1220
      N = N + 3
      I(N-2) = M(L  )
      I(N-1) = M(L+1)
      I(N  ) = M(L+2)
      LF = LF + 4
      L  = L  + 4
      IF (TY1 .EQ. 1) GO TO 1214
      N = N + 1
      I(N) = M(L-1)
      IF (TY1 .EQ. 2) L = L + 1
      IF (TY1 .NE. 4) GO TO 1214
      N = N + 2
      I(N-1) = M(L  )
      I(N  ) = M(L+1)
      L  = L + 2
 1214 IF (LF .LE. 7) GO TO 1210
      IF (N  .LE. 0) GO TO 1220
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 1230
      N = N + 2
      I(N-1) = -1
      I(N  ) = -1
 1216 IF (M1(1).EQ.T1(1,K) .AND. M1(2).EQ.T1(2,K) .AND.
     1    M1(3).EQ.NM(1  ) .AND. M1(4).EQ.NM(2 )) GO TO 1228
      N = N + 2
      I(N-1) = -1
      I(N  ) = -1
      GO TO 1228
 1218 NM(1) = M(1)
      NM(2) = M(2)
 1220 ABORT = .TRUE.
      CALL PAGE2 (2)
      WRITE  (NOUT,1222) UFM,NM(1),NM(2),KNT
 1222 FORMAT (A23,' 327, BAD DATA OR FORMAT OR NON-UNIQUE NAME. DMIG ',
     1        2A4,10X,' SORTED CARD COUNT =',I7)
      IF (IERR .EQ. 1) WRITE (NOUT,1224)
 1224 FORMAT (5X,'INPUT MATRIX TYPE (TIN) AND INPUT DATA (XIJ OR YIJ) ',
     1       'ARE NOT CONSISTANT')
 1226 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 1230
 1228 KM = 0
      KN = 0
      GO TO 2
 1230 KN = 1
      KM = 1
      GO TO 2
C
C*******        200 - DTI       ****************************************
C
 2000 IF (KM .NE. 0) GO TO 2120
      IF (FPHYS2) GO TO 2010
      IF (M(1).EQ.NM(1) .AND. M(2).EQ.NM(2)) GO TO 2100
      ASSIGN 2020 TO R
      GO TO 2300
 2010 IF (P(1) .GT. 1) DMIFLG = .TRUE.
      FPHYS2 = .FALSE.
      NM(1)  = 0
      NM(2)  = 0
 2020 FLUSH  = .FALSE.
      FLSHAL = .FALSE.
      IF (M(3) .NE. 0) FLUSH = .TRUE.
      ONM(1) = NM(1)
      ONM(2) = NM(2)
      IF (MF(1).NE.3 .OR. M(1).EQ.ONM(1) .AND. M(2).EQ.ONM(2))
     1    FLUSH = .TRUE.
      NM(1)  = M(1)
      NM(2)  = M(2)
      IPRINT = 0
      NWORDS = 2
      J0 = 0
      IF (P(1) .LE. P(2)) GO TO 2050
      FLUSH  = .TRUE.
      FLSHAL = .TRUE.
 2050 ASSIGN 2055 TO R1
      ASSIGN 2056 TO R
      GO TO 200
 2055 FLUSH = .TRUE.
 2056 IF (FLUSH) GO TO 2195
      ITRLT = 0
      DO 2060 L = 2,7
      ITRLT = ITRLT+M(L+2)
      IF (ICFIAT.EQ.8  .AND. (M(L+2).LT.0 .OR. M(L+2).GT.65535))
     1    FLUSH = .TRUE.
C     2147483647 = 2**31-1
      IF (ICFIAT.EQ.11 .AND. (M(L+2).LT.0 .OR. M(L+2).GT.2147483647))
     1    FLUSH = .TRUE.
 2060 T(L) = M(L+2)
      IF (ITRLT .NE. 0) GO TO 2080
      DO 2070 L = 2,7
 2070 T(L) = 32767
 2080 CONTINUE
      CALL WRITE (POOL,NM,2,0)
      IF (ICFIAT .EQ. 11) GO TO 2087
      DO 2085 LX = 1,3
 2085 T(LX+1) = ORF(LSHIFT(T(2*LX),16),T(2*LX+1))
      L = 3
      GO TO 2090
 2087 L = 6
 2090 CALL WRITE (POOL,T(2),L,1)
      CALL WRITE (POOL,NM,2,0)
      IF (L8 .NE. 0) WRITE (NOUT,8986) NM,DTI,(T(IP+1),IP=1,J)
      IF (M1(1).EQ.T1(1,K) .AND. M1(2).EQ.T1(2,K))
     1   CALL WRITE (POOL,NM,0,1)
      GO TO 2200
 2100 J0 = J0 + 1
      IF (M(3) .NE. J0) GO TO 2190
      L1  = 4
      L1F =-1
      GO TO 2150
 2120 L1 = 1
      L1F= 0
 2150 L  = L1
      LF = L + L1F
 2160 IF (MF(LF).EQ.3 .AND. M(L).EQ.ENDRC1 .AND. M(L+1).EQ.ENDRC2)
     1    GO TO 2180
      IF (MF(LF) .GT. 2) L = L + 1
      L  = L  + 1
      LF = LF + 1
      IF (MF(LF) .GE. 0) GO TO 2160
      CALL WRITE (POOL,M(L1),L-L1,0)
      NWORDS = NWORDS + L - L1
      IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 2190
      GO TO 2200
 2180 CALL WRITE (POOL,M(L1),L-L1,1)
      NWORDS = NWORDS + L - L1
      IF (M1(1).NE.0 .OR. M1(2).NE.0) GO TO 2195
 2190 FLUSH = .TRUE.
 2195 IF (M1(1).EQ.0 .AND. M1(2).EQ.0 .OR.
     1    M1(1).EQ.T1(1,K) .AND. M1(2).EQ.T1(2,K)) GO TO 2200
      ASSIGN 2200 TO R
      GO TO 2300
 2200 N = 0
      IF (.NOT.FLUSH .OR. IPRINT.NE.0) GO TO 1226
      CALL PAGE2 (2)
      WRITE (NOUT,2350) UFM,NM(1),NM(2),KNT
      IPRINT = 1
      GO TO 1226
 2300 IF (FLSHAL) GO TO 2370
      IF (FLUSH ) GO TO 2330
      CALL EOF (POOL)
      DMIFLG = .TRUE.
      P(1)   = P(1) + 1
 2330 IP = 3*P(3) + 4
      P(IP  ) = NM(1)
      P(IP+1) = NM(2)
      IF (FLUSH) NWORDS = 0
      P(IP+2) = ORF(LSHIFT(NWORDS/1000,16),P(1)-1)
      P(3) = P(3) + 1
      IF (.NOT.FLUSH) GO TO 2365
      CALL PAGE2 (2)
      WRITE  (NOUT,2350) UFM,NM(1),NM(2),KNT
 2350 FORMAT (A23,' 317, BAD DATA OR FORMAT OR NON-UNIQUE NAME FOR DTI '
     1,       2A4,10X,'SORTED CARD COUNT =',I7)
      CALL EOF (POOL)
      P(1) = P(1) + 1
      CALL SKPFIL (POOL,-1)
      IF (DMIFLG) CALL SKPFIL (POOL,+1)
 2360 ABORT = .TRUE.
 2365 GO TO R, (2020,2200)
 2370 WRITE  (NOUT,2380) SFM,NM(1),NM(2)
 2380 FORMAT (A25,' 318, NO ROOM IN /XDPL/ FOR DTI ',2A4)
      CALL PAGE2 (2)
      GO TO 2360
C
C     ******************************************************************
C
C     CHECK NAME FOR UNIQUENESS AMONG DMI CARDS, DTI CARDS, ETC. AND
C     RESERVED NAMES
C
  200 CONTINUE
C
C     CHECK  FIST, FIAT, DPL FOR A NAME MATCH
C
      DO 210 II = 1,IPFIST
      IF (NM(1).EQ.IFIST(2*II+1) .AND. NM(2).EQ.BCDBLK) GO TO 250
  210 CONTINUE
      NFIAT = ICFIAT*IFIAT(2) - 2
      DO 220 II = 4,NFIAT,ICFIAT
      IF (NM(1).EQ.IFIAT(II) .AND. NM(2).EQ.IFIAT(II+1)) GO TO 250
  220 CONTINUE
      NDPL = P(3)*3 + 1
      DO 230 II = 4,NDPL,3
      IF (NM(1).EQ.P(II) .AND. NM(2).EQ.P(II+1)) GO TO 250
  230 CONTINUE
      GO TO R,  (8030,2056)
  250 GO TO R1, (8025,2055)
C
C*******      192-PLOAD4     ****************************************
C
 2900 IF (KM .EQ. 1) GO TO 2940
      KM = 1
      KN = 1
      IF (MF(1) .NE. 1) BADFOR = .TRUE.
      IF (.NOT.(MF(2).EQ.2 .AND. MF(3).EQ.1 .AND. MF(4).EQ.0 .AND.
     1     MF(5).EQ.0 .AND. MF(6).EQ.0)) GO TO 2905
C
C     SPECIAL - ALLOWING PLOAD4 TO TAKE ON PLOAD2 FORMAT
C     (PLOAD4,SID,P1,E1,blank,blank,blank,"THRU",E2) FOR QUICK INPUT
C     DATA SWITCHING.  INTERCHAGNE 2ND AND 3RD FIELDS
C
      MF(2) = 1
      MF(3) = 2
      L     = M(2)
      M(2)  = M(3)
      M(3)  = L
 2905 IF (MF(2) .NE. 1) BADFOR = .TRUE.
      DO 2910 L = 3,6
      IF (MF(L).NE.2 .AND. MF(L).NE.0) BADFOR = .TRUE.
 2910 CONTINUE
      IF (MF(7).NE.3 .AND. MF(7).NE.0 .AND.
     1   .NOT.(MF(7).EQ.1 .AND. M(7).EQ.0)) BADFOR = .TRUE.
      IF (MF(8).NE.1 .AND. MF(8).NE.0) BADFOR = .TRUE.
      IF (MF(7).EQ.0 .AND. MF(8).NE.0) BADFOR = .TRUE.
      IF (MF(7).EQ.3 .AND. MF(8).NE.1) BADFOR = .TRUE.
      IF (M(1) .LE. 0) BADDAT = .TRUE.
      IF (M(2) .LE. 0) BADDAT = .TRUE.
      IF (MF(7).EQ.3 .AND. M(7).NE.THRU) BADDAT = .TRUE.
      IF (MF(7).EQ.3 .AND. M(9).LE.   0) BADDAT = .TRUE.
      IF (MF(7).EQ.3 .AND. M(9).LE.M(2)) BADDAT = .TRUE.
      L1 = 0
      IF (MF(7) .EQ. 3) L1 = 1
      DO 2920 L = 1,6
      I(L) = M(L)
 2920 CONTINUE
      I(7) = -1
      IF (L1 .EQ. 1) I(7) = 0
      I(8) = M(L1+8)
      N = 8
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
      DO 2930 L = 9,12
      I(L) = 0
 2930 CONTINUE
      N  = 12
      KM = 0
      KN = 0
      GO TO 9
C
 2940 IF (MF(1) .GT. 1) BADFOR = .TRUE.
      DO 2950 L = 2,4
      IF (MF(L).NE.2 .AND. MF(L).NE.0) BADFOR = .TRUE.
 2950 CONTINUE
      IF (MF(1) .EQ. 0) M(1) = 0
      IF (M(1)  .LT. 0) BADDAT = .TRUE.
      DO 2960 L = 1,4
      I(L) = M(L)
 2960 CONTINUE
      N  = 4
      KM = 0
      KN = 0
      GO TO 9
C
C*******       261-CQUAD4    ****************************************
C
 3100 IF (MF(2) .EQ. 0) M(2) = M(1)
      I(1) = M(1)
      DO 3110 L = 2,6
      IF (MF(L) .NE. 1) BADFOR = .TRUE.
      IF (M(L)  .LE. 0) BADDAT = .TRUE.
      I(L) = M(L)
 3110 CONTINUE
      L1 = 6
      DO 3120 L = 11,14
      L1 = L1 + 1
      I(L1) = M(L)
 3120 CONTINUE
      IF (MF(7).NE.1 .AND. MF(7).NE.2 .AND. MF(7).NE.0) BADFOR = .TRUE.
      IF (MF(7).EQ.1 .AND. (M(7).LT.0 .OR. M(7).GE.1000000))
     1    BADDAT = .TRUE.
      I(11) = M(7)
      I(12) = 0
      IF (MF(7) .EQ. 1) I(12) = 1
      I(13) = M(8)
      N = 13
      GO TO 9
C
C*******       354-CTRIA3      **************************************
C
 3200 IF (MF(2) .EQ. 0) M(2) = M(1)
      I(1) = M(1)
      DO 3210 L = 2,5
      IF (MF(L) .NE. 1) BADFOR = .TRUE.
      IF (M(L)  .LE. 0) BADDAT = .TRUE.
 3210 I(L) = M(L)
      IF (MF(6).NE.1 .AND. MF(6).NE.2 .AND. MF(6).NE.0) BADFOR = .TRUE.
      IF (MF(6).EQ.1 .AND. (M(6).LT.0 .OR. M(6).GE.1000000))
     1    BADDAT = .TRUE.
      I( 6) = M(11)
      I( 7) = M(12)
      I( 8) = M(13)
      I( 9) = M(6)
      I(10) = 0
      I(11) = M(7)
      IF (MF(6) .EQ. 1) I(10) = 1
      N = 11
      GO TO 9
C
C*******        262-MAT8      ****************************************
C
 3300 IF (MF(2).EQ.0 .OR. MF(3).EQ.0 .OR. MF(5).EQ.0) GO TO 7
      IF (M(1) .LE. 0) GO TO 8
      IF (XM(2).EQ.0.0 .OR. XM(3).EQ.0.0) GO TO 8
      IF (XM(5).LE.0.0) GO TO 8
      IF (MF(12).EQ.2 .AND. XM(12).LE.0.0) GO TO 8
      IF (MF(14).EQ.2 .AND. XM(14).LE.0.0) GO TO 8
      IF (MF(16).EQ.2 .AND. XM(16).LE.0.0) GO TO 8
      IF (MF(13) .EQ. 0) XM(13) = XM(12)
      IF (MF(15) .EQ. 0) XM(15) = XM(14)
      N = 18
      GO TO 3
C
C*******        280-PCOMP     ****************************************
C
 4100 KN = 1
      IF (ICOMP .GT. 1) GO TO 4140
      ICOMP = 2
      IF (MF(1).NE.1) BADFOR = .TRUE.
      IF (MF(2).NE.2 .AND. MF(2).NE.0) BADFOR = .TRUE.
      IF (MF(3).NE.2 .AND. MF(3).NE.0) BADFOR = .TRUE.
      IF (MF(4).NE.2 .AND. MF(4).NE.0) BADFOR = .TRUE.
      IF (MF(5).NE.3 .AND. MF(5).NE.0) BADFOR = .TRUE.
      L = 0
      IF (MF(5).EQ.3) L = 1
      IF (MF(6).NE.0) BADFOR = .TRUE.
      IF (MF(7).NE.0) BADFOR = .TRUE.
      IF (MF(8).NE.3 .AND. MF(8).NE.0   ) BADFOR = .TRUE.
      IF (M(1).LE.0 .OR. M(1).GE.1000000) BADDAT = .TRUE.
      IF (MF(5).EQ.3 .AND. XM(4).LE.0.0 ) BADDAT = .TRUE.
      FAILUR = -1
      IF (MF(5)  .EQ. 0) FAILUR = 0
      IF (FAILUR .EQ. 0) GO TO 4120
      IF (M(5).EQ.IHILL(1) .AND. M(6).EQ.IHILL(2)) FAILUR = 1
      IF (M(5).EQ.IHOFF(1) .AND. M(6).EQ.IHOFF(2)) FAILUR = 2
      IF (M(5).EQ.ITSAI(1) .AND. M(6).EQ.ITSAI(2)) FAILUR = 3
      IF (M(5).EQ.ISTRS(1) .AND. M(6).EQ.ISTRS(2)) FAILUR = 4
      IF (M(5).EQ.ISTRN(1) .AND. M(6).EQ.ISTRN(2)) FAILUR = 5
      IF (FAILUR .EQ. -1) BADDAT = .TRUE.
 4120 LAMOPT = -1
      IF (MF(8)  .EQ. 0) LAMOPT = 0
      IF (LAMOPT .EQ. 0) GO TO 4130
      IF (M(L+8).EQ.IALL (1) .AND. M(L+9).EQ.IALL (2)) LAMOPT = 0
      IF (M(L+8).EQ.ISYM (1) .AND. M(L+9).EQ.ISYM (2)) LAMOPT = 1
      IF (M(L+8).EQ.IMEM (1) .AND. M(L+9).EQ.IMEM (2)) LAMOPT = 2
      IF (M(L+8).EQ.ISYMM(1) .AND. M(L+9).EQ.ISYMM(2)) LAMOPT = 3
      IF (LAMOPT .EQ. -1) BADDAT = .TRUE.
 4130 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 4135
      BADFOR = .TRUE.
      KN = 0
      ICOMP = 1
      N = 0
      GO TO 9
 4135 I(1) = M(1)
      I(2) = M(2)
      I(3) = M(3)
      I(4) = M(4)
      I(5) = FAILUR
      I(6) = 0
      I(7) = 0
      I(8) = LAMOPT
      N    = 8
      GO TO 9
C
 4140 N = 0
      DO 4190 L = 1,2
      L1 = 4*(L-1)
      L2 = L1
      DO 4150 L3 = 1,4
      IF (MF(L1+L3) .NE. 0) GO TO 4160
 4150 CONTINUE
      IF (L .EQ. 1) BADFOR = .TRUE.
      GO TO 4195
 4160 IF (L.EQ.2 .AND. MF(4).EQ.3) L2 = L2 + 1
      IF (ICOMP .EQ. 3) GO TO 4170
      ICOMP = 3
      IF (MF(1).NE.1) BADFOR = .TRUE.
      IF (MF(2).NE.2) BADFOR = .TRUE.
      IF (MF(3).NE.2 .AND. MF(3).NE.0) BADFOR = .TRUE.
      IF (M(1)  .LE.  0) BADDAT = .TRUE.
      IF (XM(2) .LE.0.0) BADDAT = .TRUE.
      GO TO 4180
 4170 IF (MF(L1+1).NE.1 .AND. MF(L1+1).NE.0) BADFOR = .TRUE.
      IF (MF(L1+2).NE.2 .AND. MF(L1+2).NE.0) BADFOR = .TRUE.
      IF (MF(L1+3).NE.2 .AND. MF(L1+3).NE.0) BADFOR = .TRUE.
      IF (MF(L1+1).EQ.1 .AND. M (L2+1).LE.0) BADDAT = .TRUE.
      IF (MF(L1+1) .EQ. 0) M(L2+1) = IOLD1
      IF (MF(L1+2).EQ.2 .AND. XM(L2+2).LE.0.0) BADDAT = .TRUE.
      IF (MF(L1+2) .EQ. 0) M(L2+2) = IOLD2
      IF (MF(L1+3) .EQ. 0) M(L2+3) = IOLD3
 4180 IF (MF(L1+4).NE.3 .AND. MF(L1+4).NE.0) BADFOR = .TRUE.
      IF (MF(L1+4).EQ.3 .AND. (M(L2+4).NE.IYES .AND. M(L2+4).NE.INO))
     1    BADDAT = .TRUE.
      IOUT = 0
      IF (M(L2+4) .EQ. IYES) IOUT = 1
      I(N+1) = M(L2+1)
      I(N+2) = M(L2+2)
      I(N+3) = M(L2+3)
      I(N+4) = IOUT
      IOLD1  = M(L2+1)
      IOLD2  = M(L2+2)
      IOLD3  = M(L2+3)
      N = N + 4
 4190 CONTINUE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
 4195 KN = 0
      ICOMP  = 1
      I(N+1) =-1
      N = N + 1
      GO TO 9
C
C*******        281-PCOMP1    ****************************************
C
 4300 KN = 1
      IF (ICOMP .GT. 1) GO TO 4340
      ICOMP = 2
      IF (MF(1).NE.1) BADFOR = .TRUE.
      IF (MF(2).NE.2 .AND. MF(2).NE.0) BADFOR = .TRUE.
      IF (MF(3).NE.2 .AND. MF(3).NE.0) BADFOR = .TRUE.
      IF (MF(4).NE.2 .AND. MF(4).NE.0) BADFOR = .TRUE.
      IF (MF(5).NE.3 .AND. MF(5).NE.0) BADFOR = .TRUE.
      L = 0
      IF (MF(5) .EQ. 3) L = 1
      IF (MF(6).NE.1) BADFOR = .TRUE.
      IF (MF(7).NE.2) BADFOR = .TRUE.
      IF (MF(8).NE.3 .AND. MF(8).NE.0   ) BADFOR = .TRUE.
      IF (M(1).LE.0 .OR. M(1).GE.1000000) BADDAT = .TRUE.
      IF (MF(5).EQ.3 .AND. XM(4).LE.0.0 ) BADDAT = .TRUE.
      FAILUR = -1
      IF (MF(5)  .EQ. 0) FAILUR = 0
      IF (FAILUR .EQ. 0) GO TO 4320
      IF (M(5).EQ.IHILL(1) .AND. M(6).EQ.IHILL(2)) FAILUR = 1
      IF (M(5).EQ.IHOFF(1) .AND. M(6).EQ.IHOFF(2)) FAILUR = 2
      IF (M(5).EQ.ITSAI(1) .AND. M(6).EQ.ITSAI(2)) FAILUR = 3
      IF (M(5).EQ.ISTRS(1) .AND. M(6).EQ.ISTRS(2)) FAILUR = 4
      IF (M(5).EQ.ISTRN(1) .AND. M(6).EQ.ISTRN(2)) FAILUR = 5
      IF (FAILUR .EQ. -1) BADDAT = .TRUE.
 4320 IF (M(L+6) .LE.  0) BADDAT = .TRUE.
      IF (XM(L+7).LE.0.0) BADDAT = .TRUE.
      LAMOPT = -1
      IF (MF(8)  .EQ. 0) LAMOPT = 0
      IF (LAMOPT .EQ. 0) GO TO 4330
      IF (M(L+8).EQ.IALL (1) .AND. M(L+9).EQ.IALL (2)) LAMOPT = 0
      IF (M(L+8).EQ.ISYM (1) .AND. M(L+9).EQ.ISYM (2)) LAMOPT = 1
      IF (M(L+8).EQ.IMEM (1) .AND. M(L+9).EQ.IMEM (2)) LAMOPT = 2
      IF (M(L+8).EQ.ISYMM(1) .AND. M(L+9).EQ.ISYMM(2)) LAMOPT = 3
      IF (LAMOPT .EQ. -1) BADDAT = .TRUE.
 4330 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 4335
      BADFOR = .TRUE.
      KN = 0
      ICOMP = 1
      N = 0
      GO TO 9
 4335 I(1) = M(1)
      I(2) = M(2)
      I(3) = M(3)
      I(4) = M(4)
      I(5) = FAILUR
      I(6) = M(L+6)
      I(7) = M(L+7)
      I(8) = LAMOPT
      N    = 8
      GO TO 9
C
 4340 N = 0
      DO 4390 L = 1,8
      IF (MF(L) .NE. 0) GO TO 4360
      IF (L .EQ. 1) BADFOR = .TRUE.
      GO TO 4395
 4360 IF (ICOMP .EQ. 3) GO TO 4370
      ICOMP = 3
      IF (MF(1) .NE. 2) BADFOR = .TRUE.
      GO TO 4380
 4370 IF (MF(L).NE.2 .AND. MF(L).NE.0) BADFOR = .TRUE.
      IF (MF(L) .EQ. 0) M(L) = IOLD1
 4380 I(N+1) = M(L)
      IOLD1  = M(L)
      N = N + 1
 4390 CONTINUE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
 4395 KN = 0
      ICOMP  = 1
      I(N+1) =-1
      N = N + 1
      GO TO 9
C
C*******        282-PCOMP2    ****************************************
C
 4500 KN = 1
      IF (ICOMP .GT. 1) GO TO 4540
      ICOMP = 2
      IF (MF(1) .NE. 1) BADFOR = .TRUE.
      IF (MF(2).NE.2 .AND. MF(2).NE.0) BADFOR = .TRUE.
      IF (MF(3).NE.2 .AND. MF(3).NE.0) BADFOR = .TRUE.
      IF (MF(4).NE.2 .AND. MF(4).NE.0) BADFOR = .TRUE.
      IF (MF(5).NE.3 .AND. MF(5).NE.0) BADFOR = .TRUE.
      L = 0
      IF (MF(5) .EQ. 3) L = 1
      IF (MF(6) .NE. 1) BADFOR = .TRUE.
      IF (MF(7) .NE. 0) BADFOR = .TRUE.
      IF (MF(8).NE.3 .AND. MF(8).NE.0   ) BADFOR = .TRUE.
      IF (M(1).LE.0 .OR. M(1).GE.1000000) BADDAT = .TRUE.
      IF (MF(5).EQ.3 .AND. XM(4).LE.0.0 ) BADDAT = .TRUE.
      FAILUR = -1
      IF (MF(5)  .EQ. 0) FAILUR = 0
      IF (FAILUR .EQ. 0) GO TO 4520
      IF (M(5).EQ.IHILL(1) .AND. M(6).EQ.IHILL(2)) FAILUR = 1
      IF (M(5).EQ.IHOFF(1) .AND. M(6).EQ.IHOFF(2)) FAILUR = 2
      IF (M(5).EQ.ITSAI(1) .AND. M(6).EQ.ITSAI(2)) FAILUR = 3
      IF (M(5).EQ.ISTRS(1) .AND. M(6).EQ.ISTRS(2)) FAILUR = 4
      IF (M(5).EQ.ISTRN(1) .AND. M(6).EQ.ISTRN(2)) FAILUR = 5
      IF (FAILUR .EQ. -1) BADDAT = .TRUE.
 4520 IF (M(L+6) .LE.  0) BADDAT = .TRUE.
      LAMOPT = -1
      IF (MF(8)  .EQ. 0) LAMOPT = 0
      IF (LAMOPT .EQ. 0) GO TO 4530
      IF (M(L+8).EQ.IALL (1) .AND. M(L+9).EQ.IALL (2)) LAMOPT = 0
      IF (M(L+8).EQ.ISYM (1) .AND. M(L+9).EQ.ISYM (2)) LAMOPT = 1
      IF (M(L+8).EQ.IMEM (1) .AND. M(L+9).EQ.IMEM (2)) LAMOPT = 2
      IF (M(L+8).EQ.ISYMM(1) .AND. M(L+9).EQ.ISYMM(2)) LAMOPT = 3
      IF (LAMOPT .EQ. -1) BADDAT = .TRUE.
 4530 IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 4535
      BADFOR = .TRUE.
      KN = 0
      ICOMP = 1
      N = 0
      GO TO 9
 4535 I(1) = M(1)
      I(2) = M(2)
      I(3) = M(3)
      I(4) = M(4)
      I(5) = FAILUR
      I(6) = M(L+6)
      I(7) = 0
      I(8) = LAMOPT
      N    = 8
      GO TO 9
C
 4540 N = 0
      DO 4590 L = 1,4
      L1 = 2*(L-1)
      DO 4550 L3 = 1,2
      IF (MF(L1+L3) .NE. 0) GO TO 4560
 4550 CONTINUE
      IF (L .EQ. 1) BADFOR = .TRUE.
      GO TO 4595
 4560 IF (ICOMP .EQ. 3) GO TO 4570
      ICOMP = 3
      IF (MF(1)  .NE. 2) BADFOR = .TRUE.
      IF (MF(2)  .NE. 2) BADFOR = .TRUE.
      IF (XM(1) .LE.0.0) BADDAT = .TRUE.
      GO TO 4580
 4570 IF (MF(L1+1).NE.2 .AND. MF(L1+1).NE. 0) BADFOR = .TRUE.
      IF (MF(L1+2).NE.2 .AND. MF(L1+2).NE. 0) BADFOR = .TRUE.
      IF (MF(L1+1).EQ.2 .AND. XM(L1+1).LE..0) BADDAT = .TRUE.
      IF (MF(L1+1) .EQ. 0) M(L1+1) = IOLD1
      IF (MF(L1+2) .EQ. 0) M(L1+2) = IOLD2
 4580 I(N+1) = M(L1+1)
      I(N+2) = M(L1+2)
      IOLD1  = M(L1+1)
      IOLD2  = M(L1+2)
      N = N + 2
 4590 CONTINUE
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
 4595 KN = 0
      ICOMP  = 1
      I(N+1) =-1
      N = N + 1
      GO TO 9
C
C*******        283-PSHELL    ****************************************
C
 4700 IF (KM .EQ. 1) GO TO 4740
      KM = 1
      KN = 1
      IF (MF( 1).NE.1                  ) BADFOR = .TRUE.
      IF (MF( 2).NE.1 .AND. MF( 2).NE.0) BADFOR = .TRUE.
      IF (MF( 3).NE.2 .AND. MF( 3).NE.0) BADFOR = .TRUE.
      IF (MF( 4).NE.1 .AND. MF( 4).NE.0) BADFOR = .TRUE.
      IF (MF( 5).NE.2 .AND. MF( 5).NE.0) BADFOR = .TRUE.
      IF (MF( 6).NE.1 .AND. MF( 6).NE.0) BADFOR = .TRUE.
      IF (MF( 7).NE.2 .AND. MF( 7).NE.0) BADFOR = .TRUE.
      IF (MF( 8).NE.2 .AND. MF( 8).NE.0) BADFOR = .TRUE.
      IF (M(1) .LE. 0) BADDAT = .TRUE.
      IF (MF(2).EQ.1 .AND.  M(2).LE.0) BADDAT = .TRUE.
      IF (MF(4).EQ.1 .AND.  M(4).LE.0) BADDAT = .TRUE.
      IF (MF(4).NE.0 .AND. MF(5).EQ.0) XM(5) = 1.0
      IF (MF(6).EQ.1 .AND.  M(6).LE.0) BADDAT = .TRUE.
      IF (MF(6).NE.0 .AND. MF(4).EQ.0) BADDAT = .TRUE.
      IF (MF(6).NE.0 .AND. MF(7).EQ.0) XM(7) = 0.833333
      DO 4710 L = 2,6,2
      IF (M(L).EQ.0 .AND. XM(L+1).GT.0.0) BADDAT = .TRUE.
 4710 CONTINUE
      DO 4720 L = 1,8
      I(L) = M(L)
 4720 CONTINUE
      IOLMF2 = MF(2)
      IOLMF4 = MF(4)
      IOLDM2 =  M(2)
      IOLDM4 =  M(4)
      IOLDM6 =  M(6)
      OLDXM3 = XM(3)
      N = 8
      IF (M1(1).EQ.0 .AND. M1(2).EQ.0) GO TO 9
      Z( 9) = -0.5*OLDXM3
      Z(10) =  0.5*OLDXM3
      DO 4730 L = 11,17
      I(L) = 0
 4730 CONTINUE
      N  = 17
      KM = 0
      KN = 0
      GO TO 9
C
 4740 IF (MF(1).NE.2 .AND. MF(1).NE.0) BADFOR = .TRUE.
      IF (MF(2).NE.2 .AND. MF(2).NE.0) BADFOR = .TRUE.
      IF (MF(3).NE.1 .AND. MF(3).NE.0) BADFOR = .TRUE.
      IF (MF(4).NE.1 .AND. MF(4).NE.2 .AND. MF(4).NE.0) BADFOR = .TRUE.
      IF (MF(5).NE.1 .AND. MF(5).NE.2 .AND. MF(5).NE.0) BADFOR = .TRUE.
      IF (MF(6).NE.2 .AND. MF(6).NE.0) BADFOR = .TRUE.
      IF (MF(1) .EQ. 0) XM(1) = -0.5*OLDXM3
      IF (MF(2) .EQ. 0) XM(2) =  0.5*OLDXM3
      IF (MF(3).EQ.1 .AND. M(3).LE.0) BADDAT = .TRUE.
      IF (MF(3).NE.0 .AND. (IOLMF2.EQ.0 .OR. IOLMF4.EQ.0))
     1    BADDAT = .TRUE.
      IF (MF(3).NE.0 .AND. (M(3).EQ.IOLDM2 .OR. M(3).EQ.IOLDM4))
     1    BADDAT = .TRUE.
      IF (MF(4).EQ.1 .AND. M(4).LT.0) BADDAT = .TRUE.
      IF (MF(5).EQ.1 .AND. M(5).LT.0) BADDAT = .TRUE.
      IF (IOLDM2.EQ.0 .AND. IOLDM4.EQ.0 .AND.
     1    IOLDM6.EQ.0 .AND. M(3).EQ.0) BADDAT = .TRUE.
      DO 4750 L = 1,4
      I(L) = M(L)
 4750 CONTINUE
      I(5) = 0
      IF (MF(4) .EQ. 1) I(5) = 1
C
C     I(6) IS THE INTEGRATION ORDER (SET TO 0)
C
C     NOTE
C     ----
C
C     THE INTEGRATION ORDER IS NOT USED IN THE PROGRAM,
C     BUT THIS WORD IS REQUIRED BECAUSE OF THE DESIGN
C     OF THE EST DATA FOR THE CQUAD4 ELEMENT.
C
      I(6) = 0
      I(7) = M(5)
      I(8) = 0
      IF (MF(5) .EQ. 1) I(8) = 1
      I(9) = M(6)
      N  = 9
      KM = 0
      KN = 0
      GO TO 9
C
      END
