      SUBROUTINE TABFMT
C
C     MODULE MAIN PROGRAM FOR DMAP MODULE TABPRT
C
C     THE CALL TO THIS MODULE IS
C
C     TABPRT TDB // C,N,KEY / C,N,OPT1 / C,N,OPT2 $
C
C            TDB IS THE TABLE DATA BLOCK TO BE PRINTED.
C
C            KEY IS THE BCD VALUE WHICH DETERMINES THE FORMAT BY
C                 WHICH THE TABLE IS PRINTED.
C                 THERE IS NO DEFAULT VALUE FOR KEY.
C
C            OPT1 IS A SKIP FACTOR BETWEEN DATA LINES.
C                 OPT1.EQ.0 MEANS  NO SPACE BETWEEN DATA LINES.
C                 OPT1.NE.0 MEANS ONE SPACE BETWEEN DATA LINES.
C                 THE DEFAULT VALUE FOR OPT1 IS 0
C
C            OPT2 IS ZERO BY DEFAULT.
C                 SKIP FILE-NAME AND KEY CHECKING IF OPT2 IS NON-ZERO.
C
      INTEGER         P,P2,P3,NA,R,X(14),SUBNAM(2),NAME(2),NONE(2),
     1                NAM(2),RE,F,T(7),WD,RL,Y,Z(2),EID,A,B,
     2                H1,H2,H3,HX,ZERO,ONE,TWO
      REAL            RX(14)
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /BLANK / P(2),P2,P3
      COMMON /SYSTEM/ NB,NO,JUNK1(6),NLPP,JUNK2(2),LINE
      COMMON /OUTPUT/ T1(32),T2(32),T3(32),H1(32),H2(32),H3(32)
      COMMON /TABFTX/ LA,NA(2,21)  ,  HX(32,40)  , RE(21)
      COMMON /ZZZZZZ/ IX(1)
      EQUIVALENCE     (RX(1),X(1),IX(1))
      DATA    NONE  / 4H (NO , 4HNE)  /
      DATA    SUBNAM/ 4HTABF , 4HMT   /
      DATA    F     / 101   /
      DATA    ZERO  / 4H  0 /, ONE    / 4H  1 / , TWO / 4H  2 /
C
C
    1 FORMAT (1H )
C
      LC = KORSZ(X) - NB
      IB = LC + 1
      IF (LC .LE. 0) CALL MESAGE (-8,LC,SUBNAM)
      LS = 1
      IF (P2 .NE. 0) LS = 2
C
      DO 100 I = 1,LA
      IF (P(1).EQ.NA(1,I) .AND. P(2).EQ.NA(2,I)) GO TO 200
  100 CONTINUE
      GO TO 9901
C
  200 CONTINUE
      CALL FNAME (F,NAME)
      IF (NAME(1).EQ.NONE(1) .AND. NAME(2).EQ.NONE(2)) GO TO 9902
      T(1) = F
      CALL RDTRL (T)
      IF (T(1) .LE. 0) GO TO 9902
      CALL OPEN (*9902,F,X(IB),0)
      CALL READ (*9903,*9904,F,NAM,2,RE(I),KF)
      IF (NAM(1).EQ.P(1) .AND. NAM(2).EQ.P(2)) GO TO 250
      IF (P3 .EQ. 0) GO TO 9901
  250 CONTINUE
C
      GO TO (1100,1200,1300,1400,1500,1600,1700,1800,1900,2000
     1      ,2100,2200,2300,2400,2500,2600,2700,2800,2900,3000
     2      ,3100), I
C
C     PRINT CONTENTS OF TABLE DATA BLOCK BGPDT.
C
 1100 CONTINUE
      M1 = 2
      M2 = 3
      M3 = 4
      ASSIGN 1110 TO R
      GO TO 8000
 1110 CONTINUE
      H1(19) = P(1)
      H1(20) = P(2)
      H1(24) = ONE
      IF (LC .LT. 4) GO TO 9905
      J = 0
 1120 CALL READ (*9903,*1180,F,X,4,0,KF)
      J = J + 1
      LINE = LINE + LS
      IF (LINE .LE. NLPP) GO TO 1140
      CALL PAGE
      WRITE (NO,1)
      LINE = LS
 1140 IF (P2 .NE. 0) WRITE (NO,1)
      WRITE  (NO,1150) J,X(1),(RX(L),L=2,4)
 1150 FORMAT (20X,I10,I13,1X,1P,3E20.5)
      GO TO 1120
 1180 CONTINUE
      IF (KF .EQ. 0) GO TO 9000
      GO TO 9906
C
C     PRINT CONTENTS OF TABLE DATA BLOCK GPL.
C
 1200 CONTINUE
C
C     RECORD 1
C
      M1 = 2
      M2 = 5
      M3 = 6
      ASSIGN 1205 TO R
      GO TO 8000
 1205 CONTINUE
      H1(19) = P(1)
      H1(20) = P(2)
      H1(24) = ONE
      IF (LC .LT. 5) GO TO 9905
      J = -4
 1210 CALL READ (*9903,*1230,F,X,5,0,KF)
      J = J + 5
      LINE = LINE + LS
      IF (LINE.LE.NLPP) GO TO 1220
      CALL PAGE
      WRITE (NO,1)
      LINE = LS
 1220 IF (P2 .NE. 0) WRITE (NO,1)
      WRITE  (NO,1225) J,(X(L),L=1,5)
 1225 FORMAT (11X,I8,5(9X,I8,3X))
      GO TO 1210
 1230 CONTINUE
      IF (KF .EQ. 0) GO TO 1250
      J = J + 5
      LINE = LINE + LS
      IF (LINE .LE. NLPP) GO TO 1240
      CALL PAGE
      WRITE (NO,1)
      LINE = LS
 1240 IF (P2 .NE. 0) WRITE (NO,1)
      WRITE (NO,1225) J,(X(L),L=1,KF)
C
C     RECORD 2
C
 1250 IF (I .EQ. 4) GO TO 9000
      M1 = 2
      M2 = 7
      M3 = 8
      ASSIGN 1255 TO R
      GO TO 8000
 1255 CONTINUE
      H1(19) = P(1)
      H1(20) = P(2)
      H1(24) = TWO
      IF (LC .LT. 6) GO TO 9905
      J = -2
 1260 CALL READ (*9903,*1280,F,X,6,0,KF)
      J = J + 3
      LINE = LINE + LS
      IF (LINE .LE. NLPP) GO TO 1270
      CALL PAGE
      WRITE (NO,1)
      LINE = LS
 1270 IF (P2 .NE. 0) WRITE (NO,1)
      WRITE  (NO,1275) J,(X(L),L=1,6)
 1275 FORMAT (11X,I8,2X,3(9X,I8,4X,I12))
      GO TO 1260
 1280 CONTINUE
      IF (KF .EQ. 0) GO TO 1292
      J = J + 3
      LINE = LINE + LS
      IF (LINE .LE. NLPP) GO TO 1290
      CALL PAGE
      WRITE (NO,1)
      LINE = LS
 1290 IF (P2 .NE. 0) WRITE (NO,1)
      WRITE (NO,1275) J,(X(L),L=1,KF)
C
 1292 IF (MOD(KF,2) .EQ. 0) GO TO 9000
      GO TO 9906
C
C     PRINT CONTENTS OF TABLE DATA BLOCK CSTM
C
 1300 CONTINUE
      M1 = 2
      M2 = 9
      M3 = 10
      ASSIGN 1310 TO R
      GO TO 8000
 1310 CONTINUE
      H1(19) = P(1)
      H1(20) = P(2)
      H1(24) = ONE
      IF (LC .LT. 14) GO TO 9905
      J = 0
 1320 CALL READ (*9903,*1380,F,X,14,0,KF)
      J = J + 1
      LINE = LINE + LS + 2
      IF (LINE .LE. NLPP) GO TO 1340
      CALL PAGE
      WRITE (NO,1)
      LINE = LS + 2
 1340 IF (P2 .NE. 0) WRITE (NO,1)
      WRITE (NO,1350) J,X(1),X(2),RX( 6),RX( 7),RX( 8),RX( 3)
     1                           ,RX( 9),RX(10),RX(11),RX( 4)
     2                           ,RX(12),RX(13),RX(14),RX( 5)
 1350 FORMAT( 10X,I10,I10,I10, 1P,3E20.8,10X,1P,E20.8
     1       /40X,             1P,3E20.8,10X,1P,E20.8
     2       /40X,             1P,3E20.8,10X,1P,E20.8)
      GO TO 1320
 1380 CONTINUE
      IF (KF .EQ. 0) GO TO 9000
      GO TO 9906
C
C     PRINT CONTENTS OF TABLE DATA BLOCK GPLD
C
 1400 CONTINUE
      GO TO 1200
C
C     PRINT CONTENTS OF TABLE DATA BLOCK EQEXIN
C
 1500 CONTINUE
      M1 = 2
      M2 = 11
      M3 = 12
      ASSIGN 1501 TO R
      GO TO 8000
 1501 CONTINUE
      H1(19) = P(1)
      H1(20) = P(2)
      H1(24) = ONE
 1502 IF (LC .LT. 8) GO TO 9905
      J = -3
 1510 CALL READ (*9903,*1530,F,X,8,0,KF)
      J = J + 4
      LINE = LINE + LS
      IF (LINE .LE. NLPP) GO TO 1520
      CALL PAGE
      WRITE (NO,1)
      LINE = LS
 1520 IF (P2 .NE. 0) WRITE (NO,1)
      WRITE  (NO,1525) J,(X(L),L=1,8)
 1525 FORMAT (7X,I8,4(7X,I8,6X,I8))
      GO TO 1510
 1530 CONTINUE
      IF (KF .EQ. 0) GO TO 1550
      J = J + 4
      LINE = LINE + LS
      IF (LINE .LE. NLPP) GO TO 1540
      CALL PAGE
      WRITE (NO,1)
      LINE = LS
 1540 IF (P2 .NE. 0) WRITE (NO,1)
      WRITE (NO,1525) J,(X(L),L=1,KF)
C
C     RECORD 2
C
 1550 IF (H1(24) .EQ. TWO) GO TO 9000
      M1 = 2
      M2 = 11
      M3 = 22
      ASSIGN 1555 TO R
      GO TO 8000
 1555 CONTINUE
      H1(19) = P(1)
      H1(20) = P(2)
      H1(24) = TWO
      GO TO 1502
C
C     PRINT CONTENTS OF TABLE DATA BLOCK EQDYN
C
 1600 CONTINUE
      GO TO 1500
C
C     PRINT CONTENTS OF TABLE DATA BLOCK GPDT
C
 1700 CONTINUE
      M1 = 2
      M2 = 13
      M3 = 14
      ASSIGN 1710 TO R
      GO TO 8000
 1710 CONTINUE
      H1(19) = P(1)
      H1(20) = P(2)
      H1(24) = ONE
      IF (LC .LT. 7) GO TO 9905
      J = 0
 1720 CALL READ (*9903,*1750,F,X,7,0,KF)
      J = J + 1
      LINE = LINE + LS
      IF (LINE .LE. NLPP) GO TO 1730
      CALL PAGE
      WRITE (NO,1)
      LINE = LS
 1730 IF (P2 .NE. 0) WRITE (NO,1)
      WRITE  (NO,1740) X(1),X(2),RX(3),RX(4),RX(5),X(6),X(7)
 1740 FORMAT (7X,I8,10X,I8,10X,3(1P,E12.5,5X),5X,I8,10X,I8)
      GO TO 1720
 1750 CONTINUE
      IF (KF .EQ. 0) GO TO 9000
      GO TO 9906
C
C     PRINT CONTENTS OF TABLE DATA BLOCK GPTT
C
 1800 CONTINUE
C
C     RECORD 0
C
      M1 = 2
      M2 = 15
      M3 = 16
      ASSIGN 1805 TO R
      GO TO 8000
 1805 CONTINUE
      H1(19) = P(1)
      H1(20) = P(2)
      H1(24) = ZERO
      IF ((LC/3)*3 .EQ. 0) GO TO 9905
      IVAL = (LC/3)*3
      CALL READ (*9903,*1812,F,X,IVAL,0,KF)
      GO TO 9905
 1812 WD = ((KF-1)/3) + 1
      IF (KF .EQ. 0) WD = ((LC-1)/3) + 1
      DO 1825 J = 1,WD
      LINE = LINE + LS
      IF (LINE .LE. NLPP) GO TO 1815
      CALL PAGE
      WRITE (NO,1)
      LINE = LS
 1815 IF (P2 .NE. 0) WRITE (NO,1)
      IF (X(3*J-1) .EQ. -1) GO TO 1822
      WRITE  (NO,1821) J,X(3*J-2),RX(3*J-1),X(3*J)
 1821 FORMAT (7X,I8,10X,I8,14X,1P,E12.5,19X,I8)
      GO TO 1825
 1822 WRITE  (NO,1823) J,X(3*J-2),X(3*J-1),X(3*J)
 1823 FORMAT (7X,I8,10X,I8,14X,6X,I3,22X,I8)
 1825 CONTINUE
C
C     RECORD 1 AND ALL OTHERS
C
      M1 = 17
      M2 = 1
      M3 = 18
      ASSIGN 1835 TO R
      GO TO 8000
 1835 CONTINUE
      DO 1895 RL = 1,J
      IF (X(3*RL)   .EQ. 0) GO TO 1895
      IF ((LC-3*WD) .LT. 4) GO TO 9905
      CALL READ (*9903,*9904,F,Y,1,0,KF)
 1840 CALL READ (*9903,*1895,F,Z,2,0,KF)
 1845 CALL READ (*9903,*9904,F,EID,1,0,KF)
C
C     ELEMENT ID EQUALS ZERO INDICATES THE END OF DATA FOR CURRENT TYPE
C
      IF ((LC-3*WD) .LT. Z(2)) GO TO 9905
C
C     ELEMENT ID LESS THAN ZERO INDICATES NONEXISTENT TEMPERATURE VALUES
C
      IF (EID) 1848,1840,1847
 1847 IVAL = 3*WD + 1
      CALL READ (*9903,*9904,F,X(IVAL),Z(2),0,KF)
 1848 A = 3*WD + 1
      B = A + 7
      IF (B .GE. (A+Z(2))) B = A + Z(2) - 1
      LINE = LINE + LS
      IF (LINE .LE. NLPP) GO TO 1865
      CALL PAGE
      WRITE  (NO,1850) X(3*RL),Y
 1850 FORMAT (9X,I8,11X,I8,11X,I8,13X,I8)
      WRITE  (NO,1860)
 1860 FORMAT (14H0   ELEMENT ID,8X,5H( 1 ),9X,5H( 2 ),9X,5H( 3 ),9X,
     1                             5H( 4 ),9X,5H( 5 ),9X,5H( 6 ),9X,
     2                             5H( 7 ),9X,5H( 8 ) )
      WRITE (NO,1)
      LINE = LS + 3
 1865 IF (EID) 1866,1840,1867
 1866 WRITE (NO,1870) EID
      GO TO 1845
 1867 WRITE  (NO,1870) EID,(RX(L),L=A,B)
 1870 FORMAT (4X,I8,3X,8(2X,1P,E12.5) )
      IF (P2 .NE. 0) WRITE (NO,1)
      IF (B .EQ. (A+Z(2)-1)) GO TO 1845
      A = A + 8
 1875 B = A + 7
      IF (B .GE. (A+Z(2))) B = A + Z(2) - 1
      LINE = LINE + LS
      IF (LINE .LE. NLPP) GO TO 1880
      CALL PAGE
      WRITE (NO,1850) X(3*RL),Y
      WRITE (NO,1)
      LINE = LS + 3
 1880 WRITE  (NO,1885) (RX(L),L=A,B)
 1885 FORMAT (17X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5,
     1         2X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5,2X,1P,E12.5)
      IF (P2 .NE. 0) WRITE (NO,1)
      IF (B .EQ. (A+Z(2)-1)) GO TO 1845
      A = A + 8
      GO TO 1875
 1895 CONTINUE
      IF (KF .EQ. 0) GO TO 9000
      GO TO 9906
C
C     PRINT CONTENTS OF TABLE DATA BLOCK GPCT
C
 1900 CONTINUE
      M1 = 19
      M2 = 20
      M3 = 21
      ASSIGN 1910 TO R
      GO TO 8000
 1910 CONTINUE
      IF (LC .LT. 12) GO TO 9905
      J = 0
 1920 CALL READ (*1990,*9904,F,PI,1,0,KF)
      J  = J + 1
      WD = 10
      LINE = LINE + LS
      IF (LINE .LE. NLPP) GO TO 1930
      CALL PAGE
      WRITE (NO,1)
      LINE = LS
 1930 IF (P2 .NE. 0) WRITE (NO,1)
      CALL READ (*9903,*1980,F,M,1,0,KF)
      CALL READ (*9903,*1940,F,X,10,0,KF)
      GO TO 1945
 1940 WD = KF
 1945 WRITE  (NO,1950) J,PI,M,(X(L),L=1,WD)
 1950 FORMAT (3X,I8,12(2X,I8))
      IF (M .LE. 10) GO TO 1920
 1960 LINE = LINE + LS
      IF (LINE .LE. NLPP) GO TO 1965
      CALL PAGE
      WRITE (NO,1)
      LINE = LS
 1965 IF (P2 .NE. 0) WRITE (NO,1)
      CALL READ (*9903,*1975,F,X,10,0,KF)
      WRITE  (NO,1970) (X(L),L=1,WD)
 1970 FORMAT (31X,10(2X,I8))
      GO TO 1960
 1975 WD = KF
      WRITE (NO,1970) (X(L),L=1,WD)
      GO TO 1920
 1980 M = 0
      WRITE (NO,1950) PI,M
      GO TO 1920
 1990 IF (J .EQ. 0) GO TO 9903
      GO TO 9000
C
C     PRINT CONTENTS OF
C
 2000 CONTINUE
      GO TO 9000
C
 2100 CONTINUE
      GO TO 9000
C
 2200 CONTINUE
      GO TO 9000
C
 2300 CONTINUE
      GO TO 9000
C
 2400 CONTINUE
      GO TO 9000
C
 2500 CONTINUE
      GO TO 9000
C
 2600 CONTINUE
      GO TO 9000
C
 2700 CONTINUE
      GO TO 9000
C
 2800 CONTINUE
      GO TO 9000
C
 2900 CONTINUE
      GO TO 9000
C
 3000 CONTINUE
      GO TO 9000
C
 3100 CONTINUE
      GO TO 9000
C
C
C     INTERNAL ROUTINE TO SET HEADINGS AND INITIALIZE LINE COUNTER.
C     -------------------------------------------------------------
C
 8000 CONTINUE
      DO 8010 M = 1,32
      H1(M) = HX(M,M1)
      H2(M) = HX(M,M2)
      H3(M) = HX(M,M3)
 8010 CONTINUE
      LINE = NLPP
      GO TO R, (1110,1205,1255,1310,1501,1555,1710,1805,1835,1910)
C
C
C     PRINT TRAILER OF TABLE DATA BLOCK
C     ---------------------------------
C
 9000 CONTINUE
      WRITE  (NO,9010) (T(L),L=2,7)
 9010 FORMAT (15H0*** TRAILER = ,6I18)
      GO TO 9999
C
C
 9901 WRITE  (NO,9951) UWM,P
 9951 FORMAT (A25,' 2094, SUBROUTINE TABFMT, KEYNAME ',2A4,
     1       ' NOT IN LIST OF AVAILABLE KEYNAMES.')
      GO TO 9993
C
 9902 WRITE  (NO,9952) UWM
 9952 FORMAT (A25,' 2095, SUBROUTINE TABFMT, PURGED INPUT.')
      GO TO 9995
C
 9903 WRITE  (NO,9953) UWM
 9953 FORMAT (A25,' 2096, SUBROUTINE TABFMT, EOF ENCOUNTERED.')
      GO TO 9995
C
 9904 WRITE  (NO,9954) UWM
 9954 FORMAT (A25,' 2097, SUBROUTINE TABFMT, EOR ENCOUNTERED.')
      GO TO 9995
C
 9905 WRITE  (NO,9955) UWM
 9955 FORMAT (A25,' 2098, SUBROUTINE TABFMT, INSUFFICIENT CORE.')
      GO TO 9995
C
 9906 WRITE  (NO,9956) UWM,KF
 9956 FORMAT (A25,' 2099, SUBROUTINE TABFMT, KF =',I10)
      GO TO 9995
C
 9993 WRITE  (NO,9994) (NA(1,L),NA(2,L),L=1,LA)
 9994 FORMAT ('0*** LIST OF RECOGNIZED KEYNAMES FOLLOWS...', /(20X,2A4))
C
 9995 CONTINUE
C
C     DO NOT CALL PEXIT SINCE THIS IS AN OUTPUT PROCESSOR.
C
 9999 CALL CLOSE (F,1)
      RETURN
C
      END
