      SUBROUTINE MPYQ (Z      )
C
C     MPYQ IS CALLED ONCE PER EXECUTION OF MPYAD. IT PERFORMS GENERAL
C     INITIALIZATION FOR EACH OF THE ENTRY POINTS
C     I.E.  SETTING UP MPYAD GO-TO-BRANCHES, ARITH AND BPICK FOR METHOD
C           1, AND ARITH2, APICK2 AND BPICK2 FOR METHOD 2
C
C     ENTRY POINTS -
C           MPY1V   (PERFORMS THE INNER LOOP FOR MPYAD, METHOD 1,
C                    TRANSPOSE AND NON-TRANSPOSE. IT IS CALLED ONCE FOR
C                    EACH COLUMNN OF THE A MATRIX)
C           MPY2NV  (PERFORMS THE INNER LOOP FOR THE NON-TRANSPOSE CASE
C                    OF METHOD 2. IT IS CALLED ONCE FOR EACH COUMN OF
C                    THE B MATRIX)
C           MPY2TV  (SAME AS MPY2NV, EXECPT IT IS FOR THE TRANSPOSE
C                    CASE)
C           MPY3T   (PERFORMS THE INNER LOOP FOR THE TRANSPOSE CASE OF
C                    METHOD 3. IT IS CALLED ONCE FOR EACH COLUMN OF THE
C                    B MATRIX)
C     (WHERE V STANDS FOR VAX VERSION, AND T IS THE TRANSPOSE FLAG)
C
C     THE MPYi ROUTINES PERFORM THE MATRIX MULTIPLICATION AND ADDITION
C     FOR THE MPYAD INNER LOOPS
C
C           (+/-)A * B (+/-)C = D   OR (+/-)A(T) * B (+/-)C = D
C
C
C     LAST REVISED BY G.CHAN/UNISYS  1/91
C     (1) MPY3T WAS PREVIOUSLY A .MDS SUBROUTINE. IT IS NOW AN ENTRY
C         POINT IN THIS MPYQ ROUTINE
C         (MPY3T IS AN ENTRY POINT IN MPYQ, IN IBM AND CDC VERSIONS)
C     (2) TO IMPROVE MPYAD INNER LOOP LOGIC FOR THE COMMON CASES
C
C
      IMPLICIT INTEGER (A-Z)
      REAL             A(4)    ,B(4)    ,D(4)    ,Z(1)    ,AA(4) ,AAA  ,
     1                 BSR     ,AAS(1)  ,DDS(1)  ,BBB     ,BSI   ,BBS
      DOUBLE PRECISION AD(2)   ,BD(2)   ,DD(2)   ,ZD(1)   ,ADD(2),BDR  ,
     1                 BDI     ,AAD(1)  ,DDD(1)  ,BBD(1)
      DIMENSION        ZZ(1)
      COMMON /MPYADX/  FILEA(7),FILEB(7),FILEC(7),FILED(7),NZ    ,T    ,
     1                 SIGNAB  ,SIGNC   ,PREC1   ,SCRTCH  ,TIME
     2       /SYSTEM/  KSYSTM(65)
     3       /TYPE  /  PRC(2)  ,NWDS(4) ,RC(4)
     4       /NAMES /  RD      ,RDREW   ,WRT     ,WRTREW  ,CLSREW,CLS
     5       /ZBLPKX/  D       ,DROW
     6       /ZNTPKX/  A       ,I       ,EOL     ,EOR
     7       /PACKX /  TYPED   ,TYPD1   ,ONE1    ,PP1     ,INCR1
     8       /UNPAKX/  TYPEBD  ,ONE2    ,PP2     ,INCR2
      COMMON /MPYADZ/  RCB     ,RCD     ,LL      ,LLL     ,JB    ,NBX  ,
     1                 NDX     ,JMAX1X  ,ACOL    ,ACOL1   ,ACOLN ,ACORE,
     2                 APOINT  ,BCOL    ,CROW    ,FIRSTL  ,NA    ,NB   ,
     3                 ND      ,NWDA    ,NWDB    ,NWDD    ,PREC  ,JMAX ,
     4                 INCRA   ,BLOCK(20)
      COMMON /MPYQT4/  RCA     ,PRCA    ,ALL4    ,JUMP4   ,PREC4
      COMMON /ZZZZZZ/  BBS(17000)
      EQUIVALENCE      (KSYSTM( 1),SYSBUF)  ,(KSYSTM( 2),MOUT )  ,
     1                 (KSYSTM(58),IPASS )
      EQUIVALENCE      (A(1)    ,AD(1)   )  ,(B(1)      ,BD(1))  ,
     1                 (D(1)    ,DD(1)   )  ,(FILEA(2),M      )  ,
     2                 (FILEA(3),N       )  ,(FILEA(5),TYPEA  )  ,
     3                 (FILEB(2),Q       )  ,(FILEB(3),R      )  ,
     4                 (FILEB(5),TYPEB   )  ,(FILEC(5),TYPEC  )  ,
     5                 (FILED(5),TYPD    )  ,(NZZ     ,BUF1   )  ,
     6                 (ACOLN   ,AROWN   )  ,(AA(1)   ,ADD(1) )  ,
     7                 (ACOL1   ,AROW1   )  ,(ACOL    ,AROW   )  ,
     8                 (BBS(1)  ,BBD(1)  )
      EQUIVALENCE      (BLOCK(2),TYPE    )  ,(BLOCK(3),FORM   )  ,
     1                 (BLOCK(4),ROW, J3 )  ,(BLOCK(5),POINT  )  ,
     2                 (BLOCK(6),NBRSTR  )  ,(BLOCK(8),FLAG   )
      EQUIVALENCE      (BLOCK(5),JB31    )  ,(BLOCK(6),NTERM3 )  ,
     1                 (BLOCK(7),JB3N    )  ,(BLOCK(8),B3FLAG )
C     DATA    MASK6F / X'00FFFFFF'  /
      DATA    MASK6F / 16777215 /
C
C     MASK6F= '00FFFFFF'X (OR X'00FFFFFF') = 16777215
C     RCB   = 1 IF B IS REAL,2 IF B IS COMPLEX
C     NWDB  = NUMBER OF WORDS PER ELEMENT OF B
C     NBX   = NUMBER OF ELEMENTS PER COLUMN OF B
C     NB    = NUMBER OF WORDS PER COLUMN OF B
C     NDX   = NUMBER OF ELEMENTS PER COLUMN OF C AND D
C     ND    = NUMBER OF WORDS PER COLUMN OF C AND D
C     NZZ   = BUF1 = POINTER TO FIRST GINO BUFFER
C     BUF2  = POINTER TO SECOND GINO BUFFER
C     BUF3  = POINTER TO THIRD GINO BUFFER
C     JJ    = MAX. NO. OF COLUMNS OF B AND D THAT MAY BE HELD IN CORE
C     MPASS1= NUMBER OF PASSES REQUIRED FOR METHOD ONE
C     JZB   = POINTER TO FIRST ELEMENT OF B FOR SP REFERENCE
C     JZDB  = POINTER TO FIRST ELEMENT OF B FOR DP REFERENCE
C     JB    = POINTER TO FIRST ELEMENT OF B FOR PRECISION OF PROBLEM
C     NWDA  = NUMBER OF WORDS PER ELEMENT OF A
C     NWDD  = NUMBER OF WORDS PER ELEMENT OF D
C     ACORE = POINTER TO FIRST WORD FOR STORAGE OF PACKED COLUMNS
C             OF A MATRIX FOR METHOD TWO
C*****
      RCA  = RC(TYPEA)
      MODB = MOD(TYPEB,2)
      MODA = MOD(TYPEA,2)
      PRCA = PRC(TYPEA)
      FA3  = FILEA(3)
C
C     IF DIAG 43 IS ON, SKIP ALL (1991) SPEED IMPROVEMENT LOGIC
C     (THIS IS ONLY TEMPORARY)
C
      ALL  = 0
      IF (TYPEA.EQ.TYPEB .AND. TYPEA.EQ.TYPED) ALL = TYPEA
      ALL4 = ALL
      IF (ALL4 .EQ. 0) ALL4 = 5
      IF (TYPED.GE.3 .AND. TYPEA.LE.2 .AND. TYPEB.LE.2) ALL4 = 6
      CALL SSWTCH (43,J)
      IF (J .EQ. 1) ALL = 0
      JUMP4 = TYPEB + (TYPEA-1)*4
      PREC4 = PREC1 - 1
C
C     RCA, PRCA, ALL4 AND JUMP4 ARE USED IN MPY4T
C
      GO TO (20,40,100,300), TYPED
C
C     REAL SINGLE PRECISION
C
   20 ASSIGN 840 TO ARITH
      ASSIGN 750 TO BPICK
      IF (T .NE. 0)  GO TO 30
C
      ASSIGN 1210 TO ARITH2
      ASSIGN 1010 TO BPICK2
      ASSIGN 1100 TO APICK2
      GO TO 600
C
   30 ASSIGN 1600 TO ARITH2
      ASSIGN 1400 TO BPICK2
      ASSIGN 1500 TO APICK2
      GO TO 600
C
C     REAL DOUBLE PRECISION
C
   40 ASSIGN 850 TO ARITH
      ASSIGN 770 TO BPICK
      IF (MODB .EQ. 1) ASSIGN 790 TO BPICK
      IF (T .NE. 0) GO TO 60
C
      ASSIGN 1220 TO ARITH2
      ASSIGN 1030 TO BPICK2
      IF (MODB .EQ. 1) ASSIGN 1050 TO BPICK2
      ASSIGN 1120 TO APICK2
   50 IF (MODA .EQ. 1) ASSIGN 1140 TO APICK2
      GO TO 600
C
   60 ASSIGN 1610 TO ARITH2
      ASSIGN 1420 TO BPICK2
      IF (MODB .EQ. 1) ASSIGN 1440 TO BPICK2
      ASSIGN 1520 TO APICK2
   70 IF (MODA .EQ. 1) ASSIGN 1540 TO APICK2
      GO TO 600
C
C     COMPLEX SINGLE PRECISION
C
  100 ASSIGN 860 TO ARITH
      GO TO (110,110,120,130), TYPEB
  110 ASSIGN 750 TO BPICK
      GO TO 140
  120 ASSIGN 760 TO BPICK
      GO TO 140
  130 ASSIGN 810 TO BPICK
  140 IF (T .NE. 0)  GO TO 220
C
      ASSIGN 1230 TO ARITH2
      GO TO (150,150,160,170), TYPEB
  150 ASSIGN 1010 TO BPICK2
      GO TO 180
  160 ASSIGN 1020 TO BPICK2
      GO TO 180
  170 ASSIGN 1070 TO BPICK2
  180 GO TO (190,190,200,210), TYPEA
  190 ASSIGN 1100 TO APICK2
      GO TO 600
  200 ASSIGN 1110 TO APICK2
      GO TO 600
  210 ASSIGN 1160 TO APICK2
      GO TO 600
  220 ASSIGN 1620 TO ARITH2
C
      GO TO (230,230,240,250), TYPEB
  230 ASSIGN 1400 TO BPICK2
      GO TO 260
  240 ASSIGN 1410 TO BPICK2
      GO TO 260
  250 ASSIGN 1460 TO BPICK2
  260 GO TO (270,270,280,290), TYPEA
  270 ASSIGN 1500 TO APICK2
      GO TO 600
  280 ASSIGN 1510 TO APICK2
      GO TO 600
  290 ASSIGN 1560 TO APICK2
      GO TO 600
C
C     COMPLEX DOUBLE PRECISION
C
  300 ASSIGN 870 TO ARITH
      GO TO (310,320,330,340), TYPEB
  310 ASSIGN 790 TO BPICK
      GO TO 350
  320 ASSIGN 770 TO BPICK
      GO TO 350
  330 ASSIGN 800 TO BPICK
      GO TO 350
  340 ASSIGN 780 TO BPICK
  350 IF (T .NE. 0) GO TO 440
C
      ASSIGN 1240 TO ARITH2
      GO TO (360,370,380,390), TYPEB
  360 ASSIGN 1050 TO BPICK2
      GO TO 400
  370 ASSIGN 1030 TO BPICK2
      GO TO 400
  380 ASSIGN 1060 TO BPICK2
      GO TO 400
  390 ASSIGN 1040 TO BPICK2
  400 GO TO (50,410,420,430), TYPEA
  410 ASSIGN 1120 TO APICK2
      GO TO 600
  420 ASSIGN 1150 TO APICK2
      GO TO 600
  430 ASSIGN 1130 TO APICK2
      GO TO 600
C
  440 ASSIGN 1630 TO ARITH2
      GO TO (450,460,470,480), TYPEB
  450 ASSIGN 1440 TO BPICK2
      GO TO 490
  460 ASSIGN 1420 TO BPICK2
      GO TO 490
  470 ASSIGN 1450 TO BPICK2
      GO TO 490
  480 ASSIGN 1430 TO BPICK2
  490 GO TO (70,500,510,520), TYPEA
  500 ASSIGN 1520 TO APICK2
      GO TO 600
  510 ASSIGN 1550 TO APICK2
      GO TO 600
  520 ASSIGN 1530 TO APICK2
C
C     MPYQ INITIALIZATION DONE
C
  600 RETURN
C
C
      ENTRY MPY1V (ZZ      ,Z      ,ZD      )
C     =====================
C
C     METHOD 1  (TRANSPOSE AND NON-TRANSPOSE)
C
  700 B (2) = 0.
      BD(2) = 0.D0
  710 CALL ZNTPKI
      I1 = I - 1
      IF (T) 730,720,730
  720 K1 = LL
      K2 = I1*RCD + 1
      GO TO 740
  730 K1 = I1*RCB + JB
      K2 = LLL
  740 K3 = K2 + JMAX1X
      IF (ALL .NE. 0) GO TO (900,920,940,960), ALL
      DO 880 K = K2,K3,NDX
      J  = K1
      GO TO BPICK, (750,760,770,780,790,800,810)
  750 IF (Z(J) .EQ. 0.0) GO TO 880
      B(1) = Z(J)
      GO TO 830
  760 IF (Z(J).EQ.0.0 .AND. Z(J+1).EQ.0.0) GO TO 880
      B(1) = Z(J  )
      B(2) = Z(J+1)
      GO TO 830
  770 IF (ZD(J) .EQ. 0.D0) GO TO 880
      BD(1) = ZD(J)
      GO TO 830
  780 IF (ZD(J).EQ.0.D0 .AND. ZD(J+1).EQ.0.D0) GO TO 880
      BD(1) = ZD(J  )
      BD(2) = ZD(J+1)
      GO TO 830
  790 IF (Z(J) .EQ. 0.0) GO TO 880
      BD(1) = Z(J)
      GO TO 830
  800 IF (Z(J).EQ.0.0 .AND. Z(J+1).EQ.0.0) GO TO 880
      BD(1) = Z(J  )
      BD(2) = Z(J+1)
      GO TO 830
  810 IF (ZD(J).EQ.0.D0 .AND. ZD(J+1).EQ.0.D0) GO TO 880
      B(1) = ZD(J  )
      B(2) = ZD(J+1)
C
  830 GO TO ARITH, (840,850,860,870)
  840 Z(K) = Z(K) + A(1)*B(1)
      GO TO 880
  850 ZD(K) = ZD(K) + AD(1)*BD(1)
      GO TO 880
  860 Z(K  ) = Z(K  ) + A(1)*B(1) - A(2)*B(2)
      Z(K+1) = Z(K+1) + A(1)*B(2) + A(2)*B(1)
      GO TO 880
  870 ZD(K  ) = ZD(K  ) + AD(1)*BD(1) - AD(2)*BD(2)
      ZD(K+1) = ZD(K+1) + AD(1)*BD(2) + AD(2)*BD(1)
  880 K1 = K1 + NBX
      IF (EOL) 980,710,980
C
C     COMMON CASES (TYPEA=TYPEB=TYPED=PREC)
C
C     PREC=1, ARITH(840) AND BPICK(750)
C     PREC=2, ARITH(850) AND BPICK(770)
C     PREC=3, ARITH(860) AND BPICK(760)
C     PREC=4, ARITH(870) AND BPICK(780)
C
  900 DO 910 K = K2,K3,NDX
      Z(K) = Z(K) + A(1)*Z(K1)
  910 K1 = K1 + NBX
      IF (EOL) 980,710,980
  920 DO 930 K = K2,K3,NDX
      ZD(K) = ZD(K) + AD(1)*ZD(K1)
  930 K1 = K1 + NBX
      IF (EOL) 980,710,980
  940 DO 950 K = K2,K3,NDX
      Z(K  ) = Z(K  ) + A(1)*Z(K1  ) - A(2)*Z(K1+1)
      Z(K+1) = Z(K+1) + A(1)*Z(K1+1) + A(2)*Z(K1  )
  950 K1 = K1 + NBX
      IF (EOL) 980,710,980
  960 DO 970 K = K2,K3,NDX
      ZD(K  ) = ZD(K  ) + AD(1)*ZD(K1  ) - AD(2)*ZD(K1+1)
      ZD(K+1) = ZD(K+1) + AD(1)*ZD(K1+1) + AD(2)*ZD(K1  )
  970 K1 = K1 + NBX
      IF (EOL) 980,710,980
  980 RETURN
C
C
      ENTRY MPY2NV (ZZ      ,Z      ,ZD      )
C     ======================
C
C     METHOD 2 NON-TRANSPOSE CASE
C
      B(2)  = 0.
      BD(2) = 0.D0
      AA(2) = 0.
      ADD(2)= 0.D0
      L     = FIRSTL
      ACOL  = ACOL1
 1000 CALL ZNTPKI
      IF (I.LT.ACOL1 .OR. I.GT.ACOLN .OR. I.LT.ACOL) GO TO 1290
      L     = L - 2*(I-ACOL)
      ACOL  = I
      APOINT= ZZ(L)
      IF (APOINT .EQ. 0) GO TO 1280
      NBR   = ZZ(L-1)
      IF (ALL .NE. 0) GO TO (1260,1265,1270,1275), ALL
      GO TO BPICK2, (1010,1020,1030,1040,1050,1060,1070)
 1010 B(1)  = A(1)
      GO TO 1090
 1020 B(1)  = A(1)
      B(2)  = A(2)
      GO TO 1090
 1030 BD(1) = AD(1)
      GO TO 1090
 1040 BD(1) = AD(1)
      BD(2) = AD(2)
      GO TO 1090
 1050 BD(1) = A(1)
      GO TO 1090
 1060 BD(1) = A(1)
      BD(2) = A(2)
      GO TO 1090
 1070 B(1)  = AD(1)
      B(2)  = AD(2)
C
 1090 NBRSTR = ZZ( APOINT+1 )
      INIT   = ZZ( APOINT )
      APOINT = APOINT + 2
      J      = APOINT
      IF ( PRCA .EQ. 2 ) J = J/2 + 1
      APOINT = APOINT+ NBRSTR*NWDA
      IROW   = INIT*RCD - RCD + 1
      NROW   = IROW + NBRSTR*RCD - 1
      DO 1250 K = IROW,NROW,RCD
      GO TO APICK2, (1100,1110,1120,1130,1140,1150,1160)
 1100 AA(1) = Z(J)
      GO TO 1200
 1110 AA(1) = Z(J  )
      AA(2) = Z(J+1)
      GO TO 1200
 1120 ADD(1)= ZD(J)
      GO TO 1200
 1130 ADD(1)= ZD(J  )
      ADD(2)= ZD(J+1)
      GO TO 1200
 1140 ADD(1)= Z(J)
      GO TO 1200
 1150 ADD(1)= Z(J  )
      ADD(2)= Z(J+1)
      GO TO 1200
 1160 AA(1) = ZD(J  )
      AA(2) = ZD(J+1)
C
 1200 GO TO ARITH2, (1210,1220,1230,1240)
 1210 Z(K)  = Z(K) + AA(1)*B(1)
      GO TO 1250
 1220 ZD(K) = ZD(K) + ADD(1)*BD(1)
      GO TO 1250
 1230 Z(K  )= Z(K  ) + AA(1)*B(1) - AA(2)*B(2)
      Z(K+1)= Z(K+1) + AA(1)*B(2) + AA(2)*B(1)
      GO TO 1250
 1240 ZD(K  ) = ZD(K  ) + ADD(1)*BD(1) - ADD(2)*BD(2)
      ZD(K+1) = ZD(K+1) + ADD(1)*BD(2) + ADD(2)*BD(1)
 1250 J = J + RCA
      NBR  = NBR - 1
      IF (NBR) 1280,1280,1090
C
C     COMMON CASES (TYPEA=TYPEB=TYPED=PREC)
C
C     PREC=1, ARITH2(1210), APICK2(1100) AND BPICK2(1010)
C     PREC=2, ARITH2(1220), APICK2(1120) AND BPICK2(1030)
C     PREC=3, ARITH2(1230), APICK2(1110) AND BPICK2(1020)
C     PREC=4, ARITH2(1620), APICK2(1510) AND BPICK2(1410)
C
 1260 NBRSTR = ZZ( APOINT+1 )
      INIT   = ZZ( APOINT )
      APOINT = APOINT + 2
      J      = APOINT
      IF ( PRCA .EQ. 2 ) J = J/2 + 1
      APOINT = APOINT+ NBRSTR*NWDA
      IROW  = INIT*RCD - RCD + 1
      NROW  = IROW + NBRSTR*RCD - 1
      DO 1262 K = IROW,NROW,RCD
      Z(K)  = Z(K) + Z(J)*A(1)
 1262 J     = J + RCA
      NBR   = NBR - 1
      IF (NBR) 1280,1280,1260
C
 1265 NBRSTR = ZZ( APOINT+1 )
      INIT   = ZZ( APOINT )
      APOINT = APOINT + 2
      J      = APOINT
      IF ( PRCA .EQ. 2 ) J = J/2 + 1
      APOINT = APOINT+ NBRSTR*NWDA
      IROW  = INIT*RCD - RCD + 1
      NROW  = IROW + NBRSTR*RCD - 1
      DO 1267 K = IROW,NROW,RCD
      ZD(K) = ZD(K) + ZD(J)*AD(1)
 1267 J     = J + RCA
      NBR   = NBR - 1
      IF (NBR) 1280,1280,1265
C
 1270 NBRSTR = ZZ( APOINT+1 )
      INIT   = ZZ( APOINT )
      APOINT = APOINT + 2
      J      = APOINT
      IF ( PRCA .EQ. 2 ) J = J/2 + 1
      APOINT = APOINT+ NBRSTR*NWDA
      IROW  = INIT*RCD - RCD + 1
      NROW  = IROW + NBRSTR*RCD - 1
      DO 1272 K = IROW,NROW,RCD
      Z(K  )  = Z(K  ) + Z(J)*A(1) - Z(J+1)*A(2)
      Z(K+1)  = Z(K+1) + Z(J)*A(2) + Z(J+1)*A(1)
 1272 J     = J + RCA
      NBR   = NBR - 1
      IF (NBR) 1280,1280,1270
C
 1275 NBRSTR = ZZ( APOINT+1 )
      INIT   = ZZ( APOINT )
      APOINT = APOINT + 2
      J      = APOINT
      IF ( PRCA .EQ. 2 ) J = J/2 + 1
      APOINT = APOINT+ NBRSTR*NWDA
      IROW  = INIT*RCD - RCD + 1
      NROW  = IROW + NBRSTR*RCD - 1
      DO 1277 K = IROW,NROW,RCD
      ZD(K  ) = ZD(K  ) + ZD(J)*AD(1) - ZD(J+1)*AD(2)
      ZD(K+1) = ZD(K+1) + ZD(J)*AD(2) + ZD(J+1)*AD(1)
 1277 J     = J + RCA
      NBR   = NBR - 1
      IF (NBR) 1280,1280,1275
C
 1280 L = L - 2
      ACOL = ACOL + 1
 1290 IF (EOL .EQ. 0) GO TO 1000
      RETURN
C
C
      ENTRY MPY2TV (ZZ      ,Z      ,ZD      )
C     ======================
C
C     METHOD 2 - TRANSPOSE CASE
C
C     COMMENTS FROM G.CHAN/UNISYS      1/91
C     OBSERVE THAT THERE IS NO DO-LOOP IN THIS MPY2TV LOGIC. IT IS
C     THEREFORE CONCLUDED THAT THE TRANSPOSE CASE WOULD TAKE MUCH MORE
C     TIME THAN THE NON-TRANSPOSE CASE
C
      B(2)  = 0.
      BD(2) = 0.D0
      AA(2) = 0.
      ADD(2)= 0.D0
      DD(1) = 0.D0
      DD(2) = 0.D0
      L     = FIRSTL
      APOINT= ZZ(L)
      AROW  = AROW1
      IF (CROW .EQ. MASK6F) GO TO 1350
      GO TO 1330
 1300 APOINT = ZZ(L)
      IF (CROW-AROW) 1320,1340,1350
 1310 DROW = CROW
      CALL ZBLPKI
 1320 IF (EOL .NE. 0) GO TO 1350
 1330 CALL ZNTPKI
      CROW  = I
 1340 DD(1) = AD(1)
      DD(2) = AD(2)
      IF (CROW-AROW) 1310,1360,1350
 1350 DD(1) = 0.D0
      DD(2) = 0.D0
      IF (APOINT .EQ. 0) GO TO 1690
 1360 DROW  = AROW
      IF (APOINT .EQ. 0) GO TO 1680
      NBRSTR= ZZ(L-1)
 1370 NBR   = ZZ( APOINT+1 )
      NBR1  = NBR
      INIT  = ZZ( APOINT )
      APOINT = APOINT + 2
      J = APOINT
      IF ( PRCA .GT. 1 ) J = J/2 + 1
      APOINT = APOINT + NBR*NWDA
      K     = (INIT-1)*RCB + 1
 1380 IF (ALL .NE. 0) GO TO (1640,1645,1650,1655), ALL
      GO TO BPICK2, (1400,1410,1420,1430,1440,1450,1460)
 1400 B(1)  = Z(K)
      GO TO 1470
 1410 B(1)  = Z(K  )
      B(2)  = Z(K+1)
      GO TO 1470
 1420 BD(1) = ZD(K)
      GO TO 1470
 1430 BD(1) = ZD(K  )
      BD(2) = ZD(K+1)
      GO TO 1470
 1440 BD(1) = Z(K)
      GO TO 1470
 1450 BD(1) = Z(K  )
      BD(2) = Z(K+1)
      GO TO 1470
 1460 B(1)  = ZD(K  )
      B(2)  = ZD(K+1)
C
 1470 GO TO APICK2, (1500,1510,1520,1530,1540,1550,1560)
 1500 AA(1) = Z(J)
      GO TO 1570
 1510 AA(1) = Z(J  )
      AA(2) = Z(J+1)
      GO TO 1570
 1520 ADD(1)= ZD(J)
      GO TO 1570
 1530 ADD(1)= ZD(J  )
      ADD(2)= ZD(J+1)
      GO TO 1570
 1540 ADD(1)= Z(J)
      GO TO 1570
 1550 ADD(1)= Z(J  )
      ADD(2)= Z(J+1)
      GO TO 1570
 1560 AA(1) = Z(J  )
      AA(2) = Z(J+2)
C
 1570 GO TO ARITH2, (1600,1610,1620,1630)
 1600 D(1)  = D(1) + AA(1)*B(1)
      GO TO 1660
 1610 DD(1) = DD(1) + ADD(1)*BD(1)
      GO TO 1660
 1620 D(1)  = D(1) + AA(1)*B(1) - AA(2)*B(2)
      D(2)  = D(2) + AA(1)*B(2) + AA(2)*B(1)
      GO TO 1660
 1630 DD(1) = DD(1) + ADD(1)*BD(1) - ADD(2)*BD(2)
      DD(2) = DD(2) + ADD(1)*BD(2) + ADD(2)*BD(1)
      GO TO 1660
C
C     COMMON CASES (TYPEA=TYPEB=TYPED=PREC)
C
C     PREC=1, ARITH2(1600), APICK2(1500) AND BPICK2(1400)
C     PREC=2, ARITH2(1610), APICK2(1520) AND BPICK2(1420)
C     PREC=3, ARITH2(1620), APICK2(1510) AND BPICK2(1410)
C     PREC=4, ARITH2(1630), APICK2(1530) AND BPICK2(1430)
C
 1640 D(1) = D(1) + Z(J)*Z(K)
      J = J + RCA
      K = K + RCB
      NBR = NBR - 1
      IF (NBR) 1670,1670,1640
C
 1645 DD(1) = DD(1) + ZD(J)*ZD(K)
      J = J + RCA
      K = K + RCB
      NBR = NBR - 1
      IF (NBR) 1670,1670,1645
C
 1650 D(1) = D(1) + Z(J)*Z(K  ) - Z(J+1)*Z(K+1)
      D(2) = D(2) + Z(J)*Z(K+1) + Z(J+1)*Z(K  )
      J = J + RCA
      K = K + RCB
      NBR = NBR - 1
      IF (NBR) 1670,1670,1650
C
 1655 DD(1) = DD(1) + ZD(J)*ZD(K  ) - ZD(J+1)*ZD(K+1)
      DD(2) = DD(2) + ZD(J)*ZD(K+1) + ZD(J+1)*ZD(K  )
      J = J + RCA
      K = K + RCB
      NBR = NBR - 1
      IF (NBR) 1670,1670,1655
C
 1660 J = J + RCA
      K = K + RCB
      NBR = NBR - 1
      IF (NBR .GT. 0) GO TO 1380
 1670 NBRSTR = NBRSTR - 1
      IF (NBRSTR .GT. 0) GO TO 1370
 1680 CALL ZBLPKI
 1690 L = L - 2
      AROW = AROW + 1
      IF (AROW .LE. AROWN) GO TO 1300
      RETURN
C
C
      ENTRY MPY3T (*,AAS      ,AAD      ,DDS      ,DDD      )
C     ===============================
C
C     METHOD 3 (TRANSPOSE ONLY)
C
      B3FLAG = -1
      CALL GETSTR (*2400,BLOCK)
CIBMNB 6/93
      IF ( BLOCK( 2 ) .EQ. TYPEB ) GO TO 1699
      TYPEB = BLOCK( 2 )
      RCB   = RC( TYPEB )
      ALL   = 0
 1699 CONTINUE
CIBMNE 6/93
      IF (ALL .NE. 0) GO TO (1760,1920,2060,2270), ALL
      GO TO (1700,1800,2000,2100), TYPED
C
C     PERFORM ARITHMETIC IN REAL SINGLE PRECISION
C
 1700 JB3N = JB31 + NTERM3 - 1
      DO 1740 JB3 = JB31,JB3N
      K = J3
      BBB = BBS(JB3)
      IF (BLOCK(2) .EQ. 2) BBB = BBD(JB3)
      IF (TYPEA    .NE. 2) GO TO 1720
      DO 1710 I = AROW1,AROWN
      AAA = AAD(K)
      DDS(I) = DDS(I) + AAA*BBB
 1710 K  = K + NA
      GO TO 1740
 1720 DO 1730 I = AROW1,AROWN
      DDS(I) = DDS(I) + AAS(K)*BBB
 1730 K  = K + NA
 1740 J3 = J3 + 1
      CALL ENDGET (BLOCK)
      CALL GETSTR (*2500,BLOCK)
      GO TO 1700
C
C     COMMON CASE (TYPEA=TYPEB=TYPED=PREC=1)
C
 1760 JB3N = JB31 + NTERM3 - 1
      DO 1780 JB3 = JB31,JB3N
      K  = J3
      DO 1770 I = AROW1,AROWN
      DDS(I) = DDS(I) + AAS(K)*BBS(JB3)
 1770 K  = K  + NA
 1780 J3 = J3 + 1
      CALL ENDGET (BLOCK)
      CALL GETSTR (*2500,BLOCK)
      GO TO 1760
C
C     PERFORM ARITHMETIC IN REAL DOUBLE PRECISION
C
 1800 K1 = 2*(PRCA-1) + PRC(TYPEB)
 1810 JB3N = JB31 + NTERM3 - 1
      DO 1900 JB3 = JB31,JB3N
      K = J3
      GO TO (1820,1840,1860,1880), K1
 1820 DO 1830 I = AROW1,AROWN
      DDD(I) = DDD(I) + AAS(K)*BBS(JB3)
 1830 K = K + FA3
      GO TO 1900
 1840 DO 1850 I = AROW1,AROWN
      DDD(I) = DDD(I) + AAS(K)*BBD(JB3)
 1850 K = K + FA3
      GO TO 1900
 1860 DO 1870 I = AROW1,AROWN
      DDD(I) = DDD(I) + AAD(K)*BBS(JB3)
 1870 K = K + FA3
      GO TO 1900
 1880 DO 1890 I = AROW1,AROWN
      DDD(I) = DDD(I) + AAD(K)*BBD(JB3)
 1890 K = K + FA3
 1900 J3 = J3 + 1
      CALL ENDGET (BLOCK)
      CALL GETSTR (*2500,BLOCK)
      GO TO 1810
C
C     COMMON CASE (TYPEA=TYPEB=TYPED=PREC=2)
C
 1920 JB3N = JB31 + NTERM3 - 1
      DO 1940 JB3 = JB31,JB3N
      K = J3
      DO 1930 I = AROW1,AROWN
      DDD(I) = DDD(I) + AAD(K)*BBD(JB3)
 1930 K = K  + FA3
 1940 J3 = J3 + 1
      CALL ENDGET (BLOCK)
      CALL GETSTR (*2500,BLOCK)
      GO TO 1920
C
C     PERFORM ARITHMETIC IN COMPLEX SINGLE PRECISION
C
 2000 BSI  = 0.
      I1   = 2*AROW1 - 1
      IN   = 2*AROWN - 1
 2010 IF (RCA .EQ. 2) J3 = 2*J3 - 1
      JB3N = JB31 + RCB*NTERM3 - RCB
      DO 2050 JB3 = JB31,JB3N,RCB
      BSR = BBS(JB3)
      IF (RCB .EQ. 2) BSI = BBS(JB3+1)
      K = J3
      IF (RCA .EQ. 2) GO TO 2030
      DO 2020 I = I1,IN,2
      DDS(I  ) = DDS(I  ) + AAS(K)*BSR
      DDS(I+1) = DDS(I+1) + AAS(K)*BSI
 2020 K = K + NA
      GO TO 2050
 2030 DO 2040 I = I1,IN,2
      DDS(I  ) = DDS(I  ) + AAS(K)*BSR - AAS(K+1)*BSI
      DDS(I+1) = DDS(I+1) + AAS(K)*BSI + AAS(K+1)*BSR
 2040 K = K + NA
 2050 J3 = J3 + RCA
      CALL ENDGET (BLOCK)
      CALL GETSTR (*2500,BLOCK)
      GO TO 2010
C
C     COMMON CASE (TYPEA=TYPEB=TYPED=PREC=3)
C
 2060 I1 = 2*AROW1 - 1
      IN = 2*AROWN - 1
 2070 J3 = 2*J3 - 1
      JB3N = JB31 + RCB*NTERM3 - RCB
      DO 2090 JB3 = JB31,JB3N,RCB
      K = J3
      DO 2080 I = I1,IN,2
      DDS(I  ) = DDS(I  ) + AAS(K)*BBS(JB3  ) - AAS(K+1)*BBS(JB3+1)
      DDS(I+1) = DDS(I+1) + AAS(K)*BBS(JB3+1) + AAS(K+1)*BBS(JB3  )
 2080 K = K + NA
 2090 J3 = J3 + RCA
      CALL ENDGET (BLOCK)
      CALL GETSTR (*2500,BLOCK)
      GO TO 2070
C
C     PERFORM ARITHMETIC IN COMPLEX DOUBLE PRECISION
C
 2100 BDI = 0.
      INCA= RCA*FA3
      I1  = 2*AROW1 - 1
      IN  = 2*AROWN - 1
 2110 IF (RCA .EQ. 2) J3 = 2*J3 - 1
      JB3N = JB31 + RCB*NTERM3 - RCB
      DO 2260 JB3 = JB31,JB3N,RCB
      K = J3
      GO TO (2120,2130,2140,2150), TYPEB
 2120 BDR = BBS(JB3)
      GO TO 2160
 2130 BDR = BBD(JB3)
      GO TO 2160
 2140 BDR = BBS(JB3  )
      BDI = BBS(JB3+1)
      GO TO 2160
 2150 BDR = BBD(JB3  )
      BDI = BBD(JB3+1)
 2160 GO TO (2170,2190,2210,2230), TYPEA
 2170 DO 2180 I = I1,IN,2
      DDD(I  ) = DDD(I  ) + AAS(K)*BDR
      DDD(I+1) = DDD(I+1) + AAS(K)*BDI
 2180 K = K + INCA
      GO TO 2250
 2190 DO 2200 I = I1,IN,2
      DDD(I  ) = DDD(I  ) + AAD(K)*BDR
      DDD(I+1) = DDD(I+1) + AAD(K)*BDI
 2200 K = K + INCA
      GO TO 2250
 2210 DO 2220 I = I1,IN,2
      DDD(I  ) = DDD(I  ) + AAS(K)*BDR - AAS(K+1)*BDI
      DDD(I+1) = DDD(I+1) + AAS(K)*BDI + AAS(K+1)*BDR
 2220 K = K + INCA
      GO TO 2250
 2230 DO 2240 I = I1,IN,2
      DDD(I  ) = DDD(I  ) + AAD(K)*BDR - AAD(K+1)*BDI
      DDD(I+1) = DDD(I+1) + AAD(K)*BDI + AAD(K+1)*BDR
 2240 K = K + INCA
 2250 J3  = J3 + RCA
 2260 CONTINUE
      CALL ENDGET (BLOCK)
      CALL GETSTR (*2500,BLOCK)
      GO TO 2110
C
C     COMMON CASE (TYPEA=TYPEB=TYPED=PREC=4)
C
 2270 INCA= RCA*FA3
      I1  = 2*AROW1 - 1
      IN  = 2*AROWN - 1
 2280 J3 = 2*J3 - 1
      JB3N = JB31 + RCB*NTERM3 - RCB
      DO 2300 JB3 = JB31,JB3N,RCB
      K = J3
      DO 2290 I = I1,IN,2
      DDD(I  ) = DDD(I  ) + AAD(K)*BBD(JB3  ) - AAD(K+1)*BBD(JB3+1)
      DDD(I+1) = DDD(I+1) + AAD(K)*BBD(JB3+1) + AAD(K+1)*BBD(JB3  )
 2290 K = K + INCA
 2300 J3  = J3 + RCA
      CALL ENDGET (BLOCK)
      CALL GETSTR (*2500,BLOCK)
      GO TO 2280
C
 2400 RETURN 1
 2500 RETURN
      END
