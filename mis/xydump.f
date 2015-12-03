      SUBROUTINE XYDUMP (OUTFIL,TYPE)
C
      LOGICAL         PUNCH      ,PLOT       ,PRINT      ,OUTOPN    ,
     1                NULL       ,RANDOM     ,ONES       ,PAPLOT    ,
     2                ON         ,OK         ,INTORE     ,DEC
      INTEGER         ONEONE(2)  ,EOR        ,OUTFIL     ,TCURVE    ,
     1                XAXIS      ,YAXIS      ,YTAXIS     ,YBAXIS    ,
     2                CURVE      ,YCYCLE(2)  ,XCYCLE     ,CENTER    ,
     3                Z          ,LIMIT(2,3) ,STEPS      ,VECTOR    ,
     4                FILE       ,SUBC       ,VECID      ,BUF       ,
     5                BEGIN      ,TYPE       ,TWO1
      REAL            YMIN(2)    ,YMAX(2)    ,VALUE(60)  ,RZ(1)     ,
     1                RBUF(100)  ,IDOUTR(300),YLIMIT(2,3)
      COMMON /MACHIN/ MACHX
      COMMON /BLANK / BLKCOM     ,VARI(3)    ,NFRAME     ,NCARD
      COMMON /TWO   / TWO1(32)
      COMMON /ZZZZZZ/ Z(1)
      COMMON /XYWORK/ FILE       ,TCURVE(32) ,NTOPS      ,PRINT     ,
     1                IFILE      ,XAXIS(32)  ,NBOTS      ,PLOT      ,
     2                VECTOR     ,YAXIS(32)  ,VECID(5)   ,PUNCH     ,
     3                MAJOR      ,YTAXIS(32) ,SUBC(5)    ,CENTER    ,
     4                RANDOM     ,YBAXIS(32) ,IDIN(153)  ,BUF(100)  ,
     5                IVALUE(60) ,IAT        ,IDOUT(300) ,OUTOPN    ,
     6                STEPS      ,NAT        ,PAPLOT
      EQUIVALENCE     (LIMIT(1,1),YLIMIT(1,1)) ,(Z(1),RZ(1)) ,
     1                (BUF(1),RBUF(1)) ,(IDOUT(1),IDOUTR(1)) ,
     2                (IVALUE(1),VALUE(1))
      DATA   ONEONE / 1,1 /, EOR/ 1 /, NOEOR/ 0 /
      DATA   NPAPLT / 0   /
C
C     SET MINIMUM X-DIFFERENCE
C
C     BUT FIRST CONVERT X FROM INTERGER TO REAL IF NECESSARY.
C
      INTORE = .FALSE.
      DEC = MACHX.EQ.5 .OR. MACHX.EQ.6 .OR. MACHX.EQ.21
      J   = 1
      IS1 = STEPS - 1
C
C     NOW SEARCH LIST FOR FIRST NON-ZERO ENTRY
C
   10 IF (Z(IAT+J) .NE. 0) GO TO 20
      J = J + 1
      IF (J .GT. IS1) GO TO 50
      GO TO 10
C
C              UNIVAC             CDC             CRAY
   20 IF (MACHX.EQ.3 .OR. MACHX.EQ.4 .OR. MACHX.EQ.12)
     1   IF (IABS(Z(IAT+J))-TWO1(2)) 40,40,50
C
C     IBM, VAX, UNIX
C
      IF (.NOT.DEC .AND. IABS(Z(IAT+J)).GT.TWO1(9)) GO TO 50
      IF (     DEC .AND. (Z(IAT+J).LT.1 .OR. Z(IAT+J).GT.127)) GO TO 50
   40 INTORE = .TRUE.
      IF (J .EQ. 1) RZ(IAT+J) = Z(IAT+J)
C
   50 OK = .FALSE.
      DO 70 I = 1,IS1
      J = IAT + I
      IF (INTORE) RZ(J+1) = Z(J+1)
      DIFF = RZ(J+1) - RZ(J)
      IF (.NOT.OK) GO TO 60
      IF (DIFF .EQ. 0.0) GO TO 70
      XINC = AMIN1(XINC,DIFF)
      GO TO 70
   60 IF (DIFF .EQ. 0.0) GO TO 70
      XINC = DIFF
      OK   = .TRUE.
   70 CONTINUE
      IF (.NOT.OK) XINC = 1.0
C
C     SET XMIN AND XMAX FOR ALL DATA
C
      XCYCLE    = 0
      YCYCLE(1) = 0
      YCYCLE(2) = 0
      XMIN = RZ(IAT+1)
      J    = IAT + STEPS
      XMAX = RZ(J)
C
C     REDUCE THESE LIMITS TO USER SPECIFIED LIMITS
C
      IF (IVALUE(1) .NE. 1) XMIN = (VALUE(1))
      IF (IVALUE(2) .NE. 1) XMAX = (VALUE(2))
C
C     FURTHER EXPAND XLIMITS TO INCLUDE Y-AXIS INTERCEPT
C
      IF (IVALUE(9) .EQ. 1) GO TO 80
      IF (IVALUE(36).EQ.1 .AND. VALUE(9).LE.0.0) GO TO 90
      XMIN = AMIN1(XMIN,VALUE(9))
      XMAX = AMAX1(XMAX,VALUE(9))
C
C     IF X-DIRECTION IS LOG AND XMIN IS NEGATIVE OR ZERO, SET YMIN
C     EQUAL TO THE SMALLEST NON-ZERO POSITIVE VALUE
C
   80 IF (IVALUE(36) .NE. 1) GO TO 130
   90 IF (XMIN    .GT.  0.0) GO TO 120
      DO 100 I = 1,STEPS
      J = IAT + I
      IF (RZ(J) .GT. 0.0) GO TO 110
  100 CONTINUE
      XMIN = 1.0
      XMAX = 10.
      GO TO 120
  110 XMIN = RZ(J)
  120 CALL XYLOG (XMIN,XMAX,XCYCLE)
C
C     SWITCH XMIN AND XMAX (SAFETY CHECK) IF NECESSARY
C
  130 IF (XMIN .LE. XMAX) GO TO 140
      TEMP = XMIN
      XMIN = XMAX
      XMAX = TEMP
C
C     USING XMIN AND XMAX AS LIMITS DETERMINE Y-LIMITS FOR TOP AND
C     BOTTOM.
C
C     I1 = FIRST STEP WITHIN XMIN TO XMAX
C     I2 = LAST  STEP WITHIN XMIN TO XMAX
C
C     FIRST FIND I1 AND I2
C
  140 DO 150 I = 1,STEPS
      J = IAT + I
      IF (XMIN.LE.RZ(J) .AND. RZ(J).LE.XMAX) GO TO 160
  150 CONTINUE
      I1 = 0
      GO TO 180
  160 I1 = I
      J  = IAT + STEPS + 1
      DO 170 I = 1,STEPS
      J = J - 1
      IF (XMIN.LE.RZ(J) .AND. RZ(J).LE.XMAX) GO TO 190
  170 CONTINUE
  180 I2 = 0
      GO TO 200
  190 I2 = J - IAT
  200 IF (I1 .NE. 0) GO TO 210
C
C     FIND FOLLOWING VALUES FOR CURVES AS A GROUP
C
C    YLIMIT(1,1)=YMIN TOP, YLIMIT(1,2)=YMAX TOP, YLIMIT(1,3)=MIN POS TOP
C    YLIMIT(2,1)=YMIN BOT, YLIMIT(2,2)=YMAX BOT, YLIMIT(2,3)=MIN POS BOT
C
      YMIN(1) = 0.0
      YMIN(2) = 0.0
      YMAX(1) = 10.
      YMAX(2) = 10.
      GO TO 330
  210 M = 1
      IF (NBOTS .NE. 0) M = 2
      BEGIN = IAT
      DO 280 I = 1,M
      LIMIT(I,1) = 1
      LIMIT(I,2) = 1
      LIMIT(I,3) = 1
      DO 260 J = 1,NTOPS
      K  = J*STEPS + BEGIN
      J1 = K + I1
      J2 = K + I2
      IF (LIMIT(I,1) .NE. 1) GO TO 240
C
C     FIND FIRST NON-INTEGER 1 VALUE
C
      DO 220 K = J1,J2
      IF (Z(K) .NE. 1) GO TO 230
  220 CONTINUE
      GO TO 260
  230 YLIMIT(I,1) = RZ(K)
      YLIMIT(I,2) = RZ(K)
  240 DO 250 K = J1,J2
      IF (Z(K) .EQ. 1) GO TO 250
      YLIMIT(I,1) = AMIN1(RZ(K),YLIMIT(I,1))
      YLIMIT(I,2) = AMAX1(RZ(K),YLIMIT(I,2))
      IF (RZ(K) .LE. 0.0) GO TO 250
      IF (LIMIT(I,3) .EQ. 1) YLIMIT(I,3) = RZ(K)
      YLIMIT(I,3) = AMIN1(YLIMIT(I,3),RZ(K))
  250 CONTINUE
  260 CONTINUE
      BEGIN = CENTER
C
C     DEFAULT YLIMITS IF ALL CURVES NULL
C
      IF (LIMIT(I,1) .NE. 1) GO TO 270
      YLIMIT(I,1) = 0.0
      YLIMIT(I,2) = 100.
  270 IF (LIMIT(I,3) .EQ. 1) YLIMIT(I,3) = 10.0
C
  280 CONTINUE
C
C     SET FINAL Y-LIMITS FOR UPPER AND LOWER CURVES
C
C
C     K=1 IMPLIES WHOLE CURVES
C     K=2 IMPLIES UPPER AND LOWER CURVES
C
      K = 1
      IF (NBOTS .GT. 0) K = 2
      DO 320 I = 1,K
      YMIN(I) = YLIMIT(I,1)
      YMAX(I) = YLIMIT(I,2)
C
C     REDUCE THESE CURVE LIMITS TO LIMITS SET BY USER
C
      ITEMP = 2*(I+K)
      IF (IVALUE(ITEMP-1) .NE. 1) YMIN(I) = (VALUE(ITEMP-1))
      IF (IVALUE(ITEMP  ) .NE. 1) YMAX(I) = (VALUE(ITEMP  ))
C
C     FURTHER EXPAND LIMITS TO INCLUDE X-AXIS
C
      ITEMP = I + K
      IF (IVALUE(ITEMP+8) .EQ. 1) GO TO 290
      IF (IVALUE(ITEMP+35).EQ.1 .AND. VALUE(ITEMP+8).LE.0.E0) GO TO 300
      YMIN(I) = AMIN1(YMIN(I),VALUE(ITEMP+8))
      YMAX(I) = AMAX1(YMAX(I),VALUE(ITEMP+8))
C
C     IF Y-DIRECTION IS LOG AND YMIN IS NEGATIVE OR ZERO SET YMIN
C     EQUAL TO SMALLEST POSITIVE CURVE VALUE WITHIN XLIMITS
C
  290 IF (IVALUE(ITEMP+35) .NE. 1) GO TO 310
  300 IF (YMIN(I) .LE. 0.0) YMIN(I) = YLIMIT(I,3)
      CALL XYLOG (YMIN(I),YMAX(I),YCYCLE(I))
C
C     SWITCH YMIN AND YMAX (SAFETY CHECK) IF NECESSARY
C
  310 IF (YMIN(I) .LE. YMAX(I)) GO TO 320
      TEMP    = YMIN(I)
      YMIN(I) = YMAX(I)
      YMAX(I) = TEMP
  320 CONTINUE
C
C     ALL CURVE LIMITS HAVE NOW BEEN SET FOR THIS FRAME
C
C
C     OUTPUT EACH CURVE AND AN IDOUT RECORD IF PLOTS = .TRUE.
C
C     FILL IDOUT
C
  330 DO 340 I = 1,300
  340 IDOUT(I) = 0
      IF (PLOT .AND. OUTOPN) NFRAME = NFRAME + 1
      IDOUT(1) = SUBC(FILE)
      IDOUT(2) = NFRAME
      IDOUT(6) = VECTOR
      IDOUT(9) = IVALUE(45)
      IF (IVALUE(43) .EQ. 0) VALUE(43) = 1.0
      IDOUT(43)  = IVALUE(43)
      IDOUTR(10) = XINC
      IDOUT(245) = TYPE
      IDOUT(246) = STEPS
      IDOUTR(282)= VALUE(57)
      IF (IDOUTR(282) .LT. 1.0) IDOUTR(282) = 1.0
      IDOUT(283) = IVALUE(50)
      IF (IVALUE(47) .EQ. 3) IDOUT(283) = IVALUE(41)
      IDOUT(284) = IVALUE(47)
      IDOUT(285) = IVALUE(48)
      IDOUT(286) = IVALUE(49)
      IDOUT(287) = IVALUE(46)
      IDOUT( 44) = IVALUE(58)
      IDOUT( 45) = IVALUE(59)
      IF (PRINT) IDOUT(288) = 1
      IF (PLOT ) IDOUT(289) = 1
      IF (.NOT.PAPLOT) GO TO 350
      IF (.NOT.PLOT) IDOUT(289) = -1
      IF (     PLOT) IDOUT(289) =  2
      NPAPLT = NPAPLT+1
      IDOUT(281) = NPAPLT
  350 ON = .FALSE.
      IF (PLOT .OR. PAPLOT) ON = .TRUE.
      IF (PUNCH) IDOUT(290) = 1
      DO 360 I = 51,146
  360 IDOUT(I) = IDIN(I)
C
C     BRANCH ON TOP, BOTTOM, OR WHOLE CURVE (FIRST WILL BE TOP OR WHOLE)
C
      I = 3
      IF (Z(I).EQ.0 .OR. RANDOM) GO TO 400
C
C     TOP CURVE ID
C
      CURVE = 0
      IDOUT(7) = 1
      IDOUT(8) = 1
      IDOUTR(11) = XMIN
      IDOUTR(12) = XMAX
      IDOUTR(13) = YMIN(1)
      IDOUTR(14) = YMAX(1)
      IFLAG = 0
      IF (INTORE) IFLAG = 1
      CALL XYTICS (IDOUT(15),IDOUTR(15),IVALUE(17),IDOUT(11) ,
     1             IDOUT(12),IVALUE(21),XCYCLE,IFLAG)
      CALL XYTICS (IDOUT(23),IDOUTR(23),IVALUE(19),IDOUT(13),
     1             IDOUT(14),IVALUE(23),YCYCLE(1),0)
      IDOUT(31) = IVALUE(34) + IVALUE(25)
      IDOUT(32) = IVALUE(34) + IVALUE(26)
      IDOUT(33) = IVALUE(34) + IVALUE(29)
      IDOUT(34) = IVALUE(34) + IVALUE(30)
      IDOUT(35) = XCYCLE
      IDOUT(36) = YCYCLE(1)
      IDOUT(37) = IVALUE(15)
      IDOUT(38) = IVALUE(11)
      IF (IDOUT(38)  .EQ. 1) IDOUTR(38) = 0.0
      IF (IDOUTR(38) .LT. YMIN(1)) IDOUT(37) = 0
      IDOUT(39) = IVALUE(14)
      IDOUT(40) = IVALUE( 9)
      IF (IDOUT(40)  .EQ. 1) IDOUTR(40) = 0.0
      IF (IDOUTR(40) .LT. XMIN) IDOUT(39) = 0
      IDOUT(41) = IVALUE(40)
      IDOUT(243) = IVALUE(53)
      IDOUT(244) = IVALUE(54)
      DO 370 I=1,32
      IDOUT(I+146) = TCURVE(I)
      IDOUT(I+178) = XAXIS(I)
      IDOUT(I+210) = YTAXIS(I)
  370 CONTINUE
      GO TO 420
C
C     BOTTOM CURVE ID (SET ONLY VALUES THAT CHANGE FROM THE TOP CURVES)
C
  380 CURVE = 0
      IDOUT(7)   = -1
      IDOUTR(13) = YMIN(2)
      IDOUT(8)   = 1
      IDOUTR(14) = YMAX(2)
      CALL XYTICS (IDOUT(23),IDOUTR(23),IVALUE(20),IDOUT(13),
     1             IDOUT(14),IVALUE(24),YCYCLE(2),0)
      IDOUT(31) = IVALUE(35) + IVALUE(25)
      IDOUT(32) = IVALUE(35) + IVALUE(26)
      IDOUT(33) = IVALUE(35) + IVALUE(31)
      IDOUT(34) = IVALUE(35) + IVALUE(32)
      IDOUT(36) = YCYCLE(2)
      IDOUT(37) = IVALUE(16)
      IDOUT(38) = IVALUE(12)
      IF (IDOUT(38)  .EQ. 1) IDOUTR(38) = 0.0
      IF (IDOUTR(38) .LT. YMIN(2)) IDOUT(37) = 0
      IDOUT(243) = IVALUE(55)
      IDOUT(244) = IVALUE(56)
      DO 390 I = 1,32
      IDOUT(I+146) = TCURVE(I)
      IDOUT(I+178) = XAXIS(I)
      IDOUT(I+210) = YBAXIS(I)
  390 CONTINUE
      IPAIR = CENTER + STEPS
      GO TO 430
C
C     WHOLE CURVE ID
C
  400 CURVE = 0
      IDOUT(7) = 0
      IDOUT(8) = 1
      IDOUTR(11) = XMIN
      IDOUTR(12) = XMAX
      IDOUTR(13) = YMIN(1)
      IDOUTR(14) = YMAX(1)
      IFLAG = 0
      IF (INTORE) IFLAG = 1
      CALL XYTICS (IDOUT(15),IDOUTR(15),IVALUE(17),IDOUT(11),
     1             IDOUT(12),IVALUE(21),XCYCLE,IFLAG)
      CALL XYTICS (IDOUT(23),IDOUTR(23),IVALUE(18),IDOUT(13),
     1             IDOUT(14),IVALUE(22),YCYCLE(1),0)
      IDOUT(31) = IVALUE(33) + IVALUE(25)
      IDOUT(32) = IVALUE(33) + IVALUE(26)
      IDOUT(33) = IVALUE(33) + IVALUE(27)
      IDOUT(34) = IVALUE(33) + IVALUE(28)
      IDOUT(35) = XCYCLE
      IDOUT(36) = YCYCLE(1)
      IDOUT(37) = IVALUE(13)
      IDOUT(38) = IVALUE(10)
      IF (IDOUT(38)  .EQ. 1) IDOUT(38) = 0.0
      IF (IDOUTR(38) .LT. YMIN(1)) IDOUT(37) = 0
      IDOUT(39) = IVALUE(14)
      IDOUT(40) = IVALUE( 9)
      IF (IDOUT(40)  .EQ. 1) IDOUTR(40) = 0.0
      IF (IDOUTR(40) .LT. XMIN) IDOUT(39) = 0
      IDOUT(41 ) = IVALUE(40)
      IDOUT(243) = IVALUE(51)
      IDOUT(244) = IVALUE(52)
      DO 410 I=1,32
      IDOUT(I+146) = TCURVE(I)
      IDOUT(I+178) = XAXIS(I)
      IDOUT(I+210) = YAXIS(I)
  410 CONTINUE
      GO TO 420
C
C     IDOUT IS COMPLETE   OUTPUT CURVES
C
  420 ASSIGN 590 TO ICONT
      IPAIR = IAT + STEPS
      N = 1
C
  430 MCOUNT = 0
      DO 580 M = 1,NAT,3
      MCOUNT = MCOUNT + 1
C
C     CURVE NUMBER, ID, COMPONENT
C
      IDOUT(4) = Z(M)
      ITEMP = M + N
      IDOUT(5) = Z(ITEMP)
      IF (IDOUT(5) .NE. 1000) CURVE = CURVE + 1
      IDOUT(3) = CURVE
C
C     MEAN RESPONSE IN PLACE OF SUBCASE IF RANDOM
C
      IF (RANDOM) IDOUT(1) = Z(ITEMP+1)
C
C     SET NUMBER OF ZERO CROSSINGS IF RANDOM
C
      IF (RANDOM) IDOUT(42) = BUF(MCOUNT+20)
C
C     COMPUTE Y1 = YMIN  AND Y2 = YMAX  FOR ALL DATA FOR THIS CURVE
C
      BEGIN = IPAIR + MCOUNT*STEPS - STEPS
      NULL  = .TRUE.
      DO 460 K = 1,STEPS
      I = BEGIN + K
      IF (Z(I) .EQ. 1) GO TO 460
      IF (.NOT.NULL  ) GO TO 440
      NX1 = K
      NX2 = K
      Y1  = RZ(I)
      Y2  = RZ(I)
      NULL= .FALSE.
      GO TO 460
  440 IF (RZ(I) .GE. Y1) GO TO 450
      Y1  = RZ(I)
      NX1 = K
      GO TO 460
  450 IF (RZ(I) .LE. Y2) GO TO 460
      Y2  = RZ(I)
      NX2 = K
  460 CONTINUE
C
      IF (.NOT.NULL) GO TO 470
      IDOUTR(297) = 0.0
      IDOUTR(298) = 0.0
      IDOUTR(299) = 0.0
      IDOUTR(300) = 0.0
      GO TO 480
  470 NX1 = NX1 + IAT
      NX2 = NX2 + IAT
      IDOUTR(297) = Y1
      IDOUTR(298) = RZ(NX1)
      IDOUTR(299) = Y2
      IDOUTR(300) = RZ(NX2)
C
C     COMPUTE Y1 AND Y2 FOR DATA BETWEEN XMIN AND XMAX
C
  480 NULL = .TRUE.
      IF (I1 .EQ. 0) GO TO 520
      DO 510 K = I1,I2
      I = BEGIN + K
      IF (Z(I) .EQ. 1) GO TO 510
      IF (.NOT.NULL  ) GO TO 490
      NX1 = K
      NX2 = K
      Y1  = RZ(I)
      Y2  = RZ(I)
      NULL= .FALSE.
      GO TO 510
  490 IF (RZ(I) .GE. Y1) GO TO 500
      Y1  = RZ(I)
      NX1 = K
      GO TO 510
  500 IF (RZ(I) .LE. Y2) GO TO 510
      Y2  = RZ(I)
      NX2 = K
  510 CONTINUE
      IF (.NOT.NULL) GO TO 530
  520 IDOUTR(293) = 0.0
      IDOUTR(294) = 0.0
      IDOUTR(295) = 0.0
      IDOUTR(296) = 0.0
      GO TO 540
  530 NX1 = NX1 + IAT
      NX2 = NX2 + IAT
      IDOUTR(293) = Y1
      IDOUTR(294) = RZ(NX1)
      IDOUTR(295) = Y2
      IDOUTR(296) = RZ(NX2)
C
  540 IDOUTR(291) = RZ(IAT+1)
      ITEMP = IAT + STEPS
      IDOUTR(292) = RZ(ITEMP)
C
C     IDOUT IS COMPLETE FOR THIS CURVE
C
      IF (IDOUT(5).NE.0 .AND. IDOUT(5).NE.1000)
     1    CALL XYOUT (-1,IDOUT(1),IDOUTR(1))
      IF (ON) CALL WRITE (OUTFIL,IDOUT(1),300,EOR)
      IDOUT(8) = 0
C
C     DUMP ALL PAIRS TO PRINTER AND PUNCH,  THOSE IN RANGE TO PLOTTER
C
      Y1 = IDOUTR(13)
      Y2 = IDOUTR(14)
      IF (ON) CALL WRITE (OUTFIL,ONEONE(1),2,NOEOR)
      ONES = .TRUE.
      IF (IDOUT(5) .EQ. 1000) GO TO 570
      DO 560 K = 1,STEPS
      I = BEGIN + K
      J = IAT + K
      BUF(1) = Z(J)
      BUF(2) = Z(I)
      IF (Z(I) .EQ. 1) GO TO 560
      IF (K.LT.I1 .OR. K.GT.I2) GO TO 560
      IF (PRINT .OR. PUNCH) CALL XYOUT (1,BUF(1),RBUF(1))
      IF (RZ(I).LT.Y1 .OR. RZ(I).GT.Y2) GO TO 550
      IF (ON) CALL WRITE (OUTFIL,BUF(1),2,NOEOR)
      ONES = .FALSE.
      GO TO 560
  550 IF (ONES) GO TO 560
      IF (ON) CALL WRITE (OUTFIL,ONEONE(1),2,NOEOR)
      ONES = .TRUE.
  560 CONTINUE
  570 IF (ON) CALL WRITE (OUTFIL,BUF(1),0,EOR)
  580 CONTINUE
C
      GO TO ICONT, (590,600)
C
C     DO BOTTOM CURVES IF ANY
C
  590 ASSIGN 600 TO ICONT
      N = 2
      IF (IDOUT(7) .GT. 0) GO TO 380
  600 RETURN
      END
