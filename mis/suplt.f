      SUBROUTINE SUPLT (IZ,IY,X,U,GPLST,PEN,DEFORM)
C
C     TO CREATE A SET OF UNIQUE LINES TO BE PLOTTED.  IN ADDITION, TO
C     AVOID SKIPPING ALL OVER THE PLOT FOR EACH LINE.
C
C     INPUT (SIMULAR TO -GPCT-)
C       NGRID - NUMBER OF INTERNAL GRID POINTS.
C       IY    - 1 THRU NGRID - POINTERS TO FIRST CONNECTION OF THE
C                              INTERNAL GRID MATCHING THIS INDEX.
C                              IF THE GRID HAS NO ENTRIES IZ(I+1) WILL
C                              HAVE THE SAME POINTER VALUE.
C             - NGRID+1      - POINTER TO END-OF-RECORD.
C       IZ    - CONNECTING INTERNAL GRIDS.  POINTER FOR NEXT GRID
C                              DETERMINES LAST ENTRY.  ENTRIES PUSHED
C                              DOWN AND  -1  ADDED AT END AS EACH ENTRY
C                              IS USED.
C
C       NTAB  = TOTAL ENTRY COUNTER IN GPCT
C       ID1   = START OF CURRENT -LINE-
C       ID2   = END   OF CURRENT -LINE-
C       ID3   = START OF LAST -LINE-
C       ID4   = END   OF LAST -LINE-
C
      INTEGER         IZ(1),IY(1),PEN,DEFORM,GPLST(1),NM(5),M(71),
     1                M1(17),M2(17),M3(11),M4(17),M5(9),ERR(4),LM(5),
     2                LIMT(2)
      REAL            U(2,1),X(3,1)
      COMMON /SYSTEM/ SYSBUF,IOUT
      COMMON /BLANK / NGRID,SKP1(19),MERR
      EQUIVALENCE     (M1(1),M( 1)), (M2(1),M(18)), (M3(1),M(35)),
     1                (M4(1),M(46)), (M5(1),M(63))
      DATA    NM    / 17,17,11,17, 9 /,
     1        LM    /  1,18,35,46,63 /,
     2        M1    / 4H(35X, 4H,26H, 4HSUPL, 4HT RE, 4HJECT, 4HED P,
     3                4HLOT., 4H PIV, 4HOT,I, 4H8,26, 4HH IS, 4H ZER,
     4                4HO OR, 4H SAM, 4HE AS, 4H ENT, 4HRY.)/,
     5        M2    / 4H(35X, 4H,54H, 4HSUPL, 4HT RE, 4HJECT, 4HED P,
     6                4HLOT., 4H NEG, 4HATIV, 4HE NU, 4HMBER, 4H ENT,
     7                4HRIES, 4H - N, 4H3,N4, 4H =,2, 4HI10)/,
     8        M3    / 4H(35X, 4H,31H, 4HUNEX, 4HPECT, 4HED E, 4HOF I,
     9                4HN SU, 4HPLT , 4H- PI, 4HVOT,, 4HI10)/,
     O        M4    / 4H(35X, 4H,11H, 4HSUPL, 4HT-EN, 4HTRY,, 4HI10,,
     1                4H22H , 4HFOR , 4HPIVO, 4HT NO, 4HT FO, 4HUND ,
     2                4H(,2I, 4H6,8H, 4H) RA, 4HNGE., 4H)   /,
     3        M5    / 4H(35X, 4H,24H, 4HNO E, 4HLEME, 4HNTS , 4HIN T,
     4                4HHIS , 4HSET., 4H)   /
C
      LINESP = 0
C
C     LOCATE FIRST PIVOT (ID1) WITH ODD NUMBER OF ENTRIES
C
      ID2 = 0
      ID1 = 0
      DO 10 I = 1,NGRID
      LL  = IY(I+1) - IY(I)
      IF (LL .EQ. 0) GO TO 10
C
C     IN CASE AN ODD NO. ENTRIES ISN'T FOUND THE DEFAULT IS FIRST PIVOT
C
      IF (ID2 .EQ. 0) ID2 = I
      IF (MOD(LL,2) .EQ. 0) GO TO 10
      ID1 = I
      I1  = IY(I)
      ID2 = IZ(I1)
      GO TO 20
   10 CONTINUE
C
   20 NTAB = IY(NGRID+1) - IY(1)
C
C     NO ELEMENTS IN THE SET
C
      IF (NTAB .EQ. 0) GO TO 440
C
C     SEE IF ANY ODD ENTRIES FOUND
C
      LIMT(1) = 0
      LIMT(2) = NGRID + 1
      IF (ID1 .NE. 0) GO TO 140
      ID1 = ID2
      I1  = IY(ID1)
      ID2 = IZ(I1)
      GO TO 140
C
C     START OF LOOP AFTER FIRST -LINE-
C
   30 IF (N4) 410,260,40
C
C     LAST END HAS ENTRY TO CONTINUE FROM
C
   40 ID1 = ID4
C
C     INTERNAL SEARCH FOR FIRST GPCT ENTRY ABOVE AND BELOW THE PIVOT
C     VALUE FOR THE PIVOT           *** M I N / M A X ***
C
   50 I1 = IY(ID1)
      IL =-100000
      IH = 100000
      J1 = IY(ID1+1) - 1
C
      DO 80 I = I1,J1
      IF (IZ(I)    ) 100,400,60
   60 IF (IZ(I)-ID1)  70,400,90
   70 IL = IZ(I)
   80 CONTINUE
      GO TO 100
C
   90 IH = IZ(I)
C
C     DETERMINE  WHICH IS CLOSER TO PIVOT
C
  100 I = ID1 - IL
      J = IH  - ID1
      IF (J-I) 130,110,120
C
C     EQUAL DISTANT, GO TO SAME DIRECTION AS BEFORE
C
  110 IF (ID4-ID3) 120,400,130
C
C     ID2 IS LESSOR ID
C
  120 ID2 = IL
      GO TO 140
C
C     ID2 IS GREATER ID
C
  130 ID2 = IH
C
C     OUTPUT THE LINE -
C     NOTE THAT ID4 MAY BE RESET AT 320 SO DONT TAKE SHORTCUTS
C
  140 CONTINUE
      I  = IABS(GPLST(ID1))
      J  = IABS(GPLST(ID2))
      IF (DEFORM .NE. 0) GO TO 160
      X1 = X(2,I)
      Y1 = X(3,I)
      X2 = X(2,J)
      Y2 = X(3,J)
      GO TO 170
  160 X1 = U(1,I)
      Y1 = U(2,I)
      X2 = U(1,J)
      Y2 = U(2,J)
  170 CONTINUE
      CALL LINE (X1,Y1,X2,Y2,PEN,0)
      LINESP = LINESP + 1
C
C     REMOVE ENTRIES FROM CORE, LEFT SHIFT AS NEEDED AND PUT -1 AT THE
C     END OF THE TABLE.  PLACE THE NUMBER OF ENTRIES LEFT IN N3 AND N4.
C     DECREMENT THE TOTAL NUMBER OF ENTRIES BY 2. SET ID3 AND ID4.
C
      IF (NTAB .LE. 2) GO TO 460
      KK = 0
      J1 = I1
      J2 = IY(ID1+1) - 1
      LL = ID2
  180 IPAR = J1
      IL = 0
C
      DO 230 I = J1,J2
      IF (IZ(I)-LL) 210,200,190
  190 IZ(IPAR) = IZ(I)
      GO TO 220
C
C     COMPONENT TO BE ELIMINATED HAS BEEN FOUND
C
  200 IL = 1
      GO TO 230
  210 IF (IZ(I)) 240,400,220
  220 IPAR = IPAR + 1
  230 CONTINUE
C
  240 IZ(IPAR) = -LL
      IF (IL .EQ. 0) GO TO 430
      IF (KK .NE. 0) GO TO 250
      KK = 2
      ID3= ID1
      N3 = IPAR - J1
      LL = ID2
      J1 = IY(LL  )
      J2 = IY(LL+1) - 1
      LL = ID1
      GO TO 180
  250 NTAB= NTAB - 2
      ID4 = ID2
      N4  = IPAR - J1
      GO TO 30
C
C     CASE ID4 HAS NO MORE ENTRIES. CHECK IF ID3 CAN BE PIVOT
C
  260 IF (N3) 410,280,270
C
C     NONZERO - ID3 IS TO BE ID1
C
  270 ID1 = ID3
      GO TO 50
C
C     ID3 AND ID4 ARE NULL.  GO TO CLOSEST END OF TABLE FROM ID4
C
  280 I = NGRID - ID4
      J = 1
      IF (I .GT. ID4) J = -1
      L = (J+2)/2 + 1
      LIM = LIMT(L)
      LEN = ID4
C
      ASSIGN 310 TO IRET1
      KK = ID4
  290 KK = KK + J
      IF (KK .EQ. LIM) GO TO IRET1, (310,420)
      IPAR = 2
      ASSIGN 300 TO IRET
      GO TO 370
C
C     CHECK IF ANY ENTRIES FOUND
C
  300 IF (IPAR .EQ. 0) GO TO 290
C
C     ENTRY FOUND
C
      LEN = KK + J
      ID4 = KK
      N4  = IPAR
      GO TO 320
C
C     THAT END OF TABLE FAILED - TRY OTHER END
C
  310 J   = -J
      LIMT(L) = LEN
      L   = (J+2)/2 + 1
      LIM = LIMT(L)
      KK  = ID4
      ASSIGN 420 TO IRET1
      GO TO 290
C
C     AN ENTRY WAS FOUND - CHECK FOR ODD NUMBER OF ENTRIES FOR PIVOT
C
  320 IF (MOD(IPAR,2) .EQ. 1) GO TO 360
C
C     NOT AN ODD NUMBER OF ENTRIES FOR ID4.  CHECK GPCT ENTRIES
C     FOR ONLY ONE ENTRY.
C
      ASSIGN 340 TO IRET
C
      IH = J2
      IL = J1 - 1
  330 IL = IL + 1
      KK = IZ(IL)
      IF (KK .LE. 0) GO TO 40
      IPAR = 1
      GO TO 370
  340 CONTINUE
      IF (IPAR .EQ.  1) GO TO 360
      IF (IL   .LT. IH) GO TO 330
C
C     PIVOT NOW DETERMINED
C
      GO TO 40
  360 ID1 = KK
      GO TO 50
C
C
C     INTERNAL ROUTINE TO DETERMINE NUMBER OF ENTRIES FOR PIVOT
C
C     INPUT
C        IPAR = 1 -- 0,1 OR MORE THAN 1 ENTRY RETURN
C             = 2 -- ACTUAL NUMBER OF ENTRIES RETURN
C        KK   = ID OF PIVOT
C
C     OUTPUT
C        IPAR = DESIRED NUMBER OF ENTRIES
C        KK   = SAME AS INPUT
C        J1   = POINTER TO 1ST LOCATION
C        J2   = NOT NECESSARILY LAST LOCATION (I.E. IPAR INPUT AS 1)
C
  370 J1 = IY(KK  )
      J2 = IY(KK+1) - 1
      IF (IPAR .EQ. 1) J2 = MIN0(J1+2,J2)
      IPAR = 0
      IF (J2-J1 .LT. 0) GO TO 390
C
      DO 380 I = J1,J2
      IF (IZ(I)) 390,400,380
  380 IPAR = IPAR + 1
  390 GO TO IRET, (300,340)
C
C     ERROR MESSAGES
C
  400 ERR(1) = 1
      ERR(2) = ID1
      K = 1
      GO TO 450
  410 ERR(1) = 2
      ERR(2) = N3
      ERR(3) = N4
      K = 2
      GO TO 450
  420 ERR(1) = 1
      ERR(2) = ID4
      K = 3
      GO TO 450
  430 ERR(1) = 3
      ERR(2) = LL
      ERR(3) = J1
      ERR(4) = J2
      K = 4
      GO TO 450
  440 ERR(1) = 0
      K = 5
C
  450 I = LM(K)
      CALL WRTPRT (MERR,ERR,M(I),NM(K))
      IF (K .EQ. 5) GO TO 530
C
C     CONVERT TABLE TO ORIGINAL VALUES UNLESS THIS IS THE LAST CALL
C
  460 IL = NGRID  + 1
      I  = IY(IL) - 1
      IF (DEFORM .NE. 0) GO TO 530
      DO 480 J = 1,I
  480 IZ(J) = IABS(IZ(J))
C
      DO 520 J1 = 1,NGRID
      IF (IY(J1) .EQ. IY(J1+1)) GO TO 520
      I = IY(J1)
      L = I
      N = IY(J1+1) - 1
      IF (I+1 .GT. N) GO TO 520
C
C     SHUTTLE EXCHANGE
C     (NOTE FROM G.CHAN/UNISYS  10/1990
C     THERE ARE MORE THAN JUST A SHUTTLE SORTING HERE. REPLACING THE
C     SHUTTLE EXCHANGE METHOD BY SORT ROUTINE, WHICH USES A MUCH FASTER
C     TECHNEQUE, DOES NOT WORK HERE)
C
  490 IF (IZ(I) .LE. IZ(I+1)) GO TO 510
      K = IZ(I+1)
      IZ(I+1) = IZ(I)
      IZ(I  ) = K
      J = I
  500 IF (J .EQ. L) GO TO 510
      IF (IZ(J) .GE. IZ(J-1)) GO TO 510
      K = IZ(J)
      IZ(J  ) = IZ(J-1)
      IZ(J-1) = K
      J = J - 1
      GO TO 500
  510 IF (I .GE. N-1) GO TO 520
      I = I + 1
      GO TO 490
C
  520 CONTINUE
C
C     A NONSTANDARD RETURN COULD BE ADDED HERE.  BAD PLOT RESULTS IF
C     THIS ROUTINE FAILS.  THE FRAME WILL BE PRESENT HOWEVER
C
  530 CONTINUE
      RETURN
      END
