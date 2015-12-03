      SUBROUTINE GP3C
C
C     GP3C EXECUTES ONLY IF PLOAD2 AND/OR PLOAD3 CARDS ARE PRESENT. ITS
C     FUNCTION IS TO --
C     (1) PROCESS PLOAD2 CARDS SO THAT THEIR FORMAT IS IDENTICAL TO
C         PLOAD CARDS.  IF A PLOAD RECORD EXISTS ON GEOM3, PLOAD2 DATA
C         IS APPENDED TO THE DATA, SORTED, AND ALL RESULTING PLOAD DATA
C         IS WRITTEN ON SCR2.
C     (2) PROCESS PLOAD3 CARDS SO THAT ALL PRESSURES APPLIED TO AN ISO-
C         PARAMETRIC SOLID ARE GATHERED IN ONE ENTRY AND SORTED BY THE
C         FACE NUMBER TO WHICH THE PRESSURE IS APPLIED.  THE SORTED
C         PRESSURES AND GRID POINT NUMBERS FOR EACH ELEMENT ARE WRITTEN
C         ON SCR2.
C
C
      EXTERNAL        ANDF
      INTEGER         GEOM3 ,GEOM2 ,SCR2  ,BUF1  ,BUF2  ,BUF   ,CLSREW,
     1                WRTREW,RDREW ,ELEM  ,FILE  ,NAM(2),PLOAD2,Z     ,
     2                CARDID,PLOAD3,PL2   ,PL3   ,PLD(3),D1    ,D2    ,
     3                ANDF  ,FACES(6,12)  ,PL3ERR(14)
      REAL            RZ(1),P(12)
      COMMON /GP3COM/ GEOM3 ,EQEXIN,GEOM2 ,SLT   ,GPTT  ,SCR1  ,SCR2   ,
     1                BUF1  ,BUF2  ,BUF(50)      ,CARDID(60)   ,IDNO(30)
     2              , CARDDT(60)   ,MASK(60)     ,STATUS(60)   ,NTYPES ,
     3                IPLOAD,IGRAV ,PLOAD2(2)    ,LOAD(2)      ,NOPLD2 ,
     4                TEMP(2)      ,TEMPD(2)     ,TEMPP1(2)    ,
     5                TEMPP2(2)    ,TEMPP3(2)    ,TEMPRB(2)    ,BUF3   ,
     6                PLOAD3(2)    ,IPLD3
      COMMON /NAMES / RD    ,RDREW ,WRT   ,WRTREW,CLSREW
      COMMON /ZZZZZZ/ Z(1)
      COMMON /GPTA1 / NELEM ,LAST  ,INCR  ,ELEM(1)
      COMMON /TWO   / TWO(32)
      COMMON /SYSTEM/ SYSBUF,NOUT
      EQUIVALENCE     (RZ(1),Z(1))
C
C  FACE                 IHEX1             IHEX2             IHEX3
C   NO                D1      D2        D1      D2        D1      D2
      DATA    FACES/   1,      3,        1,      5,        1,      7,
     1                 2,      4,        3,      7,        4,     10,
     2                 1,      6,        1,     15,        1,     24,
     2                 2,      5,        3,     13,        4,     21,
     3                 2,      7,        3,     17,        4,     27,
     3                 3,      6,        5,     15,        7,     24,
     4                 3,      8,        5,     19,        7,     30,
     4                 4,      7,        7,     17,       10,     27,
     5                 1,      8,        1,     19,        1,     30,
     5                 4,      5,        7,     13,       10,     21,
     6                 5,      7,       13,     17,       21,     27,
     6                 6,      8,       15,     19,       24,     30/
C
      DATA   N3304,N3305,PL3ERR/4H3304, 4H3305, 4H0***, 4H USE, 4HR FA,
     C                          4HTAL , 4HMESS, 4HAGE , 4H330*, 4H, PL,
     2                          4HOAD3, 4H CAR, 4HD FR, 4HOM L, 4HOAD ,
     3                          4HSET /
      DATA   NAM  /    4HGP3C,4H      /
C
C     CHECK TRAILER BITS FOR PRESENCE OF PLOAD2 AND PLOAD3 CARDS.
C     IF NONE EXIST, RETURN.  OTHERWISE, BRANCH AND INITIALIZE TO
C     PROCESS ONE OF THESE CARD TYPES.
C
      NOGO = 0
      PL2  = 0
      PL3  = 0
      J    = (PLOAD2(2)-1)/16
      K    = PLOAD2(2) - 16*J
      IF (ANDF(BUF(J+2),TWO(K+16)) .NE. 0) PL2 = 1
      J    = (PLOAD3(2)-1)/16
      K    = PLOAD3(2) - 16*J
      IF (ANDF(BUF(J+2),TWO(K+16)) .NE. 0) PL3 = 1 - 2*PL2
      FILE = SCR2
      IF (PL2-PL3 .NE. 0) CALL OPEN (*210,SCR2,Z(BUF2),WRTREW)
      IF (PL2 .EQ. 0) GO TO 15
      NOPLD2 = 1
      PLD(1) = PLOAD2(1)
      PLD(2) = PLOAD2(2)
      PLD(3) = 24
      INCRD  = 3
      INCL   = 6
      IDL    = 2
      GO TO 10
   15 IF (PL3 .EQ. 0) GO TO 196
      NOPLD2 = NOPLD2 + 2
      PLD(1) = PLOAD3(1)
      PLD(2) = PLOAD3(2)
      PLD(3) = 255
      INCRD  = 5
      INCL   = 39
      IDL    = 1
C
C     READ PLOAD2 OR PLOAD3 CARDS INTO CORE IN AN EXPANDED FORMAT.
C     SET THE SET ID NEGATIVE TO INDICATE THE CARD IS NOT YET CONVERTED.
C
   10 I = 1
      FILE = GEOM3
      CALL PRELOC (*210,Z(BUF1),GEOM3)
      CALL LOCATE (*230,Z(BUF1),PLD,FLAG)
      IF (PL2 .NE. 1) GO TO 20
      PLD(1) = CARDID(IPLOAD  )
      PLD(2) = CARDID(IPLOAD+1)
   20 CALL READ (*220,*30,GEOM3,Z(I),INCRD,0,FLAG)
      Z(I) = -Z(I)
      IF (PL2 .EQ.  1) GO TO 29
      IF (I .LT. INCL) GO TO 25
      DO 21 J = 2,I,INCL
      K = J
      IF (Z(J) .NE. Z(I+2)) GO TO 21
      IF (Z(J-1) .EQ. Z(I)) GO TO 22
   21 CONTINUE
   25 P(1)   = RZ(I+1)
      Z(I+1) = Z(I+2)
      RZ(I+2)= P(1)
      Z(I+14)= Z(I+3)
      Z(I+15)= Z(I+4)
      Z(I+3) =-1
      GO TO 29
   22 J = K + 2
   23 IF (Z(J) .EQ. -1) GO TO 24
      J = J + 1
      IF (J .LE. K+12) GO TO 23
      GO TO 25
   24 RZ(J) = RZ(I+1)
      IF (J .LT. K+12) Z(J+1) = -1
      J = K + 15 + 2*(J-K-2)
      Z(J  ) = Z(I+3)
      Z(J+1) = Z(I+4)
      GO TO 20
   29 Z(I+INCL-1) = 0
      I = I+INCL
      IF (I .LT. BUF2) GO TO 20
      CALL MESAGE (-8,0,NAM)
   30 CALL CLOSE (GEOM3,CLSREW)
      NPLD2 = I - INCL
      NWDS  = I - 1
C
C     POSITION TO FIRST DATA RECORD ON GEOM2.
C
      FILE = GEOM2
      CALL OPEN (*130,GEOM2,Z(BUF1),RDREW)
      CALL FWDREC (*220,GEOM2)
C
C     READ 3-WORD RECORD ID. LOOK FOR ID IN ELEM TABLE.
C     IF NOT THERE, SKIP RECORD.
C     IF PROCESSING PLOAD2, AND NOT A TWO-DIMENSIONAL ELEMENT, SKIP REC.
C     IF PROCESSING PLOAD3, AND NOT AN ISOPARAMETRIC ELEMENT, SKIP REC.
C     OTHERWISE,  INITIALIZE PARAMETERS.
C
   50 CALL READ (*130,*50,GEOM2,BUF,3,0,FLAG)
      DO 60 I = 1,LAST,INCR
      IF (BUF(1) .EQ. ELEM(I+3)) GO TO 80
   60 CONTINUE
   70 CALL FWDREC (*220,GEOM2)
      GO TO 50
   80 NGPS  = ELEM(I+9)
      ITYPE = ELEM(I+2)
C
C   . IF ELEMENT TYPE IS 68 (QUADTS) THEN USE FIRST FOUR  GRID POINTS
C   . IF ELEMENT TYPE IS 69 (TRIATS) THEN USE FIRST THREE GRID POINTS
C
      IF (ITYPE.EQ.68 .OR. ITYPE.EQ.69) NGPS = NGPS/2
      IF (PL2.EQ.1 .AND. (NGPS.LT.3 .OR. NGPS.GT.4)) GO TO 70
      IF (PL3.EQ.1 .AND. (ITYPE.LT.65 .OR. ITYPE.GT.67)) GO TO 70
      ITYPE  = 2*(ITYPE-64) - 1
      NWDECT = ELEM(I+5)
      J1 = ELEM(I+12)
      J2 = J1 + NGPS - 1
C
C     READ EACH ELEMENT IN RECORD. LOOK FOR ELEMENT ID MATCH IN PLOAD2
C     OR PLOAD3 LIST.  IF FOUND, SET THE SET ID POSITIVE TO INDICATE
C     ENTRY IS CONVERTED.
C
   90 CALL READ (*220,*50,GEOM2,BUF,NWDECT,0,FLAG)
      DO 110 I = 1,NPLD2,INCL
      IF (Z(I) .GT. 0) GO TO 110
      IF (Z(I+IDL) .NE. BUF(1)) GO TO 110
      Z(I) = -Z(I)
      IX   = I
      IF (PL3 .EQ. 1) GO TO 300
C
C     PLACE GRID POINT NUMBERS FROM ELEMENT CARD IN PLOAD2 ENTRY TO
C     MAKE IT LOOK LIKE PLOAD CARD.
C
      DO 100 J = J1,J2
      Z(IX+2) = BUF(J)
      IX = IX + 1
  100 CONTINUE
      GO TO 110
C
C     FIND THE DIAGONALS ON THE PLOAD3 CARD ON THE ELEMENT CARD TO
C     DETERMINE THE FACES TO WHICH THE PRESSURES ARE APPLIED.  SORT
C     THE PRESSURES BY FACE NUMBER AND APPEN+ THE GRID POINT NUMBERS
C     FROM THE ELEMENT CARD TO THE PLOAD3 ENTRY.
C
  300 NP = 0
      DO 310 J = 1,12
      IF (Z(I+J+1) .EQ. -1) GO TO 315
      NP = NP + 1
      P(J) = RZ(I+J+1)
  310 CONTINUE
  315 DO 320 J = 1,6
  320 RZ(I+J)  = 0.0
      DO 350 J = 1,NP
      K   = I + 14 + 2*(J-1)
      ID1 = Z(K  )
      ID2 = Z(K+1)
      DO 322 K = J1,J2
      IF (ID1 .EQ. BUF(K)) GO TO 324
  322 CONTINUE
      GO TO 335
  324 ID1 = K - J1 + 1
      DO 326 K = J1,J2
      IF (ID2 .EQ. BUF(K)) GO TO 328
  326 CONTINUE
      GO TO 335
  328 ID2 = K - J1 + 1
      D1  = MIN0(ID1,ID2)
      D2  = MAX0(ID1,ID2)
      DO 330 K = 1,12
      NFACE = (K+1)/2
      IF (D1 .NE. FACES(ITYPE  ,K)) GO TO 330
      IF (D2 .EQ. FACES(ITYPE+1,K)) GO TO 340
  330 CONTINUE
  335 NOGO = 1
      PL3ERR(7) = N3305
      WRITE (NOUT,420) PL3ERR,Z(I),BUF(1)
      GO TO 350
  340 RZ(I+NFACE) = RZ(I+NFACE)+P(J)
  350 CONTINUE
      IX = IX + 7
      DO 360 J = J1,J2
      Z(IX) = BUF(J)
      IX = IX + 1
  360 CONTINUE
      IF (IX+1-I .GT. 39) GO TO 110
      K = I + 38
      DO 370 J = IX,K
  370 Z(J) = 0
  110 CONTINUE
      GO TO 90
C
C     HERE WHEN END-OF-FILE ON GEOM2 IS ENCOUNTERED.
C     MAKE SURE ALL PLOAD2 OR PLOAD3 ENTRIES HAVE BEEN CONVERTED.
C
  130 CALL CLOSE (GEOM2,CLSREW)
      DO 140 I = 1,NPLD2,INCL
      IF (Z(I) .GT. 0) GO TO 140
      NOGO = 1
      BUF(1) = -Z(I)
      BUF(2) =  Z(I+IDL)
      IF (PL2 .EQ. 1) CALL MESAGE (30,105,BUF)
      PL3ERR(7) = N3304
      IF (PL3 .EQ. 1) WRITE (NOUT,410) PL3ERR,BUF(1),BUF(2)
  140 CONTINUE
      IF (NOGO .NE. 0) CALL MESAGE (-61,0,0)
      IF (PL3  .EQ. 1) GO TO 190
C
C     LOCATE PLOAD RECORD ON GEOM3. IF PRESENT, READ PLOAD DATA INTO
C     CORE (AFTER PLOAD2 DATA) AND SORT COMBINED DATA ON SET ID.
C
      CALL PRELOC (*210,Z(BUF1),GEOM3)
      CALL LOCATE (*180,Z(BUF1),CARDID(IPLOAD),FLAG)
      I = NPLD2 + 6
  160 CALL READ (*220,*170,GEOM3,Z(I),6,0,FLAG)
      I = I + 6
      IF (I .LT. BUF2) GO TO 160
      CALL MESAGE (-8,0,NAM)
  170 NPLD2 = I - 6
      NWDS  = I - 1
      CALL SORT (0,0,6,1,Z,NWDS)
  180 CALL CLOSE (GEOM3,CLSREW)
C
C     WRITE DATA ON SCR2, SET FLAG TO INDICATE AND RETURN.
C
  190 CALL WRITE (SCR2,PLD,3,0)
      CALL WRITE (SCR2,Z,NWDS,1)
      IF (PL2 .NE. 1) GO TO 196
  195 PL2 = -PL2
      PL3 = -PL3
      GO TO 15
  196 CALL CLOSE (SCR2,CLSREW)
      RETURN
C
C     ERROR MESSAGES.
C
  200 CALL MESAGE (N,FILE,NAM)
  210 N = -1
      GO TO 200
  220 N = -2
      GO TO 200
C
C     ABNORMAL RETURN.
C
  230 IF (PL3 .LT. 0) GO TO 195
      CALL CLOSE (GEOM3,CLSREW)
      RETURN
C
C     PLOAD3 CARD ERRORS
C
  410 FORMAT (14A4,I9,' REFERENCES MISSING OR NON-ISOPARAMETRIC ELEMENT'
     1       ,I9)
  420 FORMAT (14A4,I9,' HAS INVALID GRID POINT NUMBERS FOR ELEMENT',I9)
      END
