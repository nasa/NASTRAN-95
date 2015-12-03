      SUBROUTINE PSTQ1(NTYPE)
C  THIS ROUTINE CALCULATES PHASE I OUTPUT FOR PLA3
C  FOR COMBINATION ELEMENTS
C
C**************** PHASE I  STRESS DATA RECOVERY ************************
C **********************************************************************
C
C     9/12/67         E C P T     L I S T I N G
C                    ***************************
C ECPT  TRMEM   QDMEM   TRPLT   QDPLT   TRIA1   QUAD1   TRIA2   QUAD2
C **********************************************************************
C   1   EL.ID   EL.ID   EL.ID   EL.ID   EL.ID   EL.ID   EL.ID   EL.ID
C   2   GRID A  GRID A  GRID A  GRID A  GRID A  GRID A  GRID A  GRID A
C   3   GRID B  GRID B  GRID B  GRID B  GRID B  GRID B  GRID B  GRID B
C   4   GRID C  GRID C  GRID C  GRID C  GRID C  GRID C  GRID C  GRID C
C   5   THETA   GRID D  THETA   GRID D  THETA   GRID D  THETA   GRID D
C   6   MATID   THETA   MATID1  THETA   MATID1  THETA   MAT ID  THETA
C   7   T       MAT ID  I       MATID1  T1      MATID1  T       MAT ID
C   8   NS MASS T       MATID2  I       MATID2  T1      NS MASS T
C   9   CSID 1  NS MASS T2      MATID2  I       MATID2  CSID 1  NS MASS
C  10   X1      CSID 1  NS MASS T2      MATID3  I       X1      CSID 1
C  11   Y1      X1      Z1      NS MASS T2      MATID3  Y1      X1
C  12   Z1      Y1      Z2      Z1      NS MASS T2      Z1      Y1
C  13   CSID 2  Z1      CSID 1  Z2      Z1      NS MASS CSID 2  Z1
C  14   X2      CSID 2  X1      CSID 1  Z2      Z1      X2      CSID 2
C  15   Y2      X2      Y1      X1      CSID 1  Z2      Y2      X2
C  16   Z2      Y2      Z1      Y1      X1      CSID 1  Z2      Y2
C  17   CSID 3  Z2      CSID 2  Z1      Y1      X1      CSID 3  Z2
C  18   X3      CSID 3  X2      CSID 2  Z1      Y1      X3      CSID 3
C  19   Y3      X3      Y2      X2      CSID 2  Z1      Y3      X3
C  20   Z3      Y3      Z2      Y2      X2      CSID 2  Z3      Y3
C  21   TEMP    Z3      CSID 3  Z2      Y2      X2      TEMP    Z3
C  22           CSID 4  X3      CSID 3  Z2      Y2              CSID 4
C  23           X4      Y3      X3      CSID 3  Z2              X4
C  24           Y4      Z3      Y3      X3      CSID 3          Y4
C  25           Z4      TEMP    Z3      Y3      X3              Z4
C  26           TEMP            CSID 4  Z3      Y3              TEMP
C  27                           X4      TEMP    Z3
C  28                           Y4              CSID 4
C  29                           Z4              X4
C  30                           TEMP            Y4
C  31                                           Z4
C  32                                           TEMP
C **********************************************************************
C
      DIMENSION SAVE(32)
C
      COMMON /PLA3ES/ ECPT(100), PH1OUT(173) ,DUMMY(27)
C
C
C     THIS SUBROUTINE INCORPORATES TRIA1, QUAD1, TRIA2, QUAD2
C
C              NTYPE = 1  IMPLIES STRIA1
C              NTYPE = 2  IMPLIES STRIA2
C              NTYPE = 3  IMPLIES SQUAD1
C              NTYPE = 4  IMPLIES SQUAD2
C
C     SAVE THE INCOMING ECPT
C
      DO 10 I=1,32
   10 SAVE(I) = ECPT(I)
C
C     TRANSFER TO OPERATIONS DESIRED
C
C              STRIA1    STRIA2    SQUAD1    SQUAD2
      GO TO(20,100,150,230),NTYPE
C
C     **************
C     *** STRIA1 ***
C     **************
C
C     SET UP ECPT FOR PSTRM1, FIRST CHECK T1 FOR ZERO
   20 IF( SAVE(7) .EQ. 0.0E0 ) GO TO 50
      DO 30 I=9,21
   30 ECPT(I) = SAVE(I + 6)
C
      CALL PSTRM1 (0)
C
C     MOVE OUTPUT FROM PSTRM1 TO NEAR BOTTOM OF PH1OUT
C     WORDS (1 THRU 36) DOWN TO (99 THRU 134)
C
C
      DO 40 I=1,36
   40 PH1OUT(I + 98) = PH1OUT(I)
      GO TO 60
C
   50 PH1OUT( 99) = ECPT(1)
      PH1OUT(100) = 0.0E0
C
C     SET UP CALL TO PSTPL1, CHECK I EQUAL TO ZERO
   60 IF( SAVE(9) .EQ. 0.0E0 ) GO TO 90
      DO 70 I=1,5
   70 ECPT(I) = SAVE(I)
      DO 80 I=6,25
   80 ECPT(I) = SAVE(I + 2)
C
      CALL PSTPL1
      RETURN
C
   90 PH1OUT(1) = ECPT(1)
      PH1OUT(2) = 0.0E0
      RETURN
C
C     **************
C     *** STRIA2 ***
C     **************
  100 IF( SAVE(7) .EQ. 0.0E0 ) GO TO 140
C     SET UP CALL TO PSTRM1
C
C      ECPT IS OK AS DELIVERED TO THIS ROUTINE
C
      CALL PSTRM1(0)
C
C     MOVE OUTPUT FROM PSTRM1 TO NEAR BOTTOM OF PH1OUT
C     WORDS (1 THRU 36) DOWN TO (99 THRU 134)
C
      DO 110 I=1,36
  110 PH1OUT(I + 98) = PH1OUT(I)
C
C     SET UP CALL TO PSTPL1
C
      DO 120 I=1,6
  120 ECPT(I) = SAVE(I)
      ECPT(7) = SAVE(7) ** 3  / 12.0E0
      ECPT(8) = SAVE(6)
      ECPT(9) = SAVE(7)
      ECPT(10)= SAVE(8)
      ECPT(11) = SAVE(7)/2.0E0
      ECPT(12) = -ECPT(11)
      DO 130 I=13,25
  130 ECPT(I) = SAVE(I - 4)
C
      CALL PSTPL1
      RETURN
C
  140 PH1OUT(  1) = ECPT(1)
      PH1OUT(  2) = 0.0E0
      PH1OUT( 99) = ECPT(1)
      PH1OUT(100) = 0.0E0
      RETURN
C
C     **************
C     *** SQUAD1 ***
C     **************
C
  150 IF(SAVE(8).EQ.0.0E0)GO TO 180
C
C     SET UP CALL TO PSQDM1
C
      ECPT(9) = SAVE(13)
      DO 160 I=10,26
  160 ECPT(I) = SAVE(I+6)
C
      CALL PSQDM1
C
C     MOVE OUTPUT DOWN TO NEAR BOTTOM OF PH1OUT
C     WORDS (1 THRU 45) DOWN TO (129 THRU 173)
C
      DO 170 I=1,45
  170 PH1OUT(I + 128) = PH1OUT(I)
C
      GO TO 190
  180 PH1OUT(129) = ECPT(1)
      PH1OUT(130) = 0.0E0
C
  190 IF( SAVE(10) .EQ. 0.0E0 ) GO TO 220
C
C     SET UP CALL TO PSQPL1
C
      DO 200 I=1,6
  200 ECPT(I) = SAVE(I)
      DO 210 I=7,30
  210 ECPT(I) = SAVE(I + 2)
C
      CALL PSQPL1
      RETURN
C
  220 PH1OUT(1) = ECPT(1)
      PH1OUT(2) = 0.0E0
      RETURN
C
C     **************
C     *** SQUAD2 ***
C     **************
C
  230 IF( SAVE(8) .EQ. 0.0E0 ) GO TO 270
C
C     SET UP CALL TO PSQDM1
C
C      ECPT IS OK AS DELIVERED TO THIS ROUTINE
C
      CALL PSQDM1
C
C     MOVE OUTPUT DOWN TO NEAR BOTTOM OF PH1OUT
C     WORDS (1 THRU 45) DOWN TO (129 THRU 173)
C
      DO 240 I=1,45
  240 PH1OUT(I + 128) = PH1OUT(I)
C
C
C     SET UP CALL TO PSQPL1
C
      DO 250 I=1,7
  250 ECPT(I) = SAVE(I)
      ECPT(8) = SAVE(8) **3 / 12.0E0
      ECPT(9) = SAVE(7)
      ECPT(10)= SAVE(8)
      ECPT(11)= SAVE(9)
      ECPT(12) = SAVE(8)/2.0E0
      ECPT(13) =-ECPT(12)
      DO 260 I=14,30
  260 ECPT(I) = SAVE(I - 4)
C
      CALL PSQPL1
C
      RETURN
C
  270 PH1OUT(1) = ECPT(1)
      PH1OUT(2) = 0.0E0
      PH1OUT(129) = ECPT(1)
      PH1OUT(130) = 0.0E0
      RETURN
      END
