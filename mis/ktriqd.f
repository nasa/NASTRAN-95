      SUBROUTINE KTRIQD (NTYPE)
C
C
C     8/18/67         E C P T     L I S T I N G
C
C ECPT  TRMEM   QDMEM   TRPLT   QDPLT   TRIA1   QUAD1   TRIA2   QUAD2
C ***** ******* ******* ******* ******* ******* ******* ******* ********
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
C
C
      LOGICAL         HEAT
      INTEGER         SCR4,IECPT(4),BCD(2,4),BGPDT(4)
      DIMENSION       SAVE(32)
      COMMON /BLANK / SKIP(16),VOLUME,SURFAC
      COMMON /MATOUT/ DUM(6),RHO
      COMMON /SMA1HT/ HEAT
      COMMON /SMA1ET/ ECPT(100)
      COMMON /SMA1DP/ DUMMY(600)
      EQUIVALENCE     (SAVE(1),ECPT(50)),(ECPT(1),IECPT(1))
      DATA    BCD   / 4HCTRI,2HA1,4HCTRI,2HA2,4HCQUA,2HD1,4HCQUA,2HD2 /
      DATA    OLD   , KOUNT,NGPT / 0.0,   2*0     /
      DATA    SCR4  , BGPDT/  304, 15, 9, 16, 10  /
C
C     THIS SUBROUTINE INCORPORATES TRIA1, QUAD1, TRIA2, QUAD2
C
C             NTYPE = 1  IMPLIES KTRIA1
C             NTYPE = 2  IMPLIES KTRIA2
C             NTYPE = 3  IMPLIES KQUAD1
C             NTYPE = 4  IMPLIES KQUAD2
C
C     CALLS FROM THIS ROUTINE CAN BE MADE TO
C
C            KTRMEM - TRIANGULAR MEMBRANE ROUTINE
C            KQDMEM - QUADRILATERAL MEMBRANE ROUTINE
C            KTQPLT - TRIANGULAR OR QUADRILATERAL PLATE ROUTINE
C            QDMM1X - HIGHER LEVEL QUADRIALATER MEMBRANE ROUTINE
C
C     ALL INSERTIONS OF 6X6 ELEMENT STIFFNESS MATRICES ARE HANDLED BY
C     THE ABOVE ROUTINES.
C
C
C     THE SAVED ECPT IS EQUIVALENCED TO ECPT(50)
C
C     SAVE THE INCOMING ECPT
C
      DO 10 I = 1,32
   10 SAVE(I) = ECPT(I)
C
C     TRANSFER TO OPERATIONS DESIRED
C
C           KTRIA1 KTRIA2 KQUAD1 KQUAD2
      GO TO (  20,    70,   100,   150), NTYPE
C
C     *** KTRIA1 ***
C
C     SET UP ECPT FOR CALL TO KTRMEM (0), FIRST CHECK T1 FOR ZERO.
C
   20 IF (SAVE(7) .EQ. 0.0) GO TO 40
      DO 30 I = 9,21
   30 ECPT(I) = SAVE(I+6)
C
      CALL KTRMEM (0)
C
C     SET UP ECPT FOR CALL TO TQPLT(3), FIRST CHECK I AND T2 EQUAL ZERO.
C
   40 IF (SAVE(9) .EQ. 0.0) GO TO 200
      DO 50 I = 1,5
   50 ECPT(I) = SAVE(I)
      DO 60 I = 6,25
   60 ECPT(I) = SAVE(I+2)
C
      IF (.NOT.HEAT) CALL KTRPLT
      GO TO 200
C
C     *** KTRIA2 ***
C
   70 IF (SAVE(7) .EQ. 0.0) GO TO 200
C
C     SET UP ECPT FOR CALL TO KTRMEM (0)
C
C     ECPT IS OK AS DELIVERED TO THIS ROUTINE
C
      CALL KTRMEM (0)
C
C     SET UP ECPT FOR CALL TO KTQPLT (3)
C
      DO 80 I = 1,6
   80 ECPT(I) = SAVE(I)
      ECPT(7) = SAVE(7)**3/12.0
      ECPT(8) = SAVE(6)
      ECPT(9) = SAVE(7)
      ECPT(10)= SAVE(8)
      DO 90 I = 13,25
   90 ECPT(I) = SAVE(I-4)
C
      IF (.NOT.HEAT) CALL KTRPLT
      GO TO 200
C
C     *** KQUAD1 ***
C
  100 IF (SAVE(8) .EQ. 0.0) GO TO 120
C
C     SET UP ECPT FOR CALL TO KQDMEM
C
      ECPT(9) = SAVE(13)
      DO 110 I = 10,26
  110 ECPT(I) = SAVE(I+6)
C
      CALL KQDMEM
C
  120 IF (SAVE(10) .EQ. 0.0) GO TO 200
C
C     SET UP ECPT FOR CALL TO KTQPLT (4)
C
      DO 130 I = 1,6
  130 ECPT(I) = SAVE(I)
      DO 140 I = 7,30
  140 ECPT(I) = SAVE(I+2)
C
      IF (.NOT.HEAT) CALL KQDPLT
      GO TO 200
C
C     *** KQUAD2 ***
C
  150 IF (SAVE(8) .EQ. 0.0) GO TO 200
C
C     SET UP ECPT FOR CALL TO KQDMEM
C     (WHICH HAS WEAK STIFFNESS MATRIX FORMULATION)
C     OR
C     SET UP ECPT FOR CALL TO QDMM1D/S (BETTER MEMBRANE FORMALATION)
C     THE PROBLEM HERE IS THAT KTRIQD AND KQDMEM ARE EMGOLD ELEMENTS
C     WHILE QDMM1D/S ARE EMGPRO NEW ELEMENTS.
C     TO SOLVE THIS PROPLEM, WE NEED A S.P./D.P. QDMM1X ELEMENT ROUTINE
C     THAT USES QDMM1D/S FORMULATION WITH EMGOLD/SMA1B TECHNIQUE.
C
C     ECPT IS OK AS DELIVERED TO THIS ROUTINE
C
C     CALL QDMM1X
C     (QDMM1X IS INCOMPLETE AS OF 3/92. GO BACK TO KQDMEM)
C
      CALL KQDMEM
C
C     SET UP ECPT FOR CALL TO KTQPLT (4)
C
      DO 160 I = 1,7
  160 ECPT(I) = SAVE(I)
      ECPT(8) = SAVE(8)**3/12.0
      ECPT(9) = SAVE(7)
      ECPT(10)= SAVE(8)
      ECPT(11)= SAVE(9)
      DO 170 I = 14,30
  170 ECPT(I) = SAVE(I-4)
C
      IF (.NOT. HEAT) CALL KQDPLT
C
C
C     SAVE ELEMENT NAME, ID, THICKNESS, DENSITY, NO. OF GRID POINTS,
C     AND GRID PT DATA IF USER REQUESTED VOLUME AND AREA COMPUTATION
C
  200 IF (HEAT .OR. (VOLUME.LE.0.0 .AND. SURFAC.LE.0.0)) GO TO 220
      IF (SAVE(1) .EQ. OLD) GO TO 210
      OLD  = SAVE(1)
      NGPT = 3
      IF (NTYPE .GE. 3) NGPT = 4
      KOUNT = 0
  210 KOUNT = KOUNT + 1
      IF (KOUNT .LT. NGPT) GO TO 220
      ECPT(2) = SAVE(7)
      ECPT(3) = RHO
      IECPT(4)= NGPT
      I = BGPDT(NTYPE)
      K = NGPT*4
      IF (NTYPE .GE. 3) ECPT(2) = SAVE(8)
      CALL WRITE (SCR4,BCD(1,NTYPE),2,0)
      CALL WRITE (SCR4,ECPT(1),4,0)
      CALL WRITE (SCR4,SAVE(2),NGPT,0)
      CALL WRITE (SCR4,SAVE(I),K,1)
  220 RETURN
      END
