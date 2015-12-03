      SUBROUTINE PKTRQD (NTYPE)
C  THIS ROUTINE SETS UP THE ECPT FOR COMBINATION ELEMENTS IN PLA4
C
C
C **********************************************************************
C
C     8/18/67         E C P T     L I S T I N G
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
      COMMON /PLA4ES/ ECPT(100)
      COMMON /PLA42D/ DUMMY(600)
      EQUIVALENCE (SAVE(1),ECPT(50))
C
C     THIS SUBROUTINE INCORPORATES TRIA1, QUAD1, TRIA2, QUAD2
C
C              NTYPE = 1  IMPLIES KTRIA1
C              NTYPE = 2  IMPLIES KTRIA2
C              NTYPE = 3  IMPLIES KQUAD1
C              NTYPE = 4  IMPLIES KQUAD2
C
C
C     THE SAVED ECPT IS EQUIVALENCED TO ECPT(50)
C
C     SAVE THE INCOMING ECPT
C
      DO 10 I=1,32
   10 SAVE(I) = ECPT(I)
C
C     TRANSFER TO OPERATIONS DESIRED
C
C              KTRIA1    KTRIA2    KQUAD1    KQUAD2
      GO TO(20,70,100,150),NTYPE
C
C     *** KTRIA1 ***
C
C     SET UP ECPT FOR CALL TO PKTRMS
   20 IF( SAVE(7) .EQ. 0.0E0 ) GO TO 40
      DO 30 I=9,21
   30 ECPT(I) = SAVE(I + 6)
C
      CALL PKTRMS(0)
C
C     SET UP CALL TO PKTRPL
   40 IF( SAVE(9) .EQ. 0.0E0 ) RETURN
      DO 50 I=1,5
   50 ECPT(I) = SAVE(I)
      DO 60 I=6,25
   60 ECPT(I) = SAVE(I + 2)
C
      CALL PKTRPL
      RETURN
C
C     *** KTRIA2 ***
C
   70 IF( SAVE(7) .EQ. 0.0E0 ) RETURN
C     SET UP CALL TO PKTRMS
C
C      ECPT IS OK AS DELIVERED TO THIS ROUTINE
C
      CALL PKTRMS(0)
C
C     SET UP CALL TO PKTRPL
C
      DO 80 I=1,6
   80 ECPT(I) = SAVE(I)
      ECPT(7) = SAVE(7) ** 3  / 12.0E0
      ECPT(8) = SAVE(6)
      ECPT(9) = SAVE(7)
      ECPT(10)= SAVE(8)
      DO 90 I=13,25
   90 ECPT(I) = SAVE(I - 4)
C
      CALL PKTRPL
      RETURN
C
C     *** KQUAD1 ***
C
  100 IF(SAVE(8).EQ.0.0E0)GO TO 120
C
C     SET UP CALL TO PKQDMS
C
      ECPT(9) = SAVE(13)
      DO 110 I=10,26
  110 ECPT(I) = SAVE(I+6)
C
      CALL PKQDMS
C
  120 IF( SAVE(10) .EQ. 0.0E0 ) RETURN
C
C     SET UP CALL TO PKQDPL
C
      DO 130 I=1,6
  130 ECPT(I) = SAVE(I)
      DO 140 I=7,30
  140 ECPT(I) = SAVE(I + 2)
C
      CALL PKQDPL
      RETURN
C
C     *** KQUAD2 ***
C
  150 IF( SAVE(8) .EQ. 0.0E0 ) RETURN
C
C     SET UP CALL TO PKQDMS
C
C      ECPT IS OK AS DELIVERED TO THIS ROUTINE
C
      CALL PKQDMS
C
C     SET UP CALL TO PKQDPL
C
      DO 160 I=1,7
  160 ECPT(I) = SAVE(I)
      ECPT(8) = SAVE(8) **3 / 12.0E0
      ECPT(9) = SAVE(7)
      ECPT(10)= SAVE(8)
      ECPT(11)= SAVE(9)
      DO 170 I=14,30
  170 ECPT(I) = SAVE(I - 4)
C
      CALL PKQDPL
C
      RETURN
      END
