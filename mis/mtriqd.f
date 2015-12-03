      SUBROUTINE MTRIQD (NTYPE)
C
C
C     8/18/67           E C P T     L I S T I N G
C
C     ECPT    TRPLT     TRIA1     TRIA2     QDPLT     QUAD1     QUAD2
C     *****************************************************************
C
C      1     ELEM ID   ELEM ID   ELEM ID   ELEM ID   ELEM ID   ELEM ID
C      2     GRID A    GRID A    GRID A    GRID A    GRID A    GRID A
C      3     GRID B    GRID B    GRID B    GRID B    GRID B    GRID B
C      4     GRID C    GRID C    GRID C    GRID C    GRID C    GRID C
C      5     THETA     THETA     THETA     GRID D    GRID D    GRID D
C      6     MATID1    MATID1    MAT ID    THETA     THETA     THETA
C      7     I         T1        T         MATID1    MATID1    MAT ID
C      8     MATID2    MATID2    NS MASS   I         T1        T
C      9     T2        I         CSID 1    MATID2    MATID2    NS MASS
C     10     NS MASS   MATID3    X1        T2        I         CSID 1
C     11     Z1        T2        Y1        NS MASS   MATID3    X1
C     12     Z2        NS MASS   Z1        Z1        T2        Y1
C     13     CSID 1    Z1        CSID 3    Z2        NS MASS   Z1
C     14     X1        Z2        X2        CSID 1    Z1        CSID 2
C     15     Y1        CSID 1    Y2        X1        Z2        X2
C     16     Z1        X1        Z2        Y1        CSID 1    Y2
C     17     CSID 2    Y1        CSID 3    Z1        X1        Z2
C     18     X2        Z1        X3        CSID 2    Y1        CSID 3
C     19     Y2        CSID 2    Y3        X2        Z1        X3
C     20     Z2        X2        Z3        Y2        CSID 2    Y3
C     21     CSID 3    Y2        TEMP      Z2        X2        Z3
C     22     X3        Z2                  CSID 3    Y2        CSID 4
C     23     Y3        CSID 3              X3        Z2        X4
C     24     Z3        X3                  Y3        CSID 3    Y4
C     25     TEMP      Y3                  Z3        X3        Z4
C     26               Z3                  CSID 4    Y3        TEMP
C     27               TEMP                X4        Z3
C     28                                   Y4        CSID 4
C     29                                   Z4        X4
C     30                                   TEMP      Y4
C     31                                             Z4
C     32                                             TEMP
C
C
      LOGICAL         HEAT
      DIMENSION       SAVE(32),ISAVE(32)
      COMMON /SMA2ET/ ECPT(100)
      COMMON /SMA2HT/ HEAT
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/ RHO
      EQUIVALENCE     (SAVE(1),ISAVE(1),ECPT(50))
C
C     THIS SUBROUTINE INCORPORATES TRIA1, QUAD1, TRIA2, QUAD2
C
C             NTYPE = 1  IMPLIES MTRIA1
C             NTYPE = 2  IMPLIES MTRIA2
C             NTYPE = 3  IMPLIES MQUAD1
C             NTYPE = 4  IMPLIES MQUAD2
C
C     IF (I . EQ. 0) THEN COMPUTE UNCOUPLED MASS
C
C     CALL MASSTQ (NARG)
C          WHERE   NARG = 5 FOR TRIA1
C                  NARG = 4 FOR TRIA2
C                  NARG = 2 FOR QUAD1
C                  NARG = 1 FOR QUAD2
C
C     CALLS FROM THIS ROUTINE CAN BE MADE TO
C
C            MTRPLT - TRIANGULAR PLATE ROUTINE
C            MQDPLT - QUADRILATERAL PLATE ROUTINE
C            MASSTQ - UNCOUPLED MASS COMBINATION ELEMENT ROUTINE
C
C     ALL INSERTIONS OF 6X6 ELEMENT MASS MATRICES ARE HANDLED BY
C     THE ABOVE ROUTINES.
C
C     THE SAVED ECPT IS EQUIVALENCED TO ECPT(50)
C
C
C     SAVE THE INCOMING ECPT
C
      INFLAG = 4
      DO 10 I = 1,32
   10 SAVE(I) = ECPT(I)
C
C     TRANSFER TO OPERATIONS DESIRED
C
C            MTRIA1 MTRIA2 MQUAD1 MQUAD2
      GO TO (   20,    60,   100,   150), NTYPE
C
C     *** MTRIA1 ***
C
C     SET UP ECPT FOR CALL TO MTRPLT.  FIRST CHECK I EQUAL ZERO
C
   20 IF (SAVE(9) .NE. 0.0) GO TO 30
      NARG = 5
      CALL MASSTQ (NARG)
      GO TO 200
C
   30 DO 40 I = 1,5
   40 ECPT(I) = SAVE(I)
      DO 50 I = 6,25
   50 ECPT(I) = SAVE(I+2)
      MATID   = ISAVE(6)
      IF (SAVE(7) .EQ. 0.0) GO TO 54
      CALL MAT (ECPT(1))
      ECPT(10) = SAVE(12) + RHO*SAVE(7)
C
      GO TO 56
   54 ECPT(10) = SAVE(12)
   56 IF (.NOT.HEAT) CALL MTRPLT
      GO TO 200
C
C     *** MTRIA2 ***
C
C     SET UP ECPT FOR CALL TO MTRPLT
C
   60 IF (SAVE(7) .NE. 0.0) GO TO 70
      NARG = 4
      CALL MASSTQ (NARG)
      GO TO 200
C
   70 DO 80 I = 1,6
   80 ECPT(I) = SAVE(I)
      ECPT(7) = SAVE(7)**3/12.0
      ECPT(8) = SAVE(6)
      ECPT(9) = SAVE(7)
      MATID   = ISAVE(6)
      CALL MAT (ECPT(1))
      ECPT(10) = SAVE(8) + RHO*SAVE(7)
      DO 90 I = 13,25
   90 ECPT(I) = SAVE(I-4)
C
      IF (.NOT. HEAT) CALL MTRPLT
      GO TO 200
C
C     *** MQUAD1 ***
C
C      SET UP ECPT FOR CALL TO MQDPLT.  FIRST CHECK I EQUAL ZERO
C
  100 IF (SAVE(10) .NE. 0.0) GO TO 110
      NARG = 2
      CALL MASSTQ (NARG)
      GO TO 200
C
  110 DO 130 I = 1,6
  130 ECPT(I) = SAVE(I)
      DO 140 I = 7,30
  140 ECPT(I) = SAVE(I+2)
      MATID = ISAVE(7)
      IF (SAVE(8) .EQ. 0.0) GO TO 144
      CALL MAT (ECPT(1))
      ECPT(11) = SAVE(13) + RHO*SAVE(8)
C
      GO TO 146
  144 ECPT(11) = SAVE(13)
  146 IF (.NOT.HEAT) CALL MQDPLT
      GO TO 200
C
C     *** MQUAD2 ***
C
C     SET UP ECPT FOR CALL TO MQDPLT
C
  150 IF (SAVE(8) .NE. 0.0) GO TO 160
      NARG = 1
      CALL MASSTQ (NARG)
      GO TO 200
C
  160 DO 170 I = 1,7
  170 ECPT(I) = SAVE(I)
      ECPT(8) = SAVE(8)**3/12.0
      ECPT(9) = SAVE(7)
      ECPT(10)= SAVE(8)
      MATID   = ISAVE(7)
      CALL MAT (ECPT(1))
      ECPT(11) = SAVE(9) + RHO*SAVE(8)
      DO 180 I = 14,30
  180 ECPT(I) = SAVE(I-4)
C
      IF (.NOT. HEAT) CALL MQDPLT
  200 RETURN
      END
