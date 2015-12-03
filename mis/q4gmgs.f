      SUBROUTINE Q4GMGS (MID,FACTOR,G)
C     &    ENTRY Q4GMGD (MID,FACTOD,D)
C
C
C     MATERIAL PROPERTY MATRIX GENERATOR ROUTINE FOR QUAD4 ELEMENT
C
C     THIS ROUTINE BUILDS THE MATERIAL PROPERTY MATRIX, G, USING THE
C     OUTPUT OF SUBROUTINE 'MAT' (/MATOUT/).
C
C     ALL THE MATERIAL OPTIONS, ISOTROPIC, ORTHOTROPIC, AND ANISOTROPIC,
C     ARE AVAILABLE.
C
C     OUTPUT WILL BE G(9) OR D(9) FOR MID1, MID2 AND MID4.
C     FOR MID3,  G(4) OR D(4) IS SENT BACK.
C
      REAL             FACTOR,G(9),CONST,MTYPE,NU12,NU21
      DOUBLE PRECISION FACTOD,D(9),DONST
      COMMON  /MATOUT/ RMTOUT(25)
      EQUIVALENCE      (RMTOUT(1),E1),(RMTOUT(2),NU12),(RMTOUT(3),E2)
C
C     SINGLE PRECISION SECTION -
C
      DO 10 I=1,9
   10 G(I) = 0.0
      MTYPE= RMTOUT(25)
      MTYP = IFIX(MTYPE+.05) - 2
      IF (MTYP) 20,30,80
C
C     ISOTROPIC MATERIALS (MAT1)
C
   20 IF (MID .NE. 3) GO TO 40
      G(1) = RMTOUT(6)
      G(4) = G(1)
      GO TO 100
C
C     ANISOTROPIC MATERIALS (MAT2)
C
   30 IF (MID .EQ. 3) GO TO 60
   40 DO 50 I=1,3
   50 G(I) = RMTOUT(I)
      G(4) = G(2)
      G(5) = RMTOUT(4)
      G(6) = RMTOUT(5)
      G(7) = G(3)
      G(8) = G(6)
      G(9) = RMTOUT(6)
      GO TO 100
C
   60 DO 70 I=1,4
   70 G(I) = RMTOUT(I)
      G(3) = G(2)
      GO TO 100
C
C     ORTHOTROPIC MATERIALS (MAT8)
C
   80 IF (MID .EQ. 3) GO TO 90
      NU21 = NU12 * E2 / E1
      CONST= 1.0 - (NU21*NU12)
      G(1) = E1 / CONST
      G(2) = NU12 * E2 / CONST
      G(4) = G(2)
      G(5) = E2 / CONST
      G(9) = RMTOUT(4)
      GO TO 100
C
   90 G(1) = RMTOUT(6)
      G(4) = RMTOUT(5)
      IF (G(1).EQ.0.0 .AND. G(4).EQ.0.0) GO TO 120
C
C     STANDARD RETURN
C
  100 DO 110 I=1,9
  110 G(I) = G(I)*FACTOR
      GO TO 310
C
C     FATAL RETURN
C
  120 MID = -MID
      GO TO 310
C
      ENTRY Q4GMGD (MID,FACTOD,D)
C     ===========================
C
      DO 200 I=1,9
  200 D(I) = 0.0D0
      MTYPE= RMTOUT(25)
      MTYP = IFIX(MTYPE+.05) - 2
      IF (MTYP) 210,220,270
C
C     ISOTROPIC MATERIALS (MAT1)
C
  210 IF (MID .NE. 3) GO TO 230
      D(1) = RMTOUT(6)
      D(4) = D(1)
      GO TO 290
C
C     ANISOTROPIC MATERIALS (MAT2)
C
  220 IF (MID .EQ. 3) GO TO 250
  230 DO 240 I=1,3
  240 D(I) = RMTOUT(I)
      D(4) = D(2)
      D(5) = RMTOUT(4)
      D(6) = RMTOUT(5)
      D(7) = D(3)
      D(8) = D(6)
      D(9) = RMTOUT(6)
      GO TO 290
C
  250 DO 260 I=1,4
  260 D(I) = RMTOUT(I)
      D(3) = D(2)
      GO TO 290
C
C     ORTHOTROPIC MATERIALS (MAT8)
C
  270 IF (MID .EQ. 3) GO TO 280
      NU21 = NU12 * E2 / E1
      DONST= 1.0D0 - DBLE(NU21*NU12)
      D(1) = E1 / DONST
      D(2) = NU12 * E2 / DONST
      D(4) = D(2)
      D(5) = E2 / DONST
      D(9) = RMTOUT(4)
      GO TO 290
C
  280 D(1) = RMTOUT(6)
      D(4) = RMTOUT(5)
      IF (D(1).EQ.0.0D0 .AND. D(4).EQ.0.0D0) GO TO 120
C
C     STANDARD RETURN
C
  290 DO 300 I=1,9
  300 D(I) = D(I)*FACTOD
C
  310 RETURN
      END
