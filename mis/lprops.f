      SUBROUTINE LPROPS (G)
C     &    ENTRY LPROPD (D)
C
C     THIS ROUTINE RETURNS INTRINSIC LAYER PROPERTIES FOR
C     ALL LAYERS REFERENCING MAT1, MAT2 OR MAT8 PROPERTY
C     ENTRIES IN A STANDARD FORMAT AS REQUIRED FOR FILE PCOMPS
C
      REAL             G(25),MTYPE,NU12,NU21
      DOUBLE PRECISION D(25),DONST
      COMMON  /MATOUT/ RMTOUT(25)
      EQUIVALENCE      (RMTOUT(1),E1),(RMTOUT(2),NU12),(RMTOUT(3),E2)
C
C     SINGLE PRECISION -
C
      DO 10 I=1,25
   10 G(I) = 0.0
      MTYPE = RMTOUT(25)
      MTYP = IFIX(MTYPE+.05) - 2
      IF (MTYP) 20,30,60
C
C****
C     ISOTROPIC MATERIALS, MAT1 IN MAT2 FORMAT
C
C****  LAYER PROPERTY MATRIX
C
   20 CONTINUE
C
C****
C     ANISOTROPIC MATERIALS, MAT2
C
C****  LAYER PROPERTY MATRIX
C
   30 DO 40 I=1,3
   40 G(I) = RMTOUT(I)
      G(5) = RMTOUT(4)
      G(6) = RMTOUT(5)
      G(9) = RMTOUT(6)
      G(4) = G(2)
      G(7) = G(3)
      G(8) = G(6)
C
C**** TRANSVERSE SHEAR FLEXIBILITY MATRIX
      DO 50 I=10,13
      II = I - 9
   50 G(I) = RMTOUT(II)
      G(12) = G(11)
C
C**** THERMAL COEFFICIENTS OF EXPANSION
      G(14) = RMTOUT( 8)
      G(15) = RMTOUT( 9)
      G(16) = RMTOUT(10)
C
C**** ULTIMATE STRENGTH VALUES
      G(17) = RMTOUT(13)
      G(18) = RMTOUT(13)
      G(19) = RMTOUT(14)
      G(20) = RMTOUT(14)
      G(21) = RMTOUT(15)
      G(22) = 0.0
C
C*** RHO, TREF, GE
      G(23) = RMTOUT( 7)
      G(24) = RMTOUT(11)
      G(25) = RMTOUT(12)
      GO TO 160
C
C****
C     ORTHOTROPIC MATERIALS, MAT8
C
C****  LAYER PROPERTY MATRIX
C
   60 NU21 = NU12 * E2 / E1
      CONST= 1.0 - (NU21*NU12)
      G(1) = E1 / CONST
      G(2) = NU12 * E2 / CONST
      G(5) = E2 / CONST
      G(4) = G(2)
      G(9) = RMTOUT(4)
C
C**** TRANSVERSE SHEAR FLEXIBILITY MATRIX
      G(10) = RMTOUT(6)
      G(13) = RMTOUT(5)
C
C**** THERMAL COEFFICIENTS OF EXPANSION
      G(14) = RMTOUT(8)
      G(15) = RMTOUT(9)
C
C**** ULTIMATE STRENGTH VALUES
      G(17) = RMTOUT(11)
      G(18) = RMTOUT(12)
      G(19) = RMTOUT(13)
      G(20) = RMTOUT(14)
      G(21) = RMTOUT(15)
      G(22) = RMTOUT(17)
C
C*** RHO, TREF, GE
      G(23) = RMTOUT( 7)
      G(24) = RMTOUT(10)
      G(25) = RMTOUT(16)
      GO TO 160
C
      ENTRY LPROPD (D)
C     ================
C
C     DOUBLE PRECISION -
C
      DO 100 I=1,25
  100 D(I) = 0.0D0
      MTYPE = RMTOUT(25)
      MTYP  = IFIX(MTYPE+.05) - 2
      IF (MTYP) 110,120,150
C
C****
C     ISOTROPIC MATERIALS, MAT1 IN MAT2 FORMAT
C
C****  LAYER PROPERTY MATRIX
C
  110 CONTINUE
C
C****
C     ANISOTROPIC MATERIALS, MAT2
C
C****  LAYER PROPERTY MATRIX
C
  120 DO 130 I=1,3
  130 D(I) = RMTOUT(I)
      D(5) = RMTOUT(4)
      D(6) = RMTOUT(5)
      D(9) = RMTOUT(6)
      D(4) = D(2)
      D(7) = D(3)
      D(8) = D(6)
C
C**** TRANSVERSE SHEAR FLEXIBILITY MATRIX
      DO 140 I=10,13
      II = I - 9
  140 D(I) = RMTOUT(II)
      D(12) = D(11)
C
C**** THERMAL COEFFICIENTS OF EXPANSION
      D(14) = RMTOUT( 8)
      D(15) = RMTOUT( 9)
      D(16) = RMTOUT(10)
C
C**** ULTIMATE STRENGTH VALUES
      D(17) = RMTOUT(13)
      D(18) = RMTOUT(13)
      D(19) = RMTOUT(14)
      D(20) = RMTOUT(14)
      D(21) = RMTOUT(15)
      D(22) = 0.0D0
C
C*** RHO, TREF, GE
      D(23) = RMTOUT( 7)
      D(24) = RMTOUT(11)
      D(25) = RMTOUT(12)
      GO TO 160
C
C****
C     ORTHOTROPIC MATERIALS, MAT8
C
C****  LAYER PROPERTY MATRIX
C
  150 NU21 = NU12 * E2 / E1
      DONST= 1.0D0 - DBLE(NU21*NU12)
      D(1) = E1 / DONST
      D(2) = NU12 * E2 / DONST
      D(5) = E2 / DONST
      D(4) = D(2)
      D(9) = RMTOUT(4)
C
C**** TRANSVERSE SHEAR FLEXIBILITY MATRIX
      D(10) = RMTOUT(6)
      D(13) = RMTOUT(5)
C
C**** THERMAL COEFFICIENTS OF EXPANSION
      D(14) = RMTOUT(8)
      D(15) = RMTOUT(9)
C
C**** ULTIMATE STRENGTH VALUES
      D(17) = RMTOUT(11)
      D(18) = RMTOUT(12)
      D(19) = RMTOUT(13)
      D(20) = RMTOUT(14)
      D(21) = RMTOUT(15)
      D(22) = RMTOUT(17)
C
C*** RHO, TREF, GE
      D(23) = RMTOUT( 7)
      D(24) = RMTOUT(10)
      D(25) = RMTOUT(16)
C
  160 RETURN
      END
