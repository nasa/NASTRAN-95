      SUBROUTINE ANGTRS (THETA,K,TRANS)
C     &    ENTRY ANGTRD
C
C     ROUTINE TO CALCULATE AND OUTPUT THE INPLANE ROTATION
C     TRANSFORMATION IN 3-D USING THE ANGLE OF ROTATION.
C
C     IF K=1, TRANS OR TRAND WILL BE TRANSPOSED AND THEN RETURNED.
C
C     SINGLE PRECISION -
C
      REAL               TRANS(9)
      DOUBLE PRECISION   TRAND(9), THETAD
C
      DO 10 I = 1,9
   10 TRANS(I) = 0.0
C
      TRANS(1) =  COS(THETA)
      TRANS(2) =  SIN(THETA)
      TRANS(4) = -TRANS(2)
      TRANS(5) =  TRANS(1)
      TRANS(9) =  1.0
C
      IF (K .NE. 1) GO TO 30
      TRANS(2) = -TRANS(2)
      TRANS(4) = -TRANS(4)
      RETURN
C
      ENTRY ANGTRD (THETAD,K,TRAND)
C     =============================
C
C     DOUBLE PRECISION -
C
      DO 20 I = 1,9
   20 TRAND(I) = 0.0D0
C
      TRAND(1) =  DCOS(THETAD)
      TRAND(2) =  DSIN(THETAD)
      TRAND(4) = -TRAND(2)
      TRAND(5) =  TRAND(1)
      TRAND(9) =  1.0D0
C
      IF (K .NE. 1) GO TO 30
      TRAND(2) = -TRAND(2)
      TRAND(4) = -TRAND(4)
   30 RETURN
      END
