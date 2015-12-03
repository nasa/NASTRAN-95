      SUBROUTINE SINC0S1 (ROW,SICK, D,O,COS)
C                    =
C     SUBROUTINE SICOX (D,O,COS)
C
C     THIS ROUTINE WAS CALLED SICOX BEFORE, WITH ENTRY POINT SINCAS
C                                                                =
C     THIS ROUTINE IS CALLED ONLY BY TRIDI SUBROUTINE, WHICH IS CALLED
C     ONLY BY VALVEC
C
C     IT CALCULATES SINES AND COSINES FOR GIVENS TRIDIAGONALIZATION
C
      INTEGER          ROWP2,ROW,SICK
      REAL             D(1),O(1),COS(1),Z
      COMMON /GIVN  /  TITLE(100),N
C
C     D   = DIAGONAL AND SINES.
C     O   = OFF-DIAGONAL.
C     COS = COSINES.
C
C     RETURN
C
C
C     ENTRY SINCAS (ROW,SICK)
C     =======================
C
C     CALCULATE THE SINES AND COSINES OF ROW -ROW-.
C
      SICK  = 0
      ROWP2 = ROW + 2
      DO 105 I = ROWP2,N
      IF (D(I) .EQ. 0.0) GO TO 101
C
C     CALCULATE THE ROTATION.
C
      SICK = 1
      Z    = SQRT(D(I)**2 + D(ROW+1)**2)
      D(I) = D(I)/Z
      COS(I) = D(ROW+1)/Z
      D(ROW+1) = Z
      IF (COS(I) .GE. 0.0) GO TO 105
      COS(I) = ABS(COS(I))
      D(I)   = -D(I)
      D(ROW+1) = -D(ROW+1)
      GO TO 105
C
C     NO ROTATION.
C
  101 COS(I) = 1.0
  105 CONTINUE
      O(ROW) = D(ROW+1)
      RETURN
      END
