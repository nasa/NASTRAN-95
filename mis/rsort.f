      SUBROUTINE RSORT (NWDS,KEYWX,L,NX)
C
C     RSORT SORTS REAL NUMBERS IN L(NWDS,NCOL)
C           WHERE NCOL = IABS(NX)/NWDS
C
C     IF KEYWX .LT. 0 SORT BY ABSOLUTE VALUE
C     IF NX    .LT. 0 SORT IN DECREASING SEQUENCE
C
C     COMMENTS FROM G.C./UNISYS
C     THIS ROUTINE IS INEFFICIENT FOR LARGE ARRAY OF L
C
      LOGICAL MAG,BCK
      REAL    L(1),TEMP(50)
      INTEGER NAM(2)
      COMMON /SYSTEM/ IBUF,NOUT
      DATA    NAM   / 4HRSOR, 4HT    /
C
      IF (NWDS .LE. 50) GO TO 30
      WRITE  (NOUT,20)
   20 FORMAT (' *** ARRAY TEMP OF 50 EXCEEDED')
      CALL MESAGE (-37,0,NAM)
C
   30 MAG = .FALSE.
      BCK = .FALSE.
      IF (KEYWX .LT. 0) MAG = .TRUE.
      IF (NX    .LT. 0) BCK = .TRUE.
      KEYWD= IABS(KEYWX)
      NNN  = IABS(NX)
      III  = NWDS+KEYWD
      IA   = NWDS-KEYWD
      IF (NNN-NWDS-NWDS .LT. 0) GO TO 150
      DO 140 I = III,NNN,NWDS
      JJ = I-NWDS
      IF (BCK) GO TO 40
      IF (MAG) IF (ABS(L(I))-ABS(L(JJ))) 50,140,140
      IF (L(I)-L(JJ)) 50,140,140
   40 IF (MAG) IF (ABS(L(JJ))-ABS(L(I))) 50,140,140
      IF (L(JJ)-L(I)) 50,140,140
   50 JJ = JJ-NWDS
      IF (JJ .LE. 0) GO TO 70
      IF (BCK) GO TO 60
      IF (MAG) IF (ABS(L(I))-ABS(L(JJ))) 50,80,80
      IF (L(I) - L(JJ)) 50,80,80
   60 IF (MAG) IF (ABS(L(JJ))-ABS(L(I))) 50,80,80
      IF (L(JJ)-L(I)) 50,80,80
   70 JJ = NWDS
      GO TO 90
   80 JJ = JJ+IA+NWDS
   90 II = I-KEYWD
      DO 100 J = 1,NWDS
      II = II+1
  100 TEMP(J) = L(II)
  110 IIA = II-NWDS
      L(II) = L(IIA)
      II = II-1
      IF (II-JJ) 120,120,110
  120 II = II-NWDS
      DO 130 J = 1,NWDS
      II = II+1
  130 L(II) = TEMP(J)
  140 CONTINUE
C
  150 RETURN
      END
