      SUBROUTINE STACK (IDEG,NEW,ILD,IW)
C
C     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
C
C     STACK POINTS OF ZERO DEGREE AT END OF THE NUMBERING.
C     IW IS SCRATCH STORAGE.
C
      DIMENSION       IDEG(1),  NEW(1),   ILD(1),   IW(1)
      COMMON /BANDS / NN
      COMMON /BANDD / DUM(5),   KT
C
      KT  = 0
      NN1 = NN - 1
C
C     LIST POINTS OF ZERO DEGREE AND INCREMENT COUNTER KT.
C
      DO 10 I = 1,NN
      IF (IDEG(I) .GT. 0) GO TO 10
      KT = KT + 1
      IW(KT) = ILD(I)
   10 CONTINUE
      IF (KT .LE. 0) GO TO 80
C
C     SORT LIST OF RENUMBERED NUMBERS TO BE STACKED.
C
      IF (KT .LE. 1) GO TO 40
      KT1 = KT-1
      DO 30 I = 1,KT1
      K = KT - I
      KFLAG = 0
      DO 20 J = 1,K
      J1 = J + 1
      IF (IW(J) .LE. IW(J1))  GO TO 20
      KFLAG = 1
      L = IW(J)
      IW(J ) = IW(J1)
      IW(J1) = L
   20 CONTINUE
      IF (KFLAG .EQ. 0) GO TO 40
   30 CONTINUE
   40 CONTINUE
C
C     STACK POINTS OF ZERO DEGREE AT END OF NEW.
C
      DO 70 L = 1,KT
      I = IW(L) - L + 1
      K = NEW(I)
      IF (I .GE. NN) GO TO 60
      DO 50 J = I,NN1
   50 NEW(J) = NEW(J+1)
   60 NEW(NN) = K
   70 CONTINUE
C
C     CORRECT ILD, THE INVERSE OF NEW.
C
   80 DO 90 I = 1,NN
      K = NEW(I)
   90 ILD(K) = I
      RETURN
      END
