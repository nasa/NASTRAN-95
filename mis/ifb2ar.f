      SUBROUTINE IFB2AR (TYPE,IFB,AR,L)
C
C     THIS ROUTINE STORES IN ARRAY AR(L+1) THE BCD VALUE OF IFB, AND
C     UPDATE THE L COUNTER
C
C     IF TYPE=1, IFB IS AN INTEGER, AND 8 DIGITS ARE USED IN AR, AND
C                L IS INCREASED BY 2 (INTEGER IS RIGHT ADJUSTED)
C     IF TYPE=2, IFB IS A REAL NUMBER, 12 DIGITS ARE USED IN AR, AND
C                L IS INCREASED BY 3
C     IF TYPE=3, IFB IS A BCD WORD, 4 LETTERS ARE USE IN AR, AND
C                L IS INCREASED BY 1
C
      INTEGER      IA,TYPE,IFB,AR(1),L,SUB(2),ZERO(2)
      REAL         RA,X,XL
      CHARACTER*7  FMTX,FMT(10)
      CHARACTER*8  C8
      CHARACTER*10 FMTY,FNT(9)
      CHARACTER*12 C12
      EQUIVALENCE  (IA,RA)
      DATA  FMT / '(F12.9)', '(F12.8)', '(F12.7)', '(F12.6)', '(F12.5)',
     1            '(F12.4)', '(F12.3)', '(F12.2)', '(F12.1)', '(F12.0)'/
      DATA  FNT /'(1X,F11.8)', '(1X,F11.7)', '(1X,F11.6)', '(1X,F11.5)',
     1           '(1X,F11.4)', '(1X,F11.3)', '(1X,F11.2)', '(1X,F11.1)',
     2           '(1X,F11.0)'/
      DATA  ZERO/  4H    ,    4H 0.0  /
      DATA  SUB /  4HIFB2,    4HAR    /
C
      K = -1
      J = TYPE + 1
      GO TO (300,200,300,250), J
 100  K = K + 1
      IF (K) 150,200,250
 150  CALL MESAGE (-37,0,SUB)
C
C     INTEGER, RIGHT ADJUSTED
C
 200  WRITE (C8,210,ERR=300) IFB
      READ  (C8,220) AR(L+1),AR(L+2)
 210  FORMAT (I8)
 220  FORMAT (2A4)
      L = L + 2
      RETURN
C
C     BCD WORD
C
 250  AR(L+1) = IFB
      L = L + 1
      RETURN
C
C     REAL NUMBER
C
 300  IA = IFB
      X  = ABS(RA)
      IF (X .LT. 1.0E-36) GO TO 390
      XL = ALOG10(X)
      IF (XL.GT.-4.0 .AND. XL.LT.10.0) IF (XL-1.0) 350,350,330
 310  WRITE  (C12,320,ERR=100) RA
 320  FORMAT (1P,E12.5)
      GO TO 370
 330  I = XL
      IF (RA .LT. 0.) I = I + 1
      IF (I.LE.0   .OR.   I.GT.9 ) GO TO 310
      IF (RA.GT.0. .AND. XL.GT.0.) GO TO 340
      FMTX = FMT(I)
      GO TO 360
 340  FMTY = FNT(I)
      WRITE (C12,FMTY) RA
      GO TO 370
 350  FMTX = FMT(1)
 360  WRITE  (C12,FMTX) RA
 370  READ   (C12,380) (AR(L+J),J=1,3)
 380  FORMAT (3A4)
      GO TO 400
 390  AR(L+1) = ZERO(1)
      AR(L+2) = ZERO(1)
      AR(L+3) = ZERO(2)
 400  L = L + 3
      RETURN
      END
