      SUBROUTINE TYPFLT (X,Y,XYD,V,FIELD,OPT)
C
C
C     (X,Y) = STARTING OR ENDING POINT OF THE NUMBER TO BE TYPED (ALWAYS
C             LEFT-TO-RIGHT OR TOP-TO-BOTTOM).
C     XYD   = +/-1 IF X = STARTING OR ENDING POINT OF THE NUMBER.
C           = +/-2 IF Y = STARTING OR ENDING POINT OF THE NUMBER.
C     V     = REAL NUMBER TO BE TYPED.
C     FIELD = FIELD WIDTH OF THE NUMBER (IF POSITIVE, THE NUMBER WILL BE
C             CENTERED AT (X,Y) - IF NEGATIVE, THE NUMBER WILL BE TYPED
C             STARTING OR ENDING AT (X,Y) - IF XYD = 1 OR 2, THE NUMBER
C             WILL BE TYPED IN THE X OR Y DIRECTION).
C     OPT   = -1 TO INITIATE  THE TYPING MODE.
C           = +1 TO TERMINATE THE TYPING MODE.
C           =  0 TO TYPE THE NUMBER.
C
      INTEGER          PLOTER,DIR,EXP,D(9),C(100),ASTER,DECPNT,PLUS,
     1                 MINUS,FW,EXPFLD,TRA,XYD,FIELD,OPT
      DOUBLE PRECISION VAL,Z
      COMMON /PLTDAT/  MODEL,PLOTER,SKPPLT(18),SKPA(3),CNTX,CNTY
      DATA    ASTER ,  DECPNT,PLUS,MINUS / 41,44,39,40 /
      DATA    TENM2 ,  TEN7,TEN8  / 1.E-2, 1.E7, 1.E8  /
C
      IF (OPT .EQ. 0) GO TO 20
      CALL TIPE (0,0,0,0,0,OPT)
      GO TO 200
   20 VAL = ABS(V)
      FW  = MIN0(25,IABS(FIELD))
      IF (FW .EQ. 0) GO TO 200
      DO 21 I = 1,FW
      C(I) = 1
   21 CONTINUE
      EXP = 0
      IF (V .NE. 0.) GO TO 30
C
C     INPUT VALUE = 0.
C
      FW   = MIN0(FW,2)
      NSIG = 1
      C(2) = DECPNT
      GO TO 150
C
   30 EXPFLD = 0
      IF (V .LT. 0.) GO TO 35
C
C     SINCE -V- IS POSITIVE, THE NUMBER WILL BE UNSIGNED. IF FIELD.GT.4,
C     THE NUMBER OF SIGNIFICANT DIGITS TYPED WILL BE AT LEAST -FIELD-4-.
C     IF FIELD.LE.4, -FIELD-1-.
C
      NSIG = FW - 4
      IF (NSIG) 40,40,100
C
C     SINCE -V- IS NEGATIVE, THE NUMBER WILL BE SIGNED.  IF FIELD.GT.5,
C     THE NUMBER OF SIGNIFICANT DIGITS TYPED WILL BE AT LEAST -FIELD-5-.
C     IF FIELD.LE.5, -FIELD-2-.
C
   35 NSIG = FW - 5
      IF (NSIG) 40,40,100
C
C     THE NUMBER WILL BE TYPED WITHOUT AN EXPONENT.
C
   40 NSIG = NSIG + 3
      EXPFLD = 1
C
C     THE NUMBER MUST FIRST BE MULTIPLIED BY SOME POWER OF TEN (EXP)
C     SUCH THAT THE PRODUCT IS BETWEEN 10**7 AND 10**8 SO THAT IT
C     CAN BE EXPRESSED AS AN 8-SIGNIFICANT DIGIT INTEGER.
C
  100 Z = 10.D0**IABS(EXP)
      IF (EXP .LT. 0) A = VAL/Z
      IF (EXP .GE. 0) A = VAL*Z
      IF (A .GE. TENM2) GO TO 105
C
C     A .LT. 10**-2
C
      EXP = EXP + 10
      GO TO 100
C
  105 IF (A.GE.TEN7 .AND. A.LT.TEN8) GO TO 115
      IF (A .LT. TEN7) GO TO 110
C
C     A .GE. 10**8
C
      EXP = EXP - 10
      GO TO 100
C
C     A .GE. 10**-2  AND  .LT. 10**7
C
  110 EXP = EXP + 1
      GO TO 100
C
C     A .GE. 10**7  AND  .LT. 10**8  (SEPARATE THE 8 SIGNIFICANT DIGITS)
C
  115 NUM = A
      EXP = -EXP + 7
      DO 116 I = 1,8
      J   = NUM/10**(8-I)
      D(I)= J + 1
      NUM = NUM - J*10**(8-I)
  116 CONTINUE
      IF (EXPFLD .NE. 0) GO TO 130
      IF (EXP.GE.-4 .AND. EXP.LE.NSIG+2) GO TO 135
C
C     USE STANDARD FORMAT (-X.XXX-XX)
C
      NSIG = MIN0(NSIG,8)
      ASSIGN 120 TO TRA
      GO TO 180
  120 N = 0
      IF (V .GT. 0.) GO TO 121
      C(1) = MINUS
      N = 1
  121 C(N+1) = D(1)
      C(N+2) = DECPNT
      N = N + 2
      IF (NSIG .EQ. 1) GO TO 124
      DO 123 I = 2,NSIG
      N = N + 1
      C(N) = D(I)
  123 CONTINUE
  124 IF (EXP .GE. 0) C(N+1) = PLUS
      IF (EXP .LT. 0) C(N+1) = MINUS
      N = N + 1
      NUM = IABS(EXP)
      DO 125 I = 1,2
      J = NUM/10**(2-I)
      N = FW - (2-I)
      C(N) = J + 1
      NUM = NUM - J*10**(2-I)
  125 CONTINUE
      GO TO 150
C
C     STANDARD FORMAT CANNOT BE USED.
C
  130 IF (EXP.LT.NSIG .AND. EXP.GE.-NSIG) GO TO 136
      DO 131 I = 1,FW
      C(I) = ASTER
  131 CONTINUE
      GO TO 150
C
C     THE NUMBER CAN BE EXPRESSED WITHOUT AN EXPONENT.
C
  135 NSIG = MIN0(8,NSIG+3)
  136 ASSIGN 137 TO TRA
      GO TO 180
  137 N = 1
      IF (V .GT. 0.) GO TO 138
      C(1) = MINUS
      N = 2
  138 IF (EXP .GE. 0) GO TO 144
C
C     NEGATIVE EXPONENT
C
      J = NSIG
  141 D(J+1) = D(J)
      J = J - 1
      IF (J .NE. 0) GO TO 141
      D(1) = 1
      ASSIGN 142 TO TRA
      IF (NSIG+N .GE. FW) GO TO 180
      NSIG = NSIG + 1
  142 C(N+0) = D(1)
      C(N+1) = DECPNT
      N = N + 1 + IABS(EXP)
      DO 143 I = 2,NSIG
      C(N) = D(I)
      N = N + 1
  143 CONTINUE
      GO TO 150
C
C     POSITIVE EXPONENT.
C
  144 ASSIGN 145 TO TRA
      IF (NSIG+N .GE. FW) GO TO 180
  145 J = EXP + 1
      DO 146 I = 1,J
      C(N) = D(I)
      N = N + 1
  146 CONTINUE
      C(N) = DECPNT
      J = J + 1
      IF (J .GT. NSIG) GO TO 150
      DO 147 I = J,NSIG
      N = N + 1
      C(N) = D(I)
  147 CONTINUE
C
  150 XX = X
      YY = Y
      IF (FIELD.GT.0 .AND. NSIG.GT.1) GO TO 155
C
C     THE TYPED NUMBER IS NOT TO BE CENTERED AT (X,Y).
C
      DIR = XYD
      GO TO 160
C
C     THE TYPED NUMBER IS TO BE CENTERED AT (X,Y).
C
  155 XY = FW/2
      IF (FW/2 .EQ. (FW+1)/2) XY = XY - .5
      DIR = MAX0(1,IABS(XYD))
      IF (DIR .EQ. 1) XX = X - XY*CNTX
      IF (DIR .EQ. 2) YY = Y - XY*CNTY
C
C     TYPE THE NUMBER.
C
  160 CALL TYPE10 (XX,YY,DIR,C,FW,0)
      GO TO 200
C
C     ROUND THE NUMBER.
C
  180 IF (NSIG .EQ. 8) GO TO 190
      IF (D(NSIG+1) .LE. 5) GO TO 190
      J = NSIG
  181 D(J) = D(J) + 1
      IF (D(J) .LE. 10) GO TO 190
      D(J) = 1
      J = J - 1
      IF (J .NE. 0) GO TO 181
      IF (D(1) .NE. 1) GO TO 190
      J = NSIG - 1
  182 IF (J .EQ. 0) GO TO 183
      D(J+1) = D(J)
      J = J - 1
      GO TO 182
  183 D(1) = 2
      EXP = EXP + 1
  190 GO TO TRA, (120,137,142,145)
C
  200 RETURN
      END
