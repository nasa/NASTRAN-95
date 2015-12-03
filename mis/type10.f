      SUBROUTINE TYPE10 (X,Y,XYD,CHR,NN,OPT)
C                                                	
C     (X,Y) = STARTING OR ENDING POINT OF THE LINE TO BE TYPED (ALWAYS
C             LEFT-TO-RIGHT OR TOP-TO-BOTTOM)
C     XYD   = (+/-)1 IF X = STARTING OR ENDING POINT OF THE LINE
C           = (+/-)2 IF Y = STARTING OR ENDING POINT OF THE LINE
C     CHR   = CHARACTERS TO BE TYPED
C     NN    = NUMBER OF CHARACTERS
C     OPT   = -1 TO INITIATE  THE TYPING MODE
C           = +1 TO TERMINATE THE TYPING MODE
C           =  0 TO TYPE A LINE
C
      INTEGER         XYD,CHR(1),OPT,OPTX,A(6),TYPE,D,PLTYPE
      REAL            XY(2,2),CSCALE
      COMMON /PLTDAT/ SKPPLT(2),XYMIN(2),XYMAX(15),CSCALE,SKPA(3),
     1                CNTCHR(6),PLTYPE
      DATA    A(6)  , TYPE,LSTCHR / 0, 4, 48 /
C
      IF (PLTYPE .LT. 0) GO TO 175
      OPTX = -1
      IF (OPT) 200,100,150
  100 A(5) = IFIX(CSCALE+.44)
      XY(1,1) = X
      XY(2,1) = Y
      XY(1,2) = X
      XY(2,2) = Y
      N = 1
      IF (N .LE. 0) N = 1
C
C     SCREEN OUT TRAILING BLANKS
C
      DO 102 J = 1,NN
      IF (IABS(CHR(J)) .NE. 48) N = J
  102 CONTINUE
      IF (N.EQ.1 .AND. IABS(CHR(1)).EQ.48) RETURN
      D = MAX0(IABS(XYD),1)
      S = CNTCHR(D)
      IF (XYD.EQ.-1 .OR. XYD.EQ.2) S = -S
C
C     TYPE THE LINE
C
      DO 125 J = 1,N
      XY(D,2) = XY(D,1) + S*FLOAT(J-1)
      DO 105 I = 1,2
      IF (XY(I,2)+.1.LT.XYMIN(I) .OR. XY(I,2)-.1.GT.XYMAX(I)) GO TO 125
      A(I+2) = XY(I,2) + .1
  105 CONTINUE
C
C     MAKE SURE EACH CHARACTER IS A VALID CHARACTER (UNLESS NN.LE.0)
C
      K = J
      IF (XYD .LT. 0) K = N - J + 1
      A(2) = IABS(CHR(K))
      IF (NN  .LE. 0) GO TO 120
      IF (A(2).EQ.0 .OR. A(2).GT.LSTCHR) GO TO 125
      IF (A(2) .EQ. 0) GO TO 125
C
C     TYPE THE CHARACTER
C
  120 A(1) = TYPE
      IF (OPTX .EQ. 0) GO TO 121
      A(1) = TYPE + 10
      OPTX = 0
  121 CALL WPLT10 (A,0)
  125 CONTINUE
      GO TO 200
C
C     TERMINATE THE TYPING MODE
C
  150 CALL WPLT10 (A,1)
      OPTX = -1
      GO TO 200
C
C     DRAW THE LINE OF CHARACTERS
C
  175 CALL DRWCHR (X,Y,XYD,CHR,NN,OPT)
C
  200 RETURN
      END
