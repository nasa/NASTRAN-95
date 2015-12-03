      SUBROUTINE TIPE (X,Y,XYD,CHR,N,OPT)
C
C     (X,Y) = STARTING OR ENDING POINT OF THE LINE TO BE TYPED (ALWAYS
C             LEFT-TO-RIGHT OR TOP-TO-BOTTOM.
C     XYD   = +/-1 IF X = STARTING OR ENDING POINT OF THE LINE.
C           = +/-2 IF Y = STARTING OR ENDING POINT OF THE LINE.
C     CHR   = CHARACTERS TO BE TYPED.
C     N     = NUMBER OF CHARACTERS.
C     OPT   = -1 TO INITIATE  THE TYPING MODE.
C           = +1 TO TERMINATE THE TYPING MODE.
C           =  0 TO TYPE A LINE.
C
      INTEGER         XYD,CHR(1),OPT,PLOTER,CHAR,C(80),BLANK,LSTCHR,
     1                CHARX,D
      REAL            XY(2,2)
      COMMON /PLTDAT/ MODEL,PLOTER,SKPPLT(18),SKPA(3),CNTCHR(2)
      COMMON /CHAR94/ CHAR(60)
      DATA    BLANK , LSTCHR / 48,47 /
C
      IF (OPT .NE. 0)  GO TO 150
C
C     OPT = 0.
C
      D = MAX0(IABS(XYD),1)
      S = CNTCHR(D)
      IF (XYD.EQ.-1 .OR. XYD.EQ.2) S = -S
      XY(1,1) = X
      XY(2,1) = Y
      XY(1,2) = XY(1,1)
      XY(2,2) = XY(2,1)
C
C     PRINT A MAXIMUM OF 80 CHARACTERS AT A TIME.
C
      DO 130 J = 1,N,80
      IF (XYD .LT. 0) GO TO 105
      L1 = J
      L2 = L1 + 79
      IF (L2 .GT. N) L2 = N
      GO TO 106
  105 L2 = N  - J + 1
      L1 = L2 - 79
      IF (L1 .LE. 0) L1 = 1
C
  106 NC = 0
      DO 120 L = L1,L2
      CHARX = CHR(L)
      DO 110 I = 1,LSTCHR
      IF (CHARX .EQ. CHAR(I)) GO TO 111
  110 CONTINUE
      I  = BLANK
  111 NC = NC + 1
      C(NC) = I
  120 CONTINUE
C
C     TYPE THE -NC- CHARACTERS JUST PROCESSED.
C
      XY(D,2) = XY(D,1) + S*FLOAT(L1-1)
      CALL TYPE10 (XY(1,2),XY(2,2),XYD,C,NC,0)
      GO TO 130
  130 CONTINUE
      GO TO 200
C
C     OPT = +/-1
C
  150 CALL TYPE10 (0,0,0,0,0,OPT)
      GO TO 200
  200 RETURN
      END
