      SUBROUTINE PRINT (X,Y,XYD,CHR,N,OPT)
C
C     (X,Y) = STARTING OR ENDING POINT OF THE LINE TO BE PRINTED (ALWAYS
C             LEFT-TO-RIGHT OR TOP-TO-BOTTOM).
C     CHR   = CHARACTERS TO BE PRINTED (4 PER WORD).
C     N     = NUMBER OF 4 CHARACTER WORDS.
C     XYD   = +/-1 IF X = STARTING OR ENDING POINT OF THE LINE.
C     ...   = +/-2 .. Y = ........ .. ...... ..... .. ... .....
C     OPT   = -1 TO INITIATE  THE TYPING MODE.
C     ...   = +1 .. TERMINATE ... ...... .....
C     ...   =  0 .. PRINT A LINE.
C
      EXTERNAL        ORF,KRSHFT,KLSHFT
      INTEGER         XYD,CHR(1),OPT,ORF,C(80),BLANK,BLNK,CHARX,D
      REAL            XY(2,2)
      COMMON /PLTDAT/ SKPPLT(20),SKPA(3),CNTCHR(2)
      COMMON /SYSTEM/ SKPSYS(40),NCPW
      DATA    BLANK / 1H /
C
      IF (OPT .NE. 0) GO TO 150
      BLNK = KRSHFT(KLSHFT(BLANK,1),1)
      D = MAX0(IABS(XYD),1)
      S = CNTCHR(D)
      IF (XYD.EQ.-1 .OR. XYD.EQ.2) S = -S
      XY(1,1) = X
      XY(2,1) = Y
      XY(1,2) = XY(1,1)
      XY(2,2) = XY(2,1)
C
C     SEPARATE 80 CHARACTERS AT A TIME.
C
      DO 130 J = 1,N,20
      IF (XYD .LT. 0) GO TO 105
      L1 = J
      L2 = L1 + 19
      IF (L2 .GT. N) L2 = N
      GO TO 106
  105 L2 = N - J + 1
      L1 = L2 - 19
      IF (L1 .LE. 0) L1 = 1
C
  106 NC = 0
      DO 120 L = L1,L2
      DO 110 I = 1,4
      CHARX = KRSHFT(CHR(L),NCPW-I)
      NC = NC + 1
      C(NC) = ORF(KLSHFT(CHARX,NCPW-1),BLNK)
  110 CONTINUE
  120 CONTINUE
C
C     TYPE THE -NC- CHARACTERS JUST SEPARATED.
C
      XY(D,2) = XY(D,1) + S*FLOAT(L1-1)
      CALL TIPE (XY(1,2),XY(2,2),XYD,C,NC,0)
  130 CONTINUE
      GO TO 200
C
C     OPT = +/-1
C
  150 CALL TIPE (0,0,0,0,0,OPT)
  200 RETURN
      END
