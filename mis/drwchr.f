      SUBROUTINE DRWCHR (X,Y,XYD,CHR,NN,OPT)
C
C     (X,Y)  = STARTING OR ENDING POINT OF THE LINE TO BE TYPED (ALWAYS
C              LEFT-TO-RIGHT OR TOP-TO-BOTTOM)
C     XYD    = (+/-)1 IF X = STARTING OR ENDING POINT OF THE LINE
C            = (+/-)2 IF Y = STARTING OR ENDING POINT OF THE LINE
C     CHR    = CHARACTERS TO BE DRAWN
C     NN     = NUMBER OF CHARACTERS
C     OPT    = -1 TO INITIATE  THE TYPING MODE
C            = +1 TO TERMINATE THE TYPING MODE
C            =  0 TO TYPE A LINE
C     CSCALE = SCALE FOR CHARACTER SIZE (REAL)
C
      INTEGER         XYD,CHR(1),OPT,CHRIND,XYCHR,D
      REAL            SAVE(2,2),XY(2,2),XYC(2,2),CSCALE
      COMMON /PLTDAT/ SKPLT(2),REG(2,2),XYMAX(2),EDGE(11),CSCALE,
     1                SKPA(3),CNTCHR(2)
      COMMON /CHRDRW/ LSTIND,CHRIND(60),XYCHR(2,1)
      DATA    LSTCHR/ 48 /
C
      IF (OPT .EQ. 0) GO TO 100
      CALL LINE (0,0,0,0,0,OPT)
      GO TO 200
C
  100 N = NN
      IF (N .LE. 0) N = 1
      D = MAX0(IABS(XYD),1)
      S = CNTCHR(D)
      IF (XYD.EQ.-1 .OR. XYD.EQ.2) S = -S
      XYC(1,1) = 3.0*CSCALE
      XYC(2,1) = 3.0*CSCALE
      XY(1,1)  = X - XYC(1,1)
      XY(2,1)  = Y - XYC(2,1)
      XY(1,2)  = XY(1,1)
      XY(2,2)  = XY(2,1)
      DO 110 I = 1,2
      SAVE(I,1)= REG(I,1)
      REG(I,1) = AMAX1(-EDGE(I),REG(I,1)-XYC(I,1))
      SAVE(I,2)= REG(I,2)
      REG(I,2) = AMIN1(XYMAX(I)+EDGE(I),REG(I,2)+XYC(I,1))
  110 CONTINUE
C
C     TYPE THE LINE.
C
      DO 125 J = 1,N
      XY(D,2)  = XY(D,1) + S*FLOAT(J-1)
C
C     MAKE SURE EACH CHARACTER IS A VALID CHARACTER.
C
      I = J
      IF (XYD .LT. 0) I = N - J + 1
      K = CHR(I)
      IF (NN.NE.0 .AND. K.GE.LSTCHR) GO TO 125
      IF (K .GT. LSTIND) GO TO 125
C
C     DRAW THE CHARACTER.
C
  120 N1 = CHRIND(K)
      IF (N1 .GT. 0) GO TO 121
      K  = -N1
      GO TO 120
  121 N2 = CHRIND(K+1)
      IF (N2 .GT. 0) GO TO 122
      K  = K + 1
      GO TO 121
C
  122 N2 = N2 - 1
      DO 124 L = N1,N2
      DO 123 I = 1,2
      XYC(I,1) = XYC(I,2)
      XYC(I,2) = XY(I,2) + CSCALE*FLOAT(IABS(XYCHR(I,L)))
  123 CONTINUE
      IF (L.EQ.N1 .OR. XYCHR(1,L).LT.0 .OR. XYCHR(2,L).LT.0) GO TO 124
      CALL LINE (XYC(1,1),XYC(2,1),XYC(1,2),XYC(2,2),1,0)
  124 CONTINUE
  125 CONTINUE
C
      DO 190 I = 1,2
      REG(I,1) = SAVE(I,1)
      REG(I,2) = SAVE(I,2)
  190 CONTINUE
C
  200 RETURN
      END
