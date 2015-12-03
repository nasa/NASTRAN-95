      SUBROUTINE XYGRAF (GRAPH)
C
      LOGICAL         EXCEED
      INTEGER         Z,TITLEC,XTITLE,TITLEL,TITLER,SYSBUF,M(5),
     1                NU(5,10)
      REAL            GRAPH(3,8)
      CHARACTER       UFM*23,UWM*25,UIM*29
      COMMON /XMSSG / UFM,UWM,UIM
      COMMON /SYSTEM/ SYSBUF,L,IDUM(6),NLPP,IDUM2(2),LINES,ITLNS
      COMMON /XYPPPP/ IFRAME,TITLEC(32),TITLEL(14),TITLER(14),
     1                XTITLE(32),ID(300),MAXPLT,XMIN,XINC,EXCEED,I123,
     2                MAXROW
      COMMON /ZZZZZZ/ Z(1)
C
      DATA    NU(1, 1),NU(1, 2),NU(1, 3) /  4H****   ,4H **   ,4H**** /,
     1        NU(2, 1),NU(2, 2),NU(2, 3) /  4H*  *   ,4H  *   ,4H   * /,
     2        NU(3, 1),NU(3, 2),NU(3, 3) /  4H*  *   ,4H  *   ,4H  *  /,
     3        NU(4, 1),NU(4, 2),NU(4, 3) /  4H*  *   ,4H  *   ,4H *   /,
     4        NU(5, 1),NU(5, 2),NU(5, 3) /  4H****   ,4H****  ,4H**** /
C
      DATA    NU(1, 4),NU(1, 5),NU(1, 6) /  4H****   ,4H* *   ,4H**** /,
     1        NU(2, 4),NU(2, 5),NU(2, 6) /  4H   *   ,4H* *   ,4H*    /,
     2        NU(3, 4),NU(3, 5),NU(3, 6) /  4H ***   ,4H****  ,4H**** /,
     3        NU(4, 4),NU(4, 5),NU(4, 6) /  4H   *   ,4H  *   ,4H   * /,
     4        NU(5, 4),NU(5, 5),NU(5, 6) /  4H****   ,4H  *   ,4H**** /
C
     1        NU(1, 7),NU(1, 8),NU(1, 9) /  4H****   ,4H****  ,4H**** /,
     2        NU(2, 7),NU(2, 8),NU(2, 9) /  4H*      ,4H   *  ,4H*  * /,
     3        NU(3, 7),NU(3, 8),NU(3, 9) /  4H****   ,4H  *   ,4H**** /,
     4        NU(4, 7),NU(4, 8),NU(4, 9) /  4H*  *   ,4H *    ,4H*  * /,
     5        NU(5, 7),NU(5, 8),NU(5, 9) /  4H****   ,4H*     ,4H**** /
C
      DATA    NU(1,10)                   /  4H****   /,
     1        NU(2,10)                   /  4H*  *   /,
     2        NU(3,10)                   /  4H****   /,
     3        NU(4,10)                   /  4H   *   /,
     4        NU(5,10)                   /  4H****   /
C
      CALL PAGE1
C
C     GRAPH HEADING DATA
C
      IF (IFRAME.LT.0 .OR. IFRAME.GT.99999) IFRAME = 0
      N  = 100000
      DO 10 I = 1,5
      N  = N/10
      M(I) = IFRAME/N
      IFRAME = IFRAME - M(I)*N
   10 M(I) = M(I) + 1
      N1 = M(1)
      N2 = M(2)
      N3 = M(3)
      N4 = M(4)
      N5 = M(5)
      LINES = LINES + 21
      ITLNS = ITLNS + 21
      WRITE  (L,20) (NU(I,N1),NU(I,N2),NU(I,N3),NU(I,N4),NU(I,N5),I=1,5)
   20 FORMAT (1H0,60X,25HF     R     A     M     E, //,
     1       5(59X,A4,2X,A4,2X,A4,2X,A4,2X,A4,/))
      WRITE  (L,30) TITLEC,(XTITLE(I),I=1,28)
   30 FORMAT (1H0,4X,31A4,A3, /1H0,4X,15HX-AXIS TITLE = ,28A4,/1H0)
C
      IF (I123 .EQ. 1) GO TO 70
C
C     DUAL FRAME TITLE FRAME
C
      WRITE  (L,60)
      WRITE  (L,40) TITLEL, TITLER
   40 FORMAT (13X,1HI,57X,3HI I,57X,1HI, /13X,2HI ,14A4,4HI I ,14A4,1HI,
     1       /13X,1HI,57X,3HI I,57X,1HI)
      WRITE  (L,50) (GRAPH(I,6),GRAPH(I,7),GRAPH(I,8),I=2,3)
   50 FORMAT (12X,2(2H I,1P,E14.6,1P,E21.6,1P,E21.6,2H I))
   60 FORMAT (13X,1H+,57(1H-),3H+ +,57(1H-),1H+)
      WRITE  (L,60)
      GO TO 110
C
C     WHOLE FRAME TITLE FRAME
C
   70 WRITE  (L,80)
      WRITE  (L,90) TITLEL
   80 FORMAT (13X,1H+,117(1H-),1H+)
   90 FORMAT (13X,1HI,117X,1HI/13X,2HI ,14A4,60X,1HI, /13X,1HI,117X,1HI)
      WRITE  (L,100) GRAPH(1,6),GRAPH(1,7),GRAPH(1,8)
  100 FORMAT (13X,1HI,1P,E14.6,37X,1P,E14.6,37X,1P,E14.6,2H I)
      WRITE  (L,80)
C
C     DUMP GRAPH
C
  110 F     = XMIN - XINC
      DO 160 I = 1,MAXPLT
      TEMP  = F + FLOAT(I)*XINC
      I1    = (I-1)*30 + 1
      I2    = I1 + 29
      LINES = LINES + 1
      ITLNS = ITLNS + 1
      IF (LINES-NLPP) 120,120,140
  120 CONTINUE
      WRITE  (L,130) TEMP,(Z(J),J=I1,I2)
  130 FORMAT (1X,1P,E11.4,1X,29A4,A3)
      GO TO 160
  140 LINES = 1
      WRITE  (L,150) TEMP,(Z(J),J=I1,I2)
  150 FORMAT (1H1,1P,E11.4,1X,29A4,A3)
  160 CONTINUE
C
      IF (I123 .EQ. 1) GO TO 170
      WRITE (L,60)
      GO TO 180
  170 WRITE (L,80)
C
  180 IF (EXCEED) WRITE (L,190) UIM
      EXCEED = .FALSE.
  190 FORMAT (A29,'. THERE WERE MORE POINTS BELOW THIS POINT WHICH WE',
     1       'ARE NOT PLOTTED HERE',/5X,'DUE TO CORE RESTRICTION')
      RETURN
      END
