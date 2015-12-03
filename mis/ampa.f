      SUBROUTINE AMPA (AERO,QJH,QHH,AJJL,QHHLO,QJHLO,INDEX,IMAX,IANY)
C
C     THE PURPOSE OF THIS ROUTINE IS TO
C         1. INITIALIZE QHJ AND QHH
C         2. COPY USEFUL DATA FROM QJH AND QHH TO QHHLO AND  QJHLO
C         3. SET UP INDEX, IMAX,IANY, AND AMPCOM
C
C     OPEN CORE IS LAID OUT AS FOLLOWS
C
C     CONTENTS                POINTER                  LENGTH
C     --------                -------                  ------
C     AJJL HEADER
C     NCOL
C     NSUB
C     M-K PAIRS               IAJJL                    2*NSUB +2
C      .
C      .
C      .
C     AERO RECORD 2           IAERO                    2* IMAX
C     M- K  PAIRS
C      .
C      .
C      .
C     QHH HEADER RECORD(RST)
C     NOH (OLD)
C     M- K PAIRS              IQHH                     2*NQHH
C       .
C       .
C       .
C     BUFFER2                 IBUF2
C     BUFFER1                 IBUF1
C
C
C     SPECIAL CODE EXISTS IN CASE AJJK HEADER HAS ONLY 2 WORDS
C
      INTEGER         AERO,QJH,QHH,AJJL,QHHLO,QJHLO,XQHHL,SYSBUF,
     1                NAME(2),MCBAJJ(7),FILE,AJJCOL,QHHCOL
      REAL            Z(1)
      COMMON /UNPAKX/ IT1,II,JJ,INCR
      COMMON /PACKX / IT2,IT3,II1,JJ1,INCR1
      COMMON /SYSTEM/ SYSBUF,NOUT,SKP(52),IPREC
      COMMON /BLANK / NOUE,XQHHL,IGUST
      COMMON /AMPCOM/ NCOL,NSUB,XM,XK,AJJCOL,QHHCOL,NGP,NGPD(2,30),
     1                MCBQHH(7),MCBQJH(7),NOH,IDJH,MCBRJH(7)
      COMMON /ZZZZZZ/ IZ(1)
      EQUIVALENCE     (Z(1),IZ(1))
      DATA    NAME  / 4HAMPA,4H..../
C
C     INITIALIZE
C
      MCBAJJ(1) = AJJL
      CALL RDTRL (MCBAJJ)
      MCBQHH(1) = QHH
      CALL RDTRL (MCBQHH)
      MCBQJH(1) = QJH
      CALL RDTRL (MCBQJH)
      IANY = 1
      IBUF1= KORSZ(IZ) - SYSBUF + 1
      IBUF2= IBUF1 - SYSBUF
C
C     EXTRACT DATA FROM AJJL HEADER
C
      FILE = AJJL
      CALL OPEN (*900,AJJL,IZ(IBUF1),0)
      CALL READ (*910,*920,AJJL,IZ,-2,0,IFLAG)
      CALL READ (*910,*30,AJJL,IZ,IBUF2-1,0,IFLAG)
      GO TO 980
C
C     PROCESS AJJL  DATA
C
   30 CALL CLOSE (AJJL,1)
      NOAJJH= 0
      IAJJL = 4
      IF (IFLAG .EQ. 0) GO TO 50
      NCOL = IZ(1)
      IZX  = 3
      NSUB = MIN0(IZ(IZX),MCBAJJ(2)/NCOL)
      NGP  = IZ(2*NSUB+4)
      K    = 2*NSUB + 5
      IAERO= K - 1
      DO 35 I = 1,NGP
      NGPD(1,I) = IZ(K  )
      NGPD(2,I) = IZ(K+1)
      K = K + 3
   35 CONTINUE
      GO TO 55
C
C     NO AJJ HEADER DATA
C
   50 NOAJJH = 1
      IAERO  = 3
C
C     BRING IN AERO DATA
C
   55 CONTINUE
      CALL GOPEN (AERO,IZ(IBUF1),0)
      FILE = AERO
      CALL FWDREC (*910,AERO)
      NZ = IBUF2 - IAERO
      CALL READ (*910,*60,AERO,IZ(IAERO),NZ,0,IFLAG)
      GO TO 980
C
C     AERO DATA IN CORE
C
   60 CALL CLOSE (AERO,1)
      IMAX = IFLAG/2
      IF (NOAJJH .EQ. 0) GO TO 70
C
C     FIX UP FOR AJJ MISSING HEADER
C
      NCOL = MCBAJJ(2)/IMAX
      NSUB = IMAX
      NGP  = 1
      NGPD(1,1) = 1
      NGPD(2,1) = NCOL
      IAERO = IFLAG + 3
      K = IAERO
      DO 65 I = 1, IFLAG
      IZ(K) = IZ(I+2)
      K = K+1
   65 CONTINUE
C
C     PUT HEADERS FROM OLD QHH IN CORE
C
   70 IF (XQHHL .EQ. 1) GO TO 80
      FILE = QHH
      CALL OPEN (*900,QHH,IZ(IBUF1),0)
      CALL FREAD (QHH,IZ,-2,0)
      IQHH = IAERO + 2*IMAX + 2
      NZ   = NZ - 2*IMAX
      CALL READ (*910,*75,QHH,IZ(IQHH),NZ,0,IFLAG)
      GO TO 980
   75 CALL CLOSE (QHH,1)
      IQHH = IQHH + 2
      NQHH = MIN0((IFLAG-2)/2,MCBQHH(2)/NOH)
C
C     BUILD INDEX FILE
C
   80 CONTINUE
      I = 0
      CALL GOPEN (INDEX,IZ(IBUF1),1)
   90 CONTINUE
      XM =  Z(IAERO+I  )
      XK =  Z(IAERO+I+1)
C
C     SEARCH FOR COLUMN NUMBER IN AJJL
C
      J  =  0
  100 CONTINUE
      XMA = Z(IAJJL+J  )
      XKA = Z(IAJJL+J+1)
      IF (XMA.EQ.XM .AND. XKA.EQ.XK) GO TO 120
      J =  J + 2
      IF (J .GE. 2*NSUB) CALL MESAGE (-7,0,NAME)
      GO TO 100
C
C     FOUND IN AJJL
C
  120 CONTINUE
      AJJCOL = (J/2)*NCOL + 1
C
C     SEARCH FOR COLUMN NUMBER IN QHH
C
      QHHCOL = 0
      IF (XQHHL .EQ. 1) GO TO 140
      J = 0
  130 CONTINUE
      XMA = Z(IQHH+J  )
      XKA = Z(IQHH+J+1)
      IF (XMA.EQ.XM .AND. XKA.EQ.XK) GO TO 150
      J = J + 2
      IF (J .GE. 2*NQHH) GO TO 140
      GO TO 130
C
C     FOUND IN QHH
C
  150 QHHCOL = (J/2)*NOH + 1
C
C     WRITE ON INDEX
C
  140 CALL WRITE (INDEX,XM,4,1)
      IF (QHHCOL .EQ. 0) IANY = 0
      I = I + 2
      IF (I .GE. 2*IMAX) GO TO 200
      GO TO 90
C
C     DONE WITH INDEX
C
  200 CALL CLOSE (INDEX,1)
C
C     COPY OLD  QHH  ONTO QHHLO
C
      IF (XQHHL .EQ. 1) GO TO 300
      IT1  = MCBQHH(5)
      IT2  = IT1
      IT3  = IT1
      INCR = 1
      INCR1= 1
      IF (MCBQHH(1) .LE. 0) GO TO 230
      CALL GOPEN (QHH,IZ(IBUF1),0)
      CALL GOPEN (QHHLO,IZ(IBUF2),1)
      NCLQHH    = MCBQHH(2)
      MCBQHH(2) = 0
      MCBQHH(6) = 0
      MCBQHH(7) = 0
      MCBQHH(1) = QHHLO
      CALL CYCT2B (QHH,QHHLO,NCLQHH,IZ,MCBQHH)
      CALL CLOSE (QHH,1)
      CALL CLOSE (QHHLO,1)
      CALL WRTTRL (MCBQHH)
C
C     COPY OLD QJH ONTO QJHLO
C
  230 CONTINUE
C
C     COPY QJH ONTO QJHLO
C
      IF (MCBQJH(1) .LE. 0) GO TO 250
      CALL GOPEN (QJH,IZ(IBUF1),0)
      CALL GOPEN (QJHLO,IZ(IBUF2),1)
      NCLQJH    = MCBQJH(2)
      MCBQJH(1) = QJHLO
      MCBQJH(2) = 0
      MCBQJH(6) = 0
      MCBQJH(7) = 0
      CALL CYCT2B (QJH,QJHLO,NCLQJH,IZ,MCBQJH)
      CALL CLOSE (QJH,1)
      CALL CLOSE (QJHLO,1)
      CALL WRTTRL (MCBQJH)
  250 CONTINUE
C
C     PUT HEADERS ON NEW OUTPUT FILES
C
  300 CONTINUE
      IF (MCBQHH(1) .LE. 0) GO TO 350
      FILE  = QHH
      CALL OPEN (*900,QHH,IZ(IBUF1),1)
      CALL FNAME (QHH,MCBQHH)
      CALL WRITE (QHH,MCBQHH,2,0)
      CALL WRITE (QHH,NOH,1,0)
      CALL WRITE (QHH,IMAX,1,0)
      CALL WRITE (QHH,IZ(IAERO),2*IMAX,1)
      CALL CLOSE (QHH,3)
      MCBQHH(1) = QHH
      MCBQHH(2) = 0
      MCBQHH(3) = NOH
      MCBQHH(4) = 2
      MCBQHH(5) = 2 + IPREC
      MCBQHH(6) = 0
      MCBQHH(7) = 0
  350 CONTINUE
      IF (MCBQJH(1) .LE. 0) GO TO 360
      FILE = QJH
      CALL OPEN (*900,QJH,IZ(IBUF1),1)
      CALL FNAME (QJH,MCBQJH)
      CALL WRITE (QJH,MCBQJH,2,0)
      CALL WRITE (QJH,NOH,1,0)
      CALL WRITE (QJH,IMAX,1,0)
      CALL WRITE (QJH,IZ(IAERO),2*IMAX,1)
      CALL CLOSE (QJH,3)
      MCBQJH(1) = QJH
      MCBQJH(2) = 0
      MCBQJH(3) = NCOL
      MCBQJH(4) = 2
      MCBQJH(5) = 2 + IPREC
      MCBQJH(6) = 0
      MCBQJH(7) = 0
  360 CONTINUE
      IANY = 0
C
C     PUT HEADER ON QHJL
C
      IF (IGUST .LE. 0) RETURN
      FILE = MCBRJH(1)
      CALL OPEN (*900,FILE,IZ(IBUF1),1)
      CALL FNAME (FILE,MCBRJH(2))
      CALL WRITE (FILE,MCBRJH(2),2,0)
      CALL WRITE (FILE,NOH,1,0)
      CALL WRITE (FILE,IMAX,1,0)
      CALL WRITE (FILE,IZ(IAERO),2*IMAX,1)
      CALL CLOSE (FILE,3)
      CALL MAKMCB (MCBRJH,FILE,NCOL,2,2+IPREC)
      CALL WRTTRL (MCBRJH)
      RETURN
C
C     ERROR MESSAGES
C
  900 IP1 = -1
  901 CALL MESAGE (IP1,FILE,NAME)
  910 IP1 = -2
      GO TO 901
  920 IP1 = -3
      GO TO 901
  980 IP1 = -8
      GO TO 901
      END
