      SUBROUTINE DDUMX
C
C     DELETE ANY OF THE FOLLOW ENTRY POINT IF A SUBROUTINE OF THE SAME
C     NAME ALREADY EXISTS
C
      INTEGER         II(9),KK(9)
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /SYSTEM/ IBUF,NOUT
      DATA    II    / 9*0/,   JJ /4HDDUM/,     KK /
     1        1H1,1H2,1H3,1H4,1H5,1H6,1H7,1H8,1H9 /
C
      GO TO 30
C
C
      ENTRY DDUM9
C     ===========
C
      J = 9
      GO TO 10
C
C
      ENTRY DDUM8
C     ===========
C
      J = 8
      GO TO 10
C
C
      ENTRY DDUM7
C     ===========
C
      J = 7
      GO TO 10
C
C
      ENTRY DDUM6
C     ===========
C
      J = 6
      GO TO 10
C
C
      ENTRY DDUM5
C     ===========
C
      J = 5
      GO TO 10
C
C
      ENTRY DDUM4
C     ===========
C
      J = 4
      GO TO 10
C
C
      ENTRY DDUM3
C     ===========
C
      J = 3
      GO TO 10
C
C
      ENTRY DDUM2
C     ===========
C
      J = 2
      GO TO 10
C
C
      ENTRY DDUM1
C     ===========
C
      J = 1
C     GO TO 10
C
   10 IF (II(J) .NE. 0) GO TO 30
      II(J)  = 1
      WRITE  (NOUT,20) UWM,JJ,KK(J)
   20 FORMAT (A25,' 2182, SUBROUTINE ',2A4,' IS DUMMY.  ONLY ONE OF ',
     1       'THESE MESSAGES WILL APPEAR PER  OVERLAY OF THIS DECK.')
   30 RETURN
      END
