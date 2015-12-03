      SUBROUTINE SDUMX2
C
C     DELETE ANY OF THE FOLLOW ENTRY POINT IF A SUBROUTINE OF THE SAME
C     NAME ALREADY EXISTS
C
      INTEGER         II(9),KK(9)
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /SYSTEM/ IBUF,NOUT
      DATA    II    / 9*0/,   JJ /4HSDUM/,     KK /
     1        2H12,2H22,2H32,2H42,2H52,2H62,2H72,2H82,2H92 /
C
      GO TO 30
C
C
      ENTRY SDUM92
C     ============
C
      J = 9
      GO TO 10
C
C
      ENTRY SDUM82
C     ============
C
      J = 8
      GO TO 10
C
C
      ENTRY SDUM72
C     ============
C
      J = 7
      GO TO 10
C
C
      ENTRY SDUM62
C     ============
C
      J = 6
      GO TO 10
C
C
      ENTRY SDUM52
C     ============
C
      J = 5
      GO TO 10
C
C
      ENTRY SDUM42
C     ============
C
      J = 4
      GO TO 10
C
C
      ENTRY SDUM32
C     ============
C
      J = 3
      GO TO 10
C
C
      ENTRY SDUM22
C     ============
C
      J = 2
      GO TO 10
C
C
      ENTRY SDUM12
C     ============
C
      J = 1
C     GO TO 10
C
   10 IF (II(J) .NE. 0) GO TO 30
      II(J)  = 1
      WRITE  (NOUT,20) UWM,JJ,KK(J)
   20 FORMAT (A25,' 2182, SUBROUTINE ',2A4,' IS DUMMY.  ONLY ONE OF ',
     1       'THESE MESSAGES WILL APPEAR PER OVERLAY OF THIS DECK.')
   30 RETURN
      END
