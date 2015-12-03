      SUBROUTINE A8 2 INT (*,A,N,B,INT)
C
      CHARACTER*8     C
      REAL            A(2)
      COMMON /XREADX/ NOUT
C
C     THESE ROUTINES PERFORM IN THE OPPOSITE DIRECTION AS THOSE OF THE
C     INT2A8 GROUP OF ROUTINES
C     THIS ROUTINE IS MACHINE INDEPENDENT
C
C     ENTRY POINTS   A8 2 INT  (BCD-INTEGER VERSION)
C                    K8 2 INT  (CHARACTER-INTEGER VERSION)
C                    A8 2 FP   (BCD-REAL VERSION)
C                    K8 2 FP   (CHARACTER-REAL VERSION)
C
      NT = +1
      GO TO 20
C
      ENTRY K8 2 INT (*,C,N,B,INT)
C     ****************************
C
      NT = +1
      GO TO 30
C
      ENTRY A8 2 FP (*,A,N,B,INT)
C     ***************************
C
      NT = -1
C
 20   IF (N .GT. 8) GO TO 50
      INT = NT
      CALL NA12IF (*80,A,N,B,INT)
      RETURN
C
      ENTRY K8 2 FP (*,C,N,B,INT)
C     ***************************
C
      NT = -1
C
 30   IF (N .GT. 8) GO TO 50
      INT = NT
      CALL NK12IF (*80,C,N,B,INT)
      RETURN
C
 50   WRITE  (NOUT,60) N,NT
 60   FORMAT ('  N.GT.8/A82INT',I5,7X,'NT=',I2)
 80   RETURN 1
      END
