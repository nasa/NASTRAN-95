      SUBROUTINE GPWG1B (MO,OGPWG,WTMASS,IPOINT)
C
C     DOUBLE PRECISION VERSION, BY G.CHAN/UNISYS  8/86
C
C     THIS ROUTINE WRITES OGPWG--
C         HEADER
C         MO =  36 D.P.WORDS
C         S  =  9  D.P.WORDS
C         MX,XX,YX,ZX,MY,XY,YY,ZY,MZ,XZ,YZ,ZZ  = 12 D.P.WORDS
C         I  =  9  D.P.WORDS
C         I1P, I2P, I3P = 3 D.P.WORDS
C         Q  =  9  D.P.WORDS
C               78 D.P.WORDS (156 S.P.WORDS) TOTAL
C
      DOUBLE PRECISION  S(3,3),MT(3,3),MTR(3,3),MR(3,3),TEMP(3,3),
     1                  DZ(36),DELTA,EPSI
      INTEGER           SYSBUF,MO,OGPWG,NAME(2),Z(150)
      EQUIVALENCE       (DZ(1),Z(1),IZ(1))
C
      COMMON /ZZZZZZ/ IZ(1)
      COMMON /UNPAKX/ IT1,II,JJ,INCR
      COMMON /SYSTEM/ SYSBUF
      COMMON /OUTPUT/ HEAD(1)
C
      DATA    NAME  / 4HGPWG,4H1B   /
C
C     ASSIGN BUFFER
C     OPEN OGPWG, PUT ON OFP HEADER
C
      IBUF = KORSZ(Z)- SYSBUF+1
      CALL GOPEN (MO,Z(IBUF),0)
C
C     UNPACK MO  + MOVE TO PARTITIONS
C
      IT1  = 2
      INCR = 1
      JJ   = 6
      II   = 1
      K    = 1
      DO 30 I=1,6
      CALL UNPACK (*10,MO,DZ(K))
      GO TO 30
   10 DO 20 L=1,6
      M = L+K-1
      DZ(M) =0.0D0
   20 CONTINUE
   30 K = K+6
      CALL CLOSE (MO,1)
      DELTA=1.D0/WTMASS
      DO 40 I=1,36
      DZ(I) = DZ(I)*DELTA
   40 CONTINUE
C
C     OPEN OGPWG FOR OUTPUT
C
      CALL GOPEN (OGPWG,Z(IBUF),1)
      DO 42 I = 104,150
   42 Z(I) = 0
      Z(101) = 1
      Z(102) = 13
      Z(103) = IPOINT
      Z(110) = 78*2
      CALL WRITE (OGPWG,Z(101),50,0)
      CALL WRITE (OGPWG,HEAD,96,1)
C
C     PUT MO  ON OGPWG
C
      CALL WRITE (OGPWG,Z(1),72,0)
C
C     PARTITION MO INTO MT, MTR, AND MR
C     AND CREATE DIAGONAL S MATRIX
C
      MT(1,1) = DZ(1)
      MT(1,2) = DZ(2)
      MT(1,3) = DZ(3)
      MT(2,1) = DZ(7)
      MT(2,2) = DZ(8)
      MT(2,3) = DZ(9)
      MT(3,1) = DZ(13)
      MT(3,2) = DZ(14)
      MT(3,3) = DZ(15)
      MTR(1,1)= DZ(4)
      MTR(2,1)= DZ(5)
      MTR(3,1)= DZ(6)
      MTR(1,2)= DZ(10)
      MTR(2,2)= DZ(11)
      MTR(3,2)= DZ(12)
      MTR(1,3)= DZ(16)
      MTR(2,3)= DZ(17)
      MTR(3,3)= DZ(18)
      MR(1,1) = DZ(22)
      MR(1,2) = DZ(23)
      MR(1,3) = DZ(24)
      MR(2,1) = DZ(28)
      MR(2,2) = DZ(29)
      MR(2,3) = DZ(30)
      MR(3,1) = DZ(34)
      MR(3,2) = DZ(35)
      MR(3,3) = DZ(36)
      S(1,1)  = 1.0D0
      S(1,2)  = 0.0D0
      S(1,3)  = 0.0D0
      S(2,1)  = 0.0D0
      S(2,2)  = 1.0D0
      S(2,3)  = 0.0D0
      S(3,1)  = 0.0D0
      S(3,2)  = 0.0D0
      S(3,3)  = 1.0D0
C
C     COMPUTE  DETERMINATE OF  MT
C
      DELTA = DSQRT(MT(1,1)**2 + MT(2,2)**2 + MT(3,3)**2)
      EPSI  = DSQRT(MT(2,1)**2 + MT(3,1)**2 + MT(3,2)**2)
      IF (EPSI  .EQ. 0.0D0) GO TO 60
      EPSI = EPSI/DELTA
      IF (DELTA .EQ. 0.0D0) GO TO 45
      IF (EPSI .LT. 1.0D-6) GO TO 60
C
C     ROTATE COORDINATES
C
   45 R = EPSI
      CALL MESAGE (42,R,NAME)
      DO 50 I=1,3
      DO 50 J=1,3
      TEMP(I,J)= MT(I,J)
   50 CONTINUE
C
C     COMPUTE EIGENVECTORS OF  MT  BY JACOBY  METHOD
C
      CALL GPWG1C (TEMP,S,DZ(1),IFLAG)
      IF (IFLAG .GT. 0) CALL MESAGE(-7,0,NAME)
C
C     ORDER EIGENVECTORS  SUCH THAT
C
C     TRANSFORM  MT
C
      CALL GMMATD (MT,3,3,0,S,3,3,0,TEMP)
      CALL GMMATD (S,3,3,1,TEMP,3,3,0,MT)
C
C     TRANSFORM  MTR
C
      CALL GMMATD (MTR,3,3,0,S,3,3,0,TEMP)
      CALL GMMATD (S,3,3,1,TEMP,3,3,0,MTR)
C
C     TRANSFORM  MR
C
      CALL GMMATD (MR,3,3,0,S,3,3,0,TEMP)
      CALL GMMATD (S,3,3,1,TEMP,3,3,0,MR)
C
C     OUTPUT S
C
   60 CALL WRITE (OGPWG,S,18,0)
C
C     COMPUTE   MX,XX,YX,ZX
C
      DZ(1) = MT(1,1)
      DZ(2) = 0.0D0
      DZ(3) = 0.0D0
      DZ(4) = 0.0D0
      IF (DZ(1) .EQ. 0.0D0) GO TO 70
      DZ(2) = MTR(1,1)/DZ(1)
      DZ(3) =-MTR(3,1)/DZ(1)
      DZ(4) = MTR(2,1)/DZ(1)
   70 CALL WRITE (OGPWG,DZ(1),8,0)
      DZ(5) = MT(2,2)
      DZ(6) = 0.0D0
      DZ(7) = 0.0D0
      DZ(8) = 0.0D0
      IF (DZ(5) .EQ.0. 0D0) GO TO 80
      DZ(6) = MTR(3,2)/DZ(5)
      DZ(7) = MTR(2,2)/DZ(5)
      DZ(8) =-MTR(1,2)/DZ(5)
   80 CALL WRITE (OGPWG,DZ(5),8,0)
      DZ( 9) = MT(3,3)
      DZ(10) = 0.0D0
      DZ(11) = 0.0D0
      DZ(12) = 0.0D0
      IF (DZ(9) .EQ. 0.0D0) GO TO 90
      DZ(10) =-MTR(2,3)/DZ(9)
      DZ(11) = MTR(1,3)/DZ(9)
      DZ(12) = MTR(3,3)/DZ(9)
   90 CALL WRITE (OGPWG,DZ(9),8,0)
C
C     COMPUTE INERTIAS
C
      TEMP(1,1) = MR(1,1) - DZ(5)*DZ(8)*DZ(8) - DZ(9)*DZ(11)*DZ(11)
      TEMP(2,1) =-MR(1,2) - DZ(9)*DZ(10)*DZ(11)
      TEMP(1,2) = TEMP(2,1)
      TEMP(1,3) =-MR(1,3) - DZ(5)*DZ(6)*DZ(8)
      TEMP(3,1) = TEMP(1,3)
      TEMP(2,2) = MR(2,2) - DZ(9)*DZ(10)*DZ(10) - DZ(1)*DZ(4)*DZ(4)
      TEMP(2,3) =-MR(2,3) - DZ(1)*DZ(3)*DZ(4)
      TEMP(3,2) = TEMP(2,3)
      TEMP(3,3) = MR(3,3) - DZ(1)*DZ(3)*DZ(3) - DZ(5)*DZ(6)*DZ(6)
      CALL WRITE (OGPWG,TEMP,18,0)
      CALL GPWG1C (TEMP,S,DZ(1),IFLAG)
      IF (IFLAG .GT. 0) CALL MESAGE(-7,0,NAME)
C
C     PUT OUT  PRINCIPLE INERTIA-S
C
      CALL WRITE (OGPWG,DZ(1),6,0)
C
C     PUT  OUT  Q
C
      CALL WRITE (OGPWG,S,18,0)
      CALL CLSTAB (OGPWG,1)
      RETURN
      END
