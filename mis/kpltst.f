      SUBROUTINE KPLTST (G1,G2,G3,G4)
C
C     THIS ROUTINE WILL VERIFY THAT THE 4 GRID POINTS IN 3 SPACE LIE IN
C     AN APPROXIMATE PLANE. IF NOT THE NOGO FLAG IS SET TRUE AND A
C     MESSAGE IS WRITEN.
C
      LOGICAL         NOGO
      INTEGER         OUT
      REAL            G1(3),G2(3),G3(3),G4(3)
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /SYSTEM/ SYSBUF,OUT,NOGO
      COMMON /SMA1ET/ ID
      COMMON /SMA1DP/ R13(3),R24(3),RXR(3),R(3)
C
      R13(1) = G3(1) - G1(1)
      R13(2) = G3(2) - G1(2)
      R13(3) = G3(3) - G1(3)
      R24(1) = G4(1) - G2(1)
      R24(2) = G4(2) - G2(2)
      R24(3) = G4(3) - G2(3)
      CALL SAXB (R13,R24,RXR)
C
C     NORMALIZE
C
      DL     = SQRT(RXR(1)**2 + RXR(2)**2 + RXR(3)**2)
      IF (DL) 20,20,10
   10 RXR(1) = RXR(1)/DL
      RXR(2) = RXR(2)/DL
      RXR(3) = RXR(3)/DL
      R1L    = SQRT(R13(1)**2 + R13(2)**2 + R13(3)**2)
      R2L    = SQRT(R24(1)**2 + R24(2)**2 + R24(3)**2)
      DL     = AMIN1(R1L,R2L)
      R(1)   = G2(1) - G1(1)
      R(2)   = G2(2) - G1(2)
      R(3)   = G2(3) - G1(3)
      DH     = SADOTB(R,RXR)
      IF (DL) 20,20,15
   15 IF (ABS(DH/DL) .LE. 0.10) RETURN
C
C     NOT PLANER
C
   20 CALL PAGE2 (-2)
      WRITE  (OUT,30) UWM,ID
   30 FORMAT (A25,' 4000, ONE SIDE OF ELEMENT',I10,
     1       ' CONNECTING FOUR POINTS IS NOT APPROXIMATELY PLANER.')
      RETURN
      END
