      SUBROUTINE CDTFBS (DX,DY,IOBUF,FILEU,NROW)
C
C     CDTFBS IS A SPECIAL VERSION OF GFBS USED BY COMPLEX DETERMINATE
C     METHOD
C
C     DEFINITION OF INPUT PARAMETERS
C
C     FILEU = MATRIX CONTROL BLOCK FOR THE UPPER TRIANGLE U
C     DX    = THE LOAD VECTOR B
C     DY    = THE SOLUTION VECTOR X
C     IOBUF = THE INPUT BUFFER
C
      INTEGER          FILEU(7)  ,TYPEAR   ,CDP      ,PARM(4)  ,
     1                 IOBUF(1)
      DOUBLE PRECISION DX(1)     ,DY(1)    ,DTEMP
      COMMON /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                 REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                 RDP       ,CSP      ,CDP      ,SQR      ,
     3                 RECT      ,DIAG     ,LOWTRI   ,UPRTRI   ,
     4                 SYM       ,ROW      ,IDENTY
      COMMON /UNPAKX/  ITYPEX    ,IUNPAK   ,JUNPAK   ,INCRX
      DATA    PARM(3), PARM(4)   /4HDETF,4HBS  /
C
      INCRX  = 1
      ITYPEX = CDP
      TYPEAR = CDP
C
C     BEGIN BACKWARD PASS
C
      IOFF = FILEU(7) - 1
      PARM(2) = FILEU(1)
      CALL OPEN (*80,FILEU,IOBUF,RD)
      DO 70 I = 1,NROW
      IUNPAK = 0
      J  = NROW - I + 1
      JJ = J + J
      CALL BCKREC (FILEU)
      CALL UNPACK (*90,FILEU,DY)
      CALL BCKREC (FILEU)
      ISING = 0
      K = (JUNPAK-IUNPAK+1)*2
      JU = JUNPAK + JUNPAK
      GO TO 30
   10 ISING = 1
C
C     DIVIDE BY THE DIAGONAL
C
      DTEMP    = (DX(JJ)*DY(K-1)-DX(JJ-1)*DY(K))/(DY(K)**2+DY(K-1)**2)
      DX(JJ-1) = (DX(JJ-1)*DY(K-1)+DX(JJ)*DY(K))/(DY(K)**2+DY(K-1)**2)
      DX(JJ  ) = DTEMP
   20 K  = K  - 2
      JU = JU - 2
      JUNPAK = JUNPAK - 1
      IF (K .EQ. 0) GO TO 60
      IF (DY(K).EQ.0.D0 .AND. DY(K-1).EQ.0.D0) GO TO 20
   30 IF (JUNPAK-J) 50,10,40
   40 JK = (J-IOFF)*2
      DX(JK-1) = DX(JK-1) - DX(JU-1)*DY(K-1) + DX(JU  )*DY(K)
      DX(JK  ) = DX(JK  ) - DX(JU  )*DY(K-1) - DX(JU-1)*DY(K)
      GO TO 20
   50 CONTINUE
      DX(JU-1) = DX(JU-1) - DX(JJ-1)*DY(K-1) + DX(JJ  )*DY(K)
      DX(JU  ) = DX(JU  ) - DX(JJ  )*DY(K-1) - DX(JJ-1)*DY(K)
      GO TO 20
   60 IF (ISING .EQ. 0) GO TO 90
   70 CONTINUE
      CALL CLOSE (FILEU,REW)
      RETURN
C
   80 PARM(1) = -1
      GO TO 100
   90 PARM(1) = -5
  100 CALL MESAGE (PARM(1),PARM(2),PARM(3))
      RETURN
      END
