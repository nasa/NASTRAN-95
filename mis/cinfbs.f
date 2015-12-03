      SUBROUTINE CINFBS (DX,DY,IOBUF)
C
C     CINVFB DOES THE FORWARD AND BACKWARD PASS FOR COMPLEX INVERSE POWE
C
      INTEGER            NAME(2)   ,TYPEAR   ,CDP      ,IOBUF(1) ,EOL
      DOUBLE PRECISION   DX(1)     ,DY(1)    ,DA       ,DTEMP
C     COMMON   /DESCRP/  LENGTH    ,MAJOR
      COMMON   /NAMES /  RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                   RDP       ,CSP      ,CDP
      COMMON   /ZNTPKX/  DA(2)     ,II       ,EOL
      COMMON   /CINFBX/  IFILL(7)  ,IFILU(7)
      EQUIVALENCE        (IFILL(3),NROW)
      DATA      NAME  /  4HCINF, 4HBS   /
C
C     TRANSFER THE LOAD VECTOR TO THE SOLUTION VECTOR
C
      TYPEAR = CDP
      NROW2  = NROW + NROW
      DO 10 I = 1,NROW2
   10 DY(I) = DX(I)
C
C     BEGIN FORWARD PASS
C
C     CALL GOPEN (IFILL(1),IOBUF,RDREW)
      J = 1
  100 CALL INTPK (*200,IFILL(1),0,TYPEAR,0)
  110 IF (EOL) 3010,120,3010
  120 CALL ZNTPKI
      IF (J-II) 184,130,110
C
C     PERFORM THE REQUIRED ROW INTERCHANGE
C
  130 IN1 = (J+IFIX(SNGL(DA(1))))*2 - 1
      DTEMP     = DY(2*J-1)
      DY(2*J-1) = DY(IN1)
      DY(IN1)   = DTEMP
      DTEMP     = DY(2*J)
      DY(2*J)   = DY(IN1+1)
      DY(IN1+1) = DTEMP
  160 IF (EOL) 200,170,200
  170 CALL ZNTPKI
  184 DY(2*II-1) = DY(2*II-1) - DY(2*J-1)*DA(1) + DY(2*J)*DA(2)
      DY(2*II  ) = DY(2*II  ) - DY(2*J-1)*DA(2) - DY(2*J)*DA(1)
      GO TO 160
  200 J = J + 1
      IF (J .LT. NROW) GO TO 100
      CALL REWIND (IFILL(1))
C
C     BEGIN BACKWARD PASS
C
      IOFF = IFILU(7) - 1
      J = NROW
  210 CALL INTPK (*3020,IFILU(1),0,TYPEAR,0)
      IF (EOL) 3020,230,3020
  230 CALL ZNTPKI
      I = NROW - II + 1
      IF (I .NE. J) GO TO 275
C
C     DIVIDE BY THE DIAGONAL
C
      DTEMP     = (DY(2*I-1)*DA(1)+DY(2*I)*DA(2))/(DA(1)**2+DA(2)**2)
      DY(2*I  ) = (DY(2*I)*DA(1)-DY(2*I-1)*DA(2))/(DA(1)**2+DA(2)**2)
      DY(2*I-1) = DTEMP
C
C     SUBTRACT OFF REMAINING TERMS
C
  255 IF (I .GT. J) GO TO 230
      IF (EOL) 300,270,300
  270 CALL ZNTPKI
      I = NROW - II + 1
  275 IN1 = I
      IN2 = J
      IF (I .LT. J) GO TO 279
      K   = IN1
      IN1 = IN2 - IOFF
      IN2 = K
  279 IN1 = IN1 + IN1 - 1
      IN2 = IN2 + IN2 - 1
      DY(IN1  ) = DY(IN1  ) - DY(IN2)*DA(1) + DY(IN2+1)*DA(2)
      DY(IN1+1) = DY(IN1+1) - DY(IN2)*DA(2) - DY(IN2+1)*DA(1)
      GO TO 255
  300 J = J - 1
      IF (J .GT. 0) GO TO 210
      CALL REWIND (IFILU(1))
      RETURN
C
 3010 IFILE = IFILL(1)
      GO TO 3040
 3020 IFILE = IFILU(1)
 3040 CALL MESAGE (-5,IFILE,NAME)
      RETURN
      END
