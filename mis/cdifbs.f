      SUBROUTINE CDIFBS(DZ,BUF)
C
C     SUBROUTINE TO DO THE FBS PASS TO FIND THE LEFT EIGENVECTOR FOR
C     THE TRANSPOSED MATRIX
C
      INTEGER            UPRTRI    ,EOL      ,NAME(2)
      DOUBLE PRECISION   DTEMP     ,DZ(1)    ,DA
      DIMENSION          BUF(1)
C     COMMON   /DESCRP/  LENGTH    ,MAJOR
      COMMON   /ZNTPKX/  DA(2)     ,II       ,EOL
      COMMON   /NAMES/   RD        ,RDREW    ,WRT      ,WRTREW   ,
     1                   REW       ,NOREW    ,EOFNRW   ,RSP      ,
     2                   RDP       ,CSP      ,CDP
      COMMON   /CDCMPX/  IDUMM(20) ,IOF
      COMMON   /CINVPX/  IDUM(36)  ,SCRFIL(11)
      EQUIVALENCE        (SCRFIL(6),UPRTRI)  ,(SCRFIL(8),LOWTRI) ,
     1                   (IDUM(2),NROW)
      DATA      NAME  /  4HCDIF,4HBS  /
C
      CALL SSWTCH (12,IDIAG)
C
C     BEGIN THE FORWARD PASS USING THE UPPER TRIANGLE
C
      IOFF = IOF - 1
      CALL GOPEN (UPRTRI,BUF,RDREW)
      NROW2 = NROW + NROW
      DO 100 I = 1,NROW
      J = I + I
      CALL INTPK (*100,UPRTRI,0,CDP,0)
   10 CALL ZNTPKI
      IF (II-I) 30,20,40
C
C     DIVIDE BY THE DIAGONAL
C
   20 DTEMP = (DZ(J-1)*DA(1)+DZ(J  )*DA(2))/(DA(1)**2 + DA(2)**2)
      DZ(J) = (DZ(J  )*DA(1)-DZ(J-1)*DA(2))/(DA(1)**2 + DA(2)**2)
      DZ(J-1) = DTEMP
      GO TO 90
C
C     SUBTRACT OFF NORMAL TERM
C
   30 DZ(J-1) = DZ(J-1) - DZ(2*II-1)*DA(1) + DZ(2*II)*DA(2)
      DZ(J  ) = DZ(J  ) - DZ(2*II-1)*DA(2) - DZ(2*II)*DA(1)
      GO TO 90
C
C     SUBTRACT OFF ACTIVE COLUMN TERMS
C
   40 K = (I-IOFF)*2
      DZ(2*II-1) = DZ(2*II-1) - DZ(K-1)*DA(1) + DZ(K  )*DA(2)
      DZ(2*II  ) = DZ(2*II  ) - DZ(K  )*DA(1) - DZ(K-1)*DA(2)
   90 IF (EOL) 100,10,100
  100 CONTINUE
      CALL CLOSE (UPRTRI,REW)
C
C     BEGIN BACKWARD PASS USING THE LOWER TRIANGLE
C
      CALL GOPEN (LOWTRI,BUF,RDREW)
      CALL SKPREC (LOWTRI,NROW)
      DO 200 I = 1,NROW
      CALL BCKREC (LOWTRI)
      INTCHN = 0
      CALL INTPK (*200,LOWTRI,0,CDP,0)
      J = (NROW-I+1)*2
  120 CALL ZNTPKI
      IF (II .NE. NROW-I+1) GO TO 150
      IF (II .LT. J/2) GO TO 1010
C
C     PERFORM THE INTERCHANGE
C
      INTCHN = IFIX(SNGL(DA(1)))*2
      IF (IDIAG .NE. 0) WRITE (6,131) I,INTCHN
  131 FORMAT (5H I = ,I5,10HINTCHNG =  ,I5)
      GO TO 190
  130 IN1   = J + INTCHN
      DTEMP = DZ(J)
      DZ(J) = DZ(IN1)
      DZ(IN1) = DTEMP
      DTEMP   = DZ(J-1)
      DZ(J-1  ) = DZ(IN1-1)
      DZ(IN1-1) = DTEMP
      GO TO 200
  150 CONTINUE
      DZ(J-1) = DZ(J-1) - DZ(2*II-1)*DA(1) + DZ(2*II)*DA(2)
      DZ(J  ) = DZ(J  ) - DZ(2*II-1)*DA(2) - DZ(2*II)*DA(1)
  190 IF (EOL) 195,120,195
  195 IF (INTCHN) 200,200,130
  200 CALL BCKREC (LOWTRI)
      CALL CLOSE (LOWTRI,REW)
      RETURN
C
 1010 CALL MESAGE (-7,LOWTRI,NAME)
      RETURN
      END
