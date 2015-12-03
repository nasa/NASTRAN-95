      SUBROUTINE MBGEOD
C
C     SUBROUTINE TO COMPUTE GEOMETRY AND INDEXES OF REGIONS
C
      LOGICAL        CNTRL2,CNTRL1,CRANK1,CRANK2,ASYM
      COMMON /MBOXC/ NJJ,CRANK1,CRANK2,CNTRL1,CNTRL2,NBOX,NPTS0,NPTS1,
     1               NPTS2,ASYM,GC,CR,MACH,BETA,EK,EKBAR,EKM,BOXL,BOXW,
     2               BOXA,NCB,NSB,NSBD,NTOTE,KC,KC1,KC2,KCT,KC1T,KC2T
      COMMON /MBOXA/ X(12),Y(12),TANG(10),ANG(10),COTANG(10)
C
C     MAIN GEOMETRY
C
      BIG = -1.0E35
      DO 50 I = 1,10
      TANG(I) = 0.0
      ANG(I)  = 0.0
   50 CONTINUE
      Y(4) = Y(1)
      Y(6) = Y(3)
C
      IF (CRANK1) GO TO 400
      X(2) = X(3)
      Y(2) = Y(3)
      TANG(2) = 0.0
C
 400  IF (CRANK2) GO TO 500
      X(5) = X(6)
      Y(5) = Y(6)
      TANG(5) = 0.0
C
 500  TANG(1) = (X(2)-X(1))/(Y(2)-Y(1))
      ANG(1)  = 57.2958*ATAN(TANG(1))
      IF (CRANK1) TANG(2) = (X(3)-X(2))/(Y(3)-Y(2))
      ANG(2)  = 57.2958*ATAN(TANG(2))
      TANG(4) = (X(5)-X(4))/(Y(5)-Y(4))
      ANG(4)  = 57.2958*ATAN(TANG(4))
      IF (CRANK2) TANG(5) = (X(6)-X(5))/(Y(6)-Y(5))
      ANG(5)  = 57.2958*ATAN(TANG(5))
C
      AREAW   = 0.5*(X(1)*(Y(1)-Y(2)) + X(2)*(Y(1)-Y(3)) +
     1               X(3)*(Y(2)-Y(3)) + X(4)*(Y(5)-Y(1)) +
     2               X(5)*(Y(3)-Y(1)) + X(6)*(Y(3)-Y(5)))
C
C     CONTROL1 SURFACE GEOMETRY
C
      AREA1 = 0.0
      IF (.NOT.CNTRL1) GO TO 1620
      TANG(7) = (X(9)-X(8))/(Y(9)-Y(8))
      ANG(7)  = 57.2958*ATAN(TANG(7))
C
      IF (ABS(Y(7)-Y(8)) .GT. 0.01) GO TO 1000
      Y(7) = Y(8)
      TM   = BIG
      IF (Y(7) .GT. Y(5)) GO TO 900
 800  X(7) = TANG(4)*(Y(7)-Y(4)) + X(4)
      GO TO 1100
 900  X(7) = TANG(5)*(Y(7)-Y(5)) + X(5)
      GO TO 1100
C
 1000 TM   = (X(7)-X(8))/(Y(7)-Y(8))
      IF (Y(5).EQ.Y(7) .AND. X(5).EQ.X(7)) GO TO 1100
      Y(7) = (TM*Y(8)-TANG(4)*Y(4)+X(4)-X(8))/(TM-TANG(4))
      IF (Y(7) .LE. Y(5)) GO TO 800
      Y(7) = (TM*Y(8)-TANG(5)*Y(5)+X(5)-X(8))/(TM-TANG(5))
      GO TO 900
 1100 TANG(6) = TM
C
      IF (ABS(Y(11)-Y(9)) .GT. 0.01) GO TO 1400
      Y(11) =  Y(9)
      TM    =  BIG
      IF (Y(11) .GT. Y(5)) GO TO 1300
 1200 X(11) = TANG(4)*(Y(11)-Y(4)) + X(4)
      GO TO 1500
 1300 X(11) = TANG(5)*(Y(11)-Y(5)) + X(5)
      GO TO 1500
C
 1400 TM    = (X(11)-X(9))/(Y(11)-Y(9))
      IF (Y(5).EQ.Y(11) .AND. X(5).EQ.X(11)) GO TO 1500
      Y(11) = (TM*Y(9)-TANG(4)*Y(4)+X(4)-X(9))/(TM-TANG(4))
      IF (Y(11) .LE. Y(5)) GO TO 1200
      Y(11) = (TM*Y(9)-TANG(5)*Y(5)+X(5)-X(9))/(TM-TANG(5))
      GO TO 1300
 1500 TANG(8) = TM
C
      IF (Y(7).LE.Y(5) .AND. Y(11).GE.Y(5)) GO TO 1600
      AREA1 = 0.5*((X(8)-X(11))*(Y(7)-Y(9)) +
     1             (X(9)-X(7))*(Y(8)-Y(11)))
      GO TO 1620
C
 1600 AREA1 = 0.5*(X(5)*(Y(11)-Y(7)) + X(8)*(Y(7)-Y(9)) +
     1             X(9)*(Y(8)-Y(11)) + X(7)*(Y(5)-Y(8)) +
     2             X(11)*(Y(9)-Y(5)))
C
C     CONTROL2 SURFACE GEOMETRY
C
 1620 AREA2 = 0.0
      IF (.NOT.CNTRL2) GO TO 1700
      TANG(10) = (X(10)-X(9))/(Y(10)-Y(9))
      ANG(10)  = 57.2958*ATAN(TANG(10))
      IF (ABS(Y(12)-Y(10)) .GT. 0.01) GO TO 1660
      Y(12) = Y(10)
      TM    = BIG
      IF (Y(12) .GT. Y(5)) GO TO 1650
 1640 X(12) = TANG(4)*(Y(12)-Y(4)) + X(4)
      GO TO 1670
 1650 X(12) = TANG(5)*(Y(12)-Y(5)) + X(5)
      GO TO 1670
 1660 TM    = (X(12)-X(10))/(Y(12)-Y(10))
      IF (Y(5).EQ.Y(12) .AND. X(5).EQ.X(12) ) GO TO 1670
      Y(12) = (TM*Y(10)-TANG(4)*Y(4)+X(4)-X(10))/(TM-TANG(4))
      IF (Y(12) .LE. Y(5)) GO TO 1640
      Y(12) = (TM*Y(10)-TANG(5)*Y(5)+X(5)-X(10))/(TM-TANG(5))
      GO TO 1650
 1670 TANG(9) = TM
C
      IF (Y(11).LE.Y(5) .AND. Y(12).GE.Y(5)) GO TO 1680
      AREA2 = 0.5*((X(9)-X(12))*(Y(11)-Y(10))
     1           +(X(10)-X(11))*(Y(9)-Y(12)))
      GO TO 1700
C
 1680 AREA2 = 0.5*(X(5)*(Y(12)-Y(11))+X(9)*(Y(11)
     1      - Y(10))+X(10)*(Y(9)-Y(12))+X(11)*(Y(5)
     2      - Y(9))+X(12)*(Y(10)-Y(5)))
C
C     PRINT GEOMETRY DATA
C
 1700 CR    = X(4) - X(1)
      CALL MBPRIT (AREAW,AREA1,AREA2)
      GC    = 2.0*CR**2
      XCENT = (X(3)+X(4)+X(6))/4.0
      YCENT = Y(3)*(0.333 + 0.167*(X(6)-X(3))/X(4))
C
      DO 2100 I = 1,10
      IF (TANG(I) .NE. 0) GO TO 1900
      COTANG(I) = BIG
      GO TO 2100
 1900 IF (TANG(I) .NE. BIG) GO TO 2000
      COTANG(I) = 0.
      GO TO 2100
 2000 COTANG(I) = 1./TANG(I)
 2100 CONTINUE
      RETURN
      END
