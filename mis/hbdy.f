      SUBROUTINE HBDY (ECPT,NECPT,IOPT,RVECT,IVECT)
C
C     THIS SUBROUTINE CALCULATES THE GEOMETRIC PROPERTIES OF THE VARIOUS
C     TYPES OF HBDY ELEMENTS. IOPT IS DESCRIBED BELOW
C
C     THE ECPT INPUT DATA IS
C
C     POSITION    DATA
C        1       EL ID
C        2       FLAG
C        3       SIL-1
C        4       SIL-2
C        5       SIL-3
C        6       SIL-4
C        7       SIL-5
C        8       SIL-6
C        9       SIL-7
C       10       SIL-8
C       11       VECTOR V1
C       12       VECTOR V2
C       13       VECTOR V3
C       14       ECPT14
C       15       MAT ID
C       16       A-FACTOR
C       17       EMISSIVITY
C       18       ABSORBTIVIY
C       19       R1
C       20       R2
C       21       CS-1
C       22       X1
C       23       Y1
C       24       Z1
C       25       CS-2
C       26       X2
C       27       Y2
C       28       Z2
C       29       CS-3
C       30       X3
C       31       Y3
C       32       Z3
C       33       CS-4
C       34       X4
C       35       Y4
C       36       Z4
C       37-52    NOT USED
C       53       AVG. EL. TEMP.
C
C     THE VALUE OF FLAG INDICATES THE TYPE OF ELEMENT
C
C       FLAG     TYPE
C       ****     ****
C        1       POINT
C        2       LINE
C        3       REV
C        4       TRIANGLE
C        5       QUADRILATERAL
C        6       ELLIPTIC CYLINDER
C        7       FTUBE
C
C
C     THE OUTPUT DATA IS PLACED IN  VECT AND IVECT
C         THE FORMATS ARE
C
C     POSITION
C          IOPT=  1             2
C      1        EL ID         EL ID
C      2        AREA          AREA
C      3        EMIS          SIL-1
C      4        ---           SIL-2
C      5        SIL-1         SIL-3
C      6        SIL-2         SIL-4
C      7        SIL-3         AREA-1
C      8        SIL-4         AREA-2
C      9        GFACT-1       AREA-3
C     10        GFACT-2       AREA-4
C     11        GFACT-3       N1X
C     12        GFACT-4       N1Y
C     13                      N1Z
C     14                      N2X  -  FOR FLAG = 6 ONLY
C     15                      N2Y  -
C     16                      N2Z  -
C
C
      INTEGER         NECPT(5),IVECT(5),FLAG
      REAL            ECPT(36),DXYZ(3),RVECT(16),V(3)
      CHARACTER       UFM*23,UWM*25
      COMMON /XMSSG / UFM,UWM
      COMMON /CONDAS/ CONSTS(5)
      EQUIVALENCE     (CONSTS(1),PI), (DXYZ(1),DX), (DXYZ(2),DY),
     1                (DXYZ(3),DZ)
C
C
      DO 10 I = 1,16
      RVECT(I) = 0.0
   10 IVECT(I) = 0
      IVECT(1) = NECPT(1)
      FLAG = NECPT(2)
      IF (FLAG.LE.0 .OR. FLAG.GT. 7) GO TO 210
      IF (FLAG .EQ. 7) ECPT(16) = PI*(ECPT(19) + ECPT(20))
C
      GO TO (20,30,40,50,60,90,30), FLAG
C
C     FLAG = POINT
C
   20 IVECT(3) = NECPT(3)
      RVECT(7) = ECPT(16)
      RVECT(2) = ECPT(16)
      CALL SANORM (*110,ECPT(11))
      NPTS = 1
      GO TO 110
C
C     FLAG = LINE
C
   30 IVECT(3) = NECPT(3)
      IVECT(4) = NECPT(4)
      NPTS = 2
      DX = ECPT(26) - ECPT(22)
      DY = ECPT(27) - ECPT(23)
      DZ = ECPT(28) - ECPT(24)
C
      TEMP = DX**2 + DY**2 + DZ**2
      IF (TEMP .LE. 1.0E-20) GO TO 210
C
C     AREA CALCULATIONS
C
      RVECT(2) = ECPT(16)*SQRT(TEMP)
      RVECT(7) = RVECT(2)*0.5
      RVECT(8) = RVECT(7)
C
C     NORMAL VECTOR CALCULATIONS
C
      TEMP  =(DX*ECPT(11) + DY*ECPT(12) + DZ*ECPT(13))/TEMP
      RVECT(11) = ECPT(11) - TEMP*DX
      RVECT(12) = ECPT(12) - TEMP*DY
      RVECT(13) = ECPT(13) - TEMP*DZ
C
C     NORMALIZE
C
      CALL SANORM (*110,RVECT(11))
      GO TO 110
C
C     TYPE= REV
C
   40 IVECT(3) = NECPT(3)
      IVECT(4) = NECPT(4)
      NPTS = 2
      DX = ECPT(26) - ECPT(22)
      DZ = ECPT(28) - ECPT(24)
      TEMP = SQRT(DX**2 +DZ**2)*PI
      IF (TEMP .LE. 1.0E-20) GO TO 210
      RVECT(7) = (2.0*ECPT(22) + ECPT(26))*TEMP/3.0
      RVECT(8) = (2.0*ECPT(26) + ECPT(22))*TEMP/3.0
      RVECT(2) =  RVECT(7) + RVECT(8)
C
      TEMP = TEMP/PI
      RVECT(11) = DZ/TEMP
      RVECT(13) =-DX/TEMP
      GO TO 110
C
C     FLAG = AREA3
C
   50 IVECT(3) = NECPT(3)
      IVECT(4) = NECPT(4)
      IVECT(5) = NECPT(5)
      NPTS = 3
      DX = ECPT(26) - ECPT(22)
      DY = ECPT(27) - ECPT(23)
      DZ = ECPT(28) - ECPT(24)
      RVECT(7) = ECPT(30) - ECPT(26)
      RVECT(8) = ECPT(31) - ECPT(27)
      RVECT(9) = ECPT(32) - ECPT(28)
C
C     CALC. NORMAL VECTOR
C
      CALL SAXB (DXYZ,RVECT(7),RVECT(11))
C
      CALL SANORM (*210,RVECT(11))
C
      RVECT(2) = TEMP/2.0
      RVECT(7) = TEMP/6.0
      RVECT(8) = RVECT(7)
      RVECT(9) = RVECT(7)
C
      GO TO 110
C
C     FLAG = AREA4
C
   60 DO 70 I = 3,6
   70 IVECT(I) = NECPT(I)
      NPTS = 4
      DO 80 I = 1,3
C
C     CALCULATE  DIFFERENCE VECTORS
C
C        R2 - R1
C
      RVECT(I+6) = ECPT(I+25) - ECPT(I+21)
C
C        R3 - R1
C
      RVECT(I+13) = ECPT(I+29) - ECPT(I+21)
C
C        R4 - R2
C
      V(I) = ECPT(I+33) - ECPT(I+25)
   80 CONTINUE
C
C        (R3 - R1) X (R4 - R2)
C
      CALL SAXB (RVECT(14),V,RVECT(11))
C
C     2*AREA
C
      TEMP  = SQRT(RVECT(11)**2 + RVECT(12)**2 + RVECT(13)**2)
      RVECT(2) = TEMP/2.0
C
C     NORMALIZE
C
      CALL SANORM (*210,RVECT(11))
C
      CALL SAXB (RVECT(7),RVECT(14),DXYZ)
C
C     AREA OF TRIANGLE 123
C
      TEMP = SQRT(DX**2 + DY**2 + DZ**2)/2.0
C
      CALL SAXB (RVECT(7),V,DXYZ)
C
C     AREA OF TRIANGLE 412
C
      DX =  SQRT(DX**2 + DY**2 + DZ**2)/2.0
C
C     AREA FOR POINTS
C
      RVECT( 7) = (RVECT(2)+DX   )/6.0
      RVECT( 8) = (RVECT(2)+TEMP )/6.0
      RVECT( 9) = (RVECT(2)*2.-DX)/6.0
      RVECT(10) = (RVECT(2)*2.-TEMP)/6.0
      RVECT(14) = 0.0
      RVECT(15) = 0.0
      RVECT(16) = 0.0
      NPTS = 4
      GO TO 110
C
C     FLAG = ELCYL
C
   90 IVECT(3) = NECPT(3)
      IVECT(4) = NECPT(4)
      NPTS = 2
      DX = ECPT(26) - ECPT(22)
      DY = ECPT(27) - ECPT(23)
      DZ = ECPT(28) - ECPT(24)
      TEMP = SQRT(DX**2 + DY**2 + DZ**2)
      RVECT(2) = TEMP*ECPT(16)
      IF (IOPT .EQ. 3) RVECT(2) = TEMP
      IF (TEMP .LE. 0) GO TO 210
      CALL SAXB (ECPT(11),DXYZ,RVECT(14))
      CALL SAXB (DXYZ,RVECT(14),RVECT(11))
C
      CALL SANORM (*210,RVECT(11))
      CALL SANORM (*210,RVECT(14))
      DO 100 I = 1,3
      RVECT(I+10) = RVECT(I+10)*ECPT(20)
  100 RVECT(I+13) = RVECT(I+13)*ECPT(19)
      RVECT(7) = RVECT(2)/2.0
      RVECT(8) = RVECT(7)
C
C     IOPT EQUALS 1
C     CALCULATE G FACTORS. STORE IN NEW LOCATIONS.
C     WORK FROM LAST TO FIRST
C
C     CHECK FOR ZERO AREA
C
  110 AREA = RVECT(2)
      IF (AREA .LT. 1.0E-20) GO TO 210
  120 IF (IOPT .GT.       1) GO TO 170
      DO 130 I = 1,NPTS
      J = NPTS - I + 1
  130 RVECT(J+8) =  RVECT(J+6)/AREA
C
      DO 160 I = 1,4
      J =  5-I
      IF (J -NPTS) 150,150,140
  140 IVECT(J+4) = 0
      GO TO 160
  150 IVECT(J+4) = IVECT(J+2)
  160 CONTINUE
C
C     STORE EMISSIVITY VALUE
C
      RVECT(3) = ECPT(17)
      RETURN
C
C     IOPT EQUALS 2
C
  170 IF (IOPT .EQ. 2) RETURN
      DO 180 I = 1,NPTS
      RVECT(I+6) = RVECT(I+6)*ECPT(18)
  180 CONTINUE
      RETURN
C
  210 WRITE  (6,220) UWM,NECPT(1)
  220 FORMAT (A25,' 2154, ZERO AREA OR ILLEGAL CONNECTION FOR HBDY ',
     1       'ELEMENT NUMBER',I9)
      AREA = 1.0
      GO TO 120
      END
