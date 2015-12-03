      SUBROUTINE BASGLB (VIN1,VOUT1,PONT,ICSTM)
C
C     THIS ROUTINE CONTAINS FOUR ENTRY POINTS
C
C     1- BASGLB TRANSFORMS A VECTOR FROM BASIC TO GLOBAL
C     2- GLBBAS TRANSFORMS A VECTOR FROM GLOBAL TO BASIC
C     3- FDCSTM FINDS THE LOGICAL RECORD ON THE CSTM FOR A PARTICULAR ID
C     4- GBTRAN FINDS A PARTICULAR GLOBAL TO BASIC TRANSFORMATION AND
C        RETURNS IT AS A 3 X 3 STORED BY ROWS.
C
C
      LOGICAL         TONLY
      INTEGER         CSTM,TYSYS,CHECK
      REAL            T(9)
      DIMENSION       VIN(3),VIN1(3),VOUT1(3),TI(3,3),TL(3,3),
     1                PONT(3),PONT1(3),TZ(3,3),IPARM(2)
      COMMON /XCSTM / TZ
      COMMON /LOADX / LC(4),CSTM,LC1(10),IDUM(3),ICM
      COMMON /TRANX / NSYS,TYSYS,RO(3),TO(3,3)
      COMMON /SYSTEM/ IBUF,NOUT
      DATA    IPARM/ 4HBASG,2HLB /
C
C     NSYS  IS SYSTEM NUMBER
C     TYSYS IS SYSTEM TYPE
C     RO IS LOCATION OF ORIGIN
C     TO IS ROTATION MATRIX
C
      TONLY = .FALSE.
      CHECK = 123456789
      ASSIGN 90 TO IEXIT
      GO TO 10
C
C
      ENTRY GBTRAN (ICSTM,PONT,T)
C     ===========================
C
      IF (ICSTM .EQ. 0) GO TO 300
      IF (TYSYS.GE.2 .AND. CHECK.NE.123456789) WRITE (NOUT,5)
    5 FORMAT ('0*** SYSTEM POTENTIAL ERROR, GBTRAN WAS CALLED WITHOUT',
     1        ' FIRST CALLING BASGLB')
      CHECK = 123456789
      TONLY = .TRUE.
      GO TO 235
C
C
      ENTRY FDCSTM (ICSTM)
C     ====================
C
      TONLY = .FALSE.
      ASSIGN 50 TO IEXIT
C
C     FDCSTM WILL FIND REQUESTED SYSTEM (ICSTM)
C
   10 CONTINUE
      IF (ICSTM .EQ. 0) GO TO 81
      IF (ICM   .NE. 0) GO TO 80
      IF (ICSTM-NSYS) 20,40,20
   20 CALL READ (*60,*80,CSTM,NSYS,14,0,FLAG)
      IF (ICSTM-NSYS) 20,30,20
   30 CALL BCKREC (CSTM)
   40 GO TO IEXIT, (90,240,50)
   50 RETURN
C
   60 N1 = -2
      IPARM1 = CSTM
   70 CALL MESAGE (N1,IPARM1,IPARM)
C
C     UNABLE TO FIND REQUESTED COORDINATE SYSTEM
C
   80 N1      =-30
      IPARM1  = 25
      IPARM(1)= ICSTM
      GO TO 70
C
C     REQUEST FOR BASIC COORDINATE SYSTEM
C
   81 CONTINUE
      TYSYS = 1
      NSYS  = 0
      RO(1) = 0.0
      RO(2) = 0.0
      RO(3) = 0.0
      DO 82 I = 1,3
      DO 82 J = 1,3
      TO(J,I) = 0.0
   82 CONTINUE
      TO(1,1) = 1.0
      TO(2,2) = 1.0
      TO(3,3) = 1.0
      GO TO 40
C
C     CONVERTS BASIC TO GLOBAL
C
   90 IOTH = 0
C
C     RECTANGULAR
C
  100 DO 120 I = 1,3
      DO 110 J = 1,3
      TZ(I,J) = TO(J,I)
  110 TI(I,J) = TO(J,I)
  120 VIN(I)  = VIN1(I)
      IF (TYSYS-2) 130,140,140
  130 CALL MPYL (TI(1,1),VIN(1),3,3,1,VOUT1(1))
      GO TO 50
C
C     CYLINDRICAL
C
  140 DO 150 I = 1,3
  150 PONT1(I) = PONT(I) - RO(I)
      CALL MPYL (TI(1,1),PONT1(1),3,3,1,VIN(1))
      DO 160 I = 1,3
      DO 160 J = 1,3
  160 TL(I,J) = 0.0
      R = SQRT(VIN(1)*VIN(1) + VIN(2)*VIN(2))
      IF (R  .EQ.  0.0) GO TO 210
      IF (TYSYS .GT. 2) GO TO 230
      TL(3,3) = 1.0
      TL(1,1) = VIN(1)/R
      TL(2,2) = TL(1,1)
      TL(2,1) = VIN(2)/R
      TL(1,2) =-TL(2,1)
  170 CALL MPYL (TL(1,1),TI(1,1),3,3,3,TZ(1,1))
  180 IF (TONLY) GO TO 201
      IF ( IOTH) 270,190,270
  190 DO 200 I = 1,3
  200 VIN(I) = VIN1(I)
      CALL MPYL (TZ(1,1),VIN(1),3,3,1,VOUT1(1))
      GO TO 50
C
C     RETURN THE TRANSFORMATION ONLY
C
  201 T(1) = TZ(1,1)
      T(2) = TZ(1,2)
      T(3) = TZ(1,3)
      T(4) = TZ(2,1)
      T(5) = TZ(2,2)
      T(6) = TZ(2,3)
      T(7) = TZ(3,1)
      T(8) = TZ(3,2)
      T(9) = TZ(3,3)
      GO TO 50
C
C     ORIENTATION ARBITARY   TL = I   I.E. TZ = TI
C
  210 DO 220 I = 1,3
      DO 220 J = 1,3
      TZ(I,J) = TI(I,J)
  220 CONTINUE
      GO TO 180
C
C     SPHERICAL
C
  230 XL = SQRT(VIN(1)*VIN(1) + VIN(2)*VIN(2) + VIN(3)*VIN(3))
      XR = VIN(1)/R
      YR = VIN(2)/R
      ZL = VIN(3)/XL
C
C     BUILD TL TRANSPOSE
C
      TL(1,1) = VIN(1)/XL
      TL(1,2) = XR*ZL
      TL(1,3) =-YR
      TL(2,1) = VIN(2)/XL
      TL(2,2) = YR*ZL
      TL(2,3) = XR
      TL(3,1) = ZL
      TL(3,2) =-R/XL
      GO TO 170
C
C
      ENTRY GLBBAS (VIN1,VOUT1,PONT,ICSTM)
C     ====================================
C
      TONLY = .FALSE.
  235 ASSIGN 240 TO IEXIT
      IOTH = 1
      GO TO 10
C
C     CONVERTS FROM GLOBAL TO BASIC
C
  240 IF (TYSYS-2) 250,100,100
  250 IF ( TONLY ) GO TO 261
      DO 260 I = 1,3
  260 VIN(I) = VIN1(I)
      CALL MPYL (TO(1,1),VIN(1),3,3,1,VOUT1(1))
      GO TO 50
C
C     RETURN THE TRANSFORMATION ONLY.
C
  261 T(1) = TO(1,1)
      T(2) = TO(2,1)
      T(3) = TO(3,1)
      T(4) = TO(1,2)
      T(5) = TO(2,2)
      T(6) = TO(3,2)
      T(7) = TO(1,3)
      T(8) = TO(2,3)
      T(9) = TO(3,3)
      GO TO 50
C
C     COMPUTE TL TRANSPOSE
C
C     TRANSPOSE ROTATION PRODUCT
C
  270 DO 280 I = 1,3
      VIN(I) = VIN1(I)
      DO 280 J = 1,3
  280 TI(I,J) = TZ(J,I)
      CALL MPYL (TI(1,1),VIN(1),3,3,1,VOUT1(1))
      GO TO 50
C
C     COORDINATE SYSTEM 0
C
  300 DO 310 I = 2,8
  310 T(I) = 0.
      T(1) = 1.
      T(5) = 1.
      T(9) = 1.
      GO TO 50
      END
