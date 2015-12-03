      SUBROUTINE GPSTGS
C
C     THIS SUBROUTINE GENERATES THE GRID POINT SINGULARITY TABLE
C     BY EXAMINING THE TRANSLATIONAL AND ROTATIONAL 3 X 3
C     SUBMATRICES ALONG THE LEADING DIAGONAL OF THE INPUT
C     STIFFNESS MATRIX
C
      DIMENSION   IARRAY(8), ISUBNM(2)
C
      INTEGER     GPST     , TTLWDS
C
      REAL        B(9), FL(3), D
      REAL        M(3), R(3) , TEMP, FM, FR, DET, CONST, DTOL
C
      COMMON /GPSTGX/ GPST , IGPST, NPVT, NSING , IBUF2
      COMMON /GPSTGY/ D(18)
      COMMON /SYSTEM/ ISYS(69)    , TOLEL
      COMMON /ZZZZZZ/ IZ(1)
C
      EQUIVALENCE (IORDER, IARRAY(1)), (NWDS, IARRAY(2))
C
      DATA ISUBNM / 4HGPST,4HG   /
C
      DTOL = TOLEL
C
C AT THIS POINT, BOTH TRANSLATIONAL AND ROTATIONAL DIAGONAL 3X3 S ARE
C STORED IN THE D ARRAY.  HENCE WE PROCESS THEM.
C
      IP = NPVT - 1
      ASSIGN 470 TO IGOTO
      ASSIGN 20 TO IBACK
      DO 10 I = 1,9
   10 B(I) = D(I)
      GO TO 90
   20 DO 30 I = 1,9
   30 B(I) = D(I+9)
C
C INSURE THE SYMMETRY OF THE B MATRIX
C
      IF (B(2) .NE. 0.0 .AND. B(4) .NE. 0.0) GO TO 40
      B(2) = 0.0
      B(4) = 0.0
      GO TO 50
   40 TEMP = (B(2) + B(4)) / 2.0
      B(2) = TEMP
      B(4) = TEMP
   50 IF (B(3) .NE. 0.0 .AND. B(7) .NE. 0.0) GO TO 60
      B(3) = 0.0
      B(7) = 0.0
      GO TO 70
   60 TEMP = (B(3) + B(7)) / 2.0
      B(3) = TEMP
      B(7) = TEMP
   70 IF (B(6) .NE. 0.0 .AND. B(8) .NE. 0.0) GO TO 80
      B(6) = 0.0
      B(8) = 0.0
      GO TO 90
   80 TEMP = (B(6) + B(8)) / 2.0
C
C SCALE THE MATRIX BY DIVIDING EACH ELEMENT OF B BY THE LARGEST ELEMENT.
C IF THE LARGEST ELEMENT IS NON-POSITIVE, THE SINGULARITY IS OF ORDER 3.
C
   90 TEMP = B(1)
      DO 100 I = 2,9
      IF (B(I) .GT. TEMP) TEMP = B(I)
  100 CONTINUE
      IF (TEMP .LE. 0.0) GO TO 430
      DO 110 I = 1,9
  110 B(I) = B(I) / TEMP
C
C FIND THE SQUARES OF THE MAGNITUDES OF THE VECTORS OF THE ROWS OF THE
C B MATRIX.
C
      IORDER = 0
      J = 0
      DO 120 I = 1,9,3
      J = J + 1
      FL(J) = B(I)**2 + B(I+1)**2 + B(I+2)**2
      IF (FL(J) .EQ. 0.0) IORDER = IORDER + 1
  120 CONTINUE
      IF (IORDER .EQ. 2)  GO TO 410
      IF (IORDER .EQ. 0)  GO TO 260
C
C AT THIS POINT ONE AND ONLY ONE FL(I) IS ZERO.
C
      DO 130 I = 1,3
      ISAVE = I
      IF (FL(I) .EQ. 0. 0) GO TO (140,150,160), ISAVE
  130 CONTINUE
      CALL MESAGE (-30,26,ISUBNM)
  140 FM = B(5) * B(9)  -  B(6) * B(8)
      FR = SQRT( (B(5)**2  +  B(6)**2)  *  (B(8)**2  +  B(9)**2) )
      GO TO 170
  150 FM = B(1) * B(9)  -  B(3) * B(7)
      FR = SQRT( (B(1)**2  +  B(3)**2)  *  (B(7)**2  +  B(9)**2) )
      GO TO 170
  160 FM = B(1) * B(5)  -  B(2) * B(4)
      FR = SQRT( (B(1)**2  +  B(2)**2)  *  (B(4)**2  +  B(5)**2) )
  170 IF ( FM   .EQ. 0.0 ) GO TO 180
      IF ( FR   .LE. 0.0 ) GO TO 250
      IF ( FM/FR .GE. DTOL ) GO TO 250
C
C HERE WE HAVE THAT THE ORDER OF THE SINGULARITY IS 2.
C
  180 IORDER = 2
      NWDS   = 0
      TTLWDS  = 2
      GO TO (190,200,210), ISAVE
  190 K1   = 5
      K2   = 9
      INC1 = 1
      INC2 = 3
      INC3 = 2
      GO TO 220
  200 K1   = 1
      K2   = 9
      INC1 = 2
      INC2 = 3
      INC3 = 1
      GO TO 220
  210 K1   = 1
      K2   = 5
      INC1 = 3
      INC2 = 2
      INC3 = 1
  220 IF (B(K1) .LE. 0.0  .AND.  B(K2) .LE. 0.0) GO TO 430
      IF (B(K1) .LE. 0.0) GO TO 230
      NWDS      = 2
      TTLWDS     = 4
      IARRAY(3) = IP + INC1
      IARRAY(4) = IP + INC2
      IPOINT    = 5
      GO TO 240
  230 IPOINT = 3
  240 IF (B(K2) .LE. 0.0) GO TO 440
      NWDS = NWDS + 2
      TTLWDS = TTLWDS + 2
      IARRAY(IPOINT)   = IP + INC1
      IARRAY(IPOINT+1) = IP + INC3
      GO TO 440
C
C AT THIS POINT WE HAVE THAT ONE AND ONLY ONE FL IS ZERO BUT THAT ORDER
C OF THE SINGULARITY IS 1.
C
  250 IORDER    = 1
      NWDS      = 1
      TTLWDS    = 3
      IARRAY(3) = IP + ISAVE
      GO TO 440
C
C AT STATEMENT NO. 260, WE HAVE THAT ALL THE FL(I) ARE .GT. 0.0, SO
C THAT THE DETERMINANT, DET, OF B MUST BE COMPUTED.
C
  260 DET = B(1) * ( B(5)*B(9) - B(6)*B(8) )
     1    - B(2) * ( B(4)*B(9) - B(6)*B(7) )
     2    + B(3) * ( B(4)*B(8) - B(5)*B(7) )
      CONST = 0.05*DTOL * FL(1) * FL(2) * FL(3)
      IF (DET .GT. CONST) GO TO 460
C
C COMPUTE M(I) AND R(I)
C
      M(1) = B(5) * B(9) - B(6) * B(8)
      M(2) = B(1) * B(9) - B(3) * B(7)
      M(3) = B(1) * B(5) - B(2) * B(4)
      R(1) = SQRT ( B(5)**2 + B(6)**2 ) * SQRT ( B(8)**2 + B(9)**2 )
      R(2) = SQRT ( B(1)**2 + B(3)**2 ) * SQRT ( B(7)**2 + B(9)**2 )
      R(3) = SQRT ( B(1)**2 + B(2)**2 ) * SQRT ( B(4)**2 + B(5)**2 )
C
C FIND I1,J1,K1 SUCH THAT M(I1)/R(I1) .GE. M(J1)/R(J1) .GE. M(K1)/R(K1)
C
      I1 = 1
      J1 = 2
      K1 = 3
      IF (M(1)*R(2).GE.M(2)*R(1)) GO TO 270
      I1 = 2
      J1 = 1
  270 IF (M(I1)*R(K1).GE.M(K1)*R(I1)) GO TO 280
      ITEMP = I1
      I1    = K1
      K1    = ITEMP
  280 IF (M(J1)*R(K1).GE.M(K1)*R(J1)) GO TO 290
      ITEMP = J1
      J1    = K1
      K1    = ITEMP
  290 IF (M(I1).GE.R(I1)*DTOL) GO TO 400
C
C HERE THE SINGULARITY IS OF ORDER 2.
C
      NWDS   = 0
      TTLWDS = 2
      IORDER = 2
C
C FIND II, JJ, KK SUCH THAT B(II) .GE. B(JJ) .GE. B(KK)
C
      II = 1
      JJ = 5
      KK = 9
      IF (B(1) .GE. B(5)) GO TO 300
      II = 5
      JJ = 1
  300 IF (B(II) .GE. B(KK)) GO TO 310
      ITEMP = II
      II    = KK
      KK    = ITEMP
  310 IF (B(JJ) .GE. B(KK)) GO TO 320
      ITEMP = JJ
      JJ    = KK
      KK    = ITEMP
  320 LL    = II
      KOUNT = 0
      IPOINT= 3
  330 IF (B(LL) .LE. 0.0) GO TO 440
      NWDS   = NWDS + 2
      TTLWDS = TTLWDS + 2
      IF (LL - 5) 340,350,360
  340 INC1 = 2
      INC2 = 3
      GO TO 370
  350 INC1 = 1
      INC2 = 3
      GO TO 370
  360 INC1 = 1
      INC2 = 2
  370 IARRAY(IPOINT)   = IP + INC1
      IARRAY(IPOINT+1) = IP + INC2
      IPOINT = IPOINT + 2
      KOUNT  = KOUNT  + 1
      IF (KOUNT - 2) 380,390,440
  380 LL = JJ
      GO TO 330
  390 LL = KK
      GO TO 330
C
C AT THIS POINT THE SINGULARITY IS OF ORDER 1.
C
  400 IORDER = 1
      NWDS   = 1
      TTLWDS = 3
      IARRAY(3) = IP + I1
      IF (M(J1).LT.R(J1)*DTOL) GO TO 440
      NWDS   = 2
      TTLWDS = 4
      IARRAY(4) = IP + J1
      IF (M(K1).LT.R(K1)*DTOL) GO TO 440
      NWDS   = 3
      TTLWDS = 5
      IARRAY(5) = IP + K1
      GO TO 440
C
C AT THIS POINT 2 ROWS OF THE B MATRIX ARE IDENTICALLY ZERO.
C
  410 NWDS   = 2
      TTLWDS = 4
      IPOINT = 2
      DO 420 I = 1,3
      IF (FL(I) .NE. 0.0) GO TO 420
      IPOINT = IPOINT + 1
      IARRAY(IPOINT) = IP + I
  420 CONTINUE
      GO TO 440
C
C THE SINGULARITY IS OF ORDER 3
C
  430 IORDER = 3
      NWDS   = 3
      TTLWDS = 5
      IARRAY(3) = IP + 1
      IARRAY(4) = IP + 2
      IARRAY(5) = IP + 3
C
C WRITE IARRAY ON THE GPST FILE.
C
  440 IF (IGPST.EQ.1) GO TO 450
      IGPST = 1
      CALL GOPEN (GPST,IZ(IBUF2),1)
  450 NSING = NSING + 1
      CALL WRITE (GPST,IARRAY,TTLWDS,0)
  460 GO TO IGOTO, (470,480)
  470 ASSIGN 480 TO IGOTO
      IP = IP + 3
      GO TO IBACK, (20,430)
  480 CONTINUE
      RETURN
      END
