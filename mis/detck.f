      SUBROUTINE DETCK (JARG,IFGPST,NPVT)
C
C     COMMENTS FROM G.CHAN/UNISYS, 5/1991,
C     THIS ROUTINE WAS NAMED DETCKX BEFORE, WHICH HAD NOT BEEN TESTED.
C     THE ONE THAT USED TO BE DETCK APPEARS TO BE AN OLDER VERSION, AND
C     SHOULD BE REPLACED BY THIS ONE, IF THIS ONE WORKS
C
C     THIS ROUTINE GENERATES THE GRID POINT SINGULARITY TABLE BY
C     EXAMINING THE TRANSLATIONAL AND DIAGONAL 3 X 3 SUBMATRICES OF THE
C     KGG MATRIX.
C     IF JARG = 0, THE PIVOT POINT HAS ELEMENTS ATTACHED TO IT.
C     IF JARG =-1, THE PIVOT IS A SCALAR POINT AND NO ELEMENTS ARE
C                  CONNECTED TO IT.
C     IF JARG = 1, THE PIVOT POINT IS A GRID POINT AND NO ELEMENTS ARE
C                  CONNECTED TO IT.
C
      INTEGER          TNWDS,IARRAY(8),IZ(1),BACK,NAME(2)
      DOUBLE PRECISION D,B,DZ,FL,R,M,TEMP,FM,FR,DET,CONST,DTOL
      COMMON /MA1XX /  D(18),B(9),DZ(1),FL(3),R(3),M(3)
      COMMON /SYSTEM/  ISYS(69),TOLEL
      EQUIVALENCE      (IZ(1),DZ(1)),(IARRAY(1),IORDER),(IARRAY(2),NWDS)
      DATA    NAME  /  4HDETC,4HK   /,  NEOR  / 0 /
C
      DTOL = TOLEL
      IARG = JARG
      IF (IARG) 10,20,25
   10 IORDER = 1
      NWDS   = 1
      IARRAY(3) = NPVT
      CALL WRITE (IFGPST,IARRAY(1),3,NEOR)
      RETURN
C
C     AT THIS POINT, BOTH TRANSLATIONAL AND ROTATIONAL DIAGONAL 3X3 S
C     ARE STORED IN THE D ARRAY.  HENCE WE PROCESS THEM.
C
   20 CONTINUE
   25 CONTINUE
      IP = NPVT - 1
      ASSIGN 450 TO IGOTO
      IF (IARG .NE. 1) GO TO 30
      ASSIGN 425 TO BACK
      GO TO 425
   30 ASSIGN 50 TO BACK
      DO 40 I = 1,9
   40 B(I) = D(I)
      GO TO 90
   50 DO 60 I = 1,9
   60 B(I) = D(I+9)
C
C     INSURE THE SYMMETRY OF THE B MATRIX
C
      IF (B(2).NE.0.0D0 .AND. B(4).NE.0.0D0) GO TO 65
      B(2) = 0.0D0
      B(4) = 0.0D0
      GO TO 70
   65 TEMP = (B(2) + B(4))/2.0D0
      B(2) = TEMP
      B(4) = TEMP
   70 IF (B(3).NE.0.0D0 .AND. B(7).NE.0.0D0) GO TO 75
      B(3) = 0.0D0
      B(7) = 0.0D0
      GO TO 80
   75 TEMP = (B(3) + B(7))/2.0D0
      B(3) = TEMP
      B(7) = TEMP
   80 IF (B(6).NE.0.0D0 .AND. B(8).NE.0.0D0) GO TO 85
      B(6) = 0.0D0
      B(8) = 0.0D0
      GO TO 90
   85 TEMP = (B(6) + B(8))/2.0D0
C
C     SCALE THE MATRIX BY DIVIDING EACH ELEMENT OF B BY THE LARGEST
C     ELEMENT. IF THE LARGEST ELEMENT IS NON-POSITIVE, THE SINGULARITY
C     IS OF ORDER 3.
C
   90 TEMP = B(1)
      DO 100 I = 2,9
      IF (B(I) .GT. TEMP) TEMP = B(I)
  100 CONTINUE
      IF (TEMP .LE. 0.0D0) GO TO 425
      DO 110 I = 1,9
  110 B(I) = B(I)/TEMP
C
C     FIND THE SQUARES OF THE MAGNITUDES OF THE VECTORS OF THE ROWS OF
C     THE B MATRIX.
C
      IORDER = 0
      J = 0
      DO 120 I = 1,9,3
      J = J + 1
      FL(J) = B(I)**2 + B(I+1)**2 + B(I+2)**2
      IF (FL(J) .EQ. 0.0D0) IORDER = IORDER + 1
  120 CONTINUE
      IF (IORDER .EQ. 2)  GO TO 410
      IF (IORDER .EQ. 0)  GO TO 250
C
C     AT THIS POINT ONE AND ONLY ONE FL(I) IS ZERO.
C
      DO 130 I = 1,3
      ISAVE = I
      IF (FL(I) .EQ. 0. 0D0) GO TO (140,150,160), ISAVE
  130 CONTINUE
      CALL MESAGE (-30,26,NAME)
  140 FM = B(5)*B(9) - B(6)*B(8)
      FR = DSQRT((B(5)**2 + B(6)**2)*(B(8)**2 + B(9)**2))
      GO TO 170
  150 FM = B(1)*B(9) - B(3)*B(7)
      FR = DSQRT((B(1)**2 + B(3)**2)*(B(7)**2 + B(9)**2))
      GO TO 170
  160 FM = B(1)*B(5) - B(2)*B(4)
      FR = DSQRT((B(1)**2 + B(2)**2)*(B(4)**2 + B(5)**2))
  170 IF (FM .EQ. 0.0D0) GO TO 175
      IF (FR.LE.0.0D0 .OR. FM/FR.GE.DTOL) GO TO 240
C
C     HERE WE HAVE THAT THE ORDER OF THE SINGULARITY IS 2.
C
  175 IORDER = 2
      NWDS   = 0
      TNWDS  = 2
      GO TO (180,190,200), ISAVE
  180 K1   = 5
      K2   = 9
      INC1 = 1
      INC2 = 3
      INC3 = 2
      GO TO 210
  190 K1   = 1
      K2   = 9
      INC1 = 2
      INC2 = 3
      INC3 = 1
      GO TO 210
  200 K1   = 1
      K2   = 5
      INC1 = 3
      INC2 = 2
      INC3 = 1
  210 IF (B(K1).LE.0.0D0 .AND. B(K2).LE.0.0D0) GO TO 425
      IF (B(K1) .LE. 0.0D0) GO TO 220
      NWDS      = 2
      TNWDS     = 4
      IARRAY(3) = IP + INC1
      IARRAY(4) = IP + INC2
      IPOINT    = 5
      GO TO 230
  220 IPOINT = 3
  230 IF (B(K2) .LE. 0.0D0) GO TO 430
      NWDS  = NWDS  + 2
      TNWDS = TNWDS + 2
      IARRAY(IPOINT  ) = IP + INC1
      IARRAY(IPOINT+1) = IP + INC3
      GO TO 430
C
C     AT THIS POINT WE HAVE THAT ONE AND ONLY ONE FL IS ZERO BUT THAT
C     ORDER OF THE SINGULARITY IS 1.
C
  240 IORDER    = 1
      NWDS      = 1
      TNWDS     = 3
      IARRAY(3) = IP + ISAVE
      GO TO 430
C
C     AT STATEMENT NO. 250, WE HAVE THAT ALL THE FL(I) ARE .GT. 0.0D0,
C     SO THAT THE DETERMINANT, DET, OF B MUST BE COMPUTED.
C
  250 DET = B(1)*(B(5)*B(9) - B(6)*B(8)) - B(2)*(B(4)*B(9) - B(6)*B(7))
     1    + B(3)*(B(4)*B(8) - B(5)*B(7))
      CONST = 0.05D0*DTOL*FL(1)*FL(2)*FL(3)
      IF (DET .GT. CONST) GO TO 440
C
C     COMPUTE M(I) AND R(I)
C
      M(1) = B(5)*B(9) - B(6)*B(8)
      M(2) = B(1)*B(9) - B(3)*B(7)
      M(3) = B(1)*B(5) - B(2)*B(4)
      R(1) = DSQRT(B(5)**2 + B(6)**2) * DSQRT(B(8)**2 + B(9)**2)
      R(2) = DSQRT(B(1)**2 + B(3)**2) * DSQRT(B(7)**2 + B(9)**2)
      R(3) = DSQRT(B(1)**2 + B(2)**2) * DSQRT(B(4)**2 + B(5)**2)
C
C     FIND I1, J1, K1
C     SUCH THAT M(I1)/R(I1) .GE. M(J1)/R(J1) .GE. M(K1)/R(K1)
C
      I1 = 1
      J1 = 2
      K1 = 3
      IF (M(1)*R(2) .GE. M(2)*R(1)) GO TO 270
      I1 = 2
      J1 = 1
  270 IF (M(I1)*R(K1) .GE. M(K1)*R(I1)) GO TO 280
      ITEMP = I1
      I1    = K1
      K1    = ITEMP
  280 IF (M(J1)*R(K1) .GE. M(K1)*R(J1)) GO TO 290
      ITEMP = J1
      J1    = K1
      K1    = ITEMP
  290 IF (M(I1) .GE. R(I1)*DTOL) GO TO 400
C
C     HERE THE SINGULARITY IS OF ORDER 2.
C
      NWDS   = 0
      TNWDS  = 2
      IORDER = 2
C
C     FIND II, JJ, KK SUCH THAT B(II) .GE. B(JJ) .GE. B(KK)
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
  330 IF (B(LL) .LE. 0.0D0) GO TO 430
      NWDS  = NWDS  + 2
      TNWDS = TNWDS + 2
      IF (LL - 5) 340,350,360
  340 INC1 = 2
      INC2 = 3
      GO TO 370
  350 INC1 = 1
      INC2 = 3
      GO TO 370
  360 INC1 = 1
      INC2 = 2
  370 IARRAY(IPOINT  ) = IP + INC1
      IARRAY(IPOINT+1) = IP + INC2
      IPOINT = IPOINT + 2
      KOUNT  = KOUNT  + 1
      IF (KOUNT - 2) 380,390,430
  380 LL = JJ
      GO TO 330
  390 LL = KK
      GO TO 330
C
C     AT THIS POINT THE SINGULARITY IS OF ORDER 1.
C
  400 IORDER = 1
      NWDS   = 1
      TNWDS  = 3
      IARRAY(3) = IP + I1
      IF (M(J1) .LT. R(J1)*DTOL) GO TO 430
      NWDS  = 2
      TNWDS = 4
      IARRAY(4) = IP + J1
      IF (M(K1) .LT. R(K1)*DTOL) GO TO 430
      NWDS  = 3
      TNWDS = 5
      IARRAY(5) = IP + K1
      GO TO 430
C
C     AT THIS POINT 2 ROWS OF THE B MATRIX ARE IDENTICALLY ZERO.
C
  410 NWDS   = 2
      TNWDS  = 4
      IPOINT = 2
      DO 420 I = 1,3
      IF (FL(I) .NE. 0.0D0) GO TO 420
      IPOINT = IPOINT + 1
      IARRAY(IPOINT) = IP + I
  420 CONTINUE
      GO TO 430
C
C     THE SINGULARITY IS OF ORDER 3
C
  425 IORDER = 3
      NWDS   = 3
      TNWDS  = 5
      IARRAY(3) = IP + 1
      IARRAY(4) = IP + 2
      IARRAY(5) = IP + 3
C
C     WRITE IARRAY ON THE GPST FILE.
C
  430 CALL WRITE (IFGPST,IARRAY(1),TNWDS,NEOR)
  440 GO TO IGOTO, (450,460)
  450 ASSIGN 460 TO IGOTO
      IP = IP + 3
      GO TO BACK, (50,425)
  460 CONTINUE
      RETURN
      END
