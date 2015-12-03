      SUBROUTINE NRLSUM
C
C     NRLSUM   OES2,OEF2/NRLSTR,NRLFOR/V,N,NMODES/V,N,NSHOCK(NDIR)/
C              C,Y,DIRECT=123/C,Y,SQRSS=0 $
C
C     NRLSUM COMPUTES NRL SUM STRESSES AND FORCES FOR DDAM. IT IS
C     ASSUMED THAT THE USER HAS REQUESTED STRESSES AND FORCES IN SORT2
C     FORMAT (BUT RESULTS WILL BE SORT1). NRLSUM READS ITEMS FOR AN
C     ELEMENT (FOR ALL SUBCASES) AND COMPUTES THE NRL SUM.  UP TO 3
C     SCRATCH FILES ARE USED TO STORE THE SUMS FOR EACH SHOCK DIRECTION.
C     PRINCIPAL STRESSES WILL BE COMPUTED BASED ON THE SUMS. THE NUMBER
C     OF SUBCASES IS NMODES*NSHOCK WITH THE ORDER 1-NMODES,
C     NMODES+1 - 2*NMODES, ... NSHOCK*NMODES.
C
C     (IF (SQRSS.EQ.1), SQUARE ROOT OF THE SUM OF THE SQUARES IS USED
C     INSTEAD OF NRL SUM
C
      INTEGER         FILE,BUF1,BUF2,BUF3,BUF4,OES2,SYSBUF,ELTYPE,SCR1,
     1                SCR2,SCR3,ELID,OLDTYP,SCR(3),
     2                OEF2,OFIL,IDIR(2),INUM(3),NSUB(3),DIRECT,SQRSS
      DIMENSION       SIG(6),SIGP(3),SMAT(3,3),DCOS(3,3)
      DIMENSION       Z(20),NAM(2),STRESS(146),ISTRES(146),MCB(7)
      COMMON /SYSTEM/ SYSBUF
      COMMON /BLANK / NMODES,NSHOCK,DIRECT,SQRSS
      COMMON /ZZZZZZ/ IZ(1)
      EQUIVALENCE    (Z(1),IZ(1)), (STRESS(1),ISTRES(1))
      EQUIVALENCE    (SIGP(1),SA), (SIGP(2),SB), (SIGP(3),SC)
      EQUIVALENCE    (SIG(1) ,SX), (SIG(2) ,SY), (SIG(3) ,SZ),
     1               (SIG(4),SXY), (SIG(5),SYZ), (SIG(6),SZX)
      DATA    OES2  , NRLSTR,SCR1,SCR2,SCR3 / 101,201,301,302,303/
      DATA    OEF2  , NRLFOR /102,202 /
      DATA    SCR   / 301,302,303     /
      DATA    IDIR  / 4HDIRE,4HCTIO   /
      DATA    INUM  / 4HN 1 ,4HN 2 ,4HN 3 /
      DATA    DTOR  / 0.0174532925E0  /
      DATA    NAM   / 4HNRLS,4HUM     /, I0 / 0 /
C
      LCORE= KORSZ(Z)
      BUF1 = LCORE- SYSBUF + 1
      BUF2 = BUF1 - SYSBUF
      BUF3 = BUF2 - SYSBUF
      BUF4 = BUF3 - SYSBUF
      IF (NSHOCK .EQ. 3) GO TO 20
      IF (NSHOCK .EQ. 2) GO TO 10
      BUF4 = BUF2
      BUF3 = BUF2
      GO TO 20
C
   10 BUF4 = BUF3
C
   20 LCORE = BUF4 - 1
      IF (LCORE .LE. 0) GO TO 1008
      NDIR = NSHOCK
      IF (NDIR  .GT. 1) GO TO 11
      NSUB(1) = DIRECT
      GO TO 14
   11 IF (NDIR   .GT.  2) GO TO 13
      IF (DIRECT .EQ. 23) GO TO 12
      NSUB(1) = 1
      NSUB(2) = 2
      IF (DIRECT .EQ. 13) NSUB(2) = 3
      GO TO 14
   12 NSUB(1) = 2
      NSUB(2) = 3
      GO TO 14
   13 NSUB(1) = 1
      NSUB(2) = 2
      NSUB(3) = 3
   14 CONTINUE
      IFIL = OES2
      OFIL = NRLSTR
C
   15 FILE = IFIL
      OLDTYP = 0
      CALL OPEN (*710,IFIL,Z(BUF1),0)
      CALL FWDREC (*1002,IFIL)
      CALL GOPEN (SCR1,Z(BUF2),1)
      IF (NSHOCK .GT. 1) CALL GOPEN (SCR2,Z(BUF3),1)
      IF (NSHOCK .EQ. 3) CALL GOPEN (SCR3,Z(BUF4),1)
C
   30 CALL READ (*410,*1003,IFIL,STRESS,146,1,IWORDS)
      ELTYPE= ISTRES(3)
      NWDS  = ISTRES(10)
      I5    = ISTRES(5)
      ELID  = ISTRES(5)/10
C
C     REFORMULATE TO SORT 1 FORMAT
C
      ISTRES(2) = 5
      IF (IFIL .EQ. OEF2) ISTRES(2) = 4
      ISTRES(141) = IDIR(1)
      ISTRES(142) = IDIR(2)
C
C     WRITE ONTO SCRATCH ONLY FOR NEW ELEMENT TYPE
C
      IF (ELTYPE .EQ. OLDTYP)  GO TO 45
      DO 35 I = 1,NSHOCK
      ISTRES(4) = I
      ISTRES(5) = I
      ISTRES(8) = I
      ISUB = NSUB(I)
      ISTRES(143) = INUM(ISUB)
      IF (OLDTYP .NE. 0) CALL WRITE (SCR(I),0,0,1)
      CALL WRITE (SCR(I),STRESS,146,1)
   35 CONTINUE
C
      OLDTYP = ELTYPE
C
C     READ STRESS INFO FOR NUMBER OF MODES AND SHOCK DIRECTIONS
C
      IF (NMODES*NWDS .GT. LCORE)  GO TO 1008
   45 DO 400 NS = 1,NSHOCK
      ISCR = 300 + NS
C
      CALL FREAD (IFIL,Z(1),NWDS*NMODES,0)
C
C     GO TO PROPER SECTION FOR EACH ELEMENT TYPE
C
C     FOR FORCES, COMPUTATIONS ARE EASIER. SO LETS NOT HAVE A COMPUTED
C     GO TO
C
      IF (IFIL .EQ. OES2) GO TO 46
C
      IF (ELTYPE.GE.20 .AND. ELTYPE.LE.33) GO TO 400
      IF (ELTYPE.GE.39 .AND. ELTYPE.LE.52) GO TO 400
      IF (ELTYPE.EQ.62 .OR.  ELTYPE.EQ.68 .OR. ELTYPE.EQ.69 .OR.
     1    ELTYPE.EQ.72) GO TO 400
      IF (ELTYPE.GE.65 .AND. ELTYPE.LE.67) GO TO 400
      IF (ELTYPE.EQ. 9 .OR.  ELTYPE.EQ.16 .OR. ELTYPE.EQ.73 .OR.
     1    ELTYPE.EQ.76) GO TO 400
      I3 = 1
      I2 = NWDS
      I1 = 2
      IF (ELTYPE.EQ.35 .OR.  ELTYPE.EQ.70 .OR. ELTYPE.EQ.71) I1 = 3
      GO TO 105
C
   46 CONTINUE
C
      GO TO ( 50, 60, 50, 70, 70, 80, 80, 80, 90, 50,
     1       100,100,100,400, 80, 90, 80, 80, 80,400,
     2       400,400,400,400,400,400,400,400,400,400,
     3       400,400,400,110,120,130,140,150,160,160,
     4       160,160,400,400,400,400,400,400,400,400,
     5       400,400,170,170,170,170,170,170,170,170,
     6       170, 90, 90, 80,220,220,220,400,400,180,
     7       190,400,200,200,200,210,400,400,400,400,
     8       400,400, 80), ELTYPE
C
C     ROD, TUBE, CONROD
C
   50 I1 = 2
      I2 = 4
      I3 = 2
      ASSIGN 55 TO IRET
      GO TO 390
C
C     IGNORE MARGINS OF SAFETY
C
   55 IZ(I0+3) = 1
      IZ(I0+5) = 1
      GO TO 395
C
C     BEAM
C
   60 I1 = 2
      I2 = 5
      I3 = 1
      ASSIGN 65 TO IRET
      GO TO 390
   65 Z(6) = Z(5) + AMAX1(Z(2),Z(3),Z(4))
      Z(7) = Z(5) + AMIN1(Z(2),Z(3),Z(4))
      IZ(I0+8) = 1
      I1 = 9
      I2 = 11
      I3 = 1
      ASSIGN 66 TO IRET
      GO TO 390
   66 Z(12) = Z(5) + AMAX1(Z(9),Z(10),Z(11))
      Z(13) = Z(5) + AMIN1(Z(9),Z(10),Z(11))
      IZ(I0+14) = 1
      GO TO 395
C
C     SHEAR
C
   70 I1 = 2
      I2 = 3
      I3 = 1
      ASSIGN 75 TO IRET
      GO TO 390
   75 IZ(I0+4) = 1
      GO TO 395
C
C     TRBSC, TRPLT, QDPLT, TRIA1, TRIA2, TRIA3, QUAD1, QUAD2, QUAD4
C
   80 I1 = 3
      I2 = 5
      I3 = 1
      J3 = 3
      J4 = 4
      J5 = 5
      J6 = 6
      J7 = 7
      J8 = 8
      J9 = 9
      ASSIGN 85 TO IRET
      GO TO 390
   85 SS = .5*(Z(J3) + Z(J4))
      ST = Z(J3) - Z(J4)
      SQ = SQRT(.25*ST**2 + Z(J5)**2)
      Z(J7) = SS + SQ
      Z(J8) = SS - SQ
      Z(J9) = SQ
      SD = 2.*Z(J5)
      IF (ABS(SD).LT.1.E-15 .AND. ABS(ST).LT.1.E-15) GO TO 87
      Z(J6) = ATAN2(SD,ST)*28.6478898
      GO TO 88
   87 Z(J6) = 0.
   88 IF (J3 .EQ. 11) GO TO 395
      IF (ELTYPE.EQ. 9 .OR. ELTYPE.EQ.16) GO TO 395
C                 TRMEM             QDMEM
      IF (ELTYPE.EQ.62 .OR. ELTYPE.EQ.63) GO TO 395
C                QDMEM1            QDMEM2
      IF (ELTYPE .EQ. 35) GO TO 125
C                  CONEAX
      I1 = 11
      I2 = 13
      I3 = 1
      J3 = 11
      J4 = 12
      J5 = 13
      J6 = 14
      J7 = 15
      J8 = 16
      J9 = 17
      GO TO 390
C
C     TRMEM, QDMEM,  QDMEM1, QDMEM2
C
   90 I1 = 2
      I2 = 4
      I3 = 1
      J3 = 2
      J4 = 3
      J5 = 4
      J6 = 5
      J7 = 6
      J8 = 7
      J9 = 8
      ASSIGN 85 TO IRET
      GO TO 390
C
C     CELAS1,2,3
C
  100 I1 = 2
      I2 = 2
      I3 = 1
  105 ASSIGN 395 TO IRET
      GO TO 390
C
C     BAR - ADD AXIAL STRESS TO EXTENSIONAL STRESSES DUE TO BENDING
C           BEFORE COMPUTING NRL SUMS. THEN ZERO OUT AXIAL STRESS
C           AND MAX AND MIN STRESSES
C
  110 I1 = 2
      I2 = 5
      I3 = 1
      DO 113 J = 1,NMODES
      ISUB = NWDS*(J-1)
      DO 111 I = 2,5
  111 Z(ISUB+I) = Z(ISUB+I)+Z(ISUB+6)
      DO 112 I = 10,13
  112 Z(ISUB+I) = Z(ISUB+I)+Z(ISUB+6)
  113 CONTINUE
      ASSIGN 115 TO IRET
      GO TO 390
  115 Z(6) = 0.
      Z(7) = 0.
      Z(8) = 0.
      IZ(I0+9) = 1
      I1   = 10
      I2   = 13
      I3   = 1
      ASSIGN 116 TO IRET
      GO TO 390
  116 Z(14) = 0.
      Z(15) = 0.
      IZ(I0+16) = 1
      GO TO 395
C
C     CONEAX
C
  120 I1 = 4
      I2 = 6
      I3 = 1
      J3 = 4
      J4 = 5
      J5 = 6
      J6 = 7
      J7 = 8
      J8 = 9
      J9 = 10
      ASSIGN 85 TO IRET
      GO TO 390
  125 IF (J3 .EQ. 12) GO TO 395
      I1 = 12
      I2 = 14
      I3 = 1
      J3 = 12
      J4 = 13
      J5 = 14
      J6 = 15
      J7 = 16
      J8 = 17
      J9 = 18
      GO TO 390
C
C     TRIARG
C
  130 I1 = 2
      I2 = 5
      I3 = 1
      GO TO 105
C
C     TRAPRG
C
  140 I1 = 2
      I2 = 21
      I3 = 1
      GO TO 105
C
C     TORDRG
C
  150 I1 = 2
      I2 = 16
      I3 = 1
      GO TO 105
C
C     TETRA, WEDGE, HEXA1, HEXA2
C
  160 I1 = 2
      I2 = 7
      I3 = 1
      ASSIGN 165 TO IRET
      GO TO 390
  165 Z(8) = SQRT((Z(2)-Z(3))**2 + (Z(3)-Z(4))**2 + (Z(4)-Z(2))**2 +
     1               6.*(Z(5)**2 + Z(6)**2 + Z(7)**2)) / 3.
      Z(9) = -(Z(2)+Z(3)+Z(4)) / 3.
      GO TO 395
C
C     DUM1 - DUM9
C
  170 I1 = 2
      I2 = 10
      I3 = 1
      GO TO 105
C
C     TRIAAX
C
  180 I1 = 3
      I2 = 11
      I3 = 1
      GO TO 105
C
C     TRAPAX
C
  190 I1 = 3
      I2 = 47
      I3 = 1
      GO TO 105
C
C     TRIM6, TRPLT1, TRSHL
C
  200 IEND = 8
      ISKIP= 8
      IF (ELTYPE .NE. 73) GO TO 201
      IEND = 4
      ISKIP= 7
  201 J2 = -5
      IJ = 0
  202 IJ = IJ + 1
      J2 = J2 + ISKIP
      J4 = J2 + 2
      I1 = J2
      I2 = J4
      I3 = 1
      ASSIGN 205 TO IRET
      GO TO 390
  205 SS = .5*(Z(J2)+Z(J2+1))
      ST = Z(J2) - Z(J2+1)
      SQ = SQRT(.25*ST**2 + Z(J4)**2)
      Z(J4+2) = SS + SQ
      Z(J4+3) = SS - SQ
      Z(J4+4) = SQ
      SD = 2.*Z(J4)
      IF (ABS(SD).LT.1.E-15 .AND. ABS(ST).LT.1.E-15) GO TO 206
      Z(J4+1) = ATAN2(SD,ST) * 28.6478898
      GO TO 207
  206 Z(J4+1) = 0.
  207 IF (IJ .LT. IEND) GO TO 202
      GO TO 395
C
C     IS2D8
C
  210 IJ = 0
      J2 = 1
  211 IJ = IJ + 1
      J2 = J2 + 5
      J4 = J2 + 2
      I1 = J2
      I2 = J4
      I3 = 1
      ASSIGN 215 TO IRET
      GO TO 390
  215 IF (IJ .LT. 8) GO TO 211
      GO TO 395
C
C     IHEX1,2,3
C
  220 I1 = 3
      I2 = 4
      I3 = 1
      ASSIGN 221 TO IRET
      GO TO 390
  221 I1 = 11
      IF (ELTYPE. EQ. 67) I1 = 12
C                   IHEX3
      I2 = I1+1
      ASSIGN 222 TO IRET
      GO TO 390
  222 I1 = I1 + 6
      I2 = I1 + 1
      ASSIGN 223 TO IRET
      GO TO 390
C
C     COMPUTE PRINCIPAL STRESSES
C
  223 SIG(1) = Z( 3)
      SIG(2) = Z(11)
      SIG(3) = Z(17)
      SIG(4) = Z( 4)
      SIG(5) = Z(12)
      SIG(6) = Z(18)
      IF (ELTYPE .NE. 67) GO TO 224
C                   IHEX3
      SIG(2) = Z(12)
      SIG(3) = Z(18)
      SIG(5) = Z(13)
      SIG(6) = Z(19)
  224 CONTINUE
C*****
C     SOLVE CUBIC EQUATION FOR PRINCIPAL STRESSES
C*****
C
C     S**3 + P*S**2 + Q*S + R = 0.0
C
C     REF. -- CRC STANDARD MATH TABLES 14TH ED., PP. 392,3
C
      RM = 0.0
      DO 262 I = 1,6
      IF (ABS(SIG(I)) .GT. RM) RM = ABS(SIG(I))
  262 CONTINUE
      IF (RM .LE. 0.0) GO TO 267
      THRESH = 1.0E-5
  264 DO 263 I = 1,6
      IF (ABS(SIG(I)/RM) .LT. THRESH) SIG(I) = 0.0
  263 CONTINUE
      RX = SX/RM
      RY = SY/RM
      RZ = SZ/RM
      RXY= SXY/RM
      RYZ= SYZ/RM
      RZX= SZX/RM
      P  =-RX - RY - RZ
      Q  = RX*RY + RY*RZ + RZ*RX - RXY**2 - RYZ**2 - RZX**2
      R  =-(RX*RY*RZ +2.0*RXY*RYZ*RZX -RX*RYZ**2 -RY*RZX**2 -RZ*RXY**2)
      A  = (3.0*Q - P**2)/3.0
      B  = (2.0*P**3 - 9.0*P*Q + 27.0*R)/27.0
      X  =-A**3/27.0
      IF (X .GT. 0.0) GO TO 270
C
C     CHECK FOR IMAGINARY ROOTS
C
      IF (ABS(X) .GT. RM*1.0E-6) GO TO 265
C
C     CHECK FOR 3 EQUAL ROOTS
C
      IF (ABS(B) .GT. 1.0E-6) GO TO 265
      X  = 0.0
      PHI= 0.0
      GO TO 275
  265 THRESH = 10.0*THRESH
      IF (THRESH .LT. 1.1E-3) GO TO 264
  267 SA = 0.0
      SB = 0.0
      SC = 0.0
      GO TO 280
  270 COSPHI =-(B/2.0)/SQRT(X)
      IF (ABS(COSPHI) .GT. 1.0) GO TO 265
      PHI= ACOS(COSPHI)
      X  = 2.0*SQRT(-A/3.0)
  275 SA = (X*COS(PHI/3.0)-P/3.0)*RM
      SB = (X*COS(PHI/3.0+120.0*DTOR)-P/3.0)*RM
      SC = (X*COS(PHI/3.0+240.0*DTOR)-P/3.0)*RM
      RM = 0.0
      DO 276 I = 1,3
      IF (ABS(SIGP(I)) .GT. RM) RM = ABS(SIGP(I))
  276 CONTINUE
      DO 277 I = 1,3
      IF (ABS(SIGP(I)/RM) .LT. 1.0E-5) SIGP(I) = 0.0
  277 CONTINUE
C*****
C     COMPUTE MEAN STRESS OR PRESSURE
C*****
  280 SN =-(SA+SB+SC)/3.0
C*****
C     COMPUTE OCTAHEDRAL SHEAR STRESS
C*****
      SO = SQRT(((SA+SN)**2 + (SB+SN)**2 + (SC+SN)**2)/3.0)
C*****
C     COMPUTE DIRECTION COSINES OF THE PRINCIPAL PLANES
C*****
      RM = 1.0E-6
      DO 600 I = 1,3
      IF (SIGP(I) .EQ. 0.0) GO TO 580
      SMAT(1,1) = 1.0 - SX/SIGP(I)
      SMAT(2,1) =-SXY/SIGP(I)
      SMAT(3,1) =-SZX/SIGP(I)
      SMAT(1,2) = SMAT(2,1)
      SMAT(2,2) = 1.0 - SY/SIGP(I)
      SMAT(3,2) =-SYZ/SIGP(I)
      SMAT(1,3) = SMAT(3,1)
      SMAT(2,3) = SMAT(3,2)
      SMAT(3,3) = 1.0 - SZ/SIGP(I)
      CALL SAXB (SMAT(1,1),SMAT(1,2),DCOS(1,I))
      RX = SADOTB(DCOS(1,I),DCOS(1,I))
      J  = 1
      CALL SAXB (SMAT(1,2),SMAT(1,3),DCOS(1,I))
      RY = SADOTB(DCOS(1,I),DCOS(1,I))
      IF (RY .GT. RX) J = 2
      CALL SAXB (SMAT(1,3),SMAT(1,1),DCOS(1,I))
      RZ = SADOTB(DCOS(1,I),DCOS(1,I))
      IF (RZ.GT.RY .AND. RZ.GT.RX) J = 3
      P = SMAT(1,J)
      Q = SMAT(2,J)
      R = SMAT(3,J)
      IF (J-2) 450,460,470
  450 J = 2
      GO TO 480
  460 J = 3
      GO TO 480
  470 J = 1
  480 S = SMAT(1,J)
      T = SMAT(2,J)
      V = SMAT(3,J)
      IF (ABS(Q)  .LE. RM) GO TO 500
      RX = V - T*R/Q
      IF (ABS(RX) .LE. RM) GO TO 490
      RZ =-(S - T*P/Q)/RX
      RY =-(P + R*RZ)/Q
  485 X  = 1.0 + RZ*RZ + RY*RY
      DCOS(1,I) = 1.0/SQRT(X)
      DCOS(2,I) = RY*DCOS(1,I)
      DCOS(3,I) = RZ*DCOS(1,I)
      GO TO 600
  490 RX = S - T*P/Q
      IF (ABS(RX) .LE. RM) GO TO 580
      RY =-R/Q
      X  = 1.0 + RY*RY
      DCOS(1,I) = 0.0
      DCOS(3,I) = 1.0/SQRT(X)
      DCOS(2,I) = RY*DCOS(3,I)
      GO TO 600
  500 IF (ABS(R) .LE. RM) GO TO 520
      RZ = -P/R
      IF (ABS(T) .LE. RM) GO TO 510
      RY =-(S - V*P/R)/T
      GO TO 485
  510 IF (ABS(S-V*P/R) .LE. RM) GO TO 580
      DCOS(1,I) = 0.0
      DCOS(2,I) = 1.0
      DCOS(3,I) = 0.0
      GO TO 600
  520 IF (ABS(P) .LE. RM) GO TO 580
      IF (ABS(V) .LE. RM) GO TO 530
      RZ =-T/V
      X  = 1.0 + RZ*RZ
      DCOS(1,I) = 0.0
      DCOS(2,I) = 1.0/SQRT(X)
      DCOS(3,I) = RZ*DCOS(2,I)
      GO TO 600
  530 IF (ABS(T) .LE. RM) GO TO 580
      DCOS(1,I) = 0.0
      DCOS(2,I) = 0.0
      DCOS(3,I) = 1.0
      GO TO 600
  580 DCOS(1,I) = 0.0
      DCOS(2,I) = 0.0
      DCOS(3,I) = 0.0
  600 CONTINUE
      IPTS = 0
      IF (ELTYPE .EQ. 67) IPTS = 1
C                   IHEX3
      Z(5) = SA
      Z(9) = SN
      Z(10)= SO
      Z(IPTS+13) = SB
      Z(IPTS+19) = SC
      DO 610 I = 1,3
      Z(      5+I) = DCOS(1,I)
      Z(IPTS+13+I) = DCOS(2,I)
      Z(IPTS+19+I) = DCOS(3,I)
  610 CONTINUE
      GO TO 395
C
C     PERFORM NRL SUMS
C
  390 DO 393 I = I1,I2,I3
      SUM  = 0.
      RMAX = 0.
      DO 392 J = 1,NMODES
      ISUB = NWDS*(J-1) + I
      SUM  = SUM + Z(ISUB)**2
      IF (ABS(Z(ISUB)) .GT. RMAX) RMAX = ABS(Z(ISUB))
  392 CONTINUE
      IF (SQRSS .EQ. 1) RMAX = 0.
      SUM = SUM  - RMAX**2
      SUM = RMAX + SQRT(SUM)
      Z(I)= SUM
  393 CONTINUE
C
      GO TO IRET, (55,65,66,75,85,115,116,165,205,215,221,222,223,395)
C
C     WRITE NRL SUMS TO APPROPRIATE SCRATCH FILE
C
  395 IZ(1) = I5
      CALL WRITE (ISCR,Z,NWDS,0)
C
  400 CONTINUE
C
C     DONE WITH THIS ELEMENT.  SINCE WE ARE WRITING IN SORT1, EOR IS
C     NEEDED ON SCRATCH FILE ONLY IF ELEMENT TYPE CHANGES.  THIS WILL BE
C     CHECKED ABOVE.  SKIP EOR ON OES2 AND GO BACK.
C
      FILE = IFIL
      CALL FWDREC (*1002,IFIL)
      GO TO 30
C
C     EOF ON OES2.  WRITE EOR ON SCRATCH FILE AND COPY THEM TO OUTPUT
C     DATA BLOCK.
C
  410 CALL CLOSE (IFIL,1)
C
      DO 415 I = 2,7
  415 MCB(I) = 1
      DO 420 I = 1,NSHOCK
      CALL WRITE (SCR(I),0,0,1)
      CALL CLOSE (SCR(I),1)
      MCB(1) = SCR(I)
      CALL WRTTRL (MCB)
  420 CONTINUE
C
      LCORE = BUF2 - 1
      CALL GOPEN (OFIL,Z(BUF1),1)
      DO 700 I = 1,NSHOCK
      CALL GOPEN (SCR(I),Z(BUF2),0)
C
  430 CALL READ (*690,*440,SCR(I),Z,LCORE,0,IWORDS)
      CALL WRITE (OFIL,Z,LCORE,0)
      GO TO 430
C
C     EOR
C
  440 CALL WRITE (OFIL,Z,IWORDS,1)
      GO TO 430
C
C     EOF
C
  690 CALL CLOSE (SCR(I),1)
C
  700 CONTINUE
C
      CALL CLOSE (OFIL,1)
      MCB(1) = OFIL
      CALL WRTTRL (MCB)
C
C     GO BACK FOR FORCES
C
  710 IF (IFIL .EQ. OEF2) RETURN
      IFIL = OEF2
      OFIL = NRLFOR
      GO TO 15
C
 1002 N = -2
      GO TO 1010
 1003 N = -3
      GO TO 1010
 1008 N = -8
      FILE = 0
 1010 CALL MESAGE (N,FILE,NAM)
      RETURN
      END
