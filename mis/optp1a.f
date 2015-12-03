      SUBROUTINE OPTP1A (ELT,ELOP,ELE,DTYP)
C
      INTEGER         ELT(1),IWD(28),COUNT,EST,SYSBUF,OUTTAP,ELOP(2,1),
     1                YCOR,ECOR,B1P1,IE(1),IPT(21),IMAT(1),NAME(2),
     2                DTYP(1)
      REAL            ELE(1)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /BLANK / SKP1(2),COUNT,SKP2(2),YCOR,B1P1,NPOW,
     1                NELW,NWDSE,NPRW,NWDSP,SKP3,
     2                MPT,SKP4(3),EST,SKP5(2),NELTYP,ITYPE(21)
      COMMON /OPTPW1/ ECOR,E(1)
      COMMON /GPTA1 / NTYPES,LAST,INCR,NE(1)
      COMMON /SYSTEM/ SYSBUF,OUTTAP
      COMMON /MATIN / MATID,INFLAG,TEMP,PLA,SINTH,COSTH
      COMMON /MATOUT/ OMAT(20)
      COMMON /NAMES / NRD,NOEOR,NWRT,NWEOR
      EQUIVALENCE     (E(1),IE(1)), (OMAT(1),IMAT(1)), (K1,C1)
      DATA    NAME  / 4H OPT,4HP1A  /
C
C     POINTER TO IPT ARRAY - ZERO MEANS ELEMENT NOT USED.
C     UPDATE IPT DIMENSIONS AS NEW ELEMENTS ARE ADDED
C
      DATA    IPT   / 15,17,21,11,23,  25,11,13,11, 1,  5,9,7,9,19,
     1                 9, 9, 3,27,27,   0/
C
C     WORD POINTER TO EST AND MATERIAL STRESS LIMITS
C     WORD 1 = 100*WORD TO OPTIMIZE (EST - IF.NE.0) + ALTERNATE
C     WORD 2 = 100*WORD FOR STRESS LIMIT + ALTERNATE
C              WHERE 1 = SHEAR
C                    2 = TENSION/COMPRESSION
C                    3 = ANY/ALL NONZERO
C
      DATA    IWD   / 506,201 , 500,200 , 700,100 , 709,303 , 700,300 ,
C                      11        13        15        17        19
     1                800,300 , 810,303 ,1718,202 , 910,202 ,1011,303 ,
C                      21        23        25       27
     2               1300,303 , 800,303 , 800,303 ,1400,300 /
C
      NELW   = 0
      SINTH  = 0.0
      COSTH  = 1.0
      PLA    = 0.0
      INFLAG = 2
C
C     COPY POINTER ARRAY INTO CORE
C
      DO 10 I = 1,NTYPES
      ELT(I) = DTYP(I)
   10 CONTINUE
C
C     ZERO OUT POINTER ARRAY
C
      I1 = 2*(NPOW+1)
      DO 20 I = 2,I1
   20 ELOP(I,1) = 0
      ELOP(1,1) = 1
C
C     READ IN ELEMENT TYPE
C
   30 CALL READ (*120,*170,EST,IETYP,1,NOEOR,I)
      IF (IETYP .GT. NTYPES) GO TO 110
      INTYP = DTYP(IETYP)
      IF (INTYP .LE. 0) GO TO 110
C
C     DECODE LIMITS NEEDED
C
      I  = IPT(INTYP)
      J2 = IWD(I)
      J1 = J2/100
      J2 = J2 - J1*100
      I2 = IWD(I+1)
      I1 = I2/100
      I2 = I2 - I1*100
      NEST = (IETYP-1)*INCR + 12
      NEST = NE(NEST)
      IF (NEST .GT. ECOR) GO TO 200
   40 CALL READ (*160,*115,EST,E,NEST,NOEOR,K1)
      MATID = IE(J1-1)
      IF (MATID .EQ. 0) MATID = IE(J2-1)
      TEMP = E(NEST)
      CALL MAT (IE(1))
C
C     TEST IF PERTINENT STRESS LIMITS ARE ZERO
C
      K1 = 0
      K2 = 0
      IF (I1.EQ.2 .AND. I2.EQ.2) GO TO 50
C
C     SHEAR
C
      IF (OMAT(15) .NE. 0.0 ) GO TO 50
      IF (I1 .NE. 2) K1 = 1
      IF (I2.EQ.1  .OR. I2.EQ.3) K2 = 1
   50 IF (I1.EQ.1 .AND. I2.LE.1) GO TO 70
C
C     TENSION
C
      IF (OMAT(13) .NE. 0.0) GO TO 60
      IF (I1 .GT. 1) K1 = K1 + 1
      IF (I2 .GT. 1) K2 = K2 + 1
C
C     COMPRESSION
C
   60 IF (OMAT(14) .NE. 0.0) GO TO 70
      IF (I1 .GT. 1) K1 = K1 + 1
      IF (I2 .GT. 1) K2 = K2 + 1
C
   70 IF (K1.GE.I1 .AND. K2.GE.I2) GO TO 40
C
C     CHECK IF PROPERTY IS NONZERO AND STORE INFO IN PID POINTER
C
      IF (E(J1) .NE. 0.0) GO TO 90
   80 IF (E(J2) .EQ. 0.0) GO TO 40
C
      IF (K2 .GE. I2) GO TO 40
C
C     ALTERNATE PROPERTY USED
C
      K1 = J2*100 + I2
      GO TO 100
C
   90 IF (K1 .GE. I1) GO TO 80
C
C     PRIMARY PROPERTY USED
C
      K1 = J1*100 + I1
  100 IF (NELW+5 .GT. YCOR) GO TO 180
      ELE(NELW+1) = E(1)
      ELE(NELW+2) = OMAT(13)
      ELE(NELW+3) = OMAT(14)
      ELE(NELW+4) = OMAT(15)
C
C     NOTE, K1 = C1
C
      ELE(NELW+5) = C1
      NELW = NELW + NWDSE
       GO TO 40
C
C     NEW ELEMENT TYPE
C
  110 CONTINUE
      CALL FREAD (EST,0,0,NWEOR)
      IF (IETYP .GT. NTYPES) GO TO 120
      IF (INTYP .LE. 0) GO TO 30
  115 ELOP(1,INTYP+1) = NELW + 1
      GO TO 30
C
C     EOF
C
  120 I1 = NPOW + 1
      DO 130 I = 2,I1
      IF (ELOP(1,I) .GT. 0) GO TO 130
      ELOP(1,I) = ELOP(1,I-1)
  130 CONTINUE
      IF (NELW .NE. 0) GO TO 150
      CALL PAGE2 (-2)
      WRITE  (OUTTAP,140) UFM
  140 FORMAT (A23,' 2295, NO ELEMENTS EXIST FOR OPTIMIZATION.')
      COUNT = -1
  150 RETURN
C
C     ILLEGAL EOF
C
  160 CALL MESAGE (-2,EST,NAME)
C
C     ILLEGAL EOR
C
  170 CALL MESAGE (-3,EST,NAME)
C
C     INSUFFICIENT CORE
C
  180 CALL PAGE2 (-2)
      WRITE  (OUTTAP,190) UFM,NAME,B1P1,IE(1)
  190 FORMAT (A23,' 2296, INSUFFICIENT CORE ',2A4,1H(,I10,' ), ELEMENT',
     1        I9)
      NELW = 0
      GO TO 150
  200 CALL PAGE2 (-2)
      WRITE (OUTTAP,190) NAME,ECOR,IETYP
      NELW = 0
      GO TO 150
      END
