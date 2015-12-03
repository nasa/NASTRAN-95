      SUBROUTINE SSGHT2 (FILE,DELTA,UNI)
C
C     THIS ROUTINE USES THE TEMPERATURE VECTOR DATA TO CALCULATE
C     LOAD VECTER TERMS WITH THE EQUATION-
C
C     DELTAP = (K(TI) - K(TO))*T1
C          WHERE       TO IS THE INITIAL TEMPERATURE
C                      TI IS THE  NEW  TEMPERATURE VECTOR
C                      K  IS THE TEMPERATURE DEPENDENT CONDUCTIVITY
C                          MATRIX
C                      DELTAP  IS THE NONLINEAR LOAD
C
      INTEGER          ELID,SUB,SIL,NPTS(15),NELS(15),IP(4),SMAP(52),
     1                 FILE,FLAG,SINDX(4),SUBR(2)
      REAL             MATO,MATOUT,TEMP,UNI(1),DELTA(1)
      DOUBLE PRECISION CONSTD,DRTEMP(3,4),DRT(4,4),C(12),K(9),KQ(9),
     1                 DR(3,4),T1(8),EL,AREA,RBAR,PI,FACT,DETERM,DADOTB
      COMMON/ CONDAD/  CONSTD(5)
      COMMON/ MATIN /  MATID,INFLAG,TEMP,DUM,SINTH,COSTH
      COMMON/ HMTOUT/  MATOUT(6)
      COMMON/ ESTOUT/  ELID,SUB,NAME(2),SIL(8),IMAT,AF,THETA,R(3,8),
     1                 MATO(6)
      EQUIVALENCE      (CONSTD(1),PI)
      DATA    NPTS  /  2,3,4,3,4,4,6,8, 8,1,2,2,3,4,2 /
      DATA    NELS  /  1,1,4,1,4,1,3,5,10,1,1,1,1,4,1 /
      DATA    SUBR  /  4HSSGH ,4HT2   /
      DATA    SMAP  /  1      ,2      ,3      ,6      ,
     1                 1      ,2      ,6      ,5      ,
     2                 1      ,4      ,5      ,6      ,
     3                 1      ,2      ,3      ,6      ,
     4                 1      ,3      ,4      ,8      ,
     5                 1      ,3      ,8      ,6      ,
     6                 1      ,5      ,6      ,8      ,
     7                 3      ,6      ,7      ,8      ,
     8                 2      ,3      ,4      ,7      ,
     9                 1      ,2      ,4      ,5      ,
     O                 2      ,4      ,5      ,7      ,
     1                 2      ,5      ,6      ,7      ,
     2                 4      ,5      ,7      ,8      /
C
C     READ DATA, 45 WORDS PER ELEMENT.
C
   10 CALL READ (*480,*470,FILE,ELID,45,0,FLAG)
C
C     CALCULATE AVERAGE ELEMENT TEMPERATURE
C
      NP    = NPTS(SUB)
      XPTS  = FLOAT(NP)
      IF (SUB .GT. 9) XPTS = XPTS*2.0
C
      TEMP  = 0.0
      DO 20 I = 1,NP
      LTEMP = SIL(I)
      TEMP  = TEMP + UNI(LTEMP)
      IF (SUB .LE. 9) GO TO 20
      IF (SIL(I+4) .EQ. 0) GO TO 20
      LTEMP = SIL(I+4)
      TEMP  = TEMP + UNI(LTEMP)
   20 CONTINUE
      TEMP  = TEMP/XPTS
C
C     SET UP CALL TO MATERIAL SUBROUTINE
C
      INFLAG = 1
      IF (SUB.GE.2 .AND. SUB.LE.5) INFLAG = 2
      IF (SUB.GE.6 .AND. SUB.LE.9) INFLAG = 3
      SINTH = 0.0
      COSTH = 1.0
      IF (THETA.EQ.0.0 .OR. INFLAG.NE.2) GO TO 30
      SINTH = SIN(THETA)
      COSTH = COS(THETA)
   30 MATID = IMAT
      CALL HMAT (ELID)
C
C     SUBTRACT  CONDUCTIVITY AT INITIAL TEMPERATURE AND PLACE IN  MATRIX
C
      IF (INFLAG .EQ. 2) GO TO 40
      IF (INFLAG .EQ. 3) GO TO 50
      K(1) = MATOUT(1) - MATO(1)
      GO TO 60
   40 K(1) = MATOUT(1) - MATO(1)
      K(2) = MATOUT(2) - MATO(2)
      K(3) = K(2)
      K(4) = MATOUT(3) - MATO(3)
      GO TO 60
   50 K(1) = MATOUT(1) - MATO(1)
      K(2) = MATOUT(2) - MATO(2)
      K(3) = MATOUT(3) - MATO(3)
      K(4) = K(2)
      K(5) = MATOUT(4) - MATO(4)
      K(6) = MATOUT(5) - MATO(5)
      K(7) = K(3)
      K(8) = K(6)
      K(9) = MATOUT(6) - MATO(6)
   60 CONTINUE
      IP(1) = 1
      IP(2) = 2
      IP(3) = 3
      IF (SUB.NE.3 .AND. SUB.NE.5) GO TO 100
C
C     MOVE  QUADS TO ELEMENT COORDINATES
C
      DO 95 J = 1,2
      L  = 1
      M  = 2
      I1 = 1
      I2 = 2
      I3 = 3
      I4 = 4
      IF (J .EQ. 1) GO TO 65
      L  = 3
      M  = 4
      I1 = 3
      I2 = 4
      I3 = 1
      I4 = 2
   65 CONTINUE
      DO 70 I = 1,3
      DR(I,1) = R(I,I2) - R(I,I1)
      DR(I,3) = R(I,I3) - R(I,I1)
      DR(I,2) = R(I,I4) - R(I,I2)
   70 CONTINUE
      CALL DAXB (DR(1,3),DR(1,2),DR(1,4))
C
      AREA = DSQRT(DR(1,4)**2 + DR(2,4)**2 + DR(3,4)**2)
C
      DO 80 I = 1,3
   80 DR(I,4) = DR(I,4)/AREA
      EL = DR(1,4)*DR(1,1) + DR(2,4)*DR(2,1) + DR(3,4)*DR(3,1)
      DO 81 I = 1,3
   81 DR(I,1) = DR(I,1) - EL*DR(I,4)
      EL = DSQRT(DR(1,1)**2 + DR(2,1)**2 + DR(3,1)**2)
      DO 82 I = 1,3
   82 DR(I,1) = DR(I,1)/EL
C
      CALL DAXB (DR(1,4),DR(1,1),DR(1,2))
      DO 90 I = 1,3
   90 DR(I,4) = R(I,I4) - R(I,I1)
      CALL GMMATD (DR(1,1),2,3,0, DR(1,3),2,3,1, KQ)
      DRT(L,3) = KQ(1)
      DRT(L,4) = KQ(2)
      DRT(M,3) = KQ(3)
      DRT(M,4) = KQ(4)
      DRT(L,1) = 0.0D0
      DRT(L,2) = EL
      DRT(M,1) = 0.0D0
      DRT(M,2) = 0.0D0
   95 CONTINUE
      GO TO 120
  100 IF (SUB.NE.2 .AND .SUB.NE.4) GO TO 120
C
C     MOVE  TRIANGLES TO ELEMENT COORDINATES
C
      DO 110 I = 1,3
      DR(I,1) = R(I,2) - R(I,1)
  110 DR(I,2) = R(I,3) - R(I,1)
C
      EL   = DR(1,1)**2 + DR(2,1)**2 + DR(3,1)**2
      EL   = DSQRT(EL)
      AREA = DADOTB( DR(1,1),DR(1,2))/EL
      CALL DAXB (DR(1,1), DR(1,2), DR(1,3))
      DR(2,3) = DSQRT(DR(1,3)**2 + DR(2,3)**2 + DR(3,3)**2)/EL
      DR(1,3) = AREA
      DR(1,1) = 0.0D0
      DR(1,2) = EL
      DR(2,1) = 0.0D0
      DR(2,2) = 0.0D0
  120 CONTINUE
C
C     LOOP  ON  SUBELEMENTS  (ONE FOR MOST)
C
      NEL = NELS(SUB)
      DO 460 IEL = 1,NEL
C
      GO TO (130,160,160,140,140, 200,220,240,240,330,330,330,
     1       330,330,330), SUB
C
C     RODS,BARS, ETC.
C
  130 C(1) = 1.0D0
      C(2) =-1.0D0
      EL = (R(1,2)-R(1,1))**2 + (R(2,2)-R(2,1))**2 + (R(3,2)-R(3,1))**2
      EL = DSQRT(EL)
      KQ(1) = AF*K(1)/EL
      IP(1) = 1
      IP(2) = 2
      NP = 2
      NQ = 1
      GO TO 300
C
C     RING ELEMENTS, TRIANGLES AND QUADRILATERALS
C
  140 RBAR = 0.0
      DO 150 I = 1,3
      IG = I + IEL - 1
      IF (IG .GT. 4) IG = IG - 4
      RBAR  = RBAR + R(1,IG)
  150 IP(I) = IG
      AF = RBAR/3.0*PI
      IF (SUB .EQ. 5) GO TO 160
      I1 = IP(1)
      I2 = IP(2)
      I3 = IP(3)
      GO TO 180
  160 J  = 1
      I1 = 1
      I2 = 2
      I3 = 3
      IF (IEL.EQ.2 .OR. IEL.EQ.4) I3 = 4
      IP(1) = 1
      IP(2) = 2
      IP(3) = 3
      IF (IEL .EQ. 1) GO TO 165
      IP(3) = 4
      IF (IEL .EQ. 2) GO TO 165
      J     = 3
      IP(1) = 3
      IP(2) = 4
      IP(3) = 1
      IF (IEL .EQ. 3) GO TO 165
      IP(3) = 2
  165 DO 170 I = 1,4
      DR(1,I) = DRT(J,I)
      DR(2,I) = DRT(J+1,I)
  170 CONTINUE
  180 CONTINUE
      AREA = DR(1,I1)*(DR(2,I2) - DR(2,I3))
     1     + DR(1,I2)*(DR(2,I3) - DR(2,I1))
     2     + DR(1,I3)*(DR(2,I1) - DR(2,I2))
C
      C(1) = (DR(2,I2) - DR(2,I3))/AREA
      C(2) = (DR(2,I3) - DR(2,I1))/AREA
      C(3) = (DR(2,I1) - DR(2,I2))/AREA
C
      C(4) = (DR(1,I3) - DR(1,I2))/AREA
      C(5) = (DR(1,I1) - DR(1,I3))/AREA
      C(6) = (DR(1,I2) - DR(1,I1))/AREA
C
      IF (SUB.EQ.3 .OR. SUB.EQ.5) AREA = AREA/2.0D0
      DO 190 I = 1,4
  190 KQ(I) = K(I)*AREA*AF/2.0D0
C
      NP = 3
      NQ = 2
      GO TO 300
C
C     SOLID ELEMENTS
C
  200 DO 210 I = 1,4
  210 IP(I) = I
      GO TO 260
C
C     WEDGE
C
  220 LROW = 4*IEL - 4
      DO 230 I = 1,4
      I1 = LROW + I
  230 IP(I) = SMAP(I1)
      GO TO 260
C
C     HEXA1 AND HEXA2 ELEMENTS
C
  240 LROW = 4*IEL + 8
      DO 250 I = 1,4
      I1 = LROW +I
  250 IP(I) = SMAP(I1)
      I1 = IP(1)
  260 DO 270 I = 1,3
      IG = IP(I+1)
      DO 270 J = 1,3
      DR(J,I) = R(J,IG) - R(J,I1)
  270 CONTINUE
C
C     COMPUTE INVERSE AND BRING ALONG THE DETERMINANT FROM INVERD.
C
      ISING = 0
      CALL INVERD (3, DR(1,1),3,C(1), 0, DETERM,ISING,C(5))
      DO 280 I = 1,3
      IG = 4*I - 4
      C(IG+1) = 0.0D0
      DO 280 J = 2,4
      I1 = IG + J
      C(I1  ) = DR(J-1,I)
      C(IG+1) = C(IG+1) - C(I1)
  280 CONTINUE
      FACT = DETERM/6.0D0
      IF (SUB .EQ. 9)  FACT = FACT/2.0D0
      DO 290 I = 1,9
  290 KQ(I) = K(I)*FACT
      NP = 4
      NQ = 3
C
C     PERFORM  MATRIX MULTPLIES FOR EACH SUBELEMENT
C                               T
C                       DP  =  C  K  C * T1
C
  300 DO 310 I = 1,NP
      IG = IP(I)
      LTEMP = SIL(IG)
      T1(I) = UNI(LTEMP)
      SINDX(I) = SIL(IG)
  310 CONTINUE
      CALL GMMATD (C,NQ,NP,0, T1,NP,1,0, DRTEMP)
      CALL GMMATD (KQ,NQ,NQ,0, DRTEMP,NQ,1,0, DRTEMP(1,3))
      CALL GMMATD (C,NQ,NP,1, DRTEMP(1,3),NQ,1,0, KQ(1))
      DO 320 I = 1,NP
      IG = SINDX(I)
      DELTA(IG)  = DELTA(IG) + KQ(I)
  320 CONTINUE
      GO TO 460
C
C     BOUNDARY HEAT CONVECTION ELEMENTS
C
  330 ITYPE = SUB - 9
      IF (ITYPE .GT. 7) GO TO 10
      GO TO (340,350,370,380,380,350,350), ITYPE
  340 NP = 1
      KQ(1) = AF*K(1)
      GO TO 410
  350 NP = 2
      EL = (R(1,2)-R(1,1))**2 + (R(2,2)-R(2,1))**2 + (R(3,2)-R(3,1))**2
      KQ(1) = AF*K(1)*DSQRT(EL)/3.0D0
      KQ(2) = KQ(1)/2.0D0
      KQ(3) = KQ(2)
      KQ(4) = KQ(1)
      GO TO 410
C
C     RING SURFACE
C
  370 EL    = ((R(1,2)-R(1,1))**2 + (R(3,2)-R(3,1))**2)
      C(1)  = PI*K(1)*DSQRT(EL)/6.0D0
      KQ(1) = C(1)*(3.0D0*R(1,1) + R(1,2))
      KQ(2) = C(1)*(      R(1,1) + R(1,2))
      KQ(3) = KQ(2)
      KQ(4) = C(1)*(      R(1,1) + 3.0D0*R(1,2))
      NP= 2
      GO TO 410
C
C     TRIANGLES  (ALSO FOR SUBELEMENT OF QUAD)
C
  380 DO 390 I = 1,3
      IG = I + IEL - 1
      IF (IG .GT. 4) IG = IG - 4
      IP(I) = IG
  390 CONTINUE
      I1 = IP(1)
      I2 = IP(2)
      I3 = IP(3)
      DO 400 I = 1,3
      DR(I,1) = R(I,I2) - R(I,I1)
  400 DR(I,2) = R(I,I3) - R(I,I1)
      CALL DAXB (DR(I,1),DR(I,2),DR(I,3))
      AREA = DSQRT(DR(1,3)**2 + DR(2,3)**2 + DR(3,3)**2)/12.0D0
      IF (ITYPE .EQ. 5) AREA = AREA/2.0D0
      KQ(1) = AREA*K(1)
      KQ(2) = KQ(1)/2.0D0
      KQ(3) = KQ(2)
      KQ(4) = KQ(2)
      KQ(5) = KQ(1)
      KQ(6) = KQ(2)
      KQ(7) = KQ(2)
      KQ(8) = KQ(2)
      KQ(9) = KQ(1)
      NP    = 3
C
C     PERFORM MATRIX MULTIPLY, FIRST GET TEMPERATURE VECTOR
C
  410 DO 430 I = 1,NP
      IG = IP(I)
      LTEMP  = SIL(IG)
      T1(I)  = UNI(LTEMP)
      IF (SIL(IG+4) .NE. 0) GO TO 420
      T1(I+4) = 0.0D0
      GO TO 430
  420 LTEMP   = SIL(IG+4)
      T1(I+4) = UNI(LTEMP)
  430 CONTINUE
      CALL GMMATD (KQ(1),NP,NP,0, T1(1),NP,1,0, C)
      CALL GMMATD (KQ(1),NP,NP,0, T1(5),NP,1,0, C(5))
      DO 450 I = 1,NP
      IG  = IP(I)
      IPG = SIL(IG)
      DELTA(IPG) = DELTA(IPG) + C(I) - C(I+4)
      IG = IG + 4
      IF (SIL(IG)) 450,450,440
  440 IPG = SIL(IG)
      DELTA(IPG) = DELTA(IPG) + C(I+4) - C(I)
  450 CONTINUE
C
  460 CONTINUE
      GO TO 10
  470 RETURN
C
  480 J = -2
      CALL MESAGE (FILE,J,SUBR)
      RETURN
      END
