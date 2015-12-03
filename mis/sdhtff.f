      SUBROUTINE SDHTFF
C
C     THIS ROUTINE CALCULATES THE PHASE 1 FLUX-TEMPERATURE RELATIONSHIPS
C
      INTEGER         SUB,NELS(18),IP(32),SMAP(52),STRSPT,SIG
      REAL            C(12),K,KQ(9),DR(3,4),MATO,EL,ZI(3),VEC(3),VVEC(3)
      COMMON /CONDAS/ CONSTS(5)
      COMMON /SDR2X4/ DUMX(109),STRSPT
      COMMON /SDR2X5/ EST(100),IDE,SIG(32),NQ,NSIL,NAME(2),K(9),CE(96),
     1                DSHPB(3,32)
      COMMON /SDR2X6/ SUB,IMAT,AF,THETA,R(3,32)
      COMMON /HMTOUT/ MATO(6)
      EQUIVALENCE     (CONSTS(1),PI)
      DATA    NELS  / 1,1,4,1,4,1,3,5,10,1,1,1,1,4,1,1,1,1 /
      DATA    SMAP  / 1        ,2        ,3        ,6      ,
     1                1        ,2        ,6        ,5      ,
     2                1        ,4        ,5        ,6      ,
     3                1        ,2        ,3        ,6      ,
     4                1        ,3        ,4        ,8      ,
     5                1        ,3        ,8        ,6      ,
     6                1        ,5        ,6        ,8      ,
     7                3        ,6        ,7        ,8      ,
     8                2        ,3        ,4        ,7      ,
     9                1        ,2        ,4        ,5      ,
     O                2        ,4        ,5        ,7      ,
     1                2        ,5        ,6        ,7      ,
     2                4        ,5        ,7        ,8      /
C
      GO TO (30,40,40,40,40,50,50,50,50,30,30,30,30,30,30,50,50,30), SUB
   30 K(1) = MATO(1)
      NQ   = 1
      GO TO  60
   40 K(1) = MATO(1)
      K(2) = MATO(2)
      K(3) = K(2)
      K(4) = MATO(3)
      NQ   = 2
      GO TO  60
   50 K(1) = MATO(1)
      K(2) = MATO(2)
      K(3) = MATO(3)
      K(4) = K(2)
      K(5) = MATO(4)
      K(6) = MATO(5)
      K(7) = K(3)
      K(8) = K(6)
      K(9) = MATO(6)
      NQ   = 3
   60 CONTINUE
      IP(1)= 1
      IP(2)= 2
      IP(3)= 3
      IF (SUB .EQ. 17) GO TO 111
      IF (SUB.NE.3 .AND. SUB.NE.5) GO TO 100
C
C     MOVE  QUADS TO ELEMENT COORDINATES
C     (CQUAD4? APPEARENTLY UP TO ELEMENT TYPE 52 ONLY)
C
      DO 70 I = 1,3
      DR(I,1) = R(I,2) - R(I,1)
      DR(I,3) = R(I,3) - R(I,1)
   70 DR(I,2) = R(I,4) - R(I,2)
      CALL SAXB (DR(1,3),DR(1,2),DR(1,4))
C
      EL   = SQRT(DR(1,1)**2 + DR(2,1)**2 + DR(3,1)**2)
      AREA = SQRT(DR(1,4)**2 + DR(2,4)**2 + DR(3,4)**2)
C
      DO 80 I = 1,3
      DR(I,1) = DR(I,1)/EL
   80 DR(I,4) = DR(I,4)/AREA
C
      CALL SAXB (DR(1,4),DR(1,1),DR(1,2))
      DO 90 I = 1,3
   90 DR(I,4) = R(I,4) - R(I,1)
      CALL GMMATS (DR(1,1),2,3,0,DR(1,3),2,3,1,KQ)
      DR(1,3) = KQ(1)
      DR(1,4) = KQ(2)
      DR(2,3) = KQ(3)
      DR(2,4) = KQ(4)
      DR(1,2) = EL
      DR(1,1) = 0.0
      DR(2,1) = 0.0
      DR(2,2) = 0.0
      GO TO 120
  100 IF (SUB.NE.2 .AND. SUB.NE.4) GO TO 120
C
C     MOVE  TRIANGLES TO ELEMENT COORDINATES
C     (CTRIA3?)
C
      DO 110 I = 1,3
      DR(I,1) = R(I,2) - R(I,1)
  110 DR(I,2) = R(I,3) - R(I,1)
C
      EL   = DR(1,1)**2 + DR(2,1)**2 + DR(3,1)**2
      EL   = SQRT(EL)
      AREA = SADOTB(DR(1,1),DR(1,2))/EL
      CALL SAXB (DR(1,1),DR(1,2),DR(1,3))
      DR(2,3) = SQRT(DR(1,3)**2 + DR(2,3)**2 + DR(3,3)**2)/EL
      DR(1,3) = AREA
      DR(1,1) = 0.0
      DR(1,2) = EL
      DR(2,1) = 0.0
      DR(2,2) = 0.0
      GO TO 120
C
C     IS2D8-CENTROID ONLY-WE NEED TO CONVERT ONLY GRIDS 5-8 TO LOCAL
C     COORDS
C
  111 DO 112 I = 1,3
  112 ZI(I) = R(I,2) - R(I,1)
      ZLEN  = SQRT(ZI(1)**2 + ZI(2)**2 + ZI(3)**2)
      DO 113 I = 1,3
  113 ZI(I) = ZI(I)/ZLEN
      DO 115 I = 5,8
      DO 114 J = 1,3
  114 VEC(J) = R(J,I) - R(J,1)
      DR(1,I-4) = SADOTB(VEC,ZI)
      CALL SAXB (ZI,VEC,VVEC)
      DR(2,I-4) = SQRT(VVEC(1)**2 + VVEC(2)**2 + VVEC(3)**2)
  115 CONTINUE
  120 CONTINUE
C
C     LOOP  ON  SUBELEMENTS  (ONE FOR MOST)
C
      FACT = 0.0
      NEL  = NELS(SUB)
      XELS = FLOAT(NEL)
      DO 460 IEL = 1,NEL
C
      GO TO (130,160,160,140,140,200,220,240,240,330,330,330,
     1       330,330,330,285,291,330), SUB
C
C     RODS,BARS, ETC.
C
  130 EL = 0.0
      DO 135 I = 1, 3
      EL = EL + (R(I,1)-R(I,2))**2
  135 CONTINUE
      EL = SQRT(EL)
      C(1) = -1.0/EL
      C(2) =  1.0/EL
      NP = 2
      GO TO 300
C
C     RING ELEMENTS, TRIANGLES AND QUADRILATERALS
C
  140 AF = 1.0
  160 DO 170 I = 1,3
      IG = I + IEL - 1
      IF (IG .GT. 4) IG = IG - 4
  170 IP(I) = IG
      I1 = IP(1)
      I2 = IP(2)
      I3 = IP(3)
      AREA = DR(1,I1)*(DR(2,I2)-DR(2,I3)) + DR(1,I2)*(DR(2,I3)-DR(2,I1))
     2     + DR(1,I3)*(DR(2,I1)-DR(2,I2))
      C(1) = (DR(2,I2) - DR(2,I3))/AREA
      C(2) = (DR(2,I3) - DR(2,I1))/AREA
      C(3) = (DR(2,I1) - DR(2,I2))/AREA
      C(4) = (DR(1,I3) - DR(1,I2))/AREA
      C(5) = (DR(1,I1) - DR(1,I3))/AREA
      C(6) = (DR(1,I2) - DR(1,I1))/AREA
C
      NP   = 3
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
      I1 = LROW + I
  250 IP(I) = SMAP(I1)
  260 I1 = IP(1)
      DO 270 I = 1,3
      IG = IP(I+1)
      DO 270 J = 1,3
       DR(J,I) = R(J,IG) - R(J,I1)
  270 CONTINUE
C
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
C
      ISING = -1
      CALL INVERS (3,DR,3,C,0,DETERM,ISING,C(4))
      DO 280 I = 1,3
      IG = 4*I - 4
      C(IG+1) = 0.0
      DO 280 J = 2,4
      I1 = IG + J
      C(I1  ) = DR(J-1,I)
      C(IG+1) = C(IG+1) - C(I1)
  280 CONTINUE
      NP = 4
      GO TO 300
C
C     ISOPARAMETRIC SOLIDS
C
  285 IG = 0
      DO 290 I = 1,3
      DO 290 J = 1,NSIL
      IG = IG + 1
  290 CE(IG) = DSHPB(I,J)
      GO TO 460
C
C     IS2D8- SINCE CENTROID ONLY, WE CAN EASILY COMPUTE SHAPE FUNCTIONS
C     DERIVATIVES, JACOBIAN,ETC.. THE FINAL RESULT OF DNDX,DNDY=DNL IS
C     GIVEN
C
  291 X68 = DR(1,2) - DR(1,4)
      X57 = DR(1,1) - DR(1,3)
      Y68 = DR(2,2) - DR(2,4)
      Y57 = DR(2,1) - DR(2,3)
      DENOM = -X68*Y57 + X57*Y68
      DO 292 I = 1,24
  292 CE(I)  = 0.
      CE( 5) = Y68/DENOM
      CE( 6) =-Y57/DENOM
      CE( 7) =-Y68/DENOM
      CE( 8) = Y57/DENOM
      CE(13) =-X68/DENOM
      CE(14) = X57/DENOM
      CE(15) = X68/DENOM
      CE(16) =-X57/DENOM
      GO TO 460
C
C     SUPERIMPOSE C MATRICES ONTO CE MATRICES OF THE WHOLE ELEMENT
C
  300 DO 310 I = 1,NP
      DO 310 J = 1,NQ
      I1 = NP*(J-1) + I
      IG = NSIL*(J-1) + IP(I)
      CE(IG) = CE(IG) + C(I1)/XELS
  310 CONTINUE
      GO TO 460
C
C     BOUNDARY HEAT CONVECTION ELEMENTS
C
  330 ITYPE = SUB - 9
      IF (ITYPE .GT. 7) RETURN
      GO TO (340,350,370,380,380,350,350), ITYPE
  340 NP = 1
      C(1) = 1.0
      FACT = AF*K(1)
      GO TO 410
  350 NP = 2
      C(1) = 0.5
      C(2) = 0.5
      EL = SQRT((R(1,1)-R(1,2))**2 + (R(2,1)-R(2,2))**2 +
     1          (R(3,1)-R(3,2))**2)
      FACT = AF*EL*K(1)
      GO TO 410
C
C     RING SURFACE
C
  370 EL   = ((R(1,2)-R(1,1))**2 + (R(3,2)-R(3,1))**2)
      FACT = 3.0*(R(1,1) + R(1,2))
      C(1) = (2.0*R(1,1) + R(1,2))/FACT
      C(2) = (R(1,1) + 2.0*R(1,2))/FACT
      FACT = (R(1,1) + R(1,2))*PI*SQRT(EL)*K(1)
      NP   = 2
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
      CALL SAXB (DR(1,1),DR(1,2),DR(1,3))
      AREA = (SQRT(DR(1,3)**2 + DR(2,3)**2 +DR(3,3)**2))/2.0
      IF (ITYPE .EQ. 5) AREA = AREA/2.0
      FACT = FACT + AREA*MATO(1)
      C(1) = 1.0/3.0
      C(2) = C(1)
      C(3) = C(1)
      NP   = 3
C
C     SUPERIMPOSE C MATRIX INTO CE MATRIX
C
  410 DO 420 I = 1,NP
      IG = IP(I)
      CE(IG) = CE(IG) + C(I)/XELS
      IG = IP(I) + 4
  420 CE(IG) = CE(IG) - C(I)/XELS
      K(1) = FACT
  460 CONTINUE
      RETURN
      END
