      SUBROUTINE TSHEAR
C
C     ELEMENT TEMPERATURE AND DEFORMATION LOADING FOR THE SHEAR PANEL.
C
C     FORMULATION IS THAT OF A PSEUDO-ROD ON EACH EDGE OF THE SHEAR
C     PANEL.
C
C     ECPT( 1)         - ELEMENT ID
C     ECPT( 2 THRU 5)  - 4 GRID SILS
C     ECPT( 6)         - MATERIAL ID
C     ECPT( 7)         - THICKNESS
C     ECPT( 8)         - NON-STRUCTURAL MASS
C     ECPT( 9 THRU 24) - 4 POINTS (CSID,X,Y,Z) REPEATS
C     ECPT(25)         - ELEMENT TEMPERATURE
C     ECPT(26)         - F1 EFFECTIVENESS FACTOR DIRECTION 1, (NOT USED)
C     ECPT(27)         - F2 EFFECTIVENESS FACTOR DIRECTION 2, (NOT USED)
C
      INTEGER         NCSID(4,4)
      COMMON /SSGETT/ ELTYPE   ,OLDEL    ,EORFLG   ,ENDID   ,BUFFLG   ,
     1                ITEMP    ,IDEFT    ,IDEFM
      COMMON /TRIMEX/ ECPT(1)  ,ISILS(4) ,MID      ,THICK   ,FMU      ,
     1                CSID(4,4),ELTEMP   ,F12(2)
      COMMON /MATIN / MATID    ,INFLAG   ,TEMP     ,STRESS  ,SINTH    ,
     1                COSTH
      COMMON /MATOUT/ E1       ,G        ,NU       ,RHO     ,ALPHA    ,
     1                TO1      ,GE       ,SIGMAT   ,SIGMAC  ,SIGMAS
      COMMON /SSGWRK/ VEC(3,4) ,XL(4)    ,DIAG1(3) ,DIAG2(3),TI(16)   ,
     1                PA       ,TSQ      ,VECA(3)  ,VECB(3) ,AREA     ,
     2                VMAG     ,I        ,J        ,IA      ,IB       ,
     3                I12      ,TBAR     ,IN
      COMMON /ZZZZZZ/ PG(1)
      EQUIVALENCE     (NCSID,CSID)
C
      F12(1) = 1.00
      F12(2) = 1.00
C
      IF (F12(1).EQ.0.0 .AND. F12(2).EQ.0.0) RETURN
C
C     MATERIAL DATA ACQUISITION
C
      INFLAG = 1
      MATID  = MID
      TEMP   = ELTEMP
      CALL MAT (ECPT(1))
C
C     GRID POINT TEMPERATURES
C
      IF (ITEMP) 100,800,100
  100 CALL SSGETD (ECPT(1),TI,4)
C
C     ELEMENT DEFORMATION (NOT USED)
C
C     4 NORMALIZED EDGE VECTORS AND LENGTHS
C
      DO 300 I = 1,4
      IGRID2 = I + 1
      IF (I .EQ. 4) IGRID2 = 1
C
      DO 210 J = 1,3
      VEC(J,I) = CSID(J+1,I) - CSID(J+1,IGRID2)
  210 CONTINUE
C
      CALL NORM (VEC(1,I),XL(I))
  300 CONTINUE
C
      IF (F12(1).GT.1.01 .AND. F12(2).GT.1.01) GO TO 500
C
C     PROJECTED AREA IS NEEDED. FIRST OBTAIN THE DIAGONAL VECTORS.
C
      DO 400 I = 1,3
      DIAG1(I) = CSID(I+1,3) - CSID(I+1,1)
      DIAG2(I) = CSID(I+1,4) - CSID(I+1,2)
  400 CONTINUE
C
C     NORMAL VECTOR (DIAG1 X DIAG2)
C
      CALL SAXB (DIAG1,DIAG2,DIAG2)
      CALL NORM (DIAG2,VKL)
      PA = 0.5*VKL
C
C     LOOP THROUGH LOADS ON 4 EDGES.
C
  500 TSQ = THICK*THICK
      DO 700 I = 1,4
      I12 = MOD(I,2)
      IF (I12 .EQ. 0) I12 = 2
      IA = I
      IB = IA + 1
      IF (I .EQ. 4) IB = 1
C
C     TEMPERATURE
C
      TBAR = (TI(IA+1) + TI(IB+1))/2.0 - TO1
C
C     EXTENSIONAL AREA
C
      IF (F12(I12) .LE. 1.01) GO TO 550
      AREA = 0.50*F12(I12)*TSQ
      GO TO 560
  550 AREA = F12(I12)*PA*THICK/(XL(I12) + XL(I12+2))
C
  560 VMAG = E1*AREA*ALPHA*TBAR
      DO 570 J = 1,3
      VECA(J) = VMAG*VEC(J,I)
      VECB(J) =-VECA(J)
  570 CONTINUE
C
      IF (NCSID(1,IB)) 580,590,580
  580 CALL BASGLB (VECB(1),VECB(1),ISILS(IB),CSID(1,IB))
  590 IN = ISILS(IB) - 1
      DO 610 J = 1,3
      IN = IN + 1
      PG(IN) = PG(IN) + VECB(J)
  610 CONTINUE
C
      IF (NCSID(1,IA)) 620,630,620
  620 CALL BASGLB (VECA(1),VECA(1),ISILS(IA),CSID(1,IA))
  630 IN = ISILS(IA) - 1
      DO 640 J = 1,3
      IN = IN + 1
      PG(IN) = PG(IN) + VECA(J)
  640 CONTINUE
C
  700 CONTINUE
  800 RETURN
      END
