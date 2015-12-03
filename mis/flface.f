      SUBROUTINE FLFACE (TYPE,ECT,ELT,GRID)
C
C     LOCATES THE FLUID GRID POINTS DEFINING THE FACE OF A FLUID
C     ELEMENT.  THE FACE MAY BE SPECIFIED IN TWO MANNERS.
C
C     1) FACE NUMBER - ELT(2) LESS THEN ZERO AND FACE = ELT(3)
C     2) STRUCTURAL ELEMENT WHICH COINCIDES WITH FACE -
C                         ELT(2) = ELEMENT ID AND ELT(3)-ELT(6) = GRIDS
C
      LOGICAL         ERROR
      INTEGER         ECT(10)  ,ELT(7)   ,GF(10)   ,TYPE     ,GRID(4)  ,
     1                GS1      ,GS2      ,GS3      ,GS4      ,GF1      ,
     2                GF2      ,GF3      ,GF4      ,GRIDF(4) ,NFACE(4) ,
     3                FACEID   ,HEX1(4,6),HEX2(4,6),TETRA(4,4)         ,
     4                WEDGE(4,5)         ,FACE(4,6,4)
      REAL            MAG      ,R1(3)    ,R2(3)    ,R3(3)    ,KS(3)    ,
     1                KF(3)    ,CS(3)    ,ANGLE(6) ,Z        ,HEIGTH(6)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /FLBPTR/ ERROR    ,ICORE    ,LCORE    ,IBGPDT   ,NBGPDT
      COMMON /ZZZZZZ/ Z(1)
      COMMON /SYSTEM/ SYSBUF   ,NOUT
      EQUIVALENCE     (HEX1(1,1),FACE(1,1,1)),  (HEX2(1,1),FACE(1,1,2)),
     1                (TETRA(1,1),FACE(1,1,3)), (WEDGE(1,1),FACE(1,1,4))
C
C     DATA DEFINING FACES OF THE FLUID ELEMENTS
C
C     NUMBER OF GRID POINTS PER ELEMENT
C
C                   FHEX1     FHEX2     FTETRA    FWEDGE
C
      DATA    GRIDF / 8        ,8        ,4        ,6       /
C
C     NUMBER OF FACES ON EACH ELEMENT
C
      DATA    NFACE / 6        ,6        ,4        ,5       /
C
C     GRID POINTS WHICH DEFINE FACES FOR FHEX1 ELEMENTS
C
      DATA    HEX1  / 1        ,4        ,3        ,2        ,
     1                1        ,2        ,6        ,5        ,
     2                2        ,3        ,7        ,6        ,
     3                3        ,4        ,8        ,7        ,
     4                4        ,1        ,5        ,8        ,
     5                5        ,6        ,7        ,8        /
C
C     GRID POINTS WHICH DEFINE FACES FOR FHEX2 ELEMENTS
C
      DATA    HEX2  / 1        ,4        ,3        ,2        ,
     1                1        ,2        ,6        ,5        ,
     2                2        ,3        ,7        ,6        ,
     3                3        ,4        ,8        ,7        ,
     4                4        ,1        ,5        ,8        ,
     5                5        ,6        ,7        ,8        /
C
C     GRID POINTS WHICH DEFINE FACES FOR FTETRA ELEMENTS
C
      DATA    TETRA / 1        ,3        ,2        ,-1       ,
     1                1        ,2        ,4        ,-1       ,
     2                2        ,3        ,4        ,-1       ,
     3                3        ,1        ,4        ,-1       /
C
C     GRID POINTS WHICH DEFINE FACES FOR FWEDGE ELEMENTS
C
      DATA    WEDGE / 1        ,3        ,2        ,-1       ,
     1                1        ,2        ,5        ,4        ,
     2                2        ,3        ,6        ,5        ,
     3                3        ,1        ,4        ,6        ,
     4                4        ,5        ,6        ,-1       /
C
C
C     DETERMINE HOW THE FACE IS SPECIFIED
C
C     SUBTRACT IFP CARD NUMBER OF ELEMENT JUST BEFORE CFHEX1 FROM TYPE
      INTYPE = TYPE - 332
C
      NF = NFACE(INTYPE)
      IF (ELT(2) .LT. 0) GO TO 200
C
C     THE FACE IS DEFINED BY STRUCTURAL GRIDS
C
C     INITIALIZE POINTERS TO GRID POINT DATA
C
      NGRIDS = 4
      IF (ELT(6) .LT. 0) NGRIDS = 3
      GS1 = IBGPDT + (ELT(3)-1)*4
      GS2 = IBGPDT + (ELT(4)-1)*4
      GS3 = IBGPDT + (ELT(5)-1)*4
      GS4 = -1
      IF (NGRIDS .EQ. 4) GS4 = IBGPDT + (ELT(6)-1)*4
C
      NGRIDF = GRIDF(INTYPE)
      DO 10 I = 1,NGRIDF
   10 GF(I) = IBGPDT + (ECT(I+2)-1)*4
C
C     FIND NORMAL VECTOR TO STRUCTURAL ELEMENT FACE
C
      DO 20 I = 1,3
      R1(I) = Z(GS2+I) - Z(GS1+I)
   20 R2(I) = Z(GS3+I) - Z(GS1+I)
C
      KS(1) = R1(2)*R2(3) - R1(3)*R2(2)
      KS(2) = R1(3)*R2(1) - R1(1)*R2(3)
      KS(3) = R1(1)*R2(2) - R1(2)*R2(1)
C
      MAG = SQRT(KS(1)**2 + KS(2)**2 + KS(3)**2)
      IF (MAG .LT. 1.0E-7) GO TO 8005
      DO 30 I = 1,3
   30 KS(I) = KS(I)/MAG
C
C     FIND AREA OF STRUCTURE FACE AND TOLERANCE USED IN CHECKING
C     SEPERATIOON
C
      AREA = MAG
      IF (GS4 .LT. 0) AREA = MAG/2.0
      TOL = .2*SQRT(AREA)
C
C     FIND CENTROID OF STRUCTURAL FACE
C
      DO 35 I = 1,3
      CS(I) = Z(GS1+I) + Z(GS2+I) + Z(GS3+I)
      IF (NGRIDS .EQ. 4) CS(I) = CS(I) + Z(GS4+I)
   35 CS(I) = CS(I)/FLOAT(NGRIDS)
C
C     PROCESS EACH FACE OF THE FLUID ELEMENT - FIRST GET GRID POINTERS
C     POINTERS
C
      DO 100 IF = 1,NF
      I   = FACE(1,IF,INTYPE)
      GF1 = GF(I)
      I   = FACE(2,IF,INTYPE)
      GF2 = GF(I)
      I   = FACE(3,IF,INTYPE)
      GF3 = GF(I)
      I   = FACE(4,IF,INTYPE)
      GF4 = -1
      IF (I .GT. 0) GF4 = GF(I)
C
C     FIND NORMAL TO FLUID FACE
C
      DO 40 I = 1,3
      R2(I) = Z(GF2+I) - Z(GF1+I)
   40 R3(I) = Z(GF3+I) - Z(GF1+I)
C
      KF(1) = R2(2)*R3(3) - R2(3)*R3(2)
      KF(2) = R2(3)*R3(1) - R2(1)*R3(3)
      KF(3) = R2(1)*R3(2) - R2(2)*R3(1)
C
      MAG = SQRT(KF(1)**2 + KF(2)**2 + KF(3)**2)
      IF (MAG .LT. 1.0E-7) GO TO 8006
      DO 45 I = 1,3
   45 KF(I) = KF(I)/MAG
C
C     DETERMINE ANGLE BETWEEN FACES
C
      ANGLE(IF) = KF(1)*KS(1) + KF(2)*KS(2) + KF(3)*KS(3)
      IF (ABS(ANGLE(IF)) .LE. .866) GO TO 100
C
C     FIND DISTANCE FROM THE CENTROID OF THE STRUCTURE TO THE FLUID
C     FACE.  THE DISTANCE IS MEASURED ALONG THE NORMAL TO THE
C     FLUID FACE
C
      DO 60 I = 1,3
   60 R2(I) = CS(I) - Z(GF1+I)
C
      HEIGTH(IF) = ABS(KF(1)*R2(1) + KF(2)*R2(2) + KF(3)*R2(3))
C
  100 CONTINUE
C
C     CHOSE THE FACE OF THE FLUID WITH THE SMALLEST DISTANCE TO THE
C     STRUCTURAL ELEMENT AND WITH THE ANGLE BETWEEN THE TWO FACES LESS
C     THEN 30 DEGREES
C
      DIST   = 1.0E+10
      FACEID = 0
      DO 110 IF = 1,NF
      IF (ABS(ANGLE(IF)) .LE. .866) GO TO 110
      IF (HEIGTH(IF) .GE. DIST) GO TO 110
      DIST   = HEIGTH(IF)
      FACEID = IF
  110 CONTINUE
      IF (FACEID .EQ. 0) GO TO 8007
C
C     VERIFY THAT THE FACE IS WITHIN PROPER TOLERENCE
C
      IF (DIST .GT. TOL) GO TO 8008
C
C     IF ANGLE WAS COMPUTED NEGATIVE - SWICTH STRUCTURAL GRIDS AROUND
C     IN ELEMENT TABLE RECORD FOR LATER USE
C
      IF (ANGLE(FACEID) .GE. 0.0) GO TO 300
      IF (NGRIDS .EQ. 3) GO TO 120
      I      = ELT(3)
      ELT(3) = ELT(6)
      ELT(6) = I
      I      = ELT(4)
      ELT(4) = ELT(5)
      ELT(5) = I
      GO TO 300
C
  120 I      = ELT(3)
      ELT(3) = ELT(5)
      ELT(5) = I
      GO TO 300
C
C     THE FACE IS DEFINED BY A FACE ID
C
  200 FACEID = ELT(3)
      IF (FACEID.LT.1 .OR. FACEID.GT.NF) GO TO 8009
C
C     USING THE FACE SPECIFIES OR FOUND - RETURN THE PROPER
C     FLUID GRID POINTS
C
  300 DO 310 I = 1,4
      J = FACE(I,FACEID,INTYPE)
      IF (J .GT. 0) GO TO 305
      GRID(I) = -1
      GO TO 310
  305 GRID(I) = ECT(J+2)
  310 CONTINUE
C
      RETURN
C
C     ERROR CONDITIONS
C
C     BAD GEOMETRY FOR STRUCTURAL ELEMENT
C
 8005 WRITE (NOUT,9005) UFM,ELT(2)
      GO TO 9000
C
C     BAD GEOMETRY FOR FLUID ELEMENT
C
 8006 WRITE (NOUT,9006) UFM,IF,ECT(1)
      GO TO 9000
C
C     NO FACE WITHIN 30 DEGREES FO STRUCTURAL ELEMENT FACE
C
 8007 WRITE (NOUT,9007) UFM,ECT(1),ELT(2)
      GO TO 9000
C
C     FLUID ELEMENT IS NOT WITHIN TOLERENCE RANGE OF STRUCTURAL ELEMENT
C
 8008 WRITE (NOUT,9008) UFM,ECT(1),ELT(2)
      GO TO 9000
C
C     ILLEGAL FACE NUMBER
C
 8009 WRITE (NOUT,9009) UFM,FACEID,ECT(1)
C
 9000 ERROR = .TRUE.
      RETURN
C
C     ERROR FORMATS
C
 9005 FORMAT (A23,' 8005. BAD GEOMETRY DEFINED FOR STRUCTURAL ELEMENT',
     1        I9)
 9006 FORMAT (A23,' 8006. BAD GEOMETRY DEFINED FOR FACE',I9,
     1       ' OF FLUID ELEMENT',I9)
 9007 FORMAT (A23,' 8007. NO FACE ON FLUID ELEMENT',I9,
     1       ' IS WITHIN 30 DEGREES OF STRUCTURAL ELEMENT',I9)
 9008 FORMAT (A23,' 8008. THE DISTANCE BETWEEN FLUID ELEMENT',I9,
     1       ' AND STRUCTURAL ELEMENT',I9, /30X,
     2       'IS GREATER THAN THE ALLOWED TOLERANCE.')
 9009 FORMAT (A23,' 8009. FACE',I9,' SPECIFIED FOR FLUID ELEMENT',I9,
     1       ' IS AN ILLEGAL VALUE.')
      END
