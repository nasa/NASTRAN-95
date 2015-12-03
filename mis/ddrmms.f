      SUBROUTINE DDRMMS (BUF,ELTYPE,BUF4,BUF6)
C
      EXTERNAL ANDF
      INTEGER  ANDF   ,BUF4  ,BUF6  ,DIT   ,ELM(4),ELT   ,BUFA(100)    ,
     1         EST    ,IELID ,IELTMP,INT1  ,Z     ,FILE  ,ELTYPE,MATFLG,
     2         MATID  ,MPT   ,MTD(4),N     ,NELT  ,NWORDS,N1MAT ,N2MAT ,
     3         TMP(4),WRD(4)
      REAL     BUF(16),ELTEMP,STRESS,SINTH ,COTH  ,E     ,G     ,NU    ,
     1         RHO    ,ALPHA ,T0    ,GSUBE ,SIGT  ,SIGC  ,SIGS  ,FINT1 ,
     2         TEMP   ,CPRIM
      COMMON /MATIN /  MATID ,MATFLG,ELTEMP,STRESS,SINTH ,COTH
      COMMON /MATOUT/  E,G   ,NU,RHO,ALPHA ,T0    ,GSUBE ,SIGT   ,SIGC ,
     1                 SIGS
      COMMON /ZZZZZZ/  Z(1)
      COMMON /SYSTEM/  ISYS(61)
      EQUIVALENCE     (INT1,FINT1) ,(IELTMP,ELTEMP)
      DATA    INT1/1/ ,EST/109/ ,MPT/110/ ,DIT/111/ ,ELM/1,3,10,34  / ,
     1        MTD   / 4,4,4,16/ ,TMP/ 17,16,17,42 / ,WRD/17,16,17,42/
C
C
      DO 210 I = 1,4
      IF (ELM(I) .EQ. ELTYPE) GO TO 215
  210 CONTINUE
      GO TO 230
  215 NELT = I
      CALL OPEN (*240,EST,Z(BUF4),0)
  220 CALL FWDREC (*250,EST)
      CALL FREAD (EST,ELT,1,0)
      IF (ELT .NE. ELTYPE) GO TO 220
      NWORDS = WRD(NELT)
      CALL FREAD (EST,BUFA,NWORDS,0)
      CALL CLOSE (EST,1)
      N1MAT  = BUF4 - BUF6
      CALL PREMAT (Z(BUF6),Z(BUF6),Z(BUF4),N1MAT,N2MAT,MPT,DIT)
      MATFLG = 1
      ITEMP  = TMP(NELT)
      IMATID = MTD(NELT)
      IELTMP = BUFA(ITEMP)
      MATID  = BUFA(IMATID)
      IELID  = BUFA(1)
      CALL MAT (IELID)
  230 CONTINUE
      IF (ELTYPE) 200,200,10
   10 IF (ELTYPE .GT. 100) GO TO 200
C              ROD       BEAM      TUBE      SHEAR     TWIST
      GO TO (  21       ,1        ,21       ,1        ,1
C              TRIA1     TRBSC     TRPLT     TRMEM     CONROD
     1        ,20       ,20       ,20       ,40       ,21
C              ELAS1     ELAS2     ELAS3     ELAS4     QDPLT
     2        ,1        ,1        ,1        ,1        ,20
C              QDMEM     TRIA2     QUAD2     QUAD1     DAMP1
     3        ,40       ,20       ,20       ,20       ,1
C              DAMP2     DAMP3     DAMP4     VISC      MASS1
     4        ,1        ,1        ,1        ,1        ,1
C              MASS2     MASS3     MASS4     CONM1     CONM2
     5        ,1        ,1        ,1        ,1        ,1
C              PLOTEL    REACT     QUAD3     BAR       CONE
     6        ,1        ,1        ,1        ,22       ,1
C              TRIARG    TRAPRG    TORDRG    TETRA     WEDGE
     7        ,1        ,1        ,1        ,150      ,150
C              HEXA1     HEXA2     FLUID2    FLUID3    FLUID4
     8        ,150      ,150      ,1        ,1        ,1
C              FLMASS    AXIF2     AXIF3     AXIF4     SLOT3
     9        ,1        ,1        ,1        ,1        ,1
C              SLOT4     HBDY      DUM1      DUM2      DUM3
     A        ,1        ,1        ,1        ,1        ,1
C              DUM4      DUM5      DUM6      DUM7      DUM8
     B        ,1        ,1        ,1        ,1        ,1
C              DUM9      QDMEM1    QDMEM2    QUAD4     IHEX1
     C        ,1        ,40       ,40       ,20       ,1
C              IHEX2     IHEX3     QUADTS    TRIATS    TRIAAX
     D        ,1        ,1        ,1        ,1        ,1
C              TRAPAX    AERO1     TRIM6     TRPLT1    TRSHL
     E        ,1        ,1        ,1        ,1        ,1
C              FHEX1     FHEX2     FTETRA    FWEDGE    IS2D8
     F        ,1        ,1        ,1        ,1        ,40
C              ELBOW     FTUBE     TRIA3     -----     -----
     G        ,22       ,1        ,20       ,1        ,1
C              -----     -----     -----     -----     -----
     H        ,1        ,1        ,1        ,1        ,1
C              -----     -----     -----     -----     -----
     I        ,1        ,1        ,1        ,1        ,1
C              -----     -----     -----     -----     -----
     J        ,1        ,1        ,1        ,1        ,1    ), ELTYPE
C
C     ROD  CONROD  TUBE
C
   21 BUF(3) = FINT1
      BUF(5) = FINT1
C
C     M. S. IN TENSION OR COMPRESSION
C
      IF (BUF(2) .GE. 0.0) GO TO 300
      IF (SIGC   .EQ. 0.0) GO TO 301
      BUF(3) = (-ABS(SIGC)/BUF(2))-1.0
      GO TO 301
  300 IF (SIGT.LE.0.0 .OR. BUF(2).EQ.0.0) GO TO 301
      BUF(3) = SIGT/BUF(2)-1.0
C
C     M. S. IN TORSION
C
  301 IF (BUF(4).EQ.0.0 .OR. SIGS.LE.0.0) GO TO 200
      BUF(3) = SIGS/ABS(BUF(4))-1.0
      GO TO 200
C
C     BAR  ELBOW
C
   22 BUF( 7) = BUF(6) + AMAX1(BUF(2),BUF(3),BUF(4),BUF(5))
      BUF( 8) = BUF(6) + AMIN1(BUF(2),BUF(3),BUF(4),BUF(5))
      BUF( 9) = FINT1
      BUF(14) = BUF(6) + AMAX1(BUF(10),BUF(11),BUF(12),BUF(13))
      BUF(15) = BUF(6) + AMIN1(BUF(10),BUF(11),BUF(12),BUF(13))
      BUF(16) = FINT1
C
C     M. S. IN TENSION
C
      IF (SIGT .LE. 0.0) GO TO 302
      TEMP = BUF(7)
      IF (BUF(7) .LT. BUF(14)) TEMP = BUF(14)
      IF (TEMP   .LE. 0.0) GO TO 302
      BUF(9) = SIGT/TEMP-1.0
C
C     M. S. IN COMPRESSION
C
  302 IF (SIGC .EQ. 0.0) GO TO 200
      TEMP = BUF(8)
      IF (BUF(8) .GT. BUF(15)) TEMP = BUF(15)
      IF (TEMP   .GE. 0.0) GO TO 200
      CPRIM   =-ABS(SIGC)
      BUF(16) = CPRIM/TEMP - 1.0
      GO TO 200
C
C     TRIA1  TRIA2  TRIA3  QUAD1  QUAD2  QUAD4  TRBSC  TRPLT  QDPLT
C
   20 I = 2
      ASSIGN 30 TO IRETRN
      GO TO 100
   30 I = 10
      ASSIGN 200 TO IRETRN
      GO TO 100
C
C     TRMEM  QDMEM  QDMEM1  QDMEM2  IS2D8
C
   40 I = 1
      ASSIGN 200 TO IRETRN
      GO TO 100
C
C     PRINCIPAL STRESS EQUATIONS FOR 2-DIMENSIONAL ELEMENTS
C
  100 TEMP     = BUF(I+1) - BUF(I+2)
      BUF(I+7) = SQRT((TEMP/2.0)**2 + BUF(I+3)**2)
      DELTA    = (BUF(I+1) + BUF(I+2)) / 2.0
      BUF(I+5) = DELTA + BUF(I+7)
      BUF(I+6) = DELTA - BUF(I+7)
C
      IF (ANDF(ISYS(61),1)) 120,120,110
  110 BUF(I+7) = SQRT(BUF(I+1)**2 + BUF(I+2)**2 - BUF(I+1)*BUF(I+2)
     1         +  3.0*BUF(I+3)**2)
C
  120 DELTA = 2.0*BUF(I+3)
      IF (ABS(DELTA).LT.1.0E-15 .AND. ABS(TEMP).LT.1.0E-15) GO TO 121
      BUF(I+4) = ATAN2(DELTA,TEMP)*28.6478898
      GO TO IRETRN, (30,200)
C
  121 BUF(I+4) = 0.0
      GO TO IRETRN, (30,200)
C
C     TETRA  WEDGE  HEXA1  HEXA2
C
  150 BUF(8) = SQRT(BUF(2)*(BUF(2)-BUF(3)-BUF(4))*2.0
     1       +  2.0*BUF(3)*(BUF(3)-BUF(4)) + 2.0*BUF(4)**2
     2       +  6.0*(BUF(5)**2 + BUF(6)**2 + BUF(7)**2)) / 3.0
      GO TO 200
C
    1 CONTINUE
  200 RETURN
C
C     ERROR PROCESSING FOR DDRMMS
C
  240 N = -1
      FILE = EST
      GO TO 260
  250 N = -2
      FILE = EST
  260 CALL MESAGE (N,FILE,NAM)
      RETURN
      END
