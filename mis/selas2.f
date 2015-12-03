      SUBROUTINE SELAS2
C*****
C THIS ROUTINE IS PHASE II OF STRESS DATA RECOVERY FOR THE SCALAR SPRING
C ELEMENTS ELAS1, ELAS2, ELAS3 AND ELAS4.
C*****
C
C
C
C
C SDR2 VARIABLE CORE
C
      COMMON   /ZZZZZZ/  ZZ(1)
C
C BLOCK FOR POINTERS, LOADING TEMPERATURE AND ELEMENT DEFORMATION.
C
      COMMON   /SDR2X4/
     1                   DUMMY(33)          ,ICSTM
     2,                  NCSTM              ,IVEC
     3,                  IVECN              ,TEMPLD
     4,                  ELDEFM
C
C SDR2 INPUT AND OUTPUT BLOCK
C
      COMMON   /SDR2X7/
     1                   JELID              ,ISILNO(2)
     2,                  STIFF              ,SCOEFF
     3,                  XXXXXX(95)
     4,                  JSELID             ,STRESS
     5,                  YYYYYY(98)
     6,                  JFELID             ,FORCE
     7,                  ZZZZZZ(23)
      EQUIVALENCE
     1                   (SCOEFF,ICOEFF)
C
C
C
      IDISP = IVEC - 1
      DISP1 = 0.0
      DISP2 = 0.0
      IF (ISILNO(1) .LE. 0) GO TO 10
      IU = IDISP + ISILNO(1)
      DISP1 = ZZ(IU)
   10 IF (ISILNO(2) .LE. 0) GO TO 20
      IU = IDISP + ISILNO(2)
      DISP2 = ZZ(IU)
   20 JFELID = JELID
      FORCE = STIFF * (DISP1 - DISP2)
      IF (ICOEFF .EQ. (-1)) RETURN
      STRESS = SCOEFF * FORCE
      JSELID = JELID
      RETURN
      END
