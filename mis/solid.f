      SUBROUTINE SOLID( TEMPS, PG, ITYPE )
C*****
C  ELEMENT THERMAL LOAD GENERATOR FOR THE WEDGE, HEXA1, AND HEXA2
C
C  ITYPE = 1 IMPLIES WEDGE - 3 TETRAHEDRONS
C
C  ITYPE = 2 IMPLIES HEXA(6-SIDED-SOLID) 5 TETRAHEDRONS
C
C  ITYPE = 3 IMPLIES HEXA(6-SIDED-SOLID) 10 TETRAHEDRONS
C
C*****
      INTEGER       NECPT(52)           ,M(13,4)
C
      REAL          TEMPS(8)           ,PG(6)              ,TMPS(4)
C
      COMMON/TRIMEX/ECPT(100)
C
      EQUIVALENCE( NECPT(1), ECPT(1) )
C*****
C
C  E C P T     TETRA          WEDGE          HEXA
C  -----------------------------------------------
C  ECPT( 1) =  EL ID          EL ID          EL ID
C  ECPT( 2) =  MAT-ID         MAT-ID         MAT-ID
C  ECPT( 3) =  GRID-1         GRID-1         GRID-1
C  ECPT( 4) =  GRID-2         GRID-2         GRID-2
C  ECPT( 5) =  GRID-3         GRID-3         GRID-3
C  ECPT( 6) =  GRID-4         GRID-4         GRID-4
C  ECPT( 7) =  CSID-1         GRID-5         GRID-5
C  ECPT( 8) =  X1             GRID-6         GRID-6
C  ECPT( 9) =  Y1             CSID-1         GRID-7
C  ECPT(10) =  Z1             X1             GRID-8
C  ECPT(11) =  CSID-2         Y1             CSID-1
C  ECPT(12) =  X2             Z1             X1
C  ECPT(13) =  Y2             CSID-2         Y1
C  ECPT(14) =  Z2             X2             Z1
C  ECPT(15) =  CSID-3         Y2             CSID-2
C  ECPT(16) =  X3             Z2             X2
C  ECPT(17) =  Y3             CSID-3         Y2
C  ECPT(18) =  Z3             X3             Z2
C  ECPT(19) =  CSID-4         Y3             CSID-3
C  ECPT(20) =  X4             Z3             X3
C  ECPT(21) =  Y4             CSID-4         Y3
C  ECPT(22) =  Z4             X4             Z3
C  ECPT(23) =  EL-TEM         Y4             CSID-4
C  ECPT(24)                   Z4             X4
C  ECPT(25)                   CSID-5         Y4
C  ECPT(26)                   X5             Z4
C  ECPT(27)                   Y5             CSID-5
C  ECPT(28)                   Z5             X5
C  ECPT(29)                   CSID-6         Y5
C  ECPT(30)                   X6             Z5
C  ECPT(31)                   Y6             CSID-6
C  ECPT(32)                   Z6             X6
C  ECPT(33)                   ELTEMP         Y6
C  ECPT(34)                                  Z6
C  ECPT(35)                                  CSID-7
C  ECPT(36)                                  X7
C  ECPT(37)                                  Y7
C  ECPT(38)
C  ECPT(39)                                  CSID-8
C  ECPT(40)                                  X8
C  ECPT(41)                                  Y8
C  ECPT(42)                                  Z8
C  ECPT(43)                                  EL-TEMP
C*****
C
C*****
C  MAP FOR WEDGE  M(I,J)  I=TETRAHEDRON, J=GRID POINT
C*****
      DATA M( 1,1),M( 1,2),M( 1,3),M( 1,4) / 1   ,2   ,3   ,6 /
      DATA M( 2,1),M( 2,2),M( 2,3),M( 2,4) / 1   ,2   ,6   ,5 /
      DATA M( 3,1),M( 3,2),M( 3,3),M( 3,4) / 1   ,4   ,5   ,6 /
C*****
C  MAP FOR HEXA-SOLID (5 OR 10 TETRAHEDRONS)
C*****
      DATA M( 4,1),M( 4,2),M( 4,3),M( 4,4) / 1   ,2   ,3   ,6 /
      DATA M( 5,1),M( 5,2),M( 5,3),M( 5,4) / 1   ,3   ,4   ,8 /
      DATA M( 6,1),M( 6,2),M( 6,3),M( 6,4) / 1   ,3   ,8   ,6 /
      DATA M( 7,1),M( 7,2),M( 7,3),M( 7,4) / 1   ,5   ,6   ,8 /
      DATA M( 8,1),M( 8,2),M( 8,3),M( 8,4) / 3   ,6   ,7   ,8 /
      DATA M( 9,1),M( 9,2),M( 9,3),M( 9,4) / 2   ,3   ,4   ,7 /
      DATA M(10,1),M(10,2),M(10,3),M(10,4) / 1   ,2   ,4   ,5 /
      DATA M(11,1),M(11,2),M(11,3),M(11,4) / 2   ,4   ,5   ,7 /
      DATA M(12,1),M(12,2),M(12,3),M(12,4) / 2   ,5   ,6   ,7 /
      DATA M(13,1),M(13,2),M(13,3),M(13,4) / 4   ,5   ,7   ,8 /
C*****
C  BRANCH ON ELEMENT TYPE
C*****
      GO TO(1000,2000,3000), ITYPE
C*****
C  COME HERE FOR WEDGE COMPUTATIONS.
C  KTETRA IS CALLED 3 TIMES BASED ON WEDGE MAPPING MATRIX.
C*****
 1000 ITET = 1
      NTET = 3
      ITEMP= 33
      NGRIDS = 6
      IOPT = 0
      GO TO 6000
C*****
C  COME HERE FOR 5-TETRAHEDRON 6-SIDED SOLID
C*****
 2000 ITET = 4
      NTET = 8
      ITEMP= 43
      NGRIDS = 8
      IOPT = 0
      GO TO 6000
C*****
C  COME HERE FOR 10-TETRAHEDRON 6-SIDED SOLID
C*****
 3000 ITET = 4
      NTET =13
      ITEMP= 43
      NGRIDS = 8
      IOPT = 1
      GO TO 6000
 6000 DO 6010 J = 1,50
      ECPT(J+50) = ECPT(J)
 6010 CONTINUE
C
C  FILL MAT ID AND EL TEMP
C
      NECPT(2) = NECPT(52)
      NECPT(23) = NECPT (ITEMP+50)
      DO 8000 I = ITET,NTET
C
C     FILL IN GRID SIL-S AND COORDINATE SETS
C
      DO 7030 J = 1,4
      KPOINT = M(I,J)
      TMPS(J) = TEMPS(KPOINT)
      NECPT(J+2) = NECPT(KPOINT+52)
      KPOINT = 4*KPOINT + NGRIDS - 3
      JPOINT = 4*J + 2
      NECPT(JPOINT+1) = NECPT(KPOINT+52)
      NECPT(JPOINT+2) = NECPT(KPOINT+53)
      NECPT(JPOINT+3) = NECPT(KPOINT+54)
      NECPT(JPOINT+4) = NECPT(KPOINT+55)
 7030 CONTINUE
      CALL TETRA( TMPS(1), PG(1), IOPT )
 8000 CONTINUE
C*****
C  ALL THROUGH
C*****
      RETURN
      END
