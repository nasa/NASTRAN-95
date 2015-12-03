      SUBROUTINE KSOLID (ITYPE)
C
C     IOPT = 1 IMPLIES WEDGE - 3 TETRAHEDRONS
C     IOPT = 2 IMPLIES HEXA(6-SIDED-SOLID) 5  TETRAHEDRONS
C     IOPT = 3 IMPLIES HEXA(6-SIDED-SOLID) 10 TETRAHEDRONS
C
C     ECPT        TETRA          WEDGE          HEXA
C     ------------------------------------------------
C     ECPT( 1) =  EL ID          EL ID          EL ID
C     ECPT( 2) =  MAT-ID         MAT-ID         MAT-ID
C     ECPT( 3) =  GRID-1         GRID-1         GRID-1
C     ECPT( 4) =  GRID-2         GRID-2         GRID-2
C     ECPT( 5) =  GRID-3         GRID-3         GRID-3
C     ECPT( 6) =  GRID-4         GRID-4         GRID-4
C     ECPT( 7) =  CSID-1         GRID-5         GRID-5
C     ECPT( 8) =  X1             GRID-6         GRID-6
C     ECPT( 9) =  Y1             CSID-1         GRID-7
C     ECPT(10) =  Z1             X1             GRID-8
C     ECPT(11) =  CSID-2         Y1             CSID-1
C     ECPT(12) =  X2             Z1             X1
C     ECPT(13) =  Y2             CSID-2         Y1
C     ECPT(14) =  Z2             X2             Z1
C     ECPT(15) =  CSID-3         Y2             CSID-2
C     ECPT(16) =  X3             Z2             X2
C     ECPT(17) =  Y3             CSID-3         Y2
C     ECPT(18) =  Z3             X3             Z2
C     ECPT(19) =  CSID-4         Y3             CSID-3
C     ECPT(20) =  X4             Z3             X3
C     ECPT(21) =  Y4             CSID-4         Y3
C     ECPT(22) =  Z4             X4             Z3
C     ECPT(23) =  EL-TEM         Y4             CSID-4
C     ECPT(24)                   Z4             X4
C     ECPT(25)                   CSID-5         Y4
C     ECPT(26)                   X5             Z4
C     ECPT(27)                   Y5             CSID-5
C     ECPT(28)                   Z5             X5
C     ECPT(29)                   CSID-6         Y5
C     ECPT(30)                   X6             Z5
C     ECPT(31)                   Y6             CSID-6
C     ECPT(32)                   Z6             X6
C     ECPT(33)                   ELTEMP         Y6
C     ECPT(34)                                  Z6
C     ECPT(35)                                  CSID-7
C     ECPT(36)                                  X7
C     ECPT(37)                                  Y7
C     ECPT(38)
C     ECPT(39)                                  CSID-8
C     ECPT(40)                                  X8
C     ECPT(41)                                  Y8
C     ECPT(42)                                  Z8
C     ECPT(43)                                  EL-TEMP
C
C     MAP FOR WEDGE  M(I,J)  I = TETRAHEDRON, J = GRID POINT
C
      LOGICAL         NOGO
      INTEGER         NECPT(52),OUT,M(22,4)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ SYSBUF,OUT,NOGO
      COMMON /SMA1ET/ ECPT(100)
      COMMON /SMA1CL/ IOPT4,K4GGSW,NPVT,ISKP(19),NOGOO
      COMMON /SMA1DP/ R12(3),R13(3),R(3),RXR(3),R24(3)
      EQUIVALENCE     (NECPT(1),ECPT(1))
      DATA    M( 1,1),M( 1,2),M( 1,3),M( 1,4) / 1   ,2   ,3   ,4 /
      DATA    M( 2,1),M( 2,2),M( 2,3),M( 2,4) / 1   ,2   ,3   ,5 /
      DATA    M( 3,1),M( 3,2),M( 3,3),M( 3,4) / 1   ,2   ,3   ,6 /
      DATA    M( 4,1),M( 4,2),M( 4,3),M( 4,4) / 1   ,4   ,5   ,6 /
      DATA    M( 5,1),M( 5,2),M( 5,3),M( 5,4) / 2   ,4   ,5   ,6 /
      DATA    M( 6,1),M( 6,2),M( 6,3),M( 6,4) / 3   ,4   ,5   ,6 /
      DATA    M( 7,1),M( 7,2),M( 7,3),M( 7,4) / 2   ,1   ,4   ,6 /
      DATA    M( 8,1),M( 8,2),M( 8,3),M( 8,4) / 2   ,3   ,4   ,6 /
      DATA    M( 9,1),M( 9,2),M( 9,3),M( 9,4) / 1   ,3   ,4   ,5 /
      DATA    M(10,1),M(10,2),M(10,3),M(10,4) / 2   ,3   ,4   ,5 /
      DATA    M(11,1),M(11,2),M(11,3),M(11,4) / 3   ,1   ,5   ,6 /
      DATA    M(12,1),M(12,2),M(12,3),M(12,4) / 2   ,1   ,5   ,6 /
C
C     MAP FOR HEXA-SOLID (5 OR 10 TETRAHEDRONS)
C
      DATA    M(13,1),M(13,2),M(13,3),M(13,4) / 1   ,2   ,3   ,6 /
      DATA    M(14,1),M(14,2),M(14,3),M(14,4) / 1   ,3   ,4   ,8 /
      DATA    M(15,1),M(15,2),M(15,3),M(15,4) / 1   ,3   ,8   ,6 /
      DATA    M(16,1),M(16,2),M(16,3),M(16,4) / 1   ,5   ,6   ,8 /
      DATA    M(17,1),M(17,2),M(17,3),M(17,4) / 3   ,6   ,7   ,8 /
      DATA    M(18,1),M(18,2),M(18,3),M(18,4) / 2   ,3   ,4   ,7 /
      DATA    M(19,1),M(19,2),M(19,3),M(19,4) / 1   ,2   ,4   ,5 /
      DATA    M(20,1),M(20,2),M(20,3),M(20,4) / 2   ,4   ,5   ,7 /
      DATA    M(21,1),M(21,2),M(21,3),M(21,4) / 2   ,5   ,6   ,7 /
      DATA    M(22,1),M(22,2),M(22,3),M(22,4) / 4   ,5   ,7   ,8 /
      DATA    IDELEM / 0 /
C
C     BRANCH ON ELEMENT TYPE
C
      IGFLAG = 0
      GO TO (1000,2000,3000), ITYPE
C
C     COME HERE FOR WEDGE COMPUTATIONS.
C     KTETRA IS CALLED 3 TIMES BASED ON WEDGE MAPPING MATRIX.
C
 1000 ITET   = 1
      NTET   = 12
      ITEMP  = 33
      NGRIDS = 6
      IOPT   = 0
C
C     BASE CROSS PRODUCT
C
      IF (NECPT(1) .EQ. IDELEM) GO TO 1951
      IDELEM = NECPT(1)
      IGFLAG = 1
      R12(1) = ECPT(14) - ECPT(10)
      R12(2) = ECPT(15) - ECPT(11)
      R12(3) = ECPT(16) - ECPT(12)
      R13(1) = ECPT(18) - ECPT(10)
      R13(2) = ECPT(19) - ECPT(11)
      R13(3) = ECPT(20) - ECPT(12)
      CALL SAXB (R12,R13,RXR)
C
C     IN THE ABOVE, THE WEDGE IS NUMBERED 1,2,3 COUNTERCLOCKWISE AT THE
C     BASE AND 4,5,6 COUNTER CLOCKWISE AT THE TOP. (LOOKING DOWN ON WED)
C
      R12(1) = ECPT(26) - ECPT(22)
      R12(2) = ECPT(27) - ECPT(23)
      R12(3) = ECPT(28) - ECPT(24)
      R13(1) = ECPT(30) - ECPT(22)
      R13(2) = ECPT(31) - ECPT(23)
      R13(3) = ECPT(32) - ECPT(24)
      CALL SAXB (R12,R13,R)
C
      IF (SADOTB(R,RXR)) 1800,1800,1950
C
C     ERROR CONDITION - BAD GEOMETRY
C
 1800 WRITE  (OUT,1900) UFM,NECPT(1)
 1900 FORMAT (A23,' 4001, ELEMENT',I10,' HAS BAD GEOMETRY.')
      NOGOO = 1
      RETURN
C
C     PLANER CHECKS FOR WEDGE
C
 1950 CALL KPLTST (ECPT(10),ECPT(14),ECPT(26),ECPT(22))
      CALL KPLTST (ECPT(10),ECPT(22),ECPT(30),ECPT(18))
      CALL KPLTST (ECPT(14),ECPT(18),ECPT(30),ECPT(26))
 1951 IF (NOGOO .EQ. 1) RETURN
      GO TO 6000
C
C     COME HERE FOR 5-TETRAHEDRON 6-SIDED SOLID
C
 2000 ITET   = 13
      NTET   = 17
      ITEMP  = 43
      NGRIDS = 8
      IOPT   = 0
      GO TO 3500
C
C     COME HERE FOR 10-TETRAHEDRON 6-SIDED SOLID
C
 3000 ITET   = 13
      NTET   = 22
      ITEMP  = 43
      NGRIDS = 8
      IOPT   = 1
C
C     CHECK GEOMETRY OF 6-SIDED SOLID AT THIS POINT
C
 3500 IF (NECPT(1) .EQ. IDELEM) GO TO 2951
      IDELEM = NECPT(1)
      IGFLAG = 1
      R13(1) = ECPT(20) - ECPT(12)
      R13(2) = ECPT(21) - ECPT(13)
      R13(3) = ECPT(22) - ECPT(14)
      R24(1) = ECPT(24) - ECPT(16)
      R24(2) = ECPT(25) - ECPT(17)
      R24(3) = ECPT(26) - ECPT(18)
      CALL SAXB (R13,R24,RXR)
C
      R12(1) = ECPT(36) - ECPT(28)
      R12(2) = ECPT(37) - ECPT(29)
      R12(3) = ECPT(38) - ECPT(30)
      R13(1) = ECPT(40) - ECPT(32)
      R13(2) = ECPT(41) - ECPT(33)
      R13(3) = ECPT(42) - ECPT(34)
      CALL SAXB (R12,R13,R)
C
      IF (SADOTB(RXR,R)) 1800,1800,2950
C
C     PLANER CHECKS FOR HEXA-5 OR HEXA-10
C
 2950 CALL KPLTST (ECPT(12),ECPT(16),ECPT(20),ECPT(24))
      CALL KPLTST (ECPT(12),ECPT(16),ECPT(32),ECPT(28))
      CALL KPLTST (ECPT(16),ECPT(20),ECPT(36),ECPT(32))
      CALL KPLTST (ECPT(20),ECPT(24),ECPT(40),ECPT(36))
      CALL KPLTST (ECPT(24),ECPT(12),ECPT(28),ECPT(40))
      CALL KPLTST (ECPT(28),ECPT(32),ECPT(36),ECPT(40))
 2951 IF (NOGOO .EQ. 1) RETURN
      GO TO 6000
C
C     AT THIS POINT ALL CHECKS HAVE BEEN MADE. NOW FORM THE ECPT FOR
C     EACH TETRAHEDRON AND CALL KTETRA(IOPT). IOPT = 1 IMPLIES TO COMPUT
C     HALF STIFFNESS. IOPT = 0 IMPLIES COMPUTE FULL STIFFNESS.
C
 6000 DO 6010 J = 1,50
      ECPT(J+50) = ECPT(J)
 6010 CONTINUE
C
C     FILL MAT ID AND EL TEMP
C
      NECPT( 2) = NECPT(52)
      NECPT(23) = NECPT(ITEMP+50)
      JTYPE     = ITYPE
      DO 8000 I = ITET,NTET
      IF (I .EQ. NTET) JTYPE = -ITYPE
      IF (ITYPE .EQ. 1) IOPT = I + 10
C
C     FILL IN GRID SIL-S AND COORDINATE SETS
C
      DO 7030 J = 1,4
      KPOINT = M(I,J)
      NECPT(J+2) = NECPT(KPOINT+52)
      KPOINT = 4*KPOINT + NGRIDS - 3
      JPOINT = 4*J + 2
      NECPT(JPOINT+1) = NECPT(KPOINT+52)
      NECPT(JPOINT+2) = NECPT(KPOINT+53)
      NECPT(JPOINT+3) = NECPT(KPOINT+54)
      NECPT(JPOINT+4) = NECPT(KPOINT+55)
 7030 CONTINUE
C
C     BUMP IOPT IF GEOMETRY TESTS ARE TO BE MADE
C
      IF (IGFLAG .EQ. 1) IOPT = IOPT + 100
      CALL KTETRA (IOPT,JTYPE)
 8000 CONTINUE
C
C     ALL THROUGH
C
      RETURN
      END
