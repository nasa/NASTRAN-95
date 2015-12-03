      SUBROUTINE MSOLID (ITYPE)
C
C     THIS ROUTINE CALCULATES THE MASS MATRICES FOR THE SOLID ELEMENTS,
C
C          I =     ELEMENT
C          ***     *******
C          1       CTETRA
C          2       CWEDGE
C          3       CHEXA1
C          4       CHEXA2
C
C     A SERIES OF 6 BY 6 DIAGONAL MATRICES ARE CALUCLATED, ONE PER
C     CONNECTED GRID POINT
C
C     ECPT        TETRA          WEDGE          HEXA
C     -------------------------------------------------
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
      LOGICAL          HEAT
      INTEGER          M(14,4),NECPT(100)
      DOUBLE PRECISION PTMASS,EMASS,R(3,3),MGE(36)
      COMMON /SMA2HT/  HEAT
      COMMON /SMA2ET/  ECPT(100)
      COMMON /MATIN /  MATIDC,MATFLG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /MATOUT/  RHO
      COMMON /HMTOUT/  CP
      COMMON /SMA2DP/  PTMASS,EMASS,R,MGE,NPTS,NEL,MFIRST,KPT,NROW,JLOC,
     1                 ITEST,J1,M1,ILOC(4)
      COMMON /SMA2CL/  DUM1(2),NPVT
      COMMON /SMA2IO/  DUM2(10),IFMGG,DUMXX(1),IFBGG
      EQUIVALENCE      (ECPT(1),NECPT(1))
      DATA    M( 1,1), M( 1,2),M( 1,3),M( 1,4) / 1   ,2   ,3   ,4 /
      DATA    M( 2,1), M( 2,2),M( 2,3),M( 2,4) / 1   ,2   ,3   ,6 /
      DATA    M( 3,1), M( 3,2),M( 3,3),M( 3,4) / 1   ,2   ,6   ,5 /
      DATA    M( 4,1), M( 4,2),M( 4,3),M( 4,4) / 1   ,4   ,5   ,6 /
      DATA    M( 5,1), M( 5,2),M( 5,3),M( 5,4) / 1   ,2   ,3   ,6 /
      DATA    M( 6,1), M( 6,2),M( 6,3),M( 6,4) / 1   ,3   ,4   ,8 /
      DATA    M( 7,1), M( 7,2),M( 7,3),M( 7,4) / 1   ,3   ,8   ,6 /
      DATA    M( 8,1), M( 8,2),M( 8,3),M( 8,4) / 1   ,5   ,6   ,8 /
      DATA    M( 9,1), M( 9,2),M( 9,3),M( 9,4) / 3   ,6   ,7   ,8 /
      DATA    M(10,1), M(10,2),M(10,3),M(10,4) / 2   ,3   ,4   ,7 /
      DATA    M(11,1), M(11,2),M(11,3),M(11,4) / 1   ,2   ,4   ,5 /
      DATA    M(12,1), M(12,2),M(12,3),M(12,4) / 2   ,4   ,5   ,7 /
      DATA    M(13,1), M(13,2),M(13,3),M(13,4) / 2   ,5   ,6   ,7 /
      DATA    M(14,1), M(14,2),M(14,3),M(14,4) / 4   ,5   ,7   ,8 /
C
C     SET THE ELEMENT PARAMETERS ACCORDING TO THE TYPE
C                               NPTS = NO. OF CONNECTED POINTS
C                               NEL  = NO. OF SUBELEMENTS
C                               MFIRST=POSITION OF FIRST ROW OF MAPPING
C                                       MATRIX
C
      GO TO (100,110,120,130), ITYPE
  100 NPTS  = 4
      NEL   = 1
      MFIRST= 1
      GO TO 140
  110 NPTS  = 6
      NEL   = 3
      MFIRST= 2
      GO TO 140
  120 NPTS  = 8
      NEL   = 5
      MFIRST= 5
      GO TO 140
  130 NPTS  = 8
      NEL   = 10
      MFIRST= 5
  140 CONTINUE
C
C     FETCH THE MATERIAL ID AND THE DENSITY, RHO
C
      MATIDC = NECPT(2)
      MATFLG = 4
      NTEMP  = 5*NPTS + 3
      ELTEMP = ECPT(NTEMP)
      IF (.NOT.HEAT) CALL MAT (ECPT(1))
      IF (HEAT) CALL HMAT (ECPT)
      IF (HEAT) RHO = CP
      IF (RHO .EQ. 0.0) GO TO 1200
C
C     ZERO OUT POINT MASS
C
      PTMASS = 0.0D0
C
C     LOOP ON SUBELEMENTS
C
      DO 1000 ME = 1,NEL
      NROW = MFIRST + ME - 1
C
C     SET UP POINTERS TO LOCATION VECTORS AND TEST IF ELEMENT IS
C     CONNECTED
C
      ITEST = 0
      DO 300 I = 1,4
      KPT = M(NROW,I)
      IF (NECPT(KPT+2) .NE. NPVT) GO TO 250
      ITEST = 1
C
C     THE LOCATION OF THE VECTOR DATA IN THE ECPT IS
C
  250 ILOC(I) = 4*KPT + NPTS
  300 CONTINUE
      IF (ITEST .EQ. 0) GO TO 1000
C
C     CALCULATE DIFFERENCE VECTORS FROM THE FIRST VECTOR
C
      DO 500 I = 2,4
      DO 400 J = 1,3
      JLOC = ILOC(I) + J - 1
      J1   = ILOC(1) + J - 1
  400 R(I-1,J) = ECPT(JLOC) - ECPT(J1)
  500 CONTINUE
C
C     THE MASS ON EACH POINT DUE TO THE TETRAHEDRON IS
C     (NEGATIVE VALUE OF RHO IS ALLOWED)
C
      EMASS = RHO/24.D0*DABS((R(3,1)*(R(1,2)*R(2,3) - R(1,3)*R(2,2))
     1                      + R(3,2)*(R(1,3)*R(2,1) - R(1,1)*R(2,3))
     2                      + R(3,3)*(R(1,1)*R(2,2) - R(1,2)*R(2,1))))
      IF (ITYPE.NE.4) GO TO 600
      EMASS = EMASS/2.0D0
C
C     THE MASS IS NOW ADDED TO THE APPROPRIATE POINT
C
  600 PTMASS = PTMASS + EMASS
 1000 CONTINUE
C
C     THE MASSES ARE EXPANDED AND INSERTED
C
      IF (HEAT) GO TO 1150
      DO 1100 I = 1,36
 1100 MGE(I) = 0.0D0
      M1     =-1
      MGE(1) = PTMASS
      MGE(8) = MGE(1)
      MGE(15)= MGE(1)
      CALL SMA2B (MGE(1),NPVT,M1,IFMGG,0.0D0)
      GO TO 1200
 1150 CALL SMA2B (PTMASS,NPVT,NPVT,IFBGG,0.0D0)
C
C     ALL DONE
C
 1200 RETURN
      END
