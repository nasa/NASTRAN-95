      SUBROUTINE MCONE
C
C     MASS MATRIX GENERATION FOR AXIS-SYMETRIC CONICAL SHELL ELEMENT
C
C     ECPT( 1) = ELEMENT ID             INTEGER        ECT
C     ECPT( 2) = SIL PT A               INTEGER        ECT
C     ECPT( 3) = SIL PT B B             INTEGER        ECT
C     ECPT( 4) = MATID 1                INTEGER        EPT
C     ECPT( 5) = T   (MEMBRANE THICK)   REAL           EPT
C     ECPT( 6) = MATID 2                INTEGER        EPT
C     ECPT( 7) = I   (MOM.OF INERTIA)   REAL           EPT
C     ECPT( 8) = MATID 3                INTEGER        EPT
C     ECPT( 9) = TS  (SHEAR THICKNESS)  REAL           EPT
C     ECPT(10) = NON-STRUCTURAL-MASS    REAL           EPT
C     ECPT(11) = Z1                     REAL           EPT
C     ECPT(12) = Z2                     REAL           EPT
C     ECPT(13) = PHI  1                 REAL           EPT
C     ECPT(14) = PHI  2                 REAL           EPT
C     ECPT(15) = PHI  3                 REAL           EPT
C     ECPT(16) = PHI  4                 REAL           EPT
C     ECPT(17) = PHI  5                 REAL           EPT
C     ECPT(18) = PHI  6                 REAL           EPT
C     ECPT(19) = PHI  7                 REAL           EPT
C     ECPT(20) = PHI  8                 REAL           EPT
C     ECPT(21) = PHI  9                 REAL           EPT
C     ECPT(22) = PHI 10                 REAL           EPT
C     ECPT(23) = PHI 11                 REAL           EPT
C     ECPT(24) = PHI 12                 REAL           EPT
C     ECPT(25) = PHI 13                 REAL           EPT
C     ECPT(26) = PHI 14                 REAL           EPT
C     ECPT(27) = COORD. SYS. ID PT.1    INTEGER        BGPDT
C     ECPT(28) = RADIUS PT. 1           REAL           BGPDT
C     ECPT(29) = DISTANCE TO PT.1       REAL           BGPDT
C     ECPT(30) = NULL                   REAL           BGPDT
C     ECPT(31) = COORD. SYS. ID PT.2    INTEGER        BGPDT
C     ECPT(32) = RADIUS PT 2            REAL           BGPDT
C     ECPT(33) = DISTANCE TO PT. 2      REAL           BGPDT
C     ECPT(34) = NULL                   REAL           BGPDT
C     ECPT(35) = ELEMENT TEMPERATURE    REAL           GEOM3
C
      INTEGER          NECPT(100)
      REAL             L,MU
      DOUBLE PRECISION MASS
      COMMON /CONDAS/  PI, TWOPI, RADEG, DEGRA, S4PISQ
      COMMON /MATIN /  MATID, INFLAG, ELTEMP
      COMMON /MATOUT/  RHO
      COMMON /SMA2ET/  ECPT(100)
      COMMON /SMA2IO/  DUM4(10), IFMGG, DUM5(25)
      COMMON /SMA2CL/  DUM3(2),NPVT
      COMMON /SMA2DP/  MASS(36), TEMP, L, TERM, M1
      EQUIVALENCE      (RA,ECPT(28)), (RB,ECPT(32)), (ZA,ECPT(29)),
     1                 (ZB,ECPT(33)), (T,ECPT(5))  , (MU,ECPT(10)),
     2                 (NECPT(1),ECPT(1))
C
      L = SQRT((RB-RA)**2 + (ZB-ZA)**2)
C
C     NEXT LINE WAS REMOVED BY M.H./NAVY. ERROR FOR CONICAL SHELL MASS
C
C
      TEMP = RB/6.0 + RA/3.0
C
      IF (T) 30,40,30
   30 INFLAG = 4
      MATID  = NECPT(4)
      ELTEMP = ECPT(35)
      CALL MAT (NECPT(1))
   40 DO 50 I = 1,36
   50 MASS(I) = 0.0D0
      TERM = PI*L*TEMP*(RHO*T + MU)
      IF (NECPT(1)-(NECPT(1)/1000)*1000-1 .EQ. 0) TERM = TERM*2.0
      MASS( 1) = TERM
      MASS( 8) = TERM
      MASS(15) = TERM
      M1 = -1
      CALL SMA2B (MASS(1),NPVT,M1,IFMGG,0.0D0)
      RETURN
      END
