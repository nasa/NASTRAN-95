      SUBROUTINE SCONE2 (SORC)
C
C     PHASE II OF STRESS DATA RECOVERY
C
C     OUTPUTS FROM PHASE I ARE THE FOLLOWING (TOTAL OF 118 WORDS) -
C     1) ELEMENT ID
C     2 AND 3) SILS A AND B
C     4) S SUB T
C     5) N
C     6) I
C     7) Z1
C     8) Z2
C     9 THRU 22) PHI-S
C     23 THRU 118) TWO 8X6 S MATRICES
C
      LOGICAL         ZERO
      INTEGER         SIL(2),IFORCE(8),ISTRES(100),ELEMID,SORC,
     1                IBLOCK(9,14)
      REAL            NPHI,PHI(14),FORCE(7),S(96),STRESS(18),ZOFF(2),III
      COMMON /CONDAS/ CONSTS(5)
      COMMON /ZZZZZZ/ Z(1)
      COMMON /SDR2X4/ DUMMY(35),IVEC,DUM11(3)
      COMMON /SDR2X7/ COMMUN(225)
      COMMON /SDR2X8/ VEC(8),SUM(8),SIG(3),SIG1,SIG2,SIG12,TEMP,
     1                DELTA,THETA,NPOINT,ZOVERI,IPT,BLOCK(9,14),
     2                NELHAR,ELEMID,HARM,N,SINPHI,CONPHI,NPHI,NANGLE
      EQUIVALENCE     (CONSTS(4),DEGRA),(NELEM,COMMUN(1)),
     1                (SIL(1),COMMUN(2)),(III,COMMUN(6)),
     2                (ZOFF(1),COMMUN(7)),(PHI(1),COMMUN(9)),
     3                (S(1),COMMUN(23)),(IBLOCK(1,1),BLOCK(1,1)),
     4                (STRESS(1),COMMUN(101),ISTRES(1)),
     5                (FORCE (1),COMMUN(201),IFORCE(1))
      DATA    NELOLD/ -1 /
C
      DO 10 I = 1,8
   10 SUM(I) = 0.0
C
      ELEMID = NELEM/1000
      NELHAR = NELEM - ELEMID*1000
C
C     ZERO OUT BLOCK IF THIS IS FIRST CALL WITH HARMONIC = 0 FOR THIS
C     ELEMENT
C
      N = NELHAR - 1
      IF (N .NE. 0) GO TO 21
      IF (ELEMID .EQ. NELOLD) GO TO 21
      NELOLD = ELEMID
      DO 12 I = 2,9
      DO 12 J = 1,14
      BLOCK(I,J) = 0.0
   12 CONTINUE
C
C     INSERT ANGLES FOR OUTPUT INTO FIRST ROW OF BLOCK
C
      ZERO = .FALSE.
      J = 0
      DO 19 I = 1,14
      IF (PHI(I)) 17,15,17
   15 IF (ZERO) GO TO 19
      ZERO = .TRUE.
   17 J = J + 1
      BLOCK(1,J) = PHI(I)
   19 CONTINUE
      J = J + 1
      IF (J .LE. 14) IBLOCK(1,J) = 1
   21 HARM = N
C
      DO 30 I = 1,2
C
C     DISPLACEMENT VECTOR POINTER
C
      NPOINT = IVEC + SIL(I) - 1
C
      CALL GMMATS (S(48*I-47),8,6,0, Z(NPOINT),6,1,0, VEC(1))
C
      DO 25 J = 1,8
   25 SUM(J) = SUM(J) + VEC(J)
   30 CONTINUE
C
C     INSERT HARMONIC STRESSES AND FORCES INTO BLOCK FOR THIS HARMONIC
C
      DO 40 I = 1,14
      IF (IBLOCK(1,I) .EQ. 1) GO TO 50
      NPHI   = HARM*BLOCK(1,I)*DEGRA
      SINPHI = SIN(NPHI)
      CONPHI = COS(NPHI)
      GO TO (35,36), SORC
   35 BLOCK(2,I) = BLOCK(2,I) + SINPHI*SUM(1)
      BLOCK(3,I) = BLOCK(3,I) + SINPHI*SUM(2)
      BLOCK(4,I) = BLOCK(4,I) - CONPHI*SUM(3)
      BLOCK(5,I) = BLOCK(5,I) + SINPHI*SUM(4)
      BLOCK(6,I) = BLOCK(6,I) + SINPHI*SUM(5)
      BLOCK(7,I) = BLOCK(7,I) - CONPHI*SUM(6)
      BLOCK(8,I) = BLOCK(8,I) + SINPHI*SUM(7)
      BLOCK(9,I) = BLOCK(9,I) - CONPHI*SUM(8)
      GO TO 40
   36 BLOCK(2,I) = BLOCK(2,I) + CONPHI*SUM(1)
      BLOCK(3,I) = BLOCK(3,I) + CONPHI*SUM(2)
      BLOCK(4,I) = BLOCK(4,I) + SINPHI*SUM(3)
      BLOCK(5,I) = BLOCK(5,I) + CONPHI*SUM(4)
      BLOCK(6,I) = BLOCK(6,I) + CONPHI*SUM(5)
      BLOCK(7,I) = BLOCK(7,I) + SINPHI*SUM(6)
      BLOCK(8,I) = BLOCK(8,I) + CONPHI*SUM(7)
      BLOCK(9,I) = BLOCK(9,I) + SINPHI*SUM(8)
   40 CONTINUE
C
C     COPY FORCES INTO FORCE OUTPUT BLOCK
C
   50 IFORCE(1) = ELEMID
      IFORCE(2) = NELHAR
      FORCE (3) = SUM(4)
      FORCE (4) = SUM(5)
      FORCE (5) = SUM(6)
      FORCE (6) = SUM(7)
      FORCE (7) = SUM(8)
C
C     COMPUTE STRESSES AT Z1 AND Z2
C
      ISTRES(1) = ELEMID
      ISTRES(2) = NELHAR
C
      DO 70 I = 1,2
      ZOVERI = 0.0
      IF (III .NE. 0.0) ZOVERI = ZOFF(I)/III
C
      DO 60 J = 1,3
   60 SIG(J) = SUM(J) + SUM(J+3)*ZOVERI
C
      IPT = 8*I - 6
      STRESS(IPT+1) = ZOFF(I)
      STRESS(IPT+2) = SIG(1)
      STRESS(IPT+3) = SIG(2)
      STRESS(IPT+4) = SIG(3)
      ISTRES(IPT+5) = 1
      ISTRES(IPT+6) = 1
      ISTRES(IPT+7) = 1
      ISTRES(IPT+8) = 1
   70 CONTINUE
C
      RETURN
      END
