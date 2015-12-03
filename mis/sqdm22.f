      SUBROUTINE SQDM22
C
C     PHASE-II STRESS-DATA-RECOVERY ROUTINE FOR THE -QDMEM2- ELEMENT.
C
C     THIS ROUTINE USES DATA PREPARED BY -SQDM21-, THE PHASE-I ROUTINE,
C     TOGETHER WITH THE DISPLACEMENT VECTOR AND TEMPERATURE DATA
C     TO ARRIVE AT STRESS AND FORCE OUTPUTS.
C
      INTEGER         IFORCE(1),ISTR(1),ISILS(4),EJECT,ISHED(7),ISTYP(2)
      REAL            STRESS(8),FORCE(17),KIJ(9,16),SG(36),PT(3,4),
     1                ST(3),RG(4),FRLAST(2)
      COMMON /ZZZZZZ/ Z(1)
      COMMON /SDR2X4/ DUMMY(35),IVEC,IVECN,LDTEMP,DEFORM,DUM8(8),TLOADS
      COMMON /SDR2X7/ ID(217)
      COMMON /SDR2X8/ VEC(4),SIGXYZ(3),F(4,3),SHEARS(4),CVC(4),FF(4,3),
     1                CFRVEC(20),CSHARS(4)
      COMMON /SDR2X9/ NCHK,ISUB,ILD,FRTMEI(2),TWOTOP,FNCHK
      COMMON /SYSTEM/ IBFSZ,NOUT,IDM(9),LINE
      EQUIVALENCE     (TEMP,LDTEMP)
      EQUIVALENCE     (ID(2),ISILS(1)),(ID(7),TSUB0),(ID(8),KIJ(1,1)),
     1                (ID(101),STRESS(1),ISTR(1)),
     2                (ID(152),SG(1)),(ID(188),PT(1,1)),
     3                (ID(200),ST(1)),(ID(201),FORCE(1),IFORCE(1)),
     4                (ID(203),RG(1))
      EQUIVALENCE     (F2,FORCE( 2)),(F1,FORCE( 3)),(F3,FORCE( 4)),
     1                (F4,FORCE( 5)),(F6,FORCE( 6)),(F5,FORCE( 7)),
     2                (F7,FORCE( 8)),(F8,FORCE( 9)),(FK1,FORCE(10)),
     3                (Q1,FORCE(11)),(FK2,FORCE(12)),(Q2,FORCE(13)),
     4                (FK3,FORCE(14)),(Q3,FORCE(15)),(FK4,FORCE(16)),
     5                (Q4,FORCE(17))
      EQUIVALENCE     (ISHED(1),LSUB),(ISHED(2),LLD),
     1                (ISHED(6),FRLAST(1))
      DATA    ISTYP / 4HQDME, 2HM2 /
      DATA    LSUB  , LLD,FRLAST   / 2*-1, -1.0E30,-1.0E30 /
C
C     SIG , SIG , TAU   = SUMMATION((S )(U )) - (S )(TEMP-T )
C        X     Y     XY               I   I       T        0
C
      SIGXYZ(1) = 0.0
      SIGXYZ(2) = 0.0
      SIGXYZ(3) = 0.0
      CFRVEC(2) = 0.0
      CFRVEC(3) = 0.0
      CFRVEC(4) = 0.0
C
      DO 20 I = 1,4
      J = IVEC + ISILS(I)
      CALL SMMATS (SG(9*I-8),3,3,0, Z(J-1),3,1,0, VEC,CVC)
      DO 10 J = 1,3
      SIGXYZ(J  ) = SIGXYZ(J  ) + VEC(J)
      CFRVEC(J+1) = CFRVEC(J+1) + CVC(J)
   10 CONTINUE
   20 CONTINUE
C
      IF (LDTEMP .EQ. -1) GO TO 40
      TBAR = TEMP - TSUB0
      DO 30 J = 1,3
      SIGXYZ(J) = SIGXYZ(J) - ST(J)*TBAR
   30 CONTINUE
C
C     FORCES
C          I                             T
C        (F ) = SUMMATION((K  )(U )) - (P )(TEMP-T )
C                           IJ   I       I        0
C
   40 IPART   = 0
      DO 60 I = 1,4
      F(I,1)  = 0.0
      F(I,2)  = 0.0
      F(I,3)  = 0.0
      FF(I,1) = 0.0
      FF(I,2) = 0.0
      FF(I,3) = 0.0
      DO 50 J = 1,4
      K       = IVEC  + ISILS(J)
      IPART   = IPART + 1
      CALL SMMATS (KIJ(1,IPART),3,3,0, Z(K-1),3,1,0, VEC,CVC)
      F(I,1)  = F(I,1)  + VEC(1)
      F(I,2)  = F(I,2)  + VEC(2)
      F(I,3)  = F(I,3)  + VEC(3)
      FF(I,1) = FF(I,1) + CVC(1)
      FF(I,2) = FF(I,2) + CVC(2)
      FF(I,3) = FF(I,3) + CVC(3)
   50 CONTINUE
      IF (LDTEMP .EQ. -1) GO TO 60
      TBAR   = TEMP - TSUB0
      F(I,1) = F(I,1) - PT(1,I)*TBAR
      F(I,2) = F(I,2) - PT(2,I)*TBAR
      F(I,3) = F(I,3) - PT(3,I)*TBAR
   60 CONTINUE
C
C     SHEARS = SUMMATION (R )(U )
C                          I   I
      DO 80 I = 1,4
      IP1 = I + 1
      IF (IP1 .EQ. 5) IP1 = 1
      SHEARS(I) = (F(IP1,2)-F(I,1))/RG(I)
      CSHARS(I) = (FF(IP1,2)-FF(I,1))/ABS(RG(I))
   80 CONTINUE
C
C     ALL COMPUTATIONS COMPLETE.
C
      Q1 = -SHEARS(1)
      Q2 =  SHEARS(2)
      Q3 = -SHEARS(3)
      Q4 =  SHEARS(4)
      CFRVEC(14) = -CSHARS(1)
      CFRVEC(16) = +CSHARS(2)
      CFRVEC(18) = -CSHARS(3)
      CFRVEC(20) = +CSHARS(4)
C
      ISTR(1)   = ID(1)
      CFRVEC(1) = STRESS(1)
      STRESS(2) = SIGXYZ(1)
      STRESS(3) = SIGXYZ(2)
      STRESS(4) = SIGXYZ(3)
C
      IFORCE(1) = ID(1)
      F1 = F(1,1)
      F2 = F(1,2)
      F3 = F(2,2)
      F4 = F(2,1)
      F5 = F(3,1)
      F6 = F(3,2)
      F7 = F(4,2)
      F8 = F(4,1)
      CFRVEC( 6) = FF(1,1)
      CFRVEC( 5) = FF(1,2)
      CFRVEC( 7) = FF(2,2)
      CFRVEC( 8) = FF(2,1)
      CFRVEC(10) = FF(3,1)
      CFRVEC( 9) = FF(3,2)
      CFRVEC(11) = FF(4,2)
      CFRVEC(12) = FF(4,1)
C
      FK1 = F(1,3)
      FK2 = F(2,3)
      FK3 = F(3,3)
      FK4 = F(4,3)
      CFRVEC(13) = FF(1,3)
      CFRVEC(15) = FF(2,3)
      CFRVEC(17) = FF(3,3)
      CFRVEC(19) = FF(4,3)
C
      TEMP = STRESS(2) - STRESS(3)
C
C     COMPUTE TAU
C
      STRESS(8) = SQRT((TEMP/2.0)**2+STRESS(4)**2)
      DELTA = (STRESS(2)+STRESS(3))/2.0
C
C     COMPUTE SIGMA 1 AND SIGMA 2
C
      STRESS(6) = DELTA + STRESS(8)
      STRESS(7) = DELTA - STRESS(8)
      DELTA = 2.0*STRESS(4)
C
C     COMPUTE PHI 1 DEPENDING ON WHETHER OR NOT SIGMA XY AND/OR
C               (SIGMA 1 - SIGMA 2) ARE ZERO
C
      IF (ABS(TEMP) .LT. 1.0E-15) GO TO 5
      STRESS(5) = ATAN2(DELTA,TEMP)*28.64788980
      GO TO 7
    5 IF (ABS(DELTA) .LT. 1.0E-15) GO TO 6
      STRESS(5) = 0.0
      GO TO 7
    6 STRESS(5) = 45.0
    7 IF (NCHK .LE. 0) GO TO 150
C
C     STRESS/FORCE PRECISION CHECK
C
      K = 0
C
C     STRESSES
C
      CALL SDRCHK (STRESS(2),CFRVEC(2),3,K)
C
C     FORCES
C
      CALL SDRCHK (FORCE(2),CFRVEC(5),16,K)
      IF (K .EQ. 0) GO TO 150
C
C     LIMITS EXCEEDED
C
      J = 0
      IF (LSUB.EQ.ISUB .AND. FRLAST(1).EQ.FRTMEI(1) .AND.
     1    LLD .EQ.ILD  .AND. FRLAST(2).EQ.FRTMEI(2)) GO TO 120
C
      LSUB = ISUB
      LLD  = ILD
      FRLAST(1) = FRTMEI(1)
      FRLAST(2) = FRTMEI(2)
      J = 1
      CALL PAGE1
  100 CALL SD2RHD (ISHED,J)
      LINE = LINE + 1
      WRITE  (NOUT,110)
  110 FORMAT (3X,4HTYPE,5X,3HEID,4X,2HSX,4X,2HSY,3X,3HSXY,11H  F1-4  F1-
     1,60H2  F2-1  F2-3  F3-2  F3-4  F4-3  F4-1   K-1  SH12   K-2  SH2
     2,25H3   K-3  SH34   K-4  SH41)
      GO TO 130
  120 IF (EJECT(2) .NE. 0) GO TO 100
C
  130 WRITE  (NOUT,140) ISTYP,CFRVEC
  140 FORMAT (2H0 ,A4,A2,I7,19F6.1)
C
  150 RETURN
      END
