      SUBROUTINE SAXIF1 (IOPT)
C
C     THIS ROUTINE GENERATES MATRICES WHICH RELATE PRESSURE TO VELOCITY
C     IN A FLUID. IOPT DETERMINES THE ELEMENT TYPE
C
C        IOPT     TYPE
C         0      CAXIF2
C         1      CAXIF3
C         2      CAXIF4
C
      INTEGER         NEST(100),SIL
      DIMENSION       A(9)
      COMMON /SDR2X5/ EST(100),NID,SIL(4),SV(95)
      COMMON /SDR2X6/ HM(9),R(4),Z(4),AM(9),COEF,EN,EL,RBAR,ZBAR,
     1                R1N,R2N,RBN1,DR,DZ,I1,I2,I3,IRET,IJ,IK,KJ
      EQUIVALENCE     (EST(1),NEST(1)),(AM(1),A(1))
C
      DO 10 I = 1,44
   10 SV(I) = 0.0
      NID = NEST(1)
      IF (IOPT-1) 20,50,70
C
C     CAXIF2 ELEMENTS
C
   20 IF (NEST(6) .GE. 1) GO TO 30
      COEF = EST(4)*(EST(13)-EST(9))
      IF (COEF .EQ. 0.0) RETURN
      SV(3) = 1.0/COEF
      SV(4) = -SV(3)
      GO TO 40
   30 IF (NEST(6) .GT. 1) GO TO 40
      COEF  = EST(4)*(EST(8)+EST(12))
      IF (COEF .EQ. 0.0) RETURN
      SV(1) = -1.0/COEF
      SV(2) =  SV(1)
   40 CONTINUE
      EN    = FLOAT(NEST(6))
      RBAR  = (EST(8)+EST(12))/2.0
      ZBAR  = (EST(9)+EST(13))/2.0
      DR    = EST(12) - EST(8)
      DZ    = EST(13) - EST(9)
      R1N   = EST( 8)**NEST(6)
      R2N   = EST(12)**NEST(6)
      RBN1  = RBAR**(NEST(6)-1)
      HM(1) = EST(13)/(R1N*DZ)
      HM(2) =-EST( 9)/(R2N*DZ)
      HM(3) =-1.0/(R1N*DZ)
      HM(4) = 1.0/(R2N*DZ)
      EL    = SQRT(DZ**2 +DR**2)
      COEF  = RBN1/(EST(4)*EL)
      AM(1) = EN*DR*COEF
      AM(2) = (EN*DR*ZBAR + RBAR*DZ)*COEF
      AM(3) = EN*EL*COEF
      AM(4) = EN*ZBAR*EL*COEF
      SV(5) = AM(1)*HM(1) + AM(2)*HM(3)
      SV(6) = AM(1)*HM(2) + AM(2)*HM(4)
      SV(7) = AM(3)*HM(1) + AM(4)*HM(3)
      SV(8) = AM(3)*HM(2) + AM(4)*HM(4)
      SIL(1)= NEST(2)
      SIL(2)= NEST(3)
      RETURN
C
C     CAXIF3 ELEMENT
C
   50 N    = NEST(7)
      EN   = FLOAT(N)
      RHO  = EST(5)
      DO 60 I = 1,3
      SIL(I) = NEST(I+1)
      IR   = 4*(I-1) + 9
      R(I) = EST(IR  )
      Z(I) = EST(IR+1)
   60 CONTINUE
      I1   = 1
      I2   = 2
      I3   = 3
      RBAR = (R(I1)+R(I2)+R(I3))/3.0
      ZBAR = (Z(I1)+Z(I2)+Z(I3))/3.0
      IRET = 4
      GO TO 120
C
C     CAXIF4 ELEMENT
C
   70 N    = NEST(8)
      EN   = FLOAT(N)
      RHO  = EST(6)*4.0
      DO 80 I = 1,4
      SIL(I) = NEST(I+1)
      IR   = 4*(I-1) + 10
      R(I) = EST(IR  )
      Z(I) = EST(IR+1)
   80 CONTINUE
      RBAR = (R(1)+R(2)+R(3)+R(4))/4.0
      ZBAR = (Z(1)+Z(2)+Z(3)+Z(4))/4.0
      I1   = 1
      I2   = 2
      I3   = 3
      IRET = 1
      GO TO  120
   90 I3   = 4
      IRET = 2
      GO TO  120
  100 I2   = 3
      IRET = 3
      GO TO  120
  110 I1   = 2
      IRET = 4
C
C     ACTUAL SUBTRIANGLE CALCULATION
C
  120 IF (RHO .EQ. 0.0) RETURN
      A(1) = 0.0
      A(2) =-1.0/RHO
      A(3) = 0.0
      A(5) = A(2)*EN
      A(4) = A(5)/RBAR
      A(6) = A(4)*ZBAR
      A(7) = 0.0
      A(8) = 0.0
      A(9) = A(2)
C
      COEF = (R(I2)-R(I1))*(Z(I3)-Z(I1)) - (R(I3)-R(I1))*(Z(I2)-Z(I1))
      IF (COEF .EQ. 0.0) RETURN
      HM(1) = (R(I2)*Z(I3)-R(I3)*Z(I2))/COEF
      HM(2) = (R(I3)*Z(I1)-R(I1)*Z(I3))/COEF
      HM(3) = (R(I1)*Z(I2)-R(I2)*Z(I1))/COEF
      HM(4) = (Z(I2)-Z(I3))            /COEF
      HM(5) = (Z(I3)-Z(I1))            /COEF
      HM(6) = (Z(I1)-Z(I2))            /COEF
      HM(7) = (R(I3)-R(I2))            /COEF
      HM(8) = (R(I1)-R(I3))            /COEF
      HM(9) = (R(I2)-R(I1))            /COEF
      DO 150 J = 1,3
      JCOL  = I1
      IF (J .EQ. 2) JCOL = I2
      IF (J .EQ. 3) JCOL = I3
      DO 150 I = 1,3
      IJ = (2+IOPT)*(I-1) + JCOL
      DO 140 K = 1,3
      IK = 3*(I-1) + K
      KJ = 3*(K-1) + J
      SV(IJ) = SV(IJ) + A(IK)*HM(KJ)
  140 CONTINUE
  150 CONTINUE
      GO TO (90,100,110,160 ), IRET
C
C     THE CENTROID  CALCULATIONS ARE COMPLETE.
C
  160 NSTA = 3*(IOPT+2)
      NCOL = IOPT + 2
      IF (IOPT .EQ. 2) RHO = EST(6)
      DO 170 I = 1,NCOL
      J = I + 1
      IF (J .GT.NCOL) J = J - NCOL
      EL = SQRT((R(J)-R(I))**2  + (Z(J)-Z(I))**2)*RHO
C
      IK = NSTA + 2*NCOL*(I-1) + I
      IJ = IK + J - I
      SV(IK) = -1.0/EL
      SV(IJ) = -SV(IK)
      COEF   = -EN/((R(I)+R(J))*RHO)
      IK     = IK + NCOL
      IJ     = IJ + NCOL
      SV(IK) = COEF
      SV(IJ) = COEF
  170 CONTINUE
      RETURN
      END
