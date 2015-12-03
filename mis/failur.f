      SUBROUTINE FAILUR (FTHR,ULTSTN,STRESL,FINDEX)
C
C      THIS ROUTINE COMPUTES THE FAILURE INDEX OF A LAYER
C      IN A LAMINATED COMPOSITE ELEMENT USING ONE OF THE
C      FOLLOWING FIVE FAILURE THEORIES CURRENTLY AVAILABLE
C
C        1.   HILL
C        2.   HOFFMAN
C        3.   TSAI-WU
C        4.   MAX STRESS
C        5.   MAX STRAIN
C
C        DEFINITIONS
C        -----------
C        XT = ULTIMATE UNIAXIAL TENSILE STRENGTH IN THE FIBER
C             DIRECTION
C        XC = ULTIMATE UNIAXIAL COMPRESSIVE STRENGTH IN THE
C             FIBER DIRECTION
C        YT = ULTIMATE UNIAXIAL TENSILE STRENGTH PERPENDICULAR TO
C             THE FIBER DIRECTION
C        YC = ULTIMATE UNIAXIAL COMPRESSIVE STRENGTH PERPENDICULAR
C             TO THE FIBER DIRECTION
C        S  = ULTIMATE PLANAR SHEAR STRENGTH UNDER PURE SHEAR
C             LOADING
C        SIMILARILY FOR THE ULTIMATE STRAINS
C
      DIMENSION       ULTSTN(6),STRESL(3)
      INTEGER         FTHR
      COMMON /SYSTEM/ IBUF,NOUT
C
C**** CHECK FOR ZERO STRENGTH VALUES
C
      DO 10 I = 1,5
   10 IF (ULTSTN(I) .EQ. 0.0) GO TO 90
C
C**** ULTIMATE STRENGTH VALUES
C
      XT      = ULTSTN(1)
      XC      = ULTSTN(2)
      YT      = ULTSTN(3)
      YC      = ULTSTN(4)
      S       = ULTSTN(5)
      F12     = ULTSTN(6)
C
C**** LAYER STRESSES
C
      SIG1    = STRESL(1)
      SIG2    = STRESL(2)
      TAU12   = STRESL(3)
C
C**** LAYER STRAINS
C
      EPS1    = STRESL(1)
      EPS2    = STRESL(2)
      GAMA    = STRESL(3)
C
C
      GO TO (20,30,40,60,70), FTHR
C
C     H I L L   F A I L U R E  T H E O R Y
C     ====================================
C
   20 X = XT
      IF (SIG1 .LT. 0.0) X = XC
C
      Y = YT
      IF (SIG2 .LT. 0.0) Y = YC
C
      XX = XT
      IF ((SIG1*SIG2) .LT. 0.0) XX = XC
C
      FINDEX = ( SIG1*SIG1 )/( X*X )
      FINDEX = FINDEX + ( SIG2*SIG2 )/( Y*Y )
      FINDEX = FINDEX - ( SIG1*SIG2 )/( XX*XX )
      FINDEX = FINDEX + ( TAU12*TAU12 )/( S*S )
      GO TO 80
C
C
C     H O F F M A N  F A I L U R E  T H E O R Y
C     =========================================
C
   30 FINDEX = ( 1.0/XT - 1.0/XC )*SIG1
      FINDEX = FINDEX + ( 1.0/YT - 1.0/YC )*SIG2
      FINDEX = FINDEX + ( SIG1*SIG1 )/( XT*XC )
      FINDEX = FINDEX + ( SIG2*SIG2 )/( YT*YC )
      FINDEX = FINDEX + ( TAU12*TAU12 )/( S*S )
      FINDEX = FINDEX - ( SIG1*SIG2 )/( XT*XC )
      GO TO 80
C
C
C     T S A I-W U  F A I L U R E  T H E O R Y
C     =======================================
C
C**** CHECK STABILITY CRITERIA FOR THE INTERACTION TERM F12
   40 IF (F12 .EQ. 0.0) GO TO 50
C
      CRIT = ( 1.0/(XT*XC) )*( 1.0/(YT*YC) ) - ( F12*F12 )
      IF (CRIT .GT. 0.0) GO TO 50
C
C**** IF STABILITY CRITERIA IS VIOLATED THEN SET THE
C     F12 THE INTERACTION TERM TO ZERO
C
      F12 = 0.0
C
C
   50 FINDEX = ( 1.0/XT - 1.0/XC )*SIG1
      FINDEX = FINDEX + ( 1.0/YT - 1.0/YC )*SIG2
      FINDEX = FINDEX + ( SIG1*SIG1 )/( XT*XC )
      FINDEX = FINDEX + ( SIG2*SIG2 )/( YT*YC )
      FINDEX = FINDEX + ( TAU12*TAU12 )/( S*S )
      IF (F12 .EQ. 0.0) GO TO 80
      FINDEX = FINDEX + ( 2.0*F12*SIG1*SIG2 )
      GO TO 80
C
C
C     M A X  S T R E S S  F A I L U R E  T H E O R Y
C     ==============================================
C
   60 FI1 = SIG1/XT
      IF (SIG1 .LT. 0.0) FI1 = SIG1/XC
C
      FI2 = SIG2/YT
      IF (SIG2 .LT. 0.0) FI2 = SIG2/YC
C
      FI12 = ABS(TAU12)/S
C
      FINDEX = FI1
      IF (FI2  .GT. FINDEX) FINDEX = FI2
      IF (FI12 .GT. FINDEX) FINDEX = FI12
      GO TO 80
C
C
C     M A X  S T R A I N  F A I L U R E  T H E O R Y
C     ==============================================
C
   70 FI1 = EPS1/XT
      IF (EPS1 .LT. 0.0) FI1 = EPS1/XC
C
      FI2 = EPS2/YT
      IF (EPS2 .LT. 0.0) FI2 = EPS2/YC
C
      FI12 = ABS(GAMA)/S
C
      FINDEX = FI1
      IF (FI2  .GT. FINDEX) FINDEX = FI2
      IF (FI12 .GT. FINDEX) FINDEX = FI12
C
   80 CONTINUE
C
      RETURN
C
C
C     NON-FATAL ERROR
C
C
   90 FINDEX = 0.0
      RETURN
      END
