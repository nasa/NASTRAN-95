      SUBROUTINE DZY (X,Y,Z,SGR,CGR,XI1,XI2,ETA,ZETA,AR,AO,KR,CBAR,
     1                BETA,FMACH,LSH,IDZDY,DZDYR,DZDYI)
C
C     CALCULATION OF THE DZ AND DY MATRICES USED IN SLENDER BODY FLOW
C
C     X        X- COORDINATE OF THE RECEIVING POINT
C     Y        Y - COORDINATE OF THE RECEIVING POINT
C     Z        Z - COORDINATE OF THE RECEIVING POINT
C     SGR      SINE OF THE RECEIVING POINT DIHEDRAL ANGLE
C     CGR      COSINE OF RECEIVING POINT DIHEDRAL ANGLE
C     XI1
C     XI2
C     ETA
C     ZETA
C     AR       ASPECT RATIO OF THE SENDING BODY
C     A0       RADIUS OF THE SENDING BODY
C     KR       REDUCED FREQUENCY
C     CBAR     REFERENCE CHORD LENGTH
C     BETA     SQRT(1.0-M**2)
C     FMACH    MACH NUMBER
C     IDZDY    FLAG INDICATING WHETHER DZ OF DY IS TO BE
C              CALCULATED.  =0  DZ, OTHERWISE DY
C     DZDYR    REAL PART OF DZ OR DY
C     DZDYI    IMAGINARY PART OF DZ OR DY
C
C
      REAL   KD1PR, KD1PI, KD2PR, KD2PI, KD1MR, KD1MI, KD2MR, KD2MI
      DATA   PI16 / 50.265482 /
C
C
C     THE COMPLEX NUMBERS IN THIS ROUTINE ARE TREATED SEPERATLY AS
C     THE REAL PART,  NAME APPENDED BY AN  -R- ,  AND THE
C     IMAGINARY PART, NAME APPENDED BY AN  -I- .
C
      E   = AO*SQRT(ABS(1.0 - AR**2))/2.0
      X01 = X - XI1
      X02 = X - XI2
C
C     CHECK ON INPUT FLAG,  = 0  DZ ,  = 1  DY
C
      IF (IDZDY .EQ. 1) GO TO 200
C
C     **     **
C     *  D Z  *
C     **     **
C
      SGS = 0.0
      CGS = 1.0
      IF (AR .LT. 1.0)  GO TO 400
C
      Z01 = Z - (ZETA+E)
      Z02 = Z - (ZETA-E)
      Y01 = Y - ETA
      Y02 = Y01
      GO TO 300
C
C     **     **
C     *  D Y  *
C     **     **
C
  200 SGS = -1.0
      IF (LSH .EQ. 1) SGS = 1.0
      CGS =  0.0
      IF (AR .GT. 1.0) GO TO 400
C
      Z01 = Z - ZETA
      Z02 = Z01
      Y01 = Y - (ETA+E)
      Y02 = Y - (ETA-E)
C
C     ****  DZ AR .GE. 1  ****
C     ****  DY AR .LE. 1  ****
C
  300 CONTINUE
      L   = 0
      Z0  = Z - ZETA
      Y0  = Y - ETA
C
      R1SQR = Y01**2 + Z01**2
      R2SQR = Y02**2 + Z02**2
      R1FOR = R1SQR**2
      R2FOR = R2SQR**2
C
      CALL FLLD (X01,X02,Y01,Z01,SGR,CGR,SGS,CGS,KR,CBAR,FMACH,E,L,
     1           KD1PR,KD1PI,KD2PR,KD2PI)
C
      IF (AR .NE. 1.0)  GO TO 320
C
C     IDENTICAL RESULTS FROM FLLD, THEREFORE SKIP SECOND CALL
C
      KD1MR = KD1PR
      KD1MI = KD1PI
      KD2MR = KD2PR
      KD2MI = KD2PI
      GO TO 360
  320 CONTINUE
      CALL FLLD (X01,X02,Y02,Z02,SGR,CGR,SGS,CGS,KR,CBAR,FMACH,E,L,
     1           KD1MR,KD1MI,KD2MR,KD2MI)
  360 CONTINUE
      DZDYR = 0.0
      DZDYI = 0.0
      IF (R1SQR.LE.0.0001. OR .R2SQR.LE.0.0001) GO TO 370
C
C     REAL
C
      DZDYR = ((KD1PR/R1SQR+KD1MR/R2SQR) + (KD2PR/R1FOR+KD2MR/R2FOR))
     1        /PI16*(-1.0)
C
C     IMAGINARY
C
      DZDYI = ((KD1PI/R1SQR+KD1MI/R2SQR) + (KD2PI/R1FOR+KD2MI/R2FOR))
     1        /PI16*(-1.0)
  370 CONTINUE
C
      RETURN
C
C     ****   DZ-AR .LT. 1   ****
C     ****   DY-AR .GT. 1   ****
C
  400 SL1 = 0.0
      TL1 = 0.0
      SL2 = 0.0
      TL2 = 0.0
      CL1 = 1.0
      CL2 = 1.0
      E   = 1.732051*E
      Y0  = Y - ETA
      Z0  = Z - ZETA
C
      CALL TVOR (SL1,CL1,TL1,SL2,CL2,TL2,SGS,CGS,SGR,CGR,X01,X02,
     1           Y0,Z0,E,BETA,CBAR,FMACH,KR,DZDYR,DZDYI)
      RETURN
      END
