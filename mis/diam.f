      SUBROUTINE DIAM (NC,MAXDEG,NL,NODESL,IDEM,MAXLEV,IG,IC,IDEG,
     1                 IDIS,IW,ICC,JG)
C
C     DETERMINE NL STARTING POINTS AND STORE IN NODESL.
C     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
C
      DIMENSION       IG(1),    IDIS(1),  IW(1),    ICC(1),   IC(1),
     1                JG(1),    IDEG(1),  NODESL(1)
      COMMON /BANDS / NN
C
      NL = 0
      MAXLEV = 600000
      DO 100 I = 1,NN
      IF (NC-IC(I)) 100,40,100
   40 IF (MAXDEG-IDEG(I)) 100,50,50
   50 MD = IDIST(I,ML,MAXLEV,IG,IC,IDEG,IDIS,IW,ICC,JG)
      IF (MD) 120,120,60
   60 IF (ML-MAXLEV) 70,80,100
   70 MAXLEV = ML
      NL = 1
      NODESL(1) = I
      GO TO 100
   80 IF (NL .GE. IDEM) GO TO 100
      NL = NL + 1
      NODESL(NL) = I
  100 CONTINUE
      RETURN
C
  120 ML = 1
      NODESL(1) = I
      MAXLEV = 0
      RETURN
      END
