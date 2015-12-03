      FUNCTION IDIST (NS,ML,MAXLEV,IG,IC,IDEG,IDIS,IW,ICC,JG)
C
C     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
C
C     THIS FUNCTION HAS AS ITS VALUE THE MAXIMUM DISTANCE OF ANY NODE
C     IN COMPONENT IC(NS) FROM THE NODE NS.
C     THE DISTANCE OF EACH NODE IN THIS COMPONENT IS STORED IN THE ARRAY
C     IDIS.
C     THE MAXIMUM NUMBER OF NODES AT THE SAME DISTANCE FROM NS IS
C     STORED IN ML.
C
C     INTEGER          BUNPK
      DIMENSION        IC(1),    IDEG(1),  IDIS(1),  IW(1),    ICC(1),
     1                 IG(1),    JG(1)
      COMMON /BANDS /  NN
C
      ICN = IC(NS)
      NNC = ICC(ICN+1) - ICC(ICN)
      DO 50 I = 1,NN
      IF (IC(I)-IC(NS)) 50,40,50
   40 IDIS(I) = 0
   50 CONTINUE
      LL = 1
      L  = 0
      KI = 0
      KO = 1
      ML = 0
      IW(1) = NS
      IDIS(NS) = -1
  130 KI = KI + 1
      IF (KI-LL) 135,132,135
  132 L  = L + 1
      LL = KO + 1
      K  = KO - KI + 1
      IF (K-ML) 135,135,133
  133 ML = K
      IF (ML-MAXLEV) 135,135,220
  135 II = IW(KI)
      N  = IDEG(II)
      IF (N) 140,215,140
  140 CALL BUNPAK (IG,II,N,JG)
      DO 200 I = 1,N
      IA = JG(I)
      IF (IDIS(IA)) 200,150,200
  150 IDIS(IA) = L
      KO = KO + 1
      IW(KO) = IA
  200 CONTINUE
      IF (KO-NNC) 130,205,205
  205 IDIST = L
      IDIS(NS) = 0
      K = KO - LL + 1
      IF (K-ML) 206,206,207
  207 ML = K
  206 CONTINUE
      RETURN
C
  215 L = 0
      GO TO 205
  220 IDIST = 1
      RETURN
      END
