      SUBROUTINE RELABL (NS,NODES,IG,IC,IDEG,IDIS,IW,NEW,ICC,ILD,IAJ,
     1                   JG,IDIM)
C
C     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
C
C     GENERATE A RELABELING SCHEME STARTING WITH NS NODES FOR WHICH
C     LABELS HAVE BEEN STORED IN ARRAY NODES.
C     SET UP ILD AND NEW.
C     ILD(OLD) = NEW
C     NEW(NEW) = OLD, THE INVERSE OF ILD
C     IAJ IS DIMENSIONED TO IDIM
C
      INTEGER         X
      DIMENSION       IG(1),    IC(1),    IDEG(1),  IDIS(1),  IW(1),
     1                NEW(1),   ICC(1),   ILD(1),   NODES(1), IAJ(1),
     2                JG(1)
      COMMON /BANDS / NN,       DUMS(3),  MAXGRD
      COMMON /SYSTEM/ IBUF,     NOUT
C
      I   = NODES(1)
      ICN = IC(I)
      NT  = ICC(ICN) - 1
      DO 90 I = 1,NN
      IF (IC(I)-ICN) 90,80,90
 80   IDIS(I) = 0
 90   CONTINUE
      DO 100 J = 1,NS
      JJ = NODES(J)
      IDIS(JJ) =-1
      JT = J + NT
      NEW(JT) = JJ
 100  ILD(JJ) = JT
      KI = NT
      KO = NS + NT
      LL = KO
      L  = 1
      J  = KO
      NNC= ICC(ICN+1) - 1
 110  KI = KI + 1
      IF (KI-LL) 130,120,130
 120  L  = L  + 1
      LL = KO + 1
 130  II = NEW(KI)
      N  = IDEG(II)
      IF (N) 140,270,140
 140  IJ = 0
      CALL BUNPAK (IG,II,N,JG)
      DO 170 I = 1,N
      IA = JG(I)
      IF (IDIS(IA)) 170,150,170
 150  IJ = IJ + 1
      IF (IJ .LE. IDIM) GO TO 160
C
C     DIMENSION EXCEEDED.  STOP JOB.
C
      NGRID = -2
      RETURN
C
 160  IDIS(IA) = L
      KO       = KO + 1
      IAJ(IJ)  = IA
      IW(IJ)   = IDEG(IA)
 170  CONTINUE
      IF (IJ-1) 260,180,190
 180  J  = KO
      IZ = IAJ(1)
      NEW(KO) = IZ
      ILD(IZ) = KO
      GO TO 260
 190  X = 0
      DO 230 I = 2,IJ
      IF (IW(I)-IW(I-1)) 210,230,230
 210  CONTINUE
      X = IW(I)
      IW(I  ) = IW(I-1)
      IW(I-1) = X
      X = IAJ(I)
      IAJ(I  ) = IAJ(I-1)
      IAJ(I-1) = X
 230  CONTINUE
      IF (X) 240,240,190
 240  DO 250 I = 1,IJ
      J  = J + 1
      IZ = IAJ(I)
      NEW(J ) = IZ
      ILD(IZ) = J
 250  CONTINUE
 260  IF (KO-NNC) 110,270,270
 270  CONTINUE
C
C     REVERSE SEQUENCE FOR THIS COMPONENT (ICN).
C
C     ICC IS AN ARRAY USED FOR IDENTIFYING COMPONENTS IN THE NEW ARRAY.
C     ICC(N1) CONTAINS THE INDEX FOR THE NEW ARRAY AT WHICH COMPONENT
C         N1 STARTS.
C
      N1 = ICC(ICN) - 1
      N2 = NN - ICC(ICN+1) + 1
      IF (N2 .GT. NN) N2 = 0
C
C     REVERSE THE NODAL CM SEQUENCE, OMITTING THE FIRST N1 AND THE LAST
C     N2 POINTS.
C
C     NEW(N1) = OLD LABEL FOR NODE NOW LABELLED N1.
C     ILD(N1) = NEW LABEL FOR NODE ORIGINALLY LABELED N1.
C     N1      = NUMBER OF POINTS AT BEGINNING OF SEQUENCE TO OMIT FROM
C               REVERSAL.
C     N2      = NUMBER OF POINTS AT END OF SEQUENCE TO OMIT FROM
C               REVERSAL.
C     NN      = NUMBER OF NODES.
C     J       = NUMBER OF INTERCHANGES TO MAKE.
C
      J  = (NN-N1-N2)/2
      IF (J .LE. 0) RETURN
      LL = NN - N2 + 1
C
C     MAKE INTERCHANGES IN NEW ARRAY.
C
      DO 290 I = 1,J
      L = LL - I
      K = NEW(L)
      M = N1 + I
      NEW(L) = NEW(M)
 290  NEW(M) = K
C
C     CORRECT ILD, THE INVERSE OF NEW.
C
      L = 1  + N1
      M = NN - N2
      DO 300 I = L,M
      K = NEW(I)
 300  ILD(K) = I
C
      RETURN
      END
