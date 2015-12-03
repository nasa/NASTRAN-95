      SUBROUTINE LSPLIN(NI,XYI,ND,XYD,KY,KD,KT,DZ,DTX,DTY,DTOR,
     *           G,NCORE,ISNG)
      LOGICAL IKT
      LOGICAL BNONE,BONE
      LOGICAL SPEC
      LOGICAL OXR,OYR
      LOGICAL BOTH
      LOGICAL STWO,SONE,NETHR
      INTEGER SIZE
C
      DIMENSION XYI(1),XYD(1),G(1),NAME(2)
      DATA NAME/4HLSPL,4HIN  /
      SPEC = .FALSE.
      BONE = .FALSE.
      BNONE = .FALSE.
      SONE = .FALSE.
      STWO = .FALSE.
      OXR = .FALSE.
      OYR = .FALSE.
      BOTH = .FALSE.
      IKT = .FALSE.
      NETHR = .TRUE.
      EY = FLOAT(KY)
C
C     KY EFFECTS RIGID BODY ROWS AND COLUMNS OF A AND ROWS OF B
C
C     DTX AND DTY EFFECT ROWS AND COLUMNS OF A AND ROWS OF B
C
C     KD EFFECTS COLUMNS OF B
C
C     SPEC GUARDS AGAINST SINGULAR MATRIX FOR ROTATIONS WITHOUT Y
C     CONSTRAINED
C
      IF(KD .EQ. 0) BNONE = .TRUE.
      IF(KD .EQ. 1) BONE = .TRUE.
      IF(KY.EQ.-1) SONE = .TRUE.
      IF(KY.EQ.1) STWO = .TRUE.
      IF(KT.EQ.1) IKT = .TRUE.
      IF(DTX.LT.0.0) OXR = .TRUE.
      IF(DTY.LT.0.0) OYR = .TRUE.
      IF(OXR.AND.OYR) BOTH = .TRUE.
      IF(.NOT.OXR.AND..NOT.OYR) NETHR = .FALSE.
      DTOR2 =  DTOR/2.0
      NSC = 3
      IF(KY.EQ.1) NSC = 2
      IF(KY.EQ.-1) NSC = 1
      SIZE = 3
      IF(OXR) SIZE = SIZE-1
      IF(OYR) SIZE = SIZE-1
      IF(OYR.AND. KY.GT.-1) GO TO 5
      GO TO 7
    5 TEMP = XYI(1)
      SPEC = .TRUE.
      NII = 2*NI
      DO 6 I = 1,NII,2
      IF(XYI(I) .NE. TEMP) SPEC = .FALSE.
    6 CONTINUE
    7 CONTINUE
      NCA = SIZE*NI + NSC
      IF(SPEC) NCA = NCA -1
      NCA2= 2*NCA
      NCB = (KD+1) * ND
      NCC = SIZE*NI
C
C     CORE NEEDED
C                A         G        INVERS
      NEEDED =NCA*NCA+  NCB*NCC  +    3*NCA
C                                   B
      IF(IKT) NEEDED = NEEDED +  NCB*NCA
C                                        C
      IF(.NOT.IKT) NEEDED = NEEDED +  NCC*NCA
      IF(NEEDED .GT.NCORE) CALL MESAGE(-8,0,NAME)
      IS= NCORE -3*NCA-1
      IG= 1
C
C     IF KT = 1  COMPUTE  B THEN A  THEN  C IN THE SPACE OF A
C
C     IF KT = 0 COMPUTE  C THEN A  THEN  B IN THE SPACE OF A
C
      IF(IKT) GO TO 100
C
C     FILL IN C MATRIX
C
      IC = NCB * NCC
      MP = IC +1
   10 DO 30 I = 1,NCC
      DO 20 J = 1,NCA
      IC = IC +1
      G(IC) = 0.0
      IF(I.EQ.J) G(IC) = 1.0
   20 CONTINUE
   30 CONTINUE
      IF(IKT) GO TO 300
      NC = NCC
      IA =  IC
      GO TO 200
C
C     B MATRIX
C
  100 IB =  NCB * NCC
      MP = IB + 1
  110 NJ = 2*ND
      NII= 2*NI
      DO 130 J=1,NJ,2
      DO 120 I=1,NII,2
      YM = XYD(J+1) - XYI(I+1)
      AYM= ABS(YM)
      AYMD = AYM*DTOR2
      YP =(XYD(J+1) + XYI(I+1))
      AYP= ABS(YP) * EY
      AYPD = AYP*DTOR2
      IB= IB +1
      G(IB) =    AYM**3/12.0  -  XYD(J)*XYI(I)*AYMD
     *           + AYP**3/12.0 - XYD(J)*XYI(I)*AYPD
      IF(BNONE) GO TO 111
      G(IB+NCA) = AYM*YM/4.0  + AYP*YP/4.0
      IF(BONE) GO TO 111
      G(IB+NCA2)=  XYI(I)*AYMD  + XYI(I)*AYPD
  111 IF(BOTH) GO TO 120
      IF(OXR) GO TO 113
      IB=IB+1
      G(IB) =    -AYM*YM/4.0  + AYP*YP/4.0
      IF(BNONE) GO TO 112
      G(IB+NCA) = -AYM/2.0 + AYP/2.0
      IF(BONE) GO TO 112
      G(IB+NCA2) = 0.0
  112 IF(OYR) GO TO 120
  113 IB = IB +1
      G(IB)=   XYD(J)*AYMD   + XYD(J)*AYPD
      IF(BNONE) GO TO 120
      G(IB+NCA)  =  0.0
      IF(BONE) GO TO 120
      G(IB+NCA2) =  -AYMD   - AYPD
  120 CONTINUE
      IB = IB +1
      IF(SONE) GO TO 123
      G(IB) = 1.0
      IF(BNONE) GO TO 121
      G(IB+NCA) = 0.0
      IF(BONE) GO TO 121
      G(IB+NCA2) = 0.0
  121 IF(STWO) GO TO 122
      IB = IB +1
      G(IB) = XYD(J+1)
      IF(BNONE) GO TO 122
      G(IB+NCA) = 1.0
      IF(BONE) GO TO 122
      G(IB+NCA2) = 0.0
  122 IF(SPEC) GO TO 128
      IB = IB +1
      G(IB) =-XYD(J)
      IF(BNONE) GO TO 128
      G(IB+NCA) = 0.0
      IF(BONE) GO TO 128
      G(IB+NCA2) = 1.0
      GO TO 128
  123 G(IB) = XYD(J+1)
      IF(BNONE) GO TO 128
      G(IB+NCA) = 1.0
      IF(BONE) GO TO 128
      G(IB+NCA2) = 0.0
  128 IB = IB + KD*NCA
  130 CONTINUE
      IF(.NOT.IKT) GO TO 400
      IA = IB
      NC = NCB
C
C     A MATRIX
C
  200 NII= 2*NI
      K = IA
C
C     ZERO A
C
      II = K+1
      IK = II + NCA*NCA
      DO 210 I = II,IK
  210 G(I) = 0.0
      II = 0
      DO 240 I = 1,NII,2
      DO 230 J = I,NII,2
      K = K+1
      YP   =(XYI(I+1) + XYI(J+1))
      AYP  = ABS(YP) * EY
      AYPD = AYP*DTOR2
      YM   = XYI(I+1) - XYI(J+1)
      AYM  = ABS(YM)
      AYMD = AYM*DTOR2
      G(K)       = AYM**3/12.0 - XYI(I)*XYI(J)*AYMD
     *              + AYP**3/12.0 - XYI(I)*XYI(J)*AYPD
      IF(I.EQ.J) G(K) = G(K) + DZ
      IF(BOTH) GO TO 230
      IF(OXR) GO TO 212
      G(K+NCA)  = AYM*YM/4.0  + AYP*YP/4.0
      IF(OYR) GO TO 214
      G(K+NCA2)  = XYI(I)*AYMD  + XYI(I)*AYPD
      K = K+1
      G(K)  =    -AYM*YM/4.0  + AYP*YP/4.0
      G(K+NCA)  = -AYM/2.0 + AYP/2.0
      IF(I.EQ.J) G(K+NCA) = G(K+NCA) + DTX
      G(K+NCA2)  = 0.0
      K = K+1
      G(K)       = XYI(J)*AYMD  + XYI(J)*AYPD
      G(K+NCA)   = 0.0
      G(K+NCA2) = -AYMD  - AYPD
      IF(I.EQ.J) G(K+NCA2) = G(K+NCA2) + DTY
      GO TO 230
  212 G(K+NCA) = XYI(I)*AYMD  + XYI(I)*AYPD
      K = K+1
      G(K) = XYI(J)*AYMD  + XYI(J)*AYPD
      G(K+NCA) = -AYMD   - AYPD
      IF(I.EQ.J) G(K+NCA) = G(K+NCA) + DTY
      GO TO 230
  214 K = K+1
      G(K)  =    -AYM*YM/4.0  + AYP*YP/4.0
      G(K+NCA)  = -AYM/2.0 + AYP/2.0
      IF(I.EQ.J) G(K+NCA) = G(K+NCA) + DTX
  230 CONTINUE
      K = K+1
      IF(SONE) GO TO 234
      G(K) = 1.0
      IF(BOTH) GO TO 231
      G(K+NCA) = 0.0
      IF(NETHR) GO TO 231
      G(K+NCA2) = 0.0
  231 IF(STWO) GO TO 232
      K = K+1
      G(K) = XYI(I+1)
      IF(BOTH) GO TO 232
      IF(OXR) G(K+NCA) = 0.0
      IF(OYR) G(K+NCA)  = 1.0
      IF(NETHR) GO TO 232
      G(K+NCA) = 1.0
      G(K+NCA2) = 0.0
  232 IF(SPEC) GO TO 238
      K = K+1
      G(K) = -XYI(I)
      IF(BOTH) GO TO 238
      IF(OXR) G( K+NCA) = 1.0
      IF(OYR) G( K+NCA) = 0.0
      IF(NETHR) GO TO 238
      G( K+NCA) = 0.0
      G( K+NCA2) = 1.0
      GO TO 238
  234 G(K) = XYI(I+1)
      IF(BOTH) GO TO 238
      IF(OXR) G( K+NCA) = 0.0
      IF(OYR) G( K+NCA) = 1.0
      IF(NETHR) GO TO 238
      G( K+NCA) = 1.0
      G( K+NCA2) = 0.0
  238 II = II+1
      K = K + SIZE*II + (SIZE-1)*NCA
  240 CONTINUE
C
C     LOWER TRIANGLE IF A STORED TRANSPOSE INTO UPPER TRIANGLE
C
      K = IA
      DO 260 I = 1,NCA
      DO 250 J = I,NCA
      K = K+1
      KK = K + (NCA-1)*(J-I)
      G(KK) = G(K)
  250 CONTINUE
      K = K+I
  260 CONTINUE
C
C     CALL  INVERSE   A-1 C  OR  A-1 B
C
C     REPLACE CALLS TO INVAER WITH CALLS TO INVERS.
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      ISNG = -1
      CALL INVERS(NCA,G(IA+1),NCA,G(MP),NC,DET,ISNG,G(IS))
      IF(ISNG.EQ.2) GO TO 1000
C
C     ADJUST INDEXES TO A AND A-1 RESULT
C
      IB = IA
      ICB= IB+1
      IF(.NOT.IKT) GO TO 110
      IC = IA
      ICC = IC +1
      GO TO 10
  300 CALL GMMATS(G(MP),NCB,NCA,0,G(ICC),NCC,NCA,1,G(IG))
      GO TO 1000
  400 CALL GMMATS(G(MP),NCC,NCA,0,G(ICB),NCB,NCA,1,G(IG))
 1000 RETURN
      END
