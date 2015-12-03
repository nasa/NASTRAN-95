      SUBROUTINE SBAR2( TI )
C******
C THIS ROUTINE IS THE PHASE II SUBROUTINE OF STRESS DATA RECOVERY FOR
C THE BEAM ELEMENT.
C******
C
      REAL    I1       ,I2       ,L        ,M1A      ,M2A      ,M1B
     1,       M2B      ,I12      ,K1A      ,K2A      ,K1B      ,K2B
     2,       TI(14)    ,FRLAST(2)
      INTEGER TLOADS   ,EJECT    ,ISHED(7)
C
C
      COMMON   /SYSTEM/  IBFSZ    ,NOUT     ,IDM(9)   ,LINE
C
C SDR2 VARIABLE CORE
C
      COMMON   /ZZZZZZ/  ZZ(1)
C
C BLOCK FOR POINTERS, LOADING TEMPERATURE AND ELEMENT DEFORMATION.
C
      COMMON   /SDR2X4/
     1                   XXXXXX(33)         ,ICSTM
     2,                  NCSTM              ,IVEC
     3,                  IVECN              ,LDTEMP
     4,                  ELDEFM             ,DUM8(8),TLOADS
C
C THE FIRST 100 LOCATIONS OF THE SDR2X7 BLOCK ARE RESERVED FOR INPUT
C PARAMETERS, THE SECOND 100 FOR STRESS OUTPUT PARAMETERS, AND FORCE
C OUTPUT PARAMETERS BEGIN AT LOCATION 201.
C
      COMMON   /SDR2X7/
     1                   JELID              ,JSILNO(2)
     2,                  SA(36)             ,SB(36)
     3,                  ST                 ,SDELTA
     4,                  A                  ,FJ
     5,                  I1                 ,I2
     6,                  I12                ,C1
     7,                  C2                 ,D1
     8,                  D2                 ,F1
     9,                  F2                 ,G1
     T,                  G2                 ,T SUB 0
     1,                  SIGMAT             ,SIGMAC
     2,                  L                  ,THERM(6)
C
C     THERM ACTUALLY HAS 30 VALUES
C
      COMMON   /SDR2X7/
     1                   ISELID             ,SIG1A
     2,                  SIG2A              ,SIG3A
     3,                  SIG4A              ,SIGAX
     4,                  SIGAMX             ,SIGAMN
     5,                  MSTEN              ,SIG1B
     6,                  SIG2B              ,SIG3B
     7,                  SIG4B              ,SIGBMX
     8,                  SIGBMN             ,MSCOM
     9,                  YYYYYY(84)
      COMMON   /SDR2X7/
     1                   IFELID             ,M1A
     2,                  M2A                ,M1B
     3,                  M2B                ,V1
     4,                  V2                 ,FX
     5,                  T
C
C SDR2 SCRATCH BLOCK
C
      COMMON   /SDR2X8/
     1                   FA(6)              ,FB(6)
     2,                  IDISP              ,IUA
     3,                  IUB                ,P1
     4,                  K1A                ,K2A
     5,                  K1B                ,K2B
     6,                  Q                  ,W
     7,                  CFA(6)             ,CFB(6)
     8,                  CFRVEC(10)         ,FRVEC(10)
C
C  STRESS/FORCE PRECISION CHECK
C
      COMMON   /SDR2X9/
     1                   NCHK               ,ISUB
     2,                  ILD                ,FRTMEI(2)
     3,                  TWOTOP             ,FNCHK
C
      EQUIVALENCE
     1                   (LDTEMP,TEMPLD)    ,(MSTEN,SMTEN)
     2,                  (MSCOM,SMCOM)      ,(ISHED(6),FRLAST(1))
     3,                  (IEID,CFRVEC(1))
     4,                  (ISHED(1),LSUB)    ,(ISHED(2),LLD)
C
      DATA LLD, LSUB, FRLAST / 2*-100, -1.0E30, -1.0E30 /
C
      IDISP = IVEC - 1
      IUA = IDISP + JSILNO(1)
      CALL SMMATS (SA(1),6,6,0, ZZ(IUA),6,1,0,  FA,CFA )
      IUB = IDISP + JSILNO(2)
      CALL SMMATS (SB(1),6,6,0, ZZ(IUB),6,1,0,  FB,CFB )
      P1  =  FA(1) + FB(1)
      V1  = -FA(2) - FB(2)
      V2  = -FA(3) - FB(3)
      T   = -FA(4) - FB(4)
      M2A =  FA(5) + FB(5)
      M1A = -FA(6) - FB(6)
      FX  = -P1 - SDELTA * ELDEFM
      CFRVEC(2) = CFA(6) + CFB(6)
      CFRVEC(3) = CFA(5) + CFB(5)
      CFRVEC(9) = CFA(4) + CFB(4)
      CFRVEC(7) = CFA(3) + CFB(3)
      CFRVEC(6) = CFA(2) + CFB(2)
      CFRVEC(8) = CFA(1) + CFB(1)
C
C IF LDTEMP = -1, THE LOADING TEMPERATURE IS UNDEFINED
C
      IF( TLOADS .EQ. 0 ) GO TO 10
      TSAVE = TI(2)
      TI(2) = (TI(1) + TI(2))/2.0  -  TSUB0
      CALL GMMATS( THERM,6,5,0,  TI(2),5,1,0,  FA(1) )
      TI(2) = TSAVE
      FX = FX - FA(1)
      V1 = V1 - FA(2)
      V2 = V2 - FA(3)
      T  = T - FA(4)
      M2A = M2A + FA(5)
      M1A = M1A - FA(6)
   10 M1B = M1A - V1*L
      M2B = M2A - V2*L
      CFRVEC(4) = CFRVEC(2) + CFRVEC(6) * L
      CFRVEC(5) = CFRVEC(3) + CFRVEC(7) * L
      FRVEC(2) = M1A
      FRVEC(3) = M2A
      FRVEC(4) = M1B
      FRVEC(5) = M2B
      FRVEC(6) = V1
      FRVEC(7) = V2
      FRVEC(8) = FX
      FRVEC(9) = T
C*****
C COMPUTE ELEMENT STRESSES AT 4 POINTS
C*****
C
C COMPUTE K1A AND K2A
C
      IF (I12 .NE. 0.0) GO TO 30
      IF (I1 .NE. 0.0) GO TO 20
      K1A = 0.0
      GO TO 40
   20 K1A = -M1A / I1
      GO TO 40
   30 K1A = (M2A * I12  -  M1A * I2) / (I1 * I2  -  I12**2)
      K2A = (M1A * I12  -  M2A * I1) / (I1 * I2  -  I12**2)
      GO TO 60
   40 IF (I2 .NE. 0.0) GO TO 50
      K2A = 0.0
      GO TO 60
   50 K2A = -M2A / I2
C
C COMPUTE SIG1A, SIG2A, SIG3A AND SIG4A
C
   60 SIG1A = K1A * C1  +  K2A * C2
      SIG2A = K1A * D1  +  K2A * D2
      SIG3A = K1A * F1  +  K2A * F2
      SIG4A = K1A * G1  +  K2A * G2
C
C COMPUTE K1B AND K2B
C
      IF (I12 .NE. 0.0) GO TO 80
      IF (I1 .NE. 0.0) GO TO 70
      K1B = 0.0
      GO TO 90
   70 K1B = -M1B / I1
      GO TO 90
   80 K1B = (M2B * I12  -  M1B * I2) / (I1 * I2  -  I12**2)
      K2B = (M1B * I12  -  M2B * I1) / (I1 * I2  -  I12**2)
      GO TO 110
   90 IF (I2 .NE. 0.0) GO TO 100
      K2B = 0.0
      GO TO 110
  100 K2B = -M2B / I2
C
C COMPUTE SIG1B, SIG2B, SIG3B AND SIG4B
C
  110 SIG1B = K1B * C1  +  K2B * C2
      SIG2B = K1B * D1  +  K2B * D2
      SIG3B = K1B * F1  +  K2B * F2
      SIG4B = K1B * G1  +  K2B * G2
      IF( TLOADS .EQ. 0 ) GO TO 115
C
C     TEST IF AT LEAST ONE POINT TEMPERATURE IS GIVEN
C
      DO 111 I = 7,14
      IF( TI(I) .NE. 0.0 ) GO TO 112
  111 CONTINUE
      GO TO 115
  112 IF( A .EQ. 0.0 ) GO TO 115
      EALF =-ST / A
      SIG1A = SIG1A + EALF*(TI(7) - TI(3)*C1 - TI(5)*C2 - TI(1))
      SIG2A = SIG2A + EALF*(TI(8) - TI(3)*D1 - TI(5)*D2 - TI(1))
      SIG3A = SIG3A + EALF*(TI(9) - TI(3)*F1 - TI(5)*F2 - TI(1))
      SIG4A = SIG4A + EALF*(TI(10) - TI(3)*G1 - TI(5)*G2 - TI(1))
      SIG1B = SIG1B + EALF*(TI(11) - TI(4)*C1 - TI(6)*C2 - TI(2))
      SIG2B = SIG2B + EALF*(TI(12) - TI(4)*D1 - TI(6)*D2 - TI(2))
      SIG3B = SIG3B + EALF*(TI(13) - TI(4)*F1 - TI(6)*F2 - TI(2))
      SIG4B = SIG4B + EALF*(TI(14) - TI(4)*G1 - TI(6)*G2 - TI(2))
  115 CONTINUE
C
C COMPUTE AXIAL STRESS
C
      CFRVEC(10) = 0.0
      SIGAX = 0.0
      IF (A .NE. 0.0) SIGAX = FX / A
      IF (A.NE.0.0) CFRVEC(10) = CFRVEC(8) / A
      FRVEC(10) = SIGAX
C
C COMPUTE MAXIMA AND MINIMA
C
      SIGAMX = SIGAX + AMAX1(SIG1A,SIG2A,SIG3A,SIG4A)
      SIGBMX = SIGAX + AMAX1(SIG1B,SIG2B,SIG3B,SIG4B)
      SIGAMN = SIGAX + AMIN1(SIG1A,SIG2A,SIG3A,SIG4A)
      SIGBMN = SIGAX + AMIN1(SIG1B,SIG2B,SIG3B,SIG4B)
C
C COMPUTE MARGIN OF SAFETY IN TENSION
C
      IF(SIGMAT.LE.0.0)GO TO 620
      IF(AMAX1(SIGAMX,SIGBMX).LE.0.0) GO TO 620
      Q=SIGMAT/AMAX1(SIGAMX,SIGBMX)
      SMTEN=Q-1.0
      GO TO 630
  620 MSTEN=1
C
C      COMPUTE MARGIN OF SAFETY IN COMPRESSION
C
  630 IF(SIGMAC .LE. 0.0) GO TO 640
      IF(AMIN1(SIGAMN,SIGBMN).GE.0.0) GO TO 640
      W = -SIGMAC/AMIN1(SIGAMN,SIGBMN)
      SMCOM=W-1.0
      GO TO 150
  640 MSCOM=1
  150 ISELID = JELID
      IFELID = JELID
C
C  . STRESS CHECK...
C
      IF (NCHK.LE.0) GO TO 230
      IEID = JELID
      K = 0
      CALL SDRCHK (FRVEC(2),CFRVEC(2),9,K)
C
      IF (K.EQ.0) GO TO 230
C
C  . LIMITS EXCEEDED...
      J = 0
      IF (LSUB.EQ.ISUB .AND. FRLAST(1).EQ.FRTMEI(1) .AND.
     1    LLD .EQ.ILD  .AND. FRLAST(2).EQ.FRTMEI(2) ) GO TO 200
      LSUB = ISUB
      LLD = ILD
      FRLAST(1) = FRTMEI(1)
      FRLAST(2) = FRTMEI(2)
      J = 1
      CALL PAGE1
  180 CALL SD2RHD (ISHED,J)
      LINE = LINE + 1
      WRITE(NOUT,190)
  190 FORMAT(7X,47HTYPE     EID    M1A    M2A    M1B    M2B     V1,5X,
     1 23HV2     FA      T     SA)
      GO TO 210
C
  200 IF (EJECT(2).NE.0) GO TO 180
  210 WRITE(NOUT,220) IEID,(CFRVEC(I),I=2,10)
  220 FORMAT (1H0,7X,3HBAR,I8,9F7.1)
C
  230 CONTINUE
      RETURN
      END
