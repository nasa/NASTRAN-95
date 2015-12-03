      SUBROUTINE SROD2
C*****
C THIS ROUTINE IS PHASE II OF STRESS DATA RECOVERY FOR THE ROD.
C*****
      REAL    CFRVEC(4),FRLAST(2)
      INTEGER EJECT    ,ISHD(7)  ,TYP(4)
C
      COMMON   /SYSTEM/  IBFSZ    ,NOUT     ,IDM(9)   ,LINE
      COMMON /SDR2DE/ SKP2DE(8),IELTYP
C
C SDR2 VARIABLE CORE
C
      COMMON   /ZZZZZZ/  ZZ(1)
C
C BLOCK FOR POINTERS, LOADING TEMPERATURE AND ELEMENT DEFORMATION.
C
      COMMON   /SDR2X4/
     1                   DUMMY(33)          ,ICSTM
     2,                  NCSTM              ,IVEC
     3,                  IVECN              ,TEMPLD
     4,                  ELDEFM
C
C SDR2 INPUT AND OUTPUT BLOCK
C
      COMMON   /SDR2X7/
     1                   IELID              ,ISILNO(2)
     2,                  SAT(3)             ,SBT(3)
     3,                  SAR(3)             ,SBR(3)
     4,                  ST                 ,SDELTA
     5,                  AREA               ,FJOVRC
     6,                  T SUBC 0           ,SIGMAT
     7,                  SIGMAC             ,SIGMAS
     8,                  DUMMY2(77)
     9,                  JSELID             ,SIGMA
     T,                  SMSIG              ,TAU
     1,                  SMTAU              ,DUMMY3(95)
     2,                  JFELID             ,P
     3,                  TORQUE             ,DUMMY4(22)
C
C SCRATCH BLOCK
C
      COMMON   /SDR2X8/
     1                   TRANA              ,TRANB
     2,                  ROTA               ,ROTB
     3,                  IUTA               ,IUTB
     4,                  IURA               ,IURB
     5,                  IFRVEC(7)          ,CHKVEC(4)
C
      COMMON /SDR2X9/ NCHK,ISUB,ILD,FRTMEI(2),TWOTOP,FNCHK
C
      EQUIVALENCE
     1                   (TEMPLD,LDTEMP)    ,(SMSIG,MSSIG)
     2,                  (SMTAU,MSTAU)
     3,      (CFRVEC(1),CSIGA) , (CFRVEC(2),CTAU) , (CFRVEC(3),CP)
     4,      (CFRVEC(4),CTRQUE), (IFRVEC(4),CFRVEC(1))
     5,      (ISHD(1),LSUB), (ISHD(2),LLD), (ISHD(6),FRLAST(1))
C
      DATA LLD,LSUB,FRLAST / 2*-1, -1.0E30, -1.0E30 /
      DATA TYP / 4H CON , 4HROD  , 4HTUBE, 1H   /
C
      IDISP = IVEC - 1
      IUTA  = IDISP + ISILNO(1)
      CALL SMMATS (SAT(1),3,1,1, ZZ(IUTA),3,1,0, TRANA,CTRNA)
      IUTB  = IDISP + ISILNO(2)
      CALL SMMATS (SBT(1),3,1,1, ZZ(IUTB),3,1,0, TRANB,CTRNB)
      SIGMA = TRANA + TRANB + SDELTA * ELDEFM
      CSIGA = CTRNA + CTRNB
      IF (LDTEMP .EQ. (-1) ) GO TO 10
      SIGMA = SIGMA + ST * (TEMPLD - T SUBC 0)
   10 IURA  = IUTA + 3
      CHKVEC(1) = SIGMA
      CALL SMMATS (SAR(1),3,1,1, ZZ(IURA),3,1,0, ROTA,CRTA)
      IURB  = IUTB + 3
      CALL SMMATS (SBR(1),3,1,1, ZZ(IURB),3,1,0, ROTB,CRTB)
      TORQUE = ROTA + ROTB
      CP = AREA * CSIGA
      CHKVEC(3) = P
      CTAU = ABS (FJOVRC) * CTRQUE
      CHKVEC(2) = TAU
      CTRQUE = CRTA + CRTB
      CHKVEC(4) = TORQUE
C
C COMPUTE AXIAL FORCE, P, AND TORQUE
C
      P = AREA * SIGMA
      TAU = FJOVRC * TORQUE
C
C COMPUTE MARGIN OF SAFETY IN EXTENSION
C
      IF(SIGMA.LE.0.0)GO TO 101
      IF(SIGMAT.LE.0.0)GO TO 102
      SMSIG=SIGMAT/SIGMA-1.0
      GO TO 180
  101 IF(SIGMA.NE.0.0) GO TO 103
      GO TO 102
  103 IF(SIGMAC .LE. 0.0) GO TO 102
      SMSIG = -SIGMAC/SIGMA - 1.0
      GO TO 180
  102 MSSIG=1
C
C     COMPUTE MARGIN OF SAFETY IN TORSION
C
  180 IF(SIGMAS.LE.0.0) GO TO 190
      IF(TAU.EQ.0.0)GO TO 190
      SMTAU= SIGMAS/ABS(TAU) - 1.0
      GO TO 110
  190 MSTAU=1
  110 JSELID = IELID
      JFELID = IELID
      IF (NCHK .LE. 0 ) GO TO 260
C
C  . CHECK PRECISION...
C
      IFRVEC(3) = IELID
      K = 0
      CALL SDRCHK (CHKVEC,CFRVEC,4,K)
C
      IF (K.EQ.0) GO TO 260
C
C  . LIMITS EXCEEDED...
C
      J = 0
      IFRVEC(1) = TYP(4)
      IF (IELTYP.EQ.10) IFRVEC(1) = TYP(1)
      IFRVEC(2) = TYP(2)
      IF (IELTYP.EQ.3) IFRVEC(2) = TYP(3)
C
      IF (LSUB.EQ.ISUB .AND. FRLAST(1).EQ.FRTMEI(1) .AND.
     1    LLD .EQ.ILD  .AND. FRLAST(2).EQ.FRTMEI(2) ) GO TO 240
C
      LSUB = ISUB
      LLD = ILD
      FRLAST(1) = FRTMEI(1)
      FRLAST(2) = FRTMEI(2)
      J = 1
      CALL PAGE1
C
  220 CALL SD2RHD (ISHD,J)
      LINE = LINE + 1
      WRITE(NOUT,230)
  230 FORMAT(7X,4HTYPE,5X,3HEID,5X,2HSA,5X,2HST,5X,9HAF TORQUE )
      GO TO 245
C
  240 IF (EJECT(2).NE.0) GO TO 220
  245 WRITE(NOUT,250) IFRVEC
  250 FORMAT (1H0,3X,2A4,I7,4F7.1)
  260 CONTINUE
      RETURN
      END
