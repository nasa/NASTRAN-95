      SUBROUTINE SPANL2(IARG)
C*****
C THIS ROUTINE IS PHASE II OF STRESS DATA RECOVERY FOR THE SHEAR AND
C TWIST PANEL ELEMENTS.
C*****
C
C
      REAL    FRLAST(2)
      INTEGER EJECT    ,ISHD(7)  ,ISTYP(2) ,TYP(4)   ,IFOR(1)
C
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
C SDR2 PHASE II INPUT AND OUTPUT BLOCK.
C
      COMMON   /SDR2X7/
     1                   IELID              ,ISILNO(4)
     2,                  S(3,4)             ,A(2)
     3,                  T                  ,RATIO(3)
     4,                  SIGS               ,RQ(4)
     5,                  RK(4)              ,XXXXXX(68)
      COMMON   /SDR2X7/
     1                   JSELID             ,STRES(3)
     2,                  YYYYYY(96)
      COMMON   /SDR2X7/
     1                   JFELID             ,FORCES(16)
     2,                  ZZZZZZ(8)
C
C SDR2 SCRATCH BLOCK
C
      COMMON   /SDR2X8/
     1                   S1BAR              ,TERM
     2,                  TAU(4)             ,IDISP
     3,                  IU                 ,CTU(4)
     4,                  CFRVEC(19)
C
C OUTPUT PRECISION CHECK BLOCK
C
      COMMON /SDR2X9/ NCHK,ISUB,ILD,FRTMEI(2),TWOTOP,FNCHK
C
      COMMON   /SYSTEM/  IBFSZ    ,NOUT     ,IDM(9)   ,LINE
C
      EQUIVALENCE(STRES(1),TAUMAX)
      EQUIVALENCE(STRES(2),TAUAVG)
      EQUIVALENCE(STRES(3),MARSAF,SAFMAR)
      EQUIVALENCE(FORCES(1),IFOR(1),P13)
      EQUIVALENCE(FORCES(2),P24)
C////////  FOLLOWING 8 FORCES MAY NOT BE EQUIVALENCED CORRECTLY YET/////
      EQUIVALENCE(FORCES(1),F1 )
      EQUIVALENCE(FORCES(2),F2 )
      EQUIVALENCE(FORCES(3),F3 )
      EQUIVALENCE(FORCES(4),F4 )
      EQUIVALENCE(FORCES(5),F5 )
      EQUIVALENCE(FORCES(6),F6 )
      EQUIVALENCE(FORCES(7),F7 )
      EQUIVALENCE(FORCES(8),F8 )
      EQUIVALENCE(FORCES( 9),RK1)
      EQUIVALENCE(FORCES(10),Q1)
      EQUIVALENCE(FORCES(11),RK2)
      EQUIVALENCE(FORCES(12),Q2)
      EQUIVALENCE(FORCES(13),RK3)
      EQUIVALENCE(FORCES(14),Q3)
      EQUIVALENCE(FORCES(15),RK4)
      EQUIVALENCE(FORCES(16),Q4)
      EQUIVALENCE(ISHD(1),LSUB)
      EQUIVALENCE(ISHD(2),LLD)
      EQUIVALENCE(ISHD(6),FRLAST(1))
      EQUIVALENCE(CFRVEC(1),IFRVEC)
C
      DATA LSUB,LLD,FRLAST / 2*-1, -1.0E30, -1.0E30 /
      DATA TYP / 4HSHEA,1HR, 4HTWIS,1HT /
      DATA LARG / 0 /
C
      IDISP = IVEC - 1
C
C COMPUTE AVERAGE STRESS ALONG SIDE 1 IF WE ARE DEALING WITH A SHEAR
C PANEL OR MEAN FIBRE SHEAR STRESS IF WE HAVE A TWIST PANEL.
C
      CS1BR = 0.0
      S1BAR = 0.0
      DO 10 I = 1,4
      IU = IDISP + ISILNO(I)
      IF (IARG .EQ. 5) IU = IU + 3
      CALL SMMATS(S(1,I),3,1,1,ZZ(IU),3,1,0,TERM,CTRM)
      CS1BR = CS1BR + CTRM
   10 S1BAR = S1BAR + TERM
C
C COMPUTE STRESSES AT THE CORNERS
C
      TAU(1) = RATIO(1) * S1BAR
      TAU(2) = S1BAR / RATIO(1)
      TAU(3) = RATIO(2) * S1BAR
      TAU(4) = RATIO(3) * S1BAR
      CTU(1) = ABS (RATIO(1)) * CS1BR
      CTU(2) = CS1BR / ABS (RATIO(1))
      CTU(3) = ABS (RATIO(2)) * CS1BR
      CTU(4) = ABS (RATIO(3)) * CS1BR
C
C COMPUTE AVERAGE STRESS
C
      TAUAVG = 0.25 * (TAU(1) + TAU(2) + TAU(3) + TAU(4))
      CFRVEC(3) = 0.25E0 * (CTU(1) + CTU(2) + CTU(3) + CTU(4) )
C
C COMPUTE MAXIMUM STRESS
C
      TAUMAX = ABS(TAU(1))
      CFRVEC(2) = TAUMAX
      DO 50 I = 2,4
      IF (ABS(TAU(I)) .GT. TAUMAX) TAUMAX = ABS(TAU(I))
      IF (CTU(I).GT.CFRVEC(2))  CFRVEC(2) = CTU(I)
   50 CONTINUE
C
C COMPUTE MARGIN OF SAFETY
C
      IF(SIGS.LE.0.0)GO TO 100
      IF(TAUMAX.EQ.0.0)GO TO 100
      SAFMAR=SIGS/TAUMAX-1.0
      GO TO 101
  100 MARSAF=1
  101 CONTINUE
C
C FOR A SHEAR PANEL COMPUTE LOADS, FOR A TWIST PANEL COMPUTE STRESSES.
C
      IF( IARG .NE. 4 ) GO TO 70
C
C     SHEAR PANEL FORCES
C
      Q1 = S1BAR*T /  SQRT( 1.0 + ( RQ(4)/RK(1) )**2)
      Q2 = S1BAR * RQ(1) /  SQRT( 1.0 + ( RQ(4)/RK(2) )**2)
      Q3 = S1BAR * RQ(2) /  SQRT( 1.0 + ( RQ(4)/RK(3) )**2)
      Q4 = S1BAR * RQ(3) /  SQRT( 1.0 + ( RQ(4)/RK(4) )**2)
      CFRVEC(13) = CS1BR * ABS(T) / SQRT (1.0E0 + (RQ(4)/RK(1) ) **2 )
      DO 60 I = 1,3
      F     = SQRT (1.0E0 + ( RQ(4)/RK(I+1) ) **2  )
      FORCES(2*I+10) = S1BAR * RQ(I) / F
   60 CFRVEC(2*I+13) = CS1BR * ABS(RQ(I)) / F
C
      F     = ABS (RQ(4))
      RK1 = -( Q1 + Q4 ) * RQ(4)
      RK2 = -( Q1 + Q2 ) * RQ(4)
      RK3 = -( Q2 + Q3 ) * RQ(4)
      RK4 = -( Q3 + Q4 ) * RQ(4)
      CFRVEC(12) = (CFRVEC(13) + CFRVEC(19)) * F
      CFRVEC(14) = (CFRVEC(13) + CFRVEC(15)) * F
      CFRVEC(16) = (CFRVEC(15) + CFRVEC(17)) * F
      CFRVEC(18) = (CFRVEC(17) + CFRVEC(19)) * F
      F1 = Q4 * RK(4)
      F2 = Q1 * RK(1)
      F5 = Q2 * RK(2)
      F6 = Q3 * RK(3)
      CFRVEC(4) = CFRVEC(19) * ABS (RK(4) )
      CFRVEC(5) = CFRVEC(13) * ABS (RK(1) )
      CFRVEC(8) = CFRVEC(15) * ABS (RK(2) )
      CFRVEC(9) = CFRVEC(17) * ABS (RK(3) )
      F3 = -F2
      F4 = -F5
      F7 = -F6
      F8 = -F1
      CFRVEC( 6) = CFRVEC(5)
      CFRVEC( 7) = CFRVEC(8)
      CFRVEC(10) = CFRVEC(9)
      CFRVEC(11) = CFRVEC(4)
      GO TO 80
C
C     TWIST STRESSES
C
   70 P13 = A(1) * S1BAR * T
      P24 = A(2) * S1BAR * T
      TERM = T / 6.0
      CFRVEC(4) = A(1) * CS1BR * T
      CFRVEC(5) = A(2) * CS1BR * T
      P13  = P13 * TERM
      P24  = P24 * TERM
      CFRVEC(4) = ABS (CFRVEC(4) * TERM)
      CFRVEC(5) = ABS (CFRVEC(5) *TERM)
C
C STORE ELEMENT ID IN OUTPUT SLOTS.
C
   80 JSELID = IELID
      JFELID = IELID
      IF (NCHK.LE.0) GO TO 260
C
C  . CHECK PRECISION...
C
      K = 0
C
C  . STRESSES...
      CALL SDRCHK (STRES(1),CFRVEC(2),2,K)
C
C  . FORCES...
      I = 16
      IF (IARG.NE.4) I = 2
      CALL SDRCHK (FORCES(1),CFRVEC(4),I,K)
      IF (K.EQ.0) GO TO 260
C
C  . LIMITS EXCEEDED...
      IFRVEC = IELID
      I = 1
      IF (IARG.NE.4) I = 3
      ISTYP(1) = TYP(I)
      ISTYP(2) = TYP(I+1)
      J = 0
C
      IF  (LSUB.EQ.ISUB .AND. FRLAST(1).EQ.FRTMEI(1) .AND. LARG.EQ.IARG
     1.AND. LLD .EQ.ILD  .AND. FRLAST(2).EQ.FRTMEI(2) ) GO TO 230
      LSUB = ISUB
      LARG = IARG
      LLD = ILD
      FRLAST(1) = FRTMEI(1)
      FRLAST(2) = FRTMEI(2)
      J = 2
      CALL PAGE1
  200 CALL SD2RHD (ISHD,J)
      LINE = LINE + 1
      IF (IARG.EQ.4) WRITE(NOUT,210)
      IF (IARG.NE.4) WRITE(NOUT,220)
  210 FORMAT (7X,4HTYPE,5X,42HEID  SMAX  SAVE  F1-4  F1-2  F2-1  F2-3  F
     1,60H3-2  F3-4  F4-3  F4-1   K-1  SH12   K-2  SH23   K-3  SH34    
     2, 9HK-4  SH41)
  220 FORMAT (7X,4HTYPE,5X,27HEID  SMAX  SAVE  M1-3  M2-4)
      GO TO 240
  230 IF (EJECT(2).NE.0) GO TO 200
  240 I = 19
      IF (IARG.NE.4) I = 5
      WRITE(NOUT,250) ISTYP,(CFRVEC(J),J=1,I)
  250 FORMAT (1H0,6X,A4,A1,I7,18F6.1)
C
  260 CONTINUE
      RETURN
      END
