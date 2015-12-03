      SUBROUTINE SUBP (I,L,LS,J,SGR,CGR,YREC,ZREC,SUM,XIC,DELX,EE,XLAM,
     1                 SG,CG,YS,ZS)
C
C     COMPUTES ELEMENTS OF THE SUBMATRICES  DPP, DPZ  AND  DPY
C     USING  SUBROUTINES  SNPDF,  INCRO  AND SUBI
C
      REAL           KR,M
      COMPLEX        DPUR,DPUL,DPLR,DPLL,DP,SUM
      DIMENSION      XIC(1),DELX(1),EE(1),XLAM(1),SG(1),CG(1),YS(1),
     1               ZS(1)
      COMMON /AMGMN/ MCB(7),NROW,ND,NE,REFC,FMACH,KR
      COMMON /DLCOM/ DUM(3),F
C
      EPS  = 0.00001
      M    = FMACH
      BETA = SQRT(1.0-M*M)
      FL   = REFC
      FLND = FLOAT(ND)
      FLNE = FLOAT(NE)
      SGS  = SG(LS)
      CGS  = CG(LS)
      DPUR = (0.0,0.0)
      DPUL = (0.0,0.0)
      DPLR = (0.0,0.0)
      DPLL = (0.0,0.0)
      DIJ  = 0.0
      DELR = 0.0
      DELI = 0.0
      DIJI = 0.0
      DELRI= 0.0
      DELII= 0.0
C
C     UPPER RIGHT SENDING POINT
C
      IGO  = 1
      TL   = XLAM(J)
      SQTL = SQRT(1.0+TL**2)
      SL   = TL/SQTL
      CL   = 1.0/SQTL
      X    = XIC(I) + F*DELX(I)
      X0   = X - XIC(J)
      Y0   = YREC - YS(LS)
      Z0   = ZREC - ZS(LS)
      ES   = EE(LS)
      DXS  = DELX(J)
      AX   = X0
      AY   = Y0
      AZ   = Z0
      CV   = DXS
C
   30 NOBI = 1
      CALL SNPDF (SL,CL,TL,SGS,CGS,SGR,CGR,X0,Y0,Z0,ES,DIJ,BETA,CV)
      IF (KR .LE. EPS) GO TO  40
      SDELX= DXS
      DELY = 2.0*ES
      AX1  = AX + ES*TL
      AY1  = AY + ES*CGS
      AZ1  = AZ + ES*SGS
      AX2  = AX - ES*TL
      AY2  = AY - ES*CGS
      AZ2  = AZ - ES*SGS
      CALL INCRO (AX,AY,AZ,AX1,AY1,AZ1,AX2,AY2,AZ2,SGR,CGR,SGS,CGS,
     1            KR,FL,BETA,SDELX,DELY,DELR,DELI)
   40 CONTINUE
      DP = CMPLX(((DIJ+DIJI)-(DELR +DELRI)),(-DELI-DELII))
      GO TO (140,150,170,180), IGO
  140 CONTINUE
      DPUR = DP
C
C     TEST FOR ABS(YS(LS)) .LE..001 TAKEN OUT
C
      IF (ND .EQ. 0) GO TO 160
C
C     UPPER LEFT  SENDING POINT
C
      IGO  = 2
      SGS  =-SGS
      TL   =-TL
      SL   =-SL
      Y0   = YREC + YS(LS)
      AY   = Y0
      GO TO  30
  150 CONTINUE
      DPUL = DP
  160 CONTINUE
      IF (NE .EQ. 0) GO TO 190
C
C     LOWER RIGHT SENDING POINT
C
      IGO  = 3
      TL   = XLAM(J)
      SL   = TL/(SQRT(1.0+TL*TL))
      Y0   = YREC - YS(LS)
      Z0   = ZREC + ZS(LS)
      AY   = Y0
      AZ   = Z0
      SGS  =-SG(LS)
      GO TO  30
  170 CONTINUE
      DPLR = DP
      IF (ND .EQ. 0) GO TO 190
C
C     LOWER LEFT  SENDING POINT
C
      IGO  = 4
      SGS  = SG(LS)
      TL   =-XLAM(J)
      SL   = TL/(SQRT(1.0+TL*TL))
      Y0   = YREC + YS(LS)
      AY   = Y0
      GO TO  30
  180 CONTINUE
      DPLL = DP
  190 CONTINUE
      SUM  = DPUR + FLND*DPUL + FLNE*DPLR + FLND*FLNE*DPLL
      RETURN
      END
