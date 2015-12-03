      SUBROUTINE SUBPB (I,L,LS,J,SGR,CGR,YREC,ZREC,SUM,XIC,DELX,EE,XLAM,
     1                  SG,CG,YS,ZS,NAS,NASB,AVR,ZB,YB,ARB,XLE,XTE,X,NB)
C
C     COMPUTES ELEMENTS OF THE SUBMATRICES  DPP, DPZ  AND  DPY
C     USING  SUBROUTINES  SNPDF, INCRO AND SUBI
C
      REAL           KR,M
      COMPLEX        DPUR,DPUL,DPLR,DPLL,DP,SUM
      DIMENSION      XIC(1),DELX(1),EE(1),XLAM(1),SG(1),CG(1),YS(1),
     1               ZS(1),NAS(1),NASB(1),AVR(1),ZB(1),YB(1),ARB(1),
     2               XLE(1),XTE(1),X(1)
      COMMON /AMGMN/ MCB(7),NROW,ND,NE,REFC,FMACH,KR
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
      INFL = 0
      IOUTFL = 0
C
C     UPPER RIGHT SENDING POINT
C
      IGO  = 1
      TL   = XLAM(J)
      SQTL = SQRT(1.0+TL**2)
      SL   = TL/SQTL
      CL   = 1.0/SQTL
      X0   = X(I) - XIC(J)
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
      NA2  = 0
      CALL SNPDF (SL,CL,TL,SGS,CGS,SGR,CGR,X0,Y0,Z0,ES,DIJ,BETA,CV)
      IF (KR .LE. EPS) GO TO 40
      SDELX = DXS
      DELY = 2.0*ES
      AX1  = AX + ES*TL
      AY1  = AY + ES*CGS
      AZ1  = AZ + ES*SGS
      AX2  = AX - ES*TL
      AY2  = AY - ES*CGS
      AZ2  = AZ - ES*SGS
      CALL INCRO (AX,AY,AZ,AX1,AY1,AZ1,AX2,AY2,AZ2,SGR,CGR,SGS,CGS,
     1            KR,FL,BETA,SDELX,DELY,DELR,DELI)
   40 IF (NB .EQ. 0) GO TO 120
      NOAS = NAS(L)
C
C     CHECK FOR ASSOCIATED BODIES
C
      IF (NOAS .EQ. 0) GO TO 120
      DIJS  = DIJ
      DELRS = DELR
      DELIS = DELI
      DIJI  = 0.0
      DELRI = 0.0
      DELII = 0.0
      NA1   = NA2 + 1
      NA2   = NA2 + NOAS
      IF (NA2 .GT. NB) NA2 = NB
C
C     START DO-LOOP FOR THE SUMMATION OF THE WING-IMAGE CONTRIBUTIONS
C     OVER  RANGE(P)
C
      DO 110 NA = NA1,NA2
      NOB  = NASB(NA)
C
C     NOB IS THE SEQUENCE NUMBER OF THE CURRENT BODY ASSOCIATED WITH
C     PANEL  L  IN WHICH THE SENDING POINT  J  LIES
C
      NOBI = NOB
      DA   = AVR(NOB)
      DAR  = ARB(NOB)
      DXLE = XLE(NOB)
      DXTE = XTE(NOB)
      GO TO (50,60,70,80), IGO
   50 CONTINUE
      DZB  = ZB(NOB)
      DYB  = YB(NOB)
      DETA = YS(LS)
      DZETA= ZS(LS)
      GO TO  90
   60 CONTINUE
      DZB  = ZB(NOB)
      DYB  =-YB(NOB)
      DETA =-YS(LS)
      DZETA= ZS(LS)
      GO TO  90
   70 CONTINUE
      DZB  =-ZB(NOB)
      DYB  = YB(NOB)
      DETA = YS(LS)
      DZETA=-ZS(LS)
      GO TO  90
   80 CONTINUE
      DZB  =-ZB(NOB)
      DYB  =-YB(NOB)
      DETA =-YS(LS)
      DZETA=-ZS(LS)
   90 CONTINUE
      DCGAM= CGS
      DSGAM= SGS
      DEE  = ES
      DXI  = XIC(J)
      IF (DXI.LT.DXLE .OR. DXI.GT.DXTE) GO TO 110
      CALL SUBI (DA,DZB,DYB,DAR,DETA,DZETA,DCGAM,DSGAM,DEE,DXI,TL,DETAI,
     1           DZETAI,DCGAMI,DSGAMI,DEEI,DTLAMI,DMUY,DMUZ,INFL,IOUTFL)
      DIJ  = 0.0
      IF (INFL.NE.0 .OR. IOUTFL.EQ.0) GO TO 100
      DTL  = DTLAMI
      DSQRTL = SQRT(1.0+DTL**2)
      DSL  = DTL/DSQRTL
      DCL  = 1.0/DSQRTL
      X0I  = X0
      Y0I  = YREC - DETAI
      Z0I  = ZREC - DZETAI
      CALL SNPDF (DSL,DCL,DTL,DSGAMI,DCGAMI,SGR,CGR,X0I,Y0I,Z0I,DEEI,
     1            DIJ,BETA,CV)
      DIJI = DIJI + DIJ
      IF  (KR .LE. EPS) GO TO 100
      DELR = 0.0
      DELI = 0.0
      AYI  = Y0I
      AZI  = Z0I
      AY1I = AYI - DEEI*DCGAMI
      AZ1I = AZI - DEEI*DSGAMI
      AY2I = AYI + DEEI*DCGAMI
      AZ2I = AZI + DEEI*DSGAMI
      DEEI2 = 2.0*DEEI
      CALL INCRO (AX,AYI,AZI,AX1,AY1I,AZ1I,AX2,AY2I,AZ2I,SGR,CGR,DSGAMI,
     1            DCGAMI,KR,FL,BETA,SDELX,DEEI2,DELR,DELI)
      DELRI = DELRI + DELR
      DELII = DELII + DELI
      GO TO 110
  100 CONTINUE
      DELRI = 0.0
      DELII = 0.0
  110 CONTINUE
      DIJ  = DIJS
      DELR = DELRS
      DELI = DELIS
  120 CONTINUE
      DP = CMPLX(((DIJ+DIJI)-(DELR+DELRI)),(-DELI-DELII))
      GO TO (140,150,170,180), IGO
  140 CONTINUE
      DPUR = DP
      IF (ND .EQ. 0) GO TO 160
C
C     UPPER LEFT  SENDING POINT
C
      IGO = 2
      SGS =-SGS
      TL  =-TL
      SL  =-SL
      Y0  = YREC + YS(LS)
      AY  = Y0
      GO TO 30
  150 CONTINUE
      DPUL = DP
  160 CONTINUE
      IF (NE .EQ. 0) GO TO 190
C
C     LOWER RIGHT SENDING POINT
C
      IGO = 3
      TL  = XLAM(J)
      SL  = TL/(SQRT(1.0+TL*TL))
      Y0  = YREC - YS(LS)
      Z0  = ZREC + ZS(LS)
      AY  = Y0
      AZ  = Z0
      SGS =-SG(LS)
      GO TO 30
  170 CONTINUE
      DPLR = DP
      IF (ND .EQ. 0) GO TO 190
C
C     LOWER LEFT  SENDING POINT
C
      IGO = 4
      SGS = SG(LS)
      TL  =-XLAM(J)
      SL  = TL/(SQRT(1.0+TL*TL))
      Y0  = YREC + YS(LS)
      AY  = Y0
      GO TO 30
  180 CONTINUE
      DPLL = DP
  190 CONTINUE
      SUM  = DPUR + FLND*DPUL + FLNE*DPLR + FLND*FLNE*DPLL
      RETURN
      END
