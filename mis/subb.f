      SUBROUTINE SUBB(KB,KS,I,J,JB,LB,LS,NDY,NYFL,PI,EPS,SGR,CGR,
     *   AR,BETA,SUM,RIA,DELX,YB,ZB,YS,ZS,X)
C   ***   COMPUTES ELEMENTS OF THE SUBMATRICES  DZP, DZZ, DZY, DYP,
C         DYZ  AND  DYY  USING  SUBROUTINE  DZY
      REAL       KD1R,KD1I, KD2R,KD2I
      COMPLEX    DPUR,DPUL,DPLR,DPLL,DP,SUM
      DIMENSION RIA(1),DELX(1),YB(1),ZB(1),YS(1),ZS(1),X(1)
      COMMON /AMGMN/ MCB(7),NROW,ND,NE,REFC,FMACH,KR
      COMMON     /KDS/ IND,KD1R,KD1I,KD2R,KD2I
      FLND = FLOAT(ND)
      FLNE = FLOAT(NE)
      IND  = 0
      DPUR = (0.0,0.0)
      DPUL = (0.0,0.0)
      DPLR = (0.0,0.0)
      DPLL = (0.0,0.0)
      ANOT = RIA(JB)
      DXS  = DELX(J)
      ABSYB= ABS(YB(LB))
      ABSZB= ABS(ZB(LB))
      IFLAG = 0
      IDFLAG = 0
      IF (KB.EQ.0)  GO TO  20
      TEST1= ABS(YB(LB) -YB(KB))
      TEST2= ABS(ZB(LB) -ZB(KB))
      IF  (TEST1.GT.EPS. OR .TEST2.GT.EPS)  GO TO  20
      IFLAG = 1
      IF(NDY .NE. NYFL) GO TO 20
      IF( I .NE. J ) GO TO 20
      IDFLAG = 1
      D2D  =       1.0 /(2.0*PI*ANOT*ANOT*(1.0+AR))
      IF    (NDY.NE.0)  D2D=D2D/AR
      SUM  = CMPLX(D2D,0.0)
      SIGN1 = 1.0
      IF(NDY.NE.0) SIGN1 = -1.0
      IF(ABSYB.LT.EPS) SUM=(1.0+SIGN1*FLND)*SUM
      IF(ABSZB.LT.EPS) SUM=(1.0+SIGN1*FLNE)*SUM
      DPUR = SUM
   20 CONTINUE
      XX   = X(I)
      Y    = YS(KS)
      Z    = ZS(KS)
      XI1  = X(J) - 0.5*DXS
      XI2  = X(J) + 0.5*DXS
      ETA  = YS(LS)
      ZETA = ZS(LS)
      AO   = ANOT
      IDZDY= NDY
      IGO  = 1
      LHS = 0
      IF(IFLAG .EQ. 1) GO TO 45
   30 CONTINUE
      CALL        DZY  (XX, Y, Z, SGR, CGR, XI1, XI2, ETA, ZETA, AR, AO,
     1  KR, REFC, BETA, FMACH, LHS,
     2         IDZDY ,   DZDYR ,   DZDYI )
      DP   = CMPLX(DZDYR,DZDYI)
      GO TO  (40,50,70,80),  IGO
   40 CONTINUE
C  UPPER RIGHT-HAND SIDE CONTRIBUTION
      DPUR = DP
      IF (KB.EQ.LB)  GO TO 100
   45 CONTINUE
      IF (ND.EQ.0) GO TO 60
      IF (IDFLAG.EQ.1.AND.ABSYB.LT.EPS) GO TO 60
      IGO  = 2
      ETA  = -YS(LS)
      LHS = 1
      GO TO  30
   50 CONTINUE
C  UPPER LEFT-HAND SIDE CONTRIBUTION
      DPUL = DP
   60 CONTINUE
      IF (NE.EQ.0) GO TO 90
      IF(IDFLAG.EQ.1.AND.ABSZB.LT.EPS) GO TO 90
      IGO  = 3
      ETA  =  YS(LS)
      ZETA = -ZS(LS)
      LHS = 1
      GO TO  30
   70 CONTINUE
C  LOWER RIGHT-HAND SIDE CONTRIBUTION
      DPLR = DP
      IF (ND.EQ.0) GO TO 90
      IF(IDFLAG.EQ.1.AND.ABSYB.LT.EPS) GO TO 90
      IGO  = 4
      ETA  = -YS(LS)
      ZETA = -ZS(LS)
      LHS = 0
      GO TO  30
   80 CONTINUE
C  LOWER  LEFT-HAND SIDE CONTRIBUTION
      DPLL = DP
   90 CONTINUE
      SUM  = DPUR + FLND*DPUL + FLNE*DPLR + FLND*FLNE*DPLL
  100 CONTINUE
      RETURN
      END
