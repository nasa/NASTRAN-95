      SUBROUTINE MBDPDH (AJJL,F,DF,F1,DF1,F2,DF2,XWTE,YWTE,PAREA,CAPPHI,
     1                   DPHITE,DSS,Q,Q1,Q2,NDN,ND1,NW1,NWN,KTE,KTE1,
     2                   KTE2,NTE,NNCB,NNSBD,IN17,IBUF,A)
C
      LOGICAL        CNTRL2,CNTRL1,CRANK1,CRANK2,ASYM,SURF,LPHI,TEBOX
      DIMENSION      F(1),DF(1),F1(1),DF1(1),F2(1),DF2(1),XWTE(1),
     1               YWTE(1),PAREA(50,50,3),NDN(1),ND1(1),NW1(1),NWN(1),
     2               KTE(1),KTE1(1),KTE2(1),NTE(1),IBUF(1)
      COMPLEX        CAPPHI(1),DPHITE(3,NNSBD),DSS(NNCB,NNSBD),DPHI,TDH,
     1               WS,WF1,WF2,TEMPHI,WPHI,SUMPHI,TRAILE,Q(1),Q1(1),
     2               Q2(1),A(1)
      COMMON /MBOXA/ X(12),Y(12),TANG(10),ANG(10),COTANG(10)
      COMMON /MBOXC/ NJJ ,CRANK1,CRANK2,CNTRL1,CNTRL2,NBOX,NPTS0,NPTS1,
     1               NPTS2,ASYM,GC,CR,MACH,BETA,EK,EKBAR,EKM,BOXL,BOXW,
     2               BOXA ,NCB,NSB,NSBD,NTOTE,KC,KC1,KC2,KCT,KC1T,KC2T
      DATA    NHCONT,NHDSS /4HCONT,4HDSS /
C
      NSKP   = 0
      N1     = NPTS0 + NPTS1
      CALL GOPEN (IN17,IBUF,0)
      DO 4000 MOOD = 1,NJJ
      DO 110 I = 1,NSBD
      NTE(I) = 0
      DPHITE(1,I) = (0.0, 0.0)
      DPHITE(2,I) = (0.0, 0.0)
      DPHITE(3,I) = (0.0, 0.0)
  110 CONTINUE
      DO 120 I = 1,NCB
      DO 120 J = 1,NSBD
      DSS(I,J) = (0.0, 0.0)
  120 CONTINUE
      DO 121 J = 1,KCT
      Q (J)  = (0.0, 0.0)
      F(J)   = 0.0
  121 DF(J)  = 0.0
      IF (.NOT.CNTRL1) GO TO 116
      DO 115 J = 1,KC1T
      Q1(J)  = (0.0, 0.0)
      F1(J)  = 0.0
      DF1(J) = 0.0
  115 CONTINUE
  116 IF (.NOT.CNTRL2) GO TO 118
      DO 117 J = 1,KC2T
      Q2(J)  = (0.0, 0.0)
      F2(J)  = 0.0
      DF2(J) = 0.0
  117 CONTINUE
  118 CALL FREAD (IN17,Z,-NSKP,0)
      JJ = MOOD
      IF (JJ .GT. NPTS0) GO TO 140
      CALL FREAD (IN17,F,KCT,0)
      CALL FREAD (IN17,DF,KCT,0)
      NSKP = NSKP + 2*KCT
      GO TO 160
  140 IF (JJ .GT. N1) GO TO 150
      CALL FREAD (IN17,F1 ,KC1T,0)
      CALL FREAD (IN17,DF1,KC1T,0)
      NSKP = NSKP + KC1T*2
      GO TO 160
  150 CALL FREAD (IN17,F2 ,KC2T,0)
      CALL FREAD (IN17,DF2,KC2T,0)
      NSKP = NSKP + KC2T*2
  160 CALL BCKREC (IN17)
C
C     START LOOP FOR ROWS ON PLANFORM
C
      KC  = 0
      KC1 = 0
      KC2 = 0
      DO 3000 I = 1,NCB
      IXR = I - 1
      XB  = BOXL*(FLOAT(IXR) + 0.5)
      XBB = XB + BOXL/2.0
C
C     BOXES ON PLANE OF MAIN
C
      DO 1100 J = 1,NSBD
      IF (.NOT.(I.GE.ND1(J) .AND. I.LE.NDN(J))) GO TO 1100
      DPHI  = (0.0, 0.0)
      WPHI  = DPHI
      TDH   = (0.0 ,0.0)
      LPHI  = .FALSE.
      SURF  = .FALSE.
      TEBOX = .FALSE.
      IF (I .GE. (NW1(J)+NWN(J))/2) TEBOX = .TRUE.
      IYR   = J - 1
      YB    = BOXW*FLOAT(IYR)
      K     = 1
      IF (YB .GT. Y(2)) K = 2
      PAW   = PAREA(I,J,1)
      PAF1  = PAREA(I,J,2)
      PAF2  = PAREA(I,J,3)
      PAWF  = PAW + PAF1 + PAF2
      IF (.NOT.TEBOX .AND. BETA.GT.TANG(K)) PAWF = 1.0
      PAD   = 1.0 - PAWF
      WS    = (0.0, 0.0)
      WF1   = (0.0, 0.0)
      WF2   = (0.0, 0.0)
      IF (J.EQ.1 .AND. ASYM) GO TO  800
      IF (J .GT. NSB) GO TO 500
      IF (PAD .GE. 0.995) GO TO  400
      IF (PAW .LT. 0.005) GO TO  200
C
      KC = KC + 1
      WS = 2.0*PAW*CMPLX(DF(KC), EK*F(KC))
C
  200 IF (PAF1 .LT. 0.005) GO TO  250
C
      KC1 = KC1 + 1
      WF1 = 2.0*PAF1*CMPLX(DF1(KC1), EK*F1(KC1))
C
  250 IF (PAF2 .LT. 0.005) GO TO 300
C
      KC2 = KC2 + 1
      WF2 = 2.0*PAF2*CMPLX(DF2(KC2), EK*F2(KC2))
C
  300 TDH    = (WS+WF1+WF2)/(PAWF*CR)
      LPHI   = .TRUE.
      TEMPHI = SUMPHI(IXR,IYR,ND1,NDN,CAPPHI,DSS,NNCB,NNSBD,ASYM)
      DPHI   = TDH*CAPPHI(1) + TEMPHI
      IF (PAWF .GE. .005) SURF = .TRUE.
      IF (.NOT.SURF .OR. .NOT.TEBOX) GO TO 350
      NTE(J) =  I
      DPHITE(3,J) = DPHITE(2,J)
      DPHITE(2,J) = DPHITE(1,J)
      DPHITE(1,J) = DPHI
C
  350 IF (PAWF .GT. 0.995) GO TO  800
  400 IF (.NOT.TEBOX) GO TO 500
      XT = XWTE(J)
      IF (XT .GE. XBB) GO TO 420
      DPHITE(1,J) = TRAILE(XT,J,NTE,DPHITE,NNSBD,BOXL)
      IF (XT .LE. XB) GO TO 450
  420 IF (XT .GE. XBB+BOXL) GO TO 500
      WPHI = DPHI
      GO TO 800
  450 EX   = EK*(XB-XT)/BOXL
      WPHI = DPHITE(1,J)*CMPLX(COS(EX), -SIN(EX))
      GO TO 800
  500 DPHI = PAWF*DPHI
      WPHI = (0.0, 0.0)
      IF (.NOT.LPHI) TEMPHI = SUMPHI(IXR,IYR,ND1,NDN,CAPPHI,DSS,NNCB,
     1                               NNSBD,ASYM)
      TDH  = PAD*(WPHI-TEMPHI)/CAPPHI(1) + PAWF*TDH
      IF (.NOT.SURF) DPHI = WPHI
  800 IF (SURF) CALL MBGAW (BOXL,DPHI,WS,PAW,PAF1,PAF2,Q,Q1,Q2,J,KC,KC1,
     1                      KC2)
C
      DSS(I,J) = TDH
 1100 CONTINUE
 3000 CONTINUE
      CALL MBGATE (NTOTE,DPHITE,NNSBD,YWTE,Q,Q1,Q2,KTE,KTE1,KTE2)
      CALL MBGAE (AJJL,IN17,A,F,DF,F1,DF1,F2,DF2,Q,Q1,Q2,MOOD)
      CALL BUG (NHCONT,3000,NJJ,30)
      CALL BUG (NHDSS ,3000,DSS,4)
 4000 CONTINUE
      CALL CLOSE (IN17,1)
      RETURN
      END
