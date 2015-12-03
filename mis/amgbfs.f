      SUBROUTINE AMGBFS (SKJ,EE,DELX,NC,NBA,XIS2,XIS1,A0,A0P,NSBE)
C
C     BUILD SKJ CALL BFSMAT THEN SHUFFEL AND DEAL
C
      INTEGER         SKJ,NAME(2),TSKJ,SYSBUF,SCR1,SCR2,ECORE
      DIMENSION       EE(1),DELX(1),NC(1),NBA(1),XIS2(1),XIS1(1),A0(1),
     1                A0P(1),NSBE(1)
      COMMON /AMGMN / MCB(7),NROW,ND,NE,REFC,FMACH,RFK,TSKJ(7),ISK,NSK
      COMMON /DLBDY / NJ1,NK1,NP,NB,NTP,NBZ,NBY,NTZ,NTY,NT0,NTZS,NTYS,
     1                INC,INS,INB,INAS,IZIN,IYIN,INBEA1,INBEA2,INSBEA,
     2                IZB,IYB,IAVR,IARB,INFL,IXLE,IXTE,INT121,INT122,
     3                IZS,IYS,ICS,IEE,ISG,ICG,IXIJ,IX,IDELX,IXIC,IXLAM,
     4                IA0,IXIS1,IXIS2,IA0P,IRIA,INASB,IFLA1,IFLA2,ITH1A,
     5                ITH2A,ECORE,NEXT,SCR1,SCR2,SCR3,SCR4,SCR5
      COMMON /SYSTEM/ SYSBUF
      COMMON /ZBLPKX/ A(4),IIS
      COMMON /ZZZZZZ/ Z(1)
      DATA    NAME  / 4HAMGB,4HFS  /
      DATA    NHBFS , NHG,NHA      / 4HBFS ,4HG   ,4HA   /
C
      NSB   = NTYS + NTZS
      NZY2  = NSB*2
      NT02  = NT0*2
      NTP2  = NTP*2
      LENGTH= NT0 + NSB
      ISL   = ISK - 1
      II    = ISK + LENGTH
      NN    = NSK + NZY2 + NTP2
      IBUF2 = ECORE
      IF (NSB .EQ. 0) GO TO 40
      IBUF2 = ECORE - SYSBUF
C
C     CALL BFSMAT
C     SCR1 HAS NTZS + NTYS ROWS WITH NTO*2 THEN NTZS+NTYS*2 TERMS
C     ROWS ARE Z FOR Z , Y THEN Z FOR ZY , AND Y FOR Y
C
      CALL GOPEN (SCR1,Z(IBUF2),1)
      ICORR = NEXT
      IF (NEXT+LENGTH*4 .GT. IBUF2) GO TO 998
      CALL BFSMAT (ND,NE,NB,NP,NTP,LENGTH,NT0,SCR1,JF,JL,Z(INAS),FMACH,
     1             Z(IYB),Z(IZB),Z(IYS),Z(IZS),Z(IX),DELX,EE,Z(IXIC),
     2             Z(ISG),Z(ICG),Z(IARB),Z(IRIA),Z(INBEA1),Z(INBEA2),
     3             Z(INASB),Z(INB),NC,Z(ICORR),Z(IAVR),REFC,A0,XIS1,
     4             XIS2,RFK,NSBE,NT0)
      CALL WRITE  (SCR1,0,0,1)
      CALL CLOSE  (SCR1,1)
      CALL DMPFIL (SCR1,Z(NEXT),IBUF2-NEXT)
      CALL GOPEN  (SCR1,Z(IBUF2),0)
      NCORE = NT0*NZY2*2
      IF (NCORE+NEXT .GT. IBUF2) GO TO 998
      CALL ZEROC  (Z(NEXT),NCORE)
      I    = NEXT
      IZBF = 1
      DO 30 J = 1,NSB
      CALL FREAD (SCR1,Z(I),NT02,0)
      CALL FREAD (SCR1,Z(I),-NZY2,0)
      I = I + NT02
      IF (JF .EQ. 0) GO TO 20
      IF (J.LT.JF .OR. J.GT.JL) GO TO 20
      IZBF = -IZBF
      IF (IZBF .LT. 0) GO TO 30
      I = I + NT02
   20 I = I + NT02
   30 CONTINUE
      CALL BCKREC (SCR1)
C
C     BUILD NT0 COLUMNS OF SKJ
C
   40 IF (NT0 .EQ. 0) GO TO 100
      IBF  = NEXT - 2
      K    = 1
      KS   = 1
      NBXR = NC(K)
      DO 70 I = 1,NT0
      CALL BLDPK (3,3,SKJ,0,0)
      IF (I .GT. NTP) GO TO 45
      A(1) = 2.0*EE(KS)*DELX(I)
      A(2) = 0.0
      IIS  = ISL + (I-1)*2 + 1
      CALL ZBLPKI
      A(1) = (EE(KS)*DELX(I)**2)/2.0
      IIS  = IIS + 1
      CALL ZBLPKI
      IF (I .EQ.    NTP) GO TO 45
      IF (I .EQ. NBA(K)) K = K + 1
      IF (I .EQ.   NBXR) GO TO 44
      GO TO 45
   44 KS   = KS + 1
      NBXR = NBXR + NC(K)
   45 IF (NSB .EQ. 0) GO TO 60
      IBF  = IBF + 2
      DO 50 J = 1,NZY2
      L    = (J-1)*NT02
      A(1) = Z(IBF+L  )
      A(2) = Z(IBF+L+1)
      IIS  = ISL + NTP2 + J
      CALL ZBLPKI
   50 CONTINUE
   60 CALL BLDPKN (SKJ,0,TSKJ)
   70 CONTINUE
C
C     SLENDER BODY ONLY PART OF SKJ  BFS * G
C
  100 IF (NSB .EQ. 0) GO TO 900
      NCORE = NZY2*NSB*4 + NSB*NSB*2
      IF (NCORE+NEXT .GT. IBUF2) GO TO 998
      CALL ZEROC (Z(NEXT),NCORE)
      I    = NEXT
      IZBF = 1
      DO 130 J = 1,NSB
      CALL FREAD (SCR1,Z(I),-NT02,0)
      CALL FREAD (SCR1,Z(I), NZY2,0)
      I = I + NZY2
      IF (JF .EQ. 0) GO TO 120
      IF (J.LT.JF .OR. J.GT.JL) GO TO 120
      IZBF = -IZBF
      IF (IZBF .LT. 0) GO TO 130
      I = I + NZY2
  120 I = I + NZY2
  130 CONTINUE
C
C     BFS AT NEXT  G AT IG
C
      IG   = I
      IA   = IG + NSB*NSB*2
      NFYB = NB + 1 - NBY
      IROW = IG
      RFKOC= 2.0*RFK/REFC
      IBZY = 0
      P5   = .5
      IF (NTZS .EQ. 0) GO TO 170
      NFSE = 1
      NLSE = 0
      NFB  = 1
      NBX  = NBZ
  141 DO 160 IB = NFB,NBX
      NLSE = NLSE + NSBE(IB)
      DO 150 IT = NFSE,NLSE
      DX   = XIS2(IT) - XIS1(IT)
      A02P = 2.0/A0(IT)*A0P(IT)
      IF (NFSE .EQ. NLSE) GO TO 148
      IF (IT   .NE. NFSE) GO TO 142
      X2 = P5*(XIS2(IT+1) + XIS1(IT+1))
      X1 = P5*(XIS2(IT  ) + XIS1(IT  ))
      Z(IROW  ) = (-1.0/(X2-X1))*DX
      Z(IROW+2) = -Z(IROW)*(A0(IT)/A0(IT+1))**2
      GO TO 148
  142 IF (IT .EQ. NLSE) GO TO 145
      X1 = P5*(XIS2(IT-1) + XIS1(IT-1))
      X2 = P5*(XIS2(IT  ) + XIS1(IT  ))
      X3 = P5*(XIS2(IT+1) + XIS1(IT+1))
      Z(IROW-2) = (1.0/(X3-X1) - 1.0/(X2-X1))*DX*(A0(IT)/A0(IT-1))**2
      Z(IROW)   = (1.0/(X2-X1) - 1.0/(X3-X2))*DX
      Z(IROW+2) = (1.0/(X3-X2) - 1.0/(X3-X1))*DX*(A0(IT)/A0(IT+1))**2
      GO TO 148
  145 X1 = P5*(XIS2(IT-1) + XIS1(IT-1))
      X2 = P5*(XIS2(IT  ) + XIS1(IT  ))
      Z(IROW  ) = (1.0/(X2-X1))*DX
      Z(IROW-2) =-Z(IROW)*(A0(IT)/A0(IT-1))**2
  148 Z(IROW  ) = Z(IROW) + DX*A02P
      Z(IROW+1) = DX*RFKOC
      IROW = IROW + NZY2 + 2
  150 CONTINUE
      NFSE = NFSE + NSBE(IB)
  160 CONTINUE
  170 IF (IBZY .EQ. 1) GO TO 200
      IBZY = 1
      IF (NTYS .EQ. 0) GO TO 200
      NFB  = NFYB
      NBX  = NB
      NFSE = 1
      NLSE = 0
      NL   = NFYB - 1
      IF (NL .EQ. 0) GO TO 141
      DO 172 J = 1,NL
      NLSE = NLSE + NSBE(J)
      NFSE = NFSE + NSBE(J)
  172 CONTINUE
      GO TO 141
C
C     MULTIPLY BFS * G
C
  200 CALL BUG (NHBFS ,200,Z(NEXT),NZY2*NZY2)
      CALL BUG (NHG   ,200,Z(IG),NSB*NSB*2)
      CALL GMMATC (Z(NEXT),NZY2,NSB,0,Z(IG),NSB,NSB,0,Z(IA))
      CALL BUG (NHA   ,200,Z(IA),NZY2*NSB*2)
      IROW = IA - 2
      DO 220 I = 1,NSB
      CALL BLDPK (3,3,SKJ,0,0)
      IROW = IROW + 2
      K    = IROW
      DO 210 J = 1,NZY2
      A(1) = Z(K  )
      A(2) = Z(K+1)
      IIS  = ISL + NTP2 + J
      CALL ZBLPKI
      K    = K + NZY2
  210 CONTINUE
      CALL BLDPKN (SKJ,0,TSKJ)
  220 CONTINUE
  900 ISK  = II
      NSK  = NN
      CALL CLOSE (SCR1,1)
 1000 RETURN
C
C     ERROR MESSAGES
C
  998 CALL MESAGE (-8,0,NAME)
      GO TO 1000
      END
