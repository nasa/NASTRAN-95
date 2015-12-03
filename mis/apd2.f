      SUBROUTINE APD2 (IOPT,CAO1,CAO2,NCORE,ID)
C
      INTEGER         PA2S,PA2E,CA2S,CA2E,IZ(1),NAM(2),IAX(1),CAO1(1),
     1                CAO2(1),PC,PPC,BET,TYPE(3),
     2                CP,ACSID,EID,EIDB,CID(5),CIDBX,AUSET(6,2),SILB,
     3                UK,USA,NECTA(6),KEY(5),SILDX(2),ACSIX(4),BACK,
     4                SCR1,SCR2,SCR3,SCR4,SCR5,ECTA,BGPA,GPLA,USETA,
     5                SILA,CSTMA,ACPT,BUF10,BUF11,BUF12,ACSIB,PID
      REAL            RB1(3),ACPL(3,3),VX1(3),VX2(3)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /BLANK / NK,NJ,LUSETA
      COMMON /SYSTEM/ SYSBUF,NOT
      COMMON /APD1C / EID,PID,CP,NSPAN,NCHORD,LSPAN,LCHORD,IGID,
     1                X1,Y1,Z1,X12,X4,Y4,Z4,X43,XOP,X1P,ALZO,MCSTM,
     2                NCST1,NCST2,CIDBX,ACSID,IACS,SILB,NCRD,
     3                SCR1,SCR2,SCR3,SCR4,SCR5,ECTA,BGPA,GPLA,USETA,
     4                SILA,CSTMA,ACPT,BUF10,BUF11,BUF12,NEXT,LEFT,ISILN,
     5                NCAM,NAEF1,NAEF2,
     6                NCA1,NCA2,CA2S,CA2E,CA3S,CA3E,CA4S,CA4E,
     7                NPA1,NPA2,PA2S,PA2E,PA3S,PA3E,PA4S,PA4E
      COMMON /APD1D / ICPL(14),YP4,S1,C1,XP2,XP3,XP4,RA1(3)
      COMMON /APD12C/ KEY,AUSET,USA,UK,NCAM2,NASB
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (Z(1),IZ(1)),(ICPL(3),RB1(1)),(ICPL(6),ACPL(1,1)),
     1                (NECTA(1),EIDB),(NECTA(2),CID(1)),(KEY(2),NP),
     2                (KEY(3),NSTRIP),(KEY(4),NTP),(EID,IAX(1)),
     3                (SILDX(1),ICID),(ACSIX(1),ACSIB),(ACSIX(2),VX2(1))
      DATA    TYPE  / 1HZ,2HZY,1HY /
      DATA    NAM   / 4HAPD2,4H    /
      DATA    PIO180/ .0174532925  /
C
      IBC   = 0
      NB    = 0
      IF (IOPT .EQ. 1) GO TO 200
C
C     PROCESS CAERO2 WITHOUT CAERO1 ATTACHED
C
      NP    = 0
      NSTRIP= 0
      NTP   = 0
      NAS   = 0
      IPC   = 1
      IDS   = CAO2(1)
   10 IF (CAO2(IPC) .LT. 0) GO TO 191
   11 PC    = CAO2(IPC+1) - 1
      IF (IBC .NE. NB) GO TO 31
C
C     LOOP OVER ALL CAERO2 WITH CURRENT ID TO SET UP POINTERS
C
      NB    = 0
      IBC   = 0
      NBZ   = 0
      NBY   = 0
      NTZ   = 0
      NTY   = 0
      NTZS  = 0
      NTYS  = 0
      NBEA1 = 0
      NSBEA = 0
      NFL   = 0
      NT121 = 0
      NT122 = 0
      K     = IPC
   12 IF (CAO2(K) .NE. IDS) GO TO 19
      NB    = NB + 1
      L     = CAO2(K+1) - 1
      DO 13 M = 1,7
   13 IAX(M) = IZ(L+M)
      ASSIGN 14 TO IRET
      GO TO 29
   14 CONTINUE
      GO TO (15,15,16), BET
   15 NBZ   = NBZ  + 1
      NTZ   = NTZ  + NINT
      NTZS  = NTZS + NSB
      IF (BET .EQ. 1) GO TO 17
   16 NBY   = NBY  + 1
      NTY   = NTY  + NINT
      NTYS  = NTYS + NSB
   17 CONTINUE
      NBEA1 = NBEA1 + NINT
      NSBEA = NSBEA + NSB
      NT121 = NT121 + NTH1
      NT122 = NT122 + NTH2
      NFL   = NFL + KT1
      K     = K + 2
      IF (K .GT. NCAM2*2) GO TO 19
      GO TO 12
   19 IDS   = CAO2(K)
      NTO   = NTP + NTZ + NTY
      NAS   = NASB
C
C     NOW SET UP POINTERS TO BUILD ACPT IN CORE
C
      I = NCORE
      IZ(I   ) = 2
      IZ(I+ 1) = NTP
      IZ(I+ 2) = NTP*2
      IZ(I+ 3) = NP
      IZ(I+ 4) = NB
      IZ(I+ 5) = NTP
      IZ(I+ 6) = NBZ
      IZ(I+ 7) = NBY
      IZ(I+ 8) = NTZ
      IZ(I+ 9) = NTY
      IZ(I+10) = NTO
      IZ(I+11) = NTZS
      IZ(I+12) = NTYS
      IZ(I+13) = NSTRIP
      INC    = I + 14
      INB    = INC  + NP
      INAS   = INB  + NP
      INBEA1 = INAS + NP
      INBEA2 = INBEA1 + NB
      INSBEA = INBEA2 + NB
      IZB    = INSBEA + NB
      IYB    = IZB  + NB
      IAVR   = IYB  + NB
      IARB   = IAVR + NB
      INFL   = IARB + NB
      IXLE   = INFL + NB
      IXTE   = IXLE + NB
      INT121 = IXTE + NB
      INT122 = INT121 + NB
      IZS    = INT122 + NB
      IYS    = IZS + NB + NSTRIP
      IEE    = IYS + NB + NSTRIP
      ISG    = IEE + NSTRIP
      ICG    = ISG + NSTRIP
      IX     = ICG + NSTRIP
      IDELX  = IX  + NTP   + NBEA1
      IXIC   = IDELX + NTP + NBEA1
      IXLAM  = IXIC  + NTP
      IAO    = IXLAM + NTP
      IXIS1  = IAO   + NSBEA
      IXIS2  = IXIS1 + NSBEA
      IAOP   = IXIS2 + NSBEA
      IRIA   = IAOP  + NSBEA
      INASB  = IRIA  + NBEA1
      IFLA1  = INASB + NAS
      IFLA2  = IFLA1 + NFL
      ITH1A  = IFLA2 + NFL
      ITH2A  = ITH1A + NT121
      NWR    = ITH2A + NT122 - NCORE
      NA     = ITH2A + NT122 - 1
      I      = NA + NP*6 +1
      IF (I .GT. LEFT) CALL MESAGE (-8,0,NAM)
C
C     IF PANELS EXIST INSERT DATA FROM SCRATCH FILES
C
      IF(NP.EQ.0) GO TO 31
      NASS = NA
      CALL WRITE (SCR3,0,0,1)
      CALL WRITE (SCR4,0,0,1)
      CALL WRITE (SCR5,0,0,1)
      CALL CLOSE (SCR3,1)
      CALL CLOSE (SCR4,1)
      CALL CLOSE (SCR5,1)
      CALL GOPEN (SCR3,Z(BUF10),0)
      CALL GOPEN (SCR4,Z(BUF11),0)
      CALL GOPEN (SCR5,Z(BUF12),0)
      DO 21 I = 1,NP
      CALL FREAD (SCR5,IZ(INC),1,0)
      CALL FREAD (SCR5,IZ(INB),1,0)
      CALL FREAD (SCR5,K,1,0)
      DO 22 J = 1,6
   22 IZ(NA+J) = IZ(K+J)
      INC = INC + 1
      INB = INB + 1
   21 NA  = NA  + 6
      DO 23 I = 1,NSTRIP
      CALL FREAD (SCR3,IZ(IYS),1,0)
      CALL FREAD (SCR3,IZ(IZS),1,0)
      CALL FREAD (SCR3,IZ(IEE),1,0)
      CALL FREAD (SCR3,IZ(ISG),1,0)
      CALL FREAD (SCR3,IZ(ICG),1,0)
      IYS = IYS + 1
      IZS = IZS + 1
      IEE = IEE + 1
      ISG = ISG + 1
   23 ICG = ICG + 1
      DO 24 I = 1,NTP
      CALL FREAD (SCR4,IZ(IXIC),1,0)
      CALL FREAD (SCR4,IZ(IDELX),1,0)
      CALL FREAD (SCR4,IZ(IXLAM),1,0)
      Z(IX) = Z(IXIC) + .5*Z(IDELX)
      IXIC  = IXIC  + 1
      IDELX = IDELX + 1
      IXLAM = IXLAM + 1
   24 IX    = IX    + 1
      CALL CLOSE (SCR3,1)
      CALL CLOSE (SCR4,1)
      CALL CLOSE (SCR5,1)
C
C     FILL IN ASSOCIATED BODIES
C
      NA  = NASS
      DO 26 I = 1,NP
      L   = 0
      DO 25 J = 1,6
      IF (IZ(NA+J) .EQ. 0) GO TO 25
      L   = L + 1
      IBT = IPC
      DO 27 K = 1,NB
      M   = CAO2(IBT+1)
      IF (IZ(M) .NE. IZ(NA+J)) GO TO 28
      IZ(INASB) = K
      INASB = INASB +1
      GO TO 25
   28 IBT = IBT + 2
   27 CONTINUE
      GO TO 880
   25 CONTINUE
      IZ(INAS) = L
      INAS = INAS + 1
      NA   = NA + 6
   26 CONTINUE
   31 CONTINUE
      IBC  = IBC + 1
C
C     MOVE TO COMMON
C
      DO 20 J = 1,16
   20 IAX(J) = IZ(J+PC)
      IZ(PC+2) = ACSID
      ACSIB = ACSID
      X4    = X1
      Y4    = Y1 + 1.0
      Z4    = Z1
      X43   = X12
      IGID  =-IGID
      CALL APDCS
      IGID  =-IGID
C
C     MOVE AERO CORD SYS TO ICPL
C
      IF (ACSID .EQ. 0) GO TO 35
      DO 34 I = 1,14
      ICPL(I) = IZ(IACS+I-1)
   34 CONTINUE
   35 CONTINUE
      ASSIGN 85 TO IRET
      GO TO 29
C
C     FIND PAERO2 CARD
C
   29 CONTINUE
      IF (PA2S .EQ. 0) GO TO 990
      DO 30 J = PA2S,PA2E,15
      IF (PID .EQ. IZ(J)) GO TO 40
   30 CONTINUE
      GO TO 990
   40 PPC = J
C
C     GET BODY TYPE AND NUMBER OF ELEMENTS
C
      NSB  = NSPAN
      NINT = NCHORD
      BET  = IZ(PPC+1)
      DO 50 J = 1,3
      IF (BET .EQ. TYPE(J)) GO TO 60
   50 CONTINUE
   60 BET  = J
      LTH1 = IZ(PPC+7)
      LTH2 = IZ(PPC+8)
      NTH1 = 0
      NTH2 = 0
      KT1  = 0
      IF (LSPAN .EQ. 0) GO TO 70
      CALL APDOE (LSPAN,IZ,NAEF1,NAEF2,ISPAN,JSPAN)
      IF (ISPAN .EQ. 0) GO TO 950
      NSB  = JSPAN - 1
   70 IF (LCHORD .EQ. 0) GO TO 79
      CALL APDOE (LCHORD,IZ,NAEF1,NAEF2,ICHORD,JCHORD)
      IF (ICHORD .EQ. 0) GO TO 960
      NINT = JCHORD - 1
   79 IF (NINT .EQ. 0) GO TO 80
      KT1  = KT1 + 1
      IF (IZ(PPC+ 9) .EQ. 0) GO TO 920
      IF (IZ(PPC+11) .EQ. 0) GO TO 75
      KT1  = KT1 + 1
      IF (IZ(PPC+13) .EQ. 0) GO TO 75
      KT1  = KT1 + 1
   75 IF (LTH1 .EQ. 0) GO TO 940
      CALL APDOE (LTH1,IZ,NAEF1,NAEF2,ITH1,NTH1)
      IF (ITH1 .EQ. 0) GO TO 940
      IF (LTH2 .EQ. 0) GO TO 80
      CALL APDOE (LTH2,IZ,NAEF1,NAEF2,ITH2,NTH2)
      IF (ITH2 .EQ. 0) GO TO 930
   80 IF (NSB .LT. 2) GO TO 970
      GO TO IRET, (14,85)
C
C     PUT IN TERMS FOR SOME BODY ARRAYS
C
   85 IZ(INBEA1) = NINT
      IF(IBC.GT.1 .AND. BET.LT.IZ(INBEA2-1)) GO TO 870
      IZ(INBEA2) = BET
      IZ(INSBEA) = NSB
      Z(IZB)  = RA1(3)
      Z(IYB)  = RA1(2)
      Z(IZS)  = RA1(3)
      Z(IYS)  = RA1(2)
      Z(IAVR) = Z(PPC+3)
      Z(IARB) = Z(PPC+4)
      IZ(INFL)= KT1
      IZ(INT121) = NTH1
      IZ(INT122) = NTH2
      INBEA1  = INBEA1 + 1
      INBEA2  = INBEA2 + 1
      INSBEA  = INSBEA+1
      IZB  = IZB + 1
      IYB  = IYB + 1
      IZS  = IZS + 1
      IYS  = IYS + 1
      IAVR = IAVR+ 1
      IARB = IARB+ 1
      INFL = INFL+ 1
      INT121 = INT121 + 1
      INT122 = INT122 + 1
C
C     ADD SOME MISC ARRAYS
C
      IF (NTH1 .EQ. 0) GO TO 89
      DO 86 I = 1,NTH1
      Z(ITH1A) = Z(ITH1+I)*PIO180
   86 ITH1A = ITH1A + 1
      IF (NTH2 .EQ. 0) GO TO 88
      DO 87 I = 1,NTH2
      Z(ITH2A) = Z(ITH2+I)*PIO180
   87 ITH2A = ITH2A + 1
   88 K = PPC + 9
      IF (IZ(K).NE.1 .AND. IZ(K+1).NE.NINT .AND. NTH2.EQ.0) GO TO 910
      DO 81 I = 1,KT1
      IZ(IFLA1) = IZ(K)
      IZ(IFLA2) = IZ(K+1)
      K = K + 2
      IF (IZ(IFLA1) .GT. IZ(IFLA2)) GO TO 910
      IF (IZ(IFLA2) .GT. NINT) GO TO 910
      IF (I .EQ. 1) GO TO 82
      IF (IZ(IFLA1) .LE. IZ(IFLA2-1)) GO TO 910
   82 IFLA1 = IFLA1 + 1
      IFLA2 = IFLA2 + 1
   81 CONTINUE
   89 LRSB = IZ(PPC+5)
      LRIB = IZ(PPC+6)
      IF (LRSB .EQ. 0) GO TO 91
      CALL APDOE (LRSB,IZ,NAEF1,NAEF2,IRSB,NRSB)
      IF (IRSB .EQ.     0) GO TO 900
      IF (NRSB .NE. NSB+1) GO TO 900
   91 IF (LRIB .EQ.     0) GO TO 92
      CALL APDOE (LRIB,IZ,NAEF1,NAEF2,IRIB,NRIB)
      IF (IRIB .EQ.      0) GO TO 890
      IF (NRIB .NE. NINT+1) GO TO 890
   92 CONTINUE
      WIDTH = Z(PPC+3)
C
C     GENERATE ELEMENTS
C
      EIDB   = EID - 1
      CIDBX  = CIDBX + 1
      VX1(2) = RA1(2)
      VX1(3) = RA1(3)
C
C     PUT IN PROPER MASKS FOR USET
C
      IF (BET .EQ. 1) GO TO 90
      AUSET(2,2) = UK
      AUSET(6,2) = UK
      IF (BET .EQ. 2) GO TO 90
      AUSET(3,2) = USA
      AUSET(5,2) = USA
   90 CONTINUE
C
C     BUMP NJ AND NK
C
      NJA = NSB + NINT
      NKA = NSB*2
      NJ  = NJ + NJA
      NK  = NK + NKA
      IZ(NCORE+1) = IZ(NCORE+1) + NJA
      IZ(NCORE+2) = IZ(NCORE+2) + NKA
      IF (BET .NE. 2) GO TO 94
      NJ  = NJ + NJA
      NK  = NK + NKA
      IZ(NCORE+1) = IZ(NCORE+1) + NJA
      IZ(NCORE+2) = IZ(NCORE+2) + NKA
   94 I = 1
   95 EIDB = EIDB + 1
      CID(1) = CIDBX
      CIDBX  = CIDBX + 1
      CID(2) = CIDBX
      CID(5) = EIDB
C
C     GRID POINTS IN AERO SYSTEM
C
      IF (I .NE. 1) GO TO 110
      ASSIGN 110 TO BACK
      ICID = CID(1)
      IF (LSPAN .EQ. 0) VX1(1) = RA1(1) + (X12/NSB)*(I-1)
      IF (LSPAN .NE. 0) VX1(1) = RA1(1) + Z(ISPAN+I)*X12
      OLDX = VX1(1)
      Z(IXLE)  = OLDX
      Z(IXIS1) = OLDX
      IXIS1 = IXIS1 + 1
      KK = 1
      GO TO 130
  110 ASSIGN 120 TO BACK
      ICID = CID(2)
      IF (LSPAN .EQ. 0) VX1(1) = RA1(1) + (X12/NSB)*I
      IF (LSPAN .NE. 0) VX1(1) = RA1(1) + Z(ISPAN+I+1)*X12
      Z(IXTE ) = VX1(1)
      Z(IXIS2) = VX1(1)
      IXIS2 = IXIS2 + 1
      IF (I .NE. 1) Z(IXIS1) = OLDX
      IF (I .NE. 1) IXIS1 = IXIS1 + 1
      KK = 1
      GO TO 130
  120 ASSIGN 160 TO BACK
C
C     A0 AND AOP
C
      Z(IAO ) = WIDTH
      Z(IAOP) = 0.0
      IF (LRSB .EQ. 0) GO TO 125
      Z(IAO ) = (Z(IRSB+I  ) + Z(IRSB+I+1))*.5
      Z(IAOP) = (Z(IRSB+I+1) - Z(IRSB+I))/(VX1(1)-OLDX)
  125 IAO  = IAO  + 1
      IAOP = IAOP + 1
      TEMP = (VX1(1)+OLDX)/2.0
      OLDX = VX1(1)
      VX1(1) = TEMP
      ICID = CID(5)
      KK   = 2
C
C     CONVERT TO BASIC
C
  130 IF (ACSID .EQ. 0) GO TO 140
      CALL GMMATS (ACPL,3,3,0,VX1,3,1,0,VX2)
      DO 135 K = 1,3
  135 VX2(K) = VX2(K) + RB1(K)
      GO TO 150
  140 DO 145 K = 1,3
  145 VX2(K) = VX1(K)
C
C     PUT OUT BGPDT GPL USET
C
  150 CALL WRITE (BGPA,ACSIX,4,0)
      CALL WRITE (GPLA,ICID,1,0)
      CALL WRITE (USETA,AUSET(1,KK),6,0)
C
C     BUMP POINTERS
C     PUT OUT SIL EQEXIN SILGA
C
      NCRD   = NCRD + 1
      SILB   = SILB + 6
      ISILN  = ISILN+ 6
      LUSETA = SILB
      SILDX(2) = 10*SILB + 1
      CALL WRITE (SILA,SILB,1,0)
      CALL WRITE (SCR2,ISILN,1,0)
      CALL WRITE (SCR2,SILB,1,0)
      CALL WRITE (SCR1,ICID,2,0)
      GO TO BACK, (110,120,160)
C
C     PUT OUT ECT
C
  160 CID(1) = NCRD - 3
      IF (I .EQ. 1) CID(1) = CID(1) + 1
      CID(2) = NCRD - 1
      CID(3) = CID(1)
      CID(4) = CID(2)
      CID(5) = NCRD
      CALL WRITE (ECTA,NECTA,6,0)
      I  = I + 1
      IF (I .LE. NSB) GO TO 95
C
C     INTEFERENCE CALCULATIONS AND ARRAYS
C
      IF (NINT .EQ. 0) GO TO 170
      P1 = 1.0/NINT
      DO 165 J = 1,NINT
      Z(IRIA) = WIDTH
      IF (LRIB .NE. 0) Z(IRIA) = .5*(Z(IRIB+J)+Z(IRIB+J+1))
      IRIA = IRIA + 1
      D1   = P1*(J-1)
      D2   = P1*J
      IF (LCHORD .NE. 0) D1 = Z(ICHORD+J  )
      IF (LCHORD .NE. 0) D2 = Z(ICHORD+J+1)
      Z(IDELX) = X12*(D2-D1)
      Z(IX) = RA1(1) + X12*(D1+D2)/2.0
      IF (J .EQ.    1) Z(IXLE) = RA1(1) + D1*X12
      IF (J .EQ. NINT) Z(IXTE) = RA1(1) + D2*X12
      IDELX = IDELX + 1
      IX = IX + 1
  165 CONTINUE
  170 CONTINUE
      IXLE = IXLE + 1
      IXTE = IXTE + 1
      IZ(PC+ 4) = NSB
      IZ(PC+ 5) = 1
      IZ(PC+ 8) = 2
      IZ(PC+16) = BET
      IF (BET .EQ. 1) GO TO 190
      AUSET(2,2) = USA
      AUSET(6,2) = USA
      AUSET(3,2) = UK
      AUSET(5,2) = UK
  190 IF (IBC .EQ. NB) CALL WRITE (ACPT,IZ(NCORE),NWR,1)
  191 IF (IOPT .EQ. 1) GO TO 230
      IPC = IPC + 2
      IF (IPC .LT. NCAM2*2) GO TO 10
      GO TO 1000
C
C     CAERO2 WITH CAERO1 ATTACHED
C
  200 IPC = 1
      IDS = ID
  210 IF (CAO2(IPC) .EQ. ID) GO TO 11
  220 IPC = IPC + 2
      IF (IPC .LT. NCAM2*2) GO TO 210
      GO TO 1000
  230 CAO2(IPC) = -CAO2(IPC)
      GO TO 220
 1000 RETURN
C
C     ERROR MESSAGES
C
  912 CALL MESAGE (-61,0,NAM)
  870 WRITE  (NOT,8777) UFM,EID
 8777 FORMAT (A23,' 2273, CAERO2',I9,' NOT INPUT IN Z, ZY, Y SEQUENCE.')
      GO TO 912
  880 WRITE  (NOT,8888) UFM,IZ(NA+J),CAO2(IBT)
 8888 FORMAT (A23,' 2274, ASSOCIATED BODY',I9,' WAS NOT FOUND WITH ',
     1       'CAERO2 GROUP',I9,1H.)
      GO TO 912
  890 J = LRIB
      GO TO 941
  900 J = LRSB
      GO TO 941
  910 WRITE  (NOT,9111) UFM,EID
 9111 FORMAT (A23,' 2275, CAERO2',I9,' HAS INCONSISTENT USE FOR THI OR',
     1       ' THN, OR LTH2 IS REQUIRED.')
      GO TO 912
  920 WRITE  (NOT,9222) UFM,EID
 9222 FORMAT (A23,' 2276, THI1 AND THN1 REQUIRED FOR CAERO2',I9,1H.)
      GO TO 912
  930 J = LTH2
      GO TO 941
  940 J = LTH1
  941 WRITE  (NOT,9999) UFM,J,EID
 9999 FORMAT (A23,' 2429, WRONG NUMBER OF WORDS OR CARD NOT FOUND FOR',
     1       ' CARD ID',I9, /28X,'ASSOCIATED WITH CAERO2 ID',I9)
      GO TO 912
  950 CALL EMSG (0,2326,1,2,0)
      WRITE  (NOT,951) EID,LSPAN
  951 FORMAT (10X,'CAERO2 ELEMENT NO.',I9,' REFERENCES AEFACT CARD NO.',
     1        I9,' WHICH DOES NOT EXIST.')
      GO TO 912
  960 CALL EMSG (0,2327,1,2,0)
      WRITE (NOT,951) EID,LCHORD
      GO TO 912
  970 WRITE  (NOT,971) UFM,EID
  971 FORMAT (A23,' 2277, CAERO2 BODY',I9,' DOES NOT HAVE ENOUGH ',
     1       'SLENDER ELEMENTS.')
      GO TO 912
  990 CALL EMSG (0,2323,1,2,0)
      WRITE  (NOT,991) PID,EID
  991 FORMAT (10X,'PAERO2 CARD NO.',I9,' REFERENCED BY CAERO2 CARD NO.',
     1        I9,' BUT DOES NOT EXIST.')
      GO TO 912
      END
