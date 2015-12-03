      SUBROUTINE APD5
C
      EXTERNAL        ORF
      INTEGER         NAM(2),IZ(1),BACK,PSPA,IC(16),EID,PID,CIDBX,SILB,
     1                SCR1,ECTA,BGPA,GPLA,USETA,SILA,ACPT,BUF10,CA5S,
     2                CA5E,PA5S,PA5E,AUSET(6,2),SILC,ORF,USA,UK,EIDB,
     3                SILDX(4),ACSIX(4),CID(5),NECTA(2)
      REAL            VX1(3),VX2(3),ACPL(3,3),RB1(3)
      DIMENSION       AI(3),HEAD(10),IHEAD(10)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /BLANK / NK,NJ,LUSETA
      COMMON /APD1C / EID,PID,CP,NSPAN,NCHORD,NTHRY,NTHICK,IGID,
     1                X1,Y1,Z1,X12,X4,Y4,Z4,X43,XOP,X1P,ALZO,MCSTM,
     2                NCST1,NCST2,CIDBX,ACSID,IACS,SILB,NCRD,SCR1,
     3                SCR2,SCR3,SCR4,SCR5,ECTA,BGPA,GPLA,USETA,SILA,
     4                CSTMA,ACPT,BUF10,BUF11,BUF12,NEXT,LEFT,ISILN,
     5                NCAM,NAEF1,NAEF2,
     6                NCA1,NCA2,CA2S,CA2E,CA3S,CA3E,CA4S,CA4E,
     7                NPA1,NPA2,PA2S,PA2E,PA3S,PA3E,PA4S,PA4E,CA5S,CA5E,
     8                PA5S,PA5E
      COMMON /APD1D / ICPL(14),YP4,S1,C1,XP2,XP3,XP4,RA1(3)
      COMMON /ZZZZZZ/ Z(1)
      COMMON /BITPOS/ IBIT(64)
      COMMON /TWO   / ITWO(32)
      COMMON /SYSTEM/ SYSBUF,NOT
      EQUIVALENCE     (ICPL(3),RB1(1)),(ICPL(6),ACPL(1,1)),
     1                (NECTA(1),EIDB),(NECTA(2),CID(1)),
     2                (ACSIX(2),VX2(1)),(Z(1),IZ(1)),(EID,IC(1)),
     3                (SILDX(1),ICID),(SILDX(3),SILC),
     4                (AI(1),DY),(AI(2),BLOC),(AI(3),CA),
     5                (HEAD(1),IHEAD(1))
      DATA    NAM   / 4HAPD5,4H    /
C
      LCA  = 16
      NC5  = ((CA5E-CA5S)+1)/LCA
      NCAM = NCAM + NC5
C
C     INITIAL SETUP
C
      I17  = IBIT(17)
      I18  = IBIT(18)
      I19  = IBIT(19)
      I20  = IBIT(20)
      PSPA = ORF(ITWO(I17),ITWO(I20))
      USA  = ORF(PSPA,ITWO(I18))
      UK   = ORF(ITWO(I19),ITWO(I20))
      DO 5 J = 1,2
      DO 5 I = 1,6
    5 AUSET(I,J) = USA
      AUSET(3,2) = UK
      AUSET(5,2) = UK
      IHEAD(1)   = 5
      SILC = SILB
C
C     LOOP ON NC5 MOVING CAERO5 CARD TO COMMON
C
      DO 400 I = 1,NC5
      NTOT = 0
      N = (I-1)*LCA - 1
      DO 10 J = 1,LCA
      IC(J) = IZ(CA5S+N+J)
   10 CONTINUE
      MCSTM = MCSTM + 1
      IZ(CA5S+N+2) = MCSTM
      IZ(CA5S+N+8) = 5
      ACSIX(1) = MCSTM
C
C     FIND PAERO5 CARD
C
      CALL APDOE (PID,IZ,PA5S,PA5E,IPID,NPC)
      IF (IPID .EQ. 0) GO TO 999
C
C     FIND NUMBER OF STRIPS
C
      ISPAN = NSPAN
      IAST  = 0
      IF (NSPAN .NE. 0) GO TO 20
      CALL APDOE (NCHORD,IZ,NAEF1,NAEF2,IAST,NSPAN)
      IF (IAST .EQ. 0) GO TO 998
      NSPAN = NSPAN - 1
      IAST  = IAST + 1
   20 IZ(CA5S+N+4) = NSPAN
      IZ(CA5S+N+5) = 1
      IPP = IPID + 7
      NPC = NPC - 6
      IF (NPC .LT. NSPAN) GO TO 997
      IHEAD(9) = NSPAN
C
C     GET POINTS IN PROPER COORD SYSTEM
C
      CALL APDCS
      HEAD(10) = SQRT(1.0+(XP4/YP4)**2)
      IF (NEXT+3*NSPAN .GT. LEFT) GO TO 996
      IOC = NEXT
C
C     GENERATE DATA FOR BOXES
C
      NCRDP= NCRD
      FSJ1 = APDF(Z(IAST),1,ISPAN)
      YJ1  = FSJ1*YP4
      DJ1  = FSJ1*XP4
      CJ1  = X12 + FSJ1*(X43-X12)
      XIJ1 = DJ1
      XI1J1= DJ1 + CJ1
      EIDB = EID - 1
      DO 100 J = 1,NSPAN
      YJ   = YJ1
      FSJ1 = APDF(Z(IAST),J+1,ISPAN)
      YJ1  = FSJ1*YP4
      DJ1  = FSJ1*XP4
      CJ1  = X12 + FSJ1*(X43-X12)
      DY   = YJ1 - YJ
      YA   = .5*DY + YJ
      YSP  = YA
      CLOC = X12 - (X12-X43)*YA/YP4
      BLOC = CLOC*.5
      CAOC = Z(IPP)
      IPP  = IPP + 1
      CA   = CAOC*CLOC
      NSIZE= 2
      IF (CAOC .NE. 0.0) NSIZE = 3
      NJ   = NJ + NSIZE
      NK   = NK + NSIZE
      NTOT = NTOT + NSIZE
      KK   = 0
      DO 40 K = 1,3
      Z(IOC+J+KK) = AI(K)
      KK = KK + NSPAN
   40 CONTINUE
C
C     EXTERNAL ID S
C
      EIDB   = EIDB   + 1
      CID(1) = CIDBX  + 1 + 2*(J-1)
      CID(2) = CID(1) + 1
      CID(3) = CID(1) + 2
      CID(4) = CID(3) + 1
      CID(5) = EIDB
      NCID   = CID(4)
C
C     BGPDT, SPL, AND USET
C
      XIJ  = XIJ1
      XI1J = XI1J1
      XIJ1 = DJ1
      XI1J1= DJ1 + CJ1
      XIC  = (XIJ+XIJ1+BLOC)*.5
      VX1(3) = 0
      IF (J .NE. 1) GO TO 310
      ASSIGN 300 TO BACK
      ICID   = CID(1)
      VX1(1) = XIJ
      VX1(2) = YJ
      KK = 1
      GO TO 340
  300 ASSIGN 310 TO BACK
      ICID   = CID(2)
      VX1(1) = XI1J
      VX1(2) = YJ
      KK     = 1
      GO TO 340
  310 ASSIGN 320 TO BACK
      ICID   = CID(3)
      VX1(1) = XIJ1
      VX1(2) = YJ1
      KK     = 1
      GO TO 340
  320 ASSIGN 330 TO BACK
      ICID   = CID(4)
      VX1(1) = XI1J1
      VX1(2) = YJ1
      KK     = 1
      GO TO 340
  330 ASSIGN 360 TO BACK
      ICID   = CID(5)
      VX1(1) = XIC
      IF (NSIZE .EQ. 3) AUSET(6,2) = UK
      VX1(2) = YSP
      KK     = 2
  340 CALL GMMATS (ACPL,3,3,0,VX1,3,1,0,VX2)
      DO 350 K = 1,3
  350 VX2(K) = VX2(K) + RB1(K)
      CALL WRITE (BGPA,ACSIX,4,0)
      CALL WRITE (GPLA,ICID,1,0)
      CALL WRITE (USETA,AUSET(1,KK),6,0)
C
C     SIL AND EQEXIN
C
      NCRD  = NCRD + 1
      SILC  = SILC + 6
      ISILN = ISILN +6
      SILDX(4) = ISILN
      LUSETA = SILC
      SILDX(2) = 10*SILC + 1
      CALL WRITE (SILA,SILC,1,0)
      CALL WRITE (SCR2,ISILN,1,0)
      CALL WRITE (SCR2,SILC,1,0)
      CALL WRITE (SCR1,ICID,2,0)
      GO TO BACK, (300,310,320,330,360)
C
C     ECT
C
  360 CID(1) = IAPD(1,J  ,1,NCRDP)
      CID(2) = IAPD(2,J  ,1,NCRDP)
      CID(4) = IAPD(1,J+1,1,NCRDP)
      CID(3) = IAPD(2,J+1,1,NCRDP)
      CID(5) = CID(3) + 1
      CALL WRITE (ECTA,NECTA(1),6,0)
      AUSET(6,2) = USA
  100 CONTINUE
      CIDBX = NCID
C
C     PUT OUT ACPT REC
C
      IHEAD(2) = NTOT
      IHEAD(4) = NTHRY
      IF (NTHRY .EQ. 1) HEAD(10) = 0.0
      IHEAD(5) = NTHICK
      IHEAD(6) = IZ(IPID+1)
      IHEAD(7) = IZ(IPID+3)
      IHEAD(8) = IZ(IPID+5)
C
C     PROPERTY DATA
C
      IL = 0
      IN = NSPAN + 1
C
C     ALPHAS
C
      CALL APDOE (IZ(IPID+2),IZ,NAEF1,NAEF2,IL,NW)
      IF (IL .EQ. 0) GO TO 994
      IF (IHEAD(6) .EQ. 1) GO TO 50
      IHEAD(3) = NW/IN
      IF (MOD(NW,IN) .NE. 0) GO TO 994
      IHEAD(6) = NSPAN
      GO TO 60
   50 IHEAD(3) = NW/2
      IF (MOD(NW,2) .NE. 0) GO TO 994
C
C     INTEGRALS
C
   60 INT = 0
      ITN = 0
      IF (NTHICK .EQ. 0) GO TO 70
      CALL APDOE (NTHICK,IZ,NAEF1,NAEF2,INT,NWI)
      IF (INT .EQ. 0) GO TO 995
      IF (IHEAD(7).EQ.0 .AND. NWI.LT. 6) GO TO 995
      IF (IHEAD(7).NE.0 .AND. NWI.NE.12) GO TO 995
      GO TO 90
C
C     TAUS
C
   70 CALL APDOE (Z(IPID+6),IZ,NAEF1,NAEF2,ITN,NWT)
      IF (INT .EQ. 0) GO TO 993
      IF (IHEAD(8) .EQ. 0) GO TO 993
      IF (IHEAD(8) .EQ. 1) GO TO 80
      IF (NWT .NE. 3*NSPAN) GO TO 993
      IHEAD(8) = NSPAN
      GO TO 90
   80 IF (NWT .NE. 3) GO TO 993
C
C     THICKNESSES
C
   90 ITK = 0
      IF (NTHICK.NE.0 .AND. IHEAD(7).EQ.0) GO TO 99
      CALL APDOE (Z(IPID+4),IZ,NAEF1,NAEF2,ITK,NWTK)
      IF (ITK .EQ. 0) GO TO 992
      IF (INT .EQ. 0) GO TO 92
      IF (IHEAD(7) .NE. 1) IHEAD(7) = NSPAN
      IF (IHEAD(7).EQ.NSPAN .AND. NWTK.LT.NSPAN) GO TO 992
      GO TO 99
   92 IF (NWTK .LT. 2) GO TO 992
      IF (IHEAD(8).EQ.NSPAN .AND. NWTK.LT.2*NSPAN) GO TO 992
   99 CALL WRITE (ACPT,IHEAD,10,0)
      CALL WRITE (ACPT,Z(IOC+1),NSPAN*3,0)
      CALL WRITE (ACPT,Z(IL+1),NW,0)
      IF (INT .NE. 0) CALL WRITE (ACPT,Z(INT+1),NWI,0)
      IF (INT.NE.0 .AND. ITK.NE.0) CALL WRITE (ACPT,Z(ITK+1),NWTK,0)
      IF (ITN .NE. 0) CALL WRITE (ACPT,Z(ITN+1),NWT,0)
      IF (ITN .NE. 0) CALL WRITE (ACPT,Z(ITK+1),NWTK,0)
      CALL WRITE (ACPT,0,0,1)
  400 CONTINUE
      SILB = SILC
 1001 RETURN
C
C     ERROR MESSAGES
C
  992 I = IZ(IPID+4)
      GO TO 9941
  993 I = IZ(IPID+6)
      GO TO 9941
  994 I = IHEAD(6)
 9941 WRITE  (NOT,9942) UFM,I,EID
 9942 FORMAT (A23,' 2429, WRONG NUMBER OF WORDS OR CARD NOT FOUND FOR ',
     1       'CARD ID',I9, /29X,'ASSOCIATED WITH CAERO5 ID',I9)
      GO TO 1000
  995 I = NTHICK
      GO TO 9941
  996 CALL MESAGE (-8,0,NAM)
  997 I = PID
      GO TO 9941
  998 I = NCHORD
      GO TO 9941
  999 CALL EMSG (0,2323,1,2,0)
      WRITE  (NOT,891) PID,EID
  891 FORMAT (10X,16HPAERO5 CARD NO. ,I8,31H REFERENCED BY CAERO5 CARD N
     1O. ,I8,15H DOES NOT EXIST)
      GO TO 1000
 1000 CALL MESAGE (-61,0,NAM)
      GO TO 1001
      END
