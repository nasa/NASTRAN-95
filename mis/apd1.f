      SUBROUTINE APD1 (FST,NS,FCT,NC,LS,LC)
C
      LOGICAL         LS,LC
      INTEGER         IZ(1),NAME(2),FILE,NCARY(2),SILC,NECTA(6),
     1                CP,ACSID,EID,EIDB,CID(5),CIDBX,AUSET(6,2),SILB,
     2                RDREW,CLSREW,AYS(5),KEY(5),SILDX(4),ACSIX(4),BACK,
     3                SCR1,SCR2,SCR3,SCR4,SCR5,ECTA,BGPA,GPLA,USETA,
     4                SILA,CSTMA,ACPT,BUF10,BUF11,BUF12,WTREW,ACSIB,PID
      REAL            RB1(3),ACPL(3,3),VX1(3),VX2(3),AXIC(3),FST(1),
     1                FCT(1),XB(5)
      COMMON /BLANK / NK,NJ,LUSETA
      COMMON /SYSTEM/ SYSBUF,NOT
      COMMON /APD1C / EID,PID,CP,NSPAN,NCHORD,LSPAN,LCHORD,IGID,
     1                X1,Y1,Z1,X12,X4,Y4,Z4,X43,XOP,X1P,ALZO,MCSTM,
     2                NCST1,NCST2,CIDBX,ACSID,IACS,SILB,NCRD,
     3                SCR1,SCR2,SCR3,SCR4,SCR5,ECTA,BGPA,GPLA,USETA,
     4                SILA,CSTMA,ACPT,BUF10,BUF11,BUF12,NEXT,LEFT,ISILN
      COMMON /APD1D / ICPL(14),YP4,S1,C1,XP2,XP3,XP4,RA1(3)
      COMMON /APD12C/ KEY,AUSET,USA,UK,NCAM2,NASB,IPPC
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (ICPL(3),RB1(1)),(ICPL(6),ACPL(1,1)),
     1                (NECTA(2),CID(1)),(KEY(2),NP),(KEY(3),NSTRIP),
     2                (KEY(4),NTP),(KEY(5),F),(AYS(1),YS),(AYS(2),ZS),
     3                (AYS(3),EE),(AYS(4),SG),(AYS(5),CG),(AXIC(1),XIC),
     4                (AXIC(2),DELX),(AXIC(3),XLAM),(SILDX(1),ICID),
     5                (SILDX(3),SILC),(ACSIX(1),ACSIB),(Z(1),IZ(1)),
     6                (ACSIX(2),VX2(1)),(NECTA(1),EIDB)
      DATA    RDREW , CLSREW,WTREW / 0,1,1 /
      DATA    NAME  / 4HAPD1,1H    /
C
      KEY(1) = 1
      SILC   = SILB
C
C     IF NEW IGRID SET INITIALIZE
C
      IF (.NOT.LS) GO TO 30
      NP   = 0
      NTP  = 0
      NBOX = 0
      NASB = 0
      NSTRIP = 0
      CALL GOPEN (SCR3,Z(BUF10),WTREW)
      CALL GOPEN (SCR4,Z(BUF11),WTREW)
      CALL GOPEN (SCR5,Z(BUF12),WTREW)
C
C     MAKE COORD SYSTEM AND GET POINTS IN PROPER SYSTEM
C
   30 CALL APDCS
      SG = S1
      CG = C1
      ACSIB = MCSTM
C
C     CHECK FOR ASSOCIATED BODIES
C
      DO 40 J = 1,6
      IF (IZ(IPPC+J) .EQ. 0) GO TO 45
   40 NASB = NASB + 1
   45 CONTINUE
C
C     GENERATE BOXES
C
      NCRDP= NCRD
      NP   = NP + 1
      FSJ1 = APDF(FST,1,NSPAN)
      YJ1  = FSJ1*YP4
      DJ1  = FSJ1*XP4
      CJ1  = (1.0-FSJ1)*XP2 + FSJ1*(XP3-XP4)
      EIDB = EID - 1
      DO 370 J = 1,NS
      YJ   = YJ1
      DJ   = DJ1
      CJ   = CJ1
      FSJ1 = APDF(FST,J+1,NSPAN)
      YJ1  = FSJ1*YP4
      DJ1  = FSJ1*XP4
      CJ1  = (1.0-FSJ1)*XP2 + FSJ1*(XP3-XP4)
      EE   = .5*(YJ1-YJ)
      YSP  = YJ + EE
      NSTRIP = NSTRIP + 1
      FCI1 = APDF(FCT,1,NCHORD)
      XI1J = DJ + FCI1*CJ
      XI1J1= DJ1+ FCI1*CJ1
      DS   = 1.0/(YJ1-YJ)
      YS   = YSP*CG + RA1(2)
      ZS   = YSP*SG + RA1(3)
      CALL WRITE (SCR3,AYS(1),5,0)
      DO 370 I = 1,NC
      NTP  = NTP + 1
      XIJ  = XI1J
      XIJ1 = XI1J1
      FCI1 = APDF(FCT,I+1,NCHORD)
      XI1J = DJ + FCI1*CJ
      XI1J1= DJ1+ FCI1*CJ1
      AIJ  = (1.0-XOP)*XIJ  + XOP*XI1J
      AIJ1 = (1.0-XOP)*XIJ1 + XOP*XI1J1
      XIC  = .5*(AIJ+AIJ1)  + RA1(1)
      XLAM = (AIJ1-AIJ)*DS
      DELX = .50*(-XIJ+XI1J - XIJ1+XI1J1)
      CALL WRITE (SCR4,AXIC(1),3,0)
      XIC  = XIC - RA1(1)
      EIDB = EIDB + 1
      NBOX = NBOX + 1
      CID(1) = CIDBX + I + (NC+1)*(J-1)
      CID(2) = CID(1) + 1
      CID(3) = CID(1) + NC + 1
      CID(4) = CID(3) + 1
      CID(5) = EIDB
      NCID = CID(4)
      NJ   = NJ + 1
      NK   = NK + 2
      VX1(3) = 0
      IF (J .NE. 1) GO TO 310
      IF (I .NE. 1) GO TO 300
      ASSIGN 300 TO BACK
      ICID = CID(1)
      VX1(1) = XIJ
      VX1(2) = YJ
      KK = 1
      GO TO 340
  300 ASSIGN 310 TO BACK
      ICID = CID(2)
      VX1(1) = XI1J
      VX1(2) = YJ
      KK = 1
      GO TO 340
  310 IF (I .NE. 1) GO TO 320
      ASSIGN 320 TO BACK
      ICID = CID(3)
      VX1(1) = XIJ1
      VX1(2) = YJ1
      KK = 1
      GO TO 340
  320 ASSIGN 330 TO BACK
      ICID = CID(4)
      VX1(1) = XI1J1
      VX1(2) = YJ1
      KK = 1
      GO TO 340
  330 ASSIGN 360 TO BACK
      ICID = CID(5)
      VX1(1) = XIC + .25*DELX
      VX1(2) = YSP
      KK = 2
  340 CALL GMMATS (ACPL,3,3,0, VX1,3,1,0, VX2)
      DO 350 K = 1,3
  350 VX2(K) = VX2(K) + RB1(K)
      CALL WRITE (BGPA,ACSIX,4,0)
      CALL WRITE (GPLA,ICID,1,0)
      CALL WRITE (USETA,AUSET(1,KK),6,0)
      NCRD = NCRD + 1
      SILC = SILC + 6
      ISILN= ISILN + 6
      SILDX(4) = ISILN
      LUSETA = SILC
      SILDX(2) = 10*SILC + 1
      CALL WRITE (SILA,SILC,1,0)
      CALL WRITE (SCR2,ISILN,1,0)
      CALL WRITE (SCR2,SILC,1,0)
      CALL WRITE (SCR1,ICID,2,0)
      GO TO BACK, (300,310,320,330,360)
  360 CID(1) = IAPD(I  ,J  ,NC,NCRDP)
      CID(2) = IAPD(I+1,J  ,NC,NCRDP)
      CID(4) = IAPD(I  ,J+1,NC,NCRDP)
      CID(3) = IAPD(I+1,J+1,NC,NCRDP)
      CID(5) = CID(3) + 1
      CALL WRITE (ECTA,NECTA(1),6,0)
  370 CONTINUE
      CIDBX = NCID
      NCARY(1) = NC
      NCARY(2) = NBOX
      CALL WRITE (SCR5,NCARY,2,0)
C
C     ADD PROPERITY CARD POINTERS FOR APD2
C
      CALL WRITE (SCR5,IPPC,1,0)
      SILB = SILC
      IF(.NOT.LC) RETURN
C
C     WRITE ACPT TABLE
C
      F = X1P - XOP
      CALL WRITE (ACPT,KEY,5,0)
C
C     COPY STUFF FROM SCRATCH FILES TO ACPT
C
      FILE = SCR5
      K = 3
      ASSIGN 410 TO IRET
      GO TO 375
  410 ASSIGN 420 TO IRET
      FILE = SCR3
      K = 5
      GO TO 375
  420 ASSIGN 430 TO IRET
      FILE = SCR4
      K = 3
  375 CALL WRITE (FILE,0,0,1)
      CALL CLOSE (FILE,CLSREW)
      CALL GOPEN (FILE,Z(BUF12),RDREW)
      DO 400 I = 1,K
  380 CALL READ (*480,*390,FILE,XB(1),K,0,J)
C
C     SKIP PROPERTY CARD POINTERS
C
      IF (I.EQ.3 .AND. FILE.EQ.SCR5) GO TO 380
      CALL WRITE (ACPT,XB(I),1,0)
      GO TO 380
  390 CALL REWIND (FILE)
      CALL SKPREC (FILE,1)
  400 CONTINUE
      CALL CLOSE (FILE,CLSREW)
      GO TO IRET, (410,420,430)
  430 CALL WRITE (ACPT,0,0,1)
      RETURN
C
C     ERROR MESAGES
C
  480 IP1 = -2
      CALL MESAGE (IP1,FILE,NAME)
      RETURN
      END
