      SUBROUTINE APD3
C
      EXTERNAL        ORF
      LOGICAL         CNTRL1,CNTRL2,CRANK1,CRANK2
      INTEGER         NAM(2),IZ(1),BACK,PSPA,RET,IC(16),EID,PID,CIDBX,
     1                SILB,SCR1,ECTA,BGPA,GPLA,USETA,SILA,ACPT,BUF10,
     2                CA3S,CA3E,PA3S,PA3E,AUSET(6,2),SILC,ORF,USA,UK,
     3                EIDB,SILDX(2),ACSIX(4),CID(5),NECTA(2)
      REAL            VX1(3),VX2(3),ACPL(3,3),RB1(3)
      DIMENSION       IHEAD(10),BND(24)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /BLANK / NK,NJ,LUSETA
      COMMON /APD1C / EID,PID,CP,NSPAN,NCHORD,LSPAN,LCHORD,IGID,
     1                X1,Y1,Z1,X12,X4,Y4,Z4,X43,XOP,X1P,ALZO,MCSTM,
     2                NCST1,NCST2,CIDBX,ACSID,IACS,SILB,NCRD,SCR1,
     3                SCR2,SCR3,SCR4,SCR5,ECTA,BGPA,GPLA,USETA,SILA,
     4                CSTMA,ACPT,BUF10,BUF11,BUF12,NEXT,LEFT,ISILN,
     5                NCAM,NAEF1,NAEF2,
     6                NCA1,NCA2,CA2S,CA2E,CA3S,CA3E,CA4S,CA4E,
     7                NPA1,NPA2,PA2S,PA2E,PA3S,PA3E,PA4S,PA4E
      COMMON /APD1D / ICPL(14),YP4,S1,C1,XP2,XP3,XP4,RA1(3)
      COMMON /ZZZZZZ/ Z(1)
      COMMON /BITPOS/ IBIT(64)
      COMMON /TWO   / ITWO(32)
      COMMON /SYSTEM/ SYSBUF,NOT
      EQUIVALENCE     (ICPL(3),RB1(1)),(ICPL(6),ACPL(1,1)),
     1                (NECTA(1),EIDB),(NECTA(2),CID(1)),
     2                (ACSIX(2),VX2(1)),(SILDX(1),ICID),
     3                (Z(1),IZ(1)),(EID,IC(1)),
     4                (CRANK1,IHEAD(3)),(CRANK2,IHEAD(4)),
     5                (CNTRL1,IHEAD(5)),(CNTRL2,IHEAD(6))
      DATA     NAM  / 4HAPD3,4H    /
C
      NOGO = 0
      LCA  = 16
      NC3  = ((CA3E-CA3S)+1)/LCA
      NCAM = NCAM+NC3
C
C     INITIAL SETUP
C
      I17 = IBIT(17)
      I18 = IBIT(18)
      I19 = IBIT(19)
      I20 = IBIT(20)
      PSPA= ORF(ITWO(I17),ITWO(I20))
      USA = ORF(PSPA,ITWO(I18))
      UK  = ORF(ITWO(I19),ITWO(I20))
      DO 5 J = 1,2
      DO 5 I = 1,6
    5 AUSET(I,J) = USA
      AUSET(3,2) = UK
      IHEAD(1) = 3
      SILC = SILB
C
C     LOOP ON NC3 MOVING CAERO3 CARD TO COMMON
C
      DO 400 I = 1,NC3
      N = (I-1)*LCA - 1
      DO 10 J = 1,LCA
      IC(J) = IZ(CA3S+N+J)
   10 CONTINUE
      MCSTM = MCSTM + 1
      IZ(CA3S+N+2) = MCSTM
      IZ(CA3S+N+8) = 3
      ACSIX(1) = MCSTM
C
C     GET POINTS IN PROPER COORD SYSTEM
C
      CALL APDCS
C
C     FIND PAERO3 CARD
C
      J = PA3S
   20 IF (J .GE. PA3E) GO TO 999
      IF (IZ(J) .EQ. PID) GO TO 30
      J = J + 4 + IZ(J+3)
      GO TO 20
   30 IPID = J
      IHEAD(7) = IZ(IPID+1)
      CRANK1 = .FALSE.
      CRANK2 = .FALSE.
      IF (Z(IPID+5) .GT. 0.0) CRANK1 = .TRUE.
      IF (Z(IPID+7) .GT. 0.0) CRANK2 = .TRUE.
      CNTRL1 = .FALSE.
      CNTRL2 = .FALSE.
      IF (IZ(IPID+2) .GT. 0 ) CNTRL1 = .TRUE.
      IF (IZ(IPID+2) .EQ. 2 ) CNTRL2 = .TRUE.
C
C     GENERATE AERO POINTS FOR CAERO3  PUT POINTS 1-4 IN BGPDT
C
      DO 40 J = 13,24
   40 BND(J) = 0.0
      VX1(3) = 0.0
      KK = 1
      ASSIGN 50 TO BACK
      IBS = NCRD + 1
      VX1(1) = 0.0
      VX1(2) = 0.0
      BND(1) = 0.0
      BND(2) = 0.0
      GO TO 300
   50 ASSIGN 60 TO BACK
      VX1(1) = X12
      VX1(2) = 0.0
      BND(7) = X12
      BND(8) = 0.0
      GO TO 300
   60 VX1(1) = XP4
      VX1(2) = YP4
      BND(5) = XP4
      BND(6) = YP4
      ASSIGN 70 TO BACK
      GO TO 300
   70 ASSIGN 80 TO BACK
      VX1(1) = XP4 + X43
      VX1(2) = YP4
      BND(11) = VX1(1)
      BND(12) = VX1(2)
      GO TO 300
C
C     ADD POINTS 5 AND 6 IF THEY EXIST
C
   80 BND(3) = BND(5)
      BND(4) = BND(6)
      IF (.NOT.CRANK1) GO TO 90
      VX1(1) = Z(IPID+4)
      VX1(2) = Z(IPID+5)
      BND(3) = VX1(1)
      BND(4) = VX1(2)
      ASSIGN 90 TO BACK
      GO TO 300
   90 BND(9) = BND(11)
      BND(10) = BND(12)
      IF (.NOT.CRANK2) GO TO 100
      VX1(1) = Z(IPID+6)
      VX1(2) = Z(IPID+7)
      BND(9) = VX1(1)
      BND(10)= VX1(2)
      ASSIGN 100 TO BACK
      GO TO 300
C
C      ADD CONTROLS
C
  100 IF (.NOT.CNTRL1) GO TO 120
      ASSIGN 101 TO BACK
      VX1(1)  = Z(IPID+8)
      VX1(2)  = Z(IPID+9)
      BND(15) = VX1(1)
      BND(16) = VX1(2)
      GO TO 300
  101 ASSIGN 102 TO BACK
      VX1(1)  = Z(IPID+10)
      VX1(2)  = Z(IPID+11)
      BND(13) = VX1(1)
      BND(14) = VX1(2)
      GO TO 300
  102 ASSIGN 103 TO BACK
      VX1(1)  = Z(IPID+12)
      VX1(2)  = Z(IPID+13)
      BND(17) = VX1(1)
      BND(18) = VX1(2)
      GO TO 300
  103 ASSIGN 104 TO BACK
      VX1(1)  = Z(IPID+14)
      VX1(2)  = Z(IPID+15)
      BND(21) = VX1(1)
      BND(22) = VX1(2)
      GO TO 300
  104 IF (.NOT.CNTRL2) GO TO 120
      ASSIGN 105 TO BACK
      VX1(1)  = Z(IPID+16)
      VX1(2)  = Z(IPID+17)
      BND(19) = VX1(1)
      BND(20) = VX1(2)
      GO TO 300
  105 ASSIGN 120 TO BACK
      VX1(1)  = Z(IPID+18)
      VX1(2)  = Z(IPID+19)
      BND(23) = VX1(1)
      BND(24) = VX1(2)
      GO TO 300
C
C     CONNECT POINT TO BOXES FOR ECTA
C
  120 EIDB   = EID
      CID(1) = IBS
      CID(2) = IBS + 1
      CID(5) = IBS
      IF (CRANK1) GO TO 121
      IF (CRANK2) GO TO 122
      CID(3) = IBS + 3
      CID(4) = IBS + 2
      GO TO 124
  121 IF (CRANK2) GO TO 123
      CID(3) = IBS + 3
      CID(4) = IBS + 4
      GO TO 124
  122 CID(3) = IBS + 4
      CID(4) = IBS + 2
      GO TO 124
  123 CID(3) = IBS + 5
      CID(4) = IBS + 4
  124 CONTINUE
      CALL WRITE (ECTA,NECTA,6,0)
      EIDB   = EIDB + 1
      CID(1) = IBS + 2
      CID(2) = IBS + 3
      CID(5) = IBS + 2
      IBS    = IBS + 4
      IF (.NOT.CRANK1 .AND. .NOT.CRANK2) GO TO 130
      IF (CRANK1 .AND. CRANK2) GO TO 125
      CID(3) = IBS
      CID(4) = CID(5)
      IBS = IBS + 1
      GO TO 129
  125 CID(3) = IBS + 1
      CID(4) = IBS
      IBS    = IBS + 2
  129 CALL WRITE (ECTA,NECTA,6,0)
      EIDB   = EIDB + 1
  130 IF (.NOT.CNTRL1) GO TO 135
      CID(1) = IBS + 2
      CID(2) = IBS + 3
      CID(3) = IBS + 1
      CID(4) = IBS
      CID(5) = IBS + 2
      CALL WRITE (ECTA,NECTA,6,0)
      EIDB   = EIDB + 1
      IF (.NOT.CNTRL2) GO TO 135
      CID(3) = IBS+5
      CID(4) = IBS+4
      CALL WRITE (ECTA,NECTA,6,0)
C
C     FIND CONTROL POINTS FOR ELEMENT
C
  135 CALL APDOE (NSPAN,IZ,NAEF1,NAEF2,ILW,NWW)
      IF (ILW .EQ. 0) GO TO 998
      IF (NWW .LT. 6) GO TO 998
      IF (MOD(NWW,2) .NE. 0) GO TO 998
      ILC1 = 0
      ILC2 = 0
      NWC1 = 0
      NWC2 = 0
      IF (.NOT.CNTRL1) GO TO 140
      CALL APDOE (NCHORD,IZ,N AEF1,NAEF2,ILC1,NWC1)
      IF (ILC1 .EQ. 0) GO TO 997
      IF (NWC1 .LT. 6) GO TO 997
      IF (MOD(NWC1,2) .NE. 0) GO TO 997
      IF (.NOT.CNTRL2) GO TO 140
      CALL APDOE (LSPAN,IZ,NAEF1,NAEF2,ILC2,NWC2)
      IF (ILC2 .EQ. 0) GO TO 996
      IF (NWC2 .LT. 6) GO TO 996
      IF (MOD(NWC2,2) .NE. 0) GO TO 996
  140 IHEAD( 8) = NWW/2
      IHEAD( 9) = NWC1/2
      IHEAD(10) = NWC2/2
      IHEAD( 2) = IHEAD(8)+IHEAD(9)+IHEAD(10)
      NK = NK + IHEAD(2)
      NJ = NJ + IHEAD(2)
      IZ(CA3S+N+4) = IHEAD(2)
      IZ(CA3S+N+5) = 1
C
C     START THE ACPT AND ADD THE CONTROL POINTS IN A LOOP
C
      CALL WRITE (ACPT,IHEAD,10,0)
      CALL WRITE (ACPT,BND,24,0)
      EIDB = EID - 1
      KK  = 2
      NN  = NWW
      KKK = ILW - 1
      ASSIGN 150 TO RET
      GO TO 190
  150 IF (IHEAD(9) .EQ. 0) GO TO 180
      ASSIGN 160 TO RET
      NN  = NWC1
      KKK = ILC1 - 1
      GO TO 190
  160 IF (IHEAD(10) .EQ. 0) GO TO 180
      ASSIGN 180 TO RET
      NN  = NWC2
      KKK = ILC2 - 1
      GO TO 190
  180 CALL WRITE (ACPT,0,0,1)
C
C     GEOMETRY CHECKS
C
      NM = 0
      IF (BND(1) .GT.  BND(3)) NM = 1
      IF (BND(3) .GT.  BND(5)) NM = 1
      IF (BND(15).GT. BND(17)) NM = 1
      IF (CNTRL2 .AND. BND(17).GT.BND(19)) NM = 1
      IF (BND(16) .LT. BND(14)) NM = 1
      IF (BND(18) .LT. BND(22)) NM = 1
      IF (BND(20) .LT. BND(24)) NM = 1
      IF (NM .EQ. 1) NOGO = 1
      IF (NM .EQ. 1) WRITE (NOT,1851) UFM,EID
 1851 FORMAT (A23,' 2278, PLANFORM GEOMETRY FOR CAERO3 ID',I9,
     1       ' IS IN ERROR', /5X,'CHECK SWEEP  ANGLE FOR LEADING EDGE ',
     2       'OR CONTROL SURFACE HINGE LINE.')
      GO TO 400
C
C     PUT CONTROL POINTS IN TABLE
C
  190 J = 2
  195 CONTINUE
      VX1(1) = Z(KKK+J  )
      VX1(2) = Z(KKK+J+1)
      CALL WRITE (ACPT,VX1,2,0)
      ASSIGN 200 TO BACK
      GO TO 300
  200 CONTINUE
      J = J + 2
      IF (J .LE. NN) GO TO 195
      GO TO RET, (150,160,180)
C
C     BGPA  GPL  USET
C
  300 CALL GMMATS (ACPL,3,3,0,VX1,3,1,0,VX2)
      DO 310 K = 1,3
  310 VX2(K) = VX2(K) + RB1(K)
      CALL WRITE (BGPA,ACSIX,4,0)
      IF (KK .EQ. 2) GO TO 320
      CIDBX = CIDBX + 1
      ICID  = CIDBX
      GO TO 330
  320 EIDB  = EIDB + 1
      ICID  = EIDB
  330 CALL WRITE (GPLA,ICID,1,0)
      CALL WRITE (USETA,AUSET(1,KK),6,0)
C
C     SIL AND EQEXIN
C
      NCRD  = NCRD + 1
      SILC  = SILC + 6
      ISILN = ISILN + 6
      LUSETA= SILC
      SILDX(2) = 10*SILC + 1
      CALL WRITE (SILA,SILC,1,0)
      CALL WRITE (SCR2,ISILN,1,0)
      CALL WRITE (SCR2,SILC,1,0)
      CALL WRITE (SCR1,ICID,2,0)
      GO TO BACK, (50,60,70,80,90,100,101,102,103,104,105,120,200)
  400 CONTINUE
      SILB = SILC
      IF (NOGO .EQ. 1) GO TO 1001
 1000 RETURN
C
C     ERROR MESSAGES
C
  996 I = LSPAN
      GO TO 9941
  997 I = NCHORD
      GO TO 9941
  998 I = NSPAN
 9941 WRITE  (NOT,9942) UFM,I,EID
 9942 FORMAT (A23,' 2429, WRONG NUMBER OF WORDS OR CARD NOT FOUND FOR ',
     1       'CARD ID',I9, /29X,'ASSOCIATED WITH CAERO3 ID',I9)
      GO TO 1001
  999 CALL EMSG (0,2323,1,2,0)
      WRITE  (NOT,891) PID,EID
  891 FORMAT (10X,16HPAERO3 CARD NO. ,I8,31H REFERENCED BY CAERO3 CARD N
     *O. ,I8,15H DOES NOT EXIST)
 1001 CALL MESAGE (-61,0,NAM)
      GO TO 1000
      END
