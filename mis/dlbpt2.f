      SUBROUTINE DLBPT2 (INPUT,W1JK,W2JK)
C
      INTEGER         W1JK,W2JK,SYSBUF,ECORE,TW1JK,TW2JK,NAME(2)
      DIMENSION       A(4),IZ(1)
      COMMON /PACKX / ITI,ITO,II,NN,INCR
      COMMON /AMGP2 / TW1JK(7),TW2JK(7)
      COMMON /AMGMN / MCB(7),NROW,ND,NE,REFC,FMACH,RFK
      COMMON /SYSTEM/ SYSBUF
      COMMON /ZZZZZZ/ NJ1,NK1,NP,NB,NTP,NBZ,NBY,NTZ,NTY,NT0,NTZS,NTYS,
     1                Z(1)
      EQUIVALENCE     (IZ(1),Z(1))
      DATA    NAME  / 4HDLBP,4HT2  /
C
C     GET CORE THEN SET POINTERS TO ACPT TABLE ARRAYS
C
      ECORE = KORSZ(IZ) - 4*SYSBUF
C
C     READ LENGTHS OF ARRAYS
C
      CALL FREAD (INPUT,NJ1,13,0)
C
C     COMPUTE POINTERS TO OPEN CORE
C
      IF (NTP .EQ. 0) CALL FREAD (INPUT,0,0,1)
      IF (NTP .EQ. 0) GO TO 50
      LNS  = IZ(1)
      INC  = 1
      INS  = INC
      INB  = INS + NP
      INAS = INB + NP
      IZIN = INAS
      IYIN = IZIN
      INBEA1 = IYIN   + NP
      INBEA2 = INBEA1 + NB
      INSBEA = INBEA2 + NB
      IZB  = INSBEA + NB
      IYB  = IZB  + NB
      IAVR = IYB  + NB
      IARB = IAVR + NB
      INFL = IARB + NB
      IXLE = INFL + NB
      IXTE = IXLE + NB
      INT121 = IXTE   + NB
      INT122 = INT121 + NB
      IZS = INT122 + NB
      N = 3*NP + 12*NB
C
C     READ FIXED ARRAYS
C
      IF (N .GT. ECORE) GO TO 180
      CALL FREAD (INPUT,IZ,N,0)
C
C     GET LENGTHS OF VARIABLE ARRAYS, PANELS THEN BODIES
C
      LNAS = 0
      IF (NP .EQ. 0) GO TO 20
      DO 10 I = 1,NP
   10 LNAS = LNAS + IZ(INAS+I-1)
   20 LNB  = 0
      LNSB = 0
      LNFL = 0
      LT1  = 0
      LT2  = 0
      DO 30 I = 1,NB
      K    = I - 1
      LNB  = LNB  + IZ(INBEA1+K)
      LNSB = LNSB + IZ(INSBEA+K)
      LNFL = LNFL + IZ(INFL+K)
      LT1  = LT1  + IZ(INT121+K)
   30 LT2  = LT2  + IZ(INT122+K)
C
C     READ VARIABLE  ARRAYS AND SET POINTERS TO CORE
C
      NEXT = N + 1
      N = 2*NB + 5*LNS + 4*NTP + 3*LNB + 4*LNSB + LNAS + 2*LNFL
     *  + LT1  + LT2
      IF (NEXT+N .GE. ECORE) GO TO 180
      CALL READ (*190,*190,INPUT,IZ(NEXT),N,1,NW)
      NEXT = NEXT+ N + 1
      IYS  = IZS + NB + LNS
      ICS  = IYS
      IEE  = ICS + NB + LNS
      ISG  = IEE + LNS
      ICG  = ISG + LNS
      IXIJ = ICG
      IX   = IXIJ+ LNS
      IDELX= IX  + NTP + LNB
C
C     COMPUTE TERMS AND PACK
C
      NN = II + 1
      DO 40 I = 1,NTP
      A(1) = 0.0
      A(2) = 1.0
      CALL PACK (A,W1JK,TW1JK)
      A(1) = -(2.0/REFC)
      A(2) = Z(IDELX+I-1)/(2.0*REFC)
      CALL PACK (A,W2JK,TW2JK)
C
C     BUMP PACK INDEXES
C
      II = II + 2
      IF (I .EQ. NTP) GO TO 40
      NN = NN + 2
   40 CONTINUE
   50 NTZY = NTZ + NTY
      IF (NTZY .EQ. 0) GO TO 70
      NN   = II + 1
      A(1) = 0.0
      A(2) = 0.0
      DO 60 I = 1,NTZY
      CALL PACK (A,W1JK,TW1JK)
      CALL PACK (A,W2JK,TW2JK)
   60 CONTINUE
   70 NTZY = NTZS + NTYS
      IF (NTZY .EQ. 0) GO TO 200
C
C     ANOTHER HARDER SHUFFLE
C
      III = II
      INBEA2 = INBEA2 - 1
      INSBEA = INSBEA - 1
      IFY = II
      IF (NBZ .EQ. 0) GO TO 120
      DO 110 I = 1,NBZ
      IBT = IZ(INBEA2+I)
      NBE = IZ(INSBEA+I)
      IF (IBT .EQ. 2) GO TO 90
      A(1) = 0.0
      A(2) = 1.0
      A(3) = -2.0/REFC
      A(4) = 0.0
      DO 80 J = 1,NBE
      NN = II + 1
      CALL PACK (A,W1JK,TW1JK)
      CALL PACK (A(3),W2JK,TW2JK)
      II  = II + 2
      IFY = II
   80 CONTINUE
      GO TO 110
   90 A(1) = 0.0
      A(4) = 0.0
      DO 100 J = 1,NBE
      NN   = II + 3
      A(2) = 0.0
      A(3) = 1.0
      CALL PACK (A,W1JK,TW1JK)
      A(2) = -2.0/REFC
      A(3) = 0.0
      CALL PACK (A,W2JK,TW2JK)
      II = II + 4
  100 CONTINUE
  110 CONTINUE
  120 IF (NBY .EQ. 0) GO TO 170
      II = IFY
      NBTD = NB - NBY + 1
      DO 160 I = NBTD,NB
      IBT = IZ(INBEA2+I)
      NBE = IZ(INSBEA+I)
      IF (IBT .EQ. 3) GO TO 140
      A(2) = 0.0
      A(3) = 0.0
      DO 130 J = 1,NBE
      NN   = II + 3
      A(1) = 0.0
      A(4) =-1.0
      CALL PACK (A,W1JK,TW1JK)
      A(1) = -2.0/REFC
      A(4) = 0.0
      CALL PACK (A,W2JK,TW2JK)
      II  = II + 4
  130 CONTINUE
      GO TO 160
  140 A(1) = 0.0
      A(2) =-1.0
      A(3) =-2.0/REFC
      A(4) = 0.0
      DO 150 J = 1,NBE
      NN = II + 1
      CALL PACK (A,W1JK,TW1JK)
      CALL PACK (A(3),W2JK,TW2JK)
      II = II + 2
  150 CONTINUE
  160 CONTINUE
  170 II = III + NTZY*2
      NN = II  - 1
      GO TO 200
C
C     ERROR MESSAGES
C
  180 CALL MESAGE (-8,0,NAME)
  190 CALL MESAGE (-7,0,NAME)
  200 RETURN
      END
