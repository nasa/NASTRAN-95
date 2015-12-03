      SUBROUTINE FA1K (IMETH,K,RHO,OUTFIL,ICO)
C
C     FA1K BUILDS AN INTERPOLATED MATRIX ON OUTFIL FROM QHHL OR FSAVE
C
      LOGICAL         NEW
      INTEGER         SYSBUF,OUT,BUFF,BUFF1,FLOOP,NS(2),TYPE,TRL(7),
     1                OUTFIL,FSAVE,QHHL,SCR2,SCR3,SCR4,MCB(7)
      REAL            K
      DIMENSION       Z(1)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /ZZZZZZ/ IZ(1)
      COMMON /UNPAKX/ IOUT,INN,NNN,INCR1
      COMMON /PACKX / ITI,ITO,II,NN,INCR
      COMMON /SYSTEM/ SYSBUF,OUT,DUM(52),IPREC
      COMMON /BLANK / FLOOP
      EQUIVALENCE     (IZ(1),Z(1))
      DATA    FSAVE / 201/, QHHL /104/, SCR2,SCR3,SCR4 /302,303,304/
      DATA    NS    / 4HFA1K,4H    /
C
      NCORE = KORSZ(IZ) - ICO
      BUFF  = NCORE - SYSBUF
      BUFF1 = BUFF  - SYSBUF
      TRL(1)= FSAVE
      CALL RDTRL (TRL)
C
C     READ IN DEPENDENT POINTS AND SET K AND RHO
C
      JJ    = TRL(3)*3
      IFIL  = FSAVE
      CALL GOPEN (FSAVE,IZ(BUFF+1),0)
      CALL READ (*430,*5,FSAVE,Z,JJ,1,NWR)
    5 CONTINUE
      I     = (FLOOP-1)*3 + 1
      CMACH = Z(I  )
      K     = Z(I+1)
      RHO   = Z(I+2)
      INCR1 = 1
      INCR  = 1
      II    = 1
      INN   = 1
      GO TO (10,100), IMETH
C
C     SURFACE SPLINE INTERPOLATION
C
   10 IF (FLOOP .NE. 1) GO TO 70
C
C     SET UP CALL TO SPLINE INTERPOLATOR
C
      NRHO = TRL(7)
      JI   = NRHO*3
      J    = 1
      DO 50 I = 1,JJ,JI
      Z(J  ) = Z(I  )
      Z(J+1) = Z(I+1)
   50 J    = J + 2
      ND   = JJ/JI
      NI   = TRL(4)
      TYPE = 1
      IDP  = 1
      IIP  = ND*2 + IDP
      IG   = IIP + 2*NI
      NI2  = NI*2
      CALL READ (*430,*60,FSAVE,Z(IIP),NI2,1,NWR)
   60 CALL FWDREC (*430,FSAVE)
      CALL CLOSE (FSAVE,2)
C
C     REWRITE QHHL SO EACH LIST MATRIX IS A COLUMN
C
      GO TO 300
   15 J  = 1
   20 JI = IG
      GO TO 350
   30 IF (J .EQ. NCOL) GO TO 40
      J  = J + 1
      GO TO 20
   40 CALL CLOSE (QHHL,1)
      CALL CLOSE (OUTFIL,1)
      CALL WRTTRL (TRL)
      GO TO 200
C
C     GET A COLUMN FROM FSAVE AND BUILD QHH ON OUTFIL
C
   70 NF = 2 + (FLOOP-1)/TRL(7)
      DO 80 I = 1,NF
      CALL FWDREC (*430,FSAVE)
   80 CONTINUE
   85 IOUT = TRL(6)
      ITI  = IOUT
      ITO  = IOUT
      NWC  = 1
      IF (ITO.EQ.2 .OR. ITO.EQ.3) NWC = 2
      IF (ITO .EQ. 4) NWC = 4
      MCB(1) = QHHL
      CALL RDTRL (MCB)
      NC   = MCB(3)
      NN   = NC
      NNN  = NC*NC
      CALL UNPACK (*410,FSAVE,Z)
      IJ   = 1
      CALL CLOSE (FSAVE,1)
      CALL GOPEN (OUTFIL,IZ(BUFF+1),1)
      MCB(1) = OUTFIL
      MCB(2) = 0
      MCB(3) = NC
      MCB(4) = 1
      MCB(5) = IOUT
      MCB(6) = 0
      MCB(7) = 0
      DO 90 I = 1,NC
      CALL PACK (Z(IJ),OUTFIL,MCB)
      IJ = IJ + NC*NWC
   90 CONTINUE
      CALL CLOSE (OUTFIL,1)
      CALL WRTTRL (MCB)
      GO TO 450
C
C     LINEAR SPLINE INTERPOLATION
C
C
C     IS A GOOD MATRIZ ON FSAVE
C
  100 EPS = .001
      NEW = .TRUE.
      NI  = TRL(4)
      IF (FLOOP .EQ. 1) GO TO 110
      OK    = Z(I-2)
      OMACH = Z(I-3)
      IF (ABS(CMACH-OMACH) .LT. EPS) NEW = .FALSE.
C
C     REWRITE QHHL IF NEW IS TRUE
C
      IF (.NOT.NEW) GO TO 180
      IF (FLOOP .NE. 1) GO TO 120
C
C     TEST TO SEE IF QHHL HAS ENOUGH MACH NUMBERS
C
  110 NIP  = NI*2
      NOGO = 0
      IIP  = JJ + 1
      CALL READ (*430,*111,FSAVE,Z(IIP),NIP,1,NWR)
  111 CALL BCKREC (FSAVE)
      TEMP = 0.0
      DO 119 I = 1,JJ,3
      IF (TEMP .EQ. Z(I)) GO TO 119
      TEMP = Z(I)
      NF   = 0
      DO 115 J = 1,NIP,2
      IF (TEMP-Z(IIP+J-1) .LT. EPS) NF = NF + 1
  115 CONTINUE
      IF (NF .GT. 1) GO TO 119
      WRITE (OUT,400) UFM,TEMP
      NOGO = 1
  119 CONTINUE
      IF (NOGO .EQ.1 ) GO TO 410
  120 J   = 1
      NRD = 0
      DO 125 I = 1,JJ,3
      IF (ABS(CMACH-Z(I)) .LT. EPS) GO TO 126
      GO TO 125
  126 IF (Z(I+2) .NE. RHO) GO TO 125
      Z(J  ) = Z(I  )
      Z(J+1) = Z(I+1)
      J   = J + 2
      NRD = NRD + 1
  125 CONTINUE
      IDP = 1
      IIP = NRD*2 + IDP
      NI2 = NI*2
      CALL READ (*430,*130,FSAVE,Z(IIP),NI2,1,NWR)
  130 CALL FWDREC (*430,FSAVE)
      CALL CLOSE (FSAVE,2)
      GO TO 300
  135 IG  = IIP + NI*2
      NF  = 0
      IK  = 1
      IFIL= QHHL
      JJ  = 2*NI + 1
      I   = 1
  138 IF (ABS(CMACH-Z(IIP+I-1)) .LT. EPS) GO TO 140
C
C     SKIP MATRIX
C
      DO 139 J = 1,NCM
  139 CALL FWDREC (*430,QHHL)
      GO TO 150
  140 Z(IIP+IK) = Z(IIP+I)
      IK = IK + 2
      NF = NF + 1
      JI = IG
      GO TO 350
  150 CONTINUE
      I  = I + 2
      IF (I .EQ. JJ) GO TO 160
      GO TO 138
  160 CALL CLOSE (QHHL,1)
      CALL CLOSE (OUTFIL,1)
      CALL WRTTRL (TRL)
C
C     SET UP CALL TO SPLINE INTERPOLATION
C
      TYPE = -1
      ND   = NRD
      NI   = NF
      GO TO 200
C
C     GET COLUMN FROM FSAVE AND BUILD QHH
C
  170 CALL GOPEN (FSAVE,IZ(BUFF+1),0)
      IJ = 3
  171 DO 175 I = 1,IJ
  175 CALL FWDREC (*430,FSAVE)
      GO TO 85
  180 IF (OK-K.EQ.0.0) GO TO 190
      TRL(7) = TRL(7) + 1
  190 IJ = TRL(7) + 1
      CALL WRTTRL (TRL)
      GO TO 171
C
C     CALL MINTRP
C
  200 IG   = IIP + 2*NI
      NC   = NCORE - IG
      NOGO = 0
      CALL MINTRP (NI,Z(IIP),ND,Z(IDP),TYPE,0,0,0.0,OUTFIL,SCR2,SCR3,
     1             SCR4,Z(IG),NC,NOGO,IPREC)
      IF (NOGO .EQ. 1) GO TO 410
C
C     INTERPOLATED MATRIX IS ON SCR2 MOVE TO FSAVE
C
      CALL OPEN (*430,FSAVE,IZ(BUFF+1),3)
      CALL GOPEN (SCR2,IZ(BUFF1+1),0)
      TRL(1) = SCR2
      CALL RDTRL (TRL)
      NCOL = TRL(2)
      NN   = TRL(3)
      NNN  = NN
      ITI  = TRL(5)
      ITO  = ITI
      IOUT = ITI
      TRL(1) = FSAVE
      TRL(2) = 0
      TRL(6) = 0
      TRL(7) = 0
      I = 1
  210 CALL UNPACK (*410,SCR2,Z)
      CALL PACK (Z,FSAVE,TRL)
      IF (I .EQ. NCOL) GO TO 230
      I = I + 1
      GO TO 210
  230 CALL CLOSE (SCR2,1)
      CALL CLOSE (FSAVE,1)
      CALL RDTRL (TRL)
      TRL(6) = ITO
      IF (IMETH .EQ. 2) TRL(7) = 1
      CALL WRTTRL (TRL)
      GO TO 170
C
C     SET UP COLUMN - MATRIX COPY
C
  300 CALL GOPEN (QHHL,IZ(BUFF+1),0)
      TRL(1) = QHHL
      CALL RDTRL (TRL)
      NCOL = TRL(2)/TRL(3)
      NCM  = TRL(3)
      CALL GOPEN (OUTFIL,IZ(BUFF1+1),1)
      NNN  = NCM
      NN   = NCM*NCM
      ITI  = TRL(5)
      ITO  = ITI
      IOUT = ITI
      NWC  = 1
      IF (ITO.EQ.2 .OR. ITO.EQ.3) NWC = 2
      IF (ITO .EQ. 4) NWC = 4
      TRL(1) = OUTFIL
      TRL(2) = 0
      TRL(3) = NN
      TRL(6) = 0
      TRL(7) = 0
      GO TO (15,135), IMETH
C
C     MAKE A COLUMN INTO MATRIX
C
  350 DO 390 ILOP = 1,NCM
      CALL UNPACK (*360,QHHL,Z(JI))
      GO TO 380
  360 N = NCM*NWC
      DO 370 IJ = 1,N
  370 Z(JI+IJ-1) = 0.0
  380 JI = JI + NCM*NWC
  390 CONTINUE
      CALL PACK (Z(IG),OUTFIL,TRL)
      GO TO (30,150), IMETH
C
C     ERROR MESSAGES
C
  400 FORMAT (A23,' 2270, LINEAR INTERPOLATION WITHOUT ENOUGH IND. ',
     1       'MACH NUMBERS EQUAL TO DEP. MACH ',F10.4)
  410 WRITE  (OUT,420) UFM
  420 FORMAT (A23,' 2271, INTERPOLATION MATRIX IS SINGULAR')
      GO TO  440
  430 CALL MESAGE (-3,IFIL,NS)
  440 CALL MESAGE (-61,0,NS)
  450 RETURN
      END
