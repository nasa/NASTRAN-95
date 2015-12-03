      SUBROUTINE STPDA (INPUT,AJJL,SKJ)
C
C     DRIVER FOR STRIP THEORY
C
      INTEGER         SYSBUF,IZ(8),AJJL,SKJ,NAME(2),CLAF,LCLAF,LCIRC
      COMPLEX         EKM
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /STRIPC/ NS,BREF,CLAM,FM,NCIRC,NNCIRC,EKR(1),
     1                DUM,BB(4),BETA(4),EKM(4,4)
      COMMON /ZZZZZZ/ Z(1)
      COMMON /SYSTEM/ SYSBUF,NOUT
      COMMON /CONDAS/ PI,TWOPI
      COMMON /AMGMN / MCB(7),NROW,ND,NE,REFC,FMACH,RFK,TSKJ(7),ISK,NSK
      COMMON /BLANK / NK,NJ
      COMMON /PACKX / ITI,IT0,II,NN,INCR
      EQUIVALENCE     (IZ(1),Z(1))
      DATA     NAME / 4HSTPD,4HA   /
C
      ICORE = KORSZ(IZ) - 4*SYSBUF
C
C     BRING IN DATA AND ALLOCATE CORE
C
      CALL FREAD (INPUT,Z,8,0)
      NNJ   = IZ(1)
      CLAF  = IZ(2)
      LCLAF = IZ(3)
      NCIRC = IZ(4)
      LCIRC = IZ(5)
      NNCIRC= NCIRC + 1
      NMACH = IZ(6)
      NS    = IZ(7)
      I8    = 8
      CLAM  = Z(I8)
      FM    = 1.0
      BREF  = REFC / 2.0
      EKR(1)= RFK
      IDY   = 1
      IBLOC = IDY  + NS
      ID    = IBLOC+ NS
      ICA   = ID   + NS
      IGAP  = ICA  + NS
      INSIZE= IGAP + NS
      ICLA  = INSIZE + NS
      IBM   = ICLA + NS
      IGM   = IBM  + 16 * NS
      IPM   = IGM  + 12 * NS
      IOC   = IPM  + 37 * NS
      IF (IOC.GT.ICORE) CALL MESAGE (-8,0,NAME)
C
C     READ IN ARRAYS WHICH ARE FIXED
C
      NW = 6*NS
      CALL FREAD (INPUT,Z,NW,0)
C
C     SET CLA ARRAY OR BB AND BETA
C
      IF (CLAF .EQ. 0) GO TO 40
      IF (CLAF .LT. 0) GO TO 30
C
C     FIND MACH NUMBER FOR CLA
C
      DO 10 I = 1,NMACH
      CALL FREAD (INPUT,RM,1,0)
      IF (RM .EQ. FMACH) GO TO 20
      CALL FREAD (INPUT,Z,-NS,0)
   10 CONTINUE
      GO TO 999
C
C     MACH NUMBER NOT INPUT ON AEFACT CARD CLCAF
C
   20 CALL FREAD (INPUT,Z(ICLA),NS,1)
      GO TO 90
   30 CALL FREAD (INPUT,RM,1,0)
      CALL FREAD (INPUT,Z(ICLA),NS,1)
      DO  35 I = 1,NS
      Z(ICLA+I-1) = Z(ICLA+I-1) * SQRT((1.0-(RM*RM*CLAM*CLAM)) /
     1                           (1.0-(FMACH*FMACH*CLAM*CLAM)))
   35 CONTINUE
      GO TO 90
   40 DO 50 I = 1,NS
   50 Z(ICLA+I-1) = TWOPI
      IF (NCIRC .EQ. 0) GO TO 80
      DO 60 I = 1,NMACH
      CALL FREAD (INPUT,RM,1,0)
      IF (RM .EQ. FMACH) GO TO 70
      CALL FREAD (INPUT,Z,-(2*NCIRC+1),0)
   60 CONTINUE
      GO TO 998
   70 CALL FREAD (INPUT,BB(1),1,0)
      DO 75 I = 2,NNCIRC
      CALL FREAD (INPUT,BB(I),1,0)
      CALL FREAD (INPUT,BETA(I),1,0)
   75 CONTINUE
   80 CALL FREAD (INPUT,Z,0,1)
C
C     OUTPUT SKJ
C
   90 ITI = 1
      IT0 = 3
      II  = ISK
      NSK = NSK+1
      NN  = NSK
      RM  = 1.0
      DO 100 I = 1,NNJ
      CALL PACK (RM,SKJ,TSKJ)
      II  = II+1
      IF (I .EQ. NNJ) GO TO 100
      NN  = NN+1
  100 CONTINUE
      ISK = II
      NSK = NN
      ITI = 3
      IT0 = 3
      CALL STPBG  (Z(IBM),Z(IGM),NS,Z(IBLOC),Z(ID),Z(ICA),Z(INSIZE))
      CALL STPPHI (Z(ICA),Z(IBLOC),Z(IPM),NS)
      CALL STPAIC (Z(IBLOC),Z(IDY),Z(INSIZE),Z(IGAP),Z(IBM),Z(IGM),
     1             Z(IPM),NS,Z(ICLA),AJJL)
      NROW = NROW + NNJ
      RETURN
C
C     ERROR MESSAGES
C
  998 N = LCIRC
      GO TO 1000
  999 N = LCLAF
 1000 WRITE  (NOUT,9999) UFM,FMACH,N
 9999 FORMAT (A23,' 2426, MACH NUMBER ',F10.5,' WAS NOT FOUND ON ',
     1       'AEFACT CARD',I9)
      CALL MESAGE (-61,0,NAME)
      RETURN
      END
