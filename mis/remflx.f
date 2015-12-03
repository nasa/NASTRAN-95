      SUBROUTINE REMFLX (NGRIDS)
C
C     CHECK FOR REMFLUX IN MAGNETIC FIELD PROBLEMS WHEN COMPUTING
C     PROLATE SPHEROIDAL COEFFICIENTS
C
      LOGICAL         REMFL,HITONE
      INTEGER         REMFLD,SCR1,BUF1,BUF3,BUF2,HEST,MCB(7),FILE,
     1                POINTR(6,19),IPOINT(32),DIT,DITFIL,ELTYPE,ESTWDS
      DIMENSION       NAM(2),REM(3),ECPT(200),NECPT(200),IZ(1),G(3,3),
     1                IWORK(3,3)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /GPTA1 / NELEMS,LAST,INCR,NE(1)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,STRESS,SINTH,COSTH
      COMMON /HMTOUT/ XMAT(6)
      COMMON /HMATDD/ IIHMAT,NNHMAT,MPTFIL,DITFIL
      COMMON /UNPAKX/ JOUT,II,NN,JNCR
      COMMON /SYSTEM/ IBUF,IOUT
      COMMON /ZZZZZZ/ Z(1)
      COMMON /BIOT  / DUM(10),BUF1,REMFL,LCORE
      EQUIVALENCE     (Z(1),IZ(1)),(ECPT(1),NECPT(1))
      DATA    REMFLD, HEST,MPT,DIT, SCR1/
     1        107   , 108 ,109,110, 301 /
      DATA    NAM   / 4HREMF  ,4HLX     /
C
C                   TYPE  ISIL   MID   ITH NGRIDS ITEMP
C
      DATA    POINTR/ 1,    2,    4,    0,    2,    17,
     1                3,    2,    4,    0,    2,    16,
     2                6,    2,    6,    5,    3,    27,
     3                9,    2,    6,    5,    3,    21,
     4               10,    2,    4,    0,    2,    17,
     5               16,    2,    7,    6,    4,    26,
     6               17,    2,    6,    5,    3,    21,
     7               18,    2,    7,    6,    4,    26,
     8               19,    2,    7,    6,    4,    32,
     9               34,    2,   16,    0,    2,    42,
     O               36,    2,    6,    5,    3,    19,
     1               37,    2,    7,    6,    4,    24,
     2               39,    3,    2,    0,    4,    23,
     3               40,    3,    2,    0,    6,    33,
     4               41,    3,    2,    0,    8,    43,
     5               42,    3,    2,    0,    8,    43,
     6               65,    2,   10,    0,    8,    48,
     7               66,    2,   22,    0,   20,   108,
     8               67,    2,   34,    0,   32,   168  /
C
      REMFL  = .FALSE.
      MCB(1) = REMFLD
      CALL RDTRL (MCB)
C
C     CHECK FOR ANY REMFLUX
C
      IF (MCB(6) .EQ. 0) RETURN
C
C     TOO BAD
C
      REMFL  = .TRUE.
      NCOL   = MCB(2)
      NROW   = MCB(3)
      NCOUNT = NROW/3
C
C     BRING IN MATERIALS SINCE H=B/MU
C
      IIHMAT = NGRIDS
      NNHMAT = LCORE
      MPTFIL = MPT
      DITFIL = DIT
      CALL PREHMA (Z)
      NEXTZ  = NNHMAT + 1
C
      BUF2   = BUF1 - IBUF
      BUF3   = BUF2 - IBUF
C
C     SET UP POINTERS
C     IHC = START OF RESULTS HC = B/MU
C     IREM= REMFL COLUMN
C     ICT = COUNTER FOR NUMBER OF ELEMENTS AT EACH PROLATE GRID (FOR
C     AVERAGING
C
      IHC  = NEXTZ
      IREM = IHC + 3*NGRIDS
      ICT  = IREM + NROW
      IF (BUF3 .LT. ICT+NGRIDS) GO TO 1008
C
      CALL GOPEN (SCR1,Z(BUF1),1)
      CALL GOPEN (REMFLD,Z(BUF2),0)
      CALL GOPEN (HEST,Z(BUF3),0)
C
      II = 1
      NN = NROW
      JNCR = 1
      JOUT = 1
      JCOUNT = 0
C
    3 DO 5 I = 1,NGRIDS
    5 IZ(ICT+I) = 0
      N3 = 3*NGRIDS
      DO 6 I = 1,N3
    6 Z(IHC+I) = 0.
C
C     UNPACK A COULMN OF REMFLD
C
      JCOUNT = JCOUNT + 1
      CALL UNPACK (*20,REMFLD,Z(IREM+1))
      GO TO 40
C
C     ZERO COLUMN
C
   20 DO 30 I = 1,N3
   30 Z(IHC+I) = 0.
      GO TO 130
C
C     SINCE THE ELEMENT DATA DO NOT CHANGE WITH REMFLD COLIMN, THIS IS
C     NOT NECESSARILY THE BEST KIND OF LOOPING. BUT OTHER WAYS WOULD
C     NEED MORE CORE AND IF THERE IS MORE THAN ONE REMFLUX CASE, IT
C     WOULD BE A SURPRISE
C
   40 FILE  = HEST
      KOUNT = 0
   45 CALL READ (*100,*1003,HEST,ELTYPE,1,0,IFLAG)
      IDX   = (ELTYPE-1)*INCR
      ESTWDS= NE(IDX+12)
C
C     PICK UP ELEMENT TYPE INFO
C
      DO 50 I = 1,19
      JEL = I
      IF (ELTYPE-POINTR(1,I)) 500,60,50
   50 CONTINUE
      GO TO 500
C
   60 ISIL  = POINTR(2,JEL)
      IMID  = POINTR(3,JEL)
      ITH   = POINTR(4,JEL)
      IGRIDS= POINTR(5,JEL)
      ITEMP = POINTR(6,JEL)
C
   65 CALL READ (*1002,*45,HEST,ECPT,ESTWDS,0,IFLAG)
C
C     PICK UP REMFLUX FOR THIS ELEMENT
C
      KOUNT = KOUNT + 1
      NHIT  = 0
      HITONE= .FALSE.
      DO 650 I = 1,IGRIDS
  650 IPOINT(I) = 0
      DO 68 I = 1,NGRIDS
      DO 66 J = 1,IGRIDS
      IPT = NECPT(ISIL+J-1)
      IF (IPT .EQ. IZ(I)) GO TO 67
   66 CONTINUE
      GO TO 68
C
C     MATCH
C
   67 HITONE = .TRUE.
      NHIT   = NHIT + 1
      IZ(ICT+I) = IZ(ICT+I) + 1
      IPOINT(J) = I
      IF (NHIT .EQ. IGRIDS) GO TO 69
   68 CONTINUE
      IF (.NOT.HITONE) GO TO 65
   69 CONTINUE
C
      ISUB   = IREM + 3*(KOUNT-1)
      REM(1) = Z(ISUB+1)
      REM(2) = Z(ISUB+2)
      REM(3) = Z(ISUB+3)
C
C     PICK UP MATERIALS
C
      MATID  = NECPT(IMID)
      ELTEMP = ECPT(ITEMP)
      INFLAG = 3
      SINTH  = 0.
      COSTH  = 0.
      CALL HMAT (NECPT(1))
      G(1,1) = XMAT(1)
      G(1,2) = XMAT(2)
      G(1,3) = XMAT(3)
      G(2,1) = XMAT(2)
      G(2,2) = XMAT(4)
      G(2,3) = XMAT(5)
      G(3,1) = XMAT(3)
      G(3,2) = XMAT(5)
      G(3,3) = XMAT(6)
C
C     FOR COMMENTS ON MATERIALS SEE EM2D
C
      IF (ITH .EQ. 0) GO TO 80
      ANGLE = ECPT(ITH)*0.017453293
      IF (XMAT(3).EQ.0. .AND. XMAT(5).EQ.0.) GO TO 70
      GO TO 80
   70 IF (ABS(ANGLE) .LE. .0001) GO TO 80
      S   = SIN(ANGLE)
      C   = COS(ANGLE)
      CSQ = C*C
      SSQ = S*S
      CS  = C*S
      X2  = 2.*CS*XMAT(2)
      G(1,1) = CSQ*XMAT(1) - X2 + SSQ*XMAT(4)
      G(1,2) = CS*(XMAT(1) - XMAT(4)) + (CSQ-SSQ)*XMAT(2)
      G(2,2) = SSQ*XMAT(1) + X2 + CSQ*XMAT(4)
      G(2,1) = G(1,2)
      G(3,3) = XMAT(6)
      G(1,3) = 0.
      G(2,3) = 0.
      G(3,1) = 0.
      G(3,2) = 0.
C
C     SINCE MAT5 INFO FOR TRAPRG,TRIARG IS GIVEN IN X-Y ORDER,
C     INETRCHANGE YA AND Z
C
      TEMP   = G(2,2)
      G(2,2) = G(3,3)
      G(3,3) = TEMP
      TEMP   = G(1,2)
      G(1,2) = G(1,3)
      G(1,3) = TEMP
      G(2,1) = G(1,2)
      G(3,1) = G(1,3)
C
C     SOLVE MU*H = B
C
   80 CALL INVERS (3,G,3,REM,1,DET,ISING,IWORK)
      IF (ISING .EQ. 2) GO TO 510
C
C     REM NOW HAS HC- CHECK POINTER LIST TO SEE WHICH GRIDS ARE ON THE
C     SPHEROID AND ADD REMFLUX T THOSE ALREADY ACCUMULATED
C
      DO 90 I = 1,IGRIDS
      IF (IPOINT(I) .EQ. 0) GO TO 90
      ISUB = IHC + 3*(IPOINT(I)-1)
      DO 85 J = 1,3
      Z(ISUB+J) = Z(ISUB+J) + REM(J)
   85 CONTINUE
   90 CONTINUE
C
C     GO BACK FOR ANOTHER ELEMEENT
C
      GO TO 65
C
C     DONE WITH ALL TYPES-AVERAGE THE RESULTS BY NUMBER OF ELEMENTS AT
C     EACH
C
  100 DO 120 I = 1,NGRIDS
      DEN = FLOAT(IZ(ICT+I))
      IF (DEN .EQ. 0.) GO TO 120
      ISUB = 3*(I-1) + IHC
      DO 110 J = 1,3
      Z(ISUB+J) = Z(ISUB+J)/DEN
  110 CONTINUE
  120 CONTINUE
C
C     WRITE RESULTS TO SCR1
C
  130 CALL WRITE (SCR1,Z(IHC+1),3*NGRIDS,1)
C
C     GO BACK FOR ANOTHER REMFLD RECORD
C
      IF (JCOUNT .EQ. NCOL) GO TO 140
      CALL REWIND (HEST)
      CALL FWDREC (*1002,HEST)
      GO TO 3
C
C     DONE
C
  140 CALL CLOSE (SCR1,1)
      MCB(1) = SCR1
      MCB(2) = NCOL
      MCB(3) = 3*NGRIDS
      DO 150 I = 4,7
  150 MCB(I) = 0
      CALL WRTTRL (MCB)
      CALL CLOSE (HEST,1)
      CALL CLOSE (REMFLD,1)
      RETURN
C
  500 WRITE  (IOUT,501) UFM
  501 FORMAT (A23,', ILLEGAL ELEMENT TYPE IN REMFLX')
      GO TO  1061
  510 WRITE  (IOUT,511) UFM,MATID
  511 FORMAT (A23,', MATERIAL',I9,' IS SINGULAR IN REMFLX')
      GO TO  1061
C
 1002 N = -2
      GO TO 1010
 1003 N = -3
      GO TO 1010
 1008 N = -8
      FILE = 0
 1010 CALL MESAGE (N,FILE,NAM)
 1061 CALL MESAGE (-61,0,0)
      RETURN
      END
