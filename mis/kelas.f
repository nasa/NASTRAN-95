      SUBROUTINE KELAS (IJKLMN)
C*****
C THIS ROUTINE COMPUTES THE ELEMENT STIFFNESS AND STIFFNESS DAMPING
C 1 X 1 MATRICES FOR ELEMENTS ELAS1, ELAS2, ELAS3, ELAS4.
C*****
C
C
C
C              E C P T - S  F O R  E L A S  E L E M E N T S
C
C
C
C                  TYPE             TYPE           TYPE           TYPE
C         CELAS1           CELAS2         CELAS3         CELAS4
C ECPT(1) IELID     I      IELID     I    IELID      I   IELID      I
C ECPT(2) IGP1      I      K         R    IS1        I   K          R
C ECPT(3) IGP2      I      IGP1      I    IS2        I   IS1        I
C ECPT(4) IC1       I      IGP2      I    K          R   IS2        I
C ECPT(5) IC2       I      IC1       I    GSUBE      R
C ECPT(6) K         R      IC2       I    S          R
C ECPT(7) GSUBE     R      GSUBE     R
C ECPT(8) S         R      S         R
C
C
C
      DOUBLE PRECISION
     1                   KE
C
C
C
      DIMENSION
     1                   IECPT(5)
C
C
C
      COMMON   /SYSTEM/
     1                   ISYS
C
C SMA1 I/O PARAMETERS
C
      COMMON   /SMA1IO/
     1                   IFCSTM             ,IFMPT
     2,                  IFDIT              ,IDUM1
     3,                  IFECPT             ,IGECPT
     4,                  IFGPCT             ,IGGPCT
     5,                  IFGEI              ,IGGEI
     6,                  IFKGG              ,IGKGG
     7,                  IF4GG              ,IG4GG
     8,                  IFGPST             ,IGGPST
     9,                  INRW               ,OUTRW
     T,                  CLSNRW             ,CLSRW
     1,                  NEOR               ,EOR
     2,                  MCBKGG(7)          ,MCB4GG(7)
C
C SMA1 VARIABLE CORE BOOKKEEPING PARAMETERS
C
      COMMON   /SMA1BK/
     1                   ICSTM              ,NCSTM
     2,                  IGPCT              ,NGPCT
     3,                  IPOINT             ,NPOINT
     4,                  I6X6K              ,N6X6K
     5,                  I6X64              ,N6X64
C
C SMA1 PROGRAM CONTROL PARAMETERS
C
      COMMON   /SMA1CL/
     1                   IOPT4              ,K4GGSW
     2,                  NPVT               ,LEFT
     3,                  FROWIC             ,LROWIC
     4,                  NROWSC             ,TNROWS
     5,                  JMAX               ,NLINKS
     6,                  LINK(10)           ,IDETCK
     7,                  DODET              ,NOGO
C
C ECPT COMMON BLOCK
C
      COMMON   /SMA1ET/
     1                   ECPT(100)
C
C
C
      EQUIVALENCE
     1                   (IECPT(1),ECPT(1))
C
C
C
      DATA
     1                   ISCALR /0/
C
C
C
      IARG = IJKLMN
C
C MAKE THE ECPT-S FOR ALL ELAS ELEMENTS LOOK EXACTLY LIKE THE ECPT FOR
C ELAS1
C
      GO TO (50,10,30,40), IARG
C
C ELAS2
C
   10 SAVE = ECPT(2)
      DO 20 I = 3,6
   20 IECPT(I-1) = IECPT(I)
      ECPT(6) = SAVE
      GO TO 50
C
C ELAS3
C
   30 ECPT(7) = ECPT(5)
      ECPT(6)  = ECPT(4)
      IECPT(4) = 1
      IECPT(5) = 1
      GO TO 50
C
C ELAS4
C
   40 ECPT(6)  = ECPT(2)
      IECPT(2) = IECPT(3)
      IECPT(3) = IECPT(4)
      IECPT(4) = 1
      IECPT(5) = 1
C
C DETERMINE WHICH POINT IS THE PIVOT POINT AND SET APPROPRIATE POINTERS
C
   50 IND = 2
      IF (IECPT(2) .EQ. NPVT) GO TO 60
      IF (IECPT(3) .NE. NPVT) RETURN
      IPVT  = 3
      IPDOF = 5
      INPVT = 2
      INPDOF = 4
      IF (IECPT(2) .EQ. 0) IND = 1
      GO TO 80
C
C CHECK TO SEE IF BOTH POINTS MATCH THE PIVOT POINT.
C
   60 IF (IECPT(3) .NE. NPVT) GO TO 70
      IF (ISCALR .EQ. 0) GO TO 65
      ISCALR = 0
      RETURN
   65 ISCALR = 1
      IND = 4
   70 IPVT   = 2
      IPDOF  = 4
      INPVT  = 3
      INPDOF = 5
      IF (IECPT(3) .EQ. 0) IND = 1
   80 IF (IECPT(IPDOF)  .LE. 0) IECPT(IPDOF)  = 1
      IF (IECPT(INPDOF) .LE. 0) IECPT(INPDOF) = 1
C
C II AND JJ ARE THE ROW AND COLUMN INDICES OF THE MATRIX INTO WHICH THE
C SPRING AND SPRING DAMPING CONSTANTS WILL BE ADDED.
C
      II = IECPT(IPVT)  + IECPT(IPDOF)  - 1
      JJ = IECPT(INPVT) + IECPT(INPDOF) - 1
      KE = ECPT(6)
      INDEX = 6
      IFILE = IFKGG
   85 ASSIGN 100 TO IRETRN
      I = II
      J = II
   90 CALL SMA1B (KE,J,I,IFILE,0.0D0)
      IF (IND .EQ. 1) GO TO 130
      GO TO IRETRN, (100,110,120,130)
  100 ASSIGN 110 TO IRETRN
      KE = - KE
      J  =   JJ
      GO TO 90
  110 IF (IND .NE. 4) GO TO 130
      ASSIGN 120 TO IRETRN
      KE = ECPT(6)
      I = JJ
      GO TO 90
  120 ASSIGN 130 TO IRETRN
      KE = -KE
      J  = II
      GO TO 90
  130 IF (INDEX .EQ. 7)  RETURN
      IF (IOPT4 .EQ. 0  .OR.  IARG .EQ. 4) RETURN
C
C IF G SUB E IS NON-ZERO, SET PARAMETERS FOR K4GG INSERTION.
C
      IF (ECPT(7) .EQ. 0.0) RETURN
      K4GGSW = 1
      IFILE = IF4GG
      KE = ECPT(7) * ECPT(6)
      INDEX = 7
      GO TO 85
      END
