      SUBROUTINE PLA41
C
C     THIS ROUTINE APPENDS DISPLACEMENT VECTOR INFORMATION TO THE
C     ECPTNL DATA BLOCK AND CREATES A SCRATCH DATA BLOCK, ECPTS, OF
C     THIS MERGED INFORMATION.  ECPTS IS PROCESSED BY SUBROUTINE PLA41.
C
      INTEGER         SYSBUF,CLSRW,BUFFR1,BUFFR2,UGV,ECPTNL,ECPTS,
     1                EOR,FILE,ELTYPE
      DIMENSION       NAME(2),MCBUGV(7),NWORDS(40),NGPTS(40),
     1                XECPT(100),IECPT(100)
      COMMON /SYSTEM/ SYSBUF
      COMMON /ZZZZZZ/ Z(1)
      COMMON /UNPAKX/ ITYPEB,IUNPK,JUNPK,INCUPK
      EQUIVALENCE     (XECPT(1),IECPT(1))
      DATA    UGV   , ECPTNL,ECPTS / 106,103,301   /
      DATA    NAME  / 4HPLA4,4H1   /
      DATA    EOR   , NEOR,CLSRW   / 1,0,1         /
C
C    1        ROD       BEAM      TUBE      SHEAR     TWIST
C    2        TRIA1     TRBSC     TRPLT     TRMEM     CONROD
C    3        ELAS1     ELAS2     ELAS3     ELAS4     QDPLT
C    4        QDMEM     TRIA2     QUAD2     QUAD1     DAMP1
C    5        DAMP2     DAMP3     DAMP4     VISC      MASS1
C    6        MASS2     MASS3     MASS4     CONM1     CONM2
C    7        PLOTEL    REACT     QUAD3     BAR       CONE
C    8          X         X         X         X         X
C
      DATA   NWORDS/
     1         20,        0,       19,        0,        0,
     2         33,        0,        0,       27,       20,
     3          0,        0,        0,        0,        0,
     4         32,       27,       32,       38,        0,
     5          0,        0,        0,        0,        0,
     6          0,        0,        0,        0,        0,
     7          0,        0,        0,       45,        0,
     8          0,        0,        0,        0,        0 /
      DATA   NGPTS /
     1          2,        2,        2,        4,        4,
     2          3,        3,        3,        3,        2,
     3          2,        2,        2,        2,        4,
     4          4,        3,        4,        4,        2,
     5          2,        2,        2,        2,        2,
     6          2,        2,        2,        2,        2,
     7          2,        0,        0,        2,        2,
     8          0,        0,        0,        0,        0 /
C
C     INITIALIZE
C
      IZMAX  = KORSZ(Z)
      BUFFR1 = IZMAX  - SYSBUF
      BUFFR2 = BUFFR1 - SYSBUF
      LEFT   = BUFFR2 - 1
C
C     READ THE DISPLACEMENT VECTOR INTO OPEN CORE.
C
      FILE = UGV
      CALL GOPEN (UGV,Z(BUFFR1),0)
      MCBUGV(1) = UGV
      CALL RDTRL (MCBUGV)
      IF (LEFT .LT. MCBUGV(3)) CALL MESAGE (-8,0,NAME)
      ITYPEB = 1
      IUNPK  = 1
      JUNPK  = MCBUGV(3)
      INCUPK = 1
      CALL UNPACK (*9050,UGV,Z(1))
      CALL CLOSE  (UGV,CLSRW)
C
C     OPEN THE ECPTNL AND ECPTS FILES.
C
      CALL GOPEN (ECPTS ,Z(BUFFR1),1)
      CALL GOPEN (ECPTNL,Z(BUFFR2),0)
C
C     READ AND WRITE THE PIVOT POINT
C
   10 CALL READ  (*60,*9030,ECPTNL,NPVT,1,NEOR,IFLAG)
      CALL WRITE (ECPTS,NPVT,1,NEOR)
C
C     READ ELEMENT TYPE
C
   20 CALL READ (*9020,*50,ECPTNL,ELTYPE,1,NEOR,IFLAG)
      J = NWORDS(ELTYPE)
      IF (J .LE. 0) CALL MESAGE (-30,114,IECPT(1))
C
C     READ THE ECPT ENTRY FOR THIS ELEMENT.
C
      CALL FREAD (ECPTNL,XECPT,J,0)
C
C     APPEND DISPLACEMENT VECTOR TO THE ECPT ENTRY
C
      J = J + 1
      NWDS = 3
      IF (ELTYPE .EQ. 34) NWDS = 6
      NOGPTS = NGPTS(ELTYPE)
      DO 40 I = 1,NOGPTS
      INDEX = IECPT(I+1)
      DO 30 K = 1,NWDS
      XECPT(J) = Z(INDEX)
      INDEX = INDEX + 1
   30 J = J + 1
   40 CONTINUE
C
C     THE ECPT ENTRY IS NOW COMPLETE.  WRITE IT OUT.
C
      CALL WRITE (ECPTS,ELTYPE, 1,NEOR)
      CALL WRITE (ECPTS,XECPT,J-1,NEOR)
      GO TO 20
C
C     AN EOR HAS BEEN READ ON ECPTNL.  WRITE EOR ON ECPTS.
C
   50 CALL WRITE (ECPTS,0,0,EOR)
      GO TO 10
C
C     PROCESSING IS COMPLETE.  CLOSE FILES.
C
   60 CALL CLOSE (ECPTNL,CLSRW)
      CALL CLOSE (ECPTS,CLSRW)
      RETURN
C
C     FATAL ERRORS
C
 9020 CALL MESAGE (-2,FILE,NAME)
 9030 CALL MESAGE (-3,FILE,NAME)
 9050 CALL MESAGE (-30,83,NAME)
      RETURN
      END
