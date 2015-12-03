      SUBROUTINE PLA31
C
C     THIS ROUTINE READS THE INCREMENTAL DISPLACEMENT VECTOR INTO CORE
C     AND APPENDS THE PROPER DISPLACEMENT VALUES TO THE ESTNL ENTRY FOR
C     EACH ELEMENT, THEREBY CREATING THE ESTNLS, THE ESTNL SCRATCH FILE,
C     WHICH IS PROCESSED BY SUBROUTINE PLA32.
C
      INTEGER         BUFSZ,BUFR1,BUFR2,DELUGV,ESTNL,ESTNLS,FILE,EOR,
     1                CLSRW,IZ(1),IESTBK(100),ESTWDS(40),ELTYPE
      DIMENSION       NAME(2),NGPTS(40),MCBUGV(7),ESTBK(100)
      COMMON /BLANK / ICOM
      COMMON /SYSTEM/ BUFSZ
      COMMON /ZZZZZZ/ Z(1)
      COMMON /UNPAKX/ ITYPEB,IUNPK,JUNPK,INCUPK
      EQUIVALENCE     (Z(1),IZ(1)),(ESTBK(1),IESTBK(1))
      DATA    NAME  / 4HPLA3,4H1   /
      DATA    DELUGV, ESTNL,ESTNLS / 104,105,301/
      DATA    EOR   , NEOR,CLSRW   / 1,0,1      /
C
C    1        ROD       BEAM      TUBE      SHEAR     TWIST
C    2        TRIA1     TRBSC     TRPLT     TRMEM     CONROD
C    3        ELAS1     ELAS2     ELAS3     ELAS4     QDPLT
C    4        QDMEM     TRIA2     QUAD2     QUAD1     DAMP1
C    5        DAMP2     DAMP3     DAMP4     VISC      MASS1
C    6        MASS2     MASS3     MASS4     CONM1     CONM2
C    7        PLOTEL    REACT     QUAD3     BAR       CONE
C    8        TRIARG    TRAPRG    TORDRG    CORE      CAP
C
      DATA    ESTWDS/
     1           21,        0,       20,        0,        0,
     2           38,        0,        0,       27,       21,
     3            0,        0,        0,        0,        0,
     4           32,       32,       37,       43,        0,
     5            0,        0,        0,        0,        0,
     6            0,        0,        0,        0,        0,
     7            0,        0,        0,       50,        0,
     8            0,        0,        0,        0,        0 /
      DATA    NGPTS /
     1            2,        2,        2,        4,        4,
     2            3,        3,        3,        3,        2,
     3            2,        2,        2,        2,        4,
     4            4,        3,        4,        4,        2,
     5            2,        2,        2,        2,        2,
     6            2,        2,        2,        2,        2,
     7            2,        0,        0,        2,        2,
     8            3,        4,        2,        4,        2 /
C
C     DETERMINE SIZE OF CORE, DEFINE BUFFERS AND INITIALIZE CORE
C     POINTERS AND COUNTERS
C
      IZMAX = KORSZ (Z)
      BUFR1 = IZMAX - BUFSZ
      BUFR2 = BUFR1 - BUFSZ
      LEFT  = BUFR2 - 1
      IDISP = 0
C
C     OPEN THE DISPLACEMENT VECTOR FILE AND READ THE DISPLACEMENT VECTOR
C     INTO OPEN CORE.
C
      FILE = DELUGV
      CALL GOPEN (DELUGV,Z(BUFR1),0)
      MCBUGV(1) = DELUGV
      CALL RDTRL (MCBUGV)
      IF (LEFT .LT. MCBUGV(3)) CALL MESAGE (-8,0,NAME)
      ITYPEB = 1
      IUNPK  = 1
      JUNPK  = MCBUGV(3)
      INCUPK = 1
      CALL UNPACK (*9040,DELUGV,Z(IDISP+1))
      CALL CLOSE (DELUGV,CLSRW)
C
C     BUILD THE SCRATCH FILE ESTNLS
C
      CALL GOPEN (ESTNL,Z(BUFR1),0)
      CALL GOPEN (ESTNLS,Z(BUFR2),1)
C
C     READ AN ELEMENT TYPE FROM ESTNL AND WRITE IT ON ESTNLS.
C
   10 CALL READ (*60,*9030,ESTNL,ELTYPE,1,NEOR,IFLAG)
      NWDSRD = ESTWDS(ELTYPE)
      IF (NWDSRD .LE. 0) CALL MESAGE (-30,91,ELTYPE)
      CALL WRITE (ESTNLS,ELTYPE,1,NEOR)
C
C     READ AN ESTNL ENTRY
C
   20 J = NWDSRD
      CALL READ (*9020,*50,ESTNL,ESTBK,J,NEOR,IFLAG)
      NOGPTS = NGPTS(ELTYPE)
      IF (NOGPTS .LE. 0) CALL MESAGE (-30,92,ELTYPE)
C
C     APPEND THE DISPLACEMENT VECTORS ONTO THE ESTBK.
C
      NWDS = 3
      J = J + 1
      IF (ELTYPE.EQ. 1 .OR. ELTYPE.EQ. 3 .OR. ELTYPE.EQ.10 .OR.
     1    ELTYPE.EQ. 6 .OR. ELTYPE.EQ.17 .OR. ELTYPE.EQ.18 .OR.
     2    ELTYPE.EQ.19 .OR. ELTYPE.EQ.34) NWDS = 6
      DO 40 I = 1,NOGPTS
      INDEX = IDISP + IESTBK(I+1)
      DO 30 K = 1,NWDS
      ESTBK(J) = Z(INDEX)
      INDEX = INDEX + 1
   30 J = J + 1
   40 CONTINUE
C
C     THE APPENDED ESTNL ENTRY, WHICH IS AT ESTBK IS NOW COMPLETE.
C
      CALL WRITE (ESTNLS,ESTBK,J-1,NEOR)
      GO TO 20
C
C     WRITE AN EOR ON THE ESTNLS FILE.
C
   50 CALL WRITE (ESTNLS,0,0,EOR)
      GO TO 10
C
C     PROCESSING IS NOW COMPLETE
C
   60 CALL CLOSE (ESTNL,CLSRW)
      CALL CLOSE (ESTNLS,CLSRW)
      RETURN
C
C     FATAL ERRORS
C
 9020 CALL MESAGE (-2,FILE,NAME)
 9030 CALL MESAGE (-3,FILE,NAME)
 9040 CALL MESAGE (-5,DELUGV,NAME)
      RETURN
      END
