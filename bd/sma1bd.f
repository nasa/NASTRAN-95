      BLOCK DATA SMA1BD
CSMA1BD
C
      INTEGER           CLSRW    ,CLSNRW   ,EOR      ,OUTRW
      DOUBLE PRECISION  DPDUM(514)
      COMMON  /SMA1IO/  IFCSTM   ,IFMPT    ,IFDIT    ,IDUM1    ,
     1                  IFECPT   ,IGECPT   ,IFGPCT   ,IGGPCT   ,
     2                  IFGEI    ,IGGEI    ,IFKGG    ,IGKGG    ,
     3                  IF4GG    ,IG4GG    ,IFGPST   ,IGGPST   ,
     4                  INRW     ,OUTRW    ,CLSNRW   ,CLSRW    ,
     5                  NEOR     ,EOR      ,MCBKGG(7),MCB4GG(7)
C
C     SMA1 VARIABLE CORE BOOKKEEPING PARAMETERS
C
      COMMON  /SMA1BK/  ICSTM    ,NCSTM    ,IGPCT    ,NGPCT    ,
     1                  IPOINT   ,NPOINT   ,I6X6K    ,N6X6K    ,
     2                  I6X64    ,N6X64
      COMMON  /SMA1DP/  DPDUM
C
C     SMA1 PROGRAM CONTROL PARAMETERS
C
      COMMON  /SMA1CL/  IOPT4    ,K4GGSW   ,NPVT     ,LEFT     ,
     1                  FROWIC   ,LROWIC   ,NROWSC   ,TNROWS   ,
     2                  JMAX     ,NLINKS   ,LINK(10) ,IDETCK   ,
     3                  DODET    ,NOGOO    ,DUMMY(200)
C
C     ECPT COMMON BLOCK
C
      COMMON  /SMA1ET/  ECPT(200)
C
C
      DATA     NLINKS / 10 /
      DATA     NOGOO  /  0 /
      DATA     IFCSTM,IFMPT,IFECPT,IFGPCT,IFDIT / 101,102,103,104,105 /
      DATA     IFKGG,IF4GG,IFGPST / 201,202,203 /
      DATA     INRW,CLSRW,CLSNRW,EOR,NEOR,OUTRW / 0,1,2,1,0,1 /
      END
