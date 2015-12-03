      BLOCK DATA PLA4BD
CPLA4BD
C
      INTEGER         CSTM   ,ECPTS  ,GPCT   ,ECPTO  ,OUTRW  ,
     1                EOR    ,CLSRW  ,DIT
C
      COMMON /PLA42C/ NPVT   ,GAMI   ,GAMIP1 ,IPASS  ,ICSTM  ,
     1                NCSTM  ,IGPCT  ,NGPCT  ,IPOINT ,NPOINT ,
     2                I6X6K  ,N6X6K  ,CSTM   ,MPT    ,ECPTS  ,
     3                GPCT   ,DIT    ,KGGNL  ,ECPTO  ,INRW   ,
     4                OUTRW  ,EOR    ,NEOR   ,CLSRW  ,JMAX   ,
     5                FROWIC ,LROWIC ,NROWSC ,NLINKS ,NWORDS(40),
     6                IOVRLY(40)     ,LINK(40)       ,NOGO
C
      DATA    NPVT  , GAMI,GAMIP1,IPASS,ICSTM,NCSTM  / 6*0   /,
     1        IGPCT , NGPCT,IPOINT,NPOINT,I6X6K,N6X6K/ 6*0   /,
     2        CSTM  , MPT,GPCT,DIT, KGGNL,ECPTO, ECPTS       /
     3        101   , 102,104 ,105, 201  ,202  , 301         /,
     4        INRW  , OUTRW , EOR, NEOR, CLSRW / 0,1,1,0,1   /,
     5        JMAX  , FROWIC,LROWIC,NROWSC,NLINKS / 4*0, 1   /,
     6        NWORDS/
C
C    1         ROD       BEAM      TUBE      SHEAR     TWIST
C    2         TRIA1     TRBSC     TRPLT     TRMEM     CONROD
C    3         ELAS1     ELAS2     ELAS3     ELAS4     QDPLT
C    4         QDMEM     TRIA2     QUAD2     QUAD1     DAMP1
C    5         DAMP2     DAMP3     DAMP4     VISC      MASS1
C    6         MASS2     MASS3     MASS4     CONM1     CONM2
C    7         PLOTEL    REACT     QUAD3     BAR       CONE
C    8         TRIARG    TRAPRG    TORDRG    CORE      CAP
C
     1         26,       0,        25,       0,        0,
     2         42,       0,        0,        36,       26,
     3         0,        0,        0,        0,        0,
     4         44,       36,       44,       50,       0,
     5         0,        0,        0,        0,        0,
     6         0,        0,        0,        0,        0,
     7         0,        0,        0,        57,       0,
     8         0,        0,        0,        0,        0    /,
C
     7        IOVRLY/ 40*1/,
     8        NOGO  / 0   /
      END
