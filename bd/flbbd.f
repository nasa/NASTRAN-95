      BLOCK DATA FLBBD
CFLBBD
C     FLBBD - BLOCK DATA FOR MODULE FLBMG
C
      INTEGER         GEOM2    ,ECT      ,BGPDT    ,SIL      ,GEOM3    ,
     1                CSTM     ,USET     ,EQEXIN   ,USETF    ,USETS    ,
     2                AF       ,DKGG     ,FBELM    ,FRELM    ,CONECT   ,
     3                AFMAT    ,AFDICT   ,KGMAT    ,KGDICT
C
C     GINO FILES
C
      COMMON /FLBFIL/ GEOM2    ,ECT      ,BGPDT    ,SIL      ,MPT      ,
     1                GEOM3    ,CSTM     ,USET     ,EQEXIN   ,USETF    ,
     2                USETS    ,AF       ,DKGG     ,FBELM    ,FRELM    ,
     3                CONECT   ,AFMAT    ,AFDICT   ,KGMAT    ,KGDICT
C
C     INPUT DATA BLOCKS
C
      DATA            GEOM2    ,ECT      ,BGPDT    ,SIL      ,MPT      ,
     1                GEOM3    ,CSTM     ,USET     ,EQEXIN             /
     2                101      ,102      ,103      ,104      ,105      ,
     3                106      ,107      ,108      ,109                /
C
C     OUTPUT DATA BLOCKS
C
      DATA            USETF    ,USETS    ,AF       ,DKGG               /
     1                201      ,202      ,203      ,204                /
C
C     INTERNAL SCRATCH FILES
C
      DATA            FBELM    ,FRELM    ,CONECT   ,AFMAT    ,AFDICT   ,
     1                KGMAT    ,KGDICT                                 /
     2                301      ,302      ,303      ,304      ,305      ,
     3                306      ,307                                    /
C
      END
