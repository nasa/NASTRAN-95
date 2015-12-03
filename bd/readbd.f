      BLOCK DATA READBD
CREADBD
      INTEGER         SCR1,SCR2,SCR3,SCR4,SCR5,SCR6,SCR7,ORDER,RSTRT,
     1                PHIA,OEIGS
      REAL            LMIN,LMAX
      COMMON /REGEAN/ IM(7),IK(7),IEV(7),SCR1,SCR2,SCR3,SCR4,SCR5,LCORE,
     1                RMAX,RMIN,MZ,NEV,EPSI,RMINR,NE,NIT,NEVM,SCR6,SCR7,
     2                NFOUND,LAMA,IBUCK,NSYM
      COMMON /INVPWX/ IFILK(7),IFILM(7),IFILLM(7),IFILVC(7),
     1                ISCR1,ISCR2,ISCR3,ISCR4,ISCR5,ISCR6,ISCR7,ISCR8,
     2                IDUMP,LMIN,LMAX,NOEST,NDPLUS,NDMNUS,EPS,NOVECT
      COMMON /GIVN  / G1,MO,MD,MR1,M1,M2,M3,M4,G2(8),RSTRT,NCOL,G3(2),
     1                G4(82),ORDER,G5(2),LGAMA,G6(4),OEIGS,PHIA,G7(2),
     2                MAX,X(35)
      DATA    MO    , MD ,MR1,M1 ,M2 ,M3 ,M4 ,LGAMA,OEIGS,PHIA /
     1        301   , 304,202,303,307,308,309,201  ,204  ,305  /
      DATA    ORDER , RSTRT,NCOL,MAX,IM     ,IK     ,IEV             /
     1        -2    , 0    ,0   ,253,102,6*0,101,6*0,302,0,0,2,1,0,0 /
      DATA    SCR1  , SCR2,SCR3,SCR4,SCR5,LAMA,SCR6,SCR7 /
     1        306   , 307 ,303 ,304 ,305 ,301 ,308 ,204  /
      DATA    RMAX  , RMIN,EPSI   ,RMINR      /
     1        100.0 , .01 ,1.0E-11,-.001      /
      DATA    MZ    , NEV,NE,NIT,NEVM,NFOUND  /
     1        0     , 9  ,4 ,30 ,5   ,0       /
      DATA    IFILK , IFILM  ,IFILLM ,IFILVC  /
     1        101,6*0,102,6*0,201,6*0,202,6*0 /
      DATA    ISCR1 ,ISCR2,ISCR3,ISCR4,ISCR5,ISCR6,ISCR7,ISCR8,IDUMP/
     1        301   ,302  ,303  ,304  ,305  ,306  ,307  ,308  ,204  /
      DATA    NOEST ,NDPLUS,NDMNUS,EPS  ,NOVECT,LMIN,LMAX,NSYM      /
     1        5     ,5     ,0     ,.0001,0     ,0.  ,60.  ,0        /
      END
