      BLOCK DATA SDR2BD
CSDR2BD
      IMPLICIT INTEGER (A-Z)
C
      INTEGER         RFMTS(40)
C
      COMMON /SDR2X1/ IEIGEN,IELDEF,ITLOAD,ISYMFL,ILOADS,IDISPL,ISTR  ,
     1                IELF  ,IACC  ,IVEL  ,ISPCF ,ITTL  ,ILSYM ,IFROUT,
     2                ISLOAD,IDLOAD,ISORC
C
      COMMON /SDR2X2/ CASECC,CSTM  ,MPT   ,DIT   ,EQEXIN,SIL   ,GPTT  ,
     1                EDT   ,BGPDT ,PG    ,QG    ,UGV   ,EST   ,PHIG  ,
     2                EIGR  ,OPG1  ,OQG1  ,OUGV1 ,OES1  ,OEF1  ,PUGV1 ,
     3                OEIGR ,OPHIG ,PPHIG ,ESTA  ,GPTTA ,HARMS ,XYCDB ,
     4                SCR3  ,PCOMPS,OES1L ,OEF1L
C
      COMMON /SDR2X4/ NAM(2),END   ,MSET  ,ICB(7),OCB(7),MCB(7),DTYPE(8)
     1,               ICSTM ,NCSTM ,IVEC  ,IVECN ,TEMP  ,DEFORM,FILE  ,
     2                BUF1  ,BUF2  ,BUF3  ,BUF4  ,BUF5  ,ANY   ,ALL   ,
     3                TLOADS,ELDEF ,SYMFLG,BRANCH,KTYPE ,LOADS ,SPCF  ,
     4                DISPL ,VEL   ,ACC   ,STRESS,FORCE ,KWDEST,KWDEDT,
     5                KWDGPT,KWDCC ,NRIGDS,STA(2),REI(2),DS0(2),DS1(2),
     6                FRQ(2),TRN(2),BK0(2),BK1(2),CEI(2),PLA(22)      ,
     7                NRINGS,NHARMS,AXIC  ,KNSET ,ISOPL ,STRSPT,DDRMM ,
     8                ISOPL8
C
      EQUIVALENCE     (STA(1),RFMTS(1))
C
C*****
C     DATA DEFINING POSITIONS OF PARAMETERS IN A CASE CONTROL RECORD.
C*****
      DATA  IEIGEN/  5/,IELDEF/  6/,ITLOAD/  7/,ISYMFL/ 16/,ILOADS/ 17/,
     1      IDISPL/ 20/,ISTR  / 23/,IELF  / 26/,IACC  / 29/,IVEL  / 32/,
     2      ISPCF / 35/,ITTL  / 39/,ILSYM /200/,IFROUT/145/,ISLOAD/  4/,
     3      IDLOAD/ 13/,ISORC /136/
C*****
C     DATA DEFINING DATA BLOCK FILE NUMBERS.
C*****
      DATA  CASECC/101/,CSTM  /102/,MPT   /103/,DIT   /104/,EQEXIN/105/,
     1      SIL   /106/,GPTT  /107/,EDT   /108/,BGPDT /109/,PG    /110/,
     2      QG    /111/,UGV   /112/,EST   /113/,PHIG  /112/,EIGR  /110/,
     3      OPG1  /201/,OQG1  /202/,OUGV1 /203/,OES1  /204/,OEF1  /205/,
     4      PUGV1 /206/,OEIGR /201/,OPHIG /203/,PPHIG /206/,ESTA  /301/,
     5      GPTTA /302/,HARMS /137/,XYCDB /114/,SCR3  /303/,PCOMPS/116/,
     6      OES1L /207/,OEF1L /208/
C*****
C     DATA DEFINING RIGID FORMATS.
C*****
      DATA  NRIGDS/ 10   /,
     1      RFMTS / 4HSTAT,4HICS ,
     2              4HREIG,4HEN  ,
     3              4HDS0 ,4H    ,
     4              4HDS1 ,4H    ,
     5              4HFREQ,4H    ,
     6              4HTRAN,4HSNT ,
     7              4HBKL0,4H    ,
     8              4HBKL1,4H    ,
     9              4HCEIG,4HEN  ,
     O              4HPLA ,4H    ,
     1              20*0         /
C*****
C     MISC. DATA.
C*****
      DATA   NAM  / 4HSDR2,4H    /, END/4HEND /, DTYPE/2,3,1,5,4,6,7,8/,
     1       MSET / 1001/ ,ISOPL8/  0  /
C
      END
