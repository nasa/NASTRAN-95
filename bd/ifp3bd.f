      BLOCK DATA IFP3BD
CIFP3BD
C     B L O C K   D A T A   F O R   I F P 3
C
C
      INTEGER            FILE          ,INAME         ,CDTYPE
      INTEGER            AXIC1         ,CCONEX        ,FORCEX
      INTEGER            FORCE         ,GRAV          ,LOAD
      INTEGER            MOMAX         ,MOMENT        ,MPCADD
      INTEGER            MPCAX         ,OMITAX        ,POINTX
      INTEGER            PRESAX        ,RINGAX        ,SECTAX
      INTEGER            SEQGP         ,SPCAX         ,SUPAX
      INTEGER            TEMPAX        ,TEMPD         ,PLOAD
      INTEGER            MPC           ,SPC           ,GRID
      INTEGER            SUPORT        ,NEG111        ,T65535
      INTEGER            TEMP          ,OMIT          ,SPCADD
      INTEGER            ONE           ,ZERO
      INTEGER            CTRIAA        ,CTRAPA
      INTEGER            RFORCE
C
      COMMON /IFP3CM /  FILE(6)        ,INAME(12)     ,CDTYPE(50)
     1                  ,AXIC1(3)      ,CCONEX(3)     ,FORCEX(3)
     2                  ,FORCE(3)      ,GRAV(3)       ,LOAD(3)
     3                  ,MOMAX(3)      ,MOMENT(3)     ,MPCADD(3)
     4                  ,MPCAX(3)      ,OMITAX(3)     ,POINTX(3)
     5                  ,PRESAX(3)     ,RINGAX(3)     ,SECTAX(3)
     6                  ,SEQGP(3)      ,SPCAX(3)      ,SUPAX(3)
     7                  ,TEMPAX(3)     ,TEMPD(3)      ,PLOAD(3)
     8                  ,MPC(3)        ,SPC(3)        ,GRID(3)
     9                  ,SUPORT(3)     ,NEG111(3)     ,T65535(3)
     T                  ,TEMP(3)       ,OMIT(3)       ,SPCADD(3)
     1                  ,ONE           ,ZERO          ,IHEADB(96)
     2                  ,CTRIAA(3)     ,CTRAPA(3)     ,ICONSO
     3                  ,RFORCE(3)
C
      DATA ONE/1/ , ZERO/0/
C
      DATA FILE  ( 1), FILE  ( 2) / 201   , 208    /
      DATA FILE  ( 3), FILE  ( 4) / 209   , 210    /
      DATA FILE  ( 5), FILE  ( 6) / 301   , 215    /
C
      DATA INAME ( 1), INAME ( 2) / 4HGEOM, 4H1    /
      DATA INAME ( 3), INAME ( 4) / 4HGEOM, 4H2    /
      DATA INAME ( 5), INAME ( 6) / 4HGEOM, 4H3    /
      DATA INAME ( 7), INAME ( 8) / 4HGEOM, 4H4    /
      DATA INAME ( 9), INAME (10) / 4HSCRT, 4HCH   /
      DATA INAME (11), INAME (12) / 4HAXIC, 4H     /
C
      DATA CDTYPE( 1), CDTYPE( 2) / 4HAXIC, 4H     /
      DATA CDTYPE( 3), CDTYPE( 4) / 4HCCON, 4HEAX  /
      DATA CDTYPE( 5), CDTYPE( 6) / 4HFORC, 4HEAX  /
      DATA CDTYPE( 7), CDTYPE( 8) / 4HFORC, 4HE    /
      DATA CDTYPE( 9), CDTYPE(10) / 4HGRAV, 4H     /
      DATA CDTYPE(11), CDTYPE(12) / 4HLOAD, 4H     /
      DATA CDTYPE(13), CDTYPE(14) / 4HMOMA, 4HX    /
      DATA CDTYPE(15), CDTYPE(16) / 4HMOME, 4HNT   /
      DATA CDTYPE(17), CDTYPE(18) / 4HMPCA, 4HDD   /
      DATA CDTYPE(19), CDTYPE(20) / 4HMPCA, 4HX    /
      DATA CDTYPE(21), CDTYPE(22) / 4HOMIT, 4HAX   /
      DATA CDTYPE(23), CDTYPE(24) / 4HPOIN, 4HTAX  /
      DATA CDTYPE(25), CDTYPE(26) / 4HPRES, 4HAX   /
      DATA CDTYPE(27), CDTYPE(28) / 4HRING, 4HAX   /
      DATA CDTYPE(29), CDTYPE(30) / 4HSECT, 4HAX   /
      DATA CDTYPE(31), CDTYPE(32) / 4HSEQG, 4HP    /
      DATA CDTYPE(33), CDTYPE(34) / 4HSPCA, 4HDD   /
      DATA CDTYPE(35), CDTYPE(36) / 4HSPCA, 4HX    /
      DATA CDTYPE(37), CDTYPE(38) / 4HSUPA, 4HX    /
      DATA CDTYPE(39), CDTYPE(40) / 4HTEMP, 4HAX   /
      DATA CDTYPE(41), CDTYPE(42) / 4HTEMP, 4HD    /
      DATA CDTYPE(43), CDTYPE(44) / 4HCTRI, 4HAAX  /
      DATA CDTYPE(45), CDTYPE(46) / 4HCTRA, 4HPAX  /
      DATA CDTYPE(47), CDTYPE(48) / 4HRFOR, 4HCE   /
C
      DATA AXIC1  /515     ,5      ,0       /
      DATA CCONEX /8515    ,85     ,0       /
      DATA FORCEX /2115    ,21     ,0       /
      DATA FORCE  /4201    ,42     ,0       /
      DATA GRAV   /4401    ,44     ,0       /
      DATA LOAD   /4551    ,61     ,0       /
      DATA MOMAX  /3815    ,38     ,0       /
      DATA MOMENT /4801    ,48     ,0       /
      DATA MPCADD /4891    ,60     ,0       /
      DATA MPCAX  /4015    ,40     ,0       /
      DATA OMITAX /4315    ,43     ,0       /
      DATA POINTX /4915    ,49     ,0       /
      DATA PRESAX /5215    ,52     ,0       /
      DATA RINGAX /5615    ,56     ,0       /
      DATA SECTAX /6315    ,63     ,0       /
      DATA SEQGP  /5301    ,53     ,0       /
      DATA SPCAX  /6215    ,62     ,0       /
      DATA SUPAX  /6415    ,64     ,0       /
      DATA TEMPAX /6815    ,68     ,0       /
      DATA TEMPD  /5641    ,65     ,0       /
      DATA PLOAD  /5101    ,51     ,0       /
      DATA MPC    /4901    ,49     ,0       /
      DATA SPC    /5501    ,55     ,0       /
      DATA GRID   /4501    ,45     ,0       /
      DATA SUPORT /5601    ,56     ,0       /
      DATA TEMP   /5701    ,57     ,0       /
      DATA OMIT   /5001    ,50     ,0       /
      DATA SPCADD /5491    ,59     ,0       /
      DATA CTRIAA /7012    ,70     ,0       /
      DATA CTRAPA /7042    ,74     ,0       /
      DATA RFORCE /5509    ,55     ,0       /
      DATA ICONSO / 0 /
      DATA NEG111 /-1      ,-1     ,-1      /
      DATA T65535/ 65535, 65535, 65535 /
      DATA IHEADB /
     1                4HI N ,4HP U ,4HT   ,4HD A ,4HT A ,4H  E
     2               ,4HR R ,4HO R ,4HS   ,4HD E ,4HT E ,4HC T
     3               ,4HE D ,4H  B ,4HY   ,4HI F ,4HP 3 ,4H
     4               ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     5               ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     6               ,4H    ,4H    ,4H    ,4H (AX,4HIS-S,4HYMME
     7               ,4HTRIC,4H CON,4HICAL,4H SHE,4HLL D,4HATA
     8               ,4HPROC,4HESSO,4HR-GE,4HNERA,4HTOR),4H
     9               ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     T               ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     1               ,4H    ,4H    ,4H    ,4H    ,4H    ,4H ===
     2               ,4H====,4H====,4H====,4H====,4H====,4H====
     3               ,4H====,4H====,4H====,4H====,4H====,4H====
     4               ,4H====,4H    ,4H    ,4H    ,4H    ,4H
     5               ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     6               ,4H    ,4H    ,4H    ,4H    ,4H    ,4H      /
C
      END
