      BLOCK DATA DPDCBD
CDPDCBD
C BLOCK DATA PROGRAM FOR THE DYNAMICS POOL DISTRIBUTOR
C*****
C
      INTEGER       GPL   ,SIL   ,USET  ,USETD ,GPLD  ,SILD  ,DPOOL
     1             ,DLT   ,FRL   ,TFL   ,TRL   ,PSDL  ,EED   ,SCR1
     2             ,SCR2  ,SCR3  ,SCR4  ,BUF   ,BUF1  ,BUF2  ,BUF3
     3             ,BUF4  ,EQDYN ,SDT   ,EPOINT,SEQEP ,EIGC  ,EIGB
     5             ,LOADS ,DLOAD ,FREQ1 ,FREQ  ,TIC   ,TSTEP ,TF
     6             ,PSD   ,EIGR
C
      DIMENSION BUF(24)   ,EPOINT(2)    ,SEQEP(2)     ,MCB(7)
     1         ,NAM(2)    ,LOADS(32)    ,DLOAD(2)     ,FREQ1(2)
     2         ,FREQ(2)   ,EIGC(2)      ,EIGB(2)      ,NOLIN(21)
     3         ,TIC(2)    ,TSTEP(2)     ,TF(2)        ,PSD(2)
     4         ,MSG(3)    ,EIGR(2)
C
      COMMON/DPDCOM/DPOOL ,GPL   ,SIL   ,USET  ,GPLD  ,SILD  ,USETD
     1             ,DLT   ,FRL   ,NLFT  ,TFL   ,TRL   ,PSDL  ,EED
     2             ,SCR1  ,SCR2  ,SCR3  ,SCR4  ,BUF   ,BUF1  ,BUF2
     3             ,BUF3  ,BUF4  ,EPOINT,SEQEP ,L     ,KN    ,NEQDYN
     4             ,LOADS ,DLOAD ,FREQ1 ,FREQ  ,NOLIN ,NOGO
     5             ,MSG   ,TIC   ,TSTEP ,TF    ,PSD   ,EIGR  ,EIGB
     6             ,EIGC  ,MCB   ,NAM   ,EQDYN ,SDT   ,INEQ
C*****
C INPUT FILES
C*****
      DATA    DPOOL/101/  ,GPL/102/     ,SIL/103/     ,USET/104/
C*****
C OUTPUT FILES
C*****
      DATA    GPLD  /201/ ,SILD  /202/  ,USETD /203/  ,TFL   /204/
     1       ,DLT   /205/ ,PSDL  /206/  ,FRL   /207/  ,NLFT  /208/
     2       ,TRL   /209/ ,EED   /210/  ,EQDYN /211/  ,SDT   /212/
C*****
C SCRATCH FILES
C*****
      DATA    SCR1/301/   ,SCR2/302/    ,SCR3/303/    ,SCR4/304/
C*****
C DATA DEFINING INPUT CARDS
C*****
      DATA EPOINT  /   707,     7/
     1    ,SEQEP   /  5707,    57/
     2    ,LOADS   /    27,    17,     0,     0
     3             ,    37,    18,     0,     0
     4             ,    77,    19,     0,     0
     5             ,  5107,    51,     6,     0
     6             ,  5207,    52,     6,     0
     7             ,  7107,    71,     5,     0
     8             ,  7207,    72,    10,     0
     9             ,     0,     0,     0,     0/
     A    ,DLOAD   /    57,     5/
     B    ,FREQ1   /  1007,    10/
     C    ,FREQ    /  1307,    13/
      DATA NOLIN   /  3107,    31,     8
     E             ,  3207,    32,     8
     F             ,  3307,    33,     8
     G             ,  3407,    34,     8
     H             ,  3507,    35,    16
     I             ,  3607,    36,     5
     J             ,  3707,    37,     8/
      DATA TIC     /  6607,    66/
     I    ,TSTEP   /  8307,    83/
     J    ,TF      /  6207,    62/
     K    ,EIGR    /   307,     3/
     L    ,EIGB    /   107,     1/
     M    ,EIGC    /   207,     2/
C*****
C MISC DATA
C*****
      DATA MCB     /   7*0/
     1    ,NAM     /4HDPD ,4H    /
      END
