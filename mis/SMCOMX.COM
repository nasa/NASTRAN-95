      DOUBLE PRECISION  DAJJR   ,DAJJI   ,DDR     ,DDC     ,MINDD   
      INTEGER           POWER   ,STURM   ,CHLSKY
      LOGICAL           OPNSCR
      COMMON  /SMCOMX/  NCOL    ,IERROR  ,IVWRDS  ,MAXNAC  ,NSPILL
     &,                 MAXINLOP,IDBASE  ,IDBMAX  ,IBUF1   ,IBUF2
     &,                 OPNSCR  ,IOLOOP  ,IILOOP  ,LASCOL  ,KROW
     &,                 KROWS   ,KROWN   ,KRIDX   ,KRIDXN  ,JRIDXN
     &,                 JROW    ,JROWS   ,JROWN   ,JRIDX   ,JVIDX
     &,                 IROW1   ,IROWN   ,KFRCOL  ,KLSCOL  ,KLSROW
     &,                 IOL     ,IIL     ,KTYPE   ,ISKIP   ,INDEXV
     &,                 INDEXVD ,JCOL    ,KAROWS  ,MXRECL  ,NVTERM
     &,                 KCOL    ,MAXNCOL ,MEMFRE  ,MEMCOL1 ,MEMLCK
     &,                 MEMLAS  ,MEMCOLN ,ISPILL  ,KPREC   ,NBANDW
     &,                 MAXNAR  ,MBLK(15),MOBLK(15)
      COMMON  /SMCOMY/  DAJJR   ,DAJJI   ,AJJR    ,AJJI
      COMMON  /SYSTEM/  ISYSBF  ,NOUT    ,DUM1(37),NBPW    ,DUM2(14)
     &,                 ISPREC
      COMMON  /SFACT /  MCB(7)  ,LLL(7)  ,DBC(7)  ,ISCR1   ,ISCR2
     &,                 LCORE   ,DDR     ,DDC     ,POWER   ,SCR3    
     &,                 MINDD   ,CHLSKY
      COMMON  /STURMX/  STURM   ,SHFTPT  ,KEEP    ,PTSHFT  ,NR
