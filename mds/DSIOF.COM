       PARAMETER  ( MAXPRI = 80, MAXFCB = 89 )
       COMMON / DSIO /  IEOR          
     *,         IOERR , IPRVOP, IRETRN, IRWORD, IDATAD          
     *,         IDSN  , LCW   , LWORDS, MASKH1, MASKH2          
     *,         MASKE1, MASKE2, MASKE3, MASKE4, MAXDSN, NWORDS          
     *,         NBUFF , IOBLK , NBFZ  , NLR             
     *,         MASKQ1, MASKQ2, MASKQ3, MASKQ4, IDSX  , IDSP            
     *,         IDSC  , IDSRH , IDSRT , IDSSB , IDSSE , IDSCH           
     *,         IDSCT , IDSSH , IDSST , IDSSD , IDSEB , IDSEF           
     *,         IBLOCK, LASNAM, MCBMAS, MULQ1 , MULQ2 , MULQ3
     *,         LHALF
     *,         LENDSP, LENWPB, NWRDEL(4)     
        COMMON /DSNAME/ MDSNAM(MAXFCB)
        COMMON /DSDEVC/ NUMDEV, DEV(10)
        CHARACTER*2     DEV
        CHARACTER*80    MDSNAM     
        COMMON /DBM   / IDBBAS, IDBFRE, IDBDIR, INDBAS, INDCLR, INDCBP
     *,                 NBLOCK, LENALC, IOCODE, IFILEX, NAME,   MAXALC
     *,                 MAXBLK, MAXDSK, IDBLEN, IDBADR, IBASBF, INDDIR 
     *,                 NUMOPN, NUMCLS, NUMWRI, NUMREA, LENOPC   
        INTEGER  FCB
        COMMON / FCB  / FCB(17,MAXFCB)
