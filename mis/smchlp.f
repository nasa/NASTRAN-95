      SUBROUTINE SMCHLP
C      
C SMCHLP WRITES THE CONTENTS OF THE KEY PARAMETERS IN THE SYMMETRIC
C DECOMPOSITION COMMON BLOCKS
C
      INCLUDE 'SMCOMX.COM'
      WRITE ( NOUT, 9001 ) 
     &    NCOL   , IERROR  , IVWRDS , MAXNAC 
     &,   NSPILL , MAXINLOP, IDBASE , IDBMAX 
     &,   IBUF1  , IBUF2   , OPNSCR , IOLOOP
     &,   LASCOL , KROW    , KROWS  , KROWN
     &,   KRIDX  , KRIDXN  , JRIDXN , JROW    
     &,   JROWS  , JROWN   , JRIDX  , JVIDX  
     &,   IROW1  , IROWN   , KFRCOL , KLSCOL
     &,   KLSROW , IOL     , IIL    , KTYPE 
     &,   ISKIP  , INDEXV  , KCOL   , MAXNCOL
     &,   MEMFRE , MEMCOL1 , MEMLCK , MEMLAS
     &,   MEMCOLN, ISPILL  , NBANDW , NVTERM
9001  FORMAT(//
     &,  ' NCOL   =',I9,' IERROR  =',I9,' IVWRDS =',I9,' MAXNAC =',I9
     &,/,' NSPILL =',I9,' MAXINLOP=',I9,' IDBASE =',I9,' IDBMAX =',I9
     &,/,' IBUF1  =',I9,' IBUF2   =',I9,' OPNSCR =',L9,' IOLOOP =',I9
     &,/,' LASCOL =',I9,' KROW    =',I9,' KROWS  =',I9,' KROWN  =',I9
     &,/,' KRIDX  =',I9,' KRIDXN  =',I9,' JRIDXN =',I9,' JROW   =',I9
     &,/,' JROWS  =',I9,' JROWN   =',I9,' JRIDX  =',I9,' JVIDX  =',I9
     &,/,' IROW1  =',I9,' IROWN   =',I9,' KFRCOL =',I9,' KLSCOL =',I9
     &,/,' KLSROW =',I9,' IOL     =',I9,' IIL    =',I9,' KTYPE  =',I9
     &,/,' ISKIP  =',I9,' INDEXV  =',I9,' KCOL   =',I9,' MAXNCOL=',I9
     &,/,' MEMFRE =',I9,' MEMCOL1 =',I9,' MEMLCK =',I9,' MEMLAS =',I9
     &,/,' MEMCOLN=',I9,' ISPILL  =',I9,' NBANDW =',I9,' NVTERM =',I9  
     & )
      WRITE ( NOUT, 9002 ) ISYSBF, ISPREC
9002  FORMAT(
     & /,' ISYSBF =',I9,' ISPREC   =',I9 )
      WRITE ( NOUT, 9003 ) MBLK, MOBLK
9003  FORMAT(/,' MBLK (INPUT MATRIX STRING BLOCK)=',/,3(5I10,/)
     &      ,/,' MOBLK (OUTPUT MATRIX STRING BLOCK)=',/,3(5I10,/))
      WRITE ( NOUT, 9004 ) LCORE, POWER, MINDD, CHLSKY, ISCR1
9004  FORMAT(
     & /,' LCORE  =',I9,' POWER   =',I9,' MINDD   =',E16.8
     &,/,' CHLSKY =',I9,' ISCR1   =',I9 )
      WRITE ( NOUT, 9005 ) MCB, LLL
9005  FORMAT('  INPUT MATRIX MCB=',/,7I8,
     & /,    ' OUTPUT MATRIX MCB=',/,7I8 )
      RETURN
      END
