      SUBROUTINE SMCPH2 ( ZI, ZR, ZD )              
C     
C SMCPH2 PERFORMS THE ACTUAL DECOMPOSITION OF THE MATRIX THAT WAS 
C SETUP IN MEMORY AND/OR THE SPILL BY SMCPH1.
C SEE SMCPH1 FOR THE DEFINITION OF SOME OF THE VARIABLES IN /SMCOMX/
C
      REAL              ZR(10)
      DOUBLE PRECISION  ZD(10)  ,XND(10)              
      INTEGER           ZI(10)  ,ITEMP(4)
      INTEGER           PRC     ,WORDS   ,RLCMPX  ,NAME(2)
      CHARACTER*4       CNAME(2)
      CHARACTER         UFM*23  ,UWM*25  ,UIM*29  ,SFM*25
      INCLUDE           'SMCOMX.COM'                    
      COMMON  /LOGOUT/  LOUT
      COMMON  /XMSSG /  UFM     ,UWM     ,UIM     ,SFM
      COMMON  /NAMES /  RDNRW   ,RDREW   ,WRT     ,WRTREW  ,REW     
     1,                 NOREW   ,EOFNRW  ,RSP     ,RDP     ,CSP     
     2,                 CDP     ,SQR     ,RECT    ,DIAG    ,LOWTRI  
     3,                 UPRTRI  ,SYM
      COMMON  /ZZZZZZ/  XNS(10)
      COMMON  /TYPE  /  PRC(2)  , WORDS(4), RLCMPX(4)     
      EQUIVALENCE       ( XNS, XND )
      EQUIVALENCE       ( MBLK(6), MTERMS ), (MBLK(5), MSTR )
      EQUIVALENCE       ( MBLK(4), MROW   ), (MBLK(2), MTYPE)
      EQUIVALENCE       ( NAME   , CNAME  )
c
c   open core is allocated as follows for the decomposition
c
C       -------------------------------
C       zi(1)
C       Directory  (4,n) , n=number of columns of matrix
C                  (1,i) = index to active rows and terms within memory
C                  (2,i) = first column data needed for this pivot
C                  (3,i) = last pivot column to use this data
C                          (also, the last active row of this column)
C                  (4,i) = savpos position pointer for data spilled to a
C                          scratch file
C       -------------------------------
C       zi(nar)
C       Area for storage of row numbers used for previous column of
C       decomposition (length=MAXNAR+2)
C       -------------------------------    
C       zi(ispill)
C       Area to read data from spill file (length =MXRECL+4)
C       This area is not needed if no columns written to spill file
C       -------------------------------
C       zi(ILSROW)
C       Area for storage of last non-zero row term for a given column
C       (length=MAXNCOL)
C       -------------------------------
C       zi(ioloop)
C       Values for outer loop terms in all row computations in the
C       current pivotal column.
C
C         temp = temp + a(i,j) * a(k,j) / a(j,j)
C                                ===============
C            i = row of column
C            k = pivotal column being processed
C            j = 1, k-1
C          a(i,k) = a(i,k) - temp
C          (Note, length is 2*MAXNCOL)
C          MAXNCOL = maximum number of columns referenced by any
C                    pivotal column
C       -------------------------------     
C       zi(iiloop)
C       Values for inner loop terms in each row computation
C         temp = temp + a(i,j) * a(k,j) / a(j,j)
C                       ======
C            i = row of column
C            k = pivotal column being processed
C            j = 1, k-1
C          a(i,k) = a(i,k) - temp  
C          (Note, length is  MAXNCOL*MAXNAC)
C          MAXNAC = MAXIMUM NUMBER OF ACTIVE ROWS FOR ANY GIVEN COLUMN
C       -------------------------------         
C       zi(iwork)
C       Temporary storage for storing "temp" values for each row (see
C       "temp" in above equation for zi(iiloop) )
C       -------------------------------
C       zi(idbase)
C       Memory for rows and terms of columns as pointed to by directory
C       in the first part of open core.  This data is loaded from the  
C       bottom up to allow for better management of open core.
C       The format for the storage of this data is as follows:
C          (index from directory above points to the first word of the
C           data that follows)
C               1.  Column number
C               2.  Length of active row section (m*2), m=number of
C                   repeats of contents of words 5 and 6 below.
C               3.  Total number of words in this block of allocation
C               4.  Length of values section
C               5.  row number
C               6.  number of consecutive values beginning at this row
C                   (words 5 and 6 repeat m times)
C           5+2*m.  value for first row
C     5+2*m+iprec.  next row value (iprec=1,2,4)
C   5+2*m+iprec*l.  last row value for column (l=total values)
C       -------------------------------         
C       zi(ibuf2)
C       Buffer for spill file if all column values can not be kept in memory   
C       -------------------------------
C       zi(ibuf1)
C       Buffer for input matrix file to be decomposed
C       -------------------------------
C
c      CALL AUDIT ('SMCPH2  ',1 )
      NAR    = NCOL*4 + 1
      ILSROW = NAR + MAXNAR + 2
      MSPILL = 0
      ISPILL = 0
      IF ( NSPILL .EQ. 0 ) GO TO 5
      ISPILL = NAR + MAXNAR + 1   
      IF ( MOD( ISPILL,2 ) .EQ. 0 ) ISPILL = ISPILL + 1
      ILSROW = ISPILL + MXRECL + 4
5     CONTINUE
      IOLOOP = ILSROW + MAXNCOL + 2  
      IF ( MOD( IOLOOP,2 ) .EQ. 0 ) IOLOOP = IOLOOP + 1
      IILOOP = IOLOOP + 2*MAXNCOL*IVWRDS
      IWORK  = IILOOP + MAXNCOL*MAXNAC*IVWRDS
      ITOTAL = IWORK  + MAXNAC*IVWRDS
      INDDIR = LASCOL * 4 - 3
      IDBASE = ZI( INDDIR )
      MEMFRE = 0
      MEMLAS = 0
      MEMLCK = 0
      MEMCOL1= 1
      XNCOL  = NCOL
      XSPILL = NSPILL 
      XFACT  = XNCOL / ( XNCOL-NSPILL )
C
C  MORE = ESTIMATED NUMBER OF WORDS NEEDED FOR STORING ALL OF MATRIX
C  IADJ = WORDS OF COLUMN DATA THAT WILL NEED TO BE WRITTEN TO THE SPILL
C         FILE TO ALLOW FOR "ITOTAL" WORDS FOR THE PHASE II ARRAYS.
C
      MORE   = XFACT * ( LCORE - IDBASE )
      IADJ   = 0
      IF ( ITOTAL .GT. IDBASE ) IADJ = ITOTAL - IDBASE
      MAXMEM = ITOTAL + MORE + IADJ
      CALL SSWTCH ( 45, L45 )
      IF ( NSPILL .EQ. 0 .AND. L45 .EQ. 0 ) GO TO 10
      WRITE ( LOUT, 8002 ) MAXMEM, LCORE
8002  FORMAT(
     &    7X,' ESTIMATED OPEN CORE NEEDED TO ELIMINATE USE OF SPILL=',I8
     &,/, 7X,' OPEN CORE AVAILABLE FOR THIS DECOMPOSITION          =',I8 
     & )
C
C TEST TO BE SURE THAT AT LEAST HALF OF THE MEMORY IS AVAILABLE.
C IF NOT, USE OLD METHOD INSTEAD OF THIS ONE.
C
      XMAXMEM = MAXMEM
      XCORE   = LCORE
      PERCNT  = XCORE / XMAXMEM
      IF ( PERCNT .LT. .5 ) GO TO 7008
C
C CHECK TO SEE IF ENOUGH OPEN CORE FOR INNER AND OUTER LOOP VALUES
C
10    IF ( ITOTAL .LT. IDBASE ) GO TO 500
C
C NEED MORE OPEN CORE FOR LOOP AREAS.  WRITE COLUMN DATA TO SPILL FILE.
C IF COLUMNS WERE WRITTEN TO SPILL FILE FROM SMCPH1, THEN FILE WILL
C STILL BE OPEN.  IF NOT, MUST ALLOW FOR SPILL AREA IN OPEN CORE AND
C RE-ADJUST THE OPEN CORE POINTERS.
C      
      NEXTRA = 0
      IF ( OPNSCR ) GO TO 20
      OPNSCR = .TRUE.
      CALL OPEN ( *7003, ISCR1, ZI( IBUF2 ), WRTREW )
      ISPILL = NAR + MAXNAR + 1   
      IF ( MOD( ISPILL,2 ) .EQ. 0 ) ISPILL = ISPILL + 1
      ILSROW = ISPILL + MXRECL + 4
      IOLOOP = ILSROW + MAXNCOL + 2
      IF ( MOD( IOLOOP,2 ) .EQ. 0 ) IOLOOP = IOLOOP + 1
      IILOOP = IOLOOP + 2*MAXNCOL*IVWRDS
      IWORK  = IILOOP + MAXNCOL*MAXNAC*IVWRDS
      ITOTAL = IWORK  + MAXNAC*IVWRDS
20    CONTINUE
C
C WRITE THE LAST COLUMN OF DATA CURRENTLY IN MEMORY TO THE SPILL FILE
C      
      INDEX  = ZI( INDDIR ) 
      IRVAL  = INDEX + 4                                            
      NRVALS = ZI( INDEX+1 )
      NTERMS = ZI( INDEX+3 )                         
      IVVAL  = IRVAL + NRVALS         
      ITEMP( 1 ) = ZI( INDEX )
      ITEMP( 2 ) = NRVALS
      ITEMP( 3 ) = 0
      ITEMP( 4 ) = NTERMS
C      PRINT *,' SMCPH2 CALLING WRITE FOR ITEMP,NRVALS,NTERMS,IVWRDS'
C      PRINT *,                           ITEMP,NRVALS,NTERMS,IVWRDS
      CALL WRITE ( ISCR1, ITEMP, 4, 0 )
      CALL SAVPOS( ISCR1, KPOS )   
      CALL WRITE ( ISCR1, ZR( IRVAL ), NRVALS, 0 )
      CALL WRITE ( ISCR1, ZR( IVVAL ), NTERMS*IVWRDS, 1 )  
      ZI( INDDIR   ) = 0       
      ZI( INDDIR+3 ) = KPOS
50    INDDIR = INDDIR - 4
      IF ( INDDIR .LE. 0 ) GO TO 7008
      IF ( ZI ( INDDIR ) .EQ. 0 ) GO TO 50
C
C RESET IDBASE TO INDICATE THE LAST COLUMN OF DATA IN MEMORY
C      
      IDBASE = ZI( INDDIR )
      MSPILL = MSPILL + 1
      GO TO 10
C
C OPEN THE OUTPUT FILE
C
500   CONTINUE
      LEFT   = IDBASE - ITOTAL
C
C DETERMINE HOW MANY MORE COLUMNS OF THE INNER LOOP AREA AND
C EXTRA TERMS OF THE OUTER LOOP AREA ARE AVAILABLE
C   NEXTRA = NUMBER OF EXTRA COLUMNS AVAILABLE IN THE INNER LOOP AREA
C          = NUMBER OF EXTRA COLUMNS AVAILABLE IN THE OUTER LOOP AREA
C            (INNER LOOP AREA SIZE = MAXNAC * ( MAXNCOL + NEXTRA ) )
C            (OUTER LOOP AREA SIZE = 2      * ( MAXNCOL + NEXTRA ) )
C          = NUMBER OF EXTRA ROWS IN THE "ILSROW" ARRAY (MAXNCOL+NEXTRA)
C  (Note: for each column added, we need the following:
C           for array ILSROW:                1
C           to insure double word boundary:  1
C           for outer loop:                  2*IVWRDS
C           for inner loop:                  MAXNAC*IVWRDS
C           ( must allow for temp array size:    MAXNAC*IVWRDS
      NEED   = 2 + 2*IVWRDS + MAXNAC*IVWRDS
      NEXTRA = ( LEFT - 2 - (MAXNAC*IVWRDS) ) / NEED
C      PRINT *,' LEFT,NEED,NEXTRA=',LEFT,NEED,NEXTRA
      IF ( NEXTRA .EQ. 0 ) GO TO 505
      IOLOOP = ILSROW +            ( MAXNCOL+NEXTRA ) + 2   
      IF ( MOD( IOLOOP,2 ) .EQ. 0 ) IOLOOP = IOLOOP + 1
      IILOOP = IOLOOP + ( 2      * ( MAXNCOL+NEXTRA ) ) * IVWRDS
      IWORK  = IILOOP + ( MAXNAC * ( MAXNCOL+NEXTRA ) ) * IVWRDS
      ITOTAL = IWORK  +            ( MAXNAC           ) * IVWRDS
505   IF ( KPREC .EQ. 2 ) IOLOOP = IOLOOP / 2 + 1
      IF ( KPREC .EQ. 2 ) IILOOP = IILOOP / 2 + 1
      IF ( KPREC .EQ. 2 ) IWORK  = IWORK  / 2 + 1
      NVTERM = 1
      IF ( KTYPE .GE. 3 ) NVTERM = 2
      IF ( MSPILL .NE. 0 ) WRITE ( LOUT, 8001 ) MSPILL
8001  FORMAT(8X,'ADDITIONAL COLUMNS WRITTEN TO SPILL '
     &,'FOR PHASE II PROCESSING =',I6)
      IF ( .NOT. OPNSCR ) GO TO 510
      CALL CLOSE ( ISCR1, 1 )
      CALL OPEN  ( *7002, ISCR1, ZI( IBUF2 ), RDREW )
510   CONTINUE
      CALL OPEN ( *7001, LLL, ZI( IBUF1 ), WRTREW )
      CALL FNAME ( LLL, NAME )
      CALL WRITE ( LLL, NAME, 2, 1 )
C
C DO THE DECOMPOSITION NOW
C      
c      CALL AUDIT ( 'SMC2RD  ', 1 )
C      PRINT *,' IILOOP,IOLOOP,NAR,ILSROW,NEXTRA,IDBASE,IWORK,ISPILL'
C      PRINT *,  IILOOP,IOLOOP,NAR,ILSROW,NEXTRA,IDBASE,IWORK,ISPILL
      GO TO ( 1000, 2000, 3000, 4000 ), KTYPE
1000  CONTINUE
      CALL SMC2RS ( ZI, ZR, ZR( IILOOP ), ZR( IOLOOP ), ZI( NAR )
     &,    ZI( ILSROW ), ZR( IWORK ), MAXNAC, MAXNCOL+NEXTRA, MAXNAR )
      GO TO 5000
2000  CONTINUE
      CALL SMC2RD ( ZI, ZD, ZD( IILOOP ), ZD( IOLOOP ), ZI( NAR )
     &,    ZI( ILSROW ), ZD( IWORK ), MAXNAC, MAXNCOL+NEXTRA, MAXNAR )
      GO TO 5000
3000  CONTINUE                 
C      PRINT *,' CALLING SMC2CS'
      CALL SMC2CS ( ZI, ZR, ZD( IILOOP ), ZD( IOLOOP ), ZI( NAR )
     &,    ZI( ILSROW ), ZD( IWORK ), MAXNAC, MAXNCOL+NEXTRA, MAXNAR )
      GO TO 5000
4000  CONTINUE
C      PRINT *,' CALLING SMC2CD'
      CALL SMC2CD ( ZI, ZD, ZD( IILOOP ), ZD( IOLOOP ), ZI( NAR )
     &,    ZI( ILSROW ), ZD( IWORK ), MAXNAC, MAXNCOL+NEXTRA, MAXNAR )
      GO TO 5000
5000  CONTINUE
c      CALL AUDIT ( 'SMC2RD  ', 2 )
      CALL CLOSE ( LLL  , 1 )
      CALL CLOSE ( ISCR1, 1 )
      GO TO 7777
7001  CONTINUE
      CALL FNAME ( LLL, NAME )
      IERROR = 2
      WRITE ( NOUT, 9001 ) UFM, LLL(1), CNAME
9001  FORMAT(1X,A23,/,' SMCPH2 UNABLE TO OPEN FILE ',I4,' ;FILE NAME ='
     &,  2A4 )
      GO TO 7100
7002  CALL FNAME ( ISCR1, NAME )
      WRITE ( NOUT, 9001 ) UFM, ISCR1, CNAME
      IERROR = 3
      GO TO 7100
7003  CONTINUE
      IERROR = 2
      CALL FNAME ( ISCR1, NAME )
      WRITE ( NOUT, 9001 ) UFM, ISCR1, CNAME
      GO TO 7100
7008  CONTINUE
      CALL FNAME ( LLL, NAME )
      MINUM = (.5 * XMAXMEM ) - LCORE
      WRITE ( LOUT, 9008 ) NCOL, MINUM
9008  FORMAT(8X,'INSUFFICIENT OPEN CORE FOR DECOMPOSITION WITH NEW'
     &,' METHOD'
     &,/,    8X,'TOTAL NUMBER OF COLUMNS IN MATRIX =',I8
     &,/,    8X,'SUGGESTED ADDITIONAL OPEN CORE IS =',I8)
      CALL CLOSE ( ISCR1, 1 )
C      CALL MESAGE ( -8, 0, 0 )
      IERROR = 1
      GO TO 7777
7100  CALL MESAGE ( -61, 0, 0 )  
7777  CONTINUE
c      CALL AUDIT ( 'SMCPH2  ',2)
c      CALL AUDIT ( 'END     ',1)
c      IF ( NCOL .NE. 0 ) STOP
      RETURN
      END
