      SUBROUTINE SMCPH1 ( ZI, ZR, ZD )              
      REAL              ZR(4)   ,MINDS
      LOGICAL           FRSTVAL 
      INTEGER           ZI(4)   ,ITEMP(4)
      INTEGER           PRC     ,WORDS   ,RLCMPX  ,NAME(2), REW
      DOUBLE PRECISION  ZD(4)   ,XND(10)
      CHARACTER         UFM*23  ,UWM*25  ,UIM*29  ,SFM*25
      CHARACTER*4       CNAME(2)
      CHARACTER*14      CTYPE(4)
C 
C  KTYPE   = TYPE (1-RS,2-RD,3-CS,4-CD) OF LOWER TRIANGULAR MATRIX
C  KPREC   = PRECISION (1-SINGL, 2-DOUBL) OF LOWER TRIANGULAR MATRIX
C  MAXROW  = HIGHEST ROW NUMBER REFERENCED THUS FAR IN PROCESSING
C            A GIVEN COLUMN - NEEDED TO DETERMINE CREATED TERMS DURING
C            DECOMPOSITION
C  MAXINLOP= MAXIMUM TERMS FOR ANY GIVEN INNER LOOP
C  MAXNCOL = MAXIMUM NUMBER OF COLUMNS REFERENCED BY ANY GIVEN COLUMN
C  LASCOL  = LAST COLUMN NUMBER OF MATRIX TO BE DECOMPOSED
C  NEXCOL  = FIRST NON-ZERO TERM IN CURRENT PIVOT COLUMN BELOW DIAGONAL
C            USED TO DETERMINE THE NEXT PIVOT COLUMN WHERE THE ROW
C            WILL BE NEEDED.
C  ICURCOL = CURRENT COLUMN BEING PROCESSED
C  MXRECL  = MAXIMUM SIZE IN WORDS OF ANY ONE RECORD WRITTEN TO THE 
C            SPILL FILE
C  NSPILL  = NUMBER OF COLUMNS WRITTEN TO THE SPILL FILE
C
      INCLUDE  'SMCOMX.COM'
      COMMON  /XMSSG /  UFM     ,UWM     ,UIM     ,SFM
      COMMON  /NTIME /  NITEMS  ,TMIO    ,TMBPAK  ,TMIPAK  ,TMPAK   
     1,                 TMUPAK  ,TMGSTR  ,TMPSTR  ,TMT(4)  ,TML(4)
      COMMON  /NAMES /  RDNRW   ,RDREW   ,WRT     ,WRTREW  ,REW     
     1,                 NOREW   ,EOFNRW  ,RSP     ,RDP     ,CSP     
     2,                 CDP     ,SQR     ,RECT    ,DIAG    ,LOWTRI  
     3,                 UPRTRI  ,SYM
      COMMON  /ZZZZZZ/  XNS(10)
      COMMON  /TYPE  /  PRC(2)  , WORDS(4), RLCMPX(4)     
      COMMON  /LOGOUT/  LOUT
      EQUIVALENCE       ( DDR     , DSR    ), (DDC    , DSC  )
      EQUIVALENCE       ( MINDD   , MINDS  ), (XNS    , XND  )
      EQUIVALENCE       ( MBLK(6) , MTERMS ), (MBLK(5), MSTR )
      EQUIVALENCE       ( MBLK(4) , MROW   ), (MBLK(2), MTYPE)
      EQUIVALENCE       ( CNAME   , NAME   )
      DATA              CTYPE / 'REAL SINGLE   ', 'REAL DOUBLE   '
     &,                         'COMPLEX SINGLE', 'COMPLEX DOUBLE' /
C
C   open core is allocated as follows for phase1 of the decomposition
C
C       -------------------------------
C       zi(1) -  Beginning of directory for in-memory column data
C       Directory (4,n) , n=number of columns of matrix
C                  (1,i) = index to active rows and terms within memory
C                  (2,i) = first column data needed for this pivot
C                  (3,i) = last pivot column to use this data
C                  (4,i) = savpos position pointer for data spilled to a
C                          scratch file
C       -------------------------------
C       zi(iacrow) - Beginning of active row vector.
C       Vector for determining active rows for each column, n words
C       Each row value will define the next column where the row value
C       is next needed for calculation of the lll matrix.
C       -------------------------------     
C       zi(IRVAL) - Stagging area for storing data 
C       Defines the values in the next section of open core, 2*n
C                  (1,i) = row number
C                  (2,i) = number of consecutive terms beginning at row
C       This section and the next section are staging areas for storing
C       of rows and row values of columns to be pointed to by the directory
C       in the first part of open core.
C       -------------------------------         
C       zi(IVVAL)
C       Row values of column as defined by previous section, n*iprec words
C       -------------------------------         
C       zi(idbase)
C       Memory for rows and terms of columns as pointed to by directory
C       in the first part of open core.  This data is loaded from the  
C       bottom up to allow for better management of open core in
C       subroutine smcph2 which is called after this subroutine.
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
C           5+2*m.  row value for first row
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
c      CALL AUDIT ( 'BEGIN   ', 1 )
c      CALL AUDIT ( 'SMCPH1  ', 1 )
      CALL FNAME ( MCB, NAME ) 
      NCOL   = MCB( 2 )
      MEMCOLN= 0
      MXRECL = 0
      MAXNAC = 0
      MAXNAR = 0
      IPREC  = PRC  ( MCB( 5 ) )
      KTYPE  = MCB( 5 )
      IF ( ISPREC .EQ. 2 .AND. KTYPE .EQ. 1 ) KTYPE = 2
      IF ( ISPREC .EQ. 2 .AND. KTYPE .EQ. 3 ) KTYPE = 4
      IF ( ISPREC .EQ. 1 .AND. KTYPE .EQ. 2 ) KTYPE = 1
      IF ( ISPREC .EQ. 1 .AND. KTYPE .EQ. 4 ) KTYPE = 3
      IF ( KTYPE  .EQ. 1 .OR.  KTYPE .EQ. 3 ) KPREC = 1
      IF ( KTYPE  .EQ. 2 .OR.  KTYPE .EQ. 4 ) KPREC = 2
      IVWRDS = WORDS( KTYPE )
      IACROW = 4*NCOL + 1
      IRVAL  = IACROW + NCOL
      IVVAL  = IRVAL  + 2*NCOL
C
C ENSURE THAT IVVAL IS ON A DOUBLE WORD BOUNDARY
C      
      IF ( MOD( IVVAL,2 )  .EQ. 0 ) IVVAL = IVVAL + 1
      IDBASE = IVVAL  + IVWRDS*NCOL
C
C ENSURE THAT IDBASE IS ON A DOUBLE WORD BOUNDARY
C      
      IF ( MOD( IDBASE,2 ) .EQ. 0 ) IDBASE = IDBASE + 1
      IF ( LCORE .LT. (IDBASE + 2*ISYSBF) ) GO TO 7001
      IBUF1  = LCORE - ISYSBF
      IBUF2  = IBUF1 - ISYSBF
      IDBMAX = IBUF2 - 1
C
C ENSURE THAT IDBMAX IS ON A DOUBLE WORD BOUNDARY
C      
      IF ( MOD( IDBMAX,2 ) .EQ. 0 ) IDBMAX = IDBMAX - 1
      IDBIND = IDBMAX
      CALL OPEN   ( *7002, MCB, ZI(IBUF1), RDREW )
      CALL SKPREC ( MCB, 1 )
      MBLK(1) = MCB( 1 )
      LLL(2)  = MCB( 2 )
      LLL(3)  = MCB( 2 )
      LLL(4)  = 4
      LLL(5)  = KTYPE
      LLL(6)  = 0
      LLL(7)  = LSHIFT( 1, NBPW-2 - (NBPW-32) )
      ICURCOL = 1
      OPNSCR  = .FALSE.
      NSPILL  = 0
      MAXROW  = 0
      MAXINLOP= 0
      MAXNCOL = 0
      LASCOL  = 0
      POWER   = 0
      IF ( KPREC .NE. 2 ) GO TO 5
      DDR     = 1.0D0
      DDC     = 0.0D0
      MINDD   = 1.0D+25
      GO TO 8
5     DSR     = 1.0
      DCR     = 0.0
      MINDS   = 1.0E+25
8     CONTINUE
      MOBLK( 1 ) = LLL( 1 )
      MOBLK( 2 ) = KTYPE
      MOBLK( 3 ) = 1
C
C ZERO OUT THE ACTIVE COLUMN VECTOR
C      
      DO 10 I = 1, NCOL
      ZI( IACROW + I - 1 ) = 0
10    CONTINUE
      LEN = NCOL*4
C
C ZERO OUT THE DIRECTORY
C      
      DO 20 I = 1, LEN
      ZI( I )   = 0
20    CONTINUE
50    CONTINUE
      NTERMS  = 0
      FRSTVAL = .TRUE.
      INDEXR  = IRVAL
      INDEXV  = IVVAL
      INDEXVD = ( INDEXV / 2 ) + 1
      MBLK(8) = -1            
      NEXCOL  = 0
      INDDIR = (ICURCOL-1)*4 + 1    
100   CALL GETSTR ( *1000, MBLK )
      IF ( ICURCOL .LE. ( MROW+MTERMS-1) ) GO TO 120
C
C ALL ROW TERMS ARE BEFORE CURRENT PIVOT COLUMN; SKIP THESE TERMS
C AND GET NEXT STRING.
C CHECK TO SEE IF THIS IS THE FIRST TERM OF THE PIVOT COLUMN
C      
      IF ( ZI( INDDIR + 1 ) .EQ. 0 ) ZI( INDDIR + 1 ) = MROW 
      CALL ENDGET ( MBLK )
      GO TO 100
C
C SAVE SOME OR ALL OF THE TERMS IN THIS STRING
C IF THIS IS NOT THE FIRST STRING TO PROCESS, THEN SAVE ALL VALUES
C
120   ISKIP = 0
      IF ( .NOT. FRSTVAL ) GO TO 140
C
C CHECK IF THIS IS THE FIRST TERM OF THE PIVOT COLUMN
C      
      IF ( ZI( INDDIR + 1 ) .EQ. 0 ) ZI( INDDIR + 1 ) = MROW 
C
C OTHERWISE, CHECK IF ALL TERMS OR ONLY SOME ARE TO BE SAVED
C      
      FRSTVAL = .FALSE.
      IF ( ICURCOL .EQ. MROW ) GO TO 130
C
C CHECK FOR ZERO ON THE DIAGONAL
C      
      IF ( ICURCOL .LT. MROW ) GO TO 7004
C
C SKIP ALL TERMS BEFORE THE CURRENT PIVOT COLUMN 
C      
      ISKIP = ICURCOL - MROW 
      ZI( INDEXR   ) = ICURCOL
      NTERMS         = MTERMS - ISKIP
      ZI( INDEXR+1 ) = NTERMS 
      IF ( ( MTERMS-ISKIP ) .GT. 1 ) NEXCOL = ICURCOL + 1
      GO TO 200
130   CONTINUE
      IF ( MTERMS .GT. 1 ) NEXCOL = MROW + 1
      ZI(INDEXR   ) = MROW
      ZI(INDEXR+1 ) = MTERMS
      NTERMS        = MTERMS
      GO TO 200
140   CONTINUE
C
C CHECK TO SEE IF CURRENT STRING IS AN EXTENSION OF PREVIOUS STRING
C      
      IF ( (ZI( INDEXR )+ZI( INDEXR+1 ) ) .EQ. MROW ) GO TO 170
C
C NO, MUST CREATE NEW POINTER FOR VALUES
C BUT FIRST, CHECK FOR PROVIDING FOR COMPUTED TERMS OF 
C PREVIOUS PIVOT COLUMNS
C      
      IROW1 = ZI( INDEXR ) + ZI( INDEXR+1 )
      IROWN = MROW - 1
      IRFLAG = 1
      GO TO 6000
C
C NOW CHECK IF THE ADDED TERMS ARE PART OF SAME STRING AS THAT JUST
C GOTTEN FROM GETSTR CALL
C
150   IF ( ( ZI(INDEXR) + ZI(INDEXR+1) ) .EQ. MROW ) GO TO 170 
C
C NEW STRING TO BE DEFINED FOR THE CURRENT TERMS FROM GETSTR
C
160   INDEXR = INDEXR + 2
      ZI(INDEXR   ) = MROW
      ZI(INDEXR+1 ) = MTERMS 
      NTERMS        = NTERMS + MTERMS
      IF ( NEXCOL .EQ. 0 ) NEXCOL = MROW
      GO TO 200
170   CONTINUE
C
C TERMS ARE AN EXTENSION OF EXISTING DATA, CHANGE THE NUMBER OF TERMS 
C      
      ZI( INDEXR+1 ) = ZI( INDEXR+1 ) + MTERMS
      NTERMS         = NTERMS + MTERMS
      IF ( NEXCOL .EQ. 0 ) NEXCOL = MROW
200   CALL SMCRTR ( ZR, ZD )      
C
C SET ACTIVE COLUMN ROW NUMBERS FOR POSSIBLE EXPANDED TERMS
C      
      IROW1 = MROW + ISKIP
      IROWN = IROW1 + MTERMS - 1 - ISKIP
      DO 400 K = IROW1, IROWN
      ZI( IACROW + K - 1 ) = NEXCOL
400   CONTINUE
C
C GO AND GET ADDITIONAL STRINGS IF ANY
C      
      CALL ENDGET ( MBLK )
      GO TO 100
1000  CONTINUE
C
C END OF READING CURRENT COLUMN, CHECK IF DIAGONAL TERM FOUND
C      
C      PRINT *,' SMCPH1,ICURCOL,NEXCOL,MAXROW=',ICURCOL,NEXCOL,MAXROW
      IF ( FRSTVAL ) GO TO 7004
C
C SEE IF ANY COMPUTED TERMS FROM PREVIOUS PIVOT COLUMNS ARE TO BE 
C ADDED ONTO THE END OF THE CURRENT ACTIVE ROWS FOR THIS COLUMN
C      
      LROW  = ZI( INDEXR ) + ZI( INDEXR+1 ) - 1
      IF ( LROW .GT. MAXROW ) MAXROW = LROW
      IROW1 = LROW + 1
      IROWN = MAXROW
      IRFLAG = 2
C      PRINT *,' B1050,ICURCOL,IROWN,IROW1=',ICURCOL,IROWN,IROW1
      IF ( IROWN .GE. IROW1 ) GO TO 6000
C
C SET UP DIRECTORY AND SAVE DATA EITHER
C IN MEMORY OR ON SPILL FILE
C
 1050 CONTINUE
C
C RECOMPUTE LROW IN CASE NEW TERMS WERE ADDED FROM PREVIOUS PIVOT COLUMNS
C      
      LROW   = ZI( INDEXR ) + ZI( INDEXR+1 ) - 1     
C
C INDEXR POINTS TO CURRENT DIRECTORY ENTRY BUT INDEXV POINTS TO NEXT
C AVAILABLE POSITION FOR STORING TERMS
C      
      NRVALS = INDEXR - IRVAL + 2
      NVVALS = INDEXV - IVVAL 
      NWORDS = NRVALS + NVVALS + 4
C
C SAVE DATA IN MEMORY AND SET DIRECTORY ACCORDINGLY
C      
      ITEST = IDBIND - NWORDS + 1
C
C MAKE SURE ITEST IS ON DOUBLE WORD BOUNDARY
C      
      IF ( MOD( ITEST,2 ) .EQ. 0 ) ITEST = ITEST - 1
C
C CHECK TO SEE IF THERE IS SUFFICIENT MEMORY
C      
      MAXNAR = MAX0( NRVALS, MAXNAR )
      IF ( ITEST .LT. IDBASE ) GO TO 1800
      IDBIND = ITEST
      ZI( INDDIR     ) = IDBIND
      ZI( INDDIR + 3 ) = 0
      ZI( IDBIND     ) = ICURCOL
      ZI( IDBIND + 1 ) = NRVALS
      ZI( IDBIND + 2 ) = NWORDS
      ZI( IDBIND + 3 ) = NTERMS
      IDBIND = IDBIND + 3
      DO 1100 K = 1, NRVALS
      ZI( IDBIND + K ) = ZI( IRVAL + K - 1 )
1100  CONTINUE
      IDBIND = IDBIND + NRVALS
      IF ( KPREC .EQ. 2 ) GO TO 1300
      DO 1200 K = 1, NVVALS
      ZI( IDBIND + K ) = ZI( IVVAL + K - 1 )
1200  CONTINUE
      GO TO 1400
1300  INDXV = IDBIND / 2 
      NV    = NVVALS / 2                   
      IVD   = IVVAL  / 2 
      DO 1350 K = 1, NV
      ZD( INDXV+K ) = ZD( IVD+K )
1350  CONTINUE
1400  CONTINUE
      IDBIND = IDBIND + NVVALS - NWORDS
      LASCOL = ICURCOL
      MEMCOLN= ICURCOL
      ITEST  = NRVALS + NVVALS + 4
      IF ( ITEST .GT. MXRECL ) MXRECL = ITEST
      GO TO 2000
1800  CONTINUE
      IF ( OPNSCR ) GO TO 1810
      OPNSCR = .TRUE.
      CALL OPEN ( *7003, ISCR1, ZI(IBUF2), WRTREW )
C
C NO MORE MEMORY, SAVE COLUMN DATA TO SPILL FILE, KEEP RECORD POSITION
C
1810  ITEMP( 1 ) = ICURCOL
      ITEMP( 2 ) = NRVALS
      ITEMP( 3 ) = 0
      ITEMP( 4 ) = NTERMS
      CALL WRITE ( ISCR1, ITEMP, 4, 0 )
      CALL SAVPOS( ISCR1, KPOS )            
      CALL WRITE ( ISCR1, ZI( IRVAL ), INDEXR-IRVAL+2, 0 )
      CALL WRITE ( ISCR1, ZI( IVVAL ), INDEXV-IVVAL+2, 1 )
      ZI( INDDIR   ) = 0
      ZI( INDDIR+3 ) = KPOS
      NSPILL = NSPILL + 1
      ITEST  = NRVALS + NVVALS + 4
      IF ( ITEST .GT. MXRECL ) MXRECL = ITEST
2000  CONTINUE
      LROW   = ZI( INDEXR ) + ZI( INDEXR+1 ) - 1
C
C SAVE LAST PIVOT COLUMN FOR WHICH DATA IN THIS COLUMN IS USED
C      
      IF ( NTERMS .GT. MAXNAC ) MAXNAC = NTERMS
      ZI( INDDIR+2 ) = LROW
      IFIRSTC = ZI( INDDIR+1 )
      MAXTES  = ( ICURCOL - IFIRSTC + 1 )
      IF ( MAXTES .GT. MAXNCOL  ) MAXNCOL = MAXTES
      MAXTES  = NTERMS * ( ICURCOL - IFIRSTC )
      IF ( MAXTES .GT. MAXINLOP ) MAXINLOP = MAXTES
C
C CHECK TO DETERMINE IF ALL COLUMNS HAVE BEEN PROCESSED
C      
      IF ( ICURCOL .GE. NCOL ) GO TO 7777
C
C CHECK IF ONLY ONE TERM IN THIS COLUMN
C      
      IF ( NEXCOL .NE. 0 ) GO TO 2005
C
C MUST FIND FIRST NON-ZERO TERM FOLLOWING THE CURRENT PIVOT
C      
      DO 2002 K = ICURCOL+1, NCOL
      IF ( ZI( IACROW+K ) .NE. ICURCOL ) GO TO 2002
      NEXCOL = K
      GO TO 2005
2002  CONTINUE
      WRITE ( NOUT, 9901 ) ICURCOL
      GO TO 2030
9901  FORMAT(' SYMMETRIC DECOMPOSITION FOUND NO TERMS BEING '
     &,' CONNECTED TO DIAGONAL ON COLUMN ',I6)
2005  CONTINUE
C      PRINT *,' AFTER 2005,ICURCOL,NEXCOL=',ICURCOL,NEXCOL
C
C UPDATE ACTIVE ROWS IN COLUMN VECTOR FOR ALL TERMS OF THIS COLUMN
C      
      LEN = IRVAL + NRVALS - 1
      DO 2010 K = IRVAL, LEN, 2
      IROW = ZI( K )
      NROW = IROW + ZI( K+1 ) - 1
      DO 2010 L = IROW, NROW
      ZI( IACROW + L - 1 ) = NEXCOL      
2010  CONTINUE
      DO 2020 L = ICURCOL+1, MAXROW
      IF ( ZI( IACROW+L-1 ) .EQ. ICURCOL ) ZI( IACROW+L-1) = NEXCOL 
2020  CONTINUE
2030  CONTINUE
C
C END OF CURRENT COLUMN, PREPARE FOR NEXT COLUMN
C
C      write ( nout, 901 ) icurcol
C901   format(20x,' Active rows after processing column ',i10)
C      do 2040 l = 1, ncol
C      write ( nout, 902 ) l, zi(iacrow+l-1)
C902   format(' Row, next reference =',2i7)
C2040  continue
C      write ( nout, 903 )
C903   format(20x, ' Directory',/,
C     &' Column  Memory Index   First Used    Last Used    Savpos')
C      do 2050 l = 1, ncol
C      ind = ( l-1 ) * 4 + 1
C      write ( nout, 904 ) l, zi(ind), zi(ind+1), zi(ind+2), zi(ind+3)
C904   format( i7, i14, i13, i13, i9)
C2050  continue
      ICURCOL = ICURCOL + 1    
      INDDIR  = (ICURCOL-1)*4 + 1    
      GO TO 50
C
C THE FOLLOWING IS AN INTERNAL ROUTINE TO ADD COMPUTED TERMS RESULTING
C FROM THE PROCESSING OF PREVIOUS PIVOT COLUMNS INTO THE CURRENT ACTIVE
C ROWS FOR THE CURRENT COLUMN 
C
6000  CONTINUE
      DO 6100 K = IROW1, IROWN
      IF ( ZI(IACROW + K - 1 ) .LT. ICURCOL ) GO TO 6100
      IF ( NEXCOL .EQ. 0 ) NEXCOL = K 
C
C NEED TO ADD THIS TERM TO THE ACTIVE ROWS
C CHECK TO SEE IF THIS TERM IS AN EXTENSION OF CURRENT TERMS
C      
      IF ( (ZI( INDEXR ) + ZI( INDEXR+1 ) ) .EQ. K )
     &    GO TO 6010
C
C NO, NEED TO CREATE ANOTHER POINTER
C      
      INDEXR = INDEXR + 2
      ZI( INDEXR )   = K
      ZI( INDEXR+1 ) = 1
      NTERMS         = NTERMS +1
      GO TO 6020
6010  CONTINUE
C
C JUST ADD TO THE NUMBER OF CONSECUTIVE VALUES FOR CURRENT ROW
C      
      ZI( INDEXR+1 ) = ZI( INDEXR+1 ) + 1
      NTERMS         = NTERMS + 1
6020  CONTINUE      
C
C NOW, ZERO OUT ROW VALUE
C      
      GO TO ( 6030, 6040, 6050, 6060), KTYPE
C
C TYPE IS REAL SP
C
6030  ZR( INDEXV ) = 0.
      INDEXV = INDEXV + 1
      GO TO 6100
C
C TYPE IS REAL DP
C
6040  ZD( INDEXVD ) = 0.D0
      INDEXVD = INDEXVD + 1
      INDEXV  = INDEXV  + 2
      GO TO 6100
C
C TYPE IS COMPLEX SP
C
6050  ZR( INDEXV   ) = 0.
      ZR( INDEXV+1 ) = 0.
      INDEXV = INDEXV + 2
      GO TO 6100
C
C TYPE IS COMPLEX DP
C
6060  ZD( INDEXVD   ) = 0.D0
      ZD( INDEXVD+1 ) = 0.D0
      INDEXVD = INDEXVD + 2
      INDEXV  = INDEXV  + 4
      GO TO 6100
6100  CONTINUE
      GO TO ( 150, 1050 ), IRFLAG
C
C INSUFFICIENT MEMORY
C
7001  MINMUM = NCOL*7 + 2*NCOL*IVWRDS + 2*ISYSBF 
      WRITE ( NOUT, 9001 ) UFM, MCB(1), CNAME, NCOL, KTYPE
     &,                    LCORE, MINMUM
9001  FORMAT(1X,A23,/,' INSUFFICIENT MEMORY TO DECOMPOSE MATRIX IN '
     &,I4,' FILE NAME=',2A4
     &,/,' NUMBER OF COLUMNS=',I7,' TYPE=',I2,' MEMORY AVAILABLE =',I10
     &,/,' MINIMUM REQUIRED IS =',I10)
C      CALL MESAGE ( -8, 0, 0 )
      IERROR = 1
      GO TO 7777
7002  CALL FNAME ( MCB, NAME )
      WRITE ( NOUT, 9002 ) UFM, MCB(1), CNAME
9002  FORMAT(1X, A23, /,' SMCPH1 UNABLE TO OPEN FILE ',I4,' NAME= ',2A4)
      IERROR = 2
      CALL MESAGE ( -61, 0, 0 )
7003  CALL FNAME ( ISCR1, NAME )
      WRITE ( NOUT, 9003 ) UFM, ISCR1, CNAME
9003  FORMAT(1X, A23, /,' SMCPH1 UNABLE TO OPEN FILE ',I4,' NAME= ',2A4)
      IERROR = 2
      CALL MESAGE ( -61, 0, 0 )
C
C ZERO ON DIAGONAL, TERMINATE DECOMPOSITION BUT FIRST SCAN REST OF
C MATRIX TO DETERMINE OTHER COLUMNS WITH ZERO DIAGONALS.
C
7004  CONTINUE
      IERROR = 7
      IZEROS = 1
      INDEXZ = 0
      IF ( FRSTVAL ) GO TO 7020
      CALL ENDGET ( MBLK )
7010  CALL SKPREC ( MBLK, 1 )  
7020  INDEXZ = INDEXZ + 1
      ZI ( INDEXZ ) = ICURCOL
7025  ICURCOL = ICURCOL + 1
      IF ( ICURCOL .GT. NCOL ) GO TO 7050
      MBLK( 8 ) = -1
7030  CALL GETSTR ( *7020, MBLK )
      CALL ENDGET ( MBLK ) 
      IF ( ICURCOL .GE. MROW .AND. ICURCOL .LE. MROW+MTERMS-1)GO TO 7040
      IF ( MROW    .GT. ICURCOL ) GO TO 7010
      GO TO 7030
7040  CALL SKPREC ( MBLK, 1 )
      GO TO 7025
7050  CALL CLOSE ( MCB  , REW )
      CALL CLOSE ( ISCR1, REW )
      WRITE ( NOUT, 9050 ) UFM, CNAME, (ZI(K),K=1,INDEXZ)
9050  FORMAT(A23,' 3097, SYMMETRIC DECOMPOSITION OF DATA BLOCK ',2A4
     &,      ' ABORTED BECAUSE THE FOLLOWING COLUMNS ARE SINGULAR -'
     &,/,(5X,20I6,/))
      RETURN
7777  CONTINUE
C      CALL SMCHLP
C      CALL SMCDMP ( ZI, ZR, ZD )
7778  CONTINUE
      CALL CLOSE ( MCB, REW )
      ITWRDS  = IDBMAX - IDBIND
      ITCOLS  = NCOL   - NSPILL
      CALL SSWTCH ( 45, L45 )
      IF ( L45 .EQ. 0 ) GO TO 7779
      WRITE ( LOUT, 9004 ) ITCOLS , NSPILL , MAXNAC, MAXNCOL
     &,                    MAXINLOP, ITWRDS
9004  FORMAT(/
     &  ,14X,' STATISTICS FOR SYMMETRIC DECOMPOSITION OF FILE ',/
     &,/, 7X,' COLUMNS CONTAINED IN MEMORY                         =',I8
     &,/, 7X,' COLUMNS WRITTEN TO SPILL FILE                       =',I8
     &,/, 7X,' MAX. NO. OF ACTIVE ROWS FOR ANY ACTIVE COLUMN       =',I8
     &,/, 7X,' MAX. NUMBER OF COLUMNS REFERENCED BY A PIVOT COLUMN =',I8   
     &,/, 7X,' MAX. TERMS FOR ANY GIVEN INNER LOOP                 =',I8
     &,/, 7X,' TOTAL WORDS IN OPEN CORE USED FOR COLUMN DATA       =',I8 
     & )
      WRITE ( LOUT, 9005 ) 'INPUT ', CNAME, CTYPE( MCB( 5 ) )
      CALL FNAME ( LLL, NAME )
      WRITE ( LOUT, 9005 ) 'OUTPUT', CNAME, CTYPE( KTYPE )
9005  FORMAT(     
     &   8X, A6,' FILE: ',2A4     ,'      DATA TYPE= ',A14 )
c      CALL AUDIT( 'SMCPH1  ', 2 )
7779  CONTINUE
      RETURN
      END
