      PROGRAM NASTRN        
      USE MODFILESYS
C        
CDE   D. Everhart
CDE   03 JAN 2017
CDE   Changes made to remove any reference to the DOSNAM common
CDE   (NASNAMES.COM).
CDE   
CDE   Additionally, a do loop was corrected.  (See DO 20.....)
CDE   The size of the DSNMAES array specified in DSIOF.COM and
CDE   NASNAMES.COM is only 89.  This loop was looping to 90 resulting
CDE   in overwriting memory somewhere else. This overwriting did not
CDE   appear to cause any errors before, but when removing the
CDE   NASNAMES.COM inclusion from GNFIAT, a wierd error occurred in a
CDE   seemingly unrelated place in the DBM common.  Anyway, it is
CDE   almost as if changing how GNFIAT accesses the DBM common changed
CDE   common alignment and now overwriting below causes a segfault.
CDE
CDE   All of the GETENV calls to set up file names were condensed
CDE   in to one-liners.
CDE
CDE   Update the scr file numbers to be I0.2
CDE
      CHARACTER*80    VALUE
      CHARACTER*5     TMP
      INTEGER         SPERLK
      REAL            SYSTM(94)
      COMMON / LSTADD / LASTAD
      COMMON / SYSTEM / ISYSTM(94),SPERLK
      COMMON / LOGOUT / LOUT
      COMMON / RESDIC / IRDICT, IROPEN
      COMMON / ZZZZZZ / IZ(14000000)
      COMMON / DBM    / IDBBAS, IDBFRE, IDBDIR, INDBAS, INDCLR, INDCBP
     &,                 NBLOCK, LENALC, IOCODE, IFILEX, NAME,   MAXALC
     &,                 MAXBLK, MAXDSK, IDBLEN, IDBADR, IBASBF, INDDIR
     &,                 NUMOPN, NUMCLS, NUMWRI, NUMREA, LENOPC
      COMMON / DSNAME / DSNAMES(89)
      CHARACTER * 80    DSNAMES
      EQUIVALENCE    ( ISYSTM, SYSTM )
      LENOPC = 14000000
C        
C     SAVE STARTING CPU TIME AND WALL CLOCK TIME IN /SYSTEM/        
C      
      ISYSTM(18) = 0
      CALL SECOND (SYSTM(18))        
      CALL WALTIM (ISYSTM(32))        
C        
C     EXECUTE NASTRAN SUPER LINK
C        
      LEN = 80
      VALUE = ' '
      CALL BTSTRP
      CALL GETENV ( 'DBMEM', VALUE )
      READ ( VALUE, * ) IDBLEN
      CALL GETENV ( 'OCMEM', VALUE )
      READ ( VALUE, * ) IOCMEM
      IF ( IOCMEM .LE. LENOPC ) GO TO 10
      PRINT *,' LARGEST VALUE FOR OPEN CORE ALLOWED IS:',LENOPC
      CALL MESAGE ( -61, 0, 0 )
10    IF ( IDBLEN .NE. 0 ) IDBLEN = LENOPC - IOCMEM 
      LASTAD = LOCFX( IZ( IOCMEM ) )
      IF ( IDBLEN .NE. 0 ) IDBADR = LOCFX( IZ( IOCMEM+1 ) )
      LENOPC = IOCMEM
      CALL DBMINT
      LOUT   = 3
      IRDICT = 4
      SPERLK = 1        
      ISYSTM(11) = 1        
C
      CALL MODFILESYS_INIT_FILENAMES
C
      OPEN (  3, FILE=DSNAMES(3) ,STATUS='UNKNOWN')
      IF ( DSNAMES(11) .NE. 'none' )
     & OPEN ( 11, FILE=DSNAMES(11),STATUS='UNKNOWN')
      IF ( DSNAMES(12) .NE. 'none' )
     & OPEN ( 12, FILE=DSNAMES(12),STATUS='UNKNOWN')
      IF ( DSNAMES(10) .NE. 'none' )
     & OPEN ( 10, FILE=DSNAMES(10),STATUS='UNKNOWN')
      IF ( DSNAMES(4) .NE. 'none' )
     & OPEN ( 4, FILE=DSNAMES(4),STATUS='UNKNOWN')
      IF ( DSNAMES(1) .NE. 'none' )
     & OPEN ( 1, FILE=DSNAMES(1),STATUS='UNKNOWN')
C
      CALL XSEM00       
C
      STOP
      END        
