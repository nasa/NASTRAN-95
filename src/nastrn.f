      PROGRAM NASTRN        
      USE MODFILESYS
      USE MODDBMEM
      USE MODCORE
C        
      CHARACTER*80    VALUE
      CHARACTER*5     TMP
      INTEGER         SPERLK
      REAL            SYSTM(94)
      COMMON / SYSTEM / ISYSTM(94),SPERLK
      COMMON / LOGOUT / LOUT
      COMMON / RESDIC / IRDICT, IROPEN
      COMMON / DSNAME / DSNAMES(89)
      CHARACTER * 80    DSNAMES
      EQUIVALENCE    ( ISYSTM, SYSTM )
C        
C     SAVE STARTING CPU TIME AND WALL CLOCK TIME IN /SYSTEM/        
C      
      ISYSTM(18) = 0
      CALL SECOND (SYSTM(18))        
      CALL WALTIM (ISYSTM(32))        
C        
C     EXECUTE NASTRAN SUPER LINK
C        
      VALUE = ' '
      CALL BTSTRP
      CALL MODDBMEM_INITIALIZE
C
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
