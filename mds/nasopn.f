      SUBROUTINE NASOPN ( *, LU, DSN )
      CHARACTER*80  IFILE, DSN
      CHARACTER*80 RFDIR
CDE   D. Everhart
CDE   03 JAN 2017
CDE   RFDIR was taken from the DOSNAM common.  In an effort to get
CDE   rid of this common, RFDIR is simply pulled from the environment
CDE   variable with the same name.
CDE   INCLUDE 'NASNAMES.COM'
      LOGICAL IEXIST
      CALL GETENV('RFDIR', RFDIR)
      KLEN = INDEX( RFDIR, ' ' )
      IFILE = RFDIR(1:KLEN-1) // '/NASINFO'
      DSN = IFILE
      INQUIRE ( FILE=IFILE, EXIST=IEXIST )
      IF ( .NOT. IEXIST ) GO TO 100
      OPEN ( UNIT=LU, FILE=IFILE, STATUS='OLD', ERR=100 )
      RETURN
100   RETURN 1
      END
