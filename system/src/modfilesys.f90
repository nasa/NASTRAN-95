!----------------------------------------------------------------------------------------------------------------------------------+
!                                                        M O D F I L E S Y S                                                       |
!                                                                                                                      D. Everhart |
!                                                                                                                      03 JAN 2017 |
!----------------------------------------------------------------------------------------------------------------------------------+
!  File system module
!----------------------------------------------------------------------------------------------------------------------------------+
MODULE MODFILESYS
!----------------------------------------------------------------------------------------------------------------------------------+
                                                      IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4),PARAMETER           ::                  MAXPRI  =      80 
INTEGER(KIND=4),PARAMETER           ::                  MAXFCB  =      89 
PRIVATE :: MAXFCB, MAXPRI
!----------------------------------------------------------------------------------------------------------------------------------+
CHARACTER(LEN=MAXPRI),DIMENSION(MAXFCB) ::      SYSTEM_FILENAMES            ! Initializing this to empty string causes problems
                                                                            ! in opening the NASINFO file ( see NASOPN, NSINFO).
                                                                            ! Not sure what is going on here, so for now, it
                                                                            ! remains uninitialized.
CHARACTER(LEN=MAXPRI)                   ::        PUNCH_FILENAME
CHARACTER(LEN=MAXPRI)                   ::          LOG_FILENAME
CHARACTER(LEN=MAXPRI)                   ::   DICTIONARY_FILENAME
CHARACTER(LEN=MAXPRI)                   ::  OUTPUT_TAPE_FILENAME
CHARACTER(LEN=MAXPRI)                   ::   INPUT_TAPE_FILENAME
CHARACTER(LEN=MAXPRI)                   ::         PLOT_FILENAME
CHARACTER(LEN=MAXPRI)                   ::     OUTPUT11_FILENAME
CHARACTER(LEN=MAXPRI)                   ::      INPUT12_FILENAME
EQUIVALENCE ( SYSTEM_FILENAMES(   1 ),            PUNCH_FILENAME )
EQUIVALENCE ( SYSTEM_FILENAMES(   3 ),              LOG_FILENAME )
EQUIVALENCE ( SYSTEM_FILENAMES(   4 ),       DICTIONARY_FILENAME )
EQUIVALENCE ( SYSTEM_FILENAMES(   7 ),      OUTPUT_TAPE_FILENAME )
EQUIVALENCE ( SYSTEM_FILENAMES(   8 ),       INPUT_TAPE_FILENAME )
EQUIVALENCE ( SYSTEM_FILENAMES(  10 ),             PLOT_FILENAME )
EQUIVALENCE ( SYSTEM_FILENAMES(  11 ),         OUTPUT11_FILENAME )
EQUIVALENCE ( SYSTEM_FILENAMES(  12 ),          INPUT12_FILENAME )
COMMON /DSNAME/ SYSTEM_FILENAMES
!----------------------------------------------------------------------------------------------------------------------------------+
CHARACTER(LEN=MAXPRI)                   ::      SCRATCH_DIRECTORY           ! 
CHARACTER(LEN=MAXPRI)                   :: RIGID_FORMAT_DIRECTORY           ! 
!----------------------------------------------------------------------------------------------------------------------------------+
CHARACTER(LEN=80),DIMENSION(10)     ::                    SDSN         = '' ! FIXME: document the purpose of these filenames.
                                                                            !        Appears to only be used in SOFIO routine.
COMMON /SOFDSN/ SDSN
!----------------------------------------------------------------------------------------------------------------------------------+
                                                          CONTAINS
!----------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE MODFILESYS_INIT_FILENAMES
IMPLICIT NONE
CHARACTER(LEN=5)  :: TMP = ''
INTEGER(KIND=4)   :: I
!
!     Initialize SYSTEM_FILENAMES (DSNAMES common) to be SCRXX files in the
!     temp location. TODO: GETENV is the old version.  This routine needs to
!     convert to GET_ENVIRONMENT_VARIABLE subroutine which has more error
!     checking capability.
!
CALL GETENV ( 'RIGID_FORMAT_DIRECTORY',  RIGID_FORMAT_DIRECTORY  )
RIGID_FORMAT_DIRECTORY = TRIM(ADJUSTL(RIGID_FORMAT_DIRECTORY))

CALL GETENV ( 'SCRATCH_DIRECTORY', SCRATCH_DIRECTORY )
SCRATCH_DIRECTORY = TRIM(ADJUSTL(SCRATCH_DIRECTORY))

DO I = 1, MAXFCB
  WRITE ( TMP, '(A3,I0.2)' ) 'scr',I
  SYSTEM_FILENAMES(I) = TRIM(SCRATCH_DIRECTORY)//'/'//TMP
END DO

! Set file names based on environment varialbles.

CALL GETENV ( 'PUNCHNM',       PUNCH_FILENAME )
CALL GETENV (   'LOGNM',         LOG_FILENAME )
CALL GETENV (  'DICTNM',  DICTIONARY_FILENAME )
CALL GETENV (  'OPTPNM', OUTPUT_TAPE_FILENAME )
CALL GETENV (  'NPTPNM',  INPUT_TAPE_FILENAME )
CALL GETENV (   'PLTNM',        PLOT_FILENAME )
CALL GETENV (   'FTN11',    OUTPUT11_FILENAME )
CALL GETENV (   'FTN12',     INPUT12_FILENAME )
CALL GETENV (   'FTN13', SYSTEM_FILENAMES(13) )
CALL GETENV (   'FTN14', SYSTEM_FILENAMES(14) )
CALL GETENV (   'FTN15', SYSTEM_FILENAMES(15) )
CALL GETENV (   'FTN16', SYSTEM_FILENAMES(16) )
CALL GETENV (   'FTN17', SYSTEM_FILENAMES(17) )
CALL GETENV (   'FTN18', SYSTEM_FILENAMES(18) )
CALL GETENV (   'FTN19', SYSTEM_FILENAMES(19) )
CALL GETENV (   'FTN20', SYSTEM_FILENAMES(20) )
CALL GETENV (   'FTN21', SYSTEM_FILENAMES(21) )

! SOFIO files.

CALL GETENV (    'SOF1',    SDSN( 1) )
CALL GETENV (    'SOF2',    SDSN( 2) )
CALL GETENV (    'SOF3',    SDSN( 3) )
CALL GETENV (    'SOF4',    SDSN( 4) )
CALL GETENV (    'SOF5',    SDSN( 5) )
CALL GETENV (    'SOF6',    SDSN( 6) )
CALL GETENV (    'SOF7',    SDSN( 7) )
CALL GETENV (    'SOF8',    SDSN( 8) )
CALL GETENV (    'SOF9',    SDSN( 9) )
CALL GETENV (   'SOF10',    SDSN(10) )

END SUBROUTINE MODFILESYS_INIT_FILENAMES
!----------------------------------------------------------------------------------------------------------------------------------+
END MODULE MODFILESYS                        
!----------------------------------------------------------------------------------------------------------------------------------+
