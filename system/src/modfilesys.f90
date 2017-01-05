!----------------------------------------------------------------------------------------------------------------------------------+
!                                                        M O D F I L E S Y S                                                       |
!                                                                                                                      D. Everhart |
!                                                                                                                      03 JAN 2017 |
!----------------------------------------------------------------------------------------------------------------------------------+
! The MIT License (MIT)
! 
! Copyright (c) 2017 Daniel Everhart
! 
! Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
! (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify,
! merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
! 
! The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
! 
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
! OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
! LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
! IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
!  File system module
!----------------------------------------------------------------------------------------------------------------------------------+
MODULE MODFILESYS
USE MODSYSTEM
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
INTEGER(KIND=4),PARAMETER     ::         LOGFILE_UNIT =            3 ! FORT LOGICAL UNIT NO. FOR SYSTEM LOGFILE
INTEGER(KIND=4)               ::         LOUT         = LOGFILE_UNIT ! FORT LOGICAL UNIT NO. FOR SYSTEM LOGFILE (used to be set by
                                                                     ! main program.
PRIVATE :: LOUT
COMMON /LOGOUT/ LOUT
!----------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4)               ::               IRDICT = RESTART_DICT_UNIT ! FORT LOGICAL UNIT NO. For system restart dictionary? 
                                                                          !   Was set in main program to 4.  This is the same as the
                                                                          !   restart dictionary, so we are assuming that it is the
                                                                          !   same here.
INTEGER(KIND=4)               ::               IROPEN                     ! Appears to be a flag relating to the restart dictionary.
                                                                          !   See subroutines: XCHK, XGPIMW, & XCSA.  This value 
                                                                          !   appears to only be set in XGPIMW. It does not appear
                                                                          !   to be initialized anywhere.  In XGPIMW, it is checked
                                                                          !   against the value of 1 before it is then assigned the
                                                                          !   value of 1.  It appears that the system depends on
                                                                          !   this value not accidentally being 1 at program
                                                                          !   initialization.
PRIVATE :: IRDICT,IROPEN
COMMON /RESDIC/ IRDICT,IROPEN
!----------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4)                     ::                    IEOR            
INTEGER(KIND=4)                     ::                   IOERR            
INTEGER(KIND=4)                     ::                  IPRVOP
INTEGER(KIND=4)                     ::                  IRETRN
INTEGER(KIND=4)                     ::                  IRWORD
INTEGER(KIND=4)                     ::                  IDATAD          
INTEGER(KIND=4)                     ::                    IDSN  
INTEGER(KIND=4)                     ::                     LCW   
INTEGER(KIND=4)                     ::                  LWORDS
INTEGER(KIND=4)                     ::                  MASKH1
INTEGER(KIND=4)                     ::                  MASKH2          
INTEGER(KIND=4)                     ::                  MASKE1
INTEGER(KIND=4)                     ::                  MASKE2
INTEGER(KIND=4)                     ::                  MASKE3
INTEGER(KIND=4)                     ::                  MASKE4
INTEGER(KIND=4)                     ::                  MAXDSN
INTEGER(KIND=4)                     ::                  NWORDS          
INTEGER(KIND=4)                     ::                   NBUFF 
INTEGER(KIND=4)                     ::                   IOBLK 
INTEGER(KIND=4)                     ::                    NBFZ  
INTEGER(KIND=4)                     ::                     NLR             
INTEGER(KIND=4)                     ::                  MASKQ1
INTEGER(KIND=4)                     ::                  MASKQ2
INTEGER(KIND=4)                     ::                  MASKQ3
INTEGER(KIND=4)                     ::                  MASKQ4
INTEGER(KIND=4)                     ::                    IDSX  
INTEGER(KIND=4)                     ::                    IDSP            
INTEGER(KIND=4)                     ::                    IDSC  
INTEGER(KIND=4)                     ::                   IDSRH 
INTEGER(KIND=4)                     ::                   IDSRT 
INTEGER(KIND=4)                     ::                   IDSSB 
INTEGER(KIND=4)                     ::                   IDSSE 
INTEGER(KIND=4)                     ::                   IDSCH           
INTEGER(KIND=4)                     ::                   IDSCT 
INTEGER(KIND=4)                     ::                   IDSSH 
INTEGER(KIND=4)                     ::                   IDSST 
INTEGER(KIND=4)                     ::                   IDSSD 
INTEGER(KIND=4)                     ::                   IDSEB 
INTEGER(KIND=4)                     ::                   IDSEF           
INTEGER(KIND=4)                     ::                  IBLOCK
INTEGER(KIND=4)                     ::                  LASNAM
INTEGER(KIND=4)                     ::                  MCBMAS
INTEGER(KIND=4)                     ::                   MULQ1 
INTEGER(KIND=4)                     ::                   MULQ2 
INTEGER(KIND=4)                     ::                   MULQ3
INTEGER(KIND=4)                     ::                   LHALF
INTEGER(KIND=4)                     ::                  LENDSP
INTEGER(KIND=4)                     ::                  LENWPB
INTEGER(KIND=4),DIMENSION(4)        ::                  NWRDEL
INTEGER(KIND=4),DIMENSION(52)       ::              DSIOF_CELL
PRIVATE ::   IEOR, IOERR,IPRVOP,IRETRN,IRWORD,IDATAD,  IDSN,   LCW,LWORDS,MASKH1,MASKH2,MASKE1,MASKE2,MASKE3,MASKE4,  &
           MAXDSN,NWORDS, NBUFF, IOBLK,  NBFZ,   NLR,MASKQ1,MASKQ2,MASKQ3,MASKQ4,  IDSX,  IDSP,  IDSC, IDSRH, IDSRT,  &
            IDSSB, IDSSE, IDSCH, IDSCT, IDSSH, IDSST, IDSSD, IDSEB, IDSEF,IBLOCK,LASNAM,MCBMAS, MULQ1, MULQ2, MULQ3,  &
            LHALF,LENDSP,LENWPB,NWRDEL,DSIOF_CELL
EQUIVALENCE ( DSIOF_CELL(   1 ),    IEOR  )                                                                           !           
EQUIVALENCE ( DSIOF_CELL(   2 ),   IOERR  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(   3 ),  IPRVOP  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(   4 ),  IRETRN  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(   5 ),  IRWORD  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(   6 ),  IDATAD  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(   7 ),    IDSN  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(   8 ),     LCW  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(   9 ),  LWORDS  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  10 ),  MASKH1  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  11 ),  MASKH2  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  12 ),  MASKE1  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  13 ),  MASKE2  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  14 ),  MASKE3  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  15 ),  MASKE4  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  16 ),  MAXDSN  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  17 ),  NWORDS  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  18 ),   NBUFF  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  19 ),   IOBLK  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  20 ),    NBFZ  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  21 ),     NLR  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  22 ),  MASKQ1  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  23 ),  MASKQ2  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  24 ),  MASKQ3  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  25 ),  MASKQ4  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  26 ),    IDSX  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  27 ),    IDSP  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  28 ),    IDSC  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  29 ),   IDSRH  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  30 ),   IDSRT  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  31 ),   IDSSB  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  32 ),   IDSSE  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  33 ),   IDSCH  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  34 ),   IDSCT  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  35 ),   IDSSH  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  36 ),   IDSST  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  37 ),   IDSSD  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  38 ),   IDSEB  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  39 ),   IDSEF  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  40 ),  IBLOCK  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  41 ),  LASNAM  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  42 ),  MCBMAS  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  43 ),   MULQ1  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  44 ),   MULQ2  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  45 ),   MULQ3  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  46 ),   LHALF  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  47 ),  LENDSP  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  48 ),  LENWPB  )                                                                           !
EQUIVALENCE ( DSIOF_CELL(  49 ),  NWRDEL  )                                                                           !  THRU 52
COMMON /  DSIO/ DSIOF_CELL
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
SUBROUTINE MODFILESYS_OPEN_SYSTEM_FILES
IMPLICIT NONE
                                        OPEN (  3, FILE=SYSTEM_FILENAMES( 3),STATUS='UNKNOWN')
IF ( SYSTEM_FILENAMES(11) .NE. 'none' ) OPEN ( 11, FILE=SYSTEM_FILENAMES(11),STATUS='UNKNOWN')
IF ( SYSTEM_FILENAMES(12) .NE. 'none' ) OPEN ( 12, FILE=SYSTEM_FILENAMES(12),STATUS='UNKNOWN')
IF ( SYSTEM_FILENAMES(10) .NE. 'none' ) OPEN ( 10, FILE=SYSTEM_FILENAMES(10),STATUS='UNKNOWN')
IF ( SYSTEM_FILENAMES( 4) .NE. 'none' ) OPEN (  4, FILE=SYSTEM_FILENAMES( 4),STATUS='UNKNOWN')
IF ( SYSTEM_FILENAMES( 1) .NE. 'none' ) OPEN (  1, FILE=SYSTEM_FILENAMES( 1),STATUS='UNKNOWN')
END SUBROUTINE MODFILESYS_OPEN_SYSTEM_FILES
!----------------------------------------------------------------------------------------------------------------------------------+
END MODULE MODFILESYS                        
!----------------------------------------------------------------------------------------------------------------------------------+
