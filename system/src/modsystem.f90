!----------------------------------------------------------------------------------------------------------------------------------+
!                                                        M O D S Y S T E M                                                         |
!                                                                                                                      D. Everhart |
!                                                                                                                      02 JAN 2017 |
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
!----------------------------------------------------------------------------------------------------------------------------------+
!                   SYSTEM DEFINES VARIOUS MACHINE DEPENDENT, OPERATING SYSTEM, AND NASTRAN PARAMETERS.
!
! This module is a FORTRAN 90 wrapper for the /SYSTEM/ common.  It will give a more modern handle into the methods associated
! with the SYSTEM common. Eventually, methods can be added to facilitate modification of these methods.
!
! NOTE the EQUIVALENCE statements.  These are defining alternate, more meaningful names for these SYSTEM CELL values.
! Eventually, these values will all be named, and accessed through the more  modern F90 way.
! Comments for parameter descriptions taken from bd/semdbd.f DATA BLOCK routine.  Eventually, initiallization  of these values
! should take place here instead of the DATA BLOCK and BTSTRP subroutine.
! 
! Parameters are initialized as in the bd/semdbd.f.  Some are initialized as they will be set in BTSTRP, though BTSTRP still runs
! and sets these values.  Other commons such as MACHIN should be aded to this module and initialized similarly.
!----------------------------------------------------------------------------------------------------------------------------------+
MODULE MODSYSTEM
!----------------------------------------------------------------------------------------------------------------------------------+
                                                          IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4)               ::      BUFFER_LENGTH, SYSBUF  =    1028 ! (MACH DEP) NO. OF WORDS IN A GINO BUFFER.
INTEGER(KIND=4)               ::  PRINT_OUTPUT_UNIT, OUTTAP  =       6 ! (MACH DEP) FORTRAN LOGICAL UNIT NO. FOR SYSTEM PRINT OUTPUT
INTEGER(KIND=4)               ::   EXEC_STATUS_NOGO, NOGO    =       0 ! FLAG DEFINING EXECUTION STATUS DURING -FRONT END-.
INTEGER(KIND=4)               ::    INPUT_DATA_UNIT, INTP    =       5 ! (MACH DEP) FORT LOGICAL UNIT NO. FOR SYSTEM INPUT
INTEGER(KIND=4)               ::                     MPC     =       0 ! MULTI-POINT CONSTRAINT SET ID  FOR CURRENT SUBCASE.
INTEGER(KIND=4)               ::                     SPC     =       0 ! SINGLE-POINT CONSTR SET ID  FOR CURRENT SUBCASE.
INTEGER(KIND=4)               ::                     LOGFL   =       0 ! CONSOLE/LOGFILE MESSAGE CONTROL. USED ONLY IN MSFC,
                                                                       !   UNIVAC VER - LOGFL = 190
INTEGER(KIND=4)               ::                     LOAD    =       1 ! POINTER TO FIRST RECORD IN CASE CONTROL DATA BLOCK FOR
                                                                       !   CURRENT SUBCASE.
INTEGER(KIND=4)               ::     LINES_PER_PAGE, NLPP    =      55 ! (MACH DEP) NUMBER OF LINES PER PAGE OF PRINTED OUTPUT.
INTEGER(KIND=4)               ::                     MTEMP   =       0 ! MATERIAL TEMPERATURE SET ID.
INTEGER(KIND=4)               :: CURRENT_PAGE_COUNT, NPAGES  =       0 ! CURRENT  PAGE COUNT.
INTEGER(KIND=4)               ::                     NLINES  =       0 ! CURRENT NUMBER OF LINES ON CURRENT PAGE.
INTEGER(KIND=4)               ::                     TLINES  =       0 ! TOTAL NUMBER OF LINES PRINTED IN JOB.
INTEGER(KIND=4)               ::                     MXLINS  =   20000 ! MAX NO. OF LINES OF PRINTED OUTPUT FOR THE PROBLEM.
INTEGER(KIND=4),DIMENSION( 3) ::                     DATE    =       0 ! TODAY-S DATE, INTEGERS, 2 DIGITS EACH
   REAL(KIND=4)               ::     CPU_START_TIME, TIMEZ   =       0 ! CPU TIME IN SECONDS, WHEN PROBLEM BEGAN. NOT NECESSARILY
                                                                       !   ZERO. USED IN TMTOGO
INTEGER(KIND=4)               ::                     ECHOF   =       2 ! NUMBER INDICATING FORM OF BULK DATA ECHO.
INTEGER(KIND=4)               ::                     PLOTF   =       0 ! FLAG INDICATING REQUEST FOR STRUCT PLOTS
                                                                       !   (NON-ZERO = PLOT, SEE PLOTOPT IN SUB NASCAR FOR DETAILS)
INTEGER(KIND=4)               ::                     APPRCH  =       0 ! APPROACH FLAG (1=FORCE, 2=DISPL , 3=DMAP). APPRCH .LT. 0
                                                                       ! INDICATES A RESTART.
INTEGER(KIND=4)               ::        LINK_NUMBER, LINKNO  =       0 ! CURRENT LINK NO. (IN BCD, E.G. NSXX)  INITIALLY SET TO
                                                                       !   NS01 IN SUBROUTINE BTSTRP. SUBSEQUENTLY SET TO THE
                                                                       !   CORRECT LINK NO. IN SUBROUTINE ENDSYS. 
                                                                       ! = WAS THE MACHINE TYPE, MACH (LEVEL 17 AND EARLIER VERSION)
INTEGER(KIND=4)               ::                     LSYSTM  =     180 ! LENGTH OF SYSTEM COMMON BLOCK.
INTEGER(KIND=4)               ::                     ICFIAT  =      11 ! REPLACES (EDTUMF) FLAG HERE, WHICH IS NO LONGER USED.
                                                                       ! ICFIAT IS THE NUMBER OF WORDS PER FIAT ENTRY.
                                                                       !  . IF ICFIAT=8, DATA BLOCK GINO TRAILER 6 WORDS ARE PACKED
                                                                       !    INTO 4TH, 5TH, AND 6TH WORDS OF EACH FIAT ENTRY.
                                                                       !  . IF ICFIAT=11, NO PACKING IN FIAT ENTRY, AND THE TRAILER
                                                                       !    6 WORDS ARE SAVED IN 4TH THRU 6TH, AND 9TH THRU 11TH
                                                                       !    WORDS. THE TRAILER WORDS ARE THEREFORE NOT BOUNDED BY
                                                                       !    SIZE LIMITATION OF 65535 (HALF OF A 32-BIT WORD).
                                                                       !  . THE FIAT POINTERS IN /XFIST/ MUST BE IN COMPLETE
                                                                       !    AGREEMENT WITH THE SELECTION OF ICFIAT=8, OR ICFIAT=11.
                                                                       !    (SEE DATA SETTING OF /XFIST/ BELOW)
                                                                       !  . THE REST OF NASTRAN .MIS ROUTINES ARE CODED TO HANDLE
                                                                       !    ICFIAT=8 OR 11 AUTOMATICALLY. THE .MDS ROUTINES ARE
                                                                       !    NOT AFFECTED SINCE THE 7TH AND 8TH WORDS OF THE FIAT
                                                                       !    ENTRY REMAIN UNCHANGED.
                                                                       !  = WAS EDTUMF FLAG, USED IN PRE-1987 NASTRAN VER.
INTEGER(KIND=4)               ::                     RFFLAG  =       0 ! RIGID FORMAT FLAG
INTEGER(KIND=4)               ::                     CPPGCT  =       0 ! PAGE COUNT USED BY XCHK ROUTINE
INTEGER(KIND=4)               ::                     MN      =       0 ! NUMBER OF RINGS/NUMBER OF HARMONICS FOR AXISYMMETRIC SHELL.
INTEGER(KIND=4)               ::                     DUMMYI  =       0 ! (UNUSED WORD)
INTEGER(KIND=4)               ::                     MAXFIL  =      35 ! MAXIMUM NUMBER OF UNITS TO BE ALLOCATED TO FIAT.
INTEGER(KIND=4)               ::                     MAXOPN  =      16 ! MAXIMUM NUMBER OF FILES OPEN AT ONE TIME.
INTEGER(KIND=4)               ::                     HICORE  =   85000 ! HI-CORE LENGTH FOR UNIVAC AND VAX (GETS RESET TO 50,000 BY
                                                                       !   BTSTRP FOR VAX)
INTEGER(KIND=4)               :: PROBLEM_START_TIME, TIMEW   =       0 ! PROBLEM START TIME (INTEGR SECONDS AFTER MIDNITE)
INTEGER(KIND=4)               ::                     OFPFLG  =       0 ! OFP OPERATE FLAG - SET NON-ZERO WHEN OFP OPERATES
INTEGER(KIND=4)               ::                     NBRCBU  =      15 ! (CDC ONLY) LENGTH OF FET + DUMMY INDEX UNIVAC DRUM FILE
                                                                       !   ALLOCATION (1 FOR POSITION, 2 FOR TRACK)
INTEGER(KIND=4)               ::                     LPRUS   =      64 ! (CDC ONLY) NUMBER OF WORDS PER PHYSICAL RECORD UNIT (PRU)
INTEGER(KIND=4)               ::                     NPRUS   =       0 ! (CDC ONLY) NUMBER OF PRU-S PER GINO RECORD BLOCK
INTEGER(KIND=4)               ::                     KSYS37  =       0 ! ERROR CONTROL WORD, USED LOCALLY BY QPARMD AND QPARMR.
                                                                       !   ALSO USED LOCALLY IN LINK1 FOR NASINFO FILE UNIT NO.
INTEGER(KIND=4)               ::                     QQ      =       0 ! HYDROELASTIC PROBLEM FLAG.
INTEGER(KIND=4)               ::      BITS_PER_CHAR, NBPC    =       8 ! (MACH DEP) NO. OF BITS PER CHARACTER.
INTEGER(KIND=4)               ::      BITS_PER_WORD, NBPW    =      32 ! (MACH DEP) NO. OF BITS PER WORD.
INTEGER(KIND=4)               ::     CHARS_PER_WORD, NCPW    =       4 ! (MACH DEP) NO. OF CHARACTERS PER WORDS.
INTEGER(KIND=4),DIMENSION( 3) ::                     SYSDAT            ! THREE BCD WORD ARRAY CONTAINING MONTH, ' 19', AND LAST
                                                                       !   TWO DIGITS OF YEAR OF SYSTEM GENERATION DATE.
                                                                       !   THESE CELLS ARE SET BY SUBROUTINE NASCAR.
CHARACTER(LEN=12)             ::        SYSTEM_DATE  = 'APR. 19 95  '  ! System date in CHARACTER form to be EQUIVALENCEd with
                                                      ! | 1|| 2|| 3|   !   SYSDAT above. It looks weird because of word alignment.
INTEGER(KIND=4)               ::                     TAPFLG  =       0 ! WORD SET BY NASTRAN CARD TO INDICATE FILES TO BE TAPES
                                                                       !   WHETHER OR NOT THEY ARE ON DISK.  BITS TURNED ON COUNTING
                                                                       !   FROM RIGHT REPRESENT THE FILES IN XXFIAT.
INTEGER(KIND=4),DIMENSION( 9) ::                     ADUMEL  =       0 ! NINE WORD ARRAY CONTAINING DATA EXTRACTED FROM THE ADUM-I
                                                                       !   CARDS BY IFP.
INTEGER(KIND=4)               ::     PRECISION_FLAG, IPREC   =       2 ! PRECISION FLAG, 1=SP, 2=DP.
INTEGER(KIND=4)               ::                     ITHRML  =       0 ! THERMAL ANALYSIS FLAG:  0 = STRUCTURAL ANALYSIS,
                                                                       !                         1 = THERMAL ANALYSIS.
INTEGER(KIND=4),DIMENSION( 9) ::                     MODCOM  =       0 ! NINE WORD ARRAY FOR MODULE COMMUNICATIONS. SYSTEM(58),
                                                                       !   PRE-SELECT METHOD FOR MPYAD (1,2,3,DEFAULT=0) SYSTEM(59),
                                                                       !   PLOT TAPE TRACK SIZE
INTEGER(KIND=4),DIMENSION( 3) ::                     HDY     =       0 ! THREE WORD ARRAY ALA SEW
INTEGER(KIND=4)               ::                     SSCELL  =       0 ! MULTILEVEL SUBSTRUCTURE ANALYSIS COMMUNICATION CELL.
   REAL(KIND=4)               ::                     TOLEL   =    0.01 ! SINGULARITY TOLERANCE FOR SMA1,EMG. RESET BY NASCAR.
INTEGER(KIND=4)               ::                     MESDAY  =       0 ! DAYFILE MESSAGE FLAG
LOGICAL(KIND=4)               ::                     BITPAS  = .FALSE. ! CDC TAPE PROCESSING BIT - FALSE FOR LINK1 ONLY
LOGICAL(KIND=4)               ::                     PASS    = .FALSE. ! CDC MESSAGE AND TIMING FLAG - FALSE FOR LINK1 ONLY
INTEGER(KIND=4)               ::                     ITIME   =       0 ! WAS: WALL TIME ELAPSED SINCE PROBLEM START (SECONDS)
                                                                       ! IS : PROBLEM START TIME IN SECONDS SINCE JAN-1-1970,
                                                                       !      GREENWICH-MEAN-TIME (GMT)
INTEGER(KIND=4)               ::                     CTIME   =       0 ! WAS: CENTRAL PROCESSOR TIME SINCE PROBLEM START (SECONDS)
                                                                       ! IS : PRINT FLAG FOR DMAP SEQUENCE NO. AND NAME, ALL LINKS
                                                                       !      (SEE NASTRN OR NAST01.MDS)
INTEGER(KIND=4)               ::                     NOSBE   =       0 ! (CDC ONLY) FLAG FOR NOS(0) OR NOSBE(1)
INTEGER(KIND=4)               ::                     BANDIT  =       0 ! BANDIT OPTION FLAG (SEE BANDIT FOR MORE DETAILS)
INTEGER(KIND=4)               ::                     PZEL    =       0 ! PIEZOELECTRIC PROBLEM FLAG (INPUT VIA NASTRAN SYSTEM(78))
INTEGER(KIND=4),DIMENSION( 3) ::                     SWITCH  =       0 ! SENSE SWITCH BITS FOR DIAG CARD AND USED BY SSWTCH
INTEGER(KIND=4)               ::                     ICPFLG  =       0 ! CHECKPOINT FLAG (0 = NO CHECKPOINT, 1 = CHECKPOINT)
INTEGER(KIND=4)               ::                     JRUN    =       0 ! JRUN FOR VARIAN  (HEAT PROBLEM)
INTEGER(KIND=4)               ::                     JMAX    =       0 ! JMAX FOR VARIAN  (HEAT PROBLEM)
INTEGER(KIND=4)               ::                     LINTC   =     800 ! MAX. ALLOWABLE LINES OF INTERSECTION USED IN HDPLOT
INTEGER(KIND=4)               ::                     INTRA   =       0 ! INTERACTIVE REQUEST FLAG FOR PLOT, OUTPUT, AND SCAN
                                                                       !   (0=NONE, 1=PLOT ONLY, 2=OUTPUT PRINT AND SCAN ONLY,
                                                                       !    3=BOTH)
INTEGER(KIND=4)               ::                     OSPCNT  =      15 ! BAR OFFSET WARNING MESSAGE IF OFFSET BAR LENGTH EXCEEDS
                                                                       !   NON-OFFEST LENGTH BY THIS LIMIT (DEFAULT IS 15 PERCENT)
INTEGER(KIND=4),DIMENSION( 3) ::                     K8890   =       0 ! 3 WORDS RESERVED FOR USER. WILL NOT BE USED BY COSMIC
INTEGER(KIND=4)               ::  PUNCH_OUTPUT_UNIT, LPCH    =       1 ! (MACH DEP) FORTRAN LOGICAL UNIT NO. FOR PUNCH
INTEGER(KIND=4),PARAMETER     ::           RESTART_DICT_UNIT =       4 ! FORTRAN LOGICAL UNIT NO. FOR RESTART DICTIONARY PUNCH
INTEGER(KIND=4)               ::              LDICT= RESTART_DICT_UNIT ! FORTRAN LOGICAL UNIT NO. FOR RESTART DICTIONARY PUNCH
                                                                       ! FIXME Is this the same as IRDICT below?
INTEGER(KIND=4)               ::                     IAEROT  =       0 ! INTEGER FLAG INDICATING AERODYNAMIC THEORY (SPECIFIED
                                                                       !   VIA NASTRAN CARD AND USED ONLY IN APDB MODULE
                                                                       !     0 FOR COMPRESSOR BLADES, THEORY 6, DEFAULT,
                                                                       !     1 FOR SWEPT TURBOPROP. BLADES, THEORY 7)
INTEGER(KIND=4)               ::                     KSYS94  =       0 ! FLAG FOR REMOVALS OF MPYDRI(1), MPY4T(10), NEW FBS(100),
                                                                       !   TRNSPS(1000), AND NEW FBS IN FEER(10000)
INTEGER(KIND=4)               ::         SUPER_LINK, SPERLK  =       1 ! NASTRAN SUPERLINK FLAG. SET BY SEMDBD OR NASTRN FOR
                                                                       !   UNIX MACHINE
INTEGER(KIND=4),DIMENSION(56) ::                     LEFT    =       0 ! (85 UNUSED WORDS).  KSYS99 USED IN ERRTRC (SEE LEFT2)
INTEGER(KIND=4)               ::                     LOGLIN  =       0 !
INTEGER(KIND=4),DIMENSION(28) ::                     LEFT2   =       0 !
!----------------------------------------------------------------------------------------------------------------------------------+
PRIVATE :: SYSBUF, OUTTAP,   NOGO,   INTP,    MPC,    SPC,  LOGFL,   LOAD,   NLPP,  MTEMP, NPAGES, NLINES, &
           TLINES, MXLINS,   DATE,  TIMEZ,  ECHOF,  PLOTF, APPRCH, LINKNO, LSYSTM, ICFIAT, RFFLAG, CPPGCT, &
               MN, DUMMYI, MAXFIL, MAXOPN, HICORE,  TIMEW, OFPFLG, NBRCBU,  LPRUS,  NPRUS, KSYS37,     QQ, &
             NBPC,   NBPW,   NCPW, SYSDAT, TAPFLG, ADUMEL,  IPREC, ITHRML, MODCOM,    HDY, SSCELL,  TOLEL, &
           MESDAY, BITPAS,   PASS,  ITIME,  CTIME,  NOSBE, BANDIT,   PZEL, SWITCH, ICPFLG,   JRUN,   JMAX, &
            LINTC,  INTRA, OSPCNT,  K8890,   LPCH,  LDICT, IAEROT, KSYS94, SPERLK,   LEFT, LOGLIN,  LEFT2   
!----------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4),DIMENSION(180) :: SYSTEM_CELL
!----------------------------------------------------------------------------------------------------------------------------------+
EQUIVALENCE (SYSTEM_CELL(   1 ), SYSBUF, BUFFER_LENGTH          )  !           
EQUIVALENCE (SYSTEM_CELL(   2 ), OUTTAP, PRINT_OUTPUT_UNIT      )  !         
EQUIVALENCE (SYSTEM_CELL(   3 ),   NOGO, EXEC_STATUS_NOGO       )  !         
EQUIVALENCE (SYSTEM_CELL(   4 ),   INTP, INPUT_DATA_UNIT        )  !         
EQUIVALENCE (SYSTEM_CELL(   5 ),    MPC                         )  !         
EQUIVALENCE (SYSTEM_CELL(   6 ),    SPC                         )  !         
EQUIVALENCE (SYSTEM_CELL(   7 ),  LOGFL                         )  !         
EQUIVALENCE (SYSTEM_CELL(   8 ),   LOAD                         )  !         
EQUIVALENCE (SYSTEM_CELL(   9 ),   NLPP, LINES_PER_PAGE         )  !         
EQUIVALENCE (SYSTEM_CELL(  10 ),  MTEMP                         )  !         
EQUIVALENCE (SYSTEM_CELL(  11 ), NPAGES, CURRENT_PAGE_COUNT     )  !         
EQUIVALENCE (SYSTEM_CELL(  12 ), NLINES                         )  !         
EQUIVALENCE (SYSTEM_CELL(  13 ), TLINES                         )  !         
EQUIVALENCE (SYSTEM_CELL(  14 ), MXLINS                         )  !         
EQUIVALENCE (SYSTEM_CELL(  15 ),   DATE                         )  !  THRU  17
EQUIVALENCE (SYSTEM_CELL(  18 ),  TIMEZ, CPU_START_TIME         )  !         
EQUIVALENCE (SYSTEM_CELL(  19 ),  ECHOF                         )  !         
EQUIVALENCE (SYSTEM_CELL(  20 ),  PLOTF                         )  !         
EQUIVALENCE (SYSTEM_CELL(  21 ), APPRCH                         )  !          
EQUIVALENCE (SYSTEM_CELL(  22 ), LINKNO, LINK_NUMBER            )  !          
EQUIVALENCE (SYSTEM_CELL(  23 ), LSYSTM                         )  !          
EQUIVALENCE (SYSTEM_CELL(  24 ), ICFIAT                         )  !          
EQUIVALENCE (SYSTEM_CELL(  25 ), RFFLAG                         )  !          
EQUIVALENCE (SYSTEM_CELL(  26 ), CPPGCT                         )  !          
EQUIVALENCE (SYSTEM_CELL(  27 ),     MN                         )  !          
EQUIVALENCE (SYSTEM_CELL(  28 ), DUMMYI                         )  !          
EQUIVALENCE (SYSTEM_CELL(  29 ), MAXFIL                         )  !          
EQUIVALENCE (SYSTEM_CELL(  30 ), MAXOPN                         )  !          
EQUIVALENCE (SYSTEM_CELL(  31 ), HICORE                         )  !          
EQUIVALENCE (SYSTEM_CELL(  32 ),  TIMEW, PROBLEM_START_TIME     )  !          
EQUIVALENCE (SYSTEM_CELL(  33 ), OFPFLG                         )  !          
EQUIVALENCE (SYSTEM_CELL(  34 ), NBRCBU                         )  !          
EQUIVALENCE (SYSTEM_CELL(  35 ),  LPRUS                         )  !          
EQUIVALENCE (SYSTEM_CELL(  36 ),  NPRUS                         )  !          
EQUIVALENCE (SYSTEM_CELL(  37 ), KSYS37                         )  !          
EQUIVALENCE (SYSTEM_CELL(  38 ),     QQ                         )  !          
EQUIVALENCE (SYSTEM_CELL(  39 ),   NBPC, BITS_PER_CHAR          )  !          
EQUIVALENCE (SYSTEM_CELL(  40 ),   NBPW, BITS_PER_WORD          )  !          
EQUIVALENCE (SYSTEM_CELL(  41 ),   NCPW, CHARS_PER_WORD         )  !          
EQUIVALENCE (SYSTEM_CELL(  42 ), SYSDAT, SYSTEM_DATE            )  !  THRU  44
EQUIVALENCE (SYSTEM_CELL(  45 ), TAPFLG                         )  !          
EQUIVALENCE (SYSTEM_CELL(  46 ), ADUMEL                         )  !  THRU  54
EQUIVALENCE (SYSTEM_CELL(  55 ),  IPREC, PRECISION_FLAG         )  !          
EQUIVALENCE (SYSTEM_CELL(  56 ), ITHRML                         )  !          
EQUIVALENCE (SYSTEM_CELL(  57 ), MODCOM                         )  !  THRU  65
EQUIVALENCE (SYSTEM_CELL(  66 ),    HDY                         )  !  THRU  68
EQUIVALENCE (SYSTEM_CELL(  69 ), SSCELL                         )  !          
EQUIVALENCE (SYSTEM_CELL(  70 ),  TOLEL                         )  !          
EQUIVALENCE (SYSTEM_CELL(  71 ), MESDAY                         )  !          
EQUIVALENCE (SYSTEM_CELL(  72 ), BITPAS                         )  !          
EQUIVALENCE (SYSTEM_CELL(  73 ),   PASS                         )  !          
EQUIVALENCE (SYSTEM_CELL(  74 ),  ITIME                         )  !          
EQUIVALENCE (SYSTEM_CELL(  75 ),  CTIME                         )  !          
EQUIVALENCE (SYSTEM_CELL(  76 ),  NOSBE                         )  !          
EQUIVALENCE (SYSTEM_CELL(  77 ), BANDIT                         )  !          
EQUIVALENCE (SYSTEM_CELL(  78 ),   PZEL                         )  !          
EQUIVALENCE (SYSTEM_CELL(  79 ), SWITCH                         )  !  THRU  81
EQUIVALENCE (SYSTEM_CELL(  82 ), ICPFLG                         )  !          
EQUIVALENCE (SYSTEM_CELL(  83 ),   JRUN                         )  !          
EQUIVALENCE (SYSTEM_CELL(  84 ),   JMAX                         )  !          
EQUIVALENCE (SYSTEM_CELL(  85 ),  LINTC                         )  !          
EQUIVALENCE (SYSTEM_CELL(  86 ),  INTRA                         )  !          
EQUIVALENCE (SYSTEM_CELL(  87 ), OSPCNT                         )  !          
EQUIVALENCE (SYSTEM_CELL(  88 ),  K8890                         )  !  THRU  90
EQUIVALENCE (SYSTEM_CELL(  91 ),   LPCH, PUNCH_OUTPUT_UNIT      )  !          
EQUIVALENCE (SYSTEM_CELL(  92 ),  LDICT                         )  !          
EQUIVALENCE (SYSTEM_CELL(  93 ), IAEROT                         )  !          
EQUIVALENCE (SYSTEM_CELL(  94 ), KSYS94                         )  !          
EQUIVALENCE (SYSTEM_CELL(  95 ), SPERLK, SUPER_LINK             )  !          
EQUIVALENCE (SYSTEM_CELL(  96 ),   LEFT                         )  !  THRU 151
EQUIVALENCE (SYSTEM_CELL( 152 ), LOGLIN                         )  !          
EQUIVALENCE (SYSTEM_CELL( 153 ),  LEFT2                         )  !  THRU 180   
!----------------------------------------------------------------------------------------------------------------------------------+
COMMON /SYSTEM/ SYSTEM_CELL
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
END MODULE MODSYSTEM                        
!----------------------------------------------------------------------------------------------------------------------------------+
