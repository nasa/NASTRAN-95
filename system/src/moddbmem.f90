!----------------------------------------------------------------------------------------------------------------------------------+
!                                                        M O D D B M E M                                                           |
!                                                                                                                      D. Everhart |
!                                                                                                                      20 DEC 2016 |
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
!----------------------------------------------------------------------------------------------------------------------------------+
MODULE MODDBMEM
!----------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4)                     ::DB_BAS,           IDBBAS
INTEGER(KIND=4)                     ::DB_FRE,           IDBFRE
INTEGER(KIND=4)                     ::DB_DIR,           IDBDIR
INTEGER(KIND=4)                     ::                  INDBAS
INTEGER(KIND=4)                     ::                  INDCLR
INTEGER(KIND=4)                     ::                  INDCBP
INTEGER(KIND=4)                     ::                  NBLOCK
INTEGER(KIND=4)                     ::                  LENALC
INTEGER(KIND=4)                     ::                  IOCODE
INTEGER(KIND=4)                     ::                  IFILEX
INTEGER(KIND=4)                     ::                    NAME
INTEGER(KIND=4)                     ::DB_MAX_ALL_BUFFS, MAXALC
INTEGER(KIND=4)                     ::                  MAXBLK
INTEGER(KIND=4)                     ::                  MAXDSK
INTEGER(KIND=4)                     ::DB_MEMORY_LENGTH, IDBLEN
INTEGER(KIND=4)                     ::DB_ADDRESS_START, IDBADR
INTEGER(KIND=4)                     ::                  IBASBF
INTEGER(KIND=4)                     ::                  INDDIR 
INTEGER(KIND=4)                     ::                  NUMOPN
INTEGER(KIND=4)                     ::                  NUMCLS
INTEGER(KIND=4)                     ::                  NUMWRI
INTEGER(KIND=4)                     ::                  NUMREA
INTEGER(KIND=4)                     ::OPEN_CORE_LENGTH, LENOPC             ! Was set in the main program... not sure what the
                                                                           !   name should be. lengthOpenCore?
INTEGER(KIND=4),DIMENSION(23)       ::                DBM_CELL
!----------------------------------------------------------------------------------------------------------------------------------+
PRIVATE :: IDBBAS,IDBFRE,IDBDIR,INDBAS,INDCLR,INDCBP,NBLOCK,LENALC,IOCODE,IFILEX,  NAME,MAXALC,MAXBLK, &
           MAXDSK,IDBLEN,IDBADR,IBASBF,INDDIR,NUMOPN,NUMCLS,NUMWRI,NUMREA,LENOPC,DBM_CELL
!----------------------------------------------------------------------------------------------------------------------------------+
EQUIVALENCE ( DBM_CELL(   1), IDBBAS, DB_BAS                )                                   !           
EQUIVALENCE ( DBM_CELL(   2), IDBFRE, DB_FRE                )                                   !
EQUIVALENCE ( DBM_CELL(   3), IDBDIR, DB_DIR                )                                   !
EQUIVALENCE ( DBM_CELL(   4), INDBAS                        )                                   !
EQUIVALENCE ( DBM_CELL(   5), INDCLR                        )                                   !
EQUIVALENCE ( DBM_CELL(   6), INDCBP                        )                                   !
EQUIVALENCE ( DBM_CELL(   7), NBLOCK                        )                                   !
EQUIVALENCE ( DBM_CELL(   8), LENALC                        )                                   !
EQUIVALENCE ( DBM_CELL(   9), IOCODE                        )                                   !
EQUIVALENCE ( DBM_CELL(  10), IFILEX                        )                                   !
EQUIVALENCE ( DBM_CELL(  11),   NAME                        )                                   !
EQUIVALENCE ( DBM_CELL(  12), MAXALC, DB_MAX_ALL_BUFFS      )                                   !
EQUIVALENCE ( DBM_CELL(  13), MAXBLK                        )                                   !
EQUIVALENCE ( DBM_CELL(  14), MAXDSK                        )                                   !
EQUIVALENCE ( DBM_CELL(  15), IDBLEN, DB_MEMORY_LENGTH      )                                   !
EQUIVALENCE ( DBM_CELL(  16), IDBADR, DB_ADDRESS_START      )                                   !
EQUIVALENCE ( DBM_CELL(  17), IBASBF                        )                                   !
EQUIVALENCE ( DBM_CELL(  18), INDDIR                        )                                   !
EQUIVALENCE ( DBM_CELL(  19), NUMOPN                        )                                   !
EQUIVALENCE ( DBM_CELL(  20), NUMCLS                        )                                   !
EQUIVALENCE ( DBM_CELL(  21), NUMWRI                        )                                   !
EQUIVALENCE ( DBM_CELL(  22), NUMREA                        )                                   !
EQUIVALENCE ( DBM_CELL(  23), LENOPC, OPEN_CORE_LENGTH      )                                   !
!----------------------------------------------------------------------------------------------------------------------------------+
COMMON / DBM  / DBM_CELL
!----------------------------------------------------------------------------------------------------------------------------------+
                                                          CONTAINS
!----------------------------------------------------------------------------------------------------------------------------------+
SUBROUTINE MODDBMEM_INITIALIZE
USE MODSYSTEM
USE MODCORE
INTEGER LOCFX
CHARACTER(LEN=80) :: VAL = ''
!----------------------------------------------------------------------------------------------------------------------------------+
! This block sets up the core memory.  It reads the DBMEM size and OCMEM size
! from environment variables.  It is unclear why both values are read, because
! the IDBLEN value is re-calculated based on the total open core length LENOPC,
! less the OCMEM specified in the environment varable.  LENOPC (OPEN_CORE_LENGTH) used set to
! 14000000 initially in the main program.  It is then set to IOCMEM value read
! from the environment variable OCMEM.  Now, we just read it in.
!
! The result is that the core memory (ZZZZZZ) is set up so that the 'available'
! core goes to index (CORE_LAST_ADDRESS) and the DBMEM begins in the following
! index.
!
! DBMINT is called to initialized the DBMEM.
! What is not clear is why DBMEM is even specified.  It is overridden by OCMEM,
! so why not always have DBMEM calculated to use remainder of available core
! memory?

CALL GETENV ( 'DBMEM', VAL )
READ ( VAL, * ) DB_MEMORY_LENGTH
CALL GETENV ( 'OCMEM', VAL )
READ ( VAL, * ) OPEN_CORE_LENGTH

IF ( OPEN_CORE_LENGTH .GT. SYSTEM_CORE_SIZE ) THEN
  PRINT *,' LARGEST VALUE FOR OPEN CORE ALLOWED IS:',SYSTEM_CORE_SIZE
  CALL MESAGE ( -61, 0, 0 )
END IF

IF ( DB_MEMORY_LENGTH .NE. 0 ) DB_MEMORY_LENGTH = SYSTEM_CORE_SIZE - OPEN_CORE_LENGTH 
CORE_LAST_ADDRESS = LOCFX( SYSTEM_CORE( OPEN_CORE_LENGTH ) )
IF ( DB_MEMORY_LENGTH .NE. 0 ) DB_ADDRESS_START = LOCFX( SYSTEM_CORE( OPEN_CORE_LENGTH+1 ) )

!----------------------------------------------------------------------------   
!     DBMINT - INITIALIZES ALL PARAMETERS AND THE FREE BLOCK CHAIN              
!              FOR THE IN-MEMORY DATA BASE.                                     
!                                                                               
!        ARGUMENTS                                                              
!            IDBADR - (INPUT)-BEGINNING ADDRESS FOR IN-MEMORY DATA BASE         
!            IDBLEN - (INPUT)-NUMBER OF MEMORY WORDS FOR IN-MEMORY              
!                             DATA BASE                                         
!        / DBMPAR/                                                              
!            IDBBAS - (OUTPUT)-INDEX TO IN-MEMORY DATA BASE RELATIVE            
!                              TO /DBM/                                         
!            IDBFRE - (OUTPUT)-INDEX TO FREE CHAIN OF IN-MEMORY DATA            
!                              BASE RELATIVE TO /DBM/                           
!            IDBDIR - (OUTPUT)-INDEX TO FIRST DIRECTORY BLOCK                   
!        FREE CHAIN FORMAT                                                      
!               IDBFRE==> WORD 0 - 0   (POINTS TO PREVIOUS FREE BLOCK           
!                                      IN CHAIN, ALWAYS 0 FOR 1ST BLK)          
!                         WORD 1 - 0   (POINTS TO NEXT BLOCK IN CHAIN           
!                                      -INITIALLY SET TO ZERO)                  
!                         WORD 2 - L   (NUMBER OF FREE WORDS IN BLOCK)          
!        DIRECTORY FORMAT                                                       
!               THE FIRST TWO WORDS OF THE DIRECTORY BLOCK CONTAIN:             
!                   WORD  0 - MAXIMUM NUMBER OF ENTRIES IN DIRECTORY            
!                   WORD  1 - CURRENT ENTRIES IN THE DIRECTORY                  
!               EACH ENTRY IN THE DIRECTORY HAS THE FOLLOWING FORMAT            
!               (NOTE, FIRST ENTRY BEGINS AT WORD 3 OF BLOCK)                   
!                   WORD  0 - UNIT NUMBER OF DMAP FILE AS FOUND IN FIAT         
!                   WORD  1 - INDEX TO FIRST IN-MEMORY DATA BLOCK               
!                   WORD  2 - INDEX TO LAST IN-MEMORY DATA BLOCK                
!                   WORD  3 - INDEX TO CURRENT IN-MEMORY DATA BLOCK             
!                   WORD  4 - CURRENT BLOCK NUMBER BEING PROCESSED              
!                   WORD  5 - LAST BLOCK NUMBER                                 
!                   WORD  6 - ORIGINAL BUFFER ADDRESS                           
!                   WORD  7 - TOTAL BLOCKS (EXT. FILE + IN M. DB)               
!                   WORD  8 - OPEN FLAG FOR EXT. FILE (0,NO;1,YES)              
!               WORDS  9-10 - DMAP FILE NAME                                    
!               WORDS 11-16 - DMAP FILE TRAILER                                 
!----------------------------------------------------------------------------   
DB_DIR = 0                                                                
IF ( DB_MEMORY_LENGTH .NE. 0 ) THEN
  !  INITIALIZE THE CHAIN OF FREE BLOCKS AS ONE BIG FREE BLOCK                    
  DB_BAS = LOCFX( SYSTEM_CORE )                                                     
  DB_FRE = DB_ADDRESS_START - DB_BAS + 1                                              
  SYSTEM_CORE( DB_FRE )  = 0                                                        
  SYSTEM_CORE( DB_FRE+1) = 0                                                        
  SYSTEM_CORE( DB_FRE+2) = DB_MEMORY_LENGTH - 2                                               
  DB_MAX_ALL_BUFFS       = DB_MEMORY_LENGTH / ( BUFFER_LENGTH - 3 + 4 )                                  
  DB_DIR = 1                                                                
END IF
END SUBROUTINE MODDBMEM_INITIALIZE
!----------------------------------------------------------------------------------------------------------------------------------+
END MODULE MODDBMEM
