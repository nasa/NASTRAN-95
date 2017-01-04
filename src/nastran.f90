!----------------------------------------------------------------------------------------------------------------------------------+
!                                                        N A S T R A N                                                             |
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
!----------------------------------------------------------------------------------------------------------------------------------+
! This is the main program.  It is all new code.  It is loosely based on the original nastrn.f program file.  Everything has been
! migrated to FORTRAN 90 here, and it employs the use of MODULES instead of COMMON blocks.  The license is MIT, though it the
! NASA Open Source Agreement still applies to the rest of the code.
!----------------------------------------------------------------------------------------------------------------------------------+
PROGRAM NASTRAN        
USE MODFILESYS
USE MODDBMEM
IMPLICIT NONE
   
CPU_START_TIME = 0
CALL SECOND (CPU_START_TIME)
CALL WALTIM (PROBLEM_START_TIME)        
   
CALL BTSTRP
CALL MODDBMEM_INITIALIZE

SUPER_LINK = 1        
CURRENT_PAGE_COUNT = 1        

CALL MODFILESYS_INIT_FILENAMES
CALL MODFILESYS_OPEN_SYSTEM_FILES

CALL XSEM00       

STOP
END PROGRAM NASTRAN
!----------------------------------------------------------------------------------------------------------------------------------+
