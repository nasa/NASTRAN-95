!----------------------------------------------------------------------------------------------------------------------------------+
!                                                        M O D C O R E                                                             |
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
!
! This module is a FORTRAN 90 wrapper for the /ZZZZZZ/ common.  Currently, the nastrn.f main program sizes this to 14,000,000 
! words.
!
!----------------------------------------------------------------------------------------------------------------------------------+
MODULE MODCORE
!----------------------------------------------------------------------------------------------------------------------------------+
                                                          IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------------------+
INTEGER(KIND=4),PARAMETER                   :: SYSTEM_CORE_SIZE = 14000000 ! Size of the system core in words.
INTEGER(KIND=4),DIMENSION(SYSTEM_CORE_SIZE) :: SYSTEM_CORE
COMMON / ZZZZZZ / SYSTEM_CORE
!----------------------------------------------------------------------------------------------------------------------------------+
END MODULE MODCORE
!----------------------------------------------------------------------------------------------------------------------------------+
