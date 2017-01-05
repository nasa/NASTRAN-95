!----------------------------------------------------------------------------------------------------------------------------------+
!                                                        M O D S E M L N K                                                         |
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
!                   LINK DRIVERS
!
!----------------------------------------------------------------------------------------------------------------------------------+
MODULE MODSEMLNK
!----------------------------------------------------------------------------------------------------------------------------------+
                                                      IMPLICIT NONE
!----------------------------------------------------------------------------------------------------------------------------------+

!----------------------------------------------------------------------------------------------------------------------------------+
!     SEM DEFINES DATA FOR THE LINK DRIVERS (XSEMI).
!     MASK   = OSCAR MASK
!     MASK2,MASK3 =  OSCAR MASKS (MACHINE DEPENDENT).
!     NAME   = ARRAY OF LINK NAMES
!
!WKBR COMMON /SEM   / MASK, MASK2, MASK3, NAME(15)
INTEGER(KIND=4)                ::  MASK        = 65535
INTEGER(KIND=4)                ::  MASK2       =     0
INTEGER(KIND=4)                ::  MASK3       =     0
INTEGER(KIND=4),DIMENSION(30)  ::  LNKNOS
CHARACTER(LEN=4),DIMENSION(30) ::  LINK_NAMES  = (/  'NS01', 'NS02', 'NS03', 'NS04', 'NS05', 'NS06',     &
                                                     'NS07', 'NS08', 'NS09', 'NS10', 'NS11', 'NS12',     &
                                                     'NS13', 'NS14', 'NS15', 'NS16', 'NS17', 'NS18',     &
                                                     'NS19', 'NS20', 'NS21', 'NS22', 'NS23', 'NS24',     &
                                                     'NS25', 'NS26', 'NS27', 'NS28', 'NS29', 'NS30' /)
EQUIVALENCE (LNKNOS, LINK_NAMES)
!PRIVATE ::   MASK, MASK2, MASK3, LNKNOS, LINK_NAMES
COMMON /SEM/ MASK, MASK2, MASK3, LNKNOS
!----------------------------------------------------------------------------------------------------------------------------------+
!                                                         CONTAINS
!----------------------------------------------------------------------------------------------------------------------------------+
!----------------------------------------------------------------------------------------------------------------------------------+
END MODULE MODSEMLNK
!----------------------------------------------------------------------------------------------------------------------------------+
