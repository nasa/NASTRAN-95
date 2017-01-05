!----------------------------------------------------------------------------------------------------------------------------------+
!                                                        M O D S E M L N K                                                         |
!                                                                                                                      D. Everhart |
!                                                                                                                      02 JAN 2017 |
!----------------------------------------------------------------------------------------------------------------------------------+
! See NASA Open Source Agreement in top level directory.
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
! Some of this code used to exist in BTSTRP which was a bootstrapping routine for initializing system parameters.  Most of the
! initialization is taken care of in the MODSYSTEM module.  These values are here to use as a guide for editing system
! parameters...  The parameters in the module are set to match up with number 19, PC MS/DOS.
!
!     BASED ON MACHINE NUMBER, BTSTRP WILL DEFINE ALL THE     
!     MACHINE-DEPENDENT CONSTANTS NEEDED IN NASTRAN. THESE CONSTANTS    
!     ARE SAVED IN LABEL COMMONS /SYSTEM/, /LHPWX/, /MACHIN/ & /CHMACH/       
!        
!     SEE ALSO  PRTPRM, SDCMPS, SDR2E, AND UPCASE WHEN NASTRAN SOURCE   
!     CODE IS PORTED TO OTHER (NEW) MACHINE        
!        
!
!     MACH   = MACHX = HOST MACHINE        
!              ANY SUBROUTINE, THAT USES 'MACHX' INSTEAD OF 'MACH' IN   
!              LABEL COMMON /MACHIN/, CONTAINES MACHINE CONSTANTS THAT  
!              ARE USED LOCALLY.        
!     NMACH  = NUMBER OF MACHINES        
!     MCONST = ARRAY CONTAINING MACHINE DEPENDENT CONSTANTS        
!        
!        
!     COSMIC/NASTRAN SUPPORTS ONLY MACHINES 2, 5, 6, 7, 8, 9, 10, 16,        
!     21, & 22.   CONSTANTS FOR OTHER MACHINES MAY NOT BE EXACT      
!        
!     -MACHINE-    IBM/  UNIVAC   CDC   DEC/    DEC/   SUN   IBM/    HP    SGI    MAC   CRAY  
!           DUMMY   MVS    FTN   FTN5    VMS  ULTRIX SOLARIS  AIX    UX    IRIS        UNICOS 
!     MACH = -1-  ---2-  ---3-  ---4-  ---5-  ---6-  ---7-  ---8-  ---9-  --10-  --11-  --12- 
!                                                     +-------+
!          CONVEX   NEC  FUJITSU  SUN  AMDAHL  PRIME  |  PC   |          DEC/   DEC/ 
!                                SUNOS                |MS/DOS |  DUMMY OPENVMS  OSF  
!           --13-  --14-  --15-  --16-  --17-  --18-  | --19- |  --20-  --21-  --22- 
!                                                     +-------+
!                                                        ^
!                                                        |
!                                                        +- We Chose configuration 19 as as starting point.
!  MACHINE NAMES
!
!     DATA    COMPUT/  'DUMMY      ', 'IBM        ', 'UNIVAC     ', &
!                      'CDC        ', 'DEC-VAX    ', 'DEC-MIPS   ', &
!                      'SUN        ', 'IBM RS6000 ', 'HP         ', &
!                      'SGI        ', 'MACINTOCH  ', 'CRAY       ', &
!                      'CONVEX     ', 'NEC        ', 'FUJITSU    ', &
!                      'SUN        ', 'AMDAHL     ', 'PRIME      ', &
!                      'PC         ', 'DUMMY      ', 'DEC-ALPHA  ', &
!                      'DEC-ALPHA  '/
!     
!  MACHINE OPERATING SYSTEM
!
!     DATA    COMPOS/  '       ', 'MVS    ', 'FTN    ',     &
!                      'FTN5   ', 'VMS    ', 'ULTRIX ',     &
!                      'SOLARIS', 'AIX    ', 'HP-UX  ',     &
!                      'IRIX   ', '       ', 'UNICOS ',     &
!                      '       ', '       ', '       ',     &
!                      'SUNOS  ', '       ', '       ',     &
!                      'MS-DOS ', '       ', 'OPENVMS',     &
!                      'OSF    '/
!
!     SYSBUF        =  LENGTH OF NASTRAN I/O BUFFER
!     INTP(X100)    =  FORTRAN UNIT NO. FOR INPUT DATA        
!     OUTTAP        =  FORTRAN UNIT NO. FOR PRINTED OUTPUT        
!     NLPP(X100)    =  NUMBER OF LINES PRINTED PER PAGE        
!     NWPIC         =  NUMBER OF WORDS PER INPUT CARD, USED ONLY IN XGPIBS
!                        TODO:  Check XGPIBS.... does it even get used anymore?
!     NBPC(X100)    =  NUMBER OF BITS PER CHARACTER        
!     NBPW          =  NUMBER OF BITS PER WORD        
!     IPREC(X100)   =  PRECISION (1 = S.P., 2 = D.P.)        
!     RECL(X10)     =  DIRECT FILE RECORD LENGTH (USED IN FORTRAN OPEN    
!                      STATEMENT) BY WORDS (= 1), OR BYTE (= NCPW)        
!     QP            =  REAL*16 PRECISION FLAG (1 = YES, 0 = NO)        
!     LPCH(X100)    =  FORTRAN UNIT NO. FOR PUNCHED OUTPUT        
!     LDICT         =  FORTRAN UNIT NO. FOR RESTART DICTIONARY PUNCH      
!     LOWPW, HIGHPW =  MACHINE NUMERIC RANGE FOR S. P. REAL NUMBER,      
!                        USED ONLY BY RCARD, RCARD2, XRCARD AND YRCARD        
!     NUDFLW(X100)  =  FLOATING NUMBER UNDERFLOW CONTROL        
!                      (USED ONLY BY FQRW AND FQRWV)        
!     MXFL          =  MAXINUM FILES MAXFIL CAN REQUEST VIA THE NASTRAN  
!                      CARD, USED ONLY IN NASCAR        
!     KSHIFT        =  SHIFT COUNTS USED IN A DIVIDE TO CONVERT A GINO LOC    
!                      RETURNED FROM SAVPOS TO GINO BLOCK NUMBER, USED IN EMA 
!     MTISA         =  MANTISSA BITS, USED ONLY IN SDCMPS        
!                                                                                               +----------------------------------+
!                                                                                               |  Currently, we are using values  |
!                                                                                               |   in this location (CONFIG #19)  |
!                                                                                               +----------------------------------+
!                                                                                                               19
!                                                                                                                |
!                                                                                                                V
!----------------+-----------------------------------------------------------------------------------------------------------------+
! SYSBUF         |  200,4100, 871,  1042,1028,1028,1028,1028,1028,1028,1028,2052,1028,2052,2052,1028,1028,1028,1028,1028,1028,1028 |
! INTP,OUTTAP    |  506, 506, 506,   506, 506, 506, 506, 506, 506, 506, 506, 506, 506, 506, 506, 506, 506, 506, 506, 506, 506, 506 |
! NLPP, NWPIC    | 5000,5518,5518,  4208,5518,5518,5518,5518,5518,5518,5518,5509,5518,5500,5500,5500,5500,5500,5500,5500,5518,5500 |
! NBPC, NBPW     |  636, 832, 936,   660, 832, 832, 832, 832, 832, 832, 832, 864, 832, 864, 864, 832, 832, 832, 832, 832, 832, 832 |
! IPREC,RECL,QP  |  200, 240, 210,   110, 210, 210, 240, 240, 240, 210, 200, 180, 240, 100, 100, 200, 200, 200, 240, 200, 210, 200 |
! LPCH,LDICT     |  703, 707, 103,   707, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104 |
! HIGHPW (LOWPW) |   38,  75,  38,   321,  38,  38,  38,  38,  38,  38,  38,2465,  38,   0,   0,   0,   0,   0,  38,   0,  38,   0 |
! NUDFLW,MXFL    | 1650,1650,1849,  1475, 875,1675,1675,1675,1675,1675,1675,1675,1675,1675,1675,1675,1675,1675,1675,1675, 975,1675 |
! KSHIFT         |    1,4096,4096,262144,4096,4096,4096,4096,4096,4096,4096,4096,4096,   0,   0,   0,   0,   0,4096,   0,4096,   0 |
! MTISA          |    0,2426,2760,  4896,2355,2355,2352,2355,2355,2355,2355,4896,2352,4896,4896,2355,2355,2355,2355,   0,2355,   0 |
!----------------+-----------------------------------------------------------------------------------------------------------------+
SUBROUTINE BOOTSTRAP        
USE MODSYSTEM
USE MODSEMLNK
IMPLICIT NONE
EXTERNAL LSHIFT,RSHIFT,ANDF,COMPLF,KHRFN1,LOCFX  
INTEGER  LSHIFT,RSHIFT,ANDF,COMPLF,KHRFN1,LOCFX
INTEGER   I,KA,LOCF,IMASK
INTEGER   MVAX,FCB,ORDER,ABCD,AK,RECL,QP
DATA    MVAX  , ABCD  , KA   /  1H1,   4HABCD  ,4HA           /   
!
  IBMCDC = 1        
  IF (HOST_MACHINE.EQ.2 .OR. HOST_MACHINE.EQ.4) IBMCDC = 0        
!        
! MANTISSA BITS        
!        
  IF (PRECISION_FLAG .EQ. 1) THEN 
    MANTISSA_BITS = MANTISSA_BITS / 100
  ELSE IF (PRECISION_FLAG .EQ. 2) THEN 
    MANTISSA_BITS = MOD(MANTISSA_BITS,100)        
  END IF
!        
!     ZERO FIELD KA, AK AND GENERATE A IMASK FOR FIRST BYTE        
!        
  AK   = KHRFN1(0,1,KA,4)        
  KA   = KHRFN1(0,1,KA,1)        
  I    = 2**BITS_PER_CHAR - 1        
  IMASK = LSHIFT(I,BITS_PER_WORD-BITS_PER_CHAR)        
!        
!     CHECK BCD WORD (NOT CHARACTER WORD) STORING ORDER.        
!     IF 'ABCD' IS STORED INTERNALLY IN A-B-C-D ORDER, SET ORDER TO 0,  
!     OR  IF IT IS STORED IN REVERSED ORDER, D-C-B-A,  SET ORDER TO 1   
!        
  I = ANDF(ABCD,IMASK)        
  ORDER = 0        
  IF (BITS_PER_WORD.LT.60 .AND. I.NE.KA .AND. I.NE.AK) ORDER = 1        
!        
!     CHECK SYSTEM LOC OR %LOC FUNCTION.
!     IF SYSTEM LOC FUNCTION IS WORD COUNT, SET LOCF TO 1        
!     IF SYSTEM LOC FUNCTION IS BYTE COUNT, SET LOCF TO NCPW        
!        
!     Initially, assume LOCF = 1.  This is done by setting 
!     LOCF_QP_RECL_ORDER (formerly LQRO) to 1000.  
!     This is done to calculate the offset  for 10 WORDS.
!     The divide by 10 tells us the offset per word. LOCFX is
!     shown below to aid in understanding this calculation.
!        
!                 ENTRY LOCFX (I)        
!                 ===============        
!                 K = LQRO/1000        
!                 LOCFX = LOC(I)/K        
!                 RETURN        
!        
  LOCF_QP_RECL_ORDER = 1000
  I    = LOCFX(SYSTEM_CELL(11)) - LOCFX(SYSTEM_CELL(1))        
  LOCF = I/10        
!        
!     MERGE LOCF, QP, RECL, AND ORDER INTO LQRO        
!        
  RECL = 4            ! See table in comments below
  QP   = 0
  LOCF_QP_RECL_ORDER = LOCF*1000 + QP*100 + RECL*10 + ORDER        
!        
!        
!     GENERATE MASKS        
!                   7094      360         1108            6600        
!     MASK2  = 777777007777,FFFFFFF0,777777607777,77777760777777777777  
!     MASK3  = 377777777777,7FFFFFFF,377777777777,37777777777777777777  
!     TWO(1) = 020000000000,80000000,020000000000,00000000020000000000  
!        
  MASK2  = COMPLF(LSHIFT(2**BITS_PER_CHAR-1,BITS_PER_WORD-4*BITS_PER_CHAR))        
  MASK3  = RSHIFT(COMPLF(0),1)        
  MZERO  = LSHIFT(1,BITS_PER_WORD-1)        
  TWO(1) = LSHIFT(1,31)        
!        
!     TWO(1) = LSHIFT(1,31) = 2**31        
!            = +2147483648   IN MACHINES WITH MORE THAN 32-BIT WORD     
!            = -2147483648   IN 32-BIT MACHINES. A NEGATIVE NUMBER!     
!            = -0.000E0      IN SOME  32-BIT MACHINES        
!            = +0.000E0      IN OTHER 32-BIT MACHINES        
!     NOTICE FOR THE 32-BIT MACHINES, IABS(-2147483648) IS FATAL!       
!        
  LINK_NUMBER = LNKNOS(1)
!        
RETURN        
END SUBROUTINE BOOTSTRAP
