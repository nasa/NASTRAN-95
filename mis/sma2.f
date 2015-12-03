      SUBROUTINE SMA2
C ******
C THIS ROUTINE IS A DRIVER AND INITIALIZATION PROGRAM FOR MODULE
C 2.4.2 OF THE NASTRAN SYSTEM.  IT GENERATES THE MASS MATRIX, MGG, AND
C THE DAMPING MATRIX, BGG.
C ******
C
C
C
C
      LOGICAL             HEAT
C
C
      DOUBLE PRECISION
     1                   DZ                 ,ZZZZZZ
C
C
C
      INTEGER
     1                   IZ(1)              ,EOR
     2,                  CLSRW              ,CLSNRW
     3,                  FROWIC
     4,                  TNROWS             ,OUTRW
     5,                  BGGIND
C
C
C
      DIMENSION
     1                   NMSMA2(2)
      COMMON /BLANK/      WTMASS             ,NOMGG
     1,                  NOBGG
C
C
C
      COMMON   /SYSTEM/  ISYS,ISEW1(53),IPREC,ITHERM
C
C SMA2 I/O PARAMETERS
C
      COMMON   /SMA2IO/
     1                   IFCSTM             ,IFMPT
     2,                  IFDIT              ,IDUM1
     3,                  IFECPT             ,IGECPT
     4,                  IFGPCT             ,IGGPCT
     5,                  IDUM2              ,IDUM3
     6,                  IFMGG              ,IGMGG
     7,                  IFBGG              ,IGBGG
     8,                  IDUM4              ,IDUM5
     9,                  INRW               ,OUTRW
     T,                  CLSNRW             ,CLSRW
     1,                  NEOR               ,EOR
     2,                  MCBMGG(7)          ,MCBBGG(7)
C
C SMA2 VARIABLE CORE
C
      COMMON   /ZZZZZZ /  Z(1)
C
C SMA2 VARIABLE CORE BOOKKEEPING PARAMETERS.
C
      COMMON   /SMA2BK/
     1                   ICSTM              ,NCSTM
     2,                  IGPCT              ,NGPCT
     3,                  IPOINT             ,NPOINT
     4,                  I6X6M              ,N6X6M
     5,                  I6X6B              ,N6X6B
C
C SMA2 PROGRAM CONTROL PARAMETERS
C
      COMMON   /SMA2CL/
     1                   IOPTB              ,BGGIND
     2,                  NPVT               ,LEFT
     3,                  FROWIC             ,LROWIC
     4,                  NROWSC             ,TNROWS
     5,                  JMAX               ,NLINKS
     6,                  LINK(10)           ,NOGO
C
C ELEMENT DATA
C
      COMMON   /GPTA1/ NELEMS, LAST, INCR, NE(1)
C
C ECPT COMMON BLOCK
C
      COMMON   /SMA2ET/
     1                   ECPT(100)
C
C SCRATCH BLOCK FOR ELEMENT ROUTINES
C
      COMMON   /SMA2DP/
     1                   ZZZZZZ(300)
C
      COMMON   /SMA2HT/  HEAT
C
      COMMON   /HMATDD/  IHMAT, NHMAT, MPTMPT, IDIT
C
C
      EQUIVALENCE
     1                   (Z(1),IZ(1),DZ)
C
C
C
      DATA
     1                   NMSMA2(1) /4HSMA2/ ,NMSMA2(2) /4H    /
C
C*****
C  SET HEAT FLAG
C*****
      HEAT = .FALSE.
      IF( ITHERM .NE. 0 )   HEAT = .TRUE.
C
C
      CALL DELSET
      IZMAX = KORSZ (Z)
C
C SET PURGE FLAGS FOR BGG AND NO PURGE FLAG FOR MGG.
C
      BGGIND = -1
      NOBGG  = -1
      NOMGG = 1
C
C ATTEMPT TO OPEN THE OUTPUT FILE FOR THE MASS MATRIX.  IF IT IS NOT
C IN THE OSCAR, EXECUTION WILL BE TERMINATED SINCE WE DO NOT ALLOW
C THE USER TO GENERATE ONLY A BGG. (EXCEPT IN A HEAT TRANSER PROBLEM)
C
      IGMGG = IZMAX - ISYS
      IF( HEAT ) GO TO 5
      CALL OPEN(*100,IFMGG,Z(IGMGG),OUTRW)
C
C WRITE A TWO WORD BCD HEADER AND CLOSE THE MGG FILE WITHOUT REWIND.
C
      CALL FNAME (IFMGG,Z(1))
      CALL WRITE (IFMGG,Z(1),2,EOR)
      CALL CLOSE (IFMGG,CLSNRW)
C
C ATTEMPT TO OPEN THE BGG FILE.
C
    5 IGBGG = IGMGG
      IOPTB = 0
      CALL OPEN(*10,IFBGG,Z(IGBGG),OUTRW)
      IOPTB = 1
      IGBGG = IGBGG - ISYS
      CALL FNAME (IFBGG,Z(1))
      CALL WRITE (IFBGG,Z(1),2,EOR)
      CALL CLOSE(IFBGG,CLSNRW)
C
C SET UP POINTERS TO GINO BUFFERS AND SET UP MATRIX CONTROL BLOCKS.
C
   10 IGECPT = IGBGG  - ISYS
      IGGPCT = IGECPT - ISYS
      MCBMGG(1) = IFMGG
      MCBMGG(2) = 0
      MCBMGG(3) = 0
      MCBMGG(4) = 6
      MCBMGG(5) = IPREC
      MCBMGG(6) = 0
      MCBMGG(7) = 0
      IF (IOPTB .EQ. 0) GO TO 30
      MCBBGG(1) = IFBGG
      DO 20 I = 2,7
   20 MCBBGG(I) = MCBMGG(I)
C
C ATTEMPT TO READ THE CSTM INTO CORE.
C
   30 NCSTM = 0
      ICSTM = 0
      LEFT = IGGPCT - 1
      CALL OPEN(*50,IFCSTM,Z(IGMGG),INRW)
      CALL FWDREC(*9020,IFCSTM)
      CALL READ(*9030,*40,IFCSTM,Z(1),LEFT,EOR,NCSTM)
C
C IF CORE WAS FILLED WITHOUT HITTING AN EOR CALL MESAGE
C
      CALL MESAGE (-8,IFCSTM,IFCSTM)
   40 LEFT = LEFT - NCSTM
C
C PRETRD SETS UP FUTURE CALLS TO TRANSD.
C
      CALL PRETRD (Z(ICSTM+1),NCSTM)
      CALL PRETRS(Z(ICSTM+1),NCSTM)
      CALL CLOSE (IFCSTM,CLSRW)
   50 IMAT1 = NCSTM
      NMAT1 = 0
      NMAT2 = 0
      NMAT3 = 0
      NMAT4 = 0
      IMAT11 = IMAT1 + 1
C*****
C  IF -HEAT- PROBLEM THEN HMAT IS USED FOR MAT4 AND MAT5 CARDS.
C*****
      IF( .NOT. HEAT ) GO TO 56
      IHMAT = IMAT11 + 1
      NHMAT = IMAT11 + LEFT - 2
      MPTMPT = IFMPT
      IDIT = IFDIT
      CALL HMAT( 0 )
      LEFT = LEFT - NHMAT + IHMAT
      IGPCT = NHMAT +1
      GO TO 58
   56 CALL PREMAT(IZ(IMAT11),Z(IMAT11),Z(IGMGG),LEFT,MATCR,IFMPT,IFDIT)
      LEFT = LEFT - MATCR
      IGPCT = NCSTM + MATCR
C
C OPEN THE ECPT INPUT FILE AND THE GPCT INPUT FILE.
C
   58 CALL OPEN(*9070,IFECPT,Z(IGECPT),INRW)
      CALL FWDREC(*9080,IFECPT)
      CALL OPEN(*9090,IFGPCT,Z(IGGPCT),INRW)
      CALL FWDREC(*9100,IFGPCT)
C
C REOPEN THE MGG OUTPUT FILE WITHOUT REWIND, AND THE BGG, IF CALLED FOR.
C
      IF(.NOT.HEAT)CALL OPEN(*9110,IFMGG,Z(IGMGG),3)
      IF(IOPTB.NE.0)CALL OPEN(*9120,IFBGG,Z(IGBGG),3)
C
C CALL SUBROUTINE SMA2A WHICH WILL PERFORM ALL THE COMPUTATIONS.
C
      CALL SMA2A
C
C CLOSE FILES AND WRITE TRAILERS.
C
      CALL CLOSE (IFGPCT,CLSRW)
      CALL CLOSE (IFECPT,CLSRW)
      CALL CLOSE (IFMGG ,CLSRW)
      MCBMGG(3) = MCBMGG(2)
      IF (MCBMGG(6) .NE. 0) GO TO 70
      DO 60 I = 2,7
   60 MCBMGG(I) = 0
      NOMGG = -1
   70 IF( .NOT. HEAT ) CALL WRTTRL( MCBMGG )
      IF (IOPTB .EQ. 0) GO TO 100
      CALL CLOSE (IFBGG ,CLSRW)
      IF (MCBBGG(6) .EQ. 0) GO TO 80
      MCBBGG(3) = MCBBGG(2)
      CALL WRTTRL (MCBBGG(1))
      NOBGG = 1
      GO TO 100
   80 DO 90 I = 2,7
   90 MCBBGG(I) = 0
      NOBGG = -1
  100 RETURN
C
C SUBROUTINE SMA2 ERROR EXITS.
C
 9020 IFILE = IFCSTM
      GO TO 10002
 9030 IFILE = - IFCSTM
      GO TO 10002
 9070 IFILE = IFECPT
      GO TO 10001
 9080 IFILE = IFECPT
      GO TO 10002
 9090 IFILE = IFGPCT
      GO TO 10001
 9100 IFILE = IFGPCT
      GO TO 10002
 9110 IFILE = IFMGG
      GO TO 10002
 9120 IFILE = IFBGG
      GO TO 10002
10001 IPARM = -1
      GO TO 10010
10002 IPARM = -2
10010 CALL MESAGE (IPARM,IFILE,NMSMA2(1))
      RETURN
      END
