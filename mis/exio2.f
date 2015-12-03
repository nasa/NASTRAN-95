      SUBROUTINE EXIO2
C
C     EXIO2 COPIES SUBSTRUCTURE ITEMS BETWEEN THE SOF AND AN EXTERNAL
C     TAPE USING FORTRAN FORMATTED IO.  THE TAPE COULD HAVE BEEN CREATED
C     OR COULD BE READ ON A DIFFERENT BRAND OF COMPUTER.
C
C
      LOGICAL         UNIVAC
      INTEGER         DRY      ,XBLK     ,UNAME    ,POS      ,
     1                UNIT     ,FORT     ,NUM(32)  ,SOFIN    ,
     2                SOFOUT   ,REWI     ,EQF
      CHARACTER       UFM*23   ,UWM*25
      COMMON /XMSSG / UFM      ,UWM
      COMMON /BLANK / DRY      ,XBLK     ,DEVICE(2),UNAME(2) ,
     1                FORMT(2) ,MODE(2)  ,POS(2)   ,DATYPE(2),
     2                NAMES(10),UNIT     ,UNIVAC   ,LBUF     ,
     3                IADD
      COMMON /SYSTEM/ SYSBUF   ,NOUT     ,X1(36)   ,NBPC     ,
     1                NBPW     ,NCPW
      DATA    FORT  , SOFIN    ,SOFOUT   ,REWI     ,EQF      /
     1        4HFORT, 4HSOFI   ,4HSOFO   ,4HREWI   ,4HEOF    /
      DATA    NUM   /
     1        2H1   , 2H2      ,2H3      ,2H4      ,2H5      ,
     2        2H6   , 2H7      ,2H8      ,2H9      ,2H10     ,
     3        2H11  , 2H12     ,2H13     ,2H14     ,2H15     ,
     4        2H16  , 2H17     ,2H18     ,2H19     ,2H20     ,
     5        2H21  , 2H22     ,2H23     ,2H24     ,2H25     ,
     6        2H26  , 2H27     ,2H28     ,2H29     ,2H30     ,
     7        2H31  , 2H32     /
C
C     INITIALIZE
C
      NOGO = 0
C
C     DECODE FORTRAN UNIT
C
      IF (UNAME(1) .NE. FORT) GO TO 20
      DO 10 I = 1,32
      UNIT = I
      IF (UNAME(2) .EQ. NUM(UNIT)) GO TO 30
   10 CONTINUE
   20 NOGO = 1
      CALL PAGE2 (-2)
      WRITE (NOUT,6356) UWM,UNAME
C
C     DECODE MODE OF OPERATION
C
   30 IOMODE = 0
      IF (MODE(1) .EQ. SOFOUT) IOMODE = 1
      IF (MODE(1) .EQ. SOFIN ) IOMODE = 2
      IF (IOMODE  .GT.      0) GO TO 40
      NOGO = 1
      CALL PAGE2 (-2)
      WRITE (NOUT,6338) UWM,MODE
C
C     IF ERRORS THEN QUIT
C
   40 IF (NOGO .EQ. 0) GO TO 50
      DRY = -2
      GO TO 300
C
C     SET POSITION AND UNIVAC FLAGS
C
   50 UNIVAC = .TRUE.
      IF (XBLK .LE. 0) XBLK = 3960
      XBLK = XBLK - MOD(XBLK,132)
      LBUF = XBLK/NCPW
      IF (MOD(XBLK,NCPW) .NE. 0) LBUF = LBUF + 1
      IADD = 2
      IF (POS(1) .EQ. REWI) IADD = 1
      IF (POS(1) .EQ.  EQF) IADD = 3
C
C     BRANCH ON MODE OF OPERATION
C
      GO TO (100,200), IOMODE
C
C     SOFOUT
C
  100 CALL EXO2
      GO TO 300
C
C     SOFIN
C
  200 CALL EXI2
C
C     NORMAL MODULE COMPLETION
C
  300 RETURN
C
C     MESSAGE TEXT
C
 6338 FORMAT (A25,' 6338, ',2A4,' IS AN INVALID MODE PARAMETER FOR ',
     1       'MODULE EXIO')
 6356 FORMAT (A25,' 6356, ',2A4,' IS AN INVALID UNIT FOR MODULE EXIO,',
     1       ' EXTERNAL FORMAT')
      END
