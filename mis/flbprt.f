      SUBROUTINE FLBPRT (IUSET,IEQEX,IBUF)
C
C     HYDROELEASTIC USET OUTPUT
C
C     PRINTS DOF VS. DISP SETS IF DIAG 32 IS ON.
C     PRINTS DISP SETS VS. DOF IF DIAG 33 IS ON.
C
      EXTERNAL        ANDF
      INTEGER         Z        ,SYSBUF   ,EQEXIN   ,D32      ,D33      ,
     1                FILE     ,NAME(2)  ,MSK(17)  ,ZGRD(10) ,TITLE(3,9)
     2,               ZDOF(10) ,TWO      ,UM       ,UO       ,UR       ,
     3                USG      ,USB      ,UL       ,UA       ,UF       ,
     4                US       ,UN       ,UG       ,UX       ,UY       ,
     5                UFR      ,UZ       ,UAB      ,UI       ,DASH     ,
     6                ASTRIC   ,BLANK    ,SBIT(17) ,EXPNT    ,UPBIT(17),
     7                ANDF
      CHARACTER       UFM*23   ,UWM*25
      COMMON /XMSSG / UFM      ,UWM
      COMMON /FLBFIL/ DUM1(8)  ,EQEXIN
      COMMON /ZZZZZZ/ Z(1)
      COMMON /SYSTEM/ SYSBUF   ,NOUT     ,DUM2(6)  ,NLPP     ,
     1                DUM3     ,NPAGE    ,LINE
      COMMON /TWO   / TWO(32)
      COMMON /BITPOS/ UM       ,UO       ,UR       ,USG      ,
     1                USB      ,UL       ,UA       ,UF       ,
     2                US       ,UN       ,UG       ,UE       ,
     3                UP       ,UNE      ,UFE      ,UD       ,
     4                UPS      ,USA      ,UK       ,UPA      ,
     5                U21      ,U22      ,U23      ,UX       ,
     6                UY       ,UFR      ,UZ       ,UAB      ,
     7                UI
      DATA    NAME  / 4HFLBP   , 4HRT    /
      DATA    TITLE / 4H       , 4H      , 4H MPC  ,
     1                4H       , 4H      , 4H SPC  ,
     2                4H       , 4H      , 4HOMIT  ,
     3                4H       , 4HANAL  , 4HYSIS  ,
     4                4H       , 4HPERM  , 4H SPC  ,
     5                4H       , 4HBDRY  , 4H SPC  ,
     6                4H   S   , 4HTRUC  , 4HTURE  ,
     7                4H       , 4H   F  , 4HLUID  ,
     8                4HFREE   , 4H SUR  , 4HFACE  /
      DATA    BLANK / 1H  /    , DASH    / 1H- /   , ASTRIC / 1H* /
C
C
C     DETERMINE IF ANY OUTPUT IS REQUESTED
C
      CALL SSWTCH (32,D32)
      CALL SSWTCH (33,D33)
      IF (D32.EQ.0 .AND. D33.EQ.0) RETURN
C
C     READ EQEXIN INTO CORE
C
      FILE = EQEXIN
      CALL OPEN (*1001,EQEXIN,Z(IBUF),0)
      CALL FWDREC (*1002,EQEXIN)
      CALL FWDREC (*1002,EQEXIN)
      NZ = IBUF - IEQEX
      CALL READ (*1002,*10,EQEXIN,Z(IEQEX),NZ,1,NEQEX)
      GO TO 1008
   10 CALL CLOSE (EQEXIN,1)
C
C     SORT ON INTERNAL ID
C
      CALL SORT (0,0,2,2,Z(IEQEX),NEQEX)
C
C     SET UP USET MASKS FOR DOF VS. DISP SET PRINTOUT
C
      IF (D32 .EQ. 0) GO TO 100
      MSK( 1) = TWO(USB)
      MSK( 2) = TWO(USG)
      MSK( 3) = TWO( UL)
      MSK( 4) = TWO( UA)
      MSK( 5) = TWO( UF)
      MSK( 6) = TWO( UN)
      MSK( 7) = TWO( UG)
      MSK( 8) = TWO( UR)
      MSK( 9) = TWO( UO)
      MSK(10) = TWO( US)
      MSK(11) = TWO( UM)
      MSK(12) = TWO( UX)
      MSK(13) = TWO( UY)
      MSK(14) = TWO(UFR)
      MSK(15) = TWO( UZ)
      MSK(16) = TWO(UAB)
      MSK(17) = TWO( UI)
C
      DO 20 I = 1,17
   20 SBIT(I) = 0
C
C     PASS THROUGH EQEXIN TABLE AND DETERMINE NUMBER OF DOF FOR EACH
C     POINT
C
      JUSET = IUSET - 1
      LINE  = NLPP
      INPNT = 0
      DO 60 K = 1,NEQEX,2
      ITYPE = MOD(Z(IEQEX+K),10)
      NDOF  = 6
      IF (ITYPE .EQ. 2) NDOF = 1
C
C     FOR EACH DOF - GET USET ENTRY AND TEST VARIOUS MACK BITS
C
      DO 50 KK = 1,NDOF
      JUSET = JUSET + 1
      IU    = Z(JUSET)
      INPNT = INPNT + 1
      EXPNT = Z(IEQEX+K-1)
      IDOF  = KK
      IF (NDOF .EQ. 1) IDOF = 0
      DO 30 IBIT = 1,17
      IF (ANDF(MSK(IBIT),IU) .NE. 0) GO TO 25
      UPBIT(IBIT) = BLANK
      GO TO 30
   25 UPBIT(IBIT) = ASTRIC
      SBIT (IBIT) = SBIT(IBIT) + 1
   30 CONTINUE
C
C     PRINT LINE OF OUTPUT
C
      LINE = LINE + 1
      IF (LINE .LE. NLPP) GO TO 40
      CALL PAGE1
      WRITE (NOUT,2000)
      LINE = 1
   40 WRITE (NOUT,2010) INPNT,EXPNT,DASH,IDOF,UPBIT
   50 CONTINUE
   60 CONTINUE
C
C     PRINT COLUMN TOTALS
C
      WRITE (NOUT,2020) SBIT
C
C     SET UP MASKS FOR DISP SET VS. DOF PRINTOUT
C
  100 IF (D33 .EQ. 0) RETURN
      MSK( 1) = TWO( UM)
      MSK( 2) = TWO( US)
      MSK( 3) = TWO( UO)
      MSK( 4) = TWO( UA)
      MSK( 5) = TWO(USG)
      MSK( 6) = TWO(USB)
      MSK( 7) = TWO( UX)
      MSK( 8) = TWO( UY)
      MSK( 9) = TWO(UFR)
C
C     PASS THROUGH EQEXIN TABLE ONCE FOR EACH DISP SET TO BE PRINTED
C
      DO 150 IMK = 1,9
      INUM  = -9
      ICOL  =  0
      LINE  = NLPP
      JUSET = IUSET - 1
      DO 130 K = 1,NEQEX,2
      ITYPE = MOD(Z(IEQEX+K),10)
      NDOF  = 6
      IF (ITYPE .EQ. 2) NDOF = 1
C
C     FOR EACH DOF - TEST IF IT IS IN DESIRED SET FOR THIS PASS
C
      EXPNT = Z(IEQEX+K-1)
      DO 120 KK = 1,NDOF
      JUSET = JUSET + 1
      IF (ANDF(Z(JUSET),MSK(IMK)) .EQ. 0) GO TO 120
      IDOF = KK
      IF (NDOF .EQ. 1) IDOF = 0
      ICOL = ICOL + 1
      ZGRD(ICOL) = EXPNT
      ZDOF(ICOL) = IDOF
      IF (ICOL .LT. 10) GO TO 120
C
C     WE HAVE ACUMULATED 10 POINTS - PRINT THEM
C
      ICOL = 0
      LINE = LINE + 1
      IF (LINE .LE. NLPP) GO TO 110
      CALL PAGE1
      WRITE (NOUT,2030) (TITLE(I,IMK),I=1,3)
      LINE = 1
  110 INUM = INUM + 10
      WRITE (NOUT,2040) INUM,(ZGRD(I),ZDOF(I),I=1,10)
C
  120 CONTINUE
  130 CONTINUE
C
C     PRINT ANY REMAINING ENTRIES
C
      IF (ICOL .EQ. 0) GO TO 150
      LINE = LINE + 1
      IF (LINE .LE. NLPP) GO TO 140
      CALL PAGE1
      WRITE (NOUT,2030) (TITLE(I,IMK),I=1,3)
      LINE = 1
  140 INUM = INUM + 10
      WRITE (NOUT,2040) INUM,(ZGRD(I),ZDOF(I),I=1,ICOL)
C
  150 CONTINUE
C
C     PRINT OUT COMPLETE
C
      RETURN
C
C     ERROR CONDITIONS - PRINT NON-FATAL MESSAGE
C
 1001 N = 1
      GO TO 1100
 1002 N = 2
      GO TO 1100
 1008 WRITE (NOUT,2050) UWM
      RETURN
 1100 CALL MESAGE (N,FILE,NAME)
      RETURN
C
C     FORMAT STATEMENTS
C
 2000 FORMAT (//12X,'INT DOF  EXT GP. DOF   SB   SG    L    A    F   ',
     1       'N    G    R    O    S    M    X    Y   FR    Z   AB    I',
     2       /1X,131(1H-))
 2010 FORMAT (10X,I8,1X,I8,1X,A1,I2,1X,17(4X,A1))
 2020 FORMAT (1H0,31H-- C O L U M N   T O T A L S -- ,17I5)
 2030 FORMAT (45X,3A4,17H DISPLACEMENT SET, //16X,3H-1-,8X,3H-2-,8X,
     1        3H-3-,8X,3H-4-,8X,3H-5-,8X,3H-6-,8X,3H-7-,8X,3H-8-,8X,
     2        3H-9-,7X,4H-10- ,/1H )
 2040 FORMAT (1H ,I6,1H=,10(1X,I8,1H-,I1))
 2050 FORMAT (A25,' 8011, INSUFFICIENT CORE TO HOLD CONTENTS OF EQEXIN',
     1     ' DATA BLOCK', /31X,'HYDROELASTIC USET PRINTOUT TERMINATED.')
      END
