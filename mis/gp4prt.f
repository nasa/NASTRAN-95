      SUBROUTINE GP4PRT (IBUF)
C
C     1. PRINTS  DOF VS. DISP. SETS IF DIAG 21 ON.
C     2. PRINTS  DISP. SETS VS. DOF IF DIAG 22 ON.
C     3. CREATES SUBSTRUCTURE COUPLING DATA TABLE.
C
C     IF IBUF IS .LT. 0, SOME FILES MAY NOT BE CLOSED PROPERLY WHEN
C     THIS ROUTINE IS CALLED
C
      EXTERNAL        ANDF,ORF
      INTEGER         TITLE(2,8),Z,SYSBUF,ANDF,EQEXIN,ORF,FILE,NAME(3),
     1                D21,D22,MSK(12),IFLG(8),SCR1,ZDUM(10),ZCOM(10),
     2                EREC1,BUF,TWO,UM,UO,UR,USG,USB,UL,UA,UF,US,UN,UG,
     3                DASH,UPBIT(12),BLANK,TDB204,NAM204(2),TRL(7),
     4                EXFLAG,EXTYPE,SBIT(12),
     5                IFRMAT(32),IIFRMT(2),IAFRMT(2)
      CHARACTER       UFM*23,UWM*25,UIM*29
      COMMON /XMSSG / UFM,UWM,UIM
      COMMON /SYSTEM/ SYSBUF,NOUT,JUNK(6),NLPP,MTEMP,NPAGE,LINE
      COMMON /TWO   / TWO(32)
      COMMON /BITPOS/ UM,UO,UR,USG,USB,UL,UA,UF,US,UN,UG
      COMMON /ZZZZZZ/ Z(1)
      COMMON /BLANK / LUSET,MPCF1,MPCF2,SINGLE,OMIT1,REACT,NSKIP,
     1                REPEAT,NOSETS,NOL,NOA,IDSUB,IAUTSP
      DATA    SCR1  / 301/  , EQEXIN / 103   /, TDB204 / 204 /
      DATA    NAME  / 4HGP4P, 4HRT   , 4H    /
      DATA    IBEGN , IEND  / 4HBEGN , 4HEND /
      DATA    TITLE / 4H    , 4H MPC ,
     2                4H    , 4H SPC ,
     3                4H    , 4HOMIT ,
     4                4HANAL, 4HYSIS ,
     5                4H  SU, 4HPORT ,
     6                4HPERM, 4H SPC ,
     7                4HBDRY, 4H SPC ,
     8                4HAUTO, 4H SPC /
      DATA    BLANK / 1H  /
      DATA    DASH  / 1H- /
      DATA    IFRMAT/ 4H(13X, 4H,I6,, 4H3X,I, 4H8,1X, 4H,A1,, 4HI2,1,
     1                4HX   , 4H,1X,, 4H  I6, 4H,1X,, 4H  I6, 4H,1X,,
     2                4H  I6, 4H,1X,, 4H  I6, 4H,1X,, 4H  I6, 4H,1X,,
     3                4H  I6, 4H,1X,, 4H  I6, 4H,1X,, 4H  I6, 4H,1X,,
     X                4H  I6, 4H,1X,,
     4                4H  I6, 4H,1X,, 4H  I6, 4H,1X,, 4H  I6, 4H)   /
      DATA    IIFRMT/ 4H,1X,, 4H  I6/
      DATA    IAFRMT/ 4H,3X,, 4H  A4/
      DATA    IPRINT/ 0   /
C
C
      CALL SSWTCH (21,D21)
      CALL SSWTCH (22,D22)
      IF (D21.NE.1 .AND. D22.NE.1 .AND. IDSUB.LE.0) RETURN
      IF (IPRINT .EQ. 1) RETURN
      IPRINT  = 1
      NAME(3) = IBEGN
      CALL CONMSG (NAME,3,0)
      BUF  = IABS(IBUF)
      FILE = EQEXIN
      IF (IBUF .LT. 0) CALL CLOSE (EQEXIN,1)
      CALL OPEN (*1220,EQEXIN,Z(BUF),0)
      CALL FWDREC (*1230,EQEXIN)
      CALL FWDREC (*1230,EQEXIN)
      EREC1 = LUSET +1
      CALL READ (*1230,*2900,EQEXIN,Z(EREC1),BUF-EREC1,1,KN)
      GO TO 9001
 2900 CALL CLOSE (EQEXIN,1)
      CALL SORT (0,0,2,2,Z(EREC1),KN)
C
      IF (D21 .NE. 1) GO TO 3000
      KU = 1
      MSK(KU+1 ) = TWO(USB)
      MSK(KU+2 ) = TWO(USG)
      MSK(KU+3 ) = TWO( UL)
      MSK(KU+4 ) = TWO( UA)
      MSK(KU+5 ) = TWO( UF)
      MSK(KU+6 ) = TWO( UN)
      MSK(KU+7 ) = TWO( UG)
      MSK(KU+8 ) = TWO( UR)
      MSK(KU+9 ) = TWO( UO)
      MSK(KU+10) = TWO( US)
      MSK(KU+11) = TWO( UM)
      DO 2910 KU = 1,12
      SBIT(KU) = 0
 2910 CONTINUE
      CALL PAGE1
      LINE = LINE + 2
      WRITE (NOUT,1900) UIM
      LINE = LINE + 4
      WRITE (NOUT,1902)
      I  = EREC1
      KL = 0
      DO 2960 K = 1,KN,2
      ITM = Z(K+I)/10
      ITM = Z(K+I) - 10*ITM
      L   = 6
      IF (ITM .EQ. 2) L = 1
      DO 2950 KK = 1,L
      KL = KL + 1
      IU = Z(KL)
      IP = Z(I+K-1)
      IDOF = KK
      IF (ANDF(MSK(11),IU) .EQ. 0) GO TO 2914
      IF (ANDF(MSK(2),IU).NE.0 .OR. ANDF(MSK(3),IU).NE.0) GO TO 2914
      SBIT(1)   = SBIT(1) + 1
      UPBIT(1)  = SBIT(1)
      IFRMAT(8) = IIFRMT(1)
      IFRMAT(9) = IIFRMT(2)
      GO TO 2916
 2914 UPBIT(1)  = BLANK
      IFRMAT(8) = IAFRMT(1)
      IFRMAT(9) = IAFRMT(2)
 2916 DO 2940 KU = 2,12
      INDEX = 2*(KU-1) + 8
      IF (ANDF(MSK(KU),IU) .EQ. MSK(KU)) GO TO 2920
      UPBIT(KU) = BLANK
      IFRMAT(INDEX  ) = IAFRMT(1)
      IFRMAT(INDEX+1) = IAFRMT(2)
      GO TO 2940
 2920 SBIT (KU) = SBIT(KU) + 1
      UPBIT(KU) = SBIT(KU)
      IFRMAT(INDEX  ) = IIFRMT(1)
      IFRMAT(INDEX+1) = IIFRMT(2)
 2940 CONTINUE
      IF (L .EQ. 1) IDOF = 0
      LINE = LINE + 1
      IF (LINE .LE. NLPP) GO TO 2945
      CALL PAGE1
      WRITE (NOUT,1902)
      LINE = LINE + 5
 2945 WRITE (NOUT,IFRMAT) KL,IP,DASH,IDOF,UPBIT
 2950 CONTINUE
 2960 CONTINUE
      WRITE (NOUT,1901) SBIT
      LINE = LINE + 2
C
 3000 IF (D22.NE.1 .AND. IDSUB.LE.0) RETURN
      MSK(1)  = TWO(UM )
      MSK(2)  = TWO(US )
      MSK(3)  = TWO(UO )
      MSK(4)  = TWO(UA )
      MSK(5)  = TWO(UR )
      MSK(6)  = TWO(USG)
      MSK(7)  = TWO(USB)
      EXFLAG  = 0
      EXTYPE  = 0
      IF (D22 .NE. 1) GO TO 3010
      CALL PAGE1
      LINE = LINE + 2
      WRITE (NOUT,1907) UIM
      LINE = LINE + 4
 3010 FILE = SCR1
      IF (IBUF .LT. 0) CALL CLOSE (SCR1,1)
      CALL OPEN (*1220,SCR1,Z(BUF),1)
      DO 4000 IMK = 1,8
      IFLG(IMK) = 0
      I  = EREC1
      IP = 0
      KL = 0
      DO 3960 K = 1,KN,2
      ITM = Z(K+I)/10
      ITM = Z(K+I) - 10*ITM
      L   = 6
      IF (ITM .EQ. 2) L = 1
      DO 3950 KK = 1,L
      KL = KL + 1
      IU = Z(KL)
      IF (Z(I+K-1) .LT. IP) EXFLAG = 1
      IP = Z(I+K-1)
      IF (L .EQ. 1) GO TO 3920
      IDOF = KK
      EXTYPE = ORF(EXTYPE,2)
      GO TO 3930
 3920 IDOF = 0
      EXTYPE = ORF(EXTYPE,1)
 3930 IF (IMK .NE. 8) GO TO 3940
      IF (ANDF(IU,MSK(2)) .EQ. 0) GO TO 3950
      IF (ANDF(IU,MSK(6)).NE.0 .OR. ANDF(IU,MSK(7)).NE.0) GO TO 3950
      GO TO 3945
 3940 IF (ANDF(IU,MSK(IMK)) .NE. MSK(IMK)) GO TO 3950
 3945 CALL WRITE (SCR1,10*IP+IDOF,1,0)
      IFLG(IMK) = 1
 3950 CONTINUE
 3960 CONTINUE
      IF (IFLG(IMK) .NE. 1) GO TO 4000
      CALL WRITE (SCR1,0,0,1)
 4000 CONTINUE
      CALL WRITE (SCR1,Z(1),LUSET,1)
      CALL CLOSE (SCR1,1)
      CALL OPEN  (*1220,SCR1,Z(BUF),0)
      IFLAG = 0
      DO 4500 I = 1,8
      IF (IFLG(I) .NE. 1) GO TO 4500
      IFLAG = IFLAG + 1
      CALL READ (*1230,*4010,SCR1,Z(1),BUF,1,KN)
      CALL PAGE2 (-4)
      WRITE (NOUT,9501) FILE
      GO TO 4600
 4010 CONTINUE
      IF (IDSUB.LE.0 .OR. I.NE.4) GO TO 4040
      CALL CLOSE (SCR1,2)
      FILE = TDB204
      CALL OPEN  (*1220,TDB204,Z(BUF),1)
      CALL FNAME (TDB204,NAM204)
      CALL WRITE (TDB204,NAM204,2,1)
      CALL WRITE (TDB204,Z(1),KN,1)
      CALL CLOSE (TDB204,1)
      TRL(1) = TDB204
      TRL(2) = 0
      TRL(3) = KN
      TRL(4) = 0
      TRL(5) = IDSUB
      TRL(6) = EXFLAG
      TRL(7) = EXTYPE
      CALL WRTTRL (TRL)
      CALL OPEN (*1220,SCR1,Z(BUF),2)
 4040 CONTINUE
      IF (D22 .NE. 1) GO TO 4500
      IPAS = KN/10
      IREM = KN - 10*IPAS
      IF (IFLAG .GT. 1) LINE = NLPP
      ID1  =-9
      INOS = 0
      IF (IPAS .LT. 1) GO TO 4105
      DO 4100 K = 1,IPAS
      DO 4050 J = 1,10
      INOS = INOS + 1
      ZDUM(J) = Z(INOS)/10
      ZCOM(J) = Z(INOS) - 10*ZDUM(J)
 4050 CONTINUE
      LINE = LINE + 1
      IF (IFLAG.EQ.1 .AND. K.EQ.1) GO TO 4060
      IF (LINE .LE. NLPP) GO TO 4090
      CALL PAGE1
 4060 WRITE (NOUT,1910) TITLE(1,I),TITLE(2,I)
      LINE = LINE + 5
 4090 CONTINUE
      ID1 = ID1 + 10
      WRITE (NOUT,1913) ID1,(ZDUM(KK),ZCOM(KK),KK=1,10)
 4100 CONTINUE
 4105 IF (IREM .EQ. 0) GO TO 4500
      DO 4110 J = 1,IREM
      INOS = INOS + 1
      ZDUM(J) = Z(INOS)/10
      ZCOM(J) = Z(INOS) - ZDUM(J)*10
 4110 CONTINUE
      LINE = LINE + 1
      IF (IFLAG.EQ.1 .AND. IPAS.EQ.0) GO TO 4120
      IF (LINE .LE. NLPP) GO TO 4400
      CALL PAGE1
 4120 WRITE (NOUT,1910) TITLE(1,I),TITLE(2,I)
      LINE = LINE + 5
 4400 CONTINUE
      ID1 = ID1 + 10
      WRITE (NOUT,1913) ID1,(ZDUM(KK),ZCOM(KK),KK=1,IREM)
 4500 CONTINUE
C
C     RE-ESTABLISH USET IN OPEN CORE.
C
 4600 CALL READ (*1230,*9001,SCR1,Z(1),LUSET,1,KN)
      CALL CLOSE (SCR1,1)
      NAME(3) = IEND
      CALL CONMSG (NAME,3,0)
C
C     TERMINATE RUN IF DIAG 21 OR 22, AND DIAG 20 ARE REQUESTED BY UESER
C     SIMLUTANEOUSLY
C
      CALL SSWTCH (20,J)
      IF (J.EQ.0 .OR. D21+D22.EQ.0) RETURN
      WRITE  (NOUT,4700)
 4700 FORMAT (10X,25HJOB TERMINATED BY DIAG 20)
      CALL PEXIT
C
 1220 J = -1
      GO TO 1260
 1230 J = -2
 1260 CALL MESAGE (J,FILE,NAME)
      RETURN
C
 1900 FORMAT (A29,' 2118, SUBROUTINE GP4PRT - DIAG 21 SET-DOF VS. DISP',
     1       ' SETS FOLLOWS.')
 1901 FORMAT (1H0, 34H--- C O L U M N   T O T A L S --- , 12I7)
 1902 FORMAT (1H0,14X,5H(SIL), /14X,
     1        48HINT DOF   EXT GP. DOF  SAUTO     SB     SG      ,
     2        49HL      A      F      N      G      R      O      ,
     3         8HS      M, /1H , 131(1H-))
 1907 FORMAT (A29,' 2119, SUBROUTINE GP4PRT - DIAG 22 SET DISP SETS VS',
     1       '. DOF FOLLOWS')
 1910 FORMAT (1H0,52X,2A4,17H DISPLACEMENT SET ,/
     1        1H0,15X,3H-1-,8X,3H-2-,8X,3H-3-,8X,3H-4-,8X,3H-5-,
     2             8X,3H-6-,8X,3H-7-,8X,3H-8-,8X,3H-9-,7X,4H-10- ,/1H )
 1913 FORMAT (1H ,I6,1H=,10(1X,I8,1H-,I1))
C
C     ERRORS
C
 9001 CALL PAGE2(-4)
      WRITE  (NOUT,9501) UWM,FILE
 9501 FORMAT (A25,' 2110, INSUFFICIENT CORE TO HOLD CONTENTS OF GINO ',
     1       'FILE',I4, //5X,
     2       'FURTHER PROCESSING OF THIS DATA BLOCK IS ABANDONED.')
C
      CALL CLOSE (FILE,1)
      RETURN
C
      END
