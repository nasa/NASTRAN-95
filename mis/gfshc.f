      SUBROUTINE GFSHC(AWY,NUY,HC,IDENT,AC,MROW)
C
C     ROUTINE TO GENERATE CONSTRAINT MATRIX FOR PURELY INCOMPRESSIBLE
C     FORMULATION WHEN NO SPC'S ARE ON FLUID
C
      DOUBLE PRECISION        DZ(1)    ,DTERM    ,VAL
C
      REAL          RZ(1)
C
      INTEGER       Z        ,SYSBUF   ,MCB(7)   ,NAME(2)  ,HC
     1             ,TI1      ,TO1      ,TO2      ,AWY      ,SCR
     2             ,AC
C
C     OPEN CORE
C
      COMMON / ZZZZZZ /        Z(1)
C
C     SYSTEM COMMON
C
      COMMON / SYSTEM /       SYSBUF
C
C     PACK - UNPACK COMMON BLOCKS
C
      COMMON / PACKX /        TI1      ,TO1      ,I1       ,N1
     1                       ,INCR1
      COMMON / UNPAKX /       TO2      ,I2       ,N2       ,INCR2
      COMMON / ZBLPKX /       A(4)     ,IROW
C
      EQUIVALENCE   ( Z(1) , RZ(1) , DZ(1) )
     1             ,( VAL , A(1) )
C
      DATA NAME / 4HGFSH , 4HC    /
C
C
C     ALLOCATE CORE
C
      NZ = KORSZ(Z(1))
      IBUF = NZ - SYSBUF
      NZ = IBUF - 1
      IF(NZ .LT. NUY) GO TO 1008
C
C     FORM A COLUMN VECTOR OF ONES
C
      TI1 = 1
      TO1 = 2
      I1 = 1
      N1 = NUY
      INCR1 = 1
      DO 30 I=1,NUY
   30 RZ(I) = 1.0
      CALL MAKMCB(MCB,IDENT,NUY,2,2)
      CALL GOPEN(IDENT,Z(IBUF),1)
      CALL PACK(RZ(1),IDENT,MCB)
      CALL CLOSE(IDENT,1)
      CALL WRTTRL(MCB)
C
      CALL SSG2B(AWY,IDENT,0,AC,0,2,1,SCR)
C
C     PERFORM MULTIPLY TO GET COMPRESSIBLITY MATRIX
C
C
C     UNPACK ROW OF AC INTO CORE
C
      MCB(1) = AC
      CALL RDTRL(MCB)
      NROW = MCB(3)
      IF(NZ .LT. 2*NROW) GO TO 1008
      TO2 = 2
      I2 = 1
      N2 = NROW
      INCR2 = 1
C
      CALL GOPEN(AC,Z(IBUF),0)
      CALL UNPACK(*40,AC,DZ(1))
      GO TO 60
C
C     AC IS NULL
C
   40 DO 50 I=1,NROW
   50 DZ(I) = 0.0D0
C
   60 CALL CLOSE(AC,1)
C
C     LOCATE LARGEST TERM IN AC
C
      DTERM = -1.0D10
      DO 210 I=1,NROW
      IF(DZ(I) .LE. DTERM) GO TO 210
      MROW = I
      DTERM = DZ(I)
  210 CONTINUE
C
C     GENERATE THE HC MATRIX
C
      CALL MAKMCB(MCB,HC,NROW,1,2)
      CALL GOPEN(HC,Z(IBUF),1)
C
C     GENERATE COLUMNS UP TO MROW
C
      IF(MROW .EQ. 1) GO TO 230
      MR = MROW - 1
      DO 220 IR = 1,MR
      CALL BLDPK(2,2,HC,0,0)
      IROW = IR
      VAL = 1.0D0
      CALL ZBLPKI
      IROW = MROW
      VAL = -DZ(IR) / DTERM
      CALL ZBLPKI
  220 CALL BLDPKN(HC,0,MCB)
C
C     PACK OUT NULL COLUMN FOR MROW
C
  230 CALL BLDPK(2,2,HC,0,0)
      CALL BLDPKN(HC,0,MCB)
C
C     GENERATE REMAINING ROWS
C
      IF(MROW .GE. NROW) GO TO 250
      MR = MROW + 1
      DO 240 IR=MR,NROW
      CALL BLDPK(2,2,HC,0,0)
      IROW = MROW
      VAL = -DZ(IR) / DTERM
      CALL ZBLPKI
      IROW = IR
      VAL = 1.0D0
      CALL ZBLPKI
  240 CALL BLDPKN(HC,0,MCB)
C
  250 CALL CLOSE(HC,1)
      CALL WRTTRL(MCB)
C
      RETURN
C
C     ERRORS
C
 1008 CALL MESAGE(-8,0,NAME)
      RETURN
      END
