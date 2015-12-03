      SUBROUTINE GFSCOM(AWY,NUY,KC,IDENT,AC,SCR)
C
C     ROUTINE TO COMPUTE THE FLUID COMPRESSIBILTY MATRIX
C
C     THIS MATRIX CONTAINS THE SPRING FACTOR WHICH COUPLES THE
C     STRUCTURE AND FREE SURFACE TO PREVENT VOLUME CHANGES
C
      DOUBLE PRECISION        DZ(1)    ,DKCOMP   ,VAL
C
      REAL          KCOMP    ,RZ(1)
C
      INTEGER       KC       ,AC       ,Z        ,SYSBUF   ,AWY
     1             ,MCB(7)   ,NAME(2)  ,TI1      ,TO1      ,TO2
     2             ,SCR
C
C
C     MODULE PARAMETERS
C
      COMMON /BLANK/     NOGRAV   ,NOFREE   ,KCOMP   ,COMPTP
     1                       ,FORM     ,LMODES
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
      DATA NAME / 4HGFSC , 4HOM   /
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
C     SET UP TO CREATE KC MATRIX
C
   60 CALL CLOSE(AC,1)
C
      DKCOMP = DBLE(KCOMP)
      CALL GOPEN(KC,Z(IBUF),1)
      CALL MAKMCB(MCB,KC,NROW,1,2)
C
C     LOOP OVER NON-ZERO TERMS OF AC TO CREATE KC
C
      DO 90 I=1,NROW
      CALL BLDPK(2,2,KC,0,0)
      IF(DZ(I) .EQ. 0.0D0) GO TO 80
      DO 70 J=1,NROW
      IROW = J
      VAL = DKCOMP * DZ(J) * DZ(I)
      CALL ZBLPKI
   70 CONTINUE
   80 CALL BLDPKN(KC,0,MCB)
   90 CONTINUE
      CALL CLOSE(KC,1)
C
C     WRITE TRAILER
C
      CALL WRTTRL(MCB)
      RETURN
C
C     ERRORS
C
 1008 CALL MESAGE(-8,0,NAME)
      RETURN
      END
