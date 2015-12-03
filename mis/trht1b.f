      SUBROUTINE TRHT1B(IOF,DELTA)
C
C
      DOUBLE PRECISION        QBLOCK(6),          BLOCK(2), BLK(2)
C
      INTEGER                 MCB(7),   NAME(2),  IQBLK(12),IBLOCK(11)
C
      COMMON /BLANK/    BETA,     TABS,     NORAD,    RADLIN
      COMMON /TRHTX /         IK(7),    IB(7),    ICR1,     ICR2,
     1                        ICR3,     ICR4,     ICR5,     ISYM,
     2                        ICR6,     ICR7
C
      EQUIVALENCE             ( IQBLK(1),         QBLOCK(1) )
      EQUIVALENCE             ( IQBLK(2),         IBLOCK(1) )
      EQUIVALENCE             ( QBLOCK(2),        BLOCK(1) )
      EQUIVALENCE             ( QBLOCK(5),        BLK(1) )
C
C ----------------------------------------------------------------------
C
      IBLOCK(1) =2
      BLOCK(1)= 1.0D0/DELTA
      BLOCK(2)= 0.0D0
      IBLOCK(7)= 2
      BLK(1) = BETA
      BLK(2) = 0.0D0
      CALL SSG2C(IB,IK,ICR6,1,IBLOCK)
      MCB(1)=ICR6
      CALL RDTRL(MCB(1))
      IF ( MCB(4) .EQ. 6) GO TO  10
      CALL FACTRU(*40,ICR6,ICR1,ICR2,ICR3,ICR4,ICR7)
      ISYM = 0
      GO TO  20
C
C     SYMMETRIC DECOMP
C
   10 CALL FACTOR( ICR6, ICR1, ICR2, ICR3, ICR4, ICR7 )
      ISYM =1
C
C     LLL  IS ON ICR1
C
C     FORM  A  MATRIX
C
   20 BLK(1) = -(1.0D0 - BETA)
      BLK(2) = 0.0
      CALL SSG2C(IB,IK,ICR6,1,IBLOCK)
   30 RETURN
   40 CALL MESAGE(-5,ICR6,NAME)
      GO TO 30
      END
