      SUBROUTINE CFBSOR (LL,UL,BX,XX,IOPT)
C
      INTEGER         UL,BX,XX,MCB(7)
      COMMON /ZZZZZZ/ IZ(1)
      COMMON /GFBSX / JFL(7), JFU(7),JFB(7),JFX(7),JX,JPREC,JSIGN
      COMMON /FBSX  / MFL(7),MFLT(7),MFB(7),MFX(7),MX,MPREC,MSIGN
      COMMON /SYSTEM/ IDUM(54),IPREC
C
      NZ = KORSZ(IZ)
      MCB(1) = IABS(BX)
      CALL RDTRL (MCB)
      IF (IOPT .EQ. 1) GO TO 100
C
C     SYMETRIC FBS
C
      MFL(1) = LL
      CALL RDTRL (MFL)
      DO 10 I = 1,7
      MFB(I) = MCB(I)
      MFX(I) = MCB(I)
   10 CONTINUE
      MFX(1) = XX
      MX     = NZ
      MFX(5) = MAX0(MFL(5),MFB(5))
      MPREC  = IPREC
      MSIGN  = +1
      IF (BX .LT. 0) MSIGN = -1
      CALL FBS (IZ,IZ)
      CALL WRTTRL (MFX)
   20 RETURN
C
C     UNSYMETRIC FBS
C
  100 JFL(1) = LL
      CALL RDTRL (JFL)
      JFU(1) = UL
      CALL RDTRL (JFU)
      DO 110 I = 1,7
      JFB(I) = MCB(I)
      JFX(I) = MCB(I)
  110 CONTINUE
      JFX(1) = XX
      JX     = NZ
      JPREC  = IPREC
      JFX(5) = MAX0(JFL(5),JFB(5))
      JSIGN  = +1
      IF (BX .LT. 0) JSIGN = -1
      CALL GFBS (IZ,IZ)
      CALL WRTTRL (JFX)
      GO TO 20
      END
