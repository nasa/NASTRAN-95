      SUBROUTINE Q4NRMS (BGPDT,GPNORM,IORDER,IFLAG)
C     &    ENTRY Q4NRMD (BGPDT,GPNORM,IORDER,IFLAG)
C
C*****
C     COMPUTES UNIT NORMAL VECTORS FOR QUAD4 GRID POINTS.
C*****
      INTEGER          IORDER(4)
      REAL             BGPDT(4,4),GPNORM(4,4),
     1                 SHP(4), SSHP(4,2),  V(3,3), TSHP(4), TSSHP(4,2),
     2                 AXI(4),AETA(4),ETA,VMAG,XI
      DOUBLE PRECISION DSHP(4),DSSHP(4,2),DV(3,3),TDSHP(4),TDSSHP(4,2),
     1                 ADI(4),AETD(4),ETD,DMAG,DI
      DATA   AXI     / -1.0  ,  1.0  , 1.0  , -1.0   /
      DATA   AETA    / -1.0  , -1.0  , 1.0  ,  1.0   /
      DATA   ADI     / -1.0D0,  1.0D0, 1.0D0, -1.0D0 /
      DATA   AETD    / -1.0D0, -1.0D0, 1.0D0,  1.0D0 /
C
C     SINGLE PRECISION SECTION -
C*****
C     COMPUTE SHAPE FUNCTION DERIVATIVES
C*****
      IFLAG = 0
      DO 50 II=1,4
      IO  = IORDER(II)
      XI  = AXI(IO)
      ETA = AETA(IO)
      CALL Q4SHPS (XI,ETA,SHP,SSHP)
C*****
C     SORT THE SHAPE FUNCTIONS
C*****
      DO 10 I=1,4
      TSHP(I) = SHP(I)
      DO 10 J=1,2
   10 TSSHP(I,J) = SSHP(I,J)
C
      DO 20 IK=1,4
      I = IORDER(IK)
      SHP(IK) = TSHP(I)
      DO 20 J=1,2
   20 SSHP(IK,J) = TSSHP(I,J)
C*****
C     COMPUTE VECTOR
C*****
      DO 30 I=1,2
      DO 30 J=1,3
      V(I,J) = 0.0
      J1 = J + 1
      DO 30 K=1,4
   30 V(I,J) = V(I,J) + SSHP(K,I)*BGPDT(J1,K)
C
      V(3,1) = V(1,2)*V(2,3) - V(2,2)*V(1,3)
      V(3,2) = V(1,3)*V(2,1) - V(2,3)*V(1,1)
      V(3,3) = V(1,1)*V(2,2) - V(2,1)*V(1,2)
      VMAG   = V(3,1)**2+V(3,2)**2+V(3,3)**2
C
      IF (VMAG .GT. 1.0E-11) GO TO 40
      IFLAG = 1
      GO TO 200
C
   40 VMAG =  SQRT(VMAG)
      GPNORM(2,II) = V(3,1)/VMAG
      GPNORM(3,II) = V(3,2)/VMAG
      GPNORM(4,II) = V(3,3)/VMAG
   50 CONTINUE
      GO TO 200
C
      ENTRY Q4NRMD (BGPDT,GPNORM,IORDER,IFLAG)
C     =======================================
C
C     DOUBLE PRECISION SECTION -
C
C*****
C     COMPUTE SHAPE FUNCTION DERIVATIVES
C*****
      IFLAG = 0
      DO 150 II=1,4
      IO = IORDER(II)
      DI = ADI(IO)
      ETD = AETD(IO)
      CALL Q4SHPD (DI,ETD,DSHP,DSSHP)
C
C     SORT THE SHAPE FUNCTIONS
C
      DO 110 I=1,4
      TDSHP(I) = DSHP(I)
      DO 110 J=1,2
  110 TDSSHP(I,J) = DSSHP(I,J)
C
      DO 120 IK=1,4
      I = IORDER(IK)
      DSHP(IK) = TDSHP(I)
      DO 120 J=1,2
  120 DSSHP(IK,J) = TDSSHP(I,J)
C*****
C     COMPUTE VECTOR
C*****
      DO 130 I=1,2
      DO 130 J=1,3
      DV(I,J) = 0.0D0
      J1 = J + 1
      DO 130 K=1,4
  130 DV(I,J) = DV(I,J) + DSSHP(K,I)*BGPDT(J1,K)
C
      DV(3,1) = DV(1,2)*DV(2,3) - DV(2,2)*DV(1,3)
      DV(3,2) = DV(1,3)*DV(2,1) - DV(2,3)*DV(1,1)
      DV(3,3) = DV(1,1)*DV(2,2) - DV(2,1)*DV(1,2)
      DMAG    = DV(3,1)**2+DV(3,2)**2+DV(3,3)**2
C
      IF (DMAG .GT. 1.0D-11) GO TO 140
      IFLAG = 1
      GO TO 200
C
  140 DMAG = DSQRT(DMAG)
      GPNORM(2,II) = DV(3,1)/DMAG
      GPNORM(3,II) = DV(3,2)/DMAG
      GPNORM(4,II) = DV(3,3)/DMAG
  150 CONTINUE
C
  200 RETURN
      END
