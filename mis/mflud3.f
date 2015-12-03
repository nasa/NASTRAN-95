      SUBROUTINE MFLUD3
C*****
C     THIS ROUTINE GENERATES THE PSUEDO   MASS    MATRIX TERMS
C     FOR THE TRIANGULAR FLUID ELEMENT
C*****
C     THE ECPT DATA IS THE FOLLOWING
C
C         FIELD         SYMBOL
C           1             ID
C           2             SIL1
C           3             SIL2
C           4             SIL3
C           5             RHO
C           6             BULK
C           7             N
C           8             CSF
C           9             R1
C           10            Z1
C           11            -
C           12            CSF
C           13            R2
C           14            Z2
C           15            -
C           16            CSF
C           17            R3
C           18            Z3
C           19            -
C           20            -
C****
      DOUBLE PRECISION        R         ,Z        ,ARE2
     1                        ,PIAB     ,EMASS
C*****
      INTEGER NECPT(100)
      COMMON/SMA2CL/ DUM(2),NPVT
      COMMON/SMA2IO/ DUM1(10),IFMGG
      COMMON  /SMA2ET/ ECPT(100)
      COMMON/SMA2DP/          R(3)      ,Z(3)     ,ARE2
     1                       ,PIAB      ,EMASS    ,JP
     2                       ,IR        ,JPVT     ,IGRID
      EQUIVALENCE        (ECPT(1),NECPT(1))
C*****
C*****
C
      IF(ECPT(6).EQ. 0.0) RETURN
C*****
C     STORE THE POINT LOCATIONS AND FIND THE PIVOT POINT
C*****
      JP =0
      DO 20 I=1,3
      IR =  9 + 4*(I-1)
      R(I)  = ECPT(IR)
      IF(ECPT(IR).LE.0.0) GO TO 1000
      Z(I)  = ECPT(IR+1)
      IF( NPVT .NE. NECPT(I+1)) GO TO 20
      JP = I
   20 CONTINUE
      IF( JP .EQ.0) GO TO 1000
      ARE2=DABS((R(2) -R(1))*(Z(3)-Z(1)) - (R(3)-R(1))*(Z(2)-Z(1)) )
      PIAB = 2.617994D-2 *ARE2 / DBLE( ECPT(6) )
      IF (NECPT(7) .EQ. 0) PIAB = PIAB*2.0D0
      JPVT = NPVT
      DO 50  I = 1,3
      IGRID = NECPT(I+1)
      EMASS = PIAB*( R(1)+R(2)+R(3) +R(JP) +R(I))
      IF (I .EQ. JP) EMASS = EMASS*2.0D0
      CALL SMA2B ( EMASS, IGRID,JPVT,IFMGG,0.0D0)
   50 CONTINUE
 1000 RETURN
      END
