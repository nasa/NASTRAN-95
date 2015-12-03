      SUBROUTINE MFREE
C     THIS ROUTINE GENERATES MASS TERMS FOR THE INTERNALLY CREATED
C     ELEMENT WHICH DESCRIBES FREE SURFACE EFFECTS
C*****
C     THE ECPT DATA IS
C         NO.       DESCRIPTION
C         1         EL ID
C         2         SIL 1
C         3         SIL 2
C         4         GAMMA
C         5         N
C         6         0
C         7         R1
C         8         Z1
C         9         -
C         10        0
C         11        R2
C         12        Z2
C         13        -
      DOUBLE PRECISION   RP             ,RN
     1                  ,DR             ,CT
     2                  ,EM
C
      INTEGER    NECPT(100)
C
      COMMON /SMA2DP/    RP             ,RN
     1                  ,DR             ,CT
     2                  ,EM
      COMMON /SMA2IO/    IO(36)
C
      COMMON /SMA2CL/    DUM(2)        ,NPVT
C
      COMMON /SMA2ET/    ECPT(100)
      EQUIVALENCE   (NECPT(1),ECPT(1))
      IFILE = IO(11)
      IF (ECPT(4).EQ.0.0) GO TO 1100
      IF(NECPT(2).EQ. NECPT(3)) GO TO 500
      DR = ECPT(11) - ECPT(7)
      IF(NPVT .EQ.NECPT(2)) GO TO 20
      IF(NPVT .NE.NECPT(3)) GO TO 1000
C
      RP = ECPT(11)
      RN = ECPT(7)
      IP = NECPT(3)
      IN = NECPT(2)
C
      GO TO 50
C
   20 RP = ECPT(7)
      RN = ECPT(11)
      IP =NECPT(2)
      IN =NECPT(3)
   50 CT = (0.2617994D0/ECPT(4)) * DR
      IF( NECPT(5) .EQ. 0) CT = 2.0D0 * CT
      EM = CT * (3.0D0*RP +RN)
      CALL SMA2B (EM,IP,IP,IFILE,0.0D0)
      EM = CT * ( RP +RN)
      CALL SMA2B (EM,IN,IP,IFILE,0.0D0)
      GO TO 1100
C
C      CASE OF CENTER ELEMENT CONNECTED TO ONE POINT
C
  500 IF(NECPT(2).NE.NPVT) GO TO 1000
       CT = 1.5707963D0 / DBLE( ECPT(4) )
      RP = ECPT (7)
      IF( NECPT(5).LE. 0 ) GO TO 510
      RN =NECPT(5)
      CT  = CT/ (2.0D0*RN +2.0D0)
  510 EM = CT*RP**2
      IP = NPVT
      CALL SMA2B( EM,IP,IP,IFILE,0.0D0)
 1000 RETURN
 1100 RETURN
      END
