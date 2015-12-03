      SUBROUTINE SOLVE1(A1,R1,RP,XI,LAM2,LAM3,LAM4,CONT)
C
C     ROUTINE TO SOLVE FOR LAMBDAS AS FNCTS. OF XI
C
C
      REAL LAM2,LAM3,LAM4
C
      IF (RP.EQ.0.0) GO TO 20
C
      SUM = A1 + XI / RP
      SINSUM = SIN(SUM)
      BB = R1 - RP * (SIN(A1) - SINSUM)
      RT = 0.0E0
      IF( SINSUM .NE. 0.0E0 ) RT = BB / SINSUM
      PSI1 = COS(SUM)
      PSI2 = -SINSUM / RP
C
C     CHECK FOR SHELL CAP CASE
      IF ( A1 .NE. 0.0 )  GO TO 40
      LAM2  = 0.0E0
      IF( BB .NE. 0.0E0 ) LAM2  = PSI1 / BB
      LAM3  =  1.0 / RP
      LAM4  = -1.0 / RP**2
      GO TO 50
C
C     ALF1 = ALF2
C
   20 SINA = SIN(A1)
      COSA = COS(A1)
      BB = R1 + XI * COSA
      RT = 0.0E0
      IF( SINA .NE. 0.0E0 ) RT = BB / SINA
      PSI1 = COSA
      PSI2 = 0.0
C
   40 LAM2 = 0.0E0
      IF( BB .NE. 0.0E0 ) LAM2 = PSI1 / BB
      LAM3 = 0.0E0
      IF( RT .NE. 0.0E0 ) LAM3 = 1.0E0 / RT
      LAM4 = 0.0E0
      IF( BB .NE. 0.0E0 ) LAM4 = PSI2 / BB
C
   50 CONTINUE
      RETURN
      END
