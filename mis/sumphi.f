      COMPLEX FUNCTION SUMPHI(IXR,IYR,ND1,NDN,CAPPHI,DSS,N,M,ASYM)
C
C     FUNCTION TO COMPUTE SUM OF CAPPHI-DELTA SOURCE STENGTH PRODUCT
C
      LOGICAL ASYM
      DIMENSION ND1(1),NDN(1)
      COMPLEX  CAPPHI(1),DSS(N,M)
C
      SUMPHI  =  ( 0.0 , 0.0 )
      IF ( IXR .EQ. 0 )   RETURN
      DO   400   I = 1 , IXR
      IXS  =  I - 1
      IP  =  IXR - IXS
      LTOT  =  2 * IP + 1
      IPHI  =  ( IP * ( IP + 1 ) ) / 2
      IYS  =  IYR - IXR + IXS
      DO   300   L = 1 , LTOT
      IF ( ASYM .AND. IYS .EQ. 0 )   GO TO  200
      J  =  IABS ( IYS ) + 1
      IF ( .NOT. ( I .GE. (ND1(J)) .AND. I .LE. NDN(J) ) )   GO TO 200
      S  =  1.0
      IF ( ASYM .AND. IYS .LT. 0 )   S  =  -S
      IJPHI  =  IPHI + 1 + IABS ( IYR - IYS )
      SUMPHI  =  SUMPHI + S * CAPPHI(IJPHI) * DSS(I,J)
 200  IYS  =  IYS + 1
 300  CONTINUE
 400  CONTINUE
      RETURN
      END
