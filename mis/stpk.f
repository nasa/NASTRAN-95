      SUBROUTINE STPK(EK,N,NSTOP,NOPEN,NSTED,TSR,PM,CR,CI,IM,J1)
C     COMPUTES K MATRIX FOR STRIP NUMBER N
C     EK= LOCAL REDUCED FREQUENCY
C     NSTOP  =2 FOR NO CONTROL SURFACE
C     NOPEN  =1 FOR OPEN GAP
C     TSR = GAP/SEMICHORD RATIO  (FOR CLOSED STAGE ONLY)
C     NSTED =1 FOR STEADY CASE
      DIMENSION P(37)   , PM(1)
      COMPLEX EKM,W,T,R,V1,V2,UNIT,CMP0
      COMPLEX W2
      COMMON /STRIPC/NNS,BREF,CLAM,FM,NCIRC,NNCIRC,EKR(1),
     *   DUM,       BB(4),BETA(4),EKM(4,4)
      DATA NHEK,NHTR,NHTI,NHSIZE /4HEK  ,4HTR  ,4HTI  ,4HSIZE/
      UNIT=CMPLX(1.0,0.0)
      CMP0=CMPLX(0.0,0.0)
      T=2.0*UNIT
      DO 10 I=1,37
   10 P(I)=PM(I)
      DO 15 I=1,4
      DO 15 J=1,4
   15 EKM(I,J)= CMP0
      A1=0.318310
      A2=0.101321
      IF(NSTED.NE.1) GO TO 50
C     STEADY CASE
      E1K = 1.E20
      EKM(1,2)=2.0
      IF(NSTOP.EQ.2) GO TO 100
      EKM(1,3)=A1*2.0*P(1)
      EKM(2,3)=A1*P(5)
      EKM(3,2)=A1*2.0*P(31)
      EKM(3,3)=A2*(2.0*P(1)*P(31) + P(35) )
      EKM(4,2)=A1*P(8)
      EKM(4,3)=A2*(P(1)*P(8) + P(10) )
      IF(NOPEN.EQ.1) GO TO 100
C     CLOSED STAGE
      EKM(1,4)=A1*2.0*P(13)
      EKM(2,4)=A1*P(15)
      TST=AMAX1(0.01  ,TSR)
      EKM(3,4)=A2*(2.0*P(13)*P(31) +2.0*ALOG(TST) +P(21))
      EKM(4,4)=A2*(P(13)*P(8) + P(18))
   50 IF(NSTED.EQ.1) GO TO 100
C
C     UNSTEADY CASE, EM(1,1)=(K SUB A)/EK**2, ETC.
      E1K  = 1./EK
      T=CMP0
      V1=CMP0
      V2=CMP0
      IF(EK.GT.1000.0) GO TO 71
      IF ( NCIRC.GT.0 ) GO TO 73
      CALL STPBS0(EK,1,BJ0,BY0)
      CALL STPBS1(EK,1,BJ1,BY1)
      DENOM=(BJ1+BY0)**2 + (BY1-BJ0)**2
      CR= (BJ1*(BJ1+BY0) + BY1*(BY1-BJ0) )/DENOM
      CI=-(BY1*BY0 + BJ1*BJ0)/DENOM
C     (CR + I*CI  = THEODORSEN FUNCTION)
      GO TO 72
C  NEXT 8 STATEMENTS ARE FOR GENERATION OF WAGNER FUNCTIONS
   73 CR = BB(1)
      CI = 0.0
      DO 40 NN = 2,NNCIRC
      BEOEK = BETA(NN)/EK
      FCR = BB(NN)/(1.0 + BEOEK*BEOEK)
      CR = CR + FCR
      CI = CI + FCR*BEOEK
   40 CONTINUE
72    T=2.0*CMPLX(CR,CI) - UNIT
      W=CMPLX(0.0,EK)
      V1=UNIT/W
      V2=V1*V1
   60 R=T+UNIT
      W2 = -W*W
      EKM(1,1)=-( R*V1 +1. )
      EKM(1,1) = EKM(1,1) * W2
      EKM(1,2)=-( R*(V2+V1) + V1 + 0.5 )
      EKM(1,2) = EKM(1,2) * W2
      EKM(2,1)=-( 0.5 )
      EKM(2,1) = EKM(2,1) * W2
      EKM(2,2)=-( V1 + 0.375 )
      EKM(2,2) = EKM(2,2) * W2
      IF(NSTOP.EQ.2) GO TO 100
      EKM(1,3)=-A1*( R*(V2*P(1)+0.5*V1*P(2)) + V1*P(3) + 0.5 *P(4) )
      EKM(1,3) = EKM(1,3) * W2
      EKM(2,3)=-A1*( V2*P(5) + 0.5*V1*P(6) + 0.25*P(7) )
      EKM(2,3) = EKM(2,3) * W2
      EKM(3,1)=-A1*( R*V1*P(31) + P(3) )
      EKM(3,1) = EKM(3,1) * W2
      EKM(3,2)=-A1*( R*(V2+V1)*P(31) + V1*P(32) + 0.25*P(6) )
      EKM(3,2) = EKM(3,2) * W2
      EKM(3,3)=-A2*( R*(V2*P(1)+0.5*V1*P(2))*P(31) +
     1                  V2*P(35) + V1*P(36) + 0.5*P(37) )
      EKM(3,3) = EKM(3,3) * W2
      EKM(4,1)=-A1*0.5*( R*V1*P(8) + P(4) )
      EKM(4,1) = EKM(4,1) * W2
      EKM(4,2)=-A1*0.5*( R*(V2+V1)*P(8) + V1*P(9) + 0.5*P(7) )
      EKM(4,2) = EKM(4,2) * W2
      EKM(4,3)=-A2*( R*(V2*P(1)+0.5*V1*P(2))*0.5*P(8) +
     1                  V2*P(10) + 0.5*V1*P(11) + 0.25*P(12) )
      EKM(4,3) = EKM(4,3) * W2
      IF(NOPEN.NE.1) GO TO 70
C     OPEN STAGE
      EKM(1,4)=-A1*( R*V1*P(1) + P(3) )
      EKM(1,4) = EKM(1,4) * W2
      EKM(2,4)=-A1*( V1*P(5) + 0.25*P(6) )
      EKM(2,4) = EKM(2,4) * W2
      EKM(3,4)=-A2*( R*V1*P(1)*P(31) + V1*P(35) + P(17) )
      EKM(3,4) = EKM(3,4) * W2
      EKM(4,4)=-A2*( R*0.5*V1*P(1)*P(8) + V1*P(10) + 0.5*P(37) )
      EKM(4,4) = EKM(4,4) * W2
      GO TO 100
   70 CONTINUE
C     CLOSED STAGE
      EKM(1,4)=-A1*( R*(V2*P(13)+ V1*P(1) ) + V1*P(14) + P(3) )
      EKM(1,4) = EKM(1,4) * W2
      EKM(2,4)=-A1*( V2*P(15) + 2.0*V1*P(5) + 0.25*P(6) )
      EKM(2,4) = EKM(2,4) * W2
      TST=AMAX1(0.01  ,TSR)
      EKM(3,4)=-A2*( R*(V2*P(13)+V1*P(1))*P(31) +
     1               V2*(2.0*ALOG(TST) + P(21)) + V1*P(16) + P(17) )
      EKM(3,4) = EKM(3,4) * W2
      EKM(4,4)=-A2*( R*(V2*P(13)+V1*P(1))*0.5*P(8) +
     1               V2*P(18) + V1*P(19) + 0.5*P(37) )
      EKM(4,4) = EKM(4,4) * W2
  100 CONTINUE
      CALL BUG(NHEK  ,100,EK,1)
      CALL BUG(NHTR  ,100,CR,1)
      CALL BUG(NHTI  ,100,CI,1)
      CALL BUG(NHSIZE,100,N,1)
      RETURN
71    CR = .5
      CI = 0.
      W = CMPLX(0.0,EK)
      GO TO 60
      END
