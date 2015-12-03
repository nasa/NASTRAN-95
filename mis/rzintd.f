      DOUBLE PRECISION FUNCTION RZINTD(IP,IQ,R,Z,NGRIDS)
      DOUBLE PRECISION R(4),Z(4),PT(3),H(3)
      DOUBLE PRECISION XINT,RRP,ZZQ,DRDXI,DZDXI,DRDETA,DZDETA,DETJ
      DOUBLE PRECISION RR,ZZ
      IF(NGRIDS.EQ.3)GO TO 200
      NPT=3
      PT(1)=-.7745966692D0
      PT(2)=0.D0
      PT(3)=-PT(1)
      H(1)=5.D0/9.D0
      H(2)=8.D0/9.D0
      H(3)=H(1)
      XINT=0.D0
      DO 100 III=1,NPT
      DO 100 JJJ=1,NPT
      RR=.25D0*((1.D0-PT(III))*(1.D0-PT(JJJ))*R(1)
     1         +(1.D0+PT(III))*(1.D0-PT(JJJ))*R(2)
     1         +(1.D0+PT(III))*(1.D0+PT(JJJ))*R(3)
     1         +(1.D0-PT(III))*(1.D0+PT(JJJ))*R(4))
      ZZ=.25D0*((1.D0-PT(III))*(1.D0-PT(JJJ))*Z(1)
     1         +(1.D0+PT(III))*(1.D0-PT(JJJ))*Z(2)
     1         +(1.D0+PT(III))*(1.D0+PT(JJJ))*Z(3)
     1         +(1.D0-PT(III))*(1.D0+PT(JJJ))*Z(4))
      RRP=RR**IP
      ZZQ=ZZ**IQ
      DRDXI=.25D0*((1.D0-PT(JJJ))*(R(2)-R(1))+(1.D0+PT(JJJ))*(R(3)-R(4))
     1)
      DZDXI=.25D0*((1.D0-PT(JJJ))*(Z(2)-Z(1))+(1.D0+PT(JJJ))*(Z(3)-Z(4))
     1)
      DRDETA=.25D0*((1.D0-PT(III))*(R(4)-R(1))+(1.D0+PT(III))*(R(3)-R(2)
     1))
      DZDETA=.25D0*((1.D0-PT(III))*(Z(4)-Z(1))+(1.D0+PT(III))*(Z(3)-Z(2)
     1))
      DETJ=DRDXI*DZDETA-DZDXI*DRDETA
      DETJ=DABS(DETJ)
      XINT=XINT+RRP*ZZQ*H(III)*H(JJJ)*DETJ
  100 CONTINUE
      RZINTD=XINT
  200 RETURN
      END
