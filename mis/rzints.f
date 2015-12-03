      FUNCTION RZINTS(IP,IQ,R,Z,NGRIDS)
      DIMENSION R(4),Z(4),PT(3),H(3)
      IF(NGRIDS.EQ.3)GO TO 200
      NPT=3
      PT(1)=-.7745966692
      PT(2)=0.
      PT(3)=-PT(1)
      H(1)=5./9.
      H(2)=8./9.
      H(3)=H(1)
      XINT=0.
      DO 100 III=1,NPT
      DO 100 JJJ=1,NPT
      RR=.25*((1.-PT(III))*(1.-PT(JJJ))*R(1)
     1       +(1.+PT(III))*(1.-PT(JJJ))*R(2)
     1       +(1.+PT(III))*(1.+PT(JJJ))*R(3)
     1       +(1.-PT(III))*(1.+PT(JJJ))*R(4))
      ZZ=.25*((1.-PT(III))*(1.-PT(JJJ))*Z(1)
     1       +(1.+PT(III))*(1.-PT(JJJ))*Z(2)
     1       +(1.+PT(III))*(1.+PT(JJJ))*Z(3)
     1       +(1.-PT(III))*(1.+PT(JJJ))*Z(4))
      RRP=RR**IP
      ZZQ=ZZ**IQ
      DRDXI=.25*((1.-PT(JJJ))*(R(2)-R(1))+(1.+PT(JJJ))*(R(3)-R(4)))
      DZDXI=.25*((1.-PT(JJJ))*(Z(2)-Z(1))+(1.+PT(JJJ))*(Z(3)-Z(4)))
      DRDETA=.25*((1.-PT(III))*(R(4)-R(1))+(1.+PT(III))*(R(3)-R(2)))
      DZDETA=.25*((1.-PT(III))*(Z(4)-Z(1))+(1.+PT(III))*(Z(3)-Z(2)))
      DETJ=DRDXI*DZDETA-DZDXI*DRDETA
      DETJ=ABS(DETJ)
      XINT=XINT+RRP*ZZQ*H(III)*H(JJJ)*DETJ
  100 CONTINUE
      RZINTS=XINT
  200 RETURN
      END
