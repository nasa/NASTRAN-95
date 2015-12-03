      SUBROUTINE STPPHI(CA,BLOC,PM,NS)
C     PHI-FUNCTIONS FOR EACH STRIP (NACA TM 991, PG 19).
C     THE FOLLOWING FUNCTIONS ARE NOT COMPUTED, THEY ARE LEFT ZEROED
C         - NUMBERS  20, 22-30, 33, 34
      DIMENSION CA(1),BLOC(1),PM(37,NS)
      DIMENSION P(37)
      PI=3.141593
      DO 100 N=1,NS
      DO 10 I=1,37
   10 P(I)=0.0
      CT=CA(N)/BLOC(N)
      IF(CT.LE.1.0E-03) GO TO 50
      C=CT-1.0
      C2=C*C
      S2=1.0-C2
      S =SQRT(S2)
      X=ATAN2(S,C)
C     WATCH THIS TRIG
      PMX=PI-X
      P(1)  = PMX + S
      P(2)  = PMX*(1.+2.*C) + S*(2.+C)
      P(3)  = PMX + S*C
      P(4)  = PMX*2.*C  + S*2.*(2.+C2)/3.
      P(5)  = S*(1.-C)
      P(6)  = 2.*PMX + S*2.*(2.-C)*(1.+2.*C)/3.
      P(7)  = PMX*(0.5+2.*C) + S*(8.+5.*C+4.*C2-2.*C2*C)/6.
      P(8)  = PMX*(-1.+2.*C) + S*(2.-C)
      P(9)  = PMX*(1.+2.*C) + S*(2.+3.*C+4.*C2)/3.
      P(11) = P(2)*P(3)
      P(12) = PMX*PMX*(0.5+4.*C2) + PMX*S*C*(7.+2.*C2) + S2*(2.+2.5*C2)
      P(13) = SIN(0.5*X)/COS(0.5*X)
      P(14) = 2.*S
      P(15) = P(13)-P(14)
      P(16) = P(1)*P(14)
      P(17) = P(3)**2 +S2*S2
      P(18) = -P(13)*(PMX*(1.+2.*C)-S*C)
      P(19) = P(3)*S
      P(21) = -2.*(C + ALOG(S2) )
      P(31) = PMX - S
      P(32) = PMX + S*(1.+2.*C)
      P(35) = 2.*S2
      P(36) = P(32)*P(3) + 2.*S2*S2
      P(37) = P(3)*( P(2) - P(3) )
      P(10) = P(31)*P(5)
   50 DO 60 I=1,37
   60 PM(I,N)= P(I)
  100 CONTINUE
      RETURN
      END
