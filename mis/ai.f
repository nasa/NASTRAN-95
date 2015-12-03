      FUNCTION AI (I,J,K,L,M,N,IP,IQ,R,Z)
C
      DIMENSION  R(1),Z(1)
C
      IF (R(I) .EQ. R(J)) GO TO 20
      RD   = R(J)
      IF (R(J) .EQ. 0.0) RD = R(I)
      ABS1 = ABS((R(I)-R(J))/RD)
      IF (ABS1 .LE. .0001) GO TO 20
      AMKL = (R(L)*Z(K)-R(K)*Z(L))/(R(L)-R(K))
      AKKL = (Z(L)-Z(K))/(R(L)-R(K))
      AMMN = (R(N)*Z(M)-R(M)*Z(N))/(R(N)-R(M))
      AKMN = (Z(N)-Z(M))/(R(N)-R(M))
      IF (AKMN.NE.AKKL .OR. AMMN.NE.AMKL) GO TO 30
   20 AI  = 0.0
      GO TO 510
   30 CONTINUE
      ISS = IABS(IP)
      IRR = IABS(IQ)
      IF (IQ + 1) 100,300,50
   50 CONTINUE
      MM = IP
      NN = IQ + 1
      AI = BINT(I,J,AMMN,AKMN,MM,NN,R,Z) - BINT(I,J,AMKL,AKKL,MM,NN,R,Z)
      GO TO 510
  100 CONTINUE
      IF (IP .LT. 0) GO TO 200
      MM = IP
      NN = IRR - 1
      AI = F89(I,AMKL,AKKL,MM,NN,R) - F89(I,AMMN,AKMN,MM,NN,R)
     1   - F89(J,AMKL,AKKL,MM,NN,R) + F89(J,AMMN,AKMN,MM,NN,R)
      ARR= IRR
      AI = (1.0/(1.0 - ARR))*AI
      GO TO 510
  200 CONTINUE
      MM = ISS
      NN = IRR - 1
      AI = FF100(I,AMKL,AKKL,MM,NN,R) -FF100(I,AMMN,AKMN,MM,NN,R)
     1   - FF100(J,AMKL,AKKL,MM,NN,R) +FF100(J,AMMN,AKMN,MM,NN,R)
      ARR= IRR
      AI = (1.0/(1.0-ARR))*AI
      GO TO 510
  300 CONTINUE
      IF (IP + 1) 400,500,301
  301 CONTINUE
      MM = IP + 1
      AMM= MM
      XX = R(I)**MM/AMM
      AI = (
     1   +XX*ALOG(ABS(AMKL+AKKL*R(I)))-AKKL/AMM*F89(I,AMKL,AKKL,MM,1,R)
     2   -XX*ALOG(ABS(AMMN+AKMN*R(I)))+AKMN/AMM*F89(I,AMMN,AKMN,MM,1,R)
     3     )
      XX = R(J)**MM/AMM
      AI = (
     1   -XX*ALOG(ABS(AMKL+AKKL*R(J)))+AKKL/AMM*F89(J,AMKL,AKKL,MM,1,R)
     2   +XX*ALOG(ABS(AMMN+AKMN*R(J)))-AKMN/AMM*F89(J,AMMN,AKMN,MM,1,R)
     3     ) + AI
      GO TO 510
  400 CONTINUE
      MM = ISS - 1
      AMM= MM
      XX = AMM*R(I)**MM
      AI = (
     1  -ALOG(ABS(AMKL+AKKL*R(I)))/XX+AKKL/AMM*FF100(I,AMKL,AKKL,MM,1,R)
     2  +ALOG(ABS(AMMN+AKMN*R(I)))/XX-AKMN/AMM*FF100(I,AMMN,AKMN,MM,1,R)
     3     )
      XX = AMM*R(J)**M
      AI = (
     1  +ALOG(ABS(AMKL+AKKL*R(J)))/XX-AKKL/AMM*FF100(J,AMKL,AKKL,MM,1,R)
     2  -ALOG(ABS(AMMN+AKMN*R(J)))/XX+AKMN/AMM*FF100(J,AMMN,AKMN,MM,1,R)
     3     ) + AI
      GO TO 510
  500 CONTINUE
      AI = F6211(I,AMKL,AKKL,R) - F6211(I,AMMN,AKMN,R)
     1   - F6211(J,AMKL,AKKL,R) + F6211(J,AMMN,AKMN,R)
  510 CONTINUE
      RETURN
      END
