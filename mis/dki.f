      DOUBLE PRECISION FUNCTION DKI(I,J,K,L,M,N,IP,IQ,R,Z)
      DOUBLE PRECISION AI, R, Z, RD, ABS1, AMKL, AKKL, AMMN, AKMN, ARR
      DOUBLE PRECISION DKINT, DK89, DK100, DK211
      DOUBLE PRECISION XX, AMM
      DIMENSION  R(1) , Z(1)
      IF (R(I) .EQ. R(J)) GO TO 20
      RD = R(J)
      IF (R(J) .EQ. 0.0D0) RD = R(I)
      ABS1 =  DABS( (R(I) - R(J)) / RD )
      IF (ABS1 .LE. 0.1D-3) GO TO 20
      AMKL = (R(L)*Z(K)-R(K)*Z(L)) / (R(L)-R(K))
      AKKL = (Z(L)-Z(K)) / (R(L)-R(K))
      AMMN = (R(N)*Z(M)-R(M)*Z(N)) / (R(N)-R(M))
      AKMN = (Z(N)-Z(M)) / (R(N)-R(M))
      IF (AKMN .NE. AKKL .OR. AMMN .NE. AMKL) GO TO 30
   20 AI = 0.0D0
      GO TO  510
   30 CONTINUE
      ISS = IABS(IP)
      IRR = IABS(IQ)
      IF (IQ + 1) 100,300,50
   50 CONTINUE
      MM = IP
      NN = IQ + 1
      AI =DKINT(I,J,AMMN,AKMN,MM,NN,R,Z) -DKINT(I,J,AMKL,AKKL,MM,NN,R,Z)
      GO TO  510
  100 CONTINUE
      IF (IP .LT. 0) GO TO 200
      MM = IP
      NN = IRR - 1
      AI =DK89(I,AMKL,AKKL,MM,NN,R)  -  DK89(I,AMMN,AKMN,MM,NN,R)
     1   -DK89(J,AMKL,AKKL,MM,NN,R)  +  DK89(J,AMMN,AKMN,MM,NN,R)
      ARR = IRR
      AI = (1.0D0 / (1.0D0 - ARR)) * AI
      GO TO  510
  200 CONTINUE
      MM = ISS
      NN = IRR - 1
      AI =DK100(I,AMKL,AKKL,MM,NN,R) -DK100(I,AMMN,AKMN,MM,NN,R)
     1   -DK100(J,AMKL,AKKL,MM,NN,R) +DK100(J,AMMN,AKMN,MM,NN,R)
      ARR = IRR
      AI = (1.0D0 / (1.0D0 - ARR)) * AI
      GO TO  510
  300 CONTINUE
      IF (IP + 1) 400,500,301
  301 CONTINUE
      MM = IP + 1
      AMM=MM
      XX=R(I)**MM/AMM
      AI=   (
     *  +XX*DLOG(DABS(AMKL+AKKL*R(I)))-AKKL/AMM*DK89(I,AMKL,AKKL,MM,1,R)
     *  -XX*DLOG(DABS(AMMN+AKMN*R(I)))+AKMN/AMM*DK89(I,AMMN,AKMN,MM,1,R)
     *      )
      XX=R(J)**MM/AMM
      AI=   (
     *  -XX*DLOG(DABS(AMKL+AKKL*R(J)))+AKKL/AMM*DK89(J,AMKL,AKKL,MM,1,R)
     *  +XX*DLOG(DABS(AMMN+AKMN*R(J)))-AKMN/AMM*DK89(J,AMMN,AKMN,MM,1,R)
     *      ) + AI
      GO TO  510
  400 CONTINUE
      MM = ISS - 1
      AMM=MM
      XX=AMM*R(I)**MM
      AI=   (
     * -DLOG(DABS(AMKL+AKKL*R(I)))/XX+AKKL/AMM*DK100(I,AMKL,AKKL,MM,1,R)
     * +DLOG(DABS(AMMN+AKMN*R(I)))/XX-AKMN/AMM*DK100(I,AMMN,AKMN,MM,1,R)
     *      )
      XX=AMM*R(J)**M
      AI=   (
     * +DLOG(DABS(AMKL+AKKL*R(J)))/XX-AKKL/AMM*DK100(J,AMKL,AKKL,MM,1,R)
     * -DLOG(DABS(AMMN+AKMN*R(J)))/XX+AKMN/AMM*DK100(J,AMMN,AKMN,MM,1,R)
     *      ) + AI
      GO TO  510
  500 CONTINUE
      AI = DK211(I,AMKL,AKKL,R) - DK211(I,AMMN,AKMN,R)
     1   - DK211(J,AMKL,AKKL,R) + DK211(J,AMMN,AKMN,R)
  510 CONTINUE
      DKI = AI
      RETURN
      END
