      SUBROUTINE ALG1 (LNCT)
C
      DIMENSION       RDATA(4)
      COMMON /GAS   / G,EJ,R,CP,GAMMA,ROJCP
      COMMON /SYSTEM/ SYSBUF,NOUT
      COMMON /ALGINO/ DUM,NALGDB
      COMMON /UD3PRT/ IPRTC
C
      LOG1 = NALGDB
      LOG2 = NOUT
      CALL FREAD (LOG1,RDATA,4,1)
      CP   = RDATA(1)
      R    = RDATA(2)
      G    = RDATA(3)
      EJ   = RDATA(4)
      IF (CP .EQ. 0.0) CP = 0.24
      IF (R  .EQ. 0.0) R  = 53.32
      IF (G  .EQ. 0.0) G  = 32.174
      IF (EJ .EQ. 0.0) EJ = 778.16
      IF (IPRTC .EQ. 1) WRITE(LOG2,10) CP,R,G,EJ
  10  FORMAT (/10X,'SPECIFIC HEAT AT CONSTANT PRESSURE',5X,1H=,F8.5,
     1        /10X,'GAS CONSTANT',27X,1H=,F8.4,
     2        /10X,'GRAVITATIONAL CONSTANT',17X,1H=,F8.4,
     3        /10X,'JOULES EQUIVALENT',22X,1H=,F8.3)
      LNCT  = LNCT + 5
      ROJCP = R/(EJ*CP)
      GAMMA = 1.0/(1.0-ROJCP)
      RETURN
      END
