      SUBROUTINE FRMAX(IFK,IFM,N,IPR,RSN,RSM)
      DIMENSION          ZK(1)  ,ZM(1)
      DOUBLE PRECISION   RSN    ,RSM    ,RATIO  ,RATINV  ,
     1                   DZK(1) ,DZM(1)
      EQUIVALENCE       (DZK(1) ,ZK(1) ),(DZM(1) ,ZM(1) )
      COMMON  /UNPAKX/   IPRC   ,IP     ,NP     ,INCR
      IPRC = IPR
      INCR = 1
      RSN = 0.D0
      RSM = 0.D0
      DO 30 I = 1,N
      IP = I
      NP = I
      CALL UNPACK(*30,IFK,DZK(1))
      CALL UNPACK(*30,IFM,DZM(1))
      IF(IPR .EQ. 2) GO TO 10
      IF (ZK(1).EQ.0.OR.ZM(1).EQ.0) GO TO 30
      RATIO = ZK(1) / ZM(1)
      GO TO 20
   10 IF (DZK(1).EQ.0.0D0.OR.DZM(1).EQ.0.0D0) GO TO 30
      RATIO = DZK(1)/DZM(1)
   20 RATINV = 1.D0 /RATIO
      IF(RATIO .GT. RSM) RSM = RATIO
      IF(RATINV .GT. RSN) RSN = RATINV
   30 CONTINUE
      RSN = 1.D0 / RSN
      RETURN
      END
