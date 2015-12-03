      SUBROUTINE RBMG3
C*****
C     SOLVE KLL * DM = -KLR  FOR DM (WHERE LLL IS THE TRI FACTOR)
C     THEN COMPUTE X = KRR + KLR(T) * DM
C     AND ESP = NORM(X) / NORM(KRR)
C*****
      INTEGER       DM    ,SCR1  ,SCR2
C
      DATA    LLL         ,KLR   ,KRR   /101   ,102   ,103   /
     1       ,DM                        /201/
     2       ,SCR1 ,SCR2                /301   ,302   /
C*****
      CALL SOLVER(LLL,DM,KLR,KRR,SCR1,EPS,1,SCR2)
      CALL MESAGE(35,0,EPS)
      RETURN
C
      END
