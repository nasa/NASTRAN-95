      SUBROUTINE DPDAA
C*****
C DPDAA PERFORMS A BINARY SEARCH IN EQDYN AND CONVERTS THE GRID NO
C AND COMPONENT CODE TO AN SIL VALUE.
C*****
C
      INTEGER       GPL   ,SIL   ,USET  ,USETD ,GPLD  ,SILD  ,DPOOL
     1             ,DLT   ,FRL   ,TFL   ,TRL   ,PSDL  ,EED   ,SCR1
     2             ,SCR2  ,SCR3  ,SCR4  ,BUF   ,BUF1  ,BUF2  ,BUF3
     3             ,BUF4  ,NGRID ,EQDYN ,EPOINT,SEQEP ,Z     ,LOADS
     5             ,PSD   ,DLOAD ,FREQ1 ,FREQ  ,TIC   ,TSTEP ,TF
     6             ,EIGR  ,EIGB  ,EIGC
C
      DIMENSION BUF(24)   ,EPOINT(2)    ,SEQEP(2)     ,MCB(7)
     1         ,NAM(2)    ,LOADS(32)    ,DLOAD(2)     ,FREQ1(2)
     2         ,FREQ(2)   ,ZZ(1)        ,BUFR(20)     ,NOLIN(21)
     3         ,TIC(2)    ,TSTEP(2)     ,TF(2)        ,PSD(2)
     4         ,MSG(3)    ,EIGR(2)      ,EIGB(2)      ,EIGC(2)
C
      COMMON/DPDCOM/DPOOL ,GPL   ,SIL   ,USET  ,GPLD  ,SILD  ,USETD
     1             ,DLT   ,FRL   ,NLFT  ,TFL   ,TRL   ,PSDL  ,EED
     2             ,SCR1  ,SCR2  ,SCR3  ,SCR4  ,BUF   ,BUF1  ,BUF2
     3             ,BUF3  ,BUF4  ,EPOINT,SEQEP ,L     ,KN    ,NEQDYN
     4             ,LOADS ,DLOAD ,FREQ1 ,FREQ  ,NOLIN ,NOGO
     5             ,MSG   ,TIC   ,TSTEP ,TF    ,PSD   ,EIGR  ,EIGB
     6             ,EIGC  ,MCB   ,NAM   ,EQDYN ,SDT   ,INEQ
C
      COMMON/ZZZZZZ/Z(1)
C
      EQUIVALENCE   (Z(1) ,ZZ(1)),(BUF(1),BUFR(1)),(MSG(2),NGRID)
C
C*****
C IF EQDYN IS NOT IN CORE, READ IT IN AND SET FLAG.
C*****
      IF(INEQ .NE. 0) GO TO 1
      CALL GOPEN(EQDYN,Z(BUF3),0)
      CALL FREAD(EQDYN,Z,NEQDYN+1,1)
      CALL CLOSE(EQDYN,1)
      INEQ= 1
C*****
C PERFORM SEARCH.
C*****
    1 KLO= 1
      KHI= KN
      NGRID= BUF(L)
    2 K= (KLO+KHI+1)/2
    3 IF(NGRID - Z(2*K-1)) 4,11,5
    4 KHI= K
      GO TO 6
    5 KLO= K
    6 IF(KHI-KLO-1) 10,7,2
    7 IF(K.EQ.KLO) GO TO 8
      K= KLO
      GO TO 9
    8 K= KHI
    9 KLO= KHI
      GO TO 3
   10 CALL MESAGE(30,MSG,MSG(2))
      NOGO= 1
   11 BUF(L)= Z(2*K)
      IF(BUF(L+1) .NE. 0) BUF(L)= BUF(L)+BUF(L+1)-1
      RETURN
      END
