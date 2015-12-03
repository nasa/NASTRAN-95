      SUBROUTINE EADD (P,PREC)
C
      INTEGER          IA(7)    ,IB(7)    ,IC(7)     ,PREC
      DOUBLE PRECISION BETA(2)  ,P(1)     ,ALPHA(2)
      COMMON /REGEAN/  IM(7)    ,IK(7)    ,IEV(7)    ,KA(5)    ,LC    ,
     1                 NN(13)   ,IBUCK
      COMMON /BLANK /  XX
      COMMON /SADDX /  NOMAT    ,NZ       ,MCBS(67)
      COMMON /ZZZZZZ/  CORE(1)
      EQUIVALENCE      (MCBS( 1),IA(1))   ,(MCBS( 8),IALP)     ,
     1                 (MCBS( 9),ALPHA(1)),(MCBS(13),IB(1))    ,
     2                 (MCBS(20),IBETA)   ,(MCBS(21),BETA(1))  ,
     3                 (MCBS(61),IC(1))
C
      NZ = (KORSZ(CORE)/2)*2 - LC
      DO 10 I = 1,7
      IA(I) = IM(I)
      IB(I) = IK(I)
   10 IC(I) = IK(I)
      IC(1) = KA(1)
      KPREC = IK(5)
      IF (PREC.GE.1 .AND. PREC.LE.4) KPREC = PREC
      IALP = KPREC
      ALPHA(1) = P(1)
      IBETA  = KPREC
      BETA(1)= 1.0D0
      NOMAT  = 2
      CALL SADD (CORE,CORE)
      CALL WRTTRL (IC)
      RETURN
      END
