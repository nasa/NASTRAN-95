      SUBROUTINE STPAIC(BLOC,DY,NSIZE,GAP,BM,GM,PM,NS,CLA,AJJL)
      COMPLEX CH,CDUM,EKM
      DIMENSION BLOC(1),DY(1),NSIZE(1),GAP(1)
      DIMENSION BM(4,4,NS),GM(4,3,NS),PM(37,NS)
      DIMENSION CLA(1)
      DIMENSION CH(3,3),CDUM(4,4)
      COMMON /STRIPC/NNS,BREF,CLAM,FM,NCIRC,NNCIRC,EKR(1),
     *   DUM,       BB(4),BETA(4),EKM(4,4)
      COMMON /AMGMN / MCB(7),NROW,ND,NE,REFC,FMACH,RFK,TSKJ(7),ISK,NSK
      COMMON /PACKX / ITI,ITO,II,NN,INCR
      K = 1
      II = NROW +1
      NN = NROW
      IF(EKR(1).LE..00001) EKR(1) = 0.0
      NSTED=0
      IF(EKR(K).EQ.0.0) NSTED=1
      DO 190 N=1,NS
      BOB=BLOC(N)/BREF
      EKL=EKR(K)*BOB
      CONST=  CLA(N)*DY(N)*CLAM
      CR = FM
      IF ( NCIRC.NE.0 ) CR = BB(1)
      CI = 0.
      NOPEN = 0
      IF(NSIZE(N).EQ.3.AND.GAP(N).EQ.0.0) NOPEN = 1
      TSR= 0.5*GAP(N)/BLOC(N)
      IM=NSIZE(N)
      IF(IM-3) 31,32,32
   31 JM=2
      J1=2
      GO TO 33
   32 JM=4
      J1=3
   33 CONTINUE
      CALL STPK(EKL,N,NSIZE(N),NOPEN,NSTED,TSR,PM(1,N),CR,CI,IM,J1)
      DO 50 I=1,IM
      DO 50 J=1,JM
      CDUM(I,J)=CMPLX(0.0,0.0)
      DO 50 M=1,JM
   50 CDUM(I,J) = CDUM(I,J) + BM(I,M,N)*EKM(M,J)
      DO 70 I=1,IM
      DO 70 J=1,J1
      CH(I,J)  =CMPLX(0.0,0.0)
      DO 60 M=1,JM
   60 CH(I,J)   = CH(I,J)   + CDUM(I,M)*GM(M,J,N)
   70 CH(I,J)   = CONST * CH(I,J)
      NN = NN + IM
      DO 80 I=1,IM
      CALL PACK(CH(1,I),AJJL,MCB)
   80 CONTINUE
      II = II + IM
  190 CONTINUE
      RETURN
      END
