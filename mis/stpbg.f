      SUBROUTINE STPBG(BM,GM,NS,BLOC,D,CA,NSIZE)
C     MAKES MATRICES BM AND GM FOR EACH STRIP
      DIMENSION NSIZE(1),CA(1),D(1),BLOC(1),BM(4,4,NS),GM(4,3,NS)
      DO 100 N=1,NS
      DO 10 I=1,4
      DO 10 J=1,4
   10 BM(I,J,N)=0.0
      DO 15 I=1,4
      DO 15 J=1,3
   15 GM(I,J,N)=0.0
      BM(1,1,N)=  BLOC(N)
      BM(2,2,N)= -BLOC(N)*BLOC(N)
      GM(1,1,N)=-1.0/BLOC(N)
      GM(2,2,N)= 1.0
      IF(NSIZE(N).EQ.2) GO TO 50
C         CONTROL SURFACE CASE
      E= CA(N) + D(N) - 1.5*BLOC(N)
      BM(3,3,N)=E*BLOC(N)
      BM(3,4,N)= BM(2,2,N)
      GM(3,3,N)= 1.0
      GM(4,3,N)= -E/BLOC(N)
   50 CONTINUE
  100 CONTINUE
      RETURN
      END
