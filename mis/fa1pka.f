      SUBROUTINE FA1PKA(A,M1K,M1B,EIV,NCORE,N)
C
C     FA1PKA BUILDS THE MATRIX FOR ALLMAT
C
      INTEGER NAME(2)
      REAL A(1),EIV(1)
      REAL M1K(1),M1B(1)
      DATA NAME /4HFA1P,4HKA  /
      DATA NHEIGS,NHEIGE /4HEIGS,4HEIGE/
      N2 = N*2
      IZ = 0
      IMK = N
      IMI = N*N*2
      IMB = IMI + N
      K = 0
      DO 10 I = 1,N
      DO 20 J = 1,N
      K = K +1
      A(IZ+J) = 0.0
      A(IMK+J) = M1K(K)
      A(IMB+J) = M1B(K)
      A(IMI+J) = 0.0
      IF(I.EQ.J) A(IMI+J) = 1.0
   20 CONTINUE
      IZ = IZ + N2
      IMK = IMK + N2
      IMI = IMI + N2
      IMB = IMB + N2
   10 CONTINUE
C
C     CALL HSBG AND ATEIG FOR EIVENVALUES
C
      N4=N2*2
      IL = 1
      IH = IL + N2
      IM=IH+N4
      II=IM+N4
      IF(II     .GT.NCORE) CALL MESAGE(-8,0,NAME)
      CALL SSWTCH(39,L39)
      IF(L39.NE.0) CALL CONMSG(NHEIGS,1,0)
      CALL HSBG(N2,A,N2,A)
      CALL ATEIG(N2,A,EIV(IH),EIV(IM),EIV(IL),N2,
     *              A,EIV(IH),EIV(IM))
      IL = 0
      DO 30 I=1,N2
      EIV(I+IL) = EIV(I+IH-1)
      EIV(I+IL+1) = EIV(I + IM -1)
      IL = IL +1
   30 CONTINUE
      IF(L39.NE.0) CALL CONMSG(NHEIGE,1,0)
      RETURN
      END
