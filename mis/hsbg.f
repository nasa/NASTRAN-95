      SUBROUTINE HSBG(N,A,IA,B)
C
C     ..................................................................
C
C        SUBROUTINE HSBG
C
C        PURPOSE
C           TO REDUCE A REAL MATRIX INTO UPPER ALMOST TRIANGULAR FORM
C
C        USAGE
C           CALL HSBG(N,A,IA)
C
C        DESCRIPTION OF THE PARAMETERS
C           N      ORDER OF THE MATRIX
C           A      THE INPUT MATRIX, N BY N
C           IA     SIZE OF THE FIRST DIMENSION ASSIGNED TO THE ARRAY
C                  A IN THE CALLING PROGRAM WHEN THE MATRIX IS IN
C                  DOUBLE SUBSCRIPTED DATA STORAGE MODE.  IA=N WHEN
C                  THE MATRIX IS IN SSP VECTOR STORAGE MODE.
C
C        REMARKS
C           THE HESSENBERG FORM REPLACES THE ORIGINAL MATRIX IN THE
C           ARRAY A.
C
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
C           NONE
C
C        METHOD
C           SIMILARITY TRANSFORMATIONS USING ELEMENTARY ELIMINATION
C           MATRICES, WITH PARTIAL PIVOTING.
C
C        REFERENCES
C           J.H. WILKINSON - THE ALGEBRAIC EIGENVALUE PROBLEM -
C           CLARENDON PRESS, OXFORD, 1965.
C
C     ..................................................................
C
      DIMENSION B(1)
      DOUBLE PRECISION A(1),PIV,S,T
C
C     MAKE THIS ROUTINE DOUBLE A AND B ARE SAME SPACE
C
      N2=N*N
      K=N2
      DO 10 I=1,N2
      A(K)=B(K)
10    K=K-1
      L=N
      NIA=L*IA
      LIA=NIA-IA
C
C        L IS THE ROW INDEX OF THE ELIMINATION
C
   20 IF(L-3) 360,40,40
   40 LIA=LIA-IA
      L1=L-1
      L2=L1-1
C
C        SEARCH FOR THE PIVOTAL ELEMENT IN THE LTH ROW
C
      ISUB=LIA+L
      IPIV=ISUB-IA
      PIV=DABS(A(IPIV))
      IF(L-3) 90,90,50
   50 M=IPIV-IA
      DO 80 I=L,M,IA
      T=DABS(A(I))
      IF(T-PIV) 80,80,60
   60 IPIV=I
      PIV=T
   80 CONTINUE
   90 IF(PIV) 100,320,100
100   IF(PIV-DABS(A(ISUB))) 180,180,120
C
C        INTERCHANGE THE COLUMNS
C
  120 M=IPIV-L
      DO 140 I=1,L
      J=M+I
      T=A(J)
      K=LIA+I
      A(J)=A(K)
  140 A(K)=T
C
C        INTERCHANGE THE ROWS
C
      M=L2-M/IA
      DO 160 I=L1,NIA,IA
      T=A(I)
      J=I-M
      A(I)=A(J)
  160 A(J)=T
C
C        TERMS OF THE ELEMENTARY TRANSFORMATION
C
  180 DO 200 I=L,LIA,IA
      A(I)=A(I)/A(ISUB)
200   CONTINUE
C
C        RIGHT TRANSFORMATION
C
      J=-IA
      DO 240 I=1,L2
      J=J+IA
      LJ=L+J
      DO 220 K=1,L1
      KJ=K+J
      KL=K+LIA
      A(KJ)=A(KJ)-A(LJ)*A(KL)
220   CONTINUE
  240 CONTINUE
C
C        LEFT TRANSFORMATION
C
      K=-IA
      DO 300 I=1,N
      K=K+IA
      LK=K+L1
      S=A(LK)
      LJ=L-IA
      DO 280 J=1,L2
      JK=K+J
      LJ=LJ+IA
      S=S+A(LJ)*A(JK)
280   CONTINUE
  300 A(LK)=S
C
C        SET THE LOWER PART OF THE MATRIX TO ZERO
C
      DO 310 I=L,LIA,IA
  310 A(I)=0.0
  320 L=L1
      GO TO 20
  360 RETURN
      END
