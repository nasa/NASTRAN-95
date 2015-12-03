      SUBROUTINE DIAG36 (Z,BUF,GPL,SIL,EQEXIN)
C
C     THIS ROUTINE PRINTS THE INTERNAL-EXTERNAL-SIL NOS. OF THE GRID
C     POINTS AND SCALAR POINTS, AS REQUESTED BY DIAG 36
C
      INTEGER         Z(2),     BUF,      GPL,      SIL,      EQEXIN,
     1                FILE,     NAM(2)
      COMMON /SYSTEM/ IBUF,     L,        DUMMY(6), NLPP
      COMMON /NAMES / RD,       RDREW,    SKIP(2),  REW
      DATA            NAM /     4HDIAG,   4H34      /
C
      FILE = GPL
      Z(1) = GPL
      CALL RDTRL (Z(1))
      N1 = Z(2)
      N2 = N1 + N1
      N3 = N2 + N1 + 1
      IF (N1 .LE. 0) GO TO 150
C
      N = 1
      DO 10 I = 1,2
      CALL OPEN (*150,FILE,Z(BUF),RDREW)
      CALL FWDREC (*160,FILE)
      CALL READ (*150,*170,FILE,Z(N),N1,1,J)
      CALL CLOSE (FILE,REW)
      FILE = SIL
 10   N = N + N1
C
C     HERE WE HAVE, IN INTERNAL NUMBER ORDER,
C        Z(   1 THRU N1) = EXTERNAL NOS.
C        Z(N1+1 THRU N2) = SIL NOS.
C
      NLPX = NLPP - 8
      N = NLPX*3
      DO 60 I = 1,N1,N
      CALL PAGE1
      WRITE  (L,30)
 30   FORMAT (/46X,38HTABLE OF INTERNAL-EXTERNAL-SIL NUMBERS,
     1       //10X,3(6X,30HINTERNAL  EXTERNAL      SIL   ),
     2        /10X,3(6X,3(10H--------  )))
      IM1 = I - 1
      DO 50 J = 1,NLPX
      J1 = IM1 + J
      J2 = J1  + NLPX
      J3 = J2  + NLPX
      IF (J3 .LE. N1) WRITE (L,40)
     1   J1,Z(J1),Z(J1+N1), J2,Z(J2),Z(J2+N1), J3,Z(J3),Z(J3+N1)
      IF (J3.GT.N1 .AND. J2.LE.N1) WRITE (L,40)
     1   J1,Z(J1),Z(J1+N1), J2,Z(J2),Z(J2+N1)
      IF (J2.GT.N1 .AND. J1.LE.N1) WRITE (L,40) J1,Z(J1),Z(J1+N1)
 40   FORMAT (10X,3(4X,3I10,2X))
 50   CONTINUE
 60   CONTINUE
C
      CALL SSWTCH (20,J)
      IF (J .EQ. 0) RETURN
C
      FILE = EQEXIN
      CALL OPEN (*150,FILE,Z(BUF),RDREW)
      CALL FWDREC (*160,FILE)
      CALL READ (*150,*170,FILE,Z( 1),N2,1,J)
      CALL READ (*150,*170,FILE,Z(N3),N2,1,J)
      CALL CLOSE (FILE,REW)
      I = N3 - 1
      J = N2
      K = N3 + N2 - 1
      DO 70 N = 1,N1
      Z(I  ) = Z(K  )
      Z(I-1) = Z(J  )
      Z(I-2) = Z(J-1)
      I = I - 3
      J = J - 2
 70   K = K - 2
C
C     HERE WE HAVE AN ARRAY OF EXTERNAL-INTERNAL-CODED SIL. PRINT IT OUT
C
      NLPX = NLPX*3
      N    = NLPX*3
      N3   = N3 - 1
      DO 100 I = 1,N3,N
      CALL PAGE1
      WRITE  (L,80)
 80   FORMAT (/44X,44HTABLE OF EXTERNAL-INTERNAL-CODED SIL NUMBERS,
     1       //10X,3(6X,30HEXTERNAL  INTERNAL CODED SIL  ),
     2        /10X,3(5X,3(10H--------- ),1X))
      IM1 = I - 1
      DO 90 J = 1,NLPX,3
      J1 = IM1 + J
      J2 = J1  + NLPX
      J3 = J2  + NLPX
      IF (J3 .LE. N3) WRITE (L,40)
     1   Z(J1),Z(J1+1),Z(J1+2), Z(J2),Z(J2+1),Z(J2+2),
     2   Z(J3),Z(J3+1),Z(J3+2)
      IF (J3.GT.N3 .AND. J2.LE.N3) WRITE (L,40)
     1   Z(J1),Z(J1+1),Z(J1+2), Z(J2),Z(J2+1),Z(J2+2)
      IF (J2.GT.N3 .AND. J1.LE.N3) WRITE (L,40) Z(J1),Z(J1+1),Z(J1+2)
 90   CONTINUE
 100  CONTINUE
C
      WRITE  (L,110)
 110  FORMAT (//10X,33H*** JOB TERMINATED BY DIAG 20 ***)
      CALL PEXIT
C
 150  N = -1
      GO TO 180
 160  N = -2
      GO TO 180
 170  N = -7
 180  CALL MESAGE (N,FILE,NAM)
      RETURN
      END
