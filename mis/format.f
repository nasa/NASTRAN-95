      SUBROUTINE FORMAT (A,N1X,N2X,N3X,L1X,L2X)
C
C  $MIXED_FORMATS
C
      REAL            FF(6),F(6,6,2),F11(6),F21(6),F31(6),F41(6),F51(6),
     1                F61(6),F12(6),F22(6),F32(6),F42(6),F52(6),F62(6)
      DIMENSION       A(6)
      COMMON /SYSTEM/ SKIP,MO
      EQUIVALENCE     (F11(1),F(1,1,1)), (F21(1),F(1,2,1)),
     1                (F31(1),F(1,3,1)), (F41(1),F(1,4,1)),
     2                (F51(1),F(1,5,1)), (F61(1),F(1,6,1)),
     3                (F12(1),F(1,1,2)), (F22(1),F(1,2,2)),
     4                (F32(1),F(1,3,2)), (F42(1),F(1,4,2)),
     5                (F52(1),F(1,5,2)), (F62(1),F(1,6,2))
      DATA    F11   / 4H(I5, ,4H49X, ,4H1P,1 ,4HE19. ,4H6,I0 ,4H58)  /,
     1        F21   / 4H(I5, ,4H40X, ,4H1P,2 ,4HE19. ,4H6,I0 ,4H48)  /,
     2        F31   / 4H(I5, ,4H30X, ,4H1P,0 ,4HE19. ,4H6,I0 ,4H39)  /,
     3        F41   / 4H(I5, ,4H21X, ,4H1P,4 ,4HE19. ,4H6,I0 ,4H29)  /,
     4        F51   / 4H(I5, ,4H11X, ,4H1P,5 ,4HE19. ,4H6,I0 ,4H20)  /,
     5        F61   / 4H(I5, ,4H02X, ,4H1P,6 ,4HE19. ,4H6,I0 ,4H10)  /
      DATA    F12   / 4H(I5, ,4H02X, ,4H1P,1 ,4HE19. ,4H6,I1 ,4H05)  /,
     1        F22   / 4H(I5, ,4H02X, ,4H1P,2 ,4HE19. ,4H6,I0 ,4H86)  /,
     2        F32   / 4H(I5, ,4H02X, ,4H1P,3 ,4HE19. ,4H6,I0 ,4H67)  /,
     3        F42   / 4H(I5, ,4H02X, ,4H1P,4 ,4HE19. ,4H6,I0 ,4H48)  /,
     4        F52   / 4H(I5, ,4H02X, ,4H1P,5 ,4HE19. ,4H6,I0 ,4H29)  /,
     5        F62   / 4H(I5, ,4H02X, ,4H1P,6 ,4HE19. ,4H6,I0 ,4H10)  /
C
       N1 = N1X
       N2 = N2X
       N3 = N3X
       L1 = L1X
       L2 = L2X
       N  = (N2-N1+N3)/N3
       IF (N .LE. 0) GO TO 20
       IF (N .GT. 6) N = 6
       L  = 2
       IF (L1.LE.0 .OR. L2.LE.0) L = 1
       DO 10 I = 1,6
       FF(I) = F(I,N,L)
   10  CONTINUE
       L1 = IABS(L1)
       L2 = IABS(L2)
       WRITE (MO,FF,ERR=20) L1,(A(I),I=N1,N2,N3),L2
C
   20  RETURN
       END
