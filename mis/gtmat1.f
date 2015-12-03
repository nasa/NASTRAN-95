      SUBROUTINE GTMAT1 (SYM,TT)
C
C     THIS SUBROUTINE PROCESSES TRANSFORMATION MATRICES
C     IT IS CALLED ONLY BY CMSFIL
C
      EXTERNAL        RSHIFT   ,ANDF     ,ORF
      INTEGER         TRN      ,SYM      ,ORF     ,TRAN    ,ECPT1    ,
     1                ANDF     ,CHK1     ,CHK2    ,NAME(2) ,RSHIFT
      DIMENSION       ECPT(4)  ,TID(3,3) ,TT(3,3) ,LIST(32),SYMM(6,6),
     1                SMAT(6,3),PROD(6)  ,TC(3,3) ,TG6(6,6),TG(3,3)  ,
     2                T(6,6)
      DIMENSION       ACPT(1)
      COMMON /GTMATX/ LOC1     ,LEN1     ,TRN     ,TT6(6,6),TC6(6,6)
      COMMON /ZZZZZZ/ Z(1)
      EQUIVALENCE     (ECPT1,ECPT(1))
      EQUIVALENCE     (IFLAG,RFLAG)
      DATA    TID   / 1., 0., 0., 0., 1., 0., 0., 0., 1. /
      DATA    SMAT  /-1., 1., 1., 1.,-1.,-1., 1.,-1., 1.,-1., 1.,-1.,
     1                1., 1.,-1.,-1.,-1., 1.  /
      DATA    NAME  / 4HGTMT, 4H1Z            /
C
      IKIND = 0
      DO 10 I = 1,6
      DO 10 J = 1,6
      TT6(I,J) = 0.0
   10 CONTINUE
      IF (TRN.EQ.0 .AND. SYM.EQ.0) GO TO 170
      IF (LOC1.EQ.0 .OR. TRN.EQ.0) GO TO 30
      CALL PRETRS (Z(LOC1),LEN1)
      IKIND = ORF(IKIND,1)
      DO 20 I = 2,4
      ECPT(I) = 0.0
   20 CONTINUE
      ECPT1 = TRN
      CALL TRANSS (ECPT,TT)
      GO TO 50
   30 DO 40 I = 1,3
      DO 40 J = 1,3
      TT(I,J) = TID(I,J)
   40 CONTINUE
   50 DO 60 I = 1,3
      DO 60 J = 1,3
      TT6(I  ,J  ) = TT(I,J)
      TT6(I+3,J+3) = TT(I,J)
   60 CONTINUE
      DO 70 I = 1,6
      DO 70 J = 1,6
      SYMM(I,J) = 0.0
   70 CONTINUE
      IF (SYM .EQ. 0) GO TO 120
      IKIND = ORF(IKIND,1)
      CALL DECODE (SYM,LIST,NDIR)
      DO 80 I = 1,6
      PROD(I) = 1.0
   80 CONTINUE
      DO 100 I = 1,NDIR
      IDIR = LIST(I) + 1
      IDIR = 4 - IDIR
      DO 90 J = 1,6
      PROD(J) = PROD(J)*SMAT(J,IDIR)
   90 CONTINUE
  100 CONTINUE
      DO 110 I = 1,6
      SYMM(I,I) = PROD(I)
  110 CONTINUE
      GO TO 140
  120 DO 130 I = 1,6
      SYMM(I,I) = 1.0
  130 CONTINUE
  140 CALL GMMATS (TT6,6,6,0, SYMM,6,6,0, T)
      DO 150 I = 1,6
      DO 150 J = 1,6
      TT6(I,J) = T(I,J)
  150 CONTINUE
      DO 160 I = 1,3
      DO 160 J = 1,3
      TT(I,J) = TT6(I,J)
  160 CONTINUE
      ISAV = IKIND
      RETURN
C
  170 DO 180 I = 1,6
      TT6(I,I) = 1.0
  180 CONTINUE
      DO 190 I = 1,3
      DO 190 J = 1,3
      TT(I,J) = TID(I,J)
  190 CONTINUE
      ISAV = IKIND
      CHK1 = 13579
      RETURN
C
C
      ENTRY GTMAT2 (LOC2,LEN2,ACPT,TC)
C     ================================
C
      IKIND = ISAV
      DO 200 I = 1,6
      DO 200 J = 1,6
      TC6(I,J) = 0.0
  200 CONTINUE
      RFLAG = ACPT(1)
      IF (LOC2.EQ.0 .OR. IFLAG.EQ.0) GO TO 210
      CALL PRETRS (Z(LOC2),LEN2)
      CALL TRANSS (ACPT,TC)
      IKIND = ORF(IKIND,2)
      GO TO 230
  210 DO 220 I = 1,3
      DO 220 J = 1,3
      TC(I,J) = TID(I,J)
  220 CONTINUE
  230 DO 240 I = 1,3
      DO 240 J = 1,3
      TC6(I  ,J  ) = TC(I,J)
      TC6(I+3,J+3) = TC(I,J)
  240 CONTINUE
      CHK2 = 24680
      RETURN
C
C
      ENTRY GTMAT3 (TRAN,TG,TG6,IHELP)
C     ================================
C
      IF (CHK1.NE.13579 .AND. CHK2.NE.24680) CALL MESAGE (-37,0,NAME)
      DO 300 I = 1,6
      DO 300 J = 1,6
      TG6(I,J) = 0.0
  300 CONTINUE
      IF (TRAN) 340,330,310
  310 CALL PRETRS (Z(LOC1),LEN1)
      DO 320 I = 2,4
      ECPT(I) = 0.0
  320 CONTINUE
      ECPT1 = TRAN
      IKIND = ORF(IKIND,8)
      IF (TRAN .NE. TRN) IKIND = ORF(IKIND,16)
      CALL TRANSS (ECPT,TG)
      IKIND = ORF(IKIND,4)
      GO TO 370
  330 IKIND = ORF(IKIND,4)
  340 DO 350 I = 1,3
      DO 350 J = 1,3
      TG(I,J) = TID(I,J)
  350 CONTINUE
      IF (ANDF(RSHIFT(IKIND,1),1).NE.1 .OR. TRAN.NE.-1) GO TO 360
      CALL GMMATS (TT6,6,6,0, TC6,6,6,0, TG6)
      IHELP = IKIND
      RETURN
C
  360 CONTINUE
  370 DO 380 I = 1,3
      DO 380 J = 1,3
      TG6(I  ,J  ) = TG(I,J)
      TG6(I+3,J+3) = TG(I,J)
  380 CONTINUE
      IHELP = IKIND
      RETURN
      END
