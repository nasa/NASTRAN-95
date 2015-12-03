      SUBROUTINE MPYDRI (A,DA,B,DB,C,DC)
C
C     SPECIAL MPYAD PERFORMS THE MATRIX OPERATION
C        (+/-)A   *B (+/-)C = D   OR
C        (+/-)A(T)*B (+/-)C = D
C
C     WHERE A, OR B IS , OR BOTH ARE, DIAGONAL, ROW VECTOR, OR IDENTITY
C     MATRIX.  MATRIX C CAN BE PURGED
C
C     THIS ROUITNE DOES NOT HANDEL A-TRANSPOSE, WHILE B IS DIAGNOL, ROW
C     VECTOR, OR IDENTIY MASTRIX. ONLY EXCEPTION IS A IS TRULY (Nx1).
C
C     NOTE -
C     1. IN NASTRAN GINO, THE TRAILER 2ND AND 3RD WORDS FOR A ROW-VECTOR
C        IS (1xM), AND THE DIAGONAL MATRIX IS ALSO (1xM)
C     2. THE ROW-VECTOR AND DIAGONAL MATRIX ARE PACKED IN ONE RECORD.
C        AND THUS, THEY REQUIRE SPECIAL ATTENTION DEALING WITH THE FILEB
C        WHILE FILEA IS ALREADY A ROW-VECTOR, OR A DIAGONAL MATRIX
C
C     WRITTEN BY G.CHAN/UNISYS,  1/92
C     LAST MODIFIED FOR SPECIAL CASES THAT INVOLVE B MATRIX IS ALSO
C     A DIAGONAL MATRIX OR A ROW-VECOTR,  2/93                 ----
C
      IMPLICIT INTEGER (A-Z)
      INTEGER          NAME(2),AD(7),SD(7)
      REAL             A(1),B(1),C(1)
      DOUBLE PRECISION DA(1),DB(1),DC(1)
      CHARACTER        UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /XMSSG /  UFM,UWM,UIM,SFM
      COMMON /MPYADX/  FILEA(7),FILEB(7),FILEC(7),FILED(7),NZZ,T,SIGNAB,
     1                 SIGNC,PREC,SCR
      COMMON /SYSTEM/  SYSBUF,NOUT
      COMMON /TYPE  /  PRC(2),WORDS(4)
      COMMON /NAMES /  RD,RDREW,WRT,WRTREW,CLSREW
      COMMON /PACKX /  TYPEP,TYPOUT,IP,JP,INCRP
      COMMON /UNPAKX/  TYPEU,IU,JU,INCRU
      COMMON /TRNSPX/  NAMEA(7),NAMEAT(7),LCORE,NSCR,ISCR
      EQUIVALENCE      (FILEA(1),FA   ),(FILEA(4),FORMA),
     2                 (FILEA(5),TYPEA),(FILEB(1),FB   ),
     4                 (FILEB(4),FORMB),(FILEB(5),TYPEB),
     5                 (FILEC(1),FC   ),(FILEC(4),FORMC),
     7                 (FILEC(5),TYPEC),(FILED(1),FD   ),
     8                 (FILED(2),COLD ),(FILED(4),FORMD),
     9                 (FILED(5),TYPED)
      DATA    NAME   / 4HMPYA , 4HDRI /,
     1        DIAGNL , ROWVEC , IDENT / 3, 7, 8  /
C
C     MOVE TRUE ROWS AND COLUMNS INTO ROWA/B/C AND COLA/B/C
C
      COLA = FILEA(2)
      ROWA = FILEA(3)
      COLB = FILEB(2)
      ROWB = FILEB(3)
      COLC = FILEC(2)
      ROWC = FILEC(3)
      IF (FORMA.EQ.DIAGNL .OR. FORMA.EQ.ROWVEC) COLA = ROWA
      IF (FORMA .EQ. ROWVEC) ROWA = 1
      IF (FORMB.EQ.DIAGNL .OR. FORMB.EQ.ROWVEC) COLB = ROWB
      IF (FORMB .EQ. ROWVEC) ROWB = 1
      IF (FORMC.EQ.DIAGNL .OR. FORMC.EQ.ROWVEC) COLC = ROWC
      IF (FORMC .EQ. ROWVEC) ROWC = 1
C
      IF (SIGNAB.EQ.0 .AND. FC.EQ.0) GO TO 1100
      IF (SIGNAB.EQ.0 .AND. FC.NE.0) GO TO 780
      BUF1  = NZZ  - SYSBUF
      BUF2  = BUF1 - SYSBUF
      BUF3  = BUF2 - SYSBUF
      COLD  = 0
      ROWD  = ROWA
      IF (T .EQ. 1) ROWD = COLA
      IF (PREC.EQ.1 .AND. (TYPED.EQ.2 .OR. TYPED.EQ.4)) TYPED = TYPED -1
      TYPOUT= TYPED
      NWDS  = WORDS(TYPED)
      ROWA2 = ROWA*NWDS
      ROWB2 = ROWB*NWDS
      ROWD2 = ROWD*NWDS
      COLB2 = COLB*2
      NZ    = BUF3 - 1
      SD(1) = SCR
      IF (FC .NE. 0) GO TO 10
      SD(1) = FD
      NZ    = BUF2 - 1
   10 CALL MAKMCB (SD,SD,ROWD,FORMD,TYPED)
C
C     REMEMBER, ONLY FILEA CAN HAVE TRANSPOSE, NOT FILEB.
C     IF FILEA IS DIAGONAL, ROW VECTOR, OR IDENTITY MATRIX, THE ACTUAL
C     TRANSPOSE OF FILEA HAS NEVER TAKEN PLACE.
C
C     FA, FB, FC AND FD ARE FILEA, FILEB, FILEC AND FILED RESPECTIVELY.
C     AD(1) IS EITHER FILEA OR FILED, AND
C     SD(1) IS EITHER SCRATCH FILE OR FILED
C
      IF (T .EQ. 1) GO TO 30
      DO 20 I = 1,7
   20 AD(I) = FILEA(I)
      GO TO 50
   30 DO 40 I = 1,7
   40 AD(I) = FILED(I)
   50 IP    = 1
      JP    = ROWD
      INCRP = 1
      IU    = 1
      INCRU = 1
      IF (FA .LE. 0) GO TO 60
      FILE = FA
      CALL OPEN (*1010,FA,A(BUF1),RDREW)
      CALL FWDREC (*1020,FA)
   60 IF (FB .LE. 0) GO TO 70
      FILE = FB
      CALL OPEN (*1010,FB,A(BUF2),RDREW)
      CALL FWDREC (*1020,FB)
C
   70 IF (FA .LE. 0) GO TO 80
      IF (FORMA .EQ. DIAGNL) GO TO  90
      IF (FORMA .EQ. ROWVEC) GO TO 200
      IF (FORMA .EQ. IDENT ) GO TO 400
   80 IF (T .EQ. 1)  GO TO 990
      IF (FORMB .EQ. DIAGNL) GO TO 490
      IF (FORMB .EQ. ROWVEC) GO TO 600
      IF (FORMB .EQ. IDENT ) GO TO 750
      FILE = 0
      GO TO 1070
C
C                                         D   G   J   M
C     FILEA IS                            E   H   K   N
C     DIAGONAL -                          F   I   L   O
C                      a      a  0  0    aD  aG  aJ  aM
C                      b      0  b  0    bE  bH  bK  bN
C                      c ==>  0  0  c    cF  cI  cL  cO
C
C     SPECIAL CASE NEEDS TO BE CONSIDERED -
C     FILEB IS ALSO A DIAGONAL MATRIX. (FILEB CANNOT BE A ROW VECTOR)
C
   90 FILE  = FA
      JU    = ROWA
      TYPEU = TYPED*SIGNAB
      CALL UNPACK (*1050,FA,A)
      CALL CLOSE (FA,CLSREW)
      CALL GOPEN (SD,A(BUF1),WRTREW)
      FILE  = FB
      JU    = ROWB
      TYPEU = TYPED
      IF (FORMB .EQ. DIAGNL) GO TO 150
      DO 145 I = 1,COLB
      CALL UNPACK (*1050,FB,B)
      GO TO (100,110,120,130), TYPEB
  100 DO 105 J = 1,ROWB
  105 C(J) = A(J)*B(J)
      GO TO 140
  110 DO 115 J = 1,ROWB
  115 DC(J) = DA(J)*DB(J)
      GO TO 140
  120 DO 125 J = 1,ROWB2,2
      C(J  ) = A(J)*B(J  ) - A(J+1)*B(J+1)
  125 C(J+1) = A(J)*B(J+1) + A(J+1)*B(J  )
      GO TO 140
  130 DO 135 J = 1,ROWB2,2
      DC(J  ) = DA(J)*DB(J  ) - DA(J+1)*DB(J+1)
  135 DC(J+1) = DA(J)*DB(J+1) + DA(J+1)*DB(J  )
  140 CALL PACK (C,SD,SD)
  145 CONTINUE
      GO TO 190
C
C     SPECIAL CASE - FILEB IS ALSO A DIAGONAL MATRIX
C
  150 CALL UNPACK (*1050,FB,B)
      IF (TYPEB .GE. 3) GO TO 165
      DO 155 J = 1,ROWB
  155 C(J) = 0.0
      DO 160 J = 1,ROWB
      IF (TYPEB .EQ. 1)  C(J) =  A(J)* B(J)
      IF (TYPEB .EQ. 2) DC(J) = DA(J)*DB(J)
      CALL PACK (C,SD,SD)
      C(J) = 0.0
      IF (TYPEB .EQ. 2) DC(J) = 0.0D+0
  160 CONTINUE
      GO TO 190
C
  165 DO 170 J = 1,ROWB2
  170 C(J) = 0.0
      DO 180 J = 1,ROWB2,2
      IF (TYPEB .EQ. 4) GO TO 175
      C(J  ) = A(J)*B(J  ) - A(J+1)*B(J+1)
      C(J+1) = A(J)*B(J+1) + A(J+1)*B(J  )
      CALL PACK (C,SD,SD)
      C(J  ) = 0.0
      C(J+1) = 0.0
      GO TO 180
  175 DC(J  ) = DA(J)*DB(J  ) - DA(J+1)*DB(J+1)
      DC(J+1) = DA(J)*DB(J+1) + DA(J+1)*DB(J  )
      CALL PACK (C,SD,SD)
      DC(J  ) = 0.0D+0
      DC(J+1) = 0.0D+0
  180 CONTINUE
C
  190 CALL CLOSE (FB,CLSREW)
      CALL CLOSE (SD,CLSREW)
      GO TO 800
C                                         E       I      M
C     FILEA IS A ROW     a                F       J      N
C     VECTOR -           b                G       K      O
C     RESULT IN FILED,   c                H       L      P
C     A (Nx1) RECT.      d ==> a b c d  aE+bF+  aI+bJ+  aM+bN+
C     MATRIX or A ROW-                  cG+dH   cK+dL   cO+dP
C     VECTOR
C
C     SPECIAL CASE NEEDS TO BE CONSIDERED -
C     FILEB IS A DIAGONAL MATRIX. (FILEB CANNOT BE A ROW VECTOR)
C
C
C     TRANSPOSE OF FILEA,                 E       F       G
C     A ROW VECTOR -               a     aE      aF      aG
C                                  b     bE      bF      bG
C                                  c     cE      cF      cG
C                                  d     dE      dF      dG
C
C     SPECIAL CASES NEED TO BE CONSIDERED -
C     FILEB MUST BE A (Nx1) RECTANGULAR MATRIX, OR A ROW VECTOR
C
  200 FILE  = FA
      JU    = ROWA
      TYPEU = TYPED*SIGNAB
      CALL UNPACK (*1050,FA,A)
      CALL CLOSE (FA,CLSREW)
      CALL GOPEN (SD,A(BUF1),WRTREW)
      FILE  = FB
      TYPEU = TYPED
      IF (T .EQ. 1) GO TO 350
C
C     FILEA IS A ROW-VECTOR, RESULT IS ALSO A ROW-VECTOR, OR A
C     (Nx1) RECTANGULAR MATRIX
C
      JU    = ROWB
      IF (FORMB .EQ. DIAGNL) GO TO 260
      IF (ROWB  .NE.   ROWA) GO TO 1030
      COLB4 = COLB*4
      DO 205 J = 1,COLB4
  205 C(J) = 0.0
      DO 250 J = 1,COLB
      CALL UNPACK (*290,FB,B)
      GO TO (210,220,230,240), TYPEB
  210 DO 215 K = 1,ROWB
  215 C(J) = C(J) + A(K)*B(K)
      GO TO 250
  220 DO 225 K = 1,ROWB
  225 DC(J) = DC(J) + DA(K)*DB(K)
      GO TO 250
  230 DO 235 K = 1,ROWB2,2
      C(J  ) = C(J  ) + A(K)*B(K  ) - A(K+1)*B(K+1)
  235 C(J+1) = C(J+1) + A(K)*B(K+1) + A(K+1)*B(K  )
      GO TO 250
  240 DO 245 K = 1,ROWB,2
      DC(J  ) = DC(J  ) + DA(K)*DB(K  ) - DA(K+1)*DB(K+1)
  245 DC(J+1) = DC(J+1) + DA(K)*DB(K+1) + DA(K+1)*DB(K  )
  250 CONTINUE
      GO TO 300
C
C     SPECIAL CASE - FILEB IS A DIAGONAL MATRIX.
C
  260 CALL UNPACK (*1050,FB,B)
      GO TO (270,280,290,300), TYPEB
  270 DO 275 J = 1,COLB
  275 C(J) = A(J)*B(J)
      GO TO 310
  280 DO 285 J = 1,COLB
  285 DC(J) = DA(J)*DB(J)
      GO TO 310
  290 DO 295 J = 1,COLB2,2
      C(J  ) = A(J)*B(J  ) - A(J+1)*B(J+1)
  295 C(J+1) = A(J)*B(J+1) + A(J+1)*B(J  )
      GO TO 310
  300 DO 305 J = 1,COLB2,2
      DC(J  ) = DA(J)*DB(J  ) - DA(J+1)*DB(J+1)
  305 DC(J+1) = DA(J)*DB(J+1) + DA(J+1)*DB(J  )
C
  310 CALL CLOSE (FB,CLSREW)
      IF (FC .EQ. 0) GO TO 340
      FILE  = FC
      TYPEU = TYPEC*SIGNC
      CALL GOPEN (FC,A(BUF2),RDREW)
      IF (FORMC .NE. ROWVEC) GO TO 311
      CALL UNPACK (*1050,FC,A(1))
      GO TO 314
  311 IP = 1
      JP = 1
      DO 313 J = 1,COLC
      CALL UNPACK (*312,FC,A(J*NWDS-1))
      GO TO 313
  312 A(J*NWDS-1) = 0.
      A(J*NWDS  ) = 0.
  313 CONTINUE
C
  314 CALL CLOSE (FC,CLSREW)
      GO TO (315,325), TYPED
  315 DO 320 J = 1,ROWD2
  320 C(J) = C(J) + A(J)
      GO TO 340
  325 DO 330 J = 1,ROWD2
  330 DC(J) = DC(J) + DA(J)
C
  340 CALL PACK (C,SD,SD)
      FORMD = ROWVEC
      GO TO 970
C
C     FILEA (A ROW VECTOR) TRANSFPOSE
C
  350 IF (FORMB .EQ. ROWVEC) GO TO 390
      IF (ROWB .NE. 1) GO TO 1030
      IU = 0
      J  = 1
      DO 380 I = 1,ROWB
      CALL UNPACK (*360,FB,B(J))
      IF (IU .NE. I) GO TO 1030
      GO TO 380
  360 JE = J + NWDS
      DO 370 K = J,JE
  370 B(K) = 0.0
  380 J  = J + NWDS
      CALL CLOSE (FB,CLSREW)
      IU = 1
      GO TO 610
C
C     SPECAIL CASE - FILE B IS A ROW VECTOR
C
  390 IF (ROWB .NE. 1) GO TO 1030
      JU = COLB
      CALL UNPACK (*1030,FB,B(1))
      CALL CLOSE (FB,CLSREW)
      GO TO 610
C
C     FILEA IS IDENTITY -
C
C     SPECIAL CASEs NEED TO BE CONSIDERED -
C     SIGNAB IS NEGATIVE, OR FILEB IS A DIAGONAL MATRIX
C     (FILEB CANNOT BE A ROW-VECTOR)
C
  400 CALL CLOSE (FA,CLSREW)
      IF (FORMB.EQ.DIAGNL .OR. SIGNAB.LT.0) GO TO 420
      FILE = SD(1)
      CALL OPEN (*1010,FA,A(BUF1),WRTREW)
      CALL REWIND (FB)
      CALL CPYFIL (FB,SD,A(1),NZ,K)
      CALL CLOSE (FB,CLSREW)
      CALL CLOSE (SD,CLSREW)
      IF (FC .EQ. 0) GO TO 410
      DO 405 I = 2,7
  405 SD(I) = FILEB(I)
      GO TO 800
  410 DO 415 I = 2,7
  415 FILED(I) = FILEB(I)
      GO TO 1100
C
C     SPECIAL CASE - FILEB IS A DIAGONAL MATRIX
C                    OR SIGNAB IS NEGATIVE
C
  420 CALL GOPEN (SD,A(BUF1),WRTREW)
      JU    = ROWB
      FILE  = FB
      TYPEU = TYPED*SIGNAB
      IF (FORMB .NE. DIAGNL) GO TO 430
      CALL UNPACK (*1050,FB,B)
      CALL CLOSE (FB,CLSREW)
      J  = 1
      DO 425 I = 1,ROWA
      IP = I
      JP = I
      CALL PACK (B(J),SD,SD)
  425 J = J + NWDS
      CALL CLOSE (SD,CLSREW)
      IF (FC) 800,950,800
C
C     SPECIAL CASE - SIGNAB IS NEGATIVE
C
  430 FILE = FB
      DO 435 I = 1,COLB
      CALL UNPACK (*1050,FB,B)
      CALL PACK (B,SD,SD)
  435 CONTINUE
      CALL CLOSE (SD,CLSREW)
      CALL CLOSE (FB,CLSREW)
      IF (FC) 800,950,800
C
C     FILEA IS A COLUMN MATRIX -
C     i.e. A (1,N) RECTANGULAR MATRIX OR A (Nx1) TRANSPOSE
C
C     FILEB MUST BE A (Nx1) RECTANGULAR MATRIX
C
C     CURRENTLY THIS CASE IS HANDLED IN MPYAD SUBROUINTE
C
C     HOWEVER, IF FILEB IS A ROW VECTOR,  IT IS HANDLED IN 600
C     IF FILEA IS A ROW VECTOR TRANSPOSE, IT IS HANDLED IN 200/350
C
C 440 CONTINUE
C
C                                         X   0   0      X
C     FILEB IS DIAGONAL -                 0   Y   0      Y
C                                         0   0   Z <==  Z
C                             a  e  i    aX  eY  iZ
C                             b  f  j    bX  fY  jZ
C                             c  g  k    cX  gY  kZ
C                             d  h  l    dX  hY  lZ
C
  490 FILE  = FB
      JU    = COLB
      TYPEU = TYPED*SIGNAB
      CALL UNPACK (*1050,FB,B)
      CALL CLOSE (FB,CLSREW)
      CALL GOPEN (SD,A(BUF2),WRTREW)
      FILE  = FA
      JU    = ROWA
      TYPEU = TYPED
      DO 590 I = 1,COLA
      CALL UNPACK (*1050,FA,A)
      GO TO (500,520,540,560) TYPEB
  500 DO 510 J = 1,ROWA
  510 C(J)  = A(J)*B(I)
      GO TO 580
  520 DO 530 J = 1,ROWA
  530 DC(J) = DA(J)*DB(I)
      GO TO 580
  540 DO 550 J = 1,ROWA2,2
      C(J  ) = A(J)*B(J  ) - A(J+1)*B(J+1)
  550 C(J+1) = A(J)*B(J+1) + A(J+1)*B(J  )
      GO TO 580
  560 DO 570 J = 1,ROWA2,2
      DC(J  ) = DA(J)*DB(J  ) - DA(J+1)*DB(J+1)
  570 DC(J+1) = DA(J)*DB(J+1) + DA(J+1)*DB(J  )
  580 CALL PACK (C,SD,SD)
  590 CONTINUE
      CALL CLOSE (AD,CLSREW)
      CALL CLOSE (SD,CLSREW)
      GO TO 800
C
C     FILEB IS A ROW VECTOR -                            E
C                                                        F
C     NOTE - FILEA MUST BE A               E   F   G <== G
C     ONE-COLUMN MATRIX.             a    aE  aF  aG
C     i.e. A(1xN) OR                 b    bE  bF  bG
C          A(Nx1) TRNASPOSE          c    cE  cF  cG
C                                    d    dE  dF  dG
C     WE ALREADY HANDLED THE CASE
C     WHERE FILEA IS A ROW-VECTOR TRANSPOSE IN 200
C
  600 FILE  = FB
      JU    = COLB
      TYPEU = TYPED*SIGNAB
      IF (T .EQ. 1) GO TO 602
      IF (COLA .NE. 1) GO TO 1030
      CALL UNPACK (*1050,FB,B)
      GO TO 608
  602 IF (ROWA .NE. 1) GO TO 1030
      J = COLA*NWDS
      DO 604 I = 1,J
  604 B(I) = 0.0
      J = 1
      DO 606 I = 1,COLA
      CALL UNPACK (*606,FB,B(J))
  606 J = J + NWDS
  608 CALL CLOSE (FB,CLSREW)
      FILE  = FA
      JU    = ROWA
      TYPEU = TYPED
      CALL UNPACK (*1050,FA,A)
      CALL CLOSE (AD,CLSREW)
      CALL GOPEN (FD,A(BUF1),WRTREW)
  610 DO 710 J = 1,COLB
      GO TO (620,640,660,680), TYPEA
  620 DO 630 I = 1,ROWA
  630 C(I) = A(I)*B(J)
      GO TO 700
  640 DO 650 I = 1,ROWA
  650 DA(I) = DA(I)*DB(J)
      GO TO 700
  660 DO 670 I = 1,ROWA2,2
      C(I  ) = A(I)*B(J  ) - A(I+1)*B(J+1)
  670 C(I+1) = A(I)*B(J+1) + A(I+1)*B(J  )
      GO TO 700
  680 DO 690 I = 1,ROWA2,2
      DC(I  ) = DA(I)*DB(J  ) - DA(I+1)*DB(J+1)
      DC(I+1) = DA(I)*DB(J+1) + DA(I+1)*DB(J  )
  690 KX = KX + NWDS
  700 CALL PACK (C,FD,FILED)
  710 CONTINUE
      CALL CLOSE (FD,CLSREW)
      GO TO 800
C
C     FILEB IS IDENTITY -
C
C     SPECIAL CASE NEEDS TO BE CONSIDERED -
C     NEGATIVE SIGNAB
C
  750 CALL CLOSE (FB,CLSREW)
      FILE = SD(1)
      CALL OPEN (*1010,SD,A(BUF2),WRTREW)
      IF (SIGNAB .LT. 0) GO TO 760
      CALL REWIND (FA)
      CALL CPYFIL (FA,SD,A(1),NZ,K)
      GO TO 770
C
  760 TYPEU = TYPED*SIGNAB
      JU    = ROWA
      FILE  = FA
      DO 765 I = 1,COLA
      CALL UNPACK (*1050,FA,A)
      CALL PACK (A,SD,SD)
  765 CONTINUE
  770 CALL CLOSE (FA,CLSREW)
      CALL CLOSE (SD,CLSREW)
      IF (FC) 800,950,800
C
C     NULL MATRIX PRODUCT A*B, COPY FILEC TO FILED
C
  780 FILE = FC
      CALL OPEN (*1010,FC,A(BUF1),RDREW)
      FILE = FD
      CALL OPEN (*1010,FD,A(BUF2),WRTREW)
      CALL CPYFIL (FC,FD,A(1),NZ,K)
      CALL CLOSE (FC,CLSREW)
      CALL CLOSE (FD,CLSREW)
      DO 790 I = 2,7
      FILED(I) = FILEC(I)
  790 CONTINUE
      GO TO 1100
C
C     ADD PRODUCT OF A*B TO C
C
  800 IF (FC .EQ. 0) GO TO 950
      CALL GOPEN (FD,A(BUF3),WRTREW)
      FILE = FC
      CALL OPEN (*1010,FC,A(BUF2),RDREW)
      CALL FWDREC (*1020,FC)
      FILE = SD(1)
      CALL OPEN (*1010,SD,A(BUF1),RDREW)
      CALL FWDREC (*1020,SD)
      JU   = ROWC
      TYPEP = TYPED
      DO 920 I = 1,COLC
      TYPEU = TYPED*SIGNC
      CALL UNPACK (*810,FC,C)
      GO TO 830
  810 DO 820 J = 1,ROWD2
  820 C(J) = 0.0
  830 TYPEU = TYPED
      CALL UNPACK (*840,SD,B)
      GO TO 860
  840 DO 850 J = 1,ROWD2
  850 B(J) = 0.0
  860 GO TO (870,890,870,890), TYPED
  870 DO 880 J = 1,ROWD2
  880 A(J) = B(J) + C(J)
      GO TO 910
  890 DO 900 J = 1,ROWD2
  900 DA(J) = DB(J) + DC(J)
  910 CALL PACK (A,FD,FILED)
  920 CONTINUE
      CALL CLOSE (FC,CLSREW)
      CALL CLOSE (SD,CLSREW)
C
  950 IF (COLD .NE. 0) GO TO 970
      DO 960 I = 2,7
  960 FILED(I) = SD(I)
  970 CALL CLOSE  (FD,CLSREW)
      CALL WRTTRL (FILED)
      GO TO 1100
C
C     ERROR
C
  990 WRITE  (NOUT,1000) SFM
 1000 FORMAT (A25,'. MPYDRI DOES NOT HANDLE A-TRANSPOSE. SHOULD NOT BE',
     1       ' CALLED BY MPYAD')
      GO TO 1070
 1010 J = -1
      GO TO 1080
 1020 J = -2
      GO TO 1080
 1030 WRITE  (NOUT,1040) UFM
 1040 FORMAT (A23,' FROM MPYAD/MPYDRI.  FILES NOT COMPATIBLE')
      GO TO 1070
 1050 WRITE  (NOUT,1060) UFM
 1060 FORMAT (A23,' FROM MPYAD/MPYDRI.  NULL COLUMN ENCOUNTERED DURING',
     1        ' MATRIX UNPACK')
 1070 J = -37
 1080 CALL MESAGE (J,FILE,NAME)
C
 1100 RETURN
      END
