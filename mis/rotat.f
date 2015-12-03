      SUBROUTINE ROTAT (ECT2,B1,GPLST,X)
C
      INTEGER         GPLST(1),ECT2,B1,TYPES(13),ESYM,ELID,GPTS(12),
     1                BAR,OFFSET
      REAL            X(3,1),MAGTUD(2),NORMAL(3)
      DIMENSION       REC1(146),REC2(17),A(3,3),T(2,2),V(2,3),CROSS(3),
     1                SHEAR(3),ISYM(13)
      COMMON /BLANK / SKIP(23),OES1,SCR1,SCR2,NEWOES
      COMMON /XXPARM/ SKPPAR(211),ICASE,FLAG,DATA,SKPARM
      EQUIVALENCE     (REC1(3),ITYPE),(REC1(4),ISUB),(REC1( 5),TIME),
     1                (REC1(6),EIGEN),(REC1(10),NWDS)
      DATA    TYPES / 6,7,8,9,15,16,17,18,19,62,63,64,83 /
      DATA    ISYM  / 2HT1,2HTB,2HTP,2HTM,2HQP,2HQM,2HT2,2HQ2,2HQ1,2HM1,
     1                2HM2,2HQ4,2HT3/,    ESYM/2H  /,    BAR /2HBR/
C
      TWOPI = 8.0*ATAN(1.0)
      IRDECT= 0
      SUM   = 0.0
      CALL OPEN (*110,NEWOES,GPLST(B1),1)
      IREC  = 0
      ELID  = 0
   10 CALL READ (*110,*110,OES1,REC1,146,1,M)
      IF (ISUB .NE. ICASE) GO TO 13
      IF (FLAG .EQ.   0.0) GO TO 14
      IF (FLAG.EQ.1.0 .AND. TIME.EQ.DATA) GO TO 14
      FIGEN = SQRT(ABS(EIGEN))/TWOPI
      IF (FLAG.EQ.2.0 .AND. ABS(FIGEN-DATA).GT.1.0E-5) GO TO 14
   13 IF (IREC .EQ. 0) GO TO 18
      GO TO 100
C
C     CHECK ELEMENT TYPE
C
   14 IREC = IREC + 2
      DO 15 IT = 1,13
      IF (ITYPE .EQ. TYPES(IT)) GO TO 20
   15 CONTINUE
C
C     SKIP SUBCASE
C
   18 CALL FWDREC (*110,OES1)
      GO TO 10
C
C     CHECK ELEMENT TYPE
C
   20 IF (ELID .NE. 0) GO TO 21
      CALL READ (*22,*22,ECT2,ESYM,1,0,N)
      CALL FREAD (ECT2,NGPPE,1,0)
      IRDECT = 1
      OFFSET = 0
      IF (ESYM .EQ. BAR) OFFSET = 6
      IF (ESYM.EQ.ISYM(12) .OR. ESYM.EQ.ISYM(13)) OFFSET = 1
      IF (ESYM .EQ. ISYM(IT)) GO TO 23
   21 CALL FREAD (ECT2,ELID,1,0)
      IF (ELID .EQ. 0) GO TO 20
      J = 1 + NGPPE + OFFSET
      CALL FREAD (ECT2,0,-J,0)
      GO TO 21
   22 CALL BCKREC (ECT2)
      IRDECT = 0
      GO TO 18
C
C     PROCESS SUBCASE
C
   23 CALL WRITE (NEWOES,REC1,146,1)
      NWDS = NWDS - 1
   25 CALL READ  (*100,*56,OES1,IELMT,1,0,M)
      CALL FREAD (OES1,REC2,NWDS,0)
   30 CALL FREAD (ECT2,ELID,1,0)
      IF (ELID .EQ. 0) GO TO 55
      CALL FREAD (ECT2,0,-1,0)
      CALL FREAD (ECT2,GPTS,NGPPE,0)
      IF (OFFSET .NE. 0) CALL FREAD (ECT2,0,-OFFSET,0)
   29 IF (ELID .EQ. IELMT/10) GO TO 31
      IF (ELID .GT. IELMT/10) GO TO 60
      GO TO 30
   31 IG1 = GPTS(1)
      IG2 = GPTS(2)
      IG1 = IABS(GPLST(IG1))
      IG2 = IABS(GPLST(IG2))
      IG3 = GPTS(3)
      IG3 = IABS(GPLST(IG3))
      DO 32 I = 1,3
      V(1,I) = X(I,IG1) - X(I,IG2)
      V(2,I) = X(I,IG1) - X(I,IG3)
   32 CONTINUE
      MAGTUD(1) = SQRT(V(1,1)**2 + V(1,2)**2 + V(1,3)**2)
      MAGTUD(2) = SQRT(V(2,1)**2 + V(2,2)**2 + V(2,3)**2)
      DO 33 I = 1,3
      V(1,I) = V(1,I)/MAGTUD(1)
      V(2,I) = V(2,I)/MAGTUD(2)
      A(1,I) = V(1,I)
   33 CONTINUE
      A(2,1) = A(1,2)
      A(3,1) = A(1,3)
      A(3,3) =   V(1,1)*V(2,2) - V(2,1)*V(1,2)
      CROSS(1) = V(1,2)*V(2,3) - V(2,2)*V(1,3)
      CROSS(2) = V(2,1)*V(1,3) - V(1,1)*V(2,3)
      CROSS(3) = A(3,3)
      A(2,2) = CROSS(1)*V(1,3) - V(1,1)*CROSS(3)
      A(2,3) = V(1,1)*CROSS(2) - CROSS(1)*V(1,2)
      A(3,2) = A(2,3)
      IEL    = 0
      DO 40 MORE = 1,2
      IF (ITYPE.EQ.9 .OR. ITYPE.EQ.16) GO TO 34
      NORM   = IEL + 2
      ISHEAR = IEL + 4
      GO TO 35
   34 NORM   = IEL + 1
      ISHEAR = IEL + 3
   35 T(1,1) = REC2(NORM  )
      T(2,2) = REC2(NORM+1)
      T(1,2) = REC2(ISHEAR)
      T(2,1) = T(1,2)
      DO 38 I = 1,3
      SUM = 0.0
      DO 37 J = 1,2
      DO 37 K = 1,2
      SUM = SUM + A(I,J)*A(I,K)*T(J,K)
   37 CONTINUE
      NORMAL(I) = SUM
   38 CONTINUE
      SHEAR(1) = A(2,1)*A(1,1)*T(1,1) + A(2,1)*A(1,2)*T(1,2)
     1         + A(2,2)*A(1,2)*T(2,1) + A(2,2)*A(1,2)*T(2,2)
      SHEAR(2) = A(3,1)*A(1,1)*T(1,1) + A(3,1)*A(1,2)*T(1,2)
     1         + A(3,2)*A(1,2)*T(2,1) + A(3,2)*A(1,2)*T(2,2)
      SHEAR(3) = A(3,1)*A(2,1)*T(1,1) + A(3,1)*A(2,2)*T(1,2)
     1         + A(3,2)*A(2,1)*T(2,1) + A(3,2)*A(2,2)*T(2,2)
      DO 39 I = 1,3
      ISHEAR = ISHEAR + 1
      REC2(NORM  ) = NORMAL(I)
      REC2(ISHEAR) = SHEAR(I)
      NORM = NORM + 1
   39 CONTINUE
      IEL  = IEL + 8
      IF (ITYPE.EQ.9 .OR. ITYPE.EQ.16) GO TO 50
   40 CONTINUE
   50 CALL WRITE (NEWOES,IELMT,1,0)
      CALL WRITE (NEWOES,REC2,NWDS,0)
      GO TO 25
C
C     CLOSE RECORD
C
   55 CALL FREAD (OES1,0,0,1)
   56 CALL WRITE (NEWOES,0,0,1)
      GO TO 10
C
C     SKIP ELEMENT
C
   60 CALL READ  (*100,*56,OES1,IELMT,1,0,M)
      CALL FREAD (OES1,REC2,NWDS,0)
      GO TO 29
  100 CONTINUE
  110 IF (IRDECT .GT. 0) CALL BCKREC (ECT2)
      CALL BCKREC (OES1)
      CALL CLOSE  (NEWOES,1)
      RETURN
      END
