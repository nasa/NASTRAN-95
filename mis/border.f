      SUBROUTINE BORDER (GPLST,X,U,ISTORE,DEFORM,B1,OPCOR)
C
      INTEGER        GPLST(1),DEFORM,OPCOR,SCR2,WORDS(2),ELID,B1,SCR4
      REAL           X(3,1),U(2,1)
      DIMENSION      PT(2,3),ISTORE(2)
      COMMON /BLANK/ SKIP(25),SCR2,SCR3,SCR4
      EQUIVALENCE    (WORDS(1),NELMT),(WORDS(2),IGDPT)
C
      LCOR = OPCOR/5 - 1
      CALL OPEN (*150,SCR2,GPLST(B1),0)
      CALL LINE (0.,0.,0.,0.,1,-1)
    9 CALL FWDREC (*100,SCR2)
   10 CALL READ (*100,*9,SCR2,IFLAG,1,0,M)
      IF (IFLAG .EQ.  0) GO TO 100
      IF (IFLAG .EQ. -1) GO TO 9
      CALL FREAD (SCR2,WORDS,2,0)
      IE = -1
   20 IE = IE + 2
      CALL READ (*100,*30,SCR2,ELID,1,0,M)
      CALL FREAD (SCR2,ISTORE(IE),2,0)
      GO TO 20
   30 IONE = ISTORE(1)
      ITWO = ISTORE(2)
      IF (NELMT .EQ. 1) GO TO 50
      IE = 2*NELMT
      IE1= IE - 1
      DO 37 I = 1,IE1
      IF (ISTORE(I) .EQ. 0) GO TO 37
      IP1 = I + 1
      DO 36 J = IP1,IE
      IF (ISTORE(I) .NE. ISTORE(J)) GO TO 36
      ISTORE(I) = 0
      ISTORE(J) = 0
      GO TO 37
   36 CONTINUE
   37 CONTINUE
      J = 0
      DO 40 I = 1,IE
      IF (ISTORE(I) .EQ. 0) GO TO 40
      J = J + 1
      IF (J-1) 38,38,39
   38 IONE = ISTORE(I)
      GO TO 40
   39 ITWO = ISTORE(I)
   40 CONTINUE
      IF (J .EQ. 0) GO TO 10
   50 IG = IABS(GPLST(IGDPT))
      IF (DEFORM .NE. 0) GO TO 57
      PT(1,3) = X(2,IG)
      PT(2,3) = X(3,IG)
      GO TO 60
   57 PT(1,3) = U(1,IG)
      PT(2,3) = U(2,IG)
   60 IG = IONE
      DO 65 I = 1,2
      IG = IABS(GPLST(IG))
      IF (DEFORM .NE. 0) GO TO 63
      PT(1,I) = X(2,IG)
      PT(2,I) = X(3,IG)
      GO TO 64
   63 PT(1,I) = U(1,IG)
      PT(2,I) = U(2,IG)
   64 CALL LINE (PT(1,I),PT(2,I),PT(1,3),PT(2,3),1,0)
      IG = ITWO
   65 CONTINUE
      GO TO 10
  100 CALL LINE (0.,0.,0.,0.,1,+1)
      CALL CLOSE (SCR2,1)
  150 RETURN
      END
