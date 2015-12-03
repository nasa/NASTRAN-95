      SUBROUTINE SCALEX(ILVAL,CODE,L)
      INTEGER L(1),CODE,EXPND(6)
      DO 101 I=1,6
  101 L(I)=0
      IF(CODE) 102,102,103
  102 L(1) = ILVAL
      GO TO 110
  103 ID=CODE
      DO 104 I=1,6
      INV=7-I
      EXPND(INV)=MOD(ID,10)
  104 ID=ID/10
      J=0
      DO 107 I=1,6
      IF(EXPND(I).EQ.0) GO TO 107
      IF(I.LT.2) GO TO 106
      II=I-1
      DO 105 K=1,II
      IF(EXPND(K).EQ.EXPND(I)) GO TO 107
  105 CONTINUE
  106 J=J+1
      L(J)=EXPND(I)
  107 CONTINUE
      I=0
  108 I=I+1
      L(I)=ILVAL+L(I)-1
      IF(I-J) 108,110,110
  110 RETURN
      END
