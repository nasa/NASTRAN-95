      SUBROUTINE ELELBL (GPLST,X,U,DEFORM,BUF1)
C
      LOGICAL         SOLID
      INTEGER         BLANK    ,BUF1     ,BR       ,Q4       ,T3      ,
     1                CHR      ,CREW     ,DEFORM   ,HB       ,TWOD    ,
     2                ECT      ,ELID     ,ELIDP(2) ,ELSETS   ,ELTYPE  ,
     3                GPTS     ,GPLST(1) ,LBL(10)  ,LBLP(8)  ,RDREW   ,
     4                PID      ,PLABEL   ,PLTFLG   ,PSET     ,OFFSET
      REAL            INFNTY   ,LEN      ,MA       ,MAXLEN   ,MB      ,
     1                MINSLP   ,X(3,1)   ,U(2,1)
      COMMON /BLANK / SKP(3),PLTFLG,SKP1(6),SKP2(2),ELSETS,CASECC(5),ECT
      COMMON /SYSTEM/ KSYSTM(40),NCPW
      COMMON /NAMES / NOREW,RDREW,SKPN(2),CREW
      COMMON /GPTA1 / NTYPS,LAST,INCR,NE(1)
      COMMON /CHAR94/ CHR(60)
      COMMON /PLTDAT/ SKPPLT(20),SKPA(3),CNTCHR(2)
      COMMON /DRWDAT/ PSET,PLABEL
      COMMON /PLTSCR/ NCOR,XY(2,8),GPTS(4)
C
      DATA   BLANK  , INFNTY,SLPMAX   / 1H ,1.E3,5. /, PID / 4      /,
     1       ITETRA / 2HTE   /, IECT  / 4HECT   / , HB     / 2HHB   /,
     2       BR     / 2HBR   /, Q4    / 2HQ4    /,  T3     / 2HT3   /
C
      NP = 0
      CALL TIPE (0,0,0,0,0,-1)
      CNTX = CNTCHR(1)
      CNTY = CNTCHR(1) + (CNTCHR(2) - CNTCHR(1))/2.
C
C   . CHECK IF PROPERTY ID IS TO BE TYPED NEXT TO ELEMENT ID
C
      IF (PLABEL .NE. PID) GO TO 40
      IF (PLTFLG .LT.   0) GO TO 10
      CALL PRELOC (*10,GPLST(BUF1),ECT)
      CALL FNAME  (ECT,GPTS(1))
      IF (GPTS(1) .EQ. IECT) GO TO 20
      CALL CLOSE (ECT,CREW)
   10 PLABEL = PID - 1
      GO TO 40
   20 CALL DELSET
      LPID = 0
C
C   . READ THE ELEMENT TYPE + NUMBER OF GRID POINTS / ELEMENT OF THIS
C     TYPE.
C
   30 IF (LPID .GT. 0) CALL FWDREC(*40,ECT)
   40 CALL READ (*200,*200,ELSETS,ELTYPE,1,0,I)
      CALL FREAD (ELSETS,NGPEL,1,0)
      TWOD = 0
      IF (NGPEL .GT. 2) TWOD = 1
      NGPEL = IABS (NGPEL)
      SOLID =.FALSE.
      IF ((ELTYPE.EQ.ITETRA .OR. NGPEL.GT.4) .AND. ELTYPE.NE.HB)
     1     SOLID = .TRUE.
C-----
C   . REJECT ELEMENTS WITH 0 OR MORE THAN --NCOR-16-- GRID POINTS
C
      IF (NGPEL.GT.1 .AND. NGPEL.LT.NCOR-13) GO TO 60
   50 CALL FREAD (ELSETS,ELID,1,0)
      IF (ELID .LE. 0) GO TO 40
      CALL FREAD (ELSETS,0,-1,0)
      CALL FREAD (ELSETS,0,-NGPEL,0)
      GO TO 50
   60 CONTINUE
C-----
      IF (PLABEL .NE. PID) GO TO 90
      J = 16
      DO 70 I = 1,NTYPS
      IF (NE(J) .EQ. ELTYPE) GO TO 80
   70 J = J + INCR
      GO TO 90
   80 LPID = J - 12
      IF (NE(LPID+2) .LE. 0) GO TO 90
      NPID = NE(LPID+2)
      CALL LOCATE (*90,GPLST(BUF1),NE(LPID),GPTS(1))
      GO TO 100
   90 LPID = 0
C
  100 NGPEL1 = NGPEL + 1
      OFFSET = 0
      IF (ELTYPE .EQ. BR) OFFSET = 6
      IF (ELTYPE.EQ.Q4 .OR. ELTYPE.EQ.T3) OFFSET = 1
C
C     READ AN ELEMENT ID + ITS GRID POINTS.
C
  102 CALL FREAD (ELSETS,ELID,1,0)
      IF (ELTYPE .EQ. HB) NGPEL = 8
      IF (ELID   .LE.  0) GO TO 30
      CALL FREAD (ELSETS,0,-1,0)
      CALL FREAD (ELSETS,GPTS(1),NGPEL,0)
      IF (OFFSET .GT. 0) CALL FREAD (ELSETS,0,-OFFSET,0)
      IF (ELTYPE .NE. HB) GO TO 1028
      DO 1023 I = 2,4
      IF (GPTS(I)) 1023,1025,1023
 1023 CONTINUE
      I = 5
 1025 NGPEL = I - 1
 1028 CONTINUE
      K = ELID
      NL = 0
      DO 103 I = 1,8
      J = ELID/10**(8-I)
      IF (J.EQ.0 .AND. NL.EQ.0) GO TO 103
      NL = NL + 1
      LBL(NL) = CHR(J+1)
      ELID = ELID - J*10**(8-I)
  103 CONTINUE
      LBL(NL+1) = KHRFN1(BLANK,1,ELTYPE,1)
      LBL(NL+2) = KHRFN1(BLANK,1,ELTYPE,2)
      NL = NL + 2
C
C   . DECODE PROPERTY ID
C
      IF (LPID .LE. 0) GO TO 105
 1040 CALL READ (*1041,*1041,ECT,ELIDP,2,0,I)
      CALL FREAD (ECT,0,-(NPID-2),0)
      IF (ELIDP(1) .EQ. K) GO TO 1042
      GO TO 1040
 1041 LPID = -1
      GO TO 105
C
C   . ELEMENT PROPERTY FOUND
C
 1042 K  = 10000000
      NP = 0
      DO 1043 I = 1,8
      J = ELIDP(2)/K
      IF (J.EQ.0 .AND. NP.EQ.0) GO TO 1043
      NP = NP + 1
      LBLP(NP) = CHR(J+1)
      ELIDP(2) = ELIDP(2) - J*K
 1043 K = K/10
C
  105 CONTINUE
C
C   . SET UP THE COORDINATES OF THE GRID POINTS
C
      DO 108 I = 1,NGPEL
      J = GPTS(I)
      J = IABS(GPLST(J))
      IF (DEFORM .NE. 0) GO TO 106
      XX = X(2,J)
      YY = X(3,J)
      GO TO 107
  106 XX = U(1,J)
      YY = U(2,J)
  107 IF (SOLID) GO TO 1071
      XY(1,I) = XX
      XY(2,I) = YY
      J = NGPEL + I
      XY(1,J) = XX
      XY(2,J) = YY
      GO TO 108
 1071 IF (I .GT. 2) GO TO 1072
      XY(1,I) = XX
      XY(2,I) = YY
      IF (I .NE. 1) GO TO 1072
      XY(1,3) = 0.0
      XY(2,3) = 0.0
 1072 XY(1,3) = XX + XY(1,3)
      XY(2,3) = YY + XY(2,3)
  108 CONTINUE
C
      IF (SOLID) GO TO 160
      IF (TWOD  .NE. 0) GO TO 110
      IF (NGPEL .EQ. 2) GO TO 125
      K = 3
      GO TO 120
C
C     FIND THE BASE OF THIS POLYGON = LONGEST SIDE (IF MORE THAN ONE
C     LONGEST SIDE, CHOOSE FROM THEM THE SIDE OF SMALLEST SLOPE).
C
  110 MAXLEN = 0.
      DO 116 I = 1,NGPEL
      XX = XY(1,I+1) - XY(1,I)
      YY = XY(2,I+1) - XY(2,I)
      LEN = XX**2 + YY**2
      IF (XX .NE. 0.) GO TO 111
      SLP = INFNTY
      GO TO 112
  111 SLP = ABS(YY/XX)
  112 IF (MAXLEN-LEN) 113,114,116
  113 MAXLEN = LEN
      GO TO 115
  114 IF (SLP .GE. MINSLP) GO TO 116
  115 K = I
      MINSLP = SLP
  116 CONTINUE
C
      IF (K .EQ. 1) GO TO 122
  120 DO 121 I = 1,NGPEL1
      XY(1,I) = XY(1,K)
      XY(2,I) = XY(2,K)
      K = K + 1
  121 CONTINUE
  122 IF (NGPEL .EQ. 6) GO TO 140
      IF (NGPEL-3) 125,140,150
C
C     LINE ELEMENT.
C
  125 XX = XY(1,2) - XY(1,1)
      IF (XX .EQ. 0.) GO TO 126
      YY = XY(2,2) - XY(2,1)
      SLP= YY/XX
      GO TO 127
  126 SLP= INFNTY
  127 XC = (XY(1,1) + XY(1,2))/2.
      YC = (XY(2,1) + XY(2,2))/2.
C
      IF (ABS(SLP)-1.) 128,128,129
  128 YC = YC + CNTY
      GO TO 175
  129 IF (ABS(SLP)-SLPMAX) 130,131,131
  130 XC = XC - SIGN(CNTX,SLP)
      GO TO 175
  131 XC = XC + CNTX
      GO TO 175
C
C     TRIANGULAR ELEMENT.  POINTS 1+2 ARE THE BASE - POINT 3 THE APEX.
C
  140 XC = (XY(1,1) + XY(1,2) + XY(1,3))/3.
      YC = (XY(2,1) + XY(2,2) + XY(2,3))/3.
      GO TO 175
C
C     QUADRILATERAL ELEMENT.
C
  150 XX = (XY(1,3)+XY(1,4)) - (XY(1,1)+XY(1,2))
      IF (XX .NE. 0.) GO TO 151
      MA = INFNTY
      GO TO 152
  151 YY = (XY(2,3)+XY(2,4)) - (XY(2,1)+XY(2,2))
      MA = YY/XX
      BA = (XY(2,1)+XY(2,2))/2. - MA*(XY(1,1)+XY(1,2))/2.
  152 XX = (XY(1,2)+XY(1,3)) - (XY(1,1)+XY(1,4))
      IF (XX .NE. 0.) GO TO 153
      MB = INFNTY
      GO TO 155
  153 YY = (XY(2,2)+XY(2,3)) - (XY(2,1)+XY(2,4))
      MB = YY/XX
      BB = (XY(2,1)+XY(2,4))/2. - MB*(XY(1,1)+XY(1,4))/2.
C
  155 IF (ABS(MA) .GE. INFNTY) GO TO 156
      IF (ABS(MB) .GE. INFNTY) GO TO 157
      IF (MB .EQ. MA) GO TO 158
      XC = (BA-BB)/(MB-MA)
      YC = MA*XC + BA
      GO TO 175
  156 XC = (XY(1,1) + XY(1,2))/2.
      YC = MB*XC + BB
      GO TO 175
  157 XC = (XY(1,1) + XY(1,4))/2.
      YC = MA*XC + BA
      GO TO 175
  158 XC = (XY(1,3) + XY(1,4) + XY(1,2)+XY(1,1))/4.0
      YC = (XY(2,3) + XY(2,4) + XY(2,2)+XY(2,1))/4.0
      GO TO 175
C
C   . ELEMENTS WITH MORE THAN FOUR GRIDS
C
  160 XC = XY(1,3)/FLOAT(NGPEL)
      YC = XY(2,3)/FLOAT(NGPEL)
      GO TO 175
C
C     SETUP THE STRAIGHT LINE EQUATION OF THE LINE ON WHICH THE ELEMENT
C     LABEL IS TO BE TYPED - Y=MX+B.
C
  175 XX = XY(1,2) - XY(1,1)
      IF (XX .EQ. 0.) GO TO 176
      YY = XY(2,2) - XY(2,1)
      SLP= YY/XX
      B  = YC - XC*SLP
      GO TO 180
  176 SLP= INFNTY
C
C     TYPE THE ELEMENT LABEL (NL CHARACTERS)
C
  180 ZZ = NL/2
      IF (NL/2 .EQ. (NL+1)/2) ZZ = ZZ - .5
      ABSSLP = ABS(SLP)
      CC = CNTX
      IF (ABSSLP .GE. SLPMAX) CC = CNTY
      K = MAX0(NL,NP)
C
      DO 191 I = 1,K
      XX = CC*(ZZ - FLOAT(I-1))
      IF (ABSSLP .GT. 1.) GO TO 181
      XX = XC - XX
      YY = SLP*XX + B
      GO TO 190
  181 IF (ABSSLP .GE. SLPMAX) GO TO 182
      YY = SIGN(1.,SLP)
      GO TO 183
  182 YY = -1.
  183 YY = YC - YY*XX
      IF (ABSSLP .GE. INFNTY) GO TO 184
      XX = (YY-B)/SLP
      GO TO 190
  184 XX = XC
C
C
C     OFFSET THE HB LABEL AND PROPERTY ID IF ANY WHEN TIPE LABEL
C
  190 IF (ELTYPE .NE. HB) GO TO 1905
      JTJ = 2
      IF (ABSSLP .LT. SLPMAX) YY = YY - JTJ*CC
      IF (ABSSLP .GE. SLPMAX) XX = XX + JTJ*CC
 1905 IF (NL .GE. I) CALL TIPE (XX,YY,1,LBL(I),1,0)
      IF (LPID .LE. 0) GO TO 191
      IF (NP   .LT. I) GO TO 191
      IF (ABSSLP .LT. SLPMAX) YY = YY - 2.*CC
      IF (ABSSLP .GE. SLPMAX) XX = XX + 2.*CC
      CALL TIPE (XX,YY,1,LBLP(I),1,0)
  191 CONTINUE
      GO TO 102
C
  200 CALL TIPE (0,0,0,0,0,1)
      IF (PLABEL .EQ. PID) CALL CLOSE (ECT,CREW)
      RETURN
      END
