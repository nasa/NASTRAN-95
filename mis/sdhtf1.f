      SUBROUTINE SDHTF1 (TYPE,REJECT)
C
C     THIS ROUTINE CONVERTS THE EST DATA FOR ALL THERMAL ELEMENTS TO A
C     COMMON FORMAT. SDHT1B IS CALLED TO PRODUCE THE OUTPUT
C
      LOGICAL         REJECT
      INTEGER         ELID,SUB,SIL,NESTO(100),ELEM,NEST(2),TYPE,
     1                POINTR(8,23),TYPOLD,STRSPT,ESTWDS,NESTSC(200),
     2                POINT1(8,20),POINT2(8, 3),TUBE,FTUBE,CHBDY
      DIMENSION       SHP(32),DSHP(3,32),XJACOB(3,3),BXYZ(3,32),GPT(32)
      COMMON /CONDAS/ CONSTS(5)
      COMMON /SDR2X4/ DUMX(109),STRSPT,DDRMM,ISOPL8
      COMMON /SDR2X5/ EST(100),ELID,SIL(32),NQ,NP,NAME(2),DOMX(105),
     1                DSHPB(3,32)
      COMMON /SDR2X6/ SUB,IMAT,AF,THETA,R(3,32),ESTSCR(200)
      COMMON /GPTA1 / NELS,LAST,INCR,ELEM(1)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,DUM(1),SINTH,COSTH
      EQUIVALENCE     (CONSTS(1),PI),(NESTSC(1),ESTSCR(1)),
     1                (NESTO(1),SUB),(NEST(1),EST(1)),
     2                (POINT1(1,1),POINTR(1,1)),
     3                (POINT2(1,1),POINTR(1,21))
      DATA    TYPOLD, NUMELT, TUBE, FTUBE, CHBDY /
     1             0,     23,    3,    82,    52 /
C     DATA    HEX   / 16    /
C
C     THE POINTERS TO THE EST DATA ARE
C
C        IM    MAT ID
C        ITH   THETA
C        IA    AREA
C        IG    GRID POINT DATA
C        IS    SIL MINUS 1
C        NP    NO. OF POINTS
C        SUB   SUBROUTINE TYPE
C                       NO.  IS   ITH  IM   IA   IG   NP   SUB
C                      ----  --   ---  --   --   --   --   ----
      DATA   POINT1 /    1   ,0   ,0   ,4   ,5   ,9   ,2   ,1
     2                  ,3   ,0   ,0   ,4   ,5   ,8   ,2   ,1
     3                  ,6   ,0   ,5   ,6   ,7   ,15  ,3   ,2
     4                  ,9   ,0   ,5   ,6   ,7   ,9   ,3   ,2
     5                  ,10  ,0   ,0   ,4   ,5   ,9   ,2   ,1
     6                  ,16  ,0   ,6   ,7   ,8   ,10  ,4   ,3
     7                  ,17  ,0   ,5   ,6   ,7   ,9   ,3   ,2
     8                  ,18  ,0   ,6   ,7   ,8   ,10  ,4   ,3
     9                  ,19  ,0   ,6   ,7   ,8   ,16  ,4   ,3
     T                  ,34  ,0   ,0   ,16  ,17  ,34  ,2   ,1
     1                  ,36  ,0   ,5   ,6   ,0   ,7   ,3   ,4
     2                  ,37  ,0   ,6   ,7   ,0   ,8   ,4   ,5
     3                  ,39  ,1   ,0   ,2   ,0   ,7   ,4   ,6
     4                  ,40  ,1   ,0   ,2   ,0   ,9   ,6   ,7
     5                  ,41  ,1   ,0   ,2   ,0   ,11  ,8   ,8
     6                  ,42  ,1   ,0   ,2   ,0   ,11  ,8   ,9
     7                  ,52  ,1   ,0   ,15  ,16  ,21  ,8   ,10
     8                  ,62  ,0   ,6   ,7   ,8   ,10  ,4   ,3
     9                  ,63  ,0   ,6   ,7   ,8   ,10  ,4   ,3
     T                  ,65  ,0   ,0   ,10  ,0   ,16  ,8   ,16 /
      DATA   POINT2 /    66  ,0   ,0   ,22  ,0   ,28  ,20  ,16
     2                  ,67  ,0   ,0   ,34  ,0   ,40  ,32  ,16
     3                  ,76  ,0   ,11  ,12  ,13  ,14  ,8   ,17 /
C
      IF (TYPE .EQ.  FTUBE) GO TO 115
      IF (TYPE .EQ. TYPOLD) GO TO 50
      TYPOLD = TYPE
      REJECT = .TRUE.
      DO 20 I = 1,NUMELT
      IEL = I
      IF (TYPE-POINTR(1,I)) 30,40,20
   20 CONTINUE
   30 RETURN
C
   40 REJECT = .FALSE.
   50 IF ((TYPE.GE.65.AND.TYPE.LE.67) .AND. STRSPT.EQ.0)
     1     STRSPT = STRSPT + 1
      IP = (TYPE-1)*INCR
      ESTWDS = ELEM(IP+12)
C
C     THE LOCATIONS OF DATA FOR EACH PARTICULAR ELEMENT ARE ZEROED OUT
C
      NQ = 0
      DO 55 I = 1,100
   55 NESTO(I) = 0
      NAME(1) = ELEM(IP+1)
      NAME(2) = ELEM(IP+2)
      ELID = NEST(1)
      DO 56 I = 1,32
   56 NEST(I+101) = 0
      DO 57 I = 1,201
   57 NEST(I+137) = 0
      IF (TYPE .EQ. TUBE) EST(5) = PI*ESTSCR(6)*(ESTSCR(5)-ESTSCR(6))
      IF (TYPE.EQ.CHBDY .AND. NESTSC(2).EQ.7)
     1   EST(16) = PI*(ESTSCR(19)+ESTSCR(20))
      IS  = POINTR(2,IEL)
      ITH = POINTR(3,IEL)
      IM  = POINTR(4,IEL)
      IA  = POINTR(5,IEL)
      IG  = POINTR(6,IEL)
      SUB = POINTR(8,IEL)
      NP  = POINTR(7,IEL)
C
      IF (SUB .EQ. 10) SUB = SUB + NESTSC(2) - 1
      INFLAG = 1
      IF (SUB.GE.16) INFLAG = 3
      IF (SUB.LT.2 .OR. SUB.GT.5) GO TO 60
      INFLAG = 2
      GO TO 70
   60 IF (SUB.LT.6 .OR. SUB.GT.9) GO TO 70
      INFLAG = 3
   70 CONTINUE
      IF (SUB .NE. 16) GO TO 79
C
C     GET SHAPE FUNCTIONS ETC. FOR STRESS POINT(ALSO DETERMINE THE
C     STRESS POINT, WHICH WILL BE THE GRID POINTS PLUS CENTROID IN
C     ELEMENT COORDINATES
C
      ITYPE = TYPE - 64
      DO 71 I = 1,NP
      GPT(I) = ESTSCR(5*NP+7+I)
      DO 71 J = 1,3
      BXYZ(J,I) = ESTSCR(NP+4+4*I+J)
   71 CONTINUE
C
C     GET STRESS POINT
C
      Y =-1.
      Z =-1.
      IF (ITYPE .GT. 1) GO TO 502
      D = 2.
      X = 1.
      GO TO 505
  502 D = 1.
      X = 0.
  505 IF (ITYPE .GT. 1) GO TO 560
      GO TO (510,520,530,510,540,520,530,510,550), STRSPT
  510 X = X - D
      GO TO 590
  520 X = X + D
      GO TO 590
  530 Y = Y + D
      GO TO 590
  540 Z = Z + D
      Y = -1.
      GO TO 590
  550 X = 0.
      Y = 0.
      Z = 0.
      GO TO 590
  560 GO TO (510,520,520,530,530,510,510,570,580,520,
     1       530,510,580,520,520,530,530,510,510,570,
     2       550), STRSPT
  570 Y = Y - D
      GO TO 590
  580 Z = Z + 1.
      Y = -1.
      D = 3. - D
  590 CALL IHEXSS (ITYPE,SHP,DSHP,XJACOB,DETJ,ELID,X,Y,Z,BXYZ)
C
C     GET DERIVATIVES W.R.T.X,Y,Z(REVERSE CALLING SEQUENCE BECAUSE
C     COLUMN-STORED
C
      CALL GMMATS (DSHP,NP,3,0,XJACOB,3,3,0,DSHPB)
C
   79 CONTINUE
C
      IF (IA .GT. 0) AF = ESTSCR(IA)
      MATID = NESTSC(IM)
      IF (MATID .LE. 0) RETURN
      SINTH = 0.0
      COSTH = 1.0
      IF (INFLAG .NE. 2) GO TO 80
      THETA = ESTSCR(ITH)*PI/180.
      IF (THETA .EQ. 0.0) GO TO 80
      SINTH = SIN(THETA)
      COSTH = COS(THETA)
   80 ITEMP = IG + 4*NP
      ELTEMP= ESTSCR(ITEMP)
      IF (SUB .NE. 16) GO TO 85
      ISOPL8= 8
      ELTEMP= 0.
      DO 82 I = 1,NP
   82 ELTEMP= ELTEMP + GPT(I)*SHP(I)
   85 CONTINUE
      IMAT  = MATID
      CALL HMAT (ELID)
C
      DO 110 I = 1,NP
      IP = 4*(I-1) + IG
      DO 100 J = 1,3
      ILOC = IP + J
  100 R(J,I) = ESTSCR(ILOC)
      ISIL   = IS + I + 1
      SIL(I) = NESTSC(ISIL)
  110 CONTINUE
C
      CALL SDHTFF
      GO TO 120
C
C     FTUBE CONVECTION ELEMENT
C
  115 REJECT =.FALSE.
      I = 0
      NEST(I+101) = NESTSC(1)
      NEST(I+102) = NESTSC(2)
      NEST(I+103) = NESTSC(3)
      EST (I+104) = ESTSCR(4)*ESTSCR(5)
      EST (I+105) = 0.0
C
  120 RETURN
      END
