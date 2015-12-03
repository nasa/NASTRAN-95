      SUBROUTINE SSGHT1 (IEST,FILE,NEQUIV  )
C*****
C     THIS ROUTINE CONVERTS THE EST DATA FOR ALL THERMAL ELEMENTS TO A
C     COMMON FORMAT. OPTIONAL TASKS INCLUDE CALCULATING MATOUT DATA AND
C     CONVERTING  SIL VALUES TO UN VALUES.
C*****
      INTEGER  ELID, SUB, SIL, NESTO(45), ELEM, NEST(2),ZP, BUFM
      INTEGER  TYPE,FILE, POINTR(8,23), NEQUIV(1) ,SUBR(2),FLAG
      INTEGER POINT1(8,20),POINT2(8, 3)
      REAL     EST(100)
      LOGICAL  LINEAR
C
      COMMON/ CONDAS/ CONSTS(5)
      COMMON/ ESTOUT/ ELID,SUB,NAME(2),SIL(8),IMAT,AF,THETA,R(3,8),
     1                MATO(6)
      COMMON/  MATIN/ MATID,INFLAG,ELTEMP,DUM(1),SINTH,COSTH
      COMMON/ HMTOUT/ BUFM(7)
      COMMON/ GPTA1 / NELEMS, LAST, INCR, ELEM(1)
      COMMON/ HMATDD/ XXX(4), LINEAR
C
      EQUIVALENCE  (CONSTS(1) , PI     )
      EQUIVALENCE  (NESTO(1),ELID) ,( NEST(1), EST(1) )
      EQUIVALENCE  (POINT1(1,1),POINTR(1,1)), (POINT2(1,1),POINTR(1,21))
C
      DATA SUBR / 4HSSGH ,4HT1   /
      DATA NUMELT / 23 /
C*****
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
     2                  ,67  ,0   ,0   ,34  ,0   ,40  ,32  ,1
     3                  ,76  ,0   ,11  ,12  ,13  ,14  ,8   ,17 /
C*****
      CALL DELSET
   10 CALL READ(*120,*140,IEST,TYPE,1,0,FLAG)
      DO 20 I =1,NUMELT
      IEL = I
      IF (TYPE   - POINTR(1,I)) 30,40,20
   20 CONTINUE
   30 CALL FWDREC(*150,IEST)
      GO TO 10
C
   40 CONTINUE
      ZP =   (TYPE-1)*INCR
      NAME(1)= ELEM(ZP+1)
      NAME(2)= ELEM(ZP+2)
      NWORDS = ELEM(ZP+12)
   50 CONTINUE
      CALL READ(*150,*130,IEST,EST,NWORDS,0,FLAG)
      ELID = NEST(1)
      DO 55 I = 5,45
      NESTO(I) = 0
   55 CONTINUE
      IF( TYPE .EQ. 3) EST(5) = PI*EST(6)*(EST(5)-EST(6))
      IF( TYPE.EQ.52 .AND. NEST(2).EQ.7) EST(16)=PI*(EST(19)+EST(20))
      IS = POINTR(2,IEL)
      ITH= POINTR(3,IEL)
      IM = POINTR(4,IEL)
      IA = POINTR(5,IEL)
      IG = POINTR(6,IEL)
      SUB= POINTR(8,IEL)
      NP = POINTR(7,IEL)
C
      IF(SUB .EQ. 10) SUB = SUB + NEST(2)-1
      INFLAG =1
      IF( SUB .GE. 16) INFLAG=3
      IF( SUB .LT. 2 .OR. SUB .GT. 5)  GO TO 60
      INFLAG =2
      GO TO 70
   60 IF(SUB  .LT. 6 .OR. SUB .GT. 9)  GO TO 70
      INFLAG =3
   70 CONTINUE
      IF( IA.GT. 0) AF = EST(IA)
      MATID = NEST(IM)
      IF(MATID .LE. 0) GO TO 50
      SINTH=0.0
      COSTH=1.0
      IF( INFLAG .NE. 2) GO TO 80
      THETA= EST(ITH)*PI/180.0
      IF( THETA .EQ.0.0)GO TO 80
      SINTH= SIN(THETA)
      COSTH= COS(THETA)
   80 ITEMP = IG + 4*NP
      ELTEMP = EST(ITEMP)
      IMAT = MATID
      LINEAR=.FALSE.
      CALL HMAT( ELID )
C*****
C     TEST IF NONLINEAR
C*****
      IF( LINEAR ) GO TO 50
      DO 90 I=1,6
   90 MATO(I)= BUFM(I)
      DO 110 I=1,NP
      JPOINT = 4*(I-1) + IG
      DO 100 J=1,3
      ILOC = JPOINT + J
  100 R(J,I) = EST(ILOC)
      ISIL= IS+I + 1
      IPT    =NEST(ISIL)
      IF( IPT.EQ. 0) GO TO 110
      SIL(I) = NEQUIV(IPT)
  110 CONTINUE
C*****
C     WRITE A UNIFORM EST GROUP OF CONVERTED DATA HERE
C*****
      CALL WRITE(FILE,NESTO(1), 45, 0 )
C*****
C     RETURN FOR ANOTHER ELEMENT
C******
      GO TO 50
  120 RETURN
C*****
C     DONE WITH THIS ELEMENT TYPE
C*****
  130 IF( FLAG .EQ. 0) GO TO 10
C******
  140 J=-3
      GO TO 160
  150 J = -2
  160 CALL MESAGE(J,IEST,SUBR)
      RETURN
      END
