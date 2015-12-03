      SUBROUTINE COMBO (CDATA,NX,EXTRA,NNAM,NAME,NN,VAR,IER)
C
C     THIS ROUTINE  PROCESSES THE  COMBINE INPUT.
C        THE  INPUT/ OUTPUTS  ARE
C
C                  CDATA  -  XRCARD  IMAGE OF  COMBINE CARD  (IN)
C                  NX     -  NUMBER OF EXTRAS                (IN)
C                  EXTRA  -  3 BY NX ARRAY OF EXTRAS         (IN)
C                  NNAM   -  NUMBER OF CURRENT SUBS NAMES (IN/OUT)
C                  NAMES  -  ARRAY OF  CURRENT SUBS NAMES (IN/OUT)
C                  NN     -  NUMBER OF SUBS TO BE COMBINED  (OUT)
C                  VAR    -  3 BY NVAR ARRAY OF VARIABLES   (OUT)
C                            ARRANGED AS- KEY WORD + 2 DATA WORDS
C
C
C
C
      EXTERNAL RSHIFT   ,COMPLF
C
      INTEGER  CDATA(5) ,EXTRA(3,1)    ,NAME(2,1) ,VAR( 3,2)
      INTEGER  RSHIFT   ,COMPLF        ,EQSN
C
      DIMENSION   INUM(7)   ,NUMBS(7) ,MOPT(3)   ,MSORT(3)  ,NAI(7)
C
      DATA   INUM   / 4HN1  , 4HN2  ,4HN3  ,4HN4  ,4HN5  ,4HN6  ,4HN7  /
      DATA   LPRN,    NOPT,   NSORT, MOPT                ,MSORT        /
     1       4H(    , 4HOPTS, 4HSORT,4HAUTO,4HMAN ,4HREST,4HX   ,4HY   ,
     2       4HZ    /
      DATA   MANU   / 4HMANU/
      DATA   NNO    / 4HNAME/ ,NNC /4HNAMC/, NAMS /4HNAMS/
      DATA   NAI    / 4HNA1 ,4HNA2 ,4HNA3  ,4HNA4 ,4HNA5 ,4HNA6 ,4HNA7 /
      DATA   NCNO   / 4HNCNO/
      DATA   EQSN   / 4H=   /
C
C
      LWORD  =  RSHIFT( COMPLF(0),1)
      IER    = 0
C     COMBINE OPERATION
C         PROCESS PRIMARY CARD -COMBINE( OPTS,SORT) = NAME1,NAME2, ETC
C     SET DEFAULTS
      DO 1210 I =1,150
 1210 VAR(I,1) = 0
      JNAM = 6
      VAR(1,1) = NOPT
      VAR(2,1) = MOPT(1)
      VAR(1,2) = NSORT
      VAR(2,2) = MSORT(1)
      IF( CDATA(5) .NE. LPRN) GO TO 1220
      K = 6
C
C     PROCESS  AUTO/MAN  OR XYZ
C
 1211 DO 1215 I =1,3
      IF (  CDATA(K) .NE. MOPT(I)) GO TO 1212
      VAR(2,1) = MOPT(I)
      GO TO 1216
 1212 IF ( CDATA(K) .NE. MANU) GO TO  1213
      VAR(2,1) = MOPT(2)
      GO TO 1216
 1213 IF (  CDATA(K) .NE. MSORT(I)) GO TO 1215
      VAR(2,2) = MSORT(I)
      GO TO 1216
 1215 CONTINUE
C     NOT VALID    ASSUME EQ SIGN OR NAME
C
      GO TO 1222
 1216 K  = K+2
      GO TO 1211
C
C     NO  OPTION
 1220 K = 4
C
C     CHECK FOR  EQ SIGN
 1222 IF ( CDATA( K+1) .EQ. EQSN)  K =K+2
C
C     PROCESS NAMES
      NN = 0
      DO 1235 I = 1,7
      KN = K + 2*I -2
      IF ( CDATA( KN) .EQ. LWORD) GO TO 1236
C
      VAR(1,I+2) = NAMS
      VAR(2,I+2) = CDATA(KN)
      VAR(3,I+2) = CDATA(KN+1)
C
C     FIND STRUCTURE NUMBER
      IF ( NNAM .EQ. 0 ) GO TO 1231
      DO 1230 J =1, NNAM
      IF ( CDATA(KN) .NE. NAME(1,J) .OR. CDATA(KN+1).NE.NAME(2,J))
     1     GO TO 1230
      NUMBS(I) = J
      GO TO 1232
 1230 CONTINUE
C
C     NEW NAME
C
 1231 NNAM  = NNAM +1
      NUMBS(I) = NNAM
      NAME(1,NNAM) = CDATA(KN)
      NAME(2,NNAM) = CDATA(KN+1)
 1232 NN= NN+1
 1235 CONTINUE
C
C
C     MOVE  EXTRAS INTO PLACE  CHANGE NAME TO NAMC
 1236 IC = 0
      DO 1240  J = 1,NX
      IX  = J +3*NN  +2
      IF ( EXTRA(1,J) .NE. NNO ) GO TO 1238
      EXTRA(1,J) = NNC
      IC = IX
 1238 DO 1240 K = 1,3
      VAR( K,IX) = EXTRA(K,J)
 1240 CONTINUE
C
C     SET  STRUCTURE NUMBER KEYS
C
      IF( NN .EQ. 0) GO TO 1248
C
      DO 1245  I = 1, NN
C
      IX =  I + NN  +2
      VAR(1,IX) = INUM(I)
      VAR(2,IX) = -1
      VAR(3,IX) = NUMBS(I)
      IY = IX+NN
      VAR(1,IY) = NAI(I)
      VAR(2,IY) = VAR(2,I+2)
      VAR(3,IY) = VAR(3,I+2)
 1245 CONTINUE
      GO TO 1250
 1248 IER = 1
C
C     CHECK  FOR NAMC AS A PREVIOUS NAME  OR MISSING
 1250 IF ( IC .EQ. 0) GO TO 1265
      DO 1260 J =1,NNAM
      IF (VAR(2,IC).NE. NAME(1,J).OR.VAR(3,IC).NE. NAME(2,J)) GO TO 1260
      GO TO 1265
 1260 CONTINUE
C
C     OK -NEW NAME , ADD TO LIST
C
      NNAM = NNAM+1
      NAME(1,NNAM) = VAR(2,IC)
      NAME(2,NNAM) = VAR(3,IC)
      IX = NX+3*NN+3
      VAR(1,IX) = NCNO
      VAR(2,IX) = -1
      VAR(3,IX) = NNAM
      RETURN
 1265 IER = IER +2
      RETURN
      END
