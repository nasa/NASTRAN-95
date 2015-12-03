      SUBROUTINE RCOVMS
C
C     THIS ROUTINE GENERATES THE MODAL SOLUTION ITEM FOR RIGID FORMAT 3
C
      LOGICAL         MRECVR
      INTEGER         DRY        ,STEP       ,FSS        ,RFNO       ,
     1                SOLN       ,RC         ,SWRT       ,SRD        ,
     2                EOG        ,EOI        ,BUF1       ,Z          ,
     3                RD         ,RDREW      ,WRT        ,WRTREW     ,
     4                REW        ,NAME(2)    ,FILE       ,SOF3
      COMMON /BLANK / DRY        ,LOOP       ,STEP       ,FSS(2)     ,
     1                RFNO       ,NEIGV      ,LUI        ,UINMS(2,5) ,
     2                NOSORT     ,UTHRES     ,PTHRES     ,QTHRES
      COMMON /RCOVCR/ ICORE      ,LCORE      ,BUF1       ,BUF2       ,
     1                BUF3       ,BUF4       ,SOF1       ,SOF2       ,
     2                SOF3
      COMMON /RCOVCM/ MRECVR     ,UA         ,PA         ,QA         ,
     1                IOPT       ,RSS(2)     ,ENERGY     ,UIMPRO     ,
     2                RANGE(2)   ,IREQ       ,LREQ       ,LBASIC
      COMMON /NAMES / RD         ,RDREW      ,WRT        ,WRTREW     ,
     1                REW        ,NOREW      ,EOFNRW
      COMMON /ZZZZZZ/ Z(1)
      DATA    LAMS  , SOLN /   4HLAMS,4HSOLN   /
      DATA    SRD   , SWRT,EOG,EOI   / 1,2,2,3 /
      DATA    LAMA  / 102  /,  I7    / 7 /
      DATA    NAME  / 4HRCOV,  4HMS      /
C
C
C     CREATE SOLN FOR RIGID FORMAT 3
C
      IF (MRECVR) GO TO 500
C
C     WRITE GROUP 0
C
      RC = 3
      CALL SFETCH (FSS,SOLN,SWRT,RC)
      CALL SUWRT (FSS,2,1)
      CALL SUWRT (RFNO,1,1)
      CALL SUWRT (NEIGV,1,EOG)
C
C     IF NO EIGENVALUES, GO HOME
C
      IF (NEIGV .LE. 0) GO TO 430
C
C     COPY RECORD 2 OF LAMA OR CLAMA TO GROUP 1 OF SOLN
C
      FILE = LAMA
      CALL OPEN (*9001,LAMA,Z(BUF1),RDREW)
      CALL FWDREC (*9002,LAMA)
      CALL FREAD (LAMA,ITYPE,1,1)
      NW = 7
      IF (ITYPE .EQ. 90) NW = 6
      Z(I7) = 0
      I = 1
  410 CALL READ (*9002,*420,LAMA,Z(1),NW,0,NWDS)
      CALL SUWRT (Z,7,I)
      GO TO 410
  420 CALL SUWRT (0,0,EOG)
      CALL CLOSE (LAMA,REW)
C
C     FINISH
C
  430 CALL SUWRT (0,0,EOI)
      RETURN
C
C     FOR MODAL RECOVER COPY THE LAMS ITEM TO SOLN
C
  500 CALL SFETCH (FSS,LAMS,SRD,RC)
      IF (RC .NE. 1) GO TO 6000
      CALL SUREAD (Z(1),-2,N,RC)
      IF (N .GT. SOF3) GO TO 9008
      CALL SFETCH (FSS,SOLN,SWRT,RC)
      CALL SUWRT (Z(1),N,EOI)
      RETURN
C
C     ERROR RETURNS
C
 6000 CALL SMSG (RC-2,LAMS,FSS)
      GO TO 9200
 9001 N = 1
      GO TO 9100
 9002 N = 2
      GO TO 9100
 9008 N = 8
      GO TO 9100
 9100 CALL MESAGE (N,FILE,NAME)
 9200 CALL SOFCLS
      IOPT = -1
      CALL CLOSE (LAMA,REW)
      RETURN
      END
