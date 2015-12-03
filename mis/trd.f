      SUBROUTINE TRD
C
C     TRANSIENT RESPONSE MODULE DRIVER
C
C     INPUTS   CASEXX,     TRL,NLFT,DIT,KHH,BHH,MHH,PH
C              CASEXX,     TRL,NLFT,DIT,KDD,BDD,MDD,PD
C
C     OUTPUTS  UDVT,PNLD
C              UHVT,PNLH
C
C     PARAMETERS -- MODAL --BCD--INPUT--MODAL=MODAL IMPLIES MODAL
C                   NOUE  --INT--INPUT--NUMBER OF EXTRA POINTS
C                   NONCUP--INT--INPUT--NONCUP=-1 IMPLIES NONCOUPLED
C                   NCOL  --INT--IN/OUT--APPEND FLAG 0  NO APPEND
C                                                    +  COL NUMBER OF
C                                                        LAST TIME STEP
C
C     SCRATCHES   --
C
C
C
C
C
C
C
      INTEGER            CASEXX    ,TRL      ,NLFT     ,DIT      ,
     1                   KDD       ,BDD      ,MDD      ,PD       ,
     1                   UDVT      ,PNLD     ,MODAL(2) ,SCR1     ,
     1                   SCR2      ,SCR3     ,SCR4     ,SCR5     ,
     1                   SCR6      ,SCR7     ,SCR8     ,
     1                   SR1       ,SR2      ,SR3      ,SR4      ,
     1                   SR5       ,SR6      ,NAME(2)  ,
     1                   IZ(1)
C
      COMMON /BLANK/  MODAL     ,NOUE     ,NONCUP   ,NCOL
      COMMON/SYSTEM/IBUF, IDUMMY(53), IPREC
      COMMON   /ZZZZZZ /  Z(1)
      COMMON   /TRDXX /  IK(7)     ,IM(7)    ,IB(7)    ,SR1      ,
     1                   SR2       ,SR3      ,SR4      ,SR5      ,
     2                   SR6       ,IOPEN    ,ISYM     ,TO       ,
     3                   NOPD      ,ISPNL
C
      EQUIVALENCE        ( Z(1)    ,IZ(1))
C
      DATA               CASEXX    ,TRL      ,NLFT     ,DIT      /
     1                   101       ,102      ,103      ,104      /
     1                   ,KDD      ,BDD      ,MDD      ,PD       /
     1                   105       ,106      ,107      ,108      /
     1                   ,UDVT     ,PNLD     ,SCR1     ,SCR2     /
     1                   201       ,202      ,301      ,302      /
     1                   ,SCR3     ,SCR4     ,SCR5     ,SCR6     /
     1                   303       ,304      ,305      ,306      /
     1                   ,SCR7     ,SCR8     ,         MODA      /
     1                   307       ,308      ,         4HMODA    /
     1                   ,NAME               /
     1                   4HTRD     ,4H       /
C
C ----------------------------------------------------------------------
C
C     INITIALIZE
C
      MODA1 = -1
      IF ( MODA .EQ. MODAL(1)) MODA1 = 1
C
C     BUILD INITIAL CONDITIONS
C
      IF (IPREC.EQ.1)
     1CALL TRD1A  (CASEXX, TRL, SCR1, NLFTP, NGROUP, MODA1)
      IF (IPREC.EQ.2)
     1CALL TRD1A2 (CASEXX, TRL, SCR1, NLFTP, NGROUP, MODA1)
C
C     TEST FOR ZERO APPLIED LOAD
C
      IK(1) = SCR1
      CALL RDTRL(IK(1))
      IF ( IK(6) .NE. 0) GO TO 10
      IF ( NLFTP .NE. 0) GO TO 10
      IK(1) = PD
      IK(6) = 0
      CALL RDTRL(IK)
      IF(  IK(6) .NE. 0) GO TO 10
      IF (NCOL.GT.0) GO TO 10
      CALL MESAGE(-46,0,0)
   10 CONTINUE
C
C     ESTIMATE CORE
C
      IF( NONCUP .LT. 0 .AND. MODAL(1) .EQ. MODA .AND. NLFTP .EQ. 0)
     1  GO TO 100
      NZ = KORSZ (Z)
      IGROUP = NZ- 3*NGROUP +1
      IK(1) = KDD
      CALL RDTRL( IK)
      IF( IK(1) .LT. 0) GO TO 20
      NROW = IK(3)
      GO TO 21
   20 IK(1) = 0
   21 IB(1) = BDD
      CALL RDTRL(IB)
      IF( IB(1) .LT. 0) GO TO 30
      NROW = IB(3)
      GO TO 31
   30 IB(1) = 0
   31 IM(1) = MDD
      CALL RDTRL(IM)
      IF( IM(1) .LT. 0) GO TO 35
      NROW = IM(3)
      GO TO 36
   35 IM(1) = 0
   36 CONTINUE
      ICRQ = 8*IBUF + 7*IPREC*NROW - IGROUP
      IF(ICRQ.GT.0) CALL MESAGE(-8,ICRQ,NAME)
C
C     SET UP COMMON
C
      SR1=SCR2
      SR2=SCR3
      SR3=SCR4
      SR4=SCR5
      SR5=SCR6
      SR6=SCR7
      ISKIP  = 1
      JGROUP = IGROUP
      DO 45 I = 1, NGROUP
      NSKIP = IZ(JGROUP+2)
      IF (NSKIP .EQ. 1) GO TO 40
      ISKIP = 0
      GO TO 47
   40 JGROUP = JGROUP + 3
   45 CONTINUE
   47 DO 50  I= 1,NGROUP
      CALL KLOCK(ITIME1)
      NSTEP = IZ(IGROUP)
      DELTA =  Z(IGROUP+1)
      IGROUP= IGROUP +3
      IF (IPREC.EQ.1) CALL INITL  (3*NGROUP, DELTA)
      IF (IPREC.EQ.2) CALL INITL2 (3*NGROUP, DELTA)
      CALL KLOCK(ITIME3)
      IF (IPREC.EQ.1)
     1CALL TRD1C  (SCR1, PD, NGROUP, NLFTP, UDVT, I, SCR8, DIT, NLFT,
     2             NOUE, MODA1, PNLD, ISKIP)
      IF (IPREC.EQ.2)
     1CALL TRD1C2 (SCR1, PD, NGROUP, NLFTP, UDVT, I, SCR8, DIT, NLFT,
     2             NOUE, MODA1, PNLD, ISKIP)
      CALL KLOCK (ITIME2)
      CALL TMTOGO(ITLEFT)
      IF( ITLEFT .LE. 0) GO TO 60
      IF(  I .EQ. NGROUP) GO TO 50
C
C     COMPUTE TIME TO DO NEXT ITERATION
C
      IF (2*(ITIME3-ITIME1 + ((ITIME2-ITIME3)/NSTEP)*IZ(IGROUP)).GE.
     1   ITLEFT) GO TO 60
   50 CONTINUE
   55 IK(1) = UDVT
      CALL RDTRL (IK(1))
      NCOL = IK(2)/3
      RETURN
C
C     UNCOUPLED MODAL
C
  100 CALL TRD1E(MDD,BDD,KDD,PD,UDVT,NGROUP)
      GO TO 55
C
C     INSUFFICIENT TIME LEFT TO FINISH
C
   60 CONTINUE
      IK(1) =UDVT
      CALL RDTRL( IK(1))
      NCOL = IK(2)/3
      IK(1) = PD
      CALL RDTRL( IK)
      CALL MESAGE (45, IK(2)-NCOL, NAME)
      IF (NCOL.EQ.0) CALL MESAGE(-37,0,NAME)
      RETURN
      END
