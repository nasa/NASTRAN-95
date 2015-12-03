      SUBROUTINE QLOADL (IOPT)
C
C     THIS ROUTINE CALCULATES THERMAL LOADS FROM QBDY1, QBDY2, OR
C     QVECT DATA. THE INPUT DATA, READ FROM FILE SLT, IS -
C
C     ENTRY       QBDY1         QBDY2          QVECT
C     -----       -----         -----          -----
C       1          TYPE         EL.ID.          SIL1
C       2         EL.ID.         TYPE           SIL2
C       3          SIL1          SIL1           SIL3
C       4          SIL2          SIL2           SIL4
C       5          SIL3          SIL3          EL.ID.
C       6          SIL4          SIL4           TYPE
C      7-10      C1,C2,C3,C4    -SAME          -SAME
C     11-13       E VECTOR       NONE           NONE
C     14-16      V1 VECTOR         *             *
C     17-19      V2 VECTOR         *             *
C
      LOGICAL         NOGO,TRANST
      INTEGER         SLT,OLD,BG,SUBR(2),IGRIDS(6),TYPE,SILS(4),IE(3),
     1                MINUS(2)
      REAL            E(3),COEF(4),V1(3),V2(3),CARD(19)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /LOADX / LC,SLT,BG,OLD,NXN(12),IFM,NYN(2),ILID
      COMMON /ZZZZZZ/ CORE(1)
      COMMON /SYSTEM/ SYSBUF,IOUT
      COMMON /QVECT / ITRAN,IQVECT
      EQUIVALENCE     (TYPE,CARD(1)),(ID,CARD(2)),(SILS(1),CARD(3)),
     1                (COEF(1),CARD(7)),(E(1),IE(1),CARD(11)),
     2                (V1(1),CARD(14)),(V2(1),CARD(17))
      DATA    IGRIDS/ 1,2,2,3,4,2   /
      DATA    SUBR  / 4HQLOA,4HDL   /
      DATA    ITRAN1, IOLD,MINUS    / 4HTRAN,0,-1,-1 /
C
      TRANST = .FALSE.
      IF (ITRAN .EQ. ITRAN1) TRANST = .TRUE.
      NWORDS = 10
      IF (IOPT .EQ. 3) NWORDS = 19
C
      CALL READ (*100,*110,SLT,CARD(1),NWORDS,0,FLAG)
C
C     REARRANGE CARD ARRAY FOR UNIFORMITY.
C
      GO TO (20,10,40), IOPT
   10 DOT = CARD(1)
      CARD(1) = CARD(2)
      CARD(2) = DOT
   20 N = IGRIDS(TYPE)
C
C     QBDY1 OR QBDY2
C
      DO 30 I = 1,N
      ISIL = SILS(I)
      CORE(ISIL) = CORE(ISIL) + COEF(I)
   30 CONTINUE
      RETURN
C
C     QVECT LOADS
C
   40 DOT     = CARD(5)
      DOT2    = CARD(6)
      CARD(6) = CARD(4)
      CARD(5) = CARD(3)
      CARD(4) = CARD(2)
      CARD(3) = CARD(1)
      CARD(2) = DOT
      CARD(1) = DOT2
      N       = IGRIDS(TYPE)
      DOT     = 0.0
      INT     = 0
      IF (TYPE .EQ. 6) GO TO 70
      DO 50 I = 1,3
      IF (NUMTYP(IE(I)) .EQ. 1) GO TO 51
      DOT = DOT + E(I)*V1(I)
      GO TO 50
   51 INT = INT + 1
   50 CONTINUE
      IF (INT .GT.   0) GO TO 90
      IF (DOT .GE. 0.0) RETURN
      DO 60 I = 1,N
      ISIL = SILS(I)
   60 CORE(ISIL) = CORE(ISIL) - DOT*COEF(I)
      RETURN
C
C     QVECT ON ELCYL ELEMENT
C
   70 DOT2 = 0.0
      DO 80 I = 1,3
      IF (NUMTYP(IE(I)) .EQ. 1) GO TO 81
      DOT  = DOT  + E(I)*V1(I)
      DOT2 = DOT2 + E(I)*V2(I)
      GO TO 80
   81 INT = INT + 1
   80 CONTINUE
      IF (INT .GT. 0) GO TO 90
      COEF(1) = COEF(1)*SQRT(DOT**2 + DOT2**2)
      COEF(2) = COEF(1)
      ISIL = SILS(1)
      CORE(ISIL) = CORE(ISIL) + COEF(1)
      ISIL = SILS(2)
      CORE(ISIL) = CORE(ISIL) + COEF(2)
      RETURN
C
C     GOES HERE IF INTEGERS ARE FOUND IN E VECTOR
C
   90 IF (.NOT. TRANST) GO TO 120
C
C     BUILD QVECT RECORDS FOR TRANSIENT
C
      IF (ILID .EQ. IOLD) GO TO 91
      IF (IOLD .EQ.    0) GO TO 92
C
C     TERMINATE OLD RECORD
C
      CALL WRITE (IQVECT,MINUS,2,0)
   92 IOLD = ILID
      CALL WRITE (IQVECT,ILID,1,0)
C
C     DUMP DATA ON IQVECT
C
   91 CALL WRITE (IQVECT,N,1,0)
      DO 93 I = 1,N
      CALL WRITE (IQVECT,SILS(I),1,0)
      CALL WRITE (IQVECT,COEF(I),1,0)
   93 CONTINUE
      CALL WRITE (IQVECT,IE,3,0)
      CALL WRITE (IQVECT,V1,6,0)
      RETURN
C
  100 CALL MESAGE (-2,SLT,SUBR)
  110 CALL MESAGE (-3,SLT,SUBR)
  120 NOGO = .TRUE.
      WRITE  (IOUT,130) UFM,ID
  130 FORMAT (A23,' 3080, ERROR IN QVECT DATA, INTEGER VALUES SPECIFIED'
     1,      ' FOR THERMAL FLUX VECTOR COMPONENTS', /30X,
     2       'IN A NON-TRANSIENT ANALYSIS.', /30X,'ELEMENT ID = ',I9)
      CALL MESAGE (-61,0,SUBR)
      RETURN
      END
