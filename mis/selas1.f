      SUBROUTINE SELAS1(IARG)
C*****
C THIS ROUTINE IS PHASE I OF STRESS DATA RECOVERY FOR THE ELAS ELEMENTS.
C
C
C
C*****
C
C
C
C              E C P T - S  F O R  E L A S  E L E M E N T S
C
C
C
C                  TYPE             TYPE           TYPE           TYPE
C         CELAS1           CELAS2         CELAS3         CELAS4
C ECPT(1) IELID     I      IELID     I    IELID      I   IELID      I
C ECPT(2) IGP1      I      K         R    IS1        I   K          R
C ECPT(3) IGP2      I      IGP1      I    IS2        I   IS1        I
C ECPT(4) IC1       I      IGP2      I    K          R   IS2        I
C ECPT(5) IC2       I      IC1       I    GSUBE      R
C ECPT(6) K         R      IC2       I    S          R
C ECPT(7) GSUBE     R      GSUBE     R
C ECPT(8) S         R      S         R
C
C
C
      DIMENSION
     1                   IECPT(6)
C
C SDR2 PHASE I INPUT AND OUTPUT BLOCK
C
      COMMON   /SDR2X5/
     A                   ECPT(100),
     1                   JELID              ,ISILNO(2)
     2,                  STIFF              ,SCOEFF
     3,                  DUMMY(120)
C
C
C
      EQUIVALENCE
     1                   (IECPT(1),ECPT(1)) ,(SCOEFF,ICOEFF)
C
C BUILD UP OUTPUT BLOCK DEPENDING UPON WHICH ELEMENT TYPE, ELAS1, ELAS2,
C ELAS3 OR ELAS4, IS BEING WORKED ON.
C
      GO TO (10,20,30,40),IARG
C
C ELAS1
C
   10 ISILNO(1) = IECPT(2) + IECPT(4)
      ISILNO(2) = IECPT(3) + IECPT(5)
      IF (IECPT(4) .GT. 0) ISILNO(1) = ISILNO(1) - 1
      IF (IECPT(5) .GT. 0) ISILNO(2) = ISILNO(2) - 1
      STIFF  = ECPT(6)
      SCOEFF = ECPT(8)
      GO TO 50
C
C ELAS2
C
   20 ISILNO(1) = IECPT(3) + IECPT(5)
      ISILNO(2) = IECPT(4) + IECPT(6)
      IF (IECPT(5) .GT. 0) ISILNO(1) = ISILNO(1) - 1
      IF (IECPT(6) .GT. 0) ISILNO(2) = ISILNO(2) - 1
      STIFF  = ECPT(2)
      SCOEFF = ECPT(8)
      GO TO 50
C
C ELAS3
C
   30 ISILNO(1) = IECPT(2)
      ISILNO(2) = IECPT(3)
      STIFF  = ECPT(4)
      SCOEFF = ECPT(6)
      GO TO 50
C
C ELAS4
C
   40 ISILNO(1) = IECPT(3)
      ISILNO(2) = IECPT(4)
      STIFF  = ECPT(2)
      ICOEFF = -1
C
C STORE ELEMENT ID.
C
   50 JELID = IECPT(1)
      RETURN
      END
