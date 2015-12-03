      SUBROUTINE SCALED (TYPE,EMORD)
C
C     THIS ROUTINE PROCESSES CELAS, CDAMP, AND CMASS ELEMENTS.
C
C     TYPE  - DENOTES FORM OF EST DATA. IE CELAS1,CELAS2,ETC.
C     EMORD - DENOTES MATRIX  1 = CELAS = STIFFNESS MATRIX,
C                             2 = CMASS = MASS MATRIX,
C                             3 = CDAMP = DAMPING MATRIX
C
C     EST FOR ELAS ELEMENTS
C
C                     TYPE           TYPE           TYPE           TYPE
C             CELAS1         CELAS2         CELAS3         CELAS4
C             ------  ----   ------  ----   ------  ----   ------  ----
C     ECPT(1) IELID     I    IELID     I    IELID     I    IELID     I
C     ECPT(2) IGP1      I    K         R    IS1       I    K         R
C     ECPT(3) IGP2      I    IGP1      I    IS2       I    IS1       I
C     ECPT(4) IC1       I    IGP2      I    K         R    IS2       I
C     ECPT(5) IC2       I    IC1       I    GSUBE     R
C     ECPT(6) K         R    IC2       I    S         R
C     ECPT(7) GSUBE     R    GSUBE     R
C     ECPT(8) S         R    S         R
C
      LOGICAL          NOGO
      INTEGER          TYPE,EMORD,EID,ISIL(2),ICOMP(2),GPT(4),CPT(2),
     1                 KPT(4),GSPT(4),CODE,IEST(1),DICT(7),GSUBE,ELID,
     2                 ESTID
      DOUBLE PRECISION DZ(16)
      DIMENSION        Z(16)
      CHARACTER        UFM*23,UWM*25
      COMMON /XMSSG /  UFM,UWM
      COMMON /EMGEST/  EST(100)
      COMMON /EMGPRM/  DUMY(15),IMAT(3),IPREC,NOGO
      COMMON /SYSTEM/  KSYSTM(65)
      COMMON /EMGDIC/  DUM2(2),NLOCS,ELID,ESTID
      EQUIVALENCE      (KSYSTM(2),IOUTPT),(Z(1),DZ(1)),(IEST(1),EST(1))
      DATA    GPT   /  2, 3, 2, 3 /, CPT / 4, 5 /, KPT /6, 2, 4, 2 /
      DATA    GSPT  /  7, 7, 5, 0 /
C
C     TEST IF MATRIX TO BE PRODUCED IS REQUESTED
C
      IF (IMAT(EMORD) .EQ. 0) RETURN
C
C     MOVE EST DATA TO LOCAL ARRAYS.  LOCATIONS ARE GIVEN BY DATA //
C
      EID     = IEST(1)
      IP      = KPT(TYPE)
      Z(1)    = EST(IP)
      GSUBE   = 0
      ICOMP(1)= 0
      ICOMP(2)= 0
      DICT(2) = 1
      NGRIDS  = 2
      IP      = GPT(TYPE)
      ISIL(1) = IEST(IP)
      ISIL(2) = IEST(IP+1)
      IF (TYPE .GE. 3) GO TO 10
      IP = CPT(TYPE)
      IF (IEST(IP  ) .NE. 0) ICOMP(1) = IEST(IP  ) - 1
      IF (IEST(IP+1) .NE. 0) ICOMP(2) = IEST(IP+1) - 1
C
C     IF ONE SIL IS ZERO INSURE THAT IT IS THE SECOND.
C     IF BOTH SILS ARE NON-ZERO MAKE SURE HIGHER OF TWO IS SECOND.
C
   10 IF (ISIL(2) .EQ. 0) GO TO 5
      IF (ISIL(1) .EQ. 0) GO TO 4
      IF (ISIL(1) .LE. ISIL(2)) GO TO 5
C
C     SWITCH SILS AND COMPS
C
    4 IP      = ISIL(2)
      ISIL(2) = ISIL(1)
      ISIL(1) = IP
      IP      = ICOMP(2)
      ICOMP(2)= ICOMP(1)
      ICOMP(1)= IP
    5 IF (ISIL(2) .GT. 0) GO TO 20
C
C     IF THE SECOND SIL EQUALS ZERO THE ELEMENT IS GROUNDED
C     ONLY A SINGLE MATRIX TERM IS PRODUCED
C
      NGRIDS = 1
      DICT(2)= 1
      NTERMS = 1
      CODE   = 2**ICOMP(1)
      NCOL   = 1
      GO TO 80
C
   20 IF (ISIL(2) .NE. ISIL(1)) GO TO 30
C
C     IF THE ELEMENT CONNECTS TWO COMPONENTS OF THE SAME POINT IT
C     MUST HAVE SPECIAL TREATMENT
C
      IF (ICOMP(2) .EQ. ICOMP(1)) GO TO 110
C
C     IN THE GENERAL CASE, THE CONNECTED COMPONENTS MAY BE THE SAME
C     AND THE MATRIX IS A 2 BY 2.  IF THE COMPONENTS ARE DIFFERENT
C     THE MATRIX WILL BE A 4 BY 4 WITH ADDITIONAL ZEROS.
C
      GO TO 40
   30 IF (ICOMP(1) .EQ. ICOMP(2)) GO TO 70
C
   40 NTERMS= 16
      CODE  = 2**ICOMP(1) + 2**ICOMP(2)
      NCOL  = 4
      DO 50 I = 2,16
      Z( I) = 0.0
   50 CONTINUE
      IF (ICOMP(2) .LT. ICOMP(1)) GO TO 60
      Z( 4) =-Z(1)
      Z(13) =-Z(1)
      Z(16) = Z(1)
      IF (ISIL(1) .NE. ISIL(2)) GO TO 80
      Z( 2) = Z( 4)
      Z( 5) = Z(13)
      Z( 6) = Z(16)
      Z( 4) = 0.0
      Z(13) = 0.0
      Z(16) = 0.0
      GO TO 80
   60 Z( 6) = Z(1)
      Z( 7) =-Z(1)
      Z(10) =-Z(1)
      Z(11) = Z(1)
      Z( 1) = 0.0
      IF (ISIL(1) .NE. ISIL(2)) GO TO 80
      Z( 1) = Z(11)
      Z( 2) = Z(10)
      Z( 5) = Z( 7)
      Z( 7) = 0.0
      Z(10) = 0.0
      Z(11) = 0.0
      GO TO 80
C
C     COMPONENTS ARE THE SAME FOR BOTH POINTS
C
   70 NTERMS= 4
      NCOL  = 2
      CODE  = 2**ICOMP(1)
      Z(2)  =-Z(1)
      Z(3)  =-Z(1)
      Z(4)  = Z(1)
C
C     OUTPUT THE MATRIX HERE
C
   80 DICT(1) = ESTID
      DICT(3) = NCOL
      DICT(4) = CODE
      DICT(5) = 0
      IPG     = GSPT(TYPE)
C
C     STRUCTURAL DAMPING FOR  STIIFNESS MATRICES IS INSERTED IN DICT
C
      IF (EMORD.EQ.1 .AND. TYPE.LE.3) DICT(5) = IEST(IPG)
      IF (IPREC .EQ. 1) GO TO 100
      I = NTERMS
   90 DZ(I) = Z(I)
      I = I - 1
      IF (I .GT. 0) GO TO 90
  100 CALL EMGOUT (Z,DZ,NTERMS,1,DICT,EMORD,IPREC)
      RETURN
C
  110 WRITE  (IOUTPT,120) UWM,EID
  120 FORMAT (A25,' 3120, IMPROPER CONNECTION ON CELAS ELEMENT',I9)
      RETURN
      END
