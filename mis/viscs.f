      SUBROUTINE VISCS
C
C     THIS SUBROUTINE COMPUTES THE 12X12 MATRIX BGG FOR A VISCOUS
C     (DASHPOT) ELEMENT
C
C     SINGLE PRECISION VERSION
C
C     THE ECPT ENTRIES FOR THE VISC ELEMENT ARE
C
C         ECPT
C     ECPT( 1)   ELEMENT ID
C     ECPT( 2)   SIL NUMBER FOR GRID POINT A
C     ECPT( 3)   SIL NUMBER FOR GRID POINT B
C     ECPT( 4)   EXTENSIONAL DAMPING CONSTANT  - C1
C     ECPT( 5)   TORSIONAL DAMPING COEFFICIENT - C2
C     ECPT( 6)   COORD. SYSTEM ID FOR POINT A
C     ECPT( 7)   X1
C     ECPT( 8)   Y1
C     ECPT( 9)   Z1
C     ECPT(10)   COORD. SYSTEM ID FOR POINT B
C     ECPT(11)   X2
C     ECPT(12)   Y2
C     ECPT(13)   Z2
C     ECPT(14)   ELEMENT TEMPERATURE (NOT USED)
C
C
      LOGICAL         NOGO,IDBUG
      INTEGER         IECPT(14),ELID,ESTID,DICT(7),INDX(4),KX(4),KBX(4)
      REAL            VEC(3),D(64),B(144),TA(9),TB(9)
      CHARACTER       UFM*23
      COMMON /XMSSG / UFM
      COMMON /SYSTEM/ SKP,IOUTPT,KSYSTM(53),IHEAT
      COMMON /EMGEST/ ECPT(14)
      COMMON /EMGPRM/ IXTRA,JCORE,NCORE,DUM(12),ISTIF,IMASS,IDAMP,
     1                IPREC,NOGO,HEAT,ICMBAR,LCSTM,LMAT,LHMAT
      COMMON /ZZZZZZ/ XX(1)
      COMMON /EMGDIC/ IDM,LDICT,NGRIDS,ELID,ESTID
      EQUIVALENCE     (ECPT(1),IECPT(1),IELID), (DICT(5),DICT5),
     1                (INDX(1),IA), (INDX(2),IAB), (INDX(3),IBA),
     2                (INDX(4),IB)
      DATA    KX    / 1 ,7 ,73 ,79 /
      DATA    KBX   / 40,46,112,118/
C
C     INITIALIZE EMGOUT PARAMETERS
C
      IDBUG   = .TRUE.
      NGRIDS  = 2
      LDICT   = 5 + NGRIDS
      DICT(1) = ESTID
      DICT(2) = 1
      DICT(3) = 12
      DICT(4) = 63
      DICT5   = 0.
      IFILE   = 3
      IP      = IPREC
C
C     NOW COMPUTE THE LENGTH OF THE ROD AND NORMALIZE
C
      FL = 0.
      DO 20 I = 1,3
      VEC(I) = ECPT(I+6) - ECPT(I+10)
   20 FL = FL + VEC(I)**2
      FL = SQRT(FL)
C
      IF (FL .LE. 0) GO TO 7770
      DO 30 I = 1,3
   30 VEC(I) = VEC(I)/FL
C
C     SET UP THE N MATRIX
C
      DO 40 I = 1,3
      DO 40 J = 1,3
      IX = (I-1)*3 + J
   40 D(IX) = VEC(I)*VEC(J)
C
C     INITIALIZE THE B MATRIX
C
      DO 50 I = 1,144
   50 B(I) = 0.
C
C     SWAP INDICES A AND B IF NECESSARY SO MATRIX WILL BE ORDERED
C     BY INCREASING SIL VALUE
C
      IPA = 6
      IPB = 10
      IF (IECPT(2) .LT. IECPT(3)) GO TO 60
      IX  = IPA
      IPA = IPB
      IPB = IPA
C
C     CONVERT GRID POINTS TO BASIC COORDINATES IF NECESSARY
C
   60 IA  = 1
      IAB = 1
      IF (IECPT(IPA) .EQ. 0) GO TO 70
      IA  = 19
      IAB = 10
      CALL TRANSS (ECPT(IPA),TA(1))
      CALL GMMATS (TA(1), 3,3,1, D(1), 3,3,0, D(10))
      CALL GMMATS (D(10), 3,3,0, TA(1),3,3,0, D(19))
C
   70 IB  = 1
      IBA = 1
      IF (IECPT(IPB) .EQ. 0) GO TO 80
      IB  = 28
      IBA = 37
      CALL TRANSS (ECPT(IPB), TB(1))
      CALL GMMATS (TB(1),3,3,1, D(1), 3,3,0, D(37))
      CALL GMMATS (D(37),3,3,0, TB(1),3,3,0, D(28))
C
      CALL GMMATS (D(IAB),3,3,0, TB(1), 3,3,0, D(46))
      IAB = 46
C
   80 IF (IECPT(IPA) .EQ. 0) GO TO 90
      CALL GMMATS (D(IBA),3,3,0, TA(1),3,3,0, D(55))
      IBA = 55
C
C     CALCULATE THE DAMPING MATRIX B
C
C                       ****                    ****
C                       *      /     /      /      *
C                       * C D  /   0 /-C D  /  0   *
C                       *  1 AA/     /  1 AB/      *
C                       *--------------------------*
C                       *  0   /C D  /   0  /-C D  *
C                       *      / 2 AA/      /  2 AB*
C         B    =        *--------------------------*
C                       *-C D  /   0 / C D  /  0   *
C                       *  1 BA/     /  1 BB/      *
C                       *------------/-------------*
C                       *  0   /-C D /   0  / C D  *
C                       *      /  2 BA      /  2 BB*
C                       *      /     /      /      *
C                       ****                    ****
C
   90 C1 = ECPT (4)
      C2 = ECPT (5)
C
      DO 120 JTJ = 1,4
      KB  = KX(JTJ)
      KBB = KBX(JTJ)
      J   = 0
      I1  = INDX(JTJ)
      I2  = I1 + 8
      IF (MOD(JTJ,2) .NE. 0) GO TO 100
      C1  = -C1
      C2  = -C2
C
  100 DO 110 I = I1,I2
      B(KB)  = C1*D(I)
      B(KBB) = C2*D(I)
      IF (MOD(I,3) .EQ. 0) J = 9
      KB  = KB  + 1 + J
      KBB = KBB + 1 + J
      J = 0
  110 CONTINUE
C
  120 CONTINUE
C
C     OUTPUT THE MATRIX
C
      CALL EMGOUT (B,B,144,1,DICT,IFILE,IP)
      RETURN
C
C     ERROR EXITS
C
 7770 WRITE  (IOUTPT,7775) UFM,IELID
 7775 FORMAT (A23,' 31XX, ILLEGAL GEOMETRY OR CONNECTIONS FOR VISC ',
     1       'ELEMENT',I10)
      NOGO = .TRUE.
      RETURN
      END
