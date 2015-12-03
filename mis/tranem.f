      SUBROUTINE TRANEM (MCSID, NGRID, R, ICOMP, U, RC)
C*****
C     COMPUTES A STRESS TRANSFORMATION MATRIX U FOR TRIANGLES AND QUADS.
C     INPUTS
C        MCSID  ID OF COORDINATE SYSTEM REFERENCED ON MAT1,2 DATA CARD.
C        NGRID  3 FOR TRIANGLES, 4 FOR QUADS.
C        R      ARRAY OF BASIC LOCATIONS OF ELEMENT GRID PTS (3,NGRID).
C     OUTPUTS
C        ICOMP  1 (IF MAT X-AXIS IS USED) OR 2 (IF Y-AXIS IS USED).
C        U      ARRAY (3X3) FOR TRANSFORMATION, STORED BY ROW.
C        RC     BASIC LOCATION COORDINATES OF ELEMENT CENTER.
C     REQUIREMENTS
C        SUBROUTINE PRETRS MUST SET UP FOR TRANSS. SEE P.M. PAGE 3.4-66
C*****
      INTEGER ECPT(4),SUBNAM(2)
C
      REAL RC(3)
      REAL R(9)
      REAL U(9)
      REAL RCENT(4)
C
      EQUIVALENCE  (RCENT(1), ECPT(1))
C
      DATA SUBNAM /4HTRAN,2HEM/
C
C-----------------------------------------------------------------------
C
      IF(NGRID .NE.3 .AND. NGRID.NE.4 )  CALL MESAGE(-61,0,SUBNAM)
C*****
C     FIND THE UNIT NORMAL OF THE ELEMENT
C*****
      I = 3*(NGRID-3)
      VN1 = (R(8)-R(2))*(R(I+9)-R(6))-(R(9)-R(3))*(R(I+8)-R(5))
      VN2 = (R(9)-R(3))*(R(I+7)-R(4))-(R(7)-R(1))*(R(I+9)-R(6))
      VN3 = (R(7)-R(1))*(R(I+8)-R(5))-(R(8)-R(2))*(R(I+7)-R(4))
      TEMP = SQRT(VN1**2+VN2**2+VN3**2)
      IF(TEMP .LE. 0.0) CALL MESAGE(-61,0,SUBNAM)
      VN1 = VN1 / TEMP
      VN2 = VN2 / TEMP
      VN3 = VN3 / TEMP
C*****
C     GET THE UNIT VECTORS OF MCSID AT ELEM CENTER. PUT IN U TEMPORARILY
C*****
      GRDS = NGRID
      DO 20 IC=1,3
      SUM = 0.0
      DO 10 IG=1,NGRID
      K = 3*IG + IC-3
      SUM = SUM +R(K)
   10 CONTINUE
      RCENT(IC+1) = SUM / GRDS
      RC(IC) = RCENT(IC+1)
   20 CONTINUE
      ECPT(1) = MCSID
      CALL TRANSS(ECPT,U)
C*****
C     SELECT FIRST OR SECOND VECTOR TO PROJECT FOR ELEM-MAT X-AXIS
C*****
      VNDOTM=VN1*U(1)+VN2*U(4)+VN3*U(7)
      IF( VNDOTM**2 .GT. 0.4) GO TO 30
      ICOMP = 1
      VM1 = U(1)
      VM2 = U(4)
      VM3 = U(7)
      GO TO 40
   30 CONTINUE
      ICOMP = 2
      VM1 = U(2)
      VM2 = U(5)
      VM3 = U(8)
      VNDOTM = VN1*VM1+VN2*VM2+VN3*VM3
   40 CONTINUE
C*****
C     FIND COSINE AND SINE OF ANGLE
C*****
      VE1 = R(4) - R(1)
      VE2 = R(5) - R(2)
      VE3 = R(6) - R(3)
      C = VE1*(VM1-VNDOTM*VN1)
     *  + VE2*(VM2-VNDOTM*VN2)
     *  + VE3*(VM3-VNDOTM*VN3)
      S = VE1*(VM2*VN3-VM3*VN2)
     *  + VE2*(VM3*VN1-VM1*VN3)
     *  + VE3*(VM1*VN2-VM2*VN1)
      TEMP = SQRT(C*C+S*S)
      IF(TEMP .LE. 0.0) CALL MESAGE(-61,0,SUBNAM)
      C = C/TEMP
      S = S/TEMP
C*****
C     FILL IN THE U MATRIX, ROW STORED.
C*****
      U(1) = C*C
      U(4) = S*S
      U(7) = -C*S
      U(2) = U(4)
      U(5) = U(1)
      U(8) = -U(7)
      U(3) = 2.0*U(8)
      U(6) = -U(3)
      U(9) = U(1)-U(4)
C
      RETURN
C
      END
