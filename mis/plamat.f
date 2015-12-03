      SUBROUTINE PLAMAT
C THIS ROUTINE RETURNS GP ROTATED FOR PLA3 AND PLA4
C
      DIMENSION X(27)
      COMMON /MATIN / MATID,INFLAG,ELTEMP,PLAARG,SINTH,COSTH
      COMMON /MATOUT/ G11,G12,G13,G22,G23,G33 , DUMMY(14)
      COMMON /PLAGP / GP(9) , MIDGP , ELID
C
C  TEST TO SEE IF INCOMING MATERIAL ID IS EQUAL TO MATERIAL ID IN
C  PLAGP.  IF NOT USE REGULAR CALL TO MAT TO GET GP
C
      IF( MIDGP .NE. MATID ) GO TO 10
C
C                           T
C  TRANSFORM G   ,  G  =   U  *  G   * U
C             P      P            P
C
      X(1)  = COSTH**2
      X(2)  = SINTH**2
      X(3)  = COSTH * SINTH
      X(4)  = X(2)
      X(5)  = X(1)
      X(6)  = -X(3)
      X(7)  = 2.0 * X(6)
      X(8)  = -X(7)
      X(9)  = X(1) - X(2)
      CALL GMMATS(GP(1),3,3,0,X( 1),3,3,0,X(19))
      CALL GMMATS(X( 1),3,3,1,X(19),3,3,0,X(10))
      G11 = X(10)
      G12 = X(11)
      G13 = X(12)
      G22 = X(14)
      G23 = X(15)
      G33 = X(18)
      RETURN
   10 INFLAG = 2
      CALL MAT (ELID)
      RETURN
      END
