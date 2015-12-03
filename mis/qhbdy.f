      SUBROUTINE QHBDY
C*****
C
C  THIS ROUTINE APPLIES THE LOADS DUE TO A SELECTED HEAT FLUX
C  LOADING CONDITION.
C
C  DATA CARD IS...
C
C  QHBDY  SETID  FLAG  Q0  AF  G1  G2  G3  G4
C                ============================
C                ABOVE FIELDS AVAILABLE TO THIS ROUTINE ONLY.
C                GRIDS ARE IN INTERNAL NOTATION AT THIS POINT.
C*****
      INTEGER  MAP(15)  ,CARD(7)  ,IGRIDS(5),SLT      ,OLD      ,BG
      INTEGER  SILS(4)  ,GRIDS(4) ,ORDER(4) ,SUBR(2)
C
      REAL     BGPDT(4,4)         ,X(4)     ,Y(4)     ,Z(4)
      REAL     DATA4(4) ,P(4)     ,R12(3)   ,R13(3)   ,LENGTH
C
      COMMON /CONDAS/ CONSTS(5)
      COMMON/LOADX / LC, SLT, BG, OLD, N(12), IFM
      COMMON/ZZZZZZ/ CORE(1)
C
      EQUIVALENCE ( CONSTS(1) , PI     )
      EQUIVALENCE(X(1),BGPDT(1,2)), (Y(1),BGPDT(1,3)), (Z(1),BGPDT(1,4))
      EQUIVALENCE(IFLAG,CARD(1)), (Q0,CARD(2)), (AF,CARD(3))
      EQUIVALENCE(GRIDS(1),CARD(4),SILS(1))
C
      DATA MAP/ 1,2,3,  1,2,4,  2,3,1,  3,4,2,  4,1,3 /
      DATA IGRIDS/ 1,2,2,3,4 /
      DATA SUBR/ 4HQHBD,4HY    /
C*****
C  READ AND PROCESS ONE QHBDY IMAGE PER CALL TO THIS ROUTINE.
C*****
      CALL READ(*902,*903,SLT,CARD(1),7,0,FLAG)
      NGRIDS = IGRIDS(IFLAG)
C*****
C  OBTAIN A GRID (INTERNAL) POINT SORT VECTOR SO AS TO CALL FOR BGPDT
C  DATA EFFICIENTLY.
C*****
      IF( NGRIDS .LE. 1 ) GO TO 35
      CALL PERMUT( GRIDS(1), ORDER(1), NGRIDS, OLD )
      GO TO 38
   35 ORDER(1) = 1
C*****
C  PICK UP BGPDT FOR THE 1 TO 4 POINTS AND OBTAIN THE SILS.
C*****
   38 DO 40 I = 1,NGRIDS
      L = ORDER(I)
      CALL FNDPNT( DATA4(1), GRIDS(L) )
      BGPDT(L,1) = DATA4(1)
      BGPDT(L,2) = DATA4(2)
      BGPDT(L,3) = DATA4(3)
      BGPDT(L,4) = DATA4(4)
      CALL FNDSIL( GRIDS(L) )
   40 CONTINUE
C*****
C  ALL DATA IS AT HAND FOR LOAD CALCULATIONS
C*****
      AF = AF * Q0
      GO TO (100,200,300,400,500),IFLAG
C*****
C  IFLAG=1   A POINT...
C*****
  100 P(1) = AF
      GO TO 700
C*****
C  IFLAG=2  A LINE...
C*****
  200 LENGTH = SQRT( (X(2)-X(1))**2 + (Y(2)-Y(1))**2 +  (Z(2)-Z(1))**2 )
      P(1) = AF * LENGTH * 0.50E0
      P(2) = P(1)
      GO TO 700
C*****
C  IFLAG=3  A LINE OF REVOLUTION...
C*****
  300 FACT = PI*Q0*SQRT( (X(2)-X(1))**2 + (Z(2)-Z(1))**2 ) / 3.0E0
      P(1) = FACT * (2.0E0*X(1) + X(2))
      P(2) = FACT * (X(1) + 2.0E0*X(2))
      GO TO 700
C*****
C  IFLAG=4  A TRIANGLE...
C*****
  400 FACT = Q0 / 6.0E0
      IMAP = 1
      NMAP = 3
      GO TO 600
C*****
C  IFLAG=5  A QUADRILATERAL...
C*****
  500 FACT = Q0 / 12.0E0
      IMAP = 4
      NMAP = 15
C*****
C  MAP 1 OR 4 TRIANGLES INTO 3 OR 4 POINTS.
C*****
  600 P(1) = 0.0E0
      P(2) = 0.0E0
      P(3) = 0.0E0
      P(4) = 0.0E0
      DO 650 I = IMAP,NMAP,3
      I1 = MAP(I)
      I2 = MAP(I+1)
      I3 = MAP(I+2)
      R12(1) = X(I2) - X(I1)
      R12(2) = Y(I2) - Y(I1)
      R12(3) = Z(I2) - Z(I1)
      R13(1) = X(I3) - X(I1)
      R13(2) = Y(I3) - Y(I1)
      R13(3) = Z(I3) - Z(I1)
      CALL SAXB( R12(1), R13(1), R12(1) )
      FACTX= FACT * SQRT( R12(1)**2  + R12(2)**2  + R12(3)**2 )
      P(I1) = P(I1) + FACTX
      P(I2) = P(I2) + FACTX
      P(I3) = P(I3) + FACTX
  650 CONTINUE
C*****
C  LOAD VALUES COMPLETE.
C*****
  700 DO 800 I = 1,NGRIDS
      ISIL = SILS(I)
      CORE(ISIL  ) = CORE(ISIL  ) + P(I)
  800 CONTINUE
      RETURN
C*****
C  END OF FILE OR END OF RECORD HIT ERROR.
C*****
  902 CALL MESAGE(-2,SLT,SUBR)
  903 CALL MESAGE(-3,SLT,SUBR)
      GO TO 902
      END
