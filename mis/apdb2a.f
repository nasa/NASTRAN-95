      SUBROUTINE APDB2A (NLINE,NL,SCR1,NSTNS,M1,S1,SN,TBLT,TBLR)
C
C     GENERATE BASIC TO LOCAL TRANSFORMATION MATRIX FOR
C     STREAMLINE NL OF SWEPT TURBOPROP BLADE.
C
      REAL L1,L2,L3,M1
C
      INTEGER SCR1,FILE,NAME(2)
C
      DIMENSION PN(3),P1(3),FN(3),F1(3),S1(3),SN(3),TBLT(3),TBLR(3)
      DIMENSION DATA(7)
C
      DATA FILE/301/,NAME  /4HAPDB,4H2A  /
C
C---------------------------------------------------------------------
C     INPUT VARIABLES--
C     NLINE      TOTAL NO. OF STREAMLINES
C     NL         PRESENT STEAMLINE
C     SCR1       SCRATCH UNIT WITH BASIC COORDINATES OF NODES
C     NSTNS      TOTAL NO. OF STATIONS
C     M1         SIGN BASED ON ROTATION OF BLADE
C     S1         COORDINATES OF LEADING EDGE OF CURRENT STREAMLINE
C     SN         COORDINATES OF TRAILING EDGE OF CURRENT STREAMLINE
C
C     OUTPUT VARIABLES--
C     TBLT      BASIC TO LOCAL TRANSFORMATION FOR TRANSLATION
C     TBLR      BASIC TO LOCAL TRANSFORMATION FOR ROTATION
C
C     LOCAL VARIABLES--
C     PN        COORDINATES TRAILING EDGE PREVIOUS STREAMLINE
C     P1        COORDINATES LEADING EDGE PREVIOUS STREAMLINE
C     FN        COORDINATES TRAILING EDGE NEXT STREAMLINE
C     F1        COORDINATES LEADING EDGE NEXT STREAMLINE
C---------------------------------------------------------------------
C     EXTRACT COORDINATES FOR PREVIOUS--P-FOR FIRST STREAMLINE
C---------------------------------------------------------------------
      IF(NL.NE.1)GO TO 10
      DO 5 I=1,3
      PN(I)=SN(I)
  5   P1(I)=S1(I)
C----------------------------------------------------------------------
C     NOW COORDINATES FOR NEXT--F-FOR LAST STREAMLINE
C----------------------------------------------------------------------
 10   IF(NL.NE.NLINE)GO TO 15
      DO 12 I=1,3
      FN(I)=SN(I)
 12   F1(I)=S1(I)
      GO TO 50
C----------------------------------------------------------------------
C     NOW COORDINATES FOR NEXT--F-FOR ALL OTHER STREAMLINES
C---------------------------------------------------------------------
 15   CALL FREAD(SCR1,DATA,7,0)
      F1(1)=DATA(5)
      F1(2)=DATA(6)
      F1(3)=DATA(7)
C----------------------------------------------------------------------
C    COMPUTE SKIP TO TRAILING EDGE COORDINATES
C-----------------------------------------------------------------------
      NSKIP=(2-NSTNS)*7
      CALL READ(*905,*900,SCR1,DATA,NSKIP,0,MM)
      CALL FREAD(SCR1,DATA,7,0)
      FN(1)=DATA(5)
      FN(2)=DATA(6)
      FN(3)=DATA(7)
C----------------------------------------------------------------------
C     RETURN TO START OF RECORD
C----------------------------------------------------------------------
      CALL BCKREC(SCR1)
C---------------------------------------------------------------------
C     COMPUTE SKIP TO ORIGINAL LOCATION AT ENTRY TO THIS ROUTINE
C---------------------------------------------------------------------
      NSKIP=-NSTNS*NL*7
      CALL READ(*905,*900,SCR1,DATA,NSKIP,0,MM)
 50   A1=SN(1)-S1(1)
      B1=SN(2)-S1(2)
      C1=SN(3)-S1(3)
C
      A2=FN(1)-P1(1)
      B2=FN(2)-P1(2)
      C2=FN(3)-P1(3)
C
      A3=PN(1)-F1(1)
      B3=PN(2)-F1(2)
      C3=PN(3)-F1(3)
C
      A4=B2*C1-B1*C2
      B4=C2*A1-C1*A2
      C4=A2*B1-A1*B2
C
      A5=B1*C3-B3*C1
      B5=C1*A3-C3*A1
      C5=A1*B3-A3*B1
C
      L1=SQRT(A1**2+B1**2+C1**2)
      L2=SQRT(A4**2+B4**2+C4**2)
      L3=SQRT(A5**2+B5**2+C5**2)
C
      A6=0.5 *(A4/L2  +  A5/L3)
      B6=0.5 *(B4/L2  +  B5/L3)
      C6=0.5 *(C4/L2  +  C5/L3)
C---------------------------------------------------------------------
C     BASIC TO LOCAL TRANSFORMATION FOR TRANSLATION
C---------------------------------------------------------------------
      TBLT(1)= A6*M1
      TBLT(2)= B6*M1
      TBLT(3)= C6*M1
C----------------------------------------------------------------------
C     BASIC TO LOCAL TRANSFORMATION FOR ROTATION
C---------------------------------------------------------------------
      TBLR(1)= -M1*A1/L1
      TBLR(2)= -M1*B1/L1
      TBLR(3)= -M1*C1/L1
      IF(NL.EQ.NLINE)RETURN
C---------------------------------------------------------------------
C     SET PREVIOUS COORDINATES--P- TO PRESENT STREAMLINE
C---------------------------------------------------------------------
      DO 800 I=1,3
      PN(I)=SN(I)
 800  P1(I)=S1(I)
      RETURN
C     E-O-R    ENCOUNTERED
 900  IP1 = -3
      GO TO 999
C     E-O-F    ENCOUNTERED
 905  IP1 = -2
 999  CALL MESAGE(IP1,FILE,NAME)
      RETURN
      END
