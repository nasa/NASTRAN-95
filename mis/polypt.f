      SUBROUTINE POLYPT( LOCTOF,STEDGE,TR, NGRIDF,FLEDGE,FL,LOCFOS, EPS,
     1  NPOLY,P)
C
C     POLYPT DETERMINES PERIMETER POINTS OF AREA COMMON TO STRUCTURAL
C        TRIANGLE BOUNDED BY TR POINTS AND FLUID ELEMENT BOUNDED BY
C        (3 OR 4) FL POINTS
C
      DOUBLE PRECISION P(2,7)
      DOUBLE PRECISION TR(3,3), FL(3,4), SS(2), P1(2), EPS(2)
      INTEGER   STEDGE(2,3), FLEDGE(2,4), KEDGE(2,5), JEDGE(2,7)
     1, LOCTOF(3), LOCFOS(4)
C
      IP= 0
      NPOLY= 0
C
      DO 10 I=1,2
      DO 10 J=1,7
   10 P(I,J)= 0.D0
C
      DO 20 K=1,3
      IF ( LOCTOF(K) .LT. 0)  GO TO 40
   20 CONTINUE
C
C     STRUCTURAL TRIANGLE IS COMMON AREA WHEN NO STR PTS LIE OUTSIDE
C        FLUID ELEMENT BOUNDRY
      IP= 3
      DO 30 K=1,3
      DO 30 I=1,2
   30 P(I,K)= TR(I,K)
      GO TO 9000
C
   40 CONTINUE
C
      K= NGRIDF -1
      DO 50 I=1,2
      DO 45 J=1,K
      JEDGE(I,J)= FLEDGE(I,J)
   45 JEDGE(I,J+NGRIDF)= FLEDGE(I,J)
   50 JEDGE(I,NGRIDF)= FLEDGE(I,NGRIDF)
C
      DO 60 I=1,2
      DO 55 J=1,2
      KEDGE(I,J)= STEDGE(I,J)
   55 KEDGE(I,J+3)= STEDGE(I,J)
   60 KEDGE(I,3)= STEDGE(I,3)
C
C
      DO 100 K=1,3
      K1= KEDGE(1,K)
      K2= KEDGE(2,K)
      DO 100 J=1,NGRIDF
      J1= JEDGE(1,J)
      J2= JEDGE(2,J)
      CALL PTINTR( TR(1,K1),TR(1,K2), FL(1,J1),FL(1,J2), SS, INTER, EPS)
      IF (INTER .EQ. 1)  GO TO 200
  100 CONTINUE
C
C - - AREAS ARE DISJOINT
      GO TO 9000
C
C
  200 JLAST= J
      JJ1= J
      JJ2= J +NGRIDF -1
      KLAST= K
      KK1= K +1
      KK2= K+2
C
      IF (LOCTOF(K1) .EQ. 1)  GO TO 1800
C     1ST TRI POINT IS OUTSIDE FLUID BOUNDRY
      P1(2)= SS(2)
      P1(1)= SS(1)
      AP1= (P1(1)-TR(1,K1))**2 +(P1(2)-TR(2,K1))**2
      JP1= JLAST
      JJ1= JLAST+1
C
      DO 300 J=JJ1,JJ2
      J1= JEDGE(1,J)
      J2= JEDGE(2,J)
      CALL PTINTR( TR(1,K1),TR(1,K2), FL(1,J1),FL(1,J2), SS, INTER, EPS)
      IF (INTER .EQ. 1)  GO TO 400
  300 CONTINUE
C
      IP= IP+1
      P(1,IP)= P1(1)
      P(2,IP)= P1(2)
      GO TO 1000
C
  400 AP2= (SS(1)-TR(1,K1))**2 + (SS(2)-TR(2,K1))**2
      IF (AP1 .LT. AP2)  GO TO 500
C
      P(1,IP+1)= SS(1)
      P(2,IP+1)= SS(2)
      P(1,IP+2)= P1(1)
      P(2,IP+2)= P1(2)
      IP= IP+2
      JLAST= JP1
      GO TO 600
C
  500 P(1,IP+1)= P1(1)
      P(2,IP+1)= P1(2)
      P(1,IP+2)= SS(1)
      P(2,IP+2)= SS(2)
      IP= IP+2
      JLAST= J
C
  600 CONTINUE
      IF ( JLAST .GT. NGRIDF)  JLAST= JLAST -NGRIDF
      JJ1= JLAST
      JJ2= JJ1 +NGRIDF -1
      J2= JEDGE(2,JLAST)
      GO TO 2000
C
C     SEARCH ALONG LAST STRUCTURAL TRIANGLE EDGE FOR NEXT PTINTR
C
 1000 IF ( LOCTOF(K2) .LT. 0)  GO TO 1100
      IF (TR(1,K2) .EQ. P(1,1)  .AND. TR(2,K2) .EQ. P(2,1))  GO TO 9000
      IP= IP+1
      P(1,IP)= TR(1,K2)
      P(2,IP)= TR(2,K2)
      KLAST= KLAST +1
      IF ( KLAST .EQ. KK2)   GO TO 9000
      K2= KEDGE(2,KLAST)
      GO TO 1000
C
 1100 CONTINUE
      JJ1= JLAST
      IF (JJ1 .GT. JJ2)  GO TO 9000
      DO 1150 J= JJ1,JJ2
      J1= JEDGE(1,J)
      J2= JEDGE(2,J)
      CALL PTINTR( P(1,IP),TR(1,K2), FL(1,J1),FL(1,J2), SS, INTER, EPS)
      IF ( INTER .EQ. 1)  GO TO 1200
 1150 CONTINUE
C
      GO TO 9000
C
 1200 IF (SS(1) .EQ. P(1,1)  .AND.  SS(2) .EQ. P(2,1))  GO TO 9000
      IP= IP +1
      P(1,IP)= SS(1)
      P(2,IP)= SS(2)
      JLAST= J
      GO TO 2000
C
 1800 P(1,IP+1)= TR(1,K1)
      P(2,IP+1)= TR(2,K1)
      P(1,IP+2)= SS(1)
      P(2,IP+2)= SS(2)
      IP= IP+2
C
C     SEARCH ALONG LAST FLUID EDGE FOR NEXT PTINTR
C
 2000 IF ( LOCFOS(J2) .LT. 0)  GO TO 2100
      IF (FL(1,J2) .EQ. P(1,1)  .AND.  FL(2,J2) .EQ. P(2,1))  GO TO 9000
      IP= IP+1
      P(1,IP)= FL(1,J2)
      P(2,IP)= FL(2,J2)
      JLAST= JLAST +1
      IF ( JLAST .GT. JJ2)   GO TO 9000
      J2= JEDGE(2,JLAST)
      GO TO 2000
C
 2100 CONTINUE
      KK1= KLAST
      IF (KK1 .GT. KK2)  GO TO 9000
      DO 2150 K=KK1,KK2
      K1= KEDGE(1,K)
      K2= KEDGE(2,K)
      CALL PTINTR( P(1,IP),FL(1,J2), TR(1,K1),TR(1,K2), SS, INTER, EPS)
      IF ( INTER .EQ. 1)  GO TO 2200
 2150 CONTINUE
C
      GO TO 9000
C
 2200 IF (SS(1) .EQ. P(1,1)  .AND.  SS(2) .EQ. P(2,1))  GO TO 9000
      IP= IP +1
      P(1,IP)= SS(1)
      P(2,IP)= SS(2)
      KLAST= K
      GO TO 1000
C
C
 9000 CONTINUE
      NPOLY= IP
      RETURN
      END
