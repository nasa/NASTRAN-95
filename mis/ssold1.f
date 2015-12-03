      SUBROUTINE SSOLD1(ITYPE)
C*****
C
C  E C P T     TETRA          WEDGE          HEXA
C  -----------------------------------------------
C  ECPT( 1) =  EL ID          EL ID          EL ID
C  ECPT( 2) =  MAT-ID         MAT-ID         MAT-ID
C  ECPT( 3) =  GRID-1         GRID-1         GRID-1
C  ECPT( 4) =  GRID-2         GRID-2         GRID-2
C  ECPT( 5) =  GRID-3         GRID-3         GRID-3
C  ECPT( 6) =  GRID-4         GRID-4         GRID-4
C  ECPT( 7) =  CSID-1         GRID-5         GRID-5
C  ECPT( 8) =  X1             GRID-6         GRID-6
C  ECPT( 9) =  Y1             CSID-1         GRID-7
C  ECPT(10) =  Z1             X1             GRID-8
C  ECPT(11) =  CSID-2         Y1             CSID-1
C  ECPT(12) =  X2             Z1             X1
C  ECPT(13) =  Y2             CSID-2         Y1
C  ECPT(14) =  Z2             X2             Z1
C  ECPT(15) =  CSID-3         Y2             CSID-2
C  ECPT(16) =  X3             Z2             X2
C  ECPT(17) =  Y3             CSID-3         Y2
C  ECPT(18) =  Z3             X3             Z2
C  ECPT(19) =  CSID-4         Y3             CSID-3
C  ECPT(20) =  X4             Z3             X3
C  ECPT(21) =  Y4             CSID-4         Y3
C  ECPT(22) =  Z4             X4             Z3
C  ECPT(23) =  EL-TEM         Y4             CSID-4
C  ECPT(24)                   Z4             X4
C  ECPT(25)                   CSID-5         Y4
C  ECPT(26)                   X5             Z4
C  ECPT(27)                   Y5             CSID-5
C  ECPT(28)                   Z5             X5
C  ECPT(29)                   CSID-6         Y5
C  ECPT(30)                   X6             Z5
C  ECPT(31)                   Y6             CSID-6
C  ECPT(32)                   Z6             X6
C  ECPT(33)                   ELTEMP         Y6
C  ECPT(34)                                  Z6
C  ECPT(35)                                  CSID-7
C  ECPT(36)                                  X7
C  ECPT(37)                                  Y7
C  ECPT(38)
C  ECPT(39)                                  CSID-8
C  ECPT(40)                                  X8
C  ECPT(41)                                  Y8
C  ECPT(42)                                  Z8
C  ECPT(43)                                  EL-TEMP
C*****
      REAL NU
      INTEGER NECPT(100),NPHI(170), M(14,4)
C
      COMMON / SDR2X5/ ECPT(100),PHIOUT(170)
      COMMON / SDR2X6 /       CMAT(18,8)         ,BETA(8)
     1                       ,TEMP(18)           ,ELVOL
     2                       ,VOL                ,FACT
     3                       ,NPTS               ,NEL
     4                       ,MFIRST             ,NROW
     5                       ,ITEST              ,FLOC
     6                       ,J1                 ,JLOC
     7                       ,KPT                ,NTEMP
     8                       ,GE(36)             ,H(4,4)
     9                       ,R(3,3)             ,TI(9)
      COMMON / MATIN / NMAT,MATFLG,ELTEMP
      COMMON / MATOUT/ E,G,NU,RHO,ALFA,TEMPO
C
      EQUIVALENCE (NPHI(1),PHIOUT(1))
      EQUIVALENCE (NECPT(1),ECPT(1))
C
      DATA  M( 1,1),M( 1,2),M( 1,3),M( 1,4)/ 1   ,2   ,3   ,4 /
C
      DATA  M( 2,1),M( 2,2),M( 2,3),M( 2,4)/ 1   ,2   ,3   ,6 /
      DATA  M( 3,1),M( 3,2),M( 3,3),M( 3,4)/ 1   ,2   ,6   ,5 /
      DATA  M( 4,1),M( 4,2),M( 4,3),M( 4,4)/ 1   ,4   ,5   ,6 /
C
      DATA  M( 5,1),M( 5,2),M( 5,3),M( 5,4)/ 1   ,2   ,3   ,6 /
      DATA  M( 6,1),M( 6,2),M( 6,3),M( 6,4)/ 1   ,3   ,4   ,8 /
      DATA  M( 7,1),M( 7,2),M( 7,3),M( 7,4)/ 1   ,3   ,8   ,6 /
      DATA  M( 8,1),M( 8,2),M( 8,3),M( 8,4)/ 1   ,5   ,6   ,8 /
      DATA  M( 9,1),M( 9,2),M( 9,3),M( 9,4)/ 3   ,6   ,7   ,8 /
      DATA  M(10,1),M(10,2),M(10,3),M(10,4)/ 2   ,3   ,4   ,7 /
      DATA  M(11,1),M(11,2),M(11,3),M(11,4)/ 1   ,2   ,4   ,5 /
      DATA  M(12,1),M(12,2),M(12,3),M(12,4)/ 2   ,4   ,5   ,7 /
      DATA  M(13,1),M(13,2),M(13,3),M(13,4)/ 2   ,5   ,6   ,7 /
      DATA  M(14,1),M(14,2),M(14,3),M(14,4)/ 4   ,5   ,7   ,8 /
C
      GO TO (100,110,120,130),ITYPE
C*****
C     THE TYPE OF THE ELEMENT DETERMINES THE FOLLOWING PARAMETERS
C*****
  100 NPTS  =4
      NEL   =1
      MFIRST=1
      GO TO 140
  110 NPTS  =6
      NEL   =3
      MFIRST=2
      GO TO 140
  120 NPTS  =8
      NEL   =5
      MFIRST=5
      GO TO 140
  130 NPTS  =8
      NEL   =10
      MFIRST=5
  140 CONTINUE
C*****
C     ZERO OUT ARRAYS
C*****
      ELVOL =0.0
      DO 200  J =1,NPTS
      BETA(J) =0.0
      DO 200  I =1,18
      CMAT(I,J) = 0.0
  200 CONTINUE
C*****
C     LOOP ON SUBELEMENTS
C*****
      DO 1000 ME =1,NEL
      NROW = MFIRST +ME -1
C*****
C     J  CORRESPONDS TO THE X,Y,AND Z LOCATIONS OF EACH CONNECTED POINT
C*****
      DO 400 J =1,3
      J1 = M(NROW,1)*4 +NPTS +J -1
C*****
C     I  CORRESPONDS TO POINTS 2,3,AND 4
C*****
      DO 400 I= 1,3
      JLOC = M(NROW,I+1)*4 + NPTS + J - 1
C*****
C     ECPT(JLOC) IS THE JTH COMPONENT OF POINT I+1
C*****
  400 R(I,J) = ECPT(JLOC) -ECPT(J1)
C*****
C     INVERT THE GEOMETRY MATRIX EXPLICITLY USING VECTOR OPERATORS
C*****
      CALL SAXB(   R(1,3),R(1,2),TEMP)
      H(2,1) = TEMP(1) + TEMP(2) + TEMP(3)
      H(2,2) = R(2,2)*R(3,3) -R(3,2)*R(2,3)
      H(2,3) = R(3,2)*R(1,3) -R(1,2)*R(3,3)
      H(2,4) = R(1,2)*R(2,3) -R(2,2)*R(1,3)
      CALL SAXB(   R(1,1),R(1,3),TEMP)
      H(3,1) = TEMP(1)+ TEMP(2) +TEMP(3)
      H(3,2) = R(2,3)*R(3,1) -R(3,3)*R(2,1)
      H(3,3) = R(3,3)*R(1,1) -R(1,3)*R(3,1)
      H(3,4) = R(1,3)*R(2,1) -R(2,3)*R(1,1)
      CALL SAXB(   R(1,1),R(1,2),TEMP)
      H(4,1) =-TEMP(1) -TEMP(2)-TEMP(3)
      H(4,2) = R(2,1)*R(3,2) -R(3,1)*R(2,2)
      H(4,3) = R(3,1)*R(1,2) -R(1,1)*R(3,2)
      H(4,4) = R(1,1)*R(2,2) -R(2,1)*R(1,2)
      VOL =   (R(1,3)*TEMP(1) + R(2,3)*TEMP(2) +R(3,3)*TEMP(3))/6.0
      ELVOL = ELVOL +VOL
      DO 500 I = 1,4
      KPT = M(NROW,I)
      BETA(KPT) = BETA(KPT) + VOL
      CMAT(1, KPT) = H(2,I)/6.0  + CMAT(1 ,KPT)
      CMAT(5, KPT) = H(3,I)/6.0  + CMAT(5 ,KPT)
      CMAT(9, KPT) = H(4,I)/6.0  + CMAT(9 ,KPT)
      CMAT(11,KPT) = H(4,I)/6.0  + CMAT(11,KPT)
      CMAT(12,KPT) = H(3,I)/6.0  + CMAT(12,KPT)
      CMAT(13,KPT) = H(4,I)/6.0  + CMAT(13,KPT)
      CMAT(15,KPT) = H(2,I)/6.0  + CMAT(15,KPT)
      CMAT(16,KPT) = H(3,I)/6.0  + CMAT(16,KPT)
      CMAT(17,KPT) = H(2,I)/6.0  + CMAT(17,KPT)
  500 CONTINUE
 1000 CONTINUE
C*****
C     END OF ELEMENT LOOP
C*****
C*****
C     CMAT CONTAINS THE SUM OF THE STRAIN -DISPLACEMENT MATRICES
C                       TIMES THE VOLUME OF THE CONNECTED TETRAHEDRON
C
C     CALL THE MATERIAL  ROUTINE TO OBTAIN PARAMETERS
C*****
      NMAT = NECPT(2)
      MATFLG =1
      ELTEMP = ECPT(5*NPTS+3)
      CALL MAT (ECPT(1))
      FACT = E /((1.0+NU)*(1.0-2.0*NU) )
      DO 1010 I = 1,36
 1010 GE(I) = 0.0
      GE(1) = FACT*(1.0-NU)
      GE(2) = FACT* NU
      GE(3) = GE(2)
      GE(7) = GE(2)
      GE(8) = GE(1)
      GE(9) = GE(2)
      GE(13)= GE(2)
      GE(14)= GE(2)
      GE(15)= GE(1)
      GE(22)= G
      GE(29)= G
      GE(36)= G
C*****
C     EACH CMAT MATRIX IS PREEMULTIPLIED BY THE STRESS-STRAIN GE MATRIX
C         AND DIVIDED BY THE SUM OF THE VOLUMES.
C     IF NECESSARY THE MATRIX IS POST-MULTIPLIED BY A GLOBAL TRANSFORM T
C
C     LOOP ON GRID POINTS
C*****
      DO 2000 I =1,NPTS
      NPHI(I+1) = NECPT(I+2)
      K= NPTS+I +8
      PHIOUT(K) =  BETA(I)/(4.0*ELVOL)
      ICORD = NPTS +4*I -1
      DO 1100 J =1,18
 1100 CMAT(J,I) = CMAT(J,I)/ELVOL
      K= NPTS*2 +18*I -9
      IF(NECPT(ICORD).NE.0) GO TO 1200
      CALL GMMATS( GE,6,6,0,CMAT(1,I),6,3,0,PHIOUT(K) )
      GO TO 2000
 1200 CALL TRANSS(NECPT(ICORD), TI )
      CALL GMMATS( CMAT(1,I),6,3,0,TI,3,3,0,TEMP)
      CALL GMMATS( GE,6,6,0,TEMP,6,3,0, PHIOUT(K) )
 2000 CONTINUE
C
      NPHI(1) = NECPT(1)
      PHIOUT(NPTS+2) = TEMPO
      TEMP(1)= ALFA
      TEMP(2)= ALFA
      TEMP(3)= ALFA
      TEMP(4)= 0.0
      TEMP(5)= 0.0
      TEMP(6)= 0.0
C*****
C     THE THERMAL EXPANSION VECTOR IS MULTIPLIED BY THE STRESS-STRAIN
C       MATRIX,GE
C*****
      CALL GMMATS( GE,6,6,0,TEMP(1),6,1,0,PHIOUT(NPTS+3) )
C*****
C     THE OUTPUT ARRAY IS NOW COMPLETE
C*****
      RETURN
C*****
C     PHIOUT CONTAINS THE FOLLOWING WHERE N IS THE NUMBER OF CORNERS
C
C              ELEMENT ID
C              N SILS
C              T SUB 0
C              6 THERMAL STRESS COEFFICIENTS
C              N VOLUME RATIO COEFFICIENTS
C              N 6 BY 3 MATRICES RELATING STRESS TO DISPLACEMENTS
C
C*****
      END
