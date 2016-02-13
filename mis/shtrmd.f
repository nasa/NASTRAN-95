      SUBROUTINE SHTRMD (*,ELID,MM,NNODE,XI,ETA,GPTH,EPNORM,EGPDT,
     1                   IORDER,MMN,DETERM,TH,SHP,TIE,BTERMS)
C
C     TO CONSTRUCT THE JACOBIAN, SET UP INTEGRATION POINT COORD.
C     SYSTEM, AND CALCULATE [B] TERMS FOR THE ISOPARAMETRIC QUADRATIC
C     QUAD8 AND TRIA6 SHELL ELEMENTS
C     =====     =====
C
C        *********************************************************
C        *                                                       *
C        *  PRESENTLY COSMIC/NASTRAN DOES NOT USE THIS ROUTINE   *
C        *                                                       *
C        *********************************************************
C
C     INPUT :
C           ELID     - ELEMENT ID
C           MM       - MAXIMUM NO. OF NODES FOR THIS TYPE ELEMENT
C           NNODE    - THE NO. OF PRESENT NODES
C           XI
C           ETA      - INTEGERATION POINT COORDINATES
C           GPTH     - GRID POINT THICKNESSES
C           EPNORM   - GRID POINT NORMALS IN ELEMENT COORD. SYSTEM
C           EGPDT    - GRID POINT DATA IN ELEMENT COORD. SYSTEM
C           IORDER   - REORDERING ARRAY
C           MMN      - MISSING MIDSIDE NODE INDICATOR
C     OUTPUT:
C           DETERM   - DETERMINANT OF JACOBIAN
C           TH       - THICKNESS AT THIS INTEGRATION POINT
C           SHP      - ARRAY OF SHAPE FUNCTIONS
C           TIE      - TRANSFORMATION BETWEEN INTEG. PT. AND ELEMENT
C                      COORD. SYSTEMS
C           BTERMS   - TERMS OF [B] (DERIVATIVES OF SHAPE FUNCTIONS
C                      WITH RESPECT TO XYZ OF ELEMENT COORD. SYSTEM)
C
      LOGICAL          BADJAC
      INTEGER          IORDER(1),MMN(1),ELID
      DOUBLE PRECISION EGPDT(4,1),EPNORM(4,1),DETERM,XI,ETA,HZTA,DETJ,
     1                 SHP(10),JACOB(3,3),DSHPX(10),DSHPE(10),DSHP(16),
     2                 TSHP(8),TDSHP(16),BTERMS(1),PSITRN(9),TIE(9),
     3                 GPTH(1),TH
      COMMON /Q8T6DT/  DETJ,HZTA,PSITRN,NN1,BADJAC,NN2
C
C
C     DOUBLE PRECISION VERSION
C
      IF (MM .EQ. 8) GO TO 10
      IF (MM .EQ. 6) GO TO 20
      GO TO 80
C
C     QUAD8 VERSION
C
   10 CALL Q8SHPD (MMN,XI,ETA,SHP,DSHP)
      GO TO 30
C
C     TRIA6 VERSION
C
   20 CALL T6SHPD (XI,ETA,MMN,SHP,DSHPX,DSHPE)
      DO 25 I = 1,MM
      DSHP(I   ) = DSHPX(I)
   25 DSHP(I+MM) = DSHPE(I)
C
   30 DO 40 I = 1,MM
      TSHP (I) = SHP (I)
      TDSHP(I) = DSHP(I)
   40 TDSHP(I+MM) = DSHP(I+MM)
      DO 45 I = 1,MM
      IO = IORDER(I)
      SHP (I) = TSHP(IO)
      DSHP(I) = TDSHP(IO)
   45 DSHP(I+MM) = TDSHP(IO+MM)
C
      TH = 0.0D0
      DO 50 I = 1,NNODE
   50 TH = TH + GPTH(I)*SHP(I)
C
      NN1 = NNODE
      NN2 = MM
      HZTA = 0.0D0
      CALL JACOBD (ELID,SHP,DSHP,GPTH,EGPDT,EPNORM,JACOB)
      IF (BADJAC) GO TO 80
C
      DETERM = DETJ
      DO 60 I = 1,9
   60 TIE(I) = PSITRN(I)
C
      IJ = 1
      DO 70 I = 1,2
      DO 70 J = 1,NNODE
      BTERMS(IJ) = JACOB(I,1)*DSHP(J) + JACOB(I,2)*DSHP(J+MM)
   70 IJ = IJ + 1
      RETURN
C
   80 RETURN 1
C
C
      ENTRY SHTRMS (*,ELID,MM,NNODE,XI,ETA,GPTH,EPNORM,EGPDT,
     1                   IORDER,MMN,DETERM,TH,SHP,TIE,BTERMS)
C     =======================================================
C
C     SINGLE PRELCISION VERSION
C
      GO TO 80
      END
