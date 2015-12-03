      SUBROUTINE T3GEMS (IERR,EGPDT,IORDER,GB,GS,LX,LY,EDGLEN,SHRFLX,
     1                   AIC,JOG,JOK,K11,K22)
C
C     SINGLE PRECISION ROUTINE TO SET UP THE REQUIRED SHEAR-RELATED
C     TRANSFORMATION TO RELIEVE THE TRIA3 GEOMETRY BIAS IN BENDING.
C
C     INPUT :
C           EGPDT  - BGPDT DATA IN ELEMENT COORD. SYSTEM
C           IORDER - ARRAY OF ORDER INDICATORS FOR REARRANGED DATA
C           GB     - ARRAY OF BENDING MATERIAL PROPERTIES
C           GS     - ARRAY OF SHEAR   MATERIAL PROPERTIES
C           LX     - DIMENSION OF ELEMENT ALONG X-AXIS
C           LY     - DIMENSION OF ELEMENT ALONG Y-AXIS
C           EDGLEN - EDGE LENGTHS
C           SHRFLX - LOGICAL INDICATING THE PRESENCE OF SHEAR FLEX
C     OUTPUT:
C           IERR   - ERROR FLAG
C           AIC    - TRANSFORMATION TO RELIEVE GEOMETRY BIAS
C           JOG    - SHEAR   STIFFNESS FACTOR
C           JOK    - BENDING STIFFNESS FACTOR
C           K11    - BENDING STIFFNESS FACTOR
C           K22    - BENDING STIFFNESS FACTOR
C
C
C     [C]    - TRANSFORMATION TO YIELD GAMMAT ALONG THE ELEMENT SIDES.
C
C     [AA]   - TRANSFORMATION FROM GAMMA0 (AT THE ELEMENT CENTER) TO
C              GAMMAT (ALONG THE ELEMENT SIDES).
C
C                  -1
C     [AIC]  - [AA]  [C]
C
C
      LOGICAL  SHRFLX
      INTEGER  IORDER(3),INDEX(3,3)
      REAL     EGPDT(4,3),GB(9),GS(4),EDGLEN(3),LX,LY,AIC(18),
     1         JOG,JOK,K11,K22,XX(3),YY(3),AA(9),H1,H2,BDUM(3),
     2         DETERM,COSA,SINA,COSB,SINB,COSC,SINC
C
C
      IERR = 0
      DO 20 I = 1,3
      DO 10 J = 1,3
      JO = IORDER(J)
      IF (I .NE. JO) GO TO 10
      XX(I) = EGPDT(2,J)
      YY(I) = EGPDT(3,J)
   10 CONTINUE
   20 CONTINUE
C
      COSA = ((XX(2)-XX(1))/EDGLEN(1))
      SINA = ((YY(2)-YY(1))/EDGLEN(1))
      COSB = ((XX(3)-XX(2))/EDGLEN(2))
      SINB = ((YY(3)-YY(2))/EDGLEN(2))
      COSC = ((XX(1)-XX(3))/EDGLEN(3))
      SINC = ((YY(1)-YY(3))/EDGLEN(3))
C
      AA(1) = SINA
      AA(2) = COSA
      AA(3) = 1.0
      AA(4) = SINB
      AA(5) = COSB
      AA(6) = 1.0
      AA(7) = SINC
      AA(8) = COSC
      AA(9) = 1.0
C
      CALL INVERS (3,AA,3,BDUM,0,DETERM,ISING,INDEX)
      IF (ISING .NE. 1) GO TO 30
C
      AIC( 1) = AA(1)*SINA
      AIC( 2) = AA(1)*COSA
      AIC( 3) = AA(2)*SINB
      AIC( 4) = AA(2)*COSB
      AIC( 5) = AA(3)*SINC
      AIC( 6) = AA(3)*COSC
      AIC( 7) = AA(4)*SINA
      AIC( 8) = AA(4)*COSA
      AIC( 9) = AA(5)*SINB
      AIC(10) = AA(5)*COSB
      AIC(11) = AA(6)*SINC
      AIC(12) = AA(6)*COSC
      AIC(13) = AA(7)*SINA
      AIC(14) = AA(7)*COSA
      AIC(15) = AA(8)*SINB
      AIC(16) = AA(8)*COSB
      AIC(17) = AA(9)*SINC
      AIC(18) = AA(9)*COSC
C
C     CALCULATE THE BENDING STIFFNESS FACTORS
C
      H1  = LY
      H2  = LX
      K11 = 1.0/(H1*H1)*GB(5)
      K22 = 1.0/(H2*H2)*GB(1)
C
      JOK = K11*K22
      IF (JOK .NE. 0.0) JOK = 1.0/JOK
      JOG = 0.0
      IF (SHRFLX) JOG = GS(1)*GS(4) - GS(2)*GS(3)
      IF (JOG .NE. 0.0) JOG = 1.0/JOG
      GO TO 40
C
   30 IERR = 1
   40 RETURN
      END
