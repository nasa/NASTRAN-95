      SUBROUTINE FTUBE
C
C     THIS IS THE FLUID TUBE ELEMENT IN HEAT TRANSFER.
C     IT COMPUTES AND OUTPUTS THE CONDUCTIVITY AND/OR CAPACITY MATRICES
C     OF THE ELEMENT.
C
C     - SINGLE AND DOUBLE PRECISION VERSION -
C
C     EST ENTRY FOR -FTUBE- ELEMENT.
C     ==============================
C
C     EST( 1) = ELEMENT ID
C     EST( 2) = SIL-A
C     EST( 3) = SIL-B
C     EST( 4) = HEAT CAPACITY/UNIT VOLUME = RHO C
C     EST( 5) = VOLUME FLOW RATE = VDOT         P
C     EST( 6) = DIAMETER AT A
C     EST( 7) = DIAMETER AT B = DIAMETER AT A IF NOT DEFINED.
C     EST( 8) = CSID-A  NOT USED
C     EST( 9) = XA
C     EST(10) = YA
C     EST(11) = ZA
C     EST(12) = CSID-B  NOT USED
C     EST(13) = XB
C     EST(14) = YB
C     EST(15) = ZB
C     EST(16) = AVG TEMP OF ELEMENT.  NOT USED.
C
C
      LOGICAL          HEAT     ,ERROR
      INTEGER          DICT(7)  ,ESTID    ,IEST(1)
      REAL             RK(4)    ,ID1      ,ID2      ,DICT5
      DOUBLE PRECISION K(4)     ,LENGTH
      CHARACTER        UFM*23   ,UWM*25   ,UIM*29
      COMMON /XMSSG /  UFM      ,UWM      ,UIM
      COMMON /SYSTEM/  SYSBUF   ,IOUTPT
      COMMON /EMGPRM/  DUM15(15),KMB(3)   ,IPREC    ,ERROR    ,HEAT
      COMMON /EMGDIC/  DMMM(4)  ,ESTID
      COMMON /EMGEST/  EST(16)
      COMMON /CONDAS/  PI
      EQUIVALENCE      (IEST(1) , EST(1)) ,(RK(1)   , K(1))   ,
     1                 (DICT(5) , DICT5 )
C
      IF (.NOT.HEAT) GO TO 240
      DICT(1) = ESTID
      DICT(2) = 1
      DICT(3) = 2
      DICT(4) = 1
      DICT5   = 0.0
      IF (KMB(1) .EQ. 0) GO TO 170
C
C     CONDUCTIVITY
C
      RHOCP = EST(4)
      VDOT  = EST(5)
C
C     STORE CONDUCTIVITY BY COLUMNS
C
      K(1) = DBLE(RHOCP*VDOT)
C
      K(2) = -K(1)
      K(3) = 0.0D0
      K(4) = 0.0D0
C
C     OUTPUT VIA EMGOUT THE FULL MATRIX IN GLOBAL, UNSYMETRIC
C
      IFIL = 1
      ISZE = 4
      ASSIGN 170 TO IRTN
      IF (IPREC .EQ. 2) GO TO 160
  150 RK(1) = SNGL(K(1))
      RK(2) = SNGL(K(2))
      RK(3) = SNGL(K(3))
      RK(4) = SNGL(K(4))
  160 CALL EMGOUT (RK(1),K(1),ISZE,1,DICT,IFIL,IPREC)
      GO TO IRTN, (170,240)
C
C     CAPACITY MATRIX
C
  170 IF (KMB(3) .EQ. 0) GO TO 240
      RHOCP = EST( 4)
      VDOT  = EST( 5)
      ID1   = EST( 6)
      IF (EST(7)) 190,180,190
  180 ID2 = ID1
      GO TO 200
  190 ID2   = EST( 7)
  200 XA    = EST( 9)
      YA    = EST(10)
      ZA    = EST(11)
      XB    = EST(13)
      YB    = EST(14)
      ZB    = EST(15)
      LENGTH = DBLE((XB-XA))**2 + DBLE((YB-YA))**2 + DBLE((ZB-ZA))**2
      IF (LENGTH .GT. 0.0D0) GO TO 220
      LENGTH = DSQRT(LENGTH)
      WRITE  (IOUTPT,210) UIM,IEST(1)
  210 FORMAT (A29,' FROM ELEMENT FTUBE -', /5X,'ELEMENT WITH ID =',I9,
     1       ' HAS A ZERO LENGTH.')
      ERROR = .TRUE.
C
C     FILL AND OUTPUT CAPACITY MATRIX BY COLUMNS IN GLOBAL, SYMMETRIC.
C
  220 K(1) = (DBLE(RHOCP*PI*(ID1+ID2)))**2*LENGTH/32.0D0
      K(2) = 0.0D0
      K(3) = 0.0D0
      K(4) = K(1)
      DICT(2) = 2
      IFIL = 3
      ISZE = 2
      ASSIGN 240 TO IRTN
      IF (IPREC-1) 240,150,160
C
  240 RETURN
      END
