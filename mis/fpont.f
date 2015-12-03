      SUBROUTINE FPONT
C
C     DOES DIRECT,TPONT,FPONT,AND SCALAR LOADS
C
      INTEGER         GPID,SLT,PONT(5),SWLOAD(2)
      DIMENSION       IGPCO(4,5),GPCO1(3),GPCO2(3),GPCO3(3),GPCO4(3),
     1                VECT1(3),VECT2(3),IORD(5),VECT(3),GRIDP(7)
      COMMON /LOADX / LC,SLT,BG,OLD,N(12),IFM
      COMMON /ZZZZZZ/ CORE(1)
      EQUIVALENCE     (GPID,GRIDP(2)), (GRIDP(4),IP1), (GRIDP(5),IP2),
     1                (GRIDP(6),IP3) , (GRIDP(7),IP4),
     2                (IGPCO(2,1),GPCO1(1)), (IGPCO(2,2),GPCO2(1)),
     3                (IGPCO(2,3),GPCO3(1)), (IGPCO(2,4),GPCO4(1)),
     4                (ICOSYT,GRIDP(3))
      DATA    SWLOAD/ 4HFPON,4HT   /
C
      NR = 6
      NP = 5
      MINUS = 5
   10 CALL READ (*120,*130,SLT,GRIDP(2),NR,0,FLAG)
      SCALE   = GRIDP(3)
      PONT(1) = IP1
      PONT(2) = IP2
      IF (NP .EQ. 3) GO TO 20
      PONT(3) = IP3
      PONT(4) = IP4
   20 PONT(NP)= GPID
      CALL PERMUT (PONT(1),IORD(1),NP,OLD)
      DO 30 I = 1,NP
      L = IORD(I)
   30 CALL FNDPNT (IGPCO(1,L),PONT(L))
      IF (NP .EQ. 3) GO TO 50
      DO 40 I = 1,3
      VECT1(I) = GPCO2(I) - GPCO1(I)
   40 VECT2(I) = GPCO4(I) - GPCO3(I)
      CALL CROSS (VECT1(1),VECT2(1),VECT(1))
      GO TO 70
   50 DO 60 I = 1,3
   60 VECT(I) = GPCO2(I) - GPCO1(I)
   70 CALL NORM (VECT(1),XL)
   80 IF (IGPCO(1,NP)) 90,100,90
   90 CALL BASGLB (VECT(1),VECT(1),IGPCO(2,NP),IGPCO(1,NP))
  100 CALL FNDSIL (GPID)
      GPID = GPID + (IFM-MINUS)*3 - 1
      DO 110 I = 1,3
      IN = GPID + I
      CORE(IN) = CORE(IN) + VECT(I)*SCALE
  110 CONTINUE
      GO TO 150
  120 N1 = -2
      GO TO 140
  130 N1 = -3
  140 IPARM = SLT
      CALL MESAGE (N1,IPARM,SWLOAD)
  150 RETURN
C
C
      ENTRY TPONT
C     ===========
C
C     TPONT PROCESSES FORCE1 AND MOMENT1 CARDS
C
      NR = 4
      NP = 3
      MINUS = 3
      GO TO 10
C
C
      ENTRY DIRECT
C     ============
C
C     DIRECT PROCESSES FORCE+ MOMENT CARDS
C
      NP = 1
      MINUS = 1
      CALL READ (*120,*130,SLT,GRIDP(2),6,0,FLAG)
      DO 170 I = 1,3
  170 VECT(I) = GRIDP(I+4)
      CALL FNDPNT (IGPCO(1,1),GPID)
      SCALE = GRIDP(4)
      IF (ICOSYT .EQ. IGPCO(1,NP)) GO TO 100
      IF (ICOSYT) 180,80,180
  180 CALL GLBBAS (VECT(1),VECT(1),IGPCO(2,1),ICOSYT)
      GO TO 80
C
C
      ENTRY SLOAD
C     ===========
C
C     SLOAD PROCESSES SLOAD CARDS
C
      CALL READ (*120,*130,SLT,GRIDP(2),2,0,FLAG)
      CALL FNDSIL (GPID)
      CORE(GPID) = CORE(GPID) + GRIDP(3)
      GO TO 150
      END
