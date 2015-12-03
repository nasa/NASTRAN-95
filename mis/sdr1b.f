      SUBROUTINE SDR1B (IPVECT,IM1,IM2,IOUT,MAJOR,SUB1,SUB2,IUSET,
     1                  IOPT,IOUT1)
C
      INTEGER NAME(2),CORE(7),SYSBUF,IPV1(7)
      COMMON /SYSTEM/ SYSBUF
      COMMON /ZZZZZZ/ KORE(1)
      COMMON /PATX  / NZ,NSUB1,NSUB2,NSUB3,IUSET1
      COMMON /PARMEG/ IA(7),IA11(7),IA12(7),IB11(7),IB12(7),NZ1,IRULE
      COMMON /UNPAKX/ ITU1,IIU1,JJU1,INCR1
      COMMON /PACKX / ITP1,ITP2,IIP1,JJP1,INCR
      EQUIVALENCE     (CORE(1),KORE(1))
      DATA    NAME  / 4HSDR1,4HB   /
C
C
      NZ  = KORSZ(CORE)
      NZ1 = NZ
      IUSET1 = IUSET
      DO 10 I = 2,7
      IA11(I) = 0
      IA12(I) = 0
      IA(I)   = 0
   10 CONTINUE
      IA11(1) = IM1
      IF (IM1 .EQ. 0) GO TO 20
      CALL RDTRL (IA11)
   20 IA12(1) = IM2
      CALL RDTRL (IA12)
      IF (IA11(1).LT.0 .AND. IA12(1).LT.0) RETURN
      CALL CALCV (IPVECT,MAJOR,SUB1,SUB2,CORE)
      IF (IOPT .NE. 0) GO TO 60
      IF (IA12(1) .LE. 0) IA12(1) = 0
   30 IB11(1) = 0
      IB12(1) = 0
      IA(3) = NSUB1 + NSUB2 + NSUB3
      IA(2) = MAX0(IA11(2),IA12(2))
      IA(4) = 2
      IF (IM2 .EQ. 0) IA12(5) = IA11(5)
      IPREC = MIN0(1-MOD(IA11(5),2),1-MOD(IA12(5),2))
      ITYPE = 1
      IF (IA11(5).GT.2 .OR. IA12(5).GT.2) ITYPE = 3
      IA(5) = IPREC + ITYPE
   40 IRULE = 0
      IA(1) = IOUT
      IPV1(1) = IPVECT
      CALL RDTRL (IPV1)
      CORE(1) = 0
      CORE(2) = 1
      CORE(3) = IA(2)
      CORE(4) = 2
      CORE(5) = 1
      CORE(6) = 0
      CORE(7) = 0
      CALL MERGE (CORE,IPV1,CORE)
      CALL WRTTRL (IA)
      RETURN
C
C     EXPAND YS
C
   60 NZ = NZ - SYSBUF
      CALL OPEN (*130,IM2,CORE(NZ+1),0)
      NZ = NZ - SYSBUF
      CALL OPEN (*150,IOUT1,CORE(NZ+1),1)
      CALL FNAME (IM2,CORE)
      CALL WRITE (IOUT1,CORE,2,1)
      IA(1) = IM2
      CALL RDTRL (IA)
      NOYS  = IA(2)
      IA(2) = 0
      IA(1) = IOUT1
      IA(6) = 0
      IA(7) = 0
      CALL FWDREC (*130,IM2)
      NLOAD = IA11(2)
      ITU1  = IA(5)
      INCR  = 1
      ITP1  = ITU1
      ITP2  = ITP1
      INCR1 = 1
      DO 100 I = 1,NLOAD
      IF (I .GT. NOYS) GO TO 81
      IIU1 = 0
      CALL UNPACK (*80,IM2,CORE)
      IIP1 = IIU1
      JJP1 = JJU1
   81 CALL PACK( CORE,IOUT1,IA)
      GO TO 100
   80 CORE(1) = 0
      CORE(2) = 0
      CORE(3) = 0
      CORE(4) = 0
      IIP1 = 1
      JJP1 = 1
      GO TO 81
  100 CONTINUE
      CALL CLOSE (IOUT1,1)
      CALL CLOSE (IM2,1)
      CALL WRTTRL (IA)
      IA12(1) = IOUT1
      CALL RDTRL (IA12)
      GO TO 30
C
C
      ENTRY SDR1C (IPVECT,IM1,IOUT)
C     =============================
C
C     EXPAND ROWS OF IM1 TO D SET SIZE
C
      DO 120 I = 1,7
      IA12(I) = 0
      IB11(I) = 0
      IB12(I) = 0
  120 CONTINUE
      IA11(1) = IM1
      CALL RDTRL (IA11)
      IA(1) = IM1
      CALL RDTRL (IA)
      IA(3) = NSUB1 + NSUB2 + NSUB3
      GO TO 40
C
C     ERROR MESAGES
C
  130 IP1 = -1
      IP2 = IM2
  140 CALL MESAGE (IP1,IP2,NAME)
  150 IP1 = -1
      IP2 = IOUT1
      GO TO 140
      END
