      SUBROUTINE Q2BCS (EST,PLANAR,RMAT,ET,IERROR)
C
C     BASIC CALCULATIONS ARE PERFORMED FOR THE QDMEM2 ELEMENT IN THIS
C     ROUTINE (SINGLE-PRECISION VERSION)
C
      LOGICAL     PLANAR
      REAL        EST(1)
      REAL        MAG   ,D12(3),G1(3) ,IAREA  ,D13(3) ,GRID(3,5),
     1            G2(3) ,ITWOH ,D24(3),VEC(3) ,G3(3)  ,ET(3,3)  ,
     2            G5(3) ,G4(3) ,SADOTB,RMAT(3,5)
      EQUIVALENCE (GRID(1,1),G1(1)),(GRID(1,2),G2(1)),(GRID(1,3),G3(1)),
     1            (GRID(1,4),G4(1)),(GRID(1,5),G5(1))
C
C     MOVE GRID COORDINATES
C
      DO 10 I = 1,3
      G1(I) = EST(I+10)
      G2(I) = EST(I+14)
      G3(I) = EST(I+18)
      G4(I) = EST(I+22)
   10 CONTINUE
C
C     FORM  D   , D   AND  D   VECTORS
C            13    24       12
C
      DO 20 I = 1,3
      D12(I) = G2(I) - G1(I)
      D13(I) = G3(I) - G1(I)
      D24(I) = G4(I) - G2(I)
   20 CONTINUE
C
C     NVEC  =  D13 CROSS D24 = K-VECTOR (UN-NORMALIZED)
C
      CALL SAXB (D13,D24,VEC)
      MAG   =  SQRT (SADOTB(VEC,VEC))
      IAREA = 0.5*MAG
C
C     NORMALIZE K-VECTOR
C
      IF (MAG) 100,100,30
   30 ET(1,3) = VEC(1)/MAG
      ET(2,3) = VEC(2)/MAG
      ET(3,3) = VEC(3)/MAG
C
C     H = .5 * (D   DOT K-VEC)
C                12
C
      ITWOH = SADOTB(D12,ET(1,3))
C
C     I-VECTOR (UN-NORMALIZED) = (D  ) - 2 H (K-VECTOR)
C                                  12
C
      DO 40 I = 1,3
      VEC(I) = D12(I) - ITWOH*ET(I,3)
   40 CONTINUE
      MAG =  SQRT(SADOTB(VEC,VEC))
C
C     NORMALIZE I-VECTOR
C
      IF (MAG) 100,100,50
   50 ET(1,1) = VEC(1)/MAG
      ET(2,1) = VEC(2)/MAG
      ET(3,1) = VEC(3)/MAG
C
C     JVEC = KVEC CROSS IVEC
C
      CALL SAXB (ET(1,3),ET(1,1),ET(1,2))
C
C     FILL THE SUB-TRIANGLE ELEMENT COORDINATE MATRIX
C
      DO 60 I = 1,3
      G5(I) = 0.25*(G1(I) + G2(I) + G3(I) + G4(I))
   60 CONTINUE
      RMAT(1,1) = 0.0
      RMAT(2,1) = 0.0
      RMAT(3,1) =-ITWOH/2.0
      DO 70 I = 2,5
      VEC(1) = GRID(1,I) - G1(1)
      VEC(2) = GRID(2,I) - G1(2)
      VEC(3) = GRID(3,I) - G1(3)
      CALL GMMATS (ET,3,3,0, VEC,3,1,0, RMAT(1,I))
      RMAT(1,I) = RMAT(1,I) + RMAT(1,1)
      RMAT(2,I) = RMAT(2,I) + RMAT(2,1)
      RMAT(3,I) = RMAT(3,I) + RMAT(3,1)
   70 CONTINUE
C
C     SET PLANAR FLAG .TRUE. OR .FALSE.
C
      IF ((ITWOH/2.0)**2/IAREA .LE. 0.01) GO TO 80
      PLANAR = .FALSE.
      GO TO 90
   80 PLANAR = .TRUE.
C
C     ALL BASIC CALCULATIONS NOW COMPLETE
C
   90 IERROR = 0
      RETURN
C
C     ERROR CONDITION, BAD ELEMENT GEOMETRY.
C
  100 IERROR = 1
      RETURN
      END
