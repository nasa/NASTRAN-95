      SUBROUTINE KTUBE
C*****
C THE TUBE BEING SO SIMILAR TO THE ROD, WE ALTER THE ECPT FOR THE TUBE
C SO THAT IT IS IDENTICAL TO THE ONE FOR THE ROD AND THEN CALL KROD
C TO COMPUTE THE ELEMENT STIFFNESS MATRICES.
C*****
C
C
C
C                      E C P T  F O R  T H E  T U B E
C
C
C
C ECPT( 1)  -  ELEMENT ID.
C ECPT( 2)  -  SCALAR INDEX NUMBER FOR GRID POINT A
C ECPT( 3)  -  SCALAR INDEX NUMBER FOR GRID POINT B
C ECPT( 4)  -  MATERIAL ID.
C ECPT( 5)  -  OUTSIDE DIAMETER
C ECPT( 6)  -  THICKNESS
C ECPT( 7)  -  NON-STRUCTURAL MASS
C ECPT( 8)  -  COOR. SYS. ID. FOR GRID POINT A
C ECPT( 9)  -  BASIC COORDINATES OF GRID POINT A
C ECPT(10)  -                ...
C ECPT(11)  -                ...
C ECPT(12)  -  COOR. SYS. ID. FOR GRID POINT B
C ECPT(13)  -  BASIC COORDINATES OF GRID POINT B
C ECPT(14)  -                ...
C ECPT(15)  -                ...
C ECPT(16)  -  ELEMENT TEMPERATURE
C
C
C
      COMMON   /SMA1ET/
     1                   ECPT(16)           ,DUM(84)
C
C
C
      COMMON   /SMA1DP/
     1                   TEMP               ,A
     2,                  FJ                 ,C
C
C
C
      COMMON /CONDAS/    PI       ,TWOPI    ,RADEG    ,DEGRA    ,
     1                   S4PISQ
C
C
C
      TEMP = ECPT(5) - ECPT(6)
C
C COMPUTE AREA, TORSIONAL INERTIA AND STRESS COEFFICIENT.
C
      A = TEMP * ECPT(6) * PI
      FJ = .25 * A * (TEMP**2  +  ECPT(6)**2)
      C  = .5  * ECPT(5)
C
C MOVE THE -END- OF THE ARRAY -DOWN ONE SLOT- SO THAT ENTRIES 7 THRU 16
C OF THE ECPT WILL BE STORED AT POSITIONS 8 THRU 17.
C
      M = 18
      DO 10 I = 1,10
      M = M - 1
   10 ECPT(M) = ECPT(M-1)
      ECPT(5) = A
      ECPT(6) = FJ
      ECPT(7) = C
      CALL KROD
      RETURN
      END
