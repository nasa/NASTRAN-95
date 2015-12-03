      SUBROUTINE HRING (POINTS)
C
C     HEAT CONDUCTIVITY SMA1 ROUITNE FOR TRIANGULAR (POINTS=3) AND
C     TRAPEZOIDAL (POINTS=4) RING ELEMENTS.
C     THIS ROUTINE IS SEPARATE FROM KTRAPR AND KTRIRG SO AS TO BE
C     IN OVERLAY WITH KTRMEM AND KQDMEM.
C
      LOGICAL         NOGO
      INTEGER         POINTS  ,OUTPT   ,SYSBUF  ,TINT    ,MAP(15)
      CHARACTER       UFM*23  ,UWM*25  ,UIM*29  ,SFM*25  ,SWM*27
      COMMON /XMSSG / UFM     ,UWM     ,UIM     ,SFM     ,SWM
      COMMON /SYSTEM/ SYSBUF  ,OUTPT   ,NOGO
      COMMON /SMA1ET/ ECPT(100)
      EQUIVALENCE     (T,TINT)
      DATA    PI23  / 2.0943951024E0 /
      DATA    MAP   / 1,2,3, 1,2,3, 2,3,4, 3,4,1, 4,1,2 /
C
C     ECPT LISTS
C
C     ECPT     TRIRG -------- TRMEM          TRAPRG ------- QDMEM
C     ===========================================================
C      1       EL-ID          EL-ID          EL-ID          EL-ID
C      2       SIL-1          SIL-1          SIL-1          SIL-1
C      3       SIL-2          SIL-2          SIL-2          SIL-2
C      4       SIL-3          SIL-3          SIL-3          SIL-3
C      5       THETA          THETA          SIL-4          SIL-4
C      6       MATID          MATID          THETA          THETA
C      7       CSID-1         T              MATID          MATID
C      8       X1             NS-MASS        CSID-1         T
C      9       Y1             CSID-1         X1             NS-MASS
C     10       Z1             X1             Y1             CSID-1
C     11       CSID-2         Y1             Z1             X1
C     12       X2             Z1             CSID-2         Y1
C     13       Y2             CSID-2         X2             Z1
C     14       Z2             X2             Y2             CSID-2
C     15       CSID-3         Y2             Z2             X2
C     16       X3             Z2             CSID-3         Y2
C     17       Y3             CSID-3         X3             Z2
C     18       Z3             X3             Y3             CSID-3
C     19       AVG-TEMP       Y3             Z3             X3
C     20                      Z3             CSID-4         Y3
C     21                      AVG-TEMP       X4             Z3
C     22                                     Y4             CSID-4
C     23                                     Z4             X4
C     24                                     AVG-TEMP       Y4
C     25                                                    Z4
C     26                                                    AVG-TEMP
C
C     GEOMETRY CHECKS  X  MUST BE .GT.0, AND Y  = 0  FOR I = 1,2,..,PTS.
C                       I                     I
C
      I1 = POINTS + 4
      I2 = I1 + 4*POINTS - 1
      DO 100 I = I1,I2,4
      IF (ECPT(I+1)) 900,900,90
   90 IF (ECPT(I+2)) 900,100,900
  100 CONTINUE
C
C     POINT ORDERING CHECK.
C
      IF (POINTS .EQ. 4) GO TO 200
      I1 = 1
      I2 = 3
      GO TO 300
  200 I1 = 4
      I2 = 15
  300 JPOINT = POINTS + 1
      DO 600 I = I1,I2,3
      IR = MAP(I  )*4 + JPOINT
      IS = MAP(I+1)*4 + JPOINT
      IT = MAP(I+2)*4 + JPOINT
      TEMP = (ECPT(IS) - ECPT(IR))*(ECPT(IT+2) - ECPT(IS+2))  -
     1       (ECPT(IT) - ECPT(IS))*(ECPT(IS+2) - ECPT(IR+2))
      IF (TEMP) 900,900,600
  600 CONTINUE
C
C     TRAPEZOID TEST.
C
      IF (POINTS .NE. 4) GO TO 700
      IF (ECPT(11)-ECPT(15)) 650,640,650
  640 IF (ECPT(19)-ECPT(23)) 650,670,650
  650 CALL PAGE2 (-2)
      WRITE  (OUTPT,660) SWM,ECPT(1)
  660 FORMAT (A27,' 2158, A TRAPRG ELEMENT =',I14,
     1       ' DOES NOT HAVE SIDE 1-2 PARALLEL TO SIDE 3-4.')
C
C     THICKNESS OF TRMEM OR QDMEM TO BE CALLED BELOW.
C     QDMEM WILL SUBDIVIDE THICKNESS FOR SUB-TRIANGLES AND THUS
C     T IS SET = INTEGER 1 AS A FLAG TO QDMEM ROUTINE WHICH WILL
C     COMPUTE T FOR EACH.
C
  670 TINT = 1
      TINT = TINT
      GO TO 750
  700 T = PI23*(ECPT(8) + ECPT(12) + ECPT(16))
C
C     CONVERT ECPT TO THAT OF A TRMEM OR QDMEM.
C
  750 J = 5*POINTS + 6
      K = 4*POINTS + 1
      DO 800 I = 1,K
      ECPT(J) = ECPT(J-2)
      J = J - 1
  800 CONTINUE
      ECPT(POINTS+4) = T
      ECPT(POINTS+5) = 0.0
      IF (POINTS .EQ. 4) GO TO 850
      CALL KTRMEM (0)
      RETURN
C
  850 CALL KQDMEM
      RETURN
C
C     BAD GEOMETRY FATAL ERROR.
C
  900 WRITE  (OUTPT,910) UFM,ECPT(1)
  910 FORMAT (A23,' 2159, TRIRG OR TRAPRG ELEMENT =',I14,
     1       ' POSSESSES ILLEGAL GEOMETRY.')
      NOGO = .TRUE.
      RETURN
      END
