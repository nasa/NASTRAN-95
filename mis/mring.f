      SUBROUTINE MRING (POINTS)
C
C     HEAT CONDUCTIVITY SMA2 ROUITNE FOR TRIANGULAR (POINTS=3) AND
C     TRAPEZOIDAL (POINTS=4) RING ELEMENTS.
C     THIS ROUTINE IS SEPARATE FROM MTRAPR AND MTRIRG SO AS TO BE
C     IN OVERLAY WITH MTRMEM AND MQDMEM.
C
      LOGICAL         NOGO
      INTEGER         POINTS   ,OUTPT    ,SYSBUF   ,TINT     ,MAP(15)
      CHARACTER       UFM*23   ,UWM*25   ,UIM*29   ,SFM*25   ,SWM*27
      COMMON /XMSSG / UFM      ,UWM      ,UIM      ,SFM      ,SWM
      COMMON /SYSTEM/ SYSBUF   ,OUTPT    ,NOGO
      COMMON /SMA2ET/ ECPT(100)
      EQUIVALENCE     (T,TINT)
      DATA    PI23  / 2.0943951024  /
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
C     GEOMETRY CHECKS X  MUST BE .GT.0, AND Y = 0  FOR I = 1,2,..,POINTS
C                      I                     I
C
      I1 = POINTS + 4
      I2 = I1 + 4*POINTS - 1
      DO 20 I = I1,I2,4
      IF (ECPT(I+1)) 140,10,10
   10 IF (ECPT(I+2)) 140,20,140
   20 CONTINUE
C
C     POINT ORDERING CHECK.
C
      IF (POINTS .EQ. 4) GO TO 30
      I1 = 1
      I2 = 3
      GO TO 40
   30 I1 = 4
      I2 = 15
   40 JPOINT = POINTS + 1
      DO 50 I = I1,I2,3
      IR = MAP(I  )*4 + JPOINT
      IS = MAP(I+1)*4 + JPOINT
      IT = MAP(I+2)*4 + JPOINT
      TEMP = (ECPT(IS) - ECPT(IR))*(ECPT(IT+2) - ECPT(IS+2))  -
     1       (ECPT(IT) - ECPT(IS))*(ECPT(IS+2) - ECPT(IR+2))
      IF (TEMP) 140,140,50
   50 CONTINUE
C
C     TRAPEZOID TEST.
C
      IF (POINTS .NE. 4) GO TO 100
      IF (ECPT(11) - ECPT(15)) 70,60,70
   60 IF (ECPT(19) - ECPT(23)) 70,90,70
   70 CALL PAGE2 (-2)
      WRITE  (OUTPT,80) SWM,ECPT(1)
   80 FORMAT (A27,' 3091, A TRAPRG ELEMENT =',I14,' DOES NOT HAVE ',
     1       'SIDE 1-2 PARALLEL TO SIDE 3-4.')
C
C     THICKNESS OF TRMEM OR QDMEM TO BE CALLED BELOW.
C     QDMEM WILL SUBDIVIDE THICKNESS FOR SUB-TRIANGLES AND THUS
C     T IS SET = INTEGER 1 AS A FLAG TO QDMEM ROUTINE WHICH WILL
C     COMPUTE T FOR EACH.
C
C     TEMP. PATH FOR APPROX. THICKNESS
C
   90 T = PI23*(ECPT(9) + ECPT(13) + ECPT(17) + ECPT(21))*3.0/4.0
      GO TO 110
  100 T = PI23*(ECPT(8) + ECPT(12) + ECPT(16))
C
C  CONVERT ECPT TO THAT OF A TRMEM OR QDMEM.
C
  110 J = 5*POINTS + 6
      K = 4*POINTS + 1
      DO 120 I = 1,K
      ECPT(J) = ECPT(J-2)
      J = J - 1
  120 CONTINUE
      ECPT(POINTS+4) = T
      ECPT(POINTS+5) = 0.0
      IF (POINTS .EQ. 4) GO TO 130
C
C     MTRMEM CALL
C
      CALL MASSTQ (4)
      RETURN
C
C     MQDMEM CALL
C
  130 CALL MASSTQ (1)
      RETURN
C
C     BAD GEOMETRY FATAL ERROR.
C
  140 WRITE  (OUTPT,150) UFM,ECPT(1)
  150 FORMAT (A23,' 3092, TRIRG OR TRAPRG ELEMENT =',I14,' POSSESSES ',
     1       'ILLEGAL GEOMETRY.')
      NOGO = .TRUE.
      RETURN
      END
