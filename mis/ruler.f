      SUBROUTINE RULER (RULE,ICP,ZRCT,ONCT,LIST,N,BUFF,IOPT)
C
C     DETERMINES STRING OF ZEROS AND ONES IN LIST BY APPLYING RULE TO
C     CP.
C
      EXTERNAL        ORF
      INTEGER         ZRCT,ONCT,OCT,RULE,EOL,ORF,ZCT
      DIMENSION       LIST(1),BUFF(1),ICP(1)
      COMMON /TWO   / TWO1(32)
      COMMON /ZNTPKX/ A1(4),L,EOL
C
C     PICK UP PARAMETERS
C
      EOL = 0
      R   = RULE
      NAMCP = ICP(1)
      ZCT = 0
      OCT = 0
      ASSIGN 150 TO IS
      IF (R .GE. 0.0) ASSIGN 140 TO IS
      R   = ABS(R)
      L   = 0
      J1  = 0
      M   = 0
      N1  = N
      IF (NAMCP .EQ. 0) GO TO 50
      CALL GOPEN (NAMCP,BUFF,0)
      CALL INTPK (*50,NAMCP,0,1,0)
      GO TO 60
   50 M   = N1
      EOL = 1
   60 DO 200 I = 1,N1
      J = (I+31)/32
      IF (M   .GE. I) GO TO 90
      IF (EOL .EQ. 0) GO TO 80
      L = N1
      A1(1) = 0.0
      GO TO 90
   80 CALL ZNTPKI
   90 IF (L .EQ. I) GO TO 110
      M = L
      A = 0.0
      GO TO 120
  110 A = A1(1)
  120 IF (IOPT.EQ.1 .OR. J.LE.J1) GO TO 130
      J1 = J
      LIST(J) = 0
  130 GO TO IS, (140,150)
  140 IF (A-R) 160,190,160
  150 IF (A-R) 160,190,200
  160 OCT = OCT + 1
      IF (IOPT .EQ. 1) GO TO 180
      K = I - ((I-1)/32)*32
      LIST(J) = ORF(LIST(J),TWO1(K))
      GO TO 200
  180 LIST(I) = OCT
      GO TO 200
  190 ZCT = ZCT + 1
      IF (IOPT .NE. 0) LIST(I) = -ZCT
  200 CONTINUE
      ZRCT = ZCT
      ONCT = OCT
      IF (NAMCP .NE. 0) CALL CLOSE (NAMCP,1)
      RETURN
      END
