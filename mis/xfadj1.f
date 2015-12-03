      SUBROUTINE XFADJ1 (BF,SHIFT,SD)
C
C     XFADJ1 ADJUSTS 4 CHARACTER FIELDS, LEFT OR RIGHT, 2 OR 4 FIELDS
C     AT A TIME
C
C     BF    = ADDR OF LEFT MOST FIELD
C     SHIFT = LSHIFT OR RSHIFT
C     SD   = 0 SINGLE (2 FIELDS), 1 DOUBLE (4 FIELDS)
C     RIGHT SHIFTING CAUSES INSERTION OF LEADING ZEROS
C
      IMPLICIT INTEGER (A-Z)
      EXTERNAL        LSHIFT,RSHIFT,ANDF,ORF,SHIFT
      LOGICAL         DEC
      DIMENSION       BF(1),BK(6),MK(6),SFT(3)
      COMMON /MACHIN/ MACH
      COMMON /XSRTCM/ BIMSK1(6),BIMSK2(5),BIMSK3(4),BIMSK4(4),BIMSK5(2),
     1                BIMSK6,BKMSK1(8),BKMSK2,SHIFTS(4),ICON1,ICON2,
     2                STAR,PLUS,DOLLAR,STARL,SLASH,SFTM,MASK,BLANK,MKA,
     3                IS,MBIT4
      EQUIVALENCE     (BK(1) ,BKMSK1(2)),(MK(1),BIMSK1(1)),
     1                (SFT(1),SHIFTS(2)),(BLKS, BKMSK1(8)),
     2                (BKX   ,BKMSK1(1))
C
C     DATA     BK   / 4H0000,4H0000,4H0000,4H000 ,4H00  ,4H0   /
C     DATA     (MK(I),I=1,6) /O777777000000,O777700000000,O770000000000,
C    1                        O000000770000,O000077770000,O007777770000/
C     DATA     (SFT(I),I=1,3)/6,12,18/
C     DATA     BLKS / 4H    /,    BKX/4H0000/
C
C
C     INITIALIZE ROUTINES
C
      DEC = MACH.EQ.5 .OR. MACH.EQ.6 .OR. MACH.EQ.21
      IF (SHIFT(MK(3),SFT(1)) .NE. 0) GO TO 10
C
C     LEFT SHIFT REQUESTED
C
      BLK= BLKS
      I1 = 1
      I2 = 2
      I3 = 3
      I4 = 4
      J  = 3
      GO TO 30
C
C     RIGHT SHIFT REQUESTED
C
   10 BLK= BKX
      J  = 4
      IF (SD .EQ. 0) GO TO 20
C
C     DOUBLE FIELD
C
      I1 = 4
      I2 = 3
      I3 = 2
      I4 = 1
      GO TO 30
C
C     SINGLE FIELD
C
   20 I1 = 2
      I2 = 1
C
C     TOTAL FIELD SHIFTS
C
   30 N = 0
   40 IF (J.EQ.4 .AND. BF(I1).NE.BLKS) GO TO 60
      IF (J.EQ.3 .AND. BF(I1).NE.BLKS .AND. BF(I1).NE.BKX) GO TO 60
      BF(I1) = BF(I2)
      BF(I2) = BLK
      IF (SD .EQ. 0) GO TO 50
      N = N + 1
      BF(I2) = BF(I3)
      BF(I3) = BF(I4)
      BF(I4) = BLK
      IF (N .NE. 3) GO TO 40
   50 IF (BF(I1) .EQ. BLKS) RETURN
C
C     CHARACTER SHIFTS BETWEEN FIELDS
C
   60 N = 0
      IF (J .EQ. 3) GO TO 150
C
C     RIGHT
C
      II = I1
   70 IF (BF(II) .NE. BLKS) GO TO 80
      BF(II) = BKX
      GO TO 110
   80 IF (BF(II) .EQ. BKX) GO TO 110
   90 IF (.NOT.DEC) IHLD = RSHIFT(ANDF(MK(3),BF(II)),1)
      IF (     DEC) IHLD = KHRFN4(RSHIFT(KHRFN4(KHRFN1(BKMSK2,1,
     1                     BF(II),1)),1))
      IF (IHLD .NE. ICON1) GO TO 100
      N = N + 1
      IF (.NOT.DEC) BF(II) = LSHIFT(BF(II),SFT(1))
      IF (     DEC) BF(II) = KHRFN3( BKMSK2,BF(II),1,1)
      IF (N .LT. 3) GO TO 90
      GO TO 120
  100 IF (N .NE. 0) GO TO 120
  110 II = II - 1
      IF (II .EQ. 0) GO TO 130
      GO TO 70
  120 N2 = 4 - N
      IF (.NOT.DEC) BF(II) = ORF(RSHIFT(BF(II),SFT(N)),BK(N2))
      IF (     DEC) BF(II) = KHRFN3( BK(N2),BF(II),N,0)
      N = 0
      GO TO 110
  130 N = 0
C
C     RIGHT
C
  140 IF (DEC) GO TO 141
      IF (ANDF(MK(4),BF(I1)) .NE. ANDF(MK(4),BLKS)) GO TO 170
      GO TO 160
  141 IF (KHRFN1(MK(4),4,BF(I1),4) .NE. KHRFN1(MK(4),4,BLKS,4))
     1   GO TO 170
      GO TO 160
C
C     LEFT
C
  150 IF (.NOT.DEC) IHLD = RSHIFT(ANDF(MK(3),BF(I1)),1)
      IF (     DEC) IHLD = KHRFN4(RSHIFT(KHRFN4(KHRFN1(BKMSK2,1,BF(I1),
     1                     1)),1))
      IF (IHLD.NE.ICON1 .AND. IHLD.NE.ICON2) GO TO 170
  160 N = N + 1
      IF (.NOT.DEC) BF(I1) = SHIFT(BF(I1),SFT(1))
      IF (     DEC) BF(I1) = KHRFN3( BKMSK2,BF(I1),1,4-J)
      IF (N .GE. 3) GO TO 180
      IF (J .EQ. 3) GO TO 150
      GO TO 140
  170 IF (N .EQ. 0) RETURN
  180 IF (J .EQ. 4) GO TO 190
C
C     LEFT SHIFTS
C
      N1 = N
      N2 = N + 3
      GO TO 200
C
C     RIGHT SHIFTS
C
  190 N1 = 7 - N
      N2 = 4 - N
  200 N3 = 4 - N
      IF (.NOT.DEC) BF(I1) = ORF(ANDF(MK(N1),BF(I1)),ANDF(MK(N2),
     1                       ISFT(BF(I2),SFT(N3),J)))
      IF (     DEC) BF(I1) = KHRFN3(BF(I1),BF(I2),N3,J-3)
      BF (I1) = ORF(BF(I1),BKMSK2)
      IF (.NOT.DEC) BF(I2) = ORF(ANDF(MK(N1),SHIFT(BF(I2),SFT(N))),
     1                       BK(N2))
      IF (     DEC) BF(I2) = KHRFN3( BK(N2),BF(I2),N,4-J)
      IF (SD .EQ. 0) RETURN
C
      IF (.NOT.DEC) BF(I2) = ORF(ANDF(MK(N1),BF(I2)),ANDF(MK(N2),
     1                       ISFT(BF(I3),SFT(N3),J)))
      IF (     DEC) BF(I2) = KHRFN3( BF(I2),BF(I3),N3,J-3 )
      BF(I2) = ORF(BF(I2),BKMSK2)
      IF (BF(I2) .EQ. BLK) RETURN
C
      IF (.NOT.DEC) BF(I3) = ORF(ANDF(MK(N1),SHIFT(BF(I3),SFT(N))),
     1                       BK(N2))
      IF (     DEC) BF(I3) = KHRFN3(BK(N2),BF(I3),N,4-J)
      IF (.NOT.DEC) BF(I3) = ORF(ANDF(MK(N1),BF(I3)),ANDF(MK(N2),
     1                       ISFT(BF(I4),SFT(N3),J)))
      IF (     DEC) BF(I3) = KHRFN3(BF(I3),BF(I4),N3,J-3)
      BF(I3) = ORF(BF(I3),BKMSK2)
      IF (BF(I3) .EQ. BLK) RETURN
C
      IF (.NOT.DEC) BF(I4) = ORF(ANDF(MK(N1),SHIFT(BF(I4),SFT(N))),
     1                       BK(N2))
      IF (     DEC) BF(I4) = KHRFN3(BK(N2),BF(I4),N,4-J)
      RETURN
      END
