      SUBROUTINE CMIWRT (ICODE,NAME1,NAME2,LOC,NW,A,IZ)
C
C     THIS SUBROUTINE WRITES FORMATTED SOF ITEMS.
C     ICODE = 1 FOR EQSS    ICODE = 2 FOR BGSS    ICODE = 3 FOR CSTM
C     ICODE = 4 FOR PLTS    ICODE = 5 FOR LODS    ICODE = 7 FOR LOAP
C     NAME1 IS PSEUDOSTRUCTURE NAME, NAME2 IS COMPONENT NAME
C
      EXTERNAL        ANDF
      INTEGER         OUTT,ANDF
      DIMENSION       NAME1(2),NAME2(2),A(1),IZ(1),IBITS(32),IPL(6),
     1                IH1(96),IH2(96),IH3(96),IH4(96),IH5(96),IH6(96)
      COMMON /SYSTEM/ XXX,OUTT,JUNK1(6),NLPP,JUNK2(2),NLINE
      COMMON /OUTPUT/ ITITL(96),IHEAD(96)
      DATA IH1 / 9*4H    ,4H EQS,4HS IT,4HEM F,4HOR S,4HUBST,4HRUCT,
     1           4HURE ,2*4H    ,4H COM,4HPONE,4HNT  ,11*4H    ,4HGRID,
     2           4H POI,4HNT  ,4H INT,4HERNA,4HL   ,4H  CO,4HMPON,
     3           4HENT ,2*4H    ,4H GRI,4HD PO,4HINT ,4H  IN,4HTERN,
     4           4HAL  ,4H   C,4HOMPO,4HNENT,2*4H    ,4H  GR,4HID P,
     5           4HOINT,4H   I,4HNTER,4HNAL ,4H    ,4HCOMP,4HONEN,
     6           4HT   ,4H    ,4HID  ,4H    ,4H POI,4HNT I,4HD   ,
     7           4H    ,4H DOF,4*4H    ,4H ID ,4H    ,4H  PO,4HINT ,
     8           4HID  ,4H    ,4H  DO,4HF   ,3*4H    ,4H  ID,4H    ,
     9           4H   P,4HOINT,4H ID ,4H    ,4H   D,4HOF  ,4H     /
      DATA IH2 / 11*4H    ,4HBGSS,4H ITE,4HM FO,4HR SU,4HBSTR,4HUCTU,
     1           4HRE  ,21*4H    ,4HINTE,4HRNAL,4H    ,4H CST,4HM ID,
     2           4*4H    ,4H  C ,4HO O ,4HR D ,4HI N ,4HA T ,4HE S ,
     3           17*4H    ,4HPOIN,4HT ID,4H    ,4H   N,4HO.  ,3*4H    ,
     4           4HX1  ,3*4H    ,4HX2  ,3*4H    ,4HX3  ,8*4H     /
      DATA IH3 / 12*4H    ,4HCSTM,4H ITE,4HM FO,4HR SU,4HBSTR,
     1           4HUCTU,4HRE  ,13*4H    ,2*4H    ,4H CST,4HM   ,4HTYPE,
     2           2*4H    ,4HC O ,4HO R ,4HD I ,4HN A ,4HT E ,4HS   ,
     3           4HO F ,4H  O ,4HR I ,4HG I ,4HN   ,3*4H    ,4H   T,
     4           4H R A,4H N S,4H F O,4H R M,4H A T,4H I O,4H N  ,
     5           5*4H    ,4H  ID,5*4H    ,4HX1  ,3*4H    ,4HX2  ,
     6           3*4H    ,4HX3  ,6*4H    ,4H   M,4H A T,4H R I,4H X  ,
     7           5*4H      /
      DATA IH4 / 12*4H    ,4HPLTS,4H ITE,4HM FO,4HR SU,4HBSTR,
     1           4HUCTU,4HRE  ,13*4H    ,2*4H    ,4HCOMP,4HONEN,4HT   ,
     2           4H    ,4H C O,4H O R,4H D I,4H N A,4H T E,4H S  ,
     3           4H O F,4H   O,4HR I ,4HG I ,4HN   ,3*4H    ,4H   T,
     4           4H R A,4H N S,4H F O,4H R M,4H A T,4H I O,4H N  ,
     5           6*4H    ,4H  NA,4HME  ,3*4H    ,4H X1 ,3*4H    ,
     6           4H X2 ,3*4H    ,4H X3 ,6*4H    ,4H   M,4H A T,4H R I,
     7           4H X  ,6*4H      /
      DATA IH5 / 12*4H    ,4HLODS,4H ITE,4HM FO,4HR SU,4HBSTR,
     1           4HUCTU,4HRE  ,18*4H    ,4H COM,4HPONE,4HNT  ,4H  NU,
     2           4HMBER,4H OF ,21*4H    ,5*4H    ,4H   N,4HAME ,
     3           4H    ,4H  LO,4HAD S,4HETS ,4H  L ,4HO A ,4HD   ,
     4           4HS E ,4HT   ,4HI D ,4HE N ,4HT I ,4HF I ,4HC A ,
     5           4HT I ,4HO N ,4H  N ,4HU M ,4HB E ,4HR S ,5*4H      /
      DATA IH6 / 9*4H         ,4HEQSS,4H ITE,4HM - ,4HSCAL,4HAR I,
     1           4HNDEX,4H LIS,4HT FO,4HR SU,4HBSTR,4HUCTU,4HRE  ,
     2           11*4H        ,4H INT,4HERNA,4HL   ,4H INT,4HERNA,
     3           4HL   ,4H  CO,4HMPON,4HENT ,2*4H         ,4H  IN,
     4           4HTERN,4HAL  ,4H  IN,4HTERN,4HAL  ,4H   C,4HOMPO,
     5           4HNENT,2*4H         ,4H   I,4HNTER,4HNAL ,4H   I,
     6           4HNTER,4HNAL ,4H    ,4HCOMP,4HONEN,4HT   ,4H POI,
     7           4HNT I,4HD   ,4H  SI,4HL ID,2*4H         ,4H DOF,
     8           3*4H         ,4H  PO,4HINT ,4HID  ,4H   S,4HIL I,
     9           4HD   ,4H    ,4H  DO,4HF   ,2*4H         ,4H   P,
     A           4HOINT,4H ID ,4H    ,4HSIL ,4HID  ,4H    ,4H   D,
     B           4HOF  ,4H     /
      DATA LOAP/ 4HLOAP/
C
      IST  = LOC
      IFIN = LOC + NW - 1
      GO TO (1,2,3,4,5,6,5,8), ICODE
C
C     EQSS ITEM
C
    1 DO 100 I = 1,96
  100 IHEAD(I) = IH1(I)
C
C     INSERT NAMES INTO HEADING
C
      IHEAD(17) = NAME1(1)
      IHEAD(18) = NAME1(2)
      IHEAD(22) = NAME2(1)
      IHEAD(23) = NAME2(2)
      CALL PAGE
      IF (NW .NE. 0) GO TO 140
      WRITE (OUTT,1009)
      GO TO 700
C
  140 DO 101 I = IST,IFIN,9
      NLINE = NLINE + 1
      IF (NLINE .LE. NLPP) GO TO 150
      CALL PAGE
      NLINE = NLINE + 1
  150 CONTINUE
      ICOMP = ANDF(IZ(I+2),63)
      CALL BITPAT (ICOMP,IBITS(1))
      I2 = 3
      IF (I+5 .GT. IFIN) GO TO 151
      ICOMP = ANDF(IZ(I+5),63)
      CALL BITPAT (ICOMP,IBITS(4))
      I2 = 6
      IF (I+8 .GT. IFIN) GO TO 151
      ICOMP = ANDF(IZ(I+8),63)
      CALL BITPAT (ICOMP,IBITS(7))
      I2 = 9
  151 CONTINUE
      WRITE (OUTT,1000) (IZ(I+J-1),IZ(I+J),IBITS(J),IBITS(J+1),J=1,I2,3)
  101 CONTINUE
      GO TO 700
C
C     EQSS - SCALER INDEX LIST
C
    8 DO 600 I = 1,96
  600 IHEAD(I) = IH6(I)
      IHEAD(22) = NAME1(1)
      IHEAD(23) = NAME1(2)
      CALL PAGE
C
      IP = 0
      DO 603 I = IST,IFIN,6
      NLINE = NLINE + 1
      IF (NLINE .LE. NLPP) GO TO 601
      CALL PAGE
      NLINE = NLINE + 1
  601 CONTINUE
      KCODE = IZ(I+1)
      CALL BITPAT (KCODE,IBITS(1))
      I2 = 2
      IPL(1) = IP + 1
      IF (I+3 .GT. IFIN) GO TO 602
      KCODE = IZ(I+3)
      CALL BITPAT (KCODE,IBITS(3))
      I2 = 4
      IPL(3) = IP + 2
      IF (I+5 .GT. IFIN) GO TO 602
      KCODE = IZ(I+5)
      CALL BITPAT (KCODE,IBITS(5))
      I2 = 6
      IPL(5) = IP + 3
  602 CONTINUE
      WRITE (OUTT,1000) (IPL(J),IZ(I+J-1),IBITS(J),IBITS(J+1),J=1,I2,2)
      IP = IP + 3
  603 CONTINUE
      GO TO 700
C
C     BGSS ITEM
C
    2 DO 200 I = 1,96
  200 IHEAD(I)  = IH2(I)
      IHEAD(20) = NAME1(1)
      IHEAD(21) = NAME1(2)
      CALL PAGE
      J = 0
      DO 201 I = IST,IFIN,4
      J = J + 1
      NLINE = NLINE + 1
      IF (NLINE .LE. NLPP) GO TO 250
      CALL PAGE
      NLINE = NLINE + 1
  250 CONTINUE
      WRITE (OUTT,1001) J,IZ(I),A(I+1),A(I+2),A(I+3)
  201 CONTINUE
      GO TO 700
C
C     CSTM ITEM
C
    3 DO 300 I = 1,96
  300 IHEAD(I)  = IH3(I)
      IHEAD(20) = NAME1(1)
      IHEAD(21) = NAME1(2)
      CALL PAGE
      DO 301 I = IST,IFIN,14
      NLINE = NLINE + 4
      IF (NLINE .LE. NLPP) GO TO 350
      CALL PAGE
      NLINE = NLINE + 4
  350 CONTINUE
      I1 = I + 2
      I2 = I + 13
      WRITE (OUTT,1002) IZ(I),IZ(I+1),(A(KK),KK= I1,I2)
  301 CONTINUE
      GO TO 700
C
    4 DO 400 I = 1,96
C
C     PLTS ITEM
C
  400 IHEAD(I)  = IH4(I)
      IHEAD(20) = NAME1(1)
      IHEAD(21) = NAME1(2)
      CALL PAGE
      DO 401 I = IST,IFIN,14
      NLINE = NLINE + 4
      IF (NLINE .LE. NLPP) GO TO 450
      CALL PAGE
      NLINE = NLINE + 4
  450 CONTINUE
      I1 = I + 2
      I2 = I + 13
      WRITE (OUTT,1004) IZ(I),IZ(I+1),(A(J),J=I1,I2)
  401 CONTINUE
      GO TO 700
C
C     LODS AND LOAP ITEMS
C
    5 DO 500 I = 1,96
  500 IHEAD(I)  = IH5(I)
      IHEAD(20) = NAME1(1)
      IHEAD(21) = NAME1(2)
      IF (ICODE .EQ. 7) IHEAD(13) = LOAP
      CALL PAGE
    6 IF (NW.EQ.0 .OR. NW.EQ.1) GO TO 520
      NL = NW/5 + 3
      NLINE = NLINE + NL
      IF (NLINE .LE. NLPP) GO TO 550
      CALL PAGE
      NLINE = NLINE + NL
  550 CONTINUE
      IST1 = IST + 1
      WRITE (OUTT,1006) NAME2(1),NAME2(2),IZ(IST),(IZ(J),J=IST1,IFIN)
      GO TO 700
C
  520 NLINE = NLINE + 2
      IF (NLINE .LE. NLPP) GO TO 560
      CALL PAGE
      NLINE = NLINE + 2
  560 CONTINUE
      WRITE (OUTT,1008) NAME2(1),NAME2(2)
  700 RETURN
C
 1000 FORMAT (6X,I8,4X,I8,6X,A4,A2,2(13X,I8,4X,I8,6X,A4,A2))
 1001 FORMAT (33X,I8,4X,I8,3X,3(3X,E13.6))
 1002 FORMAT (/10X,I8,3X,I4,3X,3(3X,E13.6),4X,3(3X,E13.6),
     1        /80X,3(3X,E13.6), /80X,3(3X,E13.6))
 1004 FORMAT (/14X,2A4,3X,3(3X,E13.6),4X,3(3X,E13.6)
     1        /77X,3(3X,E13.6), /77X,3(3X,E13.6))
 1006 FORMAT (/26X,2A4,3X,I8,5X,6(2X,I8)/(50X,2X,I8,2X,I8,2X,I8,2X,I8,
     1        2X,I8,2X,I8,/))
 1008 FORMAT (/26X,2A4,17X,32HNO LOAD SETS FOR THIS COMPONENT. )
 1009 FORMAT (/30X,64HALL DEGREES OF FREEDOM FOR THIS COMPONENT HAVE BEE
     1N REDUCED OUT. )
      END
