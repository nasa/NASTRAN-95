      SUBROUTINE PHDMIA
C
C     PUNCH SINGLE- OR DOUBLE-FIELD DMI CARDS FOR REAL, SINGLE-
C     PRECISION MATRICES.
C
C  $MIXED_FORMAT
C
      LOGICAL          FIRST
      INTEGER          FMT(8),IQX(8),KFMT(22),RET,NAME(2),RET1,LFMT(23),
     1                 H1,H2,H3,C1,C2,C3,H1A,C1A,ERNO
      REAL             QX(8)
      COMMON /PHDMIX/  NAME,NAM,IFO,ITIN,ITOUT,IR,IC,NOUTPT,KPP,NLPP,
     1                 ERNO,ICOL,IRO,X,ICARD1
      COMMON /SYSTEM/  DUM90(90),NP
      COMMON /MAHCIN/  MACH
      EQUIVALENCE      (QX(1),IQX(1)), (LFMT(2),KFMT(1))
      DATA    DMI   ,  IZ, P  , DMIS  , S       /
     1        3HDMI ,  0 , 1H+, 4HDMI*, 1H*     /
      DATA    KFMT  /  3*4H$$$$,16*4H**** , 4HA1,A,4H2,I5,4H)   /
      DATA    KFMTI ,  KFMTR1,KFMTR2/4HI8 ,,4HF8.1,4H,          /
      DATA    KDMTI ,  KDMTR1,KDMTR2/4HI16,,4H1PE1,4H6.8,       /
      DATA    KFMTB ,  KFMT8 ,KDMTR0/4H    ,4H4X, ,4H  E1       /
      DATA    KDMTB ,  KDMT8 /4H    ,4H8X,      /
      DATA    H1    ,  H2    ,H3    ,C1    ,C2    ,C3    ,H1A   ,C1A   /
     1        4H(A4,,  4H4X, ,4H2A4,,4H(A1,,4HA2, ,4HI5, ,4H A4,,4H A1,/
      DATA   LFMT(1)/  4H(1X,/
C
C     IBM/AIX (MACH=8) DOES NOT LIKE THE NON-ANSI STANDARD FORMAT
C     1PE16.8 (THE STANDARD IS 1P,E16.8).
C
      IF (MACH .EQ. 8) KDMTR1 = KDMTR0
C
C     CALLED INITIALLY FOR EACH MATRIX.
C
C     SET PUNCH UNIT TO 7 FOR IBM AND CDC AND TO 1 FOR UNIVAC
C
      ERNO  = 0
      NOUT  = NOUTPT
      KP    = KPP
      NKP   = 8/KP
      ICARD =-1
      ICARD1= 0
      GO TO (10,20), KP
   10 DMIPS = DMI
      PS = P
      GO TO 30
   20 DMIPS = DMIS
      PS = S
      DO 25 I = 12,19
   25 KFMT(I) = KDMTB
   30 WRITE  (NP,1) DMI,NAME,IZ,IFO,ITIN,ITOUT,IR,IC,P,NAM,ICARD1
    1 FORMAT (A3,5X,2A4,4I8,8X,2I8,A1,A2,I5)
      IF (NOUT .LE. 0) GO TO 40
      WRITE  (NOUT,2) DMI,NAME,IZ,IFO,ITIN,ITOUT,IR,IC,P,NAM,ICARD1
    2 FORMAT (1H1,/1X,A3,5X,2A4,4I8,8X,2I8,A1,A2,I5)
      L = 1
   40 RETURN
C
C
      ENTRY PHDMIB
C     ============
C
C     CALLED FOR FIRST NON-ZERO ELEMENT OF EACH COLUMN.
C
      IQ   = 0
      IROW = IRO
      FIRST=.TRUE.
      IQ   = IQ + 1
      FMT(IQ) = 0
      IQX(IQ) = 0
      IQ   = IQ + 1
      FMT(IQ) = 1
      IQX(IQ) = ICOL
      IQ   = IQ + 1
      FMT(IQ) = 1
      IQX(IQ) = IROW
      IQ   = IQ + 1
      FMT(IQ) = 2
      QX(IQ)  = X
      RETURN
C
C
      ENTRY PHDMIC
C     ============
C
C     CALLED FOR EACH NON-ZERO ELEMENT OF COLUMN EXCEPT FIRST ONE.
C
C     LOOK FOR FULL CARD
C
      IF (IQ .LT. NKP) GO TO 100
      ASSIGN 100 TO RET
      GO TO 700
C
C     DETERMINE IF NEW ENTRY IS CONSECUTIVE OR NON-CONSECUTIVE.
C
  100 IF (IRO .NE. IROW+1) GO TO 200
      IROW = IRO
      IQ   = IQ + 1
      FMT(IQ) = 2
      QX(IQ)  = X
      RETURN
C
  200 IQ   = IQ + 1
      IROW = IRO
      FMT(IQ) = 1
      IQX(IQ) = IRO
      IF (IQ .LT. NKP) GO TO 300
      ASSIGN 300 TO RET
      GO TO 700
  300 IQ = IQ + 1
      FMT(IQ) = 2
      QX(IQ)  = X
      RETURN
C
C
      ENTRY PHDMID
C     ============
C
C     ENTRY POINT FOR COLUMN TERMINATION CALL
C
      IF (IQ .LE. 0) RETURN
      ASSIGN 500 TO RET
      GO TO 700
  500 RETURN
C
C     PUNCH CARD
C
  700 N = IQ
      ASSIGN 800 TO RET1
      GO TO 1000
  800 IF (FIRST) GO TO 900
      WRITE (NP,KFMT,ERR=810) PS,NAM,ICARD,(QX(L),L=1,IQ),PS,NAM,ICARD1
  810 LFMT(2) = C1A
      IF (NOUT .LE. 0) GO TO 850
      IF (L .LT. NLPP) GO TO 830
      WRITE  (NOUT,3)
    3 FORMAT (1H1)
      L = 0
  830 WRITE (NOUT,LFMT,ERR=840) PS,NAM,ICARD,(QX(L),L=1,IQ),
     1                          PS,NAM,ICARD1
  840 L  = L + 1
  850 IQ = 0
      GO TO 950
  900 WRITE (NP,KFMT,ERR=910) DMIPS,NAME,(QX(L),L=2,IQ),PS,NAM,ICARD1
  910 LFMT(2) = H1A
      IF (NOUT .LE. 0) GO TO 940
      IF (L .LT. NLPP) GO TO 920
      WRITE (NOUT,3)
      L  = 0
  920 WRITE (NOUT,LFMT,ERR=930) DMIPS,NAME,(QX(L),L=2,IQ),PS,NAM,ICARD1
  930 L  = L + 1
  940 FIRST = .FALSE.
      IQ = 0
  950 GO TO RET, (100,300,500)
C
C     BUILD FORMAT FOR CARD IMAGE.
C
 1000 ICARD  = ICARD + 1
      ICARD1 = ICARD + 1
      IF (ICARD1 .GT. 99999) GO TO 9901
      GO TO (1001,1101), KP
 1001 IF (FIRST) GO TO 1005
      I1 = 1
      KFMT(1) = C1
      KFMT(2) = C2
      KFMT(3) = C3
      GO TO 1009
 1005 I1 = 2
      KFMT(1) = H1
      KFMT(2) = H2
      KFMT(3) = H3
      KFMT(4) = KFMTB
      KFMT(5) = KFMTB
 1009 DO 1030 I = I1,N
      K  = FMT(I)
      IF (K .EQ. 2) GO TO 1020
 1010 KFMT(2*I+2) = KFMTI
      KFMT(2*I+3) = KFMTB
      GO TO 1030
 1020 KFMT(2*I+2) = KFMTR1
      KFMT(2*I+3) = KFMTR2
 1030 CONTINUE
      IF (N .GE. NKP) GO TO 1999
      N1 = N + 1
      DO 1040 I = N1,NKP
      KFMT(2*I+2) = KFMT8
      KFMT(2*I+3) = KFMT8
 1040 CONTINUE
      GO TO 1999
 1101 IF (FIRST) GO TO 1105
      I1 = 1
      KFMT(1) = C1
      KFMT(2) = C2
      KFMT(3) = C3
      GO TO 1109
 1105 I1 = 2
      KFMT(1) = H1
      KFMT(2) = H2
      KFMT(3) = H3
      KFMT(4) = KDMT8
      KFMT(5) = KDMTB
 1109 DO 1130 I = I1,N
      K  = FMT(I)
      IF (K .EQ. 2) GO TO 1120
 1110 KFMT(2*I+2) = KDMTI
      KFMT(2*I+3) = KDMTB
      GO TO 1130
 1120 KFMT(2*I+2) = KDMTR1
      KFMT(2*I+3) = KDMTR2
 1130 CONTINUE
      IF (N .GE. NKP) GO TO 1999
      N1  = N + 1
      DO 1140 I = N1,NKP
      KFMT(2*I+2) = KDMT8
      KFMT(2*I+3) = KDMT8
 1140 CONTINUE
      GO TO 1999
 1999 GO TO RET1, (800)
C
C
C     ERROR MESSAGES
C
 9901 ERNO = 1
      GO TO 9999
C
C
 9999 RETURN
C
      END
