      SUBROUTINE QPARMD
C
C     MODULE PARAMD PERFORMS THE FOLLOW OP ON PARAMETERS IN DOUBLE
C     PRECISION
C     (REFERENCE - MODULE PARAMR AND SUBROUTINE QPARMR)
C
C     DMAP
C     PARAMD  / /C,N,OP/ V,N,OUTD/V,N,IND1/V,N,IND2/
C                        V,N,OUTC/V,N,INC1/V,N,INC2/V,N,FLAG $
C
C         OP        COMPUTE
C         --        -------------------------------------------
C      BY DEFAULT   FLAG = 0
C      1  ADD       OUTD = IND1 + IND2
C      2  SUB       OUTD = IND1 - IND2
C      3  MPY       OUTD = IND1 * IND2
C      4  DIV       OUTD = IND1 / IND2 (IF IND2 = 0, FLAG IS SET TO +1)
C      5  NOP       RETURN
C      6  SQRT      OUTD = DSQRT(IND1)
C      7  SIN       OUTD = DSIN(IND1) WHERE IND1 IS IN RADIANS
C      8  COS       OUTD = DCOS(IND1) WHERE IND1 IS IN RADIANS
C      9  ABS       OUTD = DABS(IND1)
C     10  EXP       OUTD = DEXP(IND1)
C     11  TAN       OUTD = DTAN(IND1) WHERE IND1 IS IN RADIANS
C     12  ADDC      OUTC = INC1 + INC2
C     13  SUBC      OUTC = INC1 - INC2
C     14  MPYC      OUTC = INC1 * INC2
C     15  DIVC      OUTC = INC1 / INC2 (IF INC2 = 0, FLAG IS SET TO +1)
C     16  COMPLEX   OUTC = (IND1,IND2)
C     17  CSQRT     OUTC = DCSQRT(INC1)
C     18  NORM      OUTD = DSQRT(OUTC(1)**2 + OUTC(2)**2)
C     19  REAL      IND1 = OUTC(1),  IND2 = OUTC(2)
C     20  POWER     OUTD = IND1**IND2
C     21  CONJ      OUTC = DCONJG(INC1)
C     22  EQ        FLAG =-1 IF IND1 COMPARES WITH IND2
C     23  GT        FLAG =-1 IF IND1 IS GT IND2
C     24  GE        FLAG =-1 IF IND1 IS GE IND2
C     25  LT        FLAG =-1 IF IND1 IS LT IND2
C     26  LE        FLAG =-1 IF IND1 IS LE IND2
C     27  NE        FLAG =-1 IF IND1 IS NE IND2
C     28  LOG       OUTD = DLOG10(IND1)
C     29  LN        OUTD = DLOG(IND1)
C     30  FIX       FLAG = OUTD
C     31  FLOAT     OUTD = FLOAT(FLAG)
C
C     NEW OP CODE ADDED IN THIS NEW VERSION, 12/1988 -
C
C     32  ERR       IF FLAG IS 0, SYSTEM NOGO FLAG IS SET TO ZERO
C                   IF FLAG IS NON-ZERO, JOB TERMINATED IF ANY PREVIOUS
C                      PARAMD (OR PARAMR) CONTAINS NON-FATAL ERROR(S)
C
      LOGICAL          PRT
      INTEGER          OP,OPCODE(50),FLAG,IVPS(1),NAME(2),IL(8),ILX(8),
     1                 NAM(2),BLNK
      REAL             TEMP(2)
      DOUBLE PRECISION OUTD,IND1,IND2,OUTC,INC1,INC2,DENOM,TEMPD
      CHARACTER        UFM*23,UWM*25,UIM*29
      COMMON /XMSSG /  UFM,UWM,UIM
      COMMON /BLANK /  OP(2),OUTD,IND1,IND2,OUTC(2),INC1(2),INC2(2),FLAG
      COMMON /XVPS  /  VPS(1)
      COMMON /ILXXD /  IL1,IL2,IL3,IL4,IL5,IL6,IL7,IL8
      COMMON /SYSTEM/  IBUF,NOUT,NOGO,DUMMY(33),KSYS37
      EQUIVALENCE      (VPS(1),IVPS(1)), (TEMPD,TEMP(1)), (IL,IL1)
      DATA NAME     /  4HQPAR,4HMD  /      ,IFIRST / 15  /
      DATA OPCODE   /  4HADD ,4HSUB ,4HMPY ,4HDIV ,4HNOP ,
     1                 4HSQRT,4HSIN ,4HCOS ,4HABS ,4HEXP ,
     2                 4HTAN ,4HADDC,4HSUBC,4HMPYC,4HDIVC,
     3                 4HCOMP,4HCSQR,4HNORM,4HREAL,4HPOWE,
     4                 4HCONJ,4HEQ  ,4HGT  ,4HGE  ,4HLT  ,
     5                 4HLE  ,4HNE  ,4HLOG ,4HLN  ,4HFIX ,
     6                 4HFLOA,4HERR ,4H    ,4H    ,4H    ,
     7                 4H    ,4H    ,4H    ,4H    ,4H    ,
     8                 4H    ,4H    ,4H    ,4H    ,4H    ,
     9                 4H    ,4H    ,4H    ,4H    ,4H    /
      DATA ILX      /  4H1ST ,4H2ND ,4H3RD ,4H4TH ,4H5TH ,
     1                 4H6TH ,4H7TH ,4H8TH               /
      DATA PARM,NAM /  4HPARM,4H/PAR,3HAMD/,BLNK  /4H    /
C
C     SUPPRESS ALL PARAMETER CHECK MESSAGES IF DIAG 37 IS ON
C
      CALL SSWTCH (37,I)
      PRT = I .EQ. 0
      IF (PRT) NAM(1) = BLNK
      IF (PRT) NAM(2) = BLNK
C
C     COMPUTE VPS INDEXES AND PARAMETER NAMES
C
      DO 2 I = 2,8
      CALL FNDPAR (-I,IL(I))
    2 CONTINUE
      IF (.NOT.PRT) GO TO 4
      CALL PAGE2 (IFIRST)
      IFIRST = 6
      WRITE  (NOUT,3) UIM,OP
    3 FORMAT (A29,' FROM PARAMD MODULE - OP CODE = ',2A4, /5X,
     1        '(ALL PARAMD MESSAGES CAN BE SUPPRESSED BY DIAG 37)')
C
C     BRANCH ON OPERATION CODE
C
    4 IFLAG = FLAG
      FLAG  = 0
      IERR  = 0
C
      DO 5 IOP = 1,32
      IF (OP(1) .EQ. OPCODE(IOP)) GO TO
     1   (  10,  20,  30,  40,  50,  60,  70,  80,  90, 100,
     2     110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
     3     210, 220, 230, 240, 250, 260, 270, 280, 290, 300,
     4     310, 320    ), IOP
    5 CONTINUE
      WRITE  (NOUT,6) OP(1),NAM
    6 FORMAT (22X,'UNRECOGNIZABLE OP CODE = ',A4,' (INPUT ERROR)  ',2A4)
      CALL MESAGE (-7,0,NAME)
C
C *******
C     D.P. REAL NUMBER FUNCTIONS
C *******
C
C     ADD
C
   10 OUTD = IND1 + IND2
      GO TO 600
C
C     SUBTRACT
C
   20 OUTD = IND1 - IND2
      GO TO 600
C
C     MULTIPLY
C
   30 OUTD = IND1*IND2
      GO TO 600
C
C     DIVIDE
C
   40 OUTD = 0.D+0
      IF (IND2 .EQ. 0.D+0) GO TO 45
      OUTD = IND1/IND2
      GO TO 600
   45 WRITE  (NOUT,47) NAM
   47 FORMAT (5X,'ERROR - DIVIDED BY ZERO  ',2A4)
      IERR = 1
      FLAG =+1
      IF (IL8 .LE. 0) GO TO 730
      IVPS(IL8) = FLAG
      I = IL8 - 3
      IF (PRT) WRITE (NOUT,48) IVPS(I),IVPS(I+1),FLAG
   48 FORMAT (22X,2A4,2H =,I10,'   (OUTPUT)')
      GO TO 730
C
C     NOP
C
   50 RETURN
C
C     SQUARE ROOT
C
   60 IF (IND1 .LT. 0.D+0) GO TO 65
      OUTD = DSQRT(IND1)
      GO TO 650
   65 WRITE  (NOUT,67) NAM
   67 FORMAT (5X,'ERROR - OPERATING ON A NEGATIVE NUMBER  ',2A4)
      OUTD = 0.D+0
      IERR = 1
      GO TO 650
C
C     SINE
C
   70 OUTD = DSIN(IND1)
      GO TO 650
C
C     COSINE
C
   80 OUTD = DCOS(IND1)
      GO TO 650
C
C     ABSOLUTE VALUE
C
   90 OUTD = DABS(IND1)
      GO TO 650
C
C     EXPONENTIAL
C
  100 OUTD = DEXP(IND1)
      GO TO 650
C
C     TANGENT
C
  110 OUTD = DTAN(IND1)
      GO TO 650
C
C     NORM
C
  180 OUTD = DSQRT(OUTC(1)**2 + OUTC(2)**2)
      GO TO 690
C
C     POWER
C
  200 OUTD  = IND1**IND2
      GO TO 600
C
C     LOG
C
  280 IF (IND1 .LT. 0.D+0) GO TO 65
      OUTD = DLOG10(IND1)
      GO TO 650
C
C     NATURAL LOG
C
  290 IF (IND1 .LT. 0.D+0) GO TO 65
      OUTD = DLOG(IND1)
      GO TO 650
C
C     FLOAT
C
  310 OUTD = IFLAG
      GO TO 670
C
C     ERR
C
  320 IF (IFLAG.NE.0 .AND. KSYS37.NE.0) GO TO 970
      KSYS37 = 0
      NOGO   = 0
      IF (PRT) WRITE (NOUT,325)
  325 FORMAT (5X,'SYSTEM NOGO FLAG IS RESET TO INTEGER ZERO',/)
      GO TO 990
C
C *******
C     COMPLEX FUNCTIONS
C *******
C
C     ADD COMPLEX
C
  120 OUTC(1) = INC1(1) + INC2(1)
      OUTC(2) = INC1(2) + INC2(2)
      GO TO 730
C
C     SUBTRACT COMPLEX
C
  130 OUTC(1) = INC1(1) - INC2(1)
      OUTC(2) = INC1(2) - INC2(2)
      GO TO 730
C
C     MULTIPLY COMPLEX
C
  140 OUTC(1) = INC1(1)*INC2(1) - INC1(2)*INC2(2)
      OUTC(2) = INC1(1)*INC2(2) + INC1(2)*INC2(1)
      GO TO 730
C
C     DIVIDE COMPLEX
C
  150 DENOM = INC2(1)**2 + INC2(2)**2
      IF (DENOM .EQ. 0.D+0) GO TO 155
      OUTC(1) = (INC1(1)*INC2(1) + INC1(2)*INC2(2))/DENOM
      OUTC(2) = (INC1(2)*INC2(1) - INC1(1)*INC2(2))/DENOM
      GO TO 730
  155 OUTC(1) = 0.D+0
      OUTC(2) = 0.D+0
      GO TO 45
C
C     COMPLEX
C
  160 OUTC(1) = IND1
      OUTC(2) = IND2
      GO TO 710
C
C     COMPLEX SQUARE ROOT
C
  170 OUTC(1) = (INC1(1)**2 + INC1(2)**2)**0.25D0
     1          *DCOS(0.5D0*DATAN2(INC1(2),INC1(1)))
      OUTC(2) = (INC1(1)**2 + INC1(2)**2)**0.25D0
     1          *DSIN(0.5D0*DATAN2(INC1(2),INC1(1)))
      GO TO 760
C
C     CONJUGATE
C
  210 OUTC(1) = INC1(1)
      OUTC(2) =-INC1(2)
      GO TO 760
C
C     REAL
C
  190 IND1 = OUTC(1)
      IND2 = OUTC(2)
      GO TO 770
C
C     EQUAL
C
  220 IF (IND1 .EQ. IND2) FLAG = -1
      GO TO 660
C
C     GREATER THAN
C
  230 IF (IND1 .GT. IND2) FLAG = -1
      GO TO 660
C
C     GREATER THAN OR EQUAL
C
  240 IF (IND1 .GE. IND2) FLAG = -1
      GO TO 660
C
C     LESS THAN
C
  250 IF (IND1 .LT. IND2) FLAG = -1
      GO TO 660
C
C     LESS THAN OR EQUAL
C
  260 IF (IND1 .LE. IND2) FLAG = -1
      GO TO 660
C
C     NOT EQUAL
C
  270 IF (IND1 .NE. IND2) FLAG = -1
      GO TO 660
C
C     FIX
C
  300 FLAG = OUTD
      GO TO 720
C
C ---------------------------------------------------
C
C     INPUT PARAMETER ECHO
C
  600 ASSIGN 620 TO IRTN3
      ASSIGN 800 TO IRTN4
  610 IF (.NOT.PRT) GO TO 615
      I = IL3 - 3
      IF (IL3 .LE. 0) WRITE (NOUT,640) ILX(3),PARM,IND1
      IF (IL3 .GT. 0) WRITE (NOUT,640) IVPS(I),IVPS(I+1),IND1
  615 IF (IL3 .EQ. 0) IERR = 1
      GO TO IRTN3, (620,800)
  620 IF (.NOT.PRT) GO TO 645
      J = IL4 - 3
      IF (IL4 .LE. 0) WRITE (NOUT,640) ILX(4),PARM,IND2
      IF (IL4 .GT. 0) WRITE (NOUT,640) IVPS(J),IVPS(J+1),IND2
  640 FORMAT (22X,2A4,3H = ,D15.8,'  (INPUT)')
  645 IF (IL4 .EQ. 0) IERR = 1
      GO TO IRTN4, (800,880,910)
C
  650 ASSIGN 800 TO IRTN3
      GO TO 610
C
  660 ASSIGN 620 TO IRTN3
      ASSIGN 910 TO IRTN4
      GO TO 610
C
  670 IF (.NOT.PRT) GO TO 685
      I = IL8 - 3
      IF (IL8 .LE. 0) WRITE (NOUT,680) ILX(8),PARM,IFLAG
      IF (IL8 .GT. 0) WRITE (NOUT,680) IVPS(I),IVPS(I+1),IFLAG
  680 FORMAT (22X,2A4,2H =,I10,'   (INPUT)')
  685 IF (IL8 .EQ. 0) IERR = 1
      GO TO 800
C
  690 IF (.NOT.PRT) GO TO 705
      I = IL5 - 3
      IF (IL5 .LE. 0) WRITE (NOUT,700) ILX(5),PARM,OUTC
      IF (IL5 .GT. 0) WRITE (NOUT,700) IVPS(I),IVPS(I+1),OUTC
  700 FORMAT (22X,2A4,4H = (,D15.8,1H,,D15.8,')   (INPUT)')
  705 IF (IL5 .EQ. 0) IERR = 1
      GO TO 800
C
  710 ASSIGN 620 TO IRTN3
      ASSIGN 880 TO IRTN4
      GO TO 610
C
  720 IF (.NOT.PRT) GO TO 725
      I = IL2 - 3
      IF (IL2 .LE. 0) WRITE (NOUT,640) ILX(2),PARM,OUTD
      IF (IL2 .GT. 2) WRITE (NOUT,640) IVPS(I),IVPS(I+1),OUTD
  725 IF (IL2 .EQ. 0) IERR = 1
      GO TO 910
C
  730 ASSIGN 750 TO IRTN6
  740 IF (.NOT.PRT) GO TO 745
      I = IL6 - 3
      IF (IL6 .LE. 0) WRITE (NOUT,700) ILX(6),PARM,INC1
      IF (IL6 .GT. 0) WRITE (NOUT,700) IVPS(I),IVPS(I+1),INC1
      IF (IL6 .EQ. 0) IERR = 1
  745 GO TO IRTN6, (750,880)
  750 IF (.NOT.PRT) GO TO 755
      J = IL7 - 3
      IF (IL7 .LE. 0) WRITE (NOUT,700) ILX(7),PARM,INC2
      IF (IL7 .GT. 0) WRITE (NOUT,700) IVPS(J),IVPS(J+1),INC2
  755 IF (IL7 .EQ. 0) IERR = 1
      GO TO 880
C
  760 ASSIGN 880 TO IRTN6
      GO TO 740
C
  770 IF (.NOT.PRT) GO TO 775
      I = IL5 - 3
      IF (IL5 .LE. 0) WRITE (NOUT,700) ILX(5),PARM,OUTC
      IF (IL5 .GT. 0) WRITE (NOUT,700) IVPS(I),IVPS(I+1),OUTC
  775 IF (IL5 .EQ. 0) IERR = 1
      GO TO 840
C
C     OUTPUT PARAMETER CHECK
C
C     SECOND PARAMETER - OUTD
C
  800 IF (IL2  .GT.  0) GO TO 820
      WRITE  (NOUT,810) ILX(2),NAM
  810 FORMAT (22X,A4,'PARAMETER IS MISSING    (OUTPUT ERROR)  ',2A4)
      IERR = 1
      GO TO 950
  820 IF (IERR .EQ. 0) GO TO 825
      TEMP(1) = VPS(IL2  )
      TEMP(2) = VPS(IL2+1)
      OUTD  = TEMPD
  825 TEMPD = OUTD
      VPS(IL2  ) = TEMP(1)
      VPS(IL2+1) = TEMP(2)
      I = IL2 - 3
      IF (PRT) WRITE (NOUT,830) IVPS(I),IVPS(I+1),OUTD
  830 FORMAT (22X,2A4,3H = ,D15.8,'  (OUTPUT)')
      GO TO 950
C
C     THIRD AND FOURTH PARAMETERS - IND1, IND2
C
  840 IF (IL3 .GT.  0) GO TO 850
      WRITE (NOUT,810) ILX(3),NAM
      IERR = 1
      GO TO 860
  850 IF (IERR .EQ. 0) GO TO 855
      TEMP(1) = VPS(IL3  )
      TEMP(2) = VPS(IL3+1)
      IND1  = TEMPD
  855 TEMPD = IND1
      VPS(IL3  ) = TEMP(1)
      VPS(IL3+1) = TEMP(2)
      I = IL3 - 3
      IF (PRT) WRITE (NOUT,830) IVPS(I),IVPS(I+1),IND1
  860 IF (IL4 .GT.  0) GO TO 870
      WRITE (NOUT,810) ILX(4),NAM
      IERR = 1
      GO TO 950
  870 IF (IERR .EQ. 0) GO TO 875
      TEMP(1) = VPS(IL4  )
      TEMP(2) = VPS(IL4+1)
      IND2  = TEMPD
  875 TEMPD = IND2
      VPS(IL4  ) = TEMP(1)
      VPS(IL4+1) = TEMP(2)
      J = IL4 - 3
      IF (PRT) WRITE (NOUT,830) IVPS(J),IVPS(J+1),IND2
      GO TO 950
C
C     FIFTH PARAMETER - OUTC
C
  880 IF (IL5 .GT.  0) GO TO 890
      WRITE (NOUT,810) ILX(5),NAM
      IERR = 1
      GO TO 950
  890 IF (IERR .EQ. 0) GO TO 895
      TEMP(1) = VPS(IL5  )
      TEMP(2) = VPS(IL5+1)
      OUTC(1) = TEMPD
      TEMP(1) = VPS(IL5+2)
      TEMP(2) = VPS(IL5+3)
      OUTC(2) = TEMPD
  895 TEMPD = OUTC(1)
      VPS(IL5  ) = TEMP(1)
      VPS(IL5+1) = TEMP(2)
      TEMPD = OUTC(2)
      VPS(IL5+2) = TEMP(1)
      VPS(IL5+3) = TEMP(2)
      I = IL5 - 3
      IF (PRT) WRITE (NOUT,900) IVPS(I),IVPS(I+1),OUTC
  900 FORMAT (22X,2A4,4H = (,D15.8,1H,,D15.8,')   (OUTPUT)')
      GO TO 950
C
C     EIGHTH PARAMETER - FLAG
C
  910 IF (IL8 .GT.  0) GO TO 920
      WRITE (NOUT,810) ILX(8),NAM
      IERR = 1
      GO TO 950
  920 IF (IERR .EQ. 0) IVPS(IL8) = FLAG
      I = IL8 - 3
      IF (PRT) WRITE (NOUT,930) IVPS(I),IVPS(I+1),IVPS(IL8)
  930 FORMAT (22X,2A4,2H =,I12,6X,'(OUTPUT)')
C
  950 IF (IERR .EQ.  0) GO TO 990
      WRITE  (NOUT,960) UWM,NAM
  960 FORMAT (A25,' - I/O ERROR, OUTPUT NOT SAVED. OUTPUT DEFAULT ',
     1       'VALUE REMAINS ',2A4,/)
      GO TO 990
  970 WRITE  (NOUT,980) NAM
  980 FORMAT (5X,'JOB TERMINTATED DUE TO PREVIOUS ERROR(S)  ',2A4,/)
      CALL PEXIT
  990 IF (KSYS37 .EQ. 0) KSYS37 = IERR
      RETURN
C
      END
