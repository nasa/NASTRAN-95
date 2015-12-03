      SUBROUTINE MPLPRT
C
C     PRINTS MPL TABLE FOR DOCUMENTATION PURPOSES
C     AND CHECKS VALIDITY OF MANY ITEMS.
C
      DOUBLE PRECISION XX
      REAL            X(2,1)
      INTEGER         KP(6),FLAG,FLAGB,FLAGS,TOT,FLGTOT,ADD(2),
     1                T1,T2,T3,H1,H2,H3,H1X(32),H2X(32),H3X(32)
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25,SWM*27
      COMMON /XMSSG / UFM,UWM,UIM,SFM,SWM
      COMMON /SYSTEM/ NB,NO,JUNK1(6),NLPP,JUNK2(2),LINE
      COMMON /XFIST / NFIST
      COMMON /XPFIST/ NPFIST
      COMMON /OUTPUT/ T1(32),T2(32),T3(32),H1(32),H2(32),H3(32)
      COMMON /XGPI2 / LMPL,MPLPNT,MPL(1)
      COMMON /XGPI2X/ XX(1)
C
      EQUIVALENCE    (XX(1),X(1,1))
C
      DATA KP    / 1,1,2,2,2,4 / ,  ADD   /4HADD ,4H    /
      DATA FLAGB / 1H  /  ,  FLAGS /4H ***/
      DATA H1X/4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     2        ,4H   M,4H O D,4H U L,4H E  ,4H P R,4H O P,4H E R,4H T I
     3        ,4H E S,4H   L,4H I S,4H T  ,4H    ,4H    ,4H    ,4H
     4        ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    /
      DATA H2X/4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H    ,4H
     2        ,4H    ,4H    ,4H    ,4H    ,4H    ,4H   -,4H - -,4H - -
     3        ,4H - -,4H P A,4H R A,4H M E,4H T E,4H R S,4H - -,4H - -
     4        ,4H - -,4H - -,4H    ,4H    ,4H    ,4H    ,4H    ,4H    /
      DATA H3X/4H  MP,4HLID ,4HNWDS,4H  WD,4H1  M,4HOD-N,4HAME ,4HTYP
     2        ,4H IN ,4H OUT,4H  SC,4HR  T,4HOT  ,4H   I,4HD TY,4HP
     3        ,4HP   ,4H   D,4HEFAU,4HLT (,4HIF A,4HNY) ,4H    ,4H   W
     4        ,4H1-W2,4H FLG,4H    ,4H    ,4H    ,4H    ,4H    ,4H    /
C
   21 FORMAT (7H0      ,3I5,2X ,2A4 ,I3,4I5  ,10X,A3)
   22 FORMAT (7H0      ,3I5,2X ,2A4 ,I3,4I5
     1       ,10X,50H----- N O   P A R A M E T E R S   E X I S T -----
     2       ,10X,A3)
   23 FORMAT (7H0      ,3I5,2X,8H (NONE) )
   24 FORMAT (7H0      ,3I5,2X,2A4,I3    )
   31 FORMAT (59X,I2,5H. INT,I5,7X,16H-- NO DEFAULT --,6X ,I2          )
   32 FORMAT (59X,I2,5H. RSP,I5,7X,16H-- NO DEFAULT --,6X ,I2          )
   33 FORMAT (59X,I2,5H. BCD,I5,7X,16H-- NO DEFAULT --,6X ,I2,1H-,I2   )
   34 FORMAT (59X,I2,5H. RDP,I5,7X,16H-- NO DEFAULT --,6X ,I2,1H-,I2,A4)
   35 FORMAT (59X,I2,5H. CSP,I5,7X,16H-- NO DEFAULT --,6X ,I2,1H-,I2,A4)
   36 FORMAT (59X,I2,5H. CDP,I5,7X,16H-- NO DEFAULT --,6X ,I2,1H-,I2,A4)
   41 FORMAT (59X,I2,5H. INT,I5, I15,14X                  ,I2          )
   42 FORMAT (59X,I2,5H. RSP,I5, 1P,E20.4,9X              ,I2          )
   43 FORMAT (59X,I2,5H. BCD,I5, 11X,2A4,10X              ,I2,1H-,I2   )
   44 FORMAT (59X,I2,5H. RDP,I5, 1P,D20.4,9X              ,I2,1H-,I2,A4)
   45 FORMAT (59X,I2,5H. CSP,I5, 3H  (,1P,E11.4,1H, ,1P,E11.4,3H   ,
     1                                                     I2,1H-,I2,A4)
   46 FORMAT (59X,I2,5H. CDP,I5, 3H  (,1P,D11.4,1H, ,1P,D11.4,3H)  ,
     1                                                     I2,1H-,I2,A4)
   47 FORMAT (10X,'NOTE - THE ABOVE PARAMETER DEFAULTS WILL BE CHANGED',
     1 ' TO ALL ZEROS BY THE ADD MODULE.  HOWEVER, IF ALL 4 PARAMETERS',
     2 ' ARE NOT', /10X,'SPECIFIED, THEY WILL BE CHANGED TO 2*(1.,0.),',
     3 ' 2*(0.D0,0.D0), OR 2*(0.,0.), 2*(1.D0,0.D0) DEPENDING ON ',
     4 'MATRICES INVOLVED')
C
C     INITIALIZATION
C
      CALL PAGE
      MPLID = 0
      NPAD  = 0
      I2    = 0
      DO 80 I = 1,32
      H1(I) = H1X(I)
      H2(I) = H2X(I)
      H3(I) = H3X(I)
   80 CONTINUE
      CALL PAGE
C
C     PROCESS NEXT ENTRY
C
  100 CONTINUE
      IF (I2-LMPL) 110,900,9901
  110 I0 = I2
      I1 = I2 + 1
      I2 = I0 + MPL(I1)
      MPLID = MPLID + 1
C
C     TEST FOR MODULE TYPE
C
      IF (MPL(I1)-1) 9904,120,112
  112 IF (MPL(I1+1) .EQ. 0) GO TO 120
      IF (MPL(I0+4) .LT. 3) GO TO 130
C
C     EXECUTIVE MODULE
C
      CALL PAGE2 (-2)
      L1 = I0 + 2
      L2 = L1 + 2
      WRITE (NO,24) MPLID,MPL(I1),I1,(MPL(L),L=L1,L2)
      GO TO 100
C
C     PAD SPACE
C
  120 CALL PAGE2 (-2)
      WRITE (NO,23) MPLID,MPL(I1),I1
      NPAD = NPAD + 1
      GO TO 100
C
C     FUNCTIONAL MODULE
C
  130 IF (MPL(I1) .GT. 7) GO TO 140
C
C     NO PARAMETERS EXIST FOR THIS FUNCTIONAL MODULE
C
      CALL PAGE2 (-2)
      L1  = I0 + 5
      L2  = L1 + 2
      TOT = 0
      DO 137 L = L1,L2
  137 TOT = TOT + MPL(L)
      FLGTOT = FLAGB
      IF (TOT .GT. NFIST-NPFIST) FLGTOT = FLAGS
      L1  = I0 + 2
      WRITE (NO,22) MPLID,MPL(I1),I1,(MPL(L),L=L1,L2),TOT,FLGTOT
      GO TO 100
C
C     PARAMETERS EXIST FOR THIS FUNCTIONAL MODULE
C
  140 CONTINUE
C
C     DETERMINE THE NUMBER OF PARAMETERS FOR FUNCTIONAL MODULE
C
      NP = 0
      I  = I0 + 8
  150 CONTINUE
      IF ((I-1)-(I2)) 151,160,9903
  151 IP = IABS(MPL(I))
      IF (IP .GT. 6) GO TO 9902
      IF (MPL(I)) 152,9902,154
  152 NP = NP + 1
      I  = I  + 1
      GO TO 150
  154 NP = NP + 1
      I  = I + 1 + KP(IP)
      GO TO 150
  160 IF (NP .LE. 0) GO TO 9903
C
      CALL PAGE2 (-2-NP)
      L1 = I0 + 5
      L2 = L1 + 2
      TOT= 0
      DO 167 L = L1,L2
  167 TOT = TOT + MPL(L)
      FLGTOT = FLAGB
      IF (TOT .GT. NFIST-NPFIST) FLGTOT = FLAGS
      L1 = I0 + 2
      WRITE (NO,21) MPLID,MPL(I1),I1,(MPL(L),L=L1,L2),TOT,FLGTOT
C
C     PRINT PARAMETERS
C
      NP = 0
      I  = I0 + 8
      J2 = 0
  170 CONTINUE
      NP = NP + 1
      J1 = J2 + 1
      IF ((I-1)-(I2)) 175,200,9903
  175 IP = IABS(MPL(I))
      IF (IP .GT. 6) GO TO 9902
      IF (MPL(I)) 180,9902,190
C
C     PARAMETER HAS NO DEFAULT VALUE
C
  180 CONTINUE
      J2 = J1
      GO TO (181,182,183,184,185,186), IP
C
C     INTEGER
C
  181 WRITE (NO,31) NP,I,J1
      GO TO 188
C
C     REAL SINGLE-PRECISION
C
  182 WRITE (NO,32) NP,I,J1
      GO TO 188
C
C     ALPHANUMERIC (BCD)
C
  183 J2 = J2 + 1
      WRITE (NO,33) NP,I,J1,J2
      GO TO 188
C
C     REAL DOUBLE-PRECISION
C
  184 J2 = J2 + 1
      FLAG = FLAGB
      IF (MOD(J1,2) .EQ. 0) FLAG = FLAGS
      WRITE (NO,34) NP,I,J1,J2,FLAG
      GO TO 188
C
C     COMPLEX SINGLE-PRECISION
C
  185 J2   = J2 + 1
      FLAG = FLAGB
      IF (MOD(J1,2) .EQ. 0) FLAG = FLAGS
      WRITE (NO,35) NP,I,J1,J2,FLAG
      GO TO 188
C
C     COMPLEX DOUBLE-PRECISION
C
  186 J2   = J2 + 3
      FLAG = FLAGB
      IF (MOD(J1,2) .EQ. 0) FLAG = FLAGS
      WRITE (NO,36) NP,I,J1,J2,FLAG
      GO TO 188
  188 CONTINUE
      I = I + 1
      GO TO 170
C
C     PARAMETER HAS A DEFAULT VALUE
C
  190 CONTINUE
      GO TO (191,192,193,194,195,196), IP
C
C     INTEGER
C
  191 J2 = J1
      WRITE (NO,41) NP,I,MPL(I+1),J1
      I = I + 2
      GO TO 198
C
C     REAL SINGLE-PRECISION
C
  192 J2 = J1
      M  = MPL(I+1)
      WRITE (NO,42) NP,I,X(1,M),J1
      I  = I + 2
      GO TO 198
C
C     ALPHANUMERIC (BCD)
C
  193 J2 = J1 + 1
      WRITE (NO,43) NP,I,MPL(I+1),MPL(I+2),J1,J2
      I = I + 3
      GO TO 198
C
C     REAL DOUBLE-PRECISION
C
  194 J2 = J1 + 1
      M  = MPL(I+1)
      FLAG = FLAGB
      IF (MOD(J1,2) .EQ. 0) FLAG = FLAGS
      WRITE (NO,44) NP,I,XX(M),J1,J2,FLAG
      I = I + 3
      GO TO 198
C
C     COMPLEX SINGLE-PRECISION
C
  195 J2 = J1 + 1
      M  = MPL(I+1)
      FLAG = FLAGB
      IF (MOD(J1,2) .EQ. 0) FLAG = FLAGS
      WRITE (NO,45) NP,I,X(1,M),X(2,M),J1,J2,FLAG
      I  = I + 3
      GO TO 198
C
C     COMPLEX DOUBLE-PRECISION
C
  196 J2 = J1 + 3
      M1 = MPL(I+1)
      M2 = MPL(I+3)
      FLAG = FLAGB
      IF (MOD(J1,2) .EQ. 0) FLAG = FLAGS
      WRITE (NO,46) NP,I,XX(M1),XX(M2),J1,J2,FLAG
      I = I + 5
      GO TO 198
  198 CONTINUE
      GO TO 170
C
  200 CONTINUE
      IF (MPL(L1).NE.ADD(1) .OR. MPL(L1+1).NE.ADD(2)) GO TO 100
      CALL PAGE2 (-2)
      WRITE (NO,47)
      GO TO 100
C
C     TERMINATION
C
  900 CONTINUE
      CALL PAGE2 (-4)
      WRITE  (NO,901)
  901 FORMAT ('0*** END OF MPL PRINTOUT')
      WRITE  (NO,902) MPLID,NPAD
  902 FORMAT ('0*** THE MPL CONTAINS ',I3,' ENTRYS.  OF THESE, ',I3,
     1       ' ARE PAD ENTRYS.')
C
      RETURN
C
C     ERROR MESSAGES
C
 9901 WRITE  (NO,9951) SWM,I2,LMPL
 9951 FORMAT (A27,' 65, POINTER I2 =',I10,' DOES NOT AGREE WITH LMPL =',
     1        I11)
      GO TO 9995
C
 9902 WRITE  (NO,9952) SWM
 9952 FORMAT (A27,' 66, ILLEGAL PARAMETER TYPE CODE.')
      GO TO 9995
C
 9903 WRITE  (NO,9953) SWM
 9953 FORMAT (A27,' 67, ERROR IN PARAMETER SEQUENCE.')
      GO TO 9995
C
 9904 WRITE  (NO,9954) SWM
 9954 FORMAT (A27,' 68, ILLEGAL WORD COUNT.')
C
C
 9995 CALL PAGE2 (4)
      WRITE  (NO,9996)
 9996 FORMAT (5X,'MPL TABLE LISTING CANCELLED.')
C
      RETURN
      END
