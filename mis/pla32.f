      SUBROUTINE PLA32
C
C     THIS ROUTINE READS THE ESTNLS DATA BLOCK CREATED IN SUBROUTINE
C     PLA31, AND CALLS THE PROPER ELEMENT ROUTINE TO COMPUTE ELEMENT
C     STRESSES.
C     ELEMENT STRESS INFORMATION IS STORED BY THE ELEMENT ROUTINE IN
C     /STROUT/.  THE ELEMENT ROUTINE ALSO UPDATES THE EST ENTRY WHICH
C     HAS BEEN COMMUNICATED TO IT VIA /PLA32E/.  NOTE THAT THIS UPDATED
C     EST ENTRY DOES NOT CONTAIN DISPLACEMENT VECTOR INFORMATION.
C
      INTEGER         BUFSZ,BUFR1,BUFR2,BUFR3,CSTM,DIT,ESTNLS,CASECC,
     1                ONLES,ESTNL1,EOR,CLSRW,FILE,IZ(1),IESTBK(100),
     9                ESTWDS(40),ELTYPE,OUTRW,PLACNT,PLSETN,PLANOS(2),
     2                OSTRT(7),ESTT(7),SETNO
      DIMENSION       NAME(2),NSTWDS(40),NWDSP2(40),P(4),IP(4),DUM2(2),
     1                TUBSAV(20),ICHAR(9),ITITLE(3),IY(30)
      COMMON /BLANK / PLACNT,PLSETN
      COMMON /SYSTEM/ BUFSZ
      COMMON /CONDAS/ PI,TWOPI,RADEG,DEGRA,S4PISQ
      COMMON /ZZZZZZ/ Z(1)
      COMMON /PLA32C/ GAMMA,GAMMAS,IPASS
      COMMON /PLA32E/ ESTBK(100)
C
C     SCRATCH BLOCK USED BY ELEMENT ROUTINES (325 SINGLE PRECISION
C     CELLS)  AND OUTPUT BLOCK FOR ELEMENT STRESSES
C
      COMMON /PLA32S/ XXXXXX(325)
      COMMON /SOUT  / YYYYYY(30)
      EQUIVALENCE     (Z(1),IZ(1)),(ESTBK(1),IESTBK(1)),(P(1),IP(1)),
     1                (YYYYYY(1),IY(1))
      DATA    NAME  / 4HPLA3, 4H2          /
      DATA    ITITLE/ 4HLOAD, 4H FAC,4HTOR /
      DATA    CSTM  , MPT,DIT,ESTNLS,CASECC/ 101,102,103,301,106 /
      DATA    ONLES , ESTNL1  / 201,202    /
      DATA    INRW  , OUTRW,EOR,NEOR,CLSRW / 0,1,1,0,1 /
      DATA    PLANOS/ 1103,11 /
C
C    1        ROD       BEAM      TUBE      SHEAR     TWIST
C    2        TRIA1     TRBSC     TRPLT     TRMEM     CONROD
C    3        ELAS1     ELAS2     ELAS3     ELAS4     QDPLT
C    4        QDMEM     TRIA2     QUAD2     QUAD1     DAMP1
C    5        DAMP2     DAMP3     DAMP4     VISC      MASS1
C    6        MASS2     MASS3     MASS4     CONM1     CONM2
C    7        PLOTEL    REACT     QUAD3     BAR       CONE
C    8        TRIARG    TRAPRG    TORDRG    CORE      CAP
C
      DATA    ESTWDS/
     1           21,        0,       20,        0,        0,
     2           38,        0,        0,       27,       21,
     3            0,        0,        0,        0,        0,
     4           32,       32,       37,       43,        0,
     5            0,        0,        0,        0,        0,
     6            0,        0,        0,        0,        0,
     7            0,        0,        0,       50,        0,
     8            0,        0,        0,        0,        0 /
      DATA    NSTWDS/
     1            5,        0,        5,        0,        0,
     2           17,        0,        0,        8,        5,
     3            0,        0,        0,        0,        0,
     4            8,       17,       17,       17,        0,
     5            0,        0,        0,        0,        0,
     6            0,        0,        0,        0,        0,
     7            0,        0,        0,       16,        0,
     8            0,        0,        0,        0,        0 /
      DATA    NWDSP2/
     1           33,        0,       32,        0,        0,
     2           56,        0,        0,       36,       33,
     3            0,        0,        0,        0,        0,
     4           44,       50,       61,       67,        0,
     5            0,        0,        0,        0,        0,
     6            0,        0,        0,        0,        0,
     7            0,        0,        0,       62,        0,
     8            0,        0,        0,        0,        0 /
C
C     DEFINE POSITION IN CASECC RECORD OF DESTINATION (PRINTER, PUNCH,
C     ETC.) OF ELEMENT STRESSES.
C
      DATA          IDEST /24/
C
C
C     DETERMINE SIZE OF CORE, DEFINE BUFFERS AND INITIALIZE CORE POINTER
C     AND COUNTERS
C
      IZMAX = KORSZ(Z)
      BUFR1 = IZMAX - BUFSZ
      BUFR2 = BUFR1 - BUFSZ
      BUFR3 = BUFR2 - BUFSZ
      LEFT  = BUFR3 - 1
      IPASS = PLACNT- 1
      ICSTM = 0
      NCSTM = 0
      DO 5 I = 1,7
      OSTRT(I) = 0
    5 ESTT(I)  = 0
C
C     ATTEMPT TO READ CSTM INTO CORE
C
      FILE = CSTM
      CALL OPEN (*20,CSTM,Z(BUFR1),INRW)
      CALL FWDREC (*9020,CSTM)
      CALL READ (*9020,*10,CSTM,Z(ICSTM+1),LEFT,EOR,NCSTM)
      CALL MESAGE (-8,0,NAME)
   10 LEFT = LEFT - NCSTM
      CALL CLOSE (CSTM,CLSRW)
      CALL PRETRS (Z(ICSTM+1),NCSTM)
   20 IMAT = NCSTM
C
C     COMPUTE GAMMA AND GAMMAS FROM THE PROPER PLFACT CARD
C
      FILE = MPT
      CALL PRELOC (*9010,Z(BUFR1-3),MPT)
      CALL LOCATE (*9040,Z(BUFR1-3),PLANOS,IFLAG)
   30 CALL READ (*9020,*9030,MPT,SETNO,1,NEOR,IFLAG)
      IF (SETNO .EQ. PLSETN) GO TO 40
   35 CALL READ (*9020,*9030,MPT,NN,1,NEOR,IFLAG)
      IF (NN .EQ. -1) GO TO 30
      GO TO 35
   40 IF (PLACNT .LE. 4) GO TO 45
      CALL READ (*9020,*9030,MPT,0,-(PLACNT-4),NEOR,IFLAG)
   45 NWDSRD = 4
      IF (PLACNT .LT. 4) NWDSRD = PLACNT
      CALL READ (*9020,*9030,MPT,P,NWDSRD,NEOR,IFLAG)
      IF (IP(NWDSRD) .NE. -1) GO TO 48
      IF (PLACNT-3) 42,43,44
  42  GAMMAS = 1.0
      GO TO 46
  43  GAMMAS = (P(2) - P(1))/P(1)
      GO TO 46
  44  GAMMAS = (P(3) - P(2))/(P(2) - P(1))
  46  GAMMA = 1.0
      GO TO 65
  48  A = P(2) - P(1)
      IF (PLACNT-3) 50,55,60
   50 GAMMAS = 0.0
      GAMMA  = A/P(1)
      GO TO 65
  55  GAMMAS = A/P(1)
      GAMMA = (P(3) - P(2))/A
      GO TO 65
   60 WORD = P(3) - P(2)
      GAMMAS = WORD/A
      GAMMA  = (P(4) - P(3))/WORD
   65 CALL CLOSE (MPT,CLSRW)
C
C     READ MPT AND DIT FILES.  NOTE MINUS SIGN ON DIT TO TRIGGER PLA
C     FLAG.
C
      CALL PREMAT (IZ(IMAT+1),Z(IMAT+1),Z(BUFR1-3),LEFT,MUSED,MPT,-DIT)
      LEFT = LEFT  - MUSED
      ICC  = NCSTM + MUSED
C
C     READ CASECC INTO OPEN CORE
C
      FILE = CASECC
      CALL OPEN (*9010,CASECC,Z(BUFR1),INRW)
      CALL FWDREC (*9020,CASECC)
      CALL READ (*9020,*68,CASECC,Z(ICC+1),LEFT,EOR,NCC)
      CALL MESAGE (-8,0,NAME)
   68 LEFT = LEFT - NCC
      CALL CLOSE (CASECC,CLSRW)
C
C OPEN INPUT FILE
C
      FILE = ESTNLS
      CALL OPEN (*9010,ESTNLS,Z(BUFR1),INRW)
      CALL FWDREC (*9020,ESTNLS)
C
C     OPEN THE ELEMENT STRESS FILE FOR OUTPUT AND BUILD HEADER WHICH IS
C     NON-CHANGING.
C
      FILE = ONLES
      CALL OPEN (*9010,ONLES,Z(BUFR2),OUTRW)
      CALL FNAME (ONLES,DUM2)
      CALL WRITE (ONLES,DUM2,2,EOR)
C
C     THE FOLLOWING INDICES HAVE TO CHANGE  WHEN THERE ARE CHANGES IN
C     THE FORMAT OF THE CASECC DATA BLOCK
C
      IONLES = ICC + NCC
      IZ(IONLES+1) = IZ(ICC+18) + 100
      IZ(IONLES+2) = 5
      IZ(IONLES+4) = IZ(ICC+1)
      IZ(IONLES+5) = IZ(ICC+4)
      IZ(IONLES+6) = 0
      IZ(IONLES+7) = 0
      IZ(IONLES+8) = 0
      IZ(IONLES+9) = 0
      ILOW  = IONLES + 51
      IHIGH = IONLES + 146
      LEFT  = LEFT - 146
      IF (LEFT .LT. 0) CALL MESAGE (-8,0,NAME)
      J = ICC + 38
      DO 70 I = ILOW,IHIGH
      J = J + 1
   70 IZ(I) = IZ(J)
C
C     STORE LOAD FACTOR AND INTEGER IN LABEL PORTION OF OUTPUT
C
      IZ(IONLES+135) = ITITLE(1)
      IZ(IONLES+136) = ITITLE(2)
      IZ(IONLES+137) = ITITLE(3)
      III = PLACNT - 1
      CALL INT2AL (III,IZ(IONLES+138),ICHAR)
C
C     DEFINE DESTINATION OF OUTPUT
C
      I = ICC + IDEST
      JDEST = IZ(I)
C
C     OPEN THE ESTNL1 FILE FOR OUTPUT.
C
      FILE = ESTNL1
      CALL OPEN (*9010,ESTNL1,Z(BUFR3),OUTRW)
      CALL FNAME (ESTNL1,DUM2)
      CALL WRITE (ESTNL1,DUM2,2,EOR)
      FILE = ESTNLS
C
C     READ ELEMENT TYPE
C
   80 CALL READ (*220,*9030,ESTNLS,ELTYPE,1,NEOR,IFLAG)
C
C     FILL IN REMAINDER OF ID RECORD FOR THE ONLES FILE
C
      IZ(IONLES+3)  = ELTYPE
      IZ(IONLES+10) = NSTWDS(ELTYPE)
      IF (NSTWDS(ELTYPE) .LE. 0) CALL MESAGE (-30,91,ELTYPE)
C
C     WRITE ID RECORD FOR ONLES FILE
C
      CALL WRITE (ONLES,IZ(IONLES+1),146,EOR)
      CALL WRITE (ESTNL1,ELTYPE,1,NEOR)
C
C     READ AN ENTRY FROM THE APPENDED ESTNL FILE AND CALL THE PROPER
C     ROUTINE
C
   90 CALL READ (*9020,*210,ESTNLS,ESTBK,NWDSP2(ELTYPE),NEOR,IFLAG)
C
C               1,ROD    2,BEAM    3,TUBE   4,SHEAR   5,TWIST
      GO TO (     110,      999,      120,      999,      999,
C             6,TRIA1   7,TRBSC   8,TRPLT   9,TRMEM 10,CONROD
     1            130,      999,      999,      140,      110,
C            11,ELAS1  12,ELAS2  13,ELAS3  14,ELAS4  15,QDPLT
     2            999,      999,      999,      999,      999,
C            16,QDMEM  17,TRIA2  18,QUAD2  19,QUAD1  20,DAMP1
     3            150,      160,      170,      180,      999,
C            21,DAMP2  22,DAMP3  23,DAMP4   24,VISC  25,MASS1
     4            999,      999,      999,      999,      999,
C            26,MASS2  27,MASS3  28,MASS4  29,CONM1  30,CONM2
     5            999,      999,      999,      999,      999,
C           31,PLOTEL  32,REACT  33,QUAD3    34,BAR   35,CONE
     6            999,      999,      999,      190,      999,
C           36,TRIARG 37,TRAPRG 38,TORDRG   39,CORE?   40,CAP?
     7            999,      999,      999,      999,      999), ELTYPE
C
C     ROD, CONROD
C
  110 CALL PSROD
C
C     IF ELEMENT IS A TUBE, RESTORE SAVED EST ENTRY AND STORE UPDATED
C     STRESS VARIABLES IN PROPER SLOTS.
C
      IF (ELTYPE .NE. 3) GO TO 200
      DO 115 I = 1,16
  115 ESTBK(I) = TUBSAV(I)
      ESTBK(17) = ESTBK(18)
      ESTBK(18) = ESTBK(19)
      ESTBK(19) = ESTBK(20)
      ESTBK(20) = ESTBK(21)
      GO TO 200
C
C
C     TUBE - REARRANGE ESTBK FOR THE TUBE SO THAT IT IS IDENTICAL TO THE
C            ONE FOR THE ROD
C
C     SAVE THE EST ENTRY FOR THE TUBE EXCEPT THE 4 WORDS WHICH WILL BE
C     UPDATED BY THE THE ROD ROUTINE AND THE DISPLACEMENT VECTORS
C
  120 DO 125 I = 1,16
  125 TUBSAV(I) = ESTBK(I)
C
C     COMPUTE AREA, TORSIONAL INERTIA TERM AND STRESS COEFFICIENT
C
      D  = ESTBK(5)
      T  = ESTBK(6)
      DMT= D - T
      A  = DMT*T* PI
      FJ = .25*A*(DMT**2 + T**2)
      C  = D/2.0
C
C     MOVE THE END OF THE ESTBK ARRAY DOWN ONE SLOT SO THAT ENTRIES 7
C     THRU 32 WILL BE MOVED TO POSITIONS 8 THRU 33.
C
      M = 33
      DO 127 I = 1,26
      ESTBK(M) = ESTBK(M-1)
  127 M = M - 1
      ESTBK(5) = A
      ESTBK(6) = FJ
      ESTBK(7) = C
      GO TO 110
C
C     TRIA1
C
  130 CALL PSTRI1
      GO TO 200
C
C     TRMEM
C
  140 CALL PSTRM
      GO TO 200
C
C     QDMEM
C
  150 CALL PSQDM
      GO TO 200
C
C     TRIA2
C
  160 CALL PSTRI2
      GO TO 200
C
C     QUAD2
C
  170 CALL PSQAD2
      GO TO 200
C
C     QUAD1
C
  180 CALL PSQAD1
      GO TO 200
C
C     BAR
C
  190 CALL PSBAR
      GO TO 200
C
C     ALTER ELEMENT IDENTIFICATION FROM EXTERNAL (USER) IDENTIFICATION
C     TO INTERNAL ID., AND WRITE OUTPUT FILES.
C
  200 IY(1) = 10*IY(1) + JDEST
      CALL WRITE (ONLES,IY,NSTWDS(ELTYPE),NEOR)
      CALL WRITE (ESTNL1,ESTBK,ESTWDS(ELTYPE),NEOR)
      OSTRT(2) = OSTRT(2) + 1
      ESTT(2)  = ESTT(2)  + 1
      GO TO 90
C
C     WRITE EORS
C
  210 CALL WRITE (ONLES,0,0,EOR)
      CALL WRITE (ESTNL1,0,0,EOR)
      GO TO 80
C
C     CLOSE FILES AND WRITE TRAILERS
C
  220 CALL CLOSE (ONLES,CLSRW)
      CALL CLOSE (ESTNL1,CLSRW)
      CALL CLOSE (ESTNLS,CLSRW)
      OSTRT(1) = ONLES
      ESTT(1)  = ESTNL1
      CALL WRTTRL (OSTRT)
      CALL WRTTRL (ESTT)
      RETURN
C
C     FATAL ERRORS
C
  999 CALL MESAGE (-30,92,ELTYPE)
 9010 J = -1
      GO TO 9050
 9020 J = -2
      GO TO 9050
 9030 J = -3
      GO TO 9050
 9040 J = -5
 9050 CALL MESAGE (J,FILE,NAME)
      RETURN
      END
