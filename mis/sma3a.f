      SUBROUTINE SMA3A (MCBCUR)
C*****
C THIS ROUTINE BUILDS A GENERAL ELEMENT MATRIX (DOUBLE PRECISION AND
C SYMMETRIC) OF SIZE LUSET X LUSET.  MCBCUR IS THE MATRIX CONTROL BLOCK
C FOR THIS MATRIX.
C*****
      DOUBLE PRECISION
     1                   DQ(1)              ,DPWORD
     2,                  DET
      INTEGER
     1                   IQ(1)              ,EOR
     2,                  OUTRW              ,CLSRW
     3,                  CLSNRW             ,SMALL
      LOGICAL
     1                   ZONLY
C
      DIMENSION
     1                   MCBCUR(7)          ,MCB(7)
     2,                  Q(1)               ,IBUFF3(3)
     3,                  NAME(2)
C
      COMMON /BLANK/
     1                   LUSET              ,NGENEL
     2,                  NOECPT
      COMMON   /SYSTEM/
     1                   ISYS               ,DUMMY(53)
     2,                  IPREC
      COMMON   /ZZZZZZ/
     1                   Q
      COMMON   /GENELY/
     1                   IFGEI              ,IFKGGX
     2,                  IFOUT              ,IFA
     3,                  IFB                ,IFC
     4,                  IFD                ,IFE
     5,                  IFF
     8,                  INRW               ,OUTRW
     9,                  CLSRW              ,CLSNRW
     T,                  EOR                ,NEOR
     1,                  MCBA(7)            ,MCBB(7)
     2,                  MCBC(7)            ,MCBD(7)
     3,                  MCBE(7)            ,MCBF(7)
     4,                  MCBKGG(7)
     1,                  IUI                ,IUD
     2,                  IZI                ,IS
     3,                  IZIS               ,ISTZIS
     4,                  IBUFF3             ,LEFT
      COMMON   /ZBLPKX/
     1                   DPWORD             ,DUM2(2)
     2,                  INDEX
C
      EQUIVALENCE
     1                   (IQ(1),DQ(1),Q(1))
     2,                  (IBUFF3(2),M)      ,(IBUFF3(3),N)
C
      DATA               NAME(1)/4HSMA3/    ,NAME(2)/4HA   /
C
C MAKE THE ARGUMENT A LOCAL VARIABLE
C
      DO 10 I=1,7
   10 MCB(I) = MCBCUR(I)
C
C READ THE UI SET OF SCALAR INDEX NUMBERS INTO OPEN CORE.
C
      CALL FREAD(IFGEI,IQ(IUI+1),M,0)
C
C IUD POINTS TO THE ZEROTH LOCATION OF THE UD ARRAY.
C
      IUD = IUI + M
      LEFT = LEFT - M
C
C SET UP ARITHMETIC CONSTANTS.
C
      MPN = M + N
      MSQ = M**2
      NSQ = N**2
      ZONLY = .FALSE.
      IF (N .EQ. 0) ZONLY = .TRUE.
      IF (ZONLY) GO TO 20
C
C SINCE N .NE. 0, THE UD SET EXISTS.  READ IT INTO CORE.
C
      CALL FREAD(IFGEI,IQ(IUD+1),N,0)
      LEFT = LEFT - N
C
C BUILD THE ARRAY IQ(IP+1),IQ(IP+2),...,IQ(IP+MPN) SUCH THAT
C IQ(IP+K) = L IMPLIES IQ(IUI+L) IS THE K TH SMALLEST NUMBER OF THE
C SET OF NUMBERS IQ(IUI+1),...,IQ(IUD+N)
C
   20 IP = IUI + MPN
      K  = IP
      LIMK = IP + MPN
      LOW = IUI + 2
      LIM = IUI + MPN
   30 SMALL = IQ(IUI+1)
      ISMALL = IUI + 1
      DO 40 J=LOW,LIM
      IF (IQ(J) .GE. SMALL) GO TO 40
      SMALL  = IQ(J)
      ISMALL = J
   40 CONTINUE
      K = K + 1
      IDIFF = ISMALL - IUI
      IQ(K) = IDIFF
      IQ(IDIFF) = IQ(IDIFF) + LUSET
      IF (K .LT. LIMK) GO TO 30
      LOW = IUI + 1
      DO 50 I=LOW,LIM
      IF (IQ(I) .LE. LUSET) CALL MESAGE (-30,28,5)
   50 IQ(I) = IQ(I) - LUSET
C
C READ INDICATOR OF Z OR K MATRIX
C
      CALL FREAD(IFGEI,IZK,1,0)
C
C SET UP POINTERS TO THE ZEROTH LOCATION OF THE DOUBLE PRECISION ARRAYS
C       -1
C K  ORZ  AND S
C  E    E      E
C
      IZI = (IUI + 2*MPN - 1) / 2  +  2
      IS = IZI + MSQ
C
C READ IN THE M**2 SINGLE PRECISION ELEMENTS OF THE SYMMETRIC Z OR K
C INTO A TEMPORARY BUFFER BEGINNING AT Q(IBUFF)
C
      IBUFF = IUI + 2 * (MPN + MSQ)
C
C IF ALL OF Z OR K CANNOT FIT INTO THIS BUFFER, READ BLOCKS OF M WORDS
C
      IF (IBUFF + MSQ .GT. LEFT) GO TO 70
      IND = NEOR
      IF (ZONLY) IND = EOR
      CALL FREAD(IFGEI,IQ(IBUFF+1),MSQ,IND)
C
C STORE THE SINGLE PRECISION MATRIX IN ITS DOUBLE PRECISION LOCATION.
C
      LIM = IZI + MSQ
      I   = IZI
      J   = IBUFF
   60 I   = I + 1
      IF (I .GT. LIM) GO TO 100
      J   = J + 1
      DQ(I) = Q(J)
      GO TO 60
C
C READ Z OR K INTO THE BUFFER M WORDS AT A TIME AND STORE M WORDS
C AT A TIME
C
   70 IND = NEOR
      DO 90 K=1,M
      IF (K .EQ. M  .AND.  ZONLY) IND = EOR
      CALL FREAD(IFGEI,Q(IBUFF+1),M,IND)
      I = IZI + (K - 1) * M
      J = IBUFF
      LIM = I + M
   80 I = I + 1
      IF (I .GT. LIM) GO TO 90
      J = J + 1
      DQ(I) = Q(J)
      GO TO 80
   90 CONTINUE
C
C IF K IS INPUT DO NOT COMPUTE INVERSE
C
  100 IF (IZK.EQ.2) GO TO 105
C*****
C COMPUTE THE INVERSE OF Z
C                        E
C*****
C
C THE 4TH ARGUMENT OF INVERD IS A DUMMY D.P. ARGUMENT WHILE 3 * M
C WORDS OF WORKING STORAGE ARE NEEDED FOR THE 8TH ARGUMENT OF SUBROUTINE
C INVERD.  SUBROUTINE INVERD WILL RETURN Z  INVERSE AT DQ(IZI+1)
C                                         E
C
      IBUFF = IUI + 2 * (MPN + MSQ) + 5
      II = IBUFF + 2 * M
C     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
      ISING = -1
      CALL INVERD (M,DQ(IZI+1),M,IQ(IBUFF+1),0,DET,ISING,IQ(II+1))
C
C ISING = 2 IMPLIES A SINGULAR Z
C                               E
C
      IF (ISING .EQ. 2) CALL MESAGE (-5,IDUMMY,NAME)
  105 CONTINUE
C
C READ IN THE M*N ELEMENTS OF THE M X N  S MATRIX IF N .GT. 0.
C THIS MATRIX IS SINGLE PRECISION AND ROW STORED.
C
      IF (ZONLY) GO TO 130
      IBUFF = MPN + 2*MSQ + 2*M*N +5
      CALL FREAD(IFGEI,Q(IBUFF+1),M*N,1)
C
C STORE THE S  MATRIX AT DQ(IS+1) MAKING S  DOUBLE PRECISION
C            E                            E
C
      LOW = IS + 1
      LIM = IS + M*N
      J = IBUFF
      DO 110 I=LOW,LIM
      J = J + 1
  110 DQ(I) = Q(J)
C                  -1
C COMPUTE K S  OR Z  S AND STORE AT DQ(IZIS+1)
C          E E     E  E
C
      IZIS = IS + M*N
      CALL GMMATD (DQ(IZI+1),M,M,0, DQ(IS+1),M,N,0, DQ(IZIS+1) )
C
C          T        T -1
C COMPUTE S K S OR S Z  S AND STORE AT DQ(ISTZIS+1)
C          E E E    E E  E
C
      ISTZIS = IZIS + M*N
      CALL GMMATD (DQ(IS+1),M,N,1, DQ(IZIS+1),M,N,0, DQ(ISTZIS+1) )
C
C              -1
C SET K S  OR Z  S  NEGATIVE
C      E E     E  E
C
      LOW = IZIS + 1
      LIM = IZIS + M*N
      DO 120 I=LOW,LIM
  120 DQ(I) = -DQ(I)
C*****
C AT THIS POINT ALL MATRICES HAVE BEEN COMPUTED
C*****
C
C INITIALIZE FOR OUTPUT ONTO THE FILE
C
  130 IZROW  = 1
      IZSCOL = 1
      ICOL = 1
      LIMJUI = IUI + M
      LIMJUD = IUI + MPN
      JUI  = IUI + 1
      JUD  = IUD + 1
C******
C BEGIN OUTPUT LOOP
C******
  140 ILOOP = 0
      IF (ZONLY) GO TO 150
      IF (IQ(JUI) - IQ(JUD)) 150,470,240
C
C AT THIS POINT, WRITE OUT COLUMN(S) CORRESPONDING TO THE UI SET.
C
  150 IF (IQ(JUI) - ICOL) 160,190,170
C
C A TRANSFER TO STATEMENT NO. 1115 WILL BE MADE IF THE MAXIMUM OF THE
C UD SET IS LESS THAN THE MINIMUM OF THE UI SET AND THE COLUMNS
C CORRESPONDING TO THE UD SET HAVE BEEN OUTPUT.
C
  160 IF (ILOOP .EQ. 1  .OR.  ZONLY) GO TO 480
      ILOOP = 1
      GO TO 240
C
C SINCE IQ(JUI) .GT. ICOL, IQ(JUI) - ICOL COLUMNS OF ZERO VECTORS MUST
C BE OUTPUT.
C
  170 LIM = IQ(JUI) - ICOL
      DO 180 I=1,LIM
      CALL BLDPK (2, IPREC, MCB(1), 0, 0)
  180 CALL BLDPKN(MCB(1),0,MCB)
C
C INITIALIZE FOR THE OUTPUT OF THE CURRENT COLUMN BY CALLING BLDPK
C
  190 CALL BLDPK (2, IPREC, MCB(1), 0, 0)
      DO 220 I=1,MPN
      IPPI = IP + I
      IF (IQ(IPPI) .GT. M) GO TO 200
C
C SINCE IQ(IPPI).LE.M,OUTPUT AN ELEMENT OF K OR Z INVERSE
C
      JROW = IZROW
      JCOL = IQ(IPPI)
      K = (JROW - 1) * M  +  JCOL  +  IZI
      GO TO 210
C
C HERE WE ARE DEALING WITH A MEMBER OF THE UD SET.  HENCE AN ELEMENT OF
C                -1
C THE -K S  OR -Z  S  MATRIX MUST BE OUTPUT
C       E E      E  E
C
  200 JROW = IZROW
      JCOL = IQ(IPPI) - M
      K = (JROW - 1) * N  +  JCOL  +  IZIS
C
C FILL ZBLPKI COMMON BLOCK
C
  210 KK = IQ(IPPI)
      INDEX = IQ(KK)
      DPWORD = DQ(K)
      IF (DPWORD .NE. 0.0D0) CALL ZBLPKI
  220 CONTINUE
C
C THE CURRENT COLUMN IS COMPLETE.  CALL BLDPKN TO WRAP UP.
C
      CALL BLDPKN(MCB(1),0,MCB)
      IZROW = IZROW + 1
      ICOL  = IQ(JUI) + 1
      JUI = JUI + 1
      IF (JUI .GT. LIMJUI) JUI = LIMJUI
  230 IF (IZROW .GT. M  .AND.  IZSCOL .GT. N) GO TO 320
      GO TO 140
C
C AT THIS POINT WRITE OUT A COLUMN(S) USING THE UD SET.
C
  240 IF (IQ(JUD) - ICOL) 250,280,260
C
C A TRANSFER TO STATEMENT NO. 1185 WILL BE MADE IF THE MAXIMUM OF THE
C UI SET IS LESS THAN THE MINIMUM OF THE UD SET AND THE COLUMNS
C CORRESPONDING TO THE UI SET HAVE BEEN OUTPUT.
C
  250 IF (ILOOP .EQ. 1) GO TO 490
      ILOOP = 1
      GO TO 150
C
C WRITE ZERO COLUMN(S).
C
  260 LIM = IQ(JUD) - ICOL
      DO 270 I=1,LIM
      CALL BLDPK (2, IPREC, MCB(1), 0, 0)
  270 CALL BLDPKN(MCB(1),0,MCB)
  280 CALL BLDPK (2, IPREC, MCB(1), 0, 0)
C
C OUTPUT A COLUMN WHOSE SIL NO. IS A MEMBER OF THE UD SET.
C
      DO 310 I=1,MPN
      IPPI = IP + I
      IF (IQ(IPPI) .GT. M) GO TO 290
C
C                                           -1
C SINCE IQ(IPPI).LE.M,AN ELEMENT OF -KS OR -Z  S MUST BE OUTPUT
C
      JROW = IQ(IPPI)
      JCOL = IZSCOL
      K = (JROW - 1) * N  +  JCOL  +  IZIS
      GO TO 300
C
C                       T         T -1
C OUTPUT AN ELEMENT OF S K S  OR S Z  S
C                       E E E     E E  E
C
  290 JROW = IQ(IPPI) - M
      JCOL = IZSCOL
      K = (JROW - 1) * N  + JCOL  +  ISTZIS
C
C SET UP PARAMETERS IN ZBLPKI COMMON BLOCK
C
  300 KK = IQ(IPPI)
      INDEX = IQ(KK)
      DPWORD = DQ(K)
      IF (DPWORD .NE. 0.0D0) CALL ZBLPKI
  310 CONTINUE
C
C WRAP UP THIS COLUMN.
C
      CALL BLDPKN(MCB(1),0,MCB)
      IZSCOL = IZSCOL + 1
      ICOL   = IQ(JUD)+ 1
      JUD    = JUD + 1
      IF (JUD .GT. LIMJUD) JUD = LIMJUD
      GO TO 230
C
C DETERMINE IF ZERO COLUMNS ARE TO BE OUTPUT.
C
  320 K = IUI + M
      L = IUD + N
      MAX = IQ(K)
      IF (IQ(L) .GT. MAX) MAX = IQ(L)
      LIM = MAX - LUSET
      IF (LIM) 330,350,500
C
C OUTPUT LIM ZERO COLUMNS
C
  330 LIM = IABS(LIM)
      DO 340 I = 1,LIM
      CALL BLDPK (2, IPREC, MCB(1), 0, 0)
  340 CALL BLDPKN(MCB(1),0,MCB)
  350 DO 360 I=1,7
  360 MCBCUR(I) = MCB(I)
      RETURN
C
C FATAL ERROR MESSAGES
C
  470 CALL MESAGE (-30,28,1)
  480 CALL MESAGE (-30,28,2)
  490 CALL MESAGE (-30,28,3)
  500 CALL MESAGE (-30,28,4)
      RETURN
      END
