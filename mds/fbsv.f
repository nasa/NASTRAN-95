      SUBROUTINE FBSV (*,ZS,ZD)        
C        
C     GIVEN A LOWER UNIT TRIANGULAR FACTOR WITH DIAGONAL SUPERIMPOSED        
C     AND WRITTEN WITH TRAILING STRING DEFINITION WORDS, FBSV WILL        
C     PERFORM THE FORWARD-BACKWARD SUBSTITUTION NECESSARY TO SOLVE A        
C     LINEAR SYSTEM OF EQUATIONS.        
C        
C     THIS FBSV.MDS ROUTINE IS ALMOST SAME AS FBS.MIS        
C     IT IS INTENDED TO BE USED FOR VAX, A VIRTUAL MEMORY MACHINE.        
C     FBSV.MDS DIFFERS FROM FBS.MIS IN        
C       1. THREE BUFFERS ARE USED        
C       2. OPEN CORE IS REDUCED. THE INTENTION HERE IS TO AVOID        
C          ACCESSIVE SYSTEM PAGING.        
C        
C     FBSV IS CALLED ONLY BY FBS ROUTINE.        
C     IT IS INTRODUCED INTO NASTRAN REPERTORY BY G.CHAN/UNISYS, 10/88        
C        
      LOGICAL IDENT        
      INTEGER DBL     ,DBU      ,DBB      ,DBX      ,PREC     ,SIGN    ,        
     1        SYSBUF  ,PRC      ,WORDS    ,RLCMPX   ,BUF1     ,BUF2    ,        
     2        BUF3    ,TYPEL    ,TYPEB    ,TYPEX    ,SUBNAM(2),BEGN    ,        
     3        END     ,BUF(2)   ,RC       ,EOL      ,RD       ,RDREW   ,        
     4        WRT     ,WRTREW   ,REW      ,EOFNRW   ,SWITCH   ,RSP     ,        
     5        RDP     ,CSP      ,CDP      ,BLOCK(15),HICORE   ,SYS34        
      REAL    ZS(1)   ,XS(4)    ,YS(4)        
      DOUBLE  PRECISION          ZD(1)    ,XD       ,YD        
      COMMON /FBSX  / DBL(7)    ,DBU(7)   ,DBB(7)   ,DBX(7)   ,LCORE   ,        
     1                PREC      ,SIGN        
      COMMON /SYSTEM/ SYSBUF    ,NOUT     ,SY1(28)  ,HICORE   ,SY2(2)  ,        
     1                SYS34        
      COMMON /NAMES / RD        ,RDREW    ,WRT      ,WRTREW   ,REW     ,        
     1                NOREW     ,EOFNRW   ,RSP      ,RDP      ,CSP     ,        
     2                CDP        
      COMMON /TYPE  / PRC(2)    ,WORDS(4) ,RLCMPX(4)        
      COMMON /PACKX / ITYPE1    ,ITYPE2   ,I1       ,J1       ,INCR1        
      COMMON /UNPAKX/ ITYPE3    ,I2       ,J2       ,INCR2        
      COMMON /ZNTPKX/ XD(2)     ,IX       ,EOL        
      COMMON /ZBLPKX/ YD(2)     ,IY        
      EQUIVALENCE    (DBL(5),TYPEL) , (DBB(5),TYPEB) , (DBX(5),TYPEX)  ,        
     1               (XD(1) ,XS(1)) , (DBL(2),NL   ) , (YD(1) ,YS(1))        
      DATA    SUBNAM/ 4HFBSV, 1H  / , BEGN/ 4HBEGN / , END/ 3HEND /        
C        
C        
C     CHECK OPEN CORE SITUATION. IF SYS34 (PAGECNTL) IS NOT ZERO, SET        
C     OPEN CORE SIZE TO SIZE SPECIFIED BY SYS34 (MINUS OVERHEAD OF 6000        
C     WORDS). TOO BIG A CORE SIZE MAY CAUSE EXCESSIVE PAGING AND SLOW        
C     DOWN FBS OPERATION        
C        
C        
      KORCHG = 0        
      IF (SYS34 .EQ. 0) GO TO 50        
      KORCHG = HICORE - SYS34 + 6000        
      IF (KORCHG .LE. 0) GO TO 80        
      LCORE  = LCORE- KORCHG        
      WRITE (NOUT,40) SYS34        
 40   FORMAT ('0*** SYSTEM INFORMATION MESSAGE - OPEN CORE FOR FBS IS',        
     1        ' SET TO',I7,' WORDS BY PAGECNTL OF /SYSTEM/',/)        
      GO TO 80        
 50   IF (HICORE .GT. 130001) WRITE (NOUT,60) HICORE        
 60   FORMAT ('0*** SYSTEM INFORMATION MESSAGE - PRESENT OPEN CORE =',        
     1        I7, /5X,'FURTHER INCREASE OF OPEN CORE MAY ACTUALLY ',        
     2        'SLOW DOWN FBS''S OPERATION.', //5X,        
     3        'SUGGESTION: TO OPTIMIZE CORE USAGE AND USER''S PROBLEM,',        
     4        ' AND TO MINIMIZE VAX''S PAGE FAULTS,', /5X,        
     4        'CHECK WORKING_SET PAGE LIMIT ASSIGNED TO USER, AND SET ',        
     6        'NASTRAN PAGECNTL WORD OF /SYSTEM/ TO MATCH, BUT NOT TO ',        
     7        'EXCEED, THE CURRENT SETTING',/)        
C        
C     GENERAL INITIALIZATION        
C        
 80   BUF3   = LCORE- SYSBUF        
      BUF2   = BUF3 - SYSBUF        
      BUF1   = BUF2 - SYSBUF        
      NNN    = BUF1 - 1        
      BUF(1) = SUBNAM(1)        
      BUF(2) = BEGN        
      CALL CONMSG (BUF,2,0)        
      NBRLOD = DBB(2)        
      RC     = RLCMPX(TYPEB)        
      I2     = 1        
      J2     = NL        
      INCR2  = 1        
      I1     = 1        
      J1     = NL        
      INCR1  = 1        
      ITYPE1 = TYPEL        
      ITYPE2 = TYPEX        
      ITYPE3 = SIGN*TYPEL        
      NWDS   = WORDS(TYPEL)*NL        
      NVECS  = NNN/NWDS        
      IF (NVECS .EQ. 0) CALL MESAGE(-8,NWDS-NNN,SUBNAM)        
      SWITCH = 1        
      IF (TYPEL.EQ.RSP .AND. RC.EQ.2) SWITCH = 2        
      IF (TYPEL.EQ.RDP .AND. RC.EQ.2) SWITCH = 3        
      IF (SWITCH .NE. 1) NVECS = NVECS/2        
      K1     = 1        
      BLOCK(1) = DBL(1)        
      DBX(2) = 0        
      DBX(6) = 0        
      DBX(7) = 0        
      IDENT  = .FALSE.        
      IF (DBB(4) .EQ. 8) IDENT = .TRUE.        
      NNNDBL = NNN/2        
      NTERMS = RLCMPX(TYPEL)*NL        
      IF (IDENT) NBRLOD = NL        
C        
C     OPEN OUTPUT FILE (DBX), LOAD VECTORS FILE (DBB), AND LOWER        
C     TRIANGULAR FACTOR FILE (DBL)        
C        
      CALL GOPEN (DBX,ZS(BUF3),WRTREW)        
      CALL GOPEN (DBL,ZS(BUF1),RDREW )        
      IF (.NOT.IDENT) CALL GOPEN (DBB,ZS(BUF2),RDREW)        
C        
C     CHECK TIMING AND ISSUE MESSAGE        
C        
      NPASS = (NBRLOD+NVECS-1)/NVECS        
      CALL SSWTCH (11,L11)        
      IF (NPASS .GE. 10) L11=1        
      IF (L11 .NE. 1) GO TO 140        
      CALL PAGE2 (-4)        
      WRITE (NOUT,100) TYPEL,NPASS        
  100 FORMAT ('0*** USER INFORMATION MESSAGE FROM FBS',I1,' - NO. OF ',        
     1        'PASSES NEEDED TO COMPLETE FBS OPERATION =',I5)        
      IF (NPASS .GT. 15) WRITE (NOUT,110)        
  110 FORMAT (5X,'INCREASE OF OPEN CORE MAY ACTUALLY SLOW DOWN FBS ',        
     1        'OPERATION.')        
      GO TO 140        
  120 IF (L11 .LT. 0) GO TO 150        
      CALL CPUTIM (J,T2,1)        
      T2 = T2-T1        
      IF (L11 .GT. 0) WRITE (NOUT,130) T2        
  130 FORMAT (5X,'TIME TO COMPLETE ONE PASS =',F10.4,' CPU SECONDS',//)        
      L11 = -1        
      CALL TMTOGO (J)        
      I = NPASS*T2        
      IF (J .LT. I) CALL MESAGE (-50,I,SUBNAM)        
      GO TO 150        
  140 CALL CPUTIM (J,T1,1)        
C        
C     COMPUTE EXTENT OF THIS PASS        
C        
  150 KN   = MIN0(K1+NVECS-1,NBRLOD)        
      LAST = 1 + (KN-K1)*NWDS        
      IF (IDENT) GO TO 190        
      GO TO (160,170,180), SWITCH        
C        
C     NORMAL CASE - FILL CORE WITH LOAD VECTORS        
C        
  160 DO 168 L=1,LAST,NWDS        
      CALL UNPACK (*162,DBB,ZS(L))        
      GO TO 168        
  162 LN = L+NWDS-1        
      DO 164 LL=L,LN        
  164 ZS(LL) = 0.        
  168 CONTINUE        
      GO TO 200        
C        
C     SPECIAL CASE - FACTOR IS RSP AND VECTORS ARE CSP        
C        
  170 LAST = 1 + 2*(KN-K1)*NWDS + NWDS        
      L = 0        
      DO 171 K=1,NNNDBL        
  171 ZD(K) = 0.        
      DO 178 K=K1,KN        
      ICSPSG = CSP*SIGN        
      CALL INTPK (*176,DBB,0,ICSPSG,0)        
  172 CALL ZNTPKI        
      ZS(L+IX   ) = XS(1)        
      ZS(L+IX+NL) = XS(2)        
      IF (EOL .EQ. 0) GO TO 172        
  176 L = L + 2*NL        
  178 CONTINUE        
      GO TO 200        
C        
C     SPECIAL CASE - FACTOR IS RDP AND VECTORS ARE CDP        
C        
  180 LAST = 1 + 2*(KN-K1)*NWDS + NWDS        
      L = 0        
      DO 181 K=1,NNNDBL        
  181 ZD(K) = 0.        
      DO 188 K=K1,KN        
      ICDPSG = CDP*SIGN        
      CALL INTPK (*186,DBB,0,ICDPSG,0)        
  182 CALL ZNTPKI        
      ZD(L+IX   ) = XD(1)        
      ZD(L+IX+NL) = XD(2)        
      IF (EOL .EQ. 0) GO TO 182        
  186 L = L + 2*NL        
  188 CONTINUE        
      GO TO 200        
C        
C     SPECIAL CASE - GENERATE IDENTITY MATRIX        
C        
  190 L = 0        
      DO 197 K=1,NNNDBL        
  197 ZD(K) = 0.        
      DO 198 K=K1,KN        
      GO TO (191,192,193,194), TYPEL        
  191 ZS(L+K) = 1.0        
      GO TO 196        
  192 ZD(L+K) = 1.0D0        
      GO TO 196        
  193 ZS(L+2*K-1) = 1.0        
      GO TO 196        
  194 ZD(L+2*K-1) = 1.0D0        
  196 L = L + NTERMS        
  198 CONTINUE        
C        
C     COMPUTE FORWARD-BACKWARD SUBSTITUTION ON LOAD VECTORS NOW IN CORE        
C        
  200 CALL REWIND (DBL)        
      CALL FWDREC (*270,DBL)        
C        
      GO TO (201,202,203,204), TYPEL        
C        
  201 CALL FBS1 (BLOCK,ZS,ZS(LAST),NWDS)        
      GO TO 210        
  202 CALL FBS2 (BLOCK,ZS,ZS(LAST),NWDS)        
      GO TO 210        
  203 CALL FBS3 (BLOCK,ZS,ZS(LAST),NWDS)        
      GO TO 210        
  204 CALL FBS4 (BLOCK,ZS,ZS(LAST),NWDS)        
C        
C     PACK SOLUTION VECTORS ONTO OUTPUT FILE        
C        
  210 GO TO (220,230,240), SWITCH        
C        
C     NORMAL CASE - CALL PACK        
C        
  220 DO 228 L=1,LAST,NWDS        
      CALL PACK (ZS(L),DBX,DBX)        
  228 CONTINUE        
      GO TO 250        
C        
C     SPECIAL CASE - FACTOR IS RSP AND VECTORS ARE CSP, CALL BLDPK        
C        
  230 L = 0        
      DO 238 K=K1,KN        
      CALL BLDPK (CSP,TYPEX,DBX,0,0)        
      DO 234 I=1,NL        
      YS(1) = ZS(L+I   )        
      YS(2) = ZS(L+I+NL)        
      IY = I        
      CALL ZBLPKI        
  234 CONTINUE        
      CALL BLDPKN (DBX,0,DBX)        
      L = L + 2*NL        
  238 CONTINUE        
      GO TO 250        
C        
C     SPECIAL CASE - FACTOR IS RDP AND VECTORS ARE CDP, CALL BLDPK        
C        
  240 L = 0        
      DO 248 K=K1,KN        
      CALL BLDPK (CDP,TYPEX,DBX,0,0)        
      DO 244 I=1,NL        
      YD(1) = ZD(L+I   )        
      YD(2) = ZD(L+I+NL)        
      IY = I        
      CALL ZBLPKI        
  244 CONTINUE        
      CALL BLDPKN (DBX,0,DBX)        
      L = L + 2*NL        
  248 CONTINUE        
C        
C     TEST FOR MORE PASSES        
C        
  250 IF (KN .EQ. NBRLOD) GO TO 300        
      K1   = KN + 1        
      GO TO 120        
C        
C     ERROR        
C        
  270 CALL MESAGE (-2,DBL,SUBNAM)        
C        
  300 IF (.NOT.IDENT) CALL CLOSE (DBB,REW)        
      CALL CLOSE (DBL,REW)        
      CALL CLOSE (DBX,REW)        
      BUF(2) = END        
      CALL CONMSG (BUF,2,0)        
      RETURN 1        
      END        
