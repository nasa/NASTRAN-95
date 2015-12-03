      SUBROUTINE FBSI (ZS,ZD)
C
C     GIVEN A LOWER TRIANGULAR FACTOR WITH DIAGONAL SUPERIMPOSED, AND
C     WRITTEN WITH TRAILING STRING DEFINITION WORDS, FBS WILL PERFORM
C     THE FORWARD-BACKWARD SUBSTITUTION NECESSARY TO SOLVE A LINEAR
C     SYSTEM OF EQUATIONS.
C
C     OPEN CORE IS DEFINED AS FOLLOWS
C
C     ZS(   1         ) - FIRST RIGHT HAND VECTOR ON FILE DBB
C                         (SIZE = NCOL*NWDS)
C                         NCOL = NUMBER OF COLUMNS (ROWS) IN LOWER  
C                                TRIANGULAR MATRIX
C                         NWDS = 1, IF MATRICES ARE REAL SINGLE
C                              = 2, IF MATRICES ARE REAL DOUBLE OR 
C                                COMPLEX SINGLE
C                              = 4, IF MATRICES ARE COMPLEX DOUBLE
C     ZS( NCOL*NWDS+1 ) - NEXT RIGHT HAND VECTOR
C         .
C         .               ( "NRHV" RIGHT HAND VECTORS WILL BE LOADED INTO 
C         .               MEMORY)
C         .
C     ZS( MTRIA       ) - MEMORY FOR STORAGE OF ALL OR PART OF THE LOWER
C                         TRIANGULAR MATRIX.  (SEE SUBROUTINE FBSRDM FOR
C                         FORMAT OF STORAGE OF MATRIX.)
C     ZS( BUF1        ) - BUFFER FOR FILE WITH RIGHT HAND VECTORS
C                         AND FOR SOLUTION VECTORS
C     ZS( BUF2        ) - BUFFER FOR FILE WITH TRIANGULAR MATRIX
C
      IMPLICIT INTEGER (A-Z)
      LOGICAL         IDENT
      INTEGER         SUBNAM(2) ,BLOCK(15),BEGN     ,END      ,INAME(2)
      REAL            ZS(1)     ,XS(4)    ,YS(4)
      DOUBLE  PRECISION          ZD(1)    ,XD       ,YD
      CHARACTER       UFM*23    ,UWM*25   ,UIM*29
      COMMON /LOGOUT/ LOUT
      COMMON /XMSSG / UFM       ,UWM      ,UIM
      COMMON /FBSX  / DBL(7)    ,DBU(7)   ,DBB(7)   ,DBX(7)   ,LCORE   ,
     1                PREC      ,SIGN     ,SCRX
      COMMON /FBSM  / NVEC      ,NVECSZ   ,NWDS     ,LASIND   ,IPOS(7)
      COMMON /SYSTEM/ SYSBUF    ,NOUT     ,SKIP(91) ,KSYS94
      COMMON /NAMES / RD        ,RDREW    ,WRT      ,WRTREW   ,REW     ,
     1                NOREW     ,EOFNRW   ,RSP      ,RDP      ,CSP     ,
     2                CDP
      COMMON /TYPE  / PRC(2)    ,WORDS(4) ,RLCMPX(4)
      COMMON /PACKX / ITYPE1    ,ITYPE2   ,I1       ,J1       ,INCR1
      COMMON /UNPAKX/ ITYPE3    ,I2       ,J2       ,INCR2
      COMMON /ZNTPKX/ XD(2)     ,IX       ,EOL
      COMMON /ZBLPKX/ YD(2)     ,IY
      EQUIVALENCE     (DBL(2),NCOL),   (DBB(5),TYPEB), (DBX(5),TYPEX),
     1                (XD(1),XS(1)), (YD(1),YS(1))
      DATA    SUBNAM/ 4HFBSI,4H    /  
      DATA    BEGN  / 4HBEGN/
      DATA    END   / 4HEND /
C
C     GENERAL INITIALIZATION
C
      BUF2     = LCORE - SYSBUF
      BUF1     = BUF2  - SYSBUF
      TYPEL    = DBL(5)            
      RCB      = RLCMPX( TYPEB )
      RCL      = RLCMPX( TYPEL )
      NWDS     = WORDS ( TYPEB )
      IF ( RCB .EQ. RCL .AND. TYPEL .GT. TYPEB ) NWDS = WORDS( TYPEL )
      NRHVWD   = NWDS * NCOL      
      NWDS     = WORDS ( TYPEL )
      NRHV     = DBB(2)
      IDENT    = .FALSE.
      IF (DBB(4) .EQ. 8) IDENT = .TRUE.
      IF (IDENT) NRHV = NCOL
      SWITCH   = 1
C 
C SET SWITCH AS FOLLOWS:
C  =1, IF LOWER TRIANGULAR MATRIX AND RIGHT HAND VECTORS ARE SAME TYPE
C  =2, LOWER TRIANGULAR MATRIX IS REAL SINGLE AND RIGHT HAND VECTOR IS 
C      COMPLEX
C  =3, LOWER TRIANGULAR MATRIX IS REAL DOUBLE AND RIGHT HAND VECTOR IS 
C      COMPLEX
C  (NOTE, IF SWITCH IS .NE. 1, THEN THE REAL AND IMAGINARY PARTS OF THE
C   THE RIGHT HAND VECTOR ARE TREATED AS TWO SEPARATE VECTORS.  I.E.,
C   THE REAL PART BECOMES ONE VECTOR AND THE IMAGINARY PART BECOMES A
C   SECOND VECTOR.)
C
      IF (TYPEL.EQ.RSP .AND. RCB.EQ.2) SWITCH = 2
      IF (TYPEL.EQ.RDP .AND. RCB.EQ.2) SWITCH = 3
      IF (SWITCH .EQ. 1 ) GO TO 90
      IF (SWITCH .EQ. 3 ) GO TO 70
      NRHVWD   = 2 * NCOL      
      GO TO 90
70    CONTINUE
      NRHVWD   = 4 * NCOL      
90    CONTINUE
      MTRIA    = NRHV * NRHVWD + 1
C
C ENSURE DOUBLE WORD BOUNDARY
C
      MTRIA    = ( MTRIA/2 ) * 2 + 1
      MEMAVL   = BUF1 - MTRIA - 2
      SUBNAM(2) = BEGN
      CALL CONMSG (SUBNAM,2,0)
      CALL FBSRDM ( DBL   , ZS(MTRIA), ZS(MTRIA), ZS(MTRIA) 
     &,             MEMAVL, ZS(BUF2 ), LASIND   , IPOS )
      CALL SSWTCH ( 47, L47 )
      CALL FNAME ( DBL, INAME )
      IF ( L47 .EQ. 0 ) GO TO 100
      WRITE ( LOUT, 9001 ) DBL(1), INAME, IPOS( 1 ), NCOL, LCORE, MEMAVL
      CALL FNAME ( DBB, INAME )
      WRITE ( LOUT, 9002 ) DBB(1), INAME, DBL, DBB
9001  FORMAT(4X
     &,      ' FORWARD BACKWARD SUBSTITUTION OF FILE ',I3,'   NAME=',2A4
     &,/,4X, ' LAST COLUMN OF TRIANGULAR MATRIX IN MEMORY        =',I8
     &,/,4X, ' TOTAL COLUMNS IN TRIANGULAR MATRIX                =',I8
     &,/,4X, ' TOTAL OPEN CORE AVAILABLE FOR USE                 =',I8
     &,/,4X, ' OPEN CORE AVAILABLE FOR TRIANGULAR MATRIX STORAGE =',I8 )
9002  FORMAT(4X
     &,      ' RIGHT HAND VECTOR FILE ',I3,'   NAME=',2A4
     &,/,4X, ' TRIANGULAR MATRIX TRAILER    =', 7I6 
     &,/,4X, ' RIGHT HAND VECTOR(S) TRAILER =', 7I6 )   
100   CONTINUE
      I2       = 1
      J2       = NCOL
      INCR2    = 1
      I1       = 1
      J1       = NCOL
      INCR1    = 1
      ITYPE1   = TYPEL
      ITYPE2   = TYPEX
      ITYPE3   = SIGN*TYPEL
      DBX(2)   = 0
      DBX(6)   = 0
      DBX(7)   = 0
      BLOCK(1) = DBL(1)
C
C     OPEN RIGHT HAND VECTORS FILE (DBB)
C
      LAST  = NRHV*NRHVWD
      IF ( IDENT ) GO TO 280
      CALL GOPEN ( DBB, ZS(BUF1), RDREW )
      GO TO ( 140, 180, 230 ), SWITCH
C
C     READ RIGHT HAND VECTORS INTO MEMORY
C
  140 DO 170 L = 1, LAST, NRHVWD
      CALL UNPACK ( *150, DBB, ZS(L) )
      GO TO 170
  150 LN = L + NRHVWD - 1
      DO 160 LL = L,LN
  160 ZS( LL ) = 0.
  170 CONTINUE
      GO TO 390
C
C     SPECIAL CASE - LOWER TRIANGULAR MATRIX IS RSP AND VECTORS ARE CSP
C
  180 LAST2 = LAST / 2
      L = 0
      DO 190 K = 1,LAST2
  190 ZD(K)    = 0.0D+0
      DO 220 K = 1, NRHV
      ICSPSG   = CSP*SIGN
      CALL INTPK ( *210, DBB, 0, ICSPSG, 0 )
  200 CALL ZNTPKI
      ZS(L+IX     ) = XS(1)
      ZS(L+IX+NCOL) = XS(2)
      IF ( EOL .EQ. 0 ) GO TO 200
  210 L = L + 2*NCOL
  220 CONTINUE
      GO TO 390
C
C     SPECIAL CASE - LOWER TRIANGULAR MATRIX IS RDP AND VECTORS ARE CDP
C
  230 LAST2 = LAST / 2
      L = 0
      DO 240 K = 1,LAST2
  240 ZD(K)    = 0.0D+0
      DO 270 K = 1, NRHV
      ICDPSG   = CDP*SIGN
      CALL INTPK ( *260, DBB, 0, ICDPSG, 0 )
  250 CALL ZNTPKI
      ZD(L+IX     ) = XD(1)
      ZD(L+IX+NCOL) = XD(2)
      IF ( EOL .EQ. 0 ) GO TO 250
  260 L        = L + 2*NCOL
  270 CONTINUE
      GO TO 390
C
C     SPECIAL CASE - GENERATE IDENTITY MATRIX
C
  280 LAST = NRHV * NRHVWD
      DO 290 K = 1,LAST
  290 ZD(K) = 0.0D+0
      L = 0
      GO TO ( 300, 320, 340, 360 ), TYPEL
  300 DO 310 K = 1, NRHV
      ZS(L+K)  = 1.0
  310 L        = L + NRHVWD
      GO TO 400
  320 DO 330 K = 1, NRHV
      ZD(L+K)  = 1.0D+0
  330 L        = L + NRHVWD
      GO TO 400
  340 DO 350 K    = 1, NRHV
      ZS(L+2*K-1) = 1.0
  350 L           = L + NRHVWD
      GO TO 400
  360 DO 370 K    = 1, NRHV
      ZD(L+2*K-1) = 1.0D+0
  370 L           = L + NRHVWD
      GO TO 400
C
C    CLOSE RIGHT HAND VECTORS FILE (DBB).
C    START FORWARD-BACKWARD SUBSTITUTION ON RIGHT HAND VECTORS
C
  390 CALL CLOSE  (DBB,REW)
  400 CONTINUE
      J      = TYPEL 
      NVEC   = NRHV
      NVECSZ = NCOL
      IF ( SWITCH .GT. 1 ) NVEC = NVEC*2
      GO TO ( 410, 420, 430, 440), J
  410 CONTINUE
      CALL FBSI1 ( BLOCK, ZS, ZS(MTRIA), ZS(MTRIA), ZS(BUF2) ) 
      GO TO 500
  420 CONTINUE  
      CALL FBSI2 ( BLOCK, ZS, ZS(MTRIA), ZS(MTRIA), ZS(BUF2) ) 
      GO TO 500
  430 CONTINUE
      CALL FBSI3 ( BLOCK, ZS, ZS(MTRIA), ZS(MTRIA), ZS(BUF2) ) 
      GO TO 500
  440 CONTINUE
      CALL FBSI4 ( BLOCK, ZS, ZS(MTRIA), ZS(MTRIA), ZS(BUF2) ) 
      GO TO 500
C
C     OPEN AND PACK SOLUTION VECTORS ONTO OUTPUT FILE (DBX)
C
  500 CALL GOPEN ( DBX, ZS(BUF1), WRTREW)
      GO TO ( 510, 530, 560 ), SWITCH
C
C     NORMAL CASE - CALL PACK
C
  510 DO 520 L = 1, LAST, NRHVWD
      CALL PACK ( ZS(L), DBX, DBX )
  520 CONTINUE
      GO TO 600
C
C     SPECIAL CASE - LOWER TRIANGULAR MATRIX IS RSP AND VECTORS ARE CSP
C
  530 L = 0
      DO 550 K = 1, NRHV
      CALL BLDPK ( CSP, TYPEX, DBX, 0, 0 )
      DO 540 I = 1, NCOL
      YS(1)    = ZS(L+I     )
      YS(2)    = ZS(L+I+NCOL)
      IY       = I
      CALL ZBLPKI
  540 CONTINUE
      CALL BLDPKN ( DBX, 0, DBX )
      L        = L + 2*NCOL
  550 CONTINUE
      GO TO 600
C
C     SPECIAL CASE - LOWER TRIANGULAR MATRIX IS RDP AND VECTORS ARE CDP
C
  560 L = 0
      DO 580 K = 1, NRHV
      CALL BLDPK ( CDP, TYPEX, DBX, 0, 0 )
      DO 570 I = 1,NCOL
      YD(1)    = ZD(L+I     )
      YD(2)    = ZD(L+I+NCOL)
      IY       = I
      CALL ZBLPKI
  570 CONTINUE
      CALL BLDPKN ( DBX, 0, DBX )
      L        = L + 2*NCOL
  580 CONTINUE
      GO TO 600
C
C     JOB DONE. CLOSE TRIANGULAR MATRIX AND SOLUTION FILE.
C
  600 CALL CLOSE ( DBX, REW )
      SUBNAM( 2 ) = END
      CALL CONMSG ( SUBNAM, 2, 0 )
      RETURN
      END
