      SUBROUTINE MMA ( ZI, ZR, ZD )
C
C     MMA PERFORMS THE MATRIX OPERATION
C       (+/-)A    * B (+/-)C = D   OR
C       (+/-)A(T) * B (+/-)C = D
C
C     USING METHODS 10, 11, 20, 21, 30, 31, 32, 40, 41
C
C
C  IN REGARDS TO THE METHODS BELOW, WHEN MULTIPLE COLUMNS OF A MATRIX
C  ARE STORED AND READ BY GETSTR, THEN THE MATRIX IS STORED IN MEMORY IN 
C  COMPACT FORM.  SEE SUBROUTINES 'MMARM1,2,3,4' FOR A DESCRIPTION OF 
C  THIS COMPACT FORM.  WHEN ONLY A SINGLE COLUMN OF A MATRIX IS STORED 
C  AND IT IS BEING READ BY GETSTR, IT IS STORED IN COMPACT FORM IN MEMORY.  
C  SEE SUBROUTINES 'MMARC1,2,3,4' FOR A DESCRIPTION OF THIS FORM.
C
C   ------------------------------------------------------------------------
C   METHOD     METHOD OF READING MATRIX    MULTIPLE COLUMNS OF MATRIX STORED
C                 A        B       C           A         B        D
C   ------------------------------------------------------------------------
C     10        UNPACK  UNPACK   UNPACK       YES        NO       NO
C     11        UNPACK  GETSTR   UNPACK       YES        NO       NO
C     20        UNPACK  UNPACK   UNPACK       NO         YES      YES
C     21        GETSTR  UNPACK   UNPACK       NO         YES      YES
C     30        GETSTR  UNPACK   UNPACK       YES        NO       NO
C     31        GETSTR  GETSTR   UNPACK       YES        NO       NO   
C     32        GETSTR  GETSTR   GETSTR       YES        NO       NO   
C     40        UNPACK  GETSTR   UNPACK       NO         YES      YES
C     41        GETSTR  GETSTR   UNPACK       NO         YES      YES
C   ------------------------------------------------------------------------
C
C   TO DETERMINE WHICH METHOD TO USE, THE FOLLOWING RATIONAL IS USED.      
C   
C   1.  DETERMINE THE METHOD FOR READING MATRICES "A" AND "B".  THIS IS 
C       DETERMINED BY EXAMINING THE FOLLOWING PERCENTAGE:
C 
C            (MEMORY TO CONTAIN ENTIRE MATRIX) 
C            ------------------------------------  = PERCENTAGE
C            (MEMORY TO CONTAIN COMPACTED MATRIX)
C
C       IF THE PERCENTAGE IS .GE. THE VARIABLE "TESTPCT", THEN UNPACK IS
C       USED.  OTHERWISE, GETSTR IS USED.
C
C            NiSTOR (i=A or B) = 1, CALL UNPACK TO READ MATRIX
C                              = 2, CALL GETSTR TO READ MATRIX
C
C    2. THE RESULTS OF THE FIRST TEST WILL NARROW THE OPTIONS TO TWO
C       DIFFERENT METHODS AS FOLLOWS:
C
C                                  CANDIDATE METHOD
C                10     11     20     21     30     31     32     40     41
C       NASTOR =  1      1      1      2      2      2      2      1      2
C       NBSTOR =  1      2      1      1      1      2      2      2      2
C
C          FOR NASTOR = 1 AND NBSTOR = 1, METHODS 10 AND 20 ARE CONSIDERED
C          FOR NASTOR = 1 AND NBSTOR = 2, METHODS 11 AND 40 ARE CONSIDERED
C          FOR NASTOR = 2 AND NBSTOR = 1, METHODS 21 AND 30 ARE CONSIDERED
C          FOR NASOTR = 2 AND NBSTOR = 2, METHODS 31,32 AND 41 ARE CONSIDERED
C            (NOTE, METHOD 32 IS ONLY AVAILABLE WITH "A" TRANSPOSED)
C
C    3. LASTLY, DETERMINE THE ESTIMATED NUMBER OF PASSES FOR EACH OF THE
C       TWO CANDIDATE METHODS.  THE METHOD WITH THE FEWER NUMBER OF PASSES
C       IS CHOSEN.
C        
C       MPASSii (ii=10,11,20,21,30,31,32,40,41) = ESTIMATED NUMBER OF PASSES
C                                                 FOR METHOD ii.
C
C       NiTOTAL (i=A,B,C) = MEMORY WORDS TO CONTAIN ENTIRE FULL MATRIX 
C       NiPACK  (i=A,B,C) = MEMORY WORDS TO CONTAIN ENTIRE MATRIX IN COMPACT 
C                           FORM.
C       NWDD              = NUMBER OF WORDS FOR EACH ELEMENT OF THE "D" MATRIX 
C
C
C     THE FOLLOWING SUBROUTINES ARE CALLED FOR THE DIFFERENT METHODS AND
C     MATRIX "D" TYPES (RS,RD,CS,CD).
C
C          METHODS  MAIN      OTHER SUBROUTINES DEPENDING ON TYPE
C                 SUBROUTINE    RS     RD     CS     CD  
C            10     MMA1      MMA101 MMA102 MMA103 MMA104 
C            11     MMA1      MMA111 MMA112 MMA113 MMA114
C            20     MMA2      MMA201 MMA202 MMA203 MMA204 
C            21     MMA2      MMA211 MMA212 MMA213 MMA214
C            30     MMA3      MMA301 MMA302 MMA303 MMA304 
C            31     MMA3      MMA311 MMA312 MMA313 MMA314  
C            32     MMA3      MMA321 MMA322 MMA323 MMA324 (TRANSPOSE ONLY)
C            40     MMA4      MMA401 MMA402 MMA403 MMA404 
C            41     MMA4      MMA411 MMA412 MMA413 MMA414
C ---------------------------------------------------------------------------      
      INTEGER           NAMEA(2) ,NAMEB(2)  ,NAMEC(2) , NAMED(2)
      INTEGER           ZI(2)    ,PRNTYP(4) ,MODULE(3),PREC1
      INTEGER           BLK1(15) ,BLK2(15)  ,EOL      ,EOR
      INTEGER           SIGNAB   ,SIGNC     ,T        ,SCRTCH
      INTEGER           FILEA    ,FILEB     ,FILEC    ,FILED
      INTEGER           SYSBUF   ,TYPEI     ,TYPEP    ,TYPEU
      INTEGER           ISAVE(9)
      REAL              ZR(2)
      DOUBLE PRECISION  ZD(2)    ,AD(2)   , DD(2)   
      CHARACTER         UFM*23   ,UWM*25    ,UIM*29
      CHARACTER*6       UPMETH(2)
      CHARACTER*2       CT
      INCLUDE           'MMACOM.COM'
      COMMON / NAMES  / RD       ,RDREW     ,WRT     ,WRTREW,CLSREW,CLS
      COMMON / XMSSG  / UFM      ,UWM       ,UIM
      COMMON / LOGOUT / LOUT
      COMMON / MPYADX / FILEA(7) ,FILEB(7)  ,FILEC(7)    
     1,                 FILED(7) ,NZ        ,T       ,SIGNAB,SIGNC,PREC1 
     2,                 SCRTCH   ,TIME
      COMMON / SYSTEM / KSYSTM(152)
      COMMON / PACKX  / TYPEI    ,TYPEP     ,IROW1P  ,IROWNP, INCRP
      COMMON / UNPAKX / TYPEU    ,IROWU     ,IROWNU  ,INCRU
      COMMON / ZBLPKX / D(4)     ,IROWBK
      COMMON / ZNTPKX / A(4)     ,IROWIN    ,EOL     ,EOR
      EQUIVALENCE      (AD(1)     ,A(1)  ) , (DD(1)     ,D(1) )
      EQUIVALENCE      (KSYSTM( 1),SYSBUF) , (KSYSTM( 2),NOUT ) 
     1,                (KSYSTM(58),KSYS58) , (KSYSTM(40),NBPW )
     2,                (KSYSTM(55),IPREC )
      EQUIVALENCE      (FILEA(2)  ,NAC   ) , (FILEA(3)  ,NAR   )
     1,                (FILEA(4)  ,NAFORM) , (FILEA(5)  ,NATYPE)
     2,                (FILEA(6)  ,NANZWD) , (FILEA(7)  ,NADENS)
      EQUIVALENCE      (FILEB(2)  ,NBC   ) , (FILEB(3)  ,NBR   )
     1,                (FILEB(4)  ,NBFORM) , (FILEB(5)  ,NBTYPE)
     2,                (FILEB(6)  ,NBNZWD) , (FILEB(7)  ,NBDENS)
      EQUIVALENCE      (FILEC(2)  ,NCC   ) , (FILEC(3)  ,NCR   )
     1,                (FILEC(4)  ,NCFORM) , (FILEC(5)  ,NCTYPE)
     2,                (FILEC(6)  ,NCNZWD) , (FILEC(7)  ,NCDENS)
      EQUIVALENCE      (FILED(2)  ,NDC   ) , (FILED(3)  ,NDR   )
     1,                (FILED(4)  ,NDFORM) , (FILED(5)  ,NDTYPE)
     2,                (FILED(6)  ,NDNZWD) , (FILED(7)  ,NDDENS)
C
      DATA    MODULE / 4HMMA , 2*4H    /
      DATA    JBEGN  /  4HBEGN/, JEND  / 3HEND/
      DATA    UPMETH / 'UNPACK', 'STRING' /
      DATA PRNTYP / 2HRS, 2HRD, 2HCS, 2HCD /
      DATA TESTPCT / .8 /
      ISAVE( 1 ) = TYPEI
      ISAVE( 2 ) = TYPEP
      ISAVE( 3 ) = IROW1P
      ISAVE( 4 ) = IROWNP
      ISAVE( 5 ) = INCRP
      ISAVE( 6 ) = TYPEU
      ISAVE( 7 ) = IROWU
      ISAVE( 8 ) = IROWNU
      ISAVE( 9 ) = INCRU
      CALL SSWTCH ( 19, L19 )
      MODULE( 3 ) = JBEGN
      CALL CONMSG ( MODULE, 3, 0 )
      NDR = NAR
      NDC = NBC
      IF ( T   .NE. 0   ) NDR = NAC
      IF ( NDFORM .NE. 0 ) GO TO 50
      NDFORM = 2
      IF ( NDR .EQ. NDC ) NDFORM = 1
50    CONTINUE
      IF ( FILEA( 6 ) .EQ. 0 .OR. FILEB( 6 ) .EQ. 0 ) GO TO 5000
      IF ( SIGNAB     .EQ. 0 ) GO TO 5000
      IF ( T        .NE. 0   ) GO TO 100
      IF ( NAC      .NE. NBR ) GO TO 7001
      IF ( FILEC(1) .EQ. 0   ) GO TO 200
      IF ( NAR      .NE. NCR ) GO TO 7001
      IF ( NBC      .NE. NCC ) GO TO 7001
      GO TO 200
100   CONTINUE
      IF ( NAR      .NE. NBR ) GO TO 7001
      IF ( FILEC(1) .EQ. 0   ) GO TO 200
      IF ( NAC      .NE. NCR ) GO TO 7001
      IF ( NBC      .NE. NCC ) GO TO 7001
200   CONTINUE
      NWDC = 0
      CALL DSSIZE ( FILEA, NCOLS, NATERMS, NASTRGS, NWDA )
      CALL DSSIZE ( FILEB, NCOLS, NBTERMS, NBSTRGS, NWDB )
      IF ( FILEC( 1 ) .NE. 0 ) 
     &  CALL DSSIZE ( FILEC, NCOLS, NCTERMS, NCSTRGS, NWDC )
      NWDD    = MAX0 ( NWDA, NWDB, NWDC )
      NDTYPE  = 2
      IF ( NWDD .EQ. 4 ) NDTYPE = 4
      IF ( NWDD .EQ. 1 ) NDTYPE = 1
      IF ( NDTYPE .EQ. 1 .OR. NDTYPE .EQ. 4 ) GO TO 250
      ITEST1  = MIN0 ( NATYPE, NBTYPE, NCTYPE )
      ITEST2  = MAX0 ( NATYPE, NBTYPE, NCTYPE )
      NDTYPE  = 3
      IF ( ITEST2 .EQ. 3 .AND. 
     &  ( NATYPE.EQ.2 .OR. NBTYPE.EQ.2 .OR. NCTYPE.EQ.2 ) ) NDTYPE = 4
      IF ( ITEST2 .LE. 2 ) NDTYPE = 2
250   CONTINUE
      NATOTAL = NAC * NAR * NWDD
      NBTOTAL = NBC * NBR * NWDD
      IF ( FILEC(1) .NE. 0 ) NCTOTAL = NCC * NCR * NWDD
      NDTOTAL = NDC * NDR * NWDD
      NAPACK  = 2*NAC + 2*NASTRGS + NATERMS*NWDD
      NBPACK  = 2*NBC + 2*NBSTRGS + NBTERMS*NWDD   
      IF ( FILEC(1) .NE. 0 ) NCPACK  = 2*NDC + 2*NCSTRGS + NCTERMS*NWDD   
      DENSTYA = ( NADENS*1.) / 10000.
      DENSTYB = ( NBDENS*1.) / 10000.
      IF ( FILEC(1) .NE. 0 ) DENSTYC = ( NCDENS*1.) / 10000.
      NASTOR  = 2
      NBSTOR  = 2
      NCSTOR  = 2
      X = NATOTAL
      Y = NAPACK
      PERCNTA = Y / X
      X = NBTOTAL
      Y = NBPACK
      PERCNTB = Y / X
      IF ( FILEC( 1 ) .EQ. 0 ) GO TO 300
      X = NCTOTAL
      Y = NCPACK
      PERCNTC = Y / X
300   CONTINUE
      IF ( PERCNTA .GE. TESTPCT ) NASTOR = 1
      IF ( PERCNTB .GE. TESTPCT ) NBSTOR = 1 
      IF ( FILEC(1) .NE. 0 .AND. PERCNTC .GE. TESTPCT ) NCSTOR = 1 
      MEMAVL  = NZ - 4*SYSBUF
      MPASS10 = (NATOTAL / ( MEMAVL - (NBR + NDR)*NWDD       ) ) + 1
      MPASS11 = (NATOTAL / ( MEMAVL - NDR*NWDD - (NBPACK/NBC)) ) + 1
      MPASS20 = ((NBTOTAL + NDTOTAL) / (MEMAVL - NAR*NWDD    ) ) + 1
      MPASS21 = ((NBTOTAL + NDTOTAL) / (MEMAVL - (NAPACK/NAC)) ) + 1
      MPASS30 = (NAPACK  / ( MEMAVL - (NBR + NDR)*NWDD       ) ) + 1
      MPASS31 = (NAPACK  / ( MEMAVL - NDR*NWDD - (NBPACK/NBC)) ) + 1 
      MPASS32 = (NAPACK  / ( MEMAVL - (NCPACK/NDC) - (NBPACK/NBC)) ) + 1 
      MPASS40 = ((NBPACK + NDTOTAL) / (MEMAVL - NAR*NWDD     ) ) + 1
      MPASS41 = ((NBPACK + NDTOTAL) / (MEMAVL - (NAPACK/NAC) ) ) + 1
      IF ( NASTOR .EQ. 1 .AND. NBSTOR .EQ. 1 ) GO TO 1000
      IF ( NASTOR .EQ. 2 .AND. NBSTOR .EQ. 1 ) GO TO 1100
      IF ( NASTOR .EQ. 1 .AND. NBSTOR .EQ. 2 ) GO TO 1200
      IF ( NASTOR .EQ. 2 .AND. NBSTOR .EQ. 2 ) GO TO 1300
1000  CONTINUE   
C---------USE UNPACK FOR MATRICES "A" AND "B"  (CHOOSE METHOD 10 OR 20)
      METHOD = 10
      IF ( MPASS10 .EQ. 1       ) GO TO 2000
      IF ( MPASS10 .LE. MPASS20 ) GO TO 2000
      METHOD = 20
      GO TO 2000
1100  CONTINUE 
C---------USE GETSTR FOR MATRIX "A"; UNPACK FOR MATRIX "B"  
C         (CHOOSE METHOD 21 OR 30)
      METHOD = 21
      IF ( MPASS21 .EQ. 1       ) GO TO 2000
      IF ( MPASS21 .LE. MPASS30 ) GO TO 2000
      METHOD = 30
      GO TO 2000
1200  CONTINUE    
C---------USE UNPACK FOR MATRIX "A"; GETSTR FOR MATRIX "B" 
C         (CHOOSE METHOD 11 OR 40)
      METHOD = 11
      IF ( MPASS11 .EQ. 1       ) GO TO 2000
      IF ( MPASS11 .LE. MPASS40 ) GO TO 2000
      METHOD = 40
      GO TO 2000
1300  CONTINUE
C---------USE GETSTR FOR MATRICES "A" AND "B" (CHOOSE METHOD 31, 32 OR 41)
      METHOD = 31
      IF ( MPASS31 .EQ. 1       ) GO TO 1310
      IF ( MPASS31 .LE. MPASS41 ) GO TO 1310
      METHOD = 41
      GO TO 2000
1310  CONTINUE
      IF ( NCSTOR .EQ. 2 .AND. T .NE. 0 ) METHOD = 32
2000  CONTINUE
      IF(L19.EQ.0) GO TO 3000
      CALL FNAME ( FILEA, NAMEA )
      CALL FNAME ( FILEB, NAMEB )
      CALL FNAME ( FILEC, NAMEC )
      CALL FNAME ( FILED, NAMED )
      WRITE( LOUT,2001, IOSTAT=IERR )
     &         NAMEA, NAR, NAC, NATERMS, DENSTYA, PRNTYP( NATYPE )
     &,        NAMEB, NBR, NBC, NBTERMS, DENSTYB, PRNTYP( NBTYPE )
2001  FORMAT(
     & '  /-----------------------------------------------------------/'
     &,/
     &,'  /     MATRIX      ROWS   COLS     TERMS   DENS    TYPE      /'
     &,/
     &,'  /-----------------------------------------------------------/'
     &,/
     &,'     A- ',2A4,I8,I7,I10,F7.4, 5X, A2
     &,/
     &,'     B- ',2A4,I8,I7,I10,F7.4, 5X, A2 )
      IF (FILEC(1) .EQ. 0) GO TO 2010
      WRITE( LOUT,2002, IOSTAT=IERR )
     &        NAMEC, NCR, NCC, NCTERMS, DENSTYC, PRNTYP( NCTYPE )
2002  FORMAT(
     & '     C- ',2A4,I8,I7,I10, F7.4, 5X, A2 )
2010  WRITE( LOUT, 2003 ) NAMED, NDR, NDC, PRNTYP(NDTYPE)
2003  FORMAT('     D- ',2A4, I8, I7, 10X, 7X,   5X, A2 )
      WRITE( LOUT, 2004 ) SIGNAB, SIGNC, NZ, KSYS58
2004  FORMAT('     SIGNAB =',I2,'  SIGNC =',I2,'  MEMORY =',I10
     &,'  SYSTEM(58)=',I3 )
      WRITE( LOUT, 2005 ) UPMETH( NASTOR ), NATOTAL, NAPACK
     &,                   UPMETH( NBSTOR ), NBTOTAL, NBPACK  
      IF ( FILEC( 1 ) .NE. 0 ) 
     &WRITE( LOUT, 20051) UPMETH( NCSTOR ), NCTOTAL, NCPACK  
      WRITE( LOUT, 20052) T, METHOD, PRNTYP( NDTYPE )
2005  FORMAT(
     & '  /-----------------------------------------------------------/'
     &,/
     &,'  /    READ METHOD   MEMORY (FULL MATRIX)    MEMORY (STRINGS) /'
     &,/
     &,'  /-----------------------------------------------------------/'
     &,/
     &,'     A-  ',A6,I21,I21 
     &,/
     &,'     B-  ',A6,I21,I21 
     &)
20051 FORMAT(
     & '     C-  ',A6,I21,I21 )
20052 FORMAT(
     & '     T =',I2,'    SUGGESTED METHOD =',I2
     &,'    "D" MATRIX TYPE:',1X,A2)
      WRITE( LOUT, 2006 ) MPASS10,MPASS11,MPASS20,MPASS21,MPASS30
     &,                           MPASS31,MPASS32,MPASS40,MPASS41
2006  FORMAT(
     & '  /-----------------------------------------------------------/'
     &,/
     & '  /       ESTIMATED NUMBER OF PASSES REQUIRED PER METHOD      /'
     &,/
     &,'  /         10   11   20   21   30   31   32   40   41        /'
     &,/
     &,'  /-----------------------------------------------------------/' 
     &,/
     &,'         ',9I5  
     &,/
     &,'  /-----------------------------------------------------------/' 
     & )
3000  CONTINUE
      IF ( FILED( 1 ) .LT. 0 ) GO TO 7777
      IF (  KSYS58 .NE. 0 .AND. 
     &     (KSYS58 .GE.10 .AND. KSYS58 .LE. 11) .OR.
     &     (KSYS58 .GE.20 .AND. KSYS58 .LE. 21) .OR.
     &     (KSYS58 .GE.30 .AND. KSYS58 .LE. 31) .OR.
     &     (KSYS58 .GE.40 .AND. KSYS58 .LE. 41) )  METHOD = KSYS58
      IF ( KSYS58 .EQ. 32 .AND. T .NE. 0 ) METHOD = KSYS58
      IF ( METHOD .EQ. 10 ) NBSTOR = 1
      IF ( METHOD .EQ. 11 ) NBSTOR = 2
      IF ( METHOD .EQ. 20 ) NASTOR = 1
      IF ( METHOD .EQ. 21 ) NASTOR = 2
      IF ( METHOD .EQ. 30 ) NBSTOR = 1
      IF ( METHOD .EQ. 31 ) NBSTOR = 2     
      IF ( METHOD .EQ. 32 ) NBSTOR = 2     
      IF ( METHOD .EQ. 40 ) NASTOR = 1
      IF ( METHOD .EQ. 41 ) NASTOR = 2
      IF ( METHOD .EQ. 10 .OR. METHOD .EQ. 11 )  
     &     CALL MMA1 ( ZI, ZR, ZD, ZR, ZD )   
      IF ( METHOD .EQ. 20 .OR. METHOD .EQ. 21 )   
     &     CALL MMA2 ( ZI, ZR, ZD, ZR, ZD )   
      IF ( METHOD .GE. 30 .AND. METHOD .LE. 32 ) 
     &     CALL MMA3 ( ZI, ZR, ZD, ZR, ZD )   
      IF ( METHOD .EQ. 40 .OR. METHOD .EQ. 41 ) 
     &     CALL MMA4 ( ZI, ZR, ZD, ZR, ZD )   
      CT = 'NT'
      IF ( T .NE. 0 ) CT = 'T '
      WRITE ( LOUT, 2007 ) METHOD, CT, IPASS
2007  FORMAT('   METHOD USED = ',I2,A2,'  ACTUAL NUMBER OF PASSES =',I4)
      GO TO 7777
C
C "A" AND "B" MATRICES ARE NULL, MOVE "C" TO "D" IF "C" EXISTS
C
5000  CONTINUE
      IF ( FILED( 1 ) .LT. 0 ) GO TO 7777
      NDTYPE = NCTYPE
      WRITE ( LOUT, 9002 ) 
9002  FORMAT('       MMA - NULL MATRIX PRODUCT')
      IBUF1 = NZ    - SYSBUF
      IBUF2 = IBUF1 - SYSBUF
      IF ( FILEC( 1 ) .EQ. 0 ) GO TO 5900
      IF ( SIGNC      .EQ. 0 ) GO TO 5900
      IF ( SIGNC      .LT. 0 ) GO TO 5500
C
C USE CPYSTR TO COPY "C" TO "D"
C
      BLK1( 1 ) = FILEC( 1 )
      BLK2( 1 ) = FILED( 1 )
      CALL GOPEN ( FILEC, ZR( IBUF1 ), RDREW )
      CALL GOPEN ( FILED, ZR( IBUF2 ), WRTREW)
      DO 5200 I = 1, NCC
      CALL CPYSTR ( BLK1, BLK2, 0, 0 )
5200  CONTINUE
      CALL CLOSE ( FILED, CLSREW )
      CALL CLOSE ( FILEC, CLSREW )
      FILED( 2 ) = FILEC( 2 )
      FILED( 3 ) = FILEC( 3 )
      FILED( 4 ) = FILEC( 4 )
      FILED( 5 ) = FILEC( 5 )
      FILED( 6 ) = FILEC( 6 )
      FILED( 7 ) = FILEC( 7 )
      GO TO 7777
C
C USE INTPK/BLDPK TO COPY C TO D BECAUSE SIGNS CONFLICT
C
5500  CONTINUE
      FILED( 2 ) = 0
      FILED( 6 ) = 0
      FILED( 7 ) = 0
      CALL GOPEN ( FILEC, ZR( IBUF1 ), RDREW )
      CALL GOPEN ( FILED, ZR( IBUF2 ), WRTREW)
      DO 5600 I = 1, NCC
      CALL BLDPK ( NDTYPE, NDTYPE, FILED, BLK1, 1 )
      CALL INTPK ( *5550 , FILEC , 0, NDTYPE*SIGNC, 0 )
5510  CALL ZNTPKI
      CALL BLDPKI ( A, IROWIN, FILED, BLK1 )
      IF ( EOL .EQ. 0 ) GO TO 5510
5550  CALL BLDPKN ( FILED, BLK1, FILED )
5600  CONTINUE
      FILED( 3 ) = FILEC( 3 )
      FILED( 4 ) = FILEC( 4 )
      FILED( 5 ) = FILEC( 5 )
      CALL CLOSE ( FILEC, CLSREW )
      CALL CLOSE ( FILED, CLSREW )
      GO TO 7777
C
C CREATE NULL MATRIX BECAUSE "C" MATRIX IS NULL
C
5900  CONTINUE
      NDR = 0
      NDC = 0
      CALL GOPEN ( FILED, ZR( IBUF1 ) , WRTREW )
      NDC = NBC
      NDR = NAR
      IF ( NAR .EQ. NBC ) NDR = NAC
      DD( 1 ) = 0.0D0
      INCRP   = 1
      IROW1P  = 1
      IROWNP  = 1
      TYPEI   = PREC1
      IF ( TYPEI .EQ. 0 ) TYPEI = 1
      TYPEP   = TYPEI
      NUMC    = NDC
      FILED( 2 ) = 0
      FILED( 3 ) = NDR
      FILED( 5 ) = IPREC
      FILED( 6 ) = 0
      FILED( 7 ) = 0
      DO 5950 I = 1, NUMC
      CALL PACK ( DD, FILED, FILED )
5950  CONTINUE
      CALL CLOSE ( FILED, CLSREW )
      GO TO 7777
C MATRICES ARE INCOMPATIBLE FOR MULTIPLICATION
7001  CONTINUE
      WRITE ( NOUT, 9001 ) UFM
9001  FORMAT( A23, 
     &      ' MATRICES FOR MULTIPLICATION HAVE INCOMPATIBLE SIZES',/)
      CALL FNAME ( FILEA, NAMEA )
      CALL FNAME ( FILEB, NAMEB )
      CALL FNAME ( FILEC, NAMEC )
      CALL FNAME ( FILED, NAMED )
      WRITE( NOUT,2001, IOSTAT=IERR )
     &         NAMEA, NAR, NAC, NATERMS, DENSTYA, PRNTYP( NATYPE )
     &,        NAMEB, NBR, NBC, NBTERMS, DENSTYB, PRNTYP( NBTYPE )
      IF ( FILEC(1) .EQ. 0) GO TO 7002
      WRITE( NOUT,2002, IOSTAT=IERR )
     &         NAMEC, NCR, NCC, NCTERMS, DENSTYC, PRNTYP( NCTYPE )
7002  CALL MESAGE ( -61, 0, 0 )
7777  CONTINUE
      MODULE( 3 ) = JEND
      CALL CONMSG ( MODULE, 3, 0 )
      TYPEI  = ISAVE( 1 )
      TYPEP  = ISAVE( 2 )
      IROW1P = ISAVE( 3 )
      IROWNP = ISAVE( 4 )
      INCRP  = ISAVE( 5 )
      TYPEU  = ISAVE( 6 )
      IROWU  = ISAVE( 7 )
      IROWNU = ISAVE( 8 )
      INCRU  = ISAVE( 9 )
      RETURN
      END
