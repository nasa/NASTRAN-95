      SUBROUTINE INPUT
C
C     INPUT I1,I2,I3,I4,I5/O1,O2,O3,O4,O5/C,N,-V1-/C,N,-V2-/C,N,-V3- $
C
C
      EXTERNAL        ORF
      LOGICAL         INOPEN(5)
      INTEGER         FILIN(5),FILOUT(5),FILE,FIL,HFIL(3,5),SPERLK,
     1                MODCOM(9),RDFLG,R,R1,R2,PARAMA,PARAMB,PARAMC,
     2                PARAM1,PARAMN,T(7),MNAM(2),EEE(3),IX(1),TWO,ORF,
     3                K(100),KT(20),I1T(20),J1T(20),I2T(20),J2T(20),
     4                CORD2C(2),KDN(2,20),KL(20),KNO(20),KDSORT(270),
     5                K1(100),K2(100),K3(70)
      REAL            LAMBDA,QK(100)
      CHARACTER*8     E1,E2,E3,CHR
      CHARACTER       UFM*23,UWM*25,UIM*29,SFM*25
      COMMON /XMSSG / UFM,UWM,UIM,SFM
      COMMON /MACHIN/ MACH
      COMMON /SYSTEM/ KSYSTM(100)
      COMMON /BLANK / PARAMA,PARAMB,PARAMC
      COMMON /TWO   / TWO(32)
      COMMON /ZZZZZZ/ X(1)
      COMMON /CONDAS/ PI,TWOPI,RADEG,DEGRA,S4PISQ
      EQUIVALENCE     (KSYSTM( 1),NBUF), (KSYSTM( 2),NOUT  ),
     1                (KSYSTM( 4),NIN ), (KSYSTM(12),NLINES),
     2                (KSYSTM(57),MODCOM(1)), (KSYSTM(95),SPERLK),
     3                (QK(1),K(1)), (X(1),IX(1)),
     4                (KDSORT(  1),K1(1)), (KDSORT(101),K2(1)),
     5                (KDSORT(201),K3(1))
C
      DATA    CORD2C / 2001,20        /
      DATA    MNAM   / 4HINPU, 4HT    /
      DATA    FILIN  / 101,102,103,104,105/
      DATA    FILOUT / 201,202,203,204,205/
      DATA    EEE    / 3*2147483647 /
      DATA    PARAM1 , PARAMN  / 1, 8 /
      DATA    E1,E2  / 'ENDDATA ', 'END DATA' /, E3 / 'ENDATA  ' /
      DATA    INOPEN / 5*.FALSE./
C
C     SORTSEQUENCE (INTERNALSEQUENCEID)
C
C                1    2    3    4    5    6    7    8    9    0
C
      DATA  K1/ 116, 115,   2, 211,  58,  57,  59,  61,  60,  62
     1        , 169, 215, 216, 221, 144, 214, 137, 104, 134, 105
     2        , 135, 106, 136, 165, 213, 114, 233, 113, 181, 189
     3        , 191,   3, 185, 186, 184, 188, 187, 177, 178, 176
     4        , 172, 182, 190, 170, 151, 161,  56,  70,  83,  85
     5        ,   4,  78,  79,  77,  82,  81,  68,  69,  67,  63
     6        ,  71,  84,  54,  55,  49,  50,  51,  52,  23,  24
     7        ,  25,  26,  36,  37,  38,  39, 122, 123,  80,  76
     8        ,  89, 148, 138, 121, 101,  98,  99,   5,   1, 127
     9        , 128, 145, 227, 228, 229, 230, 231, 235,   6,   7 /
C
C                1    2    3    4    5    6    7    8    9    0
C
      DATA  K2/   8, 129,   9,  75, 219,  10,  27,  28,  29,  30
     1        ,  31,  32,  33,  34,  35, 152, 153, 154,  92,  94
     2        , 183, 124,  91, 102, 110, 109, 140, 141, 142, 143
     3        , 205, 206, 223, 224, 210, 240, 241, 242, 243, 225
     4        , 226, 244,  96,  13, 203,  22, 150, 217, 139, 146
     5        , 220, 171, 209, 179, 234, 107, 133, 100, 155, 156
     6        , 157, 222, 158, 159, 160, 111, 245, 247, 249, 251
     7        , 253, 255, 257, 259, 261, 246, 248, 250,  16,  21
     8        , 149,  88,  90,  95, 164, 252, 254, 256, 130, 202
     9        , 232, 258, 260, 262, 199, 200, 201, 166, 167,  97 /
C
C                1    2    3    4    5    6    7    8    9    0
C
      DATA  K3/ 236, 237, 238, 239, 117, 112, 204, 180,  40,  41
     1        ,  42,  14,  17, 108,  11,  12,  74,  86,  44,  45
     2        ,  93, 103,  15,  18,  19,  20,  72,  73, 118, 119
     3        , 212,  43, 194, 125, 126, 162, 131, 132, 192, 193
     4        , 195, 196, 197, 198, 207, 208, 120, 147,  64, 173
     5        ,  46,  47,  48, 163, 168, 218,  87,  53,  65, 174
     6        ,  66, 175,   0,   0,   0,   0,   0,   0,   0,   0 /
C
C                1    2    3    4    5    6    7    8    9    0
C
      DATA  KL/ 901,1301,5501,5481,4501,2408, 501,5301,2801,3301,
     1         5401,5551,3001,5001,5008,5561,2001,   0,   0,   0/
      DATA  KT/   2,   2,   1,   1,   1,   1,   2,   1,   1,   1,
     1            1,   1,   2,   1,   1,   1,   1,   0,   0,   0/
C
      DATA I1T/   2,   2,   5,   5,   4,   3,   2,   5,   3,   4,
     1            5,   5,   3,   5,   5,   6,   3,   0,   0,   0/
      DATA J1T/   9,  13,   7,  10,  13,   8,   5,   5,  12,   1,
     1            6,   1,  14,   2,   2,  12,   4,   0,   0,   0/
C
      DATA I2T/   7,   7,   0,   0,   0,   0,   7,   0,   0,   0,
     1            0,   0,   7,   0,   0,   0,   0,   0,   0,   0/
      DATA J2T/   8,  10,   0,   0,   0,   0,   9,   0,   0,   0,
     1            0,   0,  16,   0,   0,   0,   0,   0,   0,   0/
C
      DATA KNO/  76,  68,  16,  12,   1, 180,  72,   4,  57,  52,
     1           25, 105,  48,  15, 258, 215,   9,   0,   0,   0/
C
      DATA       KDN(1, 1),KDN(2, 1) / 4HCELA , 4HS4   /,
     2           KDN(1, 2),KDN(2, 2) / 4HCMAS , 4HS4   /,
     3           KDN(1, 3),KDN(2, 3) / 4HSPC  , 4H     /,
     4           KDN(1, 4),KDN(2, 4) / 4HSPC1 , 4H     /,
     5           KDN(1, 5),KDN(2, 5) / 4HGRID , 4H     /,
     6           KDN(1, 6),KDN(2, 6) / 4HCBAR , 4H     /,
     7           KDN(1, 7),KDN(2, 7) / 4HCDAM , 4HP4   /,
     8           KDN(1, 8),KDN(2, 8) / 4HSEQG , 4HP    /,
     9           KDN(1, 9),KDN(2, 9) / 4HCQUA , 4HD1   /,
     O           KDN(1,10),KDN(2,10) / 4HCTRI , 4HA1   /,
     1           KDN(1,11),KDN(2,11) / 4HSLOA , 4HD    /,
     2           KDN(1,12),KDN(2,12) / 4HSPOI , 4HNT   /,
     3           KDN(1,13),KDN(2,13) / 4HCROD , 4H     /,
     4           KDN(1,14),KDN(2,14) / 4HOMIT , 4H     /,
     5           KDN(1,15),KDN(2,15) / 4HCNGR , 4HNT   /,
     6           KDN(1,16),KDN(2,16) / 4HASET , 4H     /,
     7           KDN(1,17),KDN(2,17) / 4HXXXX , 4H     /,
     8           KDN(1,18),KDN(2,18) / 4HXXXX , 4H     /,
     9           KDN(1,19),KDN(2,19) / 4HXXXX , 4H     /,
     O           KDN(1,20),KDN(2,20) / 4HXXXX , 4H     /
C
C
      LF(I,J,N) = I + N*(J-1)
C
      IF (PARAM1.LE.PARAMA .AND. PARAMA.LE.PARAMN) GO TO 20
      WRITE  (NOUT,10) UFM,PARAMA
   10 FORMAT (A23,' 1738, UTILITY MODULE INPUT FIRST PARAMETER VALUE - '
     1,       I20,' OUT OF RANGE')
      GO TO 9999
C
   20 KOR  = 10*NBUF + 1
      NKOR = KORSZ(X) - 10*NBUF
      IF (NKOR .LE. 0) CALL MESAGE (-8,NKOR,MNAM)
      CALL PAGE1
      NLINES = NLINES + 8
      WRITE  (NOUT,1)
    1 FORMAT (//20X,'* U T I L I T Y   M O D U L E   I N P U T *',///,
     1       20X,'INPUT DATA ECHO (DATA READ VIA FORTRAN, REMEMBER TO ',
     2       'RIGHT ADJUST)', ///20X,'*   1  **   2  **   3  **   4  ',
     3       '**   5  **   6  **   7  **   8  **   9  **  10  *' ,///)
      IOX = 0
      IOY = 0
      IF (MACH.LT.5 .OR. SPERLK.NE.0) GO TO 100
C
C     ON VAX-11/780 OR UNIX MACHINES, SEARCH FOR END OF BULK DATA DECK.
C
   60 READ (NIN,70,END=80) CHR
   70 FORMAT (A8)
      IF (CHR.EQ.E1 .OR. CHR.EQ.E2 .OR. CHR.EQ.E3) GO TO 100
      GO TO 60
C
C     ENDDATA CARD NOT FOUND
C
   80 WRITE  (NOUT,90) UFM
   90 FORMAT (A23,' - "ENDDATA" CARD NOT FOUND BY INPUT MODULE')
      CALL MESAGE (-37,0,MNAM)
C
  100 GO TO (1000,2000,3000,4000,5000,6000,7000,8000), PARAMA
C
C
C     PARAMA = 1 LAPLACE NETWORK
C
C     INPUT, ,,,,/,G2,,G4,/C,N,1/C,N,1 $         STATICS
C     INPUT, ,GEOM2,,GEOM4,/,G2,,G4,/C,N,1/C,N,1 $         STATICS
C     INPUT, ,,,,/,G2,,,/C,N,1/C,N,2 $          REAL-EIG W/O MASS COUPL
C     INPUT, ,GEOM2,,,/,G2,,,/C,N,1/C,N,2 $     REAL-EIG W/O MASS COUPL
C     INPUT, ,,,,/,G2,,,/C,N,1/C,N,3 $          REAL-EIG WITH MASS COUPL
C     INPUT, ,GEOM2,,,/,G2,,,/C,N,1/C,N,3 $     REAL-EIG WITH MASS COUPL
C
C
 1000 GO TO (1100,1200,1300), PARAMB
C
 1100 READ   (NIN,1110) N,ZK,U
 1110 FORMAT (I8,2E8.0)
      CALL PAGE2 (-1)
      WRITE  (NOUT,1111) N,ZK,U
 1111 FORMAT (21X,I8,1P,2E8.1,0P,F8.5)
C
      ASSIGN 1140 TO R2
      GO TO 1205
 1140 CONTINUE
C
C     G4
C
      IFIL = 4
      ASSIGN 1181 TO R
      GO TO 9100
C
C     SPC
C
 1181 IC = 3
      ASSIGN 1182 TO R
      GO TO 9200
 1182 K(1) = 1000 + N
      K(3) = 0
      K(4) = 0
      DO 1183 I = 2,N
      K(2) = I
 1183 CALL WRITE (FILE,K,4,0)
      DO 1184 I = 2,N
      K(2)  = LF(1,I,N1)
      QK(4) = U
      CALL WRITE (FILE,K,4,0)
      K(2)  = K(2) + N
      K(4)  = 0
 1184 CALL WRITE (FILE,K,4,0)
      DO 1185 I = 2,N
      K(2)  = N*N1 + I
 1185 CALL WRITE (FILE,K,4,0)
      ASSIGN 1190 TO R1
      GO TO 9600
 1190 RETURN
C
C
 1200 READ   (NIN,1201) N,ZK,ZM
 1201 FORMAT (I8,2E8.0)
      CALL PAGE2 (-1)
      WRITE (NOUT,1111) N,ZK,ZM
C
 1204 ASSIGN 1299 TO R2
C
 1205 N1  = N + 1
      NM1 = N - 1
C
C     G2
C
      IFIL = 2
      ASSIGN 1211 TO R
      GO TO 9100
C
C     CELAS4
C
 1211 IC = 1
      ASSIGN 1213 TO R
      GO TO 9200
 1213 QK(2) = ZK
      DO 1214 J = 2,N
      DO 1214 I = 1,N
      K(1) = LF(I,J,N1)
      K(3) = K(1)
      K(4) = K(3) + 1
      IF (PARAMB.NE.1 .AND. I.EQ.1) K(3) = 0
      IF (PARAMB.NE.1 .AND. I.EQ.N) K(4) = 0
 1214 CALL WRITE (FILE,K,4,0)
      DO 1215 J = 1,N
      DO 1215 I = 2,N
      K(3) = LF(I,J,N1)
      K(4) = K(3) + N1
      K(1) = K(3) + 1000000
      IF (PARAMB.NE.1 .AND. J.EQ.1) K(3) = 0
      IF (PARAMB.NE.1 .AND. J.EQ.N) K(4) = 0
 1215 CALL WRITE (FILE,K,4,0)
      ASSIGN 1216 TO R1
      GO TO 9650
 1216 IF (PARAMB .EQ. 1) GO TO 1240
C
C     CMASS4
C
      IC = 2
      ASSIGN 1218 TO R
      GO TO 9200
 1218 QK(2) = ZM
      K(4)  = 0
      DO 1219 J = 2,N
      DO 1219 I = 2,N
      K(3) = LF(I,J,N1)
      K(1) = K(3) + 2000000
 1219 CALL WRITE (FILE,K,4,0)
      IF (PARAMB .EQ. 3) GO TO 1230
 1220 ASSIGN 1240 TO R1
      GO TO 9650
C
 1230 QK(2) = -F*ZM
      DO 1232 J = 2,N
      DO 1232 I = 1,N
      K(3) = LF(I,J,N1)
      K(1) = K(3) + 3000000
      K(4) = K(3) + 1
      IF (I .EQ. 1) K(3) = 0
      IF (I .EQ. N) K(4) = 0
 1232 CALL WRITE (FILE,K,4,0)
      DO 1234 J = 1,N
      DO 1234 I = 2,N
      K(3) = LF(I,J,N1)
      K(4) = K(3) + N1
      K(1) = K(3) + 4000000
      IF (J .EQ. 1) K(3) = 0
      IF (J .EQ. N) K(4) = 0
 1234 CALL WRITE (FILE,K,4,0)
      QK(2) = -F*ZM/2.0
      DO 1236 J = 1,N
      DO 1236 I = 1,N
      K(3) = LF(I,J,N1)
      K(1) = K(3) + 5000000
      K(4) = K(3) + N1 + 1
      IF (I.EQ.1 .OR. J.EQ.1) K(3) = 0
      IF (I.EQ.N .OR. J.EQ.N) K(4) = 0
      IF (K(3).NE.0 .OR. K(4).NE.0) CALL WRITE (FILE,K,4,0)
 1236 CONTINUE
      DO 1238 J = 1,N
      DO 1238 I = 1,N
      K(3) = LF(I,J,N1)
      K(1) = K(3) + 6000000
      K(4) = K(3) + N1
      K(3) = K(3) + 1
      IF (I.EQ.N .OR. J.EQ.1) K(3) = 0
      IF (I.EQ.1 .OR. J.EQ.N) K(4) = 0
      IF (K(3).NE.0 .OR. K(4).NE.0) CALL WRITE (FILE,K,4,0)
 1238 CONTINUE
      GO TO 1220
 1240 IF (MODCOM(1) .NE. 0) GO TO 1295
C
C     DO NOT GENERATE CNGRNT DATA FOR N LESS THAN 3.
C
      IF (N .LT. 3) GO TO 1295
C
C     CNGRNT
C
      IC = 15
      ASSIGN 1245 TO R
      GO TO 9200
 1245 DO 1251 J = 2,N
      DO 1250 I = 1,N
      IF (PARAMB.NE.1 .AND. (I.EQ.1 .OR. I.EQ.N)) GO TO 1250
      K(1) = LF(I,J,N1)
      CALL WRITE (FILE,K,1,0)
 1250 CONTINUE
 1251 CONTINUE
      DO 1256 J = 1,N
      IF (PARAMB.NE.1 .AND. (J.EQ.1 .OR. J.EQ.N)) GO TO 1256
      DO 1255 I = 2,N
      K(1) = LF(I,J,N1) + 1000000
      CALL WRITE (FILE,K,1,0)
 1255 CONTINUE
 1256 CONTINUE
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
      IF (PARAMB .EQ. 1) GO TO 1259
      DO 1257 J = 2,N
      DO 1257 I = 1,N,NM1
      K(1) = LF(I,J,N1)
 1257 CALL WRITE (FILE,K,1,0)
      DO 1258 J = 1,N,NM1
      DO 1258 I = 2,N
      K(1) = LF(I,J,N1) + 1000000
 1258 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
 1259 CONTINUE
      IF (PARAMB .EQ. 1) GO TO 1285
      DO 1260 J = 2,N
      DO 1260 I = 2,N
      K(1) = LF(I,J,N1) + 2000000
 1260 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
      IF (PARAMB .EQ. 2) GO TO 1285
      DO 1265 J = 2,N
      DO 1265 I = 2,NM1
      K(1) = LF(I,J,N1) + 3000000
 1265 CALL WRITE (FILE,K,1,0)
      DO 1270 J = 2,NM1
      DO 1270 I = 2,N
      K(1) = LF(I,J,N1) + 4000000
 1270 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
      DO 1272 J = 2,N
      DO 1272 I = 1,N,NM1
      K(1) = LF(I,J,N1) + 3000000
 1272 CALL WRITE (FILE,K,1,0)
      DO 1273 J = 1,N,NM1
      DO 1273 I = 2,N
      K(1) = LF(I,J,N1) + 4000000
 1273 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
      DO 1280 L = 1,2
      DO 1275 J = 2,NM1
      DO 1275 I = 2,NM1
      K(1) = LF(I,J,N1) + 1000000*L + 4000000
 1275 CALL WRITE (FILE,K,1,0)
 1280 CONTINUE
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
      DO 1282 J = 1,N
      DO 1281 I = 1,N
      IF (I.NE.1 .AND. I.NE.N .AND. J.NE.1 .AND. J.NE.N) GO TO 1281
      IF (J.EQ.1 .AND. I.EQ.N .OR.  J.EQ.N .AND. I.EQ.1) GO TO 1281
      K(1) = LF(I,J,N1) + 5000000
      CALL WRITE (FILE,K,1,0)
 1281 CONTINUE
 1282 CONTINUE
      DO 1284 J = 1,N
      DO 1283 I = 1,N
      IF (I.NE.1 .AND. I.NE.N .AND. J.NE.1 .AND. J.NE.N) GO TO 1283
      IF (J.EQ.1 .AND. I.EQ.1 .OR.  J.EQ.N .AND. I.EQ.N) GO TO 1283
      K(1) = LF(I,J,N1) + 6000000
      CALL WRITE (FILE,K,1,0)
 1283 CONTINUE
 1284 CONTINUE
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
 1285 ASSIGN 1290 TO R1
      GO TO 9600
 1290 GO TO R2, (1140,1299)
 1295 ASSIGN 1290 TO R
      GO TO 9500
 1299 RETURN
C
 1300 READ   (NIN,1301) N,ZK,ZM,F
 1301 FORMAT (I8,3E8.0)
      CALL PAGE2 (-1)
      WRITE (NOUT,1111) N,ZK,ZM,F
      GO TO 1204
C
C
C     PARAMA = 2 RECTANGULAR FRAME MADE FROM BAR-S OR ROD-S
C
C     INPUT, ,,,,/G1,G2,,,/C,N,2/C,N,I/C,N,J $
C     INPUT GEOM1,GEOM2,,,/G1,G2,,,/C,N,2/C,N,I/C,N,J $
C            I=1  REGULAR BANDING
C            I=2  DOUBLE BANDING
C            I=3  ACTIVE COLUMN BANDING
C            I=4  REVERSE DOUBLE BANDING
C            J=0  BAR CONFIGURATION
C            J=1  ROD CONFIGURATION 1  (DIAGONALS IN BOTH DIRECTIONS)
C            J=2  ROD CONFIGURATION 2  (DIAGONALS IN LR TO UL DIRECTN)
C            J=3  ROD CONFIGURATION 3  (STATICALLY DETERMINATE)
C
C
 2000 READ   (NIN,2001) NX,NY,DX,DY,IP,LAMBDA
 2001 FORMAT (2I8,2E8.0,I8,E8.0)
      CALL PAGE2 (-1)
      WRITE  (NOUT,2002) NX,NY,DX,DY,IP,LAMBDA
 2002 FORMAT (21X,2I8,1P,2E8.1,I8,1P,2E8.1)
      NX1 = NX + 1
      NY1 = NY + 1
      ASSIGN 2295 TO R2
C
C     G1
C
 2005 IFIL = 1
      ASSIGN 2010 TO R
      GO TO 9100
C
C     GRID
C
 2010 IC = 5
      ASSIGN 2015 TO R
      GO TO 9200
 2015 QK(5) = 0.0
      K(2)  = 0
      K(6)  = 0
      K(8)  = 0
      SL  = SIN(DEGRA*LAMBDA)
      CL  = COS(DEGRA*LAMBDA)
      DDY = DY*CL
      JJ  = -1
      DO 2020 J = 1,NY1
      JJ  = JJ + 1
      IF (JJ .GT. IOY) JJ = 0
      QK(4) = DDY*FLOAT(J-1)
      XO  = FLOAT(J-1)*SL
      II  = -1
      DO 2020 I = 1,NX1
      II  = II + 1
      IF (II .GT. IOX) II = 0
      K(1)  = LF(I,J,NX1)
      QK(3) = DX*FLOAT(I-1) + XO
      IF (II.EQ.0 .OR. JJ.EQ.0) GO TO 2018
C
      K(7) = 6
C
      GO TO 2020
 2018 K(7) = IP
 2020 CALL WRITE (FILE,K,8,0)
      ASSIGN 2050 TO R1
      GO TO 9650
C
 2050 IF (PARAMB .EQ. 1) GO TO 2290
      IC = 8
      ASSIGN 2060 TO R
      GO TO 9200
 2060 GO TO (2290,2200,2300,2350), PARAMB
C
C     DOUBLE BANDING
C
 2200 IF (MOD(NY,2) .EQ. 0) GO TO 2212
      KK = NY1/2
      NN = 1
      GO TO 2214
 2212 KK = NY1/2 + 1
      NN = 2
 2214 IJ = NY1*NX1
      DO 2240 J = 1,IJ
      K(1) = J
      IW = MOD(J,NX1)
      IF (IW .EQ. 0) IW = NX1
      IL = (J-1)/NX1 + 1
      ILMK = IL - KK
      GO TO (2220,2230), NN
 2220 IF (ILMK) 2221,2221,2222
 2221 ILL = -2*ILMK + 1
      GO TO 2223
 2222 ILL  = 2*ILMK
 2223 K(2) = LF(IW,ILL,NX1)
      GO TO 2240
 2230 IF (ILMK) 2231,2232,2232
 2231 ILL  = -2*ILMK
      GO TO 2233
 2232 ILL  = 2*ILMK + 1
 2233 K(2) = LF(IW,ILL,NX1)
 2240 CALL WRITE (FILE,K,2,0)
C
 2270 ASSIGN 2290 TO R1
      GO TO 9650
 2290 GO TO R2, (2295,3010)
 2295 ASSIGN 2400 TO R
      GO TO 9500
C
C     ACTIVE COLUMNS BANDING
C
 2300 IJ  = NX1*NY1
      IF (MOD(NY,2) .EQ. 0) GO TO 2311
      KK  = IJ/2
      KKK = 0
      NN  = 1
      GO TO 2315
 2311 KK  = (NY/2+1)*NX1
      KKK = IJ - KK
      NN  = 2
 2315 DO 2340 J = 1,IJ
      K(1) = J
      GO TO (2320,2330), NN
 2320 IF (J-KK) 2321,2321,2322
 2321 K(2) = J + KK
      GO TO 2340
 2322 K(2) = J - KK
      GO TO 2340
 2330 IF (J-KKK) 2331,2331,2332
 2331 K(2) = J + KK
      GO TO 2340
 2332 K(2) = J - KKK
 2340 CALL WRITE (FILE,K,2,0)
      GO TO 2270
C
C     REVERSE DOUBLE BANDING
C
 2350 IJ = NX1*NY1
      IF (MOD(NX,2) .EQ. 0) GO TO 2360
      KK = NX1/2
      NN = 1
      GO TO 2370
 2360 KK = NX1/2 + 1
      NN = 2
 2370 DO 2390 J = 1,IJ
      K(1) = J
      IW   = MOD(J,NX1)
      IF (IW .EQ. 0) IW = NX1
      IL   = (J-1)/NX1 + 1
      IWMK = IW - KK
      GO TO (2380,2385), NN
 2380 IF (IWMK) 2381,2381,2382
 2381 IWW  = -2*IWMK + 1
      GO TO 2383
 2382 IWW  = 2*IWMK
 2383 K(2) = LF(IL,IWW,NY1)
      GO TO 2390
 2385 IF (IWMK) 2386,2387,2387
 2386 IWW = -2*IWMK
      GO TO 2388
 2387 IWW  = 2*IWMK + 1
 2388 K(2) = LF(IL,IWW,NY1)
 2390 CALL WRITE (FILE,K,2,0)
      GO TO 2270
C
C     G2
C
 2400 IFIL = 2
      ASSIGN 2410 TO R
      GO TO 9100
 2410 IF (PARAMC .NE. 0) GO TO 2700
C
C     CBAR
C
      IC = 6
      ASSIGN 2420 TO R
      GO TO 9200
 2420 K(2)  = 101
      QK(5) = 0.0
      QK(6) = 0.0
      QK(7) = 1.0
      K(8)  = 1
      DO 2430 I = 9,16
 2430 K(I) = 0
      DO 2450 J = 1,NY1
      DO 2450 I = 1,NX
      K(1) = LF(I,J,NX1)
      K(3) = K(1)
      K(4) = K(1) + 1
 2450 CALL WRITE (FILE,K,16,0)
      DO 2460 J = 1,NY
      DO 2460 I = 1,NX1
      K(3) = LF(I,J,NX1)
      K(4) = K(3) + NX1
      K(1) = K(3) + 1000000
 2460 CALL WRITE (FILE,K,16,0)
 2470 ASSIGN 2600 TO R1
      GO TO 9650
 2600 IF (MODCOM(1) .NE. 0) GO TO 2695
C
C     CNGRNT    (OUT OF SEQUENCE FOR CROD CASES)
C
      IC = 15
      ASSIGN 2610 TO R
      GO TO 9200
 2610 DO 2620 J = 1,NY1
      DO 2620 I = 1,NX
      K(1) = LF(I,J,NX1)
 2620 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
      DO 2640 J = 1,NY
      DO 2640 I = 1,NX1
      K(1) = LF(I,J,NX1) + 1000000
 2640 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
      IF (PARAMC .EQ. 0) GO TO 2680
      DO 2655 J = 1,NY
      DO 2653 I = 1,NX
      K(1) = LF(I,J,NX1)*2 + 1999999
      CALL WRITE (FILE,K,1,0)
      IF (PARAMC.EQ.3 .AND. J.GT.1) GO TO 2655
 2653 CONTINUE
 2655 CONTINUE
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
      IF (PARAMC .NE. 1) GO TO 2680
      DO 2670 J = 1,NY
      DO 2670 I = 1,NX
      K(1) = LF(I,J,NX1)*2 + 2000000
 2670 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
 2680 ASSIGN 2690 TO R1
      GO TO 9600
 2690 RETURN
 2695 ASSIGN 2690 TO R
      GO TO 9500
C
C     CROD
C
 2700 IC = 13
      ASSIGN 2710 TO R
      GO TO 9200
 2710 K(2) = 101
      DO 2720 J = 1,NY1
      DO 2720 I = 1,NX
      K(1) = LF(I,J,NX1)
      K(3) = K(1)
      K(4) = K(3) + 1
 2720 CALL WRITE (FILE,K,4,0)
      DO 2730 J = 1,NY
      DO 2730 I = 1,NX1
      K(3) = LF(I,J,NX1)
      K(4) = K(3) + NX1
      K(1) = K(3) + 1000000
 2730 CALL WRITE (FILE,K,4,0)
      DO 2750 J = 1,NY
      DO 2740 I = 1,NX
      K(3) = LF(I,J,NX1) + 1
      K(4) = K(3) + NX
      K(1) = 2*K(3) + 1999997
      CALL WRITE (FILE,K,4,0)
      IF (PARAMC.EQ.3 .AND. J.GT.1) GO TO 2750
      IF (PARAMC .NE. 1) GO TO 2740
      K(1) = K(1) + 1
      K(3) = K(3) - 1
      K(4) = K(4) + 1
      CALL WRITE (FILE,K,4,0)
 2740 CONTINUE
 2750 CONTINUE
      GO TO 2470
C
C
C     PARAMA = 3 RECTANGULAR PLATE MADE FROM QUAD1-S
C
C     INPUT, ,,,,/G1,G2,,G4,/C,N,3/C,N,I $
C     INPUT GEOM1,GEOM2,,GEOM4,/G1,G2,,G4,/C,N,3/C,N,I $
C              I=1  REGULAR BANDING
C              I=2  DOUBLE BANDING
C              I=3  ACTIVE COLUMN BANDING
C              I=4  REVERSE DOUBLE BANDING
C
C
 3000 READ  (NIN,3001) NX,NY,DX,DY,IP,LAMBDA,TH
 3001 FORMAT (2I8,2E8.0,I8,2E8.0)
      CALL PAGE2 (-2)
      WRITE  (NOUT,2002) NX,NY,DX,DY,IP,LAMBDA,TH
      READ   (NIN,3002) IY0,IX0,IYL,IXW,IOX,IOY
 3002 FORMAT (6I8)
      WRITE  (NOUT,3003) IY0,IX0,IYL,IXW,IOX,IOY
 3003 FORMAT (21X,6I8)
      NX1 = NX + 1
      NY1 = NY + 1
C
C     GRID
C
      ASSIGN 3010 TO R2
      GO TO 2005
 3010 ASSIGN 3020 TO R
      GO TO 9500
C
C     G2
C
 3020 IFIL = 2
      ASSIGN 3030 TO R
      GO TO 9100
C
C     CQUAD1
C
 3030 IF (PARAMA .EQ. 4) GO TO 4100
      IC = 9
      ASSIGN 3040 TO R
      GO TO 9200
 3040 K(2)  = 101
      QK(7) = TH
      DO 3060 J = 1,NY
      DO 3060 I = 1,NX
      K(1) = LF(I,J,NX1)
      K(3) = K(1)
      K(4) = K(3) + 1
      K(6) = K(1) + NX1
      K(5) = K(6) + 1
 3060 CALL WRITE (FILE,K,7,0)
      ASSIGN 3061 TO R1
      GO TO 9650
 3061 IF (MODCOM(1) .NE. 0) GO TO 3066
C
C     CNGRNT (OUT OF SEQUENCE)
C
      IC = 15
      ASSIGN 3062 TO R
      GO TO 9200
 3062 DO 3063 J = 1,NY
      DO 3063 I = 1,NX
      K(1) = LF(I,J,NX1)
 3063 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
 3065 ASSIGN 3070 TO R1
      GO TO 9600
 3066 ASSIGN 3070 TO R
      GO TO 9500
C
C     SPC-S AND OMIT-S
C
 3070 IF (IP+IY0+IX0+IYL+IXW+IOX+IOY .EQ. 0) GO TO 3090
C
C     G4
C
      IFIL = 4
      ASSIGN 3080 TO R
      GO TO 9100
C
C     SPC
C
 3071 IC = 3
      ASSIGN 3072 TO R
      GO TO 9200
 3072 K(1) = 1000*NX + NY
      K(4) = 0
      DO 3073 I = 1,NX1
      K(2) = I
      K(3) = IY0
      IF (I .EQ.    1) K(3) = IUNION(IY0,IX0)
      IF (I .EQ.  NX1) K(3) = IUNION(IY0,IXW)
      IF (K(3) .NE. 0) CALL WRITE (FILE,K,4,0)
 3073 CONTINUE
      DO 3074 I = 2,NY
      K(2) = LF(1,I,NX1)
      K(3) = IX0
      IF (K(3) .NE. 0) CALL WRITE (FILE,K,4,0)
      K(2) = K(2) + NX
      K(3) = IXW
      IF (K(3) .NE. 0) CALL WRITE (FILE,K,4,0)
 3074 CONTINUE
      K(2) = NX1*NY
      DO 3075 I = 1,NX1
      K(2) = K(2) + 1
      K(3) = IYL
      IF (I .EQ.    1) K(3) = IUNION(IYL,IX0)
      IF (I .EQ.  NX1) K(3) = IUNION(IYL,IXW)
      IF (K(3) .NE. 0) CALL WRITE (FILE,K,4,0)
 3075 CONTINUE
      ASSIGN 3089 TO R1
      GO TO 9650
 3080 IF (IOX+IOY .EQ. 0) GO TO 3071
C
C     OMIT
C
      IC = 14
      ASSIGN 3081 TO R
      GO TO 9200
 3081 DO 3082 I = 2,12,2
 3082 K(I) = I/2
      JJ = 0
      DO 3087 J = 2,NY
      JJ = JJ + 1
      IF (JJ .GT. IOY) GO TO 3086
      II = 0
      DO 3085 I = 2,NX
      II = II + 1
      IF (II .GT. IOX) GO TO 3084
      K(1) = LF(I,J,NX1)
      DO 3083  L = 3,11,2
 3083 K(L) = K(1)
C
      CALL WRITE (FILE,K,10,0)
C
      GO TO 3085
 3084 II = 0
 3085 CONTINUE
      GO TO 3087
 3086 JJ = 0
 3087 CONTINUE
      ASSIGN 3088 TO R1
      GO TO 9650
 3088 IF (IP+IY0+IX0+IYL+IXW .GT. 0) GO TO 3071
 3089 ASSIGN 3090 TO R
      GO TO 9500
 3090 RETURN
C
C
C     PARAMA = 4 RECTANGULAR PLATE MADE FROM TRIA1-S
C
C     INPUT, ,,,,/G1,G2,,G4,/C,N,4/C,N,I/C,N,J $
C     INPUT GEOM1,GEOM2,,GEOM4,/G1,G2,,G4,/C,N,4/C,N,I/C,N,J $
C            I=1  REGULAR BANDING
C            I=2  DOUBLE BANDING
C            I=3  ACTIVE COLUMN BANDING
C            I=4  REVERSE DOUBLE BANDING
C            J=1  TRIANGLE CONFIGURATION OPTION NO. 1    (LL TO UR)
C            J=2  TRIANGLE CONFIGURATION OPTION NO. 2    (LR TO UL)
C
C
 4000 GO TO 3000
C
C     CTRIA1
C
 4100 IC = 10
      ASSIGN 4200 TO R
      GO TO 9200
 4200 K(2)  = 101
      QK(6) = TH
      DO 4500 J = 1,NY
      DO 4500 I = 1,NX
      K(3) = LF(I,J,NX1)
      K(4) = K(3) + 1
      K(1) = 2*K(3) - 1
      GO TO (4300,4400), PARAMC
 4300 K(5) = K(4) + NX1
      CALL WRITE (FILE,K,6,0)
      K(1) = K(1) + 1
      K(4) = K(3) + NX1
      K(3) = K(5)
      K(5) = K(4) - NX1
      CALL WRITE (FILE,K,6,0)
      GO TO 4500
 4400 K(5) = K(3) + NX1
      CALL WRITE (FILE,K,6,0)
      K(1) = K(1) + 1
      K(3) = K(5) + 1
      K(4) = K(5)
      K(5) = K(3) - NX1
      CALL WRITE (FILE,K,6,0)
 4500 CONTINUE
      ASSIGN 4550 TO R1
      GO TO 9650
 4550 IF (MODCOM(1) .NE. 0) GO TO 3066
C
C     CNGRNT (OUT OF SEQUENCE)
C
      IC = 15
      ASSIGN 4600 TO R
      GO TO 9200
 4600 DO 4650 J = 1,NY
      DO 4650 I = 1,NX
      K(1) = LF(I,J,NX1)*2 - 1
 4650 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
      DO 4750 J = 1,NY
      DO 4750 I = 1,NX
      K(1) = LF(I,J,NX1)*2
 4750 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
      GO TO 3065
C
C
C     PARAMA = 5 N-SEGMENT STRING
C
C     INPUT, ,,,,/,G2,,,/C,N,5 $
C     INPUT, ,GEOM2,,,/,G2,,,/C,N,5 $
C
C
 5000 READ   (NIN,5010) N,XK1,XK2,XM,XB
 5010 FORMAT (I8,4E8.0)
      CALL PAGE2 (-1)
      WRITE  (NOUT,5011) N,XK1,XK2,XM,XB
 5011 FORMAT (21X,I8,1P,4E8.1)
      N1  = N + 1
      NM1 = N - 1
C
C     G2
C
      IFIL = 2
      ASSIGN 5100 TO R
      GO TO 9100
 5100 IF (XB .EQ. 0.0) GO TO 5140
C
C     CDAMP4
C
      IC = 7
      ASSIGN 5110 TO R
      GO TO 9200
 5110 QK(2) = XB
      K(4)  = 0
      DO 5120 I = 2,N
      K(1)  = I + 2000000
      K(3)  = I
 5120 CALL WRITE (FILE,K,4,0)
      ASSIGN 5140 TO R1
      GO TO 9650
C
C     CELAS4
C
 5140 IC = 1
      ASSIGN 5160 TO R
      GO TO 9200
 5160 QK(2) = XK1
      DO 5170 I = 1,N
      K(1) = I
      K(3) = I
      K(4) = I + 1
      IF (I .EQ. 1) K(3) = 0
      IF (I .EQ. N) K(4) = 0
 5170 CALL WRITE (FILE,K,4,0)
      IF (XK2 .NE. 0.0) GO TO 5190
 5175 ASSIGN 5210 TO R1
      GO TO 9650
C
 5190 QK(2) = XK2
      K(4)  = 0
      DO 5200 I = 2,N
      K(1)  = I + 3000000
      K(3)  = I
 5200 CALL WRITE (FILE,K,4,0)
      GO TO 5175
C
 5210 IF (XM .EQ. 0.0) GO TO 5260
C
C     CMASS4
C
      IC = 2
      ASSIGN 5220 TO R
      GO TO 9200
 5220 QK(2) = XM
      K(4)  = 0
      DO 5230 I = 2,N
      K(1)  = I + 1000000
      K(3)  = I
 5230 CALL WRITE (FILE,K,4,0)
      ASSIGN 5260 TO R1
      GO TO 9650
 5260 IF (MODCOM(1) .NE. 0) GO TO 5750
      IF (N .LE. 2) GO TO 5750
      IF (N.EQ.3 .AND. XM.EQ.0.0 .AND. XB.EQ.0.0 .AND. XK2.EQ.0.0)
     1   GO TO 5750
C
C     CNGRNT
C
      IC = 15
      ASSIGN 5300 TO R
      GO TO 9200
 5300 IF (N .EQ. 3) GO TO 5400
      DO 5320 I = 2,NM1
      K(1) = I
 5320 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
 5400 IF (XM .EQ. 0.0) GO TO 5500
      DO 5420 I = 2,N
      K(1) = I + 1000000
 5420 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
 5500 IF (XB .EQ. 0.0) GO TO 5600
      DO 5520 I = 2,N
      K(1) = I + 2000000
 5520 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
 5600 IF (XK2 .EQ. 0.0) GO TO 5700
      DO 5620 I = 2,N
      K(1) = I + 3000000
 5620 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
 5700 ASSIGN 5900 TO R1
      GO TO 9600
 5750 ASSIGN 5900 TO R
      GO TO 9500
 5900 RETURN
C
C
C     PARAMA = 6 N-CELL BAR
C
C     INPUT, ,,,,/G1,G2,,G4,/C,N,6 $
C     INPUT GEOM1,GEOM2,,GEOM4,/G1,G2,,G4,/C,N,6 $
C
C
 6000 READ   (NIN,6010) N,XL,IP,IFLG,IG0,M,IOX
 6010 FORMAT (I8,E8.0,5I8)
      CALL PAGE2 (-1)
      WRITE  (NOUT,6011) N,XL,IP,IFLG,IG0,M,IOX
 6011 FORMAT (21X,I8,1P,E8.1,5I8)
      N1 = N + 1
C
C     G1
C
      IFIL = 1
      ASSIGN 6100 TO R
      GO TO 9100
C
C     GRID
C
 6100 IC = 5
      ASSIGN 6200 TO R
      GO TO 9200
 6200 K(2)  = 0
      QK(4) = 0.0
      QK(5) = 0.0
      K(6)  = 0
      K(8)  = 0
      II    = 0
      DO 6300 I = 1,N1
      II    = II + 1
      K(1)  = I
      QK(3) = XL*FLOAT(I-1)/FLOAT(N)
      IF (I.EQ.1 .OR. II.GT.IOX) GO TO 6280
      K(7) = 0
      GO TO 6300
 6280 K(7) = IP
      II   = 0
 6300 CALL WRITE (FILE,K,8,0)
      ASSIGN 6600 TO R1
      GO TO 9600
C
C     G2
C
 6600 IFIL = 2
      ASSIGN 6610 TO R
      GO TO 9100
C
C     CBAR
C
 6610 IC = 6
      ASSIGN 6620 TO R
      GO TO 9200
 6620 K(2) = 101
      K(8) = IFLG
      K(5) = IG0
      K(6) = 0
      K(7) = 0
      IF (IFLG .EQ. 2) GO TO 6635
      READ   (NIN,6630) X1,X2,X3
 6630 FORMAT (3E8.0)
      CALL PAGE2 (-1)
      WRITE  (NOUT,6631) X1,X2,X3
 6631 FORMAT (21X,1P,3E8.1)
      QK(5) = X1
      QK(6) = X2
      QK(7) = X3
      GO TO 6640
 6635 GO TO 9907
 6640 DO 6645 I = 9,16
 6645 K(I) = 0
      DO 6650 I = 1,N
      K(1) = I
      K(3) = I
      K(4) = I + 1
 6650 CALL WRITE (FILE,K,16,0)
      IF (M.LE.0 .OR. M.GT.N-1) GO TO 6670
      K(2) = 102
      K(3) = 2
      DO 6660 I = 1,M
      K(1) = N + I
      K(4) = N - I + 2
 6660 CALL WRITE (FILE,K,16,0)
 6670 ASSIGN 6680 TO R1
      GO TO 9650
 6680 IF (MODCOM(1) .NE. 0) GO TO 6694
C
C     CNGRNT
C
      IC = 15
      ASSIGN 6685 TO R
      GO TO 9200
 6685 DO 6690 I = 1,N
      K(1) = I
 6690 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
      ASSIGN 6695 TO R1
      GO TO 9600
 6694 ASSIGN 6695 TO R
      GO TO 9500
 6695 IF (IOX .EQ. 0) RETURN
C
C     G4
C
      IFIL = 4
      ASSIGN 6700 TO R
      GO TO 9100
C
C     OMIT
C
 6700 IC = 14
      ASSIGN 6710 TO R
      GO TO 9200
 6710 DO 6712 I = 2,12,2
 6712 K(I) = I/2
      II = 0
      DO 6720 I = 2,N
      II = II + 1
      IF (II .GT. IOX) GO TO 6716
      K(1) = I
      DO 6714 L = 3,11,2
 6714 K(L) = K(1)
      CALL WRITE (FILE,K,12,0)
      GO TO 6720
 6716 II = 0
 6720 CONTINUE
      ASSIGN 6730 TO R1
      GO TO 9600
 6730 RETURN
C
C
C     PARAMA = 7 FULL MATRIX AND OPTIONAL UNIT LOAD
C
C     INPUT, ,,,,/,G2,G3,,G5/C,N,7 $
C     INPUT, ,GEOM2,GEOM3,,/,G2,G3,,G5/C,N,7 $
C
C
 7000 READ   (NIN,7001) N,NSLOAD
 7001 FORMAT (2I8)
      CALL PAGE2 (-1)
      WRITE  (NOUT,7002) N,NSLOAD
 7002 FORMAT (21X,2I8)
      N1 = N + 1
C
C     G2
C
      IFIL = 2
      ASSIGN 7010 TO R
      GO TO 9100
C
C     CELAS4
C
 7010 IC = 1
      ASSIGN 7011 TO R
      GO TO 9200
 7011 QK(2) = 1.0
      II = 0
      DO 7020 I = 1,N
      IF (I .GT. 1) II = II + N1 - I
      DO 7020 J = I,N
      K(1) = II + J
      K(3) = I
      K(4) = J
      IF (I .EQ. J) K(4) = 0
 7020 CALL WRITE (FILE,K,4,0)
      ASSIGN 7030 TO R1
      GO TO 9650
 7030 IF (MODCOM(1) .NE. 0) GO TO 7036
C
C     DO NOT GENERATE CNGRNT DATA FOR N LESS THAN 3.
C
      IF (N .LT. 3) GO TO 7036
C
C     CNGRNT
C
      IC = 15
      ASSIGN 7032 TO R
      GO TO 9200
 7032 II = 0
      DO 7033 I = 1,N
      IF (I .GT. 1) II = II + N1 - I
      K(1) = II + I
 7033 CALL WRITE (FILE,K,1,0)
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
      II = 0
      DO 7035 I = 1,N
      IF (I .GT. 1) II = II + N1 - I
      DO 7034 J = I,N
      IF (J .EQ. I) GO TO 7034
      K(1) = II + J
      CALL WRITE (FILE,K,1,0)
 7034 CONTINUE
 7035 CONTINUE
      K(1) = -1
      CALL WRITE (FILE,K,1,0)
      ASSIGN 7037 TO R1
      GO TO 9600
 7036 ASSIGN 7037 TO R
      GO TO 9500
 7037 IF (NSLOAD .EQ. 0) GO TO 7070
C
C     G3
C
      IFIL = 3
      ASSIGN 7040 TO R
      GO TO 9100
C
C     SLOAD
C
 7040 IC = 11
      ASSIGN 7050 TO R
      GO TO 9200
 7050 K(1)  = N
      QK(3) = 1.0
      DO 7060 I = 1,N
      K(2)  = I
 7060 CALL WRITE (FILE,K,3,0)
      ASSIGN 7070 TO R1
      GO TO 9600
C
C     G5
C
 7070 IFIL = 5
      ASSIGN 7080 TO R
      GO TO 9100
C
C     SPOINT
C
 7080 IC = 12
      ASSIGN 7090 TO R
      GO TO 9200
 7090 DO 7100 I = 1,N
 7100 CALL WRITE (FILE,I,1,0)
      ASSIGN 7110 TO R1
      GO TO 9600
 7110 NE = N*N1/2
      CALL PAGE2 (-2)
      WRITE  (NOUT,7201) N,NE
 7201 FORMAT ('0*INPUT* FULL MATRIX OF ORDER',I9,'   GENERATED WITH',
     1        I9,'  ELEMENTS')
      IF (NSLOAD .EQ. 0) GO TO 7203
      CALL PAGE2 (-2)
      WRITE  (NOUT,7202) N
 7202 FORMAT ('0*INPUT* LOAD SET',I9,'  GENERATED')
 7203 RETURN
C
C
C     PARAMA = 8 N-SPOKE WHEEL
C
C     INPUT, ,,,,/G1,G2,,,/C,N,8 $
C     INPUT GEOM1,GEOM2,,,/G1,G2,,,/C,N,8 $
C
C
 8000 READ   (NIN,8010) N,XL,IP,IFLG,IG0,ICEN
 8010 FORMAT (I8,E8.0,4I8)
      CALL PAGE2 (-1)
      WRITE  (NOUT,8011) N,XL,IP,IFLG,IG0,ICEN
 8011 FORMAT (21X,I8,1P,E8.1,4I8)
      N1 = N + 1
C
C     G1
C
      IFIL = 1
      ASSIGN 8100 TO R
      GO TO 9100
8100  CONTINUE
C
C     LOCATE AND COPY CORD2C CARD FROM THE FIRST INPUT FILE
C
      IBUF = (IFIL+4)*NBUF + 1
      CALL PRELOC (*9908,X(IBUF),FILIN(IFIL))
      CALL LOCATE (*9909,X(IBUF),CORD2C,QK(3))
      CALL READ (*9908,*8120,FILIN(IFIL),QK(4),13,0,IFLAG)
      CALL CLOSE (FILIN(IFIL),1)
      INOPEN(IFIL) = .FALSE.
8120  CONTINUE
      IC = 17
      ASSIGN 8200 TO R
      GO TO 9200
8200  CONTINUE
      CALL WRITE (FILE,QK(4),13,0)
      ASSIGN 8250 TO R1
      GO TO 9650
C
C     GRID
C
8250  CONTINUE
      IC = 5
      ASSIGN 8260 TO R
      GO TO 9200
8260  CONTINUE
      K(2)  = 2
C     QK(2) = QK(5)    THIS WILL ASSIGN REFERENCE NUMBER ON CORD2C CARD
C                      TO THE GRID POINTS
C
      IF (N.GT.0 .AND. N.LT.256) GO TO 8050
      CALL PAGE2 (-2)
      WRITE  (NOUT,8030) UWM
 8030 FORMAT (A25,' 2369, WHEEL MUST HAVE FEWER THAN 256 SPOKES. ',
     1       'INPUT MODULE RESETTING TO 255')
      N = 255
 8050 N1 = N + 1
      QK(3) = XL
      QK(5) = 0.0
      K(6)  = 2
      K(8)  = 0
      K(7)  = IP
      DO 8300 I = 1,N
      K(1)  = I
      QK(4) = 360.0/FLOAT(N)*FLOAT(I-1)
 8300 CALL WRITE (FILE,K,8,0)
      K(1)  = N1
      K(2)  = 0
      QK(3) = 0.0
      QK(4) = 0.0
      K(6)  = 0
      IF (ICEN .NE. 0) K(7) = ICEN
      CALL WRITE (FILE,K,8,0)
      ASSIGN 8600 TO R1
      GO TO 9600
C
C     G2
C
 8600 IFIL = 2
      ASSIGN 8610 TO R
      GO TO 9100
C
C     CBAR
C
 8610 IC = 6
      ASSIGN 8620 TO R
      GO TO 9200
 8620 K(2) = 101
      K(8) = IFLG
      K(5) = IG0
      K(6) = 0
      K(7) = 0
      IF (IFLG .EQ. 2) GO TO 8635
      READ   (NIN,8630) X1,X2,X3
 8630 FORMAT (3E8.0)
      CALL PAGE2 (-1)
      WRITE  (NOUT,8631) X1,X2,X3
 8631 FORMAT (21X,1P,3E8.1)
      QK(5) = X1
      QK(6) = X2
      QK(7) = X3
      GO TO 8640
 8635 GO TO 9907
 8640 DO 8645 I = 9,16
 8645 K(I) = 0
      DO 8650 I = 1,N
      K(1) = I
      K(3) = I
      K(4) = I + 1
      IF (K(4) .EQ. N1) K(4) = 1
 8650 CALL WRITE (FILE,K,16,0)
      K(4) = N1
      DO 8655 I = 1,N
      K(1) = N + I
      K(3) = I
 8655 CALL WRITE (FILE,K,16,0)
      ASSIGN 8950 TO R1
      GO TO 9650
 8950 ASSIGN 8900 TO R
      GO TO 9500
 8900 RETURN
C
C
C     UTILITY I/O ROUTINES
C
C
 9100 FILE = FILOUT(IFIL)
      IBUF = (IFIL-1)*NBUF + 1
      CALL GOPEN (FILE,X(IBUF),1)
      T(1) = FILE
      DO 9110 J = 2,7
 9110 T(J) = 0
      FIL  = FILIN(IFIL)
      IBUF = (IFIL+4)*NBUF + 1
      IF (PARAMA - 8) 9115,9190,9115
 9115 CONTINUE
      CALL OPEN (*9130,FIL,X(IBUF),0)
      INOPEN(IFIL) = .TRUE.
      T(1) = FIL
      CALL RDTRL (T)
      T(1) = FILE
      CALL SKPREC (FIL,1)
      CALL FREAD (FIL,HFIL(1,IFIL),3,0)
      DO 9120 J = 1,3
      IF (HFIL(J,IFIL) .NE. EEE(J)) GO TO 9190
 9120 CONTINUE
      GO TO 9904
 9130 INOPEN(IFIL) = .FALSE.
 9190 GO TO R, (1181,1211,2010,2410,3030,3080,5100,6100,6610,6700,
     1          7010,7040,7080,8100,8610)
C
C
 9200 IF (INOPEN(IFIL)) GO TO 9230
 9210 CALL WRITE (FILE,KL(IC),1,0)
      CALL WRITE (FILE,16*(I1T(IC)-2)+J1T(IC),1,0)
      CALL WRITE (FILE,KNO(IC),1,0)
      GO TO R, (1182,1213,1218,1245,2015,2060,2420,2610,2710,3040,
     1          3062,3072,3081,4200,4600,5110,5160,5220,5300,6200,
     2          6620,6685,6710,7011,7032,7050,7090,8200,8620,8260)
 9230 KNOIC = KNO(IC)
      KSRT  = KDSORT(KNOIC)
 9235 KNOX  = HFIL(3,IFIL)
      KSRTX = KDSORT(KNOX)
      IF (KSRT .LT. KSRTX) GO TO 9210
      IF (KSRT .EQ. KSRTX) GO TO 9906
      CALL WRITE (FILE,HFIL(1,IFIL),3,0)
 9240 CALL READ (*9902,*9250,FIL,X(KOR),NKOR,0,RDFLG)
      CALL WRITE (FILE,X(KOR),NKOR,0)
      GO TO 9240
 9250 CALL WRITE (FILE,X(KOR),RDFLG,1)
      CALL FREAD (FIL,HFIL(1,IFIL),3,0)
      DO 9260 J = 1,3
      IF (HFIL(J,IFIL) .NE. EEE(J)) GO TO 9235
 9260 CONTINUE
      INOPEN(IFIL) = .FALSE.
      CALL CLOSE (FIL,1)
      GO TO 9210
C
 9400 KTT  = KT(IC)
      I1TT = I1T(IC)
      J1TT = J1T(IC) + 16
      T(I1TT) = ORF(T(I1TT),TWO(J1TT))
      IF (KTT .EQ. 1) GO TO 9450
      I2TT = I2T(IC)
      J2TT = J2T(IC) + 16
      T(I2TT) = ORF(T(I2TT),TWO(J2TT))
 9450 GO TO R, (9620,9670)
C
C
 9500 IF (INOPEN(IFIL)) GO TO 9520
      CALL WRITE (FILE,EEE,3,1)
      GO TO 9510
 9505 CALL CLOSE (FIL,1)
      INOPEN(IFIL) = .FALSE.
 9510 CALL WRTTRL (T)
      CALL CLOSE (FILE,1)
      GO TO R, (1290,2400,2690,3020,3070,3090,5900,6695,7037,8900,
     1          9630)
 9520 CALL WRITE (FILE,HFIL(1,IFIL),3,0)
 9525 CALL READ  (*9505,*9530,FIL,X(KOR),NKOR,0,RDFLG)
      CALL WRITE (FILE,X(KOR),NKOR,0)
      GO TO 9525
 9530 CALL WRITE (FILE,X(KOR),RDFLG,1)
      GO TO 9525
C
 9600 CALL WRITE (FILE,0,0,1)
      ASSIGN 9620 TO R
      GO TO 9400
 9620 ASSIGN 9630 TO R
      GO TO 9500
 9630 GO TO R1, (1190,1290,2690,3070,5900,6600,6695,6730,7037,7070,
     1           7110,8600,8900)
C
 9650 CALL WRITE (FILE,0,0,1)
      ASSIGN 9670 TO R
      GO TO 9400
 9670 GO TO R1, (1216,1240,2050,2290,2600,3061,3088,3089,4550,5140,
     1           5210,5260,6680,7030,8250,8950)
C
C     DIAGNOSTIC PROCESSING
C
 9900 CALL MESAGE (M,FILE,MNAM)
 9902 M = -2
      GO TO 9900
 9904 WRITE  (NOUT,9954) SFM
 9954 FORMAT (A25,' 1742, NO DATA PRESENT')
      GO TO 9999
 9906 WRITE  (NOUT,9956) UFM,KDN(1,IC),KDN(2,IC)
 9956 FORMAT (A23,' 1744, DATA CARD(S) -',2A4,'- GENERATED BY UTILITY',
     1       ' MODULE INPUT NOT ALLOWED TO APPEAR IN BULK DATA')
      GO TO 9999
 9907 WRITE  (NOUT,9957) UFM
 9957 FORMAT (A23,' 1745, UTILITY MODULE CANNOT HANDLE THE IFLG=2 CASE',
     1       ' SINCE THERE IS NO WAY TO GENERATE GRID POINT G0')
      GO TO 9999
 9908 M = -1
      GO TO 9900
 9909 WRITE  (NOUT,9959) UFM
 9959 FORMAT (A23,' 1746, COORDINATE SYSTEM NOT DEFINED ON A CORD2C',
     1       ' CARD')
C
 9999 M = -61
      CALL PAGE2 (-2)
      GO TO 9900
C
      END
