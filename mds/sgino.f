      SUBROUTINE SGINO        
C        
C     REVISED  9/90 BY G.CHAN/UNISYS. TO REACTIVATE PLT1 FILE        
C        
C     THE HIGH POINTS OF PLT1 FILE ARE        
C       NEW 130 COLUMN FORMAT RECORD        
C       MACHINE PORTABLE FILE        
C       NO DATA RECONSTRUCTION REQUIRED WHEN PLT1 IS USED BY AN EXTERNAL
C          TRANSLATOR PROGRAM        
C                                          PLT2 FILE       PLT1 FILE    
C     ---------------------------------  -------------  --------------  
C     FILE TYPE, SEQUENTIAL FORMATTED    NO CARRIGE CTRL CARRIAGE CTRL  
C     RECORD  TYPE                        ASSCII/BINARY*    ASCII       
C     RECORD  LENGTH                       3000 BYTES     130 COLUMNS   
C     FORTRAN FORMAT                      (10(180A4))*   (5(2I3,4I5))   
C     PLOT COMMANDS PER PHYSICAL RECORD        100            5        
C     DATA TYPE PER COMMAND (TOTAL)         30 BYTES       26 DECIMALS  
C          COMMAND, P   (SEE USER'S MANUAL   1 BYTE         3 DIGITS    
C          CONTROL, C    PAGE 4.4-2)         1 BYTE         3 DITITS    
C          FIRST  VALUE, R                   5 BYTES        5 DIGITS    
C          SECOND VALUE, S                   5 BYTES        5 DIGITS    
C          THIRD  VALUE, T                   5 BYTES        5 DIGITS    
C          FOURTH VALUE, U                   5 BYTES        5 DIGITS    
C          FILLER (ALL ZEROS)                8 BYTES          NONE      
C     DATA BYTE PACKING                        YES            NO        
C     FILE - EDITED, PRINTED, SCREEN VIEWING   NO             YES       
C     PORTABLE FILE AMONG MACHINES             NO             YES       
C     FORTRAN UNIT NUMBER                      13             12        
C     DISC STORAGE REQUIREMENT                  -           25% LESS    
C     IF MAGNETIC TAPE - TRACK AND PARITY     9,ODD          9,ODD      
C     (* 1. ASCII RECORD, BUT DATA STORED IN BINARY BYTES.        
C           (IN EARLY NASTRAN PLOT TAPE DESIGN, A BYTE HAD 6        
C           BITS. BUT IT IS NO LONGER TRUE. NOW, A BYTE CAN        
C           BE 6, 8 OR 9 BITS, DEPENDING ON THE MACHINE)        
C        2. SINCE THE RECORD LENGTH IS 3000 BYTES, A FORMAT        
C           OF (750A4) IS SUFFICIENT)        
C        
      IMPLICIT INTEGER (A-Z)        
      LOGICAL         OPEN,NOPACK        
      INTEGER         BUF(1),LBUF(1),A(1),NAME(2),FORMAT(3),FORMTX(3)   
      CHARACTER*7     FORTN,NONE        
      COMMON /SYSTEM/ IDUM1,NOUT,SKPSYS(36),NBPC,NBPW,NCPW        
CWKBNB
      CHARACTER*80  DSNAMES
      COMMON / DSNAME / DSNAMES(80)
CWKBNE
      DATA    OPEN  /.FALSE. /, NAME  / 4H SGI, 2HNO /        
      DATA    PLT1,PLT2,PLTX /  4HPLT1, 4HPLT2,    0 /        
      DATA    FORMAT/ 4H(10( ,  4H180A, 4H4))        /,        
     1        FORMTX/ 4H(5(2 ,  4HI3,4, 4HI5))       /        
      DATA    FORTN , NONE   / 'FORTRAN', 'NONE   '  /        
      DATA    SHIFT , NBITS  /  0, 0                 /        
C        
      GO TO 250        
C        
C        
      ENTRY SOPEN (*,PLTAPE,BUF,IBFSZ)        
C     ================================        
C        
C     PLT2 FILE -        
C     IBFSZ (FIRST WORD OF /XXPARM/), IS THE PLOT FILE BUFFER SIZE. IT  
C     IS SET EQUAL TO PDATA(12,1)/NCPW IN PLTSET. PDATA(12,1) IS        
C     INITIALIZED  IN PLOTBD VIA DATA(12,1) WHICH IS EQUIVALENT TO      
C     PBFSIZ(1,1). COMPLICATED ISN'T IT?        
C        
C     (PBFSIZ(1,1)=3000, NCPW=4, IBFSZ AND BFSZ ARE THEREFORE =750 EACH 
C     EACH PHYSICAL RECORD HOLDS 100 (=3000/30) PLOT COMMANDS)        
C        
C     NOTE - BOTH PLT2 AND PLT1 ARE SEQUENTIAL FILES, NOT DIRECT ACCESS 
C     FILES. THE RECORD LENGTH, IF USED, IS BASED ON NO. CHARACTERS PER 
C     WORD        
C        
      PTAPE = 10
      IF (PLTAPE.NE.PLT1 .AND. PLTAPE.NE.PLT2) RETURN 1        
      PLTX   = PLTAPE        
      NOPACK = PLTX .EQ. PLT1        
      IF (NOPACK) GO TO 10        
C        
C     PLT2 -        
C        
      BFSZ   = IBFSZ        
      IRECSZ = NCPW*BFSZ        
      NOFF   = LOCFX(BUF(1)) - LOCFX(LBUF(1))        
      GO TO 20        
C        
C     PLT1 -        
C        
CWKBR 10   PTAPE  = 12        
 10   CONTINUE        
      NOPACK = .TRUE.        
      BFSZ   = 30        
      IRECSZ = (BFSZ/6)*(2*3 + 4*5)        
      NONE   = FORTN        
      FORMAT(1) = FORMTX(1)        
      FORMAT(2) = FORMTX(2)        
      FORMAT(3) = FORMTX(3)        
C        
C     NOFF CAN BE SET TO ZERO IF LBUF IS LOCALLY DIMENSIONED TO 30 WORDS
C     AND OPEN CORE IS NOT USED        
C        
      NOFF   = LOCFX(BUF(1)) - LOCFX(LBUF(1))        
C        
C     OPEN STATEMENT ADDED TO SET OUTPUT RECORDSIZE GREATER THAN DEFAULT
C     (COMMENTS FORM G.C./UNISYS 1989 -        
C     RECORDSIZE IS NOT ALLOWED FOR SEQUENTIAL FILE IN SOME COMPILERS,  
C     (e.g. DEC/ULTRIX(RISC), AND BLOCKSIZE AND ORGANINZATION ARE NOT   
C     DEFINED. RECORDTYPE='FIXED' IS ALSO NOT ALLOWED FOR SEQUENTIAL    
C     FORMATTED FILE.        
C     FOR UNICOS, RECL IS NOT ALLOWED IF ASSCESS=SEQUENTIAL)        
C        
C     FOR MACHINES THAT DO NOT HAVE 'APPEN' FEATURE        
C        
 20   IF (OPEN) GO TO 80        
C     MA = 'A'        
C     IF (NONE .EQ. 'NONE') MA = 'M'        
C     IF (MACH .EQ IBM) CALL FILEDEF (PTAPE,RECFM,FB(MA))        
      OPEN (UNIT   = PTAPE,        
CWKBI
     1      FILE = DSNAMES(10),
     1      STATUS = 'OLD',        
     2      FORM   = 'FORMATTED',        
     3      ACCESS = 'SEQUENTIAL',        
     4      IOSTAT = J        
CHP  5      ,CARRIAGECONTROL = NONE        
CHP  6      ,RECL  = IRECSZ        
C            RECL IS NEEDED BY VAX, AND POSSIBLY OTHER MACHINES)        
     6      )        
      IF (J .NE. 0) GO TO 60        
 30   READ   (PTAPE,40,END=50) J        
 40   FORMAT (A1)        
      GO TO 30        
 50   BACKSPACE PTAPE        
      GO TO 80        
C        
 60   OPEN (UNIT   = PTAPE,        
CWKBI
     1      FILE = DSNAMES(10),
     1      STATUS = 'NEW',        
     2      FORM   = 'FORMATTED',        
     3      ACCESS = 'SEQUENTIAL',        
     4      IOSTAT = J        
CHP  5      ,CARRIAGECONTROL = NONE        
CHP  6      ,RECL  = IRECSZ        
C            RECL IS NEEDED BY VAX, AND POSSIBLY OTHER MACHINES)        
     6      )        
      IF (J .EQ. 0) GO TO 80        
      WRITE  (NOUT,70) PLTX,PTAPE        
 70   FORMAT ('0*** SYSTEM FATAL ERROR. SGINO CAN NOT OPEN ',A4,        
     1        ' FILE, FORTRAN UNIT',I5)        
      CALL MESAGE (-61,0,0)        
C        
 80   OPEN  = .TRUE.        
      NB    = 1        
      IF (NOPACK) GO TO 210        
      ASSIGN 100 TO TRA        
      WORD  = 0        
      NBITS = NBPW - NBPC        
      SHIFT = NBITS        
      GO TO 250        
C        
C        
      ENTRY SWRITE (PLTAPE,A,N,EORX)        
C     ==============================        
C        
C     SWRITE IS CALLED ONLY BY WPLT10        
C        
      IF (PLTAPE .NE. PLTX) GO TO 180        
      EOR = EORX        
      NW  = 1        
  90  IF (NOPACK) GO TO 120        
C        
C     ORIGINAL BYTE PACKING LOGIC        
C        
 100  IF (NW .GT. N) GO TO 110        
CUNIX IF (A(NW) .NE. 0) WORD =  OR(ISHFT(A(NW),SHIFT),WORD)        
      IF (A(NW) .NE. 0) WORD = IOR(ISHFT(A(NW),SHIFT),WORD)             
      NW  = NW + 1        
      IF (SHIFT .EQ. 0) GO TO 105        
      SHIFT = SHIFT - NBPC        
      GO TO 100        
 105  LBUF(NB+NOFF) = WORD        
      IF (NB .EQ. BFSZ) GO TO 200        
      WORD  = 0        
      NB    = NB + 1        
      SHIFT = NBITS        
      GO TO 100        
C        
 110  IF (EOR .EQ. 0) GO TO 250        
      EOR = 0        
      IF (SHIFT .NE. NBITS) GO TO 115        
      NB  = NB - 1        
      IF (NB) 190,190,200        
C        
 115  LBUF(NB+NOFF) = WORD        
      GO TO 200        
C        
C     NON BYTE PACKING LOGIC        
C        
 120  IF (NW .GT. N) GO TO 125        
      LBUF(NB+NOFF) = A(NW)        
      NW = NW + 1        
      NB = NB + 1        
      IF (NB .LE. BFSZ) GO TO 120        
      NB = NB - 1        
      GO TO 200        
C        
 125  IF (EOR .EQ. 0) GO TO 250        
      EOR = 0        
      IF (NB .GE. BFSZ) GO TO 135        
      DO 130 J = NB,BFSZ        
 130  LBUF(J+NOFF) = 0        
 135  NB = BFSZ        
      GO TO 200        
C        
C        
      ENTRY SCLOSE (PLTAPE)        
C     =====================        
C        
      EOF = 0        
C        
 150  IF (PLTAPE .NE. PLTX) GO TO 180        
      IF (.NOT.NOPACK .AND. SHIFT.NE.NBITS) GO TO 155        
      NB = NB - 1        
      IF (NB) 170,170,160        
 155  LBUF(NB+NOFF) = WORD        
 160  ASSIGN 165 TO TRA        
      GO TO 200        
 165  ASSIGN 100 TO TRA        
      IF (NOPACK) ASSIGN 120 TO TRA        
 170  IF (EOF .EQ. 0) GO TO 175        
      ENDFILE PTAPE        
      GO TO 190        
 175  PLTX = 0        
      GO TO 190        
C        
 180  WRITE  (NOUT,185) PLTX,PLTAPE        
 185  FORMAT ('0*** SYSTEM FATAL ERROR FROM SGINO. ',A4,' FILE OR ',A4, 
     1        ' FILE GOT LOST')        
      CALL ERRTRC (NAME)        
C        
C        
      ENTRY SEOF (PLTAPE)        
C     ===================        
C        
      EOF = 1        
      GO TO 150        
C        
 190  NB = 1        
      GO TO 250        
C        
 200  WRITE (PTAPE,FORMAT) (LBUF(NOFF+J),J=1,NB)        
      NB = 1        
      WORD  = 0        
      SHIFT = NBITS        
      GO TO TRA, (100,120,165)        
C        
 210  ASSIGN 120 TO TRA        
C        
 250  RETURN        
      END        
