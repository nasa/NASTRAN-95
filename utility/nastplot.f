      PROGRAM NASTPLOT        
C        
CDC   PROGRAM NASPLOT (INPUT,OUTPUT,TAPE5=INPUT,TAPE6=OUTPUT,           
CDC                    TAPE13,TAPE14,TAPE15,TAPE16,TAPE17)              
C        
C     A STAND-ALONE TRANSLATOR PROGRAM (NASTPLOT or NASPLOT) TO        
C     INTERPRET THE NASTRAN GENERAL PURPOSE PLOTTER FILE (OR TAPE)      
C        
C     THIS ROUTINE SHOULD NOT BE COMPILED AND INCLUDED IN NASTRAN       
C     LIBRARY        
C     THIS NASTPLOT PACKAGE INCLUDES THE FOLLOWING SUBROUTINES/AND      
C     ENTRY POINTS        
C        
C          GETCMD, DRWAXS/ENDAXS, DRWKHR/ENDCHR, DRWLIN/ENDLIN,        
C          TXNPEN, TXINIT/TXFINS, TXPLOT, PSINIT/PSFINS/PSTRKE,        
C          PSLINE, PSCHAR/PSENDC        
C     IF TEKTRONIX PLOT-10 TCS AND ADVANCED GRAPHING II PACKAGES ARE    
C     AVAILABLE IN THE COMPUTER SYSTEM. THE NEXT SUBROUTINES ARE USED   
C          NASTEK, PFRAME 
C     (ACTIVATE NASTEK AND PFRAME BY REPLACE C+ BY 2 BLANKS IN FIRST 2
C     COLUMNS OF TEXT BELOW)        
C        
C     IF PLOT-10 AND ADVANCED GRAPHING II ARE NOT AVAILABLE, THE LINES  
C     BETWEEN LABELS 250 AND 260 SHOULD BE COMMENTED OUT OR DELETED.    
C        
C     INPUT FILE: NASTRAN PLT1 OR PLT2 FILE        
C     PLOTTER   : TEKTRONIX OR POSTSCRIPT FILE(S)        
C        
C     ***************************************************************   
C     *   DO NOT COMPILE THIS PROGRAM NOR INSERT ALL RELOCATABLES   *   
C     *   INTO NASTRAN LIBRARY                                      *   
C     *   (HOWEVER. IT DOES NO HARM)                                *   
C     ***************************************************************   
C        
C     THE PLOTTING SECTION OF THIS PROGRAM IS PLOTTER DEPENDENT.        
C     CURRENTLY TEKTRONIX PLOTTER OR POSTSCRIPT PRINTER ARE USED.       
C     ALL SUPPORTING ROUTINES WITH PREFIX 'TX' ARE TEKTRONIX ASSOCIATED 
C     AND THOSE WITH PREFIX 'PS' ARE POSTSCRIPT ASSOCIATED. THE 'PS'    
C     ROUTINES USE ONLY STANDARD FORTRAN COMMANDS.        
C        
C     SUBROUTINE GETCMD IS USED ONLY WHEN MACHINE THAT GENERATED THE    
C     PLOT FILE AND THE MACHINE THAT IS NOW READING THE PLOT FILE HAVE  
C     DIFFERENT NUMBER OF BITS PER COMPUTER WORD.        
C     CURRENTLY GETCMD IS NOT WORKING.        
C        
C     A PC VERSION (MS-DOS/BASIC WITH GRAHPIC) IS ALSO AVAILABLE WHICH  
C     USES ONLY PLT1 FILE. NO SPECIAL HARDWARE OR SOFTWARE ARE NEEDED.  
C        
C     WRITTEN BY G.CHAN/UNISYS, 3/1991        
C        
C        
C     THE '$' ON A FORMAT LINE IS VAX SPECIAL. IT SHOULD BE DELETED IN  
C     ALL NON-VAX MACHINES.        
C        
C     VAX ONLY: SEE THE OPEN STATEMENT AFTER STA. 600 FOR STANDARD OUT- 
C     PUT LIST FILE.        
C        
C     CDC ONLY: DELETE THE 'EOF' ON-LINE FUNCTION,        
C               RE-ACTIVATE THE ON-LINE 'ISHFT' FUNCTION BELOW, AND     
C               SET 'AA' AND 'BB' TO REAL, NOT D.P.        
C        
      IMPLICIT INTEGER (A-Z)        
      LOGICAL      CHRSET,LINSET,AXSSET,VAX,IBM,TEK,PSC,NOTYET,FOUND,   
     1             ADOT,LUX,DEBUG        
      INTEGER      Z(3000),W(30)        
      REAL         XFACT,YFACT,XSIZE,YSIZE,SX,SY,SCALE,CSCAL        
      REAL*8       AA,BB        
CCDC  REAL         AA,BB        
      CHARACTER*1  TEMP,ESC,NUMB1(10),CHR1(5),BLNK1,STP1,NXT1,NA1(32),  
     1             KD,KP,KL,KT,K1,K2,KB,KI,KU,KC,KV,KX,KY,KP1,KL1,KT1,  
     2             K12,KN1,KS1,KH,STR,AL1,KA1,KR1,KNUM(2),KNUM1,BELL,   
     3             KQ1,DorS,A1Z(26)        
      CHARACTER*4  NAM4,BLNK4,STOP4,CHR4        
      CHARACTER*32 NAM32,MACH32,MACH33        
      CHARACTER    CLEAR*2,KNUM2*2,MACH3*3,XX3*3,NUMB10*10,DATE13*13,   
     1             FMT500*8,A2Z*26        
      COMMON       CMND,CNTRL,R,S,T,U,XFACT,YFACT,XMAX,YMAX,XHI,YHI,    
     1             XLO,YLO,PENCHG,PENCNT,OLDPEN,PENNO(99)        
      COMMON /IO/  IN,NOUT        
      COMMON /EC/  ESC,CLEAR        
      COMMON /QQ/  BI,BO,WRD,BYT,DUM(22)        
      COMMON /PS/  LU,SCALE,JOUNT,NBUFF,ONCE,CSCAL,CHRPOS        
      EQUIVALENCE  (NUMB1(1),NUMB10), (BLNK4,BLNK1), (CHR1(1),CHR4),    
     1             (NAM4,NAM32,NA1(1)), (MACH3,MACH32), (Z(1),W(1)),    
     2             (KNUM(1),KNUM1,KNUM2), (A1Z(1),A2Z)        
      DATA         CHRSET,LINSET,AXSSET,TEK,PSC  / 5*.FALSE.     /,     
     1             NOLINE,SELECT,PLOTID,DEBUG / 2*0, -1, .TRUE.  /,     
     2             XSIZE,YSIZE,YES,NO,KQ1 / 1.2, 1., 1HY,1HN, 'q'/,     
     3             STR,BLNK4,NUMB10 / '*', '    ',  '1234567890' /,     
     4             AA,BB,FMT500     / 1.D-21, 1.D-25, '(3000A1)' /,     
     5             STP1,NXT1,DATE13 / 'S', 'N', 'OCT. 27, 1990'  /,     
     6             KD,KP,KL,KT,KB   / '.', 'P', 'L', 'T', ' '    /,     
     7             KH,KP1,KL1,KT1,K1/ 'H', 'p', 'l', 't', '1'    /,     
     8             KI,KU,KC,KV,KX   / 'I', 'U', 'C', 'V', 'X'    /,     
     9             KY,K2,XX3,AL1,KA1/ 'Y', '2', 'UNX','A','a'    /,     
     O             KN1,KS1,SYY,SNN  / 'n', 's', 1Hy, 1Hn         /,     
     1             T4,T3,T2,T1,KR1  / 10000,  1000, 100, 10, 'R' /,     
     2             MACH32   / 'VAX/TEKTRONIX COMPUTER GRAPHIC  ' /,     
     3             MACH33   / 'VAX/POSTSCRIPT COMPUTER GRAPHIC ' /,     
     4             STOP4,A2Z/ 'STOP','ABCDEFGHIJKLMNOPQRSTUVWXYZ'/,     
     5             DorS     / '.'                                /,     
     6             MASK     / '000000FF'X                        /      
C    6             MASK     / X'000000FF', Z000000FF,  63,  511        
C                               UNIX     ,   IBM    , CDC, UNIVAC       
C        
C     SUMMARY OF PROGRAM'S VARIABLES:        
C        
C       VAX = .TURE. IF CURRENT MACHINE IS A  VAX, .FALSE. OTHERWISE    
C       IBM = .TURE. IF CURRENT MACHINE IS AN IBM, .FALSE. OTHERWISE    
C       TEK = .TRUE., TEKTRONIX SCREEN PLOT        
C       PSC = .TURE., POSTSCRIPT FILES GENERATION        
C      MASK = A MASK USED TO PICK UP THE LAST CHARACTER OF A WORD       
C             (MACHINE DEPENDENT)        
C       NBS = NO OF BITS TO BE SHIFTED        
C           = 24 = 3*8, FOR IBM, VAX        
C           = 54 = 9*6, FOR CDC        
C           = 27 = 3*9, FOR UNIVAC        
C       K12 = 1 OR 2 FOR 'PLT1' OR 'PLT2' FILE        
C       PID = 0, IF PLOT FILE DOES NOT HAVE A PLOT-ID FRAME,        
C           = 1, OTHERWISE        
C       ALL = 'Y', STACK ALL PLOTS INTO ONE POSTSCRIPT FILE        
C           = 'N', OTHERWISE.   (= 0, UNDEFINED STATUS)        
C        BI = NO. OF BITS PER WORD OF CURRENT COMPUTER RUNNING NASTPLOT 
C        BO = NO. OF BITS PER WORD OF THE COMPUTER WHICH GENERATED THE  
C             PLT1 OR PLT2 TILE        
C       FIL = INPUT  FILE UNIT FOR PLT1 OR PLT2    (FRMTTD, SQUNTL)     
C       LU  = OUTPUT FILE UNIT FOR POSTSCRIPT FILE (FRMTTD, SQUNTL)     
C      LUX  = IF .TRUE. AND ALL = 'N', LU UNIT NUMBER INCREASES BY 1    
C             EACH TIME A NEW POSTSCRIPT FILE IS GENERATED        
C       IN  = SYSTEM INPUT  FILE, UNIT 5        
C      NOUT = SYSTEM OUTPUT FILE, UNIT 6        
C    LUOPEN = 0, NO LU FILE OPENED.  = 1, LU FILE ALREADY OPENED        
C    NOLINE = 1, ADD "%!" LINE AT THE BEGINNING OF A POSTSCRIPT FILE    
C           = 0, NO SUCH LINE ADDED        
C    CHRPOS = 0, TYPING CHARACTERS ON POSTCRIPT FILE WILL BE STRUNG     
C             TOGETHER FOR ONE OUTPUT COMMAND        
C           = 1, (VIA POSTSCRIPT HELP ONLY) EACH TYPING CHARACTER IS    
C             SENT OUT AS A POSTSCRIPT COMMAND        
C       RL  = RECORD LENGTH IN WORDS        
C       NW  = NO. OF WORDS PER PLOT COMMNAD        
C       JID = WORD LOCATION OF PLOT ID NUMBER IN RECORD        
C       DOT = LOCATION OF '.' IN THE INPUT FILE NAME        
C      DorS = '.' OR '/', ON FILE NAME        
C      ADOT = FLAG FOR THE ORIGINAL FILE NAME THAT HAS A DOT, OR NO DOT 
C      NREC = NO. OF PHYSICAL RECORDS READ FROM INPUT FILE FIL        
C       NRC = ECHO CONTROL = NO. RECORDS/ECHO        
C     JOUNT = NO. OF POSTSCRIPT OUTPUT LINES ACCUMULATED AFTER 'stroke' 
C     NBUFF = POSTCRIPT BUFFER SIZE (BETWEEN 'stroke')        
C       KK  = A COUNTER - NO. OF RECORDS READ ON THE PLOT TAPE        
C    NOTYET = .TRUE. WHEN 1ST NEW PLOT COMMAND HAS NOT FULLY PROCESSED  
C       PNO = SEQUENTIAL PLOT FRAME NUMBER ON PLOT TAPE        
C     FOUND = .TURE. WHEN A NEW PLOT FOUND ON PLOT TAPE (CMND=1)        
C    SELECT = PLOT ID NO. USER SELECTED FOR PLOTTING        
C    LASTSL = PLOT ID NO. LAST SELECTED BY USER        
C     NPLOT = NO. OF PLOTS ACCUMULATED IN THE POSTSCRIPT FILE        
C    PLOTID = CURRENT PLOT ID NO. FOUND ON PLOT TAPE        
C     (NOTE - PLOT ID ON PLOT TAPE IS NOT RELIABLE. THE TAPE MAY HAVE   
C             MISSING PLOT ID, AND IT COULD HAVE TWO PLOT ID 1)        
C        
      EOF(J) = 0*J        
C     ISHFT(W,I) = SHIFT(W,-I)        
C        
C     INITIALIZE MACHINE DEPENDENT CONSTANTS:        
C        
CCDC  BI    = 6        
CUNV  BI    = 9        
      BI    = 8        
      NBS   = 24        
      ADOT  = .TRUE.        
      LUX   = .FALSE.        
      VAX   = .TRUE.        
      IBM   = .FALSE.        
C        
C     GENERAL INITIALIZATION:        
C        
      IF (AA*AA .GT. BB*BB) VAX = .FALSE.        
      IF (BI .EQ. 6) NBS = 54        
      IF (BI .EQ. 9) NBS = 27        
      IF (BI .NE. 8) ADOT = .FALSE.        
      IF (.NOT. VAX) MACH3 = XX3        
C        
      IN    = 5        
      NOUT  = 6        
      FIL   = 13        
      LU    = 14        
      NBUFF = 500        
      WRD   = 1        
      BYT   = 4        
      SCALE = 0.54        
      CSCAL = 13.0        
      NPLOT = 0        
      ALL   = 0        
      HELP  = 0        
      CHRPOS= 0        
      PSALL = 0        
      LUOPEN= 0        
      ESC   = CHAR(27)        
      CLEAR = ESC//CHAR(12)        
      BELL  = CHAR(7)        
      GO TO 20        
C        
C     SELECT TEKTRONIX PLOT OR POSTCRIPT FILE        
C        
   10 CDC = EOF(IN)        
   20 WRITE  (NOUT,25)        
   25 FORMAT (//,' ENTER PLOTTER SELECTION, TEKTRONIX OR POSTSCRIPT? ', 
     1       '(T,P,q,S,HELP) ',$)        
      READ   (IN,30,END=10) TEMP        
   30 FORMAT (A1)        
      IF (TEMP .EQ. KH) GO TO 60        
      IF (TEMP.EQ.KS1 .OR. TEMP.EQ.STP1) GO TO 1050        
      IF (TEMP.NE.KT .AND. TEMP.NE.KT1 .AND. TEMP.NE.KP .AND.        
     1    TEMP.NE.KQ1) GO TO 20        
      IF (TEMP.EQ.KT .OR. TEMP.EQ.KT1) TEK = .TRUE.        
      IF (TEMP.EQ.KP .OR. TEMP.EQ.KQ1) PSC = .TRUE.        
      IF (TEMP .EQ. KQ1) NOLINE = 1        
      IF (TEK) DEBUG = .FALSE.        
      IF (PSC) MACH32 = MACH33        
      IF (.NOT.PSC .OR. HELP.EQ.0) GO TO 80        
      HELP = 0        
      WRITE  (NOUT,40)        
   40 FORMAT (//,' ADDITIONAL HELP MESSAGES (POSTSCRIPT ONLY) -', //1X, 
     1        'OVERALL PLOT SIZE IS CONTROLLED BY PLOT SCALE, AND CHAR',
     2        'ACTER SIZE CAN BE CHANGED', /4X,'BY CHARACTER SCALE, ',  
     3        'ONLY IF ORIGINAL PLOT INCLUDES TYPING CAPABILITY', /,    
     4        ' PostScript EFFICIENY IS GREATLY IMPROVED IF TYPING',    
     5        ' CAPABILITY IS INCLUDED IN THE', /4X,        
     6        'ORIGINAL PLOT. (see NASTRAN PLOTTER card)', //,        
     7        ' FOUR CHARACTER TYPING SYMBOLS HAVE BEEN CHANGED', /,    
     8        '    CHARACTER      NASTRAN         POSTCSRIPT   ', /,    
     9        '      CODE         ORIGINAL        EQUIVALENCE  ', /,    
     O        '    ---------    -------------    --------------', /,    
     1        '     49 (6)      SMALL CIRCLE     BIG SOLID DOT ', /,    
     2        '     50 (7)      SMALL SQUARE     SQUARE-CIRCLE ', /,    
     3        '     51 (8)      SMALL DIAMOND    DAGGER        ', /,    
     4        '     52 (9)      SMALL TRIANGLE   DOUBLE-DAGGER ', //,   
     5        ' IF CHARACTERS ARE POSITIONED BADLY, ENTER * ON NEXT',   
     6        ' COMMAND', //,        
     7        ' YOU MAY ENTER ''ALL'' FOR THE PLOT NO. TO BE PLOTTED')  
      IF (VAX) WRITE (NOUT,45)        
   45 FORMAT (/,' WHEN FINISH, SEND PostScript FILE(S) OUT TO PRINTER', 
     1        ' WITHOUT HEADER PAGE using', /4X,'PRINT/NOFLAG option')  
      WRITE  (NOUT,50)        
   50 FORMAT (/,' HIT C/R TO CONTINUE ',$)        
      READ (IN,30,END=55) TEMP        
      IF (TEMP .EQ. STR) CHRPOS = 1        
      GO TO 80        
   55 CDC = EOF(IN)        
      GO TO 80        
C        
C     HELP MESSAGES        
C        
   60 HELP = 1        
      WRITE  (NOUT,70)        
   70 FORMAT (/,' ENTER "S" TO STOP',        
     1        /,' ENTER "T" FOR TEKTRONIX SCREEN PLOTTER',        
     2        /,' ENTER "P" or "q" TO GENERATE POSTCRIPT FILE(S)',      
     3        /,'    IF "P", POSTCRIPT FILE(S) BEGINS WITH A "%!" LINE',
     4        /,'    IF "q", NO "%!" LINE ADDED',        
     5        /,'    (THIS "%!" LINE IS NEEDED IN SOME UNIX MACHINES, ',
     6        'AND OTHERS DON''T)', //1X,        
     7        'INPUT FILE NAME MUST BE fname.PLT1 or fname.PLT2',       
     8        /,' PLOT FRAME NUMBER IS USED TO SPECIFIED WHICH PLOT TO',
     9        ' BE PLOTTED.',  /,' IF FRAME NO. IS NOT PRESENT ON ',    
     O        'ORIGINAL TAPE, SEQUENTIAL FRAME COUNT IS USED')        
      GO TO 20        
C        
C     GET PLOT FILE FILENAME AND OPEN PLOT FILE        
C        
   80 WRITE  (NOUT,90) MACH32,DATE13        
   90 FORMAT (//////33X,4H****, /31X,1H*,6X,1H*, /30X,1H*,8X,1H*,       
     1       /30X,'*   N A S T P L O T',        
     2       /30X,1H*,8X,1H*, /31X,1H*,6X,1H*, /33X,4H****,        
     3       ///7X,A32,'   SYSTEM RELEASE - ',A13,        
     4       //7X,'WRITTEN BY UNISYS/',17X,'FOR COSMIC', /11X,        
     5       'NASTRAN MAINTENANCE GROUP',6X,'UNIVERSITY OF GEORGIA',    
     6       /11X,'HUNTSVILLE, ALABAMA',12X,'ATHENS, GEORGIA 30602',    
     7       /42X,'PHONE (708) 542-3265', ////7X,        
     8       '*** AT THE END OF EACH PLOT, HIT C/R TO CONTINUE ***',    
     9       /18X,3H===,15X,3H===)        
  100 CDC = EOF(IN)        
      WRITE  (NOUT,110) DorS        
  110 FORMAT (//,' ENTER NAME OF PLOT FILE (e.g. TEST',A1,'PLT1): ',$)  
      READ   (IN,115,END=100) NAM32        
  115 FORMAT (A32)        
      IF (NAM4.EQ.BLNK4 .OR. NAM4.EQ.STOP4) GO TO 1000        
C        
C     CHECK PLT1 OR PLT2 TAPE        
C        
      I = 33        
  120 I = I - 1        
      IF (I .LE. 0) GO TO 1000        
      IF (NA1(I) .EQ. KB) GO TO 120        
      IF ((NA1(I ).EQ.K1 .OR. NA1(I).EQ.K2) .AND. NA1(I-4).EQ.DorS .AND.
     1   (NA1(I-1).EQ.KT .OR. NA1(I-1).EQ.KT1) .AND.        
     2   (NA1(I-2).EQ.KL .OR. NA1(I-2).EQ.KL1) .AND.        
     3   (NA1(I-3).EQ.KP .OR. NA1(I-3).EQ.KP1)) GO TO 140        
C        
C     IMPROPER INPUT FILE NAME        
C        
      WRITE  (NOUT,130) DorS,DorS,(NA1(J),J=1,I)        
  130 FORMAT (/,' FILE ERROR - This program uses only FNAME',A1,'PLT1 ',
     1        'or FNAME',A1,'PLT2 file', 5X,33A1)        
      IF (.NOT.ADOT) WRITE (NOUT,135) DorS,DorS        
  135 FORMAT (' ADD ',A1,'PLT1 or ',A1,'PLT2 TO THE FILE NAME CORRES',  
     1        'PONDING TO YOUR ORIGINAL', /1X,'NASTRAN PLOT FILE SPEC') 
      IF (IBM) WRITE (NOUT,137)        
  137 FORMAT (' USE A SLASH IF FILE IS A DATA SET NAME, OR NO SLASH IF',
     1        ' IT IS A DDNAME')        
      GO TO 100        
C        
C     INITIALIZE PARAMETERS FOR PLT1 OR PLT2 OF RECORDS:        
C     REMOVE '.PLTi' IF ORIGINAL FILE NAME CONTAINS NO FILE EXTENSION   
C        
  140 DOT = I - 4        
      IF (ADOT) GO TO 150        
      DO 145 J = DOT,I        
  145 NA1(J) = KB        
  150 K12 = K1        
      IF (NA1(I) .EQ. K2) K12 = K2        
      RL  = 30        
      NW  = 6        
      NRC = 500        
      JID = 3        
      IF (K12 .EQ. K1) GO TO 160        
      RL  = 3000        
      NW  = 30        
      NRC = 30        
      JID = 6        
      IF (.NOT.VAX) JID = 3        
C     THE ABOVE LINE MAY REQUIRE VERIFICAION ????        
C        
C     OPEN INPUT PLT1 OR PLT2 FILE        
C        
C     PLT1: RECORD LENGTH IS IN 130  BYTES (30 DATA WORDS)        
C           FILE WAS CREATED SEQUENTIAL, FORMATTED, CARRIAGE CONTROL    
C     PLT2: RECORD LENGTH IS IN 3000 BYTES        
C           FILE WAS CREATED SEQUENTIAL, FORMATTED, NO CARRIAGE CONTROL,
C           AND 3000 BYTE LONG RECORDSIZE        
C        
  160 OPEN (UNIT=FIL,FILE=NAM32,ERR=850,STATUS='OLD',FORM='FORMATTED',  
     1      ACCESS='SEQUENTIAL',
     2                          RECL=RL)        
      IF (K12 .EQ. K1) GO TO 200        
C        
C     IF PLT2 IS USED, INQUIRE WHAT MACHINE THE TAPE WAS GENERATED      
C        
  170 CDC = EOF(IN)        
      WRITE  (NOUT,180) K12        
  180 FORMAT (/,' FROM WHAT MACHINE WAS THIS PLT',A1,' FILE GENERATED?',
     1        /,' (Ibm,Univac,Cdc,Vax,uniX,craY): ',$)        
      READ (IN,30,END=170) TEMP        
      IF (TEMP.NE.KI .AND. TEMP.NE.KU .AND. TEMP.NE.KC .AND.        
     1    TEMP.NE.KV .AND. TEMP.NE.KX .AND. TEMP.NE.KY) GO TO 170       
      BO = 8        
      IF (TEMP .EQ. KU) BO = 9        
      IF (TEMP .EQ. KC) BO = 6        
C     IF (BO .EQ. BI) GO TO 200        
      IF (BO .EQ.  8) GO TO 200        
      WRITE  (NOUT,190)        
  190 FORMAT (//,' *** THIS NASTPLOT VERSION CAN NOT PROCESS PLOT TAPE',
     1       ' GENERATED FROM A', /5X,'UNIVAC OR CDC MACHINE')        
      GO TO 1000        
C        
C     CHECK PROSCRIPT PLOTTER SCALE AND        
C     INQUIRE SINGLE OR MULTIPLE POSTSCRIPT OUTPUT FILES        
C        
  200 IF (TEK) GO TO 250        
      IF (.NOT.PSC) GO TO 280        
      WRITE  (NOUT,210) SCALE,CSCAL        
  210 FORMAT (/,' CURRENT PLOT SCALE AND CHARACTER SCALE ARE',2F6.2,    
     1        1H;, /,' ENTER NEW SCALES (2F6.2) or C/R ',$)        
      READ   (IN,220,ERR=230) SX,SY        
  220 FORMAT (2F6.2)        
      IF (SX .GT. 0.01) SCALE = SX        
      IF (SY .GT. 0.01) CSCAL = SY        
C        
  230 CDC = EOF(IN)        
      WRITE  (NOUT,240) K12        
  240 FORMAT (/,' IF PLT',A1,' HAS MULTIPLE PLOTS, STACK ALL PLOTS IN', 
     1        ' ONE POSTSCRIPT FILE? (Y,N) ',$)        
      READ (IN,30,END=230) ALL        
      IF (ALL .EQ. SYY) ALL = YES        
      IF (ALL .EQ. SNN) ALL = NO        
      IF (ALL .EQ. YES) GO TO 280        
      IF (ALL .NE.  NO) GO TO 230        
      GO TO 280        
C        
C     IF MACHINE IS VAX, EQUIPPED WITH TEKTRONIX PLOT-10 TSC AND        
C     ADVANCED GRAPHING II PACKAGES, CALL NASTEK TO DO THE JOB        
C        
  250 CONTINUE        
C     IF (.NOT.VAX) GO TO 260        
      WRITE  (NOUT,255)        
  255 FORMAT (/,' PLOT-10 TCS AND ADVANCED GRAPHING II PACKAGES ON YOUR'
     1,       ' VAX SYSTEM? (Y/N) ',$)        
      READ (IN,30) J        
      IF (J .NE. YES) GO TO 260        
      I = 5        
      J = 30        
      IF (K12 .EQ. K1) GO TO 257        
      I = 100        
      J = 3000        
  257 CONTINUE
C+    CALL NASTEK (*1050,FIL,I,Z,J)        
      CLOSE (UNIT=FIL)        
      GO TO 1000        
C        
C     CHECK TEKTRONIX HORIZ. AND VERTICAL AXES        
C        
  260 WRITE  (NOUT,265) XSIZE,YSIZE        
  265 FORMAT (/,' CURRENT FRAME SIZE IS:  X-AXIS=',F6.2,' Y-AXIS=',F6.2,
     1        /,' ENTER NEW SIZES (2F6.2) or C/R: ',$)        
      READ   (IN,220,ERR=275) SX,SY        
      IF (SX .GT. 0.01) XSIZE = SX        
      IF (SY .GT. 0.01) YSIZE = SY        
      IF (YSIZE .LE. 32.0) GO TO 280        
      WRITE  (NOUT,270)        
  270 FORMAT (' MAXIMUM LENGTH FOR Y-AXIS IS 32.0 INCHES')        
      GO TO 250        
  275 CDC = EOF(IN)        
C        
C     LOOK FOR PLOT ID-FRAME IN Z(n). IF IT IS FOUND, REPLACE THE PLOT  
C     NUMBER IN Z(JID) BY ZERO.  n IS 19 FOR PLT1 AND n IS 90 FOR PLT2. 
C     SET PID FLAG TO 1 IF PLOT ID-FRAME DOES EXIST. OTHERWISE ZERO     
C     (PLOT ID-FRAME IS THE PLOT WITH MANY HORIZONTAL LINES, AND USER   
C     ID AT MID PAGE)        
C        
  280 REWIND FIL        
      PLOTID = -1        
      LASTSL = -1        
      PNO = 0        
      KK  = 1        
      IF (K12 .EQ. K2) GO TO 290        
      READ (FIL,460,END=900) W        
      IF (W(19)-16) 300,320,300        
C        
  290 READ (FIL,FMT500,END=900) Z        
C        
C       NEXT DO LOOP LOCATES '16' ON PLT2 FILE        
C       (IT WAS FOUND ON THE 90TH WORD)        
C        
C       DO 295 I = 1,99        
C       L = IAND(Z(I),MASK)        
C       WRITE  (NOUT,292) I,L        
C 292   FORMAT ('  WORD',I3,' = ',I7)        
C 295   CONTINUE        
C        
      IF (IAND(Z(90),MASK) .EQ. 16) GO TO 320        
  300 WRITE  (NOUT,310) K12        
  310 FORMAT (/,' THERE IS NO PLOT ID FRAME (PLOT NO. 0) ON USER''S ',  
     1       'PLT',A1,' FILE',/)        
      PID = 0        
      GO TO 350        
  320 PID = 1        
      Z(JID) = 0        
C        
C     ON THE PLOT FILE AS GENERATED BY NASTRAN, BOTH PLOT ID-FRAME AND  
C     THE FIRST STRUCTURE PLOT ARE PLOT NUMBER 1.  TREAT THE PLOT ID-   
C     FRAME AS PLOT NUMBER 0 THROUGHOUT THIS NASTPLOT PROGRAM        
C        
      WRITE  (NOUT,330) K12        
  330 FORMAT (/,' THERE IS A PLOT ID FRAME (PLOT NUMBER 0) ON USER''S ',
     1        'PLT',A1,' FILE')        
      GO TO 350        
C        
  340 WRITE (NOUT,310)        
C        
C     INQUIRE WHICH PLOT TO BE PLOTTED (SELECT)        
C     (ALLOW 5-DIGIT PLOT NO, CAN BE LEFT OR RIGHT AJUSTED)        
C        
  350 WRITE  (NOUT,360)        
  360 FORMAT (//,' ENTER PLOT NUMBER TO BE PLOTTED, ''NEXT'' or ',      
     1        '''STOP'' ',$)        
      READ   (IN,370,END=350) CHR1        
  370 FORMAT (5A1)        
      IF (CHR4 .EQ. BLNK4) GO TO 350        
      SELECT = -99        
      IF (PSC .AND. (CHR1(1).EQ.AL1 .OR. CHR1(1).EQ.KA1)) PSALL = 1     
      IF (PSALL.NE.0 .OR. CHR1(1).EQ.NXT1 .OR. CHR1(1).EQ.KN1) GO TO 510
      IF (CHR1(1).EQ.STP1 .OR. CHR1(1).EQ.KS1) GO TO 1010        
      SELECT = 0        
      TEN = 1        
      K   = 5        
      DO 410 J = 1,5        
      IF (CHR1(K) .EQ. BLNK1) GO TO 410        
      DO 380 I = 1,10        
      IF (CHR1(K) .EQ. NUMB1(I)) GO TO 400        
  380 CONTINUE        
      WRITE  (NOUT,390)        
  390 FORMAT (/5X,'...INPUT ERROR')        
      GO TO 350        
  400 IF (I .EQ. 10) I = 0        
      SELECT = SELECT + I*TEN        
      TEN = TEN*10        
  410 K   = K - 1        
      FOUND = .FALSE.        
      IF (SELECT     .LT. 0) GO TO 900        
      IF (SELECT+PID .EQ. 0) GO TO 340        
      IF (LASTSL    .EQ. -2) GO TO 510        
      IF (SELECT - LASTSL) 950,420,510        
  420 CDC = EOF(IN)        
      WRITE  (NOUT,430) LASTSL        
  430 FORMAT (' LAST PLOT WAS PLOT NO.',I3,'.   ARE YOU SURE? (Y,N) ',$)
      READ (IN,30,END=420) J        
      IF (J.EQ.YES .OR. J.EQ.SYY) GO TO 950        
      IF (J.NE.NO .AND. J.NE.SNN) GO TO 420        
      GO TO 350        
C        
C     SEARCH PLOT TAPE FOR PLOT SELECTED        
C     UP TO 2 DIGITS ONLY FOR PLOT ID        
C        
  440 IF (K12 .EQ. K2) GO TO 490        
  450 READ   (FIL,460,END=900) W        
  460 FORMAT (5(2I3,4I5))        
      KK = KK + 1        
      IF (DEBUG .AND. MOD(KK,NRC) .EQ. 0) WRITE (NOUT,470) KK        
  470 FORMAT (10X,'...SEARCHING',I6,' RECORDS')        
  480 FORMAT (10X,'...WORKING  ',I6,' RECORDS PROCESSED')        
      IF (W(1) .NE. 1) GO TO 450        
      GO TO 510        
  490 READ   (FIL,FMT500,END=900) Z        
C 500 FORMAT (3000A1)        
      KK = KK + 1        
      IF (DEBUG .AND. MOD(KK,NRC) .EQ. 0) WRITE (NOUT,470) KK        
      IF (.NOT.VAX .AND. ISHFT(Z(1),NBS).NE.1) GO TO 490        
      IF (     VAX .AND. IAND(Z(4),MASK).NE.1) GO TO 490        
C        
C     WRITE  (NOUT,505) JID,(Z(J),J=1,9)        
C 505 FORMAT (' JID =',I3,',  FIRST 9 BYTES OF NEW-PLOT RECORD = ',     
C    1        /12X,9I7)        
C        
      IF (.NOT.VAX) PLOTID = ISHFT(Z(JID-1),NBS)*10 + ISHFT(Z(JID),NBS) 
      IF (     VAX) PLOTID = IAND(Z(JID+1),MASK)*10 + IAND(Z(JID),MASK) 
C        
C     SELECT WAS SET TO -99 IF USER WANTS THE NEXT PLOT, OR        
C     USER REQUEST ALL PLOTS ON POSTSCRIPT        
C        
  510 IF (K12.NE.K2 .OR. LASTSL.EQ.-2) PLOTID = Z(JID)        
C     IF (DEBUG) WRITE (NOUT,545) PLOTID        
      IF (SELECT .EQ. -99) SELECT = PLOTID        
      IF (LASTSL .EQ. -2 ) LASTSL = -1        
      IF (PLOTID.GE.0 .OR. PSALL.NE.0) IF (PLOTID-SELECT) 440,540,520   
      IF (PNO+PID .EQ. SELECT) GO TO 540        
      PNO = PNO + 1        
      GO TO 440        
C        
C     INPUT TAPE MAY NOT SPECIFY PLOT NO.        
C        
  520 CDC = EOF(IN)        
      WRITE  (NOUT,530) PLOTID,SELECT        
  530 FORMAT (/,' PLOT NO.',I4,' JUST FOUND. INPUT TAPE MAY NOT USE ',  
     1       'OR SPECIFY PLOT NO.',I4,        
     2        /,' CONTINUE SEARCHING or REWIND TAPE? (Y,N,R) ',$)       
      READ (IN,30,END=520) TEMP        
      IF (TEMP .EQ. KR1) GO TO 950        
      IF (TEMP .EQ. KY ) GO TO 440        
      GO TO 350        
C        
C     MATCHING PLOT NUMBER JUST FOUND.        
C     SET N1 FOR THE 900 LOOP, THAT POINTS TO THE 2ND PLOT COMMAND      
C        
  540 FOUND  = .TRUE.        
      PLOTID = SELECT        
      IF (PLOTID .EQ. -1) PLOTID = 0        
      IF (DEBUG) WRITE (NOUT,545) PLOTID        
  545 FORMAT (13X,'PLOT NO =',I5,'  FOUND')        
      NOTYET = .TRUE.        
      NREC   = KK        
      N1     = 7        
      IF (K12 .EQ. K2) N1 = 1        
      IF (PSC) GO TO 550        
C        
C     INITIALIZE TEKRONIX:        
C        
      CALL TXINIT        
      XHI    = 0        
      YHI    = 0        
      PENCHG = 0        
      PENCNT = 0        
      OLDPEN = 0        
      GO TO 620        
C        
C     INITIALIZE POSTCRIPT:        
C        
  550 NPLOT  = NPLOT + 1        
      JOUNT  = 0        
      ONCE   =-1        
      IF (ADOT) GO TO 560        
      IF (ALL .EQ. YES) IF (LUOPEN) 590,555,590        
  555 NA1(DOT) = A1Z(NPLOT)        
      GO TO 585        
  560 KNUM1  = KB        
      KNUM(2)= KB        
      IF (ALL .EQ. YES) IF (LUOPEN) 590,580,590        
      IF (NPLOT .LT. 10) WRITE (KNUM1,565) NPLOT        
      IF (NPLOT .GE. 10) WRITE (KNUM2,570) NPLOT        
  565 FORMAT (I1)        
  570 FORMAT (I2)        
  580 NA1(DOT+2) = KNUM1        
      NA1(DOT+3) = KNUM(2)        
      NA1(DOT+4) = KB        
  585 OPEN (UNIT=LU,FILE=NAM32,FORM='FORMATTED',STATUS='NEW',ERR=870    
C    1     )        
     1     ,CARRIAGECONTROL='LIST')   ! RECOMMANDED FOR VAX TO GENERATE 
C                                     ! STANDARD LIST FILE        
      LUOPEN = 1        
  590 CALL PSINIT (NOLINE)        
      IF (ALL .EQ. YES) NOLINE = 1        
      GO TO 620        
C        
  600 NREC = NREC + 1        
      IF (PSC .AND. DEBUG .AND. MOD(NREC,NRC).EQ.0)        
     1    WRITE (NOUT,480) NREC        
C        
C     READ NEXT RECORD        
C        
      IF (K12 .EQ. K2) GO TO 610        
      READ (FIL,460,END=900) W        
      GO TO 620        
  610 READ (FIL,FMT500,END=900) Z        
C        
C     INTERCHANGE BYTE ORDER IF MACHINE IS VAX        
C        
  620 KK = KK + 1        
      IF (K12.EQ.K1 .OR. .NOT.VAX) GO TO 640        
      DO 630 I = 1,RL,4        
      J      = Z(I  )        
      Z(I  ) = Z(I+3)        
      Z(I+3) = J        
      J      = Z(I+1)        
      Z(I+1) = Z(I+2)        
      Z(I+2) = J        
  630 CONTINUE        
C        
C     PROCESS ONE PHYSICAL PLOT RECORD        
C        
  640 DO 800 N = N1,RL,NW        
C        
C     GET NEXT COMMAND,        
C        
      IF (K12 .EQ. K2) GO TO 650        
C        
C     PLT1 FILE: TOTAL OF 5 PLOT COMMANDS/RECORD        
C        
      CMND  = W(N)        
      IF (CMND .EQ. 0) GO TO 810        
      CNTRL = W(N+1)        
      R = W(N+2)        
      S = W(N+3)        
      T = W(N+4)        
      U = W(N+5)        
      GO TO 680        
C        
C     PLT2 FILE:  TOTAL OF 100 PLOT COMMANDS/RECORD        
C     EACH PLOT COMMAND IS 30 BYTE LONG, AND DATA IN FIRST 22 BYTES     
C        
  650 IF (BI .EQ. BO) GO TO 660        
      CALL GETCMD (Z)        
      GO TO 680        
C        
  660 N22  = N + 21        
      DO 670 J = N,N22        
      IF (     VAX) Z(J) = IAND(Z(J), MASK)        
      IF (.NOT.VAX) Z(J) = ISHFT(Z(J),-NBS)        
  670 CONTINUE        
      CMND  = Z(N)        
      IF (CMND .EQ. 0) GO TO 810        
      CNTRL = Z(N+1)        
      R = Z(N+ 2)*T4 + Z(N+ 3)*T3 + Z(N+ 4)*T2 + Z(N+ 5)*T1 + Z(N+ 6)   
      S = Z(N+ 7)*T4 + Z(N+ 8)*T3 + Z(N+ 9)*T2 + Z(N+10)*T1 + Z(N+11)   
      T = Z(N+12)*T4 + Z(N+13)*T3 + Z(N+14)*T2 + Z(N+15)*T1 + Z(N+16)   
      U = Z(N+17)*T4 + Z(N+18)*T3 + Z(N+19)*T2 + Z(N+20)*T1 + Z(N+21)   
      IF (.NOT.NOTYET) GO TO 680        
      NOTYET =.FALSE.        
      XMAX = S        
      YMAX = T        
      XFACT= XSIZE/XMAX        
      YFACT= YSIZE/YMAX        
      XLO  = XMAX        
      YLO  = YMAX        
      GO TO 800        
C        
C     CMND =  0, NON-OPERATION. A PADDING COMMANDS OF ALL ZEROS        
C          =  1, START-NEW-PLOT. R IS THE PLOT NUMBER        
C          =  2, SELECT-CAMERA. CNTRL=1 FILM ONLY, =2 HARDCOPY, =3 BOTH 
C          =  3, SKIP-TO-A-NEW-FRAME. CNTRL=1,2,3 SAME AS CMND=2        
C                AT LEAST ONE CMND=3 COMMAND AFTER A CMND=1 COMMAND AND 
C                BEFORE NEXT CMND=1        
C          =  4, TYPE-CHARACTER        
C          =  5, DRAW-LINE        
C          =  6, DRAW-AXIS        
C          = 14, 15, 16, SAME AS 4, 5, 6        
C                THESE COMMANDS INDICATE FIRST OF A SERIES OF 4, 5, OR  
C                6 COMMNADS THAT FOLLOW        
C     CMND = 2,3 ARE FOR MICROFILM PLOTTER ONLY. THEY ARE NOT USED      
C                IN THIS NASPLT PROGRAM        
C        
C     CNTRL= A PEN NO., OR A LINE DENSITY, OR A CAMERA NO, OR A POINTER 
C            INTO A LIST OF CHARACTERS AND SYMBOL (TABLE 1 OF THE       
C            USER'S MANUAL, P.4.4-5)        
C        
C     R,S,T,U = DATA VALUES        
C        
C        
C        
C     NOTE, CMND=1 (START-NEW-PLOT) ALWAYS LOCATES ON THE FIRST COMMAND 
C     OF A PLOT RECORD        
C     IGNORE CMND =2 OR 3, WHICH ARE FOR MICROFILM PLOTTER ONLY        
C        
  680 IF (CMND .NE. 1) GO TO 720        
      IF (N    .NE. 1) WRITE (NOUT,685) BELL,BELL,BELL,N,CNTRL        
  685 FORMAT (1X,3A1,/,'0*** LOGIC ERROR. NEW PLOT FOUND AT MIDDLE OF ',
     1        'RECORD', /5X,'N =',I5,',  PLOT NO.',I5)        
C        
C     END OF PREVIOUS PLOT        
C        
      WRITE  (NOUT,690) BELL        
  690 FORMAT (1X,A1)        
      IF (TEK) GO TO 710        
      CALL PSTRKE        
      CALL PSFINS        
      IF (ALL .EQ. YES) GO TO 695        
      CLOSE (UNIT=LU)        
      IF (LUX) LU = LU + 1        
      LUOPEN = 0        
  695 WRITE  (NOUT,700) PLOTID,K12        
  700 FORMAT (/,' A PLOT WAS GENERATED FROM PLOT NO.',I3,' OF THE PLT', 
     1        A1,' FILE',/)        
      IF (K12  .EQ. K2) BACKSPACE FIL        
      LASTSL = SELECT        
      SELECT = R        
      PLOTID = R        
      IF (PSALL) 540,350,540        
C        
  710 READ (IN,30,END=715) J        
  715 CDC = EOF(IN)        
      CALL TXFINS        
      GO TO 350        
C        
  720 IF (CMND .LE. 3) GO TO 800        
C        
C     CHECK FOR NEW CHARACTER, LINE, OR AXIS SET        
C        
      IF (CMND.LT.14 .OR. CMND.GT.16) GO TO 730        
      IF (CMND .EQ. 14) CHRSET = .TRUE.        
      IF (CMND .EQ. 15) LINSET = .TRUE.        
      IF (CMND .EQ. 16) AXSSET = .TRUE.        
      CMNDX = CMND        
      CMND  = CMND - 10        
C        
C     CHECK FOR END OF CHARACTER, LINE, OR AXIS SET        
C        
  730 IF (.NOT.PSC) GO TO 740        
      IF (CHRSET .AND. CMND.NE.4) CALL PSENDC (CHRSET,CMNDX)        
      IF (CMND .NE. 4) GO TO 750        
      CALL PSCHAR (CHRSET,CMNDX)        
      GO TO 800        
  740 IF (CHRSET .AND. CMND.NE.4) CALL ENDKHR (CHRSET)        
      IF (LINSET .AND. CMND.NE.5) CALL ENDLIN (LINSET)        
      IF (AXSSET .AND. CMND.NE.6) CALL ENDAXS (AXSSET)        
C        
  750 IF (CMND-5) 760,770,790        
C         CMND  =  4   5   6        
C           or    14  15  16        
C        
  760 IF (TEK) CALL DRWKHR (CHRSET)        
      GO TO 800        
  770 IF (PSC) GO TO 780        
      CALL DRWLIN (LINSET)        
      GO TO 800        
  780 CALL PSLINE        
      GO TO 800        
  790 IF (PSC) GO TO 780        
      CALL DRWAXS (AXSSET)        
  800 CONTINUE        
C        
C     LOOP BACK FOR MORE PLOT RECORDS FROM PLOT FILE        
C     SESET N1 FOR THE 900 LOOP, THAT POINTS TO THE 1ST PLOT COMMAND    
C        
  810 N1 = 1        
      GO TO 600        
C        
C     PLOT FILE NOT FOUND. TRY AGAIN        
C        
  850 WRITE  (NOUT,860) NAM32        
  860 FORMAT (/,' *** NO SUCH INPUT FILE ',A32)        
      IF (IBM) WRITE (NOUT,137)        
      GO TO 100        
C        
  870 WRITE  (NOUT,880) NAM32        
  880 FORMAT (//,' *** CAN NOT OPEN OUTPUT FILE - ',A32,        
     1        /5X,'JOB ABORTED')        
      CLOSE (UNIT=FIL)        
      GO TO 1050        
C        
C     EOF ENCOUNTERED        
C        
  900 WRITE (NOUT,690) BELL        
      IF (PLOTID.EQ.-1 .OR. .NOT.FOUND) GO TO 920        
      IF (PSC) GO TO 910        
      READ (IN,30,END=905) J        
  905 CDC = EOF(IN)        
      CALL TXFINS        
      GO TO 920        
  910 J = 0        
      IF (CHRSET) CALL PSENDC (CHRSET,J)        
      CALL PSTRKE        
      CALL PSFINS        
      IF (ALL .EQ. YES) GO TO 915        
      CLOSE (UNIT=LU)        
      IF (LUX) LU = LU + 1        
      LUOPEN = 0        
  915 IF (NPLOT .GT. 0) WRITE (NOUT,700) PLOTID,K12        
  920 IF (.NOT.FOUND) WRITE (NOUT,930) SELECT        
  930 FORMAT (/,' PLOT NO.',I4,' DOES NOT EXIST')        
  940 WRITE  (NOUT,945) PLOTID,K12        
  945 FORMAT (/,' EOF ENCOUNTERED. THERE ARE ONLY',I3,' PLOTS ON ',     
     1       'USER''S PLT',A1,' FILE', /,        
     2       ' START ALL OVER PLOTTING AGAIN? (Y,N) ',$)        
      READ (IN,30,END=940) J        
      IF (J.EQ.NO   .OR. J.EQ.SNN) GO TO 1010        
      IF (J.NE.YES .AND. J.NE.SYY) GO TO  920        
      SELECT = -1        
C        
C     START ALL OVER AGAIN        
C        
  950 REWIND FIL        
      KK    = 0        
      PSALL = 0        
      PNO   = 0        
      PLOTID=-1        
      LASTSL=-2        
      IF (K12 .EQ. K1) READ (FIL,   460,END=1010) W        
      IF (K12 .EQ. K2) READ (FIL,FMT500,END=1010) Z        
      IF (PID .EQ.  0) GO TO 960        
      Z(JID) = 0        
      GO TO 970        
  960 IF (K12 .EQ. K1) GO TO 970        
      IF (.NOT.VAX) Z(JID) = ISHFT(Z(JID-1),NBS)*10 + ISHFT(Z(JID),NBS) 
      IF (     VAX) Z(JID) = IAND(Z(JID+1),MASK)*10 + IAND(Z(JID),MASK) 
  970 IF (SELECT) 350,510,510        
C        
C     JOB DONE. TIDY UP ALL LOOSE ENDS        
C        
 1000 WRITE (NOUT,1020)        
      GO TO 1050        
 1010 CLOSE (UNIT=FIL)        
      IF (ALL.EQ.YES .AND. LUOPEN.EQ.1) CLOSE (UNIT=LU)        
      WRITE  (NOUT,1020)        
 1020 FORMAT (//5X,'*** JOB DONE ***')        
      IF (.NOT.PSC .OR. NPLOT.LE.0) GO TO 1050        
      KC = KB        
      IF (NPLOT .GT. 1) KC = STP1        
      IF (ALL .EQ. YES) GO TO 1030        
      NA1(DOT+3) = KB        
      IF (NPLOT .LE. 1) GO TO 1030        
      NA1(DOT+2) = STR        
      KB = STP1        
 1030 WRITE  (NOUT,1040) NPLOT,KC,KB,NAM32        
 1040 FORMAT (//4X,I2,' PLOT FILE',A1,' GENERATED. SEND THE FOLLOWING ',
     1       'PostScript FILE',A1, /5X,'TO PRINTER - ',A32)        
C        
 1050 CONTINUE        
      END        
C        
C     ================================================================= 
C        
      SUBROUTINE GETCMD (Z)        
C        
C     THIS SUBROUTINE GET THE PLOT COMMAND        
C        
C     THIS SUBROUTINE IS NEEDED ONLY WHEN THE MACHINE READING THE PLT2  
C     TAPE AND THE MACHINE WHO WROTE THE PLT2 TAPE, HAVE DIFFERENT      
C     NUMBER OF BITS PER BYTE, NBPB        
C     E.G.  NBPB IS 6 FOR CDC, 8 FOR VAX AND IBM, AND 9 FOR UNIVAC      
C        
C     BI = NBPB ON WRITING COMPUTER        
C     BO = NBPB ON READING COMPUTER        
C        
      IMPLICIT INTEGER (A-Z)        
      INTEGER     Z(1),Q(1)        
      COMMON /QQ/ BI,BO,WRD,BYT,PC,CI,R4,R3,R2,R1,R0,S4,S3,S2,S1,S0,    
     1            T4,T3,T2,T1,T0,U4,U3,U2,U1,U0        
      COMMON      CMND,CTRL,R,S,T,U        
      EQUIVALENCE (Q(1),PC)        
      DATA   MAX/ 3000 /        
C        
      IF (BI .EQ. BO) STOP ' NO NEED TO CALL GETCMD'        
      IF (BI .NE. BO) STOP ' PRESENTLY, GETCMD DOES NOT WORK'        
C        
C     GET 30-BYTE COMMAND INTO 30 INTEGERS        
C     THE FOLLOWING LOGIC IS FACTITIOUS        
C        
      IF (BI.EQ.8 .AND. BO.EQ.6) GO TO 1110        
C        
C     NBPC OF 6 INTO NBPC OF 8        
C        
      DO 1100 I = 1,30        
      Q(I) = Z(WRD)        
      IF (BYT .EQ.   0) WRD = WRD + 1        
      IF (BYT .EQ.   0) BYT = 6        
      IF (WRD .GT. MAX) WRD = 1        
 1100 BYT = BYT - 1        
      GO TO 1130        
C        
C     NBPC OF 8 INTO NBPC OF 6        
C        
 1110 DO 1120 I = 1,30        
      Q(I) = Z(WRD)        
      IF (BYT .EQ.   0) WRD = WRD + 1        
      IF (BYT .EQ.   0) BYT = 8        
      IF (WRD .GT. MAX) WRD = 1        
 1120 BYT = BYT - 1        
C        
 1130 CMND = PC        
      CTRL = CI        
      R    = R4*10000 + R3*1000 + R2*100 + R1*10 + R0        
      S    = S4*10000 + S3*1000 + S2*100 + S1*10 + S0        
      T    = T4*10000 + T3*1000 + T2*100 + T1*10 + T0        
      U    = U4*10000 + U3*1000 + U2*100 + U1*10 + U0        
      RETURN        
      END        
C        
C     ================================================================= 
C        
      SUBROUTINE DRWAXS (AXSSET)        
C        
      IMPLICIT INTEGER (A-Z)        
      REAL     XFACT,YFACT,X,Y        
      LOGICAL  AXSSET,P16        
      COMMON   CMND,CNTRL,R,S,T,U,XFACT,YFACT,XMAX,YMAX,XHI,YHI,XLO,YLO,
     1         PENCHG,PENCNT,OLDPEN,PENNO(99)        
      DATA     AXSCNT,P16 / 0, .TRUE. /        
C        
C     CHECK FOR INITIAL COMMAND = 16        
C        
      IF (AXSSET) GO TO 1150        
      AXSSET = .TRUE.        
      P16    = .FALSE.        
C        
C     CHECK COORDINATES AGAINST MAX & MIN ENCOUNTERED SO FAR        
C        
 1150 XHI = MAX(R,T,XHI)        
      YHI = MAX(S,U,YHI)        
      XLO = MIN(R,T,XLO)        
      YLO = MIN(S,U,YLO)        
C        
C     CHECK FOR A PEN CHANGE AND/OR NEW PEN ID        
C        
      IF (CNTRL .EQ. OLDPEN) GO TO 1180        
      CALL TXNPEN (CNTRL)        
      PENCHG = PENCHG + 1        
      IF (PENCNT .GE. 99) GO TO 1170        
      PENCNT = PENCNT + 1        
      PENNO(PENCNT) = CNTRL        
      IF (PENCNT .LE. 1) GO TO 1170        
      DO 1160 N = 2,PENCNT        
      IF (PENNO(PENCNT) .NE. PENNO(N-1)) GO TO 1160        
      PENCNT = PENCNT -1        
      GO TO 1170        
 1160 CONTINUE        
 1170 OLDPEN = CNTRL        
 1180 AXSCNT = AXSCNT + 1        
C        
C     MOVE PLOTTER PEN TO BEGINNING OF AXIS        
C        
      X = XFACT*FLOAT(R)        
      Y = YFACT*FLOAT(S)        
      CALL TXPLOT (X,Y,3)        
C        
C     DRAW AXIS ON PLOTTING SURFACE        
C        
      X = XFACT*FLOAT(T)        
      Y = YFACT*FLOAT(U)        
      CALL TXPLOT (X,Y,2)        
      RETURN        
C        
C        
      ENTRY ENDAXS (AXSSET)        
C     =====================        
C        
C     REINITILIZE COMMANDS        
C        
      P16    = .TRUE.        
      AXSCNT =  0        
      AXSSET = .FALSE.        
      RETURN        
      END        
C        
C     ================================================================= 
C        
      SUBROUTINE DRWKHR (CHRSET)        
C        
      IMPLICIT  INTEGER (A-Z)        
      LOGICAL   CHRSET        
      REAL      XFACT,YFACT,X,Y,ANGLE,HITE        
      CHARACTER CHR94*52,CHRSAV*57,BLNK1,QUES1        
      DIMENSION CHRCOD(53)        
      COMMON    CMND,CNTRL,R,S,T,U,XFACT,YFACT,XMAX,YMAX,XHI,YHI,XLO,YLO
      DATA      LINE,SAVCNT,CHRCNT,UNKCNT / 1, 0, 0, 0/        
      DATA      CHRSAV,BLNK1,QUES1    / ' ', ' ', '?' /        
      DATA      CHR94 /        
     1          '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ()+-*/=.,$" O[<^'/ 
      DATA      CHRCOD /32,33,34,35,36,37,38,39,40,41,        
     1                  49,50,51,52,53,54,55,56,57,58,59,60,61,        
     2                  62,63,64,65,66,67,68,69,70,71,72,73,74,        
     3                  24,25,27,29,26,31,45,30,28,20,23,        
     4                  16, 1, 0, 5, 2,47/        
C        
C     CHECK FOR INITIAL COMMAND = 14        
C        
      IF (CHRSET) GO TO 1200        
      CHRSET = .TRUE.        
      LINE   = LINE + 1        
C        
C     UPDATE COUNTER & POINTER        
C        
 1200 SAVCNT = SAVCNT + 1        
      CHRCNT = CHRCNT + 1        
C        
C     STORE CHARACTER IN STRING CHRSAV        
C        
      IF (CNTRL.LE.52 .AND. CNTRL.GT.0) GO TO 1210        
      CHRSAV(SAVCNT:SAVCNT) = QUES1        
      CODE   = CHRCOD(53)        
      UNKCNT = UNKCNT + 1        
      GO TO 1220        
C        
 1210 CHRSAV(SAVCNT:SAVCNT) = CHR94(CNTRL:CNTRL)        
      CODE = CHRCOD(CNTRL)        
C        
C     IF CHRSAV STRING IS FULL, PRINT IT        
C        
 1220 IF (SAVCNT .NE. 57) GO TO 1230        
      CHRSAV = BLNK1        
      SAVCNT = 0        
      LINE   = LINE + 1        
C        
C     COMPARE (X,Y) WITH MAX & MIN ENCOUNTERED SO FAR        
C        
 1230 XHI = MAX(XHI,R)        
      YHI = MAX(YHI,S)        
      XLO = MIN(XLO,R)        
      YLO = MIN(YLO,S)        
C        
C     DRAW CHARACTER ON PLOTTER        
C        
      X     = XFACT*FLOAT(R)        
      Y     = YFACT*FLOAT(S)        
      HITE  = 0.08*XFACT*100.0        
      ANGLE = 0.0        
C        
C     BRING IN NASTRAN SYMBOL ROUTINE IF IT IS NEEDED        
C        
C     CALL SYMBOL (X,Y,HITE,CODE,ANGLE,-1)        
      RETURN        
C        
C        
      ENTRY ENDKHR (CHRSET)        
C     =====================        
C        
C     PRINT OUT CONTENTS OF CHRSAV & COUNTERS        
C     REINITIALIZE        
C        
      LINE   = 1        
      CHRSAV = BLNK1        
      CHRSET = .FALSE.        
      SAVCNT = 0        
      CHRCNT = 0        
      UNKCNT = 0        
      RETURN        
      END        
C        
C     ================================================================= 
C        
      SUBROUTINE DRWLIN (LINSET)        
C        
      IMPLICIT INTEGER (A-Z)        
      REAL     XFACT,YFACT,X,Y,OLDX,OLDY        
      LOGICAL  LINSET,P15,LINPRV        
      COMMON   CMND,CNTRL,R,S,T,U,XFACT,YFACT,XMAX,YMAX,XHI,YHI,XLO,YLO,
     1         PENCHG,PENCNT,OLDPEN,PENNO(99)        
      DATA     LINCNT,P15,LINPRV / 0,.TRUE.,.FALSE. /        
C        
C     CHECK FOR INITIAL COMMAND = 15        
C        
      IF (LINSET) GO TO 1250        
      LINSET = .TRUE.        
      P15    = .FALSE.        
C        
C     CHECK COORDINATES AGAINST MAX & MIN ENCOUNTERED SO FAR        
C        
 1250 XHI  = MAX(R,T,XHI)        
      YHI  = MAX(S,U,YHI)        
      XLO  = MIN(R,T,XLO)        
      YLO  = MIN(S,U,YLO)        
      FILL = 0        
C        
C     CHECK FOR A PEN CHANGE AND/OR NEW PEN ID        
C        
      IF (CNTRL .EQ. OLDPEN) GO TO 1280        
      IF (CNTRL .GT.     31) FILL = 1        
      IF (CNTRL.EQ.0 .AND. OLDPEN .GT.31) IFILL = 1        
      CALL TXNPEN (CNTRL)        
      PENCHG = PENCHG + 1        
      IF (PENCNT .GE. 99) GO TO 1270        
      PENCNT = PENCNT + 1        
      PENNO(PENCNT) = CNTRL        
      IF (PENCNT .LE. 1) GO TO 1270        
      DO 1260 N = 2,PENCNT        
      IF (PENNO(PENCNT) .NE. PENNO(N-1)) GO TO 1260        
      PENCNT = PENCNT - 1        
      GO TO 1270        
 1260 CONTINUE        
 1270 OLDPEN = CNTRL        
 1280 LINCNT = LINCNT + 1        
      X = XFACT*FLOAT(R)        
      Y = YFACT*FLOAT(S)        
      I = 3        
      IF (FILL .EQ. 1) I = -3        
C        
C     IF FILL = 1 THEN START TO FILL ELEMENT        
C        
      IF (.NOT.LINPRV .OR. X.NE.OLDX .OR. Y.NE.OLDY) CALL TXPLOT (X,Y,I)
C        
C     DRAW LINE ON PLOTTER SURFACE        
C        
      X = XFACT*FLOAT(T)        
      Y = YFACT*FLOAT(U)        
C        
C     IF CNTRL = 0 CLOSE ELEMENT AND FILL        
C        
      J = 2        
      IF (CNTRL.EQ.0 .AND. IFILL.EQ.1) J = -2        
      IF (J .EQ. -2) IFILL = 0        
      CALL TXPLOT (X,Y,J)        
C        
C     REMEMBER POSITION FOR NEXT SUCCESSIVE DRAWLINE COMMAND        
C        
      OLDX   = X        
      OLDY   = Y        
      LINPRV = .TRUE.        
      RETURN        
C        
C        
      ENTRY ENDLIN (LINSET)        
C     =====================        
C        
C     CHECK FOR INITIAL COMMAND=15, AND PRINT "DRAW LINES" SUMMARY      
C     REINITIALIZE        
C        
      P15    = .TRUE.        
      LINCNT =  0        
      LINSET = .FALSE.        
      LINPRV = .FALSE.        
      RETURN        
      END        
C        
C     ================================================================= 
C        
      SUBROUTINE TXNPEN (CNTRL)        
C        
C     TEKTRONIX PEN AND COLOR CONTROL        
C        
      INTEGER     CNTRL        
      CHARACTER   ESC*1,CLEAR*2,LINCLR*4        
      COMMON /IO/ IN,NOUT        
      COMMON /EC/ ESC,CLEAR        
C        
C     CHANGE COLOR BY PEN NUMBER.        
C        
      IF (CNTRL .GT. 15) GO TO 1460        
      GO TO (1310,1320,1330,1340,1350,1360,1370,1380,1390,1400,        
     1       1410,1420,1430,1440,1450), CNTRL        
 1310 LINCLR = ESC//'ML1'        
      GO TO 1620        
 1320 LINCLR = ESC//'ML2'        
      GO TO 1620        
 1330 LINCLR = ESC//'ML3'        
      GO TO 1620        
 1340 LINCLR = ESC//'ML4'        
      GO TO 1620        
 1350 LINCLR = ESC//'ML5'        
      GO TO 1620        
 1360 LINCLR = ESC//'ML6'        
      GO TO 1620        
 1370 LINCLR = ESC//'ML7'        
      GO TO 1620        
 1380 LINCLR = ESC//'ML8'        
      GO TO 1620        
 1390 LINCLR = ESC//'ML9'        
      GO TO 1620        
 1400 LINCLR = ESC//'ML:'        
      GO TO 1620        
 1410 LINCLR = ESC//'ML;'        
      GO TO 1620        
 1420 LINCLR = ESC//'ML<'        
      GO TO 1620        
 1430 LINCLR = ESC//'ML='        
      GO TO 1620        
 1440 LINCLR = ESC//'ML>'        
      GO TO 1620        
 1450 LINCLR = ESC//'ML?'        
      GO TO 1620        
C        
 1460 IF (CNTRL .LT. 32) RETURN        
      J = CNTRL - 31        
      GO TO (1470,1480,1490,1500,1510,1520,1530,1540,1550,1560,        
     1       1570,1580,1590,1600,1610), J        
 1470 LINCLR = ESC//'MP!'        
      GO TO 1620        
 1480 LINCLR = ESC//'MP"'        
      GO TO 1620        
 1490 LINCLR = ESC//'MP#'        
      GO TO 1620        
 1500 LINCLR = ESC//'MP$'        
      GO TO 1620        
 1510 LINCLR = ESC//'MP%'        
      GO TO 1620        
 1520 LINCLR = ESC//'MP&'        
      GO TO 1620        
 1530 LINCLR = ESC//'MP'//CHAR(39)        
      GO TO 1620        
 1540 LINCLR = ESC//'MP('        
      GO TO 1620        
 1550 LINCLR = ESC//'MP)'        
      GO TO 1620        
 1560 LINCLR = ESC//'MP*'        
      GO TO 1620        
 1570 LINCLR = ESC//'MP+'        
      GO TO 1620        
 1580 LINCLR = ESC//'MP,'        
      GO TO 1620        
 1590 LINCLR = ESC//'MP-'        
      GO TO 1620        
 1600 LINCLR = ESC//'MP.'        
      GO TO 1620        
 1610 LINCLR = ESC//'MP/'        
 1620 WRITE  (NOUT,1630) LINCLR        
 1630 FORMAT (1X,A4)        
      RETURN        
      END        
C        
C     ================================================================= 
C        
      SUBROUTINE TXINIT        
C        
C     THIS ROUTINE INITIALIZES THE TEKTRONIX GRAPHIC MODE AND CLEARS    
C     THE PREVIOUS PLOT OFF THE SCREEN.        
C        
      CHARACTER   ESC*1,CLEAR*2,INDEX(15)*12        
      CHARACTER*4 TMODE,EMODE,LINCLR        
      COMMON /IO/ IN,NOUT        
      COMMON /EC/ ESC,CLEAR        
C        
      LINCLR = ESC//'ML1'        
      TMODE  = ESC//'%!0'        
C        
C     SET THE COLOR REGISTERS. FROM RED TO BLUE.        
C        
      INDEX( 1) = ESC//'TG1410F40'        
      INDEX( 2) = ESC//'TG142G8B8F4'        
      INDEX( 3) = ESC//'TG143H<B8F4'        
      INDEX( 4) = ESC//'TG144I6C2F4'        
      INDEX( 5) = ESC//'TG145J:B8F4'        
      INDEX( 6) = ESC//'TG146K4C<F4'        
      INDEX( 7) = ESC//'TG147K4D6E0'        
      INDEX( 8) = ESC//'TG148L8B8F4'        
      INDEX( 9) = ESC//'TG149O0C2F4'        
      INDEX(10) = ESC//'TG14:P>B8F4'        
      INDEX(11) = ESC//'TG14;Q8C2F4'        
      INDEX(12) = ESC//'TG14<R7B8F4'        
      INDEX(13) = ESC//'TG14=S;B8F4'        
      INDEX(14) = ESC//'TG14>U1C2F4'        
      INDEX(15) = ESC//'TG14?0C2F4'        
C        
      WRITE  (NOUT,1700) TMODE,CLEAR        
 1700 FORMAT (1X,A4,A2)        
      WRITE  (NOUT,1710) (INDEX(I),I=1,15)        
 1710 FORMAT (1X,A12)        
      WRITE  (NOUT,1700) LINCLR        
      RETURN        
C        
C        
      ENTRY TXFINS        
C     ============        
C        
C     THIS ROUTINE INITIALIZES THE EDIT MODE AND CLEARS THE PREVIOUS    
C     PLOT OFF THE TEKTRONIC SCREEN.        
C        
      LINCLR = ESC//'ML1'        
      EMODE  = ESC//'%!2'        
      WRITE  (NOUT,1720) CLEAR,LINCLR,EMODE        
 1720 FORMAT (1X,A2,A4,A4)        
      RETURN        
      END        
C        
C     ================================================================= 
C        
      SUBROUTINE TXPLOT (X,Y,N)        
C        
C     THIS SUBROUTINE CHANGES COORDINATES INTO TEXTRONIX COORDINATES    
C        
      INTEGER     N,NUMBER(2),CON(12,2),ADE(5),TWO(12)        
      REAL        X,Y        
      CHARACTER   ESC*1,CLEAR*2,PANEND*3,ASC*5        
      CHARACTER*8 PSTART,PEND,PANST        
      COMMON /IO/ IN,NOUT        
      COMMON /EC/ ESC,CLEAR        
      DATA   TWO/ 1,2,4,8,16,32,64,128,256,512,1024,2048 /        
C        
      IX = NINT(X*3071.0)        
      IY = NINT(Y*3071.0)        
C        
      NUMBER(1) = IX        
      NUMBER(2) = IY        
C        
C     CONVERTS TWO INTEGER VALUES INTO TEKTRONIX TYPE ADE (ASCII TO     
C     DECIMAL EQUIVALENT) CHARACTERS        
C        
      DO 1820 K = 1,2        
      IDIGIT = 0        
C        
      I = 12        
      DO 1810 J = 1,12        
      IF (TWO(I)-NUMBER(K) .GT. 0) GO TO 1800        
      IDIGIT = 1        
      NUMBER(K) = NUMBER(K) - TWO(I)        
 1800 CON(I,K) = IDIGIT        
      IDIGIT = 0        
 1810 I = I - 1        
 1820 CONTINUE        
C        
      ADE(1) = 32        
      ADE(2) = 96        
      ADE(3) = 96        
      ADE(4) = 32        
      ADE(5) = 64        
C        
      I = 12        
      DO 1830 J = 8,12        
      IF (CON(I,1) .EQ. 1) ADE(4) = ADE(4) + TWO(I-7)        
 1830 I = I - 1        
      ASC(4:4) = CHAR(ADE(4))        
C        
      I = 7        
      DO 1840 J = 3,7        
      IF (CON(I,1) .EQ. 1) ADE(5) = ADE(5) + TWO(I-2)        
 1840 I = I - 1        
      ASC(5:5) = CHAR(ADE(5))        
C        
      I = 2        
      DO 1850 J = 1,2        
      IF (CON(I,1) .EQ. 1) ADE(2) = ADE(2) + TWO(I)        
 1850 I = I - 1        
C        
C     IS THERE A LINE MISSING HERE? SOMETHING SUCH AS        
C     ASC(?:?) = CHAR(ADE(2))???   G.C  9/90        
C        
      I = 12        
      DO 1860 J = 8,12        
      IF (CON(I,2) .EQ. 1) ADE(1) = ADE(1) + TWO(I-7)        
 1860 I = I - 1        
      ASC(1:1) = CHAR(ADE(1))        
C        
      I = 7        
      DO 1870 J = 3,7        
      IF (CON(I,2) .EQ. 1) ADE(3) = ADE(3) + TWO(I-2)        
 1870 I = I - 1        
      ASC(3:3) = CHAR(ADE(3))        
C        
      I = 2        
      DO 1880 J = 1,2        
      IF (CON(I,2) .EQ. 1) ADE(2) = ADE(2) + TWO(I)        
 1880 I = I - 1        
      ASC(2:2) = CHAR(ADE(2))        
C        
C     END OF CONVERSION        
C        
      PSTART = ESC//'LF'//ASC        
      PEND   = ESC//'LG'//ASC        
      PANST  = ESC//'LP'//ASC        
      PANEND = ESC//'LE'        
C        
      IF (N .EQ.  3) WRITE (NOUT,1890) PSTART        
      IF (IABS(N) .EQ. 2) WRITE (NOUT,1890) PEND        
      IF (N .EQ. -3) WRITE (NOUT,1890) PANST        
      IF (N .EQ. -2) WRITE (NOUT,1900) PANEND        
 1890 FORMAT (1X,A8)        
 1900 FORMAT (1X,A3)        
      RETURN        
      END        
C        
C     ================================================================= 
C        
      SUBROUTINE PSINIT (NOLINE)        
C        
C     POSTSCRIPT NEW PLOT ROUTINE        
C        
C     ON A 8 X 11 (HORIZ X VERT) PAGE, THE ORIGIN IS AT THE LOWER LEFT  
C     CONNER. NOW TO CENTER THE PLOT HORIZONTALLY ALONG THE 11 IN. EDGE,
C     MOVE THE ORIGIN 589 UNITS TO THE RIGHT, 116 UNITS UP, AND ROTATE  
C     THE PAPER BY 90 DEGREE.        
C        
      COMMON /PS/ LU,DUMMY(6)        
C        
      IF (NOLINE .EQ. 0) WRITE (LU,1950)        
      WRITE  (LU,1960)        
 1950 FORMAT ('%!')        
 1960 FORMAT ('589 116 translate', /,'90 rotate')        
      RETURN        
C        
C        
      ENTRY PSFINS        
C     ============        
C        
C     CLOSE ROUTINE        
C        
      WRITE  (LU,1970)        
 1970 FORMAT ('showpage')        
      RETURN        
C        
C        
      ENTRY PSTRKE        
C     ============        
C        
C     STROKE ROUTINE        
C        
      WRITE  (LU,1980)        
 1980 FORMAT ('stroke')        
      RETURN        
      END        
C        
C     ================================================================= 
C        
      SUBROUTINE PSLINE        
C        
C     POSTSCRIPT LINE ROUTINE        
C        
      INTEGER     R,S,T,U        
      REAL        SCALE        
      CHARACTER*7 MT,LT        
      COMMON      CMND,CTRL,R,S,T,U        
      COMMON /PS/ LU,SCALE,JOUNT,NBUFF,DUMMY(3)        
      DATA    MT, LT /' moveto', ' lineto' /        
C        
      R = R*SCALE        
      S = S*SCALE        
      T = T*SCALE        
      U = U*SCALE        
      WRITE  (LU,2000) R,S,MT, T,U,LT        
 2000 FORMAT (2I5,A7, /,2I5,A7)        
      JOUNT = JOUNT + 1        
      IF (JOUNT .LE. NBUFF) RETURN        
      CALL PSTRKE        
      JOUNT = 0        
      RETURN        
      END        
C        
C     ================================================================= 
C        
      SUBROUTINE PSCHAR (CHRSET,CMNDX)        
C        
C     PUT CHARACTERS INTO ONE STRING IF APPLICABLE. UP TO 70 CHARACTERS.
C        
C     IF CMNDS=4 AT 2120 IS REMOVED, NO CHARACTER STRING WILL BE FORMED,
C     AND EACH CHARACTER WILL BE POSITIONED AND TYPED INDIVIDUALLY      
C        
      IMPLICIT INTEGER (A-Z)        
      LOGICAL     CHRSET        
      REAL        SCALE,CSCAL        
      CHARACTER   CHR94*48,BSS0*2        
      CHARACTER*1 SAVE(76),SHOW(6),BS,S0,S2,S3,S5,S6,S7,BS0(2)        
      COMMON      CMND,CNTRL,R,S,T        
      COMMON /PS/ LU,SCALE,JOUNT,NBUFF,ONCE,CSCAL,CHRPOS        
      EQUIVALENCE (BS0(1),BSS0,BS),(BS0(2),S0)        
      DATA        SHOW  / ')',' ','s','h','o','w' /        
      DATA        BSS0,S2,S3,S5,S6,S7 / '\0','2','3','5','6','7'/       
      DATA        CHR94 /        
     1           '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ()+-*/=.,$" ' /   
C                 000000000111111111122222222223333333333444444444      
C                          0         0         0         0       8      
C                 49-52 = SPECIAL CHARACTERS        
C                 FROM NASTRAN USER NANUAL P. 4.4-5        
C        
      IF (ONCE .NE. -1) GO TO 2110        
      ONCE = 0        
      WRITE  (LU,2100) CSCAL        
 2100 FORMAT ('/Courier-Bold findfont',F5.1,' scalefont setfont')       
C        
C     CORRECTION FOR CHARACTER X-Y POSITION.        
C     FROM CHARACTER CENTER TO CHARACTER LOWER LEFT CORNER        
C        
      DELY = 8*T/2 + 1        
      DELX = DELY*2/3 + 1        
      GO TO 2120        
C        
 2110 IF (CMNDX .EQ. 14) GO TO 2200        
      IF (ONCE  .GT.  0) GO TO 2140        
 2120 IF (CHRPOS .EQ. 0) CMNDX = 4        
      R  = (R-DELX)*SCALE        
      S  = (S-DELY)*SCALE        
      WRITE  (LU,2130) R,S        
 2130 FORMAT (2I5,' moveto')        
      JOUNT = JOUNT + 1        
      COUNT = 0        
      ONCE  = 1        
C        
 2140 COUNT = COUNT + 1        
      SAVE(COUNT) = SHOW(2)        
      IF (CNTRL.GE.49 .AND. CNTRL.LE.52) GO TO 2150        
      IF (COUNT.LE.70 .AND. CNTRL.GE.1 .AND. CNTRL.LE.48)        
     1    SAVE(COUNT) = CHR94(CNTRL:CNTRL)        
      GO TO 2170        
C        
C     SPECIAL SYMBOLS (USER'S MANUAL P. 4.4-5)        
C                 NASTRAN         POSTCSRIPT        
C     CNTRL       ORIGINAL        EQUIVALENCE        
C     -----     -------------    --------------        
C       49      SMALL CIRCLE     BIG SOLID DOT        
C       50      SMALL SQUARE     SQUATE-CIRCLE        
C       51      SMALL DIAMOND    DAGGER        
C       52      SMALL TRIANGLE   DOUBLE-DAGGER        
C        
C     SINCE THESE SYMBOLS MAY BE PART OF A TEXT, TO ACTUALLY DRAW THESE 
C     NASTRAN ORIGINAL SYMBOLS IS NOT RECOMMENDED        
C        
 2150 IF (COUNT+3 .GT. 70) GO TO 2170        
      SAVE(COUNT  ) = BS        
      SAVE(COUNT+1) = S2        
      SAVE(COUNT+2) = S6        
      COUNT = COUNT + 3        
      IF (CNTRL .NE. 50) GO TO 2160        
      SAVE(COUNT-1) = S5        
      SAVE(COUNT  ) = S0        
      GO TO 2170        
 2160 IF (CNTRL .EQ. 49) SAVE(COUNT) = S7        
      IF (CNTRL .EQ. 51) SAVE(COUNT) = S2        
      IF (CNTRL .EQ. 52) SAVE(COUNT) = S3        
 2170 RETURN        
C        
C        
      ENTRY PSENDC (CHRSET,CMNDX)        
C     ===========================        
C        
 2200 IF (COUNT-1) 2270,2210,2230        
 2210 WRITE  (LU,2220) SAVE(1)        
 2220 FORMAT (1H(,A1,6H) show)        
      GO TO 2260        
 2230 DO 2240 J = 1,6        
 2240 SAVE(COUNT+J) = SHOW(J)        
      COUNT = COUNT + 6        
      WRITE  (LU,2250) (SAVE(J),J=1,COUNT)        
 2250 FORMAT (1H(,76A1)        
 2260 COUNT = 0        
      ONCE  = 0        
      JOUNT = JOUNT + 1        
      IF (JOUNT .LE. NBUFF) GO TO 2270        
      CALL PSTRKE        
      JOUNT = 0        
 2270 IF (CMNDX.EQ.14 .AND. CMND.EQ.4) GO TO 2120        
      CHRSET = .FALSE.        
      RETURN        
      END        
C        
C     ================================================================= 
C        
C+    SUBROUTINE NASTEK (*,FIL,NR,IBUFF,NBUFF)        
C        
C     NASTEK - INTERACTIVE DISPLAY OF NASTRAN-GENERATED PLOTS - NOV83   
C                                                       REVISED APR86   
C        
C     NASTEK PLOTS NASTRAN-GENERATED PLT1 OR PLT2 FILES ON TEKTRONIX    
C     4010 AND 4050 SERIES TERMINALS, AND RUNS ON VAX-11 COMPUTERS.     
C        
C     THE USER CAN        
C           - PLOT ALL FRAMES SEQUENTIALLY W/AUTOMATIC HARDCOPY OR PLOT 
C             ANY FRAME HE CHOOSES        
C           - SET A SCALE FACTOR TO SHRINK OR ENLARGE THE PLOTS        
C           - DRAW WITH SOLID, DASHED, OR DOTTED LINES IF THE PEN       
C             PARAMETER WAS USED IN NASTRAN        
C        
C     INPUT - THE NASTRAN-GENERATED PLT1 OR PLT2 FILE.        
C           - FIVE USER-DEFINED VALUES WHICH ARE INPUT BY THE USER      
C             IN RESPONSE TO QUESTIONS ASKED BY NASTEK.        
C        
C     NASTEK IS WRITTEN IN VAX FORTRAN 77 WITH SUBROUTINES FROM THE     
C     TEKTRONIX PLOT-10 TCS AND ADVANCED GRAPHING II PACKAGES.        
C        
C     VARIABLES        
C     CHANGE - CHANGING PLOT OPTION?        
C     CHAR   - CHARACTER ARRAY        
C     CI     - NASTRAN CONTROL INDEX        
C     CSIZE  - CHARACTER SIZE FOR QUESTIONS, NOTES, ETC        
C     CURFR  - COUNTER FOR CURRENT FRAME NUMBER IN PLT1 OR PLT2 FILE    
C     FR     - USER FRAME NUMBER TO BE PLOTTED        
C     IB     - INDEX FOR IBUFF        
C     IBUFF  - USED TO READ IN AND UNPACK PLT1 OR PLT2 FILE        
C     NBUFF  - LENGTH OF IBUFF ARRAY, EITHER 30 OR 3000        
C     LEN2X  - HALF THE WIDTH  OF FRAME IN SCREEN COORDINATES        
C     LEN2Y  - HALF THE HEIGHT OF FRAME IN SCREEN COORDINATES        
C     MAXFR  - MAXIMUM NUMBER OF FRAMES IN PLT1 OR PLT2 FILE        
C     MINX,MINY,MAXX,MAXY,MMINX,MMINY,MMAXX,MMAXY - SCREEN COORDINATES  
C     NUM    - STRING OF ASCII NUMBERS USED TO OUTPUT A NUMBER        
C     OLDT,OLDU - LAST POSITION DRAWN TO        
C     PC     - NASTRAN PLOT COMMAND  0 - NO OPERATION        
C                                    1 - START NEW PLOT        
C                                  2,3 - NOT USED IN NASTEK        
C                                    4 - PRINT A CHARACTER        
C                                  5,6 - DRAW A LINE        
C     PLOTID - HAS PLOTID FRAME BEEN PLOTTED        
C     PLOTOP - PLOT OPTION, 1 CHOOSING, 2 AUTOMATIC        
C     FIL    - NASTRAN-GENERATED PLOT FILE, PLT1 OR PLT2        
C     NR     - NO. OF PLOT COMMANDS PER FILE RECORD        
C            = 5 FOR PLT1, AND 100 FOR PLT2 FILE        
C     R,S,T,U,RSTU - NASTRAN COORDINATES        
C     RMIN,SMIN,RMAX,SMAX - NASTRAN COORDINATES        
C     SCALE  - SCALE FACTOR > 0        
C     SEARCH - SEARCHING FOR A FRAME?        
C     SIZE   - CHARACTER SIZE ON PLOTS        
C     SYM    - SYMBOL SIZES IN NASTRAN COORDINATES        
C     TERMT  - TERMINAL TYPE        
C     XCEN,YCEN - CENTER OF FRAME IN SCREEN COORDINATES        
C     XCS,YCS   - 1/2 WIDTH AND HEIGHT FOR CHARACTER IN NASTRAN        
C              COORDINATES USED TO MOVE THE CHARACTERS DOWN AND TO THE  
C              LEFT SO THAT THEY ARE CENTERED WRT THE NASTRAN COORDINATE
C              GIVING THEIR LOCATION        
C     XFAC,YFAC - SCALE FACTORS FOR CHARACTER SIZE DEPENDING ON USER    
C              SCALE FACTOR        
C        
C     WRITTEN BY: ROBERT LIPMAN        
C                 DAVID TAYLOR NAVAL SHIP RESEARCH AND DEVELOPMENT      
C                 CENTER, NUMERICAL STRUCTURAL MECHANICS BRANCH        
C                 CODE 1844        
C                 BETHESDA, MARYLAND  20084-5000        
C                 (202)-227-1922        
C        
C     MODIFIED BY G.CHAN/UNISYS IN AREAS WHERE PLOT COMMANDS ARE READ   
C     OFF PLT1 OR PLT2 FILE, IN DECODING THE COMMANDS, AND IN REPLACING 
C     THE IF-THEN-ELSE-ENDIF STUFFS BY THE OLD STYLE SIMPLE 'IF' FORTRAN
C     THE FANZY IF-THEN-ELSE-ENDIF STUFFS ARE EASY ONLY TO THE ORIGINAL 
C     DEVELOPER, AND IS VERY HARD FOR ANOTHER READER TO FOLLOW. UPDATING
C     THE PROGRAM BECOMES VERY DIFFICULT.  3/93        
C 
C+    LOGICAL        SEARCH,CHANGE,PLOTID        
C+    INTEGER        IBUFF(NBUFF),PC,CI,PLOTOP,FR,CURFR,PLTX,PLT1,PLT2, 
C+   1               FIL,XCEN,YCEN,CSIZE,TERMT        
C+    REAL           RSTU(4),CHAR(48)        
C+    COMMON /FRAME/ MMINX,MMINY,MMAXY,MMAXX,RSTU,SCALE,CURFR,        
C+   1               XCEN,YCEN,RMIN,RMAX,SMIN,SMAX,TERMT,XCS,YCS,       
C+   2               SYM1,SYM2,SYM3,SYM7,CSIZE        
C+    EQUIVALENCE    (RSTU(1),R),(RSTU(2),S),(RSTU(3),T),(RSTU(4),U),   
C+   1               (NO,CHAR(24))        
C+    DATA    CHAR /        
C+   1               1H0, 1H1, 1H2, 1H3, 1H4, 1H5, 1H6, 1H7, 1H8, 1H9,  
C+   2               1HA, 1HB, 1HC, 1HD, 1HE, 1HF, 1HG, 1HH, 1HI, 1HJ,  
C+   3               1HK, 1HL, 1HM, 1HN, 1HO, 1HP, 1HQ, 1HR, 1HS, 1HT,  
C+   4               1HU, 1HV, 1HW, 1HX, 1HY, 1HZ, 1H(, 1H), 1H+, 1H-,  
C+   5               1H*, 1H/, 1H=, 1H., 1H,, 1H$, 1H', 1H   /        
C+    DATA    NOUT , IN / 6,5/,  PLT1,PLT2,NN / 4HPLT1, 4HPLT2, 1Hn  /  
C        
C+    PLTX = PLT1        
C+    IF (NR .EQ. 100) PLTX = PLT2        
C+    NCOM = NBUFF/NR        
C        
C     INITIALIZE THE TERMINAL        
C        
C+ 10 WRITE  (NOUT,20)        
C+ 20 FORMAT (/,' ENTER THE TERMINAL BAUD RATE? ',$)        
C+    READ (IN,30,ERR=10) IBAUD        
C+ 30 FORMAT (I5)        
C+    IF (IBAUD.NE.300  .AND. IBAUD.NE.1200 .AND. IBAUD.NE.4800 .AND.   
C+   1    IBAUD.NE.9600 .AND. IBAUD.NE.109200) GO TO 10        
C+    CALL INITT (IBAUD/10)        
C+    CALL TERM  (3,4096)        
C        
C     INITIALIZE SOME VARIABLES        
C        
C+    CHANGE =.FALSE.        
C+    OLDT   = 1.E30        
C+    OLDU   = 1.E30        
C        
C     SET MAXIMUM SIZE OF FRAME IN SCREEN COORDINATES        
C     3014/3900 = 8.5/11        
C        
C+    MMINX = 0        
C+    MMINY = 0        
C+    MMAXX = 3900        
C+    MMAXY = 3014        
C+    XCEN  = (MMAXX + MMINX)/2        
C+    YCEN  = (MMAXY + MMINY)/2        
C        
C     START OUTPUT        
C        
C+    CALL CHRSIZ (4)        
C+    TERMT = 0        
C+ 40 CALL NEWPAG        
C+    IF (TERMT .NE. 0) CALL CHRSIZ (CSIZE)        
C+    CALL ANMODE        
C        
C     PRINT SOME NOTES        
C        
C+    IF (TERMT .NE. 0) GO TO 80        
C        
C     INSERT NOTES HERE        
C        
C     GET TERMINAL TYPE FROM USER        
C        
C+ 50 WRITE  (NOUT,60)        
C+ 60 FORMAT (/,' ENTER THE TEKTRONIX TERMINAL TYPE', /,        
C+   1           ' 1 - 4014, 4015, 4054', /,        
C+   2           ' 2 - 4016', /,        
C+   3           ' 3 - 4010, 4012, 4013, 4051, 4052', /,' ? ',$)        
C+    READ (IN,70,ERR=50) TERMT        
C+ 70 FORMAT (I1)        
C+    IF (TERMT.EQ.0 .OR. TERMT.GE.4) GO TO 50        
C+    IF (TERMT .EQ. 1) CSIZE = 4        
C+    IF (TERMT .EQ. 2) CSIZE = 3        
C+    IF (TERMT .EQ. 3) CSIZE = 1        
C+    IF (TERMT .LE. 2) CALL CHRSIZ (CSIZE)        
C        
C+ 80 REWIND FIL        
C+    IB     = NBUFF        
C+    CURFR  =-1        
C+    MAXFR  = 100000        
C+    PLOTID = .FALSE.        
C+    SEARCH = .TRUE.        
C+    FR     = 0        
C        
C     GET PLOTTING OPTION FROM USER        
C        
C+ 90 WRITE  (NOUT,100)        
C+100 FORMAT (/,' ENTER A PLOTTING OPTION', /,        
C+   1          ' 1 - PLOT ANY FRAME OF YOUR CHOICE', /,        
C+   2          ' 2 - PLOT AUTOMATICALLY, ALL FRAMES SEQUENTIALLY WITH',
C+   3          ' HARDCOPY', /,' ? ',$)        
C+    READ (IN,70,ERR=90) PLOTOP        
C+    IF (PLOTOP.NE.1 .AND. PLOTOP.NE.2) GO TO 90        
C        
C     PRINT NOTES        
C        
C+    IF (PLOTOP .EQ. 1) WRITE (NOUT,110)        
C+110 FORMAT (/,' NOTE - WHEN ASKED ''PLOT FRAME NUMBER ?'' ENTER',     
C+   1        /9X,'ANY FRAME NUMBER TO PLOT THAT FRAME OR',        
C+   2        /9X,' 0 TO PLOT THE NEXT FRAME, OR',        
C+   3        /8X,'-1 TO RESTART NASTEK, OR',        
C+   4        /8X,'-2 TO END NASTPLOT')        
C+    IF (PLOTOP .EQ. 2) WRITE (NOUT,120)        
C+120 FORMAT (/' NOTE - AFTER ALL FRAMES ARE PLOTTED THE PROGRAM',      
C+   1        /8X,'WILL ASK ''RESTART NASTPLOT (Y/N) ?''')        
C        
C     GET SCALE FACTOR FROM USER        
C        
C+130 WRITE  (NOUT,140)        
C+140 FORMAT (/' ENTER A SCALE FACTOR (SCALE>0)',        
C+   1        /' =1 NORMAL',        
C+   2        /' >1 ENLARGE ALL FRAMES',        
C+   3        /' <1 SHRINK ALL FRAMES',/,' ? ',$)        
C+    READ (IN,150,ERR=130) SCALE        
C+150 FORMAT (F7.3)        
C+    IF (SCALE.LT.0.01 .OR. SCALE.GT.999.) GO TO 130        
C        
C     BRANCH DEPENDING ON IF RESTARTED AND PLOT OPTION        
C        
C+    IF (.NOT.CHANGE) GO TO 160        
C+    CHANGE = .FALSE.        
C+    IF (PLOTOP .EQ. 1) GO TO 340        
C        
C     READ PLT1 OR PLT2 FILE AND UNPACK ONE RECORD WITH NR COMMNADS     
C        
C+160 IF (IB .LE. NBUFF) GO TO 280        
C+    IB = 1 - NCOM        
C+    READ (FIL,170,END=180,ERR=260) IBUFF        
C+170 FORMAT (3000A1)        
C+    GO TO 280        
C        
C     EOF FOUND ON PLOT FILE, REWIND IT IF NOT EMPTY        
C        
C+180 IF (MAXFR .EQ. 100000) MAXFR = CURFR        
C+    IF (MAXFR .NE. -1) GO TO 200        
C+    WRITE  (NOUT,190) PLTX        
C+190 FORMAT (/,' *ERROR*  THIS IS NOT A ',A4,' FILE, TRY AGAIN')       
C+    RETURN 1        
C        
C+200 REWIND FIL        
C+    CURFR = -1        
C+    IB    = NBUFF        
C        
C     EOF ON PLOT FILE, PLOTOP=1 AND SEARCHING THEN FRAME NOT FOUND,    
C     TRY AGAIN        
C        
C+    IF (PLOTOP .EQ. 2) GO TO 220        
C+    IF (.NOT.SEARCH) GO TO 340        
C+    SEARCH = .FALSE.        
C+    WRITE  (NOUT,210) MAXFR        
C+210 FORMAT (25X,'MAXIMUM FRAME NUMBER IS',I4,', TRY AGAIN')        
C+    GO TO 340        
C        
C     EOF ON PLOT FILE, PLOTOP=2, OPTION TO RESTART NASTPLOT        
C        
C+220 CALL BELL        
C+    CALL HDCOPY        
C+    CALL MOVABS (0,3120)        
C+    IF (TERMT .LE. 2) CALL CHRSIZ (CSIZE)        
C+    CALL ANMODE        
C+230 WRITE  (NOUT,240)        
C+240 FORMAT (' RESTART NASTPLOT (Y/N) ? ',$)        
C+    READ   (IN,250,ERR=230) NY        
C+250 FORMAT (A1)        
C+    IF (NY.EQ.NO .OR. NY.EQ.NN) GO TO 500        
C+    GO TO 40        
C        
C     ERROR WHILE READING PLT1 OR PLT2 FILE        
C        
C+260 WRITE  (NOUT,270) PLTX        
C+270 FORMAT (/' *ERROR*  READING ',A4,'  FILE')        
C+    GO TO 500        
C        
C     GET A PLOT COMMAND AND DECIDE IF ITS OK        
C        
C+280 IB = IB + NCOM        
C+    PC = IBUFF(IB)        
C+    IF (PC.LE.0 .OR. PC.GE.17 .OR. (PC.NE.1.AND.SEARCH)) GO TO 160    
C        
C     GET CONTROL INDEX, RSTU        
C        
C+    CI = IBUFF(IB+1)        
C+    IF (PLTX .EQ. PLT1) GO TO 310        
C+    J1 = 3        
C+    DO 300 I = 1,4        
C+    J2 = J1 + 4        
C+    RSTU(I) = 0.0        
C+    DO 290 J = J1,J2        
C+    RSTU(I) = RSTU(I)*10. + IBUFF(J+IB)        
C+290 CONTINUE        
C+300 J1 = J1 + 5        
C+    GO TO 320        
C        
C+310 R = IBUFF(IB+2)        
C+    S = IBUFF(IB+3)        
C+    T = IBUFF(IB+4)        
C+    U = IBUFF(IB+5)        
C        
C     BRANCH ON A PLOT COMMAND        
C        
C+320 GO TO (330,160,160,430,490,490,160,160,160,160,        
C+   1       160,160,160,430,490,490,500), PC        
C        
C     START OF A NEW PLOT IN PLOT FILE        
C        
C+330 CURFR = CURFR + 1        
C        
C     IF SEARCHING FOR A FRAME, HAVE WE FOUND IT?        
C        
C+    IF (.NOT.SEARCH) GO TO 340        
C        
C     FRAME HAS BEEN FOUND, START PLOTTING, PLOTID FRAME IS PLOTTED ONCE
C     IF FRAME NOT FOUND, GO TO 40        
C        
C+    IF (CURFR .NE. FR) GO TO 160        
C+    SEARCH = .FALSE.        
C+    IF (CURFR .NE. 0) GO TO 420        
C+    IF (PLOTID) GO TO 340        
C+    PLOTID =.TRUE.        
C+    GO TO 420        
C        
C     NOT SEARCHING FOR A FRAME, PREVIOUS PLOT IS FINISHED        
C     PLOTOP=1, GET FRAME NUMBER FROM USER        
C        
C+340 IF (PLOTOP .EQ. 2) GO TO 410        
C+    CALL BELL        
C+    CALL MOVABS (0,3120)        
C+    IF (TERMT .LE. 2) CALL CHRSIZ (CSIZE)        
C+350 CALL ANMODE        
C+    WRITE  (NOUT,360)        
C+360 FORMAT (' PLOT FRAME NUMBER ? ',$)        
C+    READ (IN,370,ERR=350) FR        
C+370 FORMAT (I2)        
C+    IF (FR.EQ.0 .AND. CURFR.EQ.-1) FR = 1        
C+    IF (.NOT.(FR.LT.-2 .OR. FR.GT.MAXFR)) GO TO 390        
C+    WRITE  (NOUT,380)        
C+380 FORMAT (' TRY AGAIN')        
C+    CALL BELL        
C+    GO TO 350        
C        
C     BRANCH DEPENDING ON NEW FRAME NUMBER        
C     -1 - RESTART NASTEK        
C     -2 - END NASTEK        
C      0 OR CURRENT FRAME NUMBER - PLOT THAT FRAME        
C     ELSE SEARCH FOR THE FRAME        
C        
C+390 IF (FR .EQ.  0) FR = CURFR        
C+    IF (FR .NE. -1) GO TO 400        
C+    CHANGE = .TRUE.        
C+    GO TO 40        
C+400 IF (FR .EQ. -2) GO TO 500        
C+    IF (FR .EQ. CURFR) GO TO 420        
C+    SEARCH =.TRUE.        
C+    IF (FR .GE. CURFR) GO TO 160        
C+    CURFR = -1        
C+    REWIND FIL        
C+    IB = 1 - NCOM        
C+    GO TO 160        
C        
C     PLOTOP = 2, MAKE HARDCOPY        
C        
C+410 CALL BELL        
C+    CALL HDCOPY        
C        
C     START PLOTTING A FRAME BY DRAWING A BOX        
C        
C+420 CONTINUE
C+    CALL PFRAME        
C+    GO TO 160        
C        
C     PRINT A CHARACTER OR SYMBOL BASED ON THE CONTROL INDEX,        
C     DON'T PRINT IT IF IT IS OUTSIDE THE BOX        
C        
C+430 IF (CI.LT.1 .OR. CI.GT.48) GO TO 440        
C+    R = R - XCS        
C+    S = S - YCS        
C+    IF (R.LT.RMIN .OR. R.GT.RMAX .OR. S.LT.SMIN .OR. S.GT.SMAX)       
C+   1    GO TO 160        
C+    CALL MOVEA (R,S)        
C+    CALL A1OUT (1,CHAR(CI))        
C+440 IF (CI .GT. 52) GO TO 160        
C+    IF (R.LT.RMIN .OR. R.GT.RMAX .OR. S.LT.SMIN .OR. S.GT.SMAX)       
C+   1    GO TO 160        
C+    CALL MOVEA (R,S)        
C+    IF (CI-50) 450,460,470        
C        
C     CIRCLE SYMBOL        
C        
C+450 CALL MOVER (-SYM1,   0.)        
C+    CALL DRAWR ( SYM3, SYM7)        
C+    CALL DRAWR ( SYM7, SYM3)        
C+    CALL DRAWR ( SYM7,-SYM3)        
C+    CALL DRAWR ( SYM3,-SYM7)        
C+    CALL DRAWR (-SYM3,-SYM7)        
C+    CALL DRAWR (-SYM7,-SYM3)        
C+    CALL DRAWR (-SYM7, SYM3)        
C+    CALL DRAWR (-SYM3, SYM7)        
C+    GO TO 160        
C        
C     SQUARE SYMBOL        
C        
C+460 CALL MOVER (-SYM1, SYM1)        
C+    CALL DRAWR ( SYM2,   0.)        
C+    CALL DRAWR (   0.,-SYM2)        
C+    CALL DRAWR (-SYM2,   0.)        
C+    CALL DRAWR (   0., SYM2)        
C+    GO TO 160        
C        
C     DIAMOND SYMBOL        
C        
C+470 IF (CI .EQ. 52) GO TO 480        
C+    CALL MOVER (   0., SYM1)        
C+    CALL DRAWR ( SYM1,-SYM1)        
C+    CALL DRAWR (-SYM1,-SYM1)        
C+    CALL DRAWR (-SYM1, SYM1)        
C+    CALL DRAWR ( SYM1, SYM1)        
C+    GO TO 160        
C        
C     TRIANGLE SYMBOL        
C        
C+480 CALL MOVER (   0., SYM1)        
C+    CALL DRAWR ( SYM1,-SYM2)        
C+    CALL DRAWR (-SYM2,   0.)        
C+    CALL DRAWR ( SYM1, SYM2)        
C+    GO TO 160        
C        
C     DRAW LINE DEPENDING ON CONTROL INDEX, NO MOVE IF OLD END IS NEW   
C     START        
C     CI = 1, SOLID,      PEN 1        
C        = 2, DOTTED,     PEN 2        
C        = 3, DASH-DOT,   PEN 3        
C        = 4, SHORT-DASH, PEN 4        
C        = 5, LONG-DASH,  PEN 5        
C        
C+490 IF (CI.LT.1 .OR. CI.GT.5) CI = 1        
C+    IF (OLDT.NE.R .OR. OLDU.NE.S) CALL MOVEA (R,S)        
C+    CALL DASHA (T,U,CI-1)        
C+    OLDT = T        
C+    OLDU = U        
C+    GO TO 160        
C        
C     JOB DONE        
C        
C+500 RETURN        
C+    END        
C        
C     ================================================================= 
C        
C+     SUBROUTINE PFRAME        
C        
C     THIS ROUTINE IS CALLED ONLY BY NASTEK        
C        
C     GIVEN THE NASTRAN SIZE OF THE FRAME (S,T), FIT A FRAME ON THE     
C     TEKTRONIX SCREEN AND DRAW A BOX AROUND IT        
C        
C+    INTEGER        CURFR,XCEN,YCEN,NUM(4),SIZE,TERMT        
C+    REAL           RSTU(4),XFAC(4),YFAC(4)        
C+    COMMON /FRAME/ MMINX,MMINY,MMAXY,MMAXX,RSTU,SCALE,CURFR,        
C+   1               XCEN,YCEN,RMIN,RMAX,SMIN,SMAX,TERMT,XCS,YCS,       
C+   2               SYM1,SYM2,SYM3,SYM7,CSIZE        
C+    EQUIVALENCE    (RSTU(1),R),(RSTU(2),S),(RSTU(3),T),(RSTU(4),U)    
C+    DATA    XFAC / 1.9 ,1.64,1.1 ,1./        
C+    DATA    YFAC / 1.94,1.68,1.12,1./        
C        
C     (1) FIND SIZE OF STANDARD BOX        
C        
C+    MINX = MMINX        
C+    MINY = MMINY        
C+    MAXY = MMAXY        
C+    IF (T .NE. 0.) GO TO 10        
C+    WRITE (NOUT,15) S,T        
C+    GO TO 120        
C+ 10 MAXX = MINX + MAXY*(S/T)        
C+    IF (MAXX .LE. MMAXX) GO TO 30        
C+    MAXX = MMAXX        
C+    IF (S .NE. 0.) GO TO 20        
C+    WRITE  (NOUT,15) S,T        
C+ 15 FORMAT (/,' *ERROR*  EITHER S=0 OR T=0',2(1P,E9.2))        
C+    GO TO 120        
C+ 20 MAXY  = MAXX*(T/S)        
C+ 30 LEN2X = (MAXX-MINX)/2        
C+    LEN2Y = (MAXY-MINY)/2        
C        
C     (2) SCALE AND CENTER THE BOX        
C        
C+    IF (SCALE.GT.1. .AND. CURFR.NE.0) GO TO 40        
C+    TEMP = 1.        
C+    IF (CURFR .NE. 0) TEMP = SCALE        
C+    MINX = XCEN - LEN2X*TEMP        
C+    MAXX = XCEN + LEN2X*TEMP        
C+    MINY = YCEN - LEN2Y*TEMP        
C+    MAXY = YCEN + LEN2Y*TEMP        
C+    RMIN = 0.        
C+    RMAX = S        
C+    SMIN = 0.        
C+    SMAX = T        
C+    GO TO 50        
C+ 40 IF (SCALE .LE. 1.) GO TO 50        
C+    MINX = XCEN - LEN2X        
C+    MAXX = XCEN + LEN2X        
C+    MINY = YCEN - LEN2Y        
C+    MAXY = YCEN + LEN2Y        
C+    RMIN = S*(1.-1./SCALE)/2.        
C+    RMAX = S*(1.+1./SCALE)/2.        
C+    SMIN = T*(1.-1./SCALE)/2.        
C+    SMAX = T*(1.+1./SCALE)/2.        
C        
C     (3) DRAW A BOX AROUND THE FRAME, AND SET SCREEN AND VIRTUAL WINDOW
C        
C+ 50 CALL ANMODE        
C+    CALL NEWPAG        
C+    IF (.NOT.(MAXX.LE.MINX .OR. MAXY.LE.MINY .OR. RMAX.LE.RMIN .OR.   
C+   1    SMAX.LE.SMIN)) GO TO 60        
C+    CALL ANMODE        
C+    WRITE  (NOUT,55) MINX,MAXX,MINY,MAXY,RMIN,RMAX,SMIN,SMAX        
C+ 55 FORMAT (/,' *ERROR*  SOME OF THE MINS>=MAXS',/4I9,/4(1P,E9.3))    
C+ 60 IF (MAXX .NE. MINX) GO TO 70        
C+    CALL ANMODE        
C+    WRITE  (NOUT,65) MAXX,MINX        
C+ 65 FORMAT (/,' *ERROR*  MAXX=MINX',2I6)        
C+    GO TO 120        
C+ 70 CALL DWINDO (RMIN,RMAX,SMIN,SMAX)        
C+    CALL TWINDO (MINX,MAXX,MINY,MAXY)        
C+    CALL MOVABS (MINX,MINY)        
C+    CALL DRWABS (MINX,MAXY)        
C+    CALL DRWABS (MAXX,MAXY)        
C+    CALL DRWABS (MAXX,MINY)        
C+    CALL DRWABS (MINX,MINY)        
C        
C     (4) PRINT THE FRAME NUMBER AND SCALE IN THE LOWER RIGHT CORNER    
C        
C     CSIZE = 1,2,3,4        
C        
C+    IY = 75        
C+    IF (TERMT .GT. 2) GO TO 80        
C+    CALL CHRSIZ (CSIZE)        
C+    IY = 40        
C        
C+ 80 FCURFR = CURFR        
C+    CALL IFORM  (FCURFR,4,NUM,32)        
C+    CALL MOVABS (MAXX+15,MINY+IY)        
C+    CALL ANSTR  (4,NUM)        
C        
C+    ND = 0        
C+    IF (SCALE .LT. 10.0) ND = 2        
C+    IF (SCALE .LT. 100.) ND = 1        
C+    CALL FFORM  (SCALE,4,ND,NUM,32)        
C+    CALL MOVABS (MAXX+15,MINY)        
C+    CALL ANSTR  (4,NUM)        
C        
C     (5) FIND CHARACTER SIZE IN NASTRAN COORDINATES        
C        
C     THE NASTRAN COORDINATES FROM THE PLOT FILE ARE THE LOCATIONS      
C     OF THE CENTER OF A CHARACTER. HALF THE WIDTH AND HEIGHT        
C     (XCS,YCS) OF THE SIZE OF A CHARACTER MUST BE SUBTRACTED FROM      
C     THE NASTRAN COORDINATES TO PRINT THEM IN THE PROPER LOCATION.     
C        
C+    XCS = 7.7*(RMAX-RMIN)/(MAXX-MINX)        
C+    YCS = 1.8*XCS        
C        
C     SET SYMBOL SIZE        
C        
C+    SYM1 = YCS*SCALE        
C+    SYM2 = 2.0*YCS*SCALE        
C+    SYM3 = 0.3*YCS*SCALE        
C+    SYM7 = 0.7*YCS*SCALE        
C        
C     SET AND SCALE CHARACTER SIZE        
C        
C+    SIZE = 1        
C+    IF (TERMT .GT. 2) GO TO 90        
C+    IF (SCALE.LT.1.1 .OR. CURFR.EQ.0) SIZE = 4        
C+    IF (SCALE .LT. 1.7)  SIZE = 3        
C+    IF (SCALE .LT. 1.8)  SIZE = 2        
C+    IF (TERMT.EQ.2 .AND. SIZE.EQ.4) SIZE = 3        
C+    CALL CHRSIZ (SIZE)        
C+ 90 XCS = XCS*XFAC(SIZE)        
C+    YCS = YCS*YFAC(SIZE)        
C+    IF (CURFR .NE. 0) GO TO 100        
C+    XCS = -XCS        
C+    YCS = -YCS        
C        
C+100 RETURN        
C        
C     ERROR        
C        
C+120 STOP 'ERROR IN PFRAME/NASTEK/NASTPLOT'        
C+    END        
