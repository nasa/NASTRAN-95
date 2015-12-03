      SUBROUTINE DUMMY        
C
C     NOTE:
C     THIS DUMMY.MIS ROUTINE CONTAINS 4 MACHINE VERSIONS (IBM,CDC,VAX,
C     AND UNIVAC). MOVE THIS SUBROUTINE TO THE MDS GROUP AND
C     REPLACE ALL THE 'C+' BY 2 SPACES IF MACHINE IS IBM, OR
C     REPLACE ALL THE 'C-' BY 2 SPACES IF MACHINE IS CDC, OR
C     REPLACE ALL THE 'C=' BY 2 SPACES IF MACHINE IS VAX, AND UNIX, OR
C     REPLACE ALL THE 'C*' BY 2 SPACES IF MACHINE IS UNIVAC
C     REPLACE ALL THE 'C.' BY 2 SPACES IF MACHINE TYPE IS 1, AND 11-20 
C        
C ****        
C     IBM VERSION        
C        
C     THIS SUBROUTINE PROVIDES ENTRIES FOR THE DUMMY ROUTINES        
C     USED BY OTHER COMPUTER MACHINES, AND ARE REFERENCED IN        
C     VARIOUS NASTRAN LINKS        
C        
C     THIS SUBROUTINE INCLUDES ALSO SOME DUMMY ROUTINES NOT YET        
C     WRITTEN        
C        
C     THIS ROUTINE SHOULD BE MOVED TO NASTRAN MACHINE-DEPENDENT        
C     SECTION (MDS)        
C ****        
C        
C+    DIMENSION       N(1)        
C+    CHARACTER*8     NAME        
C        
C+    COMMON /MACHIN/ MACH        
C+    COMMON /SYSTEM/ ISYSBF, NOUT        
C        
C+    IF (MACH .EQ. 2) GO TO 250        
C+    WRITE  (NOUT,20) MACH
C+ 20 FORMAT (/,' MACH =',I7)
C+    NAME = 'DUMMY'        
C+    GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN UNIVAC MACHINE        
C ****        
C        
C        
C+    ENTRY NTRAN (I,J,K)        
C+    NAME = 'NTRAN'        
C+    GO TO 100        
C        
C+    ENTRY CONTIN        
C+    NAME = 'CONTIN'        
C+    GO TO 100        
C        
C+    ENTRY FACIL (I,J)        
C+    NAME = 'FACIL'        
C+    GO TO 100        
C        
C+    ENTRY FACSF (I)        
C+    NAME = 'FACSF'        
C+    GO TO 100        
C        
C+    ENTRY UNVOPN (I)        
C+    NAME = 'UNVOPN'        
C+    GO TO 100        
C        
C+    ENTRY UNVCLS (I)        
C+    NAME = 'UNVCLS'        
C+    GO TO 100        
C        
C+    ENTRY ADDCRD (I,J)        
C+    NAME = 'ADDCRD'        
C+    GO TO 100        
C        
C ****        
C     ROUTINES USED BY UNIVAC AND IBM        
C ****        
C        
C     ENTRY RETURN        
C     GO TO 250        
C        
C+    ENTRY MSGUNI        
C+    IF (MACH .EQ. 2) GO TO 250        
C+    NAME = 'MSGUNI'        
C+    GO TO 100        
C        
C+    ENTRY XEOT (I,J,K,L)        
C+    IF (MACH .EQ. 2) GO TO 250        
C+    NAME = 'XEOT'        
C+    GO TO 100        
C        
C     ENTRY TPSWIT (I,J,K,L)        
C     NAME = 'TPSWIT'        
C     GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN IBM MACHINE        
C ****        
C        
C     ENTRY UMFTRN (I)        
C     NAME = 'UMFTRN'        
C     GO TO 100        
C        
C     ENTRY TAPSWI (I,J,K,L)        
C     NAME = 'TAPSWI'        
C     GO TO 100        
C        
C     ENTRY SOFIOI        
C     NAME = 'SOFIOI'        
C     GO TO 100        
C
C     ENTRY SEARCH (I)
C     NAME = 'SEARCH'
C     GO TO 100
C        
C ... NEXT THREE ARE SYSTEM ROUTINES THAT OPEN FILE DYNAMICALLY WITHOUT        
C     THE USE OF JCL. THESE ROUTINES ARE COMMONLY 'LOCAL INSTALLED'.        
C        
C     IQADDN CHECKS WHETHER A FILE EXISTS OR NOT        
C     QQDCBF BUILDS AN ATTRIBUTE LIST BY DDNAME        
C     QQGETF ALLOCATES FILE IN TSO OR BATCH        
C        
C     ENTRY IQZDDN (I)        
C     NAME = 'IQZDDN'        
C     GO TO 100        
C        
C     ENTRY QQDCBF (I,J,K,L,M,N)        
C     NAME = 'QQDCBF'        
C     GO TO 100        
C        
C     ENTRY QQGETF (I,J,K)        
C     NAME = 'QQGETF'        
C     GO TO 100        
C        
C ****        
C     ROUTINE USED ONLY BY IBM AND VAX        
C ****        
C        
C     ENTRY SOFIOF        
C     NAME = 'SOFIOF'        
C     GO TO 100        
C
C     THE FOLLOWING THREE ARE FUNCTIONS FOR QUAD WORD OPERATIONS   
C                                           (REAL*16)
C     ENTRY QABS (I)                                         
C     NAME = 'QABS'                                        
C     GO TO 100                                                    
C
C     ENTRY SNGLQ (I)                                      
C     NAME = 'SNGLQ'
C     GO TO 100                                                    
C
C     ENTRY DBLEQ (I) 
C     NAME = 'DBLEQ'
C     GO TO 100                                                    
C
C     ENTRY QSQRT (I)                                         
C     NAME = 'QSQRT'
C     GO TO 100                                                    
C
C     ENTRY QLOG (I)                                         
C     NAME = 'QLOG'
C     GO TO 100                                                    
C
C     ENTRY QEXTD (I)                                      
C     NAME = 'QEXTD'
C     GO TO 100                                                    
C
C ****                                                             
C     ROUTINE USED BY UNIVAC AND VAX        
C ****        
C        
C+    ENTRY DEFCOR        
C+    NAME = 'DEFCOR'        
C+    GO TO 100        
C        
C ****        
C     ROUTINES USED BY ALL MACHINES, EXCEPT VAX        
C ****        
C        
C     ENTRY GPERR        
C     NAME = 'GPERR'        
C     GO TO 100        
C        
C     ENTRY PDUMP        
C     GO TO 250        
C        
C     ENTRY MPY1        
C     NAME = 'MPY1'        
C     GO TO 100        
C        
C     ENTRY MPY2NT        
C     NAME = 'MPY2NT'        
C     GO TO 100        
C        
C     ENTRY MPY2T        
C     NAME = 'MPY2T'        
C     GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN CDC MACHINE        
C ****        
C        
C+    ENTRY LINK (I,J,K)        
C+    NAME = 'LINK'        
C+    GO TO 100        
C        
C+    ENTRY REMARK (I)        
C+    NAME = 'REMARK'        
C+    GO TO 100        
C        
C+    ENTRY CDCBUG (I,J,K,L)        
C+    NAME = 'CDCBUG'        
C+    GO TO 100        
C        
C+    ENTRY CDCOPN (I)        
C+    NAME = 'CDCOPN'        
C+    GO TO 100        
C        
C+    ENTRY CDCCLS (I)        
C+    NAME = 'CDCCLS'        
C+    GO TO 100        
C        
C+    ENTRY CDCKSZ (I)        
C+    NAME = 'CDCKSZ'        
C+    GO TO 100        
C        
C+    ENTRY PF (I,J,K)        
C+    NAME = 'PF'        
C+    GO TO 100        
C        
C+    ENTRY ISWAP (I)        
C+    NAME = 'ISWAP'        
C+    GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN VAX MACHINE        
C ****        
C        
C+    ENTRY VAXEND        
C+    NAME = 'VAXEND'        
C+    GO TO 100        
C        
C+    ENTRY VAXERR (L)        
C+    WRITE (NOUT,50) L        
C+ 50 FORMAT (/,' *** GINO ERROR AT LOC',I5)        
C+    GO TO 220        
C        
C+    ENTRY VAXSCH        
C+    NAME = 'VAXSCH'        
C+    GO TO 100        
C        
C+    ENTRY VAXBRK        
C+    NAME = 'VAXBRK'        
C+    GO TO 100        
C        
C+    ENTRY MPY1V (I,J,K)        
C+    NAME = 'MPY1V'        
C+    GO TO 100        
C        
C+    ENTRY MPY2NV (I,J,K)        
C+    NAME = 'MPY2NV'        
C+    GO TO 100        
C        
C+    ENTRY MPY2TV (I,J,K)        
C+    NAME = 'MPY2TV'        
C+    GO TO 100        
C        
C ****        
C     ROUTINES THAT PERFORM NO PARTICULAR FUNCTIONS, BUT THEY        
C     ARE STILL CALLED BY NASTRAN        
C ****        
C        
C+    ENTRY UNLOAD (I)        
C     CALLED BY INPTT1        
C+    GO TO 250        
C        
C ****        
C     THE FOLLOWING ROUTINES SEEM TO BE NO LONGER USED IN NASTRAN        
C ****        
C        
C+    ENTRY JIDINT (I)        
C+    NAME = 'JIDINT'        
C+    GO TO 100        
C        
C+    ENTRY OPMESG        
C+    NAME = 'OPMESG'        
C+    GO TO 100        
C        
C     ENTRY PDUM1,PDUM2,...,PDUM9 HAD BEEN REPLACED BY PDUMI        
C     ENTRY QDMM3, SQDM31, AND SQDM32 ARE NOW OBSOLETE        
C        
C+    ENTRY SEMTRN        
C+    NAME = 'SEMTRN'        
C+    GO TO 100        
C        
C ****        
C     DUMMY ROUTINES REFERENCED ONLY IN LINK 2, ALL MACHINES        
C ****        
C        
C+    ENTRY PDUMI (*,*,*,I,J,K,L,M,N,O)        
C+    NAME = 'PDUMI'        
C+    GO TO 100        
C        
C ****        
C     DUMMY ROUTINES REFERENCED ONLY IN LINK 5, ALL MACHINES        
C ****        
C        
C+    ENTRY PLBAR1 (I,J)        
C+    NAME = 'PLBAR1'        
C+    GO TO 100        
C      
C+    ENTRY PLOADX        
C+    NAME = 'PLOADX'        
C+    GO TO 100        
C        
C+    ENTRY ERRTRC (NAM)        
C     ==================        
C     ERROR TRACEBACK        
C        
C+    GO TO 220        
C        
C+100 WRITE  (NOUT,150) NAME        
C+150 FORMAT ('0*** SYSTEM FATAL ERROR  ---  JOB TERMINATED',        
C+   1        ' DUE TO CALL TO DUMMY SUBROUTINE.  ENTRY NAME IS ', A8)        
C+    GO TO 220                                                
C        
C ****        
C     TO FORCE A SYSTEM FATAL ERROR FOR TRACEBACK        
C ****        
C        
C+220 WRITE  (NOUT,230)        
C+230 FORMAT ('0*** ERROR TRACEBACK IN SYSTEM LOG FILE')        
C+    I = 987654321        
C+    N(I) = 1        
C+250 RETURN        
C
C
C     SUBROUTINE DUMMY        
C        
C ****        
C     CDC VERSION        
C        
C     THIS SUBROUTINE PROVIDES ENTRIES FOR THE DUMMY ROUTINES        
C     USED BY OTHER COMPUTER MACHINES, AND ARE REFERENCED IN        
C     VARIOUS NASTRAN LINKS        
C        
C     THIS SUBROUTINE INCLUDES ALSO SOME DUMMY ROUTINES NOT YET        
C     WRITTEN        
C        
C     THIS ROUTINE SHOULD BE MOVED TO NASTRAN MACHINE-DEPENDENT        
C     SECTION (MDS)        
C ****        
C        
C-    CHARACTER*8     NAME        
C        
C-    COMMON /MACHIN/ MACH        
C-    COMMON /SYSTEM/ ISYSBF, NOUT        
C        
C-    IF (MACH .EQ. 4) GO TO 250        
C-    WRITE  (NOUT,20) MACH
C- 20 FORMAT (/,' MACH =',I7)
C-    NAME = 'DUMMY'        
C-    GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN UNIVAC MACHINE        
C ****        
C        
C-    ENTRY NTRAN (I,J,K)        
C-    NAME = 'NTRAN'        
C-    GO TO 100        
C        
C-    ENTRY CONTIN        
C-    NAME = 'CONTIN'        
C-    GO TO 100        
C        
C-    ENTRY FACIL (I,J)        
C-    NAME = 'FACIL'        
C-    GO TO 100        
C        
C-    ENTRY FACSF (I)        
C-    NAME = 'FACSF'        
C-    GO TO 100        
C        
C-    ENTRY UNVOPN (I)        
C-    NAME = 'UNVOPN'        
C-    GO TO 100        
C        
C-    ENTRY UNVCLS (I)        
C-    NAME = 'UNVCLS'        
C-    GO TO 100        
C        
C-    ENTRY ADDCRD (I,J)        
C-    NAME = 'ADDCRD'        
C-    GO TO 100        
C        
C ****        
C     ROUTINES USED BY UNIVAC AND IBM        
C ****        
C        
C-    ENTRY RETURN        
C-    GO TO 250        
C        
C-    ENTRY MSGUNI        
C-    IF (MACH .EQ. 2) GO TO 250        
C-    NAME = 'MSGUNI'        
C-    GO TO 100        
C        
C-    ENTRY XEOT (I,J,K,L)        
C-    IF (MACH .EQ. 2) GO TO 250        
C-    NAME = 'XEOT'        
C-    GO TO 100        
C        
C-    ENTRY TPSWIT (I,J,K,L)        
C-    NAME = 'TPSWIT'        
C-    GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN IBM MACHINE        
C ****        
C        
C-    ENTRY UMFTRN (I)        
C-    NAME = 'UMFTRN'        
C-    GO TO 100        
C        
C-    ENTRY TAPWSI (I,J,K,L)        
C-    NAME = 'TAPSWI'        
C-    GO TO 100        
C
C-    ENTRY SEARCH (I)
C-    NAME = 'SEARCH'
C-    GO TO 100
C        
C-    ENTRY SOFIOI        
C-    NAME = 'SOFIOI'        
C-    GO TO 100        
C        
C-    ENTRY IQZDDN (I)        
C-    NAME = 'IQZDDN'        
C-    GO TO 100        
C        
C-    ENTRY QQDCBF (I,J,K,L,M,N)        
C-    NAME = 'QQDCBF'        
C-    GO TO 100        
C        
C-    ENTRY QQGETF (I,J,K)        
C-    NAME = 'QQGETF'        
C-    GO TO 100        
C        
C ****        
C     ROUTINE USED ONLY BY IBM AND VAX        
C ****        
C        
C-    ENTRY SOFIOF        
C-    NAME = 'SOFIOF'        
C-    GO TO 100        
C        
C     THE FOLLOWING THREE ARE FUNCTIONS FOR QUAD WORD OPERATIONS       
C                                           (REAL*16)
C-    ENTRY QABS (I)                                        
C-    NAME = 'QABS'
C-    GO TO 100                                                        
C
C-    ENTRY SNGLQ (I)                                        
C-    NAME = 'SNGLQ'
C-    GO TO 100                                                        
C
C-    ENTRY DBLEQ (I) 
C-    NAME = 'DBLEQ'
C-    GO TO 100                                                        
C
C-    ENTRY QSQRT (I)                                       
C-    NAME = 'QSQRT'
C-    GO TO 100                                                        
C
C-    ENTRY QLOG (I)                                      
C-    NAME = 'QLOG'
C-    GO TO 100                                                    
C
C-    ENTRY QEXTD (I)                                      
C-    NAME = 'QEXTD'
C-    GO TO 100                                                    
C
C ****                                                                 
C     ROUTINE USED BY UNIVAC AND VAX        
C ****        
C        
C-    ENTRY DEFCOR        
C-    NAME = 'DEFCOR'        
C-    GO TO 100        
C        
C ****        
C     ROUTINES USEDS BY ALL MACHINES, EXCEPT VAX        
C ****        
C        
C     ENTRY GPERR (I,J)        
C     NAME = 'GPERR'        
C     GO TO 100        
C        
C     ENTRY PDUMP        
C     NAME = 'PDUMP'        
C     GO TO 250        
C        
C     ENTRY MPY1        
C     NAME = 'MPY1'        
C     GO TO 100        
C        
C     ENTRY MPY2NT        
C     NAME = 'MPY2NT'        
C     GO TO 100        
C        
C     ENTRY MPY2T        
C     NAME = 'MPY2T'        
C     GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN CDC MACHINE        
C ****        
C        
C     ENTRY LINK (I,J,K)        
C     NAME = 'LINK'        
C     GO TO 100        
C        
C     ENTRY REMARK (I)        
C     NAME = 'REMARK'        
C     GO TO 100        
C        
C     ENTRY CDCBUG (I,J,K,L)        
C     NAME = 'CDCBUG'        
C     GO TO 100        
C        
C     ENTRY CDCOPN (I)        
C     NAME = 'CDCOPN'        
C     GO TO 100        
C        
C     ENTRY CDCCLS (I)        
C     NAME = 'CDCCLS'        
C     GO TO 100        
C        
C     ENTRY PF (I,J,K)        
C     NAME = 'PF'        
C     GO TO 100        
C        
C     ENTRY ISWAP (I)        
C     NAME = 'ISWAP'        
C     GO TO 100        
C        
C-    ENTRY CDCKSZ (I)        
C-    ENCODE (20,30,A) I        
C- 30 FORMAT ('OPEN CORE =',I7,2X)        
C-    CALL REMARK (A)        
C-    GO TO 250        
C        
C ****        
C     ROUTINES USED ONLY IN VAX MACHINE        
C ****        
C        
C-    ENTRY VAXEND        
C-    NAME = 'VAXEND'        
C-    GO TO 100        
C        
C-    ENTRY VAXERR (L)        
C-    WRITE  (NOUT,50) L        
C- 50 FORMAT (/,' *** GINO ERROR AT LOC',I5)        
C-    GO TO 220        
C        
C-    ENTRY VAXSCH        
C-    NAME = 'VAXSCH'        
C-    GO TO 100        
C        
C-    ENTRY VAXBRK        
C-    NAME = 'VAXBRK'        
C-    GO TO 100        
C        
C-    ENTRY MPY1V (I,J,K)        
C-    NAME = 'MPY1V'        
C-    GO TO 100        
C        
C-    ENTRY MPY2NV (I,J,K)        
C-    NAME = 'MPY2NV'        
C-    GO TO 100        
C        
C-    ENTRY MPY2TV (I,J,K)        
C-    NAME = 'MPY2TV'        
C-    GO TO 100        
C        
C ****        
C     ROUTINES THAT PERFORM NO PARTICULAR FUNCTIONS, BUT THEY        
C     ARE STILL CALLED BY NASTRAN        
C ****        
C        
C-    ENTRY UNLOAD (I)        
C     CALLED BY INPTT1        
C-    GO TO 250        
C        
C ****        
C     THE FOLLOWING ROUTINES SEEM TO BE NO LONGER USED IN NASTRAN        
C ****        
C        
C-    ENTRY JIDINT (I)        
C-    NAME = 'JIDINT'        
C-    GO TO 100        
C        
C-    ENTRY OPMESG        
C-    NAME = 'OPMESG'        
C-    GO TO 100        
C        
C     ENTRY PDUM1,PDUM2,...,PDUM9 HAD BEEN REPLACED BY PDUMI        
C     ENTRY QDMM3, SQDM31, AND SQDM32 ARE NOW OBSOLETE        
C        
C-    ENTRY SEMTRN        
C-    NAME = 'SEMTRN'        
C-    GO TO 100        
C        
C ****        
C     DUMMY ROUTINES REFERENCED ONLY IN LINK 2, ALL MACHINES        
C ****        
C        
C-    ENTRY PDUMI (*,*,*,I,J,K,L,M,N,O)        
C-    NAME = 'PDUMI'        
C-    GO TO 100        
C        
C ****        
C     DUMMY ROUTINES REFERENCED ONLY IN LINK 5, ALL MACHINES        
C ****        
C        
C-    ENTRY PLBAR1 (I,J)        
C-    NAME = 'PLBAR1'        
C-    GO TO 100        
C      
C-    ENTRY PLOADX        
C-    NAME = 'PLOADX'        
C-    GO TO 100        
C        
C-    ENTRY ERRTRC (NAM)        
C     ==================        
C     ERROR TRACEBACK        
C        
C-    GO TO 220        
C        
C-100 WRITE  (NOUT,150) NAME        
C-150 FORMAT ('0*** SYSTEM FATAL ERROR  ---  JOB TERMINATED',        
C-   1        ' DUE TO CALL TO DUMMY SUBROUTINE.  ENTRY NAME IS ', A8)        
C-    GO TO 220                                           
C        
C ****        
C     TO FORCE A SYSTEM FATAL ERROR FOR TRACEBACK        
C ****        
C        
C-220 WRITE  (NOUT,230)        
C-230 FORMAT ('0*** ERROR TRACEBACK IN SYSTEM LOG FILE')        
C-    I =-3        
C-    READ (I) J,K,M,N,O        
C-250 RETURN        
C
C
C     SUBROUTINE DUMMY        
C        
C ****        
C     VAX VERSION  (MODIFIED FOR DEC/ULTRIX)      
C        
C     THIS SUBROUTINE PROVIDES ENTRIES FOR THE DUMMY ROUTINES        
C     USED BY OTHER COMPUTER MACHINES, AND ARE REFERENCED IN        
C     VARIOUS NASTRAN LINKS        
C        
C     THIS SUBROUTINE INCLUDES ALSO SOME DUMMY ROUTINES NOT YET        
C     WRITTEN        
C        
C     THIS ROUTINE SHOULD BE MOVED TO NASTRAN MACHINE-DEPENDENT        
C     SECTION (MDS)        
C ****        
C        
      DIMENSION       N(1)        
      CHARACTER*8     NAME        
C        
      COMMON /MACHIN/ MACH        
      COMMON /SYSTEM/ ISYSBF, NOUT        
C        
      IF (MACH .EQ. 6) GO TO 250        
      WRITE  (NOUT,20) MACH
   20 FORMAT (/,' MACH =',I7)
      NAME = 'DUMMY'        
      GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN UNIVAC MACHINE        
C ****        
C        
      ENTRY ZCORSZ (I)
      NAME = 'ZCORSZ'
      GO TO 100
C        
      ENTRY MVBITS (I1,I2,I3,I4,I5)
      NAME = 'MVBITS'
      GO TO 100
C
      ENTRY CODKEY (CODE,KEY)        
      NAME = 'CODKEY'        
      GO TO 100        
C        
      ENTRY KCONEQ       
      NAME = 'KCONEQ'        
      GO TO 100        
C        
      ENTRY FNXTVQ (V1,V2,V3,V4,V5,ZB,I)        
      NAME = 'FNXTVQ'        
      GO TO 100        
C      
      ENTRY NTRAN (I,J,K)        
      NAME = 'NTRAN'        
      GO TO 100        
C        
      ENTRY CONTIN        
      NAME = 'CONTIN'        
      GO TO 100        
C        
      ENTRY FACIL (I,J)        
      NAME = 'FACIL'        
      GO TO 100        
C        
      ENTRY FACSF (I)        
      NAME = 'FACSF'        
      GO TO 100        
C        
      ENTRY UNVOPN (I)        
      NAME = 'UNVOPN'        
      GO TO 100        
C        
      ENTRY UNVCLS (I)        
      NAME = 'UNVCLS'        
      GO TO 100        
C        
      ENTRY ADDCRD (I,J)        
      NAME = 'ADDCRD'        
      GO TO 100        
C        
C ****        
C     ROUTINES USED BY UNIVAC AND IBM        
C ****        
C        
      ENTRY RETURN        
      GO TO 250        
C        
      ENTRY MSGUNI        
      IF (MACH .EQ. 2) GO TO 250        
      NAME = 'MSGUNI'        
      GO TO 100        
C        
      ENTRY XEOT (I,J,K,L)        
      IF (MACH .EQ. 2) GO TO 250        
      NAME = 'XEOT'        
      GO TO 100        
C        
      ENTRY TPSWIT (I,J,K,L)        
      NAME = 'TPSWIT'        
      GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN IBM MACHINE        
C ****        
C        
      ENTRY UMFTRN (I)        
      NAME = 'UMFTRN'        
      GO TO 100        
C        
      ENTRY TAPSWI (I,J,K,L)        
      NAME = 'TAPSWI'        
      GO TO 100        
C
      ENTRY SEARCH (I)
      NAME = 'SEARCH'
      GO TO 100
C        
      ENTRY SOFIOI        
      NAME = 'SOFIOI'        
      GO TO 100        
C        
      ENTRY IQZDDN (I)        
      NAME = 'IQZDDN'        
      GO TO 100        
C        
      ENTRY QQDCBF (I,J,K,L,M,N)        
      NAME = 'QQDCBF'        
      GO TO 100        
C        
      ENTRY QQGETF (I,J,K)        
      NAME = 'QQGETF'        
      GO TO 100        
C        
C ****        
C     ROUTINE USED ONLY BY IBM AND VAX        
C ****        
C        
C     ENTRY SOFIOF        
C     NAME = 'SOFIOF'        
C     GO TO 100        
C        
C     THE FOLLOWING THREE ARE FUNCTIONS FOR QUAD WORD OPERATIONS       
C                                           (REAL*16)
      ENTRY QABS (I)                                         
      NAME = 'QABS'
      GO TO 100                                                        
C
      ENTRY SNGLQ (I)                                    
      NAME = 'SNGLQ'
      GO TO 100                                                        
C
      ENTRY DBLEQ (I)
      NAME = 'DBLEQ'
      GO TO 100                                                        
C
      ENTRY QSQRT (I)                                       
      NAME = 'QSQRT'
      GO TO 100                                                        
C
      ENTRY QLOG (I)                                       
      NAME = 'QLOG'
      GO TO 100                                                    
C
      ENTRY QEXTD (I)                                       
      NAME = 'QEXTD'
      GO TO 100                                                    
C
C ****                                                                 
C     ROUTINE USED BY UNIVAC AND VAX        
C ****        
C        
C     ENTRY DEFCOR        
C     NAME = 'DEFCOR'        
C     GO TO 100        
C        
C ****        
C     ROUTINES USED BY ALL MACHINES, EXCEPT VAX        
C ****        
C        
      ENTRY GPERR (I,J)        
      NAME = 'GPERR'        
      GO TO 100        
C        
      ENTRY PDUMP        
      GO TO 250        
C        
      ENTRY MPY1        
      NAME = 'MPY1'        
      GO TO 100        
C        
      ENTRY MPY2NT        
      NAME = 'MPY2NT'        
      GO TO 100        
C        
      ENTRY MPY2T        
      NAME = 'MPY2T'        
      GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN CDC MACHINE        
C ****        
C        
      ENTRY LINK (I,J,K)        
      NAME = 'LINK'        
      GO TO 100        
C        
      ENTRY REMARK (I)        
      NAME = 'REMARK'        
      GO TO 100        
C        
      ENTRY CDCBUG (I,J,K,L)        
      NAME = 'CDCBUG'        
      GO TO 100        
C        
      ENTRY CDCOPN (I)        
      NAME = 'CDCOPN'        
      GO TO 100        
C        
      ENTRY CDCCLS (I)        
      NAME = 'CDCCLS'        
      GO TO 100        
C        
      ENTRY CDCKSZ (I)        
      NAME = 'CDCKSZ'        
      GO TO 100        
C        
      ENTRY PF (I,J,K)        
      NAME = 'PF'        
      GO TO 100        
C        
      ENTRY ISWAP (I)        
      NAME = 'ISWAP'        
      GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN VAX MACHINE        
C ****        
C        
      ENTRY VAXEND        
      NAME = 'VAXEND'        
      GO TO 100        
C        
      ENTRY VAXERR (L)        
      WRITE  (NOUT,50) L        
   50 FORMAT (/,' *** GINO ERROR AT LOC',I5)        
      GO TO 220        
C        
C     ENTRY VAXSCH        
C     NAME = 'VAXSCH'        
C     GO TO 100        
C        
      ENTRY VAXBRK        
      NAME = 'VAXBRK'        
      GO TO 100        
C        
C     ENTRY MPY1V (I,J,K)        
C     NAME = 'MPY1V'        
C     GO TO 100        
C        
C     ENTRY MPY2NV (I,J,K)        
C     NAME = 'MPY2NV'        
C     GO TO 100        
C        
C     ENTRY MPY2TV (I,J,K)        
C     NAME = 'MPY2TV'        
C     GO TO 100        
C        
C ****        
C     ROUTINES THAT PERFORM NO PARTICULAR FUNCTIONS, BUT THEY        
C     ARE STILL CALLED BY NASTRAN        
C ****        
C        
      ENTRY UNLOAD (I)        
C     CALLED BY INPTT1        
      GO TO 250        
C        
C ****        
C     THE FOLLOWING ROUTINES SEEM TO BE NO LONGER USED IN NASTRAN        
C ****        
C        
      ENTRY JIDINT (I)        
      NAME = 'JIDINT'        
      GO TO 100        
C        
      ENTRY OPMESG        
      NAME = 'OPMESG'        
      GO TO 100        
C        
C     ENTRY PDUM1,PDUM2,...,PDUM9 HAD BEEN REPLACED BY PDUMI        
C     ENTRY QDMM3, SQDM31, AND SQDM32 ARE NOW OBSOLETE        
C        
      ENTRY SEMTRN        
      NAME = 'SEMTRN'        
      GO TO 100        
C        
C ****        
C     DUMMY ROUTINES REFERENCED ONLY IN LINK 2, ALL MACHINES        
C ****        
C        
      ENTRY PDUMI (*,*,*,I,J,K,L,M,N,O)        
      NAME = 'PDUMI'        
      GO TO 100        
C        
C ****        
C     DUMMY ROUTINES REFERENCED ONLY IN LINK 5, ALL MACHINES        
C ****        
C        
      ENTRY PLBAR1 (I,J)        
      NAME = 'PLBAR1'        
      GO TO 100        
C      
      ENTRY PLOADX        
      NAME = 'PLOADX'        
      GO TO 100        
C        
CWKBD ENTRY ERRTRC (NAM)        
C     ==================        
C     ERROR TRACEBACK        
C        
CWKBD GO TO 220        
C        
  100 WRITE  (NOUT,150) NAME        
  150 FORMAT ('0*** SYSTEM FATAL ERROR  ---  JOB TERMINATED',        
     1        ' DUE TO CALL TO DUMMY SUBROUTINE.  ENTRY NAME IS ', A8)        
      GO TO 220                                            
C        
C ****        
C     TO FORCE A SYSTEM FATAL ERROR FOR TRACEBACK (VAX ONLY, NOT UNIX) 
C ****        
C        
  220 IF (MACH .NE. 5) GO TO 240                             
      WRITE  (NOUT,230)        
  230 FORMAT ('0*** ERROR TRACEBACK IN SYSTEM LOG FILE')        
      I = 987654321        
      N(I) = 0        
  240 STOP                                             
  250 RETURN        
C
C
C     SUBROUTINE DUMMY        
C        
C ****        
C     UNIVAC  VERSION        
C        
C     THIS SUBROUTINE PROVIDES ENTRIES FOR THE DUMMY ROUTINES        
C     USED BY OTHER COMPUTER MACHINES, AND ARE REFERENCED IN        
C     VARIOUS NASTRAN LINKS        
C        
C     THIS SUBROUTINE INCLUDES ALSO SOME DUMMY ROUTINES NOT YET        
C     WRITTEN        
C        
C     THIS ROUTINE SHOULD BE MOVED TO NASTRAN MACHINE-DEPENDENT        
C     SECTION (MDS)        
C ****        
C        
C*    CHARACTER*8     NAME        
C        
C*    COMMON /MACHIN/ MACH        
C*    COMMON /SYSTEM/ ISYSBF, NOUT        
C        
C*    IF (MACH .EQ. 3) GO TO 250        
C*    WRITE  (NOUT,20) MACH
C* 20 FORMAT (/,' MACH =',I7)
C*    NAME = 'DUMMY'        
C*    GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN UNIVAC MACHINE        
C ****        
C        
C     ENTRY NTRAN (I,J,K)        
C     NAME = 'NTRAN'        
C     GO TO 100        
C        
C     ENTRY CONTIN        
C     NAME = 'CONTIN'        
C     GO TO 100        
C        
C     ENTRY FACIL (I,J)        
C     NAME = 'FACIL'        
C     GO TO 100        
C        
C     ENTRY FACSF (I)        
C     NAME = 'FACSF'        
C     GO TO 100        
C        
C     ENTRY UNVOPN (I)        
C     NAME = 'UNVOPN'        
C     GO TO 100        
C        
C     ENTRY UNVCLS (I)        
C     NAME = 'UNVCLS'        
C     GO TO 100        
C        
C     ENTRY ADDCRD (I,J)        
C     NAME = 'ADDCRD'        
C     GO TO 100        
C        
C ****        
C     ROUTINES USED BY UNIVAC AND IBM        
C ****        
C        
C*    ENTRY RETURN        
C*    GO TO 250        
C        
C     ENTRY MSGUNI        
C     IF (MACH .EQ. 2) GO TO 250        
C     NAME = 'MSGUNI'        
C     GO TO 100        
C        
C     ENTRY XEOT (I,J,K,L)        
C     IF (MACH .EQ. 2) GO TO 250        
C     NAME = 'XEOT'        
C     GO TO 100        
C        
C     ENTRY TPSWIT (I,J,K,L)        
C     NAME = 'TPSWIT'        
C     GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN IBM MACHINE        
C ****        
C        
C*    ENTRY UMFTRN (I)        
C*    NAME = 'UMFTRN'        
C*    GO TO 100        
C        
C*    ENTRY TAPSWI (I,J,K,L)        
C*    NAME = 'TAPWSI'        
C*    GO TO 100        
C        
C*    ENTRY SEARCH (I)
C*    NAME = 'SEARCH'
C*    GO TO 100
C
C*    ENTRY SOFIOI        
C*    NAME = 'SOFIOI'        
C*    GO TO 100        
C        
C*    ENTRY IQZDDN (I)        
C*    NAME = 'IQZDDN'        
C*    GO TO 100        
C        
C*    ENTRY QQDCBF (I,J,K,L,M,N)        
C*    NAME = 'QQDCBF'        
C*    GO TO 100        
C        
C*    ENTRY QQGETF (I,J,K)        
C*    NAME = 'QQGETF'        
C*    GO TO 100        
C        
C ****        
C     ROUTINE USED ONLY BY IBM AND VAX        
C ****        
C        
C*    ENTRY SOFIOF        
C*    NAME = 'SOFIOF'        
C*    GO TO 100        
C        
C     THE FOLLOWING THREE ARE FUNCTIONS FOR QUAD WORD OPERATIONS       
C                                           (REAL*16)
C*    ENTRY QABS (I)                                        
C*    NAME = 'QABS'
C*    GO TO 100                                                        
C
C*    ENTRY SNGLQ (I)                                    
C*    NAME = 'SNGLQ'
C*    GO TO 100                                                        
C
C*    ENTRY DBLEQ (I) 
C*    NAME = 'DBLEQ'
C*    GO TO 100                                                        
C
C*    ENTRY QSQRT (I)                                 
C*    NAME = 'QSQRT'
C*    GO TO 100                                                        
C
C*    ENTRY QLOG (I)                                 
C*    NAME = 'QLOG'
C*    GO TO 100                                                    
C
C*    ENTRY QEXTD (I)                                   
C*    NAME = 'QEXTD'
C*    GO TO 100                                                    
C
C ****                                                                 
C     ROUTINE USED BY UNIVAC AND VAX        
C ****        
C        
C     ENTRY DEFCOR        
C     NAME = 'DEFCOR'        
C     GO TO 100        
C        
C ****        
C     ROUTINES USED BY ALL MACHINES, EXCEPT VAX        
C ****        
C        
C     ENTRY GPERR (I,J)        
C     NAME = 'GPERR'        
C     GO TO 100        
C        
C     ENTRY PDUMP        
C     GO TO 250        
C        
C     ENTRY MPY1        
C     NAME = 'MPY1'        
C     GO TO 100        
C        
C     ENTRY MPY2NT        
C     NAME = 'MPY2NT'        
C     GO TO 100        
C        
C     ENTRY MPY2T        
C     NAME = 'MPY2T'        
C     GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN CDC MACHINE        
C ****        
C        
C*    ENTRY LINK (I,J,K)        
C*    NAME = 'LINK'        
C*    GO TO 100        
C        
C*    ENTRY REMARK (I)        
C*    NAME = 'REMARK'        
C*    GO TO 100        
C        
C*    ENTRY CDCBUG (I,J,K,L)        
C*    NAME = 'CDCBUG'        
C*    GO TO 100        
C        
C*    ENTRY CDCOPN (I)        
C*    NAME = 'CDCOPN'        
C*    GO TO 100        
C        
C*    ENTRY CDCCLS (I)        
C*    NAME = 'CDCCLS'        
C*    GO TO 100        
C        
C*    ENTRY CDCKSZ (I)        
C*    NAME = 'CDCKSZ'        
C*    GO TO 100        
C       
C*    ENTRY PF (I,J,K)        
C*    NAME = 'PF'        
C*    GO TO 100        
C        
C*    ENTRY ISWAP (I)        
C*    NAME = 'ISWAP'        
C*    GO TO 100        
C        
C ****        
C     ROUTINES USED ONLY IN VAX MACHINE        
C ****        
C        
C*    ENTRY VAXEND        
C*    NAME = 'VAXEND'        
C*    GO TO 100        
C        
C*    ENTRY VAXERR (L)        
C*    WRITE  (NOUT,50) L        
C* 50 FORMAT (/,' *** GINO ERROR AT LOC',I5)        
C*    GO TO 220        
C        
C*    ENTRY VAXSCH        
C*    NAME = 'VAXSCH'        
C*    GO TO 100        
C        
C*    ENTRY VAXBRK        
C*    NAME = 'VAXBRK'        
C*    GO TO 100        
C        
C*    ENTRY MPY1V (I,J,K)        
C*    NAME = 'MPY1V'        
C*    GO TO 100        
C        
C*    ENTRY MPY2NV (I,J,K)        
C*    NAME = 'MPY2NV'        
C*    GO TO 100        
C        
C*    ENTRY MPY2TV (I,J,K)        
C*    NAME = 'MPY2TV'        
C*    GO TO 100        
C        
C ****        
C     ROUTINES THAT PERFORM NO PARTICULAR FUNCTIONS, BUT THEY        
C     ARE STILL CALLED BY NASTRAN        
C ****        
C        
C*    ENTRY UNLOAD (I)        
C     CALLED BY INPTT1        
C*    GO TO 250        
C        
C ****        
C     THE FOLLOWING ROUTINES SEEM TO BE NO LONGER USED IN NASTRAN        
C ****        
C        
C*    ENTRY JIDINT (I)        
C*    NAME = 'JIDINT'        
C*    GO TO 100        
C        
C*    ENTRY OPMESG        
C*    NAME = 'OPMESG'        
C*    GO TO 100        
C        
C     ENTRY PDUM1,PDUM2,...,PDUM9 HAD BEEN REPLACED BY PDUMI        
C     ENTRY QDMM3, SQDM31, AND SQDM32 ARE NOW OBSOLETE        
C        
C*    ENTRY SEMTRN        
C*    NAME = 'SEMTRN'        
C*    GO TO 100        
C        
C ****        
C     DUMMY ROUTINES REFERENCED ONLY IN LINK 2, ALL MACHINES        
C ****        
C        
C*    ENTRY PDUMI (*,*,*,I,J,K,L,M,N,O)        
C*    NAME = 'PDUMI'        
C*    GO TO 100        
C        
C ****        
C     DUMMY ROUTINES REFERENCED ONLY IN LINK 5, ALL MACHINES        
C ****        
C        
C*    ENTRY PLBAR1 (I,J)        
C*    NAME = 'PLBAR1'        
C*    GO TO 100        
C      
C*    ENTRY PLOADX        
C*    NAME = 'PLOADX'        
C*    GO TO 100        
C        
C*    ENTRY ERRTRC (NAM)        
C     ==================        
C     ERROR TRACEBACK        
C        
C*    GO TO 220        
C        
C*100 WRITE  (NOUT,150) NAME        
C*150 FORMAT ('0*** SYSTEM FATAL ERROR  ---  JOB TERMINATED',        
C*   1        ' DUE TO CALL TO DUMMY SUBROUTINE.  ENTRY NAME IS ', A8)        
C*    GO TO 220                                           
C        
C ****        
C     TO FORCE A SYSTEM FATAL ERROR FOR TRACEBACK        
C ****        
C        
C*220 WRITE  (NOUT,230)        
C*230 FORMAT ('0*** ERROR TRACEBACK IN SYSTEM LOG FILE')        
C*    X =-1.0        
C*    X = SQRT(X)        
C*250 RETURN        
C
C        
C     SUBROUTINE DUMMY                           
C        
C ****        
C     MACHINES 1, AND 6 THRU 20 VERSION         
C        
C     THIS SUBROUTINE PROVIDES ENTRIES FOR THE DUMMY ROUTINES        
C     USED BY OTHER COMPUTER MACHINES, AND ARE REFERENCED IN        
C     VARIOUS NASTRAN LINKS        
C        
C     THIS SUBROUTINE INCLUDES ALSO SOME DUMMY ROUTINES NOT YET        
C     WRITTEN        
C        
C     THIS ROUTINE SHOULD BE MOVED TO NASTRAN MACHINE-DEPENDENT        
C     SECTION (MDS)        
C ****        
C        
C.    DIMENSION       N(1)        
C.    CHARACTER*8     NAME        
C        
C.    COMMON /MACHIN/ MACH        
C.    COMMON /SYSTEM/ ISYSBF, NOUT        
C        
C.    IF (MACH.EQ.1 .AND. MACH.GE.6) GO TO RETURN
C.    WRITE  (NOUT,150) NAME,MACH 
C.150 FORMAT ('0*** SYSTEM FATAL ERROR  ---  JOB TERMINATED', /5X,
C.   1       'SUBROUTINE DUMMY FOR MACHINE TYPE',I4,' IS NOT AVAILABLE')  
C.    I = 987654321        
C.    N(I) = 0        
C.    STOP 
C
      END        
