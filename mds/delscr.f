      SUBROUTINE DELSCR                                                         
C                                                                               
C     THIS SUBROUTINE IS CALLED AT THE BEGINNING OF EACH FUNCTIONAL             
C     MODULE TO PHYSICALLY DELETE ALL SCRATCH FILES USED BY A                   
C     PREVIOUS FUNCTIONAL MODULE                                                
C                                                                               
      INTEGER*2       IUNIT                                                     
C                                                                               
      COMMON /DSUNIT/ IUNIT(220)                                                
      COMMON /XFIST / IFIST(2)                                                  
      COMMON /XPFIST/ IPFIST                                                    
      INCLUDE 'DSIOF.COM'                                                       
C                                                                               
      DATA MASK / 32767 /                                                       
C                                                                               
      NFILES = IFIST(2) - IPFIST                                                
      IF (NFILES .EQ. 0) RETURN                                                 
      ISTR = IPFIST + 1                                                         
      IEND = IFIST(2)                                                           
      DO 100 I = ISTR, IEND                                                     
      IFILE = IFIST(2*I+1)                                                      
      IF (IFILE.LT.301 .OR. IFILE.GT.320) GO TO 100                             
      IFILEX = 0                                                                
      CALL GETURN (IFILE)                                                       
      IF (IFILEX .EQ. MASK) GO TO 100                                           
      CALL DBMMGR ( 7 )                                                         
  100 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
