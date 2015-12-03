      SUBROUTINE DSINQR ( DSN, ISTAT, ISIZE )                                   
C        DSINQR DETERMINES THE EXISTANCE OF A FILE:                             
C            DSN   ( INPUT  )   FILE NAME                                       
C            ISTAT ( OUTPUT )   =0, IF NOT EXIST; =1, IF EXIST                  
C            ISIZE ( OUTPUT )   = FILE SIZE IN GINO BLOCKS                      
C                                                                               
      LOGICAL       AVAIL                                                       
      CHARACTER*(*) DSN                                                         
      INQUIRE( FILE=DSN, EXIST=AVAIL, NEXTREC = NREC )                          
      ISTAT = 0                                                                 
      IF ( AVAIL ) ISTAT = 1                                                    
      ISIZE = NREC - 1                                                          
      RETURN                                                                    
      END                                                                       
