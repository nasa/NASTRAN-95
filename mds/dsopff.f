      SUBROUTINE DSOPFF ( DSNAME, IUNIT, ISTATUS )                              
      CHARACTER*72      DSNAME                                                  
      COMMON / SYSTEM / SYSBUF, IWR                                             
      COMMON / MACHIN / MAC(3), LQRO
      INCLUDE          'DSIOF.COM'                                              
      NBUFF4 = NBUFF * ( MOD(LQRO,100) / 10 )                           
      OPEN  ( UNIT=IUNIT, FILE=DSNAME, RECL=NBUFF4, FORM='UNFORMATTED'          
     &,       ACCESS='DIRECT', IOSTAT=ISTATUS, ERR=701 
     &,       STATUS='UNKNOWN' )                        
      GO TO 777                                                                 
701   WRITE ( IWR, 901 ) IUNIT, ISTATUS, DSNAME                                 
901   FORMAT(//,' FATAL ERROR IN DSOPFF, UNABLE TO OPEN UNIT=',I4               
     &         ,' IOSTAT=',I5                                                   
     &       ,/,' FILE NAME=',A72 )                                             
      ICCERR = ISTATUS                                                          
      CALL DSMSG  ( 101 )                                                       
      CALL MESAGE ( -61, 0, 0 )                                                 
777   RETURN                                                                    
      END                                                                       
