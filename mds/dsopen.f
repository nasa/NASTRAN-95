      SUBROUTINE DSOPEN ( DSNAME, IUNIT, IOP )                                  
      CHARACTER*72      DSNAME                                                  
      INCLUDE          'DSIOF.COM'                                              
C      print *,' dsopen,iunit,iop,dsname=',iunit,iop,dsname                     
      IF ( IOP .NE. 1 ) CALL DSOPFF ( DSNAME, IUNIT, ICCER )                    
      IF ( IOP .EQ. 1 ) CALL DSOCFF ( DSNAME, IUNIT, ICCER )                    
      NUMOPN = NUMOPN + 1                                                       
 700  RETURN                                                                    
      END                                                                       
