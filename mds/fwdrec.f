        SUBROUTINE FWDREC ( *, FILE )                                           
      INCLUDE 'DSIOF.COM'                                                       
        INTEGER           FILE                                                  
        NAME = FILE                                                             
        CALL DSGEFL                                                             
        CALL DSFWR1                                                             
        CALL DSSDCB                                                             
        IF ( IRETRN .EQ. 1 ) RETURN 1                                           
        RETURN                                                                  
        END                                                                     
