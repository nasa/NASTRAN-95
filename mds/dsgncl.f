        SUBROUTINE DSGNCL                                                       
      INCLUDE 'DSIOF.COM'                                                       
      INCLUDE 'GINOX.COM'                                                       
        IDSN =  MDSFCB( 2, IFILEX )                                             
        CALL DSCLOS( IDSN )                                                     
        MDSFCB( 1,IDSN ) = IAND( MDSFCB( 1,IDSN ), MASKH1 )                     
        RETURN                                                                  
        END                                                                     
