      SUBROUTINE DSCLOS ( IUNIT )                                               
      INCLUDE 'DSIOF.COM'                                                       
c      print *,' dsclos,iunit=',iunit                                           
      CLOSE ( IUNIT )                                                           
      NUMCLS = NUMCLS + 1                                                       
      RETURN                                                                    
      END                                                                       
