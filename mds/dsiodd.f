      SUBROUTINE DSIODD                                                         
      INCLUDE 'GINOX.COM'                                                       
      INCLUDE 'DSIOF.COM'                                                       
      LGINOX = 5*NUMFCB + NUMSOF + 2                                            
      LHALF  = 16                                                               
      LENDSP = 0                                                                
      LENWPB = 0                                                                
      DATA MASKH1 /Z'FFFF0000'/                                                     
      DATA MASKH2 /Z'0000FFFF'/                                                     
      DATA MASKE1 /Z'FF000000'/                                                     
      DATA MASKE2 /Z'00FF0000'/                                                     
      DATA MASKE3 /Z'0000FF00'/                                                      
      DATA MASKE4 /Z'000000FF'/                                                      
      DATA MCBMAS /Z'40000000'/                                                      
      MAXDSN = NUMFCB                                                           
      MASKQ1 = MASKE1                                                           
      MASKQ2 = MASKE2                                                           
      MASKQ3 = MASKE3                                                           
      MASKQ4 = MASKE4                                                           
      MULQ1  = 2**24                                                            
      MULQ2  = 2**16                                                            
      MULQ3  = 2**8                                                             
      DATA IDSX  /Z'00EE0000'/                                                      
      DATA IDSP  /Z'000E0000'/                                                      
      DATA IDSC  /Z'000C0000'/                                                      
      DATA IDSRH /Z'11000000'/                                                      
      DATA IDSRT /Z'77000000'/                                                      
      DATA IDSSB /Z'22000000'/                                                      
      DATA IDSSE /Z'7F000000'/                                                      
      DATA IDSCH /Z'3B000000'/                                                      
      DATA IDSCT /Z'3F000000'/                                                      
      DATA IDSSH /Z'4B000000'/                                                      
      DATA IDSST /Z'4F000000'/                                                      
      DATA IDSSD /Z'DD000000'/                                                      
      DATA IDSEB /Z'EB000000'/                                                      
      DATA IDSEF /Z'EF000000'/                                                      
      NWRDEL( 1 ) = 1                                                           
      NWRDEL( 2 ) = 2                                                           
      NWRDEL( 3 ) = 2                                                           
      NWRDEL( 4 ) = 4                                                           
      RETURN                                                                    
      END                                                                       
