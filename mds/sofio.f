        SUBROUTINE SOFIO ( ISOP, IBLKNM, BUF )                                  
      INCLUDE 'GINOX.COM'                                                       
      INCLUDE 'DSIOF.COM'                                                       
        COMMON / SOFCOM / NFILES, FILNAM( 10 ), FILSIZ( 10 )                    
        COMMON / SYS    / BLKSIZ, DIRSIZ, SUPSIZ, AVBLKS, HIBLK                 
        COMMON / SYSTEM / ISYSBF, IWR                                           
        common / sofdsn / sofdsn(10)                                            
        CHARACTER*4       FILNAM                                                
        CHARACTER*80      DSNAME                                                
        character*80      sofdsn                                                
        INTEGER           FILSIZ, HIBLK, BUF(10)                                
        IF ( LENSOF( 1 ) .NE. 0 ) GO TO 20                                      
        NUMBLK = 1                                                              
        IF ( LENWPB .NE. 0 ) NUMBLK = ISYSBF / LENWPB                           
        DO 10 K = 1, NFILES                                                     
        LENSOF( K ) = 0                                                         
        DSNAME      = sofdsn(K)                                                 
        CALL DSINQR ( DSNAME, ISTAT, ISIZE)                                     
        IF (ISTAT .EQ. 0) GO TO 10                                              
        LENSOF( K ) = FILSIZ( K )                                               
10      CONTINUE                                                                
        LASFIL = 0                                                              
20      CONTINUE                                                                
        IF ( ISOP .EQ. 7 ) GO TO 200                                            
        IBLK   = IBLKNM                                                         
        IF ( IBLK .LE. 0 ) GO TO 700                                            
        IFILE  = 0                                                              
        DO 50 K = 1, NFILES                                                     
        IF ( IBLK .LE. FILSIZ( K ) )GO TO 30                                    
        IBLK   = IBLK - FILSIZ( K )                                             
        GO TO 50                                                                
30      IFILE  = K                                                              
        GO TO 100                                                               
50      CONTINUE                                                                
        WRITE( IWR, 9910 ) IBLKNM                                               
9910    FORMAT(' *** SUBSTRUCTURING ERROR - BLOCK NUMBER OUT OF RANGE ',        
     *         ' -  BLOCK NUMBER IS ',I10)                                      
        WRITE( IWR, 9915 )                                                      
9915    FORMAT( //,' THE FOLLOWING SOF FILES WERE AVAILABLE',//)                
        DO 60 K = 1, NFILES                                                     
        WRITE( IWR, 9920 ) sofdsn( K ), FILSIZ( K ), LENSOF( K )                
9920    FORMAT(' FILE ',A72' HAS ',I10, ' BLOCKS - BLOCKS USED ',I10)           
60      CONTINUE                                                                
        CALL MESAGE (-61, 0, 0)                                                 
100     IF ( LASFIL .EQ. IFILE ) GO TO 120                                      
        IF ( LASFIL .NE. 0 ) CALL DSCLOS ( 90 )                                 
        IALLOC = NUMBLK * FILSIZ(IFILE)                                         
        dsname = sofdsn( IFILE )                                                
        IOP = 0                                                                 
        CALL DSOPEN (  DSNAME, 90, IOP )                                        
        LASFIL = IFILE                                                          
120     IF ( ISOP .EQ. 1 ) GO TO 140                                            
        IF ( ( IBLK - LENSOF( IFILE ) ) .LE. 1 ) GO TO 130                      
        NUM  = IBLK - LENSOF( IFILE ) - 1                                       
        IF ( NUM .EQ. 0 ) GO TO 130                                             
        DO 125 K = 1, NUM                                                       
        LENSOF( IFILE ) = LENSOF( IFILE ) + 1                                   
        CALL DSWRIT ( 90, BUF(4), NBUFF, LENSOF( IFILE ),ICERR )                
        IF ( ICERR .NE. 0 ) GO TO 701                                           
125     CONTINUE                                                                
130     CONTINUE                                                                
        CALL DSWRIT ( 90, BUF(4), NBUFF, IBLK, ICERR )                          
        IF ( ICERR .NE. 0 ) GO TO 701                                           
        IF ( IBLK   .GT. LENSOF( IFILE ) ) LENSOF( IFILE ) = IBLK               
        IF ( IBLKNM .GT. HIBLK ) HIBLK = IBLKNM                                 
        GO TO 700                                                               
140     CALL DSREAD ( 90, BUF(4), NBUFF, IBLK )                                 
        GO TO 700                                                               
200     CALL DSCLOS( 90 )                                                       
        LASFIL = 0                                                              
700     GO TO 7000                                                              
701     IF ( ICERR .EQ. 28 ) WRITE ( IWR, 901 )                                 
        IF ( ICERR .NE. 28 ) WRITE ( IWR, 902 )                                 
        CALL MESAGE (-61, 0, 0)                                                 
901     FORMAT(///,' INSUFFICIENT SPACE FOR SOF FILE ON DEFAULT',               
     &             ' DEVICE---JOB ABORTED.')                                    
902     FORMAT(///,' I/O ERROR OCCURRED ON SOF FILE, JOB ABORTED')              
7000    RETURN                                                                  
        END                                                                     
