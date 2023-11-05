C
C  Steve Gibbons   NGI
C  2023-09-19
C
C  ccdtest
C
C Cross-Correlation-based Differential Time Estimation
C
C Need to specify the following arguments
C    EV1CODE
C    EV2CODE
C    STACODE
C    PHACODE
C    DTXTEM  - expected relative time of template signal
C    DTXTAR  - expected relative time of target signal
C    DTMPWN  - number of seconds before and after DTXTEM to search
C    DTARWN  - number of seconds before and after DTXTAR to search
C    ICC  = 1 for CC, 2 for CC * | CC |
C
C Our input file (read to standard input) needs to start a line with
C either * or # for a comment, or
C WF  ev1SACFILEname   ev2SACFILEname  - for a waveform, or
C FS  lofreq  hifreq   iord    passes  - for a filter specification, or
C WS  winlensec  winstepsec            - for a window specification
C
C
      PROGRAM ccdtest
      IMPLICIT NONE
C
C First define parameters
C NFSMAX is the maximum number of filter specifications
C NFS    is the actual number of filter specifications read in
C
C We need 4 parameters for each filter specification
C real*4  FLOARR
C real*4  FHIARR
C integer IORDAR
C integer PASSAR
C
      INTEGER     NFSMAX
      PARAMETER ( NFSMAX = 15 )
      INTEGER     NFS
      INTEGER     IFS
C
      REAL*4      FLO
      REAL*4      FHI
      INTEGER     IORD
      INTEGER     PASSES
      REAL*4      FLOARR( NFSMAX )
      REAL*4      FHIARR( NFSMAX )
      INTEGER     IORDAR( NFSMAX )
      INTEGER     PASSAR( NFSMAX )
C
C Now define window specifications
C NWSMAX is the maximum number of window specifications
C NWS    is the actual number of window specifications
C
      INTEGER     NWSMAX
      PARAMETER ( NWSMAX = 10 )
      INTEGER     NWS
      INTEGER     IWS
C
      REAL*4      WINLEN
      REAL*4      WINSTP
      REAL*4      WINLAR( NWSMAX )
      REAL*4      WINSAR( NWSMAX )
      INTEGER     NLENWN
      INTEGER     NWSTEP
C
C NWPMAX is maximum number of waveform pairs
C That is a single channel that should be 
C present for both events
C
      INTEGER     NWPMAX
      PARAMETER ( NWPMAX = 25 )
      INTEGER     NWP
      INTEGER     IWP
C
C NSMPMX is the maximum number of samples allowed
C in a given waveform read.
C This should be quite limited as we want
C relatively short waveforms surrounding 
C a single arrival.
C Say 3 minutes before and 3 minutes after at 100 Hz
C 100 * 6 * 60 = 36000
C
      INTEGER     NSMPMX
      PARAMETER ( NSMPMX = 36000 )
C
C Now REAL*4 arrays for the actual waveforms
C
      REAL*4      REV1WF( NSMPMX, NWPMAX )
      REAL*4      REV2WF( NSMPMX, NWPMAX )
      REAL*4      R1VALS( NSMPMX         )
      REAL*4      R2VALS( NSMPMX         )
C
C     REAL*8 arrays for the starting epoch times
C
      REAL*8      DTIM01
      REAL*8      DTIM02
C
C     REAL*8 arrays for the sampling intervals
C
      REAL*8      DELTA1
      REAL*8      DELTA2
      REAL*8      DBEG1 
      REAL*8      DBEG2 
C
      REAL*8      DELTAT
      REAL*8      DINTAT
      REAL*8      DTIM0A
      REAL*8      DTIM0B
      LOGICAL     OFIRSA
      LOGICAL     OFIRSB
      LOGICAL     ODELTA
C
C     INTEGER arrays for the number of samples
C
      INTEGER     NSMPE1(         NWPMAX )
      INTEGER     NSMPE2(         NWPMAX )
      INTEGER     NSAMP1
      INTEGER     NSAMP2
      INTEGER     ISAMP 
C
C We now need the arrays to store the correlation functions
C NCCSMX is the maximum number of samples in the correlation functions
C If we assume 8 seconds at 100 Hz then 800 should be enough.
C
      INTEGER     NCCSMX
      PARAMETER ( NCCSMX = 800 )
      INTEGER     NCCSAC
      INTEGER     ICCS
C
C Now we need to determine the maximum number of correlation functions
C This is a little trickier as it will depend upon the window specifications
C Let's say 2000 for now. NCCFMX = 5000
C
      INTEGER     NCCFMX
      PARAMETER ( NCCFMX = 5000 )
      INTEGER     NCCFAC
      INTEGER     ICCF
C
      REAL*4      RCCMAT( NCCSMX, NCCFMX )
      REAL*4      RCCVEC( NCCSMX )
C
C     INTEGER arrays for SAC epoch time
C
      INTEGER     NZDTA1(6)
      INTEGER     NZDTA2(6)
C
      REAL*8      DRELT
      REAL*8      DCCOPT
      REAL*8      DCORRT
      REAL*8      DEDIFF
      REAL*8      DSHIFT
C
C DLOW to ensure comparison
C
      REAL*8      DLOW
      PARAMETER ( DLOW = 1.0d-5 )
C
C Parameters for spacing and specification of the correlation windows
C
      INTEGER     ICORR
      INTEGER     NCORR
      INTEGER     IN1TEM
      INTEGER     IN1TAR
      INTEGER     IN1COR
C
C Parameters for the filtering
C
      CHARACTER*8 APROTO
      CHARACTER*8 CHTYPE
      REAL        A
      REAL        TRBNDW
C
C Storage for the epoch time character strings
C
      CHARACTER*(24) CHUTM1
      CHARACTER*(24) CHUTM2
C
C ICC is the flag for CC versus CC * | CC |
C
      INTEGER     ICC
C
      INTEGER     NTSTAR
      INTEGER     NELCC 
C
C Arguments for the routine CSARGM
C
      INTEGER     IERR
      INTEGER     NARGM
      PARAMETER ( NARGM = 6 )
      INTEGER     NARGS, CSLEN, IARGL( NARGM, 2 )
C
C Variables to store the input parameters.
C
      CHARACTER*(200) CHARG
      CHARACTER*(200) CFNAME
      CHARACTER*(20)  EV1CODE
      CHARACTER*(20)  EV2CODE
      CHARACTER*(8)   STACODE
      CHARACTER*(8)   PHACODE
C DTXTEM is the expected time in seconds of the template signal from 
C the start of the template waveform
C DTXTAR is the expected time in seconds of the target signal from 
C the start of the target waveform
      REAL*8          DTXTEM
      REAL*8          DTXTAR
      REAL*8          DTMPWN
      REAL*8          DTARWN
      INTEGER         NTMPWN
      INTEGER         NTARWN
      INTEGER         NB4TEMREF
      INTEGER         NB4TARREF
      REAL*8          DTEMRF
      REAL*8          DTEMSTART
      INTEGER         NTEMSTART
      REAL*8          DTARRF
      REAL*8          DTARSTART
      INTEGER         NTARSTART
C
      REAL*4          BEG
      REAL*4          RDT
C
      INTEGER     NIARGS
      INTEGER     IARGC
      INTEGER     IARG
C
      INTEGER     I1
      INTEGER     I2
      INTEGER     ILEN
C
      INTEGER     ISMPMX
      REAL*4      RMAXVL
      REAL*4      RSCALE
C
      NIARGS  = IARGC()
      IF ( NIARGS.NE.9 ) THEN
        WRITE (6,*) 'Usage: ccdtest ev1 ev2 sta pha '
        WRITE (6,*) '        dtxtem dtxtar dtmpwn dtarwn icc '
        CALL EXIT(1)
      ENDIF
      CSLEN  = 198
C
C Read EV1CODE
C
      CHARG  = ' '
      IARG   = 1
      CALL GETARG( IARG, CHARG )
      EV1CODE = CHARG(1:20)
C
C Read EV2CODE
C
      CHARG  = ' '
      IARG   = 2
      CALL GETARG( IARG, CHARG )
      EV2CODE = CHARG(1:20)
C
C Read STACODE
C
      CHARG  = ' '
      IARG   = 3
      CALL GETARG( IARG, CHARG )
      STACODE = CHARG(1:8)
C
C Read PHACODE
C
      CHARG  = ' '
      IARG   = 4
      CALL GETARG( IARG, CHARG )
      PHACODE = CHARG(1:8)
C
C Read DTXTEM
C
      CHARG  = ' '
      IARG   = 5
      CALL GETARG( IARG, CHARG )
      READ ( CHARG, *, END=99, ERR=99 ) DTXTEM
C
C Read DTXTAR
C
      CHARG  = ' '
      IARG   = 6
      CALL GETARG( IARG, CHARG )
      READ ( CHARG, *, END=99, ERR=99 ) DTXTAR
C
C Read DTMPWN
C
      CHARG  = ' '
      IARG   = 7
      CALL GETARG( IARG, CHARG )
      READ ( CHARG, *, END=99, ERR=99 ) DTMPWN
C
C Read DTARWN
C
      CHARG  = ' '
      IARG   = 8
      CALL GETARG( IARG, CHARG )
      READ ( CHARG, *, END=99, ERR=99 ) DTARWN
C
C Read ICC
C
      CHARG  = ' '
      IARG   = 9
      CALL GETARG( IARG, CHARG )
      READ ( CHARG, *, END=99, ERR=99 ) ICC
      IF ( ICC.NE.1 .AND. ICC.NE.2 ) THEN
        WRITE (6,*) 'Error: ICC must be 1 for CC  or '
        WRITE (6,*) '                   2 for CC*|CC| '
        GOTO 99
      ENDIF
C
      WRITE (6,*) 'EV1CODE ', EV1CODE
      WRITE (6,*) 'EV2CODE ', EV2CODE
      WRITE (6,*) 'STACODE ', STACODE
      WRITE (6,*) 'PHACODE ', PHACODE
      WRITE (6,*) 'DTXTEM  ', DTXTEM 
      WRITE (6,*) 'DTXTAR  ', DTXTAR 
      WRITE (6,*) 'DTMPWN  ', DTMPWN 
      WRITE (6,*) 'DTARWN  ', DTARWN 
      WRITE (6,*) 'ICC     ', ICC    
C 
C  Now start reading in input lines
C 
      NFS    = 0
      NWS    = 0
      NWP    = 0
      OFIRSA = .FALSE.
      OFIRSB = .FALSE.
      ODELTA = .FALSE.
 50   CONTINUE
      CHARG  = ' '
      READ (5,'(A)',END=60,ERR=50) CHARG
      IF ( CHARG(1:1).EQ.'*' ) GOTO 50
      IF ( CHARG(1:1).EQ.'#' ) GOTO 50
C
C Read here if we have a window specification
C WS  winlen  winstp
C
      IF ( CHARG(1:3).EQ.'WS ' ) THEN
        NARGS  = 3
        CALL CSARGM( CHARG, NARGS, NARGM, CSLEN, IARGL, IERR )
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'CSARGM returned IERR = ', IERR
          GOTO 99
        ENDIF
        IARG   = 2
        I1     = IARGL( IARG, 1 )
        I2     = IARGL( IARG, 2 )
        READ ( CHARG(I1:I2), *, END=99, ERR=99 ) WINLEN
        IARG   = 3
        I1     = IARGL( IARG, 1 )
        I2     = IARGL( IARG, 2 )
        READ ( CHARG(I1:I2), *, END=99, ERR=99 ) WINSTP
C       .
C       . OK. Against all odds we seem to have read in
C       . WINLEN and WINSTP
C       .
        IF ( NWS.EQ.NWSMAX ) THEN
          WRITE (6,*) 'NWS = ', NWS,' too many.'
          GOTO 99
        ENDIF
        NWS    = NWS + 1
        WINLAR( NWS    )  = WINLEN
        WINSAR( NWS    )  = WINSTP
C       .
      ENDIF
C
C Read here if we have a filter specification!
C FS   flo  fhi  iord  passes
C
      IF ( CHARG(1:3).EQ.'FS ' ) THEN
        NARGS  = 5
        CALL CSARGM( CHARG, NARGS, NARGM, CSLEN, IARGL, IERR )
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'CSARGM returned IERR = ', IERR
          GOTO 99
        ENDIF
C
        IARG   = 2
        I1     = IARGL( IARG, 1 )
        I2     = IARGL( IARG, 2 )
        READ ( CHARG(I1:I2), *, END=99, ERR=99 ) FLO
        IARG   = 3
        I1     = IARGL( IARG, 1 )
        I2     = IARGL( IARG, 2 )
        READ ( CHARG(I1:I2), *, END=99, ERR=99 ) FHI
        IF ( FHI.LE.FLO ) THEN
          WRITE (6,*) 'FLO = ', FLO
          WRITE (6,*) 'FHI = ', FHI
          GOTO 99
        ENDIF
        IARG   = 4
        I1     = IARGL( IARG, 1 )
        I2     = IARGL( IARG, 2 )
        READ ( CHARG(I1:I2), *, END=99, ERR=99 ) IORD
        IARG   = 5
        I1     = IARGL( IARG, 1 )
        I2     = IARGL( IARG, 2 )
        READ ( CHARG(I1:I2), *, END=99, ERR=99 ) PASSES
C       .
C       . OK. Against all odds we seem to have read in
C       . FLO, FHI, IORD, PASSES so we now try to enter
C       . them into our arrays.
C       .
        IF ( NFS.EQ.NFSMAX ) THEN
          WRITE (6,*) 'NFS = ', NFS,' too many.'
          GOTO 99
        ENDIF
        NFS    = NFS + 1
        FLOARR( NFS    )  = FLO
        FHIARR( NFS    )  = FHI
        IORDAR( NFS    )  = IORD
        PASSAR( NFS    )  = PASSES
C       .
      ENDIF
C
      IF ( CHARG(1:3).EQ.'WF ' ) THEN
C       .
C       . Want to read in a pair of waveforms
C       .
        NARGS  = 3
        CALL CSARGM( CHARG, NARGS, NARGM, CSLEN, IARGL, IERR )
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'CSARGM returned IERR = ', IERR
          GOTO 99
        ENDIF
        IARG   = 2
        I1     = IARGL( IARG, 1 )
        I2     = IARGL( IARG, 2 )
        ILEN   = I2 - I1 + 1
        CFNAME = ' '
        CFNAME(1:ILEN) = CHARG(I1:I2)    
C       .
C       . We now have a filename for the ev1 waveform
C       .
        CALL RSAC1( CFNAME, R1VALS, NSAMP1, BEG, RDT, NSMPMX, IERR )
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'RSAC1 returned IERR = ', IERR,' for file'
          WRITE (6,*) CFNAME
          GOTO 99
        ENDIF 
c       WRITE (6,*) 'RDT    = ', RDT    
        DELTA1 = DBLE( RDT )
        DBEG1  = DBLE( BEG )
        WRITE (6,*) 'NSAMP1 = ', NSAMP1
        WRITE (6,*) 'DELTA1 = ', DELTA1
        WRITE (6,*) 'DBEG1  = ', DBEG1 
C       .
C       . Now read the SAC epoch time
C       .
        CALL GETNHV('NZYEAR',NZDTA1(1),IERR)
        CALL GETNHV('NZJDAY',NZDTA1(2),IERR)
        CALL GETNHV('NZHOUR',NZDTA1(3),IERR)
        CALL GETNHV('NZMIN', NZDTA1(4),IERR)
        CALL GETNHV('NZSEC', NZDTA1(5),IERR)
        CALL GETNHV('NZMSEC',NZDTA1(6),IERR)
        print *, NZDTA1(1), NZDTA1(2), NZDTA1(3),
     1           NZDTA1(4), NZDTA1(5), NZDTA1(6)
        CALL SACH2E( NZDTA1, DTIM01 )
        DTIM01 = DTIM01 + DBEG1
        WRITE (6,'(F20.3)') DTIM01
C       .
C       . We now have a start time, an nsamp, and a delta for wf1
C       .
        IF ( ODELTA ) THEN
          IF ( DABS( DELTAT-DELTA1).GT.DLOW ) THEN
            WRITE (6,*) 'deltat = ', DELTAT
            WRITE (6,*) 'delta1 = ', DELTA1
            GOTO 99
          ENDIF
        ELSE
          DELTAT = DELTA1
          IF ( DELTAT.LT.DLOW ) THEN
            WRITE (6,*) 'deltat = ', DELTAT
            GOTO 99
          ENDIF
          DINTAT = 1.0d0/DELTAT
          ODELTA = .TRUE.
        ENDIF
C       .
        IF ( OFIRSA ) THEN
          IF ( DABS( DTIM0A-DTIM01).GT.DLOW ) THEN
            WRITE (6,'(A,1X,f20.4)') 'DTIM0A = ', DTIM0A
            WRITE (6,'(A,1X,f20.4)') 'DTIM01 = ', DTIM01
            GOTO 99
          ENDIF
        ELSE
          DTIM0A = DTIM01
          OFIRSA = .TRUE.
        ENDIF
C       .
C       . Now get waveform for event 2
C       .
        IARG   = 3
        I1     = IARGL( IARG, 1 )
        I2     = IARGL( IARG, 2 )
        ILEN   = I2 - I1 + 1
        CFNAME = ' '
        CFNAME(1:ILEN) = CHARG(I1:I2)
C       .
C       . We now have a filename for the ev1 waveform
C       .
        CALL RSAC1( CFNAME, R2VALS, NSAMP2, BEG, RDT, NSMPMX, IERR )
        IF ( IERR.NE.0 ) THEN
          WRITE (6,*) 'RSAC1 returned IERR = ', IERR,' for file'
          WRITE (6,*) CFNAME
          GOTO 99
        ENDIF
c       WRITE (6,*) 'RDT    = ', RDT    
        DELTA2 = DBLE( RDT )
        DBEG2  = DBLE( BEG )
        WRITE (6,*) 'NSAMP2 = ', NSAMP2
        WRITE (6,*) 'DELTA2 = ', DELTA2
        WRITE (6,*) 'DBEG2  = ', DBEG2
C       .
C       . Now read the SAC epoch time
C       .
        CALL GETNHV('NZYEAR',NZDTA2(1),IERR)
        CALL GETNHV('NZJDAY',NZDTA2(2),IERR)
        CALL GETNHV('NZHOUR',NZDTA2(3),IERR)
        CALL GETNHV('NZMIN', NZDTA2(4),IERR)
        CALL GETNHV('NZSEC', NZDTA2(5),IERR)
        CALL GETNHV('NZMSEC',NZDTA2(6),IERR)
        print *, NZDTA2(1), NZDTA2(2), NZDTA2(3),
     1           NZDTA2(4), NZDTA2(5), NZDTA2(6)
        CALL SACH2E( NZDTA2, DTIM02 )
        DTIM02 = DTIM02 + DBEG2
        WRITE (6,'(F20.3)') DTIM02
C       .
        IF ( ODELTA ) THEN
          IF ( DABS( DELTAT-DELTA2).GT.DLOW ) THEN
            WRITE (6,*) 'deltat = ', DELTAT
            WRITE (6,*) 'delta2 = ', DELTA2
            GOTO 99
          ENDIF
        ENDIF
        IF ( OFIRSB ) THEN
          IF ( DABS( DTIM0B-DTIM02).GT.DLOW ) THEN
            WRITE (6,'(A,1X,f20.4)') 'DTIM0B = ', DTIM0B
            WRITE (6,'(A,1X,f20.4)') 'DTIM02 = ', DTIM02
            GOTO 99
          ENDIF
        ELSE
          DTIM0B = DTIM02
          OFIRSB = .TRUE.
        ENDIF
C       .
        IF ( NWP.EQ.NWPMAX ) THEN
          WRITE (6,*) 'NWP    = ', NWP,' too many.'
          GOTO 99
        ENDIF
        NWP    = NWP + 1
C       .
        NSMPE1( NWP ) = NSAMP1
        NSMPE2( NWP ) = NSAMP2
        DO ISAMP = 1, NSAMP1
          REV1WF( ISAMP, NWP ) = R1VALS( ISAMP )
        ENDDO
        DO ISAMP = 1, NSAMP2
          REV2WF( ISAMP, NWP ) = R2VALS( ISAMP )
        ENDDO
C       .
      ENDIF
      GOTO 50
 60   CONTINUE
C
      WRITE (6,*) 'Read in ', NWS,' window specifications'
      DO IWS = 1, NWS
        WRITE (6,170) IWS, WINLAR( IWS ), WINSAR( IWS )
      ENDDO
 170  FORMAT('Window ',I2,' winlen ',f8.4,' winstep ',f8.4)
C
C
      WRITE (6,*) 'Read in ', NFS,' filter specifications'
      DO IFS = 1, NFS
        WRITE (6,171) IFS, FLOARR( IFS ), FHIARR( IFS ),
     1                     IORDAR( IFS ), PASSAR( IFS )
      ENDDO
 171  FORMAT('Filter ',I2,' fr ',f6.2,',',f6.2,' ord ',I2,' pass ',I2)
C
      WRITE (6,*) 'Number of waveform pairs = ', NWP
      WRITE (6,172) DELTAT
 172  FORMAT ('deltat = ', f8.4 )
      WRITE (6,173) 'DTIM0A', DTIM0A
      WRITE (6,173) 'DTIM0B', DTIM0B
 173  FORMAT (A, ' = ', f20.4 )
 174  FORMAT (A, ' = ', I20   )
C
C We now need to calculate som integer index parameters
C for highly accurate time-keeping and we need to determine
C some double precision parameters to store the key absolute times.
C
C NTMPWN and NTARWN are simply the number of samples corresponding
C to DTMPWN and DTARWN. (They are just rounded down without complication)
C
      NTMPWN = INT( DTMPWN*DINTAT )
      NTARWN = INT( DTARWN*DINTAT )
C
C DTEMRF will be the reference epoch time for our template
C        signal and will be close to (DTIM0A + DTXTEM) but will
C        be exactly on a node of our input waveform.
C
      NB4TEMREF = INT( DTXTEM*DINTAT )
      DTEMRF    = DTIM0A + DBLE( NB4TEMREF )*DELTAT
      DTEMSTART = DTEMRF - DBLE( NTMPWN    )*DELTAT
      NTEMSTART = NB4TEMREF - NTMPWN
C If NTEMSTART is 3 then this is element 4 in the array since
C the first has index one and there are 3 sets of DELTAT to add
C to the starting time.
C
      NB4TARREF = INT( DTXTAR*DINTAT )
      DTARRF    = DTIM0B + DBLE( NB4TARREF )*DELTAT
      DTARSTART = DTARRF - DBLE( NTARWN    )*DELTAT
      NTARSTART = NB4TARREF - NTARWN
C
      WRITE (6,173) 'DTEMRF   ', DTEMRF      
      WRITE (6,173) 'DTEMSTART', DTEMSTART      
      WRITE (6,174) 'NB4TEMREF', NB4TEMREF      
      WRITE (6,174) 'NTMPWN   ', NTMPWN      
      WRITE (6,174) 'NTEMSTART', NTEMSTART      
C
      WRITE (6,173) 'DTARRF   ', DTARRF      
      WRITE (6,173) 'DTARSTART', DTARSTART      
      WRITE (6,174) 'NTARWN   ', NTARWN      
      WRITE (6,174) 'NB4TARREF', NB4TARREF      
      WRITE (6,174) 'NTARSTART', NTARSTART      
C
C OK. Now we need to loop around:
C   (1) our waveform pairs
C   (2) our filter specifications
C   (3) our window specifications
C For each new combination, we will increment NCCFAC by 1
C
C Here are the four fixed parameters for the filtering.
      CHTYPE = 'BP      '
      APROTO = 'BU      '
      TRBNDW = 0.5
      A      = 1.0
C
      NCCFAC = 0
      DO IWP = 1, NWP
        DO IFS = 1, NFS
C         .
C         . OK. At this point we need to copy the waveform pair
C         . back into the temporary arrays where we will filter them.
C         .
          DO ISAMP = 1, NSAMP1
            R1VALS( ISAMP ) = REV1WF( ISAMP, IWP )
          ENDDO
          DO ISAMP = 1, NSAMP2
            R2VALS( ISAMP ) = REV2WF( ISAMP, IWP )
          ENDDO
C         .
C         . R1VALS now contains the template waveform
C         . R2VALS now contains the target waveform
C         . Now we copy the filter parameters back to the temporary
C         . variables.
C         .
          FLO    = FLOARR( IFS    )
          FHI    = FHIARR( IFS    )
          IORD   = IORDAR( IFS    )
          PASSES = PASSAR( IFS    )
C         .
C         . First filter the template waveform
C         .
          CALL XAPIIR( R1VALS, NSAMP1, APROTO, TRBNDW, A, IORD,
     1                   CHTYPE, FLO, FHI, RDT, PASSES )
C         .
C         . Now filter the target waveform
C         .
          CALL XAPIIR( R2VALS, NSAMP2, APROTO, TRBNDW, A, IORD,
     1                   CHTYPE, FLO, FHI, RDT, PASSES )
C         .
          DO IWS = 1, NWS
            WINLEN = WINLAR( IWS    )
            WINSTP = WINSAR( IWS    )
            NLENWN = INT( DBLE( WINLEN )*DINTAT )
            NWSTEP = INT( DBLE( WINSTP )*DINTAT )
c           WRITE (6,174) 'IWS   ', IWS   
c           WRITE (6,174) 'NLENWN', NLENWN
c           WRITE (6,174) 'NWSTEP', NWSTEP
c The total number of samples under which the correlation
C window can start is 2*NTMPWN
C So the number of correlations will be NCORR = 2*NTMPWN/NWSTEP
            NCORR = 2*NTMPWN/NWSTEP
            IN1COR = 1
            IN1TEM = 1 + NTEMSTART - NWSTEP
            IN1TAR = 1 + NTARSTART - NWSTEP
            DO ICORR = 1, NCORR
              IN1TEM = IN1TEM + NWSTEP
              IN1TAR = IN1TAR + NWSTEP
              IF ( NCCFAC.EQ.NCCFMX ) THEN
                WRITE (6,*) 'NCCFMX reached: ', NCCFMX
                GOTO 99
              ENDIF
              NCCFAC = NCCFAC + 1
C             .
C             . Need to zero out row NCCFAC of RCCMAT
C             .
              DO ISAMP = 1, NCCSMX
                RCCMAT( ISAMP, NCCFAC ) = 0.0
              ENDDO
              NTSTAR = 2*NTARWN + NLENWN - (ICORR-1)*NWSTEP
              NELCC  = NTSTAR - NLENWN + 1
C             .
C             . The template window is the NLENWN samples
C             . starting in location R1VALS( IN1TEM )
C             . The target window is the NTSTAR samples
C             . starting in location R2VALS( IN1TAR )
C             . The CC-trace is the NELCC samples
C             . starting in location RCCVEC( IN1COR )
C             . RCCVEC has length NCCSMX
c             print *, ' -------------------- '
c             print *, 'ICORR  = ', ICORR
c             print *, 'IN1COR = ', IN1COR
c             print *, 'IN1TEM = ', IN1TEM
c             print *, 'IN1TAR = ', IN1TAR
c             print *, 'NLENWN = ', NLENWN
c             print *, 'NTSTAR = ', NTSTAR
c             print *, 'NELCC  = ', NELCC  
c             print *, 'NCCSMX = ', NCCSMX 
c STEVE need a subroutine Calculate CC Function
              CALL CALCCF( IERR, IN1TEM, IN1TAR, IN1COR, NCCSMX,
     1                     R1VALS, R2VALS, RCCVEC,
     2                     NLENWN, NTSTAR, NELCC, ICC )
              IF ( IERR.NE.0 ) THEN
                WRITE (6,*) 'Subroutine CALCCF returned IERR = ', IERR
                GOTO 99
              ENDIF
C             .
C             . Transfer RCCVEC to RCCMAT
C             .
              ISMPMX = -1
              RMAXVL = -1.0
              DO ISAMP = 1, NELCC
                RCCMAT( ISAMP, NCCFAC ) = RCCVEC( ISAMP )
                IF ( RCCVEC( ISAMP ).GT.RMAXVL ) THEN
                  ISMPMX = ISAMP
                  RMAXVL = RCCVEC( ISAMP )
                ENDIF
              ENDDO
C             .
              WRITE (6,181) NCCFAC, ISMPMX, RMAXVL
 181  FORMAT('Calc. ',I5,' ismpmx ', I5,' val ',f6.4)
C             .
            ENDDO
          ENDDO
C         . enddo for iws = 1, nws
        ENDDO
C       . enddo for ifs = 1, nfs
      ENDDO
C     . enddo for iwp = 1, nwp
C
C OK. We have now collected all of our correlation functions in the
C RCCMAT array - we can now create a stack of all the values
C in RCCVEC.
C
      ISMPMX = -1
      RMAXVL = -1.0
      RSCALE = 1.0/REAL( NCCFAC )
      DO ISAMP = 1, NELCC 
        RCCVEC( ISAMP ) = 0.0
        DO ICCF = 1, NCCFAC
          RCCVEC( ISAMP ) = RCCVEC( ISAMP )+RSCALE*RCCMAT( ISAMP, ICCF )
        ENDDO
        IF ( RCCVEC( ISAMP ).GT.RMAXVL ) THEN
          ISMPMX = ISAMP
          RMAXVL = RCCVEC( ISAMP )
        ENDIF
      ENDDO
      WRITE (6,181) 0, ISMPMX, RMAXVL
C
C So we know that the maximum on the stack is close to 
C sample ISMPMX - but we want it more accurately than this!
C CCVECI - Cross Correlation Vector Interpolate.
C Assumes that the time value is (ISAMP-1)*DELTAT
C and that the maximum value is near sample ISMPMX
C (and this is far from the edges)
C This should return DRELT with the best estimate of the
C time (relative to DTARSTART) at which the maximum is found.
C
      CALL CCVECI( IERR, NELCC, ISMPMX, RCCVEC, DELTAT,
     1             DRELT, DCCOPT )
      IF ( IERR.NE.0 ) THEN
        WRITE (6,*) 'Error from subroutine CCVECI'
        GOTO 99
      ENDIF
C
      DCORRT = DTARSTART + DRELT
      DEDIFF = DCORRT    - DTEMSTART
      WRITE (6,311) 'DTEMSTART = ', DTEMSTART
      WRITE (6,311) 'DCORRT    = ', DCORRT   
 311  FORMAT(A,1X,f20.4)
C
C H01  H02  2007-08-15T08:00:08.467  2007-08-15T12:00:08.709  LP53   P1  0.926
C
      DSHIFT    = DTEMRF - DTEMSTART
      DTEMSTART = DTEMSTART + DSHIFT
      DCORRT    = DCORRT    + DSHIFT
      CALL E2UTMS( DTEMSTART , CHUTM1 )
      CALL E2UTMS( DCORRT    , CHUTM2 )
C
      WRITE (6,399) EV1CODE, EV2CODE, CHUTM1, CHUTM2,
     1              STACODE, PHACODE, DCCOPT, DEDIFF
 399  FORMAT(A20,1X,A20,1X,A24,1X,A24,1X,A8,1X,A8,1X,f6.4,1X,f20.4)
C
      print *,' NCCFAC = ', NCCFAC
      CALL EXIT(0)
 99   CONTINUE
      WRITE (6,*) 'Abnormal exit.'
      WRITE (6,*) 'IARG   = ', IARG
      WRITE (6,*) 'CHARG  = ', CHARG
      CALL EXIT(1)
      END
C=====================================================================

C
C REAL*8 STEVE
C
c        CALL GETNHV('NZYEAR',NZDTA1(1),IERR)
c        CALL GETNHV('NZJDAY',NZDTA1(2),IERR)
c        CALL GETNHV('NZHOUR',NZDTA1(3),IERR)
c        CALL GETNHV('NZMIN', NZDTA1(4),IERR)
c        CALL GETNHV('NZSEC', NZDTA1(5),IERR)
c        CALL GETNHV('NZMSEC',NZDTA1(6),IERR)
c        IF ( IERR.NE.0 ) THEN
c          WRITE (6,*) 'GETNHV returned IERR = ', IERR,' for file'
c          WRITE (6,*) CFNAME
c          GOTO 99
c        ENDIF
c
C
C*********************************************************************
C Subroutine Character String ARGument Map ***************************
C            -         -      ---      -   ***************************
C Steve Gibbons August 2001 (University of Leeds)                    C
C____________________________________________________________________C
C                                                                    C
C Takes in a character string, LINE, a number of arguments, NARGS,   C
C and a maximum line length. Assuming the arguments to be separated  C
C by spaces, CSARGM will fill the integer array IARGL with the       C
C character positions of the start and end of the argument.          C
C                                                                    C
C____________________________________________________________________C
C                                                                    C
C Input Variables :-                                                 C
C ===============                                                    C
C                                                                    C
C  Character                                                         C
C  ---------                                                         C
C                                                                    C
C     LINE      : String (*) containing input arguments.             C
C                                                                    C
C  Integer                                                           C
C  -------                                                           C
C                                                                    C
C     NARGS     : Number of arguments to be searched for.            C
C     NARGM     : Maximum no. of arguments (i.e. dimension for IARGL C
C     CSLEN     : Length of character string.                        C
C                                                                    C
C Output Variables :-                                                C
C ================                                                   C
C                                                                    C
C     IARGL     : Array dim ( NARGM, 2 ).                            C
C                 On exit, IARGL( I, 1 ) contains the character      C
C                 position where argument I begins: IARGL( I, 2 )    C
C                 contains the character position where arg I ends.  C
C                                                                    C
C     IERR      : =0 all NARGS are found and located.                C
C                 >0 CSLEN was exceeded with only IERR arguments     C
C                 being located.                                     C
C                 IERR = -1 means that no arguments were found.      C
C                 IERR = -2 means that the last argument was not     C
C                 terminated, indicating a possible string longer    C
C                 than was anticipated.                              C
C____________________________________________________________________C
C
C*********************************************************************
      SUBROUTINE CSARGM( LINE, NARGS, NARGM, CSLEN, IARGL, IERR )
      IMPLICIT NONE
C____________________________________________________________________C
C Variable declarations - Parameters ................................C
C
      CHARACTER *(*) LINE
      INTEGER        NARGS, NARGM, CSLEN, IARGL( NARGM, 2 ), IERR
C____________________________________________________________________C
C Variable declarations - Working variables .........................C
C
      INTEGER        I, IARGF
      LOGICAL        OWORDP, OWORDC
C_____________________________________________________________________
C                  **************************************************C
C START OF PROGRAM **************************************************C
C____________________________________________________________________C
C
      IF ( NARGS.LT.0 .OR. NARGS.GT.NARGM ) THEN
        PRINT *,' Subroutine CSARGM.'
        PRINT *,' NARGS = ', NARGS
        PRINT *,' NARGM = ', NARGM
        PRINT *,' Program aborted.'
        STOP
      ENDIF
C
      DO IARGF = 1, NARGM
        IARGL( IARGF, 1 ) = -1
        IARGL( IARGF, 2 ) = -1
      ENDDO
C
      IARGF  = 0
      OWORDP = .FALSE.
      OWORDC = .FALSE.
      DO I = 1, CSLEN
        IF ( LINE(I:I).EQ.' ' ) THEN
          OWORDC = .FALSE.
        ELSE
          OWORDC = .TRUE.
        ENDIF
        IF ( I.EQ.1 ) OWORDP = .FALSE.
C
C Treat the case of us starting a new word
C
        IF ( .NOT. OWORDP .AND. OWORDC ) THEN
          IARGF = IARGF + 1
          IARGL( IARGF, 1 ) = I
        ENDIF
C
C Treat the case of us ending a word
C
        IF ( OWORDP .AND. .NOT. OWORDC ) IARGL( IARGF, 2 ) = I - 1
C
        IF ( IARGF.EQ.NARGS .AND. IARGL( IARGF, 2 ).GT.0 ) THEN
          IERR = 0
          RETURN
        ENDIF
C
        OWORDP = OWORDC
C
      ENDDO
C
      IF ( IARGF.EQ.0 ) THEN
        IERR = -1
        RETURN
      ENDIF
c     print *,' iargf = ', iargf
      IF ( IARGF.LT.NARGS ) IERR = IARGF
      IF ( IARGF.EQ.NARGS .AND. IARGL( IARGF, 2 ).LT.0 ) THEN
        IERR = -2
        IARGL( IARGF, 2 ) = CSLEN
      ENDIF
C
      RETURN
      END
C*********************************************************************
C
C Steve Gibbons
C NGI
C
C SAC human time 2 epoch time
C
C ISACTM( 1 ) = year
C ISACTM( 2 ) = Julian day
C ISACTM( 3 ) = hour
C ISACTM( 4 ) = minute
C ISACTM( 5 ) = second
C ISACTM( 6 ) = millisecond
C
      SUBROUTINE SACH2E( ISACTM, SACEPT )
      IMPLICIT NONE
C
      INTEGER ISACTM( 6 )
      REAL*8  SACEPT
C
      INTEGER NDAYS
      INTEGER IY
      INTEGER LEAPYR
C
      NDAYS  = 0
C
      IF ( ISACTM(1).LT.1970 ) THEN
        DO IY = ISACTM(1), 1969
          NDAYS   = NDAYS - 365
          CALL ISLEAP( IY, LEAPYR )
          NDAYS   = NDAYS - LEAPYR
        ENDDO
      ENDIF
C
      IF ( ISACTM(1).GT.1970 ) THEN
        DO IY = 1970, ISACTM(1) - 1
          NDAYS   = NDAYS + 365
          CALL ISLEAP( IY, LEAPYR )
          NDAYS   = NDAYS + LEAPYR
        ENDDO
      ENDIF
C
      NDAYS   = NDAYS + ISACTM( 2 ) - 1
      SACEPT  = 86400.0d0*DBLE( NDAYS )
      SACEPT  = SACEPT + 3600.0d0*DBLE( ISACTM(3) )
      SACEPT  = SACEPT +   60.0d0*DBLE( ISACTM(4) )
      SACEPT  = SACEPT +          DBLE( ISACTM(5) )
      SACEPT  = SACEPT +  0.001d0*DBLE( ISACTM(6) )
C
      RETURN
      END
C
C
C Steve Gibbons
C NGI 2023-09-18
C
C      isleap.f
C
C Inputs a year IYEAR and outputs LEAPYR = 0 if not leap year
C and LEAPYR = 1 if it is.
C
      SUBROUTINE ISLEAP( IYEAR, LEAPYR )
      IMPLICIT NONE
C
      INTEGER IYEAR
      INTEGER LEAPYR
C
      INTEGER ITERM1
      INTEGER ITERM2
      INTEGER ITERM3
C
      LEAPYR = 0
      ITERM1 = IYEAR - (IYEAR/4)*4
      ITERM2 = IYEAR - (IYEAR/100)*100
      ITERM3 = IYEAR - (IYEAR/400)*400
      IF ( ITERM1.EQ.0 .AND. ITERM2.NE.0 .OR. ITERM3.EQ.0 ) LEAPYR = 1
C
      RETURN
      END
C
C --- subroutine which performs time-domain cross-correlation operations
C     The waveform template is stored in 
C       R1VALS( IN1TEM ) to R1VALS( IN1TEM + NLENWN - 1 )
C     The target waveform is stored in
C       R2VALS( IN1TAR ) to R2VALS( IN1TAR + NTSTAR - 1 )
C     There will be calculated NELCC vector correlations
C       that will be stored in RCCVEC( IN1COR ) to 
C         RCCVEC( IN1COR + NELCC - 1 )
C     so           NELCC should be  NTSTAR - NLENWN + 1
C     No checks are made on the end of arrays for reading.
C     We do however check that IN1COR + NELCC - 1 .le. NCCSMX
C
C     If ICC.eq.1 then we return the cross-correlation coefficients
C     If ICC.eq.2 then we return the C . |C|
C
      SUBROUTINE CALCCF( IERR, IN1TEM, IN1TAR, IN1COR, NCCSMX,
     1                   R1VALS, R2VALS, RCCVEC,
     2                   NLENWN, NTSTAR, NELCC, ICC )
      IMPLICIT NONE
C
      INTEGER            IERR
      INTEGER            IN1TEM
      INTEGER            IN1TAR
      INTEGER            IN1COR
      INTEGER            NCCSMX
      REAL*4             R1VALS( * )
      REAL*4             R2VALS( * )
      REAL*4             RCCVEC( NCCSMX )
      INTEGER            NLENWN
      INTEGER            NTSTAR
      INTEGER            NELCC
      INTEGER            ICC
C
      REAL*4             RLOW
      PARAMETER        ( RLOW = 0.000001 )
      INTEGER            IPTCOR
      INTEGER            IPTTEM
      INTEGER            IPTTAR
      INTEGER            JPTTAR
C
      REAL*8             DACC
      REAL*8             DFAC
      REAL*4             RNMTEM
      REAL*4             RNMTAR
      REAL*4             RSCALE
C
      IERR   = 0
C
      IF ( (NTSTAR - NLENWN + 1).NE.NELCC    .OR.
     1     (IN1COR + NELCC - 1).GT.NCCSMX  ) THEN
        WRITE (6,*) 'Subroutine CALCCF: problem '
        WRITE (6,*) 'NCCSMX   = ', NCCSMX
        WRITE (6,*) 'IN1COR   = ', IN1COR
        WRITE (6,*) 'NTSTAR   = ', NTSTAR
        WRITE (6,*) 'NLENWN   = ', NLENWN
        WRITE (6,*) 'NELCC    = ', NELCC 
        IERR   = 1
        RETURN
      ENDIF
C
      IF ( ICC.NE.1 .AND. ICC.NE.2 ) THEN
        WRITE (6,*) 'Subroutine CALCCF: problem '
        WRITE (6,*) 'ICC      = ', ICC   
        IERR   = 1
        RETURN
      ENDIF
C
C Zero the RCCVEC array in case we need to make a speedy exit
C
      DO IPTCOR = 1, NCCSMX
        RCCVEC( IPTCOR ) = 0.0
      ENDDO
C
C Now calculate the norm of our template waveform.
C If this is (almost) zero then we exit.
C
      DACC   = 0.0d0
      DO IPTTEM = IN1TEM, IN1TEM + NLENWN - 1
        DFAC   = DBLE( R1VALS( IPTTEM ) )
        DACC   = DACC + DFAC*DFAC
      ENDDO
      DFAC   = DSQRT( DACC )
      RNMTEM = REAL( DFAC )
C
C Exit if template norm is trivially low
C
      IF ( RNMTEM.LT.RLOW ) THEN
        WRITE (6,*) 'Subroutine CALCCF: info '
        WRITE (6,*) 'Template norm = ', RNMTEM
        RETURN
      ENDIF
C
C Loop around the nodes we need to calculate a CC for
C
      IPTTAR = IN1TAR - 1
      DO IPTCOR = IN1COR , IN1COR + NELCC - 1
        IPTTAR = IPTTAR + 1
C       .
C       . Calculate the norm of the NLENWN elements of the
C       . target waveform starting at index IPTTAR
C       .
        DACC   = 0.0d0
        DO JPTTAR = IPTTAR, IPTTAR + NLENWN - 1
          DFAC   = DBLE( R2VALS( JPTTAR ) )
          DACC   = DACC + DFAC*DFAC
        ENDDO
        DFAC   = DSQRT( DACC )
        RNMTAR = REAL( DFAC )
C       .
C       . Only calculate the correlation coefficient if
C       . the target waveform norm is non-zero
C       .
        IF ( RNMTAR.GT.RLOW ) THEN
          RSCALE = 1.0/(RNMTEM*RNMTAR )
          IPTTEM = IN1TEM - 1
          DACC   = 0.0d0
          DO JPTTAR = IPTTAR, IPTTAR + NLENWN - 1
            IPTTEM = IPTTEM + 1
            DFAC   = DBLE( R2VALS( JPTTAR ) )
            DACC   = DACC + DFAC*DBLE( R1VALS( IPTTEM ) )
          ENDDO
          RCCVEC( IPTCOR ) = RSCALE*REAL( DACC )
          IF ( ICC.EQ.2 ) RCCVEC( IPTCOR ) = 
     1        RCCVEC( IPTCOR )*ABS( RCCVEC( IPTCOR ) )
        ENDIF
C       .
      ENDDO
C
      RETURN
      END
C
C Cross-Correlation Vector interpolate
C We have a REAL*4 vector of CC values RCCVEC (length NELCC)
C at which the value at ISMPMX is the highest.
C Node ISAMP has an "x-value" of (ISAMP-1)*DELTAT
C We want to use high order finite differences to estimate
C the x-value at which the true maximum occurs.
C
      SUBROUTINE CCVECI( IERR, NELCC, ISMPMX, RCCVEC, DELTAT,
     1                    DXOPT, DYOPT )
      IMPLICIT NONE
C
      INTEGER IERR
      INTEGER NELCC
      INTEGER ISMPMX
      REAL*4  RCCVEC( * )
      REAL*8  DELTAT
      REAL*8  DXOPT
      REAL*8  DYOPT
C
      INTEGER IND
      INTEGER JSAMP
      REAL*8  DXVALS( 5 )
      REAL*8  DYVALS( 5 )
C
      INTEGER            NNDS
      INTEGER            NCFM
      PARAMETER        ( NCFM = 5, NNDS = 5 )
      REAL*8             COEFM( NCFM, NCFM )
      REAL*8             WORK( NCFM )
      INTEGER            IPCM( NCFM )
C
      INTEGER            J
      REAL*8             DX1
      REAL*8             DX2
      REAL*8             DYDX1
      REAL*8             DYDX2
      REAL*8             DLOW
      PARAMETER        ( DLOW = 1.0d-7 )
      REAL*8             DM
      REAL*8             DC
      REAL*8             DXPROV
C
      IERR   = 0
      IF ( ISMPMX.LT.3 .OR. ISMPMX.GT.(NELCC-2) ) THEN
        WRITE (6,*) 'Subroutine CCVECI: Error '
        WRITE (6,*) ' ISMPMX = ', ISMPMX
        WRITE (6,*) ' NELCC  = ', NELCC  
        IERR   = 1
        RETURN
      ENDIF
C
      IND    = 0
      DO JSAMP = ISMPMX - 2, ISMPMX + 2
        IND    = IND + 1
        DXVALS( IND ) = DBLE( JSAMP-1 )*DELTAT
        DYVALS( IND ) = DBLE( RCCVEC( JSAMP ) )
      ENDDO
C
      DXOPT   = DXVALS( 3 )
      DYOPT   = DYVALS( 3 )
C
C Set DX1 to be halfway between DXVALS( 2 ) and DXVALS( 3 )
C Set DX2 to be halfway between DXVALS( 3 ) and DXVALS( 4 )
C
      DX1     = 0.5d0*( DXVALS( 2 ) + DXVALS( 3 ) )
      DX2     = 0.5d0*( DXVALS( 3 ) + DXVALS( 4 ) )
      IF ( DABS( DX2-DX1 ).LT.DLOW ) RETURN
C
      CALL GFDCFD( DX1, DXVALS, NNDS, COEFM, NCFM, IPCM, WORK)
      DYDX1   = 0.0d0
      DO J = 1, NNDS
        DYDX1   = DYDX1 + COEFM( 2, J )*DYVALS( J )
      ENDDO
C
      CALL GFDCFD( DX2, DXVALS, NNDS, COEFM, NCFM, IPCM, WORK)
      DYDX2   = 0.0d0
      DO J = 1, NNDS
        DYDX2   = DYDX2 + COEFM( 2, J )*DYVALS( J )
      ENDDO
C
C We assume the first derivative to vary as a straight line
C around the points DX1 and DX2. Assume the form "y = mx + c"
C Our optimal x is where y = 0
C
      DM     = (DYDX1-DYDX2)/(DX1-DX2)
C     . If the line is flat we will never cross zero
      IF ( DABS( DM ).LT.DLOW ) RETURN
      DC     = DYDX1 - DM*DX1
C
      DXPROV = -DC/DM
C     .
C     . DXPROV now contains our guess at the azimuth for which the
C     . derivative is zero. If it is not between DXVALS(2) and DXVALS(4)
C     . then this contradicts our assumption about DXVALS(3) being the
C     . optimal with DYVALS well-behaved.
C     .
      IF ( DXPROV.LE.DXVALS( 2 ) ) RETURN
      IF ( DXPROV.GE.DXVALS( 4 ) ) RETURN
C     .
      DXOPT  = DXPROV
      CALL GFDCFD( DXOPT, DXVALS, NNDS, COEFM, NCFM, IPCM, WORK)
      DYOPT  = 0.0d0
      DO J = 1, NNDS
        DYOPT   = DYOPT + COEFM( 1, J )*DYVALS( J )
      ENDDO
C
      RETURN
      END
C
C*********************************************************************
C subroutine General Finite Difference Coefficient Find **************
C            -       -      -          -           -    **************
C Steve Gibbons Mon Sep 20 16:57:54 BST 1999                         C
C____________________________________________________________________C
C                                                                    C
C Given a value of X and the values of x_i at NNDS distinct points,  C
C (X need not necessarily be one of the x_i) then the array COEFM    C
C is returned with the finite difference coefficients such that      C
C the ND^{th} derivative of a function f, evaluated at x = X,        C
C is given by                                                        C
C                                                                    C
C f^{ ND }( X ) = \sum_{j = 1, NNDS} COEFM( ND + 1, j )*f( x_j )     C
C                                                                    C
C This is a general version of the routine FDCINV which is valid     C
C only for equally spaced grid nodes.                                C
C                                                                    C
C Coefficients for up to the (NNDS-1)^{th} derivative are given      C
C although care must be taken to ensure the highest derivatives      C
C are sufficiently accurate.                                         C
C                                                                    C
C____________________________________________________________________C
C                                                                    C
C Input variables :-                                                 C
C ===============                                                    C
C  Double Precision                                                  C
C  ----------------                                                  C
C     X         : Abscissa at which derivatives are to be            C
C                  evaluated.                                        C
C     XARR      : Array of dimension ( NNDS ).                       C
C                  XARR( i ) contains the value of x/r at the        C
C                   i^{th} grid node.                                C
C                                                                    C
C     COEFM     : Dimension ( NCFM, NCFM).                           C
C     WORK      : Workspace array for LAPACK inversion routine.      C
C                 Dimension ( NCFM )                                 C
C                                                                    C
C  Integer                                                           C
C  -------                                                           C
C                                                                    C
C     NNDS      : Number of grid nodes.                              C
C     NCFM      : Leading order of coefficient matrix.               C
C                                                                    C
C     IPCM      : Work array for LAPACK routines to perform          C
C                 pivotting in the matrix inversion.                 C
C                 Dimension ( NCFM )                                 C
C                                                                    C
C____________________________________________________________________C
C
C*********************************************************************
      SUBROUTINE GFDCFD ( X, XARR, NNDS, COEFM, NCFM, IPCM, WORK)
      IMPLICIT NONE
C____________________________________________________________________C
C Variable declarations - Parameters ................................C
      INTEGER NNDS, NCFM, IPCM( NCFM )
      DOUBLE PRECISION X, COEFM( NCFM, NCFM ), WORK( NCFM ),
     1                 XARR( NNDS )
C____________________________________________________________________C
C Variable declarations - Working variables .........................C
C
      INTEGER I, J, NDER, INODE, INFO, ICOL, IROW
      DOUBLE PRECISION DZERO, LOW, FAC
      PARAMETER ( DZERO = 0.0d0, LOW = 1.0d-8 )
C____________________________________________________________________C
C START OF PROGRAM **************************************************C
C____________________________________________________________________C
C
C Check bounds of integer parameters
C
      IF ( NNDS.GT.NCFM ) THEN
         PRINT *,' Subroutine GFDCFD: '
         PRINT *,' NNDS = ', NNDS,'. NCFM = ', NCFM
         PRINT *,' Program aborted.'
         STOP
      ENDIF
C
C Calculate the distances h_i = ( x_i - X )
C These h_i must be distinct and this will be
C checked for - otherwise matrix is singular.
C We can store the h_i in WORK as this will not
C be needed until the inversion, by which time
C it will not be needed by us.
C
      DO I = 1, NNDS
        WORK( I ) = XARR( I ) - X
      ENDDO
C
C Now check for the uniqueness of the points ...
C
      DO I = 1, NNDS - 1
        DO J = I + 1, NNDS
          IF ( ABS( WORK( I ) - WORK( J ) ).LT.LOW ) THEN
            PRINT *,' Subroutine GFDCFD.'
            PRINT *,' X values ',I,' and ',J,' are'
            PRINT *,' identical.'
            PRINT *,' Program aborted.'
            STOP
          ENDIF
        ENDDO
      ENDDO
C
C____________________________________________________________________C
C Parameters are ok so let's zero COEFM
C
      I = 0
      CALL MATOP( COEFM, DZERO, NCFM, NCFM, I )
C
C (nder+1) is the number of the matrix column being filled in.
C inode is the number of the matrix row being filled in.
C
      DO NDER = 0, NNDS - 1
        ICOL = NDER + 1
        DO INODE = 1, NNDS
          IROW = INODE
          IF ( NDER.EQ.0 ) THEN
            COEFM( IROW, ICOL )  = 1.0d0
          ELSE
            FAC = WORK( INODE )/DBLE( NDER )
            COEFM( IROW, ICOL ) = COEFM( IROW, ICOL-1 )*FAC
          ENDIF
        ENDDO
      ENDDO
C
C Ok - this matrix is now ready for inversion -
C For this we use the LAPACK routines DGETRF and DGETRI
C First perform LU decomposition
C
      CALL DGETRF( NNDS, NNDS, COEFM, NCFM, IPCM, INFO )
C
C     . Check that LU decomposition has gone without
C     . problem.
C     .
C
      IF ( INFO.NE.0 ) THEN
         PRINT *,' Subroutine GFDCFD.'
         PRINT *,' The LAPACK subroutine DGETRF has'
         PRINT *,' returned ',INFO,' as a value of '
         PRINT *,' INFO in LU decomposition of COEFM matrix.'
         PRINT *,' Program aborted.'
         STOP
      ENDIF
C     .
C     . Now compute the inverse with the LAPACK routine
C     . DGETRI.
C     .
      CALL DGETRI( NNDS, COEFM, NCFM, IPCM, WORK, NCFM, INFO )
C     .
C     . Check that inversion has gone without problem.
C     .
      IF ( INFO.NE.0 ) THEN
         PRINT *,' Subroutine GFDCFD.'
         PRINT *,' The LAPACK subroutine DGETRI has'
         PRINT *,' returned ',INFO,' as a value of '
         PRINT *,' INFO in inversion of COEFM matrix.'
         PRINT *,' Program aborted.'
         STOP
      ENDIF
C     .
      RETURN
      END
C*********************************************************************

C*********************************************************************
C subroutine MATrix OPeration ****************************************
C Steve Gibbons 23.4.97 Does operation on a two-dimensional array.   C
C                                                Can set equal to a  C
C                       constant; multiply by a constant or have a   C
C                       constant added to it.                        C
C____________________________________________________________________C
C Input variables :-                                                 C
C ===============                                                    C
C  Integer                                                           C
C  -------                                                           C
C     IOP       : Type of operation to be done.                      C
C                    WHOLE MATRIX OPERATIONS                         C
C                  IOP=0  -->  Each element of the matrix = CONST    C
C                  IOP=1  -->  Each el. is multiplied by CONST       C
C                  IOP=2  -->  Each el. is added to CONST            C
C     NDIM1     : First dimension of the matrix.                     C
C     NDIM2     : Second dimension of the matrix.                    C
C  Double Precision                                                  C
C  ----------------                                                  C
C     MAT       : Matrix with dimension ( NDIM1, NDIM2 )             C
C     CONST     : Double precision constant.                         C
C____________________________________________________________________C
C
C*********************************************************************
      SUBROUTINE MATOP ( MAT, CONST, NDIM1, NDIM2, IOP)
      IMPLICIT NONE
C____________________________________________________________________C
C Variable declarations - Parameters ................................C
      INTEGER NDIM1, NDIM2, IOP
      DOUBLE PRECISION MAT ( NDIM1, NDIM2 ), CONST
C____________________________________________________________________C
C Variable declarations - Working variables .........................C
      INTEGER I, J
C____________________________________________________________________C
C START OF PROGRAM **************************************************C
C____________________________________________________________________C
C
C First do case IOP=0 
      IF ( IOP.EQ.0 ) THEN
         DO J = 1, NDIM2
            DO I = 1, NDIM1
               MAT ( I, J) = CONST
            ENDDO
         ENDDO
         RETURN
      ENDIF
C Now do case IOP=1
      IF ( IOP.EQ.1 ) THEN
         DO J = 1, NDIM2
            DO I = 1, NDIM1
               MAT ( I, J) = MAT ( I, J)*CONST
            ENDDO
         ENDDO
         RETURN
      ENDIF
C Now do case IOP=2
      IF ( IOP.EQ.2 ) THEN
         DO J = 1, NDIM2
            DO I = 1, NDIM1
               MAT ( I, J) = MAT ( I, J) + CONST
            ENDDO
         ENDDO
         RETURN
      ENDIF
      PRINT *,' Subroutine MATOP. IOP must be 0,1 or 2.'
      STOP
      END
C*********************************************************************

C--------------
C Subroutine E to UTMS
C Input epoch time E output
C  123456789012345678901234
C 'yyyy-mm-ddThh:mm:ss:mmmm'
C--------------

      SUBROUTINE E2UTMS( DEPOCH, CHUTMS )
      IMPLICIT NONE
C
      REAL*8         DEPOCH
      CHARACTER*(24) CHUTMS
C
      INTEGER        ISACTM( 6 )
      INTEGER        IYEAR
      INTEGER        IJUL 
      INTEGER        IMONTH
      INTEGER        IDOM  
      INTEGER        IHOUR 
      INTEGER        IMINUTE
      INTEGER        ISECOND
      INTEGER        I10000
C
      INTEGER        IERR
C
      CALL SACE2X( DEPOCH, ISACTM )
      IYEAR   = ISACTM( 1 )
      IJUL    = ISACTM( 2 )
      IHOUR   = ISACTM( 3 )
      IMINUTE = ISACTM( 4 )
      ISECOND = ISACTM( 5 )
      I10000  = ISACTM( 6 )
C
      CALL JUL2MD( IYEAR, IJUL, IMONTH, IDOM, IERR )
C
      WRITE ( CHUTMS( 1: 4), '(I4.4)'  ) IYEAR
      CHUTMS( 5: 5)                    = '-'
      WRITE ( CHUTMS( 6: 7), '(I2.2)'  ) IMONTH
      CHUTMS( 8: 8)                    = '-'
      WRITE ( CHUTMS( 9:10), '(I2.2)'  ) IDOM
      CHUTMS(11:11)                    = 'T'
      WRITE ( CHUTMS(12:13), '(I2.2)'  ) IHOUR
      CHUTMS(14:14)                    = ':'
      WRITE ( CHUTMS(15:16), '(I2.2)'  ) IMINUTE
      CHUTMS(17:17)                    = ':'
      WRITE ( CHUTMS(18:19), '(I2.2)'  ) ISECOND
      CHUTMS(20:20)                    = '.'
      WRITE ( CHUTMS(21:24), '(I4.4)'  ) I10000 
C
      RETURN
      END
C
C
C Steve Gibbons
C NGI
C
C SAC epoch time 2 human time
C (EXCEPT THAT it is 10000ths of a second - not millisecond)
C
C ISACTM( 1 ) = year
C ISACTM( 2 ) = Julian day
C ISACTM( 3 ) = hour
C ISACTM( 4 ) = minute
C ISACTM( 5 ) = second
C ISACTM( 6 ) = 10000ths of a second
C
      SUBROUTINE SACE2X( SACEPT, ISACTM )
      IMPLICIT NONE
C
      REAL*8  SACEPT
      INTEGER ISACTM( 6 )
C
      INTEGER NDAYIY
      INTEGER NDAYOR
      INTEGER NDAYS
      INTEGER IY
      INTEGER LEAPYR
      REAL*8  DSEC
C
      NDAYS  = INT( SACEPT/86400.0d0 )
      IF ( SACEPT.LT.0 ) NDAYS = NDAYS - 1
      ISACTM( 3 ) = SACEPT/3600.0d0 - 24.0d0*DBLE( NDAYS )
      ISACTM( 4 ) = SACEPT/60.0d0 - 1440.0d0*DBLE( NDAYS ) -
     1                60.0d0*DBLE( ISACTM( 3 ) )
      DSEC        = SACEPT - 86400.0d0*DBLE( NDAYS ) -
     1                        3600.0d0*DBLE( ISACTM( 3 ) ) -
     2                          60.0d0*DBLE( ISACTM( 4 ) )
      ISACTM( 5 ) = INT( DSEC )
      ISACTM( 6 ) = NINT( 10000.0d0*DSEC ) - 10000*ISACTM( 5 )
      IF ( ISACTM( 6 ).GE.10000 ) ISACTM( 6 ) = 9999
      IF ( ISACTM( 6 ).LT.0    ) ISACTM( 6 ) = 0
C
      NDAYOR = NDAYS
      IF ( NDAYOR.LT.0 ) THEN
        ISACTM( 1 ) = 1969
 72     CONTINUE
        CALL ISLEAP(  ISACTM( 1 ), LEAPYR )
        NDAYIY = 365 + LEAPYR
        NDAYS  = NDAYS + NDAYIY
        IF ( NDAYS.GE.0 ) GOTO 73
        ISACTM( 1 ) = ISACTM( 1 ) - 1
        GOTO 72
 73     CONTINUE
      ENDIF
C
      IF ( NDAYOR.GE.0 ) THEN
        ISACTM( 1 ) = 1970
 82     CONTINUE
        CALL ISLEAP(  ISACTM( 1 ), LEAPYR )
        NDAYIY = 365 + LEAPYR
        IF ( NDAYS.LT.NDAYIY ) GOTO 83
        ISACTM( 1 ) = ISACTM( 1 ) + 1
        NDAYS  = NDAYS - NDAYIY
        GOTO 82
 83     CONTINUE
      ENDIF
C
      ISACTM( 2 ) = NDAYS + 1
C
      RETURN
      END
C
C
C Steve Gibbons
C NGI 2023-09-18
C
C JUL2MD
C 
C Julian day to Month and Day
C
      SUBROUTINE JUL2MD( IYEAR, JUL, IMON, IDOM, IERR )
      IMPLICIT NONE
C
      INTEGER IYEAR, IMON, IDOM, IERR
C
      INTEGER IM, NODIM( 12 ), NODIY, JUL2, JUL3, JUL
      INTEGER LEAPYR
C
      IERR   = 0
      NODIM(  1 ) = 31
      NODIM(  2 ) = 28
      NODIM(  3 ) = 31
      NODIM(  4 ) = 30
      NODIM(  5 ) = 31
      NODIM(  6 ) = 30
      NODIM(  7 ) = 31
      NODIM(  8 ) = 31
      NODIM(  9 ) = 30
      NODIM( 10 ) = 31
      NODIM( 11 ) = 30
      NODIM( 12 ) = 31
C
      CALL ISLEAP( IYEAR, LEAPYR )
      NODIY       = 365        + LEAPYR
      NODIM( 2 )  = NODIM( 2 ) + LEAPYR
C
      IF ( JUL.LT.1 .OR. JUL.GT.NODIY ) THEN
        PRINT *,' Subroutine JUL2MD'
        PRINT *,' JUL = ', JUL
        IERR = 1
        RETURN
      ENDIF
C
      JUL2 = 0
      DO IM = 1, 12
        JUL3 = JUL2 + NODIM( IM )
        IF ( JUL.GT.JUL2 .AND. JUL.LE.JUL3 ) THEN
          IMON = IM
          GOTO 91
        ENDIF
        JUL2 = JUL2 + NODIM( IM )
      ENDDO
 91   CONTINUE
C
      IDOM = JUL
      IF ( IMON.EQ.1 ) RETURN
C
      DO IM = 1, IMON - 1
        IDOM = IDOM - NODIM( IM )
      ENDDO
C
      RETURN
      END
C*********************************************************************
