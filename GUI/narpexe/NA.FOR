CMS$OPTIMIZE:'ON:xp'
      PROGRAM NARP2020
C**********************************************************************
C This Fortran program was originally developed by Prof. Chanan Singh
C (Texas A&M University) and the Associated Power Analysts, Inc.
C**********************************************************************
C  TO COMPILE WITH FTN77 ON PRIME USE:
Cff
C  FTN77 NARP.FTN -SI -BIG -INTL -ZEROISE -SAVE -OPTIMIZE
C  SEG -LOAD
C  LO NARP.BIN
C  LI VAPPLB
C  LI F77LIB
C  LI
C  Q
C**********************************************************************
C  TO COMPILE ON 486 OR 386/387 PC WITH SALFORD FTN77 USE:
C
C  FTN77 NARP/LINK/INTL/SI/SAVE/ZEROISE
C**********************************************************************
C  TO COMPILE ON THE 486 OR 386 PC WITH FULL ERROR CHECKING USE:
C
C  FTN77 NARPE/SI/INTL/ZEROISE/SAVE/FULLCHECK/B NARPE.OBJ
C  FTN77 SEEDER/SI/INTL/ZEROISE/SAVE/B SEEDER.OBJ
C  LINK77 <LINKNARP
C
C  note: NARPE.FOR is the regular narp program with subroutines SEEDER,
C        GSTATE, and LSTATE removed.  These subroutines are put in another
C        file called SEEDER.FOR.  File LINKNARP then has the following data:
C
C  LOAD NARPE.OBJ
C  LOAD SEEDER.OBJ
C  FILE NARPE.EXE
C**********************************************************************
C!MS$DEBUG
      INCLUDE "NARPCOM.txt"
      INCLUDE 'NARPERC.txt'

      CHARACTER(len=100) INPUT_DIR,OUTPUT_DIR
! 使用COMMON块共享字符串变量
      COMMON /GlobalVars/ LEEI_PATH, INPUTB_PATH, INPUTC_PATH, TEMP_PATH,
     &EXTRA_PATH, INTMDT_PATH, DATOUT_PATH, DUMP_PATH,
     &OUTPUT_PATH, TRAOUT_PATH
! 声明字符变量，每个最长200个字符
      CHARACTER(len=200) :: LEEI_PATH, INPUTB_PATH, 
     &INPUTC_PATH, TEMP_PATH, 
     &EXTRA_PATH, INTMDT_PATH, DATOUT_PATH, DUMP_PATH,
     &OUTPUT_PATH, TRAOUT_PATH
      INTEGER I
      CHARACTER*200 ARG
C    ! 获取命令行参数的总数
      IF (IARGC() .LT. 2) THEN
          PRINT *, '错误: 需要至少2个参数。'
          STOP
      END IF
      CALL GETARG(1, INPUT_DIR)
      CALL GETARG(2, OUTPUT_DIR)
C Construct the full path to the input files
      LEEI_PATH = TRIM(INPUT_DIR)//'\LEEI'
      INPUTB_PATH = TRIM(INPUT_DIR) // '\INPUTB'
      INPUTC_PATH = TRIM(INPUT_DIR) // '\INPUTC'
      TEMP_PATH = TRIM(OUTPUT_DIR) // '\TEMP'
      EXTRA_PATH = TRIM(OUTPUT_DIR) // '\EXTRA'
      INTMDT_PATH = TRIM(OUTPUT_DIR) // '\INTMDT'
      DATOUT_PATH = TRIM(OUTPUT_DIR) // '\DATOUT'
      DUMP_PATH = TRIM(OUTPUT_DIR) // '\DUMP'
      OUTPUT_PATH = TRIM(OUTPUT_DIR) // '\OUTPUT'
      TRAOUT_PATH = TRIM(OUTPUT_DIR) // '\TRAOUT'


      ! 程序的其他部分...
      WRITE(*,44)
   44 FORMAT(15X,'NARP Version - March 3, 1996',/)
C
C  CHANGE TO MAKE TU IBM COMPILER WORK
      EX4 = '    '
      EX2 = '  '
C  CHANGE TO MAKE PRIME COMPILER WORK
      SSQ=0.
      XLAST=0.
      IOPN=0
C OPTION ERR INTRODUCED IN OPEN STATEMENTS OF LEEI,INPUTB AND INPUTC

      OPEN(UNIT=8,FILE=LEEI_PATH,STATUS='OLD',ACCESS='SEQUENTIAL')
      
      OPEN(UNIT=18,FILE=TEMP_PATH,STATUS='OLD',ERR=105)
      GO TO 200
 105  CONTINUE
      OPEN(UNIT=18,FILE=TEMP_PATH,STATUS='NEW')
      GO TO 200
 100  WRITE(*,110)
 110  FORMAT(15X,'LOAD DATA FILE LEEI DOES NOT EXIST',/
     *,15X,'PROGRAM ABORTED',/)
      IOPN=IOPN+1
 200  CONTINUE
      OPEN(UNIT=14,FILE=EXTRA_PATH,STATUS='OLD',ACCESS='SEQUENTIAL',
     &ERR=610)
      GO TO 611
 610  CONTINUE
       OPEN(UNIT=14,FILE=EXTRA_PATH,STATUS='NEW',ACCESS='SEQUENTIAL')
 611   CONTINUE
      OPEN(UNIT=10,FILE=INPUTB_PATH,STATUS='OLD',ACCESS='SEQUENTIAL')
      GO TO 400
 300  WRITE(*,115)
 115  FORMAT(15X,'BASE DATA FILE INPUTB DOES NOT EXIST',/,
     *15X,'PROGRAM ABORTED',/)
      IOPN=IOPN+1
 400  CONTINUE
      OPEN(UNIT=11,FILE=INPUTC_PATH,STATUS='OLD',ACCESS='SEQUENTIAL')
      GO TO 600
 500  WRITE(*,120)
 120  FORMAT(15X,'CHANGE DATA FILE INPUTC DOES NOT EXIST',/,
     *15X,'PROGRAM ABORTED',/)
      IOPN=IOPN+1
 600  CONTINUE
C  REMOVED STATUS='NEW' TO GIVE ADDITIONAL FLEXIBILITY ON O/P FILES...GENE P.
      OPEN(UNIT=12,FILE=INTMDT_PATH,STATUS='OLD',ACCESS='SEQUENTIAL'
     &,ERR=615)
      GO TO 616
 615  CONTINUE
      OPEN(UNIT=12,FILE=INTMDT_PATH,STATUS='NEW',ACCESS='SEQUENTIAL')
 616  CONTINUE
      OPEN(UNIT=9,FILE=DATOUT_PATH,STATUS='OLD',ACCESS='SEQUENTIAL',ERR=
     *620)
      GO TO 621
 620  CONTINUE
      OPEN(UNIT=9,FILE=DATOUT_PATH,STATUS='NEW',ACCESS='SEQUENTIAL')
 621  CONTINUE
      OPEN(UNIT=16,FILE=OUTPUT_PATH,STATUS='OLD',ACCESS='SEQUENTIAL',
     *ERR=601)
      GO TO 602
 601  CONTINUE
      OPEN(UNIT=16,FILE=OUTPUT_PATH,STATUS='NEW',ACCESS='SEQUENTIAL')
 602  CONTINUE
      OPEN(UNIT=15,FILE=DUMP_PATH,STATUS='OLD',FORM='UNFORMATTED',
     * ACCESS='SEQUENTIAL',ERR=800)
C IF DUMP EXISTS ASSUME RESTART- C. SINGH DEC 1993
      IRST=1
      GO TO 850
C IF DUMP DOES NOT EXIST OPEN DUMP AND ASSUME A BASE RUN - C. SINGH DEC 1993
 800  IRST=0
      OPEN(UNIT=15,FILE=DUMP_PATH,STATUS='NEW',FORM='UNFORMATTED',
     * ACCESS='SEQUENTIAL')
 850  CONTINUE

      IF(IOPN.NE.0)GO TO 700
      NOTIFY = 20

      JU = 16
      JU1 = 12
      JU2 = 10
      JU3 = 11
      JU4 = 9
      NFCST = 1
      NOERR = 1
      LVLTRC = 0
      NHRSYR = 8760
      WRITE(*,389)
 389  FORMAT(15X,'CALL INITFL',/)
      CALL INITFL

      CALL DATAF1(JU)

      IF(IABORT.NE.0)GO TO 700
      IF(IRST.EQ.0)GO TO 362

      OPEN(UNIT=13,FILE=TRAOUT_PATH,STATUS='OLD',ACCESS='APPEND'
     &,ERR=356)
      GO TO 361

 356  CONTINUE
      IF(IOI.EQ.0)GO TO 359
      WRITE(*,357)
 357  FORMAT(15X,'FOR RESTART TRAOUT DOES NOT EXIST, PROGRAM ABORTED',/)
      GO TO 700

 362  CONTINUE
      OPEN(UNIT=13,FILE=TRAOUT_PATH,STATUS='OLD',ACCESS='SEQUENTIAL',
     *ERR=359)
      GO TO 361

 359  CONTINUE
      OPEN(UNIT=13,FILE=TRAOUT_PATH,STATUS='NEW',ACCESS='SEQUENTIAL')
      WRITE(*,351)
 351  FORMAT(15X,'FILE TRAOUT DOES NOT EXIST, A NEW FILE OPENED.')
 361  CONTINUE

      LSTEP = MAXEUE/20
      CALL INITL
      IFIN=FINISH/8760

C LOGIC FOR DUMPING INTERMEDIATE RESULTS CHANGED
C INDUMP ARRAY NO LONGER REQUIRED
C APRIL 1994 -C. SINGH

      INTVT=INTV
      IF(IRST.EQ.0)GO TO 10

      CALL RSTART

      INTVTT=CLOCK/8760

      WRITE(*,66)
 66   FORMAT(15x,///,15X,'**WARNING ! THIS IS A RESTART CASE.')

      WRITE(*,33)INTVTT
 33   FORMAT(15X,'**NUMBER OF PREVIOUS REPLICATIONS : ',I8,/)

 10   CONTINUE

      CALL CONTRL(RFLAG)

 700  CONTINUE
      CLOSE(UNIT=16)

C  CLOSE REST OF OUTPUT FILES TO PROPERLY PLACE AN EOF MARKER...GENE P. 1/26/89
C CLOSE AND DELETE FILES -C. SINGH DEC 1993
      CLOSE(UNIT=9,STATUS='DELETE')
      CLOSE(UNIT=12,STATUS='DELETE')

      IF(IOI.NE.0)GO TO 950
      CLOSE(UNIT=13,STATUS='DELETE')

 950  CONTINUE
      CLOSE(UNIT=14,STATUS='DELETE')

      IF(IREM.NE.0)GO TO 900
      CLOSE(UNIT=15,STATUS='DELETE')

 900  CONTINUE
      STOP
      END



      BLOCK DATA

      INCLUDE 'NARPERC.txt'
C     INTEGER*2 MAXHR,MXPLHR,INTCH,LNSTAT,LINENO,MARGIN,LNCAP,NLINES
C  MOVED TO ERCCOM TO ELIMINATE COMMON BLOCK MISALIGNMENT ERROR ... GENE 1/26/89
      DATA PLNDST /600*1.0/
      DATA CADJ /20*0.0/
      DATA SADJ /20*0.0/
      DATA LSFLG/20*0/
      DATA BLPA /1800*0.0/
      DATA SUSTAT /96*0.0/
      DATA LOLTHA,LOLGHA,MGNTHA,MGNGHA,LOLTPA,LOLGPA,MGNTPA,MGNGPA
     @/800 * 0/
      DATA LOLTHP,LOLGHP,MGNTHP,MGNGHP,LOLTPP,LOLGPP,MGNTPP,MGNGPP
     @/40 * 0/
      DATA SOLTHA,SOLGHA,SGNTHA,SGNGHA,SOLTPA,SOLGPA,SGNTPA,SGNGPA
     @,SOLSHA,SGNSHA,SOLSPA,SGNSPA/1200 * 0.0/
      DATA SOLTHP,SOLGHP,SGNTHP,SGNGHP,SOLTPP,SOLGPP,SGNTPP,SGNGPP
     @,SOLSHP,SGNSHP,SOLSPP,SGNSPP/60 * 0.0/
      DATA HLOLE,DPLOLE,EUES/1320*0.0/
      DATA XNEWA,XNEWP,SSQA,SSQP/126*0.0/
C  ADDED THE FOLLOWING LINE ... GENE PRESTON
      DATA WOLSHA/20*0./
      END



      SUBROUTINE INITL

C:::::LAST UPDATE: 6/22/88
       INCLUDE 'NARPERC.txt'

C        WRITE (16,1000)
C1000 FORMAT (/,' ',' ENTERING SUBROUT. INITL',/)
      CLOCK = 0.0


C......BEGIN YEAR IN FIRST QUARTER....JQRTR IS NOT USED !(YL 2021)...........

      JQRTR = 1

C......INITIALIZE FLAGS.................................................

      LSSFLG = 0.0
      SFLAG = 1.0

C......SCHEDULE ALL QUARTERLY STATE CHANGES.............................
      ATRIB(2) = 3.0
      ATRIB(1) = 0.5
      CALL FILEM

      ATRIB(2) = 3.0
      ATRIB(1) = QTR(1)
      CALL FILEM
      ATRIB(1) = QTR(2)
      CALL FILEM
      ATRIB(1) = QTR(3)
      CALL FILEM
C...DO NOT PUT ANYTHING BETWEEN THESE TWO FILEM CALLS. NEED THE 8760.
C.....SCHEDULE FIRST CALL TO YEAR
      ATRIB(1) = 8760.
      ATRIB(2) = 4.
      CALL FILEM
C......SCHEDULE FIRST CALL TO SBRTNE WEEK TO SCHEDULE PLANNED OUTAGE....

      ATRIB(1) = 0.5
      ATRIB(2) = 2.0
      CALL FILEM

C......SCHEDULE FIRST MONTE CARLO DRAW..................................

      ATRIB(2) = 1.0
CC    ATRIB(1) = DFLOAT(JSTEP)
      ATRIB(1) =        JSTEP
      CALL FILEM
      RETURN
      END



C!MS$NODEBUG
      SUBROUTINE SEEDER

C:::::LAST UPDATE: 6/18/88

      INCLUDE 'NARPERC.txt'

C.......................................................................
C
C......Each Generator and Each Transmission Line has a dedicated Random
C      Number Seed.  We assert those seeds in this Subrout. A User
C      Defined Global Seed (JSEED) initiates this process
C
C......Draw a Random Number for Each Generator..........................


C%%%  WRITE (16,1000)
C1000 FORMAT(/,' ', 'ENTERING SUBROUTNE SEEDER',/)
      IGSEED(1) = JSEED
      DO 10 I = 1, NUNITS
	    IGSEED(I) = IGSEED(I) * 9649599
	    IF (IGSEED(I) .LT. 0) THEN
	      IGSEED(I) = IGSEED(I) + 2147483647
	      IGSEED(I)=IGSEED(I)+1
	      END IF
	    J = I + 1
	    IGSEED(J) = IGSEED(I)
C!      WRITE (16,1010) I, IGSEED(J)
C 1010   FORMAT(5X,I3,5X,I12)
10     CONTINUE

C......Continue, Seeding Lines..........................................

       ILSEED(1) = IGSEED(NUNITS)
      DO 20 I = 1, NLINES
	      ILSEED(I) = ILSEED(I) * 9649599
	      IF (ILSEED(I) .LT. 0) THEN
	      ILSEED(I) = ILSEED(I) + 2147483647
	      ILSEED(I) = ILSEED(I) + 1
	      END IF
	      J = I + 1
	    ILSEED(J) = ILSEED(I)
C!      WRITE(16,1010) I, ILSEED(J)
   20 CONTINUE

      RETURN
      END


      SUBROUTINE GSTATE

C:::::LAST UPDATE: 6/18/88

      INCLUDE 'NARPERC.txt'
      INCLUDE 'NARPCOM.txt'

C.......................................................................
C      GSTATE determines the available capacity for each
C              generator ... up to NUNITS ...
C              It uses In-Line Code for the Pseudo_Random_
C              Number_Generator
C.......................................................................


C     WRITE(*,1000) NUNITS
C1000 FORMAT(/,' ','ENTERING SUBROUTNE GSTATE, THE # OF UNITS IS',I4)

C......Draw a random number for each unit...............................


      DO 10 I = 1, NUNITS
	     IGSEED(I) = IGSEED(I) * 65539
	     IF (IGSEED(I) .LT. 0) THEN
	      IGSEED(I) = IGSEED(I) + 2147483647
	      IGSEED(I) = IGSEED(I) + 1
         END IF
	     YFL = FLOAT(IGSEED(I))
	     YFL = YFL * .4656613E-9
	     RANDG(I) = YFL

C       WRITE(*,11)YFL
C  11   FORMAT(10X,F8.6)
   10 CONTINUE

C......Determine the State and Available Capacity for Each Generator....

      DO 20 I = 1, NUNITS
	    IF (RANDG(I) .LT. PROBG(I,1)) THEN
	       PCTAVL(I) = 1.0
	     ELSE IF (RANDG(I) .LT. PROBG(I,2)) THEN
	         PCTAVL(I) = DERATE(I)
	     ELSE
	       PCTAVL(I) = 0.0
C         IF (I.EQ.1) WRITE(*,*)'UNIT 1, AREA 1 FAILED'
C         IF (I.EQ.2) WRITE(*,*)'UNIT 2, AREA 1 FAILED'
C         IF (I.EQ.33) WRITE(*,*)'UNIT 1, AREA 2 FAILED'

	     ENDIF
	     AVAIL(I) = PCTAVL(I) * RATING(I) * PLNDST(I)
   20 CONTINUE

C%%% set trace ......................................................
CH
C      IF (CLOCK.EQ.43810.) THEN
C     IF (LVLTRC .GE. 1) THEN
C     IF(JHOUR.EQ.755) THEN
C     WRITE(*,1029) JHOUR,CLOCK
C1029 FORMAT(/'     HOUR =',I5,'     CLOCK =',F12.0)
C      WRITE(*,*) '   UNIT     RANDOM  PROBABILITIES   PLAN    PCT',
C    @            '   RATING      AVAIL'
C      WRITE(*,*) ' ---- ----- ------  -------------   ----    ---',
C    @            '   ------      -----'
C      DO 30 I = 1, NUNITS
C     IJ='    '
C     IF(PLNDST(I).EQ.0.) IJ='MAIN'
C        WRITE(*,1030) I,NAMU(I),NUMP(I),RANDG(I),(PROBG(I,J),J=1,2),
C    @                 PLNDST(I),PCTAVL(I),RATING(I),AVAIL(I),IJ
C1030  FORMAT(I5,1X,A4,A2,2X,F5.4,2(2X,F6.4),2X,F4.2,3X,F4.2,3X,F6.1,2X,
C    1   F9.1,2X,A4)
C 30   CONTINUE
C      END IF
CH
      RETURN
      END




      SUBROUTINE LSTATE

C:::::LAST UPDATE: 6/18/88

      INCLUDE 'NARPERC.txt'

C.......................................................................
C
C      LSTATE establishes the STATUS for each line.
C             It uses In-Line Code for the Pseudo_Random_
C             Number_Generator
C.......................................................................


C     WRITE(*,1000)
C1000 FORMAT(/,' ','ENTERING SUBROUTNE LSTATE')

C......Draw a random number for each line...............................

      DO 10 I = 1, NLINES
	    ILSEED(I) = ILSEED(I) * 65539
	    IF (ILSEED(I) .LT. 0) THEN
	      ILSEED(I) = ILSEED(I) + 2147483647
	      ILSEED(I) = ILSEED(I) + 1
	    END IF
C	WRITE(*,*)I,ILSEED(I)
	    YFL = FLOAT(ILSEED(I))
C	WRITE(*,*)I,YFL
	    YFL = YFL * .4656613E-9
C	WRITE(*,*)I,YFL
	    RANDL(I) = YFL
   10 CONTINUE

C......Determine the STATUS of each line................................

      DO 20 I = 1, NLINES
	    IF (RANDL(I) .LE. PROBL(1,I)) THEN
	      LNSTAT(I) = 1
	    ELSE  IF (RANDL(I) .LE. PROBL(2,I)) THEN
	      LNSTAT(I) = 2
	    ELSE IF (RANDL(I) .LE. PROBL(3,I)) THEN
	      LNSTAT(I) = 3
	    ELSE IF (RANDL(I) .LE. PROBL(4,I)) THEN
	      LNSTAT(I) = 4
	    ELSE IF (RANDL(I) .LE. PROBL(5,I)) THEN
	      LNSTAT(I) = 5
	    ELSE
	      LNSTAT(I) = 6
	    ENDIF
C     WRITE(*,1020) I,LNSTAT(I)
C1020 FORMAT (' ','LINE =',I3,' STATE =',I2)
   20 CONTINUE

      RETURN
      END


C!MS$DEBUG
      SUBROUTINE INITFL

C::::::LAST UPDATE: 6/18/88

      INCLUDE 'NARPERC.txt'

C%%%  WRITE (16,1000)
C1000 FORMAT(/,' ',' ENTERING SUBROUTNE INITFL ')

C......Initialize Pointers into EVNTS Array.............................

      MFA = 1
      IPOINT = 0
      MXCELL = 100
      N = MXCELL - 1

      DO 10 I = 1, MXCELL
	     EVNTS(I) = 0.0
   10 CONTINUE

      DO 20 I = 1, N, 4
	    J = I + 4
	    EVNTS(I) = J
   20 CONTINUE

C!    DO 30 I = 1, 100, 4
C!      WRITE(16,1030) I,(EVNTS(J),J=I,I+3)
C!1030   FORMAT(5X,I4,4(5X,F10.0))
C! 30 CONTINUE
      RETURN
      END



      SUBROUTINE FILEM

C:::::LAST UPDATE: 6/27/88

      INCLUDE 'NARPERC.txt'

C.....PLACE DATA INTO ARRAY EVNTS.......................................

C%%%  WRITE(16,1000) ATRIB(1), ATRIB(2)
C1000 FORMAT (/,' ',' FILING EVENT; ATRIB VECTOR IS ',(2F12.2),/)
C  .....PRINT ARRAR EVNTS
C       WRITE(16,5)IPOINT
C  5    FORMAT(10X,'CONTENTS OF ARRAY EVNTS',/,10X,'IPOINT = ',I4,/)

      K = 0
      NEW = MFA
      EVNTS(NEW+1) = ATRIB(1)
      EVNTS(NEW+2) = ATRIB(2)
      NUMINQ = NUMINQ + 1

C     WRITE(16,1001) IPOINT,MFA,NUMINQ
C1001 FORMAT(' ',' IPOINT = ',I3,' MFA = ',I3,' NUMINQ = ',I3)


C.....REDEFINE FIRST AVAILABLE ENTRY....................................

      MFA = EVNTS(MFA)
C100  IF (IPOINT .GT. 0) THEN      THE 100 IS NOT USED ... GENE P. 1/26/89
      IF (IPOINT .GT. 0) THEN
	    GOTO 110
      END IF

C.....FILE 1 IS EMPTY, SET FIRST POINTER TO NEW ENTRY, SUCCESSOR TO 0...

      IPOINT = NEW
      EVNTS(NEW) = 0
C     WRITE(16,1002) IPOINT, MFA, NEW
C1002  FORMAT(' ','EVNTS ARRAY IS EMPTY, FILING FIRST ENTRY',/,
C    @           'IPOINT = ',I3,'  MFA = ',I3,' NEW = ',I3)

      RETURN

C.....FILE 1 CONTAINS ENTRIES, SYNCHRONIZE..............................

 110  J = IPOINT
      IF (ATRIB(1) .GT. EVNTS(J+1)) THEN
	    GOTO 115
      END IF

C.....NEW ENTRY BECOMES FIRST ENTRY,....................................
C.....RESET FIRST POINTER AND DEFINE SUCCESSOR..........................

      IPOINT = NEW
      EVNTS(NEW) = J
C       DO 11 I=1,50
C       WRITE(16,20)EVNTS(I)
C  11   CONTINUE
C  20   FORMAT(5X,F10.2)
C       WRITE(16,5)IPOINT
      RETURN

C......NEW ENTRY GOES INTO MIDDLE OF FILE...............................

 115  K = J
      J = EVNTS(J)
      IF (J .EQ. 0) THEN
	     GO TO 120
      END IF
      IF (ATRIB(1) .GT. EVNTS(J+1)) THEN
	    GO TO 115
      END IF

C.....FILE INTO MIDDLE - SET SUCCESSOR POINTER..........................

      EVNTS(NEW) = J

C.....SET PREDECESSOR'S SUCCESSOR POINTER...............................

      EVNTS(K) = NEW
C       DO 12 I=1,50
C       WRITE(16,20)EVNTS(I)
C  12   CONTINUE
C       WRITE(16,5)IPOINT
      RETURN

C.....NEW ENTRY BECOMES LAST IN FILE....................................

 120  EVNTS(K) = NEW
      EVNTS(NEW)=0


C       DO 13 I=1,50
C       WRITE(16,20)EVNTS(I)
C  13   CONTINUE
C       WRITE(16,5)IPOINT

      RETURN
      END




      SUBROUTINE REMOVE

C:::::LAST UPDATE: 6/18/88

      INCLUDE 'NARPERC.txt'

C%%%  WRITE(16,1000)
C1000 FORMAT(/,' ',' REMOVING EVENT FROM EVENT LIST',/)

      IF (NUMINQ .LE. 0) THEN
	    WRITE(16,1001)
 1001   FORMAT(' ','ERROR IN REMOVE, EVENTS LIST IS EMPTY')
	    STOP
      END IF

C......REMOVE ENTRY FROM EVNTS ARRAY ...................................

      ATRIB(1) = EVNTS(IPOINT + 1)
      ATRIB(2) = EVNTS(IPOINT + 2)

C%%%  WRITE(16,1009) ATRIB(1), ATRIB(2)
C1009 FORMAT(' ', 'ATRIB VECTOR IS:  ',2F12.2)

      NUMBER = ATRIB(2)
      NUMINQ = NUMINQ - 1

C......UPDATE FILE POINTERS...FIRST UPDATE MFA..........................

      MFA = IPOINT
      EVNTS(IPOINT + 1) = 0.
      EVNTS(IPOINT + 2) = 0.

C......FIRST POSITION POINTER..ITEM ALWAYS REMOVED FROM FIRST POSITION

	  IPOINT = EVNTS(IPOINT)

C     WRITE(16,1010) IPOINT, NUMINQ
C1010 FORMAT(' ',' IPOINT = ',I3,' NUMINQ = ',I3)

      RETURN
      END



      SUBROUTINE CONTRL(RFLAG)

C:::::LAST UPDATE: 6/16/88

      INCLUDE 'NARPERC.txt'

C%%%  WRITE(16,1000)
C1000 FORMAT (/,' ',' ::::::::::::::::: IN CONTRL :::::::::::::::::'/,/)

C......PULLS THE NEXT EVENT FROM FILE AND CHECKS TIME. EVENTS IS CALLED.
C......UNLESS END OF SIMULATION IS REACHED, THEN REPORT IS CALLED.......

   10 IF (IPOINT .EQ. 0) THEN
	    WRITE(16,1001)
 1001   FORMAT(/,' ',' ERROR: POINTER TO EVENT FILE IS 0 AND CONTROL',
     +         ' HAS BEEN CALLED')
C     WRITE (16,1109)
C1109 FORMAT(/,/,' ','ECHO CHECKING EVENTS ARRAY',/,/)

C     DO 88 I = 1, 100, 4
C  88 WRITE(16,1188) (EVNTS(J),J = I, I+3)
C1188 FORMAT(4(5X,F7.2))

	    RETURN
      END IF

      IF (CLOCK .GT. EVNTS(IPOINT+1)) THEN
	    WRITE(16,1010) ATRIB(1), ATRIB(2)
 1010   FORMAT(' ','ERROR: NEXT EVENT TO OCCUR SCHEDULED PRIOR TO ',
     +         'CURRENT TIME. ',/,5X,'ATRIB VECTOR IS ',2(F12.2,5X))

	    WRITE (16,1020) EVNTS(IPOINT+1),EVNTS(IPOINT+2)
 1020   FORMAT(' ','PENDING EVENTS ATRIB VECTOR IS ',2(F12.2,5X))

	    RETURN
      ELSE
	    CONTINUE
      END IF

      IF(RFLAG.EQ.1.0) THEN
       RETURN
      ELSE
       CONTINUE
      END IF

      IF (EVNTS(IPOINT+1) .GT. (FINISH+1.0)) THEN
	    IYEAR = CLOCK/8760.D0
	    CALL REPORT(IYEAR)
	    RETURN
      ELSE
	    CALL REMOVE
	    CLOCK = ATRIB(1)
	    NUMBER = ATRIB(2)

C       WRITE(16,1002) CLOCK,NUMBER
C1002   FORMAT(' ',' SIMULATION CLOCK UPDATED TO: ',F12.0,/,
C    +       ' ',' FOR EVENT TYPE: ',I2)

	    CALL EVENTS(NUMBER,RFLAG)
CH      CALL EVENTS
      END IF

      GO TO 10

      END


      SUBROUTINE EVENTS(NUMBER,RFLAG)
CH      SUBROUTINE EVENTS
      INCLUDE 'NARPERC.txt'

C::::::LAST UPDATE: 6/18/88
      DATA  IERR /0/
C%%%  WRITE(16,1000) NUMBER
C1000 FORMAT(/,' ',' EVENTS CALLED WITH ARGUMENT = ',I2)

      GO TO (1,2,3,4) NUMBER

    1 CALL HOUR
      JHOUR=JHOURT
      RETURN

    2 CALL WEEK
      RETURN

    3 CALL QUARTR
      RETURN

    4 CALL YEAR(IERR,RFLAG)
      IF (IERR.EQ.1) WRITE(*,*) 'ERROR IN SUBROUTINE YEAR'
      RETURN
      END



      SUBROUTINE WEEK
C:::::LAST UPDATE: 7/06/88

      INCLUDE 'NARPERC.txt'

C%%%  WRITE (16,1000)
C1000 FORMAT (' ','SUBROUTNE WEEK CALLED')

      ATRIB(1) = CLOCK + 168
      CALL FILEM
      DO 10 I = 1, NUNITS
	    PLNDST(I) = 1.0
   10 CONTINUE

      JWEEK = JHOUR/168 + 1
C      WRITE(*,*)JWEEK,JHOUR
      IF(JWEEK.GT.52) RETURN

      MT=JPLOUT(JWEEK,1)+1
      IF (MT.EQ.1) RETURN

C     WRITE(16,*)
C     WRITE(16,*)' SCHEDULED OUT THIS WEEK '
C     WRITE(16,*)' ================================================'
C     WRITE (16,1010) JWEEK,(JPLOUT(JWEEK,M),M=2,MT)
C1010  FORMAT (' WEEK ',I3,5X,12(I2,2X))

	  DO 20 M = 2,MT
	    J = JPLOUT(JWEEK,M)
	    PLNDST(J) = 0.0
  20  CONTINUE

      RETURN
      END



       SUBROUTINE FINDLD(NFACT)

C:::::LAST UPDATE: 6/18/88

      INCLUDE 'NARPERC.txt'

C     WRITE(*,1000) JDAY
C1000 FORMAT (/,' ','ENTERING SUBROUTNE FINDLD, JDAY =',I4)
C      WRITE(*,*)JHOUR,JDAY
C......NFACT IS THE NUMBER OF THE FORECAST ERROR FACTOR TO BE USED......
C......CAPREQ(J) IS PASSED TO TRANSMISSION MODULE.......................

      DO 10 J = 1, NOAREA

C       IF (NFACT.GT.1) THEN
C       WRITE(*,1010) J,HRLOAD(J,JHOUR),PKLOAD(J),FCTERR(J,NFACT)
C1010   FORMAT (' ','FOR AREA ',I2,' HRLOAD = ',F7.5,' PKLOAD =',
C    @  F6.0,' FCTERR = ',F6.2)
C       ENDIF

	    CAPREQ(J) = HRLOAD(J,JHOUR)  * FCTERR(J,NFACT)
   10 CONTINUE

C       WRITE (16,1001) CLOCK
C1001   FORMAT(' ',' AT TIME: ',F12.2)

      DO 20 J = 1, NOAREA
	    IF (NFACT.GT.1) THEN
C         WRITE(16,1002) J, CAPREQ(J)
	    ENDIF
C1002   FORMAT(' ',' CAPACITY REQUIRED FOR AREA ',I2,' = ',F12.4)
   20 CONTINUE

      RETURN
      END


      SUBROUTINE FINDTN

C:::::LAST UPDATE: 6/18/88

      INCLUDE 'NARPERC.txt'

C     WRITE(16,1000)
C1000 FORMAT(/,' ',' ENTERING SUBROUTNE FINDTN')

C......INTCH IS MATRIX OF CONTRACT INTERCHANGES.........................
C......JENT IS POINTER IDENTIFYING INVOLVED AREAS.......................
C......TRNSFR STORES NET TRANSFER TO (FROM) REST OF POOL................
C......TRANSFERS TREATED AS FROM HIGHER TO LOWER AREA NUMBER...........

      DO 5 I=1,NOAREA
         TRNSFR(I)=0.
 5    CONTINUE

      DO 10 J2 = 1, NOAREA
	    DO 10 J1 = 1, NOAREA
	      IF (JENT(J1,J2) .GT. 0) THEN
	        JPOINT = JENT(J1,J2)
		    TRNSFR(J1) =TRNSFR(J1)+INTCH(JPOINT,JDAY)
	        TRNSFR(J2) =TRNSFR(J2)-INTCH(JPOINT,JDAY)
	      END IF
   10 CONTINUE

C     DO 20 J = 1, NOAREA
C       WRITE(*,1001) J, TRNSFR(J)
C1001   FORMAT(' ',' AREA: ',I2,' TRANSFER: ',F12.2)
C  20 CONTINUE
      RETURN
      END


      SUBROUTINE QUARTR

      INCLUDE 'NARPERC.txt'

      DATA IQ/0/
C     IQ = 1
      IQ = IQ + 1
C      WRITE(*,1000)IQ
C 1000 FORMAT (' ','ENTERING SUBROUTNE QUARTR',I2)
      IF (IQ .EQ. 5) IQ = 1
      DO 5 I = 1, NOAREA
        SUMAR4= 0.0
        SUMAR3= 0.0
        SUMAR2= 0.0
        SUMAR1= 0.0
        DO 4 J = 1, NOGEN(I)
          SUMAR4 = SUMAR4 + RATES(J,4)
          SUMAR3 = SUMAR3 + RATES(J,3)
          SUMAR2 = SUMAR2 + RATES(J,2)
    4     SUMAR1 = SUMAR1 + RATES(J,1)
C     WRITE(*,6) SUMAR1,SUMAR2,SUMAR3,SUMAR4
    5 CONTINUE

C   6 FORMAT(' ',' AREA RATINGS =',4(F7.1,1X))

      DO 10 I = 1, NUNITS
	    RATING(I) = RATES(I,IQ)
   10   CONTINUE

C      WRITE(*,1001) CLOCK
 1001 FORMAT(' ',' AT TIME: ',F12.2,' QRTRLY RATING CHANGE COMPLETED')
CC    ATRIB(1) = CLOCK + DFLOAT(NHRSYR)
      ATRIB(1) = CLOCK +        NHRSYR

      CALL FILEM

      RETURN
      END



       SUBROUTINE HOUR

C:::::LAST UPDATE: 6/30/88

      INCLUDE 'NARPERC.txt'

      DIMENSION LOAD(20)
      DOUBLE PRECISION DXX
      DATA IFLAG,JFLAG /0,0/
C.......................................................................

CC    ATRIB(1) = CLOCK + DFLOAT(JSTEP)
      ATRIB(1) = CLOCK +        JSTEP
      CALL FILEM


C     WRITE(*,1000)
C1000  FORMAT(/,' ', 'ENTERING SUBROUTNE HOUR')

C......UPDATE HOUR NUMBER...............................................
C     XX = FLOAT(NHRSYR)
      DXX = 8760.
      JHOUR = DMOD(CLOCK,DXX)
      IF(JHOUR.EQ.0)JHOUR=8760
      JJ = MOD(JHOUR,JFREQ)
CH .......
C      JDAY = JHOUR/24+1
      JDAY = (JHOUR-.1)/24 + 1
      IF (JDAY.GT.365) JDAY = 365
CH .......
       JHOURT=JHOUR
       IF(JSTEP.EQ.24)JHOUR=MXPLHR(JDAY)
C       WRITE(*,*)JHOUR,MXPLHR(JDAY)
C......DRAW GENERATOR STATUSES AND SUM CAPACITIES BY AREA...............
C     IF (MOD(JHOUR,1000).EQ.0) WRITE(*,5533) CLOCK
C5533 FORMAT(' ','IN HOUR AT TIME ',F8.0)
CH .......
      IF(JJ.EQ.0)JJ=1
      IF (JJ.EQ.1. OR . JFREQ.EQ.1) THEN
	    CALL GSTATE
	    CALL SUMCAP
	    CALL LSTATE
	    CALLS = 0.
	    CALL FINDTN
      ENDIF

C STMULT(I,1) IS FORWARD DIRECTION ADJUSTMENT
C STMULT(I,2) IS BACKWARD DIRECTION ADJUSTMENT

	DO 22 I=1,NLINES
	  DO 22 J=1,2
   22   STMULT(I,J) = 1.0

C        WRITE(*,123) MXCRIT,(JCRIT(I),I=1,20)
C  123   FORMAT(' ',' MXCRIT =',I4,/,'JCRIT ARRAY IS',/,
C     @  10I7,/,10I7)

	JPNT = 1

	DO 23 I =1,MXCRIT
	  IF (JCRIT(JPNT).EQ.0) THEN
	    JPNT = JPNT+1
	  ELSE
	    NOCRIT = JCRIT(JPNT+1)
	    CRTVAL = 0.0

	    DO 124 II = 1,NOCRIT

C  HAD TO ADD THIS TEST ... PROGRAM LOGIC ERROR ... GENE P. 1/28/89

	      IF(JCRIT(JPNT+1+II).GT.0.AND.JCRIT(JPNT+1+II).LE.500)THEN
	        CRTVAL = CRTVAL + AVAIL(JCRIT(JPNT+1+II))
	      ELSE
	        IF(IGENE.EQ.0) WRITE(*,*)'LOGIC ERROR ... GENE P.'
	        IGENE=1
	      ENDIF
  124   CONTINUE

	    IF (CRTVAL.EQ.0.0) THEN

C MODIFY LINE CAPACITY - FORWARD

		  STMULT(JCRIT(JPNT),1)=FLOAT(JCRIT(JPNT+NOCRIT+3))/100.
		  STMULT(JCRIT(JPNT),2)=FLOAT(JCRIT(JPNT+NOCRIT+4))/100.

C                 WRITE(*,1024) STMULT(JCRIT(JPNT),1),JCRIT(JPNT)
C1024             FORMAT(' ',' CAPACITY REDUCED TO',F5.3,' FOR',I3)

	    ENDIF

	    JPNT=JPNT+NOCRIT+5

	  ENDIF
23    CONTINUE

C      DO 99 I=1,3
C      WRITE(*,*)STMULT(I,1),STMULT(I,2)
C 99   CONTINUE
C     WRITE(*,1003) NFCST
C1003 FORMAT(' ',' THE NUMBER OF FORECASTS IS',I2)


	  DO 30 NST = 1, NFCST

C 1010   FORMAT (' ','FINDLD CALLED WITH ARGUMENT OF',I3)

	    CALL FINDLD(NST)
	    FLAG = 0.0
	    FLAG2 = 0.0

	    DO 10 J = 1, NOAREA

C          IF (NST.GT.1) THEN
C          WRITE(*,1015) SYSCON(J), CAPREQ(J)
C          ENDIF
C 1015     FORMAT(' ','SYSCON = ',F8.1,'CAPREQ = ',F8.1)

	      MARGIN(J) = IFIX(SYSCON(J) - CAPREQ(J))
	      JMJ = IFIX(SYSOWN(J) - CAPREQ(J))
	      IF (JMJ .LT. 0) FLAG2 = 1.
	      IF (MARGIN(J) .LT. 0) THEN
	         FLAG = FLAG + 1.0
	         IT = J
	      END IF

C         IF (FLAG .GT. 0.) THEN
C         WRITE(*,1020) (MARGIN(JJ),JJ=1,NOAREA),FLAG
C         ENDIF
C1020     FORMAT (' ','MARGINS ARE',3(I8,3X),' FLAG IS',F4.0)
   10   CONTINUE

C         IF (FLAG .GT. 0.) THEN
C         WRITE(*,1020) (MARGIN(JJ),JJ=1,NOAREA),FLAG
C         ENDIF

          IF (FLAG .EQ. 0.0) THEN
C        WRITE (16,2224)
C2224    FORMAT (' ','RETURNING: NO LOSS')
	         RETURN
          ELSE
          END IF

          NSHRT = IFIX(FLAG)
          IF (NSHRT .EQ. NOAREA) GO TO 6767

          IF (FLAG .GT. 1.0) THEN
	        CONTINUE

C         IF (CALLS .NE. 0.) THEN

          ELSE
	        NEED = ABS(MARGIN(IT))
	        DO 20 J = 1, NOAREA
	          IF (MARGIN(J) .GT. 0) THEN
	            N = LINENO(J,IT)
	            IF (N .GT. 0) THEN
		          M =(LNSTAT(N)-1)*3+2
		          IF (LP(N,2).EQ.J) THEN
		             XX = BLPA(N,M)
		             LNCAP(N) = IFIX(XX)
		          ELSE
		             XX = BLPA(N,M+1)
		             LNCAP(N) = IFIX(XX)
		          ENDIF
	              MXHELP = MIN(MARGIN(J),LNCAP(N))
	              NEED = NEED - MXHELP
	            END IF
	          END IF
   20       CONTINUE

C ********************************************************************
C   HEURISTIC NETWORK FLOW CHECK (SIMPLIFIED TRANSPORTATION MODEL)
C   TO ENABLE, CHANGE THE CH'S TO BLANKS
CH             IF (NEED .LE. 0) THEN
C             WRITE (16,2222)
C2222         FORMAT (' ','RETURNING. ONE AREA SHORT & HELP FEASIBLE.')
CH               RETURN
CH             END IF
C   END OF HEURISTIC NETWORK FLOW CHECK
C ********************************************************************

          END IF

	      DO 24 J = 1, NOAREA

C            WRITE(16,1024) MARGIN(J),SYSCON(J),CAPREQ(J),TRNSFR(J),
C    @                      SADJ(J)
C1024        FORMAT(5X,I8,4(5X,F10.2))

	        IF (MARGIN(J).LT.0) TEMPFL = 1.
   24     CONTINUE

C         WRITE(*,1038) IFLAG,JFLAG
C1038     FORMAT(' CALLING TRANSM, IFLAG = ',I1,' JFLAG = ',I1)

	     JFLAG=0
	     CALL TRANSM(IFLAG,JFLAG)
	     CALLS = 1.
	     IF (IFLAG .EQ. 0) IFLAG = 1
	     IF(JFLAG.EQ.0)RETURN
	     TEMPFL=0.

C        WRITE(16,*)
C        WRITE(16,1026) CLOCK
C1026    FORMAT(' TXMOD CALLED AT TIME: ',F6.0)
C        WRITE(16,*)
C        WRITE(16,*) '     MARGIN       SYSCON         CAPREQ         ',
C    @               'TRNSFR        SADJ'
C        WRITE(16,*) '     --------     ----------     ----------     ',
C    @               '---------     ----------'

	     DO 25 J = 1, NOAREA
	       ZZ =  - SADJ(J)
	       MARGIN(J) = IFIX(ZZ)

C            WRITE(16,1025) MARGIN(J),SYSCON(J),CAPREQ(J),TRNSFR(J),
C    @                      SADJ(J)
C1025        FORMAT(5X,I8,4(5X,F10.2))

	       IF (MARGIN(J).LT.0) TEMPFL = 1.
   25    CONTINUE

C             WRITE(*,2626)(MARGIN(J),J=1,NOAREA)
C 2626    FORMAT(5X,3I8,/)

	     IF (TEMPFL.EQ.0) RETURN

 6767    CALL HRSTAT(NST)

C          WRITE(16,1030)JHOUR,JDAY,MXPLHR(JDAY)
C1030      FORMAT(' JHOUR = ',I5,' JDAY = ',I5,' MXPLHR = ',I5)
CH ........

	     IF (JHOUR.EQ.MXPLHR(JDAY)) CALL PKSTAT(NST)
   30 CONTINUE

      RETURN
      END






      SUBROUTINE SUMCAP

C:::::LAST UPDATE: 6/18/88

      INCLUDE 'NARPERC.txt'

C.......................................................................
C
C      SUMCAP determines the total available capacity for
C             each system.
C.......................................................................


C     WRITE(*,1000)
C1000 FORMAT(/,' ',' ENTERING SUBROUTNE SUMCAP')


C......Split Capacity by Ownership......................................


      DO 10 I = 1, NUNITS
C       WRITE(*,11)I,(CAPOWN(J,I),J=1,NOAREA)
C  11   FORMAT(' ',' UNIT',I3,' OWNERSHIP:',3(F5.2,1X))
	    DO 10 J = 1, NOAREA
	      CAPAVL(J,I) = AVAIL(I) * CAPOWN(J,I)
   10 CONTINUE

C......Sum the Capacity Owned by System.................................

C     WRITE(*,*) '     SYSOWN           CAPAVL'
C     WRITE(*,*) '     -----------      -----------'
      DO 25 J = 1, NOAREA
	     SYSOWN(J) = 0.0
	     DO 20 I = 1, NUNITS
   20      SYSOWN(J) = SYSOWN(J) + CAPAVL(J,I)
C          WRITE(*,1020) SYSOWN(J),CAPAVL(I,J)
C 1020     FORMAT(5X,2(F10.2,5X))
   25 CONTINUE

C      ICOUNT = 0
C      DO 30 J = 1, NOAREA
C        SYSCON(J) = 0.0
C        DO 30 I = 1, NOGEN(J)
C          ICOUNT = ICOUNT + 1
C   30     SYSCON(J) = SYSCON(J) + AVAIL(ICOUNT)

      DO 29 I=1,NOAREA
 29     SYSCON(I)=0.0

      DO 30 I=1,NUNITS
        J=CAPCON(I)
 30     SYSCON(J)=SYSCON(J)+AVAIL(I)

      DO 35 I=1,NOAREA
        TRNSFJ(I)=SYSCON(I)-SYSOWN(I)
 35   CONTINUE

C       WRITE(*,1130)(AVAIL(31),AVAIL(63),AVAIL(95))
C       WRITE(*,1130)(TRNSFJ(I),I=1,NOAREA)
C 1130  FORMAT(10X,3(F8.1,3X))
C      WRITE(*,1110) (SYSCON(J),J=1,NOAREA)
C      WRITE(*,1120) (SYSOWN(J),J=1,NOAREA)
C 1110 FORMAT (' ','CAPACITIES CONTROLLED = ',3(F8.1,3X))
C 1120 FORMAT (' ','CAPACITIES OWNED =      ',3(F8.1,3X))

      RETURN
      END



      SUBROUTINE PKSTAT(NST)

C:::::LAST UPDATE: 7/09/88

      INCLUDE 'NARPERC.txt'


C.......................................................................


C     WRITE(16,1212) NST,CLOCK
C1212  FORMAT(' ',' PKSTAT CALLED; NST = ',I3,/,' ',' CLOCK =',F7.0)


	  SUM = 0.0
	  FLAG = 0.0
	  DO 10 J = 1, NOAREA
	    SUM = SUM + FLOAT(MARGIN(J))
10    CONTINUE

C.....SOMEBODY HAS A LOSS, ELSE WE WOULDN'T BE HERE............
C.....UPDATE POOL PK LOLE......................................

	  IF (SUM .GE. 0.) THEN
	     LOLTPP(NST) = LOLTPP(NST) + 1
      ELSE
	      LOLGPP(NST) = LOLGPP(NST) + 1
      ENDIF

      DO 20 J = 1, NOAREA
	     X = MARGIN(J)
	     IF (X .LT. 0.0) THEN
C           LSFLG(J) = 1
	        FLAG = 1.0
	        IF (SUM .GT. 0.0) THEN
	           LOLTPA(J,NST) = LOLTPA(J,NST) + 1
	           MGNTPA(J,NST) = MGNTPA(J,NST) + IFIX(-X)
	           MGNTPP(NST) = MGNTPP(NST) + IFIX(-X)
	        ELSE
	           LOLGPA(J,NST) = LOLGPA(J,NST) + 1
	           MGNGPA(J,NST) = MGNGPA(J,NST) + IFIX(-X)
	           MGNGPP(NST) = MGNGPP(NST) + IFIX(-X)
	        END IF
	     END IF
20    CONTINUE


       RETURN
       END




      SUBROUTINE HRSTAT(NST)

C:::::LAST UPDATE: 8/30/88

      INCLUDE 'NARPERC.txt'


C.......................................................................

C       WRITE(13,999) NST,CLOCK
C  999  FORMAT (' ',' HRSTAT CALLED; NST = ',I3,/,' ', 'CLOCK = ',F10.0)

C       WRITE(13,14)(MARGIN(I),I=1,3)
C 14     FORMAT(5X,3(I6,2X))
	SUM = 0.0
	FLAG = 0.0
	DO 10 J = 1, NOAREA
	  SUM = SUM + FLOAT(MARGIN(J))
   10  CONTINUE

C.....SOMEBODY HAS A LOSS, ELSE WE WOULDN'T BE HERE............
C.....UPDATE POOL PK LOLE......................................

       IF (SUM .GE. 0.) THEN
	     LOLTHP(NST) = LOLTHP(NST) + 1
       ELSE
	     LOLGHP(NST) = LOLGHP(NST) + 1
       ENDIF

      DO 20 J = 1, NOAREA
	    X = MARGIN(J)
	    IF (X .LT. 0.0) THEN
	      LSFLG(J) = LSFLG(J)+1

C         WRITE(13,888)(LSFLG(K),K=1,NOAREA)
C 888     FORMAT(2X,15(I4,1X))

	      FLAG = 1.0
	      IF (SUM .GT. 0.0) THEN
	        LOLTHA(J,NST) = LOLTHA(J,NST) + 1

C             WRITE(*,887)J,LOLTHA(J,NST)
C  887        FORMAT(' ',' AREA',I3,' LOLE YTD',I4)

	        MGNTHA(J,NST) = MGNTHA(J,NST) + IFIX(-X)
	        MGNTHP(NST) = MGNTHP(NST) + IFIX(-X)
	      ELSE
	        LOLGHA(J,NST) = LOLGHA(J,NST) + 1
	        MGNGHA(J,NST) = MGNGHA(J,NST) + IFIX(-X)
	        MGNGHP(NST) = MGNGHP(NST) + IFIX(-X)
	      END IF
	    END IF
   20 CONTINUE

C             WRITE(*,888)J,LOLGHA(J,NST)
C  888        FORMAT(' ',' AREA',I3,' LOLE YTD',I4)

      RETURN
      END



C      DESCRIPTION OF VARIABLES IN COMMON/TM
C      LP(I,J)  I  ENTRY  NUMBER
C         LP(I,1)  LINE NUMBER SET=I
C         LP(I,2)  STARTING NODE
C         LP(I,3)  ENDING NODE
C      BLPA(I,J)   CONTAINS THE DATA ON ADMITTANCE,CAP AND
C      AND BACKWARD CAP OF LINE AT POSITION I IN LP(I,J).
C         BLPA(I,1),BLPA(I,2),BLPA(I,3)  ADMITTANCE(NEG NUMBER),
C                                         CAP IN FORWARD DIRECTION
C                                         AND CAP IN BACKWARD DIRECTION
C                                         FOR STATE 1 OF LINE.
C         BLPA(I,4),BLPA(I,5),BLPA(I,6)   ABOVE DATA FOR STATE 2.
C           REPEAT FOR SIX STATES.IN THE LAST STATE  DO NOT USE ZERO ADM
C           AND ZERO CAPS. INSTEAD USE -.1 AND .1,.1 FOR ADM AND CAPS.
C         BN(I,J)   BN(I,1)  IS THE NODE NUMBER AND BN(I,4) IS THE
C         CONSTRAINT ON SUM OF FLOWS AT THE NODE.
C         BN(I,2) BEFORE CALLING TM2 BN(I,2) IS THE LOAD AT THE NODE AND
C         RETURN IT GIVES THE NET INJECTIONS.
C         BN(I,3) BEFORE CALLING TM2 REPRESENTS THE INPUT  NET INJECTION
C         AND AT RETURN GIVES THE LOAD CURTAILMENTS.
C         NR   IS THE REFERENCE NODE.
C         NLS  0 FOR LOSS SHARING AND 1 FOR NO LOSS SHARING.
C         CAPREQ(J)   LOAD IN AREA J
C
C
C      IFLAG  0 FOR FIRST CALL OF TRANSM AND 1 FOR SUBSEQUENT CALLS.
C      JFLAG  0 FOR TRANSFERS ONLY AND 1 FOR FULL STUDY.IF THERE IS LOAD
C      AT ANY BUS WITH THE POSSIBLE TRANSFERS PROGRAM GOES INTO FULL STU
C
C      FLOWS(I)  CONTAINS LINE FLOWS COMPUTED BY TRANSM.
C



       SUBROUTINE TRANSM(IFLAG,JFLAG)
       INCLUDE 'NARPERC.txt'
       DOUBLE PRECISION BLP
       REAL INJ,INJB,LOD,LODC,INJ1
       DIMENSION BLP(100,3),BNS(20,6)

       LCLOCK=CLOCK
       JHOUR=JHOUR
       IOI=IOI
       NN=NOAREA
       NL=NLINES

       IF(IFLAG.EQ.1)GO TO 200
         DO 100 I=1,NL
           BLP(I,1)=BLPA(I,1)
           BLP(I,2)=BLPA(I,2)
           BLP(I,3)=BLPA(I,3)
 100     CONTINUE
         CALL TM1(BN,LP,BLP,BLP0,NN,NL,BB,ZB,LT,NR)
 200   CONTINUE

       DO 250 I=1,NL
         L=(LNSTAT(I)-1)*3
         BLP(I,1)=BLPA(I,L+1)
         BLP(I,2)=BLPA(I,L+2)*STMULT(I,1)
         BLP(I,3)=BLPA(I,L+3)*STMULT(I,2)
 250   CONTINUE

       IF(JFLAG.EQ.1)GO TO 500
         NLST=NLS
         NLS=1
         DO 300 I=1,NN
           BN(I,3)=TRNSFR(I)+TRNSFJ(I)
           BN(I,2)=0.
           BNS(I,1)=BN(I,3)
           BNS(I,2)=BN(I,2)
           BNS(I,5)=SYSCON(I)
           BNS(I,6)=CAPREQ(I)
           X=SYSCON(I)-CAPREQ(I)

           IF(X.GE.0)GO TO 301
             IF(X.GT.BN(I,3))BN(I,3)=X
           GO TO 300
 301       CONTINUE

           IF(BN(I,3).LT.0.)BN(I,3)=0.
 300     CONTINUE

         CALL TM2(BN,LP,BLP,BLP0,NN,NL,BB,ZB,LT,NR,FLOW,NLS,BNS,LCLOCK
     *,JHOUR,IOI,JFLAG,INDIC)

         NLS=NLST
         KFLAG=1

C       READ STAGE 1 RESULTS FROM IMSL
C       READ(14,*)LCL
C 401   FORMAT(5X,'TRANSFERS ',I6,5X,I6 /)
C       IF(LCL.NE.LCLOCK)GO TO 407
C       IF(INDIC.EQ.0)GO TO 407
C       DO 405 I=1,NN
C       READ(14,*)BN(I,2)
C 405   CONTINUE
C       DO 406 I=1,NL
C       READ(14,*)FLOW(I)
C 406   CONTINUE
C 407   CONTINUE
C       IF(LCL.NE.LCLOCK)BACKSPACE 14

         DO 400 I=1,NN
           SADJ(I)=0.
           SADJ(I)=SYSCON(I)-CAPREQ(I)-BN(I,2)
           IF(SADJ(I).LT.0.)KFLAG=0
 400     CONTINUE

         IF(KFLAG.EQ.1)GO TO 1000

           JFLAG=1
           DO 450 I=1,NN
             CADJ(I)=BN(I,2)*NLS
 450       CONTINUE

 500   CONTINUE

       IF(NLS.EQ.0)GO TO 610
         DO 600 I=1,NN
           BN(I,3)=SYSCON(I)-CAPREQ(I)-CADJ(I)
           BN(I,2)=CAPREQ(I)
           BNS(I,1)=BN(I,3)
           BNS(I,2)=BN(I,2)
           BNS(I,4)=BN(I,4)
 600     CONTINUE

         DO 700 I=1,NL
           J=LP(I,2)
           K=LP(I,3)
           BLP(I,2)=BLP(I,2)-FLOW(I)
           BLP(I,3)=BLP(I,3)+FLOW(I)
           IF(BLP(I,2).LT.0.0) BLP(I,2)=0.
           IF(BLP(I,3).LT.0.0) BLP(I,3)=0.
           BN(J,4)=BN(J,4)+FLOW(I)
           BN(J,5)=BN(J,5)-FLOW(I)
           BN(K,4)=BN(K,4)-FLOW(I)
           BN(K,5)=BN(K,5)+FLOW(I)
 700     CONTINUE
       GO TO 620
 610   CONTINUE

       DO 621 I=1,NN
         BN(I,3)=SYSCON(I)-CAPREQ(I)-CADJ(I)
         BN(I,2)=CAPREQ(I)
         BNS(I,1)=BN(I,3)
         BNS(I,2)=BN(I,2)
         BNS(I,4)=BN(I,4)
 621   CONTINUE

 620   CONTINUE

       CALL TM2(BN,LP,BLP,BLP0,NN,NL,BB,ZB,LT,NR,FLOW,NLS,BNS,LCLOCK
     *,JHOUR,IOI,JFLAG,INDIC)

 2626  FORMAT(10X,3F10.1)

       DO 720 I=1,NN
         BN(I,4)=BNS(I,4)
         BN(I,5)=BNS(I,4)
 720   CONTINUE

       DO 650 I=1,NN
         SADJ(I)=BN(I,2)-SYSCON(I)+CAPREQ(I)+CADJ(I)
 650   CONTINUE

C       WRITE(13,1001)(SADJ(I),I=1,3)
C 1001  FORMAT(5X,3(F10.2,2X))
 1000  CONTINUE
C       IF(LCLOCK.EQ.5472)STOP
       RETURN
       END



C      SUBROUTINE THETAC
C      THIS SUBROUTINE GENERATES VECTOR THETC OF BUS ANGLES
C      FOR ALL BUSES INCLUDING REF BUS.
C      THET(I) CONTAINS ANGLE AT NODE I OF THE ADM MATRIX
C      LT(I) CONTAINS THE ACTUAL NODE CORRESPONDING TO NODE
C      I OF THE ADM MATRIX

       SUBROUTINE THETAC(THET,THETC,LT,NN)
       DIMENSION THET(20),THETC(20),LT(20)
       NX=NN-1

       DO 100 I=1,NN
  100    THETC(I)=0.

       DO 200 I=1,NX
         J=LT(I)
         THETC(J)=THET(I)
  200  CONTINUE

       RETURN
       END



C*      SUBROUTINE ADMITB
C       GENERATES YBUS MATRIX
C       LP(I,J) J=1  LINE NUMBER
C               J=2  FROM NODE
C               J=3  TO NODE
C       BB(.,.) YBUS MATRIX



      SUBROUTINE ADMITB(LP,BLP,BB)
      DOUBLE PRECISION BB,BLP
      DIMENSION LP(100,3),BLP(100,3),BB(20,20)

      DO 100 I = 1,20
      DO 100 J = 1,20
100   BB(I,J)=0.0

      I=1
150   J=LP(I,2)
	  K=LP(I,3)
	  BB(J,K)=BB(J,K) + BLP(I,1) * (-1.0)
      BB(K,J)=BB(J,K)
	  BB(J,J)=BB(J,J)+BLP(I,1)
	  BB(K,K)=BB(K,K)+BLP(I,1)
      I=I+1

C       END OF DATA DETECTED BY LINE #0

	     IF (LP (I,1) .EQ. 0) GO TO 200
        GO TO 150

200      CONTINUE
	     RETURN
	     END


C       SUBROUTINE XIMPA
C       MODIFIES ZBUS MATRIX IF ZIJ ADDED BETWEEN NI,NJ
C       ZB IS INPUT ZBUS MATRIX
C       Z  IS MODIFIED ZBUS MATRIX
C
C
	     SUBROUTINE XIMPA(ZB,Z,ZIJ,NI,NJ,NN)
	      DOUBLE PRECISION ZB,Z,ZIJ
        DIMENSION ZB(20,20), Z(20,20),ZCOL(20)

C        WRITE(*,*)NI,NJ,ZIJ
C        DO 100 I=1,NN
C        WRITE(*,*)(ZB(I,J),J=1,NN)
C 100    CONTINUE

	     DO 250 I= 1, NN
250       ZCOL(I)=ZB(I,NI)-ZB(I,NJ)

	     ZN1=ZB(NI,NI)+ZB(NJ,NJ)-2.0*ZB(NI,NJ)-ZIJ

	      DO 300 I=1,NN
	     DO 300 J=1,NN
	        Z(I,J)=ZB(I,J)-(ZCOL(I)*ZCOL(J))/ZN1
300   CONTINUE

C       DO 101 I=1,NN
C       WRITE(*,*)(Z(I,J),J=1,NN)
C101    CONTINUE

	RETURN
	END



C       SUBROUTINE XIMPAR
C       MODIFIES ZBUS MATRIX IF ZIJ ADDED BETWEEN NI AND REF
C
C
	SUBROUTINE XIMPAR(ZB,Z,ZIJ,NI,NN)
	DOUBLE PRECISION ZB,Z,ZIJ
	DIMENSION ZB(20,20),Z(20,20),ZCOL(20)
C        WRITE(*,*)NI,ZIJ
C        DO 100 I=1,NN
C        WRITE(*,*)(ZB(I,J),J=1,NN)
C 100    CONTINUE

	DO 250 I=1,NN
 250    ZCOL(I)=ZB(I,NI)

	ZN1=ZB(NI,NI)-ZIJ
	DO 300 I=1,NN
	  DO 300 J=1,NN
	    Z(I,J)=ZB(I,J)-(ZCOL(I)*ZCOL(J))/ZN1
 300   CONTINUE

C        DO 101 I=1,NN
C        WRITE(*,*)(Z(I,J),J=1,NN)
C 101    CONTINUE

	RETURN
	END


C       SUBROUTINE THETA
C       COMPUTES NODE ANGLES FOR NODES OF ZBUS MATRIX
C        Z   ZBUS MATRIX
C        INJ  BUS INJECTION VECTOR
C        THET NODE ANGLES COMPUTED
C        NN   NODES
C
	SUBROUTINE THETA(Z,THET,INJ,NN)
	DOUBLE PRECISION Z
	REAL INJ
	DIMENSION Z(20,20), INJ(20), THET(20)
	DO 350 I=1,NN
	  THET(I)=0.
	  DO 350 J=1,NN
350     THET(I)=THET(I)+Z(I,J)*INJ(J)
	RETURN
	END



C       SUBROUTINE FLO
C       COMPUTES LINE FLOWS
C       BLP(I,J)
C                 J=1  ADM OF LINE
C                 J=2  CAP FROM I TO J
C                 J=3  CAP IN OPP
C       FLOW VECTOR OF LINE FLOWS
C
	SUBROUTINE FLO(LP,BLP,FLOW,THET,SFLOW)
	DOUBLE PRECISION BLP
	DIMENSION LP(100,3),BLP(100,3),FLOW(100),THET(20),SFLOW(20)
	I=1
400      J=LP(I,2)
	     K=LP(I,3)
	     FLOW(I)=(THET(J)-THET(K))*BLP(I,1)
	     SFLOW(K)=SFLOW(K)+FLOW(I)
	     SFLOW(J)=SFLOW(J)-FLOW(I)
	     I=I+1
	     IF (LP(I,1) .EQ. 0) GO TO 450
	     GO TO 400
450     RETURN
	     END




C       * SUBROUTINE TO TAKE INVERSE
C       Y CONTAINS INVERSE
	SUBROUTINE MINV(Y,N)
	DOUBLE PRECISION Y,Z,PIV,X,PM
	DIMENSION Y(20,20),Z(20,40)

	DO 100 I=1,N
	  DO 100 J=1,N
	    Z(I,J)=Y(I,J)
	    Z(I,J+N)=0.0
	    IF(I.EQ.J)Z(I,J+N)=1.0
100    CONTINUE

	     DO 200 I=1,N
	      PIV=Z(I,I)

	       DO 300 J=1,N
	      IF(I.EQ.J)GO TO 450

	         PM=Z(J,I)/PIV

	      DO 400 K=I+1,N+I
	          Z(J,K)=Z(J,K)-Z(I,K)*PM
400       CONTINUE

450     CONTINUE
300   CONTINUE

200    CONTINUE

	     DO 600 I=1,N
	       X=Z(I,I)
	     DO 600 J=1,N
	       Y(I,J)=Z(I,J+N)/X
600     CONTINUE

	     RETURN
	     END



C       SUBROUTINE ADMITR
C       REMOVES REF NODE
C       BB AT INPUT CONTAINS ENTIRE YBUS MAT
C       BB AT OUTPUT CONTAINS YBUS MAT WITHOUT REF BUS
C       BN(I,J)  J=1   BUS NUM
C                J=2   LOAD AT BUS I
C                J=3   GEN AT BUS I
C                J=4   MAX FLOW ALLOWED AT BUS I
C       INJB   VECTOR OF BUS INJECTIONS
C       INJ    VECTOR OF BUS INJECTIONS WITHOUT REF BUS
C       NR     REF BUS
C
	SUBROUTINE ADMITR(BB,NR,NN,BN,LT)
	DOUBLE PRECISION BB,BT
	DIMENSION BB(20,20),BT(20,20),BN(20,5),LT(20)
	REAL INJ(20),INJB(20)

	NX=NN-1
	II=0

	DO 110 I=1,NN
	  IF(I.EQ.NR)GO TO 110
	  II=II+1
	  LT(II)=BN(I,1)
	  III=0
	  DO 100 J=1,NN
	    IF(J.EQ.NR)GO TO 100
	    III=III+1
	    BT(II,III)=BB(I,J)
100   CONTINUE
110    CONTINUE

	DO 200 I=1,NN
	  DO 200 J=1,NN
  200   BB(I,J)=0.0

	DO 300 I=1,NX
	  DO 300 J=1,NX
 300    BB(I,J)=BT(I,J)

	LT(NX+1)=NR

	RETURN
	END



C       SUBROUTINE ASSIGN
C       ASSIGNS INJECTIONS AT BUSES SO AS TO BALANCE
C       POSITIVE AND NEGATIVE INJECTIONS
C       INJ VECTOR OF INPUT INJECTIONS
C       INJ1  VECTOR OF MODIFIED INJECTIONS
C       LOD   VECTOR OF LOADS
C       LODC  VECTOR OF LOAD CURTAILMENTS
C       NLS   0    LOSS SHARING
C             1    NO LOSS SHARING
C
	SUBROUTINE ASSIGN(INJ,INJ1,LOD,LODC,LT,NN,NLS)
	REAL INJ,INJ1,LOD,LODC
	DIMENSION INJ(20),INJ1(20),LOD(20),LODC(20),LT(20)

	DO 50 I=1,NN
	  INJ1(I)=INJ(I)
 50    CONTINUE

	SUMP=0.
	SUMN=0.

	DO 100 I=1,NN
C        WRITE(*,*)SUMP,SUMN,INJ1(I)
	  IF(INJ1(I).GT.0)GO TO 150
	    SUMN=SUMN+INJ1(I)
	    GO TO 100
 150  SUMP=SUMP+INJ1(I)
 100  CONTINUE

	SUMN=ABS(SUMN)
	IF(SUMN.GT.SUMP)GO TO 300
	  DO 200 I=1,NN
	    IF(INJ1(I).LE.0.)GO TO 200
	      INJ1(I)=INJ1(I)*SUMN/SUMP
 200    CONTINUE

	  GO TO 400

 300  CONTINUE

	IF(NLS.EQ.0)GO TO 1000

	  DO 350 I=1,NN
	    IF(INJ1(I).GT.0)GO TO 350
	      PINT=INJ1(I)
	      INJ1(I)=INJ1(I)*SUMP/SUMN
	      J=LT(I)
	      LODC(J)=INJ1(I)-PINT
 350    CONTINUE

	  GO TO 400

 1000 CONTINUE

	SUML=0.
	DO 1400 I=1,NN
	  SUML=SUML+LOD(I)
 1400   CONTINUE

	DIFF=SUMN-SUMP
	DO 1500 I=1,NN
	  J=LT(I)
	  LODC(J)=LOD(J)*DIFF/SUML
	  INJ1(I)=INJ1(I)+LODC(J)
 1500   CONTINUE

 400    CONTINUE
	RETURN
	END



C       SUB CONNLS
C       FORMULATES CONSTRAINTS FOR NO LOSS SHARING
C
	SUBROUTINE CONNLS(A,BC,INJ,INJB,NX,XOB,IBAS,NR,LT,B,BLP,LP,NL,
     *  M,N,BN,B1,LOD,NLS,XOBI,BS,TAB,N1)
	DOUBLE PRECISION XOB,A,B,XOBI,BS,TAB,BC,BLP
	REAL INJ,INJB,LOD
	DIMENSION A(200,250),BC(20,20),INJ(20),IBAS(250),XOB(250)
     *  ,IEQ(20),BN(20,5),LOD(20),XOBI(2,250),BS(200),TAB(200,250)
	DIMENSION LT(20),BLP(100,3),B(200),B1(200),LP(100,3),INJB(20)

	NMAX2=200
	NMAX1=250
	DO 50 I=1,NMAX1
	  IBAS(I)=0.
	  XOB(I)=0.
	  XOBI(1,I)=0.
	  XOBI(2,I)=0.
	  DO 60 J=1,NMAX2
	    A(J,I)=0.
 60   CONTINUE
 50   CONTINUE

      DO 55 I=1,NMAX2
         B(I)=0.
	  B1(I)=0.
	  BS(I)=0.
 55   CONTINUE

	DO 100 I=1,NX
      II=I
      J=LT(I)
100   IEQ(J)=I

	IEQ(NR)=II+1
	NXD=2*NX

	DO 200 I=1,NX
	  JJ=1
	  DO 300 J=1,NX
        A(I,JJ)=BC(I,J)
        A(I,JJ+1)=-BC(I,J)
        JJ=JJ+2
300   CONTINUE
      A(I,NXD+I)=-1.0
      B(I)=INJ(I)
200   CONTINUE

	  NX1=NX+1

      DO 400 I=1,NL
        IF (LP(I,3) .NE. NR) GO TO 410
          K=LP(I,2)
          J1=IEQ(K)
          I1=J1*2-1
          A(NX1,I1)=A(NX1,I1)+BLP(I,1)
          A(NX1,I1+1)=A(NX1,I1+1)-BLP(I,1)
410     CONTINUE
        IF(LP(I,2).NE.NR)GO TO 400
          K=LP(I,3)
          J1=IEQ(K)
          I1=J1*2-1
          A(NX1,I1)=A(NX1,I1)+BLP(I,1)
          A(NX1,I1+1)=A(NX1,I1+1)-BLP(I,1)
400   CONTINUE

      NP=2*NX+1
      A(NX1,NX+NP)=1
      B(NX1)=INJB(NR)
      NP1=3*NX+1

      DO 600 I=1,NP1
        A(NX1,I)=-A(NX1,I)
600   CONTINUE

      IF ( B(NX1) .GT.0) GO TO 500
        XOB(NP1)=1.
500   CONTINUE

      NXD=2*NX
      DO 700 I=1,NX1
        IF ( B(I) .GT. 0) GO TO 700
        XOB(NXD+I)=1.0
700   CONTINUE

      NP1=3*NX+1

      IF(NLS.EQ.0)GO TO 850

      DO 800 I=1,NX1
        A(I+NX1,NP1+I)=1
        A(I+NX1,NXD+I)=1
        B(I+NX1)=ABS( B(I))
800   CONTINUE
      GO TO 910

850   CONTINUE
      DO 860 I=1,NX1
        A(I+NX1,NP1+I)=1
        A(I+NX1,NXD+I)=1
        IF(B(I+NX1).GT.0)GO TO 870
          J=LT(I)
          B(I+NX1)=LOD(J)
          GO TO 860
870     CONTINUE
        J=LT(I)
        B(I+NX1)=B(I+NX1)+LOD(J)
860   CONTINUE

910   CONTINUE

      NX2=NX1+NX1
      NP2=2*NX+2*NX1
      NP3=NP2+NL

	  DO 900 I=1,NL
        I1=NX2+I
        I2=I1+NL
        J1=LP(I,2)
        J2=LP(I,3)
        J4=IEQ(J1)
        J5=IEQ(J2)
        J42=J4*2
        J41=J42-1
        J52=J5*2
        J51=J52-1
        IF(J4.GT.NX)GO TO 950
          A(I1,J41)=BLP(I,1)
          A(I1,J42)=-BLP(I,1)
          A(I2,J41)=-BLP(I,1)
          A(I2,J42)=BLP(I,1)
 950    CONTINUE
        IF(J5.GT.NX)GO TO 960
          A(I1,J51)=-BLP(I,1)
          A(I1,J52)=BLP(I,1)
          A(I2,J51)=BLP(I,1)
          A(I2,J52)=-BLP(I,1)
 960    CONTINUE
        A(I1,NP2+I)=1
        A(I2,NP3+I)=1
        B(I1)=BLP(I,2)
        B(I2)=BLP(I,3)
 900  CONTINUE

      NXL=NP3+NL
      NXD1=NXD+1
      M=2*NX1+2*NL
      N=NXL

      DO 1300 I=1,NX1
        DO 1400 J=1,NXD
          K=LT(I)
          A(I+M,J)=A(I,J)
          B(I+M)=BN(K,5)
          A(I+M+NX1,J)=-A(I,J)
          B(I+M+NX1)=BN(K,4)
 1400   CONTINUE
        A(I+M,I+N)=1.
        A(I+M+NX1,I+N+NX1)=1.
 1300 CONTINUE

      M=M+2*NX1
      N=N+2*NX1

      DO 1050 I=1,M
        B1(I)=B(I)
 1050 CONTINUE

      DO 1000 I=1,NX1
        IF(B(I).GT.0.)B(I)=0.
 1000 CONTINUE

      DO 1200 I=1,M
        IF(B(I).GE.0)GO TO 1200
        DO 1210 J=1,N
          A(I,J)=-A(I,J)
 1210   CONTINUE
        B(I)=-B(I)
 1200 CONTINUE

      DO 1500 I=1,NX1
        A(I,I+N)=1.
        XOBI(1,I+N)=1.0
 1500 CONTINUE

      DO 1600 I=1,N
        XOBI(2,I)=XOB(I)
 1600 CONTINUE

      N=N+NX1
      NXD2=NXD1+NX1
      II=0

      DO 1100 I=NXD2,N
        II=II+1
        IBAS(II)=I
 1100 CONTINUE

      NX2=NX1+1
      J=0

      DO 1700 I=NX2,M
        J=J+1
        DO 1800 K=1,N
          TAB(J,K)=A(I,K)
 1800   CONTINUE
        BS(J)=B(I)
 1700 CONTINUE

      DO 1900 I=1,NX1
        J=J+1
        DO 1950 K=1,N
          TAB(J,K)=A(I,K)
 1950   CONTINUE
        BS(J)=B(I)
 1900 CONTINUE

      DO 2000 I=1,M
        DO 2000 J=1,N
          A(I,J)=TAB(I,J)
 2000 CONTINUE

      N1=N-NX1
      RETURN
      END



       SUBROUTINE  CONLS(A,BC,INJ,INJB,NX,XOB,IBAS,NR,LT,B,BLP,LP,NL,
     * M,N,BN,B1,LOD,NLS,XOBI,BS,TAB,N1)
       DOUBLE PRECISION XOB,A,B,XOBI,BS,TAB,BC,BLP
       REAL INJ,INJB,LOD
       DIMENSION A(200,250),BC(20,20),INJ(20),IBAS(250),XOB(250)
     * ,IEQ(20),BN(20,5),LOD(20),XOBI(2,250),BS(200),TAB(200,250)
       DIMENSION LT(20),BLP(100,3),B(200),B1(200),LP(100,3),INJB(20)

       NX1=NX+1
       NP1=3*NX+1
       NP=2*NX+1
       NP2=NP1+NX1
       NMAX1=250
       NMAX2=200
       DO 50 I=1,NMAX1
         IBAS(I)=0.
         XOB(I)=0.
         XOBI(1,I)=0.
         XOBI(2,I)=0.
         DO 60 J=1,NMAX2
           A(J,I)=0.
 60      CONTINUE
 50    CONTINUE

! Added by Yongli 09.2022 for rigorous initialization of BS
		DO 55 I=1,NMAX2
		  B(I)=0.
		  B1(I)=0.
		  BS(I)=0.
55    CONTINUE
! Added by Yongli 09.2022 for rigorous initialization

       DO 100 I=1,NX
         II=I
         J=LT(I)
100      IEQ(J)=I

       IEQ(NR)=II+1
       NXD=2*NX
       DO 200 I=1,NX
         JJ=1
         DO 300 J=1,NX
           A(I,JJ)=BC(I,J)
           A(I,JJ+1)=-BC(I,J)
           JJ=JJ+2
300      CONTINUE
         A(I,NXD+I)=-1.0
         J=LT(I)
         B(I)=-LOD(J)
         A(I,NP1+I)=-1.
200    CONTINUE

      NX1=NX+1
      DO 400 I=1,NL
        IF (LP(I,3) .NE. NR) GO TO 410
          K=LP(I,2)
          J1=IEQ(K)
          I1=J1*2-1
          A(NX1,I1)=A(NX1,I1)+BLP(I,1)
          A(NX1,I1+1)=A(NX1,I1+1)-BLP(I,1)
410     CONTINUE
        IF(LP(I,2).NE.NR)GO TO 400
          K=LP(I,3)
          J1=IEQ(K)
          I1=J1*2-1
          A(NX1,I1)=A(NX1,I1)+BLP(I,1)
          A(NX1,I1+1)=A(NX1,I1+1)-BLP(I,1)
400   CONTINUE

      A(NX1,NX+NP)=1
      B(NX1)=-LOD(NR)
      A(NX1,NP1+NX1)=1.
      DO 600 I=1,NP2
        A(NX1,I)=-A(NX1,I)
600   CONTINUE

      NXD=2*NX
      DO 700 I=1,NX1
        XOB(NXD+I)=1.0
700   CONTINUE
      DO 860 I=1,NX1
        A(I+NX1,NP2+I)=1
        A(I+NX1,NXD+I)=1
        J=LT(I)
        B(I+NX1)=LOD(J)
860   CONTINUE

      NX2=NX1+NX1
      NP2=2*NX+2*NX1
      DO 910 I=1,NX1
        A(I+NX2,NP1+I)=1.
        A(I+NX2,NP2+NX1+I)=1.
        J=LT(I)
        B(I+NX2)=LOD(J)+INJ(I)
 910  CONTINUE
      NX3=3*NX1
      NP22=NP2+2*NX1
      NP3=NP22+NL

      DO 900 I=1,NL
        I1=NX3+I
        I2=I1+NL
        J1=LP(I,2)
        J2=LP(I,3)
        J4=IEQ(J1)
        J5=IEQ(J2)
        J42=J4*2
        J41=J42-1
        J52=J5*2
        J51=J52-1
        IF(J4.GT.NX)GO TO 950
          A(I1,J41)=BLP(I,1)
          A(I1,J42)=-BLP(I,1)
          A(I2,J41)=-BLP(I,1)
          A(I2,J42)=BLP(I,1)
 950    CONTINUE
        IF(J5.GT.NX)GO TO 960
          A(I1,J51)=-BLP(I,1)
          A(I1,J52)=BLP(I,1)
          A(I2,J51)=BLP(I,1)
          A(I2,J52)=-BLP(I,1)
 960    CONTINUE
        A(I1,NP22+I)=1
        A(I2,NP3+I)=1
        B(I1)=BLP(I,2)
        B(I2)=BLP(I,3)
 900  CONTINUE

      NXL=NP3+NL
      NXD1=NXD+1
      M=3*NX1+2*NL
      N=NXL

      DO 1300 I=1,NX1
        DO 1400 J=1,NXD
          K=LT(I)
          A(I+M,J)=A(I,J)
          B(I+M)=BN(K,4)
          A(I+M+NX1,J)=-A(I,J)
          B(I+M+NX1)=BN(K,5)
 1400   CONTINUE
        A(I+M,I+N)=1.
        A(I+M+NX1,I+N+NX1)=1.
 1300 CONTINUE

      M=M+2*NX1
      N=N+2*NX1

      DO 1050 I=1,M
        B1(I)=B(I)
 1050 CONTINUE

      DO 1000 I=1,NX1
        IF(B(I).GT.0.)B(I)=0.
 1000 CONTINUE

      DO 1200 I=1,M
        IF(B(I).GE.0)GO TO 1200
        DO 1210 J=1,N
          A(I,J)=-A(I,J)
 1210   CONTINUE
        B(I)=-B(I)
 1200 CONTINUE

      DO 1500 I=1,NX1
        A(I,I+N)=1.
        XOBI(1,I+N)=1.0
 1500 CONTINUE
      DO 1600 I=1,N
        XOBI(2,I)=XOB(I)
 1600 CONTINUE

      N=N+NX1
      NXD2=NXD1+NX1+NX1
      II=0

      DO 1100 I=NXD2,N
        II=II+1
        IBAS(II)=I
 1100 CONTINUE

      NX2=NX1+1
      J=0
      DO 1700 I=NX2,M
        J=J+1
        DO 1800 K=1,N
          TAB(J,K)=A(I,K)
 1800   CONTINUE
        BS(J)=B(I)
 1700 CONTINUE

      DO 1900 I=1,NX1
        J=J+1
        DO 1950 K=1,N
          TAB(J,K)=A(I,K)
 1950   CONTINUE
        BS(J)=B(I)
 1900 CONTINUE

      DO 2000 I=1,M
        DO 2000 J=1,N
          A(I,J)=TAB(I,J)
 2000 CONTINUE

      N1=N-NX1
      RETURN
      END



      SUBROUTINE LINP(M,N,A,B,XOB,XOBI,IBAS,BS,NXT,TAB,LCLOCK,JFLAG,N1)
      DOUBLE PRECISION TAB,BS,P,XOB,CB,PY,PI,PROD,A,B,XOBI,RAT,RAT1
     *,CC,CM,CABA,DIFF,TABT,BS1,WORK,ZWORK,DET,BNUM,BAB,RCOND
      DIMENSION A(200,250),B(200),TAB(200,250),BS(200),P(200),PI(2
     *00),CB(200),XOB(250),PY(200),XOBI(2,250),TABT(200,250),BS1
     *(200),WORK(250),ZWORK(250),DET(2),RIBS(250)
      INTEGER IBAS(250),IBASF(250),IPVT(250)
      COMMON /PRIME1/ P,PI,CB,PY,TABT,BS1,WORK,ZWORK,DET,RIBS,IBASF,IPVT

	  DO 45 I=1,M
        BS1(I)=BS(I)
 45   CONTINUE

      IR=0
      INV=0
      DO 50 I=1,N
        XOB(I)=XOBI(1,I)
        IBASF(I)=0
 50   CONTINUE

      IPH=1
      DO 55 I=1,200
        DO 55 J=1,250
          TAB(I,J)=0.
 55   CONTINUE

      DO 100 I=1,M
        K=IBAS(I)
        IBASF(K)=1
        DO 200 J=1,M
 200      TAB(I,J)=0.
        TAB(I,I)=1.
        CB(I)=XOB(K)
 100  CONTINUE

  110  CONTINUE
      N=N1

      CALL RM(M,CB,TAB,PY)

 111  CONTINUE

      CM=0.
C      WRITE(14,*)M,N
C      DO 301 I=1,N
C 301  RIBS(I)=111.0

      DO 300 I=1,N
        IF(IBASF(I).EQ.1)GO TO 300
        CALL RC(M,PY,A,I,PROD)
        CC=XOB(I)-PROD
        CABA=DABS(CC)
        IF(CABA.LT.0.1E-6)CC=0.

C      RIBS(I)=CC
C      IC=C*1000.
C      C=IC/1000.
C      WRITE(14,*)C

        DIFF=CC-CM
        DIFF=DABS(DIFF)
        IF(DIFF.LT.0.1E-06)GO TO 300
        IF(CC.LT.CM)IND=I
        IF(CC.LT.CM)CM=CC
 300  CONTINUE

      IF(CM.LT.0.)GO TO 400
      IF(IR.EQ.1)GO TO 1000

      DO 305 I=1,M
        IMAX=0
        K=IBAS(I)
        DO 305 J=1,M
 305      TABT(J,I)=A(J,K)

C      WRITE(14,*)LCLOCK,IPH
C      WRITE(14,308)(IBAS(I),I=1,M)

 308  FORMAT(5X,'BASE',15(I3,1X)/15(I3,1X))
      CALL DGECO(TABT,200,M,IPVT,RCOND,ZWORK)
      CALL DGEDI(TABT,200,M,IPVT,DET,WORK,01)
      CALL RM(M,CB,TABT,PY)

      DO 307 I=1,N
        IF(IBASF(I).EQ.0)GO TO 307
        CALL RC(M,PY,A,I,PROD)
        CC=XOB(I)-PROD
        CC= DABS(CC)
        IF(CC.LT..1E-8)GO TO 307
        IMAX=500
 307  CONTINUE

      IF(IMAX.EQ.0)GO TO 113
      WRITE(14,144)INV,LCLOCK
 144  FORMAT(5X,'INVERSIONS =',I2,'LCLOCK =',I6)
 113  CONTINUE

      IF(IMAX.EQ.500)INV=INV+1

      DO 405 I=1,M
        BS(I)=0.
        DO 405 J=1,M
          BS(I)=BS(I)+TABT(I,J)*BS1(J)
 405      TAB(I,J)=TABT(I,J)

	  IR=1

      GO TO 111

 400  CONTINUE
      IR=0
      CALL MC(M,IND,TAB,A,P)
      ICOUN=0
C      WRITE(14,*)M
C      RAT1=-1.

      DO 500 I=1,M
      IF(P(I).LE.0.1E-06)GO TO 500
      BNUM=BS(I)
      BAB=DABS(BNUM)
      IF(BAB.LT..1E-12)BNUM=0.
      RAT=BNUM/P(I)
      ICOUN=ICOUN+1
C      WRITE(14,*)RAT
      IF(ICOUN.EQ.1)RAT1=RAT
      IF(ICOUN.EQ.1)IPIV=I
      IF(ICOUN.EQ.1)GO TO 500
      IF(RAT.GE.RAT1)GO TO 505
      IPIV=I
      RAT1=RAT
      GO TO 500
 505  CONTINUE
      IF(IPH.EQ.2)GO TO 501
      IF(BS(I).NE.0)GO TO 501
      K=IBAS(I)
      IF(XOB(K).EQ.1.)IPIV=I
 501  CONTINUE
 500  CONTINUE

C      IF(RAT1.GE.-1.E-08)GO TO 509
C      WRITE(*,5011)RAT1
C 5011 FORMAT(5X,'SOLUTION IS UNBOUNDED',E13.6,/)
C 509  CONTINUE

      DO 700 I=1,M
        IF(P(I).EQ.0)GO TO 700
        IF(I.EQ.IPIV)GO TO 700
        DO 800 J=1,M
 800      TAB(I,J)=TAB(I,J)-TAB(IPIV,J)*P(I)/P(IPIV)
        BS(I)=BS(I)-BS(IPIV)*P(I)/P(IPIV)
 700  CONTINUE

      DO 600 I=1,M
        TAB(IPIV,I)=TAB(IPIV,I)/P(IPIV)
 600  CONTINUE

      BS(IPIV)=BS(IPIV)/P(IPIV)
      CB(IPIV)=XOB(IND)
      K1=IBAS(IPIV)
      IBASF(K1)=0
      IBASF(IND)=1
      IBAS(IPIV)=IND
      SUMO=0.

      DO 1050 I=1,M
        K=IBAS(I)
        SUMO=SUMO+BS(I)*XOB(K)
1050  CONTINUE

      GO TO 110

1000  CONTINUE
      IF(IPH.EQ.2)GO TO 2000
C      N=N-NXT

	  DO 1100 I=1,N
        XOB(I)=XOBI(2,I)
 1100 CONTINUE

C      WRITE(*,1170)(XOB(I),I=1,N)
 1170 FORMAT(5X,15F5.0)

      DO 1200 I=1,M
        K=IBAS(I)
        CB(I)=XOB(K)
 1200 CONTINUE

      IPH=2
      GO TO 110

 2000 CONTINUE

C      WRITE(14,*)(RIBS(I),I=1,N)
C      WRITE(14,2001)
C 2001 FORMAT(5X,'VALUES OF MARGINAL COSTS FOR NON BASIC VARIABLES',/)
      RETURN
      END


      SUBROUTINE RM(M,CB,TAB,PY)
      DOUBLE PRECISION TAB,CB,PY
      DIMENSION CB(200),TAB(200,250),PY(200)
      DO 100 I=1,M
        PY(I)=0.
        DO 200 J=1,M
 200      PY(I)=PY(I)+CB(J)*TAB(J,I)
 100  CONTINUE
      RETURN
      END


      SUBROUTINE RC(M,PY,A,IND,PROD)
      DOUBLE PRECISION PY,A,PROD
      DIMENSION A(200,250),PY(200)
      PROD=0.
      DO 100 I=1,M
        PROD=PROD+PY(I)*A(I,IND)
 100  CONTINUE
      RETURN
      END


      SUBROUTINE MC(M,IN,TAB,A,CO)
      DOUBLE PRECISION TAB,CO,A
      DIMENSION TAB(200,250),A(200,250),CO(200)
      DO 100 I=1,M
        CO(I)=0.
        DO 200 J=1,M
 200      CO(I)=CO(I)+TAB(I,J)*A(J,IN)
 100  CONTINUE
      RETURN
      END


C     ------ USELESS -- YONGLI
      SUBROUTINE MC1(M,I,TAB,B,BS)
      DOUBLE PRECISION TAB,BS,B
      DIMENSION TAB(200,250),B(200),BS(200)
      DO 100 I=1,M
        BS(I)=0.
        DO 200 J=1,M
 200      BS(I)=BS(I)+TAB(I,J)*B(J)
 100  CONTINUE
      RETURN
      END


      SUBROUTINE NET(B,B1,RES,IBAS,BS,M,N)
      DOUBLE PRECISION BS,B
      DIMENSION B(200),B1(200),RES(250,3),IBAS(250),BS(200)

	  NX1=N-M

	  DO 100 I=1,N
        DO 100 J=1,3
 100      RES(I,J)=0.

	  DO 200 I=1,M
        K=IBAS(I)

        IF(K.LE.NX1) GO TO 210
		  K1=K-NX1
		  RES(K,1)=-BS(I)
          GO TO 200

 210    CONTINUE
        RES(K,1)=BS(I)
 200  CONTINUE

	  DO 300 I=1,M
        I1=I+NX1
        IF(B1(I).LT.0.)RES(I1,2)=1.
        RES(I1,1)=RES(I1,1)+B(I)
 300  CONTINUE

      RETURN
      END



      SUBROUTINE TM1(BN,LP,BLP,BLP0,NN,NL,BB,ZB,LT,NR)
      DOUBLE PRECISION ZB0,BB,ZB,BLP,BLP0
      REAL INJ,LOD,INJB
      DIMENSION BN(20,5),LP(100,3),BLP(100,3),BLP0(100)
     *,BB(20,20),ZB(20,20),LT(20)
     *,ZB0(20,20)

      MULT=1
      NX=NN-1

      CALL ADMITB(LP,BLP,BB)
      CALL ADMITR(BB,NR,NN,BN,LT)

      DO 200 I=1,NL
		BLP0(I)=BLP(I,1)
 200  CONTINUE

      DO 300 I=1,NX
		DO 300 J=1,NX
 300  		ZB0(I,J)=BB(I,J)

C       DO 210 I=1,NX
C         WRITE(16,215)(BB(I,J),J=1,NX)
C 210  CONTINUE
C 215  FORMAT(2X,9(F7.2,1X)/2X,9(F7.2,1X))

      CALL MINV(ZB0,NX)

      DO 400 I=1,NX
		DO 400 J=1,NX
 400      ZB(I,J)=ZB0(I,J)

      RETURN
      END



      SUBROUTINE TM2(BN,LP,BLP,BLP0,NN,NL,BB,ZB,LT,NR,FLOW,NLS,BNS,
     *LCLOCK,JHOUR,IOI,JFLAG,INDIC)

      COMMON /GENS/ AVAIL(600),CAPAVL(20,600),CAPOWN(20,600),
     @DERATE(600),ICLASS(600),IDATES(600,4),IGSEED(601),JPLOUT(52,120),
     @JNTOWN(600),NOGEN(20),NUNITS,PCTAVL(600),PLNDST(600),
     @RANDG(600),RATES(600,4),RATING(600),SYSCON(20),SYSOWN(20),
     +PROBC(9,2),PROBG(600,2),CAPCON(600)

      COMMON/PRIM1/A,XOB,XOBI,TAB,IBAS
      DOUBLE PRECISION A,XOB,BS,B,XOBI,TAB,BB,ZB,Z,BT,ZT,BLP,BLP0
     *,BIJ,ZIJ
      REAL INJB,INJ,LOD,LODC,INJ1
      DIMENSION BN(20,5),LP(100,3),BLP(100,3),BLP0(100)
     *,BB(20,20),ZB(20,20),LT(20),IEQ(20),BT(20,20),
     *ZT(20,20),BBM(20,20),FLOW(100),RES(250,3),LODC(20)
     *,LOD(20),INJ(20),INJB(20),INJ1(20),Z(20,20)
      DIMENSION A(200,250),XOB(250),IBAS(250),B1(200)
     *,BS(200),B(200),THET(20),THETC(20),XOBI(2,250),TAB(200,250)
     *,SFLOW(20),BNS(20,6),NNTAB(600)

      DO 49 I=1,20
		SFLOW(I)=0.
 49   CONTINUE

      MULT=0
      NX=NN-1

      DO 50 I=1,NX
		II=I
		J=LT(I)
 50   	IEQ(J)=I

	  IEQ(NR)=II+1
      DO 100 I=1,NN
		INJB(I)=BN(I,3)-BN(I,2)*MULT
		LOD(I)=BN(I,2)
 100  CONTINUE

      DO 200 I=1,NN
		J=LT(I)
		INJ(I)=INJB(J)
 200  CONTINUE

      DO 300 I=1,NX
		DO 300 J=1,NX
			BT(I,J)=BB(I,J)
 300  		ZT(I,J)=ZB(I,J)

      DO 400 I=1,NL
        D=BLP(I,1)-BLP0(I)
        IDD=D*1000.
        IF(IDD.EQ.0)GO TO 400
        NI=LP(I,2)
        NJ=LP(I,3)

        IF((NI.EQ.NR).OR.(NJ.EQ.NR))GO TO 410

        NII=IEQ(NI)
        NJJ=IEQ(NJ)
        BIJ=BLP(I,1)

        IF(BIJ.EQ.0.)GO TO 431
          ZIJ=-1/BIJ
          CALL XIMPA(ZB,Z,ZIJ,NII,NJJ,NN)
          CALL ADMITM(BB,NII,NJJ,BIJ,NX)
          DO 430 I1=1,NX
            DO 430 J1=1,NX
              ZB(I1,J1)=Z(I1,J1)
 430      CONTINUE

 431    CONTINUE

C      DO 303 IXX=1,NX
C      WRITE(*,302)(BB(IXX,IJJ),IJJ=1,NX)
C 303  CONTINUE

       BIJ=-BLP0(I)
       ZIJ=-1/BIJ

C      DO 301 IXX=1,NX
C      WRITE(*,302)(BB(IXX,IJJ),IJJ=1,NX)
C 301  CONTINUE
C 302  FORMAT(5X,10F8.2/10F8.2)

       CALL XIMPA(ZB,Z,ZIJ,NII,NJJ,NN)
       CALL ADMITM(BB,NII,NJJ,BIJ,NX)

	   DO 420 I1=1,NX
         DO 420 J1=1,NX
           ZB(I1,J1)=Z(I1,J1)
 420   CONTINUE

       GO TO 400

 410   IF(NI.EQ.NR)NI=NJ
       NII=IEQ(NI)
       BIJ=BLP(I,1)

       IF(BIJ.EQ.0.)GO TO 400
       ZIJ=-1/BIJ

	   CALL XIMPAR(ZB,Z,ZIJ,NII,NN)
       CALL ADMREF(BB,NII,BIJ,NX)

	   DO 460 I1=1,NX
         DO 460 J1=1,NX
           ZB(I1,J1)=Z(I1,J1)
 460   CONTINUE

       BIJ=-BLP0(I)
       ZIJ=-1/BIJ

	   CALL XIMPAR(ZB,Z,ZIJ,NII,NN)
       CALL ADMREF(BB,NII,BIJ,NX)

	   DO 450 I1=1,NX
         DO 450 J1=1,NX
           ZB(I1,J1)=Z(I1,J1)
 450   CONTINUE

 400  CONTINUE


      CALL ASSIGN(INJ,INJ1,LOD,LODC,LT,NN,NLS)
      CALL THETA(ZB,THET,INJ1,NX)
      CALL THETAC(THET,THETC,LT,NN)
      CALL FLO(LP,BLP,FLOW,THETC,SFLOW)

      INDIC=0

      DO 500 I=1,NL
        IF(FLOW(I).LT.0)GO TO 510
          IF(FLOW(I).GT.BLP(I,2))INDIC=1

		  GO TO 500

 510    FF=FLOW(I)

        FF=ABS(FF)
        DIRF=BLP(I,3)
        IF(FF.GT.DIRF)INDIC=1
 500  CONTINUE

      DO 511 I=1,NN
        IF(SFLOW(I).LT.0)GO TO 512
        IF(SFLOW(I).GT.BN(I,4))INDIC=1

		GO TO 511

 512    FF=SFLOW(I)
        FF=ABS(FF)
        DIRF=BN(I,5)
        IF(FF.GT.DIRF)INDIC=1
 511  CONTINUE


C      WRITE(*,*)INDIC
      IF(INDIC.EQ.1)GO TO 600

      DO 550 I=1,NN
        J=LT(I)
C       WRITE(*,*)I,J
        BN(J,2)=INJ1(I)
        BN(I,3)=LODC(I)
 550  CONTINUE

      INL=0
      GO TO 1000

 600  CONTINUE

      IF(NLS.EQ.0)GO TO 1200
      CALL CONNLS(A,BB,INJ,INJB,NX,XOB,IBAS,NR,LT,B,BLP,LP,NL,M,N
     *,BN,B1,LOD,NLS,XOBI,BS,TAB,N1)

C       DO 1400 I=1,M
C       WRITE(*,1410)(A(I,J),J=1,N),B(I)
C 1400  CONTINUE
C 1410  FORMAT(12F6.2/12F6.2)
C       DO 1411 I=1,M
C       B(I)=B(I)/10.
C       B1(I)=B1(I)/10.
C       BS(I)=BS(I)/10.
C       DO 1411 J=1,NX+NX
C       A(I,J)=A(I,J)/10.
C       TAB(I,J)=TAB(I,J)/10.
C 1411  CONTINUE

      NXT=NX+1

      CALL LINP(M,N,A,B,XOB,XOBI,IBAS,BS,NXT,TAB,LCLOCK,JFLAG,N1)
      CALL NET(B,B1,RES,IBAS,BS,M,N)

      NXD1=NX*2+1
      NXD2=NXD1+NX

      DO 700 I=NXD1,NXD2
        II=I-NXD1+1
        J=LT(II)
        BN(J,2)=-RES(I,1)
        BN(J,3)=-BN(J,3)+BN(J,2)
        IF(BN(J,3).LT.0)BN(J,3)=0.
 700  CONTINUE

      NXD1=4*NX+3
      NXD2=4*NX+2+NL
      J=0

      DO 800 I=NXD1,NXD2
        J=J+1
        FLOW(J)=RES(I,1)
 800  CONTINUE

      IF(IOI.EQ.0)GO TO 807
      WRITE(13,806)LCLOCK,JHOUR,JFLAG
 806  FORMAT(5X,I8,2X,I4,2X,I1)

C     PRINT TABLE OF UNITS ON OUTAGE OR DERATED TO TRAOUT FILE
C     JXX IS UNIT NUMBER ON OUTAGE, DERATED, OR ON PLANNED MAINTENANCE.
C     NNTAB(I) IS INDEX OF UNIT ON OUTAGE OR ON MAINTENANCE.
C     IF UNIT IS DERATED, THEN NNTAB(I) HAS - SIGN.

      JXX=0
      DO 1710 I=1,NUNITS
        IF(PLNDST(I).EQ.1.0)GO TO 1708
        JXX=JXX+1
        NNTAB(JXX)=I
        GO TO 1710
 1708   IF(PCTAVL(I).EQ.1.0)GO TO 1710
        JXX=JXX+1
        NNTAB(JXX)=I
        IF(PCTAVL(I).NE.0.0)NNTAB(JXX)=-I
 1710 CONTINUE

      WRITE(13,1711)JXX,(NNTAB(I),I=1,JXX)
 1711 FORMAT(20I4)
      IF(JFLAG.EQ.1)GO TO 833

      DO 831 I=1,NN
        WRITE(13,402)BN(I,1),BNS(I,1),BN(I,2),BNS(I,5),BNS(I,6)
 831  CONTINUE

 833  CONTINUE

 402  FORMAT(5X,F3.0,4(F10.2,2X))
      IF(JFLAG.EQ.0)GO TO 837

      DO 801 I=1,NN
        WRITE(13,802)(BN(I,J),J=1,3),(BNS(I,J),J=1,2),BN(I,4)
 801  CONTINUE

 837  CONTINUE
 802  FORMAT(5X,F3.0,5(F8.0,5X))

      DO 803 I=1,NL
        WRITE(13,804)(LP(I,J),J=1,3),(BLP(I,J),J=1,3),FLOW(I)
 803  CONTINUE

 807  CONTINUE
 804  FORMAT(5X,3(I3,2X),4(F8.0,2X))
      GO TO 1000

1200  CONTINUE

      CALL CONLS(A,BB,INJ,INJB,NX,XOB,IBAS,NR,LT,B,BLP,LP,NL,M,N
     *,BN,B1,LOD,NLS,XOBI,BS,TAB,N1)

C       DO 1400 I=1,M
C       WRITE(*,1410)(A(I,J),J=1,N),B(I)
C 1400  CONTINUE
C 1410  FORMAT(12F6.2/12F6.2)

      NXT=NX+1
      CALL LINP(M,N,A,B,XOB,XOBI,IBAS,BS,NXT,TAB,LCLOCK,JFLAG,N1)
      CALL NET(B,B1,RES,IBAS,BS,M,N)

C      DO 710 I=1,N
C      WRITE(*,720)IBAS(I),BS(I),B(I),B1(I),RES(I,1)
C 710  CONTINUE
C 720  FORMAT(10X,I3,5X,4F10.2)

      NX1=NX+1
      NXD1=NX*2+2*NX1+1
      NXD2=NXD1+NX

      DO 1700 I=NXD1,NXD2
        II=I-NXD1+1
        J=LT(II)
        BN(J,3)=RES(I,1)
        III=I+NX1
        BN(J,2)=RES(III,1)-LOD(J)+BN(J,3)
1700  CONTINUE

      NXD1=6*NX1-1
      NXD2=NXD1-1+NL
      J=0
      DO 1800 I=NXD1,NXD2
        J=J+1
        FLOW(J)=RES(I,1)
1800  CONTINUE

      IF(IOI.EQ.0)GO TO 707
      WRITE(13,806)LCLOCK,JHOUR,JFLAG

C     PRINT TABLE OF UNITS ON OUTAGE OR DERATED TO TRAOUT FILE
C     JXX IS UNIT NUMBER ON OUTAGE, DERATED, OR ON PLANNED MAINTENANCE.
C     NNTAB(I) IS INDEX OF UNIT ON OUTAGE OR ON MAINTENANCE.
C     IF UNIT IS DERATED, THEN NNTAB(I) HAS - SIGN.

      JXX=0
      DO 1810 I=1,NUNITS
        IF(PLNDST(I).EQ.1.0)GO TO 1808
        JXX=JXX+1
        NNTAB(JXX)=I
        GO TO 1810
 1808   IF(PCTAVL(I).EQ.1.0)GO TO 1810
        JXX=JXX+1
        NNTAB(JXX)=I
        IF(PCTAVL(I).NE.0.0)NNTAB(JXX)=-I
 1810 CONTINUE

      WRITE(13,1711)JXX,(NNTAB(I),I=1,JXX)
C
      DO 701 I=1,NN
        WRITE(13,802)(BN(I,J),J=1,3),(BNS(I,J),J=1,2),BN(I,4)
 701  CONTINUE

      DO 703 I=1,NL
        WRITE(13,804)(LP(I,J),J=1,3),(BLP(I,J),J=1,3),FLOW(I)
 703  CONTINUE

 707  CONTINUE

1000  CONTINUE
      DO 1900 I=1,NX
        DO 1900 J=1,NX
          BB(I,J)=BT(I,J)
          ZB(I,J)=ZT(I,J)
1900  CONTINUE

      RETURN
      END



      SUBROUTINE ADMITM(BB,NII,NJJ,BIJ,NX)
      DOUBLE PRECISION BB,BIJ
      DIMENSION BB(20,20)
      BB(NII,NJJ)=BB(NII,NJJ)-BIJ
      BB(NJJ,NII)=BB(NJJ,NII)-BIJ
      BB(NII,NII)=BB(NII,NII)+BIJ
      BB(NJJ,NJJ)=BB(NJJ,NJJ)+BIJ
      RETURN
      END


      SUBROUTINE ADMREF(BB,NII,BIJ,NX)
      DOUBLE PRECISION BB,BIJ
      DIMENSION BB(20,20)
      BB(NII,NII)=BB(NII,NII)+BIJ
      RETURN
      END



C@AUG30 ADD@-----------------------------------------------------------
      SUBROUTINE YEAR (IERR,RFLAG)
C....................................................................
      INCLUDE 'NARPERC.txt'
C....................................................................
      DIMENSION XPROB(5)
      CHARACTER*2 XG/'GC'/,XT/'TC'/,XS/'GT'/,XA/'AV'/
      DATA XPROB /.0668,.2417,.383,.2417,.0668/

      IF (NFCST.EQ.1) XPROB(1) = 1.0

C.....SCHEDULE EVENT FOR NEXT END OF YEAR..............................
C     ATRIB(1) = CLOCK + FLOAT(NHRSYR)
      ATRIB(1) = CLOCK + 8760.D0
      ATRIB(2) = 4.0
      CALL FILEM
C     IYEAR = IFIX((CLOCK+1)/FLOAT(NHRSYR))
      IYEAR = (CLOCK+1.D0)/8760.D0
      XYEAR = FLOAT(IYEAR)
      JDAY = 0
C  EXPLANATION OF VARIABLES USED TO COLLECT AND STORE RELIABILITY STATS
C
C  THESE VARIABLES OCCUR IN THE SECTION OF CODE WHICH FOLLOWS THIS
C
C  ALL VARIABLES HAVE 6-DIGIT IDENTIFIERS.
C
C  THE FIRST THREE DIGITS IDENTIFY THE TYPE OF STATISTIC
C      LOL MEANS THIS IS ANNUAL LOSS OF LOAD STATISTIC OF SOME TYPE
C      MGN MEANS THIS IS ANNUAL SUM OF NEGATIVE MARGINS (EUE)
C      SOL MEANS THIS IS THE CUMULATIVE SUM, ALL YEARS, LOLE
C      SNG MEANS THIS IS THE CUMULATIVE SUM, ALL YEARS, EUE
C
C  THE FOURTH DIGIT IDENTIFIES CAUSE OF OUTAGE
C           T IS TRANSMISSION
C           G IS GENERATION
C           S IS SUM OF T & G
C
C   THE FIFTH DIGIT INDICATES WHETHER THE STAT IS HOURLY OR PEAK
C               H IS FOR HOURLY STATS
C               P IS FOR PEAK STATS
C
C   THE SIXTH DIGIT IS FOR AREA OR POOL
C               A IS AREA
C               P IS POOL
C
C        EXAMPLES
C SGNSPA  IS CUMULATIVE SUM, EUE, TOTAL OF T&G, PEAK, AREA
C LOLTHP  IS ANNUAL LOSS OF LOAD, TRANSMISSION, HOURLY, POOL
C   AND SO ON


      DO 10 N = 1, NFCST
      DO 10 IAR= 1, NOAREA
C COMPUTE LOLES, SUM FOR FINAL REPORT, TAKE WEIGHTED AVG

C   AREA LOLE FOR FORECAST N IS SUM OF TRANS & GEN LOLES
	 LOLSHA(IAR,N) = LOLTHA(IAR,N) + LOLGHA(IAR,N)
C         WRITE(*,*)LOLSHA(IAR,N),IAR,N
C   CUMULATIVE SUM OF HOURLY AREA LOLES FOR THIS FORECAST IS NEXT
	 SOLSHA(IAR,N) = SOLSHA(IAR,N) + FLOAT(LOLSHA(IAR,N))
C   CUMULATIVE SUM OF HOURLY AREA LOLES ASSIGNED TO TRANSMISSION
	 SOLTHA(IAR,N) = SOLTHA(IAR,N) + FLOAT(LOLTHA(IAR,N))
	 SOLGHA(IAR,N) = SOLGHA(IAR,N) + FLOAT(LOLGHA(IAR,N))
	 WOLSHA(IAR) = WOLSHA(IAR) + FLOAT(LOLSHA(IAR,N))*XPROB(N)
	 WOLGHA(IAR) = WOLGHA(IAR) + FLOAT(LOLGHA(IAR,N))*XPROB(N)
	 WOLTHA(IAR) = WOLTHA(IAR) + FLOAT(LOLTHA(IAR,N))*XPROB(N)
C COMPUTE TOTAL MAGNITUDES, SUM FOR FINAL REPORT, TAKE WEIGHTED AVG
	 MGNSHA(IAR,N) = MGNTHA(IAR,N) + MGNGHA(IAR,N)
	 SGNSHA(IAR,N) = SGNSHA(IAR,N) + FLOAT(MGNSHA(IAR,N))
	 SGNTHA(IAR,N) = SGNTHA(IAR,N) + FLOAT(MGNTHA(IAR,N))
	 SGNGHA(IAR,N) = SGNGHA(IAR,N) + FLOAT(MGNGHA(IAR,N))
	 WGNSHA(IAR) = WGNSHA(IAR) + FLOAT(MGNSHA(IAR,N))*XPROB(N)
	 WGNGHA(IAR) = WGNGHA(IAR) + FLOAT(MGNGHA(IAR,N))*XPROB(N)
	 WGNTHA(IAR) = WGNTHA(IAR) + FLOAT(MGNTHA(IAR,N))*XPROB(N)
   10 CONTINUE

      DO 12 IAR = 1, NOAREA
        SWNGHA(IAR) = SWNGHA(IAR) + WGNGHA(IAR)
        SWNTHA(IAR) = SWNTHA(IAR) + WGNTHA(IAR)
        SWNSHA(IAR) = SWNSHA(IAR) + WGNSHA(IAR)
        SWLGHA(IAR) = SWLGHA(IAR) + WOLGHA(IAR)
        SWLTHA(IAR) = SWLTHA(IAR) + WOLTHA(IAR)
   12   SWLSHA(IAR) = SWLSHA(IAR) + WOLSHA(IAR)

      DO 20 N = 1, NFCST
        DO 20 IAR = 1, NOAREA
C COMPUTE LOLES, SUM FOR FINAL REPORT, TAKE WEIGHTED AVG
	       LOLSPA(IAR,N) = LOLTPA(IAR,N) + LOLGPA(IAR,N)
	       SOLSPA(IAR,N) = SOLSPA(IAR,N) + FLOAT(LOLSPA(IAR,N))
	       SOLTPA(IAR,N) = SOLTPA(IAR,N) + FLOAT(LOLTPA(IAR,N))
	       SOLGPA(IAR,N) = SOLGPA(IAR,N) + FLOAT(LOLGPA(IAR,N))
	       WOLGPA(IAR) = WOLGPA(IAR) + FLOAT(LOLGPA(IAR,N))*XPROB(N)
	       WOLTPA(IAR) = WOLTPA(IAR) + FLOAT(LOLTPA(IAR,N))*XPROB(N)
	       WOLSPA(IAR) = WOLSPA(IAR) + FLOAT(LOLSPA(IAR,N))*XPROB(N)
C COMPUTE TOTAL MAGNITUDES, SUM FOR FINAL REPORT, TAKE WEIGHTED AVG
	       MGNSPA(IAR,N) = MGNTPA(IAR,N) + MGNGPA(IAR,N)
	       SGNSPA(IAR,N) = SGNSPA(IAR,N) + FLOAT(MGNSPA(IAR,N))
	       SGNTPA(IAR,N) = SGNTPA(IAR,N) + FLOAT(MGNTPA(IAR,N))
	       SGNGPA(IAR,N) = SGNGPA(IAR,N) + FLOAT(MGNGPA(IAR,N))
	       WGNGPA(IAR) = WGNGPA(IAR) + FLOAT(MGNGPA(IAR,N))*XPROB(N)
	       WGNTPA(IAR) = WGNTPA(IAR) + FLOAT(MGNTPA(IAR,N))*XPROB(N)
	       WGNSPA(IAR) = WGNSPA(IAR) + FLOAT(MGNSPA(IAR,N))*XPROB(N)
   20 CONTINUE

C POOL STATISTICS, TOTAL, CUMULATE, WEIGHTED AVERAGE
      DO 22 IAR = 1, NOAREA
        SWNGPA(IAR) = SWNGPA(IAR) + WGNGPA(IAR)
        SWNTPA(IAR) = SWNTPA(IAR) + WGNTPA(IAR)
        SWNSPA(IAR) = SWNSPA(IAR) + WGNSPA(IAR)
        SWLGPA(IAR) = SWLGPA(IAR) + WOLGPA(IAR)
        SWLTPA(IAR) = SWLTPA(IAR) + WOLTPA(IAR)
   22   SWLSPA(IAR) = SWLSPA(IAR) + WOLSPA(IAR)

      NOERR=NORR
      DO 28 IAR = 1, NOAREA
        IPHOUR = LOLSHA(IAR,NOERR) + 1
        IF (IPHOUR.GT.22) IPHOUR = 22
        IPDP = LOLSPA(IAR,NOERR) + 1
        IF (IPDP.GT.22) IPDP = 22
        IPEUE = MGNSHA(IAR,NOERR)/LSTEP + 1
        IF (IPEUE.GT.22) IPEUE = 22
        HLOLE(IAR,IPHOUR) = HLOLE(IAR,IPHOUR) + 1.0
        DPLOLE(IAR,IPDP) = DPLOLE(IAR,IPDP) + 1.0
        EUES(IAR,IPEUE) = EUES(IAR,IPEUE) + 1.0
   28 CONTINUE

      DO 30 N = 1, NFCST
C COMPUTE LOLES, SUM FOR FINAL REPORT, TAKE WEIGHTED AVG
	    LOLSHP(N) = LOLTHP(N) + LOLGHP(N)
	    SOLSHP(N) = SOLSHP(N) + FLOAT(LOLSHP(N))
	    SOLTHP(N) = SOLTHP(N) + FLOAT(LOLTHP(N))
	    SOLGHP(N) = SOLGHP(N) + FLOAT(LOLGHP(N))
	    WOLGHP = WOLGHP + FLOAT(LOLGHP(N))*XPROB(N)
	    WOLTHP = WOLTHP + FLOAT(LOLTHP(N))*XPROB(N)
	    WOLSHP = WOLSHP + FLOAT(LOLSHP(N))*XPROB(N)
C COMPUTE TOTAL MAGNITUDES, SUM FOR FINAL REPORT, TAKE WEIGHTED AVG
	    MGNSHP(N) = MGNTHP(N) + MGNGHP(N)
	    SGNSHP(N) = SGNSHP(N) + FLOAT(MGNSHP(N))
	    SGNTHP(N) = SGNTHP(N) + FLOAT(MGNTHP(N))
	    SGNGHP(N) = SGNGHP(N) + FLOAT(MGNGHP(N))
	    WGNGHP = WGNGHP + FLOAT(MGNGHP(N))*XPROB(N)
	    WGNTHP = WGNTHP + FLOAT(MGNTHP(N))*XPROB(N)
	    WGNSHP = WGNSHP + FLOAT(MGNSHP(N))*XPROB(N)
   30 CONTINUE

      SWNGHP = SWNGHP + WGNGHP
      SWNTHP = SWNTHP + WGNTHP
      SWNSHP = SWNSHP + WGNSHP
      SWLGHP = SWLGHP + WOLGHP
      SWLTHP = SWLTHP + WOLTHP
      SWLSHP = SWLSHP + WOLSHP

      DO 40 N = 1, NFCST

C COMPUTE LOLES, SUM FOR FINAL REPORT, TAKE WEIGHTED AVG
	    LOLSPP(N) = LOLTPP(N) + LOLGPP(N)
	    SOLSPP(N) = SOLSPP(N) + FLOAT(LOLSPP(N))
	    SOLTPP(N) = SOLTPP(N) + FLOAT(LOLTPP(N))
	    SOLGPP(N) = SOLGPP(N) + FLOAT(LOLGPP(N))
	    WOLGPP = WOLGPP + FLOAT(LOLGPP(N))*XPROB(N)
	    WOLTPP = WOLTPP + FLOAT(LOLTPP(N))*XPROB(N)
	    WOLSPP = WOLSPP + FLOAT(LOLSPP(N))*XPROB(N)
C COM   PUTE TOTAL MAGNITUDES, SUM FOR FINAL REPORT, TAKE WEIGHTED AVG
	    MGNSPP(N) = MGNTPP(N) + MGNGPP(N)
	    SGNSPP(N) = SGNSPP(N) + FLOAT(MGNSPP(N))
	    SGNTPP(N) = SGNTPP(N) + FLOAT(MGNTPP(N))
	    SGNGPP(N) = SGNGPP(N) + FLOAT(MGNGPP(N))
	    WGNGPP = WGNGPP + FLOAT(MGNGPP(N))*XPROB(N)
	    WGNTPP = WGNTPP + FLOAT(MGNTPP(N))*XPROB(N)
	    WGNSPP = WGNSPP + FLOAT(MGNSPP(N))*XPROB(N)
   40 CONTINUE

      SWNGPP = SWNGPP + WGNGPP
      SWNTPP = SWNTPP + WGNTPP
      SWNSPP = SWNSPP + WGNSPP
      SWLGPP = SWLGPP + WOLGPP
      SWLTPP = SWLTPP + WOLTPP
      SWLSPP = SWLSPP + WOLSPP

C COMPUTE SQUARE OF VARIABLES
      DO 45 IAR=1,NOAREA
        XNEWA(IAR,1)=XNEWA(IAR,1)+(WOLSHA(IAR))**2
        XNEWA(IAR,2)=XNEWA(IAR,2)+(WGNSHA(IAR))**2
        XNEWA(IAR,3)=XNEWA(IAR,3)+(WOLSPA(IAR))**2
 45   CONTINUE

      XNEWP(1)=XNEWP(1)+WOLSHP**2
      XNEWP(2)=XNEWP(2)+WGNSHP**2
      XNEWP(3)=XNEWP(3)+WOLSPP**2

C PRINT ANNUALS & RESET TO 0
 1005 FORMAT(1H1)
 2030 FORMAT(2X,I2,2X,I2,9X,F8.2,2X,F8.2,12X,A2)
 2031 FORMAT(6X,I2,9X,F8.2,2X,F8.2,12X,A2)
 2040 FORMAT(2X,I2,2X,A2,9X,F8.2,2X,F8.2,12X,A2)
 2041 FORMAT(6X,A2,9X,F8.2,2X,F8.2,12X,A2)
 2002 FORMAT(2X,'AREA FORECAST',4X,'PEAK STATISTICS',11X,'REMARKS')
 2003 FORMAT(2X,'NO   NO',10X,'LOLE',7X,'XLOL')
 2004 FORMAT(19X,'(DAYS/YR)',3X,'MW')
 1000 FORMAT(31X,'TABLE',I5,/)
 1001 FORMAT(22X,'RESULTS AFTER ',I6,' REPLICATIONS',/)
 1002 FORMAT(2X,'AREA FORECAST',4X,'HOURLY STATISTICS',11X,'PEAK STATIST
     *ICS',11X,'REMARKS')
 1003 FORMAT(2X,'NO   NO',7X,'HLOLE',4X,'XLOL',7X,'EUE',7X,'LOLE',7X,
     *'XLOL')
 1004 FORMAT(16X,'(HRS/YR)',2X,'(MW)',6X,'(MWH)',5X,'(DAYS/YR)',2X
     *,'(MW)')

      IF(IOJ.EQ.0)GO TO 61
      WRITE(16,1005)
      ITAB=ITAB+1
      WRITE(16,1000)ITAB
C     IF (MOD(IYEAR,10).EQ.0) WRITE(*,1000) IYEAR
      WRITE(16,1001)IYEAR

      IF(INDX.EQ.1)GO TO 201
        WRITE(16,1002)
        WRITE(16,1003)
        WRITE(16,1004)
        GO TO 205
 201  CONTINUE

      WRITE(16,2002)
      WRITE(16,2003)
      WRITE(16,2004)
 205  CONTINUE

      DO 60 J=1, NOAREA
	    DO 50 N = 1, NFCST
	      IF (LOLGHA(J,N).GT.0) THEN
	        XMGNA = FLOAT(MGNGHA(J,N))/FLOAT(LOLGHA(J,N))
	      ELSE
	        XMGNA = 0.0
	      ENDIF

		  IF (LOLGPA(J,N).GT.0) THEN
	        XMGNP = FLOAT(MGNGPA(J,N))/FLOAT(LOLGPA(J,N))
	      ELSE
	        XMGNP = 0.0
	      ENDIF

          IF(INDX.EQ.1)GO TO 211
            WRITE(16,1030)J, N,LOLGHA(J,N),XMGNA,MGNGHA(J,N)
     *       ,LOLGPA(J,N),XMGNP,XG
            GO TO 215
 211      CONTINUE

		  WRITE(16,2030)J,N,LOLGPA(J,N),XMGNP,XG
 215      CONTINUE

		  IF (LOLTHA(J,N).GT.0) THEN
	        XMGNA = FLOAT(MGNTHA(J,N))/FLOAT(LOLTHA(J,N))
	      ELSE
	        XMGNA = 0.0
	      ENDIF

	      IF (LOLTPA(J,N).GT.0) THEN
	        XMGNP = FLOAT(MGNTPA(J,N))/FLOAT(LOLTPA(J,N))
	      ELSE
	        XMGNP = 0.0
	      ENDIF

          IF(INDX.EQ.1)GO TO 207
            WRITE(16,1030)J, N,LOLTHA(J,N),XMGNA,MGNTHA(J,N)
     *      ,LOLTPA(J,N),XMGNP,XT
            GO TO 208
 207      WRITE(16,2030)J,N,LOLTPA(J,N),XMGNP,XT
 208      CONTINUE

	      IF (LOLSHA(J,N).GT.0) THEN
	        XMGNA = FLOAT(MGNSHA(J,N))/FLOAT(LOLSHA(J,N))
	      ELSE
	        XMGNA = 0.0
	      ENDIF

	      IF (LOLSPA(J,N).GT.0) THEN
	        XMGNP = FLOAT(MGNSPA(J,N))/FLOAT(LOLSPA(J,N))
	      ELSE
	        XMGNP = 0.0
	      ENDIF

          IF(INDX.EQ.1)GO TO 209
            WRITE(16,1030)J,N,LOLSHA(J,N),XMGNA,MGNSHA(J,N)
     *          ,LOLSPA(J,N),XMGNP,XS
          GO TO 210
 209      WRITE(16,2030)J,N,LOLSPA(J,N),XMGNP,XT
 210      CONTINUE

 50     CONTINUE

 1030   FORMAT(2X,I2,2X,I2,5X,I4,5X,F8.2,2X,I8
     *     ,5X,I4,5X,F8.2,12X,A2)
 1031   FORMAT(6X,I2,5X,I4,5X,F8.2,2X,I8
     *    ,5X,I4,5X,F8.2,12X,A2)

		IF (WOLGHA(J).GT.0.0) THEN
          XMGNA = WGNGHA(J)/WOLGHA(J)
	    ELSE
	      XMGNA = 0.0
	    ENDIF

		IF (WOLGPA(J).GT.0.0) THEN
          XMGNP = WGNGPA(J)/WOLGPA(J)
	    ELSE
	      XMGNP = 0.0
	    ENDIF

        IF(INDX.EQ.1)GO TO 221
          WRITE(16,1040)J,XA,WOLGHA(J),XMGNA,WGNGHA(J)
     *      ,WOLGPA(J),XMGNP,XG
        GO TO 225
 221    CONTINUE
        WRITE(16,2040)J,XA,WOLGPA(J),XMGNP,XG
 225    CONTINUE

		IF (WOLTHA(J).GT.0.0) THEN
          XMGNA = WGNTHA(J)/WOLTHA(J)
	    ELSE
	      XMGNA = 0.0
	    ENDIF

		IF (WOLTPA(J).GT.0.0) THEN
          XMGNP = WGNTPA(J)/WOLTPA(J)
	    ELSE
	      XMGNP = 0.0
	    ENDIF

		IF(INDX.EQ.1)GO TO 241
          WRITE(16,1040)J,XA,WOLTHA(J),XMGNA,WGNTHA(J)
     *    ,WOLTPA(J),XMGNP,XT
        GO TO 245
 241    CONTINUE
        WRITE(16,2040)J,XA,WOLTPA(J),XMGNP,XT
 245    CONTINUE

		IF (WOLSHA(J).GT.0.0) THEN
          XMGNA = WGNSHA(J)/WOLSHA(J)
	    ELSE
	      XMGNA = 0.0
	    ENDIF

	    IF (WOLSPA(J).GT.0.0) THEN
          XMGNP = WGNSPA(J)/WOLSPA(J)
	    ELSE
	      XMGNP = 0.0
	    ENDIF

	    IF(INDX.EQ.1)GO TO 231
          WRITE(16,1040)J,XA,WOLSHA(J),XMGNA,WGNSHA(J)
     *      ,WOLSPA(J),XMGNP,XS
        GO TO 235
 231    CONTINUE
        WRITE(16,2040)J,XA,WOLSPA(J),XMGNP,XS
 235    CONTINUE

 1040   FORMAT(2X,I2,2X,A2,5X,F7.2,2X,F8.2,3X,F7.0
     *      ,5X,F7.2,2X,F8.2,12X,A2)
 1041   FORMAT(6X,A2,5X,F7.2,2X,F8.2,3X,F7.0
     *    ,5X,F7.2,2X,F8.2,12X,A2)
   60 CONTINUE


C POOL STATISTICS
      WRITE(16,1060)
 1060 FORMAT (' ',/,10X,' POOL STATISTICS',/)

      DO 70 N = 1, NFCST
	    IF (LOLGHP(N).GT.0) THEN
	      XMGNH = FLOAT(MGNGHP(N))/FLOAT(LOLGHP(N))
	    ELSE
	      XMGN = 0.0
	    ENDIF

	    IF (LOLGPP(N).GT.0) THEN
	      XMGNP = FLOAT(MGNGPP(N))/FLOAT(LOLGPP(N))
	    ELSE
	      XMGN = 0.0
	    ENDIF

	    IF(INDX.EQ.1)GO TO 261
          WRITE(16,1031) N,LOLGHP(N),XMGNH,MGNGHP(N)
     *     ,LOLGPP(N),XMGNP,XG
        GO TO 265
 261    CONTINUE
        WRITE(16,2031)N,LOLGPP(N),XMGNP,XG
 265    CONTINUE

		IF (LOLTHP(N).GT.0) THEN
	      XMGNH = FLOAT(MGNTHP(N))/FLOAT(LOLTHP(N))
	    ELSE
	      XMGNH = 0.0
	    ENDIF

		IF (LOLTPP(N).GT.0) THEN
	      XMGNP= FLOAT(MGNTPP(N))/FLOAT(LOLTPP(N))
	    ELSE
	      XMGNP= 0.0
	    ENDIF

        IF(INDX.EQ.1)GO TO 251
          WRITE(16,1031) N,LOLTHP(N),XMGNH,MGNTHP(N)
     *      ,LOLTPP(N),XMGNP,XT
        GO TO 255
 251    CONTINUE
        WRITE(16,2031) N,LOLTPP(N),XMGNP,XT
 255    CONTINUE

	     IF (LOLSHP(N).GT.0) THEN
	      XMGNH= FLOAT(MGNSHP(N))/FLOAT(LOLSHP(N))
	     ELSE
	      XMGNH= 0.0
	     ENDIF

	     IF (LOLSPP(N).GT.0) THEN
	      XMGNP= FLOAT(MGNSPP(N))/FLOAT(LOLSPP(N))
	     ELSE
	      XMGNP= 0.0
	     ENDIF

	     IF(INDX.EQ.1)GO TO 271
         WRITE(16,1031) N,LOLSHP(N),XMGNH,MGNSHP(N)
     *         ,LOLSPP(N),XMGNP,XS
       GO TO 275
 271   CONTINUE
       WRITE(16,2031)N,LOLSPP(N),XMGNP,XS
 275   CONTINUE

 70    CONTINUE

	      IF (WOLGHP.GT.0.0) THEN
       XMGNH= WGNGHP/WOLGHP
	     ELSE
	     XMGNH= 0.0
	     ENDIF

	     IF (WOLGPP.GT.0.0) THEN
       XMGNP= WGNGPP/WOLGPP
	     ELSE
       XMGNP= 0.0
	     ENDIF

	     IF(INDX.EQ.1)GO TO 281
       WRITE(16,1041)XA,WOLGHP,XMGNH,WGNGHP
     *  ,WOLGPP,XMGNP,XG
      GO TO 285
 281   CONTINUE
       WRITE(16,2041)XA,WOLGPP,XMGNP,XG
 285   CONTINUE

	    IF (WOLTHP.GT.0.0) THEN
       XMGNH= WGNTHP/WOLTHP
	   ELSE
	   XMGNH= 0.0
	   ENDIF

	     IF (WOLTPP.GT.0.0) THEN
         XMGNP= WGNTPP/WOLTPP
	      ELSE
	     XMGNP= 0.0
	     ENDIF

	     IF(INDX.EQ.1)GO TO 291
       WRITE(16,1041)XA,WOLTHP,XMGNH,WGNTHP
     *    ,WOLTPP,XMGNP,XT
       GO TO 295
 291    CONTINUE
       WRITE(16,2041)XA,WOLTPP,XMGNP,XT
 295   CONTINUE

	     IF (WOLSHP.GT.0.0) THEN
       XMGNH= WGNSHP/WOLSHP
	     ELSE
	     XMGNH= 0.0
	     ENDIF

	      IF (WOLSPP.GT.0.0) THEN
        XMGNP= WGNSPP/WOLSPP
	    ELSE
	    XMGNP= 0.0
       ENDIF

	     IF(INDX.EQ.1)GO TO 301
       WRITE(16,1041)XA,WOLSHP,XMGNH,WGNSHP
     *    ,WOLSPP,XMGNP,XS
      GO TO 305
 301  CONTINUE
       WRITE(16,2041)XA, WOLSPP,XMGNP,XS
 305  CONTINUE

  61  CONTINUE

C ZERO OUT THE ACCUMULATORS
      DO 5 IAR = 1, NOAREA
        WGNGHA(IAR)=0.0
        WGNTHA(IAR)=0.0
        WGNSHA(IAR)=0.0
        WOLGHA(IAR)=0.0
        WOLTHA(IAR)=0.0
    5   WOLSHA(IAR)=0.0

      DO 15 IAR = 1, NOAREA
        WGNGPA(IAR)=0.0
        WGNTPA(IAR)=0.0
        WGNSPA(IAR)=0.0
        WOLGPA(IAR)=0.0
        WOLTPA(IAR)=0.0
   15   WOLSPA(IAR)=0.0

	   DO 25 IAR = 1, NOAREA
        WGNSPA(IAR)=0.0
   25   WOLSPA(IAR)=0.0

      WGNSHP=0.0
      WOLSHP=0.0
      WGNSPP=0.0
      WOLSPP=0.0
      WGNGHP=0.0
      WOLGHP=0.0
      WGNGPP=0.0
      WOLGPP=0.0
      WGNTHP=0.0
      WOLTHP=0.0
      WGNTPP=0.0
      WOLTPP=0.0

      DO 100 N = 1,NFCST
        DO 90 IAR = 1, NOAREA
          LOLTHA(IAR,N) = 0
          LOLGHA(IAR,N) = 0
          MGNTHA(IAR,N) = 0
          MGNGHA(IAR,N) = 0
          LOLTPA(IAR,N) = 0
          LOLGPA(IAR,N) = 0
          MGNTPA(IAR,N) = 0
          MGNGPA(IAR,N) = 0
   90   CONTINUE
       LOLTHP(N) = 0
       LOLGHP(N) = 0
       MGNTHP(N) = 0
       MGNGHP(N) = 0
       LOLTPP(N) = 0
       LOLGPP(N) = 0
       MGNTPP(N) = 0
       MGNGPP(N) = 0
  100 CONTINUE

      IF(IYEAR.LE.5)WRITE(*,461) KWHERE, KVWHEN, KVSTAT, KVTYPE, KVLOC
  461 FORMAT(' ',' KVs = ',5I4)


C BEGIN CHECKING FOR CONVERGENCE
      IF (KWHERE.EQ.1) THEN
	    IF (KVWHEN.EQ.1) THEN
	      IF (KVSTAT.EQ.1) THEN
C LOOP FOR HOURLY, AREA, LOLE
	        IF (KVTYPE.EQ.1) THEN
		      SUM = SWLSHA(KVLOC)
	        ELSE
		      SUM = SOLSHA(KVLOC,NOERR)
	        ENDIF
C KVSTAT = 2, LOOP FOR HOURLY, AREA, EUE
	      ELSE
	        IF (KVTYPE.EQ.1) THEN
		      SUM = SWNSHA(KVLOC)
	        ELSE
		      SUM = SGNSHA(KVLOC,NOERR)
	        ENDIF
	      ENDIF
C KVWHEN = 2, LOOP FOR PEAK, AREA, LOLE
        ELSE
	      IF (KVSTAT.EQ.1) THEN
	        IF (KVTYPE.EQ.1) THEN
		      SUM = SWLSPA(KVLOC)
	        ELSE
		      SUM = SOLSPA(KVLOC,NOERR)
		    ENDIF
C KVWHEN=2, KVSTAT = 2, LOOP FOR PEAK, AREA, EUES
	      ELSE
	        IF (KVTYPE.EQ.1) THEN
		      SUM = SWNSPA(KVLOC)
	        ELSE
		      SUM = SGNSPA(KVLOC,NOERR)
	        ENDIF
	      ENDIF
	    ENDIF
      ELSE
	    IF (KVWHEN.EQ.1) THEN
	      IF (KVSTAT.EQ.1) THEN
C LOOP FOR HOURLY, POOL, LOLE
	        IF (KVTYPE.EQ.1) THEN
		      SUM = SWLSHP
	        ELSE
		      SUM = SOLSHP(NOERR)
	        ENDIF
C KVSTAT = 2, LOOP FOR HOURLY, POOL, EUE
	      ELSE
	        IF (KVTYPE.EQ.1) THEN
		      SUM = SWNSHP
	        ELSE
		      SUM = SGNSHP(NOERR)
	        ENDIF
	      ENDIF
C KVWHEN = 2, LOOP FOR PEAK, POOL, LOLE
	    ELSE
	      IF (KVSTAT.EQ.1) THEN
	        IF (KVTYPE.EQ.1) THEN
		      SUM = SWLSPP
	        ELSE
		      SUM = SOLSPP(NOERR)
		    ENDIF
C KVWHEN=2, KVSTAT = 2, LOOP FOR PEAK, POOL, EUES
	      ELSE
	        IF (KVTYPE.EQ.1) THEN
		      SUM = SWNSPP
	        ELSE
		      SUM = SGNSPP(NOERR)
	        ENDIF
	      ENDIF
	    ENDIF
      ENDIF

	  SUMX=SUM
      CALL CVCHK(SUM,IYEAR,RFLAG)
      NOERR=1

C      WRITE(*,*)CLOCK,FINISH,RFLAG,IYEAR,INDUMP(IYEAR)
C     IF(INDUMP(IYEAR).EQ.1)CALL INTM
C LOGIC ADDED FOR CALLING INTM WITHOUT USING INDUMP

      IF(IYEAR.EQ.INTVT)CALL INTM

      IFIN=FINISH/8760

      IF(IYEAR.EQ.INTVT)GO TO 102
        IF(IYEAR.EQ.IFIN)CALL INTM
 102  CONTINUE

      IF(IYEAR.EQ.INTVT)INTVT=INTVT+INTV

      IF (RFLAG.EQ.1.0) CALL REPORT(IYEAR)

      IF(RFLAG.EQ.1)GO TO 101
        IF (CLOCK.GE.FINISH) CALL REPORT(IYEAR)
 101  CONTINUE

      RETURN
      END




	SUBROUTINE REPORT(IYEAR)

C  MADE SEVERAL MINOR CHANGES TO MAKE STANDARD WITH FTN RULES ... GENE 1/26/89
      DIMENSION SUMHL(15),SUMDP(15),SUMEUE(15),PHL(15,22),PDP(15,22),
     @PEUE(15,22)
      INCLUDE 'NARPERC.txt'
      CHARACTER XG*2,XT*2,XS*2,XA*2,FH*4,SH*1
      INCLUDE 'NARPCOM.txt'
      DATA SUMHL,SUMDP,SUMEUE,PHL,PDP,PEUE/1035*0/
      DATA XG/'GC'/,XT/'TC'/,XS/'GT'/,XA/'AV'/,FH/'ERCO'/,SH/'T'/

 2030 FORMAT(2X,I2,2X,I2,9X,F8.2,2X,F8.2,12X,A2)
 2031 FORMAT(6X,I2,9X,F8.2,2X,F8.2,12X,A2)
 2040 FORMAT(2X,I2,2X,A2,9X,F8.2,2X,F8.2,12X,A2)
 2041 FORMAT(6X,A2,9X,F8.2,2X,F8.2,12X,A2)
 2002 FORMAT(2X,'AREA FORECAST',4X,'PEAK STATISTICS',11X,'REMARKS')
 2003 FORMAT(2X,'NO   NO',10X,'LOLE',7X,'XLOL')
 2004 FORMAT(19X,'(DAYS/YR)',3X,'MW')
 1030 FORMAT(2X,I2,2X,I2,5X,F7.2,2X,F8.2,3X,F7.0
     *,5X,F7.2,2X,F8.2,12X,A2)
 1031 FORMAT(6X,I2,5X,F7.2,2X,F8.2,3X,F7.0
     *,5X,F7.2,2X,F8.2,12X,A2)
 1040 FORMAT(2X,I2,2X,A2,5X,F7.2,2X,F8.2,3X,F7.0
     *,5X,F7.2,2X,F8.2,12X,A2)
 1041 FORMAT(6X,A2,5X,F7.3,2X,F8.2,3X,F7.0
     *,5X,F7.3,2X,F8.2,12X,A2)

	  XYEAR = FLOAT(IYEAR)

 1005 FORMAT(1H1)
      ITAB=ITAB+1
      WRITE(16,1005)
      WRITE(16,1000)ITAB

C     IF (MOD(IYEAR,10).EQ.0) WRITE(*,1000) IYEAR
 1000 FORMAT(31X,'TABLE',I5,/)
 1006 FORMAT(16X,'TABLE',I5,/)

      WRITE(16,1001)IYEAR

 1001 FORMAT(15X,'FINAL RESULTS AFTER ',I6,' REPLICATIONS',/)
 1002 FORMAT(2X,'AREA FORECAST',4X,'HOURLY STATISTICS',11X,'PEAK STATIST
     *ICS',11X,'REMARKS')
 1003 FORMAT(2X,'NO   NO',7X,'HLOLE',4X,'XLOL',7X,'EUE',7X,'LOLE',7X,
     *'XLOL')
 1004 FORMAT(16X,'(HRS/YR)',2X,'(MW)',6X,'(MWH)',5X,'(DAYS/YR)',2X
     *,'(MW)')

      IF(INDX.EQ.1)GO TO 201
        WRITE(16,1002)
        WRITE(16,1003)
        WRITE(16,1004)
      GO TO 205

 201    WRITE(16,2002)
        WRITE(16,2003)
        WRITE(16,2004)
 205  CONTINUE

	DO 60 J=1, NOAREA
	  IF(NFCST.EQ.1)GO TO 11476

	  DO 50 N = 1, NFCST
	    IF (SOLGHA(J,N).GT.0.) THEN
	      XMGN = SGNGHA(J,N)/SOLGHA(J,N)
	    ELSE
	      XMGN = 0.0
	    ENDIF
	    XLOL = SOLGHA(J,N)/XYEAR
	    EUE  = SGNGHA(J,N)/XYEAR

	    IF (SOLGPA(J,N).GT.0.) THEN
	      XMGNP= SGNGPA(J,N)/SOLGPA(J,N)
	    ELSE
	      XMGNP= 0.0
	    ENDIF

	    XLOLP= SOLGPA(J,N)/XYEAR
	    EUEP = SGNGPA(J,N)/XYEAR

		IF(INDX.EQ.1)GO TO 211

		WRITE(16,1030)J,N,XLOL,XMGN,EUE
     *       ,XLOLP,XMGNP,XG

		GO TO 215
 211    CONTINUE
        WRITE(16,2030)J,N,XLOLP,XMGNP,XG

 215    CONTINUE
	    IF (SOLTHA(J,N).GT.0.) THEN
	      XMGN = SGNTHA(J,N)/SOLTHA(J,N)
	    ELSE
	      XMGN = 0.0
	    ENDIF

	    XLOL = SOLTHA(J,N)/XYEAR
	    EUE  = SGNTHA(J,N)/XYEAR
	    IF (SOLTPA(J,N).GT.0.) THEN
	      XMGNP= SGNTPA(J,N)/SOLTPA(J,N)
	    ELSE
	      XMGNP= 0.0
	    ENDIF

	    XLOLP= SOLTPA(J,N)/XYEAR
	    EUEP = SGNTPA(J,N)/XYEAR

	    IF(INDX.EQ.1)GO TO 207
        WRITE(16,1030)J,N,XLOL,XMGN,EUE
     *          ,XLOLP,XMGNP,XT
	    GO TO 208
 207    WRITE(16,2030)J,N,XLOLP,XMGNP,XT

 208    CONTINUE
	    IF (SOLSHA(J,N).GT.0.) THEN
	      XMGN = SGNSHA(J,N)/SOLSHA(J,N)
	    ELSE
	      XMGN = 0.0
	    ENDIF

	    XLOL = SOLSHA(J,N)/XYEAR
	    EUE  = SGNSHA(J,N)/XYEAR

	    IF (SOLSPA(J,N).GT.0.) THEN
	      XMGNP= SGNSPA(J,N)/SOLSPA(J,N)
	    ELSE
	      XMGNP= 0.0
	    ENDIF

	    XLOLP= SOLSPA(J,N)/XYEAR
	    EUEP = SGNSPA(J,N)/XYEAR
        IF(INDX.EQ.1)GO TO 209
        WRITE(16,1030)J,N,XLOL,XMGN,EUE
     *     ,XLOLP,XMGNP,XS
        GO TO 210
 209    CONTINUE
        WRITE(16,2030)J,N,XLOLP,XMGNP,XS
 210    CONTINUE
  50  CONTINUE
C ****  NEW CODE ...KCP
      WRITE(16,'(1H )')
C ****
11476 IF (SWLGHA(J).GT.0.0) THEN
        XMGN = SWNGHA(J)/SWLGHA(J)
	  ELSE
	    XMGN = 0.0
	  ENDIF

	  XLOL = SWLGHA(J)/XYEAR
	  EUE  = SWNGHA(J)/XYEAR
	  IF (SWLGPA(J).GT.0.0) THEN
        XMGNP= SWNGPA(J)/SWLGPA(J)
	  ELSE
	    XMGNP= 0.0
	  ENDIF

	  XLOLP= SWLGPA(J)/XYEAR
	  EUEP = SWNGPA(J)/XYEAR
      IF(INDX.EQ.1)GO TO 221
      WRITE(16,1040)J,XA,XLOL,XMGN,EUE
     *          ,XLOLP,XMGNP,XG
	  GO TO 225
 221  CONTINUE
      WRITE(16,2040)J,XA,XLOLP,XMGNP,XG

 225  CONTINUE
	  IF (SWLTHA(J).GT.0.0) THEN
        XMGN = SWNTHA(J)/SWLTHA(J)
	  ELSE
	    XMGN = 0.0
	  ENDIF

	  XLOL = SWLTHA(J)/XYEAR
	  EUE  = SWNTHA(J)/XYEAR
	  IF (SWLTPA(J).GT.0.0) THEN
        XMGNP= SWNTPA(J)/SWLTPA(J)
	  ELSE
	    XMGNP= 0.0
	  ENDIF

	  XLOLP= SWLTPA(J)/XYEAR
	  EUEP = SWNTPA(J)/XYEAR
      IF(INDX.EQ.1)GO TO 241
      WRITE(16,1040)J,XA,XLOL,XMGN,EUE
     *     ,XLOLP,XMGNP,XT
      GO TO 245
 241  CONTINUE
      WRITE(16,2040)J,XA,XLOLP,XMGNP,XT

 245  CONTINUE
	  IF (SWLSHA(J).GT.0.0) THEN
        XMGN = SWNSHA(J)/SWLSHA(J)
	  ELSE
	    XMGN = 0.0
	  ENDIF

	  XLOL = SWLSHA(J)/XYEAR
	  SSQA(J,1)=XNEWA(J,1)/XYEAR-XLOL**2
	  EUE  = SWNSHA(J)/XYEAR
	  SSQA(J,2)=XNEWA(J,2)/XYEAR-EUE**2

	  IF (SWLSPA(J).GT.0.0) THEN
        XMGNP= SWNSPA(J)/SWLSPA(J)
	  ELSE
	    XMGNP= 0.0
	  ENDIF

	  XLOLP= SWLSPA(J)/XYEAR
	  SSQA(J,3)=XNEWA(J,3)/XYEAR-XLOLP**2
	  EUEP = SWNSPA(J)/XYEAR

      IF(INDX.EQ.1)GO TO 231
      WRITE(16,1040)J,XA,XLOL,XMGN,EUE
     *         ,XLOLP,XMGNP,XS
      GO TO 235
 231  CONTINUE
      WRITE(16,2040)J,XA,XLOLP,XMGNP,XS

 235  CONTINUE
      SUSTAT(J,4)=XLOL
      SUSTAT(J,5)=XLOLP
      SUSTAT(J,6)=EUE
C ****  NEW CODE ...KCP
      WRITE(16,'(1H ,/)')
C ****
 60        CONTINUE


C POOL STATISTICS
      WRITE(16,1060)
 1060 FORMAT (' ',/,10X,' POOL STATISTICS',/)
	  IF(NFCST.EQ.1)GO TO 11477
      DO 70 N = 1, NFCST
	    IF (SOLGHP(N).GT.0.) THEN
	      XMGN = SGNGHP(N)/SOLGHP(N)
	    ELSE
	      XMGN = 0.0
	    ENDIF
	    XLOL = SOLGHP(N)/XYEAR
	    EUE  = SGNGHP(N)/XYEAR
        IF (SOLGPP(N).GT.0) THEN
          XMGNP= SGNGPP(N)/SOLGPP(N)
        ELSE
          XMGNP= 0.0
        ENDIF
        XLOLP= SOLGPP(N)/XYEAR
        EUEP =  0.0

        IF(INDX.EQ.1)GO TO 261
          WRITE(16,1031) N,XLOL,XMGN,EUE
     *       ,XLOLP,XMGNP,XG
          GO TO 265
 261    CONTINUE
          WRITE(16,2031)N,XLOLP,XMGNP,XG
 265    CONTINUE

	      IF (SOLTHP(N).GT.0.) THEN
	      XMGN = SGNTHP(N)/SOLTHP(N)
	      ELSE
	      XMGN = 0.0
	      ENDIF

		    XLOL = SOLTHP(N)/XYEAR
	      EUE  = SGNTHP(N)/XYEAR
	     IF (SOLTPP(N).GT.0) THEN
	      XMGNP= SGNTPP(N)/SOLTPP(N)
	     ELSE
	      XMGNP= 0.0
	     ENDIF
	     XLOLP= SOLTPP(N)/XYEAR
	     EUEP =  0.0
      IF(INDX.EQ.1)GO TO 251
      WRITE(16,1031) N,XLOL,XMGN,EUE
     *,XLOLP,XMGNP,XT
      GO TO 255
 251  CONTINUE
      WRITE(16,2031)N,XLOLP,XMGNP,XT
 255  CONTINUE
	     IF (SOLSHP(N).GT.0.) THEN
	      XMGN = SGNSHP(N)/SOLSHP(N)
	     ELSE
	      XMGN = 0.0
	     ENDIF
	      XLOL = SOLSHP(N)/XYEAR
          	EUE  = SGNSHP(N)/XYEAR
	        IF (SOLSPP(N).GT.0) THEN
	         XMGNP= SGNSPP(N)/SOLSPP(N)
	          ELSE
	          XMGNP= 0.0
	         ENDIF
	           XLOLP= SOLSPP(N)/XYEAR
	EUEP =  0.0
       IF(INDX.EQ.1)GO TO 271
      WRITE(16,1031) N,XLOL,XMGN,EUE
     *,XLOLP,XMGNP,XS
      GO TO 275
 271  CONTINUE
      WRITE(16,2031)N,XLOLP,XMGNP,XS
 275  CONTINUE
 70   CONTINUE

11477 CONTINUE

       IF (SWLGHP.GT.0.0) THEN
             XMGN = SWNGHP/SWLGHP
	       ELSE
	         XMGN = 0.0
	       ENDIF
	       XLOL = SWLGHP/XYEAR
	       EUE  = SWNGHP/XYEAR
	       IF (SWLGPP.GT.0.0) THEN
             XMGNP= SWNGPP/SWLGPP
	       ELSE
	         XMGNP= 0.0
	       ENDIF
       	XLOLP= SWLGPP/XYEAR
       	EUEP = 0.0
C ****  NEW CODE ...KCP
      WRITE(16,'(1H )')
C ****
      IF(INDX.EQ.1)GO TO 281
      WRITE(16,1041)XA,XLOL,XMGN,EUE
     *,XLOLP,XMGNP,XG
      GO TO 285
 281  CONTINUE
      WRITE(16,2041)XA,XLOLP,XMGNP,XG
 285  CONTINUE
	       IF (SWLTHP.GT.0.0) THEN
             XMGN = SWNTHP/SWLTHP
	       ELSE
	         XMGN = 0.0
       	ENDIF
	       XLOL = SWLTHP/XYEAR
	       EUE  = SWNTHP/XYEAR
	       IF (SWLTPP.GT.0.0) THEN
             XMGNP= SWNTPP/SWLTPP
	       ELSE
	         XMGNP= 0.0
       	ENDIF
	         XLOLP= SWLTPP/XYEAR
	       EUEP = 0.0
      IF(INDX.EQ.1)GO TO 291
      WRITE(16,1041)XA,XLOL,XMGN,EUE
     *,XLOLP,XMGNP,XT
       GO TO 295
 291   CONTINUE
      WRITE(16,2041)XA,XLOLP,XMGNP,XT
 295  CONTINUE
       	IF (SWLSHP.GT.0.0) THEN
             XMGN = SWNSHP/SWLSHP
	       ELSE
	         XMGN = 0.0
	       ENDIF
	       XLOL = SWLSHP/XYEAR
	       SSQP(1)=XNEWP(1)/XYEAR-XLOL**2
	       EUE  = SWNSHP/XYEAR
	       SSQP(2)=XNEWP(2)/XYEAR-EUE**2
	       IF (SWLSPP.GT.0.0) THEN
             XMGNP= SWNSPP/SWLSPP
	       ELSE
	         XMGNP= 0.0
	       ENDIF
	       XLOLP= SWLSPP/XYEAR
	       SSQP(3)=XNEWP(3)/XYEAR-XLOLP**2
	       EUEP = 0.0
      IF(INDX.EQ.1)GO TO 301
      WRITE(16,1041)XA,XLOL,XMGN,EUE
     *,XLOLP,XMGNP,XS
      GO TO 305
 301  CONTINUE
      WRITE(16,2041)XA,XLOLP,XMGNP,XS
 305  CONTINUE
       DO 46 IAR=1,NOAREA
       DO 46 J=1,3
       SSQA(IAR,J)=(SSQA(IAR,J)/XYEAR)**.5
 46    CONTINUE
       DO 47 J=1,3
       SSQP(J)=(SSQP(J)/XYEAR)**.5
 47    CONTINUE
      SUSTAT(NOAREA+1,4)=XLOL
      SUSTAT(NOAREA+1,5)=XLOLP
      SUSTAT(NOAREA+1,6)=EUE
      WRITE(16,113)
 113  FORMAT(1H1)
      ITAB=ITAB+1
      WRITE(16,111)ITAB
 111  FORMAT(30X,'TABLE ',I4)
      WRITE(16,112)
 112  FORMAT(25X,'SUMMARY OF RESULTS',/)
      IF(INDX.EQ.1)GO TO 310
      WRITE(16,114)
      GO TO 315
 310  CONTINUE
      WRITE(16,214)
 315  CONTINUE
 114  FORMAT(10X,'AREA',7X,'PEAK',8X,'INSTALLED',3X,'AREA   ',8X,
     *'HLOLE',19X,'EUE',16X,'LOLE',/,
     * 55X,'----------------',8X,'----------------',5X,
     *'----------------',/,
     * 33X,'CAPACITY',4X,'PCT RES',
     * 3X,'MAGN',6X,'PCT SD',8X,'MAGN',6X,'PCT SD',5X,'MAGN',6X,'PCT SD'
     * )
 214  FORMAT(10X,'AREA',7X,'PEAK',8X,'INSTALLED',3X,'AREA   ',
     *55X,
     *'----------------',/,
     *33X,'CAPACITY',4X,'PCT RES',
     *5X,'MAGN',6X,'PCT SD'
     *)

      DO 101 I=1,NOAREA
        IF(SUSTAT(I,1).GT.0.)
     &   SUSTAT(I,3)=(SUSTAT(I,2)-SUSTAT(I,1))/SUSTAT(I,1)*100.

        IF(INDX.EQ.1)GO TO 320
          SSHL=0.
          SSEU=0.
          SSLO=0.
          IF(SUSTAT(I,4).GT.0.)SSHL=SSQA(I,1)/SUSTAT(I,4)*100.
          IF(SUSTAT(I,6).GT.0.)SSEU=SSQA(I,2)/SUSTAT(I,6)*100.
          IF(SUSTAT(I,5).GT.0.)SSLO=SSQA(I,3)/SUSTAT(I,5)*100.

C     WRITE(16,102)NAMA(I),(SUSTAT(I,J),J=1,4),SSQA(I,1),SUSTAT(I,6)
C    &,SSQA(I,2),SUSTAT(I,5),SSQA(I,3)

          WRITE(16,102)NAMA(I),(SUSTAT(I,J),J=1,4),SSHL,SUSTAT(I,6)
     &   ,SSEU,SUSTAT(I,5),SSLO
        GO TO 325
 320    CONTINUE
          SSLO=0.
          IF(SUSTAT(I,5).GT.0.)SSLO=SSQA(I,3)/SUSTAT(I,5)*100.
          WRITE(16,202)NAMA(I),(SUSTAT(I,J),J=1,3),SUSTAT(I,5),SSLO
 325    CONTINUE

	    NO1=NOAREA+1
        SUSTAT(NO1,1)=SUSTAT(NO1,1)+SUSTAT(I,1)
        SUSTAT(NO1,2)=SUSTAT(NO1,2)+SUSTAT(I,2)
 101  CONTINUE

      IF(SUSTAT(I,1).GT.0.)
     &   SUSTAT(NO1,3)=(SUSTAT(I,2)-SUSTAT(I,1))/SUSTAT(I,1)*100.

      IF(INDX.EQ.1)GO TO 330
        SSHL=0.
        SSEU=0.
        SSLO=0.
        IF(SUSTAT(NO1,4).GT.0.)SSHL=SSQP(1)/SUSTAT(NO1,4)*100.
        IF(SUSTAT(NO1,6).GT.0.)SSEU=SSQP(2)/SUSTAT(NO1,6)*100.
        IF(SUSTAT(NO1,5).GT.0.)SSLO=SSQP(3)/SUSTAT(NO1,5)*100.
        WRITE(16,103)FH,SH,(SUSTAT(NO1,J),J=1,4),SSHL,SUSTAT(NO1,
     &     6),SSEU,SUSTAT(NO1,5),SSLO
        GO TO 335
 330  CONTINUE
        SSLO=0.
        IF(SUSTAT(NO1,5).GT.0.)SSLO=SSQP(3)/SUSTAT(NO1,5)*100.
        WRITE(16,203)FH,SH,(SUSTAT(NO1,J),J=1,3)
     &     ,SUSTAT(NO1,5),SSLO
 335  CONTINUE

 102  FORMAT(10X,A4,5X,F7.0,5X,F8.0,5X,F5.1,5X,F7.3,4X,F7.3
     &,4X,F8.0,3X,F8.0,3X,F7.3,3X,F7.3)
 202  FORMAT(10X,A4,5X,F7.0,5X,F8.0,5X,F5.1,5X
     &,F7.3,3X,F7.3)
 103  FORMAT(1H ,/,10X,A4,A1,4X,F7.0,5X,F8.0,5X,F5.1,5X,F7.3,4X,F7.3
     &,4X,F8.0,3X,F8.0,3X,F7.3,3X,F7.3)
 203  FORMAT(1H ,/,10X,A4,A1,4X,F7.0,5X,F8.0,5X,F5.1,5X
     &,F7.3,3X,F7.3)

C     WRITE(16,113)
C     WRITE(16,'(1H ,///)')
C     ITAB=ITAB+1
C     WRITE(16,1045)ITAB
C1045 FORMAT(10X,'TABLE',I4,/,5X,'AREA',5X,'STANDARD DEVIATIONS FOR',/,
C    *14X,'HLOLE',5X,'EUE',10X,'LOLE',/,13X,'(HRS/YR)',2X,'(MWH)',
C    *8X,'(DAYS/YR)'/)
C1043 FORMAT(5X,I2,7X,F7.3,3X,F8.0,6X,F7.3)
C1046 FORMAT(14X,F7.3,3X,F7.3,6X,F7.3)
C     DO 104 I=1,NOAREA
C     WRITE(16,1043)I,(SSQA(I,J),J=1,3)
C104  CONTINUE
C     WRITE(16,1044)
C1044 FORMAT(5X,'POOL STATS',/)
C     WRITE(16,1046)(SSQP(J),J=1,3)

      DO 100 J=1,22
        DO 100 I = 1, NOAREA
  100     SUMHL(I) = SUMHL(I) + HLOLE(I,J)
      DO 110 J = 1, 22
        DO 110 I = 1, NOAREA
  110     SUMDP(I) = SUMDP(I) + DPLOLE(I,J)
      DO 120 J = 1,22
        DO 120 I = 1, NOAREA
  120     SUMEUE(I) = SUMEUE(I) + EUES(I,J)
      DO 130 J = 1, 22
        DO 130 I = 1, NOAREA
  130     PHL(I,J) = HLOLE(I,J)/SUMHL(I)
      DO 140 J = 1, 22
        DO 140 I = 1, NOAREA
  140     PDP(I,J) = DPLOLE(I,J)/SUMHL(I)
      DO 150 J = 1, 22
        DO 150 I = 1, NOAREA
  150     PEUE(I,J) = EUES(I,J)/SUMEUE(I)

      DO 200 I=1,NOAREA
	    ITAB=ITAB+1
	    WRITE(16,9975)ITAB,I
	    DO 160 J=1,22
	      K=J-1
	      LIMIT=K*LSTEP
	      WRITE(16,9974)K,DPLOLE(I,J),PDP(I,J),HLOLE(I,J),PHL(I,J),
     +                  LIMIT,EUES(I,J),PEUE(I,J)
 160    CONTINUE
 200  CONTINUE

 9975 FORMAT(1H1,T58,'TABLE',I5,//,1H ,T43,'PROBABILITY DISTRIBUTIONS ',
     +       'FOR AREA',I3,///,1H ,T12,'DAILY PEAK LOLES PER YEAR',T50,
     +       'HOURLY LOLES PER YEAR',T95,'ANNUAL UNSERVED ENERGY (MWH)',
     +       //,1H ,'NUMBER',T12,'OBSERVATIONS',T27,'PROBABILITY',T48,
     +       'OBSERVATIONS',T63,'PROBABILITY',T88,'LIMIT (MWH)',T103,
     +       'OBSERVATIONS',T118,'PROBABILITY',/)
 9974 FORMAT(1H ,I4,T15,F6.0,T29,F5.3,T51,F6.0,T65,F5.3,T89,I6,T106,
     +       F6.0,T120,F6.4)
      RETURN
C     STOP
      END



      SUBROUTINE CVCHK(SUM,IYEAR,RFLAG)
      INCLUDE 'NARPERC.txt'
C     MOVED DATA SSQ,XLAST/2*0.0/ INITIALIZATION TO FRONT OF MAIN PROGRAM
      DATA DIFF/0./

      RFLAG = 0.0
C      WRITE(*,1) SUM
C    1 FORMAT(' ',' INE CVCHK, SUM =',F9.1)
C      WRITE(*,2) XLAST
C    2 FORMAT(' ',' INE CVCHK, XLAST =',F9.1)
      IF (CLOCK .GE. FINISH) THEN
	    RFLAG = 1.0
C       RETURN
      ENDIF

      DIFF = SUM - XLAST
      XLAST = SUM
      SSQ = SSQ + DIFF ** 2
      AYEAR = FLOAT(IYEAR)


      IF (IYEAR .LT. 5) THEN
	    RETURN
      END IF

      XMEAN = SUM / AYEAR
      IF (CVTEST.EQ.0.0) THEN
	    XX = 0.025*XMEAN
      ELSE
	    XX = CVTEST*XMEAN
      ENDIF

	  VAR = (SSQ/AYEAR) - (XMEAN ** 2)
	  STD = VAR ** 0.5

      STDERR = STD/(AYEAR ** 0.5)
      XKCPXX = 0.

      IF (XX.NE.0.0) XKCPXX = STDERR/XX

      WRITE(*,1000) IYEAR,SUM,DIFF,XMEAN,XKCPXX
 1000 FORMAT(' ',I6,' REPLICATION; SUM=',F7.1,'  NEW=',F5.1,
     @'  MEAN=',F6.3,'  CONVERGENCE=',F7.3)

      IF (XMEAN .EQ. 0.0) THEN
	    RETURN
      ENDIF

      IF (STDERR .LT. 0.0) RETURN

      IF (STDERR .LT. XX) THEN
	    RFLAG = 1.0
	    WRITE(*,1122) CLOCK, STDERR, XX
 1122   FORMAT('**************** SIMULATION CONVERGED ****************',
     @  /,'    CLOCK = ',F11.1,' STD = ',F5.2,' TEST = ',F5.2)
      ENDIF

C9999 RETURN     THE 9999 IS NEVER USED ... GENE P. 1/26/89
      RETURN
      END



      SUBROUTINE DATAF1(JU)
      INCLUDE 'NARPERC.txt'
      INCLUDE 'NARPCOM.txt'
C IABORT ADDED TO ABORT PROGRAM BECAUSE OF ERRORS-C. SINGH DEC 1993
      IABORT=0
      NR=1
      JU4=9
      JU2=10
      JU3=11
      JU1=12
C      JU=6     NOTE THAT UNIT=1 IS SCREEN... GENE P. 1/27/89
      CALL SRTC(JU1,JU2,JU3,JU4)
      IF(IABORT.NE.0)GO TO 100
      CALL SRMC(JU1,JU2,JU3,JU4)
      IF(IABORT.NE.0)GO TO 100
      CALL SRLD(JU1,JU2,JU3,JU4)
      IF(IABORT.NE.0)GO TO 100
      CALL SRUD(JU1,JU2,JU3,JU4)
      IF(IABORT.NE.0)GO TO 100
      CALL SRFC(JU1,JU2,JU3,JU4)
      IF(IABORT.NE.0)GO TO 100
      CALL SROD(JU1,JU2,JU3,JU4)
      IF(IABORT.NE.0)GO TO 100
      CALL SRTD(JU1,JU2,JU3,JU4)
      IF(IABORT.NE.0)GO TO 100
      CALL SRDD(JU1,JU2,JU3,JU4)
      IF(IABORT.NE.0)GO TO 100

      REWIND (JU1)
      CALL PIND(JU1,JU)

      REWIND (JU1)
      CALL INPUT(JU1)

      IF(IABORT.NE.0)GO TO 100

      REWIND (JU1)
      CALL SEEDER

 100  CONTINUE
      RETURN
      END




C
C THIS SUBROUTINE READS ZZTC CARDS
C
      SUBROUTINE SRTC(JU1,JU2,JU3,JU4)
      INCLUDE 'NARPERC.txt'
      INCLUDE 'NARPCOM.txt'
C ILINE AND JLINE INTRODUCED TO KEEP LINE COUNT IN INPUTB AND INPUTC
C C.SINGH DEC 1993
      ILINE=0
      JLINE=0
      N=2
 51   READ(JU2,10)STRING
      ILINE=ILINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 50 I=1,20
      IF(TITL(I).NE.BLANK)NI=1
 50   CONTINUE
      IF(NI.EQ.0)GO TO 51
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 51
      BACKSPACE JU2
      READ(STRING,21)IC
      IF(IC.EQ.CBTC)GO TO 52
      WRITE(*,53)ILINE
 53   FORMAT(15X,'CARD ZZTC NOT FOUND IN SEQUENCE IN INPUTB',
     */,15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED INLINE ',I4,/)
      IABORT=1
      RETURN
 52   CONTINUE
      ILINE=ILINE-1
      DO 100 I=1,N
 100  READ(JU2,110)(TITL(J),J=1,20)
 110  FORMAT(20A4)
C ILINE UPDATE - C.SINGH DEC 1993
      ILINE=ILINE+2
      I1=0
 120  READ(JU2,10)STRING
 10   FORMAT(A80)
C ILINE UPDATE - C.SINGH 1993
      ILINE=ILINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 115 I=1,20
      IF(TITL(I).NE.BLANK)NI=1
 115  CONTINUE
      IF(NI.EQ.0)GO TO 120
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 130
      BACKSPACE JU2
C ILINE UPDATE - C. SINGH 1993
      READ(STRING,21)IC
      IF(IC.EQ.CBMC)GO TO 70
      WRITE(*,71)ILINE
 71   FORMAT(15X,'CARD ZZMC NOT IN SEQUENCE IN INPUTB',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN
 70   CONTINUE
      ILINE=ILINE-1
      GO TO 140
 130  READ(STRING,121)I1,(AT(J),J=1,39)
 121  FORMAT(I2,39A2)
C CHECKING FOR SERIAL NUMBER GREATER THAN 20 IN INPUTB-C. SINGH DEC 1993
      IF(I1.LE.20)GO TO 122
      WRITE(*,127)I1,ILINE
 127  FORMAT(15X,'SERIAL NUMBER ',I2,' EXCEEDS 20 FOR ZZTC DATA IN FILE
     *INPUTB',/,15X,'THIS SERIAL NUMBER  AND CORRESPONDING DATA IS IGNOR
     *ED',/,15X,'THIS ERROR OCCURS IN LINE ',I4,' OF FILE INPUTB',/)
      GO TO 120
 122  CONTINUE
      SNBI(I1)=I1
      DO 126 J=1,39
 126  AX(I1,J)=AT(J)
      GO TO 120
 140  CONTINUE
 61   READ(JU3,10)STRING
      JLINE=JLINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 60 I=1,20
      IF(TITL(I).NE.BLANK)NI=1
 60   CONTINUE
      IF(NI.EQ.0)GO TO 61
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 61
      BACKSPACE JU3
      READ(STRING,21)IC
      IF(IC.EQ.CBTC)GO TO 62
      WRITE(*,63)JLINE
 63   FORMAT(15X,'CARD ZZTC NOT FOUND IN SEQUENCE IN INPUTC',
     */,15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE ',I4,/)
      IABORT=1
      RETURN
 62   CONTINUE
      JLINE=JLINE-1
      DO 200 I=1,N
 200  READ(JU3,110)(TITL(J),J=1,20)
C UPDATE JLINE - C. SINGH DEC 1993
      JLINE=JLINE+2
      I1=0
 220  READ(JU3,10)STRING
C JLINE UPDATE - C. SINGH DEC 1993
      JLINE=JLINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 215 I=1,20
      IF(TITL(I).NE.BLANK)NI=1
 215  CONTINUE
 20   FORMAT(A2)
 21   FORMAT(A4)
      IF(NI.EQ.0)GO TO 220
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 230
      BACKSPACE JU3
C JLINE UPDATE -C. SINGH DEC 1993
      READ(STRING,21)IC
      IF(IC.EQ.CBMC)GO TO 80
      WRITE(*,81)JLINE
 81   FORMAT(15X,'CARD ZZMC NOT IN SEQUENCE IN INPUTC',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN
 80   CONTINUE
      JLINE=JLINE-1
      GO TO 240
 230  READ(STRING,121)I1,(AT(J),J=1,39)
C CHECKING FOR SERIAL NUMBER GREATER THAN 20 IN INPUTC
      IF(I1.LE.20)GO TO 124
      WRITE(*,128)I1,JLINE
 128  FORMAT(15X,'SERIAL NUMBER ',I2,' EXCEEDS 20 FOR ZZTC DATA IN FILE
     *INPUTC',/,15X,'THIS SERIAL NUMBER  AND CORRESPONDING DATA IS IGNOR
     *ED',/,15X,'THIS ERROR OCCURS IN LINE ',I4,' OF FILE INPUTC',/)
 124  CONTINUE
      SNBI(I1)=I1
      DO 225 J=1,39
 225  AX(I1,J)=AT(J)
      GO TO 220
 240  CONTINUE
      K=0
      DO 141 I=1,20
      IF(SNBI(I).EQ.0)GO TO 141
      K=K+1
 141  CONTINUE
      WRITE(JU1,260)K
 260  FORMAT(2X,I2)
      DO 150 I=1,20
      IF(SNBI(I).EQ.0)GO TO 150
      WRITE(JU1,123)(AX(I,J),J=1,39)
 150  CONTINUE
 123  FORMAT(2X,39A2)
      RETURN
      END
C
C SUBROUTINE TO CREATE ZZMC
C
      SUBROUTINE SRMC(JU1,JU2,JU3,JU4)
      INCLUDE 'NARPERC.txt'
      INCLUDE 'NARPCOM.txt'
      II=0
      N=3
      DO 100 I=1,N
 100  READ(JU2,110)(TITL(J),J=1,20)
 110  FORMAT(20A4)
      ILINE=ILINE+N
      I1=0
 120  READ(JU2,10)STRING
 10   FORMAT(A80)
C UPDATE LINE NUMBER IN INPUTB- C. SINGH DEC 1993
      ILINE=ILINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 115 I=1,20
      IF(TITL(I).NE.BLANK)NI=1
 115  CONTINUE
      IF(NI.EQ.0)GO TO 120
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 130
      BACKSPACE JU2
C UPDATE LINE NUMBER IN INPUTB -C. SINGH DEC 1993
      READ(STRING,21)IC
      IF(IC.EQ.CBLD)GO TO 70
      WRITE(*,71)ILINE
 71   FORMAT(15X,'CARD ZZLD NOT IN SEQUENCE IN INPUTB',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN
 70   CONTINUE
      ILINE=ILINE-1
      GO TO 140
 130  II=II+1
CPC
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*,END=121)JSEED,NLS,IW1,IW2,IW3,KWHERE,KVWHEN,KVSTAT,
     *KVTYPE,KVLOC,CVTEST,FINISH,JSTEP,JFREQ,MAXEUE,IOI,IOJ,IREM
     *,INTV,IREPD,IREPM
      BACKSPACE 18
C      WRITE(*,131) KVWHEN
C 131  FORMAT(' ',' AT STMT 131, KVWHEN IS',I3)
      GO TO 120
 121  CONTINUE
      WRITE(*,122)ILINE
 122  FORMAT(15X,'NO OF ENTRIES FEWER THAN REQUIRED',/,
     *15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
      RETURN
 140  CONTINUE
      III=0
      IT=II
      DO 200 I=1,N
 200  READ(JU3,110)(TITL(J),J=1,20)
C UPDATE LINE NUMBER IN INPUTC-C. SINGH DEC 1993
      JLINE=JLINE+N
      I1=0
 220  READ(JU3,10)STRING
C UPDATE LINE NUMBER IN INPUTC-C. SINGH DEC 1993
      JLINE=JLINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 215 I=1,20
      IF(TITL(I).NE.BLANK)NI=1
 215  CONTINUE
 20   FORMAT(A2)
 21   FORMAT(A4)
      IF(NI.EQ.0)GO TO 220
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 230
      BACKSPACE JU3
C UPDATE LINE NUMBER IN INPUTC
      READ(STRING,21)IC
      IF(IC.EQ.CBLD)GO TO 80
      WRITE(*,81)JLINE
 81   FORMAT(15X,'CARD ZZLD NOT IN SEQUENCE IN INPUTC',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN
 80   CONTINUE
      JLINE=JLINE-1
      GO TO 240
CPC96
 230  WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)XI1
      BACKSPACE 18
      I1 = XI1
      IF(I1.EQ.0)GO TO 270
      II=II+1
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*,END=221)JSEED,NLS,IW1,IW2,IW3,KWHERE,KVWHEN,KVSTAT,
     *KVTYPE,KVLOC,CVTEST,FINISH,JSTEP,JFREQ,MAXEUE,IOI,IOJ,IREM
     *,INTV,IREPD,IREPM
      BACKSPACE 18
C      WRITE(*,231) KVWHEN
C 231  FORMAT(' ',' AT STMT 231, KVWHEN IS',I3)
      GO TO 220
 221  CONTINUE
      WRITE(*,222)JLINE
 222  FORMAT(15X,'NO OF ENTRIES FEWER THAN REQUIRED',/,
     *15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1
      RETURN
 270  CONTINUE
      III=III+1
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)IX,SNXI(III)
      BACKSPACE 18
      GO TO 220
 240  CONTINUE
      WRITE(JU1,460)JSEED,NLS,IW1,IW2,IW3,CVTEST
C465  FORMAT(A4)
 460  FORMAT(3X,I7,2X,I1,2X,3(I2,2X),F6.3)
      FINISH=FINISH*8760.
      WRITE(*,461) KWHERE, KVWHEN, KVSTAT, KVTYPE, KVLOC
  461 FORMAT(' ',' KVs = ',5I4)
C TESTING SEASONS- C. SINGH DEC 1993
      IF(IW1.LE.52)GO TO 510
      WRITE(*,520)
      GO TO 700
 520  FORMAT(15X,'ENDING WEEK FOR SEASON 1 NOT FEASIBLE',/
     *,15X,'PROGRAM ABORTED',/)
 510  CONTINUE
      IF(IW1.LE.IW2)GO TO 530
      WRITE(*,520)
      GO TO 700
 530  CONTINUE
      IF(IW2.LE.52)GO TO 540
      WRITE(*,521)
      GO TO 700
 521  FORMAT(15X,'ENDING WEEK FOR SEASON 2 NOT FEASIBLE',/
     *,15X,'PROGRAM ABORTED',/)
 540  CONTINUE
      IF(IW2.LE.IW3)GO TO 550
      WRITE(*,521)
      GO TO 700
 550  CONTINUE
      IF(IW3.LE.52)GO TO 560
      WRITE(*,522)
      GO TO 700
 522  FORMAT(15X,'ENDING WEEK FOR SEASON 3 NOT FEASIBLE',/
     *,15X,'PROGRAM ABORTED',/)
C TESTING FOR FINISH- C. SINGH DEC 1993
 560  CONTINUE
C TESTING FOR FINISH COMMENTED OUT BECAUSE IT IS NO LONGER NEEDED.
C     INISH=FINISH/8760
C     IF(INISH.LE.100000)GO TO 570
C     WRITE(*,523)
C523  FORMAT(15X,'FIN GREATER THAN 100000',/,15X,
C    *'PROGRAM ABORTED',/)
C     GO TO 700
C570  CONTINUE
      INDX=0
      IF(JSTEP.EQ.24)INDX=1
      RETURN
 700  IABORT=IABORT+1
      RETURN
      END



C
C SUBROUTINE TO CREATE ZZLD
C
      SUBROUTINE SRLD(JU1,JU2,JU3,JU4)
      INCLUDE 'NARPERC.txt'
      INCLUDE 'NARPCOM.txt'
      ISN=1
      II=0
      N=3
      DO 100 I=1,N
 100  READ(JU2,110)(TITL(J),J=1,20)
 110  FORMAT(20A4)
      I1=0
      ILINE=ILINE+N
 120  READ(JU2,10)STRING
 10   FORMAT(A80)
C UPDATE LINE NUMBER IN INPUTB- C. SINGH DEC 1993
      ILINE=ILINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 115 I=1,20
      IF(TITL(I).NE.BLANK)NI=1
 115  CONTINUE
      IF(NI.EQ.0)GO TO 120
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 130
      BACKSPACE JU2
C UPDATE LINE NUMBER IN INPUTB -C. SINGH DEC 1993
      READ(STRING,21)IC
      IF(IC.EQ.CBUD)GO TO 70
      WRITE(*,71)ILINE
 71   FORMAT(15X,'CARD ZZUD NOT IN SEQUENCE IN INPUTB',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN
 70   CONTINUE
      ILINE=ILINE-1
      GO TO 140
 130  II=II+1
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*,END=121)SNRI(II),NAR(II),(RATES(II,J),J=1,2),
     *(ID(II,J),J=1,4),RATES(II,3)
      LNCOUNT(II,1)=ILINE
      BACKSPACE 18
C TESTING FOR FEASIBILITY OF DATA- C. SINGH DEC 1993
C TESTING FOR SN
      WRITE(*,*)II
      IF(II.EQ.1)GO TO 320
      IF(SNRI(II).GT.SNRI(II-1))GO TO 320
      WRITE(*,300)ILINE
 300  FORMAT(15X,'S.N. IN ZZLD IN INPUTB NOT IN SEQUENTIALLY ASCENDING O
     *RDER',/15X,'PROGRAM ABORTED',/15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
 320  CONTINUE
      GO TO 120
 121  CONTINUE
      WRITE(*,122)ILINE
 122  FORMAT(15X,'NO OF ENTRIES FEWER THAN REQUIRED',/,
     *15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
      RETURN
 140  CONTINUE
      III=0
      IT=II
      DO 200 I=1,N
 200  READ(JU3,110)(TITL(J),J=1,20)
C UPDATE LINE NUMBER IN INPUTC-C. SINGH DEC 1993
      JLINE=JLINE+N
      I1=0
 220  READ(JU3,10)STRING
C UPDATE LINE NUMBER IN INPUTC-C. SINGH DEC 1993
      JLINE=JLINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 215 I=1,20
      IF(TITL(I).NE.BLANK)NI=1
 215  CONTINUE
 20   FORMAT(A2)
 21   FORMAT(A4)
      IF(NI.EQ.0)GO TO 220
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 230
      BACKSPACE JU3
C UPDATE LINE NUMBER IN INPUTC
      READ(STRING,21)IC
      IF(IC.EQ.CBUD)GO TO 80
      WRITE(*,81)JLINE
 81   FORMAT(15X,'CARD ZZUD NOT IN SEQUENCE IN INPUTC',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN
 80   CONTINUE
      JLINE=JLINE-1
      GO TO 240
CPC 96
 230  WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)XI1
      BACKSPACE 18
      I1=XI1
      IF(I1.EQ.0)GO TO 270
      II=II+1
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*,END=221)SNRI(II),NAR(II),(RATES(II,J),J=1,2),
     *(ID(II,J),J=1,4),RATES(II,3)
      BACKSPACE 18
      LNCOUNT(II,1)=JLINE
      GO TO 220
 270  CONTINUE
      III=III+1
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)IX,SNXI(III)
      BACKSPACE 18
      GO TO 220
 221  CONTINUE
      WRITE(*,222)JLINE
 222  FORMAT(15X,'NO OF ENTRIES FEWER THAN REQUIRED',/,
     *15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1
      RETURN
 240  CONTINUE
      ITEMP=IT
      IT=II
      II=ITEMP
      DO 441 I=1,IT
      SNBI(I)=(SNRI(I)+.000001)*1000
      SN(I)=I
      SNDI(I)=(SNXI(I)+.000001)*1000
 441  CONTINUE
      IF(III.EQ.0)GO TO 401
      DO 400 I=1,III
      DO 400 J=1,II
      IF(SNBI(J).EQ.SNDI(I))SNBI(J)=0
 400  CONTINUE
 401  CONTINUE
      IF(II.EQ.IT)GO TO 446
      DO 442 I=1,II
      IIT=II+1
      DO 443 J=IIT,IT
      IF(SNBI(I).EQ.SNBI(J))SNBI(I)=0
 443  CONTINUE
 442  CONTINUE
 446  CONTINUE
      ILINET=ILINE
      JLINET=JLINE
      DO 470 II=1,IT
      IF(SNBI(II).EQ.0)GO TO 470
      IF(II.GT.ITEMP)GO TO 475
      ILINE=LNCOUNT(II,1)
C TESTING FOR FEASIBILITY OF DATA- C. SINGH DEC 1993
C TESTING FOR PEAK LOAD
      IF((RATES(II,1).LT.90000).AND.(RATES(II,1).GE.0))GO TO 321
      WRITE(*,301)ILINE
 301  FORMAT(15X,'PEAK IS OUT OF RANGE',/,15X,'PROGRAM ABORTED',/15X,
     *'ERROR OCCURED IN LINE IN INPUTB',I4,/)
      IABORT=1
 321  CONTINUE
C TESTING FOR LOAD FORECAST UNCERTAINTY
      IF((RATES(II,2).LT.20).AND.(RATES(II,2).GE.0))GO TO 322
      WRITE(*,302)ILINE
 302  FORMAT(15X,'LFU IS OUT OF RANGE',/,15X,'PROGRAM ABORTED',/15X,
     *'ERROR OCCURED IN LINE IN INPUTB',I4,/)
       IABORT=1
 322   CONTINUE
C TESTING FOR OUTAGE WINDOW
      IF((ID(II,1).GE.1).AND.(ID(II,2).LE.52))GO TO 303
      WRITE(*,304)ILINE
 304  FORMAT(15X,'OUTAGE WINDOW IMPROPERLY DEFINED',/,15X,'PROGRAM ABORT
     *ED',/15X,'ERROR OCCURED IN LINE IN INPUTB',I4,/)
      IABORT=1
 303  DUM=0
      IF(ID(II,2).GT.ID(II,1))GO TO 305
      WRITE(*,304)ILINE
      IABORT=1
 305  CONTINUE
C TESTING FOR FORBIDDEN PERIOD
      IF((ID(II,3).GE.ID(II,1)).AND.(ID(II,4).LE.ID(II,2)))GO TO 306
      WRITE(*,307)ILINE
      IABORT=1
 306  CONTINUE
      IF(ID(II,3).LE.ID(II,4))GO TO 308
      WRITE(*,307)ILINE
      IABORT=1
 308  CONTINUE
 307  FORMAT(15X,'FORBIDDEN PERIOD IMPROPERLY DEFINED',/,15X,
     *'PROGRAM ABORTED',/15X,'ERROR OCCURED IN LINE INPUTB',I4,/)
       GO TO 470
 475  CONTINUE
      JLINE=LNCOUNT(II,1)
C  TESTING FOR FEASIBILITY OF DATA- C. SINGH DEC 1993
C TESTING FOR PEAK LOAD
      IF((RATES(II,1).LT.90000).AND.(RATES(II,1).GE.0))GO TO 521
      WRITE(*,501)JLINE
 501  FORMAT(15X,'PEAK IS OUT OF RANGE',/,15X,'PROGRAM ABORTED',/15X,
     *'ERROR OCCURED IN LINE IN INPUTC',I4,/)
      IABORT=1
 521  CONTINUE
C TESTING FOR LOAD FORECAST UNCERTAINTY
      IF((RATES(II,2).LT.20).AND.(RATES(II,2).GE.0))GO TO 522
      WRITE(*,502)JLINE
 502  FORMAT(15X,'LFU IS OUT OF RANGE',/,15X,'PROGRAM ABORTED',/15X,
     *'ERROR OCCURED IN LINE IN INPUTC',I4,/)
       IABORT=1
 522   CONTINUE
C TESTING FOR OUTAGE WINDOW
      IF((ID(II,1).GE.1).AND.(ID(II,2).LE.52))GO TO 503
      WRITE(*,504)JLINE
 504  FORMAT(15X,'OUTAGE WINDOW IMPROPERLY DEFINED',/,15X,'PROGRAM ABORT
     *ED',/15X,'ERROR OCCURED IN LINE IN INPUTC',I4,/)
      IABORT=1
 503  DUM=0
      IF(ID(II,2).GT.ID(II,1))GO TO 505
      WRITE(*,504)JLINE
      IABORT=1
 505  CONTINUE
C TESTING FOR FORBIDDEN PERIOD
      IF((ID(II,3).GE.ID(II,1)).AND.(ID(II,4).LE.ID(II,2)))GO TO 506
      WRITE(*,507)JLINE
      IABORT=1
 506  CONTINUE
      IF(ID(II,3).LE.ID(II,4))GO TO 508
      WRITE(*,507)JLINE
      IABORT=1
 508  CONTINUE
 507  FORMAT(15X,'FORBIDDEN PERIOD IMPROPERLY DEFINED',/,15X,
     *'PROGRAM ABORTED',/15X,'ERROR OCCURED IN LINE INPUTC',I4,/)
 470  CONTINUE
      ILINE=ILINET
      JLINE=JLINET
      IF(IABORT.EQ.1)RETURN
      CALL ARANGB(SN,SNBI,IT)
      K=0
      DO 141 I=1,IT
        IF(SNBI(I).EQ.0)GO TO 141
        K=K+1
 141  CONTINUE
      WRITE(JU1,260)K
 260  FORMAT(I2)
      NOAREA=K
      LL=0
      DO 150 I=1,IT
      IF(SNBI(I).EQ.0)GO TO 150
      LL=LL+1
      L=SN(I)
      WRITE(JU1,460)(RATES(L,J),J=1,2),(ID(L,J),J=1,4),RATES(L,3)
      WRITE(JU4,465)NAR(L)
      BACKSPACE JU4
      READ(JU4,465)NAMA(LL)
      BACKSPACE JU4
      IWINDO(LL,1)=ID(L,1)
      IWINDO(LL,2)=ID(L,3)
      IWINDO(LL,3)=ID(L,4)
      IWINDO(LL,4)=ID(L,2)
 150  CONTINUE
 465  FORMAT(A4)
 460  FORMAT(F8.0,1X,F5.2,1X,4(I2,1X),F8.0)
      RETURN
      END


C
C SUBROUTINE TO CREATE ZZUD
C
      SUBROUTINE SRUD(JU1,JU2,JU3,JU4)
      INCLUDE 'NARPERC.txt'
      INCLUDE 'NARPCOM.txt'
      SNRIL=1000
      II=0
      DO 50 I=1,1000
      IPTD(I,1)=2
      IPTD(I,2)=0
      IPTD(I,3)=0
      IPTD(I,4)=0
      IPTD(I,5)=0
      IPTD(I,6)=0
 50   CONTINUE
      N=12
      DO 100 I=1,N
 100  READ(JU2,110)(TITL(J),J=1,20)
 110  FORMAT(20A4)
      ILINE=ILINE+N
      I1=0
 120  READ(JU2,10)STRING
 10   FORMAT(A80)
C UPDATE LINE NUMBER IN INPUTB- C. SINGH DEC 1993
      ILINE=ILINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0

      DO 115 I=1,20
      IF(TITL(I).NE.BLANK)NI=1
 115  CONTINUE

      IF(NI.EQ.0)GO TO 120
      READ(STRING,20)IC

      IF(IC.NE.CARD)GO TO 130
      BACKSPACE JU2

C UPDATE LINE NUMBER IN INPUTB -C. SINGH DEC 1993
      READ(STRING,21)IC
      IF(IC.EQ.CBFC)GO TO 70
      WRITE(*,71)ILINE
 71   FORMAT(15X,'CARD ZZFC NOT IN SEQUENCE IN INPUTB',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN

 70   CONTINUE
      ILINE=ILINE-1
      GO TO 140

 130  II=II+1

      IF(II.LE.SNRIL)GO TO 505
      WRITE(*,506)ILINE
 506  FORMAT(15X,'NUMBER OF ENTRIES EXCEEDS SNRI LIMIT'
     *,/,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
      RETURN

 505  CONTINUE
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*,END=121)SNRI(II),NAT(II),NAR(II),(HRLOAD(J,II),J=1,7
     *),(ID(II,J),J=1,5)
      BACKSPACE 18
      LNCOUNT(II,1)=ILINE
      ID(II,7)=1
      ID(II,8)=ILINE
      IF(II.NE.1)GO TO 510
      IF(SNRI(II).EQ.1)GO TO 510
      WRITE(*,511)ILINE
 511  FORMAT(15X,'WARNING: FIRST S.N. IN ZZUD DATA IN INPUTB NOT 1'
     *,/,15X,'WARNING OCCURED IN LINE',I4,/)
 510  CONTINUE
      GO TO 120

 121  CONTINUE
      WRITE(*,122)ILINE
 122  FORMAT(15X,'NO OF ENTRIES FEWER THAN REQUIRED',/,
     *15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
      RETURN

 140  CONTINUE
      III=0
      IT=II
      DO 200 I=1,N
 200  READ(JU3,110)(TITL(J),J=1,20)
C UPDATE LINE NUMBER IN INPUTC-C. SINGH DEC 1993
      JLINE=JLINE+N
      I1=0
 220  READ(JU3,10)STRING
C UPDATE LINE NUMBER IN INPUTC-C. SINGH DEC 1993
      JLINE=JLINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 215 I=1,20
      IF(TITL(I).NE.BLANK)NI=1
 215  CONTINUE
 20   FORMAT(A2)
 21   FORMAT(A4)
      IF(NI.EQ.0)GO TO 220
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 230
      BACKSPACE JU3
C UPDATE LINE NUMBER IN INPUTC
      READ(STRING,21)IC
      IF(IC.EQ.CBFC)GO TO 80
      WRITE(*,81)JLINE
 81   FORMAT(15X,'CARD ZZFC NOT IN SEQUENCE IN INPUTC',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN
 80   CONTINUE
      JLINE=JLINE-1
      GO TO 240
CPC 96
 230  WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)XI1
      BACKSPACE 18
      I1=XI1
      IF(I1.EQ.0)GO TO 270
      II=II+1
      IF(II.LE.SNRIL)GO TO 705
      WRITE(*,706)JLINE
 706  FORMAT(15X,'NUMBER OF ENTRIES EXCEEDS SNRI LIMIT'
     *,/,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
       IABORT=1
       RETURN
 705  CONTINUE
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*,END=221)SNRI(II),NAT(II),NAR(II),(HRLOAD(J,II),J=1,7
     *),(ID(II,J),J=1,5)
      BACKSPACE 18
      ID(II,7)=2
      ID(II,8)=JLINE
      LNCOUNT(II,1)=JLINE
      GO TO 220
 221  CONTINUE
      WRITE(*,222)JLINE
 222  FORMAT(15X,'NO OF ENTRIES FEWER THAN REQUIRED',/,
     *15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1
      RETURN
 270  CONTINUE
      III=III+1
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)IX,SNXI(III)
      BACKSPACE 18
      GO TO 220
 240  CONTINUE
      ITEMP=IT
      IT=II
      II=ITEMP
      DO 441 I=1,IT
		SNBI(I)=(SNRI(I)+.000001)*1000
		SN(I)=I
		SNDI(I)=(SNXI(I)+.000001)*1000
 441  CONTINUE

      IF(III.EQ.0)GO TO 401
		DO 400 I=1,III
			DO 400 J=1,II
				IF(SNBI(J).EQ.SNDI(I))SNBI(J)=0
 400  		CONTINUE
 401  CONTINUE

      IF(II.EQ.IT)GO TO 446
		DO 442 I=1,II
			IIT=II+1
			DO 443 J=IIT,IT
				IF(SNBI(I).EQ.SNBI(J))SNBI(I)=0
 443  		CONTINUE
 442  	CONTINUE
 446  CONTINUE

      ILINET=ILINE
      JLINET=JLINE
      DO 470 II=1,IT
C TESTING FOR DATA FEASIBILITY-C. SINGH DEC 1993
      IF(SNBI(II).EQ.0)GO TO 470
      IF(II.GT.ITEMP)GO TO 475
      ILINE=LNCOUNT(II,1)
      WRITE(JU4,465)NAT(II),NAR(II)
      BACKSPACE JU4
      READ(JU4,520)NA
      BACKSPACE JU4
      ITAG=0
      DO 525 K4=1,NOAREA
      IF(NA.EQ.NAMA(K4))ITAG=K4
 525  CONTINUE
      IF(ITAG.GE.1)GO TO 530
      WRITE(*,531)ILINE
 531  FORMAT(15X,'AREA OF LOC DOES NOT MATCH ANY AREA',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED INPUTB  IN LINE',I4,/)
      IABORT=1
 530  CONTINUE

C TEST FOR DFOR FOR AND DER
      IF((HRLOAD(5,II).LE.1).AND.(HRLOAD(5,II).GE.0))GO TO 535
      WRITE(*,536)ILINE
 536  FORMAT(15X,'DFOR NOT WITHIN RANGE',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE',I4,/)
      IABORT=1
 535  CONTINUE
      IF((HRLOAD(6,II).LE.1).AND.(HRLOAD(6,II).GE.0))GO TO 540
      WRITE(*,541)ILINE
 541  FORMAT(15X,'FOR NOT WITHIN RANGE',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
 540  CONTINUE
      IF((HRLOAD(7,II).LE.100.).AND.(HRLOAD(7,II).GE.0.))GO TO 545
      WRITE(*,551)ILINE
 551  FORMAT(15X,'DERATING NOT WITHIN RANGE',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1

C TESTING FOR MAINTENANCE DATA
 545  CONTINUE
       GO TO 470
 475  CONTINUE
      JLINE=LNCOUNT(II,1)
      WRITE(JU4,465)NAT(II),NAR(II)
      BACKSPACE JU4
      READ(JU4,520)NA
      BACKSPACE JU4
 520  FORMAT(8X,A4)
      ITAG=0
      DO 725 K4=1,NOAREA
      IF(NA.EQ.NAMA(K4))ITAG=K4
 725  CONTINUE
      IF(ITAG.GE.1)GO TO 730
      WRITE(*,731)ILINE
 731  FORMAT(15X,'AREA OF LOC DOES NOT MATCH ANY AREA',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED INPUTC  IN LINE',I4,/)
      IABORT=1
 730  CONTINUE

C TEST FOR DFOR FOR AND DER
      IF((HRLOAD(5,II).LE.1).AND.(HRLOAD(5,II).GE.0))GO TO 735
      WRITE(*,736)ILINE
 736  FORMAT(15X,'DFOR NOT WITHIN RANGE',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE',I4,/)
      IABORT=1
 735  CONTINUE
      IF((HRLOAD(6,II).LE.1).AND.(HRLOAD(6,II).GE.0))GO TO 740
      WRITE(*,741)ILINE
 741  FORMAT(15X,'FOR NOT WITHIN RANGE',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1
 740  CONTINUE
      IF((HRLOAD(7,II).LE.100.).AND.(HRLOAD(7,II).GE.0.))GO TO 745
      WRITE(*,751)ILINE
 751  FORMAT(15X,'DERATING NOT WITHIN RANGE',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1
 745  CONTINUE
 470  CONTINUE
      ILINE=ILINET
      JLINE=JLINET
      CALL ARANGB(SN,SNBI,IT)
      K=0
      DO 141 I=1,IT
      IF(SNBI(I).EQ.0)GO TO 141
      K=K+1
 141  CONTINUE
      WRITE(JU1,260)K
      NUNITS=K
 260  FORMAT(I3)

      LL=0
      DO 150 I=1,IT
        IF(SNBI(I).EQ.0)GO TO 150
        LL=LL+1
        L=SN(I)
        WRITE(JU4,465)NAT(L),NAR(L)
C      WRITE(JU1,465)NAT(L),NAR(L)
        BACKSPACE JU4
        READ(JU4,466)NAMU(LL),NUMP(LL),NA
        BACKSPACE JU4
466     FORMAT(A4,A2,2X,A4)
465     FORMAT(A8,A4)

        IF(LL.NE.1)GO TO 145
          NMP(LL)=1
          NP=1
          IPTD(NP,1)=ID(L,1)
          IPTD(NP,6)=LL
145     CONTINUE

		LM=LL-1
        IF(LM.EQ.0)GO TO 147
          DO 146 K4=1,LM
            IF(NAMU(LL).EQ.NAMU(K4))NMP(LL)=NMP(K4)
            IF(NAMU(LL).EQ.NAMU(K4))GO TO 147
146       CONTINUE
          NP=NP+1
          NMP(LL)=NP
          IPTD(NP,1)=ID(L,1)
          IPTD(NP,6)=LL
147     CONTINUE

		NUMA=0
        DO 148 K4=1,NOAREA
          IF(NA.EQ.NAMA(K4))NUMA=K4
148     CONTINUE
        WRITE(JU1,460)LL,NMP(LL),NUMA,(HRLOAD(J,L),J=1,7)
     *  ,(ID(L,J),J=2,5),ID(L,1)
        ITAG=NUMA
        NPLANT=NMP(LL)

C TESTING FOR MAINTENANCE DATA
        IF(IPTD(NPLANT,1).EQ.ID(L,1))GO TO 550
        IF(ID(L,7).EQ.2)GO TO 547
        WRITE(*,546)ID(L,8)
        GO TO 550

 547    WRITE(*,746)ID(L,8)
 546    FORMAT(15X,'MODE OF PLANNED OUTAGE(A/P) MIXED WITHIN A PLANT'
     * ,/,15X,'PROGRAM ABORTED',/,15X,
     *  'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
 550    CONTINUE

		IF(NUMA.GE.1)GO TO 555
        IF(ID(L,7).EQ.2)GO TO 557
        WRITE(*,556)ID(L,8)
        GO TO 151
 557    WRITE(*,756)ID(L,8)
 556    FORMAT(15X,'AREA OF LOCATION NOT BEING VALID',/,15X,
     *    'MAINTENANCE DATA CAN NOT BE VERIFIED',/,15X,
     *  'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
 656    FORMAT(15X,'AREA OF LOCATION NOT BEING VALID',/,15X,
     *     'MAINTENANCE DATA CAN NOT BE VERIFIED',/,15X,
     *      'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
        GO TO 151
 555    CONTINUE

		DWIN1=IWINDO(ITAG,2)-IWINDO(ITAG,1)
		DWIN2=IWINDO(ITAG,4)-IWINDO(ITAG,3)
		IPTD(NPLANT,4)=DWIN1
		IPTD(NPLANT,5)=DWIN2

	    IF(ID(II,3).EQ.0)GO TO 560
        IF(ID(II,1).EQ.0)GO TO 560
        IF(ID(II,2).GE.IWINDO(ITAG,1))GO TO 560
        IF(ID(L,7).EQ.2) GO TO 562
        WRITE(*,561)ID(L,8)
        IABORT=1
        GO TO 560
 562    WRITE(*,761)ID(L,8)
 561    FORMAT(15X,'B1 EARLIER THAN BEG OF FIRST OUTAGE WINDOW',
     *      /,15X,'PROGRAM ABORTED',/,15X,
     *      'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
        IABORT=1
 560    CONTINUE

	    IF(ID(II,3).LT.DWIN1)GO TO 570
        IF(ID(L,7).EQ.2)GO TO 572
        IABORT=1
        WRITE(*,571)ID(L,8)
        GO TO 570
 572    WRITE(*,771)ID(L,8)
 571    FORMAT(15X,'D1 GREATER THAN FIRST OUTAGE WINDOW',
     *   /,15X,'PROGRAM ABORTED',/,15X,
     *  'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
        IABORT=1
 570    CONTINUE
        IF(ID(II,5).EQ.0)GO TO 580
        IF(ID(II,1).EQ.0)GO TO 580
        IF(ID(II,4).GE.IWINDO(ITAG,3))GO TO 580
        IF(ID(L,7).EQ.2)GO TO 582
        IABORT=1
        WRITE(*,581)ID(L,8)
        GO TO 580
 582    CONTINUE
        WRITE(*,781)ID(L,8)
 581    FORMAT(15X,'B2 EARLIER THAN BEG OF SECOND OUTAGE WINDOW',
     *    /,15X,'PROGRAM ABORTED',/,15X,
     *      'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
        IABORT=1
 580    CONTINUE
        IF(ID(II,5).LT.DWIN2)GO TO 590
        IF(ID(L,7).EQ.2)GO TO 592
        IABORT=1
        WRITE(*,591)ID(L,8)
        GO TO 590
 592    WRITE(*,791)ID(L,8)
 591    FORMAT(15X,'D2 GREATER THAN SECOND OUTAGE WINDOW',
     *   /,15X,'PROGRAM ABORTED',/,15X,
     *  'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
        IABORT=1
 590    CONTINUE
        IPTD(NPLANT,2)=IPTD(NPLANT,2)+ID(L,3)
        IPTD(NPLANT,3)=IPTD(NPLANT,3)+ID(L,5)
 151    CONTINUE

 150  CONTINUE

      DO 795 I=1,NP
      IF(IPTD(I,1).EQ.1)GO TO 795
      IF(IPTD(I,2).LT.IPTD(I,4))GO TO 796
      NPLANT=IPTD(I,6)
      WRITE(*,797)NAMU(NPLANT)
      IABORT=1
 796  CONTINUE
      IF(IPTD(I,3).LT.IPTD(I,5))GO TO 795
      NPLANT=IPTD(II,6)
      WRITE(*,798)NAMU(NPLANT)
      IABORT=1
 795  CONTINUE
 797  FORMAT(15X,'TOTAL MAINTENANCE TIME GREATER THAN WINDOW 1',
     */,15X,'ERROR OCCURED IN PLANT ',A4,/,
     */15X,'PROGRAM ABORTED',/)
 798  FORMAT(15X,'TOTAL MAINTENANCE TIME GREATER THAN WINDOW 2',
     */,15X,'ERROR OCCURED IN PLANT ',A4,/,
     */15X,'PROGRAM ABORTED',/)
 746  FORMAT(15X,'MODE OF PLANNED OUTAGE(A/P) MIXED WITHIN A PLANT'
     *,/,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
 756  FORMAT(15X,'AREA OF LOCATION NOT BEING VALID',/,15X,
     *'MAINTENANCE DATA CAN NOT BE VERIFIED',/)
 761  FORMAT(15X,'B1 EARLIER THAN BEG OF FIRST OUTAGE WINDOW',
     */,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
 771  FORMAT(15X,'D1 GREATER THAN FIRST OUTAGE WINDOW',
     */,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
 781  FORMAT(15X,'B2 EARLIER THAN BEG OF SECOND OUTAGE WINDOW',
     */,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
 791  FORMAT(15X,'D2 GREATER THAN SECOND OUTAGE WINDOW',
     */,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
 460  FORMAT(3(I3,1X),4(F6.0,1X),2(F5.4,1X),F3.0,1X,2(I2,1X,I2,1
     *X),I1)
      RETURN
      END



C
C SUBROUTINE TO CREATE ZZFC
C
      SUBROUTINE SRFC(JU1,JU2,JU3,JU4)
      INCLUDE 'NARPERC.txt'
      INCLUDE 'NARPCOM.txt'
      II=0
      N=2
      DO 100 I=1,N
 100  READ(JU2,110)(TITL(J),J=1,20)
 110  FORMAT(20A4)
      ILINE=ILINE+N
      I1=0
 120  READ(JU2,10)STRING
 10   FORMAT(A80)
C UPDATE LINE NUMBER IN INPUTB- C. SINGH DEC 1993
      ILINE=ILINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 115 I=1,20
      IF(TITL(I).NE.BLANK)NI=1
 115  CONTINUE
      IF(NI.EQ.0)GO TO 120
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 130
      BACKSPACE JU2
C UPDATE LINE NUMBER IN INPUTB -C. SINGH DEC 1993
      READ(STRING,21)IC
      IF(IC.EQ.CBOD)GO TO 70
      WRITE(*,71)ILINE
 71   FORMAT(15X,'CARD ZZOD NOT IN SEQUENCE IN INPUTB',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN
 70   CONTINUE
      ILINE=ILINE-1
      GO TO 140
 130  II=II+1
      IF(II.LE.SNRIL)GO TO 505
      WRITE(*,506)ILINE
 506  FORMAT(15X,'NUMBER OF ENTRIES EXCEEDS SNRI LIMIT'
     *,/,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
       IABORT=1
       RETURN
 505  CONTINUE
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*,END=121)SNRI(II),NAR(II),NAE(II),(HRLOAD(J,II),J=1,3
     *)
      BACKSPACE 18
      LNCOUNT(II,1)=ILINE
      IF(II.NE.1)GO TO 510
      IF(SNRI(II).EQ.1)GO TO 510
      WRITE(*,511)ILINE
 511  FORMAT(15X,'WARNING: FIRST S.N. IN ZZUD DATA IN INPUTB NOT 1'
     *,/,15X,'WARNING OCCURED IN LINE',I4,/)
 510  CONTINUE
      GO TO 120
 121  CONTINUE
      WRITE(*,122)ILINE
 122  FORMAT(15X,'NO OF ENTRIES FEWER THAN REQUIRED',/,
     *15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
      RETURN
 140  CONTINUE
      III=0
      IT=II
      DO 200 I=1,N
 200  READ(JU3,110)(TITL(J),J=1,20)
C UPDATE LINE NUMBER IN INPUTC-C. SINGH DEC 1993
      JLINE=JLINE+N
      I1=0
 220  READ(JU3,10)STRING
C UPDATE LINE NUMBER IN INPUTC-C. SINGH DEC 1993
      JLINE=JLINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 215 I=1,20
      IF(TITL(I).NE.BLANK)NI=1
 215  CONTINUE
 20   FORMAT(A2)
 21   FORMAT(A4)
      IF(NI.EQ.0)GO TO 220
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 230
      BACKSPACE JU3
C UPDATE LINE NUMBER IN INPUTC
      READ(STRING,21)IC
      IF(IC.EQ.CBOD)GO TO 80
      WRITE(*,81)JLINE
 81   FORMAT(15X,'CARD ZZOD NOT IN SEQUENCE IN INPUTC',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN
 80   CONTINUE
      JLINE=JLINE-1
      GO TO 240
CPC 96
 230  WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)XI1
      BACKSPACE 18
      I1=XI1
      IF(I1.EQ.0)GO TO 270
      II=II+1
      IF(II.LE.SNRIL)GO TO 705
      WRITE(*,706)JLINE
 706  FORMAT(15X,'NUMBER OF ENTRIES EXCEEDS SNRI LIMIT'
     *,/,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
       IABORT=1
       RETURN
 705  CONTINUE
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*,END=221)SNRI(II),NAR(II),NAE(II),(HRLOAD(J,II),J=1,3
     *)
      BACKSPACE 18
      LNCOUNT(II,1)=JLINE
      GO TO 220
 221  WRITE(*,222)JLINE
 222  FORMAT(15X,'NO OF ENTRIES FEWER THAN REQUIRED',/,
     *15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1
      RETURN
 270  CONTINUE
      III=III+1
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)IX,SNXI(III)
      BACKSPACE 18
      GO TO 220
 240  CONTINUE
      ITEMP=IT
      IT=II
      II=ITEMP
      IF(IT.EQ.0)GO TO 447
      DO 441 I=1,IT
      SNBI(I)=(SNRI(I)+.000001)*1000
      SN(I)=I
      SNDI(I)=(SNXI(I)+.000001)*1000
 441  CONTINUE
      IF(III.EQ.0)GO TO 401
      IF(II.EQ.0)GO TO 401
      DO 400 I=1,III
      DO 400 J=1,II
      IF(SNBI(J).EQ.SNDI(I))SNBI(J)=0
 400  CONTINUE
 401  CONTINUE
      IF(II.EQ.IT)GO TO 446
      IF(II.EQ.0)GO TO 446
      DO 442 I=1,II
      IIT=II+1
      DO 443 J=IIT,IT
      IF(SNBI(I).EQ.SNBI(J))SNBI(I)=0
 443  CONTINUE
 442  CONTINUE
 446  CONTINUE
      IF(IT.EQ.0)GO TO 447
      ILINET=ILINE
      JLINET=JLINE
      DO 470 II=1,IT
      IF(SNBI(II).EQ.0)GO TO 470
      IF(II.GT.ITEMP)GO TO 475
C  TESTING FOR DATA FEASIBILITY-C. SINGH DEC 1993
      ILINE=LNCOUNT(II,1)
      WRITE(JU4,465)NAR(II),NAE(II)
      BACKSPACE JU4
      READ(JU4,466)NA1,NA2
      BACKSPACE JU4
      ITAG1=0
      DO 525 K4=1,NOAREA
      IF(NA1.EQ.NAMA(K4))ITAG1=K4
 525  CONTINUE
      IF(ITAG1.GE.1)GO TO 530
      WRITE(*,531)ILINE
 531  FORMAT(15X,'"FROM AREA" DOES NOT MATCH ANY AREA',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED INPUTB  IN LINE',I4,/)
      IABORT=1
 530  CONTINUE
      ITAG2=0
      DO 526 K4=1,NOAREA
      IF(NA2.EQ.NAMA(K4))ITAG2=K4
 526  CONTINUE
      IF(ITAG2.GE.1)GO TO 532
      WRITE(*,533)ILINE
 533  FORMAT(15X,'"TO AREA" DOES NOT MATCH ANY AREA',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED INPUTB  IN LINE',I4,/)
      IABORT=1
 532  CONTINUE
      IB=HRLOAD(1,II)
      IE=HRLOAD(2,II)
      MWS=HRLOAD(3,II)
      IF(IE.GE.IB)GO TO 540
      WRITE(*,541)ILINE
 541  FORMAT(15X,'END DAY EARLIER THAN BEG DAY',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
 540  CONTINUE
      IF((IB.GE.1).AND.(IE.LE.365))GO TO 550
      WRITE(*,551)ILINE
 551  FORMAT(15X,'BEG OR END DAY IMPOSSIBLE',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
 550  CONTINUE
      IF(MWS.LE.99999)GO TO 560
      WRITE(*,561)ILINE
 561  FORMAT(15X,'MW GREATER THAN ALLOWABLE FORMAT',
     */,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
 560  CONTINUE
      GO TO 470
 475  CONTINUE
      JLINE=LNCOUNT(II,1)
C TESTING FOR DATA FEASIBILITY-C. SINGH DEC 1993
      WRITE(JU4,465)NAR(II),NAE(II)
      BACKSPACE JU4
      READ(JU4,466)NA1,NA2
      BACKSPACE JU4
      ITAG1=0
      DO 725 K4=1,NOAREA
      IF(NA1.EQ.NAMA(K4))ITAG1=K4
 725  CONTINUE
      IF(ITAG1.GE.1)GO TO 730
      WRITE(*,731)JLINE
 731  FORMAT(15X,'"FROM AREA" DOES NOT MATCH ANY AREA',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED INPUTC  IN LINE',I4,/)
      IABORT=1
 730  CONTINUE
      ITAG2=0
      DO 726 K4=1,NOAREA
      IF(NA2.EQ.NAMA(K4))ITAG2=K4
 726  CONTINUE
      IF(ITAG2.GE.1)GO TO 732
      WRITE(*,733)JLINE
 733  FORMAT(15X,'"TO AREA" DOES NOT MATCH ANY AREA',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED INPUTC  IN LINE',I4,/)
      IABORT=1
 732  CONTINUE
      IB=HRLOAD(1,II)
      IE=HRLOAD(2,II)
      MWS=HRLOAD(3,II)
      IF(IE.GE.IB)GO TO 740
      WRITE(*,741)JLINE
 741  FORMAT(15X,'END DAY EARLIER THAN BEG DAY',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1
 740  CONTINUE
      IF((IB.GE.1).AND.(IE.LE.365))GO TO 750
      WRITE(*,751)JLINE
 751  FORMAT(15X,'BEG OR END DAY IMPOSSIBLE',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTc IN LINE ',I4,/)
      IABORT=1
 750  CONTINUE
      IF(MWS.LE.99999)GO TO 760
      WRITE(*,761)JLINE
 761  FORMAT(15X,'MW GREATER THAN ALLOWABLE FORMAT',
     */,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1
 760  CONTINUE
 470  CONTINUE
      ILINE=ILINET
      JLINE=JLINET
      CALL ARANGB(SN,SNBI,IT)
 447  CONTINUE
      K=0
      IF(IT.EQ.0)GO TO 142
      DO 141 I=1,IT
      IF(SNBI(I).EQ.0)GO TO 141
      K=K+1
 141  CONTINUE
 142  CONTINUE
      WRITE(JU1,260)K
 260  FORMAT(I3)
      IF(K.EQ.0)GO TO 151
      LL=0
      DO 150 I=1,IT
      IF(SNBI(I).EQ.0)GO TO 150
      LL=LL+1
      L=SN(I)
      WRITE(JU4,465)NAR(L),NAE(I)
      BACKSPACE JU4
      READ(JU4,466)NA1,NA2
      BACKSPACE JU4
466   FORMAT(A4,A4)
465   FORMAT(A4,A4)
      DO 148 K4=1,NOAREA
      IF(NA1.EQ.NAMA(K4))NUMA1=K4
      IF(NA2.EQ.NAMA(K4))NUMA2=K4
148   CONTINUE
      IB=HRLOAD(1,L)
      IE=HRLOAD(2,L)
      MWS=HRLOAD(3,L)
      WRITE(JU1,460)NUMA1,NUMA2,IB,IE,MWS
 150  CONTINUE
 460  FORMAT(I2,1X,I2,1X,I3,1X,I3,1X,I5)
 151  CONTINUE
      RETURN
      END


C
C SUBROUTINE TO CREATE ZZOD
C
      SUBROUTINE SROD(JU1,JU2,JU3,JU4)
      INCLUDE 'NARPERC.txt'
      INCLUDE 'NARPCOM.txt'
      II=0
      N=3
      DO 100 I=1,N
 100  READ(JU2,110)(TITL(J),J=1,20)
 110  FORMAT(20A4)
      ILINE=ILINE+N
      I1=0
 120  READ(JU2,10)STRING
 10   FORMAT(A80)
C UPDATE LINE NUMBER IN INPUTB- C. SINGH DEC 1993
      ILINE=ILINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 115 I=1,20
      IF(TITL(I).NE.BLANK)NI=1

 115  CONTINUE
      IF(NI.EQ.0)GO TO 120
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 130
      BACKSPACE JU2
C UPDATE LINE NUMBER IN INPUTB -C. SINGH DEC 1993
      READ(STRING,21)IC
      IF(IC.EQ.CBTD)GO TO 70
      WRITE(*,71)ILINE
 71   FORMAT(15X,'CARD ZZTD NOT IN SEQUENCE IN INPUTB',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN

 70   CONTINUE
      ILINE=ILINE-1
      GO TO 140
 130  II=II+1
      IF(II.LE.SNRIL)GO TO 505
      WRITE(*,506)ILINE
 506  FORMAT(15X,'NUMBER OF ENTRIES EXCEEDS SNRI LIMIT'
     *,/,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
       IABORT=1
       RETURN

 505  CONTINUE
CPC 96
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*,END=121)SNRI(II),NAT(II),(HRLOAD(J,II),J=1,NOAREA)
      BACKSPACE 18
      LNCOUNT(II,1)=ILINE
      GO TO 120

 121  CONTINUE
      WRITE(*,122)ILINE
 122  FORMAT(15X,'NO OF PERCENTAGES FEWER THAN NUMBER OF AREAS',/,15X
     *,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
       IABORT=1
      RETURN

 140  CONTINUE
      III=0
      IT=II
      DO 200 I=1,N
 200  READ(JU3,110)(TITL(J),J=1,20)

C UPDATE LINE NUMBER IN INPUTC-C. SINGH DEC 1993
      JLINE=JLINE+N
      I1=0
 220  READ(JU3,10)STRING
C UPDATE LINE NUMBER IN INPUTC-C. SINGH DEC 1993
      JLINE=JLINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0

      DO 215 I=1,20
		IF(TITL(I).NE.BLANK)NI=1

 215  CONTINUE
 20   FORMAT(A2)
 21   FORMAT(A4)
      IF(NI.EQ.0)GO TO 220
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 230
      BACKSPACE JU3
C UPDATE LINE NUMBER IN INPUTC
      READ(STRING,21)IC
      IF(IC.EQ.CBTD)GO TO 80
      WRITE(*,81)JLINE
 81   FORMAT(15X,'CARD ZZTD NOT IN SEQUENCE IN INPUTC',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN

 80   CONTINUE
      JLINE=JLINE-1
      GO TO 240
CPC 96
 230  WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18

      READ(18,*)XI1
      BACKSPACE 18
      I1=XI1
      IF(I1.EQ.0)GO TO 270
      II=II+1
      IF(II.LE.SNRIL)GO TO 705
      WRITE(*,706)JLINE
 706  FORMAT(15X,'NUMBER OF ENTRIES EXCEEDS SNRI LIMIT'
     *,/,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
       IABORT=1
       RETURN

 705  CONTINUE
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*,END=221)SNRI(II),NAT(II),(HRLOAD(J,II),J=1,NOAREA)
      BACKSPACE 18
      LNCOUNT(II,1)=JLINE
      GO TO 220
 221  CONTINUE
      WRITE(*,222)JLINE
 222  FORMAT(15X,'NO OF PERCENTAGES FEWER THAN NUMBER OF AREAS',/,15X
     *,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
       IABORT=1
      RETURN

 270  CONTINUE
      III=III+1
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)IX,SNXI(III)
      BACKSPACE 18
      GO TO 220
 240  CONTINUE
      ITEMP=IT
      IT=II
      II=ITEMP
      IF(IT.EQ.0)GO TO 447
      DO 441 I=1,IT
      SNBI(I)=(SNRI(I)+.000001)*1000
      SN(I)=I
      SNDI(I)=(SNXI(I)+.000001)*1000
 441  CONTINUE
      IF(III.EQ.0)GO TO 401
      IF(II.EQ.0)GO TO 401
      DO 400 I=1,III
      DO 400 J=1,II
      IF(SNBI(J).EQ.SNDI(I))SNBI(J)=0
 400  CONTINUE
 401  CONTINUE
      IF(II.EQ.IT)GO TO 446
      IF(II.EQ.0)GO TO 446
      DO 442 I=1,II
      IIT=II+1
      DO 443 J=IIT,IT
      IF(SNBI(I).EQ.SNBI(J))SNBI(I)=0
 443  CONTINUE
 442  CONTINUE
 446  CONTINUE
      IF(IT.EQ.0)GO TO 447
      ILINET=ILINE
      JLINET=JLINE

      DO 470 II=1,IT
		IF(SNBI(II).EQ.0)GO TO 470
		IF(II.GT.ITEMP)GO TO 475
		ILINE=LNCOUNT(II,1)
		WRITE(JU4,465)NAT(II)
		BACKSPACE JU4
		READ(JU4,466)NA1,NA2
		BACKSPACE JU4
		ITAG=0
		DO 510 K4=1,NUNITS
			IF((NA1.EQ.NAMU(K4)).AND.(NA2.EQ.NUMP(K4)))ITAG=K4

 510  	CONTINUE
		IF(ITAG.GE.1)GO TO 520
		WRITE(*,521)ILINE
 521  	FORMAT(15X,'JOINTLY OWNED UNIT DOES NOT EXIST IN ZZUD DATA',
     */,15X,'PROGRAM ABORTED',
     */,15X,'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
		IABORT=1

 520  	CONTINUE
		TOT=0.
		DO 530 J=1,NOAREA
			TOT=HRLOAD(J,II)+TOT

 530  	CONTINUE
		IF(TOT.EQ.100.)GO TO 535
		WRITE(*,536)ILINE
 536  	FORMAT(15X,'OWNERSHIPS DONOT SUM TO 100 PERCENT',/,
     *15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
		IABORT=1
 535  	CONTINUE
		GO TO 470

 475  	CONTINUE
		JLINE=LNCOUNT(II,1)
		WRITE(JU4,465)NAT(II)
		BACKSPACE JU4
		READ(JU4,466)NA1,NA2
		BACKSPACE JU4
		ITAG=0
		DO 710 K4=1,NUNITS
			IF((NA1.EQ.NAMU(K4)).AND.(NA2.EQ.NUMP(K4)))ITAG=K4

 710  	CONTINUE
		IF(ITAG.GE.1)GO TO 720
		WRITE(*,721)JLINE
 721  	FORMAT(15X,'JOINTLY OWNED UNIT DOES NOT EXIST IN ZZUD DATA',
     */,15X,'PROGRAM ABORTED',
     */,15X,'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
		IABORT=1
 720  	CONTINUE
		TOT=0.
		DO 730 J=1,NOAREA
			TOT=HRLOAD(J,II)+TOT

 730  	CONTINUE
		IF(TOT.EQ.100.)GO TO 735
		WRITE(*,736)JLINE
 736  	FORMAT(15X,'OWNERSHIPS DONOT SUM TO 100 PERCENT',/,
     *15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
		IABORT=1

 735  	CONTINUE

 470  CONTINUE

      ILINE=ILINET
      JLINE=JLINET
      CALL ARANGB(SN,SNBI,IT)
 447  CONTINUE
      K=0
      IF(IT.EQ.0)GO TO 142
      DO 141 I=1,IT
      IF(SNBI(I).EQ.0)GO TO 141
      K=K+1
 141  CONTINUE
 142  CONTINUE
      WRITE(JU1,260)K
      IF(K.EQ.0)GO TO 151
 260  FORMAT(I3)
      LL=0
      DO 150 I=1,IT
		IF(SNBI(I).EQ.0)GO TO 150
		LL=LL+1
		L=SN(I)
		WRITE(JU4,465)NAT(L)
		BACKSPACE JU4
		READ(JU4,466)NA1,NA2
		BACKSPACE JU4
466   	FORMAT(A4,A2)
465   	FORMAT(A8)
		DO 148 K4=1,NUNITS
			IF((NA1.EQ.NAMU(K4)).AND.(NA2.EQ.NUMP(K4)))NUMU=K4

148   	CONTINUE
		WRITE(JU1,460)NUMU,(HRLOAD(J,L),J=1,NOAREA)

 150  CONTINUE
      IF(LL.LE.600)GO TO 151
      WRITE(*,152)
 152  FORMAT(15X,'NO OF JNT OWNED UNITS GREATER THAN 600',
     */,15X,'PROGRAM ABORTED',/)
      IABORT=1

 151  CONTINUE
 460  FORMAT(I3,1X,15(F5.1,1X))
      RETURN
      END



C
C SUBROUTINE TO CREATE ZZTD
C
      SUBROUTINE SRTD(JU1,JU2,JU3,JU4)
      INCLUDE 'NARPERC.txt'
      INCLUDE 'NARPCOM.txt'
      DATA NMLR /1000*0/
      II=0
      N=7
      DO 100 I=1,N
 100  READ(JU2,110)(TITL(J),J=1,20)
 110  FORMAT(20A4)
      ILINE=ILINE+N
      I1=0
 120  READ(JU2,10)STRING
 10   FORMAT(A80)
C UPDATE LINE NUMBER IN INPUTB- C. SINGH DEC 1993
      ILINE=ILINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 115 I=1,20
      IF(TITL(I).NE.BLANK)NI=1
 115  CONTINUE
      IF(NI.EQ.0)GO TO 120
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 130
      BACKSPACE JU2
C UPDATE LINE NUMBER IN INPUTB -C. SINGH DEC 1993
      READ(STRING,21)IC
      IF(IC.EQ.CBDD)GO TO 70
      WRITE(*,71)ILINE
 71   FORMAT(15X,'CARD ZZDD NOT IN SEQUENCE IN INPUTB',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN

 70   CONTINUE
      ILINE=ILINE-1
      GO TO 140
 130  II=II+1
      BACKSPACE JU2
      IF(II.LE.SNRIL)GO TO 505
      WRITE(*,506)ILINE
 506  FORMAT(15X,'NUMBER OF ENTRIES EXCEEDS SNRI LIMIT'
     *,/,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
       IABORT=1
       RETURN

 505  CONTINUE
      READ(JU2,*)SNRI(II),ID(II,1),NAR(II),NAE(II)
     *,(HRLOAD(J,II),J=1,15),(DYLOAD(J,II),J=1,9)
      LNCOUNT(II,1)=ILINE

      IF(II.NE.1)GO TO 510
      IF(SNRI(II).EQ.1)GO TO 510

      WRITE(*,511)ILINE
 511  FORMAT(15X,'WARNING: FIRST S.N. IN ZZTD DATA IN INPUTB NOT 1'
     *,/,15X,'WARNING OCCURED IN LINE',I4,/)

 510  CONTINUE
      ILINE=ILINE+1
      GO TO 120
 140  CONTINUE
      III=0
      IT=II
      DO 200 I=1,N
 200  READ(JU3,110)(TITL(J),J=1,20)

C UPDATE LINE NUMBER IN INPUTC-C. SINGH DEC 1993
      JLINE=JLINE+N
      I1=0
 220  READ(JU3,10)STRING

C UPDATE LINE NUMBER IN INPUTC-C. SINGH DEC 1993
      JLINE=JLINE+1
      READ(STRING,110)(TITL(J),J=1,20)

      NI=0
      DO 215 I=1,20
		IF(TITL(I).NE.BLANK)NI=1

 215  CONTINUE
 20   FORMAT(A2)
 21   FORMAT(A4)

      IF(NI.EQ.0)GO TO 220
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 230
      BACKSPACE JU3

C UPDATE LINE NUMBER IN INPUTC
      READ(STRING,21)IC
      IF(IC.EQ.CBDD)GO TO 80
      WRITE(*,81)JLINE
 81   FORMAT(15X,'CARD ZZDD NOT IN SEQUENCE IN INPUTC',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN

 80   CONTINUE
      JLINE=JLINE-1
      GO TO 240

CPC 96
 230  WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)XI1
      BACKSPACE 18
      I1=XI1
      IF(I1.EQ.0)GO TO 270
      II=II+1
      BACKSPACE JU3
      IF(II.LE.SNRIL)GO TO 705
      WRITE(*,706)JLINE
 706  FORMAT(15X,'NUMBER OF ENTRIES EXCEEDS SNRI LIMIT'
     *,/,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
       IABORT=1
       RETURN

 705  CONTINUE
      READ(JU3,*)SNRI(II),ID(II,1),NAR(II),NAE(II)
     *,(HRLOAD(J,II),J=1,15),(DYLOAD(J,II),J=1,9)
      LNCOUNT(II,1)=JLINE
      JLINE=JLINE+1
      GO TO 220

 270  CONTINUE
      III=III+1

CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)IX,SNXI(III)
      BACKSPACE 18
      JLINE=JLINE+1
      GO TO 220

 240  CONTINUE
      ITEMP=IT
      IT=II
      II=ITEMP
      DO 441 I=1,IT
      SNBI(I)=(SNRI(I)+.000001)*1000
      SN(I)=I
      SNDI(I)=(SNXI(I)+.000001)*1000

 441  CONTINUE
      IF(III.EQ.0)GO TO 401
      DO 400 I=1,III
      DO 400 J=1,II
      IF(SNBI(J).EQ.SNDI(I))SNBI(J)=0

 400  CONTINUE
 401  CONTINUE
      IF(II.EQ.IT)GO TO 446
      DO 442 I=1,II
		IIT=II+1
		DO 443 J=IIT,IT
			IF(SNBI(I).EQ.SNBI(J))SNBI(I)=0

 443  	CONTINUE
 442  CONTINUE

 446  CONTINUE
      ILINET=ILINE
      JLINET=JLINE
      DO 470 II=1,IT
		IF(SNBI(II).EQ.0)GO TO 470
		IF(II.GT.ITEMP)GO TO 475
		ILINE=LNCOUNT(II,1)
      WRITE(JU4,465)NAR(II),NAE(II)
      BACKSPACE JU4
      READ(JU4,466)NA1,NA2
      BACKSPACE JU4
      ITAG1=0
      DO 525 K4=1,NOAREA
      IF(NA1.EQ.NAMA(K4))ITAG1=K4
 525  CONTINUE
      IF(ITAG1.GE.1)GO TO 530
      WRITE(*,531)ILINE
 531  FORMAT(15X,'"FROM AREA" DOES NOT MATCH ANY AREA',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED INPUTB  IN LINE',I4,/)
      IABORT=1
 530  CONTINUE
      ITAG2=0
      DO 526 K4=1,NOAREA
      IF(NA2.EQ.NAMA(K4))ITAG2=K4
 526  CONTINUE
      IF(ITAG2.GE.1)GO TO 532
      WRITE(*,533)ILINE
 533  FORMAT(15X,'"TO AREA" DOES NOT MATCH ANY AREA',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED INPUTB  IN LINE',I4,/)
      IABORT=1
 532  CONTINUE
      ITAG=0
      IF(HRLOAD(1,II).GT.0.)ITAG=1
      IF(HRLOAD(5,II).GT.0.)ITAG=1
      IF(HRLOAD(9,II).GT.0.)ITAG=1
      IF(HRLOAD(13,II).GT.0.)ITAG=1
      IF(DYLOAD(2,II).GT.0.)ITAG=1
      IF(DYLOAD(6,II).GT.0.)ITAG=1
      IF(ITAG.EQ.0)GO TO 535
      WRITE(*,536)ILINE
 536  FORMAT(15X,'ADMITANCE SPECIFIED POSITIVE',/,
     *15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
 535  CONTINUE
      ITAG=0
      IF(HRLOAD(2,II).LT.0.)ITAG=1
      IF(HRLOAD(3,II).LT.0.)ITAG=1
      IF(HRLOAD(6,II).LT.0.)ITAG=1
      IF(HRLOAD(7,II).LT.0.)ITAG=1
      IF(HRLOAD(10,II).LT.0.)ITAG=1
      IF(HRLOAD(11,II).LT.0.)ITAG=1
      IF(HRLOAD(14,II).LT.0.)ITAG=1
      IF(HRLOAD(15,II).LT.0.)ITAG=1
      IF(DYLOAD(3,II).LT.0.)ITAG=1
      IF(DYLOAD(4,II).LT.0.)ITAG=1
      IF(DYLOAD(7,II).LT.0.)ITAG=1
      IF(DYLOAD(8,II).LT.0.)ITAG=1
      IF(ITAG.EQ.0)GO TO 540
      WRITE(*,541)ILINE
 541  FORMAT(15X,'CAPACITY IS SPECIFIED AS NEGATIVE',
     */,15X,'PROGRAM ABORTED',/,
     *15X,'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
 540  CONTINUE
      SUMPR=HRLOAD(4,II)+HRLOAD(8,II)+HRLOAD(12,II)+
     *DYLOAD(1,II)+DYLOAD(5,II)+DYLOAD(9,II)-1.
      SUMPR=ABS(SUMPR)
      IF(SUMPR.LE..0001)GO TO 545
      WRITE(*,546)ILINE
 546  FORMAT(15X,'SUM OF PROBABILITIES DOES NOT ADD TO 1.'
     *,/,15X,'PROGRAM ABORTED',/,
     *15X,'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
 545  CONTINUE
      GO TO 470
 475  CONTINUE
      JLINE=JLINET
      WRITE(JU4,465)NAR(II),NAE(II)
      BACKSPACE JU4
      READ(JU4,466)NA1,NA2
      BACKSPACE JU4
      ITAG1=0
      DO 725 K4=1,NOAREA
      IF(NA1.EQ.NAMA(K4))ITAG1=K4
 725  CONTINUE
      IF(ITAG1.GE.1)GO TO 730
      WRITE(*,731)JLINE
 731  FORMAT(15X,'"FROM AREA" DOES NOT MATCH ANY AREA',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED INPUTC  IN LINE',I4,/)
      IABORT=1
 730  CONTINUE
      ITAG2=0
      DO 726 K4=1,NOAREA
      IF(NA2.EQ.NAMA(K4))ITAG2=K4
 726  CONTINUE
      IF(ITAG2.GE.1)GO TO 732
      WRITE(*,733)JLINE
 733  FORMAT(15X,'"TO AREA" DOES NOT MATCH ANY AREA',/,15X,
     *'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED INPUTC  IN LINE',I4,/)
      IABORT=1
 732  CONTINUE
      ITAG=0
      IF(HRLOAD(1,II).GT.0.)ITAG=1
      IF(HRLOAD(5,II).GT.0.)ITAG=1
      IF(HRLOAD(9,II).GT.0.)ITAG=1
      IF(HRLOAD(13,II).GT.0.)ITAG=1
      IF(DYLOAD(2,II).GT.0.)ITAG=1
      IF(DYLOAD(6,II).GT.0.)ITAG=1
      IF(ITAG.EQ.0)GO TO 735
      WRITE(*,736)JLINE
 736  FORMAT(15X,'ADMITANCE SPECIFIED POSITIVE',/,
     *15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1
 735  CONTINUE
      ITAG=0
      IF(HRLOAD(2,II).LT.0.)ITAG=1
      IF(HRLOAD(3,II).LT.0.)ITAG=1
      IF(HRLOAD(6,II).LT.0.)ITAG=1
      IF(HRLOAD(7,II).LT.0.)ITAG=1
      IF(HRLOAD(10,II).LT.0.)ITAG=1
      IF(HRLOAD(11,II).LT.0.)ITAG=1
      IF(HRLOAD(14,II).LT.0.)ITAG=1
      IF(HRLOAD(15,II).LT.0.)ITAG=1
      IF(DYLOAD(3,II).LT.0.)ITAG=1
      IF(DYLOAD(4,II).LT.0.)ITAG=1
      IF(DYLOAD(7,II).LT.0.)ITAG=1
      IF(DYLOAD(8,II).LT.0.)ITAG=1
      IF(ITAG.EQ.0)GO TO 740
      WRITE(*,741)JLINE
 741  FORMAT(15X,'CAPACITY IS SPECIFIED AS NEGATIVE',
     */,15X,'PROGRAM ABORTED',/,
     *15X,'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1

 740  CONTINUE
      SUMPR=HRLOAD(4,II)+HRLOAD(8,II)+HRLOAD(12,II)+
     *DYLOAD(1,II)+DYLOAD(5,II)+DYLOAD(9,II)-1.
      SUMPR=ABS(SUMPR)
      IF(SUMPR.LE..0001)GO TO 745
      WRITE(*,746)JLINE
 746  FORMAT(15X,'SUM OF PROBABILITIES DOES NOT ADD TO 1.'
     *,/,15X,'PROGRAM ABORTED',/,
     *15X,'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1

 745  CONTINUE
 470  CONTINUE
      ILINE=ILINET
      JLINE=JLINET

      CALL ARANGB(SN,SNBI,IT)

      K=0
      DO 141 I=1,IT
		IF(SNBI(I).EQ.0)GO TO 141
		K=K+1

 141  CONTINUE
      WRITE(JU1,260)K
 260  FORMAT(I2)
      LL=0
      DO 150 I=1,IT
		IF(SNBI(I).EQ.0)GO TO 150
		LL=LL+1
		L=SN(I)

		WRITE(JU4,465)NAR(L),NAE(L)
		BACKSPACE JU4
		READ(JU4,466)NA1,NA2
		BACKSPACE JU4
466   	FORMAT(A4,A4)
465   	FORMAT(A4,A4)

		DO 148 K4=1,NOAREA
			IF(NA1.EQ.NAMA(K4))NUMA1=K4
			IF(NA2.EQ.NAMA(K4))NUMA2=K4
148   	CONTINUE
		NML(LL)=ID(L,1)
		NMLR(ID(L,1))=LL
		WRITE(JU1,460)LL,NUMA1,NUMA2
     *,(HRLOAD(J,L),J=1,3)
     *,(HRLOAD(J,L),J=5,7)
     *,(HRLOAD(J,L),J=9,11)
     *,(HRLOAD(J,L),J=13,15)
     *,(DYLOAD(J,L),J=2,4)
     *,(DYLOAD(J,L),J=6,8),HRLOAD(4,L),HRLOAD(8,L),HRLOAD(12,L)
     *,DYLOAD(1,L),DYLOAD(5,L),DYLOAD(9,L)
 150  CONTINUE
 460  FORMAT(3(I2,1X),6(F7.1,1X),/,9(F7.1,1X),/,3(F7.1,1X)
     *,6(F7.5,1X))
      RETURN
      END



C
C SUBROUTINE TO CREATE ZZDD
C
      SUBROUTINE SRDD(JU1,JU2,JU3,JU4)
      INCLUDE 'NARPERC.txt'
      INCLUDE 'NARPCOM.txt'
      II=0
      N=3

      DO 100 I=1,N
 100    READ(JU2,110)(TITL(J),J=1,20)
 110  FORMAT(20A4)

      ILINE=ILINE+N
      I1=0
 120  READ(JU2,10,END=123)STRING
 10   FORMAT(A80)

C UPDATE LINE NUMBER IN INPUTB- C. SINGH DEC 1993
      ILINE=ILINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 115 I=1,20
        IF(TITL(I).NE.BLANK)NI=1
 115  CONTINUE

      IF(NI.EQ.0)GO TO 120
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 130
      BACKSPACE JU2

C UPDATE LINE NUMBER IN INPUTB -C. SINGH DEC 1993
      READ(STRING,21)IC
      IF(IC.EQ.CBND)GO TO 70
      WRITE(*,71)ILINE
 71   FORMAT(15X,'CARD ZZND NOT IN SEQUENCE IN INPUTB',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN

 70   CONTINUE
      ILINE=ILINE-1
      GO TO 140

 130  II=II+1
      IF(II.LE.SNRIL)GO TO 505
      WRITE(*,506)ILINE
 506  FORMAT(15X,'NUMBER OF ENTRIES EXCEEDS SNRI LIMIT'
     *,/,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
       IABORT=1
       RETURN
 505  CONTINUE
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)SNRI(II),ID(II,1),NAT(II),NAT(II+250),
     *NAT(II+500),NAT(II+750),RATES(II,1),RATES(II,2)
      BACKSPACE 18
      LNCOUNT(II,1)=ILINE
      IF(II.NE.1)GO TO 510
      IF(SNRI(II).EQ.1)GO TO 510
      WRITE(*,511)ILINE
 511  FORMAT(15X,'WARNING: FIRST S.N. IN ZZDD DATA IN INPUTB NOT 1'
     *,/,15X,'WARNING OCCURED IN LINE',I4,/)
 510  CONTINUE
      GO TO 120

 123  CONTINUE

      WRITE(*,124)
 124  FORMAT(15X,'END OF INPUTB REACHED BEFORE ZZND',/,
     *15X,'PROGRAM ABORTED',/)
      IABORT=1
      RETURN

 140  CONTINUE
      III=0
      IT=II
      DO 200 I=1,N
 200  READ(JU3,110)(TITL(J),J=1,20)
C UPDATE LINE NUMBER IN INPUTC-C. SINGH DEC 1993
      JLINE=JLINE+N
      I1=0
 220  READ(JU3,10,END=223)STRING
C UPDATE LINE NUMBER IN INPUTC-C. SINGH DEC 1993
      JLINE=JLINE+1
      READ(STRING,110)(TITL(J),J=1,20)
      NI=0
      DO 215 I=1,20
      IF(TITL(I).NE.BLANK)NI=1
 215  CONTINUE
 20   FORMAT(A2)
 21   FORMAT(A4)
      IF(NI.EQ.0)GO TO 220
      READ(STRING,20)IC
      IF(IC.NE.CARD)GO TO 230
      BACKSPACE JU3
C UPDATE LINE NUMBER IN INPUTC
      READ(STRING,21)IC
      IF(IC.EQ.CBND)GO TO 80
      WRITE(*,81)JLINE
 81   FORMAT(15X,'CARD ZZND NOT IN SEQUENCE IN INPUTC',/,
     *15X,'PROGRAM ABORTED',/,15X,'ERROR OCCURED IN LINE',I4,/)
      IABORT=1
      RETURN
 80   CONTINUE
      JLINE=JLINE-1
      GO TO 240
CPC 96
 230  WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)XI1
      BACKSPACE 18
      I1=XI1
      IF(I1.EQ.0)GO TO 270
      II=II+1
      IF(II.LE.SNRIL)GO TO 705
      WRITE(*,706)JLINE
 706  FORMAT(15X,'NUMBER OF ENTRIES EXCEEDS SNRI LIMIT'
     *,/,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
       IABORT=1
       RETURN
 705  CONTINUE
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)SNRI(II),ID(II,1),NAT(II),NAT(II+250),
     *NAT(II+500),NAT(II+750),RATES(II,1),RATES(II,2)
      BACKSPACE 18
      LNCOUNT(II,1)=JLINE
      GO TO 220
 223  CONTINUE
      WRITE(*,224)
 224  FORMAT(15X,'END OF INPUTC REACHED BEFORE ZZND',/,
     *15X,'PROGRAM ABORTED',/)
      IABORT=1
      RETURN
 270  CONTINUE
      III=III+1
CPC 96
      WRITE(18,FMT='(A80)')STRING
      BACKSPACE 18
      READ(18,*)IX,SNXI(III)
      BACKSPACE 18
      GO TO 220

 240  CONTINUE
      ITEMP=IT
      IT=II
      II=ITEMP
      IF(IT.EQ.0)GO TO 447

      DO 441 I=1,IT
        SNBI(I)=(SNRI(I)+.000001)*1000
        SN(I)=I
        SNDI(I)=(SNXI(I)+.000001)*1000
 441  CONTINUE

      IF(III.EQ.0)GO TO 401
      IF(II.EQ.0)GO TO 401
      DO 400 I=1,III
      DO 400 J=1,II
      IF(SNBI(J).EQ.SNDI(I))SNBI(J)=0
 400  CONTINUE
 401  CONTINUE
      IF(II.EQ.IT)GO TO 446
      IF(II.EQ.0)GO TO 446
      DO 442 I=1,II
      IIT=II+1
      DO 443 J=IIT,IT
      IF(SNBI(I).EQ.SNBI(J))SNBI(I)=0
 443  CONTINUE
 442  CONTINUE
 446  CONTINUE
      IF(IT.EQ.0)GO TO 447
      ILINET=ILINE
      JLINET=JLINE
      DO 470 II=1,IT
      IF(SNBI(II).EQ.0)GO TO 470
      IF(II.GT.ITEMP)GO TO 475
      ILINE=LNCOUNT(II,1)
      WRITE(JU4,465)NAT(II),NAT(250+II),NAT(500+II),NAT(750+II)
      BACKSPACE JU4
      READ(JU4,466)(NATT(J),J=1,8)
      BACKSPACE JU4
      NUMU1=0
      NUMU2=0
      NUMU3=0
      NUMU4=0
      DO 548 K4=1,NUNITS
      IF((NATT(1).EQ.NAMU(K4)).AND.(NATT(2).EQ.NUMP(K4)))NUMU1=K4
      IF((NATT(3).EQ.NAMU(K4)).AND.(NATT(4).EQ.NUMP(K4)))NUMU2=K4
      IF((NATT(5).EQ.NAMU(K4)).AND.(NATT(6).EQ.NUMP(K4)))NUMU3=K4
      IF((NATT(7).EQ.NAMU(K4)).AND.(NATT(8).EQ.NUMP(K4)))NUMU4=K4
548   CONTINUE
      IF(NUMU1.GE.1)GO TO 520
      IF(NATT(1).EQ.CBXX)GO TO 520
      WRITE(*,521)ILINE
 521  FORMAT(15X,'FIRST UNIT UNIDENTIFIED',/,
     *15X,'PROGRAM ABORTED',/,15X
     *,'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
 520  CONTINUE
      IF(NUMU2.GE.1)GO TO 525
      IF(NATT(3).EQ.CBXX)GO TO 525
      WRITE(*,526)ILINE
 526  FORMAT(15X,'SECOND UNIT UNIDENTIFIED',/,
     *15X,'PROGRAM ABORTED',/,15X
     *,'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
 525  CONTINUE
      IF(NUMU3.GE.1)GO TO 530
      IF(NATT(5).EQ.CBXX)GO TO 530
      WRITE(*,531)ILINE
 531  FORMAT(15X,'THIRD UNIT UNIDENTIFIED',/,
     *15X,'PROGRAM ABORTED',/,15X
     *,'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
 530  CONTINUE
      IF(NUMU4.GE.1)GO TO 540
      IF(NATT(7).EQ.CBXX)GO TO 540
      WRITE(*,541)ILINE
 541  FORMAT(15X,'FOURTH UNIT UNIDENTIFIED',/,
     *15X,'PROGRAM ABORTED',/,15X
     *,'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
      IABORT=1
 540  CONTINUE
      LX=ID(II,1)
      NLL=NMLR(LX)
      IF(NLL.NE.0)GO TO 550
      WRITE(*,551)ILINE
 551  FORMAT(15X,'UNIDENTIFIABLE LINE NUMBER',/
     *,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTB IN LINE ',I4,/)
 550  CONTINUE
      GO TO 470
 475  CONTINUE
      JLINE=LNCOUNT(II,1)
      WRITE(JU4,465)NAT(II),NAT(250+II),NAT(500+II),NAT(750+II)
      BACKSPACE JU4
      READ(JU4,466)(NATT(J),J=1,8)
      BACKSPACE JU4

      NUMU1=0
      NUMU2=0
      NUMU3=0
      NUMU4=0
      DO 748 K4=1,NUNITS
        IF((NATT(1).EQ.NAMU(K4)).AND.(NATT(2).EQ.NUMP(K4)))NUMU1=K4
        IF((NATT(3).EQ.NAMU(K4)).AND.(NATT(4).EQ.NUMP(K4)))NUMU2=K4
        IF((NATT(5).EQ.NAMU(K4)).AND.(NATT(6).EQ.NUMP(K4)))NUMU3=K4
        IF((NATT(7).EQ.NAMU(K4)).AND.(NATT(8).EQ.NUMP(K4)))NUMU4=K4
748   CONTINUE

      IF(NUMU1.GE.1)GO TO 720
      IF(NATT(1).EQ.CBXX)GO TO 720
      WRITE(*,721)JLINE
 721  FORMAT(15X,'FIRST UNIT UNIDENTIFIED',/,
     *15X,'PROGRAM ABORTED',/,15X
     *,'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1
 720  CONTINUE
      IF(NUMU2.GE.1)GO TO 725
      IF(NATT(3).EQ.CBXX)GO TO 725
      WRITE(*,726)JLINE
 726  FORMAT(15X,'SECOND UNIT UNIDENTIFIED',/,
     *15X,'PROGRAM ABORTED',/,15X
     *,'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1
 725  CONTINUE
      IF(NUMU3.GE.1)GO TO 730
      IF(NATT(5).EQ.CBXX)GO TO 730
      WRITE(*,731)JLINE
 731  FORMAT(15X,'THIRD UNIT UNIDENTIFIED',/,
     *15X,'PROGRAM ABORTED',/,15X
     *,'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1
 730  CONTINUE
      IF(NUMU4.GE.1)GO TO 740
      IF(NATT(7).EQ.CBXX)GO TO 740
      WRITE(*,741)JLINE
 741  FORMAT(15X,'FOURTH UNIT UNIDENTIFIED',/,
     *15X,'PROGRAM ABORTED',/,15X
     *,'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
      IABORT=1
 740  CONTINUE
      LX=ID(II,1)
      NLL=NMLR(LX)
      IF(NLL.NE.0)GO TO 750
      WRITE(*,751)JLINE
 751  FORMAT(15X,'UNIDENTIFIABLE LINE NUMBER',/
     *,15X,'PROGRAM ABORTED',/,15X,
     *'ERROR OCCURED IN INPUTC IN LINE ',I4,/)
 750  CONTINUE
 470  CONTINUE
      ILINE=ILINET
      JLINE=JLINET
      CALL ARANGB(SN,SNBI,IT)
 447  CONTINUE
      K=0
      IF(IT.EQ.0)GO TO 142
      DO 141 I=1,IT
      IF(SNBI(I).EQ.0)GO TO 141
      K=K+1
 141  CONTINUE
 142  CONTINUE
      WRITE(JU1,260)K
      IF(K.EQ.0)GO TO 151
 260  FORMAT(I3)
      LL=0
      DO 150 I=1,IT
      IF(SNBI(I).EQ.0)GO TO 150
      LL=LL+1
      IF(LL.LE.50)GO TO 152
      WRITE(*,153)
      IABORT=1
      GO TO 151
 152  CONTINUE
      L=SN(I)
      WRITE(JU4,465)NAT(L),NAT(250+L),NAT(500+L),NAT(750+L)
      BACKSPACE JU4
      READ(JU4,466)(NATT(J),J=1,8)
      BACKSPACE JU4
466   FORMAT(4(A4,A2,2X))
465   FORMAT(4A8)

      NUMU1=0
      NUMU2=0
      NUMU3=0
      NUMU4=0
      DO 148 K4=1,NUNITS
        IF((NATT(1).EQ.NAMU(K4)).AND.(NATT(2).EQ.NUMP(K4)))NUMU1=K4
        IF((NATT(3).EQ.NAMU(K4)).AND.(NATT(4).EQ.NUMP(K4)))NUMU2=K4
        IF((NATT(5).EQ.NAMU(K4)).AND.(NATT(6).EQ.NUMP(K4)))NUMU3=K4
        IF((NATT(7).EQ.NAMU(K4)).AND.(NATT(8).EQ.NUMP(K4)))NUMU4=K4
148   CONTINUE

      LX=ID(L,1)
      NLL=NMLR(LX)
      WRITE(JU1,460)NLL,NUMU1,NUMU2,NUMU3,NUMU4,RATES(L,1)
     *,RATES(L,2)
 150  CONTINUE
 460  FORMAT(I2,1X,4(I3,1X),2(F5.2,1X))
 153  FORMAT(15X,'NUMBER OF ENTRIES FOR LINE DERATING GREATER THAN 50',
     */,15X,'ERROR OCCURED IN ZZDD DATA',/,15X,'PROGRAM ABORTED',/)
 151  CONTINUE
      RETURN
      END



C------Yongli: Adjust if SNBI not in ascending order -------
      SUBROUTINE ARANGB(SN,SNBI,II)
      INTEGER SNBI(500),SN(500)
      III=II-1
      DO 100 I=1,III
		I1=I+1
		DO 100 J=I1,II
			IF(SNBI(I).LE.SNBI(J))GO TO 100
			ITEM1 =SNBI(I)
			ITEM2 =SN(I)
			SNBI(I)=SNBI(J)
			SN(I)=SN(J)
			SNBI(J)=ITEM1
			SN(J)=ITEM2
 100  CONTINUE
      RETURN
      END



       SUBROUTINE PIND(JU1,JU)
C   THERE WERE MANY FTN ERRORS CLEANED UP HERE IN DIM STMTS GENE P. 1/26/89
      INCLUDE 'NARPERC.txt'
      INTEGER POL
      CHARACTER*4 POL1,NPOL1,MODA1,MODP1
      EQUIVALENCE (POL1,POL),(NPOL1,NPOL),(MODA1,MODA),(MODP1,MODP)
      DIMENSION C(15)
      INCLUDE 'NARPCOM.txt'
      DATA POL1/'YES'/,NPOL1/'NO'/,MODA1/'A'/,MODP1/'P'/
       ITAB=0
       READ(JU1,5)N
       DO 8 I=1,N
       READ(JU1,7)(AT(J),J=1,40)
       WRITE(JU,9)(AT(J),J=1,40)
 8     CONTINUE
 9     FORMAT(20X,40A2)
 7     FORMAT(40A2)
 5     FORMAT(2X,I2)
 10    FORMAT(1H1)
 15    FORMAT(////)
       WRITE(JU,10)
       READ(JU1,20)JSEED,NLS,IW1,IW2,IW3
 20    FORMAT(3X,I7,2X,I1,2X,3(I2,2X))
       ICOM=POL
       IF(NLS.EQ.1)ICOM=NPOL
       CVTESP=CVTEST*100.
       WRITE(JU,30)JSEED,ICOM,IW1,IW2,IW3,CVTESP
 30    FORMAT(10X,'SUMMARY OF OPTIONS :',//,
     * 10X,'SEED FOR RANDOM NUMBER GENERATOR = ',I7,//,
     * 10X,'LOSS SHARING = ',A4,//,10X,'ENDING WEEK FOR FIRST SEASON = '
     *,I2,//,10X,'ENDING WEEK FOR SECOND SEASON =',I2,//,10X,'ENDING WEE
     *K FOR THIRD SEASON = ',I2,//10X,'SPECIFICATIONS FOR CONVERGENCE TE
     *ST :',//,15X,'* PERCENT ERROR FOR CONVERGENCE (CVT) : ',F5.2)
       IF(KWHERE.EQ.2)GO TO 21
       WRITE(JU,22)KVLOC
       GO TO 41
 21    WRITE(JU,23)
 22    FORMAT(15X,'* CONVERGENCE BASED ON  STATISTICS OF AREA ',I2)
 23    FORMAT(15X,'* CONVERGRNCE BASED ON POOL STATISTICS')
 41    CONTINUE
       IF(KVWHEN.EQ.2)GO TO 24
       IF(KVSTAT.EQ.2)GO TO 27
       WRITE(JU,25)
       GO TO 42
 27    WRITE(JU,26)
       GO TO 42
 24    WRITE(JU,28)
 25    FORMAT(15X,'* HOURLY LOLE IS USED FOR CONVERGENCE')
 26    FORMAT(15X,'* EUE IS USED FOR CONVERGENCE')
 28    FORMAT(15X,'* LOLE IS USED FOR CONVERGENCE')
 42    CONTINUE
       IF(KVTYPE.EQ.2)GO TO 31
       WRITE(JU,32)
       GO TO 43
 31    WRITE(JU,33)
 32    FORMAT(15X,'* WEIGHTED AVERAGE OF STATISTICS USED FOR CONVERGEN',
     *'CE')
 33    FORMAT(15X,'* STATISTICS FOR BASE FORECAST USED FOR CONVERGENCE')
 43    CONTINUE
       IFIN=FINISH/8760
       WRITE(JU,34)IFIN
 34    FORMAT(15X,'* REPLICATIONS TO END SIMULATION IF NO CONVERGENCE :'
     *,I4,//)
       WRITE(JU,35)
 35    FORMAT(10X,'OTHER VARIABLES',//)
       IF(JSTEP.EQ.24)GO TO 36
       WRITE(JU,37)
       GO TO 44
 36    WRITE(JU,38)
 37    FORMAT(15X,'* STATISTCS ARE COLLECTED EVERY HOUR')
 38    FORMAT(15X,'* STATISTICS ARE COLLECTED AT POOL PEAK HOUR')
 44    CONTINUE
       IF(JFREQ.EQ.24)GO TO 46
       WRITE(JU,47)
       GO TO 48
 46    WRITE(JU,49)
 47    FORMAT(15X,'* MONTE CARLO DRAWS ARE PERFORMED EVERY HOUR')
 49    FORMAT(15X,'* MONTE CARLO DRAWS ARE PERFORMED DAILY')
 48    CONTINUE
       IF(IOI.EQ.1)GO TO 51
       WRITE(JU,52)
       GO TO 53
 51    WRITE(JU,54)
 52    FORMAT(15X,'* RESULTS OF TRANSMISSION MODULE ARE NOT PRINTED')
 54    FORMAT(15X,'* RESULTS OF TRANSMISSION MODLE PRINTED IN TRAOUT')
 53    CONTINUE
       IF(IOJ.EQ.1)GO TO 55
       WRITE(JU,56)
       GO TO 57
 55    WRITE(JU,59)
 57    CONTINUE
 56    FORMAT(15X,'* RESULTS AFTER EACH REPLICATION ARE NOT PRINTED')
 59    FORMAT(15X,'* RESULTS AFTER EACH REPLICATION ARE PRINTED')
       WRITE(JU,61)MAXEUE
 61    FORMAT(15X,'* UPPER LIMIT OF EUE PROB DISTRIBUTION =',I8,' MW')
       IF(IRST.EQ.0)GO TO 65
       WRITE(JU,66)
 66    FORMAT(15x,///,15X,'**WARNING ! THIS IS A RESTART CASE.')
       GO TO 67
 65    CONTINUE
       WRITE(16,68)
 68    FORMAT(15X,'* THIS IS A NEW RUN.',/)
 67    CONTINUE
       IF(IREPD.EQ.0)GO TO 999
       WRITE(JU,10)
       WRITE(JU,15)
       ITAB=ITAB+1
       WRITE(JU,110)ITAB
110    FORMAT(40X,'TABLE',I4,/,40X,'SYSTEM DATA',/,40X,'-----------',/)
       READ(JU1,115)N
115    FORMAT(I2)
       WRITE(JU,120)
120    FORMAT(5X,'AREA   PEAK   LOAD FORECAST   OUTAGE WINDOW  FORBIDDEN
     * PERIOD  SUM OF FLOWS')
       WRITE(JU,121)
121    FORMAT(5X,'        MW   UNCERTAINTY (%)  BEG WK  END WK  BEG WK
     *END WK     CONSTRAINT')
       DO 200 I=1,N
       READ(JU1,125)R1,R2,M3,M4,M5,M6,R7
       WRITE(JU,130)NAMA(I),R1,R2,M3,M4,M5,M6,R7
200    CONTINUE
125    FORMAT(F8.0,1X,F5.0,1X,4(I2,1X),F8.0)
130    FORMAT(5X,A4,2X,F7.0,4X,F5.2,10X,I2,6X,I2,6X,I2,6X,I2,8X,F7
     *.0)
       WRITE(JU,10)
       WRITE(JU,15)
       ITAB=ITAB+1
       WRITE(JU,310)ITAB
310    FORMAT(40X,'TABLE',I4,/,40X,'UNIT DATA',/,40X,'--------',/)
335    FORMAT(60X,'...........CONTINUED',/)
       WRITE(JU,340)
       ICOUNT=50
       IC=0
       READ(JU1,315)NUNITS
315    FORMAT(I3)
       DO 300 I=1,NUNITS
         READ(JU1,320)M1,M2,M3,R1,R2,R3,R4,R5,R6,R7,N1,N2,N3,N4,N5
         IF(IC.NE.ICOUNT)GO TO 330
           WRITE(JU,10)
           WRITE(JU,15)
           WRITE(JU,335)
           WRITE(JU,310)ITAB
           WRITE(JU,340)
           ICOUNT=ICOUNT+50
330      CONTINUE
         ICOM=MODP
         IF(N5.EQ.0)ICOM=MODA
         WRITE(JU,325)NAMU(M1),NUMP(M1),NAMA(M3),R1,R2,R3,R4,R5,R6,R7,
     *      ICOM,N1,N2,N3,N4
300    CONTINUE

325    FORMAT(5X,A4,A2,6X,A4,6X,4(F6.0,1X),2X,F5.4,4X,F5.4,4X,F3.0,5X,
     *A4,5X,2(I2,4X,I2,6X))
320    FORMAT(3(I3,1X),4(F6.0,1X),2(F5.4,1X),F3.0,1X,2(I2,1X,I2,1X),I1)
340    FORMAT(5X,'UNIT NAME',2X,'LOCATION',6X,' CAPACITY FOR SEASON'
     *,8X,'DFOR',5X,'FOR',4X,'DERAT.',2X,'PRESCH.',2X,'FIRST OUTA',
     *'GE',2X,'SECOND OUTAGE',/,30X,'1',6X,'2',6X,'3',6X,'4',23X,'%',
     *5X,'OR AUTO',5X,'ST',3X,'DUR',6X,'ST',3X,'DUR',/)
       WRITE(JU,10)
       WRITE(JU,15)
       ITAB=ITAB+1
       WRITE(JU,410)ITAB
410    FORMAT(40X,'TABLE',I4,/,40X,'FIRM CONTRACTS',/,40X,'--------',
     *'-----',/)
       WRITE(JU,440)
       ICOUNT=50
       IC=0
       READ(JU1,415)IT
415    FORMAT(I3)
       IF(IT.EQ.0)GO TO 416
       DO 400 I=1,IT
       READ(JU1,420)J1,J2,I1,I2,MW
       IF(IC.NE.ICOUNT)GO TO 430
       WRITE(JU,10)
       WRITE(JU,15)
       WRITE(JU,335)
       WRITE(JU,410)ITAB
       WRITE(JU,440)
       ICOUNT=ICOUNT+50
430    CONTINUE
       WRITE(JU,425)NAMA(J1),NAMA(J2),I1,I2,MW
400    CONTINUE
416    CONTINUE
425    FORMAT(5X,A4,2X,A4,4X,I3,8X,I3,6X,I5)
420    FORMAT(I2,1X,I2,1X,I3,1X,I3,1X,I5)
440    FORMAT(5X,'FROM  TO    BEG DAY  END DAY        MW',/)
       WRITE(JU,10)
       WRITE(JU,15)
       ITAB=ITAB+1
       WRITE(JU,510)ITAB
510    FORMAT(40X,'TABLE',I4,/,40X,'OWNERSHIP DATA',/,40X,'--------',
     *'-----',/)
       WRITE(JU,540)
       WRITE(JU,545)(JX,JX=1,NOAREA)
       ICOUNT=50
       IC=0
       READ(JU1,515)IT
515    FORMAT(I3)
       IF(IT.EQ.0)GO TO 516
       DO 500 I=1,IT
       READ(JU1,520)IU,(C(J),J=1,NOAREA)
       IF(IC.NE.ICOUNT)GO TO 530
       WRITE(JU,10)
       WRITE(JU,15)
       WRITE(JU,335)
       WRITE(JU,510)ITAB
       WRITE(JU,540)
       ICOUNT=ICOUNT+50
530    CONTINUE
       WRITE(JU,525)NAMU(IU),NUMP(IU),(C(J),J=1,NOAREA)
500    CONTINUE
525    FORMAT(5X,A4,A2,5X,15(F5.1,1X))
520    FORMAT(I3,1X,15(F5.1,1X))
540    FORMAT(5X,'UNIT NAME','  PERCENT OWNERSHIP OF AREA',/)
545    FORMAT(16X,9(I1,5X),6(I2,4X))
516    CONTINUE
       WRITE(JU,10)
       WRITE(JU,15)
       ITAB=ITAB+1
       WRITE(JU,610)ITAB
610    FORMAT(40X,'TABLE',I4,/,40X,'LINE DATA',/,40X,'--------',
     *'-',/)
       WRITE(JU,640)
       WRITE(JU,645)
       ICOUNT=50
       IC=0
       READ(JU1,615)IT
615    FORMAT(I2)
       DO 600 I=1,IT
       READ(JU1,620)I1,I2,I3,A1,C1,R1,A2,C2,R2,A3,C3,R3,A4,C4,R4,
     *A5,C5,R5,A6,C6,R6,P1,P2,P3,P4,P5,P6
       IF(IC.NE.ICOUNT)GO TO 630
       WRITE(JU,10)
       WRITE(JU,15)
       WRITE(JU,335)
       WRITE(JU,510)ITAB
       WRITE(JU,540)
       ICOUNT=ICOUNT+50
630    CONTINUE
       WRITE(JU,625)I1,NAMA(I2),NAMA(I3),1,A1,C1,R1,P1
       WRITE(JU,626)2,A2,C2,R2,P2
       WRITE(JU,626)3,A3,C3,R3,P3
       WRITE(JU,626)4,A4,C4,R4,P4
       WRITE(JU,626)5,A5,C5,R5,P5
       WRITE(JU,626)6,A6,C6,R6,P6
600    CONTINUE
625    FORMAT(5X,I2,7X,A4,2X,A4,4X,I2,2X,F9.3,3X,F7.1,6X,F7.1,6X,F7.5)
626    FORMAT(28X,I2,2X,F9.3,3X,F7.1,6X,F7.1,6X,F7.5)
620    FORMAT(3(I2,1X),6(F7.0,1X),/,9(F7.0,1X),/,9(F7.0,1X))
640    FORMAT(5X,'LINE NO',2X,'FROM',2X,'TO  ',2X,'STATE',2X,'ADMITANCE'
     *,2X,'CAPACITY(+)',2X,'CAPACITY(-)',2X,'PROBABILITY',/)
645    FORMAT(34X,'PER UNIT',2X)
       WRITE(JU,10)
       WRITE(JU,15)
       ITAB=ITAB+1
       WRITE(JU,710)ITAB
710    FORMAT(40X,'TABLE',I4,/,40X,'LINE DERATING DATA',/,40X,'---------
     *----------',/)
       WRITE(JU,740)
740    FORMAT(5X,'LINE',5X,'UNITS DERATING THIS LINE',5X,'DERATING FACTO
     *RS',/,43X,'FORWARD BACKWARD',/)
       ICOUNT=50
       IC=0
       READ(JU1,715)IT
715    FORMAT(I3)
       IF(IT.EQ.0)GO TO 716
       DO 700 I=1,IT
       READ(JU1,720)NLL,N1,N2,N3,N4,R1,R2
       IF(N1.EQ.0)N1=1000
       IF(N2.EQ.0)N2=1000
       IF(N3.EQ.0)N3=1000
       IF(N4.EQ.0)N4=1000
       IF(IC.NE.ICOUNT)GO TO 730
       WRITE(JU,10)
       WRITE(JU,15)
       WRITE(JU,335)
       WRITE(JU,710)ITAB
       WRITE(JU,740)
       ICOUNT=ICOUNT+50
730    CONTINUE
       WRITE(JU,725)NLL,NAMU(N1),NUMP(N1),NAMU(N2),NUMP(N2),NAMU(N3),NUM
     *P(N3),NAMU(N4),NUMP(N4),R1,R2
700    CONTINUE
725    FORMAT(5X,I3,5X,4(A4,A2,2X),2(F5.2,1X))
720    FORMAT(I2,1X,4(I3,1X),2(F5.2,1X))
716    CONTINUE
 999   CONTINUE
       RETURN
       END



       SUBROUTINE INPUT(JU)

C THIS SUBROUTINE READS DATA FROM INPUT.DAT AND CREATES APPROPRIATE
C ARRAYS.
C JU IS THE LOGICAL UNIT NUMBER FOR FILE INPUT.DAT
C THE SEQUENCE OF CARDS IN INPUT.DAT IS AS FOLLOWS:
C CARD  TYPE 0 (ONE CARD)
C VARIABLES :JSEED,NLS,IW1,IW2,IW3
C            JSEED SEED FOR RN
C            NLS LOSS SHARING POLICY 0 FOR LOSS SHARING AND 1 FOR
C            NO LOSS SHARING
C            IW1,IW2,IW3  ENDING WEEKS FOR FOUR SEASONS.
C FORMAT : 3X,I7,2X,I1,2X,3(I2,2X)
C CARD  TYPE 1  (ONE CARD)
C VARIABLES : NOAREA
C FORMAT : I2
C CARD TYPE 2 (EQUAL TO NOAREA)
C PKLOAD(J),FU(J),MINRAN(J),MAXRAN(J),INHBT1(J),INHBT2(J),BN(J,4)
C PKLOAD(J):PEAK LOAD IN ARAEA J
C FU(J) : PERCENT LOAD FORECAST UNCERTAINTY IN AREA J
C MINRAN(J) : EARLIEST WEEK OF MAINTENANCE WINDOW IN AREA J
C MAXRAN(J) : LATEST WEEK OF MAINTENANCE WINDOW IN AREA J
C INHBT1(J),INHBT2(J) : MAINTENANCE FORBIDDEN FROM WEEK INHBT1(J)
C TO WEEK INHBT2(J) IN AREA J.
C FORMAT : F5.0,1X,F5.0,1X,4(I2,1X),F7.0
C CARD TYPE 3 (ONE CARD)
C VARIABLE : NUNITS
C FORMAT : I3
C CARD TYPE 4 (EQUAL TO NUNITS)
C VARIABLES :
C (ID(I,K),K=1,3),(RATES(I,K),K=1,4),DFOR,FOR,DERATE(I),ID(I,K),K=4,7
C ID(I,K),K=1,3 : UNIT NUMBER,PLANT NUMBER,AREA OF LOCATION
C RATES : UNIT CAPACITIES IN FOUR SEASONS
C DERATE(I) : PERCENT DERATING
C ID(I,K),K=4,7 : K=4 STARTING WEEK OF FIRST PLANNED OUTGE
C                 K=5 DURATION OF FIRST PLANNED OUTAGE IN WEEKS
C                 K=6 STARTING WEEK OF SECOND PLANNED OUTAGE
C                 K=7 DURATION OF SECOND PLANNED OUTAGE
C                 K=8 1 IF MAINTENANCE IS PRESCHEDULED AND 0 IF
C                     AUTOMATIC BY THE PROGRAM.
C FORMAT :
C (3(I3,1X),4(F6.0,1X),2(F5.4,1X),F3.0,1X,2(I2,1X,I2,1X),I1
C CARD TYPE 5 (ONE CARD)
C VARIABLE : IT IS THE NUMBER OF CARDS DESCRIBING INTERCHANGE IE
C NUMBER OF CARDS OF TYPE 6
C CARD TYPE 6 (EQUAL TO IT )
C VARIABLES : J1,J2,I1,I2,MW
C J1 : SENDING AREA
C J2 : RECEIVING AREA
C I1,I2 : STARTING AND ENDING DAYS OF THE FIRM INTERCHANGE
C MW : MAGNITUDE OF INTERCHANGE IN MW
C CARD TYPE 7 (ONE)
C VARIABLE : IT1 EQUALS THE FOLLOWING CARDS TYPE 8
C FORMAT : I3
C CARD TYPE 8
C VARIABLES :IU,C(J),J=1,15
C IU : UNIT NUMBER
C C(J) : PERCENT OF UNIT OWNED BY AREA J
C FORMAT : I3,1X,15(F4.1,1X)
C CARD TYPE 9 (ONE)
C VARIABLE : NLINES
C CARD TYPE 10 (EQUAL TO NLINES)
C VARIABLES :LP(L,K),K=1,3;BLPA(L,K),K=1,18;PROBL(L,K),K=1,6
C LP(L,K) ; K=1 LINE NUMBER,K=2 STARTING AREA,K=3 ENDING AREA.
C BLPA(L,K) :SIX SETS OF THREE ENTRIES - ADMITANCE,FORWARD AND
C BACKWARD CAPACITIES FOR SIX STATES.
C PROBL(L,K) : PROBABILITIES OF SIX STATES.
C FORMAT :
C (3(I2,1X),6(F7.0,1X),/9(F7.0,1X),/,9(F7.0,1X))
C CARD TYPE 11 (ONE)
C VARIABLES : IT2 IS THE NUMBER OF CARDS OF TYPE 12
C CARD TYPE 12
C VARIABLES : L1,I1,I2,I3,I4,F1,F2
C L1 : LINE NUMBER
C I1,I2,I3,I4 : UNITS CRITICAL TO THIS LINE .IF LESS THAN 4,INSERT 0.
C F1,F2 : FORWARD AND BACKWARD MOD FACTORS.
C THIS IS FOLLOWED BY LOAD DATA FOR THE ARAEAS.FIRST INPUT LOAD DATA FOR
C AREA 1 THEN FOR 2 AND SO ON. EACH LINE READS DATA FOR 12 HOURS IN THE
C FORMAT 12F6.0

      INCLUDE 'NARPERC.txt'
      INCLUDE 'NARPCOM.txt'
	ISH=0
	JU1=13
	JU8=8
	DO 100 I=1,52
      DO 100 J=1,120
100     JPLOUT(I,J)=0

C READ DATA FROM FILE INPUT.DAT AND CREATE THE APPROPRIATE ARRAYS.
	CALL  DATAX(JU)

C READ LOAD DATA FROM INPUT.DAT
C        CALL XLDPU(JU8)
C        CALL XLD(JU8)

	 CALL XLDNEW(JU8)
	 IF(IABORT.NE.0)RETURN

C ADD INTERCHANGES
	CALL XPORTA

C FIND DAILY PEAKS AND HOUR OF DAILY PEAKS
	CALL DPEAK

C FIND WEEKLY PEAKS
	CALL WPEAKF

C FIND HOUR OF POOL DAILY PEAKS
	CALL PLHR

C REMOVE INTERCHANGES AND RESTORE HRLOAD
	CALL XPORTS

C        DO 400 I=1,365
C       WRITE(*,351)(DYLOAD(K,I),K=1,NOAREA),(MAXHR(K,I),K=1,
C     *  NOAREA),MXPLHR(I)
C400     CONTINUE
C351     FORMAT(2X,3(F7.0),3I6,I6)

C CALCULATE THE TOTAL OWNED CAP AND INTERCHANGE AT PEAK
C SUBROUTINE ADDED APRIL 1994-C. SINGH

	CALL RESCA

C PREPARE JPLOUT - TABLE OF PLANNED OUTAGES OF UNITS
	CALL SMAINT

C     WRITE(*,*) ' ** JPLOUT MATRIX BY WEEK **'
C       DO 500 I=1,52
C      WRITE(*,501) I,(JPLOUT(I,J),J=1,20)
C 501 FORMAT(1X,I2,2X,20I4)
C 500    CONTINUE
	RETURN
	END




C
C THIS SUBROUTINE READS ALL DATA EXCEPT THE HOURLY LOAD DATA AND
C CREATES APPROPRIATE ARRAYS.
C
      SUBROUTINE DATAX(JU)
      DIMENSION C(15),JCRITT(100,6),FCR(100,2)
C
C  COMMON STATEMENTS
C
      INCLUDE 'NARPERC.txt'
      INCLUDE 'NARPCOM.txt'
C READ TITLE CARDS
C
C
      READ(JU,1)K
 1    FORMAT(2X,I2)
      DO 2 I=1,K
        READ(JU,3)(AX(I,J),J=1,39)
 2    CONTINUE
 3    FORMAT(2X,39A2)

C
C  READ JSEED,LOSS SHARING POLICY,ENDING WEEKS OF FOUR SEASONS,AND
C  CREATE QTR(3) HOURS OF THE YEAR FOR QUARTER CHANGE.
C
	ISH=0
	JU1=13
	READ(JU,50)JSEED,NLS,IW1,IW2,IW3
50      FORMAT(3X,I7,2X,I1,2X,3(I2,2X))
	QTR(1)=IW1*168+.5
	QTR(2)=IW2*168+.5
	QTR(3)=IW3*168+.5
C
C  READ AREA ANNUAL PEAK LOADS, AREA MAINTENANCE WINDOWS, AND CONSTRAINT
C  ON THE AREA SUM OF FLOWS. CREATE PKLOAD(J), MINRAN(J),MAXRAN(J),
C  INHBT1(J) AND INHBT2(J),
C
      NORR=1
      NFCST=1
      READ(JU,100) NOAREA

      DO 200 J=1,NOAREA
        READ(JU,110)PKLOAD(J),FU(J),MINRAN(J),MAXRAN(J),INHBT1(J),
     *  INHBT2(J),BN(J,4)
        BN(J,5)=BN(J,4)
        SUSTAT(J,1)=PKLOAD(J)
        IF(FU(J).NE.0.)NORR=3
        IF(FU(J).NE.0.)NFCST=5
        BN(J,1)=J
        BN(J,2)=0
        BN(J,3)=0
	    FCTERR(J,3)=1.
	    FCTERR(J,4)=1.-FU(J)/100.
	    FCTERR(J,2)=1.+FU(J)/100.
	    FCTERR(J,1)=1.+FU(J)*2.5/100.
	    FCTERR(J,5)=1.-FU(J)*2.5/100.
200   CONTINUE
100   FORMAT(I2)
110   FORMAT(F8.0,1X,F5.0,1X,4(I2,1X),F8.0)


C
C  CREATE PROBD(5) PROB DIST OF LOAD FORECAST UNCERTAINTY.
C
	PROBD(1)=.067
	PROBD(2)=.242
	PROBD(3)=.382
	PROBD(4)=.242
	PROBD(5)=.067

C
C  READ UNIT DATA AND CREATE
C  DERATE(I)= FRACTION OF UNIT I AVAILABLE AFTER DERATING
C  NOGEN(J): NO. OF GENERATORS IN AREA J.
C  NUNITS: NO. OF GENERATORS IN ALL AREAS.
C  PROB(K,I):CUM. PROB. THAT UNIT I IS IN STATE K. K=1 FULL AVAIL.
C  K=2 DERATE
C  RATES(I,K) SEASONAL RATING OF UNIT I, K=1,4
C  1= JAN,MAR 2=APR,JUNE, 3=JULY,SEPT, 4=OCT,DEC.

      DO 900 J=1,15
         DO 900 I=1,600
 900        CAPOWN(J,I)=0.0

      READ(JU,300) NUNITS
C     WRITE(*,311) NUNITS
C311  FORMAT(' ',' THE NUMBER OF UNITS IS',I4)
300     FORMAT(I3)

      DO 400 J=1,NOAREA
400     NOGEN(J)=0

      DO 500 I=1,NUNITS
        READ(JU,310)(ID(I,K),K=1,3),(RATES(I,K),K=1,4),DFOR,FOR,
     *  DERATE(I), (ID(I,K),K=4,8)
	    J=ID(I,3)
	    J1=ID(I,1)
	    CAPCON(J1)=J
	    CAPOWN(J,J1)=1.
	    NOGEN(J)=NOGEN(J)+1

        PROBG(I,1)=1.0-FOR-DFOR
        PROBG(I,2)=1.-FOR
	    DERATE(I)=1.-DERATE(I)/100.
500   CONTINUE

310   FORMAT(3(I3,1X),4(F6.0,1X),2(F5.4,1X),F3.0,1X,2(I2,1X,
     *  I2,1X),I1)

		  IF(ISH.EQ.0)GO TO 512

	 	     DO 510 I=1,NUNITS
	  WRITE(JU1,511)(ID(I,K),K=1,8),(RATES(I,K),K=1,4),DERATE(I),
     *  PROBG(I,1),PROBG(I,2)
510    CONTINUE

511   FORMAT(2X,8(I3,1X),4(F6.0),F5.1,2(F7.5,1X))

		  DO 210 J=1,NOAREA
	  WRITE(JU1,115)PKLOAD(J),FU(J),MINRAN(J),MAXRAN(J),INHBT1(J)
     *  ,INHBT2(J),BN(J,1),BN(J,4),NOGEN(J)
210 	  CONTINUE

512 	  CONTINUE
115 	  FORMAT(5X,F5.0,1X,F5.2,1X,4(I2,1X),2(F6.0,1X),I3)


C
C  READ CONTRACT INTERCHANGES BETWEEN AREAS AND CREATE :
C  JENT(J1,J2)-- MATRIX INDICATING AREAS WHICH HAVE INTERCHANGE
C  IF 0, NONE, IF POSITIVE, THE POINTER L USED IN INTCH (L,N)
C  INTCH(L,N)- CONTRACT INTERCHANGES BETWEEN AREAS INDICATED BY POINTER
C  L, DAY N.
C
      DO 600 I=1,15
        DO 600 J=1,15
600       JENT(I,J)=0

      DO 650 L=1,60
        DO 650 N=1,365
650       INTCH(L,N)=0

      READ(JU,700) IT
700     FORMAT(I3)

      IP=0
	  IF(IT.EQ.0)GO TO 821

      DO 800 I=1,IT
        READ(JU,815) J1,J2,I1,I2,MW
        IF(JENT(J1,J2) .NE. 0) GO TO 810
          IP=IP+1
          JENT(J1,J2)=IP
          INTCHR(IP,1)=J1
          INTCHR(IP,2)=J2
810     CONTINUE
        L=JENT(J1,J2)
        DO 820 K=I1,I2
820       INTCH(L,K)=MW
800   CONTINUE

815     FORMAT(I2,1X,I2,1X,I3,1X,I3,1X,I5)

		  IF(ISH.EQ.0)GO TO 821

		  DO 816 I=1,15
	  	  WRITE(*,817)(JENT(I,J),J=1,15)
816 	  CONTINUE
817     FORMAT(2X,15I5)

		  DO 818  I=1,365
	 	   WRITE(*,819)(INTCH(L,I),L=1,20)
818 	  CONTINUE

821 	  CONTINUE
819     FORMAT(20I4)


C
C  READ OWNERSHIP OF UNITS AND CREATE CAPOWN(J,I),
C  FRACTION OF UNIT I OWNED BY AREA J.
C
      READ(JU,910) IT1
910     FORMAT(I3)
	  IF(IT1.EQ.0)GO TO 924

      DO 950 I=1,IT1
        READ(JU,920) IU,(C(J),J=1,NOAREA)

C       WRITE(JU1,923)IU,(C(J),J=1,NOAREA)
C923    FORMAT(2X,I3,2X,15(F4.0,1X))

        DO 960 J=1,15
          CAPOWN(J,IU)=C(J)/100.
960     CONTINUE
950   CONTINUE

920     FORMAT(I3,1X,15(F5.1,1X))
	IF(ISH.EQ.0)GO TO 924

	DO 922 I=1,NUNITS
	  WRITE(JU1,921)(CAPOWN(J,I),J=1,NOAREA)
922     CONTINUE

924     CONTINUE
921     FORMAT(2X,15(F5.2))


C
C   READ LINE DATA AND CREATE
C   LINENO(J1,J2) LINE NUMBER BETWEEN AREA J1 AND J2.
C   NLINES   NUMBER OF LINES
C   PROBL(K,L) CUMULATIVE PROBABILITY THAT LINE L IS
C   IN STATE K, K=1,5.
C
      READ(JU,1000) NLINES
1000    FORMAT(I2)

      DO 1100 L=1,NLINES
        READ(JU,1110)(LP(L,K),K=1,3),(BLPA(L,K),K=1,18),
     *  (PROBL(K,L),K=1,6)
        J0= LP(L,1)
        J1=LP(L,2)
        J2=LP(L,3)
        LINENO(J1,J2)=J0
1100  CONTINUE

		  DO 1150 L=1,NLINES
	 	   DO 1160 M=2,6
	   	   PROBL(M,L)=PROBL(M,L)+PROBL(M-1,L)
1160  	  CO NTINUE
1150 	  CONTINUE

	     DO 1151 I=1,NLINES
	  	  DO 1152 J=1,16,3
	     IBIN=BLPA(I,J)
	     IF(IBIN.NE.0)GO TO 1153
	      BLPA(I,J)=-.5
	      BLPA(I,J+1)=1.
	      BLPA(I,J+2)=1.
 1153   CONTINUE
 1152 	  CONTINUE
 1151 	  CONTINUE

1110    FORMAT(3(I2,1X),6(F7.0,1X),/9(F7.0,1X),/,9(F7.0,1X))
1111    FORMAT(3(I2,1X),6(F7.0,1X),/9(F7.0,1X),/,9(F7.2,1X))

		  IF(ISH.EQ.0)GO TO 1162

		  DO 1161 I=1,NLINES
	  WRITE(JU1,1111)(LP(I,K),K=1,3),(BLPA(I,K),K=1,18),
     *  (PROBL(K,I),K=1,6)
1161 	  CONTINUE

1162 	  CONTINUE
C1112   FORMAT(2X,3I4,6(F6.4,2X))


C
C READ CRITICAL UNIT DATA AND CREATE JCRIT(L) -
C POSITION 1           LINE NUMBER
C POSITION 2           N =  NUMBER OF CRITICAL UNITS EFFECTING THIS LINE
C POSITIONS 3 TO 2+N   CRITICAL UNITS EFFECTING THIS LINE
C POSITION  3+N        THRESHOLD CAPACITY
C POSITIONS 4+N,5+N    MOD FACTORS*100
C THIS SEQUENCE IS REPEATED FOR OTHER LINES.
C
		  READ(JU,1200)IT2
1200    FORMAT(I3)
1210    FORMAT(I2,1X,4(I3,1X),2(F5.0,1X))
		   MXCRIT=IT2

		  IF(IT2.EQ.0)GO TO 1271

		  DO 1251 I=1,IT2
	 	   READ(JU,1210)(JCRITT(I,J),J=1,5),FCR(I,1),FCR(I,2)
	  	  JCRITT(I,6)=0
	  	  DO 1255 J1=2,5
	    IF(JCRITT(I,J1).EQ.0)GO TO 1256
	      JCRITT(I,6)=JCRITT(I,6)+1
 1256  	   CONTINUE
 1255 	  CONTINUE
1251	   CONTINUE

		  J=1
1257    IND=500
		  DO 1252 I=1,IT2
	  	  IF((JCRITT(I,1)).LT.IND)IND=JCRITT(I,1)
1252 	  CONTINUE

		  IF(IND.EQ.500)GO TO 1250

		  IND1=0
		  IMAX=500

		  DO 1253 I=1,IT2
	  	  IF(IND.NE.(JCRITT(I,1)))GO TO 1254
	  	  IF(JCRITT(I,6).GE.IMAX)GO TO 1254
	  	  IND1=I
	 	   IMAX=JCRITT(I,6)
 1254 CONTINUE
 1253 CONTINUE

		  L1=JCRITT(IND1,1)
		  I1=JCRITT(IND1,2)
		  I2=JCRITT(IND1,3)
		  I3=JCRITT(IND1,4)
		  I4=JCRITT(IND1,5)
		  F1=FCR(IND1,1)
		  F2=FCR(IND1,2)
		  IC=0
		  JCRIT(J)=L1
		  JCRIT(J+2)=I1
		  JCRIT(J+1)=JCRIT(J+1)+1
		  IF(I2.EQ.0)GO TO 1260
		  JCRIT(J+3)=I2
		  JCRIT(J+1)=JCRIT(J+1)+1
		  IF(I3.EQ.0)GO TO 1260
		  JCRIT(J+4)=I3
		  JCRIT(J+1)=JCRIT(J+1)+1
		  IF(I4.EQ.0)GO TO 1260
		  JCRIT(J+5)=I4
		  JCRIT(J+1)=JCRIT(J+1)+1
1260    N=JCRIT(J+1)+J+2
		  JCRIT(N)=IC
		  JCRIT(N+1)=F1*100 + 0.5
		  JCRIT(N+2)=F2*100 + 0.5
		  JCRITT(IND1,1)=500
		  J=N+3
		   GO TO 1257

1250    CONTINUE
		  JCRIT(J)=-1

		  IF(ISH.EQ.0)GO TO 1271
		  DO 1270 I=1,J
	  	  WRITE(JU1,700)JCRIT(I)
1270 	  CONTINUE

1271 	  CONTINUE
     	  RETURN
    	   END



C
C THIS SUBROUTINE FINDS DAILY PEAKS
C
      SUBROUTINE DPEAK
      INCLUDE 'NARPERC.txt'
	  JC=0
200   JC=JC+1
	  IF(JC.GT.NOAREA)GO TO 150
	  DO 100 I=1,365
	    XMAX=0.
	    DO 110 J=1,24
	      J1=(I-1)*24+J
	      IF(HRLOAD(JC,J1).LE.XMAX)GO TO 110
	      XMAX=HRLOAD(JC,J1)
	      MAXHR(JC,I)=J1
110     CONTINUE
	    DYLOAD(JC,I)=XMAX
100   CONTINUE
	  GO TO 200
150   CONTINUE

	  DO 250 I=1,NOAREA
	    XMAX=0.
	     DO 260 J=1,365
	      IF(DYLOAD(I,J).LE.XMAX)GO TO 260
	      XMAX=DYLOAD(I,J)
	      MAXDAY(I)=J
 260    CONTINUE
 250  CONTINUE
		    RETURN
		    END




C
C THIS SUBROUTINE FINDS WEEKLY PEAKS
C

      SUBROUTINE WPEAKF
      INCLUDE 'NARPERC.txt'
      JC=0
 100  JC=JC+1
      IF(JC.GT.NOAREA)GO TO 500
C
C        FIND WEEKLY PEAKS
C
      DO 120 I=1,52
        XMAX=0.
        JW=7

        IF(I.EQ.52)JW=8

        DO 130 J=1,JW
          J1=(I-1)*7+J
          IF(DYLOAD(JC,J1).GT.XMAX) XMAX=DYLOAD(JC,J1)
  130   CONTINUE

        WPEAK(JC,I)=XMAX
  120 CONTINUE

      GO TO 100
 500  RETURN
      END



C
C THIS SUBROUTINE FINDS THE POOL PEAK HOURS
C
      SUBROUTINE PLHR
     INCLUDE 'NARPERC.txt'
	 DO 100 I=1,365
	   XMAX=0.
	   DO 110 J=1,24
	     J1=(I-1)*24+J
	     HRL=0.

	     DO 115 K=1,NOAREA
115        HRL=HRLOAD(K,J1)+HRL

	     IF(HRL.LE.XMAX)GO TO 110

	     XMAX=HRL
	     MXPLHR(I)=J1
110   CONTINUE
100 	  CONTINUE
		    RETURN
		    END




		  SUBROUTINE XLDNEW(JU)
    	  INTEGER IRLOAD(12)
    	  INCLUDE 'NARPERC.txt'
    	  INCLUDE 'NARPCOM.txt'

		  IAREA=0
		  LLINE=0

		  DO 210 I=1,15
	  	  DO 215 J=1,8760
	   	   HRLOAD(I,J)=0.
 215  CONTINUE
 210  CONTINUE

	WRITE(*,205)
 205    FORMAT(5X,'LOAD DATA FOR FOLLOWING AREAS HAS BEEN READ',/)
 220    READ(JU,350,END=550)NCAR,NAAREA
	LLINE=LLINE+1
	WRITE(*,353)NCAR,NAAREA
C       BACKSPACE 1
	IF(NCAR.NE.SYIAR)GO TO 220
	ITAG=0
	DO 230 K4=1,NOAREA
	IF(NAAREA.EQ.NAMA(K4))ITAG=K4
 230    CONTINUE
	IF(ITAG.GT.0)GO TO 240
	WRITE(*,235)LLINE
 235    FORMAT(15X,'AREA NAME DOESNOT MATCH',/,
     *15X,'PROGRAM ABORTED',/,
     *15X,'ERROR OCCURRED IN LEEI IN LINE ',I5,/)
	IABORT=1
	RETURN

 240    CONTINUE
	IAREA=IAREA+1
	I=ITAG
C       WRITE(*,*)I,IAREA
	LLINE=LLINE+1
	READ(JU,351)IY1,IY2
	IF((IY1.EQ.99).AND.(IY2.EQ.99))GO TO 220
	IF((IY1.NE.1).AND.(IY2.NE.1))GO TO 560
	BACKSPACE JU
	LLINE=LLINE-1

 351    FORMAT(2I2)

	DO 300 J=1,730
	  K1=(J-1)*12+1
	  K2=K1+11
	  LLINE=LLINE+1
	  READ(JU,352,ERR=510,END=510)IY1,IY2,IY3,(IRLOAD(K),K=1,12)
	  K3=0
	  DO 310 K=K1,K2
	    K3=K3+1
	    HRLOAD(I,K)=IRLOAD(K3)
310   CONTINUE
300 	  CONTINUE

		  GO TO 220

352    FORMAT(3I2,14X,12I5)
350     FORMAT(A4,1X,A4)
353     FORMAT(5X,A4,1X,A4)
710    CONTINUE

		  DO 400 I=1,NOAREA
	  	  XMAX=0.

	  	  DO 500 J=1,8760
	    IF(HRLOAD(I,J).GT.XMAX)XMAX=HRLOAD(I,J)
500   CONTINUE

	  IF(XMAX.LT..001) XMAX=.001

	  DO 600 J=1,8760
	    HRLOAD(I,J)=HRLOAD(I,J)/XMAX

600   CONTINUE
400 	  CONTINUE

		  DO 700 I=1,NOAREA
	  DO 800 J=1,8760
	    HRLOAD(I,J)=HRLOAD(I,J)*PKLOAD(I)
800   CONTINUE
700 	  CONTINUE

		  RETURN

 510    CONTINUE
		  WRITE(*,520)LLINE
 520    FORMAT(15X,'UNEXPECTED TYPE OR END OF DATA',/,
     *  15X,'PROGRAM ABORTED',/,
     *  15X,'ERROR OCCURRED IN LEEI IN LINE ',I5,/)
		  IABORT=1

	 	  RETURN

 550    CONTINUE
		  IF(IAREA.EQ.NOAREA)GO TO 710
		  WRITE(*,555)LLINE,IAREA
 555    FORMAT(15X,'UNEXPECTED END OF FILE WHILE SEARCHING AREA NAME',
     *  /,15X,'PROGRAM ABORTED',/,15X,
     *  'ERROR OCCURED IN LEEI IN LINE ',I5,/,
     *  15X,'NUMBER OF AREAS ALREADY IDENTIFIED ',I5,/)
		  IABORT=1
		  RETURN

 560    CONTINUE
		  WRITE(*,565)NAAREA,LLINE
 565    FORMAT(15X,'BEGINNING OF ',A4,' DATA NOT JAN 1',
     *  /,'PROGRAM ABORTED',/,
     *  15X,'ERROR OCCURRED IN LEEI IN LINE ',I5,/)
		  IABORT=1
		  RETURN
		  END




		  SUBROUTINE SMAINT
    	  INCLUDE 'NARPERC.txt'
		  IA=0
 25 	  IA=IA+1
		  IF(IA.GT.NOAREA)GO TO 115
		  CALL PMSC(IA)
		  GO TO 25
 115 	  CONTINUE

		  DO 50 I=1,52
	  	  DO 50 J=1,120
50      	  JPLOUT(I,J)=0

		  DO 100 I=1,NUNITS
	  	  IF(ID(I,4).EQ.0 ) GO TO 200
     *         J1=ID(I,4)
	 	   J2=J1+ID(I,5)-1
	  	  IF(J2.GT.52)J2=52
	 	   DO 110 J=J1,J2
	   	   K=JPLOUT(J,1)+1
	    	  JPLOUT(J,1)=K
110    	   JPLOUT(J,K+1)=ID(I,1)

200   CONTINUE

	 	   IF(ID(I,6).EQ.0) GO TO 100
	  	  J1=ID(I,6)
	 	   J2=J1+ID(I,7)-1
	  	  IF(J2.GT.52)J2=52
	  	  DO 210 J=J1,J2
	   	   K=JPLOUT(J,1)+1
	   	   JPLOUT(J,1)=K
210     JPLOUT(J,K+1)=ID(I,1)

100	   CONTINUE

		  RETURN
		  END


		  SUBROUTINE XPORTA
      INCLUDE 'NARPERC.txt'
		  DO 100 J1=1,NOAREA
	  DO 200 J2=1,NOAREA
	    IF(JENT(J1,J2).EQ.0)GO TO 200
	    L=JENT(J1,J2)
	    DO 300 K=1,365
	      IF(INTCH(L,K).EQ.0)GO TO 300
	      K1=(K-1)*24+1
	      K2=K1+23
	      DO 350 K3=K1,K2
	        HRLOAD(J1,K3)=HRLOAD(J1,K3)+INTCH(L,K)
	        HRLOAD(J2,K3)=HRLOAD(J2,K3)-INTCH(L,K)
350       CONTINUE
300     CONTINUE
200   	  CONTINUE
100 	  CONTINUE
		  	  RETURN
		    END



	SUBROUTINE XPORTS
    INCLUDE 'NARPERC.txt'
	DO 100 J1=1,NOAREA
	  DO 200 J2=1,NOAREA
        IF(JENT(J1,J2).EQ.0)GO TO 200
		L=JENT(J1,J2)
		DO 300 K=1,365
		  IF(INTCH(L,K).EQ.0)GO TO 300
		  K1=(K-1)*24+1
		  K2=K1+23
		  DO 350 K3=K1,K2
			HRLOAD(J1,K3)=HRLOAD(J1,K3)-INTCH(L,K)
			HRLOAD(J2,K3)=HRLOAD(J2,K3)+INTCH(L,K)
350       CONTINUE
300     CONTINUE
200   CONTINUE
100 	  CONTINUE
		    RETURN
		    END




		    SUBROUTINE RESCA
		    INCLUDE 'NARPERC.txt'
		     DO 100 I=1,NOAREA
		    IDAY=MAXDAY(I)
		    IHR=IDAY*24
	 	   IPER=1
	 	   IF(IHR.LE.QTR(1))GO TO 150
	 	   IPER=2
	 	   IF(IHR.LE.QTR(2))GO TO 150
	 	   IPER=3
	 	   IF(IHR.LE.QTR(3))GO TO 150
	 	   IPER=4

 150  CONTINUE

	  DO 200 J=1,NUNITS
	    SUSTAT(I,2)=SUSTAT(I,2)+CAPOWN(I,J)*RATES(J,IPER)
 200  CONTINUE
C       WRITE(*,*)SUSTAT(I,2),SUSTAT(I,1)

	  DO 250 J1=1,NOAREA

	    IF(JENT(I,J1).EQ.0)GO TO 260
	      IP=JENT(I,J1)
	      SUSTAT(I,2)=SUSTAT(I,2)-INTCH(IP,IDAY)
C         WRITE(*,*)SUSTAT(I,2),SUSTAT(I,1)
 260    CONTINUE

	    IF(JENT(J1,I).EQ.0)GO TO 270
C         WRITE(*,*)SUSTAT(I,2),SUSTAT(I,1)
	      IP=JENT(J1,I)
	      SUSTAT(I,2)=SUSTAT(I,2)+INTCH(IP,IDAY)
 270    CONTINUE

C       WRITE(*,*)SUSTAT(I,2),SUSTAT(I,1)
 250  CONTINUE

C     WRITE(*,*)SUSTAT(I,2),SUSTAT(I,1)
 100 	  CONTINUE

		  	  RETURN
		  	  END




C***********************************************************************
C
C        THIS SUBROUTINE IS FOR AUTOMATIC SCHEDULING OF PLANNED
C        MAINTENANCE
C
C***********************************************************************
C
      SUBROUTINE PMSC(IA)
C
C               SCHLOS   SCHEDULED LOSS DUE TO MAINTENANCE
C               DURLOS   LOSS DUE TO MAINTENANCE
C               SDLOLP   SCHEDULED LOSS X DURATION X PROB. OF FORCED OUT
C               LOCREW   LOCATION & CREW
C               MINRAN   MINIMUM RANGE
C               MAXRAN   MAXIMUN RANGE
C               LOAD     WEEKLY PEAK LOAD
C               WEEKS    ASSIGNED TIME FOR MAINTENANCE
C               CULOLP   CUMMLOLP
C               INHBT1   INHBTED PERIOD STARTS
C               INHBT2   INHBTED PERIOD ENDED
C
C
      INCLUDE 'NARPERC.txt'
      DOUBLE PRECISION PA
      DIMENSION P(500),NPSU(500)
      INTEGER CAPLOS(500),EFLOAD(52)
      INTEGER LOAD(52),NO(500),
     *RESERV(52),G,SCHLOS(500),DURLOS(500,2)
      CHARACTER*2 WEEKS(52),A,B,AB
      INTEGER MWTOT(200),MWEEK(500) ,INDEX(500,2),INDEX1(500),MWI(500)
      INTEGER  LOCREW(500),CHECK(150,52)
      DIMENSION KA(5000),PA(5000),ID1(500,8),RD(500,3),IDT(500
     *,8),RDT(500,3)
      INTEGER  DURLO(500,2),LOCRE(500),CAPLO(500),NR0(500)
C  ADDED NEXT THREE LINES TO ELIMINATE COMPILER ERRORS ... GENE P. 1/26/89
      COMMON /KAPA/KA,PA
      INCLUDE 'NARPCOM.txt'
      DATA A/'AA'/,B/'..'/,AB/'PP'/
C
C      N IS THE HIGHEST PLANT NUMBER
C      M IS THE TOTAL NUMBER OF GENERATING UNITS .
C
      JU1=16
C  ADDED TO DEFINE WRITE TO SCREEN IN PMSC ... GENE P. 1/26/89
      JU=1
      ISHORT=0
      ITC=ITAB
      INUO=500
      INPO=150
      N=1

      DO 1100 I=1,INUO
        INDEX(I,1)=0
        INDEX(I,2)=0
        INDEX1(I)=0
        P(I)=1.0
1100  CONTINUE

	NGU=0
	DO 13 I=1,NUNITS
	  IF(ID(I,3).NE.IA)GO TO 13

	  NGU=NGU+1
	  IF(ID(I,2).GT.N) N=ID(I,2)
	  NR0(NGU)=ID(I,1)
	  ID1(NGU,1)=NGU

	  DO 15 J=2,8
 15     ID1(NGU,J)=ID(I,J)

	  NU1=RATES(I,1)
	  NU2=RATES(I,2)
	  NU3=RATES(I,3)
	  NU4=RATES(I,4)
	  CAPLOS(NGU)=MAX0(NU1,NU2,NU3,NU4)
C       SUSTAT(IA,2)=SUSTAT(IA,2)+CAPLOS(NGU)

	  RD(NGU,2)=1.-PROBG(I,2)
	  RD(NGU,1)=PROBG(I,2)-PROBG(I,1)
	  RD(NGU,3)=1-DERATE(I)
C        WRITE(JU1,*)NGU,CAPLOS(NGU)
 13     CONTINUE

      IF(NGU.EQ.0) GO TO 199
      DO 1150 I=1,NGU
        DURLOS(I,1)=ID1(I,5)
        DURLOS(I,2)=ID1(I,7)
        ID1(I,4)=ID1(I,4)*168 - 168
        ID1(I,5)=ID1(I,5)*168
        ID1(I,6)=ID1(I,6)*168 - 168
        ID1(I,7)=ID1(I,7)*168
        LOCREW(I)=ID1(I,2)
1150  CONTINUE

C
C        READ :
C
C          G       APPROXIMATELY PEAK LOAD . THIS NUMBER IS USED TO
C                  ENSURE THAT LOAD PLUS CAPACITY ON PLANNED MAINTENANCE
C                  DOES NOT EXCEED THIS VALUE DURING ANY WEEK OF THE YEA
C          CAP1,
C          CAP2    THESE VALUES OF CAPACITY ARE USED TO COMPUTE THE
C                  SLOPE OF THE CAPACITY OUTAGE PROBABILITY TABLE
C          IOPT    0 IF DEPARTURE RATES ARE USED
C                  1 IF DFOR AND FOR ARE USED
C                 >1 IF DAFOR IS USED
C
      CAP=0.
      DO 1110 I=1,NGU
        CAP=CAP+CAPLOS(I)
 1110 CONTINUE

C      WRITE(JU1,*)CAP
      CAP1=CAP/20
      CAP2=CAP/5
      G=PKLOAD(IA)
      M=NGU
      I1=0
C
C        ARRANGE THE SEQUENCE OF UNITS SUCH THAT FOR A GIVEN PLANT
C        THE UNITS ARE ARRANGED IN THE DESCENDING ORDER OF
C        (CAPACITY*PLANNED MAINTENANCE DURATION)
C
      DO 500 I=1,N
515     IC=0
        CMAX=-5.0

        DO 510 J=1,M
          IF(ID1(J,2).NE.I)GO TO 510
          IC=1
          IDUR1=DURLOS(J,1)
          IDUR2=DURLOS(J,2)
          IDUR=MAX0(IDUR1,IDUR2)
          CDUR=CAPLOS(J)*IDUR
          IF(CDUR.GT.CMAX)JT=J
          IF(CDUR.GT.CMAX)CMAX=CDUR
510     CONTINUE

        IF(IC.EQ.0)GO TO 500

        I1=I1+1

        DO 520 J=1,8
520       IDT(I1,J)=ID1(JT,J)

        ID1(JT,2)=0
        DO 530 J=1,3
530       RDT(I1,J)=RD(JT,J)

        CAPLO(I1)=CAPLOS(JT)
        DURLO(I1,1)=DURLOS(JT,1)
        DURLO(I1,2)=DURLOS(JT,2)
        LOCRE(I1)=LOCREW(JT)
        GO TO 515

500   CONTINUE

      DO 640 I1=1,M
        DO 620 J=1,8
620       ID1(I1,J)=IDT(I1,J)
        DO 630 J=1,3
630       RD(I1,J)=RDT(I1,J)

        CAPLOS(I1)=CAPLO(I1)
        DURLOS(I1,1)=DURLO(I1,1)
        DURLOS(I1,2)=DURLO(I1,2)
        LOCREW(I1)=LOCRE(I1)
640   CONTINUE

      ID1(M+1,1)=2000

C
C        CALL SUBROUTINE PMLOLP TO CREATE CAPACITY OUTAGE PROBABILITY
C        TABLE
C
C  KA AND PA PASSED THROUGH COMMON BLOCK .... GENE P. 1/26/89
      CALL PMLOLP(CAPLOS,RD,NGU,P,NGS)

C
C        CALCULATE M FACTOR ,IE, THE SLOPE OF CAPACITY OUTAGE PROBABILIT
C        TABLE
C
C      DO 641 I=1,M
C        WRITE(JU1,*)CAPLOS(I),RD(I,1),RD(I,2),LOCREW(I),DURLOS(I,1),
C    *DURLOS(I,2),ID1(I,5),ID1(I,7),MAXRAN(IA),MINRAN(IA),INHBT1(IA),
C    *INHBT2(IA),ID1(I,8)
C 641  CONTINUE
C     DO 301 IX=1,NGS
C       WRITE(JU1,*)KA(IX),PA(IX)
C301  CONTINUE

      DO 400 I=1,NGS
        IF(KA(I).GE.CAP1)GO TO 410
 400  CONTINUE

 410  UL=PA(I)
      CAP1=KA(I)
      DO 420 I=1,NGS
        IF(KA(I).GE.CAP2)GO TO 430
 420  CONTINUE

 430  XLL=PA(I)
      CAP2=KA(I)
C     WRITE(JU1,*)CAP1,CAP2,XLL,UL
      XM=5000
      IF(XLL.LT..0000001)GO TO 431
      DENOM=ALOG(UL/XLL)
      IF(DENOM.LT..0000001)GO TO 431
      XM=(CAP2-CAP1)/DENOM
 431  CONTINUE
C     WRITE(JU1,4)XM
C4    FORMAT(10X,'M FACTOR =',F6.2,//)

C
C      READ SCHEDULED LOSS, DURATION LOSS, PREVIOUS MAINT.
C      WEEKS, MINIMUM RANGE, MAX. RANGE, WEEKLY PREDICTED MAX. LOAD
C      INHBTED PERIODS.
C      TO SAVE COMPUTING TIME THE DATA IS ARRANGED BY MAINTENANCE-
C      CREW UNITS. ALL GENERATING UNITS IN A PARTICULAR LOCATION-CREW
C      IS ARRANGED IN SEQUENCE BASED ON THE LARGEST SCHEDULE MAINTENANCE
C      X DURATION
C
C
      DO 3 I=1,52
   3    WEEKS(I)=B
C
      DO 39 I=1,52
        DO 39 II=1,INPO
   39     CHECK(II,I)=0.
C
      DO 7 I=1,52
        LOAD(I)=WPEAK(IA,I)
7     CONTINUE

	  DO 6 I=1,52
        EFLOAD(I)=LOAD(I)
    6 CONTINUE
C
      DO 5 I=1,N
        MWTOT(I)=0
        DO 5 J=1,M
    5     MWEEK(J)=0

      IF(IREPM.EQ.0)GO TO 61
      WRITE(JU1,178)
 178  FORMAT('1',10(/),5X,' WEEKLY PEAK LOAD BEFORE MAINTENANCE SCHEDULI
     *NG',/)
      WRITE(JU1,290)(LOAD(I),I=1,52)
 61   CONTINUE

      NPS=0
      DO 436 I=1,M
        IF(ID1(I,8).EQ.0)GO TO 436
        NPS=NPS+1
        NPSU(NPS)=ID1(I,1)
 436  CONTINUE

      I=0
      LTEM=0

      DO 52 J=1,M
        R=(1-P(J)+P(J)*EXP(CAPLOS(J)/XM  ))
        SCHLOS(J)=CAPLOS(J)-XM *ALOG(R)
        IF(SCHLOS(J).LE.0)SCHLOS(J)=CAPLOS(J)

C       WRITE(JU1,*)SCHLOS(J)
      IF(LTEM.EQ.LOCREW(J)) GO TO 10
      I=I+1
      LTEM=LOCREW(J)
      MWTOT(I)=0
      MWI(I)=LOCREW(J)
10    CONTINUE

C      WRITE(JU,444)NPS
C444   FORMAT(10X,'NPS=',I3)

      IF(NPS.EQ.0) GO TO 11
      DO 450 II=1,NPS
        IN1=NPSU(II)
        IF(ID1(J,1).EQ.IN1)GO TO 460
450   CONTINUE
      GO TO 11
 460  CONTINUE

      IF(ID1(J,4).EQ.0.AND.ID1(J,5).EQ.0)GO TO 461
C      WRITE(JU,*)ID(J,5),DURLOS(J,1)
        M1=ID1(J,4)/168+1
        M2=M1+DURLOS(J,1)-1
        IPN=I

        IF(M2.GT.52)M2=52

        IF(M2.EQ.0)GO TO 471

	      DO 470 J2=M1,M2
            CHECK(IPN,J2)=LOCREW(J)
            EFLOAD(J2)=EFLOAD(J2)+SCHLOS(J)
            LOAD(J2)=LOAD(J2)+CAPLOS(J)
470       CONTINUE

          INDEX(J,1)=M1
 461  CONTINUE
        INDEX1(J)=1
 471  CONTINUE

      IF(ID1(J,6).EQ.0.AND.ID1(J,7).EQ.0)GO TO 462
      M3=ID1(J,6)/168+1
      M4=M3+DURLOS(J,2)-1
      IF(M4.GT.52)M4=52
      IF(M4.EQ.0)GO TO 462
        DO 475 J2=M3,M4
          CHECK(IPN,J2)=LOCREW(J)
          EFLOAD(J2)=EFLOAD(J2)+SCHLOS(J)
          LOAD(J2)=LOAD(J2)+CAPLOS(J)
 475    CONTINUE
        INDEX(J,2)=M3

 462  CONTINUE
      GO TO 52

11    CONTINUE

      MWEEK(J)=SCHLOS(J)*(DURLOS(J,1)+DURLOS(J,2))
      MWTOT(I)=MWTOT(I)+MWEEK(J)
   52 CONTINUE
C
C
      ITC=ITC+1
      IF(IREPM.EQ.0)GO TO 62
      WRITE(JU1,323)IA
 323  FORMAT('1',50X,' PLANNED MAINTENANCE SCHEDULE FOR AREA',I3/)
      WRITE(JU1,523)
 523  FORMAT(51X,32('-'))
      WRITE(JU1,190)ITC
  190 FORMAT(63X,'TABLE',I4,//)
      WRITE(JU1,801)
  801 FORMAT(2X,123('-'))
      WRITE(JU1,802)
  802 FORMAT(2X,'|',6X,'|',6X,'|',107X,'|')
      WRITE(JU1,803)
  803 FORMAT(2X,'|',6X,'|',6X,'|',50X,'MONTHS',51X,'|')
      WRITE(JU1,802)
      WRITE(JU1,811)
  811 FORMAT(2X,'|      |      |',107('-'),'|')
      WRITE(JU1,804)
  804 FORMAT(2X,'|',6X,'|',6X,'|',4(26X,'|'))
      WRITE(JU1,805)
  805 FORMAT(2X,'| UNIT | CAP  |',2X,'JAN',6X,'FEB',6X,'MAR',3X,
     *'|',2X,'APR',6X,'MAY',6X,'JUN',3X,'|',2X,'JUL',6X,'AUG',6X,
     *'SEP',3X,'|',2X,'OCT',6X,'NOV',6X,'DEC',3X,'|')
      WRITE(JU1,806)
  806 FORMAT(2X,'| NAME | (MW) |',4(26X,'|'))
      WRITE(JU1,807)
  807 FORMAT(2X,'|',6X,'|',6X,'| 1 2 3 4 5 6 7 8 9 0 1 2 3','|',
     *' 4 5 6 7 8 9 0 1 2 3 4 5 6','|',' 7 8 9 0 1 2 3 4 5 6 7 8 9',
     *'|',' 0 1 2 3 4 5 6 7 8 9 0 1 2','|')
  809 FORMAT(2X,'|',6X,'|',6X,'|                   1      ','|',
     *'             2            ','|','       3                  ',
     *'|',' 4                   5    ','|')
      WRITE(JU1,809)
      WRITE(JU1,801)
      WRITE(JU1,804)

 62   CONTINUE
C
C
C      SCHEDULE FIRST THE PLANT WITH THE LARGEST TOTAL
C      SCHEDULE MW LOSS X DURATION .
C      FIND THE LARGEST MWTOT(I)
C
C
      I=N
      DO 89 LM=1,N
        MAX1=0

        DO 14 K=1,N
          IF(MAX1.GE.MWTOT(K))GO TO 14
            MAX1=MWTOT(K)
            I=K
   14   CONTINUE
C
C       CREATE MWEEK(J)
C
        K1=MWI(I)

        DO 300 K2=1,INUO
 300      MWEEK(K2)=0

		DO 310 K2=1,M
          IF(ID1(K2,2).NE.K1)GO TO 310
            MWEEK(K2)=SCHLOS(K2)*(DURLOS(K2,1)+DURLOS(K2,2))
 310    CONTINUE

C       WRITE(JU1,*)N,M,I
C       WRITE(JU1,*)(MWEEK(K4),K4=1,M)
C       CLEAR THE MWTOT(I), SINCE NO LONGER REQUIRED .

        IF(MWTOT(I).EQ.0)GO TO 89

		MWTOT(I)=0

C      SET THE INDEX FOR MWEEK(J), J=N1,N2
        N1=1
        N2=0
        DO 16 J=1,M
          IF(MWEEK(J).NE.0) N2=J
          IF(MWEEK(J).EQ.0.AND.N2.EQ.0) N1=N1+1
   16   CONTINUE

C      SCHEDULE ALL GENERATORS WITHIN THE SAME PLANT
   18 CONTINUE

      DO 30 JA=N1,N2
        J=JA
        J4=JA

C
C      SET INDEX FOR SCHEDULING WITHIN THE FLEXIBLE MAINTENANCE
C      PERIOD, BETWEEN MIN. AND MAX. RANGE .

      IF(MWEEK(JA).EQ.0)GO TO 30

 191  MT=0
      IMT=0
      I5=2
      IF(ID1(J,5).GT.ID1(J,7))I5=1
      IF(ID1(J,5).EQ.0.OR.ID1(J,7).EQ.0)I5=0
      DURL=DURLOS(J,1)
      IF(DURLOS(J,2).GT.DURL)DURL=DURLOS(J,2)
      MAXR=MAXRAN(IA)
      MINR=MINRAN(IA)

      IF(I5.EQ.0) GO TO 19

        IF(I5.EQ.2) GO TO 41
          MAXRAN(IA)=INHBT1(IA)
          GO TO 42
 41     CONTINUE
          MINRAN(IA)=INHBT2(IA)
 42     CONTINUE

   19 I1=MINRAN(IA)

      IM=DURL-1
      IT=MAXRAN(IA)-IM
      IX=53-DURL
      IF(I1.LE.0)I1=1
      IF(IX.GT.IT)IX=IT
      ITE=10**6
      IC=0

      DO 22 J1=I1,IX
        IR=SCHLOS(J)
        IS=J1+IM
        IF(J1.GE.(INHBT1(IA)-IM).AND.IS.LE.(INHBT2(IA)+IM))GO TO 22
        IF(LOCREW(J).EQ.CHECK(I,J1).OR.LOCREW(J).EQ.CHECK(I,IS))GO TO 22
        DO 21 L=J1,IS
          IF((IR+EFLOAD(L)).GE.G)GO TO 22
   21   CONTINUE

        DO 20 L=J1,IS
          IC=1
   20     IR=IR+EFLOAD(L)

        IF(ITE.LT.IR)GO TO 22
        ITE=IR
        M1=J1
   22 CONTINUE

      MAXRAN(IA)=MAXR
      MINRAN(IA)=MINR

      IF(I5.EQ.0)GO TO 43
        I6=I5
        IC=0
        MT=M1
        IMT=IM
        DURL=DURLOS(J,1)
        IF(DURLOS(J,2).LE.DURL)DURL=DURLOS(J,2)
        IF(I5.EQ.2)GO TO 45
          MINRAN(IA)=INHBT2(IA)
          GO TO 46
 45     CONTINUE
          MAXRAN(IA)=INHBT1(IA)
 46     CONTINUE

        I5=0
        GO TO 19

 43   CONTINUE

C
C      IC IS THE INDEX TO CHECK WHETHER THE SCHEDULE IS FEASIBLE .
C
      IF(IC.NE.0)GO TO 27
   24 J4=J4-1
      IF(J4.GT.0)GO TO 25
      IU0=ID1(JA,1)
      IU0=NR0(IU0)
      IBG1N=ID1(JA,4)/168+1
      IBG2N=ID1(JA,6)/168+1
      ID1R=ID1(JA,5)/168
      ID2R=ID1(JA,7)/168
      WRITE(JU1,100)NAMU(IU0),NUMP(IU0),IBG1N,ID1R,IBG2N,ID2R
  100 FORMAT(10X,'SCHEDULE FOR  GENERATOR **',A4,A2,
     *'**  IS NOT FEASIBLE',/10X,'THIS UNIT IS SCHEDULED WITH PRESPECIFI
     *ED PARAMETERS AS FOLLOWS:',/,10X,'BEGINING FIRST OUTAGE =',I2,
     *'  DURATION =',I2,/,10X,'BEGINING SECOND OUTAGE =',I2,'  DURATION
     * =',I2)

      MWEEK(JA)=0
      INDEX(J,1)=0
      INDEX(J,2)=0
      GO TO 18
C
C      CANCEL THE PREVIOUS SCHEDULED GENERATOR, TRY TO SCHEDULE THE
C      NON-FEASIBLE ONE. INTERCHANGE ORDER OF THE SCHEDULING.
C
   25 IF(MWEEK(J4).EQ.0)GO TO 24
      IF(INDEX(J4,1).EQ.0.AND.INDEX(J4,2).EQ.0)GO TO 24

      IF(INDEX(J4,1).EQ.0)GO TO 124

      NC1=INDEX(J4,1)
      NC2=NC1+DURLOS(J4,1)-1

      DO 26 J2=NC1,NC2
        CHECK(I,J2)=0
        LOAD(J2)=LOAD(J2)-CAPLOS(J4)
        EFLOAD(J2)=EFLOAD(J2)-SCHLOS(J4)
   26   WEEKS(J2)=B

      INDEX(J4,1)=0
 124  CONTINUE

      IF(INDEX(J4,2).EQ.0)GO TO 191

      NC1=INDEX(J4,2)
      NC2=NC1+DURLOS(J4,2)-1
      DO 126 J2=NC1,NC2
        CHECK(I,J2)=0
        LOAD(J2)=LOAD(J2)-CAPLOS(J4)
        EFLOAD(J2)=EFLOAD(J2)-SCHLOS(J4)
  126   WEEKS(J2)=B
      INDEX(J4,2)=0

      GO TO 191

C
C      ARRAY INDEX(J) IS THE STARTING SCHEDULED MAINTENANCE WEEK
C      FOR GENERATOR J .
 27   CONTINUE
      IMM=IM+1
      IMM=IMM*168
      IF(IMM.EQ.ID1(J,5))GO TO 48
      M3=MT
      IM3=IMT
      M4=M1
      IM4=IM
      GO TO 49
 48   M3=M1
      IM3=IM
      M4=MT
      IM4=IMT
 49   CONTINUE
      INDEX(J,1)=M3
      ID1(J,4)=(M3-1)*168
      INDEX1(J)=0
      M2=M3+IM3

      IF(M2.LT.M3)GO TO 149
      IF(M3.EQ.0)GO TO 149
      DO 28 J2=M3,M2
        CHECK(I,J2)=LOCREW(J)
        EFLOAD(J2)=EFLOAD(J2)+SCHLOS(J)
   28   LOAD(J2)=LOAD(J2)+CAPLOS(J)

C      WRITE(JU1,3311)M3,M2,J
C3311 FORMAT(10X,'M1=',I4,5X,'M2=',I4,5X,'J=',I4)

 149  CONTINUE

      INDEX(J,2)=M4
      ID1(J,6)=(M4-1)*168
      M2=M4+IM4
      IF(M2.LT.M4)GO TO 29
      IF(M4.EQ.0)GO TO 29

C     WRITE(JU1,*)M4,M2

      DO 51 J2=M4,M2
        CHECK(I,J2)=LOCREW(J)
        EFLOAD(J2)=EFLOAD(J2)+SCHLOS(J)
 51     LOAD(J2)=LOAD(J2)+CAPLOS(J)

C     WRITE(JU1,*)M4,M2,J4,JA

   29 J4=J4+1
      IF(J4.GT.JA)GO TO 30
      J=J-1
      IF(MWEEK(J).EQ.0)GO TO 29

      GO TO 19

   30 CONTINUE
   89 CONTINUE

C
C      PRINT OUTPUT OF THE SCHEDULED GENERATORS OF THE PLANT
C
      NTI=0
      NMAX=40
      DO 31 I=1,M
        JB=I
        IF(INDEX(JB,1).EQ.0.AND.INDEX(JB,2).EQ.0)GO TO 31
        IF(INDEX(JB,1).EQ.0)GO TO 311
        MC1=INDEX(JB,1)
        MC2=MC1+DURLOS(JB,1)-1
        IF(MC2.GT.52)MC2=52

        DO 40 L=MC1,MC2
          WEEKS(L)=A
          IF(INDEX1(JB).EQ.0)GO TO 40
          WEEKS(L)=AB
   40   CONTINUE

 311    CONTINUE
        IF(INDEX(JB,2).EQ.0)GO TO 312
        MC1=INDEX(JB,2)
        MC2=MC1+DURLOS(JB,2)-1

        IF(MC2.GT.52)MC2=52
        DO 144 L=MC1,MC2
          WEEKS(L)=A
          IF(INDEX1(JB).EQ.0)GO TO 144
          WEEKS(L)=AB
 144    CONTINUE

 312    CONTINUE
        IU0=ID1(JB,1)
        IP0=ID1(JB,2)
        IU0=NR0(IU0)
        IF(IREPM.EQ.0)GO TO 64
        WRITE(JU1,85)NAMU(IU0),NUMP(IU0),CAPLOS(JB),(WEEKS(KL),KL=1,52)
   85   FORMAT(2X,'|',A4,A2,'|',1X,I4,1X,'|',4(13A2,'|'))
 64     CONTINUE
        DO 87 L=1,52
   87     WEEKS(L)=B
C
C
        NTI=NTI+1
        IF(NTI.LE.NMAX)GO TO 31
        NMAX=NMAX+40
        IF(IREPM.EQ.0)GO TO 65

		WRITE(JU1,804)
        WRITE(JU1,801)
        WRITE(JU1,190)ITC
        WRITE(JU1,801)
        WRITE(JU1,802)
        WRITE(JU1,803)
        WRITE(JU1,802)
        WRITE(JU1,801)
        WRITE(JU1,804)
        WRITE(JU1,805)
        WRITE(JU1,806)
        WRITE(JU1,807)
        WRITE(JU1,804)
        WRITE(JU1,801)
        WRITE(JU1,804)

   65   CONTINUE
   31 CONTINUE

      IF(IREPM.EQ.0)GO TO 66
      WRITE(JU1,804)
      WRITE(JU1,801)
      WRITE(JU1,258)
  258 FORMAT('1',10(/),T4,4X,' WEEKLY PEAK LOAD +EFFECTIVE CAPACITY (MW)
     *) ON MAINTENANCE',/)
      WRITE(JU1,290)(EFLOAD(I),I=1,52)
      WRITE(JU1,268)
  268 FORMAT('0',5(/),T4,4X,' WEEKLY PEAK LOAD + CAPACITY ON MAINTENANCE
     *',/)
      WRITE(JU1,290)(LOAD(I),I=1,52)
  290 FORMAT('0', 1(/),13(2X,I6))
   66  CONTINUE

      SUM=0.
      ITAB=ITC
      DO 111 I=1,NGU
        IND=ID1(I,1)
        IND=NR0(IND)
        ID(IND,4)=ID1(I,4)/168+1
        ID(IND,6)=ID1(I,6)/168+1
C      WRITE(JU1,8888)IND,I,(ID(IND,J),J=1,8)
C8888 FORMAT(' IND =',I5,'  I =',I5,'  ID =',8I4)
 111  CONTINUE

199      RETURN
      END



C
C********************************************************************
C                                                                   *
C        THIS SUBROUTINE COMPUTES THE CAPACITY OUTAGE PROBABILITY   *
C        TABLE FOR COMPUTING THE M FACTOR USED IN SUBROUTINE PMSC   *
C                                                                   *
C********************************************************************
C
C ---- Yongli 05.2022: NGS and P are never used and no outside values are given

C  KA AND PA PASSED TO SUBROUTINE THROUGH COMMON KAPA ... GENE P. 1/26/89
      SUBROUTINE PMLOLP(CAPLOS,RD,NGU,P,NGS)
      DOUBLE PRECISION PA,P1,P2,P3
      DIMENSION RD(500,3),KA(5000),PA(5000),P(500)
      INTEGER CAPLOS(500)
      COMMON /KAPA/KA,PA
      ISHORT=0
      JU1=13
C
C        INITIALIZE
C
      DO 2000 I=1,NGU
C      WRITE(JU1,*)CAPLOS(I),(RD(I,J),J=1,3)
2000  CONTINUE

      INC=10
      INGS=5000
      XINC=5.6
      NU=0
      CAPIND=0.
      XMGN=0.

      DO 5 I=1,INGS
        KA(I)=0
        PA(I)=0.
    5 CONTINUE

      PA(1)=1.0
      NGS=1
C
C        ADD UNITS TO CAPACITY OUTAGE PROBABILITY TABLE
C

      DO 300 I=1,NGU
        NT=I
C
C        UPDATE THE SYSTEM CAPACITY UP TO THIS UNIT
C
        CAPIND=CAPIND+CAPLOS(NT)
        KT=CAPLOS(NT)
        TK=KT
        KT=(TK+XINC-0.1)/INC
        KT=KT*INC
        PK=KT
C
C        COMPUTE THE DERATING AND ROUND OFF TO INC
C
        PK=PK*RD(NT,3)
        KP=(PK+XINC-0.1)/INC
        KP=KP*INC
        IU=3
        IF(KP.EQ.0)IU=2
        P1=0.
        P2=0.
        P3=0.
        P2=RD(NT,1)
        P3=RD(NT,2)
        P1=1.-P2-P3
        IRAT=P1*10**6
        IF(IRAT.EQ.0)GO TO 303

C
C        CALL SUBROUTINE GSM FOR UNIT ADDITION TO GENERATION
C        SYSTEM MODEL
C
C      WRITE(JU1,*)KT,KP,CAPLOS(NT)
C  KA AND PA PASSED THROUGH COMMON KAPA ... GENE P. 1/26/89

		CALL GSM(NGS,KT,KP,P1,P2,P3)

        TOTCAP=CAPIND
303     NU=NU+1
300 	  CONTINUE

C
C        RETURN WHEN ALL UNITS HAVE BEEN ADDED
C
      RETURN
      END



C
C********************************************************************
C                                                                   *
C        THIS SUBROUTINE ADDS A 2-STATE OR A 3-STATE UNIT MODEL     *
C        TO GENERATION SYSTEM MODEL                                 *
C                                                                   *
C********************************************************************
C
C  KA AND PA PASSED THROUGH COMMON KAPA ... GENE P. 1/26/89
      SUBROUTINE GSM(N,KT,KP,P1,P2,P3)
      DOUBLE PRECISION PA,P
      DOUBLE PRECISION P1,P2,P3
      DIMENSION KO(5000,3),P(5000),KA(5000),PA(5000)
      COMMON /KAPA/KA,PA
      ISHORT=0
      INGS=5000
      INGS1=INGS-50
C1000 DUM=0.     THE 1000 IS NEVER REFERENCED ... GENE P. 1/26/89
      DUM=0.
C
C        CREATE OUTAGE STATES AFTER UNIT ADDITION
C
      DO 50 I=1,N
		KO(I,1)=KA(I)
		KO(I,2)=KA(I)+KP
		KO(I,3)=KA(I)+KT
		P(I)=PA(I)
   50 CONTINUE

      NN=N+1
      NNN=NN+1

	  DO 60 I=NN,NNN
		KO(I,1)=1000000
		KO(I,2)=1000000
		KO(I,3)=1000000
		P(I)=0.
   60 CONTINUE
C
C        SELECT OUTAGE STATES IN ASCENDING ORDER OF MAGNITUDE AND
C        CALCULATE THEIR PROBABILITIES AND FREQUENCIES
C
      L=1
      J1=2
      J2=1
      IF(KP.EQ.0) J2=J2+1
      J3=1
      KA(L)=0
      PA(L)=1.

   70 L=L+1
      KCH=MIN0(KO(J1,1),KO(J2,2),KO(J3,3))
      KA(L)=KCH
      PA(L)=P(J1)*P1+P(J2)*P2+P(J3)*P3
      IF(KCH.EQ.KO(J1,1)) J1=J1+1
      IF(KCH.EQ.KO(J2,2)) J2=J2+1
      IF(KCH.EQ.KO(J3,3)) J3=J3+1
      JM=MIN0(J1,J2,J3)
      IF(JM.GT.N) GO TO 5
      IF(PA(L).LE.0.1E-08) GO TO 5
      IF(L.GT.INGS1)GO TO 5
      GO TO 70

    5 N=L

      IF(L.LE.INGS1)GO TO 6

      WRITE(*,130)PA(L)
 130  FORMAT(10X,'*** TO PREVENT ARRAY OVERFLOW,CUT OFF PROB. IN CAP OU
     *TAGE TABLE REDUCED TO ',E13.6)

   6  CONTINUE

      NN=N+1
      PA(NN)=0.0
      RETURN
      END



      SUBROUTINE INTM
      INCLUDE 'NARPERC.txt'
      COMMON /GlobalVars/ LEEI_PATH, INPUTB_PATH, INPUTC_PATH, TEMP_PATH,
     &EXTRA_PATH, INTMDT_PATH, DATOUT_PATH, DUMP_PATH,
     &OUTPUT_PATH, TRAOUT_PATH
! 声明字符变量，每个最长200个字符
      CHARACTER(len=200) :: LEEI_PATH, INPUTB_PATH, INPUTC_PATH, TEMP_PATH, 
     &EXTRA_PATH, INTMDT_PATH, DATOUT_PATH, DUMP_PATH,
     &OUTPUT_PATH, TRAOUT_PATH
      CLOSE(13)
      OPEN(UNIT=13,FILE=TRAOUT_PATH,STATUS='OLD',ACCESS='APPEND')
      OPEN(UNIT=15,FILE=DUMP_PATH,FORM='UNFORMATTED'
     &,ACCESS='SEQUENTIAL')
      WRITE(15)ATRIB(2),CLOCK,IPOINT,XLAST,SSQ,MFA,NUMINQ,ITAB,INTVT
      WRITE(15)(EVNTS(I),I=1,103)
      WRITE(15)(IGSEED(I),I=1,601)
      WRITE(15)(ILSEED(I),I=1,61)
      WRITE(15)(LT(I),I=1,20)

	  DO 10 I=1,20
        WRITE(15)(BB(I,J),J=1,20)
        WRITE(15)(ZB(I,J),J=1,20)
 10   CONTINUE

	  DO 20 I=1,15
		  WRITE(15)(SOLTHA(I,J),J=1,5)
		  WRITE(15)(SOLGHA(I,J),J=1,5)
		  WRITE(15)(SOLSHA(I,J),J=1,5)
		  WRITE(15)(SGNTHA(I,J),J=1,5)
		  WRITE(15)(SGNGHA(I,J),J=1,5)
		  WRITE(15)(SGNSHA(I,J),J=1,5)
		  WRITE(15)(SOLTPA(I,J),J=1,5)
		  WRITE(15)(SOLGPA(I,J),J=1,5)
		  WRITE(15)(SOLSPA(I,J),J=1,5)
		  WRITE(15)(SGNTPA(I,J),J=1,5)
		  WRITE(15)(SGNGPA(I,J),J=1,5)
		  WRITE(15)(SGNSPA(I,J),J=1,5)
 20   CONTINUE

      WRITE(15)(SWLSHA(I),I=1,15)
      WRITE(15)(SWLGHA(I),I=1,15)
      WRITE(15)(SWLTHA(I),I=1,15)
      WRITE(15)(SWNSHA(I),I=1,15)
      WRITE(15)(SWNGHA(I),I=1,15)
      WRITE(15)(SWNTHA(I),I=1,15)
      WRITE(15)(SWLSPA(I),I=1,15)
      WRITE(15)(SWLGPA(I),I=1,15)
      WRITE(15)(SWLTPA(I),I=1,15)
      WRITE(15)(SWNSPA(I),I=1,15)
      WRITE(15)(SWNGPA(I),I=1,15)
      WRITE(15)(SWNTPA(I),I=1,15)
      WRITE(15)(SOLTHP(I),I=1,5)
      WRITE(15)(SOLGHP(I),I=1,5)
      WRITE(15)(SOLSHP(I),I=1,5)
      WRITE(15)(SGNTHP(I),I=1,5)
      WRITE(15)(SGNGHP(I),I=1,5)
      WRITE(15)(SGNSHP(I),I=1,5)
      WRITE(15)(SOLTPP(I),I=1,5)
      WRITE(15)(SOLGPP(I),I=1,5)
      WRITE(15)(SOLSPP(I),I=1,5)
      WRITE(15)(SGNTPP(I),I=1,5)
      WRITE(15)(SGNGPP(I),I=1,5)
      WRITE(15)(SGNSPP(I),I=1,5)
      WRITE(15)SWLSHP,SWLGHP,SWLTHP,SWNSHP,SWNTHP,SWNGHP
     *,SWLSPP,SWLGPP,SWLTPP,SWNSPP,SWNGPP,SWNTPP
      DO 40 I=1,15
      WRITE(15)(HLOLE(I,J),J=1,22)
      WRITE(15)(DPLOLE(I,J),J=1,22)
      WRITE(15)(EUES(I,J),J=1,22)
 40   CONTINUE

      DO 60 I=1,15
        WRITE(15)(XNEWA(I,J),J=1,3)
 60   CONTINUE

      WRITE(15)(XNEWP(J),J=1,3)
C  ADDED THIS LINE ... GENE PRESTON
      WRITE(15) WOLSHA
      WRITE(15)(LSFLG(J),J=1,15)
      REWIND 15

      CLOSE(15)
      RETURN
      END


      SUBROUTINE RSTART
      INCLUDE 'NARPERC.txt'
C  ON FIRST READ FILE IS EMPTY AND RETURN THROUGH LINE 70 ... GENE P.
      COMMON /GlobalVars/ LEEI_PATH, INPUTB_PATH, INPUTC_PATH, TEMP_PATH,
     &EXTRA_PATH, INTMDT_PATH, DATOUT_PATH, DUMP_PATH,
     &OUTPUT_PATH, TRAOUT_PATH
! 声明字符变量，每个最长200个字符
      CHARACTER(len=200) :: LEEI_PATH, INPUTB_PATH, INPUTC_PATH, TEMP_PATH, 
     &EXTRA_PATH, INTMDT_PATH, DATOUT_PATH, DUMP_PATH,
     &OUTPUT_PATH, TRAOUT_PATH
      OPEN(UNIT=15,FILE=DUMP_PATH,FORM='UNFORMATTED',
     &ACCESS='SEQUENTIAL')
      READ(15,END=70)ATRIB(2),CLOCK,IPOINT,XLAST,SSQ,MFA,NUMINQ,ITAB
     *,INTVT
      READ(15)(EVNTS(I),I=1,103)
      READ(15)(IGSEED(I),I=1,601)
      READ(15)(ILSEED(I),I=1,61)
      READ(15)(LT(I),I=1,20)

      DO 10 I=1,20
        READ(15)(BB(I,J),J=1,20)
        READ(15)(ZB(I,J),J=1,20)
 10   CONTINUE

      DO 20 I=1,15
        READ(15)(SOLTHA(I,J),J=1,5)
        READ(15)(SOLGHA(I,J),J=1,5)
        READ(15)(SOLSHA(I,J),J=1,5)
        READ(15)(SGNTHA(I,J),J=1,5)
        READ(15)(SGNGHA(I,J),J=1,5)
        READ(15)(SGNSHA(I,J),J=1,5)
        READ(15)(SOLTPA(I,J),J=1,5)
        READ(15)(SOLGPA(I,J),J=1,5)
        READ(15)(SOLSPA(I,J),J=1,5)
        READ(15)(SGNTPA(I,J),J=1,5)
        READ(15)(SGNGPA(I,J),J=1,5)
        READ(15)(SGNSPA(I,J),J=1,5)
 20   CONTINUE

      READ(15)(SWLSHA(I),I=1,15)
      READ(15)(SWLGHA(I),I=1,15)
      READ(15)(SWLTHA(I),I=1,15)
      READ(15)(SWNSHA(I),I=1,15)
      READ(15)(SWNGHA(I),I=1,15)
      READ(15)(SWNTHA(I),I=1,15)
      READ(15)(SWLSPA(I),I=1,15)
      READ(15)(SWLGPA(I),I=1,15)
      READ(15)(SWLTPA(I),I=1,15)
      READ(15)(SWNSPA(I),I=1,15)
      READ(15)(SWNGPA(I),I=1,15)
      READ(15)(SWNTPA(I),I=1,15)
      READ(15)(SOLTHP(I),I=1,5)
      READ(15)(SOLGHP(I),I=1,5)
      READ(15)(SOLSHP(I),I=1,5)
      READ(15)(SGNTHP(I),I=1,5)
      READ(15)(SGNGHP(I),I=1,5)
      READ(15)(SGNSHP(I),I=1,5)
      READ(15)(SOLTPP(I),I=1,5)
      READ(15)(SOLGPP(I),I=1,5)
      READ(15)(SOLSPP(I),I=1,5)
      READ(15)(SGNTPP(I),I=1,5)
      READ(15)(SGNGPP(I),I=1,5)
      READ(15)(SGNSPP(I),I=1,5)
      READ(15)SWLSHP,SWLGHP,SWLTHP,SWNSHP,SWNTHP,SWNGHP
     *,SWLSPP,SWLGPP,SWLTPP,SWNSPP,SWNGPP,SWNTPP

      DO 40 I=1,15
        READ(15)(HLOLE(I,J),J=1,22)
        READ(15)(DPLOLE(I,J),J=1,22)
        READ(15)(EUES(I,J),J=1,22)
 40   CONTINUE

      DO 60 I=1,15
        READ(15)(XNEWA(I,J),J=1,3)
 60   CONTINUE

      READ(15)(XNEWP(J),J=1,3)
C  ADDED THIS LINE ... GENE PRESTON
      READ(15) WOLSHA
      READ(15)(LSFLG(J),J=1,15)
 70   REWIND 15
      CLOSE(15)
      RETURN
      END


      SUBROUTINE DGECO(A,LDA,N,IPVT,RCOND,Z)
      INTEGER LDA,N,IPVT(*)
      DOUBLE PRECISION A(LDA,*),Z(*)
      DOUBLE PRECISION RCOND
C
C     DGECO FACTORS A DOUBLE PRECISION MATRIX BY GAUSSIAN ELIMINATION
C     AND ESTIMATES THE CONDITION OF THE MATRIX.
C
C     IF  RCOND  IS NOT NEEDED, DGEFA IS SLIGHTLY FASTER.
C     TO SOLVE  A*X = B , FOLLOW DGECO BY DGESL.
C     TO COMPUTE  INVERSE(A)*C , FOLLOW DGECO BY DGESL.
C     TO COMPUTE  DETERMINANT(A) , FOLLOW DGECO BY DGEDI.
C     TO COMPUTE  INVERSE(A) , FOLLOW DGECO BY DGEDI.
C
C     ON ENTRY
C
C        A       DOUBLE PRECISION(LDA, N)
C                THE MATRIX TO BE FACTORED.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C     ON RETURN
C
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
C                WHICH WERE USED TO OBTAIN IT.
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
C
C        IPVT    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C
C        RCOND   DOUBLE PRECISION
C                AN ESTIMATE OF THE RECIPROCAL CONDITION OF  A .
C                FOR THE SYSTEM  A*X = B , RELATIVE PERTURBATIONS
C                IN  A  AND  B  OF SIZE  EPSILON  MAY CAUSE
C                RELATIVE PERTURBATIONS IN  X  OF SIZE  EPSILON/RCOND .
C                IF  RCOND  IS SO SMALL THAT THE LOGICAL EXPRESSION
C                           1.0 + RCOND .EQ. 1.0
C                IS TRUE, THEN  A  MAY BE SINGULAR TO WORKING
C                PRECISION.  IN PARTICULAR,  RCOND  IS ZERO  IF
C                EXACT SINGULARITY IS DETECTED OR THE ESTIMATE
C                UNDERFLOWS.
C
C        Z       DOUBLE PRECISION(N)
C                A WORK VECTOR WHOSE CONTENTS ARE USUALLY UNIMPORTANT.
C                IF  A  IS CLOSE TO A SINGULAR MATRIX, THEN  Z  IS
C                AN APPROXIMATE NULL VECTOR IN THE SENSE THAT
C                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     LINPACK DGEFA
C     BLAS DAXPY,DDOT,DSCAL,DASUM
C     FORTRAN DABS,DMAX1,DSIGN
C
C     INTERNAL VARIABLES
C
      DOUBLE PRECISION DDOT,EK,T,WK,WKM
      DOUBLE PRECISION ANORM,S,DASUM,SM,YNORM
      INTEGER INFO,J,K,KB,KP1,L
C
C
C     COMPUTE 1-NORM OF A
C
      ANORM = 0.0D0
      DO 10 J = 1, N
	    ANORM = DMAX1(ANORM,DASUM(N,A(1,J),1))
   10 CONTINUE

C
C     FACTOR
C
      CALL DGEFA(A,LDA,N,IPVT,INFO)
C
C     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
C     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  TRANS(A)*Y = E .
C     TRANS(A)  IS THE TRANSPOSE OF A .  THE COMPONENTS OF  E  ARE
C     CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH IN THE ELEMENTS OF W  WHERE
C     TRANS(U)*W = E .  THE VECTORS ARE FREQUENTLY RESCALED TO AVOID
C     OVERFLOW.
C
C     SOLVE TRANS(U)*W = E
C
      EK = 1.0D0
      DO 20 J = 1, N
	    Z(J) = 0.0D0
   20 CONTINUE

	  DO 100 K = 1, N
	    IF (Z(K) .NE. 0.0D0) EK = DSIGN(EK,-Z(K))
	    IF (DABS(EK-Z(K)) .LE. DABS(A(K,K))) GO TO 30
	    S = DABS(A(K,K))/DABS(EK-Z(K))
	    CALL DSCAL(N,S,Z,1)
	    EK = S*EK

   30   CONTINUE

	    WK = EK - Z(K)
	    WKM = -EK - Z(K)
	    S = DABS(WK)
	    SM = DABS(WKM)
	    IF (A(K,K) .EQ. 0.0D0) GO TO 40
	      WK = WK/A(K,K)
	      WKM = WKM/A(K,K)
	      GO TO 50
   40   CONTINUE
	      WK = 1.0D0
	      WKM = 1.0D0
   50   CONTINUE

	    KP1 = K + 1
	    IF (KP1 .GT. N) GO TO 90
	      DO 60 J = KP1, N
	        SM = SM + DABS(Z(J)+WKM*A(K,J))
	        Z(J) = Z(J) + WK*A(K,J)
	        S = S + DABS(Z(J))
   60     CONTINUE
	      IF (S .GE. SM) GO TO 80
	        T = WKM - WK
	        WK = WKM
	        DO 70 J = KP1, N
		      Z(J) = Z(J) + T*A(K,J)
   70       CONTINUE
   80     CONTINUE
   90   CONTINUE

	    Z(K) = WK
  100 CONTINUE

      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)

C
C     SOLVE TRANS(L)*Y = W
C
      DO 120 KB = 1, N
	    K = N + 1 - KB
	    IF (K .LT. N) Z(K) = Z(K) + DDOT(N-K,A(K+1,K),1,Z(K+1),1)
	    IF (DABS(Z(K)) .LE. 1.0D0) GO TO 110
	    S = 1.0D0/DABS(Z(K))
	    CALL DSCAL(N,S,Z,1)
  110   CONTINUE
	    L = IPVT(K)
	    T = Z(L)
	    Z(L) = Z(K)
	    Z(K) = T
  120 CONTINUE

      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)

C
      YNORM = 1.0D0
C
C     SOLVE L*V = Y
C
      DO 140 K = 1, N
	    L = IPVT(K)
	    T = Z(L)
	    Z(L) = Z(K)
	    Z(K) = T
	    IF (K .LT. N) CALL DAXPY(N-K,T,A(K+1,K),1,Z(K+1),1)
	    IF (DABS(Z(K)) .LE. 1.0D0) GO TO 130
	    S = 1.0D0/DABS(Z(K))
	    CALL DSCAL(N,S,Z,1)
	    YNORM = S*YNORM
  130   CONTINUE
  140 CONTINUE

      S = 1.0D0/DASUM(N,Z,1)
      CALL DSCAL(N,S,Z,1)
      YNORM = S*YNORM

C
C     SOLVE  U*Z = V
C
      DO 160 KB = 1, N
	    K = N + 1 - KB
	    IF (DABS(Z(K)) .LE. DABS(A(K,K))) GO TO 150
	    S = DABS(A(K,K))/DABS(Z(K))
	    CALL DSCAL(N,S,Z,1)
	    YNORM = S*YNORM
  150   CONTINUE
	    IF (A(K,K) .NE. 0.0D0) Z(K) = Z(K)/A(K,K)
	    IF (A(K,K) .EQ. 0.0D0) Z(K) = 1.0D0
	    T = -Z(K)
	    CALL DAXPY(K-1,T,A(1,K),1,Z(1),1)
  160 CONTINUE

C     MAKE ZNORM = 1.0
      S = 1.0D0/DASUM(N,Z,1)

      CALL DSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0D0) RCOND = 0.0D0

      RETURN
      END



      SUBROUTINE DGEFA(A,LDA,N,IPVT,INFO)
      INTEGER LDA,N,IPVT(*),INFO
      DOUBLE PRECISION A(LDA,*)
C
C     DGEFA FACTORS A DOUBLE PRECISION MATRIX BY GAUSSIAN ELIMINATION.
C
C     DGEFA IS USUALLY CALLED BY DGECO, BUT IT CAN BE CALLED
C     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
C     (TIME FOR DGECO) = (1 + 9/N)*(TIME FOR DGEFA) .
C
C     ON ENTRY
C
C        A       DOUBLE PRECISION(LDA, N)
C                THE MATRIX TO BE FACTORED.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C     ON RETURN
C
C        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
C                WHICH WERE USED TO OBTAIN IT.
C                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
C                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
C                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
C
C        IPVT    INTEGER(N)
C                AN INTEGER VECTOR OF PIVOT INDICES.
C
C        INFO    INTEGER
C                = 0  NORMAL VALUE.
C                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
C                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES
C                     INDICATE THAT DGESL OR DGEDI WILL DIVIDE BY ZERO
C                     IF CALLED.  USE  RCOND  IN DGECO FOR A RELIABLE
C                     INDICATION OF SINGULARITY.
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS DAXPY,DSCAL,IDAMAX
C
C     INTERNAL VARIABLES
C
      DOUBLE PRECISION T
      INTEGER IDAMAX,J,K,KP1,L,NM1
C
C
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
C
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 K = 1, NM1
	    KP1 = K + 1
C
C        FIND L = PIVOT INDEX
C
	    L = IDAMAX(N-K+1,A(K,K),1) + K - 1
	    IPVT(K) = L
C
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
C
	    IF (A(L,K) .EQ. 0.0D0) GO TO 40
C
C           INTERCHANGE IF NECESSARY
C
	      IF (L .EQ. K) GO TO 10
	         T = A(L,K)
	         A(L,K) = A(K,K)
	         A(K,K) = T
   10     CONTINUE
C
C           COMPUTE MULTIPLIERS
C
	      T = -1.0D0/A(K,K)
	      CALL DSCAL(N-K,T,A(K+1,K),1)
C
C           ROW ELIMINATION WITH COLUMN INDEXING
C
	      DO 30 J = KP1, N
	        T = A(L,J)
	        IF (L .EQ. K) GO TO 20
		      A(L,J) = A(K,J)
		      A(K,J) = T
   20       CONTINUE
	        CALL DAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
   30     CONTINUE
	      GO TO 50
   40   CONTINUE

	    INFO = K
   50   CONTINUE

   60 CONTINUE

   70 CONTINUE

      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0D0) INFO = N

      RETURN
      END


      SUBROUTINE  DSCAL(N,DA,DX,INCX)
C
C     SCALES A VECTOR BY A CONSTANT.
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DA,DX(*)
      INTEGER I,INCX,M,MP1,N,NINCX
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
	     DX(I) = DA*DX(I)
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
	DX(I) = DA*DX(I)
   30 CONTINUE
      IF( N .LT. 5 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
	DX(I) = DA*DX(I)
	DX(I + 1) = DA*DX(I + 1)
	DX(I + 2) = DA*DX(I + 2)
	DX(I + 3) = DA*DX(I + 3)
	DX(I + 4) = DA*DX(I + 4)
   50 CONTINUE
      RETURN
      END


      INTEGER FUNCTION IDAMAX(N,DX,INCX)
C
C     FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DX(*),DMAX
      INTEGER I,INCX,IX,N
C
      IDAMAX = 0
      IF( N .LT. 1 ) RETURN
      IDAMAX = 1
      IF(N.EQ.1)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      IX = 1
      DMAX = DABS(DX(1))
      IX = IX + INCX
      DO 10 I = 2,N
	 IF(DABS(DX(IX)).LE.DMAX) GO TO 5
	 IDAMAX = I
	 DMAX = DABS(DX(IX))
    5    IX = IX + INCX
   10 CONTINUE
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
   20 DMAX = DABS(DX(1))
      DO 30 I = 2,N
	 IF(DABS(DX(I)).LE.DMAX) GO TO 30
	 IDAMAX = I
	 DMAX = DABS(DX(I))
   30 CONTINUE
      RETURN
      END


      DOUBLE PRECISION FUNCTION DASUM(N,DX,INCX)
C
C     TAKES THE SUM OF THE ABSOLUTE VALUES.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DX(*),DTEMP
      INTEGER I,INCX,M,MP1,N,NINCX
C
      DASUM = 0.0D0
      DTEMP = 0.0D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1)GO TO 20
C
C        CODE FOR INCREMENT NOT EQUAL TO 1
C
      NINCX = N*INCX
      DO 10 I = 1,NINCX,INCX
	DTEMP = DTEMP + DABS(DX(I))
   10 CONTINUE
      DASUM = DTEMP
      RETURN
C
C        CODE FOR INCREMENT EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,6)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
	DTEMP = DTEMP + DABS(DX(I))
   30 CONTINUE
      IF( N .LT. 6 ) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,6
	DTEMP = DTEMP + DABS(DX(I)) + DABS(DX(I + 1)) + DABS(DX(I + 2))
     *  + DABS(DX(I + 3)) + DABS(DX(I + 4)) + DABS(DX(I + 5))
   50 CONTINUE
   60 DASUM = DTEMP
      RETURN
      END


      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
C
C     CONSTANT TIMES A VECTOR PLUS A VECTOR.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DX(*),DY(*),DA
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF (DA .EQ. 0.0D0) RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
	    DY(IY) = DY(IY) + DA*DX(IX)
	    IX = IX + INCX
	    IY = IY + INCY
   10 CONTINUE
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,4)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
	DY(I) = DY(I) + DA*DX(I)
   30 CONTINUE
      IF( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
	    DY(I) = DY(I) + DA*DX(I)
	    DY(I + 1) = DY(I + 1) + DA*DX(I + 1)
	    DY(I + 2) = DY(I + 2) + DA*DX(I + 2)
	    DY(I + 3) = DY(I + 3) + DA*DX(I + 3)
   50 CONTINUE
      RETURN
      END


      DOUBLE PRECISION FUNCTION DDOT(N,DX,INCX,DY,INCY)
C
C     FORMS THE DOT PRODUCT OF TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DX(*),DY(*),DTEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      DDOT = 0.0D0
      DTEMP = 0.0D0
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
C          NOT EQUAL TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
	DTEMP = DTEMP + DX(IX)*DY(IY)
	IX = IX + INCX
	IY = IY + INCY
   10 CONTINUE
      DDOT = DTEMP
      RETURN
C
C        CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C        CLEAN-UP LOOP
C
   20 M = MOD(N,5)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
	DTEMP = DTEMP + DX(I)*DY(I)
   30 CONTINUE
      IF( N .LT. 5 ) GO TO 60
   40 MP1 = M + 1
      DO 50 I = MP1,N,5
	DTEMP = DTEMP + DX(I)*DY(I) + DX(I + 1)*DY(I + 1) +
     *   DX(I + 2)*DY(I + 2) + DX(I + 3)*DY(I + 3) + DX(I + 4)*DY(I + 4)
   50 CONTINUE
   60 DDOT = DTEMP
      RETURN
      END



      SUBROUTINE DGEDI(A,LDA,N,IPVT,DET,WORK,JOB)
      INTEGER LDA,N,IPVT(*),JOB
      DOUBLE PRECISION A(LDA,*),DET(2),WORK(*)
C
C     DGEDI COMPUTES THE DETERMINANT AND INVERSE OF A MATRIX
C     USING THE FACTORS COMPUTED BY DGECO OR DGEFA.
C
C     ON ENTRY
C
C        A       DOUBLE PRECISION(LDA, N)
C                THE OUTPUT FROM DGECO OR DGEFA.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY  A .
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX  A .
C
C        IPVT    INTEGER(N)
C                THE PIVOT VECTOR FROM DGECO OR DGEFA.
C
C        WORK    DOUBLE PRECISION(N)
C                WORK VECTOR.  CONTENTS DESTROYED.
C
C        JOB     INTEGER
C                = 11   BOTH DETERMINANT AND INVERSE.
C                = 01   INVERSE ONLY.
C                = 10   DETERMINANT ONLY.
C
C     ON RETURN
C
C        A       INVERSE OF ORIGINAL MATRIX IF REQUESTED.
C                OTHERWISE UNCHANGED.
C
C        DET     DOUBLE PRECISION(2)
C                DETERMINANT OF ORIGINAL MATRIX IF REQUESTED.
C                OTHERWISE NOT REFERENCED.
C                DETERMINANT = DET(1) * 10.0**DET(2)
C                WITH  1.0 .LE. DABS(DET(1)) .LT. 10.0
C                OR  DET(1) .EQ. 0.0 .
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS
C        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.
C        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY
C        AND IF DGECO HAS SET RCOND .GT. 0.0 OR DGEFA HAS SET
C        INFO .EQ. 0 .
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS DAXPY,DSCAL,DSWAP
C     FORTRAN DABS,MOD
C
C     INTERNAL VARIABLES
C
      DOUBLE PRECISION T
      DOUBLE PRECISION TEN
      INTEGER I,J,K,KB,KP1,L,NM1
C
C
C     COMPUTE DETERMINANT
C
      IF (JOB/10 .EQ. 0) GO TO 70
	    DET(1) = 1.0D0
	    DET(2) = 0.0D0
	    TEN = 10.0D0
	    DO 50 I = 1, N
	      IF (IPVT(I) .NE. I) DET(1) = -DET(1)
	      DET(1) = A(I,I)*DET(1)
C        ...EXIT
	      IF (DET(1) .EQ. 0.0D0) GO TO 60

   10       IF (DABS(DET(1)) .GE. 1.0D0) GO TO 20
	          DET(1) = TEN*DET(1)
	          DET(2) = DET(2) - 1.0D0
	        GO TO 10
   20       CONTINUE

   30       IF (DABS(DET(1)) .LT. TEN) GO TO 40
	          DET(1) = DET(1)/TEN
	          DET(2) = DET(2) + 1.0D0
	        GO TO 30
   40       CONTINUE
   50   CONTINUE
   60   CONTINUE
   70 CONTINUE
C
C     COMPUTE INVERSE(U)
C
      IF (MOD(JOB,10) .EQ. 0) GO TO 150
	 DO 100 K = 1, N
	    A(K,K) = 1.0D0/A(K,K)
	    T = -A(K,K)
	    CALL DSCAL(K-1,T,A(1,K),1)
	    KP1 = K + 1
	    IF (N .LT. KP1) GO TO 90
	    DO 80 J = KP1, N
	       T = A(K,J)
	       A(K,J) = 0.0D0
	       CALL DAXPY(K,T,A(1,K),1,A(1,J),1)
   80    CONTINUE
   90    CONTINUE
  100    CONTINUE
C
C        FORM INVERSE(U)*INVERSE(L)
C
	 NM1 = N - 1
	 IF (NM1 .LT. 1) GO TO 140
	 DO 130 KB = 1, NM1
	    K = N - KB
	    KP1 = K + 1
	    DO 110 I = KP1, N
	       WORK(I) = A(I,K)
	       A(I,K) = 0.0D0
  110       CONTINUE
	    DO 120 J = KP1, N
	       T = WORK(J)
	       CALL DAXPY(N,T,A(1,J),1,A(1,K),1)
  120       CONTINUE
	    L = IPVT(K)
	    IF (L .NE. K) CALL DSWAP(N,A(1,K),1,A(1,L),1)
  130    CONTINUE
  140    CONTINUE
  150 CONTINUE
      RETURN
      END


      SUBROUTINE  DSWAP (N,DX,INCX,DY,INCY)
C
C     INTERCHANGES TWO VECTORS.
C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL ONE.
C     JACK DONGARRA, LINPACK, 3/11/78.
C
      DOUBLE PRECISION DX(*),DY(*),DTEMP
      INTEGER I,INCX,INCY,IX,IY,M,MP1,N
C
      IF(N.LE.0)RETURN
      IF(INCX.EQ.1.AND.INCY.EQ.1)GO TO 20
C
C       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL
C         TO 1
C
      IX = 1
      IY = 1
      IF(INCX.LT.0)IX = (-N+1)*INCX + 1
      IF(INCY.LT.0)IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
	DTEMP = DX(IX)
	DX(IX) = DY(IY)
	DY(IY) = DTEMP
	IX = IX + INCX
	IY = IY + INCY
   10 CONTINUE
      RETURN
C
C       CODE FOR BOTH INCREMENTS EQUAL TO 1
C
C
C       CLEAN-UP LOOP
C
   20 M = MOD(N,3)
      IF( M .EQ. 0 ) GO TO 40
      DO 30 I = 1,M
	DTEMP = DX(I)
	DX(I) = DY(I)
	DY(I) = DTEMP
   30 CONTINUE
      IF( N .LT. 3 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,3
	DTEMP = DX(I)
	DX(I) = DY(I)
	DY(I) = DTEMP
	DTEMP = DX(I + 1)
	DX(I + 1) = DY(I + 1)
	DY(I + 1) = DTEMP
	DTEMP = DX(I + 2)
	DX(I + 2) = DY(I + 2)
	DY(I + 2) = DTEMP
   50 CONTINUE
      RETURN
      END



