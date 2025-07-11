2!		PROGRAM		: BPCREF
5!		VERSION		: V7.0
6!		EDIT		: 07
7!		EDIT DATE	: 24-SEP-79
10		EXTEND
11	! &
	&
	&
	!		  C O P Y R I G H T &
	&
	&
  !	      Copyright (C) 1977, 1978, 1979 by &
  !	        Digital Equipment Corporation, Maynard, Mass. &
  !	&
  !	&
  !	This software is furnished under a license and may be used and &
  !	copied  only  in accordance with the terms of such license and &
  !	with the  inclusion  of  the  above  copyright  notice.   This &
  !	software  or  any  other copies thereof may not be provided or &
  !	otherwise made available to any other person.  No title to and &
  !	ownership of the software is hereby transferred. &
  !	&
  !	The information in this software is subject to change  without &
  !	notice  and should not be construed as a commitment by Digital &
  !	Equipment Corporation. &
  !	&
  !	DIGITAL assumes no responsibility for the use  or  reliability &
  !	of its software on equipment that is not supplied by DIGITAL. &
  !	&
  !******************************************************************* &
	&

20	! &
	&
	&
	!	M O D I F I C A T I O N    H I S T O R Y &
	&
	&

21	! VER/ED	EDIT DATE	REASON &
	! &
	&
100	! &
	&
	&
	!	G E N E R A L    D E S C R I P T I O N &
	&
	&

110	! BPCREF GENERATES A CROSS-REFERENCE LISTING OF BASIC-PLUS I &
	! COMPILED PROGRAMS.  EACH USE OF A VARIABLE IS INDICATED &
	! IN ONE OF THREE USAGE TYPES: &
	! 	" "	REFERENCE &
	! 	"@"	DESTRUCTIVE REFERENCE &
	! 	"#"	DEFINITION IN A "DEF FN" OR "DIM" STATEMENT. &
	! WHEN RUN, BPCREF REQUESTS INPUT AND OUTPUT FILES, AND THE &
	! LINE-WIDTH OF THE OUTPUT FILE.  THE DEFAULT EXTENSIONS ARE: &
	! 	.BAC	COMPILED INPUT FILE &
	!	.BAS	SOURCE INPUT FILE (OPTIONAL) &
	! 	.CRF	OUTPUT FILE &
	! IT IS BEYOND THE SCOPE OF THIS DOCUMENT TO DESCRIBE THE &
	! INTERNAL FORMAT OF BASIC-PLUS. &
	! &
	! &
120	! &
	! OPERATING INSTRUCTIONS: &
	!	RUN BPCREF  (OR USE THE BPCREF CCL) &
	!	BPCREF COMMAND? &
	! INPUT THE FILES TO CREF, FOR EXAMPLE: &
	!	   FOO.CRF=FOO.BAC &
	! THE ABOVE MAY ALSO BE WRITTEN: &
	!	   FOO=FOO &
	! OR: &
	!	   FOO &
	! TO INCLUDE SOURCE CODE, TYPE: &
	!	   FOO.CRF=FOO.BAC,FOO.BAS &
	! OR ANY OF THE FOLLOWING: &
	!	   FOO=FOO,FOO &
	!	   FOO,FOO &
	!	   FOO/SOURCE &
	!	   FOO/S &
	! IN ADDITION TO THE SOURCE SWITCH, THE FOLLOWING MAY BE GIVEN: &
	!	   /Q[UEUE]	QUEUE THE FILE ON THE PRINTER &
	!	   /NOD[ELETE]	DON'T DELETE .CRF FILE AFTER QUEUING &
	!	   /NOH[EADER]	DON'T PRINT PAGE HEADERS (FOR FILCOM) &
	!	*  /NOC[REF]	DON'T PRINT FULL CREF, JUST LOCAL/GLOBAL &
	!	*  /GL[OW]:N	GLOBAL LOW LIMIT &
	!	*  /GH[IGH]:N	GLOBAL HIGH LIMIT &
	!	*  /LL[OW]:N	LOCAL LOW LIMIT &
	!	*  /LH[IGH]:N	LOCAL HIGH LIMIT &
	!	   /W[IDTH]:N	USE N FOR THE CREF LINE WIDTH (N >= 72) &
	!	   /P[AGE]:N	USE N FOR THE PAGE LENGTH &
	!	   /DEBUG	FOR IN-HOUSE USE ONLY &
	! &
	! SWITCHES MARKED WITH "*" ARE UNDOCUMENTED FUNCTIONALITY. &
	! CODE MUST BE CHANGED IN BPCREF.BAS TO ENABLE THEM. &
	! USE AT YOUR OWN RISK. &
	! &
	! ANY OTHER SWITCH WILL BE APPENDED TO THE QUEUE COMMAND STRING. &
	! IF THE FILE IS QUEUED, AND THE OUTPUT DEVICE IS A LINEPRINTER, &
	! THE FILE WILL BE WRITTEN TO THE PUBLIC DISK STRUCTURE AND &
	! QUEUED TO THAT PRINTER.  IF OUTPUT IS SOMEWHERE ELSE, IT &
	! WILL BE QUEUED TO THE DEFAULT PRINTER: &
	!	   BPCREF LP2:=FOO/S/Q &
	! IS EQUIVALENT TO &
	!	   BPCREF FOO.CRF=FOO.BAC,FOO.BAS &
	!	   QUE LP2:FOO=FOO.CRF/DE &
	! DEFAULT VALUES FOR THE SWITCHES ARE DEFINED AT LINE 1030. &
	! IF THESE ARE CHANGED (TO ASSUME /SOURCE, FOR EXAMPLE), &
	! THE SWITCH MAY BE TURNED OFF BY TYPING /NOSOURCE OR /NOQUEUE. &
	! &
	! &
	! IF BPCREF IS RUN DETACHED (BY USING THE CCL COMMAND &
	! "BPCREF/DET ...", IT WILL ATTEMPT TO "KILL" ITSELF AFTER &
	! PROCESSING COMPLETES.  NOTE THAT ERROR MESSAGES ARE PRINTED &
	! EVEN IF BPCREF/BPCRF1 IS DETACHED. &
	! &
	&
130	! &
	!	G L O B A L / L O C A L   L I S T I N G S &
	&
	! FOR THE BENFIT OF ALL YOU PEOPLE OUT THERE WHO STAY UP &
	! LATE READING LISTINGS, FUNCTIONALITY WAS ADDED TO BPCREF &
	! TOO LATE FOR REGULAR DOCUMENTATION.  TO ENABLE THESE &
	! FUNCTIONS, YOU MUST EDIT LINES 12760 AND 12810. &
	! &
	! THE /GL:, /GH:, /LL:, AND /LH: SWITCHES ALLOW PARTITIONING &
	! THE PROGRAM INTO TWO REGIONS: INSIDE AND OUTSIDE THE &
	! LINE NUMBERS SPECIFIED BY THE RESPECTIVE SWITCHES. &
	! THE DEFAULT VALUES FOR THE SWITCHES ARE AS FOLLOWS: &
	! &
	!	/GL:, /LL:	0 &
	!	/GH:, /LH:	32767 &
	! &
	! IF A GLOBAL CREF IS REQUESTED BY DEFINING /GL: OR /GH:, A &
	! LISTING WILL BE PRINTED OF ALL VARIABLES THAT ARE REFERENCED &
	! BOTH INSIDE AND OUTSIDE THE BOUNDARIES DEFINED BY /GL: AND /GH:. &
	! VARIABLES THAT ARE ONLY REFERENCED WITHIN (OR ONLY WITHOUT) &
	! THE BOUNDARIES WILL NOT BE PRINTED. &
	! &
	! IF A LOCAL CREF IS REQUESTED BY DEFINING /LL: OR /LH:, A &
	! LISTING WILL BE PRINTED OF ALL VARIABLES THAT ARE ONLY REFERENCED &
	! WITHIN THE SPECIFIED LIMITS.  ANY VARIABLE REFERENCED OUTSIDE THE &
	! BOUNDARIES WILL NOT BE PRINTED, EVEN IF IT IS ALSO REFERENCED &
	! WITHIN THE BOUNDARIES. &
	! &
	! NORMALLY, THE GLOBAL AND/OR LOCAL CREF'S ARE PRINTED IN ADDITION &
	! TO THE NORMAL FULL CREF.  THE FULL CREF MAY BE OMITTED BY &
	! SPECIFYING THE /NOCREF SWITCH. &
	! &
	! THE "CORRECT REFERENCE" CHECK IS PRINTED AFTER EACH CREF &
	! LISTING.  INCORRECT LINE NUMBERS ARE ALWAYS REPORTED. &
	! &
300	! &
	&
	&
	!	I / O    C H A N N E L S &
	&
	&

301!	CHANNEL #		USED FOR &
   !
310!		4		COMMUNICATION FILE &

800	! &
	&
	&
	!	F U N C T I O N / S U B R O U T I N E    D E S C . &
	&
	&

801!	FUNCTION/SUBROUTINE		USE &
   !
810!		FNCSI%(START%,MODEL$,FLAG%)	FILE-NAME SCANNER &
   !		FNSCAN%(STOP$,END%)		SWITCH SCANNER &
   !		FNMATCH%(MODEL$,VAL%,FLAG%)	SWITCH MATCHER &
   !		FNERR$(CODE%)			RETURNS ERROR TEXT &
900	! &
	&
	&
	!	D I M E N S I O N    S T A T E M E N T S &

910	DIM CSI.S%(30), CSI.M%(30) &
		! CSI.S%()	COMMAND STRING SCANNER WORK ARRAY &
		! CSI.M%()	COMMAND STRING SCANNER WORK ARRAY &

920	DIM #4%, COMM.CODE%(255), COMM$(7)=64, COMM(1), COMM%(10) &
		! COMMUNICATION ARRAY FOR PASSING PARAMETERS TO BPCRF1: &
		! COMM.CODE%()	PASSES CODE TABLE &
		! COMM$()	PASSES FILE NAMES AND STRINGS &
		! COMM()	PASSES TIME(0) AND TIME(1) &
		! COMM%()	PASSES FLAGS &
	&
999	! &
	&
	&
	! 	M A I N    C O D I N G    A R E A &
	&
	&

1000	! &
	&
	! 	G E T   P A R A M E T E R S   A N D   S E T U P &
	&

1010	ON ERROR GOTO 19000\ PRINT IF CCPOS(0%) &
	\ VERSION$ = "V7.0-07" &
	\ JUNK$ = SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)) &
	\ JOBNO% = ASCII(JUNK$) / 2% &
	\ DETFLAG% = (ASCII(RIGHT(JUNK$,2%)) AND 1%) <> 0% &
	\ PRINT "BPCREF"; CHR$(9%); VERSION$; CHR$(9%); FNERR$(0%) &
				UNLESS CCL.ENTRY% &
		! VERSION$	VERSION/EDIT NUMBER &
		! JOBNO%	USER JOB NUMBER (FOR FILE NAME) &
		! DETFLAG%	NON-ZERO IF DETACHED ON ENTRANCE &
		! CCL.ENTRY%	WAS SET (AT 30000) IF CCL ENTRANCE &

1020	NULL$ = "" &
	\ LINESIZE%	= 132% &
	\ PAGESIZE%	= 60% &
	\ SSWITCH%	= 0% &
	\ QSWITCH%	= 0% &
	\ DSWITCH%	= -1% &
	\ HSWITCH%	= -1% &
	\ CSWITCH%	= -1% &
	\ GLOW%		= 0% &
	\ GHIGH%	= 32767% &
	\ LLOW%		= 0% &
	\ LHIGH%	= 32767% &
	\ DEBUG%	= 0% &
		! CLEAR QUE SWITCH BUFFER AND DEFINE DEFAULT SWITCH VALUES: &
		! LINESIZE%	/WIDTH: VALUE	(COLUMNS PER LINE) &
		! PAGESIZE%	/PAGE: VALUE	(LINES PER PAGE) &
		! SSWITCH%	/[NO]SOURCE	(INCLUDE .BAS FILE) &
		! QSWITCH%	/[NO]QUEUE	(QUE OUTPUT) &
		! DSWITCH%	/[NO]DELETE	(/NOD OMITS DELETION) &
		! HSWITCH%	/[NO]HEAD	(/NOH OMITS HEADING) &
		! CSWITCH%	/[NO]CREF	(/NOC OMITS STANDARD PRINT) &
		! GLOW%		/GLOW: VALUE	(GLOBAL LOW LIMIT) &
		! GHIGH%	/GHIGH: VALUE	(GLOBAL HIGH LIMIT) &
		! LLOW%		/LLOW: VALUE	(LOCAL LOW LIMIT) &
		! LHIGH%	/LHIGH: VALUE	(LOCAL HIGH LIMIT) &

1030	JUNK%, HSIZE1% = 421% &
		! HSIZE1%	TABLE SIZE + 1 (MUST BE PRIME) &
		! JUNK%		CHECKS THAT HSIZE1% INDEED IS PRIME &
		! NOTE: HSIZE1% MUST BE PRIME. THE HASH-CODE ALGORITHM &
		! WORKS BEST IF HSIZE2% IS PRIME, ALSO. &
	&
1100	! &
	&
	!	C H E C K   H S I Z E 1 %   F O R   P R I M A L I T Y &

1110	FOR HSIZE1% = ((HSIZE1% - 1%) OR 1%) STEP -2% UNTIL HSIZE1% < 1% &
	  \ HSIZE2% = HSIZE1% / 2% &
	  \ GOTO 1120 IF (HSIZE1%/Q%)*Q% = HSIZE1% &
			FOR Q%=3% TO HSIZE2% STEP 2% &
	  \ GOTO 1130 &
		! CHECK THIS HSIZE1% FOR PRIMALITY &
		! NOTE: LINES 1100-1130 MAY BE REMOVED WHEN HSIZE% IS OK &

1120	NEXT HSIZE1% &
		! LOOP UNTIL YOU GET A PRIME VALUE FOR HSIZE1% &

1130	PRINT "%Correct prime ="; HSIZE1% IF HSIZE1% <> JUNK% &
		! DOUBLE CHECK THAT HASH-TABLE SIZE IS CORRECT &
		! AND RESET REHASH VALUE &

1200	! &
	&
	!	G E T   F I L E S &

1210	GOTO 19050 IF FNCSI%(0%,"BPCRF1",8192%+128%+8%) &
	\ CHAIN.FILE$ = CSI.FILE$ &
		! DO THE SPECIAL CSI SCAN FOR THE LAST OPENED FILE &
		! RETAIN DEVICE (8192), EXTENSION (8), AND P,PN (128) &
		! INSERT THE CORRECT FILE NAME TO GET THE CHAIN FILE: &
		! SY:BPCREF.BAC		CHAIN TO BPCRF1.BAC &
		! SY:BPCREF.TSK		CHAIN TO BPCRF1.TSK &
		! DK1:(2,2)BPCREF.BAC	CHAIN TO DK1:(2,2)BPCRF1.BAC &
		! ETC. &

1220	GOSUB 12600 &
	\ IF CSI.COMMAND$ = NULL$ THEN &
	  IF CSI.ERROR% = 0% OR CCL.ENTRY% = 0% &
	    THEN GOTO 1020 &
	    ELSE GOTO 19080 &
		! GET THE FILE SPEC.  EXIT IF MESSED UP AND CCL ENTRY. &
		! OTHERWISE, JUST TRY AGAIN &
		! NULL$		GENERAL-PURPOSE NULL STRING &

1230	CLOCKTIME = TIME(0%)\ CPUTIME = TIME(1%) &
	\ CCOMMAND$ = CHR$(8%) + CHAIN.FILE$ + CHR$(13%) + CVT%$(31100%) &
	\ QCOMMAND$ =  "Q " + QCOMMAND$ + CHR$(13%) + CHR$(DETFLAG%) &
	\ TM1FILE$ = "CRF1" + NUM1$(JOBNO%) + ".TMP" &
	\ TM2FILE$ = "CRF2" + NUM1$(JOBNO%) + ".TMP" &
	\ OPEN TM1FILE$ FOR OUTPUT AS FILE 4%, CLUSTERSIZE -8%, FILESIZE 8% &
		! START TIMING AND OPEN COMMUNICATION FILE &
		! CCOMMAND$	USED TO CHAIN TO $QUEUE AND RETURN TO BPCRF1 &
		! QCOMMAND$	COMMAND PASSED TO $QUEUE &
	&
	&
1300	! &
	&
	!	I N I T I A L I Z E   F O R   C O L L E C T I O N &

1310	RESTORE\ JUNK$ = "" &
	\ READ JUNK$ UNTIL JUNK$ = "*PUSHPOP*" &
	\ READ COMM.CODE%(Q%) FOR Q% = 0% TO 255% &
	\ COMM$(0%)	= BACFILE$ &
	\ COMM$(1%)	= BASFILE$ &
	\ COMM$(2%)	= LSTFILE$ &
	\ COMM$(3%)	= TM1FILE$ &
	\ COMM$(4%)	= TM2FILE$ &
	\ COMM$(5%)	= PROG.NAME$ &
	\ COMM$(6%)	= CCOMMAND$ &
	\ COMM$(7%)	= QCOMMAND$ &
	\ COMM(0%)	= CLOCKTIME &
	\ COMM(1%)	= CPUTIME &
	\ COMM%(0%)	= PAGESIZE% &
	\ COMM%(1%)	= LINESIZE% &
	\ COMM%(2%)	= QSWITCH% &
	\ COMM%(3%)	= HSWITCH% &
	\ COMM%(4%)	= HSIZE1% &
	\ COMM%(5%)	= CSWITCH% &
	\ COMM%(6%)	= GLOW% &
	\ COMM%(7%)	= GHIGH% &
	\ COMM%(8%)	= LLOW% &
	\ COMM%(9%)	= LHIGH% &
	\ COMM%(10%)	= DEBUG% &
		! SETUP THE COMMUNICATION VECTOR &

1350	CLOSE 4% &
	\ JUNK$ = SYS(CHR$(8%) + TM1FILE$) &
		! SETUP FOR CHAINING &

1360	ON ERROR GOTO 1370 &
	\ CHAIN CHAIN.FILE$ LINE 31000% &
		! OFF WE GO &

1370	PRINT '?Could not chain to "'; CHAIN.FILE$; '" -- '; FNERR$(ERR) &
	\ GOTO 19080 &
		! WELL, HERE WE ARE AGAIN &
	&
12000	! &
	&
	!	S U B R O U T I N E S &

12600	! &
	&
	!	G E T   C O M M A N D S   A N D   O P E N   F I L E S &

12610	IF CSI.COMMAND$ = NULL$ &
	  THEN PRINT "BPCREF command"; &
	    \ INPUT LINE CSI.COMMAND$\ CSI.COMMAND$ = CVT$$(CSI.COMMAND$,-1%) &
	    \ GOTO 12760 IF CSI.COMMAND$ = NULL$ &
		! GET SOMETHING TO DO, (OR GIVE SOME HELP) &

12620	ISOUTPUT% = INSTR(1%, CSI.COMMAND$, "=") &
	\ GOTO 12750 IF FNCSI%(ISOUTPUT%+1%, "SY:.BAC/RO", 0%) &
	\ BACFILE$ = CSI.FILE$\ PROG.NAME$ = CSI.NAME$ &
	\ IF CSI.S1% < 0% OR ((CSI.STATUS% AND 255%) <> 0%) &
	  THEN PRINT "?"; BACFILE$; " must be on disk" &
	    \ CSI.ERROR% = 6%\ GOTO 12750 &
		! FIND THE FIRST INPUT FILE AND PARSE IT &
		! MAKE SURE IT'S ON THE DISK &
		! BACFILE$	.BAC INPUT FILE &
		! PROG.NAME$	.BAC INPUT FILE -- JUST THE NAME &

12630	GOSUB 12800\ GOTO 12750 IF CSI.ERROR% &
	\ BASFILE$ = NULL$\ INSWITCH$ = NONSWITCH$ &
	\ IF (CSI.STOP% < LEN(CSI.COMMAND$)) OR SSWITCH% THEN &
	  GOTO 12750 IF FNCSI%(CSI.STOP%+1%, "SY:"+PROG.NAME$+".BAS/RO", 0%) &
	  \ BASFILE$ = CSI.FILE$ &
	  \ GOSUB 12800\ GOTO 12750 IF CSI.ERROR% &
	  \ INSWITCH$ = INSWITCH$ + NONSWITCH$ &
		! PROCESS ANY SWITCHES ON .BAC, THEN &
		! PROCESS .BAS FILE (IF ONE GIVEN OR /SOURCE REQUESTED) &
		! BASFILE$	.BAS INPUT FILE &

12640	GOTO 12750 IF FNCSI%(1%+((ISOUTPUT% = 0%) AND LEN(CSI.COMMAND$)), &
			"SY:"+PROG.NAME$+".CRF", 0%) &
	\ GOSUB 12800\ GOTO 12750 IF CSI.ERROR% &
	\ LSTFILE$ = CSI.FILE$\ QCOMMAND$ = NULL$\ OUTSWITCH$ = NONSWITCH$ &
	\ IF QSWITCH% THEN &
	  IF CSI.S1% > 0% AND (CSI.STATUS% AND 255%) <> 6% &
	    THEN QCOMMAND$ = PROG.NAME$ + OUTSWITCH$ &
			+ "=" + LSTFILE$ + INSWITCH$ &
	    ELSE JUNK% = INSTR(1%, CSI.FILE$, ":") &
	      \ LSTFILE$ = RIGHT(CSI.FILE$, JUNK%+1%) &
	      \ QCOMMAND$ = LEFT(CSI.FILE$, JUNK%) + PROG.NAME$ &
			+ OUTSWITCH$ + "=" + LSTFILE$ + INSWITCH$ &
		! IF BOTH OUTPUT AND INPUT FILES WERE SPECIFIED, &
		! SCAN THE COMMAND FROM THE LEFT, ELSE, SCAN A DUMMY &
		! STRING (WHICH DOES THE RIGHT THING) &
		! IF WE'RE QUEUING AND THE OUTPUT DEVICE IS LP:, &
		! OUTPUT TO THE DISK AND QUEUE IT ON THE CORRECT PRINTER &
12700	IF LINESIZE% < 72% THEN &
	  PRINT "%Illegal /WIDTH, minimum = 72" &
	  \ LINESIZE% = 72% &
		! MAKE SURE THERE'S ENOUGH ROOM ON THE LINE &

12710	QCOMMAND$ = QCOMMAND$ + "/DE" IF DSWITCH% &
	\ IF INSWITCH$ + OUTSWITCH$ <> "" AND DETFLAG% = 0% &
	  THEN PRINT '%Unknown switch(es), "'; QCOMMAND$; '"'; &
	    \ PRINT " will be passed to queue manager"; IF QSWITCH% &
	    \ PRINT &
		! DON'T LET THE USER GET TOO FAR OUT OF LINE &

12720	GOTO 12790 &
		! NO MORE SWITCH PROCESSING &

12750	PRINT "?Command error -- "; FNERR$(CSI.ERROR%) &
		! SORRY ABOUT THAT &

12760	PRINT 'BPCREF command format: "OUTFILE.CRF=INFILE.BAC[,INFILE.BAS]"' &
	\ PRINT "Note the following switches:" &
	\ PRINT " /SOURCE          Include source code file" &
	\ PRINT " /QUEUE           Queue file for printing" &
	\ PRINT " /NODELETE	  Don't delete file after queueing" &
	\ PRINT " /NOHEAD          No header lines (for FILCOM)" &
	\ PRINT " /WIDTH:"; NUM1$(LINESIZE%); TAB(18%); &
				 "Line width for cref output" &
	\ PRINT " /PAGE:"; NUM1$(PAGESIZE%); TAB(18%); &
				 "Number of lines per page" &
	\ PRINT " /NOCREF	  No standard CREF printout" &
	\ PRINT " /GLOW:0	  Global low  limit" &
	\ PRINT " /GHIGH:32767	  Global high limit" &
	\ PRINT " /LLOW:0	  Local  low  limit" &
	\ PRINT " /LHIGH:32767	  Local  high limit" &
	\ PRINT 'The command "FILE" defaults to "FILE.CRF=FILE.BAC"' &
	\ PRINT 'The command "FILE/S" defaults to "FILE.CRF=FILE.BAC,FILE.BAS"' &
		! GIVE A HINT &

12780	CSI.COMMAND$ = NULL$ &
		! TRY AGAIN &

12790	RETURN &
12800	! &
	&
	!	P R O C E S S   S W I T C H E S &

12810	NONSWITCH$ = NULL$ &
	\ WHILE MID(CSI.COMMAND$,CSI.STOP%,1%) = "/" &
	  \ CSI.STOP% = CSI.STOP% + 1% &
	  \ CSI.SW% = FNSCAN%("/",LEN(CSI.COMMAND$)+1%) &
	  \ CSI.SW% = FNSCAN%(",",CSI.SW%) &
	  \ CSI.SW% = FNSCAN%("=",CSI.SW%) &
	  \ IF CSI.SW% > CSI.STOP% THEN &
	    SSWITCH%	= FNMATCH%("SOURCE", SSWITCH%,  1%) &
	    \ QSWITCH%	= FNMATCH%("QUEUE",  QSWITCH%,  1%) &
	    \ DSWITCH%	= FNMATCH%("DELETE", DSWITCH%,  1%) &
	    \ HSWITCH%	= FNMATCH%("HEADER", HSWITCH%,  1%) &
	    \ DEBUG%	= FNMATCH%("DEBUG",  DEBUG%,    5%)	IF 0% &
	    \ LINESIZE%	= FNMATCH%("WIDTH",  LINESIZE%,-1%) &
	    \ PAGESIZE%	= FNMATCH%("PAGE",   PAGESIZE%,-1%) &
	    \ CSWITCH%	= FNMATCH%("CREF",   CSWITCH%,  1%) &
	    \ GLOW%	= FNMATCH%("GLOW",   GLOW%,    -2%) &
	    \ GHIGH%	= FNMATCH%("GHIGH",  GHIGH%,   -2%) &
	    \ LLOW%	= FNMATCH%("LLOW",   LLOW%,    -2%) &
	    \ LHIGH%	= FNMATCH%("LHIGH",  LHIGH%,   -2%) &
		! FOR ALL THE SWITCHES, GET THE ACTUAL SWITCH LENGTH &
		! AND TEST IT AGAINST THE CANDIDATES. &
		! IF NO MATCH, ADD IT TO THE QUEUE SWITCHES. &
		! NOTE:	THE "IF 0%" REMOVES DEBUG MODE SINCE THE &
		!	PROGRAM IS (OFFICIALLY) DEBUGGED. &

12820	    NONSWITCH$ = MID(CSI.COMMAND$,CSI.STOP%-1%,CSI.SW%-CSI.STOP%+1%) &
				IF CSI.SW% <> CSI.STOP% &
	    \ CSI.STOP% = CSI.SW% &
	\ NEXT &
		! DO AS MANY AS POSSIBLE &

12830	RETURN &

14500	DEF* FNERR$(Q%) &
		= CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+CHR$(Q%)),3%),4%) &
		! &
	&
		! F N E R R $ ( Q % ) &
	&
		!	RETURN THE SYSTEM ERROR MESSAGE FOR Q% &
		! &
	&
19000	! &
	&
	!	F A T A L   E R R O R S   C O M E   H E R E &

19010	RESUME 32767 IF ERR = 11% AND ERL = 12610% &
		! HANDLE EXPECTED ERROR(S) &

19020	RESUME 19050 &
		! DIE ON UNEXPECTED ERRORS &

19050	E0% = ERR\ E1% = ERL &
	\ ON ERROR GOTO 0 &
	\ PRINT "?BPCREF "; VERSION$; "	Fatal error"; E0%; &
			"at line"; E1%; " -- "; FNERR$(E0%) &

19080	CLOSE Q% FOR Q% = 0% TO 12% &

19090	GOTO 32767 &
		! CAN'T DO MORE &
	&
20000	! &
	&
	!	D A T A   T A B L E S &
	&

20010	DATA	"*CSI*"
20020	DATA	29,0128, 29,0001, 29,0001, 29,0008, 27,0004, 27,0001
20030	DATA	27,0002, 00,0000, 29,1024, 29,8192, 29,8192
20090	DATA	"*PUSHPOP*" &

20099	! &
	! PUSH-POP TYPE CODE TABLE.  NOTE THAT THE VALUES &
	! ARE TWO GREATER THAN THE "ACTUAL" TYPES TO SIMPLIFY &
	! THE ON "TYPE" GOSUB ... STATEMENT &
	! ACTUAL TYPES ARE TAKEN FROM THE "PPCODE" PROGRAM &
	! &

21000	DATA	02,02,02,02,02,02,02,02,02,02,02,02,02,02,02,02
21010	DATA	02,02,06,02,02,05,07,04,03,04,03,03,04,04,04,04
21020	DATA	08,08,04,04,08,09,09,09,02,02,02,02,02,02,02,02
21030	DATA	07,05,02,19,02,02,18,02,03,02,01,01,02,01,01,02
21040	DATA	02,02,02,02,01,01,01,01,01,01,01,01,03,02,02,04
21050	DATA	04,02,02,02,02,13,02,02,11,02,02,02,02,04,04,04
21060	DATA	04,04,04,04,04,04,02,02,02,02,02,02,02,02,02,02
21070	DATA	02,02,02,02,02,02,02,02,06,02,02,02,01,01,01,01
21080	DATA	02,02,02,02,02,02,02,02,02,02,02,02,02,02,02,02
21090	DATA	02,02,02,02,02,02,02,02,02,02,02,02,02,02,02,02
21100	DATA	02,02,02,03,03,03,04,04,04,04,04,04,03,03,04,04
21110	DATA	04,04,04,04,04,04,04,04,04,04,04,04,04,04,02,02
21120	DATA	02,02,02,02,02,02,02,02,02,02,02,02,02,02,02,02
21130	DATA	02,02,02,02,02,02,11,05,11,05,11,05,14,02,05,02
21140	DATA	02,02,02,02,02,02,02,02,02,02,02,15,15,15,15,15
21160	DATA	15,15,15,16,16,17,17,02,02,02,02,02,02,02,02,02 &
26100	! &
	&
	!	F N C S I % ( P L A C E % , M O D E L $ , F L A G % ) &

26110	DEF* FNCSI%(PLACE%, MODEL$, FLAG%) &
		! &
		!	IF FLAG% IS ZERO, DO A COMPLETE FILENAME &
		!	SCAN ON CSI.COMMAND$ STARTING AT BYTE PLACE%. &
		!	IF FLAG% IS NON-ZERO, THE NAME OF THE LAST &
		!	FILE TO BE OPENED IS RETURNED USING FLAG% &
		!	TO DETERMINE WHICH FIELDS ARE TO BE RETURNED. &
		!	NOTE: CSI.STOP% IS NOT SET BY THIS CALL. &
		!	FLAG% IS BIT-ENCODED AS DESCRIBED FOR THE &
		!	S1% FIELD (BYTES 29 AND 30 OF THE RETURNED &
		!	STRING).  NOTE THAT THE MODEL$ STRING MAY &
		!	ALSO BE USED TO FILL IN ADDITIONAL FIELDS. &
		!	IF CALLED IMMEDIATELY UPON ENTRANCE TO &
		!	THE PROGRAM, THIS ALLOWS DETERMINING THE &
		!	PROGRAM'S FILE NAME, EXTENSION, AND/OR &
		!	LOCATION (PUBLIC VERSUS PRIVATE DISK, ETC.) &
		!	FOR EXAMPLE: &
		!	  STOP IF FNCSI%(0%,"FOO",8192%+128%+8%) &
		!	SETS CSI.FILE$ TO "DK2:[3,4]FOO.BAC" IF &
		!	THE PROGRAM'S FILE NAME IS "DK2:[3,4]BAR.BAC". &
		! &
		!	IF ANY FIELDS ARE MISSING, FILL THEM IN &
		!	FROM MODEL$ (IF THEY ARE PRESENT THERE). &
		!	RETURN ZERO IF CORRECT, THE ERROR CODE IF NOT. &
		!	INPUT PARAMETERS: &
		!	  CSI.COMMAND$	GLOBAL STRING TO SCAN (PLACE% > 0%) &
		!	  PLACE%	WHERE TO START THIS SCAN &
		!	  MODEL$	DEFAULT SPECIFICATIONS &
		!	SCAN RESULTS ARE RETURNED AS FOLLOWS: &
		!	  CSI.FILE$	FULL FILENAME AFTER PROCESSING &
		!	  CSI.NAME$	JUST THE NAME PART OF THE FILE &
		!	  CSI.STOP%	WHERE THE SCAN STOPPED (PLACE% > 0%) &
		!	  CSI.STATUS%	STATUS VALUE AFTER SCAN &
		!	  CSI.S1%	S1% FLAG WORD (SEE PROG. MANUAL) &
		!	  CSI.S%(30)	EXPANDED FILE NAME (WORD-PACKED) &
		!	  CSI.ERROR%	ERROR CODE (= FNCSI% VALUE) &
		! &
		!	THE FOLLOWING SHOULD BE DIMENSIONED: &
		!	  DIM CSI.S%(30), CSI.M%(30) &
		! &
		!	JUNK% AND JUNK$ ARE USED FOR SCRATCH STORAGE &
		! &
		!	FOR EXAMPLE: &
		!	  CSI.COMMAND$ = "FOO[2,*],DK0:FOOBAR.FOO"" &
		!	  \ Q% = FNCSI%(1%, "[1,3].BAR") &
		!	RETURNS: &
		!	  CSI.FILE$ = "[2,*]FOO.BAR" &
		!	  CSI.STOP% = 9% &
		!	IF FNCSI% IS RECALLED: &
		!	  Q% = FNCSI%(CSI.STOP%+1%, ["[2,4].BAR") &
		!	RETURNS: &
		!	  CSI.FILE$ = "DK0:[2,4]FOOBAR.FOO" &
		! &
26120	ON ERROR GOTO 26180 &
	\ IF FLAG% <> 0% &
	  THEN CHANGE SYS(CHR$(12%)) TO CSI.S% &
	  ELSE CHANGE SYS(CHR$(6%) + CHR$(-23%) &
			+ RIGHT(CSI.COMMAND$,PLACE%)) TO CSI.S% &
	    \ CSI.STOP% = LEN(CSI.COMMAND$) - RECOUNT + 1% &
		! GET THE FILE INTO THE INTERNAL FORMAT. &
		! GET THE LAST FILE OPENED IF FLAG% <> 0% &

26130	CHANGE SYS(CHR$(6%) + CHR$(-10%) + MODEL$)  TO CSI.M% &
	\ CSI.S%(JUNK%) = CSI.S%(JUNK%) + SWAP%(CSI.S%(JUNK%+1%)) &
			FOR JUNK% = 5% TO 29% STEP 2% &
	\ CSI.S%(29%) = FLAG% IF FLAG% <> 0% &
	\ CSI.M%(JUNK%) = CSI.M%(JUNK%) + SWAP%(CSI.M%(JUNK%+1%)) &
			FOR JUNK% = 5% TO 29% STEP 2% &
	\ RESTORE\ JUNK$ = ""\ READ JUNK$ UNTIL JUNK$ = "*CSI*" &
	\ FOR JUNK% = 5% TO 25% STEP 2% &
	  \ READ CSI.F%, CSI.M% &
	  \ CSI.M% = CSI.M% AND CSI.M%(CSI.F%) &
	  \ CSI.S%(JUNK%) = CSI.M%(JUNK%) &
			IF NOT (CSI.M% IMP CSI.S%(CSI.F%)) &
	  \ CSI.S%(CSI.F%) = CSI.S%(CSI.F%) OR CSI.M% &
				UNLESS JUNK% = 7% OR JUNK% = 23% &
		! MERGE SPECIFICATION FIELDS &
		! CSI.F%	THE BIT TO TEST &
		! CSI.M%	WHICH FLAG WORD (27% OR 29%) TO TEST &
		! THE MERGE TAKES PLACE IF THE MASKED BIT IS CLEAR IN &
		! THE SOURCE$ FLAG AND SET IN THE MODEL$ STRING. &
		! THE SOURCE$ FLAG BITS ARE ALSO UPDATED UNLESS THIS IS &
		! THE FIRST WORD OF A TWO-WORD FIELD (FILE NAME OR &
		! DEVICE NAME). &
		! (AFTER 15 YEARS IN THIS BUSINESS, I FINALLY USED IMP.) &

26140	NEXT JUNK% &
	\ CSI.FILE$ = NULL$\ JUNK% = CSI.S%(29%) &
	\ IF JUNK% AND 8192% THEN &
	  IF JUNK% < 0% &
	    THEN CSI.FILE$ = RAD$(CSI.S%(23%)) + RAD$(CSI.S%(25%)) + ":" &
	    ELSE CSI.FILE$ = CHR$(CSI.S%(23%)) + CHR$(SWAP%(CSI.S%(23%))) &
	      \ CSI.F% = SWAP%(CSI.S%(25%)) AND 255% &
	      \ CSI.FILE$ = "SY" IF (CSI.F% <> 0%) &
					AND ((CSI.F% AND 3%) <> 3%) &
	      \ CSI.FILE$ = CSI.FILE$ + NUM1$(CSI.S%(25%) AND 255%) &
				IF CSI.F% = 255% &
	      \ CSI.FILE$ = CSI.FILE$ + ":" &
		! PROCESS LOGICAL AND/OR PHYSICAL DEVICE NAMES &
	&
26150	GOTO 26170 UNLESS JUNK% AND 128% &
	\ IF JUNK% AND 256% &
	  THEN CSI.FILE$ = CSI.FILE$ + "[*" &
	  ELSE CSI.FILE$ = CSI.FILE$ + "[" &
			+ NUM1$(SWAP%(CSI.S%(5%)) AND 255%) &
		! SKIP P,PN PROCESSING UNLESS THERE IS ONE. &
		! DO PROJECT NUMBER FIRST &

26160	IF JUNK% AND 512% &
	  THEN CSI.FILE$ = CSI.FILE$ + ",*]" &
	  ELSE CSI.FILE$ = CSI.FILE$ + "," + NUM1$(CSI.S%(5%) AND 255%) &
			+ "]" &
		! NOW, DO THE PROGRAMMER NUMBER &

26170	CSI.FILE$ = CSI.FILE$ + RAD$(CSI.S%(7%)) + RAD$(CSI.S%(9%)) &
				IF JUNK% AND 1% &
	\ CSI.FILE$ = CSI.FILE$ + "." + RAD$(CSI.S%(11%)) &
				IF JUNK% AND 8% &
	\ CSI.FILE$ = CSI.FILE$ + "<" &
			+ NUM1$(SWAP%(CSI.S%(21%)) AND 255%) + ">" &
				IF JUNK% AND 1024% &
	\ JUNK% = CSI.S%(27%) &
	\ CSI.FILE$ = CSI.FILE$ + "/SI:" + NUM1$(CSI.S%(13%)) &
				IF JUNK% AND 4% &
	\ CSI.FILE$ = CSI.FILE$ + "/CL:" + NUM1$(CSI.S%(15%)) &
				IF JUNK% AND 1% &
	\ CSI.FILE$ = CSI.FILE$ + "/MO:" + NUM1$(CSI.S%(17%) AND 32767%) &
				IF JUNK% AND 2% &
	\ CSI.FILE$ = CVT$$(CSI.FILE$, 2%) &
	\ CHANGE SYS(CHR$(6%) + CHR$(-10%) + CSI.FILE$) TO CSI.S% &
	\ CSI.STATUS% = STATUS &
	\ CSI.S%(JUNK%) = CSI.S%(JUNK%) + SWAP%(CSI.S%(JUNK%+1%)) &
			FOR JUNK% = 5% TO 29% STEP 2% &
	\ CSI.NAME$ = CVT$$(RAD$(CSI.S%(7%)) + RAD$(CSI.S%(9%)), 2%) &
	\ CSI.S1% = CSI.S%(29%)\ CSI.ERROR% = 0% &
	\ GOTO 26190 &
		! REBUILD THE FILE NAME STRING AND PARSE IT TO SETUP &
		! RETURN VALUES.  WHEW. &

26180	CSI.ERROR% = ERR &
	\ RESUME 26190 &
		! HERE ON ANY ERROR -- THE CALLER PRINTS THE MESSAGE &

26190	ON ERROR GOTO 19000 &
	\ FNCSI% = CSI.ERROR% &
	\ FNEND &
	&
26200	! &
	&
	!	F N S C A N % ( S T O P $ , E N D % ) &

26210	DEF* FNSCAN%(STOPBYTE$, ENDBYTE%) &
		! &
		!	SCAN CSI STRING CSI.COMMAND$ FOR &
		!	STOPBYTE$.  RETURN WHERE THE SCAN STOPPED AT &
		!	IF THE BYTE WAS FOUND AND THE POSITION &
		!	WAS BEFORE ENDBYTE%.  OTHERWISE, RETURN &
		!	ENDBYTE%. &
		!	NOTE:  START THE SCAN AT CSI.STOP%. &
		! &

26220	FNSCAN%, JUNK% = INSTR(CSI.STOP%, CSI.COMMAND$, STOPBYTE$) &
	\ FNSCAN% = ENDBYTE% IF JUNK% = 0% OR JUNK% > ENDBYTE% &
		! AS WE SAID ... &

26230	FNEND &
	&
26300	! &
	&
	!	F N M A T C H % ( M O D E L $ , S W V A L U E % , &
	!				S W L E N % ) &
	&
	! &

26310	DEF* FNMATCH%(MODEL$, SWVALUE%, SWLEN%) &
		!	IF THIS SWITCH DOESN'T MATCH, RETURN SWVALUE%. &
		!	ABS(SWLEN%) DEFINES THE NUMBER OF BYTES &
		!	THAT MUST MATCH.  IF THE STRING DOES MATCH, &
		!	CHECK THE SIGN OF SWLEN%: &
		!	SWLEN% > 0%, NO VALUE MAY BE PRESENT, RETURN &
		!		1 OR 0 DEPENDING ON THE PRESENCE &
		!		OF A "NO" PREFIX. &
		!	SWLEN% < 0%, A VALUE MUST BE PRESENT, RETURN IT &
		!	IF CONFUSED, SET CSI.ERROR%. &
		!	NOTE:  THE ABOVE REFERS TO THE PROGRAM, NOT YOU. &
		!	SWITCHES WITHOUT VALUES MAY HAVE "NO" PREFIX. &

26320	FNMATCH% = SWVALUE% &
	\ JUNK1% = MID(CSI.COMMAND$,CSI.STOP%,2%) <> "NO" &
	\ JUNK2% = CSI.STOP%\ JUNK2% = CSI.STOP% + 2% UNLESS JUNK1% &
	\ GOTO 26390 IF MID(CSI.COMMAND$,JUNK2%,ABS(SWLEN%)) &
				<> LEFT(MODEL$,ABS(SWLEN%)) &
	\ JUNK% = FNSCAN%(":",CSI.SW%) &
	\ SWLEN% = (SWLEN% < 0%) &
	\ GOTO 26390 IF MID(CSI.COMMAND$,JUNK2%,JUNK%-JUNK2%) &
			<> LEFT(MODEL$, JUNK%-JUNK2%) &
	\ IF ((JUNK% = CSI.SW%) EQV SWLEN%) &
				OR ((JUNK1% = 0%) AND SWLEN%) THEN &
	    CSI.ERROR% = 67%\ GOTO 26390 &
		! SETUP FOR SCANNING: &
		! JUNK1%	FLAG, SET IF NO "NO" PREFIX &
		! JUNK2%	WHERE TO START LOOKING FOR SWITCH MATCH &
		! JUNK%		END OF STRING TO MATCH AGAINST MODEL$ &
		! THEN, EXIT "ILLEGAL SWITCH" IF MESSED UP &

26330	FNMATCH% = JUNK1% &
	\ IF SWLEN% THEN &
	  ON ERROR GOTO 26350 &
	  \ FNMATCH% = VAL(MID(CSI.COMMAND$,JUNK%+1%,CSI.SW%-JUNK%-1%)) &
		! LOOK FOR MATCHING SWITCH &

26340	ON ERROR GOTO 19000 &
	\ CSI.STOP% = CSI.SW% &
	\ GOTO 26390 &
		! FOUND SOMETHING, RESET SCAN POINTER &

26350	CSI.ERROR% = ERR\ RESUME 26340 &
		! CRASH IT &

26390	 FNEND &
	&
30000	! &
	&
	!	C C L   E N T R Y &

30010	CSI.COMMAND$ = SYS(CHR$(7%)) &
	\ JUNK% = INSTR(1%, CSI.COMMAND$, " ") &
	\ JUNK% = LEN(CSI.COMMAND$) IF JUNK% = 0% &
	\ CSI.COMMAND$=RIGHT(CSI.COMMAND$,JUNK%+1%) &
		! GET THE COMMAND LINE &
		! STRIP OFF THE CCL NAME (SO WE CAN USE ANY) &

30020	CCL.ENTRY% = LEN(CSI.COMMAND$) &
	\ GOTO 1000 &

32767	END
