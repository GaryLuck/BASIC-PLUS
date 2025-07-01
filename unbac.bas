1	! Start of Decompiler for BASIC-PLUS &
800	  EXTEND
810	  DIM ADDR%(348%),VAR$(240%) &
	\ DIM #1%,A%(255%),A$(255%)=16%,ADDR2%(240%),VAR2$(240%)=32%, &
		P1%(8%),DTA$(31%)=16%,ADDR1%(4096%),VAR1$(4096%)=32% &
	\ DIM #9%,P%(32767%) &

1000	  EXTEND &
	\ VERSION$="7.1" &
	\ S1$="" &
	\ E$ = SYS(CHR$(6%)+CHR$(9%)+CHR$(0%)) &
	\ PRINT "UNBAC - ";VERSION$; "  "; &
		CVT$$(RIGHT(E$,3%),4%),DATE$(0%);"  ";TIME$(0%) &
	\ PRINT FNINM$ &
	\ PRINT &
	\ ON ERROR GOTO 19000 &
	\ PRINT 'TYPE "?" FOR HELP.' &
	\ JOB$ = RIGHT(NUM1$(100%+ASCII(E$)/2%),2%)
1010	  PRINT "[]"; &
	\ GET #0% \ FIELD #0%, RECOUNT AS A$ &
	\ A$ = CVT$$(A$,39%) &
	\ GOTO 2000 IF A$="?" &
	\ GOTO 1010 IF A$="" &
	\ GOSUB 10000 &
	\ GOTO 1010 IF F1% &
	\ I% = INSTR(1%,A$,"=") &
	\ IF I%=0% THEN A1$ = A$ &
		\ I% = INSTR(1%,A1$,".") &
		\ A1$ = LEFT(A1$,I%-1%) IF I% &
		\ GOTO 1030
1020	  A1$ = LEFT(A$,I%-1%) &
	\ A$ = RIGHT(A$,I%+1%)
1030	  I% = INSTR(1%,A$,".") &
	\ A$ = A$+".BAC" IF I%=0% &
	\ I% = INSTR(1%,A1$,".") &
	\ A1$ = A1$+".BAS" IF I%=0% &
	\ V$ = SYS(CHR$(6%)+CHR$(-10%)+A$) &
	\ L%=SWAP%(CVT$%(RIGHT(V$,13%))) &
	\ I%=ASCII(RIGHT(S$,22%)) &
	\ IF (STATUS AND 255%) THEN PRINT "?Must be on disk." &
		\ GOTO 1010
1040	  OPEN A$ FOR INPUT AS FILE 9% &
	\ SP% = FNW%(514%) &
	\  Q% = FNB%(SP%+1%) &
	\ SCALEF% = (256% - FNB%(SP% + 39%)) AND 255% &
	\ IF Q%<1% OR Q%>4% THEN &
		 PRINT "?Version # out of range (";NUM1$(Q%);")!" &
		\ GOTO 1010
1050	  V$ = SYS(CHR$(6%)+CHR$(-10%)+A1$) &
	\ IF (STATUS AND 255%)=0% THEN &
		  OPEN A1$ FOR INPUT AS FILE 1% &
		\ CLOSE 1% &
		\ PRINT "?Supersede Error on ";A1$ &
		\ GOTO 1010
1060	  OPEN "DECM"+JOB$+".TMP" FOR OUTPUT AS FILE 1% &
	\ PRINT "Please Wait for variable table to be extracted. . ." &
		IF SLASH.V% &
	\ RESTORE &
	\ GOSUB 15800 &
	\ FOR K%=0% TO 255% &
		\ READ A%,L%,T%,T1%,D$ &
		\ A%(K%)=SWAP%(T1%)+T%*16%+L% &
	\ NEXT K% &
	\ RESTORE &
	\ GOSUB 15800 &
	\ FOR K%=0% TO 255% &
		\ READ A%,L%,T%,T1%,D$ &
		\ A$(K%)=FNFLG$(D$) &
	\ NEXT K% &
	\ DTA$(0%)=VERSION$ &
	\ DTA$(1%)=FNFLG$("FN") &
	\ DTA$(2%)=FNFLG$(" ") &
	\ DTA$(3%)=FNFLG$(" STEP ") &
	\ DTA$(4%)=FNFLG$(" TO ") &
	\ DTA$(5%)=FNFLG$("FOR ") &
	\ DTA$(6%)=FNFLG$("WHILE ") &
	\ DTA$(7%)=FNFLG$("UNTIL ") &
	\ DTA$(8%)=FNFLG$("INPUT ") &
	\ DTA$(9%)=FNFLG$("RECORD ") &
	\ DTA$(10%)=FNFLG$(" AS ") &
	\ DTA$(11%)=FNFLG$("USING ") &
	\ DTA$(12%)=FNFLG$(",") &
	\ DTA$(13%)=FNFLG$(";") &
	\ DTA$(14%)=FNFLG$(")") &
	\ DTA$(15%)=FNFLG$("=") &
	\ DTA$(16%)=FNFLG$(" THEN ") &
	\ E1$=FNFLG$(", ") &
	\ DTA$(17%)=E1$+FNFLG$("MODE ") &
	\ DTA$(18%)=E1$+FNFLG$("FILESIZE ") &
	\ DTA$(19%)=E1$+FNFLG$("CLUSTERSIZE ") &
	\ DTA$(20%)=E1$+FNFLG$("RECORDSIZE ") &
	\ DTA$(21%)=E1$+FNFLG$("COUNT ") &
	\ DTA$(22%)=E1$+FNFLG$("BLOCK ") &
	\ DTA$(23%)=FNFLG$("0%") &
	\ DTA$(24%)=FNFLG$("0.") &
	\ DTA$(25%)=FNFLG$("1%") &
	\ DTA$(26%)=FNFLG$("1.") &
	\ DTA$(27%)=E1$ &
	\ DTA$(28%)=FNFLG$("(") &
	\ DTA$(29%)=FNFLG$("'") &
	\ DTA$(30%)=FNFLG$('"') &
	\ F1% = VAL(CVT$$(A$(65%),1%)) &
	\ S$ = SYS(CHR$(8%)+JOB$+A1$+"="+A$) &
	\ SPDA% = FNW%(SP%+28%) &
	\ FLEN% = FNB%(SP%+38%) &
	\ VTBL% = SPDA% + 1214% &
	\ SPST% = FNW%(SP%+24%)-FNW%(SPDA%+2%) &
	! Scan Symbol Table and put variables in Hash array &
	\ FOR L%=65% TO 90% &
		\ X% = VTBL%+(L%-65%)*2% &
		\ I% = FNW%(X%) &
		\ WHILE I% AND -2% &
			\ V$ = CHR$(L%) &
			\ A%,X% = I%+X% &
			\ I% = FNG.N% &
			\ A% = 0% &
			\ I% = FNG.T%(A%+I%,V$) WHILE I% &
			\ I%=FNW%(X%) &
		\ NEXT &
	\ NEXT L% &
	\ A% = VAL(CVT$$(A$(L%-26%),1%)) &
	\ F1% = F1% XOR A% &
	\ GOSUB 11000 IF SLASH.V% &
	\ A% = FNIST%(-24%,"DET")+FNIST%(-60%,"ERL")+ &
		FNIST%(-26%,"ERR")+FNIST%(-58%,"LINE")+FNIST%(-30%,"NUM")+ &
		FNIST%(-34%,"NUM2")+FNIST%(-16%,"PI")+FNIST%(-54%,"RECOUNT")+ &
		FNIST%(-62%,"STATUS") &
	\ F% = ABS(SGN(F1%-F1%/7%*7%+48% - SPST%)) * 2% IF F%=0% &
	\ ADDR2%(J%)=ADDR%(J%) FOR J%=0% TO 240% &
	\ FOR J%=0% TO 240% &
		\ A$ = VAR$(J%) &
		\ VAR2$(J%)=A$ &
	\ NEXT J% &
	\ P1%(0%)=OVR% &
	\ P1%(1%)=EXTND% &
	\ P1%(2%)=LFFLG% &
	\ P1%(3%)=SLASH.N% &
	\ P1%(4%)=WIDTH% &
	\ P1%(5%)=IMAX% &
	\ P1%(6%)=F.COND% &
	\ P1%(7%)=F.FOR% &
	\ C.DEF$ = FNFLG$("DEF ") &
	\ C.DEF$ = FNFLG$("DEF* ") IF BP2% &
	\ A$(8%)=FNFLG$("CCPOS") IF BP2% &
	\ GOSUB 13000 &
	\ P1%(8%) = SCALEF% &
	\ DTA$(31%)=C.DEF$ &
	\ CLOSE 1%,9% &
	\ CHAIN S1$+"UNBAC1" LINE 2000 IF F%=0% &
	\ GOTO 32767 IF F% AND 2% &
	\ OPEN A1$ FOR OUTPUT AS FILE 1% &
	\ PRINT #1%,'10	PRINT "UNBAC - ";CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)),3%)'+ &
		',4%),DATE$(0%); &'+CHR$(10%)+CHR$(13%)+CHR$(0%)+CHR$(9%)+CHR$(9%)'"  ";TIME$(0%)' &
	\ PRINT #1%,'	\ PRINT ';"'";'Type "?" for Help';"'" &
	\ PRINT #1%,'	\ PRINT "[]"; \ GET \ FIELD RECOUNT AS A$' &
	\ PRINT #1%,'	\ DECOMPILE A$' &
	\ PRINT #1%,'	\ END' &
	\ CLOSE 1% &
	\ GOTO 32767 &

2000	  PRINT &
	\ PRINT "Command format is:  source.file=compile.file/option(s)" &
	\ PRINT 'Where:	"source.file" is for the output of the decompiler.' &
	\ PRINT '	"compiled.file" is the input to the decompiler.' &
	\ PRINT '	"option(s)" can be one or more of the following:' &
	\ PRINT '		/E - force output to BASIC-PLUS EXTEND mode'+ &
		' format.' &
	\ PRINT '		/L - Used to force Line Feed continuation,'+ &
		' even in EXTEND mode.' &
	\ PRINT '		/V - Used to change the Variable names in the'+ &
		' program.  ' &
	\ PRINT '		/N - Used to inhibit the heading that is '+ &
		'normally put' &
	\ PRINT '		     at the end of the first line in the '+ &
		' program.' &
	\ PRINT '	      /W:n - Used to set the width of output lines.'+ &
		' This number is just' &
	\ PRINT '		     a guideline for the program, it can and will'+ &
		' be exceeded' &
	\ PRINT '		     occasionally. If this option is not'+ &
		' specified, the width' &
	\ PRINT '		     will be set to 80.' &
	\ PRINT '	       /CO - Used to disable the automatic indenting'+ &
		' of Conditionals on' &
	\ PRINT '		     output (i.e. putting in a tab for each'+ &
		' level of IF/THEN' &
	\ PRINT '		     statement.)' &
	\ PRINT '		/F - Used to disable the automatic indenting'+ &
		' of FOR loops on' &
	\ PRINT '		     output (i.e. outputting a tab for '+ &
		'each level of FOR loop).' &
	\ PRINT '	      /I:n - Set the maximum depth of indenting.  If'+ &
		' this option is not' &
	\ PRINT '		     specified, then n is set to (width-30)/8.' &
	\ PRINT '		/2 - Used to output BASIC-PLUS 2 compatible '+ &
		'function' &
	\ PRINT '		     names and definitions.' &
	\ GOTO 1010 &

10000	  F1% = 0% &
	\ I% = INSTR(1%,A$,"/V") &
	\ IF I%=0% THEN SLASH.V% = 0% ELSE SLASH.V% = -1% &
			\ GOSUB 12000
10010	  I% = INSTR(1%,A$,"/L") &
	\ IF I%=0% THEN LFFLG% = 0% ELSE LFFLG% =-1% &
			\ GOSUB 12000
10020	  I% = INSTR(1%,A$,"/E") &
	\ IF I%=0% THEN EXTND% = 0% ELSE EXTND% = -1% &
			\ GOSUB 12000
10030	  I% = INSTR(1%,A$,"/N") &
	\ IF I%=0% THEN SLASH.N% = 0% ELSE SLASH.N% = -1% &
			\ GOSUB 12000
10040	  I% = INSTR(1%,A$,"/W:") &
	\ IF I%=0% THEN WIDTH%=80% ELSE WIDTH%=FNN% &
		\ IF WIDTH% < 40% THEN F1% = -1% &
			\ PRINT "?Width too small."
10050	  I% = INSTR(1%,A$,"/I:") &
	\ IF I%=0% THEN IMAX% = (WIDTH%-30%)/8% ELSE IMAX%=FNN% &
		\ IF IMAX% < 0% OR IMAX% > WIDTH%/8%+1% THEN &
			  F1% = -1% &
			\ PRINT "?Illegal Indent Max."
10060	  I% = INSTR(1%,A$,"/2") &
	\ IF I%=0% THEN BP2% = 0% ELSE BP2% = -1% &
			\ GOSUB 12000
10070	  I% = INSTR(1%,A$,"/CO") &
	\ IF I%=0% THEN F.COND% = 0% ELSE F.COND% = -1% &
			\ GOSUB 12000
10080	  I% = INSTR(1%,A$,"/F") &
	\ IF I%=0% THEN F.FOR% = 0% ELSE F.FOR% = -1% &
			\ GOSUB 12000
10090	  RETURN &

11000	  PRINT 'Type a <CR> to exit' &
	\ INPUT "Need Help <N>";A$ &
	\ A$ = LEFT(CVT$$(A$,-1%),1%) &
	\ GOTO 11010 IF A$<>"Y" &
	\ PRINT 'First Enter the Variable Name that you want to change.' &
	\ PRINT 'Then enter the new name.' &
	\ PRINT 'The only check made is to make sure that the name entered' &
	\ PRINT 'is a legal BASIC-PLUS name.  No checking is done to make' &
	\ PRINT 'sure that the new name is consistant with the old one (i.e.' &
	\ PRINT 'changing a string to an integer, an array to a single element,' &
	\ PRINT 'etc.)  The variable should have one of the following formats:' &
	\ PRINT &
	\ PRINT '    Type	      Example' &
	\ PRINT 'Simple Integer		A9%' &
	\ PRINT 'Simple Floating		A9' &
	\ PRINT 'Floating Array		A9(' &
	\ PRINT 'Integer Array		A9%(' &
	\ PRINT 'Function		FNA9%    [No Parentheses]' &
	\ PRINT 'String			A9$' &
	\ PRINT 'String Array		A9$(' &
	\ PRINT
11010	  PRINT "Old variable"; \ INPUT A$ &
	\ RETURN IF A$="" &
	\ A$ = CHR$(32%+ASCII(RIGHT(A$,3%)))+RIGHT(A$,4%) IF LEFT(A$,2%)="FN" &
	\ A$=FNFLG$(A$) &
	\ GOTO 11020 IF VAR$(I%)=A$ FOR I%=0% TO 240% &
	\ GOTO 11020 IF VAR1$(-I%) = A$ FOR I% = -OVR% TO -1% &
	\ PRINT "Variable not found. . ." &
	\ GOTO 11010
11020	  INPUT "New Variable Name";D$ &
	\ B$,D$=CVT$$(D$,39%) &
	\ B$=LEFT(B$,LEN(B$)-1%) IF RIGHT(B$,LEN(B$))="(" &
	\ C$ = RIGHT(B$,LEN(B$)) &
	\ B$ = LEFT(B$,LEN(B$)-1%) IF C$="%" OR C$="$" &
	\ B$ = RIGHT(B$,3%) IF LEFT(B$,2%)="FN" &
	\ GOTO 11099 IF B$="" &
	\ GOTO 11099 IF ASCII(B$)<65% OR ASCII(B$)>64%+26% &
	\ GOTO 11099 IF INSTR(1%,"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.", &
		MID(B$,J%,1%)) = 0% FOR J%=2% TO LEN(B$) &
	\ GOTO 11099 IF LEN(B$)>29% &
	\ IF LEFT(D$,2%)="FN" THEN D$ = CHR$(32%+ASCII(RIGHT(D$,3%)))+ &
		RIGHT(D$,4%)
11030	  IF I%>=0% THEN VAR$(I%)=FNFLG$(D$) ELSE VAR1$(-I%)=FNFLG$(D$)
11040	IF EXTND%=0% THEN &
		IF LEN(D$)>2% THEN EXTND% = -1% ELSE &
		IF LEN(D$)=2% THEN EXTND% = -1% IF INSTR(1%,"0123456789", &
			RIGHT(D$,2%)) = 0%
11050	  GOTO 11010
11099	  PRINT "Illegal Variable Name!" &
	\ GOTO 11010 &
	&

12000	  I1% = INSTR(I%+1%,A$,"/") &
	\ I1% = LEN(A$)+1% IF I1%=0% &
	\ A$ = LEFT(A$,I%-1%)+RIGHT(A$,I1%) &
	\ RETURN
12010	  DEF* FNN% &
	\ I1% = INSTR(I%+1%,A$,"/") &
	\ I1% = LEN(A$)+1% IF I1%=0% &
	\ FNN% = VAL(MID(A$,I%+3%,I1%-4%))
12020	  A$ = LEFT(A$,I%-1%)+RIGHT(A$,I1%) &
	\ FNEND
13000	  RETURN IF SCALEF% >=0% AND SCALEF% < 7% &
	\ PRINT "%Program has an unusual Scale Factor";SCALEF% &
	\ PRINT " Resetting to 0" &
	\ SCALEF% = 0% &
	\ RETURN &
	&
	&

15000	  DEF* FNERRTXT$(E%)=CVT$$(RIGHT(SYS(CHR$(6%)+CHR$(9%)+ &
		CHR$(E%)),3%),4%) &

15100	! Look up address A% in hash file.  Returns position of A% if found, &
	! and position to put new address in if not found.  A negative position &
	! means that hash table is full, and the position points to the &
	! overflow area.  (See FNIST%) &
	  DEF* FNSRCH%(A%) &
		\ I% = (A% XOR SWAP%(A%)) AND 32767% &
		\ H1%,S1%=I%-I%/241%*241% &
		\ R1%=I%-I%/239%*239%+1%
15110		  I% = ADDR%(H1%) &
		\ IF I%=0% OR I%=A% THEN FNSRCH%=H1% &
			\ GOTO 15140
15120		  H1% = H1%-R1% &
		\ H1%=H1%+241% IF H1%<0% &
		\ GOTO 15110 IF H1%<>S1% &
		\ PRINT "%Variable Table full.  Will use overflow." &
			IF OVR%=0% &
		\ FOR I%=1% TO OVR% &
			\ IF ADDR1%(I%) = A% THEN FNSRCH%=-I% &
				\ GOTO 15140
15130		  NEXT I% &
		\ FNSRCH% = -OVR%-1% &
		\ ADDR1%(OVR%+1%)=0%
15140	  FNEND &

15200	! Insert variable V$ (referenced by address A%) into hash table. &
	  DEF* FNIST%(A%,E$) &
	\ I%=FNSRCH%(A%) &
	\ SPST% = ASCII(A$(L%+165%))-128% &
	\ IF I%>=0% THEN &
		  IF ADDR%(I%)=0% THEN 15220 &
		  ELSE PRINT "%Multiple Definition";E$,VAR$(I%) IF F%=0% &
	  ELSE &
	  IF ADDR1%(-I%) THEN PRINT "%Ovr Multiple Definition";E$,VAR1$(-I%) &
		IF F%=0% &
	  ELSE OVR% = -I% &
		\ ADDR1%(OVR%)=A% &
		\ VAR1$(OVR%)=FNFLG$(E$)
15210	  GOTO 15230
15220	  VAR$(I%)=FNFLG$(E$) &
	\ ADDR%(I%)=A%
15230	  FNEND &

15300	! Gets a variable name from symbol table at address A% &
	  DEF* FNG.N%
15310	  A% = A% - 1% &
	\ I% = FNB%(A%) &
	\ IF (I% AND 128%)=0% THEN &
		  V$ = V$ + CHR$(I%) IF I% &
		\ F% = (I%-10%)/2%+1% IF I% AND I%<32% IF F%=0% &
		\ GOTO 15310
15320	  IF EXTND%=0% THEN &
		  IF LEN(V$)>2% THEN EXTND%=-1% ELSE &
		  IF LEN(V$)=2% THEN EXTND%=-1% IF INSTR(1%, &
			"0123456789",RIGHT(V$,2%)) = 0%
15330	  FNG.N% = A% AND -2% &
	\ FNEND &

15400	! Classifies variable name V$ with flag at address X% return next &
	! link to different type (same name), and inserts variable into hash &
	! table &
	  DEF* FNG.T%(X%,V$) &
	\ A% = X% - 2% &
	\ FNG.T% = FNW%(A%) &
	\ I% = FNB%(X%) &
	\ IF I% AND 8% THEN X%=26% ELSE &
	  IF I% AND 1% THEN X%=2%  ELSE &
	  IF I% AND 4% THEN X%=6%  ELSE &
			    X%=FLEN%*2%
15410	  V$ = V$ + "%" IF I% AND 1% &
	\ V$ = V$ + "$" IF I% AND 4% &
	\ V$ = V$ + "(" IF I% AND 8% &
	\ IF I% AND 16% THEN V$ = CHR$(32%+ASCII(V$))+RIGHT(V$,2%)
15420	  X% = FNIST%(A%-X%-SPDA%,V$) &
	\ FNEND &

15500	  DEF* FNW%(A%) = P%(A%/2%-256%) &
	\ DEF* FNB%(A%) &
	\ IF A% AND 1% THEN FNB%=SWAP%(FNW%(A%)) AND 255% &
	ELSE                FNB%=      FNW%(A%)  AND 255%
15510	  FNEND &

15600	  DEF* FNFLG$(A$)=LEFT(CHR$(128% OR ASCII(A$))+RIGHT(A$,2%),LEN(A$)) &

15700	DEF* FNINM$ \ READ CE% \ CE$="" \ &
	FOR CE1%=1% TO CE% \ &
		READ CE2% \ &
		CE$=CHR$(CE2%+CE1%)+CE$ \ &
	NEXT CE1% \ &
	CE1%=(CE%/2%) \ &
	CE2%=CE%-CE1% \ &
	FNINM$=MID(CE$,CE2%+1%,CE1%)+LEFT(CE$,CE2%) \ &
	FNEND
15800	READ CE% \ READ CE1% FOR CE2%=1% TO CE% \ RETURN
18000	! START OF DATA TO PASS TO PART 2 &
	DATA &
		54,77,77,70,80, &
		60,61,66,68,71, &
		70,54,20,69,55, &
		69,69,63,59,60, &
		47,11,55,56,60, &
		58,59,40,28,20, &
		19,25,19,-1,44, &
		49,-4,7,45,34, &
		32,39,35,26,33, &
		-13,-1,-15,-2,18, &
		28,22,-20,-9,29, &
		  0,1,9,2,9, &
		  1,1,9,2,"", &
		  2,1,9,2,"", &
		  3,1,5,1,"STOP", &
		  4,1,4,0,"", &
		  5,1,4,0,"", &
		  6,1,2,1,SYS, &
		  7,1,2,1,TAB, &
		  8,1,2,1,POS, &
		  9,1,2,1,CHR$, &
		 10,1,5,11,"", &
		 11,1,2,1,ASCII, &
		 12,1,2,1,DATE$, &
		 13,1,2,1,TIME$, &
		 14,1,2,1,TIME, &
		 15,1,2,1,PEEK, &
		 16,1,5,2,"SLEEP ", &
		 17,1,5,2,"WAIT ", &
		 18,3,0,1,"", &
		 19,1,5,1,RANDOMIZE, &
		 20,1,5,1,RESTORE, &
		 21,3,5,3,"RESUME ", &
		 22,0,5,6,"GOSUB ", &
		 23,3,5,12,CHANGE, &
		 24,3,5,12,CHANGE, &
		 25,3,5,7,"MAT READ ", &
		 26,3,5,7,"MAT PRINT ", &
		 27,3,5,7,"MAT PRINT ", &
		 28,3,5,7,"MAT INPUT ", &
		 29,3,5,7,"ZER", &
		 30,3,5,7,"CON", &
		 31,3,5,7,"IDN", &
		 32,5,5,7,TRN(, &
		 33,5,5,7,INV(, &
		 34,5,5,7,*, &
		 35,5,5,7,*, &
		 36,5,5,7,"", &
		 37,7,5,7,*, &
		 38,7,5,7,+, &
		 39,7,5,7,-, &
		 40,1,5,8," ", &
		 41,1,5,8," FOR INPUT ", &
		 42,1,5,8," FOR OUTPUT ", &
		 43,1,5,4,NAME, &
		 44,1,5,2,"KILL ", &
		 45,1,9,0,14, &
		 46,1,5,5,"CHAIN ", &
		 47,1,2,1,LEFT, &
		 48,0,5,6,"GOTO ", &
		 49,3,5,3,"ON ERROR GOTO ", &
		 50,1,6,4,"", &
		 51,3,2,2,"", &
		 52,1,9,0,"", &
		 53,1,9,0,"", &
		 54,4,5,11,"", &
		 55,1,5,1,END, &
		 56,3,5,7,"MAT PRINT ", &
		 57,1,4,0,"", &
		 58,1,4,0,"", &
		 59,1,4,0,"", &
		 60,1,4,0,"", &
		 61,1,4,0,"", &
		 62,1,4,0,"", &
		 63,1,1,50,==, &
		 64,1,9,8,81, &
		 65,1,9,8,07, &
		 66,1,9,8,"", &
		 67,1,9,8,98, &
		 68,1,4,0,"", &
		 69,1,4,0,"", &
		 70,1,4,0,"", &
		 71,1,4,0,"", &
		 72,1,4,0,"", &
		 73,1,4,0,"", &
		 74,1,4,0,"", &
		 75,1,4,0,"", &
		 76,1,4,0,"", &
		 77,1,0,0,"0.", &
		 78,1,0,0,"1.", &
		 79,3,4,0,"", &
		 80,3,4,0,"", &
		 81,1,0,0,"1%", &
		 82,1,4,0,"", &
		 83,1,4,0,"", &
		 84,1,4,0,"", &
		 85,1,4,0,"", &
		 86,1,4,0,"", &
		 87,1,7,1,"", &
		 88,3,7,1,"", &
		 89,1,2,1,CVT%$, &
		 90,1,2,1,CVTF$, &
		 91,1,2,1,CVT$%, &
		 92,1,2,1,CVT$F, &
		 93,3,3,0,"LSET ", &
		 94,3,3,2,"LSET ", &
		 95,3,3,4,"LSET ", &
		 96,3,3,0,"RSET ", &
		 97,3,3,2,"RSET ", &
		 98,3,3,4,"RSET ", &
		 99,3,9,5,"", &
		100,3,9,6,"", &
		101,3,9,7,"", &
		102,2,5,9,"PUT #", &
		103,2,5,9,"GET #", &
		104,1,3,1,"", &
		105,1,2,1,MAGTAPE, &
		106,1,5,2,"UNLOCK #", &
		107,1,2,1,XLATE, &
		108,1,2,2,"", &
		109,1,2,1,CVT$$, &
		110,1,9,2,"", &
		111,1,2,1,STRING$, &
		112,1,2,1,BUFSIZ, &
		113,1,2,1,SUM$, &
		114,1,2,1,DIF$, &
		115,1,2,1,PROD$, &
		116,1,2,1,QUO$, &
		117,1,2,1,COMP%, &
		118,1,2,1,PLACE$, &
		119,1,2,1,NUM1$, &
		120,3,0,2,"", &
		121,1,2,0,"", &
		122,1,2,1,SPEC%, &
		123,1,2,0,"", &
		124,1,4,0,"", &
		125,1,4,0,"", &
		126,1,4,0,"", &
		127,1,4,0,"", &
		128,1,1,60,+, &
		129,1,1,60,+, &
		130,1,1,60,-, &
		131,1,1,60,-, &
		132,1,4,0,"", &
		133,1,4,0,"", &
		134,1,1,70,*, &
		135,1,1,70,*, &
		136,1,1,70,/, &
		137,1,1,70,/, &
		138,1,2,1,RAD$, &
		139,1,2,1,SWAP%, &
		140,1,1,80,^, &
		141,1,1,80,^, &
		142,1,1,50,=, &
		143,1,1,50,=, &
		144,1,1,50,=, &
		145,1,1,50,==, &
		146,1,1,50,>, &
		147,1,1,50,>, &
		148,1,1,50,>, &
		149,1,1,50,>=, &
		150,1,1,50,>=, &
		151,1,1,50,>=, &
		152,1,1,50,<, &
		153,1,1,50,<, &
		154,1,1,50,<, &
		155,1,1,50,<=, &
		156,1,1,50,<=, &
		157,1,1,50,<=, &
		158,1,1,50,<>, &
		159,1,1,50,<>, &
		160,1,1,50,<>, &
		161,1,1,91,-, &
		162,1,1,91,-, &
		163,3,0,3,"", &
		164,3,0,4,"", &
		165,3,0,5,"", &
		166,3,3,0,"", &
		167,3,3,0,"", &
		168,3,3,0,"", &
		169,3,3,1,"", &
		170,3,3,1,"", &
		171,3,3,1,"", &
		172,3,0,6,"", &
		173,3,0,7,"", &
		174,3,3,2,"", &
		175,3,3,4,"", &
		176,3,3,3,"", &
		177,3,3,5,"", &
		178,3,3,0,"", &
		179,3,3,0,"", &
		180,3,3,1,"", &
		181,3,3,1,"", &
		182,3,3,2,"", &
		183,3,3,2,"", &
		184,3,3,3,"", &
		185,3,3,3,"", &
		186,3,3,4,"", &
		187,3,3,4,"", &
		188,3,3,5,"", &
		189,3,3,5,"", &
		190,1,0,0,0%, &
		191,1,2,0,"", &
		192,1,2,0,"", &
		193,1,2,0,"", &
		194,1,2,1,SIN, &
		195,1,2,1,COS, &
		196,1,2,1,ATN, &
		197,1,2,1,SQR, &
		198,1,2,1,EXP, &
		199,1,2,1,LOG, &
		200,1,2,1,LOG10, &
		201,1,2,1,RND, &
		202,1,2,1,FIX, &
		203,1,2,1,TAN, &
		204,1,2,1,LEN, &
		205,1,2,1,SPACE$, &
		206,1,2,1,RIGHT, &
		207,1,2,1,MID, &
		208,1,2,1,INSTR, &
		209,1,2,1,NUM$, &
		210,1,2,1,VAL, &
		211,1,2,1,SGN, &
		212,1,2,1,INT, &
		213,1,2,1,ABS, &
		214,3,7,2,"", &
		215,3,5,3,"GOTO ", &
		216,3,7,2,"IF ", &
		217,3,4,0,"", &
		218,3,7,2,"UNLESS ", &
		219,1,4,0,"", &
		220,0,5,10,DEF, &
		221,1,5,1,FNEND, &
		222,3,5,3,"GOSUB ", &
		223,1,5,1,RETURN, &
		224,1,1,40,"NOT ", &
		225,1,1,30," AND ", &
		226,1,1,20," OR ", &
		227,1,1,20," XOR ", &
		228,1,1,10," IMP ", &
		229,1,1,10," EQV ", &
		230,1,8,0,"", &
		231,1,8,0,"", &
		232,1,8,0,"", &
		233,1,1,60,+, &
		234,1,6,4,"", &
		235,7,6,1,"", &
		236,7,6,3,"", &
		237,7,6,1,"", &
		238,7,6,3,"", &
		239,7,6,0,"", &
		240,7,6,2,"", &
		241,7,6,0,"", &
		242,7,6,2,"", &
		243,7,6,4,"", &
		244,7,6,4,"", &
		245,7,6,5,NEXT, &
		246,7,6,5,NEXT, &
		247,1,5,2,"CLOSE ", &
		248,1,9,0,"", &
		249,1,9,0,"", &
		250,1,9,4,"", &
		251,1,9,3,"", &
		252,1,9,3,"", &
		253,1,9,3,"", &
		254,1,9,2,"", &
		255,1,9,2,2 &

19000	  IF ERL=1040% OR ERL=1030% THEN PRINT "?Error with ";A$; &
		" - ";FNERRTXT$(ERR) &
		\ RESUME 1010
19010	  IF ERL=1050% THEN RESUME 1060 IF ERR=5% &
		\ PRINT "?Error on checking for source - ";FNERRTXT$(ERR) &
		\ RESUME 1010
19020	  IF ERL=1060% THEN KILL "DECM"+JOB$+".TMP" &
		\ PRINT "?Can't chain to part 2 of this program." &
		\ PRINT "?It must be on the same disk and account" &
		\ PRINT " as this program  (";S1$;")." &
		\ RESUME 32767
19030	  IF ERR=11% AND ERL=1010% THEN RESUME 32767
19040	  IF ERL=12010% THEN PRINT "?ILLEGAL NUMBER for ";MID(A$,I%,I1%-I%) &
		\ F% = -1% &
		\ RESUME 12020
19999	PRINT FNERRTXT$(ERR);" at line ";NUM1$(ERL) &
	\ V$ = SYS(CHR$(9%)) &
	&
	&

32767 END
