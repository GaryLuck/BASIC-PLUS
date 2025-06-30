1	! UNBAC1 - Part 2 of the decompiler for BASIC-PLUS &
	! Copyright @1979 by Bill Lortz &
	! &

800	  EXTEND &
	\ DIM #1%,A%(255%),A$(255%)=16%,ADDR2%(240%),VAR2$(240%)=32%, &
		P1%(8%),DTA$(31%)=16%,ADDR3%(4096%),VAR3$(4096%)=32% &
	\ DIM TYP%(257%),O.LEN%(255%),DSC$(257%) &
	\ DIM #3%,P%(32767%) &
	\ DIM #1%,STACK%(32767%) &
	\ DIM #4%,STACK$(32767%)=32% &
	\ DIM     ADDR%(240%) &
	\ DIM #6%,VAR$(240%)=32%,ADDR1%(4096%),VAR1$(4096%)=32% &
	\ DIM #7%,STMT%(32767%) &

1000	  CHAIN "$DECOM"
2000	  I$ = SYS('7'C) &
	\ PRINT ""; &
	\ JOB$ = "DECM"+LEFT(I$,2%)+".TMP" &
	\ I$ = RIGHT(I$,3%) &
	\ OPEN JOB$ FOR INPUT AS FILE 1% &
	\ KILL JOB$ &
	\ VERSION$=DTA$(0%) &
	\ I% = INSTR(1%,I$,"=") &
	\ S.FIL$ = LEFT(I$,I%-1%) &
	\ C.FIL$ = RIGHT(I$,I%+1%) &
	\ TYP%(J%)=(A%(J%) AND 255%)/16%*256% + (SWAP%(A%(J%)) AND 255%) &
		FOR J%=0% TO 255% &
	\ O.LEN%(J%)=A%(J%) AND 15% FOR J%=0% TO 255% &
	\ DSC$(J%)=A$(J%) FOR J%=0% TO 255% &
	\ OVR% = P1%(0%) &
	\ EXTND% = P1%(1%) &
	\ LFFLG% = P1%(2%) &
	\ SLASH.N% = P1%(3%) &
	\ WIDTH% = P1%(4%) &
	\ IMAX% = P1%(5%) &
	\ F.COND% = P1%(6%) &
	\ F.FOR% = P1%(7%) &
	\ SCALEF% = P1%(8%) &
	\ C.FN$=DTA$(1%) &
	\ C.SP$=DTA$(2%) &
	\ C.STEP$=DTA$(3%) &
	\ C.TO$=DTA$(4%) &
	\ C.FOR$=DTA$(5%) &
	\ C.WHILE$=DTA$(6%) &
	\ C.UNTIL$=DTA$(7%) &
	\ C.INPUT$=DTA$(8%) &
	\ C.RECORD$=DTA$(9%) &
	\ C.AS$=DTA$(10%) &
	\ C.USING$=DTA$(11%) &
	\ C.COMMA$=DTA$(12%) &
	\ C.SEMI$=DTA$(13%) &
	\ C.RPAR$=DTA$(14%) &
	\ C.EQU$=DTA$(15%) &
	\ C.MODE$=DTA$(17%) &
	\ C.SIZE$=DTA$(18%) &
	\ C.CLUST$=DTA$(19%) &
	\ C.RSIZE$=DTA$(20%) &
	\ C.COUNT$=DTA$(21%) &
	\ C.BLOCK$=DTA$(22%) &
	\ C.0$=DTA$(23%) &
		\ C.COMMA.SP$=DTA$(27%) &
	\ C.LPAR$=DTA$(28%) &
	\ C.SQTE$=DTA$(29%) &
	\ C.DQTE$=DTA$(30%) &
	\ C.DEF$=DTA$(31%) &
	\ OPEN JOB$ FOR OUTPUT AS FILE 6% &
	\ ADDR%(J%)=ADDR2%(J%) FOR J%=0% TO 240% &
	\ VAR$(J%)=VAR2$(J%) FOR J%=0% TO 240% &
	\ ADDR1%(J%)=ADDR3%(J%) FOR J%=1% TO OVR% &
	\ VAR1$(J%)=VAR3$(J%) FOR J%=1% TO OVR% &
	\ VAR1$(OVR%+1%)="" &
	\ CLOSE 1% &
	\ N$="" ! Define null string to save space. &
	\ DSC$(256%)=C.LPAR$ \ TYP%(256%)=256%+3% ! Define internal op code 256 &
	\ DSC$(257%)=N$  \ TYP%(257%)=256%+1% ! Define internal op code 257 &
	\ OPEN JOB$ FOR OUTPUT AS FILE 1% &
	\ OPEN JOB$ FOR OUTPUT AS FILE 4% &
	\ OPEN JOB$ FOR OUTPUT AS FILE 7% &
	\ KILL JOB$ &
	\ OPEN S.FIL$ FOR OUTPUT AS FILE 2% &
	\ OPEN C.FIL$ FOR INPUT AS FILE 3% &
	\ SP%   = FNW%(514%) &
	\ Q%    = FNB%(SP% + 1%) &
	\ SPDA% = FNW%(SP%+28%) &
	\ SPTA% = FNW%(SP%+30%) &
	\ SCTH% = SPTA% &
	\ FLEN% = FNB%(SP%+38%) &
	\ SPST% = FNW%(SP%+24%)-FNW%(SPDA%+2%) &
	\ S2%(0%)=-1% &
	\ NEWLINE%=1% \ INLINE%=2% \ DONE%=3% ! State constants &
	\ L.STATE%=DONE% &
	\ LPTR%=SCTH% &
	\ EPTR%=0% &
	\ E.TRM$='13'C+'10'C &
	\ C.TRM$='10'C+'13'C+'0'C &
	\ C.TRM$=" &"+E.TRM$ IF EXTND% AND LFFLG%=0% &
	\ WIDTH% = WIDTH% - 2% IF LEFT(C.TRM$,2%)=" &" &
	\ FFIL$ = STRING$(LEN(CVTF$(0.))-2%,0%) &
	\ SCALEF. = 10. ^ SCALEF% &
	&

2100	! New line header dispatch area. &
	  I% = FNW%(LPTR%) &
	\ IF I% THEN &
		  LPTR% = LPTR% + I% &
		\ L% = FNW%(LPTR%+10%) &
		\ T% = FNB%(LPTR%+9% ) &
		\ PRINT #2%,CVT$$(TERM$,1%); \ TERM$=N$ &
		\ A% = FNCLEAN.UP% IF T%<>7% IF EPTR% &
		\ A% = FNNEW.LINE% IF L%<>LASLIN% &
		\ LASLIN%=L% &
		\ BCOD%=FNW%(LPTR%+2%)+LPTR% &
		\ ECOD%=BCOD%+FNW%(LPTR%+4%) &
		\ GOTO 2500 IF T%=7% &
		\ GOTO 2300 IF T%=1% &
		\ GOTO 2400 IF T%=4% &
		\ GOTO 2500
2110	! Finish up processing. &
	  PRINT #2%,CVT$$(TERM$,1%); \ TERM$=N$ &
	\ A% = FNCLEAN.UP% &
	\ PRINT #2%,"!"; IF L.STATE%=NEWLINE% &
	\ PRINT #2% &
	\ CLOSE 2% &
	\ V$ = SYS('9'C) &
	\ STOP &
	&
	&
2200	! Handles the termination of statement modifiers &
	  IF S%=0% THEN 2250 &
		ELSE IF S%(S%)<>BCOD% THEN 2500 &
			ELSE IF S2%(S%)=0% THEN S% = S% - 1% &
				\ STFLG%=0% &
				\ GOTO 2200
2205	  OP% = FNB%(STMT%) &
	\ GOTO 2270 IF OP%=87% &
	\ IF OP%<>214% THEN S% = S% - 1% &
		\ GOTO 2200
2210	  L% = FNR%(STMT% + 1%) + BCOD% &
	\ IF L%<>S1%(S%) THEN S% = S% - 1% &
			\ GOTO 2200
2220	  L% = S2%(S%) &
	\ S% = S% - 1% &
	\ IF L%<0% THEN MODIF$ = LEFT(MODIF$,LEN(MODIF$)+L%)+C.UNTIL$+ &
				 RIGHT(MODIF$,LEN(MODIF$)+L%+8%) &
	ELSE   MODIF$ = LEFT(MODIF$,LEN(MODIF$)-L%)+C.WHILE$+RIGHT(MODIF$, &
				LEN(MODIF$)-L%+4%)
2240	  GOTO 2200
2250	  S% = 1% &
	\ STFLG%=0% &
	\ A% = FNPRINT%(FNARG$) IF EPTR% &
	\ GOSUB 13300 &
	\ A% = FNFORMAT%(O$) &
	\ A% = FNFORMAT%(TERM$+MODIF$) &
	\ MODIF$,TERM$ = N$ &
	\ S%=0% &
	\ GOTO 2500
2270	  A% = FNPRINT%(FNARG$) IF EPTR% &
	\ STFLG% = 0% &
	\ GOSUB 13300 &
	\ I% = S2%(S%) \ I%=SGN(I%)*I% IF S% &
	\ L% = S2%(S%-1%) \ L%=SGN(L%)*L% IF S%-1% &
	\ E$=FNPUSH$(N$) IF IFTHEN%=0% &
	\ C% = -S% * (NOT F.COND%) &
	\ C% = IMAX% - LEN(FNINDENT$) + 1% IF C% + LEN(FNINDENT$) - 1%>IMAX% &
	\ O$,TERM$=FNPUSH$(MID(MODIF$,LEN(MODIF$)-I%+1%,I%-L%-1%)+ &
		C.SP$+"THEN"+C.TRM$+FNINDENT$+STRING$(C%,9%)+"  "+FNPOP$+ &
		O$+TERM$+LEFT(MODIF$,LEN(MODIF$)-I%-1%)+C.TRM$+FNINDENT$+ &
		STRING$(C%-1%,9%)+"  ELSE"+C.TRM$+FNINDENT$+STRING$(C%,9%)+"  ") &
	\ S% = S% - 1% &
	\ MODIF$ = RIGHT(MODIF$,LEN(MODIF$)-L%) &
	\ IFTHEN%=IFTHEN%+1% &
	\ IF S%=0% THEN &
		  A% = FNFORMAT%(FNPOP$) &
		\ IFTHEN%=0% &
		\ L.STATE%=NEWLINE% &
		\ I.IND% = I.IND% + 1%
2280	  GOTO 2500 &
	&

2300	! Process DIM statements &
	  GOSUB 13010 &
	\ L.STATE%=NEWLINE% &
	\ A%=FNPRINT%("DIM ") &
	\ C%=0% &
	\ FOR L%=BCOD%+2% TO ECOD%-3% STEP 2% &
		\ T% = FNR%(L%) &
		\ A$=FNVAR$(T%) &
		\ T% = T% + SPDA% &
		\ I%=FNB%(T%+10%) &
		\ IF I%<>C% THEN PRINT #2%,C.TRM$+FNINDENT$+"\ DIM "; &
				IF L.STATE%<>NEWLINE% &
			\ PRINT #2%,"#"+NUM1$(I%/2%)+"%"; IF I% &
			\ C%=I% &
			\ L.STATE%=INLINE%
2310	  	  PRINT #2%,", "; IF L.STATE%=INLINE% &
		\ L.STATE%=INLINE% &
		\ E$=A$+NUM1$(FNW%(T%+22%))+"%" &
		\ E$=E$+","+NUM1$(FNW%(T%+24%))+"%" IF FNW%(T%+24%) &
		\ E$=E$+")" &
		\ E$=E$+"="+NUM1$(2%*FNW%(T%+20%))+"%" &
			IF C% IF FNB%(T%+28%) AND 4% &
		\ PRINT #2%,C.TRM$+FNINDENT$+'9'C; &
			IF CCPOS(2%)+LEN(E$)>=WIDTH% &
		\ PRINT #2%,CVT$$(E$,1%); &
	\ NEXT L% &
	\ L.STATE%=INLINE% &
	\ GOTO 2100 &
	&

2400	! DATA statement processing routine &
	  GOSUB 13000 &
	\ PRINT #2%,"DATA"+'9'C; &
	\ C%=0% &
	\ E$=N$ &
	\ FOR J%=BCOD%+1% TO ECOD% - 1% &
		\  I% = FNB%(J%) &
		\ E$ = E$ + CHR$(I%) &
		\ IF I%=34% THEN &
			IF C%=0% THEN C%=2% ELSE C%=0% IF C%=2% ELSE &
		  IF I%=39% THEN &
			IF C%=0% THEN C%=1% ELSE C%=0% IF C%=1% ELSE &
		  IF I%=44% AND C%=0% THEN &
			  PRINT #2%,C.TRM$;FNINDENT$;'9'C; &
				IF CCPOS(2%)+LEN(E$)>=WIDTH% &
			\ PRINT #2%,CVT$$(E$,1%); &
			\ E$=N$
2410	  NEXT J% &
	\ PRINT #2%,CVT$$(E$,1%); &
	\ GOTO 2100 &

2500	! "Ordinary" statement dispatch area &
	  OP% = FNSCAN%(3%) &
	\ GOTO 2100 IF OP%=-1% &
	\ GOTO 2200 IF OP%=-2% ! At branch address on top of stack &
	\ T% = TYP%(OP%) &
	\ ST% = T% AND 255% &
	\ T%  = T%/256% &
	\ STFLG%=0% IF T%<>8% AND T%<>9% AND OP%<>28% &
	\ IF T%=4% THEN PRINT "%Undefined OP Code";OP%;"in line";LASLIN%
2510	  IF T%<>5% THEN ON T%-5% GOTO 2600,2700,2800,2900 ELSE &
	  A% = FNPRINT%(TERM$) IF STFLG% AND 4% &
	\ IF (STFLG% AND NOT 260%)=0% THEN STFLG% = STFLG% AND 256% &
		\ IF OP%=28% OR STFLG%=0% THEN &
		 	 ON ST% GOTO 10000,10100,10200,10300,10400,10500,10600, &
				      10700,10800,10900,11000,11100,11200
2520	  A% = FNERROR%(2520%,CVT%$(STFLG%)) &
	\ STFLG% = 0% &
	\ GOTO 2510 &

2600	! For loop processing area &
	  GOTO 2500 IF OP%=50% ! Ignore these critters &
	\ IF ST%=4% OR ST%=5% THEN &
		  A% =FNCLEAN.UP% IF EPTR% &
		\ GOTO 2500 IF ST%=4% &
		\ F.IND% = F.IND% - 1% &
		\ GOSUB 13010 &
		\ A%=FNPRINT%(DSC$(245%)) &
		\ I%=FNR%(STMT%+1%) &
		\ A%=FNPRINT%(C.SP$+FNVAR$(I%)) IF I% &
		\ GOTO 2500
2610	  S$ = N$ &
	\ I% = FNR%(STMT%+1%) &
	\ IF I% THEN &
		  S$ = C.STEP$+RIGHT(FNARG$,2%) IF (FNR%(STMT%+3%) AND 1%)=0% &
		\ S$ = C.TO$+RIGHT(FNARG$,2%)+S$ IF (ST% AND 2%)=0% &
		\ S$ = C.FOR$+FNARG$+S$
2620	  A% = FNCLEAN.UP% IF EPTR% &
	\ L% = STMT% &
	\ IF ST% AND 2% THEN A%=FNSCAN%(3%) &
		\ GOTO 2680 IF A%<>234% &
		\ IF FNB%(STMT%(EPTR%))<>121% THEN &
		    S$ = S$ + SPACE$(SGN(LEN(S$))) + C.WHILE$ + FNARG1$(N$) ELSE &
		  EPTR% = EPTR% - 1% &
		\ S$ = S$ + SPACE$(SGN(LEN(S$)))+C.UNTIL$+FNARG1$(N$)
2630	  IF ST% AND 1% THEN MODIF$ = C.SP$ + S$ + MODIF$ ELSE &
		  GOSUB 13010 &
		\ A%=FNPRINT%(S$) &
		\ F.IND% = F.IND% + 1% &
		\ GOTO 2500
2640	  GOSUB 13000 IF S%=0% &
	\ S% = S% + 1% &
	\ S%(S%)=FNR%(L%+5%)+BCOD% &
	\ S1%(S%)=L% &
	\ S2%(S%)=0% &
	\ GOTO 2500 &

2680	  A% = FNERROR%(2680%,CVT%$(A%)) ! Expected op 234, got an A% &
	\ GOTO 2500 &

2700	! Conditionals (IF THEN/UNLESS THEN/IF/UNLESS/WHILE/UNTIL) processing &
	  GOTO 2730 IF ST%<>1% &
	\ IF OP%=87% THEN A% = FNERROR%(2700%,N$) ! Unexpected else &
		\ GOTO 2500
2710	  IF FNB%(STMT%(EPTR%))<>121% THEN S$=DSC$(216%) + FNARG1$(N$)+" THEN " &
					 ELSE EPTR% = EPTR% - 1% &
					    \ S$=DSC$(218%)+FNARG1$(N$)+" THEN "
2720	  A% = FNCLEAN.UP% &
	\ GOSUB 13010 &
	\ L.STATE%=NEWLINE% &
	\ I.IND% = I.IND% + 1% &
	\ A%=FNPRINT%(S$+C.TRM$+FNINDENT$+"  ") &
	\ GOTO 2500
2730	  IF OP%=214% THEN A% = FNERROR%(2730%,CVT%$(STMT%)+CVT%$(S%(S%))) &
		\ GOTO 2500 ! Unexpected branch in line.
2735	  IF FNB%(STMT%(EPTR%))=121% THEN EPTR% = EPTR% - 1% &
		\ OP% = 218%
2740	  S$ = FNARG1$(N$) &
	\ A%=FNPUSH%(STMT%(EPTR%+1%))+FNCLEAN.UP% &
	\ GOSUB 13000 &
	\ S% = S% + 1% &
	\ S%(S%)=FNR%(STMT%+1%)+BCOD% &
	\ S1%(S%)=FNPOP% &
	\ IF OP%=216% THEN MODIF$ = C.SP$+DSC$(216%)+S$+MODIF$ &
		\ S2%(S%)=LEN(MODIF$)-1% &
		\ GOTO 2500
2750	  MODIF$ = C.SP$+DSC$(218%)+S$+MODIF$ &
	\ S2%(S%)=-LEN(MODIF$)+1% &
	\ GOTO 2500 &

2800	! Process READ statements &
	  IF STFLG% AND 1% THEN A%=FNPRINT%(C.COMMA.SP$) &
			   ELSE   GOSUB 13000 &
				\ A%=FNPRINT%("READ ") &
				\ STFLG%=1%
2810	  A% = FNSCAN%(2%) &
	\ GOTO 2830 IF A%<0% &
	\ EPTR% = EPTR% + 1% &
	\ STMT%(EPTR%)=STMT% &
	\ T% = TYP%(A%) &
	\ ST% = T% AND 255% &
	\ T% = T%/256% &
	\ GOTO 2830 IF T%<>3% OR ST% AND 1% &
	\ S$=FNARG$ &
	\ IF S$=N$ THEN &
		  A% = FNERROR%(2810%,N$) ! Wierd expressiong &
		\ A%=FNPRINT%(S$) &
		\ GOTO 2500
2820	  A%=FNPRINT%(LEFT(S$,LEN(S$)-1%)) &
	\ GOTO 2500
2830	  A% = FNERROR%(2830%,CVT%$(A%)) ! Illegal Op Code A% &
	\ GOTO 2500 &
	&

2900	! PRINT/PRINT USING/INPUT/INPUT LINE/FIELD &
	  STFLG% = STFLG% AND -2% &
	\ IF ST%=0% THEN EPTR%=EPTR%+1% &
		\ STMT%(EPTR%)=STMT% &
		\ A%=FNPRINT%(TERM$) IF STFLG% AND 4% &
		\ TERM$,TERM1$=N$  IF STFLG% AND 4% &
		\ STFLG% = STFLG% AND NOT 4% &
		\ GOTO 2500 IF OP%<>53% &
		\ EPTR% = EPTR% - 1% &
		\ STFLG% = 0% &
		\ GOTO 2500 &

2910	  GOTO 2970 IF STFLG% &
	\ IF ST%=3% OR ST%=4% THEN &
		  GOTO 3030 IF FNCHECK%(EPTR%,248%) &
		\ GOTO 3030 IF EPTR%<7%-ST% &
		\ GOTO 3030 IF FNCHECK%(EPTR%-1%,52%) IF ST%=3% &
		\ GOTO 3030 IF FNCHECK%(EPTR%-1%,45%) &
		\ EPTR% = EPTR% - 1% &
		\ GOSUB 13100 &
		\ GOSUB 13000 &
		\ GOSUB 13800 &
		\ GOTO 3030 IF I%<>A% OR A%<2% &
		\ E$ = N$ \ E$ = "LINE " IF ST%=4% &
		\ A%=FNPRINT%(C.INPUT$+E$+S$+A$) &
		\ TERM1$=C.COMMA$ &
		\ STFLG%=2% &
		\ STFLG%=0% IF ST%=4% &
		\ GOTO 2500
2930	  IF ST%>4% AND ST%<>8% THEN &
		  GOSUB 13400 &
		\ E$ = FNARG1$(N$) ! Remove the 0% stacked on there. &
		\ GOTO 3030 IF FNCHECK%(EPTR%,249%) &
		\ EPTR% = EPTR% - 1% &
		\ GOSUB 13100 &
		\ GOSUB 13000 &
		\ A%=FNPRINT%("FIELD "+S$+V$) &
		\ TERM$,TERM1$=N$ &
		\ STFLG%=8% &
		\ GOTO 2500
2940	  GOTO 3030 IF ST%<>2% AND ST%<>8% &
	\ IF OP% = 110% THEN &
		  A$ = FNARG1$(N$) &
		\ GOSUB 13100 &
		\ GOSUB 13000 &
		\ A%=FNPRINT%("PRINT "+S$+C.RECORD$+A$) &
		\ TERM1$=C.COMMA$ \ TERM$=C.SEMI$ &
		\ STFLG%=4% &
		\ GOTO 2500
2950	  A$ = FNARG1$(N$) IF OP%<>1% AND OP%<>2% AND OP%<>67% &
	\ E$ = FNARG1$(N$) IF ST%=8% &
	\ E$ = C.USING$+FNARG1$(N$) IF ST%=8% &
	\ A$ = E$ + C.COMMA.SP$ + A$ IF ST%=8% AND OP%<>67% &
	\ A$ = E$ IF OP%=67% &
	\ GOTO 3030 IF FNCHECK%(EPTR%,249%) &
	\ IF FNB%(STMT%(EPTR%-1%))=52% THEN EPTR% = EPTR% - 2% &
		\ GOSUB 13100 &
		\ GOTO 3030 IF OP% &
		\ GOSUB 13000 &
		\ A$=C.COMMA$ IF OP%=1% &
		\ S$ = LEFT(S$,LEN(S$)-2%) IF OP%=2% &
		\ A%=FNPRINT%(C.INPUT$+S$+A$) &
		\ TERM$,TERM1$=C.SEMI$ \ TERM$,TERM1$=N$ IF OP%=1% &
		\ STFLG%=2% &
		\ GOTO 2500
2960	  EPTR% = EPTR% - 1% &
	\ GOSUB 13100 &
	\ A$=C.COMMA$ IF OP%=1% \ A$=N$ IF OP%=2% &
	\ GOSUB 13000 &
	\ S$ = LEFT(S$,LEN(S$)-2%) IF OP%=2% &
	\ A%=FNPRINT%( "PRINT "+S$+A$) &
	\ TERM$,TERM1$=C.SEMI$ \ TERM$,TERM1$=N$ IF OP%=1% OR OP%=2% &
	\ TERM1$=C.COMMA$ IF ST%=8% &
	\ STFLG%=4% &
	\ STFLG%=0% IF OP%=2% &
	\ GOTO 2500
2970	  IF ST%>4% AND ST%<>8% THEN GOTO 3030 IF (STFLG% AND 12%)=0% &
		\ GOTO 2930 IF STFLG% AND 4% &
		\ GOSUB 13400 &
		\ GOTO 3030 IF EPTR% &
		\ A%=FNPRINT%(C.COMMA$+V$) &
		\ GOTO 2500
2980	  IF ST%=3% THEN &
		  GOTO 3030 IF (STFLG% AND 2%)=0% &
		\ GOSUB 13700 IF FNCHECK%(EPTR%,248%)=0% IF EPTR% &
		\ GOSUB 13800 &
		\ GOTO 3030 IF I%<>A% OR A%<2% OR EPTR% &
		\ A% = FNPRINT%(TERM1$+A$) &
		\ TERM1$=C.COMMA$ &
		\ TERM$=N$ &
		\ GOTO 2500
2990	  GOTO 3030 IF ST%<>2% AND ST%<>8% &
	\ GOTO 3030 IF (STFLG% AND 6%)=0% &
	\ GOTO 2940 IF OP%=110% &
	\ A$=N$ \ A$=FNARG1$(N$) IF OP%<>1% AND OP%<>2% AND OP%<>67% &
	\ A$=C.COMMA$ IF OP%=1% &
	\ IF EPTR%=0% THEN &
		  GOSUB 13900 &
		\ TERM1$=C.COMMA$ IF ST%=8% &
		\ GOTO 2500
3010	  GOTO 3030 IF FNCHECK%(EPTR%,249%) &
	\ EPTR% = EPTR% -1% IF FNB%(STMT%(EPTR%-1%))=52% &
	\ EPTR% = EPTR% - 1% &
	\ GOTO 3030 IF EPTR%
3020	  GOSUB 13900 &
	\ GOTO 2500
3030	  A% = FNERROR%(3030%,CVT%$(OP%)+CVT%$(ST%)+CVT%$(STFLG%)) &
	\ GOTO 2500 &
	&

10000	! Process Simple Statements (containing no expressions i.e. STOP) &
	  A% = FNCLEAN.UP% &
	\ STFLG% = 0% &
	\ GOSUB 13000 &
	\ A%=FNPRINT%(DSC$(OP%)) &
	\ GOTO 2500 &

10100	! Process Simple Single Expression Statements. &
	  A$ = FNARG1$(N$) &
	\ A% = FNCLEAN.UP% &
	\ GOSUB 13000 &
	\ A%=FNPRINT%(DSC$(OP%)+A$) &
	\ GOTO 2500 &

10200	! Process GOTO's/GOSUB's/RESUME's/ON ERROR GOTO's &
	  L% = FNR%(STMT% + 1%) &
	\ L% = FNW%(L% + SPTA% + 10%) IF L% &
	\ A% = FNCLEAN.UP% &
	\ GOSUB 13000 &
	\ E$ = N$ &
	\ E$ = FNFLG$(NUM1$(L%)) IF OP%<>21% OR L% &
	\ A%=FNPRINT%(DSC$(OP%)+E$) &
	\ GOTO 2500 &

10300	! Process NAME xx AS yy statements &
	  A$ = C.AS$+FNARG1$(N$) &
	\ A$ = "NAME "+FNARG1$(N$)+A$ &
	\ A% = FNCLEAN.UP% &
	\ GOSUB 13000 &
	\ A%=FNPRINT%(A$) &
	\ GOTO 2500 &

10400	! Process Two argument statements (CHAIN X$ X%) &
	  A$ = FNARG1$(N$) &
	\ A$ = FNARG1$(N$) + C.SP$ + A$ &
	\ A% = FNCLEAN.UP% &
	\ GOSUB 13000 &
	\ A%=FNPRINT%(DSC$(OP%)+A$) &
	\ GOTO 2500 &

10500	! Process ON x GOTO/ON x GOSUB statments &
	  A$ = FNARG1$(N$) &
	\ A% = FNCLEAN.UP% &
	\ GOSUB 13000 &
	\ L% = FNB%(STMT% + 1%) &
	\ A%=FNPRINT%("ON "+A$+C.SP$+DSC$(OP%)+ &
		NUM1$(FNW%(SPTA%+10%+FNR%(L%+STMT%)))) &
	\ A%=FNPRINT%(C.COMMA$+FNFLG$(NUM1$(FNW%(SPTA%+10%+FNR%(I% + STMT%))))) &
		FOR I% = L% - 2% TO 2% STEP -2% &
	\ BCOD% = BCOD% + L% + 2% &
	\ GOTO 2500 &

10600	! MAT statements &
	  IF OP%=26% OR OP%=27% OR OP%=56% THEN ! Mat PRINT &
		  GOSUB 13600 &
		\ GOTO 10699 IF FNCHECK%(EPTR%,249%) &
		\ EPTR% = EPTR% - 1% &
		\ GOSUB 13100 &
		\ GOSUB 13000 &
		\ A$ = A$ + C.COMMA$ IF OP%=26% &
		\ A$ = A$ + C.SEMI$ IF OP%=27% &
		\ A% = FNPRINT%(DSC$(OP%)+S$+B$+A$) &
		\ GOTO 10699 IF FNB%(BCOD%)<>2% &
		\ BCOD% = BCOD% + 1% &
		\ GOTO 2500
10610	  IF OP%>=29% AND OP%<=31% THEN !Mat A=IDN,CON,ZER,etc. &
		  GOSUB 13600 &
		\ B$ = LEFT(B$,LEN(B$)-1%) IF LEN(A$) &
		\ A$ = "("+A$ IF LEN(A$) &
		\ GOSUB 13000 &
		\ A% = FNPRINT%("MAT "+B$+C.EQU$+DSC$(OP%)+A$) &
		\ GOTO 2500
10620	  IF OP%=32% OR OP%=33% THEN  ! Mat A=TRN(B),INV(B) &
		  GOSUB 13000 &
		\ A$ = FNVAR$(FNR%(STMT%+1%)) &
		\ B$ = FNVAR$(FNR%(STMT%+3%)) &
		\ A% = FNPRINT%("MAT "+LEFT(B$,LEN(B$)-1%)+C.EQU$+DSC$(OP%)+ &
			LEFT(A$,LEN(A$)-1%)+")") &
		\ GOTO 2500
10630	  IF OP%=36% THEN ! Mat A=B &
		  GOSUB 13000 &
		\ A$ = FNVAR$(FNR%(STMT%+1%)) &
		\ A$ = LEFT(A$,LEN(A$)-1%) &
		\ B$ = FNVAR$(FNR%(STMT%+3%)) &
		\ B$ = LEFT(B$,LEN(B$)-1%) &
		\ A% = FNPRINT%("MAT "+B$+C.EQU$+A$) &
		\ GOTO 2500
10640	  IF OP%=34% OR OP%=35% THEN ! Mat A=(K)*B &
		  A$ = FNARG1$(N$) &
		\ GOSUB 13000 &
		\ B$ = FNVAR$(FNR%(STMT%+1%)) &
		\ A$ = "("+A$+")*"+LEFT(B$,LEN(B$)-1%) &
		\ B$ = FNVAR$(FNR%(STMT%+3%)) &
		\ B$ = LEFT(B$,LEN(B$)-1%) &
		\ A% = FNPRINT%("MAT "+B$+C.EQU$+A$) &
		\ GOTO 2500
10650	  IF OP%>=37% AND OP%<=39% THEN ! MAT A=B*C, B-C, B+C &
		  GOSUB 13000 &
		\ A$ = FNVAR$(FNR%(STMT%+1%)) &
		\ A$ = LEFT(A$,LEN(A$)-1%) &
		\ B$ = FNVAR$(FNR%(STMT%+3%)) &
		\ B$ = LEFT(B$,LEN(B$)-1%) &
		\ S$ = FNVAR$(FNR%(STMT%+5%)) &
		\ S$ = LEFT(S$,LEN(S$)-1%) &
		\ A% = FNPRINT%("MAT "+S$+C.EQU$+B$+DSC$(OP%)+A$) &
		\ GOTO 2500
10660	  IF OP%<>25% AND OP%<>28% THEN 10699 ELSE &
		  GOSUB 13600 &
		\ IF OP%=25% THEN &
			  GOSUB 13000 &
			\ A% = FNPRINT%(DSC$(OP%)+B$+A$) &
			\ GOTO 2500
10665	  IF STFLG% AND 256% THEN S$=C.COMMA.SP$ ELSE &
		  GOTO 10699 IF FNCHECK%(EPTR%,248%) &
		\ GOTO 10699 IF FNCHECK%(EPTR%-1%,45%) &
		\ EPTR% = EPTR% - 1% &
		\ S$ = N$ &
		\ GOSUB 13100 &
		\ S$ = DSC$(OP%)+S$ &
		\ GOSUB 13000
10670	  A% = FNPRINT%(S$+B$+A$) &
	\ STFLG% = STFLG% OR 256% &
	\ GOTO 2500
10699	  A% = FNERROR%(10699%,CVT%$(OP%)) ! Mat statement error. &
	\ GOTO 2500 &

10700	! Process OPEN statments &
	  A$ = N$ &
	\ IF FNB%(STMT%(EPTR%))<>108% THEN &
			  A$ = C.MODE$+FNARG1$(N$) &
			\ A$ = N$ IF CVT$$(A$,1%) = ", MODE 0%" &
			\ GOTO 10720 &

10710	  EPTR% = EPTR% - 1% &
	\ A$ = C.MODE$+FNARG1$(N$)
10720	  E$ = FNARG1$(N$) &
	\ A$ = C.SIZE$ + E$ + A$ IF E$<>C.0$ &
	\ E$ = FNARG1$(N$) &
	\ A$ = C.CLUST$ + E$ + A$ IF E$<>C.0$ &
	\ E$ = FNARG1$(N$) &
	\ A$ = C.RSIZE$ + E$ + A$ IF E$<>C.0$ &
	\ A$ = "AS FILE " + FNARG1$(N$) + A$ &
	\ A$ = "OPEN "+ FNARG1$(N$) + DSC$(OP%) + A$ &
	\ GOSUB 13000 &
	\ A%=FNPRINT%(A$) &
	\ GOTO 2500 &

10800	! Process GET/PUT statments &
	  A$ = N$ &
	\ E$ = C.0$ &
	\ E$ = FNARG1$(N$) IF Q%>3% &
	\ A$ =  C.COMMA.SP$ + C.USING$ + E$ IF E$<>C.0$ &
	\ E$ = FNARG1$(N$) IF Q%>3% OR OP%=102% &
	\ A$ = C.COUNT$+E$ IF E$<>C.0$ &
	\ IF FNB%(STMT%(EPTR%))=123% THEN EPTR% = EPTR% - 1% &
		\ A$ = C.BLOCK$+FNARG1$(N$) + A$ &
		\ GOTO 10820
10810	  E$ = FNARG1$(N$) IF Q%>3% &
	\ E$ = FNARG1$(N$) &
	\ A$ = C.COMMA.SP$+C.RECORD$+E$ + A$ IF E$<>C.0$
10820	  A$ = DSC$(OP%)+FNARG1$(N$)+A$ &
	\ A% = FNCLEAN.UP% &
	\ GOSUB 13000 &
	\ A%=FNPRINT%(A$) &
	\ BCOD% = BCOD% - 1% IF Q%<4% &
	\ GOTO 2500 &

10900	! Process Function DEF's &
	  A% = FNCLEAN.UP% &
	\ GOSUB 13000 &
	\ L% = FNW%(FNR%(STMT%+1%)+SPDA%+2%) &
	\ T% = 2% &
	\ WHILE L% AND 3% &
		\ T% = T% + 2% &
		\ L% = L% / 4% &
	\ NEXT &
	\ S$ = C.RPAR$ &
	\ S$ = C.COMMA$+FNVAR$(FNR%(STMT%+L%))+S$ FOR L%=4% TO T% STEP 2% &
	\ S$ = RIGHT(S$,2%) &
	\ S$ = "("+S$ IF T%<>2% &
	\ A% = FNPRINT%(C.DEF$+FNVAR$(FNR%(STMT%+1%)+4%)+S$) &
	\ BCOD% = BCOD% + T% + 2% &
	\ GOTO 2500 &

11000	  A% = FNCLEAN.UP% IF OP%<>10% &
	\ GOTO 2500 &

11100	! Process Change statements &
	  A$ = FNVAR$(FNR%(STMT%+1%)) &
	\ A$ = LEFT(A$,LEN(A$)-1%) &
	\ IF OP%=23% THEN A$ = FNARG1$(N$)+C.TO$+A$ ELSE &
			  A$ = A$+C.TO$ &
			\ A% = FNCLEAN.UP% &
			\ A% = FNSCAN%(2%) &
			\ GOTO 11120 IF A%<0% &
			\ EPTR% = EPTR% + 1% &
			\ STMT%(EPTR%)=STMT% &
			\ E$ = FNARG$ &
			\ A$ = A$ + LEFT(E$,LEN(E$)-1%)
11110	  A% = FNCLEAN.UP% &
	\ GOSUB 13000 &
	\ A% = FNPRINT%("CHANGE "+A$) &
	\ GOTO 2500
11120	  A% = FNERROR%(11120%,CVT%$(A%)) ! Illegal op code A% in CHANGE &
	\ GOTO 11110 &
	&

11200	  stop &

13000	  PRINT #2%,CVT$$(TERM$,1%); \ TERM$=N$ &
	\ A% = FNCLEAN.UP% IF EPTR% &
	\ GOTO 13020
13010	  PRINT #2%,CVT$$(TERM$,1%); \ TERM$=N$
13020	  PRINT #2%,C.TRM$+FNINDENT$+"\ "; IF L.STATE% = INLINE% IF S%=0% &
	\ L.STATE% = INLINE% &
	\ RETURN &

13100	  S$ = "#"+FNARG1$(N$)+C.COMMA.SP$ &
	\ S$ = N$ IF CVT$$(S$,1%)="#0%, " &
	\ RETURN &
	&

13200	  Z.A.OP% = FNB%(STMT%(EPTR%)) &
	\ Z.A.T%  = TYP%(Z.A.OP%) &
	\ Z.A.T1% = Z.A.T% AND 255% &
	\ Z.A.T%  = Z.A.T% / 256% &
	\ RETURN &

13300	  O$=N$ &
	\ RETURN IF BUFFLG%=0% &
	\ O$=O$+STACK$(A%) FOR A%=BUFFLG% TO STSP% &
	\ STSP% = BUFFLG% - 1% &
	\ BUFFLG% = 0% &
	\ RETURN &

13400	! Get Length and Variable for field statments. &
	  V$ = FNVAR$(FNR%(STMT%+1%)) &
	\ IF ST%>5% THEN &
		  A$ = FNARG1$(N$)+C.RPAR$ &
		\ A$ = FNARG1$(N$)+C.COMMA$+A$ IF ST%=7% &
		\ V$ = V$ + A$
13410	  V$ = FNARG1$(N$) + C.AS$ + V$ &
	\ RETURN &

13500	! Pulls off next item pointed to by EPTR% and classifies it for &
	! FNARG1$ into the Variables -->> Z.A1.OP%  Z.A1.T%  Z.A1.T1% &
	! If there is no next item, or Z.A1.T% > 2% then error is printed if &
	! it exists and -1 is returned for Z.A1.OP% &
	  IF EPTR%=0% THEN Z.A1.OP% = -1% &
		\ RETURN
13510	  Z.A1.OP% = FNB%(STMT%(EPTR%)) &
	\ EPTR% = EPTR% - 1% &
	\ Z.A1.T%,Z.A1.T1% = TYP%(Z.A1.OP%) &
	\ Z.A1.T% = Z.A1.T% / 256% &
	\ Z.A1.T1% = Z.A1.T1% AND 255% &
	\ IF Z.A1.T% > 2% THEN A% = FNERROR%(13510%,CVT%$(Z.A1.T%)) &
		\ Z.A1.OP% = -1%
13520	  RETURN &
	&

13600	! Routine to process the dimensions that are stacked for MAT statements &
	  S$ = C.COMMA$+FNARG1$(N$) &
	\ B$ = FNARG1$(N$) &
	\ S$ = N$ IF S$=C.COMMA$+C.0$ &
	\ B$,S$=N$ IF CVT$$(S$,1%)=",(NOT 0%)" AND B$=C.0$ &
	\ A$ = B$ + S$ &
	\ B$ = FNVAR$(FNR%(STMT%+1%)) &
	\ B$ = LEFT(B$,LEN(B$)-1%) IF LEN(A$)=0% &
	\ A$ = A$ + C.RPAR$ IF LEN(A$) &
	\ RETURN &
	&
	&

13700	  IF FNCHECK%(EPTR%-1%,52%)=0% THEN &
		IF FNCHECK%(EPTR%-1%,45%)=0% THEN &
			EPTR% = EPTR% - 1%
13710	  RETURN &

13800	! Get Variable name for input statments. &
	  A% = FNSCAN%(2%) &
	\ EPTR% = EPTR% + 1% &
	\ STMT%(EPTR%)=STMT% &
	\ A$ = FNARG$ &
	\ A% = LEN(A$) &
	\ I% = INSTR(1%,A$,C.EQU$) &
	\ A$ = LEFT(A$,I% - 1%) &
	\ RETURN &

13900	! Print data &
	  TERM$,TERM1$=N$ IF OP%=1% OR OP%=2% &
	\ A%=FNPRINT%(TERM1$+A$) IF OP%<>67% &
	\ TERM$,TERM1$=C.SEMI$ IF OP%<>1% AND OP%<>2% &
	\ RETURN &
	&
15000	! Compares FNB%(STMT%(arg1%)) with arg2%, sets EPTR%=arg1% &
	  DEF* FNCHECK%(A%,I%) &
	\ EPTR% = A% &
	\ IF FNB%(STMT%(EPTR%))<>I% THEN FNCHECK%=-1% ELSE FNCHECK%=0%
15010	  FNEND &
	&
	! Look up address A% in hash file.  Returns position of A% if found, &
	! and position to put new address in if not found.  A negative position &
	! means that hash table is full, and the position points to the &
	! overflow area.  (See FNIST%) &
	\ DEF* FNSRCH%(A%) &
		\ I% = (A% XOR SWAP%(A%)) AND 32767% &
		\ H1%,S1%=I%-I%/241%*241% &
		\ R1%=I%-I%/239%*239%+1%
15110		  I% = ADDR%(H1%) &
		\ IF I%=0% OR I%=A% THEN FNSRCH%=H1% &
			\ GOTO 15140
15120		  H1% = H1%-R1% &
		\ H1%=H1%+241% IF H1%<0% &
		\ GOTO 15110 IF H1%<>S1% &
		\ FOR I%=1% TO OVR% &
			\ IF ADDR1%(I%) = A% THEN FNSRCH%=-I% &
				\ GOTO 15140
15130		  NEXT I% &
		\ FNSRCH% = -OVR% - 1%
15140	  FNEND &
	&
	! Set up a new line number &
	\ DEF* FNNEW.LINE% &
	\ I.IND% = 0% &
	\ IF L.STATE%=INLINE% OR L.STATE%=NEWLINE% THEN &
		  A%=FNPRINT%("!") IF L.STATE%=NEWLINE% &
		\ A%=FNPRINT%(C.TRM$+'9'C+"  ! Program name: "+LEFT(C.FIL$, &
			INSTR(1%,C.FIL$,".")-1%)+"		"+ &
			"Compiled with SCALE "+NUM1$(SCALEF%)+" on V"+ &
			CHR$(FNB%(SP%+2%))+CHR$(FNB%(SP%+3%))+"."+ &
			CHR$(FNB%(SP%))+C.TRM$+'9'C+ &
			"  ! Decompiled on "+DATE$(0%)+" at "+TIME$(0%)+ &
			" by UNBAC Version "+VERSION$) &
				IF SLASH.N%=0% &
		\ SLASH.N% = -1% &
		\ A%=FNPRINT%(E.TRM$)
15510	  L.STATE%=NEWLINE% &
	\ A%=FNPRINT%(NUM1$(L%)+STRING$(F.IND%+I.IND%+1%+(T%=3%),9%)+"  ") &
	\ IF EXTND%=-1% THEN L.STATE%=INLINE% &
		\ EXTND%=-2% &
		\ A%=FNPRINT%("EXTEND"+'10'C+'13'C+'0'C+'9'C)
15520	  FNEND &
	&
	! Scans from current code pointer (BCOD%) to end of code (ECOD%) ptr &
	! keeping track of starting addresses of Op Codes in STMT%(). &
	! Returns when Op Code type is greater than T% or the current address &
	! is either the same as the top of the "modifier" stack or at the end &
	! Value returned:  >0 - Op Code that caused the return. &
	!		   -1 - At end of line &
	!		   -2 - At address on top of modifier stack &
	! EPTR% is incremented for each item put in STMT%().  STMT% is set &
	! to the address of the code (or address) that caused a return. &
	! If more data is on the line then BCOD% is automatically incremented &
	! to skip over the offending instruction (except for variable length &
	! instructions like DEF's and ON statments. &
	\ DEF* FNSCAN%(T%) &
	\ S0% = S%(S%) &
	\ GOTO 15650 IF BCOD% = S0%
15610	  STMT% = BCOD% &
	\ IF BCOD% >= ECOD% THEN FNSCAN% = -1% &
		\ GOTO 15690
15620	  ZOP% = FNB%(BCOD%) &
	\ BCOD% = BCOD% + O.LEN%(ZOP%)
15630	  IF (SWAP%(TYP%(ZOP%)) AND 255%)<=T% THEN &
		  EPTR% = EPTR% + 1% &
		\ STMT%(EPTR%) = STMT% &
		\ GOTO 15650 IF BCOD% = S0% &
		\ GOTO 15610
15640	  FNSCAN% = ZOP% &
	\ GOTO 15690 IF ZOP%<>87% AND ZOP%<>214%
15650	  FNSCAN% = -2%
15690	  FNEND &
	&
	! This function pulls LET statements and expressions off the &
	! stack (It actually only pulls off the assignment statements and &
	! then calls FNARG1$ to get the expression. &
	\ DEF* FNARG$ &
	\ FNARG$ = N$ &
	\ GOTO 15790 IF EPTR%=0% &
	\ GOSUB 13200 &
	\ IF Z.A.OP%=249% THEN EPTR%=EPTR%-1% &
	\ GOSUB 13100 &
	\ S$=LEFT(S$,LEN(S$)-2%) &
	\ FNARG$="PRINT "+S$+";" &
	\ GOTO 15790
15710	  IF Z.A.T% > 3% THEN A% = FNERROR%(15710%,CVT%$(Z.A.OP%)) &
		\ EPTR% = EPTR% - 1% &
		\ GOTO 15790
15720	  Z.A$ = N$ &
	\ GOTO 15760 IF Z.A.T%<3% &
	\ IF Z.A.T1% AND 1% THEN A% = FNERROR%(15720%,CVT%$(Z.A.OP%)) &
		\ EPTR% = EPTR% - 1% &
		\ GOTO 15790
15730	  EPTR1% = EPTR% &
	\ EPTR% = EPTR% - 1% &
	\ Z.E$ = N$ &
	\ IF Z.A.T1% AND 6% THEN &
		  Z.E$ = FNARG1$(N$) + C.RPAR$ &
		\ Z.E$ = FNARG1$(N$) + C.COMMA$ + Z.E$ IF Z.A.T1% AND 4%
15740	  Z.A$ = DSC$(Z.A.OP%) + FNVAR$(FNR%(STMT%(EPTR1%)+1%)) + Z.E$
15745	  IF EPTR%=0% THEN FNARG$ = Z.A$ + C.EQU$ &
		\ GOTO 15790
15750	  GOSUB 13200 &
	\ IF Z.A.T% > 3% THEN A% = FNERROR%(15750%,CVT%$(Z.A.OP%)) &
		\ EPTR% = EPTR% - 1% &
		\ FNARG$ = Z.A$ + C.EQU$ &
		\ GOTO 15790
15760	  IF Z.A.T% < 3% THEN FNARG$ = Z.A$ + C.EQU$ + FNARG1$(N$) &
		\ GOTO 15790
15770	  IF Z.A.OP% = 104% THEN EPTR% = EPTR% - 1% &
		\ GOTO 15750
15775	  EPTR1% = EPTR% &
	\ EPTR% = EPTR% - 1% &
	\ E$ = N$ &
	\ IF Z.A.T1% AND 6% THEN &
		  E$ = FNARG1$(N$) + C.RPAR$ &
		\ E$ = FNARG1$(N$) + C.COMMA$ + E$ IF Z.A.T1% AND 4%
15780	  Z.A$ = Z.A$ + C.COMMA$ +FNVAR$(FNR%(STMT%(EPTR1%)+1%)) + E$ &
	\ GOTO 15745
15785	  FNARG$ = Z.A$ &
	\ GOTO 15790
15789	  A% = FNERROR%(15789%,CVT%$(Z.A.OP%))
15790	  FNEND &
	&
	! This function converts the Reverse Polish items scanned to &
	! algebraic notation by scanning backwards through the list. &
	\ DEF* FNARG1$(O$) &
	\ Z.A1.L.OP% = FNPUSH%(257%) ! Push end-of-stack op code
15810	  Z.A1.EPTR1% = EPTR% &
	\ GOSUB 13500 ! Classify the current op code pointer to by EPTR% &
	\ GOTO 15899 IF Z.A1.OP% = -1% ! Done or error. &
	\ ON Z.A1.T% + 1% GOTO 15820,15830,15860
15820	  O$ = DSC$(FNPOP%)+FNCLASSIFY.PUSH$(STMT%(Z.A1.EPTR1%))+O$ &
	\ O$ = DSC$(FNPOP%) + O$ WHILE Z.A1.L.OP%=256% OR Z.A1.L.OP%=161% OR &
				       Z.A1.L.OP%=162% OR Z.A1.L.OP%=224% &
	\ GOTO 15810 IF Z.A1.L.OP%<>257% &
	\ FNARG1$ = O$ &
	\ GOTO 15899
15830	  Z.A1.T2% = TYP%(STACK%(ISP%)) AND 255% &
	\ IF Z.A1.T1% >= (TYP%(Z.A1.L.OP%) AND 255%) AND Z.A1.OP%<>224% &
			AND (Z.A1.T2%<>Z.A1.T1% OR (Z.A1.T2% AND 1%)) THEN &
		  Z.A1.L.OP% = FNPUSH%(Z.A1.OP%) &
		\ GOTO 15810
15840	  O$ = C.RPAR$ + O$ &
	\ A% = FNPUSH%(256%) &
	\ Z.A1.L.OP% = FNPUSH%(Z.A1.OP%) &
	\ GOTO 15810
15860	  GOTO 15810 IF Z.A1.T1%=0% &
	\ A% = FNPUSH%(Z.A1.EPTR1%) + FNPUSH%(Z.A1.OP%) &
	\ GOSUB 13500 ! Classify the current op code pointed to by EPTR% &
	\ IF Z.A1.T% OR Z.A1.T1%>1% THEN &
		  A% = FNERROR%(15860%,N$) ! No function argument paramater. &
		\ GOTO 15899
15870	  IF Z.A1.OP% =  81% THEN Z.A1.N% = 1% ELSE &
	  IF Z.A1.OP% = 190% THEN Z.A1.N% = 0% ELSE &
				  Z.A1.N% = FNR%(STMT%(EPTR%+1%)+1%)
15880	  O$ = C.RPAR$ + O$ &
	\ A% = FNPUSH%(Z.A1.N%) &
	\ WHILE Z.A1.N% AND 3% &
		\ Z.A1.N% = FNPUSH%(Z.A1.N%/4%) &
		\ O$ = C.COMMA$ + FNARG1$(N$) + O$ &
		\ Z.A1.N% = FNPOP% &
	\ NEXT &
	\ O$ = RIGHT(O$,2%) &
	\ O$ = "(" + O$ IF FNPOP% AND 3% &
	\ Z.A1.OP% = FNPOP% &
	\ Z.A1.T1% = TYP%(Z.A1.OP%) AND 255% &
	\ Z.A1.EPTR1% = FNPOP% &
	\ IF Z.A1.T1% = 1% THEN O$ = DSC$(Z.A1.OP%) + O$ ELSE &
	  IF Z.A1.T1% = 2% THEN O$ = FNVAR$(FNR%(STMT%(Z.A1.EPTR1%)+1%)+4%) + O$
15890	  Z.A1.L.OP% = 256% &
	\ O$ = DSC$(FNPOP%) + O$ WHILE Z.A1.L.OP% = 256% OR Z.A1.L.OP%=161% OR &
				       Z.A1.L.OP% = 162% OR Z.A1.L.OP%=224% &
	\ GOTO 15810 IF Z.A1.L.OP%<>257% &
	\ FNARG1$=O$
15899	  FNEND &
	&
	! This routine returns the argument that the op code is pushing on &
	! the BASIC-PLUS stack. &
	\ DEF* FNCLASSIFY.PUSH$(STMT%) &
	\ Z.C.OP%=FNB%(STMT%) &
	\ Z.C.T1%=TYP%(Z.C.OP%) ! We know that major group is 0. &
	\ IF Z.C.T1%=0% THEN FNCLASSIFY.PUSH$=DSC$(Z.C.OP%) &
		\ GOTO 15999	! Special 0%,1%,0.,1.
15910	  IF Z.C.T1%=1% THEN FNCLASSIFY.PUSH$=FNFLG$(NUM1$(FNR%(STMT%+1%))+"%") &
		\ GOTO 15999	! Integer immediate.
15920	  IF Z.C.T1% = 2% THEN Z.C.A$ = NUM1$(CVT$F(FFIL$+CVT%$(FNR%( &
			STMT%+1%)))/SCALEF.) &
		\ Z.C.A$ = Z.C.A$ + "." IF INSTR(1%,Z.C.A$,".")=0% &
		\ FNCLASSIFY.PUSH$=FNFLG$(Z.C.A$) &
		\ GOTO 15999	! Short-Floating immediate.
15930	  IF Z.C.T1% = 3% THEN FNCLASSIFY.PUSH$,Z.C.A$ = FNVAR$(FNR%( &
			STMT%+1%)) &
		\ GOTO 15999 IF Z.C.A$ <> N$ &
		\ Z.C.N% = FNR%(STMT% + 1%) + SPDA% &
		\ Z.C.A$ = CVT%$(FNW%(Z.C.N%)) + Z.C.A$ &
			FOR Z.C.N% = Z.C.N% TO Z.C.N% + FLEN%*2% - 2% STEP 2% &
		\ Z.C.A$ = NUM1$(CVT$F(Z.C.A$)/SCALEF.) &
		\ Z.C.A$ = Z.C.A$ + "." IF INSTR(1%,Z.C.A$,".")=0% &
		\ FNCLASSIFY.PUSH$ = FNFLG$(Z.C.A$) &
		\ GOTO 15999 &
		! Floating variable (possibly constant)
15940	  IF Z.C.T1% = 4% THEN FNCLASSIFY.PUSH$,Z.C.A$ = FNVAR$( &
			FNR%(STMT%+1%)) &
		\ GOTO 15999 IF Z.C.A$ <> N$ &
		\ FNCLASSIFY.PUSH$ = FNFLG$(NUM1$(FNW%(FNR%(STMT%+1%)+ &
			SPDA%))+"%") &
		\ GOTO 15999 &
		! Integer  variable (possibly a constant on early versions)
15950	  IF Z.C.T1% = 5% THEN FNCLASSIFY.PUSH$,Z.C.A$=FNVAR$(FNR%(STMT%+1%)) &
		\ GOTO 15999 IF Z.C.A$<>N$ &
		\ Z.C.N% = FNR%(STMT%+1%) &
		\ Z.C.L% = FNW%(Z.C.N%+4%+SPDA%)-1% &
		\ Z.C.N% = Z.C.N% + FNW%(Z.C.N%+SPDA%+2%) + SPST% &
		\ Z.C.A$ = Z.C.A$ + CHR$(FNB%(Z.C.N%)) &
			FOR Z.C.N%=Z.C.N% TO Z.C.N%+Z.C.L% &
		\ FNCLASSIFY.PUSH$=C.DQTE$ + Z.C.A$ + '"' &
		\ FNCLASSIFY.PUSH$=C.SQTE$+Z.C.A$ + "'" IF INSTR(1%,Z.C.A$,'"') &
		\ GOTO 15999 &
		! String variables or constants.
15960	A% = FNPUSH%(Z.A1.L.OP%) &
	\ IF Z.C.T1% = 6% THEN FNCLASSIFY.PUSH$ = FNVAR$(FNR%(STMT%+1%))+ &
			FNARG1$(N$)+C.RPAR$ &
		\ A% = FNPOP% &
		\ GOTO 15999 &
		! Single subscript arrays (float, int, or $)
15970	  Z.C.A$ = FNPUSH$(FNARG1$(N$)) &
	\ FNCLASSIFY.PUSH$ = FNVAR$(FNR%(STMT%+1%)) + FNARG1$(N$) &
		+ C.COMMA$ + FNPOP$ + C.RPAR$ &
	\ A% = FNPOP% &
	! Two subscript arrays.
15999	  FNEND &
	&
	! Pushes argument on stack (open on virtual array) &
	\ DEF* FNPUSH%(A%) &
	\ ISP% = ISP% + 1% &
	\ STACK%(ISP%),FNPUSH% = A% &
	\ FNEND &
	&
	! Pops argument from stack and sets Z.A1.L.OP% & function to value. &
	\ DEF* FNPOP% &
	\ FNPOP%,Z.A1.L.OP% = STACK%(ISP%) &
	\ ISP% = ISP% - 1% &
	\ FNEND &
	&
	! Pushes string onto string stack &
	\ DEF* FNPUSH$(A$) &
	\ A% = LEN(A$) &
	\ FOR Z.P.A% = A% TO 1% STEP -32% &
		\ STSP% = STSP% + 1% &
		\ STACK$(STSP%) = RIGHT(A$,Z.P.A%-31%) &
		\ A$ = LEFT(A$,Z.P.A%-32%) &
	\ NEXT Z.P.A% &
	\ STSP% = STSP% + 1% &
	\ STACK$(STSP%) = CVT%$(A%) &
	\ FNEND &
	&
	! Pops string from string stack. &
	\ DEF* FNPOP$ &
	\ A% = CVT$%(STACK$(STSP%)) &
	\ STSP% = STSP% - 1% &
	\ Z.P.A$ = N$ &
	\ FOR A% = A% TO 1% STEP -32% &
		\ Z.P.A$ = Z.P.A$ + STACK$(STSP%) &
		\ STSP% = STSP% - 1% &
	\ NEXT A% &
	\ FNPOP$ = Z.P.A$ &
	\ FNEND &
	&
	&
	! Calls FNARG$ until stack is empty &
	\ DEF* FNCLEAN.UP% &
	\ IF EPTR% THEN &
		  Z.C.I%=0% &
		\ WHILE EPTR% &
			\ Z.C.I%=Z.C.I%+1% &
			\ Z.C.A$=FNPUSH$(FNARG$) &
		\ NEXT &
		\ FOR Z.C.I% = Z.C.I% TO 1% STEP -1% &
			\ GOSUB 13010 &
			\ A% = FNPRINT%(FNPOP$) &
		\ NEXT Z.C.I%
16110	  FNEND &
	&
	&
	\ DEF* FNW%(A%) = P%(A%/2%-256%) &
	\ DEF* FNB%(A%) &
	\ IF A% AND 1% THEN FNB% = SWAP%(P%(A%/2%-256%)) AND 255% &
	ELSE                FNB% =       P%(A%/2%-256%)  AND 255%
16210	  FNEND &
	\ DEF* FNR%(A%) &
	\ IF A% AND 1% THEN FNR%=(FNW%(A%-1%) AND -256%)+ &
				 (FNW%(A%+1%) AND  255%) &
		       ELSE FNR%=SWAP%(FNW%(A%))
16220	  FNEND &
	&
	! This function searches for the variable name given is address &
	\ DEF* FNVAR$(A%) &
	\ A% = FNSRCH%(A%) &
	\ IF A%>=0% THEN IF ADDR%(A%)=0% THEN Z.V.A$=N$ ELSE &
					       Z.V.A$=VAR$(A%) ELSE &
	  				       Z.V.A$=VAR1$(-A%)
16310	  Z.V.A$ = C.FN$+CVT$$(Z.V.A$,33%) IF ASCII(Z.V.A$)>224% &
	\ FNVAR$ = Z.V.A$ &
	\ FNEND &
	&
	! Outputs data to output file or buffers it depending whether we are &
	! in a statement modifier or not. &
	\ DEF* FNPRINT%(A$) &
	\ IF S%=0% THEN A%=FNFORMAT%(A$) &
		\ GOTO 16499
16410	  BUFFLG% = STSP% + 1% IF BUFFLG%=0% &
	\ FOR A% = 1% TO LEN(A$) STEP 32% &
		\ STSP% = STSP% + 1% &
		\ STACK$(STSP%)=MID(A$,A%,32%) &
	\ NEXT A%
16499	  FNEND &
	&
	! This function indents one <tab> for each count in I.IND% and F.IND% &
	\ DEF* FNINDENT$ &
	\ Z.I.I% = 0% &
	\ Z.I.I% = I.IND% IF F.COND%=0% &
	\ Z.I.I% = Z.I.I% + F.IND% IF F.FOR%=0% &
	\ Z.I.I% = IMAX% IF Z.I.I% > IMAX% &
	\ FNINDENT$ = STRING$(1% + Z.I.I%,9%) &
	\ FNEND &
	&
	! This function edits an output string to try and prevent it from &
	! exceeding the width specified by WIDTH% &
	\ DEF* FNFORMAT%(B$) &
		\ WHILE LEN(B$) &
		\ Z.P.I% = INSTR(1%,B$,C.TRM$) &
		\ Z.P.I% = LEN(B$)+1% IF Z.P.I%=0% &
		\ Z.P.A$ = LEFT(B$,Z.P.I%-1%) &
		\ B$ = RIGHT(B$,Z.P.I%) &
		\ WHILE LEN(Z.P.A$) &
		\ Z.P.P% = CCPOS(2%) &
		\ Z.P.L.S% = 0% &
		\ FOR Z.P.K% = 1% TO LEN(Z.P.A$) &
			\ Z.P.C% = ASCII(MID(Z.P.A$,Z.P.K%,1%)) &
			\ IF (Z.P.C% AND 127%)=9% THEN Z.P.I%=8%-(Z.P.P% AND 7%) &
			ELSE IF (Z.P.C% AND 127%)>31% THEN Z.P.I%=1% ELSE &
				  Z.P.I% = 0%
16610			  IF Z.P.I%+Z.P.P%>WIDTH% AND Z.P.L.S%>1% THEN &
				  PRINT #2%,CVT$$(LEFT(Z.P.A$,Z.P.L.S%-1%),1%); &
					C.TRM$;FNINDENT$;'9'C; &
				\ Z.P.A$ = RIGHT(Z.P.A$,Z.P.L.S%) &
				\ GOTO 16680
16620			  Z.P.L.S% = Z.P.K% IF Z.P.C% > 127% &
			\ Z.P.P% = Z.P.P% + Z.P.I% &
		\ NEXT Z.P.K% &
		\ PRINT #2%,CVT$$(Z.P.A$,1%); &
		\ Z.P.A$=N$
16680	  NEXT &
	\ IF LEFT(B$,LEN(C.TRM$))=C.TRM$ THEN PRINT #2%,C.TRM$; &
		\ B$ = RIGHT(B$,LEN(C.TRM$)+1%)
16686	  NEXT
16690	  FNEND &
	&
	! Sets the parity bit on the first character passed to function &
	\ DEF* FNFLG$(A$)=CHR$(128% OR ASCII(A$))+RIGHT(A$,2%) &
	&
	! Prints out error messages &
	\ DEF* FNERROR%(I%,E$) &
	\ PRINT "%Error encountered processing line";LASLIN%,"L=";I% &
	\ WHILE LEN(E$) &
		\ PRINT CVT$%(E$), &
		\ E$ = RIGHT(E$,3%) &
	\ NEXT &
	\ PRINT &
	\ FNEND
32767	  END
                                                                                                                                                                                                                                                                                                                                                                                 