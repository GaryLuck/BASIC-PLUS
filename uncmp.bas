1	 Z9% = -1%

	! FLAG TO PRINT HEADER SPLAT


11	!



	!		C O P Y R I G H T



  		Copyright (C) 1979 by

  !		Small Business Software Services

  !		54 Alpine St.

  !		Arlington, Mass.

  !

  !

  !	This software is  furnished under the  stipulation  that is not

  !	released or otherwise made available to any third party without

  !	expressed writted permission from SBSS and may only be utilized

  !	on a single  predetermined  CPU unit and may only be copied for

  !	use on said CPU unit with the inclusion  of the above copyright

  !	notice.

  !

  !	The information in this software is subject to change without

  !	notice  and should not be  construed as a commitment by  SBSS

  !	nor any party thereof.

  !

  !	SBSS  assumes no responsibility for the use  or  reliability

  !	of its software.

  !

  !	Although tested  and  believed to be error-free, any defects

  !	should be reported to SBSS as soon as possible.

  !

  !*****************************************************************



100	H8$="UT:"
200       DIM #1%, A%(32767%)

	\ DIM #3%, V0%(5%), V%(500%,5%)

	\ DIM #6%, V0$(5%)=32%, V$(500%,5%)=32%

	\ DIM S%(30%)

	\ B3% = 0%

	\ C% = 0% UNLESS C%


300	ON ERROR GOTO 3000

	\ GOTO 500 IF C%

	\ PRINT IF POS(0%)

	\ IF Z9%

		THEN PRINT

		\ PRINT "UNCOMP"+CHR$(9%)+"V06C-01A"+CHR$(9%)+CVT$$(RIGHT(SYS(CVT%$(1545%)),3%),4%)


400	 PRINT IF POS(0%)

	\ PRINT "#";

	\ OPEN "KB:UNCOMP.CMD" FOR INPUT AS FILE 5%

	\ INPUT LINE #5%, C$

	\ C$ = CVT$$(C$,38%)

	\ GOTO 400 IF C$ = ""

	\ CLOSE 5%


500	 E% = 0%

	\ GOSUB 1900

	\ IF E% THEN IF C% THEN 300 ELSE 400


600	 RESTORE

	\  V0%(Q8%) = 0% FOR Q8% = 0% TO 5%

	\ S% = FNA%(FNA%(514%)+28%)

	\ X9% = 4%+2%*FNB%(FNA%(514%)+38%)-2%

	\ FOR J% = 0% TO 8%

		\ READ V$, X%, Y%

		\ Q%, V0%(Y%) = V0%(Y%) + 1%

		\ V%(Q%,Y%) = X% + S%

		\ V$(Q%,Y%) = V$

	\ NEXT J%

	\ DATA  PI,	-16,    1,		STATUS, -62,2,

		LINE,   -58,    2,		ERR,    -26,2,

		ERL,    -60,    2,		DET,    -24,1,

		NUM,    -30,    2,		NUM2,   -34,2,

		RECOUNT,-54,    2


700	 I% = S% + 1214%

	\ FOR J% = I% TO I%+50% STEP 2%


800		 X% = J%


900		 IF    FNA%(X%) = 0% THEN 1600

		  ELSE    X% = X% + FNA%(X%)

			\ Y% = X% -1%

			\ V9$, V$ = CHR$((J%-I%)/2%+65%)


1000		V0% = FNB%(Y%)

		\ IF V0% AND 128% THEN  Y% = Y% - 1% IF Y% AND 1%

				  ELSE  V9$, V$ = V$ + CHR$(V0%)

					\ B3% = B3% OR V0%<48% OR V0%>57%

						OR LEN(V$)>2%

					\ Y% = Y% - 1%

					\ GOTO 1000


1100		T% = FNB%(Y%)

		\ V$ = "FN" + V$ IF T% AND 16%

		\ V$ = V$ + MID("%  $",T% AND 7%,1%)


1200		   IF T% AND 16% THEN GOTO 1400

		  ELSE IF T% AND  8% THEN Q% = FNX%(Y%-28%,5%)

		  ELSE IF T% AND  4% THEN Q% = FNX%(Y%- 8%,3%)

		  ELSE IF T% AND  2% THEN Q% = FNX%(Y%- X9%,1%)

		  ELSE IF T% AND  1% THEN Q% = FNX%(Y%- 4%,2%)


1300		IF FNA%(Y%-2%) = 0% THEN 900

					ELSE	Y% = Y% + FNA%(Y%-2%) - 2%

						\ V$ = V9$

						\ GOTO 1100


1400		Q% = FNX%(Y%-8%-(T% AND 6%),4%)

		\	IF T% AND 4% THEN Q% = FNX%(Y%-8%,3%)

		  ELSE IF T% AND 2% THEN Q% = FNX%(Y%-X9%,1%)

		  ELSE IF T% AND 1% THEN Q% = FNX%(Y%-4%,2%)


1500		GOTO 1300


1600	NEXT J%


1700	V0$(5%) = CVT%$(B3%)

	\ CLOSE 1%, 3%, 6%


1800	CHAIN H8$+"UNCMP2" 110%


1900	X% = INSTR(1%,C$,"=")

	\ F1$ = RIGHT(C$,X%+1%)

	\ F2$ = LEFT(C$,X%-1%)

	\ I9% = INSTR(1%,F1$,".")

	\ I9% = LEN(F1$)+1% UNLESS I9%

	\ F2$ = "KB:" IF F2$=""

	\ I8% = (RIGHT(F2$,LEN(F2$))=":")

	\ F2$ = F2$+LEFT(F1$,I9%-1%)+".BAS" IF I8%


2000	F1$ = FNF$(F1$,"BAC")

	\ F2$ = FNF$(F2$,"BAF")

	\ F0$ = MID(NUM$(ASCII(SYS(CVT%$(1545%)))/2%+100%),3%,2%)+".TMP"

	\ F3$ = "UNC1" + F0$

	\ F6$ = "UNC2" + F0$


2100	X% = 0%

	\ OPEN F1$ FOR INPUT AS FILE 1%, MODE 8192%

	\ OPEN F3$ FOR OUTPUT AS FILE 3%

	\ OPEN F6$ FOR OUTPUT AS FILE 6%

	\ V0$(0%) = H8$

	\ V0$(1%) = CVTF$(TIME(0%))

	\ V0$(2%) = CVTF$(TIME(1%))

	\ V0$(3%) = F1$

	\ V0$(4%) = F2$

	\ RETURN


2200	DEF FNA%(I%) = A%(I%/2% - 256%)


2300	DEF FNB%(I%)

		\ IF I% AND 1% THEN FNB% = SWAP%(FNA%(I%-1%)) AND 255%

				 ELSE FNB% = FNA%(I%) AND 255%


2400	FNEND


2500	DEF FNX%(I%,J%)

		\ Q%, V0%(J%) = V0%(J%) + 1%

		\ V%(Q%,J%) = I%

		\ V$(Q%,J%) = CVT$$(V$,2%)

	\ FNEND


2600	DEF FNF$(F$,E$)

		\ CHANGE SYS(CHR$(6%)+CHR$(-10%)+F$) TO S%

		\ F$ = RAD$(FNS%(7%)) + RAD$(FNS%(9%)) + "."

		\ S1% = FNS%(29%)

		\ IF S1% AND 8% THEN F$ = F$ + RAD$(FNS%(11%))

				ELSE F$ = F$ + E$


2700		F$ = "["+NUM1$(S%(6%))+","+NUM1$(S%(5%))+"]"+F$ IF S1% AND 128%

		\ F9$ = CHR$(S%(23%))+CHR$(S%(24%))

		\ F9$ = F9$ + NUM1$(S%(25%)) IF S%(26%)=255%

		\ F9$ = F9$ + ":"

		\ F$ = F9$ + F$ IF S1% AND 8192%

		\ F$ = F$ + "<" + NUM1$(S%(22%)) + ">" IF S1% AND 1024%

		\ FNF$ = F$

	\ FNEND


2800	DEF FNS%(I%) = S%(I%) OR SWAP%(S%(I%+1%))


2900	DEF FNS$(X$) = X$ + SPACE$(30%-LEN(X$))


3000	IF ERL = 400% THEN RESUME 32767

			ELSE PRINT IF POS(0%)

			\ PRINT CVT$$(RIGHT(SYS(CVT%$(1545%)+CHR$(ERR)),3%),4%)

			\ RESUME 400 IF Z9%

			\ RESUME 32767


30000	C$ = SYS(CHR$(7%))

	\ C% = INSTR(1%,C$," ")

	\ C$ = CVT$$(RIGHT(C$,C%+1%),38%)

	\ GOTO 100 IF C%

	\ PRINT CVT$$(RIGHT(SYS(CVT%$(1545%)+CHR$(97%)),3%),4%)

	\ GOTO 32767

					!CCL ENTRY.


32767	C$=SYS(CHR$(9%))

	\ END
