; M80MEMTP
;   David Allday - 24 January 2021
;        version 3
;
; Memory test program for the Nascom MAP80 256k memory card
;  It is designed to test the paging options provided by the MAP80 256k card.
;    but can be used to test nonpaged memory by setting Numberof64kpages to 0.
; 
;   I got a bit confused as the Pages the manual seem to talk about is rows 0 to 3 whereas
;   the circuit diagram talks about rows 1 to 4 
;   I believe page 0 is row 1.
;  
;  Since we are talking about a NASSYS program the memory from 0000H to 07FFH 
;     will always be used by the monitor, working ram and video ram.
;     So testing between 0000H and 07FFH will not be very useful 
;     ( unless page count set to 0 and you avoid the ram used by NASSYS and this program )
;
;  The progam will always be in memory 0C80H no matter what paging it does.
;
;  if start address is between 1000H and 7FFFH and the end address is 8000H or below then
;        it will test each 32k page using the lower 32k of memory.
;
;  if start address is between 8000H and FFFFH and the end address is 0000H or  > 8000H and <= FFFFH then
;        it will test each 32k page using the upper 32k of memory.
;
;  if start address is between 1000H and 7FFFH and the end address is above 8000H then
;        it will test each 64k page of memory.
;
;  The standard 256k memory card has 8 pages of 32k
;    so will check pages 0 to 7 
;  The standard 256k memory card has 4 pages of 64k
;    so will check pages 0 to 3
; 
;  The program calculates the number of test blocks it needs to do depending upon the type of test being done.
;     So for 64k tests the number of blocks will be the number of 64k pages.
;     For 32k tests the number of blocks will be twice the number of 64k pages.
;
;  If you have more than 1 card or have upgraded it to a 1Mbyte card then
;   you can specify the number of 64k pages to test from 1 to 010H ( 16 64k pages )
;
;  Use this at your own risk, I think it works but no guarantees.
;  If you want to use or amend this program please feel free.
;   It would be nice if you mentioned me and let me know how it goes
;            david_nascom@davjanbec.co.uk
;   
; NOTE1:- The opcode test uses NMI so be careful trying to single step this code.
; NOTE2:- The program is written for the NASSYS3 so you cannot test memory locations 000H to 07FFH
;
;***************** CALLING THE PROGRAM
;
; Call the program using 
;  EC80 [startaddress [endaddress [Numberof64kpages] ] ]
;      these values are stored in the program 
;        so a call to EC80 will use the same values as the last call
;   startaddress	The testing starts at this address
;   endaddress		The testing finished at this address -1
;   Numberof64kpages	The number of 64k memory pages to be tested.
;			0 - special case and NO paging is activated.
;			4 - standard M80 256k card fully populated
;			10 - (16 dec) if you have 4 cards or moded the card to provide 1Mbyte.
;  e.g.
;      EC80 8000 9000 2
;        will test memory from 8000 to 8FFF for 4 32k pages 
;                Port EF set to 80, 81, 82, 83 for the tests
;
;      EC80 1000 0000 2
;        will test memory from 1000 to FFFF for 2 64k pages 
;                Port EF set to 00, 01 for the tests
;
;      EC80 1000 0000 0
;        will test memory from 1000 to FFFF without paging. 
;                Port EF is not change for this test.
;
;         Only the low byte is used from lastpage parameter.
;         The end address tested is actually -1 of the endaddress value.
;
; *************** OUTPUT MESSAGES 
;
;   The program will output a message on the top line.
;
;   e.g. in the default call to C80
;      MAP80 MEMTST V3 S:8000 E:FFFF 32KU pg:00L 8888
;      where
;        V3     is the version of the program
;        S:8000 is the first address to be tested
;        E:FFFF is the last address to be tested
;        32KU   means it is using 32k paging mode using the upper 32k of memory
;               This can be 
;                   32KL meaning it is using 32k paging mode using the lower 32k of memory
;                   64K  meaning it is using 64k paging mode
;        pg:00L shows the 32k page being tested either Lower or Upper
;        8888   is the current byte being tested
;
;   A similar message is displayed on the main screen
;
;   S:8000 E:FFFF 08 32KU pages
;      where
;        S:8000 is the first address to be tested
;        E:FFFF is the last address to be tested
;        08     is the number of pages to be tested
;        32KU   means it is using 32k paging mode using the upper 32k of memory
;                This can be  
;                   32KL meaning it is using 32k paging mode using the lower 32k of memory
;                   64K  meaning it is using 64k paging mode
;
;  It then outputs a messafe for each bank as it is zeroised.  
;        Zero:8000 - FFFF pg:00L
;        Zero:8000 - FFFF pg:00U
;        Zero:8000 - FFFF pg:01L 
;        etc . . . . 
;
;  It then outputs a message for each bank as it is tested
;        TEST:8000 - FFFF pg:00L
;        TEST:8000 - FFFF pg:00U
;        TEST:8000 - FFFF pg:01L 
;        etc . . . . 
;    and the current address will be displayed in the right hand side of the top line.
;     
;  The outputs messages will be slightly different for 64k mode and the non-paging mode
; 
;  Any errors encounted will be reported - see RESULTS section below.
;
; ***************   STOPPING THE PROGRAM 
;
; The program monitors the keyboard during the test and any key will interrupt it.
;   This will undo the effect of the C button on the "press SP, C or Q" input.
;   NOTE:- it only checks the keys at the end of each page in the initial setting to zero 
;             to avoid slowing down that part of the processing.
;    
; When the program says "press SP, C or Q"
;   after 10 error lines or at end of each cycle
;   	You can press space to continue
;   	press C to continue without stopping for any errors
;   	press Q or anything else to return to NAS-SYS
;
; *****************  Requirements:
;
; It is set to test a fully populated MAP80 256k memory card.
; The program assumes you have NAS-SYS3.
; The program defaults assume that you have a MAP80 256k Ram card fully populated.
; That you are running the program in the WRAM area ( 4114 chip in Nascom 2 board).
; That the card is using the standard control port at 0xFE.
;
; *****************  Processing:
;  It has 2 phases
;  Phase 1
;      It sets all memory locations on all pages to 0x0
;  Phase 2
;      It tests each byte to be zero at the start
;      It does a "bit walk" to check for linked data lines
;      It writes 0xFF to check you can set all bits.
;      If the other tests have worked 
;           ( Originally tested failed memory but there was an issue if ROM memory had certain values caused it to crash?)
;           It uses the Nascom NMI single step function
;           to check that opcodes can work by using LD A,(HL)
;               where HL points at a location with 055H
;      		   and checking it actually worked.
;
; at the end shows "press SP, C or Q" at the end of each cycle
;    space or c starts the process all over again
;    enter Q or any other character to return to NAS-SYS ( see "press SP, C or Q" above )
;
; *******************  Results:
;  memory address with a letter A means
;     The memory location was not 0 when checked.
;         All memory locations are set to zero before the tests begin
;         Non zero here means is was changed by something since the start.
;         It could mean problems with the address lines, or the paging port FE.
;             or that that memory is on the Nascom card and enabling ram disable.
; memory address with a letter B means
;     The bit walk on that byte of memory failed.
; memory address with a letter C means
;     The program could not store 0xFF at that byte.
; memory address with a letter D means
;     The attempt to run opcode test was not successful
;          NOTE:- it is only run if the other tests have worked.
;
;  Changes 
;  Version 2 - January 2021
;  1.changed where the address value was written to on the header line.
;    the routine was outputting a space into the extra space right of the header line
;    This could cause an issue with the keyboard routines if all the values past 0xBF9 were 0x00.
;    The rout routine caould at that point update the kmap table with the space.
;  2.Changed to check key routine to check for any input ( keyboard or uart )
;
;  Version 3 - January 2021
;  1. changed start message to state switching x type pages
;            e.g. switching 8 32KU pages  ( 32k pages into upper 32k of memory )
;  2. moved stack to end of code
;  3. optimised some of the code to save space
;
; Nas-Sys Equates

PORT0:	EQU	0C00H	; copy of port 0
ARGN:	EQU	0C0BH	; number of arguments supplied 
ARG1:	EQU	0C0CH	; Holds program address
ARG2:	EQU	0C0EH	; holds first parameter = start address
ARG3:	EQU	0C10H   ; holds second parameter = end address
ARG4:	EQU	0C12H	; holds third paramter = No of 32k pages
ARG5:   EQU 0C14H   ; holds forth paramter = 
CURSOR:	EQU	0C29H   ; hold position of the cursor on the screen
NMI:	EQU	0C7DH	; address to hold NMI routine - starts with a JP 


; these are the RST values - some require other bytes to follow them
; e.g.
; RST ROUT	; output a single character
; 

RIN:  	EQU	08H	; wait for a character
SCAL:	EQU	18H	; Subroutine call - follow by number of routine
PRS:	EQU	28H ; Output the following characters until a 0 is encountered
ROUT:	EQU	30H ; Output to output a single character

;  values to be used with SCAL 
;     e.g. 
;        RST  SCAL
;	     DEFB TBCD3

MRET:	EQU	5BH	; return control to NAS-SYS
KDB:	EQU	61H	; scan keyboard
ZIN:    EQU 62H ; scan for input
BLINK:	EQU	7BH 	; blink cursoe and wait for input 
TBCD3:	EQU	66H	; output HL as ascii plus space
TBCD2:	EQU	67H	; output A as ascii + add to C
B2HEX:	EQU	68H	; output A as ascii
SPACE:	EQU	69H	; output a space
CRLF:	EQU	6AH	; output a cr and lf

; controls for the screen
CS:	EQU	0CH	; Clear screen used with RST PRS
CR:	EQU	0DH	; Do character return
ESC:	EQU	1BH	; clear line and put cursor at start
;
;
;
	ORG	0c80H	; had to use 0C80H as program needs all the working Ram :)
	JP	START

STOREADDR:	DEFW	0H	; store address we got to as screen updated

NO64KPAGES:	DEFB	04	; the number of 64k pages involved in the tests
                        	; when doing 32k paging tests we double this number
				; for standard 256k card the max is 4
				; you can go higher with more cards

PAGEINCR: DEFB  01		; The incremental value to switch to the next page
				
PAGETYPE: DEFB	00		; the value to be loaded into OFE to set the first page
				; it will be changed to 080H for testing upper 32k pages
				; or 0C0H for testing lower 32k pages.
			

TESTVALUE:	DEFB	055H	; what to load to the memory
TESTCYCLE: 	DEFB	0	; Test cycle - either set memory to zero or do tests
SAVESTACK:	DEFW	0H	; save Stack pointer before doing NMI
SAVENMI:	DEFW	0H	; save area for old NMI address
ERRORS:		DEFB	0H	; how many errors have occurred
WAITONERROR:	DEFB	0H	; set to zero to wait after 10 errors

SAVERESULT:	DEFB	0H	; save the returned byte

	; normally 8000 to 0000 to cover the top 32k
STARTADDR:	DEFW	08000H	; start address for test of a page
ENDADDR:	DEFW	00000H	; end address for test of a page


START:	
	LD SP,TSTSTACK		; use our own stack

	LD   HL,(CURSOR)	; save current cursor position
	PUSH HL

	LD   HL,0BCAH	; set screen to heading line
	LD (CURSOR),HL	
	RST PRS		; clear current line
	DEFB ESC,0

	RST PRS
	DEFM "MAP80 MEMTST V3"
	DEFB 0

	POP HL		; restore cursor position
	LD (CURSOR),HL	

	XOR A
	LD (WAITONERROR),A	; so we wait after 10 error lines
	; ---- check if paramters supplied
	LD  A,(ARGN)	; load how many arguments 
	OR  A
	JR  Z,NOPARMS	; no paramters given just E entered
	CP  1
	JR  Z,NOPARMS	; just the Execute address given
	CP  2
	JR  Z,TWO
	CP  3
	JR  Z,THREE
	; assume we have 4 or more parameters - will ignore the rest
	; drop down to 4 parameters
	; Load parameters into local memory 
	; so if called again same values will be used
	
FOUR:	LD A,(ARG4)		; just the low bytes from this parameter
	; allow 0 as a special case - means no paging 
FOUROK:
	LD (NO64KPAGES),A	; store number of 64k pages to process

THREE:	LD HL,(ARG3)	; new end address
	LD (ENDADDR),HL

TWO:	LD HL,(ARG2)	; new start address
	LD (STARTADDR),HL

NOPARMS:	; paramters all loaded
		; check the type of test being done.
	LD A,1	; set page increment value to 1 used for 32k pages
	LD C,A
	; set up addresses
	LD HL,(ENDADDR)     ; finish address
	LD DE,(STARTADDR)     ; start address
	DEC HL		; get the real finish address in HL


TESTTYPE:		; check if doing lower upper 32k or 64k

	LD A,D 		; get the high byte of the START address
	ADD 80H		; adding 80H will set carry flag if greater than 80H
	JR C,TOP32K	; if carry set then start address is in top 32k
			; else start address is in bottom 32k  
	LD A,H	 	; get the high byte of the end address
	ADD 80H		; adding 7FH will set carry flag if greater than 80H
	JR C,FULL64K	; if carry set then end address is in top 32k so doing 64k paging

LOW32K:
	LD A,0C0H	; value to output to use the lower 32k for paging 
	LD B,A
	JR SETPAGETYPE
TOP32K: LD A,080H	; value to output to use the upper 32k for paging
	LD B,A
	JR SETPAGETYPE

FULL64K: 
	LD A,00H	; value to output to test the first 64k page
	LD B,A
	INC BC		; increment value so page increment is now 2

SETPAGETYPE:
	LD (PAGEINCR),BC	; save increment value and what type of test is being done 


	
; ------------------------- display details about the test.
DISPLAYSTART:

	LD  DE,(CURSOR)	; hold current cursor position
	LD	HL,0BDAH	; set screen to heading line
	LD (CURSOR),HL
	CALL OUTRANGE   ; test range 
	CALL DISPSIZE   ; test type 32k or 64k and paging ?
	LD (CURSOR),DE ; put cursor back onto main screen

    ; move onto screen and repeat details
	CALL OUTRANGE

    CALL GETPAGETYPE
    ; Sets C to the page type (PAGETYPE) 
    ; Sets B to the number of pages to do ( NO64KPAGES ) 
    ;  will be *2 if doing 32k pages (PAGETYPE != 0 )
    ; A == B and the Z flag is set if PAGETYPE == 0 ( 64k paging )
    
    OR A
	JR Z,NOPAGING	; zero means no paging 

	RST PRS
	DEFM "switching "
	DEFB 0

	LD  A,B		; Number set earlier by GETPAGETYPE 
	RST SCAL        ; print  count number
	DEFB B2HEX
	RST SCAL        ; print space
	DEFB SPACE
	
	CALL DISPSIZE   ; output type of test being done

	RST PRS
	DEFM " pages"
	DEFB 0
	JR  WAITSTART

NOPAGING:
    ; nothing to do if not paging
    
WAITSTART:	

; new line before we start the test
    RST SCAL
    DEFB CRLF

; -------------- wait for key press before starting
; removed so it goes straight in
;	RST PRS
;	DEFB CR
;	DEFM "To start "
;	DEFB 0
;	CALL WAIT
;
; -------------------------- start running the tests

LOOP:	; start by setting test type
	
	LD	A,1		; 2 cycles ( zerioise and then test )
	;   firstly to zeroize all pages
	;   second to do bit walk
	;             and opcode test
	LD	(TESTCYCLE),A

NXTCYCLE:	; move to next cycle

PAGESTART:	; start here for new pages

    CALL GETPAGETYPE    
    ; Sets C to the page type (PAGETYPE) 
    ; Sets B to the number of pages to do ( NO64KPAGES ) 
    ;  will be *2 if doing 32k pages (PAGETYPE != 0 )
    ;

NXTPAGE:
	LD A,B		; retrieve counter to check if paging 
	OR A		; should only be zero first time if not paging
	JR Z,SKIPPAGING	; if the count was zero then no paging 
	LD A,C          ; get page type - the value to set the page mode
	OUT (0FEH),A	; set memory
	LD A,(PAGEINCR) ; will only add 1 or 2 to the C register ready for next page         
	ADD A,C
	LD C,A		; save it again
	JR SETADDRESS
SKIPPAGING:
	INC B		; increment B from 0 to 1 to do 1 cycle
SETADDRESS:
	PUSH BC		; save counter and next port value        
	; set up addresses
	LD HL,(ENDADDR)     ; finish address
	LD DE,(STARTADDR)     ; start address

	; decide which test is being done
	LD	A,(TESTCYCLE)	; check what test we are doing
	CP	2
	JP	Z,BITWALK
	; else number 1 is zero all memory

ZERO:
;   set all memory addresses to value 0

 	CALL OUTZERO	 ; output start message 

	XOR A		 ; clear carry flag
	SBC HL,DE        ; calculate count

	LD B,H           ; and in BC for the LDIR code
	LD C,L
	LD HL,(STARTADDR); start address
	LD D,H           ; and in DE
	LD E,L

	; LDIR has an issue if any ROM in the way
	;  so doing own loop
	;  We need to implicity set each address to zero
        ; the LDI instruction moves (DE) to (HL) but they are the same address.
ZBYTE:
	; removed these as sloowwww down the zeroise process
	; CALL OUTADDHEADER	 ; put address on screen
	; CALL CHECKKEY	; check for key press
	LD (HL),0       ; copy 0 to location 
	LDI             ; next location and dec BC

	JP PE,ZBYTE       ; loop till end reached

	CALL CHECKKEY	; check for key press after each zeroisation

	JP	ENDPAGE


BITWALK:	; test page to see if is starts at zero
                ; then do bit walk

 	CALL OUTTEST	    ; output start message 
	XOR A		 ; ensure flags are reset
	SBC HL,DE        ; calculate count
	LD B,H           ; and in BC for the LDIR code
	LD C,L
	LD HL,(STARTADDR); reload start address

;--------- loop through all addresses
ZCHECK:
	LD D,0		 ; clear error flag
	CALL OUTADDHEADER	 ; put address on screen
	XOR A            ; 
	CP (HL)          ; memory should be zero from original zeroising
	LD A,"A"         ; load error letter in case it failed
	CALL NZ,ERR      ; Fault A if non zero from cp statement

; -------------------    walking bit test
	LD A,1           ; intial bit position 
WALK:	LD (HL),A        ; put into memory
	CP (HL)          ; did it get there
	PUSH AF          ; save bit pattern
	LD A,"B"         ; load error code in case
	CALL NZ,ERR      ; report Fault B if walk failed
	POP AF           ; get bit pattern and flags from stack
	JR NZ,LOADFF     ; if it failed jump to next test
	RLA              ; move bit to next position
	JR NC,WALK       ; repeat until the bit reaches the carry flag

;-------------------- Load FF test  
LOADFF:
 	LD A,0FFH      ; Store FF 
	LD (HL),A        ; store FF into test location
	CP (HL)          ; did it get there
	LD A,"C"         ; load error letter in case
	CALL NZ,ERR      ; Fault C as FF not loaded

	; test the running of an opcode in the memory area

	LD A,D           ; get error flag set in ERR routine
	OR A             ; Non-zero if error occured 
	CALL Z,OPCODETEST	; if no other errors test if opcode fetch works
	                    ; opcode test separate routine so we can use DJNZ to loop

	LD A,D           ; get error flag set in ERR routine
	OR A             ; Non-zero if error occured 
	CALL NZ,ERRORLINE   ; output a newline - or wait if too many errors

;--------------------- end of check a single byte
	CALL CHECKKEY	; check for key press
			 ; unless we reset DL it does 
                         ; (HL) -> (DE) and DE is ROM - 0x00xx or 0x01xx
	LD	D,H	 ; reset DE for the LDI
	LD	E,L
	LDI              ; step onto next location
	JP PE,ZCHECK       ; loop till end reached

;----------- end of walk test 

; drop through to ENDPAGE when done

ENDPAGE:	; come here when we have finished test for one page

	POP BC		 ; get counter and port value from stack
	DJNZ NXTPAGE	 ; decement B and jump if not zero

	;RST PRS
	;DEFM "End Test"
	;DEFB CR,0
	;CALL WAIT	 ; wait between test type


; check if we have done all tests
	LD	A,(TESTCYCLE)	; load current test number
	INC	A
	LD	D,A			; store for start check
	LD	(TESTCYCLE),A		; store away again
	CP	3
	JP	NZ,NXTCYCLE


	CALL WAIT	 ; wait after memtest


	JP LOOP          ; go back to start

;---------------------------------------
; -------------------- end of main loop
;---------------------------------------

;------------------------- get page type
; V3 added this routine as used more than once
GETPAGETYPE:
    ; Sets C to the page type (PAGETYPE) 
    ; Sets B to the number of pages to do ( NO64KPAGES ) 
    ;  will be *2 if doing 32k pages (PAGETYPE != 0 )
    ; A == B and the Z flag is set if PAGETYPE == 0 ( 64k paging )
    
    LD  A,(PAGETYPE)	; load the value to set the first page
	LD  C,A       	; value that starts with page 0
	OR  A		; if zero then doing 64k pages
	LD  A, (NO64KPAGES)	; get number of pages to check
	; next test checking previous or
	JR  Z,GETPAGETYPE64K		; flag set by previous OR A 
			; doing 32k pages so need to double the number of tests done
	RLA			; shift left == multiply by 2 ( carry bit reset by OR A )
				; needed so we can do 2 32k tests on each 64k page	
GETPAGETYPE64K:
	LD  B,A			; store away for later count down
    RET


;---------------------- do opcode test
;                       as a subroutine as main loop was too big for JR commands
OPCODETEST:

LOADOPCODE:
	PUSH HL
	LD HL,DUMMY
DUMMY:
	LD A,(HL)	 ; dummy to get code value	
	POP HL
	LD (HL),A	; LD A,(HL)

	PUSH HL		; save current registers
 	PUSH DE
	PUSH BC
	; save current stack so we can reset it later
	LD (SAVESTACK),SP
 
	; set stack pointer for the test
	; LD      SP,TSTSTACK
	; assuming we don't need to and can use our own stack
	; put user pc on top of stack
	PUSH	HL	; add HL to top of stack 

	; do step commands
	; set so the NMI address is our routine
	;  NMI address starts with a JP 
	LD HL,(NMI+1)
	LD (SAVENMI),HL
	LD HL,TRAP
	LD (NMI+1),HL
	LD HL,TESTVALUE	; point HL at the test value
	LD A,0AAH	; set A and then see if it changes value

	; set bit 3 of p0, to activate nmi
	PUSH AF
	LD   A,8
	OUT  (0),A
	POP  AF
        ; execute one step of program
	RETN	; not sure why RETN?

; come here after nmi or bpt
TRAP:
	; reset NMI address
	LD HL,(SAVENMI)
	LD (NMI+1),HL
	LD	(SAVERESULT),A	; save result
	; reset nmi bit in p0
	LD   A,(PORT0)
	OUT  (0),A

	; set my stack pointer
	LD      SP,(SAVESTACK)

	POP	BC	; restore registers
	POP	DE
	POP	HL

	; output the A register
	LD	A,(SAVERESULT)
	LD	E,A

; DEBUG - output value returned
;	RST     SCAL
;	DEFB	B2HEX
;	RST SCAL
;	DEFB SPACE       ; and space

	LD	A,(TESTVALUE)	; check if it is the same as LD A command 
	CP	E
	LD	A,"D"	 ; set error code to D
	CALL NZ,ERR      ; Fault D if the LD A,TESTVALUE failed

	RET

; -----------------   Output TEST Start Address and page number
OUTTEST: 

	RST PRS
	DEFM "TEST:"
	DEFB 0
    JR OUTADDR      ; jump to output address

; -----------------   Output zeroize message 
;                     Start Address in DE 
;                     BC - port value and count
OUTZERO: 
	RST PRS
	DEFM "Zero:" 
	DEFB 0
	
; drop through to output address
; ---------------- output the current address 
OUTADDR: 
    ; save registers we will use
	PUSH BC		; BC has port value and count
	PUSH HL		; End Address

	; extra saves so we call pull for message
	PUSH BC		; BC has port value and count
	PUSH HL		; End Address
	PUSH DE		; DE start address

  	POP HL		 ; gets start address
	RST SCAL         ; print  address
	DEFB TBCD3

	RST PRS
	DEFM "- " 
	DEFB 0


  	POP HL		 ; gets end address
	DEC HL		 ; dec to show actual end address
	RST SCAL         ; print  address

	DEFB TBCD3
  	POP HL		 ; gets port value and count
	CALL OUTPAGE	; output page being tested

	RST SCAL
	DEFB CRLF	 ; do a new line

    ; reset registers 
    POP HL
    POP BC

	RET

;------------- output page number 
; in header area and on current line
OUTPAGE: ; call here to output page details in the header
	; at the heading line
	PUSH HL		; save address to stack
	LD HL,(CURSOR)
	EX (SP),HL	; exchange cursor value with address in stack
	PUSH HL		; re save address to stack
	LD HL,0BEDH	; set screen to heading line
	LD (CURSOR),HL	
	POP HL 		; return address
	CALL OUTPAGE1	; output details
	EX (SP),HL	; get cursor and save HL
	LD (CURSOR),HL
	POP HL		; get address

OUTPAGE1:	; call here to output page where the cursor is
	LD A,(NO64KPAGES)
	OR A
	RET Z	; return if doing non paging processing
	RST PRS
	DEFM "pg:"      ; V2 shorten the page text to accommodate moving the address value
	DEFB 0
	LD A,L		; bring down just the lower parts of L
			; which is the Port value to set page
	DEC A		; actually the next page so reduce by 1
			; ( Port value incremented after setting port )
    	AND 3FH		; remove the 2 control bits
	SRL A		; shift to right by 1 and sets carry flag
	PUSH	AF	; save the carry flag
	RST SCAL    ; print  port value
	DEFB B2HEX
	;RST SCAL         ; output space
	;DEFB SPACE
	LD  A,(PAGETYPE) 	; check type of test
	OR A		; or wih self will set z flag if zeros
	JR Z,PAGE64	; 64k tests
	POP AF		; Get Carry flag
	LD A,"U"
	JR C,UPPER	; if carry set then upper 32k page
	LD A,"L"
UPPER:
    	RST ROUT	; output the single character
	RET
PAGE64:			; no further action
	POP AF
	RET

; -----------------   Output Address in HL on header and then reset cursor 
OUTADDHEADER: 
	PUSH BC          ; save registers BC as TBCD3 adds to C
	PUSH DE          ; No need to save A register
	PUSH HL
	LD DE,(CURSOR)   ; get current cursor on screen
	LD HL,0BF5H	; set screen to heading line
	             ; V2 moved placment of the address value left by 1
	LD (CURSOR),HL	
	POP HL
	PUSH HL
	RST SCAL         ; print current address
	DEFB TBCD3
	LD (STOREADDR),HL    ; save address we may bomb
	LD (CURSOR),DE   ; replace old cursor on screen
	POP HL
	POP DE
	POP BC           ; get count back
	RET              ; return


; -----------------   Error routine
ERR: 
	PUSH AF          ; save Flags
	PUSH BC          ; save count
	RST ROUT         ; output error letter
	RST SCAL
	DEFB SPACE       ; and space
	RST SCAL         ; output address
	DEFB TBCD3
	INC D            ; tell calling code Err called
	POP BC           ; retrieve count
	POP AF           ; retrieve flags
	RET              ; return

; ------------------ routine to output a newline on screen
;                    if an error has occurred
;                    and wait if 10 errors have occurred
;                    ( wait may not wait if WAITONERROR is not Zero )
ERRORLINE:
	RST SCAL
	DEFB CRLF
	LD A,(ERRORS)
	INC A
	CP 0AH
	JR NZ,ERRJUSTCR
	CALL WAIT
	XOR A	; reset error count 
ERRJUSTCR: 
	LD (ERRORS),A	; store new error count

	RET


; ---------------------------- check for key press
CHECKKEY:	; check for key press
            ; removed register saves as not needed
	RST  SCAL
	DEFB ZIN	; scan for input V2 - changed to use any input to wait
	RET   NC
	XOR  A
	LD (WAITONERROR),A	; reset wait on error

; -----------------   wait until key pressed

WAIT:
 	PUSH DE          ; DE used for cursor position
	LD DE,(CURSOR)   ; get current cursor on screen
	LD A,(WAITONERROR)
	OR A
	JR NZ,CONT1
	RST PRS
	DEFM "press SP, C or Q"
	DEFB 0

	RST RIN          ; wait for character returned in A
	CP " "		     ; if space carry on 
	JR Z,CONT1     ; else jump to warn start nassys 3
	CP "C"		; C will continue without stopping
	JR NZ,NASSYS	; exit if anything else 

	LD (WAITONERROR),A	; set it so it does not wait

CONT1:
	LD (CURSOR),DE   ; replace old cursor on screen
	RST PRS		; clear current line
	DEFB ESC,0

	POP DE           ; get old DE
	RET              ; return
NASSYS:
	RST SCAL
	DEFB CRLF	 ; do a new line
	RST SCAL 	 ; return to NasSys3
	DEFB MRET


;---------------------- output the range of the memory test
;                       
OUTRANGE:
	RST PRS
	DEFM "S:"
	DEFB 0

	LD HL,(STARTADDR)     ; start address
	RST SCAL         ; print address
	DEFB TBCD3

	RST PRS
	DEFM "E:"
	DEFB 0
	LD HL,(ENDADDR)     ; finish address
	DEC HL
	RST SCAL         ; print address
	DEFB TBCD3
    RET 
    
;-------------------- V3 - output the type of test 
DISPSIZE:
    ; Sets C to the page type (PAGETYPE) 
    ; Sets B to the number of pages to do ( NO64KPAGES ) 
    ;  will be *2 if doing 32k pages (PAGETYPE != 0 )
    ;  A == B and the Z flag is set if PAGETYPE == 0 ( 64k paging )
    CALL GETPAGETYPE
    
    ; LD  A,B     ; retrieve no64kpages - already in A

	OR  A
	JR  Z,DISPEND		; if zero then no paging
	LD A,C          ; load page type
	OR A		; check for Zero by using OR
	JR Z,REP64K ; if (PAGETYPE) is zero then doing 64k pages
			; doing 32k pages
	RST PRS
	DEFM "32K"
	DEFB 0
	LD A,C          ; load page type again
	CP 80H
	LD A,"U"
	JR Z,DISPUL
	LD A,"L"
DISPUL:
	RST ROUT	; output the single character
	RET         ; return as now finished
REP64K:
	RST PRS
	DEFM "64K"
	DEFB 0
DISPEND:
	RET

; V3 moved stack to end of code
;  our own stack to use - also used for opcode call
; used values so I could check and see how much we used 
; after run seemed to only get down to 27 leaving 28 (8 bytes ) free
;  so commented out the other spaces
STACKSPACE:
;		DEFB 	3FH, 3EH, 3DH, 3CH, 3BH, 3AH, 39H, 38H
;		DEFB 	37H, 36H, 35H, 34H, 33H, 32H, 31H, 30H

		DEFB 	2FH, 2EH, 2DH, 2CH, 2BH, 2AH, 29H, 28H
		DEFB 	27H, 26H, 25H, 24H, 23H, 22H, 21H, 20H
		DEFB 	1FH, 1EH, 1DH, 1CH, 1BH, 1AH, 19H, 18H
		DEFB 	17H, 16H, 15H, 14H, 13H, 12H, 11H, 10H
		DEFB 	0FH, 0EH, 0DH, 0CH, 0BH, 0AH, 09H, 08H
		DEFB 	07H, 06H, 05H, 04H, 03H, 02H, 01H, 00H

TSTSTACK:	EQU $		; the program stack starts here
    


;End of code


