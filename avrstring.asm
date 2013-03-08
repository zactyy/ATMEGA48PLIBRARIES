; ******************************************************************
; * Author: Zachary Tyynismaa                                      *             
; * Project: AVR string Utilities	                               *
; *                                                                *
; * File: avrstring.asm											   *
; * Description: This library is intended to provide limited	   *
; *              string handeling routines, construction and dest  *
; *                                                                *
; * Inputs: Takes arguements through the pointer registers		   *
; *                                                                *
; * outputs: Modified strings in memory return values in r10	   *
; *                                                                *
; * Computations: strcpy,strlength,strcmp,strtok,strcat,strReverse *
; *				  strEncode, newString, DeleteString   			   *
; * Dependencies: Malloc and Free form memAlloc library			   *
; *																   *
; * Revision History: 3/8/2013 12:53:58 PM (revised program)       *
; ******************************************************************

; strLength
; This routine will count the number of characters in the
; specified zero terminated string. It does not count the
; zero character at the end of the string.
;
; Input:	X	- pointer to the source string
; output:	SUBROUTINEREG	- count of characters in the string
; Errors:	none
; Uses:	SUBROUTINEREG is used, all other registers are preserved
strlength:
	clr SUBROUTINEREG
	push TEMP			;	push registers for preservation
    push XL				
	push XH   
    strlenloop:
		inc SUBROUTINEREG
		ld TEMP, X+
		cpi TEMP, 0
		brne strlenloop	
	dec SUBROUTINEREG
	pop XH
	pop XL
	pop TEMP
	ret

; strcpy
; This routine will copy a string from one memory buffer to another.
; It will copy all characters up to and including the terminating
; zero from the source buffer to the destination buffer.
; NOTE: to avoid errors you should only use constructor generated pointers
;
; Input:	X	- pointer to the source string
;			Y	- pointer to the destination string
; output:	SUBROUTINEREG	- number of characters copied successfully
; Errors:	none
; Uses:	SUBROUTINEREG is used; all other registers are preserved
strcpy:
	clr SUBROUTINEREG
	push TEMP2
	push TEMP	;	push registers for preservation
    push XL			
	push XH
	push YL
	push YH
	strcpyloop:
		inc SUBROUTINEREG
		ld TEMP, X+
		st Y+, TEMP
		cpi TEMP, 0
		brne strcpyloop
		dec SUBROUTINEREG
		rjmp PopXYTandT2
			  
; strcat
; This routine will copy the specified source string and append it
; to the end of the destination string. All characters in the source
; string up to and including the terminating zero will be copied to
; the destination buffer starting at the terminating zero of the
; destination string.
; NOTE: to avoid errors you should only use constructor generated pointers
;
; Input:	X	- pointer to beginning of source string
;			Y	- pointer to the beginning of the destination string
; output:	SUBROUTINEREG	- number of characters appended
; Errors:	none
; Uses:	SUBROUTINEREG is used; all other registers are preserved
strcat:
	clr SUBROUTINEREG
	push TEMP2
	push TEMP			;	push registers for preservation 
    push XL							
	push XH
	push YL
	push YH
	strcatReadToNull:
		ld TEMP, Y+
		cpi TEMP, 0
		brne strcatReadToNull
		sbiw YH:YL,1
	rcall strcpy
	rjmp PopXYTandT2

; strcmp
; This routine will perform a case sensitive compare of two zero
; terminated strings to determine the relative sort order of the
; two strings. The collating sequence is the standard ASCII code
; sequence.
;
; Input:	X	- pointer to first string
;			Y	- pointer to the second string
; output:	SUBROUTINEREG	- relative sort order
;	1 if first string is > second string
;	0 if both strings are identical
;	-1 if first string is < second string
;	Z flag set if strings are equal
;	C flag cleared if string 1 >= string 2
;	C flag set if string 1 < string 2
; Errors:	none
; Uses:	SUBROUTINEREG and status register are used; all other registers are preserved
strcmp:
	clr SUBROUTINEREG
	push TEMP2		;	push registers for preservation 
	push TEMP						
    push XL
	push XH
	push YL
	push YH
    strcmploop:		
		ld TEMP, X+	;	read character from string
		ld TEMP2, Y+;	read character from string
		cpi TEMP,0	;	has first string terminated
		breq strcmpxNull	;	if so check other character
	compareChar:
		cp TEMP, TEMP2	;	if not compare characters
		breq  strcmploop;	if equal repear previous process
		cp TEMP, TEMP2	;	if not equal see if one is greater
		brsh strcmpxIsGreater;	if so return greater
		rjmp strcmpxIsLower	;	if not return lower
	strcmpxIsEqual: 
		clc				;	clear carry flag
		sez				;	set z flag
		rjmp PopXYTandT2;	return
	strcmpxNull:
		cpi TEMP2,0		;	has second string terminated
		breq strcmpxIsEqual;	if so return equal
		rjmp strcmpxIsLower	;	if not return lower
	strcmpxIsGreater:
		inc r10			;	set relative sort order
		clc				;	clear carry flag
		rjmp PopXYTandT2;	return
	strcmpxIsLower:
		dec r10			;	set relative sort order
		sec				;	set carry flag
		rjmp PopXYTandT2;	return
	

; strtok
; A series of calls to this routine will break a specified zero
; terminated source string into a sequence of tokens. Each of the
; tokens is delimited by a second zero terminated string. The
; routine should replace all occurrences of the delimiter by a
; terminating zero.
; NOTE: to avoid errors you should only use constructor generated pointers
;
; Input:	X	– pointer to the source string
;			Y	– pointer to the delimiter string
;			(Z  - pointer to next token in sequence after init call)
; output:	Z flag is set if the source string cannot be tokenized anymore
; Errors:	none
; Uses:	status, X and Z register is used; all other registers are preserved
strtok:
	clr SUBROUTINEREG	;	clear return register
	cpi XL,0			;	check if pointer is null
	cpc XH, r10
	breq strtokxIsNull
	strtokpreserve:
		push TEMP		;	push registers for preservation 
		push XL								
		push XH
		push YL
		push YH
		ld TEMP, X		;	check if string is terminating
		tst TEMP
		breq strtokIsEnd;	if so end with special return
	strtokloop:
		ld TEMP, X+		;	load character
		cpi TEMP,0		;	check if null
		brne strtokCheckChar	;	if null return
		rjmp strtokSourceTerminating
	strtokCheckChar:	;	if not compare against delimiter
		ld TEMP2, Y+	;	load delimiter character
		cpi TEMP2, 0	;	see if delimiter is terminating
	    breq strtokCheckNext	;	if so we need to check the next source character
		cp TEMP, TEMP2	;	does the source character match the delimiter
		brne strtokCheckChar;	if not try next delimiter character
		clr TEMP2		;	if so clear temp2
		st -X, TEMP2	;	so as to write a zero to the source character address
		adiw XH:XL, 1	;	reincrement the so it is pointing to start of next token
	exitstrtokCheckChar:;	if null return
		mov ZL,XL	;	save current address as it is the
		mov	ZH,XH	;	start of the next token
		pop YH		;	restore registers
		pop YL
		pop XH
		pop XL
		pop TEMP
		ret
	strtokSourceTerminating:
		sbiw XH:XL,1	;	move pointer to back to null
		sez				;	set flag to denote no more tokenization is possible
		rjmp exitstrtokCheckChar
	strtokCheckNext:
		pop  YH			;	reset the pointer for the delimiter string
		pop  YL
		push YL
		push YH
		rjmp strtokloop	;	evaluate next character

	strtokxIsNull:
		mov XL,ZL		;	if given null pointer as operand load Z pointer into X
		mov XH,ZH		;	and proceed as you would on the first call
		rjmp strtokpreserve

	strtokIsEnd:    
	  	clr XL		;	clear X for null error return
		clr XH
		pop YH		;	restor original registers
		pop YL
		pop TEMP	;	but discard original X pointer
		pop TEMP
		pop TEMP
		sez			;	set flag to denote no more tokenization is possible
	    ret

; strReverse
; This routine will reverse all characters, according to position, within a string.
; It should perform an in place reversal. This means that a copy of the string should
; not be made. For example, “cat” becomes “tac”, “cat” is lost. The terminating
; zero should be left at the end of the string.
;
; Input:	X	– pointer to the source string
; output:	SUBROUTINEREG	- number of characters reversed
; Errors:	none
; Uses:	SUBROUTINEREG is used; all other registers are preserved
strReverse:
	clr SUBROUTINEREG
	push TEMP2						;push registers for preservation 
	push TEMP						
	push XL
	push XH
	push YL
	push YH
	clr TEMP2
		 rcall strLength
		 mov YL,XL
		 mov YH,XH
		 mov TEMP2, SUBROUTINEREG
		 adc XL, SUBROUTINEREG
		 clr SUBROUTINEREG
		 adc XH, SUBROUTINEREG
	reverseLoop:					;using temp2 to indicate the middle, pulls chars from X and Y and stores them reciprically
		cpi TEMP2, 2				;it then increments and decrements through memory while decrementing temp2
		brlo reverseDone			
		ld TEMP, -X
		push TEMP
		ld TEMP,Y
	    st X,TEMP
		pop TEMP
		st Y+, TEMP
		dec TEMP2
		dec TEMP2
		inc SUBROUTINEREG
		inc SUBROUTINEREG
		rjmp reverseLoop
	reverseDone:						;exit routine pops back push data
		rjmp PopXYTandT2

; strEncode
; This routine will encode all characters in a string, according to a shift key.
; It should perform an in place encoding. The encoding is based on a shift key
; (positive or negative). The shift key indicates the number to add or subtract from
; the original character to obtain the encoded character. Note that this routine should
; add or subtract from the original character’s ASCII code. For example,
; if ‘A’ is the plaintext character and 3 is the shift key, ‘D’ is the encoded character.
; You do not need to consider wrapping encoded characters (i.e. ‘A’ – 3 Æ ‘>’, not
; ‘X’)The terminating
; zero should be left at the end of the string.
;
; Input:	X	– pointer to the source string, SUBROUTINEREG - the shift key
; output:	SUBROUTINEREG	- the shift key	   
; Errors:	none		 
; Uses:	All registers are preserved
strEncode:
	push TEMP			;	push registers for preservation a
	push XL						
	push XH
	mov TEMP, SUBROUTINEREG	;	move the shift key into a comparable register if its zero return if not begin
	cpi TEMP, 1
	brsh strEncodeBegin
	pop XH
	pop XL
	pop TEMP
	ret
	strEncodeBegin:	;	load charcter from memory, if not null terminator add the shift then store back to memory
		ld TEMP, X	;	repeat until null terminator is read then go to exit routine
		cpi TEMP,0
		breq equal
		ADD TEMP, SUBROUTINEREG
		st X+, TEMP
		rjmp strEncodeBegin
	equal:					;	exit routine pops back push data
		pop XH
		pop XL
		pop TEMP
		ret

; newString
; given a value for string length in XL allocates memory
; and returns pointer to said memory
;
; Input:	XL	– size argument
; output:	X   - pointer to allocated memory
; Errors:	none		 
; Uses:	All registers are preserved
newString:
	inc XL		;	increase size to include header byte
	push r16	;	preserve r16
	mov r16, XL	;	preserve size in r16
	rcall Malloc;	get memory of total size
	st X+, r16  ;	wright size to header increment memory address
	pop r16
	ret

; deleteString
; given a string pointer frees its associated memory
;
; Input:	X	– pointer to the source string,
; output:	none   
; Errors:	none		 
; Uses:	All registers are preserved
deleteString:
	push YL		;	preserve YL
	ld YL, -X	;	read header size value into YL
	rcall Free	;	free memory
	pop YL		;	restore YL
	ret