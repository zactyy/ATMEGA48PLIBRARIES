; * Project: memoryAllocation		                               *
; * Author: Zachary Tyynismaa(please attribute properly            *
; * File: memoryAllocation.asm							           *
; * Description: This assembly language code template is designed  *
; *              provide typical code requided for all programs.   *
; *                                                                *
; * Inputs: Takes commond through X and Y registers  			   *
; *                                                                *
; * outputs: Pointers, and memory allocation					   *
; *                                                                *
; * Computations: Free, Malloc									   *
; *                                                                *
; * Revision History: 2/24/2013 9:22:38 AM (Created program)       *
; ******************************************************************
; ******************************************************************
; *                                                                *
; *                   include File Definitions                     *
; *                                                                *
; ******************************************************************

; ******************************************************************
; *                                                                *
; *                   Code Segment Definitions                     *
; *                                                                *
; ******************************************************************


; AddressReturnAndSetBlock
; This routine when given an XL and number of
; XH(000 = 1) sets the bits in the handler and returns
; the proper address
;
; Input:	XL, XH	- internal managment code,  number blocks
; output:	XH:XL	- pointer to the address of allocated memory
; Errors:	none
; Uses:	XH and XL (r27:r26) are used, all other registers are preserved
AddressReturnAndSetBlock:
	push r24				;	Push Temp values onto stack to restore after
	push r25				;	opertation
	clr r16				;	clear Temp
	;read pool indicator
	bst XL, 0				; 
	bld r16, 0				;	move the pool value from the addressing code
	clt						;	into temp
	bst XL, 1
	bld r16, 1
	clt

	;Based on Temps Value branch to the correct Pool
	tst r16				
	breq Pool0Set				
	cpi r16, 1
	breq Pool1Set	
	cpi r16, 2
	breq Pool2Set	
	cpi r16, 3
	breq Pool3Set	


	;	Get the block value from addressing code
	;   block code 000 corresponds with block 0
	GetBlock:					
	clr r16					
	bst XL, 4
	bld r16, 0
	clt
	bst XL, 5
	bld r16, 1
	clt
	bst XL, 6
	bld r16, 2
	ret

	Pool0Set:
	rcall GetBlock				;get starting block
	ldi	r24, 0x04				;load pool starting address
	ldi r25, 0x01
	clr r17
		P0Addset:					;iterate pointer to block address
		cp r17, r16
		breq SetBlockBits
		adiw r25:r24, 8
		inc r17
		rjmp P0Addset

	Pool1Set:						;get starting block
		rcall GetBlock	
		ldi	r24, 0x44				;load pool starting address
		ldi r25, 0x01
		clr r17
		P1AddSet:					;iterate pointer to block address
			cp r17, r16
			breq SetBlockBits
			adiw r25:r24, 16
			inc r17
			rjmp P1AddSet

	Pool2Set:						;get starting block
		rcall GetBlock
		ldi	r24, 0xC4				;load pool starting address
		ldi r25, 0x01
		clr r17
		rjmp P1AddSet						;iterate pointer to block address
		

	Pool3Set:						;get starting block
		rcall GetBlock
		ldi	r24, 0x44				;load pool starting address
		ldi r25, 0x02
		clr r17
		rjmp P1AddSet					;iterate pointer to block address

	SetBlockBits:			; load Temp2 with  pool management byte
		cpi r25, 0x01
		brne HigherSet			;address is higher
		cpi r24, 0x44
		brlo LowerSet			;address is lower
		cpi r24, 0xC4
		brsh MiddleSet		;address is Pool2Set
		lds r17, 0x0101	;address is Pool1Set
		rjmp LoadBlockSet
	
	LowerSet:
		lds r17, 0x0100	;address is Pool0Set
		rjmp LoadBlockSet
	
	HigherSet:
		cpi r24, 0x44		
		brlo MiddleSet			;address is between(lower)
		lds r17, 0x0103	;address is Pool3Set
		rjmp LoadBlockSet
	
	MiddleSet:
		lds r17, 0x0102	;address is Pool2Set
	
	LoadBlockSet:
		mov XL, r17
		clr r17
		
		;rotate to starting block bit of managment byte
		BlockBitsSetBegin:			
			cp r17, r16
			breq BlockBitsSet
			inc r17
			bst XL,0
			ror XL
			bld XL,7
			rjmp BlockBitsSetBegin
		
		
		;rotate and set block bits	
		BlockBitsSet:					
			set
			bld XL,0
			clt
			inc r17
			dec XH
			cpi r17,8			; have you finished rotating
			breq ExitAddressReturnAndSetBlock		; go to exit
			tst XH				; are there any XH left
			breq FinishRotating
			bst XL,0
			ror XL
			bld XL,7
			rjmp BlockBitsSet

		FinishRotating:
			bst XL,0
			ror XL
			bld XL,7
			inc r17
			cpi r17,8				; have you finished rotating
			breq ExitAddressReturnAndSetBlock		; go to exit
			rjmp finishRotating
			

		ExitAddressReturnAndSetBlock:			;write back out modified management byte
			bst XL,0
			ror XL
			bld XL,7
			cpi r25, 0x01
			brne HIGHERS
			cpi r24, 0x44
			brlo LOWERS
			cpi r24, 0xC4
			brsh MIDDLES
			sts 0x0101, XL
			rjmp STOREBLOCK
	
			LOWERS:
				sts 0x0100, XL
				rjmp STOREBLOCK
		
			HIGHERS:
				cpi r24, 0x44
				brlo MIDDLES
				sts 0x0103, XL
				rjmp STOREBLOCK
	
			MIDDLES:
				sts 0x0102, XL
	   
		    STOREBLOCK:
				mov XL, r24
				mov XH, r25
				pop r25
				pop r24
				ret
			


; Malloc
; This routine when given an XL as the size of memory
; to be allocated finds free sequencial memory and
; sends an internal code to another program which
; sets the management bits and returns the address
; which this program then returns
;
; Input:	XL(r26)	- 
; output:	XH:XL	- pointer to the address of allocated memory
; Errors:	none
; Uses:	XH and XL (r27:r26) are used, all other registers are preserved
Malloc:
	;do we need to allocate anything
	tst XL
	breq returnNull
	
	;so now we need to allocate
	push r16			;push temp registers
	push r17
	push r10
	clr r10   ;need this empty to story address code

	;see if between block sizes if so go to greater
	mov r16, XL
	ANDI r16, 0x0F
	cpi r16, 9
	brsh Greater
	
	;"divide" by 8 to get the number of blocks 
	lsr XL
	lsr XL
	lsr XL

	tst XL			;is it less than 8 bytes?
	brne CheckPool0 
	inc XL
	push XL			;ensure persistancy
	;go check zero pool
	rjmp checkPool0
	
	;returnNull routine 
	
	
	;"divide" by 8 and add 1 to get the number of blocks 
	Greater:
		lsr XL
		lsr XL
		lsr XL
		inc XL
		push XL			;ensure persistancy
		rjmp CheckPool0
	returnNull:
		ldi r16, 0x00
		mov XH, r16
		mov XL, r16
		ret 

	;set count and clear sequence, load managment byte, call pool begin
	; if PoolBegin returns null, from here on XL represents number of blocks needed
	CheckPool0:
		ldi XH, 0x08		;count
		clr r16			;sequence count
		lds r17, 0x0100	;managment byte address
		rcall PoolBegin
		tst XH				;if null go to next
		breq CheckPool1
		ret

	CheckPool1:
		pop XL						;the has been a null overwrite pop block count from stack
		bst XL,0
		brtc NotOdd
		inc XL
	NotOdd:	
		lsr XL						;the remaining pools have larger blocks
		push XL
		push r16
		mov r16, r10
		ANDI r16, 0xF0
		mov r10, r16
		pop r16
		swap r10
		inc r10
		swap r10
		ldi XH, 0x08
		clr r16
		lds r17, 0x0101
		rcall PoolBegin
		tst XH
		breq CheckPool2 
		ret

	CheckPool2:
		pop XL				;the has been a null overwrite pop block count from stack
		push XL
		push r16
		mov r16, r10
		ANDI r16, 0xF0
		mov r10, r16
		pop r16		
		swap r10
		inc r10
		swap r10
		ldi XH, 0x08
		clr r16
		lds r17, 0x0102
		rcall PoolBegin
		tst XH
		breq CheckPool3
		ret

	PNS:				;number of nessacary bits not sequential
		tst XH			;is rotation finished
		breq returnNull ;if it is return null
		clr r16		;if not clear sequence count and return to loop
		inc r10	;increment address code
		rjmp PoolBeginAgain

	CheckPool3:
		pop XL
		push XL				;the has been a null overwrite pop block count from stack
		push r16
		mov r16, r10
		ANDI r16, 0xF0
		mov r10, r16
		pop r16		
		swap r10
		inc r10
		swap r10
		ldi XH, 0x08
		clr r16
		lds r17, 0x0103
		rcall PoolBegin
		ret

	;find consecutive bits for blocks specified by XL
	PoolBegin:
		clt
		dec XH				;decrement count
		bst r17,0			;load zero bit into T
					
		brts PNS			;if t flag set block is in use go to pool not sequential
		inc r10	;increment address code
		inc r16			;incrent number of bits sequentially open
		cp r16, XL			;do I have enough clear bits
		breq MallocEnd	; if I do go to allocate end
		tst XH				; if not have I finished the count
		brne PoolBeginAgain	; if I have return null
		rcall returnNull
		ret 
		
		PoolBeginAgain:
			bst r17,0
			ror r17	;rotate temp2 management byte copy
			bld r17,7
			rjmp PoolBegin

   ;go set memory managment byte and return pointer	
	MallocEnd:
		sub  r10, XL
		swap r10					;move pool nibble to right and block to left
		push XL
		mov  XL,r10				;move addressing code into xl for next routine
		pop  XH								;mov count into XH for the next routine 
		rcall AddressReturnAndSetBlock		;call routine with x = addresscode:blocks
		pop r17							;pop off address from rcall
		POP r16
		POP r16							
		pop r10					;pop data and return
		pop r17 
		pop r16
		ret


; AddressFreeClearBlocks
; This routine when given an XL and number of
; XH(000 = 1) clears the bits in the handler and returns
;
; Input:	XL, XH	- internal managment code,  number blocks
; output:	XH:XL	- pointer to the address of allocated memory
; Errors:	none
; Uses:	XH and XL (r27:r26) are used, all other registers are preserved
AddressFreeClearBlocks:
	push r24				;	Push Temp values onto stack to restore after
	push r25				;	opertation
	clr r16				;	clear Temp
	;read pool indicator
	bst XL, 0				; 
	bld r16, 0				;	move the pool value from the addressing code
	clt						;	into temp
	bst XL, 1
	bld r16, 1
	clt

	;Based on Temps Value branch to the correct Pool
	tst r16				
	breq Pool0Clear				
	cpi r16, 1
	breq Pool1Clear	
	cpi r16, 2
	breq Pool2Clear	
	cpi r16, 3
	breq Pool3Clear	

	Pool0Clear:
		rcall GetBlock				;get starting block
		ldi	r24, 0x04				;load pool starting address
		ldi r25, 0x01
		clr r17
		P0AddClear:					;iterate pointer to block address
		cp r17, r16
		breq ClearBlockBits
		adiw r25:r24, 8
		inc r17
		rjmp P0AddClear

	Pool1Clear:						;get starting block
		rcall GetBlock	
		ldi	r24, 0x44				;load pool starting address
		ldi r25, 0x01
		clr r17
		P1AddClear:					;iterate pointer to block address
			cp r17, r16
			breq ClearBlockBits
			adiw r25:r24, 16
			inc r17
			rjmp P1AddClear

	Pool2Clear:						;get starting block
		rcall GetBlock	
		ldi	r24, 0xC4				;load pool starting address
		ldi r25, 0x01
		clr r17
		rjmp P1AddClear
	Pool3Clear:						;get starting block
		rcall GetBlock	
		ldi	r24, 0x44				;load pool starting address
		ldi r25, 0x02
		clr r17
		rjmp P1AddClear
	ClearBlockBits:			; load Temp2 with  pool management byte
		cpi r25, 0x01
		brne HigherClear	;address is higher
		cpi r24, 0x44
		brlo LowerClear			;address is lower
		cpi r24, 0xC4
		brsh MiddleClear		;address is Pool2Set
		lds r17, 0x0101	;address is Pool1Set
		rjmp LoadBlockClear
	
	LowerClear:
		lds r17, 0x0100	;address is Pool0Set
		rjmp LoadBlockClear
	
	HigherClear:
		cpi r24, 0x44		
		brlo MiddleClear		;address is between(lower)
		lds r17, 0x0103	;address is Pool3Set
		rjmp LoadBlockClear
	
	MiddleClear:
		lds r17, 0x0102	;address is Pool2Set
	
	LoadBlockClear:
		mov XL, r17
		clr r17
		
		;rotate to starting block bit of managment byte
		BlockBitsClearBegin:			
			cp r17, r16
			breq BlockBitsClear
			inc r17
			bst XL,0
			ror XL
			bld XL,7
			rjmp BlockBitsClearBegin
		
		
		;rotate and set block bits	
		BlockBitsClear:					
			clt
			bld XL,0
			inc r17
			dec XH
			cpi r17,8			; have you finished rotating
			breq ExitAddressFreeClearBlocks		; go to exit
			tst XH				; are there any XH left
			breq FinishRotatingClear
			bst XL,0
			ror XL
			bld XL,7
			rjmp BlockBitsClear

		FinishRotatingClear:
			bst XL,0
			ror XL
			bld XL,7
			inc r17
			cpi r17,8				; have you finished rotating
			breq ExitAddressFreeClearBlocks		; go to exit
			rjmp FinishRotatingClear
			
		ExitAddressFreeClearBlocks:			;write back out modified management byte
			bst XL,0
			ror XL
			bld XL,7
			cpi r25, 0x01
			brne HighersClear
			cpi r24, 0x44
			brlo LowersClear
			cpi r24, 0xC4
			brsh MiddlesClear
			sts 0x0101, XL
			rjmp StoreBlockClear
	
			LowersClear:
				sts 0x0100, XL
				rjmp StoreBlockClear
		
			HighersClear:
				cpi r24, 0x44
				brlo MiddlesClear
				sts 0x0103, XL
				rjmp StoreBlockClear
	
			MiddlesClear:
				sts 0x0102, XL
	   
		    StoreBlockClear:
				mov XL, r24
				mov XH, r25
				pop r25
				pop r24
				ret





; Free
; This routine when given an YL as the size to be freed
; and XH:XL as the starting address of the memory
; finds the appropriate pool and blocks and sends an
; internal code to another routine which then clear
; the freed bits and returns.
;
; Input:	XL:XH, YL	- address pointer,  sizeof
; output:	none
; Errors:	none
; Uses:	XH and XL (r27:r26) are used, all other registers are preserved
Free:
	;do we need to free anything
	tst YL
	brne FreeProceed
	ldi XL, 0
	ldi XH, 0
	ret
	FreeProceed:
	
	;so now we need to allocate
	push r16			;push temp registers
	push r17
	push r10
	clr r10   ;need this empty to story address code

	;see if between block sizes if so go to greater
	mov r16, YL
	andi r16, 0x0F
	cpi r16, 9
	brsh GreaterFree
	
	;"divide" by 8 to get the number of blocks 
	lsr YL
	lsr YL
	lsr YL
	tst YL
	brne GetPoolFree
	inc YL
	rjmp GetPoolFree
	
	;"divide" by 8 and add 1 to get the number of blocks 
	GreaterFree:
		lsr YL
		lsr YL
		lsr YL
		inc YL

	
	;Find which Pool the address is in
	GetPoolFree:
		push r24
		push r25
		push YL
		clr YL
		cpi XH, 0x01
		brne HighersFree
		cpi XL, 0x44
		brlo LowersFree
		cpi XL, 0xC4
		brsh MiddlesFree
		rcall FindBlockPool1	;search pool for block
		swap YL
		inc YL
		mov XL,YL
		pop XH
		rcall AddressFreeClearBlocks
		rjmp ExitFree
		LowersFree:
			rcall FindBlockPool0		;search pool for block
			swap YL
			mov XL,YL
			pop XH
			rcall AddressFreeClearBlocks
			rjmp ExitFree
		HighersFree:
			cpi XL, 0x44
			brlo MiddlesClear
			rcall FindBlockPool3		;search pool for block
			swap YL
			inc YL
			inc YL
			inc YL
			mov XL,YL
			pop XH
			rcall AddressFreeClearBlocks
			rjmp ExitFree
		MiddlesFree:
			rcall FindBlockPool2		;search pool for block
			swap YL
			inc YL
			inc YL
			mov XL,YL
			pop XH
			rcall AddressFreeClearBlocks
		ExitFree:
			pop r25
			pop r24
			pop r10
			pop r17
			pop r16
			ret


		;start at the beggining of pool memory and compare to the pointer
		;iterate through blocks until match
		FindBlockPool0:
			ldi r24, 0x04
			ldi r25, 0x01
			FindBlockPool0Loop:
				cp r24,XL
				cpc r25,XH
				breq FindBlockPoolExit
				ADIW r25:r24, 8
				inc YL
				rjmp FindBlockPool0Loop
			FindBlockPoolExit:
				ret

		FindBlockPool1:
			ldi r24, 0x44
			ldi r25, 0x01
			FindBlockPool1Loop:
				cp r24,XL
				cpc r25,XH
				breq FindBlockPoolExit
				ADIW r25:r24, 16
				inc YL
				rjmp FindBlockPool1Loop

		FindBlockPool2:
			ldi r24, 0xC4
			ldi r25, 0x01
			FindBlockPool2Loop:
				cp r24,XL
				cpc r25,XH
				breq FindBlockPoolExit
				ADIW r25:r24, 16
				inc YL
				rjmp FindBlockPool2Loop

		FindBlockPool3:
			ldi r24, 0x44
			ldi r25, 0x02
			FindBlockPool3Loop:
				cp r24,XL
				cpc r25,XH
				breq FindBlockPoolExit
				ADIW r25:r24, 16
				inc YL
				rjmp FindBlockPool3Loop