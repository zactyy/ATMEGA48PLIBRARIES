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
; *                   Code Segment Definitions                     *
; *                                                                *
; ******************************************************************
.equ MPOOL00 = 0x02C0
.equ MPOOL01 = 0x02C1
.equ MPOOL02 = 0x02C2
.equ MPOOL03 = 0x02C3
.equ MPOOL00L = 0xC0
.equ MPOOL00H = 0x02
.equ MPOOL01L = 0xC1
.equ MPOOL01H = 0x02
.equ MPOOL02L = 0xC2
.equ MPOOL02H = 0x02
.equ MPOOL03L = 0xC3
.equ MPOOL03H = 0x02
.equ POOL03L = 0x40
.equ POOL03H = 0x02
.equ POOL02L = 0xC0
.equ POOL02H = 0x01
.equ POOL01L = 0x40
.equ POOL01H = 0x01
.equ POOL00L = 0x00
.equ POOL00H = 0x01
.equ NULL =0x00
;.cseg	
;.ORG 0X0000 
;	rjmp Malloc		
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
	tst XL				
	breq returnNull			;	if null no memory is needed	
	push r18				;	push temp registers
	push r17				;
	push r16				;
	clr r18					;	need this empty to story address code

	mov r16,r26				;	provide operand for DetermineBlockSize
	rcall DetermineBlockSize;	get number of blocks
	mov r27,r16				;	copy number of blocks into r27
	clr r16					;	clear r16 so we can call pool0 management byte
	clr r17					;	clear r17 will hold blockid byter
	rcall LoadPoolByte		;	return memory managment byte to r26

	PoolSearch:
		bst r26,0			;	store the status of the zero position bit
		brts SequenceFail	;	if we encounter a sequence has failed
		inc r18				;	if clear we have another
		ResumeLoop:			;	label for resuming from sequence failure
			bst r26,0
			ror r26			;	rotate  management byte copy
			bld r26,7
			inc r17			;	increment our blockID
			cp r18,r27		;	have we met the sequence requirments?	
			breq BlocksFound;	if we have go to blocks found
			cpi r17,8		;	have we rotated through the entire block?	
			breq ContinueSearch	;	if we have branch so we can try another pool
			rjmp PoolSearch	;	if not, repeat
			ContinueSearch:
			clr r18			;	we have reached 8 so clear the count
			tst r16			;	see if we are transition from the zero pool
			brne NoResizeNeeded	;	if not no resize is need
			bst r27,0		;	if so check if its odd if it is inc and shift right once
			brtc NotOdd
			inc r27
		NotOdd:
			lsr r27
		NoResizeNeeded:
			inc r16			;	increment the POOLID
			cpi r16,4		;	if we've exhausted our search lets end the search
			brsh PoolSearchOver
			rcall LoadPoolByte	;	if not load next byte and repeat
			rjmp PoolSearch
			PoolSearchOver:	;	if exiting in failure
			pop r18			;	pop data
			pop r16
			pop r17
			rjmp returnNull	;	return null pointer
		SequenceFail:
			clr r16
			rjmp ResumeLoop

	BlocksFound:
		sub r17,r27
		swap r17
		or r16, r17
		mov r26,r16
		rcall AddressReturnAndSetBlock
		pop r16					;	pop data and return
		pop r17 
		pop r18
		ret
	returnNull:
		ldi XH, NULL	; set pointer value to 
		mov XL,XH		;
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
	brne FreePointerNotNull
	ldi XL, NULL
	ldi XH, NULL
	ret
	FreePointerNotNull:
	push r16			;so now we need to allocate
	push r17			;push temp registers
	push r10
	clr r10			;need this empty to story address code
	mov r16,r28
	rcall DetermineBlockSize
	mov r10,r16
	
	GetPoolFree:			;	Find which Pool the address is in
		push r24			;	Push these as the will be used to 
		push r25			;	find the block
		clr YL
		ldi r24,POOL03L
		ldi	r25,POOL03H
		cp XL,r24			;	Check if its in Pool3
		cpc XH,r24
		brsh FindBlockPool3
		ldi r24,POOL02L
		ldi	r25,POOL02H
		cp XL,r24
		cpc XH,r25
		brsh FindBlockPool2
		ldi r24,POOL01L
		ldi	r25,POOL01H
		cp XL,r24
		cpc XH,r25
		brsh FindBlockPool1
		ldi r24,POOL00L
		ldi	r25,POOL00H
		cp XL,r24
		cpc XH,r25
		brsh FindBlockPool0
		;TODO - need to do some error check case here
			
	ExitFree:
		swap YL
		or r16,YL
		mov XL,r16
		mov XH,r10
		rcall AddressFreeClearBlocks
		pop r25
		pop r24
		pop r10
		pop r17
		pop r16
		ret


	;start at the beggining of pool memory and compare to the pointer
	;iterate through blocks until match
	FindBlockPool0:
		ldi r24, POOL00L
		ldi r25, POOL00H
		ldi r16, 0x00
		FindBlockPool0Loop:
			cp r24,XL
			cpc r25,XH
			breq FindBlockPoolExit
			ADIW r25:r24, 8
			inc YL
			rjmp FindBlockPool0Loop

	FindBlockPoolLoop:
		cp r24,XL
		cpc r25,XH
		breq FindBlockPoolExit
		ADIW r25:r24, 16
		inc YL
		rjmp FindBlockPoolLoop

	FindBlockPoolExit:
		rjmp ExitFree
		
	FindBlockPool1:
		ldi r24, POOL01L
		ldi r25, POOL01H
		ldi r16, 0x01
		rjmp FindBlockPoolLoop
	
	FindBlockPool2:
		ldi r24, POOL02L
		ldi r25, POOL02H
		ldi r16, 0x02
		rjmp FindBlockPoolLoop

	FindBlockPool3:
		ldi r24, POOL03H
		ldi r25, POOL03H
		ldi r16, 0x01
		rjmp FindBlockPoolLoop

; AddressReturnAndSetBlock
; This routine when given an r26 and r27 sets management bits from
; memory according to r26
;
; Input:	r26, r27	- internal managment code,  number blocks
; output:	memory management updated to reflect clear
; Errors:	none
; Uses:	XH and XL (r27:r26) are used, all other registers are preserved
AddressReturnAndSetBlock:
	push r24				;	Push Temp values onto stack to restore after
	push r25				;	opertation
	push r16				;	
	push r17				;
	rcall ReadPoolID		;   POOLID loaded into r16
	rcall ReadBlockID		;	BLOCKID loaded into r17
	rcall ReturnBlockAddress;   Takes r16 and r17 return address with r25:r24
	rcall LoadPoolByte		;	Loads r16's pool management byte into r26
	push r16
	clr r16	
	rcall RotateToBlock		;	Rotates r26 so r17's block is at zero
	rcall BlockBitsSet		;	Clears r26 per r27, rotates byte back to start
	pop r16
	rcall StorePoolByte		;	Stores r26 back to Pool management memory
	mov XL, r24
	mov XH, r25
	pop r17
	pop r16
	pop r25
	pop r24
	ret

; AddressFreeClearBlocks
; This routine when given an r26 and r27 clears management bits from
; memory according to r26
;
; Input:	r26, r27	- internal managment code,  number blocks
; output:	memory management updated to reflect clear
; Errors:	none
; Uses:	XH and XL (r27:r26) are used, all other registers are preserved
AddressFreeClearBlocks:
	push r24			;	Push Temp values onto stack to restore after
	push r25			;	opertation
	push r16			;
	push r17			;
	rcall ReadPoolID	;	Reads PoolID into r16 
	rcall ReadBlockID	;	Reads BLOCKID into r17
	rcall LoadPoolByte	;	Loads r16's pool management byte into r26
	push r16			;	Stores POOLID
	clr r16	
	rcall RotateToBlock	;	Rotates r26 so r17's block is at zero, clears r16
	rcall BlockBitsClear;	Clears r26 per r27, rotates byte back to start
	pop r16				;	Restores POOLID
	rcall StorePoolByte	;	Stores r26 back to Pool management memory
	pop r17
	pop r16
	pop r25
	pop r24
	ret



;LoadPoolByte:
;This routine when given a POOLID will load and return its value in r26
; 
; Input:	r16
; output:	r26
; Errors:	none
; Uses: r26 is used, all other registers are preserved
LoadPoolByte:
	tst r16				
	brne LoadPool1Byte
	rjmp LoadPool0				
	LoadPool1Byte:
		cpi r16, 1
		breq LoadPool1
	LoadPool2Byte:
		cpi r16, 2
		breq LoadPool2	
	LoadPool3Byte:
		cpi r16, 3
		breq LoadPool3	
	LoadPool0:
		lds r26,MPOOL00	;load pool0 management byte
		ret
	LoadPool1:
		lds r26,MPOOL01	;load pool1 management byte
		ret
	LoadPool2:
		lds r26,MPOOL02	;load pool2 management byte
		ret
	LoadPool3:
		lds r26,MPOOL03	;load pool3 management byte
		ret

; StorePoolByte:
; This routine when given a POOLID will store r26 into pool management
; 
; Input:	r16,r26
; output:	none
; Errors:	none
; Uses:  all other registers are preserved
StorePoolByte:
	tst r16				
	brne StorePool1Byte
	rjmp StorePool0				
	StorePool1Byte:
		cpi r16, 1
		breq StorePool1
	StorePool2Byte:
		cpi r16, 2
		breq StorePool2
	StorePool3Byte:
		cpi r16, 3
		breq StorePool3
	StorePool0:
		sts  MPOOL00,r26	;Store pool0 management byte
		ret
	StorePool1:
		sts  MPOOL01,r26	;Store pool1 management byte
		ret
	StorePool2:
		sts  MPOOL02,r26	;store pool2 management byte
		ret
	StorePool3:
		sts  MPOOL03,r26;store pool3 management byte
		ret

;ReadPoolID:
;This routine returns POOLID when given managment code in r26
; 
; Input:	r26
; output:	r16
; Errors:	none
; Uses: r16 is used, all other registers are preserved
ReadPoolID:
	mov	r16,r26
	andi r16,0x0F
	ret

; ReadBlockID:
; This routine returns BLOCKID when given managment code in r26
; 
; Input:	r26
; output:	r17
; Errors:	none
; Uses: r17 is used, all other registers are preserved
ReadBlockID:					
	mov  r17,r26
	swap r17
	andi r17,0x0F
	ret

; ReturnBlockAddress:
; Returns Address of Block, Given POOLID and BLOCKID in r16, r17
; address = Poolstarting address + (BlockID * blocksize)
; 
; Input:	r16,r17 - POOLID,BLOCKID
; output:	r24:r25	- Block Address
; Errors:	none
; Uses: r24:r25 are used, all other registers are preserved
ReturnBlockAddress:
	rcall PoolStartingAddress
	push r16
	tst r16
	breq ReturnBlockAddressisPool0
	ldi r16, 16
	rjmp CalculateAddress
	ReturnBlockAddressisPool0:
		ldi r16, 8
		CalculateAddress:
			mul r16,r17
			add r24,r0
			adc r25,r1
			pop r16
			ret

; PoolStartingAddress:
; Given r16, returns Starting address of pool in r24:r25
; 
; Input:	r16	    - POOLID
; output:	r24:r25	- Pool Starting Address
; Errors:	none
; Uses: r24:r25 are used, all other registers are preserved	
PoolStartingAddress:
	tst r16				
	brne Pool1StartingAddress
	rjmp Pool0Starting			
	Pool1StartingAddress:
		cpi r16, 1
		breq Pool1Starting
	Pool2StartingAddress:
		cpi r16, 2
		breq Pool2Starting	
	Pool3StartingAddress:
		cpi r16, 3
		breq Pool3Starting
	Pool0Starting:
		ldi r24, POOL00L
		ldi r25, POOL00H
		ret
	Pool1Starting:
		ldi r24, POOL01L
		ldi r25, POOL01H
		ret
	Pool2Starting:
		ldi r24, POOL02L
		ldi r25, POOL02H
		ret
	Pool3Starting:
		ldi r24, POOL03L
		ldi r25, POOL03H
		ret

; RotateToBlock:
; Given r17, returns Starting address of pool in r24:r25
; 
; Input:	r17, r26	    - BLOCKID, management byte
; output:	block in zero position of managment byte, 
; Errors:	none
; Uses: r16 and r26 are used, all other registers are preserved	
RotateToBlock:
	cp r16, r17
	breq BlockInPosition
	inc r16
	bst r26,0
	ror r26
	bld r26,7
	rjmp RotateToBlock
	BlockInPosition:
		ret

; BlockBitsClear:
; Given r17, returns updated managment byte
; 
; Input:	r17, r26,r27,r16 - BLOCKID, management byte, number of blocks, count
; output:	updated managment byte
; Errors:	none
; Uses: r16,r26 and r27 are used, all other registers are preserved	
BlockBitsClear:					
	andi r26,0b11111110		;clear end bit
	dec r27
	tst r27					; are there any blocks left to clear
	breq FinishRotating
	inc r16
	bst r26,0
	ror r26
	bld r26,7
	rjmp BlockBitsClear

; BlockBitsSet:
; Given r17, returns updated managment byte
; 
; Input:	r17, r26,r27,r16 - BLOCKID, management byte, number of blocks, count
; output:	updated managment byte
; Errors:	none
; Uses: r16,r26 and r27 are used, all other registers are preserved	
BlockBitsSet:					
	ori r26,0b00000001	;set end bit
	dec r27
	tst r27				; are there any blocks left to clear
	breq FinishRotating
	inc r16
	bst r26,0
	ror r26
	bld r26,7
	rjmp BlockBitsSet

; FinishRotating:
; Given r16, finishes rotating updated managment byte
; 
; Input:	r26,r16 - management byte, count
; output:	updated managment byte
; Errors:	none
; Uses: r16 andr26 are used, all other registers are preserved	
FinishRotating:
	cpi r16,8
	breq RotateExit
	FinishRotatingLoop:
		bst XL,0
		ror XL
		bld XL,7
		inc r16
		cpi r16,8				; have you finished rotating
		brne FinishRotatingLoop	; go to exit
	RotateExit:
		ret

; DetermineBlockSize:
; Given r16, finishes rotating updated managment byte
; 
; Input:	r16 - size of memory being allocated
; output:	r16	- number of 8 byte blocks needed
; Errors:	none
; Uses: r16 is used, all other registers are preserved
DetermineBlockSize:
	push r17
	mov  r17,r16
	andi r17, 0x07
	tst r17
	breq GreaterFree	;	if you their will be a remainder add 8
	ldi r17,0x08
	add r16,r17
	GreaterFree:
	lsr r16				;	"divide" by 8 to get the number of blocks 
	lsr r16
	lsr r16
	pop r17
	ret