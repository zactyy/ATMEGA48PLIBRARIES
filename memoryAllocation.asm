; * Project: memoryAllocation		                               *
; * Author: Zachary Tyynismaa(please attribute properly            *
; * File: memoryAllocation.asm							           *
; * Description: This assembly language code template is designed  *
; *              provide typical code requided for all programs.   *
; *                                                                *
; * Inputs: Takes commands through X and Y registers  			   *
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
.equ POOL03 = 0x0240
.equ POOL02 = 0x01C0
.equ POOL01 = 0x0140
.equ POOL00 = 0x0100
.equ NULL =0x00
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
			bst r27,0		;	if so check if its odd  and
			brtc NotOdd
			inc r27			;	if it is increment
		NotOdd:
			lsr r27			;	in either case shift right once
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
		sub r17,r27		;	get value of staring block in sequence
		swap r17		;	move to upper nibble
		or r16, r17		;	combine with poolid
		mov r26,r16		;	move addressing code into r26
		rcall AddressReturnAndSetBlock
		pop r16			;	pop data and return
		pop r17 
		pop r18
		ret
	returnNull:
		ldi XH, NULL	; set pointer value to null
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
	tst YL				;	do we need to free anything
	breq returnNull		;	if not return null
	push r16			;	so now we need to allocate
	push r17			;	push temp registers
	push r10
	push r24			;	Push these as the will be used to 
	push r25			;	find the block
	clr r10				;	need this empty to story address code
	mov r16,r28
	rcall DetermineBlockSize;	Determine block size
	mov r10,r16			;	copy block size into r10
	push r10			;	store 8 byte block size
	andi r16, 0x01		;	
	tst r16				;	determine needed 16 blocks size
	breq MAGet16ByteBlockCount
	inc r10				;	round blocks size
	MAGet16ByteBlockCount:
	lsr r10
	GetPoolFree:		;	Find which Pool the address is in
		clr YL
		ldi r16,0x03
		rcall PoolStartingAddress;	get pool3 address
		cp XL,r24				 ;	Check if its in Pool3
		cpc XH,r24
		brsh FindBlockPool128	 ;	get block id
		dec r16
		rcall PoolStartingAddress;	get pool2 address
		cp XL,r24				 ;	Check if its in Pool2
		cpc XH,r25
		brsh FindBlockPool128	 ;	get block id
		dec r16
		rcall PoolStartingAddress;	get pool1 address
		cp XL,r24				 ;	Check if its in Pool1
		cpc XH,r25
		brsh FindBlockPool128	 ;	get block id
		dec r16
		rcall PoolStartingAddress;	get pool0 address
		cp XL,r24				 ;	Check if its in Pool0
		cpc XH,r25
		brlo returnNull			 ;	invalid address return null
		pop r10					 ;	working with 8 byte blocks
		push r10				 ;	so restore original block #
		rjmp FindBlockPool0		 ;	get block id
		
	FindBlockPool0:		;	start at the beggining of pool0 memory 
		cp r24,XL		;	iterate through blocks until match
		cpc r25,XH		;	and compare to the pointer
		breq ExitFree	;	go clear corresponding pool and block
		ADIW r25:r24, 8	;	add block width
		inc YL			;	inc block id
		rjmp FindBlockPool0
	FindBlockPool128:	;	start at the beggining of pool1/2/3 memory 
		cp r24,XL		;	iterate through blocks until match
		cpc r25,XH		;	and compare to the pointer
		breq ExitFree	;	go clear corresponding pool and block
		ADIW r25:r24, 16;	add block width
		inc YL			;	inc block id
		rjmp FindBlockPool128
	ExitFree:
		swap YL		;	move blockid to high nibble
		or r16,YL	;	combine block and pool ids
		mov XL,r16	;	copy into XL
		mov XH,r10	;	copy blocks required into r10
		rcall AddressFreeClearBlocks	;	update managment byte
		pop r25		;	discard pushed r10 value
		pop r25		;	restore registers original values
		pop r24
		pop r10
		pop r17
		pop r16
		rjmp returnNull

; AddressFreeClearBlocks
; This routine when given an r26 and r27 clears management bits from
; memory according to r26
;
; Input:	r26, r27	- internal managment code,  number blocks
; output:	memory management updated to reflect clear
; Errors:	none
; Uses:	XH:XL(r27:r26),r16 and r17 are used, all other registers are preserved
AddressFreeClearBlocks:
	rcall ReadPoolID	;	Reads PoolID into r16 
	rcall ReadBlockID	;	Reads BLOCKID into r17
	rcall LoadPoolByte	;	Loads r16's pool management byte into r26
	push r16			;	Stores POOLID
	rcall RotateToBlock	;	Rotates r26 so r17's block is at zero, clears r16
	bst r26,0			;	checks status of starting block bit
	brts AddressFreeClearBlocksValid;	this is a valid case so proceed
	rjmp  returnNull	;	if already cleared return Null
	AddressFreeClearBlocksValid:
		rcall BlockBitsUpdate;	Clears r26 per r27, rotates byte back to start
		pop r16				;	Restores POOLID
		rcall StorePoolByte	;	Stores r26 back to Pool management memory
		ret

; AddressReturnAndSetBlock
; This routine when given an r26 and r27 sets management bits from
; memory according to r26
;
; Input:	r26, r27	- internal managment code,  number blocks
; output:	memory management updated to reflect clear
; Errors:	none
; Uses:	XH:XL(r27:r26),r16 and r17 are used, all other registers are preserved
AddressReturnAndSetBlock:
	push r24				;	Push Temp values onto stack to restore after
	push r25				;	opertation
	rcall ReadPoolID		;   POOLID loaded into r16
	rcall ReadBlockID		;	BLOCKID loaded into r17
	rcall ReturnBlockAddress;   Takes r16 and r17 return address with r25:r24
	rcall LoadPoolByte		;	Loads r16's pool management byte into r26
	push r16
	rcall RotateToBlock		;	Rotates r26 so r17's block is at zero
	rcall BlockBitsUpdate	;	Clears r26 per r27, rotates byte back to start
	pop r16
	rcall StorePoolByte		;	Stores r26 back to Pool management memory
	mov XL, r24
	mov XH, r25
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
	tst r16			;	is this pool0	
	breq LoadPool0	;	if so store in pool0 management				
	cpi r16, 1		;	is this pool1	
	breq LoadPool1	;	if so store in pool1 management
	cpi r16, 2		;	is this pool2	
	breq LoadPool2	;	if so store in pool2 management	
	cpi r16, 3		;	is this pool3	
	breq LoadPool3	;	if so store in pool3 management
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
	tst r16				;	is this pool0	
	breq StorePool0		;	if so store in pool0 management		
	cpi r16, 1			;	is this pool1	
	breq StorePool1		;	if so store in pool1 management	
	cpi r16, 2			;	is this pool2	
	breq StorePool2		;	if so store in pool2 management	
	cpi r16, 3			;	is this pool3	
	breq StorePool3		;	if so store in pool3 management	
	StorePool0:
		sts  MPOOL00,r26	;	Store pool0 management byte
		ret
	StorePool1:
		sts  MPOOL01,r26	;	Store pool1 management byte
		ret
	StorePool2:
		sts  MPOOL02,r26	;	store pool2 management byte
		ret
	StorePool3:
		sts  MPOOL03,r26	;	store pool3 management byte
		ret

;ReadPoolID:
;This routine returns POOLID when given managment code in r26
; 
; Input:	r26
; output:	r16
; Errors:	none
; Uses: r16 is used, all other registers are preserved
ReadPoolID:
	mov	r16,r26		;	copy addressing code
	andi r16,0x0F	;	mask pool id
	ret

; ReadBlockID:
; This routine returns BLOCKID when given managment code in r26
; 
; Input:	r26
; output:	r17
; Errors:	none
; Uses: r17 is used, all other registers are preserved
ReadBlockID:					
	mov  r17,r26	;	copy addressing code
	swap r17		;	bring block id to lower nibble
	andi r17,0x0F	;	mask block id value
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
	rcall PoolStartingAddress	;	get pool starting address
	push r16					;	preserve r16
	tst r16						;	is this pool zero
	breq ReturnBlockAddressisPool0;	if so go to zero case
	ldi r16, 16					;	if not load 16
	rjmp CalculateAddress		;	calculate address
	ReturnBlockAddressisPool0:
		ldi r16, 8				;	if pool zero load 8
		CalculateAddress:
			mul r16,r17			;	get offset from start
			add r24,r0			;	add offset to start
			adc r25,r1
			pop r16				;	restore r16 and return
			ret

; PoolStartingAddress:
; Given r16, returns Starting address of pool in r24:r25
; 
; Input:	r16	    - POOLID
; output:	r24:r25	- Pool Starting Address
; Errors:	none
; Uses: r24:r25 are used, all other registers are preserved	
PoolStartingAddress:
	tst r16					;	is POOLID 0
	brne Pool1StartingAddress;	if not see if it POOLID 1
	rjmp Pool0Starting		;	if 0 get starting address
	Pool1StartingAddress:
		cpi r16, 1			;	is POOLID 1
		breq Pool1Starting	;	if 1 get starting address
	Pool2StartingAddress:
		cpi r16, 2			;	is POOLID 2
		breq Pool2Starting	;	if 2 get starting address	
	Pool3StartingAddress:
		cpi r16, 3			;	is POOLID 3
		breq Pool3Starting	;	if 3 get starting address
	Pool0Starting:
		ldi r24, LOW(POOL00);	load and return pool 0
		ldi r25, HIGH(POOL00);	starting address
		ret
	Pool1Starting:
		ldi r24, LOW(POOL01);	load and return pool 1
		ldi r25, HIGH(POOL01);	starting address
		ret
	Pool2Starting:
		ldi r24, LOW(POOL02);	load and return pool 2
		ldi r25, HIGH(POOL02);	starting address
		ret
	Pool3Starting:
		ldi r24, LOW(POOL03);	load and return pool 3
		ldi r25, HIGH(POOL03);	starting address
		ret

; RotateToBlock:
; Given r17, returns Starting address of pool in r24:r25
;
; Input:	r17, r26	    - BLOCKID, management byte
; output:	block in zero position of managment byte, 
; Errors:	none
; Uses: r16, r17 r26 are used, all other registers are preserved	
RotateToBlock:
	clr r16				;	initialize count
	RotateToBlockLoop:
		cp r16, r17			;	does count match BLOCKID
		breq BlockInPosition;	if so go to return routine
		inc r16				;	increment count
		bst r26,0			
		ror r26				;	circular shift management byte
		bld r26,7
		rjmp RotateToBlockLoop	;	repeat
	BlockInPosition:
		ldi r17, 0b00000001	;set mask for following operation
		ret

; BlockBitsUpdate:
; Given managment byte updates by toggeling bits
; 
; Input:	r17, r26,r27,r16 - BLOCKID, management byte, number of blocks, count
; output:	updated managment byte
; Errors:	none
; Uses: r16,r26 and r27 are used, all other registers are preserved	
BlockBitsUpdate:					
	eor r26,r17		;	toggle bit value
	dec r27			;	decrement count of blocks to update
	tst r27			;	are there any blocks left to clear
	breq FinishRotating;	if bits updated finish rotating
	inc r16			;	increment our rotation count
	bst r26,0		;	
	ror r26			;	circular shift of management byte
	bld r26,7		;
	rjmp BlockBitsUpdate;	repeat

; FinishRotating:
; Given r16, finishes rotating updated managment byte
; 
; Input:	r26,r16 - management byte, count
; output:	updated managment byte
; Errors:	none
; Uses: r16 andr26 are used, all other registers are preserved	
FinishRotating:
	cpi r16,8		;	have we rotated through all 8 bits?
	breq RotateExit	;	if finsihed exit
	FinishRotatingLoop:
		bst XL,0	;
		ror XL		;	circular shift manament byte
		bld XL,7	;
		inc r16		;	increase rotation count
		cpi r16,8	;	have you finished rotating
		brne FinishRotatingLoop;	go to exit
	RotateExit:
		ret

; DetermineBlockSize:
; Given r16, finishes rotating updated managment byte
; 
; Input:	r16 - size of memory being allocated
; output:	r16	- number of 8 byte blocks needed
; Errors:	none
; Uses: r16 and r17 is used, all other registers are preserved
DetermineBlockSize:
	mov  r17,r16	;	copy size value 
	andi r17, 0x07	;	mask the 3 lsbs
	tst r17			;	see if the is a value in 3 lsbs
	breq GreaterFree;	if not go divide by 8
	ldi r17,0x08	;	if so add 8 to size so that result
	add r16,r17		;	is rounded
	GreaterFree:
	lsr r16			;	"divide" by 8 to get the number of blocks 
	lsr r16
	lsr r16
	ret