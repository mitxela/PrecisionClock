; Data  - Grey   -> PB0
; Clock - Blue   -> PB1
; Load  - Purple -> PB2

.include "tn85def.inc" 
	rjmp init
	nop
	nop
	
	dec r20
	breq incCentiSec
	reti
incCentiSec:
	ldi r20,10

	

mov r0,r13

	rcall clockBurstRead
	rcall waitlong
	
	cp r13,r0
	breq subSecond
	clr r14
	clr r15
	ldi r20,10
	rcall updateAllDisplay
	reti
subSecond:




	inc r15
	ldi r16,10
	cp r15, r16
	breq incDeciSec
	
	ldi r18,8
	mov r19,r15
	rcall sendTime
	reti

incDeciSec:
	clr r15
	ldi r18,8
	mov r19,r15
	rcall sendTime

	
	
	

	inc r14
	ldi r16,10
	cp r14,r16
	breq incSec

	ldi r18,7
	mov r19,r14
	rcall sendTime
	reti

incSec:
	clr r14
	ldi r18,7
	mov r19,r14
	rcall sendTime


	reti



init:
	ldi r16, 0b000111
	out DDRB,r16
	ldi r16,0
	out PORTB,r16

	ldi r16,(1<<ADLAR|1<<MUX1|1<<MUX0) ; Vcc, left adjust, PB3
	out ADMUX,r16

	ldi r16, (1<<CTC1|1<<CS12|1<<CS11) ;clk/32
	out TCCR1,r16

	ldi r16, 249
	out OCR1C, r16	; Should tick at 1khz

	ldi r16,1
	out OCR1A, r16
	ldi r16, (1<<OCIE1A)
	out TIMSK, r16

	ldi r20,10

	; Send init data to both

	ldi r18,$09 ;Decode mode
	ldi r19,$FF ;Code B all digits
	rcall shiftData16
	rcall pulseLoad

	ldi r18,$0A ;Intensity
	ldi r19,$0F
	rcall shiftData16
	rcall pulseLoad

	ldi r18,$0B ;Scan Limit
	ldi r19,$07 ;All digits
	rcall shiftData16
	rcall pulseLoad

	ldi r18,$FF ;Display test
	ldi r19,$00 ;Off
	rcall shiftData16
	rcall pulseLoad

	ldi r18,$0C ;Shutdown
	ldi r19,$01 ;Normal operation
	rcall shiftData16
	rcall pulseLoad

	rcall shiftData16
	rcall pulseLoad


	;First two digits are always 20
	ldi r18,1
	ldi r19,2
	rcall sendDate
	ldi r18,2
	ldi r19,0
	rcall sendDate


/////////////

	
	ldi		r16,	$8E		; remove write protection
	ldi		r17,	$00		;
	rcall	WrtCmd
	
	rcall waitlong


	;Check clock halt bit
	ldi		r16,$81
	rcall	RdCmd
	sbrc r16,7
	rcall setInitialTime



	

main:
	sei

	rcall readADC
	
	cli


	cpi r16,75
	brcc main
	cpi r16,25
	brcs main

	rcall waitForButtonRelease

	ldi r16,$7F
	and r13,r16

.def pointer = r22
	ldi pointer,10

checkButtons:
	; Up		0V -  0 to 25
	; Down		3V - 125 to 175
	; Left		4V - 175 to 225
	; Right		2V - 75 to 125
	; Freeze:	1V : 25 to 75
	rcall waitBlink
	;ldi r18,4

	;ldi r19,$0F
	;rcall sendDigit
	

	;rcall waitBlink
	;ldi r18,4

	rcall loadFromPointer
	rcall sendDigit


	rcall readADC
	cpi r16,225
	brcs checkLeft

	rcall waitBlink
	ldi r19,$0F
	rcall sendDigit
	
	rjmp checkButtons

		
checkLeft:
	cpi r16,175
	brcs checkDown
	; Left
	inc pointer
	cpi pointer,13
	brne leftOver
	ldi pointer,1
leftOver:
	rcall waitForButtonRelease
	rjmp checkButtons

checkDown:
	cpi r16,125
	brcs checkRight
	; Down
	rcall loadFromPointer
	tst r19
	brne downOver
	ldi r19,10
downOver:
	dec r19
	rcall storeToPointer
	rcall waitForButtonRelease
	rjmp checkButtons

checkRight:
	cpi r16,75
	brcs checkFreeze
	; Right

	subi pointer,1
	brne RightOver
	ldi pointer,12
RightOver:
	rcall waitForButtonRelease
	rjmp checkButtons
	
checkFreeze:
	cpi r16,25
	brcs checkUp
	; unfreeze
	rcall waitForButtonRelease

	;store new values
	ldi r16,$80
	or r13,r16
	clr r14
	clr r15
	ldi r20,10

	rcall clockBurstWrite
	

	rjmp main

checkUp:
	;Up
	rcall loadFromPointer
	cpi r19,9
	brne upOver
	ldi r19,-1
upOver:
	inc r19
	rcall storeToPointer
	rcall waitForButtonRelease
	rjmp checkButtons





sendDigit:
	mov r18,pointer
	cpi r18,7
	brcs sendDigitDate
	subi r18,6
	rcall sendTime
	ret
sendDigitDate:
	subi r18,-2
	rcall sendDate
	ret

	




readADC:
	; Enable, Start conversion, clear interrupt, Prescale 128
	ldi r16,(1<<ADEN|1<<ADSC|1<<ADIF|1<<ADPS2|1<<ADPS1|1<<ADPS0)
	out ADCSRA,r16
waitForConversion:
	sbis ADCSRA,ADIF
	rjmp waitForConversion
	in r16,ADCH
	ret


waitForButtonRelease:
	rcall readADC
	cpi r16,225
	brcs waitForButtonRelease
	ret





waitlong:
	ldi XH,1
tt:
	sbiw X,1
	brne tt
	ret


waitBlink:
	ldi XH,60
tb:
	rcall wait
	sbiw X,1
	brne tb
	ret






loadFromPointer:
	ldi ZH,HIGH(loadPointerTable)
	ldi ZL, LOW(loadPointerTable)
	mov r19,pointer
	lsl r19
	add ZL,r19
	clr r19
	adc ZH,r19
	ijmp
loadPointerTable:
	nop
	nop
	mov r19,r2
	ret
	mov r19,r3
	ret
	mov r19,r4
	ret
	mov r19,r5
	ret
	mov r19,r6
	ret
	mov r19,r7
	ret
	mov r19,r8
	ret
	mov r19,r9
	ret
	mov r19,r10
	ret
	mov r19,r11
	ret
	mov r19,r12
	ret
	mov r19,r13
	ret
	mov r19,r14
	ret
	mov r19,r15
	ret



storeToPointer:
	ldi ZH,HIGH(storePointerTable)
	ldi ZL, LOW(storePointerTable)
	mov r16,pointer
	lsl r16
	add ZL,r16
	clr r16
	adc ZH,r16
	ijmp
storePointerTable:
	nop
	nop
	mov r2,r19
	ret
	mov r3,r19
	ret
	mov r4,r19
	ret
	mov r5,r19
	ret
	mov r6,r19
	ret
	mov r7,r19
	ret
	mov r8,r19
	ret
	mov r9,r19
	ret
	mov r10,r19
	ret
	mov r11,r19
	ret
	mov r12,r19
	ret
	mov r13,r19
	ret
	mov r14,r19
	ret
	mov r15,r19
	ret



sendTime:
	ldi r16, 0 ;load low
	out PORTB,r16

	push r18
	push r19
	clr r18
	clr r19
	rcall shiftData16
	pop r19
	pop r18
	rcall shiftData16
	rcall pulseLoad
	ret


sendDate:
	ldi r16, 0 ;load low
	out PORTB,r16

	rcall shiftData16
	clr r18
	clr r19
	rcall shiftData16
	rcall pulseLoad
	ret




	; max7219 is MSB first
shiftData16:
	; data is r18:r19
	ldi r17,16
shiftBitLoop:
	cbi PORTB,1 ; clock low
	clr r16
	sbrc r18,7
	ldi r16,1 ; data high 
	out PORTB,r16
	lsl r19
	rol r18
	rcall wait
	sbi PORTB,1 ; clock high
	rcall wait

	dec r17
	brne shiftBitLoop
	ret


pulseLoad:
	sbi PORTB,2 ;Load high
	rcall wait
	cbi PORTB,1 ; clock low
	rcall wait
	ret


wait:
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	ret












updateAllDisplay:
	ldi r18,3
	mov r19,r2
	rcall sendDate
	ldi r18,4
	mov r19,r3
	rcall sendDate
	ldi r18,5
	mov r19,r4
	rcall sendDate
	ldi r18,6
	mov r19,r5
	rcall sendDate
	ldi r18,7
	mov r19,r6
	rcall sendDate
	ldi r18,8
	mov r19,r7
	rcall sendDate

	ldi r18,1
	mov r19,r8
	rcall sendTime
	ldi r18,2
	mov r19,r9
	rcall sendTime
	ldi r18,3
	mov r19,r10
	rcall sendTime
	ldi r18,4
	mov r19,r11
	rcall sendTime
	ldi r18,5
	mov r19,r12
	rcall sendTime
	ldi r18,6
	mov r19,r13
	rcall sendTime
	ldi r18,7
	mov r19,r14
	rcall sendTime
	ldi r18,8
	mov r19,r15
	rcall sendTime
	ret



setInitialTime:
	ldi ZH, HIGH(initDateTime*2)
	ldi ZL,  LOW(initDateTime*2)
	lpm r0, Z+
	lpm r1, Z+
	lpm r2, Z+
	lpm r3, Z+
	lpm r4, Z+
	lpm r5, Z+
	lpm r6, Z+
	lpm r7, Z+
	lpm r8, Z+
	lpm r9, Z+
	lpm r10,Z+
	lpm r11,Z+
	lpm r12,Z+
	lpm r13,Z+
	lpm r14,Z+
	lpm r15,Z+


	rcall waitlong

;	ldi  	r16, 	$80		; write seconds
;	ldi  	r17, 	0x55	; 00; 0xxx xxxx
;	rcall	WrtCmd

;	rcall waitlong


	rcall clockBurstWrite
ret



	; DS1302 macro
	.MACRO 	IO_in
			cbi		DDRB,	0	; Make I/O line input.
	.ENDMACRO 
	
	.MACRO 	IO_out
			sbi		DDRB,	0	; Make I/O line output.
	.ENDMACRO 
	
	.MACRO 	SCLK_high
			nop
			nop
			nop
			nop
			nop
			sbi		PORTB,	1	; Set SCLK output.
			nop
			nop
			nop
			nop
			nop
	.ENDMACRO 
	
	.MACRO 	SCLK_low
			nop
			nop
			nop
			nop
			nop
			cbi		PORTB, 1
			nop
			nop
			nop
			nop
			nop
	.ENDMACRO 
	
	.MACRO  IO_high
			sbi		PORTB,	0
	.ENDMACRO 
	
	.MACRO  IO_low
			cbi		PORTB,	0
	.ENDMACRO 
	
	.MACRO  RST_high
			nop
			nop
			nop
			nop
			nop
			sbi		PORTB,	4
			nop
			nop
			nop
			nop
			nop
	.ENDMACRO 
	
	.MACRO  RST_low
			nop
			nop
			nop
			nop
			nop
			cbi		PORTB,	4
			nop
			nop
			nop
			nop
			nop
	.ENDMACRO 
	
	;--------------------------------------------------------
	; Write a cmd / data combo from r16 / r17 to the RTC.
	WrtCmd:
		   RST_high
			mov     r18,	r16	; write command to RTC
			rcall   WrtByte
			mov     r18,	r17	; write data to RTC
			rcall   WrtByte
			RST_low
	ret
	
	; Clock out the byte in r18 to the RTC.
	; Data is clocked out starting with bit 0. 
	; Data is clocked into the RTC on the rising edge.
	
	WrtByte:
		ldi 	r16, 	8 	; 8 bits to clock out.
	WrtByte1:
		lsr 	r18		; rotate.............bit 0 into C
		brcc 	WrtByte2	; skip if bit = 0
		IO_high
		rjmp 	WrtByte3
	WrtByte2:
		IO_low
	WrtByte3:
		SCLK_high
		dec 	r16
		breq WrtByte4 			
		SCLK_low 			; Clock it out.
		rjmp WrtByte1 		; Do all the bits.
	WrtByte4:
		IO_high 			; turn on pull up.
		SCLK_low 			; Clock data in (if applicable). @@ Claes , last bit is still here after last clock
		clc	
	ret
	
	; Read a byte following write of command byte.
	RdCmd:
			RST_high
			IO_out
			mov     r18,	r16
			rcall   WrtByte
			IO_in
			rcall   RdByte
			mov     r16,	r18
			RST_low
			IO_out
	ret
	
	; Clock in a byte from the RTC to r18
	RdByte:
			ldi     r18,	0		; Empty register for data
			ldi     r16,	8		; 8 bits to clock in.
	RdByte1:
			clc						; Clear carry.
			sbic    PINB,	0		; Jump if I/O = 0.
			sec						; Set Carry Flag
			ror     r18			; Rotate data into bit.
			SCLK_high				; clock in next bit.
			SCLK_low
			dec     r16			; more bits ?
			brne    RdByte1
	ret




clockBurstRead:
		RST_high
		IO_out
		ldi     r18,	$BF
		rcall   WrtByte
		IO_in
		rcall   RdByte
		mov  r16, r18
		andi r16, $0F
		ori r16,$80
		mov  r13, r16
		swap r18
		andi r18,$0F
		mov r12,r18

		rcall   RdByte
		mov  r16, r18
		andi r16, $0F
		mov  r11, r16
		swap r18
		andi r18,$0F
		mov r10,r18	

		rcall   RdByte
		mov  r16, r18
		andi r16, $0F
		mov  r9, r16
		swap r18
		andi r18,$0F
		mov r8,r18	

		rcall   RdByte
		mov  r16, r18
		andi r16, $0F
		mov  r7, r16
		swap r18
		andi r18,$0F
		mov r6,r18	

		rcall   RdByte
		mov  r16, r18
		andi r16, $0F
		mov  r5, r16
		swap r18
		andi r18,$0F
		mov r4,r18
	
		rcall   RdByte ; day of the week
		nop
		nop
		
		rcall   RdByte
		mov  r16, r18
		andi r16, $0F
		mov  r3, r16
		swap r18
		andi r18,$0F
		mov r2,r18
		
		RST_low
		IO_out
	ret



clockBurstWrite:
	ldi		r16,	$8C		; write year separately
	mov r17,r2
	swap r17
	or r17,r3
	rcall	WrtCmd
	
	rcall waitlong

	ldi r16,$80
	mov r17,r12
	swap r17
	or r17,r13
	andi r17,$7F ;mask clock hold
	rcall WrtCmd

	ldi r16,$82
	mov r17,r10
	swap r17
	or r17,r11
	rcall WrtCmd

	ldi r16,$84
	mov r17,r8
	swap r17
	or r17,r9
	rcall WrtCmd

	ldi r16,$86
	mov r17,r6
	swap r17
	or r17,r7
	rcall WrtCmd

	ldi r16,$88
	mov r17,r4
	swap r17
	or r17,r5
	rcall WrtCmd

	ret


initDateTime:
.db 2,0,1,5,0,6,1,8, 1,5,4,5, 0,$80,0,0
