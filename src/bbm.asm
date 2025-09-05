;BASIC Boot Maker
;a C64 tool to make auto-executable programs in BASIC text area
;Not only BASIC: source file could also be a BASIC stub to launch M/L code!

;Version 1.04
;Released on September 2, 2025

;Copyright (c) 2025 Graham (Francesco Gramignani)
;https://graham-it.itch.io
;https://github.com/graham-it
;https://csdb.dk/scener/?id=40810

;Compilable with ACME 0.97
;the ACME Crossassembler for Multiple Environments
;Copyright (c) 1998-2020 Marco Baye

program_name	= "basic boot maker"
version		= "v1.04"
program_date	= "(c) 2025"
author		= "graham"
basic_line	= 2025
target		= 64		;Commodore 64

!ct pet				;PETSCII text conversion table
!src "cbm_map.def"		;Commodore machines memory map

; --------------------
; - Consts
; --------------------

source_file = 1
target_file = 2
name_max = 16			;file name length
buffer_pages = $10		;buffer size (4K)
eof_status = $40		;end of file

buffer_max = >data_buffer+buffer_pages		;buffer limit

; --------------------
; - Variables
; --------------------

data_len = $1c			;file name/buffer length
last_status = $1d		;STATUS after last disk operation
size_lo = $1e			;file size in blocks (lo-byte)
data_pages = $1e		;buffer last page (reused)
tmpptr = $fb			;temp pointer

; --------------------
; - BASIC launcher
; --------------------

;Start of program
* = BASTXT			

prg_start
	!by <next_line,>next_line	;link address
	!by <basic_line,>basic_line	;line number
	!by SYS_tok			;SYS command

;M/L code entry point (decimal)
	!by '0' + ml_start DIV 1000
	!by '0' + ml_start MOD 1000 DIV 100
	!by '0' + ml_start MOD 100  DIV 10
	!by '0' + ml_start MOD 10

	!tx SUB_tok,author		;comment
	!by 0				;end of line

next_line
	!by 0,0				;end of BASIC launcher

; --------------------
; - Main
; --------------------

;M/L code entry point
ml_start

;Display program info
	lda #<title_txt
	ldy #>title_txt
	jsr PRNSTR

;System setup
	lda #0
	sta MSGFLG		;disable Kernal messages
	sta NDX			;clear keyboard buffer index
	sta last_status		;clear last status

;Load directory
	jsr SelectFile		;let user choose a file
	jsr NXLN		;next line

	ldx last_status		;check status
	beq +

;No file selected
	lda #<noselect_txt
	ldy #>noselect_txt
	jsr PRNSTR

	jmp AskToRestart

;Start processing
+	jsr CreateBoot

;Check result
	lda #<done_txt
	ldy #>done_txt

	ldx last_status
	cpx #eof_status		;check end of file
	beq +

	lda #<error_txt
	ldy #>error_txt

+	jsr PRNSTR

AskToRestart
	lda #<restart_txt
	ldy #>restart_txt
	jsr PRNSTR

-	jsr GETIN		;check user input
	cmp #RETURN_char	;restart
	beq ml_start

	cmp #DEL_char		;quit
	bne -
	rts

; --------------------
; - Subroutines
; --------------------

SelectFile
	lda #<select_txt
	ldy #>select_txt
	jsr PRNSTR

;Set directory command
	lda #dircmd_len		;name length
	ldx #<dircmd		;name address
	ldy #>dircmd		;dir command '$'
	jsr SETNAM

;Set logical file parameters
	lda #source_file	;logical file number
	ldx FADDR		;device address
	ldy #0			;sec. address (read file)
	jsr SETLFS

	jsr OPEN		;open directory
	bcs DirExit

;Display disk header
	ldx #source_file	;set logical file as input
	jsr CHKIN

	ldy #3			;pairs to read
	jsr GetPair
	bne DirExit

	jsr GetEntry
	bne DirExit

EntryLoop
	ldx #source_file	;set logical file as input
	jsr CHKIN

	ldy #2			;pairs to read
	jsr GetPair		;display size
	bne DirExit

	jsr GetEntry		;display entry
	bne DirExit

	jsr GetName		;extract file name
	lda data_len
	beq EntryLoop		;not found

;Get user selection
	jsr CLRCH		;restore default I/O channels

-	jsr GETIN		;check user input
	cmp #RETURN_char	;select file
	beq DirExit

	cmp #CRSR_down		;next file
	beq EntryLoop

	jsr STOP		;check STOP key
	bne -

	ldy #eof_status		;force end of file
	sty last_status		;set status

DirExit
	jsr CLRCH		;restore default I/O channels
	lda #source_file
	jmp CLOSE		;close directory (then return)

GetPair
;Get pairs at the beginning of current line (y = pairs to read)
-	jsr CHRIN		;lo-byte
	tax
	jsr CHRIN		;hi-byte
	dey			;next pair
	bne -

	ldy STATUS
	bne +

	stx size_lo		;store for later (lo-byte)
	jsr PRNINT		;print size (x:a)

	lda #' '
	jsr CHROUT		;add a space

	ldy STATUS
+	sty last_status		;set status
	rts

GetEntry
;Get the rest of entry line
	ldx #0
-	jsr CHRIN
	sta data_buffer,x	;copy into buffer
	beq +
	inx
	jsr CHROUT		;print char

	ldy STATUS
	beq -

+	lda #RETURN_char	;new line
	jsr CHROUT

	ldy STATUS
	sty last_status		;set status
	rts

GetName
;Get file name from buffer
	ldy #0
	ldx #0
-	lda data_buffer,x
	beq +			;end of line
	inx
	cmp #quote_char		;opening quote
	bne -

-	lda data_buffer,x	;dir entry
	sta data_buffer,y	;file name
	beq +			;end of line
	cmp #quote_char		;closing quote
	beq +			;found
	inx
	iny
	cpy #name_max
	bne -

+	sty data_len		;name length
	rts

CreateBoot
;Set source file name
	lda data_len		;name length
	ldx #<data_buffer	;name address
	ldy #>data_buffer
	jsr SETNAM

;Set logical file parameters
	lda #source_file	;logical file number
	ldx FADDR		;device address
	ldy #0			;sec. address (read file)
	jsr SETLFS

	jsr OPEN		;open file
	bcs +++			;CloseFiles (too far branch)

	ldx #source_file	;set logical file as input
	jsr CHKIN

;Check load address (then drop it)
	jsr CHRIN		;check lo-byte
	cmp #<BASTXT
	bne +

	jsr CHRIN		;check hi-byte
	cmp #>BASTXT
	beq ++

;Not in BASIC text area
+	lda #<notbasic_txt
	ldy #>notbasic_txt
	jsr PRNSTR

	jmp CloseFiles

;Prepare boot header
++	jsr EditBootMsg		;edit boot message
	jsr SetBasicPtrs	;set BASIC pointers

;Start processing
	lda #<process_txt
	ldy #>process_txt
	jsr PRNSTR

;Set target file name
	ldx data_len		;name length (including suffix)
	cpx #name_max
	beq +			;truncate last char
	inx			;keep full name

+	inx			;add "@:" command
	inx			;(to overwrite a previously created bootable file)
	txa

	ldx #<target_name	;name address
	ldy #>target_name
	jsr SETNAM

;Set logical file parameters
	lda #target_file	;logical file number
	ldx FADDR		;device address
	ldy #1			;sec. address (write file)
	jsr SETLFS

	jsr OPEN		;open file
+++	bcs CloseFiles		;error

	ldx #target_file	;set logical file as output
	jsr CHKOUT

;Write boot header
	ldy #0
-	lda boot_header,y
	jsr CHROUT

	iny
	cpy #boot_len
	bne -
	
;Copy the rest of file
	lda #<data_buffer	;set buffer pointer (lo-byte)
	sta tmpptr

CopyLoop
;Read from source file
	ldx #source_file	;set logical file as input
	jsr CHKIN

	ldx #>data_buffer	;reset page
--	stx tmpptr+1

	ldy #0			;reset index
-	jsr CHRIN
	sta (tmpptr),y

	lda STATUS
	bne +

	iny
	bne -

	jsr STOP		;check STOP key
	beq UserBreak

	inx			;next page
	cpx #buffer_max		;check buffer limit
	bne --
	beq ++			;(forced)

+	cmp #eof_status		;check end of file
	bne ++
	iny			;add last byte

++	sta last_status		;set status
	stx data_pages		;last used page
	sty data_len		;last page length

;Write to target file
	ldx #target_file	;set logical file as output
	jsr CHKOUT

	ldx #>data_buffer	;reset page
--	stx tmpptr+1

	ldy #0			;reset index
-	lda (tmpptr),y
	jsr CHROUT

	lda STATUS
	bne CloseFiles

	cpx data_pages		;check page
	beq +

	iny
	bne -

	jsr STOP		;check STOP key
	beq UserBreak
	
	inx			;next page
	bne --			;(forced)

+	iny
	cpy data_len		;bytes to write
	bne -

	lda last_status
	beq CopyLoop		;continue copying
	bne CloseFiles		;(forced)

UserBreak
	lda #<break_txt
	ldy #>break_txt
	jsr PRNSTR

CloseFiles
	jsr CLRCH		;restore default I/O channels
	lda #source_file	;close source file
	jsr CLOSE

	lda #target_file	;close target file
	jmp CLOSE		;(then return)

EditBootMsg
;Let user edit the boot message shown at the bottom of the screen
;when loading the generated auto-executable program.

	lda #<edit_txt
	ldy #>edit_txt
	jsr PRNSTR

;Display message
	ldy #0
-	lda boot_msg,y		;boot message
	sta (PNT),y		;current screen line
	iny
	cpy #message_len
	bne -

;Edit message
	lda FADDR		;preserve device
	pha

	lda TBLX		;preserve cursor row
	pha

	lda PNT			;current screen line pointer
	sta tmpptr
	lda PNT+1
	sta tmpptr+1

	lda #5			;logical file number
	ldx #0			;device address (keyboard)
	ldy #0			;sec. address
	jsr SETLFS

	jsr OPEN		;open keyboard
	ldx #5
	jsr CHKIN		;set as input

-	jsr CHRIN		;let user edit message
	cmp #RETURN_char	;wait for return key
	bne -

	lda #5			;close keyboard
	jsr CLOSE

	pla			;restore cursor row
	tax
	inx			;next row
	stx TBLX		;set cursor row
	jsr CLRLN		;clear line

	pla			;restore device
	sta FADDR

;Store message
	ldy #0
-	lda (tmpptr),y		;get screen codes
	sta boot_msg,y		;into boot header
	iny
	cpy #message_len
	bne -
	rts

SetBasicPtrs
;Set BASIC pointers (VARTAB, ARYTAB and STREND) to the end of the source program
;LSB not changed (default = $03), MSB calculated as (rounded up):
;number of file blocks on disk (lo-byte) + BASTXT (hi-byte) (default = $08)

	lda size_lo		;max 202 blocks (normally)
	clc
	adc #>BASTXT		;Start of BASIC Program Text (hi-byte)
		
	ldx #pointers_len	;starting from the last
-	sta boot_ptrs-1,x	;set pointer (hi-byte)
	dex
	dex
	bne -
	rts

; --------------------
; - Boot header
; --------------------

;CHRGET Boot uses an undocumented (and probably never released) exploit into
;LOAD command routine of KERNAL ROM, to auto-execute a program file through
;CHRGET, EAL pointer and screen RAM manipulation.

;header length = 216 bytes	load address [2], zero page [133], screen RAM [81]
;overhead = 214 bytes		old load address is replaced with the new one

boot_header
;(starting from TXTTAB is not strictly required, added for robustness!)
	!by $2b,$00		;load address (TXTTAB)
	!by $01,$08		;$2b	TXTTAB	Pointer to the Start of BASIC Program Text

boot_ptrs
;BASIC pointers (to be set according to file length)
	!by $03,$08		;$2d	VARTAB	Pointer to the Start of the BASIC Variable Storage Area
	!by $03,$08		;$2f	ARYTAB	Pointer to the Start of the BASIC Array Storage Area
	!by $03,$08		;$31	STREND	Pointer to End of the BASIC Array Storage Area (+1)
pointers_len = * - boot_ptrs

;(cold start zero page values)
	!by $00,$a0		;$33	FRETOP	Pointer to the Bottom of the String Text Storage Area
	!by $00,$00		;$35	FRESPC	Temporary Pointer for Strings
	!by $00,$a0		;$37	MEMSIZ	Pointer to the Highest Address Used by BASIC
	!by $00,$00		;$39	CURLIN	Current BASIC Line Number
	!by $00,$00		;$3b	OLDLIN	Previous BASIC Line Number
	!by $00,$00		;$3d	OLDTXT	Pointer to the Address of the Current BASIC Statement
	!by $00,$00		;$3f	DATLIN	Current DATA Line Number
	!by $00,$08		;$41	DATPTR	Pointer to the Address of the Current DATA Item
	!by $00,$00		;$43	INPPTR	Pointer to the Source of GET, READ, or INPUT Information
	!by $00,$00		;$45	VARNAM	Current BASIC Variable Name
	!by $24,$00		;$47	VARPNT	Pointer to the Current BASIC Variable Value
	!by $00,$00		;$49	FORPNT	Temporary Pointer to the Index Variable Used by FOR
	!by $00,$00		;$4b	OPPTR	Math Operator Table Displacement
	!by $00			;$4d	OPMASK	Mask for Comparison Operation
	!by $00,$00		;$4e	DEFPNT	Pointer to the Current FN Descriptor
	!by $00,$00,$00		;$50	DSCPNT	Temporary Pointer to the Current String Descriptor
	!by $03			;$53	FOUR6	Constant for Garbage Collection
	!by $4c,$00,$00		;$54	JMPER	Jump to Function Instruction (3 bytes)
	!by $00,$00,$00,$00,$00	;$57	TEMPF1	BASIC Numeric Work Area (10 bytes)
	!by $00,$fc,$00,$00,$00	;...	 "	 "
	!by $0a,$76,$a3		;$61	FAC1	Floating Point Accumulator #1 (6 bytes)
	!by $19,$00,$20		;...	 "	 "
	!by $00			;$67	SGNFLG	Number of Terms in a Series Evaluation
	!by $00			;$68	BITS	Floating Point Accumulator #1: Overflow Digit
	!by $80,$00,$00		;$69	FAC2	Floating Point Accumulator #2 (6 bytes)
	!by $00,$04,$00		;...	 "	 "
	!by $76			;$6f	ARISGN	Result of a Sign Comparison of Accumulator #1 to Accumulator #2
	!by $00			;$70	FACOV	Low Order Mantissa Byte of Floating Point Accumulator #1
	!by $80,$a3		;$71	FBUFPT	Series Evaluation Pointer

;CHRGET Subroutine: Get Next BASIC Text Character (24 bytes, unaltered except TXTPTR)
	!by $e6,$7a		;$73	inc $7a
	!by $d0,$02		;$75	bne $79
	!by $e6,$7b		;$77	inc $7b
	!by $ad,$00,$08		;$79	lda TXTPTR 	Pointer to Current Text Character (MSB at $7b)
				;...			($02 = direct mode, any other value = RUN mode)
	!by $c9,$3a		;$7c	cmp #$3a
	!by $b0,$0a		;$7e	bcs $8a
	!by $c9,$20		;$80	cmp #$20
	!by $f0,$ef		;$82	beq $73
	!by $38			;$84	sec
	!by $e9,$30		;$85	sbc #$30
	!by $38			;$87	sec
	!by $e9,$d0		;$88	sbc #$d0
	!by $60			;$8a	rts

;(other cold start zero page values)
	!by $80,$4f,$c7,$52,$58	;$8b	RNDX	RND Function Seed Value (5 bytes)
	!by $00		 	;$90	STATUS	Kernal I/O Status Word (ST)
	!by $ff			;$91	STKEY	Flag: Was STOP Key Pressed? ($ff = no key pressed)
	!by $00		 	;$92	SVXT	Timing Constant for Tape Reads
	!by $00		 	;$93	VERCK	Flag for Load Routine (0 = LOAD, 1 = VERIFY)
	!by $00		 	;$94	C3P0	Flag: Serial Bus-Output Character Was Buffered
	!by $00		 	;$95	BSOUR	Buffered Character for Serial Bus
	!by $00		 	;$96	SYNO	Cassette Block Synchronization Number
	!by $00		 	;$97	XSAV	Temporary .X Register Save Area
	!by $00			;$98	LDTND	Number of Open I/O Files/Index to the End of File Tables
	!by $00			;$99	DFLTN	Default Input Device (0 = Keyboard)
	!by $03			;$9a	DFLTO	Default Output (CMD) Device ($03 = Screen)
	!by $00		 	;$9b	PRTY	Tape Character Parity
	!by $00		 	;$9c	DPSW	Flag: Tape Byte Received
	!by $00			;$9d	MSGFLG	Flag: Kernal Message Control ($00 = no messages, as in RUN mode)
				;...		($80 = control messages on, as in direct mode)
	!by $00		 	;$9e	PTR1	Tape Pass 1 Error Log Index
	!by $00		 	;$9f	PTR2	Tape Pass 2 Error Log Correction Index
	!by $00,$00,$00		;$a0	TIME	Software Jiffy Clock (3 bytes, note: RESET to zero!)
	!by $00,$00		;$a3	PCNTR	Temporary Data Storage Area (2 bytes)
	!by $00		 	;$a5	CNTDN	Cassette Synchronization Character Countdown
	!by $00		 	;$a6	BUFPNT	Count of Characters in Tape I/O Buffer
	!by $00		 	;$a7	INBIT	RS-232 Input Bits/Cassette Temporary Storage Area
	!by $00		 	;$a8	BITCI	RS-232 Input Bit Count/Cassette Temporary Storage
	!by $00		 	;$a9	RINONE	RS-232 Flag: Check for Start Bit
	!by $00		 	;$aa	RIDATA	RS-232 Input Byte Buffer/Cassette Temporary Storage
	!by $00		 	;$ab	RIPRTY	RS-232 Input Parity/Cassette Leader Counter
	!by $00,$00		;$ac	SAL	Pointer to the Starting Address of a Load/Screen Scrolling
	!by $ae,$07		;$ae	EAL	Pointer to Ending Address of Load (End of Program)

;Writing a value of $07 into EAL (hi-byte) forces Kernal LOAD routine
;to store the following bytes starting from $7b0

;Video Screen Memory Area (note: these are screen codes!)
	!by $20,$20,$20,$20,$20,$20,$20,$20	;$7b0	blank spaces (16 bytes)
	!by $20,$20,$20,$20,$20,$20,$20,$20	;....	(part of the second last screen row)

boot_msg
;Editable boot message (displayed at the bottom of screen during loading, 40 bytes)
	!by $02,$0f,$0f,$14,$09,$0e,$07,$2e	;$7c0	'BOOTING...'
	!by $2e,$2e,$20,$20,$20,$20,$20,$20
	!by $20,$20,$20,$20,$20,$20,$20,$20
	!by $20,$20,$20,$20,$20,$20,$20,$20
	!by $20,$20,$20,$20,$20,$20,$20,$20	;....	(end of screen memory)
message_len = * - boot_msg

	!by $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff	;$7e8	unused locations (16 bytes)
	!by $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

	!by $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff	;$7f8	Sprite Shape Data Pointers (8 bytes)	
	!by $00        				;$800	Begin of BASIC Text Area (must be $00)
						;$801	Source BASIC program starts here!
boot_len = * - boot_header

; --------------------
; - Strings
; --------------------

title_txt
	!by CLR_char		;clear screen
	!by RVS_on		;reverse mode
	!tx " ",program_name
	!tx " ",version
	!tx " ",program_date
	!tx " ",author
	!by " ",RVS_off,0	;normal mode

select_txt
	!by HOME_char,CRSR_down,CRSR_down
	!tx "select a program in basic text area",RETURN_char
	!tx "<crsr/down> skip, <return> select",RETURN_char
	!by RETURN_char,0

noselect_txt
	!tx "no selection",RETURN_char,0

notbasic_txt
	!tx "not basic",RETURN_char,0

edit_txt
	!tx "enter boot message, <return> to start",RETURN_char

;(add blank lines to prevent screen scroll)
	!by CRSR_down,CRSR_down,CRSR_down,CRSR_down
	!by CRSR_up,CRSR_up,CRSR_up,CRSR_up,0

process_txt
	!by RETURN_char
	!tx "processing...",RETURN_char
	!tx "<run/stop> to break",RETURN_char
	!by RETURN_char,0

break_txt
	!tx "break",RETURN_char,0

done_txt
	!tx "completed!",RETURN_char,0

error_txt
	!tx "error",RETURN_char,0

restart_txt
	!tx "<del> quit, <return> restart",RETURN_char,0

dircmd
	!tx "$"			:dircmd_len = * - dircmd

target_name
	!tx "@:"		;overwrite a previously created bootable file
	!tx left_arrow		;name suffix "<-" (left arrow)

data_buffer
;(processing data will be stored starting from here)
