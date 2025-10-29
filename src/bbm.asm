;BASIC Boot Maker
;a C64 tool to make auto-executable programs in BASIC text area
;Not only BASIC: source file could also be a BASIC stub to launch M/L code

;Version 1.05
;Released on October 29, 2025

;Copyright (c) 2025 Graham (Francesco Gramignani)
;https://graham-it.itch.io
;https://github.com/graham-it
;https://csdb.dk/scener/?id=40810

;Compilable with ACME 0.97
;the ACME Crossassembler for Multiple Environments
;Copyright (C) 1998-2020 Marco Baye

program_name	= "basic boot maker"
version		= "v1.05"
program_date	= "(c) 2025"
author		= "graham"
basic_line	= 2025
target		= 64		;Commodore 64

!ct pet				;PETSCII text conversion table
!src "cbm_map.def"		;Commodore Machines Memory Map

; --------------------
; - Consts
; --------------------

source_file	= 1		;source/dir logical file number
target_file	= 2		;target logical file number
name_max	= 16		;file name max length
entry_len	= 32		;stored entry length
list_width	= 28		;list width
buffer_pages	= 16		;buffer size (1 KB every 4 pages)

;Buffer top page
page_top = buffer_pages + >data_buffer

;Status codes
not_basic_st	= 4		;not a BASIC file
no_changes_st	= 8		;no changes
break_st	= 32		;user break
eof_st		= 64		;end of file
no_device_st	= 128		;device not present

; --------------------
; - Variables
; --------------------

last_status	= $1c		;status after last disk operation
source_len	= $1d		;source file name length
target_len	= $1e		;target file name length
last_page	= $1f		;buffer last used page
last_len	= $20		;buffer last page length

;Pointers
tmp_ptr		= $fb		;buffer pointer / current entry
end_ptr		= $fd		;end of program / last entry

;Reused locations
name_len	= $1d		;entry name length
quote_mode	= $1e		;quote mode
cur_line	= $1f		;current line

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

;Init program
	lda #0
	sta MSGFLG		;disable Kernal messages

;Display program info
	lda #<title_txt
	ldy #>title_txt
	jsr PRNSTR
	jsr RevLine

;Load directory
	jsr GetDir
	bne CheckResult		;some error

;Choose source file
	jsr SelectFile
	bne CheckResult		;some error

;Check load address
	jsr OpenSource		;open source file
	bne CloseSource		;some error

	lda #source_file	;close source file
	jsr CLOSE
	jsr CLRCH		;restore default I/O channels

	jsr BootEdit		;edit boot message
	jsr NameEdit		;edit target name
	bne CheckResult		;no name

;Confirm choices
	lda #<confirm_txt
	ldy #>confirm_txt
	jsr AskUser		;ask the user
	beq +			;continue

	ldx #no_changes_st	;cancel
	stx last_status		;set status
	bne CheckResult		;(forced)

;Start processing
+	lda #<started_txt
	ldy #>started_txt
	jsr PRNSTR

	jsr OpenSource		;re-open source file
	bne CloseSource		;some error

;Estimate program size
	jsr SourceSize
	cpx #eof_st		;end of file
	bne CloseSource		;some error

	lda #source_file	;close source file
	jsr CLOSE

	jsr SetBasicPtrs	;set BASIC pointers

;Generate target file
	lda #<process_txt
	ldy #>process_txt
	jsr PRNSTR

	jsr OpenSource		;re-open source file
	bne CloseSource		;some error

	jsr OpenTarget		;open target file
	bne CloseTarget		;some error

	jsr CopyFile		;source -> target

CloseTarget
	lda #target_file	;close target file
	jsr CLOSE

CloseSource
	lda #source_file	;close source file
	jsr CLOSE
	jsr CLRCH		;restore default I/O channels

CheckResult
	jsr GOCR		;carriage return
	ldx last_status		;check status

	lda #<not_basic_txt	;not a BASIC file
	ldy #>not_basic_txt
	cpx #not_basic_st
	beq +

	lda #<no_changes_txt	;no changes
	ldy #>no_changes_txt
	cpx #no_changes_st
	beq +

	lda #<BASMSG_break+2	;user break
	ldy #>BASMSG_break+2
	cpx #break_st
	beq +

	lda #<completed_st	;process completed
	ldy #>completed_st
	cpx #eof_st		;end of file
	beq +

	lda #<no_device_txt	;device not present
	ldy #>no_device_txt
	cpx #no_device_st
	beq +

	lda #<BASMSG_error+2	;other errors
	ldy #>BASMSG_error+2
+	jsr PRNSTR
	jsr GOCR		;carriage return

;Quit program
	lda #<quit_txt
	ldy #>quit_txt
	jsr AskUser		;ask the user
	bne +			;quit to BASIC	
	jmp ml_start		;restart program
+	rts

; --------------------
; - Subroutines
; --------------------

GetDir
;Load directory from current device
	lda #<select_txt
	ldy #>select_txt
	jsr PRNSTR

;Set directory command
	lda #dircmd_len		;name length
	ldx #<dircmd		;name address
	ldy #>dircmd
	jsr SETNAM

;Set logical file parameters
	lda #source_file	;logical file number
	ldx FADDR		;device address
	ldy #0			;sec. address (read file)
	jsr SETLFS

	jsr OPEN		;open directory
	ldx STATUS
	bne CloseDir		;some error

	ldx #source_file	;set logical file as input
	jsr CHKIN

	ldx TBLX		;preserve current line
	stx cur_line

;Set dir buffer
	lda #<data_buffer
	sta tmp_ptr
	lda #>data_buffer
	sta tmp_ptr+1

;Get disk header
	jsr GetPair		;load address
	bne CloseDir

LoadLoop
	jsr CheckLink		;link address
	bne CloseDir

	jsr GetPair		;size/partition
	bne CloseDir

	jsr GetEntry
	bne CloseDir

	ldx TBLX		;check line
	cpx #SCNROWS-1		;bottom line
	beq AddEntry

	jsr GOCR		;carriage return
	jsr PrintEntry

AddEntry
	clc
	lda tmp_ptr		;current entry
	sta end_ptr		;last entry
	adc #entry_len
	sta tmp_ptr

	lda tmp_ptr+1
	sta end_ptr+1
	adc #0
	sta tmp_ptr+1

	jsr STOP		;check STOP key
	bne LoadLoop

	ldx #break_st		;user break

CloseDir
	stx last_status		;set status

	lda #source_file	;close directory
	jsr CLOSE
	jsr CLRCH		;restore default I/O channels

	ldx last_status		;check status
	cpx #eof_st		;end of file
	beq +

	jsr GOCR		;carriage return (clear zero flag)
+	rts

GetPair
;Get file size or disk partition
	ldy #0
-	jsr CHRIN		;LSB/MSB
	sta (tmp_ptr),y
	ldx STATUS
	bne +			;some error
	iny
	cpy #2
	bne -
+	rts

CheckLink
;Check link address
	ldy #2
-	jsr CHRIN		;LSB/MSB
	beq +			;end of dir (no link)
	ldx STATUS
	bne ++			;some error
	dey
	bne -

	!by BIT2_opcode		;skip next two bytes
+	ldx #eof_st		;end of file
++	rts

GetEntry
;Get text of entry (x = 0)
	stx name_len		;clear name length
	stx quote_mode		;clear quote mode

-	jsr CHRIN
	sta (tmp_ptr),y
	beq +++			;end of line
	ldx STATUS
	bne +++			;some error

	cmp #quote_char
	bne +

	eor quote_mode		;switch quote mode
	sta quote_mode

+	lda quote_mode		;check quote mode
	beq ++

	ldx name_len
	cpx #name_max+1		;check length
	bcs -			;drop exceeding chars
	inc name_len

++	cpy #entry_len-1	;check length
	bcs -			;drop exceeding chars
	iny
	bcc -			;(forced)
+++	rts

SelectFile
;Select file from directory
	ldx #0
	stx NDX			;clear keyboard buffer

	lda #<first_entry	;first entry
	sta tmp_ptr
	ldy #>first_entry
	sty tmp_ptr+1

	cpy end_ptr+1		;check buffer
	bne +
	cmp end_ptr
	beq cancel_far		;empty dir (far branch)

+	ldx cur_line		;restore current line
	inx
	inx
	stx TBLX		;move to first entry

DirLoop
	jsr STUPT		;update screen line pointer
	jsr RevEntry		;highlight on

-	jsr GETIN		;input key
	beq -

	tax
	jsr RevEntry		;highlight off

;Prev entry
	cpx #CRSR_up
	bne ++

MovePrev
	sec			;move prev
	lda tmp_ptr
	sbc #entry_len
	sta tmp_ptr
	bcs +
	dec tmp_ptr+1

+	ldx TBLX		;check line
	beq +

	dec TBLX		;prev line
	bpl CheckTop		;(forced)

+	jsr NEWLNX		;scroll down screen and insert a blank line on top (x = line)

	lda LDTB1		;fix first screen line link
	ora #$80		;unlink from prev line (set bit 7)
	sta LDTB1

	jsr HOME		;cursor to home
	jsr PrintEntry

CheckTop
	ldy tmp_ptr+1		;check top
	cpy #>data_buffer
	bne DirLoop
	lda tmp_ptr
	cmp #<data_buffer
	bne DirLoop
	beq MoveNext		;(forced)

;Next entry
++	cpx #CRSR_down
	bne ++

MoveNext
	clc			;move next
	lda tmp_ptr
	adc #entry_len
	sta tmp_ptr
	bcc +
	inc tmp_ptr+1

+	ldx TBLX		;check line
	cpx #SCNROWS-1
	beq +

	inc TBLX		;next line
	bne CheckBot		;(forced)

+	jsr GOCR		;carriage return (scroll up screen)
	jsr PrintEntry

CheckBot
	ldy tmp_ptr+1		;check bottom
	cpy end_ptr+1
	bne DirLoop
	lda tmp_ptr
	cmp end_ptr
	bne DirLoop
	beq MovePrev		;(forced)

;Check STOP key
++	cpx #STOP_char

cancel_far
	beq +			;cancel

;Select entry
	cpx #RETURN_char
	bne DirLoop

	jsr RevEntry		;highlight selected
	jsr GetName		;extract file name
	beq +			;no name

	ldx #0			;no errors
	!by BIT2_opcode		;skip next two bytes

+	ldx #no_changes_st	;no changes
	stx last_status		;set status

	jsr GOCR		;carriage return

;Clear lower lines
	ldx TBLX		;current line
	stx cur_line

	ldx #SCNROWS-1		;bottom line
-	jsr CLRLN		;clear line
	dex
	cpx cur_line
	bcs -

	ldx last_status		;check status (set zero flag)
	rts

PrintEntry
;Print current entry
	ldy #0
	lda (tmp_ptr),y		;lo-byte
	tax
	iny
	lda (tmp_ptr),y		;hi-byte

	jsr PRNINT		;print size
	cpy #5			;check printed chars
	bcs +
	inc PNTR		;advance cursor

+	ldy #2
-	lda (tmp_ptr),y
	beq +			;end of line
	jsr CHROUT
	iny			;(max = entry_len)
	bne -
+	rts

RevLine
;Reverse whole screen line
	ldy #SCNCOLS-1		;last column
	!by BIT2_opcode		;skip next two bytes

RevEntry
;Toggle highlight of current entry
	ldy #list_width-1	;last column
-	lda (PNT),y
	eor #$80		;toggle reverse flag (bit 7)
	sta (PNT),y
	dey
	bpl -
	rts

GetName
;Extract file name from current entry
	ldx #0			;name length
	ldy #2			;buffer index
-	lda (tmp_ptr),y
	beq +			;end of line
	iny
	cmp #quote_char		;opening quote
	bne -

-	lda (tmp_ptr),y		;dir entry
	beq +			;end of line
	cmp #quote_char		;closing quote
	beq +			;found
	sta source_name,x	;file name
	iny			;(max = entry_len)
	inx
	cpx #name_max		;check length
	bne -

+	stx source_len		;set length
	txa			;check length (set zero flag)
	rts

OpenSource
;Set source file name
	lda source_len		;name length
	ldx #<source_name	;name address
	ldy #>source_name
	jsr SETNAM

;Set logical file parameters
	lda #source_file	;logical file number
	ldx FADDR		;device address
	ldy #0			;sec. address (read file)
	jsr SETLFS

	jsr OPEN		;open file
	ldy STATUS
	bne ++			;some error

	ldx #source_file	;set logical file as input
	jsr CHKIN

;Check load address
	ldx #0
-	jsr CHRIN
	ldy STATUS
	bne ++
	cmp boot_start,x	;BASIC load address
	bne +
	sta end_ptr,x		;reset end of program
	inx
	cpx #2
	bne -
	beq ++			;(forced)

+	ldy #not_basic_st	;not a BASIC file
++	sty last_status		;set status
	rts

SourceSize
;Get end of program address
	lda #<size_txt
	ldy #>size_txt
	jsr PRNSTR

-	inc end_ptr		;end of program
	bne +
	inc end_ptr+1

+	jsr CHRIN
	ldx STATUS
	bne +			;some error

	jsr STOP		;check STOP key
	bne -

	ldx #break_st		;user break
+	stx last_status		;set status
	rts

OpenTarget
;Set target file name
	lda target_len		;name length
	clc
	adc #at_cmd		;add replace command

	ldx #<target_cmd	;name address
	ldy #>target_cmd
	jsr SETNAM

;Set logical file parameters
	lda #target_file	;logical file number
	ldx FADDR		;device address
	ldy #1			;sec. address (write file)
	jsr SETLFS

	jsr OPEN		;open file
	lda STATUS
	bne +			;some error

	ldx #target_file	;set logical file as output
	jsr CHKOUT

;Write boot header
	ldy #0
-	lda boot_header,y
	jsr CHROUT
	lda STATUS
	bne +			;some error
	iny
	cpy #boot_len
	bne -

+	sta last_status		;set status
	rts

CopyFile
;Copy the rest of the file
	lda #<data_buffer	;set buffer pointer (lo-byte)
	sta tmp_ptr

CopyLoop
;Read from source file
	ldx #source_file	;set logical file as input
	jsr CHKIN

	ldx #>data_buffer	;reset page

ReadPage
	stx tmp_ptr+1		;set page
	ldy #0			;reset index

	jsr STOP		;check STOP key
	beq UserBreak

-	jsr CHRIN
	sta (tmp_ptr),y

	lda STATUS
	bne +			;some error

	iny
	bne -

	cpx #page_top		;check buffer limit
	beq ++
	inx			;next page
	bne ReadPage		;(forced)

+	cmp #eof_st		;end of file
	bne ++
	iny			;add last byte

++	sta last_status		;set status
	stx last_page		;last used page
	sty last_len		;last page length

;Write to target file
	ldx #target_file	;set logical file as output
	jsr CHKOUT

	ldx #>data_buffer	;reset page

WritePage
	stx tmp_ptr+1		;set page
	ldy #0			;reset index

	jsr STOP		;check STOP key
	beq UserBreak

-	lda (tmp_ptr),y
	jsr CHROUT

	lda STATUS
	bne ++			;some error

	cpx last_page		;check page
	beq +

	iny
	bne -

	inx			;next page
	bne WritePage		;(forced)

+	iny
	cpy last_len		;bytes to write
	bne -

+	lda last_status		;check reading status
	beq CopyLoop		;continue copying
	rts

UserBreak
	lda #break_st		;user break
++	sta last_status		;set status
	rts

BootEdit
;Let the user to edit the boot message that will be displayed at the bottom
;of the screen, while loading the generated auto-executable program

	lda #<boot_edit_txt
	ldy #>boot_edit_txt
	jsr PRNSTR

;Print source file name
	ldy #0
-	lda source_name,y
	jsr CHROUT
	iny
	cpy source_len
	bne -

;Open screen editor
	jsr CHRIN		;until carriage return

;Store message
	ldy #0
-	lda (PNT),y		;get screen codes
	sta boot_msg,y		;boot message
	iny
	cpy #message_len
	bne -
	jmp GOCR		;carriage return (then return)

NameEdit
;Edit target file name
	lda #<name_edit_txt
	ldy #>name_edit_txt
	jsr PRNSTR

;Print source file name
	ldy #0
	lda #up_arrow		;add prefix
-	jsr CHROUT
	lda source_name,y
	cpy source_len
	beq +
	iny
	cpy #name_max		;drop last char, if needed
	bne -

;Open screen editor
+	jsr CHRIN		;until carriage return

;Check name length
	ldy #name_max		;ignore exceeding chars
-	dey
	bmi +
	lda (PNT),y
	cmp #' '		;drop spaces at the end
	beq -
+	iny
	sty target_len
	bne +

	ldx #no_changes_st	;no name
	stx last_status		;set status
	rts

;Store file name
+	ldy #0
	sty PNTR		;begin of line
	inc CRSW		;set input from screen flag
-	jsr GETSCN		;screen code -> ASCII
	sta target_name,y
	iny
	cpy target_len
	bne -
	rts

SetBasicPtrs
;Set BASIC pointers (VARTAB, ARYTAB and STREND) to the end of the source program

	ldx #pointers_len
-	lda end_ptr+1		;hi-byte
	sta boot_ptrs-1,x
	dex
	lda end_ptr		;lo-byte
	sta boot_ptrs-1,x
	dex
	bne -
	rts

AskUser
;Ask the user a question
	jsr PRNSTR		;display question

	ldx #0
	stx NDX			;clear keyboard buffer

-	jsr GETIN		;input key
	cmp #RETURN_char	;RETURN key
	beq +
	cmp #STOP_char		;RUN/STOP key
	bne -

+	ldx #$40		;no key pressed
-	cpx SFDX		;wait for key release
	bne -

	cmp #RETURN_char	;check pressed key (set zero flag)
	rts

; --------------------
; - Boot header
; --------------------

;Following bytes are applied on top of the target file to make it bootable
;(for more info read the CHRGET Boot documentation)

;header size = 216 bytes	load address [2], zero page [133], screen RAM [56], other [25]
;overhead = 214 bytes		(original load address is replaced by the new one)

boot_header
	!by $2b,$00		;load address (TXTTAB)

boot_start
;(starting from TXTTAB is not strictly required, added for completeness)
	!by $01,$08		;$2b	TXTTAB	Pointer to the Start of BASIC Program Text

boot_ptrs
;BASIC pointers (to be set according to end of program address)
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
	!by $00,$00,$00		;$50	DSCPNT	Temporary Pointer to the Current String Descriptor (3 bytes)
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
	!by $80,$a3		;$71	FBUFPT	Series Evaluation Pointer (2 bytes)

;CHRGET Subroutine: Get Next BASIC Text Character (24 bytes, unaltered except TXTPTR)
	!by $e6,$7a		;$73	inc $7a
	!by $d0,$02		;$75	bne $79
	!by $e6,$7b		;$77	inc $7b
	!by $ad,$00,$08		;$79	lda TXTPTR 	Pointer to Current Text Character (MSB at $7b)
	!by $c9,$3a		;$7c	cmp #$3a	($02 = direct mode, any other value = RUN mode)
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
	!by $00,$00,$00		;$a0	TIME	Software Jiffy Clock (3 bytes, NOTE: reset to zero)
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

;Video Screen Memory Area (NOTE: these are screen codes!)
	!by $20,$20,$20,$20,$20,$20,$20,$20	;$7b0	blank spaces (16 bytes)
	!by $20,$20,$20,$20,$20,$20,$20,$20	;....	(part of the second last screen row)

boot_msg
;Editable boot message (displayed at the bottom of screen during loading)
	!by $20,$20,$20,$20,$20,$20,$20,$20	;$7c0	last screen row (40 bytes)
	!by $20,$20,$20,$20,$20,$20,$20,$20
	!by $20,$20,$20,$20,$20,$20,$20,$20
	!by $20,$20,$20,$20,$20,$20,$20,$20
	!by $20,$20,$20,$20,$20,$20,$20,$20	;....	(end of screen RAM)
message_len = * - boot_msg

;(cold start values between screen RAM and BASIC text)
	!by $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff	;$7e8	unused locations (16 bytes)
	!by $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

	!by $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff	;$7f8	Sprite Shape Data Pointers (8 bytes)	
	!by $00        				;$800	Begin of BASIC Text Area (must be $00)
						;$801	Source BASIC program starts here
boot_len = * - boot_header

; --------------------
; - Strings
; --------------------

title_txt
	!by CLR_char				;clear screen
	!tx " ",program_name
	!tx " ",version
	!tx " ",program_date
	!tx " ",author
	!by 0

select_txt
	!by RETURN_char,RETURN_char
	!tx "select a program in basic text area",RETURN_char
	!tx "<crsr up/down> scroll entries",RETURN_char
	!tx "<return> select, <run/stop> cancel"
	!by RETURN_char,0

boot_edit_txt
	!by RETURN_char
	!tx "edit boot message"
	!by RETURN_char
	!tx "booting ",0

name_edit_txt
	!by RETURN_char
	!tx "enter target file name"
	!by RETURN_char,0

confirm_txt
	!by RETURN_char,RETURN_char
	!tx "confirm your choices",RETURN_char
	!tx "<return> start, <run/stop> cancel"
	!by RETURN_char,0

started_txt
	!by RETURN_char
	!tx "making bootable file",RETURN_char
	!tx "<run/stop> break"
	!by RETURN_char,0

size_txt
	!by RETURN_char
	!tx "size check",RETURN_char,0

process_txt
	!tx "processing",RETURN_char,0

not_basic_txt
	!tx "not basic",0

no_changes_txt
	!tx "no changes",0

completed_st
	!tx "completed",0

no_device_txt
	!tx "no device",0

quit_txt
	!tx "<return> restart, <run/stop> quit"
	!by RETURN_char,0

dircmd
	!tx "$"					;dir command
dircmd_len = * - dircmd

target_cmd
	!tx "@:"				;replace command
at_cmd = * - target_cmd

target_name					;target file name
source_name = target_name+name_max		;source file name
data_buffer = source_name+name_max		;data buffer
first_entry = data_buffer+entry_len		;first entry

;End of program
prg_end

ml_size = prg_end-ml_start			;M/L code size
prg_size = prg_end-prg_start+2			;program size (+load address)
