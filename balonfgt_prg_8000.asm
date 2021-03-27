;---------------------------------------------------------;
; Balloon fight (VS system) source file                   ;
; Disassembled and ported for the NES by mr28cc'2021      ;
;---------------------------------------------------------;
; I didn't comment on this source completely.             ;
; Some comments/variable/routine names may be irrelevant. ;
;---------------------------------------------------------;
; For Public Domain                                       ;
;---------------------------------------------------------;
	; Music/sounds found
	; jmp loc_F739 ; piranha
	; jmp loc_F6BC ; falling enemy 
	; jmp loc_F326 ; pop balloon
	; jmp loc_F6CA ; high score
	; jmp loc_F733 ; level restart
	; jmp loc_F73F ; balloon trip / bonus level
	; jmp loc_F745 ; perfect score

	.include "defines.sinc"
	.include "joy.sinc"
	.include "ppudata.sinc"

	.segment "HEADER"
	
ines_MAPPER = 3 ; CNROM
ines_MIRRORING = 0 ; Horizontal mirroring

		.byte $4e,$45,$53,$1a   ; "NES"
		.byte 2	    ; Nnumber of 16k prg banks.
		.byte 4	    ; Number of 8k chr banks.
		.byte (ines_MAPPER<<4) | ines_MIRRORING
		.byte 0
		.byte 0,0,0,0,0,0,0,0

	.segment "VECTORS"
		.word nmi_call
		.word reset
		.word irq_call

	.segment "CHARS1"
	.incbin	"chr/balonfgt_chr_02.chr"

	.segment "CHARS2"
	.incbin	"chr/balonfgt_chr_01.chr"

	.segment "CHARS3"
	.segment "CHARS4"
	.incbin	"chr/balonfgt_chr_03.chr"

	.segment "RODATA"

	; Entry point
reset:
		SEI
		CLD
		LDA	#$10
		STA	PPU_CTRL
		LDA	#0
		STA	CTRL_PORT1
		STA	VS_CTRL_VAR
		LDA	#$40 ; '@'
		STA	CTRL_PORT2
		LDA	#0
		STA	APU_DMC_FREQ
		;LDA	#0
		;STA	COIN_COUNT_PORT
		LDX	#$FF
		TXS

:	; Wait for vsync
		LDA	PPU_STATUS
		AND	#$80
		BEQ	:-	

: 	; Wait for vsync again (why?)
		LDA	PPU_STATUS
		AND	#$80
		BEQ	:-	

		LDY	#7
		STY	TEMP+1
		LDY	#0
		STY	TEMP+0
		TYA			; A = 0

@clear_ram:
		STA	(0),Y		; Clear	RAM (0000-07ff)
		DEY
		BNE	@clear_ram	; Clear	RAM (0000-07ff)
		DEC	TEMP+1
		BPL	@clear_ram	; Clear	RAM (0000-07ff)

		LDA	#0			; Init var $15
		STA	$15

		LDA	#$35
		STA	TEMP_PTR_LO	; Init var $99
		LDA	#$48
		STA	TEMP_PTR_HI	; Init var $9A
		LDA	#$FF
		STA	$1B		; Init var $1B

		JSR	main_init
		JSR	sub_E637
		
		JSR	DBA2_read_DIPs
		JSR	init_high_score
		LDA	PPU_CTRL_VAR
		ORA	#$80 				;Enable NMI generating
		STA	PPU_CTRL
		STA	PPU_CTRL_VAR

; Infinite loop (game cycle located in the NMI)
:
		JMP	:-
; End of function reset


; =============== S U B	R O U T	I N E =======================================


main_init:				
		LDA	#0
		STA	APU_DMC_RAW	; DMC direct load = 0
		LDA	#$F
		STA	APU_SND_CHN	; APU status = 0000_1111, it enables:
					; DMC, noise, triangle and pulse channels
		LDA	#6		; PPU_MASK = 0000_0110,	or...
		STA	PPU_MASK	; ...show background in	leftmost 8 pixels of screen and...
					; ...show sprites in leftmost 8	pixels of screen.
		jsr readjoy
		lda BUTTONS
		and #$20
		lsr a
		lsr a
		lsr a
		lsr a
		lsr a
		sta TEMP


		; Set up coinage DIP switches
		; 0 by default : 1 coin / 1 credit
		
		LDA #%00000000 ; default DIP1 switch init
		;     76543210
		;     CNGxxxxS
		;     Low bit (S) - Service menu
		;     High three bits (CNG) - coinage 
		;	4 coins / 1 credit 	: 6 : %00000110
		;	3 coins / 1 credit 	: 5 : %00000101
		;	2 coins / 1 credit 	: 4 : %00000100
		;	1 coin / 1 credit 	: 0 : %00000000
		;	1 coin / 2 credit 	: 1 : %00000001
		;	1 coin / 3 credit 	: 2 : %00000010
		;	1 coin / 4 credit 	: 3 : %00000011
		;	Free play 			: 7 : %00000111

		ora TEMP
		sta DIP1

		;LDA #%01000010 ; default DIP2 switch init
		LDA #%00010011 ; default DIP2 switch init
		     ;|||||||| 
		     ;|||||||Unused dip. Added for NES/Arcade game mode %01 NES %00 arcade
		     ;|||||BBonus life reward. %00, %01, %10, %11 = 10000, 20000, 40000, no reward
		     ;|||||
		     ;||||Enemy regeneration: %00 low %01 high
		     ;||||
		     ;||DDifficulty: %00 easy, %01 normal, %10 medium, %11 hard 
		     ;||
		     ;LLives 3, 4, 5, 6 (%00,%01,%10,%11)
		STA DIP2

clear_bg_and_sprites:
		JSR	init_oam_buffer
		JSR	init_scroll
		JSR	clear_both_nametables
		RTS
; End of function main_init

BUTTONS = user_var1		;one byte per controller

readjoy:
		lda #$01
		sta CTRL_PORT1
		sta BUTTONS
		lsr a        ; now A is 0
		sta CTRL_PORT1
@loop:
		lda CTRL_PORT2
		lsr a	       ; bit 0 -> Carry
		rol BUTTONS  ; Carry -> bit 0; bit 7 -> Carry
		bcc @loop
		rts


	; Game main cycle		
nmi_call:
		LDA	PPU_CTRL_VAR
		AND	#$7F 				; Reset bit 7 in order to...
		STA	PPU_CTRL			; ...disable PPU NMI
		STA	PPU_CTRL_VAR		; And store it to ZP
		LDA	SELECTOR
		BNE	nmi_null_selector 	; if SELECTOR != 0 jump
		
		; Else, if SELECTOR == 0
		LDA	SELECTOR_COPY			
		BNE	nmi_null_selector
		
		; if SELECTOR_COPY == 0
		LDA	PPU_MASK_VAR
		ORA	#$1E		; Enable BG, sprites and leftmost 8px of BG and	sprites.
		STA	PPU_MASK
		STA	PPU_MASK_VAR

nmi_null_selector:
		LDA	#0
		STA	PPU_OAM_ADDR
		LDA	#2
		STA	OAM_DMA
		JSR	load_ppu_data	; Load data to the PPU from table
		LDA	#$3F
		STA	PPU_ADDR
		LDA	#0
		STA	PPU_ADDR 	; Set PPU address to $3F00 (BG palette data)

		STA	PPU_ADDR	; Write	0 to $3f00 (first palette slot)
		STA	PPU_ADDR	; Write	0 to second palette slot.
		LDA	SCROLL_X	; Update scroll	X
		STA	PPU_SCROLL
		LDA	SCROLL_Y	; and Y
		STA	PPU_SCROLL
		LDA	PROC_ID_LIST_01
		BNE	loc_8150
		JSR	poll_select2 ; I can poll service menu button there

		; MAIN MENU LOOP		
		JSR	sub_EA47_decrease_timers
		LDA	#%00001111
		STA	APU_SND_CHN
		JSR	play_music

		INC	$64
		LDA	$59F
		BNE	loc_812C
		LDA	$68
		BNE	loc_8129

		JSR	loc_9260_execute_from_list
		JMP	loc_812C
; ---------------------------------------------------------------------------

loc_8129:
		JSR	sub_A4A8

loc_812C:
		JSR	E6E9_process_starfield
		JSR	sub_E731	; Joypad's poll here
		LDA	JOY1_VAR
		sta rapid_a
		STA	$80
		AND	#3
		CMP	#3
		BNE	loc_8149
		LDA	#0
		STA	$80

loc_8149:

		LDA	JOY2_VAR
		STA	$81
		AND	#3
		CMP	#3
		BNE	loc_8156
		LDA	#0
		STA	$80

		JMP	loc_8156
; ---------------------------------------------------------------------------

loc_8150:
		JSR	exec
		JSR	sub_E731

loc_8156:
		LDA	PPU_STATUS
		LDA	PPU_CTRL_VAR
		ORA	#$80 ; '€'
		STA	PPU_CTRL
		STA	PPU_CTRL_VAR

		lda dip_NESMODE
		beq @exit
		lda $80 
		and #%01111111
		sta $80 
		lda rapid_a
		and #%10000000
		beq @exit
		inc rapid_a_timer
		lda rapid_a_timer
		and #4
		beq @1
		lda $80
		ora #$80
		sta $80
		bne @exit ; BRA
@1:
		lda $80
		and #%01111111
		sta $80

@exit:
irq_call:
		RTI
; End of function nmi_call

; =============== S U B	R O U T	I N E =======================================

cnrom_banktable:
		.byte $00, $01,	$02, $03

cnrom_set_bank:				
		AND	#4+8
		LSR	A
		LSR	A
		TAX
		STA	cnrom_banktable,X
		RTS

proc_process_dip_menu:
		lda dipmenu_state
		cmp #251
		bne @2
		jmp dipmenu_items_init
@2:
		cmp #252
		bne @3
		jmp dipmenu_switches1_init
@3:
		cmp #253
		bne @4
		jmp dipmenu_switches2_init
@4:
		cmp #254
		bne @dpm_check_buttons
		jmp update_menu_attributes

@dpm_check_buttons:
		lda JOY1_TRIG
		and #PAD_UP
		beq dpm_ch_down
		lda dipmenu_POS_Y
		beq dpm_ch_end
		jsr loc_F684
		dec dipmenu_POS_Y
		lda #254
		sta dipmenu_state

dpm_ch_down:		
		lda JOY1_TRIG
		and #PAD_DOWN
		beq dpm_ch_left
		lda dipmenu_POS_Y
		cmp #4
		beq dpm_ch_end
		jsr loc_F684
		inc dipmenu_POS_Y
		lda #254
		sta dipmenu_state

dpm_ch_left:
		lda JOY1_TRIG 
		and #PAD_LEFT
		beq dpm_ch_right
		ldx dipmenu_POS_Y
		lda dip_LIVES,x
		beq dpm_ch_end
		dec dip_LIVES,x
		lda #251
		sta dipmenu_state
		jmp _dip_values_array_update
dpm_ch_right:
		lda JOY1_TRIG
		and #PAD_RIGHT
		beq dpm_ch_start
		ldx dipmenu_POS_Y
		lda dip_LIVES,x
		cmp dip_max_values,x
		beq dpm_ch_end
		inc dip_LIVES,x
		lda #251
		sta dipmenu_state
		jmp _dip_values_array_update
		
dpm_ch_start:
		lda JOY1_TRIG
		and #PAD_START
		beq dpm_ch_end
		
		; START PRESSED

		LDA	#$0
		STA	PROC_ID_LIST_01
		STA off_CNROM
		LDA	#$3
		jsr sub_DE7F
		jmp loc_F733 ; level restart

dpm_ch_end:
		rts


update_menu_attributes:

		ldx dipmenu_POS_Y
		lda attrs_menu_LO,X
		sta $90
		lda attrs_menu_HI,X
		sta $91

		LDY	#13

:
		LDA	($90),Y
		STA	$300,Y
		DEY
		BPL	:-
		lda #255
		sta dipmenu_state
		RTS

dipmenu_items_init:
		lda #0
		sta $96
		sta $300
		sta $94
		
		lda #3
		sta $95	; $04 = $0301

@loop1:	
		lda $96
		asl a
		tax
		lda dip_switches,x
		sta $90
		lda dip_switches+1,x
		sta $91
		
		;lda #1
		ldx $96
		lda dip_LIVES,x
		asl a
		tay
		lda ($90),y
		sta $92
		iny
		lda ($90),y
		sta $93

		ldy #0
		lda ($92),y
		tax
		clc
		adc $300
		sta $300
		iny

@loop2:	
		lda ($92),y
		sta ($94),y
		iny
		dex
		bne @loop2
		txa
		sta ($94),y
		dey
		tya
		clc
		adc $94
		sta $94

		inc $96
		lda $96
		cmp #5
		bne @loop1
		iny
		inc dipmenu_state
		rts

dipmenu_switches1_init:

		ldx #0
		ldy #0
@loop1:
		lda dip_addresses+1,x
		sta $301,y
		lda dip_addresses,x
		iny
		sta $301,y
		iny
		lda #$84
		sta $301,y
		iny
		iny
		iny
		iny
		iny
		inx
		inx
		cpx #16
		bne @loop1
		lda #56
		sta $300

		ldx #0
		ldy #0

:
		txa
		asl a
		tay
		lda switches_buff_addr,y
		sta $96
		iny
		lda switches_buff_addr,y
		sta $97
		

		stx $92
		jsr draw_one_switch
		ldx $92
		inx
		cpx #4
		bne :-

		inc dipmenu_state
		rts

switches_buff_addr:
		.word $304, $312, $320, $32E
		.word $304, $312, $320, $32E

draw_one_switch:
		lda dipmenu_states_1b,x
		beq @1
		lda #<dip_oneswitch_on
		ldx #>dip_oneswitch_on
		jmp @2 

@1:
		lda #<dip_oneswitch_off
		ldx #>dip_oneswitch_off

@2:
		sta $94
		stx $95 ; store needed switch tiles address to 94 and 95

		ldy #0

:		
		lda ($94),y
		sta ($96),y
		iny
		cpy #4
		bne :-

		inc $96
		inc $96
		inc $96

:		
		lda ($94),y
		sta ($96),y
		iny
		cpy #8
		bne :-
		rts


dipmenu_switches2_init:

		ldx #0
		ldy #0
@loop1:
		lda dip_addresses+16+1,x
		sta $301,y
		lda dip_addresses+16,x
		iny
		sta $301,y
		iny
		lda #$84
		sta $301,y
		iny
		iny
		iny
		iny
		iny
		inx
		inx
		cpx #16
		bne @loop1
		lda #56
		sta $300

		ldx #4
		ldy #0

:
		txa
		asl a
		tay
		lda switches_buff_addr,y
		sta $96
		iny
		lda switches_buff_addr,y
		sta $97
		

		stx $92
		jsr draw_one_switch
		ldx $92
		inx
		cpx #8
		bne :-

		inc dipmenu_state
		rts

exec:
		LDA	PROC_ID_LIST_01
		JSR	execute_procedure
; ---------------------------------------------------------------------------
		.WORD proc_81F8 ; 0 NULL (just RTS)
		.WORD proc_81ED ; 1 INC PROC_ID_LIST_01
		.WORD proc_820B ; 2
		.WORD proc_820B ; 3
		.WORD proc_820B ; 4 
		.WORD proc_820B ; 5
		.WORD clear_SELECTOR ; 6

proc_81ED:
		INC	PROC_ID_LIST_01
		JSR	clear_ppumask	; Disable PPU.
		LDA	#$52 ; 'R'
		STA	SELECTOR
		STA	SELECTOR_COPY

proc_81F8:
		RTS

; ---------------------------------------------------------------------------
tbl_B1F9:	
		.BYTE $27, 3, 0, 0, 0
tbl_B1FE:	
		.BYTE $10, $E8,	$64
tbl_B201:	
		.BYTE $A, 1, $21, $21
tbl_B205:	
		.BYTE $22, $22,	$53, $B3, $13, $73
; ---------------------------------------------------------------------------

proc_820B:				; 817Eo 8180o	...
		LDX	#$F
		LDA	#0

loc_820F:				; 8213j
		STA	NT_ADDR_HI,X
		DEX
		BPL	loc_820F
		LDX	#0
		LDY	#4

loc_8219:				; 821Cj
		STX	TEMP,Y
		DEY
		BPL	loc_8219
		LDY	PROC_ID_LIST_01

loc_8220:				; 8249j 824Cj
		LDA	$599,Y
		BNE	loc_822A
		LDA	$595,Y
		BEQ	loc_824E

loc_822A:				; 8223j
		LDA	$599,Y
		SEC
		SBC	tbl_B1FE,X
		STA	$A
		LDA	$595,Y
		SBC	tbl_B1F9,X
		STA	$B
		BCC	loc_824B
		LDA	$A
		STA	$599,Y
		LDA	$B
		STA	$595,Y
		INC	TEMP,X
		BNE	loc_8220

loc_824B:				; 823Bj
		INX
		BNE	loc_8220

loc_824E:				; 8228j
		LDA	tbl_B201,Y
		STA	NT_ADDR_LO
		LDA	tbl_B205,Y
		STA	$302
		LDA	#6
		STA	$303
		LDY	#4

loc_8261:				; 8268j
		LDA	TEMP,Y
		STA	$305,Y
		DEY
		BPL	loc_8261
		LDY	#0

loc_826C:				; 8279j
		LDA	$304,Y
		BNE	loc_827B
		LDA	#$24 ; '$'
		STA	$304,Y
		INY
		CPY	#5
		BCC	loc_826C

loc_827B:				; 826Fj
		INC	PROC_ID_LIST_01
		RTS
; ---------------------------------------------------------------------------

clear_SELECTOR:				; 8186o
		LDA	#0
		STA	SELECTOR_COPY
		RTS

loc_9260_execute_from_list:				; 8123p
		LDA	#0
		STA	$60
		LDA	$69
		JSR	execute_procedure
; ---------------------------------------------------------------------------
		.WORD proc_DDE5 ; 0
		.WORD proc_DE1F ; 1
		.WORD proc_DEB7 ; 2
		.WORD proc_DFDC ; 3
		.WORD proc_E05D ; 4
		.WORD proc_E1D0 ; 5
		.WORD proc_E26B ; 6
		.WORD proc_E32D ; 7
		.WORD proc_E393 ; 8 Lobby handler
		.WORD proc_E3F1 ; 9
		.WORD proc_E2F3 ; A
		.WORD proc_9285 ; B Game loop init
		.WORD proc_DFA2 ; C
		.WORD proc_E04C ; D
		.WORD handler_init_dip_menu ; E
; ---------------------------------------------------------------------------

proc_9285:				; E2FEp
					; 927Fo
		LDA	#1
		STA	$60
		LDA	#0
		STA	$59
		STA	$5A
		LDA	$74
		CMP	#$D
		BEQ	loc_9298
		JSR	sub_D190

loc_9298:				; 9293j
		LDA	$74
		JSR	execute_procedure
; ---------------------------------------------------------------------------
		.WORD init_game ; 0
		.WORD proc_9391 ; 1
		.WORD proc_93EC ; 2
		.WORD proc_961E ; 3
		.WORD proc_982B ; 4
		.WORD proc_98BE ; 5
		.WORD proc_9911 ; 6
		.WORD proc_9954 ; 7
		.WORD proc_99CE ; 8
		.WORD proc_9A08 ; 9
		.WORD proc_9A1A ; A
		.WORD proc_9A42 ; B
		.WORD proc_9C1C ; C
		.WORD proc_9C89 ; D
		.WORD proc_A0E4 ; E
tbl_unk_92BB:	.BYTE $2B, $E, 6, 0, $23, $A1, 6, 0, $23, $B9, 6, 0, 4,	$8A, $80, $D9
					; loc_936Cr
		.BYTE 4, $94, $9C, $6C,	$ED, $F6, $45, $3B, $24, $AA

; =============== S U B	R O U T	I N E =======================================


init_game:				; 929Do
		JSR	clear_ppumask	; Disable PPU.
		LDA	#1
		STA	SELECTOR_COPY
		LDA	VS_CTRL_VAR
		AND	#$FB ; 'û'
		STA	VS_CTRL_VAR
		JSR	cnrom_set_bank
		JSR	clear_bg_and_sprites
		LDA	$6B
		BNE	loc_934A
		STA	$62
		STA	$63
		STA	$C9
		STA	$CA
		STA	$6D
		STA	$6E
		STA	$72
		STA	$73
		STA	$70
		STA	$6F
		STA	$7D4
		LDX	#7

loc_9305:				; 9308j
		STA	$2D,X
		DEX
		BPL	loc_9305
		STA	$589
		JSR	sub_EDC2
		LDA	#2
		STA	$3E2
		STA	$3E3
		STA	$42E
		STA	$42F
		LDA	$69
		CMP	#$A
		BNE	loc_9328
		LDA	#1
		STA	$65

loc_9328:				; 9322j
		LDX	$15
		LDA	#1
		STA	$62,X
		JSR	loc_A15E	; Init 1st player lives
		LDA	$65
		BEQ	loc_9340
		TXA
		EOR	#1
		TAX
		LDA	#1
		STA	$62,X
		JSR	loc_A15E	; Init 2nd player lives

loc_9340:				; 9333j
		LDA	#$FE ; 'þ'
		STA	$47A
		LDA	#$FF
		STA	$493

loc_934A:				; 92EAj
		LDA	#$FF
		STA	$3DA
		STA	$3DB
		STA	$4B8
		LDA	#1
		STA	$495
		LDA	#0
		STA	$6C
		STA	$7D
		STA	$7E
		STA	$4B7
		STA	$94
		STA	$7BD
		LDX	#$B

loc_936C:				; 9373j
		LDA	tbl_unk_92BB,X
		STA	$550,X
		DEX
		BPL	loc_936C
		JSR	sub_95C4
		LDA	$C8
		BNE	loc_9387
		LDA	$65
		BEQ	loc_9384
		LDA	#2
		STA	$BF

loc_9384:				; 937Ej
		INC	$74
		RTS
; ---------------------------------------------------------------------------

loc_9387:				; 937Aj
		LDA	#3
		STA	$74
; End of function init_game


; =============== S U B	R O U T	I N E =======================================


sub_938B:				; loc_98B6p 9AD3j ...
		LDA	#0
		STA	$4B7
		RTS
; End of function sub_938B

; ---------------------------------------------------------------------------

proc_9391:				; 929Fo
		LDA	$69
		CMP	#$A
		BEQ	loc_93A7
		LDA	$65
		JMP	loc_93A7
		LDA	$C6
		CMP	$BF
		BNE	locret_93A9
		LDA	#0
		STA	$BF
		STA	$C6

loc_93A7:				; 9395j 9399j
		INC	$74

locret_93A9:				; 939Fj
		RTS
; ---------------------------------------------------------------------------
byte_93AA:	
		.BYTE 3, 4, 5, 0, 5, 6,	6
byte_93B1:	
		.BYTE 0, 6, $C,	0, $12,	$18, $1E, $24, $2A
byte_93BA:	
		.BYTE 0, 0, 0, 0, 0, 0,	0, 0, 0, 1, 0, 0, 0, 0,	0, 1
		.BYTE 1, 0, 0, 0, 0, 1,	2, 0, 0, 0, 1, 1, 1, 2,	0, 0
		.BYTE 1, 1, 2, 2, 1, 0,	0, 0, 0, 0, 0, 1, 0, 0,	0, 0
unk_93EA:	
		.BYTE	7		; 9453r
		.BYTE	8
; ---------------------------------------------------------------------------

proc_93EC:				; 92A1o
		LDX	#9

loc_93EE:				; 93F4j
		LDA	#$FF
		STA	$5E6,X
		DEX
		BPL	loc_93EE
		LDX	#7

loc_93F8:				; 942Ej
		JSR	sub_A243
		STA	$3E2,X
		STA	$4AE,X
		STA	$3F2,X
		STA	$3FA,X
		STA	$41E,X
		STA	$426,X
		STA	$416,X
		STA	$454,X
		STA	$460,X
		LDA	#$FF
		STA	$574,X
		STA	$3DA,X
		STA	$56E,X
		LDA	#$B
		STA	$40E,X
		LDA	#$20 ; ' '
		STA	$3D2,X
		DEX
		CPX	#2
		BPL	loc_93F8
		LDX	$6E
		CPX	#7
		BCC	loc_943A
		LDX	#3
		STX	$6E

loc_943A:				; 9434j
		LDA	byte_93AA,X
		BEQ	loc_9447
		LDY	$6D
		CPY	#$10
		BCC	loc_9447
		LDA	#5

loc_9447:				; 943Dj 9443j
		STA	$76
		BEQ	loc_949F
		LDA	$69
		CMP	#$A
		BNE	loc_9456
		LDY	$15
		LDX	unk_93EA,Y

loc_9456:				; 944Fj
		LDY	byte_93B1,X
		LDX	#0

loc_945B:				; 9465j
		LDA	byte_93BA,Y
		STA	$456,X
		INY
		INX
		CPX	$76
		BNE	loc_945B
		JSR	sub_A391
		LDX	$76
		INX

loc_946D:				; 947Bj
		LDY	$454,X
		JSR	proc_A169
		LDA	#$71 ; 'q'
		STA	$3DA,X
		DEX
		CPX	#1
		BNE	loc_946D
		JSR	sub_A273
		LDA	#4
		LDX	$6B
		BEQ	loc_9488
		LDA	#2

loc_9488:				; 9484j
		STA	0
		LDY	#6

loc_948C:				; 9496j
		LDA	$51A,Y
		CLC
		ADC	0
		STA	$51A,Y
		DEY
		BPL	loc_948C
		LDA	#$40 ; '@'
		STA	$75
		INC	$74
		RTS
; ---------------------------------------------------------------------------

loc_949F:				; 9449j
		LDA	#$B
		STA	$74
		RTS
; ---------------------------------------------------------------------------
byte_94A4:	.BYTE $45, $46,	$47, $53, $54 ;	95C6r
byte_94A9:	.BYTE $1D, $1E,	$1F, $20, $21, $22, $23, $24, $25, $1E,	$20, $26, $20, $27, $1F, $22
					; 95D1r
		.BYTE $28, $29,	$2A, $21, $2B, $2C, $28, $2B
byte_94C1:	.BYTE $0C, $0D,	$0E, $0F, $10, $11, $0D, $12, $13, $0D,	$0D, $14, $0D, $15, $0E, $11
					; 95DBr
		.BYTE $16, $17,	$18, $10, $19, $1A, $1B
byte_94D8:	.BYTE $1C, 0, 2, 4, 6, 8, $A, $C, $E, $10, $12,	$14, $16, $18, $1A, $1C
					; 95E7r
		.BYTE $1E
byte_94E9:	.BYTE $20, 0, 4, 8, $C,	$10, $14, $18, $1C, $20, $24, $28, $2C,	$30, $34, $38
					; 9603r
		.BYTE $3C, $40,	$44
byte_94FC:	.BYTE $30, $30,	$30, $30, $20, $20, 8, 8, 8, 8,	$28, $28, $28, $28, $10, $10
					; 95F1r
		.BYTE 8, 8, 8, 8, $28, $28, 8, 8, $10, $10, 8, 8, $50, $50, 8, 8
byte_951C:	.BYTE $60, $90,	$48, $A8, $2C, $C4, $4C, $A4, $34, $C4,	$40, $B8, $10, $E0, $40, $B0
					; 95F7r
		.BYTE $48, $A8,	$38, $B8, $44, $A4, $18, $D8, $20, $D0,	$28, $C8, $50, $A0, $34, $BC
byte_953C:	.BYTE $60, $60,	$C0, $C0, $30, $30, $C0, $C0, $20, $60,	$60, $D0, $10, $40, $70, $C0
					; 960Dr
		.BYTE $28, $28,	$78, $C0, $38, $38, $90, $90, $30, $30,	$78, $78, $20, $20, $80, $C0
		.BYTE $38, $38,	$C0, $C0, $30, $30, $C0, $C0, $28, $58,	$58, $C0, $50, $50, $C0, $C0
		.BYTE $50, $50,	$C0, $C0, $18, $58, $58, $90, $30, $30,	$90, $90, $18, $18, $58, $58
		.BYTE $18, $48,	$48, $90
byte_9580:	.BYTE $68, $88,	$28, $C8, $48, $A8, $20, $D0, $78, $10,	$E8, $78, $5C, $9C, $5C, $D8
					; 9613r
		.BYTE $44, $A4,	$74, $18, $2C, $C4, $70, $80, $60, $90,	$38, $B8, $28, $C8, $E0, $20
		.BYTE $50, $98,	$20, $D0, $60, $90, $20, $D0, $78, $54,	$9C, $D8, $3C, $B4, $18, $D8
		.BYTE $28, $C8,	$22, $CE, $74, $44, $A4, $74, $64, $8C,	$2C, $C4, $7C, $34, $BC, $7C
		.BYTE $14, $34,	$BC, $D4

; =============== S U B	R O U T	I N E =======================================


sub_95C4:				; 9375p
		LDX	$70
		LDA	byte_94A4,X
		STA	$A0
		LDA	#$B
		STA	$A1
		LDX	$6F
		LDA	byte_94A9,X
		STA	$A2
		SEC
		SBC	#$1C
		STA	$9F
		LDA	byte_94C1,X
		STA	$A3
		SEC
		SBC	#$B
		STA	$9E
		LDX	$9F
		LDY	byte_94D8,X
		LDX	#1

loc_95EC:				; 95FFj
		LDA	#1
		STA	$36D,X
		LDA	byte_94FC,Y
		STA	$380,X
		LDA	byte_951C,Y
		STA	$352,X
		INY
		DEX
		BPL	loc_95EC
		LDX	$9E
		LDY	byte_94E9,X
		LDX	#3

loc_9608:				; 961Bj
		LDA	#0
		STA	$36F,X
		LDA	byte_953C,Y
		STA	$382,X
		LDA	byte_9580,Y
		STA	$354,X
		INY
		DEX
		BPL	loc_9608
		RTS
; End of function sub_95C4

; ---------------------------------------------------------------------------

proc_961E:				; 92A3o
		LDA	$4B7
		JSR	execute_procedure
; ---------------------------------------------------------------------------
		.WORD proc_9636
		.WORD proc_9636
		.WORD proc_9636
		.WORD proc_9636
		.WORD proc_966F
		.WORD proc_9713
		.WORD proc_97B6
		.WORD proc_97FE
		.WORD proc_981E
; ---------------------------------------------------------------------------

proc_9636:				; 9624o 9626o	...
		LDX	$4B7
		LDA	$A0,X
		JMP	loc_9685
; ---------------------------------------------------------------------------
tbl_963E:	.BYTE $2D ; -		; 9682r
		.BYTE $2E ; .
		.BYTE $2F ; /
		.BYTE $30 ; 0
		.BYTE $31 ; 1
		.BYTE $32 ; 2
		.BYTE $33 ; 3
		.BYTE $34 ; 4
		.BYTE $35 ; 5
		.BYTE $36 ; 6
		.BYTE $37 ; 7
		.BYTE $38 ; 8
		.BYTE $39 ; 9
		.BYTE $3A ; :
		.BYTE $3B ; ;
		.BYTE $3C ; <
		.BYTE $3D ; =
		.BYTE $3E ; >
		.BYTE $3F ; ?
		.BYTE $40 ; @
		.BYTE $41 ; A
		.BYTE $42 ; B
		.BYTE $43 ; C
		.BYTE $44 ; D
tbl_9656:	.BYTE	0		; 9671r
tbl_9657:	.BYTE	3		; 9677r
		.BYTE	5
		.BYTE	8
		.BYTE  $B
		.BYTE  $D
		.BYTE $10
		.BYTE $12
		.BYTE $15
		.BYTE $17
		.BYTE $1A
		.BYTE $1C
		.BYTE $1E
		.BYTE $21 ; !
		.BYTE $25 ; %
		.BYTE $28 ; (
		.BYTE $2B ; +
		.BYTE $2E ; .
		.BYTE $31 ; 1
		.BYTE $34 ; 4
		.BYTE $37 ; 7
		.BYTE $3B ; ;
		.BYTE $3E ; >
		.BYTE $40 ; @
		.BYTE $44 ; D
; ---------------------------------------------------------------------------

proc_966F:				; 962Co
		LDX	$6F
		LDA	tbl_9656,X
		STA	$773
		LDA	tbl_9657,X
		STA	$774
		LDA	#0
		STA	$775
		LDA	tbl_963E,X

loc_9685:				; 963Bj 9A86j	...
		STA	SELECTOR
; START	OF FUNCTION CHUNK FOR sub_985F

loc_9687:				; 97FBj loc_9852j ...
		INC	$4B7
		RTS
; END OF FUNCTION CHUNK	FOR sub_985F
; ---------------------------------------------------------------------------
tbl_968B:	.BYTE $28, $21,	$21, $21, $28, $20, $21, $28, $21, $28,	$28, $22, $22, $21, $23, $28
					; 9719r
		.BYTE $21, $28,	$21, $22, $29, $20, $28, $21, $22, $28,	$21, $28, $21, $28, $20, $22
		.BYTE $28, $21,	$22, $28, $28, $22, $28, $29, $21, $22,	$28, $21, $22, $29, $20, $21
		.BYTE $29, $20,	$22, $29, $21, $22, $28, $20, $22, $22,	$28, $20, $21, $23, $20, $23
		.BYTE $22, $22,	$28, $29
tbl_96CF:	.BYTE $94, $04,	$98, $98, $8C, $98, $04, $94, $84, $10,	$84, $04, $18, $88, $10, $84
					; 9721r
		.BYTE $84, $94,	$18, $88, $84, $8C, $10, $84, $98, $90,	$90, $84, $98, $0C, $98, $88
		.BYTE $98, $04,	$94, $84, $98, $8C, $84, $94, $0C, $94,	$88, $98, $8C, $98, $94, $04
		.BYTE $10, $88,	$14, $08, $98, $88, $94, $94, $88, $94,	$94, $98, $04, $10, $98, $0C
		.BYTE $08, $90,	$8C, $98
; ---------------------------------------------------------------------------

; =============== S U B R O U T I N E =======================================

; In: SELECTOR ($16) = table index

load_ppu_data:
                LDX     SELECTOR
                LDA     tbl_BG_screens_LO,X
                STA     TEMP            ; Temp variable.
                                        ; Usually paired with the next byte
                                        ; as a pointer for indirect addressing.
                LDA     tbl_BG_screens_HI,X
                STA     TEMP_HI
                JSR     _do_load_ppu_data
                LDA     #0
                STA     SELECTOR
                STA     NT_ADDR_HI
                STA     NT_ADDR_LO
                RTS
; End of function load_ppu_data


proc_9713:				; 962Eo
		LDY	$773
		LDX	$775
		LDA	tbl_968B,Y
		STA	$760,X
		STA	0
		LDA	tbl_96CF,Y
		STA	$764,X
		STA	1
		STA	2
		JSR	sub_CD40
		LDX	$775
		LDA	$31D
		STA	$768,X
		LDA	$31E
		STA	$76C,X
		INC	$775
		INC	$773
		LDA	$773
		CMP	$774
		BNE	locret_974E
		INC	$4B7

locret_974E:				; 9749j
		RTS
; ---------------------------------------------------------------------------
tbl_974F:	.BYTE $22, $21,	$21, $22, $28, $20, $21, $21, $21, $28,	$22, $21, $21, $22, $20, $23
					; 97BEr
		.BYTE $20, $21,	$22, $20, $28, $28, $20, $20
tbl_9767:	.BYTE $AE, $E7,	$70, $72, $2A, $8F, $B6, $8D, $41, $64,	$CD, $83, $8F, $67, $97, $B
					; 97C4r
		.BYTE $C3, $CB,	$46, $4D, $85, $B, $62,	$EC
tbl_977F:	.BYTE $FF		; loc_97CEr
		.BYTE $FF
		.BYTE $86 ; †
		.BYTE $24 ; $
		.BYTE $64 ; d
		.BYTE $65 ; e
		.BYTE $66 ; f
		.BYTE $67 ; g
		.BYTE $24 ; $
		.BYTE $FF
		.BYTE $FF
		.BYTE $86 ; †
		.BYTE $68 ; h
		.BYTE $69 ; i
		.BYTE $FD ; ý
		.BYTE $6A ; j
		.BYTE $6B ; k
		.BYTE $6C ; l
		.BYTE $FF
		.BYTE $FF
		.BYTE $86 ; †
		.BYTE $6D ; m
		.BYTE $FD ; ý
		.BYTE $FD ; ý
		.BYTE $FD ; ý
		.BYTE $6E ; n
		.BYTE $6F ; o
		.BYTE $FF
		.BYTE $FF
		.BYTE $86 ; †
		.BYTE $70 ; p
		.BYTE $71 ; q
		.BYTE $FD ; ý
		.BYTE $FD ; ý
		.BYTE $FD ; ý
		.BYTE $72 ; r
		.BYTE $FF
		.BYTE $FF
		.BYTE $86 ; †
		.BYTE $73 ; s
		.BYTE $74 ; t
		.BYTE $75 ; u
		.BYTE $FD ; ý
		.BYTE $76 ; v
		.BYTE $77 ; w
		.BYTE $FF
		.BYTE $FF
		.BYTE $86 ; †
		.BYTE $24 ; $
		.BYTE $78 ; x
		.BYTE $79 ; y
		.BYTE $7A ; z
		.BYTE $7B ; {
		.BYTE $24 ; $
		.BYTE	0
; ---------------------------------------------------------------------------

proc_97B6:				; 9630o
		LDA	$69
		CMP	#$A
		BEQ	loc_97F3
		LDX	$6F
		LDA	tbl_974F,X
		STA	$771
		LDA	tbl_9767,X
		STA	$772
		STA	1
		LDX	#$36 ; '6'

loc_97CE:				; 97D5j
		LDA	tbl_977F,X
		STA	NT_ADDR_LO,X
		DEX
		BPL	loc_97CE
		LDX	#0
		LDY	#5

loc_97DB:				; 97EEj
		LDA	$771
		STA	$301,X
		LDA	1
		STA	$302,X
		INC	1
		TXA
		CLC
		ADC	#9
		TAX
		DEY
		BPL	loc_97DB
		JSR	nullsub_1

loc_97F3:				; 97BAj
		JSR	tbl_C04B
		LDA	#0
		STA	$748
		JMP	loc_9687
; ---------------------------------------------------------------------------

proc_97FE:				; 9632o
		LDA	$6F
		CMP	#6
		BCC	loc_981A
		LDA	#0
		STA	NT_ADDR_HI
		LDX	$748
		JSR	loc_C194
		INC	$748
		LDA	$748
		CMP	$749
		BNE	locret_981D

loc_981A:				; 9802j
		INC	$4B7

locret_981D:				; 9818j
		RTS
; ---------------------------------------------------------------------------

proc_981E:				; 9634o
		JSR	sub_BD4D

loc_9821:				; loc_98B3j
		LDA	#0
		STA	$4B7
		INC	$74
		RTS
; ---------------------------------------------------------------------------
tbl_9829:	
		.BYTE	0		; 9865r
		.BYTE $11
; ---------------------------------------------------------------------------

proc_982B:				; 92A5o
		LDA	$4B7
		BNE	loc_9855
		LDA	$C8
		BNE	loc_9852
		LDX	#1

loc_9836:				; 9850j
		LDA	$62,X
		BEQ	loc_984F
		LDA	#1
		STA	$4E2,X
		STA	$4E4,X
		JSR	sub_A1C0
		JSR	sub_D604
		LDA	$6C
		BNE	loc_984F
		JSR	sub_D056

loc_984F:				; 9838j 984Aj
		DEX
		BPL	loc_9836

loc_9852:				; 9832j
		JMP	loc_9687
; ---------------------------------------------------------------------------

loc_9855:				; 982Ej
		CMP	#1
		BNE	loc_9875
		LDA	$6C
		BNE	loc_98B3
		LDX	#1

; =============== S U B	R O U T	I N E =======================================


sub_985F:				; 9870j 9D56p

; FUNCTION CHUNK AT 9687 SIZE 00000004 BYTES

		LDA	$62,X
		BEQ	loc_986F
		TXA
		PHA
		LDA	tbl_9829,X
		STA	0
		JSR	sub_E849
		PLA
		TAX

loc_986F:				; 9861j
		DEX
		BPL	sub_985F
		JMP	loc_9687
; End of function sub_985F

; ---------------------------------------------------------------------------

loc_9875:				; 9857j
		CMP	#2
		BNE	loc_988B
		LDA	#$F9 ; 'ù'
		STA	0
		JSR	sub_E9D0
		LDA	#$FF
		STA	0
		JSR	sub_E849

loc_9887:				; 98A8j
		INC	$4B7
		RTS
; ---------------------------------------------------------------------------

loc_988B:				; 9877j
		CMP	#3
		BNE	loc_98B6
		LDA	$65
		BEQ	loc_98A6
		LDX	$15
		JSR	loc_B032
		LDA	$15
		PHA
		EOR	#1
		STA	$15
		TAX
		JSR	loc_B032
		PLA
		STA	$15

loc_98A6:				; 9891j
		LDA	$C8
		BNE	loc_9887
		LDA	#0
		STA	$56
		STA	$57
		JSR	loc_E5A1

loc_98B3:				; 985Bj
		JMP	loc_9821
; ---------------------------------------------------------------------------

loc_98B6:				; 988Dj
		JSR	sub_938B ; -> returns A = 0
		STA	SELECTOR_COPY ; so reset SELECTOR_COPY
		JMP	proc_9A1A
; ---------------------------------------------------------------------------

proc_98BE:				; 92A7o
		LDA	$4B7
		BNE	loc_9904
		LDA	$6B
		BEQ	loc_98F0
		LDA	#7
		STA	$74
		LDA	#0
		STA	$79
		LDA	#1
		STA	$7B
		LDA	#$E0 ; 'à'
		LDX	$6C
		BNE	sub_98DF
		LDA	#4
		STA	$F2
		LDA	#$D0 ; 'Ð'

; =============== S U B	R O U T	I N E =======================================


sub_98DF:				; 98D7j 98F6p	...
		STA	$7A
		STA	$13
		STA	$7CF
		CLC
		ADC	#$F0 ; 'ð'
		STA	$7C
		LDA	#0
		STA	SELECTOR_COPY
		RTS
; End of function sub_98DF

; ---------------------------------------------------------------------------

loc_98F0:				; 98C5j
		LDA	#0
		STA	$79
		STA	$7B
		JSR	sub_98DF
		LDA	#1
		STA	$6B
		LDA	#8
		STA	$F2
		INC	$4B7

loc_9904:				; 98C1j
		JSR	sub_A273
		JSR	sub_A25D
		DEC	$75
		BNE	locret_9910
		INC	$74

locret_9910:				; 990Cj
		RTS
; ---------------------------------------------------------------------------

proc_9911:				; 92A9o
		LDA	$7A
		CLC
		ADC	#2
		STA	$7A
		STA	$13
		STA	$7CF
		LDA	$79
		ADC	#0
		STA	$79
		LDA	$7C
		CLC
		ADC	#2
		STA	$7C
		LDA	$7B
		ADC	#0
		STA	$7B
		LDA	$13
		CMP	#$D0 ; 'Ð'
		BCC	loc_9938
		INC	$74

loc_9938:				; 9934j loc_9A05j
		JSR	sub_A273
		JSR	sub_A25D
		RTS
; ---------------------------------------------------------------------------
tbl_993F:
		.BYTE $29 ; PPU addr hi
		.BYTE $AC ; PPU addr lo
		.BYTE	8 ; count
			  ; "PHASE  "
		.BYTE $19, $11, $A, $1C, $E, $24, $24, $24
		.BYTE $2B
		.BYTE $22
		.BYTE	3
		.BYTE $5F, $5C, 1
		.BYTE	0
tbl_9951:	
		.BYTE	2		; loc_99A6r
		.BYTE	5
		.BYTE	6
; ---------------------------------------------------------------------------

proc_9954:				; 92ABo
		LDA	$69
		CMP	#$A
		BEQ	loc_99C2
		LDA	$6C
		BNE	loc_99BF
		LDY	#0
		LDX	$300

loc_9963:				; 996Dj
		LDA	tbl_993F,Y
		STA	NT_ADDR_LO,X
		INY
		INX
		CMP	#0
		BNE	loc_9963
		LDX	$300
		LDA	$6D
		CLC
		ADC	#1
		BEQ	loc_99A4
		JSR	sub_A27B
		STA	$30B,X
		STA	$311,X
		TYA
		JSR	sub_A27B
		CPY	#0
		BNE	loc_9990
		LDY	#$24 ; '$'
		CMP	#0
		BEQ	loc_99B3

loc_9990:				; 9988j
		STA	$30A,X
		STA	$310,X
		TYA
		STA	$309,X
		CMP	#$24 ; '$'
		BEQ	loc_99A1
		STA	$30F,X

loc_99A1:				; 999Cj
		JMP	loc_99B3
; ---------------------------------------------------------------------------

loc_99A4:				; 9977j
		LDY	#2

loc_99A6:				; 99B1j
		LDA	tbl_9951,Y
		STA	$30B,X
		STA	$311,X
		DEX
		DEY
		BPL	loc_99A6

loc_99B3:				; 998Ej loc_99A1j
		LDA	#$11
		CLC
		ADC	$300
		STA	$300
		JMP	loc_99C2
; ---------------------------------------------------------------------------

loc_99BF:				; 995Cj
		JSR	sub_D016

loc_99C2:				; 9958j 99BCj
		LDA	#$30 ; '0'
		STA	$75
		JMP	loc_9A03
; ---------------------------------------------------------------------------
tbl_99C9:	.BYTE $29 ; )		; loc_99DBr
		.BYTE $AC ; ¬
		.BYTE $48 ; H
		.BYTE $24 ; $
		.BYTE	0
; ---------------------------------------------------------------------------

proc_99CE:				; 92ADo
		DEC	$75
		BNE	loc_9A05
		LDA	$6C
		BNE	loc_99F3
		LDX	NT_ADDR_HI
		LDY	#0

loc_99DB:				; 99E5j
		LDA	tbl_99C9,Y
		STA	NT_ADDR_LO,X
		INX
		INY
		CMP	#0
		BNE	loc_99DB
		LDA	#5
		CLC
		ADC	NT_ADDR_HI
		STA	NT_ADDR_HI
		JMP	loc_9A03
; ---------------------------------------------------------------------------

loc_99F3:				; 99D4j
		LDY	#9
		LDX	#0
		LDA	#$F8 ; 'ø'

loc_99F9:				; 9A01j
		STA	$2C0,X
		INX
		INX
		INX
		INX
		DEY
		BPL	loc_99F9

loc_9A03:				; 99C6j 99F0j
		INC	$74

loc_9A05:				; 99D0j
		JMP	loc_9938
; ---------------------------------------------------------------------------

proc_9A08:				; 92AFo
		LDA	#0
		STA	$74
		LDA	#1
		STA	$68
		LDA	$69
		CMP	#$A
		BNE	locret_9A19
		JSR	sub_EEEE

locret_9A19:				; 9A14j
		RTS
; ---------------------------------------------------------------------------

proc_9A1A:				; 98BBj A12Ej
					; ...
		LDA	#$10
		STA	$4F
		JSR	sub_BD4D
		JSR	init_oam_buffer
		LDA	#$D0 ; 'Ð'
		STA	SCROLL_Y
		LDA	#0
		STA	$6B
		STA	$74
		LDA	#1
		STA	$F2
		LDA	#$C
		STA	$69
		LDA	#$48 ; 'H'
		STA	SELECTOR

check_tbl_9A3A:				; loc_9AB4r
		RTS
; ---------------------------------------------------------------------------
		.BYTE $4A ; J
		.BYTE $4B ; K
		.BYTE $4C ; L
tbl_unk_9A3E:	.BYTE $4D, $4E,	$4F, $50 ; loc_9AC2r loc_9D27r
					; Lenght 4 checked.
; ---------------------------------------------------------------------------

proc_9A42:				; 92B3o
		LDA	$4B7
		BNE	loc_9A78
		LDA	#0
		STA	$97
		STA	$98
		LDA	#1
		STA	$6C
		LDA	#$FF
		STA	$5FA
		LDA	$1C
		AND	#3
		TAX
		LDA	$1F,X
		AND	#7
		LDX	$72
		CPX	#3
		LDX	#3
		BCC	loc_9A6A
		INX
		EOR	#$80 ; '€'

loc_9A6A:				; 9A65j
		STX	$4EA
		STA	$4E9
		LDA	#2
		STA	$4EB
		JMP	loc_9687
; ---------------------------------------------------------------------------

loc_9A78:				; 9A45j
		CMP	#1
		BNE	loc_9A80
		JSR	sub_9B5E
		RTS
; ---------------------------------------------------------------------------

loc_9A80:				; 9A7Aj
		CMP	#2
		BNE	loc_9A89
		LDA	#$49 ; 'I'
		JMP	loc_9685
; ---------------------------------------------------------------------------

loc_9A89:				; 9A82j
		CMP	#3
		BNE	loc_9ABA
		LDX	#9

loc_9A8F:				; 9A98j
		LDA	#$FF
		STA	$5E6,X
		JSR	sub_A472
		DEX
		BPL	loc_9A8F
		INC	$70
		LDA	$70
		CMP	#5
		BCC	loc_9AA6
		LDA	#0
		STA	$70

loc_9AA6:				; 9AA0j
		INC	$72
		INC	$73
		LDX	$73
		CPX	#5
		BCC	loc_9AB4
		LDX	#1
		STX	$73

loc_9AB4:				
		; WARNING!
		; The table refers to code (RTS) instead of data.
		; Need to check it.
		LDA	check_tbl_9A3A,X 
		JMP	loc_9685
; ---------------------------------------------------------------------------

loc_9ABA:				; 9A8Bj
		LDX	$72
		CPX	#4
		BCC	loc_9AC2
		LDX	#3

loc_9AC2:				; 9ABEj
		LDA	tbl_unk_9A3E,X
		STA	SELECTOR
		LDA	#4
		STA	$74
		LDA	#$40 ; '@'
		STA	$75
		LDA	#$20 ; ' '
		STA	$F2
		JMP	sub_938B
; ---------------------------------------------------------------------------
tbl_9AD6:	
		.BYTE $2A, $FF,	1, $93,	$2A, $FF, $FF, $94, $2A, $FF, 1, $95, $2A, $FF,	$FF, $96
tbl_9AE6:	
		.BYTE $00, $20,	$40, $60, $80, $A0, $C0, $E0
tbl_9AEE:	
		.BYTE $C8, $C7,	$C6, $C5, $C4, $CB, $CA, $C9
tbl_9AF6:	
		.BYTE $00, $40,	$60, $20, $A0, $C0, $00, $80
tbl_9AFE:	
		.BYTE $C8, $C6,	$C5, $CE, $CB, $CA, $CF, $C4
tbl_9B06:	
		.BYTE $00, $04,	$08, $0C, $10, $14, $18, $1C, $20, $25,	$2A, $2F, $34, $39, $3E, $43
tbl_9B16:	
		.BYTE $04, $0B,	$13, $1A, $04, $0A, $14, $1A, $03, $09,	$15, $1B, $04, $0D, $11, $1A
		.BYTE $03, $07,	$17, $1B, $05, $0C, $12, $19, $04, $0D,	$11, $1A, $03, $09, $15, $1B
		.BYTE $04, $09,	$0F, $15, $1A, $04, $0A, $0F, $14, $1A,	$03, $09, $0F, $15, $1B, $03
		.BYTE $08, $0F,	$16, $1B, $04, $0B, $0F, $13, $1A, $03,	$07, $0F, $17, $1B, $03, $09
		.BYTE $0F, $15,	$1B, $04, $0B, $0F, $13, $1A

; =============== S U B	R O U T	I N E =======================================

sub_9B5E:				; 9A7Cp
		LDY	#$FF
		STY	$4E6
		STY	$4E7

loc_9B66:				; 9B6Dj
		INY
		LDA	tbl_9AD6,Y
		STA	$301,Y
		BNE	loc_9B66
		LDX	$4EA
		LDA	$1C,X
		AND	#3
		TAX
		LDA	$4EA
		AND	#1
		BNE	loc_9B82
		INX
		INX
		INX
		INX

loc_9B82:				; 9B7Cj
		LDA	$65
		BEQ	loc_9BAC
		LDA	$72
		CMP	#2
		BCC	loc_9BAC
		LDA	$4EB
		BEQ	loc_9BAC
		CMP	#2
		BNE	loc_9B9C
		LDA	$4EA
		BNE	loc_9B9C
		LDX	#6

loc_9B9C:				; 9B93j 9B98j
		LDY	tbl_9AFE,X
		CPY	#$CE ; 'Î'
		BCC	loc_9BA6
		DEC	$4EB

loc_9BA6:				; 9BA1j
		LDA	tbl_9AF6,X
		JMP	loc_9BB2
; ---------------------------------------------------------------------------

loc_9BAC:				; 9B84j 9B8Aj	...
		LDY	tbl_9AEE,X
		LDA	tbl_9AE6,X

loc_9BB2:				; 9BA9j
		PHA
		STY	$307
		STY	$30F
		CPY	#$C9 ; 'É'
		BCC	loc_9BCD
		DEC	$301
		DEC	$309
		CPY	#$C9 ; 'É'
		BEQ	loc_9BCD
		DEC	$305
		DEC	$30D

loc_9BCD:				; 9BBBj 9BC5j
		LDY	$4E9
		TYA
		ASL	A
		BCC	loc_9BD8
		LSR	A
		ADC	#8
		TAY

loc_9BD8:				; 9BD2j
		LDA	tbl_9B06,Y
		CLC
		ADC	$4EA
		TAY
		PLA
		ADC	tbl_9B16,Y
		STA	1
		STA	$302
		STA	$30A
		INC	$30A
		CLC
		ADC	#$20 ; ' '
		STA	$30E
		STA	$306
		INC	$30E
		LDA	$301
		STA	0
		JSR	sub_E604
		LDY	$4EA
		LDA	5
		SEC
		SBC	#$10
		STA	$4FE,Y
		LDA	4
		STA	$4F9,Y
		DEC	$4EA
		BPL	locret_9C1B
		INC	$4B7

locret_9C1B:				; 9C16j
		RTS
; End of function sub_9B5E

; ---------------------------------------------------------------------------

proc_9C1C:				; 92B5o
		LDA	#0
		STA	NT_ADDR_HI
		JSR	sub_E55C
		JSR	sub_B015
		LDA	$4B7
		BNE	loc_9C40
		LDA	#$80 ; '€'
		STA	$75
		INC	$6E
		LDA	$6C
		BNE	loc_9C3C
		LDA	#2
		STA	$F2
		INC	$6D

loc_9C3C:				; 9C34j
		INC	$4B7
		RTS
; ---------------------------------------------------------------------------

loc_9C40:				; 9C2Aj
		DEC	$75
		BNE	loc_9C55
		LDA	#$D
		LDX	$6C
		BNE	loc_9C4B
		TXA

loc_9C4B:				; 9C48j
		STA	$74
		LDA	#1
		STA	$5FB
		JSR	sub_938B

loc_9C55:				; 9C42j
		LDA	#0
		STA	$80
		STA	$81
		LDX	#7

loc_9C5D:				; 9C86j
		LDA	$3DA,X
		BNE	loc_9C68
		JSR	sub_D644
		JMP	loc_9C85
; ---------------------------------------------------------------------------

loc_9C68:				; 9C60j
		CMP	#$FF
		BEQ	loc_9C85
		AND	#$F0 ; 'ð'
		CMP	#$C0 ; 'À'
		BEQ	loc_9C7C
		CMP	#$E0 ; 'à'
		BEQ	loc_9C82
		JSR	sub_AE97
		JMP	loc_9C85
; ---------------------------------------------------------------------------

loc_9C7C:				; 9C70j
		JSR	sub_D370
		JMP	loc_9C85
; ---------------------------------------------------------------------------

loc_9C82:				; 9C74j
		JSR	piranha		; Big piranha!

loc_9C85:				; 9C65j 9C6Aj	...
		DEX
		BPL	loc_9C5D
		RTS
; ---------------------------------------------------------------------------

proc_9C89:				; 92B7o
		JSR	sub_A073
		DEC	$5FB
		BNE	proc_9CAF
		LDA	#$15
		STA	$5FB
		LDA	$6C
		JSR	execute_procedure
; ---------------------------------------------------------------------------
		.WORD proc_9CAF
		.WORD proc_9CDF
		.WORD proc_9D6E
		.WORD proc_9DAC
		.WORD proc_9DC6
		.WORD proc_9DEF
		.WORD proc_9E31
		.WORD proc_9E51
		.WORD proc_9F1A
		.WORD proc_9F61
; ---------------------------------------------------------------------------

proc_9CAF:				; 9C8Fj
					; 9C9Bo
		RTS
; ---------------------------------------------------------------------------
tbl_9CB0:	
		.BYTE $20, $45,	$06, $00, $20, $58, $06, $00, $21, $94,	$05, $00, $22, $14, $05, $00
tbl_9CC0:	
		.BYTE $23, $C0,	$43, $AA, $23, $C5, $43, $FF, $23, $D9,	$02, $AA, $22, $23, $E1, $02
		.BYTE $FF, $33,	$20, $42, $03, $8F, $90, $91, $20, $55,	$03, $92, $90, $91, $00
; ---------------------------------------------------------------------------

proc_9CDF:				; 9C9Do
		LDA	#1
		STA	$5FB
		LDA	$4B7
		BNE	loc_9D1B
		JSR	clear_ppumask	; Disable PPU.
		LDA	#1
		STA	SELECTOR_COPY
		JSR	clear_both_nametables
		JSR	init_oam_buffer
		LDX	#$F

loc_9CF8:				; 9CFFj
		LDA	tbl_9CB0,X
		STA	$554,X
		DEX
		BPL	loc_9CF8
		LDA	$65
		BNE	loc_9D13
		LDA	$15
		BEQ	loc_9D13
		LDA	#$21 ; '!'
		STA	$560
		LDA	#$94 ; '”'
		STA	$561

loc_9D13:				; 9D03j 9D07j
		LDA	#1
		STA	$F0
		LDA	#$51 ; 'Q'
		BNE	loc_9D2A

loc_9D1B:				; 9CE7j
		CMP	#1
		BNE	loc_9D2D
		LDX	$72
		CPX	#4
		BCC	loc_9D27
		LDX	#3

loc_9D27:				; 9D23j
		LDA	tbl_unk_9A3E,X

loc_9D2A:				; 9D19j
		JMP	loc_9685
; ---------------------------------------------------------------------------

loc_9D2D:				; 9D1Dj
		CMP	#2
		BNE	loc_9D54
		LDX	#$1E

loc_9D33:				; 9D3Aj
		LDA	tbl_9CC0,X
		STA	$301,X
		DEX
		BPL	loc_9D33
		LDA	$65
		BNE	loc_9D51
		STA	$319
		LDA	$15
		BEQ	loc_9D51
		LDA	#$55 ; 'U'
		STA	$314
		LDA	#$D9 ; 'Ù'
		STA	$30F

loc_9D51:				; 9D3Ej 9D45j
		JMP	loc_9687
; ---------------------------------------------------------------------------

loc_9D54:				; 9D2Fj
		LDX	#1
		JSR	sub_985F
		LDA	#0
		LDX	#7

loc_9D5D:				; 9D60j
		STA	$35,X
		DEX
		BPL	loc_9D5D
		STA	$79
		STA	$7B
		JSR	sub_98DF

loc_9D69:				; 9DA9j 9DB1j	...
		INC	$6C
		RTS
; ---------------------------------------------------------------------------
tbl_9D6C:	
		.BYTE $58
		.BYTE $78
; ---------------------------------------------------------------------------

proc_9D6E:				; 9C9Fo
		LDY	#0
		LDX	#0

loc_9D72:				; 9D9Cj
		LDA	$62,X
		BEQ	loc_9D99
		TYA
		PHA
		JSR	sub_A1C0
		PLA
		TAY
		PHA
		LDA	#1
		STA	$3C1,X
		LDA	#$18
		STA	$350,X
		LDA	#0
		STA	$36B,X
		LDA	tbl_9D6C,Y
		STA	$37E,X
		JSR	sub_D2EF
		PLA
		TAY
		INY

loc_9D99:				; 9D74j
		INX
		CPX	#2
		BMI	loc_9D72
		LDA	$F1
		ORA	#$40 ; '@'
		STA	$F1
		LDA	#0
		STA	$5FC
		JMP	loc_9D69
; ---------------------------------------------------------------------------

proc_9DAC:				; 9CA1o
		LDA	#0
		STA	$5FA
		JMP	loc_9D69
; ---------------------------------------------------------------------------
tbl_9DB4: ; Apparently PPU data for updating BG
		.BYTE $21, $8B ; PPU address
		.BYTE 1
		.BYTE $2B
		.BYTE $22, $B
		.BYTE 1
		.BYTE $2B
		.BYTE 0
		.BYTE $21, $92
		.BYTE 1
		.BYTE $29
		.BYTE $22, $12
		.BYTE 1
		.BYTE $29
		.BYTE 0
; ---------------------------------------------------------------------------

proc_9DC6:				; 9CA3o
		LDX	#8

loc_9DC8:				; 9E38j
		LDY	#8

loc_9DCA:				; 9DD2j
		LDA	tbl_9DB4,X
		STA	$301,Y
		DEX
		DEY
		BPL	loc_9DCA
		LDA	$65
		BNE	loc_9DDB
		STA	$305

loc_9DDB:				; 9DD6j loc_9E1Dj ...
		LDA	$F1
		ORA	#$40 ; '@'
		STA	$F1
		JMP	loc_9D69
; ---------------------------------------------------------------------------
tbl_9DE4: ; PPU data
		.BYTE $21,$8E
		.BYTE 2
		.BYTE 0, 0
		.BYTE $22, $E
		.BYTE 2
		.BYTE 0, 0
		.BYTE 0
; ---------------------------------------------------------------------------

proc_9DEF:				; 9CA5o
		LDY	#$A

loc_9DF1:				; 9DF8j
		LDA	tbl_9DE4,Y
		STA	$301,Y
		DEY
		BPL	loc_9DF1
		LDA	$65
		BNE	loc_9E06
		LDA	$15
		BEQ	loc_9E06
		LDA	$98
		STA	$97

loc_9E06:				; 9DFCj 9E00j
		LDA	$97
		LDX	#0
		JSR	sub_9E20
		LDA	$65
		BNE	loc_9E16
		STA	$306
		BEQ	loc_9E1D

loc_9E16:				; 9E0Fj
		LDA	$98
		LDX	#5
		JSR	sub_9E20

loc_9E1D:				; 9E14j
		JMP	loc_9DDB

; =============== S U B	R O U T	I N E =======================================


sub_9E20:				; 9E0Ap 9E1Ap
		JSR	sub_A27B
		STA	$305,X
		TYA
		CMP	#0
		BNE	loc_9E2D
		LDA	#$24 ; '$'

loc_9E2D:				; 9E29j
		STA	$304,X
		RTS
; End of function sub_9E20

; ---------------------------------------------------------------------------

proc_9E31:				; 9CA7o
		LDA	#1
		STA	$5FA
		LDX	#$11
		JMP	loc_9DC8
; ---------------------------------------------------------------------------
tbl_9E3B:
		; PPU data
		.BYTE $21, $9A
		.BYTE 4
		.BYTE $19
		.BYTE $1D
		.BYTE $1C
		.BYTE $26
		.BYTE $22, $1A
		.BYTE 4
		.BYTE $19
		.BYTE $1D
		.BYTE $1C
		.BYTE $26

tbl_9E49:	
		.BYTE	0		; loc_9ED7r
		.BYTE	3
		.BYTE	5
		.BYTE	7

tbl_9E4D:	
		.BYTE  $A
		.BYTE  $B
tbl_9E4F:	
		.BYTE $22
		.BYTE $33
; ---------------------------------------------------------------------------

proc_9E51:				; 9CA9o
		JSR	sub_9ECF
		STA	$D
		LDA	$97
		STA	$E
		LDA	$98
		STA	$F
		LDX	#0

loc_9E60:				; 9E7Aj 9EAAj
		TXA
		PHA
		LDA	$62,X
		BEQ	loc_9EA5
		LDA	$D
		STA	0
		LDA	tbl_9E4D,X
		STA	1
		DEC	$E,X
		LDA	$E,X
		BMI	loc_9E7D
		JSR	sub_E8DE
		PLA
		TAX
		JMP	loc_9E60
; ---------------------------------------------------------------------------

loc_9E7D:				; 9E73j
		LDA	tbl_9E4F,X
		STA	0
		JSR	sub_E849
		LDA	$300
		SEC
		SBC	#5
		TAY
		LDX	#0

loc_9E8E:				; 9EA3j
		LDA	$301,Y
		BEQ	loc_9E97
		CMP	#$8E ; 'Ž'
		BNE	loc_9EA5

loc_9E97:				; 9E91j
		LDA	#$24 ; '$'
		STA	$301,Y
		STA	$309,Y
		INY
		INX
		CPX	#4
		BMI	loc_9E8E

loc_9EA5:				; 9E64j 9E95j
		PLA
		TAX
		INX
		CPX	#2
		BMI	loc_9E60
		LDX	$300
		LDY	#0

loc_9EB1:				; 9EBBj
		LDA	tbl_9E3B,Y
		STA	$301,X
		INX
		INY
		CPY	#$F
		BMI	loc_9EB1
		LDX	$300
		LDA	$65
		BNE	loc_9EC7
		STA	$308,X

loc_9EC7:				; 9EC2j
		LDA	#1
		STA	$5FB
		JMP	loc_9DDB

; =============== S U B	R O U T	I N E =======================================


sub_9ECF:				; proc_9E51p A0CDp
		LDX	$72
		CPX	#4
		BCC	loc_9ED7
		LDX	#3

loc_9ED7:				; 9ED3j
		LDA	tbl_9E49,X
		RTS
; End of function sub_9ECF

; ---------------------------------------------------------------------------
tbl_9EDB:	
		; PPU data
		.BYTE $22, $88
		.BYTE $11
		.BYTE $19, $24, $0E, $24, $1B, $24, $0F, $24, $0E, $24, $0C, $24, $1D
		.BYTE $24, $2C,	$2C, $2C
		.BYTE $22, $C5
		.BYTE $18
		.BYTE $1C, $1E, $19, $0E, $1B, $24, $0B, $18 
		.BYTE $17, $1E, $1C, $24, $24, $24, $01, $00
		.BYTE $00, $00, $00, $24, $19, $1D, $1C, $2C
		.BYTE $00
		.BYTE $22, $88
		.BYTE $54 ; Bit 6 set, so we need repeat next byte $54-$40 times
		.BYTE $24

tbl_9F0F:	
		.BYTE $00, $01,	$01, $02, $02
tbl_9F14:	
		.BYTE $03, $00,	$05, $00, $05, $00
; ---------------------------------------------------------------------------

proc_9F1A:				; 9CABo
		LDA	#0
		STA	$5FD
		LDA	$97
		LDX	$65
		BEQ	loc_9F28
		CLC
		ADC	$98

loc_9F28:				; 9F23j
		CMP	#$14
		BNE	loc_9F56
		LDY	$72
		CPY	#6
		BCC	loc_9F34
		LDY	#5

loc_9F34:				; 9F30j
		TYA
		STA	$5FD
		LDX	#$2F ; '/'

loc_9F3A:				; 9F41j
		LDA	tbl_9EDB,X
		STA	NT_ADDR_LO,X
		DEX
		BPL	loc_9F3A
		LDY	$5FD
		LDA	tbl_9F0F,Y
		STA	$326
		LDA	tbl_9F14,Y
		STA	$327
		LDA	#$10
		STA	$F2

loc_9F56:				; 9F2Aj
		JSR	sub_938B
		LDA	#$C0 ; 'À'
		STA	$5FB
		JMP	loc_9D69
; ---------------------------------------------------------------------------

proc_9F61:				; 9CADo
		LDA	#1
		STA	$5FB
		LDA	$4B7
		BNE	loc_9F7A
		INC	$4B7

; =============== S U B	R O U T	I N E =======================================


sub_9F6E:				; 9FA4p
		LDA	#4
		STA	$43E
		LDA	$F1
		ORA	#1
		STA	$F1
		RTS
; End of function sub_9F6E

; ---------------------------------------------------------------------------

loc_9F7A:				; 9F69j
		CMP	#1
		BNE	loc_9FBA
		LDY	#0
		LDA	$62
		BEQ	loc_9F90
		LDA	$36
		ORA	$37
		BEQ	loc_9F90
		LDX	#0
		JSR	sub_9FEA
		INY

loc_9F90:				; 9F82j 9F88j
		LDA	$63
		BEQ	loc_9F9F
		LDA	$3A
		ORA	$3B
		BEQ	loc_9F9F
		LDX	#1
		JSR	sub_9FEA

loc_9F9F:				; 9F92j 9F98j
		DEC	$43E
		BNE	loc_9FA7
		JSR	sub_9F6E

loc_9FA7:				; 9FA2j
		LDA	$36
		ORA	$37
		ORA	$3A
		ORA	$3B
		BNE	locret_9FB9
		LDA	#$40 ; '@'

loc_9FB3:				; 9FD3j
		STA	$5FB
		INC	$4B7

locret_9FB9:				; 9FAFj
		RTS
; ---------------------------------------------------------------------------

loc_9FBA:				; 9F7Cj
		CMP	#2
		BNE	loc_9FD6
		LDA	$5FD
		BEQ	loc_9FD1
		LDA	$F1
		ORA	#1
		STA	$F1
		LDX	#1

loc_9FCB:				; 9FCFj
		JSR	sub_A039
		DEX
		BPL	loc_9FCB

loc_9FD1:				; 9FC1j
		LDA	#$C0 ; 'À'
		JMP	loc_9FB3
; ---------------------------------------------------------------------------

loc_9FD6:				; 9FBCj
		LDA	#$F9 ; 'ù'
		STA	0
		JSR	sub_E9D0
		LDA	#0
		STA	$74
		RTS
; ---------------------------------------------------------------------------
tbl_9FE2:	.BYTE  $A		; 9FF2r
		.BYTE  $B
tbl_9FE4:	.BYTE	8		; A000r A04Br
		.BYTE	9
tbl_9FE6:	.BYTE	2		; A00Ar
		.BYTE $13
tbl_9FE8:	.BYTE	0		; A014r
		.BYTE $11

; =============== S U B	R O U T	I N E =======================================


sub_9FEA:				; 9F8Cp 9F9Cp
		STY	$F
		STX	$E
		LDA	#1
		STA	TEMP
		LDA	tbl_9FE2,X
		STA	TEMP_HI
		JSR	sub_E8DA
		LDX	$E
		LDA	#1
		STA	TEMP
		LDA	tbl_9FE4,X
		STA	TEMP_HI
		JSR	sub_E8DE
		LDX	$E
		LDA	tbl_9FE6,X
		STA	TEMP
		JSR	sub_E849
		LDX	$F
		LDY	tbl_9FE8,X
		LDX	#0

loc_A019:				; A02Bj
		LDA	$304,Y
		BEQ	loc_A022
		CMP	#$8E ; 'Ž'
		BNE	loc_A02D

loc_A022:				; A01Cj
		LDA	#$24 ; '$'
		STA	$304,Y
		INY
		INX
		CPX	#4
		BMI	loc_A019

loc_A02D:				; A020j
		LDY	$F
		LDX	$E
		RTS
; End of function sub_9FEA

; ---------------------------------------------------------------------------
tbl_A032:	
		.BYTE	0		; A05Br
tbl_A033:	
		.BYTE $11		; A046r
		.BYTE $20
		.BYTE $30 ; 0
		.BYTE $40 ; @
		.BYTE $50 ; P
		.BYTE $60 ; `

; =============== S U B	R O U T	I N E =======================================


sub_A039:				; loc_9FCBp
		LDA	$62,X
		BEQ	locret_A065
		LDA	#5
		STA	$F

loc_A041:				; A057j
		TXA
		PHA
		LDY	$5FD
		LDA	tbl_A033,Y
		STA	0
		LDA	tbl_9FE4,X
		STA	1
		JSR	sub_E8DE
		PLA
		TAX
		DEC	$F
		BNE	loc_A041
		TXA
		PHA
		LDA	tbl_A032,X
		STA	0
		JSR	sub_E849
		PLA
		TAX

locret_A065:				; A03Bj
		RTS
; End of function sub_A039

; ---------------------------------------------------------------------------
tbl_A066:	
		; PPU data
		.BYTE $21, $87
		.BYTE $03
		.BYTE $02, $00, $00
		.BYTE $22, $07
		.BYTE $03
		.BYTE $02, $00, $00
		.BYTE $00

; =============== S U B	R O U T	I N E =======================================


sub_A073:				; proc_9C89p
		LDX	$65

loc_A075:				; A09Cj
		LDA	$5FA
		BMI	locret_A0E3
		STA	$5E6,X
		LDA	#1
		STA	$3C3,X
		LDA	#$38 ; '8'
		STA	$352,X
		LDA	#0
		STA	$36D,X
		LDA	tbl_9D6C,X
		STA	$380,X
		INX
		INX
		LDA	#0
		JSR	sub_D467
		DEX
		DEX
		DEX
		BPL	loc_A075
		LDA	$5E6
		CMP	#2
		BCC	locret_A0E3
		INC	$5FC
		LDA	$5FC
		CMP	#5
		BNE	locret_A0E3
		LDA	#$F8 ; 'ø'
		LDX	#7

loc_A0B3:				; A0BBj
		STA	OAM_BUFFER,Y
		INY
		INY
		INY
		INY
		DEX
		BPL	loc_A0B3
		LDA	#$FF
		STA	$5FA
		LDY	#$C

loc_A0C4:				; A0CBj
		LDA	tbl_A066,Y
		STA	$301,Y
		DEY
		BPL	loc_A0C4
		JSR	sub_9ECF
		STA	$304
		STA	$30A
		LDA	$F0
		ORA	#2
		STA	$F0
		LDA	$65
		BNE	locret_A0E3
		STA	$307

locret_A0E3:				; A078j A0A3j	...
		RTS
; End of function sub_A073

; ---------------------------------------------------------------------------

proc_A0E4:				; 92B9o
		LDX	$61
		JSR	sub_AE97
		LDX	#7

loc_A0EB:				; A106j
		LDA	$3DA,X
		CMP	#$FF
		BEQ	loc_A105
		CPX	$61
		BEQ	loc_A105
		AND	#$F0 ; 'ð'
		CMP	#$C0 ; 'À'
		BEQ	loc_A102
		JSR	sub_D644
		JMP	loc_A105
; ---------------------------------------------------------------------------

loc_A102:				; A0FAj
		JSR	sub_D370

loc_A105:				; A0F0j A0F4j	...
		DEX
		BPL	loc_A0EB
		LDA	#$F8 ; 'ø'
		STA	$2F0
		STA	$2F4
		STA	$2F8
		STA	$2FC
		LDX	$61
		LDA	$3DA,X
		PHA
		AND	#$F0 ; 'ð'
		CMP	#$E0 ; 'à'
		BNE	loc_A125
		JSR	piranha		; Big piranha!

loc_A125:                               ; A120j
                PLA
                CMP     #$FF
                BNE     locret_A145
                LDA     $65
                BNE     loc_A131
                JMP     proc_9A1A
; ---------------------------------------------------------------------------

loc_A131:                               ; A12Cj
                LDA     #1
                STA     $F0
                LDA     #0
                STA     $74
                DEC     LIVES,X         ; Player 1 (or 2) lose life.
                LDA     #7
                CPX     $15
                BNE     loc_A143
                LDA     #0

loc_A143:                               ; A13Fj
                STA     $69

locret_A145:                            ; A128j
                RTS
; ---------------------------------------------------------------------------
tbl_unk_A146:	
		.BYTE	4		; sub_A14Cr
		.BYTE	8

; The number of	lives (initial).
; Set by DIP switches.
tbl_LIVES:	
		.BYTE 2, 3, 4, 5	; A160r

; =============== S U B	R O U T	I N E =======================================


sub_A14C:				; E384p
		LDY	tbl_unk_A146,X
		TXA
		PHA
		LDX	#3
		LDA	#0

loc_A155:				; A15Aj
		STA	$2D,Y
		DEY
		DEX
		BPL	loc_A155
		PLA
		TAX

loc_A15E:				; 932Ep 933Dp
		LDY	dip_LIVES
		LDA	tbl_LIVES,Y	; Init player(s) lives.
		STA	LIVES,X
		RTS
; End of function sub_A14C

; ---------------------------------------------------------------------------
tbl_A166:	
		.BYTE $02, $03,	$00	; A193r

; =============== S U B	R O U T	I N E =======================================


proc_A169:				; 9470p loc_B52Bp
		LDA	#$1D
		STA	$41E,X
		LDA	#$23 ; '#'
		STA	$426,X
		LDA	#6
		STA	$3F2,X
		LDA	#1
		STA	$3FA,X
		LDA	#$E
		STA	$40E,X
		LDA	#0
		STA	$518,X
		STA	$45A,X
		STA	$460,X
		STA	$44E,X
		STA	$446,X
		LDA	tbl_A166,Y
		STA	$4A2,X
		LDA	#4
		STA	$42E,X
		LDA	#$FF
		STA	$416,X
		LDA	$1C,X
		AND	#1
		CLC
		ADC	#1
		STA	$3C1,X
		LDA	#0
		STA	$3EA,X
		LDA	#1
		STA	$3E2,X
		RTS
; End of function proc_A169

; ---------------------------------------------------------------------------
tbl_A1B8:	
		.BYTE $20		; loc_A21Er
		.BYTE $D0 ; Ð
tbl_A1BA:	
		.BYTE	1		; A218r A221r
		.BYTE	2
tbl_A1BC:	
		.BYTE $48 ; H		; A215r
		.BYTE $A8 ; ¨
tbl_A1BE:	
		.BYTE $80 ; €		; A22Dr
		.BYTE $98 ; ˜

; =============== S U B	R O U T	I N E =======================================


sub_A1C0:				; 9842p 9D78p	...
		LDA	$6C
					
		EOR	#1
		STA	$3DA,X
		LDY	#$FF
		LDA	$6C
		BNE	loc_A1D4
		LDA	$3E2,X
		CMP	#1
		BEQ	loc_A1DE

loc_A1D4:				; A1CBj
		LDA	#2
		STA	$3E2,X
		STA	$42E,X
		LDY	#$FE ; 'þ'

loc_A1DE:				; A1D2j
		TYA
		STA	$416,X
		JSR	sub_A243
		STA	$510,X
		STA	$7D2
		LDA	#$FF
		STA	$50
		LDA	#9
		STA	$84,X
		LDA	#$20 ; ' '
		STA	$3D2,X
		LDA	#$48 ; 'H'
		STA	$3F2,X
		LDA	#8
		STA	$3FA,X
		LDA	#8
		STA	$41E,X
		LDA	#$36 ; '6'
		STA	$426,X
		LDA	#$10
		STA	$40E,X
		LDA	$6C
		BEQ	loc_A21E
		LDA	tbl_A1BC,X
		LDY	tbl_A1BA,X
		JMP	loc_A224
; ---------------------------------------------------------------------------

loc_A21E:				; A213j
		LDA	tbl_A1B8,X
		LDY	tbl_A1BA,X

loc_A224:				; A21Bj
		STA	$350,X
		TYA
		STA	$3C1,X
		LDY	$6C
		LDA	tbl_A1BE,Y
		STA	$37E,X
		LDA	#1
		STA	$36B,X
		TXA
		STA	$4A2,X
		LDA	#$F0 ; 'ð'
		STA	$88,X
		STA	$8A,X
		RTS
; End of function sub_A1C0


; =============== S U B	R O U T	I N E =======================================

sub_A243:				; loc_93F8p A1E2p
		LDA	#1
		STA	$3CA,X
		LDA	#0
		STA	$363,X
		STA	$391,X
		STA	$3A9,X
		STA	$3B5,X
		STA	$3EA,X
		STA	$406,X
		RTS
; End of function sub_A243


; =============== S U B	R O U T	I N E =======================================


sub_A25D:				; 9907p 993Bp
		JSR	sub_B015
		LDX	#1

loc_A262:				; A270j
		LDA	$62,X
		BEQ	loc_A26F
		LDA	#0
		STA	$80,X
		STA	$82,X
		JSR	process_player

loc_A26F:				; A264j
		DEX
		BPL	loc_A262
		RTS
; End of function sub_A25D


; =============== S U B	R O U T	I N E =======================================


sub_A273:				; 947Dp loc_9904p ...
		LDA	$6C
		BNE	locret_A27A
		JSR	process_enemies

locret_A27A:				; A275j
		RTS
; End of function sub_A273


; =============== S U B	R O U T	I N E =======================================


sub_A27B:				; 9979p 9983p	...
		LDY	#0

loc_A27D:				; A285j
		CMP	#$A
		BCC	locret_A288
		SEC
		SBC	#$A
		INY
		JMP	loc_A27D
; ---------------------------------------------------------------------------

locret_A288:				; A27Fj
		RTS
; End of function sub_A27B

tbl_A2A0:	
		.BYTE	0		; A3D7r
		.BYTE $10
		.BYTE $20
tbl_A2A3:	
		.BYTE $25, $26,	$27, $28, $29, $2A, $2B, $2C, $2D, $2E,	$2F, $30, $31, $32, $33, $34
		.BYTE $2A, $2B,	$2C, $2D, $2E, $2F, $30, $31, $32, $33,	$34, $35, $36, $37, $38, $39
		.BYTE $2E, $2F,	$30, $31, $32, $33, $34, $35, $36, $37,	$38, $39, $3A, $3B, $3C, $3D
tbl_A2D3:	
		.BYTE	6		; A3C6r
		.BYTE  $F
		.BYTE $15
tbl_A2D6:	
		.BYTE	1		; A3CBr
		.BYTE	2
		.BYTE	4
tbl_A2D9:	
		.BYTE $30 ; 0		; A422r
		.BYTE $2C ; ,
		.BYTE $28 ; (
		.BYTE $24 ; $
		.BYTE $20
		.BYTE $1C
		.BYTE $18
		.BYTE $14
		.BYTE $10
		.BYTE  $C
		.BYTE  $A
		.BYTE	8
		.BYTE	6
		.BYTE	4
		.BYTE	2
		.BYTE	0
		.BYTE $28 ; (
		.BYTE $24 ; $
		.BYTE $20
		.BYTE $1C
		.BYTE $18
		.BYTE $14
		.BYTE $10
		.BYTE  $E
		.BYTE  $C
		.BYTE  $A
		.BYTE	8
		.BYTE	6
		.BYTE	4
		.BYTE	2
		.BYTE	1
		.BYTE	0
		.BYTE $20
		.BYTE $1C
		.BYTE $18
		.BYTE $17
		.BYTE $15
		.BYTE $13
		.BYTE $11
		.BYTE  $F
		.BYTE  $E
		.BYTE  $D
		.BYTE  $C
		.BYTE  $B
		.BYTE  $A
		.BYTE	8
		.BYTE	6
		.BYTE	4
tbl_A309:	
		.BYTE $80, $84,	$88, $8C, $90, $94, $98, $9C, $A0, $A5,	$AA, $B0, $B5, $BA, $C0, $C5
		.BYTE $60, $65,	$6A, $70, $74, $78, $7C, $80, $85, $8A,	$90, $95, $9A, $A0, $A5, $AA
		.BYTE $4A, $50,	$55, $5A, $60, $65, $6A, $70, $75, $7A,	$80, $85, $8A, $90, $95, $9A
tbl_A339:	
		.BYTE $AF, $A7,	$9F, $97, $8F, $87, $7F, $77, $6F, $67,	$5F, $57, $4F, $47, $3F, $37
tbl_A349:	
		.BYTE $20, $20,	$20, $20, $1C, $1C, $1C, $1C, $18, $18,	$18, $18, $14, $14, $14, $14
tbl_A359:	
		.BYTE $2C, $2C,	$2A, $2A, $28, $28, $26, $26, $24, $24,	$22, $22, $20, $20, $1E, $1E
tbl_A369:	
		.BYTE $0C, $0C,	$0C, $0C, $0A, $0A, $0A, $0A, $08, $08,	$08, $08, $06, $06, $06, $06

; =============== S U B	R O U T	I N E =======================================


sub_A379:				; sub_A391p A3D0p
		LDA	$6D
		LDY	$69
		CPY	#$A
		BEQ	loc_A388
		LDY	dip_ENEMY_REGEN
		BEQ	loc_A388
		CLC
		ADC	#8

loc_A388:				; A37Fj A383j
		CMP	#$20
		BCS	loc_A38E
		LSR	A
		RTS
; ---------------------------------------------------------------------------

loc_A38E:				; A38Aj
		LDA	#$F
		RTS
; End of function sub_A379


; =============== S U B	R O U T	I N E =======================================


sub_A391:				; 9467p
		JSR	sub_A379
		TAY
		LDA	tbl_A339,Y
		STA	$4F5
		LDA	tbl_A349,Y
		STA	$4F6
		LDA	tbl_A359,Y
		STA	$4F7
		LDA	tbl_A369,Y
		STA	$4F8
		RTS
; End of function sub_A391


; =============== S U B	R O U T	I N E =======================================

sub_A3AE:				; A5D6p
		LDA	$42E,X
		CMP	#3
		BNE	loc_A3B8
		INC	$42E,X

loc_A3B8:				; A3B3j
		LDA	$518,X
		BEQ	loc_A3C1
		DEC	$518,X
		RTS
; ---------------------------------------------------------------------------

loc_A3C1:				; A3BBj
		STA	$F
		LDY	$454,X
		LDA	tbl_A2D3,Y
		STA	$D
		LDA	tbl_A2D6,Y
		STA	$E
		JSR	sub_A379
		LDY	$454,X
		CLC
		ADC	tbl_A2A0,Y
		TAY
		LDA	$426,X
		CMP	tbl_A2A3,Y
		BEQ	loc_A3F2
		LDA	$460,X
		AND	#1
		BEQ	loc_A3F0
		DEC	$41E,X
		INC	$426,X

loc_A3F0:				; A3E8j
		INC	$F

loc_A3F2:				; A3E1j
		LDA	$3FA,X
		CMP	$E
		BEQ	loc_A40A
		LDA	$460,X
		CMP	#6
		BCC	loc_A408
		INC	$3FA,X
		LDA	#0
		STA	$460,X

loc_A408:				; A3FEj
		INC	$F

loc_A40A:				; A3F7j
		INC	$460,X
		LDA	$3F2,X
		CMP	$D
		BCS	loc_A419
		INC	$3F2,X
		INC	$F

loc_A419:				; A412j
		LDA	$F
		BEQ	loc_A436
		LDA	$56E,X
		BEQ	loc_A425
		LDA	tbl_A2D9,Y

loc_A425:				; A420j
		STA	$518,X
		LDA	$3C1,X
		LSR	A
		LDA	#0
		BCS	loc_A432
		LDA	#$FF

loc_A432:				; A42Ej
		STA	$363,X
		RTS
; ---------------------------------------------------------------------------

loc_A436:				; A41Bj
		LDA	tbl_A309,Y
		STA	$45A,X
		LDA	#0
		STA	$460,X
		RTS
; End of function sub_A3AE

; ---------------------------------------------------------------------------
tbl_A442:	
		.BYTE $F8, $F0,	$E4, $D8, $C4, $B8, $AC, $AC, $AC, $AC,	$AC, $AC, $AC, $AC, $AC, $AC
tbl_A452:	
		.BYTE $B8, $C0,	$D0, $C0, $E0, $F0, $FF, 8, $10, $18, $20, $20,	$20, $20, $20, $20
tbl_A462:	
		.BYTE $40, $40,	$40, $40, $40, $40, $38, $3C, $40, $44,	$46, $48, $4A, $4C, $4E, $50

; =============== S U B	R O U T	I N E =======================================


sub_A472:				; 9A94p
		TYA
		PHA
		LDY	$72
		CPY	#$10
		BCC	loc_A47C
		LDY	#$F

loc_A47C:				; A478j
		LDA	tbl_A442,Y
		STA	$5F0,X
		LDA	tbl_A452,Y
		STA	$3B7,X
		LDA	#0
		CPY	#7
		BCC	loc_A490
		LDA	#1

loc_A490:				; A48Cj
		STA	$3AB,X
		LDA	tbl_A462,Y
		STA	$4EC
		LDA	#$14
		STA	$5FE
		PLA
		TAY
		RTS
; End of function sub_A472

; =============== S U B	R O U T	I N E =======================================


sub_A4A8:				; loc_8129p
		LDA	#0
		STA	$60
		LDA	$69
		JSR	execute_procedure
; ---------------------------------------------------------------------------
		.WORD proc_todo_DE0D ; 0
		.WORD proc_unk_DE65 ; 1
		.WORD proc_unk_DF07 ; 2
		.WORD proc_unk_E02A ;3
		.WORD proc_unk_E08F   ; id 4 "please wait" info screen related
		.WORD proc_unk_E21F ;5
		.WORD proc_unk_E2E2 ;6
		.WORD proc_unk_E345 ;7
		.WORD proc_unk_E3B6 ;8 
		.WORD proc_unk_E404 ;9
		.WORD proc_E30C ;A
		.WORD proc_unk_A4CD ; B
		.WORD proc_unk_DFA7 ; C
		.WORD proc_E04C ; D
		.WORD proc_process_dip_menu ; E
; ---------------------------------------------------------------------------


proc_unk_A4CD:				; E31Ep
					; A4C7o
		LDA	#1
		STA	$60
		JSR	sub_B015
		LDX	#1

loc_A4D6:				; A4DEj
		LDA	$62,X
		BEQ	loc_A4DD
		JSR	process_player

loc_A4DD:				; A4D8j
		DEX
		BPL	loc_A4D6
		JSR	sub_C8F0
		LDA	$6C
		BNE	loc_A510
		JSR	sub_CF55
		JSR	process_enemies
		JSR	sub_E55C
		JSR	sub_CFEA
		JSR	sub_AE00
		LDA	$68
		BEQ	loc_A513
		TXA
		PHA
		JSR	sub_A625
		PLA
		TAX
		JSR	piranha		; Big piranha!
		JSR	sub_BAEE
		JSR	sub_C0EB
		JSR	sub_AE49
		JMP	loc_A513
; ---------------------------------------------------------------------------

loc_A510:				; A4E5j
		JSR	sub_A6F8

loc_A513:				; A4F8j A50Dj
		JSR	sub_D190
		LDA	$74
		CMP	#$A
		BNE	locret_A51F
		JSR	init_oam_buffer

locret_A51F:				; A51Aj
		RTS
; End of function sub_A4A8


; =============== S U B	R O U T	I N E =======================================

process_player:
		TXA
		AND	#1
		EOR	$15
		STA	$7F
		LDA	$4A2,X
		AND	#$F
		STA	$4A2,X
		LDY	$84,X
		LDA	$80,X
		AND	#$C0 ; Check A/B
		BEQ	loc_A55F
		LDA	$80,X
		AND	#3
		ORA	#$40 ; '@'
		STA	$80,X
		AND	#$C0 ; 'À'
		EOR	$86,X
		BEQ	loc_A561
		STA	$86,X
		LDA	#0
		STA	$84,X
		STA	$80,X
		LDA	#6
		STA	$446,X
		STA	$44E,X
		LDA	#8
		STA	$9C,X
		LDA	#$10
		STA	$7C6,X
		JMP	loc_A56D
; ---------------------------------------------------------------------------

loc_A55F:				; A535j
		STA	$86,X

loc_A561:				; A543j
		CPY	#8
		BPL	loc_A56D ; branch if Y >= 8
		LDA	#$80 ; '€'
		ORA	$80,X
		STA	$80,X
		INC	$84,X

loc_A56D:				; A55Cj A563j
		JSR	sub_AE97
		LDA	$3DA,X
		BNE	loc_A5A3

loc_A575:				; B19Ep
		LDA	$6C
		BEQ	loc_A57F
		JSR	sub_C3CE
		JMP	loc_A582
; ---------------------------------------------------------------------------

loc_A57F:				; A577j
		JSR	sub_C370

loc_A582:				; A57Cj
		JSR	player_dpad
		LDY	$3CA,X
		BNE	loc_A593
		JSR	sub_A8BF
		JSR	sub_AA33
		JMP	loc_A596
; ---------------------------------------------------------------------------

loc_A593:				; A588j
		JSR	sub_A8E9

loc_A596:				; A590j
		CPX	$15
		BNE	loc_A5A0
		JSR	sub_CE4A
		JSR	sub_CDB8

loc_A5A0:				; A598j
		JSR	sub_D2EF

loc_A5A3:				; A573j
		JSR	sub_CD35
		RTS
; End of function process_player


; =============== S U B	R O U T	I N E =======================================

process_enemies:			; A277p A4EAp
		LDX	$76
		INX

loc_A5AA:				; A622j
		TXA
		AND	#1
		EOR	$15
		STA	$7F
		LDA	$4A2,X
		AND	#$F
		STA	$4A2,X
		TXA
		PHA
		JSR	sub_AE97
		PLA
		TAX
		LDY	$3DA,X
		BNE	loc_A610
		LDA	$42E,X
		CMP	#5
		BEQ	loc_A5D1
		LDA	#$10
		STA	$7C6,X

loc_A5D1:				; A5CAj
		LDA	$45A,X
		BNE	loc_A5DF
		JSR	sub_A3AE
		JSR	sub_AB4D
		JMP	loc_A5E2
; ---------------------------------------------------------------------------

loc_A5DF:				; A5D4j
		JSR	sub_AB58

loc_A5E2:				; A5DCj
		JSR	sub_AC3E
		JSR	sub_ADB5
		JSR	sub_CF9F
		JSR	sub_C370
		JSR	sub_CA55
		JSR	player_buttons
		JSR	sub_A8D4
		JSR	sub_AA4E
		LDA	$4BE,X
		BNE	loc_A60A
		LDA	$F1
		ORA	#$10
		STA	$F1
		LDA	#$10
		STA	$4BE,X

loc_A60A:				; A5FDj
		DEC	$4BE,X
		JSR	sub_D2EF

loc_A610:				; A5C3j
		LDY	$3DA,X
		INY
		BEQ	loc_A61F
		LDY	$5E4,X
		INY
		BNE	loc_A61F
		JSR	sub_CD35

loc_A61F:				; A614j A61Aj
		DEX
		CPX	#1
		BNE	loc_A5AA
		RTS
; End of function process_enemies


; =============== S U B	R O U T	I N E =======================================


sub_A625:				; A4FCp

; FUNCTION CHUNK AT A67A SIZE 00000052 BYTES

		LDA	$7D4
		BEQ	loc_A67A
		LDA	$7D3
		LSR	A
		LDA	$362
		LDY	#$FD ; 'ý'
		BCC	loc_A63E
		CMP	#$F1 ; 'ñ'
		BCS	sub_A658
		LDY	#3
		JMP	loc_A642
; ---------------------------------------------------------------------------

loc_A63E:				; A633j
		CMP	#3
		BCC	sub_A658

loc_A642:				; A63Bj
		STY	0
		LDA	$362
		CLC
		ADC	0
		STA	$362
		JSR	sub_CBC8
		JSR	sub_D319
		RTS
; End of function sub_A625

; ---------------------------------------------------------------------------
tbl_A654:	
		.BYTE $FF, $FF,	$FF, $FF

; =============== S U B	R O U T	I N E =======================================

;
sub_A658:				; A637j A640j	...
		LDA	#0
		STA	$7D4
		LDA	#$F8 ; 'ø'
		STA	$2D8
		STA	$2DC
		LDX	$7D2
		CPX	#4
		BCC	loc_A66E
		LDX	#4
; Weird code, weird table
loc_A66E:				; A66Aj
		LDA	tbl_A654,X
		STA	$50
		RTS
; End of function sub_A658

; ---------------------------------------------------------------------------
tbl_A674:	
		.BYTE $F1
		.BYTE	0
tbl_A676:	
		.BYTE $BF
		.BYTE $C4
tbl_A678:	
		.BYTE $C4
		.BYTE $BF
; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_A625

loc_A67A:				; A628j
		LDA	$50
		BNE	locret_A6CB
		LDA	$15
		LDY	$65
		BEQ	loc_A688
		LDA	$1C
		AND	#1

loc_A688:				; A682j
		TAX
		LDA	$3DA,X
		BNE	locret_A6CB
		LDA	$36B,X
		STA	$37D
		LDA	$37E,X
		STA	$390
		LDA	$350,X
		ASL	A
		LDA	#0
		ROL	A
		TAX
		LDY	#$43 ; 'C'
		LDA	tbl_A674,X
		STA	$362
		TXA
		BNE	loc_A6B1
		LDY	#3
		LDA	#2

loc_A6B1:				; A6ABj
		STA	$7D3
		STY	$2DA
		STY	$2DE
		LDA	tbl_A676,X
		STA	$2D9
		LDA	tbl_A678,X
		STA	$2DD
		LDA	#1
		STA	$7D4

locret_A6CB:				; A67Cj A68Cj
		RTS
; END OF FUNCTION CHUNK	FOR sub_A625

; =============== S U B	R O U T	I N E =======================================


sub_A6CC:				; loc_B713p
		TXA
		PHA
		LDY	$5E4,X
		INY
		BEQ	loc_A6F2
		DEY
		BNE	loc_A6F2
		LDA	$36B,X
		BEQ	loc_A6E3
		LDA	$37E,X
		CMP	#$98
		BCS	loc_A6E6

loc_A6E3:				; A6DAj
		JSR	sub_AD63

loc_A6E6:				; A6E1j
		JSR	sub_A973
		JSR	sub_AAFF
		JSR	sub_CB69
		STA	$5E4,X

loc_A6F2:				; A6D2j A6D5j
		JSR	sub_D370
		PLA
		TAX
		RTS
; End of function sub_A6CC


; =============== S U B	R O U T	I N E =======================================

sub_A6F8:				; loc_A510p
		LDX	#2
		LDA	#$FF

loc_A6FC:				; A75Aj
		PHA
		LDY	$5E4,X
		BEQ	loc_A70B
		INY
		BNE	loc_A750
		JSR	sub_ACBC
		JMP	loc_A753
; ---------------------------------------------------------------------------

loc_A70B:				; A700j
		CPX	$4E7
		BNE	loc_A722
		LDY	$4E6
		INY
		BEQ	loc_A71C
		JSR	sub_AB25
		JMP	loc_A750
; ---------------------------------------------------------------------------

loc_A71C:				; A714j
		JSR	sub_AAFF
		JMP	loc_A750
; ---------------------------------------------------------------------------

loc_A722:				; A70Ej
		CPX	$4E6
		BNE	loc_A72D
		LDY	$4E7
		INY
		BEQ	loc_A750

loc_A72D:				; A725j
		JSR	sub_AAFF
		LDA	$36B,X
		BEQ	loc_A73D
		LDA	$37E,X
		CMP	$501,X
		BCS	loc_A74C

loc_A73D:				; A733j
		JSR	sub_AD63
		JSR	sub_A973
		JSR	sub_CB69
		STA	$5E4,X
		JMP	loc_A750
; ---------------------------------------------------------------------------

loc_A74C:				; A73Bj
		LDA	#3
		STA	$4F

loc_A750:				; A703j A719j	...
		JSR	sub_D467

loc_A753:				; A708j
		PLA
		AND	$5E4,X
		INX
		CPX	#$C
		BNE	loc_A6FC
		CMP	#$FF
		BNE	locret_A774
		LDA	$5FE
		BNE	locret_A774
		LDA	$4F
		BNE	locret_A774
		LDA	#$FF
		STA	$4E6
		STA	$4E7
		JSR	sub_AE32

locret_A774:				; A75Ej A763j	...
		RTS
; End of function sub_A6F8


; =============== S U B	R O U T	I N E =======================================


player_dpad:				; loc_A582p
		LDA	$80,X
		CMP	#$80 ; '€'
		BCC	loc_A780
		PHA
		JSR	player_buttons
		PLA

loc_A780:				; A779j
		LDY	$3CA,X
		BNE	loc_A7DE
		CMP	#$80 ; '€'
		AND	#3
		BCC	loc_A7C7
		STA	$82,X
		CMP	#0
		BEQ	loc_A7AE
		CMP	$3C1,X
		BEQ	loc_A79A
		LDY	#0
		STY	$3E,X

loc_A79A:				; A794j
		STA	$3C1,X
		LDY	$3E2,X
		DEY
		BEQ	loc_A7A8
		LDA	#$3A ; ':'
		STA	$3F2,X

loc_A7A8:				; A7A1j
		LDA	#0
		STA	$4D8,X
		RTS
; ---------------------------------------------------------------------------

loc_A7AE:				; A78Fj
		LDA	$3D2,X
		CMP	#$20 ; ' '
		BEQ	loc_A7BB
		LDA	#0
		ROL	A
		STA	$3C1,X

loc_A7BB:				; A7B3j
		LDY	$3E2,X
		DEY
		BEQ	locret_A7C6
		LDA	#$48 ; 'H'
		STA	$3F2,X

locret_A7C6:				; A7BFj
		RTS
; ---------------------------------------------------------------------------

loc_A7C7:				; A789j
		CMP	#0
		BEQ	loc_A7D7
		CMP	$3C1,X
		BEQ	loc_A7D4
		LDY	#0
		STY	$3E,X

loc_A7D4:				; A7CEj
		STA	$3C1,X

loc_A7D7:				; A7C9j
		LDA	#8
		STA	$4D8,X
		LDA	#0

loc_A7DE:				; A783j
		AND	#3
		LDY	$3D2,X
		CPY	#$20 ; ' '
		BNE	loc_A7F5
		LDY	#0
		STY	$82,X
		CMP	#0
		BEQ	locret_A803
		STA	$3C1,X
		JMP	loc_A801
; ---------------------------------------------------------------------------

loc_A7F5:				; A7E5j
		CMP	#0
		BNE	loc_A801
		LDA	#1
		CPY	#$20 ; ' '
		BCC	loc_A801
		LDA	#2

loc_A801:				; A7F2j A7F7j	...
		STA	$82,X

locret_A803:				; A7EDj
		RTS
; End of function player_dpad


; =============== S U B	R O U T	I N E =======================================


player_buttons:				; A5F1p A77Cp
		LDA	$3CA,X
		BEQ	locret_A83C
		LDA	$37E,X
		SEC
		SBC	#4
		STA	$37E,X
; End of function player_buttons


; =============== S U B	R O U T	I N E =======================================


sub_A812:				; C434p
		LDA	#0
		STA	$3CA,X
		STA	$406,X
		STA	$3A9,X
		STA	$3B5,X
		STA	$4AE,X
		LDA	#$F
		STA	$40E,X
		CPX	#2
		BCS	locret_A83C
		LDA	#$F0 ; 'ð'
		STA	$88,X
		STA	$8A,X
		LDA	#1
		STA	$41E,X
		LDA	#$3F ; '?'
		STA	$426,X

locret_A83C:				; A807j A82Aj
		RTS
; End of function sub_A812

; ---------------------------------------------------------------------------
byte_A83D:	
		.BYTE $FE, $FE,	$FE, $FE, $FE, $FE, $FE, $FE, $FE, $FE,	$FE, $FE, $FE, $FE, $FE, $FE
		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
		.BYTE 0, 0, 0, 0, 0, 0,	0, 0, 0, 0, 0, 0, 0, 0,	0, 0
		.BYTE 1, 1, 1, 1, 1, 1,	1, 1, 1, 1, 1, 1, 1, 1,	1, 1
		.BYTE 2

byte_A87E:	
		.BYTE 0, $10, $20, $30,	$40, $50, $60, $70, $80, $90, $A0, $B0,	$C0, $D0, $E0, $F0
		.BYTE 0, $10, $20, $30,	$40, $50, $60, $70, $80, $90, $A0, $B0,	$C0, $D0, $E0, $F0
		.BYTE 0, $10, $20, $30,	$40, $50, $60, $70, $80, $90, $A0, $B0,	$C0, $D0, $E0, $F0
		.BYTE 0, $10, $20, $30,	$40, $50, $60, $70, $80, $90, $A0, $B0,	$C0, $D0, $E0, $F0
		.BYTE 0

; =============== S U B	R O U T	I N E =======================================

sub_A8BF:				; A58Ap
		LDA	$406,X
		CMP	$4D8,X
		BCS	loc_A8CA
		JMP	loc_A959
; ---------------------------------------------------------------------------

loc_A8CA:				; A8C5j
		LDA	#0
		STA	$406,X
		LDA	$82,X
		JMP	loc_A92E
; End of function sub_A8BF


; =============== S U B	R O U T	I N E =======================================


sub_A8D4:				; A5F4p
		LDA	$406,X
		CMP	#2
		BCS	loc_A8DE
		JMP	loc_A959
; ---------------------------------------------------------------------------

loc_A8DE:				; A8D9j
		LDA	#0
		STA	$406,X
		LDA	$3C1,X
		JMP	loc_A92E
; End of function sub_A8D4


; =============== S U B	R O U T	I N E =======================================

sub_A8E9:				; loc_A593p
		LDY	$8C,X
		INY
		BNE	loc_A91A
		STX	0
		TXA
		EOR	#1
		TAX
		LDA	$350,X
		LDY	$8C,X
		LDX	0
		DEY
		BNE	loc_A904
		CLC
		ADC	#$E
		JMP	loc_A907
; ---------------------------------------------------------------------------

loc_A904:				; A8FCj
		SEC
		SBC	#$E

loc_A907:				; A901j
		STA	$350,X
		LDA	#3
		STA	$4E0,X
		TXA
		EOR	#1
		TAY
		LDA	$3C1,Y
		STA	$3C1,X
		RTS
; ---------------------------------------------------------------------------

loc_A91A:				; A8ECj
		LDA	$8C
		ORA	$8D
		CMP	#3
		BNE	loc_A92C
		LDA	$8C,X
		LDY	#2
		LSR	A
		BCS	loc_A92A
		DEY

loc_A92A:				; A927j
		STY	$82,X

loc_A92C:				; A920j
		LDA	$82,X

loc_A92E:				; A8D1j A8E6j
		LSR	A
		BCS	loc_A948
		LSR	A
		BCC	loc_A959
		DEC	$3D2,X
		LDA	$3D2,X
		CMP	$41E,X
		BCS	loc_A959
		LDA	$41E,X
		STA	$3D2,X
		JMP	loc_A959
; ---------------------------------------------------------------------------

loc_A948:				; A92Fj
		INC	$3D2,X
		LDA	$3D2,X
		CMP	$426,X
		BCC	loc_A959
		LDA	$426,X
		STA	$3D2,X

loc_A959:				; A8C7j A8DBj	...
		LDY	$3D2,X
		LDA	$363,X
		CLC
		ADC	byte_A87E,Y
		STA	$363,X
		LDA	$350,X
		ADC	byte_A83D,Y
		STA	$350,X
		INC	$406,X
		RTS
; End of function sub_A8E9


; =============== S U B	R O U T	I N E =======================================


sub_A973:				; loc_A6E6p A740p
		LDA	$350,X
		CLC
		ADC	$5DA,X
		STA	$350,X
		CMP	$5D0,X
		BCS	loc_A992
		LDA	$5A8,X
		BEQ	loc_A98D
		LDA	$59E,X
		LSR	A
		BCC	loc_A99D

loc_A98D:				; A985j
		LDA	#9
		JMP	loc_A9A5
; ---------------------------------------------------------------------------

loc_A992:				; A980j
		LDA	$5A8,X
		BEQ	loc_A9A3
		LDA	$59E,X
		LSR	A
		BCC	loc_A9A3

loc_A99D:				; A98Bj
		DEC	$5A8,X
		JMP	loc_A9AB
; ---------------------------------------------------------------------------

loc_A9A3:				; A995j A99Bj
		LDA	#$A

loc_A9A5:				; A98Fj
		STA	$59E,X
		INC	$5A8,X

loc_A9AB:				; A9A0j
		LDY	#$FF
		LDA	$5EE,X
		BEQ	locret_A9DD
		LDA	$39D,X
		CLC
		ADC	$5A8,X
		BCC	loc_A9C1

loc_A9BB:				; A9BFj
		INY
		SBC	$5EE,X
		BCS	loc_A9BB

loc_A9C1:				; A9B9j
		SEC

loc_A9C2:				; A9C6j
		INY
		SBC	$5EE,X
		BCS	loc_A9C2
		ADC	$5EE,X
		STA	$39D,X
		LDA	$59E,X
		LSR	A
		TYA
		BCS	loc_A9DA
		EOR	#$FF
		CLC
		ADC	#1

loc_A9DA:				; A9D3j
		STA	$5DA,X

locret_A9DD:				; A9B0j
					; AAC5r
		RTS
; End of function sub_A973

; ---------------------------------------------------------------------------

; ---- unused ?
		.BYTE	0
		.BYTE	0
		.BYTE $10
		.BYTE $20
		.BYTE $30 ; 0
; ----


tbl_A9E3:	
		.BYTE $FF, $FF ,0 ,0 ,0, $FF, $FF ,0
		.BYTE 0, 0, $FF ,$FF, 0, 0, 0 ,0
		.BYTE $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
		.BYTE $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
		.BYTE $FF, $FF, 0, 0, 0, 0, 0, 0
		.BYTE 0, 0, 0, 0, 0, 0, $FF, $FF
		.BYTE $FF, $FF, 0, 0, 0, 0, 0, 0
		.BYTE 0, 0, $FF, 0, 0, 0, 0, $FF
		.BYTE $FF, 0, $FF, 0, $FF, 0, $FF, 0
		.BYTE $FF, 0, $FF, 0, $FF, 0, $FF, 0

; =============== S U B	R O U T	I N E =======================================

sub_AA33:				; A58Dp B753p
		LDA	$84,X
		BNE	loc_AA49
		INC	$4AE,X
		LDA	$4AE,X
		CMP	#5
		BCC	loc_AA49
		LDA	#0
		STA	$4AE,X
		JMP	loc_AA62
; ---------------------------------------------------------------------------

loc_AA49:				; AA35j AA3Fj
		LDA	$80,X
		JMP	loc_AA62
; End of function sub_AA33


; =============== S U B	R O U T	I N E =======================================


sub_AA4E:				; A5F7p
		INC	$4AE,X
		LDA	$4AE,X
		CMP	#5
		BCC	loc_AA60
		LDA	#0
		STA	$4AE,X
		JMP	loc_AA62
; ---------------------------------------------------------------------------

loc_AA60:				; AA56j
		LDA	#$80

loc_AA62:				; AA46j AA4Bj	...
		PHA
		LDA	$391,X
		CLC
		ADC	$3B5,X
		STA	$391,X
		LDA	$37E,X
		ADC	$3A9,X
		STA	$37E,X
		LDA	$36B,X
		LDY	$3A9,X
		BPL	loc_AA83
		SBC	#0
		JMP	loc_AA85
; ---------------------------------------------------------------------------

loc_AA83:				; AA7Cj
		ADC	#0

loc_AA85:				; AA80j
		CMP	#1
		LDA	#0
		ROL	A
		STA	$36B,X
		LDA	$3B5,X
		CLC
		ADC	$3FA,X
		STA	$3B5,X
		LDA	$3A9,X
		ADC	#0
		STA	$3A9,X
		CMP	#3
		BCC	loc_AAB1
		CMP	#$F0 ; 'ð'
		BCS	loc_AAB1
		LDA	#3
		STA	$3A9,X
		LDA	#0
		STA	$3B5,X

loc_AAB1:				; AAA1j AAA5j
		PLA
		ASL	A
		BCC	loc_AAF9
		DEC	$40E,X
		LDA	$40E,X
		BPL	loc_AAC2
		LDA	#$F
		STA	$40E,X

loc_AAC2:				; AABBj
		LDY	$42E,X
		LDA	locret_A9DD,Y	; Reading code instead of data
		CLC
		ADC	$40E,X
		TAY
		LDA	tbl_A9E3,Y
		BEQ	locret_AAF8
		LDA	$3B5,X
		SEC
		SBC	$3F2,X
		STA	$3B5,X
		LDA	$3A9,X
		SBC	#0
		STA	$3A9,X
		CMP	$416,X
		BCS	locret_AAF8
		CMP	#$20 ; ' '
		BCC	locret_AAF8
		LDA	$416,X
		STA	$3A9,X
		LDA	#0
		STA	$3B5,X

locret_AAF8:				; AAD0j AAE7j	...
		RTS
; ---------------------------------------------------------------------------

loc_AAF9:				; AAB3j
		LDA	#$F
		STA	$40E,X
		RTS
; End of function sub_AA4E


; =============== S U B	R O U T	I N E =======================================

sub_AAFF:				; A6E9p loc_A71Cp ...
		LDA	$391,X
		SEC
		SBC	$3B5,X
		STA	$391,X
		LDA	$37E,X
		SBC	$3A9,X
		STA	$37E,X
		LDA	$36B,X
		SBC	#0
		STA	$36B,X
		DEC	$5BC,X
		BPL	locret_AB24
		LDA	#4
		STA	$5BC,X

locret_AB24:				; AB1Dj
		RTS
; End of function sub_AAFF


; =============== S U B	R O U T	I N E =======================================


sub_AB25:				; A716p ACC1p
		LDY	$4E6
		LDA	$350,Y
		STA	$350,X
		LDA	$4F
		BEQ	loc_AB3B
		LDA	$350,Y
		CLC
		ADC	#4
		STA	$350,X

loc_AB3B:				; AB30j
		LDA	$37E,Y
		CLC
		ADC	#$F
		STA	$37E,X
		LDA	$36B,Y
		ADC	#0
		STA	$36B,X
		RTS
; End of function sub_AB25


; =============== S U B	R O U T	I N E =======================================


sub_AB4D:				; A5D9p
		LDY	#0
		LDA	$1C,X
		CMP	#$A0 ; ' '
		BCS	locret_AB57
		LDY	$1D,X

locret_AB57:				; AB53j
		RTS
; End of function sub_AB4D


; =============== S U B	R O U T	I N E =======================================


sub_AB58:				; loc_A5DFp
		LDA	$460,X
		BNE	loc_AB7F
		LDA	$1C,X
		CMP	#$D0 ; 'Ð'
		BNE	loc_AB70
		LDA	#2
		LDY	$1D,X
		CPY	#$80 ; '€'
		BCC	loc_AB70
		LDA	#1
		STA	$3C1,X

loc_AB70:				; AB61j AB69j
		LDA	$1F,X
		CMP	#$FE ; 'þ'
		BCS	loc_AB79
		LDY	$1E,X
		RTS
; ---------------------------------------------------------------------------

loc_AB79:				; AB74j
		LDA	$45A,X
		STA	$460,X

loc_AB7F:				; AB5Bj
		DEC	$460,X
		TXA
		AND	#1
		TAY
		LDA	$3DA,Y
		BEQ	loc_AB8F
		TYA
		EOR	#1
		TAY

loc_AB8F:				; AB89j
		LDA	$350,X
		CLC
		ADC	#8
		STA	0
		LDA	$350,Y
		STA	4
		CLC
		ADC	#8
		STA	1
		LDA	$37E,Y
		SEC
		SBC	$37E,X
		STA	3
		LDA	$36B,Y
		LDY	#$7F ; ''
		SBC	$36B,X
		BEQ	loc_ABD9
		CMP	#1
		BEQ	loc_ABF7
		LDA	3
		CMP	#$A8 ; '¨'
		BCC	locret_ABF9
		CMP	#$F0 ; 'ð'
		BCS	locret_ABF9
		JSR	sub_AC0A
		BCS	locret_ABF9
		LDA	$454,X
		CMP	#1
		BNE	loc_ABD6
		JSR	sub_ABFA
		EOR	#3
		STA	$3C1,X

loc_ABD6:				; ABCCj
		LDY	#$6F ; 'o'
		RTS
; ---------------------------------------------------------------------------

loc_ABD9:				; ABB2j
		LDA	3
		CMP	#$10
		BCC	locret_ABF9
		CMP	#$60 ; '`'
		BCS	loc_ABF7
		INY
		JSR	sub_AC0A
		BCC	loc_ABF7
		LDA	$454,X
		CMP	#1
		BNE	locret_ABF9
		JSR	sub_ABFA
		STA	$3C1,X
		RTS
; ---------------------------------------------------------------------------

loc_ABF7:				; ABB6j ABE1j	...
		LDY	#$6F ; 'o'

locret_ABF9:				; ABBCj ABC0j	...
		RTS
; End of function sub_AB58


; =============== S U B	R O U T	I N E =======================================


sub_ABFA:				; ABCEp ABF0p
		LDA	4
		SEC
		SBC	$350,X
		BPL	loc_AC05
		LDA	#2
		RTS
; ---------------------------------------------------------------------------

loc_AC05:				; AC00j
		LDA	#1
		RTS
; End of function sub_ABFA

; ---------------------------------------------------------------------------
byte_AC08:	.BYTE $30		; AC1Cr loc_AC38r
byte_AC09:	.BYTE $D1		; AC23r

; =============== S U B	R O U T	I N E =======================================


sub_AC0A:				; ABC2p ABE4p
		TYA
		PHA
		LDA	1
		CMP	0
		BCS	loc_AC1A
		PHA
		LDA	0
		STA	1
		PLA
		STA	0

loc_AC1A:				; AC10j
		LDA	1
		CMP	byte_AC08
		BCC	loc_AC2D
		LDA	0
		CMP	byte_AC09
		BCC	loc_AC2D
		SBC	1
		JMP	loc_AC38
; ---------------------------------------------------------------------------

loc_AC2D:				; AC1Fj AC26j
		SEC
		LDA	0
		SBC	1
		BCS	loc_AC38
		EOR	#$FF
		ADC	#1

loc_AC38:				; AC2Aj AC32j
		CMP	byte_AC08
		PLA
		TAY
		RTS
; End of function sub_AC0A


; =============== S U B	R O U T	I N E =======================================


sub_AC3E:				; loc_A5E2p
		CPY	#0
		BEQ	locret_AC55
		LDA	#5
		STA	$42E,X
		CPY	#$70 ; 'p'
		BCC	locret_AC55
		DEC	$42E,X
		CPY	#$80 ; '€'
		BCS	locret_AC55
		DEC	$42E,X

locret_AC55:				; AC40j AC49j	...
		RTS
; End of function sub_AC3E


; =============== S U B	R O U T	I N E =======================================


sub_AC56:				; loc_B70Cp
		LDA	$F3
		ORA	#1
		STA	$F3
		JSR	loc_AD49
		STA	$446,X
		STA	$44E,X
		STA	$3A9,X
		LDA	#$A0 ; ' '
		STA	$37E,X
		LDA	#$60 ; '`'
		STA	$3B5,X
		LDA	#$C0 ; 'À'
		STA	$5EE,X
		LDA	#$40 ; '@'
		STA	$4EC
		LDA	#1
		STA	$59E,X
		LDA	$350,X
		CMP	#$60 ; '`'
		BCC	loc_ACB0
		CMP	#$A0 ; ' '
		BCS	loc_ACB0
		LDY	$1C,X
		CMP	#$80 ; '€'
		BCC	loc_AC9D
		LDA	#$28 ; '('
		CPY	#$90 ; ''
		BCS	loc_ACA8
		LDA	#$D0 ; 'Ð'
		JMP	loc_ACA5
; ---------------------------------------------------------------------------

loc_AC9D:				; AC90j
		LDA	#$30 ; '0'
		CPY	#$60 ; '`'
		BCC	loc_ACA8
		LDA	#$D8 ; 'Ø'

loc_ACA5:				; AC9Aj
		INC	$59E,X

loc_ACA8:				; AC96j ACA1j
		CLC
		ADC	$350,X
		STA	$5D0,X
		RTS
; ---------------------------------------------------------------------------

loc_ACB0:				; AC86j AC8Aj
		JSR	sub_AD63
		RTS
; End of function sub_AC56

; ---------------------------------------------------------------------------
tbl_ACB4:	
		.BYTE $1C, $24,	$2C, $34 ; AD3Er
tbl_ACB8:	
		.BYTE $3C, $44,	$4C, $54 ; loc_AD44r

; =============== S U B	R O U T	I N E =======================================


sub_ACBC:				; A705p
		CPX	$4E7
		BNE	loc_ACD4
		JSR	sub_AB25
		LDA	#0
		STA	$5E4,X
		LDA	$501,Y
		STA	$501,X
		LDA	#3
		STA	$4F

locret_ACD3:				; ACD6j ACDBj
		RTS
; ---------------------------------------------------------------------------

loc_ACD4:				; ACBFj
		LDA	$40
		BNE	locret_ACD3
		LDA	$5FE
		BEQ	locret_ACD3
		CMP	#1
		BNE	loc_ACF6
		STX	$F
		LDY	#2

loc_ACE5:				; ACF3j
		LDA	$5E4,Y
		CMP	#$FF
		BNE	loc_ACF0
		CPY	$F
		BNE	loc_ACF6

loc_ACF0:				; ACEAj
		INY
		CPY	#$C
		BNE	loc_ACE5
		RTS
; ---------------------------------------------------------------------------

loc_ACF6:				; ACDFj ACEEj
		DEC	$5FE
		BNE	loc_AD01
		STX	$4E6
		STY	$4E7

loc_AD01:				; ACF9j
		LDA	$1C,X
		AND	#3
		TAY
		LDA	$4E9
		ASL	A
		BCC	loc_AD13
		LDA	$1D,X
		CMP	#$30 ; '0'
		BCS	loc_AD13
		INY

loc_AD13:				; AD0Aj AD10j
		LDA	$4FE,Y
		PHA
		CPX	$4E6
		BNE	loc_AD25
		LDA	#$90 ; ''
		STA	$37E,X
		PLA
		JMP	loc_AD29
; ---------------------------------------------------------------------------

loc_AD25:				; AD1Aj
		PLA
		STA	$37E,X

loc_AD29:				; AD22j
		SEC
		SBC	#$10
		STA	$501,X
		LDA	$4F9,Y
		STA	$350,X
		LDA	$1D
		AND	#3
		TAY
		LDA	$65
		BEQ	loc_AD44
		LDA	tbl_ACB4,Y
		JMP	loc_AD47
; ---------------------------------------------------------------------------

loc_AD44:				; AD3Cj
		LDA	tbl_ACB8,Y

loc_AD47:				; AD41j
		STA	$40

loc_AD49:				; AC5Cp
		LDA	#1
		STA	$36B,X
		LDA	#0
		STA	$5E4,X
		STA	$5DA,X
		STA	$39D,X
		STA	$5A8,X
		STA	$5BC,X
		STA	$391,X
		RTS
; End of function sub_ACBC


; =============== S U B	R O U T	I N E =======================================


sub_AD63:				; loc_A6E3p loc_A73Dp	...
		LDA	$5A8,X
		BNE	locret_ADB1
		LDA	$4EC
		STA	$E
		EOR	#$FF
		CLC
		ADC	#1
		STA	$F
		LDA	$1C,X
		AND	#$1C
		STA	0
		LDA	$1D,X
		LSR	A
		BCS	loc_AD98
		LDA	#9
		STA	$59E,X
		LDA	0
		ADC	$350,X
		BCS	loc_AD8F
		CMP	$F
		BCC	loc_ADAE

loc_AD8F:				; AD89j
		LDA	$350,X
		SBC	$4EC
		JMP	loc_ADAE
; ---------------------------------------------------------------------------

loc_AD98:				; AD7Dj
		LDA	#$A
		STA	$59E,X
		LDA	$350,X
		SBC	0
		BCC	loc_ADA8
		CMP	$E
		BCS	loc_ADAE

loc_ADA8:				; ADA2j
		LDA	$350,X
		ADC	$4EC

loc_ADAE:				; AD8Dj AD95j	...
		STA	$5D0,X

locret_ADB1:				; AD66j
		RTS
; End of function sub_AD63

; ---------------------------------------------------------------------------
tbl_ADB2:	.BYTE $68 ; h		; ADCBr
		.BYTE $70 ; p
		.BYTE $70

; =============== S U B	R O U T	I N E =======================================


sub_ADB5:				; A5E5p
		LDY	$454,X
		LDA	$36B,X
		BEQ	locret_ADF4
		LDA	$350,X
		CMP	#$38 ; '8'
		BCC	locret_ADF4
		CMP	#$C8 ; 'È'
		BCS	locret_ADF4
		LDA	$37E,X
		CMP	tbl_ADB2,Y
		BCC	locret_ADF4
		CMP	#$88 ; 'ˆ'
		BCS	loc_ADD8
		LDA	$1C,X
		CMP	#$F0 ; 'ð'

loc_ADD8:				; ADD2j
		LDA	#5
		BCS	loc_ADF1
		LDA	$3A9,X
		BMI	locret_ADF4
		BEQ	loc_ADEA
		LDA	$3B5,X
		LSR	A
		STA	$3B5,X

loc_ADEA:				; ADE1j
		LDA	#0
		STA	$4AE,X
		LDA	#3

loc_ADF1:				; ADDAj
		STA	$42E,X

locret_ADF4:				; ADBBj ADC2j	...
		RTS
; End of function sub_ADB5

; =============== S U B	R O U T	I N E =======================================

sub_AE00:				; A4F3p
		LDY	#0

loc_AE02:				; AE1Ej
		LDA	$3DC,Y
		CMP	#$FF
		BEQ	loc_AE1B
		AND	#$F0 ; 'ð'
		CMP	#$30 ; '0'
		BEQ	loc_AE1B
		CMP	#$60 ; '`'
		BEQ	loc_AE1B
		CMP	#$B0 ; '°'
		BEQ	loc_AE1B
		CMP	#$C0 ; 'À'
		BNE	locret_AE48

loc_AE1B:				; AE07j AE0Dj	...
		INY
		CPY	$76
		BNE	loc_AE02
		JSR	sub_BD4D
		JSR	sub_A658
		INC	$6F
		LDA	$6F
		CMP	#$18
		BCC	sub_AE32
		LDA	#9
		STA	$6F
; End of function sub_AE00


; =============== S U B	R O U T	I N E =======================================


sub_AE32:				; A771p AE2Cj
		LDA	#0
		STA	$68
		STA	$4B7
		LDX	#0
		LDA	$69
		CMP	#$B
		BNE	loc_AE46
		JSR	sub_AE89
		LDX	#$C

loc_AE46:				; AE3Fj
		STX	$74

locret_AE48:				; AE19j
		RTS
; End of function sub_AE32


; =============== S U B	R O U T	I N E =======================================


sub_AE49:				; A50Ap
		LDX	#0

loc_AE4B:				; AE73j
		LDA	$62,X
		BEQ	loc_AE70
		LDA	$3DA,X
		CMP	#$FF
		BEQ	loc_AE6C
		AND	#$F0 ; 'ð'
		CMP	#$10
		BEQ	loc_AE6C
		CMP	#$30 ; '0'
		BEQ	loc_AE6C
		CMP	#$80 ; '€'
		BEQ	loc_AE6C
		CMP	#$B0 ; '°'
		BEQ	loc_AE6C
		CMP	#$E0 ; 'à'
		BNE	loc_AE70

loc_AE6C:				; AE54j AE5Aj	...
		LDA	LIVES,X
		BEQ	loc_AE76

loc_AE70:				; AE4Dj AE6Aj
		INX
		CPX	#2
		BCC	loc_AE4B
		RTS
; ---------------------------------------------------------------------------

loc_AE76:				; AE6Ej
		STX	$61
		LDA	#$E
		STA	$74
		LDA	#0
		STA	$68
		STA	$4B7
		JSR	sub_BD4D
		JSR	sub_A658
; End of function sub_AE49


; =============== S U B	R O U T	I N E =======================================

sub_AE89:				; AE41p
		LDX	#5

loc_AE8B:				; AE94j
		LDA	$5E6,X
		BNE	loc_AE93
		INC	$5E6,X

loc_AE93:				; AE8Ej
		DEX
		BPL	loc_AE8B
		RTS
; End of function sub_AE89


; =============== S U B	R O U T	I N E =======================================


sub_AE97:				; 9C76p A0E6p	...

; FUNCTION CHUNK AT AEE5 SIZE 00000001 BYTES

		LDA	$3DA,X
		BNE	loc_AEB6
		LDA	$3E2,X
		BNE	loc_AEAD
		LDA	#$11
		CPX	#2
		BMI	loc_AEA9
		LDA	#$21 ; '!'

loc_AEA9:				; AEA5j AEB4j
		STA	$3DA,X

locret_AEAC:				; AEB0j
		RTS
; ---------------------------------------------------------------------------

loc_AEAD:				; AE9Fj
		LDA	$3EA,X
		BEQ	locret_AEAC
		LDA	#$41 ; 'A'
		BNE	loc_AEA9

loc_AEB6:				; AE9Aj
		CMP	#$FF
		BNE	loc_AEBE
		JSR	sub_AEE6
		RTS
; ---------------------------------------------------------------------------

loc_AEBE:				; AEB8j
		AND	#$F0 ; 'ð'
		CMP	#$E0 ; 'à'
		BCS	locret_AEE5
		LSR	A
		LSR	A
		LSR	A
		LSR	A
		JSR	execute_procedure
; End of function sub_AE97

; ---------------------------------------------------------------------------
		.WORD proc_AF2B
		.WORD proc_B06E
		.WORD proc_B1D3
		.WORD proc_B143
		.WORD proc_B183
		.WORD proc_B327
		.WORD proc_B3F6
		.WORD proc_B4D5
		.WORD proc_B5B6
		.WORD proc_B602
		.WORD proc_B717
		.WORD proc_B630
		.WORD proc_B6EC
; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_AE97

locret_AEE5:				; AEC2j
		RTS
; END OF FUNCTION CHUNK	FOR sub_AE97

; =============== S U B	R O U T	I N E =======================================


sub_AEE6:				; AEBAp
		CPX	#2
		BCS	locret_AF10	; There	are only two players in	this game,
					; so if	X is greater or	equal to two, let's get out of here.
		LDA	#2
		STA	$3E2,X
		STA	$42E,X
		DEC	LIVES,X
		JSR	sub_D056
		LDA	#1
		STA	$3DA,X
		JSR	sub_AFA1
		BCC	locret_AF10
		LDA	2
		STA	$350,X
		CPX	$15
		BNE	locret_AF10
		LDA	$F2
		ORA	#$80 ; '€'
		STA	$F2

locret_AF10:				; AEE8j AEFFj	...
		RTS
; End of function sub_AEE6

; ---------------------------------------------------------------------------
tbl_AF11:	
		.BYTE $00, $34,	$35, $03, $36, $37 ; loc_AF76t	AF7At
tbl_AF17:	
		.BYTE $CE, $CF,	$D0, $D1, $D2, $D3
tbl_AF1D:	
		.BYTE $0F, $40,	$35, $11, $41, $37
tbl_AF23:	
		.BYTE $D4, $D5,	$D0, $D6, $D7, $D3
tbl_AF29:	
		.BYTE	0		; AF81r
		.BYTE	6
; ---------------------------------------------------------------------------

proc_AF2B:				; AECBo
		LDA	$3DA,X
		AND	#$F
		CMP	#1
		BNE	loc_AF4B
		JSR	sub_A1C0
		LDA	#$20 ; ' '
		STA	$518,X
		STA	$520,X
		LDA	#1
		STA	$43E,X
		STA	$436,X
		INC	$3DA,X
		RTS
; ---------------------------------------------------------------------------

loc_AF4B:				; AF32j
		LDA	$80,X
		BNE	loc_AF92
		DEC	$518,X
		BNE	loc_AF5E
		LDA	#$20 ; ' '
		STA	$518,X
		DEC	$520,X
		BEQ	loc_AF92

loc_AF5E:				; AF52j
		DEC	$43E,X
		BNE	loc_AF76
		LDA	$436,X
		EOR	#1
		STA	$436,X
		BNE	loc_AF71
		LDA	#7
		BNE	loc_AF73

loc_AF71:				; AF6Bj
		LDA	#3

loc_AF73:				; AF6Fj
		STA	$43E,X

loc_AF76:				; AF61j
		LDA	#<tbl_AF11
		STA	$E
		LDA	#>tbl_AF11
		STA	$F
		LDY	$436,X
		LDA	tbl_AF29,Y
		LDY	$3E2,X
		CPY	#2
		BEQ	loc_AF8E
		CLC
		ADC	#$C

loc_AF8E:				; AF89j
		JSR	sub_B770
		RTS
; ---------------------------------------------------------------------------

loc_AF92:				; AF4Dj AF5Cj
		TXA
		STA	$4A2,X
		LDA	#0
		STA	$3DA,X
		JSR	sub_CF1B
		RTS
; ---------------------------------------------------------------------------
tbl_AF9F:	.BYTE $20		; sub_AFA1r AFE4r
		.BYTE $D0 ; Ð

; =============== S U B	R O U T	I N E =======================================


sub_AFA1:				; AEFCp
		LDA	tbl_AF9F,X
		STA	2
		LDA	#2
		STA	4
		STX	3

loc_AFAC:				; AFEBj
		LDY	#7

loc_AFAE:				; AFF1j
		CPY	3
		BEQ	loc_AFF0
		LDA	$3DA,Y
		CMP	#$FF
		BEQ	loc_AFF0
		LDA	$3CA,Y
		BEQ	loc_AFF0
		LDA	$36B,Y
		BEQ	loc_AFF0
		LDA	$37E,Y
		CMP	#$80 ; '€'
		BNE	loc_AFF0
		LDA	$350,Y
		SEC
		SBC	2
		BPL	loc_AFD7
		EOR	#$FF
		CLC
		ADC	#1

loc_AFD7:				; AFD0j
		CMP	#$10
		BCS	loc_AFF0
		DEC	4
		BEQ	loc_AFEE
		TXA
		PHA
		EOR	#1
		TAX
		LDA	tbl_AF9F,X
		STA	2
		PLA
		TAX
		JMP	loc_AFAC
; ---------------------------------------------------------------------------

loc_AFEE:				; AFDDj
		CLC
		RTS
; ---------------------------------------------------------------------------

loc_AFF0:				; AFB0j AFB7j	...
		DEY
		BPL	loc_AFAE
		SEC
		RTS
; End of function sub_AFA1

; ---------------------------------------------------------------------------
tbl_AFF5:	.BYTE  $A		; B037r
		.BYTE $10
tbl_AFF7:	.BYTE	0		; B025r
		.BYTE	5
tbl_AFF9:	.BYTE $23 ; #		; loc_B047r
		.BYTE $86 ; †
		.BYTE $42 ; B
		.BYTE $24 ; $
		.BYTE	0
		.BYTE $23 ; #
		.BYTE $98 ; ˜
		.BYTE $42 ; B
		.BYTE $24 ; $
		.BYTE	0
		.BYTE $23 ; #
		.BYTE $86 ; †
		.BYTE	2
		.BYTE $8F ; 
		.BYTE $90 ; 
		.BYTE	0
		.BYTE $23 ; #
		.BYTE $98 ; ˜
		.BYTE	2
		.BYTE $92 ; ’
		.BYTE $90 ; 
		.BYTE	0
		.BYTE $23 ; #
		.BYTE $98 ; ˜
		.BYTE	2
		.BYTE $8F ; 
		.BYTE $90 ; 
		.BYTE	0

; =============== S U B	R O U T	I N E =======================================


sub_B015:				; 9C24p sub_A25Dp ...
		LDA	$6C
		BNE	locret_B057
		LDX	$15
		DEC	$4E2,X
		BNE	locret_B057
		LDA	#$10
		STA	$4E2,X
		LDY	tbl_AFF7,X
		LDA	$4E4,X
		EOR	#1
		STA	$4E4,X
		BNE	loc_B044

loc_B032:				; 9895p 98A0p
		LDA	#$20 ; ' '
		STA	$4E2,X
		LDY	tbl_AFF5,X
		LDA	$65
		BNE	loc_B044
		LDA	$15
		BEQ	loc_B044
		LDY	#$16

loc_B044:				; B030j B03Cj	...
		LDX	$300

loc_B047:				; B051j
		LDA	tbl_AFF9,Y
		STA	$301,X
		INX
		INY
		CMP	#0
		BNE	loc_B047
		DEX
		STX	$300

locret_B057:				; B017j B01Ej
		RTS
; End of function sub_B015

; ---------------------------------------------------------------------------
tbl_B058:	.BYTE $FC, $48,	$42, $FC, $49, $43, $FC, $48, $44, $FC,	$49, $45, $FC, $4A, $46, $FC
		.BYTE $4B, $47
tbl_B06A:	.BYTE	0		; B13Cr
		.BYTE	6
		.BYTE  $C
		.BYTE	6
; ---------------------------------------------------------------------------

proc_B06E:				; AECDo
		LDA	$3DA,X
		AND	#$F
		CMP	#1
		BNE	loc_B08B
		LDA	#1
		STA	$7D,X
		STA	$9B
		INC	$3DA,X
		CPX	$15
		BNE	locret_B08A
		LDA	$F0
		ORA	#$20 ; ' '
		STA	$F0

locret_B08A:				; B082j
		RTS
; ---------------------------------------------------------------------------

loc_B08B:				; B075j
		CMP	#2
		BNE	loc_B0A2
		LDA	#8
		STA	$540,X
		LDA	#0
		STA	$436,X
		LDA	#$11

loc_B09B:				; B0E7j B337j	...
		STA	$43E,X
		INC	$3DA,X
		RTS
; ---------------------------------------------------------------------------

loc_B0A2:				; B08Dj
		CMP	#3
		BNE	loc_B0DC
		LDA	$37E,X
		SEC
		SBC	#1
		STA	$37E,X
		LDA	$36B,X
		SBC	#0
		STA	$36B,X
		CMP	#2
		BCC	loc_B0C3
		LDA	#0
		STA	$36B,X
		STA	$37E,X

loc_B0C3:				; B0B9j
		DEC	$43E,X
		BNE	loc_B0D5
		LDA	#3
		STA	$436,X
		LDA	#$A
		STA	$43E,X
		INC	$3DA,X

loc_B0D5:				; B0C6j
		JSR	sub_B12E
		JSR	sub_D266

locret_B0DB:				; B0E3j
		RTS
; ---------------------------------------------------------------------------

loc_B0DC:				; B0A4j
		CMP	#4
		BNE	loc_B0E9
		DEC	$43E,X
		BNE	locret_B0DB
		LDA	#2
		BNE	loc_B09B

loc_B0E9:				; B0DEj
		LDA	#0
		STA	$7D,X
		LDA	$37E,X
		CLC
		ADC	#3
		STA	$37E,X
		LDA	$36B,X
		ADC	#0
		STA	$36B,X
		JSR	sub_B117
		LDA	$6C
		BNE	loc_B109
		JSR	loc_C39F
		RTS
; ---------------------------------------------------------------------------

loc_B109:				; B103j
		JSR	sub_C3CE
		LDA	$3CA,X
		BEQ	locret_B116
		LDA	#$A1 ; '¡'
		STA	$3DA,X

locret_B116:				; B10Fj
		RTS

; =============== S U B	R O U T	I N E =======================================


sub_B117:				; B0FEp B756p
		DEC	$43E,X
		BNE	sub_B12E
		LDA	#2
		STA	$43E,X
		DEC	$436,X
		LDA	$436,X
		BPL	sub_B12E
		LDA	#3
		STA	$436,X
; End of function sub_B117


; =============== S U B	R O U T	I N E =======================================


sub_B12E:				; loc_B0D5p B11Aj ...
		JSR	sub_CE4A
		LDY	$436,X
		LDA	#<tbl_B058
		STA	$E
		LDA	#>tbl_B058
		STA	$F
		LDA	tbl_B06A,Y
		JSR	sub_B770
		RTS
; End of function sub_B12E

; ---------------------------------------------------------------------------

proc_B143:				; AED1o
		LDA	$4A2,X
		ORA	#$20 ; ' '
		STA	$4A2,X
		LDA	$3DA,X
		AND	#$F
		CMP	#1
		BNE	locret_B182
		INC	$37E,X
		JSR	loc_B775
		LDA	$37E,X
		CMP	#$B8 ; '¸'
		BCC	locret_B182
		LDA	#$B1 ; '±'
		STA	$3DA,X
		LDA	#0
		STA	$3E2,X
		STA	$3EA,X
		STA	$3CA,X
		JSR	sub_D604
		CPX	#2
		BCS	locret_B182
		CPX	$15
		BNE	locret_B182
		LDA	$F0
		ORA	#$40 ; '@'
		STA	$F0

locret_B182:				; B152j B15Fj	...
		RTS
; ---------------------------------------------------------------------------

proc_B183:				; AED3o
		LDA	$3DA,X
		AND	#$F
		CMP	#1
		BNE	loc_B195
		LDA	#9
		STA	$540,X
		INC	$3DA,X
		RTS
; ---------------------------------------------------------------------------

loc_B195:				; B18Aj
		DEC	$540,X
		BEQ	loc_B1A5
		LDA	#0
		STA	$80,X
		JSR	loc_A575
		JSR	sub_D266
		RTS
; ---------------------------------------------------------------------------

loc_B1A5:				; B198j
		LDA	#0
		STA	$3EA,X
		STA	$3DA,X
		RTS
; ---------------------------------------------------------------------------
tbl_B1AE:	.BYTE $FC, $5F,	$60, $FC, $61, $62, $FC, $5F, $60, $FC,	$61, $62, $63, $64, $60, $FC
					; sub_B25Ct B260t
		.BYTE $65, $62,	$66, $67, $60, $68, $69, $62, $6A, $67,	$60, $6B, $69, $62
tbl_B1CC:	.BYTE $12		; B264r
		.BYTE  $C
		.BYTE	6
		.BYTE	0
tbl_B1D0:	.BYTE $18		; B27Cr
		.BYTE $FC ; ü
		.BYTE	4
; ---------------------------------------------------------------------------

proc_B1D3:				; AECFo
		LDA	$F1
		ORA	#$20 ; ' '
		STA	$F1
		LDA	$3DA,X
		AND	#$F
		CMP	#1
		BNE	loc_B1F8
		LDA	#0
		STA	$56E,X
		STA	$436,X
		LDA	#9
		STA	$540,X
		LDA	#0
		STA	$510,X
		INC	$3DA,X
		RTS
; ---------------------------------------------------------------------------

loc_B1F8:				; B1E0j
		CMP	#2
		BNE	loc_B20D
		DEC	$540,X
		BNE	loc_B209
		LDA	#8
		STA	$43E,X
		INC	$3DA,X

loc_B209:				; B1FFj
		JSR	sub_D266
		RTS
; ---------------------------------------------------------------------------

loc_B20D:				; B1FAj
		CMP	#3
		BNE	loc_B23B
		LDA	$37E,X
		CLC
		ADC	#1
		STA	$37E,X
		LDA	$36B,X
		ADC	#0
		STA	$36B,X
		JSR	sub_B47C
		JSR	sub_B296
		DEC	$43E,X
		BNE	locret_B23A
		LDA	#5
		STA	$43E,X
		LDA	#3
		STA	$436,X
		INC	$3DA,X

locret_B23A:				; B22Bj
		RTS
; ---------------------------------------------------------------------------

loc_B23B:				; B20Fj
		CMP	#4
		BNE	loc_B289
		DEC	$43E,X
		BNE	sub_B259
		LDA	#5
		STA	$43E,X
		DEC	$436,X
		LDA	$436,X
		BPL	sub_B259
		LDA	#0
		STA	$436,X
		INC	$3DA,X

; =============== S U B	R O U T	I N E =======================================


sub_B259:				; B242j B24Fj	...
		LDY	$436,X
; End of function sub_B259


; =============== S U B	R O U T	I N E =======================================


sub_B25C:				; B30Dp
		LDA	#<tbl_B1AE
		STA	$E
		LDA	#>tbl_B1AE
		STA	$F
		LDA	tbl_B1CC,Y
		JSR	sub_B770
		LDA	$436,X
		CMP	#1
		BNE	locret_B288
		JSR	sub_D5AE
		TXA
		PHA
		LDA	$3C1,X
		AND	#3
		TAX
		LDA	tbl_B1D0,X
		CLC
		ADC	$203,Y
		STA	$203,Y
		PLA
		TAX

locret_B288:				; B26Fj
		RTS
; End of function sub_B25C

; ---------------------------------------------------------------------------

loc_B289:				; B23Dj
		CMP	#5
		BEQ	loc_B290
		JMP	loc_B306
; ---------------------------------------------------------------------------

loc_B290:				; B28Bj
		JSR	sub_B806
		JSR	loc_B775

; =============== S U B	R O U T	I N E =======================================


sub_B296:				; B225p
		JSR	sub_CC1B
		LDA	$E
		BEQ	loc_B2A8
		LDA	#$10
		STA	$43E,X
		LDA	#$26 ; '&'
		STA	$3DA,X
		RTS
; ---------------------------------------------------------------------------

loc_B2A8:				; B29Bj
		JSR	sub_C6B3
		LDA	$36B,X
		BNE	loc_B2B6
		JSR	sub_C67A
		JMP	loc_B2B9
; ---------------------------------------------------------------------------

loc_B2B6:				; B2AEj
		JSR	sub_C69D

loc_B2B9:				; B2B3j
		JSR	sub_C3EE
		LDA	$3CA,X
		BNE	loc_B2FA
		LDA	#0
		STA	$E
		JSR	sub_C51C
		JSR	sub_C5AA
		LDA	$E
		BEQ	loc_B2EF
		LDY	$548,X
		CMP	#1
		BNE	loc_B2E2
		CPY	#$13
		BCC	loc_B2DE
		CPY	#$43 ; 'C'
		BCC	loc_B2EF

loc_B2DE:				; B2D8j
		LDA	#$14
		BNE	loc_B2EC

loc_B2E2:				; B2D4j
		CPY	#$13
		BCC	loc_B2EA
		CPY	#$43 ; 'C'
		BCS	loc_B2EF

loc_B2EA:				; B2E4j
		LDA	#$44 ; 'D'

loc_B2EC:				; B2E0j
		STA	$548,X

loc_B2EF:				; B2CDj B2DCj	...
		JSR	loc_C39F
		LDA	$3DA,X
		CMP	#$31 ; '1'
		BEQ	loc_B2FF
		RTS
; ---------------------------------------------------------------------------

loc_B2FA:				; B2BFj
		LDA	#$51 ; 'Q'
		STA	$3DA,X

loc_B2FF:				; B2F7j
		LDA	$F1
		ORA	#$40 ; '@'
		STA	$F1
		RTS
; End of function sub_B296

; ---------------------------------------------------------------------------

loc_B306:				; B28Dj
		DEC	$43E,X
		BEQ	loc_B311
		LDY	#4
		JSR	sub_B25C
		RTS
; ---------------------------------------------------------------------------

loc_B311:				; B309j B36Bj
		LDA	#$61 ; 'a'
		STA	$3DA,X
		RTS
; ---------------------------------------------------------------------------
tbl_B317:	.BYTE $FC ; ü
		.BYTE $71 ; q
		.BYTE $FC ; ü
		.BYTE $FC ; ü
		.BYTE $72 ; r
		.BYTE $73 ; s
		.BYTE $FC ; ü
		.BYTE $72 ; r
		.BYTE $FC ; ü
		.BYTE $FC ; ü
		.BYTE $71 ; q
		.BYTE $73 ; s
tbl_B323:	.BYTE	0		; B398r
tbl_B324:	.BYTE	6		; B3A9r
		.BYTE $F9 ; ù
		.BYTE	7
; ---------------------------------------------------------------------------

proc_B327:				; AED5o
		LDA	$3DA,X
		AND	#$F
		CMP	#1
		BNE	loc_B33A
		LDA	#0
		STA	$436,X
		LDA	#5
		JMP	loc_B09B
; ---------------------------------------------------------------------------

loc_B33A:				; B32Ej
		CMP	#2
		BNE	loc_B36E
		DEC	$43E,X
		BNE	loc_B363
		LDA	#5
		STA	$43E,X
		INC	$436,X
		LDA	$436,X
		CMP	#4
		BNE	loc_B363
		LDA	$4F5
		STA	$540,X
		LDA	#1
		STA	$436,X
		LDA	$4F6
		JMP	loc_B09B
; ---------------------------------------------------------------------------

loc_B363:				; B341j B350j
		JSR	sub_B259

loc_B366:				; loc_B3DAj
		JSR	sub_CC1B
		LDA	$E
		BNE	loc_B311
		RTS
; ---------------------------------------------------------------------------

loc_B36E:				; B33Cj
		CMP	#3
		BNE	loc_B3DD
		DEC	$540,X
		BNE	loc_B37A
		INC	$3DA,X

loc_B37A:				; B375j
		DEC	$43E,X
		BNE	loc_B38D
		LDA	$4F6
		STA	$43E,X
		LDA	$436,X
		EOR	#1
		STA	$436,X

loc_B38D:				; B37Dj
		LDA	#<tbl_B317
		STA	$E
		LDA	#>tbl_B317
		STA	$F
		LDY	$436,X
		LDA	tbl_B323,Y
		JSR	sub_B770
		LDA	$436,X
		BEQ	loc_B3DA
		LDA	$3C1,X
		AND	#3
		TAY
		LDA	tbl_B324,Y
		STA	0
		JSR	sub_D5AE
		TXA
		PHA
		LDX	#1

loc_B3B5:				; B3D6j
		LDA	$206,Y
		PHA
		AND	#$BF ; '¿'
		STA	1
		PLA
		EOR	#$40 ; '@'
		AND	#$40 ; '@'
		ORA	1
		STA	$206,Y
		LDA	$207,Y
		CLC
		ADC	0
		STA	$207,Y
		TYA
		CLC
		ADC	#$C
		TAY
		DEX
		BPL	loc_B3B5
		PLA
		TAX

loc_B3DA:				; B3A1j
		JMP	loc_B366
; ---------------------------------------------------------------------------

loc_B3DD:				; B370j
		LDA	#$71 ; 'q'
		STA	$3DA,X
		LDA	#$20 ; ' '
		STA	$3D2,X
		RTS
; ---------------------------------------------------------------------------
tbl_B3E8:	.BYTE $FC		; sub_B47Ct B480t
		.BYTE $6C ; l
		.BYTE $6D ; m
		.BYTE $FC ; ü
		.BYTE $6E ; n
		.BYTE $6F ; o
		.BYTE $FC ; ü
		.BYTE $6C ; l
		.BYTE $6D ; m
		.BYTE $FC ; ü
		.BYTE $6E ; n
		.BYTE $70 ; p
tbl_B3F4:	.BYTE	0		; B487r
		.BYTE	6
; ---------------------------------------------------------------------------

proc_B3F6:				; AED7o
		LDA	$3DA,X
		AND	#$F
		CMP	#1
		BNE	loc_B428
		LDA	#8
		STA	$43E,X
		LDA	#1
		STA	$436,X
		LDA	$574,X
		CMP	$15
		BNE	loc_B419
		LDA	$F1
		ORA	#$80 ; '€'
		STA	$F1
		JMP	loc_B41F
; ---------------------------------------------------------------------------

loc_B419:				; B40Ej
		LDA	$F1
		ORA	#$40 ; '@'
		STA	$F1

loc_B41F:				; B416j
		LDA	#$FF
		STA	$574,X
		INC	$3DA,X
		RTS
; ---------------------------------------------------------------------------

loc_B428:				; B3FDj
		CMP	#2
		BNE	loc_B456
		LDA	$37E,X
		SEC
		SBC	#2
		STA	$37E,X
		LDA	$36B,X
		SBC	#0
		STA	$36B,X
		CMP	#2
		BCC	loc_B449
		LDA	#0
		STA	$36B,X
		STA	$37E,X

loc_B449:				; B43Fj
		DEC	$43E,X
		BNE	sub_B47C
		JSR	sub_B47C
		LDA	#$A
		JMP	loc_B09B
; ---------------------------------------------------------------------------

loc_B456:				; B42Aj
		CMP	#3
		BNE	loc_B463
		DEC	$43E,X
		BNE	locret_B462
		INC	$3DA,X

locret_B462:				; B45Dj
		RTS
; ---------------------------------------------------------------------------

loc_B463:				; B458j
		LDA	$37E,X
		CLC
		ADC	#3
		STA	$37E,X
		LDA	$36B,X
		ADC	#0
		STA	$36B,X
		LDA	$436,X
		EOR	#1
		STA	$436,X

; =============== S U B	R O U T	I N E =======================================


sub_B47C:				; B222p B44Cj	...
		LDA	#<tbl_B3E8
		STA	$E
		LDA	#>tbl_B3E8
		STA	$F
		LDY	$436,X
		LDA	tbl_B3F4,Y
		JSR	sub_B770
		JSR	loc_C39F
		RTS
; End of function sub_B47C

; ---------------------------------------------------------------------------
tbl_B491:	.BYTE $FC ; ü		; B55Ct B55Et
		.BYTE $71 ; q
		.BYTE $FC ; ü
		.BYTE $FC ; ü
		.BYTE $72 ; r
		.BYTE $73 ; s
		.BYTE $FC ; ü
		.BYTE $71 ; q
		.BYTE $FC ; ü
		.BYTE $FC ; ü
		.BYTE $72 ; r
		.BYTE $73 ; s
		.BYTE $FC ; ü
		.BYTE $71 ; q
		.BYTE $77 ; w
		.BYTE $FC ; ü
		.BYTE $72 ; r
		.BYTE $73 ; s
		.BYTE $FC ; ü
		.BYTE $71 ; q
		.BYTE $78 ; x
		.BYTE $FC ; ü
		.BYTE $72 ; r
		.BYTE $73 ; s
		.BYTE $FC ; ü
		.BYTE $71 ; q
		.BYTE $79 ; y
		.BYTE $FC ; ü
		.BYTE $72 ; r
		.BYTE $73 ; s
tbl_B4AF:	.BYTE $FC ; ü		; loc_B563t B565t
		.BYTE $74 ; t
		.BYTE $FC ; ü
		.BYTE $FC ; ü
		.BYTE $75 ; u
		.BYTE $76 ; v
		.BYTE $FC ; ü
		.BYTE $74 ; t
		.BYTE $FC ; ü
		.BYTE $FC ; ü
		.BYTE $75 ; u
		.BYTE $76 ; v
		.BYTE $FC ; ü
		.BYTE $74 ; t
		.BYTE $77 ; w
		.BYTE $FC ; ü
		.BYTE $75 ; u
		.BYTE $76 ; v
		.BYTE $FC ; ü
		.BYTE $74 ; t
		.BYTE $78 ; x
		.BYTE $FC ; ü
		.BYTE $75 ; u
		.BYTE $76 ; v
		.BYTE $FC ; ü
		.BYTE $74 ; t
		.BYTE $79 ; y
		.BYTE $FC ; ü
		.BYTE $75 ; u
		.BYTE $76 ; v
tbl_B4CD:	.BYTE $18		; B56Er
		.BYTE $12
		.BYTE  $C
		.BYTE	6
		.BYTE	0
tbl_B4D2:	.BYTE	3		; B59Fr
		.BYTE	0
		.BYTE	0
; ---------------------------------------------------------------------------

proc_B4D5:				; AED9o
		LDA	$3DA,X
		AND	#$F
		CMP	#1
		BNE	loc_B4EE
		LDA	#4
		STA	$518,X
		LDA	#4
		STA	$436,X
		LDA	$4F7
		JMP	loc_B09B
; ---------------------------------------------------------------------------

loc_B4EE:				; B4DCj
		JSR	sub_CC1B
		LDA	$E
		BEQ	loc_B4FB
		LDA	#$61 ; 'a'
		STA	$3DA,X
		RTS
; ---------------------------------------------------------------------------

loc_B4FB:				; B4F3j
		DEC	$43E,X
		BNE	loc_B555
		LDA	$4F8
		STA	$43E,X
		DEC	$518,X
		BNE	loc_B555
		LDA	#4
		STA	$518,X
		DEC	$436,X
		LDA	$436,X
		BPL	loc_B555
		LDY	$454,X
		LDA	$56E,X
		BNE	loc_B52B
		INY
		CPY	#3
		BCC	loc_B527
		LDY	#2

loc_B527:				; B523j
		TYA
		STA	$454,X

loc_B52B:				; B51Ej
		JSR	proc_A169
		LDA	#0
		STA	$3DA,X
		STA	$3CA,X
		LDA	$37E,X
		SEC
		SBC	#4
		STA	$37E,X
		LDA	$36B,X
		SBC	#0
		STA	$36B,X
		LDA	#$FF
		STA	$3A9,X
		LDA	#$B0 ; '°'
		STA	$3B5,X
		JSR	sub_D2EF
		RTS
; ---------------------------------------------------------------------------

loc_B555:				; B4FEj B509j	...
		LDA	$518,X
		AND	#1
		BNE	loc_B563
		LDA	#<tbl_B491
		LDY	#>tbl_B491
		JMP	loc_B567
; ---------------------------------------------------------------------------

loc_B563:				; B55Aj
		LDA	#<tbl_B4AF
		LDY	#>tbl_B4AF

loc_B567:				; B560j
		STA	$E
		STY	$F
		LDY	$436,X
		LDA	tbl_B4CD,Y
		JSR	sub_B770
		LDA	$56E,X
		BNE	locret_B5A7
		JSR	sub_D5AE
		TXA
		PHA
		LDA	$209,Y
		CMP	#$77 ; 'w'
		BEQ	loc_B596
		CMP	#$78 ; 'x'
		BEQ	loc_B596
		CMP	#$79 ; 'y'
		BEQ	loc_B596
		CMP	#$FC ; 'ü'
		BEQ	loc_B596
		TYA
		CLC
		ADC	#$C
		TAY

loc_B596:				; B583j B587j	...
		LDA	$454,X
		TAX
		LDA	$20A,Y
		AND	#$FC ; 'ü'
		ORA	tbl_B4D2,X
		STA	$20A,Y
		PLA
		TAX

locret_B5A7:				; B577j
		RTS
; ---------------------------------------------------------------------------
tbl_B5A8:	.BYTE $FC ; ü		; B5EDt B5F1t
		.BYTE $48 ; H
		.BYTE $42 ; B
		.BYTE $FC ; ü
		.BYTE $49 ; I
		.BYTE $43 ; C
		.BYTE $FC ; ü
		.BYTE $A4 ; ¤
		.BYTE $A5 ; ¥
		.BYTE $FC ; ü
		.BYTE $A6 ; ¦
		.BYTE $A7 ; §
tbl_B5B4:	.BYTE	0		; B5F5r
		.BYTE	6
; ---------------------------------------------------------------------------

proc_B5B6:				; AEDBo
		LDA	$3DA,X
		AND	#$F
		CMP	#1
		BNE	loc_B5D8
		LDA	#9
		STA	$436,X
		LDA	#0
		STA	$3E2,X
		CPX	$15
		BNE	loc_B5D3
		LDA	$F0
		ORA	#$80 ; '€'
		STA	$F0

loc_B5D3:				; B5CBj
		LDA	#8
		JMP	loc_B09B
; ---------------------------------------------------------------------------

loc_B5D8:				; B5BDj
		DEC	$43E,X
		BNE	loc_B5E7
		LDA	#8
		STA	$43E,X
		DEC	$436,X
		BEQ	loc_B5FC

loc_B5E7:				; B5DBj
		LDA	$436,X
		AND	#1
		TAY
		LDA	#<tbl_B5A8
		STA	$E
		LDA	#>tbl_B5A8
		STA	$F
		LDA	tbl_B5B4,Y
		JSR	sub_B770
		RTS
; ---------------------------------------------------------------------------

loc_B5FC:				; B5E5j
		LDA	#$11
		STA	$3DA,X
		RTS
; ---------------------------------------------------------------------------

proc_B602:				; AEDDo
		RTS
; ---------------------------------------------------------------------------
tbl_B603:	.BYTE $FC ; ü		; B672t B676t
		.BYTE $FC ; ü
		.BYTE $AE ; ®
		.BYTE $FC ; ü
		.BYTE $FC ; ü
		.BYTE $FC ; ü
		.BYTE $FC ; ü
		.BYTE $AF ; ¯
		.BYTE $B0 ; °
		.BYTE $FC ; ü
		.BYTE $FC ; ü
		.BYTE $FC ; ü
		.BYTE $FC ; ü
		.BYTE $B2 ; ²
		.BYTE $B3 ; ³
		.BYTE $FC ; ü
		.BYTE $B1 ; ±
		.BYTE $FC ; ü
		.BYTE $B5 ; µ
		.BYTE $B4 ; ´
		.BYTE $FC ; ü
		.BYTE $B4 ; ´
		.BYTE $B5 ; µ
		.BYTE $FC ; ü
		.BYTE $FC ; ü
		.BYTE $B5 ; µ
		.BYTE $B4 ; ´
		.BYTE $FC ; ü
		.BYTE $B4 ; ´
tbl_B620:	.BYTE $B5 ; µ		; B67Dr
		.BYTE $18
		.BYTE $12
		.BYTE  $C
		.BYTE	6
tbl_B625:	.BYTE	0		; B686r
		.BYTE	4
		.BYTE	4
		.BYTE	4
		.BYTE	0
tbl_B62A:	.BYTE	0		; B68Br
		.BYTE $30 ; 0
		.BYTE $18
		.BYTE	0
		.BYTE	0
		.BYTE	0
; ---------------------------------------------------------------------------

proc_B630:				; AEE1o
		LDA	$3DA,X
		AND	#$F
		CMP	#1
		BNE	loc_B650
		LDA	#5
		STA	$436,X
		LDA	#$8C ; 'Œ'
		STA	$37E,X
		LDA	#3
		STA	$4A2,X
		JSR	sub_D604
		LDA	#4
		JMP	loc_B09B
; ---------------------------------------------------------------------------

loc_B650:				; B637j
		CMP	#2
		BNE	loc_B6CC
		JSR	sub_D604
		DEC	$43E,X
		BNE	loc_B666
		LDA	#4
		STA	$43E,X
		DEC	$436,X
		BEQ	loc_B6C4

loc_B666:				; B65Aj
		LDA	#1
		STA	$3C1,X
		JSR	sub_D5C2
		CMP	#$11
		BEQ	locret_B6C3
		LDA	#<tbl_B603
		STA	$E
		LDA	#>tbl_B603
		STA	$F
		LDY	$436,X
		LDA	tbl_B620,Y
		JSR	sub_B770
		LDY	$436,X
		LDA	tbl_B625,Y
		STA	0
		LDA	tbl_B62A,Y
		STA	1
		JSR	sub_D5AE
		TXA
		PHA
		LDX	#5

loc_B697:				; B6BFj
		LDA	$203,Y
		CLC
		ADC	0
		STA	$203,Y
		LDA	$202,Y
		AND	#$F
		STA	$202,Y
		LDA	1
		AND	#1
		ASL	A
		ASL	A
		ASL	A
		ASL	A
		ASL	A
		ASL	A
		ORA	$202,Y
		STA	$202,Y
		LSR	1
		INY
		INY
		INY
		INY
		DEX
		BPL	loc_B697
		PLA
		TAX

locret_B6C3:				; B670j
		RTS
; ---------------------------------------------------------------------------

loc_B6C4:				; B664j
		JSR	sub_D604
		LDA	#$40 ; '@'
		JMP	loc_B09B
; ---------------------------------------------------------------------------

loc_B6CC:				; B652j
		DEC	$43E,X
		BNE	locret_B6EB
		LDA	#$FF
		CPX	#2
		BCC	loc_B6DD
		LDY	$68
		BEQ	loc_B6DD
		LDA	#$C1 ; 'Á'

loc_B6DD:				; B6D5j B6D9j
		STA	$3DA,X
		CPX	#2
		BCS	locret_B6EB
		TXA
		PHA
		JSR	sub_BD4D
		PLA
		TAX

locret_B6EB:				; B6CFj B6E2j
		RTS
; ---------------------------------------------------------------------------

proc_B6EC:				; AEE3o
		LDA	$3DA,X
		AND	#$F
		CMP	#1
		BNE	loc_B713
		LDY	#$10
		LDA	$350,X
		CMP	#$10
		BCC	loc_B704
		LDY	#$F0 ; 'ð'
		CMP	#$F0 ; 'ð'
		BCC	loc_B70C

loc_B704:				; B6FCj
		TYA
		CLC
		ADC	$350,X
		STA	$350,X

loc_B70C:				; B702j
		JSR	sub_AC56
		INC	$3DA,X
		RTS
; ---------------------------------------------------------------------------

loc_B713:				; B6F3j
		JSR	sub_A6CC
		RTS
; ---------------------------------------------------------------------------

proc_B717:				; AEDFo
		LDA	$3DA,X
		AND	#$F
		CMP	#1
		BNE	loc_B73E
		LDA	#6
		STA	$540,X
		LDA	#0
		STA	$3CA,X
		STA	$40E,X
		STA	$4AE,X
		LDA	#$FE ; 'þ'
		STA	$3A9,X
		LDA	#$80 ; '€'
		STA	$3B5,X
		INC	$3DA,X
		RTS
; ---------------------------------------------------------------------------

loc_B73E:				; B71Ej
		CMP	#2
		BNE	loc_B74A
		DEC	$540,X
		BNE	loc_B74D
		INC	$3DA,X

loc_B74A:				; B740j
		JSR	sub_C3CE

loc_B74D:				; B745j
		LDA	#0
		STA	$80,X
		STA	$84,X
		JSR	sub_AA33
		JSR	sub_B117
		LDA	$3CA,X
		BEQ	locret_B76F
		LDA	$350,X
		PHA
		JSR	sub_A1C0
		PLA
		STA	$350,X
		LDA	$F0
		ORA	#$40 ; '@'
		STA	$F0

locret_B76F:				; B75Cj
		RTS

; =============== S U B	R O U T	I N E =======================================

; ®å®¦¥ ­  ¨­¨æ¨ «¨§ æ¨î ¢á¥å á¯à ©â®¢	­  ãà®¢­¥.

sub_B770:				; loc_AF8Ep B13Fp ...
		STA	$91
		JSR	sub_DA87

loc_B775:				; B157p B293p
		JSR	sub_D5C2
		JSR	sub_D5AE
		JSR	sub_D6E6
		JSR	sub_B785
		JSR	sub_D5C2
		RTS
; End of function sub_B770


; =============== S U B	R O U T	I N E =======================================

; ‡ ç¥¬	íâ® ¤¥« âì á® á¯à ©â ¬¨?

sub_B785:				; B77Ep sub_CD35p
		JSR	sub_D5AE
		LDA	#6
		STA	$E

loc_B78C:				; B79Dj
		LDA	$4A2,X
		AND	#$20 ; ' '
		ORA	$202,Y
		STA	$202,Y
		INY
		INY
		INY
		INY
		DEC	$E
		BNE	loc_B78C
		RTS
; End of function sub_B785

; ---------------------------------------------------------------------------
tbl_unk_B7A0:	.BYTE $01, $01,	$01, $01, $01, $01, $01, $01, $01, $01,	$01, $01, $01, $01, $01, $01
					; B86Fr
		.BYTE $01, $01,	$01, $01, $00, $00, $01, $00, $00, $FF,	$00, $00, $FF, $FF, $FF, $FF
		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
		.BYTE $FF, $FF,	$FF, $FF, $00, $00, $FF, $00, $00, $01,	$00, $00, $01, $01, $01, $01
		.BYTE $01, $01,	$01, $01, $01, $01, $01, $01, $01, $01,	$01, $01, $01, $01, $01, $01
tbl_unk_B800:	.BYTE $80 ; €		; B81Dr
		.BYTE $68 ; h
		.BYTE $40 ; @
		.BYTE $40 ; @
		.BYTE $30 ; 0
		.BYTE	1

; =============== S U B	R O U T	I N E =======================================


sub_B806:				; loc_B290p
		LDA	$510,X
		BNE	loc_B837
		INC	$510,X
		LDA	#3
		STA	$540,X
		LDA	#$F0 ; 'ð'
		STA	$528,X
		LDA	$1C,X
		AND	#3
		TAY
		LDA	tbl_unk_B800,Y
		STA	$538,X
		LDA	#0
		STA	$530,X
		LDY	#1
		LDA	$3C1,X
		AND	#1
		BEQ	loc_B833
		LDY	#$30 ; '0'

loc_B833:				; B82Fj
		TYA
		STA	$548,X

loc_B837:				; B809j
		DEC	$530,X
		BNE	loc_B844
		LDA	#0
		STA	$530,X
		INC	$538,X

loc_B844:				; B83Aj
		LDA	$528,X
		CLC
		ADC	$538,X
		STA	$528,X
		LDA	$37E,X
		ADC	#0
		STA	$37E,X
		LDA	#0
		ADC	$36B,X
		STA	$36B,X
		DEC	$540,X
		BNE	locret_B892
		LDA	#3
		STA	$540,X
		LDY	$548,X
		LDA	$350,X
		CLC
		ADC	tbl_unk_B7A0,Y
		STA	$350,X
		JSR	sub_B893
		BEQ	loc_B883
		LDA	$350,X
		CLC
		ADC	0
		STA	$350,X

loc_B883:				; B878j
		INC	$548,X
		LDA	$548,X
		CMP	#$60 ; '`'
		BCC	locret_B892
		LDA	#1
		STA	$548,X

locret_B892:				; B861j B88Bj
		RTS
; End of function sub_B806


; =============== S U B	R O U T	I N E =======================================


sub_B893:				; B875p
		STX	$E
		LDY	#7

loc_B897:				; B8E6j
		CPY	$E
		BEQ	loc_B8E5
		LDA	$3DA,Y
		CMP	#$FF
		BEQ	loc_B8E5
		LDA	$3CA,Y
		BEQ	loc_B8E5
		LDA	$350,X
		CLC
		ADC	#8
		SEC
		SBC	$350,Y
		BPL	loc_B8B8
		EOR	#$FF
		CLC
		ADC	#1

loc_B8B8:				; B8B1j
		CMP	#$20 ; ' '
		BCS	loc_B8E5
		JSR	sub_CCA3
		LDA	4
		BNE	loc_B8E5
		LDA	$F
		BEQ	loc_B8E5
		LDA	5
		CMP	#$40 ; '@'
		BCS	loc_B8E5
		CMP	#8
		BCC	loc_B8E5

loc_B8D1:				; B8F7j
		LDY	#$FF
		LDA	$548,X
		CMP	#$19
		BCC	loc_B8DE
		CMP	#$50 ; 'P'
		BCC	loc_B8E0

loc_B8DE:				; B8D8j
		LDY	#1

loc_B8E0:				; B8DCj
		STY	0
		LDA	#1
		RTS
; ---------------------------------------------------------------------------

loc_B8E5:				; B899j B8A0j	...
		DEY
		BPL	loc_B897
		LDA	#0
		SEC
		SBC	$350,X
		BPL	loc_B8F5
		EOR	#$FF
		CLC
		ADC	#1

loc_B8F5:				; B8EEj
		CMP	#$18
		BCC	loc_B8D1
		LDA	#0
		RTS
; End of function sub_B893

tbl_unk_B910:	
		.BYTE $FC, $FC,	$7A, $FC, $FC, $FC, $FC, $FC, $7B, $FC,	$FC, $7C, $FC, $7D, $7E, $FC
		.BYTE $7F, $80,	$81, $82, $83, $84, $85, $86, $FC, $87,	$88, $FC, $89, $8A, $FC, $8B
		.BYTE $8C, $FC,	$8D, $8E, $FC, $8F, $90, $FC, $FC, $FC,	$FC, $FC, $FC, $FC, $FC, $FC

tbl_unk_SIX:	
		.BYTE 0, 6, $C,	$12, $18, $1E, $24, $2A	; B9B8r

; =============== S U B	R O U T	I N E =======================================

; Big piranha!

piranha:				; loc_9C82p A122p ...
		LDX	$4B8
		BPL	loc_B981
		LDA	$46
		BNE	loc_B956
		LDA	$64
		AND	#1
		TAX

loc_B956:				; B94Fj B95Ej
		JSR	sub_BA89	; Piranha's AI.
		BEQ	loc_B961
		INX
		CPX	#8
		BCC	loc_B956
		RTS
; ---------------------------------------------------------------------------

loc_B961:				; B959j
		STX	$4B8
		LDA	#0
		STA	$4B9
		LDA	#$20 ; ' '
		STA	$46
		LDA	#3
		STA	$4C
		LDA	#1
		STA	$373
		LDA	#$94 ; '”'
		STA	$386
		LDA	#3
		STA	$4AA
		RTS
; ---------------------------------------------------------------------------

loc_B981:				; B94Bj
		LDA	$4C
		BNE	loc_B99F
		INC	$4B9
		LDY	#3
		LDA	$4B9
		CMP	#3
		BCC	loc_B993
		LDY	#$10

loc_B993:				; B98Fj
		STY	$4C
		CMP	#2
		BNE	loc_B99F
		LDA	$F3
		ORA	#$40 ; '@'
		STA	$F3

loc_B99F:				; B983j B997j
		LDA	$4B9
		CMP	#4
		BCS	loc_B9A9
		JSR	sub_BA89	; Piranha's AI.

loc_B9A9:				; B9A4j
		TXA
		PHA
		LDX	#8
		LDA	#<tbl_unk_B910
		STA	$E
		LDA	#>tbl_unk_B910
		STA	$F
		LDY	$4B9
		LDA	tbl_unk_SIX,Y
		JSR	sub_B770
		PLA
		TAX
		LDA	$4B9
		CMP	#3
		BEQ	loc_B9CA
		JMP	loc_BA51
; ---------------------------------------------------------------------------

loc_B9CA:				; B9C5j
		LDA	$3DA,X
		AND	#$F0 ; 'ð'
		CMP	#$E0 ; 'à'
		BEQ	locret_BA50
		LDA	$386
		SEC
		SBC	$37E,X
		CMP	#$16
		BCS	locret_BA50
		LDA	$37E,X
		CMP	#$98 ; '˜'
		BCS	locret_BA50
		LDA	$358
		SEC
		SBC	$350,X
		BPL	loc_B9F3
		EOR	#$FF
		CLC
		ADC	#1

loc_B9F3:				; B9ECj
		CMP	#$C
		BCS	locret_BA50
		CPX	#2
		BCS	locret_BA50
		JSR	sub_D604
		LDA	#$E0 ; 'à'
		STA	$3DA,X
		STA	$37E,X
		TXA
		PHA
		JSR	sub_D5AE
		LDA	#$A2 ; '¢'
		STA	OAM_BUFFER+1,Y
		LDA	#$A3 ; '£'
		STA	OAM_BUFFER+5,Y
		TXA
		STA	OAM_BUFFER+2,Y
		STA	OAM_BUFFER+6,Y
		LDA	OAM_BUFFER
		CLC
		ADC	#3
		STA	OAM_BUFFER,Y
		STA	OAM_BUFFER+4,Y
		LDA	#5
		LDX	$3C9
		CPX	#1
		BNE	loc_BA33
		LDA	#$FC ; 'ü'

loc_BA33:				; BA2Fj
		CLC
		ADC	OAM_BUFFER+3
		STA	OAM_BUFFER+3,Y
		CLC
		ADC	#8
		STA	OAM_BUFFER+7,Y
		LDA	#$12
		STA	$4C
		PLA
		TAX
		CPX	$15
		BNE	locret_BA50
		LDA	$F2
		ORA	#$40 ; '@'
		STA	$F2

locret_BA50:				; B9D1j B9DCj	...
		RTS
; ---------------------------------------------------------------------------

loc_BA51:				; B9C7j
		LDA	$4B9
		CMP	#4
		BCC	locret_BA88
		LDA	$3DA,X
		CMP	#$E0 ; 'à'
		BNE	loc_BA65
		JSR	sub_D604
		INC	$3DA,X

loc_BA65:				; BA5Dj
		LDA	$4B9
		CMP	#7
		BCC	locret_BA88
		LDA	$3DA,X
		AND	#$F0 ; 'ð'
		CMP	#$E0 ; 'à'
		BNE	loc_BA7A
		LDA	#$B1 ; '±'
		STA	$3DA,X

loc_BA7A:				; BA73j
		LDA	#$10
		STA	$46
		LDA	#$FF
		STA	$4B8
		LDX	#8
		JSR	sub_D604

locret_BA88:				; BA56j BA6Aj
		RTS
; End of function piranha


; =============== S U B	R O U T	I N E =======================================

; Piranha's AI.

sub_BA89:				; loc_B956p B9A6p
		LDA	$3DA,X
		BNE	loc_BAA5
		LDA	$36B,X
		BEQ	loc_BAA5
		LDA	$37E,X
		CMP	#$68 ; 'h'
		BCC	loc_BAA5
		LDA	$350,X
		CMP	#$B1 ; '±'
		BCS	loc_BAA5
		CMP	#$3F ; '?'
		BCS	loc_BAA8

loc_BAA5:				; BA8Cj BA91j	...
		LDA	#1
		RTS
; ---------------------------------------------------------------------------

loc_BAA8:				; BAA3j
		STA	$358
		LDA	$3C1,X
		AND	#3
		STA	$3C9
		LDA	#0

locret_BAB5:				; BCE9r
		RTS
; End of function sub_BA89

; ---------------------------------------------------------------------------
		.BYTE $A0, $A1,	$A0, $9D, $9E, $9F, $9E, $9D
tbl_unk_BABE:
		.BYTE 2, 2, $FE, $FE	; BB45r
tbl_unk_BAC2:
		.BYTE $FE, 2, $FE, 2	; BB4Br
tbl_unk_BAC6:
		.BYTE $30, $50,	$70, $90 ; BB51r
tbl_unk_BACA:
		.BYTE $90, $70,	$50, $30 ; BB57r

		; Difficulty table
tbl_unk_BACE:	
		; Thunderbolt initial timer
		; easy, normal, medium, hard
		.BYTE $A0, $90,	$80, $70
tbl_unk_BAD2:
		; Difficulty related (mb thunderbolt restart timer)
		.BYTE $22, $1E,	$1A
tbl_unk_BAD5:
		.BYTE $16

tbl_unk_BAD6:	
		.BYTE 0, 2, 4, 6, $A, $C, $E, $10, $12,	$14, $16, $18, $1A, $1C, $1E, $20
		.BYTE $22, $24,	$26, $28, $2A, $2C, $2E, $30

; =============== S U B	R O U T	I N E =======================================


sub_BAEE:				; A504p
		LDX	#0

loc_BAF0:				; BD1Cj
		LDA	$780,X
		BEQ	loc_BAF8
		JMP	loc_BB88
; ---------------------------------------------------------------------------

loc_BAF8:				; BAF3j
		LDA	$5E
		BEQ	loc_BAFF
		JMP	loc_BB85
; ---------------------------------------------------------------------------

loc_BAFF:				; BAFAj
		LDA	#$18
		STA	$5E
		LDA	$76
		CMP	#6
		BNE	loc_BB0E
		LDA	$94
		BNE	loc_BB0E
		RTS
; ---------------------------------------------------------------------------

loc_BB0E:				; BB07j BB0Bj
		LDA	$7BD
		BNE	loc_BB38
		TXA
		PHA
		LDX	#8

loc_BB17:				; BB1Dj
		DEX
		LDA	$3DA,X
		CMP	#$FF
		BNE	loc_BB17
		STX	$7BD
		JSR	sub_D5AE
		TYA
		LDX	#5

loc_BB28:				; BB2Fj
		STA	$7BE,X
		CLC
		ADC	#4
		DEX
		BPL	loc_BB28
		LDA	#0
		STA	$773
		PLA
		TAX

loc_BB38:				; BB11j
		LDA	#0
		STA	$7A4,X
		STA	$7AA,X
		LDA	$1D,X
		AND	#3
		TAY
		LDA	tbl_unk_BABE,Y
		STA	$798,X
		LDA	tbl_unk_BAC2,Y
		STA	$79E,X
		LDA	tbl_unk_BAC6,Y
		STA	$7B0,X
		LDA	tbl_unk_BACA,Y
		STA	$7B6,X
		INC	$792,X
		LDY	$792,X
		CPY	#$18
		BCC	loc_BB69
		LDY	#$17

loc_BB69:				; BB65j
		LDA	$7B0,X
		CLC
		ADC	tbl_unk_BAD5,Y
		STA	$7B0,X
		LDA	$7B6,X
		CLC
		ADC	tbl_unk_BAD5,Y
		STA	$7B6,X
		LDA	#$A0 ; ' '
		STA	$786,X
		INC	$780,X

loc_BB85:				; BAFCj
		JMP	loc_BD17
; ---------------------------------------------------------------------------

loc_BB88:				; BAF5j
		CMP	#1
		BNE	loc_BBC9
		LDA	$786,X
		AND	#$F
		BNE	loc_BB99
		LDA	$F1
		ORA	#8
		STA	$F1

loc_BB99:				; BB91j
		DEC	$786,X
		BEQ	loc_BBAB
		LDA	$786,X
		AND	#4
		BEQ	loc_BBA8
		JSR	sub_BDBA

loc_BBA8:				; BBA3j
		JMP	loc_BD17
; ---------------------------------------------------------------------------

loc_BBAB:				; BB9Cj
		JSR	sub_BDBA
		LDA	#$FF
		STA	$304,Y
		LDA	#2
		STA	$786,X
		LDA	#8
		STA	$78C,X
		LDA	$F0
		ORA	#4
		STA	$F0
		INC	$780,X
		JMP	loc_BD17
; ---------------------------------------------------------------------------

loc_BBC9:				; BB8Aj
		CMP	#2
		BNE	loc_BBD3
		JSR	sub_BE78
		JMP	loc_BD17
; ---------------------------------------------------------------------------

loc_BBD3:				; BBCBj
		LDA	$7A4,X
		CLC
		ADC	$7B0,X
		STA	$7A4,X
		BCC	loc_BBE9
		LDA	$359,X
		CLC
		ADC	$798,X
		STA	$359,X

loc_BBE9:				; BBDDj
		LDA	$7AA,X
		CLC
		ADC	$7B6,X
		STA	$7AA,X
		BCC	loc_BC27
		LDA	$79E,X
		BMI	loc_BC0F
		LDA	$387,X
		CLC
		ADC	$79E,X
		STA	$387,X
		LDA	$374,X
		ADC	#0
		STA	$374,X
		JMP	loc_BC27
; ---------------------------------------------------------------------------

loc_BC0F:				; BBF8j
		EOR	#$FF
		CLC
		ADC	#1
		STA	0
		LDA	$387,X
		SEC
		SBC	0
		STA	$387,X
		LDA	$374,X
		SBC	#0
		STA	$374,X

loc_BC27:				; BBF3j BC0Cj
		LDA	#0
		STA	$7BC
		JSR	sub_BD20	; Fireball (cloud projectile) handler.
		LDA	$7BC
		BNE	loc_BC37
		JMP	loc_BCB8
; ---------------------------------------------------------------------------

loc_BC37:				; BC32j
		CMP	#$F
		BNE	loc_BC3E
		JMP	loc_BCFE
; ---------------------------------------------------------------------------

loc_BC3E:				; BC39j
		LDA	$F3
		ORA	#$80 ; '€'
		STA	$F3
		LDA	$7BC
		AND	#1
		BEQ	loc_BC52
		LDA	$79E,X
		BMI	loc_BC5E
		BPL	loc_BC77

loc_BC52:				; BC49j
		LDA	$7BC
		AND	#2
		BEQ	loc_BC7E
		LDA	$79E,X
		BMI	loc_BC7E

loc_BC5E:				; BC4Ej
		EOR	#$FF
		CLC
		ADC	#1
		STA	$79E,X
		LDA	$1F,X
		AND	#4
		BEQ	loc_BC7E
		LDA	#$20 ; ' '
		LDY	$7B6,X
		CPY	#$CF ; 'Ï'
		BCC	loc_BC77
		LDA	#$E0 ; 'à'

loc_BC77:				; BC50j BC73j
		CLC
		ADC	$7B6,X
		STA	$7B6,X

loc_BC7E:				; BC57j BC5Cj	...
		LDA	$7BC
		AND	#4
		BEQ	loc_BC8C
		LDA	$798,X
		BMI	loc_BC98
		BPL	loc_BCB8

loc_BC8C:				; BC83j
		LDA	$7BC
		AND	#8
		BEQ	loc_BCB8
		LDA	$798,X
		BMI	loc_BCB8

loc_BC98:				; BC88j
		EOR	#$FF
		CLC
		ADC	#1
		STA	$798,X
		LDA	$20,X
		AND	#4
		BEQ	loc_BCB8
		LDA	#$20 ; ' '
		LDY	$7B0,X
		CPY	#$CF ; 'Ï'
		BCC	loc_BCB1
		LDA	#$E0 ; 'à'

loc_BCB1:				; BCADj
		CLC
		ADC	$7B0,X
		STA	$7B0,X

loc_BCB8:				; BC34j BC8Aj	...
		JSR	sub_BDF0
		LDA	$7BC
		BEQ	loc_BCC3
		JMP	sub_BD4D
; ---------------------------------------------------------------------------

loc_BCC3:				; BCBEj
		STX	0
		TXA
		CLC
		ADC	#9
		TAX
		JSR	sub_D5C2
		LDX	0
		CMP	#0
		BNE	loc_BD0F
		LDY	$7BE,X
		LDA	$8F
		STA	$200,Y
		DEC	$78C,X
		BNE	loc_BCE5
		LDA	#8
		STA	$78C,X

loc_BCE5:				; BCDEj
		LDA	$78C,X
		TAX
		LDA	locret_BAB5,X
		STA	$201,Y
		LDA	#0
		STA	$202,Y
		LDA	$8E
		STA	$203,Y
		LDX	0
		JMP	loc_BD17
; ---------------------------------------------------------------------------

loc_BCFE:				; BC3Bj
		LDA	#0
		STA	$780,X
		LDA	#$FF
		STA	$374,X
		LDY	dip_DIFFICULTY 		; read some initialized dip?
		LDA	tbl_unk_BAD2,Y
		STA	$5E

loc_BD0F:				; BCD1j
		LDY	$7BE,X
		LDA	#$F8 ; 'ø'
		STA	$200,Y

loc_BD17:				; loc_BB85j loc_BBA8j	...
		INX
		CPX	#6
		BCS	locret_BD1F
		JMP	loc_BAF0
; ---------------------------------------------------------------------------

locret_BD1F:				; BD1Aj
		RTS
; End of function sub_BAEE


; =============== S U B	R O U T	I N E =======================================

; Fireball (cloud projectile) handler.

sub_BD20:				; BC2Cp
		TXA
		PHA
		CLC
		ADC	#9
		TAX
		LDA	$37E,X
		STA	$B
		CLC
		ADC	#8
		STA	$A
		LDA	$350,X
		STA	$D
		CLC
		ADC	#8
		STA	$C
		JSR	sub_C370
		PLA
		TAX
		RTS
; End of function sub_BD20

; ---------------------------------------------------------------------------
tbl_0toC:	.BYTE 0, 1, 2, 3, 4, 5,	6, 7, 8, 9, $A,	$B, $C ; loc_BD67r

; =============== S U B	R O U T	I N E =======================================

sub_BD4D:				; proc_981Ep 9A1Ep ...
		LDX	#5

loc_BD4F:				; BD6Ej
		LDY	$7BE,X
		LDA	#$F8 ; 'ø'
		STA	$200,Y
		STA	$374,X
		LDA	#0
		STA	$780,X
		LDY	$6D
		CPY	#$E
		BCC	loc_BD67
		LDY	#$D

loc_BD67:				; BD63j
		LDA	tbl_0toC,Y
		STA	$792,X
		DEX
		BPL	loc_BD4F

		LDX	dip_DIFFICULTY
		LDA	tbl_unk_BACE,X

		LDY	$6D
		CPY	#$E
		BCC	loc_BD7D
		LDY	#$D

loc_BD7D:
		SEC
		SBC	tbl_unk_BAD6,Y
		LDY	$65
		BEQ	loc_BD88
		SEC
		SBC	#$30

loc_BD88:
		LDY	$69
		CPY	#$A
		BNE	loc_BD90
		LDA	#$30 ; '0'

loc_BD90:
		STA	$5E
		LDA	#0
		STA	$773

loc_BD97:				; BDABj
		LDX	$773
		JSR	sub_BDBA
		LDA	#$FF
		STA	$304,Y
		INC	$773
		LDA	$773
		CMP	$775
		BNE	loc_BD97
		JSR	sub_BFA7
		LDA	#0
		STA	$773
		RTS
; End of function sub_BD4D

; ---------------------------------------------------------------------------
tbl_unk_BDB6:	.BYTE $FF		; BDDBr
		.BYTE $AA ; ª
		.BYTE $55 ; U
		.BYTE	0

; =============== S U B	R O U T	I N E =======================================

sub_BDBA:				; BBA5p loc_BBABp ...
		STX	0
		LDY	$300
		LDX	$773
		LDA	$768,X
		STA	$301,Y
		LDA	$76C,X
		STA	$302,Y
		LDA	#1
		STA	$303,Y
		LDX	0
		LDA	$786,X
		AND	#3
		TAX
		LDA	tbl_unk_BDB6,X
		STA	$304,Y
		LDA	#0
		STA	$305,Y
		TYA
		CLC
		ADC	#4
		STA	$300
		LDX	0
		RTS
; End of function sub_BDBA


; =============== S U B	R O U T	I N E =======================================


sub_BDF0:				; loc_BCB8p
		LDA	#0
		STA	$7BC
		LDY	#1

loc_BDF7:				; BE4Bj
		LDA	$62,Y
		BEQ	loc_BE4A
		LDA	$3DA,Y
		BNE	loc_BE4A
		LDA	$359,X
		CLC
		ADC	#4
		STA	0
		LDA	$350,Y
		CLC
		ADC	#8
		SEC
		SBC	0
		BPL	loc_BE19
		EOR	#$FF
		CLC
		ADC	#1

loc_BE19:				; BE12j
		CMP	#6
		BCS	loc_BE4A
		TXA
		PHA
		CLC
		ADC	#9
		TAX
		JSR	sub_CCA3
		PLA
		TAX
		LDA	4
		BNE	loc_BE4A
		TXA
		PHA
		LDX	#8
		LDA	$F
		BNE	loc_BE36
		LDX	#$E

loc_BE36:				; BE32j
		STX	0
		PLA
		TAX
		LDA	5
		CMP	0
		BCS	loc_BE4A
		LDA	#$81 ; ''
		STA	$3DA,Y
		LDA	#1
		STA	$7BC

loc_BE4A:				; BDFAj BDFFj	...
		DEY
		BPL	loc_BDF7
		RTS
; End of function sub_BDF0

; ---------------------------------------------------------------------------
tbl_BE4E:	.BYTE $91, $FC,	$FC, $92, $FC, $FC, $93, $94, $FC, $95,	$96, $FC, $97, $98, $99, $9A
					; BF43t BF45t
		.BYTE $9B, $9C
tbl_BE60:	.BYTE $FC, $FC,	$91, $FC, $FC, $92, $FC, $94, $93, $FC,	$96, $95, $99, $98, $97, $9C
					; BF4Et BF50t
		.BYTE $9B
unk_BE71:	.BYTE $9A ; š		; BF59r
		.BYTE	0
		.BYTE	3
		.BYTE	6
		.BYTE	9
		.BYTE  $C
		.BYTE  $F

; =============== S U B	R O U T	I N E =======================================


sub_BE78:				; BBCDp
		LDA	$4BB
		BEQ	loc_BE80
		JMP	loc_BF24
; ---------------------------------------------------------------------------

loc_BE80:				; BE7Bj
		LDA	#0
		STA	$37C
		LDY	$773
		LDA	$79E,X
		BPL	loc_BEAC
		LDA	#$80 ; '€'
		STA	$4BD
		LDA	$764,Y
		SEC
		SBC	#$60 ; '`'
		STA	1
		LDA	$760,Y
		SBC	#0
		CMP	#$28 ; '('
		BCS	loc_BEA9
		CMP	#$23 ; '#'
		BCC	loc_BEA9
		LDA	#$23 ; '#'

loc_BEA9:				; BEA1j BEA5j
		JMP	loc_BEC8
; ---------------------------------------------------------------------------

loc_BEAC:				; BE8Bj
		LDA	#0
		STA	$4BD
		LDA	$764,Y
		CLC
		ADC	#$80 ; '€'
		STA	1
		LDA	$760,Y
		ADC	#0
		CMP	#$24 ; '$'
		BCC	loc_BEC8
		CMP	#$28 ; '('
		BCS	loc_BEC8
		LDA	#$28 ; '('

loc_BEC8:				; loc_BEA9j BEC0j ...
		STA	0
		STA	2
		LDA	$798,X
		BMI	loc_BEE3
		LDA	$4BD
		ORA	#$40 ; '@'
		STA	$4BD
		LDA	1
		CLC
		ADC	#4
		STA	1
		JMP	loc_BEEA
; ---------------------------------------------------------------------------

loc_BEE3:				; BECFj
		LDA	1
		SEC
		SBC	#1
		STA	1

loc_BEEA:				; BEE0j
		JSR	sub_E604
		LDA	2
		CMP	#$28 ; '('
		BCC	loc_BF01
		LDA	5
		SEC
		SBC	#$10
		STA	5
		CMP	#$F0 ; 'ð'
		BCS	loc_BF01
		INC	$37C

loc_BF01:				; BEF1j BEFCj
		LDA	5
		STA	$38F
		LDA	4
		STA	$361
		INC	$773
		LDA	$773
		CMP	$775
		BNE	loc_BF1B
		LDA	#0
		STA	$773

loc_BF1B:				; BF14j
		LDA	#3
		STA	$4BC
		INC	$4BB
		RTS
; ---------------------------------------------------------------------------

loc_BF24:				; BE7Dj
		DEC	$4BC
		BNE	loc_BF38
		LDA	#3
		STA	$4BC
		INC	$4BB
		LDA	$4BB
		CMP	#7
		BCS	loc_BF85

loc_BF38:				; BF27j
		TXA
		PHA
		LDX	#$11
		JSR	sub_D5C2
		CMP	#$11
		BEQ	loc_BF82
		LDX	#<tbl_BE4E
		LDY	#>tbl_BE4E
		LDA	$4BD
		AND	#$80 ; '€'
		BEQ	loc_BF52
		LDX	#<tbl_BE60
		LDY	#>tbl_BE60

loc_BF52:				; BF4Cj
		STX	0
		STY	1
		LDX	$4BB
		LDY	unk_BE71,X
		LDX	#0

loc_BF5E:				; BF80j
		LDA	$8F
		STA	$2E4,X
		CLC
		ADC	#8
		STA	$8F
		INX
		LDA	(0),Y
		STA	$2E4,X
		INY
		INX
		LDA	$4BD
		STA	$2E4,X
		INX
		LDA	$361
		STA	$2E4,X
		INX
		CPX	#$C
		BCC	loc_BF5E

loc_BF82:				; BF41j
		PLA
		TAX
		RTS
; ---------------------------------------------------------------------------

loc_BF85:				; BF36j
		LDY	#0
		LDA	$4BD
		AND	#$80 ; '€'
		BNE	loc_BF90
		LDY	#$10

loc_BF90:				; BF8Cj
		TYA
		CLC
		ADC	$38F
		STA	$387,X
		LDA	$37C
		STA	$374,X
		LDA	$361
		STA	$359,X
		INC	$780,X
; End of function sub_BE78


; =============== S U B	R O U T	I N E =======================================


sub_BFA7:				; BDADp
		LDA	#0
		STA	$4BB
		LDA	#$F8 ; 'ø'
		STA	$2E4

loc_BFB1:				; loc_C068r C06Er
		STA	$2E8
		STA	$2EC
		RTS
; End of function sub_BFA7

; ---------------------------------------------------------------------------
		.BYTE	0
		.BYTE	1
		.BYTE	2
		.BYTE	3
		.BYTE	5
		.BYTE	7
		.BYTE	9
		.BYTE  $C
		.BYTE  $F
		.BYTE $12
		.BYTE $16
		.BYTE $1A
		.BYTE $1E
		.BYTE $23 ; #
		.BYTE $27 ; '
		.BYTE $2C ; ,
		.BYTE $31 ; 1
		.BYTE $37 ; 7
		.BYTE $40 ; @
tbl_BFCB:	.BYTE $22, $28,	$22, $22, $28, $22, $22, $22, $22, $22,	$22, $28, $21, $21, $22, $20
					; loc_C079r
		.BYTE $21, $29,	$20, $21, $28, $28, $22, $22, $28, $28,	$21, $22, $22, $28, $20, $21
		.BYTE $22, $28,	$29, $21, $22, $22, $28, $20, $20, $22,	$22, $28, $21, $21, $22, $22
		.BYTE $29, $21,	$21, $22, $28, $29, $29, $20, $20, $21,	$21, $22, $22, $28, $28, $29
tbl_C00B:	.BYTE $6F, $F, $8F, $2F, $E, $25, $38, $88, $94, $25, $38, $F, $E5, $F9, $2F, $C4
					; C081r
		.BYTE $AE, $4F,	$AF, $AF, $82, $9B, $66, $77, $EB, $F2,	$F, $45, $58, $2E, $4F,	$8E
		.BYTE $AF, $2E,	$2F, $6E, $43, $59, $4E, $C4, $D9, $22,	$3B, $EF, $C, $11, $85,	$98
		.BYTE $F, $A6, $B7, $8F, $4F, $28, $35,	$A2, $BB, $66, $77, $63, $7A, $A5, $B8,	$2F

; =============== S U B	R O U T	I N E =======================================


tbl_C04B:				; loc_97F3p
		LDA	#0
		STA	$4F
		STA	$74F
		STA	$748
		STA	$749
		STA	$74A
		STA	$74D
		STA	$74E
		LDX	$6F
		CPX	#6
		BCS	loc_C068
		RTS
; ---------------------------------------------------------------------------

loc_C068:				; C065j
		LDA	loc_BFB1+1,X	; Wtf?
		STA	$748
		LDA	loc_BFB1+2,X	; Reading code as data?
		STA	$749
		LDY	#0
		LDX	$748

loc_C079:				; C0B7j
		LDA	tbl_BFCB,X
		STA	$700,Y
		STA	0
		LDA	tbl_C00B,X
		STA	$709,Y
		STA	1
		JSR	sub_E604
		LDA	4
		STA	$72D,Y
		LDA	#0
		STA	$71B,Y
		LDA	$700,Y
		CMP	#$28 ; '('
		BCC	loc_C0AD
		LDA	5
		SEC
		SBC	#$10
		STA	5
		CMP	#$F0 ; 'ð'
		BCS	loc_C0AD
		LDA	#1
		STA	$71B,Y

loc_C0AD:				; C09Bj C0A6j
		LDA	5
		STA	$724,Y
		INY
		INX
		CPX	$749
		BNE	loc_C079
		STY	$749
		LDX	#8
		LDA	#2
		LDY	$6F
		CPY	#$13
		BCC	loc_C0CC
		LDA	#8
		STA	$4F
		LDA	#$FF

loc_C0CC:				; C0C4j
		STA	0

loc_C0CE:				; C0E0j
		LDA	0
		STA	$712,X
		LDA	#1
		STA	$73F,X
		STA	$53,X
		LDA	#4
		STA	$736,X
		DEX
		BPL	loc_C0CE
		LDA	#0
		STA	$748
		STA	$74A
		RTS
; End of function tbl_C04B


; =============== S U B	R O U T	I N E =======================================


sub_C0EB:				; A507p
		LDA	$749
		BEQ	locret_C130
		LDA	$712
		BPL	loc_C101
		LDA	$4F
		BNE	locret_C130
		LDX	#8

loc_C0FB:				; C0FFj
		STA	$712,X
		DEX
		BPL	loc_C0FB

loc_C101:				; C0F3j
		LDA	#0
		STA	$748

loc_C106:				; C12Ej
		LDA	#0
		STA	$74F
		LDX	$749

loc_C10E:				; C11Dj
		LDA	$711,X
		CMP	#1
		BEQ	loc_C119
		CMP	#3
		BNE	loc_C11C

loc_C119:				; C113j
		INC	$74F

loc_C11C:				; C117j
		DEX
		BNE	loc_C10E
		LDX	$748
		JSR	sub_C181
		INC	$748
		LDA	$748
		CMP	$749
		BNE	loc_C106

locret_C130:				; C0EEj C0F7j
		RTS
; End of function sub_C0EB

; ---------------------------------------------------------------------------
tbl_unk_C131:	.BYTE $39, $26,	$13, 0	; C1C7r
tbl_unk_C135:	.BYTE $FF, $FF,	$83, $24, $24, $24, $FF, $FF, $83, $9E,	$9F, $A0, $FF, $FF, $83, $24
					; loc_C1CDr
		.BYTE $24, $24,	0, $FF,	$FF, $83, $A1, $A3, $24, $FF, $FF, $83,	$A2, $A4, $A6, $FF
		.BYTE $FF, $83,	$24, $A5, $A7, 0, $FF, $FF, $83, $24, $A8, $24,	$FF, $FF, $83, $24
		.BYTE $A9, $24,	$FF, $FF, $83, $24, $AA, $24, 0, $FF, $FF, $83,	$24, $AD, $B0, $FF
		.BYTE $FF, $83,	$AB, $AE, $B1, $FF, $FF, $83, $AC, $AF,	$24, 0

; =============== S U B	R O U T	I N E =======================================


sub_C181:				; C122p
		LDA	$712,X
		BEQ	loc_C1C2
		LDA	$53,X
		BNE	loc_C18F
		LDA	#2
		STA	$712,X

loc_C18F:				; C188j
		DEC	$73F,X
		BNE	loc_C1C2

loc_C194:				; 980Cp
		LDA	#2
		STA	$73F,X
		DEC	$736,X
		LDA	$736,X
		BPL	loc_C1A6
		LDA	#3
		STA	$736,X

loc_C1A6:				; C19Fj
		TAY
		LDA	$712,X
		BPL	loc_C1B0
		LDY	#1
		BNE	loc_C1C5

loc_C1B0:				; C1AAj
		CMP	#2
		BNE	loc_C1B8
		LDY	#3
		BNE	loc_C1C5

loc_C1B8:				; C1B2j
		CMP	#3
		BNE	loc_C1C5
		LDA	#1
		STA	$712,X
		RTS
; ---------------------------------------------------------------------------

loc_C1C2:				; C184j C192j
		JMP	loc_C214
; ---------------------------------------------------------------------------

loc_C1C5:				; C1AEj C1B6j	...
		TXA
		PHA
		LDX	tbl_unk_C131,Y
		LDY	$300

loc_C1CD:				; C1D7j
		LDA	tbl_unk_C135,X
		STA	$301,Y
		BEQ	loc_C1DA
		INX
		INY
		JMP	loc_C1CD
; ---------------------------------------------------------------------------

loc_C1DA:				; C1D3j
		PLA
		TAX
		PHA
		LDA	$700,X
		STA	0
		LDA	$709,X
		STA	1
		LDX	$300
		LDY	#2

loc_C1EC:				; C1FEj
		LDA	0
		STA	$301,X
		LDA	1
		STA	$302,X
		INC	1
		TXA
		CLC
		ADC	#6
		TAX
		DEY
		BPL	loc_C1EC
		STX	$300
		PLA
		TAX
		LDA	$712,X
		CMP	#2
		BNE	loc_C214
		LDA	#0
		STA	$712,X
		STA	$53,X

locret_C213:				; C217j
		RTS
; ---------------------------------------------------------------------------

loc_C214:				; loc_C1C2j C20Aj
		LDA	$712,X
		BMI	locret_C213
		LDA	$72D,X
		CLC
		ADC	#$C
		STA	$74C
		LDA	#8
		LDY	$712,X
		BEQ	loc_C22B
		LDA	#$C

loc_C22B:				; C227j
		STA	$74B
		LDY	$76
		INY
		INY
		STY	$74A
		LDY	#0

loc_C237:				; C2C1j
		CPY	#2
		BCS	loc_C240
		LDA	$62,Y
		BEQ	loc_C2BB

loc_C240:				; C239j
		LDA	$3DA,Y
		BNE	loc_C2BB
		STA	$F
		LDA	$350,Y
		CLC
		ADC	#8
		SEC
		SBC	$74C
		BPL	loc_C258
		EOR	#$FF
		CLC
		ADC	#1

loc_C258:				; C251j
		CMP	$74B
		BPL	loc_C2BB
		LDA	$71B,X
		STA	0
		LDA	$724,X
		STA	1
		JSR	sub_CCB1
		LDA	4
		BNE	loc_C2BB
		LDA	5
		CMP	#$19
		BCS	loc_C2BB
		LDA	$712,X
		BNE	loc_C2B8
		LDA	$74F
		CMP	#2
		BCS	loc_C2C5
		TYA
		PHA
		TXA
		PHA
		LDY	#0
		LDX	$74D
		LDA	$712,X
		BEQ	loc_C28F
		INY

loc_C28F:				; C28Cj
		PLA
		TAX
		STA	$74D,Y
		PLA
		TAY
		LDA	#$20 ; ' '
		STA	$53,X
		INC	$712,X
		LDA	$74F
		BNE	loc_C2A9
		LDA	#1
		STA	$73F,X
		BNE	loc_C2B4

loc_C2A9:				; C2A0j
		TXA
		PHA
		TYA
		PHA
		JSR	sub_C317
		PLA
		TAY
		PLA
		TAX

loc_C2B4:				; C2A7j
		JSR	sub_C333
		RTS
; ---------------------------------------------------------------------------

loc_C2B8:				; C277j
		JSR	sub_C333

loc_C2BB:				; C23Ej C243j	...
		INY
		CPY	$74A
		BCS	locret_C2C4
		JMP	loc_C237
; ---------------------------------------------------------------------------

locret_C2C4:				; C2BFj
		RTS
; ---------------------------------------------------------------------------

loc_C2C5:				; C27Ej
		STX	0
		STY	1
		LDX	$74D
		LDY	$74E
		LDA	$53,X
		CMP	$53,Y
		BCS	loc_C2F2
		CMP	#$20 ; ' '
		BEQ	locret_C332
		LDA	$712,X
		BEQ	locret_C332
		CMP	#3
		BEQ	locret_C332
		LDA	#2
		STA	$712,X
		LDY	1
		LDX	0
		STX	$74D
		JMP	loc_C30E
; ---------------------------------------------------------------------------

loc_C2F2:				; C2D4j
		LDA	$53,Y
		CMP	#$20 ; ' '
		BEQ	locret_C332
		LDA	$712,Y
		BEQ	locret_C332
		CMP	#3
		BEQ	locret_C332
		LDA	#2
		STA	$712,Y
		LDY	1
		LDX	0
		STX	$74E

loc_C30E:				; C2EFj
		LDA	#3
		STA	$712,X
		LDA	#$20 ; ' '
		STA	$53,X
; End of function sub_C181


; =============== S U B	R O U T	I N E =======================================


sub_C317:				; C2ADp
		LDX	$74D
		LDY	$74E
		CPX	$74E
		BCS	loc_C328
		LDX	$74E
		LDY	$74D

loc_C328:				; C320j
		LDA	#2
		STA	$73F,X
		LDA	#1
		STA	$73F,Y

locret_C332:				; C2D8j C2DDj	...
		RTS
; End of function sub_C317


; =============== S U B	R O U T	I N E =======================================


sub_C333:				; loc_C2B4p loc_C2B8p
		LDA	#0
		STA	$3B5,Y
		LDA	$F
		BNE	loc_C340
		LDA	#$FE ; 'þ'
		BNE	loc_C342

loc_C340:				; C33Aj
		LDA	#2

loc_C342:				; C33Ej
		STA	$3A9,Y
		LDA	$3D2,Y
		CMP	#$20 ; ' '
		BCS	loc_C350
		LDA	#$3F ; '?'
		BNE	loc_C352

loc_C350:				; C34Aj
		LDA	#1

loc_C352:				; C34Ej
		STA	$3D2,Y
		LDA	$F1
		ORA	#2
		STA	$F1
		RTS
; End of function sub_C333

; ---------------------------------------------------------------------------
;	UNUSED DATA	.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF

; =============== S U B	R O U T	I N E =======================================

; Collisions.

sub_C370:				; loc_A57Fp A5EBp ...
		LDA	#0
		STA	$CB
		CPX	#9
		BCS	loc_C37B
		JSR	sub_C6B3

loc_C37B:				; C376j
		LDA	$36B,X
		BNE	loc_C38C
		JSR	sub_C67A
		JSR	sub_C61E
		LDA	$CB
		BNE	locret_C3C7
		BEQ	loc_C38F

loc_C38C:				; C37Ej
		JSR	sub_C69D

loc_C38F:				; C38Aj
		JSR	sub_C3EE
		LDA	$CB
		BNE	locret_C3C7
		JSR	sub_C49F
		JSR	sub_C51C
		JSR	sub_C5AA

loc_C39F:				; B105p loc_B2EFp ...
		LDA	$36B,X
		BEQ	locret_C3C7
		LDA	$37E,X
		CMP	#$98 ; '˜'
		BCC	locret_C3C7
		CPX	#8
		BCS	loc_C3B7
		LDA	$4A2,X
		ORA	#$20 ; ' '
		STA	$4A2,X

loc_C3B7:				; C3ADj
		LDA	$37E,X
		CMP	#$A0 ; ' '
		BCC	locret_C3C7
		CPX	#9
		BCS	loc_C3C8
		LDA	#$31 ; '1'
		STA	$3DA,X

locret_C3C7:				; C388j C394j	...
		RTS
; ---------------------------------------------------------------------------

loc_C3C8:				; C3C0j
		LDA	#$F
		STA	$7BC
		RTS
; End of function sub_C370


; =============== S U B	R O U T	I N E =======================================


sub_C3CE:				; A579p loc_B109p ...
		LDA	$36B,X
		BNE	loc_C3D7
		JSR	sub_C61E
		RTS
; ---------------------------------------------------------------------------

loc_C3D7:				; C3D1j
		LDA	$37E,X
		CMP	#$98 ; '˜'
		BEQ	loc_C3E8
		BCS	locret_C3E7
		CLC
		ADC	#3
		CMP	#$98 ; '˜'
		BCS	loc_C3E8

locret_C3E7:				; C3DEj C3F0j
		RTS
; ---------------------------------------------------------------------------

loc_C3E8:				; C3DCj C3E5j
		LDA	#$98 ; '˜'
		JSR	sub_C447
		RTS
; End of function sub_C3CE


; =============== S U B	R O U T	I N E =======================================

; Collisions.

sub_C3EE:				; loc_B2B9p loc_C38Fp

; FUNCTION CHUNK AT C499 SIZE 00000006 BYTES

		LDY	0
		BMI	locret_C3E7

loc_C3F2:				; C429j
		LDA	tbl_unk_C7CE,Y
		CMP	#$10
		BCC	loc_C3FC
		SEC
		SBC	#$C

loc_C3FC:				; C3F7j
		CMP	$350,X
		BEQ	loc_C403
		BCS	loc_C426

loc_C403:				; C3FFj
		LDA	tbl_unk_C851,Y
		CMP	#$F8 ; 'ø'
		BCS	loc_C40D
		SEC
		SBC	#4

loc_C40D:				; C408j
		CMP	$350,X
		BCC	loc_C426
		LDA	$A
		SEC
		SBC	#1
		CMP	tbl_unk_C6C8,Y
		BEQ	loc_C43B
		BCS	loc_C426
		CLC
		ADC	#3
		CMP	tbl_unk_C6C8,Y
		BCS	loc_C43B

loc_C426:				; C401j C410j	...
		INY
		CPY	1
		BNE	loc_C3F2
		CPX	#9
		BCS	locret_C49E
		LDA	$3CA,X
		BEQ	loc_C437
		JSR	sub_A812

loc_C437:				; C432j
		LDA	#0
		BEQ	loc_C495

loc_C43B:				; C41Aj C424j
		INC	$CB
		CPX	#9
		BCS	loc_C499
		LDA	tbl_unk_C6C8,Y
		SEC
		SBC	#$18
; End of function sub_C3EE


; =============== S U B	R O U T	I N E =======================================

; Collisions (under feets).

sub_C447:				; C3EAp
		STA	$37E,X
		LDA	#0
		STA	$391,X
		STA	$3A9,X
		LDA	#$40 ; '@'
		STA	$3B5,X
		LDA	$3D2,X
		CMP	#$20 ; ' '
		BEQ	loc_C472
		BCC	loc_C464
		LDA	#1
		BNE	loc_C466

loc_C464:				; C45Ej
		LDA	#2

loc_C466:				; C462j
		STA	$F
		LDA	$3C1,X
		AND	#$F0 ; 'ð'
		ORA	$F
		STA	$3C1,X

loc_C472:				; C45Cj
		CPX	#2
		BPL	loc_C480
		LDA	#$C
		STA	$41E,X
		LDA	#$34 ; '4'
		STA	$426,X

loc_C480:				; C474j
		CPX	#2
		BCS	loc_C493
		LDA	$3CA,X
		BNE	loc_C493
		CPX	$15
		BNE	loc_C493
		LDA	$F0
		ORA	#8
		STA	$F0

loc_C493:				; C482j C487j	...
		LDA	#1

loc_C495:				; C439j
		STA	$3CA,X
		RTS
; End of function sub_C447

; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_C3EE

loc_C499:				; C43Fj
		LDA	#2
		STA	$7BC

locret_C49E:				; C42Dj
		RTS
; END OF FUNCTION CHUNK	FOR sub_C3EE

; =============== S U B	R O U T	I N E =======================================

; Collision (above balloons).

sub_C49F:				; C396p
		LDY	0
		BMI	locret_C4DA

loc_C4A3:				; C4D8j
		LDA	$B
		SEC
		SBC	#1
		CMP	tbl_unk_C74B,Y
		BCS	loc_C4D5
		CLC
		ADC	#6
		CMP	tbl_unk_C74B,Y
		BCC	loc_C4D5
		LDA	tbl_unk_C7CE,Y
		CMP	#$10
		BCC	loc_C4BF
		SEC
		SBC	#$E

loc_C4BF:				; C4BAj
		CMP	$350,X
		BEQ	loc_C4C6
		BCS	loc_C4D5

loc_C4C6:				; C4C2j
		LDA	tbl_unk_C851,Y
		CMP	#$F8 ; 'ø'
		BCS	loc_C4D0
		SEC
		SBC	#3

loc_C4D0:				; C4CBj
		CMP	$350,X
		BCS	loc_C4DB

loc_C4D5:				; C4ABj C4B3j	...
		INY
		CPY	1
		BNE	loc_C4A3

locret_C4DA:				; C4A1j
		RTS
; ---------------------------------------------------------------------------

loc_C4DB:				; C4D3j
		INC	$CB
		CPX	#9
		BCS	loc_C516
		LDA	$3A9,X
		BPL	locret_C515
		LDA	#0
		SEC
		SBC	$3B5,X
		STA	$3B5,X
		LDA	#0
		SBC	$3A9,X
		STA	$3A9,X
		LSR	$3A9,X
		ROR	$3B5,X
		CPX	#2
		BMI	loc_C507
		LSR	$3A9,X
		ROR	$3B5,X

loc_C507:				; C4FFj loc_C64Bj
		CPX	#2
		BCS	locret_C515
		CPX	$15
		BNE	locret_C515
		LDA	$F1
		ORA	#2
		STA	$F1

locret_C515:				; C4E4j C509j	...
		RTS
; ---------------------------------------------------------------------------

loc_C516:				; C4DFj
		LDA	#1
		STA	$7BC
		RTS
; End of function sub_C49F


; =============== S U B	R O U T	I N E =======================================

; Collisions (left).

sub_C51C:				; B2C5p C399p
		LDY	0
		BMI	loc_C54D

loc_C520:				; C54Bj
		LDA	$B
		CMP	tbl_unk_C74B,Y
		BCS	loc_C548
		CMP	#$E8 ; 'è'
		BCS	loc_C532
		LDA	$A
		CMP	tbl_unk_C6C8,Y
		BCC	loc_C548

loc_C532:				; C529j
		LDA	tbl_unk_C851,Y
		CMP	#$FF
		BEQ	loc_C548
		LDA	$D
		CMP	tbl_unk_C851,Y
		BCS	loc_C548
		CLC
		ADC	#4
		CMP	tbl_unk_C851,Y
		BCS	loc_C55B

loc_C548:				; C525j C530j	...
		INY
		CPY	1
		BNE	loc_C520

loc_C54D:				; C51Ej
		INC	$CB
		CPX	#9
		BCC	locret_C55A
		LDA	$350,X
		CMP	#3
		BCC	loc_C59C

locret_C55A:				; C551j
		RTS
; ---------------------------------------------------------------------------

loc_C55B:				; C546j
		CPX	#9
		BCS	loc_C59C
		LDA	$3DA,X
		BNE	loc_C5A5
		LDY	#1
		LDA	$3D2,X
		CMP	#$21 ; '!'
		BCS	loc_C581
		CMP	#$20 ; ' '
		BNE	loc_C578
		LDA	#$22 ; '"'
		STA	$3D2,X
		BNE	loc_C581

loc_C578:				; C56Fj
		LDA	#$40 ; '@'
		SEC
		SBC	$3D2,X
		STA	$3D2,X

loc_C581:				; C56Bj C576j	...
		TYA
		STA	$3C1,X
		CPX	#2
		BCS	locret_C59B
		CPX	$15
		BNE	locret_C59B
		LDA	$47,X
		BNE	locret_C59B
		LDA	#$10
		STA	$47,X
		LDA	$F1
		ORA	#2
		STA	$F1

locret_C59B:				; C587j C58Bj	...
		RTS
; ---------------------------------------------------------------------------

loc_C59C:				; C558j C55Dj
		LDA	$7BC
		ORA	#4
		STA	$7BC
		RTS
; ---------------------------------------------------------------------------

loc_C5A5:				; C562j
		LDA	#2
		STA	$E
		RTS
; End of function sub_C51C


; =============== S U B	R O U T	I N E =======================================

; Collisions (right).

sub_C5AA:				; B2C8p C39Cp
		LDY	0
		BMI	loc_C5D9

loc_C5AE:				; C5D7j
		LDA	tbl_unk_C7CE,Y
		BEQ	loc_C5D4
		LDA	$B
		CMP	tbl_unk_C74B,Y
		BCS	loc_C5D4
		CMP	#$E8 ; 'è'
		BCS	loc_C5C5
		LDA	$A
		CMP	tbl_unk_C6C8,Y
		BCC	loc_C5D4

loc_C5C5:				; C5BCj
		LDA	$C
		CMP	tbl_unk_C7CE,Y
		BCC	loc_C5D4
		SEC
		SBC	#4
		CMP	tbl_unk_C7CE,Y
		BCC	loc_C5E7

loc_C5D4:				; C5B1j C5B8j	...
		INY
		CPY	1
		BNE	loc_C5AE

loc_C5D9:				; C5ACj
		INC	$CB
		CPX	#9
		BCC	locret_C5E6
		LDA	$350,X
		CMP	#$FD ; 'ý'
		BCS	loc_C610

locret_C5E6:				; C5DDj
		RTS
; ---------------------------------------------------------------------------

loc_C5E7:				; C5D2j
		CPX	#9
		BCS	loc_C610
		LDA	$3DA,X
		BNE	loc_C619
		LDA	$3D2,X
		CMP	#$20 ; ' '
		BCC	loc_C60B
		CMP	#$20 ; ' '
		BNE	loc_C602
		LDA	#$1E
		STA	$3D2,X
		BNE	loc_C60B

loc_C602:				; C5F9j
		LDA	#$40 ; '@'
		SEC
		SBC	$3D2,X
		STA	$3D2,X

loc_C60B:				; C5F5j C600j
		LDY	#2
		JMP	loc_C581
; ---------------------------------------------------------------------------

loc_C610:				; C5E4j C5E9j
		LDA	$7BC
		ORA	#8
		STA	$7BC
		RTS
; ---------------------------------------------------------------------------

loc_C619:				; C5EEj
		LDA	#1
		STA	$E
		RTS
; End of function sub_C5AA


; =============== S U B	R O U T	I N E =======================================

; Upper	bound checking.

sub_C61E:				; C383p C3D3p
		LDA	$37E,X
		CMP	#$10
		BCS	locret_C64E
		INC	$CB
		CPX	#9
		BCS	loc_C64F
		LDA	#$10
		STA	$37E,X
		LDA	#0
		SEC
		SBC	$3B5,X
		STA	$3B5,X
		LDA	#0
		SBC	$3A9,X
		STA	$3A9,X
		CPX	#2
		BCC	loc_C64B
		LSR	$3A9,X
		ROR	$3B5,X

loc_C64B:				; C643j
		JMP	loc_C507
; ---------------------------------------------------------------------------

locret_C64E:				; C623j
		RTS
; ---------------------------------------------------------------------------

loc_C64F:				; C629j
		LDA	#1
		STA	$7BC
		RTS
; End of function sub_C61E

; ---------------------------------------------------------------------------
tbl_unk_C655:	.BYTE	0		; loc_C685r
					; Array	size: 19 bytes
tbl_unk_C656:	.BYTE 3, 4, 6, $B, $11,	$17, $1C, $1F, $27, $2B, $2C, $2F, $32,	$34, $3A, $3D
					; C68Ar
		.BYTE $41, $42
tbl_unk_C668:	.BYTE $42		; loc_C6A8r
					; Array	size: 18 bytes
tbl_unk_C669:	.BYTE $44, $45,	$48, $4D, $51, $56, $5A, $5D, $60, $64,	$6A, $6E, $73, $78, $7C, $7E
					; C6ADr
		.BYTE $83

; =============== S U B	R O U T	I N E =======================================


sub_C67A:				; B2B0p C380p
		LDY	#0
		LDA	$37E,X
		CMP	#$BC ; '¼'
		BCS	loc_C685
		LDY	$9E

loc_C685:				; C681j
		LDA	tbl_unk_C655,Y
		STA	0
		LDA	tbl_unk_C656,Y
		CPY	#0
		BNE	loc_C69A
		LDY	$9E
		CPY	#3
		BEQ	loc_C69A
		SEC
		SBC	#1

loc_C69A:				; C68Fj C695j
		STA	1
		RTS
; End of function sub_C67A


; =============== S U B	R O U T	I N E =======================================


sub_C69D:				; loc_B2B6p loc_C38Cp
		LDY	#0
		LDA	$37E,X
		CMP	#$7C ; '|'
		BCS	loc_C6A8
		LDY	$9F

loc_C6A8:				; C6A4j
		LDA	tbl_unk_C668,Y
		STA	0
		LDA	tbl_unk_C669,Y
		STA	1
		RTS
; End of function sub_C69D


; =============== S U B	R O U T	I N E =======================================


sub_C6B3:				; loc_B2A8p C378p
		LDA	$37E,X
		STA	$B
		CLC
		ADC	#$18
		STA	$A
		LDA	$350,X
		STA	$D
		CLC
		ADC	#$10
		STA	$C
		RTS
; End of function sub_C6B3

; ---------------------------------------------------------------------------
tbl_unk_C6C8:	.BYTE $D8, $D8,	$E8, $78, $48, $48, $38, $78, $78, $A0,	$A0, $28, $28, $58, $58, $88
					; C417r C421r	...
		.BYTE $88, $40,	$40, $40, $40, $90, $90, $50, $50, $50,	$50, $A8, $48, $90, $90, $38 ; Table size of 131 bytes.
		.BYTE $38, $40,	$40, $40, $40, $98, $98, $50, $50, $58,	$58, $48, $40, $70, $70, $68
		.BYTE $68, $B0,	$68, $68, $30, $30, $70, $70, $A8, $A8,	$78, $78, $98, $30, $30, $A0
		.BYTE $A0, $00,	$98, $98, $48, $48, $48, $68, $20, $38,	$38, $70, $70, $20, $20, $20
		.BYTE $20, $20,	$20, $20, $20, $48, $20, $20, $40, $40,	$20, $40, $40, $28, $28, $68
		.BYTE $20, $20,	$20, $20, $20, $20, $20, $20, $48, $48,	$20, $20, $40, $40, $20, $20
		.BYTE $20, $20,	$68, $28, $28, $58, $58, $68, $20, $20,	$68, $68, $68, $68, $20, $20
		.BYTE $20, $20,	$68
tbl_unk_C74B:	.BYTE $F4, $F4,	$F0, $80, $50, $50, $40, $80, $80, $A8,	$A8, $30, $50, $60, $80, $90
					; C4A8r C4B0r	...
		.BYTE $B0, $48,	$80, $48, $80, $98, $B0, $58, $A0, $58,	$A0, $B0, $50, $98, $98, $40 ; Table size of 131 bytes.
		.BYTE $40, $48,	$80, $48, $80, $A0, $A0, $58, $58, $60,	$A0, $50, $48, $78, $78, $70
		.BYTE $70, $B8,	$70, $70, $38, $58, $78, $78, $B0, $C0,	$80, $80, $A0, $38, $70, $A8
		.BYTE $A8, $00,	$C0, $C0, $50, $50, $50, $70, $28, $40,	$40, $78, $78, $28, $50, $28
		.BYTE $50, $28,	$50, $28, $50, $50, $28, $50, $48, $48,	$28, $48, $48, $30, $30, $70
		.BYTE $28, $60,	$28, $60, $28, $50, $28, $50, $50, $50,	$28, $50, $48, $48, $28, $50
		.BYTE $28, $50,	$70, $30, $30, $60, $60, $70, $28, $28,	$70, $70, $70, $70, $28, $60
		.BYTE $28, $60,	$70
tbl_unk_C7CE:	.BYTE $00, $BC,	$7C, $5C, $44, $A4, $5C, $00, $E4, $3C,	$A4, $5C, $62, $9C, $A2, $5C
					; loc_C3F2r C4B5r ...
		.BYTE $62, $44,	$4A, $A4, $AA, $74, $7A, $2C, $32, $C4,	$CA, $74, $54, $24, $A4, $24 ; Table size of 131 bytes.
		.BYTE $C4, $54,	$52, $A4, $AA, $00, $D4, $44, $9C, $74,	$7A, $54, $6C, $54, $9C, $3C
		.BYTE $B4, $7C,	$24, $C4, $74, $7A, $44, $A4, $74, $7A,	$14, $D4, $74, $74, $7A, $44
		.BYTE $B4, $00,	$00, $C4, $54, $44, $A4, $74, $64, $2C,	$C4, $4C, $A4, $4C, $52, $A4
		.BYTE $AA, $34,	$3A, $C4, $CA, $6C, $7C, $82, $3C, $B4,	$6C, $00, $CC, $34, $A4, $5C
		.BYTE $3C, $42,	$9C, $BA, $34, $52, $A4, $AA, $00, $DC,	$74, $7A, $34, $A4, $14, $1A
		.BYTE $D4, $E2,	$64, $00, $BC, $00, $E4, $64, $24, $C4,	$4C, $A4, $4C, $9C, $34, $3A
		.BYTE $BC, $C2,	$74
tbl_unk_C851:	.BYTE $44, $FF,	$84, $A4, $5C, $BC, $A4, $1C, $FF, $5C,	$C4, $6C, $66, $AC, $A6, $6C
					; loc_C403r loc_C4C6r	...
		.BYTE $66, $54,	$4E, $B4, $AE, $84, $7E, $3C, $36, $D4,	$CE, $8C, $AC, $5C, $DC, $3C ; Table size of 131 bytes.
		.BYTE $DC, $5C,	$56, $AC, $AE, $2C, $FF, $5C, $B4, $84,	$7E, $AC, $94, $64, $AC, $4C
		.BYTE $C4, $84,	$3C, $DC, $84, $7E, $54, $B4, $84, $7E,	$2C, $EC, $8C, $84, $7E, $54
		.BYTE $C4, $00,	$3C, $FF, $AC, $5C, $BC, $8C, $9C, $3C,	$D4, $5C, $B4, $5C, $56, $B4
		.BYTE $AE, $44,	$3E, $D4, $CE, $94, $8C, $86, $54, $CC,	$94, $34, $FF, $5C, $CC, $A4
		.BYTE $64, $46,	$C4, $BE, $5C, $56, $CC, $AE, $24, $FF,	$84, $7E, $54, $C4, $2C, $1E
		.BYTE $EC, $E6,	$9C, $44, $FF, $1C, $FF, $9C, $3C, $DC,	$64, $BC, $64, $B4, $44, $3E
		.BYTE $CC, $C6,	$8C
;unknown_data01:	.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF

; =============== S U B	R O U T	I N E =======================================


sub_C8F0:				; A4E0p
		LDA	$65
		BNE	loc_C8F5
		RTS
; ---------------------------------------------------------------------------

loc_C8F5:				; C8F2j
		LDX	#0
		LDY	#1
		LDA	$3DA,X
		BEQ	loc_C905
		AND	#$F0 ; 'ð'
		BEQ	loc_C905

loc_C902:				; C90Cj C91Ej	...
		JMP	loc_C9C5
; ---------------------------------------------------------------------------

loc_C905:				; C8FCj C900j
		LDA	$3DA,Y
		BEQ	loc_C90E
		AND	#$F0 ; 'ð'
		BNE	loc_C902

loc_C90E:				; C908j
		LDA	$350,X
		SEC
		SBC	$350,Y
		BPL	loc_C91C
		EOR	#$FF
		CLC
		ADC	#1

loc_C91C:				; C915j
		CMP	#$E
		BCS	loc_C902
		JSR	sub_CCA3
		LDA	4
		BNE	loc_C902
		LDA	5
		CMP	#$19
		BCS	loc_C902
		CMP	#5
		BCC	loc_C988
		LDA	$F
		BNE	loc_C93B
		TXA
		PHA
		TYA
		TAX
		PLA
		TAY

loc_C93B:				; C933j
		JSR	sub_CCEF
		LDA	$6C
		BEQ	loc_C94E
		LDA	$3D2,X
		CMP	#$20 ; ' '
		BNE	loc_C94E
		LDA	#$3A ; ':'
		STA	$3D2,X

loc_C94E:				; C940j C947j
		TXA
		PHA
		TYA
		TAX
		PLA
		TAY
		LDA	$3DA,X
		BNE	loc_C985
		LDA	#$FF
		STA	$416,X
		LDA	#1
		STA	$3EA,X
		DEC	$3E2,X
		LDA	$3E2,X
		STA	$42E,X
		BPL	loc_C973
		LDA	#0
		STA	$3E2,X

loc_C973:				; C96Cj
		LDA	$F0
		ORA	#2
		STA	$F0
		LDA	$6C
		BNE	loc_C985
		LDA	#0
		STA	$495
		JSR	sub_D0D6

loc_C985:				; C957j C97Bj
		JMP	loc_C9C5
; ---------------------------------------------------------------------------

loc_C988:				; C92Fj
		LDA	$3CA,X
		BNE	loc_C9CD
		LDA	$3CA,Y
		BNE	loc_C9CD

loc_C992:				; C9D3j
		LDA	$F1
		ORA	#2
		STA	$F1
		LDA	$350,X
		SEC
		SBC	$350,Y
		BPL	loc_C9A7
		TXA
		PHA
		TYA
		TAX
		PLA
		TAY

loc_C9A7:				; C99Fj
		LDA	$3DA,X
		BNE	loc_C9B6
		LDA	#$34 ; '4'
		STA	$3D2,X
		LDA	#1
		STA	$3C1,X

loc_C9B6:				; C9AAj
		LDA	$3DA,Y
		BNE	loc_C9C5
		LDA	#$E
		STA	$3D2,Y
		LDA	#2
		STA	$3C1,Y

loc_C9C5:				; loc_C902j loc_C985j	...
		LDA	#0
		STA	$8C,X
		STA	$8C,Y
		RTS
; ---------------------------------------------------------------------------

loc_C9CD:				; C98Bj C990j
		LDA	$3DA,X
		ORA	$3DA,Y
		BNE	loc_C992
		LDA	$82,X
		ORA	$82,Y
		BEQ	loc_C9C5
		LDA	$8C,X
		CMP	#$FF
		BEQ	loc_C9E6
		LDA	$82,X
		BEQ	loc_C9F9

loc_C9E6:				; C9E0j
		LDA	$8C,Y
		CMP	#$FF
		BEQ	loc_C9F2
		LDA	$82,Y
		BEQ	loc_C9F9

loc_C9F2:				; C9EBj
		LDA	$8C,X
		ORA	$8C,Y
		BNE	locret_CA54

loc_C9F9:				; C9E4j C9F0j
		LDA	#0
		STA	$8C,X
		STA	$8C,Y
		LDA	$82,X
		BNE	loc_CA08
		LDA	#$FF
		BNE	loc_CA18

loc_CA08:				; CA02j
		LDA	$350,X
		SEC
		SBC	$350,Y
		BPL	loc_CA16
		LDA	#1
		JMP	loc_CA18
; ---------------------------------------------------------------------------

loc_CA16:				; CA0Fj
		LDA	#2

loc_CA18:				; CA06j CA13j
		STA	$8C,X
		LDA	$82,Y
		BNE	loc_CA23
		LDA	#$FF
		BNE	loc_CA33

loc_CA23:				; CA1Dj
		LDA	$350,Y
		SEC
		SBC	$350,X
		BPL	loc_CA31
		LDA	#1
		JMP	loc_CA33
; ---------------------------------------------------------------------------

loc_CA31:				; CA2Aj
		LDA	#2

loc_CA33:				; CA21j CA2Ej
		STA	$8C,Y
		ORA	$8C,X
		CMP	#$FF
		BEQ	locret_CA54
		LDA	$F1
		ORA	#2
		STA	$F1
		LDA	#$40 ; '@'
		SEC
		SBC	$3D2,X
		STA	$3D2,X
		LDA	#$40 ; '@'
		SEC
		SBC	$3D2,Y
		STA	$3D2,Y

locret_CA54:				; C9F7j CA3Aj
		RTS
; End of function sub_C8F0


; =============== S U B	R O U T	I N E =======================================


sub_CA55:				; A5EEp
		LDA	$3DA,X
		BEQ	loc_CA5B
		RTS
; ---------------------------------------------------------------------------

loc_CA5B:				; CA58j
		TXA
		TAY
		DEY

loc_CA5E:				; CB65j
		CPY	#2
		BCS	loc_CA67
		LDA	$62,Y
		BEQ	loc_CA82

loc_CA67:				; CA60j
		LDA	$3DA,Y
		BEQ	loc_CA70
		AND	#$F0 ; 'ð'
		BNE	loc_CA82

loc_CA70:				; CA6Aj
		LDA	$350,X
		SEC
		SBC	$350,Y
		BPL	loc_CA7E
		EOR	#$FF
		CLC
		ADC	#1

loc_CA7E:				; CA77j
		CMP	#$E
		BCC	loc_CA85

loc_CA82:				; CA65j CA6Ej	...
		JMP	loc_CB62
; ---------------------------------------------------------------------------

loc_CA85:				; CA80j
		JSR	sub_CCA3
		LDA	4
		BNE	loc_CA82
		LDA	5
		CMP	#$19
		BCS	loc_CA82
		CPY	#2
		BCC	loc_CA99
		JMP	loc_CB0C
; ---------------------------------------------------------------------------

loc_CA99:				; CA94j
		CMP	#5
		BCS	loc_CAA0
		JMP	loc_CB06
; ---------------------------------------------------------------------------

loc_CAA0:				; CA9Bj
		LDA	$F
		BNE	loc_CAD2
		STX	0
		STY	1
		LDX	1
		LDY	0
		JSR	sub_CCEF
		LDX	0
		LDY	1
		TYA
		STA	$574,X
		JSR	sub_D0D6
		LDA	#1
		STA	$3EA,X
		DEC	$3E2,X
		LDA	$3E2,X
		STA	$42E,X
		BPL	loc_CACF
		LDA	#0
		STA	$3E2,X

loc_CACF:				; CAC8j
		JMP	loc_CAFD
; ---------------------------------------------------------------------------

loc_CAD2:				; CAA2j
		JSR	sub_CCEF
		LDA	$3DA,Y
		BNE	loc_CB03
		TXA
		PHA
		TYA
		PHA
		TAX
		LDA	#$FF
		STA	$416,X
		LDA	#1
		STA	$3EA,X
		DEC	$3E2,X
		LDA	$3E2,X
		STA	$42E,X
		BPL	loc_CAF9
		LDA	#0
		STA	$3E2,X

loc_CAF9:				; CAF2j
		PLA
		TAY
		PLA
		TAX

loc_CAFD:				; loc_CACFj
		LDA	$F0
		ORA	#2
		STA	$F0

loc_CB03:				; CAD8j
		JMP	loc_CB62
; ---------------------------------------------------------------------------

loc_CB06:				; CA9Dj
		LDA	$F1
		ORA	#2
		STA	$F1

loc_CB0C:				; CA96j
		LDA	$3D2,Y
		CMP	#$20 ; ' '
		BCS	loc_CB20
		LDA	$350,Y
		SEC
		SBC	$350,X
		BMI	loc_CB37
		LDA	#1
		BNE	loc_CB2B

loc_CB20:				; CB11j
		LDA	$350,Y
		SEC
		SBC	$350,X
		BPL	loc_CB37
		LDA	#2

loc_CB2B:				; CB1Ej
		STA	$3C1,Y
		LDA	#$40 ; '@'
		SEC
		SBC	$3D2,Y
		STA	$3D2,Y

loc_CB37:				; CB1Aj CB27j
		LDA	$3D2,X
		CMP	#$20 ; ' '
		BCS	loc_CB4B
		LDA	$350,X
		SEC
		SBC	$350,Y
		BMI	loc_CB62
		LDA	#1
		BNE	loc_CB56

loc_CB4B:				; CB3Cj
		LDA	$350,X
		SEC
		SBC	$350,Y
		BPL	loc_CB62
		LDA	#2

loc_CB56:				; CB49j
		STA	$3C1,X
		LDA	#$40 ; '@'
		SEC
		SBC	$3D2,X
		STA	$3D2,X

loc_CB62:				; loc_CA82j loc_CB03j	...
		DEY
		BMI	locret_CB68
		JMP	loc_CA5E
; ---------------------------------------------------------------------------

locret_CB68:				; CB63j
		RTS
; End of function sub_CA55


; =============== S U B	R O U T	I N E =======================================


sub_CB69:				; A6ECp A743p
		TXA
		PHA
		LDY	#1

loc_CB6D:				; CBC1j
		LDA	$62,Y
		BEQ	loc_CBC0
		LDA	$3DA,Y
		BNE	loc_CBC0
		LDA	$350,X
		SEC
		SBC	$350,Y
		BPL	loc_CB85
		EOR	#$FF
		CLC
		ADC	#1

loc_CB85:				; CB7Ej
		CMP	#$C
		BCS	loc_CBC0
		JSR	sub_CCA3
		LDA	4
		BNE	loc_CBC0
		LDA	$F
		BNE	loc_CB98
		LDA	#$12
		BNE	loc_CB9A

loc_CB98:				; CB92j
		LDA	#$C

loc_CB9A:				; CB96j
		CMP	5
		BCC	loc_CBC0
		LDA	$F0
		ORA	#2
		STA	$F0
		sta bubble_flag
		LDA	$3DA,X
		AND	#$F0 ; 'ð'
		CMP	#$C0 ; 'À'
		BNE	loc_CBB7
		TYA
		STA	$574,X
		JSR	sub_D0D6
		JMP	loc_CBBB
; ---------------------------------------------------------------------------

loc_CBB7:				; CBABj
		TYA
		TAX
		INC	$97,X

loc_CBBB:				; CBB4j
		PLA
		TAX
		LDA	#1
		RTS
; ---------------------------------------------------------------------------

loc_CBC0:				; CB70j CB75j	...
		DEY
		BPL	loc_CB6D
		PLA
		TAX
		LDA	#0
		RTS
; End of function sub_CB69


; =============== S U B	R O U T	I N E =======================================


sub_CBC8:				; A64Dp
		LDY	#1

loc_CBCA:				; CC18j
		LDA	$62,Y
		BEQ	loc_CC17
		LDA	$3DA,Y
		BNE	loc_CC17
		STA	$F
		LDA	$37D
		STA	0
		LDA	$390
		STA	1
		JSR	sub_CCB1
		LDA	4
		BNE	loc_CC17
		LDA	$F
		BNE	loc_CC17
		LDA	5
		CMP	#$15
		BCS	loc_CC17
		LDA	$362
		SEC
		SBC	$350,Y
		BPL	loc_CBFF
		EOR	#$FF
		CLC
		ADC	#1

loc_CBFF:				; CBF8j
		CMP	#$10
		BCS	loc_CC17
		LDA	#$FF
		STA	$416,Y
		LDA	#0
		STA	$3E2,Y
		STA	$42E,Y
		LDA	$F0
		ORA	#2
		STA	$F0
		RTS
; ---------------------------------------------------------------------------

loc_CC17:				; CBCDj CBD2j	...
		DEY
		BPL	loc_CBCA
		RTS
; End of function sub_CBC8


; =============== S U B	R O U T	I N E =======================================


sub_CC1B:				; sub_B296p loc_B366p	...
		STX	$D
		LDA	#0
		STA	$E
		LDY	#1

loc_CC23:				; CC30j
		LDA	$62,Y
		BEQ	loc_CC2B
		JSR	sub_CC43

loc_CC2B:				; CC26j
		LDA	$E
		BNE	loc_CC40
		DEY
		BPL	loc_CC23
		LDY	#2

loc_CC34:				; CC3Ej
		JSR	sub_CC43
		LDA	#0
		STA	$E
		INY
		CPY	#8
		BCC	loc_CC34

loc_CC40:				; CC2Dj
		LDX	$D
		RTS
; End of function sub_CC1B


; =============== S U B	R O U T	I N E =======================================


sub_CC43:				; CC28p loc_CC34p
		CPY	$D
		BEQ	loc_CC4C
		LDA	$3DA,Y
		BEQ	loc_CC4F

loc_CC4C:				; CC45j
		JMP	locret_CCA2
; ---------------------------------------------------------------------------

loc_CC4F:				; CC4Aj
		LDA	$350,Y
		SEC
		SBC	$350,X
		BPL	loc_CC5D
		EOR	#$FF
		CLC
		ADC	#1

loc_CC5D:				; CC56j
		CMP	#$C
		BCS	locret_CCA2
		JSR	sub_CCA3
		LDA	4
		BNE	locret_CCA2
		LDA	5
		CMP	#$10
		BCS	locret_CCA2
		CPY	#2
		BCS	loc_CC77
		LDA	$3CA,X
		BNE	loc_CC93

loc_CC77:				; CC70j
		STX	0
		STY	1
		LDA	$F
		BNE	loc_CC83
		LDX	1
		LDY	0

loc_CC83:				; CC7Dj
		JSR	sub_CCEF
		LDX	0
		LDY	1
		LDA	$3CA,X
		BNE	loc_CC93
		LDA	$F
		BNE	locret_CCA2

loc_CC93:				; CC75j CC8Dj
		CPY	#2
		BCS	locret_CCA2
		TYA
		STA	$574,X
		JSR	sub_D0D6
		LDA	#1
		STA	$E

locret_CCA2:				; loc_CC4Cj CC5Fj ...
		RTS
; End of function sub_CC43


; =============== S U B	R O U T	I N E =======================================


sub_CCA3:				; B8BCp BE23p	...
		LDA	#0
		STA	$F
		LDA	$36B,X
		STA	0
		LDA	$37E,X
		STA	1
; End of function sub_CCA3


; =============== S U B	R O U T	I N E =======================================

sub_CCB1:				; C267p CBE0p
		LDA	$36B,Y
		STA	2
		LDA	$37E,Y
		STA	3
		LDA	0
		CMP	2
		BEQ	loc_CCC5
		BCC	loc_CCCB
		BCS	loc_CCE1

loc_CCC5:				; CCBFj
		LDA	1
		CMP	3
		BCS	loc_CCE1

loc_CCCB:				; CCC1j
		LDA	1
		PHA
		LDA	0
		PHA
		LDA	2
		STA	0
		LDA	3
		STA	1
		PLA
		STA	2
		PLA
		STA	3
		INC	$F

loc_CCE1:				; CCC3j CCC9j
		LDA	1
		SEC
		SBC	3
		STA	5
		LDA	0
		SBC	2
		STA	4
		RTS
; End of function sub_CCB1


; =============== S U B	R O U T	I N E =======================================

sub_CCEF:				; loc_C93Bp CAACp ...
		LDA	#0
		STA	$3B5,X
		STA	$3B5,Y
		LDA	#$FF
		STA	$3A9,X
		LDA	#1
		STA	$3A9,Y
		JSR	sub_CD10
		TXA
		PHA
		TYA
		TAX
		JSR	sub_CD10
		TXA
		TAY
		PLA
		TAX
		RTS
; End of function sub_CCEF


; =============== S U B	R O U T	I N E =======================================


sub_CD10:				; CD01p CD08p
		LDA	$3DA,X
		BNE	locret_CD33
		TYA
		PHA
		LDA	$3D2,X
		CMP	#$20 ; ' '
		BEQ	loc_CD31
		BCS	loc_CD26
		LDA	#$28 ; '('
		LDY	#1
		BNE	loc_CD2A

loc_CD26:				; CD1Ej
		LDA	#$18
		LDY	#2

loc_CD2A:				; CD24j
		STA	$3D2,X
		TYA
		STA	$3C1,X

loc_CD31:				; CD1Cj
		PLA
		TAY

locret_CD33:				; CD13j
		RTS
; End of function sub_CD10


; =============== S U B	R O U T	I N E =======================================


nullsub_1:				; 97F0p
		RTS
; End of function nullsub_1


; =============== S U B	R O U T	I N E =======================================


sub_CD35:				; loc_A5A3p A61Cp
		JSR	sub_B785
		RTS
; End of function sub_CD35


; =============== S U B	R O U T	I N E =======================================


sub_CD40:				; 972Bp
		LDA	1
		STA	2
		LDA	#$7C ; '|'
		STA	3
		LDX	#0

loc_CD4A:				; CD6Fj
		LDA	0
		STA	$301,X
		LDA	1
		STA	$302,X
		INC	1
		LDA	#$84 ; '„'
		STA	$303,X
		LDY	#0

loc_CD5D:				; CD68j
		LDA	3
		STA	$304,X
		INC	3
		INX
		INY
		CPY	#4
		BCC	loc_CD5D
		INX
		INX
		INX
		CPX	#$1C
		BCC	loc_CD4A
		LDX	#$23 ; '#'
		LDA	0
		AND	#8
		BEQ	loc_CD7B
		LDX	#$2B ; '+'

loc_CD7B:				; CD77j
		STX	$31D
		LDA	2
		LSR	A
		LSR	A
		AND	#7
		PHA
		LDA	0
		AND	#3
		STA	0
		LDA	2
		AND	#$80 ; '€'
		STA	2
		LDY	#3

loc_CD93:				; CD98j
		LSR	0
		ROR	2
		DEY
		BPL	loc_CD93
		PLA
		CLC
		ADC	2
		CLC
		ADC	#$C0 ; 'À'
		STA	$31E
		LDA	#1
		STA	$31F
		LDA	#$FF
		STA	$320
		LDA	#0
		STA	$321
		RTS
; End of function sub_CD40

; ---------------------------------------------------------------------------
tbl_unk_CDB4:	.BYTE $D0		; CE24r CE3Ar	...
		.BYTE $E0
tbl_unk_CDB6:	.BYTE $D1		; CE2Fr CE51r
		.BYTE $E1

; =============== S U B	R O U T	I N E =======================================


sub_CDB8:				; A59Dp
		LDA	$69
		CMP	#$A
		BEQ	locret_CE00
		LDA	$3DA,X
		BNE	loc_CDF9
		LDA	$7CF
		SEC
		SBC	$13
		STA	$7CE
		LDA	$36B,X
		STA	6
		LDA	$37E,X
		STA	7
		LDA	$17
		ORA	$19
		AND	#8
		BEQ	loc_CE01
		LDA	#$88 ; 'ˆ'
		JSR	sub_CF2A
		BMI	loc_CE01
		LDY	#$FF
		LDA	$3CA,X
		BNE	loc_CDF3
		LDA	$7CE
		BEQ	loc_CDF3
		BMI	loc_CDF9

loc_CDF3:				; CDEAj CDEFj
		LDY	#$FE ; 'þ'
		LDA	$7A
		BNE	loc_CE29

loc_CDF9:				; CDC1j CDF1j	...
		LDA	$13
		STA	$7CF
		LDX	$15

locret_CE00:				; CDBCj
		RTS
; ---------------------------------------------------------------------------

loc_CE01:				; CDDCj CDE3j
		LDA	$17
		ORA	$19
		AND	#4
		BEQ	loc_CDF9
		LDA	#$46 ; 'F'
		JSR	sub_CF2A
		BPL	loc_CDF9
		LDY	#1
		LDA	$3CA,X
		BNE	loc_CE1E
		LDA	$7CE
		BEQ	loc_CE20
		BPL	loc_CDF9

loc_CE1E:				; CE15j
		LDY	#2

loc_CE20:				; CE1Aj
		LDX	$6C
		LDA	$7A
		CMP	tbl_unk_CDB4,X
		BCS	loc_CDF9

loc_CE29:				; CDF7j
		TYA
		CLC
		ADC	$7A
		LDX	$6C
		CMP	tbl_unk_CDB6,X
		BCC	loc_CE3E
		LDY	#0
		CMP	#$F0 ; 'ð'
		BCS	loc_CE3D
		LDY	tbl_unk_CDB4,X

loc_CE3D:				; CE38j
		TYA

loc_CE3E:				; CE32j
		PHA
		LDA	$13
		STA	$7CF
		PLA
		LDX	$15
		JMP	sub_CF06
; End of function sub_CDB8


; =============== S U B	R O U T	I N E =======================================


sub_CE4A:				; A59Ap sub_B12Ep
		LDY	$6C
		LDA	tbl_unk_CDB4,Y
		STA	$A
		LDA	tbl_unk_CDB6,Y
		STA	$B
		LDA	$7D,X
		BNE	locret_CEBE
		LDA	$36B,X
		STA	6
		LDA	$37E,X
		STA	7
		LDA	$36B,X
		CMP	$77
		BNE	loc_CE72
		LDA	$37E,X
		CMP	$78
		BEQ	loc_CEBB

loc_CE72:				; CE69j
		LDA	$78
		STA	1
		LDA	$77
		JSR	sub_CF37
		LDA	$E
		ORA	$F
		BEQ	locret_CEBE
		LDA	$E
		BMI	loc_CEC6
		LDY	#$FD ; 'ý'
		LDA	#$18
		JSR	sub_CF2A
		BPL	loc_CEAB
		INY
		LDA	#$30 ; '0'
		JSR	sub_CF2A
		BPL	loc_CEAB
		INY
		LDA	#$38 ; '8'
		JSR	sub_CF2A
		BPL	loc_CEAB
		LDA	#$40 ; '@'
		JSR	sub_CF2A
		BMI	loc_CEBF
		LDA	$64
		AND	#1
		BNE	loc_CEBB

loc_CEAB:				; CE8Cj CE94j	...
		STY	0
		LDA	$7A
		CLC
		ADC	0
		CMP	$B
		BCC	loc_CEB8
		LDA	#0

loc_CEB8:				; CEB4j
		JSR	sub_CF06

loc_CEBB:				; CE70j CEA9j	...
		JSR	sub_CF1B

locret_CEBE:				; CE58j CE7Fj	...
		RTS
; ---------------------------------------------------------------------------

loc_CEBF:				; CEA3j
		LDA	$3A9,X
		BEQ	locret_CEBE
		BMI	locret_CEBE

loc_CEC6:				; CE83j
		LDA	7
		CLC
		ADC	#$18
		STA	7
		LDA	6
		ADC	#0
		STA	6
		LDY	#3
		LDA	#$C8 ; 'È'
		JSR	sub_CF2A
		BMI	loc_CEF9
		DEY
		LDA	#$B8 ; '¸'
		JSR	sub_CF2A
		BMI	loc_CEF9
		DEY
		LDA	#$A0 ; ' '
		JSR	sub_CF2A
		BMI	loc_CEF9
		LDA	#$90 ; ''
		JSR	sub_CF2A
		BPL	locret_CEBE
		LDA	$64
		AND	#1
		BNE	loc_CEBB

loc_CEF9:				; CEDAj CEE2j	...
		STY	0
		LDA	$7A
		CLC
		ADC	0
		CMP	$B
		BCC	sub_CF06
		LDA	$A
; End of function sub_CE4A


; =============== S U B	R O U T	I N E =======================================


sub_CF06:				; CE47j loc_CEB8p ...
		CPX	$15
		BNE	locret_CF29
		STA	$7A
		STA	$13
		LDA	$7A
		CLC
		ADC	#$F0 ; 'ð'
		STA	$7C
		LDA	#0
		ADC	#0
		STA	$7B
; End of function sub_CF06


; =============== S U B	R O U T	I N E =======================================


sub_CF1B:				; AF9Bp loc_CEBBp
		CPX	$15
		BNE	locret_CF29
		LDA	$36B,X
		STA	$77
		LDA	$37E,X
		STA	$78

locret_CF29:				; CF08j CF1Dj
		RTS
; End of function sub_CF1B


; =============== S U B	R O U T	I N E =======================================


sub_CF2A:				; CDE0p CE0Bp	...
		STA	2
		LDA	$7A
		CLC
		ADC	2
		STA	1
		LDA	#0
		ADC	#0
; End of function sub_CF2A


; =============== S U B	R O U T	I N E =======================================


sub_CF37:				; CE78p
		STA	0
		LDA	1
		SEC
		SBC	7
		STA	$F
		LDA	0
		SBC	6
		STA	$E
		RTS
; End of function sub_CF37

; ---------------------------------------------------------------------------
tbl_unk_CF47:	.BYTE 0, 0, 0, 0, 1, 1,	1 ; CF7Dr
tbl_unk_CF4E:	.BYTE $3F, $7F,	$BF, $FF, $3F, $6F, $BF	; CF85r

; =============== S U B	R O U T	I N E =======================================


sub_CF55:				; A4E7p
		LDA	#0
		LDX	#6

loc_CF59:				; CF5Dj
		STA	$4CE,X
		DEX
		BPL	loc_CF59
		LDY	#0

loc_CF61:				; CF6Bj
		JSR	sub_CF78
		LDA	$65
		BEQ	loc_CF6D
		INY
		CPY	#2
		BCC	loc_CF61

loc_CF6D:				; CF66j
		LDY	#2

loc_CF6F:				; CF75j
		JSR	sub_CF78
		INY
		CPY	#8
		BCC	loc_CF6F
		RTS
; End of function sub_CF55


; =============== S U B	R O U T	I N E =======================================

; Unknown. Every frame game cycle.

sub_CF78:				; loc_CF61p loc_CF6Fp
		LDX	#0

loc_CF7A:				; CF8Dj
		LDA	$36B,Y
		CMP	tbl_unk_CF47,X
		BNE	loc_CF8A
		LDA	$37E,Y
		CMP	tbl_unk_CF4E,X
		BCC	loc_CF91

loc_CF8A:				; CF80j
		INX
		CPX	#7
		BCC	loc_CF7A
		BCS	loc_CF99

loc_CF91:				; CF88j
		INC	$4CE,X
		TXA
		STA	$4C6,Y
		RTS
; ---------------------------------------------------------------------------

loc_CF99:				; CF8Fj
		LDA	#$FF
		STA	$4C6,Y
		RTS
; End of function sub_CF78


; =============== S U B	R O U T	I N E =======================================


sub_CF9F:				; A5E8p
		LDY	$4C6,X
		BMI	locret_CFE3
		LDA	$4CE,Y
		CMP	#4
		BCC	locret_CFE3
		INY
		CPY	#7
		BCS	loc_CFC7
		LDA	$4CE,Y
		CMP	#3
		BCS	loc_CFC7
		LDA	$3C1,X
		AND	#3
		ORA	#4
		STA	$3C1,X
		LDA	#5
		STA	$42E,X
		RTS
; ---------------------------------------------------------------------------

loc_CFC7:				; CFAEj CFB5j
		DEY
		DEY
		CPY	#7
		BCS	locret_CFE3
		LDA	$4CE,Y
		CMP	#3
		BCS	locret_CFE3
		LDA	$3C1,X
		AND	#3
		ORA	#8
		STA	$3C1,X
		LDA	#3
		STA	$42E,X

locret_CFE3:				; CFA2j CFA9j	...
		RTS
; End of function sub_CF9F

; ---------------------------------------------------------------------------
CFE4_points_offsets_zp_29:
		.BYTE	5		; D002r
		.BYTE	9
tbl_BONUS_REWARDS:
		.BYTE	1		; life for 10000 pts
		.BYTE	2		; life for 20000 pts
		.BYTE	4		; life for 40000 pts
		.BYTE	0		; no rewards

; =============== S U B	R O U T	I N E =======================================


sub_CFEA:				; A4F0p
		LDX	$15
		JSR	sub_CFF7
		LDA	$65
		BEQ	locret_D013
		TXA
		EOR	#1
		TAX
; End of function sub_CFEA


; =============== S U B	R O U T	I N E =======================================

; Bonus	award?

sub_CFF7:
		LDA	$C9,X
		BNE	locret_D013
		LDY	dip_BONUS ; dip?
		LDA	tbl_BONUS_REWARDS,Y
		BEQ	loc_D011 ; dip == no rewards? branch
		LDY	CFE4_points_offsets_zp_29,X
		CMP	$29,Y ; cmp high digits with 1, 2 or 4 (10000, 20000, 40000 points)
		BEQ	loc_D00C
		BCS	locret_D013

loc_D00C:				; Increase lives
		INC	LIVES,X
		JSR	sub_D056

loc_D011:				
		INC	$C9,X 		; no reward

locret_D013:
		RTS
; End of function sub_CFF7

; ---------------------------------------------------------------------------
unk_D014:	
		.BYTE $60
		.BYTE $70

; =============== S U B	R O U T	I N E =======================================


sub_D016:				; loc_99BFp
		LDA	#$DA ; 'Ú'
		STA	0
		LDX	#$FF
		LDY	#0

loc_D01E:				; D043j
		INX
		LDA	#$68 ; 'h'

loc_D021:				; D041j D047j
		STA	$2C3,Y
		PHA
		LDA	unk_D014,X
		STA	$2C0,Y
		LDA	0
		STA	$2C1,Y
		LDA	#3
		STA	$2C2,Y
		PLA
		CLC
		ADC	#8
		INC	0
		INY
		INY
		INY
		INY
		CPY	#$14
		BCC	loc_D021
		BEQ	loc_D01E
		CPY	#$28
		BNE	loc_D021
		RTS
; End of function sub_D016

; ---------------------------------------------------------------------------
unk_D04A:	.BYTE $23 ; #		; loc_D05Dr
		.BYTE	0
		.BYTE 6, $24, $24, $24,	$24, $24, $24, 0
byte_D054:	.BYTE $80		; loc_D0B0r
byte_D055:	.BYTE $9A		; loc_D09Ar

; =============== S U B	R O U T	I N E =======================================


sub_D056:				; 984Cp AEF4p	...
		STX	$F
		LDX	$300
		LDY	#0

loc_D05D:				; D067j
		LDA	unk_D04A,Y
		STA	$301,X
		INX
		INY
		CPY	#$A
		BNE	loc_D05D
		LDY	NT_ADDR_HI
		LDX	$F
		LDA	LIVES,X
		TAX
		LDA	$F
		BEQ	loc_D07B
		JSR	sub_D08E
		JMP	loc_D07E
; ---------------------------------------------------------------------------

loc_D07B:				; D073j
		JSR	sub_D09E

loc_D07E:				; D078j
		LDY	NT_ADDR_HI
		STA	$302,Y
		TYA
		CLC
		ADC	#9
		STA	NT_ADDR_HI
		LDX	$F
		RTS
; End of function sub_D056


; =============== S U B	R O U T	I N E =======================================


sub_D08E:				; D075p
		LDA	#$2A ; '*'

loc_D090:				; D097j
		INY
		DEX
		BMI	loc_D09A
		STA	$303,Y
		JMP	loc_D090
; ---------------------------------------------------------------------------

loc_D09A:				; D092j
		LDA	byte_D055
		RTS
; End of function sub_D08E


; =============== S U B	R O U T	I N E =======================================


sub_D09E:				; loc_D07Bp
		INY
		INY
		INY
		INY
		INY
		INY
		LDA	#$2A ; '*'

loc_D0A6:				; D0ADj
		DEY
		DEX
		BMI	loc_D0B0
		STA	$304,Y
		JMP	loc_D0A6
; ---------------------------------------------------------------------------

loc_D0B0:				; D0A8j
		LDA	byte_D054
		RTS
; End of function sub_D09E

; ---------------------------------------------------------------------------
tbl_unk_D0B4:	.BYTE $F5, $F6,	$F7, $F7, $F8, $F9, $F6, $F7, $F8, $F5,	$F7
					; D13Cr
tbl_unk_D0BF:	.BYTE 5, 7, $10, $10, $15, $20,	7, $10,	$15, 5,	$10, $50 ; D15Fr
tbl_unk_D0CB:	.BYTE $FF, 0, $FF, $FF,	$FF, $FF, 0, $FF, $FF, $FF, $FF	; D142r
					; D170r

; =============== S U B	R O U T	I N E =======================================


sub_D0D6:				; C982p CAB7p	...
		STY	$C
		STX	$D
		LDA	$3DA,X
		STA	$F
		LDY	$494
		LDA	#2
		STA	$5C,Y
		LDX	$C
		LDA	$37E,X
		SEC
		SBC	#8
		STA	$38D,Y
		LDA	$36B,X
		SBC	#0
		STA	$37A,Y
		LDA	$350,X
		CLC
		ADC	#$18
		CMP	#$F0 ; 'ð'
		BCC	loc_D106
		LDA	#$E0 ; 'à'

loc_D106:				; D102j
		STA	$35F,Y
		TXA
		BEQ	loc_D10D
		ASL	A

loc_D10D:				; D10Aj
		STA	$E
		LDX	#$A
		LDA	$495
		BEQ	loc_D135
		LDX	#9
		LDY	$D
		LDA	$3DA,Y
		AND	#$F0 ; 'ð'
		CMP	#$C0 ; 'À'
		BEQ	loc_D135
		LDX	$454,Y
		CMP	#0
		BEQ	loc_D135
		INX
		INX
		INX
		LDA	$3CA,Y
		BEQ	loc_D135
		INX
		INX
		INX

loc_D135:				; D114j D121j	...
		LDA	$494
		ASL	A
		ASL	A
		ASL	A
		TAY
		LDA	tbl_unk_D0B4,X
		STA	$2F1,Y
		LDA	tbl_unk_D0CB,X
		CMP	#1
		LDA	#0
		ADC	#$FA ; 'ú'
		STA	$2F5,Y
		LDA	$C
		STA	$2F2,Y
		STA	$2F6,Y

loc_D156:				; D177j
		INC	$47A
		INC	$47A
		LDY	$47A
		LDA	tbl_unk_D0BF,X
		STA	$47B,Y
		LDA	$C
		CPX	#$B
		BEQ	loc_D16D
		ORA	#8

loc_D16D:				; D169j
		STA	$47C,Y
		LDA	tbl_unk_D0CB,X
		BNE	loc_D17A
		LDX	#$B
		JMP	loc_D156
; ---------------------------------------------------------------------------

loc_D17A:				; D173j
		LDA	$494
		EOR	#1
		STA	$494
		LDA	#1
		STA	$495
		LDX	$D
		LDY	$C
		RTS
; End of function sub_D0D6

; ---------------------------------------------------------------------------
tbl_unk_D18C:	.BYTE $24, $FA		; D1B9r
		.BYTE $FB, $FC

; =============== S U B	R O U T	I N E =======================================


sub_D190:				; 9295p loc_A513p
		LDX	$300
		CPX	#$24 ; '$'
		BCS	locret_D1D0
		LDY	#0
		LDA	#3
		STA	$F
		LDA	$1C

loc_D19F:				; D1C6j
		AND	#$7E ; '~'
		TAY
		LDA	tbl_unk_D1D1,Y
		STA	$301,X
		INY
		LDA	tbl_unk_D1D1,Y
		STA	$302,X
		INY
		LDA	#1
		STA	$303,X
		TYA
		PHA
		LDY	$F
		LDA	tbl_unk_D18C,Y
		STA	$304,X
		PLA
		INX
		INX
		INX
		INX
		DEC	$F
		BPL	loc_D19F
		STX	$300
		LDA	#0
		STA	$301,X

locret_D1D0:				; D195j
		RTS
; End of function sub_D190

; ---------------------------------------------------------------------------
tbl_unk_D1D1:	.BYTE $20, $41,	$20, $49, $20, $69, $20, $6C, $20, $73,	$20, $74, $20, $79, $20, $7E
					; D1A2r D1A9r
		.BYTE $20, $C0,	$20, $DE, $21, 0, $21, $22, $21, $57, $21, $5D,	$21, $6B, $21, $7D
		.BYTE $21, $80,	$21, 1,	$21, $41, $21, $96, $21, $BE, $21, $C0,	$21, $DC, $22, $21
		.BYTE $22, $3E,	$22, $40, $22, $53, $22, $72, $22, $C0,	$22, $C4, $22, $DD, $22, $FE
		.BYTE $23, $18,	$23, $22, $23, $26, $23, $3B, $23, $3D,	$23, $40, $23, $49, $23, $56
		.BYTE $23, $59,	$23, $6A, $23, $94, $23, $B5, $28, $B, $28, $20, $28, $36, $28,	$43
		.BYTE $28, $47,	$28, $49, $28, $5B, $28, $64, $28, $69,	$28, $78, $28, $74, $28, $9E
		.BYTE $28, $C1,	$29, $1D, $29, $3F, $29, $82, $29, $9F,	$29, $C1, $29, $E2, $2A, $1E
;unknown_data02:	.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF
tbl_unk_D260:	.BYTE 0, 6, $12, $18, $C, $1E ;	loc_D2D5r

; =============== S U B	R O U T	I N E =======================================


sub_D266:				; B0D8p B1A1p	...
		TXA
		PHA
		LDA	$540,X
		CMP	#1
		BEQ	loc_D286
		CMP	#5
		BCC	loc_D289
		JSR	sub_D2B6
		JSR	sub_D5C2
		CMP	#$11
		BEQ	loc_D289
		JSR	sub_D5AE
		JSR	sub_D28C
		PLA
		TAX
		RTS
; ---------------------------------------------------------------------------

loc_D286:				; D26Dj
		JSR	sub_D2E0

loc_D289:				; D271j D27Bj
		PLA
		TAX
		RTS
; End of function sub_D266


; =============== S U B	R O U T	I N E =======================================


sub_D28C:				; D280p
		JSR	sub_D69B
		CPX	#2
		BCS	loc_D2A4
		LDA	$3E2,X
		BNE	locret_D2B5
		LDA	$8E
		CLC
		ADC	#3
		STA	$203,Y
		STA	$20F,Y
		RTS
; ---------------------------------------------------------------------------

loc_D2A4:				; D291j
		LDA	$8E
		STA	$207,Y
		CLC
		ADC	#3
		STA	$203,Y
		LDA	$217,Y
		STA	$213,Y

locret_D2B5:				; D296j
		RTS
; End of function sub_D28C


; =============== S U B	R O U T	I N E =======================================


sub_D2B6:				; D273p
		LDY	#5
		CPX	#2
		BCS	loc_D2D5
		DEY
		LDA	$3E2,X
		BEQ	loc_D2D5
		LDY	#0
		LDA	$3CA,X
		BEQ	loc_D2D5
		INY
		LDA	$4D6,X
		BEQ	loc_D2D5
		INY
		CMP	#1
		BEQ	loc_D2D5
		INY

loc_D2D5:				; D2BAj D2C0j	...
		LDA	tbl_unk_D260,Y
		STA	$91
		LDY	#5
		JSR	sub_DA7D
		RTS
; End of function sub_D2B6


; =============== S U B	R O U T	I N E =======================================


sub_D2E0:				; loc_D286p
		CPX	#2
		BCS	locret_D312
		LDA	#$F0 ; 'ð'
		STA	$88,X
		STA	$8A,X
		LDA	$3E2,X
		BEQ	locret_D312
; End of function sub_D2E0


; =============== S U B	R O U T	I N E =======================================


sub_D2EF:				; 9D93p loc_A5A0p ...
		TXA
		PHA
		CPX	#2
		BCS	loc_D2FA
		LDA	$3CA,X
		BNE	loc_D300

loc_D2FA:				; D2F3j
		JSR	sub_D82B
		JMP	loc_D303
; ---------------------------------------------------------------------------

loc_D300:				; D2F8j
		JSR	sub_D881

loc_D303:				; D2FDj
		JSR	sub_D5C2
		CMP	#$11
		BEQ	loc_D310
		JSR	sub_D5AE
		JSR	sub_D69B

loc_D310:				; D308j
		PLA
		TAX

locret_D312:				; D2E2j D2EDj
		RTS
; End of function sub_D2EF

; ---------------------------------------------------------------------------
tbl_unk_D313:	.BYTE	1		; D32Ar
		.BYTE	3
tbl_unk_D315:	.BYTE $C4 ; Ä		; D33Br
		.BYTE $BF ; ¿
tbl_unk_D317:	.BYTE $C9 ; É		; D336r
		.BYTE $C4 ; Ä

; =============== S U B	R O U T	I N E =======================================


sub_D319:				; A650p
		LDX	#0
		LDA	$7D3
		LSR	A
		BCC	loc_D323
		LDX	#4

loc_D323:				; D31Fj
		LDY	#1

loc_D325:				; D34Fj
		LDA	$7D0,Y
		BNE	loc_D341
		LDA	tbl_unk_D313,Y
		STA	$7D0,Y
		INC	$2D9,X
		LDA	$2D9,X
		CMP	tbl_unk_D317,Y
		BNE	loc_D341
		LDA	tbl_unk_D315,Y
		STA	$2D9,X

loc_D341:				; D328j D339j
		TXA
		EOR	#4
		TAX
		LDA	$7D0,Y
		SEC
		SBC	#1
		STA	$7D0,Y
		DEY
		BPL	loc_D325
		LDX	#$12
		JSR	sub_D5C2
		BEQ	loc_D35C
		LDA	#$F8 ; 'ø'
		STA	$8F

loc_D35C:				; D356j
		LDA	$8E
		STA	$2DB
		CLC
		ADC	#8
		STA	$2DF
		LDA	$8F
		STA	$2D8
		STA	$2DC
		RTS
; End of function sub_D319


; =============== S U B	R O U T	I N E =======================================


sub_D370:				; loc_9C7Cp loc_A102p	...
		LDA	$36B,X
		BNE	loc_D37C
		LDA	$37E,X
		CMP	#2
		BCC	loc_D3F8

loc_D37C:				; D373j
		JSR	sub_D5C2
		JSR	sub_D5AE
		JSR	sub_D77B
		STA	0
		STX	$E
		STY	$F
		LDA	$5E4,X
		BNE	loc_D3E7
		LDA	$446,X
		BEQ	loc_D3AA
		CMP	#1
		BEQ	loc_D3B8
		LDX	$F
		INC	$200,X
		INC	$208,X
		DEC	$204,X
		DEC	$20C,X
		JMP	loc_D3B8
; ---------------------------------------------------------------------------

loc_D3AA:				; D393j
		LDX	$F
		INC	$203,X

loc_D3AF:
		INC	$207,X
		DEC	$20B,X
		DEC	$20F,X

loc_D3B8:				; D397j D3A7j
		LDX	$E
		LDA	$3E,X
		BNE	loc_D3E0
		LDA	#$18
		STA	$3E,X
		LDA	$446,X
		LDY	$44E,X
		CPY	#1
		BCS	loc_D3D3
		ADC	#1
		CMP	#2
		JMP	loc_D3D7
; ---------------------------------------------------------------------------

loc_D3D3:				; D3CAj
		SBC	#1
		CMP	#1

loc_D3D7:				; D3D0j
		STA	$446,X
		LDA	#0
		ROL	A
		STA	$44E,X

loc_D3E0:				; D3BCj
		LDY	$F
		LDA	$5E4,X
		BEQ	loc_D406

loc_D3E7:				; D38Ej
		CMP	#$FE ; 'þ'
		BNE	loc_D430
		LDA	$574,X
		CMP	$15
		BNE	loc_D3F8
		LDA	$F1
		ORA	#4
		STA	$F1

loc_D3F8:				; D37Aj D3F0j
		LDA	#$FF
		STA	$3DA,X
		STA	$5E4,X
		INC	$94
		JSR	loc_D610
		RTS
; ---------------------------------------------------------------------------

loc_D406:				; D3E5j
		LDA	#$B6 ; '¶'
		STA	$201,Y
		LDA	#$B7 ; '·'
		STA	$205,Y
		LDA	#$B8 ; '¸'
		STA	$209,Y
		LDA	#$B9 ; '¹'
		STA	$20D,Y
		LDA	0
		ORA	#3
		STA	$202,Y
		STA	$206,Y
		STA	$20A,Y
		STA	$20E,Y
		LDA	#3
		STA	$4A2,X
		RTS
; ---------------------------------------------------------------------------

loc_D430:				; D3E9j
		CMP	#1
		BNE	loc_D463
		LDA	#$BA ; 'º'
		STA	$201,Y
		STA	$209,Y
		STA	$205,Y
		STA	$20D,Y
		LDA	0
		ORA	#2
		STA	$202,Y
		ORA	#$40 ; '@'
		STA	$20A,Y
		ORA	#$C0 ; 'À'
		STA	$20E,Y
		EOR	#$40 ; '@'
		STA	$206,Y
		LDA	$203,Y
		STA	$207,Y
		LDA	#$F7 ; '÷'
		STA	$5E4,X

loc_D463:				; D432j
		INC	$5E4,X
		RTS
; End of function sub_D370


; =============== S U B	R O U T	I N E =======================================


sub_D467:				; A096p loc_A750p

; FUNCTION CHUNK AT D4B1 SIZE 000000AA BYTES

		LDA	$36B,X
		BNE	loc_D473
		LDA	$37E,X
		CMP	#2
		BCC	loc_D48B

loc_D473:				; D46Aj
		JSR	sub_D5C2
		JSR	sub_D5BE
		JSR	sub_D79C
		STA	0
		LDA	$5E4,X
		BEQ	loc_D4B1
		CMP	#$FE ; 'þ'
		BCS	loc_D48B
		JSR	sub_D55B
		RTS
; ---------------------------------------------------------------------------

loc_D48B:				; D471j D485j
		CPX	$4E6
		BNE	sub_D4A8
		LDY	$5E4,X
		BEQ	loc_D4A1
		LDX	$4E7
		JSR	sub_D4A8
		STA	$4E7
		LDX	$4E6

loc_D4A1:				; D493j
		JSR	sub_D4A8
		STA	$4E6
		RTS
; End of function sub_D467


; =============== S U B	R O U T	I N E =======================================


sub_D4A8:				; D48Ej D498p	...
		JSR	sub_D604
		LDA	#$FF
		STA	$5E4,X
		RTS
; End of function sub_D4A8

; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_D467

loc_D4B1:				; D481j
		LDA	#$FC ; 'ü'
		STA	$201,Y
		STA	$205,Y
		STA	$209,Y
		STA	$20D,Y
		LDA	$501,X
		CLC
		ADC	#$F
		STA	$F
		LDA	$36B,X
		CPX	$4E7
		BNE	loc_D505
		CMP	#0
		BEQ	loc_D4DA
		LDA	$37E,X
		CMP	$F
		BCS	loc_D4F4

loc_D4DA:
		LDA	$4F
		CMP	#2
		LDA	#$BB
		BCS	loc_D4F1
		LDA	$4F
		CMP	#1
		LDA	#$BC
		BCS	loc_D4F1
		LDA	#$BE
		STA	$209,Y
		LDA	#$BD

loc_D4F1:
		STA	$201,Y

loc_D4F4:				; D4D8j
		LDA	#2
		ORA	0
		STA	$202,Y
		STA	$206,Y
		STA	$20A,Y
		STA	$20E,Y
		RTS
; ---------------------------------------------------------------------------

loc_D505:				; D4CDj
		CMP	#0
		BEQ	loc_D510
		LDA	$37E,X
		CMP	$F
		BCS	loc_D535

loc_D510:				; D507j
		LDA	#$A8 ; '¨'
		STA	$201,Y
		LDA	#$A9 ; '©'
		STA	$209,Y
		LDA	$36B,X
		BEQ	loc_D528
		LDA	$37E,X
		ADC	#8
		CMP	$F
		BCS	loc_D535

loc_D528:				; D51Dj
		LDA	$50
		AND	#1
		CMP	#1
		LDA	#$AA ; 'ª'
		ADC	#0
		STA	$205,Y

loc_D535:				; D50Ej D526j
		LDA	#3
		STA	$F
		LDA	$50
		CMP	#3
		LDA	#0
		ROR	A
		LSR	A
		ORA	$F
		ORA	0
		STA	$206,Y
		LDA	0
		ORA	$F
		STA	$202,Y
		STA	$20A,Y
		LDA	$50
		BNE	locret_D55A
		LDA	#5
		STA	$50

locret_D55A:				; D554j
		RTS
; END OF FUNCTION CHUNK	FOR sub_D467

; =============== S U B	R O U T	I N E =======================================


sub_D55B:				; D487p
		CMP	#1
		BNE	loc_D58C
		LDA	#$AC ; '¬'
		STA	$201,Y
		STA	$209,Y
		LDA	#$AD ; '­'
		STA	$205,Y
		STA	$20D,Y
		LDA	0
		ORA	#3
		STA	$202,Y
		STA	$206,Y
		ORA	#$40 ; '@'
		STA	$20A,Y
		STA	$20E,Y
		LDA	$203,Y
		STA	$207,Y
		LDA	#$F7 ; '÷'
		STA	$5E4,X

loc_D58C:				; D55Dj
		INC	$5E4,X
		RTS
; End of function sub_D55B

; ---------------------------------------------------------------------------
tbl_unk_D590:	.BYTE $18, $78,	$30, $90, $48, $A8, $60, $C0, 0	; D5B6r
tbl_unk_D599:	.BYTE $78, $18,	$90, $30, $A8, $48, $C0, $60, 0	; loc_D5BAr
tbl_unk_D5A2:	.BYTE 0, $18, $30, $40,	$50, $60, $70, $80, $90, $A0, $B0, $C0
					; sub_D5BEr

; =============== S U B	R O U T	I N E =======================================


sub_D5AE:				; B271p B3AEp	...
		LDA	$6C
		BNE	sub_D5BE
		LDA	$15
		BNE	loc_D5BA
		LDY	tbl_unk_D590,X
		RTS
; ---------------------------------------------------------------------------

loc_D5BA:				; D5B4j
		LDY	tbl_unk_D599,X
		RTS
; End of function sub_D5AE


; =============== S U B	R O U T	I N E =======================================


sub_D5BE:				; D476p D5B0j	...
		LDY	tbl_unk_D5A2,X
		RTS
; End of function sub_D5BE


; =============== S U B	R O U T	I N E =======================================


sub_D5C2:				; B66Bp loc_B775p ...
		LDY	#0
		LDA	$350,X
		STA	$8E
		LDA	$37E,X
		SEC
		SBC	$7A
		STA	$8F
		LDA	$36B,X
		SBC	#0
		BPL	loc_D5E4
		LDA	$8F
		CMP	#$F1 ; 'ñ'
		BCS	loc_D5E4
		LDY	#$10
		CMP	#$E7 ; 'ç'
		BCC	sub_D604

loc_D5E4:				; D5D6j D5DCj
		LDA	$7C
		SEC
		SBC	$37E,X
		STA	1
		LDA	$7B
		SBC	$36B,X
		BCC	sub_D604
		BNE	loc_D600
		LDA	1
		CMP	#8
		BCS	loc_D600
		INY
		CMP	#1
		BCC	sub_D604

loc_D600:				; D5F3j D5F9j
		TYA
		STA	$90
		RTS
; End of function sub_D5C2


; =============== S U B	R O U T	I N E =======================================


sub_D604:				; 9845p B171p	...
		LDA	$6C
		BNE	loc_D62A
		CPX	#9
		BPL	loc_D63F
		CPX	#$12
		BEQ	loc_D63F

loc_D610:				; D402p
		JSR	sub_D5AE

loc_D613:				; D62Fj
		LDA	#$F8 ; 'ø'
		STA	$200,Y
		STA	$204,Y
		STA	$208,Y
		STA	$20C,Y
		STA	$210,Y
		STA	$214,Y
		JMP	loc_D63F
; ---------------------------------------------------------------------------

loc_D62A:				; D606j
		JSR	sub_D5BE
		CPX	#2
		BCC	loc_D613
		LDA	#$F8 ; 'ø'
		STA	$200,Y
		STA	$204,Y
		STA	$208,Y
		STA	$20C,Y

loc_D63F:				; D60Aj D60Ej	...
		LDA	#$11
		STA	$90

locret_D643:				; D649j
		RTS
; End of function sub_D604


; =============== S U B	R O U T	I N E =======================================


sub_D644:				; 9C62p A0FCp
		JSR	sub_D5C2
		CMP	#$11
		BEQ	locret_D643
		JSR	sub_D5AE
		TXA
		PHA
		JMP	loc_D731
; End of function sub_D644

; ---------------------------------------------------------------------------
tbl_unk_D653:	.BYTE $13, $13,	$13, $1A, $13, $13 ; D6AAr
tbl_unk_D659:	.BYTE $2D, $2D,	$2F, $1B, $17, $3E ; D6B2r
tbl_unk_D65F:	.BYTE $1C, $20,	$22, $20, 2, $35 ; D6BAr
		.BYTE $15
		.BYTE $15
		.BYTE $15
		.BYTE $1D
		.BYTE $15
		.BYTE $15
		.BYTE $2E ; .
		.BYTE $2E ; .
		.BYTE $30 ; 0
		.BYTE $1E
		.BYTE $18
		.BYTE $3F ; ?
		.BYTE $1F
		.BYTE $FC ; ü
		.BYTE $24 ; $
		.BYTE $FC ; ü
		.BYTE	5
		.BYTE $37 ; 7
tbl_unk_D677:	.BYTE 2, 1, 2, $FF, $FE, $FE, $FE, $FF,	$FE, 1,	2, 2 ; loc_D6D3r
tbl_unk_D683:	.BYTE 0, $FF, 0, $FF, 0, 0, 0, 1, 0, 1,	0, 0, 0, 0, 0, 0 ; D6D9r
		.BYTE 0, 0, 0, 0, 0, 0,	0, 0

; =============== S U B	R O U T	I N E =======================================


sub_D69B:				; sub_D28Cp D30Dp
		TXA
		PHA
		LDA	$3C1,X
		LSR	A
		LDX	#5
		BCC	loc_D6A7
		LDX	#$17

loc_D6A7:				; D6A3j D6C6j
		LDA	$201,Y
		CMP	tbl_unk_D653,X
		BNE	loc_D6BF
		LDA	$205,Y
		CMP	tbl_unk_D659,X
		BNE	loc_D6BF
		LDA	$209,Y
		CMP	tbl_unk_D65F,X
		BEQ	loc_D6CB

loc_D6BF:				; D6ADj D6B5j
		DEX
		CPX	#$11
		BEQ	loc_D6C8
		CPX	#0
		BPL	loc_D6A7

loc_D6C8:				; D6C2j
		JMP	loc_D6E4
; ---------------------------------------------------------------------------

loc_D6CB:				; D6BDj
		CPX	#$11
		BCC	loc_D6D3
		TXA
		SBC	#$C
		TAX

loc_D6D3:				; D6CDj
		LDA	tbl_unk_D677,X
		STA	$4AB
		LDA	tbl_unk_D683,X
		STA	$4AC
		LDA	#0
		STA	$4AD

loc_D6E4:				; loc_D6C8j
		PLA
		TAX
; End of function sub_D69B


; =============== S U B	R O U T	I N E =======================================


sub_D6E6:				; B77Bp
		TXA
		PHA
		CPX	#2
		BCS	loc_D71A
		LDA	$4AB
		ORA	$4AC
		ORA	$4AD
		BEQ	loc_D71A
		TYA
		PHA
		LDA	$8E
		STA	0
		JSR	sub_D766
		LDA	0
		CLC
		ADC	#8
		STA	0
		JSR	sub_D766
		LDA	#0
		STA	$4AB
		STA	$4AC
		STA	$4AD
		PLA
		TAY
		JMP	loc_D731
; ---------------------------------------------------------------------------

loc_D71A:				; D6EAj D6F5j
		LDA	$8E
		STA	$203,Y
		STA	$207,Y
		STA	$20B,Y
		CLC
		ADC	#8
		STA	$20F,Y
		STA	$213,Y
		STA	$217,Y

loc_D731:				; D650j D717j
		LDA	$8F
		STA	$200,Y
		STA	$20C,Y
		CLC
		ADC	#8
		STA	$204,Y
		STA	$210,Y
		CLC
		ADC	#8
		STA	$208,Y
		STA	$214,Y
		LDA	#$F8 ; 'ø'
		LDX	$90
		BEQ	loc_D763
		DEX
		BNE	loc_D75D
		STA	$208,Y
		STA	$214,Y
		PLA
		TAX
		RTS
; ---------------------------------------------------------------------------

loc_D75D:				; D752j
		STA	$200,Y
		STA	$20C,Y

loc_D763:				; D74Fj
		PLA
		TAX
		RTS
; End of function sub_D6E6


; =============== S U B	R O U T	I N E =======================================


sub_D766:				; D6FDp D707p
		LDX	#0

loc_D768:				; D778j
		LDA	$4AB,X
		CLC
		ADC	0
		STA	$203,Y
		INY
		INY
		INY
		INY
		INX
		CPX	#3
		BNE	loc_D768
		RTS
; End of function sub_D766


; =============== S U B	R O U T	I N E =======================================


sub_D77B:				; D382p
		JSR	sub_D7B9
		LDA	$203,Y
		STA	$207,Y
		LDA	$36B,X
		BNE	loc_D791
		LDA	#$10
		CMP	$37E,X
		JMP	loc_D796
; ---------------------------------------------------------------------------

loc_D791:				; D787j
		LDA	$37E,X
		CMP	#$A8 ; '¨'

loc_D796:				; D78Ej
		LDA	#0
		ROR	A
		LSR	A
		LSR	A
		RTS
; End of function sub_D77B


; =============== S U B	R O U T	I N E =======================================


sub_D79C:				; D479p
		JSR	sub_D7B9
		LDA	$36B,X
		BNE	loc_D7AC
		LDA	#$18
		CMP	$37E,X
		JMP	loc_D7B2
; ---------------------------------------------------------------------------

loc_D7AC:				; D7A2j
		LDA	$37E,X
		CMP	$501,X

loc_D7B2:				; D7A9j
		LDA	#$20 ; ' '
		BCS	locret_D7B8
		LDA	#0

locret_D7B8:				; D7B4j
		RTS
; End of function sub_D79C


; =============== S U B	R O U T	I N E =======================================


sub_D7B9:				; sub_D77Bp sub_D79Cp

; FUNCTION CHUNK AT D800 SIZE 00000002 BYTES

		LDA	$90
		CMP	#$11
		BEQ	locret_D808
		LDA	$8E
		STA	$203,Y
		PHA
		LDA	$5E4,X
		BEQ	loc_D7D4
		PLA
		STA	$207,Y
		CLC
		ADC	#8
		JMP	loc_D7DE
; ---------------------------------------------------------------------------

loc_D7D4:				; D7C8j
		PLA
		CLC
		ADC	#4
		STA	$207,Y
		CLC
		ADC	#4

loc_D7DE:				; D7D1j
		STA	$20B,Y
		STA	$20F,Y
		LDA	$8F
		JSR	sub_D802
		CLC
		ADC	#8
		JSR	sub_D7F9
		LDA	$90
		BEQ	locret_D808
		CMP	#1
		BNE	loc_D800
		LDA	#$F8 ; 'ø'
; End of function sub_D7B9


; =============== S U B	R O U T	I N E =======================================


sub_D7F9:				; D7ECp
		STA	$204,Y
		STA	$20C,Y
		RTS
; End of function sub_D7F9

; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_D7B9

loc_D800:				; D7F5j
		LDA	#$F8 ; 'ø'
; END OF FUNCTION CHUNK	FOR sub_D7B9

; =============== S U B	R O U T	I N E =======================================


sub_D802:				; D7E6p
		STA	$200,Y
		STA	$208,Y

locret_D808:				; D7BDj D7F1j
		RTS
; End of function sub_D802

; ---------------------------------------------------------------------------
tbl_unk_D809:	.BYTE $FF, 0, $FF, 0, $FF, 0, $FF, 0, $FF, 0, $FF, 0, $FF, 0, $FF, 0
					; loc_D852r
tbl_unk_D819:	.BYTE $FF, 0, 0, $FF, 0, 0, $FF, 0, 0, $FF, 0, 0, $FF, 0, 0, 0
					; D8C1r
		.BYTE  $A
		.BYTE  $A

; =============== S U B	R O U T	I N E =======================================


sub_D82B:				; loc_D2FAp
		CPX	#2
		BCS	loc_D848
		LDA	$9C,X
		BMI	loc_D85D
		CMP	#8
		BNE	loc_D841
		CPX	$15
		BNE	loc_D841
		LDA	$F0
		ORA	#$10
		STA	$F0

loc_D841:				; D835j D839j
		LDY	$9C,X
		DEC	$9C,X
		JMP	loc_D852
; ---------------------------------------------------------------------------

loc_D848:				; D82Dj
		LDA	$42E,X
		CMP	#5
		LDY	$40E,X
		BCS	loc_D85D

loc_D852:				; D845j
		LDA	tbl_unk_D809,Y
		BEQ	locret_D85C
		LDY	#1
		JSR	sub_DA37

locret_D85C:				; D855j
		RTS
; ---------------------------------------------------------------------------

loc_D85D:				; D831j D850j
		CPX	#2
		BCS	loc_D86C
		LDA	$80,X
		AND	#3
		BEQ	loc_D86C
		CMP	$3C1,X
		BNE	loc_D870

loc_D86C:				; D85Fj D865j
		LDA	$3E,X
		BNE	locret_D87C

loc_D870:				; D86Aj
		LDA	$7C6,X
		BNE	loc_D87D
		LDA	#$10
		STA	$3E,X
		JSR	sub_DA35

locret_D87C:				; D86Ej
		RTS
; ---------------------------------------------------------------------------

loc_D87D:				; D873j
		DEC	$7C6,X
		RTS
; End of function sub_D82B


; =============== S U B	R O U T	I N E =======================================


sub_D881:				; loc_D300p
		LDA	#0
		STA	$4D6,X
		LDA	$4E0,X
		BNE	loc_D8E0
		LDA	$3D2,X
		CMP	#$20 ; ' '
		BEQ	loc_D8CC
		INC	$88,X
		LDY	$88,X
		CPY	#$10
		BCC	loc_D89F
		LDY	#0
		TYA
		STA	$88,X

loc_D89F:				; D898j
		LDA	$82,X
		CMP	$3C1,X
		BNE	loc_D8E3
		LDA	$4BE,X
		BNE	loc_D8BE
		LDA	$7F
		BNE	loc_D8B9
		CPX	$15
		BNE	loc_D8B9
		LDA	$F0
		ORA	#8
		STA	$F0

loc_D8B9:				; D8ADj D8B1j
		LDA	#$A
		STA	$4BE,X

loc_D8BE:				; D8A9j
		DEC	$4BE,X
		LDA	tbl_unk_D819,Y
		BEQ	locret_D8DF
		LDY	#0
		JSR	sub_DA37
		RTS
; ---------------------------------------------------------------------------

loc_D8CC:				; D890j
		LDA	#$F0 ; 'ð'
		STA	$88,X
		INC	$8A,X
		LDA	$8A,X
		CMP	#$80 ; '€'
		BCC	locret_D8DF

loc_D8D8:
		LDA	#0
		STA	$8A,X
		JSR	sub_DA3C

locret_D8DF:				; D8C4j D8D6j
		RTS
; ---------------------------------------------------------------------------

loc_D8E0:				; D889j
		DEC	$4E0,X

loc_D8E3:				; D8A4j
		INC	$4D6,X
		LDA	$3C1,X
		PHA
		LDA	#$F0 ; 'ð'
		STA	$8A,X
		LDY	$3E2,X
		DEY
		STY	$F
		LDA	$8E
		LDY	$3C1,X
		CPY	#2
		BCS	loc_D902
		ADC	$F
		JMP	loc_D904
; ---------------------------------------------------------------------------

loc_D902:				; D8FBj
		SBC	$F

loc_D904:				; D8FFj
		STA	$8E
		LDY	#0
		LDA	$80,X
		AND	#3
		ORA	$3C1,X
		CMP	#3
		BNE	loc_D91D
		LDA	$3C1,X
		EOR	#3
		STA	$3C1,X
		LDY	#6

loc_D91D:				; D911j
		TYA
		LDY	$3E2,X
		DEY
		BEQ	loc_D92A
		CLC
		ADC	#$48 ; 'H'
		JMP	loc_D92D
; ---------------------------------------------------------------------------

loc_D92A:				; D922j
		CLC
		ADC	#$54 ; 'T'

loc_D92D:				; D927j
		STA	$91
		LDY	#0
		JSR	sub_DA7D
		PLA
		STA	$3C1,X
		RTS
; End of function sub_D881

; ---------------------------------------------------------------------------
tbl_unk_D939:	
		.BYTE <tbl_D98D		; sub_DA7Dr
		.BYTE <tbl_D945
		.BYTE <tbl_D969
		.BYTE <tbl_D9ED
		.BYTE <tbl_D9FF
		.BYTE <tbl_DA11
tbl_unk_D93F:	.BYTE >tbl_D98D		; DA82r
		.BYTE >tbl_D945
		.BYTE >tbl_D969
		.BYTE >tbl_D9ED
		.BYTE >tbl_D9FF
		.BYTE >tbl_DA11
tbl_D945:	.BYTE $00, $01,	$02, $03, $04, $05, $00, $01, $02, $03,	$06, $07, $00, $01, $02, $03
					; D93At D940t
		.BYTE $06, $08,	$0F, $10, $02, $11, $12, $05, $0F, $10,	$02, $11, $19, $07, $0F, $10
		.BYTE $02, $11,	$19, $08
tbl_D969:	.BYTE $09, $0A,	$02, $0B, $0C, $05, $00, $01, $02, $03,	$04, $05, $00, $01, $02, $0D
					; D93Bt D941t
		.BYTE $0E, $05,	$13, $14, $02, $15, $16, $05, $0F, $10,	$02, $11, $12, $05, $13, $17
		.BYTE $02, $15,	$18, $05
tbl_D98D:	.BYTE $1A, $1B,	$1C, $1D, $1E, $1F, $1A, $1B, $20, $1D,	$1E, $FC, $1A, $21, $22, $1D
					; tbl_unk_D939t tbl_unk_D93Ft
		.BYTE $23, $24,	$13, $2D, $1C, $15, $2E, $1F, $13, $2D,	$20, $15, $2E, $FC, $13, $2F
		.BYTE $22, $15,	$30, $24, $00, $38, $35, $0D, $39, $37,	$00, $34, $35, $03, $36, $37
		.BYTE $09, $3A,	$35, $0B, $3B, $37, $0F, $40, $35, $11,	$41, $37, $13, $3C, $35, $15
		.BYTE $3D, $37,	$13, $3E, $35, $15, $3F, $37, $25, $26,	$27, $28
		.BYTE $29, $2A,	$25, $26, $2B, $28, $29, $2C, $25, $31,	$27, $32, $33, $2A, $25, $31
		.BYTE $2B, $32,	$33, $2C
tbl_D9ED:	.BYTE $4E, $4F,	$50, $51, $52, $53, $4E, $4F, $50, $51,	$5E, $58, $4E, $4F, $50, $51
					; D93Ct D942t
		.BYTE $5E, $5D
tbl_D9FF:	.BYTE $54, $55,	$50, $56, $57, $53, $4E, $4F, $50, $51,	$52, $53, $59, $5A, $50, $5B
					; D93Dt D943t
		.BYTE $5C, $53
tbl_DA11:	.BYTE $4C, $CC,	$02, $4D, $CD, $05, $4C, $2F, $22, $4D,	$30, $24, $4C, $48, $42, $FC
					; D93Et D944t
		.BYTE $49, $43,	$4C, $2F, $27, $15, $30, $2A, $4C, $2F,	$2B, $15, $30, $2C, $4C, $6C
		.BYTE $6D, $FC,	$6E, $6F

; =============== S U B	R O U T	I N E =======================================


sub_DA35:				; D879p
		LDY	#2
; End of function sub_DA35


; =============== S U B	R O U T	I N E =======================================


sub_DA37:				; D859p D8C8p
		LDA	#0
		JMP	loc_DA40
; End of function sub_DA37


; =============== S U B	R O U T	I N E =======================================


sub_DA3C:				; D8DCp
		LDY	#0
		LDA	#$24 ; '$'

loc_DA40:				; DA39j
		STA	$F
		CPX	#2
		BCC	loc_DA48
		INY
		INY

loc_DA48:				; DA44j
		TYA
		PHA
		LDA	$446,X
		LDY	$44E,X
		CPY	#1
		BCS	loc_DA5B
		ADC	#6
		CMP	#$C
		JMP	loc_DA5F
; ---------------------------------------------------------------------------

loc_DA5B:				; DA52j
		SBC	#6
		CMP	#1

loc_DA5F:				; DA58j
		STA	$446,X
		LDA	#0
		ROL	A
		STA	$44E,X
		LDA	$F
		ADC	$446,X
		CPX	#2
		BCS	loc_DA79
		LDY	$3E2,X
		DEY
		BNE	loc_DA79
		ADC	#$12

loc_DA79:				; DA6Fj DA75j
		STA	$91
		PLA
		TAY
; End of function sub_DA3C


; =============== S U B	R O U T	I N E =======================================


sub_DA7D:				; D2DCp D931p
		LDA	tbl_unk_D939,Y
		STA	$E
		LDA	tbl_unk_D93F,Y
		STA	$F
; End of function sub_DA7D


; =============== S U B	R O U T	I N E =======================================


sub_DA87:				; B772p
		TXA
		PHA
		JSR	sub_D5AE
		TYA
		PHA
		TAX
		LDY	$91
		LDA	($E),Y
		STA	$201,X
		INY
		LDA	($E),Y
		STA	$205,X
		INY
		LDA	($E),Y
		STA	$209,X
		INY
		LDA	($E),Y
		STA	$20D,X
		INY
		LDA	($E),Y
		STA	$211,X
		INY
		LDA	($E),Y
		STA	$215,X
		PLA
		TAY
		PLA
		TAX
		LDA	$3C1,X
		AND	#1
		LSR	A
		ROR	A
		ROR	A
		ORA	$4A2,X
		STA	$202,Y
		STA	$206,Y
		STA	$20A,Y
		STA	$20E,Y
		STA	$212,Y
		STA	$216,Y
		LDA	$3C1,X
		AND	#1
		BEQ	locret_DB12
		LDA	$201,Y
		PHA
		LDA	$20D,Y
		STA	$201,Y
		PLA
		STA	$20D,Y
		INY
		INY
		INY
		INY
		LDA	$201,Y
		PHA
		LDA	$20D,Y
		STA	$201,Y
		PLA
		STA	$20D,Y
		INY
		INY
		INY
		INY
		LDA	$201,Y
		PHA
		LDA	$20D,Y
		STA	$201,Y
		PLA
		STA	$20D,Y
		INY
		INY
		INY
		INY

locret_DB12:				; DADAj
		RTS
; End of function sub_DA87

; ---------------------------------------------------------------------------


; =============== S U B	R O U T	I N E =======================================


DBA2_read_DIPs:				; 8094p
		LDA	DIP1
		AND	#1
		STA	PROC_ID_LIST_01
		LDA	DIP1
		LSR	A
		LSR	A
		LSR	A
		LSR	A
		LSR	A
		AND	#7
		STA	dip_COINAGE

		LDA DIP2
		PHA
		AND #1
		STA dip_NESMODE
		PLA
		LSR	A ; shift right
		PHA ; push
		AND	#3
		STA	dip_BONUS ; dip with rewards info: 0, 1, 2, 3 for 10000, 20000, 40000 and no rewards
		PLA ; pop
		LSR	A
		LSR	A
		PHA ; push
		AND	#1 ; (A>>3) and 1. 
		STA	dip_ENEMY_REGEN ; store dip 
		PLA ; pop
		LSR	A
		PHA ; push
		AND	#3
		STA	dip_DIFFICULTY ; init difficulty dip
		PLA ; pop
		LSR	A
		LSR	A
		AND	#3
		STA	dip_LIVES
		RTS
; End of function DBA2_read_DIPs


; =============== S U B	R O U T	I N E =======================================


proc_DDE5:				; 9269o
		JSR	sub_DE77
		CMP	#$FF
		BEQ	loc_DDED
		RTS
; ---------------------------------------------------------------------------

loc_DDED:				; DDEAj
		LDA	#1
		STA	SELECTOR
		JSR	clear_nametable
		JSR	init_oam_buffer
		LDA	#$10
		STA	$4F
		LDA	#1
		STA	$68
		LDA	#0
		STA	SELECTOR_COPY
		STA	$C3
		STA	$6B
		STA	$74
		JSR	cnrom_page_set
		RTS
; End of function proc_DDE5


; =============== S U B	R O U T	I N E =======================================

; ‚ë¯®«­ï¥âáï ’Ž‹œŠŽ ¢ £« ¢­®¬ ¬¥­î.

proc_todo_DE0D:				; A4B1o
		JSR	sub_DE77
		CMP	#$FF
		BEQ	loc_DE15
		RTS
; ---------------------------------------------------------------------------

loc_DE15:				; DE12j
		LDA	$4F
		BNE	locret_DE1E
		LDA	#1
		JSR	sub_DE7F
; End of function proc_todo_DE0D

; START	OF FUNCTION CHUNK FOR proc_DE1F

locret_DE1E:				; DE17j DE24j
		RTS
; END OF FUNCTION CHUNK	FOR proc_DE1F

; =============== S U B	R O U T	I N E =======================================


proc_DE1F:				; 926Bo

; FUNCTION CHUNK AT DE1E SIZE 00000001 BYTES
; FUNCTION CHUNK AT DE4A SIZE 0000001B BYTES

		JSR	sub_DE77
		CMP	#$FF
		BNE	locret_DE1E
		LDA	$C3
		BNE	loc_DE4A
		LDA	#2
		STA	SELECTOR
		JSR	clear_nametable
		JSR	init_oam_buffer
		LDA	#$20 ; ' '
		STA	$4F
		STA	SELECTOR_COPY
		INC	$C3
; End of function proc_DE1F


; =============== S U B	R O U T	I N E =======================================


cnrom_page_set:				; DE09p DED4p	...
		LDA	VS_CTRL_VAR
		ORA	#4
		STA	VS_CTRL_VAR

		; TODO: new var with 2 and 3 pages of gfx
		
		ORA off_CNROM

		JSR	cnrom_set_bank
		LDA	#0			; reset scroll 
		STA	SCROLL_Y
		RTS
; End of function cnrom_page_set

; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR proc_DE1F

loc_DE4A:				; DE28j
		CMP	#1
		BNE	loc_DE51
		INC	$C3
		RTS
; ---------------------------------------------------------------------------

loc_DE51:				; DE4Cj
		LDX	#0
		JSR	sub_E10F
		LDA	#0
		STA	$C3
		STA	SELECTOR_COPY
		LDA	#1
		STA	$68
		LDA	#9
		STA	$4F
		RTS
; END OF FUNCTION CHUNK	FOR proc_DE1F
; ---------------------------------------------------------------------------

proc_unk_DE65:				; A4B3o
		JSR	sub_DE77
		CMP	#$FF
		BEQ	loc_DE6D
		RTS
; ---------------------------------------------------------------------------

loc_DE6D:				; DE6Aj
		LDA	$4F
		BNE	locret_DE76
		LDA	#$A
		JSR	sub_DE7F

locret_DE76:				; DE6Fj
		RTS

; =============== S U B	R O U T	I N E =======================================


sub_DE77:				; proc_DDE5p proc_todo_DE0Dp ...

; FUNCTION CHUNK AT DE88 SIZE 0000000D BYTES

		LDA	$BD
		CMP	#4
		BNE	loc_DE88
		LDA	#3
; End of function sub_DE77


; =============== S U B	R O U T	I N E =======================================

; switch scene?
; IN A = scene number

sub_DE7F:				; DE1Bp DE73p	...
		STA	$69 		; store current A to $69

loc_DE81:				; loc_E0BBj
		LDA	#0 			
		STA	$68  		; reset unknown $68
		STA	$C3 		; reset unknown $C3
		RTS
; End of function sub_DE7F

; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_DE77

loc_DE88:				; DE7Bj
		LDA	COINS_CNT1ST
		BEQ	loc_DE92
		LDA	#$2
		JSR	sub_DE7F
		RTS
; ---------------------------------------------------------------------------

loc_DE92:				; DE8Aj
		LDA	#$FF
		RTS
; END OF FUNCTION CHUNK	FOR sub_DE77
; ---------------------------------------------------------------------------

    ; Sprites 46-49 (P1	button)
    ; Sprites 4A-4D (P1	button)

    ; Sprite table format:
    ; Y, T, P, X
    ; Y	position, Tile number, Palette,	X position.

sprites_BUTTONS: 
		; This table was used in the players readiness standby screen and it's no longer needed.
		; Button icons are replaced with background labels,
		; sprites are disabled. So that's just a memento.
		.BYTE $54, $46,	0, $88
		.BYTE $5C, $47,	0, $88
		.BYTE $54, $48,	0, $90
		.BYTE $5C, $49,	0, $90
		.BYTE $AC, $4A,	0, $88
		.BYTE $B4, $4B,	0, $88
		.BYTE $AC, $4C,	0, $90
		.BYTE $B4, $4D,	0, $90

tbl_DEB5:
		.BYTE $28
		.BYTE $10
; ---------------------------------------------------------------------------

proc_DEB7:
						; i assume this proc inits screen with waiting for player(s)
		LDA	#0 
		STA	$6B			; reset unknown $6B
		STA	$74 		; and $74
		JSR	sub_DF50	; if $BD = 4, $69 = 3; $68 = 0; $C3 = 0
		BEQ	locret_DEDD
		LDA	COINS_CNT1ST
		BEQ	loc_DEFD
		LDA	$C3
		BNE	loc_DEDE
		LDA	#3				; 3 - index procedure of stack manager
		STA	SELECTOR
		JSR	clear_nametable
		JSR	init_oam_buffer
		
		; TODO create new var for CNROM page offset (page #2 DIP switches menu)

		JSR	cnrom_page_set
		LDA	#1
		STA	SELECTOR_COPY
		INC	$C3

locret_DEDD:				; DEC0j
		RTS
; ---------------------------------------------------------------------------

loc_DEDE:				; DEC8j
		JSR	sub_E1A0

; No need sprites here
;		LDX	#$1F

;loc_DEE3:				; DEEAj
;		LDA	sprites_BUTTONS,X
;		STA	$200,X
;		DEX
;		BPL	loc_DEE3
; loading it to PPU disabled

		LDA	#$20 ; ' '
		STA	$3E
		
		LDA	#0
		STA	$C3
		STA	SELECTOR_COPY
		STA	$C7
		LDA	#1
		STA	$68
		RTS
; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR proc_unk_DF07

loc_DEFD:				; DEC4j DF0Ej
		LDA	#0
		JSR	sub_DE7F
		STA	SELECTOR_COPY
		STA	$13
		RTS
; END OF FUNCTION CHUNK	FOR proc_unk_DF07

; =============== S U B	R O U T	I N E =======================================


proc_unk_DF07:				; A4B5o

; FUNCTION CHUNK AT DEFD SIZE 0000000A BYTES
; FUNCTION CHUNK AT DF2A SIZE 00000026 BYTES

		JSR	sub_DF50
		BEQ	locret_DF4F
		LDA	COINS_CNT1ST
		BEQ	loc_DEFD
		JSR	proc_unk_DF5C
		JSR	sub_E1A0

loc_DF16:				; DFBEp
		LDA	JOY1_TRIG
		AND	#PAD_START
		BEQ	loc_DF2A
		DEC	COINS_CNT1ST ; 
; End of function proc_unk_DF07


; =============== S U B	R O U T	I N E =======================================


sub_DF1E:				; DF48p
		LDA	#$B
		JSR	sub_DE7F
		STA	$65
		STA	$74
		STA	$C8
		RTS
; End of function sub_DF1E

; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR proc_unk_DF07

loc_DF2A:				; DF1Aj
		LDA	$BD
		CMP	#$A
		BEQ	loc_DF38
		CMP	#$C
		BEQ	loc_DF38
		CMP	#5
		BPL	locret_DF4F

loc_DF38:				; DF2Ej DF32j
		LDA	COINS_CNT1ST
		CMP	#1
		BNE	loc_DF42
		LDA	$BC
		BEQ	locret_DF4F

    ; PATCHED: LDA $1A
    ;	       AND #$20
    ; TO:      LDA $18
    ;	       AND #$10


loc_DF42:				; Checking joypad 2 start button
		LDA	$1A
		AND	#$10 ; ' '
		BEQ	locret_DF4F
		;JSR loc_E0A1
		;LDA	#$B
		;STA	$69

locret_DF4F:				; DF0Aj DF36j	...
		RTS
; END OF FUNCTION CHUNK	FOR proc_unk_DF07

; =============== S U B	R O U T	I N E =======================================


sub_DF50:				; DEBDp proc_unk_DF07p ...
		
		LDA	$BD 		; check unknown $BD
		CMP	#4			; 
		BNE	locret_DF5B ; NO! branch below
		; $BD == 4
		LDA	#3 			
		JSR	sub_DE7F 	;init $69 = 3; $68 = 0; $C3 = 0

locret_DF5B:				; DF54j
		RTS
; End of function sub_DF50


; =============== S U B	R O U T	I N E =======================================


proc_unk_DF5C:				; DF10p E030p
		LDA	$3E
		BNE	locret_DF99
		LDA	$C7
		EOR	#1
		STA	$C7
		LDX	$C7
		LDA	tbl_DEB5,X
		STA	$3E
		LDX	#$C

loc_DF6F:				; DF97j
		LDA	$C7
		STA	$202,X
		LDA	$BD
		CMP	#$A
		BEQ	loc_DF82
		CMP	#$C
		BEQ	loc_DF82
		CMP	#5
		BPL	loc_DF9A

loc_DF82:				; DF78j DF7Cj
		LDA	COINS_CNT1ST
		BEQ	loc_DF9A
		CMP	#1
		BNE	loc_DF8E
		LDA	$BC
		BEQ	loc_DF9A

loc_DF8E:				; DF88j
		LDA	$C7
		STA	$212,X

loc_DF93:				; DF9Fj
		DEX
		DEX
		DEX
		DEX
		BPL	loc_DF6F

locret_DF99:				; DF5Ej
		RTS
; ---------------------------------------------------------------------------

loc_DF9A:				; DF80j DF84j	...
		LDA	#0
		STA	$212,X
		JMP	loc_DF93
; End of function proc_unk_DF5C

; ---------------------------------------------------------------------------

proc_DFA2:				; 9281o
		LDA	#1
		STA	$68
		RTS
; ---------------------------------------------------------------------------

proc_unk_DFA7:				; A4C9o
		LDA	COINS_CNT1ST
		BNE	loc_DFB9

loc_DFAB:				; DFC5j
		LDA	$4F
		BNE	locret_DFB8
		LDA	#5
		JSR	sub_DE7F
		STA	$74
		STA	$C8

locret_DFB8:				; DFADj DFBCj
		RTS
; ---------------------------------------------------------------------------

loc_DFB9:				; DFA9j
		JSR	sub_DF50
		BEQ	locret_DFB8
		JSR	loc_DF16
		LDA	$69
		CMP	#$C
		BEQ	loc_DFAB
		LDA	#1
		STA	$F0
		RTS
; ---------------------------------------------------------------------------
tbl_DFCC:	.BYTE $94, $4A,	0, $88,	$9C, $4B, 0, $88, $94, $4C, 0, $90, $9C, $4D, 0, $90
					; loc_E004r
; ---------------------------------------------------------------------------

proc_DFDC:				; 926Fo
		LDA	$BD
		CMP	#4
		BNE	loc_E01F
		LDA	$C3
		BNE	loc_DFF7
		JSR	clear_nametable
		JSR	cnrom_page_set
		LDA	#4
		STA	SELECTOR
		LDA	#1
		STA	SELECTOR_COPY
		INC	$C3
		RTS
; ---------------------------------------------------------------------------

loc_DFF7:				; DFE4j
		JSR	init_oam_buffer
		LDA	#$20 ; ' '
		STA	$3E
		LDA	#$19
		STA	$4F
		LDX	#$F

loc_E004:				; E00Bj
		LDA	tbl_DFCC,X
		STA	$200,X
		DEX
		BPL	loc_E004
		JSR	sub_E1A0
		LDA	#0
		STA	$C7
		STA	$C3
		STA	SELECTOR_COPY
		STA	SCROLL_Y
		LDA	#1
		STA	$68
		RTS
; ---------------------------------------------------------------------------

loc_E01F:				; DFE0j E02Ej	...
		LDA	#0
		LDX	COINS_CNT1ST
		BEQ	loc_E027
		LDA	#2

loc_E027:				; E023j
		JMP	sub_DE7F
; ---------------------------------------------------------------------------

proc_unk_E02A:				; A4B7o
		LDA	$BD
		CMP	#4
		BNE	loc_E01F
		JSR	proc_unk_DF5C
		JSR	sub_E1A0
		LDA	$1A
		AND	#$20 ; ' '
		BNE	loc_E045
		LDA	$4F
		BNE	locret_E044
		LDA	#$D
		STA	$69

locret_E044:				; E03Ej
		RTS
; ---------------------------------------------------------------------------

loc_E045:				; E03Aj
		LDA	#4
		STA	$69
		JMP	loc_E0BB
; ---------------------------------------------------------------------------

proc_E04C:				; 9283o A4CBo
		JSR	clear_ppumask	; Disable PPU.
		LDA	#1
		STA	SELECTOR_COPY
		LDA	$BD
		CMP	#4
		BEQ	locret_E05C
		JMP	loc_E01F
; ---------------------------------------------------------------------------

locret_E05C:				; E057j
		RTS
; ---------------------------------------------------------------------------

; scene change related
proc_E05D:				; 9271o
		LDA	$BD
		CMP	#$B
		BEQ	loc_E0BE
		CMP	#4
		BEQ	loc_E0A1
		LDA	$C3
		BNE	loc_E081
		JSR	clear_nametable
		JSR	init_oam_buffer
		LDA	#7
		STA	SELECTOR
		LDA	#0
		STA	SELECTOR_COPY
		STA	$13
		JSR	cnrom_page_set
		INC	$C3
		RTS
; ---------------------------------------------------------------------------


loc_E081:				; E069j
		JSR	sub_E1A0
		LDA	#1
		STA	$68
		LDA	#0
		STA	$C3
		STA	SELECTOR_COPY
		RTS
; ---------------------------------------------------------------------------

proc_unk_E08F:				; A4B9o
		LDA	$BD
		CMP	#$D
		BEQ	loc_E0BE
		CMP	#$B
		BEQ	loc_E0BE
		CMP	#4
		BEQ	loc_E0A1
		JSR	sub_E1A0
		RTS
; ---------------------------------------------------------------------------

loc_E0A1:				; E065j E09Bj
		LDA	#$B
		STA	$69
		LDA	#1
		STA	$65
		LDA	#0
		STA	$6B
		STA	$74
		LDA	COINS_CNT1ST
		BEQ	loc_E0BB
		DEC	COINS_CNT1ST
		LDA	$BC
		BNE	loc_E0BB
		DEC	COINS_CNT1ST

loc_E0BB:				; E049j E0B1j	...
		JMP	loc_DE81
; ---------------------------------------------------------------------------

loc_E0BE:				; E061j E093j	...
		LDA	#2
		JMP	sub_DE7F

handler_init_dip_menu:

		LDA	$C3
		BNE	loc_E081
		JSR	clear_nametable
		JSR	init_oam_buffer
		LDA	#$55
		STA	SELECTOR
		LDA	#0
		STA	SELECTOR_COPY
		STA	SCROLL_Y
		LDX #8
		stx $68
		STX off_CNROM 
		JSR	cnrom_page_set
		lda #251
		sta dipmenu_state
		INC	$C3
		RTS

; ---------------------------------------------------------------------------
tbl_E03C:	.BYTE 1, 1, 1, 1, 2, 3,	4, 1, 1, 1, 1, 1, 1, 1,	2, 3
tbl_E0D3:	.BYTE 1, 2, 3, 4, 1, 1,	1, $20,	1, 2, 3, 4, 5, 6, 1, 1

; =============== S U B	R O U T	I N E =======================================


sub_E0E3:				; E169p
		LDA	#1
		STA	TEMP
		LDA	#3
		STA	TEMP_HI
		JSR	_do_load_ppu_data
		LDA	#0
		STA	NT_ADDR_HI
		STA	NT_ADDR_LO
		RTS
; End of function sub_E0E3

; ---------------------------------------------------------------------------
tbl_E0F7:	.BYTE	0		; E123r
		.BYTE	6
		.BYTE  $C
		.BYTE $12
		.BYTE $18
		.BYTE $1E
tbl_E0FD:	.BYTE	6		; E128r
		.BYTE	6
		.BYTE	6
		.BYTE	6
		.BYTE	6
		.BYTE	6
tbl_E103:	.BYTE $24 ; $		; E141r
		.BYTE $27 ; '
		.BYTE $2A ; *
		.BYTE $2D ; -
		.BYTE $30 ; 0
		.BYTE $33 ; 3
tbl_E109:	.BYTE	6		; E146r
		.BYTE	6
		.BYTE	6
		.BYTE	6
		.BYTE	6
		.BYTE	6

; =============== S U B	R O U T	I N E =======================================


sub_E10F:				; DE53p E2CBp
		LDA	#$2D ; '-'
		CPX	#0
		BEQ	loc_E117
		LDA	#$6D ; 'm'

loc_E117:				; E113j
		STA	$C0
		LDA	#$22 ; '"'
		STA	$C1
		LDA	#0
		STA	$C2

loc_E121:				; E181j
		LDX	$C2
		LDA	tbl_E0F7,X
		STA	0
		LDA	tbl_E0FD,X
		STA	1
		LDY	#5

loc_E12F:				; E135j
		LDA	(TEMP),Y
		STA	$304,Y
		DEY
		BPL	loc_E12F
		LDA	#$24 ; '$'
		STA	$30A
		STA	$30B
		LDX	$C2
		LDA	tbl_E103,X
		STA	0
		LDA	tbl_E109,X
		STA	1
		LDY	#2

loc_E14D:				; E153j
		LDA	(0),Y
		STA	$30C,Y
		DEY
		BPL	loc_E14D
		LDA	#0
		STA	$30F
		LDA	$C1
		STA	$301
		LDA	$C0
		STA	$302
		LDA	#$B
		STA	$303
		JSR	sub_E0E3
		LDA	$C0
		CLC
		ADC	#$40 ; '@'
		STA	$C0
		CMP	#$2D ; '-'
		BNE	loc_E17B
		LDA	#$23 ; '#'
		STA	$C1

loc_E17B:				; E175j
		INC	$C2
		LDA	$C2
		CMP	#5
		BNE	loc_E121
		RTS
; End of function sub_E10F

; ---------------------------------------------------------------------------
E184_str_credits:	.BYTE 12 
					.BYTE $23, $74
					.BYTE 9
					.BYTE $C, $1B,	$E, $D,	$12, $1D, $24, 0, 0
					.BYTE 0
					; tile string "CREDIT"

E192_str_freeplay:	.BYTE 12
					.BYTE $23, $74
					.BYTE $09
					.BYTE $0F, $1B,	$0E, $E,	$24, $19, $15, $A, $22
					.BYTE 0
					; tile string "FREE PLAY"

attrs_menu_LO: .byte <attr_menuitem_00, <attr_menuitem_01, <attr_menuitem_02, <attr_menuitem_03, <attr_menuitem_04 
attrs_menu_HI: .byte >attr_menuitem_00, >attr_menuitem_01, >attr_menuitem_02, >attr_menuitem_03, >attr_menuitem_04

attr_menuitem_00:
				.byte 12
				.byte $23, $d1
				.byte 6+$40
				.byte $fa

				.byte $23, $d9
				.byte 6+$40
				.byte $ff

				.byte $23, $e1
				.byte 6+$40
				.byte $ff
				.byte 0
attr_menuitem_01:
				.byte 12
				.byte $23, $d1
				.byte 6+$40
				.byte $af

				.byte $23, $d9
				.byte 6+$40
				.byte $ff

				.byte $23, $e1
				.byte 6+$40
				.byte $ff
				.byte 0

attr_menuitem_02:
				.byte 12
				.byte $23, $d1
				.byte 6+$40
				.byte $ff

				.byte $23, $d9
				.byte 6+$40
				.byte $fa

				.byte $23, $e1
				.byte 6+$40
				.byte $ff
				.byte 0

attr_menuitem_03:
				.byte 12
				.byte $23, $d1
				.byte 6+$40
				.byte $ff

				.byte $23, $d9
				.byte 6+$40
				.byte $af

				.byte $23, $e1
				.byte 6+$40
				.byte $ff
				.byte 0

attr_menuitem_04:
				.byte 12
				.byte $23, $d1
				.byte 6+$40
				.byte $ff

				.byte $23, $d9
				.byte 6+$40
				.byte $ff

				.byte $23, $e1
				.byte 6+$40
				.byte $fa
				.byte 0

nam_dip_off:
				; DIP off
				.byte 14  ; Block length (without zero?)

				.byte $22, $84
				.byte 4+$80
				.byte $ce
				.byte $de
				.byte $ee
				.byte $fe

				.byte $22, $85
				.byte 4+$80
				.byte $cf
				.byte $df
				.byte $ef
				.byte $ff
				; DIP off


; =============== S U B	R O U T	I N E =======================================


sub_E1A0:				; loc_DEDEp DF13p ...
		LDA	dip_COINAGE
		CMP	#7
		BNE	loc_E1B2
		LDX	#$D

loc_E1A8:				; E1AFj
		LDA	E192_str_freeplay,X
		STA	$300,X
		DEX
		BPL	loc_E1A8
		RTS
; ---------------------------------------------------------------------------

loc_E1B2:				; E1A4j
		LDX	#$D

loc_E1B4:				; E1BBj
		LDA	E184_str_credits,X
		STA	$300,X
		DEX
		BPL	loc_E1B4
		LDA	COINS_CNT1ST

loc_E1BF:				; E1C9j
		CMP	#$A
		BMI	loc_E1CC
		INC	$30B
		SEC
		SBC	#$A
		JMP	loc_E1BF
; ---------------------------------------------------------------------------

loc_E1CC:				; E1C1j
		STA	$30C
		RTS
; End of function sub_E1A0


; =============== S U B	R O U T	I N E =======================================


proc_E1D0:				; 9273o

; FUNCTION CHUNK AT E2C9 SIZE 00000014 BYTES

		LDA	$C3
		BNE	loc_E20F
		JSR	sub_E637
		JSR	sub_EA5D
		LDA	$589
		BPL	loc_E1E9	; 8k banks are switched	here.
		JSR	sub_E265
		RTS
; ---------------------------------------------------------------------------

loc_E1E9:				; E1E3j
		LDA	$BD		; 8k banks are switched	here.
		CMP	#5
		BEQ	loc_E21A
		JSR	clear_ppumask	; Disable PPU.
		LDA	VS_CTRL_VAR
		ORA	#4
		STA	VS_CTRL_VAR
		JSR	cnrom_set_bank
		JSR	clear_nametable
		JSR	init_oam_buffer
		LDA	#0
		STA	SCROLL_Y
		LDA	#6
		STA	SELECTOR
		INC	$C3
		JSR	sub_EACD
		RTS
; ---------------------------------------------------------------------------

loc_E20F:				; E1D2j
		LDA	#0
		STA	$58A
		JSR	sub_EB94
		JMP	loc_E2C9
; ---------------------------------------------------------------------------

loc_E21A:				; E1EDj
		LDA	#6
		JMP	sub_DE7F
; End of function proc_E1D0

; ---------------------------------------------------------------------------

proc_unk_E21F:				; A4BBo
		LDA	$C3
		BNE	loc_E25D
		LDA	#1
		STA	$58A
		LDA	#0
		STA	$580
		JSR	sub_EB94
		LDA	$580
		CMP	#4
		BEQ	loc_E238
		RTS
; ---------------------------------------------------------------------------

loc_E238:				; E235j
		JSR	sub_ED89
		LDA	#$80 ; '€'
		STA	$3E
		LDA	#1
		STA	$C3
		LDX	#$32 ; '2'

loc_E248:				; E24Fj
		LDA	$600,X
		STA	$6100,X
		DEX
		BPL	loc_E248
		LDA	#$77 ; 'w'
		STA	$61FE
		STA	$61FF
		JSR	sub_E637
		RTS
; ---------------------------------------------------------------------------

loc_E25D:				; E221j
		LDA	$3E
		BNE	locret_E26A
		LDA	#1
		STA	$F0

; =============== S U B	R O U T	I N E =======================================


sub_E265:				; E1E5p
		LDA	#0
		JSR	sub_DE7F

locret_E26A:				; E25Fj
		RTS
; End of function sub_E265


; =============== S U B	R O U T	I N E =======================================


proc_E26B:				; 9275o

; FUNCTION CHUNK AT E2DD SIZE 00000005 BYTES

		LDA	$C3
		BNE	loc_E2C9
		LDA	$15
		BNE	loc_E279
		LDA	$BD
		CMP	#6
		BEQ	loc_E2DD

loc_E279:				; E271j
		LDA	$BD
		CMP	#6
		BEQ	loc_E283
		CMP	#5
		BNE	loc_E2DD

loc_E283:				; E27Dj
		INC	$C3
		JSR	clear_ppumask	; Disable PPU.
		LDA	VS_CTRL_VAR
		ORA	#4
		STA	VS_CTRL_VAR
		JSR	cnrom_set_bank
		LDA	#1
		STA	SELECTOR_COPY
		JSR	clear_nametable
		JSR	init_oam_buffer
		JSR	sub_EBC5
		LDY	#$14

loc_E2A0:				; E2BEj
		LDA	$2DC,Y
		CLC
		ADC	#$10
		STA	$2DC,Y
		LDA	$2DF,Y
		SEC
		SBC	#$28 ; '('
		STA	$2DF,Y
		LDA	$2DE,Y
		EOR	#1
		STA	$2DE,Y
		DEY
		DEY
		DEY
		DEY
		BPL	loc_E2A0
		LDA	#0
		STA	$13
		LDA	#5
		STA	SELECTOR
		RTS
; End of function proc_E26B

; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR proc_E1D0

loc_E2C9:				; E217j E26Dj
		LDX	#1
		JSR	sub_E10F
		LDA	#1
		STA	$68
		LDA	#0
		STA	SELECTOR_COPY
		STA	$C3
		LDA	#8
		STA	$F3
		RTS
; END OF FUNCTION CHUNK	FOR proc_E1D0
; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR proc_E26B

loc_E2DD:				; E277j E281j	...
		LDA	#5
		JMP	sub_DE7F
; END OF FUNCTION CHUNK	FOR proc_E26B
; ---------------------------------------------------------------------------

proc_unk_E2E2:				; A4BDo
		LDA	$15
		BNE	loc_E2EC
		LDA	$BD
		CMP	#6
		BEQ	loc_E2DD

loc_E2EC:				; E2E4j
		LDA	$BD
		CMP	#5
		BNE	loc_E2DD
		RTS
; ---------------------------------------------------------------------------

proc_E2F3:				; 927Do
		JSR	sub_DE77
		CMP	#$FF
		BNE	loc_E313
		LDA	#$FF
		STA	$C3
		JSR	proc_9285

; =============== S U B	R O U T	I N E =======================================


sub_E301:				; loc_E313p E321p ...
		LDA	#0
		STA	$F0
		STA	$F1
		STA	$F2
		STA	$F3
		RTS
; End of function sub_E301

; ---------------------------------------------------------------------------

proc_E30C:				; A4C5o
		JSR	sub_DE77
		CMP	#$FF
		BEQ	loc_E317

loc_E313:				; E2F8j
		JSR	sub_E301
		RTS
; ---------------------------------------------------------------------------

loc_E317:				; E311j
		LDA	$C3
		BEQ	loc_E325
		JSR	sub_EF33
		JSR	proc_unk_A4CD
		JSR	sub_E301
		RTS
; ---------------------------------------------------------------------------

loc_E325:				; E319j
		JSR	sub_E301
		LDA	#0
		JMP	sub_DE7F
; ---------------------------------------------------------------------------

proc_E32D:				; 9277o
		LDA	$BD
		CMP	#7
		BEQ	loc_E36D
		LDA	#$A 		; Init selector #10 ()
		STA	SELECTOR
		JSR	clear_nametable
		JSR	init_oam_buffer
		JSR	cnrom_page_set
		LDA	#1
		STA	$68
		RTS
; ---------------------------------------------------------------------------

proc_unk_E345:				; A4BFo
		LDA	$BD
		CMP	#7
		BEQ	loc_E36D
		CMP	#8
		BEQ	loc_E368
		CMP	#9
		BEQ	loc_E368
		LDA	$C3
		BEQ	locret_E367
		LDA	$15
		EOR	#1
		TAX
		LDA	#0
		STA	$62,X
		STA	$65
		LDA	#$B
		JSR	sub_DE7F

locret_E367:				; E355j
		RTS
; ---------------------------------------------------------------------------

loc_E368:				; E34Dj E351j
		LDA	#1
		STA	$C3
		RTS
; ---------------------------------------------------------------------------

loc_E36D:				; E331j E349j
		LDX	#0
		LDA	LIVES,X
		BMI	loc_E377
		TXA
		EOR	#1
		TAX

loc_E377:				; E371j
		LDA	#0
		STA	$3EA,X
		LDA	#2
		STA	$3E2,X
		STA	$42E,X
		JSR	sub_A14C
		LDA	#$B
		JMP	sub_DE7F
; ---------------------------------------------------------------------------
tbl_E38C:	
		.BYTE	5
		.BYTE $22
		.BYTE $1A
		.BYTE	2
		.BYTE	0
		.BYTE	0
		.BYTE	0

; =============== S U B	R O U T	I N E =======================================


proc_E393:				; 9279o
		LDA	COINS_CNT1ST
		BNE	loc_E3B1
		LDA	#8
		STA	SELECTOR

loc_E39B:				; E3F5p
		JSR	clear_nametable
		JSR	init_oam_buffer
		JSR	cnrom_page_set
		LDA	#$40 ; '@'
		STA	$3E
		LDA	#$A
		STA	$C3
		LDA	#1
		STA	$68
		RTS
; ---------------------------------------------------------------------------

loc_E3B1:				; E395j E3B8j
		LDA	#9
		JMP	sub_DE7F
; End of function proc_E393

; ---------------------------------------------------------------------------

proc_unk_E3B6:				; A4C1o
		LDA	COINS_CNT1ST
		BNE	loc_E3B1

loc_E3BA:				; E408j
		LDA	$3E
		BNE	locret_E3CB
		DEC	$C3
		BPL	loc_E3CC
		LDA	#1
		STA	$C8
		LDA	#$B
		JSR	sub_DE7F

locret_E3CB:				; E3BCj
		RTS
; ---------------------------------------------------------------------------

loc_E3CC:				; E3C0j
		LDA	#$40 ; '@'
		STA	$3E
		LDX	#6

loc_E3D2:				; E3D9j
		LDA	tbl_E38C,X
		STA	$300,X
		DEX
		BPL	loc_E3D2
		LDA	$C3
		STA	$305
		RTS
; ---------------------------------------------------------------------------
tbl_unk_E3E1:
		.BYTE $64
		.BYTE $4A
		.BYTE	0
		.BYTE $A4

		.BYTE $6C
		.BYTE $4B
		.BYTE	0
		.BYTE $A4

		.BYTE $64
		.BYTE $4C
		.BYTE	0
		.BYTE $AC

		.BYTE $6C
		.BYTE $4D
		.BYTE	0
		.BYTE $AC
; ---------------------------------------------------------------------------

proc_E3F1:				; 927Bo
		LDA	#9
		STA	SELECTOR
		JSR	loc_E39B
		LDX	#$F

loc_E3FA:				; E401j
		LDA	tbl_unk_E3E1,X
		STA	$200,X
		DEX
		BPL	loc_E3FA
		RTS
; ---------------------------------------------------------------------------

proc_unk_E404:				; A4C3o
		LDA	$1A
		AND	#$20 ; ' '
		BEQ	loc_E3BA
		LDA	#7
		STA	$69
		LDA	#0
		STA	$C3
		DEC	COINS_CNT1ST
		RTS
; ---------------------------------------------------------------------------
high_score_table:
		; High score initial
		; points table
		.BYTE 0, 6, 8, 0, 0, 0		; 068000 points
		.BYTE 0, 4, 7, 1, 0, 0		; 047100 points
		.BYTE 0, 2, 6, 6, 6, 6		; 029000 points
		.BYTE 0, 1, 8, 0, 0, 0		; 018000 points
		.BYTE 0, 0, 7, 0, 0, 0		; 007000 points
		; ??
		.BYTE 0, 0, 0, 0, 0, 0		; 000000 points
		
		; Hall of names
		.BYTE 2, 8, 7		; 1st
		.BYTE $A, $18, $11	; 2nd
		.BYTE $23, $18,	$10	; 3rd
		.BYTE $A, $17, $d	; 4th
		.BYTE $22, $18,	$1e	; 5th

; =============== S U B	R O U T	I N E =======================================


init_high_score:			; 8097p
		LDX	#$32 ; '2'

@high_score_fill:			; E454j
		LDA	high_score_table,X
		STA	$600,X
		DEX
		BPL	@high_score_fill
		LDA	#$77 ; 'w'
		STA	$6FF
		STA	$6FE
		JSR	sub_E637
		RTS
; End of function init_high_score


; =============== S U B	R O U T	I N E =======================================


poll_select2:			
		LDA	JOY2_TRIG
		AND	#PAD_START
		BEQ not_B_button

		LDA	$69
		CMP	#$B
		BEQ exit_pol_sel
		CMP	#$E
		BEQ exit_pol_sel
		CMP	#$5
		BEQ exit_pol_sel

		LDA	#$e ; DIP SWITCHES MENU ENABLE
		jsr	sub_DE7F
		jsr dip_values_array_update
		jmp loc_F739 ; piranha

not_B_button:
		LDA	JOY1_VAR
		AND	#PAD_SELECT
		BEQ	exit_pol_sel ; branch, the "select" button is not pressed
		lda $69
		cmp #$e
		beq exit_pol_sel
		; SELECT detected	
		LDA	JOY1_TRIG
		STA	TEMP ; select state store to temp

		LDX	#0 ; ?
		JSR	sub_E4F7

		LDX	#0
		LSR	TEMP
		jmp sub_E4F7
exit_pol_sel:

		rts
; =============== S U B	R O U T	I N E =======================================


sub_E4F7:
		LDA	$B0,X ; is $B0 == 0 ?
		BNE	loc_E50A ; branch if so
		LDA	TEMP ; yes, $B == 1
		AND	#PAD_SELECT
		BEQ	sub_E4F7_ret

		; triggers when coin inserted
		LDA	#2
		STA	$B0,X ; 
		; -------
		LDA	#$F ; init attract mode timer?
		STA	TIMER_ATTRACT,X
		

sub_E4F7_ret:				; E4FFj
		
		RTS
; ---------------------------------------------------------------------------

loc_E50A:				; E4F9j
		; Select pressed
		LDA	TEMP
		AND	#PAD_SELECT
		BNE	loc_E520
		
		LDA	$B0,X
		CMP	#$FF
		BEQ	loc_E51B
		JSR	sub_E529 ; dig
		INC	$B2

loc_E51B:				; E514j
		LDA	#0
		STA	$B0,X
		RTS
; ---------------------------------------------------------------------------

loc_E520:				; E50Ej
		LDA	TIMER_ATTRACT,X
		BNE	locret_E528
		LDA	#$FF
		STA	$B0,X

locret_E528:				; E522j
		RTS
; End of function sub_E4F7


; =============== S U B	R O U T	I N E =======================================


sub_E529:				; E516p
		LDA	#$FF 		; sound?
		STA	$7EA
		NOP
		INC	$57E,X
; End of function sub_E529


; =============== S U B	R O U T	I N E =======================================


sub_E532:				; E4D7p
		INC	$B4,X
		LDA	#8
		CPX	#1
		BEQ	loc_E53C
		LDA	#0

loc_E53C:				; E538j
		CLC
		ADC	dip_COINAGE
		TAY
		LDA	tbl_E03C,Y
		CMP	$B4,X
		BEQ	loc_E549
		BPL	locret_E55B

loc_E549:				; E545j
		LDA	#0
		STA	$B4,X
		LDA	COINS_CNT1ST	; Coins	from #1-#2 acceptors
		CMP	#$5C ; '\'      ; No more than 92 ($5c) coins.
		BPL	locret_E55B
		LDA	tbl_E0D3,Y
		CLC
		ADC	COINS_CNT1ST	; Increase coins counter.
		STA	COINS_CNT1ST	; Coins	from #1-#2 acceptors

locret_E55B:				; E547j E551j
		RTS
; End of function sub_E532


; =============== S U B	R O U T	I N E =======================================


sub_E55C:				; 9C21p A4EDp
		LDY	$47A
		BMI	loc_E586
		LDA	$47B,Y
		STA	0
		LDA	$47C,Y
		STA	1
		INC	$493
		LDY	$493
		AND	#3
		STA	$496,Y
		JSR	sub_E8DE
		DEC	$47A
		DEC	$47A
		LDA	#$F9 ; 'ù'
		STA	0
		JSR	sub_E9D0

loc_E586:				; E55Fj
		LDA	$300
		BNE	loc_E5A1
		LDA	SELECTOR
		BNE	loc_E5A1
		LDY	$493
		BMI	loc_E5A1
		LDA	$496,Y
		ORA	#$F0 ; 'ð'
		STA	0
		JSR	sub_E849
		DEC	$493

loc_E5A1:				; 98B0p E589j	...
		LDX	#0
		TXA
		JSR	sub_E5AB
		LDX	#1
		LDA	#8
; End of function sub_E55C


; =============== S U B	R O U T	I N E =======================================


sub_E5AB:				; E5A4p
		PHA
		LDA	$5C,X
		BEQ	loc_E5CC
		TXA
		CLC
		ADC	#$F
		TAX
		JSR	sub_D5C2
		BNE	loc_E5D6
		PLA
		TAY
		LDA	$8E
		STA	$2F3,Y
		CLC
		ADC	#8
		STA	$2F7,Y
		LDA	$8F
		JMP	loc_E5DA
; ---------------------------------------------------------------------------

loc_E5CC:				; E5AEj
		LDA	#1
		STA	$37A,X
		LDA	#$F8 ; 'ø'
		STA	$38D,X

loc_E5D6:				; E5B8j
		PLA
		TAY
		LDA	#$F8 ; 'ø'

loc_E5DA:				; E5C9j
		STA	$2F0,Y
		STA	$2F4,Y
		RTS
; End of function sub_E5AB

; ---------------------------------------------------------------------------

proc_UNUSED:
		LDA	#8
		STA	5
		LDA	1
		AND	#$F8 ; 'ø'
		STA	1
		LDA	0
		AND	#$F8 ; 'ø'
		LSR	A
		LSR	A
		LSR	A
		ASL	1
		ROL	5
		ASL	1
		ROL	5
		CLC
		ADC	1
		STA	0
		LDA	5
		STA	1
		RTS

; =============== S U B	R O U T	I N E =======================================


sub_E604:				; 9C00p loc_BEEAp ...
		LDA	1
		ASL	A
		ASL	A
		ASL	A
		STA	4
		LDA	1
		AND	#$E0 ; 'à'
		LSR	A
		LSR	A
		STA	5
		LDA	0
		AND	#7
		CLC
		ROR	A
		ROR	A
		ROR	A
		ORA	5
		STA	5
		RTS
; End of function sub_E604


; =============== S U B	R O U T	I N E =======================================


sub_E637:				; 8056p 808Bp	...
		LDA	VS_CTRL_VAR
		AND	#$FD ; 'ý'      ; Reset bit 1
		STA	CTRL_PORT1	; and write it to register.
		STA	VS_CTRL_VAR	; Store	var.
		RTS
; End of function sub_E637


; =============== S U B	R O U T	I N E =======================================


execute_procedure:			; 8177p 9266p	...
		ASL	A
		TAY
		PLA
		STA	TEMP
		PLA
		STA	TEMP_HI
		INY
		LDA	(TEMP),Y
		STA	PROC_POINTER
		INY
		LDA	(TEMP),Y
		STA	PROC_POINTER_HI
		JMP	(PROC_POINTER)
; End of function execute_procedure


; =============== S U B	R O U T	I N E =======================================

; Init 256 bytes of OAM	buffer $0200.

init_oam_buffer:			; clear_bg_and_spritesp 9A21p	...
		LDA	#0
		LDX	#$40 ; '@'
		STA	PTR1
		LDA	#2
		STA	PTR1_HI
		LDY	#0

loc_E66C:				; E679j
		LDA	#$F8 ; 'ø'
		STA	(PTR1),Y
		INY
		INY
		LDA	#0
		STA	(PTR1),Y
		INY
		INY
		DEX
		BNE	loc_E66C
		RTS
; End of function init_oam_buffer


; =============== S U B	R O U T	I N E =======================================


clear_both_nametables:			; 80BBp 9CF0p
		LDA	#0
		STA	NT_ADDR_HI
		STA	NT_ADDR_LO

clear_nametable:			; DDF1p DE2Ep	...
		JSR	clear_ppumask	; Disable PPU.
		LDA	#$20 ; ' '
		JSR	wipe_nametable	; IN: A	= High PPU address (ex.	$20, $28).
					; Fills	nametable with tile $24	(" " sym from BG page).
					; Wipes	attribute grid (64 bytes) with 0.
		LDA	#$28 ; '('
		JSR	wipe_nametable	; IN: A	= High PPU address (ex.	$20, $28).
					; Fills	nametable with tile $24	(" " sym from BG page).
					; Wipes	attribute grid (64 bytes) with 0.
		RTS
; End of function clear_both_nametables


; =============== S U B	R O U T	I N E =======================================


init_scroll:				; 80B8p
		LDA	#$10
		STA	PPU_CTRL	; BG page = 2 (tiles 257-512)
					; This code is repeated	twice (the first time when the console is initialized).
		STA	PPU_CTRL_VAR

		LDA	#0		; Init scroll.
		STA	PPU_SCROLL
		STA	SCROLL_X
		STA	PPU_SCROLL
		STA	SCROLL_Y
		RTS
; End of function init_scroll


; =============== S U B	R O U T	I N E =======================================

; Disable PPU.

clear_ppumask:				; 81EFp init_gamep ...
		LDA	#0
		STA	PPU_MASK
		RTS
; End of function clear_ppumask


; =============== S U B	R O U T	I N E =======================================

; IN: A	= High PPU address (ex.	$20, $28).
; Fills	nametable with tile $24	(" " sym from BG page).
; Wipes	attribute grid (64 bytes) with 0.

wipe_nametable:				; E689p E68Ep
		STA	TEMP		; Store	A to $00
		LDA	PPU_STATUS	; Clear	vblank flag
		LDA	PPU_CTRL_VAR	; Load PPU_CTRL_VAR
		AND	#$FB ; 'û'      ; Reset bit #2 ($FB = %1111_1011)
		STA	PPU_CTRL	; Now VRAM increment = 1
		LDA	TEMP		; Restore input	parameter
		STA	PPU_ADDR	; Set PPU address
		LDA	#0
		STA	PPU_ADDR	; Set PPU address
		LDX	#4
		LDY	#0
		LDA	#$24 ; '$'

clr_nam:				; E6CCj E6CFj
		STA	PPU_DATA	; Fill 1024 tiles of nametable with $24
		DEY
		BNE	clr_nam		; Fill 1024 tiles of nametable with $24
		DEX
		BNE	clr_nam		; Fill 1024 tiles of nametable with $24
		LDA	TEMP		; Restore input	parameter
		CLC			; Add 960 ($3c0)
		ADC	#3
		STA	PPU_ADDR
		LDA	#$C0 ; 'À'
		STA	PPU_ADDR
		LDY	#$40 ; '@'      ; Wipe 64 bytes
		LDA	#0

clr_attr:				; E6E6j
		STA	PPU_DATA	; Fill 64 bytes	of attributes with 0
		DEY
		BNE	clr_attr	; Fill 64 bytes	of attributes with 0
		RTS
; End of function wipe_nametable


; =============== S U B	R O U T	I N E =======================================


E6E9_process_starfield:

		LDX	#0
		LDY	#$E
		LDA	$1B
		AND	#2
		STA	TEMP
		LDA	$1C
		AND	#2
		EOR	TEMP
		CLC
		BEQ	loc_E6FD
		SEC

loc_E6FD:				; E6FAj E701j
		ROR	$1B,X
		INX
		DEY
		BNE	loc_E6FD
		LDA	$69
		CMP	#$A
		BNE	loc_E70A
		RTS
; ---------------------------------------------------------------------------

loc_E70A:				; E707j
		LDY	#3

loc_E70C:				; E722j
		ASL	TEMP_PTR_LO
		ROL	TEMP_PTR_HI
		ROL	A
		ROL	A
		EOR	TEMP_PTR_LO
		ROL	A
		EOR	TEMP_PTR_LO
		LSR	A
		LSR	A
		EOR	#$FF
		AND	#1
		ORA	TEMP_PTR_LO
		STA	TEMP_PTR_LO
		DEY
		BPL	loc_E70C
		LDA	TEMP_PTR_LO
		STA	$1B
		LDA	TEMP_PTR_HI
		STA	$1D
		EOR	#$FF
		STA	$20
		RTS
; End of function E6E9_process_starfield


; =============== S U B	R O U T	I N E =======================================


sub_E731:				; 812Fp 8153p
		JSR	sub_E783
		LDA	VS_CTRL_VAR
		LDA	#1
		STA	CTRL_PORT1
		STA	VS_CTRL_VAR
		LDX	#0
		LDA	VS_CTRL_VAR
		AND	#$FE ; 'þ'
		STA	CTRL_PORT1
		STA	VS_CTRL_VAR
		JSR	sub_E750
		INX
		JSR	sub_E750
		RTS
; End of function sub_E731


; =============== S U B	R O U T	I N E =======================================


sub_E750:				; E748p E74Cp
		LDY	#8

loc_E752:				; E75Dj
		PHA
		LDA	CTRL_PORT1,X
		NOP
		NOP
		NOP
		LSR	A
		PLA
		ROL	A
		DEY
		BNE	loc_E752
		STX	TEMP
		ASL	TEMP
		LDX	TEMP
		LDY	JOY1_VAR,X
		STY	TEMP
		STA	JOY1_VAR,X
		STA	JOY1_TRIG,X
		LDY	#6

loc_E76F:				; E781j
		LDA	2
		BIT	TEMP
		BEQ	loc_E77B
		
		LDA	JOY1_TRIG,X
		AND	1
		STA	JOY1_TRIG,X

loc_E77B:				; E773j
		SEC
		ROR	1
		LSR	2
		DEY
		BNE	loc_E76F
; End of function sub_E750

; 7F       80
; 01111111 10000000
; 01111111 11000000
; 10111111 11100000
; 11011111 11110000
; 11101111 11111000
; 11110111 11111100
; 11111011 11111110


; =============== S U B	R O U T	I N E =======================================

; 3f, 40 ?
sub_E783:				; sub_E731p
		lda $69
		cmp #$e
		bne joymode_game
		LDA	#$1F
		STA	TEMP_HI
		LDA	#$20
		STA	PROC_POINTER
		rts
joymode_game:		
		LDA	#$7F ; $7f default
		STA	TEMP_HI
		LDA	#$80 ; $80 default
		STA	PROC_POINTER
		RTS
; End of function sub_E783

; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR _do_load_ppu_data

loc_E78C:

		STA	PPU_ADDR	; Write	high byte of PPU address.
		INY			; Increase pointer.
		LDA	(TEMP),Y	; Load low byte	of PPU address
		STA	PPU_ADDR	; and write it to PPU_ADDR.
		INY
		LDA	(TEMP),Y	; Load next byte: tile counter.
		ASL	A		; Depending on high #7 bit
					; set (or not to set) carry flag:
					; 1 = autoincrement 32
					; 0 = autoincrement 1
		PHA
		LDA	PPU_CTRL_VAR
		ORA	#4		; Set PPU autoincrement	to 32
		BCS	loc_E7A2	; Let be so, if	carry (bit #7 in data byte).
		AND	#$FB		; BUT! If NOT carry, wipe PPU_CTRL_VAR bit #2
					; 1111_1011:
					; So now PPU autoincrement is 1.

loc_E7A2:				; E79Ej
		STA	PPU_CTRL	; Update PPU control reg
		STA	PPU_CTRL_VAR	; And store it.
		PLA			; Restore shifted data.
		ASL	A		; Shift	it again.
		BCC	shift_byte_back	; If bit #6 disabled, jump
		ORA	#2		; Set bit #2 for carry.
		INY

shift_byte_back:			; E7A9j
		LSR	A		; Shift	it to the right	twice to "normalize"
		LSR	A		; (bits	#7 and #6 are cleared)
					; and carry contains bit #6 again.
		TAX			; Repeat %counter% times

loc_E7B1:				; E7BAj
		BCS	loc_E7B4
		INY

loc_E7B4:				; loc_E7B1j
		LDA	(TEMP),Y
		STA	PPU_DATA
		DEX
		BNE	loc_E7B1
		SEC
		TYA
		ADC	TEMP
		STA	TEMP
		LDA	#0
		ADC	TEMP_HI
		STA	TEMP_HI		; Increment the	pointer	by the number of %counter%.
; END OF FUNCTION CHUNK	FOR _do_load_ppu_data ;
					; And do it over again:
					; read 2 bytes PPU address,
					; read %counter%,
					; fill PPU with	data.

; =============== S U B	R O U T	I N E =======================================

; IN: TEMP = screen gfx	data

_do_load_ppu_data:
; FUNCTION CHUNK AT E78C SIZE 0000003C BYTES

		LDX	PPU_STATUS
		LDY	#0
		LDA	(TEMP),Y	; Read value from 0 (indirect)
		BNE	loc_E78C	; If this value	not zero, jump to processing.
		LDA	SCROLL_X	; Else load the	scrolling register with	the contents of	$12
		STA	PPU_SCROLL
		LDA	SCROLL_Y	; and $13.
		STA	PPU_SCROLL	; And let's get out of here.
		RTS
; End of function _do_load_ppu_data


; =============== S U B	R O U T	I N E =======================================


sub_E7DC:				; ED6Ap ED7Bp
		TXA
		PHA
		TYA
		PHA
		LDY	#0
		LDA	(PROC_POINTER),Y
		AND	#$F
		STA	5
		LDA	(PROC_POINTER),Y
		LSR	A
		LSR	A
		LSR	A
		LSR	A
		STA	4
		LDX	NT_ADDR_HI

loc_E7F3:				; E82Cj
		LDA	1
		STA	NT_ADDR_LO,X
		JSR	sub_E838
		LDA	0
		STA	NT_ADDR_LO,X
		JSR	sub_E838
		LDA	5
		STA	6
		STA	NT_ADDR_LO,X

loc_E80A:				; E815j
		JSR	sub_E838
		INY
		LDA	(PROC_POINTER),Y
		STA	NT_ADDR_LO,X
		DEC	6
		BNE	loc_E80A
		JSR	sub_E838
		STX	NT_ADDR_HI
		CLC
		LDA	#$20 ; ' '
		ADC	TEMP
		STA	TEMP
		LDA	#0
		ADC	TEMP_HI
		STA	TEMP_HI
		DEC	4
		BNE	loc_E7F3
		LDA	#0
		STA	NT_ADDR_LO,X
		PLA
		TAY
		PLA
		TAX
		RTS
; End of function sub_E7DC


; =============== S U B	R O U T	I N E =======================================


sub_E838:				; E7F8p E800p	...
		INX
		TXA
; End of function sub_E838


; =============== S U B	R O U T	I N E =======================================


sub_E83A:				; E887p
		CMP	#$3F ; '?'
		BCC	locret_E848
		LDX	NT_ADDR_HI
		LDA	#0
		STA	NT_ADDR_LO,X
		PLA
		PLA

locret_E848:				; E83Cj
		RTS
; End of function sub_E83A


; =============== S U B	R O U T	I N E =======================================


sub_E849:				; 986Ap 9884p	...
		LDX	TEMP
		JSR	sub_E855
		LDA	TEMP
		LSR	A
		LSR	A
		LSR	A
		LSR	A
		TAX
; End of function sub_E849


; =============== S U B	R O U T	I N E =======================================


sub_E855:				; E84Bp
		INX
		TXA
		AND	#$F
		CMP	#9
		BCS	loc_E8C9
		ASL	A
		ASL	A
		TAY
		STA	PROC_POINTER
		LDX	$300
		LDA	$550,Y
		STA	$301,X
		JSR	sub_E838
		INY
		LDA	$550,Y
		STA	$301,X
		JSR	sub_E838
		INY
		LDA	$550,Y
		AND	#7
		STA	$301,X
		STA	TEMP_HI
		TXA
		SEC
		ADC	TEMP_HI
		JSR	sub_E83A
		TAX
		STX	$300
		LDA	#0
		STA	$301,X
		INY
		LDA	$550,Y
		STA	PROC_POINTER_HI

loc_E899:				; E8B6j
		DEX
		LDA	$29,Y
		AND	#$F
		STA	$301,X
		DEC	TEMP_HI
		BEQ	loc_E8B8
		DEX
		LDA	$29,Y
		AND	#$F0 ; 'ð'
		LSR	A
		LSR	A
		LSR	A
		LSR	A
		STA	$301,X
		DEY
		DEC	TEMP_HI
		BNE	loc_E899

loc_E8B8:				; E8A4j
		LDA	3
		AND	#1
		BEQ	loc_E8C9
		LDY	2
		CLC
		LDA	$29,Y
		ADC	#$F8 ; 'ø'
		STA	$301,X

loc_E8C9:				; E85Bj E8BCj
		LDY	#2

loc_E8CB:				; E8D7j
		LDA	$301,X
		BNE	locret_E8D9
		LDA	#$8E ; 'Ž'
		STA	$301,X
		INX
		DEY
		BPL	loc_E8CB

locret_E8D9:				; E8CEj
		RTS
; End of function sub_E855


; =============== S U B	R O U T	I N E =======================================


sub_E8DA:				; 9FF7p
		LDX	#$FF
		BNE	loc_E8E0
; End of function sub_E8DA


; =============== S U B	R O U T	I N E =======================================


sub_E8DE:				; 9E75p A005p	...
		LDX	#0

loc_E8E0:				; E8DCj
		STX	PTR1
		LDX	#0
		STX	PTR1_HI
		STX	6
		STX	7
		LDA	TEMP_HI
		AND	#8
		BNE	loc_E8F1
		INX

loc_E8F1:				; E8EEj
		LDA	0
		STA	6,X
		LDA	TEMP_HI
		JMP	loc_E8FA
; ---------------------------------------------------------------------------

loc_E8FA:				; E8F7j
		AND	#7
		ASL	A
		ASL	A
		TAX
		LDA	PTR1
		BEQ	loc_E925
		LDA	$2D,X
		BEQ	loc_E929

loc_E907:				; E927j
		CLC
		JSR	sub_E9C9
		JSR	sub_E961
		JSR	sub_E9B6
		STA	PROC_POINTER_HI
		LDA	6
		JSR	sub_E961
		JSR	sub_E9BE
		STA	PROC_POINTER_HI
		LDA	PTR1_HI
		JSR	sub_E961
		JMP	loc_E9C6
; ---------------------------------------------------------------------------

loc_E925:				; E901j
		LDA	$2D,X
		BEQ	loc_E907

loc_E929:				; E905j
		SEC
		JSR	sub_E9C9
		JSR	sub_E9B3
		STA	PROC_POINTER_HI
		LDA	6
		JSR	sub_E9BB
		STA	PROC_POINTER_HI
		LDA	PTR1_HI
		JSR	sub_E9C3
		BNE	loc_E948
		LDA	$2F,X
		BNE	loc_E948
		LDA	$30,X
		BEQ	loc_E94E

loc_E948:				; E93Ej E942j
		BCS	locret_E960
		LDA	$2D,X
		EOR	#$FF

loc_E94E:				; E946j
		STA	$2D,X
		SEC
		LDA	#0
		STA	PROC_POINTER_HI
		LDA	$30,X
		JSR	sub_E9B3
		JSR	sub_E9BB
		JSR	sub_E9C3

locret_E960:				; loc_E948j
		RTS
; End of function sub_E8DE


; =============== S U B	R O U T	I N E =======================================


sub_E961:				; E90Bp E915p	...
		JSR	sub_E9A4
		ADC	TEMP_HI
		CMP	#$A
		BCC	loc_E96C
		ADC	#5

loc_E96C:				; E968j
		CLC
		ADC	PROC_POINTER
		STA	PROC_POINTER
		LDA	PROC_POINTER_HI
		AND	#$F0 ; 'ð'
		ADC	PROC_POINTER
		BCC	loc_E97D

loc_E979:				; E97Fj
		ADC	#$5F ; '_'
		SEC
		RTS
; ---------------------------------------------------------------------------

loc_E97D:				; E977j
		CMP	#$A0 ; ' '
		BCS	loc_E979
		RTS
; End of function sub_E961


; =============== S U B	R O U T	I N E =======================================


sub_E982:				; sub_E9B3p sub_E9BBp	...
		JSR	sub_E9A4
		SBC	1
		STA	1
		BCS	loc_E995
		ADC	#$A
		STA	1
		LDA	2
		ADC	#$F
		STA	2

loc_E995:				; E989j
		LDA	3
		AND	#$F0 ; 'ð'
		SEC
		SBC	2
		BCS	loc_E9A1
		ADC	#$A0 ; ' '
		CLC

loc_E9A1:				; E99Cj
		ORA	1
		RTS
; End of function sub_E982


; =============== S U B	R O U T	I N E =======================================


sub_E9A4:				; sub_E961p sub_E982p
		PHA
		AND	#$F
		STA	1
		PLA
		AND	#$F0 ; 'ð'
		STA	2
		LDA	3
		AND	#$F
		RTS
; End of function sub_E9A4


; =============== S U B	R O U T	I N E =======================================


sub_E9B3:				; E92Dp E957p
		JSR	sub_E982
; End of function sub_E9B3


; =============== S U B	R O U T	I N E =======================================


sub_E9B6:				; E90Ep
		STA	$30,X
		LDA	$2F,X
		RTS
; End of function sub_E9B6


; =============== S U B	R O U T	I N E =======================================


sub_E9BB:				; E934p E95Ap
		JSR	sub_E982
; End of function sub_E9BB


; =============== S U B	R O U T	I N E =======================================


sub_E9BE:				; E918p
		STA	$2F,X
		LDA	$2E,X
		RTS
; End of function sub_E9BE


; =============== S U B	R O U T	I N E =======================================


sub_E9C3:				; E93Bp E95Dp
		JSR	sub_E982

loc_E9C6:				; E922j
		STA	$2E,X
		RTS
; End of function sub_E9C3


; =============== S U B	R O U T	I N E =======================================


sub_E9C9:				; E908p E92Ap
		LDA	$30,X
		STA	3
		LDA	7
		RTS
; End of function sub_E9C9


; =============== S U B	R O U T	I N E =======================================


sub_E9D0:				; 987Dp 9FDAp	...
		LDA	#0
		STA	4
		CLC
		LDA	0
		ADC	#$10
		AND	#$F0 ; 'ð'
		LSR	A
		LSR	A
		TAY
		LDA	0
		AND	#7
		ASL	A
		ASL	A
		TAX

loc_E9E5:				; EA38j
		LDA	$29,Y
		BEQ	loc_EA3B
		LDA	$2D,X
		BEQ	loc_EA14

loc_E9EE:				; EA3Dj
		SEC
		LDA	$2C,Y
		STA	3
		LDA	$30,X
		JSR	sub_E982
		LDA	$2B,Y
		STA	3
		LDA	$2F,X
		JSR	sub_E982
		LDA	$2A,Y
		STA	3
		LDA	$2E,X
		JSR	sub_E982
		BCS	loc_EA3F
		LDA	$29,Y
		BNE	loc_EA44

loc_EA14:				; E9ECj EA42j
		LDA	#$FF
		STA	4
		SEC

loc_EA19:				; EA45j
		TYA
		BNE	locret_EA3A
		BCC	loc_EA2E
		LDA	$2D,X
		STA	$29
		LDA	$2E,X
		STA	$2A
		LDA	$2F,X
		STA	$2B
		LDA	$30,X
		STA	$2C

loc_EA2E:				; EA1Cj
		LDA	0
		AND	#8
		BEQ	locret_EA3A
		DEX
		DEX
		DEX
		DEX
		BPL	loc_E9E5

locret_EA3A:				; EA1Aj EA32j
		RTS
; ---------------------------------------------------------------------------

loc_EA3B:				; E9E8j
		LDA	$2D,X
		BEQ	loc_E9EE

loc_EA3F:				; EA0Dj
		LDA	$29,Y
		BNE	loc_EA14

loc_EA44:				; EA12j
		CLC
		BCC	loc_EA19
; End of function sub_E9D0


; =============== S U B	R O U T	I N E =======================================


sub_EA47_decrease_timers:				; 810Dp
		LDX	#$10
		DEC	TIMER_01
		BPL	loc_EA53	; branch if TIMER_01 > 0 -> check 16 bytes from $3E
		LDA	#$14 		; else
		STA	TIMER_01	; init TIMER_01 with $14 (High Score and Main Menu scenes timer)
		LDX	#$20		; Check 32 bytes from $3E

loc_EA53:				; EA4Bj EA5Aj
		LDA	$3E,X
		BEQ	loc_EA59	; if zero, process next byte
		DEC	$3E,X 		; if NOT zero, decrease itself

loc_EA59:				; EA55j
		DEX
		BPL	loc_EA53
		RTS
; End of function sub_EA47


; =============== S U B	R O U T	I N E =======================================


sub_EA5D:				; E1DDp
		LDA	$15
		BEQ	loc_EA6A
		LDX	#2

loc_EA63:				; EA68j
		LDA	$32,X
		STA	$2E,X
		DEX
		BPL	loc_EA63

loc_EA6A:				; EA5Fj
		LDY	#0
		LDX	#0

loc_EA6E:				; EA83j
		LDA	$2E,X
		LSR	A
		LSR	A
		LSR	A
		LSR	A
		STA	$581,Y
		LDA	$2E,X
		AND	#$F
		STA	$582,Y
		INY
		INY
		INX
		CPX	#3
		BNE	loc_EA6E
		LDA	$581
		ORA	$582
		ORA	$583
		BEQ	loc_EAB2
		LDA	#5
		STA	$587
		LDA	#0

loc_EA97:				; EAB0j
		STA	$588
		TAX
		LDY	#0

loc_EA9D:				; EABCj
		LDA	$581,Y
		CMP	$600,X
		BEQ	loc_EAB8
		BCS	loc_EABE
		LDA	#6
		CLC
		ADC	$588
		DEC	$587
		BNE	loc_EA97

loc_EAB2:				; EA8Ej
		LDA	#$FF
		STA	$589
		RTS
; ---------------------------------------------------------------------------

loc_EAB8:				; EAA3j
		INX
		INY
		CPY	#3
		BNE	loc_EA9D

loc_EABE:				; EAA5j
		LDA	#5
		SEC
		SBC	$587
		STA	$589
		LDA	#2
		STA	$580
		RTS
; End of function sub_EA5D


; =============== S U B	R O U T	I N E =======================================


sub_EACD:				; E20Bp
		LDX	#4
		LDA	$589
		JSR	sub_EDB7
		LDX	#$1E

loc_EAD7:				; EAE0j
		DEX
		LDA	$600,X
		STA	$606,X
		CPX	0
		BNE	loc_EAD7
		LDY	#0
		LDX	0

loc_EAE6:				; EAF0j
		LDA	$581,Y
		STA	$600,X
		INX
		INY
		CPY	#6
		BNE	loc_EAE6
		LDX	#1
		LDA	$589
		JSR	sub_EDB7
		LDX	#$F

loc_EAFC:				; EB05j
		DEX
		LDA	$624,X
		STA	$627,X
		CPX	0
		BNE	loc_EAFC
		LDX	0
		LDY	#2
		LDA	#$24 ; '$'

loc_EB0D:				; EB12j
		STA	$624,X
		INX
		DEY
		BPL	loc_EB0D
		LDA	#3
		STA	$580
		RTS
; End of function sub_EACD

; ---------------------------------------------------------------------------
tbl_unk_EB1A:	.BYTE	0		; EC37r EC5Br
		.BYTE  $A
		.BYTE  $B
		.BYTE  $C
		.BYTE  $D
		.BYTE  $E
		.BYTE  $F
		.BYTE $10
		.BYTE $11
		.BYTE $12
		.BYTE $13
		.BYTE $14
		.BYTE $15
		.BYTE $16
		.BYTE $17
		.BYTE $18
		.BYTE $19
		.BYTE $1A
		.BYTE $1B
		.BYTE $1C
		.BYTE $1D
		.BYTE $1E
		.BYTE $1F
		.BYTE $20
		.BYTE $21 ; !
		.BYTE $22 ; "
		.BYTE $23 ; #
		.BYTE $2A ; *
		.BYTE $2D ; -
		.BYTE	1
		.BYTE	2
		.BYTE	0
tbl_unk_EB3A:	.BYTE	0		; ECAFr
tbl_unk_EB3B:	.BYTE	0		; ECE3r
		.BYTE $43 ; C
		.BYTE $33 ; 3
		.BYTE $43 ; C
		.BYTE $43 ; C
		.BYTE $43 ; C
		.BYTE $53 ; S
		.BYTE $43 ; C
		.BYTE $63 ; c
		.BYTE $43 ; C
		.BYTE $73 ; s
		.BYTE $43 ; C
		.BYTE $83 ; ƒ
		.BYTE $43 ; C
		.BYTE $93 ; “
		.BYTE $43 ; C
		.BYTE $A3 ; £
		.BYTE $43 ; C
		.BYTE $B3 ; ³
		.BYTE $43 ; C
		.BYTE $C3 ; Ã
		.BYTE $53 ; S
		.BYTE $33 ; 3
		.BYTE $53 ; S
		.BYTE $43 ; C
		.BYTE $53 ; S
		.BYTE $53 ; S
		.BYTE $53 ; S
		.BYTE $63 ; c
		.BYTE $53 ; S
		.BYTE $73 ; s
		.BYTE $53 ; S
		.BYTE $83 ; ƒ
		.BYTE $53 ; S
		.BYTE $93 ; “
		.BYTE $53 ; S
		.BYTE $A3 ; £
		.BYTE $53 ; S
		.BYTE $B3 ; ³
		.BYTE $53 ; S
		.BYTE $C3 ; Ã
		.BYTE $63 ; c
		.BYTE $33 ; 3
		.BYTE $63 ; c
		.BYTE $43 ; C
		.BYTE $63 ; c
		.BYTE $53 ; S
		.BYTE $63 ; c
		.BYTE $63 ; c
		.BYTE $63 ; c
		.BYTE $73 ; s
		.BYTE $63 ; c
		.BYTE $83 ; ƒ
		.BYTE $63 ; c
		.BYTE $93 ; “
		.BYTE $63 ; c
		.BYTE $A3 ; £
		.BYTE $63 ; c
		.BYTE $B3 ; ³
		.BYTE $63 ; c
		.BYTE $C3 ; Ã
tbl_unk_EB78:	.BYTE $2D, $50,	2, $80	; loc_EBB7r
tbl_unk_EB7C:	.BYTE $28 ; (		; loc_EBC7r
		.BYTE $40 ; @
		.BYTE	0
		.BYTE $40 ; @
		.BYTE $30 ; 0
		.BYTE $41 ; A
		.BYTE	0
		.BYTE $40 ; @
		.BYTE $38 ; 8
		.BYTE $42 ; B
		.BYTE	0
		.BYTE $40 ; @
		.BYTE $28 ; (
		.BYTE $43 ; C
		.BYTE	0
		.BYTE $48 ; H
		.BYTE $30 ; 0
		.BYTE $44 ; D
		.BYTE	0
		.BYTE $48 ; H
		.BYTE $38 ; 8
		.BYTE $45 ; E
		.BYTE	0
		.BYTE $48 ; H

; =============== S U B	R O U T	I N E =======================================


sub_EB94:				; E214p E22Dp

; FUNCTION CHUNK AT EBDE SIZE 0000003F BYTES
; FUNCTION CHUNK AT EC23 SIZE 00000110 BYTES

		LDY	$58A
		BNE	loc_EBDE
		STY	$58B
		STY	$591
		INY
		STY	$58C
		STY	$58D
		LDA	#$50 ; 'P'
		STA	$592
		LDA	#3
		STA	$590
		LDA	#$15
		STA	$58E
		LDY	#3

loc_EBB7:				; EBC3j
		LDA	tbl_unk_EB78,Y
		STA	$2C0,Y
		LDA	#$2F ; '/'
		STA	$594,Y
		DEY
		BPL	loc_EBB7
; End of function sub_EB94


; =============== S U B	R O U T	I N E =======================================


sub_EBC5:				; E29Bp
		LDY	#$17

loc_EBC7:				; EBCEj
		LDA	tbl_unk_EB7C,Y
		STA	$2DC,Y
		DEY
		BPL	loc_EBC7
		LDY	#$14
		LDA	$15

loc_EBD4:				; EBDBj
		STA	$2DE,Y
		DEY
		DEY
		DEY
		DEY
		BPL	loc_EBD4
		RTS
; End of function sub_EBC5

; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_EB94

loc_EBDE:				; EB97j
		DEC	$592
		BNE	loc_EBFD
		LDA	#$50 ; 'P'
		STA	$592
		DEC	$591
		BPL	loc_EBFA
		LDA	#9
		STA	$591
		DEC	$590
		BPL	loc_EBFA
		JMP	loc_EC7D
; ---------------------------------------------------------------------------

loc_EBFA:				; EBEBj EBF5j
		JSR	sub_ED55

loc_EBFD:				; EBE1j
		DEC	$58D
		BNE	loc_EC3F
		LDX	#1
		STX	$58D
		LDY	#1
		LDA	$17
		ORA	$19
		LSR	A
		BCS	loc_EC29
		LSR	A
		BCC	loc_EC1A
		LDX	#$FF
		LDY	#$1E
		JMP	loc_EC29
; ---------------------------------------------------------------------------

loc_EC1A:				; EC11j
		LSR	A
		BCC	loc_EC23
; END OF FUNCTION CHUNK FOR sub_EB94
        JSR     sub_ED33
		JMP     loc_EC29; ---------------------------------------------------------------------------
		JMP	loc_EC29
; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_EB94

loc_EC23:				; EC1Bj
		LSR	A
		BCC	loc_EC3F
		JSR	sub_ED44

loc_EC29:				; EC0Ej EC17j	...
		LDA	#$A
		STA	$58D
		TXA
		CLC
		ADC	$58C
		STA	$58C
		TAX
		LDA	tbl_unk_EB1A,X
		BNE	loc_EC3F
		STY	$58C

loc_EC3F:				; EC00j EC24j	...
		LDA	JOY1_TRIG
		ORA	$1A
		AND	#$C0 ; 'À'
		BEQ	loc_EC9F
		LDY	$58C
		CPY	#$1E
		BEQ	loc_EC7D
		CPY	#$1D
		BEQ	loc_EC88
		LDA	$F0
		ORA	#2
		STA	$F0
		LDX	$58B
		LDA	tbl_unk_EB1A,Y
		STA	$594,X
		INC	$58B
		CPX	#2
		BNE	loc_EC72
		DEC	$58B
		LDA	#$1E
		STA	$58C
		BNE	loc_EC9F

loc_EC72:				; EC66j
		CLC
		LDA	#8
		ADC	$2C3
		STA	$2C3
		BNE	loc_EC9F

loc_EC7D:				; EBF7j EC4Cj
		LDA	#$F8 ; 'ø'
		STA	$2C0
		LDA	#4
		STA	$580
		RTS
; ---------------------------------------------------------------------------

loc_EC88:				; EC50j
		LDY	$58B
		LDA	#$2F ; '/'
		STA	$594,Y
		DEY
		BMI	loc_EC9F
		DEC	$58B
		LDA	$2C3
		SEC
		SBC	#8
		STA	$2C3

loc_EC9F:				; EC45j EC70j	...
		LDA	#$D0 ; 'Ð'
		STA	0
		LDA	#$20 ; ' '
		STA	TEMP_HI
		JSR	sub_ED6E
		LDA	$58C
		ASL	A
		TAY
		LDA	tbl_unk_EB3A,Y
		STA	$2C4
		STA	$2CC
		CLC
		ADC	#8
		STA	$2C8
		STA	$2D0
		LDA	#$4E ; 'N'
		STA	$2C5
		STA	$2C9
		STA	$2CD
		STA	$2D1
		LDA	#$40 ; '@'
		STA	$2C6
		LDA	#$C0 ; 'À'
		STA	$2CA
		LDA	#0
		STA	$2CE
		LDA	#$80 ; '€'
		STA	$2D2
		LDA	tbl_unk_EB3B,Y
		STA	$2C7
		STA	$2CB
		CLC
		ADC	#8
		STA	$2CF
		STA	$2D3
		LDY	#7

loc_ECF7:				; ECFEj
		LDA	$2CC,Y
		STA	$2D4,Y
		DEY
		BPL	loc_ECF7
		LDA	$58C
		CMP	#$1D
		BCC	loc_ED20
		CLC
		LDA	#8
		ADC	$2CF
		STA	$2CF
		LDA	#8
		ADC	$2D3
		STA	$2D3
		LDA	#$4F ; 'O'
		STA	$2D5
		STA	$2D9

loc_ED20:				; ED05j
		DEC	$58E
		BNE	locret_ED32
		LDA	#$F
		STA	$58E
		LDA	#$D9 ; 'Ù'
		EOR	$2C0
		STA	$2C0

locret_ED32:				; ED23j
		RTS
; END OF FUNCTION CHUNK	FOR sub_EB94
; ---------------------------------------------------------------------------
sub_ED33:
		LDA	$58C
		CMP	#$15
		BCC	loc_ED41
		CMP	#$1F
		BCS	loc_ED41
		LDX	#0
		RTS
; ---------------------------------------------------------------------------

loc_ED41:				; ED38j ED3Cj
		LDX	#$A
		RTS

; =============== S U B	R O U T	I N E =======================================


sub_ED44:				; EC26p
		LDA	$58C
		CMP	#1
		BCC	loc_ED52
		CMP	#$B
		BCS	loc_ED52
		LDX	#0
		RTS
; ---------------------------------------------------------------------------

loc_ED52:				; ED49j ED4Dj
		LDX	#$F6 ; 'ö'
		RTS
; End of function sub_ED44


; =============== S U B	R O U T	I N E =======================================


sub_ED55:				; loc_EBFAp
		LDA	#$12
		STA	$58F
		LDA	#$F4 ; 'ô'
		STA	0
		LDA	#$21 ; '!'
		STA	1
		LDA	#$8F ; ''
		STA	2
		LDA	#5
		STA	3
		JSR	sub_E7DC
		RTS
; End of function sub_ED55


; =============== S U B	R O U T	I N E =======================================


sub_ED6E:				; ECA7p EDAEp
		LDA	#$13
		STA	$593
		LDA	#$93 ; '“'
		STA	2
		LDA	#5
		STA	3
		JSR	sub_E7DC
		RTS
; End of function sub_ED6E

; ---------------------------------------------------------------------------
tbl_unk_ED7F:	.BYTE $75, $B5,	$F5, $35, $75 ;	EDA4r
tbl_unk_ED84:	.BYTE $22, $22,	$22, $23, $23 ;	EDA9r

; =============== S U B	R O U T	I N E =======================================


sub_ED89:				; loc_E238p
		LDX	#1
		LDA	$589
		JSR	sub_EDB7
		LDY	#0
		LDX	TEMP

loc_ED95:				; ED9Fj
		LDA	$594,Y
		STA	$624,X
		INX
		INY
		CPY	#3
		BNE	loc_ED95
		LDY	$589
		LDA	tbl_unk_ED7F,Y
		STA	TEMP
		LDA	tbl_unk_ED84,Y
		STA	TEMP_HI
		JSR	sub_ED6E
		LDA	#5
		STA	$580
		RTS
; End of function sub_ED89


; =============== S U B	R O U T	I N E =======================================


sub_EDB7:				; EAD2p EAF7p	...
		STA	TEMP

loc_EDB9:				; EDBDj
		CLC
		ADC	TEMP
		DEX
		BPL	loc_EDB9
		STA	TEMP
		RTS
; End of function sub_EDB7


; =============== S U B	R O U T	I N E =======================================


sub_EDC2:				; 930Dp loc_E489p
		LDA	$589
		BNE	locret_EDE1
		TAY
		TAX

loc_EDC9:				; EDDFj
		LDA	$600,Y
		ASL	A
		ASL	A
		ASL	A
		ASL	A
		STA	$2A,X
		LDA	$601,Y
		CLC
		ADC	$2A,X
		STA	$2A,X
		INY
		INY
		INX
		CPX	#3
		BNE	loc_EDC9

locret_EDE1:				; EDC5j
		RTS
; End of function sub_EDC2

; ---------------------------------------------------------------------------
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF
;		.BYTE $FF, $FF,	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,	$FF, $FF, $FF, $FF
tbl_unk_EEE0:	.BYTE $F0, $F8,	$81, $30, 4, $36, $C2, $CF, $E3, $1C, $DC, $1A,	$3B, $C
					; loc_EF15r

; =============== S U B	R O U T	I N E =======================================


sub_EEEE:				; 9A16p
		LDX	#1

loc_EEF0:				; EF0Bj
		LDA	tbl_dmp02,X
		STA	$4F1,X
		LDA	tbl_dmp03,X
		STA	$4F3,X
		JSR	sub_EF1E
		LDA	#0

loc_EF01:
		STA	$4ED,X
		TAY
		LDA	(2),Y
		STA	$4EF,X
		DEX
		BPL	loc_EEF0
		LDA	#0
		STA	$99
		STA	$9A
		LDX	#$D

loc_EF15:				; EF1Bj
		LDA	tbl_unk_EEE0,X
		STA	$1B,X
		DEX
		BPL	loc_EF15
		RTS
; End of function sub_EEEE


; =============== S U B	R O U T	I N E =======================================


sub_EF1E:				; EEFCp loc_EF35p
		LDA	tbl_dmp00,X
		STA	TEMP
		LDA	$4F1,X
		STA	TEMP_HI
		LDA	tbl_dmp01,X
		STA	PROC_POINTER
		LDA	$4F3,X
		STA	PROC_POINTER_HI
		RTS
; End of function sub_EF1E


; =============== S U B	R O U T	I N E =======================================


sub_EF33:				; E31Bp
		LDX	#1

loc_EF35:				; EF65j
		JSR	sub_EF1E
		LDY	$4ED,X
		LDA	$4EF,X
		BNE	loc_EF58
		INY
		INC	$4ED,X
		BNE	loc_EF4C
		INC	$4F1,X
		INC	$4F3,X

loc_EF4C:				; EF44j
		LDA	(2),Y
		STA	$4EF,X
		BNE	loc_EF58
		STA	$C3
		STA	$4ED,X

loc_EF58:				; EF3Ej EF51j
		LDA	(TEMP),Y
		STA	$80,X
		LDA	$4EF,X
		BEQ	loc_EF64
		DEC	$4EF,X

loc_EF64:				; EF5Fj
		DEX
		BPL	loc_EF35
		RTS
; End of function sub_EF33



	    ; .segment "MUSICPRG"


; START	OF FUNCTION CHUNK FOR sub_F6ED

loc_F000:				; F037j
		JSR	sub_F2B1

locret_F003:				; F012j
		RTS
; ---------------------------------------------------------------------------

loc_F004:				; loc_F72Aj
		LDA	#0
		TAX
		STA	$FD
		BEQ	loc_F01B

loc_F00B:				; F028j
		TXA
		LSR	A
		TAX

loc_F00E:				; F02Fj F0B6j
		INX
		TXA
		CMP	#4
		BEQ	locret_F003
		LDA	$FD
		CLC
		ADC	#4
		STA	$FD

loc_F01B:				; F009j
		TXA
		ASL	A
		TAX
		LDA	$E0,X
		STA	$FE
		LDA	$E1,X
		STA	$FF
		LDA	$E1,X
		BEQ	loc_F00B
		TXA
		LSR	A
		TAX
		DEC	$D0,X
		BNE	loc_F00E

loc_F031:				; loc_F05Ej
		LDY	$E8,X
		INC	$E8,X
		LDA	($FE),Y
		BEQ	loc_F000
		TAY
		CMP	#$FF
		BEQ	loc_F047
		AND	#$C0 ; 'À'
		CMP	#$C0 ; 'À'
		BEQ	loc_F053
		JMP	loc_F061
; ---------------------------------------------------------------------------

loc_F047:				; F03Cj
		LDA	$D8,X
		BEQ	loc_F05E
		DEC	$D8,X
		LDA	$EC,X
		STA	$E8,X
		BNE	loc_F05E

loc_F053:				; F042j
		TYA
		AND	#$3F ; '?'
		STA	$D8,X
		DEC	$D8,X
		LDA	$E8,X
		STA	$EC,X

loc_F05E:				; F049j F051j
		JMP	loc_F031
; ---------------------------------------------------------------------------

loc_F061:				; F044j
		TYA
		BPL	loc_F07B
		AND	#$F
		CLC
		ADC	$DF
		TAY
		LDA	tbl_unk_F172,Y
		STA	$D4,X
		TAY
		TXA
		CMP	#2
		BEQ	loc_F0C4

loc_F075:				; F0DEj
		LDY	$E8,X
		INC	$E8,X
		LDA	($FE),Y

loc_F07B:				; F062j
		TAY
		TXA
		CMP	#3
		BEQ	loc_F0E1
		PHA
		TAX
		CMP	#1
		BEQ	loc_F0B9

loc_F087:				; F0BDj
		LDX	$FD
		LDA	tbl_unk_F101,Y
		BEQ	loc_F099
		STA	APU_PL1_LO,X
		LDA	tbl_unk_F100,Y
		ORA	#8
		STA	APU_PL1_HI,X

loc_F099:				; F08Cj
		TAY
		PLA
		TAX
		TYA
		BNE	loc_F0AA
		LDY	#0
		TXA
		CMP	#2
		BEQ	loc_F0AC
		LDY	#$10
		BNE	loc_F0AC

loc_F0AA:				; F09Dj
		LDY	$DC,X

loc_F0AC:				; F0A4j F0A8j
		TYA
		LDY	$FD
		STA	APU_PL1_VOL,Y

loc_F0B2:				; F0C1j loc_F0F9j
		LDA	$D4,X
		STA	$D0,X
		JMP	loc_F00E
; ---------------------------------------------------------------------------

loc_F0B9:				; F085j
		LDA	$F5
		AND	#2
		BEQ	loc_F087
		PLA
		TAX
		JMP	loc_F0B2
; ---------------------------------------------------------------------------

loc_F0C4:				; F073j
		TYA
		LDY	$7F0
		BEQ	loc_F0CE
		LDA	#$FF
		BNE	loc_F0D9

loc_F0CE:				; F0C8j
		CLC
		ADC	#$FE ; 'þ'
		ASL	A
		ASL	A
		CMP	#$3C ; '<'
		BCC	loc_F0D9
		LDA	#$3C ; '<'

loc_F0D9:				; F0CCj F0D5j
		STA	$4008
		STA	$DE
		JMP	loc_F075
; ---------------------------------------------------------------------------

loc_F0E1:				; F07Fj
		LDA	$F4
		CMP	#2
		BEQ	loc_F0F9
		LDA	tbl_unk_F200,Y
		STA	APU_NOISE_VOL
		LDA	tbl_unk_F201,Y
		STA	APU_NOISE_LO
		LDA	tbl_unk_F202,Y
		STA	APU_NOISE_HI

loc_F0F9:				; F0E5j
		JMP	loc_F0B2
; END OF FUNCTION CHUNK	FOR sub_F6ED
; ---------------------------------------------------------------------------
		.BYTE $FF
		.BYTE $FF
		.BYTE $FF
		.BYTE $FF
tbl_unk_F100:	.BYTE	7		; F091r
tbl_unk_F101:	.BYTE $F0 ; ð		; F089r
		.BYTE	0
		.BYTE	0
		.BYTE	0
		.BYTE $D4 ; Ô
		.BYTE	0
		.BYTE $C8 ; È
		.BYTE	0
		.BYTE $BD ; ½
		.BYTE	0
		.BYTE $B2 ; ²
		.BYTE	0
		.BYTE $A8 ; ¨
		.BYTE	0
		.BYTE $9F ; Ÿ
		.BYTE	0
		.BYTE $96 ; –
		.BYTE	0
		.BYTE $8D ; 
		.BYTE	0
		.BYTE $85 ; …
		.BYTE	0
		.BYTE $7E ; ~
		.BYTE	0
		.BYTE $76 ; v
		.BYTE	0
		.BYTE $70 ; p
		.BYTE	1
		.BYTE $AB ; «
		.BYTE	1
		.BYTE $93 ; “
		.BYTE	1
		.BYTE $7C ; |
		.BYTE	1
		.BYTE $67 ; g
		.BYTE	1
		.BYTE $52 ; R
		.BYTE	1
		.BYTE $3F ; ?
		.BYTE	1
		.BYTE $2D ; -
		.BYTE	1
		.BYTE $1C
		.BYTE	1
		.BYTE  $C
		.BYTE	0
		.BYTE $FD ; ý
		.BYTE	0
		.BYTE $EE ; î
		.BYTE	0
		.BYTE $E1 ; á
		.BYTE	3
		.BYTE $57 ; W
		.BYTE	3
		.BYTE $27 ; '
		.BYTE	2
		.BYTE $F9 ; ù
		.BYTE	2
		.BYTE $CF ; Ï
		.BYTE	2
		.BYTE $A6 ; ¦
		.BYTE	2
		.BYTE $80 ; €
		.BYTE	2
		.BYTE $5C ; \
		.BYTE	2
		.BYTE $3A ; :
		.BYTE	2
		.BYTE $1A
		.BYTE	1
		.BYTE $FC ; ü
		.BYTE	1
		.BYTE $DF ; ß
		.BYTE	1
		.BYTE $C4 ; Ä
		.BYTE	6
		.BYTE $AE ; ®
		.BYTE	5
		.BYTE $F3 ; ó
		.BYTE	5
		.BYTE $9E ; ž
		.BYTE	5
		.BYTE $4D ; M
		.BYTE	5
		.BYTE	1
		.BYTE	4
		.BYTE $B9 ; ¹
		.BYTE	4
		.BYTE $75 ; u
		.BYTE	3
		.BYTE $F8 ; ø
		.BYTE	3
		.BYTE $BF ; ¿
		.BYTE	3
		.BYTE $89 ; ‰
		.BYTE	0
		.BYTE $69 ; i
		.BYTE	0
		.BYTE $63 ; c
		.BYTE	0
		.BYTE $5E ; ^
		.BYTE	0
		.BYTE $58 ; X
		.BYTE	0
		.BYTE $53 ; S
		.BYTE	0
		.BYTE $4F ; O
		.BYTE	0
		.BYTE $4A ; J
		.BYTE	0
		.BYTE $46 ; F
		.BYTE	0
		.BYTE $42 ; B
tbl_unk_F172:	.BYTE	3		; F06Ar
		.BYTE	6
		.BYTE  $C
		.BYTE $18
		.BYTE $30 ; 0
		.BYTE $12
		.BYTE $24 ; $
		.BYTE	9
		.BYTE	8
		.BYTE	4
		.BYTE	7
		.BYTE	1
		.BYTE	4
		.BYTE	8
		.BYTE $10
		.BYTE $20
		.BYTE $40 ; @
		.BYTE $18
		.BYTE $30 ; 0
		.BYTE  $C
		.BYTE	1
		.BYTE	6
		.BYTE  $C
		.BYTE $18
		.BYTE $30 ; 0
		.BYTE $60 ; `
		.BYTE $24 ; $
		.BYTE $48 ; H
		.BYTE $12
		.BYTE $10
		.BYTE	8
		.BYTE  $E
		.BYTE	2
		.BYTE	3
		.BYTE	4

; =============== S U B	R O U T	I N E =======================================


sub_F195:				; F232p F259p	...
		LDA	#0
		BEQ	loc_F1A3
; End of function sub_F195


; =============== S U B	R O U T	I N E =======================================


sub_F199:				; loc_F6A5p
		LDA	#8
		BNE	loc_F1A3
; End of function sub_F199


; =============== S U B	R O U T	I N E =======================================


sub_F19D:				; loc_F3DBp
		LDA	#$C
		BNE	loc_F1A3
; End of function sub_F19D


; =============== S U B	R O U T	I N E =======================================


sub_F1A1:				; F437p F499p	...
		LDA	#4

loc_F1A3:				; F197j F19Bj	...
		STA	$F9
		LDA	#$40 ; '@'
		STA	$FA
		STX	$FB
		STY	$FC
		LDY	#0

loc_F1AF:				; F1B7j
		LDA	($FB),Y
		STA	($F9),Y
		INY
		TYA
		CMP	#4
		BNE	loc_F1AF
		RTS
; End of function sub_F1A1


; =============== S U B	R O U T	I N E =======================================


init_music:				; loc_F749p loc_F762p	...
		

		TAX
		JSR	sub_F2BB
		STX	$F6
		LDA	$7F5
		BEQ	loc_F1D0
		CMP	#2
		BNE	loc_F1D0
		STA	$F0
		LDA	#0
		STA	$7F5

loc_F1D0:				; F1C3j F1C7j
		LDA	mus_offsets,Y
		TAY
		LDX	#0

loc_F1D6:				; F1E0j
		LDA	mus_offsets,Y
		STA	$DF,X
		INY
		INX
		TXA
		CMP	#9
		BNE	loc_F1D6
		LDA	#1
		STA	$D0
		STA	$D1
		STA	$D2
		STA	$D3
		LDA	#0
		STA	$E8
		STA	$E9
		STA	$EA
		STA	$EB
		RTS
; End of function init_music

; ---------------------------------------------------------------------------
tbl_unk_F200:	.BYTE $10
tbl_unk_F201:	.BYTE	0
tbl_unk_F202:	.BYTE $18
				.BYTE $10, 1, $18, 0, 1, $88, 2, 2, $40, 3, 5, $40, 4, 7, $40

; =============== S U B	R O U T	I N E =======================================


sub_F212:
		LDA	#$7F
		STA	APU_PL1_SWEEP
		STA	APU_PL2_SWEEP
; End of function sub_F212


; =============== S U B	R O U T	I N E =======================================


sub_F21A:				; loc_F769p
		STX	$DC
		STY	$DD

locret_F21E:				; F224j
		RTS
; End of function sub_F21A


; =============== S U B	R O U T	I N E =======================================


sub_F21F:				; F24Cp
		LDA	$7E7
		CMP	#$FD ; 'ý'
		BCS	locret_F21E
		INC	$7E7
		INC	$7E7
		RTS
; End of function sub_F21F

; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_F246

loc_F22D:				; F24Aj
		NOP
		LDX	#<tbl_F261
		LDY	#>tbl_F261
		JSR	sub_F195
		LDA	$7E7
		CMP	#$88 ; 'ˆ'
		BCC	loc_F242
		CLC
		SBC	#$18
		STA	$7E7

loc_F242:				; F23Aj
		STA	APU_PL1_LO
		RTS
; END OF FUNCTION CHUNK	FOR sub_F246

; =============== S U B	R O U T	I N E =======================================


sub_F246:				; loc_F482p

; FUNCTION CHUNK AT F22D SIZE 00000019 BYTES

		LDA	$F0
		AND	#$10
		BNE	loc_F22D
		JSR	sub_F21F
		LDA	$F3
		LSR	A
		BCS	loc_F255
		RTS
; ---------------------------------------------------------------------------

loc_F255:				; F252j
		LDX	#<tbl_F25D
		LDY	#>tbl_F25D
		JSR	sub_F195
		RTS
; End of function sub_F246

; ---------------------------------------------------------------------------
tbl_F25D:	.BYTE $1F		; loc_F255t F257t
		.BYTE $FF
		.BYTE $10
		.BYTE $C5 ; Å
tbl_F261:	.BYTE $96 ; –		; F22Et F230t
		.BYTE $AB ; «
		.BYTE $FD ; ý
		.BYTE $78 ; x

; =============== S U B	R O U T	I N E =======================================


sub_F265:				; F278p
		LDA	$7E6
		BNE	locret_F299
		INC	$7E6
		LDA	#$FD ; 'ý'
		STA	$7E7
		RTS
; End of function sub_F265


; =============== S U B	R O U T	I N E =======================================


play_music:				; j_play_musicj
		LDA	#$C0 ; 'À'
		STA	CTRL_PORT2
		JSR	sub_F265
		JSR	sub_F6ED
		JSR	sub_F447
		JSR	sub_F598
		JSR	sub_F64F
		JSR	sub_F34E
		LDA	$F1
		STA	$7E9
		LDA	#0
		STA	$F0
		STA	$F1
		STA	$F2
		STA	$F3

locret_F299:				; F268j F2A2j
		RTS
; End of function play_music

; ---------------------------------------------------------------------------
tbl_F29A:	.BYTE 0, $7F, 4, $18	; F2AAt F2ACt
; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_F34E

loc_F29E:				; loc_F386j
		LDA	$F4
		AND	#6
		BNE	locret_F299
		LDA	$F4
		AND	#$F0 ; 'ð'
		STA	$F4
		LDX	#<tbl_F29A
		LDY	#>tbl_F29A
		JMP	loc_F3DB
; END OF FUNCTION CHUNK	FOR sub_F34E

; =============== S U B	R O U T	I N E =======================================


sub_F2B1:				; loc_F000p
		LDA	$F6
		CMP	#$F0 ; 'ð'
		BNE	sub_F2BF
		INC	$7E8
		RTS
; End of function sub_F2B1


; =============== S U B	R O U T	I N E =======================================


sub_F2BB:				; F1BBp

; FUNCTION CHUNK AT F2E4 SIZE 0000001A BYTES

		CMP	#$EF
		BEQ	loc_F2E4
; End of function sub_F2BB


; =============== S U B	R O U T	I N E =======================================


sub_F2BF:				; F2B5j F2E8j	...
		LDA	#$10
		STA	APU_NOISE_VOL

loc_F2C4:				; F2ECj
		STA	APU_PL1_VOL
		STA	APU_PL2_VOL
		LDA	#0
		STA	$7E8
		STA	$F4
		STA	$F5
		STA	$F6
		STA	$7FA
		STA	$F7
		STA	APU_TRI_LINEAR
		STA	APU_DMC_RAW
		STA	$7F0
		RTS
; End of function sub_F2BF

; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_F2BB

loc_F2E4:				; F2BDj
		LDA	$F4
		CMP	#4
		BEQ	sub_F2BF
		LDA	#$10
		BNE	loc_F2C4
; ---------------------------------------------------------------------------
tbl_F2EE:	.BYTE $3F ; ?		; F3A9t F3ABt
		.BYTE $7F ; 
		.BYTE	0
		.BYTE	0
snd_pop_balloon:
		.BYTE	6
		.BYTE $7F
		.BYTE  $A
		.BYTE $C0
snd_pop_bubble:
		.BYTE	2
		.BYTE $7F
		.BYTE  $A
		.BYTE $C0
tbl_F2F6:	.BYTE	8		; F31Ft F321t
		.BYTE $7F ; 
		.BYTE  $A
		.BYTE $C0 ; À
tbl_F2FA:	.BYTE	8		; F303t F305t
		.BYTE $7F ; 
		.BYTE	5
		.BYTE $C0 ; À
; END OF FUNCTION CHUNK	FOR sub_F2BB
; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_F34E

loc_F2FE:				; F312j
		LDA	#0
		STA	$7FA
		LDX	#<tbl_F2FA
		LDY	#>tbl_F2FA
		JMP	loc_F3DB
; ---------------------------------------------------------------------------

loc_F30A:				; F36Ej
		INC	$7FB
		LDA	$7FB
		CMP	#$10
		BEQ	loc_F2FE
		RTS
; ---------------------------------------------------------------------------

loc_F315:				; F36Aj
		LDA	#0
		STA	$7FB
		LDA	#$F0 ; 'ð'
		STA	$7FA
		LDX	#<tbl_F2F6
		LDY	#>tbl_F2F6
		JMP	loc_F3DB
; ---------------------------------------------------------------------------

loc_F326:				; F374j
		LDA	$F4
		AND	#$F0 ; 'ð'
		ORA	#2
		STA	$F4
		LDA	#0
		STA	$7F7
		LDX	#<snd_pop_balloon
		LDY	#>snd_pop_balloon
		lda $6C
		bne :+
		lda bubble_flag
		beq :+
		LDX	#<snd_pop_bubble
		LDY	#>snd_pop_bubble
		lda #0		
		sta bubble_flag
:
		JMP	loc_F3DB
; ---------------------------------------------------------------------------

loc_F33A:				; F380j
		INC	$7F7
		LDA	$7F7
		CMP	#$10
		BNE	locret_F34A
		LDA	$F4
		AND	#$F0 ; 'ð'
		STA	$F4

locret_F34A:				; F342j
		RTS
; ---------------------------------------------------------------------------

loc_F34B:				; F351j
		JMP	sub_F2BF
; END OF FUNCTION CHUNK	FOR sub_F34E

; =============== S U B	R O U T	I N E =======================================


sub_F34E:				; F287p

; FUNCTION CHUNK AT F29E SIZE 00000013 BYTES
; FUNCTION CHUNK AT F2FE SIZE 00000050 BYTES

		LDA	$F0
		LSR	A
		BCS	loc_F34B
		LDA	$F6
		CMP	#$DF ; 'ß'
		BEQ	loc_F365
		CMP	#$7F ; ''
		BEQ	loc_F365
		CMP	#$20 ; ' '
		BEQ	loc_F365
		LDA	$F6
		BNE	locret_F385

loc_F365:				; F357j F35Bj	...
		LDA	$7FA
		CMP	#$F
		BEQ	loc_F315
		CMP	#$F0 ; 'ð'
		BEQ	loc_F30A
		LDA	$F0
		LSR	A
		LSR	A
		BCS	loc_F326 ; sound "pop balloon"
		LSR	A
		BCS	loc_F389
		LSR	A
		BCS	loc_F386
		LDA	$F4
		LSR	A
		LSR	A
		BCS	loc_F33A
		LSR	A
		BCS	loc_F3B0

locret_F385:				; F363j F38Dj
		RTS
; ---------------------------------------------------------------------------

loc_F386:				; F37Aj
		JMP	loc_F29E
; ---------------------------------------------------------------------------

loc_F389:				; F377j
		LDA	$F4
		AND	#$80 ; '€'
		BNE	locret_F385
		LDA	$F4
		AND	#$F0 ; 'ð'
		ORA	#4
		STA	$F4
		LDA	#3
		STA	$7FF
		LDA	#0
		STA	$7F3
		STA	$7F1
		LDA	#$10
		STA	$7F4
		LDX	#<tbl_F2EE
		LDY	#>tbl_F2EE
		BNE	loc_F3DB
		RTS
; ---------------------------------------------------------------------------

loc_F3B0:				; F383j
		INC	$7F3
		LDA	$7F3
		CMP	$7FF
		BNE	locret_F3D6
		LDA	#0
		STA	$7F3
		INC	$7F1
		LDA	$7F1
		CMP	$7F4
		BNE	loc_F3D7
		LDA	#$10
		STA	$400C
		LDA	$F4
		AND	#$F0 ; 'ð'
		STA	$F4

locret_F3D6:				; F3B9j
		RTS
; ---------------------------------------------------------------------------

loc_F3D7:				; F3C9j
		STA	APU_NOISE_LO
		RTS
; ---------------------------------------------------------------------------

loc_F3DB:				; F2AEj F307j	...
		JSR	sub_F19D
		RTS
; End of function sub_F34E

; ---------------------------------------------------------------------------
tbl_F3DF:	.BYTE $C1 ; Á		; loc_F425t F427t ...
		.BYTE $89 ; ‰
		.BYTE 2, $F
; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_F447

loc_F3E3:				; loc_F444j
		LDA	#0
		STA	$7E0
		CLC
		LDA	$1B
		AND	#7
		ADC	#2
		STA	$7E1
		LDA	$F7
		AND	#$F
		ORA	#$80 ; '€'
		STA	$F7
		BNE	loc_F425

loc_F3FC:				; loc_F486j
		INC	$7E0
		LDA	$7E0
		CMP	$7E1
		BNE	loc_F425

loc_F407:				; loc_F50Dj
		LDA	#$10
		STA	APU_PL1_VOL
		STA	APU_PL2_VOL
		LDA	#0
		STA	$F7
		LDA	$F4
		AND	#$F
		STA	$F4
		RTS
; ---------------------------------------------------------------------------

loc_F41A:				; F455j
		JSR	sub_F2BF
		LDA	#$80 ; '€'
		STA	$F4
		LDA	#2
		STA	$F0

loc_F425:				; F3FAj F405j	...
		LDX	#<tbl_F3DF
		LDY	#>tbl_F3DF
		JSR	sub_F195
		LDA	$1B
		AND	#$F
		STA	APU_PL1_LO
		LDX	#<tbl_F3DF
		LDY	#>tbl_F3DF
		JSR	sub_F1A1
		LDA	$1B
		LSR	A
		LSR	A
		AND	#$F
		STA	APU_PL2_LO
		RTS
; ---------------------------------------------------------------------------

loc_F444:				; F475j
		JMP	loc_F3E3
; END OF FUNCTION CHUNK	FOR sub_F447

; =============== S U B	R O U T	I N E =======================================


sub_F447:				; F27Ep

; FUNCTION CHUNK AT F3E3 SIZE 00000064 BYTES

		LDA	$F6
		BEQ	loc_F452
		AND	#$F
		CMP	#$F
		BEQ	loc_F452
		RTS
; ---------------------------------------------------------------------------

loc_F452:				; F449j F44Fj
		LDA	$F0
		ASL	A
		BCS	loc_F41A
		ASL	A
		BCS	loc_F489
		ASL	A
		BCS	loc_F4A4
		LDA	$F4
		ASL	A
		BCS	loc_F425
		LDA	$F6
		CMP	#$DF ; 'ß'
		BEQ	loc_F482
		LDA	$F6
		BNE	locret_F485
		LDA	$F4
		CMP	#$E0 ; 'à'
		BEQ	locret_F485
		LDA	$F3
		ASL	A
		BCS	loc_F444
		ASL	A
		BCS	loc_F4BC
		LDA	$F7
		ASL	A
		BCS	loc_F486
		ASL	A
		BCS	loc_F4D6

loc_F482:				; F466j
		JSR	sub_F246

locret_F485:				; F46Aj F470j
		RTS
; ---------------------------------------------------------------------------

loc_F486:				; F47Dj
		JMP	loc_F3FC
; ---------------------------------------------------------------------------

loc_F489:				; F458j
		JSR	sub_F2BF
		LDA	#$F
		STA	$7FA
		LDA	#$40 ; '@'
		STA	$F4
		LDX	#<tbl_F51C
		LDY	#>tbl_F51C
		JSR	sub_F1A1
		LDX	#<tbl_F518
		LDY	#>tbl_F518

loc_F4A0:				; F4BAj F4D4j
		JSR	sub_F195
		RTS
; ---------------------------------------------------------------------------

loc_F4A4:				; F45Bj
		JSR	sub_F2BF
		LDA	#$20 ; ' '
		STA	$F4
		LDA	#2
		STA	$F0
		LDX	#<tbl_F514
		LDY	#>tbl_F514
		JSR	sub_F1A1
		LDX	#<tbl_F510
		LDY	#>tbl_F510
		BNE	loc_F4A0

loc_F4BC:				; F478j
		LDA	#0
		STA	$7FC
		LDA	$F7
		AND	#$F
		ORA	#$40 ; '@'
		STA	$F7
		LDX	#<tbl_F520
		LDY	#>tbl_F520
		JSR	sub_F1A1
		LDX	#<tbl_F524
		LDY	#>tbl_F524
		BNE	loc_F4A0

loc_F4D6:				; F480j
		INC	$7FC
		LDA	$7FC
		CMP	#$12
		BEQ	loc_F50D
		CMP	#6
		BCC	loc_F4F4
		LDA	$1B
		ORA	#$10
		AND	#$7F ; ''
		STA	$7FE
		ROL	A
		STA	$7FD
		JMP	loc_F500
; ---------------------------------------------------------------------------

loc_F4F4:				; F4E2j
		INC	$7FD
		INC	$7FD
		INC	$7FE
		INC	$7FE

loc_F500:				; F4F1j
		LDA	$7FD
		STA	APU_PL2_LO
		LDA	$7FE
		STA	APU_PL1_LO
		RTS
; ---------------------------------------------------------------------------

loc_F50D:				; F4DEj
		JMP	loc_F407
; End of function sub_F447

; ---------------------------------------------------------------------------
tbl_F510:	.BYTE $B7 ; ·		; F4B6t F4B8t
		.BYTE $D5 ; Õ
		.BYTE $20
		.BYTE	0
tbl_F514:	.BYTE $B7 ; ·		; F4AFt F4B1t
		.BYTE $D5 ; Õ
		.BYTE $22 ; "
		.BYTE	0
tbl_F518:	.BYTE $97 ; —		; F49Ct F49Et
		.BYTE $93 ; “
		.BYTE $80 ; €
		.BYTE $22 ; "
tbl_F51C:	.BYTE $97 ; —		; F495t F497t
		.BYTE $93 ; “
		.BYTE $82 ; ‚
		.BYTE $22 ; "
tbl_F520:	.BYTE $3F ; ?		; F4C9t F4CBt
		.BYTE $BA ; º
		.BYTE $E0 ; à
		.BYTE	6
tbl_F524:	.BYTE $3F ; ?		; F4D0t F4D2t
		.BYTE $BB ; »
		.BYTE $CE ; Î
		.BYTE	6
tbl_F528:	.BYTE $BF ; ¿		; F540t F542t	...
		.BYTE $7F ; 
		.BYTE $50 ; P
		.BYTE	2
tbl_unk_F52C:	.BYTE $83 ; ƒ		; F56Er
		.BYTE $9C ; œ
		.BYTE $B5 ; µ
		.BYTE	0
tbl_unk_F530:	.BYTE	3		; F568r
		.BYTE	5
		.BYTE	3
tbl_F533:	.BYTE $80 ; €		; F58At
		.BYTE $7F ; 
		.BYTE $60 ; `
		.BYTE $68 ; h
tbl_F537:	.BYTE $80 ; €		; F591t
		.BYTE $7F ; 
		.BYTE $62 ; b
		.BYTE $68 ; h
; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_F598

loc_F53B:				; F5B4j loc_F5D0j
		LDA	#$FB ; 'û'
		STA	$7E7
		LDX	#<tbl_F528
		LDY	#>tbl_F528
		JSR	sub_F1A1
		LDA	$F5
		AND	#$E0 ; 'à'
		ORA	#2
		STA	$F5
		LDA	#0
		STA	$7F8
		STA	$7EA
		STA	$7F9
		INC	$7F9

loc_F55D:				; F5C8j
		DEC	$7F9
		BNE	locret_F586
		LDY	$7F8
		INC	$7F8
		LDA	tbl_unk_F530,Y
		STA	$7F9
		LDA	tbl_unk_F52C,Y
		STA	APU_PL2_SWEEP
		BNE	locret_F586
		LDA	#$7F ; ''
		STA	APU_PL2_SWEEP
		LDA	#$10
		STA	APU_PL2_VOL
		LDA	$F5
		AND	#$E0 ; 'à'
		STA	$F5

locret_F586:				; F560j F574j
		RTS
; ---------------------------------------------------------------------------

loc_F587:				; F5B1j
		JSR	sub_F2BF
		LDX	#<tbl_F533
		LDY	#>tbl_F528
		JSR	sub_F195
		LDX	#<tbl_F537
		LDY	#>tbl_F528
		JMP	loc_F5FC
; END OF FUNCTION CHUNK	FOR sub_F598

; =============== S U B	R O U T	I N E =======================================


sub_F598:				; F281p

; FUNCTION CHUNK AT F53B SIZE 0000005D BYTES

		LDA	$F6
		BEQ	loc_F5A2
		AND	#$F
		CMP	#$F
		BNE	locret_F5CF

loc_F5A2:				; F59Aj
		LDA	$F4
		AND	#$E0 ; 'à'
		BNE	locret_F5CF
		LDA	$F7
		AND	#$C0 ; 'À'
		BNE	locret_F5CF
		LDA	$F1
		LSR	A
		BCS	loc_F587
		LSR	A
		BCS	loc_F53B
		LSR	A
		BCS	loc_F616
		LSR	A
		BCS	locret_F5FB
		LSR	A
		BCS	loc_F5D3
		LDA	$7EA
		BNE	loc_F5D0
		LDA	$F5
		LSR	A
		LSR	A
		BCS	loc_F55D
		LSR	A
		LSR	A
		LSR	A
		BCS	loc_F600

locret_F5CF:				; F5A0j F5A6j	...
		RTS
; ---------------------------------------------------------------------------

loc_F5D0:				; F5C2j
		JMP	loc_F53B
; ---------------------------------------------------------------------------

loc_F5D3:				; F5BDj
		LDA	$F6
		BNE	locret_F5CF
		LDA	$F5
		AND	#2
		BNE	locret_F5CF
		LDA	$F5
		AND	#$E0 ; 'à'
		ORA	#$10
		STA	$F5
		LDA	#0
		STA	$7F2
		LDX	#<tbl_F621
		LDY	#>tbl_F621

loc_F5EE:				; F614j
		JSR	sub_F1A1
		LDA	$1B
		AND	#$3F ; '?'
		ORA	#$10
		STA	$4006
		RTS
; ---------------------------------------------------------------------------

locret_F5FB:				; F5BAj
		RTS
; ---------------------------------------------------------------------------

loc_F5FC:				; F595j
		JSR	sub_F1A1

locret_F5FF:				; F608j
		RTS
; ---------------------------------------------------------------------------

loc_F600:				; F5CDj
		INC	$7F2
		LDA	$7F2
		CMP	#5
		BNE	locret_F5FF
		LDA	$F5
		AND	#$EF ; 'ï'
		STA	$F5
		LDX	#<tbl_F625
		LDY	#>tbl_F625
		BNE	loc_F5EE

loc_F616:				; F5B7j
		LDA	$F6
		CMP	#$DF ; 'ß'
		LDY	#$A
		LDA	#$EF ; 'ï'
		JMP	loc_F749
; End of function sub_F598

; ---------------------------------------------------------------------------
tbl_F621:	.BYTE $D7 ; ×		; F5EAt F5ECt
		.BYTE $86 ; †
		.BYTE $A8 ; ¨
		.BYTE $28 ; (
tbl_F625:	.BYTE $D7 ; ×		; F610t F612t
		.BYTE $86 ; †
		.BYTE $BD ; ½
		.BYTE $48 ; H
tbl_F629:	.BYTE	6
		.BYTE $7F ; 
		.BYTE  $E
		.BYTE $68 ; h
tbl_F62D:	.BYTE	8		; F6A1t F6A3t
		.BYTE $7F ; 
		.BYTE $40 ; @
		.BYTE $28 ; (
tbl_F631:	.BYTE	8		; F649t F64Bt
		.BYTE $7F ; 
		.BYTE $45 ; E
		.BYTE $28 ; (
		.BYTE $FF
		.BYTE $7F ; 
		.BYTE	3
		.BYTE	0
; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_F64F

loc_F639:				; F66Bj
		INC	$7F6
		LDA	$7F6
		CMP	#4
		BNE	locret_F677
		LDA	$F5
		AND	#$1F
		STA	$F5
		LDX	#<tbl_F631
		LDY	#>tbl_F631
		BNE	loc_F6A5
; END OF FUNCTION CHUNK	FOR sub_F64F

; =============== S U B	R O U T	I N E =======================================


sub_F64F:				; F284p

; FUNCTION CHUNK AT F639 SIZE 00000016 BYTES

		LDA	$F6
		BEQ	loc_F659
		AND	#$F
		CMP	#$F
		BNE	locret_F677

loc_F659:				; F651j
		LDA	$F4
		AND	#$E0 ; 'à'
		BNE	locret_F677
		LDA	$F1
		ASL	A
		BCS	loc_F678
		ASL	A
		BCS	loc_F684
		LDA	$F5
		ASL	A
		ASL	A
		BCS	loc_F639
		LDA	$F1
		AND	#$20 ; ' '
		BEQ	loc_F67B
		LDA	$F6
		BEQ	loc_F6A9

locret_F677:				; F641j F657j	...
		RTS
; ---------------------------------------------------------------------------

loc_F678:				; F662j
		JMP	loc_F6BC
; ---------------------------------------------------------------------------

loc_F67B:				; F671j
		LDA	$F6
		CMP	#$DF ; 'ß'
		BNE	locret_F677
		JMP	sub_F2BF
; ---------------------------------------------------------------------------

loc_F684:				; F665j
		LDA	$F5
		AND	#$1F
		ORA	#$40 ; '@'
		STA	$F5
		LDA	#0
		STA	APU_TRI_LINEAR
		STA	$F6
		STA	$7F6
		LDA	#$10
		STA	APU_PL2_VOL
		STA	APU_PL1_VOL
		STA	APU_NOISE_VOL
		LDX	#<tbl_F62D
		LDY	#>tbl_F62D

loc_F6A5:				; F64Dj
		JSR	sub_F199
		RTS
; ---------------------------------------------------------------------------

loc_F6A9:				; F675j
		LDA	$7E9
		AND	#$20 ; ' '
		BNE	loc_F6B5
		LDA	#2
		STA	$7F5

loc_F6B5:				; 
		LDY	#8
		LDA	#$DF ; 'ß'
		JMP	loc_F781
; ---------------------------------------------------------------------------

loc_F6BC:				; loc_F678j
		LDY	#4
		LDA	#$7F ; ''
		JMP	loc_F781
; End of function sub_F64F

; ---------------------------------------------------------------------------
; START	OF FUNCTION CHUNK FOR sub_F6ED

loc_F6C3:				; F711j
		LDY	#0
		LDA	#2
		JMP	loc_F792
; ---------------------------------------------------------------------------

loc_F6CA:				; F703j F709j
		LDA	#0
		STA	$7E8
		LDY	#$B
		LDA	#$F0 ; 'ð'
		JMP	loc_F778
; ---------------------------------------------------------------------------

loc_F6D6:				; F6F9j
		LDA	#3
		STA	$7E3
		LDA	$F2
		STA	$7E2

locret_F6E0:				; F6E4j
		RTS
; ---------------------------------------------------------------------------

loc_F6E1:				; F6FEj
		DEC	$7E3
		BNE	locret_F6E0
		LDA	$7E2
		STA	$F2
		BNE	loc_F700
; END OF FUNCTION CHUNK	FOR sub_F6ED

; =============== S U B	R O U T	I N E =======================================


sub_F6ED:				; F27Bp

; FUNCTION CHUNK AT F000 SIZE 000000FC BYTES
; FUNCTION CHUNK AT F6C3 SIZE 0000002A BYTES

		LDA	$69
		CMP	#$A
		BEQ	loc_F725
		LDA	$15
		BNE	loc_F700
		LDA	$F2
		BNE	loc_F6D6
		LDA	$7E3
		BNE	loc_F6E1

loc_F700:				; F6EBj F6F5j
		LDA	$7E8
		BNE	loc_F6CA
		LDA	$F3
		AND	#8
		BNE	loc_F6CA
		LDA	$F2
		LSR	A
		BCS	loc_F75E
		LSR	A
		BCS	loc_F6C3
		LSR	A
		BCS	loc_F72D
		LSR	A
		BCS	loc_F758
		LSR	A
		BCS	loc_F745
		LSR	A
		BCS	loc_F73F
		LSR	A
		BCS	loc_F739
		LSR	A
		BCS	loc_F733

loc_F725:				; F6F1j
		LDA	$F6
		BNE	loc_F72A
		RTS
; ---------------------------------------------------------------------------

loc_F72A:				; F727j F756j	...
		JMP	loc_F004
; ---------------------------------------------------------------------------

loc_F72D:				; F714j
		LDY	#2
		LDA	#4
		BNE	loc_F762

loc_F733:				; F723j
		LDY	#9
		LDA	#$80 ; '€'
		BNE	loc_F749

loc_F739:				; F720j
		LDY	#7
		LDA	#$40 ; '@'
		BNE	loc_F749

loc_F73F:				; F71Dj
		LDY	#6
		LDA	#$20 ; ' '
		BNE	loc_F792

loc_F745:				; F71Aj
		LDY	#5
		LDA	#$10

loc_F749:				; F61Ej F737j	...
		JSR	init_music
		LDX	#$FC ; 'ü'
		LDY	#$FC ; 'ü'
		JSR	sub_F212
		INC	$7F0
		BNE	loc_F72A

loc_F758:				; F717j 
		LDY	#$3 ; BGM_1ST_PHASE
		LDA	#8
		BNE	loc_F762

loc_F75E:				; F70Ej
		LDY	#1 ; BGM_GAME_OVER
		LDA	#1

loc_F762:				; F731j F75Cj
		JSR	init_music
		LDX	#$80 ; '€'
		LDY	#$80 ; '€'

loc_F769:				; F799j
		JSR	sub_F21A
		LDA	#$83 ; 'ƒ'
		STA	APU_PL1_SWEEP
		LDA	#$7F ; ''
		STA	APU_PL2_SWEEP
		BNE	loc_F78B

loc_F778:				; F6D3j
		JSR	init_music
		LDX	#4
		LDY	#4
		BNE	loc_F788

loc_F781:				; F6B9j F6C0j
		JSR	init_music
		LDX	#$80 ; '€'
		LDY	#$80 ; '€'

loc_F788:				; F77Fj
		JSR	sub_F212

loc_F78B:				; F776j
		LDA	#0
		STA	$7F0
		BEQ	loc_F72A

loc_F792:				; F6C7j F743j
		JSR	init_music
		LDX	#$80 ; '€'
		LDY	#$BA ; 'º'
		BNE	loc_F769
; End of function sub_F6ED

; ---------------------------------------------------------------------------

; BGM music

; 0 - Stage clear BGM
; 1 - Game over BGM
; 2 - Next Level start BGM (VS only)
; 3 - First Level start BGM
; 4 - Enemy fall BGM
; 5 - Perfect bonus BGM
; 6 - Balloon trip/Bonus game BGM
; 7 - Gulp (Piranha's food) BGM
; 8 - Parachutes Away BGM
; 9 - Try again BGM
; A - Bubble get BGM
; B - High score (VS only) BGM

	; Relative offsets from the beginning of the array
mus_offsets:    .BYTE $0C, $15, $1E, $27, $30, $39, $42, $4B, $54, $5D, $66, $6F
	
	; for each BGM's header:
bgm00:          .BYTE $0C               ;
                                        ; Header format:
                                        ; B: unknown
                                        ; W: Ch1 (pulse 1) data address
                                        ; W: Ch2 (pulse 2) data address
                                        ; W: Ch3 (triangle) data address
                                        ; W: Ch4 (noise) data address
                .WORD bgm00_ch01        ; 9 bytes length
                .WORD bgm00_ch02        ; 19 bytes length
                .WORD bgm00_ch03        ; 19 bytes length
                .WORD bgm00_ch04        ; 7 bytes length
bgm_01:         .BYTE $15
                .WORD bgm01_ch01        ; 25 bytes length
                .WORD bgm01_ch02        ; 89 bytes length
                .WORD bgm01_ch03        ; 63 bytes length
                .WORD bgm01_ch04        ; 15 bytes length
bgm_02:         .BYTE $15
                .WORD bgm02_ch01        ; 12 bytes length
                .WORD bgm02_ch02        ; 25 bytes length
                .WORD bgm02_ch03        ; 18 bytes length
                .WORD bgm02_ch04        ; 26 bytes length
bgm_03:         .BYTE $15
                .WORD bgm03_ch01        ; 34 bytes length
                .WORD bgm03_ch02        ; 31 bytes length
                .WORD bgm03_ch03        ; 27 bytes length
                .WORD bgm03_ch04        ; 31 bytes length
bgm_04:         .BYTE $00
                .WORD bgm04_ch01        ; 22 bytes length
                .WORD chan_NULL
                .WORD bgm04_ch03        ; 21 bytes length
                .WORD chan_NULL
bgm_05:         .BYTE $00
                .WORD bgm05_ch01        ; 22 bytes length
                .WORD bgm05_ch02        ; 17 bytes length
                .WORD bgm05_ch03
                .WORD bgm05_ch04
bgm_06:         .BYTE $15
                .WORD bgm06_ch01        ; 147 bytes length
                .WORD bgm06_ch02        ; 138 bytes length
                .WORD bgm06_ch03        ; 68 bytes length
                .WORD bgm06_ch04        ; 48 bytes length
bgm_07:         .BYTE $15
                .WORD bgm07_ch01
                .WORD bgm07_ch02        ; 18 bytes length
                .WORD bgm07_ch03        ; 18 bytes length
                .WORD chan_NULL
bgm_08:         .BYTE $15
                .WORD chan_NULL
                .WORD bgm08_ch02        ; 15 bytes length
                .WORD bgm08_ch03        ; 17 bytes length
                .WORD chan_NULL
bgm_09:         .BYTE $0C
                .WORD bgm09_ch01
                .WORD bgm09_ch02        ; 26 bytes length
                .WORD bgm09_ch03        ; 25 bytes length
                .WORD chan_NULL
bgm_10:         .BYTE $00
                .WORD bgm10_ch01        ; 18 ($12) bytes total
                .WORD bgm10_ch02
                .WORD bgm10_ch03
                .WORD chan_NULL
bgm_11:         .BYTE $15
                .WORD bgm11_ch01
                .WORD bgm11_ch02
                .WORD bgm11_ch03
                .WORD bgm11_ch04

; Channel data below
; unknown (so far) format
; looks like note numbers + delays

bgm00_ch01:     .BYTE $82, $1C, $02, $1C, $02, $02, $1C, $1C, $00
                                        ; 9 bytes length
bgm00_ch02:     .BYTE $81, $10, $0A, $32, $28, $80, $32, $02, $32, $02, $82, $32, $81, $06, $02, $06
                .BYTE $02, $82, $32     ; 19 bytes length
bgm00_ch03:     .BYTE $81, $66, $1A, $10, $0A, $80, $10, $02, $10, $02, $82, $10, $81, $16, $02, $16
                .BYTE $02, $82, $0A     ; 19 bytes length

bgm00_ch04:     .BYTE $83, $03, $0C, $82, $03, $0C, $0C

bgm10_ch01:     .BYTE $82, 2, $80, 8, 2, $10, 2, $16, 2, $64, 2, 2, 2, $1A, $8B, 2
                .BYTE 2, 0              ; 18 bytes length

bgm10_ch02:     .BYTE $82, $02, $8B, $02, $80, $2E, $02, $08, $02, $10, $02, $16, $02, $02, $02, $12
                .BYTE $8B, $02          ; 18 bytes length

bgm10_ch03:     .BYTE $82, $02, $8B, $02, $02, $80, $10, $02, $16, $02, $64, $02, $6C, $02, $02, $02
                .BYTE $68               ; 17 bytes length

bgm09_ch01:     .BYTE $80, $12, $02, $0C, $02, $04, $02, $0C, $02, $04, $02, $2A, $02, $81, $04, $02
                .BYTE $80, $04, $02, $81, $04, $88, $02, $02, $00 ; 25 bytes length

bgm09_ch02:     .BYTE $88, $02, $02, $80, $04, $02, $2A, $02, $24, $02, $2A, $02, $24, $02, $1C, $02
                .BYTE $81, $22, $02, $80, $22, $02, $81, $24, $88, $02 ; 26 bytes length

bgm09_ch03:     .BYTE $88, $02, $80, $68, $02, $60, $02, $12, $02, $60, $02, $12, $02, $0C, $02, $81
                .BYTE $10, $02, $80, $10, $02, $81, $12, $88, $02 ; 25 bytes length

bgm06_ch01:     .BYTE $81, $02, $02, $1C, $02, $02, $02, $1C, $1C, $02, $02, $1C, $02, $02, $1C, $1C
                .BYTE $02, $02, $02, $02, $1C, $02, $1C, $1C, $02, $88, $1C, $1C, $1C, $1C, $1C, $1C ; 147 bytes length
                .BYTE $C7, $81, $5A, $02, $5A, $02, $2A, $02, $5A, $02, $FF, $88, $1C, $1C, $1C, $1C
                .BYTE $1C, $1C, $81, $46, $02, $46, $02, $02, $80, $46, $46, $81, $46, $02, $C2, $81
                .BYTE $46, $46, $80, $02, $46, $81, $02, $FF, $80, $02, $46, $46, $46, $81, $46, $02
                .BYTE $80, $02, $46, $46, $46, $81, $46, $02, $C2, $80, $02, $46, $81, $02, $46, $46
                .BYTE $FF, $C2, $81, $46, $46, $80, $32, $32, $81, $02, $FF, $C5, $80, $32, $32, $46
                .BYTE $FF, $02, $C4, $8D, $04, $04, $46, $FF, $C4, $2A, $2A, $46, $FF, $C4, $24, $24
                .BYTE $46, $FF, $C4, $1C, $1C, $46, $FF, $C4, $81, $5A, $02, $5A, $02, $2A, $02, $5A
                .BYTE $02, $FF, $00

bgm06_ch02:     .BYTE $81, $32, $02, $02, $06, $0C, $32, $02, $02, $8A, $2E, $8B, $02, $8A, $2E, $8B
                .BYTE $02, $8A, $2E, $8B, $02, $88, $2E, $32, $2E, $D0, $8C, $2C, $24, $FF, $D0, $2E ; 138 bytes length
                .BYTE $20, $FF, $C3, $80, $28, $02, $82, $02, $80, $2C, $02, $32, $02, $24, $02, $82
                .BYTE $02, $81, $02, $80, $28, $02, $06, $02, $28, $02, $81, $02, $80, $24, $02, $32
                .BYTE $02, $24, $02, $FF, $80, $28, $02, $82, $02, $80, $2C, $02, $32, $02, $24, $02
                .BYTE $82, $02, $89, $0C, $0A, $08, $06, $32, $30, $2E, $2C, $2A, $28, $26, $24, $02
                .BYTE $02, $02, $86, $02, $C7, $84, $02, $FF, $C4, $80, $28, $02, $82, $02, $80, $2C
                .BYTE $02, $32, $02, $24, $02, $82, $02, $81, $02, $80, $28, $02, $06, $02, $28, $02
                .BYTE $81, $02, $80, $24, $02, $32, $02, $24, $02, $FF

bgm06_ch03:     .BYTE $81, $14, $02, $02, $14, $1A, $14, $02, $02, $88, $10, $10, $10, $10, $14, $10
                .BYTE $85, $3C, $81, $44, $85, $4A, $81, $44, $88, $28, $24, $20, $46, $42, $40, $D2 ; 68 bytes length
                .BYTE $81, $3C, $02, $02, $44, $02, $02, $02, $4A, $02, $46, $36, $36, $38, $38, $02
                .BYTE $3A, $02, $80, $3C, $3C, $81, $02, $24, $02, $02, $2C, $24, $88, $24, $1E, $46
                .BYTE $36, $38, $3A, $FF

bgm06_ch04:     .BYTE $D8, $81, $06, $FF, $C6, $88, $06, $FF, $C7, $81, $06, $06, $80, $06, $06, $81
                .BYTE $06, $06, $80, $06, $06, $81, $06, $06, $FF, $C6, $88, $06, $FF, $E0, $81, $06 ; 48 bytes length
                .BYTE $06, $FF, $82, $0F, $81, $06, $06, $C8, $06, $06, $80, $06, $06, $81, $06, $FF

bgm02_ch02:     .BYTE $C8, $82, $02, $80, $06, $06, $06, $06, $82, $06, $89, $0C, $02, $06, $82, $02
                .BYTE $02, $89, $08, $12, $08, $12, $02, $08, $FF ; 25 bytes length

bgm02_ch01:     .BYTE $C2, $82, $02, $02, $1C, $02, $FF, $02, $02, $1C, $02, $00 ; 12 bytes length

bgm02_ch03:     .BYTE $C8, $82, $02, $02, $88, $46, $3C, $46, $82, $02, $89, $2A, $20, $4A, $82, $42
                .BYTE $02, $FF          ; 18 bytes length

bgm02_ch04:     .BYTE $C3, $82, $03, $89, $06, $03, $06, $82, $03, $89, $06, $03, $06, $03, $06, $03
                .BYTE $06, $03, $06, $06, $03, $03, $06, $03, $09, $FF ; 26 bytes length

bgm01_ch01:     .BYTE $84, $02, $02, $82, $1C, $1C, $C3, $82, $1C, $1C, $81, $1C, $1C, $1C, $02, $FF
                .BYTE $88, $1C, $1C, $1C, $1C, $1C, $1C, $1C, $00 ; 25 bytes length

bgm01_ch02:     .BYTE $80, $02, $02, $0E, $0C, $08, $02, $04, $02, $30, $04, $30, $2E, $2A, $02, $26
                .BYTE $02, $24, $26, $2A, $2E, $2A, $2E, $30, $32, $C8, $8C, $04, $24, $FF, $83, $02 ; 89 bytes length
                .BYTE $80, $0E, $02, $0E, $02, $0C, $02, $0E, $02, $60, $02, $02, $02, $0E, $02, $0C
                .BYTE $02, $02, $02, $0E, $02, $0C, $02, $0E, $02, $60, $02, $02, $02, $0E, $02, $0C
                .BYTE $02, $0E, $02, $0E, $02, $0C, $02, $0E, $02, $60, $02, $02, $02, $0E, $02, $0C
                .BYTE $02, $88, $60, $18, $16, $12, $0E, $0C, $0E

bgm01_ch03:     .BYTE $80, $02, $02, $16, $12, $0E, $02, $0C, $02, $08, $0C, $08, $04, $30, $02, $2E
                .BYTE $02, $2A, $2E, $30, $04, $30, $04, $08, $0A, $C8, $8C, $0C, $2A, $FF, $83, $02 ; 63 bytes length
                .BYTE $81, $3E, $3E, $82, $46, $1C, $46, $81, $02, $38, $3E, $02, $82, $46, $1C, $82
                .BYTE $48, $48, $81, $3E, $3E, $82, $38, $88, $24, $20, $1C, $48, $46, $42, $3E

bgm01_ch04:     .BYTE $84, $03, $03, $82, $09, $09, $C6, $82, $03, $0C, $FF, $C6, $88, $06, $FF
                                        ; 15 bytes length
bgm08_ch02:     .BYTE $ED, $89, $2A, $02, $04, $0C, $02, $04, $08, $02, $30, $26, $02, $30, $FF
                                        ; 15 bytes length
bgm08_ch03:     .BYTE $80, $02, $ED, $89, $0C, $02, $12, $60, $02, $12, $18, $02, $0E, $08, $02, $0E
                .BYTE $FF               ; 17 bytes length

bgm07_ch01:     .BYTE $80, $42, $02, $48, $02, $1E, $02, $24, $02, $02, $02, $2A, $02, $C6, $8C, $30
                .BYTE $2A, $FF, $00     ; 19 bytes length

bgm07_ch03:     .BYTE $80, $24, $02, $2A, $02, $30, $02, $06, $02, $02, $02, $0C, $02, $C6, $8C, $12
                .BYTE $18, $FF          ; 18 bytes length

bgm07_ch02:     .BYTE $80, $36, $02, $3C, $02, $42, $02, $48, $02, $02, $02, $1E, $02, $C6, $8C, $24
                .BYTE $1E, $FF          ; 18 bytes length

bgm04_ch01:     .BYTE $80, $68, $66, $64, $62, $81, $02, $80, $70, $6C, $66, $62, $18, $14, $10, $0A
                .BYTE $06, $30, $2C, $28, $02, $00 ; 22 bytes length

bgm04_ch03:     .BYTE $80, $1A, $18, $16, $14, $81, $02, $80, $02, $70, $6C, $66, $62, $18, $14, $10
                .BYTE $0A, $06, $30, $2C, $28 ; 21 bytes length


bgm03_ch01:     .BYTE $C2, $88, $1C, $1C, $1C, $1C, $1C, $1C, $83, $1C, $80, $04, $04, $2A, $02, $82
                .BYTE $1C, $FF, $81, $5A, $02, $5A, $02, $2A, $02, $5A, $1C, $81, $5A, $02, $5A, $02 ; 34 bytes length
                .BYTE $5A, $00

bgm03_ch02:     .BYTE $88, $2E, $2E, $2E, $30, $04, $30, $C4, $80, $2E, $04, $FF, $83, $02, $88, $2E
                .BYTE $2E, $2E, $30, $04, $30, $C4, $80, $2E, $04, $FF, $83, $02, $84, $02, $02 ; 31 bytes length

bgm03_ch03:     .BYTE $C2, $88, $3E, $3E, $3E, $42, $46, $42, $84, $3E, $FF, $85, $3E, $81, $3E, $88
                .BYTE $1C, $46, $1C, $81, $02, $3E, $3E, $3E, $82, $34, $02 ; 27 bytes length


bgm03_ch04:     .BYTE $C2, $88, $06, $06, $06, $06, $06, $06, $82, $06, $06, $06, $06, $FF, $C2, $81
                .BYTE $06, $06, $80, $06, $06, $81, $06, $06, $06, $06, $80, $06, $06, $FF, $09

bgm05_ch01:     .BYTE $80, $10, $02, $10, $02, $10, $02, $0C, $0C, $0C, $02, $0C, $02, $14, $14, $14
                .BYTE $02, $14, $02, $85, $10, $00 ; 22 bytes length

bgm05_ch02:     .BYTE $80, $32, $02, $32, $02, $32, $02, $C2, $32, $32, $32, $02, $32, $02, $FF, $85
                .BYTE $32               ; 17 bytes length

bgm05_ch03:     .BYTE $80, $66, $02, $66, $02, $66, $02, $62, $62, $62, $02, $62, $02, $68, $68, $68
                .BYTE $02, $68, $02, $85, $66

bgm05_ch04:     .BYTE $85, $0C, $0C, $0C, $0C

bgm11_ch01:     .BYTE $89, $24, $02, $82, $2A, $89, $20, $02, $02, $02, $2E, $2A, $24, $20, $02, $1C
                .BYTE $02, $02, $46, $02, $02, $82, $46, $89, $02, $24, $02, $2A, $02, $02, $20, $82
                .BYTE $20, $89, $2E, $02, $04, $04, $02, $2E, $02, $02, $26, $02, $02, $82, $20, $89
                .BYTE $1C, $02, $02, $42, $44, $46, $1C, $20, $02, $1C, $02, $02, $20, $24, $02, $82
                .BYTE $02, $83, $2A, $89, $02, $24, $02, $20, $1C, $02, $46, $1C, $02, $02, $1C, $02
                .BYTE $1C, $82, $02, $1C, $1C, $1C, $89, $1C, $02, $82, $02, $83, $42, $89, $02, $00

bgm11_ch02:     .BYTE $89, $02, $02, $20, $02, $02, $46, $02, $02, $82, $48, $89, $02, $82, $46, $89
                .BYTE $02, $02, $3E, $02, $02, $82, $3E, $89, $40, $02, $02, $20, $02, $02, $2E, $82
                .BYTE $48, $02, $82, $26, $89, $02, $02, $1C, $02, $02, $82, $46, $89, $46, $83, $02
                .BYTE $89, $40, $02, $40, $02, $02, $02, $42, $02, $82, $02, $83, $1E, $89, $02, $40
                .BYTE $02, $02, $40, $02, $02, $3E, $02, $02, $3E, $02, $3C, $02, $02, $02, $82, $42
                .BYTE $40, $3E, $89, $3C, $02, $82, $02, $83, $5E, $89, $02

bgm11_ch03:     .BYTE $82, $34, $89, $34, $02, $3A, $82, $3C, $3C, $3E, $3E, $42, $89, $42, $02, $1E
                .BYTE $1C, $02, $82, $34, $89, $3C, $82, $46, $3A, $89, $38, $02, $38, $82, $3E, $46
                .BYTE $1C, $3E, $3E, $40, $40, $89, $42, $40, $3E, $3C, $02, $82, $46, $89, $46, $3C
                .BYTE $02, $3A, $82, $38, $38, $42, $42, $34, $3C, $3A, $38, $34, $89, $02, $02, $83
                .BYTE $42, $89, $02
bgm11_ch04:     .BYTE $E8, $89, $06, $03, $06, $09, $03, $06, $06, $06, $03, $09, $03, $06, $FF, $FF
                .BYTE $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
                .BYTE $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
                .BYTE $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
                .BYTE $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF
                .BYTE $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF

tbl_dmp00:   
		.BYTE <tbl_JOYDUMP00
		.BYTE <tbl_JOYDUMP02
tbl_dmp01:   
		.BYTE <tbl_JOYDUMP01
		.BYTE <tbl_JOYDUMP03

tbl_dmp02:   
		.BYTE >tbl_JOYDUMP00
		.BYTE >tbl_JOYDUMP02

tbl_dmp03:   
		.BYTE >tbl_JOYDUMP01
		.BYTE >tbl_JOYDUMP03

tbl_JOYDUMP00:  
		.BYTE $00, $80, $00, $80, $00, $80, $00, $80, $00, $80, $00, $01, $81, $01, $81, $01
        .BYTE $81, $01, $81, $01, $81, $01, $81, $01, $00, $02, $00, $80, $00, $80, $00, $80 ; Joypad dumps
        .BYTE $00, $01, $81, $01, $81, $01, $81, $80, $00, $82, $02, $82, $02, $82, $80, $00
        .BYTE $02, $82, $02, $00, $80, $00, $80, $00, $80, $00, $80, $00, $80, $00, $80, $00
        .BYTE $02, $82, $02, $82, $80, $00, $02, $82, $02, $00, $02, $82, $02, $00, $80, $00
        .BYTE $80, $81, $01, $81, $01, $00, $80, $02, $82, $02, $82, $02, $82, $02, $00, $80
        .BYTE $00, $80, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

tbl_JOYDUMP01:  
		.BYTE $00, $02, $05, $05, $05, $05, $05, $05, $07, $05, $06, $02, $06, $17, $06, $07
        .BYTE $06, $3A, $05, $06, $05, $05, $06, $0B, $27, $2A, $21, $08, $06, $05, $06, $05
        .BYTE $04, $02, $07, $05, $07, $09, $05, $02, $07, $06, $06, $07, $07, $01, $06, $0C
        .BYTE $0C, $07, $02, $14, $06, $07, $06, $09, $05, $08, $05, $09, $07, $1E, $07, $15
        .BYTE $01, $07, $05, $02, $06, $08, $3C, $0A, $08, $26, $15, $07, $03, $06, $06, $06
        .BYTE $05, $01, $06, $06, $0D, $01, $07, $06, $05, $08, $07, $09, $05, $02, $0D, $07
        .BYTE $16, $0D, $FF, $AC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

tbl_JOYDUMP02:  
		.BYTE $00, $80, $00, $80, $00, $80, $00, $80, $00, $80, $00, $80, $00, $80, $00, $80
        .BYTE $00, $80, $00, $80, $00, $80, $00, $80, $00, $80, $00, $80, $81, $01, $81, $01
        .BYTE $81, $01, $00, $80, $00, $02, $82, $02, $82, $02, $82, $02, $82, $02, $82, $02
        .BYTE $82, $02, $00, $81, $01, $81, $01, $00, $80, $00, $80, $00, $80, $81, $01, $81
        .BYTE $01, $00, $80, $00, $80, $00, $80, $00, $80, $00, $80, $00, $80, $00, $80, $00
        .BYTE $80, $00, $80, $00, $80, $00, $80, $00, $80, $82, $02, $82, $02, $00, $02, $00
        .BYTE $80, $00, $80, $00, $80, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00

tbl_JOYDUMP03:  
		.BYTE $03, $05, $07, $06, $07, $05, $07, $05, $07, $05, $07, $05, $06, $05, $1E, $06
        .BYTE $22, $06, $08, $05, $13, $06, $0F, $06, $13, $08, $07, $01, $05, $07, $06, $06
        .BYTE $05, $01, $07, $06, $05, $02, $06, $07, $05, $06, $06, $06, $06, $07, $07, $07
        .BYTE $06, $02, $15, $07, $06, $07, $01, $04, $07, $05, $08, $06, $06, $01, $06, $06
        .BYTE $02, $05, $06, $06, $08, $07, $06, $09, $06, $1E, $06, $12, $05, $09, $05, $0A
        .BYTE $02, $07, $05, $07, $07, $21, $05, $26, $02, $04, $07, $08, $01, $4A, $06, $01
        .BYTE $08, $14, $08, $08, $03, $2A, $0E, $FF, $BA, $00, $00, $00, $00, $00, $00, $00

dip_item0_subitem0:
		; THREE
		.BYTE 8 
		.BYTE $21, $37
		.BYTE 5
		.byte $1d,$11,$1b,$0e,$0e
dip_item0_subitem1:
		; FOUR
		.BYTE 8 
		.BYTE $21, $37
		.BYTE 5
		.byte $24,$0f,$18,$1e,$1b
dip_item0_subitem2:
		; FIVE
		.BYTE 8 
		.BYTE $21, $37
		.BYTE 5
		.byte $24,$0f,$12,$1f,$0e
dip_item0_subitem3:
		; SIX
		.BYTE 8 
		.BYTE $21, $37
		.BYTE 5
		.byte $24,$24,$1c,$12,$21
dip_item1_subitem0:
		; EASY
		.BYTE 9 
		.BYTE $21, $76
		.BYTE 6
		.byte $24,$24,$0e,$0a,$1c,$22
dip_item1_subitem1:
		; NORMAL
		.BYTE 9 
		.BYTE $21, $76
		.BYTE 6
		.byte $17,$18,$1b,$16,$0a,$15
dip_item1_subitem2:
		; MEDIUM
		.BYTE 9 
		.BYTE $21, $76
		.BYTE 6
		.byte $16,$0e,$0d,$12,$1e,$16
dip_item1_subitem3:
		; HARD
		.BYTE 9 
		.BYTE $21, $76
		.BYTE 6
		.byte $24,$24,$11,$0a,$1b,$0d

dip_item2_subitem0:
		; LO
		.BYTE 7 
		.BYTE $21, $B8
		.BYTE 4
		.byte $24,$15,$18,$20
dip_item2_subitem1:
		; HIGH
		.BYTE 7 
		.BYTE $21, $B8
		.BYTE 4
		.byte $11,$12,$10,$11

dip_item3_subitem0:
		; 10000 PTS
		.BYTE 12
		.BYTE $21, $F3
		.BYTE 9
		.byte $01,$00,$00,$00,$00,$24,$19,$1d,$1c
dip_item3_subitem1:
		; 20000 PTS
		.BYTE 12
		.BYTE $21, $F3
		.BYTE 9
		.byte $02,$00,$00,$00,$00,$24,$19,$1d,$1c
dip_item3_subitem2:
		; 40000 PTS
		.BYTE 12
		.BYTE $21, $F3
		.BYTE 9
		.byte $04,$00,$00,$00,$00,$24,$19,$1d,$1c
dip_item3_subitem3:
		; NONE
		.BYTE 12
		.BYTE $21, $F3
		.BYTE 9
		.byte $24,$24,$24,$24,$24,$17,$18,$17,$0e
dip_item4_subitem0:
		; ARCADE
		.BYTE 10 
		.BYTE $22, $35
		.BYTE 7
		.byte $24,$0a,$1b,$0c,$0a,$0d,$0e
dip_item4_subitem1:
		; CONSOLE
		.BYTE 10 
		.BYTE $22, $35
		.BYTE 7
		.byte $0c,$18,$17,$1c,$18,$15,$0e

dip_switches:		
		.word dip_dip0, dip_dip1, dip_dip2, dip_dip3, dip_dip4
dip_dip0:
		.WORD dip_item0_subitem0, dip_item0_subitem1, dip_item0_subitem2, dip_item0_subitem3
dip_dip1:
		.WORD dip_item1_subitem0, dip_item1_subitem1, dip_item1_subitem2, dip_item1_subitem3
dip_dip2:
		.WORD dip_item2_subitem0, dip_item2_subitem1
dip_dip3:
		.WORD dip_item3_subitem0, dip_item3_subitem1, dip_item3_subitem2, dip_item3_subitem3
dip_dip4:
		.WORD dip_item4_subitem0, dip_item4_subitem1

dip_oneswitch_on:
		.byte $ce, $de, $ee, $fe
		.byte $cf, $df, $ef, $ff

dip_oneswitch_off:
		.byte $cc, $dc, $ec, $fc
		.byte $cd, $dd, $ed, $fd

dip_addresses:
		.word $2284, $2285, $2287, $2288, $228A, $228B, $228D, $228E
		.word $2291, $2292, $2294, $2295, $2297, $2298, $229A, $229B

dip_max_values:
		.byte 3, 3, 1, 3, 1


_dip_values_array_update:
		lda #1
		sta bubble_flag
		jsr loc_F326 ; pop balloon
dip_values_array_update:
		lda #0
		ldx #8
:		
		sta dipmenu_states_1b-1,x
		dex
		bne :-

		ldx #1
		lda dip_LIVES

		lsr a
		bcc :+
		stx dipmenu_states_1b
:
		lsr a
		bcc :+
		stx dipmenu_states_1b+1
:
		; process next dip
		lda dip_DIFFICULTY

		lsr a
		bcc :+
		stx dipmenu_states_1b+2
:
		lsr a
		bcc :+
		stx dipmenu_states_1b+3
:

		; process next dip
		ldx #1
		lda dip_ENEMY_REGEN

		and #1
		beq :+
		stx dipmenu_states_1b+4

:
		; process next dip
		lda dip_BONUS

		lsr a
		bcc :+
		stx dipmenu_states_1b+5
:
		lsr a
		bcc :+
		stx dipmenu_states_1b+6
:

		; process next dip
		lda dip_NESMODE
		
		and #1
		beq :+
		stx dipmenu_states_1b+7
:
		rts
