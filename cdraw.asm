
; SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
; SPDX-PackageOriginator: BudgeCo: Bill Budge
; SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
; SPDX-License-Identifier: MIT

; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


                .include "equates/system_atari8.equ"
                .include "equates/system_f256.equ"
                .include "equates/zeropage.equ"

                .include "equates/dlist.equ"
                .include "equates/gpak.equ"
                .include "equates/ppak.equ"


;--------------------------------------
; Zero-page equates (local)
;--------------------------------------

ITEMP                   = $0007

SBEGIN                  = $00D0         ; [word]
SEND                    = $00D2         ; [word]

TEMPRECT                = $00D2

DBEGIN                  = $00D4
DEND                    = $00D4

EXTRA                   = $00D6

LEFTEDGE                = $00D7

RTMOD8                  = $00DB
TBASE                   = $00DB         ; [word]


;--------------------------------------
; Code equates
;--------------------------------------

HIRES                   = $2B00


;--------------------------------------
;--------------------------------------
                * = $2480
;--------------------------------------

;======================================
; DRAW BITMAPS AND ICONS
;======================================
GETBITS         sta TEMPBITS
                stx TEMPBITS+1

                ldy #$00
                lda (TEMPBITS),Y
                sta BASE2
                sta XOffDraw._setAddr1+1    ; BITMAP

                iny
                lda (TEMPBITS),Y
                sta BASE2+1
                sta XOffDraw._setAddr1+2    ; BITMAP+1

                iny
                lda (TEMPBITS),Y
                sta VERT

                iny
                lda (TEMPBITS),Y
                sta HDIV8

                iny
                lda (TEMPBITS),Y
                sta HMOD8

                iny
                lda (TEMPBITS),Y
                sta HEIGHT

                iny
                lda (TEMPBITS),Y
                sec
                sbc #$01
                sta WIDTH

                rts


;--------------------------------------
; PEN MODE ROUTINES
;--------------------------------------
STORE1
_next1          lda (BASE2),Y
                sta (BASE1),Y

                dey
                bpl _next1
                bmi DrawBits._ENTRY2    ; [unc]


;--------------------------------------
;
;--------------------------------------
GRAB1
_next1          lda (BASE1),Y
                sta (BASE2),Y

                dey
                bpl _next1
                bmi DrawBits._ENTRY2    ; [unc]


;--------------------------------------
;
;--------------------------------------
CLR1            ; alias
OR1             ; alias

XOR1
_next1          lda (BASE2),Y
                eor (BASE1),Y
                sta (BASE1),Y

                dey
                bpl _next1
                bmi DrawBits._ENTRY2    ; [unc]


;======================================
; SET GRAPHICS MODE
;======================================
SetMode         lda PTBL1,Y                 ; Y=pen mode
                sta DrawBits._pmpatch1+1    ; make all patches

                lda PTBL2,Y
                sta HorzLine._pmpatch2+1

                lda PTBL3,Y
                sta HorzLine._pmpatch3+1
                sta VertLine._pmpatch4+1

                rts

;--------------------------------------

PTBL1           .byte <STORE1
                .byte <GRAB1
                .byte <XOR1
                .byte <CLR1
                .byte <OR1
PTBL2           .byte <STORE2
                .byte <GRAB2
                .byte <XOR2
                .byte <CLR2
                .byte <OR2
PTBL3           .byte <STORE3
                .byte <GRAB3
                .byte <XOR3
                .byte <CLR3
                .byte <OR3


;======================================
;
;======================================
DrawBits        jsr GETBITS

_ENTRY1         ldy VERT                ; get base address
                cpy #$C0
                bcs _XIT

                lda LO_,Y
                clc
                adc HDIV8
                sta BASE1

                lda HI_,Y
                adc #$00
                sta BASE1+1

                ldy WIDTH               ; blast out a scan line
_pmpatch1       jmp STORE1              ; [smc] off to appropriate mode

_ENTRY2         dec HEIGHT              ; any more?
                beq _XIT                ;   no

                lda BASE2               ; next row in bitmap
                sec
                adc WIDTH               ; width - 1
                sta BASE2
                lda BASE2+1
                adc #$00
                sta BASE2+1

                inc VERT
                bne _ENTRY1

_XIT            rts


;--------------------------------------
;--------------------------------------
; bitmap drawing offset by a number mod 7

SRADDR          .byte >SHFRSLT
                .byte >SHFRSLT+$100
                .byte >SHFRSLT+$200
                .byte >SHFRSLT+$300
                .byte >SHFRSLT+$400
                .byte >SHFRSLT+$500
                .byte >SHFRSLT+$600
SOADDR          .byte >SHFOUT
                .byte >SHFOUT+$100
                .byte >SHFOUT+$200
                .byte >SHFOUT+$300
                .byte >SHFOUT+$400
                .byte >SHFOUT+$500
                .byte >SHFOUT+$600


;======================================
;
;======================================
XOffDraw        jsr GETBITS

                ldy HMOD8               ; byte aligned?
                beq DrawBits._ENTRY1    ;   yes, special case

                lda SRADDR-1,Y          ; poke in shift result,
                sta _setAddr3+2         ; shift out table addresses
                lda SOADDR-1,Y
                sta _setAddr2+2

_next1          ldy VERT                ; get base
                cpy #$C0
                bcs DrawBits._XIT

                lda LO_,Y
                sec
                adc HDIV8               ; add byte offset + 1
                sta BASE1

                lda HI_,Y
                adc #$00
                sta BASE1+1

                ldy WIDTH               ; do a scan line
                lda #$00                ; boundary shift result
_setAddr1       ldx $FFFF,Y             ; [smc] poke bitmap address
_setAddr2       ora SHFOUT,X            ; [smc] hi-byte modified
                eor (BASE1),Y
                sta (BASE1),Y

_setAddr3       lda SHFRSLT,X           ; [smc] hi-byte modified
                dey
                bpl _setAddr1

                dec BASE1+1             ; Y=$FF

                eor (BASE1),Y
                sta (BASE1),Y

                dec HEIGHT
                beq DrawBits._XIT

                lda _setAddr1+1         ; BITMAP
                sec
                adc WIDTH
                sta _setAddr1+1

                lda _setAddr1+2         ; BITMAP+1
                adc #$00
                sta _setAddr1+2

                inc VERT
                bne _next1


;======================================
; RECTANGLE ROUTINES
;======================================
GetRectangle    sta TEMPRECT
                stx TEMPRECT+1

                ldy #$00
                lda (TEMPRECT),Y
                sta TOP

                iny
                lda (TEMPRECT),Y
                sta LFTDIV8

                iny
                lda (TEMPRECT),Y
                sta LFTMOD8

                tax
                lda MASKS,X
                eor #$FF
                sta LEFTMASK

                iny
                lda (TEMPRECT),Y
                clc
                adc TOP
                sta BOTTOM

                iny
                iny
                lda (TEMPRECT),Y
                clc
                adc LFTMOD8
                cmp #$08
                bcc _1

                sbc #$08
_1              sta RTMOD8
                tax

                dey
                lda (TEMPRECT),Y
                adc LFTDIV8
                sta RTDIV8

                lda MASKS+1,X
                sta RIGHTMASK

                rts


;--------------------------------------
;--------------------------------------
; mask and edge tables

MASKS           .byte $00,$80,$C0,$E0
                .byte $F0,$F8,$FC,$FE
                .byte $FF
EDGES           .byte $80,$40,$20,$10
                .byte $08,$04,$02,$01


;======================================
; HORIZONTAL LINE DRAWING
;======================================
HorzLine        lda LO_,X               ; X=scan line
                sta BASE1
                lda HI_,X
                sta BASE1+1

                lda LEFTMASK
                ldy LFTDIV8
                cpy RTDIV8              ; rect in a byte?
                beq _1

_pmpatch2       jmp STORE2              ; [smc]

_1              eor RIGHTMASK
                eor #$FF
                bcs _XIT

_ENTRY1         lda RIGHTMASK

_pmpatch3
_XIT            jmp STORE3              ; [smc]


;======================================
; VERTICAL LINE  DRAWING
;======================================
VertLine        lda EDGES,X             ; Y=DIV8, X=MOD8
                sta LEFTEDGE

                ldx TOP
_next1          lda LO_,X
                sta BASE1
                lda HI_,X
                sta BASE1+1

                lda LEFTEDGE
_pmpatch4       jsr STORE3              ; [smc]

                inx
                cpx BOTTOM
                bcc _next1
                beq _next1

                rts


;--------------------------------------
; PEN MODE HANDLERS FOR RECT'S
;--------------------------------------
STORE2          ; alias
GRAB2           ; alias

XOR2A           lda #$FF
XOR2            eor (BASE1),Y
                sta (BASE1),Y

                iny
                cpy RTDIV8
                bcc XOR2A
                bcs HorzLine._ENTRY1    ; [unc]


;--------------------------------------
;
;--------------------------------------
CLR2A           lda #$FF
CLR2            eor #$FF
                and (BASE1),Y
                sta (BASE1),Y

                iny
                cpy RTDIV8
                bcc CLR2A
                bcs HorzLine._ENTRY1    ; [unc]


;--------------------------------------
;
;--------------------------------------
OR2A            lda #$FF
OR2             ora (BASE1),Y
                sta (BASE1),Y

                iny
                cpy RTDIV8
                bcc OR2A
                bcs HorzLine._ENTRY1    ; [unc]


;======================================
;
;======================================
STORE3          ; alias
GRAB3           ; alias

XOR3            eor (BASE1),Y
                sta (BASE1),Y

                rts


;======================================
;
;======================================
CLR3            eor #$FF
                and (BASE1),Y
                sta (BASE1),Y

                rts


;======================================
;
;======================================
OR3             ora (BASE1),Y
                sta (BASE1),Y

                rts


;======================================
; FRAME A RECTANGLE
;======================================
FrameRectangle  jsr GetRectangle

_ENTRY1         ldx TOP
                jsr HorzLine

                ldx BOTTOM
                cpx TOP
                beq _XIT

                jsr HorzLine

                inc TOP
                dec BOTTOM

                cpx TOP
                bcc _XIT

                ldy LFTDIV8
                ldx LFTMOD8
                jsr VertLine

                ldy RTDIV8
                ldx RTMOD8
                jmp VertLine

_XIT            rts


;======================================
; DRAW A RECTANGULAR AREA
;======================================
Drawrectangle   jsr GetRectangle

_ENTRY1         ldx TOP
_next1          jsr HorzLine

                inx
                cpx BOTTOM
                bcc _next1
                beq _next1

                rts


;--------------------------------------
; RECTANGLE HIT TEST
;--------------------------------------
InRectangle     jsr GetRectangle

                ldx PARAM+3
                lda PARAM+4
                ldy PARAM+5
                cpx LFTDIV8
                bcc _XIT1
                bne _1

                cmp LFTMOD8
                bcc _XIT1

_1              cpx RTDIV8
                bcc _2
                bne _XIT1

                cmp RTMOD8
                bcc _2
                beq _2

_XIT1           clc                     ; not within rectangle
                rts

_2              cpy TOP
                bcc _XIT1

                cpy BOTTOM
                bcc _XIT
                bne _XIT1

_XIT            sec                     ; cursor within rectangle
                rts


;======================================
; USER INTERFACE AND MISC STUFF
;======================================
GetButtons      lda TRIG0
                and TRIG1
                eor #$FF
                ror
                ror

                rts


;======================================
;
;======================================
InitCursor      jsr GETBITS

                lda BASE2
                sta CURSOR
                lda BASE2+1
                sta CURSOR+1

                lda HEIGHT
                sta CURSORHEIGHT

                ldx WIDTH
                inx
                stx CURSORWIDTH


;======================================
; SPECIAL CASE CURSOR DRAWING
;======================================
XDrawCursor     lda #<CURSOR
                ldx #>CURSOR
                jmp XOffDraw


;======================================
;
;======================================
UpdateCursor    jsr DoCursorX
                stx NEWCURSORXDIV8
                sta NEWCURSORXMOD8

                jsr DoCursorY
                sta NEWCURSORY

                jsr XDrawCursor         ; erase old one

                lda NEWCURSORXDIV8
                sta cursorX_Div8
                lda NEWCURSORXMOD8
                sta cursorX_Mod8

                lda NEWCURSORY
                sta cursorY

                jmp XDrawCursor         ; draw new one


;======================================
; READ THE PADDLES AND UPDATE THE
; CURSOR LOCATION
;======================================
DoCursorX       ldx RTCLOK+2
_delay1         dex
                bne _delay1             ; break up beat freq

                lda STKX
                lsr
                lsr
                lsr

                ldy STKX+1
                beq _1

                clc
                adc #$20
_1              tax

                lda STKX
                and #$07

                rts


;======================================
;
;======================================
GetCursorX      lda cursorX_Div8
                cmp #$20
                bcs _XIT1

                asl
                asl
                asl
                adc cursorX_Mod8
                bcc _XIT

_XIT1           lda #$FF
_XIT            rts


;======================================
;
;======================================
DoCursorY       lda STKY
                rts


;======================================
;
;======================================
WAIT_           sec
_delay1         pha

_delay2         sbc #$01
                bne _delay2

                pla
                sbc #$01
                bne _delay1

                rts


;======================================
; CURSOR IN RECTANGLE TEST
;======================================
CursorInRectangle
                ldy cursorX_Div8        ; preserve A,X
                sty PARAM+3
                ldy cursorX_Mod8
                sty PARAM+4

                ldy cursorY
                sty PARAM+5

                sta NEWITEM
                stx NEWITEM+1
                jmp InRectangle


;======================================
; MENU SELECTION UTILITY
;======================================
DoMenu          sta PARAM
                stx PARAM+1

_next1          ldy #$00
_next2          lda (PARAM),Y
                bne _3

                ldx lastItem+1          ; end of list
                beq _1

                lda lastItem
                jsr Drawrectangle       ; PENMODE=XOR

                stz lastItem+1

_1              jsr UpdateCursor
                jsr GetButtons
                bmi _next1

                ldx lastItem+1
                bne _2

                rts

_2              lda lastItem
                jmp Drawrectangle

_3              sty YTEMP               ; get rectangle
                pha

                iny
                lda (PARAM),Y
                tax

                pla
                jsr CursorInRectangle   ; cursor within it?
                bcc _4                  ;   no

                jsr SELECT

                ldy YTEMP
                bcc _next2

                iny
                iny
                lda (PARAM),Y           ; jump to selection
                sta _setAddr1+1

                iny
                lda (PARAM),Y
                sta _setAddr1+2

_setAddr1       jmp $FFFF

_4              lda YTEMP               ; next menu item
                adc #$04
                tay
                bne _next2


;======================================
;
;======================================
SELECT          lda lastItem+1          ; no selection?
                beq _2                  ;   nope

                cmp NEWITEM+1           ; same as new one?
                bne _1                  ;   no

                lda lastItem
                cmp NEWITEM
                beq _3

_1              jsr Drawrectangle._ENTRY1    ;   no, not same, turn new one on

                lda lastItem
                ldx lastItem+1
                ldy NEWITEM
                sty lastItem
                ldy NEWITEM+1
                sty lastItem+1

                jsr Drawrectangle            ; turn old one off
                jmp _3

_2              jsr Drawrectangle._ENTRY1    ; turn on new one

                lda NEWITEM
                sta lastItem
                lda NEWITEM+1
                sta lastItem+1

_3              jsr UpdateCursor

                jsr GetButtons
                clc
                bmi _XIT

                lda lastItem
                ldx lastItem+1

                jsr Drawrectangle

                sec
_XIT            rts


;======================================
; INIT STUFF
;======================================
INIT            lda #>HIRES
                sta BASE1+1
                ldy #<HIRES
                sty BASE1

_next1          tya                     ; A=0
_next2          sta (BASE1),Y

                dey
                bne _next2

                inc BASE1+1
                lda BASE1+1
                cmp #>dlistMain         ; end reached yet?
                bcc _next1              ;   no

                rts


;======================================
;
;======================================
GETPTRS         pha

                lda $0000,X
                sta SBEGIN
                lda $0001,X
                sta SBEGIN+1

                lda $0000,Y
                sta DBEGIN
                lda $0001,Y
                sta DBEGIN+1

                pla
                tax
                lda $0000,X
                sta SEND
                lda $0001,X
                sta SEND+1

                rts


;======================================
;
;======================================
MoveUp          jsr GETPTRS

_next1          dec SEND+1
                dec DEND+1

                lda SBEGIN+1
                cmp SEND+1
                bcc _2
                bne _1

                lda SBEGIN
                cmp SEND
                bcc _2
                beq _2

_1              lda SBEGIN              ; C=1
                sbc SEND
                beq _XIT

                sta EXTRA

                ldy #$FF
_next2          lda (SEND),Y
                sta (DEND),Y

                dey
                cpy EXTRA
                bcs _next2

_XIT            rts

_2              ldy #$FF
_next3          lda (SEND),Y
                sta (DEND),Y

                dey
                bne _next3

                lda (SEND),Y
                sta (DEND),Y

                jmp _next1


;======================================
;
;======================================
MoveDown        jsr GETPTRS

_next1          ldx SBEGIN+1
                inx
                cpx SEND+1
                bcc _2
                bne _1

                lda SBEGIN
                cmp SEND
                bcc _2
                beq _2

_1              lda SEND
                sbc SBEGIN
                beq _XIT

                sta EXTRA

                ldy #$00
_next2          lda (SBEGIN),Y
                sta (DBEGIN),Y

                cpy EXTRA
                beq _XIT

                iny
                bne _next2

_XIT            rts

_2              ldy #$00
_next3          lda (SBEGIN),Y
                sta (DBEGIN),Y

                iny
                bne _next3

                inc SBEGIN+1
                inc DBEGIN+1
                bne _next1


;======================================
;
;======================================
AddiYX          clc
                adc $0000,Y
                sta $0000,X
                lda $0001,Y
                adc #$00
                sta $0001,X

                rts


;======================================
;
;======================================
AddYX           lda $0000,X
                clc
                adc $0000,Y
                sta $0000,X

                lda $0001,X
                adc $0001,Y
                sta $0001,X

                rts


;======================================
;
;======================================
SubtractiYX     sta TEMP

                sec
                lda $0000,Y
                sbc TEMP
                sta $0000,X

                lda $0001,Y
                sbc #$00
                sta $0001,X

                rts


;======================================
;
;======================================
SubtractYX      lda $0000,X
                sec
                sbc $0000,Y
                sta $0000,X

                lda $0001,X
                sbc $0001,Y
                sta $0001,X

                rts


;======================================
;
;======================================
CMPYX           lda $0001,Y
                cmp $0001,X
                bcc _XIT
                bne _XIT

                lda $0000,Y
                cmp $0000,X

_XIT            rts


;======================================
; MINI-FONT STUFF
;======================================
CharTo          sta CHARBITS+4
                stx CHARBITS+3
                sty CHARBITS+2

                rts


;======================================
;
;======================================
PrintChar       sta TEMP
                cmp #$24
                beq _1

                asl                     ; A*8 - TEMP
                asl
                asl
                sec
                sbc TEMP

                clc
                adc #<FONT
                sta CHARBITS
                lda #$00
                adc #>FONT
                sta CHARBITS+1

                lda #<CHARBITS
                ldx #>CHARBITS
                jsr XOffDraw

_1              ldy TEMP
                lda CWIDTH,Y

                sec
                adc CHARBITS+4
_next1          cmp #$08
                bcc _2

                inc CHARBITS+3

                sbc #$08
                bcs _next1

_2              sta CHARBITS+4

                rts


;======================================
;
;======================================
Print_          sta TBASE
                stx TBASE+1

                ldy #$00
_next1          lda (TBASE),Y
                bmi _1

                sty YTEMP
                jsr PrintChar

                ldy YTEMP
                iny
                bne _next1

_1              and #$7F
                jmp PrintChar


;--------------------------------------
;--------------------------------------

                .include "data/FONT.inc"


;--------------------------------------
;--------------------------------------
VBICursor       inc STKTIMER

                lda PORTA               ; PORTA, STICK 0
                and #$0F
                cmp #$0F
                sta ITEMP
                bne _1

                stz STKTIMER

                lda CONSOL
                and #$01
                beq _1

                jsr GetButtons
                bpl _2

_1              stz RTCLOK+1
                stz ATRACT

_2              ldx #$01

                lda STKTIMER
                cmp #$2D                ; 3/4 second
                bcc _3

                ldx #$02
_3              stx STKSPEED

                lda STKY
                ror ITEMP
                bcc CRSRUP

                ror ITEMP
                bcc CRSRDWN

_ENTRY1         sta STKY

_ENTRY2         lda STKX
                ldx STKX+1
                ror ITEMP
                bcc CRSRLFT

                ror ITEMP
                bcc CRSRRT

_ENTRY3         sta STKX
                stx STKX+1

_XIT            jmp (NMICONT)


;--------------------------------------
;
;--------------------------------------
CRSRRT          clc
                adc STKSPEED
                bcc _1

                inx
_1              cpx #$01
                bcc VBICursor._ENTRY3

                cmp #$30
                bcc VBICursor._ENTRY3
_XIT            bcs VBICursor._XIT      ; [unc]


;--------------------------------------
;
;--------------------------------------
CRSRLFT         ror ITEMP
                sec
                sbc STKSPEED
                bcs VBICursor._ENTRY3

                dex
                bpl VBICursor._ENTRY3
                bmi VBICursor._XIT      ; [unc]


;--------------------------------------
;
;--------------------------------------
CRSRDWN         clc
                adc STKSPEED
                cmp #$C0
                bcc VBICursor._ENTRY1
                bcs VBICursor._ENTRY2   ; [unc]


;--------------------------------------
;
;--------------------------------------
CRSRUP          ror ITEMP
                sec
                sbc STKSPEED
                bcs VBICursor._ENTRY1
                bcc VBICursor._ENTRY2   ; [unc]


;--------------------------------------
;--------------------------------------     junk

;   ldy #$2A
;   jmp (CRSRRT._XIT+1)

;   ror
;   cpx #$FF
;   beq $2ACB

;   inx
;   bne $2ACB

;   cpx #$00
;   beq $2ACB

;   dex
;   bcs $2ACB

;   ror
;   cpy #$FF
;   beq $2AD3

;   iny
;   bne $2AD3

;   cpy #$00
;   beq $2AD3

;   dey
;   bcs $2AD3

                .byte $A0,$2A,$6C,$B8,$2A,$6A,$E0,$FF
                .byte $F0,$EA,$E8,$D0,$E7,$E0,$00,$F0
                .byte $E3,$CA,$B0,$E0,$6A,$C0,$FF,$F0
                .byte $E3,$C8,$D0,$E0,$C0,$00,$F0,$DC
                .byte $88,$B0,$D9,$F7,$22,$02,$8D,$B8
                .byte $2A
