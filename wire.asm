
; SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
; SPDX-PackageOriginator: BudgeCo: Bill Budge
; SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
; SPDX-License-Identifier: MIT

; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


                .include "equates/gpak.equ"
                .include "equates/ppak.equ"
                .include "equates/cdraw.equ"


;--------------------------------------
; System equates
;--------------------------------------

AUDF1           = $D200
AUDC1           = $D201


;--------------------------------------
; Zero-page equates
;--------------------------------------

;   Main Program
CURSORY                 = $0082
CURSORXDIV8             = $0083
CURSORXMOD8             = $0084
LASTITEM                = $008C

;   Objects
NEXTOBJ                 = $0090
OBJCOUNT                = $0091
OBJID                   = $0096
LBASE                   = $0099
SCANMODE                = $009B

;   Editor
EDITMODE                = $00A4
SLCTMODE                = $00A5
CUROBJ                  = $00A6
CURNOISE                = $00A7
CURSCORE                = $00A8
BMULT                   = $00A9
ETIMER                  = $00AA
SERIES                  = $00AB
SLICE                   = $00AC
STEMP                   = $00AD
X1_                     = $00AE
Y1_                     = $00AF
X2_                     = $00B0
Y2_                     = $00B1
MIDX                    = $00B2
WIRE                    = $00B3
CONTACTY                = $00B4
POLYB                   = $00F0

;   Temporary
PARAM                   = $00C0
TEMP                    = $00C7
XTEMP                   = $00C8
YTEMP                   = $00C9

;   Rectangle Routines
TOP                     = $00D4
LFTDIV8                 = $00D5
LFTMOD8                 = $00D6
BOTTOM                  = $00D9


;--------------------------------------
; Code equates
;--------------------------------------

;   Externals
CHAR                    = $2933


;   PBDB
LOGIC                   = $4B00


;--------------------------------------
;--------------------------------------
                * = $A000
;--------------------------------------


;======================================
;
;======================================
START           lda #$80
                sta CUROBJ
                sta SCANMODE

                jsr DRAWPOLYS

                stz CURSORY
                stz CURSORXDIV8
                stz CURSORXMOD8

                stz EDITMODE
                stz SLCTMODE

                stz CURNOISE
                jsr INITSOUND

                ldy #$02
                jsr SETMODE

                lda #<HAND
                ldx #>HAND
                jsr XOFFDRAW

                lda #<PLIER
                ldx #>PLIER
                jsr XOFFDRAW

                lda #<SCREWDRIVER
                ldx #>SCREWDRIVER
                jsr XOFFDRAW

                ldy #$38
                ldx #$21
                lda #$06
                jsr CHARTO

                lda #<QUITMSG
                ldx #>QUITMSG
                jsr PRINT_

                lda #$01
_next1          sta ANDGATE+2

                lda #<ANDGATE
                ldx #>ANDGATE
                jsr XOFFDRAW

                lda ANDGATE+2
                clc
                adc #$0F
                cmp #$5B
                bne _next1

                adc #$01                ; +2
                ldx #$01
                stx XTEMP
_next2          sta NOTE+2

                lda #<NOTE
                ldx #>NOTE
                jsr XOFFDRAW

                ldy NOTE+2
                ldx #$16
                lda #$01
                jsr CHARTO

                lda XTEMP
                jsr PRCHAR

                inc XTEMP

                lda NOTE+2
                clc
                adc #$0E
                cmp #$BF
                bne _next2

                lda #<HAND
                ldx #>HAND
                jsr INITCRSR

_main           inc ETIMER

                jsr SOUND
                jsr UPDATECRSR
                jsr GETBUTNS
                bpl _main

                lda #<TOOLB
                ldx #>TOOLB
                jsr CRSRINRECT
                bcc _1

                stz LASTITEM+1

                lda #<CMDMENU
                ldx #>CMDMENU
                jsr DOMENU
                jmp _main

_1              jsr MODE0
                jmp _main


;======================================
;
;======================================
MODE0           lda EDITMODE
                bne _1
                jmp SELECTOBJ

_1              cmp #$01
                bne _XIT
                jmp CUTWIRE

_XIT            jmp ADDWIRE


;======================================
;
;======================================
DRAWPOLYS       ldy #$00
                jsr GETOBJ

_next1          lda OBJID
                cmp #$02                ; BPOLYGON
                beq _1

                cmp #$01                ; POLYGON
                bne _2

_1              jsr DRAWOBJ

_2              inc NEXTOBJ
                ldy NEXTOBJ
                jsr GETNEXTOBJ

                ldy NEXTOBJ
                cpy OBJCOUNT
                bne _next1

                rts


;--------------------------------------
;
;--------------------------------------
INITHAND        jsr XDRAWCRSR

                lda #<HAND
                ldx #>HAND
                ldy #$00

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
SETEDMODE       sty EDITMODE

                jmp INITCRSR


;--------------------------------------
;
;--------------------------------------
INITPLIER       jsr XDRAWCRSR

                lda #<PLIER
                ldx #>PLIER
                ldy #$01
                bne SETEDMODE


;--------------------------------------
;
;--------------------------------------
INITDRIVER      jsr XDRAWCRSR

                lda #<SCREWDRIVER
                ldx #>SCREWDRIVER
                ldy #$02
                bne SETEDMODE


;--------------------------------------
;
;--------------------------------------
QUIT            lda SLCTMODE
                beq _2

                cmp #$01
                bne _1

                jsr DISPLAYPOLY
                jmp _2

_1              jsr DISPLAYAND
_2              jsr DRAWPOLYS

                pla                     ; discard return address
                pla
                rts


;--------------------------------------
;
;--------------------------------------
SELECTOBJ       lda #<TABLEB
                ldx #>TABLEB
                jsr CRSRINRECT
                bcc SOBJ2

                lda #$01
                jsr SETDSPLY

                jsr SELECTLIB
                sty CUROBJ
                beq _1

                jsr DISPLAYPOLY
                jsr DOSOUND

_wait1          jsr GETBUTNS
                bmi _wait1

                rts

_1              sty CURNOISE
                sty CURSCORE

                rts


;======================================
;
;======================================
DOSOUND         ldy CURNOISE
                beq _XIT

                lda _soundCode-1,Y
                cmp SERIES
                bmi _XIT

                sta SERIES

_XIT            rts

;--------------------------------------

_soundCode      .byte $00,$04,$0C,$14
                .byte $24,$38,$4C


;--------------------------------------
;
;--------------------------------------
SOBJ2           lda #<ANDB
                ldx #>ANDB
                jsr CRSRINRECT
                bcc _1

                lda #$02
                jsr SETDSPLY

                jsr SELECTAND
                sty CUROBJ

                jsr DISPLAYAND
                jsr DOSOUND

_wait1          jsr GETBUTNS
                bmi _wait1

                rts

_1              lda #<NOTEB
                ldx #>NOTEB
                jsr CRSRINRECT
                bcc SOBJ4

                lda CURNOISE
                jsr HLNOISE

                stz LASTITEM+1

                lda #<NOTEMENU
                ldx #>NOTEMENU
                jsr DOMENU
                beq _2

                jsr DOSOUND

                lda CUROBJ
                beq _2

                lda CURNOISE
                jsr HLNOISE
                jmp FIXOBJ

_2              stz CURNOISE

                jmp FIXOBJ

;--------------------------------------

NOTEMENU        .word NBOX1
                .word SETNOISE
                .word NBOX2
                .word SETNOISE
                .word NBOX3
                .word SETNOISE
                .word NBOX4
                .word SETNOISE
                .word NBOX5
                .word SETNOISE
                .word NBOX6
                .word SETNOISE
                .word NBOX7
                .word SETNOISE
                .byte $00


;======================================
;
;======================================
SETNOISE        lda YTEMP
                clc
                adc #$06
                lsr
                lsr
                sta CURNOISE

                rts


;--------------------------------------
;
;--------------------------------------
SOBJ4           lda SLCTMODE
                cmp #$02
                bne SOBJ5

                lda #<BMULTBOX
                ldx #>BMULTBOX
                jsr CRSRINRECT
                bcc SOBJ5

                lda BMULT
                beq _1

                lda #<BMULTBOX
                ldx #>BMULTBOX
                jsr DRAWRECT

_1              stz LASTITEM+1

                lda #<BMULTMENU
                ldx #>BMULTMENU
                jsr DOMENU
                beq _2

                lda #<BMULTBOX
                ldx #>BMULTBOX
                jsr DRAWRECT

                jmp FIXOBJ

_2              stz BMULT

                jmp FIXOBJ

;--------------------------------------

BMULTMENU       .word BMULTBOX
                .word SETBMUL
                .byte $00


;======================================
;
;======================================
SETBMUL         lda #$80
                sta BMULT

                rts


;--------------------------------------
;--------------------------------------

SCOREMENU       .word SCBOX1
                .word SETSCORE
                .word SCBOX2
                .word SETSCORE
                .word SCBOX3
                .word SETSCORE
                .word SCBOX4
                .word SETSCORE
                .word SCBOX5
                .word SETSCORE
                .word SCBOX6
                .word SETSCORE
                .word SCBOX7
                .word SETSCORE
                .word SCBOX8
                .word SETSCORE
                .word SCBOX9
                .word SETSCORE
                .word SCBOX10
                .word SETSCORE
                .word SCBOX11
                .word SETSCORE
                .word SCBOX12
                .word SETSCORE
                .word SCBOX13
                .word SETSCORE
                .word SCBOX14
                .word SETSCORE
                .word SCBOX15
                .word SETSCORE
                .byte $00


;--------------------------------------
;
;--------------------------------------
SOBJ5           lda #<SCOREB
                ldx #>SCOREB
                jsr CRSRINRECT
                bcc SOBJ6

                lda CURSCORE
                jsr HLSCORE

                stz LASTITEM+1

                lda #<SCOREMENU
                ldx #>SCOREMENU
                jsr DOMENU
                beq _1

                lda CUROBJ
                beq _1

                lda CURSCORE
                jsr HLSCORE

                jmp FIXOBJ

_1              stz CURSCORE

                jmp FIXOBJ


;======================================
;
;======================================
SETSCORE        lda YTEMP
                clc
                adc #$06
                lsr
                lsr
                sta CURSCORE

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
SOBJ6           rts


;--------------------------------------
;
;--------------------------------------
ADDWIRE         lda SLCTMODE
                cmp #$02
                bne _XIT

                jsr SELECTLIB
                beq _XIT

                sty TEMP

                ldy #$09
                lda (LBASE),Y
                bmi _XIT

                lda TEMP
                ldx CUROBJ
                ldy TIMES4-1,X
                cmp LOGIC,Y
                beq _XIT

                cmp LOGIC+1,Y
                beq _XIT

                cmp LOGIC+2,Y
                beq _XIT

                lda LOGIC,Y
                bne _1

                lda TEMP
                sta LOGIC,Y

                ldy #$02
                jsr DRAWWIRE
                jmp _wait1

_1              iny
                lda LOGIC,Y
                bne _2

                lda TEMP
                sta LOGIC,Y

                ldy #$07
                jsr DRAWWIRE
                jmp _wait1

_2              iny
                lda LOGIC,Y
                bne _XIT

                lda TEMP
                sta LOGIC,Y

                ldy #$0C
                jsr DRAWWIRE

_wait1          jsr GETBUTNS
                bmi _wait1

_XIT            rts


;--------------------------------------
;
;--------------------------------------
CUTWIRE         lda SLCTMODE
                cmp #$02
                bne ADDWIRE._XIT

                jsr SELECTLIB
                beq ADDWIRE._XIT

                sty TEMP
                tya

                ldx CUROBJ
                ldy TIMES4-1,X
                cmp LOGIC,Y
                bne _1

                lda #$00
                sta LOGIC,Y

                lda TEMP
                ldy #$02
                jsr DRAWWIRE
                jmp ADDWIRE._wait1

_1              iny
                cmp LOGIC,Y
                bne _2

                lda #$00
                sta LOGIC,Y

                lda TEMP
                ldy #$07
                jsr DRAWWIRE
                jmp ADDWIRE._wait1

_2              iny
                cmp LOGIC,Y
                bne ADDWIRE._XIT

                lda #$00
                sta LOGIC,Y

                lda TEMP
                ldy #$0C
                jsr DRAWWIRE
                jmp ADDWIRE._wait1


;--------------------------------------
;
;--------------------------------------
FIXOBJ          ldx CUROBJ
                beq FIXOBJ3

                lda SLCTMODE
                cmp #$01
                bne _1

                lda CURNOISE
                asl
                asl
                asl
                asl
                ora CURSCORE

                ldy #$09
                sta (LBASE),Y

                rts

_1              cmp #$02
                bne FIXOBJ3

                lda CURNOISE
                asl
                asl
                asl
                asl
                ora BMULT
                ora CURSCORE
                sta TEMP

                lda TIMES4-1,X
                clc
                adc #$03
                tay

                lda TEMP
                sta LOGIC,Y

                rts

;--------------------------------------

TIMES4          .byte $00,$04,$08
                .byte $0C,$10,$14


;--------------------------------------
;
;--------------------------------------
FIXOBJ3         rts


;======================================
;
;======================================
SETDSPLY        cmp SLCTMODE
                bne SETDSP5

_ENTRY1         cmp #$00
                bne _1

                rts

_1              cmp #$01
                bne _XIT
                jmp DISPLAYPOLY

_XIT            jmp DISPLAYAND


;--------------------------------------
;
;--------------------------------------
SETDSP5         pha

                lda SLCTMODE
                jsr SETDSPLY._ENTRY1
                jsr CLEARSB

                pla
                sta SLCTMODE

                cmp #$01
                bne _XIT
                jmp SCMENU

_XIT            jmp BNMENU


;======================================
;
;======================================
SELECTLIB       jsr SELECTPOLY
                bcc _1

                tay
                jsr GETOBJ

                lda OBJID
                cmp #$03
                beq _2

_1              ldy #$00
                beq _XIT

_2              ldy NEXTOBJ
_XIT            rts


;======================================
;
;======================================
SELECTAND       ldy #$06
                lda CURSORY
_next1          cmp TIMES15-1,Y
                bcs SELECTLIB._XIT

                dey
                bpl _next1

;--------------------------------------

TIMES15         .byte $00,$0F,$1E
                .byte $2D,$3C,$4B


;======================================
;
;======================================
DISPLAYPOLY     lda CUROBJ
                beq _XIT

                ldy #$09
                lda (LBASE),Y
                and #$7F
                sta TEMP

                and #$0F
                sta CURSCORE

                jsr HLSCORE

                lda TEMP
                lsr
                lsr
                lsr
                lsr
                sta CURNOISE

                jsr HLNOISE
                jsr GETBOUNDS

                lda PARAM
                sta POLYB

                lda PARAM+1
                sec
                sbc PARAM
                clc
                adc #$01
                sta POLYB+3

                ldy PARAM+2
                lda DIV8,Y
                sta POLYB+1

                lda MOD8,Y
                sta POLYB+2

                lda PARAM+3
                sec
                sbc PARAM+2
                tay

                lda DIV8,Y
                sta POLYB+4

                lda MOD8,Y
                sta POLYB+5

                lda #<POLYB
                ldx #>POLYB
                jsr DRAWRECT

_XIT            rts


;======================================
;
;======================================
DISPLAYAND      ldx CUROBJ
                beq DISPLAYPOLY._XIT

                ldy TIMES4-1,X
                sty WIRE

                lda LOGIC,Y
                beq _1

                ldy #$02
                jsr DRAWWIRE

_1              inc WIRE
                ldy WIRE
                lda LOGIC,Y
                beq _2

                ldy #$07
                jsr DRAWWIRE

_2              inc WIRE
                ldy WIRE
                lda LOGIC,Y
                beq _3

                ldy #$0C
                jsr DRAWWIRE

_3              inc WIRE
                ldy WIRE
                lda LOGIC,Y
                sta TEMP

                and #$0F
                sta CURSCORE

                jsr HLSCORE

                lda TEMP
                and #$80
                sta BMULT
                bpl _4

                lda #<BMULTBOX
                ldx #>BMULTBOX
                jsr DRAWRECT

_4              lda TEMP
                lsr
                lsr
                lsr
                lsr
                and #$07
                sta CURNOISE

                jsr HLNOISE

                ldy CUROBJ
                lda TIMES15-1,Y
                sta ANDBOX

                lda #<ANDBOX
                ldx #>ANDBOX
                jmp DRAWRECT


;======================================
;
;======================================
HLSCORE         beq _XIT

                tay
                lda SCVERT-1,Y
                sta SCBOX

                lda #<SCBOX
                ldx #>SCBOX
                jmp DRAWRECT

_XIT            rts

;--------------------------------------

SCVERT          .byte $0C,$16,$20,$2A
                .byte $34,$3E,$48,$52
                .byte $5C,$66,$70,$7A
                .byte $84,$8E,$98


;======================================
;
;======================================
HLNOISE         beq HLSCORE._XIT

                tay
                lda NVERT-1,Y
                sta NBOX

                lda #<NBOX
                ldx #>NBOX
                jmp DRAWRECT

;--------------------------------------

NVERT           .byte $5C,$6A,$78,$86
                .byte $94,$A2,$B0


;======================================
;
;======================================
DRAWWIRE        sty CONTACTY
                tay

                jsr GETOBJ
                jsr GETBOUNDS

                lda PARAM+3
                sta X1_
                lda PARAM
                sta Y1_

                lda #$A0
                sta X2_

                ldy CUROBJ
                lda TIMES15-1,Y
                clc
                adc CONTACTY
                sta Y2_

                ldx X1_
                lda DIV8,X
                sta POLYB+1

                lda MOD8,X
                sta POLYB+2

                lda X2_
                clc
                adc X1_
                ror
                sta MIDX

                sec
                sbc X1_
                tax

                lda DIV8,X
                sta POLYB+4
                lda MOD8,X
                sta POLYB+5

                lda #<POLYB
                ldx #>POLYB
                jsr GETRECT

                ldx Y1_
                jsr HLINE

                ldx MIDX
                lda DIV8,X
                sta POLYB+1

                lda MOD8,X
                sta POLYB+2

                lda X2_
                sec
                sbc MIDX
                tax

                lda DIV8,X
                sta POLYB+4
                lda MOD8,X
                sta POLYB+5

                lda #<POLYB
                ldx #>POLYB
                jsr GETRECT

                ldx Y2_
                jsr HLINE

                lda #<POLYB
                ldx #>POLYB
                jsr GETRECT

                ldx Y1_
                ldy Y2_
                cpy Y1_
                bcs _1

                ldx Y2_
                ldy Y1_

_1              inx
                dey
                stx TOP
                sty BOTTOM

                ldy LFTDIV8
                ldx LFTMOD8
                jmp VLINE


;======================================
; clear scoreboard
;======================================
CLEARSB         ldy #$03
                jsr SETMODE

                lda #<SCOREB
                ldx #>SCOREB
                jsr DRAWRECT

                ldy #$02
                jmp SETMODE


;--------------------------------------
;
;--------------------------------------
SCMENU          ldy #$00
                lda #$04
_next1          sty XTEMP
                sta CHAR+2

                lda #$1A
                sta CHAR+3
                lda #$02
                sta CHAR+4

                lda TXTLO,Y
                ldx TXTHI,Y
                jsr PRINT_

                lda CHAR+2
                clc
                adc #$0A

                ldy XTEMP
                iny
                cpy #$10
                bne _next1

                rts


;--------------------------------------
;
;--------------------------------------
BNMENU          ldy #$00
                lda #$04
_next1          sty XTEMP
                sta CHAR+2

                lda #$1A
                sta CHAR+3
                lda #$02
                sta CHAR+4

                lda TXTLO+16,Y
                ldx TXTHI+16,Y
                jsr PRINT_

                lda CHAR+2
                clc
                adc #$0A

                ldy XTEMP
                iny
                cpy #$10
                bne _next1

                lda #$AA
                sta CHAR+2
                lda #$19
                sta CHAR+3
                lda #$06
                sta CHAR+4

                lda #<BONUSX
                ldx #>BONUSX
                jmp PRINT_


;--------------------------------------
;--------------------------------------

CMDMENU         .word HANDB
                .word INITHAND
                .word PLIERB
                .word INITPLIER
                .word DRIVERB
                .word INITDRIVER
                .word QUITB
                .word QUIT
                .byte $00


;======================================
;
;======================================
SOUND           lda ETIMER
                and #$0F
                beq _1

                rts

_1              lda SERIES
                bmi INITSOUND._XIT

                clc
                adc SLICE
                tax
                stx STEMP

                ldy EFFECTS,X
_next1          ldx NOTES-12,Y
                beq _2

                lda #$AF
                sta AUDC1
                stx AUDF1

                iny
                bne _next1

_2              inc SLICE

                ldx STEMP
                lda EFFECTS+1,X
                bne INITSOUND._XIT


;======================================
;
;======================================
INITSOUND       lda #$A0
                sta AUDC1

                ldy #$FF
                sty SERIES

                iny
                sty SLICE

_XIT            rts

;--------------------------------------

EFFECTS         .byte $54,$0C,$54,$00
                .byte $0C,$18,$24,$30,$3C,$48,$54,$00
                .byte $54,$48,$3C,$30,$24,$18,$0C,$00
                .byte $54,$0C,$54,$0C,$54,$0C,$54,$0C,$0C,$24,$3C,$54,$54,$3C,$24,$00
                .byte $0C,$18,$24,$30,$3C,$48,$54,$48,$3C,$30,$3C,$48,$3C,$30,$3C,$48,$3C,$30,$3C,$00
                .byte $0C,$18,$24,$30,$3C,$48,$54,$54,$48,$3C,$30,$24,$18,$0C,$18,$24,$30,$3C,$48,$00
                .byte $0C,$18,$24,$30,$0C,$18,$24,$30,$0C,$18,$24,$30,$0C,$18,$24,$30,$0C,$18,$24,$00

NOTES           .byte $28,$48,$28,$48,$28,$48,$28,$48,$28,$48,$28,$00
                .byte $38,$58,$38,$58,$38,$58,$38,$58,$38,$58,$38,$00
                .byte $40,$60,$40,$60,$40,$60,$40,$60,$40,$60,$40,$00
                .byte $28,$48,$28,$48,$28,$48,$28,$48,$28,$48,$28,$00
                .byte $60,$80,$60,$80,$60,$80,$60,$80,$60,$80,$60,$00
                .byte $38,$58,$38,$58,$38,$58,$38,$58,$38,$58,$38,$00
                .byte $40,$60,$40,$60,$40,$60,$40,$60,$40,$60,$40,$00
                .byte $48,$68,$48,$68,$48,$68,$48,$68,$48,$68,$48,$00

TABLEB          .byte $00,$00,$00,$BF,$13,$01
KITB            .byte $00,$13,$02,$BF,$14,$07
TOOLB           .byte $00,$21,$04,$BF,$03,$03
HANDB           .byte $00,$21,$04,$0F,$03,$03
PLIERB          .byte $10,$21,$04,$12,$03,$03
DRIVERB         .byte $23,$21,$04,$12,$03,$03
QUITB           .byte $36,$21,$04,$0A,$03,$03
ANDB            .byte $01,$14,$01,$5A,$03,$03
ANDBOX          .byte $00,$13,$07,$0E,$03,$06
SCOREB          .byte $04,$19,$03,$BB,$05,$01
SCBOX           .byte $00,$19,$03,$0A,$03,$07
SCBOX1          .byte $0C,$19,$03,$0A,$03,$07
SCBOX2          .byte $16,$19,$03,$0A,$03,$07
SCBOX3          .byte $20,$19,$03,$0A,$03,$07
SCBOX4          .byte $2A,$19,$03,$0A,$03,$07
SCBOX5          .byte $34,$19,$03,$0A,$03,$07
SCBOX6          .byte $3E,$19,$03,$0A,$03,$07
SCBOX7          .byte $48,$19,$03,$0A,$03,$07
SCBOX8          .byte $52,$19,$03,$0A,$03,$07
SCBOX9          .byte $5C,$19,$03,$0A,$03,$07
SCBOX10         .byte $66,$19,$03,$0A,$03,$07
SCBOX11         .byte $70,$19,$03,$0A,$03,$07
SCBOX12         .byte $7A,$19,$03,$0A,$03,$07
SCBOX13         .byte $84,$19,$03,$0A,$03,$07
SCBOX14         .byte $8E,$19,$03,$0A,$03,$07
SCBOX15         .byte $98,$19,$03,$0A,$03,$07
BMULTBOX        .byte $A8,$19,$03,$0A,$05,$05
NOTEB           .byte $5C,$15,$00,$62,$02,$04
NBOX            .byte $00,$14,$03,$0D,$02,$04
NBOX1           .byte $5C,$14,$03,$0D,$02,$04
NBOX2           .byte $6A,$14,$03,$0D,$02,$04
NBOX3           .byte $78,$14,$03,$0D,$02,$04
NBOX4           .byte $86,$14,$03,$0D,$02,$04
NBOX5           .byte $94,$14,$03,$0D,$02,$04
NBOX6           .byte $A2,$14,$03,$0D,$02,$04
NBOX7           .byte $B0,$14,$03,$0D,$02,$04
QUITMSG         .byte $1A,$1E,$12,$9D

TXTLO           .byte <SCOREMSG
                .byte <SC1MSG
                .byte <SC2MSG
                .byte <SC3MSG
                .byte <SC4MSG
                .byte <SC5MSG
                .byte <SC6MSG
                .byte <SC7MSG
                .byte <SC8MSG
                .byte <SC9MSG
                .byte <SC10MSG
                .byte <SC11MSG
                .byte <SC12MSG
                .byte <SC13MSG
                .byte <SC14MSG
                .byte <SC15MSG
                .byte <BONUSMSG
                .byte <BN1MSG
                .byte <BN2MSG
                .byte <BN3MSG
                .byte <BN4MSG
                .byte <BN5MSG
                .byte <BN6MSG
                .byte <BN7MSG
                .byte <BN8MSG
                .byte <BN9MSG
                .byte <BN10MSG
                .byte <BN11MSG
                .byte <BN12MSG
                .byte <BN13MSG
                .byte <BN14MSG
                .byte <BN15MSG

TXTHI           .byte >SCOREMSG
                .byte >SC1MSG
                .byte >SC2MSG
                .byte >SC3MSG
                .byte >SC4MSG
                .byte >SC5MSG
                .byte >SC6MSG
                .byte >SC7MSG
                .byte >SC8MSG
                .byte >SC9MSG
                .byte >SC10MSG
                .byte >SC11MSG
                .byte >SC12MSG
                .byte >SC13MSG
                .byte >SC14MSG
                .byte >SC15MSG
                .byte >BONUSMSG
                .byte >BN1MSG
                .byte >BN2MSG
                .byte >BN3MSG
                .byte >BN4MSG
                .byte >BN5MSG
                .byte >BN6MSG
                .byte >BN7MSG
                .byte >BN8MSG
                .byte >BN9MSG
                .byte >BN10MSG
                .byte >BN11MSG
                .byte >BN12MSG
                .byte >BN13MSG
                .byte >BN14MSG
                .byte >BN15MSG

SCOREMSG        .byte $1C,$0C,$18,$1B,$8E
SC1MSG          .byte $01,$80
SC2MSG          .byte $02,$80
SC3MSG          .byte $03,$80
SC4MSG          .byte $04,$80
SC5MSG          .byte $05,$80
SC6MSG          .byte $01,$00,$80
SC7MSG          .byte $01,$05,$80
SC8MSG          .byte $02,$00,$80
SC9MSG          .byte $02,$05,$80
SC10MSG         .byte $03,$00,$80
SC11MSG         .byte $04,$00,$80
SC12MSG         .byte $05,$00,$80
SC13MSG         .byte $07,$05,$80
SC14MSG         .byte $01,$00,$00,$80
SC15MSG         .byte $01,$05,$00,$80

BONUSMSG        .byte $0B,$18,$17,$1E,$9C
BN1MSG          .byte $01,$94
BN2MSG          .byte $02,$94
BN3MSG          .byte $03,$94
BN4MSG          .byte $04,$94
BN5MSG          .byte $05,$94
BN6MSG          .byte $06,$94
BN7MSG          .byte $07,$94
BN8MSG          .byte $08,$94
BN9MSG          .byte $09,$94
BN10MSG         .byte $01,$00,$94
BN11MSG         .byte $02,$00,$94
BN12MSG         .byte $03,$00,$94
BN13MSG         .byte $04,$00,$94
BN14MSG         .byte $05,$00,$94
BN15MSG         .byte $01,$00,$00,$94
BONUSX          .byte $0B,$18,$17,$1E,$1C,$A1

HAND            .word $A85C
                .byte $02,$22,$04,$0C,$02
                .byte $C0,$00,$60,$00,$30,$00,$18,$60
                .byte $2C,$C0,$3E,$C0,$7F,$C0,$7F,$C0
                .byte $3F,$A0,$1F,$60,$0E,$C0,$01,$80

PLIER           .word $A87B
                .byte $12,$22,$02,$0F,$03
                .byte $1C,$00,$00,$06,$00,$00,$63,$00
                .byte $00,$3B,$80,$00,$0E,$7F,$C0,$03
                .byte $80,$70,$01,$80,$18,$01,$80,$00
                .byte $01,$80,$00,$01,$80,$00,$01,$80
                .byte $00,$01,$80,$00,$01,$80,$00,$00
                .byte $C0,$00,$00,$60,$00

SCREWDRIVER     .word $A8AF
                .byte $25,$23,$02,$10,$01
                .byte $60,$60,$60,$60,$60,$60,$60,$60
                .byte $90,$F0,$F0,$F0,$F0,$F0,$F0,$60

ANDGATE         .word $A8C6
                .byte $00,$14,$01,$0D,$04
                .byte $F3,$FF,$00,$00,$FF,$01,$C0,$00
                .byte $F3,$00,$60,$00,$03,$00,$30,$00
                .byte $03,$00,$18,$00,$F3,$00,$18,$F0
                .byte $FF,$00,$1F,$F0,$F3,$00,$18,$F0
                .byte $03,$00,$18,$00,$03,$00,$30,$00
                .byte $F3,$00,$60,$00,$FF,$01,$C0,$00
                .byte $F3,$FF,$00,$00

NOTE            .word $A901
                .byte $00,$15,$00,$0C,$01
                .byte $0E,$08,$0E,$08,$08,$08,$08,$08
                .byte $78,$F8,$F8,$70


;--------------------------------------
;--------------------------------------

                .byte $B2,$00,$40,$01,$60,$00,$4F,$01
                .byte $60,$78,$7F,$01,$60,$7F,$4F,$01
                .byte $60,$78,$40,$01,$60,$00,$40,$01
                .byte $30,$00,$4F,$01,$18,$00,$7F,$01
                .byte $0E,$00,$4F,$7F,$03,$00,$3A,$9E
                .byte $00,$18,$00,$0C,$01,$70,$10,$70
                .byte $10,$10,$10,$10,$10,$1E,$1F,$1F
                .byte $0E,$D4

                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00

                .byte $C1,$C4,$B2,$A0
