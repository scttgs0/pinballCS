
; SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
; SPDX-PackageOriginator: BudgeCo: Bill Budge
; SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
; SPDX-License-Identifier: MIT

; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


                .include "equates/zeropage.equ"


;--------------------------------------
; System equates
;--------------------------------------

CASINI          = $0002

TRIG0           = $D010
TRIG1           = $D011
CONSOL          = $D01F

AUDF1           = $D200
AUDC1           = $D201

PORTA           = $D300


;--------------------------------------
; Zero-page equates
;--------------------------------------

;NOBJ                    = $0090
;OBJC                    = $0091
;OBJID                   = $0096
;LB_                     = $0099
;MIDTOP                  = $00A0
;RUNLEN                  = $00A8
;PDL0                    = $00A9
;BTN0                    = $00AB
;BTN1                    = $00AC
;PTM1                    = $00AD
;PTM2                    = $00AE
;T__                     = $00C7
;XT_                     = $00C8
;YT_                     = $00C9
;T2_                     = $00CA
;B1_                     = $00D0
;B2_                     = $00D2
;B3_                     = $00D4
;KICK                    = $00D6
;FLIPR                   = $00D7
;FRAME                   = $00D8
;LEFTX                   = $00D9
;FDIR                    = $00DA
;FX1                     = $00DB
;FX2                     = $00DC
;FWIDTH                  = $00DD
;SERIES                  = $00E0
;SLICE                   = $00E1
;ST_                     = $00E2
;DSCORE                  = $00E3
;DBONUS                  = $00E4
;BMULT                   = $00E5
;INITMODE                = $00E6
;SCB                     = $00E7
;BSTAT                   = $00E9
;X1_                     = $00EA
;Y1_                     = $00EB
;X2_                     = $00EC
;Y2_                     = $00ED
;BDX                     = $00EE
;BDY                     = $00EF
;BXACC                   = $00F0
;BYACC                   = $00F1
;BMOVE                   = $00F2
;MIDX                    = $00F3
;HCNT                    = $00F4
;HT                      = $00F5
;HVAL                    = $00F6
;P1_                     = $00F7
;P2_                     = $00F8
;RTTTA                   = $00F9
;LFTTA                   = $00FA
;HTCNT                   = $00FB
;LASTY                   = $00FC


;--------------------------------------
; Code equates
;--------------------------------------

DIV8                    = $2100
MOD8                    = $2200
SETMODE                 = $24CF
XOFFDRAW                = $2535
DOCRSRY                 = $272A
WAIT_                   = $272D
CHARTO                  = $28D8
PRCHAR                  = $28E2
PRINT_                  = $291A
CHAR                    = $2933

LOGIC                   = $4B00
WSET                    = $4B18

PBDX                    = $7A40

CATCH2B                 = $8178

GETOBJ                  = $995E
GETNOBJ                 = $996A
MAKEHOLE                = $9E9A

PTLO                    = $B400
PTHI                    = $B4C0
VLO                     = $B580
VHI                     = $B600
RCN                     = $B680
TIME                    = $B700


;--------------------------------------
;--------------------------------------
                * = $8200
;--------------------------------------

C05625          .byte $00,$04,$08,$0C,$10,$14,$18,$1C   ; 5.625
                .byte $20,$24,$28,$2C,$30,$34,$38,$3C
                .byte $40,$44,$48,$4C,$50,$54,$58,$5C
                .byte $60,$64,$67,$6B,$6F,$73,$77,$7B
                .byte $7F,$83,$87,$8B,$8F,$93,$97,$9B
                .byte $9F,$A3,$A7,$AB,$AF,$B3,$B7,$BB
                .byte $BF,$C3,$C7,$CB,$CF,$D3,$D7,$DB
                .byte $DF,$E3,$E7,$EB,$EF,$F3,$F7,$FB
C1125           .byte $00,$04,$08,$0C,$10,$14,$18,$1B   ; 11.25
                .byte $1F,$23,$27,$2B,$2F,$33,$37,$3B
                .byte $3F,$43,$47,$4B,$4E,$52,$56,$5A
                .byte $5E,$62,$66,$6A,$6E,$72,$76,$7A
                .byte $7E,$81,$85,$89,$8D,$91,$95,$99
                .byte $9D,$A1,$A5,$A9,$AD,$B1,$B4,$B8
                .byte $BC,$C0,$C4,$C8,$CC,$D0,$D4,$D8
                .byte $DC,$E0,$E4,$E7,$EB,$EF,$F3,$F7
C225            .byte $00,$04,$07,$0B,$0F,$12,$16,$1A   ; 22.5
                .byte $1E,$21,$25,$29,$2C,$30,$34,$37
                .byte $3B,$3F,$43,$46,$4A,$4E,$51,$55
                .byte $59,$5C,$60,$64,$67,$6B,$6F,$73
                .byte $76,$7A,$7E,$81,$85,$89,$8C,$90
                .byte $94,$98,$9B,$9F,$A3,$A6,$AA,$AE
                .byte $B1,$B5,$B9,$BC,$C0,$C4,$C8,$CB
                .byte $CF,$D3,$D6,$DA,$DE,$E1,$E5,$E9
C45             .byte $00,$03,$06,$08,$0B,$0E,$11,$14   ; 45.0
                .byte $17,$19,$1C,$1F,$22,$25,$28,$2A
                .byte $2D,$30,$33,$36,$39,$3B,$3E,$41
                .byte $44,$47,$4A,$4C,$4F,$52,$55,$58
                .byte $5B,$5D,$60,$63,$66,$69,$6B,$6E
                .byte $71,$74,$77,$7A,$7C,$7F,$82,$85
                .byte $88,$8B,$8D,$90,$93,$96,$99,$9C
                .byte $9E,$A1,$A4,$A7,$AA,$AD,$AF,$B2
C675            .byte $00,$02,$03,$05,$06,$08,$09,$0B   ; 67.5
                .byte $0C,$0E,$0F,$11,$12,$14,$15,$17
                .byte $18,$1A,$1C,$1D,$1F,$20,$22,$23
                .byte $25,$26,$28,$29,$2B,$2C,$2E,$2F
                .byte $31,$33,$34,$36,$37,$39,$3A,$3C
                .byte $3D,$3F,$40,$42,$43,$45,$46,$48
                .byte $49,$4B,$4D,$4E,$50,$51,$53,$54
                .byte $56,$57,$59,$5A,$5C,$5D,$5F,$60
C7875           .byte $00,$01,$02,$02,$03,$04,$05,$05   ; 78.75
                .byte $06,$07,$08,$09,$09,$0A,$0B,$0C
                .byte $0C,$0D,$0E,$0F,$10,$10,$11,$12
                .byte $13,$14,$14,$15,$16,$17,$17,$18
                .byte $19,$1A,$1B,$1B,$1C,$1D,$1E,$1E
                .byte $1F,$20,$21,$22,$22,$23,$24,$25
                .byte $25,$26,$27,$28,$29,$29,$2A,$2B
                .byte $2C,$2C,$2D,$2E,$2F,$30,$30,$31
C84375          .byte $00,$00,$01,$01,$02,$02,$02,$03   ; 84.375
                .byte $03,$04,$04,$04,$05,$05,$05,$06
                .byte $06,$07,$07,$07,$08,$08,$09,$09
                .byte $09,$0A,$0A,$0B,$0B,$0B,$0C,$0C
                .byte $0D,$0D,$0D,$0E,$0E,$0F,$0F,$0F
                .byte $10,$10,$10,$11,$11,$12,$12,$12
                .byte $13,$13,$14,$14,$14,$15,$15,$16
                .byte $16,$16,$17,$17,$18,$18,$18,$19


;--------------------------------------
;--------------------------------------
LAUNCHRUN       bit BTN0
                bmi _1

                lda PDL0
                lsr
                lsr
                lsr
                lsr
                lsr

                ldy #$08
                cmp (LB_),Y
                bcc _XIT1
                beq _XIT

                lda (LB_),Y
                cmp #$05
                bcs _XIT

                jmp ADVANCE

_XIT1           jmp RETREAT

_XIT            rts

_1              ldy #$08
                lda (LB_),Y
                beq _XIT

                jmp RETREAT

                lda BTN0
                bpl _XIT2

                lda Y2_
                ldy #$02
                cmp (B2_),Y
                bcs _XIT2

                lda PDL0
                lsr
                lsr
                sta BDY         ; [00:3F]

                clc
                rts

_XIT2           jmp PBOUNCE


;--------------------------------------
;
;--------------------------------------
LFLIP2RUN       lda #$08
                bne _1

                lda #$08
                bne _2

                lda #$00
_1              ldx #$00
                beq _3

                lda #$00
_2              ldx #$01
_3              sta T__

                ldy #$08
                lda (LB_),Y

                ldy BTN0,X
                bpl _4

                cmp #$07
                bcs _XIT

                adc T__
                sta XT_
                tax

                ldy #$02
                lda (LB_),Y
                clc
                adc FXDVERT+1,X
                sta (LB_),Y

                ldy #$05
                lda FXHEIGHT+1,X
                sta (LB_),Y

                jsr ADVANCE

                ldx XT_
                ldy #$07
                lda FXLEN+1,X
                sta (LB_),Y

                rts

_XIT            rts

_4              cmp #$00
                beq _XIT

                ldy BTN0,X
                bmi _XIT


;======================================
;
;======================================
FLPRUN4         clc
                adc T__
                sta XT_
                tax

                ldy #$07
                lda FXLEN-1,X
                sta (LB_),Y

                jsr RETREAT

                ldx XT_
                ldy #$02
                lda (LB_),Y
                sec
                sbc FXDVERT,X
                sta (LB_),Y

                ldy #$05
                lda FXHEIGHT-1,X
                sta (LB_),Y

                rts


;--------------------------------------
;--------------------------------------
;;$8470
FLIP2INIT       lda #$08
                bne _1

                lda #$00
_1              sta T__

_next1          ldy #$08
                lda (LB_),Y
                beq LFLIP2RUN._XIT

                jsr FLPRUN4
                jmp _next1

;--------------------------------------

FXDVERT         .byte $00,$00,$02,$FF
                .byte $FF,$00,$FE,$FE
                .byte $00,$02,$FF,$FF
                .byte $00,$00,$FF,$FF
FXHEIGHT        .byte $0C,$0C,$08,$08
                .byte $07,$06,$08,$08
                .byte $08,$06,$06,$06
                .byte $05,$04,$05,$06
FXLEN           .byte $24,$24,$18,$18
                .byte $15,$12,$18,$18
                .byte $10,$0C,$0C,$0C
                .byte $0A,$08,$0A,$0C
FDDVERT         .byte $00,$00,$FE,$FF
                .byte $00,$00,$00,$00
                .byte $00,$00,$00,$00
                .byte $00,$00,$FF,$FF
FHEIGHT         .byte $0B,$09,$08,$06
                .byte $05,$04,$06,$08
                .byte $07,$06,$05,$04
                .byte $03,$03,$04,$05
FTTA            .byte $04,$03,$02,$01
                .byte $00,$1F,$1E,$1D
                .byte $04,$03,$02,$01
                .byte $00,$1F,$1E,$1D

FTBLO           .byte <FFRAME1,<FFRAME2,<FFRAME3,<FFRAME4
                .byte <FFRAME5,<FFRAME6,<FFRAME7,<FFRAME8
                .byte <SFRAME1,<SFRAME2,<SFRAME3,<SFRAME4
                .byte <SFRAME5,<SFRAME6,<SFRAME7,<SFRAME8
FTBHI           .byte >FFRAME1,>FFRAME2,>FFRAME3,>FFRAME4
                .byte >FFRAME5,>FFRAME6,>FFRAME7,>FFRAME8
                .byte >SFRAME1,>SFRAME2,>SFRAME3,>SFRAME4
                .byte >SFRAME5,>SFRAME6,>SFRAME7,>SFRAME8


;--------------------------------------
;--------------------------------------
;;$8504
LFLIP2HIT       lda #$08
                bne _1

                lda #$00
_1              sta T__

                ldx BTN0
                lda #$00
                beq _3

                ldx #$0D
                lda #$08
                bne _2

                ldx #$13
                lda #$00
_2              stx FWIDTH
                sta T__

                ldx BTN1
                lda #$80
_3              stx FDIR
                sta FLIPR

                bit BMOVE
                bmi _6
                bvs _4

                ldy Y1_
                dey
                bvc _5

_4              ldy Y2_
                iny

_5              lda X1_
                sta FX1
                lda X2_
                sta FX2
                bne _10
_6              bvs _7

                ldy X1_
                dey
                ldx X2_
                dex
                bvc _8

_7              ldy X1_
                iny
                ldx X2_
                inx

_8              sty FX1
                stx FX2

                bit BDY
                bmi _9

                ldy Y2_
                iny
                bne _10

_9              ldy Y1_
                dey
_10             sty YT_

                ldy #$08
                lda (B2_),Y
                sta FRAME

                clc
                adc T__
                tax

                ldy #$02
                lda (B2_),Y
                clc
                adc FDDVERT,X
                sta T__

                cmp YT_
                bcc _11
                bne _XIT

_11             clc
                adc FHEIGHT,X
                cmp YT_
                bcc _XIT

                dec B2_+1

                ldy #$FB
                lda (B2_),Y
                sta LEFTX

                inc B2_+1

                lda FTBLO,X
                sta B3_
                lda FTBHI,X
                sta B3_+1

                lda YT_
                sbc T__                 ; C=1
                asl
                tay

                bit FLIPR
                bmi _14

                lda (B3_),Y
                adc LEFTX               ; C=0
                cmp FX2
                bcc _12
                bne _XIT

_12             iny
                lda (B3_),Y
                clc
                adc LEFTX
                cmp FX1
                bcc _XIT

                sta T__

                lda FX2
                sbc LEFTX
                sta YT_

                lda FTTA,X
                sta B3_

                bit BMOVE
                bmi _13
                bvs _next1
                bvc _17                 ; [unc]

_13             lda T__
                cmp FX2
                bcc _next1
                bcs _17                 ; [unc]

_XIT            clc
                rts

_14             lda FWIDTH
                sec
                iny
                sbc (B3_),Y
                adc LEFTX
                cmp FX2
                bcc _15
                bne _XIT

_15             dey
                lda FWIDTH
                sec
                sbc (B3_),Y
                clc
                adc LEFTX
                cmp FX1
                bcc _XIT

                sta T__

                lda FWIDTH
                sbc FX1
                clc
                adc LEFTX
                sta YT_

                lda #$20
                sec
                sbc FTTA,X
                sta B3_

                bit BMOVE
                bmi _16
                bvs _next1
                bvc _17                 ; [unc]

_16             lda T__
                cmp FX2
                bcc _17
                bcs _next1              ; [unc]

                lda FRAME
                cmp #$05
                bcs _17

_next1          ldx #$00
                lda B3_
                bpl _19

                lda FRAME
                cmp #$05
                bcs _next1

_17             ldx #$80
                lda #$10
                clc
                adc B3_
                cmp #$20
                bcc _18

                sbc #$20
_18             sta B3_

_19             ldy YT_
                lda FLPVCTR,Y
                ldy FRAME
                beq _21

                cpy #$07
                beq _21
                bit FDIR
                bmi _20

                cpx #$80
                bne _21

                eor #$FF
                clc
                adc #$01

                jmp _22

_20             cpx #$80
                bne _22

_21             lda #$00
_22             sta KICK

                stz B3_+1

                jsr BOUNCE
                jmp PUTSP


;--------------------------------------
;--------------------------------------

FLPVCTR         .byte $01,$01,$01,$01,$04,$08,$0C,$10
                .byte $14,$18,$20,$24,$28,$2C,$30,$34
                .byte $38,$3C,$3F,$3F
FFRAME1         .byte $02,$06,$01,$07,$01,$08,$01,$09
                .byte $02,$0A,$03,$0B,$05,$0C,$07,$0D
                .byte $09,$0E,$0B,$0F,$0D,$10,$0F,$10
FFRAME2         .byte $02,$05,$01,$07,$01,$08,$01,$0A
                .byte $02,$0B,$04,$0D,$06,$0E,$09,$10
                .byte $0C,$11,$0F,$11
FFRAME3         .byte $02,$05,$01,$07,$01,$09,$01,$0B
                .byte $02,$0D,$04,$0F,$07,$11,$0A,$12
                .byte $0E,$12
FFRAME4         .byte $02,$05,$01,$08,$01,$0B,$01,$0E
                .byte $02,$11,$05,$13,$0A,$13
FFRAME5         .byte $02,$07,$01,$0B,$01,$0F,$01,$12
                .byte $02,$13,$04,$13
FFRAME6         .byte $04,$13,$02,$13,$01,$12,$01,$0F
                .byte $01,$0B,$02,$07
FFRAME7         .byte $0B,$13,$05,$13,$02,$11,$01,$0E
                .byte $01,$0B,$01,$08,$02,$05
FFRAME8         .byte $0E,$12,$0A,$12,$07,$11,$04,$0F
                .byte $02,$0D,$01,$0B,$01,$09,$01,$07
                .byte $02,$05

SFRAME1         .byte $02,$05,$01,$06,$01,$07,$02,$08
                .byte $03,$09,$05,$0A,$07,$0B,$09,$0B
SFRAME2         .byte $02,$05,$01,$06,$01,$08,$02,$09
                .byte $04,$0B,$06,$0C,$09,$0C
SFRAME3         .byte $02,$05,$01,$07,$01,$09,$02,$0B
                .byte $04,$0C,$07,$0C
SFRAME4         .byte $02,$06,$01,$09,$01,$0C,$02,$0D
                .byte $06,$0D
SFRAME5         .byte $02,$08,$01,$0B,$01,$0D,$02,$0D
SFRAME6         .byte $02,$0D,$01,$0D,$01,$0B,$02,$08
SFRAME7         .byte $06,$0D,$02,$0D,$01,$0C,$01,$09
                .byte $02,$06
SFRAME8         .byte $07,$0C,$04,$0C,$02,$0B,$01,$09
                .byte $01,$07,$02,$05


;--------------------------------------
;--------------------------------------
INITBALL        bit INITMODE
                bpl INITB2

                lda LB_
                ldx LB_+1
                jsr XOFFDRAW

                ldy #$10
                lda (LB_),Y
                bmi INITB3


;======================================
;
;======================================
DRAWBALL        ldy #$12
                lda (LB_),Y
                sta IBALL+2
                dey

                lda (LB_),Y
                tax
                lda DIV8,X
                sta IBALL+3
                lda MOD8,X
                sta IBALL+4

                lda #<IBALL
                ldx #>IBALL
                jmp XOFFDRAW

;--------------------------------------

IBALL           .word $878B
                .byte $00,$00,$00,$05,$01
                .byte $70,$F8,$F8,$F8,$70


;--------------------------------------
;
;--------------------------------------
INITB2          ldy #$02
                lda (LB_),Y
                ldy #$12
                sta (LB_),Y

                dec LB_+1

                ldy #$FB
                lda (LB_),Y
                inc LB_+1

                ldy #$11
                sta (LB_),Y

                lda #$00
                ldy #$07
                sta (LB_),Y

                ldy #$10
                sta (LB_),Y
                ldy #$13
                sta (LB_),Y
                iny

                lda #$FF
                sta (LB_),Y

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
INITB3          rts


;--------------------------------------
;
;--------------------------------------
BUMPRUN         ldy #$08
                lda (LB_),Y
                bpl _1

                lda #$00
                sta (LB_),Y

                jmp ADVANCE

_1              beq _XIT
                jmp RETREAT

_XIT            rts


;======================================
;
;======================================
TSET            ldy #$08
                lda (B2_),Y
                bne _XIT

                lda #$80
                sta (B2_),Y

_XIT            rts


;--------------------------------------
;--------------------------------------

WKICK           .byte $10


;--------------------------------------
;
;--------------------------------------
BUMPHIT         jsr TSET

                lda WKICK
                sta KICK

                lda #$80
                sta B3_+1

                jsr BOUNCE
                jmp PUTSP


;--------------------------------------
;
;--------------------------------------
BUMPINIT        ldy #$08
                lda (LB_),Y
                bmi _1
                jmp INITB

_1              and #$7F
                sta (LB_),Y

                jmp INITB

                lda B3_
                cmp #$04
                beq BUMPHIT

                cmp #$1C
                beq BUMPHIT
                jmp PBOUNCE


;======================================
;
;======================================
KNOCKRUN        ldy #$08
                lda (LB_),Y
                bpl _2

                cmp #$82
                beq _1
                jmp ADVANCE

_1              lda #$02
                sta (LB_),Y

                rts

_2              beq _XIT
                jmp RETREAT

_XIT            rts


;--------------------------------------
;
;--------------------------------------
;;$881E
KNOCK1HIT       bit BMOVE
                bmi _XIT
                bvc _1

                lda WKICK
                bne _2

_1              lda WKICK
                eor #$FF
_2              sta BDY

_next1          jsr TSET

                sec
                jmp PUTSP

_XIT            jmp PBOUNCE


;--------------------------------------
;
;--------------------------------------
;;$883A
KNOCK2HIT       bit BMOVE
                bpl KNOCK1HIT._XIT
                bvs _1

                lda WKICK
                bne _2

_1              lda WKICK
                eor #$FF
_2              sta BDX
                bne KNOCK1HIT._next1

                ldy #$08
                lda (LB_),Y
                bpl _3

                cmp #$80
                bne _4

                jmp ADVANCE

_3              beq _XIT
_4              jmp RETREAT

_XIT            rts


;--------------------------------------
;--------------------------------------
;;$8861
FLASHHIT        jsr TSET
                jsr PBOUNCE
                jmp PUTSP


;--------------------------------------
;--------------------------------------
;;$886A
ROLLHIT         jsr TSET

                clc
                jmp PUTSP._ENTRY1


;--------------------------------------
;--------------------------------------
;;$8871
GATEHIT         bit BMOVE
                bmi _next1
                bvc _1

_next1          jsr PBOUNCE
                jmp PUTSP

_1              clc
                rts


;--------------------------------------
;--------------------------------------
;;$887F
GATE2HIT        bit BDX
                bmi GATEHIT._next1

                clc
                rts


;--------------------------------------
;--------------------------------------
;;$8885
GATE3HIT        bit BDX
                bpl GATEHIT._next1

                clc
                rts


;--------------------------------------
;--------------------------------------
;;$888B
DROP1RUN        ldx #$00
                beq _1

                ldx #$80
_1              stx T2_

                ldy #$08
                lda (LB_),Y
                and #$0F
                cmp #$0F
                beq _3

                eor #$0F
                ldy #$10
                and (LB_),Y
                tax

                ldy #$08
                ora (LB_),Y
                sta (LB_),Y
                txa

                ldx #$03
_next1          lsr
                bcc _2

                jsr DRAWTARG

_2              dex
                bpl _next1

                rts

_3              ldx #$03
_next2          jsr DRAWTARG

                dex
                bpl _next2

                lda #$80
_ENTRY1         ldy #$08
                sta (LB_),Y

                lda #$00
                ldy #$10
                sta (LB_),Y

                rts


;======================================
;
;======================================
DRAWTARG        sta T__
                stx XT_

                txa
                asl
                asl
                asl
                bit T2_
                bmi _1

                dec LB_+1

                ldy #$FB
                clc
                adc (LB_),Y
                tax

                lda DIV8,X
                sta DROPTXB+3
                lda MOD8,X
                sta DROPTXB+4

                inc LB_+1

                ldy #$02
                lda (LB_),Y
                sta DROPTXB+2

                lda #<DROPTXB
                ldx #>DROPTXB
                bne _2

_1              ldy #$02
                clc
                adc (LB_),Y
                sta DROPTYB+2

                dec LB_+1

                ldy #$FB
                lda (LB_),Y
                tax

                inc LB_+1

                lda DIV8,X
                sta DROPTYB+3
                lda MOD8,X
                sta DROPTYB+4

                lda #<DROPTYB
                ldx #>DROPTYB
_2              jsr XOFFDRAW

                lda T__
                ldx XT_

                rts

;--------------------------------------

DROPTXB         .word $892B
                .byte $00,$00,$00,$03
                .byte $01,$00,$54,$54

DROPTYB         .word $8935
                .byte $00,$00,$00,$07
                .byte $01,$00,$A0,$A0
                .byte $A0,$A0,$A0,$A0


;--------------------------------------
;--------------------------------------
;;$893C
DROP1HIT        dec B2_+1
                ldy #$FB
                lda X2_
                sec
                sbc (B2_),Y

                inc B2_+1

                ldx #$03
                ldy #$01
                sty T__

_next1          cmp DHITTBL,X
                bcs _1

                asl T__

                dex
                bpl _next1

                rts

_1              ldy #$10
                lda (B2_),Y
                ora T__
                sta (B2_),Y

_ENTRY1         jsr PBOUNCE
                jmp PUTSP

;--------------------------------------

DHITTBL         .byte $02,$0A,$12,$1A


;--------------------------------------
;--------------------------------------
DROP2HIT        ldy #$02
                lda Y2_
                sec
                sbc (B2_),Y

                ldx #$03
                ldy #$01
                sty T__

_next1          cmp DHITTBL,X
                bcs _1

                asl T__

                dex
                bpl _next1

                rts

_1              ldy #$10
                lda (B2_),Y
                ora T__
                sta (B2_),Y
                bpl DROP1HIT._ENTRY1

                ldx #$00
                beq _2

                ldx #$80
_2              stx T2_

                ldy #$08
                lda (LB_),Y
                and #$0F

                ldx #$03
_next2          lsr
                bcc _3
                jsr DRAWTARG

_3              dex
                bpl _next2

                lda #$00
                jmp DROP1RUN._ENTRY1


;--------------------------------------
;--------------------------------------
CATCH1HIT       bit BMOVE
                bmi _1
                bvc _1

                dec B2_+1

                ldy #$F9
                lda (B2_),Y

                inc B2_+1

                clc
                adc #$03
                cmp X1_
                bne _1

                ldy #$02
                lda (B2_),Y
                sta YT_

                ldy #$10
                lda (B2_),Y
                sta T__

                bmi _3
                bit BSTAT
                bvs _XIT3

                ora #$80
                sta (B2_),Y

                stz BDX

                lda #$20
                sta BSTAT

                jsr PUTSP._ENTRY1

_XIT1           clc
                rts

_1              lda B3_
                bne _XIT2

                lda #$01
                bcs _2

                lda #$FF
_2              sta BDX

                sec
                rts

_XIT2           jmp PBOUNCE

_3              rol
                bmi _5

                lda BSTAT
                and #$20
                beq _XIT3

                lda T__
                and #$7F
                tax

                lda YT_
                adc CATCHSTOP,X         ; C=1
                cmp Y2_
                bne _XIT1

                inc T__
                lda T__
                and #$7F

                ldx #$40
                cmp #$03
                bne _4

                ldx #$00
                lda #$C2
_4              ldy #$10
                sta (B2_),Y
                stx BSTAT

_XIT3           sec
                rts

_5              lda YT_
                clc
                adc #$10

                cmp Y2_
                bne _XIT1
                bit BSTAT
                bvc _XIT1

                dec T__
                lda T__
                cmp #$C0
                bne _6

                lda #$00
                ldy #$08
                sta (B2_),Y

                ldy #$10
                lda #$00
_6              sta (B2_),Y

                stz BSTAT

                clc
                rts

;--------------------------------------

CATCHSTOP       .byte $03,$09,$0F


;--------------------------------------
;--------------------------------------
;;$8A4C
CATCH1INIT      lda #$00
                ldy #$08
                sta (LB_),Y

                ldy #$10
                sta (LB_),Y

                rts


;--------------------------------------
;--------------------------------------
CATCH2RUN       ldy #$08
                lda (LB_),Y
                bpl _XIT

                cmp #$84
                beq _1

                jmp ADVANCE

_1              lda #$00
                sta (LB_),Y

                ldy #$00
                lda #<CATCH2B
                sta (LB_),Y

                lda #>CATCH2B
                iny
                sta (LB_),Y

_XIT            rts


;--------------------------------------
;--------------------------------------
CATCH2HIT       jsr TSET

                lda #$80
                sta BSTAT

                sec
                jmp PUTSP


;--------------------------------------
;--------------------------------------
SPINRUN         jsr KNOCKRUN

                ldy #$08
                lda (LB_),Y
                bne _XIT

                ldy #$10
                lda (LB_),Y
                beq _XIT

                sec
                sbc #$01
                sta (LB_),Y

                lda #$80
                ldy #$08
                sta (LB_),Y

                inc DSCORE
                bne _XIT

                lda #$FF
                sta DSCORE

_XIT            rts


;--------------------------------------
;--------------------------------------
SPININIT        lda #$00
                ldy #$10
                sta (LB_),Y

                jmp BUMPINIT


;--------------------------------------
;--------------------------------------
SPINHIT         lda BDY
                bpl _1

                eor #$FF
_1              lsr

                ldy #$10
                sta (B2_),Y

                jsr TSET

                clc
                jmp PUTSP._ENTRY1


;--------------------------------------
;--------------------------------------
MAGHIT          bit BMOVE
                bmi _1
                bvs _1

                sec
                rts

_1              jsr PBOUNCE
                jmp PUTSP

;--------------------------------------

CTBL1LO         .byte <C05625
                .byte <C1125
                .byte <C225
                .byte <C45
                .byte <C675
                .byte <C7875
                .byte <C84375
CTBL1HI         .byte >C05625
                .byte >C1125
                .byte >C225
                .byte >C45
                .byte >C675
                .byte >C7875
                .byte >C84375
CTBL2LO         .byte <C84375
                .byte <C7875
                .byte <C675
                .byte <C45
                .byte <C225
                .byte <C1125
                .byte <C05625
CTBL2HI         .byte >C84375
                .byte >C7875
                .byte >C675
                .byte >C45
                .byte >C225
                .byte >C1125
                .byte >C05625


;======================================
;
;======================================
ROTATE          sta T__

                lsr
                lsr
                lsr
                sta XT_

                txa
                bpl _2

                tya
                bpl _1

                jsr QUAD2

                lda #$02
                bne _4

_1              jsr QUAD3

                lda #$01
                bne _4

_2              tya
                bpl _3

                jsr QUAD1

                lda #$03
                bpl _4

_3              lda #$00
_4              clc
                adc XT_

                cmp #$04
                bcc _5

                sbc #$04
_5              pha

                cpy #$40
                bcc _6

                ldy #$3F
_6              cpx #$40
                bcc _7

                ldx #$3F
_7              lda T__
                and #$07
                beq _9

                stx XT_

                tax
                lda CTBL1LO-1,X
                sta _setAddr1+1
                sta _setAddr4+1

                lda CTBL1HI-1,X
                sta _setAddr1+2
                sta _setAddr4+2

                lda CTBL2LO-1,X
                sta _setAddr2+1
                sta _setAddr3+1

                lda CTBL2HI-1,X
                sta _setAddr2+2
                sta _setAddr3+2

                ldx XT_
_setAddr1       lda C225,X              ; [smc]
                sec
_setAddr2       sbc C225,Y              ; [smc]
                php

                ror
                plp

                ror
                eor #$C0
                sta T__

_setAddr3       lda C225,X              ; [smc]
                clc
_setAddr4       adc C225,Y              ; [smc]
                ror
                bpl _8

                lda #$7F
_8              lsr
                tay

                ldx T__
_9              pla
                ror
                ror
                bcs FIXQ3
                bmi QUAD1               ;; FIXQ2

                rts


;======================================
;
;======================================
QUAD1           stx YT_

                tya
                eor #$FF
                clc
                adc #$01

                tax
                ldy YT_

                rts


;--------------------------------------
;
;--------------------------------------
FIXQ3           bmi QUAD3


;======================================
;
;======================================
QUAD2           txa
                eor #$FF
                clc
                adc #$01
                tax
                tya

                jmp FIXQ5


;======================================
;
;======================================
QUAD3           sty YT_
                txa
                ldx YT_

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
FIXQ5           eor #$FF
                clc
                adc #$01
                tay

                rts


;======================================
;
;======================================
PBOUNCE         stz KICK
                stz B3_+1

                ;[fall-through]


;======================================
;
;======================================
BOUNCE          ldy BDY
                ldx BDX
                lda B3_
                jsr ROTATE

                tya
                bpl _XIT

                eor #$FF
                clc
                adc #$01
                tay

                bit B3_+1
                bmi _1

_setAddr1       lda C675,Y              ; [smc]
                lsr
                lsr
                bne _1

                lda #$01
_1              clc
                adc KICK
                bmi _2

                cmp #$40
                bcc _2

                lda #$3F
_2              tay

                lda #$20
                sec
                sbc B3_

                jsr ROTATE

                stx BDX
                sty BDY

_XIT            sec
                rts


;======================================
;
;======================================
ADVANCE         ldy #$07
                lda (LB_),Y
                clc

                ldy #$00
                adc (LB_),Y
                sta (LB_),Y

                iny
                lda (LB_),Y
                adc #$00
                sta (LB_),Y

                ldy #$08
                lda (LB_),Y
                adc #$01
                sta (LB_),Y

                lda LB_
                ldx LB_+1
                jmp XOFFDRAW


;======================================
;
;======================================
RETREAT         lda LB_
                ldx LB_+1
                jsr XOFFDRAW

                ldy #$00
                lda (LB_),Y

                sec
                ldy #$07
                sbc (LB_),Y

                ldy #$00
                sta (LB_),Y

                iny
                lda (LB_),Y
                sbc #$00
                sta (LB_),Y

                ldy #$08
                lda (LB_),Y
                sbc #$01
                sta (LB_),Y

                rts


;--------------------------------------
;
;--------------------------------------
INITB           ldy #$08
                lda (LB_),Y
                beq INITQUIT

                jsr RETREAT
                bcs INITB

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
INITQUIT        rts


;======================================
;
;======================================
PUTSP           bcc _XIT

_ENTRY1         php

                ldy #$09
                lda (B2_),Y
                and #$0F
                tax

                lda SCORETBL,X
                clc
                adc DSCORE
                bcc _1

                lda #$FF
_1              sta DSCORE

                lda (B2_),Y
                lsr
                lsr
                lsr
                lsr
                beq _2

                tax
                lda SOUNDTBL-1,X
                jsr DOSOUND

_2              plp
_XIT            rts


;======================================
;
;======================================
DOSOUND         cmp SERIES
                bmi _XIT

                sta SERIES
                stz SLICE

_XIT            rts


;--------------------------------------
;--------------------------------------

SCORETBL        .byte $00,$01,$02,$03
                .byte $04,$05,$0A,$0F
                .byte $14,$19,$1E,$28
                .byte $32,$4B,$64,$96
SOUNDTBL        .byte $00,$04,$0C,$14
                .byte $24,$38,$4C


;--------------------------------------
;--------------------------------------
MOVEBALL        ldy #$10
                lda (LB_),Y
                bmi INITQUIT

                sta BSTAT

                iny
                lda (LB_),Y
                tax
                sta X1_

                clc
                adc #$04
                sta X2_

                sbc #$01                ; -2, C=0
                sta MIDX

                lda DIV8,X
                sta HBALL+3
                sta VBALL+3

                lda MOD8,X
                sta HBALL+4
                sta VBALL+4

                iny
                lda (LB_),Y
                sta HBALL+2
                sta VBALL+2
                sta Y1_

                clc
                adc #$04
                sta Y2_

                iny
                lda (LB_),Y
                sta BDX

                iny
                lda (LB_),Y
                sta BDY

                iny
                lda (LB_),Y
                sta BXACC

                iny
                lda (LB_),Y
                sta BYACC

                lda BXACC
                clc
                adc BDX
                sta BXACC

                lda BYACC
                clc
                adc BDY
                sta BYACC

                stz HTCNT

                lda PTM1
_setValue1      and #$07                ; [smc]
                bne _next1

                ldy BDY
                dey
                bpl _1

                cpy #$D1
                bcs _1

                ldy #$D1
_1              sty BDY

_next1          bit BSTAT
                bpl _2

                jmp _9

_2              lda BYACC
                bmi _3

                cmp #$20
                bcs _4

                lda BXACC
                bmi _7

                cmp #$20
                bcs _6

                jmp _9

_3              clc
                adc #$20
                sta BYACC

                ldy Y2_
                iny

                lda #$40
                sta BMOVE

                jsr CHECKVERT
                bcs _5

                jsr BALLDOWN
                jmp _5

_4              sbc #$20
                sta BYACC

                ldy Y1_
                dey

                stz BMOVE

                jsr CHECKVERT
                bcs _5

                jsr BALLUP

_5              bit BSTAT
                bmi _9

                lda BXACC
                bmi _7

                cmp #$20
                bcc _next1

_6              sbc #$20
                sta BXACC

                lda #$C0
                sta BMOVE

                ldy Y1_
                lda X2_
                adc #$00

                jsr CHECKHORIZ
                bcs _8

                ldy Y2_
                lda X2_
                adc #$01

                jsr CHECKHORIZ
                bcs _8

                jsr BALLRIGHT
                jmp _next1

_7              clc
                adc #$20
                sta BXACC

                lda #$80
                sta BMOVE

                ldy Y1_
                lda X1_
                sec
                sbc #$01

                jsr CHECKHORIZ
                bcs _8

                ldy Y2_
                lda X1_
                sbc #$00

                jsr CHECKHORIZ
                bcs _8

                jsr BALLLEFT

_8              jmp _next1

_9              ldy #$07
                lda (LB_),Y
                clc
                adc #$01

                ldx HTCNT
                bne _10

                cmp #$03
                bcc _11

                ldy Y2_
                iny

                lda #$40
                sta BMOVE

                jsr CHECKVERT

_10             lda #$00
_11             ldy #$07
                sta (LB_),Y

                ldy #$10
                lda BSTAT
                sta (LB_),Y

                iny
                lda X1_
                sta (LB_),Y

                iny
                lda Y1_
                sta (LB_),Y

                iny
                lda BDX
                sta (LB_),Y

                iny
                lda BDY
                sta (LB_),Y

                iny
                lda BXACC
                sta (LB_),Y

                iny
                lda BYACC
                sta (LB_),Y

                bit BSTAT
                bpl _XIT

                jmp DRAWBALL

_XIT            rts


;======================================
;
;======================================
DOHIT           inc HTCNT

                sty HT

                ldy OBJID
                lda VHI,Y
                bne _1

                jsr PBOUNCE
                jmp _2

_1              sta B2_+1

                lda VLO,Y
                sta B2_

                ldy #$0E
                lda (B2_),Y
                sta _setAddr1+1

                iny
                lda (B2_),Y
                sta _setAddr1+2

_setAddr1       jsr $FFFF               ; [smc]

_2              lda HVAL
                ldy HT

                rts


;======================================
;
;======================================
CHECKHORIZ      sta HVAL

                lda PTLO,Y
                sta B1_

                lda PTHI,Y
                sta B1_+1

                lda PBDX,Y
                sta HCNT

                lda HVAL
                ldy #$00
_next1          cmp (B1_),Y
                bne _1

                jsr HITLEFT
                bcc _4
                bcs FIXLEFT             ; [unc]

_1              iny
                iny
                bcc _3

                cmp (B1_),Y
                bcc _2
                bne _3

_next2          jsr HITRIGHT
                bcc _4
                bcs FIXRIGHT            ; [unc]

_2              dey
                lda (B1_),Y
                iny

                tax
                bne _next2

                lda HVAL
_3              iny
_4              iny
                cpy HCNT
                bne _next1

                clc
                rts


;======================================
;
;======================================
HITLEFT         iny
                lda (B1_),Y
                sta OBJID

                iny
                iny
                lda (B1_),Y
                and #$0F
                sta B3_

                lda OBJID
                bne _ENTRY2

_ENTRY1         lda #$10
                bne _1

_ENTRY2         lda #$20
_1              sec
                sbc B3_
                sta B3_

                jmp DOHIT


;======================================
;
;======================================
HITRIGHT        dey
                lda (B1_),Y
                sta OBJID

                iny
                iny
                lda (B1_),Y
                lsr
                lsr
                lsr
                lsr
                sta B3_

                lda OBJID
                bne HITLEFT._ENTRY1
                beq HITLEFT._ENTRY2     ; [unc]


;--------------------------------------
;
;--------------------------------------
FIXLEFT         lda BDX
                bne _ENTRY3

                lda OBJID
                bne _ENTRY2

_ENTRY1         inc BDX
                bne _ENTRY3

_ENTRY2         dec BDX

_ENTRY3         lda BXACC
                and #$1F
                sta BXACC

                rts


;--------------------------------------
;
;--------------------------------------
FIXRIGHT        lda BDX
                bne FIXLEFT._ENTRY3

                lda OBJID
                bne FIXLEFT._ENTRY1
                beq FIXLEFT._ENTRY2     ; [unc]


;--------------------------------------
;
;--------------------------------------
OFFBOARD        lda #$00
                bit BMOVE
                bvs _1

                lda #$10
_1              sta B3_

                jmp PBOUNCE


;======================================
;
;======================================
CHECKVERT       lda PTLO,Y
                sta B1_

                lda PTHI,Y
                sta B1_+1

                lda PBDX,Y
                beq OFFBOARD

                sta HCNT

                ldy #$01
_next1          dey
_next2          lda (B1_),Y
                tax

                iny
                lda (B1_),Y
                sta OBJID
                beq _5

                cpx X2_
                bcc _1
                bne _2

_1              stx P1_

                iny
                lda (B1_),Y
                cmp X1_
                bcc _3

                sta P2_

                iny
                lda (B1_),Y
                sta LFTTA

                lsr
                lsr
                lsr
                lsr
                sta RTTTA

                jsr DOVHIT
                bcc _4

_next3          lda BYACC
                and #$1F
                sta BYACC

                rts

_2              iny
_3              iny
_4              iny
                cpy HCNT
                bne _next2

                clc
                rts

_5              stz P1_

                lda #$08
                sta LFTTA
                bne _6

_next4          lda (B1_),Y
                tax

                iny
                lda (B1_),Y
                bne _8

                sta OBJID

_6              lda X2_
                cmp P1_
                bcc _7

                cpx X1_
                bcc _7

                stx P2_

                iny
                iny
                lda (B1_),Y
                and #$0F
                sta RTTTA

                dey
                dey
                jsr DOVHIT
                bcs _next3

_7              iny
                lda (B1_),Y
                sta P1_

                iny
                lda (B1_),Y
                lsr
                lsr
                lsr
                lsr
                sta LFTTA

                iny
                cpy HCNT
                bne _next4

_8              lda X2_
                cmp P1_
                bcc _9

                lda #$99
                sta P2_
                lda #$08
                sta RTTTA

                jsr DOVHIT
                bcs _next3

_9              cpy HCNT
                beq _10

                jmp _next1

_10             clc
                rts


;======================================
;
;======================================
DOVHIT          lda P2_
                cmp X1_
                beq _4

                clc
                adc P1_
                ror
                clc
                adc #$02

                cmp X2_
                bcc _3

                lda LFTTA
                and #$0F
                sta LFTTA

                lda P1_
                cmp X2_
                beq _1

                lda LFTTA
                jsr VLFIX

                sta LFTTA
                beq _2

_1              lda #$20
                sec
                sbc LFTTA
_2              sta B3_

                jsr DOHIT
                bcc _XIT

                lda BDX
                bne _XIT

                dec BDX

_XIT            rts

_3              lda RTTTA
                jsr VRFIX

                sta RTTTA
                beq _5

_4              lda #$10
                sec
                sbc RTTTA
_5              sta B3_

                jsr DOHIT
                bcc _XIT2

                lda BDX
                bne _XIT2

                inc BDX

_XIT2           rts


;======================================
;
;======================================
VLFIX           bit BMOVE
                bvc _ENTRY2

_ENTRY1         cmp #$06
                bcc VRFIX._XIT
                bcs VRFIX._ENTRY1       ; [unc]

_ENTRY2         cmp #$0B
                bcs VRFIX._XIT
                bcc VRFIX._ENTRY1       ; [unc]


;======================================
;
;======================================
VRFIX           bit BMOVE
                bvc VLFIX._ENTRY1
                bvs VLFIX._ENTRY2       ; [unc]

_ENTRY1         lda #$10
                bit BMOVE
                bvc _XIT

                lda #$00

_XIT            rts


;======================================
;
;======================================
BALLDOWN        lda #<VBALL
                ldx #>VBALL
                jsr XOFFDRAW

                inc Y1_
                inc Y2_

                lda Y1_
                sta HBALL+2
                sta VBALL+2

                rts


;======================================
;
;======================================
BALLUP          dec Y1_
                dec Y2_

                lda Y1_
                sta HBALL+2
                sta VBALL+2

                lda #<VBALL
                ldx #>VBALL
                jmp XOFFDRAW


;======================================
;
;======================================
BALLRIGHT       lda X2_
                cmp #$99
                bcs _XIT

                lda #<HBALL
                ldx #>HBALL
                jsr XOFFDRAW

                inc X1_
                inc X2_

                ldy X1_
                lda DIV8,Y
                sta HBALL+3
                sta VBALL+3

                lda MOD8,Y
                sta HBALL+4
                sta VBALL+4

_XIT            rts


;======================================
;
;======================================
BALLLEFT        lda X1_
                beq BALLRIGHT._XIT

                dec X1_
                dec X2_

                ldy X1_
                lda DIV8,Y
                sta HBALL+3
                sta VBALL+3

                lda MOD8,Y
                sta HBALL+4
                sta VBALL+4

                lda #<HBALL
                ldx #>HBALL
                jmp XOFFDRAW

;--------------------------------------

HBALL           .word $9036
                .byte $00,$00,$00,$05
                .byte $01,$48,$84,$84
                .byte $84,$48

VBALL           .word $9042
                .byte $00,$00,$00,$06
                .byte $01,$70,$88,$00
                .byte $00,$88,$70


;======================================
;
;======================================
SCORE           lda #$B0
                sta CHAR+2

                ldy #<SCORE1
                ldx #>SCORE1
                lda DSCORE

                jsr DOSCORE

                lda #$98
                sta CHAR+2

                lda #<BONUS
                sta SCB
                lda #>BONUS
                sta SCB+1

                lda DBONUS
                ldy #$05
                jsr DOSCORE._ENTRY1

                stz DSCORE
                stz DBONUS

                rts


;======================================
;
;======================================
DOSCORE         sty SCB
                stx SCB+1
                ldy #$07

_ENTRY1         clc
                adc (SCB),Y
                bcc _1

                lda #$FF
_1              sta (SCB),Y

_next1          ldx #$00
                lda (SCB),Y
_next2          cmp #$0A
                bcc _2

                sbc #$0A

                inx
                bcs _next2

_2              sta (SCB),Y

                txa
                dey
                adc (SCB),Y
                sta (SCB),Y

                tya
                bne _next1

                jmp PRSCORE._ENTRY1


;======================================
;
;======================================
INITSCORE       ldy #$11
                lda #$00
                sta DSCORE
                sta DBONUS
                sta BMULT

_next1          sta SCORE1,Y

                dey
                bpl _next1

                ldy #$80
                ldx #$15
                lda #$00
                jsr CHARTO

                lda #<BMMSG
                ldx #>BMMSG
                jsr PRINT_
                jsr PRBMULT

                ldy #$B0
                ldx #$15
                lda #$00
                jsr CHARTO

                lda #<P1MSG
                ldx #>P1MSG
                jsr PRINT_

                ldy #<SCORE1
                ldx #>SCORE1
                jsr PRSCORE

                ldy #$98
                ldx #$15
                lda #$00
                jsr CHARTO

                lda #<BMSG
                ldx #>BMSG
                jsr PRINT_

                ldy #<BONUS
                ldx #>BONUS

                ;[fall-through]


;======================================
;
;======================================
PRSCORE         sty SCB
                stx SCB+1

_ENTRY1         ldy #$00
                jsr SETMODE

                ldy CHAR+2
                ldx #$24
                lda #$00
                jsr CHARTO

                ldy #$08
_next1          sty YT_

                lda (SCB),Y
                jsr PRCHAR

                dec CHAR+3
                stz CHAR+4

                ldy YT_
                dey
                bne _next1

                ldy #$02
                jmp SETMODE


;======================================
;
;======================================
PRBMULT         ldy #$00
                jsr SETMODE

                ldy #$80
                ldx #$1C
                lda #$00
                jsr CHARTO

                ldx BMULT
                inx
                cpx #$06
                bcs _1

                stx BMULT
                txa

                jsr PRCHAR

_1              ldy #$02
                jmp SETMODE

;--------------------------------------

P1MSG           .byte $19,$15,$0A,$22
                .byte $0E,$1B,$81
BMSG            .byte $0B,$18,$17
                .byte $1E,$9C
BMMSG           .byte $0B,$18,$17
                .byte $1E,$1C,$A1
SCORE1          .byte $00,$00,$00
                .byte $00,$00,$00
                .byte $00,$00,$00
BONUS           .byte $00,$00,$00
                .byte $00,$00,$00
                .byte $00,$00,$00


;======================================
;
;======================================
SOUND           lda SERIES
                bmi INITSOUND

                clc
                adc SLICE
                tax
                stx ST_

                ldy EFFECTS,X
_next1          ldx NOTES-12,Y
                beq _1

                lda #$AF
                sta AUDC1
                stx AUDF1

                iny
                bne _next1

_1              inc SLICE

                ldx ST_
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


;======================================
;
;======================================
WIRING          ldx #$00
_next1          lda #$00
                sta T__
                stx XT_

                jsr GETST
                bpl _4

                inx
                jsr GETST
                bpl _4

                inx
                jsr GETST
                bpl _4

                inx
                stx XT_

                lda T__
                beq _4

                lda LOGIC,X
                sta YT_

                and #$0F
                tay

                lda BONUSTBL,Y
                clc
                adc DBONUS
                bcc _1

                lda #$FF
_1              sta DBONUS

                lda YT_
                bpl _2

                jsr PRBMULT

_2              lda YT_
                and #$70
                lsr
                lsr
                lsr
                lsr
                beq _3

                tax
                lda SOUNDTBL-1,X
                jsr DOSOUND

_3              dec XT_
                jsr TURNOFF

                dec XT_
                jsr TURNOFF

                dec XT_
                jsr TURNOFF

_4              lda XT_
                and #$FC
                clc
                adc #$04

                tax
                cpx #$18
                bcc _next1

                rts

;--------------------------------------

BONUSTBL        .byte $00,$01,$02,$03
                .byte $04,$05,$06,$07
                .byte $08,$09,$0A,$14
                .byte $1E,$28,$32,$64


;======================================
;
;======================================
GETST           lda LOGIC,X
                beq _1

                inc T__

                tay
                lda VLO,Y
                sta B1_
                lda VHI,Y
                sta B1_+1

                ldy #$08
                lda (B1_),Y

                rts

_1              lda #$80
_XIT            rts


;======================================
;
;======================================
TURNOFF         ldx XT_
                lda LOGIC,X
                beq GETST._XIT

                tay
                lda VLO,Y
                sta LB_
                lda VHI,Y
                sta LB_+1

                ldy #$0C
                lda (LB_),Y
                sta _setAddr1+1
                iny
                lda (LB_),Y
                sta _setAddr1+2

_setAddr1       jmp $FFFF               ; [smc]


;======================================
;
;======================================
INITWORLD       ldx WSET
                lda GRAVTBL,X
                sta MOVEBALL._setValue1+1

                ldx WSET+1
                lda TIMETBL,X
                sta PLAY._setValue1+1

                ldx WSET+2
                lda KICKTBL,X
                sta WKICK

                ldx WSET+3
                lda ELASTLO,X
                sta BOUNCE._setAddr1+1
                lda ELASTHI,X
                sta BOUNCE._setAddr1+2

                rts

;--------------------------------------

GRAVTBL         .byte $FF,$7F,$3F,$1F
                .byte $0F,$07,$03,$01
TIMETBL         .byte $30,$20,$18,$10
                .byte $0C,$08,$04,$01
KICKTBL         .byte $04,$08,$0C,$10
                .byte $18,$20,$28,$38
ELASTLO         .byte $80,$40,$00,$00
                .byte $C0,$80,$40,$00
ELASTHI         .byte $83,$83,$83,$83
                .byte $82,$82,$82,$82


;--------------------------------------
;--------------------------------------
PLAY            ldy #$00
                sty RUNLEN

                jsr GETOBJ

_next1          lda OBJID
                cmp #$03
                bne _1

                ldx NOBJ
                lda LB_
                sta VLO,X
                lda LB_+1
                sta VHI,X

                ldy #$08
                lda (LB_),Y
                sta TIME,X
                lda #$00
                sta (LB_),Y

                txa
                ldx RUNLEN
                sta RCN,X

                inc RUNLEN

                jmp _2

_1              ldx NOBJ
                lda #$00
                sta VLO,X
                sta VHI,X

_2              inc NOBJ
                ldy NOBJ
                jsr GETNOBJ

                ldy NOBJ
                cpy OBJC
                bne _next1

                sty INITMODE

                jsr INITOBJS
                jsr INITSCORE
                jsr INITSOUND
                jsr INITWORLD

                lda #$AA
                sta CASINI

                ldy #$01
                jsr MAKEHOLE

                ldy #$01
                lda MIDTOP
                ldx MIDTOP+1

_next2          clc
                adc PBDX-1,Y
                sta PTLO,Y
                bcc _3

                inx
_3              txa
                sta PTHI,Y

                lda PTLO,Y
                iny
                cpy #$C0
                bcc _next2

_next3          dey
                lda PBDX,Y
                beq _next3

                sty LASTY

_next4          inc PTM1
                bne _4

                inc PTM2

_4              lda PTM1
                and #$1F
                bne _5

                jsr DOCRSRY
                sta PDL0

_5              lda TRIG0
                eor #$FF
                ror
                ror
                sta BTN0

                lda TRIG1
                eor #$FF
                ror
                ror
                sta BTN1

                lda PORTA
                eor #$FF
                asl
                asl
                asl
                asl
                pha

                ora BTN1
                sta BTN1

                pla
                asl
                ora BTN0
                sta BTN0

_setValue1      lda #$18                ; [smc]
                jsr WAIT_

                ldy #$00
_next5          cpy RUNLEN
                bcs _7

                sty NOBJ

                ldx RCN,Y
                lda TIME,X
                and PTM1
                bne _6

                lda VLO,X
                sta LB_
                lda VHI,X
                sta LB_+1

                ldy #$0A
                lda (LB_),Y
                sta _setAddr1+1
                iny
                lda (LB_),Y
                sta _setAddr1+2

_setAddr1       jsr $FFFF               ; [smc]

                ldy NOBJ
                ldx RCN,Y
                lda TIME,X
                bne _6

                ldy #$10
                lda (LB_),Y
                rol
                bcs _6
                bmi _6

                lda Y2_
                cmp LASTY
                bne _6

                lda #$80
                sta (LB_),Y

                jsr DRAWBALL

_6              ldy NOBJ
                iny
                bne _next5

_7              lda PTM1
                and #$1F
                bne _8

                jsr SCORE

_8              lda PTM1
                and #$07
                bne _9

                jsr SOUND

_9              lda PTM1
                and #$03
                bne _10

                jsr WIRING

_10             lda CONSOL
                and #$02                ; is SELECT pressed?
                beq _11                 ;   yes

                jmp _next4

_11             jsr INITSOUND

                lda #$80
                sta INITMODE


;======================================
;
;======================================
INITOBJS        ldy #$00
_next1          cpy RUNLEN
                beq _XIT

                sty NOBJ

                ldx RCN,Y
                lda VLO,X
                sta LB_
                lda VHI,X
                sta LB_+1

                ldy #$0C
                lda (LB_),Y
                sta _setAddr1+1
                iny
                lda (LB_),Y
                sta _setAddr1+2

_setAddr1       jsr $FFFF               ; [smc]

                bit INITMODE
                bpl _1

                ldy NOBJ
                ldx RCN,Y
                lda TIME,X

                ldy #$08
                sta (LB_),Y

_1              ldy NOBJ
                iny
                bne _next1

_XIT            rts

;--------------------------------------

POLY            .byte $01,$00,$04
                .byte $B5,$B5,$A6,$A6
                .byte $1B,$2A,$2A,$1B
LAUNCHER        .byte $03,$00,$04
                .byte $F6,$F6,$F0,$F0
                .byte $49,$54,$54,$49
                .word $7B00
                .byte $49,$1E,$00,$0C,$01,$0C,$07,$80
                .word $83C0
                .word $8C23
                .word $83E9
LEFTFLIPPER     .byte $03,$00,$04
                .byte $DC,$DC,$CA,$CA
                .byte $02,$11,$11,$02
                .word $7B48
                .byte $05,$19,$02,$0C,$03,$24,$03,$80
                .word $8408
                .word $8474
                .word $8508
RIGHTFLIPPER    .byte $03,$00,$04
                .byte $EC,$EC,$DA,$DA
                .byte $02,$11,$11,$02
                .word $7C17
                .byte $05,$1B,$02,$0C,$03,$24,$03,$80
                .word $840E
                .word $8474
                .word $8518
BALL            .byte $03,$00,$04
                .byte $F9,$F9,$F5,$F5
                .byte $05,$09,$09,$05
                .word $81EF
                .byte $05,$1E,$05,$05,$01,$05,$00,$80
                .word $8C78
                .word $8755
                .word $83FB
BMP1            .byte $03,$00,$08
                .byte $B2,$B7,$B7,$B2,$AB,$A6,$A6,$AB
                .byte $30,$34,$39,$3D,$3D,$39,$34,$30
                .word $7CE6
                .byte $30,$14,$06,$0E,$03,$2A,$0F,$11
                .word $87B8
                .word $87E9
                .word $87D7
BMP2            .byte $03,$00,$08
                .byte $C1,$C5,$C5,$C1,$BC,$B8,$B8,$BC
                .byte $31,$34,$38,$3B,$3B,$38,$34,$31
                .word $7D3A
                .byte $31,$17,$00,$0B,$02,$16,$0F,$11
                .word $87B8
                .word $87E9
                .word $87D7
BMP3            .byte $03,$00,$04
                .byte $CC,$CC,$C8,$C8
                .byte $30,$3F,$3F,$30
                .word $7D66
                .byte $2F,$18,$07,$10,$01,$10,$0F,$11
                .word $87B8
                .word $87E9
                .word $87D7
BMP4            .byte $03,$00,$04
                .byte $E1,$E1,$CF,$CF
                .byte $35,$39,$39,$35
                .word $7D86
                .byte $34,$19,$05,$07,$03,$15,$0F,$11
                .word $87B8
                .word $87E9
                .word $87D7
BMP5            .byte $03,$00,$04
                .byte $E6,$EF,$EC,$E3
                .byte $30,$39,$3C,$33
                .word $7DB0
                .byte $2F,$1C,$02,$0D,$02,$1A,$0F,$11
                .word $87B8
                .word $87E9
                .word $87D7
BMP6            .byte $03,$00,$04
                .byte $FA,$FD,$F4,$F1
                .byte $30,$33,$3C,$39
                .word $7DE4
                .byte $2F,$1E,$00,$0D,$02,$1A,$0F,$11
                .word $87B8
                .word $87E9
                .word $87D7
LKICK           .byte $03,$00,$05
                .byte $AA,$B8,$B8,$A6,$A6
                .byte $44,$5A,$5D,$59,$44
                .word $7E18
                .byte $43,$14,$06,$1B,$03,$51,$0F,$22
                .word $87B8
                .word $87E9
                .word $87F9
RKICK           .byte $03,$00,$05
                .byte $D1,$D1,$BE,$BE,$CE
                .byte $44,$59,$5D,$5A,$44
                .word $7EBA
                .byte $43,$17,$04,$1B,$03,$51,$0F,$22
                .word $87B8
                .word $87E9
                .word $87F9
KICK1           .byte $03,$00,$04
                .byte $DA,$DA,$D5,$D5
                .byte $4A,$53,$53,$4A
                .word $7F5C
                .byte $48,$1A,$05,$10,$01,$10,$0F,$33
                .word $8806
                .word $87E9
                .word $881E
KICK2           .byte $03,$00,$04
                .byte $EB,$EB,$DF,$DF
                .byte $4E,$53,$53,$4E
                .word $7F8C
                .byte $4E,$1B,$06,$06,$03,$12,$0F,$33
                .word $8806
                .word $87E9
                .word $883A
ROLL1           .byte $03,$00,$04
                .byte $AD,$AD,$A9,$A9
                .byte $A0,$A4,$A4,$A0
                .word $7FC2
                .byte $A0,$15,$01,$05,$01,$05,$3F,$44
                .word $884E
                .word $87E9
                .word $886A
ROLL2           .byte $03,$00,$04
                .byte $B4,$B4,$B0,$B0
                .byte $A0,$A4,$A4,$A0
                .word $7FCC
                .byte $A0,$16,$00,$05,$01,$05,$3F,$44
                .word $884E
                .word $87E9
                .word $886A
ROLL3           .byte $03,$00,$04
                .byte $BB,$BB,$B7,$B7
                .byte $A0,$A4,$A4,$A0
                .word $7FD6
                .byte $A0,$16,$07,$05,$01,$05,$3F,$44
                .word $884E
                .word $87E9
                .word $886A
TARG1           .byte $03,$00,$04
                .byte $C6,$C6,$C0,$C0
                .byte $A0,$A2,$A2,$A0
                .word $7FE0
                .byte $A0,$18,$00,$03,$01,$03,$3F,$55
                .word $884E
                .word $87E9
                .word $8861
TARG2           .byte $03,$00,$04
                .byte $D1,$D1,$CB,$CB
                .byte $A0,$A2,$A2,$A0
                .word $7FE6
                .byte $A0,$19,$03,$03,$01,$03,$3F,$55
                .word $884E
                .word $87E9
                .word $8861
TARG3           .byte $03,$00,$04
                .byte $DC,$DC,$D6,$D6
                .byte $A0,$A2,$A2,$A0
                .word $7FEC
                .byte $A0,$1A,$06,$03,$01,$03,$3F,$55
                .word $884E
                .word $87E9
                .word $8861
TARG4           .byte $03,$00,$04
                .byte $E5,$E5,$E3,$E3
                .byte $A0,$A6,$A6,$A0
                .word $7FF2
                .byte $A0,$1C,$03,$07,$01,$07,$3F,$55
                .word $884E
                .word $87E9
                .word $8861
TARG5           .byte $03,$00,$04
                .byte $EC,$EC,$EA,$EA
                .byte $A0,$A6,$A6,$A0
                .word $8000
                .byte $A0,$1D,$02,$07,$01,$07,$3F,$55
                .word $884E
                .word $87E9
                .word $8861
TARG6           .byte $03,$00,$04
                .byte $F3,$F3,$F1,$F1
                .byte $A0,$A6,$A6,$A0
                .word $800E
                .byte $A0,$1E,$01,$07,$01,$07,$3F,$55
                .word $884E
                .word $87E9
                .word $8861
LFLIPPER2       .byte $03,$00,$04
                .byte $B2,$B2,$A6,$A6
                .byte $05,$0E,$0E,$05
                .word $801C
                .byte $07,$14,$06,$08,$02,$10,$03,$80
                .word $8400
                .word $8470
                .word $8504
RFLIPPER2       .byte $03,$00,$04
                .byte $BF,$BF,$B3,$B3
                .byte $05,$0E,$0E,$05
                .word $8078
                .byte $07,$16,$03,$08,$02,$10,$03,$80
                .word $8404
                .word $8470
                .word $8512
POLY1           .byte $01,$FF,$04
                .byte $C7,$C7,$C6,$C6
                .byte $1B,$2C,$2C,$1B
POLY2           .byte $01,$FF,$04
                .byte $CD,$CD,$CC,$CC
                .byte $20,$2C,$2C,$20
POLY3           .byte $01,$FF,$04
                .byte $F3,$D2,$D2,$F3
                .byte $25,$2C,$2A,$23
POLY4           .byte $01,$FF,$04
                .byte $D2,$F3,$F3,$D2
                .byte $16,$1D,$1F,$18
LANE1           .byte $03,$00,$04
                .byte $AD,$AD,$A9,$A9
                .byte $90,$99,$99,$90
                .word $80D4
                .byte $90,$15,$01,$0A,$01,$0A,$FF,$80
                .word $83FC
                .word $83FC
                .word $8B9F
LANE2           .byte $03,$00,$04
                .byte $B7,$B7,$B3,$B3
                .byte $90,$97,$97,$90
                .word $80DE
                .byte $90,$16,$03,$08,$01,$08,$FF,$80
                .word $83FC
                .word $83FC
                .word $8B9F
LANE3           .byte $03,$00,$04
                .byte $C1,$C1,$BD,$BD
                .byte $90,$94,$94,$90
                .word $80E6
                .byte $90,$17,$05,$05,$01,$05,$FF,$80
                .word $83FC
                .word $83FC
                .word $8B9F
GATE1           .byte $03,$00,$04
                .byte $CF,$CF,$C9,$C9
                .byte $90,$98,$98,$95
                .word $80EB
                .byte $90,$19,$01,$09,$01,$09,$FF,$80
                .word $83FC
                .word $83FC
                .word $8871
GATE2           .byte $03,$00,$04
                .byte $DD,$DD,$D7,$D7
                .byte $95,$98,$98,$90
                .word $80F4
                .byte $90,$1A,$07,$09,$01,$09,$FF,$80
                .word $83FC
                .word $83FC
                .word $8871
GATE3           .byte $03,$00,$04
                .byte $EB,$EB,$E5,$E5
                .byte $90,$98,$98,$90
                .word $80FD
                .byte $90,$1C,$05,$09,$01,$09,$FF,$80
                .word $83FC
                .word $83FC
                .word $887F
GATE4           .byte $03,$00,$04
                .byte $F9,$F9,$F3,$F3
                .byte $90,$98,$98,$90
                .word $8106
                .byte $90,$1E,$03,$09,$01,$09,$FF,$80
                .word $83FC
                .word $83FC
                .word $8885
DROP1           .byte $03,$00,$04
                .byte $D5,$D5,$B6,$B6
                .byte $64,$67,$67,$64
                .word $810F
                .byte $64,$16,$06,$04,$05,$14,$1F,$55
                .word $888B
                .word $898C
                .word $893C
                .byte $00
DROP2           .byte $03,$00,$04
                .byte $AC,$AC,$A9,$A9
                .byte $64,$83,$83,$64
                .word $8132
                .byte $64,$15,$00,$20,$01,$20,$1F,$55
                .word $888F
                .word $8990
                .word $896A
                .byte $00
CATCH1          .byte $03,$00,$05
                .byte $C4,$C4,$BA,$BA,$BF
                .byte $6C,$7E,$7E,$6C,$6E
                .word $8152
                .byte $6C,$17,$02,$13,$02,$26,$0F,$55
                .word $83FC
                .word $8A4C
                .word $89AA
CATCH2          .byte $03,$00,$04
                .byte $D5,$D5,$CB,$CB
                .byte $70,$78,$78,$70
                .word $8178
                .byte $70,$19,$03,$09,$02,$12,$0F,$05
                .word $8A57
                .word $87E9
                .word $8A74
SPIN1           .byte $03,$00,$04
                .byte $EC,$EC,$E6,$E6
                .byte $64,$68,$68,$64
                .word $81D2
                .byte $64,$1C,$06,$05,$01,$05,$07,$11
                .word $8A7F
                .word $8AA2
                .word $8AAB
MAG1            .byte $03,$00,$04
                .byte $EF,$EF,$E6,$E6
                .byte $74,$7A,$7A,$74
                .word $81E1
                .byte $74,$1C,$04,$07,$02,$0E,$0F,$B3
                .word $83FC
                .word $83FC
                .word $8ABD


;--------------------------------------
;--------------------------------------

                .byte $FF,$00,$00,$00,$00
