
; SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
; SPDX-PackageOriginator: BudgeCo: Bill Budge
; SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
; SPDX-License-Identifier: MIT

; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


LIBOBJ          = $03


;--------------------------------------
; System equates
;--------------------------------------

CASINI          = $0002

SDLSTL          = $0230
SDLSTH          = $0231
COLOR1          = $02C5
COLOR2          = $02C6
COLOR4          = $02C8

TRIG0           = $D010
TRIG1           = $D011
COLPF1          = $D017
COLPF2          = $D018
COLBK           = $D01A
CONSOL          = $D01F

AUDF1           = $D200
AUDC1           = $D201

PORTA           = $D300


;--------------------------------------
; Zero-page equates
;--------------------------------------

PLAYER                  = $0080
PLAYERCNT               = $0081
BALLS                   = $0082

SLEEPCNT                = $0083
PBASE1                  = $0084         ; [word]
PBASE2                  = $0086         ; [word]
PBASE3                  = $0088         ; [word]
PBASE4                  = $008A         ; [word]
LASTY                   = $008C
LIVEBALLS               = $008D

OBJ                     = $008E         ; [word]
NEXTOBJ                 = $0090
OBJCOUNT                = $0091
OBJID                   = $0096
FILLCOLOR               = $0097
VRTXCOUNT               = $0098
LBASE                   = $0099         ; [word]
RUNLEN                  = $00A8
PDL0                    = $00A9
BUTN0                   = $00AB
BUTN1                   = $00AC
PTIMER1                 = $00AD
PTIMER2                 = $00AE
PARAM                   = $00C0         ; [4-bytes]
TEMP                    = $00C7
XTEMP                   = $00C8
YTEMP                   = $00C9
BASE1                   = $00D0         ; [word]
BASE2                   = $00D2         ; [word]
SHIFTOUT                = $00D4
SHIFTCOUNT              = $00D5
SERIES                  = $00E0
SLICE                   = $00E1
STEMP                   = $00E2
DSCORE                  = $00E3
DBONUS                  = $00E4
BMULT                   = $00E5
INITMODE                = $00E6
SCBASE                  = $00E7         ; [word]
BSTAT                   = $00E9
L00EA                   = $00EA
L00EC                   = $00EC
Y2_                     = $00ED


;--------------------------------------
; Code equates
;--------------------------------------

SHFRSLT                 = $1300
SHFOUT                  = $1A00

DIV8                    = $2100
MOD8                    = $2200
LO_                     = $2300
HI_                     = $23C0

SETMODE                 = $24CF

XOFFDRAW                = $2535
DOCRSRY                 = $272A
WAIT_                   = $272D
CHARTO                  = $28D8
PRCHAR                  = $28E2
PRINT_                  = $291A
CHAR                    = $2933

HIRES1                  = $2B00

DLIST                   = $4910
INTERRUPTS              = $49DC
LOGIC                   = $4B00
WSET                    = $4B18         ; [4-bytes]
PBDATA                  = $4B1C
PBDX                    = $7A40
DRAWBALL                = $8766
INITBALL                = $8790
WMOD3                   = $87D6
WMOD2                   = $8BBC         ; [word]
ADVANCE                 = $8BDE
WMOD4                   = $8CDA

P1STATE                 = $A000
P2STATE                 = $A200
P3STATE                 = $A400
P4STATE                 = $A600

SLEEPCODE               = $A800
SLEEPLO                 = $A900
SLEEPHI                 = $AA00
SLEEPERS                = $AB00

PBTBLO                  = $B400
PBTBHI                  = $B4C0
VECTLO                  = $B580
VECTHI                  = $B600
RUNCHN                  = $B680
TIME                    = $B700


;--------------------------------------
;--------------------------------------
                * = $9000
;--------------------------------------

L9000           .byte $3E,$90


;======================================
;======================================
L9002           lda MOD8,Y
                sta L9033
                sta L903F

_XIT            rts


;--------------------------------------
;--------------------------------------
L900C           lda L00EA
                beq L9002._XIT

                dec L00EA
                dec L00EC

                ldy L00EA
                lda DIV8,Y
                sta L9032
                sta L903E

                lda MOD8,Y
                sta L9033
                sta L903F

                lda #$2F
                ldx #$90
                jmp XOFFDRAW


;--------------------------------------

L902F           .byte $36,$90,$00
L9032           .byte $00
L9033           .byte $00,$05,$01,$48,$84,$84,$84,$48,$42,$90,$00
L903E           .byte $00
L903F           .byte $00,$06,$01,$70,$88,$00,$00,$88,$70


;======================================
;======================================
MAIN            jsr MAKEPATCH
                jsr INTERRUPTS
                jsr GOATARI

                lda #$FF
_next1          sta GAMEMODE

                jsr MAKETBLS
                jsr INITWORLD

_next2          jsr MAIN2
                jmp _next2

                lda #$00
                beq _next1

;--------------------------------------

GAMEMODE        .byte $00


;======================================
;
;======================================
MAIN2           jsr MAKEBDSPLY

                lda #$0E
                sta TEMP

_next1          lda #$00
                jsr WAIT_

                dec TEMP
                bne _next1

                lda #$00
                sta SLEEPCNT
                jsr INITPLAYERS
                jsr GETPLAYERCNT

                ldy #$00
_next2          sty BALLS

                ldy #$00
_next3          sty PLAYER

                jsr PRINTPLAYER

                lda #$38
                jsr DOSOUND
                jsr DOBALL
                jsr TALLY

                stz BMULT
                jsr PRBMULT

                ldy PLAYER
                jsr PRINTPLAYER

                ldy PLAYER
                iny
                cpy PLAYERCNT
                bcc _next3

                ldy BALLS
                jsr MAKEBALL

                ldy BALLS
                iny
                cpy #$05
                bcc _next2

                rts


;======================================
;
;======================================
GETOPTION       lda CONSOL
                and #$04

                rts


;======================================
;
;======================================
GETSELECT       lda CONSOL
                and #$02
                ora GAMEMODE

                rts


;======================================
;
;======================================
GETSTART        lda CONSOL
                ror
                bcc _down

                lda TRIG0
                beq _down

                lda PORTA
                rol
                rol
                rol
                rol
                bcc _down
                bpl _down

_up             lda #$FF
                rts

_down           lda #$00
                rts


;======================================
;
;======================================
GETPLAYERCNT    ldy #$00
                jsr PRINTPLAYER

                ldy #$00
_next1          jsr GETSTART
                bne _1

                sty PLAYERCNT
_next2          sty XTEMP
                jsr PRINTPLAYER

                ldy XTEMP
                dey
                bpl _next2

                inc PLAYERCNT

_wait1          jsr GETSTART
                beq _wait1

                rts

_1              jsr GETSELECT
                bne _2

                pla
                pla
                pla
                pla
                jmp CLOSEOBJS._ENTRY1   ; REPLACE TIME

_2              jsr GETOPTION
                bne _next1

                iny
                cpy #$04
                bcc _3

                ldy #$01
                jsr PRINTPLAYER
                ldy #$02
                jsr PRINTPLAYER
                ldy #$03
                jsr PRINTPLAYER

                ldy #$00
                sty XTEMP
                beq _wait2              ; [unc]

_3              sty XTEMP
                jsr PRINTPLAYER

_wait2          jsr GETOPTION           ; wait for key release
                beq _wait2

                ldy XTEMP
                jmp _next1


;======================================
;
;======================================
MAKEBDSPLY      ldy #$04
_next1          sty YTEMP

                jsr MAKEBALL

                ldy YTEMP
                dey
                bpl _next1

                rts


;======================================
;
;======================================
MAKEBALL        tya
                clc
                adc #$21
                sta MBALL+3

                lda #<MBALL
                ldx #>MBALL
                jmp XOFFDRAW

;--------------------------------------

MBALL           .word $915E
                .byte $48,$00,$00,$05,$01
                .byte $70,$F8,$F8,$F8,$70


;======================================
;
;======================================
INITPLAYERS     ldy #$2C
                lda #$00
                sta DSCORE
                sta DBONUS

_next1          sta SCORE1,Y

                dey
                bpl _next1

                jsr INITSOUND

                stz INITMODE
                jsr INITOBJS

                ldx #$00
                jsr SAVEPLAYER
                ldx #$01
                jsr SAVEPLAYER
                ldx #$02
                jsr SAVEPLAYER
                ldx #$03
                jmp SAVEPLAYER


;======================================
; TALLY BMULT * BONUS, SCORE
;======================================
TALLY           ldy PLAYER
                lda SCTBLO,Y
                sta SCBASE
                lda SCTBHI,Y
                sta SCBASE+1

_next1          ldy #$07
_next2          ldx #$00
                lda (SCBASE),Y
                clc
                adc BONUS,Y

_next3          cmp #$0A
                bcc _1

                sbc #$0A

                inx
                bcs _next3

_1              sta (SCBASE),Y

                txa
                dey
                adc (SCBASE),Y
                sta (SCBASE),Y

                tya
                bne _next2

                lda BMULT
                cmp #$01
                beq _2

                dec BMULT
                bpl _next1

_2              lda #$00
                ldy #$07
_next4          sta BONUS,Y

                dey
                bpl _next4

                jmp SCORE


;======================================
; MAKE SOME TABLES
;======================================
MAKETBLS        lda #>SHFRSLT
                sta BASE1+1

                lda #>SHFOUT
                sta BASE2+1

                ldy #$00
                sty BASE1
                sty BASE2

                lda #$01
                sta SHIFTCOUNT

_next1          stz SHIFTOUT

                tya
                ldx SHIFTCOUNT
_next2          lsr
                ror SHIFTOUT

                dex
                bne _next2

                sta (BASE1),Y

                lda SHIFTOUT
                sta (BASE2),Y

                iny
                bne _next1

                inc BASE1+1
                inc BASE2+1

                inc SHIFTCOUNT
                lda SHIFTCOUNT
                cmp #$08
                bne _next1

; make DIV8,MOD8,LO,HI

                lda #$00
                tay
                tax
_next3          sta MOD8,Y
                pha

                txa
                sta DIV8,Y

                pla
                clc
                adc #$01
                cmp #$08
                bcc _1

                inx
                sbc #$08

_1              iny
                bne _next3

; make HI/LO tables for Atari
; HIRES display

                lda #<HIRES1
                sta TEMP
                ldx #>HIRES1
                stx TEMP+1

                ldy #$00
_next4          cpy #$86
                bne _2

                inx
                lda #$00
_2              sta LO_,Y
                pha

                txa
                sta HI_,Y

                pla
                clc
                adc #$28
                bcc _3

                inx
_3              iny
                cpy #$C0
                bcc _next4

; make SLEEPLO, HI

                lda #<SLEEPERS
                sta TEMP
                lda #>SLEEPERS
                sta TEMP+1

                ldy #$00
                sty SLEEPCNT

_next5          sta SLEEPHI,Y

                lda TEMP
                sta SLEEPLO,Y

                clc
                adc #$16
                sta TEMP

                lda TEMP+1
                adc #$00
                sta TEMP+1

                iny
                cpy #$64
                bcc _next5

; now set up the game

                lda PBDATA
                sta OBJCOUNT

                ldy #$00
                sty RUNLEN

                jsr GETOBJ

_next6          lda OBJID
                cmp #<LIBOBJ
                bne _4

                ldx NEXTOBJ
                lda LBASE
                sta VECTLO,X

                lda LBASE+1
                sta VECTHI,X

                ldy #$08
                lda (LBASE),Y
                sta TIME,X

                lda #$00
                sta (LBASE),Y

                txa
                ldx RUNLEN
                sta RUNCHN,X

                inc RUNLEN

                jmp _5

_4              ldx NEXTOBJ
                lda #$00
                sta VECTLO,X
                sta VECTHI,X

_5              inc NEXTOBJ
                ldy NEXTOBJ
                jsr GETNEXTOBJ

                ldy NEXTOBJ
                cpy OBJCOUNT
                bne _next6

                ldy #$01
                lda OBJ
                ldx OBJ+1

_next7          clc
                adc PBDX-1,Y
                sta PBTBLO,Y
                bcc _6

                inx
_6              txa
                sta PBTBHI,Y

                lda PBTBLO,Y
                iny
                cpy #$C0
                bcc _next7

_next8          dey
                lda PBDX,Y
                beq _next8

                sty LASTY

; init scores

                ldy #$48
                ldx #$15
                lda #$00
                sta BMULT

                jsr CHARTO

                lda #<BMMSG
                ldx #>BMMSG
                jsr PRINT_
                jsr PRBMULT

                ldx #$03
_next9          stx PLAYER

                lda SCVERT,X
                sta CHAR+2

                lda SCTBLO,X
                tay
                lda SCTBHI,X
                tax

                jsr PRSCORE

                ldx PLAYER
                dex
                bpl _next9

                ldy #$58
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
PRSCORE         sty SCBASE
                stx SCBASE+1

_ENTRY1         ldy #$00
                jsr SETMODE

                ldy CHAR+2
                ldx #$25
                lda #$00
                jsr CHARTO

                ldy #$08
_next1          sty YTEMP

                lda (SCBASE),Y
                jsr PRCHAR

                dec CHAR+3
                stz CHAR+4

                ldy YTEMP
                dey
                bne _next1

                ldy #$02
                jmp SETMODE


;======================================
;
;======================================
SCORE           ldx PLAYER
                lda SCVERT,X
                sta CHAR+2

                ldy SCTBLO,X
                lda SCTBHI,X
                tax

                lda DSCORE
                jsr DOSCORE

                lda #$58
                sta CHAR+2

                lda #<BONUS
                sta SCBASE
                lda #>BONUS
                sta SCBASE+1

                lda DBONUS
                ldy #$05
                jsr DOSCORE._ENTRY1

                stz DSCORE
                stz DBONUS

                rts


;======================================
;
;======================================
DOSCORE         sty SCBASE
                stx SCBASE+1

                ldy #$07
_ENTRY1         clc
                adc (SCBASE),Y
                bcc _1

                lda #$FF
_1              sta (SCBASE),Y

_next1          ldx #$00
                lda (SCBASE),Y
_next2          cmp #$0A
                bcc _2

                sbc #$0A

                inx
                bcs _next2

_2              sta (SCBASE),Y

                txa
                dey
                adc (SCBASE),Y
                sta (SCBASE),Y

                tya
                bne _next1

                jmp PRSCORE._ENTRY1


;======================================
;
;======================================
PRBMULT         ldy #$00
                jsr SETMODE

                ldy #$48
                ldx #$1D
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


;======================================
;
;======================================
PRINTPLAYER     lda SCVERT,Y
                sta CHAR+2

                lda #$15
                sta CHAR+3
                stz CHAR+4

                lda PMSGLO,Y
                ldx PMSGHI,Y
                jmp PRINT_

;--------------------------------------

PMSGLO          .byte <P1MSG
                .byte <P2MSG
                .byte <P3MSG
                .byte <P4MSG
PMSGHI          .byte >P1MSG
                .byte >P2MSG
                .byte >P3MSG
                .byte >P4MSG

P1MSG           .byte $19,$15,$0A,$22,$0E,$1B,$81
P2MSG           .byte $19,$15,$0A,$22,$0E,$1B,$82
P3MSG           .byte $19,$15,$0A,$22,$0E,$1B,$83
P4MSG           .byte $19,$15,$0A,$22,$0E,$1B,$84

BMSG            .byte $0B,$18,$17,$1E,$9C
BMMSG           .byte $0B,$18,$17,$1E,$1C,$A1

SCVERT          .byte $78,$88,$98,$A8
SCTBLO          .byte $14,$1D,$26,$2F
SCTBHI          .byte $94,$94,$94,$94

SCORE1          .byte $00,$00,$00,$00,$00,$00,$00,$00,$00
SCORE2          .byte $00,$00,$00,$00,$00,$00,$00,$00,$00
SCORE3          .byte $00,$00,$00,$00,$00,$00,$00,$00,$00
SCORE4          .byte $00,$00,$00,$00,$00,$00,$00,$00,$00
BONUS           .byte $00,$00,$00,$00,$00,$00,$00,$00,$00


;======================================
;
;======================================
DOSOUND         cmp SERIES
                bmi _XIT

                sta SERIES
                stz SLICE

_XIT            rts


;======================================
; SOUND EFFECTS
;======================================
SOUND           lda SERIES
                bmi INITSOUND

                clc
                adc SLICE
                tax
                stx STEMP

                ldy EFFECTS,X
_next1          ldx NOTES-12,Y
                beq _1

                lda #$AF
                sta AUDC1
                stx AUDF1

_next2          pha
                pla

                dex
                bne _next2

                iny
                bne _next1

_1              inc SLICE

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


;======================================
;
;======================================
WIRING          ldx #$00
_next1          lda #$00
                sta TEMP
                stx XTEMP

                jsr GETST
                bpl _4

                inx
                jsr GETST
                bpl _4

                inx
                jsr GETST
                bpl _4

                inx
                stx XTEMP

                lda TEMP
                beq _4

                lda LOGIC,X
                sta YTEMP

                and #$0F
                tay

                lda BONUSTBL,Y
                clc
                adc DBONUS
                bcc _1

                lda #$FF
_1              sta DBONUS

                lda YTEMP
                bpl _2

                jsr PRBMULT

_2              lda YTEMP
                and #$70
                lsr
                lsr
                lsr
                lsr
                beq _3

                tax
                lda SOUNDTBL-1,X
                jsr DOSOUND

_3              dec XTEMP
                jsr TURNOFF
                dec XTEMP
                jsr TURNOFF
                dec XTEMP
                jsr TURNOFF

_4              lda XTEMP
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
SOUNDTBL        .byte $00,$04,$0C,$14
                .byte $24,$38,$4C


;======================================
;
;======================================
GETST           lda LOGIC,X
                beq _1

                inc TEMP

                tay
                lda VECTLO,Y
                sta BASE1
                lda VECTHI,Y
                sta BASE1+1

                ldy #$08
                lda (BASE1),Y

                rts

_1              lda #$80
_XIT            rts


;======================================
;
;======================================
TURNOFF         ldx XTEMP
                lda LOGIC,X
                beq GETST._XIT

                tay
                lda VECTLO,Y
                sta LBASE
                lda VECTHI,Y
                sta LBASE+1

                ldy #$0C
                lda (LBASE),Y
                sta _setAddr1+1
                iny
                lda (LBASE),Y
                sta _setAddr1+2

_setAddr1       jmp $FFFF               ; [smc]


;======================================
; DO A BALL
;======================================
DOBALL          lda #$AA
                sta CASINI

                stz INITMODE
                jsr INITOBJS

                ldx PLAYER
                jsr RESTOREPLAYER

_next1          stz LIVEBALLS

                jsr GETSELECT
                bne _1

                pla
                pla
                pla
                pla
                jsr CLOSEOBJS

                ldx PLAYER
                jmp SAVEPLAYER

_1              jsr PLAY

                lda LIVEBALLS
                bne _next1

                stz PTIMER1

_next2          jsr PLAY

                lda PTIMER1
                bne _next2

                ldx PLAYER
                jsr SAVEPLAYER

                lda #$80
                sta INITMODE
                jmp INITOBJS


;======================================
; PINBALL MAIN LOOP
;======================================
PLAY            inc PTIMER1
                bne _1

                inc PTIMER2

_1              lda PTIMER1
                and #$1F
                bne _2

                jsr DOCRSRY
                sta PDL0

_2              lda TRIG0
                eor #$FF
                ror
                ror
                sta BUTN0

                lda TRIG1
                eor #$FF
                ror
                ror
                sta BUTN1

                lda PORTA
                eor #$FF
                asl
                asl
                asl
                asl
                pha

                ora BUTN1
                sta BUTN1

                pla
                asl
                ora BUTN0
                sta BUTN0

_setValue1      lda #$10                ; [smc]
                jsr WAIT_

                ldy #$00
_next1          cpy RUNLEN
                bcs _7

                sty NEXTOBJ

                ldx RUNCHN,Y
                lda TIME,X
                and PTIMER1
                bne _6

                lda VECTLO,X
                sta LBASE
                lda VECTHI,X
                sta LBASE+1

                ldy #$0A
                lda (LBASE),Y
                sta _setAddr1+1
                iny
                lda (LBASE),Y
                sta _setAddr1+2

_setAddr1       jsr $FFFF               ; [smc]

                ldy NEXTOBJ
                ldx RUNCHN,Y
                lda TIME,X
                bne _6

                ldy #$10
                lda (LBASE),Y
                rol
                bcs _6
                bpl _3

                ldy SLEEPCNT
                cpy #$FF
                beq _6

                inc SLEEPCNT

                lda PLAYER
                sta SLEEPCODE,Y

                lda SLEEPLO,Y
                sta BASE1
                lda SLEEPHI,Y
                sta BASE1+1

                ldy #$16
_next2          lda (LBASE),Y
                sta (BASE1),Y

                dey
                bpl _next2

                jsr INITBALL
                jsr DRAWBALL
                jmp _4

_3              lda Y2_
                cmp LASTY
                beq _5

_4              lda #$FF
                sta LIVEBALLS
                bne _6

_5              lda #$80
                sta (LBASE),Y
                jsr DRAWBALL

_6              ldy NEXTOBJ
                iny
                bne _next1

_7              lda PTIMER1
                and #$1F
                bne _8

                jsr SCORE

_8              lda PTIMER1
                and #$07
                bne _9

                jsr SOUND

_9              lda PTIMER1
                and #$03
                bne _10

                jsr WIRING

_10             ldy #$00
_next3          cpy SLEEPCNT
                bcs _XIT

                sty NEXTOBJ

                lda SLEEPCODE,Y
                cmp PLAYER
                bne _13

                lda SLEEPLO,Y
                sta LBASE
                lda SLEEPHI,Y
                sta LBASE+1

                ldy #$0A
                lda (LBASE),Y
                sta _setAddr2+1
                iny
                lda (LBASE),Y
                sta _setAddr2+2

_setAddr2       jsr $FFFF               ; [smc]

                lda BSTAT
                rol
                bcs _11
                bmi _13

                lda Y2_
                cmp LASTY
                beq _11

                lda #$FF
                sta LIVEBALLS
                bne _13

_11             jsr DRAWBALL

                ldy NEXTOBJ
                cpy SLEEPCNT
                beq _12

                ldx SLEEPCNT
                lda SLEEPLO-1,X
                sta BASE1
                lda SLEEPHI-1,X
                sta BASE1+1

                lda SLEEPCODE-1,X
                sta SLEEPCODE,Y

                ldy #$16
_next4          lda (BASE1),Y
                sta (LBASE),Y

                dey
                bpl _next4

_12             dec SLEEPCNT

_13             ldy NEXTOBJ
                iny
                bne _next3

_XIT            rts


;======================================
;
;======================================
INITOBJS        ldy #$00
_next1          cpy RUNLEN
                beq PLAY._XIT

                sty NEXTOBJ

                ldx RUNCHN,Y
                lda VECTLO,X
                sta LBASE
                lda VECTHI,X
                sta LBASE+1

                ldy #$0C
                lda (LBASE),Y
                sta _setAddr1+1
                iny
                lda (LBASE),Y
                sta _setAddr1+2

_setAddr1       jsr $FFFF               ; [smc]

                ldy NEXTOBJ
                iny
                bne _next1


;======================================
;
;======================================
CLOSEOBJS       lda #$FF
                sta INITMODE
                jsr INITOBJS

_ENTRY1         ldy #$00
_next1          cpy RUNLEN
                beq PLAY._XIT

                sty NEXTOBJ

                ldx RUNCHN,Y
                lda VECTLO,X
                sta LBASE
                lda VECTHI,X
                sta LBASE+1

                lda TIME,X
                ldy #$08
                sta (LBASE),Y

                ldy NEXTOBJ
                iny
                bne _next1


;======================================
;
;======================================
GETOBJ          lda #<PBDATA+1
                sta OBJ
                lda #>PBDATA+1
                sta OBJ+1

                sty NEXTOBJ

                ldy #$00

                ;[fall-through]


;======================================
;
;======================================
GETNEXTOBJ      lda PBDATA,Y
                clc
                adc OBJ
                sta OBJ

                lda #$00
                adc OBJ+1
                sta OBJ+1

                cpy NEXTOBJ
                beq _1

                iny
                bne GETNEXTOBJ

_1              ldy #$00
                lda (OBJ),Y             ; GET OBJECT ID
                sta OBJID

; GET THE OBJECT'S L-BASE

                iny
                lda (OBJ),Y
                sta FILLCOLOR

                iny
                lda (OBJ),Y
                sta VRTXCOUNT

                lda VRTXCOUNT
                asl
                adc #$03
                adc OBJ
                sta LBASE

                lda OBJ+1
                adc #$00
                sta LBASE+1

                rts


;======================================
; SAVE PLAYER'S STATE
;======================================
SAVEPLAYER      jsr PBASES

                ldy #$00
_next1          cpy RUNLEN
                bcs _ENTRY1

                sty NEXTOBJ

                ldx RUNCHN,Y
                lda VECTLO,X
                sta LBASE
                lda VECTHI,X
                sta LBASE+1

                ldy #$00
                lda (LBASE),Y
                sta PARAM
                iny
                lda (LBASE),Y
                sta PARAM+1
                ldy #$08
                lda (LBASE),Y
                sta PARAM+2
                ldy #$10
                lda (LBASE),Y
                sta PARAM+3

                ldy NEXTOBJ
                lda PARAM
                sta (PBASE1),Y
                lda PARAM+1
                sta (PBASE2),Y
                lda PARAM+2
                sta (PBASE3),Y
                lda PARAM+3
                sta (PBASE4),Y

                iny
                bne _next1

_ENTRY1         ldy #$00
_next2          cpy SLEEPCNT
                bcs _XIT

                sty NEXTOBJ

                lda SLEEPCODE,Y
                cmp PLAYER
                bne _1

                lda SLEEPLO,Y
                sta LBASE
                lda SLEEPHI,Y
                sta LBASE+1

                jsr DRAWBALL

_1              ldy NEXTOBJ
                iny
                bne _next2

_XIT            rts


;======================================
; RESTORE A PLAYER
;======================================
RESTOREPLAYER   jsr PBASES

                lda #$80
                sta BUTN0
                sta BUTN1

                ldy #$00
_next1          cpy RUNLEN
                bcs SAVEPLAYER._ENTRY1

                sty NEXTOBJ

                ldx RUNCHN,Y
                lda VECTLO,X
                sta LBASE
                lda VECTHI,X
                sta LBASE+1

                lda TIME,X
                sta STEMP

                lda (PBASE1),Y
                sta PARAM
                lda (PBASE2),Y
                sta PARAM+1
                lda (PBASE3),Y
                sta PARAM+2
                lda (PBASE4),Y
                sta PARAM+3

_next2          ldy #$00
                lda (LBASE),Y
                cmp PARAM
                bne _1

                iny
                lda (LBASE),Y
                cmp PARAM+1
                beq _3

_1              lda STEMP
                cmp #$03
                bne _2

                ldy #$0A
                lda (LBASE),Y
                sta _setAddr1+1
                iny
                lda (LBASE),Y
                sta _setAddr1+2

_setAddr1       jsr $FFFF               ; [smc]
                jmp _next2

_2              jsr ADVANCE
                jmp _next2

_3              lda PARAM+2
                ldx STEMP
                cpx #$1F
                beq _4

                ldy #$08
                sta (LBASE),Y

_4              lda PARAM+3
                ldx STEMP
                beq _6

                cpx #$1F
                bne _5

                ora PARAM+2
_5              ldy #$10
                sta (LBASE),Y

_6              ldy NEXTOBJ
                iny
                bne _next1


;======================================
;
;======================================
PBASES          stz PBASE1
                stz PBASE3

                lda #$80
                sta PBASE2
                sta PBASE4

                lda PLTBHI,X
                sta PBASE1+1
                sta PBASE2+1

                clc
                adc #$01
                sta PBASE3+1
                sta PBASE4+1

                rts

;--------------------------------------

PLTBHI          .byte >P1STATE
                .byte >P2STATE
                .byte >P3STATE
                .byte >P4STATE


;======================================
;
;======================================
INITWORLD       ldx WSET
                lda GRAVTBL,X
                sta WMOD4

                ldx WSET+1
                lda TIMETBL,X
                sta PLAY._setValue1+1

                ldx WSET+2
                lda KICKTBL,X
                sta WMOD3

                ldx WSET+3
                lda ELASTLO,X
                sta WMOD2
                lda ELASTHI,X
                sta WMOD2+1

                rts

;--------------------------------------

GRAVTBL         .byte $FF,$7F,$3F,$1F,$0F,$07,$03,$01
TIMETBL         .byte $30,$20,$18,$10,$0C,$08,$04,$01
KICKTBL         .byte $04,$08,$0C,$10,$18,$20,$28,$38
ELASTLO         .byte $80,$40,$00,$00,$C0,$80,$40,$00
ELASTHI         .byte $83,$83,$83,$83,$82,$82,$82,$82


;======================================
;
;======================================
GOATARI         lda #<DLIST
                sta SDLSTL
                lda #>DLIST
                sta SDLSTH

                lda #$0F
                sta COLOR1
                sta COLPF1

                stz COLOR2
                stz COLPF2

                lda #$44
                sta COLOR4
                sta COLBK

                rts


;======================================
;
;======================================
MAKEPATCH       lda #$80
                sta BASE1
                lda #$24
                sta BASE1+1
                lda #$B5
                sta BASE2
                lda #$99
                sta BASE2+1

                ldy #$00
_next1          lda (BASE2),Y
                sta (BASE1),Y

                iny
                bne _next1

                inc BASE1+1
                inc BASE2+1

                lda BASE1+1
                cmp #$2B
                bne _next1

                rts


;--------------------------------------
;--------------------------------------

PATCH           .byte $FF
                .fill 74,$00
