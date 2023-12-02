
; SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
; SPDX-PackageOriginator: BudgeCo: Bill Budge
; SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
; SPDX-License-Identifier: MIT

; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


POLYGON         = 1
BPOLYGON        = 2
LIBOBJ          = 3


;--------------------------------------
; Zero-page equates
;--------------------------------------

CY_                     = $0082
CXD8                    = $0083
CXM8                    = $0084
LASTITEM                = $008C
OBJ                     = $008E         ; [word]
NEXTOBJ                 = $0090
OBJCOUNT                = $0091
PLYPTRX                 = $0092         ; [word]
PLYPTRY                 = $0094         ; [word]
OBJID                   = $0096
FILLCOLOR               = $0097
VRTXCOUNT               = $0098
LB_                     = $0099
SCANMODE                = $009B
MEMBTM                  = $009C         ; [word]
MIDBTM                  = $009E         ; [word]
MIDTOP                  = $00A0         ; [word]
MIDY                    = $00A2
COLOR                   = $00A3
CX_                     = $00A4
EDITMODE                = $00A5
OBJ2                    = $00A6
MINX                    = $00AA
MINY                    = $00AB
LFTSTOP                 = $00AC
MINPT                   = $00AC
RTSTOP                  = $00AD
MINDIST                 = $00AD
TOPSTOP                 = $00AE
MINPOLY                 = $00AE
BTMSTOP                 = $00AF
DELX                    = $00B0
DELY                    = $00B1
DRAGX                   = $00B2
DRAGY                   = $00B3
PARAM                   = $00C0         ; [6-bytes]
TEMP                    = $00C7
XTEMP                   = $00C8
YTEMP                   = $00C9
B1_                     = $00D0
VERTA                   = $00D4
D8A                     = $00D5
M8A                     = $00D6
VERTB                   = $00D7
D8B                     = $00DA
M8B                     = $00DB
BYTE2                   = $00DC

COLBW                   = $00F0
SLB                     = $00F0
GRIDON                  = $00F1
MCOLOR                  = $00F2
SLOLD                   = $00F2
XDIR                    = $00F3
SLNEW                   = $00F3
YDIR                    = $00F4

VR                      = $00F5
VRY                     = $00F5
VRXD8                   = $00F6
VRXM8                   = $00F7



;--------------------------------------
; Code equates
;--------------------------------------

D8_                     = $2100
M8_                     = $2200
LO_                     = $2300
HI_                     = $23C0

SETMODE                 = $24CF
DRAWB                   = $24F4
XOFFDRAW                = $2535
FRAMER                  = $265A
DRAWR                   = $2682
INR                     = $2692
GETB                    = $26BF
INITC                   = $26CA
XDRAWC                  = $26DE
UPDATEC                 = $26E5
DOCX                    = $2703
GETCX                   = $271A
DOCY                    = $272A
WAIT_                   = $272D
CINR                    = $2739
DOMENU                  = $274C
INIT                    = $27E0
MOVEUP                  = $2815
MOVEDOWN                = $2850
ADDIYX                  = $2889
SUBIYX                  = $28A7
CHARTO                  = $28D8
PRINT_                  = $291A

SWAPWIRE                = $4A00
SWAPDISK                = $4A08

LOGIC                   = $4B00
WSET                    = $4B18
PBDATA                  = $4B1C
OBJDX                   = $4B1D

PBDX                    = $7A40

PLAYSTART               = $9355
POLY                    = $94CA
LAUNCHER                = $94D5
LEFTFLIPPER             = $94F0
RIGHTFLIPPER            = $950B
BALL                    = $9526

BMP1                    = $9541
BMP2                    = $9564
BMP3                    = $9587
BMP4                    = $95A2
BMP5                    = $95BD
BMP6                    = $95D8

LKICK                   = $95F3
RKICK                   = $9610
KICK1                   = $962D
KICK2                   = $9648

ROLL1                   = $9663
ROLL2                   = $967E
ROLL3                   = $9699

TARG1                   = $96B4
TARG2                   = $96CF
TARG3                   = $96EA
TARG4                   = $9705
TARG5                   = $9720
TARG6                   = $973B

LFLIP2                  = $9756
RFLIP2                  = $9771

POLY1                   = $978C
POLY2                   = $9797
POLY3                   = $97A2
POLY4                   = $97AD

LANE1                   = $97B8
LANE2                   = $97D3
LANE3                   = $97EE

GATE1                   = $9809
GATE2                   = $9824
GATE3                   = $983F
GATE4                   = $985A

DROP1                   = $9875
DROP2                   = $9891
CATCH1                  = $98AD
CATCH2                  = $98CA

SPIN                    = $98E5
MGNT                    = $9900

DRAWDISPLAY             = $9920
GETOBJ                  = $995E
GETNEXTOBJ              = $996A
GETINFO                 = $9985
DRAWOBJ                 = $99C3

ALIGNPOLY               = $9E0A
MAKEHOLE                = $9E9A
SELECTPOLY              = $9EE4
POLYPOINTS              = $9F17
REMOVEPOLY              = $9F4E
GETBOUNDS               = $9FBC


;--------------------------------------
;--------------------------------------
                * = $A000
;--------------------------------------

;--------------------------------------
;
;--------------------------------------
START           jsr INIT

                lda #$00
                sta CY_
                sta CXD8
                sta CXM8
                sta SCANMODE

                jsr DRAWDISPLAY
                jsr SAVELOGO


;--------------------------------------
;
;--------------------------------------
DRAWKIT         jsr CLEARKIT

                lda #$FF
                sta COLOR

                ldy #$00
                sty EDITMODE

_next1          sty YTEMP
                lda ICONS,Y
                ldx ICONS+1,Y

                jsr XOFFDRAW

                ldy YTEMP
                iny
                iny
                cpy #$68
                bne _next1

                lda #$80
                sta SCANMODE

                ldy #$00
_next2          sty NEXTOBJ

                lda POLYS,Y
                sta OBJ
                lda POLYS+1,Y
                sta OBJ+1

                jsr GETINFO
                jsr DRAWOBJ

                ldy NEXTOBJ
                iny
                iny
                cpy #$08
                bne _next2

                lda #<HAND
                ldx #>HAND
                jsr INITC


;--------------------------------------
;
;--------------------------------------
MAIN            jsr UPDATEC
                jsr GETB
                bpl MAIN

                lda #<TABLEB
                ldx #>TABLEB
                jsr CINR
                bcc MAIN2

                jsr MODE0
                jmp MAIN


;======================================
;
;======================================
MODE0           lda EDITMODE
                bne _1

                jmp DRAGOBJ

_1              cmp #$80
                bne _2
                jmp DRAGPOINT

_2              cmp #$81
                bne _3
                jmp CUTPOINT

_3              cmp #$82
                bne _XIT
                jmp PASTEPOINT

_XIT            jmp PAINTOBJ


;--------------------------------------
;
;--------------------------------------
MAIN2           lda #<TOOLB
                ldx #>TOOLB
                jsr CINR
                bcc _1

                lda #$00
                sta LASTITEM+1

                jmp _3

_1              lda EDITMODE
                bne _XIT1

                ldy #$00
_next1          lda BOXLO,Y
                ldx BOXHI,Y
                sty YTEMP

                jsr CINR

                ldy YTEMP
                bcc _2

                lda OBJADDRHI,Y
                sta TEMP

                lda OBJLEN,Y
                ldx OBJADDRLO,Y
                ldy TEMP
                jsr ADDOBJ
                jmp MAIN

_2              iny
                cpy #$2B
                bcc _next1

_XIT1           jmp MAIN

_3              lda #<CMDMENU
                ldx #>CMDMENU
                jsr DOMENU

                jmp MAIN

;--------------------------------------

HAND            .word $A0DF
                .byte $02,$22,$04,$0C,$02
                .byte $C0,$00,$60,$00,$30,$00,$18,$60
                .byte $2C,$C0,$3E,$C0,$7F,$C0,$7F,$C0
                .byte $3F,$A0,$1F,$60,$0E,$C0,$01,$80
POINTER         .word $A0FE
                .byte $11,$22,$06,$07,$01
                .byte $F8,$F0,$F0,$F8,$9C,$0E,$06
SCISSOR         .word $A10C
                .byte $1B,$22,$05,$0B,$02
                .byte $08,$00,$8C,$00,$CC,$00,$6C,$00
                .byte $34,$00,$18,$00,$0D,$80,$32,$40
                .byte $4A,$40,$49,$80,$30,$00
HAMMER          .word $A129
                .byte $29,$22,$05,$0B,$02
                .byte $DE,$00,$DB,$00,$01,$00,$18,$00
                .byte $18,$00,$18,$00,$18,$00,$18,$00
                .byte $18,$00,$18,$00,$18,$00
BRUSH           .word $A146
                .byte $37,$22,$06,$08,$01
                .byte $FC,$3E,$1E,$00,$0C,$0C,$0C,$0C


;--------------------------------------
;
;--------------------------------------
INITHAND        jsr POINTSOFF

                lda #<HAND
                ldx #>HAND

                ldy #$00

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
SETEDMODE       sty EDITMODE
                jmp INITC


;--------------------------------------
;
;--------------------------------------
INITPOINTER     jsr POINTSON

                lda #<POINTER
                ldx #>POINTER

                ldy #$80
                bne SETEDMODE


;--------------------------------------
;
;--------------------------------------
INITSCISSOR     jsr POINTSON

                lda #<SCISSOR
                ldx #>SCISSOR

                ldy #$81
                bne SETEDMODE


;--------------------------------------
;
;--------------------------------------
INITHAMMER      jsr POINTSON

                lda #<HAMMER
                ldx #>HAMMER

                ldy #$82
                bne SETEDMODE


;--------------------------------------
;
;--------------------------------------
INITBRUSH       jsr POINTSOFF

                lda #<BRUSH
                ldx #>BRUSH

                ldy #$01
                bne SETEDMODE


;======================================
;
;======================================
POINTSON        bit EDITMODE
                bmi POINTSOFF._ENTRY2
                bpl POINTSOFF._ENTRY1   ; [unc]


;======================================
;
;======================================
POINTSOFF       bit EDITMODE
                bpl _ENTRY2

_ENTRY1         jsr DRAWPOINTS

_ENTRY2         jmp XDRAWC


;======================================
;
;======================================
DRAWLOGO        lda #<LOGO
                ldx #>LOGO
                jmp DRAWB


;======================================
;
;======================================
SAVELOGO        ldy #$01
                jsr SETMODE

                lda #<LOGO
                ldx #>LOGO
                jmp DRAWB

;-------------------------------------

LOGO            .word $B900
                .byte $00,$14,$00,$40,$14


;--------------------------------------
;--------------------------------------
WHITE           ldy #$FF

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
SETCLR          sty COLOR

                rts


;--------------------------------------
;--------------------------------------
GREEN           ldy #$55
                bne SETCLR


;--------------------------------------
;--------------------------------------
VIOLET          ldy #$AA
                bne SETCLR


;======================================
;
;======================================
CLEARKIT        ldy #$03
                jsr SETMODE

                lda #<KITB
                ldx #>KITB
                jsr DRAWR

                ldy #$02
                jmp SETMODE


;--------------------------------------
;
;--------------------------------------
PLAY            jsr POINTSOFF
                jsr CLEARKIT
                jsr DRAWLOGO
                jsr PLAYSTART

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
REEDIT          jsr SAVELOGO
                jmp DRAWKIT


;--------------------------------------
;
;--------------------------------------
MAGPAINT        jsr POINTSOFF
                jsr CLEARKIT
                jsr DRAWLOGO
                jsr MAGSTART
                jmp REEDIT


;--------------------------------------
;
;--------------------------------------
SETWORLD        jsr POINTSOFF
                jsr CLEARKIT
                jsr WORLDSTART
                jmp DRAWKIT


;--------------------------------------
;
;--------------------------------------
WIREKIT         jsr POINTSOFF
                jsr CLEARKIT
                jsr SWAPWIRE
                jmp DRAWKIT


;--------------------------------------
;
;--------------------------------------
DISKIO          jsr POINTSOFF
                jsr CLEARKIT
                jsr DRAWLOGO
                jsr SWAPDISK
                jmp REEDIT


;--------------------------------------
;
;--------------------------------------
DRAGOBJ         jsr SELECTPOLY
                bcs _1

                rts

_1              ldx #$80
                stx SCANMODE            ; NO MERGE
                tay

                jsr GETOBJ
                jsr REMOVEPOLY

_ENTRY1         jsr XDRAWC
                jsr GETBOUNDS

                ldx PARAM+3
                lda D8_,X
                sta CXD8
                lda M8_,X
                sta CXM8

                jsr XDRAWC

                ldy #$00
                sty RTSTOP
                sty BTMSTOP

                lda #$FF
                sta LFTSTOP
                sta TOPSTOP

_next1          lda (PLYPTRX),Y
                cmp LFTSTOP
                bcs _2

                sta LFTSTOP
_2              cmp RTSTOP
                bcc _3

                sta RTSTOP

_3              lda (PLYPTRY),Y
                cmp TOPSTOP
                bcs _4

                sta TOPSTOP
_4              cmp BTMSTOP
                bcc _5

                sta BTMSTOP

_5              iny
                cpy VRTXCOUNT
                bne _next1

                jsr GETCX
                sta DRAGX

                sec
                sbc LFTSTOP
                clc
                adc #$01
                sta LFTSTOP

                lda #$FF
                sec
                sbc RTSTOP
                clc
                adc DRAGX
                sec
                sbc #$01
                sta RTSTOP

                lda CY_
                sta DRAGY
                sec
                sbc TOPSTOP
                clc
                adc #$01
                sta TOPSTOP

                lda #$BF
                sec
                sbc BTMSTOP
                clc
                adc CY_
                sec
                sbc #$01
                sta BTMSTOP

_next2          jsr DRAWOBJ
                jsr GETCX

                cmp LFTSTOP
                bcs _6

                lda LFTSTOP
_6              cmp RTSTOP
                bcc _7

                lda RTSTOP
_7              tax
                sec
                sbc DRAGX
                cmp #$20
                bcc _8

                cmp #$E0
                bcc _11

_8              sta DELX
                stx DRAGX

                lda CY_
                cmp TOPSTOP
                bcs _9

                lda TOPSTOP
_9              cmp BTMSTOP
                bcc _10

                lda BTMSTOP
_10             tax
                sec
                sbc DRAGY
                sta DELY
                stx DRAGY

                ldy #$00
_next3          lda (PLYPTRX),Y
                clc
                adc DELX
                sta (PLYPTRX),Y

                lda (PLYPTRY),Y
                clc
                adc DELY
                sta (PLYPTRY),Y

                iny
                cpy VRTXCOUNT
                bne _next3

                lda OBJID
                cmp #<LIBOBJ
                bcc _11

                ldy #$02
                lda (LB_),Y
                clc
                adc DELY
                sta (LB_),Y

                iny
                lda (LB_),Y
                asl
                asl
                asl
                iny
                clc
                adc (LB_),Y
                clc
                adc DELX

                tax
                lda M8_,X
                sta (LB_),Y
                dey
                lda D8_,X
                sta (LB_),Y

_11             jsr GETB
                bpl _12

                jsr DRAWOBJ
                jsr UPDATEC
                jmp _next2

_12             lda #<TABLEB
                ldx #>TABLEB
                jsr CINR
                bcc DELETEOBJ

                lda SCANMODE
                and #$7F                ; MERGE
                sta SCANMODE

                jsr DRAWOBJ
                bcc _XIT

                jsr REMOVEPOLY
                jsr DRAWOBJ
                jsr REMOVEPOLY
                jmp DELETEOBJ

_XIT            rts


;--------------------------------------
;
;--------------------------------------
DELETEOBJ       ldy NEXTOBJ
                lda OBJDX,Y
                sta YTEMP

                tya
                clc
                adc #<OBJDX
                sta TEMP
                lda #$00
                adc #>OBJDX
                sta TEMP+1

                lda #$01
                ldy #<TEMP
                ldx #<OBJ2
                jsr ADDIYX

                lda #<OBJ
                ldx #<OBJ2
                ldy #<TEMP
                jsr MOVEDOWN

                lda YTEMP
                ldy #<OBJ
                ldx #<OBJ2
                jsr ADDIYX

                lda #$01
                ldy #<OBJ
                ldx #<OBJ
                jsr SUBIYX

                lda #$9E
                ldx #<OBJ2
                ldy #<OBJ
                jsr MOVEDOWN

                dec OBJCOUNT
                dec PBDATA

                ldx YTEMP
                inx
                txa
                sta TEMP

                ldy #<MEMBTM
                ldx #<MEMBTM
                jsr SUBIYX

                lda TEMP
                ldy #<MIDBTM
                ldx #<MIDBTM
                jsr SUBIYX

                ldy #$01
                jsr MAKEHOLE

                lda MIDTOP
                sta B1_
                lda MIDTOP+1
                sta B1_+1

                ldy #$01
_next1          lda (B1_),Y
                cmp NEXTOBJ
                bcc _1

                sbc #$01
                sta (B1_),Y

_1              ldx B1_+1
                lda B1_
                clc
                adc #$04
                sta B1_
                bcc _2

                inx
_2              stx B1_+1
                cmp #<PBDX
                bne _next1

                cpx #>PBDX
                bne _next1

                ldy #$00
_next2          jsr FIXINDX
                iny
                jsr FIXINDX
                iny
                jsr FIXINDX

                iny
                iny
                cpy #$18
                bcc _next2

                rts


;======================================
;
;======================================
FIXINDX         lda LOGIC,Y
                cmp NEXTOBJ
                bcc _XIT
                bne _1

                lda #$01
_1              sbc #$01
                sta LOGIC,Y

_XIT            rts


;======================================
;
;======================================
ADDOBJ          stx OBJ2
                sty OBJ2+1
                sta YTEMP               ; SIZE + 1

                ldx OBJCOUNT
                inx
                bmi _XIT1

                lda MIDTOP
                sec
                sbc MIDBTM
                tax

                lda MIDTOP+1
                sbc MIDBTM+1
                bne _1

                txa
                cmp #$20
                bcs _1

_XIT1           rts

_1              lda YTEMP
                ldy #<MIDBTM
                ldx #<TEMP
                jsr ADDIYX

                inc TEMP
                bne _2

                inc TEMP+1

_2              lda #<MIDBTM
                ldx #<MEMBTM
                ldy #<TEMP
                jsr MOVEUP

                lda TEMP
                sta MIDBTM
                lda TEMP+1
                sta MIDBTM+1

                ldy #$00
                jsr GETOBJ

                lda #$01
                ldy #<MEMBTM
                ldx #<TEMP
                jsr ADDIYX

                lda #<MEMBTM
                ldx #<OBJ
                ldy #<TEMP
                jsr MOVEUP

                inc MEMBTM
                bne _3

                inc MEMBTM+1

_3              ldy #$00
_next1          lda (OBJ2),Y
                sta (MEMBTM),Y

                iny
                cpy YTEMP
                bne _next1

                inc OBJCOUNT
                inc PBDATA

                ldy OBJCOUNT
                lda YTEMP
                sta PBDATA,Y

                ldx #<MEMBTM
                ldy #<MEMBTM
                jsr ADDIYX

                lda #$80
                sta SCANMODE

                ldy OBJCOUNT
                dey

                jsr GETOBJ
                jsr DRAWOBJ
                bcc _XIT

                jsr REMOVEPOLY
                jsr DRAWOBJ
                jmp DELETEOBJ

_XIT            jmp DRAGOBJ._ENTRY1


;--------------------------------------
;
;--------------------------------------
DRAGPOINT       jsr SELECTPOINT
                bcc _1

                rts

_1              jsr REMOVEPOLY

                lda #$80                ; NO MERGE
                ora SCANMODE
                sta SCANMODE
                lda FILLCOLOR
                beq _next1

                jsr POLYPOINTS

_next1          jsr DRAWOBJ

                ldy MINPT
                lda (PLYPTRX),Y
                sta DRAGX
                lda (PLYPTRY),Y
                sta DRAGY

                jsr GETCX
                bne _2

                lda #$01
_2              cmp #$9E                ; 22*7-2
                bcc _3

                lda #$9E
_3              sta (PLYPTRX),Y

                lda CY_
                bne _4

                lda #$01
_4              cmp #$BE                ; 192-2
                bcc _5

                lda #$BE
_5              sta (PLYPTRY),Y

_ENTRY1         jsr ALIGNPOLY
                php

                lda MINPT
                sec
                sbc YTEMP               ; ALIGN DISP
                bcs _6

                adc VRTXCOUNT
_6              sta MINPT

                plp
                bcc _7

                tay
                lda DRAGX
                sta (PLYPTRX),Y
                lda DRAGY
                sta (PLYPTRY),Y

                jsr ALIGNPOLY

_7              jsr GETB
                bpl ENDPTEDIT

                jsr DRAWOBJ
                bcs CUTPOINT._ENTRY1

                jsr UPDATEC
                jmp _next1


;--------------------------------------
;
;--------------------------------------
ENDPTEDIT       lda SCANMODE
                and #$7F                ; MERGE
                sta SCANMODE

                lda FILLCOLOR
                beq _1

                jsr POLYPOINTS

_1              jsr DRAWOBJ
                bcc CUTPOINT._XIT

                jsr REMOVEPOLY

                lda FILLCOLOR
                beq _2

                jsr POLYPOINTS

_2              jsr DRAWOBJ
                jsr REMOVEPOLY

                jmp CUTPOINT._ENTRY2


;--------------------------------------
;
;--------------------------------------
CUTPOINT        jsr SELECTPOINT
                bcc _1

_XIT            rts

_1              jsr REMOVEPOLY

                lda SCANMODE
                ora #$80
                sta SCANMODE

                lda FILLCOLOR
                beq _ENTRY1

                jsr POLYPOINTS

_ENTRY1         jsr DRAWOBJ

_ENTRY2         ldx VRTXCOUNT
                cpx #$04
                bcs _2

                lda NEXTOBJ
                beq ENDPTEDIT

                jmp DELETEOBJ

_2              dex
                txa
                ldy #$02
                sta (OBJ),Y

                lda MINPT               ; CHANGE POLY
                ldy #<PLYPTRX
                ldx #<PLYPTRX
                jsr ADDIYX

                lda #$01
                ldy #<PLYPTRX
                ldx #<TEMP
                jsr ADDIYX

                lda MINPT
                ldy #<PLYPTRY
                ldx #<PLYPTRY
                jsr ADDIYX

                lda #<PLYPTRY
                ldx #<TEMP
                ldy #<PLYPTRX
                jsr MOVEDOWN

                lda #$01
                ldy #<PLYPTRY
                ldx #<PLYPTRY
                jsr SUBIYX

                lda #$02
                ldy #<PLYPTRY
                ldx #<TEMP
                jsr ADDIYX

                lda #<MIDBTM
                ldx #<TEMP
                ldy #<PLYPTRY
                jsr MOVEDOWN

                lda #$02
                ldx #<MEMBTM
                ldy #<MEMBTM
                jsr SUBIYX

                lda #$02
                ldx #<MIDBTM
                ldy #<MIDBTM
                jsr SUBIYX

                ldy NEXTOBJ
                lda PBDATA+1,Y
                sec
                sbc #$02
                sta PBDATA+1,Y

                ldy NEXTOBJ
                jsr GETOBJ
                jsr ALIGNPOLY
                bcc _next1

                jmp DELETEOBJ

_next1          jsr GETB
                bmi _next1

                jmp ENDPTEDIT


;--------------------------------------
;
;--------------------------------------
PASTEPOINT      jsr SELECTPOINT
                bcc _1

                rts

_1              ldx VRTXCOUNT
                cpx #$3F
                bcs _XIT1

                lda MIDTOP
                sec
                sbc MIDBTM
                tax

                lda MIDTOP+1
                sbc MIDBTM+1
                bne _2

                txa
                cmp #$20
                bcs _2

_XIT1           rts

_2              jsr REMOVEPOLY

                lda #$80                ; ODDS, NO MERGE
                ora SCANMODE
                sta SCANMODE

                lda FILLCOLOR
                beq _3

                jsr POLYPOINTS

_3              jsr DRAWOBJ

                ldx VRTXCOUNT
                inx

                txa
                ldy #$02
                sta (OBJ),Y

                lda MINPT
                ldy #<PLYPTRY
                ldx #<PLYPTRY
                jsr ADDIYX

                lda #$02
                ldy #<MIDBTM
                ldx #<TEMP
                jsr ADDIYX

                lda #<MIDBTM
                ldx #<PLYPTRY
                ldy #<TEMP
                jsr MOVEUP

                lda MINPT
                ldy #<PLYPTRX
                ldx #<PLYPTRX
                jsr ADDIYX

                lda #$01
                ldy #<PLYPTRY
                ldx #<TEMP
                jsr ADDIYX

                lda #<PLYPTRY
                ldx #<PLYPTRX
                ldy #<TEMP
                jsr MOVEUP

                ldy NEXTOBJ
                jsr GETOBJ

                ldy MINPT
                lda MINX
                sta (PLYPTRX),Y
                lda MINY
                sta (PLYPTRY),Y

                lda #$02
                ldx #<MEMBTM
                ldy #<MEMBTM
                jsr ADDIYX

                lda #$02
                ldx #<MIDBTM
                ldy #<MIDBTM
                jsr ADDIYX

                ldy NEXTOBJ
                lda PBDATA+1,Y
                clc
                adc #$02
                sta PBDATA+1,Y

                jmp DRAGPOINT._ENTRY1


;======================================
;
;======================================
DRAWPOINTS      ldy #$00
                jsr GETOBJ

_next1          lda OBJID
                cmp #<POLYGON
                bne _1

_next2          lda FILLCOLOR
                beq _XIT1

                jsr POLYPOINTS

_XIT1           jmp _2

_1              cmp #<BPOLYGON
                beq _next2

_2              inc NEXTOBJ
                ldy NEXTOBJ
                jsr GETNEXTOBJ

                ldy NEXTOBJ
                cpy OBJCOUNT
                bne _next1

                rts


;======================================
;
;======================================
SELECTPOINT     jsr GETCX
                sta CX_

                lda #$FF
                sta MINDIST

                ldy #$00
                jsr GETOBJ

_next1          lda OBJID
                cmp #<POLYGON
                beq _1

                cmp #<BPOLYGON
                beq _1
                jmp _7

_1              ldy #$00
_next2          lda (PLYPTRX),Y
                sta XTEMP
                lda (PLYPTRY),Y
                sta YTEMP

                lda EDITMODE
                cmp #$82                ; PASTE POINT MODE?
                bne _3

                sty TEMP
                dey
                bpl _2

                ldy VRTXCOUNT
                dey

_2              lda (PLYPTRX),Y
                clc
                adc XTEMP
                ror
                sta XTEMP

                lda (PLYPTRY),Y
                clc
                adc YTEMP
                ror
                sta YTEMP

                ldy TEMP
_3              lda XTEMP
                sec
                sbc CX_
                bpl _4

                eor #$FF
                clc
                adc #$01
_4              cmp #$08
                bcs _6

                sta TEMP

                lda YTEMP
                sec
                sbc CY_
                bpl _5

                eor #$FF
                clc
                adc #$01
_5              cmp #$08
                bcs _6

                adc TEMP
                cmp MINDIST
                bcs _6

                sta MINDIST
                sty MINPT
                lda XTEMP
                sta MINX
                lda YTEMP
                sta MINY
                lda NEXTOBJ
                sta MINPOLY

_6              iny
                cpy VRTXCOUNT
                bne _next2

_7              inc NEXTOBJ
                ldy NEXTOBJ
                jsr GETNEXTOBJ

                ldy NEXTOBJ
                cpy OBJCOUNT
                beq _8

                jmp _next1

_8              lda MINDIST
                cmp #$FF
                bcc _9

                rts

_9              ldy MINPOLY
                sty NEXTOBJ
                jsr GETOBJ

                clc
                rts


;--------------------------------------
;
;--------------------------------------
PAINTOBJ        jsr XDRAWC
                jsr SELECTPOLY
                bcs _1

                lda #$00
_1              tay
                jsr GETOBJ

                lda OBJID
                cmp #<LIBOBJ
                beq _XIT

                lda SCANMODE
                ora #$80
                sta SCANMODE

                lda FILLCOLOR
                beq _2

                eor COLOR
                bne _3

_2              jsr POLYPOINTS

_3              lda COLOR
                cmp FILLCOLOR
                bne _4

                lda #$00                ; BLACK
_4              ldy #$01
                sta (OBJ),Y

                eor FILLCOLOR
                beq _next1

                sta FILLCOLOR
                jsr DRAWOBJ

_next1          jsr GETB
                bmi _next1

_XIT            jmp XDRAWC

;--------------------------------------

CMDMENU         .addr HANDB
                .addr INITHAND
                .addr POINTERB
                .addr INITPOINTER
                .addr SCISSORB
                .addr INITSCISSOR
                .addr HAMMERB
                .addr INITHAMMER
                .addr BRUSHB
                .addr INITBRUSH
                .addr WHITEB
                .addr WHITE
                .addr GREENB
                .addr GREEN
                .addr VIOLETB
                .addr VIOLET
                .addr PLAYB
                .addr PLAY
                .addr MAGNB
                .addr MAGPAINT
                .addr WORLDB
                .addr SETWORLD
                .addr WIREB
                .addr WIREKIT
                .addr DISKB
                .addr DISKIO
                .byte $00

BOXLO           .byte <POLYB,<LAUNCHERB,<LFLIPB,<RFLIPB
                .byte <BALLB,<BMP1B,<BMP2B,<BMP3B
                .byte <BMP4B,<BMP5B,<BMP6B,<LKICKB
                .byte <RKICKB,<KICK1B,<KICK2B,<ROLL1B
                .byte <ROLL2B,<ROLL3B,<TARG1B,<TARG2B
                .byte <TARG3B,<TARG4B,<TARG5B,<TARG6B
                .byte <LFLIP2B,<RFLIP2B,<POLY1B,<POLY2B
                .byte <POLY3B,<POLY4B,<LANE1B,<LANE2B
                .byte <LANE3B,<GATE1B,<GATE2B,<GATE3B
                .byte <GATE4B,<DROP1B,<DROP2B,<CATCH1B
                .byte <CATCH2B,<SPINB,<MGNTB

BOXHI           .byte >POLYB,>LAUNCHERB,>LFLIPB,>RFLIPB
                .byte >BALLB,>BMP1B,>BMP2B,>BMP3B
                .byte >BMP4B,>BMP5B,>BMP6B,>LKICKB
                .byte >RKICKB,>KICK1B,>KICK2B,>ROLL1B
                .byte >ROLL2B,>ROLL3B,>TARG1B,>TARG2B
                .byte >TARG3B,>TARG4B,>TARG5B,>TARG6B
                .byte >LFLIP2B,>RFLIP2B,>POLY1B,>POLY2B
                .byte >POLY3B,>POLY4B,>LANE1B,>LANE2B
                .byte >LANE3B,>GATE1B,>GATE2B,>GATE3B
                .byte >GATE4B,>DROP1B,>DROP2B,>CATCH1B
                .byte >CATCH2B,>SPINB,>MGNTB

OBJLEN          .byte $0B,$1B,$1B,$1B
                .byte $22,$23,$23,$1B
                .byte $1B,$1B,$1B,$1D
                .byte $1D,$1B,$1B,$1B
                .byte $1B,$1B,$1B,$1B
                .byte $1B,$1B,$1B,$1B
                .byte $1B,$1B,$0B,$0B
                .byte $0B,$0B,$1B,$1B
                .byte $1B,$1B,$1B,$1B
                .byte $1B,$1C,$1C,$1E
                .byte $1B,$1C,$1B

OBJADDRLO       .byte <POLY,<LAUNCHER,<LEFTFLIPPER,<RIGHTFLIPPER
                .byte <BALL,<BMP1,<BMP2,<BMP3
                .byte <BMP4,<BMP5,<BMP6,<LKICK
                .byte <RKICK,<KICK1,<KICK2,<ROLL1
                .byte <ROLL2,<ROLL3,<TARG1,<TARG2
                .byte <TARG3,<TARG4,<TARG5,<TARG6
                .byte <LFLIP2,<RFLIP2,<POLY1,<POLY2
                .byte <POLY3,<POLY4,<LANE1,<LANE2
                .byte <LANE3,<GATE1,<GATE2,<GATE3
                .byte <GATE4,<DROP1,<DROP2,<CATCH1
                .byte <CATCH2,<SPIN,<MGNT

OBJADDRHI       .byte >POLY,>LAUNCHER,>LEFTFLIPPER,>RIGHTFLIPPER
                .byte >BALL,>BMP1,>BMP2,>BMP3
                .byte >BMP4,>BMP5,>BMP6,>LKICK
                .byte >RKICK,>KICK1,>KICK2,>ROLL1
                .byte >ROLL2,>ROLL3,>TARG1,>TARG2
                .byte >TARG3,>TARG4,>TARG5,>TARG6
                .byte >LFLIP2,>RFLIP2,>POLY1,>POLY2
                .byte >POLY3,>POLY4,>LANE1,>LANE2
                .byte >LANE3,>GATE1,>GATE2,>GATE3
                .byte >GATE4,>DROP1,>DROP2,>CATCH1
                .byte >CATCH2,>SPIN,>MGNT

TABLEB          .byte $00,$00,$00,$BF,$13,$07
KITB            .byte $00,$14,$00,$BF,$13,$07
TOOLB           .byte $00,$21,$04,$BF,$03,$03
HANDB           .byte $00,$21,$04,$0E,$03,$03
POINTERB        .byte $0F,$21,$04,$09,$03,$03
SCISSORB        .byte $19,$21,$04,$0D,$03,$03
HAMMERB         .byte $27,$21,$04,$0D,$03,$03
BRUSHB          .byte $35,$21,$04,$0A,$03,$03
WHITEB          .byte $40,$21,$04,$09,$03,$03
GREENB          .byte $4A,$21,$04,$09,$03,$03
VIOLETB         .byte $54,$21,$04,$09,$03,$03
PLAYB           .byte $69,$21,$04,$0C,$03,$03
MAGNB           .byte $77,$21,$04,$0C,$03,$03
WORLDB          .byte $85,$21,$04,$0C,$03,$03
WIREB           .byte $93,$21,$04,$0C,$03,$03
DISKB           .byte $A1,$21,$04,$0D,$03,$03
POLYB           .byte $1B,$14,$06,$10,$02,$00
LAUNCHERB       .byte $49,$1D,$06,$0C,$00,$06
LFLIPB          .byte $02,$19,$02,$10,$02,$02
RFLIPB          .byte $02,$1B,$02,$10,$02,$02
BALLB           .byte $05,$1E,$05,$05,$00,$05
BMP1B           .byte $30,$14,$06,$11,$02,$01
BMP2B           .byte $31,$17,$00,$0A,$01,$05
BMP3B           .byte $30,$18,$07,$10,$00,$04
BMP4B           .byte $35,$19,$05,$05,$02,$02
BMP5B           .byte $30,$1C,$02,$0D,$01,$04
BMP6B           .byte $30,$1E,$00,$0D,$01,$04
LKICKB          .byte $44,$14,$06,$1A,$02,$02
RKICKB          .byte $44,$17,$04,$1A,$02,$02
KICK1B          .byte $4A,$1A,$05,$0D,$00,$05
KICK2B          .byte $4E,$1B,$06,$06,$01,$03
ROLL1B          .byte $A0,$15,$01,$05,$00,$04
ROLL2B          .byte $A0,$16,$00,$05,$00,$04
ROLL3B          .byte $A0,$16,$07,$05,$00,$04
TARG1B          .byte $A0,$18,$00,$03,$00,$06
TARG2B          .byte $A0,$19,$03,$03,$00,$06
TARG3B          .byte $A0,$1A,$06,$03,$00,$06
TARG4B          .byte $A0,$1C,$03,$07,$00,$02
TARG5B          .byte $A0,$1D,$02,$07,$00,$02
TARG6B          .byte $A0,$1E,$01,$07,$00,$02
LFLIP2B         .byte $05,$14,$06,$0A,$01,$04
RFLIP2B         .byte $05,$16,$03,$0A,$01,$04
POLY1B          .byte $1B,$18,$06,$12,$00,$01
POLY2B          .byte $20,$19,$04,$0D,$00,$01
POLY3B          .byte $25,$1A,$02,$0A,$04,$01
POLY4B          .byte $16,$1A,$02,$0A,$04,$01
LANE1B          .byte $90,$15,$01,$0A,$00,$04
LANE2B          .byte $90,$16,$03,$08,$00,$04
LANE3B          .byte $90,$17,$05,$05,$00,$04
GATE1B          .byte $90,$19,$01,$09,$00,$06
GATE2B          .byte $90,$1A,$07,$09,$00,$06
GATE3B          .byte $90,$1C,$05,$09,$00,$06
GATE4B          .byte $90,$1E,$03,$09,$00,$06
DROP1B          .byte $64,$16,$06,$04,$04,$00
DROP2B          .byte $64,$15,$00,$20,$00,$05
CATCH1B         .byte $6C,$17,$02,$13,$01,$02
CATCH2B         .byte $70,$19,$03,$09,$01,$02
SPINB           .byte $64,$1C,$06,$05,$00,$06
MGNTB           .byte $74,$1C,$04,$07,$01,$01

ICONS           .addr HAND
                .addr POINTER
                .addr SCISSOR
                .addr HAMMER
                .addr BRUSH
                .addr WHITEPAINT
                .addr GREENPAINT
                .addr VIOLETPAINT
                .addr PLAYICON
                .addr MAGNIFIER
                .addr WORLD
                .addr ANDG
                .addr DISK
                .addr POLYICON
                .addr LAUNCHER+$B
                .addr LEFTFLIPPER+$B
                .addr RIGHTFLIPPER+$B
                .addr BALL+$B
                .addr BMP1+$13
                .addr BMP2+$13
                .addr BMP3+$B
                .addr BMP4+$B
                .addr BMP5+$B
                .addr BMP6+$B
                .addr LKICK+$D
                .addr RKICK+$D
                .addr KICK1+$B
                .addr KICK2+$B
                .addr ROLL1+$B
                .addr ROLL2+$B
                .addr ROLL3+$B
                .addr TARG1+$B
                .addr TARG2+$B
                .addr TARG3+$B
                .addr TARG4+$B
                .addr TARG5+$B
                .addr TARG6+$B
                .addr LFLIP2+$B
                .addr RFLIP2+$B
                .addr LANE1+$B
                .addr LANE2+$B
                .addr LANE3+$B
                .addr GATE1+$B
                .addr GATE2+$B
                .addr GATE3+$B
                .addr GATE4+$B
                .addr DROP1+$B
                .addr DROP2+$B
                .addr CATCH1+$D
                .addr CATCH2+$B
                .addr SPIN+$B
                .addr MGNT+$B

POLYS           .addr POLY1
                .addr POLY2
                .addr POLY3
                .addr POLY4

WHITEPAINT      .word WHITEPAINT+7
                .byte $42,$22,$04,$07,$02
                .byte $FF,$C0,$FF,$C0,$00,$00,$FF,$C0,$FF,$C0,$FF,$C0,$FF,$C0

GREENPAINT      .word GREENPAINT+7
                .byte $4C,$22,$04,$07,$02
                .byte $FF,$C0,$FF,$C0,$00,$00,$55,$40,$55,$40,$55,$40,$55,$40

VIOLETPAINT     .word VIOLETPAINT+7
                .byte $56,$22,$04,$07,$02
                .byte $7F,$E0,$7F,$E0,$00,$00,$2A,$A0,$2A,$A0,$2A,$A0,$2A,$A0

PLAYICON        .word PLAYICON+7
                .byte $6B,$22,$03,$0A,$02
                .byte $78,$38,$FC,$7C,$FE,$7C,$FF,$7C,$7F,$B8,$1F,$C0,$07,$E0,$01,$F0
                .byte $00,$78,$00,$18
MAGNIFIER       .word MAGNIFIER+7
                .byte $79,$22,$03,$0A,$02
                .byte $1F,$00,$60,$C0,$C0,$60,$C0,$60,$C0,$60,$60,$C0,$1F,$80,$01,$C0
                .byte $00,$E0,$00,$60
WORLD           .word WORLD+7
                .byte $87,$22,$02,$0A,$02
                .byte $0F,$C0,$3A,$A0,$6A,$A8,$EA,$A8,$EA,$A8,$EA,$A8,$EA,$A8,$6A,$A8
                .byte $3A,$A0,$0F,$C0
ANDG            .word ANDG+7
                .byte $95,$22,$02,$0A,$03
                .byte $CF,$E0,$00,$CC,$38,$00,$0C,$0C,$00,$0C,$06,$00,$CC,$06,$30,$CC
                .byte $06,$30,$0C,$06,$00,$0C,$0C,$00,$CC,$38,$00,$CF,$E0,$00
DISK            .word DISK+7
                .byte $A3,$22,$02,$0B,$02
                .byte $FF,$F8,$80,$08,$87,$08,$8F,$88,$8F,$88,$8F,$88,$87,$08,$80,$08
                .byte $82,$08,$82,$08,$FF,$F8
POLYICON        .word POLYICON+7
                .byte $19,$14,$06,$10,$03
                .byte $E0,$00,$E0,$EF,$FE,$E0,$E0,$00,$E0,$00,$00,$00,$40,$00,$40,$40
                .byte $00,$40,$40,$00,$40,$40,$00,$40,$40,$00,$40,$40,$00,$40,$40,$00
                .byte $40,$40,$00,$40,$00,$00,$00,$E0,$00,$E0,$EF,$FE,$E0,$E0,$00,$E0


;======================================
;
;======================================
MAGSTART        lda #<ICON1
                ldx #>ICON1
                jsr XOFFDRAW
                jsr DRAWQUIT

                lda #<ICON2
                ldx #>ICON2
                jsr XOFFDRAW
                jsr INITMAG

                lda #<BRUSH
                ldx #>BRUSH
                jsr INITC

_next1          jsr UPDATEC
                jsr GETB
                bpl _next1

                lda #<VR
                ldx #>VR
                jsr CINR
                bcc _1

_next2          jsr DRAG
                jmp _next1

_1              lda #<MAGB
                ldx #>MAGB
                jsr CINR
                bcc _3

                lda #<MAG
                ldx #>MAG
                jsr CINR
                bcc _2

                jsr PLOT
                jmp _next1

_2              jsr SLIDEMAG
                jmp _next1

_3              lda #<MCMDB
                ldx #>MCMDB
                jsr CINR
                bcc _next2

                lda #$00
                sta LASTITEM+1
                jmp _4

_4              lda #<MCMDMENU
                ldx #>MCMDMENU
                jsr DOMENU

                jmp _next1

;--------------------------------------

MCMDMENU        .addr COLBWB
                .addr SWITCHCOLBW
                .addr GRIDB
                .addr GRIDTOGL
                .addr MQUITB
                .addr MQUIT
                .addr MWHTB
                .addr MWHITE
                .addr MGRNB
                .addr MGREEN
                .addr MVLTB
                .addr MVIOLET
                .byte $00


;======================================
;
;======================================
MQUIT           jsr DRAWVIEWR

                pla
                pla
                rts


;======================================
;
;======================================
DRAWQUIT        lda #$06
                ldx #$17
                ldy #$B2
                jsr CHARTO

                lda #<QUITMSG
                ldx #>QUITMSG
                jmp PRINT_

;--------------------------------------

QUITMSG         .byte $1A,$1E,$12,$9D


;=====================================
;=====================================
MWHITE          ldy #$FF

_ENTRY1         bit COLBW
                bmi _XIT

                sty MCOLOR

_XIT            rts

MGREEN          ldy #$55
                bne MWHITE._ENTRY1

MVIOLET         ldy #$AA
                bne MWHITE._ENTRY1


;======================================
;
;======================================
PLOT            jsr DRAWVIEWR

                lda MCOLOR
                sta COLOR
                jsr INSQR

                stx XTEMP
                sty YTEMP
                sta TEMP

                jsr INMAG
                bcs _4

                jsr HPLOT
                bcc _1

                lda MCOLOR
                bcs _2

_1              lda #$00
_2              sta COLOR

                ldx XTEMP
                ldy YTEMP
                lda TEMP
                jsr HPLOT
                jmp _3

_next1          jsr INSQR
                bcc _4

                stx XTEMP
                sty YTEMP
                sta TEMP

                jsr INMAG
                bcs _4

                jsr HPLOT
                bcc _4

_3              jsr DISPLAYPLOT

_4              jsr UPDATEC
                jsr GETB
                bmi _next1

                jmp DRAWVIEWR


;======================================
;
;======================================
DISPLAYPLOT     jsr XDRAWC

                lda CY_
                sec
                sbc #$47

                ldy #$00
_next1          cmp #$07
                bcc _1

                sbc #$07

                iny
                bne _next1

_1              tya
                tax
                clc
                adc VRY
                sta VERTA

                lda MUL7,X
                clc
                adc #$47
                sta VERTB

                lda VRXD8
                sta D8A
                lda VRXM8
                sta M8A

                jsr DOROW
                jmp XDRAWC


;======================================
;
;======================================
INMAG           stx PARAM+3
                sta PARAM+4
                sty PARAM+5

                lda #<MCMDB
                ldx #>MCMDB
                jsr INR

                lda TEMP
                ldx XTEMP
                ldy YTEMP

                rts


;======================================
;
;======================================
HPLOT           lda LO_,Y
                sta B1_
                lda HI_,Y
                sta B1_+1

                bit COLBW
                bpl _2

                txa
                tay
                ldx TEMP
                lda COLOR
                eor (B1_),Y
                and MASK,X
                eor (B1_),Y

                cmp (B1_),Y
                sec
                bne _1

                clc
_1              sta (B1_),Y

                rts

_2              txa
                tay
                lda TEMP
                and #$FE
                tax

                lda COLOR
                eor (B1_),Y
                and CLRMASK,X
                eor (B1_),Y

                cmp (B1_),Y
                sec
                bne _3

                clc
_3              sta (B1_),Y

                rts

;--------------------------------------

MASK            .byte $80,$40,$20,$10,$08,$04,$02,$01
CLRMASK         .byte $C0,$00,$30,$00,$0C,$00,$03

MUL7            .byte $00,$07,$0E,$15,$1C,$23,$2A
                .byte $31,$38,$3F,$46,$4D,$54,$5B
                .byte $62,$69,$70,$77,$7E,$85,$8C
                .byte $93,$9A,$A1,$A8,$AF,$B6,$BD
                .byte $C4,$CB,$D2,$D9,$E0,$E7,$EE

MCMDB           .byte $40,$14,$00,$7F,$11,$07
COLBWB          .byte $B0,$14,$00,$0B,$01,$05
GRIDB           .byte $B0,$15,$06,$0B,$01,$05
MQUITB          .byte $B0,$17,$04,$0B,$03,$05
MWHTB           .byte $B0,$1B,$02,$0B,$01,$04
MGRNB           .byte $B0,$1C,$07,$0B,$01,$04
MVLTB           .byte $B0,$1E,$04,$0B,$01,$04

ICON1           .word $AD27
                .byte $B1,$14,$00,$0A,$04
                .byte $7F,$F9,$F7,$C0,$7E
                .byte $19,$F7,$C0,$7E,$19
                .byte $F7,$C0,$7E,$19,$F7
                .byte $C0,$7E,$18,$00,$00
                .byte $7E,$19,$F7,$C0,$7E
                .byte $19,$F7,$C0,$7E,$19
                .byte $F7,$C0,$7E,$19,$F7
                .byte $C0,$7F,$F8,$00,$00

ICON2           .word $AD56
                .byte $B1,$1B,$00,$0A,$05
                .byte $0F,$F8,$7F,$C3,$FE
                .byte $0F,$F8,$7F,$C3,$FE
                .byte $00,$00,$00,$00,$00
                .byte $0F,$F8,$55,$42,$AA
                .byte $0F,$F8,$55,$42,$AA
                .byte $0F,$F8,$55,$42,$AA
                .byte $0F,$F8,$55,$42,$AA
                .byte $0F,$F8,$55,$42,$AA
                .byte $0F,$F8,$55,$42,$AA
                .byte $0F,$F8,$55,$42,$AA


;======================================
;
;======================================
INITMAG         ldy #$04
                jsr SETMODE

                lda #<MBAR1
                ldx #>MBAR1
                jsr DRAWR

                lda #<MBAR2
                ldx #>MBAR2
                jsr DRAWR

                lda #<MBAR3
                ldx #>MBAR3
                jsr DRAWR

                lda #<MBAR4
                ldx #>MBAR4
                jsr DRAWR

                lda #$00
                sta COLBW
                sta GRIDON

                ldy #$FF
                sty MCOLOR

                lda #$00
                sta VRY
                sta VRXD8
                sta VRXM8

                lda #$0D
                sta VR+3
                lda #$01
                sta VR+4
                lda #$07
                sta VR+5

                jsr BLOWUP

                ldy #$02
                jsr SETMODE


;======================================
;
;======================================
DRAWVIEWR       lda #<VR
                ldx #>VR
                jmp FRAMER


;--------------------------------------
;
;--------------------------------------
SWITCHCOLBW     lda COLBW
                eor #$80
                sta COLBW

                ldy #$FF
                sty MCOLOR

_XIT            jmp MAGNIFY


;--------------------------------------
;
;--------------------------------------
GRIDTOGL        lda GRIDON
                eor #$80
                sta GRIDON
                bmi _1

                lda #$FF
                sta DOROW._setValue1+1
                sta DOROW._setValue2+1
                sta DOROW._setValue3+1

                lda #$55
                sta DOROW._setValue4+1

                lda #$AA
                sta DOROW._setValue5+1

                bne SWITCHCOLBW._XIT

_1              lda #$FE
                sta DOROW._setValue1+1
                sta DOROW._setValue2+1
                sta DOROW._setValue3+1

                lda #$54
                sta DOROW._setValue4+1

                lda #$A8
                sta DOROW._setValue5+1

                bne SWITCHCOLBW._XIT


;======================================
;
;======================================
INSQR           lda #<MAG
                ldx #>MAG
                jsr CINR

                php

                lda CY_
                sec
                sbc #$47

                ldy #$00
_next1          cmp #$07
                bcc _1

                sbc #$07

                iny
                bne _next1

_1              tya
                adc VRY
                tay

                lda CXD8
                sec
                sbc #$15
                clc
                adc VRXM8

                ldx VRXD8
_next2          cmp #$08
                bcc _XIT

                sbc #$08

                inx
                bcs _next2

_XIT            plp
                rts


;======================================
;
;======================================
SLIDEMAG        lda #$00
                sta XDIR
                sta YDIR

                lda #<MBAR1
                ldx #>MBAR1
                jsr CINR
                bcc _1

                ldy #$FE
                sty YDIR
                bne _2

_1              lda #<MBAR4
                ldx #>MBAR4
                jsr CINR
                bcc _2

                ldy #$02
                sty YDIR

_2              lda #<MBAR2
                ldx #>MBAR2
                jsr CINR
                bcc _3

                ldx #$FE
                stx XDIR
                bne _4

_3              lda #<MBAR3
                ldx #>MBAR3
                jsr CINR
                bcc _4

                ldx #$02
                stx XDIR

_4              jsr XDRAWC
                jsr SLIDEVIEWR
                jsr XDRAWC

                rts


;======================================
;
;======================================
SLIDEVIEWR      jsr DRAWVIEWR

                clc
                ldx VRXD8
                lda XDIR
                bmi _2

                adc VRXM8
                cmp #$08
                bcc _1

                sbc #$08
                inx
_1              cpx #$26
                bcc _4
                bne _5

                cmp #$01
                bcc _4
                bcs _5

_2              adc VRXM8
                bpl _3

                adc #$08
                dex
_3              bmi _5

_4              stx VRXD8
                sta VRXM8
_5              clc
                lda YDIR
                bmi _6

                adc VRY
                cmp #$B3
                bcs _8
                bcc _7

_6              adc VRY
                bcc _8

_7              sta VRY
_8              jsr BLOWUP
                jsr DRAWVIEWR

                lda #$32
                jsr WAIT_
                jsr GETB
                bpl _XIT

                jmp SLIDEVIEWR

_XIT            rts


;======================================
;
;======================================
DRAG            jsr XDRAWC

_next1          jsr DRAWVIEWR
                jsr DOCX

                stx CXD8
                sta CXM8
                jsr DOCY

                sta CY_
                cmp #$B2
                bcc _1

                lda #$B2
_1              sta VRY

                ldx CXD8
                lda CXM8
                and #$FE
                cpx #$26
                bcc _3
                bne _2

_2              ldx #$26
                lda #$00
_3              stx VRXD8
                sta VRXM8

                jsr BLOWUP
                jsr DRAWVIEWR

                lda #$50
                jsr WAIT_
                jsr GETB
                bmi _next1

                jsr XDRAWC

                rts


;--------------------------------------
;
;--------------------------------------
MAGNIFY         jsr DRAWVIEWR
                jsr BLOWUP
                jmp DRAWVIEWR


;======================================
;
;======================================
BLOWUP          lda #$47
                sta VERTB
                lda VRY
                sta VERTA

                lda VRXD8
                sta D8A
                lda VRXM8
                sta M8A

_next1          jsr DOROW

                inc VERTA

                lda VERTB
                clc
                adc #$07
                sta VERTB

                cmp #$A9
                bcc _next1

                rts


;======================================
;
;======================================
DOROW           ldy VERTA
                lda LO_,Y
                sta B1_
                lda HI_,Y
                sta B1_+1

                ldy VERTB
                ldx #$00
                bit COLBW
                bpl _next3

_next1          lda LO_,Y
                sta _setAddr1+1,X
                lda HI_,Y
                sta _setAddr1+2,X

                iny
                inx
                inx
                inx
                cpx #$12
                bcc _next1

                jsr INITROW

                ldx #$10
                stx M8B

                ldx #$15
_next2          php

                rol
                dec BYTE2
                bne _1

                iny
                lda #$08
                sta BYTE2

                lda (B1_),Y
_1              bcc _2

                plp
                pha
                sec
_setValue1      lda #$FF
                bne _setAddr1

_2              plp
                pha
                clc

                lda #$00
_setAddr1       sta $FFFF,X
                sta $FFFF,X
                sta $FFFF,X
                sta $FFFF,X
                sta $FFFF,X
                sta $FFFF,X

                inx
                pla

                dec M8B
                bne _next2

                rts

_next3          lda LO_,Y
                sta _setAddr2+1,X
                sta _setAddr3+1,X
                lda HI_,Y
                sta _setAddr2+2,X
                sta _setAddr3+2,X

                iny
                inx
                inx
                inx
                cpx #$12
                bcc _next3

                jsr INITROW

                ldx #$08
                stx M8B

                ldx #$15
_next4          php

                rol
                dec BYTE2
                bne _3

                iny
                lda #$08
                sta BYTE2

                lda (B1_),Y
_3              php

                rol
                dec BYTE2
                bne _4

                iny
                lda #$08
                sta BYTE2

                lda (B1_),Y
_4              bcc _8

                plp
                bcc _5

                plp
                pha

                sec
_setValue2      lda #$FF
                sta D8B

                lda #$FF
                bne _setAddr2

_5              plp
                pha
                rol

                dec BYTE2
                bne _6

                iny
                lda (B1_),Y
                rol

                dey
_6              inc BYTE2
                bcc _7

                sec
_setValue3      lda #$FF
                sta D8B

                lda #$00
                bcs _setAddr2

_7              sec
_setValue4      lda #$55
                sta D8B

                lda #$55
                bne _setAddr2

_8              plp
                bcc _10

                plp
                pha
                bcc _9

                clc
                lda #$00
                sta D8B

                lda #$FF
                bne _setAddr2

_9              clc
_setValue5      lda #$AA
                sta D8B

                lda #$AA
                bne _setAddr2

_10             plp
                pha

                clc
                lda #$00
                sta D8B

_setAddr2       sta $FFFF,X
                sta $FFFF,X
                sta $FFFF,X
                sta $FFFF,X
                sta $FFFF,X
                sta $FFFF,X
                inx

                lda D8B
_setAddr3       sta $FFFF,X
                sta $FFFF,X
                sta $FFFF,X
                sta $FFFF,X
                sta $FFFF,X
                sta $FFFF,X

                pla
                inx

                dec M8B
                beq _XIT

                jmp _next4

_XIT            rts


;======================================
;
;======================================
INITROW         ldy D8A
                ldx M8A
                bne _2

                clc
                dey
                bmi _1

                lda (B1_),Y
                ror

_1              iny
                lda #$08
                sta BYTE2

                lda (B1_),Y
                rts

_2              lda #$08
                sta BYTE2

                lda (B1_),Y
_next1          rol

                dec BYTE2

                dex
                bne _next1

                rts

;--------------------------------------

MAGB            .byte $40,$14,$00,$6F,$11,$07
MAG             .byte $47,$15,$00,$61,$0F,$07
MBAR1           .byte $40,$14,$00,$06,$11,$07
MBAR2           .byte $40,$14,$00,$6F,$00,$07
MBAR3           .byte $40,$25,$00,$6F,$00,$07
MBAR4           .byte $A9,$14,$00,$06,$11,$07


;======================================
;
;======================================
WORLDSTART      jsr DRAWQUIT

                ldy #$00
_next1          sty YTEMP

                lda SLIDESLO,Y
                ldx SLIDESHI,Y
                jsr XOFFDRAW

                ldy YTEMP
                jsr INITSLIDE

                iny
                cpy #$04
                bne _next1

                lda #$00
                ldx #$15
                ldy #$06
                jsr CHARTO

                lda #<HEAD1
                ldx #>HEAD1
                jsr PRINT_

                lda #$01
                ldx #$1D
                ldy #$06
                jsr CHARTO

                lda #<HEAD2
                ldx #>HEAD2
                jsr PRINT_

                lda #$01
                ldx #$16
                ldy #$46
                jsr CHARTO

                lda #<HEAD3
                ldx #>HEAD3
                jsr PRINT_

                lda #$01
                ldx #$1B
                ldy #$46
                jsr CHARTO

                lda #<HEAD4
                ldx #>HEAD4
                jsr PRINT_

                lda #<HAND
                ldx #>HAND
                jsr INITC

_next2          jsr UPDATEC
                jsr GETB
                bpl _next2

                ldy #$00
_next3          lda SLBLO,Y
                ldx SLBHI,Y
                sty YTEMP
                jsr CINR

                ldy YTEMP
                bcc _1

                jsr DOSLIDE
                jmp _next2

_1              iny
                cpy #$04
                bne _next3

                lda #<MCMDB
                ldx #>MCMDB
                jsr CINR
                bcc _next2

                lda #$00
                sta LASTITEM+1

                jmp _2

_2              lda #<WCMDMENU
                ldx #>WCMDMENU
                jsr DOMENU

                jmp _next2

;--------------------------------------

WCMDMENU        .word MQUITB
                .word MQUIT+3
                .byte $00


;======================================
;
;======================================
DOSLIDE         lda SLIDESLO,Y
                sta SLB
                lda SLIDESHI,Y
                sta SLB+1

_next1          lda WSET,Y
                sta SLOLD

                lda CY_
                sec
                ldy #$02
                sbc (SLB),Y
                bcs _1

                ldy #$00
                beq _2

_1              ldy #$07
_next2          cmp SLDXDY,Y
                bcs _2

                dey
                bne _next2

_2              sty SLNEW
                tya

                ldy YTEMP
                sta WSET,Y

                jsr MOVESLIDE
                jsr UPDATEC

                ldy YTEMP
                jsr GETB
                bmi _next1

                rts


;======================================
;
;======================================
INITSLIDE       lda SLIDESLO,Y
                sta SLB
                lda SLIDESHI,Y
                sta SLB+1

                lda #$00
                sta SLOLD

                lda WSET,Y
                sta SLNEW

                ;[fall-through]


;======================================
;
;======================================
MOVESLIDE       ldy #$02
                lda (SLB),Y
                sta TEMP

                iny
                lda (SLB),Y
                sta SLDX+3

                iny
                lda (SLB),Y
                sta SLDX+4

_next1          ldy SLOLD
                cpy SLNEW
                beq _3
                bcs _1

                inc SLOLD

                lda SLDXDY,Y
                bne _2

_1              dec SLOLD
                lda SLDXDY-1,Y
_2              clc
                adc TEMP
                sta SLDX+2

                lda #<SLDX
                ldx #>SLDX
                jsr XOFFDRAW

                jmp _next1

_3              ldy YTEMP
                rts

;--------------------------------------

SLDXDY          .byte $02,$05,$08,$0B,$0E,$11,$14,$17

SLDX            .word SLDXp7
                .byte $00,$00,$00,$06,$02
SLDXp7          .byte $0D,$80,$0D,$80,$0D,$80,$0D,$80
                .byte $0D,$80,$0D,$80

SLBLO           .byte <SL1B
                .byte <SL2B
                .byte <SL3B
                .byte <SL4B
SLBHI           .byte >SL1B
                .byte >SL2B
                .byte >SL3B
                .byte >SL4B

SL1B            .byte $10,$16,$06,$1C,$01,$05
SL2B            .byte $10,$1D,$06,$1C,$01,$05
SL3B            .byte $50,$16,$06,$1C,$01,$05
SL4B            .byte $50,$1D,$06,$1C,$01,$05

SLIDESLO        .byte <SLIDE1
                .byte <SLIDE2
                .byte <SLIDE3
                .byte <SLIDE4
SLIDESHI        .byte >SLIDE1
                .byte >SLIDE2
                .byte >SLIDE3
                .byte >SLIDE4

SLIDE1          .word SLIDEBITS
                .byte $10,$16,$06,$1C,$02
SLIDE2          .word SLIDEBITS
                .byte $10,$1D,$06,$1C,$02
SLIDE3          .word SLIDEBITS
                .byte $50,$16,$06,$1C,$02
SLIDE4          .word SLIDEBITS
                .byte $50,$1D,$06,$1C,$02

SLIDEBITS       .byte $FF,$F8,$C0,$18,$CF,$98,$EF,$B8,$CF,$98,$C2,$18,$E2,$38
                .byte $C2,$18,$C2,$18,$E2,$38,$C2,$18,$C2,$18,$E2,$38,$C2,$18
                .byte $C2,$18,$E2,$38,$C2,$18,$C2,$18,$E2,$38,$C2,$18,$C2,$18
                .byte $E2,$38,$C2,$18,$C2,$18,$E2,$38,$C2,$18,$C0,$18,$FF,$F8

HEAD1           .byte $10,$1B,$0A,$1F,$12,$1D,$A2
HEAD2           .byte $1C,$19,$0E,$0E,$8D
HEAD3           .byte $14,$12,$0C,$94
HEAD4           .byte $0E,$15,$0A,$1C,$1D,$12,$0C,$12,$1D,$A2


;--------------------------------------
;--------------------------------------

                .byte $FF,$30,$47,$38,$43,$30,$43,$30
                .byte $47,$38,$43,$30,$03,$30,$7F,$3F
                .byte $10,$1B,$0A,$1F,$12,$1D,$A2,$1D
                .byte $12,$16,$8E,$14,$12,$0C,$94,$0E
                .byte $15,$0A,$1C,$1D,$12,$0C,$12,$1D
                .byte $A2,$00,$1D,$12,$0C,$12,$1D,$A2
                .byte $FF,$1C,$01,$06,$50,$1A,$00,$1C
                .byte $01,$06,$50,$22,$00,$1C,$01,$06
                .byte $E1,$E8,$EF,$F6,$81,$81,$81,$81
                .byte $FD,$81,$10,$1A,$00,$1C,$02,$FD
                .byte $81,$10,$22,$00,$1C,$02,$FD,$81
                .byte $50,$1A,$00,$1C,$02,$FD,$81,$50
                .byte $22,$00,$1C,$00,$00,$00,$00
