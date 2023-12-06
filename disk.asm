
; SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
; SPDX-PackageOriginator: BudgeCo: Bill Budge
; SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
; SPDX-License-Identifier: MIT

; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


                .include "equates/system_atari8.equ"
                .include "equates/zeropage.equ"

                .include "equates/ppak.equ"

                .include "equates/cdraw.equ"
                .include "equates/run2.equ"
                .include "equates/swap.equ"


;--------------------------------------
; Zero-page equates (local)
;--------------------------------------

CHARINDX                = $00A3
CHARASCII               = $00A4
CHCODE                  = $00A5

BOXCURSORON             = $00A6

DTIMER1                 = $00A7
DTIMER2                 = $00A8

DISKCMD                 = $00A9
SAVETYPE                = $00AA
BSPARM                  = $00AB         ; [4-bytes]
STACKTEMP               = $00AF


;--------------------------------------
; Code equates
;--------------------------------------

FMINIT                  = $0788

CHAR                    = $2933

GAMEBTM                 = $2B00         ; <--

HIRES                   = $2B00
HTOP                    = $490F

PBBASE                  = $4B00
PBDATA                  = $4B1C

GAMESTART               = $9048
PLAYGAME                = $9062

GAMETOP                 = $A100         ; <--


            .enc "atari-inverse"
                .cdef " Z", $A0
            .enc "none"


;--------------------------------------
;--------------------------------------
                * = $A000
;--------------------------------------

PRESTART        tsx
                stx STACKTEMP

                jsr FMINIT

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
START           ldy #$02
                jsr SETMODE

                lda #$40
                sta SHFLOC

                ldy #$00
                lda #$4D
_next1          sty XTEMP
                sta CHAR+2

                lda #$15
                sta CHAR+3
                stz CHAR+4

                lda TXTLO,Y
                ldx TXTHI,Y

                jsr PRINT_

                lda CHAR+2
                clc
                adc #$0A

                ldy XTEMP
                cpy #$02
                bne _1

                adc #$0A

_1              cpy #$04
                bne _2

                adc #$0A

_2              iny
                cpy #$07
                bne _next1

                lda #<HAND
                ldx #>HAND
                jsr INITCRSR

_wait1          jsr UPDATECRSR
                jsr GETBUTNS
                bpl _wait1

                lda #<MENUBOX
                ldx #>MENUBOX
                jsr CRSRINRECT
                bcc _3

                stz LASTITEM+1

                lda #<DISKMENU
                ldx #>DISKMENU
                jsr DOMENU

_3              jmp _wait1


;--------------------------------------

HAND            .word $A072
                .byte $00,$00,$00,$0C,$02
                .byte $C0,$00,$60,$00,$30,$00,$18,$60
                .byte $2C,$C0,$3E,$C0,$7F,$C0,$7F,$C0
                .byte $3F,$A0,$1F,$60,$0E,$C0,$01,$80
                .byte $00

DISKMENU        .addr LOADBOX
                .addr LOAD
                .addr SAVEBOX
                .addr SAVE_
                .addr QUITBOX
                .addr QUIT
                .addr MAKEBOX
                .addr MAKE
                .addr PLAYBOX
                .addr PLAY
                .byte $00

TXTLO           .byte <LOADMSG
                .byte <SAVEMSG
                .byte <QUITMSG
                .byte <MAKEMSG
                .byte <GAMEMSG
                .byte <PLAYMSG
                .byte <GAMEMSG
TXTHI           .byte >LOADMSG
                .byte >SAVEMSG
                .byte >QUITMSG
                .byte >MAKEMSG
                .byte >GAMEMSG
                .byte >PLAYMSG
                .byte >GAMEMSG

LOADMSG         .byte $15,$18,$0A,$8D
SAVEMSG         .byte $1C,$0A,$1F,$8E
QUITMSG         .byte $1A,$1E,$12,$9D
MAKEMSG         .byte $16,$0A,$14,$8E
GAMEMSG         .byte $10,$0A,$16,$8E
PLAYMSG         .byte $19,$15,$0A,$A2
INSERTMSG       .byte $12,$17,$1C,$0E,$1B,$1D,$24,$22
                .byte $18,$1E,$1B,$24,$10,$0A,$16,$0E
                .byte $24,$0D,$12,$1C,$14,$24,$0A,$17
                .byte $0D,$24,$11,$12,$1D,$24,$0A,$24
                .byte $14,$0E,$A2

MENUBOX         .byte $40,$14,$00,$68,$0F,$06
LOADBOX         .byte $4B,$14,$04,$0A,$04,$00
SAVEBOX         .byte $55,$14,$04,$0A,$04,$00
QUITBOX         .byte $5F,$14,$04,$0A,$04,$00
MAKEBOX         .byte $73,$14,$04,$14,$04,$02
PLAYBOX         .byte $93,$14,$04,$14,$04,$02


;--------------------------------------
;
;--------------------------------------
LOAD            jsr GETNAME
                jsr INPROMPT1

                stz DISKCMD

                ldx #$10                ; filename
                lda #<CHARBUF
                sta IOCB0+ICBAL,X
                lda #>CHARBUF
                sta IOCB0+ICBAH,X

                lda #$03                ; OPEN
                sta IOCB0+ICCOM,X

                lda #$04                ; sector
                sta IOCB0+ICAX1,X

                jsr CIOV
                bpl _1

                lda #$01
                bne _next2

_1              ldx #$10                ; GET $84,$09
                lda #<DBUF
                sta IOCB0+ICBAL,X
                lda #>DBUF
                sta IOCB0+ICBAH,X

                lda #<$0002             ; length
                sta IOCB0+ICBLL,X
                lda #>$0002
                sta IOCB0+ICBLH,X

                lda #$06                ; GETCHR
                sta IOCB0+ICCOM,X

                jsr CIOV
                bmi _next3

                lda #$FF
                cmp DBUF
                bne _2

                lda #$FF
                cmp DBUF+1
                bne _2

_next1          ldx #$10                ; GET $ADDR,$LEN
                lda #<DBUF
                sta IOCB0+ICBAL,X
                lda #>DBUF
                sta IOCB0+ICBAH,X

                lda #<$0004             ; length
                sta IOCB0+ICBLL,X
                lda #>$0004
                sta IOCB0+ICBLH,X

                jsr CIOV
                bpl _3

                cpy #$88
                bne _next3

                lda #$00
_next2          pha
                tya
                pha

                ldx #$10
                lda #$0C                ; CLOSE
                sta IOCB0+ICCOM,X

                jsr CIOV

                pla
                tay
                pla
                tax

                jmp _5

_2              lda #$03
                bne _next2

_next3          lda #$02
                bne _next2

_3              ldx #$10                ; read the record
                lda DBUF
                sta IOCB0+ICBAL,X
                lda DBUF+1
                sta IOCB0+ICBAH,X

                lda DBUF+2
                sec
                sbc DBUF
                sta IOCB0+ICBLL,X
                lda DBUF+3
                sbc DBUF+1
                sta IOCB0+ICBLH,X

                inc IOCB0+ICBLL,X
                bne _4

                inc IOCB0+ICBLH,X

_4              jsr CIOV
                bmi _next3
                jmp _next1

_5              cpx #$00
                beq _7

                cpx #$03
                beq _6

                tya
                jmp CIOER

_6              lda #<BLFMSG
                ldx #>BLFMSG
                jmp PRINTERR

_7              jsr INIT

                ldy PBDATA
                jsr GETOBJ
                jsr DECOMPRESS

                stz SCANMODE

                jsr DRAWDISPLAY


;--------------------------------------
;
;--------------------------------------
RESETUP         ldx STACKTEMP
                txs

                jmp START


;--------------------------------------
;
;--------------------------------------
SAVE_           jsr GETNAME
                jsr INPROMPT1

                lda #$01
                sta DISKCMD

                lda #$80
                sta SCANMODE

                jsr DRAWDISPLAY
                jsr COMPRESS

                lda #<PBBASE
                sta BSPARM
                lda #>PBBASE
                sta BSPARM+1

                lda MIDBTM
                sta BSPARM+2
                lda MIDBTM+1
                sta BSPARM+3

                lda #$08
                sta SAVETYPE

                jsr BSAVE

                stz SCANMODE

                jsr DRAWDISPLAY
                jmp RESETUP


;======================================
;
;======================================
BSAVE           ldx #$10
                lda #<CHARBUF
                sta IOCB0+ICBAL,X
                lda #>CHARBUF
                sta IOCB0+ICBAH,X

                lda #$03                ; OPEN
                sta IOCB0+ICCOM,X

                lda SAVETYPE
                sta IOCB0+ICAX1,X

                jsr CIOCL

                ldx #$10
                lda #$0A                ; PUTCHR
                sta IOCB0+ICCOM,X

                lda SAVETYPE
                cmp #$09                ; APPEND, NO HEADER?
                beq _1                  ;   yes

                lda #<SAVEHDR
                sta IOCB0+ICBAL,X
                lda #>SAVEHDR
                sta IOCB0+ICBAH,X

                lda #<$0002             ; length
                sta IOCB0+ICBLL,X
                lda #>$0002
                sta IOCB0+ICBLH,X

                jsr CIOCL

_1              ldx #$10                ; SAVE $ADDR,$LEN
                lda #<DATAHDR
                sta IOCB0+ICBAL,X
                lda #>DATAHDR
                sta IOCB0+ICBAH,X

                lda BSPARM
                sta DATAHDR
                lda BSPARM+1
                sta DATAHDR+1

                lda BSPARM+2
                sec
                sbc #$01
                sta DATAHDR+2
                lda BSPARM+3
                sbc #$00
                sta DATAHDR+3

                lda #<$0004             ; length
                sta IOCB0+ICBLL,X
                lda #>$0004
                sta IOCB0+ICBLH,X

                jsr CIOCL

                ldx #$10                ; SAVE DATA
                lda BSPARM
                sta IOCB0+ICBAL,X
                lda BSPARM+1
                sta IOCB0+ICBAH,X

                lda BSPARM+2
                sec
                sbc BSPARM
                sta IOCB0+ICBLL,X

                lda BSPARM+3
                sbc BSPARM+1
                sta IOCB0+ICBLH,X

                jsr CIOCL

                ldx #$10
                lda #$0C                ; CLOSE
                sta IOCB0+ICCOM,X

                ;[fall-through]


;======================================
;
;======================================
CIOCL           jsr CIOV

                tya
                bmi _1

                rts

_1              tya

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
CIOER           sec
                sbc #$64

                ldx #$FF
_next1          inx
                sec
                sbc #$0A
                bpl _next1

                clc
                adc #$8A
                stx ERTENS
                sta ERUNITS

                lda #<ERRMSG
                ldx #>ERRMSG

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
PRINTERR        pha
                txa
                pha

                ldx #$10
                lda #$0C                ; CLOSE
                sta IOCB0+ICCOM,X

                jsr CIOV

                lda DISKCMD
                cmp #$01
                bne _1

                sta SCANMODE

                jsr DRAWDISPLAY
_1              jsr DODIALOG

                ldy #$59
                ldx #$01
                lda #$01
                jsr CHARTO

                pla
                tax
                pla
                jsr PRINT_

                lda #$FF
                sta CH_

_wait1          cmp CH_
                beq _wait1

                jsr UNDODIALOG

                lda DISKCMD
                cmp #$02
                bne _XIT

                jmp MAKEOUT

_XIT            jmp RESETUP


;--------------------------------------
;--------------------------------------

ERRMSG          .byte $0E,$1B,$1B
                .byte $18,$1B,$24
                .byte $24,$24,$01
ERTENS          .byte $00
ERUNITS         .byte $80
BLFMSG          .byte $0B,$0A,$0D,$24,$15
                .byte $18,$0A,$0D,$24,$0F
                .byte $12,$15,$8E


;--------------------------------------
;
;--------------------------------------
QUIT            ldx STACKTEMP
                txs

                rts


;--------------------------------------
;
;--------------------------------------
MAKE            jsr GETNAME

                ldy CHARINDX
                lda #$9B                ; CR
                sta CHARBUF+3,Y

                lda #$02
                sta DISKCMD

                jsr CLEARMENU

                ldy #$BF
                jsr MAKEHOLE
                jsr SWAPUSER
                jsr INPROMPT1

                lda #<$2480
                sta BASE1
                lda #>$2480
                sta BASE1+1

                lda #<PATCH
                sta BASE2
                lda #>PATCH
                sta BASE2+1

                ldy #$00
_next1          lda (BASE1),Y
                sta (BASE2),Y

                iny
                bne _next1

                inc BASE1+1
                inc BASE2+1

                lda BASE1+1
                cmp #$2B
                bne _next1

                lda #<GAMEBTM
                sta BSPARM
                lda #>GAMEBTM
                sta BSPARM+1

                lda #<GAMETOP
                sta BSPARM+2
                lda #>GAMETOP
                sta BSPARM+3

                lda #$08
                sta SAVETYPE

                jsr BSAVE

                lda #<GAMESTART
                sta RUNAD
                lda #>GAMESTART
                sta RUNAD+1

                lda #<$02E0
                sta BSPARM
                lda #>$02E0
                sta BSPARM+1

                lda #<$02E2
                sta BSPARM+2
                lda #>$02E2
                sta BSPARM+3

                lda #$09                ; APPEND
                sta SAVETYPE

                jsr BSAVE

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
MAKEOUT         lda STACKTEMP
                jmp RELOAD


;======================================
;
;======================================
INPROMPT1       jsr DODIALOG

                ldy #$59
                ldx #$01
                lda #$00
                jsr CHARTO

                lda #<INSERTMSG
                ldx #>INSERTMSG
                jsr PRINT_

_wait1          lda CH_
                cmp #$FF
                beq _wait1

                jmp UNDODIALOG


;--------------------------------------
;
;--------------------------------------
PLAY            jsr CLEARMENU

                ldy #$BF
                jsr MAKEHOLE
                jsr SWAPUSER
                jsr PLAYGAME

                jmp MAKEOUT


;======================================
;
;======================================
CLEARMENU       jsr XDRAWCRSR

                ldy #$03
                jsr SETMODE

                lda #<MENUBOX
                ldx #>MENUBOX
                jsr DRAWRECT

                ldy #$02
                jmp SETMODE


;======================================
;
;======================================
DODIALOG        ldy #$01
                jsr SETMODE

                lda #<DIALOGBITS
                ldx #>DIALOGBITS
                jsr DRAWBITS

                ldy #$03
                jsr SETMODE

                lda #<DIALOGBOX
                ldx #>DIALOGBOX
                jsr DRAWRECT

                ldy #$02
                jsr SETMODE

                jmp FRAMERECT+3


;======================================
;
;======================================
UNDODIALOG      ldy #$00
                jsr SETMODE

                lda #<DIALOGBITS
                ldx #>DIALOGBITS
                jsr DRAWBITS

                ldy #$02
                jmp SETMODE


;======================================
;
;======================================
GETNAME         jsr CLEARMENU
                jsr DODIALOG

                lda #'D'                ; 'D1:'
                sta CHARBUF
                lda #'1'
                sta CHARBUF+1
                lda #':'
                sta CHARBUF+2

                lda #$FF
                sta CH_

                ldy #$59
                ldx #$01
                lda #$00
                stx BOXCURSOR+3
                sta BOXCURSOR+4
                sta BOXCURSORON
                sta CHARINDX

                jsr CHARTO

_next1          jsr CHECKCURSOR

                lda CH_
                cmp #$FF
                beq _next1

                cmp #$C0
                bcs _next1

_next2          cmp #$40
                bcc _1

                sbc #$40
                bcs _next2

_1              tax
                lda #$FF
                sta CH_

                lda ATASCII,X
                sta CHARASCII
                stx CHCODE

                cpx #$1C                ; ESC
                beq _2

                cpx #$0C                ; RETURN
                beq _3                  ; C=1

                jsr CURSOROFF

                lda CHARASCII
                ldx CHCODE
                jsr DOKBD

                jsr CURSOROFF
                jmp _next1

_2              clc
_3              php

                jsr CURSOROFF
                jsr UNDODIALOG

                ldy CHARINDX
                lda #$2E
                sta CHARBUF+3,Y
                lda #$50
                sta CHARBUF+4,Y
                lda #$42
                sta CHARBUF+5,Y
                lda #$9B
                sta CHARBUF+6,Y
                lda #$00
                sta CHARBUF+7,Y

                plp
                bcs _XIT

                ldx STACKTEMP
                txs

                jmp START

_XIT            rts

;--------------------------------------

DIALOGBITS      .word $B500
                .byte $57,$00,$00,$0B,$28
DIALOGBOX       .byte $57,$00,$00,$0A,$27,$06


;======================================
;
;======================================
DOKBD           ldy CHARINDX
                cpx #$34                ; BACKSP
                beq _5

                cpy #$08
                bcs _XIT1

                cpx #$21
                bne _2

                sta CHARBUF+3,Y

                lda CHAR+3
                sta LINELEN,Y

                tax
                lda CHAR+4
                sta LINELEN+9,Y

                clc
                adc #$04
                cmp #$08
                bcc _1

                sbc #$08

                inx
_1              stx CHAR+3
                stx BOXCURSOR+3
                sta CHAR+4
                sta BOXCURSOR+4
                jmp _3

_2              cmp #$30
                bcc _XIT1

                cmp #$3A
                bcs _4

                sbc #$2F                ; C=0, -10
_next1          tax

                lda CHARASCII
                sta CHARBUF+3,Y

                lda CHAR+3
                sta LINELEN,Y

                lda CHAR+4
                sta LINELEN+9,Y

                txa
                jsr PRCHAR

                lda CHAR+3
                sta BOXCURSOR+3
                lda CHAR+4
                sta BOXCURSOR+4

_3              inc CHARINDX

_XIT1           rts

_4              and #$DF
                sta CHARASCII

                cmp #$41
                bcc _XIT1

                cmp #$5B
                bcs _XIT1

                sbc #$36                ; C=0 -B7
                bne _next1

_5              dey
                bmi _XIT1

                sty CHARINDX

                lda LINELEN,Y
                sta CHAR+3
                sta CLRRECT+1
                sta BOXCURSOR+3

                lda LINELEN+9,Y
                sta CHAR+4
                sta CLRRECT+2
                sta BOXCURSOR+4

                lda #$20
                sta CHARBUF+3,Y

                ldy #$03
                jsr SETMODE

                lda #<CLRRECT
                ldx #>CLRRECT
                jsr DRAWRECT

                ldy #$02
                jmp SETMODE

;--------------------------------------

CLRRECT         .byte $59,$00,$00,$07,$00,$07


;======================================
;
;======================================
CURSOROFF       lda BOXCURSORON
                bne DOCURSOR

                rts


;======================================
;
;======================================
CHECKCURSOR     inc DTIMER1

                bne DOCURSOR._XIT

                inc DTIMER2
                lda DTIMER2
                and #$1F
                bne DOCURSOR._XIT

                lda BOXCURSORON
                eor #$FF                ; flip the bits
                sta BOXCURSORON

DOCURSOR        lda #<BOXCURSOR
                ldx #>BOXCURSOR
                jmp XOFFDRAW

_XIT            rts

;--------------------------------------

BOXCURSOR       .addr LA5AC
                .byte $59,$00,$00,$07,$01
LA5AC           .byte $FC,$FC,$FC,$FC,$FC
                .byte $FC,$FC


;======================================
;
;======================================
COMPRESS        lda #<HIRES
                sta BASE1
                lda #>HIRES
                sta BASE1+1

                ;[fall-through]


;--------------------------------------
;
;--------------------------------------
FINDZ           ldy #$00
_next2          lda (BASE1),Y
                bne _2

                iny
                bne _next2

                lda #$01
                sta (MIDBTM),Y

                tya
                iny
                sta (MIDBTM),Y          ; 1/256 RECORD

                inc BASE1+1             ; BASE+256

                lda MIDBTM              ; MIDBTM+2
                clc
                adc #$02
                sta MIDBTM

                bcc _1

                inc MIDBTM+1

_1              jmp FINDZ

_2              lda BASE1+1
                cmp #>HTOP
                bcs _3

                cpy #$02
                bcc FINDNZ

_3              sty TEMP

                lda #$01
                ldy #$00
                sta (MIDBTM),Y

                iny
                lda TEMP
                sta (MIDBTM),Y          ; 1/RUNLEN REC

                clc
                adc BASE1               ; BASE+RUNLEN
                sta BASE1
                bcc _4

                inc BASE1+1

_4              lda MIDBTM              ; MIDBTM+2
                clc
                adc #$02
                sta MIDBTM
                bcc _5

                inc MIDBTM+1

_5              lda BASE1+1
                cmp #>HTOP
                bcc FINDZ

                ldy #$00
                lda #$01
                sta (MIDBTM),Y
                iny
                sta (MIDBTM),Y

                lda MIDBTM
                clc
                adc #$02
                sta MIDBTM

                bcc _XIT

                inc MIDBTM+1

_XIT            rts


;--------------------------------------
;
;--------------------------------------
FINDNZ          iny
_next1          lda (BASE1),Y
                beq _1

                iny
                bne _next1

_next2          tya
                sta (MIDBTM),Y

                inc MIDBTM
                bne _next3

                inc MIDBTM+1

_next3          lda (BASE1),Y
                sta (MIDBTM),Y

                iny
                bne _next3              ; 256 BYTES/REC

                inc BASE1+1
                inc MIDBTM+1

                jmp FINDZ

_1              iny
                beq _next2

                lda (BASE1),Y
                bne _next1

                sty TEMP
                tya
                ldy #$00
                sta (MIDBTM),Y

                inc MIDBTM
                bne _next4

                inc MIDBTM+1

_next4          lda (BASE1),Y
                sta (MIDBTM),Y

                iny
                bne _next4              ;RUN LEN BYTES/REC

                lda TEMP
                clc
                adc BASE1
                sta BASE1
                bcc _2

                inc BASE1+1

_2              lda TEMP
                clc
                adc MIDBTM
                sta MIDBTM
                bcc _XIT

                inc MIDBTM+1

_XIT            jmp FINDZ


;======================================
;
;======================================
DECOMPRESS      lda #<HIRES
                sta BASE1
                lda #>HIRES
                sta BASE1+1

_next1          ldy #$00
                lda (OBJ),Y
                cmp #$01
                bne _4

                iny
                lda (OBJ),Y
                cmp #$01
                beq _XIT1

                sta TEMP

                tay
                dey
                lda #$00
_next2          sta (BASE1),Y

                dey
                bne _next2

                sta (BASE1),Y

                lda TEMP
                beq _1

                clc
                adc BASE1
                sta BASE1
                bcc _2

_1              inc BASE1+1

_2              lda OBJ
                clc
                adc #$02
                sta OBJ
                bcc _3

                inc OBJ+1

_3              jmp _next1

_XIT1           rts

_4              sta TEMP

                inc OBJ
                bne _5

                inc OBJ+1

_5              tay
                dey
_next3          lda (OBJ),Y
                sta (BASE1),Y

                dey
                bne _next3

                lda (OBJ),Y
                sta (BASE1),Y

                lda TEMP
                beq _6

                clc
                adc BASE1
                sta BASE1
                bcc _7

_6              inc BASE1+1

_7              lda TEMP
                beq _8

                clc
                adc OBJ
                sta OBJ
                bcc _9

_8              inc OBJ+1

_9              jmp _next1


;--------------------------------------
;--------------------------------------

ATASCII         .byte $4C,$4A,$3A,$80,$80,$4B,$5C,$5E
                .byte $4F,$80,$50,$55,$9B,$49,$5F,$7C
                .byte $56,$80,$43,$80,$80,$42,$58,$5A
                .byte $34,$80,$33,$36,$1B,$35,$32,$31
                .byte $5B,$20,$5D,$4E,$80,$4D,$3F,$81
                .byte $52,$80,$45,$59,$9F,$54,$57,$51
                .byte $39,$80,$30,$37,$9C,$38,$7D,$9D
                .byte $46,$48,$44,$80,$83,$47,$53,$41

SAVEHDR         .byte $FF,$FF
DATAHDR         .byte $00,$00,$00,$00
DBUF            .byte $00,$00,$00,$00

CHARBUF         .byte $44,$31,$3A,$00
                .byte $00,$00,$00,$00
                .byte $00,$00,$00,$00
                .byte $00,$00,$00,$00

LINELEN         .byte $00,$00,$00,$00
                .byte $00,$00,$00,$00
                .byte $00,$00


;--------------------------------------
;--------------------------------------     unused

                .text 'HD'
                .byte $80,$83

                .text 'GSA'
                .byte $84,$09

                .byte $00,$00,$00,$00
                .byte $00,$00,$00,$00

                .text 'D1:'

;--------------------------------------
                .fill 157,$00
;--------------------------------------

            .enc "atari-inverse"
                .text ' RTS'
            .enc "none"
