
; SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
; SPDX-PackageOriginator: BudgeCo: Bill Budge
; SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
; SPDX-License-Identifier: MIT

; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


;--------------------------------------
; System equates
;--------------------------------------

SDLSTL          = $0230
SDLSTH          = $0231

COLOR1          = $02C5
COLOR2          = $02C6
COLOR4          = $02C8

COLPF1          = $D017
COLPF2          = $D018
COLBK           = $D01A

DLISTL          = $D402
DLISTH          = $D403


;--------------------------------------
; Code equates
;--------------------------------------

dlistMain               = $4910


;--------------------------------------
;--------------------------------------
                * = $4B01
;--------------------------------------

                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$00
                .byte $00,$00,$00,$00,$00,$00,$00,$05
                .byte $03,$03,$03,$02,$1B,$0B,$02,$55
                .byte $0C,$82,$8F,$93,$93,$32,$32,$07
                .byte $07,$16,$08,$0E,$19,$0B,$14,$27
                .byte $BE,$BE,$B9,$AD,$5E,$50,$0E,$04
                .byte $01,$01,$55,$04,$87,$88,$5D,$5D
                .byte $AF,$BE,$BE,$B9


;======================================
;
;======================================
GOATARI         lda #<dlistMain
                sta SDLSTL
                sta DLISTL
                lda #>dlistMain
                sta SDLSTH
                sta DLISTH

                lda #$0F
                sta COLOR1
                sta COLPF1

                lda #$00
                sta COLOR2
                sta COLPF2

                lda #$44
                sta COLOR4
                sta COLBK

                rts


;--------------------------------------
                .byte $FF
;--------------------------------------


;--------------------------------------
;--------------------------------------
MAKEPATCH       sta COLOR4
                sta COLBK

                rts


;--------------------------------------
;--------------------------------------

MAKEPATCH2      .byte $AD,$00,$00,$00
                .byte $00,$00,$00,$00
                .byte $00,$00
