
; SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
; SPDX-PackageOriginator: BudgeCo: Bill Budge
; SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
; SPDX-License-Identifier: MIT

; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


                .include "equates/system_atari8.equ"

                .include "equates/dlist.equ"


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
GoAtariTest     lda #<dlistMain
                sta SDLSTL
                sta DLISTL
                lda #>dlistMain
                sta SDLSTH
                sta DLISTH

                lda #$0F
                sta COLOR1
                sta COLPF1

                stz COLOR2
                stz COLPF2

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
