
; SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
; SPDX-PackageOriginator: BudgeCo: Bill Budge
; SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
; SPDX-License-Identifier: MIT

; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


                .include "equates/system_atari8.equ"
                .include "equates/zeropage.equ"

                .include "equates/cdraw.equ"


;--------------------------------------
;--------------------------------------
                * = $49DC
;--------------------------------------

;======================================
; INIT ATARI 800
;======================================
InitAtari       stz STKX
                stz STKX+1
                stz STKY

                lda VVBLKI
                sta NMICONT
                lda VVBLKI+1
                sta NMICONT+1

                lda RTCLOK+2
_wait1          cmp RTCLOK+2            ; wait one jiffy
                beq _wait1

                lda #<VBICursor
                sta VVBLKI
                lda #>VBICursor
                sta VVBLKI+1

                rts


;--------------------------------------
;--------------------------------------

                .byte $00
