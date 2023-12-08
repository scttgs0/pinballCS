
; SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
; SPDX-PackageOriginator: BudgeCo: Bill Budge
; SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
; SPDX-License-Identifier: MIT

; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


                .include "equates/system_atari8.equ"

                .include "equates/platform_a8.equ"
                .include "equates/tst.equ"


;--------------------------------------
; Code equates
;--------------------------------------

PIECE2                  = $7B00

EDITOR                  = $A000


;--------------------------------------
;--------------------------------------
                * = $0680
;--------------------------------------

;   BOOT SECTOR HEADER

                .byte $00               ; boot flag
                .byte $8A               ; sector count; load 138 sectors
                .addr $0680             ; load addr
                .addr DISK_INIT         ; init addr


;--------------------------------------
;--------------------------------------
; Disk Initialization
;--------------------------------------
;--------------------------------------
DISK_INIT       jsr GoAtariTest
                jsr InitAtari

                stz DAUX2
                stz DBUFLO

;   load bitmaps, run, ppak, and edit
                lda #>PIECE2
                ldx #$C0                ; sector 192
                ldy #$70                ; 112 sectors
                jsr GetSectors

;   DEBUG: pause on the title screen
;_endless        jmp _endless

                jmp EDITOR


;======================================
;
;======================================
GetSectors      sta DBUFHI
                stx DAUX1
                sty _sectorCount

                lda #$01
                sta DUNIT

                lda #$52                ; READ
                sta DCOMND

_next1          jsr DSKINV

                lda DSTATS
                cmp #$01
                bne _next1

                lda DBUFLO
                adc #<$007F             ; C=1!
                sta DBUFLO
                lda DBUFHI
                adc #>$007F
                sta DBUFHI

                inc DAUX1
                bne _1

                inc DAUX2

_1              dec _sectorCount
                bne _next1

                rts


;--------------------------------------
;--------------------------------------

_sectorCount    .byte $17
                .byte $80

;--------------------------------------
                .fill 35,$00
;--------------------------------------
