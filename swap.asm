
; SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
; SPDX-PackageOriginator: BudgeCo: Bill Budge
; SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
; SPDX-License-Identifier: MIT

; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


                .include "equates/system_atari8.equ"

                .include "equates/boot2.equ"
                .include "equates/cdraw.equ"


;--------------------------------------
; System equates
;--------------------------------------

; CH_             = $02FC

; DSTATS          = $0303
; DBUFLO          = $0304
; DAUX2           = $030B


;--------------------------------------
; Code equates
;--------------------------------------

FRAMERECT_ENTRY1        = $265D

PIECE2                  = $7B00

EDITOR                  = $A000
LA010                   = $A010
LA013                   = $A013

LBF00                   = $BF00


;--------------------------------------
;--------------------------------------
                * = $4A00
;--------------------------------------

;======================================
;
;======================================
SWAPWIRE        ldx #$01
                jsr SWAPPIECE
                jmp LA013


;======================================
;
;======================================
SWAPDISK        ldx #$02
                jsr SWAPPIECE
                jmp LA010


;======================================
;
;======================================
SWAPUSER        ldx #$03
                bne SWAPSECTORS


;--------------------------------------
;
;--------------------------------------
RELOAD          pha

                jsr PCSINSERTED

                lda #$00
                sta DBUFLO
                sta DAUX2

                lda #$7B
                ldx #$C0
                ldy #$70
                jsr GetSectors

                pla
                tax
                txs
                rts


;======================================
;
;======================================
SWAPPIECE       jsr SWAPSECTORS
                jsr EDITOR

                ldx #$00


;======================================
;
;======================================
SWAPSECTORS     txa
                pha

                jsr PCSINSERTED

                pla
                tax
                lda #$00
                sta DBUFLO

                lda SECTORHI,X
                sta DAUX2

                lda ADDRHI,X
                pha

                ldy COUNT,X
                lda SECTOR,X

                tax
                pla

                jmp GetSectors


;--------------------------------------
;--------------------------------------

COUNT           .byte $26,$14,$10,$14
SECTORHI        .byte $01,$01,$01,$01
SECTOR          .byte $0A,$30,$44,$54
ADDRHI          .byte $A0,$A0,$A0,$90


;======================================
;
;======================================
PCSINSERTED     lda #$00
                sta DBUFLO
                sta DAUX2

                lda #$BF
                ldx #$C0
                ldy #$01
                jsr GetSectors

                lda DSTATS
                cmp #$01
                bne PCSINSERTED

                ldy #$40
_next1          lda LBF00,Y
                cmp PIECE2,Y
                bne _1

                dey
                bpl _next1

                rts

_1              ldy #$01
                jsr SETMODE

                lda #$F0
                ldx #$4A
                jsr DRAWBITS

                ldy #$03
                jsr SETMODE

                lda #$F7
                ldx #$4A
                jsr DRAWRECT

                ldy #$02
                jsr SETMODE
                jsr FRAMERECT_ENTRY1

                ldy #$59
                ldx #$01
                lda #$00
                jsr CHARTO

                lda #$E1
                ldx #$4A
                jsr PRINT_

                lda #$FF
                sta CH_

_next2          lda CH_
                cmp #$FF
                beq _next2

                lda #$FF
                sta CH_

                ldy #$00
                jsr SETMODE

                lda #$F0
                ldx #$4A
                jsr DRAWBITS

                ldy #$02
                jsr SETMODE
                jmp PCSINSERTED


;--------------------------------------
;--------------------------------------

INSERTMSG       .byte $12,$17,$1C,$0E,$1B,$1D,$24,$19,$0C,$1C,$24,$0D,$12,$1C,$94

DIALOGBITS      .addr $B500
                .byte $57,$00,$00,$0B,$28
DIALOGBOX       .byte $57,$00,$00,$0A,$27,$07
                .byte $04,$20,$7A,$00
