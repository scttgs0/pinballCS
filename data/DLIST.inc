
; SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
; SPDX-PackageOriginator: BudgeCo: Bill Budge
; SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
; SPDX-License-Identifier: MIT

; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


                .include "../equates/title.equ"


;--------------------------------------
; System equates
;--------------------------------------

AVB             = $40
ALMS            = $40
AJMP            = $01
AEMPTY8         = $70


;--------------------------------------
;--------------------------------------
                * = $4910
;--------------------------------------

dlistMain       .byte AEMPTY8,AEMPTY8,AEMPTY8

                .byte $0F+ALMS
                    .addr gfxTitleTop
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F

                .byte $0F+ALMS
                    .addr gfxTitleXBoundary
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F

                .byte $0F+ALMS
                    .addr gfxTitleBottom
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F
                .byte $0F,$0F,$0F

                .byte AVB+AJMP
                    .addr dlistMain
