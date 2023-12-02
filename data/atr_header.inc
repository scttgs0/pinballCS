
; SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
; SPDX-PackageOriginator: BudgeCo: Bill Budge
; SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
; SPDX-License-Identifier: MIT

; SPDX-FileCopyrightText: Copyright 2023 Scott Giese


                .word $0296             ; magic token
                .word $1680             ; image length (in paragraphs [16 bytes]... image size is $16800)
                .word $0080             ; sector length (128-byte sectors)
                .byte $00               ; image length high-byte (24-bit length)
                .dword $00000000        ; CRC
                .byte $00,$00,$00,$00   ; unused
                .byte $01               ; flags
