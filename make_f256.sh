
# SPDX-PackageSummary: Pinball Construction Set (for 8-bit systems)
# SPDX-PackageOriginator: BudgeCo: Bill Budge
# SPDX-PackageCopyrightText: Copyright (c) 1982 Bill Budge
# SPDX-License-Identifier: MIT

# SPDX-FileCopyrightText: Copyright 2023 Scott Giese


mkdir -p obj/

# -------------------------------------

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/DEMO1.PB \
        data/DEMO1.inc

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/DEMO2.PB \
        data/DEMO2.inc

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/DEMO3.PB \
        data/DEMO3.inc

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/DEMO4.PB \
        data/DEMO4.inc

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/DEMO5.PB \
        data/DEMO5.inc

# -------------------------------------

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/BITMAPS.bin \
        data/BITMAPS.inc

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/DLIST.bin \
        data/DLIST.inc

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/GPAK.bin \
        data/GPAK.inc

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/TITLE.bin \
        data/TITLE.inc

# -------------------------------------

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/boot2.bin \
        --list=obj/boot2.lst \
        --labels=obj/boot2.lbl \
        boot2.asm

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/cdraw.bin \
        --list=obj/cdraw.lst \
        --labels=obj/cdraw.lbl \
        cdraw.asm

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/disk.bin \
        --list=obj/disk.lst \
        --labels=obj/disk.lbl \
        disk.asm

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/edit.bin \
        --list=obj/edit.lst \
        --labels=obj/edit.lbl \
        edit.asm

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/platform_f256.bin \
        --list=obj/platform_f256.lst \
        --labels=obj/platform_f256.lbl \
        platform_f256.asm

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/ppak.bin \
        --list=obj/ppak.lst \
        --labels=obj/ppak.lbl \
        ppak.asm

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/run.bin \
        --list=obj/run.lst \
        --labels=obj/run.lbl \
        run.asm

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/run2.bin \
        --list=obj/run2.lst \
        --labels=obj/run2.lbl \
        run2.asm

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/swap.bin \
        --list=obj/swap.lst \
        --labels=obj/swap.lbl \
        swap.asm

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/tst.bin \
        --list=obj/tst.lst \
        --labels=obj/tst.lbl \
        tst.asm

64tass  --m65c02 \
        --flat \
        --nostart \
        -o obj/wire.bin \
        --list=obj/wire.lst \
        --labels=obj/wire.lbl \
        wire.asm


# -------------------------------------


cat     obj/boot2.bin \
        obj/GPAK.bin \
        obj/cdraw.bin \
        obj/TITLE.bin \
        obj/DLIST.bin \
        obj/platform_f256.bin \
        obj/swap.bin \
        obj/tst.bin \
        obj/BITMAPS.bin \
        obj/run.bin \
        obj/ppak.bin \
        obj/edit.bin \
        obj/wire.bin \
        obj/disk.bin \
        obj/run2.bin \
        obj/DEMO1.PB \
        obj/DEMO2.PB \
        obj/DEMO3.PB \
        obj/DEMO4.PB \
        obj/DEMO5.PB \
        >obj/pinballCS.bin
