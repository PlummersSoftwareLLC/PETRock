# PET Rock

[![CI](https://github.com/PlummersSoftwareLLC/PETRock/actions/workflows/CI.yml/badge.svg)](https://github.com/PlummersSoftwareLLC/PETRock/actions/workflows/CI.yml)

A spectrum analyzer display for the C64 that receives its data from an ESP32 MCU on the user port that does the audio processing, FFT, etc.

## Building

This repository's code targets the ca65 assembler and cl65 linker that are part of the [cc65](https://cc65.github.io/) GitHub project. You will need a fairly recent build of cc65 for assembly of this repository's contents to work. If you receive errors about the .literal mnemonic, this is the likely reason.

With the cc65 toolkit installed and in your PATH, you can build the application using the following command:

```text
ca65 -v -g -l c64rock.lst -D C64=1 -t none petrock.asm 
ld65 -v -m c64rock.map -o c64rock.prg -t none petrock.o none.lib 
```

## Loading and running

Assuming the petrock.prg file is on a disk in device 8, the spectrum analyzer display can be loaded using the following command:

```text
LOAD "C64ROCK.PRG",8
```

When the application is running, its appearance can be modified by pressing certain keys. Also, a demo mode can be enabled in case no serial input is available.

You can press H to see what key presses are recognized.

## 6502 assembly

For those who would like more information about the 6502 CPU and/or about writing assembly code for it, the folks at [6502.org](http://www.6502.org) have compiled a lot of resources on the topic. Amongst others, there is a page that contains [links to tutorials and primers](http://www.6502.org/tutorials/), which itself links to a [detailed description of the 6502 opcodes](http://www.6502.org/tutorials/6502opcodes.html) used in the PET clock source code.
