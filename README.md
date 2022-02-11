# PET Rock

A spectrum analyzer display for the PET and C64 that receives its data from an ESP32 MCU on the user port that does the audio processing, FFT, etc.

## Configuring and building

Towards the top of the [`petrock.asm`](petclock.asm) file, a number of symbols are defined that can be used to configure the build:
|Name|Possible values|Meaning|
|-|-|-|
|DEBUG|0 or 1|Set to 1 to enable code that only is included for debug builds.|
|EPROM|0 or 1|When set to 1, the BASIC stub and load address will not be included in the build output.|
|PET|1|Configure build for the PET, otherwise C64

This repository's code targets the ca65 assembler and cl65 linker that are part of the [cc65](https://cc65.github.io/) GitHub project. You will need a fairly recent build of cc65 for assembly of this repository's contents to work.

With the cc65 toolkit installed and in your PATH, you can build the application using the following command:

```text
cl65 -o petrock.prg -t none petrock.asm
```

## Loading and running

Assuming the petrock.prg file is on a disk in device 8, the clock can be loaded using the following command:

```text
LOAD "PETROCK.PRG",8
```

## 6502 assembly

For those who would like more information about the 6502 CPU and/or about writing assembly code for it, the folks at [6502.org](http://www.6502.org) have compiled a lot of resources on the topic. Amongst others, there is a page that contains [links to tutorials and primers](http://www.6502.org/tutorials/), which itself links to a [detailed description of the 6502 opcodes](http://www.6502.org/tutorials/6502opcodes.html) used in the PET clock source code.
