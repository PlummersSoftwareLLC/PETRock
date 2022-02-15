# PET Rock

[![CI](https://github.com/PlummersSoftwareLLC/PETRock/actions/workflows/CI.yml/badge.svg)](https://github.com/PlummersSoftwareLLC/PETRock/actions/workflows/CI.yml)

A spectrum analyzer display for the PET and C64 that receives its data from an ESP32 MCU on the user port that does the audio processing, FFT, etc.

## Configuring and building

Towards the top of the [`petrock.asm`](petrock.asm) file, a number of symbols can be defined that can be used to configure the build:
|Name|Possible values|Mandatory|Meaning|
|-|-|-|-|
|C64|0 or 1|No|Configure build for the Commodore 64. Exactly one of C64 or PET **must** be defined to equal 1.|
|DEBUG|0 or 1|Yes|Set to 1 to enable code that only is included for debug builds.|
|EPROM|0 or 1|Yes|When set to 1, the BASIC stub and load address will not be included in the build output.|
|PET|0 or 1|No|Configure build for the PET. Exactly one of C64 or PET **must** be defined to equal 1.|

This repository's code targets the ca65 assembler and cl65 linker that are part of the [cc65](https://cc65.github.io/) GitHub project. You will need a fairly recent build of cc65 for assembly of this repository's contents to work.

With the cc65 toolkit installed and in your PATH, you can build the application using one of the following commands:

* For the PET:

  ```text
  cl65 --verbose  --asm-include-dir include --asm-define PET=1 -o petrock.prg -t none petrock.asm
  ```

* For the C64:

  ```text
  cl65 --verbose  --asm-include-dir include --asm-define C64=1 -o c64rock.prg -t none petrock.asm
  ```

## Loading and running

Assuming the petrock.prg or c64rock.prg file is on a disk in device 8, the spectrum analyzer display can be loaded using the following command:

```text
LOAD "PETROCK.PRG",8
```

or

```text
LOAD "C64ROCK.PRG",8
```

## 6502 assembly

For those who would like more information about the 6502 CPU and/or about writing assembly code for it, the folks at [6502.org](http://www.6502.org) have compiled a lot of resources on the topic. Amongst others, there is a page that contains [links to tutorials and primers](http://www.6502.org/tutorials/), which itself links to a [detailed description of the 6502 opcodes](http://www.6502.org/tutorials/6502opcodes.html) used in the PET clock source code.
