# PET Rock

[![CI](https://github.com/PlummersSoftwareLLC/PETRock/actions/workflows/CI.yml/badge.svg)](https://github.com/PlummersSoftwareLLC/PETRock/actions/workflows/CI.yml)

A spectrum analyzer display for the C64 that receives its data from an ESP32 MCU on the user port that does the audio processing, FFT, etc.

## General Idea for Newcomers

The application logic is implemented in [petrock.asm](petrock.asm). It pulls in a few supporting include files to define symbols, add demo data, and facilitate serial communication.

The application draws 16 vertical bands of the spectrum analyzer which can be up to 16 high. The program first clears the screen, draws the border and text, fills in color, and the main draw loop calls DrawBand for each one in turn. Each frame draws a new set of peaks from the PeakData table, which has 16 entries, one per band. That data is replaced either by a new frame of demo data or an incoming serial packet and the process is repeated, running at about 40 fps.

Color RAM can be filled with different patterns by stepping through the visual styles with the C key, but it is not drawn each and every frame.  

Basic bar draw is to walk down the bar and draw a blank (when above the bar), the top of the bar, then the middle pieces, then the bottom. A visual style definition is
set that includes all of the PETSCII chars you need to draw a band, like the corners and sides, etc. It can be changed with the S key.

Every frame the serial port is checked for incoming data which is then stored in the SerialBuf. If that fills up without a nul it is reset, but if a nul comess in at the
right place (right packet size) and the magic byte matches, it is used as new peakdata and stored in the PeakData table. The code on the ESP32 sends it over as 16 nibbles
packed into 8 bytes plus a VU value.

The built-in serial code on the C64 is poor, and [serdrv.s](serdrv.s) contains a new implementation that works well for receiving data up to 4800 baud.

## Building

This repository's code targets the ca65 assembler and cl65 linker that are part of the [cc65](https://cc65.github.io/) GitHub project. You will need a fairly recent build of cc65 for assembly of this repository's contents to work. If you receive errors about the .literal mnemonic, this is the likely reason.

With the cc65 toolkit installed and in your PATH, you can build the application using the following command:

```text
ca65 -v -g -l c64rock.lst -D C64=1 -t none petrock.asm 
ld65 -v -m c64rock.map -o c64rock.prg -t none petrock.o none.lib 
```

## Loading and running

Assuming the c64rock.prg file is on a disk in device 8, the spectrum analyzer display can be loaded using the following command:

```text
LOAD "C64ROCK.PRG",8
```

When the application is running, its appearance can be modified by pressing certain keys. Also, a demo mode can be enabled in case no serial input is available.

You can press H to see what key presses are recognized.

## 6502 assembly

For those who would like more information about the 6502 CPU and/or about writing assembly code for it, the folks at [6502.org](http://www.6502.org) have compiled a lot of resources on the topic. Amongst others, there is a page that contains [links to tutorials and primers](http://www.6502.org/tutorials/), which itself links to a [detailed description of the 6502 opcodes](http://www.6502.org/tutorials/6502opcodes.html) used in the PET clock source code.
