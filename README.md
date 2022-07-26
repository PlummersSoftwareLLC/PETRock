# PET Rock

[![CI](https://github.com/PlummersSoftwareLLC/PETRock/actions/workflows/CI.yml/badge.svg)](https://github.com/PlummersSoftwareLLC/PETRock/actions/workflows/CI.yml)

A spectrum analyzer display for the C64 that receives its data from an ESP32 MCU on the user port that does the audio processing, FFT, etc.

## General Idea for Newcomers

The application logic is implemented in [petrock.asm](petrock.asm). It pulls in a few supporting include files to define symbols, add demo data, and facilitate serial communication.

The application draws 16 vertical bands of the spectrum analyzer which can be up to 16 high. The program first clears the screen, draws the border and text, fills in color, and the main draw loop calls DrawBand for each one in turn. Each frame draws a new set of peaks from the PeakData table, which has 16 entries, one per band. That data is replaced either by a new frame of demo data or an incoming serial packet and the process is repeated, running at about 40 fps.

Color RAM can be filled with different patterns by stepping through the visual styles with the C key, but it is not drawn each and every frame.  

Basic bar draw is to walk down the bar and draw a blank (when above the bar), the top of the bar, then the middle pieces, then the bottom. A visual style definition is set that includes all of the PETSCII chars you need to draw a band, like the corners and sides, etc. It can be changed with the S key.

Every frame the serial port is checked for incoming data which is then stored in the SerialBuf. If that fills up without a nul it is reset, but if a nul comess in at the right place (right packet size) and the magic byte matches, it is used as new peakdata and stored in the PeakData table. The code on the ESP32 sends it over as 16 nibbles packed into 8 bytes plus a VU value.

Concerning handling of serial input:

- On the C64, the built-in serial code on the C64 is poor. [serial/c64/driver.s](serial/c64/driver.s) contains a new implementation for the C64 that works well for receiving data up to 4800 baud.
- On the PET, a built-in serial driver is effectively absent. [serial/pet/driver.s](serial/pet/driver.s) contains an implementation for the PET that is confirmed to receive data up to 2400 baud. Due to the hardware involved (the PET uses a 6522 VIA instead of the C64's 6526 CIA), the serial driver also includes its own keyboard polling routines.

## Configuring and building

In the [`settings.inc`](settings.inc) file, a number of symbols are defined that can be used to configure the build:
|Name|Possible values|Mandatory|Meaning|
|-|-|-|-|
|BSNSS_KBD|0 or 1|Yes, on the PET|Set to 0 to indicate your PET has a graphical keyboard. Set to 1 to indicate it has a business keyboard. This setting is ignored on the C64.|
|C64|0 or 1|No|Configure build for the Commodore 64. Exactly one of C64 or PET **must** be defined to equal 1.|
|DEBUG|0 or 1|Yes|Set to 1 to enable code that only is included for debug builds.|
|PET|0 or 1|No|Configure build for the PET. Exactly one of C64 or PET **must** be defined to equal 1.|
|SERIAL|0 or 1|Yes|Set to 1 to read visualisation data from the user port
|TIMING|0 or 1|Yes|Set to 1 to show timing information concerning the drawing of spectrum analyzer updates. Only supported on the C64 and has not been used for a while, so may need some attention to make it work.|

Note that:

- the PET and C64 symbols are not set by default. The reason is that the assembly target is a prime candidate to be set via the command line.
- PETs with a built-in piezo speaker make a clicking noise with this version of the code when serial is enabled. It's somehow related to the redrawing of the VU meters and frequency bars (hints on how to fix this are welcome!)

This repository's code targets the ca65 assembler and cl65 linker that are part of the [cc65](https://cc65.github.io/) GitHub project. You will need a fairly recent build of cc65 for assembly of this repository's contents to work. If you receive errors about the .literal mnemonic, this is the likely reason.

With the cc65 toolkit installed and in your PATH, you can build the application using any of the following commands:

- If the assembly target is set in `settings.inc`:

  ```text
  cl65 -o petrock.prg -t none petrock.asm
  ```

- For the PET:

  ```text
  cl65 -o petrock.prg --asm-define PET=1 -t none petrock.asm
  ```

- For the Commodore 64:

  ```text
  cl65 -o petrock.prg --asm-define C64=1 -t none petrock.asm
  ```
  
## Loading and running

Assuming the petrock.prg file is on a disk in device 8, the spectrum analyzer display can be loaded using the following command:

```text
LOAD "PETROCK.PRG",8
```

When the application is running, its appearance can be modified by pressing certain keys. Also, a demo mode can be enabled in case no serial input is available.

You can press H to see what key presses are recognized.

## 6502 assembly

For those who would like more information about the 6502 CPU and/or about writing assembly code for it, the folks at [6502.org](http://www.6502.org) have compiled a lot of resources on the topic. Amongst others, there is a page that contains [links to tutorials and primers](http://www.6502.org/tutorials/), which itself links to a [detailed description of the 6502 opcodes](http://www.6502.org/tutorials/6502opcodes.html) used in the PET clock source code.
