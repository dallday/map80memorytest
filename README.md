# map80memorytest (m80memtp) version 7

This program is designed to run on a NASCOM2 Z80 computer.

This is a memory test program for the Nascom MAP80 256k memory card.
  It is designed to test the paging options provided by the MAP80 256k card.
    but can be used to test nonpaged memory by setting Numberof64kpages to 0.

m80memtp - stands for M80MEMoryTestPaging

The main change for Version 6 was to avoid an issue on the Nascom4 system and the serial port. 

Version 7 was a fix to version 6 to get the top line on the screen correct :(

See m80memtp.asm for full details on using the program.

See readme.txt for details on assembling the program using z80asm on a linux system.

You will need to set the "execute" flag on the build and compile.sh files if you want to use them.

File name | Description
----------|------------
build | simple bash script to assemble the code
compile.sh |calls build and then creates the .nas and .cas files.
m80memtp.asm | The memory test program code
m80memtp.bin | The assembled code in binary form
m80memtp.cas | The program in a casette format
m80memtp.nas | The program in a Nascom load format
m80memtp.lst | The output listing from the assembly
readme.txt | Some details on assembling the program
