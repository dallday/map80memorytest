# map80memorytest

This program is designed to run on a NASCOM2 Z80 computer.

This is a memory test program for the Nascom MAP80 256k memory card.
  It is designed to test the paging options provided by the MAP80 256k card.
    but can be used to test nonpaged memory by setting Numberof64kpages to 0.

See map80memtp.asm for full details on using the program.

See readme.txt for details on assembling the program.

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
