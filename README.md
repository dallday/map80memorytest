# map80memorytest (m80memtp) verion 5

This program is designed to run on a NASCOM2 Z80 computer.

This is a memory test program for the Nascom MAP80 256k memory card.
  It is designed to test the paging options provided by the MAP80 256k card.
    but can be used to test nonpaged memory by setting Numberof64kpages to 0.

m80memtp - stands for M80MEMoryTestPaging

Since we are talking about a NASSYS program the memory from 0000H to 07FFH 
     will always be used by the monitor, working ram and video ram.
     So testing between 0000H and 07FFH will not be very useful 
     ( unless page count set to 0 and you advoid the ram used by NASSYS and this program )

The progam will always be in memory 0C80H no matter what paging it does.

Usage:-   
    E C80 8000 9000 2 0
    This will test from address 8000 to 8FFF using 4 32k pages 
       starting at row 0

If start address is between 1000H and 7FFFH and the end address is 8000H or below then
        it will test each 32k page using the lower 32k of memory.

If start address is between 8000H and FFFFH and the end address is 0000H or  > 8000H and <= FFFFH then
        it will test each 32k page using the upper 32k of memory.

If start address is between 1000H and 7FFFH and the end address is above 8000H then
        it will test each 64k page of memory.

The standard 256k memory card has 8 pages of 32k  
    so will check pages 0 to 7   
The standard 256k memory card has 4 pages of 64k  
    so will check pages 0 to 3

Note it will end the test at the end address - 1   
       so E C80 1000 2000  
    will test the memory from 1000H to 1FFFH   

The main change for Version 4 was to avoid an issue on the Nascom4 system and the serial port.  
It also added a cycle number to the top screen so you know how many times the test has run.  
Version 5 was provide a start 64k page (row) as N4 used row 0 to store memory loaded nassys3.

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
