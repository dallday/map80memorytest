# map80memorytest

This program is designed to run on a NASCOM2 Z80 computer.

This is a memory test program for the Nascom MAP80 256k memory card.
  It is designed to test the paging options provided by the MAP80 256k card.
    but can be used to test nonpaged memory by setting Numberof64kpages to 0.
 
   I got a bit confused as the Pages the manual seem to talk about is rows 0 to 3 whereas
   the circuit diagram talks about rows 1 to 4 
   I believe page 0 is row 1.
  
  Since we are talking about a NASSYS program the memory from 0000H to 07FFH 
     will always be used by the monitor, working ram and video ram.
     So testing between 0000H and 07FFH will not be very useful 
     ( unless page count set to 0 and you advoid the ram used by NASSYS and this program )

  The progam will always be in memory 0D00 no matter what paging it does.

  if start address is between 1000H and 7FFFH and the end address is 8000H or below then
        it will test each 32k page using the lower 32k of memory.

  if start address is between 8000H and FFFFH and the end address is 0000H or  > 8000H and <= FFFFH then
        it will test each 32k page using the upper 32k of memory.

  if start address is between 1000H and 7FFFH and the end address is above 8000H then
        it will test each 64k page of memory.

  The standard 256k memory card has 8 pages of 32k
    so will check pages 0 to 7 
  The standard 256k memory card has 4 pages of 64k
    so will check pages 0 to 3
 
  The program calculates the number of test blocks it needs to do depending upon the type of test being done.
     So for 64k tests the number of blocks will be the number of 64k pages.
     For 32k tests the number of blocks will be twice the number of 64k pages.

  If you have more than 1 card or have upgraded it to a 1Mbyte card then
   you can specify the number of 64k pages to test from 1 to 010H ( 16 64k pages )

  Use this at your own risk, I think it works but no guarantees.
  If you want to use or amend this program please feel free.
   It would be nice if you mentioned me and let me know how it goes
            david_nascom@davjanbec.co.uk
   
 NOTE1:- The opcode test uses NMI so be careful trying to single step this code.
 NOTE2:- The program is written for the NASSYS3 so you cannot test memory locations 000H to 07FFH

***************** CALLING THE PROGRAM

 Call the program using 
  EC80 [startaddress [endaddress [Numberof64kpages] ] ]
      these values are stored in the program 
        so a call to EC80 will use the same values as the last call
   startaddress	The testing starts at this address
   endaddress		The testing finished at this address -1
   Numberof64kpages	The number of 64k memory pages to be tested.
			0 - special case and NO paging is activated.
			4 - standard M80 256k card fully populated
			10 - (16 dec) if you have 2 cards or moded the card to provide 1Mbyte.
  e.g.
      EC80 8000 9000 2
        will test memory from 8000 to 8FFF for 4 32k pages 
                Port EF set to 80, 81, 82, 83 for the tests

      EC80 1000 0000 2
        will test memory from 1000 to FFFF for 2 64k pages 
                Port EF set to 00, 01 for the tests

      EC80 1000 0000 0
        will test memory from 1000 to FFFF without paging. 
                Port EF is not change for this test.

         Only the low byte is used from lastpage parameter.
         The end address tested is actually -1 of the endaddress value.


 ***************   STOPPING THE PROGRAM 

 The program monitors the keyboard during the test and any key will interrupt it.
   This will undo the effect of the C button on the "press SP, C or Q" input.
   NOTE:- it only checks the keys at the end of each page in the initial setting to zero 
             to avoid slowing down that part of the processing.
    
 When the program says "press SP, C or Q"
   after 10 error lines or at end of each cycle
   	You can press space to continue
   	press C to continue without stopping for any errors
   	press Q or anything else to return to NAS-SYS

 *****************  Requirements:

 It is set to test a fully populated MAP80 256k memory card.
 The program assumes you have NAS-SYS3.
 The program defaults assume that you have a MAP80 256k Ram card fully populated.
 That you are running the program in the WRAM area ( 4114 chip in Nascom 2 board).
 That the card is using the standard control port at 0xFE.

