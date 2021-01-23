M80MEMTP

Memory test program for the Map80 256k card 

See m80memtp.asm for details of the tests etc.,

To assemble use you can use the build script 

./build m80memtp

which uses the z80asm (from https://www.nongnu.org/z80asm/index.html) on $PATH

The compile.sh script compiles it and then converts the output to a .nas and .cas format.

It uses the nascon convertor by Neal (from https://github.com/nealcrook/nascom/tree/master/converters).
nascon m80memtp.bin m80memtp.cas -org C80
nascon m80memtp.bin m80memtp.nas -csum -org C80

David Allday
January 2021



