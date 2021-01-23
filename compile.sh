#!/bin/bash
# compile and copy the program to the vnascom emulator
# build etc., are in the ~/bin directory
build m80memtp
# convert it to a cassette file
nascon m80memtp.bin m80memtp.cas -org C80
nascon m80memtp.bin m80memtp.nas -csum -org C80
# for nascon see https://github.com/nealcrook/nascom/tree/master/converters
#

