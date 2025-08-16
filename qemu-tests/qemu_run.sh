#!/bin/bash

# This requires you to previously run `cargo install defmt-print`

# See https://ferroussystems.hackmd.io/@jonathanpallant/ryA1S6QDJx for a description of all the relevant QEMU machines

ELF_BINARY=$1
# All suitable for thumbv7em-none-eabihf
MACHINE="-cpu cortex-m4 -machine mps2-an386"
# MACHINE="-cpu cortex-m7 -machine mps2-an500"

LOG_FORMAT='{[{L}]%bold} {s} {({ff}:{l:1})%dimmed}'
echo "ELF_BINARY=$ELF_BINARY"
shift
echo "Running on '$MACHINE'..."
echo "------------------------------------------------------------------------"
qemu-system-arm $MACHINE -semihosting-config enable=on,target=native -nographic -kernel $ELF_BINARY -serial tcp::19021,server,nowait | defmt-print -e $ELF_BINARY $* --log-format="$LOG_FORMAT"
echo "------------------------------------------------------------------------"
