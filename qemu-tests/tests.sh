#!/bin/bash

# Runs a series of sample programs in QEMU and checks that the standard output
# is as expected.

rustup target add thumbv7em-none-eabihf

FAILURE=0

fail() {
    FAILURE=1
}

mkdir -p ./target

my_diff() {
    file_a=$1
    file_b=$2
    # - Fix Windows path separators (\\) to look like UNIX ones (/) in the QEMU
    # output
    # - Fix the CRLF line endings in the files on disk, because git adds them to
    # text files.
    diff <(cat $file_a | tr -d '\r') <(cat $file_b | sed 's~\\\\~/~g')
}

binary=""
# armv7r-none-eabi tests
cargo run | tee ./target/output.txt
my_diff ./output/output.txt ./target/output.txt || fail

if [ "$FAILURE" == "1" ]; then
    echo "***************************************************"
    echo "test.sh: Output comparison failed!"
    echo "***************************************************"
    exit 1
else
    echo "***************************************************"
    echo "test.sh: Everything matches :)"
    echo "***************************************************"
fi
