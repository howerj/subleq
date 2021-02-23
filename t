#!/bin/sh
set -eu
make subleq subleq.dec
testing () {
	echo "Testing: {$1} -> {$2}"
	echo "$1" | ./subleq subleq.dec > a.txt
	echo "$2" > b.txt
	diff -w a.txt b.txt
	rm a.txt b.txt
}
testing "2 2 + . cr bye" "4 "
testing ": x 2 2 + . cr ; x bye" "4 "
testing ": hello .\" Hello, World!\" cr ; hello bye" "Hello, World!"
testing ": x 2 2 + . ; immediate : y x x 3 . ; y bye" "4 4 3 "
testing ": tst if 99 . then ; 0 tst 1 tst 2 tst bye" "99 99 "
testing ": tst if 88 else 77 then . ; -1 tst 0 tst 1 tst bye" "88 77 88 "
testing ": tst for r@ . next ; 3 tst bye" "3 2 1 0 "
