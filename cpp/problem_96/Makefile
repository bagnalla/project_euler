SHELL := /bin/bash

default:
	time g++ -std=c++2a -O3 main.cc

debug:
	time g++ -std=c++2a -g main.cc

run: default
	time ./a.out

clean:
	rm -f a.out
