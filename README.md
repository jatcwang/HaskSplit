HaskSplit
=========

*Split and Reconstruct files using Shamir's Secret Sharing Scheme. Implemented
in Haskell.*

HaskSplit allows you to split a file into multiple shares such that a predefined
number of these shares can be used to reconstruct the original file.

## Usage

There are only two commands:  `split` and `reconstruct`

`split` requires 3 parameters - the file the split, total number of shares
and the threshold for recontruction. You can optionally specify an output
directory using `-o` option.

`reconstruct` command will take the original file name as an input and searches 
through the current directory (or a directory given by the `-i` command) to find
all the file shares, and attempt to reconstruct them.

## Example Usage

Let's split a secret mp3 file into 5 parts, and say 3 of them is required to
reconstruct the original file.

    hasksplit split splitme.mp3 5 3 -o ./outputFolder

You should now see 5 files in the ./outputFolder directory, with name of the form
splitme.mp3.XXX, where XXX is between 001 and 255.

Now delete any 2 of the output files and run the following command

    hasksplit reconstruct splitme.mp3 -i ./splitFolder -o ./reconstructFolder

and you should see your original mp3 file in ./reconstructFolder!

## Limitations and Disclaimer
 
This program uses the GF(2^8) Galois Field, thus the maximum number of shares
that can be produced is 255.

While the theory behind this (SSSS) is sound and this has passed some basic
tests, please think very, very carefully before using this to split your Nuclear
Armaggedon secret code.

## Contributing and Critique

This program was written as part of my Haskell learning. The aim was to write a idiomatic
Haskell program and explore several areas such as error handling, streaming, and
command line parsing.

Therefore you are very encouraged to:

 * Critique my code and program design
 * Submit bug reports
 * Send in pull requests for additional features

If you'd like some features, feel free to file a request and I'll see what
I can do :)



