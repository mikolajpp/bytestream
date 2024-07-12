# Hoon bytestream library

## $bays bytestream type

Bytestream library provides bytestream type `$bays`, 
```
+$  bays  [pos=@ud =octs]
```
that is a pair of a cursor and octs data.

## Interface core

There are three families of functions: for reading, 
writing and appending to a bytestream. they operate 
upon three kinds of data: a singular byte, a sequence of 
bytes (octs), or a text (cord).

A combined operation of reading from a source bytestream
and writing the resulting data to a target bytestream is called
a 'write-read'. 

A write or append to a bytestream always succeeds. 

A read can fail if the source bytestream is exhausted:
reading arms either crash on failure, or return a unit (maybe).

Each read advances the cursor by a corresponding
number of bytes read; peek functions do not advance the stream.

Each write advances the cursor by a corresponding
number of bytes written; append functions do not advance the stream.

There is also a number of transformation functions, which accept
a user defined gate, which describe the way a bytestream should be transformed.

## Performance

Almost all bytestream operations are jetted; jets are provided even in the case
of simple arms, like reading a byte, but they are even more important in the case of read-write arms: here the jet not only speeds up Hoon execution, but also shaves off a Hoon call. 
This is to make this library as performant as possible on current Urbit runtimes, hopefully enabling new classes of applications. 

## Future work

The performance still suffers from the needless generation of mutated bytestreams: a change in 1 byte of a 100 Mb bytestream will result in two copies in memory until the next run of garbage collector, not to mention the very inefficient allocation patterns this entails. To avoid this problem, the runtime could provide a way to detect in-place noun mutation and pass this information to the jet.

