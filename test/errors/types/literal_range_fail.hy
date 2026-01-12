// Range checking for literal narrowing (assignments and casts).

byte badByte = 256            // Error: 256 exceeds byte range [0-255]
sbyte badSbyte = 128          // Error: 128 exceeds sbyte range [-128-127]
short badShort = 100000       // Error: 100000 exceeds short range [-32768-32767]

byte castByte = byte: 256     // Error: 256 exceeds byte range [0-255]
