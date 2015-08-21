# Array-based collections

Array-based immutable collections for scala. These collections use spire typeclasses such as Eq and Order instead of relying on the equals method of the element objects, which sometimes does not work (e.g. Array[Byte]).

They also are effectively specialized, so e.g. an ArraySet[Byte] will use an Array[Byte] internally instead of boxing like normal scala collections do.

## RadixTree

A fast immutable radix tree. Very useful for large string sets or maps. [RadixTree](RadixTree.md)
