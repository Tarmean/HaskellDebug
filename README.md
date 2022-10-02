# HaskellDebug


www.youtube.com/watch?v=9ATfLKosR-A

The Garbage Collection uses info tables to walk the heap and GHC can produce metadata like source locations for them.

This is a prototype which uses all that data to pry-style local debugging. Just throw in a `printValue` and you get dropped into a simple TUI debugger which gives source code and GHC's internal post-optimization AST for each thunk/function symbol. If you compiled with profiling, you also get a call stack for each thunk and constructor.

Currently does not support:

- Walking the runtime call stack, requires a newer GHC version
- Step-debugging. You can force thunks, but once a thunk is entered it runs until head normal form. GHCi-like step debugging would be nicer
- GHCi does not produce info-table-map entries, so no source locations in GHCi. GHCi does track source loc information for bytecode but it isn't accessible without a reference to the GHC session

Pretty rough around the edges as well. Because the prototype uses Brick it doesn't run on windows.

