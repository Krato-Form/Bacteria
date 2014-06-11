Bacteria
========

Random-ish generative figures

Getting started
---------------
- Use `quicklisp` to load dependencies, e.g. `(ql:quickload :rutils.list)`
- Compile the file
- Evaluate `(baktoo:start)`

Interaction
---------------
Pretty simple for now, just click on the screen to begin a new "colony".

Caveats
----------
- You might want to tweak `*step-delay*` to make it go faster or slower
- Highly inefficient right now, works for ~10K to ~12K nodes
- Prints the time taken to evaluate each step, you can get rid of this if you like an uncluttered Repl
- Runs in a single thread (If (when) I get around to a more interesting simulation, I'll add helpful evaluation functions to examine the state of the computation. Until then, I don't see the point of running separate threads. Yes, this means you need to kill it to start over)
- Only tested in SBCL on Linux ~~(though hopefully might work on Clozure on OSX too)~~