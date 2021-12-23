# seedCracker

You can install dependencies, build and run using `stack run`.

To record performance measurements, use `stack exec --` ..., eg:
```
stack exec -- Cracker-exe vals.txt parList 128 +RTS -N8 -s -ls
```

A sample `vals.txt` is provided. An additional `vals2.txt` provides a slightly more computationally difficult case.

We performed our tests manually. To replicate our testing, please vary the testing using the following parameters:
```
stack exec -- Cracker-exe <filename> (seq|parList|parBuffer|parMonad) <num-blocks> [naive] +RTS -N<num-threads> -s -ls
```
The following is an explanation of the parameters:
- `<filename>`: Name of file containing chunk info. It should contain newline-separated chunk coordinates, with the coordinates themselves being space-separated, with the x-coordinate first and the z-coordinate second.
- `(seq|parList|parBuffer|parMonad)`: The implemented approaches to parallelizing the algorithm:
  - `seq`: Sequential implementation
  - `parList`: Parallel implementation using chunking, on the Strategy `parList rdeepseq`
  - `parBuffer`: Naive parallelization using the Strategy `parBuffer 100` which creates 2<sup>30</sup> sparks, one for each valid 30-bit seed
  - `parMonad`: Parallel implementation using the `Par` monad, using its `parMap` function
- `<num-blocks>`: Number of blocks to use, for implementations that use chunking. For our tests, we ranged `<num-blocks>` on powers of 2 from 1 to 65,536.
- `[naive]`: Enables the naive sequential algorithm which uses map and filter. If not specified, the recursive loop algorithm is used instead (except for in the `parBuffer` case).
- `<num-threads>`: Number of threads to use.