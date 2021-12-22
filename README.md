# seedCracker

You can install dependencies, build and run using `stack run`.

To record performance measurements, use `stack exec --` ..., eg:
```
stack exec -- Cracker-exe vals.txt parList 128 +RTS -N8 -s -ls
```

A sample `vals.txt` is provided. It should contain newline-separated chunk coordinates, with the coordinates themselves being space-separated, with the x-coordinate first and the z-coordinate second. An additional `vals2.txt` provides a slightly more computationally difficult case.

We performed our tests manually. To replicate our testing, please vary the testing using the following parameters:
```
stack exec -- Cracker-exe <filename> (seq|parList|parBuffer|parMonad) <num-blocks> [naive] +RTS -N<num-threads> -s -ls
```
We ranged `<num-blocks>` on powers of 2 from 1 to 65,536.