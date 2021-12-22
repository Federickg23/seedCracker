# seedCracker

You can install dependencies, build and run using `stack run`.

To record performance measurements, use `stack exec --` ..., eg:
```
stack exec -- Cracker-exe vals.txt parList 128 +RTS -N8 -s -ls
```

A sample `vals.txt` is provided. It should contain newline-separated chunk coordinates, with the coordinates themselves being space-separated, with the x-coordinate first and the z-coordinate second.