# seedCracker

You can install dependencies, build and run using `stack run`.

To record performance measurements, use `stack exec --` ..., eg:
```
stack exec -- Cracker-exe vals.txt parList 128 +RTS -N8 -s -ls
```