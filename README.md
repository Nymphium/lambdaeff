λeff
=== 

Toy language of λ calculus + Algebraic effects

```shell-session
$ stack exec lambdaeff-exe
λeff=> let double = inst () in let h = handler double (val x -> x) ((x, k) -> k (k x)) in with h handle (perform double 3) + 10
Int 23
λeff=> 
```

detailed: https://github.com/Nymphium/lambdaeff/blob/master/text/main.pdf

