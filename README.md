#Lambda Calculators#

This repository provides reference implementaitons of the *pure*
lambda-calculus in three styles, namely higher-order abstract syntax (HOAS),
de-Bruijn indices (DBI), indexed names and named indices (INNI).  Each
implements *8* normalization strategies.

Currently, a REPL is provided for the INNI-style implementation.  To try it,
enter the `INNI` directory and compile the `Main.hs` file using GHC.

```
$ cd INNI
$ ghc -o repl Main.hs
```

Run the `repl`,

```
$ ./repl
```

and you will see the following prompt:

```
LC>
```

Below is an example interactive session:

```
LC> (<- (<- (-> m n f x (<- m f (<- n f x))) (-> f x (<- f x))) (-> f x (<- f (<- f x))))
(-> f x (<- f (<- f (<- f x))))
```

The syntax for lambda-expressions is a little different.  It can be easily
translated into the more traditional syntax.  For example,

```
(<- (-> x y x) y z)
```

will translate to

```
(\x.\y.x) y z
```

So `<-` means application, and `->` means abstraction.  Note that the last
sub-expression in an abstraction is the body of the function.  So the last `x`
in `(-> x y x)` is the funciton body.

The input expression in the above interactive session corresponds to

```
(\m.\n.\f.\x.m f (n f x)) (\f.\x.f x) (\f.\x.f (f x))
```

which means adding the Church-numeral one and two.  As expected, the
Church-numeral three was returned, because by default the REPL uses the
**normal-order** (_non_) normalization strategy.  Other strategies, including
**call-by-name** (_bnn_), **applicative-order** (_aon_), **call-by-value**
(_bvn_), **hybrid-applicative-order** (_han_), **head-spine** (_hsn_),
**head** (_hdn_), and **hybrid-normal-order** (_hnn_) can be chosen on the fly
by means of the built-in `:set` command of the REPL.

```
LC> :set bnn
```

After issuing this command in the REPL, the call-by-name normalization
strategy is chosen.  Retyping in the above expression will not result the
Church-numeral three because call-by-name does not normalize under lambda.

```
LC> (<- (<- (-> m n f x (<- m f (<- n f x))) (-> f x (<- f x))) (-> f x (<- f (<- f x))))                                                                                     
(-> f x (<- (-> f x (<- f x)) f (<- (-> f x (<- f (<- f x))) f x)))
```

