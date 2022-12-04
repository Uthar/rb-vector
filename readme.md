# rb-vector

This is an immutable vector data structure in Common Lisp. It's implemented as a
radix balanced tree of width 32.

## WARNING

This library is __EXPERIMENTAL__ and everything can change

## use

Load:

```
(asdf:load-system :rb-vector)
```

Test:

```
(asdf:test-system :rb-vector)
```

Benchmark:

```
(asdf:load-system :rb-vector/bench)
```


## portability

 - ABCL
 - ACL
 - CCL
 - CLISP
 - Clasp
 - ECL
 - LispWorks
 - SBCL

## learning resources

- paper on RRB vectors https://dx.doi.org/10.1145/2784731.2784739
- talk by Mohit Thatte https://www.youtube.com/watch?v=7BFF50BHPPo
- Polymatheia blog https://hypirion.com/musings/understanding-persistent-vector-pt-1

## similiar projects

- [FSet](https://github.com/slburson/fset)
- [cloture](https://github.com/ruricolist/cloture)
- [cl-hamt](https://github.com/danshapero/cl-hamt)
- [persistent-vector](https://github.com/DanielKeogh/persistent-vector)

## license

GPLv3
