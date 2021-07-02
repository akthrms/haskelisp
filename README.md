# Haskelisp

[48 時間で Scheme を書こう - Wikibooks](https://ja.wikibooks.org/wiki/48%E6%99%82%E9%96%93%E3%81%A7Scheme%E3%82%92%E6%9B%B8%E3%81%93%E3%81%86)

## Usage

```
$ cabal v2-run

Lisp>>>(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))
(lambda ("x") .. )
Lisp>>>(factorial 10)
3628800
Lisp>>>(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))
(lambda ("inc") .. )
Lisp>>>(define my-count (counter 5))
(lambda ("x") .. )
Lisp>>>(my-count 3)
8
Lisp>>>(my-count 6)
14
Lisp>>>quit
```
