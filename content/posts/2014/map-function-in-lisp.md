---
title: "Lisp の map 関数"
date: 2014-05-18T00:51:57+09:00
draft: false
tags: ["Lisp"]
---
Lisp にはリスト操作の map* 関数が 6 種類あるが違いが微妙なのでまとめてみた。まずは基本形の mapcar/maplist を押さえておけばいい感じ。

## mapcar/maplist

mapcar は引数にとったリストを構成する各コンスセルの car 部に対して関数を適用し、その結果をリストとして返す。
``` cl
(mapcar #'(lambda (x) (* x 2)) '(1 2 3))
;; => (2 4 6)

(mapcar #'+ '(1 2 3) '(4 5 6))
;; => (5 7 9)
```

maplist は引数にとったリストを構成する各コンスセルの cdr 部に対して関数を適用し、その結果をリストとして返す。
``` cl
(maplist #'list '(1 2 3))
;; => (((1 2 3)) ((2 3)) ((3)))

(maplist #'list '(1 2 3) '(4 5 6))
;; => (((1 2 3) (4 5 6)) ((2 3) (5 6)) ((3) (6)))
```

## mapcan/mapcon

mapcan は mapcar 同様に、引数にとったリストを構成する各コンスセルの car 部に対して関数を適用するが、その結果がリストであることを期待して nconc したものを返す。
``` cl
(mapcan #'list '(1 2 3))
;; => (nconc '(1) '(2) '(3))
;; => (1 2 3)

(mapcan #'list '(1 2 3) '(4 5 6))
;; => (nconc '(1 4) '(2 5) '(3 6))
;; => (1 4 2 5 3 6)
```

mapcon は maplist 同様に、引数にとったリストを構成する各コンスセルの cdr 部に対して関数を適用するが、その結果がリストであることを期待して nconc したものを返す。
``` cl
(mapcon #'list '(1 2 3))
;; => (nconc '(1 2 3) '(2 3) '(3))
;; => ((1 2 3) (2 3) (3))

(mapcon #'list '(1 2 3) '(4 5 6))
;; => (nconc '((1 2 3) (4 5 6)) '((2 3) (5 6)) '((3) (6)))
;; => ((1 2 3) (4 5 6) (2 3) (5 6) (3) (6))
```

## mapc/mapl

mapc/mapl のリスト処理は mapcar/maplist と同様であるが、戻り値は一番目に与えたリストを返すという点で異なる。
``` cl
(mapc #'(lambda (x) (format t "This is ~a~%" x)) '(1 2 3))
;; This is 1
;; This is 2
;; This is 3
;; => (1 2 3)
(mapc #'(lambda (x y) (format t "~a + ~a = ~a ~%" x y (+ x y))) '(1 2 3) '(4 5 6))
;; 1 + 4 = 5
;; 2 + 5 = 7
;; 3 + 6 = 9
;; => (1 2 3)

(mapl #'(lambda (x) (format t "This is ~a~%" x)) '(1 2 3))
;; This is (1 2 3)
;; This is (2 3)
;; This is (3)
;; => (1 2 3)
(mapl #'(lambda (x y) (format t "append ~a and ~a => ~a~%" x y (append x y))) '(1 2 3) '(4 5 6))
;; append (1 2 3) and (4 5 6) => (1 2 3 4 5 6)
;; append (2 3) and (5 6) => (2 3 5 6)
;; append (3) and (6) => (3 6)
;; => (1 2 3)
```

## map/mapinto (おまけ)

map は一般的なシーケンス型に適用できる mapcar の汎用版。

``` cl
(map 'vector #'+ #(1 2 3) #(4 5 6))
;; => #(5 7 9)
(map 'list #'+ '(1 2 3) '(4 5 6))
;; => (5 7 9)
```

mapinto は mapcar の結果を第一引数に指定した変数に割り当てる。

``` cl
(map-into a #'+ '(1 2 3) '(4 5 6))
;; => (5 7 9)
a
;; => (5 7 9)
```
