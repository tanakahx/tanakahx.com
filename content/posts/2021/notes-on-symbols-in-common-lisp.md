---
title: "Notes on Symbols in Common Lisp"
date: 2021-05-30T20:14:47+09:00
draft: false
---

Common Lisp で、あるパッケージ内でハッシュテーブルを作り、そのハッシュテーブルを export して、別のパッケージから参照したかった。下記のイメージ。

```common-lisp
CL-USER> (defpackage :hoge (:use :cl) (:export :*aaa*))
#<PACKAGE "HOGE">
CL-USER> (in-package :hoge)
HOGE> (defparameter *aaa* (make-hash-table))
*AAA*
HOGE> (setf (gethash 'name *aaa*) "ABC")
"ABC"
HOGE> (gethash 'name *aaa*)
"ABC"
T
```

hoge パッケージの中では何も問題ない。ところが、このハッシュテーブルを cl-user パッケージから参照すると、次のように期待した結果が得られない。

```common-lisp
HOGE> (in-package :cl-user)
#<PACKAGE "COMMON-LISP-USER">
CL-USER> hoge:*aaa*
#<HASH-TABLE :TEST EQL :COUNT 1 {10050CC5F3}> ; ハッシュテーブルは見えている。
CL-USER> (gethash 'name hoge:*aaa*)
NIL ; 値が取れていない。
NIL
```

理屈は単純で、hoge パッケージで使用した name というキーは hoge パッケージで定義されたシンボルであり、cl-user パッケージでキーとして与えたシンボル name とは実体が異なるからである。
以下のように、hoge パッケージの name を export した上で、それを指定すれば正しく値が取得できる。

```common-lisp
CL-USER> (defpackage :hoge (:use :cl) (:export :*aaa* :name))
#<PACKAGE "HOGE">
CL-USER> (gethash 'hoge:name hoge:*aaa*)
"ABC"
T
```
