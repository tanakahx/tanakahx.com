---
title: "ビンゴカード作成問題 (Common Lisp)"
date: 2015-03-07T14:07:37+09:00
draft: false
tags: ["Lisp"]
---
[ビンゴカード作成問題](http://blog.jnito.com/entry/2015/03/06/090106)のリファクタリング動画をみて、Rubyは多彩な機能が言語の組み込みライブラリとしてそろってて便利そうだなと思った。というわけで自分もプログラムを書いてみた。Common Lispで。

```cl
(ql:quickload :alexandria)
(use-package :alexandria)

(defun generate-card ()
  (let ((cards (mapcar (lambda (lst)
                         (mapcar (lambda (x)
                                   (format nil "~2d" x)) lst))
                       (mapcar (lambda (x)
                                 (subseq (shuffle (iota 16 :start (* x 16)))
                                         0 5))
                               (iota 5)))))
    ;; 行列の中央はブランクにする
    (setf (caddr (caddr cards)) "  ")
    ;; ヘッダを表示
    (format t "~{ ~a~^|~}~%" (coerce "BINGO" 'list))
    ;; 行列を表示
    (format t "~{~{~a~^|~}~%~}" (apply #'mapcar #'list cards))))
```

```console 実行結果
 B| I| N| G| O
12|17|47|50|67
 7|20|33|53|76
11|25|  |54|78
15|29|32|60|69
 9|22|42|61|77
```

Alexandriaを使っているとか気にしてはいけない。もっと短く書けそうな気もする。
