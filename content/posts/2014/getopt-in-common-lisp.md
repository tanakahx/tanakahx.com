---
title: "Common Lisp で getopt ライブラリを使う"
date: 2014-05-10T16:51:02+09:00
draft: false
tags: ["Lisp"]
---
Common Lisp に getopt のようなライブラリがあるか調べてみたら、quicklisp に登録されていたので使い方を調べてみた。

``` cl getopt-test.lisp
(use-package :getopt)

(multiple-value-bind (out-args out-opts errors)
    (getopt (cdr sb-ext:*posix-argv*) '(("h" :NONE) ("f" :REQUIRED) ("b" :OPTIONAL "bar")))
  ;; Print each lists returned by getopt
  (print out-args)
  (print out-opts)
  (print errors)
  (fresh-line)

  ;; Handle illegal options
  (when errors
    (format t "Illegal options: ~{~a ~}~%" errors)
    (quit))

  ;; Print arguments and options
  (format t "Arguments are ")
  (format t "~{~a ~}~%" out-args)
  (dolist (opt out-opts)
    (cond ((equal (car opt) "h")
	   (format t "option h~%"))
	  ((equal (car opt) "f")
	   (format t "option f is ~a~%" (cdr opt)))
	  ((equal (car opt) "b")
	   (format t "option b is ~a~%" (cdr opt))))))
```

getopt には二つの引数を渡していて、一つ目はコマンドライン引数 (文字列のリスト)、二つ目はオプションリストを指定する。オプションリストは以下の 3 要素からなるリストのリストである。

* NAME: オプションにする文字列 (long name にも対応)
* HAS-ARG: :NONE (引数なし), :REQUIRED (引数あり), :OPTION (引数は任意) のいずれか
* VAL: :OPTION 指定のオプションに何も指定しなかった場合のデフォルト値

getopt の戻り値は、オプション以外の引数 (out-args)、オプションと引数のペアのリスト (out-opts)、エラー (errors)という多値で返ってくる。errors が Non-nil であればパースが正常に終了しているので、上記サンプルのように out-opts をループしながら cond で場合分けしてもいいし、連想リスト形式なので assoc してもいいと思う。

``` console
sbcl --script getopt-test.lisp -f foo -b -h abc xyz
```
``` plain 実行結果
("abc" "xyz")
(("f" . "foo") ("b" . "bar") ("h"))
NIL
Arguments are abc xyz
option f is foo
option b is bar
option h
```

なお、Common Lisp の ANSI 標準ではコマンドライン引数の扱いに関して規定していないので、取得方法は処理系依存になるようだ。特に、SBCL は sb-ext:\*posix-argv\*, CLISP は ext:\*args\* で取得できるので、本当は処理系の差を吸収するために \*features\* から実行時の処理系を判定したほうがいいかも。

``` cl
(defun args ()
  #+sbcl  (cdr sb-ext:*posix-argv*)
  #+clisp ext:*args*)
```
