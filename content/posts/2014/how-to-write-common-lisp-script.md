---
title: "Common Lisp (SBCL) でスクリプトを書く方法"
date: 2014-08-02T00:28:06+09:00
draft: false
tags: ["Lisp"]
---
Common Lisp (SBCL) でスクリプトを書くいい方法が見つからなかったので調べてみた。重要なのは次の三点。

- Quicklisp をロードしたイメージファイルの生成
- シェルスクリプト shebang からの exec sbcl ハック
- scripted main はシェルスクリプトに与えた $0 から判定する

順を追って説明します。

## Quicklisp をロードしたイメージファイルの生成
スクリプト実行するたびに Quicklisp を実行しているとロードに時間がかかるので、あらかじめ Quicklisp をロードした状態のイメージファイル (以下の例では sbcl-base.core という名前のイメージファイル) を生成し、スクリプト実行時に指定する。

``` console
$ sbcl                                                                                                                                                                                                               [~/dev/tmp]
This is SBCL 1.2.1, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (sb-ext:save-lisp-and-die "sbcl-base.core" :executable t)
[undoing binding stack and other enclosing state... done]
[saving current Lisp image into sbcl-base.core:
writing 5744 bytes from the read-only space at 0x0x20000000
writing 3120 bytes from the static space at 0x0x20100000
writing 53903360 bytes from the dynamic space at 0x0x1000000000
done]
```

イメージファイルを指定して sbcl を起動するには --core オプションに指定すればよい。

## シェルスクリプト shebang からの exec sbcl ハック
sbcl を直接 shebang で実行するのではなく /bin/sh 経由で sbcl に exec する。`--core` オプションは先ほど生成した Quicklisp をロード済みのイメージファイルで起動するために指定している。

``` cl
#!/bin/sh
#|
exec sbcl --core sbcl-base.core --script $0 $0 "$@"
|#
;;; code comes here
```

このとき、最初の `$0` はこのファイル自身をスクリプト実行するために与えている。2 番目の `$0` は後述する scripted main であることを判定するために使う。最後の `"$@"` は残りの全引数を sbcl に与えている。

2 行目の `#|` は `#` で始まるためシェルスクリプトからはコメント行に解釈される。一方、Common Lisp からは `#|` から `|#` まではコメントであると見なされるためスクリプト実行時は無視される。ただし、最初の shebang の行のみは Common Lisp の文法では解釈できないため、.sbclrc に読み飛ばす設定をする。

``` cl .sbclrc
(set-dispatch-macro-character #\# #\!
  (lambda (stream character n)
    (declare (ignore character n))
    (read-line stream nil nil t)
    nil))
```

## scripted main はシェルスクリプトに与えた $0 から判定する
scripted main とはスクリプトとして実行された際のエントリポイントを表す造語。例えば、Python ならば

``` python
def main():
    # ...
main() # call main function directly
```

のように実際の処理をべた書きしてしまうと、このスクリプトをモジュールとして外部に公開するときに、import するたびに main() が実行されてしまい困ることになる。そこで、

``` python
if __name__ == '__main__':
    main() # scripted main
```

のように `__name__` 変数でスクリプト実行かどうかを条件判定すれば、このモジュールをスタンドアロンなスクリプトとして実行するときは main() が実行され、外部から import される場合には main() は実行されずに必要な機能のみ読み込ませることが可能になる。

これと同じことを SBCL で実現するにはコマンド実行時のみコマンドライン引数がセットされる `sb-ext:*posix-argv*` を使い、`(pathname-name *load-truename*)` で与えられる文字列が見つかればスクリプト実行なので scripted main を実行、見つからなければ REPL や外部ファイルから load されたものとして scripted main は実行しない、というように見分ける作戦にする。

``` cl
;; scripted main
(when (member (pathname-name *load-truename*)
              sb-ext:*posix-argv*
              :test #'(lambda (x y) (search x y :test #'equalp)))
  (main))
```

しかし、普通に `sbcl --script file <filename>` のようにファイルを指定して実行しただけでは `<filename>` という文字列は `sb-ext:*posix-argv*` に含まれないという問題がある。そこで、sbcl を exec するときに `--script $0` の後に重ねて `$0` を指定することで、shebang から起動された場合は `sb-ext:*posix-argv*` の第二要素に必ずスクリプトのファイル名を入れることにする。(第一要素は `sbcl` になる)

以上をまとめるとスクリプト全体は次のようになる。

``` cl myscript.lisp
#!/bin/sh
#|
exec sbcl --core sbcl-base.core --script $0 $0 "$@"
|#

(let* ((*standard-output* (make-broadcast-stream))
       (*error-output* *standard-output*))
  (ql:quickload "split-sequence"))

(in-package :cl-user)
(defpackage :myscript
  (:use #:cl #:split-sequence))
(in-package :myscript)

;;; code comes here

(defun main (argv)
  (format t "~a~%" (first argv))
  (format t "~s~%" (split-sequence #\, (first argv))))

;; scripted main
(when (member (pathname-name *load-truename*)
              sb-ext:*posix-argv*
              :test #'(lambda (x y) (search x y :test #'equalp)))
  (main (cddr sb-ext:*posix-argv*)))
```

コマンドライン引数で渡した文字列を `split-sequence` を使って `,` でリストに分割して表示するだけのスクリプト。`split-sequence` は Quicklisp からロードしているが、ロード中のメッセージを抑制するために、`*standard-output*` と `*error-output*` の束縛を一時的に変更して端末に出力されないようにしている。

``` console
$ ./myscript.lisp "hello, world"
hello, world
("hello" " world")
```

参考

[Writing Scripts with Common Lisp](http://speely.wordpress.com/2010/11/27/writing-scripts-with-common-lisp/)
