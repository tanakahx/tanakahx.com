---
title: "スクリプト実行を効率化するための SBCL コアイメージの作成方法"
date: 2014-05-10T23:23:02+09:00
draft: false
tags: ["Lisp"]
---
Common Lisp をスクリプト実行していて困るのが Quicklisp を使ったライブラリのロード方法。SLIME などの REPL 上で実行しているなら一度 ql:quickload してあげれば問題ないが、ターミナルからスクリプト実行する場合は毎回 ql:quickload が走るため時間がかかり効率が良くない。

```cl
(load "~/quicklisp/setup.lisp")
(ql:quickload "getopt")

(use-package :getopt)
;; Codes using getopt package continue below
```

そこで、あらかじめ必要なライブラリをロードした状態の SBCL のコアイメージを生成して、その上でスクリプトを実行する方法を調べてみた。SBCL には [sb-ext:save-lisp-and-die](http://www.sbcl.org/manual/index.html#Function-sb_002dext_003asave_002dlisp_002dand_002ddie sb-ext:save-lisp-and-die) という関数でコアイメージを生成することができるので、以下のようなコアイメージ生成用のスクリプトを実行して sbcl-script という実行可能形式のファイルを生成する。

``` cl
(load "~/quicklisp/setup.lisp")
(ql:quickload "getopt")
(sb-ext:save-lisp-and-die "sbcl-script" :executable t)
```

``` console
$ sbcl --script sbcl-script.lisp
```

sbcl-script を生成した結果、ファイルサイズは 55509040 byte だった。結構でかい...。

sbcl-script で実行するスクリプトはもはや ql:quickload する必要はないので、ライブラリを使用したコードを書いて気軽にスクリプト実行することができる。

```cl
(use-package :getopt)
;; Codes using getopt package continue below
```
