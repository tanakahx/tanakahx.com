---
title: "cl-launch"
date: 2015-04-05T16:01:18+09:00
draft: false
tags: ["Lisp"]
---
cl-launch は Common Lisp をスクリプト実行したり実行可能形式ファイルを出力するためのシェルラッパーらしい。Common Lisp は REPL  上でプログラムを実行する前提になっており、他の言語のように気軽にスクリプトを実行できなかったり、コンパイルして実行可能形式ファイルを生成しづらいと感じていたので、cl-launch の使い方を調べてみた。

## cl-launch のインストール方法

cl-launch_4.1.2.orig.tar.gz を伸張したディレクトリに `cd` して `make install` を実行すると `/usr/local/bin/cl` にインストールされる。インストール先を変更するには、`make install PREFIX=somewhere` のように PREFIX を指定すればよい。

## スクリプト実行方法

まず始めに次のような hello, world を表示するスクリプトを作成する。

```cl hello.lisp
#!/usr/local/bin/cl -l sbcl -E main

(defun main (argv)
  (declare (ignore argv))
  (format t "hello, world!~%"))
```

shebang (#!) で cl-launch を実行しているが、そのときに与えるオプションとして以下を指定している。
- `-l` Common Lisp 処理形を指定する。(sbcl, ccl, clisp, etc...)
- `-E` エントリポイントとして実行する関数名（指定しない場合は何も実行されないことに注意）
エントリポイントの関数はコマンドライン引数 (argv) が第一引数として与えられて実行されるので、引数をとる関数として定義する必要がある。(この例では argv を使用しないので ignore している。)

``` console 実行方法
$ chmod +x hellp.lisp
$ ./hello.lisp
```

## コマンドライン引数 (argv) を受け取る方法

コマンドライン引数は先ほどの `hello.lisp` のようにエントリポイントになる関数に第一引数としてリスト形式で渡されるため、そのままリストとして扱えばよい。ただし、argv[0] は含まれないため、代わりに `(uiop:argv0)` を実行することで得ることが出来る。

```cl sample.lisp
#!/usr/local/bin/cl -l sbcl -E main

(defun main (argv)
  (format t "~a~%" (uiop:argv0))
  (format t "~{~a~%~}" argv))
```

``` console 実行方法
$ chmod +x sample.lisp
$ ./sample.lisp a b c
./sample.lisp
a
b
c
$
```

## Quicklisp を使う方法

cl-launch には Quicklisp を使って外部ライブラリの読み込みをサポートしている。以下は有名な CL-PPCRE を利用する例。

```cl sample.lisp
#!/usr/local/bin/cl -l sbcl -Q -s cl-ppcre -E main

(defun main (argv)
  (format t "cl-ppcre: ~a~%" (cl-ppcre:scan "abc" "abxabcxab")))
```

`-Q` で Quicklisp を使うように指定し、`-s` でロードするシステム(ここでは cl-ppcre)を指定している。

## 実行可能形式ファイルを生成する方法

cl-launch は実行可能形式ファイルの生成もサポートしている。とは言っても、よくある Lisp のイメージファイルを save-lisp-and-die で吐き出す方式のため、数十 MB くらいのファイルになってしまう。ファイルサイズを減らす方法はあるんだろうか。あまりこの辺の情報がないので、最近は多少ファイルサイズが大きくなってしまっても仕方ないものと思ってたりする。大人になるってのはそういうことらしい。

先ほどのスクリプトから直接、実行可能形式に変換することが出来る。スクリプト実行した際に記述した shebang(#!) で始まる行は、それをリーダマクロで無視するコードが自動的に組み込まれるらしいので、わざわざ削除する必要はない。

```console
$ cl -l sbcl -Q -s cl-ppcre --file sample.lisp --dump '!' --output sample -E main
[undoing binding stack and other enclosing state... done]
[saving current Lisp image into /Users/tanaka/work/sample:
writing 5904 bytes from the read-only space at 0x20000000
writing 3168 bytes from the static space at 0x20100000
writing 58785792 bytes from the dynamic space at 0x1000000000
done]
$ ./sample
cl-ppcre: 3
$
```
