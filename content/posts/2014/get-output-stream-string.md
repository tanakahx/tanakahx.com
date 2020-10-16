---
title: "get-output-stream-string"
date: 2014-08-11T15:55:12+09:00
draft: false
tags: ["Lisp"]
---
get-output-stream-string は指定したストリームに出力された文字を出力順のまま文字列として返す関数である。関数が実行される際に、ストリーム中のすべての文字はクリアされる。Hyperspec の get-output-stream-string によると、 make-string-output-stream *以外*で生成したストリームに関しての動作は未定義とされている。Practical Common Lisp のサンプルプログラムに with-output-to-string で生成したストリームから get-output-stream-string で文字列として取り出しているコードがあるが、言語仕様上はよろしくないようだ。
