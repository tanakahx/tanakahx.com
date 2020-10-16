---
title: "Wordbank - 英和辞書用コマンドラインインタフェース"
date: 2014-08-18T12:52:43+09:00
draft: false
tags: ["Lisp"]
---
Common Lisp で何かプログラムを作ろうと思い立ち、[Wordbank](https://github.com/tanakahx/wordbank) というものを作りました。これはコマンドプロンプトに入力した英単語の意味を表示するだけのプログラムですが、辞書で引いた単語の履歴を覚えておいて、後で復習できるような目的で作成しています(履歴機能は未実装)。こだわったところは、SLIME 上でコンパイルしたり Quicklisp でロードできるようにしつつ、出来上がった Lisp のソースコードをそのままシェルからスクリプト実行できるようにした点です。複数ファイルから構成される場合にスクリプト実行する方法はもう少し工夫する必要がありそうです。

問題の辞書ですが、[デ辞蔵 REST版API](https://dejizo.jp/dev/index.html) という Web サービス があったので利用させて頂きました。現状、API の動作確認用プロトタイプという段階であり、クエリを投げて結果を表示することしかできませんが、引き続き履歴機能の実装と検索機能の拡充を進めていく予定です。

ちなみに HTTP リクエストには [Drakma](http://weitz.de/drakma/)、検索結果からの情報抽出には [CL-PPCRE](http://weitz.de/cl-ppcre/) という正規表現ライブラリを利用しました。Common Lisp では package.lisp にパッケージ情報やら asdf ファイルに依存関係やらを書くことが多いようですが、[CL-Project](https://github.com/fukamachi/cl-project) というスケルトン生成ツールはユニットテストの設定もしてくれるので便利でした。同様のツールで [Quickproject](http://www.xach.com/lisp/quickproject/) がありますが、こちらはユニットテストの設定はしないようです。
