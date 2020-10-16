---
title: "Atom Shell をビルドする"
date: 2014-05-26T23:54:50+09:00
draft: false
tags: ["JavaScript"]
---
GitHub が最近オープンソースで公開した [Atom](https://atom.io) というエディタが話題になっているが、それに使われているフレームワークが [Atom Shell](https://github.com/atom/atom-shell) というものらしい。GitHub の README.md によると JavaScript や HTML や CSS といった Web テクノロジでクロスプラットフォームなデスクトップアプリケーションが作れるフレームワークなのだそうだ。以前から Microsoft Windows でも HTA (HTML Application) という HTML + JScript/VBScript でデスクトップアプリケーションが作れる技術があったが、あのノリなのかもしれない。もっとも HTA は Internet Explorer 限定だったけど。

試しにソースコードを clone してビルドしてみる。とはいっても[ドキュメント](https://github.com/atom/atom-shell/tree/master/docs)の通りに進めるだけだった。まずはソースツリーを clone する。

``` console
$ git clone https://github.com/atom/atom-shell.git
$ cd atom-shell
```

ビルドに必要なものはすべて bootstrap.py を実行することでそろえてくれるようだ。

``` console
$ ./script/bootstrap.py
```

あとは build.py を実行するだけ。

``` console
$ ./script/build.py
```

最後に test.py を実行して完了！

``` console
$ cd ./script/test.py
```

out/Release/Atom.app/Contents/MacOS/Atom に実行ファイルが生成される。
