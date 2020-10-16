---
title: "Atom Shell で Hello world"
date: 2014-06-02T23:24:57+09:00
draft: false
tags: ["JavaScript"]
---
Atom Shell を使っておそらく本来の使われ方ではないプログラムを作成してみる。以下のような Node.js プログラムを普通に実行することができる模様。

``` js hello.js
console.log("hello, world");
process.exit(0);
```

最後の process.exit(0) を書かないとプロセスが終了しない。 実行するときは引数に JavaScript ファイルを指定すればよい。

``` console 実行結果
$ ./out/Release/Atom.app/Contents/MacOS/Atom hello.js
hello, world
```

require でロードしたモジュールも普通に使うことができる。

``` js cpus.js
var os = require('os');
console.log(os.cpus());
process.exit(0);
```

``` console 実行結果
$ ./out/Release/Atom.app/Contents/MacOS/Atom cpus.js
[ { model: 'Intel(R) Core(TM) i5-3317U CPU @ 1.70GHz',
    speed: 1700,
    times: { user: 4692350, nice: 0, sys: 3321620, idle: 38701300, irq: 0 } },
  { model: 'Intel(R) Core(TM) i5-3317U CPU @ 1.70GHz',
    speed: 1700,
    times: { user: 2480960, nice: 0, sys: 1154690, idle: 43077500, irq: 0 } },
  { model: 'Intel(R) Core(TM) i5-3317U CPU @ 1.70GHz',
    speed: 1700,
    times: { user: 4196740, nice: 0, sys: 2200760, idle: 40315740, irq: 0 } },
  { model: 'Intel(R) Core(TM) i5-3317U CPU @ 1.70GHz',
    speed: 1700,
    times: { user: 2333780, nice: 0, sys: 1036880, idle: 43342400, irq: 0 } } ]
```

本来のデスクトップアプリケーションを目的としたプログラムのチュートリアルは下記ドキュメントにある。

* [Quick start](https://github.com/atom/atom-shell/blob/master/docs/tutorial/quick-start.md)

ちょっと試してみたけど、main.js から HTML の DOM を操作することはできないんだろうか。よくわからない...。
