---
title: "Python でファイルを読むとき"
date: 2014-05-20T23:30:42+09:00
draft: false
tags: ["Python"]
---
Python でファイルを読むときはいつも

``` python
f = open("file.txt")
line = f.readline()
while line:
    print line
    line = f.readline()
f.close()
```

のように書いていたけど、こんな書き方しなくても、

``` python
f = open("file.txt")
for line in f:
    print line
f.close()
```

これでよかったんだ...。効率は良くないかもしれないけど、もっと短く書ける。

``` python
for line in open("file.txt"):
    print line
```

でも Python 2.6 からは with 構文使うのがいいらしい。

``` python
with open("file.txt") as f:
    for line in f:
        print line
```

これなら with を抜けた後に自動的に f.close() されるようだ。まさに Lisp の with-open-file マクロだな。
