---
title: "Octopress の記事の日付を更新するスクリプト"
date: 2014-05-16T23:53:58+09:00
draft: false
tags: ["Lisp"]
---
Octopress で rake new_post して新しい記事を作成すると、作成日がファイル名になり、ファイル中の date: にも作成した時点の日時が記録される。この日付は作成日ではなくどちらかというと deploy する日にしたいので、指定したファイル名とファイル中の date: を最新に更新するスクリプトを書いた。(SBCL 用)

{% gist e98f96e8b2f7673ce741 %}

実行方法は以下の通り。

``` console
$ sbcl --script update-md.lisp 2014-05-14-hoge.markdown
```
