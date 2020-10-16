---
title: "Hexo"
date: 2015-01-06T00:34:37+09:00
draft: false
tags: ["Blog"]
---
今までブログはOctopressという静的サイトジェネレータを使っていたが、毎回サイトの生成に時間がかかるためなんとかしたいと思っていた。探した中で良さそうだったのが[Hexo](http://hexo.io)というNode.jsベースのジェネレータ。サイト構築までの手順も少なく、処理速度もそれなりに速くて快適だ。準備に必要なコマンドは、

```console
$ npm install -g hexo
$ hexo init blog
$ cd blog
$ npm install
$ hexo s
```

だけでよい。これだけで、http://localhost:4000 にアクセスしてプレビューが見れるようになる。記事を新規追加するには `hexo n` で引数に記事のタイトルを指定すればよく、コマンドもOctopressの`rake new_post["title"]`よりはだいぶ簡潔になっている。

構成もシンプルなのでテーマのカスタマイズもやりやすい。`_config.yml`の設定がグローバルなJavaScriptオブジェクトとして扱われ、EJSテンプレートから参照できるというコンセプトもシンプルでいいと思う。

まあ、deployするまでまだ作業することが残っているので、実際は上記の作業だけで終わりというわけにはいかないが、GitHub Pagesを使っている場合、設定ファイルにリポジトリを設定しておけば`hexo d`ですぐにdeployできる。

というわけでしばらくはHexoを使ってみます。
