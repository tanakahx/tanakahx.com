---
title: "ningler - Blog application sample using ningle"
date: 2014-09-13T23:31:09+09:00
draft: false
tags: ["Lisp"]
---
[ningle](http://8arrow.org/ningle/) という Common Lisp 用の Web アプリケーションフレームワークを使ってみたところ、非常にシンプルなインタフェースでわかりやすかったので、これを使ったサンプルとして [ningler](https://github.com/tanakahx/ningler) というブログアプリを作ってみました。Python の近いものとして [Flask](http://flask.pocoo.org/) というマイクロフレームを調べていたのですが、そのチュートリアルに Flaskr というサンプルがあったので、ningler はこれを ningle 用に移植したものになります。
テンプレートエンジンには CL-EMB、データベースは SQLite を使いました。データベース用の Common Lisp ライブラリは CL-DBI を使ってます。

こういった Web アプリケーションフレームワークは慣れていないので実装の一つ一つが手探り状態でした。特に ningle を使ってみて困った点は、ある URL から別の URL にリダイレクトするときにデータも一緒に引き継ぐ方法がなかった点と、セッション管理の正当な方法がわからなかった点です。前者は Flask の flash に相当する機能で、リダイレクト後にリプライするデータをバッファに積んでおき、実際にリダイレクト先にリクエストがきたときにバッファから取り出してリプライする、という使い方を想定したもので、Flask ではフレームワークとテンプレートで連携して実現できるようになってます。
一方、ningle にはそのような連携機能はないため、今回作った ningler ではフレームワーク側に文字列をバッファしておき、リダイレクト先にリクエストがきたらバッファから取り出して CL-EMB のテンプレートに env 経由で渡してあげるという方法をとりました。後者に関しては、`clack:clackup` するときに [Clack](http://clacklisp.org/) の `<clack-middleware-session>` を指定した上で、`(ningle:context :session)` で取得できるデータにログイン状態を記録することで実現しました。これは正しい方法なのかいまいちわかってません。

また、CL-EMB が他のテンプレートを埋め込む機能がなかった点に関しては、埋め込む部分を `<% @var body %>` としておき、他のテンプレートを execute-emb した結果を env から body に渡して埋め込みました。

こんな感じで色々ケアする部分はあるようですが、複雑なフレームワークを覚えるよりは ningle のようなシンプルなフレームワーク上に色々組み合わせて構築していく方が小回りがきくため、プロトタイピングに向いていると感じました。それにしても [nitro_idiot](http://8arrow.org/) さんはこのような便利な Common Lisp ライブラリをいくつも精力的に開発されていてすごいっす。

参考

- [ningle](http://8arrow.org/ningle/)
- [Flask](http://flask.pocoo.org/)
- [Clack](http://clacklisp.org/)
- [Caveman](http://8arrow.org/caveman/)
- [CL-DBI](http://8arrow.org/cl-dbi/)
- [CL-EMB](http://www.common-lisp.net/project/cl-emb/)
