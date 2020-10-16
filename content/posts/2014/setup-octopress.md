---
title: "Setting up Octopress"
date: 2014-05-08T08:59:26+09:00
draft: false
tags: [""]
---
本ブログの構築するにあたり [Octopress](http://octopress.org/ "Octopress") を使用してみた。Octopress は静的サイトジェネレータである [Jekyll](http://github.com/mojombo/jekyll "Jekyll") のためのフレームワークらしい。Jekyll 単体でもサイト構築は可能であるが、サイトの生成からデプロイまでをすべて自分で行う必要がある。一方、Octopress を使うとこれらをすべて自動化してくれるため非常に手軽にサイトを生成することができる。さらに、Jekyll ベースのサイトジェネレータなので、ブラウザ上のエディタや独自マークアップ記法を使う必要はなく、オフラインで Markdown 記法を使って記事の編集を行い、ローカルホスト上の Web サーバを通してブラウザでの確認ができるため非常に便利。

以下は Octopress を使ったサイト生成環境を構築した際のメモ。環境は Mac OS X 10.9.2。設定にあたり、以下の web サイトが参考になった。

* [Octopress Documentation](http://octopress.org/docs/"Octopress Documentation")
* [Jekyllいつやるの？ジキやルの？今でしょ！](http://melborne.github.io/2013/05/20/now-the-time-to-start-jekyll/ "Jekyllいつやるの？ジキやルの？今でしょ！")

## Octopress の設定
まずは GitHub から clone する。

``` console
$ git clone https://github.com/imathis/octopress.git
$ cd octopress
```

次に、bundler という Ruby アプリケーション管理ツールを入れる。

``` console
$ gem install bundler
$ bundle install
```

以下を実行して Octopress のデフォルトテーマ (classic) をインストールする。

``` console
$ rake install
```

ここで以下のエラーが起きた。

``` console
$ rake install
rake aborted!
You have already activated rake 0.9.6, but your Gemfile requires rake 0.9.2.2. Prepending `bundle exec` to your command may solve this.
```

rake のバージョンがよくないようなので、Gemfile.lock ファイルを以下のように書き換えたところ rake install が動作した。

```
diff --git a/Gemfile.lock b/Gemfile.lock
index 182c30b..f487980 100644
--- a/Gemfile.lock
+++ b/Gemfile.lock
@@ -31,7 +31,7 @@ GEM
     rack (1.5.2)
     rack-protection (1.5.0)
       rack
-    rake (0.9.2.2)
+    rake (0.9.6)
     rb-fsevent (0.9.1)
     rdiscount (2.0.7.3)
     rubypants (0.2.0)
```

以上で Octopress の設定は完了したので、次のコマンドでサイトを生成することができる。

``` console
$ rake generate
```

生成したサイトの確認方法は以下の通り。

``` console
$ rake preview
```

WEBrick という web サーバがローカルホストで起動するので、ブラウザで http://localhost:4000 にアクセスすると生成したサイトを表示することができる(まだ記事を書いていないので内容は何もないが)。なお、rake preview はサイト内容に変更があった場合も自動的に検出するので、rake generate の度に再起動する必要はない。

新規に "Sample Post" というタイトルの記事を書くには次を実行する。

``` console
$ rake 'new_post[Sample Post]'
```

指定したタイトルはファイル名に変換されて source/_posts/2014-05-07-sample-post.markdown というファイルが生成される。記事の内容は Markdown 記法を用いて編集する。rake generate でサイトを生成し直すとブラウザで新しい記事がポストされていることが確認できる。なお、記事のファイル名によって URL が決定され、この例の場合のパーマリンクは、http://tanakahx.github.io/blog/2014/05/07/sample-post/index.html になる。

## GitHub Pages との連携

生成したサイトは public ディレクトリにはかれるのでこれをそのままホスティングすればサイトの完成であるが、GitHub Pages との連携も可能であり、同時に Git を使ったバージョン管理もできることから GitHub Pages と連携する方が一般的なのかも。というわけで以下に設定方法を示す。

``` console
$ rake setup_github_pages
Enter the read/write url for your repository
(For example, 'git@github.com:your_username/your_username.github.io.git)
           or 'https://github.com/your_username/your_username.github.io')
Repository url: git@github.com:tanakahx/tanakahx.github.io.git
```

GitHub のリポジトリに push する準備が整ったので次を実行して deploy する。

```console
$ rake deploy
```
