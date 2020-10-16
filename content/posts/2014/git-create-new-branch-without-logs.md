---
title: "Git の履歴を削除して新しいブランチを作成する方法"
date: 2014-06-07T15:06:33+09:00
draft: false
tags: ["Git"]
---
Git の履歴をすべて削除して新しいブランチからリポジトリ管理を始めたい場合、まずは以下のように git checkout コマンドに --orphan オプションをつけて空のブランチを作成する。

``` console
$ git checkout --orphan tmp
```

この状態で、いつものように Git 管理下におきたいファイルをステージングして commit する。何も commit していない状態だと git branch を実行しても tmp ブランチは表示されないようだ。

``` console
$ git add .
$ git commit -m "first commmit"
```

この時点で master と履歴が 1 つの tmp というブランチが存在することになる。あとは master ブランチを tmp ブランチで push して履歴をすべて削除した master ブランチに改変する。

``` console
$ push -f . tmp:master
```

## 別のリモートリポジトリに push

GitHub から clone したリポジトリを履歴を削除した上で BitBucket で新たに管理を開始したい場合などは、まず上記の手順で履歴を削除したリポジトリを作成した後、以下のように origin の url を変更する。

``` console
$ git checkout master # カレントブランチを master ブランチに戻す
$ git remote set_url origin git@bitbecket.com:username/repository.git
```

あとはリモートリポジトリに対して push すればよい。

``` console
$ git push origin master
```

最初に作成した orphan なリポジトリはもはや不要なので削除してよい。

``` console
$ git branch -d tmp
```
