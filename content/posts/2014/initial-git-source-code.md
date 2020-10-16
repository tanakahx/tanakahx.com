---
title: "初期の Git コマンドのソースコード"
date: 2014-06-08T16:38:46+09:00
draft: false
tags: ["Git"]
---
Linus さんによる Git の最初のコミットとなる e83c5163 のソースコードを読んでみた。

## init-db

.dircache という名前でディレクトリを作成し、その中に 00 ~ FF のディレクトリを作成する。Git はこのディレクトリをコンテンツ管理用の DB として扱い、ファイルのコンテンツから求めた SHA-1 ハッシュ値をファイル名としてこのディレクトリに保存する。保存するディレクトリは SHA-1 ハッシュ値の先頭 1 バイトから保存すべき 00 ~ FF のディレクトリを識別し、そのディレクトリの中に SHA-1 ハッシュ値の残りの 19 バイトからなるファイル名で保存する。このようにインデクシングする理由は、.dircache はそれが置かれているディレクトリ以下のすべてのファイルを管理するため、ファイル数が多くなった場合に対処しているのだろう (という予想)。

データの基本的なフォーマットは以下の通り。

* blob データサイズ\0データ
* tree データサイズ\0データ
* commit データサイズ\0データ

"データ" の部分はそれぞれの形式毎に変わる。

## update-cache

指定したファイルを `blob データサイズ\0データ` という形式に変換し、それを zlib で deflate した結果を .dircache/objects 以下の DB に保存する。このときのファイル名は deflate 後の結果から SHA-1 ハッシュ値を求めたものになる。

さらに、登録するファイル情報を .dircache/index にキャッシュエントリとして保存する。.dirchache/index の構造は単純で、1 つのキャッシュヘッダとそれに続くキャッシュエントリのリストから構成される。キャッシュエントリは fstat から求めたファイル情報と deflate 結果から求めた SHA-1 ハッシュ値を含んでおり、ファイル名でソートされている。また、キャッシュヘッダにもすべてのキャッシュエントリのリストから算出した SHA-1 ハッシュ値を保存する。

## write-tree

キャッシュエントリを走査し、`tree データサイズ\0ファイルモード ファイル名\0SHA-1...` という形式のデータを生成する。update-cache でファイルを blob 形式で DB に保存したのと同様に、この tree 形式を zlib で deflate して、その結果から求めた SHA-1 ハッシュ値をファイル名として DB に保存する。このときのハッシュ値は commit-tree で使う。

## commit-tree

引数で指定した SHA-1 ハッシュ値の tree を commit する。コミットメッセージは標準入力から取得する。形式は以下の通り。

`commit データサイズ\0tree SHA-1\nparent SHA-1\nauthor 本名 <ユーザ名@email> 日時\ncommitter 本名 <ユーザ名@email> 日時\n\nコミットメッセージ`

write-tree と同様に上記のデータを zlib で deflate して SHA-1 ハッシュ値からなるファイル名で DB に登録する。-p で親となる tree を指定することができ、通常は前回 commit したときの tree の SHA-1 ハッシュ値を指定する(初回 commit 時は指定しない)。この tree を逆順にたどっていくことにより履歴を巡ることが可能。

## cat-file

DB に保存されているデータはすべて zlib で deflate されているためそのままでは読めない。cat-tree はこれを読める形式に変換するコマンドで、引数に指定した SHA-1 ハッシュ値の DB ファイルを inflate して一時ファイル (temp_git_file_XXXXXX) に書き出す。

## read-tree

blob と commit 形式のファイルは cat-file で inflate すれば読めるが、tree 形式は SHA-1 ハッシュ値がバイナリ形式で含まれるためそのままでは読めない。read-tree は tree 形式のファイルをテキストとして読める形に変換して一時ファイルに書き出す。

## show-diff

.dircache/index で管理しているすべてのキャッシュエントリについて、DB 内のファイルと現在のファイルの stat 情報に差分があれば diff を表示する。

## 使い方

``` console
$ git clone github.com/gitster/git.git
$ git checkout e83c5163
```

そのままでは make できなかったので以下のパッチをあてる。

{% gist 2121b1aa0926098539a6 %}

次の実行例は DB を作成して README と Makefile を tree に登録して commit するまでの例。

``` console
$ ./init-db
defaulting to private storage area
$ ./update-cache README
$ ./update-cache Makefile
$ find .dircache -type f
.dircache/index
.dircache/objects/66/5025b11ce8fb16fadb7daebf77cb54a2ae39a1
.dircache/objects/90/9a87257113dd11a2c2749c059b4aa6d55ed9f7
$ ./write-tree
8f4d3dbaec34d144bfcf5a8f2d7e0573abc00230
$ ./commit-tree 8f4d3dbaec34d144bfcf5a8f2d7e0573abc00230
Committing initial tree 8f4d3dbaec34d144bfcf5a8f2d7e0573abc00230
first commit
1f3b88e43904b187069aa7a8b6ff8006743969c9
$ ./read-tree 8f4d3dbaec34d144bfcf5a8f2d7e0573abc00230
100644 Makefile (909a87257113dd11a2c2749c059b4aa6d55ed9f7)
100644 README (665025b11ce8fb16fadb7daebf77cb54a2ae39a1)
$ ./cat-file 665025b11ce8fb16fadb7daebf77cb54a2ae39a1
temp_git_file_OfINdU: blob
$ head temp_git_file_OfINdU

	GIT - the stupid content tracker

"git" can mean anything, depending on your mood.

 - random three-letter combination that is pronounceable, and not
   actually used by any common UNIX command.  The fact that it is a
   mispronounciation of "get" may or may not be relevant.
 - stupid. contemptible and despicable. simple. Take your pick from the
   dictionary of slang.
```

Git コマンドの使い方は以下の記事が参考にさせていただきました。

* [gitのソースコードを読む](http://d.hatena.ne.jp/n314/20130905/1378383066)
