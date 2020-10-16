---
title: "Migrate from Octopress to Hexo"
date: 2015-01-25T20:02:30+09:00
draft: false
tags: ["Blog"]
---

年末年始にOctopressからHexoに移行したときのメモ。

まずOctopressで書いていたMarkdownの記事をHexo用のフォーマットに変換するスクリプトを書いた。
```sh
#!/bin/sh
for i in *.markdown; do
    awk '!/^(layout|comments)/&&NR!=1{print $0}' $i | sed 's/^categories/tags/g' | sed 's/^title: *"\(.*\)"$/title: \1/g' > "$i".markdown.new
done
```
Hexoは1行目に`---`は書かないらしいので削除している。また、Octopressのlayout, comments等のタグもHexoでは不要なので削除し、OctopressのcategoriesはHexoではtagsとして扱うようなので変換している。最後に、Octopressでつけていたtitleのダブルクォーテーションを削除している。無論、\*.markdown.newの内容が問題なさそうであれば\*.markdownにリネームしておく。

また、各記事のPermalinkの設定もOctopressに合わせる必要があるが、これはHexoの`_config.yml`に設定することで対応できる。

```yml
permalink: blog/:year/:month/:day/:title/
```

先頭の`blog/`がミソで、最初これを設定していなかったが故にOctopressとPermalinkの構造が変わってしまい、記事へのリンクが切れてしまった。(そしてしばらく気がつかなかった...)

テーマは[Codeland](https://github.com/xing5/hexo-theme-codeland)というテーマをforkして、右のサイドペインにAboutウィジェットが表示されるようにしてみた。forkしたテーマは[Github](https://github.com/tanakahx/hexo-theme-codeland)に登録しておいた。
