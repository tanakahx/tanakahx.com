---
title: "Making A Fresh Start"
date: 2020-10-19T17:41:57+09:00
draft: false
---

多忙にかまけて更新できないまま 5 年の歳月が過ぎてしまった。今回の再開は、ブログという継続的な文章化プロセスが思考整理に効果的な影響をもたらすことを期待している側面と、自身の足跡を振り返るための盆栽的な趣味という側面からきている。

再開にあたり、これまで [GitHub Pages](https://tanakahx.github.com) と[はてなブログ](https://tanakahx.hatenablog.com)に分かれていたブログを[集約](https://tanakahx.com)した。これを機に AWS を勉強してみようと思い立ち、はじめてのクラウドコンピューティングサービスの設計に戸惑いながらも、何とか AWS で公開することができた。サイトは Hugo で生成しており、これを AWS の S3 上でホスティングしている。HTTPS 化や CDN の機能を持たせるために CloudFront を使い、ルーティングに Route 53、その他 additional な機能の実現に Lambda@Edge を導入した。当初、GitHub Pages にブログを公開していたときは Octpress や Hexo を使ってサイト生成をしていた。はてなブログに移行したのはブラウザでの手軽な編集に魅力を感じての事であったが、今回は Hugo による static site generator 方式に回帰した形となった。

ある程度内容をまとめて記事するのが理想ではあるが、それだと更新頻度が下がってしまうし継続しにくい。内容は不完全・不正確であっても良いことを認め、心理的障壁を下げ、日常を継続的に文章化することを優先させよう。