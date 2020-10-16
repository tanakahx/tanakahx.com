---
title: "ASDFの設定ファイル"
date: 2015-01-21T01:41:58+09:00
draft: false
tags: ["Lisp"]
---
`~/.config/common-lisp/source-registry.conf`
`hello`以下の*.asdファイルを探す。
```cl
(:source-registry
 (:directory (:home "common-lisp/hello"))
 :inherit-configuration)
```

プロジェクトのローカルディレクトリをプロジェクト毎に指定したい場合。
`src`以下の*.asdファイルを探す。
asdf.conf
```cl
(:source-registry
 (:directory (:here "src/"))
 :inherit-configuration)
```
`source-registry.conf`ではこのプロジェクトの`asdf.conf`をinclude指定する。
```cl
(:source-registry
 (:include (:home "common-lisp/hello2/asdf.conf"))
 :inherit-configuration)
```
