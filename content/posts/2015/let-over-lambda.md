---
title: "Let Over Lambda"
date: 2015-03-01T23:29:27+09:00
draft: false
tags: ["Lisp"]
---
先週から読み始めてようやく[読了](http://bookmeter.com/u/553612)。先々月、半分くらいまで読んでいたのだが、中断して内容を忘れてしまっていたので最初から一気に読み直した。Let Over Lambdaは一言でいうと、Common Lispのアドバンスドなマクロのテクニックが解説されている非常にためになる本。特にマクロを定義するためのdefmacro自体を拡張するマクロや、aletやalambdaといった代表的なアナフォリックマクロ、パンドリックマクロでクロージャを開くといったLispマクロならではの技術が詰まっている。これらのマクロに共通して見られることだが、本書は一貫して構文を共通化することの重要性を説いていたのが印象的であった。lambdaフォームにはシャープクォート`#'`をつけないとか、スペシャル変数には耳当てをつけないとか。

筆者の主張としては最初から最後までLispを賛美するのだが、客観性に欠ける部分があったり、効率性に関しては他言語との正当な比較をしていない等、気になるところがないわけではないが、それ以上にマクロの面白さに引き込まれて読んでいて楽しかった。On Lispと並び、個人的にはかなりのヒット本。
