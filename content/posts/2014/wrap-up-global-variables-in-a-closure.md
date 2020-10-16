---
title: "グローバル変数をクロージャで包む"
date: 2014-05-14T00:33:35+09:00
draft: false
tags: ["JavaScript"]
---
暇つぶしにリーダブルコードを読んでいたら、JavaScript のコードでグローバル変数で名前空間を汚染しないようにするために、クロージャを利用する例が掲載されていた。クロージャの使い方としてこれは知らなかったのでなるほどと感心してしまった。

``` js
var closure = (function () {
    var is_called = false;
    return function (name) {
        if (!is_called) {
            console.log("first call with " + name);
            is_called = true;
        }
        else {
            console.log("finished");
        }
    };
}());

closure("foo")
closure("foo")
closure("foo")
```

ある関数が複数回呼ばれたときに最初の一回のみ関数を実行したい場合、クロージャを使わないなら is_called というグローバル変数で一回目のコールかどうかを管理する。この例はそのグローバル変数をクロージャで包んで外部の名前空間に流出しないようにしている。

``` console 実行結果
first call with foo
finished
finished
```

Common Lisp で書いてみるとこんな感じ。

``` cl
(defparameter *closure* nil)

(setf *closure* (let ((is-called nil))
		  #'(lambda (name)
		      (cond ((null is-called)
			     (format t "first called with ~a~%" name)
			     (setf is-called t))
			    (t
			     (format t "finished~%"))))))

(funcall *closure* "foo")
(funcall *closure* "foo")
(funcall *closure* "foo")
```
