---
title: "Common Lisp の効率的なファイル読み込み"
date: 2014-09-01T02:16:13+09:00
draft: false
tags: ["Lisp"]
---
Common Lisp でファイルを読み込む方法は [Slurping a file in Common Lisp](http://www.ymeme.com/slurping-a-file-common-lisp-83.html) が参考になる。指定したファイルを読み込み、バッファ (配列) を返す関数はこんな感じ。実測はしていないが Perl スクリプト以上の速度が出るらしい。

```cl
(defun read-file (path)
  (with-open-file (in path)
    (let ((seq (make-array (file-length in) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq in))
      seq)))
```
