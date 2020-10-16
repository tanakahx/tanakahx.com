---
title: "行列積"
date: 2015-02-02T23:40:51+09:00
draft: false
tags: ["Lisp"]
---

行列積が必要になったので書いてみた。最初は配列に対する操作にしようと思ったけど、手っ取り早く使いたかったのでリストにした。loopマクロを使うとsumしてcollectするだけだから簡単に書けてよい。

```cl
(defun matrix-mul (a b)
  (let ((n (length (nth 0 a)))
        (m (length b)))
    (if (= n m)
        (loop for i below (length a)
          collect (loop for j below (length (nth 0 b))
                    collect (loop for k below n
                              sum (* (nth k (nth i a)) (nth j (nth k b))))))
        (error "Dimension error"))))
```
