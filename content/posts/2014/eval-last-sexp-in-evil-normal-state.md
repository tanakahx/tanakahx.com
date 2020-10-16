---
title: "Evil を eval-last-sexp に対応させる"
date: 2014-06-22T11:09:14+09:00
draft: false
tags: ["Emacs"]
---
Emacs では通常、Lisp の S 式の最後 (閉じ括弧の次) にカーソルを置いて C-x C-e を押すとその S 式が eval-last-sexp により評価される。しかし、Evil の normal 状態では、S 式が行末にある場合にカーソルが閉じ括弧をポイントするため、 eval-last-sexp により正しく S 式を指定して評価することができない。insert 状態では正しく指定できるが、評価するたびに insert 状態に入るのは勝手が悪すぎる。というわけで、調べてみると eval-last-sexp が内部で使っている preceding-sexp に defadvice する方法で解決できることがわかった。

[normal-state interacts poorly with eval-last-sexp](https://bitbucket.org/lyro/evil/issue/17/normal-state-interacts-poorly-with-eval)

``` cl
(defadvice preceding-sexp (around evil activate)
  "In normal-state, last sexp ends at point."
  (if (evil-normal-state-p)
      (save-excursion
        (unless (or (eobp) (eolp))
          (forward-char))
        ad-do-it)
    ad-do-it))

(defadvice pp-last-sexp (around evil activate)
  "In normal-state, last sexp ends at point."
  (if (evil-normal-state-p)
      (save-excursion
        (unless (or (eobp) (eolp))
          (forward-char))
        ad-do-it)
    ad-do-it))
```

Slime でも同様に slime-last-expression に対して defadvice することで対応できた。

``` cl
(defadvice slime-last-expression (around evil activate)
  "In normal-state, last sexp ends at point."
  (if (evil-normal-state-p)
      (save-excursion
        (unless (or (eobp) (eolp))
          (forward-char))
        ad-do-it)
    ad-do-it))
```
