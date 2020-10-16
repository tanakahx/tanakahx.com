---
title: "Sample post"
date: 2014-05-07T08:49:44+09:00
draft: false
tags: [""]
---
This is a sample post.
これはサンプル投稿です。

コードブロックのテスト
```c
#include <stdio.h>

int main()
{
    printf("Hello, world!");
    return 0;
}
```

```cl
(defmacro where (&rest clauses)
  `#'(lambda (v) 
       (and 
	,@(loop while clauses
	     collecting `(search (getf v ,(pop clauses)) ,(pop clauses))))))
```
