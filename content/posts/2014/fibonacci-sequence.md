---
title: "フィボナッチ数列"
date: 2014-05-19T22:26:09+09:00
draft: false
tags: [""]
---

なんとなくフィボナッチ数列を計算したくなった。素朴な実装だとどのくらい遅いのかというと。

``` c
#include <stdio.h>
#include <stdlib.h>

int fib(int n)
{
	if (n == 0) {
		return 0;
	}
	else if (n == 1) {
		return 1;
	}
	else {
		return fib(n-1) + fib(n-2);
	}
}

int main(int argc, char *argv[])
{
	int n = strtol(argv[1], NULL, 10);
	printf("%d\n", fib(n));
	return 0;
}
```

``` console 実行結果
$ time ./a.out 45
1134903170
./a.out 45  14.84s user 0.02s system 99% cpu 14.916 total
```

フィボナッチ数列は fib(n) と fib(n-1) の和からなっており、1 だけ異なる引数で二手に分かれて再帰していくので、何度も同じ計算を繰り返すはめになる。したがって、与えられた n に対して計算した結果をキャッシュ(メモ)しておいて、再び同じ n が与えられたらキャッシュから返すようにすれば高速化が望める。これをメモ化という。メモするための記憶領域は別途必要になるが、実行速度の向上はそのオーバーヘッドを遥かに上回る。確か最初は SICP の本か何かで読んだ気がするがだいぶ忘れてて、記憶を頼りに書こうとしたが結局ググってしまったことは内緒。

``` c
#include <stdio.h>
#include <stdlib.h>

int memo[100];

int fib(int n)
{
	if (memo[n]) {
		return memo[n];
	}
	else if (n == 0) {
		return 0;
	}
	else if (n == 1) {
		return (memo[n] = 1);
	}
	else {
		return (memo[n] = fib(n-1) + fib(n-2));
	}
}

int main(int argc, char *argv[])
{
	int n = strtol(argv[1], NULL, 10);
	printf("%d\n", fib(n));
	return 0;
}
```

お遊びなのでコードは適当に書いてます。実行速度は先ほどとは歴然の差。

``` console 実行結果
$ time ./a.out 45
1134903170
./a.out 45  0.00s user 0.00s system 38% cpu 0.006 total
```
