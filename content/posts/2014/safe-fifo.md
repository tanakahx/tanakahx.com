---
title: "安全な FIFO の作り方"
date: 2014-05-15T00:51:56+09:00
draft: false
tags: ["Python"]
---
固定長配列を使って安全な FIFO を実現するときにちょっと便利な方法を思いついたのでメモしておく。次のようなシンプルな FIFO があるとする。

``` python
class Fifo:
    def __init__(self, size):
        self.fifo = [0]*size
        self.wp = 0
        self.rp = 0

    def push(self, obj):
        self.fifo[self.wp] = obj
        self.wp += 1
        if self.wp == len(self.fifo):
            self.wp = 0

    def pop(self):
        r = self.fifo[self.rp]
        self.rp += 1
        if self.rp == len(self.fifo):
            self.rp = 0
        return r
```

push() で FIFO にデータを詰めて、pop() で先に push() したデータから取り出せる。

``` python
print "-------- Test1 --------"
fifo = Fifo(8)
for i in range(4):
    fifo.push(i)
for i in range(4):
    print fifo.pop(),
print
for i in range(8):
    fifo.push(i)
for i in range(8):
    print fifo.pop(),
print

print "-------- Test2 --------"
fifo = Fifo(8)
for i in range(9): # ERROR! (Overwrite)
    fifo.push(i)
for i in range(8):
    print fifo.pop(),
print

print "-------- Test3 --------"
fifo = Fifo(8)
for i in range(8):
    fifo.push(i)
for i in range(9): # ERROR! (Overread)
    print fifo.pop(),
print
```

``` console 実行結果
-------- Test1 --------
0 1 2 3
0 1 2 3 4 5 6 7
-------- Test2 --------
8 1 2 3 4 5 6 7
-------- Test3 --------
0 1 2 3 4 5 6 7 0
```

Test1 のように FIFO のバッファ容量をオーバーしない限り push() した通りに pop() されている。しかし、Test2 の場合はバッファ容量を超えて push() しているので、先に保存したデータを上書きしてしまっている。Test3 は逆に pop() が多すぎる場合で、データを保存していない場所からデータを読み取ってしまっている。

そこで、データの上書きや読み過ぎを防ぐ SafeFifo というクラスを作りたい。単純にリードポインタとライトポインタを比較するコードを追加すればよいのだが、既存クラスに手を加えたくない場合や、テスト時のみチェック機構が備わっている Safe 版を使いたい場合がある。そこで次のような filled という配列を用意しておき、push() して書き込まれた場所に True を記録し、pop() して読み出された場所は False を記録しておく。上書き／読み過ぎを検出しているので、出力結果がおかしくなることはない。(ただし、当然ながらバッファ容量を超えて push() した分は破棄されてしまう。)

``` python
class SafeFifo(Fifo):
    def __init__(self, size):
        Fifo.__init__(self, size)
        self.filled = [False]*size

    def safe_push(self, obj):
        if self.filled[self.wp] == False:
            self.filled[self.wp] = True
            self.push(obj)
        else:
            raise Exception('Overwrite at %d' % (self.wp))

    def safe_pop(self):
        if self.filled[self.rp] == True:
            self.filled[self.rp] = False
            return self.pop()
        else:
            raise Exception('Overread at %d' % (self.rp))
```

Fifo を SafeFifo に置き換えて例外処理を追加した使用例。

``` python
print "-------- Test1 --------"
fifo = SafeFifo(8)
for i in range(4):
    fifo.safe_push(i)
for i in range(4):
    print fifo.safe_pop(),
print
for i in range(8):
    fifo.safe_push(i)
for i in range(8):
    print fifo.safe_pop(),
print

print "-------- Test2 --------"
fifo = SafeFifo(8)
try:
    for i in range(9): # ERROR! (Overwrite)
        fifo.safe_push(i)
except Exception, e:
    print e
for i in range(8):
    print fifo.safe_pop(),
print

print "-------- Test3 --------"
fifo = SafeFifo(8)
for i in range(8):
    fifo.safe_push(i)
try:
    for i in range(9): # ERROR! (Overread)
        print fifo.safe_pop(),
except Exception, e:
    print e
print
```

実行結果は以下の通り。

``` console 実行結果
-------- Test1 --------
0 1 2 3
0 1 2 3 4 5 6 7
-------- Test2 --------
Overwrite at 0
0 1 2 3 4 5 6 7
-------- Test3 --------
0 1 2 3 4 5 6 7 Overread at 0
```

まあ普通はこんなことはやらなくてポインタを比較するだけでいいんだけど。
