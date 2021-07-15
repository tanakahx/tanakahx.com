---
title: "Build QEMU From Source Code"
date: 2021-06-06T00:17:09+09:00
draft: false
---

最新版の QEMU (6.0.0) を使いたかったので、パッケージインストールではなく、ソースコードからビルドした。
ホストは Ubuntu-20.04@WSL2 で、QEMU のターゲットマシンは i386、他は不要。

```sh
$ wget https://download.qemu.org/qemu-6.0.0.tar.xz
$ tar xvJf qemu-6.0.0.tar.xz
$ cd qemu-6.0.0
$ ./configure --prefix=$HOME/.local/qemu-6.0.0 --target-list=i386-softmmu
$ make
$ make install
```

`--target-list` でターゲットマシンを指定できる。指定しないと全ターゲットがビルド対象となるため、非常に時間がかかる。