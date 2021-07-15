---
title: "Installing Ubuntu 20.10 on Macbook Pro Late 2013"
date: 2021-07-11T23:58:40+09:00
draft: false
---

開発用に Linux 機が欲しかったのだが、世界的な半導体の供給不足のためか、品薄な上、価格が高騰しており、なかなか手が出ないでいた。
近頃は外出することも多くないので、Raspberry PI でも買って LAN にぶら下げて使おうか、と思い試してみたところ、想像以上に非力だった。
その一方で、近々、長期間外出することになりそう、ということもあり、Laptop の購入を検討し始めた。
ところが、ふと、Macbook Pro に入れればよいことを思いつき、調べてみると、所有している Late 2013 は大きな問題なく Linux が動作するようだった。
加えて、Big Sur では一応サポート対象に入ってはいるものの、次期バージョンでは外れそうな懸念もあり、OS サポートの観点からも Linux に移行して延命した方が良い、という判断に至った。

OS の入れ替えに先立ち、バッテリと SSD の換装をした。バッテリは SLODA 製の互換品を使用。換装前は 5000 mAh 程度だったものが、換装後は 7000 mAh になった。SSD は Crucial P2 の 1TB モデルを選択。ただし、そのままでは MacBook Pro のソケットに勘合しないため、Sintech NVMe SSD アダプタカードというものを噛ませた。

HW の準備が整ったところで、次は Ubuntu 20.10 をインストール。ちなみに、現在の最新版である 21.04 を最初にインストールしてみたものの、起動できなかった。

OS のインストーラは USB メモリに書き込んだイメージからブートするのだが、イメージ自体は 2 GB ちょいなので、4 GB 程度の USB メモリがあれば十分。balenaEtcher というソフトで iso イメージを USB に書き込み、option ボタンを押しっぱなしで電源を ON すると、USB からブートできる。
あとはインストーラにしたがって進めるだけだが、一点だけ、インストール後に Wifi が認識されない課題があった。
以下のコマンドによると、デバイス自体は認識されている。

```sh
$ lspci | grep BCM
03:00.0 Network controller: Broadcom Inc. and subsidiaries BCM4360 802.11ac Wireless Network Adapter (rev 03)
```

というわけで、ドライバをインストールするのだが、そのためにはインターネットに接続する必要がある。鶏と卵。
だが、Bluetooth は使えるようなので、スマートフォン(Pixel 4a) の Bluetooth テザリングを有効にし、Ubuntu と スマートフォンをペアリングすることで解決できる。
Pixel 4a Off のメニューから、"Connect to Internet" を選択するとインターネットに接続できる。

その上で、下記コマンドによりドライバをインストールする。

```sh
$ sudo apt update
$ sudo apt install bcmwl-kernel-source
```

すると、画面右上に Wifi のアイコンが表示されるので、そこから "Select Network" を選択すれば、Wifi に接続できる。
