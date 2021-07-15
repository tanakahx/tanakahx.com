---
title: "How to debug Beaglebone Black with JTAG"
date: 2021-05-18T17:06:12+09:00
draft: false
---

BeagleBone Black という single board computer を購入し、JTAG を使ったデバッグ環境を整えたときのメモ。

BeagleBone Black 本体のほかに、追加で用意するものは以下の 3 点。

- AC アダプタ (5V 1A, center plus)
- Samtec FTR-110-03-G-D-06 (Compact TI 20 pin)
- TI XDS110 (JTAG probe)

まず、BeagleBone Black はコスト削減のためか、JTAG probe を差すためのピンヘッダが実装されていないため、裏面にある P2 に Samtec のピンヘッダをはんだ付けする必要がある。
ピン間隔がよくある 2.54 mm ピッチの半分のサイズなので、細かい作業が要求される上、ピンヘッダの向きもあるので要注意。この時点で人をかなり選んでしまう気がする。

続いて、PC には Code Composer Studio version 10 をインストールし、XDS110 を USB 接続する。このとき、自動的にデバイスドライバがインストールされるはず。

AC アダプタを接続して電源を入れる際は、SD カードを抜き、S2 スイッチを押しながら AC アダプタを接続することに注意する。これにより boot sequence が変更され、eMMC boot ではなく UART boot となり、Linux ではなく UART 受信プログラムが起動する。この状態で JTAG デバッガを connect することができる。(Linux が起動してからだと connect 出来ない。Cortex-A8 の DAP アドレス 0x80001000 が見えなくなっているせい?)

Code Composer Studio の使い方で注意する点は、
 - BeagleBone_Black.ccxml -> Test Connection で以下のメッセージが出力されること。
 - BeagleBone_Black.ccxml -> Advanced タブ -> M3_wakeupSS_0 を Bypass に設定すること。


JTAG で connect する際、以下のスクリプトが実行されているようだ。

```
C:\ti\ccs1031\ccs\ccs_base\emulation\boards\beaglebone\gel\beagleboneblack.gel
```

この中で、PLL の設定や SDRAM の初期化をしているため、connect 後は at speed で動作し、0x80000000 番地から始まる 512 MB の SDRAM 領域へ何も考えずにアクセスすることができる。また、load するプログラムも ELF のアドレスを解釈してその番地に load してくれるようで、default では 0x80000000 番地の先頭から main() が始まる。bare metal な bootstrap 作業は不要で、かなりお手軽な感じでプログラムできる。

