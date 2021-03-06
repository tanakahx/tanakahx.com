---
title: "UNIX V6 memo"
date: 2015-05-03T17:41:42+09:00
draft: false
tags: ["UNIX"]
---
# ソースコード
Dennis_v6よりもKen_Wellsch_v6の方がLions本のバージョンに近い。

ソースコードは以下のリンクから入手する。

* [http://orangetide.com/Unix/](http://orangetide.com/Unix/)

# PDP-11

## CPU

CPUは8つの16bit汎用レジスタをもつ。

* r0, r1: アキュムレータ
* r2, r3, r4: 手続き呼び出しのローカル変数
* r5: 環境ポインタ
* r6: スタックポインタ(SP)
    * r6はプロセッサのモードに応じてカーネルスタックかユーザスタックのいずれかを指すため、2つのレジスタを切り替えている。
* r7: プログラムカウンタ(PC)

## PSW

* 15, 14: 現在のモード
* 13, 12: 以前のモード
* 7, 6, 5: 優先度(0: 低い、7: 高い、この優先度よりも低い割り込みは受け付けられない。)
* 4: T(トラップ)
* 3: N(負フラグ)
* 2: Z(ゼロフラグ)
* 1: V(オーバーフローフラグ)
* 0: C(キャリーフラグ)

## メモリ管理

APR(Active Page Ragister)はPAR(Page Address Register)+PDR(Page Description Register)のペア8個から構成される。これをカーネルモードとユーザモードで使い分けるために2セット持っている。どちらのAPRを使うかはPSWの状態で決定される。

## 仮想アドレスから物理アドレスへの変換

PDP-11は16bitの仮想アドレス空間を18bitの物理アドレス空間にマッピングする。
仮想アドレスのbit15-13はAPF(Active Page Field)と呼ばれ、この値に応じてAPR0-APR7のうちの一つを選択する。(ただし、カーネルモードとユーザモードのどちらのAPRを使うかはPSWによって決定される。)
仮想アドレスのbit5-0は物理アドレスでもそのままbit5-0として使われる。したがって、この64byte分は仮想アドレスの最小ブロックサイズとなる。仮想アドレスの残りのbit12-6はこのページのブロック番号として扱われる。このブロック番号は選択されたAPRのPARに設定しているブロック開始アドレス(PAF)からのオフセットとみなされ、PAFに加算された結果、物理アドレスbit17-6に使われる。

## 初期状態

システムはカーネルモードで起動し、メモリ管理は無効になっている。この場合、0~56KBの仮想アドレスはそのまま物理アドレスにマッピングされる。しかし、仮想アドレスの0160000~0177777は物理アドレスの0760000~0777777にマッピングされる。メモリマップ上のこの領域はプロセッサや周辺機器のレジスタが割り当てられている。

## SSR0

SSR0(177572): Status Register 0

## m40

システムセグメントの初期化

* KISA0(172340): Kernel I Space Address Register 0 (PARに相当)
* KISD0(172300): Kernel I Space Description Register 0 (PDRに相当)
* 設定値
	* 77406 (111 111 100 000 110)
	* 128block(4Kword), 上方拡張, Read/Writeアクセス可

## main

m40.sの_ka6: KISA6(172354)より、ka6の値は172354になっているため、*ka6と書いた場合は、KISA6のレジスタの値を表す。*ka6+16以降の物理アドレスをヒープ領域として使うために次のclearsegで0クリアし、mallocで割り当てられるようにcoremapで管理する。

mainはnewprocによりプロセス0をプロセス1にコピーする。プロセス0はそのままschedに突入するが、sleepし（ *ここの理由はまだ不明* ）、swtchが実行されることによりプロセス1に制御が移る。

プロセス1はexpandによりデータ＋スタック領域を拡張する。続いてユーザ空間用のページング(セグメント)の設定をするためestaburを実行する。estaburではあらかじめu構造体にu_uisaとu_uisdのプロトタイプ設定を構築した後、最後の処理としてsuregを実行することでハードウェアレジスタにプロトタイプ設定値を反映する。

## clearseg

引数で渡したアドレスをUISA0に設定することで、ページ0の物理アドレスを設定する。mtpiを使ってカーネルスタックに積んだ0値をユーザ空間のページ0に転送することで、ユーザ空間を32word分0クリアする。

## swtch

### savu
スタックポインタと環境ポインタを引数で指定したu構造体メンバに保存する。

```asm
_savu:
	bis	$340,PS    ; 優先度を7に設定(すべての割り込みを禁止)
	mov	(sp)+,r1   ; リターンアドレスをr1に設定
	mov	(sp),r0    ; 仮引数(u.rsavのアドレス)をr0に設定
	mov	sp,(r0)+   ; spをu.rsav[0]に設定
	mov	r5,(r0)+   ; r5をu.rsav[1]に設定
	bic	$340,PS    ; 優先度を0に設定(すべての割り込みを許可)
	jmp	(r1)       ; 呼び出し元に戻る
```

### retu
カーネル空間のページ6が指定した物理アドレスを指すようにKISA6を設定した上で、u構造体からスタックポインタと環境ポインタを復帰させる。

```asm
_retu:
	bis	$340,PS    ; 優先度を7に設定(すべての割り込みを禁止)
	mov	(sp)+,r1   ; リターンアドレスをr1に設定
	mov	(sp),KISA6 ; 仮引数(proc[0].p_addr)の値をKISA6に設定
	mov	$_u,r0     ; u構造体のアドレスをr0に設定
1:
	mov	(r0)+,sp   ; u.rsav[0]からspを復帰
	mov	(r0)+,r5   ; u.rsav[1]からr5を復帰
	bic	$340,PS    ; 優先度を0に設定(すべての割り込みを許可)
	jmp	(r1)       ; 呼び出し元に戻る
```

mainでcoremapに登録したヒープ領域はnewprocで新しいプロセスを生成するときに割り当てられ、proc構造体のp_addrに格納される。プロセスを切り替える場合、p_addrをretuに渡すことでKISA6が書き換えられ、カーネルモードにおいてu構造体が指す仮想0140000番地の示す物理アドレスがp_addrに変更される。

### aretu

引数に与えたアドレスからスタックポインタと環境ポインタを復帰させる。

### estabur

suregで設定するためのプロトタイプとなるページ設定を構築する。データセグメントはユーザモードのデータ領域＋スタック領域に加えて、カーネルモードで使用するu構造体＋カーネルスタック領域も含むことに注意する。

### sureg

ユーザ空間のページ設定をするために、u構造体に設定しているプロトタイプ(uisa, uisd)をもとにセグメントレジスタを設定する。これによりスイッチ後のプロセスをユーザ空間で実行することが出来る。

# 第8章 プロセス管理

## setpri(2156)

proc構造体を受け取り、優先度を更新する。

```
優先度 = min(127, CPU時間 + PUSER + NICE値)
```

## sleep(2066)

次の2つのパラメータを引数にとる。

* sleepする理由(chan)
* 目覚めてスケジューリングキューに入る時の優先度(pri)

priとして負数が与えられた場合は、シグナルが起きても起こさないことを表す。

## wakeup(2113)

引数に与えたchanでsleepしているプロセスに対してsetrunを実行する。

## setrun(2134)

プロセスを実行中に設定する。その優先度がカレントプロセスよりも高い場合は、再スケジューリングが必要であることを示すrunrunをセットする。

sleep(&runout)はsched(プロセス0)のみが実行するため、

```c
if(runout != 0 && (rp->p_flag&SLOAD) == 0) {
  runout = 0;
  wakeup(&runout);
}
```

におけるwakeup(&runout)はプロセス0に対するsetrun(&proc[0])と等価である。

## expand(2268)

データ＋スタック領域のサイズを変更する。縮小する場合は余った領域をmfreeする。拡大する場合は必要な領域をmallocして、あたらし領域へデータをコピーする(*スタック領域はexpandの呼び出し側が調整する*)。mallocで新しい領域を確保できない場合は、利用可能になるまでxswapによりスワップアウトする。xswap呼び出しの第二引数は非零なので、コアイメージは自由リストに追加されて解放され、SLOADフラグはクリアされる。しかし、このプロセスは続くswtchのretuでスケジューラのスタックに切り替えるまで解放されたコアイメージのスタック上で動作することに注意する。

# 第9章 ハードウェア割り込みとトラップ

割り込みが発生するとUNIXはカーネルモードに移行し、PCとPSレジスタはカーネルスタックに積まれる。割り込みルーチンは割り込みベクタにより決定される。割り込みベクタの最初のワードは割り込みルーチンのアドレスであり、次のワードは新しいPSレジスタの値である。

割り込みから復帰する場合には、カーネルスタックに積んだPCとPSを取り出して再ロードする。

## 優先度

PDP11の割り込み優先度は4,5,6,7に制限されている。(1,2,3はUNIBUSでサポートされていないらしい)

## 割り込み優先度

UNIXでは割り込み処理ルーチンは割り込み優先度と同じ優先度で実行される。つまり、ある割り込み処理ルーチンを実行している時は、それ*以下*の優先度が低い割り込みが起こったとしても無視される。

## 割り込みハンドラのための規則

ユーザプロセスmが割り込み待ちの間、ユーザプロセスn実行中に割り込みが入った場合、カーネルプロセスnはその割り込みとは無関係であるにもかかわらず、割り込みハンドラの処理を行い、その割り込みを取り扱うことに注意する。

## トラップ

トラップは外部要因の割り込みとは異なり、CPU内部から最高優先度(8)を持って実行される割り込みであり、システムコールや例外発生時にトラップがかかる。

# 第10章 アセンブラ"トラップ"ルーチン

## fuibyte(0841), fuiword(0844)
fuiwordはgwordをサブルーチンコールし、PS, nofalutをスタックにpushする。次にmfpiで例外が発生した場合、512行目"trap; br7+0"がPC,PSにロードされ、trapをカーネルモードで実行する。fuiwordはカーネルモードでのみ呼ばれるため、このときモード遷移は起きていない。この時点で古いPS, PCはこの順番でカーネルスタックにpushされる。

trapでは2word先にPSを保存する(*何のため?*)。スタックトップにあるgwordへの戻りアドレスであるPCをnofalut(=err)に書き換えて、そのままrttするとスタックは2word分popされ、errに飛ぶ。errではgwordでpushしておいたnofaltとPSを復帰させて、fuiwordへの戻りアドレスを空popにより飛ばし、fuiwordのcallerへ直接リターンする。

## 割り込み

* 現モード = カーネルモード
* 前モード = カーネルモードかユーザモード
* 優先度 = 6

## call(0776)

```asm
0570 kwlp:  jsr r0,call; _clock
```

でr0にはjsr命令の次のワード(\_clock)を指すアドレスが入り、割り込み処理ルーチンへの分岐は以下の命令で行われる。(このとき、(\*r0)になっている理由がわからない。)

```asm
0799 jsr  pc,*(r0)+
```

ユーザモードでcallが呼ばれた場合、runrun > 0であると優先度の高いプロセスが存在しているので、swtchを実行してコンテキストスイッチをする。swtchの呼び出しによりpcがスタックにpushされるので、swtchでコンテキストを切り替える前にsavuで保存される。swtchでは優先度の高いプロセスに切り替えられるが、やがてもとのプロセスが再起した場合に、スイッチ前にsavuしたスタックを復帰してretuしてreturnすることでswtchを呼び出す前のコンテキスト(そのカーネルプロセスの割り込み処理中)から再開する。

## ユーザプログラムのトラップ

* 現モード = カーネルモード
* 前モード = ユーザモード
* 優先度 = 7

割り込み処理(例えば0570行目のサブルーチンcallのコール)とは異なり、trap命令により034番地から直接0755行目のtrapに飛ぶため、スタック上にr0を積んでいない部分が大きな差であり、その他の処理ははcallとほぼ共通である。

```asm
0756 mov  PS,-4(sp)
```

において、SPから2word先に格納しているのは、

```asm
0762 jsr  r0,call1; _trap
0771 call1:
0772 tst  -(sp)
```

の最初の命令でr0に\_trap設定するとともに、古いr0をスタックに積むことでSPを1word先に進め、次のtstで最初のPSをスタックトップに含むように調整するため。

# 第11章 クロック割り込み

## clock(3725)

toutは時間待ちしているプロセスを起こすための時間を管理しているが、1セットしかないため複数のプロセスが別々の時間で待つためにはどのように管理しているかを調べたい。(calloutのように複数のイベントの発生タイミングを管理しているわけではないらしい。おそらく起こされたときに、sleep時間が足りなかったら残りの時間がその時点でのtoutよりも多く必要だったらtoutを再設定してまた寝るのかもしれない。)

# 第12章 トラップとシステムコール

mainからnewprocで作られた子プロセスがinitにexecするコードでシステムコール呼び出しを考えてみる。以下はexecを実行するコード。(コメントに命令の番地を8進数で記載した。)

```c
int	icode[]
{
	0104413,	// 0000: sys exec; init; initp
	0000014,	// 0002:
	0000010,	// 0004:
	0000777,	// 0006: br .
	0000014,	// 0010: initp: init; 0
	0000000,	// 0012:
	0062457,	// 0014: init: </etc/init\0>
	0061564,	// 0016:
	0064457,	// 0020:
	0064556,	// 0022:
	0000164,	// 0024:
};
```

trap命令は下位5bitが0ではなく、execのシステムコール番号である013(11d)が埋め込まれているので、間接型trapではなく、Lions本の

* (b) trap命令に続くプログラム列の中に埋め込まれたワードの集合として。

に該当する。以下の処理によりシステムコールに必要な引数をu.uargに代入していく。

```c
for(i=0; i<callp->count; i++) {
  u.u_arg[i] = fuiword(pc);
  pc =+ 2;
}
```

ここで、fuiwordは引数で指定したアドレスを前モードの仮想アドレスと見なして現モードのスタックにプッシュするルーチンである。trapの前モードはユーザモードなので、pcは0002番地のアドレスを指している。(trap命令実行時にpcは次の命令を指していることに注意する。)

したがって、trapにより以下の設定がなされる。

* システムコールはexec
    * 0104413の下位5bitが013(11d)であるため
* u.uarg[0]=014
* u.uarg[1]=010


```c
while(ap = fuword(u.u_arg[1])) {
  na++;
  if(ap == -1)
    goto bad;
  u.u_arg[1] =+ 2;
  for(;;) {
    c = fubyte(ap++);
    if(c == -1)
      goto bad;
    *cp++ = c;
    nc++;
    if(nc > 510) {
      u.u_error = E2BIG;
      goto bad;
    }
    if(c == 0)
      break;
  }
}
```

次にexecシステムコールの中身を見ていく。上記は先ほどtrapで設定されたu.uargを使って、引数で与えられた文字列領域を辿りカーネルが管理するバッファ領域にコピーするコードである。最初のfuword(u.u_arg[1])により、apには010番地の値、すなわち、014が入る。続いて、内部forループのfubyte(ap++)により014番地の最初の文字'/'(057)が取得できるので、これをcpが指しているバッファ領域に書き、'\0'の終端まで行う。これでwhileの1回目が完了する。

続いてwhileの2回目でのfuword(u.u_arg[1])では、u.u_arg[1]が2増やされているため、012番地の値、0が帰る。これによりwhile文は終了する。

以上をまとめると、trapにより設定されたu.u_argの第二引数であるu.u_arg[1]は実際の文字列引数へのポインタ配列の先頭要素を指すポインタにあたり、fuword, fubyteを使って2回参照することでユーザ空間からカーネル空間のバッファへと文字列の転記を行う。ポインタの配列の要素が0になると、そこが配列の最後の要素であると見なされる。この際、文字列は0終端であり、複数の文字列が存在しても詰めてバッファに格納されている。文字列の個数はnaで管理している。

execの仕様はUPM(II)によると、

* sys exec; name; args
* name: <..\0>
* args: arg0; arg1; ...; 0
* arg0: <...\0>
* arg1: <...\0>

となっており、以上で確かめたように、第二引数であるargsはarg0, arg1, ...という文字列へのポインタの配列であることがわかる。

## a.out形式について

次の4つのセクションからなる

1. ヘッダ
2. プログラムとデータ
3. シンボルテーブル
4. リロケーションビット

3, 4はldに`-s`オプションをつけるか、stripした場合は空になる。

* テキストセグメントの開始位置は8word(16byte)目
* データセグメントの開始位置は8+sizeof(text) word目
* リロケーション情報の開始位置は8+sizeof(text)+sizeof(data) word目

リロケーション情報がある場合

* シンボルテーブルの開始位置は8+(sizeof(text)+sizeof(data))\*2 word目

リロケーション情報がない場合

* シンボルテーブルの開始位置は8+(sizeof(text)+sizeof(data)) word目


#### ヘッダ(マジックナンバ)

以下、共通してテキストセグメントは0番地におかれる。

* 407

テキストセグメントは保護も共有もされない。データセグメントはテキストセグメントの直後から開始する。

* 410

テキストセグメントは保護され共有される。データセグメントはテキストセグメントの後の8Kbyte境界から開始する。

* 411

テキストセグメントは保護され共有される。テキストセグメントとデータセグメントは分離される。

## execのテキスト&データセグメントの初期化

```
u.u_base = &u.u_arg[0];
u.u_count = 8;
u.u_offset[1] = 0;
u.u_offset[0] = 0;
u.u_segflg = 1;
readi(ip);
```

続いて、実行するa.outファイルのヘッダ8byteをu.u_arg[0]~u.u_arg[3]に読み込む。a.outフォーマットの仕様から、

* u.u_arg[0] = 0407/0410/0411のマジックナンバ
* u.u_arg[1] = テキストセグメントのサイズ
* u.u_arg[2] = データセグメントのサイズ
* u.u_arg[3] = bssセグメントのサイズ

が格納される。以降はテキストセグメントとデータセグメントのサイズに応じて必要なメモリ領域を計算してexpand & estaburする。

```
estabur(0, ds, 0, 0);
u.u_base = 0;
u.u_offset[1] = 020+u.u_arg[1];
u.u_count = u.u_arg[2];
readi(ip);
```

これにより、a.outからテキストとデータセグメントを読み込み、ユーザ空間の0番地から格納する。

## execのスタックセグメントの初期化

### suword(ap, na)

前モードの仮想ap番地に対して、naの値を転送する。この場合、apはexec後の新しいスタックセグメントのスタックトップを表し、naはexecに与えた引数の個数を表す。

```
cp = bp->b_addr;        // カーネルバッファ領域のアドレス(転送するデータの内容)
ap = -nc - na*2 - 4;   // スタックトップの仮想アドレス(高位アドレスなので負)
u.u_ar0[R6] = ap;       // スタックトップをsp(R0)に設定
suword(ap, na);         // スタックトップに引数の個数naを設定(argcに相当)
c = -nc;                // スタックボトムからnc byte分上位を指す
while(na--) {
  suword(ap=+2, c);     // ここで設定する値はargv[na]に相当
  do
    subyte(c++, *cp);
  while(*cp++);
}
suword(ap+2, -1);       // argv[na]はスタックボトムを指す(無効を示すため)
```

このコードより、スタックは以下のように設定される。

* argc
* argv[0], ..., argv[na-1]
* argv[0]の文字列, ..., argv[na-1]の文字列

# fork

```asm
/ C library -- fork

/ pid = fork();
/
/ pid == 0 in child process; pid == -1 means error return
/ in child, parents id is in par_uid if needed

.globl	_fork, cerror, _par_uid

_fork:
	mov	r5,-(sp)
	mov	sp,r5
	sys	fork
		br 1f
	bec	2f
	jmp	cerror
1:
	mov	r0,_par_uid
	clr	r0
2:
	mov	(sp)+,r5
	rts	pc
.bss
_par_uid: .=.+2
```

sys forkの直後に親子が実行する命令が同じであると親子の区別がつかなくなってしまうため、forkシステムコールは親プロセスの戻りアドレスを1word分進める。そのため、親プロセスはsys forkの次はbec 2fを実行することになる。(子プロセスはsys forkから戻るとbr 1fを実行する。)

# sbreak

```c
sbreak()
{
	register a, n, d;
	int i;

	/*
	 * set n to new data size
	 * set d to new-old
	 * set n to new total size
	 */

	n = (((u.u_arg[0]+63)>>6) & 01777);
	if(!u.u_sep)
		n =- nseg(u.u_tsize) * 128;
	if(n < 0)
		n = 0;
	d = n - u.u_dsize;
	n =+ USIZE+u.u_ssize;
	if(estabur(u.u_tsize, u.u_dsize+d, u.u_ssize, u.u_sep))
		return;
	u.u_dsize =+ d;
	if(d > 0)
		goto bigger;
	a = u.u_procp->p_addr + n - u.u_ssize;
	i = n;
	n = u.u_ssize;
	while(n--) {
		copyseg(a-d, a);
		a++;
	}
	expand(i);
	return;

bigger:
	expand(n);
	a = u.u_procp->p_addr + n;
	n = u.u_ssize;
	while(n--) {
		a--;
		copyseg(a-d, a);
	}
	while(d--)
		clearseg(--a);
}
```

nはu.u_arg[0]で指定したbyte数を1 blockを32 word(64 byte)とするブロック数に変換したものである。u.u_tsizeはblock数でテキストセグメントのサイズを表している。nsegは引数にとったblock数を1セグメントを4K word(8KB)とするセグメント数に変換する。u.u_tsizeをnseg(u.u_tsize)*128でテキストセグメントを8KB境界に揃えたblock数に変換した上で、nから減算する。これはu.u_sep=0の場合、テキストとデータが分離されていないので、nからテキストセグメント分を除くことで、新しいデータセグメントのblock数を算出しているためである。

また、dは元のデータセグメントからの増減block数を表す。(増加する場合は正、減少する場合は負)

## データセグメントが縮小する場合(d <= 0)

copyseg(a-d, a)により元のスタックセグメントの位置(a-d)から縮小後のスタックセグメントの位置(a)に対してスタックのコピーが行われる。この時点では同じメモリ領域内でコピーが行われているだけであり、確保したメモリ領域に変更はないことに注意する。最後に、expand(i)を実行してはじめて不要な領域がmfreeで解放されてメモリの縮小が完了する。

## データセグメントが拡大する場合(d > 0)

最初にexpand(n)により拡張後の領域を確保する。expandでは新しい領域に対して丸ごとコピーが行われるだけなので、スタックセグメントの位置の調整は呼び出し側で行う必要がある。そこで、aを転送先のアドレス、a-dを転送元のアドレスとしてcopyseg(a-d, a)によりスタック領域をコピーする。最後に、clearseg(--a)により、データセグメントの増加した領域のみを0クリアする。

# 13章 ソフトウェア割り込み

# 16章 RKディスクドライバ

## devstart
rbp->xmemは拡張(extended)メモリアドレスを示し、PDP11-40/45では物理アドレスの拡張部にあたる上位2bit(bit17-16)が格納されている。

# 18章 ファイルのアクセスと制御

## iomove(bp, o, an, flag)

- bp バッファ領域へのポインタ
- o バッファ内オフセット
- an 転送バイト数
- flag read/write指定フラグ

iomoveはbp->b_addr[o]で与えられるバッファbpのアドレスb_addr[o]とu_baseとの間でデータを転送する。転送する方向はflagで指定され、flag=0の場合、bp->b_addr[o]からu_baseの方向であり、flag=1の場合、u_baseからbp->b_addr[0]の方向を意味する。また、u_baseが表すメモリ空間はu_segflgで指定され、u_segflg=0の場合、ユーザ空間を表し、u_segflg=1の場合、カーネル空間を表す。

```c
iomove(bp, o, an, flag)
struct buf *bp;
{
	register char *cp;
	register int n, t;

	n = an;
	cp = bp->b_addr + o;
	if(u.u_segflg==0 && ((n | cp | u.u_base)&01)==0) {
		if (flag==B_WRITE)
			cp = copyin(u.u_base, cp, n);
		else
			cp = copyout(cp, u.u_base, n);
		if (cp) {
			u.u_error = EFAULT;
			return;
		}
		u.u_base =+ n;
		dpadd(u.u_offset, n);
		u.u_count =- n;
		return;
	}
	if (flag==B_WRITE) {
		while(n--) {
			if ((t = cpass()) < 0)
				return;
			*cp++ = t;
		}
	} else
		while (n--)
			if(passc(*cp++) < 0)
				return;
}
```

## copyin(u.u_base, cp, n)

u.u_baseで指定されるユーザ空間のアドレスから、cpで指定されるカーネル空間のバッファ内アドレスに対してn byte転送する。

## copyout(cp, u.u_base, n)

cpで指定されるカーネル空間のバッファ内アドレスから、u.u_baseで指定されるユーザ空間のアドレスに対してn byte転送する。

## passc(c)

u_baseで指定されたアドレスにcを転送する。u_segflgが0の場合、u_baseはユーザ空間のアドレスとして扱われ、u_segflgが1の場合、カーネル空間のアドレスとして扱われる。その際、u_baseとu_offsetはインクリメントされ、u_countはデクリメントされる。(以下のcpassも同様)

ユーザからのread要求に対して、カーネルのバッファ領域からユーザが指定するアドレスに対してデータを転送する処理に利用される。

## cpass()

u_baseで指定されたアドレスから1byte取得して返す。

ユーザからのwrite要求に対して、ユーザが指定するアドレスからデータを取得し、カーネルのバッファ領域に転送する処理に利用される。

## readiからの呼び出し

iomove(bp, on, n, B_READ);

bp = bread(デバイス番号, ブロック番号);
on = ブロック内オフセット
n = 転送バイト数
B_READ = 読み出し
