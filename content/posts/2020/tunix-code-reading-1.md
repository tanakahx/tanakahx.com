---
title: "Tunix Code Reading 1"
date: 2020-11-09T21:10:03+09:00
draft: false
---

昔、「オペレーティングシステム 設計と実装」(Robert Switzer 著) という OS の教科書があった。同じ書名に Andrew Tanenbaum 先生の Minix の教科書があるが、これは別の書籍。本の前半は理論編で、後半はソースコード編という構成は、Minix 本と作りは似ている。

この教科書では Minix 本と同じく教育用のマイクロカーネル Tunix を作成している。Minix が実機上で動作する完全な OS として実装されているのに対し、この Tunix は Unix 系 OS のユーザランドで動作する疑似 OS となっている。OS を主要コンポーネントごとにサブシステムとして分割し、それらを独立したユーザプロセスとして実装する。カーネルはそれらのサブシステム間のメッセージを仲介することに特化したプログラム、すなわちマイクロカーネルとして位置づけ、これも独立したプロセスとして実装する。実際のメッセージ送受信は UNIX IPC を利用する。

この教科書が出版されたのは 1993 年のようだが、なんと 2020 年の今もソースコードが存在していることが判明した。懐かしさのあまりソースコードを眺めてみたが、随分昔に読んだきりでほとんど覚えていない。せっかくなので、もう一度ソースコードを読み返しつつ、今の時代にも教育的価値があるか考えてみることにする。

Tunix のソースコードでは、簡易スレッドを独自実装しており、OS のサブシステムは自身のプロセス内でメッセージループを回すために使用している。まずはこのスレッド機能から見ていくことにする。

## スレッドライブラリ

### THREAD 構造体

```c
typedef struct
{
    int     state;
    EVENT   event;
    int     regs[NR_REGS];
    int     stack[STK_SIZE];
} THREAD;

static THREAD   threads[N_THREADS];
```

レジスタの保存領域とスタック領域からなるスレッドプール。

### init_threads()

スレッドプールを初期化し、state を FREE に設定する。また、main() におけるコンテキスト情報を main_regs に保存するために、save_context(main_regs) を実行する。

### save_context(regs)

現在のスレッドのコンテキスト情報 (レジスタ値や戻り番地等) を regs で指定した領域に保存する。ここで注意すべき点は、save_context() の caller (init_hreads() や sleep() 等の関数) のさらに caller のコンテキストに戻すための情報を保存するが期待されているため、2 つ前のコールスタックまで参照している点である。

main() -> init_threads() -> save_context() というコールスタックを例にとると、以下のようなレイアウトでレジスタを保存する。

```
regs[0] = init_threads() のスタックフレームアドレス (ebp + 4: init_threads() からの戻り番地が積まれているアドレス)
regs[1] = main() のスタックフレームアドレス (ebp)
regs[2] = esi
regs[3] = edi
regs[4] = ebx
regs[5] = ecx
regs[6] = edx
regs[7] = init_threads() からの戻り番地
```

### start_thread()

スレッドプールの中から state が FREE のものを探し出し、state を ACTIVE に遷移させるとともに、次の new_context により、新スレッドのスタックをこの関数を呼んだ時点のスタックフレームで初期化する。

### new_context(sp)

main() -> start_thread() -> new_context() と呼ばれることが想定されており、このルーティンは main() のスタックフレームを ebp レジスタから割り出し、引数に指定したバッファ領域 sp に丸っとコピーする。以降、このバッファ領域をこのスレッドのスタックとして使用する。戻る時は ebp と esp を指定したスタックを指すように差し替えて ret することに注意。

### sleep(event)

あるイベント (event) が起きるまでこれ以上実行できない場合、スレッドは sleep() を実行して自ら実行権を明け渡す。現在のスレッドの state を FREE に設定し、待ち条件として event を登録し、スレッドプールから別の ACTIVE なスレッドを検索し切り替える。(ない場合は main_regs に保存したコンテキストに戻す。) この時、切り替え前のスレッドのコンテキストを save_context() で保存し、切り替え後のスレッドのコンテキストを次の restore_context() で復元する。

### restore_context(regs)

save_context() で保存したコンテキスト情報をレジスタに復帰する。注意点としては、regs[0] には save_context() の caller のスタックフレームアドレスが保存されており、esp をこの値に設定し、さらに regs[7] に保存されている caller の caller への戻り番地を (esp) に書き込むことで、restore_context からの ret 先が caller の caller に置き換わる点である。

つまり、func() -> sleep() -> save_context() で保存されたコンテキストは、復帰すると func() の sleep() 直後から再開することになる。

### end_thread

現在のスレッドの state を FREE に設定し、スレッドプールから別の ACTIVE なスレッドを検索し切り替える。(ない場合は main_regs に保存したコンテキストに戻す。) その時、sleep() 同様に、切り替え後のスレッドのコンテキストを restore_context() で復元する。

### wakeup(event)

event を待っているスレッドを全て起床させる。ただし、wakeup() 実行後に直ちにスレッドが切り替わることはなく、wakeup() を実行した現スレッドが自発的に sleep() もしくは end_thread() しない限り、そのまま実行が継続される。

## メッセージループ

次に、このスレッドライブラリを使ってサブシステムがどのように構成されているかを見ていく。まず、各サブシステムは次のようなメッセージループを実行する。カーネルからメッセージを受信し、そのメッセージに書かれているコマンドをディスパッチする構成だ。

```c
int main()
{
    init_threads(); // main_regs にコンテキストを保存

    for (;;) {
        receive(&msg); // メッセージを受信;

        start_thread();

        switch (msg.cmd) { // メッセージをデコード
            case OPEN:
                proc_open();
                break;
            case CLOSE:
                proc_close();
                break;
            // ...
        }

        end_thread();
    }
}
```

個々のメッセージ処理(proc_open() や proc_close() 等)の中では、上記の sleep() や wakeup() を実行してもよい。sleep() や end_thread() を実行した差に、別の ACTIVE なスレッドが見つからない場合、init_threads() で main_regs に保存したコンテキストを復元し、制御は init_threads() の直後の命令から実行を再開する。

Tunix のマイクロカーネルとしてのコンセプトは、プロセス管理やメモリ管理といった各サブシステムが上記のようなメッセージループを実行し、カーネルはサブシステム間のメッセージを適切にパッシングする機能に特化することにある。

### MSG

Tunix では、プロセスがシステムコールを実行すると、プロセスとカーネル、カーネルとサブシステムの間における、System V message queue を介したメッセージパッシングとしてエミュレートされる。実際の bare metal 環境ではトラップやコールゲートを使って実装される。

```c
typedef struct
{
    long    mtype;
    INT     class;              /* CRITICAL, UNCRITICAL,...         */
    INT     queue;              /* pid of recipient                 */
    INT     pid;                /* pid of sender                    */
    INT     chan;               /* sleep channel used by managers   */
    INT     cmd;                /* operation desired                */
    int     errno;              /* the error no. after a call       */
    ARGTYPE arg1;               /* the arguments for system calls   */
    ARGTYPE arg2;
    ARGTYPE arg3;
    ARGTYPE arg4;
    ARGTYPE arg5;
    ARGTYPE arg6;
    ARGTYPE arg7;
    ARGTYPE arg8;
    char    str1[64];
    char    str2[64];
} MSG;
```

メッセージは上記のような構造体になっている。cmd メンバにシステムコールを指定し、argN に引数を指定した上で、カーネルに対して msgsnd(2) を実行し、カーネルからの返答を msgrcv(2) で受け取る構成になる。