---
title: "Tunix Code Reading 2"
date: 2020-11-11T13:07:14+09:00
draft: false
---

「オペレーティングシステム 設計と実装」(Robert Switzer 著) という教科書の題材である Tunix のソースコードリーディングの続き。

## FM

今回は実際に FM (File Manager) サブシステムの動作を確認してみる。FM はファイルシステム関連のサービスを提供するサブシステムであり、UNIX のように inode によりファイルやディレクトリを実現し、アクセス権やマウント等の機能を備えている。FM の処理の一例として、open(2) システムコールの流れを追うことにする。

### ファイルシステム

ファイルシステムは mkfs.c というユーティリティプログラムで作成することになっている。このプログラムでは、以下のようなフォーマットでデータブロックを初期化する。

- ブートブロック
- スーパーブロック
- ルートディレクトリ
- フリーブロックリスト

ブロックサイズを 1024 バイトを基本とする。

### disk inode と in-core inode

inode とはファイルの実態であるが、ディスク内に格納されている inode を disk inode と呼び、メモリ上にキャッシュした inode を in-core inode と呼ぶ。in-core inode は disk inode の情報に加えて、参照カウントやフリーリスト等のメンバが加えられている。


sp->tot_size = 512 // disk block の総数は 512 個
sp->ninodes = 256 // disk inode の数は 256 個

INO_PER_BLK = BLOCK_SIZE / sizeof(D_INODE) = 1024 / 96 = 10
1 block あたり 13 disk inode 格納可能
niblks = 1 + 255 / INO_PER_BLK = 26 // 256 個の disk inode は 26 block 消費
sp->iblocks = niblks

sp->fst_data = 2 + niblks = 2 + 26 = 28 // fst = first で、boot block と super block と inode block の次から始まる data block の位置を表している?

sp->max_size = NO_PER_BLOCK * NO_PER_BLOCK * NO_PER_BLOCK // 1 block あたりに記録できる番号 (4 byte) の個数の 3 乗 -> ファイルの最大サイズ

sp->blk_free = sp->tot_size - sp->fst_data - 1 = 512 - 28 - 1 = 483 // 空ブロックの数は 489 個 (-1 しているのはなぜ?)
sp->ino_free = sp->ninodes - 1 = 255 // フリーな disk inode の数は 255 個 (-1 しているのはなぜ?)
sp->i_indx = N_INOS = 128 // 空き inode 配列の先頭の inode 番号

N_INOS = 128 // 空 inode 配列の要素数
N_BLKS = 180 // 空ブロック配列の要素数
indx = (509 - niblks) % N_BLKS = (509 - 26) % 180 = 123
sp->b_indx = indx // 空き data block 配列のインデックスは 121(?)

sp->free_ino[N_INOS] // 空き inode 番号配列。disk inode は 256 個あるのに対し、128 個しか持たないため、すべての空き inode を管理しているわけではない。この配列で管理する inode 番号を消費しつくしたら、実際に disk inode を直接見に行き空き inode を探すようになる。mode 番号を見れば空いているかどうかは区別つく。

```c
    for ( i = 0; i < N_INOS; ++i )
        sp->free_ino[i] = (SHORT) (N_INOS - i + 1);
```

sp->free_ino[  0] = 128 -   0 + 1 = 129
sp->free_ino[  1] = 128 -   1 + 1 = 128
...
sp->free_ino[127] = 128 - 127 + 1 =   2


sp->free_blk[N_BLKS] // 空きブロック番号配列。

```c
    for ( i = 0; i < indx; ++i )
        sp->free_blk[i] = (BLKNO) (indx - i + niblks + 2);
```

free_blk[  0] = 123 -   0 + 26 + 2 = 157
free_blk[  1] = 123 -   1 + 26 + 2 = 156
...
free_blk[122] = 123 - 122 + 26 + 2 =  29


### open(2)

FM が OPEN メッセージを受け取った時の処理が以下。

```c
        case OPEN:
            {
                INT     pid = msg.pid;
                char    path[64];

                (void) STRCPY(path, msg.str1);
                msg.arg1  = (ARGTYPE) proc_open(pid, path,
                                        (SHORT)msg.arg1, (SHORT)msg.arg2);
                msg.errno = errno;
                reply(pid, UNCRITICAL);
            }
            break;
```

proc_open() がシステムコールの本体であるが、msg を参照渡しではなく値渡しにしている点や、局所変数 path にわざわざコピーして渡している点は本質的。というのも、msg は FM の複数のスレッドから共有される変数であるためで、スレッド間による競合を避けるために必要なデータはスレッド自前のスタック領域にコピーして利用する必要がある。ただし、前回述べたように、このスレッドライブラリは end_thread() や sleep() により自発的に実行権を開放しない限り別のスレッドに切り替わることはないため、厳密には sleep() (end_thread() はスレッドが終了するので競合しても問題なし) を呼ぶ可能性があるときに限り、スタックへのコピーが必要になる。

proc_open() 実行後は、大域変数 errno を介して msg.errno に終了ステータスを格納後、reply(pid) によりメッセージの送信元 (msg.pid に入っている) に返信する。

### proc_open

実際の proc_open() は次のようになっている。

```c
int proc_open(pid, path, mode, perms)

INT     pid;
char    *path;
SHORT   mode;
SHORT   perms;

{
    ENTRY   *ep = find_entry(pid);
    PDATA   *pdp = &ep->pdata; 
    int     fd; 

    if ( ep == NIL_ENTRY )
        ERROR(EINVAL)

    for ( fd = 0; fd < N_FILES; ++fd )
        if ( ep->ft_indx[fd] == -1 )
            break;

    if ( fd >= N_FILES )
        ERROR(EMFILE)

    perms &= ~(ep->umask);

    ep->cl_exec[fd] = FALSE;

    if ( (ep->ft_indx[fd] = sys_open(pdp, path, mode, perms)) == -1 )
        return -1;
    else
        return fd;
}
```

まず、find_entry() はプロセスファイルテーブルリスト

```c
static ENTRY procs[N_PROCS];
```

からプロセス pid 用のエントリを検索する。その ep のファイルテーブル番号リスト (ft_indx) から未使用のエントリ fd を検索する。この fd がファイルディスクリプタなのだろう。続いて sys_open() を呼び、得られたファイルテーブル番号を ep->ft_indx[fd] に設定する。これで指定されたファイルに対するファイルテーブルが設定され、それがさらにプロセスファイルテーブル中のファイルテーブル番号リストに設定され、そのエントリ番号がファイルディスクリプタとして open(2) の戻り値となる。

ついでに sys.c にあるファイルテーブルも見ておく。

```c
typedef struct
{
    INODE   *inode; // inode へのポインタ
    LONG    offset; // ファイルオフセット
    INT     refcnt; // 参照カウント
    SHORT   mode;   // モード (RD_ONLY, WR_ONLY 等)

} ENTRY;

static ENTRY    ftable[TABSIZE];
```

見ての通り、ファイル操作に必要な inode や offset が定義されている。

このファイルテーブル番号リストとファイルテーブルの関係は初見ではすごい違和感がある。なぜファイルテーブルを個々のプロセスが持つプロセスファイルテーブルに直接作成せず、ファイルテーブル番号リスト (ft_indx) というものを一段階経由してファイルテーブルにアクセスするのだろうか。

### sys_open

少し長いので、本質的な部分以外は省略している。やっていることは path で指定されたファイルに対応する inode を探索し、参照カウントやアクセス時刻更新するとともに、ファイルテーブルのエントリを初期化して返すことである。open 時の mode 指定により、ファイルの新規作成や切り詰めの処理も行う。

```c
int sys_open(pdp, path, mode, perms)

PDATA   *pdp;
char    *path;
SHORT   mode;
SHORT   perms;

{
    SHORT   perm;
    SHORT   type;
    INODE   *ip  = dir_path(pdp, path, FIND); // path から inode を探索
    LONG    pos, now;

    // ...

    if ( errno )                    /* try to create new file       */
    {
        ip = dir_path(pdp, path, CREATE); // ファイルを新規作成
        INO_SET_MODE(ip, perms | IFREG);
        now = TIME();
        INO_SET_ATIME(ip, now);
        INO_SET_MTIME(ip, now);
        INO_SET_CTIME(ip, now);
        INO_IS_DIRTY(ip);
    }

    perm = ino_perms(ip, pdp->uid, pdp->gid);
    // パーミッション違反確認 (省略)

    type = INO_MODE(ip) & IFMT;
    // ディレクトリへの書き込み確認 (省略)

    // mode に O_TRUNC が指定されていたら ino_trunc(ip) (省略)

    // ファイルポインタを設定
    pos = ( (mode & O_APPEND) == O_APPEND ) ? INO_SIZE(ip) : 0L;

    INO_INCREF(ip);            // inode の参照カウントをインクリメント
    INO_SET_ATIME(ip, TIME()); // inode のアクセス時刻を更新
    INO_IS_DIRTY(ip);          // inode を dirty に設定

    ino_put(ip); // inode を解放

    // type == IFIFO の場合など (省略)

    return new_entry(ip, pos, mode);
}
```

### ino_perms

sys_open() はファイルのパーミッションを確認するために以下の関数を呼んでいる。見ての通り、uid/gid が inode のそれと一致した場合には、アクセス権を下位 3 bit (rwx) として返す。

```c
SHORT ino_perms(ino, uid, gid)

INODE   *ino;
SHORT   uid;
SHORT   gid;

{
    SHORT   perms;

    perms = ino->mode & 07;

    if ( ino->gid == gid )
        perms |= ((SHORT)(ino->mode & 070) >> 3);

    if ( ino->uid == uid )
        perms |= ((SHORT)(ino->mode & 0700) >> 6);

    return perms;
}
```

### new_entry

sys_open() でオープンするファイルの inode が確定したら、new_entry() によりファイルテーブルと inode の紐づけを行う。new_entry() はファイルテーブル ftable の空いているエントリを確保し、inode へのポインタやファイルオフセット等の情報を書き込み、ftable のエントリへのポインタを返す。

```c
static int new_entry(ino, offset, mode)

INODE   *ino;
LONG    offset;
SHORT   mode;

{
    ENTRY   *ep;

    for ( ep = ftable; ep < ftable + TABSIZE; ++ep )
        if ( ep->inode == NIL_INODE )
            break;

    if ( ep >= ftable + TABSIZE )
        ERROR(ENFILE)

    ep->inode  = ino;
    ep->offset = offset;
    ep->mode   = mode;
    ep->refcnt = 1;

    return (ep - ftable);
}
```

ちなみに、sys_open() をよく見ると new_entry() に inode へのポインタを渡す前に ino_put() で当該 inode を解放してしまっているが大丈夫なのだろうか? (他のサブシステムから操作されなければ問題ないのであろう。)


### dir_path

続いて sys_open() から path -> inode 変換に用いられている dir_path() を見ていく。引数の op に指定した FIND/DELETE/CREATE に応じた inode の操作が行われる、という API としてはやや微妙な感じ。これも少し長いので本質的な部分を残して省略する。

```c
INODE *dir_path(pdp, path, op)

PDATA   *pdp;
char    *path;
int     op;                 /* what to do if found (or not found)   */

{
    SHORT   ino; 
    LONG    offset; 
    INODE   *ip = dir_descend(pdp, path, &ino, &offset);
    char    name[14];
    int     is_dir;

    if ( op == FIND )
    {
        if ( ino == 0 )         /* last component not found     */
            IERROR(ENOENT)
        ino_put(ip);
        ip = ino_get(INO_DEV(ip), ino);
        return ip;
    }

    if ( op == DELETE )
    {
        // 削除可能な条件の確認 (省略)
        return dir_rm(ip, offset, is_dir);
    }

    if ( ino )                  /* op == CREATE but entry found */
    {
        ino_put(ip);
        return ino_get(INO_DEV(ip), ino);
    }

    /* we only get here if last component missing and op == CREATE  */
    last_component(path, name); // path から basename を name にコピーする
    return dir_create(ip, offset, name, pdp->uid, pdp->gid);
}
```

まず、dir_descend() は path の最後のディレクトリ部分の inode を返すことに注意。また、ファイルに関する情報として、ino と offset には次の値を返す。

- ファイルが存在する場合、ino はファイルの inode 番号、offset はディレクトリエントリのオフセット
- ファイルが存在しない場合、ino は 0、offset はディレクトリエントリの最初の空エントリ

FIND でファイルが見つかった場合 (ino != 0)、ディレクトリの inode を一旦 ino_put() してから改めてファイルの inode を ino_get() する。存在するファイルに対して CREATE が指定された場合も同様。

また、CREATE/DELETE によりファイルを新規作成または削除する場合、dir_create()、dir_rm() にディレクトリエントリのオフセット情報が必要になることに注意。

### ino_get

ino_get() は指定したデバイス番号と inode 番号に対応したメモリ内 inode へのポインタを返す。頻繁にアクセスされる inode をキャッシュするためにハッシュを用いている (ハッシュはチェイン法)。まずハッシュに存在するか否かを調べ、ハッシュに存在し、かつロックされていなければ、その inode へのポインタをロックして返す。このとき、リファレンスカウントが 0 の場合はフリーリストから外す。リファレンスカウントが 0 の場合であってもハッシュには残してあるんだね。ハッシュに存在するが、ロックされている場合、当該 inode ポインタをイベントとして、sleep() する。やがて他のスレッドにより wakeup() されるが、ここでその inode を獲得できるかどうかは自明ではない。つまり、sleep() 中であった他のスレッドも同時に wakeup() されている可能性があるため、すでにロックされてしまっている可能性があるし、さらに言えば、ハッシュにも存在しない可能性すらある。というわけで、sleep() から起きたときはどこから再開すればよいか、というと、全部最初からやり直し、となる。

ハッシュに存在しない場合は、フリーリストから inode 格納用バッファを取り出しハッシュにつなげる。(他のハッシュチェインにつながっている場合があるので、その場合は一旦外してから。) 最後に inode をリファレンスカウント 1 とロック状態に初期化し、sup_iget() で実際にディスクから inode 情報を読み込む。

```c
INODE   *ino_get(dev, i_no)

DEV     dev;
SHORT   i_no;

{
    INODE   *ino, *hp;

    for (;;)
    {
        if ( (ino = hash_search(dev, i_no)) != NIL_INODE )
        {
            if ( ino->flags & I_LOCKED )
            {
                sleep((EVENT)ino);
                continue;
            }

            if ( ino->refcnt == 0 )     /* it's on free list; take it off   */
            {
                ino->next->prev = ino->prev;
                ino->prev->next = ino->next;
            }

            ++ino->refcnt;

            ino->flags |= I_LOCKED;

            return ino;
        }

        /* inode not in inode cache */

        if ( free_list.next == &free_list )     /* out of in-core inodes    */
        {
            errno = ENFILE;
            return NIL_INODE;
        }

        /* remove a new inode from free list */
 
        ino = free_list.next;

        ino->next->prev = ino->prev;
        ino->prev->next = ino->next;

        /* reset inode number and file system */

        ino->dev = dev;
        ino->ino = i_no;

        /* remove inode from old hash queue, place on new one */

        hp = &buckets[hash_fkt(dev, i_no)];

        if ( ino->succ != NIL_INODE )
            ino->succ->pred = ino->pred;
        ino->pred->succ = ino->succ;

        ino->succ = hp->succ;
        ino->pred = hp;
        hp->succ  = ino;

        /* initialize inode */ 
 
        ino->refcnt = 1; 
        ino->flags  = I_LOCKED; 
 
        /* read inode from disk */

        sup_iget(ino);

        return ino;
    }
}
```


### ino_put

ino_put() は ino_get() で確保したメモリ内 inode を解放する。リファレンスカウントが 0 になった場合、当該 inode をフリーリストに戻す。その際、リンク数が 0 になった場合、すなわち、ファイルが完全に削除された場合、ino_trunk() により当該 inode に所属する全てのブロックを解放し、sup_ifree() により inode 番号自身も解放する。(スーパーブロックが管理する free_ino[] にその番号を戻す。) 

また、ファイルに対する変更や inode 自体への更新があり dirty フラグが立っている場合、sup_iput() により disk inode への書き込みを行う。最後に、ロックフラグを落とし、当該 inode ポインタを待っているスレッドを wakeup() する。


```c
void ino_put(ino)

INODE *ino;

{
    ino->flags |= I_LOCKED;

    --ino->refcnt;

    if ( ino->refcnt == 0 )
    {
        if ( ino->nlink == 0 )
        {
            ino_trunc(ino);

            ino->mode   = 0;
            ino->flags |= I_DIRTY;

            sup_ifree(ino->dev, ino->ino);
        }

        /* put inode on free list */ 

        ino->prev = free_list.prev;
        ino->next = &free_list;

        free_list.prev->next = ino;
        free_list.prev       = ino;
    }

    /* 
        If file accessed or inode changed or file changed 
        update disk inode. 
    */ 
 
    if ( ino->flags & I_DIRTY )
    {
        ino->ctime = TIME();

        sup_iput(ino);

        ino->flags &= ~I_DIRTY;
    }

    /* release inode lock */

    ino->flags &= ~I_LOCKED;
    wakeup((EVENT)ino); 
}
```

以上が open(2) システムコールのリクエストを FM に送った際の処理の流れだ。全ての関数は追っていないが、上に挙げた関数に目を通すことで、大枠のイメージはつかめたかな。
