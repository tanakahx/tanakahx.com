---
title: "Installing Emacs on WSL2"
date: 2021-05-23T22:40:55+09:00
draft: false
---

- OS は Ubuntu 20.4 on WSL2
- エディタは Emacs 27.2 
- X サーバは VcXsrv

な環境を整えた際のメモ。

### Emacs のビルド

最新版を使いたかったので、ソースコードからビルドした。
[Install Emacs 27.2](https://github.com/hubisan/emacs-wsl#install-emacs-272) の通りに進めたが、特に問題はなかった。

```sh
$ sudo apt update
$ sudo apt install -y autoconf automake bsd-mailx dbus-x11 debhelper dpkg-dev \
    libacl1-dev libasound2-dev libdbus-1-dev libgif-dev libgnutls28-dev libgpm-dev \
    libgtk-3-dev libjansson-dev libjpeg-dev liblcms2-dev liblockfile-dev libm17n-dev \
    libncurses5-dev liboss4-salsa2 libotf-dev libpng-dev librsvg2-dev \
    libselinux1-dev libsystemd-dev libtiff-dev libxml2-dev libxpm-dev procps quilt \
    sharutils texinfo zlib1g-dev gvfs language-pack-en-base libasound2 libaspell15 \
    libasyncns0 libatk-bridge2.0-0 libatk1.0-0 libatspi2.0-0 libbrotli1 \
    libcairo-gobject2 libcairo2 libcanberra-gtk3-0 libcanberra-gtk3-module \
    libcanberra0 libcroco3 libdatrie1 libdb5.3 libdrm2 libegl1 libenchant1c2a \
    libepoxy0 libflac8 libfontconfig1 libfreetype6 libgbm1 libgdk-pixbuf2.0-0 \
    libgif7 libgl1 libglvnd0 libglx0 libgpm2 libgraphite2-3 libgstreamer-gl1.0-0 \
    libgstreamer-plugins-base1.0-0 libgstreamer1.0-0 libgtk-3-0 libgudev-1.0-0 \
    libharfbuzz-icu0 libharfbuzz0b libhyphen0 libice6 libicu66 libjansson4 \
    libjavascriptcoregtk-4.0-18 libjbig0 libjpeg-turbo8 liblcms2-2 liblockfile1 \
    libltdl7 libm17n-0 libnotify4 libnss-mdns libnss-myhostname libnss-systemd \
    libogg0 liborc-0.4-0 libotf0 libpango-1.0-0 libpangocairo-1.0-0 \
    libpangoft2-1.0-0 libpixman-1-0 libpng16-16 libpulse0 librsvg2-2 libsasl2-2 \
    libsecret-1-0 libsm6 libsndfile1 libsoup2.4-1 libssl1.1 libstdc++6 libtdb1 \
    libthai0 libtiff5 libvorbis0a libvorbisenc2 libvorbisfile3 libwayland-client0 \
    libwayland-cursor0 libwayland-egl1 libwayland-server0 libwebp6 libwebpdemux2 \
    libwoff1 libx11-6 libx11-xcb1 libxau6 libxcb-render0 libxcb-shm0 libxcb1 \
    libxcomposite1 libxcursor1 libxdamage1 libxdmcp6 libxext6 libxfixes3 libxi6 \
    libxinerama1 libxkbcommon0 libxml2 libxpm4 libxrandr2 libxrender1 libxslt1.1 \
    libyajl2
```

```sh
$ cd ~/.local/src
$ wget https://ftp.gnu.org/pub/gnu/emacs/emacs-27.2.tar.gz
$ tar -xzvf emacs-27.2.tar.gz
```

```sh
$ cd ~/.local/src/emacs-27.2
$ ./configure --prefix=$HOME/.local/emacs-27.2 --with-cairo
$ make
$ sudo make install
```

### VcXsrv の設定

高 DPI 環境だと GUI の文字がボケて見えるので、VcXsrv の設定で回避する。

- "C:\Program Files\VcXsrv\vcxsrv.exe" を右クリックからプロパティを選択
- 互換性タブから「高 DPI 設定の変更」を選択
- 高い DPI スケールの銅をを上書きします。」にチェック
-「拡大縮小の実行元」から「アプリケーション」を選択

コマンドプロンプトから VcXsrv を実行 (-dpi auto をつけることがポイント)
```
start "" "C:\Program Files\VcXsrv\vcxsrv.exe" :0 -multiwindow -clipboard -dpi auto -wgl -ac
```

### WSL2 上の設定

.bash_aliases に以下の設定を追加した。

```sh
alias ema="export DISPLAY=$(ip route | awk '/^default/{print $3; exit}'):0.0 export LIBGL_ALWAYS_INDIRECT=1 && setxkbmap -layout us && setsid emacs"
```