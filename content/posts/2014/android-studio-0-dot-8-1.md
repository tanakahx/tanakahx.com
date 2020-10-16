---
title: "Android Studio 0.8.1"
date: 2014-07-09T23:35:30+09:00
draft: false
tags: ["Android"]
---
Android Studio 0.8.1 でプロジェクト作成してみたが、[INSTALL_FAILED_OLDER_SDK] というエラーが起きてしまい実行することができなかった。AVD や SDK のバージョンをかえてみたり実機で実行してみたけど結果は同じ。同様のエラーがないか検索してみると、app/src/build.gradle の compileSdkVersion をアプリケーションを実行する AVD や実機のバージョンに合わせればよいらしいことがわかった。今回は Android 4.0.3 (API 15) の実機が手元にあったので、以下のような設定にした。同時に、Android SDK Manager で Android 4.0.3 (API 15) の SDK Platform もインストールしておいた。

``` console
apply plugin: 'com.android.application'

android {
    compileSdkVersion 15 // Changed this value from android-L to 15
    buildToolsVersion '20.0.0'

    defaultConfig {
        applicationId "com.example.tanaka.myapplication"
        minSdkVersion 15
        targetSdkVersion 20
        versionCode 1
        versionName "1.0"
    }
    buildTypes {
        release {
            runProguard false
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
        }
    }
}

dependencies {
    compile fileTree(dir: 'libs', include: ['*.jar'])
}
```
