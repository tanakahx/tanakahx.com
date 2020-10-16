---
title: "Android Studio 0.8.6"
date: 2014-08-23T00:09:23+09:00
draft: false
tags: ["Android"]
---
Android Studio がいつの間にか 0.8.1 から 0.8.6 にアップデートされていた。以前、Android Studio 0.8.1 で作成したプログラムを実機で動かそうとしたときに `build.gradle` の設定を[このように](/blog/2014/07/09/android-studio-0-dot-8-1/)設定していた。

0.8.6 にアップデートしたついでに設定方法を見直してみた。まずプロジェクトを作成するときに `Minimum SDK` の項目に `API 19: Android 4.4 (KitKat)` を指定する (最新は Android L であるがこれを選択するとうまくいかなかった)。API 15 の実機で動作させるため、`build.gradle` の設定は以下のようにした。

```console
apply plugin: 'com.android.application'

android {
    compileSdkVersion 19
    buildToolsVersion "20.0.0"

    defaultConfig {
        applicationId "com.example.tanaka.myapplication"
        minSdkVersion 15 // Changed this value from 19 to 15
        targetSdkVersion 19
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
