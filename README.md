videoutils
----------

A grab-bag of video processing tools.

Installation
------------

## Prerequisites

### ffmpeg

Ubuntu:
```bash
sudo apt-get install ffmpeg
sudo ldconfig
```

osx homebrew:
```bash
brew install ffmpeg --with-vpx --with-vorbis --with-libvorbis --with-vpx --with-vorbis --with-theora --with-libogg --with-libvorbis --with-gpl --with-version3 --with-nonfree --with-postproc --with-libaacplus --with-libass --with-libcelt --with-libfaac --with-libfdk-aac --with-libfreetype --with-libmp3lame --with-libopencore-amrnb --with-libopencore-amrwb --with-libopenjpeg --with-openssl --with-libopus --with-libschroedinger --with-libspeex --with-libtheora --with-libvo-aacenc --with-libvorbis --with-libvpx --with-libx264 --with-libxvid
```

### nix via reflex-platform

```bash
git clone git@github.com/CBMM/videoutils
cd videoutils
git submodule update --init --recursive
```

If you haven't installed nix, install it now:

```bash
deps/reflex-platform/installNix.sh
```

Compile one of the utilities, e.g. *CutAtFrames*

```bash
cd CutAtFrames
../deps/reflex-platform/work-on ghc ./. --run "cabal configure && cabal build"
```
