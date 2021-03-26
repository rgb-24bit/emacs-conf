## My Emacs Config ##

### Install

```
$ git clone https://github.com/rgb-24bit/emacs-conf.git --recursive --shallow-submodules ~/.emacs.d
```

Update when there are new submodules:
```
$ git pull
$ git submodule update --init --recursive --depth 1
```

Dependencies:
+ [ripgrep](https://github.com/BurntSushi/ripgrep)
+ [eaf](https://github.com/manateelazycat/emacs-application-framework)
+ [lsp](https://microsoft.github.io/language-server-protocol/) for golang, rust

### Known issues

1. git warning: Clone succeeded, but checkout failed, ref [github - warning: Clone succeeded, but checkout failed - Stack Overflow](https://stackoverflow.com/questions/39542177/warning-clone-succeeded-but-checkout-failed)
```
$ git config --system core.longpaths true
```

2. git clone success, but submodule content modified:
```
$ git submodule foreach 'git checkout -f'
```
