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

Sync when submodule updated:
```
$ git submodule update --init
```

Dependencies:
+ [ripgrep](https://github.com/BurntSushi/ripgrep)
+ [eaf](https://github.com/manateelazycat/emacs-application-framework)
+ [lsp](https://microsoft.github.io/language-server-protocol/) for golang, rust

### Code Style

+ public variables `emacsc-name`, private variables `emacsc--name`
+ interactive func `emacsc/name`, general func `emacsc//name`

### Known issues

1. git warning: Clone succeeded, but checkout failed, ref [github - warning: Clone succeeded, but checkout failed - Stack Overflow](https://stackoverflow.com/questions/39542177/warning-clone-succeeded-but-checkout-failed)
```
$ git config --system core.longpaths true
```

2. git clone success, but submodule content modified:
```
$ git submodule foreach 'git checkout -f'
```

3. `straight-freeze-versions` with error `*Caches are still outdated; something is seriously wrong*`

I use require to load the configuration file, which makes the straight related functions not re-executed when the init file is reloaded.
You can restart emacs to execute `straight-freeze-versions`. [raxod502/straight.el#437](https://github.com/raxod502/straight.el/issues/437)

### Reference configuration

+ [purcell/emacs.d](https://github.com/purcell/emacs.d)
+ [syl20bnr/spacemacs](https://github.com/syl20bnr/spacemacs)
+ [hlissner/doom-emacs](https://github.com/hlissner/doom-emacs)
+ [manateelazycat/lazycat-emacs](https://github.com/manateelazycat/lazycat-emacs)
