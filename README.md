# Omnibox

### Lightweight completion/selection system for Emacs.  

![omnibox](https://github.com/sebastiencs/omnibox/raw/screenshots/omnibox.png)
[[More screenshots]](https://github.com/sebastiencs/omnibox/tree/screenshots)

Require Emacs >= 26.  
Not compatible with Emacs in terminal  

Usage:
```el
(use-package omnibox
  :config
  (global-set-key (kbd "M-x") 'omnibox-M-x)
  (omnibox-setup))
```

It replaces Helm in my daily usage but the package is still under development so don't consider it stable.
