# agda-language-server

Language Server Protocol for Agda

To be developed alongside [agda-mode-vscode](https://github.com/banacorn/agda-mode-vscode)

## Can I try it now?

Yes.

However, this project not available on Hackage for the moment.
You will have to pull it from GitHub

```
git clone git@github.com:banacorn/agda-language-server.git
```

Checkout to a version that is compatible with your agda-mode on VS Code

```
git checkout v0.0.1.0
```

Here are the versions that work (on my machine)

| Language Server | [agda-mode](https://marketplace.visualstudio.com/items?itemName=banacorn.agda-mode) |
| --------------- | ------------- |
| [v0.0.1.0](https://github.com/banacorn/agda-language-server/releases/tag/v0.0.1.0)        | v0.2.8 |


Build it with the package manager you hate the least. 

I use Stack. But seriously, I couldn't care less.

```
stack install
```

Once you have `als` installed on your machine. Open VS Code and go to agda-mode's settings. Enable "Agda Mode: Agda Language Server".

![截圖 2021-03-03 下午6 53 11](https://user-images.githubusercontent.com/797844/109795292-b709cc80-7c51-11eb-909d-982f647bd282.png)

## Current features

More stuff are clickable in the panel after loading (<kbd>C-c</kbd> <kbd>C-l</kbd>)

![截圖 2021-03-03 下午6 59 43](https://user-images.githubusercontent.com/797844/109796026-a017aa00-7c52-11eb-9e03-d21bca12e603.png)


## Why make it standalone?

* for less impact on the Agda codebase
* to help [decouple the Agda codebase](https://github.com/agda/agda/projects/5)
* we can always merge it back to Agda later anyway
