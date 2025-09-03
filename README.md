# LSP-PROXY (formerly LSP-COPILOT)

*This project has been renamed from lsp-copilot to lsp-proxy to better reflect its core functionality as a communication proxy and aggregator between Emacs and multiple LSP servers. The name change does not affect the project's features or goals.*

## Introduction
Lsp-Proxy is an LSP (Language Server Protocol) client for Emacs, implemented in Rust and inspired by [lsp-bridge](https://github.com/manateelazycat/lsp-bridge). It uses `jsonrpc.el` to facilitate communication between Emacs and the Lsp-Proxy Server. The Lsp-Proxy Server acts as an intermediary between Emacs and various language servers, handling communication with the language servers, processing the responses, and returning them to Emacs.

The features it supports are:
- find definitions/references/implementatoins/type-definition/declaration (as a xref backend)
- completion (as a capf function) support snippet and auto import, reuse requests that are already being processed, while caching the results to improve response speed, before returning all the completion candidates, the server will do fuzzy matching and filter out entries with no match.
- diagnostics (as a flycheck backend default or flymake) process diagnostics when idle.
- hover (triggered by `lsp-proxy-describe-thing-at-point`)
- code action (triggered by `lsp-proxy-execute-code-action`)
- rename (triggered by `lsp-proxy-rename`)
- format buffer (triggered by `lsp-proxy-format-buffer`)
- workspace command, such as `typescript.restartTsServer(vtsls)` 、 `reloadWorkspace(rust-analyzer)` (triggered by `lsp-proxy-execute-command`)
- inlay hints (triggered by `lsp-proxy-inlay-hints-mode`)
- documentHighlight/signature (baesd on `eldoc`)
- documentSymbols (triggered by `imenu`)

![Demo](images/show.gif)

## Prerequisites
### Emacs30.1 or 29 + eglot@latest (Reused some capabilities of eglot to reduce code duplication.)

## Installation
### Via npm (Recommended)
LSP-PROXY is now available via npm with prebuilt binaries for all major platforms:

```bash
npm install -g emacs-lsp-proxy
```

This will automatically install the appropriate binary for your platform (Linux x64/ARM64, macOS x64/ARM64, Windows x64) and make the `emacs-lsp-proxy` command available in your PATH.

### Manually
Before installing LSP-PROXY manually, you should install rust and cargo first.
```bash
git clone https://github.com/jadestrong/emacs-lsp-proxy.git ./your-directory
cd ./your-directory
cargo build --release
# delete old file if exist
rm emacs-lsp-proxy
# cp ./target/release/emacs-lsp-proxy.exe ./
cp ./target/release/emacs-lsp-proxy ./
```

### Download prebuilt binary
You can download the prebuilt binary from [releases](https://github.com/jadestrong/lsp-copilot/releases). For MacOS users, you should allow this binary to run first time, like this:
> The application cannot be opened because it is from an unidentified developer. You can allow this app to run by going to System Settings > Privacy & Security and selecting 'Allow Anyway' for this app.

## How to use
```emacs-lisp
(use-package lsp-proxy
  ;; :load-path "/path/to/lsp-proxy"
  :config
  (add-hook 'tsx-ts-mode-hook #'lsp-proxy-mode)
  (add-hook 'js-ts-mode-hook #'lsp-proxy-mode)
  (add-hook 'typescript-mode-hook #'lsp-proxy-mode)
  (add-hook 'typescript-ts-mode-hook #'lsp-proxy-mode))
```

### DOOM Emacs
- Recommend
``` elisp
(package! lsp-proxy :recipe (:host github :repo "jadestrong/lsp-proxy"
                :files ("lsp-proxy.el"))
```
- Manually
```elisp

(package! lsp-proxy :recipe (:host github :repo "jadestrong/lsp-proxy"
                :files ("lsp-proxy.el" "emacs-lsp-proxy")
                :pre-build (("cargo" "build" "--release") ("cp" "./target/release/emacs-lsp-proxy" "./"))))
```

``` elisp
(use-package! lsp-proxy
  :config
  (set-lookup-handlers! 'lsp-proxy-mode
    :definition '(lsp-proxy-find-definition :async t)
    :references '(lsp-proxy-find-references :async t)
    :implementations '(lsp-proxy-find-implementations :async t)
    :type-definition '(lsp-proxy-find-type-definition :async t)
    :documentation '(lsp-proxy-describe-thing-at-point :async t)))

```

## Add a new language
Performed simple tests on Windows 11 and Arch Linux, it works properly. I have tested it on macOS and use it for daily development in JavaScript, Rust, etc. Therefore, tools like vtsls, typescript-language-server, eslint, tailwindcss, css, and others work fine
- JavaScript/Typescript: [vtsls](https://github.com/yioneko/vtsls) (built-in)、[typescript-language-server](https://github.com/typescript-language-server/typescript-language-server)
- eslint、html、css、html: [vscode-langservers-extracted](https://github.com/hrsh7th/vscode-langservers-extracted) +the latest vscode-eslint has removed the publishDiagnostics method and only supports the pullDiagnostics method, so currently we can only use `vscode-langservers-extracted@4.8`+.
- tailwindcss: [@tailwindcss/language-server](https://www.npmjs.com/package/@tailwindcss/language-server)

The configuration for a new language can refer to the [Helix configuration](https://github.com/helix-editor/helix/blob/master/languages.toml). Supported fields are based on [the built-in configuration file](https://github.com/jadestrong/lsp-copilot/blob/main/languages.toml), and only LSP-related fields are supported.
Open custom language config file by `lsp-proxy-open-config-file` and add your config, then execute `lsp-proxy-restart`.

The configuration fields for adding language support are: `name、roots、language-id、file-types、language-servers` . Other fields in the Helix configuration are not supported.


- Vue2:
```toml
[languge-server.vls]
command = "vls"
args = ["--stdio"]

[[language]]
name = "vue"
roots = ["package.json"]
language-id = "vue"
file-types = ["vue"]
language-servers = ["vls"]
```

- Vue3
```sh
yarn global add @vue/language-server @vue/typescript-plugin typescript
```

```toml
# typescript-language-server
[language-server.typescript-language-server]
config.plugins = [
  { name = "@vue/typescript-plugin", location = "${YOUR-PATH}/node_modules/@vue/typescript-plugin", languages = ["vue"], enableForWorkspaceTypeScriptVersions = true, configNamespace = "typescript" }
]

# or vtsls
[language-server.vtsls.config.vtsls.tsserver]
globalPlugins = [
  { name = "@vue/typescript-plugin", location = "${YOUR-PATH}/node_modules/@vue/typescript-plugin", languages = ["vue"], enableForWorkspaceTypeScriptVersions = true, configNamespace = "typescript" }
]

[language-server.vue-language-server]
command = "vue-language-server"
args = ["--stdio"]

[[language]]
name = "vue"
roots = ["package.json"]
language-id = "vue"
file-types = ["vue"]
language-servers = [
  { name = "vue-language-server", except-features = ["goto-definition", "goto-implementation", "goto-type-definition", "goto-declaration", "goto-reference"] },
  "vtsls"
]
# or
# language-servers = [
#   { name = "vue-language-server", except-features = ["goto-definition", "goto-implementation", "goto-type-definition", "goto-declaration", "goto-reference"] },
#   "typescript-language-server"
# ]
```

- `except-features` can disable server's feature, view the [supported features](https://github.com/jadestrong/lsp-copilot/blob/c3d314d9bc1778b35c6ad2a046fa8b76cad94db4/src/syntax.rs#L150-L168).

## Debug
### Server bug
- `(setq lsp-proxy-log-level 3)`
- M-x `lsp-proxy-restart`
- M-x `lsp-proxy-open-log-file`
### Server crash
- Open `*lsp-proxy-events*` buffer
### Lsp server message
- Open `*lsp-proxy-log*`

## Commands
 - `lsp-proxy-find-definition`
 - `lsp-proxy-find-references`
 - `lsp-proxy-find-declaration`
 - `lsp-proxy-find-type-definition`
 - `lsp-proxy-find-implementations`
 - `lsp-proxy-format-buffer`
 - `lsp-proxy-rename`
 - `lsp-proxy-execute-code-action`
 - `lsp-proxy-execute-command`
 - `lsp-proxy-describe-thing-at-point`
 - `lsp-proxy-show-project-diagnostics`

 -----
 - lsp-proxy-open-log-file
 - lsp-proxy-open-config-file
 - lsp-proxy-restart: Restart the server
 - lsp-proxy-workspace-restart: Restart the LSP server for the current project

## Customization
| Variable                        | Default                                         | Description                                                                                |
| lsp-proxy-user-languages-config | `user-emacs-directory/lsp-proxy/languages.toml` | Where custom language server configurations are stored                                     |
| lsp-proxy-log-file-directory    | temporary-file-directory                        | Log file storage directory                                                                 |
| lsp-proxy-log-level             | 1                                               | A number indicating the log level. Defaults to 1. Warn = 0, Info = 1, Debug = 2, Trace = 3 |


## Recommend config
### Company and Corfu
```elisp
;; company
(setq company-idle-delay 0)
;; If you encounter issues when typing Vue directives (e.g., v-), you can try setting it to 1. I'm not sure if it's a problem with Volar.
(setq company-minimum-prefix-length 2)
(setq company-tooltip-idle-delay 0)

;; corfu
(setq corfu-auto-delay 0)
(setq corfu-popupinfo-delay '(0.1 . 0.1))
```

### company-box
```elisp
(defun company-box-icons--lsp-proxy (candidate)
    (-when-let* ((proxy-item (get-text-property 0 'lsp-proxy--item candidate))
                 (lsp-item (plist-get proxy-item :item))
                 (kind-num (plist-get lsp-item :kind)))
      (alist-get kind-num company-box-icons--lsp-alist)))

(setq company-box-icons-functions
      (cons #'company-box-icons--lsp-proxy company-box-icons-functions))
```

### tabnine
Install [tabnine](https://github.com/shuxiao9058/tabnine) package first, then add the following configuration to your config:
```elisp
(when (fboundp #'tabnine-completion-at-point)
  (add-hook 'lsp-proxy-mode-hook
            (defun lsp-proxy-capf ()
              (remove-hook 'completion-at-point-functions #'lsp-proxy-completion-at-point t)
              (add-hook 'completion-at-point-functions
                        (cape-capf-super
                         #'lsp-proxy-completion-at-point
                         #'tabnine-completion-at-point) nil t))))
```

### flycheck / flymake
Flycheck enabled default if flycheck-mode is installed. You can also select *flymake* by:
```elisp
(setq lsp-proxy-diagnostics-provider :flymake)
```

## Acknowledgements
Thanks to [Helix](https://github.com/helix-editor/helix), the architecture of Lsp-Proxy Server is entirely based on Helix's implementation. Language configuration and communication with different language servers are all dependent on Helix. As a Rust beginner, I've gained a lot from this approach during the implementation.

Regarding the communication between Emacs and Lsp-Proxy, I would like to especially thank [copilot.el](https://github.com/copilot-emacs/copilot.el) and [rust-analyzer](https://github.com/rust-lang/rust-analyzer). The usage of jsonrpc.el was learned from copilot.el, while the approach to receiving and handling Emacs requests was inspired by the implementation in rust-analyzer.

The various methods used to implement LSP-related functionality on the Emacs side were learned from [lsp-mode](https://github.com/emacs-lsp/lsp-mode) and [eglot](https://github.com/joaotavora/eglot). Without their guidance, many of these features would have been difficult to implement.

Regarding the communication data format between Emacs and Lsp-Proxy, I would like to especially thank [emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster). The project integrates the implementation of emacs-lsp-booster, which encodes the JSON data returned to Emacs, further reducing the load on Emacs.
