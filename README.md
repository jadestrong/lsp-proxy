# LSP-PROXY (formerly LSP-COPILOT)

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
                :files ("*.el"))
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

## Language Configuration

LSP-Proxy uses a TOML-based configuration system compatible with [Helix editor](https://docs.helix-editor.com/languages.html). The configuration consists of two main sections: language servers and language definitions.

### Configuration File

Open your user configuration:
```elisp
M-x lsp-proxy-open-config-file
```

This opens `${user-emacs-directory}/lsp-proxy/languages.toml`, which merges with built-in defaults (3 levels deep).

### Language Server Configuration

Define language servers in the `[language-server.<name>]` section:

```toml
[language-server.mylang-lsp]
command = "mylang-lsp"
args = ["--stdio"]
timeout = 20
environment = { "RUST_BACKTRACE" = "1" }
```

**Available options:**

| Key | Description |
|-----|-------------|
| `command` | Language server binary name or path (required) |
| `args` | Arguments passed to the language server |
| `timeout` | Request timeout in seconds (default: 20) |
| `environment` | Environment variables as key-value pairs |
| `config` | LSP `initializationOptions` and `workspace/configuration` |
| `experimental` | Experimental client capabilities (e.g., rust-analyzer) |

**Configuration syntax:**

Use dot notation or TOML tables for nested config:

```toml
# Dot notation
[language-server.mylang-lsp]
config.provideFormatter = true
config.lint.enable = true

# Table notation (equivalent)
[language-server.mylang-lsp.config]
provideFormatter = true

[language-server.mylang-lsp.config.lint]
enable = true
```

### Language Configuration

Define languages in `[[language]]` array sections:

```toml
[[language]]
name = "rust"
language-id = "rust"
file-types = ["rs"]
roots = ["Cargo.toml", "Cargo.lock"]
language-servers = ["rust-analyzer"]
```

**Available options:**

| Key | Description |
|-----|-------------|
| `name` | Unique language identifier (required) |
| `language-id` | LSP language identifier (required) |
| `file-types` | File extensions or glob patterns (required) |
| `roots` | Project root markers for workspace detection |
| `language-servers` | Associated language servers (required) |

**File type patterns:**

```toml
file-types = [
  "js",                    # Extension
  { glob = ".prettierrc" } # Glob pattern
]
```

**Language ID mapping:**

The `language-id` must match LSP specifications:
- `["js", "mjs", "cjs"]` → `javascript`
- `["jsx"]` → `javascriptreact`
- `["ts"]` → `typescript`
- `["tsx"]` → `typescriptreact`

### Multiple Language Servers

Configure multiple servers per language:

```toml
[[language]]
name = "typescript"
language-servers = [
  { name = "vtsls", except-features = ["format"] },
  { name = "eslint", support-workspace = true }
]
```

**Server-specific options:**

| Key | Description |
|-----|-------------|
| `name` | Language server name (required) |
| `except-features` | Disable specific features (blacklist) |
| `only-features` | Enable only specific features (whitelist) |
| `support-workspace` | Share server instance across workspaces |
| `library-directories` | External library paths for navigation |
| `config-files` | Activate only if config file exists |

**Supported features:**

Navigation: `goto-declaration`, `goto-definition`, `goto-type-definition`, `goto-reference`, `goto-implementation`

Code Intelligence: `completion`, `inline-completion`, `completion-resolve`, `signature-help`, `hover`, `document-highlight`, `inlay-hints`

Code Quality: `diagnostics`, `pull-diagnostics`, `code-action`, `rename-symbol`, `format`

Workspace: `document-symbols`, `workspace-symbols`, `workspace-command`

**Library directories:**

For external dependencies outside project roots:

```toml
[[language]]
name = "rust"
language-servers = [
  { name = "rust-analyzer", library-directories = [
    "~/.cargo/registry/src",
    "~/.rustup/toolchains"
  ]}
]
```

**Conditional activation:**

Only activate if config file exists:

```toml
[[language]]
name = "javascript"
language-servers = [
  { name = "eslint", config-files = [
    ".eslintrc.json",
    "eslint.config.js"
  ]}
]
```

### Complete Example

```toml
[language-server.gopls]
command = "gopls"

[language-server.gopls.config]
gofumpt = true

[language-server.gopls.config.hints]
assignVariableTypes = true
parameterNames = true

[[language]]
name = "go"
language-id = "go"
file-types = ["go"]
roots = ["go.mod", "go.work"]
language-servers = ["gopls"]
```

### Built-in Language Servers

LSP-Proxy includes default configurations for:

- **JavaScript/TypeScript**: vtsls, typescript-language-server, eslint
- **Web**: vscode-html-language-server, vscode-css-language-server, tailwindcss-language-server
- **Rust**: rust-analyzer
- **Python**: basedpyright
- **Go**: gopls
- **C/C++**: clangd
- **Ruby**: solargraph
- **Lua**: lua-language-server
- **Java**: jdtls
- **Dart**: dart
- **Bash**: bash-language-server
- **JSON**: vscode-json-language-server
- **TOML**: taplo

See the built-in `languages.toml` for complete configurations.

### Troubleshooting

| Issue | Solution |
|-------|----------|
| Server not starting | Verify `command` is in PATH or use absolute path |
| No completions | Check `language-id` matches server expectations |
| Project not detected | Add appropriate files to `roots` array |
| Features not working | Check server capabilities and `except-features` |
| Debug issues | Set `(setq lsp-proxy-log-level 3)` and run `M-x lsp-proxy-open-log-file` |

After configuration changes, restart:
```elisp
M-x lsp-proxy-restart
```

## Example

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

Below is a complete list of user-facing customization variables (`defcustom`) provided by the Emacs side of lsp-proxy. You can inspect or change them via `M-x customize-group RET lsp-proxy RET`, or set them in your init file with `setq` / `setq-default`.

### Core & Logging
| Variable | Default | Description |
|----------|---------|-------------|
| `lsp-proxy-log-file-directory` | `temporary-file-directory` | Directory where the external server writes its log file. Set to a persistent path if you want logs across restarts. |
| `lsp-proxy-user-languages-config` | `${user-emacs-directory}/lsp-proxy/languages.toml` | User TOML config overriding/augmenting built-in language server definitions. Edited via `M-x lsp-proxy-open-config-file`. |
| `lsp-proxy-log-max` | `0` | Max size (lines/events) of internal events buffer; `0` disables; `nil` infinite. Enable only while debugging. |
| `lsp-proxy-log-level` | `0` | Verbosity: 0 none, 1 basic, 2 verbose. Increase for more diagnostic output (may impact performance). |
| `lsp-proxy-log-buffer-max` | `message-log-max` | Controls Emacs-side *lsp-proxy-log* buffer retention. `nil` disables logging, integer truncates, `t` unlimited. |

### Change / Idle Handling
| Variable | Default | Description |
|----------|---------|-------------|
| `lsp-proxy--send-changes-idle-time` | `0` | Seconds Emacs must be idle before sending buffered `didChange` events. Raise to reduce traffic in huge files. |
| `lsp-proxy-idle-delay` | `0.500` | Debounce interval for batching after-change hooks before running idle tasks. |
| `lsp-proxy-on-idle-hook` | `nil` | Hook list run after idle delay (e.g., refresh diagnostics/Xref). Add buffer‑local functions as needed. |
| `lsp-proxy-enable-bytecode` | `t` | Use bytecode encoding (emacs-lsp-booster style) to reduce JSON parsing overhead. Disable if you see non-ASCII encoding issues. |

### Completion (Popup & Inline)
| Variable | Default | Description |
|----------|---------|-------------|
| `lsp-proxy-max-completion-item` | `20` | Maximum completion items requested/returned per query. Lower for speed, higher for breadth. |
| `lsp-proxy-inline-completion-enable-predicates` | `(evil-insert-state-p)` | All zero-arg predicates must return non-nil to allow inline completion. Customize for editing states. |
| `lsp-proxy-inline-completion-disable-predicates` | `nil` | Any predicate returning non-nil blocks inline completion (override failsafe). |
| `lsp-proxy-inline-completion-trigger-characters` | `()` | Characters that immediately trigger an inline completion request when typed. Use a list of string/char tokens. |
| `lsp-proxy-inline-completion-idle-delay` | `0.3` | Idle delay (seconds) before showing inline completion suggestions after predicates are satisfied. |

### Diagnostics
| Variable | Default | Description |
|----------|---------|-------------|
| `lsp-proxy-diagnostics-provider` | `:auto` | Backend selector: `:auto` prefers Flycheck if present; `:flycheck`, `:flymake` force; `:none` disable; `t` prefer Flymake; `nil` prefer Flycheck. |

### Navigation & Symbols
| Variable | Default | Description |
|----------|---------|-------------|
| `lsp-proxy-enable-imenu` | `t` | Enable Imenu outline via `textDocument/documentSymbol` when server capability is present. |
| `lsp-proxy-lazy-xref-threshold` | `10000` | Line-count threshold above which lazy/optimized Xref evaluation is considered for large buffers. |
| `lsp-proxy-xref-optimization-strategy` | `'optimized` | Strategy for Xref processing: `eager` original; `lazy` minimal preview; `optimized` balanced (fast with previews). |
| `lsp-proxy-enable-symbol-highlighting` | `t` | Highlight occurrences of symbol at point using `documentHighlight` support. |
| `lsp-proxy-enable-hover-eldoc` | `nil` | Request hover info automatically and integrate into Eldoc while moving point. |

### Inlay Hints
| Variable | Default | Description |
|----------|---------|-------------|
| `lsp-proxy-inlay-hints-mode-config` | `nil` | Controls inlay hint activation: `nil` disable; `t` enable globally; list of major mode symbols limits to those modes. |

### Large File Handling
| Variable | Default | Description |
|----------|---------|-------------|
| `lsp-proxy-large-file-threshold` | `(* 10 1024 1024)` | Byte size threshold (≈10MB) beyond which files load asynchronously chunk by chunk. |
| `lsp-proxy-large-file-loading-timeout` | `30` | Seconds before aborting a pending/loading large file operation. |
| `lsp-proxy-large-file-chunk-size` | `(* 1 1024 1024)` | Chunk size (≈1MB) used when streaming large file contents to the server. Adjust for speed vs memory. |

### Formatting Hooks
| Variable | Default | Description |
|----------|---------|-------------|
| `lsp-proxy-trim-trailing-whitespace` | `t` | Trim trailing spaces on lines when syncing/saving (align with project style). |
| `lsp-proxy-insert-final-newline` | `t` | Ensure file ends with a single newline. |
| `lsp-proxy-trim-final-newlines` | `t` | Remove surplus blank lines after the final newline. |

### Usage Tips
- For heavy projects, increase `lsp-proxy--send-changes-idle-time` and maybe lower `lsp-proxy-max-completion-item`.
- If inline completion feels intrusive, add predicates to `lsp-proxy-inline-completion-disable-predicates` (e.g., `(company--active-p)` or mode-specific checks).
- Set `lsp-proxy-log-level` to `2` temporarily when investigating protocol issues, together with `lsp-proxy-log-max` > 0.
- Disabling `lsp-proxy-enable-bytecode` can help pinpoint serialization issues on bleeding-edge Emacs versions.


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
