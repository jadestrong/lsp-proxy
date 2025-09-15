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

## How to add a new language

### Supported Language Servers

**Built-in Support:**
- **JavaScript/TypeScript**: [vtsls](https://github.com/yioneko/vtsls), [typescript-language-server](https://github.com/typescript-language-server/typescript-language-server)
- **Web Technologies**: [vscode-langservers-extracted](https://github.com/hrsh7th/vscode-langservers-extracted) (eslint, html, css)
- **CSS Framework**: [@tailwindcss/language-server](https://www.npmjs.com/package/@tailwindcss/language-server)
- **Rust**: rust-analyzer
- **Python**: basedpyright, pylsp
- **Go**: gopls
- And many more in the built-in configuration

### Quick Setup Guide

1. **Open configuration file**
   ```elisp
   M-x lsp-proxy-open-config-file
   ```

2. **Add language server definition**
   ```toml
   [language-server.your-server]
   command = "your-language-server"     # Required: executable command
   args = ["--stdio"]                   # Optional: command arguments
   environment = { VAR = "value" }      # Optional: environment variables  
   timeout = 20                         # Optional: request timeout (default: 20s)
   config = { key = "value" }           # Optional: initializationOptions 
   experimental = { key = "value" }     # Optional: experimental client capabilities., e.g. rust-analyzer
   ```

3. **Define language mapping**
   ```toml
   [[language]]
   name = "your-language"               # Required: unique language name
   language-id = "your-language"        # Required: LSP language identifier
   file-types = ["ext1", "ext2"]        # Required: file extensions
   roots = ["project-file.json"]        # Optional: project root detection files
   language-servers = ["your-server"]   # Required: associated servers
   ```
   
   The `language-servers` field can be a simple string array containing the server names you configured above. It can also be configured with detailed options. See the **Advanced Configuration** section for more information.
   
   **Note:** The language's name can be customized, but the `language-id` should correspond to the `file-types`. For example:
   - `["js", "mjs", "cjs"]` files correspond to `javascript` language ID
   - `[jsx]` files correspond to `javascriptreact` language ID  
   - `[tsx]` files correspond to `typescriptreact` language ID

4. **Restart LSP-Proxy**
   ```elisp
   M-x lsp-proxy-restart
   ```

### Advanced Configuration

#### Multi-Server Setup
Configure multiple language servers for a single language:
```toml
[[language]]
name = "typescript"
language-servers = [
  { name = "vtsls", except-features = ["format"] },
  { name = "eslint", support-workspace = true }
]
```

You can configure multiple language servers together for a single file type. For example, you can use `vtsls`, `eslint`, and `tailwindcss` for TypeScript/JSX files, or `eslint`, `vue-language-server`, and `tailwindcss` for Vue files. 

When multiple language servers support the same feature, you can control which server provides specific functionality by using `except-features` or `only-features`. For example, you can disable the `format` feature from the `vtsls` language server while keeping it enabled for other servers.

#### Feature Control Options
- **`except-features`**: Disable specific server capabilities. See the **Supported LSP Features** section for available features.
- **`only-features`**: Enable only specified features (whitelist). See the **Supported LSP Features**  section for available features.
- **`support-workspace`**: Enable multi-workspace support, allowing language servers like `eslint` to share a single server instance across multiple projects.
- **`library-directories`**: Additional library search paths for external dependencies. Some language servers manage libraries outside the project directory (e.g., rust-analyzer uses `~/.cargo/registry/src` and `~/.rustup/toolchains`, Dart uses `~/.pub-cache`). Since these libraries are shared across multiple projects, LSP-Proxy needs to send proper notifications (like `didOpen`) to the associated language server when navigating to files in these directories.
- **`config-files`**: Optional configuration files for server activation. When configured, the language server will only activate if at least one of the specified configuration files exists in the workspace root (e.g., `eslint.config.ts` for ESLint server, `vue.config.ts` for vue-language-server).

#### Server Configuration Examples

There are two syntaxes for setting a language server's configuration. Your configured values will be passed to the language server in the `initialize` request's `initializationOptions` and will also be used for `workspace/configuration` requests.

**Python with advanced configuration:**
```toml
[language-server.basedpyright]
command = "basedpyright-langserver"
args = ["--stdio"]
config.basedpyright.analysis.typeCheckingMode = "basic"

[[language]]
name = "python"
language-servers = [
  { name = "basedpyright", library-directories = ["~/.local/lib/python3.*/site-packages"] }
]
```

The following syntax is different from the above example, but achieves the same effect as long as it follows valid TOML syntax:
``` toml
[language-server.basedpyright.config.basedpyright]
analysis.typeCheckingMode = "basic"
```

### Supported LSP Features

LSP-Proxy supports all major Language Server Protocol features:

**Navigation & References:**
- `goto-declaration` - Jump to symbol declarations
- `goto-definition` - Jump to symbol definitions  
- `goto-type-definition` - Jump to type definitions
- `goto-reference` - Find all references
- `goto-implementation` - Find implementations

**Code Intelligence:**
- `completion` - Auto-completion with snippets
- `inline-completion` - Inline completion suggestions
- `completion-resolve` - Detailed completion information
- `signature-help` - Function signature assistance
- `hover` - Symbol documentation on hover
- `document-highlight` - Symbol highlighting
- `inlay-hints` - Inline type and parameter hints

**Code Quality:**
- `diagnostics` - Error and warning reporting
- `pull-diagnostics` - On-demand diagnostic retrieval
- `code-action` - Quick fixes and refactoring
- `rename-symbol` - Symbol renaming across workspace
- `format` - Code formatting

**Workspace Features:**
- `document-symbols` - File outline and structure
- `workspace-symbols` - Project-wide symbol search
- `workspace-command` - Execute workspace-specific commands

### Configuration Sources

LSP-Proxy uses a layered configuration system:

1. **Built-in configuration**: Default language servers and settings
2. **User configuration**: Custom overrides in `${user-emacs-directory}/lsp-proxy/languages.toml`, opened by `M-x lsp-proxy-open-config-file`
3. **Deep merging**: User config merges with built-in defaults (3 levels deep)

The configuration format is compatible with [Helix editor](https://github.com/helix-editor/helix/blob/master/languages.toml) language definitions, focusing on LSP-related fields only.

### Troubleshooting

- **Server not starting**: Check `command` path and `args` correctness
- **No completions**: Verify `language-id` matches server expectations
- **Project not detected**: Ensure `roots` files exist in your project
- **Features missing**: Check server capabilities and `except-features` configuration
- **Debug**: Use `M-x lsp-proxy-open-log-file` and set `(setq lsp-proxy-log-level 3)`

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
