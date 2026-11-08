# Java Support

## Clean the Java workspace cache

Run `M-x lsp-proxy-java-clean-workspace` from a local Java project buffer to
recover from stale or damaged JDTLS workspace indexes. The command runs
asynchronously, stops the project's JDTLS and Spring companion, waits for them
to exit, and moves the JDTLS `-data` directory to a sibling backup. It then
reopens affected buffers with their current contents so JDTLS imports the
project and rebuilds its workspace indexes. Unsaved edits are preserved.

The backup location is printed in `*Messages*`. Backups are retained; remove
them manually after checking that the reimport completed successfully. Import
progress and errors remain available through the normal modeline and
`lsp-proxy-java-open-import-log`.

The standard `jdtls` launcher and explicit `-data` arguments are supported.
Custom wrappers must supply an explicit `-data` path. Cleanup refuses shared
caches, servers serving multiple workspaces, remote buffers, unsafe paths,
and caches still locked by another process. If moving the cache fails after
stopping the servers, affected buffers reconnect to the existing cache and
the error is reported. This command rebuilds the workspace cache; it does not
clean separately configured shared indexes.

For normal build-file changes, use `lsp-proxy-java-update-project-configuration`.
For an ordinary rebuild, use `lsp-proxy-java-build-workspace` (`C-u` requests a
full rebuild). Cache cleanup is intended for workspace recovery.

## Spring 符号搜索：JDT LS + STS4

使用 `M-x lsp-proxy-workspace-symbol` 搜索当前 Java 项目的普通符号和 Spring 符号。
`xref-find-apropos` 也能显示源码符号；候选源文件在实际跳转时才打开。

| 查询 | STS4 返回的内容 |
| --- | --- |
| `@/` | RequestMapping、GetMapping、PostMapping 等请求映射 |
| `@+` | Bean 名称及类型 |
| `@>` | Function Bean |
| `@` | Spring 注解符号 |
| `//` | 已连接的运行应用 endpoint URL |

查询原样发送给语言服务器。选中源码符号后跳转到服务器提供的位置；
选中 HTTP/HTTPS 结果后通过 `browse-url` 打开。
`M-x lsp-proxy-spring-running-endpoints` 是 `//` 查询的快捷入口。

### 安装和启用

先构建包含本功能的 proxy，并让 Emacs 使用该二进制：

```sh
cargo build --release
```

如果 Emacs 仍优先找到 PATH 中的旧版本，可将 `lsp-proxy-server-path`
设为当前仓库 `target/release/emacs-lsp-proxy` 的绝对路径，重新加载
`lsp-proxy` 后再执行安装命令。

1. `M-x lsp-proxy-java-install-server`，选择 `jdtls`。
2. `M-x lsp-proxy-java-install-spring`，安装固定的 Spring Boot Tools 1.62.0。
3. 在 `lsp-proxy-user-languages-config` 指向的文件中配置两个 Java 服务器。
4. `M-x lsp-proxy-restart`，然后重新打开 Java 文件或启用 `lsp-proxy-mode`。

```toml
[[language]]
name = "java"
language-servers = [
  { name = "jdtls", support-workspace = false },
  { name = "spring-boot", only-features = ["workspace-symbols"], support-workspace = false },
]
```

内置 `[language-server.spring-boot]` 已定义 `command = "spring-boot-ls"`。
用户的 `language-servers` 会替换内置列表，所以需要同时列出 JDT LS 和 STS4。
这里让两个服务器都按项目建立实例，classpath callback 依据实际项目关联路由。
首次导入 Maven/Gradle 依赖后，STS4 才能建立完整索引。

| Emacs 选项 | 用途 |
| --- | --- |
| `lsp-proxy-java-spring-install-dir` | STS4 安装目录 |
| `lsp-proxy-java-spring-version` | 安装并使用的发行版，默认 `1.62.0` |
| `lsp-proxy-java-spring-java-home` | STS4 启动 JDK home，独立于项目编译 JDK |

STS4 1.62.0 需要 JDK 21。可以设置：

```elisp
(setq lsp-proxy-java-spring-java-home "/path/to/jdk-21")
```

未设置时，launcher 使用 `JAVA_HOME` 或 PATH 中的 `java`。
JDT LS 的启动 JDK仍按其 launcher 配置；Java 8 项目可以继续使用 JDT 的
`java.configuration.runtimes`，不需要把项目编译 JDK 改为 21。

安装器校验 Open VSX 发布包的 SHA-256，依据 `contributes.javaExtensions`
清单安装 5 个 JDT bundles，并保留安装来源、版本和 launcher 信息。
proxy 启动时自动把这些 bundles 与 Java Debug、用户 bundles 合并并去重。
安装或修改版本/JDK 后需要重启 proxy。

### 运行应用 endpoint 与排障

`//` 依赖 STS4 已取得运行应用的 LiveData；源码 `@/` 有结果并不保证 `//` 有结果。
先在本机启动 Spring Boot 应用，并启用本地 JMX，例如加入 JVM 参数
`-Dcom.sun.management.jmxremote`。运行
`M-x lsp-proxy-java-spring-connect-running-app` 选择应用并连接或刷新 LiveData，
然后运行 `M-x lsp-proxy-spring-running-endpoints`。
STS4 1.62.0 使用 `sts/livedata/listProcesses`、`connect`、`refresh` 命令管理连接。
内置配置开启 `boot-java.live-information.all-local-java-processes`，
让应用选择器能发现本机 JVM；选择后才连接。
Spring Boot 2.2 示例还启用了 `spring.application.admin.enabled=true`、
`spring.jmx.enabled=true` 和 `management.endpoints.jmx.exposure.include=*`。
应用需具备 STS4 能读取的运行时信息；
Actuator/JMX 暴露方式和应用版本会影响 mappings 的可见性。
远端应用需要另外配置 STS4 的 remote apps/JMX 连接及可访问地址；
返回 localhost URL 时需在本机建立端口转发。

查看 proxy 日志中的 `Spring classpath`：注册成功会记录 STS/JDT 实例 ID；
缺少 JDT、初始化超时、扩展命令失败或 callback 未登记会返回明确错误。
STS4 的 classpath 注册超时只有 10 秒，proxy 会等待同项目 JDT 的
`ServiceReady` 后才初始化 STS，等待上限为 120 秒。
缺少扩展时运行安装命令并重启整个 proxy，让 JDT 重新读取 bundles。
workspace symbol 搜索允许某个服务器失败后保留其他服务器结果，失败详情记录在日志中。
没有已初始化的符号服务器或全部请求失败时，搜索会返回错误。

JDT 中的 Spring 扩展还会在发现/移除 Boot 项目时请求
`vscode-spring-boot.ls.start`/`stop`。proxy 已按语言配置管理 STS 生命周期，
因此立即成功应答这两个命令。旧版将它们误当作 classpath 回调，可能在
`JdtLsExtensionPlugin`/`BootProjectTracker` 堆栈中出现
`No STS callback registered for vscode-spring-boot.ls.start`。
遇到该错误需重新构建并重启 proxy，无需重装 JDT LS 或 STS4。

普通 Java 的 definition 跳转由 JDT LS 处理，STS 应保留上述
`only-features = ["workspace-symbols"]` 配置。旧 proxy 声明支持 definition
动态注册，却忽略 JDT 的注册请求；加入 STS 后会跳过 JDT，可能将普通 Java
定义查询发给 STS 并得到空结果。现已补齐 definition 的动态注册、注销和
`documentSelector` 匹配，公共 `dynamicRegistration` 保持 `true`。
JDT 注册后即可参与定义查询；注销后停止参与，用户的 `only-features` /
`except-features` 配置仍有效。更新 proxy 后重启即可生效。

偶发 `data did not match any variant of untagged enum GotoDefinitionResponse`
表示 proxy 无法解析服务器的跳转结果。旧代码直接解析非可空的响应类型，
会将 LSP 允许的 `null`（没有找到目标）误报为解析错误；该代码自 2024 年
已有，与本次 definition 动态注册实现是不同问题。现已让 definition、
declaration、typeDefinition 和 implementation 接受 `null`，并保留已有位置
及 LocationLink 转换。其他非法响应仍报错，错误会包含服务器名和请求方法。
更新并重启 proxy 即可，无需重新安装语言服务器。没有失败响应原文时，
无法确定每次偶发错误都来自 `null`；项目尚未完成索引等瞬态原因需结合日志核实。

基础入口使用 `read-string` 和 `completing-read`，每次输入后搜索一次。
选择使用 Ivy 的用户需先显式加载 `lsp-proxy-ivy`，再调用 `lsp-proxy-ivy-workspace-symbol` 边输入边查询；
`lsp-proxy-ivy-spring-running-endpoints` 预填 `//`，详见 [Ivy 界面](ivy.md)。
Spring hover、CodeLens、properties/yaml completion 需要后续单独接入。

LSP-Proxy 默认使用 Eclipse JDT Language Server（`jdtls`）提供 Java 支持。

## jdtls Backend

### 默认配置

内置 Java 配置使用 `language-servers = ["jdtls"]`。
用户配置中的 `language-servers` 会替换内置列表；启用 Spring 时按上文同时配置 JDT LS 和 STS4。

### Install

Use the managed installer:

```elisp
M-x lsp-proxy-java-install-server
```

Then choose `jdtls` and restart LSP-Proxy. The managed install puts the server's
`bin/` directory on the proxy process `PATH`, so `command = "jdtls"` is enough.

| Setting/requirement | Purpose |
| --- | --- |
| `lsp-proxy-java-jdtls-version` | jdtls milestone to install. `nil` installs the latest known milestone. |
| `python3` | Required because `bin/jdtls` is a Python launcher. |
| JDK 21+ | Required to launch jdtls. It chooses `JAVA_HOME` first, then `java` on `PATH`. |

You can also install jdtls yourself, for example with a package manager, as long
as the `jdtls` launcher is on `PATH`.

### Configuration Fields

| Field | Default/example | Purpose |
| --- | --- | --- |
| `command` | `"jdtls"` | Starts the official launcher. Avoid hand-built `java -jar ... -data ...` commands because the launcher derives per-project workspace data. |
| `lombok-agent-arg` | `"--jvm-arg=-javaagent:{jar}"` | Tells LSP-Proxy how to append the managed Lombok agent when installed. |
| `[language-server.jdtls.config]` | table | Sent as jdtls `initializationOptions`. |
| `settings.java.referencesCodeLens.enabled` | `true` | Enables reference CodeLens. |
| `settings.java.implementationCodeLens` | `"all"` | Enables implementation CodeLens. Supported values: `"none"`, `"types"`, `"methods"`, `"all"`. |
| `settings.java.configuration.runtimes` | commented | Optional project compile JDK list. Controls project runtime model, not the JDK that launches jdtls. |

Example explicit runtimes:

```toml
[[language-server.jdtls.config.settings.java.configuration.runtimes]]
name = "JavaSE-1.8"
path = "/Library/Java/JavaVirtualMachines/zulu-8.jdk/Contents/Home"
default = true
```

When `runtimes` is absent, LSP-Proxy tries to infer a project JDK from common
project files such as `.java-version`, `.tool-versions`, `.sdkmanrc`, Maven
compiler settings, and Gradle toolchain/source compatibility. Use
`M-x lsp-proxy-java-select-project-jdk` for a session-local override; with a
prefix argument it clears the override and returns to automatic detection.

### Automatically Managed Pieces

| Piece | How to use |
| --- | --- |
| Java Debug bundle | Run `M-x lsp-proxy-java-install-debug`, or install jdtls through `M-x lsp-proxy-java-install-server`. The proxy injects the managed bundle at startup. |
| Lombok agent | Run `M-x lsp-proxy-java-install-lombok`, or install jdtls through the managed installer. The proxy appends the agent only when a managed jar exists. |
| Project runtime | Automatic unless explicit `settings.java.configuration.runtimes` entries are configured. |

If you maintain your own Lombok agent, put a `-javaagent:` argument containing
`lombok` in `args`; that suppresses the managed agent.

### Supported Capabilities and Methods

| Method/capability | What LSP-Proxy does |
| --- | --- |
| `classFileContentsSupport` | Enables dependency/JDK navigation. jdtls returns `jdt://` URIs and LSP-Proxy fetches source with `java/classFileContents`. |
| `progressReportProvider` | Enables jdtls `language/progressReport`; the proxy translates it onto normal LSP progress. |
| `advancedOrganizeImportsSupport` | Enables organize-imports code actions that require a client-side prompt. |
| `advancedGenerateAccessorsSupport` | Enables accessor-generation code actions that require a client-side prompt. |
| `overrideMethodsPromptSupport` | Enables override-methods code actions that require a client-side prompt. |
| `resolveAdditionalTextEditsSupport` | Lets completion resolve add imports through extra text edits. |
| `java.show.references` | Client-side CodeLens command; LSP-Proxy displays the locations carried by the resolved lens. |
| `java.show.implementations` | Client-side CodeLens command; LSP-Proxy displays the locations carried by the resolved lens. |
| `java/projectConfigurationUpdate` | Notification sent when LSP-Proxy needs jdtls to refresh project configuration. |

### Unsupported Extended Capabilities

Do not enable these jdtls extended capabilities until matching client handlers are
implemented:

| Capability | Status |
| --- | --- |
| `hashCodeEqualsPromptSupport` | Not implemented in the client. |
| `generateConstructorsPromptSupport` | Not implemented in the client. |
| `generateToStringPromptSupport` | Not implemented in the client. |
| `advancedExtractRefactoringSupport` | Not implemented in the client. |
| `moveRefactoringSupport` | Not implemented in the client. |

The corresponding code actions can still appear; without these extended
capability flags they run through the server-side fallback path instead of
requiring an unimplemented client prompt.
